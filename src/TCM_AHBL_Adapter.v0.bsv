// Copyright (c) 2017 Bluespec, Inc.  All Rights Reserved

package TCM_AHBL_Adapter;

// ================================================================
// Adapter converting generic 32b-wide read/write requests into an
// AHB-Lite bus master. 'Client' upstream:
// - an MMIO: requests/responses are for 64b word or sub-word,
//            and where lane-alignment is already done.

// The AHB-Lite bus master can be used with 32b buses, and manages
// byte-lane alignment, number of beats in a burst, write-strobes,
// etc. accordingly.

// Transactor from AXI4-Lite Master interface to AHB-Lite master interface.
// - 32b addr bus
// - 32b data bus
// - Sub-word writes, whole-word reads
// - SINGLE transfers only, no bursts
// - No AHBL HMASTLOCK locking.

// Ref for AHB-Lite:
//     ARM AMBA 5 AHB Protocol Specification
//     AHB5, AHB-Lite
//     ARM IHI 0033B.b, 2015
//     ARM Ltd.

// ================================================================
// TODO:
// - check if request addr is properly aligned, return error otherwise
// - Parameterize by data-width?

// ================================================================
// BSV lib imports

import DefaultValue        :: *;
import FIFOF               :: *;
import GetPut              :: *;

// ----------------
// Other lib imports

// import Semi_FIFOF :: *;
// import EdgeFIFOFs :: *;

// ================================================================
// Project imports

import AHBL_Types          :: *;

`ifdef STANDALONE
import Testbench_Commons   :: *;
`else
import ISA_Decls           :: *;
import MMU_Cache_Commons   :: *; // definitions for req/rsp types
import Cur_Cycle           :: *;
`endif

// ================================================================
// AXI4-Lite and AHB-Lite bus widths and fabric definition. These could move into a
// AHB_Fabric_Defs package

typedef 32 AHB_Wd_Addr;    // AHB-L only supports 32b addresses
`ifdef FABRIC32
typedef 32 AHB_Wd_Data;
`endif
`ifdef FABRIC64
typedef 64 AHB_Wd_Data;
`endif

typedef Bit #(AHB_Wd_Addr) AHB_Fabric_Addr;
typedef Bit #(AHB_Wd_Data) AHB_Fabric_Data;

// Byte-width of the fabric
Integer bytes_per_fabric_data = ((valueOf (AHB_Wd_Data) == 32) ? 4 : 8);

// ================================================================
// AXI4-Lite to AHB-Lite master transactor module

typedef enum { IDLE, RDATA_PENDING } Data_Pending deriving (Bits, Eq, FShow);

// ================================================================
// This module collects and discards write-responses.
// There is no write-data response to the client.
// Errors on writes are reported on a separate method that can be used
// to trigger an interrupt.

// This module avoids interleaving read and write requests,
// i.e., it launches a read request only when no write-responses from
// the AHB fabric are pending.  

// ================================================================
// MODULE INTERFACE

interface TCM_AHBL_Adapter_IFC;
   // Reset
   method Action  reset;

   // ----------------
   // interface for word/sub-word read/write client

   interface Put #(Single_Req) p_mem_single_req;
   interface Put #(Bit #(64))  p_mem_single_write_data;
   interface Get #(Read_Data)  g_mem_single_read_data;

   // ----------------
   // Fabric master interface
   interface AHBL_Master_IFC #(AHB_Wd_Data) mem_master;

   // ----------------------------------------------------------------
   // Misc. control and status

   // Misc. status; 0 = running, no error
   (* always_ready *)
   method Bit #(8) mv_status;

endinterface

// ================================================================
// Misc. help functions

// ----------------------------------------------------------------
// Address converters

`ifdef ISA_PRIV_S
// Convert a 64-bit PA to an AXI4 Fabric Address
// Discard the upper 32 bits

function AHB_Fabric_Addr fv_Addr_to_Fabric_Addr (Bit #(64) addr);
return truncate (addr);
endfunction

`else

// Convert a XLEN Address to an AXI4 Fabric Address 
function Bit #(32) fv_Addr_to_Fabric_Addr (Addr addr);
`ifdef RV32
return (addr);
`endif
`ifdef RV64
return truncate (addr);
`endif
endfunction

`endif   // `else ISA_PRIV_S

// ----------------------------------------------------------------
// Convert size code into AXI4_Size code (number of bytes in a beat).

function AHBL_Size  fv_size_code_to_AHBL_Size (Bit #(2) size_code);
if ((valueOf (AHB_Wd_Data) == 32) && (size_code == 2'b11)) return AHBL_BITS32;
   else return unpack ({ 1'b0, size_code });
endfunction

// ================================================================
// MODULE IMPLEMENTATION
`ifdef STANDALONE
(* synthesize *)
`endif
module mkTCM_AHBL_Adapter #(
     parameter Bit #(2) verbosity            // Verbosity: 0=quiet, 1 = rule firings
`ifdef STANDALONE
) (TCM_AHBL_Adapter_IFC);
`else
   , FIFOF #(Single_Req) f_single_reqs
   , FIFOF #(Bit #(64))  f_single_write_data
   , FIFOF #(Read_Data)  f_single_read_data) (TCM_AHBL_Adapter_IFC);
`endif

`ifdef STANDALONE
   FIFOF #(Single_Req) f_single_reqs <- mkFIFOF1;
   FIFOF #(Bit #(64))  f_single_write_data <- mkFIFOF1;
   FIFOF #(Read_Data)  f_single_read_data <- mkFIFOF1;
`endif

   // Output signals to AHBL bus
   Wire #(AHB_Fabric_Addr)    wi_haddr          <- mkBypassWire;
   Wire #(AHBL_Burst)         wi_hburst         <- mkBypassWire;
   Wire #(Bool)               wi_hmastlock      <- mkBypassWire;
   Wire #(AHBL_Prot)          wi_hprot          <- mkBypassWire;
   Wire #(AHBL_Size)          wi_hsize          <- mkBypassWire;
   Wire #(AHBL_Trans)         wi_htrans         <- mkBypassWire;
   Wire #(AHB_Fabric_Data)    wi_hwdata         <- mkBypassWire;
   Wire #(Bool)               wi_hwrite         <- mkBypassWire;

   // Input signals from AHBL bus
   Wire #(AHB_Fabric_Data)    wi_hrdata         <- mkBypassWire;
   Wire #(Bool)               wi_hready         <- mkBypassWire;
   Wire #(AHBL_Resp)          wi_hresp          <- mkBypassWire;

   // remembers none/read/write for the data phase
   Reg #(Bool)                rg_rsp_pending    <- mkReg (False);

   // wdata for the data phase
   Reg #(AHB_Fabric_Data)     rg_wdata          <- mkRegU;

   // are we bursting?
   Reg #(Bool)                rg_burst_last_addr<- mkReg (False);
   Reg #(Bool)                rg_burst_last_data<- mkReg (False);
   Reg #(Bool)                rg_data_lower32_ok<- mkReg (False);
   Reg #(AHB_Fabric_Data)     rg_data_lower32   <- mkRegU;

   // keeps track of the address we are at during a burst
   Reg #(AHB_Fabric_Addr)     rg_burst_addr     <- mkRegU;

   // ================================================================
   // BEHAVIOR

   // --------
   // Continuous signals
   let req = f_single_reqs.first;

   // A read request is waiting
   Bool rda_avail = req.is_read;

   // A write request is waiting. For writes, we need wr_addr and wr_data available as we
   // compute HSIZE from wr_data.wstrb
   Bool wra_avail = !rda_avail && f_single_write_data.notEmpty;

   let req_addr = fv_Addr_to_Fabric_Addr (req.addr);

   // Generate the HTRANS depending on whether we are bursting or not. This implementation never
   // signals AHBL_BUSY
   let htrans_value = ((rda_avail || wra_avail) ? (rg_burst_last_addr ? AHBL_SEQ : AHBL_NONSEQ)
                                                : AHBL_IDLE);

   // Do we need to continue with a burst? Only works for two-beat bursts.
   let bursting = (   (valueOf (AHB_Wd_Data) == 32)
                   && (req.size_code == 2'b11)
                   && (!rg_burst_last_addr));


   // --------
   // Drive the combinational AHB master output signals
   rule rl_outputs;
      wi_haddr     <= rg_burst_last_addr ? req_addr : rg_burst_addr;
      wi_hwrite    <= (! rda_avail);
      wi_hsize     <= fv_size_code_to_AHBL_Size (req.size_code);
      wi_hburst    <= AHBL_INCR;
      wi_hprot     <= defaultValue;
      wi_hmastlock <= False;
      wi_htrans    <= htrans_value;
      wi_hwdata    <= rg_wdata;
   endrule


   // --------
   // Sample responses on HREADY and update request FIFOs/burst counters to drive next request
   // and write data.
   rule rl_sample_inputs (wi_hready);
      // Master is idling, nothing to do
      if (htrans_value == AHBL_IDLE)
	 rg_rsp_pending <= False;

      // Master is receiving response for a NONSEQ/SEQ beat
      // XXX If AHBL_BUSY is introduced then the else condition has to be made explicit.
      else begin
	 if (rda_avail) begin
	    // RD address accepted, now waiting for read response
	    rg_rsp_pending <= True;
	 end

         else if (wra_avail) begin
	    // Sending wr address, next cycle send write data
	    rg_rsp_pending <= False;
            let wr_data = f_single_write_data.first;

            // FABRIC32 adjustments
            if (valueOf (AHB_Wd_Data) == 32) begin
               // Not a burst
               if (req.size_code != 2'b11) begin
                  // B, H, W: only 1 beat. Create the wdata from the correct half
                  Bool in_upper32 = (req.addr [2] == 1'b1);
                  if (in_upper32) begin
                     wr_data = { 32'b0, wr_data [63:32] };
                  end
               end

               // Last addr phase of a burst. Queue up the last wdata for the data phase.
               else if (rg_burst_last_addr) begin 
                  wr_data = { 32'b0, wr_data [63:32] };
               end
            end

            rg_wdata <= truncate (wr_data);
	 end

         else  // no transaction to initiate, should never get here
            $display ("%0d: %m.rl_inputs: AHBL_NONSEQ when there is no transaction", cur_cycle);
      end

      // When bursting, do not advance the request FIFO (this logic will only work for a
      // 2-beat burst)
      if (!bursting) begin
         f_single_reqs.deq;
         if (wra_avail) f_single_write_data.deq;
      end

      // burst controls and addr update
      rg_burst_last_addr <= bursting;
      rg_burst_last_data <= rg_burst_last_addr;
      rg_burst_addr <= req_addr + fromInteger (bytes_per_fabric_data);
   endrule


   // --------
   // Forward response to CPU 
   rule rl_rsp (wi_hready && rg_rsp_pending);
      // Capture responses into response FIFOs
      // Response handling and packing
      Bit #(64) data = zeroExtend (wi_hrdata);
      Bool ok = (wi_hresp == AHBL_OKAY);
      Bool do_enq = True;

      // FABRIC32 adjustments
      if (valueOf (AHB_Wd_Data) == 32) begin
         // Not a burst
         if (req.size_code != 2'b11) begin
            // B, H, W: only 1 beat. Adjust the data into the correct half and enq
            Bool in_upper32 = (req.addr[2] == 1'b1);
            if (in_upper32) data = { data [31:0], 32'b0 };
            do_enq = True;
         end
         else if (rg_burst_last_addr) begin 
            // First response of a burst. Just save lower 32b; enq after the next beat
            rg_data_lower32_ok <= ok;
            rg_data_lower32    <= data [31:0];
            do_enq = False;
         end
         else if (rg_burst_last_data) begin 
            // final response of a burst. pack complete data and enq
            ok   = (rg_data_lower32_ok && ok);
            data = { data [31:0], rg_data_lower32 };
            do_enq = True;
         end
      end
      if (do_enq) begin
         let rsp = Read_Data { ok: ok, data: data };
         f_single_read_data.enq (rsp);
      end
   endrule

   // ================================================================
   // INTERFACE
   method Action reset;
      f_single_reqs.clear;
      f_single_write_data.clear;
      f_single_read_data.clear;
      if (verbosity > 1)
         $display ("%0d: %m.reset", cur_cycle);
   endmethod


   // ----------------
   // interface for word/sub-word read/write client

   interface Put p_mem_single_req        = toPut (f_single_reqs);
   interface Put p_mem_single_write_data = toPut (f_single_write_data);
   interface Get g_mem_single_read_data  = toGet (f_single_read_data);


   // ----------------
   // AHBL side
   interface AHBL_Master_IFC mem_master;
      method haddr     = wi_haddr;
      method hburst    = wi_hburst;
      method hmastlock = wi_hmastlock;
      method hprot     = wi_hprot;
      method hsize     = wi_hsize;
      method htrans    = wi_htrans;
      method hwdata    = wi_hwdata;
      method hwrite    = wi_hwrite;

      method Action hrdata (AHB_Fabric_Data data);
         wi_hrdata <= data;
      endmethod

      method Action hready (Bool ready);
         wi_hready <= ready;
      endmethod

      method Action hresp (AHBL_Resp resp);
         wi_hresp <= resp;
      endmethod
   endinterface
endmodule


// ================================================================

endpackage
