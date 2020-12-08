// Copyright (c) 2020 Bluespec, Inc.  All Rights Reserved

package TCM_AHBL_Adapter;

// ================================================================
// Adapter converting generic 32b-wide read/write requests into an
// AHB-Lite bus master. 'Client' upstream:
// - an MMIO: requests/responses are for 64b word or sub-word,
//            and where lane-alignment is already done.
// 
// NOTE: This implementation only supports accesses upto 32-bits for
//       reads or writes. Burst accesses on AHB are *not* supported.
//       This is a design choice to reduce gate-count as the proposed
//       use is for RV32IMAC.
//
// The AHB-Lite bus master can be used with 32b buses, and manages
// byte-lane alignment, etc. accordingly.
//
// Ref for AHB-Lite:
//     ARM AMBA 5 AHB Protocol Specification
//     AHB5, AHB-Lite
//     ARM IHI 0033B.b, 2015
//     ARM Ltd.

// ================================================================
// TODO:
// 1. Support 64-bit accesses on FABRIC32
// 2. Use UGDEQ FIFOs for the request and wdata to avoid local FFs
//    for addr and data.
// ================================================================
// Macros:
// 
// STANDALONE: Allows separate compilation with CPU-side interfaces
//
// ================================================================
//
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
import AHBL_Defs           :: *;

`ifdef STANDALONE
import Testbench_Commons   :: *;
`else
import ISA_Decls           :: *;
import MMU_Cache_Common    :: *; // definitions for req/rsp types
import Cur_Cycle           :: *;
`endif

// ================================================================
// Local state enumeration

typedef enum { IDLE, ADDR, WDATA, RDATA } AHB_Master_State deriving (Bits, Eq, FShow);

// ================================================================
// This module collects and discards write-responses. There is no
// write-data response to the client. Errors on writes are not
// checked or reported.
//
// This module does not interleaving read and write requests issuing
// one at a time.
//
// ================================================================
// MODULE INTERFACE

interface TCM_AHBL_Adapter_IFC;
   // Reset
   method Action  reset;

`ifdef STANDALONE
   // ----------------
   // interface for word/sub-word read/write client

   interface Put #(Single_Req) p_mem_single_req;
   interface Put #(Bit #(64))  p_mem_single_write_data;
   interface Get #(Read_Data)  g_mem_single_read_data;
`endif

   // ----------------
   // Fabric master interface
   interface AHBL_Master_IFC #(AHB_Wd_Data) mem_master;

   // ----------------------------------------------------------------
   // Misc. control and status

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
// Convert size code into AHBL_Size code (number of bytes in a beat).

function AHBL_Size  fv_size_code_to_AHBL_Size (Bit #(2) size_code);
   if ((valueOf (AHB_Wd_Data) == 32) && (size_code == 2'b11)) return AHBL_BITS32;
   else return unpack ({ 1'b0, size_code });
endfunction

// Adjust incoming write data based on FABRIC-32/64
function Bit #(64) fv_get_ahb_wdata (Single_Req req, Bit #(64) wr_data);
   let wdata = wr_data;

   // FABRIC32 adjustments
   if (valueOf (AHB_Wd_Data) == 32) begin
      // Not a burst
      if (req.size_code != 2'b11) begin
         // B, H, W: only 1 beat. Create the wdata from the correct half
         Bool in_upper32 = (req.addr [2] == 1'b1);
         if (in_upper32) begin
            wdata = { 32'b0, wr_data [63:32] };
         end
      end
   end

   return (wdata);
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
   Reg  #(AHB_Fabric_Addr)    rg_haddr          <- mkRegU;
   Reg  #(AHBL_Size)          rg_hsize          <- mkRegU;
   Reg  #(AHBL_Trans)         rg_htrans         <- mkReg (AHBL_IDLE);
   Reg  #(Bool)               rg_hwrite         <- mkRegU;

   // Input signals from AHBL bus
   Wire #(AHB_Fabric_Data)    wi_hrdata         <- mkBypassWire;
   Wire #(Bool)               wi_hready         <- mkBypassWire;
   Wire #(AHBL_Resp)          wi_hresp          <- mkBypassWire;

   // wdata for the data phase
   Reg #(AHB_Fabric_Data)     rg_hwdata         <- mkRegU;

   // Master state
   Reg #(AHB_Master_State)    rg_state          <- mkReg (IDLE);

   // ================================================================
   // BEHAVIOR

   // --------
   // Continuous signals
   let req = f_single_reqs.first;

   // A read request is waiting
   Bool read_request = req.is_read;

   // A write request is waiting. For writes, we need wr_addr and wr_data available as we
   // compute HSIZE from wr_data.wstrb
   Bool write_request = !read_request && f_single_write_data.notEmpty;

   let req_addr = fv_Addr_to_Fabric_Addr (req.addr);

   // --------
   // IDLE state: Entry point for new request. Setup addr phase signals.
   rule rl_nseq_req (wi_hready && (read_request || write_request) && (rg_state == IDLE));
      rg_htrans <= AHBL_NONSEQ;
      rg_haddr  <= req_addr;
      rg_hsize  <= fv_size_code_to_AHBL_Size (req.size_code);
      rg_hwrite <= write_request;
      rg_state  <= ADDR;
      if (verbosity > 0) begin
         $display ("%0d: %m.rl_nseq_req: (addr 0x%08h) ", cur_cycle, req_addr
                 , "(read: ", fshow (read_request), ") "
                 , "(size: ", fshow (fv_size_code_to_AHBL_Size (req.size_code)));
      end
   endrule


   // --------
   // ADDR state: Setup data phase signals/sample response.
   rule rl_complete_nseq_req (wi_hready && (rg_state == ADDR));
      rg_htrans <= AHBL_IDLE;
      rg_state  <= read_request ? RDATA : WDATA;

      AHB_Fabric_Data hwdata = truncate (fv_get_ahb_wdata (
         f_single_reqs.first, f_single_write_data.first));

      if (write_request)
         rg_hwdata <= hwdata;
      if (verbosity > 0) begin
         if (write_request)
            $display ("%0d: %m.rl_complete_nseq_req: (addr 0x%08h) (wdata 0x%08h)"
               , cur_cycle, req_addr, hwdata );
         else
            $display ("%0d: %m.rl_complete_nseq_req: (addr 0x%08h) ", cur_cycle, req_addr);
      end
   endrule


   // --------
   // RDATA state: Accept read responses. Advance request queue. Forward response to CPU.
   rule rl_read_response (wi_hready && (rg_state == RDATA));
      rg_state  <= IDLE;

      // Response handling and packing
      Bit #(64) data = zeroExtend (wi_hrdata);
      Bool ok = (wi_hresp == AHBL_OKAY);

      // FABRIC32 adjustments
      if (valueOf (AHB_Wd_Data) == 32) begin
         // Not a burst
         if (req.size_code != 2'b11) begin
            // B, H, W: only 1 beat. Adjust the data into the correct half and enq
            Bool in_upper32 = (req.addr[2] == 1'b1);
            if (in_upper32) data = { data [31:0], 32'b0 };
         end
      end

      let rsp = Read_Data { ok: ok, data: data };
      f_single_read_data.enq (rsp);

      // Advance request queue
      f_single_reqs.deq;
      if (verbosity > 0) begin
         $display ("%0d: %m.rl_read_response: ", cur_cycle, fshow (rsp));
      end
   endrule


   // --------
   // WDATA state: Advance request queue.
   rule rl_write_response (wi_hready && (rg_state == WDATA));
      rg_state  <= IDLE;

      // Advance request queue
      f_single_reqs.deq;
      f_single_write_data.deq;
      if (verbosity > 0) begin
         $display ("%0d: %m.rl_write_response", cur_cycle);
      end
   endrule


   // ================================================================
   // INTERFACE
   method Action reset;
      f_single_reqs.clear;
      f_single_write_data.clear;
      f_single_read_data.clear;
      rg_htrans <= AHBL_IDLE;
      rg_state <= IDLE;
      if (verbosity > 1)
         $display ("%0d: %m.reset", cur_cycle);
   endmethod


`ifdef STANDALONE
   // ----------------
   // interface for word/sub-word read/write client

   interface Put p_mem_single_req        = toPut (f_single_reqs);
   interface Put p_mem_single_write_data = toPut (f_single_write_data);
   interface Get g_mem_single_read_data  = toGet (f_single_read_data);
`endif


   // ----------------
   // AHBL side
   interface AHBL_Master_IFC mem_master;
      method haddr     = rg_haddr;
      method hburst    = AHBL_INCR;
      method hmastlock = False;
      method hprot     = defaultValue;
      method hsize     = rg_hsize;
      method htrans    = rg_htrans;
      method hwdata    = rg_hwdata;
      method hwrite    = rg_hwrite;

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
