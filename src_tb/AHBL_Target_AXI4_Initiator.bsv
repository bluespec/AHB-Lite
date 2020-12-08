// Copyright (c) 2017 Bluespec, Inc.  All Rights Reserved

package AHBL_Target_AXI4_Initiator;

// ================================================================
// A transactor which connects as an AHB-L target on the AHB, and
// behaves as an initiator on the AXI4. 
//
// Works with FABRIC32 on the AHB-L and both FABRIC32 and FABRIC64
// on AXI4. Does not support bursts on the AHB-L.
//
// This implementation depends on the AXI4 adapters for AXI4
// signalling and compliance.
//
// ================================================================
// TODO:
// - make hex-file init optional

// ================================================================
// BSV lib imports

import DefaultValue  :: *;
import RegFile       :: *;
import FShow         :: *;

// ================================================================
// Project imports

import AHBL_Types :: *;
import AHBL_Defs  :: *;
`ifdef STANDALONE
import Testbench_Commons   :: *;
`else
import MMU_Cache_Common    :: *;
import Cur_Cycle           :: *;
`endif

// ================================================================
// Local type definitions

typedef enum { RDY, WDATA, RDATA } AHB_Target_State deriving (Bits, Eq, FShow);

typedef struct {
   Bool        upper32;
   Bool        is_read;
   AHBL_Trans  htrans; 
} Req_Control deriving (Bits, Eq, FShow);

// ----------------------------------------------------------------
// Utility functions
//
// Convert AHBL_Size code into code for request (number of bytes in a beat).
function Bit #(2)  fv_AHBL_Size_to_Req_Size (AHBL_Size ahbl_size);
   return (truncate (pack (ahbl_size)));
endfunction

// Adjust outgoing write data based on FABRIC-32/64
function Bit #(64) fv_get_axi4_wdata (Req_Control ctrl, AHB_Fabric_Data wd);
   Bit #(64) wdata = extend (wd);
   // FABRIC32 adjustments
   if (valueOf (AHB_Wd_Data) == 32) begin
      Bool in_upper32 = (req.addr [2] == 1'b1);
      if (in_upper32) wdata = {wd, 32'b0};
   end

   return (wdata);
endfunction

// Convert AXI4 64-bit response data to byte lane adjusted AHB data
function AHB_Fabric_Data fv_get_ahbl_rdata (Req_Control ctrl, Bit #(64) rdata);
   return (ctrl.upper32 ? rdata [63:32] : rdata [31:0]);
endfunction

// ================================================================
// The module

interface AHBL_Target_AXI4_Initiator;
   method Action reset;
   interface AHBL_Slave_IFC #(AHB_Wd_Data) ahbl_target;
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) axi4_initiator;
endinterface

(* synthesize *)
module mkAHBL_Target_AXI4_Initiator (AHBL_Target_AXI4_Initiator);

   Bit #(2) verbosity = 0;

   // ----------------

   Bit #(30) word_addr_lo = 0;
   // bsc bug? If we use the following, we get a strange err in mkRegFileLoad()
   // Bit #(30) word_addr_lo = byte_addr_lo [31:2];
   Bit #(30) word_addr_hi = byte_addr_hi [31:2];

   // ----------------
   // AHB-Lite signals and registers

   // Inputs
   Wire #(Bool)        w_hsel      <- mkBypassWire;
   Wire #(Bit #(32))   w_haddr     <- mkBypassWire;
   Wire #(AHBL_Burst)  w_hburst    <- mkBypassWire;
   Wire #(Bool)        w_hmastlock <- mkBypassWire;
   Wire #(AHBL_Prot)   w_hprot     <- mkBypassWire;
   Wire #(AHBL_Size)   w_hsize     <- mkBypassWire;
   Wire #(AHBL_Trans)  w_htrans    <- mkBypassWire;
   Wire #(Bit #(32))   w_hwdata    <- mkBypassWire;
   Wire #(Bool)        w_hwrite    <- mkBypassWire;
   Wire #(Bool)        w_hreadyin  <- mkBypassWire;

   // Outputs
   Wire #(Bool)        w_hreadyout <- mkBypassWire;
   Reg  #(AHB_Fabric_Data) rg_rdata<- mkRegU;

   // ----------------
   // The state of the AHB-L Target
   Reg  #(AHB_Target_State) rg_state <- mkReg (RDY);

   // ----------------
   // The AXI4 Initiator
   // Request and response FIFOs shared with the AXI4 side
   FIFOF #(Single_Req) f_single_reqs <- mkFIFOF1;
   FIFOF #(Req_Control) f_req_control <- mkFIFOF1;
   FIFOF #(Bit #(64))  f_single_write_data <- mkFIFOF1;
   FIFOF #(Read_Data)  f_single_read_data <- mkFIFOF1;

   Bit#(2) verbosity_fabric = 0;
   TCM_AXI4_Adapter_IFC axi4_adapter<- mkTCM_AXI4_Adapter (
      verbosity_fabric, f_single_reqs, f_single_write_data, f_single_read_data);

   // ================================================================
   // BEHAVIOR

   rule rl_ready_for_new_req (rg_state == RDY);
      let nstate = rg_state;
      Bool sel = (w_hsel && hreadyin && (w_htrans != AHBL_IDLE));
      if (sel) begin
	 // Register fresh address-and-control inputs as request to AXI4
         let req = Single_Req {
              is_read   : !w_hwrite
            , addr      : w_haddr
            , size_code : fv_AHBL_Size_to_Req_Size (w_hsize)
         };

         // Control information used in the data phase
         f_req_control.enq (Req_Control {
              upper32 : (req.addr [2] == 1'b1)
            , is_read : !w_hwrite
            , htrans  : w_htrans
         });

         f_single_reqs.enq (req);
         nstate = w_hwrite ? WDATA : RDATA;
      end
      rg_state <= nstate;
   endrule

   // Forward the write data to the AXI
   rule rl_write_data (rg_state == WDATA);
      f_single_write_data.enq (fv_get_axi4_wdata (f_req_control.first, w_hwdata));
      f_req_control.deq;
      rg_state <= RDY;

     if (verbosity != 0)
        $display ("%0d: %m.rl_wdata: 0x%08h", cur_cycle, w_hwdata);
   endrule

   // Forward the read response from the AXI when it is available
   rule rl_rdata (rg_state == RDATA);
      rg_state <= RDY;
      let rrsp = f_single_read_data.first; f_single_read_data.deq;
      rg_rdata <= fv_get_ahbl_data (f_req_control.first, rrsp.data);
      f_req_control.deq;
   endrule

   // ================================================================
   // INTERFACE
   method Action reset;
      f_single_reqs.clear;
      f_single_write_data.clear;
      f_single_read_data.clear;
      f_req_control.clear;
      rg_state <= RDY;
      if (verbosity > 1)
         $display ("%0d: %m.reset", cur_cycle);
   endmethod

   // AHB-Lite Target Interface
   interface AHBL_Slave_IFC ahbl_target;
      // ----------------
      // Inputs
      method Action hsel (Bool sel);
         w_hsel <= sel;
      endmethod

      method Action haddr (Bit #(32) addr);
         w_haddr <= addr;
      endmethod

      method Action hburst (AHBL_Burst burst);
         w_hburst <= burst;
      endmethod

      method Action hmastlock (Bool mastlock);
         w_hmastlock <= mastlock;
      endmethod

      method Action hprot (AHBL_Prot prot);
         w_hprot <= prot;
      endmethod

      method Action hsize (AHBL_Size size);
         w_hsize <= size;
      endmethod

      method Action htrans (AHBL_Trans trans);
         w_htrans <= trans;
      endmethod

      method Action hwdata(Bit #(32) data);
         w_hwdata <= data;
      endmethod

      method Action hwrite (Bool write);
         w_hwrite <= write;
      endmethod

      method Action hreadyin (Bool readyin);
         w_hreadyin <= readyin;
      endmethod

      // ----------------
      // Outputs

      method Bool       hreadyout = (rg_state == RDY);
      method AHBL_Resp  hresp     = AHBL_OKAY;
      method Bit #(32)  hrdata    = rg_rdata;
   endinterface

   interface AXI4_Master_IFc axi4_initiator = axi4_adapter.mem_master;

endmodule

// ================================================================

endpackage
