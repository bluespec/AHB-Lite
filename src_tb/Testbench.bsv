// Copyright (c) 2017 Bluespec, Inc.  All Rights Reserved

package Testbench;

// ================================================================
// BSV library imports

import Connectable :: *;
import FIFOF :: *;
import GetPut :: *;

// ----------------
// Other lib imports


// ================================================================
// Project imports

import AHBL_Types       :: *;
import AHBL_Defs        :: *;
import AXI4_Types       :: *;

import TCM_AHBL_Adapter :: *;
// import AHBL_Mem_Model   :: *;
import AHBL_Target_AXI4_Initiator :: *;
// import AXI4_Mem_Model :: *;
import Testbench_Commons:: *;

// ================================================================

Bit #(0) default_user = ?;

// ================================================================
/* This TB is for verifying the TCM AHBL adapter directly with a AHB-L memory model
(* synthesize *)
module mkTestbench (Empty);

   Reg #(Bit #(32)) rg_cycle <- mkReg (10);

   // Requests and data to/from memory (AHB-L fabric)
   FIFOF #(Single_Req)        f_mem_req         <- mkFIFOF1;
   FIFOF #(Bit #(64))         f_mem_wdata       <- mkFIFOF1;
   FIFOF #(Read_Data)         f_mem_rdata       <- mkFIFOF1;

   Bit #(2) verbosity_ahbl = 2;
   TCM_AHBL_Adapter_IFC ahbl_adapter<- mkTCM_AHBL_Adapter (
      verbosity_ahbl);

   AHBL_Slave_IFC #(32) mem_model <- mkAHBL_Mem_Model (0, 'hFF);

   mkConnection (ahbl_adapter.mem_master, mem_model);

   rule rl_dummy_AHBL_decoder_and_mux;
      mem_model.hsel (True);
      mem_model.hreadyin (mem_model.hreadyout);
   endrule

   Bit #(0) default_user = ?;

   rule rl_count_cycles;
      rg_cycle <= rg_cycle + 10;
   endrule

   // ----------------

   Reg #(Bit #(32)) rg_state <- mkReg (0);

   rule rl_wr_req (rg_state < 4);
      let wra = Single_Req {addr: (rg_state << 2),
			    is_read: False,
                            size_code: 2'b10};

      Bit #(64) wrd = extend (rg_state);

      f_mem_req.enq (wra);
      f_mem_wdata.enq (wrd);

      rg_state <= rg_state + 1;
      $display ("%0d: %m.rl_wr_req: ", cur_cycle, fshow (wra), " 0x%016h", wrd);
   endrule

   rule rl_rd_req ((rg_state >= 4) && (rg_state < 8));
      let rda = Single_Req {addr: (rg_state << 2),
			    is_read: True,
			    size_code: 2'b10};
      f_mem_req.enq (rda);
      rg_state <= rg_state + 1;
      $display ("%0d: %m.rl_rd_req: ", rg_cycle, fshow (rda));
   endrule

   rule rl_rd_rsp;
      let rdd = f_mem_rdata.first; f_mem_rdata.deq;
      $display ("%0d: %m.rl_rd_rsp: ", rg_cycle, fshow (rdd));
   endrule

   rule connect_req;
      let req = f_mem_req.first; f_mem_req.deq;
      ahbl_adapter.p_mem_single_req.put (req);
   endrule

   rule connect_wdata;
      let wdata = f_mem_wdata.first; f_mem_wdata.deq;
      ahbl_adapter.p_mem_single_write_data.put (wdata);
   endrule

   rule connect_rsp;
      let rsp <- ahbl_adapter.g_mem_single_read_data.get ();
      f_mem_rdata.enq (rsp);
   endrule

endmodule
*/

// --------
// Testbench with AHB-L <-> AXI4 transactor
// This TB is for verifying the TCM AHBL adapter via a AHBL-AXI4 transactor
(* synthesize *)
module mkTestbench_AXI4 (AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User));

   Reg #(Bit #(32)) rg_cycle <- mkReg (10);

   // Requests and data to/from memory (AHB-L fabric)
   FIFOF #(Single_Req)        f_mem_req         <- mkFIFOF1;
   FIFOF #(Bit #(64))         f_mem_wdata       <- mkFIFOF1;
   FIFOF #(Read_Data)         f_mem_rdata       <- mkFIFOF1;

   Bit #(2) verbosity_ahbl = 2;
   TCM_AHBL_Adapter_IFC ahbl_adapter<- mkTCM_AHBL_Adapter (
      verbosity_ahbl);

   AHBL_Target_AXI4_Initiator xactor <- mkAHBL_Target_AXI4_Initiator;

   mkConnection (ahbl_adapter.mem_master, xactor.ahbl_target);

   rule rl_dummy_AHBL_decoder_and_mux;
      xactor.ahbl_target.hsel (True);
   endrule

   Bit #(0) default_user = ?;

   // ----------------

   Reg #(Bit #(32)) rg_state <- mkReg (0);

   rule rl_init_mem (rg_state == 0);
      xactor.reset;
      ahbl_adapter.reset;
      rg_state <= rg_state + 1;
   endrule

   rule rl_wr_req (rg_state < 4);
      let wra = Single_Req {addr: (rg_state << 2),
			    is_read: False,
                            size_code: 2'b10};

      Bit #(64) wrd = extend (rg_state);

      f_mem_req.enq (wra);
      f_mem_wdata.enq (wrd);

      rg_state <= rg_state + 1;
      $display ("%0d: %m.rl_wr_req: ", cur_cycle, fshow (wra), " 0x%016h", wrd);
   endrule

   rule rl_rd_req ((rg_state >= 4) && (rg_state < 8));
      let rda = Single_Req {addr: (rg_state << 2),
			    is_read: True,
			    size_code: 2'b10};
      f_mem_req.enq (rda);
      rg_state <= rg_state + 1;
      $display ("%0d: %m.rl_rd_req: ", rg_cycle, fshow (rda));
   endrule

   rule rl_rd_rsp;
      let rdd = f_mem_rdata.first; f_mem_rdata.deq;
      $display ("%0d: %m.rl_rd_rsp: ", rg_cycle, fshow (rdd));
   endrule

   rule connect_req;
      let req = f_mem_req.first; f_mem_req.deq;
      ahbl_adapter.p_mem_single_req.put (req);
   endrule

   rule connect_wdata;
      let wdata = f_mem_wdata.first; f_mem_wdata.deq;
      ahbl_adapter.p_mem_single_write_data.put (wdata);
   endrule

   rule connect_rsp;
      let rsp <- ahbl_adapter.g_mem_single_read_data.get ();
      f_mem_rdata.enq (rsp);
   endrule

   return (xactor.axi4_initiator);

endmodule

// ================================================================

endpackage
