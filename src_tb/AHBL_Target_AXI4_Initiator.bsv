// Copyright (c) 2017 Bluespec, Inc.  All Rights Reserved

package AHBL_Target_AXI4_Initiator;

// ================================================================
// A transactor which connects as an AHB-L target on the AHB, and
// behaves as an initiator on the AXI4. 
//
// Works with FABRIC32 on the AHB-L and both FABRIC32 and FABRIC64
// on AXI4. Does not support bursts on the AHB-L.
//
// ================================================================
// TODO:
// - make hex-file init optional

// ================================================================
// BSV lib imports

import DefaultValue  :: *;
import FIFOF         :: *;
import FShow         :: *;

// ================================================================
// Project imports

import AXI4_Types :: *;
import AHBL_Types :: *;
import AHBL_Defs  :: *;
`ifdef STANDALONE
import Testbench_Commons   :: *;
`else
import MMU_Cache_Common    :: *;
import Fabric_Defs         :: *;
import Cur_Cycle           :: *;
`endif
import Semi_FIFOF       :: *;

// ================================================================
// Local type definitions

typedef enum { RDY, WDATA, RDATA } AHB_Target_State deriving (Bits, Eq, FShow);

// ----------------------------------------------------------------
// Utility functions
//
// Convert AHBL_Size code into code for request (number of bytes in a beat).
function AXI4_Size fv_AHBL_Size_to_AXI4_Size (AHBL_Size ahbl_size);
   return (pack (ahbl_size));
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

   // Outputs
   Reg  #(AHB_Fabric_Data) rg_rdata<- mkRegU;

   // ----------------
   // The state of the AHB-L Target
   Reg  #(AHB_Target_State) rg_state <- mkReg (RDY);

   // ----------------
   // The AXI4 Initiator
   // Request and response FIFOs shared with the AXI4 side
   FIFOF #(Tuple3 #(AXI4_Size,    // size_code
		    Bit #(1),     // addr bit [2]
		    Bit #(8)))    // Num beats read-data
         f_rd_rsp_control <- mkFIFOF1;
   FIFOF #(Tuple3 #(AXI4_Size,    // size_code
		    Bit #(3),     // addr lsbs
		    Bit #(8)))    // Num beats in write-data
         f_wr_data_control <- mkFIFOF1;

   // AXI4 fabric request/response
   AXI4_Master_Xactor_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) master_xactor <- mkAXI4_Master_Xactor;

   // ================================================================
   // BEHAVIOR

   Reg #(Bit #(2)) rg_wr_rsps_pending <- mkReg (0);
   rule rl_ahbl_new_req (rg_state == RDY);
      let nstate = rg_state;
      Bool sel = (w_hsel && (w_htrans != AHBL_IDLE));
      if (sel) begin
         AXI4_Size   fabric_size = fv_AHBL_Size_to_AXI4_Size (w_hsize);
         Fabric_Addr fabric_addr = pack (w_haddr);

         if (verbosity >= 1)
            $display ("%0d: %m.rl_ahbl_new_req:\n    AXI4_Rd_Addr{araddr %0h arlen 0 (burst length 1) ",
                      cur_cycle, fabric_addr,  fshow_AXI4_Size (fabric_size), "}");

         Bit #(8)    num_beats   = 1;

         // Note: AXI4 codes a burst length of 'n' as 'n-1'
         AXI4_Len fabric_len = num_beats - 1;

         // read request
         if (!w_hwrite) begin
            nstate = RDATA;
            let mem_req_rd_addr = AXI4_Rd_Addr {arid:     fabric_default_id,
                                                araddr:   fabric_addr,
                                                arlen:    0,           // burst len = arlen+1
                                                arsize:   fabric_size,
                                                arburst:  fabric_default_burst,
                                                arlock:   fabric_default_lock,
                                                arcache:  fabric_default_arcache,
                                                arprot:   fabric_default_prot,
                                                arqos:    fabric_default_qos,
                                                arregion: fabric_default_region,
                                                aruser:   fabric_default_user};
            master_xactor.i_rd_addr.enq (mem_req_rd_addr);

            f_rd_rsp_control.enq (tuple3 (fabric_size, fabric_addr [2], num_beats));
         end

         // write request
         else begin
            nstate = WDATA;
            let mem_req_wr_addr = AXI4_Wr_Addr {awid:     fabric_default_id,
                                                awaddr:   fabric_addr,
                                                awlen:    0,           // burst len = arlen+1
                                                awsize:   fabric_size,
                                                awburst:  fabric_default_burst,
                                                awlock:   fabric_default_lock,
                                                awcache:  fabric_default_arcache,
                                                awprot:   fabric_default_prot,
                                                awqos:    fabric_default_qos,
                                                awregion: fabric_default_region,
                                                awuser:   fabric_default_user};
            master_xactor.i_wr_addr.enq (mem_req_wr_addr);

            f_wr_data_control.enq (tuple3 (fabric_size, fabric_addr [2:0], num_beats));
            rg_wr_rsps_pending <= rg_wr_rsps_pending + 1;
         end
      end
      rg_state <= nstate;
   endrule

   // Forward the write data to the AXI - only works for FABRIC32
   rule rl_write_data (rg_state == WDATA);
      Bool last = True;
      f_wr_data_control.deq;

      match {.wr_req_size_code,
             .wr_req_addr_lsbs,
             .wr_req_beats } = f_wr_data_control.first;

      Bit #(4)  strb = case (wr_req_size_code)
			  axsize_1: 4'h_1;
			  axsize_2: 4'h_3;
			  axsize_4: 4'h_F;
                          default: 4'h0;  // should never get here
                       endcase;
      let mem_req_wr_data = AXI4_Wr_Data {wdata:  w_hwdata,
					  wstrb:  strb,
					  wlast:  last,
					  wuser:  fabric_default_user};
      master_xactor.i_wr_data.enq (mem_req_wr_data);
      rg_state <= RDY;

      if (verbosity >= 1) begin
	 $display ("%0d: %m.rl_write_data: beat %0d/%0d", cur_cycle);
	 $display ("    AXI4_Wr_Data{%0h strb %0h last %0d}", w_hwdata, strb, pack (last));
      end
   endrule

   // Forward the read response from the AXI when it is available. Does not process read errors.
   rule rl_rdata (rg_state == RDATA);
      rg_state <= RDY;
      let rd_data <- pop_o (master_xactor.o_rd_data);
      Bool      ok   = (rd_data.rresp == axi4_resp_okay);
      rg_rdata <= rd_data.rdata;
      f_rd_rsp_control.deq;
   endrule

   // ****************************************************************
   // BEHAVIOR: WRITE RESPONSES - responses discarded
   // The following register identifies the client

   // Record errors on write-responses from mem
   Reg #(Bool) rg_write_error <- mkReg (False);
   rule rl_write_rsp;
      let wr_resp <- pop_o (master_xactor.o_wr_resp);

      Bool err = False;
      if (rg_wr_rsps_pending == 0) begin
	 rg_write_error <= True;

	 $display ("%0d: %m.rl_write_rsp: ERROR not expecting any write-response:", cur_cycle);
	 $display ("    ", fshow (wr_resp));
      end
      else begin
	 rg_wr_rsps_pending <= rg_wr_rsps_pending - 1;
	 if (wr_resp.bresp != axi4_resp_okay) begin
	    rg_write_error <= True;
	    if (verbosity >= 1) begin
	       $display ("%0d: %m.rl_write_rsp: FABRIC RESPONSE ERROR", cur_cycle);
	       $display ("    ", fshow (wr_resp));
	    end
	 end
	 else if (verbosity >= 1) begin
	    $display ("%0d: %m.rl_write_rsp: pending=%0d, ",
		      cur_cycle, rg_wr_rsps_pending, fshow (wr_resp));
	 end
      end
   endrule

   // ================================================================
   // INTERFACE
   method Action reset;
      master_xactor.reset;
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

      // ----------------
      // Outputs

      method Bool       hreadyout = (rg_state == RDY);
      method AHBL_Resp  hresp     = AHBL_OKAY;
      method Bit #(32)  hrdata    = rg_rdata;
   endinterface

   interface AXI4_Master_IFC axi4_initiator = master_xactor.axi_side;

endmodule

// ================================================================

endpackage
