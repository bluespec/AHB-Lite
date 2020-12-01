// Copyright (c) 2017 Bluespec, Inc.  All Rights Reserved

package AHBL_Mem_Model;

// ================================================================
// A memory model that implements a model of memory:
// - Byte addressed
// - Address-range parameters 'addr_lo' to 'addr_hi'
// - AHB-Lite Slave interface with 32-bit data bus
// - Implements 'memory' with a RegFile of 32-bit words.
//    initialized by a mem hex file.
// - Supports 8b, 16b and 32b writes, based on AHBL HSIZE
//    Sub-word data is taken from proper byte lanes.
//    AHBL HADDR [1:0] and HSIZE are used to select byte lanes.
// - Supports 32b reads only (ignores HADDR [1:0]

// Ref:
//   "AMBA 3 AHB-Lite Protocol v1.0; Specification",
//   Document IHI 0033A, ARM Ltd., 2006

// ================================================================
// TODO:
// - make hex-file init optional

// ================================================================
// BSV lib imports

import DefaultValue :: *;
import RegFile      :: *;

// ================================================================
// Project imports

import AHBL_Types :: *;
import Testbench_Commons :: *;

// ================================================================
// The module
// Byte addressed.
// Implements 

(* synthesize *)
module mkAHBL_Mem_Model #(parameter Bit #(32) byte_addr_lo,
			  parameter Bit #(32) byte_addr_hi)
                        (AHBL_Slave_IFC #(32));

   Bit #(4) verbosity = 0;

   // ----------------
   // Cycle counts for debugging $displays

   Reg #(Bit #(32)) rg_cycle <- mkReg (10);

   rule rl_count_cycles;
      rg_cycle <= rg_cycle + 10;
   endrule

   // ----------------

   Bit #(30) word_addr_lo = 0;
   // bsc bug? If we use the following, we get a strange err in mkRegFileLoad()
   // Bit #(30) word_addr_lo = byte_addr_lo [31:2];
   Bit #(30) word_addr_hi = byte_addr_hi [31:2];

   // ----------------
   // AHB-Lite signals and registers

   // Inputs
   Wire #(Bool)        w_hsel      <- mkBypassWire;    Reg #(Bool)        rg_sel       <- mkReg (False);
   Wire #(Bit #(32))   w_haddr     <- mkBypassWire;    Reg #(Bit #(32))   rg_haddr     <- mkReg ({word_addr_lo, 2'b00});
   Wire #(AHBL_Burst)  w_hburst    <- mkBypassWire;    Reg #(AHBL_Burst)  rg_hburst    <- mkReg (AHBL_SINGLE);
   Wire #(Bool)        w_hmastlock <- mkBypassWire;    Reg #(Bool)        rg_hmastlock <- mkReg (False);
   Wire #(AHBL_Prot)   w_hprot     <- mkBypassWire;    Reg #(AHBL_Prot)   rg_hprot     <- mkReg (defaultValue);
   Wire #(AHBL_Size)   w_hsize     <- mkBypassWire;    Reg #(AHBL_Size)   rg_hsize     <- mkReg (AHBL_BITS32);
   Wire #(AHBL_Trans)  w_htrans    <- mkBypassWire;    Reg #(AHBL_Trans)  rg_htrans    <- mkReg (AHBL_NONSEQ);
   Wire #(Bit #(32))   w_hwdata    <- mkBypassWire;
   Wire #(Bool)        w_hwrite    <- mkBypassWire;    Reg #(Bool)        rg_hwrite    <- mkReg (False);
   Wire #(Bool)        w_hreadyin  <- mkBypassWire;

   // Outputs
   Wire #(Bool)        w_hreadyout <- mkBypassWire;
   Wire #(AHBL_Resp)   w_hresp     <- mkBypassWire;
   Wire #(Bit #(32))   w_hrdata    <- mkBypassWire;

   // ----------------
   // The Memory model
   // Addressed with 30-bit word-address; each location holds 4-byte word
   // TODO: for some reason the compile doesn't like word_addr_lo to be a module parameter.

   RegFile #(Bit #(30), Bit #(32)) rf <- mkRegFileLoad ("Mem.hex", word_addr_lo, word_addr_hi);

   Bool in_mem_bounds  = fn_is_mem0_controller_addr (rg_haddr);
   Bool in_uart_bounds = fn_is_uart0_addr (rg_haddr);
   Bool out_of_bounds  = (! (in_mem_bounds || in_uart_bounds));

   let word_addr    = rg_haddr [31:2];
   let byte_in_word = rg_haddr [1:0];
   let old_word     = rf.sub (in_mem_bounds ? word_addr : word_addr_hi);


   match { .misaligned, .new_word } = fn_replace_bytes (byte_in_word, rg_hsize, old_word, w_hwdata);

   // ================================================================
   // BEHAVIOR

   (* fire_when_enabled, no_implicit_conditions *)
   rule rl_addr_and_control;
      Bool sel = (w_hsel && w_hreadyin && (w_htrans != AHBL_IDLE));
      rg_sel <= sel;
      if (sel) begin
	 // Register fresh address-and-control inputs
	 rg_haddr     <= w_haddr;
	 rg_hburst    <= w_hburst;
	 rg_hmastlock <= w_hmastlock;
	 rg_hprot     <= w_hprot;
	 rg_hsize     <= w_hsize;
	 rg_htrans    <= w_htrans;
	 rg_hwrite    <= w_hwrite;

	 if (verbosity != 0)
	    $display ("%0d: AHBL_Mem_Model: haddr 0x%08h", rg_cycle, w_haddr,
		      fshow (w_hsize), " hwrite %0d htrans ", w_hwrite, fshow (w_htrans));
      end
   endrule

   rule rl_data;
      AHBL_Resp hresp = AHBL_OKAY;

      // Writes
      if (rg_sel && rg_hwrite && (rg_htrans != AHBL_BUSY)) begin
	 // Memory
	 if (in_mem_bounds) begin
	    rf.upd (word_addr, new_word);
	    if (verbosity != 0)
	       $display ("%0d:    write: [0x%08h]: 0x%08h <= 0x%08h", rg_cycle, {word_addr, 2'b0} , old_word, new_word);
	    hresp = AHBL_OKAY;
	 end

	 // IO
	 else if (in_uart_bounds) begin
	    Bit #(8) ascii_LF    = 'h0A;
	    Bit #(8) ascii_space = 'h20;

	    Bit #(8) ch = new_word [7:0];

	    if ((rg_haddr == 'hC000_0000) && ((ch == ascii_LF) || (ch >= ascii_space)))
	       $write ("%c", ch);
	    hresp = AHBL_OKAY;

	    if (verbosity != 0)
	       $display ("%0d:    UART out: [0x%08h] => 0x%08h", rg_cycle, {word_addr, 2'b0} , new_word);
	 end

	 // Out of bounds or misaligned
	 else begin
	    hresp = AHBL_ERROR;
	    if (verbosity != 0)
	       $display ("%0d:    write: AHBL_ERROR", rg_cycle);
	 end
      end

      // Reads
      else if (! rg_hwrite) begin
	 // Memory
	 if (in_mem_bounds) begin
	    if (verbosity != 0)
	       $display ("%0d:    read: [0x%08h] => rdata 0x%08h", rg_cycle, {word_addr, 2'b0} , old_word);
	    hresp = AHBL_OKAY;
	 end

	 // IO
	 else if (in_uart_bounds) begin
	    hresp = AHBL_OKAY;
	    if (verbosity != 0)
	       $display ("%0d:    UART in: [0x%08h]", rg_cycle, {word_addr, 2'b0});
	 end

	 // Out of bounds or misaligned
	 else begin
	    hresp = AHBL_ERROR;
	    if (verbosity != 0)
	       $display ("%0d:    read: AHBL_ERROR", rg_cycle);
	 end
      end

      // Drive outputs
      w_hreadyout <= True;
      w_hresp     <= hresp;
      w_hrdata    <= old_word;
   endrule

   // ================================================================
   // INTERFACE

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

   method Bool       hreadyout = w_hreadyout;
   method AHBL_Resp  hresp     = w_hresp;
   method Bit #(32)  hrdata    = w_hrdata;
endmodule

// ================================================================
// AHBL data is aligned to byte lanes based on addr lsbs.
// This function replaces the appropriate bytes of 'old_word'
// with the appropriate bytes of HWDATA depending on the address LSBs and transfer size.
// Also returns err=True for unsupported 'size' and misaligned addrs.

function Tuple2 #(Bool,
		  Bit #(32)) fn_replace_bytes (Bit #(2)   addr_lsbs,
					       AHBL_Size  size,
					       Bit #(32)  old_word,
					       Bit #(32)  hwdata);
   let err = False;
   let new_word = old_word;
   case (size)
      AHBL_BITS8:  case (addr_lsbs)
		      2'b00: new_word = { old_word [31:24], old_word [23:16], old_word [15:8], hwdata   [7:0] };
		      2'b01: new_word = { old_word [31:24], old_word [23:16], hwdata   [15:8], old_word [7:0] };
		      2'b10: new_word = { old_word [31:24], hwdata   [23:16], old_word [15:8], old_word [7:0] };
		      2'b11: new_word = { hwdata   [31:24], old_word [23:16], old_word [15:8], old_word [7:0] };
		   endcase
      AHBL_BITS16: case (addr_lsbs)
		      2'b00: new_word = { old_word [31:16], hwdata   [15:0] };
		      2'b10: new_word = { hwdata   [31:16], old_word [15:0] };
		      default: err = True;
		   endcase
      AHBL_BITS32: case (addr_lsbs)
		      2'b00: new_word = hwdata;
		      default: err = True;
		   endcase
      default: err = True;
   endcase
   return tuple2 (err, new_word);
endfunction

// ================================================================

endpackage
