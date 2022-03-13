// Copyright (c) 2017 Bluespec, Inc.  All Rights Reserved

package AHBL_Types;

// ================================================================
// Standard types for AHB-Lite: Master and Slave interfaces, and their
// bus data types.

// Ref:
//   "AMBA 3 AHB-Lite Protocol v1.0; Specification",
//   Document IHI 0033A, ARM Ltd., 2006

// ================================================================
// BSV library imports

import DefaultValue :: *;
import Connectable  :: *;

// ================================================================
// Bus data types

// HBURST (from master)
typedef enum {AHBL_SINGLE,    // 0
	      AHBL_INCR,      // 1
	      AHBL_WRAP4,     // 2
	      AHBL_INCR4,     // 3
	      AHBL_WRAP8,     // 4
	      AHBL_INCR8,     // 5
	      AHBL_WRAP16,    // 6
	      AHBL_INCR16     // 7
	      } AHBL_Burst
deriving (Bits, Eq, FShow);

// HPROT (from master)
typedef struct {
   Bool data_not_opcode;
   Bool privileged_not_user;
   Bool bufferable;
   Bool cacheable;
   } AHBL_Prot
deriving (Bits, Eq, FShow);

instance DefaultValue #(AHBL_Prot);
   defaultValue = AHBL_Prot {data_not_opcode:     True,
			     privileged_not_user: True,
			     bufferable:          False,
			     cacheable:           False};
endinstance

// HRESP (from slave)
//             0     1
typedef enum { AHBL_OKAY, AHBL_ERROR } AHBL_Resp
deriving (Bits, Eq, FShow);

// HSIZE (from master)
typedef enum {AHBL_BITS8,      // 0
	      AHBL_BITS16,     // 1
	      AHBL_BITS32,     // 2
	      AHBL_BITS64,     // 3
	      AHBL_BITS128,    // 4
	      AHBL_BITS256,    // 5
	      AHBL_BITS512,    // 6
	      AHBL_BITS1024    // 7
	      } AHBL_Size
deriving (Bits, Eq, FShow);

// HTRANS (from master)
typedef enum {AHBL_IDLE,      // 0
	      AHBL_BUSY,      // 1
	      AHBL_NONSEQ,    // 2
	      AHBL_SEQ        // 3
	      } AHBL_Trans
deriving (Bits, Eq, FShow);

// ================================================================
// AHB Master signal interface

(* always_ready, always_enabled *)
interface AHBL_Master_IFC #(numeric type wd_data);
   // Outputs
   (* result = "HADDR" *)     method Bit #(32)       haddr;
   (* result = "HBURST" *)    method AHBL_Burst      hburst;
   (* result = "HMASTLOCK" *) method Bool            hmastlock;
   (* result = "HPROT" *)     method AHBL_Prot       hprot;
   (* result = "HSIZE" *)     method AHBL_Size       hsize;
   (* result = "HTRANS" *)    method AHBL_Trans      htrans;
   (* result = "HWDATA" *)    method Bit #(wd_data)  hwdata;
   (* result = "HWRITE" *)    method Bool            hwrite;

   // Inputs
   (* prefix = "", result = "unused0" *)
   method Action hrdata ((* port = "HRDATA" *) Bit #(wd_data) data);

   (* prefix = "", result = "unused1" *)
   method Action hready ((* port = "HREADY" *) Bool  ready);

   (* prefix = "", result = "unused2" *)
   method Action hresp  ((* port = "HRESP" *)  AHBL_Resp resp);
endinterface

// ================================================================
// AHB Slave signal interface

(* always_ready, always_enabled *)
interface AHBL_Slave_IFC #(numeric type wd_data);
    // Inputs
   (* prefix = "", result = "unused0" *)
   method Action hsel      ((* port = "HSEL" *)      Bool            sel);
   (* prefix = "", result = "unused9" *)
   method Action hready    ((* port = "HREADY" *)    Bool            rdy);
   (* prefix = "", result = "unused1" *)
   method Action haddr     ((* port = "HADDR" *)     Bit #(32)       addr);
   (* prefix = "", result = "unused2" *)
   method Action hburst    ((* port = "HBURST" *)    AHBL_Burst      burst);
   (* prefix = "", result = "unused3" *)
   method Action hmastlock ((* port = "HMASTLOCK" *) Bool            mastlock);
   (* prefix = "", result = "unused4" *)
   method Action hprot     ((* port = "HPROT" *)     AHBL_Prot       prot);
   (* prefix = "", result = "unused5" *)
   method Action hsize     ((* port = "HSIZE" *)     AHBL_Size       size);
   (* prefix = "", result = "unused6" *)
   method Action htrans    ((* port = "HTRANS" *)    AHBL_Trans      trans);
   (* prefix = "", result = "unused7" *)
   method Action hwdata    ((* port = "HWDATA" *)    Bit #(wd_data)  data);
   (* prefix = "", result = "unused8" *)
   method Action hwrite    ((* port = "HWRITE" *)    Bool            write);

   // Outputs
   (* result = "HRDATA" *)    method Bit #(wd_data)  hrdata;
   (* result = "HREADYOUT" *) method Bool            hreadyout;
   (* result = "HRESP" *)     method AHBL_Resp       hresp;
endinterface

// ================================================================
// For debugging

function Action fa_display_bus_signals (Bit #(32)       haddr,
					AHBL_Burst      hburst,
					Bool            hmastlock,
					AHBL_Prot       hprot,
					AHBL_Size       hsize,
					AHBL_Trans      htrans,
					Bit #(wd_data)  hwdata,
					Bool            hwrite,
					Bit #(wd_data)  hrdata,
					Bool            hready,
					AHBL_Resp       hresp);
   action
      $display ("    haddr    : %08h", haddr);
      $display ("    hburst   : ",     fshow (hburst));
      $display ("    hmastlock: ",     fshow (hmastlock));
      $display ("    hprot    : %4b",  pack (hprot));
      $display ("    hsize    : ",     fshow (hsize));
      $display ("    htrans   : ",     fshow (htrans));
      $display ("    hwdata   : %08h", hwdata);
      $display ("    hwrite   : ",     fshow (hwrite));
      $display ("    hrdata   : %08h", hrdata);
      $display ("    hready   : ",     fshow (hready));
      $display ("    hresp    : ",     fshow (hresp));
   endaction
endfunction

// ================================================================
// Connecting AHB_Master_IFC directly AHB_Slave_IFC directly (no
// multi-layer interconnect

instance Connectable #(AHBL_Master_IFC #(wd_data),
		       AHBL_Slave_IFC  #(wd_data));

   module mkConnection #(AHBL_Master_IFC #(wd_data) master,
			 AHBL_Slave_IFC  #(wd_data) slave)
                       (Empty);

      // ----------------------------------------------------------------
      // Master to slave signals

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_connect_haddr;
	 slave.haddr (master.haddr);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_connect_hburst;
	 slave.hburst (master.hburst);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_connect_hmastlock;
	 slave.hmastlock (master.hmastlock);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_connect_hprot;
	 slave.hprot (master.hprot);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_connect_hsize;
	 slave.hsize (master.hsize);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_connect_htrans;
	 slave.htrans (master.htrans);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_connect_hwdata;
	 slave.hwdata (master.hwdata);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_connect_hwrite;
	 slave.hwrite (master.hwrite);
      endrule

      // ----------------------------------------------------------------
      // Slave to master signals

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_connect_hrdata;
	 master.hrdata (slave.hrdata);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_connect_hready;
	 master.hready (slave.hreadyout);
      endrule

      (* fire_when_enabled, no_implicit_conditions *)
      rule rl_connect_hresp;
	 master.hresp  (slave.hresp);
      endrule

      // Note:
      //     hsel and hready need to be driven separately
      //     e.g., by arbitration module?
   endmodule
endinstance

// ================================================================
// Dummy AHBL Master
// Always drives htrans = IDLE
// and other default value.

module mkDummy_AHBL_Master (AHBL_Master_IFC #(wd_data));
   return
   interface AHBL_Master_IFC;
      // Outputs
      method Bit #(32)       haddr     = 'hBAAD_AADD;
      method AHBL_Burst      hburst    = AHBL_SINGLE;
      method Bool            hmastlock = False;
      method AHBL_Prot       hprot     = defaultValue;
      method AHBL_Size       hsize     = AHBL_BITS32;
      method AHBL_Trans      htrans    = AHBL_IDLE;
      method Bit #(wd_data)  hwdata    = 'hBAAD_BEEF;
      method Bool            hwrite    = False;

      // Inputs
      method Action hrdata (Bit #(wd_data)  data)  = noAction;
      method Action hready (Bool            ready) = noAction;
      method Action hresp  (AHBL_Resp       resp)  = noAction;
   endinterface;
endmodule

// ================================================================
// Dummy Slave.  Always drives
// hready = True, hresp = OKAY and hdata = 'hFADAFADA;

module mkDummy_AHBL_Slave (AHBL_Slave_IFC #(wd_data));
   return
   interface AHBL_Slave_IFC;
      // Inputs
      method Action hsel      (Bool            sel)      = noAction;
      method Action hready    (Bool            rdy)      = noAction;
      method Action haddr     (Bit #(32)       addr)     = noAction;
      method Action hburst    (AHBL_Burst      burst)    = noAction;
      method Action hmastlock (Bool            mastlock) = noAction;
      method Action hprot     (AHBL_Prot       prot)     = noAction;
      method Action hsize     (AHBL_Size       size)     = noAction;
      method Action htrans    (AHBL_Trans      trans)    = noAction;
      method Action hwdata    (Bit #(wd_data)  data)     = noAction;
      method Action hwrite    (Bool            write)    = noAction;

      // Outputs
      method Bool            hreadyout = True;
      method AHBL_Resp       hresp     = AHBL_OKAY;
      method Bit #(wd_data)  hrdata    = 'hABCD_EF89;
   endinterface;
endmodule

// ================================================================
// AHBL data is aligned to byte lanes based on addr lsbs.
// This function replaces the appropriate bytes of 'old_word'
// with the appropriate bytes of HWDATA depending on the address LSBs and transfer size.
// Also returns err=True for unsupported 'size' and misaligned addrs.

function Bool fn_ahbl_is_aligned (Bit #(2) addr_lsbs, AHBL_Size size);
   let is_aligned = True;
   case (size)
      AHBL_BITS8  : return (True);
      AHBL_BITS16 : case (addr_lsbs)
                       2'b00: return (True);
                       2'b10: return (True);
                       default: return (False);
                    endcase
      AHBL_BITS32 : case (addr_lsbs)
                       2'b00: return (True);
                       default: return (False);
                    endcase
      default: return (False);
   endcase
endfunction

function Bit #(32) fn_ahbl_update_wdata (  Bit #(2)  addr_lsbs
                                         , AHBL_Size size
                                         , Bit #(32) old_word
                                         , Bit #(32) hwdata);

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
                   endcase
      AHBL_BITS32: case (addr_lsbs)
                      2'b00: new_word = hwdata;
                   endcase
   endcase
   return new_word;
endfunction

endpackage
