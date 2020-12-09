// Copyright (c) 2020 Bluespec, Inc.  All Rights Reserved

// This file is meant only for stand-alone (unit level) verification. It contains type
// definitions and utility functions which are otherwise imported from project-level files.

package Testbench_Commons;
import FShow :: *;
import AXI4_Types :: *;

// Requests and responses between:
//     MMIO     <-> TCM_AHBL_Adapter

// Single requests are from MMIO for 1, 2, 4 or 8 bytes.
typedef struct {
   Bool       is_read;
   Bit #(32)  addr;
   Bit #(2)   size_code;    // 2'b00=1 (B), 01=2 (H), 10=4 (W), 11=8 (D) bytes
} Single_Req deriving (Bits, FShow);

// Single Responses
typedef struct {
   Bool       ok;
   Bit #(64)  data;
} Read_Data deriving (Bits, FShow);

// A convenience function to return the current cycle number during BSV simulations

ActionValue #(Bit #(32)) cur_cycle = actionvalue
					Bit #(32) t <- $stime;
					return t / 10;
				     endactionvalue;

// ----------------------------------------------------------------
// "ISA-Decls" Definitions
`ifdef RV32
typedef 32 XLEN;
`elsif RV64
typedef 64 XLEN;
`endif
typedef  Bit #(XLEN)  WordXL;    // Raw (unsigned) register data
typedef  WordXL       Addr;      // addresses/pointers

// ----------------------------------------------------------------
// "SoC-Map" Definitions
// Memory controller

Bit #(32) mem0_controller_addr_base = 'h_0000_0000;
Bit #(32) mem0_controller_addr_size = 'h_1000_0000;    // 256 MB
Bit #(32) mem0_controller_addr_lim  = mem0_controller_addr_base + mem0_controller_addr_size;

function Bool fn_is_mem0_controller_addr (Bit #(32) addr);
   return ((mem0_controller_addr_base <= addr) && (addr < mem0_controller_addr_lim));
endfunction

// ----------------------------------------------------------------
// UART 0

Bit #(32) uart0_addr_base = 'hC000_0000;
Bit #(32) uart0_addr_size = 'h0000_0080;    // 128
Bit #(32) uart0_addr_lim  = uart0_addr_base + uart0_addr_size;

function Bool fn_is_uart0_addr (Bit #(32) addr);
   return ((uart0_addr_base <= addr) && (addr < uart0_addr_lim));
endfunction


// ----------------------------------------------------------------
// AXI4 Fabric Definitions
typedef 4 Wd_Id;
typedef  Bit #(Wd_Id)  Fabric_Id;

typedef 32   Wd_Addr;
typedef  Bit #(Wd_Addr)      Fabric_Addr;
typedef  TDiv #(Wd_Addr, 8)  Bytes_per_Fabric_Addr;
Integer  bytes_per_fabric_addr = valueOf (Bytes_per_Fabric_Addr);

typedef 32   Wd_Data;
typedef  Bit #(Wd_Data)             Fabric_Data;
typedef  Bit #(TDiv #(Wd_Data, 8))  Fabric_Strb;
typedef  TDiv #(Wd_Data, 8)         Bytes_per_Fabric_Data;
Integer  bytes_per_fabric_data = valueOf (Bytes_per_Fabric_Data);

typedef  0               Wd_User;
typedef  Bit #(Wd_User)  Fabric_User;

Fabric_Id    fabric_default_id       = 0;
AXI4_Burst   fabric_default_burst    = axburst_incr;
AXI4_Lock    fabric_default_lock     = axlock_normal;
AXI4_Cache   fabric_default_arcache  = arcache_dev_nonbuf;
AXI4_Cache   fabric_default_awcache  = awcache_dev_nonbuf;
AXI4_Prot    fabric_default_prot     = { axprot_2_data, axprot_1_secure, axprot_0_unpriv };
AXI4_QoS     fabric_default_qos      = 0;
AXI4_Region  fabric_default_region   = 0;
Fabric_User  fabric_default_user     = ?;
endpackage

