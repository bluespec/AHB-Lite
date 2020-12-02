// Copyright (c) 2020 Bluespec, Inc.  All Rights Reserved

package AHBL_Defs;

// ================================================================
// AHB-Lite bus widths and fabric definitions.
//
// ================================================================

// NOTE: Only FABRIC32 is usable in this release.

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

endpackage
