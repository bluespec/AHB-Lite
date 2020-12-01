// Copyright (c) 2018-2019 Bluespec, Inc. All Rights Reserved

// Top-level driver for "verilated" objects (Verilog compiled with verilator)

#include <verilated.h>

#include <sys/stat.h>  // for 'mkdir'

#include "VmkTestbench_edited.h"

// If "verilator --trace" is used, include the tracing class
#if VM_TRACE
# include <verilated_vcd_c.h>
#endif

vluint64_t main_time = 0;    // Current simulation time

double sc_time_stamp () {    // Called by $time in Verilog
    return main_time;
}

int main (int argc, char **argv, char **env) {
    Verilated::commandArgs (argc, argv);    // remember args

    VmkTestbench_edited* mkTestbench = new VmkTestbench_edited;    // create instance of model

#if VM_TRACE
    // If verilator was invoked with --trace argument,
    // and if at run time passed the +trace argument, turn on tracing
    VerilatedVcdC* tfp = NULL;
    const char* flag = Verilated::commandArgsPlusMatch("trace");
    if (flag && 0==strcmp(flag, "+trace")) {
        Verilated::traceEverOn(true);  // Verilator must compute traced signals
        VL_PRINTF("Enabling waves into vcd/vlt_dump.vcd...\n");
        tfp = new VerilatedVcdC;
        mkTestbench->trace(tfp, 99);  // Trace 99 levels of hierarchy
        mkdir("vcd", 0777);
        tfp->open("vcd/vlt_dump.vcd");  // Open the dump file
    }
#endif

    // initial conditions in order to generate appropriate edges on
    // reset
    mkTestbench->RST_N = 1;
    mkTestbench->CLK = 0;

    while (! Verilated::gotFinish ()) {

	if (main_time == 2) {
	    mkTestbench->RST_N = 0;    // assert reset
	}
	else if (main_time == 7) {
	    mkTestbench->RST_N = 1;    // Deassert reset
	}

	// Toggle clock
	if ((main_time % 10) == 5) {
	    mkTestbench->CLK = 1;
	}
	else if ((main_time % 10) == 0) {
	    mkTestbench->CLK = 0;
	}

#if VM_TRACE
	if (tfp)
	    tfp->dump(main_time);
#endif

	mkTestbench->eval ();
	main_time++;
    }

    mkTestbench->final ();    // Done simulating

    // Close trace if opened
#if VM_TRACE
    if (tfp) { tfp->close(); }
#endif

    delete mkTestbench;
    mkTestbench = NULL;

    exit (0);
}
