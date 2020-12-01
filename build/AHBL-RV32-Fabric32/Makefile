# ================================================================
# Copyright (c) 2017 Bluespec, Inc.  All Rights Reserved.

# ================================================================
# Please modify the following for your installation and setup
#
REPO ?= ../..

# ================================================================
#
BSC_COMPILATION_FLAGS += \
			 -D RV32 \
			 -D FABRIC32 \
			 -D STANDALONE \

# ================================================================
# Search path for bsc for .bsv files
SRC_DIRS = $(REPO)/src
TESTBENCH_DIRS = $(REPO)/src_tb
BSC_PATH = $(SRC_DIRS):$(TESTBENCH_DIRS):+

# ----------------
# Top-level file and module
TOPFILE   ?= $(REPO)/src_tb/Testbench.bsv
TOPMODULE ?= mkTestbench

# ================================================================
# bsc compilation flags

BSC_COMPILATION_FLAGS += \
	-keep-fires -aggressive-conditions -no-warn-action-shadowing -no-show-timestamps -check-assert \
	-suppress-warnings G0020    \
	+RTS -K128M -RTS  -show-range-conflict


# ================================================================
# Generate Verilog RTL from BSV sources (needs Bluespec 'bsc' compiler)

RTL_GEN_DIRS  = -vdir Verilog_RTL  -bdir build_dir  -info-dir build_dir
BSIM_TMP_DIRS = -bdir build_dir  -simdir build_dir  -info-dir build_dir

build_dir:
	mkdir -p $@

Verilog_RTL:
	mkdir -p $@

.PHONY: compile
compile:  build_dir  Verilog_RTL
	@echo  "INFO: Verilog RTL generation ..."
	bsc -u -elab -sim  $(BSIM_TMP_DIRS)  $(BSC_COMPILATION_FLAGS)  -p $(BSC_PATH)  $(TOPFILE)
	bsc -u -elab -verilog  $(RTL_GEN_DIRS)  $(BSC_COMPILATION_FLAGS)  -p $(BSC_PATH)  $(TOPFILE)
	@echo  "INFO: Verilog RTL generation finished"

# ================================================================
# Compile and link Verilog RTL sources into a simulator executable

SIM_EXE_FILE = exe_HW_sim

# ================================================================
# Verilator Simulator
#
# Verilator flags: notes
#    stats              Dump stats on the design, in file {prefix}__stats.txt
#    -O3                Verilator optimization level
#    -CFLAGS -O3        C++ optimization level
#    --x-assign fast    Optimize X value
#    --x-initial fast   Optimize uninitialized value
#    --noassert         Disable all assertions

VERILATOR_FLAGS = --stats -O3 -CFLAGS -O3 -LDFLAGS -static --x-assign fast --x-initial fast --noassert

# Verilator flags: use the following to include code to generate VCDs
# Select trace-depth according to your module hierarchy
VERILATOR_FLAGS += --trace  --trace-depth 10  -CFLAGS -DVM_TRACE

VTOP                = V$(TOPMODULE)_edited
VERILATOR_RESOURCES = $(REPO)/build/Resources/Verilator_resources

.PHONY: verilator_simulator
verilator_simulator:
	@echo "INFO: Verilating Verilog files (in newly created obj_dir)"
	sed  -f $(VERILATOR_RESOURCES)/sed_script.txt  Verilog_RTL/$(TOPMODULE).v > tmp1.v
	cat  $(VERILATOR_RESOURCES)/verilator_config.vlt \
	     $(VERILATOR_RESOURCES)/import_DPI_C_decls.v \
	     tmp1.v                                     > Verilog_RTL/$(TOPMODULE)_edited.v
	rm   -f  tmp1.v
	verilator \
		-IVerilog_RTL \
		-I$(REPO)/src_bsc_lib_RTL \
		$(VERILATOR_FLAGS) \
		--cc  $(TOPMODULE)_edited.v \
		--exe  sim_main.cpp \
		$(REPO)/src_tb/C_Imported_Functions.c
	@echo "INFO: Linking verilated files"
	cp  -p  $(VERILATOR_RESOURCES)/sim_main.cpp  obj_dir/sim_main.cpp
	cd obj_dir; \
	   make -j -f V$(TOPMODULE)_edited.mk  $(VTOP); \
	   cp -p  $(VTOP)  ../$(SIM_EXE_FILE)
	@echo "INFO: Created verilator executable:    $(SIM_EXE_FILE)"

# ================================================================
# Bluesim Simulator
#
BSC_C_FLAGS += \
	-Xl -v \
	-Xc -O3 -Xc++ -O3 \

# You may have to remove this line above
# for Bluespec_2019.05.beta2-debian9stretch-amd64
#	-Xc++  -D_GLIBCXX_USE_CXX11_ABI=0 \

.PHONY: bluesim_simulator
bluesim_simulator:
	@echo "INFO: linking bsc-compiled objects into Bluesim executable"
	bsc -sim -parallel-sim-link 8 \
		$(BSIM_TMP_DIRS) \
		-e $(TOPMODULE) -o ./$(SIM_EXE_FILE) \
		$(BSC_C_FLAGS) \
		$(REPO)/src_tb/C_Imported_Functions.c
	@echo "INFO: linked bsc-compiled objects into Bluesim executable"

# ================================================================

.PHONY: clean
clean:
	rm -r  build_bdir/*  *~

.PHONY: full_clean
full_clean:
	rm -r -f  build_dir  Verilog_RTL  *~
	rm -f  *$(TOPMODULE)*  *.vcd 
