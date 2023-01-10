#***************************************************************************************
# Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
# Copyright (c) 2020-2021 Peng Cheng Laboratory
#
# XiangShan is licensed under Mulan PSL v2.
# You can use this software according to the terms and conditions of the Mulan PSL v2.
# You may obtain a copy of Mulan PSL v2 at:
#          http://license.coscl.org.cn/MulanPSL2
#
# THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
# EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
# MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
#
# See the Mulan PSL v2 for more details.
#***************************************************************************************

TOP = XSTop
SIM_TOP   = SimTop
FPGATOP = top.TopMain
BUILD_DIR = ./build
TOP_V = $(BUILD_DIR)/$(TOP).v
SCALA_FILE = $(shell find ./src/main/scala -name '*.scala')
TEST_FILE = $(shell find ./src/test/scala -name '*.scala')
MEM_GEN = ./scripts/vlsi_mem_gen

SIMTOP  = top.SimTop
IMAGE  ?= temp
CONFIG ?= DefaultConfig
NUM_CORES ?= 1
ABS_WORK_DIR := $(shell pwd)
# VCS sim options
RUN_BIN_DIR ?= $(ABS_WORK_DIR)/ready-to-run
RUN_BIN ?= coremark-2-iteration
CONSIDER_FSDB ?= 1
MFC ?= 0

ifdef FLASH
	RUN_OPTS := +flash=$(RUN_BIN_DIR)/$(RUN_BIN).bin
else
	RUN_OPTS := +workload=$(RUN_BIN_DIR)/$(RUN_BIN).bin
endif
ifeq ($(CONSIDER_FSDB),1)
	RUN_OPTS += +dump-wave=fsdb
endif
RUN_OPTS += +diff=$(ABS_WORK_DIR)/ready-to-run/riscv64-nemu-interpreter-so
# RUN_OPTS += +no-diff
RUN_OPTS += -fgp=num_threads:4,num_fsdb_threads:4
# co-simulation with DRAMsim3
ifeq ($(WITH_DRAMSIM3),1)
ifndef DRAMSIM3_HOME
$(error DRAMSIM3_HOME is not set)
endif
override SIM_ARGS += --with-dramsim3
endif

# top-down
ifeq ($(ENABLE_TOPDOWN),1)
override SIM_ARGS += --enable-topdown
endif

# emu for the release version
RELEASE_ARGS = --disable-all --remove-assert --fpga-platform
DEBUG_ARGS   = --enable-difftest

ifeq ($(MFC),1)
RELEASE_ARGS += -X none -E chirrtl --output-file $(TOP).chirrtl.fir
DEBUG_ARGS += -X none -E chirrtl --output-file $(SIM_TOP).chirrtl.fir
else
RELEASE_ARGS += --emission-options disableRegisterRandomization -E verilog --output-file $(TOP).v
DEBUG_ARGS += --emission-options disableRegisterRandomization -E verilog --output-file $(SIM_TOP).v
endif

ifeq ($(RELEASE),1)
override SIM_ARGS += $(RELEASE_ARGS)
else
override SIM_ARGS += $(DEBUG_ARGS)
endif

.DEFAULT_GOAL = verilog

help:
	mill -i XiangShan.test.runMain $(SIMTOP) --help

$(TOP_V): $(SCALA_FILE)
	mkdir -p $(@D)
	time -o $(@D)/time.log mill -i XiangShan.runMain $(FPGATOP) -td $(@D) \
		--config $(CONFIG) --full-stacktrace --num-cores $(NUM_CORES) \
		$(RELEASE_ARGS)
ifeq ($(MFC),1)
	time -a -o $(@D)/time.log firtool --disable-all-randomization --disable-annotation-unknown \
	--annotation-file=$(BUILD_DIR)/$(TOP).anno.json --format=fir \
	--lowering-options=noAlwaysComb,disallowExpressionInliningInPorts,explicitBitcast \
	--verilog --dedup -o $(TOP_V) $(BUILD_DIR)/$(TOP).chirrtl.fir
endif
	sed -i -e 's/_\(aw\|ar\|w\|r\|b\)_\(\|bits_\)/_\1/g' $@
	@git log -n 1 >> .__head__
	@git diff >> .__diff__
	@sed -i 's/^/\/\// ' .__head__
	@sed -i 's/^/\/\//' .__diff__
	@cat .__head__ .__diff__ $@ > .__out__
	@mv .__out__ $@
	@rm .__head__ .__diff__

verilog: $(TOP_V)

SIM_TOP_V = $(BUILD_DIR)/$(SIM_TOP).v
$(SIM_TOP_V): $(SCALA_FILE) $(TEST_FILE)
	mkdir -p $(@D)
	@echo "\n[mill] Generating Verilog files..." > $(@D)/time.log
	@date -R | tee -a $(@D)/time.log
	time -o $(@D)/time.log mill -i XiangShan.test.runMain $(SIMTOP) -td $(@D) \
		--config $(CONFIG) --full-stacktrace --num-cores $(NUM_CORES) \
		$(SIM_ARGS)
ifeq ($(MFC),1)
	time -a -o $(@D)/time.log firtool --disable-all-randomization --disable-annotation-unknown \
	--annotation-file=$(BUILD_DIR)/$(SIM_TOP).anno.json --format=fir \
	--lowering-options=noAlwaysComb,disallowExpressionInliningInPorts,explicitBitcast \
	--verilog --dedup -o $(SIM_TOP_V) $(BUILD_DIR)/$(SIM_TOP).chirrtl.fir

	sed '/\/\/ ----- 8< ----- .*----- 8< -----/,$d' $(SIM_TOP_V) > res.v
	rm $(SIM_TOP_V)
	mv res.v $(SIM_TOP_V)
endif
	@git log -n 1 >> .__head__
	@git diff >> .__diff__
	@sed -i 's/^/\/\// ' .__head__
	@sed -i 's/^/\/\//' .__diff__
	@cat .__head__ .__diff__ $@ > .__out__
	@mv .__out__ $@
	@rm .__head__ .__diff__
	sed -i -e 's/$$fatal/xs_assert(`__LINE__)/g' $(SIM_TOP_V)

FILELIST := $(ABS_WORK_DIR)/build/cpu_flist.f
sim-verilog: $(SIM_TOP_V)
	find $(ABS_WORK_DIR)/build -name "*.v" > $(FILELIST)

clean:
	$(MAKE) -C ./difftest clean
	rm -rf ./build

init:
	git submodule update --init
	cd rocket-chip && git submodule update --init api-config-chipsalliance hardfloat

bump:
	git submodule foreach "git fetch origin&&git checkout master&&git reset --hard origin/master"

bsp:
	mill -i mill.bsp.BSP/install

idea:
	mill -i mill.scalalib.GenIdea/idea

# verilator simulation
emu:
	$(MAKE) -C ./difftest emu SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES)

emu-run:
	$(MAKE) -C ./difftest emu-run SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES)

# vcs simulation
simv:
	$(MAKE) -C ./difftest simv_rtl SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES)

simv_rtl:
	$(MAKE) -C ./difftest simv_rtl SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES) CONSIDER_FSDB=$(CONSIDER_FSDB)

simv_rtl-run:
	$(shell if [ ! -e $(ABS_WORK_DIR)/sim/rtl/$(RUN_BIN) ];then mkdir -p $(ABS_WORK_DIR)/sim/rtl/$(RUN_BIN); fi)
	touch sim/rtl/$(RUN_BIN)/sim.log
	$(shell if [ -e $(ABS_WORK_DIR)/sim/rtl/$(RUN_BIN)/simv ];then rm -f $(ABS_WORK_DIR)/sim/rtl/$(RUN_BIN)/simv; fi)
	$(shell if [ -e $(ABS_WORK_DIR)/sim/rtl/$(RUN_BIN)/simv.daidir ];then rm -rf $(ABS_WORK_DIR)/sim/rtl/$(RUN_BIN)/simv.daidir; fi)
	ln -s $(ABS_WORK_DIR)/sim/rtl/comp/simv $(ABS_WORK_DIR)/sim/rtl/$(RUN_BIN)/simv
	ln -s $(ABS_WORK_DIR)/sim/rtl/comp/simv.daidir $(ABS_WORK_DIR)/sim/rtl/$(RUN_BIN)/simv.daidir
	cd sim/rtl/$(RUN_BIN) && (./simv $(RUN_OPTS) | tee sim.log)

verdi_rtl:
	cd sim/rtl/$(RUN_BIN) && verdi -sv -2001 +verilog2001ext+v +systemverilogext+v -ssf tb_top.vf -dbdir simv.daidir -f sim_flist.f

.PHONY: verilog sim-verilog emu clean help init bump bsp $(REF_SO)

