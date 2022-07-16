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
ifeq ($(NANHU),1)
FPGATOP = top.FPGATop
else
FPGATOP = top.TopMain
endif
BUILD_DIR = ./build
TOP_V = $(BUILD_DIR)/$(TOP).v
SCALA_FILE = $(shell find ./src/main/scala -name '*.scala')
TEST_FILE = $(shell find ./src/test/scala -name '*.scala')
MEM_GEN = ./scripts/vlsi_mem_gen

ifeq ($(NANHU),1)
SIMTOP  = top.SimFPGA
else
SIMTOP  = top.SimTop
endif
IMAGE  ?= temp
CONFIG ?= DefaultConfig
NUM_CORES ?= 1

# co-simulation with DRAMsim3
ifeq ($(WITH_DRAMSIM3),1)
ifndef DRAMSIM3_HOME
$(error DRAMSIM3_HOME is not set)
endif
override SIM_ARGS += --with-dramsim3
endif

# emu for the release version
RELEASE_ARGS = --disable-all --remove-assert --fpga-platform
DEBUG_ARGS   = --enable-difftest
ifeq ($(RELEASE),1)
override SIM_ARGS += $(RELEASE_ARGS)
else
override SIM_ARGS += $(DEBUG_ARGS)
endif

TIMELOG = $(BUILD_DIR)/time.log
TIME_CMD = time -a -o $(TIMELOG)

.DEFAULT_GOAL = verilog

help:
	mill -i XiangShan.test.runMain $(SIMTOP) --help

$(TOP_V): $(SCALA_FILE)
	mkdir -p $(@D)
	mill -i XiangShan.runMain $(FPGATOP) -td $(@D)                      \
		--config $(CONFIG) --full-stacktrace --output-file $(@F)    \
		--num-cores $(NUM_CORES) $(RELEASE_ARGS)
	sed -i -e 's/_\(aw\|ar\|w\|r\|b\)_\(\|bits_\)/_\1/g' $@
	@git log -n 1 >> .__head__
	@git diff >> .__diff__
	@sed -i 's/^/\/\// ' .__head__
	@sed -i 's/^/\/\//' .__diff__
	@cat .__head__ .__diff__ $@ > .__out__
	@mv .__out__ $@
	@rm .__head__ .__diff__
	# fix sram model when RANDOMIZE_REG_INIT is defined
	sed -i -e '/.*data_hold_data.*=.*_RAND.*/d' $(TOP_V) 
ifeq ($(NANHU),1)
	sed -i -e 's/ XSTop / SLTop /g' $(TOP_V)
	sed -i -e 's/ XSTop(/ SLTop(/g' $(TOP_V)
	sed -i -e 's/ FPGATop(/ XSTop(/g' $(TOP_V)
endif

verilog: $(TOP_V)

SIM_TOP   = SimTop
SIM_TOP_V = $(BUILD_DIR)/$(SIM_TOP).v
$(SIM_TOP_V): $(SCALA_FILE) $(TEST_FILE)
	mkdir -p $(@D)
	@echo "\n[mill] Generating Verilog files..." > $(TIMELOG)
	@date -R | tee -a $(TIMELOG)
	$(TIME_CMD) mill -i XiangShan.test.runMain $(SIMTOP) -td $(@D)      \
		--config $(CONFIG) --full-stacktrace --output-file $(@F)    \
		--num-cores $(NUM_CORES) $(SIM_ARGS)
	sed -i -e 's/_\(aw\|ar\|w\|r\|b\)_\(\|bits_\)/_\1/g' $@
	@git log -n 1 >> .__head__
	@git diff >> .__diff__
	@sed -i 's/^/\/\// ' .__head__
	@sed -i 's/^/\/\//' .__diff__
	@cat .__head__ .__diff__ $@ > .__out__
	@mv .__out__ $@
	@rm .__head__ .__diff__
	sed -i -e 's/$$fatal/xs_assert(`__LINE__)/g' $(SIM_TOP_V)
	# fix sram model when RANDOMIZE_REG_INIT is defined
	sed -i -e '/.*data_hold_data.*=.*_RAND.*/d' $(SIM_TOP_V) 

sim-verilog: $(SIM_TOP_V)

sim-verilog-release:
	# if you have generated $(SIM_TOP_V) without setting RELEASE = 1, make clean first
	# force set RELEASE = 1 to generate release rtl
	$(MAKE) $(SIM_TOP_V) RELEASE=1
	# update SimTop.v, use "bosc_" module name prefix
	sed -i -e 's/ XSTop / bosc_XSTop /g' $(SIM_TOP_V)
	sed -i -e 's/ XSTop(/ bosc_XSTop(/g' $(SIM_TOP_V)
	# split rtl modules and sim top, copy extra files
	python3 scripts/parser.py SimTop --config $(CONFIG) \
		--ignore bosc_XSTop --include difftest          \
        --no-sram-conf --no-sram-xlsx --no-extra-files

clean:
	$(MAKE) -C ./difftest clean
	rm -rf ./build

clean-release:
	rm -rf ./*-Release-*

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
	$(MAKE) -C ./difftest simv SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES)

.PHONY: verilog sim-verilog emu clean help init bump bsp $(REF_SO)
