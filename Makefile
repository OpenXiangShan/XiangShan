#***************************************************************************************
# Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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
FPGATOP = top.TopMain
BUILD_DIR = ./build
TOP_V = $(BUILD_DIR)/$(TOP).v
SCALA_FILE = $(shell find ./src/main/scala -name '*.scala')
TEST_FILE = $(shell find ./src/test/scala -name '*.scala')
MEM_GEN = ./scripts/vlsi_mem_gen

SIMTOP = top.SimTop
IMAGE ?= temp
CONFIG ?= DefaultConfig

# co-simulation with DRAMsim3
ifeq ($(WITH_DRAMSIM3),1)
ifndef DRAMSIM3_HOME
$(error DRAMSIM3_HOME is not set)
endif
override SIM_ARGS += --with-dramsim3
endif

TIMELOG = $(BUILD_DIR)/time.log
TIME_CMD = time -a -o $(TIMELOG)

# remote machine with more cores to speedup c++ build
REMOTE ?= localhost

.DEFAULT_GOAL = verilog

help:
	mill XiangShan.test.runMain $(SIMTOP) --help

$(TOP_V): $(SCALA_FILE)
	mkdir -p $(@D)
	mill XiangShan.runMain $(FPGATOP) -td $(@D) --config $(CONFIG) --full-stacktrace --output-file $(@F) --disable-all --remove-assert --infer-rw --repl-seq-mem -c:$(FPGATOP):-o:$(@D)/$(@F).conf $(SIM_ARGS)
	$(MEM_GEN) $(@D)/$(@F).conf --tsmc28 --output_file $(@D)/tsmc28_sram.v > $(@D)/tsmc28_sram.v.conf
	$(MEM_GEN) $(@D)/$(@F).conf --output_file $(@D)/sim_sram.v
	# sed -i -e 's/_\(aw\|ar\|w\|r\|b\)_\(\|bits_\)/_\1/g' $@
	@git log -n 1 >> .__head__
	@git diff >> .__diff__
	@sed -i 's/^/\/\// ' .__head__
	@sed -i 's/^/\/\//' .__diff__
	@cat .__head__ .__diff__ $@ > .__out__
	@mv .__out__ $@
	@rm .__head__ .__diff__

deploy: build/top.zip


build/top.zip: $(TOP_V)
	@zip -r $@ $< $<.conf build/*.anno.json

.PHONY: deploy build/top.zip

verilog: $(TOP_V)

SIM_TOP   = SimTop
SIM_TOP_V = $(BUILD_DIR)/$(SIM_TOP).v
$(SIM_TOP_V): $(SCALA_FILE) $(TEST_FILE)
	mkdir -p $(@D)
	@echo "\n[mill] Generating Verilog files..." > $(TIMELOG)
	@date -R | tee -a $(TIMELOG)
	$(TIME_CMD) mill XiangShan.test.runMain $(SIMTOP) -td $(@D) --config $(CONFIG) --full-stacktrace --output-file $(@F) --infer-rw --repl-seq-mem -c:$(SIMTOP):-o:$(@D)/$(@F).conf $(SIM_ARGS)
	$(MEM_GEN) $(@D)/$(@F).conf --output_file $(@D)/$(@F).sram.v
	@git log -n 1 >> .__head__
	@git diff >> .__diff__
	@sed -i 's/^/\/\// ' .__head__
	@sed -i 's/^/\/\//' .__diff__
	@cat .__head__ .__diff__ $@ $(@D)/$(@F).sram.v > .__out__
	@mv .__out__ $@
	@rm .__head__ .__diff__
	sed -i -e 's/$$fatal/xs_assert(`__LINE__)/g' $(SIM_TOP_V)

sim-verilog: $(SIM_TOP_V)

SIM_CSRC_DIR = $(abspath ./src/test/csrc/common)
SIM_CXXFILES = $(shell find $(SIM_CSRC_DIR) -name "*.cpp")

DIFFTEST_CSRC_DIR = $(abspath ./src/test/csrc/difftest)
DIFFTEST_CXXFILES = $(shell find $(DIFFTEST_CSRC_DIR) -name "*.cpp")

SIM_VSRC = $(shell find ./src/test/vsrc/common -name "*.v" -or -name "*.sv")

include verilator.mk
include vcs.mk

ifndef NEMU_HOME
$(error NEMU_HOME is not set)
endif
REF_SO := $(NEMU_HOME)/build/riscv64-nemu-interpreter-so
$(REF_SO):
	$(MAKE) -C $(NEMU_HOME) ISA=riscv64 SHARE=1

SEED ?= $(shell shuf -i 1-10000 -n 1)

VME_SOURCE ?= $(shell pwd)/build/$(TOP).v
VME_MODULES ?=

#-----------------------timing scripts-------------------------
# run "make vme/tap help=1" to get help info

# extract verilog module from TopMain.v
# usage: make vme VME_MODULES=Roq
TIMING_SCRIPT_PATH = ./timingScripts
vme: $(TOP_V)
	make -C $(TIMING_SCRIPT_PATH) vme

# get and sort timing analysis with total delay(start+end) and max delay(start or end)
# and print it out
tap:
	make -C $(TIMING_SCRIPT_PATH) tap

# usage: make phy_evaluate VME_MODULE=Roq REMOTE=100
phy_evaluate: vme
	scp -r ./build/extracted/* $(REMOTE):~/phy_evaluation/remote_run/rtl
	ssh -tt $(REMOTE) 'cd ~/phy_evaluation/remote_run && $(MAKE) evaluate DESIGN_NAME=$(VME_MODULE)'
	scp -r  $(REMOTE):~/phy_evaluation/remote_run/rpts ./build

# usage: make phy_evaluate_atc VME_MODULE=Roq REMOTE=100
phy_evaluate_atc: vme
	scp -r ./build/extracted/* $(REMOTE):~/phy_evaluation/remote_run/rtl
	ssh -tt $(REMOTE) 'cd ~/phy_evaluation/remote_run && $(MAKE) evaluate_atc DESIGN_NAME=$(VME_MODULE)'
	scp -r  $(REMOTE):~/phy_evaluation/remote_run/rpts ./build

cache:
	$(MAKE) emu IMAGE=Makefile

release-lock:
	ssh -tt $(REMOTE) 'rm -f $(LOCK)'

clean: vcs-clean
	rm -rf ./build

init:
	git submodule update --init

bump:
	git submodule foreach "git fetch origin&&git checkout master&&git reset --hard origin/master"

bsp:
	mill -i mill.bsp.BSP/install

.PHONY: verilog sim-verilog emu clean help init bump bsp $(REF_SO)

