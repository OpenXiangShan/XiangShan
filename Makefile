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
FPGATOP = top.TopMain
BUILD_DIR = $(abspath ./build)

RTL_DIR = $(BUILD_DIR)/rtl

TOP_V = $(RTL_DIR)/$(TOP).sv
SCALA_FILE = $(shell find ./src/main/scala -name '*.scala')
TEST_FILE = $(shell find ./src/test/scala -name '*.scala')

SIMTOP  = top.SimTop
IMAGE  ?= temp
CONFIG ?= DefaultConfig
NUM_CORES ?= 1
ABS_WORK_DIR := $(shell pwd)
# VCS sim options
RUN_BIN_DIR ?= $(ABS_WORK_DIR)/ready-to-run
RUN_BIN ?= coremark-2-iteration
CONSIDER_FSDB ?= 1

ifdef FLASH
	RUN_OPTS := +flash=$(RUN_BIN_DIR)/$(RUN_BIN).bin
else
	RUN_OPTS := +workload=$(RUN_BIN_DIR)/$(RUN_BIN).bin
endif
ifeq ($(CONSIDER_FSDB),1)
	RUN_OPTS += +dump-wave=fsdb
endif
#RUN_OPTS += +diff=$(ABS_WORK_DIR)/ready-to-run/riscv64-nemu-interpreter-so
RUN_OPTS += +no-diff
RUN_OPTS += -fgp=num_threads:4,num_fsdb_threads:4
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
	@mkdir -p $(@D)
	$(TIME_CMD) mill -i XiangShan.runMain $(FPGATOP)                    \
		--compiler sverilog --target-dir $(@D)                      \
		--config $(CONFIG) --full-stacktrace --output-file $(@F)    \
		--infer-rw --repl-seq-mem -c:$(FPGATOP):-o:$(@D)/$(@F).conf \
		--gen-mem-verilog full --num-cores $(NUM_CORES)             \
		$(RELEASE_ARGS)
	sed -i -e 's/_\(aw\|ar\|w\|r\|b\)_\(\|bits_\)/_\1/g' $@
	@git log -n 1 >> .__head__
	@git diff >> .__diff__
	@sed -i 's/^/\/\// ' .__head__
	@sed -i 's/^/\/\//' .__diff__
	@cat .__head__ .__diff__ $@ > .__out__
	@mv .__out__ $@
	@rm .__head__ .__diff__

verilog: $(TOP_V)

SIM_TOP   = SimTop
SIM_TOP_V = $(RTL_DIR)/$(SIM_TOP).sv
$(SIM_TOP_V): $(SCALA_FILE) $(TEST_FILE)
	@mkdir -p $(@D)
	@echo "\n[mill] Generating Verilog files..." > $(TIMELOG)
	@date -R | tee -a $(TIMELOG)
	$(TIME_CMD) mill -i XiangShan.test.runMain $(SIMTOP)                \
		--compiler sverilog --target-dir $(@D)                      \
		--config $(CONFIG) --full-stacktrace --output-file $(@F)    \
		--infer-rw --repl-seq-mem -c:$(SIMTOP):-o:$(@D)/$(@F).conf  \
		--gen-mem-verilog full --num-cores $(NUM_CORES)             \
		$(SIM_ARGS)
	@git log -n 1 >> .__head__
	@git diff >> .__diff__
	@sed -i 's/^/\/\// ' .__head__
	@sed -i 's/^/\/\//' .__diff__
	@cat .__head__ .__diff__ $@ > .__out__
	@mv .__out__ $@
	@rm .__head__ .__diff__
	@sed -i -e 's/$$fatal/xs_assert_v2(`__FILE__, `__LINE__)/g' $(SIM_TOP_V)

FILELIST := $(ABS_WORK_DIR)/build/cpu_flist.f
sim-verilog: $(SIM_TOP_V)
	find $(ABS_WORK_DIR)/build -name "*.sv" > $(FILELIST)

clean:
	$(MAKE) -C ./difftest clean
	rm -rf ./build

init:
	git submodule update --init
	cd rocket-chip && git submodule update --init cde hardfloat
	cd huancun && git submodule update --init Utility

bsp:
	mill -i mill.bsp.BSP/install

# verilator simulation
emu: sim-verilog
	$(MAKE) -C ./difftest emu WITH_CONSTANTIN=0

# vcs simulation
simv:
	$(MAKE) -C ./difftest simv WITH_CONSTANTIN=0

.PHONY: verilog sim-verilog emu clean help init bump bsp

