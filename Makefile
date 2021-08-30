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
BUILD_DIR = ./build
TOP_V = $(BUILD_DIR)/$(TOP).v
SCALA_FILE = $(shell find ./src/main/scala -name '*.scala')
TEST_FILE = $(shell find ./src/test/scala -name '*.scala')
MEM_GEN = ./scripts/vlsi_mem_gen

SIMTOP  = top.SimTop
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

TIMELOG = $(BUILD_DIR)/time.log
TIME_CMD = time -a -o $(TIMELOG)

.DEFAULT_GOAL = verilog

help:
	mill XiangShan.test.runMain $(SIMTOP) --help

$(TOP_V): $(SCALA_FILE)
	mkdir -p $(@D)
	mill -i XiangShan.runMain $(FPGATOP) -td $(@D)                       \
		--config $(CONFIG) --full-stacktrace --output-file $(@F)     \
		--disable-all --remove-assert \
		$(SIM_ARGS) \
		--num-cores $(NUM_CORES)
	# $(MEM_GEN) $(@D)/$(@F).conf --tsmc28 --output_file $(@D)/tsmc28_sram.v > $(@D)/tsmc28_sram.v.conf
	# $(MEM_GEN) $(@D)/$(@F).conf --output_file $(@D)/sim_sram.v
	# sed -i -e 's/_\(aw\|ar\|w\|r\|b\)_\(\|bits_\)/_\1/g' $@
	# @git log -n 1 >> .__head__
	# @git diff >> .__diff__
	# @sed -i 's/^/\/\// ' .__head__
	# @sed -i 's/^/\/\//' .__diff__
	# @cat .__head__ .__diff__ $@ > .__out__
	# @mv .__out__ $@
	# @rm .__head__ .__diff__

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
	$(TIME_CMD) mill -i XiangShan.test.runMain $(SIMTOP) -td $(@D)       \
		--config $(CONFIG) --full-stacktrace --output-file $(@F)     \
		--num-cores $(NUM_CORES) $(SIM_ARGS)
		#--infer-rw --repl-seq-mem -c:$(SIMTOP):-o:$(@D)/$(@F).conf   \
	# $(MEM_GEN) $(@D)/$(@F).conf --output_file $(@D)/$(@F).sram.v
	# @git log -n 1 >> .__head__
	# @git diff >> .__diff__
	# @sed -i 's/^/\/\// ' .__head__
	# @sed -i 's/^/\/\//' .__diff__
	#@cat .__head__ .__diff__ $@ $(@D)/$(@F).sram.v > .__out__
	# @mv .__out__ $@
	# @rm .__head__ .__diff__
	sed -i -e 's/$$fatal/xs_assert(`__LINE__)/g' $(SIM_TOP_V)

sim-verilog: $(SIM_TOP_V)

clean:
	$(MAKE) -C ./difftest clean
	rm -rf ./build

init:
	git submodule update --init

bump:
	git submodule foreach "git fetch origin&&git checkout master&&git reset --hard origin/master"

bsp:
	mill -i mill.bsp.BSP/install

# verilator simulation
emu:
	$(MAKE) -C ./difftest emu SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES)

emu-run:
	$(MAKE) -C ./difftest emu-run SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES)

# vcs simulation
simv:
	$(MAKE) -C ./difftest simv SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES)

.PHONY: verilog sim-verilog emu clean help init bump bsp $(REF_SO)

