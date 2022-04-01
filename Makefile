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
MFC ?= 0

FPGA_MEM_ARGS = --infer-rw --repl-seq-mem -c:$(FPGATOP):-o:$(@D)/$(@F).conf --gen-mem-verilog full
SIM_MEM_ARGS = --infer-rw --repl-seq-mem -c:$(SIMTOP):-o:$(@D)/$(@F).conf --gen-mem-verilog full

# select firrtl complier
ifeq ($(MFC),1)
override FC_ARGS = --mfc
override FPGA_MEM_ARGS =
override SIM_MEM_ARGS =
endif


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

SED_CMD = sed -i -e 's/_\(aw\|ar\|w\|r\|b\)_\(\|bits_\)/_\1/g'

# add comments to 'firrtl_black_box_resource_files'
AWK_CMD = gawk -i inplace 'BEGIN{f=0} /FILE "firrtl_black_box_resource_files.f"/{f=1} !f{print $$0} f{print "//", $$0}'


.DEFAULT_GOAL = verilog

help:
	mill -i XiangShan.runMain $(FPGATOP) --help

$(TOP_V): $(SCALA_FILE)
	mkdir -p $(@D)
	$(TIME_CMD) mill -i XiangShan.runMain $(FPGATOP) -td $(@D)  \
		--config $(CONFIG)                                        \
		$(FPGA_MEM_ARGS)                                          \
		--num-cores $(NUM_CORES)                                  \
		$(RELEASE_ARGS) $(FC_ARGS)
	$(SED_CMD) $@
ifeq ($(MFC),1)
	$(AWK_CMD) $@
endif
	@git log -n 1 >> .__head__
	@git diff >> .__diff__
	@sed -i 's/^/\/\// ' .__head__
	@sed -i 's/^/\/\//' .__diff__
	@cat .__head__ .__diff__ $@ > .__out__
	@mv .__out__ $@
	@rm .__head__ .__diff__

verilog: $(TOP_V)

SIM_TOP   = SimTop
SIM_TOP_V = $(BUILD_DIR)/$(SIM_TOP).v
$(SIM_TOP_V): $(SCALA_FILE) $(TEST_FILE)
	mkdir -p $(@D)
	@echo "\n[mill] Generating Verilog files..." > $(TIMELOG)
	@date -R | tee -a $(TIMELOG)
	$(TIME_CMD) mill -i XiangShan.test.runMain $(SIMTOP) -td $(@D)  \
		--config $(CONFIG)                                            \
		$(SIM_MEM_ARGS)                                               \
		--num-cores $(NUM_CORES)                                      \
		$(SIM_ARGS) $(FC_ARGS)
	$(SED_CMD) $@
ifeq ($(MFC),1)
	$(AWK_CMD) $@
endif
	@git log -n 1 >> .__head__
	@git diff >> .__diff__
	@sed -i 's/^/\/\// ' .__head__
	@sed -i 's/^/\/\//' .__diff__
	@cat .__head__ .__diff__ $@ > .__out__
	@mv .__out__ $@
	@rm .__head__ .__diff__
	sed -i -e 's/$$fatal/xs_assert(`__LINE__)/g' $(SIM_TOP_V)

sim-verilog: $(SIM_TOP_V)

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
	$(MAKE) -C ./difftest simv SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES)

.PHONY: verilog sim-verilog emu clean help init bump bsp $(REF_SO)
