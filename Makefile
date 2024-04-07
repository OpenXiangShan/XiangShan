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

BUILD_DIR = ./build
RTL_DIR = $(BUILD_DIR)/rtl

TOP = XSTop
SIM_TOP = SimTop

FPGATOP = top.TopMain
SIMTOP  = top.SimTop

TOP_V = $(RTL_DIR)/$(TOP).v
SIM_TOP_V = $(RTL_DIR)/$(SIM_TOP).v

SCALA_FILE = $(shell find ./src/main/scala -name '*.scala')
TEST_FILE = $(shell find ./src/test/scala -name '*.scala')

MEM_GEN = ./scripts/vlsi_mem_gen
MEM_GEN_SEP = ./scripts/gen_sep_mem.sh
SPLIT_VERILOG = ./scripts/split_verilog.sh

IMAGE  ?= temp
CONFIG ?= DefaultConfig
NUM_CORES ?= 1
MFC ?= 0

# common chisel args
ifeq ($(MFC),1)
CHISEL_VERSION = chisel
FPGA_MEM_ARGS = --firtool-opt "--repl-seq-mem --repl-seq-mem-file=$(TOP).v.conf"
SIM_MEM_ARGS = --firtool-opt "--repl-seq-mem --repl-seq-mem-file=$(SIM_TOP).v.conf"
MFC_ARGS = --dump-fir \
           --firtool-opt "-O=release --disable-annotation-unknown --lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"
RELEASE_ARGS += $(MFC_ARGS)
DEBUG_ARGS += $(MFC_ARGS)
PLDM_ARGS += $(MFC_ARGS)
else
CHISEL_VERSION = chisel3
FPGA_MEM_ARGS = --infer-rw --repl-seq-mem -c:$(FPGATOP):-o:$(@D)/$(@F).conf --gen-mem-verilog full
SIM_MEM_ARGS = --infer-rw --repl-seq-mem -c:$(SIMTOP):-o:$(@D)/$(@F).conf --gen-mem-verilog full
endif

ifneq ($(XSTOP_PREFIX),)
RELEASE_ARGS += --xstop-prefix $(XSTOP_PREFIX)
DEBUG_ARGS += --xstop-prefix $(XSTOP_PREFIX)
PLDM_ARGS += --xstop-prefix $(XSTOP_PREFIX)
endif

# co-simulation with DRAMsim3
ifeq ($(WITH_DRAMSIM3),1)
ifndef DRAMSIM3_HOME
$(error DRAMSIM3_HOME is not set)
endif
override SIM_ARGS += --with-dramsim3
endif

# run emu with chisel-db
ifeq ($(WITH_CHISELDB),1)
override SIM_ARGS += --with-chiseldb
endif

# run emu with chisel-db
ifeq ($(WITH_ROLLINGDB),1)
override SIM_ARGS += --with-rollingdb
endif

# dynamic switch CONSTANTIN
ifeq ($(WITH_CONSTANTIN),0)
$(info disable WITH_CONSTANTIN)
else
override SIM_ARGS += --with-constantin
endif

# emu for the release version
RELEASE_ARGS += --fpga-platform --disable-always-basic-diff --disable-all --remove-assert
DEBUG_ARGS   += --enable-difftest
PLDM_ARGS    += --fpga-platform --enable-difftest
ifeq ($(RELEASE),1)
override SIM_ARGS += $(RELEASE_ARGS)
else ifeq ($(PLDM),1)
override SIM_ARGS += $(PLDM_ARGS)
else
override SIM_ARGS += $(DEBUG_ARGS)
endif

TIMELOG = $(BUILD_DIR)/time.log
TIME_CMD = time -a -o $(TIMELOG)

SED_CMD = sed -i -e 's/_\(aw\|ar\|w\|r\|b\)_\(\|bits_\)/_\1/g'

ifeq ($(PLDM),1)
SED_IFNDEF = `ifndef SYNTHESIS	// src/main/scala/device/RocketDebugWrapper.scala
SED_ENDIF  = `endif // not def SYNTHESIS
endif

.DEFAULT_GOAL = verilog

help:
	mill -i xiangshan[$(CHISEL_VERSION)].runMain $(FPGATOP) --help

$(TOP_V): $(SCALA_FILE)
	mkdir -p $(@D)
	$(TIME_CMD) mill -i xiangshan[$(CHISEL_VERSION)].runMain $(FPGATOP)   \
		-td $(@D) --config $(CONFIG) $(FPGA_MEM_ARGS)                    \
		--num-cores $(NUM_CORES) $(RELEASE_ARGS)
ifeq ($(MFC),1)
	$(SPLIT_VERILOG) $(RTL_DIR) $(TOP).v
	$(MEM_GEN_SEP) "$(MEM_GEN)" "$(TOP_V).conf" "$(RTL_DIR)"
endif
	$(SED_CMD) $@
	@git log -n 1 >> .__head__
	@git diff >> .__diff__
	@sed -i 's/^/\/\// ' .__head__
	@sed -i 's/^/\/\//' .__diff__
	@cat .__head__ .__diff__ $@ > .__out__
	@mv .__out__ $@
	@rm .__head__ .__diff__

verilog: $(TOP_V)

$(SIM_TOP_V): $(SCALA_FILE) $(TEST_FILE)
	mkdir -p $(@D)
	@echo "\n[mill] Generating Verilog files..." > $(TIMELOG)
	@date -R | tee -a $(TIMELOG)
	$(TIME_CMD) mill -i xiangshan[$(CHISEL_VERSION)].test.runMain $(SIMTOP)    \
		-td $(@D) --config $(CONFIG) $(SIM_MEM_ARGS)                          \
		--num-cores $(NUM_CORES) $(SIM_ARGS) --full-stacktrace
ifeq ($(MFC),1)
	$(SPLIT_VERILOG) $(RTL_DIR) $(SIM_TOP).v
	$(MEM_GEN_SEP) "$(MEM_GEN)" "$(SIM_TOP_V).conf" "$(RTL_DIR)"
endif
	$(SED_CMD) $@
	@git log -n 1 >> .__head__
	@git diff >> .__diff__
	@sed -i 's/^/\/\// ' .__head__
	@sed -i 's/^/\/\//' .__diff__
	@cat .__head__ .__diff__ $@ > .__out__
	@mv .__out__ $@
	@rm .__head__ .__diff__
ifeq ($(PLDM),1)
	sed -i -e 's/$$fatal/$$finish/g' $(SIM_TOP_V)
	sed -i -e '/sed/! { \|$(SED_IFNDEF)|, \|$(SED_ENDIF)| { \|$(SED_IFNDEF)|d; \|$(SED_ENDIF)|d; } }' $(SIM_TOP_V)
else
	sed -i -e 's/$$fatal/xs_assert(`__LINE__)/g' $(SIM_TOP_V)
endif
ifeq ($(MFC),1)
	sed -i -e "s/\$$error(/\$$fwrite(32\'h80000002, /g" $(SIM_TOP_V)
endif

sim-verilog: $(SIM_TOP_V)

clean:
	$(MAKE) -C ./difftest clean
	rm -rf $(BUILD_DIR)

init:
	git submodule update --init
	cd rocket-chip && git submodule update --init cde hardfloat

bump:
	git submodule foreach "git fetch origin&&git checkout master&&git reset --hard origin/master"

bsp:
	mill -i mill.bsp.BSP/install

idea:
	mill -i mill.scalalib.GenIdea/idea

# verilator simulation
emu: sim-verilog
	$(MAKE) -C ./difftest emu SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES)

emu-run: emu
	$(MAKE) -C ./difftest emu-run SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES)

# vcs simulation
simv: sim-verilog
	$(MAKE) -C ./difftest simv SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES)

# palladium simulation
pldm-build: sim-verilog
	$(MAKE) -C ./difftest pldm-build SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES)

pldm-run:
	$(MAKE) -C ./difftest pldm-run SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES)

pldm-debug:
	$(MAKE) -C ./difftest pldm-debug SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES)

include Makefile.test

.PHONY: verilog sim-verilog emu clean help init bump bsp $(REF_SO)
