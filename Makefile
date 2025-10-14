#***************************************************************************************
# Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
# Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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

# import docker support
include scripts/Makefile.docker

# import pdb support
include scripts/Makefile.pdb

# if XSTopPrefix is specified in yaml, use it.
ifneq ($(YAML_CONFIG),)
HAS_PREFIX_FROM_YAML = $(shell grep 'XSTopPrefix *:' $(YAML_CONFIG))
ifneq ($(HAS_PREFIX_FROM_YAML),)
XSTOP_PREFIX_YAML = $(shell grep 'XSTopPrefix *:' $(YAML_CONFIG) | sed 's/XSTopPrefix *: *//' | tr -d \"\')
override XSTOP_PREFIX = $(XSTOP_PREFIX_YAML)
endif
endif

TOP = $(XSTOP_PREFIX)XSTop
SIM_TOP = SimTop

FPGATOP = top.TopMain
SIMTOP  = top.SimTop

RTL_SUFFIX ?= sv
TOP_V = $(RTL_DIR)/$(TOP).$(RTL_SUFFIX)
SIM_TOP_V = $(RTL_DIR)/$(SIM_TOP).$(RTL_SUFFIX)
JAR = $(BUILD_DIR)/xsgen.jar

SCALA_FILE = $(shell find ./src/main/scala -name '*.scala')
TEST_FILE = $(shell find ./src/test/scala -name '*.scala')

MEM_GEN = ./scripts/vlsi_mem_gen
MEM_GEN_SEP = ./scripts/gen_sep_mem.sh

CONFIG ?= DefaultConfig
NUM_CORES ?= 1
ISSUE ?= E.b
CHISEL_TARGET ?= systemverilog

SUPPORT_CHI_ISSUE = B C E.b
ifeq ($(findstring $(ISSUE), $(SUPPORT_CHI_ISSUE)),)
$(error "Unsupported CHI issue: $(ISSUE)")
endif

ifneq ($(shell echo "$(MAKECMDGOALS)" | grep ' '),)
$(error At most one target can be specified)
endif

ifeq ($(MAKECMDGOALS),)
GOALS = verilog
else
GOALS = $(MAKECMDGOALS)
endif

# JVM memory configurations
JVM_XMX ?= 40G
JVM_XSS ?= 256m

# mill arguments for build.sc
MILL_BUILD_ARGS = -Djvm-xmx=$(JVM_XMX) -Djvm-xss=$(JVM_XSS)

# common chisel args
FPGA_MEM_ARGS = --firtool-opt "--repl-seq-mem --repl-seq-mem-file=$(TOP).$(RTL_SUFFIX).conf"
SIM_MEM_ARGS = --firtool-opt "--repl-seq-mem --repl-seq-mem-file=$(SIM_TOP).$(RTL_SUFFIX).conf"
MFC_ARGS = --target $(CHISEL_TARGET) \
           --firtool-opt "-O=release --disable-annotation-unknown --default-layer-specialization=enable --lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"

ifeq ($(CHISEL_TARGET),systemverilog)
MFC_ARGS += --split-verilog --dump-fir
endif

ifneq ($(FIRTOOL),)
MFC_ARGS += --firtool-binary-path $(abspath $(FIRTOOL))
endif

# prefix of XSTop or XSNoCTop
ifneq ($(XSTOP_PREFIX),)
COMMON_EXTRA_ARGS += --xstop-prefix $(XSTOP_PREFIX)
endif

# IMSIC bus type (AXI, TL or NONE)
ifneq ($(IMSIC_BUS_TYPE),)
COMMON_EXTRA_ARGS += --imsic-bus-type $(IMSIC_BUS_TYPE)
endif

# enable or disable dfx manually
ifeq ($(DFX),1)
COMMON_EXTRA_ARGS += --dfx true
else
ifeq ($(DFX),0)
COMMON_EXTRA_ARGS += --dfx false
endif
endif

# enable or disable sram ctl maunally
ifeq ($(SRAM_WITH_CTL),1)
COMMON_EXTRA_ARGS += --sram-with-ctl
endif

# enable non-secure access or not
# CHI requests are secure as default by now
ifeq ($(ENABLE_NS),1)
COMMON_EXTRA_ARGS += --enable-ns
endif

# CHI physical address width
ifneq ($(CHI_ADDR_WIDTH),)
COMMON_EXTRA_ARGS += --chi-addr-width $(CHI_ADDR_WIDTH)
endif

# L2 cache size in KB
ifneq ($(L2_CACHE_SIZE),)
COMMON_EXTRA_ARGS += --l2-cache-size $(L2_CACHE_SIZE)
endif

# L3 cache size in KB
ifneq ($(L3_CACHE_SIZE),)
COMMON_EXTRA_ARGS += --l3-cache-size $(L3_CACHE_SIZE)
endif

# hart id bits
ifneq ($(HART_ID_BITS),)
COMMON_EXTRA_ARGS += --hartidbits $(HART_ID_BITS)
endif

# disable xmr
ifeq ($(DISABLE_XMR),1)
COMMON_EXTRA_ARGS += --disable-xmr
endif

# configuration from yaml file
ifneq ($(YAML_CONFIG),)
COMMON_EXTRA_ARGS += --yaml-config $(YAML_CONFIG)
endif

# public args sumup
RELEASE_ARGS += $(MFC_ARGS) $(COMMON_EXTRA_ARGS)
DEBUG_ARGS += $(MFC_ARGS) $(COMMON_EXTRA_ARGS)
override PLDM_ARGS += $(MFC_ARGS) $(COMMON_EXTRA_ARGS)

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

# enable ResetGen
ifeq ($(WITH_RESETGEN),1)
override SIM_ARGS += --reset-gen
endif

# run with disable all perf
ifeq ($(DISABLE_PERF),1)
override SIM_ARGS += --disable-perf
endif

# run with disable all db
ifeq ($(DISABLE_ALWAYSDB),1)
override SIM_ARGS += --disable-alwaysdb
endif

# dynamic switch CONSTANTIN
ifeq ($(WITH_CONSTANTIN),1)
override SIM_ARGS += --with-constantin
endif

# run with sim frontend(ideal frontend)
ifeq ($(ENABLE_SIMFRONTEND),1)
override SIM_ARGS += --enable-simfrontend
endif


# emu for the release version
RELEASE_ARGS += --fpga-platform --disable-all --remove-assert --reset-gen --firtool-opt --ignore-read-enable-mem
DEBUG_ARGS   += --enable-difftest
override PLDM_ARGS += --enable-difftest
ifeq ($(RELEASE),1)
override SIM_ARGS += $(RELEASE_ARGS)
else ifeq ($(PLDM),1)
override SIM_ARGS += $(PLDM_ARGS)
else
override SIM_ARGS += $(DEBUG_ARGS)
endif

# Coverage support
ifneq ($(FIRRTL_COVER),)
comma := ,
splitcomma = $(foreach w,$(subst $(comma), ,$1),$(if $(strip $w),$w))
override SIM_ARGS += $(foreach c,$(call splitcomma,$(FIRRTL_COVER)),--extract-$(c)-cover)
endif

# use RELEASE_ARGS for TopMain by default
ifeq ($(PLDM), 1)
TOPMAIN_ARGS += $(PLDM_ARGS)
else
TOPMAIN_ARGS += $(RELEASE_ARGS)
endif

TIMELOG = $(BUILD_DIR)/time.log
TIME_CMD = time -avp -o $(TIMELOG)

ifeq ($(PLDM),1)
SED_IFNDEF = `ifndef SYNTHESIS	// src/main/scala/device/RocketDebugWrapper.scala
SED_ENDIF  = `endif // not def SYNTHESIS
endif

.DEFAULT_GOAL = verilog

help:
	mill -i xiangshan.runMain $(FPGATOP) --help

version:
	mill -i xiangshan.runMain $(FPGATOP) --version

jar:
	mill -i xiangshan.assembly

$(JAR): FORCE
	mill -i xiangshan.test.assembly
	@mkdir -p $(@D); \
	JAR_REF=$(shell mill -i show xiangshan.test.assembly 2> /dev/null); \
	[ ! -z $${JAR_REF} ] && echo $${JAR_REF} | sed 's/"//g' | awk -F: '{print $$4}' \
		| xargs -I{} cp {} $@
test-jar: $(call docker-deps,$(JAR))

comp:
	mill -i xiangshan.compile
	mill -i xiangshan.test.compile

$(TOP_V): $(SCALA_FILE)
	mkdir -p $(@D)
	$(TIME_CMD) mill -i $(MILL_BUILD_ARGS) xiangshan.runMain $(FPGATOP)   \
		--target-dir $(@D) --config $(CONFIG) --issue $(ISSUE) $(FPGA_MEM_ARGS)		\
		--num-cores $(NUM_CORES) $(TOPMAIN_ARGS)
ifeq ($(CHISEL_TARGET),systemverilog)
	$(MEM_GEN_SEP) "$(MEM_GEN)" "$@.conf" "$(@D)"
	@{ git log -n 1; git diff; } | sed 's/^/\/\// ' > $(dir $@).__diff__
	@cat $(dir $@).__diff__ $@ > $(dir $@).__out__ && mv $(dir $@).__out__ $@
endif

verilog: $(call docker-deps,$(TOP_V))

$(SIM_TOP_V): $(SCALA_FILE) $(TEST_FILE)
	mkdir -p $(@D)
	@echo -e "\n[mill] Generating Verilog files..." > $(TIMELOG)
	@date -R | tee -a $(TIMELOG)
	$(TIME_CMD) mill -i $(MILL_BUILD_ARGS) xiangshan.test.runMain $(SIMTOP)    \
		--target-dir $(@D) --config $(CONFIG) --issue $(ISSUE) $(SIM_MEM_ARGS)		\
		--num-cores $(NUM_CORES) $(SIM_ARGS) --full-stacktrace
ifeq ($(CHISEL_TARGET),systemverilog)
	$(MEM_GEN_SEP) "$(MEM_GEN)" "$@.conf" "$(@D)"
	@{ git log -n 1; git diff; } | sed 's/^/\/\// ' > $(dir $@).__diff__
	@cat $(dir $@).__diff__ $@ > $(dir $@).__out__ && mv $(dir $@).__out__ $@
ifeq ($(PLDM),1)
	sed -i -e 's/$$fatal/$$finish/g' $(RTL_DIR)/*.$(RTL_SUFFIX)
	sed -i -e '/sed/! { \|$(SED_IFNDEF)|, \|$(SED_ENDIF)| { \|$(SED_IFNDEF)|d; \|$(SED_ENDIF)|d; } }' $(RTL_DIR)/*.$(RTL_SUFFIX)
else
ifeq ($(ENABLE_XPROP),1)
	sed -i -e "s/\$$fatal/assert(1\'b0)/g" $(RTL_DIR)/*.$(RTL_SUFFIX)
else
	sed -i -e 's/$$fatal/xs_assert_v2(`__FILE__, `__LINE__)/g' $(RTL_DIR)/*.$(RTL_SUFFIX)
endif
endif
	sed -i -e "s/\$$error(/\$$fwrite(32\'h80000002, /g" $(RTL_DIR)/*.$(RTL_SUFFIX)
endif

sim-verilog: $(call docker-deps,$(SIM_TOP_V))

clean:
	$(MAKE) -C ./difftest clean
	rm -rf $(BUILD_DIR)

init:
	git submodule update --init
	cd rocket-chip && git submodule update --init cde hardfloat
	cd openLLC && git submodule update --init openNCB

bump:
	git submodule foreach "git fetch origin&&git checkout master&&git reset --hard origin/master"

deps:
	mill -i __.prepareOffline
	mill -i xiangshan.resolveFirtoolDeps

bsp:
	mill -i mill.bsp.BSP/install

idea:
	mill -i mill.idea.GenIdea/idea

check-format:
	mill xiangshan.checkFormat

reformat:
	mill xiangshan.reformat

# verilator simulation
emu-mk: sim-verilog
	$(MAKE) -C ./difftest emu-mk SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX)

emu: $(call docker-deps,emu-mk)
	$(MAKE) -C ./difftest emu SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX)

emu-run: emu
	$(MAKE) -C ./difftest emu-run SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX)

# vcs simulation
simv: sim-verilog
	$(MAKE) -C ./difftest simv SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX)

simv-run:
	$(MAKE) -C ./difftest simv-run SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX)

# galaxsim simulation
xsim: sim-verilog
	$(MAKE) -C ./difftest xsim SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX)

xsim-run:
	$(MAKE) -C ./difftest xsim-run SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX)

# palladium simulation
pldm-build: sim-verilog
	$(MAKE) -C ./difftest pldm-build SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX)

pldm-run:
	$(MAKE) -C ./difftest pldm-run SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX)

pldm-debug:
	$(MAKE) -C ./difftest pldm-debug SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX)

include Makefile.test

include src/main/scala/device/standalone/standalone_device.mk

.PHONY: FORCE verilog sim-verilog emu clean help init bump bsp $(REF_SO)
