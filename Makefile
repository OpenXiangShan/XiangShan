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
NOOP_HOME ?= `pwd`

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
SIMTOP  = top.XiangShanSim

RTL_SUFFIX ?= sv
TOP_V = $(RTL_DIR)/$(TOP).$(RTL_SUFFIX)
SIM_TOP_V = $(RTL_DIR)/$(SIM_TOP).$(RTL_SUFFIX)
JAR = $(BUILD_DIR)/xsgen.jar

SCALA_FILE = $(shell find ./src/main/scala -name '*.scala')
TEST_FILE = $(shell find ./src/test/scala -name '*.scala')

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
JAVA_BIN := $(shell command -v java 2>/dev/null)
JAVA_HOME_VALID := $(shell if [ -n "$(JAVA_HOME)" ] && [ -x "$(JAVA_HOME)/bin/java" ]; then echo yes; fi)
ifeq ($(JAVA_HOME_VALID),yes)
RESOLVED_JAVA_HOME := $(JAVA_HOME)
else
RESOLVED_JAVA_HOME := $(shell if [ -n "$(JAVA_BIN)" ]; then dirname "$$(dirname "$$(readlink -f "$(JAVA_BIN)")")"; fi)
endif
MILL_ENV = $(if $(strip $(RESOLVED_JAVA_HOME)),JAVA_HOME=$(RESOLVED_JAVA_HOME))
MILL = $(MILL_ENV) mill

# NOTE: ccache is intentionally disabled by default, as:
#   1. it does not help XiangShan's build performance in most cases, as slight change in chisel result in significant change in cpp code
#   2. it introduces too much IO overhead
# NOTE: use `make emu OBJCACHE=ccache` to enable it
OBJCACHE ?=

# common chisel args
MFC_ARGS = --target $(CHISEL_TARGET) \
           --firtool-opt "-O=release --disable-annotation-unknown --lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none"

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
override DEBUG_ARGS += $(MFC_ARGS) $(COMMON_EXTRA_ARGS)

# co-simulation with DRAMsim3
ifeq ($(WITH_DRAMSIM3),1)
ifndef DRAMSIM3_HOME
$(error DRAMSIM3_HOME is not set)
endif
override SIM_ARGS += --with-dramsim3
endif

# SimAXIMem size in GB (for sim-verilog only)
ifneq ($(SIM_MEM_SIZE),)
override SIM_ARGS += --sim-mem-size $(SIM_MEM_SIZE)
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

ifeq ($(GSIM), 1)
override SIM_ARGS += --difftest-config G
endif

# emu for the release version
RELEASE_ARGS += --fpga-platform --reset-gen --firtool-opt --ignore-read-enable-mem --firtool-opt "--default-layer-specialization=disable"
ifeq ($(FPGA), 1)
override DEBUG_ARGS	+= --fpga-platform --firtool-opt "--default-layer-specialization=disable"
else
override DEBUG_ARGS	+= --enable-difftest --firtool-opt "--default-layer-specialization=enable"
endif
ifeq ($(RELEASE),1)
override SIM_ARGS += $(RELEASE_ARGS)
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
ifeq ($(or $(PLDM),$(FPGA)), 1)
TOPMAIN_ARGS += $(DEBUG_ARGS)
else
TOPMAIN_ARGS += $(RELEASE_ARGS)
endif

ifeq ($(DUMP_CSR),1)
TOPMAIN_ARGS += --dump-csr
endif

TIMELOG = $(BUILD_DIR)/time.log
TIME_CMD = time -avp -o $(TIMELOG)

ifeq ($(PLDM),1)
SED_IFNDEF = `ifndef SYNTHESIS	// src/main/scala/device/RocketDebugWrapper.scala
SED_ENDIF  = `endif // not def SYNTHESIS
endif

.DEFAULT_GOAL = verilog

help:
	$(MILL) -i xiangshan.runMain $(FPGATOP) --help

version:
	$(MILL) -i xiangshan.runMain $(FPGATOP) --version

jar:
	$(MILL) -i xiangshan.assembly

$(JAR): FORCE
	$(MILL) -i xiangshan.test.assembly
	@mkdir -p $(@D); \
	JAR_REF=$(shell $(MILL) -i show xiangshan.test.assembly 2> /dev/null); \
	[ ! -z $${JAR_REF} ] && echo $${JAR_REF} | sed 's/"//g' | awk -F: '{print $$4}' \
		| xargs -I{} cp {} $@
test-jar: $(call docker-deps,$(JAR))

comp:
	$(MILL) -i xiangshan.compile
	$(MILL) -i xiangshan.test.compile

$(TOP_V): $(SCALA_FILE)
	mkdir -p $(@D) $(dir $(TIMELOG))
	$(MILL_ENV) $(TIME_CMD) mill -i $(MILL_BUILD_ARGS) xiangshan.runMain $(FPGATOP) \
		--target-dir $(@D) --config $(CONFIG) --issue $(ISSUE) \
		--num-cores $(NUM_CORES) $(TOPMAIN_ARGS)
ifeq ($(CHISEL_TARGET),systemverilog)
	@{ git log -n 1; git diff; } | sed 's/^/\/\// ' > $(dir $@).__diff__
	@cat $(dir $@).__diff__ $@ > $(dir $@).__out__ && mv $(dir $@).__out__ $@
endif

FRONTEND_BUILD_DIR = ./build-frontend
FRONTEND_RTL_DIR   = $(FRONTEND_BUILD_DIR)/rtl
FRONTENDTOP        = top.FrontendTopMain
FRONTEND_TOP_V     = $(FRONTEND_RTL_DIR)/FrontendTop.$(RTL_SUFFIX)
FRONTEND_BUILD_MANIFEST = $(FRONTEND_BUILD_DIR)/frontend_build_manifest.$(FRONTEND_SIM).json
FRONTEND_PYLIB_ROOT = $(FRONTEND_BUILD_DIR)/pylib-$(FRONTEND_SIM)
FRONTEND_PYLIB_DIR = $(FRONTEND_PYLIB_ROOT)/Frontend
FRONTEND_PYLIB     = $(FRONTEND_PYLIB_DIR)/libUTFrontend.so
FRONTEND_SIM ?= verilator
FRONTEND_WAVEFORM_FORMAT ?=
FRONTEND_WAVEFORM_FORMAT_DEFAULT := $(if $(filter vcs,$(FRONTEND_SIM)),fsdb,fst)
FRONTEND_WAVEFORM_FORMAT_FILE = $(FRONTEND_BUILD_DIR)/.waveform_format.$(FRONTEND_SIM)
FRONTEND_CONFIG_FILE = $(FRONTEND_BUILD_DIR)/.frontend_config.$(FRONTEND_SIM)
FRONTEND_FUNCOV_SV_DIR = src/test/python/Frontend/env/funcov/sv
FRONTEND_FUNCOV_FILELIST = $(FRONTEND_RTL_DIR)/filelist.funcov.f
FRONTEND_PICKER_FILELIST = $(FRONTEND_RTL_DIR)/filelist.f
FRONTEND_PYLIB_EXTRA_DEPS =
FRONTEND_CCACHE_DIR ?= $(abspath $(FRONTEND_BUILD_DIR)/.ccache)
FRONTEND_CCACHE_TMP ?= $(abspath $(FRONTEND_BUILD_DIR)/.ccache-tmp)
FRONTEND_BUILD_JOBS ?= 32
FRONTEND_LOCAL_CONFIG ?= .local/frontend.local.mk
FRONTEND_VCS_HOME ?= $(VCS_HOME)
FRONTEND_VERDI_HOME ?= $(VERDI_HOME)
FRONTEND_VCS_HOST ?=
FRONTEND_PICKER ?= picker
FRONTEND_TOOL_PATH ?=
FRONTEND_SWIG_LIB ?=
ifeq ($(FRONTEND_SIM),vcs)
-include $(FRONTEND_LOCAL_CONFIG)
endif
FRONTEND_SIM_ENV =
FRONTEND_BUILD_ENV = CCACHE_DIR=$(FRONTEND_CCACHE_DIR) CCACHE_TEMPDIR=$(FRONTEND_CCACHE_TMP) NPROC=$(FRONTEND_BUILD_JOBS) MAKEFLAGS=-e $(if $(FRONTEND_TOOL_PATH),PATH=$(FRONTEND_TOOL_PATH)$$PATH) $(if $(FRONTEND_SWIG_LIB),SWIG_LIB=$(FRONTEND_SWIG_LIB)) $(FRONTEND_SIM_ENV)

ifeq ($(FRONTEND_SIM),verilator)
FRONTEND_ACCESS_MODE ?= MEM_DIRECT
else ifeq ($(FRONTEND_SIM),vcs)
FRONTEND_ACCESS_MODE ?= dpi
FRONTEND_PICKER_SIM_ARGS =
FRONTEND_SIM_ENV = VCS_HOME=$(FRONTEND_VCS_HOME) VERDI_HOME=$(FRONTEND_VERDI_HOME)
FRONTEND_FUNCOV_SV_FILES = $(shell find $(FRONTEND_FUNCOV_SV_DIR) -type f -name '*.sv' -print 2>/dev/null | sort)
FRONTEND_PICKER_FILELIST = $(FRONTEND_FUNCOV_FILELIST)
FRONTEND_PYLIB_EXTRA_DEPS = $(FRONTEND_FUNCOV_FILELIST) $(FRONTEND_FUNCOV_SV_FILES)
else
$(error FRONTEND_SIM must be one of: verilator vcs)
endif

ifneq ($(FRONTEND_WAVEFORM_FORMAT),)
ifeq ($(filter $(FRONTEND_WAVEFORM_FORMAT),fst vcd fsdb),)
$(error FRONTEND_WAVEFORM_FORMAT must be one of: fst vcd fsdb)
endif
endif

ifeq ($(FRONTEND_SIM),vcs)
ifneq ($(FRONTEND_WAVEFORM_FORMAT),)
ifneq ($(FRONTEND_WAVEFORM_FORMAT),fsdb)
$(error FRONTEND_WAVEFORM_FORMAT must be fsdb when FRONTEND_SIM=vcs)
endif
endif
else ifeq ($(FRONTEND_SIM),verilator)
ifeq ($(FRONTEND_WAVEFORM_FORMAT),fsdb)
$(error FRONTEND_WAVEFORM_FORMAT must be fst or vcd when FRONTEND_SIM=verilator)
endif
endif

$(FRONTEND_WAVEFORM_FORMAT_FILE): FORCE
	@mkdir -p $(dir $@)
	@desired_format="$(FRONTEND_WAVEFORM_FORMAT)"; \
	if [ -z "$$desired_format" ]; then \
		if [ -f "$@" ]; then \
			desired_format="$$(cat "$@")"; \
		else \
			desired_format="$(FRONTEND_WAVEFORM_FORMAT_DEFAULT)"; \
		fi; \
	fi; \
	if [ ! -f "$@" ] || [ "$$(cat "$@")" != "$$desired_format" ]; then \
		printf '%s\n' "$$desired_format" > "$@"; \
	fi

$(FRONTEND_CONFIG_FILE): FORCE
	@mkdir -p $(dir $@)
	@desired_config="sim=$(FRONTEND_SIM) access=$(FRONTEND_ACCESS_MODE)"; \
	if [ "$(FRONTEND_SIM)" = "vcs" ]; then \
		desired_config="$$desired_config vcs_home=$(FRONTEND_VCS_HOME) verdi_home=$(FRONTEND_VERDI_HOME)"; \
	fi; \
	if [ ! -f "$@" ] || [ "$$(cat "$@")" != "$$desired_config" ]; then \
		printf '%s\n' "$$desired_config" > "$@"; \
	fi

$(FRONTEND_TOP_V): $(SCALA_FILE) | $(FRONTEND_WAVEFORM_FORMAT_FILE)
	mkdir -p $(@D) $(dir $(TIMELOG))
	$(MILL_ENV) $(TIME_CMD) mill -i $(MILL_BUILD_ARGS) xiangshan.runMain $(FRONTENDTOP) \
		--target-dir $(@D) --config $(CONFIG) --issue $(ISSUE) \
		--num-cores $(NUM_CORES) $(TOPMAIN_ARGS)
ifeq ($(CHISEL_TARGET),systemverilog)
	@{ git log -n 1; git diff; } | sed 's/^/\/\// ' > $(dir $@).__diff__
	@cat $(dir $@).__diff__ $@ > $(dir $@).__out__ && mv $(dir $@).__out__ $@
endif

$(FRONTEND_FUNCOV_FILELIST): FORCE $(FRONTEND_TOP_V)
	@tmp="$@.tmp"; \
	{ \
		cat "$(FRONTEND_RTL_DIR)/filelist.f"; \
		for frontend_funcov_sv in $(FRONTEND_FUNCOV_SV_FILES); do \
			printf '%s\n' "$(abspath .)/$$frontend_funcov_sv"; \
		done; \
	} > "$$tmp"; \
	if ! cmp -s "$$tmp" "$@"; then mv "$$tmp" "$@"; else rm -f "$$tmp"; fi

$(FRONTEND_PYLIB): $(FRONTEND_TOP_V) $(FRONTEND_WAVEFORM_FORMAT_FILE) $(FRONTEND_CONFIG_FILE) $(FRONTEND_PYLIB_EXTRA_DEPS)
	@if [ "$(FRONTEND_SIM)" = "vcs" ] && [ -n "$(FRONTEND_VCS_HOST)" ] && [ "$$(hostname -s)" != "$(FRONTEND_VCS_HOST)" ]; then \
		echo "frontend VCS build must run on $(FRONTEND_VCS_HOST)"; \
		echo "run this target on $(FRONTEND_VCS_HOST), or unset FRONTEND_VCS_HOST for an unrestricted build host"; \
		exit 2; \
	fi
	@if [ "$(FRONTEND_SIM)" = "vcs" ] && { [ -z "$(FRONTEND_VCS_HOME)" ] || [ -z "$(FRONTEND_VERDI_HOME)" ]; }; then \
		echo "frontend VCS build requires FRONTEND_VCS_HOME and FRONTEND_VERDI_HOME"; \
		echo "example: make frontend-vcs FRONTEND_VCS_HOME=/path/to/vcs FRONTEND_VERDI_HOME=/path/to/verdi"; \
		exit 2; \
	fi
	@if [ "$(FRONTEND_SIM)" = "vcs" ] && { [ ! -d "$(FRONTEND_VCS_HOME)" ] || [ ! -d "$(FRONTEND_VERDI_HOME)" ]; }; then \
		echo "frontend VCS build requires existing tool directories"; \
		echo "FRONTEND_VCS_HOME=$(FRONTEND_VCS_HOME)"; \
		echo "FRONTEND_VERDI_HOME=$(FRONTEND_VERDI_HOME)"; \
		exit 2; \
	fi
	rm -rf $(FRONTEND_PYLIB_DIR)
	mkdir -p $(FRONTEND_CCACHE_DIR) $(FRONTEND_CCACHE_TMP)
	@frontend_waveform_format="$$(cat "$(FRONTEND_WAVEFORM_FORMAT_FILE)" 2>/dev/null || printf '%s' '$(FRONTEND_WAVEFORM_FORMAT)')"; \
	$(FRONTEND_BUILD_ENV) time $(FRONTEND_PICKER) export $(dir $<)ClockGate.sv --sname Frontend \
		--filelist $(FRONTEND_PICKER_FILELIST) \
		--lang python --autobuild true --cp_lib true \
		--sim $(FRONTEND_SIM) --access-mode $(FRONTEND_ACCESS_MODE) \
		--tdir $(FRONTEND_PYLIB_DIR) \
		-w $(FRONTEND_BUILD_DIR)/frontend.$$frontend_waveform_format \
		--coverage $(FRONTEND_PICKER_SIM_ARGS)
$(FRONTEND_BUILD_MANIFEST): $(FRONTEND_PYLIB) $(FRONTEND_WAVEFORM_FORMAT_FILE) $(FRONTEND_CONFIG_FILE)
	@frontend_waveform_format="$$(cat "$(FRONTEND_WAVEFORM_FORMAT_FILE)" 2>/dev/null || printf '%s' '$(FRONTEND_WAVEFORM_FORMAT)')"; \
	python3 src/test/python/Frontend/tools/write_frontend_build_manifest.py \
		--repo-root . \
		--build-root $(FRONTEND_BUILD_DIR) \
		--output $(FRONTEND_BUILD_MANIFEST) \
		--sim $(FRONTEND_SIM) \
		--dut-source-sha "$${FRONTEND_DUT_SOURCE_SHA:-}" \
		--design-baseline-sha "$${FRONTEND_DESIGN_BASELINE_SHA:-}" \
		--build-config "CONFIG=$(CONFIG);ISSUE=$(ISSUE);NUM_CORES=$(NUM_CORES);CHISEL_TARGET=$(CHISEL_TARGET);WAVEFORM=$$frontend_waveform_format" \
		--build-command "make frontend CONFIG=$(CONFIG) ISSUE=$(ISSUE) NUM_CORES=$(NUM_CORES) CHISEL_TARGET=$(CHISEL_TARGET) FRONTEND_WAVEFORM_FORMAT=$$frontend_waveform_format"
frontend: $(FRONTEND_BUILD_MANIFEST)
.PHONY: frontend

frontend-verilator:
	$(MAKE) frontend FRONTEND_SIM=verilator
.PHONY: frontend-verilator

frontend-vcs:
	$(MAKE) frontend FRONTEND_SIM=vcs FRONTEND_WAVEFORM_FORMAT=fsdb
.PHONY: frontend-vcs

verilog: $(FRONTEND_WAVEFORM_FORMAT_FILE) $(call docker-deps,$(TOP_V))

$(SIM_TOP_V): $(SCALA_FILE) $(TEST_FILE)
	mkdir -p $(@D) $(dir $(TIMELOG))
	@echo -e "\n[mill] Generating Verilog files..." > $(TIMELOG)
	@date -R | tee -a $(TIMELOG)
	$(MILL_ENV) $(TIME_CMD) mill -i $(MILL_BUILD_ARGS) xiangshan.test.runMain $(SIMTOP) \
		--target-dir $(@D) --config $(CONFIG) --issue $(ISSUE) \
		--num-cores $(NUM_CORES) $(SIM_ARGS) --full-stacktrace
ifeq ($(CHISEL_TARGET),systemverilog)
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

GIT_FORCE_FLAG := $(if $(GIT_FORCE_INIT),--force)

# Initialize necessary submodules
init:
	git submodule update --init $(GIT_FORCE_FLAG)
	cd rocket-chip && git submodule update --init $(GIT_FORCE_FLAG) cde hardfloat
	cd XSCache && git submodule update --init $(GIT_FORCE_FLAG) OpenNCB

# Initialize necessary submodules (force)
#   This ensure that all submodules files are checked out to the correct commit. Good for CI.
init-force:
	$(MAKE) init GIT_FORCE_INIT=1

bump:
	git submodule foreach "git fetch origin&&git checkout master&&git reset --hard origin/master"

deps:
	$(MILL) -i __.prepareOffline
	$(MILL) -i xiangshan.resolveFirtoolDeps

bsp:
	$(MILL) -i mill.bsp.BSP/install

idea:
	$(MILL) -i mill.idea.GenIdea/idea

check-format:
	$(MILL) xiangshan.checkFormat

reformat:
	$(MILL) xiangshan.reformat

# verilator simulation
emu-mk: sim-verilog
	$(MAKE) -C ./difftest emu-mk NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX)

emu: $(call docker-deps,emu-mk)
	$(MAKE) -C ./difftest emu NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX) OBJCACHE=$(OBJCACHE)

gsim: sim-verilog
	$(MAKE) -C ./difftest emu GSIM=1 SIM_TOP=SimTop DESIGN_DIR=$(NOOP_HOME) NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX)

# vcs simulation
simv: sim-verilog
	$(MAKE) -C ./difftest simv NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX)

simv-run:
	$(MAKE) -C ./difftest simv-run NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX)

# galaxsim simulation
xsim: sim-verilog
	$(MAKE) -C ./difftest xsim NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX)

xsim-run:
	$(MAKE) -C ./difftest xsim-run NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX)

# palladium simulation
pldm-build: sim-verilog
	$(MAKE) -C ./difftest pldm-build NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX)

pldm-run:
	$(MAKE) -C ./difftest pldm-run NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX)

pldm-debug:
	$(MAKE) -C ./difftest pldm-debug NUM_CORES=$(NUM_CORES) RTL_SUFFIX=$(RTL_SUFFIX)

include Makefile.test

include src/main/scala/device/standalone/standalone_device.mk

.PHONY: FORCE verilog sim-verilog gsim emu clean help init init-force bump bsp $(REF_SO)
