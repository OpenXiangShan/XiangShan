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

SIMTOP = top.TestMain
IMAGE ?= temp

# co-simulation with DRAMsim3
ifeq ($(WITH_DRAMSIM3),1)
ifndef DRAMSIM3_HOME
$(error DRAMSIM3_HOME is not set)
endif
override SIM_ARGS += --with-dramsim3
endif

# remote machine with more cores to speedup c++ build
REMOTE ?= localhost

.DEFAULT_GOAL = verilog

help:
	mill -i XiangShan.test.runMain $(SIMTOP) --help

$(TOP_V): $(SCALA_FILE)
	mkdir -p $(@D)
	mill -i XiangShan.test.runMain $(FPGATOP) -td $(@D) --full-stacktrace --output-file $(@F) --disable-all --remove-assert --infer-rw --repl-seq-mem -c:$(FPGATOP):-o:$(@D)/$(@F).conf $(SIM_ARGS)
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

SIM_TOP   = XSSimTop
SIM_TOP_V = $(BUILD_DIR)/$(SIM_TOP).v
$(SIM_TOP_V): $(SCALA_FILE) $(TEST_FILE)
	mkdir -p $(@D)
	date -R
	mill -i XiangShan.test.runMain $(SIMTOP) -X verilog -td $(@D) --full-stacktrace --output-file $(@F) --infer-rw --repl-seq-mem -c:$(SIMTOP):-o:$(@D)/$(@F).conf $(SIM_ARGS)
	$(MEM_GEN) $(@D)/$(@F).conf --output_file $(@D)/$(@F).sram.v
	@git log -n 1 >> .__head__
	@git diff >> .__diff__
	@sed -i 's/^/\/\// ' .__head__
	@sed -i 's/^/\/\//' .__diff__
	@cat .__head__ .__diff__ $@ $(@D)/$(@F).sram.v > .__out__
	@mv .__out__ $@
	@rm .__head__ .__diff__
	sed -i '/module XSSimTop/,/endmodule/d' $(SIM_TOP_V)
	sed -i -e 's/$$fatal/xs_assert(`__LINE__)/g' $(SIM_TOP_V)
	date -R

EMU_TOP      = XSSimSoC
EMU_CSRC_DIR = $(abspath ./src/test/csrc)
EMU_VSRC_DIR = $(abspath ./src/test/vsrc)
EMU_CXXFILES = $(shell find $(EMU_CSRC_DIR) -name "*.cpp")
EMU_VFILES   = $(shell find $(EMU_VSRC_DIR) -name "*.v" -or -name "*.sv")

EMU_CXXFLAGS += -std=c++11 -static -Wall -I$(EMU_CSRC_DIR)
EMU_CXXFLAGS += -DVERILATOR
EMU_LDFLAGS  += -lpthread -ldl -lz

VEXTRA_FLAGS  = -I$(abspath $(BUILD_DIR)) --x-assign unique -O3 -CFLAGS "$(EMU_CXXFLAGS)" -LDFLAGS "$(EMU_LDFLAGS)"

# Verilator version check
VERILATOR_4_210 := $(shell expr `verilator --version | cut -f3 -d.` \>= 210)
ifeq ($(VERILATOR_4_210),1)
EMU_CXXFLAGS += -DVERILATOR_4_210
VEXTRA_FLAGS += --instr-count-dpi 1
endif

# Verilator trace support
EMU_TRACE ?=
ifeq ($(EMU_TRACE),1)
VEXTRA_FLAGS += --trace
endif

# Verilator multi-thread support
EMU_THREADS  ?= 1
ifneq ($(EMU_THREADS),1)
VEXTRA_FLAGS += --threads $(EMU_THREADS) --threads-dpi all
endif

# Verilator savable
EMU_SNAPSHOT ?=
ifeq ($(EMU_SNAPSHOT),1)
VEXTRA_FLAGS += --savable
EMU_CXXFLAGS += -DVM_SAVABLE
endif

# Verilator coverage
EMU_COVERAGE ?=
ifeq ($(EMU_COVERAGE),1)
VEXTRA_FLAGS += --coverage-line --coverage-toggle
endif

# co-simulation with DRAMsim3
ifeq ($(WITH_DRAMSIM3),1)
EMU_CXXFLAGS += -I$(DRAMSIM3_HOME)/src
EMU_CXXFLAGS += -DWITH_DRAMSIM3 -DDRAMSIM3_CONFIG=\\\"$(DRAMSIM3_HOME)/configs/XiangShan.ini\\\" -DDRAMSIM3_OUTDIR=\\\"$(BUILD_DIR)\\\"
EMU_LDFLAGS  += $(DRAMSIM3_HOME)/build/libdramsim3.a
endif

ifeq ($(DUALCORE),1)
EMU_CXXFLAGS += -DDUALCORE
endif

USE_BIN ?= 0
ifeq ($(USE_BIN),1)
EMU_CXXFLAGS += -DUSE_BIN
endif

SHOW_SCREEN ?= 0
ifeq ($(SHOW_SCREEN),1)
EMU_LDFLAGS  += -lSDL2
EMU_CXXFLAGS += -DSHOW_SCREEN
endif

# --trace
VERILATOR_FLAGS = --top-module $(EMU_TOP) \
  +define+VERILATOR=1                     \
  +define+PRINTF_COND=1                   \
  +define+RANDOMIZE_REG_INIT              \
  +define+RANDOMIZE_MEM_INIT              \
  +define+RANDOMIZE_GARBAGE_ASSIGN        \
  +define+RANDOMIZE_DELAY=0               \
  $(VEXTRA_FLAGS)                         \
  -Wno-STMTDLY -Wno-WIDTH                 \
  --assert                                \
  --stats-vars                            \
  --output-split 30000                    \
  --output-split-cfuncs 30000             \
  --compiler clang

EMU_MK := $(BUILD_DIR)/emu-compile/V$(EMU_TOP).mk
EMU_DEPS := $(EMU_VFILES) $(EMU_CXXFILES)
EMU_HEADERS := $(shell find $(EMU_CSRC_DIR) -name "*.h")
EMU := $(BUILD_DIR)/emu

$(EMU_MK): $(SIM_TOP_V) | $(EMU_DEPS)
	@mkdir -p $(@D)
	date -R
	verilator --cc --exe $(VERILATOR_FLAGS) \
		-o $(abspath $(EMU)) -Mdir $(@D) $^ $(EMU_DEPS)
	date -R

EMU_VCS := simv

VCS_SRC_FILE = $(TOP_V) \
               $(BUILD_DIR)/plusarg_reader.v \
               $(BUILD_DIR)/SDHelper.v

VCS_TB_DIR = $(abspath ./src/test/vcs)
VCS_TB_FILE = $(shell find $(VCS_TB_DIR) -name "*.c") \
              $(shell find $(VCS_TB_DIR) -name "*.v")

VCS_OPTS := -full64 +v2k -timescale=1ns/10ps \
  -LDFLAGS -Wl,--no-as-needed \
  -sverilog \
  +lint=TFIPC-L \
  -debug_all +vcd+vcdpluson \
  +define+RANDOMIZE_GARBAGE_ASSIGN \
  +define+RANDOMIZE_INVALID_ASSIGN \
  +define+RANDOMIZE_REG_INIT \
  +define+RANDOMIZE_MEM_INIT \
  +define+RANDOMIZE_DELAY=1

$(EMU_VCS): $(VCS_SRC_FILE) $(VCS_TB_FILE)
	rm -rf csrc
	vcs $(VCS_OPTS) $(VCS_SRC_FILE) $(VCS_TB_FILE)

ifndef NEMU_HOME
$(error NEMU_HOME is not set)
endif
REF_SO := $(NEMU_HOME)/build/riscv64-nemu-interpreter-so
$(REF_SO):
	$(MAKE) -C $(NEMU_HOME) ISA=riscv64 SHARE=1

LOCK = /var/emu/emu.lock
LOCK_BIN = $(abspath $(BUILD_DIR)/lock-emu)

$(LOCK_BIN): ./scripts/utils/lock-emu.c
	gcc $^ -o $@

$(EMU): $(EMU_MK) $(EMU_DEPS) $(EMU_HEADERS) $(REF_SO) $(LOCK_BIN)
	date -R
ifeq ($(REMOTE),localhost)
	CPPFLAGS=-DREF_SO=\\\"$(REF_SO)\\\" $(MAKE) VM_PARALLEL_BUILDS=1 OPT_FAST="-O3" -C $(abspath $(dir $(EMU_MK))) -f $(abspath $(EMU_MK))
else
	@echo "try to get emu.lock ..."
	ssh -tt $(REMOTE) '$(LOCK_BIN) $(LOCK)'
	@echo "get lock"
	ssh -tt $(REMOTE) 'CPPFLAGS=-DREF_SO=\\\"$(REF_SO)\\\" $(MAKE) -j230 VM_PARALLEL_BUILDS=1 OPT_FAST="-O3" -C $(abspath $(dir $(EMU_MK))) -f $(abspath $(EMU_MK))'
	@echo "release lock ..."
	ssh -tt $(REMOTE) 'rm -f $(LOCK)'
endif
	date -R

SEED ?= $(shell shuf -i 1-10000 -n 1)

VME_SOURCE ?= $(shell pwd)/build/$(TOP).v
VME_MODULES ?=

# log will only be printed when (B<=GTimer<=E) && (L < loglevel)
# use 'emu -h' to see more details
B ?= 0
E ?= -1
SNAPSHOT ?=

# enable this runtime option if you want to generate a vcd file
# use 'emu -h' to see more details
#WAVEFORM = --dump-wave

ifeq ($(SNAPSHOT),)
SNAPSHOT_OPTION =
else
SNAPSHOT_OPTION = --load-snapshot=$(SNAPSHOT)
endif

ifndef NOOP_HOME
$(error NOOP_HOME is not set)
endif
EMU_FLAGS = -s $(SEED) -b $(B) -e $(E) $(SNAPSHOT_OPTION) $(WAVEFORM) $(EMU_ARGS)

emu: $(EMU)
	ls build
	$(EMU) -i $(IMAGE) $(EMU_FLAGS)

coverage:
	verilator_coverage --annotate build/logs/annotated --annotate-min 1 build/logs/coverage.dat
	python3 scripts/coverage/coverage.py build/logs/annotated/XSSimTop.v build/XSSimTop_annotated.v
	python3 scripts/coverage/statistics.py build/XSSimTop_annotated.v >build/coverage.log

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

clean:
	git submodule foreach git clean -fdx
	git clean -fd
	rm -rf ./build

init:
	git submodule update --init

bump:
	git submodule foreach "git fetch origin&&git checkout master&&git reset --hard origin/master"

bsp:
	mill -i mill.contrib.BSP/install
.PHONY: verilog emu clean help init bump bsp $(REF_SO)
