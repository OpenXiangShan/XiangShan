TOP = TopMain
FPGATOP = FPGANOOP
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
	mill XiangShan.test.runMain top.$(TOP) --help

$(TOP_V): $(SCALA_FILE)
	mkdir -p $(@D)
	mill XiangShan.test.runMain $(SIMTOP) -X verilog -td $(@D) --full-stacktrace --output-file $(@F) --disable-all --fpga-platform $(SIM_ARGS)
	# mill XiangShan.runMain top.$(TOP) -X verilog -td $(@D) --output-file $(@F) --infer-rw $(FPGATOP) --repl-seq-mem -c:$(FPGATOP):-o:$(@D)/$(@F).conf
	# $(MEM_GEN) $(@D)/$(@F).conf >> $@
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

SIM_TOP   = XSSimTop
SIM_TOP_V = $(BUILD_DIR)/$(SIM_TOP).v
$(SIM_TOP_V): $(SCALA_FILE) $(TEST_FILE)
	mkdir -p $(@D)
	date -R
	mill XiangShan.test.runMain $(SIMTOP) -X verilog -td $(@D) --full-stacktrace --output-file $(@F) $(SIM_ARGS)
	sed -i '/module XSSimTop/,/endmodule/d' $(SIM_TOP_V)
	sed -i -e 's/$$fatal/$$finish/g' $(SIM_TOP_V)
	date -R

EMU_TOP      = XSSimSoC
EMU_CSRC_DIR = $(abspath ./src/test/csrc)
EMU_VSRC_DIR = $(abspath ./src/test/vsrc)
EMU_CXXFILES = $(shell find $(EMU_CSRC_DIR) -name "*.cpp")
EMU_VFILES   = $(shell find $(EMU_VSRC_DIR) -name "*.v" -or -name "*.sv")

EMU_CXXFLAGS += -std=c++11 -static -Wall -I$(EMU_CSRC_DIR)
EMU_CXXFLAGS += -DVERILATOR -Wno-maybe-uninitialized
EMU_LDFLAGS  += -lpthread -lSDL2 -ldl -lz

VEXTRA_FLAGS  = -I$(abspath $(BUILD_DIR)) --x-assign unique -O3 -CFLAGS "$(EMU_CXXFLAGS)" -LDFLAGS "$(EMU_LDFLAGS)"

# Verilator trace support
EMU_TRACE ?=
ifeq ($(EMU_TRACE),1)
VEXTRA_FLAGS += --trace
endif

# Verilator multi-thread support
EMU_THREADS  ?= 1
ifneq ($(EMU_THREADS),1)
VEXTRA_FLAGS += --threads $(EMU_THREADS) --threads-dpi none
endif

# Verilator savable
EMU_SNAPSHOT ?=
ifeq ($(EMU_SNAPSHOT),1)
VEXTRA_FLAGS += --savable
EMU_CXXFLAGS += -DVM_SAVABLE
endif

# co-simulation with DRAMsim3
ifeq ($(WITH_DRAMSIM3),1)
EMU_CXXFLAGS += -I$(DRAMSIM3_HOME)/src
EMU_CXXFLAGS += -DWITH_DRAMSIM3 -DDRAMSIM3_CONFIG=\\\"$(DRAMSIM3_HOME)/configs/XiangShan.ini\\\" -DDRAMSIM3_OUTDIR=\\\"$(BUILD_DIR)\\\"
EMU_LDFLAGS  += $(DRAMSIM3_HOME)/build/libdramsim3.a
endif

# --trace
VERILATOR_FLAGS = --top-module $(EMU_TOP) \
  +define+VERILATOR=1 \
  +define+PRINTF_COND=1 \
  +define+RANDOMIZE_REG_INIT \
  +define+RANDOMIZE_MEM_INIT \
  $(VEXTRA_FLAGS) \
  --assert \
  --stats-vars \
  --output-split 5000 \
  --output-split-cfuncs 5000

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

ifndef NEMU_HOME
$(error NEMU_HOME is not set)
endif
REF_SO := $(NEMU_HOME)/build/riscv64-nemu-interpreter-so
$(REF_SO):
	$(MAKE) -C $(NEMU_HOME) ISA=riscv64 SHARE=1

$(EMU): $(EMU_MK) $(EMU_DEPS) $(EMU_HEADERS) $(REF_SO)
	date -R
ifeq ($(REMOTE),localhost)
	CPPFLAGS=-DREF_SO=\\\"$(REF_SO)\\\" $(MAKE) VM_PARALLEL_BUILDS=1 OPT_FAST="-O3" -C $(abspath $(dir $(EMU_MK))) -f $(abspath $(EMU_MK))
else
	ssh -tt $(REMOTE) 'CPPFLAGS=-DREF_SO=\\\"$(REF_SO)\\\" $(MAKE) -j128 VM_PARALLEL_BUILDS=1 OPT_FAST="-O3" -C $(abspath $(dir $(EMU_MK))) -f $(abspath $(EMU_MK))'
endif
	date -R

SEED ?= $(shell shuf -i 1-10000 -n 1)

VME_SOURCE ?= $(shell pwd)
VME_MODULE ?= 

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
EMU_FLAGS = -s $(SEED) -b $(B) -e $(E) $(SNAPSHOT_OPTION) $(WAVEFORM)

emu: $(EMU)
	ls build
	$(EMU) -i $(IMAGE) $(EMU_FLAGS)

# extract verilog module from sim_top.v
# usage: make vme VME_MODULE=Roq
vme: $(SIM_TOP_V)
	mill XiangShan.runMain utils.ExtractVerilogModules -m $(VME_MODULE)

# usage: make phy_evaluate VME_MODULE=Roq REMOTE=100
phy_evaluate: vme
	scp -r ./build/extracted/* $(REMOTE):~/phy_evaluation/remote_run/rtl
	ssh -tt $(REMOTE) 'cd ~/phy_evaluation/remote_run && $(MAKE) evaluate DESIGN_NAME=$(VME_MODULE)'
	scp -r  $(REMOTE):~/phy_evaluation/remote_run/rpts ./build/rpts

# usage: make phy_evaluate_atc VME_MODULE=Roq REMOTE=100
phy_evaluate_atc: vme
	scp -r ./build/extracted/* $(REMOTE):~/phy_evaluation/remote_run/rtl
	ssh -tt $(REMOTE) 'cd ~/phy_evaluation/remote_run && $(MAKE) evaluate_atc DESIGN_NAME=$(VME_MODULE)'
	scp -r  $(REMOTE):~/phy_evaluation/remote_run/rpts ./build/rpts

cache:
	$(MAKE) emu IMAGE=Makefile

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
