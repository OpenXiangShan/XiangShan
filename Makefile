TOP = TopMain
FPGATOP = FPGANOOP
BUILD_DIR = ./build
TOP_V = $(BUILD_DIR)/$(TOP).v
SCALA_FILE = $(shell find ./src/main/scala -name '*.scala')
TEST_FILE = $(shell find ./src/test/scala -name '*.scala')
MEM_GEN = ./scripts/vlsi_mem_gen

SIMTOP = top.TestMain
IMAGE ?= temp

# remote machine with high frequency to speedup verilog generation
REMOTE ?= localhost
REMOTE_PREFIX ?= 
REMOTE_PRJ_HOME = $(REMOTE_PREFIX)/$(abspath .)/

.DEFAULT_GOAL = verilog

help:
	mill chiselModule.test.runMain top.$(TOP) --help

$(TOP_V): $(SCALA_FILE)
	mkdir -p $(@D)
	mill chiselModule.runMain top.$(TOP) -X verilog -td $(@D) --output-file $(@F) --infer-rw $(FPGATOP) --repl-seq-mem -c:$(FPGATOP):-o:$(@D)/$(@F).conf
	$(MEM_GEN) $(@D)/$(@F).conf >> $@
	sed -i -e 's/_\(aw\|ar\|w\|r\|b\)_\(\|bits_\)/_\1/g' $@
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

SIM_TOP = XSSimTop
SIM_TOP_V = $(BUILD_DIR)/$(SIM_TOP).v
SIM_ARGS =
$(SIM_TOP_V): $(SCALA_FILE) $(TEST_FILE)
	mkdir -p $(@D)
ifeq ($(REMOTE),localhost)
	mill chiselModule.test.runMain $(SIMTOP) -X verilog -td $(@D) --output-file $(@F) $(SIM_ARGS)
else
	ssh -tt $(REMOTE) "cd $(REMOTE_PRJ_HOME) && mill chiselModule.test.runMain $(SIMTOP) -X verilog -td $(@D) --output-file $(@F) $(SIM_ARGS)"
endif


EMU_CSRC_DIR = $(abspath ./src/test/csrc)
EMU_VSRC_DIR = $(abspath ./src/test/vsrc)
EMU_CXXFILES = $(shell find $(EMU_CSRC_DIR) -name "*.cpp")
EMU_VFILES = $(shell find $(EMU_VSRC_DIR) -name "*.v" -or -name "*.sv")

EMU_CXXFLAGS  = -std=c++11 -static -Wall -I$(EMU_CSRC_DIR)
EMU_CXXFLAGS += -DVERILATOR -Wno-maybe-uninitialized
EMU_LDFLAGS   = -lpthread -lSDL2 -ldl

# dump vcd: --debug --trace
VERILATOR_FLAGS = --top-module $(SIM_TOP) \
  +define+VERILATOR=1 \
  +define+PRINTF_COND=1 \
  +define+RANDOMIZE_REG_INIT \
  +define+RANDOMIZE_MEM_INIT \
  --assert \
  --savable \
  --stats-vars \
  --output-split 5000 \
  --output-split-cfuncs 5000 \
  -I$(abspath $(BUILD_DIR)) \
  --x-assign unique -O3 -CFLAGS "$(EMU_CXXFLAGS)" \
  -LDFLAGS "$(EMU_LDFLAGS)"

EMU_MK := $(BUILD_DIR)/emu-compile/V$(SIM_TOP).mk
EMU_DEPS := $(EMU_VFILES) $(EMU_CXXFILES)
EMU_HEADERS := $(shell find $(EMU_CSRC_DIR) -name "*.h")
EMU := $(BUILD_DIR)/emu

$(EMU_MK): $(SIM_TOP_V) | $(EMU_DEPS)
	@mkdir -p $(@D)
	verilator --cc --exe $(VERILATOR_FLAGS) \
		-o $(abspath $(EMU)) -Mdir $(@D) $^ $(EMU_DEPS)

ifeq ($(REMOTE),localhost)
REF_SO := $(NEMU_HOME)/build/riscv64-nemu-interpreter-so
else
REF_SO := /home/pcl/NEMU/build/riscv64-nemu-interpreter-so
endif

$(REF_SO):
	$(MAKE) -C $(NEMU_HOME) ISA=riscv64 SHARE=1

$(EMU): $(EMU_MK) $(EMU_DEPS) $(EMU_HEADERS) $(REF_SO)
	CPPFLAGS=-DREF_SO=\\\"$(REF_SO)\\\" time $(MAKE) VM_PARALLEL_BUILDS=1 OPT_FAST="-O3" -C $(dir $(EMU_MK)) -f $(abspath $(EMU_MK))

SEED ?= $(shell shuf -i 1-10000 -n 1)


# log will only be printed when (B<=GTimer<=E) && (L < loglevel)
# use 'emu -h' to see more details
B ?= 0
E ?= -1
SNAPSHOT ?=

ifeq ($(SNAPSHOT),)
SNAPSHOT_OPTION = 
else
SNAPSHOT_OPTION = --load-snapshot=$(REMOTE_PREFIX)/$(SNAPSHOT)
endif

EMU_FLAGS = -s $(SEED) -b $(B) -e $(E) $(SNAPSHOT_OPTION)

emu: $(EMU)
ifeq ($(REMOTE),localhost)
	@$(EMU) -i $(IMAGE) $(EMU_FLAGS)
else
	ssh -tt $(REMOTE) "cd $(REMOTE_PRJ_HOME) && export NOOP_HOME=$(REMOTE_PREFIX)/$(NOOP_HOME) && $(EMU) -i $(REMOTE_PREFIX)/$(IMAGE) $(EMU_FLAGS)"
endif

cache:
	$(MAKE) emu IMAGE=Makefile

clean:
	rm -rf $(BUILD_DIR)

.PHONY: verilog emu clean help $(REF_SO)
