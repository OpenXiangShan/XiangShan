TOP = TopMain
FPGATOP = FPGANOOP
BUILD_DIR = ./build
TOP_V = $(BUILD_DIR)/$(TOP).v
SCALA_FILE = $(shell find ./src/main/scala -name '*.scala')
TEST_FILE = $(shell find ./src/test/scala -name '*.scala')
MEM_GEN = ./scripts/vlsi_mem_gen

SIMTOP = top.TestMain
IMAGE ?= temp

.DEFAULT_GOAL = verilog

help:
	mill chiselModule.test.runMain top.$(TOP) --help

$(TOP_V): $(SCALA_FILE)
	mkdir -p $(@D)
	mill chiselModule.runMain top.$(TOP) -td $(@D) --output-file $(@F) --infer-rw $(FPGATOP) --repl-seq-mem -c:$(FPGATOP):-o:$(@D)/$(@F).conf
	$(MEM_GEN) $(@D)/$(@F).conf >> $@
	sed -i -e 's/_\(aw\|ar\|w\|r\|b\)_\(\|bits_\)/_\1/g' $@

verilog: $(TOP_V)

SIM_TOP = NOOPSimTop
SIM_TOP_V = $(BUILD_DIR)/$(SIM_TOP).v
$(SIM_TOP_V): $(SCALA_FILE) $(TEST_FILE)
	mkdir -p $(@D)
	mill chiselModule.test.runMain $(SIMTOP) -td $(@D) --output-file $(@F)


EMU_CSRC_DIR = $(abspath ./src/test/csrc)
EMU_VSRC_DIR = $(abspath ./src/test/vsrc)
EMU_CXXFILES = $(shell find $(EMU_CSRC_DIR) -name "*.cpp")
EMU_VFILES = $(shell find $(EMU_VSRC_DIR) -name "*.v" -or -name "*.sv")

EMU_CXXFLAGS  = -O3 -std=c++11 -static -g -Wall -I$(EMU_CSRC_DIR)
EMU_CXXFLAGS += -DVERILATOR -Wno-maybe-uninitialized
EMU_LDFLAGS   = -lpthread -lSDL2 -ldl

VERILATOR_FLAGS = --top-module $(SIM_TOP) \
  +define+VERILATOR=1 \
  +define+PRINTF_COND=1 \
	+define+RANDOMIZE_REG_INIT \
  --assert --output-split 20000 \
  --trace  \
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

REF_SO := $(NEMU_HOME)/build/riscv64-nemu-so
$(REF_SO):
	$(MAKE) -C $(NEMU_HOME) ISA=riscv64 SHARE=1

$(EMU): $(EMU_MK) $(EMU_DEPS) $(EMU_HEADERS) $(REF_SO)
	CPPFLAGS=-DREF_SO=\\\"$(REF_SO)\\\" $(MAKE) -C $(dir $(EMU_MK)) -f $(abspath $(EMU_MK))

ifdef mainargs
MAINARGS = -m $(mainargs)
endif

emu: $(EMU)
	@$(EMU) -i $(IMAGE) $(MAINARGS)

cache:
	$(MAKE) emu IMAGE=Makefile #SIMTOP=top.CacheTestMain SIM_TOP=CacheTestTop IMAGE=Makefile

clean:
	rm -rf $(BUILD_DIR)

.PHONY: verilog emu clean help $(REF_SO)
