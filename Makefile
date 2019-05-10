TOP = TopMain
BUILD_DIR = ./build
TOP_V = $(BUILD_DIR)/$(TOP).v
SCALA_FILE = $(shell find ./src/main/scala -name '*.scala')

SIMTOP = top.TestMain
IMAGE = ""
SIMCMD = test:runMain $(SIMTOP) -td $(BUILD_DIR) --image $(IMAGE) \
	--more-vcs-flags "+define+RANDOMIZE_REG_INIT"

.DEFAULT_GOAL = verilog

help:
	sbt 'test:runMain gcd.GCDMain --help'

LIBDEVICE_PATH = ./src/test/libdevice
libdevice:
	make -C $(LIBDEVICE_PATH)

$(TOP_V): $(SCALA_FILE)
	mkdir -p $(@D)
	sbt 'runMain top.$(TOP) -td $(@D) --output-file $@'
	sed -i -e 's/_\(aw\|ar\|w\|r\|b\)_\(\|bits_\)/_\1/g' $@

verilog: $(TOP_V)

SIM_TOP = NOOPSimTop
SIM_TOP_V = $(BUILD_DIR)/$(SIM_TOP).v
$(SIM_TOP_V): $(SCALA_FILE)
	mkdir -p $(@D)
	sbt 'test:runMain $(SIMTOP) -td $(BUILD_DIR) --image $(IMAGE) --output-file $@'

test: libdevice
	sbt '$(SIMCMD) --tr-rollback-buffers 0'


EMU_CSRC_DIR = $(abspath ./src/test/csrc)
EMU_VSRC_DIR = $(abspath ./src/test/vsrc)
EMU_CXXFILES = $(shell find $(EMU_CSRC_DIR) -name "*.cpp")
EMU_VFILES = $(shell find $(EMU_VSRC_DIR) -name "*.v" -or -name "*.sv")

EMU_CXXFLAGS  = -O3 -std=c++11 -static -g -Wall -I$(EMU_CSRC_DIR)
EMU_CXXFLAGS += -DVERILATOR -Wno-maybe-uninitialized
EMU_LDFLAGS   = -lpthread -lreadline -lSDL

VERILATOR_FLAGS = --top-module $(SIM_TOP) \
  +define+PRINTF_COND=1 \
	+define+RANDOMIZE_REG_INIT \
  --assert --output-split 20000 \
  --x-assign unique -O3 -CFLAGS "$(EMU_CXXFLAGS)" \
   -LDFLAGS "$(EMU_LDFLAGS)"

EMU_MK := $(BUILD_DIR)/emu-compile/V$(SIM_TOP).mk
EMU_DEPS := $(EMU_VFILES) $(EMU_CXXFILES)
EMU_HEADERS := $(shell find $(EMU_CSRC_DIR) -name "*.h")
EMU := $(BUILD_DIR)/emu

$(EMU_MK): $(SIM_TOP_V) | $(EMU_DEPS)
	@mkdir -p $(@D)
	verilator --cc --exe $(VERILATOR_FLAGS) \
		-o $(abspath $(EMU)) -Mdir $(@D) \
		$^ $(EMU_DEPS)

$(EMU): $(EMU_MK) $(EMU_DEPS) $(EMU_HEADERS)
	$(MAKE) -C $(dir $(EMU_MK)) -f $(abspath $(EMU_MK))

emu: $(EMU)
	$(EMU)

#emu: libdevice
#	sbt '$(SIMCMD) --backend-name verilator --generate-vcd-output off'

clean:
	rm -rf $(BUILD_DIR)

.PHONY: libdevice verilog test emu clean help
