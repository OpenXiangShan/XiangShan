TOP = TopMain
BUILD_DIR = ./build
TOP_V = $(BUILD_DIR)/$(TOP).v
SCALA_FILE = $(shell find ./src/main/scala -name '*.scala')

SIMTOP = top.TestMain
IMAGE = ""
SIMCMD = test:runMain $(SIMTOP) -td $(BUILD_DIR) --image $(IMAGE)

.DEFAULT_GOAL = verilog

LIBDEVICE_PATH = ./src/test/cpp/libdevice
libdevice:
	make -C $(LIBDEVICE_PATH)

$(TOP_V): $(SCALA_FILE)
	mkdir -p $(@D)
	sbt 'runMain top.$(TOP) -td $(@D) --output-file $@'

verilog: $(TOP_V)

test: libdevice
	sbt '$(SIMCMD)'

emu: libdevice
	sbt '$(SIMCMD) --backend-name verilator --generate-vcd-output off'

clean:
	rm -rf $(OBJ_DIR)/*

.PHONY: libdevice verilog test emu clean
