TOP = TopMain
BUILD_DIR = ./build
TOP_V = $(BUILD_DIR)/$(TOP).v
SCALA_FILE = $(shell find ./src/main/scala -name '*.scala')

SIMTOP = top.TestMain
IMAGE = ""
SIMCMD = test:runMain $(SIMTOP) -td $(BUILD_DIR) --image $(IMAGE)

.DEFAULT_GOAL = verilog

help:
	sbt 'test:runMain gcd.GCDMain --help'

LIBDEVICE_PATH = ./src/test/cpp/libdevice
libdevice:
	make -C $(LIBDEVICE_PATH)

$(TOP_V): $(SCALA_FILE)
	mkdir -p $(@D)
	sbt 'runMain top.$(TOP) -td $(@D) --output-file $@'
	sed -i -e 's/_\(aw\|ar\|w\|r\|b\)_\(\|bits_\)/_\1/g' $@

verilog: $(TOP_V)

test: libdevice
	sbt '$(SIMCMD) --tr-rollback-buffers 0'

emu: libdevice
	sbt '$(SIMCMD) --backend-name verilator --generate-vcd-output off'

clean:
	rm -rf $(BUILD_DIR)

.PHONY: libdevice verilog test emu clean help
