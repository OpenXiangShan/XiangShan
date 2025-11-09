#***************************************************************************************
# Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
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

BUILD_DIR = $(abspath ./build)
RTL_DIR = $(BUILD_DIR)/rtl

TOP_V = $(RTL_DIR)/$(TOP).sv
SCALA_FILE = $(shell find ./src/main/scala -name '*.scala')
TEST_FILE = $(shell find ./src/test/scala -name '*.scala')

FPGATOP = top.TopMain
SIMTOP = top.TestMain

# co-simulation with DRAMsim3
ifeq ($(WITH_DRAMSIM3),1)
ifndef DRAMSIM3_HOME
$(error DRAMSIM3_HOME is not set)
endif
override SIM_ARGS += --with-dramsim3
endif

.DEFAULT_GOAL = verilog

help:
	mill -i XiangShan.test.runMain $(SIMTOP) --help

$(TOP_V): $(SCALA_FILE)
	mkdir -p $(@D)
	mill -i XiangShan.test.runMain $(FPGATOP) --compiler sverilog       \
		--target-dir $(@D) --full-stacktrace --output-file $(@F)    \
		--disable-all --remove-assert                               \
		--infer-rw --repl-seq-mem -c:$(FPGATOP):-o:$(@D)/$(@F).conf \
		--gen-mem-verilog full $(SIM_ARGS)
	@sed -i -e 's/_\(aw\|ar\|w\|r\|b\)_\(\|bits_\)/_\1/g' $@
	@git log -n 1 >> .__head__
	@git diff >> .__diff__
	@sed -i 's/^/\/\// ' .__head__
	@sed -i 's/^/\/\//' .__diff__
	@cat .__head__ .__diff__ $@ > .__out__
	@mv .__out__ $@
	@rm .__head__ .__diff__

verilog: $(TOP_V)

SIM_TOP   = SimTop
SIM_TOP_V = $(RTL_DIR)/$(SIM_TOP).sv
$(SIM_TOP_V): $(SCALA_FILE) $(TEST_FILE)
	mkdir -p $(@D)
	@date -R
	mill -i XiangShan.test.runMain $(SIMTOP) --compiler sverilog       \
		--target-dir $(@D) --full-stacktrace --output-file $(@F)   \
		--infer-rw --repl-seq-mem -c:$(SIMTOP):-o:$(@D)/$(@F).conf \
		--disable-log --gen-mem-verilog full $(SIM_ARGS)
	@git log -n 1 >> .__head__
	@git diff >> .__diff__
	@sed -i 's/^/\/\// ' .__head__
	@sed -i 's/^/\/\//' .__diff__
	@cat .__head__ .__diff__ $@ > .__out__
	@mv .__out__ $@
	@rm .__head__ .__diff__
	@sed -i -e 's/$$fatal/xs_assert_v2(`__FILE__, `__LINE__)/g' $(SIM_TOP_V)
	@date -R

sim-verilog: $(SIM_TOP_V)

emu: sim-verilog
	$(MAKE) -C ./difftest emu WITH_CHISELDB=0 WITH_CONSTANTIN=0

clean:
	rm -rf ./build

init:
	git submodule update --init

bsp:
	mill -i mill.bsp.BSP/install

idea:
	mill -i mill.idea.GenIdea/idea

.PHONY: verilog emu clean help init bump bsp $(REF_SO)
