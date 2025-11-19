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
BUILD_DIR = $(abspath ./build)
RTL_DIR = $(BUILD_DIR)/rtl

MILL_ARGS = $(SIM_ARGS)
MILL_ARGS += --target-dir $(RTL_DIR) --full-stacktrace --dump-fir

NUM_CORES ?= 1
ifeq ($(NUM_CORES),2)
MILL_ARGS += --dual-core
endif

CHISEL_TARGET ?= systemverilog
MILL_ARGS += --target $(CHISEL_TARGET)
MILL_ARGS += --firtool-opt "-O=release --disable-annotation-unknown"
ifeq ($(CHISEL_TARGET),systemverilog)
MILL_ARGS += --split-verilog
endif

ifneq ($(FIRTOOL),)
MILL_ARGS += --firtool-binary-path $(abspath $(FIRTOOL))
endif

# Coverage support
ifneq ($(FIRRTL_COVER),)
comma := ,
splitcomma = $(foreach w,$(subst $(comma), ,$1),$(if $(strip $w),$w))
MILL_ARGS += $(foreach c,$(call splitcomma,$(FIRRTL_COVER)),--extract-$(c)-cover)
endif

ifeq ($(GSIM),1)
MILL_ARGS += --dump-fir --difftest-config G
endif

# co-simulation with DRAMsim3
ifeq ($(WITH_DRAMSIM3),1)
ifndef DRAMSIM3_HOME
$(error DRAMSIM3_HOME is not set)
endif
MILL_ARGS += --with-dramsim3
endif

TIMELOG = $(BUILD_DIR)/time.log
TIME_CMD = time -a -o $(TIMELOG)

.DEFAULT_GOAL = verilog

help:
	mill -i xiangshan.test.runMain top.XiangShanSim --help

TOP_V = $(RTL_DIR)/XSTop.sv
$(TOP_V): $(SCALA_FILE)
	@mkdir -p $(@D)
	$(TIME_CMD) mill -i xiangshan.runMain top.TopMain \
		--disable-all $(MILL_ARGS)
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
	@mkdir -p $(@D)
	@echo "\n[mill] Generating Verilog files..." > $(TIMELOG)
	@date -R | tee -a $(TIMELOG)
	$(TIME_CMD) mill -i xiangshan.test.runMain top.TestMain \
		--disable-log $(MILL_ARGS)
	@git log -n 1 >> .__head__
	@git diff >> .__diff__
	@sed -i 's/^/\/\// ' .__head__
	@sed -i 's/^/\/\//' .__diff__
	@cat .__head__ .__diff__ $@ > .__out__
	@mv .__out__ $@
	@rm .__head__ .__diff__
	@sed -i -e 's/$$fatal/xs_assert_v2(`__FILE__, `__LINE__)/g' $(RTL_DIR)/*.sv
	@sed -i -e "s/\$$error(/\$$fwrite(32\'h80000002, /g" $(RTL_DIR)/*.sv

sim-verilog: $(SIM_TOP_V)

emu: sim-verilog
	$(MAKE) -C ./difftest emu WITH_CHISELDB=0 WITH_CONSTANTIN=0

clean:
	rm -rf ./build

init:
	git submodule update --init

bsp:
	mill -i mill.bsp.BSP/install

.PHONY: verilog emu clean help init bump bsp $(REF_SO)
