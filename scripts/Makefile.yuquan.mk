# YuQuan DDR memory-controller integration.

YUQUAN_HOME ?= $(NOOP_HOME)/YuQuan
YUQUAN_ADAPTER_HOME ?= $(NOOP_HOME)/env-scripts/simulation/yuquan
YUQUAN_RTL_DIR ?= $(NOOP_HOME)/scripts/yuquan/rtl
YUQUAN_BUILD_DIR ?= $(YUQUAN_HOME)/build
YUQUAN_TOP ?= $(YUQUAN_BUILD_DIR)/BY_mc_top.sv
YUQUAN_EMU_DEPS :=
YUQUAN_DIFTEST_ARGS :=

ifeq ($(WITH_YUQUAN),1)
ifeq ($(wildcard $(YUQUAN_HOME)/Makefile),)
$(error YUQUAN_HOME is invalid: $(YUQUAN_HOME))
endif
override SIM_ARGS += --with-yuquan
YUQUAN_EMU_DEPS += yuquan-verilog
YUQUAN_DIFTEST_ARGS += WITH_EXTERNAL_MEMORY_BACKEND=1
YUQUAN_DIFTEST_ARGS += EXTERNAL_MEMORY_BACKEND_MK=$(YUQUAN_ADAPTER_HOME)/Makefile.difftest.mk
YUQUAN_DIFTEST_ARGS += YUQUAN_HOME=$(YUQUAN_HOME) YUQUAN_ADAPTER_HOME=$(YUQUAN_ADAPTER_HOME) YUQUAN_RTL_DIR=$(YUQUAN_RTL_DIR)
endif

yuquan-verilog:
	$(MAKE) -C $(YUQUAN_HOME) clean
	$(MAKE) -C $(YUQUAN_HOME) verilog
	cd $(YUQUAN_HOME) && python3 $(NOOP_HOME)/scripts/rename_yuquan.py
	test -f $(YUQUAN_TOP)

.PHONY: yuquan-verilog
