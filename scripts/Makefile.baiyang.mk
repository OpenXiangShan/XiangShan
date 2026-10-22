# Baiyang DDR memory-controller backend.

BAIYANG_HOME ?= $(NOOP_HOME)/YuQuan
BAIYANG_BUILD_DIR ?= $(BAIYANG_HOME)/build
BAIYANG_TOP ?= $(BAIYANG_BUILD_DIR)/BY_mc_top.sv
BAIYANG_EMU_DEPS :=
BAIYANG_DIFTEST_ARGS :=

ifeq ($(WITH_BAIYANG),1)
override SIM_ARGS += --with-baiyang
BAIYANG_EMU_DEPS += baiyang-verilog
BAIYANG_DIFTEST_ARGS += WITH_BAIYANG=1 BAIYANG_HOME=$(BAIYANG_HOME)
endif

baiyang-init:
ifeq ($(wildcard $(BAIYANG_HOME)/Makefile),)
	git submodule update --init $(GIT_FORCE_FLAG) YuQuan
else
	@echo "Using existing Baiyang sources at $(BAIYANG_HOME)"
endif

baiyang-verilog: baiyang-init
	$(MAKE) -C $(BAIYANG_HOME) clean
	$(MAKE) -C $(BAIYANG_HOME) verilog
	cd $(BAIYANG_HOME) && python3 $(NOOP_HOME)/scripts/rename_baiyang.py
	test -f $(BAIYANG_TOP)

.PHONY: baiyang-init baiyang-verilog
