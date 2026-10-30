# MCSim timing backend.

MCSIM_HOME ?= $(NOOP_HOME)/MCSim
MCSIM_CPU_FREQ_MHZ ?= 3000
MCSIM_CXX ?= clang++
MCSIM_EMU_DEPS :=
MCSIM_DIFTEST_ARGS :=

ifeq ($(WITH_MCSIM),1)
ifeq ($(wildcard $(MCSIM_HOME)/Makefile),)
$(error MCSIM_HOME is invalid: $(MCSIM_HOME))
endif
override SIM_ARGS += --with-mcsim
MCSIM_EMU_DEPS += mcsim-verilator-lib
MCSIM_DIFTEST_ARGS += WITH_MCSIM=1 MCSIM_HOME=$(MCSIM_HOME) MCSIM_CPU_FREQ_MHZ=$(MCSIM_CPU_FREQ_MHZ)
endif

mcsim-verilator-lib:
	$(MAKE) -C $(MCSIM_HOME) verilator-lib CXX=$(MCSIM_CXX)

.PHONY: mcsim-verilator-lib
