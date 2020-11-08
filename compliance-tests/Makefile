export RISCV_ISA = rv64i
export ROOTDIR = $(shell pwd)

SRC := $(ROOTDIR)/riscv-test-env

all:verify
	echo -e '$(RISCV_ISA) $(ROOTDIR)'

verify:
	$(SRC)/verify.sh
