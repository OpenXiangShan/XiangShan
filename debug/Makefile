ARCH = ARCH=riscv64-noop
NANOS_HOME ?= $(AM_HOME)/../nanos-lite
SINGLETEST = ALL=recursion

microbench:
	$(MAKE) -C $(AM_HOME)/apps/microbench $(ARCH) mainargs=test run 2>&1 | tee > microbench.log
	cat microbench.log | grep IPC

cputest:
	$(MAKE) -C $(AM_HOME)/tests/cputest $(ARCH) run 2>&1 | tee > cpu.log
	cat cpu.log | grep different

test:
	$(MAKE) -C $(AM_HOME)/tests/cputest $(ARCH) $(SINGLETEST) run 2>&1 | tee > test.log
	cat test.log | grep different
	cat test.log | grep ISU > isu.log

cpu:
	$(MAKE) -C $(AM_HOME)/tests/cputest $(ARCH) ALL=dummy run

xj:
	$(MAKE) -C $(NANOS_HOME) $(ARCH) run

# xjnemu:
# 	$(MAKE) -C $(NANOS_HOME) ARCH=riscv64-nemu run
