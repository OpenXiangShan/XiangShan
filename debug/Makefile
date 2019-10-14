ARCH = ARCH=riscv64-noop
NANOS_HOME ?= $(AM_HOME)/../nanos-lite
SINGLETEST = ALL=min3

cpu:
	$(MAKE) -C $(AM_HOME)/tests/cputest $(ARCH) ALL=dummy run

cputest:
	$(MAKE) -C $(AM_HOME)/tests/cputest $(ARCH) run 2>&1 | tee > cpu.log
	cat cpu.log | grep different

# bputest:
# 	$(MAKE) -C $(AM_HOME)/tests/bputest $(ARCH) run 2>&1 | tee > bpu.log
# 	cat bpu.log | grep different
bputest:
	$(MAKE) -C $(AM_HOME)/tests/bputest $(ARCH) ALL=forcall2 run 2>&1 | tee > bpu.log
	cat bpu.log | grep Mbp

test:
	$(MAKE) -C $(AM_HOME)/tests/cputest $(ARCH) $(SINGLETEST) run 2>&1 | tee > test.log
	cat test.log | grep different
	cat test.log | grep ISU > isu.log

microbench:
	$(MAKE) -C $(AM_HOME)/apps/microbench $(ARCH) mainargs=test run 2>&1 | tee > microbench.log
	cat microbench.log | grep IPC

microbencht:
	$(MAKE) -C $(AM_HOME)/apps/microbench $(ARCH) mainargs=train run 2>&1 | tee > microbench.log
	cat microbench.log | grep IPC

coremark:
	$(MAKE) -C $(AM_HOME)/apps/coremark $(ARCH) mainargs=test run 2>&1 | tee > coremark.log
	cat coremark.log | grep IPC

dhrystone:
	$(MAKE) -C $(AM_HOME)/apps/dhrystone $(ARCH) mainargs=test run 2>&1 | tee > dhrystone.log
	cat dhrystone.log | grep IPC

xj:
	$(MAKE) -C $(NANOS_HOME) $(ARCH) run

# xjnemu:
# 	$(MAKE) -C $(NANOS_HOME) ARCH=riscv64-nemu run
