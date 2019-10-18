ARCH = ARCH=riscv64-noop
NANOS_HOME ?= $(AM_HOME)/../nanos-lite
SINGLETEST = ALL=min3

# ------------------------------------------------------------------
# bulid CPU and run dummy test
# ------------------------------------------------------------------

cpu:
	$(MAKE) -C $(AM_HOME)/tests/cputest $(ARCH) ALL=dummy run

# ------------------------------------------------------------------
# run different test sets
# ------------------------------------------------------------------

cputest:
	$(MAKE) -C $(AM_HOME)/tests/cputest $(ARCH) run 2>&1 | tee > cpu.log
	cat cpu.log | grep different

# bputest:
# 	$(MAKE) -C $(AM_HOME)/tests/bputest $(ARCH) run 2>&1 | tee > bpu.log
# 	cat bpu.log | grep different
bputest:
	$(MAKE) -C $(AM_HOME)/tests/bputest $(ARCH) ALL=forcall2 run 2>&1 | tee > bpu.log
	cat bpu.log | grep Mbp

unittest:
	$(MAKE) -C $(AM_HOME)/tests/cputest $(ARCH) $(SINGLETEST) run 2>&1 | tee > test.log
	cat test.log | grep different
	cat test.log | grep ISU > isu.log

microbench:
	$(MAKE) -C $(AM_HOME)/apps/microbench $(ARCH) mainargs=test run 2>&1 | tee > microbench.log
	cat microbench.log | grep IPC

microbench_train:
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

rttos:
	$(MAKE) -C $(RTTOS_HOME)/bsp/riscv64-noop run

freertos:
	$(MAKE) -C $(FREERTOS_HOME)/Demo/riscv64-nemu noop_run

# ------------------------------------------------------------------
# get disassembled test src
# ------------------------------------------------------------------

disassemble-rttos:
	cp $(RTTOS_HOME)/bsp/riscv64-noop/build/code.txt ./d-rttos.log

disassemble-freertos:
	cp $(FREERTOS_HOME)/Demo/riscv64-nemu/build/FreeRTOS-simple.elf.txt ./d-freertos.log

