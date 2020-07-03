ARCH = ARCH=riscv64-noop
NANOS_HOME ?= $(AM_HOME)/../nanos-lite
SINGLETEST = ALL=min3

B ?= 0
E ?= -1
V ?= ALL
EMU_ARGS = B=$(B) E=$(E) V=$(V)

# ------------------------------------------------------------------
# bulid CPU and run dummy test
# ------------------------------------------------------------------

cpu:
	$(MAKE) -C $(AM_HOME)/tests/cputest $(ARCH) ALL=dummy $(EMU_ARGS) run

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

amtest:
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

xjnemu:
	$(MAKE) -C $(NANOS_HOME) ARCH=riscv64-nemu run

rttos:
	$(MAKE) -C $(RTTOS_HOME)/bsp/riscv64-noop run

rttos-debug:
	$(MAKE) -C $(RTTOS_HOME)/bsp/riscv64-noop run 2>&1 | tee > rttos.log

freertos:
	$(MAKE) -C $(FREERTOS_HOME)/Demo/riscv64-noop noop_run

xv6:
	$(MAKE) -C $(XV6_HOME) noop

xv6-debug:
	$(MAKE) -C $(XV6_HOME) noop 2>&1 | tee > xv6.log

linux:
	$(MAKE) -C $(BBL_LINUX_HOME) noop
# ------------------------------------------------------------------
# get disassembled test src
# ------------------------------------------------------------------

disassemble-rttos:
	cp $(RTTOS_HOME)/bsp/riscv64-noop/build/code.txt ./d-rttos.log

disassemble-freertos:
	cp $(FREERTOS_HOME)/Demo/riscv64-noop/build/FreeRTOS-simple.elf.txt ./d-freertos.log

disassemble-xv6:
	cp $(XV6_HOME)/build/code.txt ./d-xv6.log


P = 4
SUITE = xiangshan.backend.exu.AluTest

unit-test:
	cd .. && mill chiselModule.test.testOnly -o -s $(SUITE) -P$(P)

unit-test-all:
	cd .. && mill chiselModule.test.test -P$(P)