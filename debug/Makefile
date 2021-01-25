ARCH = ARCH=riscv64-noop
NANOS_HOME ?= $(AM_HOME)/../nanos-lite
SINGLETEST = ALL=min3

B ?= 0
E ?= -1
V ?= OFF
#V ?= OFF
EMU_ARGS = B=$(B) E=$(E) V=$(V)

# ------------------------------------------------------------------
# bulid CPU and run dummy test
# ------------------------------------------------------------------

cache:
	$(MAKE) -C $(AM_HOME)/tests/cachetest $(ARCH) ALL=loader $(EMU_ARGS) run 
	#2>&1 | tee > loader.log
	#2>&1 | tee > loader.log

cpu:
	$(MAKE) -C $(AM_HOME)/tests/cputest $(ARCH) ALL=dummy $(EMU_ARGS) run 2>&1 | tee > dummy.log

# ------------------------------------------------------------------
# run different test sets
# ------------------------------------------------------------------

cputest:
	bash cputest.sh

# bputest:
# 	$(MAKE) -C $(AM_HOME)/tests/bputest $(ARCH) run 2>&1 | tee > bpu.log
# 	cat bpu.log | grep different
bputest:
	$(MAKE) -C $(AM_HOME)/tests/bputest $(ARCH) run 2 > bpu.log
	cat bpu.log | grep Mbp

amtest:
	$(MAKE) -C $(AM_HOME)/tests/cputest $(ARCH) $(SINGLETEST) run 2 > test.log
	cat test.log | grep different
	cat test.log | grep ISU > isu.log

microbench:
	$(MAKE) -C $(AM_HOME)/apps/microbench $(ARCH) $(EMU_ARGS) mainargs=test run 
	#2>&1 | tee > microbench.log
	#2 > microbench.log
	cat microbench.log | grep IPC

microbench_train:
	$(MAKE) -C $(AM_HOME)/apps/microbench $(ARCH) $(EMU_ARGS) mainargs=train run 2 > microbench.log
	cat microbench.log | grep IPC

coremark:
	$(MAKE) -C $(AM_HOME)/apps/coremark $(ARCH) $(EMU_ARGS) mainargs=test run
	#2 > coremark.log
	cat coremark.log | grep IPC

dhrystone:
	$(MAKE) -C $(AM_HOME)/apps/dhrystone $(ARCH) $(EMU_ARGS) mainargs=test run 2 > dhrystone.log
	cat dhrystone.log | grep IPC

xj:
	$(MAKE) -C $(NANOS_HOME) $(ARCH) $(EMU_ARGS) run

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
	$(MAKE) -C $(BBL_LINUX_HOME) $(EMU_ARGS) noop
# ------------------------------------------------------------------
# get disassembled test src
# ------------------------------------------------------------------

disassemble-rttos:
	cp $(RTTOS_HOME)/bsp/riscv64-noop/build/code.txt ./d-rttos.log

disassemble-freertos:
	cp $(FREERTOS_HOME)/Demo/riscv64-noop/build/FreeRTOS-simple.elf.txt ./d-freertos.log

disassemble-xv6:
	cp $(XV6_HOME)/build/code.txt ./d-xv6.log


SUITE = cache.L2CacheTest

unit-test:
	cd .. && mill XiangShan.test.testOnly -o -s $(SUITE)

tlc-test:
	cd .. && mill XiangShan.test.testOnly -o -s cache.TLCTest.TLCCacheTest

l1-test:
	cd .. && mill XiangShan.test.testOnly -o -s cache.L1DTest.L1DCacheTest

unit-test-all:
	cd .. && mill XiangShan.test.test -P$(P)

# ------------------------------------------------------------------
# chore
# ------------------------------------------------------------------

clean:
	$(MAKE) -C .. clean
