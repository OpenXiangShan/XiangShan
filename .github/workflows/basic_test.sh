#!/bin/bash
set -e
set -o pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT_PATH=$SCRIPT_DIR/$(basename "${BASH_SOURCE[0]}")
export NOOP_HOME=$(realpath $SCRIPT_DIR/../../)

source ${SCRIPT_DIR}/setup.sh
echo "NOOP_HOME:    " $NOOP_HOME
echo "NEMU_HOME:    " $NEMU_HOME
echo "AM_HOME:      " $AM_HOME
echo "RVTEST_HOME:  " $RVTEST_HOME
echo "DRAMSIM3_HOME:" $DRAMSIM3_HOME
echo "PERF_HOME:    " $PERF_HOME

mkdir -p $PERF_HOME

echo "Building EMU ..."
# ( set -x; make -C $NOOP_HOME ./build/emu SIM_ARGS=--disable-log EMU_THREADS=8 -j200 )

NUMA_PREFIX="numactl -m 1 -C 64-71"

echo "Running cputest ..."
for test in $(ls $AM_HOME/tests/cputest/tests)
do
  ( set -x; ${NUMA_PREFIX} make -C $AM_HOME/tests/cputest ALL=$test ARCH=riscv64-noop run )
done

echo "Running riscv-tests ..."
( set -x; ${NUMA_PREFIX} make -C $RVTEST_HOME/isa SUITES+=rv64ui SUITES+=rv64um SUITES+=rv64ua SUITES+=rv64uf SUITES+=rv64ud )

echo "Running microbench ..."
( set -x; ${NUMA_PREFIX} make -C $AM_HOME/apps/microbench ARCH=riscv64-noop mainargs=test run 2> perf.log )
cat perf.log | sort | tee $PERF_HOME/microbench.log

echo "Running CoreMark ..."
( set -x; ${NUMA_PREFIX} make -C $AM_HOME/apps/coremark ARCH=riscv64-noop run 2> perf.log )
cat perf.log | sort | tee $PERF_HOME/coremark.log

 echo "Running floating-point test ..."
( set -x; ${NUMA_PREFIX} make emu IMAGE=/home/ci-runner/xsenv/workloads/povray/_3400001000_.gz EMU_ARGS="-I 5000000" )

echo "Running Linux hello-world ..."
( set -x; ${NUMA_PREFIX} make emu IMAGE=/home/ci-runner/xsenv/workloads/linux-hello/bbl.bin 2> perf.log )
cat perf.log | sort | tee $PERF_HOME/linux.log

