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
( set -x; make -C $NOOP_HOME ./build/emu SIM_ARGS=--disable-log EMU_THREADS=16 WITH_DRAMSIM3=1 -j200 )

NUMA_PREFIX="numactl -m 1 -C 64-79"

echo "Running mcf ..."
( set -x; ${NUMA_PREFIX} make emu IMAGE=/home/ci-runner/xsenv/workloads/mcf/_2550001000_.gz EMU_ARGS="-I 5000000" 2> perf.log )
cat perf.log | sort | tee $PERF_HOME/mcf.log

echo "Running xalancbmk ..."
( set -x; ${NUMA_PREFIX} make emu IMAGE=/home/ci-runner/xsenv/workloads/xalancbmk/_6600001000_.gz EMU_ARGS="-I 5000000" 2> perf.log )
cat perf.log | sort | tee $PERF_HOME/xalancbmk.log

echo "Running gcc ..."
( set -x; ${NUMA_PREFIX} make emu IMAGE=/home/ci-runner/xsenv/workloads/gcc/_1250001000_.gz EMU_ARGS="-I 5000000" 2> perf.log )
cat perf.log | sort | tee $PERF_HOME/gcc.log

echo "Running namd ..."
( set -x; ${NUMA_PREFIX} make emu IMAGE=/home/ci-runner/xsenv/workloads/namd/_4850001000_.gz EMU_ARGS="-I 5000000" 2> perf.log )
cat perf.log | sort | tee $PERF_HOME/namd.log

echo "Running lbm ..."
( set -x; ${NUMA_PREFIX} make emu IMAGE=/home/ci-runner/xsenv/workloads/lbm/_7550001000_.gz EMU_ARGS="-I 5000000" 2> perf.log )
cat perf.log | sort | tee $PERF_HOME/lbm.log

echo "Running gromacs ..."
( set -x; ${NUMA_PREFIX} make emu IMAGE=/home/ci-runner/xsenv/workloads/gromacs/_3150001000_.gz EMU_ARGS="-I 5000000" 2> perf.log )
cat perf.log | sort | tee $PERF_HOME/gromacs.log

