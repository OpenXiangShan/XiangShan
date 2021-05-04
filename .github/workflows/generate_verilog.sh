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

echo "Checking BoringUtils usages ..."
( set -x; bash ${SCRIPT_DIR}/check-usage.sh "BoringUtils" $NOOP_HOME )

echo "Generating dual-core verilog ..."
( set -x; make -C $NOOP_HOME verilog SIM_ARGS=--dual-core )

