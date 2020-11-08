#!/bin/bash

if [ -z "${CORE_V_VERIF}" ]; then
    echo "CORE_V_VERIF is unset- exiting"
    exit 1
fi

TEST=$1
WORK=$2/OpenHW-REF

TESTDIR=$(dirname ${TEST})/sim

# create an empty signature file at start
touch ${TEST}.signature.output

echo "Running: $TEST"

make -C ${CORE_V_VERIF}/cv32/sim/uvmt_cv32 \
    SIMULATOR=xrun RISCV=${RISCV} TARGET=XCELIUM \
    riscv-compliance COMPLIANCE=${TEST} \
    XRUN_RESULTS=${WORK} USE_ISS=YES | tee ${TEST}.log

exit 0