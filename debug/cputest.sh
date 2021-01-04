#!/bin/bash

TEST_HOME=$AM_HOME/tests/cputest

for test in $(ls $TEST_HOME/tests)
do
    t=${test%.c}
    echo -n -e "\x1b[0m $t: "
    make -C $TEST_HOME ARCH=riscv64-noop E=0 ALL=$t run 2>/dev/null | grep -E "HIT GOOD TRAP|IPC"
    if [[ $? != 0 ]];
    then
        echo -e "\x1b[31mfail: trap code $?"
    fi
done
