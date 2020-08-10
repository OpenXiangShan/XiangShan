#!/bin/bash

TEST_HOME=$AM_HOME/tests/cputest

for t in ${$(ls $TEST_HOME/tests)%.c}
do
    echo -n "\x1b[0m $t: "
    make -C $TEST_HOME ARCH=riscv64-noop E=0 ALL=$t run 2>/dev/null | grep "HIT GOOD TRAP" 
    if [[ $? == 1 ]];
    then
        echo "\x1b[31mfail"
    fi
done
