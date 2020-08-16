#!/bin/bash

TEST_HOME=$AM_HOME/tests/cputest

for test in $(ls $TEST_HOME/tests)
do
<<<<<<< HEAD
    echo -n "\x1b[0m $t: "
    make -C $TEST_HOME ARCH=riscv64-noop ALL=$t run 2>&1 | tee > $TEST_HOME/build/$t.log | grep "HIT GOOD TRAP" 
=======
    t=${test%.c}
    echo -n -e "\x1b[0m $t: "
    make -C $TEST_HOME ARCH=riscv64-noop E=0 ALL=$t run 2>/dev/null | grep "HIT GOOD TRAP"
>>>>>>> fix-brq-perf
    if [[ $? == 1 ]];
    then
        echo -e "\x1b[31mfail"
    fi
done
