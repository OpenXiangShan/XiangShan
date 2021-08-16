#!/bin/bash

#***************************************************************************************
# Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
# Copyright (c) 2020-2021 Peng Cheng Laboratory
#
# XiangShan is licensed under Mulan PSL v2.
# You can use this software according to the terms and conditions of the Mulan PSL v2.
# You may obtain a copy of Mulan PSL v2 at:
#          http://license.coscl.org.cn/MulanPSL2
#
# THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
# EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
# MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
#
# See the Mulan PSL v2 for more details.
#***************************************************************************************

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
