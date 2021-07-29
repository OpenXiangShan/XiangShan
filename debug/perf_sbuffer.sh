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


echo -n "store cnt: "
grep "accept req" $1 | wc -l

echo -n "dcache req cnt: "
grep "send buf" $1 | wc -l

echo -n "req[0] blocked: "
grep "\[0\] blocked by sbuffer" $1 | wc -l

echo -n "req[1] blocked: "
grep "\[1\] blocked by sbuffer" $1 | wc -l

echo -n "sbuffer cnt = 15: "
grep "sbuffer entry cnt: 15" $1 | wc -l

echo -n "sbuffer cnt = 16: "
grep "sbuffer entry cnt: 16" $1 | wc -l
