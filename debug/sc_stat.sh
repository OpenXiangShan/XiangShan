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

log_dir=$1
tage_w_sc_w=$(grep "scUpdate" $log_dir | grep "sc(1), tage(1)" -c)
tage_w_sc_r=$(grep "scUpdate" $log_dir | grep "sc(0), tage(1)" -c)
tage_r_sc_w=$(grep "scUpdate" $log_dir | grep "sc(1), tage(0)" -c)
tage_r_sc_r=$(grep "scUpdate" $log_dir | grep "sc(0), tage(0)" -c)

echo $tage_r_sc_w tage right but mispredicted by sc
echo $tage_w_sc_r tage wrong and rectified by sc
echo `expr $tage_w_sc_w + $tage_r_sc_r` branches remain unchanged, in which $tage_w_sc_w are wrong

