#***************************************************************************************
# Copyright (c) 2025 Beijing Institute of Open Source Chip (BOSC)
# Copyright (c) 2025 Institute of Computing Technology, Chinese Academy of Sciences
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

#coding=utf-8

from pyxscore import DUTSimTop, xsp
from pydifftest import difftest as df
from xspdb import *

def run_sim_top():
    dut = DUTSimTop()
    XSPdb(dut, df, xsp).set_trace()
    while True:
        dut.Step(1000)

if __name__ == "__main__":
    from bdb import BdbQuit
    try:
        run_sim_top()
    except BdbQuit:
        pass
