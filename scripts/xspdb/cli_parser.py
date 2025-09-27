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

import argparse

def args_parser():
    """create parser """

    address = lambda s:int(s, 0)
    def timesec(s):
        s = s.strip().lower()
        if s.endswith("s"):
            return int(s[:-1])
        elif s.endswith("m"):
            return int(s[:-1]) * 60
        elif s.endswith("h"):
            return int(s[:-1]) * 3600
        else:
            raise ValueError(f"Invalid time format: {s}")
    parser = argparse.ArgumentParser(description='XSPdb 运行脚本')
    parser.add_argument('-i', '--image', type=str, default="",
                        help='image file to load and run') 
    parser.add_argument('-v', '--version', action='version',
                        version='version ')
    parser.add_argument('--mem-base-address', type=address, default=0x80000000,
                        help="base address of memory")
    parser.add_argument('-l', "--log", action="store_true", default=False, 
                        help="enable logging output")
    parser.add_argument("--log-file", type=str, default="",
                        help="log file name (default: ./XSPdb.log")
    parser.add_argument('--log-level', type=str, default="", choices=["debug", "info", "warn", "error"],
                        help="set log level")
    parser.add_argument('--debug-level', type=str, default="", choices=["debug", "info", "warn", "error"],
                        help="set debug level")
    parser.add_argument('--batch', default=False,
                        help='enable batch mode')
    parser.add_argument('-c', '--max-cycles', type=int, default=0xFFFFFFFFFFFFFFFF,
                        help="maximum simulation cycles to execute")
    parser.add_argument('-t', '--interact-at', type=int, default=-1,
                        help="enter interactive mode at the specified cycle")
    parser.add_argument('-s', '--script', type=str, default="",
                        help="script file to execute")
    parser.add_argument("--max-run-time", type=timesec, default=0, 
                        help="maximum run time (eg 10s, 1m, 1h)")
    return parser.parse_args()
