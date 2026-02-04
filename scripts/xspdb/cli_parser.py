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
import os

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
    def parse_mem_size(size_str):
        if not size_str or size_str.strip() == "":
            return None
        size_str = size_str.strip().upper()
        if size_str.endswith("GB"):
            return int(size_str[:-2]) * 1024 * 1024 * 1024
        elif size_str.endswith("MB"):
            return int(size_str[:-2]) * 1024 * 1024
        elif size_str.endswith("KB"):
            return int(size_str[:-2]) * 1024
        else:
            raise ValueError(f"Invalid memory size format: {size_str}")
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
                        help="log file name (default: ./XSPdb.log)")
    parser.add_argument('--log-level', type=str, default="", choices=["debug", "info", "warn", "erro"],
                        help="set log level")
    parser.add_argument('--debug-level', type=str, default="", choices=["debug", "info", "warn", "erro"],
                        help="set debug level")
    parser.add_argument('--batch', default=False,
                        help='enable batch mode')
    parser.add_argument('-c', '--max-cycles', type=int, default=0xFFFFFFFFFFFFFFFF,
                        help="maximum simulation cycles to execute")
    parser.add_argument('-t', '--interact-at', type=int, default=-1,
                        help="enter interactive mode at the specified cycle")
    parser.add_argument('-s', '--script', type=str, default="",
                        help="script file to execute")
    parser.add_argument("-bi", "--batch-interval", type=float, default=0.1,
                        help="interval time (seconds) between batch commands")
    parser.add_argument("-r", "--replay", type=str, default="",
                        help="replay log file")
    parser.add_argument("-b", "--wave-begin", type=int, default=-2,
                        help="start waveform dump at the specified cycle")
    parser.add_argument("-e", "--wave-end", type=int, default=-2,
                        help="stop waveform dump at the specified cycle")
    parser.add_argument("--wave-path", type=str, default="",
                        help="output path for waveform file")
    parser.add_argument("--max-run-time", type=timesec, default=0,
                        help="maximum run time (eg 10s, 1m, 1h)")
    parser.add_argument("-pc", "--pc-commits", type=int, default=0,
                        help="run until the specified number of commits; -1 means no limit")
    parser.add_argument("--sim-args", type=lambda s: s.split(','), default=[],
                        help="additional simulator arguments (comma-separated)")
    parser.add_argument("-F", "--flash", type=str, default="",
                        help="flash binary file for simulation")
    parser.add_argument("--no-interact", action="store_true", default=False,
                        help="disable interactive mode (do not handle the ctrl-c signal)")
    parser.add_argument("--ram-size", type=parse_mem_size, default="",
                        help="simulation RAM size (e.g., 8GB or 128MB)")
    parser.add_argument("--diff", type=str, default="",
                        help="path to REF shared object for difftest testing")
    parser.add_argument("--cmds", type=str, default="",
                        help="XSPdb commands to execute before run (\\n for newline)")
    parser.add_argument("--cmds-post", type=str, default="",
                        help="XSPdb commands to execute after script/replay (\\n for newline)")
    parser.add_argument("--flash-base-address", type=address, default=0x10000000,
                        help="base address of flash")
    parser.add_argument("--diff-first-inst-address", type=address, default=-1,
                        help="first instruction address for difftest")
    parser.add_argument("--trace-pc-symbol-block-change", action="store_true", default=False,
                        help="enable tracing of PC symbol block changes")
    return parser.parse_args()
