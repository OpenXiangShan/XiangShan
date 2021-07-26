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

import os
import argparse

def printMap(mp):
    len_key = max(map(lambda s: len(s), mp.keys()))
    len_value = max(map(lambda v: len(str(v)), mp.values()))
    pattern = "{:<" +str(len_key) + "} {:<" +str(len_value)+ "} {:<7}%"
    total = sum(mp.values())
    for k,v in sorted(mp.items(), key=lambda x:x[1], reverse=True):
        print(
            pattern.format(k, v, round(v*100.0/total, 3))
        )


def analyzeVerilog(filename):
    mymap = {}
    last = ""
    with open(filename, "r") as f:
        line = f.readline()
        cnt = 0
        while(line):
            if "module " in line:
                if last!="" :
                    mymap[last] = cnt
                last = line[7:-2]
                cnt = 1
            else:
                cnt = cnt + 1
            line = f.readline()
        mymap[last] = cnt
        printMap(mymap)

logLevels = ['ALL', 'DEBUG', 'INFO', 'WARN', 'ERROR']

def listToStr(lst):
    acc = ''
    for l in lst:
        acc += '|' + str(l) if acc else str(l)
    return acc

def lineStrip(line):
    return line.replace('\n', '')

def getNumLogLines(filename, modules, ll=logLevels):
    cmd = "grep -E '\[({0}).*\]\[time=.*\] ({1}):' {2} | wc -l".format(
        listToStr(ll),
        listToStr(modules),
        filename
    )
    res = os.popen(cmd)
    return int(lineStrip(res.readline()), 10)

def analyzeLog(filename):
    cmd = "grep -E '\[time=.*\]' {0} ".format(filename) + " | awk -F '(:)' {'print $1'} | awk  {'print $NF'} | sort | uniq"
    res = os.popen(cmd)
    modules = list(map(lineStrip, res.readlines()))
    mymap = {}
    for m in modules:
        mymap[m] = getNumLogLines(filename, [m])
    printMap(mymap)

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-v", "--verilogFile", help="verilog file path", type=str)
    parser.add_argument("-l", "--logFile", help="log file path", type=str)
    args = parser.parse_args()

    if args.verilogFile:
        analyzeVerilog(args.verilogFile)

    if args.logFile:
        analyzeLog(args.logFile)

    if not args.verilogFile and not args.logFile:
        parser.print_help()

if __name__ == '__main__':
    main()

