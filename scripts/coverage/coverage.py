#/usr/bin/python3
# -*- coding: UTF-8 -*-

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

import sys
import re
import copy

if __name__ == "__main__":
    assert len(sys.argv) == 3, "Expect input_file and output_file"
    input_file = sys.argv[1]
    output_file = sys.argv[2]
    lines = []
    line_count = 0
    synthesis_nest_level = 0
    reg_init_nest_level = 0
    mem_init_nest_level = 0
    with open(input_file) as f:
        for line in f:
            line_count += 1

            ifdef = re.compile('`ifdef')
            ifndef = re.compile('`ifndef')
            endif = re.compile('`endif')
            # remove the line coverage results of not synthesizable code(mostly assert and fwrite)
            synthesis = re.compile('`ifndef SYNTHESIS')
            # remove the coverage results of random init variables
            reg_init = re.compile('`ifdef RANDOMIZE_REG_INIT')
            mem_init = re.compile('`ifdef RANDOMIZE_MEM_INIT')
            coverage = re.compile('^\s*(%?\d+)\s+')


            ifdef_match = ifdef.search(line)
            ifndef_match = ifndef.search(line)
            endif_match = endif.search(line)
            synthesis_match = synthesis.search(line)
            reg_init_match = reg_init.search(line)
            mem_init_match = mem_init.search(line)
            coverage_match = coverage.search(line)

            # enter synthesis block
            if synthesis_match:
                assert synthesis_nest_level == 0, "Should not nest SYNTHESIS macro"
                synthesis_nest_level = 1

            if synthesis_nest_level > 0:
                if ifdef_match or (ifndef_match and not synthesis_match):
                    synthesis_nest_level += 1
                if endif_match:
                    synthesis_nest_level -= 1
                    assert synthesis_nest_level >= 0, "Macro nest level should be >= 0"

                # remove line coverage results in systhesis block
                if coverage_match:
                    coverage_stat = coverage_match.group(1)
                    line = line.replace(coverage_match.group(1), " " * len(coverage_stat))

            # enter reg_init block
            if reg_init_match:
                assert reg_init_nest_level == 0, "Should not nest reg_init macro"
                reg_init_nest_level = 1

            if reg_init_nest_level > 0:
                if (ifdef_match and not reg_init_match) or ifndef_match:
                    reg_init_nest_level += 1
                if endif_match:
                    reg_init_nest_level -= 1
                    assert reg_init_nest_level >= 0, "Macro nest level should be >= 0"

                # remove line coverage results in systhesis block
                if coverage_match:
                    coverage_stat = coverage_match.group(1)
                    line = line.replace(coverage_match.group(1), " " * len(coverage_stat))

            # enter mem_init block
            if mem_init_match:
                assert mem_init_nest_level == 0, "Should not nest mem_init macro"
                mem_init_nest_level = 1

            if mem_init_nest_level > 0:
                if (ifdef_match and not mem_init_match) or ifndef_match:
                    mem_init_nest_level += 1
                if endif_match:
                    mem_init_nest_level -= 1
                    assert mem_init_nest_level >= 0, "Macro nest level should be >= 0"

                # remove line coverage results in systhesis block
                if coverage_match:
                    coverage_stat = coverage_match.group(1)
                    line = line.replace(coverage_match.group(1), " " * len(coverage_stat))

            lines += line

    with open(output_file, "w") as f:
        for line in lines:
            f.write(line)
