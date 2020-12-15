#/usr/bin/python3
# -*- coding: UTF-8 -*-
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
    with open(input_file) as f:
        for line in f:
            line_count += 1

            ifdef = re.compile('`ifdef')
            ifndef = re.compile('`ifndef')
            endif = re.compile('`endif')
            synthesis = re.compile('`ifndef SYNTHESIS')
            line_coverage = re.compile('^\s*([%]?\d+)\s+if')

            ifdef_match = ifdef.search(line)
            ifndef_match = ifndef.search(line)
            endif_match = endif.search(line)
            synthesis_match = synthesis.search(line)
            line_coverage_match = line_coverage.search(line)
            
            # enter synthesis block
            if synthesis_match:
                assert synthesis_nest_level == 0, "Should not nest SYNTHESIS macro"
                synthesis_nest_level = 1

            if ifdef_match or (ifndef_match and not synthesis_match):
                synthesis_nest_level += 1
            if endif_match:
                synthesis_nest_level -= 1
                assert synthesis_nest_level >= 0, "Macro nest level should be >= 0"

            # remove line coverage results in systhesis block
            if synthesis_nest_level > 0 and line_coverage_match:
                coverage_stat = line_coverage_match.group(1)
                line = line.replace(line_coverage_match.group(1), " " * len(coverage_stat))

            lines += line

    with open(output_file, "w") as f:
        for line in lines:
            f.write(line)
