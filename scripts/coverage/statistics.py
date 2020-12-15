#/usr/bin/python3
# -*- coding: UTF-8 -*-
import sys
import re
import copy

if __name__ == "__main__":
    assert len(sys.argv) == 2, "Expect input_file"
    input_file = sys.argv[1]
    coverred = 0
    not_coverred = 0
    with open(input_file) as f:
        for line in f:
            coverred_pattern = re.compile('^\s*(\d+)\s+if')
            not_coverred_pattern = re.compile('^\s*(%0+)\s+if')

            coverred_match = coverred_pattern.search(line)
            not_coverred_match = not_coverred_pattern.search(line)

            assert not (coverred_match and not_coverred_match)
            
            if coverred_match:
                coverred += 1

            if not_coverred_match:
                not_coverred += 1
    print("cover: %d not_cover: %d coverage: %f" %
            (coverred, not_coverred, float(coverred) / (coverred + not_coverred)))
