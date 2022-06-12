import sys

def err(line, loc, msg):
    print(msg)
    print(f"{sys.argv[1]}:{loc}:")
    print(line)
    exit(1)

if __name__ == "__main__":
    in_decode = False
    in_dispatch = False
    in_sync_always = False
    always_depth = 0
    line_number = 0
    with open(sys.argv[1], "r") as f:
        for line in f:
            if "$fatal" in line or "$fwrite" in line:
                err(line, line_number, "'fatal' or 'fwrite' statement was found!")
            if "module Decode" in line:
                in_decode = True
            elif "module Dispatch" in line:
                in_dispatch = True
            elif "endmodule" in line:
                in_decode = False
                in_dispatch = False
            elif in_decode and "_pc" in line:
                err(line, line_number, "PC should not be in decode!!!\n")
            elif in_dispatch and "_lsrc" in line:
                err(line, line_number, "lsrc should not be in dispatch!!!\n")
            if "always @(posedge clock) begin" in line:
                in_sync_always = True
            if in_sync_always:
                if " begin " in line or line.endswith(" begin"):
                    always_depth += 1
                if " end " in line or line.endswith(" end"):
                    always_depth -= 1
                if always_depth == 0:
                    in_sync_always = False
                if "if (reset) begin" in line:
                    err(line, line_number, "should not use sync reset!!!\n")
            line_number += 1
    exit(0)
