import sys

def err(line, loc, msg):
    print(msg)
    print(f"{sys.argv[1]}:{loc}:")
    print(line)
    exit(1)

if __name__ == "__main__":
    in_decode = False
    in_dispatch = False
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
            line_number += 1
    exit(0)
