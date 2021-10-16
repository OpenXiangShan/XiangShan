import sys

def err(line, loc, msg):
    print(msg)
    print(f"{sys.argv[1]}:{loc}:")
    print(line)
    exit(1)

if __name__ == "__main__":
    in_module = False
    line_number = 0
    with open(sys.argv[1], "r") as f:
        for line in f:
            if "$fatal" in line or "$fwrite" in line:
                err(line, line_number, "'fatal' or 'fwrite' statement was found!")
            if "module Decode" in line:
                in_module = True
            elif "endmodule" in line:
                in_module = False
            elif in_module and "_pc" in line:
                err(line, line_number, "PC should not be in decode!!!\n")
            line_number += 1
    exit(0)
