import sys

if __name__ == "__main__":
    in_module = False
    line_number = 0
    with open(sys.argv[1], "r") as f:
        for line in f:
            if "module Decode" in line:
                in_module = True
            elif "endmodule" in line:
                in_module = False
            elif in_module and "_pc" in line:
                print("PC should not be in decode!!!\n")
                print(f"{sys.argv[1]}:{line_number}:")
                print(line)
                exit(1)
            line_number += 1
    exit(0)
