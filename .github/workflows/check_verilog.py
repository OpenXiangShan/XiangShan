import sys

def err(line, loc, msg):
    print(msg)
    print(f"{sys.argv[1]}:{loc}:")
    print(line)
    exit(1)

if __name__ == "__main__":
    in_decode = False
    in_dispatch = False
    in_miss_entry = False
    in_sync_always = False
    always_depth = 0
    line_number = 0
    count_xstile = 0
    with open(sys.argv[1], "r") as f:
        for line in f:
            if "$fatal" in line or "$fwrite" in line:
                err(line, line_number, "'fatal' or 'fwrite' statement was found!")
            if "module Decode" in line:
                in_decode = True
            elif "module Dispatch" in line:
                in_dispatch = True
            elif "module MissEntry" in line:
                in_miss_entry = True
            elif "endmodule" in line:
                in_decode = False
                in_dispatch = False
                in_miss_entry = False
            elif line.startswith("module XSTile"):
                count_xstile += 1
                if count_xstile > 1:
                    err(line, line_number, "Found duplicated XSTile!\n" +
                                           "Please convert Map, Set to Seq and sort it to generate RTL in Scala.\n" +
                                           "And always use HartID from IO.\n")
            elif in_decode and "_pc" in line:
                err(line, line_number, "PC should not be in decode!!!\n")
            elif in_dispatch and "_lsrc" in line:
                err(line, line_number, "lsrc should not be in dispatch!!!\n")
            elif in_miss_entry and "refill_data_raw" in line:
                err(line, line_number, "refill_data_raw should not be in MissEntry!!!\n")
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

