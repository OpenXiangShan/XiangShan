import os
import sys
import re
# This script is used to convert a DCFG file to a disassembly file.
# Usage: python3 dcfg_to_disass.py <dcfg_file> <disass_file> <output_file>

def parse_dcfg_line(l):
  l = l.strip()
  ls = l.split("->")
  pc = ls[0]
  value = ls[1].split(",")
  if (len(value) != 3):
    print("Error: %s" % l)
    sys.exit(1)

  return (pc, value[0], value[1], value[2])

def parse_dcfg(dcfg_file_path):
  dcfg = open(dcfg_file_path, "r").readlines()

  result = {}
  for line in dcfg[1:]:
    (pc, cycle, time, instr) = parse_dcfg_line(line)
    result[pc] = (cycle, time, instr)
  return result

def insert2disass(parsed_dcfg, disass_file, output_file):
  disass = open(disass_file, "r").readlines()
  out = open(output_file, "w")
  for line in disass:
    found = False
    for dcfg_pc in parsed_dcfg.keys():
      if ">:" in line:
        # skip the first line of a method
        break
      if dcfg_pc in line:
        cycle, time, instr = parsed_dcfg[dcfg_pc]
        line = ("%10s %10s %10s" % (time, instr, cycle)) + line
        found = True
        break
    if not found:
      line = "%10s %10s %10s" % ("", "", "") + line
    print(line, end="")
    out.write(line)
  out.close()

if __name__ == "__main__":
  if 4 != len(sys.argv):
    print("Usage: %s <dcfg_file> <disass_file> <output_file>" % sys.argv[0])
    sys.exit(1)

  dcfg_file = sys.argv[1]
  if not os.path.exists(dcfg_file):
    print("Error: %s does not exist" % dcfg_file)
    sys.exit(1)

  disass_file = sys.argv[2]
  if not os.path.exists(disass_file):
    print("Error: %s does not exist" % disass_file)
    sys.exit(1)

  output_file = sys.argv[3]
  # if os.path.exists(output_file):
  #   print("Error: %s already exists" % output_file)
  #   sys.exit(1)

  parsed_dcfg = parse_dcfg(dcfg_file)
  insert2disass(parsed_dcfg, disass_file, output_file)