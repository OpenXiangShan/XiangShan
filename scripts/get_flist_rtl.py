import os
import sys

def get_dir_files(file_dir):
  file_list = []
  for root, dirs, files in os.walk(file_dir):
    for f in files:
      if not (f.startswith("STD_CLKGT")):
        file_list.append(f)
  #print(file_list)
  return file_list

def create_file_list(dst_path, lst, prefix):
  with open(dst_path, 'w') as dst:
    for name in lst:
      dst.write(f"{prefix}{name}\n")

phy_files = get_dir_files(sys.argv[1] + "/lib/sram/wrapper")
raw_files = get_dir_files("./rtl/XSTop")
rtl_files = []

for f in raw_files:
  if f not in phy_files:
    rtl_files.append(f)
  else:
    print(f"{f} will be replaced!")

create_file_list(sys.argv[1] + "/flist/flist_rtl.f", rtl_files, "../rtl/XSTop/")
