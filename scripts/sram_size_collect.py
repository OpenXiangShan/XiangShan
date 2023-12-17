import os
import re
import argparse
from datetime import datetime

# Define a function to extract information from a given Verilog file
def extract_info_from_verilog(file_path):
    with open(file_path, 'r') as file:
        content = file.read()

    # Use regular expressions to extract information from the comment line
    match = re.search(r'// name:array_(\d+)_ext depth:(\d+) width:(\d+) masked:(\w+) maskGran:(\d+) maskSeg:(\d+)', content)

    if match:
        x = int(match.group(1))
        y = int(match.group(2))
        z = int(match.group(3))
        t = match.group(4) == 'true'
        m = int(match.group(5))
        n = int(match.group(6))
        
        return (x, y, z, m, n, t)
    else:
        return None

# Create an argument parser
parser = argparse.ArgumentParser(description="Process Verilog files in a directory")

# Add an argument for the directory path
parser.add_argument("directory", help="Path to the directory containing Verilog files")

# Parse the command line arguments
args = parser.parse_args()

# Get the last level directory name from the input path
last_dir = os.path.basename(os.path.normpath(args.directory))

# Generate the output file name with the current time and last level directory name
output_file_name = f"{last_dir}_{datetime.now().strftime('%Y-%m-%d_%H-%M-%S')}.txt"

# List to store extracted information
info_list = []

# Iterate through the files in the specified directory
for filename in os.listdir(args.directory):
    if filename.startswith("array_") and filename.endswith("_ext.v"):
        file_path = os.path.join(args.directory, filename)
        info = extract_info_from_verilog(file_path)
        if info is not None:
            info_list.append(info)

# Sort the list of tuples based on Y, Z, M, N, and T (excluding X)
info_list.sort(key=lambda tup: tup[1:])

# Define the order for printing the information
output_order = ["X", "Y", "Z", "M", "N", "T"]

# Write the information to the output file
with open(output_file_name, 'w') as output_file:
    for info in info_list:
        info_dict = dict(zip(output_order, info))
        output_file.write(f"Y:{info_dict['Y']} Z:{info_dict['Z']} M:{info_dict['M']} N:{info_dict['N']} T:{info_dict['T']}\n")

print(f"File info has been written to {output_file_name}")
