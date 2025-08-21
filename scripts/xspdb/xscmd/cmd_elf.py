#***************************************************************************************
# Copyright (c) 2025 Beijing Institute of Open Source Chip (BOSC)
# Copyright (c) 2025 Institute of Computing Technology, Chinese Academy of Sciences
#
# XiangShan is licensed under Mulan PSL v2.
# You can use this software according to the terms and conditions of the Mulan PSL v2.
# You may obtain a copy of Mulan PSL v2 at:
#          http://license.coscl.org.cn/MulanPSL2
#
# THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
# EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
# MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
#
# See the Mulan PSL v2 for more details.
#***************************************************************************************


import os
import bisect
import subprocess
from . import info, error, message, warn, find_executable_in_dirs

class CmdEfl:
    """ELF command class for disassembling data"""

    def __init__(self):
        self.elf_symbol_dict = None
        self.elf_current_exe_bin_is_efl = None
        self.flag_trace_pc_symbol_block_change = False

    def api_get_elf_symbol_dict(self, elf_file, search_dirs=["./ready-to-run"]):
        """Get the symbol dictionary from an ELF file

        Args:
            elf_file (string): Path to the ELF file
            search_dirs (list): List of directories to search for CMD readelf
        """
        if not os.path.exists(elf_file):
            error(f"{elf_file} not found")
            return None
        readelf = find_executable_in_dirs("readelf", search_dirs=search_dirs)
        cmd = [readelf, "-sW", elf_file]
        try:
            output = subprocess.check_output(cmd, stderr=subprocess.STDOUT)
            lines = output.decode().splitlines()
            symbol_value_dict = {}
            symbol_name_dict = {}
            symbol_type_dict = {}
            symbol_gnored = {}
            for line in lines[1:]:
                parts = line.split()
                if len(parts) != 8:
                    continue
                if parts[1].lower() == "value":
                    continue
                value = {"addr": int(parts[1], 16), 
                         "size": int(parts[2], 16),
                         "type": parts[3],
                         "bind": parts[4],
                         "vis": parts[5],
                         "ndx": parts[6],
                         "name": parts[7],
                         }
                if value["type"] in ("NOTYPE", "FILE"):
                    if value["ndx"] not in ("1", "2", ".text", ".data"):
                        if value["type"] not in symbol_gnored:
                            symbol_gnored[value["type"]] = 1
                        else:
                            symbol_gnored[value["type"]] += 1
                        continue
                symbol_name_dict[value["name"]] = value
                if value["type"] not in symbol_type_dict:
                    symbol_type_dict[value["type"]] = 1
                else:
                    symbol_type_dict[value["type"]] += 1
                if symbol_value_dict.get(value["addr"]) is None:
                    symbol_value_dict[value["addr"]] = [value]
                else:
                    symbol_value_dict[value["addr"]].append(value)
            info("Find symbol: %s" % symbol_type_dict)
            if len(symbol_gnored) > 0:
                warn("Ignored symbol: %s" % symbol_gnored)
            return {"addr": symbol_value_dict,
                    "name": symbol_name_dict,
                    "sorted_addr": sorted(symbol_value_dict.keys()),
                    "sorted_name": sorted(symbol_name_dict.keys()),
                    }
        except subprocess.CalledProcessError as e:
            error(f"Failed to read ELF file: {e.output.decode()}")
            return None

    def api_echo_pc_symbol_block_change(self, current_pc, last_block_addr, last_pc):
        block_addr = last_block_addr
        if current_pc < 0:
            return block_addr
        if not self.flag_trace_pc_symbol_block_change:
            return block_addr
        if self.elf_current_exe_bin_is_efl is False:
            return block_addr
        if self.elf_symbol_dict is None:
            return block_addr
        symbol_index = bisect.bisect_left(self.elf_symbol_dict["sorted_addr"], current_pc) - 1
        if symbol_index < 0:
            return block_addr
        if symbol_index >= len(self.elf_symbol_dict["sorted_addr"]):
            return block_addr
        symbol_addr = self.elf_symbol_dict["sorted_addr"][symbol_index]
        if symbol_addr == last_block_addr:
            return block_addr
        # block address changed
        symbol = self.elf_symbol_dict.get("addr", {}).get(symbol_addr)
        if not symbol:
            return block_addr
        symbol_name = ','.join([s['name'] for s in symbol])
        symbol_pre = self.elf_symbol_dict.get("addr", {}).get(last_block_addr)
        symbol_pre_name = "None"
        if symbol_pre:
            symbol_pre_name = ','.join([s['name'] for s in symbol_pre])
        delta_last = last_pc-last_block_addr
        delta_curr = current_pc-symbol_addr
        message(f"PC block changed({hex(last_pc)} = > {hex(current_pc)}, " +
                f"cycle: {self.difftest_stat.trap.cycleCnt}): {symbol_pre_name}({hex(last_block_addr)})+{hex(delta_last)} " +
                f"-> {symbol_name}({hex(symbol_addr)})+{hex(delta_curr)}")
        return symbol_addr

    def complete_xtrace_pc_symbol_block_change(self, text, line, begidx, endidx):
        """Complete the command for tracing PC symbol block change

        Args:
            text (string): Current text
            line (string): Current line
            begidx (int): Beginning index
            endidx (int): Ending index
        """
        return [x for x in ["on", "off"] if x.startswith(text)] if text else ["on", "off"]
