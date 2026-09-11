#!/usr/bin/env python3

import shutil
import subprocess
import re
import bisect

class symbolTableEntry:
    def __init__(self,
                 addr: int,
                 flags: list[str],
                 section: str,
                 size: int,
                 name: str,
                 demangled_name: str = None):
        self.addr = addr
        self.flags = flags
        self.section = section
        self.size = size
        self.name = name
        self.demangled_name = demangled_name

    def __repr__(self):
        return f"(0x{self.addr:x}, {self.flags}, {self.section}, {self.size}, {self.name}, {self.demangled_name})\n"

class elfutils:
    def __init__(self, elf_path, objdump=None):
        self.elf_path = elf_path
        self.symbol_table = None
        self.function_addr_index = None
        self.bb_cache = dict()
        if not objdump:
            if shutil.which("riscv64-unknown-linux-gnu-objdump"):
                self.objdump = "riscv64-unknown-linux-gnu-objdump"
            if shutil.which("riscv64-linux-gnu-objdump"):
                self.objdump = "riscv64-linux-gnu-objdump"
            elif shutil.which("riscv64-unknown-elf-objdump"):
                self.objdump = "riscv64-unknown-elf-objdump"
            elif shutil.which("riscv64-elf-objdump"):
                self.objdump = "riscv64-elf-objdump"
            else:
                raise FileNotFoundError("No riscv64 objdump found in PATH")

    def get_symbol_table(self) -> list[symbolTableEntry] | None:
        if self.symbol_table:
            return self.symbol_table
        # decode objdump -t
        cmd = [self.objdump, "-t", self.elf_path]
        result = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        if result.returncode != 0:
            return None
        lines = result.stdout.splitlines()
        symbol_table = []
        symbol_table_start = False
        for line in lines:
            if line == "SYMBOL TABLE:":
                symbol_table_start = True
                continue
            parts = line.split()
            if (len(parts) < 4) or (not symbol_table_start):
                continue
            # objdump symbol table format:
            # addr<space>flags(7char)<space>section<space>size<space>name
            addr = parts[0]
            flags = line[len(addr)+1:len(addr)+8].split()
            parts_after = line[len(addr)+9:].split()
            section = parts_after[0]
            size = parts_after[1]
            name = parts_after[2]
            mangled_name = None
            symbol_table.append(symbolTableEntry(int(addr, 16),
                                                 flags,
                                                 section,
                                                 int(size, 16),
                                                 name,
                                                 mangled_name))
        # reread with -C to demangle names
        cmd = [self.objdump, "-t", "-C", self.elf_path]
        result = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        if result.returncode != 0:
            return None
        lines = result.stdout.splitlines()
        symbol_table_start = False
        idx = 0
        for line in lines:
            if line == "SYMBOL TABLE:":
                symbol_table_start = True
                continue
            parts = line.split()
            if (len(parts) < 4) or (not symbol_table_start):
                continue
            addr = parts[0]
            parts_after = line[len(addr)+9:].split()
            name = parts_after[2]
            if idx < len(symbol_table) and symbol_table[idx].addr == int(addr, 16):
                symbol_table[idx].demangled_name = name
                idx += 1
        self.symbol_table = symbol_table
        return symbol_table

    def __build_function_addr_index(self) -> list[symbolTableEntry]:
        self.function_addr_index = sorted(
            list(
                filter(lambda entry: 'F' in entry.flags,
                    self.get_symbol_table())
            ), key=lambda entry: entry.addr
        )
        return self.function_addr_index

    def search_function_by_addr(self, addr: int) -> symbolTableEntry | None:
        if not self.function_addr_index:
            self.__build_function_addr_index()
        bisect_result = bisect.bisect_right(
            [entry.addr for entry in self.function_addr_index], addr) - 1
        if bisect_result >= 0:
            entry = self.function_addr_index[bisect_result]
            if entry.addr <= addr < entry.addr + entry.size:
                return entry
        return None

    def devide_bb_by_symbolname(self, symbol: str) -> list[(int, int)] | None:
        # Return list of basic block start addresses and its size
        if symbol in self.bb_cache:
            return self.bb_cache[symbol]
        cmd = [self.objdump, "-d", "--disassemble="+symbol, "--visualize-jumps", self.elf_path]
        result = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        if result.returncode != 0:
            return None
        function_start = False
        bb_start = set()
        cut_next = False
        for line in result.stdout.splitlines():
            if re.match(rf"^[0-9a-fA-F]+ <{re.escape(symbol)}>:$", line):
                function_start = True
                continue
            if line.strip() == "" and function_start:
                break
            if function_start:
                # addr: edges opcode instruction
                instr_tuple = line.split("\t")
                addr = int(instr_tuple[0].strip()[:-1], 16)
                hex_code = int("".join(filter(lambda x: x in '0123456789abcdef', instr_tuple[1])), 16)
                control_flow_dir = "".join(filter(lambda x: x in '-|+>X,\'', instr_tuple[1]))
                if len(control_flow_dir):
                    control_flow_dir = control_flow_dir[-1]
                else:
                    control_flow_dir = None
                rest = "\t".join(instr_tuple[2:])
                if cut_next:
                    bb_start.add(addr)
                cut_next = False
                if control_flow_dir == '>': # jump in
                    bb_start.add(addr)
                elif control_flow_dir == '-': # jump out
                    cut_next = True
                elif control_flow_dir == 'X': # both in and out
                    bb_start.add(addr)
                    cut_next = True
        sorted_bb_start = sorted(list(bb_start))
        if len(sorted_bb_start) == 0:
            return None
        function_size = self.search_function_by_addr(sorted_bb_start[0]).size
        if not function_size:
            return None
        bb_list = []
        for i in range(len(sorted_bb_start)):
            start_addr = sorted_bb_start[i]
            size = 1
            if i + 1 < len(sorted_bb_start):
                size = sorted_bb_start[i + 1] - start_addr
            else:
                size = function_size - start_addr
            bb_list.append((start_addr, size))
        self.bb_cache[symbol] = bb_list
        return self.bb_cache[symbol]
    
    def search_bb_by_addr(self, addr: int) -> (str, int, int):
        # return (function name, bb start addr, bb size)
        function_entry = self.search_function_by_addr(addr)
        if not function_entry:
            return (None, None, None)
        bb_list = self.devide_bb_by_symbolname(function_entry.name)
        if not bb_list or len(bb_list) == 0:
            return (None, None, None)
        # bisect to find bb
        bb_start_addrs = [bb[0] for bb in bb_list]
        bisect_result = bisect.bisect_right(bb_start_addrs, addr) - 1
        if bisect_result >= 0:
            bb_start, bb_size = bb_list[bisect_result]
            if bb_start <= addr < bb_start + bb_size:
                return (function_entry.name, bb_start, bb_size)
        return (None, None, None)
