#coding=utf-8

import os
import subprocess
import tempfile
import fnmatch
from XSPdb.cmd.util import info, error, message, warn, find_executable_in_dirs, YELLOW, RESET


class CmdASM:
    """Assembly command class for disassembling data"""

    def api_asm_str(self, asm_str, entry_address=0x80000000, debug=True, target_secs=[], search_dirs=["./ready-to-run"]):
        """Assemble RISC-V assembly code and return a dict mapping section start addresses to bytes (little-endian).
        Uses riscv64-unknown-elf-gcc and objcopy.

        Args:
            asm (str): RISC-V assembly code (can contain multiple .section/.text/.data).
            entry_addr (int): Entry address for the first section (default 0x80000000).
            debug (bool): Whether to enable debug info (default False).

        Returns:
            Dict[int, bytes]: {address: bytes} for each section.
        """
        cmd_gcc = ""
        cmd_objdump = ""
        cmd_objcopy = ""
        # Check if gcc and objdump are available
        cmd_prefix = ["riscv64-unknown-elf-", "riscv64-linux-gnu-"]
        for prefix in cmd_prefix:
            if not cmd_gcc:
                cmd_gcc = find_executable_in_dirs(prefix+"gcc", search_dirs = search_dirs)
            if not cmd_objdump:
                cmd_objdump = find_executable_in_dirs(prefix+"objdump", search_dirs = search_dirs)
            if not cmd_objcopy:
                cmd_objcopy = find_executable_in_dirs(prefix+"objcopy", search_dirs = search_dirs)
        if not cmd_gcc:
            error(f"gcc with prefix[{'or'.join(cmd_prefix)}] not found, please install it")
            return None
        if not cmd_objdump:
            error(f"objdump with prefix[{'or'.join(cmd_prefix)}] not found, please install it")
            return None
        if not cmd_objcopy:
            error(f"objcopy with prefix[{'or'.join(cmd_prefix)}] not found, please install it")
            return None
        with tempfile.TemporaryDirectory() as tmpdir:
            asm_file = os.path.join(tmpdir, "input.S")
            elf_file = os.path.join(tmpdir, "output.elf")
            map_file = os.path.join(tmpdir, "output.map")
            # Write asm to file
            with open(asm_file, "w") as f:
                raw_asm = asm_str.replace("\\t", "\t").replace(";$", "\n").replace(";", "\n\t")
                if "__start" not in raw_asm:
                    raw_asm = ".global _start\n_start:\n\t" + raw_asm
                if debug:
                    info("User Input ASM:\n"+raw_asm)
                f.write(raw_asm)
            # Assemble to ELF
            gcc_cmd = [
                cmd_gcc,
                "-nostdlib", "-Ttext", hex(entry_address),
                "-Wl,-Map=" + map_file,
                asm_file, "-o", elf_file
            ]
            subprocess.check_call(gcc_cmd)
            if debug:
                objdump_dis_cmd = [
                    cmd_objdump,
                    "-d",
                    elf_file
                ]
                objdump_dis_out = subprocess.check_output(objdump_dis_cmd, encoding="utf-8")
                info("Final Decompiled ASM:\n"+objdump_dis_out)
            # Get section info using objdump
            objdump_cmd = [cmd_objdump, "-h", elf_file]
            objdump_out = subprocess.check_output(objdump_cmd, encoding="utf-8")
            # Parse section addresses and sizes
            section_info = {}
            for line in objdump_out.splitlines():
                parts = line.split()
                if len(parts) >= 6 and parts[1].startswith('.'):
                    name = parts[1]
                    size = int(parts[2], 16)
                    addr = int(parts[3], 16)
                    if size > 0:
                        section_info[name] = (addr, size)
            # Extract each section as binary
            if debug:
                info(f"Sections in ELF:\n{objdump_out}\n")
            result = {}
            for sec, (addr, size) in section_info.items():
                sec_bin = os.path.join(tmpdir, f"{sec[1:]}.bin")
                objcopy_cmd = [
                    cmd_objcopy,
                    f"-j{sec}",
                    "-O", "binary",
                    elf_file, sec_bin
                ]
                subprocess.check_call(objcopy_cmd)
                with open(sec_bin, "rb") as f:
                    data = f.read()
                if len(target_secs) > 0:
                    if not any(fnmatch.fnmatch(sec, pattern) for pattern in target_secs):
                        if debug:
                            info(f"Section {sec} not matched, skip")
                        continue
                result[sec] = (addr, data)
            if debug:
                message = ""
                for name, (addr, data) in result.items():
                    message += f"Section[{name}] at {hex(addr)}: {data}\n"
                info(f"Sections Parsed:\n{message}" + (f"Find {len(result)} sections." if message else f"{YELLOW}No sections found{RESET}"))
            return result

    def do_xasm(self, arg, debug=True):
        """Assemble RISC-V assembly code and return a dict mapping section start addresses to bytes (little-endian).
        Uses riscv64-unknown-elf-gcc and objcopy.

        Args:
            entry_addr (int): Entry address for the first section (default self.mem_base (0x80000000)). eg <0x80000000>
            target_secs (list): List of section names to extract (default empty, all sections). eg [.text*, .data*], support wildcards.
            asm_data (str): RISC-V assembly code (can contain multiple .section/.text/.data).
        Returns:
            Dict: {name: (address,bytes)} for each section.

        Examples:
            xasm <0x80000000> [.text*,.data] addi a0, a0, 1
            xasm <0x80000000> addi a0, a0, 1
            xasm [.text*] addi a0, a0, 1
        """
        if not arg:
            message("usage: xasm [<entry_address>] [[sections,...]] <asm_data>")
            return
        target_secs = []
        arg = arg.strip()
        entry_address = self.mem_base
        try:
            if arg.startswith("<"):
                cmds = arg.split(">", 1)
                entry_address = int(cmds[0].replace("<", ""), 0)
                arg = cmds[1].strip()
            if arg.startswith("["):
                cmds = arg.split("]", 1)
                target_secs = [s.strip() for s in cmds[0].replace("[", "").split(",")]
                arg = cmds[1].strip()
            asm_str = arg
            if not asm_str:
                message("usage: xasm [<entry_address>] <asm_data>")
                return
            return self.api_asm_str(asm_str, entry_address=entry_address, debug=debug, target_secs=target_secs)
        except Exception as e:
            error(f"asm {arg} fail: {str(e)}")
            return

    def do_xasm_insert(self, arg):
        """Assemble RISC-V assembly code and insert it into the target address (with no debug message).

        Args: same as xasm
        """
        sections = self.do_xasm(arg, debug=False)
        if not sections:
            warn("No sections found, ignore insert")
            return
        bytes_count = 0
        for sec, (addr, data) in sections.items():
            if len(data) == 0:
                warn(f"Empty section: {sec}, skip")
                continue
            info(f"Insert Section[{sec}] at {hex(addr)}: with {len(data)} bytes")
            if not self.api_write_bytes(addr, data):
                break
            bytes_count += len(data)
        info(f"Total {bytes_count} bytes inserted")
