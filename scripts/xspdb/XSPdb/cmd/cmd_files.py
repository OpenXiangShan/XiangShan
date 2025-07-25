#coding=utf-8

import os

from XSPdb.cmd.util import info, error, message, warn
import os

class CmdFiles:
    
    def api_dut_bin_load(self, bin_file):
        """Load a bin file into memory

        Args:
            bin_file (string): Path to the bin file
        """
        assert os.path.exists(bin_file), "file %s not found" % bin_file
        self.exec_bin_file = bin_file
        if self.mem_inited:
            self.df.overwrite_ram(bin_file, self.mem_size)
        else:
            self.api_init_mem()
        self.info_cache_asm.clear()

    def api_export_flash(self, bin_file, force_size=-1):
        """Export Flash data

        Args:
            bin_file (string): Path to the export file
        """
        if force_size < 0:
            if not self.api_check_if_xspdb_init_bin_loaded():
                return
        # search mret
        mret = 0x30200073
        last_data = 0
        bin_data = bytearray()
        bin_size = force_size if force_size > 0 else 1024*10
        for i in range(bin_size):
            data = self.df.FlashRead(i*8)
            bin_data += data.to_bytes(8, byteorder='little', signed=False)
            if (last_data >> 32 == mret or last_data & 0xffffffff == mret) and force_size < 0:
                break
            last_data = data
        with open(bin_file, "wb") as f:
            f.write(bin_data)
        info(f"export {len(bin_data)} bytes to flash file: {bin_file}")

    def api_export_ram(self, end_address, bin_file):
        """Export memory data

        Args:
            end_address (int): End address of memory
            bin_file (string): Path to the export file
        """
        if not self.mem_inited:
            error("mem not loaded")
            return
        end_index = 8 + end_address - end_address % 8
        with open(bin_file, "wb") as f:
            for index in range(self.mem_base, end_index, 8):
                f.write(self.df.pmem_read(index).to_bytes(8, byteorder='little', signed=False))
        info(f"export {end_index - self.mem_base} bytes to ram file: {bin_file}")

    def api_export_unified_bin(self, ram_start, ram_end, bin_file):
        """Export a unified bin file

        Args:
            ram_start (int): Start address of memory
            ram_end (int): End address of memory
            bin_file (string): Path to the export file
        """
        if not self.mem_inited:
            error("mem not loaded")
            return False
        if not self.api_check_if_xspdb_init_bin_loaded():
            return False
        # read flash data
        mret = 0x30200073
        last_data = 0
        last_indx = 0
        bin_data = bytearray()
        for i in range(1024*10):
            data = self.df.FlashRead(i*8)
            bin_data += data.to_bytes(8, byteorder='little', signed=False)
            last_indx = i + 1
            if last_data >> 32 == mret or last_data & 0xffffffff == mret:
                break
            last_data = data
        # check conflict
        # mem base
        mem_base = self.mem_base
        ram_start = ram_start - ram_start % 8
        sta_index = (ram_start - mem_base)//8
        if sta_index < last_indx:
            error(f"conflict with flash data, ram_start: 0x{ram_start:x}, flash_data_end: 0x{last_indx*8+ mem_base:x}, please check")
            return None
        ram_end = ram_end - ram_end % 8
        end_index = (ram_end - mem_base)//8 + 1
        # read ram data
        with open(bin_file, "wb") as f:
            f.write(bin_data)
            for index in range(last_indx, end_index):
                f.write(self.df.pmem_read(index*8 + mem_base).to_bytes(8, byteorder='little', signed=False))
        info(f"export {8*(end_index - last_indx) + len(bin_data)} bytes to unified bin file: {bin_file}")
        return True

    def api_convert_reg_file(self, file_name):
        """Parse a register file

        Args:
            file_name (file): Register file
        """
        assert os.path.exists(file_name), "file %s not found" % file_name
        ret_iregs = {}
        ret_fregs = {}
        raw_iregs = {"x%d"%i : self.iregs[i] for i in range(32)}
        raw_fregs = {"f%d"%i : self.fregs[i] for i in range(32)}
        with open(file_name, "r") as f:
            for i, l in enumerate(f.readlines()):
                try:
                    l = l.strip()
                    if not l:
                        continue
                    key, value = l.split(":")
                    key = key.strip().lower()
                    value = int(value.strip(), 0)
                    if key in raw_iregs:
                        key = raw_iregs[key]
                    if key in self.iregs:
                        assert key not in ret_iregs, f"{key} already exists"
                        ret_iregs[key] = value
                    if key in raw_fregs:
                        key = raw_fregs[key]
                    if key in self.fregs:
                        assert key not in ret_fregs, f"{key} already exists"
                        ret_fregs[key] = value
                except Exception as e:
                    assert False, f"line {i+1} parse fail: {str(e)}"
        return ret_iregs, ret_fregs

    def do_xbytes_to_bin(self, arg):
        """Convert bytes data to a binary file

        Args:
            arg (string): Bytes data
        """
        if not arg:
            message("usage xbytes_to_bin <bytes> <file>")
            return
        args = arg.strip().split()
        if len(args) < 2:
            message("usage xbytes_to_bin <bytes> <file>")
            return
        try:
            data = eval(args[0])
            if not isinstance(data, bytes):
                error("data must be bytes, eg b'\\x00\\x01...'")
                return
            with open(args[1], "wb") as f:
                f.write(data)
        except Exception as e:
            error(f"convert {args[0]} to bytes fail: {str(e)}")

    def complete_xbytes_to_bin(self, text, line, begidx, endidx):
        return self.api_complite_localfile(text)

    def do_xexport_bin(self, arg):
        """Export Flash + memory data to a file

        Args:
            end_address (int): End address of memory
            file_path (string): Path to the export file
            start_address (int): Start address of memory
        """
        mem_base = self.mem_base
        start_address = mem_base
        params = arg.strip().split()
        if len(params) < 2:
            message("usage: xexport_bin <end_address> <file> [start_address]")
            return
        file_path = params[1]
        if os.path.isdir(file_path):
            file_path = os.path.join(file_path, "XSPdb")
        file_dir = os.path.dirname(file_path)
        if not os.path.exists(file_dir):
            os.makedirs(file_dir)
        try:
            if len(params) > 2:
                start_address = int(params[2], 0)
            end_address = int(params[0], 0)
            if start_address != mem_base:
               if self.api_export_unified_bin(start_address, end_address, file_path+"_all.bin") is not None:
                   return
               warn(f"export unified bin to {file_path}_all.bin fail, try to export flash and ram individually")
            self.api_export_flash(file_path + "_flash.bin")
            self.api_export_ram(end_address, file_path + "_ram.bin")
        except Exception as e:
            error(f"convert {arg} to number fail: {str(e)}")

    def complete_xexport_bin(self, text, line, begidx, endidx):
        return self.api_complite_localfile(text)

    def do_xexport_flash(self, arg):
        """Export Flash data to a file

        Args:
            path (string): Path to the export file
            force_size (int): N*8 bytes to force export
        """
        if not arg:
            message("usage: xexport_flash <file> [force_size]")
            return
        args = arg.split()
        path = args[0]
        fsiz = -1
        try:
            if len(args) > 0:
                fsiz = int(args[1], 0)
            self.api_export_flash(path, fsiz)
        except Exception as e:
            error(f"{e}\n usage: xexport_flash <file> [force_size]")

    def complete_xexport_flash(self, text, line, begidx, endidx):
        return  self.api_complite_localfile(text)

    def do_xexport_ram(self, arg):
        """Export memory data to a file

        Args:
            addr (int): Export address
            arg (string): Path to the export file
        """
        args = arg.strip().split()
        if len(args) < 2:
            message("usage: xexport_mem <address> <file>")
            return
        try:
            self.api_export_ram(int(args[0], 0), args[1])
        except Exception as e:
            error(f"convert {args[0]} to number fail: {str(e)}")

    def complete_xexport_ram(self, text, line, begidx, endidx):
        return self.api_complite_localfile(text)

    def complete_xflash(self, text, line, begidx, endidx):
        return self.api_complite_localfile(text)

    def do_xload(self, arg):
        """Load a binary file into memory

        Args:
            arg (string): Path to the binary file
        """
        if not arg:
            message("usage: xload <bin_file>")
            return
        if not os.path.exists(arg):
            error(f"{arg} not found")
            return
        self.api_dut_bin_load(arg)

    def complete_xload(self, text, line, begidx, endidx):
        return self.api_complite_localfile(text)
