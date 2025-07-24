#coding=utf-8

from XSPdb.cmd.util import error, warn, message
import os

class CmdFlash:

    def api_get_flash_init_iregs(self):
        """Get Flash internal registers

        Returns:
            list(int): Register values
        """
        if not self.api_check_if_xspdb_init_bin_loaded():
            return []
        base_offset = 8
        reg_index = self.mpc_iregs
        regs = []
        for i in range(len(reg_index)):
            regs.append((reg_index[i], self.df.FlashRead(base_offset + i*8)))
        return regs

    def api_get_flash_init_fregs(self):
        """Get Flash floating-point registers

        Returns:
            list(int): Register values
        """
        if not self.api_check_if_xspdb_init_bin_loaded():
            return []
        base_offset = 8 + 32*8
        regs = []
        for i in range(len(self.fregs)):
            regs.append((self.fregs[i], self.df.FlashRead(base_offset + i*8)))
        return regs

    def api_set_flash_float_regs(self, regs):
        """Set Flash floating-point registers

        Args:
            regs (list(float), dict): Register values
        """
        if not self.api_check_if_xspdb_init_bin_loaded():
            return
        base_offset = 8 + 32*8
        reg_map = {k: v for v, k in enumerate(self.fregs)}
        return self.api_set_flash_data_values(base_offset, self.fregs, reg_map, regs, "fregs")

    def api_set_flash_int_regs(self, regs):
        """Set Flash internal registers

        Args:
            regs (list(int), dict): Register values
        """
        if not self.api_check_if_xspdb_init_bin_loaded():
            return
        base_offset = 8
        reg_index = self.mpc_iregs
        reg_map = {k: v for v, k in enumerate(reg_index)}
        return self.api_set_flash_data_values(base_offset, reg_index, reg_map, regs, "iregs")

    def api_check_if_xspdb_init_bin_loaded(self):
        """Check if xspdb_flash_init.bin is loaded

        Returns:
            bool: Whether it is loaded
        """
        if not self.flash_bin_file or self.xspdb_init_bin not in self.flash_bin_file:
            error(f"{self.xspdb_init_bin} not loaded")
            return False
        return True

    def api_set_flash_data_values(self, base_offset, reg_index, reg_map, kdata, kname):
        """Set Flash register values

        Args:
            base_offset (int): Base address of the registers
            reg_index (list(string)): List of register names
            reg_map (dict): Mapping of register names
            kdata (list(int), dict): Register values
            kname (string): Register name
        """
        if isinstance(kdata, list):
            for i, r in enumerate(kdata):
                if isinstance(r, str):
                    r = r.strip()
                    if r == "-":
                        continue
                    r = int(r, 0)
                assert isinstance(r, int), f"{kname}[{i}] not number"
                self.df.FlashWrite(base_offset + i*8, r)
        elif isinstance(kdata, dict):
            if "*" in kdata:
                v = kdata["*"]
                for key in reg_index:
                    if key in kdata:
                        v = kdata[key]
                    self.df.FlashWrite(base_offset + reg_map[key]*8, v)
            else:
                for key, v in kdata.items():
                    if key in reg_map:
                        self.df.FlashWrite(base_offset + reg_map[key]*8, v)
                    else:
                        warn(f"{kname}[{key}] not found")
        else:
            assert False, "regs type error"

        # delete asm data in cache
        cache_index = self.flash_base - self.flash_base % self.info_cache_bsz
        if cache_index in self.info_cache_asm:
            del self.info_cache_asm[cache_index]

    def api_dut_reset_flash(self):
        """Reset the DUT Flash"""
        self.df.flash_finish()
        self.df.InitFlash("")
        self.flash_bin_file = None

    def do_xflash(self, arg):
        """Load a binary file into Flash

        Args:
            arg (string): Path to the binary file
        """
        if not arg:
            message("usage: xload <bin_file>")
            return
        if not os.path.exists(arg):
            error(f"{arg} not found")
            return
        self.api_dut_flash_load(arg)

    def do_xreset_flash(self, arg):
        """Reset Flash

        Args:
            arg (None): No arguments
        """
        self.api_reset_flash()
