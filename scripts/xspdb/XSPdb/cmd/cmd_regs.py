#coding=utf-8

import os
from XSPdb.cmd.util import message, error

class CmdRegs:
    """Register operations"""

    def __init__(self):
        assert hasattr(self, "dut"), "this class must be used in XSPdb, canot be used alone"
        self.fregs = ["ft0", "ft1", "ft2",  "ft3", "ft4", "ft5", "ft6",  "ft7",
                      "fs0", "fs1", "fa0",  "fa1", "fa2", "fa3", "fa4",  "fa5",
                      "fa6", "fa7", "fs2",  "fs3", "fs4", "fs5", "fs6",  "fs7",
                      "fs8", "fs9", "fs10", "fs11","ft8", "ft9", "ft10", "ft11"]
        self.iregs = ["zero", "ra", "sp", "gp",  "tp", "t0", "t1", "t2",
                      "s0",   "s1", "a0", "a1",  "a2", "a3", "a4", "a5",
                      "a6",   "a7", "s2", "s3",  "s4", "s5", "s6", "s7",
                      "s8",   "s9", "s10","s11", "t3", "t4", "t5", "t6"]
        self.iregs_mapk = {k: i for i, k in enumerate(self.iregs)}
        self.iregs_mapv = {i: k for i, k in enumerate(self.iregs)}
        self.fregs_mapk = {k: i for i, k in enumerate(self.fregs)}
        self.fregs_mapv = {i: k for i, k in enumerate(self.fregs)}
        self.mpc_iregs = self.iregs.copy()
        self.mpc_iregs[0] = "mpc"

    def do_xlist_freg_map(self, arg):
        """List floating-point register mappings

        Args:
            arg (None): No arguments
        """
        for i, r in enumerate(self.fregs):
            message(f"x{i}: {r}", end=" ")
        message("")

    def do_xlist_flash_fregs(self, arg):
        """List Flash floating-point registers

        Args:
            arg (None): No arguments
        """
        for r in self.api_get_flash_init_fregs():
            message(f"{r[0]}: {hex(r[1])}", end=" ")
        message("")

    def do_xlist_flash_iregs(self, arg):
        """List Flash internal registers

        Args:
            arg (None): No arguments
        """
        for r in self.api_get_flash_init_iregs():
            message(f"{r[0]}: {hex(r[1])}", end=" ")
        message("")

    def do_xset_fregs(self, arg):
        """Set Flash floating-point registers (general)

        Args:
            arg (string): Register values
        """
        if not arg:
            message("usage: xset_fregs <regs>, format: {\"reg_name\": value} or [value1, value2, ...]")
            return
        try:
            self.api_set_flash_float_regs(eval(arg))
        except Exception as e:
            error(f"set_fregs fail: {str(e)}")

    def do_xset_ireg(self, arg):
        """Set a single Flash internal register (Integer)

        Args:
            arg (string): Register name and value
        """
        if not arg:
            message("usage: xset_ireg <reg_name> <value>")
            return
        args = arg.strip().split()
        if len(args) < 2:
            message("usage: xset_ireg <reg_name> <value>")
            return
        try:
            self.api_set_flash_int_regs({args[0]: int(args[1], 0)})
        except Exception as e:
            error(f"set_ireg fail: {str(e)}")

    def do_xset_iregs(self, arg):
        """Set Flash internal registers (Integer)

        Args:
            arg (string): Register values
        """
        if not arg:
            message("usage: xset_iregs <regs>, format: {\"reg_name\": value} or [value1, value2, ...]")
            return
        try:
            self.api_set_flash_int_regs(eval(arg))
        except Exception as e:
            error(f"set_iregs fail: {str(e)}")

    def do_xset_freg(self, arg):
        """Set a Flash floating-point register

        Args:
            arg (string): Register name and value
        """
        if not arg:
            message("usage: xset_freg <reg_name> <value>")
            return
        args = arg.strip().split()
        if len(args) < 2:
            message("usage: xset_freg <reg_name> <value>")
            return
        try:
            self.api_set_flash_float_regs({args[0]: int(args[1], 0)})
        except Exception as e:
            error(f"set_freg fail: {str(e)}")

    def complete_xset_ireg(self, text, line, begidx, endidx):
        return [k for k in ["mpc", "ra", "sp", "gp",  "tp", "t0", "t1", "t2",
                            "s0",   "s1", "a0", "a1",  "a2", "a3", "a4", "a5",
                            "a6",   "a7", "s2", "s3",  "s4", "s5", "s6", "s7",
                            "s8",   "s9", "s10","s11", "t3", "t4", "t5", "t6"] if k.startswith(text)]

    def complete_xset_freg(self, text, line, begidx, endidx):
        return [k for k in self.fregs if k.startswith(text)]

    def do_xload_reg_file(self, arg):
        """Load a register file

        Args:
            arg (file): Register file
        """
        if not arg:
            error("load_reg_file <reg_file>")
            return
        if not os.path.exists(arg):
            error("file %s not found" % arg)
            return
        iregs, fregs = self.api_convert_reg_file(arg)
        self.api_set_flash_int_regs(iregs)
        self.api_set_flash_float_regs(fregs)

    def complete_xload_reg_file(self, text, line, begidx, endidx):
        return self.api_complite_localfile(text)

    def do_xparse_reg_file(self, arg):
        """Parse a register file

        Args:
            arg (file): Register file
        """
        if not arg:
            error("parse_reg_file <reg_file>")
            return
        if not os.path.exists(arg):
            error("file %s not found" % arg)
            return
        iregs, fregs = self.api_convert_reg_file(arg)
        message("iregs:\n", str(iregs))        
        message("fregs:\n", str(fregs))

    def complete_xparse_reg_file(self, text, line, begidx, endidx):
        return self.api_complite_localfile(text)
