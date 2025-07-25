#coding=utf-8

import pdb
from .ui import enter_simple_tui
from collections import OrderedDict
import os
import inspect
import pkgutil
import signal
import time
import sys

from XSPdb.cmd.util import message, info, error, warn, build_prefix_tree, register_commands, YELLOW, RESET, xspdb_set_log, xspdb_set_log_file, log_message
from XSPdb.cmd.util import load_module_from_file, load_package_from_dir, set_xspdb_log_level
from logging import DEBUG, INFO, WARNING, ERROR

class XSPdb(pdb.Pdb):
    def __init__(self, dut, df, xsp, default_file=None,
                 mem_base=0x80000000,
                 flash_base=0x10000000,
                 finstr_addr=None,
                 default_mem_size=1024*1024*1024, # 1GB
                 default_flash_size=0x10000000,
                 no_interact=False,
                 ):
        """Create a PDB debugger for XiangShan
        Args:
            dut (DUT): DUT exported by picker
            df (difftest): Difftest exported from DUT Python library
            xsp (xspcomm): xspcomm exported from DUT Python library
            default_file (string): Default bin file to load
            mem_base (int): Memory base address
            finstr_addr (int): First instruction address
            flash_base (int): Flash base address
            default_mem_size (int): Default memory size
            default_flash_size (int): Default flash size
            no_interact (bool): No interact mode, default is False
        """
        super().__init__()
        self.dut = dut
        self.df = df
        self.xsp = xsp
        self.mem_base = mem_base
        self.finstr_addr = mem_base if finstr_addr is None else finstr_addr
        self.flash_base = flash_base
        self.flash_ends = flash_base + default_flash_size
        self.no_interact = no_interact
        self.interrupt_count = 0
        signal.signal(signal.SIGINT, self._sigint_handler)
        if no_interact:
            info("Start XSPdb whit no_interact config, press Ctrl+C will interrupt the program")
        self.dut_tree = None
        self.prompt = "(XiangShan) "
        self.in_tui = False
        # Init dut uart echo
        self.dut.InitClock("clock")
        self.c_stderr_echo = xsp.ComUseEcho(dut.difftest_uart_out_valid.CSelf(), dut.difftest_uart_out_ch.CSelf())
        self.dut.StepRis(self.c_stderr_echo.GetCb(), self.c_stderr_echo.CSelf(), "uart_echo")
        # Init difftest
        self.exec_bin_file = default_file
        self.mem_size = default_mem_size
        self.mem_inited = False
        self.api_update_pmem_base_and_first_inst_addr(self.mem_base, self.finstr_addr)
        if self.exec_bin_file:
            assert os.path.exists(self.exec_bin_file), "file %s not found" % self.exec_bin_file
            info("load: %s" % self.exec_bin_file)
            self.api_init_mem()
        self.df.InitFlash("")
        self.xspdb_init_bin = "xspdb_flash_init.bin"
        self.flash_bin_file = None
        self.df.difftest_init()
        self.difftest_stat =  df.GetDifftest(0).dut
        self.difftest_flash = df.GetFlash()
        self.register_map = OrderedDict()
        self.load_cmds()
        self.api_init_waveform()
        self.init_cmds = []
        self.sigint_original_handler = signal.getsignal(signal.SIGINT)
        self.sigint_callback = []
        self.log_cmd_prefix = "@cmd{"
        self.log_cmd_suffix = "}"

    def _sigint_handler(self, s, f):
        self.interrupt = True
        warn("[Ctrl+C] Interrupted")
        for f in self.sigint_callback:
            f(self)
        if self.no_interact:
            warn("No interact mode, exit(-1)")
            raise sys.exit(-1)
        self.interrupt_count += 1
        if self.interrupt_count > 3:
            warn("Too many interrupts, force entering pdb")
            self.set_trace()
            self.interrupt_count = 0

    def __del__(self):
        """Destructor"""
        if self.sigint_original_handler:
            signal.signal(signal.SIGINT, self.sigint_original_handler)

    def is_no_interact(self):
        """Check if no interact mode"""
        return self.no_interact

    def get_dut_tree(self):
        """Get the DUT tree"""
        if self.dut_tree is None:
            info("First time to use DUTree, build it ...")
            self.dut_tree = build_prefix_tree(self.dut.GetInternalSignalList())
        return self.dut_tree

    def api_init_mem(self):
        """Initialize memory"""
        if self.mem_inited:
            return
        self.df.InitRam(self.exec_bin_file, self.mem_size)
        self.mem_inited = True

    def api_update_pmem_base_and_first_inst_addr(self, a, b):
        """Set diftest PMEM_BASE and FIRST_INST_ADDRESS

        Args:
            a (int): PMEM_BASE value
            b (int): FIRST_INST_ADDRESS value
        Returns:
            (PMEM_BASE, FIRST_INST_ADDRESS): current value of PMEM_BASE and FIRST_INST_ADDRESS
        """
        if not hasattr(self.df, "Set_PMEM_BASE"):
            warn("difftest.Set_PMEM_BASE not found, update your difftest")
            warn("ignore memory PMEM_BASE set")
            return None, None
        if a is not None:
            self.df.Set_PMEM_BASE(a)
        if b is not None:
            self.df.Set_FIRST_INST_ADDRESS(b)
        x, y = self.df.Get_PMEM_BASE(), self.df.Get_FIRST_INST_ADDRESS()
        if a is not None:
            info("Set PMEM_BASE to %s (Current: %s)" % (hex(a), hex(x)))
        if b is not None:
            info("Set FIRST_INST_ADDRESS to %s (Current: %s)" % (hex(b), hex(y)))
        return x, y

    def load_cmds(self):
        import XSPdb.cmd
        cmd_count = self.api_load_custom_pdb_cmds(XSPdb.cmd)
        info(f"Loaded {cmd_count} functions from XSPdb.cmd")

    # override the default PDB function to avoid None cmd error
    def parseline(self, line):
        cmd, arg, line = super().parseline(line)
        return cmd or "", arg, line

    def api_load_custom_pdb_cmds(self, path_or_module):
        """Load custom command

        Args:
            path_or_module (string/Module): Command file path or directory (or python module)
        """
        if isinstance(path_or_module, str):
            if path_or_module.strip().endswith("/"):
                path_or_module = path_or_module.strip()[:-1]
        mod = path_or_module
        if not inspect.ismodule(path_or_module):
            if os.path.isdir(path_or_module):
                mod = load_package_from_dir(path_or_module)
            elif os.path.isfile(path_or_module):
                mod = load_module_from_file(path_or_module)
                return register_commands(mod, self.__class__, self)
            else:
                error(f"Invalid path or module: {path_or_module}")
                return -1
        # module
        cmd_count = 0
        for _, modname, _ in pkgutil.iter_modules(mod.__path__):
            if not modname.startswith("cmd_"):
                continue
            submod = __import__(f"{mod.__name__}.{modname}", fromlist=[modname])
            cmd_count += register_commands(submod, self.__class__, self)
        return cmd_count

    def do_xuse_custom_cmds(self, arg):
        """Load custom command from file or directory

        Args:
            arg (string): Command file path or directory (or python module)
        """
        if not arg:
            error("Please specify a file or directory")
            message("usage: xuse_custom_cmds <file/directory/module>")
            return
        cmd_count = self.api_load_custom_pdb_cmds(arg)
        info(f"Loaded {cmd_count} commands from {arg}")

    def complete_xuse_custom_cmds(self, text, line, begidx, endidx):
        """Complete the custom command file or directory"""
        return self.api_complite_localfile(text)

    def api_set_debug_level(self, level):
        """Set log level

        Args:
            level (string): Log level
        """
        level_map = {
            "debug": DEBUG,
            "info": INFO,
            "warn": WARNING,
            "error": ERROR
        }
        if level not in level_map:
            return False
        set_xspdb_log_level(level_map[level])
        return True

    def do_xset_log_level(self, arg):
        """Set log level

        Args:
            arg (string): Log level
        """
        if not arg:
            message("usage: xset_log_level <log level>, log level can be debug, info, warn, error")
            return
        level = arg.strip().lower()
        if not self.api_set_debug_level(level):
            message("usage: xset_log_level <log level>, log level can be debug, info, warn, error")

    def complete_xset_log_level(self, text, line, begidx, endidx):
        """Complete the log level"""
        return [k for k in ["debug", "info", "warn", "error"] if k.startswith(text)]

    def do_xexportself(self, var):
        """Set a variable to XSPdb self

        Args:
            var (string): Variable name
        """
        self.curframe.f_locals[var] = self

    def do_xlist_xclock_cb(self, arg):
        """List all xclock callbacks

        Args:
            arg (None): No arguments
        """
        message("Ris Cbs:")
        for cb in self.dut.xclock.ListSteRisCbDesc():
            message("\t", cb)
        message("Fal Cbs:")
        for cb in self.dut.xclock.ListSteFalCbDesc():
            message("\t", cb)

    def do_xui(self, arg):
        """Enter the Text UI interface

        Args:
            arg (None): No arguments
        """
        if self.in_tui:
            error("Already in TUI")
            return
        self.tui_ret = None
        self.in_tui = True
        enter_simple_tui(self)
        self.in_tui = False
        self.on_update_tstep = None
        self.interrupt = False
        info("XUI Exited.")
        return self.tui_ret

    def do_xcmds(self, arg):
        """Print all xcmds

        Args:
            arg (None): No arguments
        """
        cmd_count = 0
        max_cmd_len = 0
        cmds = []
        for cmd in dir(self):
            if not cmd.startswith("do_x"):
                continue
            cmd_name = cmd[3:]
            max_cmd_len = max(max_cmd_len, len(cmd_name))
            cmd_desc = f"{YELLOW}Description not found{RESET}"
            try:
                cmd_desc = getattr(self, cmd).__doc__.split("\n")[0]
            except Exception as e:
                pass
            cmds.append((cmd, cmd_name, cmd_desc))
            cmd_count += 1
        cmds.sort(key=lambda x: x[0])
        for c in cmds:
            message(("%-"+str(max_cmd_len+2)+"s: %s (from %s)") % (c[1], c[2], self.register_map.get(c[0], self.__class__.__name__)))
        info(f"Total {cmd_count} xcommands")

    def do_xapis(self, arg):
        """Print all APIs

        Args:
            arg (None): No arguments
        """
        api_count = 0
        max_api_len = 0
        apis = []
        for api in dir(self):
            if not api.startswith("api_"):
                continue
            api_name = api
            max_api_len = max(max_api_len, len(api_name))
            api_desc = f"{YELLOW}Description not found{RESET}"
            try:
                api_desc = getattr(self, api).__doc__.split("\n")[0]
            except Exception as e:
                pass
            apis.append((api, api_name, api_desc))
            api_count += 1
        apis.sort(key=lambda x: x[0])
        for c in apis:
            message(("%-"+str(max_api_len+2)+"s: %s (from %s)") % (c[1], c[2], self.register_map.get(c[0], self.__class__.__name__)))
        info(f"Total {api_count} APIs")

    @staticmethod
    def api_log_enable_log(enable):
        xspdb_set_log(enable)

    @staticmethod
    def api_log_set_log_file(log_file):
        xspdb_set_log(True)
        xspdb_set_log_file(log_file)

    def do_xlogfile_enable(self, arg):
        """Set log on or off

        Args:
            arg (string): Log level
        """
        if not arg:
            message("usage: xlogfile_enable <on or off>")
            return
        if arg == "on":
            info("Set log on")
            xspdb_set_log(True)
        elif arg == "off":
            xspdb_set_log(False)
            info("Set log off")
        else:
            message("usage: xlogfile_enable <on or off>")

    def complete_xlogfile_enable(self, text, line, begidx, endidx):
        """Complete the log level"""
        return [k for k in ["on", "off"] if k.startswith(text)]

    def do_xset_log_file(self, arg):
        """Set log file

        Args:
            arg (string): Log file name
        """
        if not arg:
            message("usage: xset_log_file <log file>")
            return
        xspdb_set_log(True)
        xspdb_set_log_file(arg)
        info("Enable log and set log file to: %s" % arg)

    def do_xnop(self, arg):
        """Nop cmd do nothing"""
        pass

    def api_busy_sleep(self, data, delta=0.1):
        for i in range(int(data//delta)):
            time.sleep(delta)
            if self.interrupt:
                return (i+1)*delta
        return data

    def do_xpause(self, arg):
        """Pause the interactive shell

        Args:
            time (float): time to pause, default is 1 second
        """
        p_time = 1
        a = arg.strip()
        if a:
            try:
                p_time = float(a)
            except Exception as e:
                error("Convert pause time fail: %s, from args: %s \nsage: xpause [time]" % (e, arg))
                return
        info("Pause for %s seconds" % p_time)
        self.api_busy_sleep(p_time)

    def complete_xset_log_file(self, text, line, begidx, endidx):
        return self.api_complite_localfile(text)

    def record_cmd(self, cmd):
        log_message(self.log_cmd_prefix + cmd + self.log_cmd_suffix)

    def onecmd(self, line, log_cmd=True):
        """Override the onecmd to log the command"""
        if log_cmd:
            self.record_cmd(line)
        return super().onecmd(line)

    def _exec_batch_cmds(self, exec=None, break_handler=None):
        exec_count = 0
        if len(self.batch_cmds_to_exec) <= 0:
            return exec_count
        if break_handler is None:
            break_handler = self.api_batch_get_default_break_cb()
        if exec is None:
            exec = self.onecmd
        self.batch_depth += 1
        while len(self.batch_cmds_to_exec) > 0:
            line, gap_time, callback = self.batch_cmds_to_exec.pop(0)
            info(f"Batch exec: '{line}'")
            self.api_dut_step_ready()
            self.__last_batch_cmd_ret__ = exec(line, False)
            if callback:
                callback(self, line)
            self.api_busy_sleep(gap_time)
            if self.interrupt:
                if callable(break_handler):
                    self.batch_depth -= 1
                    return break_handler(exec_count)
            exec_count += 1
        self.batch_depth -= 1
        return exec_count

    def interaction(self, frame, traceback):
        """Override the interaction to run init cmd"""
        if self.init_cmds:
            self.setup(frame, traceback)
            cmds = []
            while len(self.init_cmds) > 0:
                cmd = self.init_cmds.pop(0)
                info("Find init cmd: '%s', add to batch cmd queue" % cmd)
                cmds.append((cmd, 0.1, None))
            if cmds:
                self.api_batch_append_head_cmds(cmds)
            if self._exec_batch_cmds() is not False:
                return
        return super().interaction(frame, traceback)

    def api_append_init_cmd(self, cmd):
        self.init_cmds.append(cmd)
