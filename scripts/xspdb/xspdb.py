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
import sys
import inspect
import pkgutil
import signal
import time
import pdb

from bdb import BdbQuit
from pyxscore import DUTSimTop, xsp
from pydifftest import difftest as df
from collections import OrderedDict
from logging import DEBUG, INFO, WARNING, ERROR
from xspdb.xscmd.util import load_module_from_file, load_package_from_dir, set_xspdb_log_level, set_xspdb_debug_level, logging_level_map
from xspdb.xscmd.util import message, info, error, warn, build_prefix_tree, register_commands, YELLOW, RESET, xspdb_set_log, xspdb_set_log_file, log_message

class XSPdb(pdb.Pdb):
    def __init__(self, dut, default_file=None,
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

    def check_is_need_trace(self):
        if getattr(self, "__xspdb_need_fast_trace__", False) is True:
            setattr(self, "__xspdb_need_fast_trace__" ,False)
            info("Force set trace") 
            self.set_trace()
        if self.interrupt is True:
            if getattr(self, "__xspdb_set_traced__", None) is None:
                self.setattr(self, "__xspdb_set_traced__", True) 
                info("Find interrupt, set trace")
                self.set_trace()
        return False

    def __init_pdb(self, args):
        if args.log: 
            self.api_log_enable_log(True)
        if args.log_file:
            self.api_log_set_log_file(args.log_file)
        if args.debug_level:
            set_xspdb_debug_level(logging_level_map[args.debug_level])
        if args.log_level:
            set_xspdb_log_level(logging_level_map[args.log_level])
        if (args.image):
            self.api_dut_bin_load(args.image)
        if (args.mem_base_address):
            self.df.Set_PMEM_BASE(args.mem_base_address)

    def __run_batch(self, args):
        if args.interact_at > 0:
            def cb_on_interact(s, checker, k, clk, sig, target):
                info(f"Interact at HW cycle = {target}")
                setattr(self, "__xspdb_need_fast_trace__", True)
            info(f"Set interact callback at HW cycle = {args.interact_at}")
            self.api_xbreak("SimTop_top.SimTop.timer", "eq", args.interact_at, callback=cb_on_interact, callback_once=True)
        if args.script:
            if run_script(xspdb, args.script, args.batch_interval):
                return
        cycle_batch_size = 100
        cycle_batch_count = args.max_cycles // cycle_batch_size
        cycle_reach_max_time = False
        time_start = time.time()
        def emu_step(delta):
            c = self.api_step_dut(delta)
            self.check_is_need_trace()
            return c
        def run_cycle_deta(delta):
            nonlocal cycle_reach_max_time
            runc = 0
            while delta > 0 and not self.api_dut_is_step_exit():
                c = emu_step(delta)
                runc += c
                delta = delta - c
                self.check_is_need_trace()
                delta_time = time.time() - time_start
                print ("step py cycle")
                if delta_time > args.max_run_time and args.max_run_time > 0:
                    cycle_reach_max_time = True
                    XSPdb.info(f"Max run time {timesec_to_str(args.max_run_time)} reached (runed {timesec_to_str(delta_time)}), exit cycle execution")
                    break
            return runc
        run_cycles = 0
        for _ in range(cycle_batch_count):
            if not cycle_reach_max_time and not self.api_dut_is_step_exit():
                run_cycles += run_cycle_deta(cycle_batch_size)
            else:
                break
        delta = args.max_cycles - run_cycles
        info("Finished cycles: %d (%d ignored)" % (run_cycles, 0))

    def run(self, args):
        self.__init_pdb(args)
        try:
            if args.batch:
                self.__run_batch(args)
            else:
                self.set_trace()
                while True:
                    dut.Step(1000)
        except BdbQuit:
            pass

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
        import xspdb.xscmd
        cmd_count = self.api_load_custom_pdb_cmds(xspdb.xscmd)
        info(f"Loaded {cmd_count} functions from XSPdb.cmd")

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

    # override the default PDB function to avoid None cmd error
    def parseline(self, line):
        cmd, arg, line = super().parseline(line)
        return cmd or "", arg, line

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

    @staticmethod
    def api_log_enable_log(enable):
        xspdb_set_log(enable)

    @staticmethod
    def api_log_set_log_file(log_file):
        xspdb_set_log(True)
        xspdb_set_log_file(log_file)

    def api_busy_sleep(self, data, delta=0.1):
        for i in range(int(data//delta)):
            time.sleep(delta)
            if self.interrupt:
                return (i+1)*delta
        return data

    def complete_xset_log_file(self, text, line, begidx, endidx):
        return self.api_complite_localfile(text)

    def record_cmd(self, cmd):
        log_message(self.log_cmd_prefix + cmd + self.log_cmd_suffix)

    def onecmd(self, line, log_cmd=True):
        """Override the onecmd to log the command"""
        if log_cmd:
            self.record_cmd(line)
        return super().onecmd(line)
