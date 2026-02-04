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
from xspdb.xscmd.util import message, info, error, warn, build_prefix_tree, register_commands, YELLOW, RESET, xspdb_set_log, xspdb_set_log_file, log_message, xspdb_set_ui_handler, ui_prompt
from xspdb.ui import enter_tui

def timesec_to_str(t):
    h = int(t // 3600)
    m = int((t % 3600) // 60)
    s = int(t % 60)
    return f"{h:02}:{m:02}:{s:02}"

def run_script(xspdb, script_path, it_time):
    xspdb.api_exec_script(script_path, gap_time=it_time)
    xspdb.api_append_init_cmd("xnop")
    xspdb.set_trace()
    return False

def run_replay(xspdb, replay_path, it_time):
    xspdb.api_exec_script(replay_path, gap_time=it_time,
                          target_prefix=xspdb.log_cmd_prefix,
                          target_subfix=xspdb.log_cmd_suffix,
                          )
    xspdb.api_append_init_cmd("xnop")
    xspdb.set_trace()
    return False

def run_commits(xspdb, commits, max_run_time):
    time_start = time.time()
    if commits < 0:
        commits = 0xFFFFFFFFFFFFFF
    batch_size = 100
    batch_count = commits // batch_size
    batch_remain = commits % batch_size
    reach_max_time = False
    def run_delta(delta):
        nonlocal reach_max_time
        runc = 0
        while delta > 0 and not xspdb.api_dut_is_step_exit():
            c = xspdb.api_xistep(delta)
            runc += c
            delta = delta - c
            xspdb.check_is_need_trace()
            delta_time = time.time() - time_start
            if delta_time > max_run_time and max_run_time > 0:
                info(f"Max run time {timesec_to_str(max_run_time)} reached (runed {timesec_to_str(delta_time)}), exit instruction execution")
                reach_max_time = True
                break
        return runc
    run_ins = 0
    for _ in range(batch_count):
        if not reach_max_time and not xspdb.api_dut_is_step_exit():
           run_ins += run_delta(batch_size)
        else:
            break
    if not reach_max_time:
        run_ins += run_delta(batch_remain)
    message(f"Execute {run_ins} commits completed ({commits - run_ins} ignored)")

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
        self.df.difftest_init(False, self.mem_size)
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
        self._ui_handler = None

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
        if (args.flash):
            self.api_dut_flash_load(args.flash)
        if (args.flash_base_address):
            self.flash_base = args.flash_base_address
        if (args.diff_first_inst_address != -1):
            self.finstr_addr = args.diff_first_inst_address
            self.api_update_pmem_base_and_first_inst_addr(self.mem_base, self.finstr_addr)
        if (args.ram_size is not None):
            self.mem_size = args.ram_size
        if (args.trace_pc_symbol_block_change):
            self.api_turn_on_pc_symbol_block_change(True)
        if (args.cmds):
            for c in args.cmds.replace("\\n", "\n").split("\n"):
                self.api_append_init_cmd(c.strip())
        if (args.cmds_post):
            for c in args.cmds_post.replace("\\n", "\n").split("\n"):
                self.api_batch_append_tail_one_cmd(c.strip())

    def __run_batch(self, args):
        # Handle waveform control
        if args.wave_begin != args.wave_end:
            wave_file_path = args.wave_path if args.wave_path else ""
            if args.wave_begin <= 0:
                info(f"Waveform on at HW cycle = Zero")
                self.api_waveform_on(wave_file_path)
            else:
                def cb_on_wave_begin(s, checker, k, clk, sig, target):
                    info(f"Waveform on at HW cycle = {target}")
                    self.api_waveform_on(wave_file_path)
                    s.interrupt = False
                info(f"Set waveform on callback at HW cycle = {args.wave_begin}")
                self.api_xbreak("SimTop_top.SimTop.timer", "eq", args.wave_begin, callback=cb_on_wave_begin, callback_once=True)
            def cb_on_wave_end(s, checker, k, clk, sig, target):
                info(f"Waveform off at HW cycle = {target}")
                self.api_waveform_off()
                s.interrupt = False
            if args.wave_end > 0:
                info(f"Set waveform off callback at HW cycle = {args.wave_end}")
                self.api_xbreak("SimTop_top.SimTop.timer", "eq", args.wave_end, callback=cb_on_wave_end, callback_once=True)

        # Handle interact_at
        if args.interact_at > 0:
            def cb_on_interact(s, checker, k, clk, sig, target):
                info(f"Interact at HW cycle = {target}")
                setattr(self, "__xspdb_need_fast_trace__", True)
            info(f"Set interact callback at HW cycle = {args.interact_at}")
            self.api_xbreak("SimTop_top.SimTop.timer", "eq", args.interact_at, callback=cb_on_interact, callback_once=True)

        # Handle script execution
        if args.script:
            if run_script(self, args.script, args.batch_interval):
                return

        # Handle replay
        if args.replay:
            if run_replay(self, args.replay, args.batch_interval):
                return

        # Handle diff
        if args.diff:
            if not self.api_load_ref_so(args.diff):
                error(f"Load difftest ref so {args.diff} failed")
                return
            self.api_set_difftest_diff(True)

        # Set trace if cmds or cmds-post are set but no script/replay
        if args.cmds or args.cmds_post:
            if not args.script and not args.replay:
                self.set_trace()

        # Handle pc-commits
        if args.pc_commits != 0:
            cycle_index = self.dut.xclock.clk
            run_commits(self, args.pc_commits, args.max_run_time)
            run_cycles = self.dut.xclock.clk - cycle_index
            wave_at_last = (args.wave_begin != args.wave_end) and (args.wave_end <= 0)
            if run_cycles >= args.wave_end or wave_at_last:
                info("Waveform off at HW cycle = %d (simulated %d cycles)" % (self.dut.xclock.clk, run_cycles))
                self.api_waveform_off()
            return

        # Set trace if interact_at is 0
        if args.interact_at == 0:
            self.set_trace()

        # Warn if no image
        if not args.image:
            warn("No image to execute, Entering the interactive debug mode")
            self.set_trace()

        # Main cycle execution loop
        cycle_batch_size = 10000
        cycle_batch_count = args.max_cycles // cycle_batch_size
        cycle_batch_remain = args.max_cycles % cycle_batch_size
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
                if delta_time > args.max_run_time and args.max_run_time > 0:
                    cycle_reach_max_time = True
                    info(f"Max run time {timesec_to_str(args.max_run_time)} reached (runed {timesec_to_str(delta_time)}), exit cycle execution")
                    break
            return runc
        run_cycles = 0
        for _ in range(cycle_batch_count):
            if not cycle_reach_max_time and not self.api_dut_is_step_exit():
                run_cycles += run_cycle_deta(cycle_batch_size)
            else:
                break
        if not cycle_reach_max_time:
            run_cycles += run_cycle_deta(cycle_batch_remain)
        delta = args.max_cycles - run_cycles
        wave_at_last = (args.wave_begin != args.wave_end) and (args.wave_end <= 0)
        # Check if the waveform is on
        if wave_at_last or args.wave_end >= run_cycles:
            info("Waveform off at HW cycle = %d" % (self.dut.xclock.clk))
            self.api_waveform_off()
        info("Finished cycles: %d (%d ignored)" % (run_cycles, delta))

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

    def set_ui_handler(self, handler):
        self._ui_handler = handler
        xspdb_set_ui_handler(handler)

    def clear_ui_handler(self):
        self._ui_handler = None
        xspdb_set_ui_handler(None)

    def ui_tick(self):
        handler = self._ui_handler
        fn = getattr(handler, "ui_tick", None) if handler else None
        if callable(fn):
            try:
                fn()
            except Exception:
                pass

    def ui_prompt(self, msg=""):
        return ui_prompt(msg)

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

    def api_append_init_cmd(self, cmd):
        """Append a command to the init_cmds list for execution before run"""
        self.init_cmds.append(cmd)

    def api_batch_append_tail_one_cmd(self, cmd):
        """Append a command to the batch execution tail"""
        self.init_cmds.append(cmd)

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

    def do_xui(self, arg):
        """Enter the Text UI interface, or manage XUI config.

        Usage:
            xui
            xui save
            xui load
        """
        sub = (arg or "").strip()
        if sub:
            parts = sub.split(maxsplit=1)
            cmd = parts[0]
            rest = parts[1] if len(parts) > 1 else ""
            if cmd in ("save", "load"):
                return self._ui_config(cmd)
            if cmd == "goto":
                if not rest:
                    warn("Usage: xui goto <address>")
                    return
                try:
                    addr = int(rest, 0)
                except ValueError:
                    error(f"Invalid address: {rest}")
                    return
                self.api_set_info_force_mid_address(addr)
                info(f"force address set to 0x{addr:x}")
                return
            warn(f"Unknown xui subcommand: {sub}")
            return
        if self.in_tui:
            error("Already in TUI")
            return
        try:
            self.tui_ret = None
            self.in_tui = True
            enter_tui(self)
            self.in_tui = False
            self.clear_ui_handler()
            self.interrupt = False
            info("XUI Exited.")
            return self.tui_ret
        except Exception as e:
            import traceback
            self.in_tui = False
            error(f"XUI Error: {e}")
            traceback.print_exc()
            return False

    def do_xtheme(self, arg):
        """Set or list Text UI themes.

        Usage:
            xtheme <name>
            xtheme list
            xtheme cycle
        """
        handler = self._ui_handler
        if not handler:
            warn("TUI not active")
            return
        name = (arg or "").strip()
        fn = getattr(handler, "ui_theme", None)
        if not callable(fn):
            warn("Theme control not available")
            return
        if not name or name in ("list", "ls", "-l", "?"):
            fn("__list__")
            return
        if name in ("cycle", "next", "-n"):
            fn("__cycle__")
            return
        fn(name)

    def complete_xtheme(self, text, line, begidx, endidx):
        opts = ["list", "cycle", "next"]
        return [o for o in opts if o.startswith(text)]

    def _ui_config(self, action: str):
        handler = self._ui_handler
        if not handler:
            warn("TUI not active")
            return
        fn = getattr(handler, "ui_config", None)
        if not callable(fn):
            warn("Config control not available")
            return
        fn(action)

    def complete_xui(self, text, line, begidx, endidx):
        opts = ["save", "load", "goto"]
        return [o for o in opts if o.startswith(text)]

    def _exec_batch_cmds(self, break_handler=None):
        cmd_count = 0
        if not hasattr(self, "batch_depth"):
            self.batch_depth = 0
        self.batch_depth += 1
        try:
            while getattr(self, "batch_cmds_to_exec", []):
                cmd, gap_time, callback = self.batch_cmds_to_exec.pop(0)
                if gap_time:
                    self.api_busy_sleep(gap_time)
                ret = self.onecmd(cmd, log_cmd=False)
                cmd_count += 1
                setattr(self, "__last_batch_cmd_ret__", ret)
                if callback:
                    try:
                        if callback(cmd_count) is False:
                            break
                    except Exception:
                        break
                if break_handler:
                    try:
                        if break_handler(cmd_count) is False:
                            break
                    except Exception:
                        break
        finally:
            self.batch_depth -= 1
            if self.batch_depth < 0:
                self.batch_depth = 0
        return cmd_count

    def do_xcontinue_batch(self, arg):
        """Continue executing loaded batch commands"""
        return self._exec_batch_cmds()

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

    def do_xload_log(self, arg):
        """Load an XSPdb log file

        Args:
            log_file (string): Path to the log file
            delay_time (float): time delay between each cmd
        """
        usage = "usage: xload_log <log_file> [delay_time]"
        if not arg:
            message(usage)
            return
        args = arg.split()
        path = args[0]
        delay = 0
        if len(args) > 1:
            try:
                delay = float(args[1])
            except Exception as e:
                error("Convert dalay fail: %s, from args: %s\n%s" % (e, arg, usage))
        if not os.path.exists(path):
            error(f"log file: {path} not find!")
            return
        cmd_count = self.api_exec_script(path,
                                         gap_time=delay,
                                         target_prefix=self.log_cmd_prefix,
                                         target_subfix=self.log_cmd_suffix)
        if cmd_count >= 0:
            self._exec_batch_cmds()
            is_continue = getattr(self, "__last_batch_cmd_ret__", False)
            message(f"Load log: {path} success, cmd count: {cmd_count}, continue: {is_continue}")
            return is_continue

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
