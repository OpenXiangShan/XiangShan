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
import time
from . import info, error, message, warn, get_completions

class CmdDut:
    
    def __init__(self):
        assert hasattr(self, "dut"), "this class must be used in XSPdb, canot be used alone"
        self.interrupt = False
        self.xdut_signal_breaks = {}
        self.api_dut_reset()

    def api_xbreak_list(self):
        """List all breakpoints"""
        ret = []
        checker = self.xdut_signal_breaks.get("checker")
        if not checker:
            return ret
        checked = {k: v for (k, v) in checker.ListCondition().items()}
        for k, v in self.xdut_signal_breaks.items():
            if not k.startswith("xbreak-"):
                continue
            ret.append((k, v["sig"].value, v["cmp"], v["val"].value, checked[k]))
        ret.sort(key=lambda x: x[0])
        return ret

    def call_break_callbacks(self):
        checker = self.xdut_signal_breaks.get("checker")
        cb_count = 0
        if not checker:
            return cb_count
        callbacks = {}
        for k, v in self.xdut_signal_breaks.items():
            if not k.startswith("xbreak-"):
                continue
            if not callable(v["cb"]):
                continue
            callbacks[k] = (v["cb"], v["cb_once"])
        if not callbacks:
            return cb_count
        checked = {k: v for (k, v) in checker.ListCondition().items() if v}
        for k, (cb, once) in callbacks.items():
            if k not in checked:
                continue
            cb(self, checker, k, self.dut.xclock.clk, self.xdut_signal_breaks[k]["sig"].value, self.xdut_signal_breaks[k]["val"].value)
            if once:
                checker.RemoveCondition(k)
                info(f"remove signal {k} break, because callback_once is True")
                del self.xdut_signal_breaks[k]
            cb_count += 1
        if not {k: v for (k, v) in checker.ListCondition().items()}:
            checker.ClearCondition()
            self.dut.xclock.RemoveStepRisCbByDesc("xdut_signal_break")
            assert "xdut_signal_break" not in self.dut.xclock.ListSteRisCbDesc()
            self.xdut_signal_breaks.clear()
        return cb_count

    def api_is_xbreak_on(self):
        """Check if the breakpoint is on"""
        checker = self.xdut_signal_breaks.get("checker")
        if checker:
            return True
        return False

    def api_dut_is_step_exit(self):
        """Check if the step is exit"""
        return any([self.api_is_difftest_diff_exit(show_log=False),
                    self.api_is_hit_good_trap(show_log=False),
                    self.api_is_hit_good_loop(show_log=False),
                    ])

    def api_get_breaked_names(self):
        """Get the names of the breaked names"""
        names = []
        # api_xbreak_list
        names += [v[0] for v in self.api_xbreak_list() if v[4]]
        # instrunct_istep
        if self.api_break_is_instruction_commit():
            names.append("Inst commit")
        # watch_commit_pc
        if self.api_break_is_watch_commit_pc():
            names.append("Target commit")
        return ",".join(names)

    def api_dut_step_ready(self):
        """Prepare the DUT for stepping"""
        self.dut.xclock.Enable()
        assert not self.dut.xclock.IsDisable(), "clock is disable"
        self.interrupt = False # interrupt from outside by user

    def api_step_dut(self, cycle, batch_cycle=200):
        """Step through the circuit

        Args:
            cycle (int): Number of cycles
            batch_cycle (int): Number of cycles per run; after each run, check for interrupt signals
        """
        if not self.mem_inited:
            warn("mem not inited, please load bin file first")
        def check_break():
            if self.dut.xclock.IsDisable():
                info("Find break point (%s), break (step %d cycles) at cycle: %d (%s)" % (
                    self.api_get_breaked_names(),
                    self.dut.xclock.clk - c_count,
                    self.dut.xclock.clk, hex(self.dut.xclock.clk)))
                return True
            fc = getattr(self, "on_update_tstep", None)
            if fc:
                fc()
            if self.api_is_difftest_diff_exit(show_log=True):
                return True
            elif self.api_is_hit_good_trap(show_log=True):
                return True
            elif self.api_is_hit_good_loop(show_log=True):
                return True
            elif self.api_is_hit_trap_break(show_log=True):
                return True
            return False
        self.api_dut_step_ready()
        batch, offset = cycle//batch_cycle, cycle % batch_cycle
        c_count = self.dut.xclock.clk
        need_break = False
        for i in range(batch):
            if self.interrupt:
                break
            self.dut.Step(batch_cycle)
            if check_break():
                need_break = True
                break
        if not self.interrupt and not need_break:
            self.dut.Step(offset)
            check_break()
        if self.dut.xclock.IsDisable():
            self.call_break_callbacks()
        return self.dut.xclock.clk - c_count

    def api_dut_reset(self):
        """Reset the DUT"""
        for i in range(8):
            cmt = self.difftest_stat.get_commit(i)
            cmt.pc = 0x0
            cmt.instr = 0x0
        self.difftest_stat.trap.pc = 0x0
        self.difftest_stat.trap.code = 32
        self.difftest_stat.trap.hasTrap = 0
        self.dut.reset.AsImmWrite()
        self.dut.reset.value = 1
        self.dut.reset.AsRiseWrite()
        self.dut.reset.value = 1
        self.dut.Step(100)
        self.dut.reset.value = 0
        info("reset dut complete")

    def api_dut_flash_load(self, flash_file):
        """Load a bin file into Flash

        Args:
            flash_file (string): Path to the bin file
        """
        assert os.path.exists(flash_file)
        self.df.flash_finish()
        self.df.InitFlash(flash_file)
        self.flash_bin_file = flash_file
        self.info_cache_asm.clear()

    def do_xreset(self, arg):
        """Reset DUT

        Args:
            arg (None): No arguments
        """
        self.api_dut_reset()
        self.api_difftest_reset()

    def do_xwatch(self, arg):
        """Add a watch variable

        Args:
            arg (string): Variable name
        """
        key = arg.strip().split()
        if not key:
            for k, v in self.info_watch_list.items():
                message(f"{k}({v.W()}): 0x{v.value}")
            return
        arb = key[-1]
        sig = self.dut.GetInternalSignal(key[0])
        if sig:
            self.info_watch_list[arb] = sig

    def do_xset(self, arg):
        """Set the value of an internal signal

        Args:
            name (string): Name of the internal signal
            value (int): Value of the internal signal
        """
        args = arg.strip().split()
        if len(args) < 2:
            error("need args format: name value")
            return
        pin_name, pin_value = args[0], args[1]
        try:
            pin_value = int(pin_value)
        except Exception as e:
            error(f"convert {args[1]} to number fail: {str(e)}")
            return
        pin = self.dut.GetInternalSignal(pin_name)
        if pin:
            pin.AsImmWrite()
            pin.value = pin_value

    def complete_xset(self, text, line, begidx, endidx):
        cmp = get_completions(self.get_dut_tree(), text)
        return cmp

    def do_xstep(self, arg):
        """Step through the circuit

        Args:
            cycle (int): Number of cycles
            steps (int): Number of cycles per run; after each run, check for interrupt signals
        """
        try:
            steps = 200
            cycle = arg.strip().split()
            if len(cycle) > 1:
                steps = int(cycle[1])
            if len(cycle) > 0:
                cycle = int(cycle[0])
            else:
                cycle = 1
            rcycles = self.api_step_dut(cycle, steps)
            info(f"step {rcycles} cycles complete" + (f" ({cycle - rcycles} cycle missed)" if rcycles != cycle else ""))
        except Exception as e:
            error(e)
            message("usage: xstep [cycle] [<steps>]")

    def do_xprint(self, arg):
        """Print the value and width of an internal signal

        Args:
            arg (string): Name of the internal signal
        """
        sig = self.dut.GetInternalSignal(arg)
        if sig:
            message(f"value: {hex(sig.value)}  width: {sig.W()}")

    def complete_xprint(self, text, line, begidx, endidx):
        cmp = get_completions(self.get_dut_tree(), text)
        return cmp

