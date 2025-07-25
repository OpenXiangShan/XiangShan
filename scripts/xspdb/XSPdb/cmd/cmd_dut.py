#coding=utf-8

import os
import time
from XSPdb.cmd.util import info, error, message, warn, get_completions

class CmdDut:
    
    def __init__(self):
        assert hasattr(self, "dut"), "this class must be used in XSPdb, canot be used alone"
        self.interrupt = False
        self.xdut_signal_breaks = {}
        self.api_dut_reset()

    def api_xbreak(self, signal_name, condition, value, callback=None, callback_once=False):
        """Set a breakpoint on a signal

        Args:
            signal_name (string): Name of the signal
            condition (string): Condition for the breakpoint: eq, ne, gt, lt, ge, le, ch
            value (int/string): Value for the breakpoint
            callback (function): Callback function to be called when the breakpoint is hit, args:
                cb(self, checker, k, clk, sig.value, target.value)
            callback_once (bool): Whether to call the callback function only once and remove the breakpoint
        """
        checker = self.xdut_signal_breaks.get("checker")
        checker_key = "xdut_signal_break"
        if not checker:
            checker = self.xsp.ComUseCondCheck(self.dut.xclock)
            self.dut.xclock.RemoveStepRisCbByDesc(checker_key)
            self.dut.xclock.StepRis(checker.GetCb(), checker.CSelf(), checker_key)
            self.xdut_signal_breaks["checker"] = checker
        xbreak_key = "xbreak-%s-%s-%s"%(signal_name, condition, value)
        if xbreak_key in checker.ListCondition():
            error(f"signal {xbreak_key} already set")
            return
        sig = self.dut.GetInternalSignal(signal_name)
        if not sig:
            error(f"first signal {signal_name} not found")
            return
        if isinstance(value, str):
            val = self.dut.GetInternalSignal(value)
            if not val:
                error(f"second signal {value} not found")
                return
        else:
            val = self.xsp.XData(sig.W(), self.xsp.XData.InOut)
            val.value = value
        condition_map = {
            "eq": self.xsp.ComUseCondCmp_EQ,
            "ne": self.xsp.ComUseCondCmp_NE,
            "gt": self.xsp.ComUseCondCmp_GT,
            "lt": self.xsp.ComUseCondCmp_LT,
            "ge": self.xsp.ComUseCondCmp_GE,
            "le": self.xsp.ComUseCondCmp_LE,
            "ch": self.xsp.ComUseCondCmp_NE,
        }
        cmp = condition_map.get(condition.lower(), None)
        if cmp is None:
            error(f"condition '{condition}' not supported")
            return
        checker.SetCondition(xbreak_key, sig, val, cmp)
        self.xdut_signal_breaks[xbreak_key] = {"sig": sig, "val": val, "cmp": condition.lower(), "cb": callback, "cb_once": callback_once}
        return xbreak_key

    def api_xunbreak(self, xbreak_key):
        """Remove a breakpoint on a signal

        Args:
            signal_name (string): Name of the signal
        """
        checker = self.xdut_signal_breaks.get("checker")
        if not checker:
            warn("checker not found, please set a breakpoint first")
            return
        rcount = 0
        kcount = 0
        for k, _ in checker.ListCondition().items():
            kcount += 1
            if k.startswith(xbreak_key):
                checker.RemoveCondition(k)
                del self.xdut_signal_breaks[k]
                info(f"remove signal {k} break")
                rcount +=1
                kcount -= 1
        if kcount == 0:
            self.dut.xclock.RemoveStepRisCbByDesc("xdut_signal_break")
            assert "xdut_signal_break" not in self.dut.xclock.ListSteRisCbDesc()
            self.xdut_signal_breaks.clear()
            info("No signal to watch, remove checker")
        if rcount > 0:
            return
        warn(f"signal {xbreak_key} not found, please set a breakpoint first")

    def api_xbreak_update_ch(self):
        """Update the condition of the signal breakpoint"""
        checker = self.xdut_signal_breaks.get("checker")
        if not checker:
            warn("checker not found, please set a breakpoint first")
            return
        for k, v in self.xdut_signal_breaks.items():
            if not k.startswith("xbreak-"):
                continue
            if v["cmp"] == "ch":
                v["val"].value = v["sig"].value
                info(f"update signal {k}, save value {v['val'].value}")

    def api_xbreak_clear(self):
        """Clear all breakpoints"""
        checker = self.xdut_signal_breaks.get("checker")
        if not checker:
            warn("checker not found, please set a breakpoint first")
            return
        checker.ClearCondition()
        self.dut.xclock.RemoveStepRisCbByDesc("xdut_signal_break")
        assert "xdut_signal_break" not in self.dut.xclock.ListSteRisCbDesc()
        self.xdut_signal_breaks.clear()
        info("clear all signal breakpoints")

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

    def do_xunwatch(self, arg):
        """Remove a watch variable

        Args:
            arg (string): Variable name
        """
        key = arg.strip()
        if key in self.info_watch_list:
            del self.info_watch_list[key]
        else:
            error(f"watch {key} not found")

    def complete_xwatch(self, text, line, begidx, endidx):
        cmp = get_completions(self.get_dut_tree(), text)
        return cmp

    def complete_xunwatch(self, text, line, begidx, endidx):
        return [k for k in self.info_watch_list.keys() if k.startswith(text)]

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

    def do_xbreak(self, arg):
        """Set a breakpoint on a signal

        Args:
            signal_name (string): Name of the signal
            condition (string): Condition for the breakpoint: eq, ne, gt, lt, ge, le, ch, default is eq
            value (int): Value for the breakpoint, default is 0
        """
        args = arg.strip().split()
        value = 0
        condition = "eq"
        try:
            if len(args) > 2:
                try:
                    value = int(args[2], 0)
                except Exception as e:
                    value = args[2]
            if len(args) > 1:
                condition = args[1]
            if len(args) < 1:
                message("usage: xbreak signal_name [condition] [value]")
                return
            if not condition in ["eq", "ne", "gt", "lt", "ge", "le", "ch"]:
                message("condition must be eq, ne, gt, lt, ge, le, ch")
                return
            signal_name = args[0]
            self.api_xbreak(signal_name, condition, value)
        except Exception as e:
            error(f"parse args fail: {str(e)}\n usage: xbreak signal_name [condition] [value]")
            return

    def complete_xbreak(self, text, line, begidx, endidx):
        cmd = text.strip()
        cmd_list = [c for c in line.strip().split() if c]
        if (len(cmd_list) == 2 and line.endswith(" ")) or (len(cmd_list) == 3 and not line.endswith(" ")):
                return [k for k in ["eq", "ne", "gt", "lt", "ge", "le", "ch"] if k.startswith(cmd)]
        return get_completions(self.get_dut_tree(), text)

    def do_xunbreak(self, arg):
        """Remove a breakpoint on a signal

        Args:
            signal_name (string): Name of the signal
        """
        if not arg.strip():
            message("do you want to clear all breakpoints? (y/n):")
            while True:
                fc = getattr(self, "on_update_tstep", None)
                if fc:
                    fc()
                ans = input()
                if ans == "y":
                    self.api_xbreak_clear()
                    break
                elif ans == "n":
                    break
                else:
                    message("please input y or n")
            return
        if arg.strip() == "all":
            self.api_xbreak_clear()
            return
        self.api_xunbreak(arg.strip())

    def complete_xunbreak(self, text, line, begidx, endidx):
        if not text:
            return [k for k in self.xdut_signal_breaks.keys() if k.startswith("xbreak-")]
        return [k for k in self.xdut_signal_breaks.keys() if k.startswith(text) and k != "xdut_signal_break"]

    def do_xbreak_list(self, arg):
        """List all breakpoints"""
        ret = self.api_xbreak_list()
        if not ret:
            message("no signal break")
            return
        for br in ret:
            message(f"{br[0]}(0x{br[1]:x}) {br[2]} 0x{br[3]:x} hinted: {br[4]}")
        message(f"total {len(ret)} breakpoints")

    def do_xbreak_update(self, arg):
        """Update the condition of the signal breakpoint"""
        self.api_xbreak_update_ch()
        info("update signal break condition complete")
