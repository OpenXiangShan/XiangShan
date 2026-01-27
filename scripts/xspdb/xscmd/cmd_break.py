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
from . import info, error, message, warn, get_completions

class CmdBreak:

    def __init__(self):
        assert hasattr(self, "dut"), "this class must be used in XSPdb, canot be used alone"
        self.xdut_signal_breaks = {}
        self.xdut_expr_breaks = {}
        self._xdut_expr_checker = None
        self._xdut_expr_next_id = 0
        self._xdut_fsm_checker = None
        self._xdut_fsm_name = ""

    def api_xbreak(self, signal_name, condition, value, callback=None, callback_once=False):
        """Set a breakpoint on a signal

        Args:
            signal_name (string): Name of the signal
            condition (string): Condition for the breakpoint: eq, ne, gt, lt, ge, le, ch (or ==, !=, >, <, >=, <=)
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
            "==": self.xsp.ComUseCondCmp_EQ,
            "ne": self.xsp.ComUseCondCmp_NE,
            "!=": self.xsp.ComUseCondCmp_NE,
            "gt": self.xsp.ComUseCondCmp_GT,
            ">": self.xsp.ComUseCondCmp_GT,
            "lt": self.xsp.ComUseCondCmp_LT,
            "<": self.xsp.ComUseCondCmp_LT,
            "ge": self.xsp.ComUseCondCmp_GE,
            ">=": self.xsp.ComUseCondCmp_GE,
            "le": self.xsp.ComUseCondCmp_LE,
            "<=": self.xsp.ComUseCondCmp_LE,
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

    def api_xbreak_expr(self, expr, name=""):
        """Set an expression breakpoint

        Args:
            expr (string): Python-style expression
            name (string): Optional breakpoint name
        Returns:
            string: breakpoint key
        """
        if not expr:
            error("expression is empty")
            return None
        checker = self._xdut_expr_checker
        cb_key = "xdut_expr_break"
        if not checker:
            checker = self.xsp.ComUseExprCheck(self.dut.xclock)
            self.dut.xclock.RemoveStepRisCbByDesc(cb_key)
            self.dut.xclock.StepRis(checker.GetCb(), checker.CSelf(), cb_key)
            self._xdut_expr_checker = checker
        try:
            root = checker.CompileExpr(expr, self.dut.xcfg)
        except Exception as e:
            error(f"xbreak_expr compile failed: {e}")
            return None
        if root is None:
            return None
        if not name:
            name = f"xexpr-{self._xdut_expr_next_id}"
            self._xdut_expr_next_id += 1
        checker.SetExpr(name, root)
        self.xdut_expr_breaks[name] = {"expr": expr}
        return name

    def api_xunbreak_expr(self, key):
        """Remove an expression breakpoint

        Args:
            key (string): breakpoint key
        """
        checker = self._xdut_expr_checker
        if not checker:
            warn("expr checker not found, please set a breakpoint first")
            return
        if key not in self.xdut_expr_breaks:
            warn(f"expr breakpoint {key} not found")
            return
        checker.RemoveExpr(key)
        del self.xdut_expr_breaks[key]
        if not self.xdut_expr_breaks:
            self.dut.xclock.RemoveStepRisCbByDesc("xdut_expr_break")
            assert "xdut_expr_break" not in self.dut.xclock.ListSteRisCbDesc()
            self._xdut_expr_checker = None
            self.xdut_expr_breaks.clear()
            info("No expr breakpoints, remove checker")

    def api_xbreak_expr_list(self):
        """List expression breakpoints"""
        checker = self._xdut_expr_checker
        if not checker or not self.xdut_expr_breaks:
            return []
        status = checker.ListExpr()
        ret = []
        for k in sorted(self.xdut_expr_breaks.keys()):
            hit = False
            if hasattr(status, "get"):
                hit = status.get(k, False)
            else:
                try:
                    hit = status[k]
                except Exception:
                    hit = False
            ret.append((k, hit, self.xdut_expr_breaks[k]["expr"]))
        return ret

    def api_is_xbreak_expr_on(self):
        """Check if expression breakpoint is on"""
        return self._xdut_expr_checker is not None and bool(self.xdut_expr_breaks)

    def api_xbreak_fsm(self, program_text, name=""):
        """Load an FSM trigger program

        Args:
            program_text (string): FSM program text
            name (string): Optional name
        """
        if not program_text:
            error("fsm program is empty")
            return False
        checker = self._xdut_fsm_checker
        cb_key = "xdut_fsm_break"
        if not checker:
            checker = self.xsp.ComUseFsmTrigger(self.dut.xclock)
            self.dut.xclock.RemoveStepRisCbByDesc(cb_key)
            self.dut.xclock.StepRis(checker.GetCb(), checker.CSelf(), cb_key)
            self._xdut_fsm_checker = checker
        try:
            checker.LoadProgram(program_text, self.dut.xcfg)
        except Exception as e:
            error(f"xbreak_fsm load failed: {e}")
            return False
        self._xdut_fsm_name = name or self._xdut_fsm_name or "fsm"
        return True

    def api_xbreak_fsm_file(self, path):
        """Load an FSM trigger program from file"""
        if not os.path.exists(path):
            error(f"file {path} not found")
            return False
        with open(path, "r") as f:
            program = f.read()
        name = os.path.basename(path)
        return self.api_xbreak_fsm(program, name=name)

    def api_xbreak_fsm_clear(self):
        """Clear FSM trigger"""
        checker = self._xdut_fsm_checker
        if not checker:
            warn("fsm checker not found, please load a program first")
            return
        checker.Clear()
        self.dut.xclock.RemoveStepRisCbByDesc("xdut_fsm_break")
        assert "xdut_fsm_break" not in self.dut.xclock.ListSteRisCbDesc()
        self._xdut_fsm_checker = None
        self._xdut_fsm_name = ""
        info("fsm trigger cleared")

    def api_xbreak_fsm_status(self):
        """Get FSM trigger status"""
        checker = self._xdut_fsm_checker
        if not checker:
            return None
        return {
            "name": self._xdut_fsm_name or "fsm",
            "state": checker.GetCurrentState(),
            "triggered": checker.IsTriggered(),
            "trigger_state": checker.GetTriggeredState(),
        }

    def api_is_xbreak_fsm_on(self):
        """Check if FSM breakpoint is on"""
        return self._xdut_fsm_checker is not None

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
        checked_keys = list(checked.keys())
        if checked_keys:
            self._fork_backup_on_break(checked_keys)
            if self._fork_backup_is_child and self._fork_backup_target_breaks:
                if set(checked_keys) & self._fork_backup_target_breaks:
                    self._fork_backup_child_hit = True
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

    def api_get_breaked_names(self):
        """Get the names of the breaked names"""
        names = []
        # api_xbreak_list
        names += [v[0] for v in self.api_xbreak_list() if v[4]]
        # api_xbreak_expr_list
        names += [v[0] for v in self.api_xbreak_expr_list() if v[1]]
        # fsm trigger
        if self._xdut_fsm_checker and self._xdut_fsm_checker.IsTriggered():
            state = self._xdut_fsm_checker.GetTriggeredState()
            names.append(f"FSM:{state}" if state else "FSM")
        # instrunct_istep
        if self.api_break_is_instruction_commit():
            names.append("Inst commit")
        # watch_commit_pc
        if self.api_break_is_watch_commit_pc():
            names.append("Target commit")
        return ",".join(names)

    def do_xbreak(self, arg):
        """Set a breakpoint on a signal

        Args:
            signal_name (string): Name of the signal
            condition (string): Condition for the breakpoint: eq, ne, gt, lt, ge, le, ch (or ==, !=, >, <, >=, <=), default is eq
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
            signal_name = args[0]
            self.api_xbreak(signal_name, condition, value)
        except Exception as e:
            error(f"parse args fail: {str(e)}\n usage: xbreak signal_name [condition] [value]")
            return

    def complete_xbreak(self, text, line, begidx, endidx):
        cmd = text.strip()
        cmd_list = [c for c in line.strip().split() if c]
        if (len(cmd_list) == 2 and line.endswith(" ")) or (len(cmd_list) == 3 and not line.endswith(" ")):
                return [k for k in ["eq", "ne", "gt", "lt", "ge", "le", "ch", "==", "!=", ">", "<", ">=", "<="] if k.startswith(cmd)]
        return get_completions(self.get_dut_tree(), text)

    def do_xbreak_expr(self, arg):
        """Set a breakpoint on an expression

        Args:
            arg (string): Python-style expression
        """
        expr = arg.strip()
        if not expr:
            message("usage: xbreak_expr <expr>")
            return
        key = self.api_xbreak_expr(expr)
        if key:
            info(f"expr break set: {key}")

    def do_xunbreak_expr(self, arg):
        """Remove an expression breakpoint

        Args:
            arg (string): breakpoint key
        """
        key = arg.strip()
        if not key:
            message("usage: xunbreak_expr <key>")
            return
        self.api_xunbreak_expr(key)

    def complete_xunbreak_expr(self, text, line, begidx, endidx):
        return [k for k in self.xdut_expr_breaks.keys() if k.startswith(text)]

    def do_xbreak_expr_list(self, arg):
        """List expression breakpoints"""
        ret = self.api_xbreak_expr_list()
        if not ret:
            message("no expr break")
            return
        for k, hit, expr in ret:
            message(f"{k}: hit={hit} expr={expr}")
        message(f"total {len(ret)} expr breakpoints")

    def do_xbreak_fsm(self, arg):
        """Load an FSM trigger program

        Args:
            arg (string): Path to FSM program file
        """
        path = arg.strip()
        if not path:
            message("usage: xbreak_fsm <fsm_file>")
            return
        if self.api_xbreak_fsm_file(path):
            info(f"fsm loaded: {path}")

    def complete_xbreak_fsm(self, text, line, begidx, endidx):
        return self.api_complite_localfile(text)

    def do_xbreak_fsm_status(self, arg):
        """Show FSM trigger status"""
        status = self.api_xbreak_fsm_status()
        if not status:
            message("fsm not loaded")
            return
        message(f"name={status['name']} state={status['state']} triggered={status['triggered']} trigger_state={status['trigger_state']}")

    def do_xbreak_fsm_clear(self, arg):
        """Clear FSM trigger"""
        self.api_xbreak_fsm_clear()

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
