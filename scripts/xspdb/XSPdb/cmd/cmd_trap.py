#coding=utf-8

from XSPdb.cmd.util import message, warn, GREEN, RESET, info

class CmdTrap:
    """Trap command class
    """

    def __init__(self):
        assert hasattr(self, "dut"), "this class must be used in XSPdb, canot be used alone"
        self.condition_good_trap = {}
        self.break_on_trap = {}

    def api_init_good_trap(self):
        """Initialize the good trap"""
        checker = self.condition_good_trap.get("checker")
        if checker:
            return
        if hasattr(self.difftest_stat.trap, "get_code_address"):
            checker = self.xsp.ComUseCondCheck(self.dut.xclock)
            target_trap_vali = self.xsp.ComUseDataArray(1)
            target_trap_code = self.xsp.ComUseDataArray(8)
            target_trap_vali.FromBytes(int(0).to_bytes(1, byteorder='little', signed=False))
            target_trap_code.FromBytes(int(0).to_bytes(8, byteorder='little', signed=False)) #FIXME: is the good trap code zero ?
            source_trap_code = self.xsp.ComUseDataArray(self.difftest_stat.trap.get_code_address(), 8)
            source_trap_vali = self.xsp.ComUseDataArray(self.difftest_stat.trap.get_hasTrap_address(), 1)
            checker.SetCondition("good_trap", source_trap_code.BaseAddr(), target_trap_code.BaseAddr(), self.xsp.ComUseCondCmp_EQ, 8,
                                 source_trap_vali.BaseAddr(), target_trap_vali.BaseAddr(), 1)
            checker.SetValidCmpMode("good_trap", self.xsp.ComUseCondCmp_NE)
        else:
            warn("trap.get_code_address not found, please build the latest difftest-python")
            return
        trap_key = "good_trap"
        self.dut.xclock.RemoveStepRisCbByDesc(trap_key)
        self.dut.xclock.StepRis(checker.GetCb(), checker.CSelf(), trap_key)
        self.condition_good_trap["checker"] = checker

    def api_disable_good_trap(self, disable):
        """disable good trap
        Args:
            disable (bool): Whether to disable good trap
        """
        if disable:
            checker = self.condition_good_trap.get("checker")
            if checker:
                self.dut.xclock.RemoveStepRisCbByDesc("good_trap")
                self.condition_good_trap.clear()
        else:
            self.api_init_good_trap()

    def api_is_hit_good_trap(self, show_log=False):
        """Check if the good trap is hit

        Returns:
            bool: Whether the good trap is hit
        """
        trap = self.difftest_stat.trap
        if trap.hasTrap != 0 and trap.code == 0:
            if show_log:
                message(f"{GREEN}HIT GOOD TRAP at pc = 0x{trap.pc:x} cycle = 0x{trap.cycleCnt:x} {RESET}")
            return True
        return False

    def api_is_hit_good_loop(self, show_log=False):
        """Check if the good trap is hit

        Args:
            show_log (bool): Whether to show the log
        Returns:
            bool: Whether the good trap is hit
        """
        for i in range(8):
            cmt = self.difftest_stat.get_commit(i)
            if cmt and cmt.valid:
                if cmt.instr == 0x6f:
                    if show_log:
                        message(f"{GREEN}HIT GOOD LOOP at pc = 0x{cmt.pc:x}{RESET}")
                    return True
        return False

    def api_break_on_trap(self, on):
        """Set breakpoint on trap

        Args:
            on (bool): Whether to set breakpoint on trap
        """
        check = self.break_on_trap.get("checker")
        if not check:
            check = self.xsp.ComUseCondCheck(self.dut.xclock)
            target_trap_vali = self.xsp.ComUseDataArray(1)
            target_trap_vali.SetZero()
            source_trap_vali = self.xsp.ComUseDataArray(self.difftest_stat.trap.get_hasTrap_address(), 1)
            check.SetCondition("break_on_trap", source_trap_vali.BaseAddr(), target_trap_vali.BaseAddr(), self.xsp.ComUseCondCmp_EQ, 1)
            self.break_on_trap["checker"] = check
        trap_key = "break_on_trap"
        self.break_on_trap["on"] = on
        if on:
            self.dut.xclock.RemoveStepRisCbByDesc(trap_key)
            self.dut.xclock.StepRis(check.GetCb(), check.CSelf(), trap_key)
        else:
            self.dut.xclock.RemoveStepRisCbByDesc(trap_key)

    def api_is_trap_break_on(self):
        """Check if the trap is break on

        Returns:
            bool: Whether the trap is break on
        """
        return self.break_on_trap.get("on", False)

    def api_is_hit_trap_break(self, show_log=False):
        """Check if the trap is break

        Args:
            show_log (bool): Whether to show the log
        Returns:
            bool: Whether the trap is break
        """
        trap = self.difftest_stat.trap
        if trap.hasTrap != 0 and self.api_is_trap_break_on():
            if show_log:
                message(f"{GREEN}HIT TRAP BREAK pc: 0x{trap.pc:x} code: 0x{trap.code:x} hasWFI: {trap.hasWFI}{RESET}")
            return True
        return False

    def api_get_trap_info(self):
        """Get trap information

        Returns:
            dict: Trap information
        """
        trap = self.difftest_stat.trap
        return {
            "pc": trap.pc,
            "code": trap.code,
            "hasTrap": trap.hasTrap,
            "cycleCnt": trap.cycleCnt,
            "hasWFI": trap.hasWFI
        }

    def do_xgood_trap_disable(self, arg):
        """Disable good trap

        Args:
            arg (bool): Whether to disable good trap
        """
        disable = True
        if arg.strip():
            if arg.lower() == "false":
                disable = False
            elif arg.lower() == "true":
                disable = True
            else:
                warn(f"arg {arg} is not true or false\n usage: xgood_trap_disable [true|false]")
                return
        self.api_disable_good_trap(disable)
        if disable:
            info("good trap is disabled")
        else:
            info("good trap is enabled")

    def complete_xgood_trap_disable(self, text, line, begidx, endidx):
        return [x for x in ["true", "false"] if x.startswith(text)] if text else ["true", "false"]

    def do_xtrap_break_on(self, arg):
        """Set breakpoint on trap

        Args:
            arg (None): No arguments
        """
        self.api_break_on_trap(True)
        info("trap break on")

    def do_xtrap_break_off(self, arg):
        """Unset breakpoint on trap
        Args:
            arg (None): No arguments
        """
        self.api_break_on_trap(False)
        info("trap break off")

    def do_xtrap_info(self, arg):
        """Print trap information

        Args:
            arg (None): No arguments
        """
        info = self.api_get_trap_info()
        message(f"trap pc: 0x{info['pc']:x}  code: 0x{info['code']:x}  hasTrap: {info['hasTrap']}  cycle: 0x{info['cycleCnt']:x} hasWFI: {info['hasWFI']}")
