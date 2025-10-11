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
from . import message, error, warn, info, GREEN, RESET

class CmdDiffTest:

    def __init__(self):
        assert hasattr(self, "difftest_stat"), "difftest_stat not found"
        self.condition_watch_commit_pc = {}    
        self.condition_instrunct_istep = {}
        self.difftest_ref_so = self.xsp.CString()
        self.difftest_ref_is_inited = False
        self.difftest_diff_checker = {}
        self.difftest_diff_is_run = False
        self.istep_last_commit_pc = []
        self.data_last_symbol_block = -1
        self.data_last_symbol_pc = -1

    def api_load_ref_so(self, so_path):
        """Load the difftest reference shared object

        Args:
            so_path (string): Path to the shared object
        """
        if not os.path.exists(so_path):
            error(f"file {so_path} not found")
            return False
        self.difftest_ref_so.Set(so_path)
        self.df.SetProxyRefSo(self.difftest_ref_so.CharAddress())
        info(f"load difftest ref so: {so_path} complete")
        return True

    def api_get_ref_so_path(self):
        """Get the path of the difftest reference shared object

        Returns:
            string: Path to the shared object
        """
        return self.difftest_ref_so.Get()

    def api_init_ref(self, force=False):
        """Initialize the difftest reference"""
        if self.difftest_ref_is_inited:
            if not force:
                error("difftest reference already inited")
                return False
        if self.difftest_ref_so.Get() == "":
            error("difftest reference so not loaded")
            return False
        if not self.mem_inited:
            error("mem not loaded, please load bin file to mem first")
            return False
        if force and self.difftest_ref_is_inited:
            self.df.finish_device()
            self.df.GoldenMemFinish()
            self.df.difftest_finish()
            self.df.difftest_init()
            self.difftest_stat = self.df.GetDifftest(0).dut
        self.df.init_device()
        self.df.GoldenMemInit()
        self.df.init_nemuproxy(0)
        self.difftest_ref_is_inited = True
        return True

    def api_set_difftest_diff(self, turn_on):
        """Initialize the difftest diff"""
        if not self.api_init_ref():
            return False
        checker = self.difftest_diff_checker.get("checker")
        if not checker:
            checker = self.xsp.ComUseCondCheck(self.dut.xclock)
            tmp_dat = self.xsp.ComUseDataArray(4)
            checker.SetCondition("diff_test_do_diff_check", tmp_dat.BaseAddr(), tmp_dat.BaseAddr(),
                                 self.xsp.ComUseCondCmp_NE, 4, 0, 0, 0,
                                 checker.AsPtrXFunc(self.df.GetFuncAddressOfDifftestStepAndCheck()),
                                 0)
            self.difftest_diff_checker["checker"] = checker
        key = "diff_test_do_diff_check"
        self.dut.xclock.RemoveStepRisCbByDesc(key)
        self.difftest_diff_is_run = False
        if turn_on:
            self.dut.xclock.StepRis(checker.GetCb(), checker.CSelf(), key)
            self.difftest_diff_is_run = True
            info("turn on difftest diff")
        else:
            info("turn off difftest diff")
        return True

    def api_is_difftest_diff_exit(self, show_log=False):
        """Check if the difftest diff has exited

        Returns:
            bool: True if exited, False otherwise
        """
        if not self.difftest_diff_is_run:
            return False
        stat = self.df.GetDifftestStat()
        if stat == -1:
            return False
        if show_log:
            message(f"{GREEN}Difftest run exit with code: {stat} {RESET}")
        return True

    def api_is_difftest_diff_run(self):
        """Check if the difftest diff is running"""
        return self.difftest_diff_is_run

    def do_xload_difftest_ref_so(self, arg):
        """Load the difftest reference shared object

        Args:
            arg (string): Path to the shared object
        """
        if not arg.strip():
            error("difftest ref so path not found\n usage: xload_difftest_ref_so <path>")
            return
        if not self.api_load_ref_so(arg):
            error(f"load difftest ref so {arg} failed")
            return

    def complete_xload_difftest_ref_so(self, text, line, begidx, endidx):
        return self.api_complite_localfile(text)

    def api_difftest_reset(self):
        """Reset the difftest"""
        if self.difftest_ref_is_inited:
            if self.api_init_ref(force=True):
                info("difftest reset success")
                return True
            else:
                error("difftest reset failed")
                return False
        return True

    def do_xdifftest_reset(self, arg):
        """Reset the difftest

        Args:
            arg (None): No arguments
        """
        self.api_difftest_reset()

    def api_commit_pc_list(self):
        """Get the list of all commit PCs

        Returns:
        list((pc, valid)): List of PCs
        """
        index = 0
        pclist=[]
        while True:
            cmt = self.difftest_stat.get_commit(index)
            if cmt:
                pclist.append((cmt.pc, cmt.valid))
                index += 1
            else:
                break
        return pclist

    def do_xpc(self, a):
        """Print the current Commit PCs and instructions

        Args:
            a (None): No arguments
        """
        for i in range(8):
            cmt = self.difftest_stat.get_commit(i)
            message(f"PC[{i}]: 0x{cmt.pc:x}    Instr: 0x{cmt.instr:x}")

    def api_istep_update_commit_pc(self):
        old_p = self.condition_instrunct_istep.get("pc_old_list")
        new_P = self.condition_instrunct_istep.get("pc_lst_list")
        if not (old_p and new_P):
            self.istep_last_commit_pc = []
        else:
            self.istep_last_commit_pc = []
            for i in range(8):
                old_pc = int.from_bytes(old_p[i].AsBytes(), byteorder='little', signed=False)
                new_pc = int.from_bytes(new_P[i].AsBytes(), byteorder='little', signed=False)
                if old_pc != new_pc:
                    self.istep_last_commit_pc.append(new_pc)

    def comuse_checker_is_break(self, checker):
        if not checker:
            return False
        if {k: v for (k, v) in checker.ListCondition().items() if v}:
            return True
        return False

    def api_break_is_instruction_commit(self):
        """check break  is instruction commit or not"""
        checker = self.condition_instrunct_istep.get("checker")
        return self.comuse_checker_is_break(checker)

    def api_break_is_watch_commit_pc(self):
        """check break  is watch commit pc or not"""
        checker = self.condition_watch_commit_pc.get("checker")
        return self.comuse_checker_is_break(checker)

    def api_get_istep_last_commit_pc(self):
        """Get the last commit PC after instruction step

        Returns:
            list: List of commit PCs
        """
        return self.istep_last_commit_pc.copy()

    def api_xistep(self, instr_count):
        """Step through instructions, stop when find instruction commit

        Args:
            step_count (int): Number of steps to take
        Returns:
            step_taken (int)
        """
        self.api_xistep_break_on()
        update_pc_func = self.condition_instrunct_istep["pc_sync_list"]
        update_pc_func()
        step_taken = 0
        for i in range(instr_count):
            update_pc_func()
            v = self.api_step_dut(10000)
            if self.api_dut_is_step_exit():
                break
            if self.interrupt:
                break
            elif self.dut.xclock.IsDisable():
                self.api_istep_update_commit_pc()
                pc = max(self.api_get_istep_last_commit_pc() + [-1])
                self.data_last_symbol_block = self.api_echo_pc_symbol_block_change(pc,
                                                                                   self.data_last_symbol_block,
                                                                                   self.data_last_symbol_pc)
                self.data_last_symbol_pc = pc
            elif v == 10000:
                warn("step %d cycles complete, but no instruction commit find" % v)
                step_taken -= 1 # ignore record
            step_taken += 1
        # remove stepi_check
        self.api_xistep_break_off()
        return step_taken

    def api_xistep_break_on(self):
        """Set the instruction step break condition"""
        if not self.condition_instrunct_istep:
            checker = self.xsp.ComUseCondCheck(self.dut.xclock)
            self.condition_instrunct_istep["checker"] = checker
            pc_old_list = [self.xsp.ComUseDataArray(8) for i in range(8)]
            pc_lst_list = [self.xsp.ComUseDataArray(self.difftest_stat.get_commit(i).get_pc_address(), 8) for i in range(8)]
            # sync pc and add checker
            for i, opc in enumerate(pc_old_list):
                lpc = pc_lst_list[i]
                opc.SyncFrom(lpc.BaseAddr(), 8)
                checker.SetCondition("stepi_check_pc_%d" % i, lpc.BaseAddr(), opc.BaseAddr(), self.xsp.ComUseCondCmp_NE, 8)
            self.condition_instrunct_istep["pc_old_list"] = pc_old_list
            self.condition_instrunct_istep["pc_lst_list"] = pc_lst_list
            def _update_old_pc():
                for i, opc in enumerate(pc_old_list):
                    lpc = pc_lst_list[i]
                    opc.SyncFrom(lpc.BaseAddr(), 8)
            self.condition_instrunct_istep["pc_sync_list"] = _update_old_pc
        cb_key = "stepi_check"
        checker = self.condition_instrunct_istep["checker"]
        if cb_key in self.dut.xclock.ListSteRisCbDesc():
            return
        self.dut.xclock.StepRis(checker.GetCb(), checker.CSelf(), cb_key)

    def api_xistep_break_off(self):
        """Remove the instruction step break condition"""
        cb_key = "stepi_check"
        if cb_key in self.dut.xclock.ListSteRisCbDesc():
            self.dut.xclock.RemoveStepRisCbByDesc(cb_key)
        assert cb_key not in self.dut.xclock.ListSteRisCbDesc()

    def do_xistep_break(self, arg):
        """Set the instruction step break condition

        Args:
            on_or_off (str): "on" or "off"
        """
        if arg.strip() == "on":
            self.api_xistep_break_on()
        elif arg.strip() == "off":
            self.api_xistep_break_off()
        else:
            error("usage: xistep_break <on|off>")

    def complete_xistep_break(self, text, line, begidx, endidx):
        return [x for x in ["on", "off"] if x.startswith(text)] if text else ["on", "off"]

    def do_xistep(self, arg):
        """Step through instructions, stop when find instruction commit

        Args:
            step_count (int): Number of steps to take
        """
        arg = arg.strip()
        instr_count = 1
        try:
            instr_count = 1 if not arg else int(arg)
        except Exception as e:
            error(f"convert {arg} to number fail: {str(e)}")
            return
        self.api_xistep(instr_count)

    def api_difftest_get_instance(self, instance=0):
        """Get the difftest instance

        Args:
            instance (number): difftest instance to get, default is 0
        """
        return self.df.GetDifftest(instance)

