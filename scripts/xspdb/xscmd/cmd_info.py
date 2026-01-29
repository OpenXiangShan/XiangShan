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


import bisect
from collections import OrderedDict
from . import error, info

class CmdInfo:
    """Info command class
    """

    def __init__(self):
        assert hasattr(self, "dut"), "this class must be used in XSPdb, canot be used alone"
        self.info_cache_asm = {}
        self.info_cache_bsz = 256
        self.info_cached_cmpclist = None
        self.info_watch_list = OrderedDict()
        self.info_force_address = None
        self.info_last_address = None
        self.info_prev_pc_last = None
        self.info_cache_order = []
        self.info_cache_limit = 32
        self.info_last_offset = 0

    def api_increase_info_force_address(self, deta):
        """Increase the force mid address for disassembly Info (the disassembly info in the TUI window)

        Args:
            deta (int): Address to increase
        """
        if self.info_force_address is None:
            return
        self.info_force_address += deta
        return self.info_force_address

    def api_get_last_info_mid_address(self):
        """Get the last mid address of disassembly info

        Returns:
            int: Address
        """
        if self.info_last_address is not None:
            return self.info_last_address
        return self.mem_base

    def api_set_info_force_mid_address(self, val):
        """Set the force mid address for disassembly Info (the disassembly info in the TUI window)

        Args:
            val (int): Address to force disassembly
        """
        if val is not None:
            self.info_force_address = val
        else:
            self.info_force_address = None

    def api_get_info_force_mid_address(self):
        """Get the force mid address for disassembly Info (the disassembly info in the TUI window)

        Returns:
            int: Address
        """
        return self.info_force_address

    def api_get_info_last_offset(self):
        """Get the last effective offset for disassembly Info."""
        return self.info_last_offset

    def _info_cache_get(self, index):
        if index in self.info_cache_asm:
            return self.info_cache_asm[index]
        asm_data = self.api_all_data_to_asm(index, self.info_cache_bsz)
        self.info_cache_asm[index] = asm_data
        self.info_cache_order.append(index)
        if len(self.info_cache_order) > self.info_cache_limit:
            old = self.info_cache_order.pop(0)
            if old in self.info_cache_asm:
                del self.info_cache_asm[old]
        return asm_data

    def api_info_get_last_commit_pc(self):
        """Get the last commit PC

        Returns:
            int: Address
        """
        istep_pc = self.api_get_istep_last_commit_pc()
        if len(istep_pc) != 0:
            return max(istep_pc)
        valid_pc_list = [x[0] for x in self.api_commit_pc_list() if (x[1] or self.api_is_difftest_diff_run())]
        return max(valid_pc_list) if valid_pc_list else self.mem_base

    def do_xset_dasm_info_force_mid_address(self, arg):
        """Set the force mid address for disassembly Info (the disassembly info in the TUI window)

        Args:
            arg (number or empty): Address to force disassembly
        """
        if not arg.strip():
            info("reset dasm info force address to None")
            self.api_set_info_force_mid_address(None)
            return
        try:
            self.api_set_info_force_mid_address(int(arg, 0))
            info(f"force address set to 0x{self.info_force_address:x}")
        except ValueError:
            error(f"Invalid address: {arg}")
            
    def api_asm_info(self, size, offset_lines=0):
        """Get the current memory disassembly

        Args:
            size (int, int): Width, height = size

        Returns:
            list[string]: Disassembly list
        """
        # size: w, h
        _, h = size
        base_addr = self.mem_base
        pc_list = self.api_commit_pc_list()
        # ignore valid check when difftest is run
        valid_pc_list = [x[0] for x in pc_list if (x[1] or self.api_is_difftest_diff_run())]
        pc_last = base_addr

        if self.info_cached_cmpclist:
            new_pc = [x[0] for x, y in zip(pc_list, self.info_cached_cmpclist) if x[0] != y[0] and x[1] != 0]
            if new_pc:
                pc_last = max(new_pc)

        if pc_last == base_addr and valid_pc_list:
            pc_last = max(valid_pc_list)

        if self.info_force_address:
            pc_last = self.info_force_address - self.info_force_address % 2

        # Reset follow offset when PC changes (only in follow mode).
        if self.info_force_address is None and self.info_prev_pc_last is not None and pc_last != self.info_prev_pc_last:
            offset_lines = 0

        # Disable cross-call cache in follow mode to keep asm fresh.
        if self.info_force_address is None:
            self.info_cache_asm.clear()
            self.info_cache_order.clear()

        self.info_last_address = pc_last
        self.info_prev_pc_last = pc_last
        self.info_cached_cmpclist = pc_list.copy()
        # Check the cache first; if not found, generate it
        cache_index = pc_last - pc_last % self.info_cache_bsz
        asm_data = self._info_cache_get(cache_index)
        min_index = cache_index
        max_index = cache_index
        min_block = base_addr - (base_addr % self.info_cache_bsz)
        expand_limit = 16
        for _ in range(expand_limit):
            address_list = [x[0] for x in asm_data]
            pc_last_index = bisect.bisect_left(address_list, pc_last)
            desired_start = pc_last_index - h // 2 + int(offset_lines or 0)
            desired_end = desired_start + h
            expanded = False
            if desired_start < 0 and (min_index - self.info_cache_bsz) >= min_block:
                min_index -= self.info_cache_bsz
                asm_data = self.api_merge_asm_list_overlap_append(self._info_cache_get(min_index), asm_data)
                expanded = True
            if desired_end > len(asm_data):
                max_index += self.info_cache_bsz
                asm_data = self.api_merge_asm_list_overlap_append(asm_data, self._info_cache_get(max_index))
                expanded = True
            if not expanded:
                break

        # Quickly locate the position of pc_last
        address_list = [x[0] for x in asm_data]
        pc_last_index = bisect.bisect_left(address_list, pc_last)
        total_lines = len(asm_data)
        start_line = pc_last_index - h // 2 + int(offset_lines or 0)
        if total_lines <= h:
            start_line = 0
        else:
            start_line = max(0, min(start_line, total_lines - h))
        self.info_last_offset = start_line - (pc_last_index - h // 2)
        asm_lines = []
        window = asm_data[start_line:start_line + h]
        if not window:
            return asm_lines
        addr_w = max(8, max(len(f"{x[0]:x}") for x in window))
        byte_w = max(8, max(len(str(x[1])) for x in window))
        mn_w = max(6, max(len(str(x[2])) for x in window))
        for l in window:
            find_pc = l[0] in valid_pc_list
            line = "%s0x%0*x: %-*s  %-*s  %s" % (
                ">" if find_pc else " ",
                addr_w,
                l[0],
                byte_w,
                l[1],
                mn_w,
                l[2],
                l[3],
            )
            if find_pc and l[0] == pc_last:
                line = ("norm_red", line)
            if self.info_force_address is not None:
                end_addr = l[0] + (2 if "c." in l[2] else 4)
                if l[0] <= self.info_force_address < end_addr:
                    line = ("light blue", line)
            asm_lines.append(line)
        return asm_lines

    def api_abs_info(self, size):
        """Get the current status summary information, such as general-purpose registers

        Args:
            size (int, int): Width, height = size

        Returns:
            list[string]: Status list
        """
        # size: w, h
        # FIXME
        abs_list = []
        # Int regs
        abs_list += ["IntReg:"]
        def ireg_map():
            if not hasattr(self.xsp, "GetFromU64Array"):
                return [('error_red',"<Error! xspcomm.GetFromU64Array not find, please update your xspcomm lib>>")]
            return " ".join(["%3s: 0x%x" % (self.iregs[i],
                                            self.xsp.GetFromU64Array(self.difftest_stat.regs.xrf.value, i))
                             for i in range(32)])
        abs_list += [ireg_map()]
        # Float regs
        abs_list += ["\nFloatReg:"]
        def freg_map():
            return " ".join(["%3s: 0x%x" % (self.fregs[i],
                                            self.xsp.GetFromU64Array(self.difftest_stat.regs.frf.value, i))
                             for i in range(32)])
        abs_list += [freg_map()]
        # Commit PCs
        abs_list += ["\nCommit PC:"]
        abs_list += [" ".join(["0x%x%s" % (x[0], "" if x[1] else "*") for x in self.api_commit_pc_list()])]
        abs_list += ["max commit: 0x%x" % max([x[0] for x in self.api_commit_pc_list()])]
        # Add other content to display here

        # csr
        abs_list += ["\nCSR:"]
        abs_list += ["mstatus: 0x%x  " % self.difftest_stat.regs.csr.mstatus +
                     "mcause: 0x%x  " % self.difftest_stat.regs.csr.mcause +
                     "mepc: 0x%x  " % self.difftest_stat.regs.csr.mepc +
                     "mtval: 0x%x  " % self.difftest_stat.regs.csr.mtval +
                     "mtvec: 0x%x  " % self.difftest_stat.regs.csr.mtvec +
                     "privilegeMode: %d  " % self.difftest_stat.regs.csr.privilegeMode +
                     "mie: 0x%x  " % self.difftest_stat.regs.csr.mie +
                     "mip: 0x%x  " % self.difftest_stat.regs.csr.mip +
                     "satp: 0x%x  " % self.difftest_stat.regs.csr.satp +
                     "sstatus: 0x%x  " % self.difftest_stat.regs.csr.sstatus +
                     "scause: 0x%x  " % self.difftest_stat.regs.csr.scause +
                     "sepc: 0x%x  " % self.difftest_stat.regs.csr.sepc +
                     "stval: 0x%x  " % self.difftest_stat.regs.csr.stval +
                     "stvec: 0x%x  " % self.difftest_stat.regs.csr.stvec
                     ]
        # fcsr
        abs_list += ["\nFCSR: 0x%x" % self.difftest_stat.regs.fcsr.fcsr]

        # DASM info base address
        if self.info_force_address is not None:
            abs_list += [("light blue", f"\nDASM Window Force Mid Address: 0x{self.info_force_address:x}")]

        if self.api_is_difftest_diff_run():
            abs_list += [("light red", f"\nDifftest is running with ref: {self.api_get_ref_so_path()}")]

        # Bin file
        abs_list += ["\nLoaded Bin:"]
        abs_list += [f"file: {self.exec_bin_file}"]

        # Watch List
        if self.info_watch_list:
            abs_list += ["\nWatch List:"]
            for k , v in self.info_watch_list.items():
                abs_list += [f"{k}({v.W()}): 0x{v.value:x}"]

        if self.flash_bin_file:
            abs_list += ["\nFlash Bin:"]
            abs_list += [f"file: {self.flash_bin_file}"]

        # Watched commit pc
        commit_pc_cheker = self.condition_watch_commit_pc.get("checker")
        if commit_pc_cheker:
            stat_txt = "(Disabled)" if commit_pc_cheker.IsDisable() else ""
            abs_list += [f"\nWatched Commit PC{stat_txt}:"]
            watch_pc = OrderedDict()
            for k, v in commit_pc_cheker.ListCondition().items():
                pc = k.split("_")[2]
                if pc in watch_pc:
                    watch_pc[pc].append(v)
                else:
                    watch_pc[pc] = [v]
            for pc, v in watch_pc.items():
                checked = sum(v) > 0
                if checked:
                    abs_list += [("error_red", f"{pc}: {checked}")]
                else:
                    abs_list += [f"{pc}: {checked}"]

        if self.api_is_hit_good_trap():
            abs_list += ["\nProgram:"]
            abs_list += [("success_green", "HIT GOOD TRAP")]
        elif self.api_is_hit_good_loop():
            abs_list += ["\nProgram:"]
            abs_list += [("success_green", "HIT GOOD LOOP")]

        if self.api_is_trap_break_on():
            abs_list += ["\nTrap Info:"]
            trap_info = self.api_get_trap_info()
            abs_list += [f"pc: 0x{trap_info['pc']:x}  code: 0x{trap_info['code']:x}  hasTrap: {trap_info['hasTrap']}  cycle: 0x{trap_info['cycleCnt']:x} hasWFI: {trap_info['hasWFI']}"]

        if self.api_is_xbreak_on():
            abs_list += ["\nXBreaks:"]
            for br in self.api_xbreak_list():
                if br[4]:
                    abs_list += [("error_red", f"{br[0]}(0x{br[1]:x}) {br[2]} 0x{br[3]:x} hinted: {br[4]}")]
                else:
                    abs_list += [f"{br[0]}(0x{br[1]:x}) {br[2]} 0x{br[3]:x} hinted: {br[4]}"]
        
        if self.api_is_xbreak_expr_on():
            abs_list += ["\nXBreak Exprs:"]
            for k, hit, expr in self.api_xbreak_expr_list():
                if hit:
                    abs_list += [("error_red", f"{k}: hit={hit} expr={expr}")]
                else:
                    abs_list += [f"{k}: hit={hit} expr={expr}"]
        if self.api_is_xbreak_fsm_on():
            abs_list += ["\nXBreak FSM:"]
            try:
                status = self.api_xbreak_fsm_status()
                if status:
                    line = f"name={status['name']} state={status['state']} triggered={status['triggered']} trigger_state={status['trigger_state']}"
                    if status.get("triggered"):
                        abs_list += [("error_red", line)]
                    else:
                        abs_list += [line]
            except Exception as e:
                abs_list += [("error_red", f"<Error! read xbreak_fsm status failed: {e}>")]

        # TBD
        # abs_list += [("error_red", "\nFIXME:\nMore Data to be done\n")]
        return abs_list
