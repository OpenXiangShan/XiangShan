#coding=utf-8

import bisect
from collections import OrderedDict
from XSPdb.cmd.util import error, info


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

    def api_asm_info(self, size):
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

        self.info_last_address = pc_last
        self.info_cached_cmpclist = pc_list.copy()
        # Check the cache first; if not found, generate it
        cache_index = pc_last - pc_last % self.info_cache_bsz
        asm_data = self.info_cache_asm.get(cache_index,
                                           self.api_all_data_to_asm(cache_index, self.info_cache_bsz))
        self.info_cache_asm[cache_index] = asm_data

        # Need to check boundaries; if near a boundary, fetch adjacent cache blocks
        cache_index_ext = base_addr
        if pc_last % self.info_cache_bsz < h:
            cache_index_ext = cache_index - self.info_cache_bsz
        elif self.info_cache_bsz - pc_last % self.info_cache_bsz < h:
            cache_index_ext = cache_index + self.info_cache_bsz

        # Boundary is valid
        if cache_index_ext > base_addr:
            asm_data_ext = self.info_cache_asm.get(cache_index_ext,
                                                   self.api_all_data_to_asm(cache_index_ext, self.info_cache_bsz))
            self.info_cache_asm[cache_index_ext] = asm_data_ext
            if cache_index_ext < cache_index:
                asm_data = self.api_merge_asm_list_overlap_append(asm_data_ext, asm_data)
            else:
                asm_data = self.api_merge_asm_list_overlap_append(asm_data, asm_data_ext)

        # Quickly locate the position of pc_last
        address_list = [x[0] for x in asm_data]
        pc_last_index = bisect.bisect_left(address_list, pc_last)
        start_line = max(0, pc_last_index - h//2)
        asm_lines = []
        for l in  asm_data[start_line:start_line + h]:
            find_pc = l[0] in valid_pc_list
            line = "%s|0x%x: %s  %s  %s" % (">" if find_pc else " ", l[0], l[1], l[2], l[3])
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
                                            self.xsp.GetFromU64Array(self.difftest_stat.regs_int.value, i))
                             for i in range(32)])
        abs_list += [ireg_map()]
        # Float regs
        abs_list += ["\nFloatReg:"]
        def freg_map():
            return " ".join(["%3s: 0x%x" % (self.fregs[i],
                                            self.xsp.GetFromU64Array(self.difftest_stat.regs_fp.value, i))
                             for i in range(32)])
        abs_list += [freg_map()]
        # Commit PCs
        abs_list += ["\nCommit PC:"]
        abs_list += [" ".join(["0x%x%s" % (x[0], "" if x[1] else "*") for x in self.api_commit_pc_list()])]
        abs_list += ["max commit: 0x%x" % max([x[0] for x in self.api_commit_pc_list()])]
        # Add other content to display here

        # csr
        abs_list += ["\nCSR:"]
        abs_list += ["mstatus: 0x%x  " % self.difftest_stat.csr.mstatus + 
                     "mcause: 0x%x  " % self.difftest_stat.csr.mcause +
                     "mepc: 0x%x  " % self.difftest_stat.csr.mepc +
                     "mtval: 0x%x  " % self.difftest_stat.csr.mtval +
                     "mtvec: 0x%x  " % self.difftest_stat.csr.mtvec +
                     "privilegeMode: %d  " % self.difftest_stat.csr.privilegeMode +
                     "mie: 0x%x  " % self.difftest_stat.csr.mie +
                     "mip: 0x%x  " % self.difftest_stat.csr.mip + 
                     "satp: 0x%x  " % self.difftest_stat.csr.satp +
                     "sstatus: 0x%x  " % self.difftest_stat.csr.sstatus +
                     "scause: 0x%x  " % self.difftest_stat.csr.scause +
                     "sepc: 0x%x  " % self.difftest_stat.csr.sepc +
                     "stval: 0x%x  " % self.difftest_stat.csr.stval +
                     "stvec: 0x%x  " % self.difftest_stat.csr.stvec
                     ]
        # fcsr
        abs_list += ["\nFCSR: 0x%x" % self.difftest_stat.fcsr.fcsr]

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

        # TBD
        # abs_list += [("error_red", "\nFIXME:\nMore Data to be done\n")]
        return abs_list
