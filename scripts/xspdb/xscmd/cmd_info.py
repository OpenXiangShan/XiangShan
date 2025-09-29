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

