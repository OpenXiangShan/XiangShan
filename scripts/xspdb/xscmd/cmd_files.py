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
from . import info, error, message, warn

class CmdFiles:
    
    def api_dut_bin_load(self, bin_file):
        """Load a bin file into memory

        Args:
            bin_file (string): Path to the bin file
        """
        assert os.path.exists(bin_file), "file %s not found" % bin_file
        self.exec_bin_file = bin_file
        if self.mem_inited:
            self.df.overwrite_ram(bin_file, self.mem_size)
        else:
            self.api_init_mem()
        self.info_cache_asm.clear()

    def complete_xflash(self, text, line, begidx, endidx):
        return self.api_complite_localfile(text)

    def do_xload(self, arg):
        """Load a binary file into memory

        Args:
            arg (string): Path to the binary file
        """
        if not arg:
            message("usage: xload <bin_file>")
            return
        if not os.path.exists(arg):
            error(f"{arg} not found")
            return
        self.api_dut_bin_load(arg)

    def complete_xload(self, text, line, begidx, endidx):
        return self.api_complite_localfile(text)
