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
from . import message, info, error

class CmdTrap:
    """Trap command class
    """

    def __init__(self):
        assert hasattr(self, "dut"), "this class must be used in XSPdb, canot be used alone"

    def api_init_waveform(self):
        """Initialize the waveform (close waveform at beginning)
        """
        self.dut.RefreshComb()
        self.dut.CloseWaveform()
        self.waveform_on = False

    def api_is_waveform_on(self):
        """Check if waveform recording is on

        Returns:
            bool: Whether the waveform is on
        """
        return self.waveform_on

    def api_waveform_on(self, wave_file=""):
        """Start waveform recording

        Args:
            wave_file (str): Waveform file path [optional]
        """
        if self.waveform_on:
            info("waveform is already on")
            return True
        if wave_file:
            if not os.path.isabs(wave_file):
                error(f"waveform file[{wave_file}] name must be a ligal path")
                return False
            self.dut.SetWaveform(wave_file)
        self.dut.OpenWaveform()
        self.waveform_on = True
        return True

    def api_waveform_off(self):
        """Close waveform recording"""
        if not self.waveform_on:
            info("waveform is already off")
            return True
        self.dut.CloseWaveform()
        self.waveform_on = False
        return True

    def do_xwave_on(self, arg):
        """Start waveform recording

        Args:
           name (str): Waveform file path [optional]
        """
        if self.api_waveform_on(arg):
            info("waveform on")
        else:
            message("usage: xwave_on [waveform file path]")

    def do_xwave_off(self, arg):
        """Close waveform recording

        Args:
            arg (None): No arguments
        """
        self.api_waveform_off()
        info("waveform off")

    def do_xwave_flush(self, arg):
        """Flush waveform recording

        Args:
            arg (None): No arguments
        """
        if not self.waveform_on:
            error("waveform is not on")
            return
        self.dut.FlushWaveform()
        info("waveform flush complete")
