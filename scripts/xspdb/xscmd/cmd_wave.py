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
import shutil
import time
from . import message, info, error

class CmdTrap:
    """Trap command class
    """

    def __init__(self):
        assert hasattr(self, "dut"), "this class must be used in XSPdb, canot be used alone"
        self._waveform_file = None
        self._waveform_file_inited = False

    def api_init_waveform(self):
        """Initialize the waveform (close waveform at beginning)
        """
        self.dut.RefreshComb()
        self.dut.FlushWaveform()
        self.waveform_on = False
        self.dut.PauseWaveformDump()
        if not self._waveform_file_inited:
            ts = time.strftime("%Y%m%d_%H%M%S")
            ext = self.dut.GetWaveFormat()
            if not ext:
                ext = ".fst"
            self._waveform_file = os.path.join(os.getcwd(), f"wave_{ts}{ext}")
            self._waveform_file_inited = True
        info(f"waveform is off (init). default file: {self._waveform_file}. use xwave_on to enable")

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
        if not wave_file:
            if self._waveform_file:
                wave_file = self._waveform_file
            else:
                ts = time.strftime("%Y%m%d_%H%M%S")
                ext = self.dut.GetWaveFormat()
                if not ext:
                    ext = ".fst"
                wave_file = os.path.join(os.getcwd(), f"wave_{ts}{ext}")
        if not os.path.isabs(wave_file):
            error(f"waveform file[{wave_file}] name must be a ligal path")
            return False
        self.dut.SetWaveform(wave_file)
        self._waveform_file = os.path.abspath(wave_file)
        self.dut.ResumeWaveformDump()
        self.waveform_on = True
        return True

    def api_waveform_off(self):
        """Close waveform recording"""
        if not self.waveform_on:
            info("waveform is already off")
            return True
        self.dut.PauseWaveformDump()
        # self.dut.FlushWaveform()
        # self.dut.PauseWaveformDump()
        self.waveform_on = False
        return True

    def api_waveform_continue(self, src_file):
        """Continue waveform by copying src_file to current waveform file then resuming."""
        if not src_file:
            error("usage: xwave_continue <src_waveform_file>")
            return False
        if not self._waveform_file:
            error("waveform file not set, please use xwave_on <waveform_file> first")
            return False
        if not os.path.isabs(src_file):
            error(f"waveform file[{src_file}] name must be a ligal path")
            return False
        if not os.path.exists(src_file):
            error(f"waveform file[{src_file}] not found")
            return False
        if self.waveform_on:
            self.api_waveform_off()
        dst = os.path.abspath(self._waveform_file)
        src = os.path.abspath(src_file)
        if dst != src:
            try:
                shutil.copyfile(src, dst)
            except OSError as e:
                error(f"copy waveform failed: {e}")
                return False
        self.dut.SetWaveform(dst)
        self.dut.ResumeWaveformDump()
        self.waveform_on = True
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

    def do_xwave_continue(self, arg):
        """Continue waveform by copying a file then resuming.

        Args:
            name (str): Source waveform file path
        """
        if self.api_waveform_continue(arg):
            info("waveform continue")
