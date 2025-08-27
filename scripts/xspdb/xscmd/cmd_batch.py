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
import time 
from . import info, error, message, warn, find_executable_in_dirs, YELLOW, RESET

class CmdBatch:
    """Execute batch cmds"""

    def __init__(self):
        self.ignore_cmds_in_batch = [
            "xload_script",
            "xreplay_log",
        ]
        self.batch_cmds_to_exec = []
        self.batch_depth = 0

    def cmd_in_ignore_list(self, cmd):
        """Check if the command is in the ignore list"""
        for ignore_cmd in self.ignore_cmds_in_batch:
            if cmd.startswith(ignore_cmd):
                return True
        return False

    def api_exec_batch_cmd(self, cmd_list, callback=None, gap_time=0, target_prefix="", target_subfix=""):
        cmd_to_exce = []
        for i, line in enumerate(cmd_list):
            line = str(line).strip()
            if target_prefix:
                start = line.find(target_prefix)
                if start < 0:
                    continue
                line = line[start + len(target_prefix):].strip()
            if target_subfix:
                end = line.find(target_subfix)
                if end < 0:
                    continue
                line = line[:end].strip()
            if line.startswith("#"):
                continue
            tag = "__sharp_tag_%s__" % str(time.time())
            line = line.replace("\#", tag).split("#")[0].replace(tag, "#").strip()
            if not line:
                continue
            if self.cmd_in_ignore_list(line):
                warn(f"ignore batch cmd: {line}")
                continue
            cmd_to_exce.append((line, gap_time, callback))
        self.batch_cmds_to_exec = cmd_to_exce + self.batch_cmds_to_exec
        return len(cmd_to_exce)

    def api_exec_script(self, script_file, callback=None, gap_time=0, target_prefix="", target_subfix=""):
        if not os.path.exists(script_file):
            error(f"script: {script_file} not find!")
            return -1
        with open(script_file, "r") as f:
            return self.api_exec_batch_cmd(f.readlines(),
                                           callback,
                                           gap_time,
                                           target_prefix,
                                           target_subfix,
                                           )

    def api_batch_get_default_break_cb(self):
        def break_cb(c):
            warn(f"Batch cmd excution is breaked, after {c} cmds")
            return False
        return break_cb

    def api_batch_get_default_break_cb(self):
        def break_cb(c):
            warn(f"Batch cmd excution is breaked, after {c} cmds")
            return False
        return break_cb

    def do_xload_script(self, arg):
        """Load an XSPdb script

        Args:
            script (string): Path to the script file
            delay_time (float): time delay between each cmd
        """
        usage = "usage: xload_script <script_file> [delay_time]"
        if not arg:
            message(usage)
            return
        args = arg.split()
        path = args[0]
        delay = 0.2
        if len(args) > 1:
            try:
                delay = float(args[1])
            except Exception as e:
                error("Convert dalay fail: %s, from args: %s\n%s" % (e, arg, usage))
        cmd_count = self.api_exec_script(path, gap_time=delay)
        if cmd_count >= 0:
            self._exec_batch_cmds()
            is_continue = getattr(self, "__last_batch_cmd_ret__", False)
            message(f"Load script: {path} success, cmd count: {cmd_count}, continue: {is_continue}")
            return is_continue

    def complete_xload_script(self, text, line, begidx, endidx):
        return self.api_complite_localfile(text)

