#coding=utf-8

import os
import time
from xspdb.cmd.util import info, error, message, warn, find_executable_in_dirs, YELLOW, RESET


class CmdBatch:
    """Excute batch cmds"""

    def __init__(self):
        self.ignore_cmds_in_batch = [
            "xload_script",
            "xreplay_log",
        ]
        self.batch_cmds_to_exec = []
        self.batch_depth = 0

    def api_batch_append_tail_one_cmd(self, cmd, gap_time=0.1, callback=None):
        self.batch_cmds_to_exec.append((cmd, gap_time, callback))

    def api_batch_append_head_one_cmd(self, cmd, gap_time=0.1, callback=None):
        self.batch_cmds_to_exec = [(cmd, gap_time, callback)] + self.batch_cmds_to_exec

    def api_batch_append_tail_cmds(self, cmds):
        self.batch_cmds_to_exec += cmds

    def api_batch_append_head_cmds(self, cmds):
        self.batch_cmds_to_exec = cmds + self.batch_cmds_to_exec

    def is_working_in_batch_mode(self):
        return self.batch_depth > 0

    def cmd_in_ignore_list(self, cmd):
        """Check if the command is in the ignore list"""
        for ignore_cmd in self.ignore_cmds_in_batch:
            if cmd.startswith(ignore_cmd):
                return True
        return False

    def api_clear_batch_ignore_list(self):
        """Clear the ignore list"""
        self.ignore_cmds_in_batch = []
        info("ignore cmd list cleared")
        return True

    def api_add_batch_ignore_list(self, cmd):
        """Add a command to the ignore list"""
        if cmd in self.ignore_cmds_in_batch:
            info(f"cmd: {cmd} already in ignore list")
            return False
        self.ignore_cmds_in_batch.append(cmd)
        info(f"add cmd: {cmd} to ignore list")
        return True

    def api_del_batch_ignore_list(self, cmd):
        """Delete a command from the ignore list"""
        if cmd not in self.ignore_cmds_in_batch:
            info(f"cmd: {cmd} not in ignore list")
            return False
        self.ignore_cmds_in_batch.remove(cmd)
        info(f"delete cmd: {cmd} from ignore list")
        return True

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

    def complete_xbatch_unignore_cmd(self, text, line, begidx, endidx):
        """Complete the command for xbatch_unignore_cmd"""
        if not text:
            return self.ignore_cmds_in_batch
        else:
            return [cmd for cmd in self.ignore_cmds_in_batch if cmd.startswith(text)]
