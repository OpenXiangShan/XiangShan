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
import signal
from . import info, error, message, warn

class CmdForkBackup:

    def __init__(self):
        assert hasattr(self, "dut"), "this class must be used in XSPdb, canot be used alone"
        self._fork_backup_enabled = False
        self._fork_backup_is_child = False
        self._fork_backup_window_sec = 10.0
        self._fork_backup_last_fork_time = 0.0
        self._fork_backup_child_pid = None
        self._fork_backup_child_state = None
        self._fork_backup_child_wave = None
        self._fork_backup_ctl_w = None
        self._fork_backup_target_breaks = set()
        self._fork_backup_child_hit = False
        self._fork_backup_wave_dir = None
        self._fork_backup_log_path = None
        self._fork_backup_log_path_child = None
        self._fork_backup_parent_log_path = None
        self._fork_backup_wave_history = []
        self._fork_backup_keep_waves = 2

    def _fork_backup_tick(self):
        if not self._fork_backup_enabled or self._fork_backup_is_child:
            return
        if self._fork_backup_window_sec <= 0:
            return
        if self._fork_backup_child_state == "running":
            return
        self._fork_backup_reap_child()
        now = time.time()
        if now - self._fork_backup_last_fork_time < self._fork_backup_window_sec:
            return
        self._fork_backup_spawn_child()

    def _fork_backup_spawn_child(self):
        self._fork_backup_cleanup_child()
        try:
            ctl_r, ctl_w = os.pipe()
        except OSError as e:
            warn(f"fork backup: create pipe failed: {e}")
            return
        try:
            pid = os.fork()
        except OSError as e:
            warn(f"fork backup: fork failed: {e}")
            os.close(ctl_r)
            os.close(ctl_w)
            return
        if pid == 0:
            os.close(ctl_w)
            self._fork_backup_is_child = True
            self._fork_backup_enabled = False
            self._fork_backup_child_state = "waiting"
            self._fork_backup_target_breaks = set()
            self._fork_backup_child_hit = False
            self.on_update_tstep = None
            self._fork_backup_child_main(ctl_r)
            os._exit(0)
        os.close(ctl_r)
        self._fork_backup_child_pid = pid
        self._fork_backup_child_state = "waiting"
        self._fork_backup_ctl_w = ctl_w
        self._fork_backup_child_wave = None
        self._fork_backup_last_fork_time = time.time()
        info(f"fork backup: spawned child pid={pid}")
        self._fork_backup_parent_log(f"spawn pid={pid}")

    def _fork_backup_cleanup_child(self):
        if self._fork_backup_ctl_w is not None:
            try:
                os.close(self._fork_backup_ctl_w)
            except OSError:
                pass
            self._fork_backup_ctl_w = None
        if self._fork_backup_child_pid is not None:
            try:
                os.kill(self._fork_backup_child_pid, signal.SIGTERM)
            except ProcessLookupError:
                pass
            except OSError as e:
                warn(f"fork backup: kill child failed: {e}")
            self._fork_backup_child_pid = None
        self._fork_backup_child_state = None
        self._fork_backup_child_wave = None

    def _fork_backup_on_break(self, checked_keys):
        if not self._fork_backup_enabled or self._fork_backup_is_child:
            return
        if not checked_keys:
            return
        self._fork_backup_reap_child()
        if self._fork_backup_child_pid is None or self._fork_backup_child_state != "waiting":
            return
        wave_file = self._fork_backup_make_wave_path()
        if not wave_file:
            return
        payload = f"WAKE {','.join(checked_keys)} {wave_file}\n"
        try:
            os.write(self._fork_backup_ctl_w, payload.encode())
            self._fork_backup_child_state = "running"
            self._fork_backup_child_wave = wave_file
            info(f"fork backup: child pid={self._fork_backup_child_pid} wake, wave={wave_file}")
            self._fork_backup_parent_log(f"wake pid={self._fork_backup_child_pid} wave={wave_file} breaks={checked_keys}")
        except OSError as e:
            warn(f"fork backup: wake child failed: {e}")
            self._fork_backup_cleanup_child()

    def _fork_backup_on_break_any(self):
        if not self._fork_backup_enabled or self._fork_backup_is_child:
            return
        if self._fork_backup_child_pid is None or self._fork_backup_child_state != "waiting":
            return
        self._fork_backup_on_break(["*"])

    def _fork_backup_make_wave_path(self):
        wave_dir = self._fork_backup_wave_dir or os.getcwd()
        wave_dir = os.path.abspath(wave_dir)
        if not os.path.isdir(wave_dir):
            warn(f"fork backup: wave dir not found: {wave_dir}")
            return ""
        ts = time.strftime("%Y%m%d_%H%M%S")
        fname = f"fork_wave_{ts}_{os.getpid()}.fst"
        return os.path.join(wave_dir, fname)

    def _fork_backup_child_main(self, ctl_r):
        self._fork_backup_redirect_stdio()
        self._fork_backup_log("fork backup child: waiting for wake signal")
        try:
            with os.fdopen(ctl_r, "r") as f:
                line = f.readline()
        except Exception as e:
            self._fork_backup_log(f"fork backup child: read failed: {e}")
            return
        if not line:
            self._fork_backup_log("fork backup child: empty wake signal")
            return
        parts = line.strip().split(" ", 2)
        if len(parts) < 3 or parts[0] != "WAKE":
            self._fork_backup_log(f"fork backup child: invalid wake signal: {line.strip()}")
            return
        break_keys = [k for k in parts[1].split(",") if k]
        wave_file = parts[2].strip()
        self._fork_backup_target_breaks = set(break_keys)
        if "*" in self._fork_backup_target_breaks:
            self._fork_backup_target_breaks = set()
        self._fork_backup_child_hit = False
        self._fork_backup_log(f"fork backup child: wake, target={break_keys}, wave={wave_file}")
        if not self.api_waveform_on(wave_file):
            self._fork_backup_log("fork backup child: waveform on failed")
            return
        self._fork_backup_run_until_target()
        try:
            self.dut.FlushWaveform()
            self._fork_backup_log("fork backup child: waveform flush")
        except Exception as e:
            self._fork_backup_log(f"fork backup child: waveform flush failed: {e}")
        self.api_waveform_off()
        self._fork_backup_log("fork backup child: waveform off, exit")

    def _fork_backup_run_until_target(self):
        step_cycle = 10000
        while True:
            if self._fork_backup_child_hit:
                break
            if self.api_dut_is_step_exit():
                self._fork_backup_log("fork backup child: exit condition hit")
                break
            self.api_step_dut(step_cycle)
            if not self._fork_backup_target_breaks and self.dut.xclock.IsDisable():
                self._fork_backup_child_hit = True

    def _fork_backup_redirect_stdio(self):
        log_path = self._fork_backup_log_path or os.path.join(os.getcwd(), "fork_backup.log")
        self._fork_backup_log_path_child = log_path
        try:
            log_fd = os.open(log_path, os.O_WRONLY | os.O_CREAT | os.O_APPEND, 0o644)
        except OSError:
            return
        devnull_fd = os.open(os.devnull, os.O_RDONLY)
        os.dup2(devnull_fd, 0)
        os.dup2(log_fd, 1)
        os.dup2(log_fd, 2)
        os.close(devnull_fd)
        os.close(log_fd)

    def _fork_backup_log(self, msg):
        log_path = self._fork_backup_log_path_child or os.path.join(os.getcwd(), "fork_backup.log")
        ts = time.strftime("%Y-%m-%d %H:%M:%S")
        line = f"{ts} {msg}\n"
        try:
            fd = os.open(log_path, os.O_WRONLY | os.O_CREAT | os.O_APPEND, 0o644)
            os.write(fd, line.encode())
            os.close(fd)
        except OSError:
            pass

    def _fork_backup_parent_log(self, msg):
        log_path = self._fork_backup_parent_log_path or os.path.join(os.getcwd(), "fork_backup_parent.log")
        ts = time.strftime("%Y-%m-%d %H:%M:%S")
        try:
            with open(log_path, "a") as f:
                f.write(f"{ts} {msg}\n")
        except OSError:
            pass

    def _fork_backup_reap_child(self):
        if self._fork_backup_child_pid is None:
            return
        try:
            pid, _ = os.waitpid(self._fork_backup_child_pid, os.WNOHANG)
        except ChildProcessError:
            pid = self._fork_backup_child_pid
        except OSError:
            return
        if pid == 0:
            return
        if self._fork_backup_child_wave:
            self._fork_backup_wave_history.append(self._fork_backup_child_wave)
            self._fork_backup_prune_waves()
        self._fork_backup_parent_log(f"reap pid={self._fork_backup_child_pid}")
        if self._fork_backup_ctl_w is not None:
            try:
                os.close(self._fork_backup_ctl_w)
            except OSError:
                pass
        self._fork_backup_child_pid = None
        self._fork_backup_child_state = None
        self._fork_backup_child_wave = None
        self._fork_backup_ctl_w = None

    def _fork_backup_prune_waves(self):
        keep = self._fork_backup_keep_waves
        if keep <= 0:
            return
        while len(self._fork_backup_wave_history) > keep:
            old = self._fork_backup_wave_history.pop(0)
            try:
                os.remove(old)
            except OSError:
                pass

    def api_fork_backup_on(self, window_sec=10.0, wave_dir=None, log_path=None):
        """Enable fork backup for xbreak-triggered waveform.

        Args:
            window_sec (float): Backup window in wallclock seconds.
            wave_dir (str): Directory to place waveform files.
            log_path (str): Child log file path.
        """
        self._fork_backup_enabled = True
        self._fork_backup_window_sec = float(window_sec)
        self._fork_backup_last_fork_time = 0.0
        self._fork_backup_wave_dir = wave_dir or os.getcwd()
        self._fork_backup_log_path = log_path or os.path.join(os.getcwd(), "fork_backup.log")
        self._fork_backup_parent_log_path = os.path.join(os.getcwd(), "fork_backup_parent.log")
        info(f"fork backup: enabled (window={self._fork_backup_window_sec}s, wave_dir={self._fork_backup_wave_dir})")

    def api_fork_backup_off(self):
        """Disable fork backup"""
        self._fork_backup_enabled = False
        self._fork_backup_cleanup_child()
        self._fork_backup_reap_child()

    def api_fork_backup_status(self):
        """Get fork backup status"""
        return {
            "enabled": self._fork_backup_enabled,
            "window_sec": self._fork_backup_window_sec,
            "child_pid": self._fork_backup_child_pid,
            "child_state": self._fork_backup_child_state,
            "child_wave": self._fork_backup_child_wave,
            "wave_dir": self._fork_backup_wave_dir,
            "log_path": self._fork_backup_log_path,
            "parent_log_path": self._fork_backup_parent_log_path,
            "keep_waves": self._fork_backup_keep_waves,
        }

    def do_xfork_backup_on(self, arg):
        """Enable fork backup.

        Args:
            arg (string): [window_sec] [wave_dir] [log_path]
        """
        parts = arg.strip().split()
        window_sec = self._fork_backup_window_sec
        wave_dir = None
        log_path = None
        if len(parts) >= 1:
            try:
                window_sec = float(parts[0])
            except ValueError:
                error("window_sec must be a number")
                return
        if len(parts) >= 2:
            wave_dir = parts[1]
        if len(parts) >= 3:
            log_path = parts[2]
        if len(parts) > 3:
            message("usage: xfork_backup_on [window_sec] [wave_dir] [log_path]")
            return
        self.api_fork_backup_on(window_sec, wave_dir, log_path)

    def do_xfork_backup_off(self, arg):
        """Disable fork backup."""
        self.api_fork_backup_off()

    def do_xfork_backup_status(self, arg):
        """Show fork backup status."""
        st = self.api_fork_backup_status()
        message(f"enabled={st['enabled']} window_sec={st['window_sec']}")
        message(f"child_pid={st['child_pid']} child_state={st['child_state']} child_wave={st['child_wave']}")
        message(f"wave_dir={st['wave_dir']} log_path={st['log_path']}")
        message(f"parent_log_path={st['parent_log_path']} keep_waves={st['keep_waves']}")
