import os
import sys
import time
import inspect
import threading
import queue
import traceback
import subprocess
from multiprocessing.connection import Listener

from .io_capture import IOCapture, flush_cpp_stdout


class UiServer:
    def __init__(self, pdb, conn):
        self._pdb = pdb
        self._conn = conn
        self._send_lock = threading.Lock()
        self._recv_queue = queue.Queue()
        self._pending = []
        self._running = True
        self._prompt_id = 0
        self._capture = IOCapture(self._on_stream)
        self._recv_thread = threading.Thread(target=self._recv_loop, daemon=True)
        self._stream_buf = {"stdout": "", "stderr": ""}
        self._stream_last = {"stdout": 0.0, "stderr": 0.0}
        self._stream_flush_interval = 0.2
        self._asm_offset = 0

    def start(self):
        self._pdb.set_ui_handler(self)
        self._capture.start()
        self._recv_thread.start()
        self._send({"type": "pid", "pid": os.getpid()})
        self._send_state()

    def stop(self):
        self._running = False
        self._flush_stream_buffers(force=True)
        try:
            self._capture.stop()
        except Exception:
            pass
        try:
            self._pdb.clear_ui_handler()
        except Exception:
            pass
        try:
            self._conn.close()
        except Exception:
            pass

    def ui_write(self, level, msg):
        self._send({"type": "log", "level": level, "text": str(msg)})

    def ui_prompt(self, msg=""):
        self._prompt_id += 1
        req_id = self._prompt_id
        self._send({"type": "prompt", "id": req_id, "text": msg})
        while self._running:
            msg_obj = self._next_msg(block=True)
            if not msg_obj:
                continue
            if msg_obj.get("type") == "prompt_reply" and msg_obj.get("id") == req_id:
                return msg_obj.get("text", "")
            self._pending.append(msg_obj)
        return ""

    def ui_theme(self, name: str):
        if not name:
            return
        if name == "__list__":
            self._send({"type": "theme_list"})
            return
        if name == "__cycle__":
            self._send({"type": "theme_cycle"})
            return
        self._send({"type": "theme", "name": name})

    def ui_config(self, action: str):
        if action == "save":
            self._send({"type": "config_save"})
        elif action == "load":
            self._send({"type": "config_load"})

    def ui_tick(self):
        # No-op for now; reserved for future UI hooks
        return

    def _send(self, msg):
        with self._send_lock:
            try:
                self._conn.send(msg)
            except Exception:
                pass

    def _recv_loop(self):
        while self._running:
            try:
                msg = self._conn.recv()
            except EOFError:
                self._running = False
                try:
                    self._recv_queue.put({"type": "shutdown"})
                except Exception:
                    pass
                break
            except Exception:
                continue
            self._recv_queue.put(msg)

    def _next_msg(self, block=False):
        if self._pending:
            return self._pending.pop(0)
        try:
            if block:
                return self._recv_queue.get()
            return self._recv_queue.get_nowait()
        except queue.Empty:
            return None

    def _on_stream(self, stream, text):
        buf = self._stream_buf.get(stream, "") + text
        if not buf:
            return
        parts = buf.split("\n")
        tail = parts.pop() if parts else ""
        for line in parts:
            payload = line + "\n"
            self._send({"type": "stream", "stream": stream, "text": payload, "partial": False})
        self._stream_buf[stream] = tail
        now = time.monotonic()
        last = self._stream_last.get(stream, 0.0)
        if tail and now - last >= self._stream_flush_interval:
            self._send({"type": "stream", "stream": stream, "text": tail, "partial": True})
            self._stream_buf[stream] = ""
            self._stream_last[stream] = now
        elif parts:
            self._stream_last[stream] = now

    def _flush_stream_buffers(self, force=False):
        for stream in ("stdout", "stderr"):
            buf = self._stream_buf.get(stream, "")
            if not buf:
                continue
            if force or buf.endswith("\n"):
                is_partial = not buf.endswith("\n")
                self._send({"type": "stream", "stream": stream, "text": buf, "partial": is_partial})
                self._stream_buf[stream] = ""

    def _send_state(self):
        try:
            asm = self._pdb.api_asm_info(self._asm_size, offset_lines=self._asm_offset)
            abs_info = self._pdb.api_abs_info(self._abs_size)
        except Exception as e:
            self._send({"type": "log", "level": "error", "text": f"[State Error] {e}"})
            return
        force_addr = None
        eff_offset = None
        try:
            force_addr = self._pdb.api_get_info_force_mid_address()
        except Exception:
            pass
        try:
            eff_offset = self._pdb.api_get_info_last_offset()
        except Exception:
            eff_offset = None
        if eff_offset is not None:
            try:
                self._asm_offset = int(eff_offset)
            except Exception:
                pass
        self._send({
            "type": "state",
            "asm": asm,
            "abs": abs_info,
            "force_address": force_addr,
            "asm_offset": self._asm_offset,
        })

    def _handle_complete(self, line):
        cmp = []
        cmd, args, _ = self._pdb.parseline(line)
        if " " in line:
            complete_func = getattr(self._pdb, f"complete_{cmd}", None)
            if complete_func:
                arg = args
                if " " in args:
                    arg = args.split()[-1]
                idx = line.find(arg)
                cmp = complete_func(arg, line, idx, len(line))
        else:
            state = 0
            while True:
                item = self._pdb.complete(line, state)
                if not item:
                    break
                state += 1
                cmp.append(item)
        return cmp

    def _handle_hint(self, cmd: str) -> str:
        if not cmd:
            return ""
        fn = getattr(self._pdb, f"do_{cmd}", None)
        if not fn or not getattr(fn, "__doc__", None):
            return ""
        doc = inspect.cleandoc(fn.__doc__ or "")
        if not doc:
            return ""
        lines = doc.splitlines()
        usage_lines = []
        for i, line in enumerate(lines):
            if line.strip().lower().startswith("usage:"):
                for j in range(i, min(len(lines), i + 8)):
                    txt = lines[j].strip()
                    if not txt:
                        break
                    usage_lines.append(txt)
                break
        if not usage_lines:
            for line in lines:
                if line.strip():
                    usage_lines.append(line.strip())
                    break
            for line in lines[1:4]:
                if line.strip():
                    usage_lines.append(line.strip())
        return "\n".join(usage_lines)

    def _exec_cmd(self, cmd):
        if not cmd.strip():
            return
        if cmd in ["exit", "quit", "q"]:
            self._send({"type": "ui_exit"})
            self._running = False
            return
        if cmd in ["continue", "c", "count"]:
            self._pdb.tui_ret = self._pdb.onecmd(cmd, log_cmd=False)
            self._send({"type": "ui_exit"})
            self._running = False
            return
        if cmd == "xcontinue_batch":
            self._pdb._exec_batch_cmds()
            return

        self._pdb.record_cmd(cmd)
        try:
            self._pdb.onecmd(cmd, log_cmd=False)
        except Exception:
            self._send({"type": "log", "level": "error", "text": traceback.format_exc()})

    def serve(self):
        self._asm_size = (55, 20)
        self._abs_size = (80, 20)
        self.start()
        while self._running:
            msg = self._next_msg(block=True)
            if not msg:
                continue
            mtype = msg.get("type")
            if mtype == "cmd":
                cmd = msg.get("cmd", "")
                self._send({"type": "status", "state": "running", "cmd": cmd})
                self._exec_cmd(cmd)
                try:
                    flush_cpp_stdout()
                    sys.stdout.flush()
                    sys.stderr.flush()
                except Exception:
                    pass
                self._flush_stream_buffers(force=True)
                if self._running:
                    self._send({"type": "status", "state": "idle"})
                    self._send_state()
            elif mtype == "resize":
                self._asm_size = tuple(msg.get("asm_size", self._asm_size))
                self._abs_size = tuple(msg.get("abs_size", self._abs_size))
                self._send_state()
            elif mtype == "complete":
                line = msg.get("line", "")
                cmp = self._handle_complete(line)
                self._send({"type": "complete", "line": line, "items": cmp})
            elif mtype == "hint":
                cmd = msg.get("cmd", "")
                text = self._handle_hint(cmd)
                self._send({"type": "hint", "cmd": cmd, "text": text})
            elif mtype == "force_toggle":
                current = self._pdb.api_get_info_force_mid_address()
                if current is None:
                    self._pdb.api_set_info_force_mid_address(self._pdb.api_get_last_info_mid_address())
                else:
                    self._pdb.api_set_info_force_mid_address(None)
                self._send_state()
            elif mtype == "force_move":
                delta = int(msg.get("delta", 0))
                self._pdb.api_increase_info_force_address(delta)
                self._send_state()
            elif mtype == "force_clear":
                self._pdb.api_set_info_force_mid_address(None)
                self._asm_offset = 0
                self._send_state()
            elif mtype == "asm_offset":
                delta = int(msg.get("delta", 0))
                self._asm_offset += delta
                self._send({"type": "log", "level": "debug", "text": f"[AsmOffset] {self._asm_offset}"})
                self._send_state()
            elif mtype == "asm_offset_set":
                try:
                    self._asm_offset = int(msg.get("value", 0))
                except Exception:
                    self._asm_offset = 0
                self._send({"type": "log", "level": "debug", "text": f"[AsmOffset] {self._asm_offset}"})
                self._send_state()
            elif mtype == "interrupt":
                try:
                    self._send({"type": "log", "level": "warn", "text": "[Interrupt]"})
                    self._pdb._sigint_handler(None, None)
                except Exception:
                    pass
            elif mtype == "shutdown":
                self._running = False
            elif mtype == "prompt_reply":
                # replies are handled by ui_prompt via _pending queue
                self._pending.append(msg)
            else:
                continue
        self.stop()


def _build_ipc_path():
    return f"/tmp/xspdb_ui_{os.getpid()}.sock"


def enter_tui(pdb):
    ipc_path = _build_ipc_path()
    if os.path.exists(ipc_path):
        os.unlink(ipc_path)
    listener = Listener(ipc_path, family="AF_UNIX")

    env = os.environ.copy()
    cmd = [sys.executable, "-m", "xspdb.ui.textual_app", "--ipc", ipc_path]
    ui_proc = subprocess.Popen(cmd, env=env)

    conn = listener.accept()
    listener.close()

    server = UiServer(pdb, conn)
    try:
        server.serve()
    finally:
        try:
            if ui_proc.poll() is None:
                try:
                    conn.send({"type": "shutdown"})
                except Exception:
                    pass
        except Exception:
            pass
        try:
            conn.close()
        except Exception:
            pass
        if ui_proc.poll() is None:
            try:
                ui_proc.wait(timeout=2)
            except Exception:
                ui_proc.terminate()
                try:
                    ui_proc.wait(timeout=2)
                except Exception:
                    pass
        if os.path.exists(ipc_path):
            try:
                os.unlink(ipc_path)
            except Exception:
                pass
