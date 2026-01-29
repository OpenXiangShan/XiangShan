import os
import fcntl
import threading
import select
import ctypes
import sys

libc = ctypes.CDLL(None)
_libc_stdout = ctypes.c_void_p.in_dll(libc, "stdout")


def flush_cpp_stdout():
    try:
        libc.fflush(_libc_stdout)
    except Exception:
        pass


class IOCapture:
    def __init__(self, on_data):
        self._on_data = on_data
        self._stop = threading.Event()
        self._thread = None
        self._stdout_r = None
        self._stderr_r = None
        self._orig_stdout = None
        self._orig_stderr = None

    def start(self):
        self._orig_stdout = os.dup(1)
        self._orig_stderr = os.dup(2)

        stdout_r, stdout_w = os.pipe()
        stderr_r, stderr_w = os.pipe()

        os.dup2(stdout_w, 1)
        os.dup2(stderr_w, 2)
        os.close(stdout_w)
        os.close(stderr_w)

        self._stdout_r = stdout_r
        self._stderr_r = stderr_r

        for fd in (self._stdout_r, self._stderr_r):
            flags = fcntl.fcntl(fd, fcntl.F_GETFL)
            fcntl.fcntl(fd, fcntl.F_SETFL, flags | os.O_NONBLOCK)

        try:
            sys.stdout.reconfigure(line_buffering=True)
            sys.stderr.reconfigure(line_buffering=True)
        except Exception:
            pass

        self._thread = threading.Thread(target=self._pump, daemon=True)
        self._thread.start()

    def stop(self):
        self._stop.set()
        if self._thread:
            self._thread.join(timeout=0.5)
        try:
            if self._orig_stdout is not None:
                os.dup2(self._orig_stdout, 1)
            if self._orig_stderr is not None:
                os.dup2(self._orig_stderr, 2)
        except Exception:
            pass
        for fd in (self._stdout_r, self._stderr_r, self._orig_stdout, self._orig_stderr):
            if fd is None:
                continue
            try:
                os.close(fd)
            except Exception:
                pass

    def _pump(self):
        while not self._stop.is_set():
            try:
                rlist, _, _ = select.select([self._stdout_r, self._stderr_r], [], [], 0.05)
            except Exception:
                continue
            for fd in rlist:
                try:
                    data = os.read(fd, 4096)
                except BlockingIOError:
                    continue
                except Exception:
                    data = b""
                if not data:
                    continue
                text = data.decode(errors="replace")
                stream = "stdout" if fd == self._stdout_r else "stderr"
                try:
                    self._on_data(stream, text)
                except Exception:
                    pass
