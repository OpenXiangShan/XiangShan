import threading


class BackendClient:
    def __init__(self, conn, app):
        self._conn = conn
        self._app = app
        self._thread = threading.Thread(target=self._recv_loop, daemon=True)
        self._thread.start()

    def send(self, msg):
        try:
            self._conn.send(msg)
        except Exception:
            pass

    def _recv_loop(self):
        while True:
            try:
                msg = self._conn.recv()
            except EOFError:
                self._app.call_from_thread(self._app.on_backend_disconnect)
                return
            except Exception:
                continue
            self._app.call_from_thread(self._app.on_backend_message, msg)
