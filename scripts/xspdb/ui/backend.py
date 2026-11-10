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
