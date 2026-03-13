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

try:
    from textual.widgets import Static, Button
except Exception:
    Static = object
    Button = object


class AsmView:
    def __init__(self, app, formatter):
        self._app = app
        self._format = formatter
        self._memory_body = None
        self._memory_title = None
        self._follow_button = None
        self._force_address = None

    def attach(self, memory_body: Static, memory_title: Static, follow_button: Button) -> None:
        self._memory_body = memory_body
        self._memory_title = memory_title
        self._follow_button = follow_button

    def update(self, asm_lines, force_address=None) -> None:
        if self._memory_body is None:
            return
        self._memory_body.update(self._format(asm_lines))
        self._force_address = force_address
        if self._follow_button is None:
            return
        if self._memory_title is not None:
            self._memory_title.update("Memory Disassembly")
        if force_address is None:
            self._follow_button.label = "Follow"
            self._follow_button.disabled = True
        else:
            try:
                addr = int(force_address)
                self._follow_button.label = f"Unlock 0x{addr:x}"
            except Exception:
                self._follow_button.label = "Unlock"
            self._follow_button.disabled = False

    def on_follow_pressed(self) -> None:
        if self._app._backend:
            self._app._backend.send({"type": "force_clear"})

    def scroll_offset(self, delta: int) -> None:
        if not self._app._backend:
            return
        self._app._backend.send({"type": "asm_offset", "delta": int(delta)})

    def set_offset(self, value: int) -> None:
        if not self._app._backend:
            return
        self._app._backend.send({"type": "asm_offset_set", "value": int(value)})
