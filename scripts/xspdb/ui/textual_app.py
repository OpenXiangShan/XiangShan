import argparse
import os
import sys
import re
import signal
import time
from multiprocessing.connection import Client

try:
    from textual.app import App, ComposeResult
    from textual.binding import Binding
    from textual.widgets import Input, Static, Button
    from textual.geometry import Size
    from textual.strip import Strip
    from textual.containers import Vertical, VerticalScroll
    from rich.text import Text
    from rich.segment import Segment
    # rich.style.Style is used in widgets module
except Exception:
    App = None
    Text = None

from .backend import BackendClient
from .completion import CompletionController
from .config import UiConfigController
from .constants import LEVEL_PREFIX, LEVEL_STYLE, STYLE_MAP
from .layout import compose_layout
from .log_view import LogView
from .summary_view import SummaryView
from .asm_view import AsmView
from .widgets import SelectableRichLog


class XSPdbTextualApp(App):
    ALLOW_SELECT = True
    CSS = """
    Screen {
        layout: vertical;
    }
    #top {
        height: 1fr;
    }
    #bottom {
        height: 12;
    }
    #bottom_split {
        height: 1fr;
    }
    #memory {
        border: solid $panel;
    }
    #memory_header {
        height: 1;
    }
    #memory_scroll {
        height: 1fr;
        overflow-y: auto;
    }
    #memory_title {
        width: 1fr;
    }
    #memory_follow {
        width: auto;
        min-width: 8;
        height: 1;
        line-pad: 1;
        padding: 0 1;
        border: none;
    }
    #summary {
        border: solid $panel;
    }
    #vsplit {
        width: 2;
        background: $panel;
        height: 100%;
    }
    #vsplit_bottom {
        width: 2;
        background: $panel;
        height: 100%;
    }
    #hsplit {
        height: 1;
        background: $panel;
        width: 100%;
    }
    #console {
        border: solid $panel;
    }
    #stream {
        border: solid $panel;
    }
    #suggest {
        height: 4;
        color: $text-muted;
    }
    #command {
        border: solid $panel;
    }
    """

    BINDINGS = [
        ("ctrl+c", "interrupt", "Interrupt"),
        ("ctrl+q", "quit", "Quit"),
        ("ctrl+l", "clear_console", "Clear"),
        ("pageup", "console_page_up", "Console Up"),
        ("pagedown", "console_page_down", "Console Down"),
        ("end", "console_follow", "Console Follow"),
        ("ctrl+left", "shrink_left", "Shrink Left"),
        ("ctrl+right", "grow_left", "Grow Left"),
        ("ctrl+up", "grow_bottom", "Grow Bottom"),
        ("ctrl+down", "shrink_bottom", "Shrink Bottom"),
        ("ctrl+f", "toggle_force_addr", "Toggle Force Addr"),
        ("ctrl+u", "force_addr_up", "Force Addr Up"),
        ("ctrl+n", "force_addr_down", "Force Addr Down"),
        Binding("tab", "tab_complete", "Complete", priority=True),
        Binding("shift+tab", "tab_complete_prev", "Complete Prev", priority=True),
        ("ctrl+t", "toggle_theme", "Toggle Theme"),
        ("ctrl+shift+t", "cycle_theme", "Next Theme"),
        ("ctrl+alt+t", "theme_list", "Theme List"),
    ]

    def __init__(self, ipc_path, **kwargs):
        super().__init__(**kwargs)
        self._ipc_path = ipc_path
        self._backend = None
        self._history = []
        self._history_index = 0
        self._log = LogView(self, LEVEL_PREFIX, LEVEL_STYLE, self._sanitize_text, self._render_console_strip)
        self._prompt_id = None
        self._backend_pid = None
        self._asm_offset = 0
        self._completion = CompletionController(self)
        self._suppress_reset = 0
        self._suppress_input_change = 0
        self._hint_cmd = ""
        self._hint_text = ""
        self._summary = SummaryView(self, STYLE_MAP)
        self._asm_view = AsmView(self, self._format_lines)
        self._left_width = 55
        self._bottom_height = 12
        self._right_width = 40
        self._dragging = None
        self._drag_origin = 0
        self._drag_start = 0
        self._config = UiConfigController(self)
        self._scroll_debug_last = 0.0

    def compose(self) -> ComposeResult:
        yield from compose_layout()

    def on_mount(self) -> None:
        if App is None:
            self.exit(message="textual not installed")
            return
        conn = Client(self._ipc_path, family="AF_UNIX")
        self._backend = BackendClient(conn, self)
        self.set_interval(0.2, self._send_resize)
        self.set_interval(0.05, self._log.flush)
        self.query_one("#command", Input).focus()
        self._config.init_themes()
        self._config.load_config()
        try:
            self._log.attach(
                self.query_one("#console", SelectableRichLog),
                self.query_one("#stream", SelectableRichLog),
            )
        except Exception:
            pass
        try:
            self._summary.attach(self.query_one("#summary_body", Static))
        except Exception:
            pass
        try:
            self._asm_view.attach(
                self.query_one("#memory_body", Static),
                self.query_one("#memory_title", Static),
                self.query_one("#memory_follow", Button),
            )
        except Exception:
            pass
        self._apply_layout()

    def _send_resize(self):
        if not self._backend:
            return
        mem_panel = self.query_one("#memory_scroll", VerticalScroll)
        sum_panel = self.query_one("#summary", VerticalScroll)
        asm_h = max(3, mem_panel.size.height - 3)
        abs_h = max(3, sum_panel.size.height - 3)
        asm_w = max(10, mem_panel.size.width - 2)
        abs_w = max(10, sum_panel.size.width - 2)
        asm_size = (asm_w, asm_h)
        abs_size = (abs_w, abs_h)
        self._backend.send({"type": "resize", "asm_size": asm_size, "abs_size": abs_size})

    def on_input_submitted(self, event: Input.Submitted) -> None:
        if self._completion.accept_if_active():
            return
        cmd = event.value.strip()
        if self._prompt_id is not None:
            self._backend.send({"type": "prompt_reply", "id": self._prompt_id, "text": cmd})
            self._prompt_id = None
            event.input.value = ""
            return
        if not cmd:
            return
        if cmd == "clear":
            self.action_clear_console()
            event.input.value = ""
            return
        self._completion.clear_suggest()
        self._push_history(cmd)
        if self._backend:
            self._backend.send({"type": "cmd", "cmd": cmd})
        event.input.value = ""

    def _push_history(self, cmd):
        if not self._history or self._history[-1] != cmd:
            self._history.append(cmd)
        self._history_index = len(self._history)

    def on_key(self, event) -> None:
        if event.key == "ctrl+v" and not isinstance(self.focused, Input):
            event.stop()
            if hasattr(event, "prevent_default"):
                event.prevent_default()
            return
        if event.key in ("ctrl+left", "ctrl+right", "ctrl+up", "ctrl+down"):
            if event.key == "ctrl+left":
                self.action_shrink_left()
            elif event.key == "ctrl+right":
                self.action_grow_left()
            elif event.key == "ctrl+up":
                self.action_grow_bottom()
            elif event.key == "ctrl+down":
                self.action_shrink_bottom()
            event.stop()
            if hasattr(event, "prevent_default"):
                event.prevent_default()
            return
        if self._handle_panel_scroll_keys(event):
            return
        if self._completion.handle_nav_key(event.key):
            event.stop()
            if hasattr(event, "prevent_default"):
                event.prevent_default()
            return
        if self._completion.handle_enter_key(event.key):
            event.stop()
            if hasattr(event, "prevent_default"):
                event.prevent_default()
            return
        if isinstance(self.focused, Input):
            if event.key == "up":
                self._history_prev()
                event.stop()
                return
            if event.key == "down":
                self._history_next()
                event.stop()
                return

    def on_paste(self, event) -> None:
        event.stop()
        if hasattr(event, "prevent_default"):
            event.prevent_default()

    def on_input_changed(self, event: Input.Changed) -> None:
        if self._suppress_input_change > 0:
            self._suppress_input_change -= 1
            return
        if event.input.id == "command":
            value = event.input.value or ""
            if "\n" in value or "\r" in value:
                first = value.splitlines()[0]
                self._suppress_input_change += 1
                event.input.value = first
                event.input.cursor_position = len(first)
                self._log.log_line("[Paste] multi-line truncated to first line", "warn")
                try:
                    event.input.refresh()
                except Exception:
                    pass
                try:
                    self.refresh()
                except Exception:
                    pass
                return
            self._update_hint_for_value(value)
        if self._suppress_reset > 0:
            self._suppress_reset -= 1
            return
        self._completion.reset()
        if self._hint_text and not self._completion.active:
            self._show_hint(self._hint_text)

    def _history_prev(self):
        if not self._history:
            return
        self._history_index = max(0, self._history_index - 1)
        cmd = self._history[self._history_index]
        inp = self.query_one("#command", Input)
        inp.value = cmd
        inp.cursor_position = len(cmd)

    def _history_next(self):
        if not self._history:
            return
        self._history_index = min(len(self._history), self._history_index + 1)
        if self._history_index >= len(self._history):
            cmd = ""
        else:
            cmd = self._history[self._history_index]
        inp = self.query_one("#command", Input)
        inp.value = cmd
        inp.cursor_position = len(cmd)

    def on_backend_message(self, msg):
        mtype = msg.get("type")
        if mtype == "log":
            self._log.log_line(msg.get("text", ""), msg.get("level", "message"))
        elif mtype == "stream":
            self._log.log_stream(
                msg.get("stream", "stdout"),
                msg.get("text", ""),
            )
        elif mtype == "state":
            self._asm_offset = int(msg.get("asm_offset", self._asm_offset))
            self._update_state(msg.get("asm", []), msg.get("abs", []), msg.get("force_address"))
        elif mtype == "complete":
            self._completion.handle_complete(msg)
        elif mtype == "status":
            state = msg.get("state")
            if state == "running":
                cmd = msg.get("cmd", "")
                self._log.log_line(f"[Running] {cmd}", "info")
            elif state == "idle":
                self._log.log_line("[Done]", "info")
        elif mtype == "pid":
            self._backend_pid = msg.get("pid")
        elif mtype == "theme":
            name = msg.get("name", "")
            if not name:
                return
            if name not in getattr(self, "available_themes", {}):
                self._log.log_line(f"[Theme] unknown: {name}", "warn")
                self.action_theme_list()
                return
            self._config.set_theme(name)
        elif mtype == "theme_cycle":
            self.action_cycle_theme()
        elif mtype == "theme_list":
            self.action_theme_list()
        elif mtype == "config_save":
            if self._config.save_config():
                self._log.log_line("[Config] saved", "info")
            else:
                self._log.log_line("[Config] save failed", "warn")
        elif mtype == "config_load":
            if self._config.load_config():
                self._apply_layout()
                self._log.log_line("[Config] loaded", "info")
            else:
                self._log.log_line("[Config] load failed", "warn")
        elif mtype == "hint":
            cmd = msg.get("cmd", "")
            text = msg.get("text", "")
            if cmd and cmd == self._hint_cmd:
                if text:
                    self._show_hint(text)
                else:
                    self._clear_hint()
        elif mtype == "prompt":
            self._prompt_id = msg.get("id")
            self._log.log_line(msg.get("text", ""), "message")
        elif mtype == "ui_exit":
            self.exit()

    def on_backend_disconnect(self):
        self._log.log_line("[backend disconnected]", "warn")
        self.exit()

    def on_unmount(self) -> None:
        if self._backend:
            self._backend.send({"type": "shutdown"})
        try:
            os.system("stty sane")
        except Exception:
            pass

    def on_button_pressed(self, event) -> None:
        if getattr(event.button, "id", None) == "memory_follow":
            self._asm_view.on_follow_pressed()

    def _format_lines(self, lines):
        return self._summary._format_lines(lines)

    def _update_state(self, asm_lines, abs_lines, force_address=None):
        self._asm_view.update(asm_lines, force_address=force_address)
        self._summary.update(abs_lines)

    def action_interrupt(self) -> None:
        inp = self.query_one("#command", Input)
        if inp.value:
            inp.value = ""
            inp.cursor_position = 0
            self._completion.reset()
            return
        self._log.log_line("[Interrupt]", "warn")
        if self._backend_pid:
            try:
                os.kill(self._backend_pid, signal.SIGINT)
                return
            except Exception:
                pass
        if self._backend:
            self._backend.send({"type": "interrupt"})

    def action_clear_console(self) -> None:
        self._log.clear()
        self._completion.clear_suggest()

    def action_console_page_up(self) -> None:
        self._log.page_console(-1)

    def action_console_page_down(self) -> None:
        self._log.page_console(1)

    def action_console_follow(self) -> None:
        self._log.follow_all()

    def action_toggle_force_addr(self) -> None:
        if self._backend:
            self._backend.send({"type": "force_toggle"})

    def action_force_addr_up(self) -> None:
        if self._backend:
            self._backend.send({"type": "force_move", "delta": -2})

    def action_force_addr_down(self) -> None:
        if self._backend:
            self._backend.send({"type": "force_move", "delta": 2})

    def action_toggle_theme(self) -> None:
        self._config.toggle_theme()

    def action_cycle_theme(self) -> None:
        self._config.cycle_theme()

    def action_theme_list(self) -> None:
        self._config.theme_list()

    def action_tab_complete(self) -> None:
        self._completion.tab_complete(backward=False)

    def action_tab_complete_prev(self) -> None:
        self._completion.tab_complete(backward=True)


    def on_mouse_scroll_up(self, event) -> None:
        try:
            self._log_scroll_event(event, "up")
            target = self._resolve_scroll_target(event)
            if self._completion.active and target == "suggest":
                self._completion.scroll(-1)
                return
            if target == "console":
                self._log.scroll_console(-3)
                return
            if target == "stream":
                self._log.scroll_stream(-3)
                return
            if target == "memory":
                self._scroll_memory_offset(-3)
                return
            if target == "summary":
                self._scroll_panel("summary", -3)
                return
        except Exception:
            pass

    def on_mouse_scroll_down(self, event) -> None:
        try:
            self._log_scroll_event(event, "down")
            target = self._resolve_scroll_target(event)
            if self._completion.active and target == "suggest":
                self._completion.scroll(1)
                return
            if target == "console":
                self._log.scroll_console(3)
                return
            if target == "stream":
                self._log.scroll_stream(3)
                return
            if target == "memory":
                self._scroll_memory_offset(3)
                return
            if target == "summary":
                self._scroll_panel("summary", 3)
                return
        except Exception:
            pass

    def _begin_drag(self, kind: str, screen_x: int, screen_y: int) -> None:
        self._dragging = kind
        if kind == "vsplit":
            self._drag_start = screen_x
            self._drag_origin = self._left_width
        elif kind == "vsplit_bottom":
            self._drag_start = screen_x
            self._drag_origin = self._right_width
        elif kind == "hsplit":
            self._drag_start = screen_y
            self._drag_origin = self._bottom_height

    def _update_drag(self, screen_x: int, screen_y: int) -> None:
        if not self._dragging:
            return
        if self._dragging == "vsplit":
            delta = screen_x - self._drag_start
            self._left_width = max(20, min(200, self._drag_origin + delta))
            self._apply_layout()
        elif self._dragging == "vsplit_bottom":
            delta = screen_x - self._drag_start
            self._right_width = max(20, min(120, self._drag_origin - delta))
            self._apply_layout()
        elif self._dragging == "hsplit":
            delta = screen_y - self._drag_start
            self._bottom_height = max(6, min(30, self._drag_origin - delta))
            self._apply_layout()

    def _end_drag(self) -> None:
        self._dragging = None

    def _sanitize_text(self, text: str, cr_to_newline: bool = True) -> str:
        if cr_to_newline:
            text = text.replace("\r", "\n")
        else:
            text = text
        def _keep_char(ch: str) -> bool:
            if ch in ("\n", "\t", "\x1b"):
                return True
            if not cr_to_newline and ch == "\r":
                return True
            return ord(ch) >= 32
        text = "".join(ch for ch in text if _keep_char(ch))
        def _keep_sgr(match):
            seq = match.group(0)
            return seq if seq.endswith("m") else ""
        text = re.sub(r"\x1b\[[0-9;?]*[A-Za-z]", _keep_sgr, text)
        return text

    def _render_console_strip(self, console, content, no_wrap: bool = False):
        try:
            if not console._size_known:
                return None
            renderable = console._make_renderable(content)
            render_options = self.console.options
            if isinstance(renderable, Text) and (no_wrap or not console.wrap):
                render_options = render_options.update(overflow="ignore", no_wrap=True)
            content_width = console.scrollable_content_region.width
            if content_width <= 0:
                content_width = console.size.width
            if content_width <= 0:
                content_width = console.min_width or 1
            render_width = max(content_width, console.min_width)
            render_options = render_options.update_width(render_width)
            segments = self.console.render(renderable, render_options)
            lines = list(Segment.split_lines(segments))
            if not lines:
                return Strip.blank(render_width, console.rich_style)
            strips = Strip.from_lines(lines)
            for strip in strips:
                strip.adjust_cell_length(render_width)
            return strips[0]
        except Exception:
            return None


    def _set_input_text(self, text):
        inp = self.query_one("#command", Input)
        self._suppress_reset += 1
        inp.value = text
        inp.cursor_position = len(text)
        inp.focus()
        self._update_hint_for_value(text)

    def _show_hint(self, text: str) -> None:
        self._hint_text = text
        if self._completion.active:
            return
        suggest = self.query_one("#suggest", Static)
        suggest.update(text)

    def _clear_hint(self) -> None:
        if not self._hint_text and not self._hint_cmd:
            return
        self._hint_text = ""
        self._hint_cmd = ""
        if not self._completion.active:
            self._completion.clear_suggest()

    def _update_hint_for_value(self, value: str) -> None:
        if value.endswith(" "):
            stripped = value.strip()
            if stripped and " " not in stripped:
                if stripped != self._hint_cmd:
                    self._hint_cmd = stripped
                    if self._backend:
                        self._backend.send({"type": "hint", "cmd": stripped})
                return
        self._clear_hint()

    def _scroll_panel(self, panel_id, delta):
        panel = self.query_one(f"#{panel_id}", VerticalScroll)
        try:
            panel.scroll_by(0, delta)
        except Exception:
            pass

    def _scroll_memory_offset(self, delta: int) -> None:
        self._asm_view.scroll_offset(delta)

    def _resolve_scroll_target(self, event) -> str | None:
        if self._path_has_id(event, "suggest"):
            return "suggest"
        if self._path_has_id(event, "console"):
            return "console"
        if self._path_has_id(event, "stream"):
            return "stream"
        if self._path_has_id(event, "memory_scroll") or self._path_has_id(event, "memory_body"):
            return "memory"
        if self._path_has_id(event, "summary"):
            return "summary"
        try:
            widget, _region = self.screen.get_widget_at(event.screen_x, event.screen_y)
        except Exception:
            widget = None
        if self._widget_has_id(widget, "suggest"):
            return "suggest"
        if self._widget_has_id(widget, "console"):
            return "console"
        if self._widget_has_id(widget, "stream"):
            return "stream"
        if self._widget_has_id(widget, "memory_scroll") or self._widget_has_id(widget, "memory_body"):
            return "memory"
        if self._widget_has_id(widget, "summary"):
            return "summary"
        return None

    def _log_scroll_event(self, event, direction: str) -> None:
        now = time.monotonic()
        if now - self._scroll_debug_last < 0.2:
            return
        self._scroll_debug_last = now
        ids = []
        try:
            for widget in event.path:
                wid = getattr(widget, "id", None)
                if wid:
                    ids.append(wid)
        except Exception:
            pass
        wid = getattr(getattr(event, "widget", None), "id", None)
        extra = ""
        try:
            w2, _ = self.screen.get_widget_at(event.screen_x, event.screen_y)
            extra = f" hit={getattr(w2,'id',None)}"
        except Exception:
            pass
        self._log.log_line(f"[ScrollDbg] {direction} widget={wid} path={ids}{extra}", "debug")

    def _widget_has_id(self, widget, widget_id: str) -> bool:
        while widget is not None:
            if getattr(widget, "id", None) == widget_id:
                return True
            widget = getattr(widget, "parent", None)
        return False

    def _path_has_id(self, event, widget_id: str) -> bool:
        try:
            widget = getattr(event, "widget", None)
            if widget is not None and getattr(widget, "id", None) == widget_id:
                return True
            for widget in event.path:
                if getattr(widget, "id", None) == widget_id:
                    return True
            widget = getattr(event, "widget", None)
            while widget is not None:
                if getattr(widget, "id", None) == widget_id:
                    return True
                widget = getattr(widget, "parent", None)
        except Exception:
            pass
        return False

    def _handle_panel_scroll_keys(self, event) -> bool:
        focused = self.focused
        if not isinstance(focused, VerticalScroll):
            return False
        target_id = getattr(focused, "id", "")
        if target_id == "memory_scroll":
            target_id = "memory"
        if target_id not in ("memory", "summary"):
            return False
        if event.key in ("up", "down", "pageup", "pagedown", "home", "end"):
            if target_id == "memory":
                if event.key == "up":
                    self._scroll_memory_offset(-1)
                elif event.key == "down":
                    self._scroll_memory_offset(1)
                elif event.key == "pageup":
                    self._scroll_memory_offset(-max(1, focused.size.height - 2))
                elif event.key == "pagedown":
                    self._scroll_memory_offset(max(1, focused.size.height - 2))
                elif event.key == "home":
                    self._asm_view.set_offset(0)
                elif event.key == "end":
                    pass
            else:
                if event.key == "up":
                    self._scroll_panel(target_id, -1)
                elif event.key == "down":
                    self._scroll_panel(target_id, 1)
                elif event.key == "pageup":
                    self._scroll_panel(target_id, -max(1, focused.size.height - 2))
                elif event.key == "pagedown":
                    self._scroll_panel(target_id, max(1, focused.size.height - 2))
                elif event.key == "home":
                    try:
                        focused.scroll_home(animate=False)
                    except Exception:
                        pass
                elif event.key == "end":
                    try:
                        focused.scroll_end(animate=False)
                    except Exception:
                        pass
            event.stop()
            if hasattr(event, "prevent_default"):
                event.prevent_default()
            return True
        return False

    def _apply_layout(self):
        mem = self.query_one("#memory", Vertical)
        summ = self.query_one("#summary", VerticalScroll)
        bottom = self.query_one("#bottom", Vertical)
        stream = self.query_one("#stream", SelectableRichLog)
        console = self.query_one("#console", SelectableRichLog)
        mem.styles.width = self._left_width
        summ.styles.width = "1fr"
        bottom.styles.height = self._bottom_height
        stream.styles.width = self._right_width
        console.styles.width = "1fr"

    def action_shrink_left(self) -> None:
        self._left_width = max(20, self._left_width - 2)
        self._apply_layout()

    def action_grow_left(self) -> None:
        self._left_width = min(200, self._left_width + 2)
        self._apply_layout()

    def action_shrink_bottom(self) -> None:
        self._bottom_height = max(6, self._bottom_height - 1)
        self._apply_layout()

    def action_grow_bottom(self) -> None:
        self._bottom_height = min(30, self._bottom_height + 1)
        self._apply_layout()

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--ipc", required=True)
    args = parser.parse_args()
    if App is None:
        print("textual not installed, please install textual first.")
        return 1
    app = XSPdbTextualApp(args.ipc)
    app.run(mouse=True)
    return 0


if __name__ == "__main__":
    sys.exit(main())
