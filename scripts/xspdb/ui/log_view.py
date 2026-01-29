import threading

try:
    from rich.text import Text
except Exception:
    Text = None

from .stream_render import StreamRenderer


class LogView:
    _LEVEL_ORDER = {
        "debug": 10,
        "message": 20,
        "info": 20,
        "warn": 30,
        "warning": 30,
        "error": 40,
    }

    def __init__(self, app, level_prefix, level_style, sanitize_fn, render_strip_fn, min_level="info"):
        self._app = app
        self._level_prefix = level_prefix
        self._level_style = level_style
        self._sanitize = sanitize_fn
        self._render_strip = render_strip_fn
        self._min_level = min_level
        self._log_console = None
        self._stream_console = None
        self._stream_renderer = None
        self._console_follow = True
        self._stream_follow = True
        self._console_pause_notified = False
        self._stream_pause_notified = False
        self._buffer = []
        self._lock = threading.Lock()
        self._dirty = False

    def attach(self, log_console, stream_console) -> None:
        self._log_console = log_console
        self._stream_console = stream_console
        self._stream_renderer = StreamRenderer(stream_console, self._render_strip)

    def set_min_level(self, level: str) -> None:
        if level:
            self._min_level = level

    def _level_allowed(self, level: str) -> bool:
        min_order = self._LEVEL_ORDER.get(self._min_level, 20)
        level_order = self._LEVEL_ORDER.get(level, 20)
        return level_order >= min_order

    def log_line(self, text, level="message", replace: bool = False) -> None:
        if not text:
            return
        if not self._level_allowed(level):
            return
        prefix = self._level_prefix.get(level, "")
        if prefix:
            text = prefix + text
        text = self._sanitize(text)
        self._enqueue(("log", text, level, replace))

    def log_stream(self, stream: str, text: str) -> None:
        if not text:
            return
        text = self._sanitize(text, cr_to_newline=False)
        self._enqueue(("stream", stream, text))

    def flush(self) -> None:
        if not self._dirty:
            return
        if self._log_console is None:
            return
        with self._lock:
            if not self._buffer:
                self._dirty = False
                return
            items = self._buffer[:]
            self._buffer.clear()
            self._dirty = False

        log_console = self._log_console
        stream_console = self._stream_console
        replaced_log = False
        replaced_stream = False

        for item in items:
            kind = item[0]
            if kind == "log":
                _kind, text, level, replace = item
                line = self._make_text(text, level)
                if replace and log_console.lines:
                    strip = self._render_strip(log_console, line, no_wrap=True)
                    if strip is not None:
                        log_console.lines[-1] = strip
                        log_console._line_cache.clear()
                        log_console._widest_line_width = max(log_console._widest_line_width, strip.cell_length)
                        try:
                            from textual.geometry import Size
                            log_console.virtual_size = Size(log_console._widest_line_width, len(log_console.lines))
                        except Exception:
                            pass
                        replaced_log = True
                    else:
                        log_console.write(line)
                else:
                    log_console.write(line)
            elif kind == "stream":
                _kind, stream, text = item
                if self._stream_renderer is None and stream_console is not None:
                    self._stream_renderer = StreamRenderer(stream_console, self._render_strip)
                if self._stream_renderer is not None:
                    replaced_stream |= self._stream_renderer.render(stream, text)
                elif stream_console is not None:
                    stream_console.write(text)

        if replaced_log:
            try:
                log_console.refresh()
            except Exception:
                pass
        if replaced_stream and stream_console is not None:
            try:
                stream_console.refresh()
            except Exception:
                pass

        if self._console_follow and log_console is not None:
            self._scroll_end(log_console)
        if self._stream_follow and stream_console is not None:
            self._scroll_end(stream_console)

    def clear(self) -> None:
        if self._log_console is not None:
            self._log_console.clear()
        if self._stream_console is not None:
            self._stream_console.clear()
        self._console_follow = True
        self._stream_follow = True
        self._console_pause_notified = False
        self._stream_pause_notified = False
        if self._stream_renderer is not None:
            self._stream_renderer.reset()

    def pause_console_follow(self) -> None:
        if self._console_follow:
            self._console_follow = False
        if not self._console_pause_notified:
            self._console_pause_notified = True
            self._enqueue(("log", self._sanitize("[Console paused] Press End to follow.\n"), "message", False))

    def pause_stream_follow(self) -> None:
        if self._stream_follow:
            self._stream_follow = False
        if not self._stream_pause_notified:
            self._stream_pause_notified = True
            self._enqueue(("log", self._sanitize("[Stream paused] Press End to follow.\n"), "message", False))

    def follow_all(self) -> None:
        self._console_follow = True
        self._stream_follow = True
        self._console_pause_notified = False
        self._stream_pause_notified = False
        if self._log_console is not None:
            self._scroll_end(self._log_console)
        if self._stream_console is not None:
            self._scroll_end(self._stream_console)

    def scroll_console(self, delta: int) -> None:
        if self._log_console is None:
            return
        self.pause_console_follow()
        try:
            self._log_console.scroll_by(0, delta)
        except Exception:
            pass

    def scroll_stream(self, delta: int) -> None:
        if self._stream_console is None:
            return
        self.pause_stream_follow()
        try:
            self._stream_console.scroll_by(0, delta)
        except Exception:
            pass

    def page_console(self, delta: int) -> None:
        if self._log_console is None:
            return
        self.pause_console_follow()
        try:
            if delta < 0:
                self._log_console.scroll_page_up()
            else:
                self._log_console.scroll_page_down()
        except Exception:
            try:
                self._log_console.scroll_by(0, delta)
            except Exception:
                pass

    def _enqueue(self, item) -> None:
        with self._lock:
            self._buffer.append(item)
            self._dirty = True

    def _make_text(self, text: str, level: str):
        if Text is None:
            return text
        style = self._level_style.get(level)
        if style:
            return Text.from_ansi(text, style=style)
        return Text.from_ansi(text)

    def _scroll_end(self, console) -> None:
        try:
            console.scroll_end(animate=False)
        except Exception:
            try:
                console.scroll_end()
            except Exception:
                pass
