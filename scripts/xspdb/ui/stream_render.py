try:
    from textual.geometry import Size
    from textual.strip import Strip
    from rich.text import Text
    from rich.segment import Segment
except Exception:
    Size = None
    Strip = None
    Text = None
    Segment = None


class StreamRenderer:
    def __init__(self, console, render_strip):
        self._console = console
        self._render_strip = render_strip
        self._last_stream = None
        self._line_index = None
        self._line_buf = []
        self._cursor = 0

    def reset(self) -> None:
        self._last_stream = None
        self._line_index = None
        self._line_buf = []
        self._cursor = 0

    def render(self, stream: str, text: str) -> bool:
        if not text:
            return False
        changed = False
        if self._last_stream is not None and stream != self._last_stream:
            self._commit_line()
        self._last_stream = stream
        for ch in text:
            if ch == "\r":
                self._ensure_line(stream)
                self._cursor = 0
            elif ch == "\n":
                self._ensure_line(stream)
                changed |= self._update_line(stream)
                self._commit_line()
            else:
                self._ensure_line(stream)
                if self._cursor < len(self._line_buf):
                    self._line_buf[self._cursor] = ch
                else:
                    if self._cursor > len(self._line_buf):
                        self._line_buf.extend([" "] * (self._cursor - len(self._line_buf)))
                    self._line_buf.append(ch)
                self._cursor += 1
                changed |= self._update_line(stream)
        return changed

    def _label(self, stream: str) -> str:
        return "[stdout] " if stream == "stdout" else "[stderr] "

    def _ensure_line(self, stream: str) -> None:
        if self._line_index is not None:
            return
        self._line_buf = []
        self._cursor = 0
        label = self._label(stream)
        renderable = Text.from_ansi(label) if Text is not None else label
        self._console.write(renderable)
        self._line_index = len(self._console.lines) - 1

    def _update_line(self, stream: str) -> bool:
        if self._line_index is None:
            return False
        label = self._label(stream)
        content = "".join(self._line_buf)
        renderable = Text.from_ansi(label + content) if Text is not None else (label + content)
        strip = self._render_strip(self._console, renderable, no_wrap=True)
        if strip is None:
            self._console.write(renderable)
            self._line_index = len(self._console.lines) - 1
            return True
        self._console.lines[self._line_index] = strip
        try:
            self._console._line_cache.clear()
            self._console._widest_line_width = max(self._console._widest_line_width, strip.cell_length)
            self._console.virtual_size = Size(self._console._widest_line_width, len(self._console.lines))
        except Exception:
            pass
        return True

    def _commit_line(self) -> None:
        self._line_index = None
        self._line_buf = []
        self._cursor = 0
