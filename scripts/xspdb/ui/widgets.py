try:
    from textual.widgets import Input, RichLog, Static
    from textual.strip import Strip
    from textual.selection import Selection
    from rich.style import Style
except Exception:
    Input = object
    RichLog = object
    Static = object
    Strip = object
    Selection = object
    Style = object


class DragSplitter(Static):
    def __init__(self, kind: str, **kwargs):
        super().__init__(**kwargs)
        self._kind = kind

    def on_mouse_down(self, event) -> None:
        event.stop()
        self.capture_mouse(True)
        try:
            self.app._begin_drag(self._kind, event.screen_x, event.screen_y)
        except Exception:
            pass

    def on_mouse_move(self, event) -> None:
        if getattr(self.app, "_dragging", None) != self._kind:
            return
        event.stop()
        try:
            self.app._update_drag(event.screen_x, event.screen_y)
        except Exception:
            pass

    def on_mouse_up(self, event) -> None:
        if getattr(self.app, "_dragging", None) == self._kind:
            event.stop()
            try:
                self.app._end_drag()
            except Exception:
                pass
        try:
            self.capture_mouse(False)
        except Exception:
            pass


class CommandInput(Input):
    def on_paste(self, event) -> None:
        text = event.text or ""
        if not text:
            event.stop()
            if hasattr(event, "prevent_default"):
                event.prevent_default()
            return
        first = text.splitlines()[0]
        try:
            self.insert_text_at_cursor(first)
        except Exception:
            self.value = (self.value or "") + first
            self.cursor_position = len(self.value)
        if "\n" in text:
            app = getattr(self, "app", None)
            if app is not None and hasattr(app, "_log"):
                app._log.log_line("[Paste] multi-line truncated to first line", "warn")
        event.stop()
        if hasattr(event, "prevent_default"):
            event.prevent_default()


class SelectableRichLog(RichLog):
    ALLOW_SELECT = False
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self._manual_selecting = False
        self._manual_start = None

    def selection_updated(self, selection: Selection | None) -> None:
        self.refresh()
        try:
            if selection is None:
                self.app._log.log_line("[SelDbg] console selection cleared", "debug")
            else:
                self.app._log.log_line(f"[SelDbg] console selection {selection}", "debug")
        except Exception:
            pass

    def get_selection(self, selection: Selection):
        try:
            text = "\n".join(line.text for line in self.lines)
        except Exception:
            return None
        if not text:
            return None
        return selection.extract(text), "\n"

    def render_line(self, y: int) -> Strip:
        strip = super().render_line(y)
        try:
            selection = self.text_selection
            if selection is None:
                return strip
            content_y = int(self.scroll_y) + y
            span = selection.get_span(content_y)
            if span is None:
                return strip
            start, end = span
            if end == -1:
                end = strip.cell_length
            if start < 0:
                start = 0
            if end <= start:
                return strip
            selection_style = Style(reverse=True)
            left = strip.crop(0, start)
            mid = strip.crop(start, end).apply_style(selection_style)
            right = strip.crop(end, strip.cell_length)
            segments = left._segments + mid._segments + right._segments
            return Strip(segments, strip.cell_length)
        except Exception:
            return strip

    def on_mouse_down(self, event) -> None:
        if event.button != 1:
            return
        try:
            offset = event.get_content_offset_capture(self)
        except Exception:
            return
        start = self._to_content_offset(offset)
        if start is None:
            return
        self._manual_selecting = True
        self._manual_start = start
        self._set_manual_selection(start, start)
        try:
            self.capture_mouse(True)
        except Exception:
            pass
        if hasattr(event, "prevent_default"):
            event.prevent_default()
        event.stop()

    def on_mouse_move(self, event) -> None:
        if not self._manual_selecting or self._manual_start is None:
            return
        try:
            offset = event.get_content_offset_capture(self)
        except Exception:
            return
        end = self._to_content_offset(offset)
        if end is None:
            return
        self._set_manual_selection(self._manual_start, end)
        if hasattr(event, "prevent_default"):
            event.prevent_default()
        event.stop()

    def on_mouse_up(self, event) -> None:
        if not self._manual_selecting:
            return
        self._manual_selecting = False
        try:
            self.capture_mouse(False)
        except Exception:
            pass
        if hasattr(event, "prevent_default"):
            event.prevent_default()
        event.stop()

    def _to_content_offset(self, offset):
        try:
            x = max(0, int(offset.x))
            y = max(0, int(offset.y)) + int(self.scroll_y)
            return offset.__class__(x, y)
        except Exception:
            return None

    def _set_manual_selection(self, start, end) -> None:
        try:
            sel = Selection.from_offsets(start, end)
            self.screen.selections = {self: sel}
        except Exception:
            pass
