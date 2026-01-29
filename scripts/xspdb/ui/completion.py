import os

try:
    from textual.widgets import Input, Static
    from rich.text import Text
except Exception:
    Input = object
    Static = object
    Text = None


class CompletionController:
    def __init__(self, app):
        self._app = app
        self._items = []
        self._index = -1
        self._line = ""
        self._base = ""
        self._prefix = ""
        self._origin = ""
        self._token = ""
        self._stage = 0  # 0: none, 1: ready(no list), 2: list shown
        self._selected = -1
        self._grid_cols = 0
        self._grid_rows = 0
        self._view_row = 0
        self._visible_rows = 4

    @property
    def active(self) -> bool:
        return self._stage == 2

    def reset(self) -> None:
        self._items = []
        self._index = -1
        self._selected = -1
        self._line = ""
        self._base = ""
        self._prefix = ""
        self._origin = ""
        self._token = ""
        self._stage = 0
        self._grid_cols = 0
        self._grid_rows = 0
        self._view_row = 0
        self._visible_rows = 4
        self.clear_suggest()

    def clear_suggest(self) -> None:
        suggest = self._app.query_one("#suggest", Static)
        suggest.update("")

    def handle_complete(self, msg) -> None:
        line = msg.get("line", "")
        items = msg.get("items", [])
        if not items:
            return
        prefix = os.path.commonprefix(items)
        base = line[: line.rfind(" ") + 1] if " " in line else ""
        token = line[len(base):]
        self._items = items
        self._index = -1
        self._selected = 0
        self._line = line
        self._base = base
        self._prefix = prefix
        self._origin = line
        self._token = token
        self._stage = 2
        self._view_row = 0
        if len(items) == 1:
            self._app._set_input_text(base + items[0])
            self.reset()
            return
        self._app._set_input_text(base + items[0])
        self._show_suggest(items, ensure_visible=True)

    def tab_complete(self, backward: bool = False) -> None:
        inp = self._app.query_one("#command", Input)
        inp.focus()
        line = inp.value
        if self._items and line.startswith(self._base) and self._stage == 2:
            if backward:
                self._selected = (self._selected - 1) % len(self._items)
            else:
                self._selected = (self._selected + 1) % len(self._items)
            self._apply_selection(update_input=True)
            return
        if self._app._backend:
            self._app._backend.send({"type": "complete", "line": line})

    def handle_nav_key(self, key: str) -> bool:
        if key not in ("left", "right", "up", "down"):
            return False
        if not self.active or not self._items:
            return False
        cols = self._grid_cols if self._grid_cols > 0 else 1
        if key == "left":
            self._selected = (self._selected - 1) % len(self._items)
        elif key == "right":
            self._selected = (self._selected + 1) % len(self._items)
        elif key == "up":
            self._selected = (self._selected - cols) % len(self._items)
        else:
            self._selected = (self._selected + cols) % len(self._items)
        self._apply_selection(update_input=True)
        return True

    def handle_enter_key(self, key: str) -> bool:
        if key not in ("enter", "return"):
            return False
        return self.accept_if_active()

    def scroll(self, delta: int) -> None:
        if self._grid_rows <= 0:
            return
        max_start = max(0, self._grid_rows - self._visible_rows)
        self._view_row = max(0, min(max_start, self._view_row + delta))
        self._show_suggest(self._items, ensure_visible=False)

    def accept_if_active(self) -> bool:
        if self._stage == 2 and self._selected >= 0:
            new_line = self._base + self._items[self._selected]
            self._app._set_input_text(new_line)
            self.reset()
            return True
        return False

    def _show_suggest(self, items, ensure_visible: bool = True) -> None:
        suggest = self._app.query_one("#suggest", Static)
        if not items:
            suggest.update("")
            return
        if self._selected < 0:
            self._selected = 0
        if ensure_visible:
            self._ensure_visible()
        suggest.update(self._format_suggest_grid(items))
        self._stage = 2

    def _format_suggest_grid(self, items):
        suggest = self._app.query_one("#suggest", Static)
        width = max(10, suggest.size.width)
        height = max(1, suggest.size.height or 4)
        max_len = max(len(str(i)) for i in items)
        col_w = max_len + 2
        cols = max(1, width // max(1, col_w))
        if cols == 1:
            col_w = min(col_w, width)
        rows = (len(items) + cols - 1) // cols
        self._grid_cols = cols
        self._grid_rows = rows
        visible_rows = min(rows, height)
        self._visible_rows = visible_rows
        max_start = max(0, rows - visible_rows)
        if self._view_row > max_start:
            self._view_row = max_start
        if Text is None:
            lines = []
            for r in range(self._view_row, self._view_row + visible_rows):
                line = []
                for c in range(cols):
                    idx = r * cols + c
                    if idx >= len(items):
                        line.append(" " * col_w)
                        continue
                    label = str(items[idx]).ljust(col_w)
                    if idx == self._selected:
                        label = f"> {label}"
                    line.append(label)
                lines.append(" ".join(line).rstrip())
            return "\n".join(lines)
        grid = Text()
        for r in range(self._view_row, self._view_row + visible_rows):
            row = Text()
            for c in range(cols):
                idx = r * cols + c
                if idx >= len(items):
                    row.append(" " * col_w)
                else:
                    label = str(items[idx]).ljust(col_w)
                    cell = Text(label)
                    if idx == self._selected:
                        cell.stylize("reverse bold")
                    row.append(cell)
                if c != cols - 1:
                    row.append(" ")
            grid.append(row)
            if r != self._view_row + visible_rows - 1:
                grid.append("\n")
        return grid

    def _apply_selection(self, update_input: bool) -> None:
        if not self._items or self._selected < 0:
            return
        if update_input:
            new_line = self._base + self._items[self._selected]
            self._app._set_input_text(new_line)
        self._show_suggest(self._items, ensure_visible=True)

    def _ensure_visible(self) -> None:
        if self._selected < 0 or self._grid_rows <= 0:
            return
        cols = max(1, self._grid_cols)
        row = self._selected // cols
        if row < self._view_row:
            self._view_row = row
        elif row >= self._view_row + self._visible_rows:
            self._view_row = max(0, row - self._visible_rows + 1)
