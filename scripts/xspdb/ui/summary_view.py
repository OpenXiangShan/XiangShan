try:
    from rich.text import Text
    from textual.widgets import Static
except Exception:
    Text = None
    Static = object


class SummaryView:
    def __init__(self, app, style_map):
        self._app = app
        self._style_map = style_map
        self._summary_body = None

    def attach(self, summary_body: Static) -> None:
        self._summary_body = summary_body

    def update(self, lines) -> None:
        if self._summary_body is None:
            return
        self._summary_body.update(self._format_lines(lines))

    def _format_lines(self, lines):
        parts = []
        for item in lines:
            if isinstance(item, (tuple, list)) and len(item) == 2:
                style, txt = item
                txt = str(txt)
                if "|0x" in txt:
                    txt = txt.replace("|0x", " 0x", 1)
                if Text is not None:
                    parts.append(Text(txt, style=self._style_map.get(style, style)))
                else:
                    parts.append(txt)
            else:
                txt = str(item)
                if "|0x" in txt:
                    txt = txt.replace("|0x", " 0x", 1)
                if Text is not None:
                    parts.append(Text(txt))
                else:
                    parts.append(txt)
        if not parts:
            return Text("") if Text is not None else ""
        if Text is None:
            return "\n".join(parts)
        return Text("\n").join(parts)
