try:
    from textual.containers import Horizontal, Vertical, VerticalScroll
    from textual.widgets import Static, Button
except Exception:
    Horizontal = object
    Vertical = object
    VerticalScroll = object
    Static = object
    Button = object

from .widgets import CommandInput, DragSplitter, SelectableRichLog


def compose_layout():
    with Vertical(id="top"):
        with Horizontal():
            mem = Vertical(id="memory")
            with mem:
                with Horizontal(id="memory_header"):
                    yield Static("Memory Disassembly", id="memory_title")
                    yield Button("Follow", id="memory_follow", variant="primary")
                mem_scroll = VerticalScroll(id="memory_scroll")
                mem_scroll.can_focus = True
                with mem_scroll:
                    yield Static("", id="memory_body")
            vsplit = DragSplitter("vsplit", id="vsplit")
            vsplit.can_focus = True
            yield vsplit
            summ = VerticalScroll(id="summary")
            summ.can_focus = True
            with summ:
                yield Static("Summary Information", id="summary_title")
                yield Static("", id="summary_body")
    hsplit = DragSplitter("hsplit", id="hsplit")
    hsplit.can_focus = True
    yield hsplit
    with Vertical(id="bottom"):
        with Horizontal(id="bottom_split"):
            console = SelectableRichLog(id="console", max_lines=5000)
            console.can_focus = False
            yield console
            vsplit = DragSplitter("vsplit_bottom", id="vsplit_bottom")
            vsplit.can_focus = True
            yield vsplit
            stream = SelectableRichLog(id="stream", max_lines=5000)
            stream.can_focus = False
            yield stream
        yield Static("", id="suggest")
        yield CommandInput(placeholder="(xiangshan)", id="command")
