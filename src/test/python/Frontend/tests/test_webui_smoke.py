import importlib
import sys


def test_webui_server_imports_without_compiled_frontend(monkeypatch):
    monkeypatch.delitem(sys.modules, "Frontend", raising=False)
    monkeypatch.delitem(sys.modules, "webui", raising=False)
    monkeypatch.delitem(sys.modules, "webui.server", raising=False)
    monkeypatch.delitem(sys.modules, "webui.runner", raising=False)

    server = importlib.import_module("webui.server")

    assert server.app.title == "Frontend Interactive Console"
    assert server.runner.snapshot()["state"] == "idle"
