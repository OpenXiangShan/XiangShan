from __future__ import annotations

from types import SimpleNamespace

from env import api as env_api


class _ApiTargetCursorEnv:
    def __init__(self) -> None:
        self.backend_model = SimpleNamespace()


def test_run_until_golden_complete_passes_target_cursor_env_var(monkeypatch) -> None:
    captured = {}

    def fake_run_until_golden_trace_complete(env, **kwargs):
        captured["env"] = env
        captured.update(kwargs)
        return SimpleNamespace(
            ok=True,
            completed=True,
            status="cursor_target",
            cycles_run=5,
            cursor=12,
            total_entries=99,
            pending_work=0,
            monitor_error_count=0,
        )

    monkeypatch.setenv("TB_TRACE_TARGET_CURSOR", "12")
    monkeypatch.setattr(env_api, "run_until_golden_trace_complete", fake_run_until_golden_trace_complete)

    env = _ApiTargetCursorEnv()
    result = env_api.api_Frontend_run_until_golden_complete(env, max_cycles=100)

    assert result is True
    assert captured["env"] is env
    assert captured["target_cursor"] == 12
