"""Exercise metadata phase orchestration offline, without creating a DUT.

Subordinate diagnostics are replaced with bounded phases; the real one-run
entrypoint must publish their witnesses and preserve all error boundaries.
"""
from types import SimpleNamespace

import pytest


_COMPONENTS = (
    ("test_ifu_exception_metadata_satp_without_fault_diagnostic", {"satp_flush"}),
    ("test_ifu_exception_metadata_vs_nonleaf_gpf_diagnostic", {"gpaddr", "vs_nonleaf_pte"}),
    ("test_ifu_exception_metadata_cross_page_fault_diagnostic", {"cross_page"}),
    ("test_ifu_exception_metadata_backend_fault_diagnostic", {"backend_exception"}),
)
_EXPECTED = {"satp_flush", "gpaddr", "vs_nonleaf_pte", "cross_page", "backend_exception"}


def _orchestration(monkeypatch, failure=None):
    from tests.py.jiabowen import test_ifu_exception_metadata_v3_dut as diagnostic

    checks, witnesses, gaps = set(), {}, set()
    coverage = SimpleNamespace(
        _ifu_exception_metadata_checks=checks,
        _ifu_exception_metadata_witnesses=witnesses,
        _ifu_exception_metadata_gaps=gaps,
        key_hit=lambda group, bin_name: (
            group == "ifu_v3_pipeline_owner_model" and bin_name == "owner_leaf_056"
            and checks == _EXPECTED
        ),
    )
    coverage.toffee_sink = coverage
    run_errors, monitor_errors, clears, disarms, events, calls = [], [], [], [], [], []
    env = SimpleNamespace(
        functional_coverage=coverage,
        current_cycle=10,
        backend_model=SimpleNamespace(commit_ptr_flag=0, commit_ptr_value=7),
        get_errors=lambda: list(run_errors),
        _emit_event=lambda name, payload: events.append((name, payload)),
    )

    def clear():
        clears.append(env.current_cycle)
        monitor_errors.clear()

    env.monitor = SimpleNamespace(clear=clear, get_errors=lambda: list(monitor_errors))
    env.translation_oracle = SimpleNamespace(
        get_stats=lambda: {"error_count": 0},
        disarm=lambda: disarms.append(env.current_cycle),
    )
    for index, (name, components) in enumerate(_COMPONENTS):
        def phase(active_env, index=index, name=name, components=components):
            assert active_env is env
            calls.append(name)
            if failure == "monitor-before-clear" and index == 0:
                monitor_errors.append("retained monitor violation")
            active_env.monitor.clear()
            if failure == "phase-error" and index == 1:
                raise RuntimeError("subordinate diagnostic failed")
            active_env.current_cycle += 5
            for component in components:
                if failure == "missing-component" and component == "vs_nonleaf_pte":
                    continue
                checks.add(component)
                witnesses[component] = {"cycle": active_env.current_cycle, "phase": name}
            if failure == "run-error" and index == 1:
                run_errors.append("retained checker violation")
        phase.__name__ = name
        monkeypatch.setattr(diagnostic, name, phase)
    return diagnostic, env, SimpleNamespace(
        calls=calls, clears=clears, disarms=disarms, events=events,
        run_errors=run_errors, monitor_errors=monitor_errors,
    )


def test_one_run_metadata_orchestration_publishes_same_run_phase_evidence(monkeypatch):
    diagnostic, env, seen = _orchestration(monkeypatch)
    diagnostic.test_ifu_exception_metadata_all_components_one_run(env, monkeypatch)

    assert seen.calls == [name for name, _ in _COMPONENTS]
    assert len(seen.clears) == len(seen.disarms) == 4
    starts = [payload["phase"] for name, payload in seen.events
              if name == "ifu.exception_metadata_phase_start"]
    assert starts == seen.calls
    checkpoints = [payload for name, payload in seen.events
                   if name == "ifu.exception_metadata_one_run_checkpoint"]
    checkpoint, = checkpoints
    assert checkpoint["same_recorder_and_dut"] is True
    assert set(checkpoint["component_witnesses"]) == _EXPECTED
    assert [len(item["component_witnesses"]) for item in checkpoint["phases"]] == [1, 3, 4, 5]
    assert all(item["monitor_errors"] == item["translation_error_count"] == 0
               and item["missing_probes"] == [] for item in checkpoint["phases"])


@pytest.mark.parametrize("failure", [
    "monitor-before-clear", "phase-error", "run-error", "missing-component",
])
def test_one_run_metadata_orchestration_never_clears_or_accepts_failed_phase(monkeypatch, failure):
    diagnostic, env, seen = _orchestration(monkeypatch, failure)
    with pytest.raises(RuntimeError if failure == "phase-error" else AssertionError):
        diagnostic.test_ifu_exception_metadata_all_components_one_run(env, monkeypatch)
    assert not any(name == "ifu.exception_metadata_one_run_checkpoint" for name, _ in seen.events)
    if failure == "monitor-before-clear":
        assert seen.monitor_errors == ["retained monitor violation"]
        assert seen.clears == seen.disarms == []
    elif failure in {"phase-error", "run-error"}:
        assert len(seen.calls) == 2 and len(seen.disarms) == 1
        if failure == "run-error":
            assert seen.run_errors == ["retained checker violation"]
    else:
        assert len(seen.calls) == len(seen.disarms) == 4
        assert "vs_nonleaf_pte" not in env.functional_coverage._ifu_exception_metadata_checks
