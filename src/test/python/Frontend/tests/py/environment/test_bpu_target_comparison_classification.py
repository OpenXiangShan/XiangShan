"""Policy suppression cannot be counted as observed full target equality."""
from dataclasses import replace
import importlib.util
from pathlib import Path
from types import SimpleNamespace

import pytest

from env.support.bpu_v3_contract import BpuTargetDiffCycle


def _sample(**overrides):
    result = BpuTargetDiffCycle(
        s3_valid=1, s3_taken=1, s2_taken=1, s2_override=0,
        s1_target=0x40000800, s2_target=0x40000800, s3_branch_type=3,
        s3_ras_action=0, s3_target=0x40000800, s3_start_pc=0x80000040,
        ittage_hit=0, ras_enable=1, s3_first_taken_mask=1, s2_first_taken_mask=1,
        s3_override=0, signal_paths={})
    result = replace(result, **overrides)
    return replace(result, s3_override=result.expected_override)


@pytest.mark.parametrize("sample,kind", [
    (_sample(), "equal"),
    (_sample(s2_override=1, s1_target=0x40000802), "equal"),
    (_sample(s3_target=0x40000802), "diff"),
    (_sample(s2_override=1, s3_target=0x40000802), "suppressed"),
    (_sample(s3_target=0x40800800), "high_only"),
    (_sample(s2_override=1, s3_target=0x40800800), "suppressed"),
    (_sample(s2_override=1, ittage_hit=1, s3_target=0x40000802), "diff"),
    (_sample(s2_override=1, s3_ras_action=1, s3_target=0x40000802), "diff"),
])
def test_full_equality_suppression_low_difference_and_high_difference_are_distinct(sample, kind):
    assert sample.target_comparison_kind == kind
    assert (kind == "equal") == (not sample.full_target_diff)
    if kind == "suppressed":
        assert sample.full_target_diff and not sample.target_diff


def _load_canary():
    # Run the actual staged canary body with a bounded sampled timeline. There
    # is no DUT fixture or simulated pipeline; only classification is supplied.
    path = Path(__file__).parents[1] / "jiabowen/test_bpu_v3_negative_dut.py"
    spec = importlib.util.spec_from_file_location("staged_bpu_classification_canary", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _run_canary(monkeypatch, samples):
    module = _load_canary()
    trace = SimpleNamespace(cursor=0, entries=list(samples))
    program = {"source_pc": 0x80000040}
    events = []
    monitor = SimpleNamespace(clear=lambda: None, set_expected_pc=lambda pc: None,
                              get_errors=lambda: [])
    backend = SimpleNamespace(set_golden_trace=lambda obj: None, get_stats=lambda: {})
    recorder = SimpleNamespace(_try_read_dut_signal=lambda dut, name: None)
    env = SimpleNamespace(load_program=lambda payload, base: None,
                          initialize=lambda **kwargs: None, monitor=monitor,
                          set_bp_ctrl_enable=lambda **kwargs: None, backend_model=backend,
                          functional_coverage=recorder, dut=None, current_cycle=0,
                          get_errors=lambda: [], _emit_event=lambda key, event: events.append(event))
    recorder.env = env

    def step(count):
        env.current_cycle += count
        trace.cursor += count

    env.step = step
    monkeypatch.setattr(module, "build_bpu_target_change_program", lambda base: (b"", trace, program))
    monkeypatch.setattr(module, "sample_bpu_target_diff_cycle", lambda reader: samples[trace.cursor - 1])
    return module, env, events


def test_real_canary_rejects_suppression_as_a_substitute_for_two_equal_cycles(monkeypatch):
    samples = [_sample(s2_override=1, s3_target=0x40000802)] * 2 + [_sample(s3_target=0x40000802)]
    module, env, events = _run_canary(monkeypatch, samples)
    with pytest.raises(AssertionError, match="target-equal"):
        module.test_btb_lower_target_diff_semantics_canary(env)
    assert events[-1]["equal_cycles"] == []
    assert events[-1]["suppressed_cycles"] == [1, 2]
    assert events[-1]["diff_cycles"] == [3]


def test_real_canary_counts_only_full_equal_samples_and_preserves_suppressed_diagnostics(monkeypatch):
    samples = [_sample(), _sample(s2_override=1, s3_target=0x40000802),
               _sample(), _sample(s3_target=0x40000802)]
    module, env, events = _run_canary(monkeypatch, samples)
    module.test_btb_lower_target_diff_semantics_canary(env)
    assert events[-1]["equal_cycles"] == [1, 3]
    assert events[-1]["suppressed_cycles"] == [2]
    assert events[-1]["diff_cycles"] == [4]
