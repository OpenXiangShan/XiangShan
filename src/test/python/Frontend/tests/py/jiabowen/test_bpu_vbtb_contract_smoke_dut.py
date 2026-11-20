"""Current-tree #6587 VBTB native contract smoke.

This is a probe/binding and replacement contract control. It does not claim a
functional-coverage HIT. Run only while coord holds the serialized DUT slot.
"""
from __future__ import annotations

import json
import os
from dataclasses import asdict
from pathlib import Path

import pytest

from env.funcov.registry import default_pilot_csv_path
from env.runtime.artifact_provenance import file_sha256
from env.runtime.pylib import frontend_build_root_path
from env.support.bpu_vbtb_contract import (
    check_vbtb_cycle,
    check_vbtb_transition,
    sample_vbtb_cycle,
)


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_BASE = 0x8000_0000
_NOP = 0x0000_0013


def _cycle_limit() -> int:
    raw = os.getenv("TB_VBTB_SMOKE_MAX_CYCLES", "1280").strip()
    value = int(raw, 0)
    assert value > 0, "TB_VBTB_SMOKE_MAX_CYCLES must be positive"
    return value


@pytest.mark.skipif(not _RUN_DUT, reason="requires coord serialized DUT slot")
def test_bpu_vbtb_contract_smoke(env) -> None:
    env.load_program(_NOP.to_bytes(4, "little") * 4096, _BASE)
    env.initialize(reset_vector=_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_BASE)

    recorder = env.functional_coverage
    assert recorder is not None and recorder.env is env
    reader = lambda name: recorder._try_read_dut_signal(env.dut, name)

    samples = []
    previous = None
    active_cycles = []

    def observe(cycle: int, _env) -> None:
        nonlocal previous
        sample = sample_vbtb_cycle(reader)
        check_vbtb_cycle(sample)
        if previous is not None:
            prior_cycle, prior_sample = previous
            check_vbtb_transition(
                prior_sample,
                sample,
                before_cycle=prior_cycle,
                after_cycle=cycle,
                reset=False,
            )
        previous = (cycle, sample)
        if sample.values["s3_fire"]:
            active_cycles.append(int(cycle))
        samples.append({"cycle": int(cycle), **asdict(sample)})

    env.register_cycle_observer(observe)
    env.step(_cycle_limit())

    assert samples, "VBTB smoke produced no cycle samples"
    assert active_cycles, "VBTB smoke never reached active BPU S3"
    assert env.get_errors() == []
    assert env.ptw_full_ppn_checker.error_count == 0
    assert env.ptw_resp_input_checker.error_count == 0
    assert env.monitor.get_errors() == []
    assert env.translation_oracle.errors == []

    build_root = frontend_build_root_path()
    manifest = build_root / "frontend_build_manifest.verilator.json"
    assert manifest.is_file(), "missing frozen build provenance"

    run_id = os.environ.get("TB_RUN_ID", "directed-vbtb-contract-smoke")
    artifact_root = Path(os.getenv("TB_ARTIFACT_DIR", str(build_root / "artifacts")))
    output = artifact_root / run_id / "bpu_vbtb_contract_smoke.json"
    output.parent.mkdir(parents=True, exist_ok=True)
    provenance = dict(recorder.provenance)
    report = {
        "schema_version": 1,
        "kind": "migration_control_smoke",
        "exact_target_hit": False,
        "canonical_hit_eligible": False,
        "run_id": run_id,
        "testcase_nodeid": (
            "test_bpu_vbtb_contract_smoke_dut.py::"
            "test_bpu_vbtb_contract_smoke"
        ),
        "testcase_sha256": file_sha256(Path(__file__)),
        "build_manifest": str(manifest),
        "build_manifest_sha256": file_sha256(manifest),
        "runtime_provenance": provenance,
        "registry_sha256": file_sha256(default_pilot_csv_path()),
        "checker_errors": env.get_errors(),
        "monitor_errors": env.monitor.get_errors(),
        "oracle_errors": env.translation_oracle.errors,
        "contract_samples_checked": len(samples),
        "active_s3_cycles": active_cycles,
        "signal_paths": samples[0]["signal_paths"],
        "samples": samples,
    }
    output.write_text(json.dumps(report, indent=2, sort_keys=True) + "\n")
