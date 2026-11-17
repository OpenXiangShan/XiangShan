from __future__ import annotations

import json
from pathlib import Path

from .cj_offset_sequence_audit import build_report, write_json_exclusive


def _write(path: Path, payload: dict) -> str:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(payload), encoding="utf-8")
    import hashlib

    return hashlib.sha256(path.read_bytes()).hexdigest()


def _case(tmp_path: Path, ordinal: int, sequence: list[int], *, candidate: bool) -> dict:
    name = f"case-{ordinal}" + ("-cjorp" if candidate else "")
    root = tmp_path / name
    source = root / "source.S"
    binary = root / "program.bin"
    trace = root / "trace.jsonl"
    program_map = root / "program_map.json"
    scenario = root / "scenario.json"
    for path in (source, binary, trace, program_map, scenario):
        _write(path, {"name": name, "path": path.name})
    binary_sha = _write(binary, {"binary": name})
    episodes = [
        {
            "kind": "taken_cj_offset_ring",
            "parameters": {"halfword_offset": offset, "execution_position": position},
        }
        for position, offset in enumerate(sequence)
    ] if candidate else []
    generator = root / "generator.json"
    _write(generator, {"episodes": episodes})
    analysis = root / "analysis.json"
    trace_sha = _write(trace, {"trace": name})
    _write(
        analysis,
        {
            "trace_sha256": trace_sha,
            "program_map_binary_sha256": binary_sha,
            "events": [
                {"event": "trace.taken_cj_offset_ring", "count": 16},
                {"event": "trace.taken_cj_offset_transition", "count": 15},
            ] if candidate else [],
            "program_diversity": {
                "taken_cj_offset_ring_sequence": sequence if candidate else [],
                "taken_cj_offset_ring_offsets": list(range(16)) if candidate else [],
            },
        },
    )
    configuration = root / "configuration.json"
    _write(
        configuration,
        {"status": "valid", "reasons": [], "binary_sha256": binary_sha},
    )
    realization = root / "realization.json"
    _write(
        realization,
        {"status": "REALIZED", "missing_events": [], "scenario_id": name},
    )
    return {
        "name": name,
        "spec": {
            "ordinal": ordinal,
            "seed": 100 + ordinal,
            "directed_pattern": (
                "taken_cj_offset_ring_permuted" if candidate else "none"
            ),
        },
        "timing_seed": 200 + ordinal,
        "execution_status": "completed",
        "exit_code": 0,
        "qualification_pass": True,
        "program_map_status": "valid",
        "scenario_realization_status": "REALIZED",
        "source_path": str(source),
        "source_sha256": _write(source, {"source": name}),
        "bin_path": str(binary),
        "bin_sha256": binary_sha,
        "trace_path": str(trace),
        "trace_sha256": trace_sha,
        "program_map_path": str(program_map),
        "program_map_sha256": _write(program_map, {"map": name}),
        "scenario_spec_path": str(scenario),
        "scenario_spec_sha256": _write(scenario, {"scenario": name}),
        "generator_manifest_path": str(generator),
        "trace_analysis_path": str(analysis),
        "configuration_preflight_path": str(configuration),
        "scenario_realization_path": str(realization),
    }


def _campaigns(tmp_path: Path, duplicate: bool = False) -> tuple[Path, Path]:
    first = list(range(16))
    second = first if duplicate else list(reversed(first))
    candidate = tmp_path / "candidate.json"
    control = tmp_path / "control.json"
    _write(candidate, {"cases": [_case(tmp_path, 0, first, candidate=True), _case(tmp_path, 1, second, candidate=True)]})
    _write(control, {"cases": [_case(tmp_path, 0, [], candidate=False), _case(tmp_path, 1, [], candidate=False)]})
    return candidate, control


def test_sequence_audit_proves_unique_same_seed_batch(tmp_path):
    candidate, control = _campaigns(tmp_path)
    report = build_report(candidate, control)
    assert report["qualification"] == {
        "pass": True,
        "status": "qualified_for_serialized_DUT_replay",
        "reasons": [],
        "DUT_status": "not_run",
        "coverage_hit": False,
        "corpus_replacement": False,
    }
    assert report["unique_executed_sequence_count"] == 2


def test_sequence_audit_rejects_duplicate_sequences(tmp_path):
    candidate, control = _campaigns(tmp_path, duplicate=True)
    report = build_report(candidate, control)
    assert report["qualification"]["pass"] is False
    assert "executed_sequences_not_unique:1/2" in report["qualification"]["reasons"]


def test_sequence_audit_output_is_exclusive(tmp_path):
    output = tmp_path / "audit.json"
    write_json_exclusive(output, {"first": True})
    try:
        write_json_exclusive(output, {"second": True})
    except FileExistsError:
        pass
    else:
        raise AssertionError("audit output unexpectedly overwrote existing evidence")
