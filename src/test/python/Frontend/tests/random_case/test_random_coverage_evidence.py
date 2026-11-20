from __future__ import annotations

import json
from pathlib import Path

from .funcov_compare import build_report, load_artifact, write_report
from .random_case_test_support import _FakeCoordinatorEnv, _coordinator_program_map
from .run_random_campaign import _load_dut_timeline, _load_funcov_evidence
from .scenario_contract import RealizedEvent, ScenarioSpec, evaluate_realization
from .scenario_coordinator import ScenarioCoordinator, TimingPlan


def _write_funcov(
    path: Path,
    *,
    signature: str,
    run_id: str,
    hit_bin_ids: tuple[str, ...],
    program_seed: int | None = None,
    timing_seed: int | None = None,
) -> None:
    definitions = [
        {
            "bin_id": bin_id,
            "coverage_group": "group",
            "coverpoint": "point",
            "bin_name": f"name_{bin_id}",
            "suggested_testcase": "case",
        }
        for bin_id in ("BIN-001", "BIN-002", "BIN-003", "BIN-004")
    ]
    path.write_text(
        json.dumps(
            {
                "artifact_schema_version": 2,
                "artifact_tag": run_id,
                "provenance": {
                    "compatibility_signature": signature,
                    "definitions_sha256": "definitions",
                    "build_manifest_status": "valid",
                },
                "definitions": definitions,
                "hits": {
                    f"group::point::name_{bin_id}": {"bin_id": bin_id, "hits": 1}
                    for bin_id in hit_bin_ids
                },
                "run": {
                    "run_id": run_id,
                    "pytest_outcome": "pass",
                    "checker": {"status": "pass"},
                    "run_command": f"replay {run_id}",
                    **(
                        {
                            "seed": program_seed,
                            "seeds": {
                                "test": program_seed,
                                "backend": timing_seed,
                            },
                        }
                        if program_seed is not None and timing_seed is not None
                        else {}
                    ),
                },
                "stats": {"monitor": {"error_count": 0, "errors": []},
                          "translation_oracle": {"error_count": 0, "errors": []}},
                "outcome": {"status": "pass", "exit_code": 0},
                "checker": {"status": "pass", "error_count": 0, "errors": []},
            }
        ),
        encoding="utf-8",
    )


def test_funcov_report_uses_only_compatible_baseline_and_tracks_marginal_gain(
    tmp_path: Path,
) -> None:
    random_a = tmp_path / "random_a.funcov.json"
    random_b = tmp_path / "random_b.funcov.json"
    baseline = tmp_path / "baseline.funcov.json"
    incompatible = tmp_path / "incompatible.funcov.json"
    _write_funcov(
        random_a,
        signature="same",
        run_id="random-a",
        hit_bin_ids=("BIN-001",),
        program_seed=1,
        timing_seed=11,
    )
    _write_funcov(
        random_b,
        signature="same",
        run_id="random-b",
        hit_bin_ids=("BIN-001", "BIN-003"),
        program_seed=2,
        timing_seed=22,
    )
    _write_funcov(baseline, signature="same", run_id="directed", hit_bin_ids=("BIN-001", "BIN-002"))
    _write_funcov(incompatible, signature="old", run_id="old", hit_bin_ids=("BIN-004",))

    report = build_report(
        [load_artifact(random_a), load_artifact(random_b)],
        [load_artifact(baseline), load_artifact(incompatible)],
    )
    report_path = tmp_path / "comparison.json"
    write_report(report, report_path)

    assert report["random"]["union_hit_bins"] == 2
    assert report["random"]["total_definition_bins"] == 4
    assert report["random"]["hit_rate"] == 0.5
    assert report["random"]["unhit_bin_ids"] == ["BIN-002", "BIN-004"]
    assert report["qualification"]["pass"] is False
    assert report["directed_baseline"]["union_hit_bins"] == 2
    assert report["comparison"]["random_only_bin_ids"] == ["BIN-003"]
    assert report["comparison"]["baseline_only_bin_ids"] == ["BIN-002"]
    assert [item["new_random_union_bins"] for item in report["per_random_artifact_marginal_gain"]] == [1, 1]
    assert [item["seed_pair"] for item in report["per_random_artifact_marginal_gain"]] == [
        [1, 11],
        [2, 22],
    ]
    corpus = report["coverage_guided_corpus"]
    assert corpus["status"] == "available"
    assert corpus["union_preserved"] is True
    assert [item["run_id"] for item in corpus["retained_runs"]] == ["random-b"]
    assert [item["run_id"] for item in corpus["discard_candidates"]] == ["random-a"]
    assert corpus["bin_repeatability"]["distinct_seed_pairs"] == 2
    assert corpus["bin_repeatability"]["bins_hit_by_multiple_seed_pairs"] == 1
    assert corpus["bin_repeatability"]["single_seed_bin_ids"] == ["BIN-003"]
    assert [item["run_id"] for item in report["directed_baseline"]["comparable_artifacts"]] == [
        "directed"
    ]
    assert report["skipped_baseline_artifacts"][0]["reason"] == "incompatible_signature"
    assert report_path.is_file()
    assert report_path.with_suffix(".csv").is_file()


def test_funcov_report_does_not_union_incompatible_random_contracts(tmp_path: Path) -> None:
    random_a = tmp_path / "random_a.funcov.json"
    random_b = tmp_path / "random_b.funcov.json"
    _write_funcov(random_a, signature="contract-a", run_id="random-a", hit_bin_ids=("BIN-001",))
    _write_funcov(random_b, signature="contract-b", run_id="random-b", hit_bin_ids=("BIN-002",))

    report = build_report(
        [load_artifact(random_a), load_artifact(random_b)],
        [],
    )

    assert report["strictly_comparable"] is False
    assert report["random"]["union_hit_bins"] is None
    assert report["random"]["hit_rate"] is None
    assert len(report["random"]["by_contract"]) == 2
    assert "multiple_random_compatibility_contracts" in report["qualification"]["reasons"]
    assert report["coverage_guided_corpus"]["status"] == "unavailable"
    assert report["coverage_guided_corpus"]["retained_runs"] == []


def test_scenario_coordinator_records_realized_top_level_timing_actions(tmp_path: Path) -> None:
    forwarded_events: list[dict] = []
    env = _FakeCoordinatorEnv(forwarded_events.append)
    plan = TimingPlan(
        timing_seed=0x1234,
        icache_seed=0x5678,
        icache_hit_latency=2,
        icache_miss_latency=12,
        icache_miss_rate=0.1,
        warmup_response_lines=1,
        periodic_response_stride=19,
        maximum_backpressure_windows=1,
        backpressure_durations=(2,),
    )
    program_map = _coordinator_program_map()
    coordinator = ScenarioCoordinator(
        scenario_id="cfg-000",
        timing_plan=plan,
        program_map=program_map,
    )
    coordinator.install(env)

    request = {
        "type": "handshake.icache_a",
        "source": "icache_agent",
        "cycle": 9,
        "payload": {"address": 0x80000000},
    }
    pbmte_control = {
        "type": "control.translation_pbmte",
        "source": "frontend_env",
        "cycle": 9,
        "payload": {
            "requested": {"machine": 1, "hypervisor": None},
            "driven": {"machine": 1},
            "unsupported": [],
            "supported": True,
        },
    }
    response = {
        "type": "handshake.icache_d",
        "source": "icache_agent",
        "cycle": 10,
        "payload": {"address": 0x80000000, "beat_idx": 0},
    }
    env.event_sink(request)
    env.event_sink(pbmte_control)
    env.event_sink(response)
    coordinator.observe_cycle(10, env)
    coordinator.observe_cycle(11, env)

    assert forwarded_events == [request, pbmte_control, response]
    assert env.icache_agent.configuration == {
        "hit_latency": 2,
        "miss_latency": 12,
        "miss_rate": 0.1,
        "seed": 0x5678,
    }
    assert env.backend_model.values == [0, 1]
    assert coordinator.action_count == 1
    assert coordinator.has_active_action is False
    assert coordinator.unrealized_action_ids == ()
    realized = {event.event: event.count for event in coordinator.realized_events()}
    assert realized["dut.icache_request"] == 1
    assert realized["dut.icache_response"] == 1
    assert realized["dut.backend_backpressure_cycle"] == 2
    assert realized["dut.backend_backpressure_window"] == 1
    assert realized["dut.icache_response_x_backend_backpressure"] == 1

    timeline_path = tmp_path / "timeline.json"
    timeline = coordinator.write_timeline(
        timeline_path,
        execution_status="REALIZED",
        details={"monitor_errors": 0, "translation_oracle_error_count": 0, "translation_oracle_errors": []},
    )
    scenario = ScenarioSpec(
        scenario_id="cfg-000",
        family="cfg_layout_mix",
        ordinal=0,
        program_seed=1,
        timing_seed=plan.timing_seed,
        risk_tags=("boundary",),
        parameters={"timing_plan": plan.to_dict()},
    )
    loaded, loaded_events, status, reasons = _load_dut_timeline(
        timeline_path,
        scenario,
        program_map,
    )
    assert timeline["actions"][0]["status"] == "REALIZED"
    assert timeline["event_counts"]["control.translation_pbmte"] == 1
    assert any(
        event["type"] == "control.translation_pbmte"
        and event["payload"]["driven"] == {"machine": 1}
        for event in timeline["timeline"]
    )
    assert loaded["scenario_id"] == "cfg-000"
    assert loaded_events
    assert status == "valid"
    assert reasons == []


def test_timeline_loader_rejects_malformed_action_without_raising(tmp_path: Path) -> None:
    program_map = _coordinator_program_map()
    plan = TimingPlan.from_seed(7)
    scenario = ScenarioSpec(
        scenario_id="malformed-action",
        family="cfg_layout_mix",
        ordinal=0,
        program_seed=1,
        timing_seed=plan.timing_seed,
        risk_tags=("boundary",),
        parameters={"timing_plan": plan.to_dict()},
    )
    timeline_path = tmp_path / "timeline.json"
    timeline_path.write_text(
        json.dumps(
            {
                "schema_version": 1,
                "scenario_id": scenario.scenario_id,
                "execution_status": "REALIZED",
                "timing_plan": plan.to_dict(),
                "program_map_binary_sha256": program_map["binary_sha256"],
                "actions": ["truncated-action"],
                "details": {"monitor_errors": 0, "translation_oracle_error_count": 0, "translation_oracle_errors": []},
                "dropped_timeline_events": 0,
                "realized_events": [],
            }
        ),
        encoding="utf-8",
    )

    _, events, status, reasons = _load_dut_timeline(
        timeline_path,
        scenario,
        program_map,
    )

    assert events == ()
    assert status == "invalid"
    assert reasons == [
        "scenario_timeline_incomplete_actions=index-0-not-an-object"
    ]


def test_timeline_loader_rejects_dropped_events_and_oracle_errors(tmp_path: Path) -> None:
    program_map = _coordinator_program_map()
    plan = TimingPlan.from_seed(7)
    scenario = ScenarioSpec(
        scenario_id="invalid-evidence",
        family="translation_timing_cross",
        ordinal=0,
        program_seed=1,
        timing_seed=plan.timing_seed,
        risk_tags=("translation",),
        parameters={"timing_plan": plan.to_dict()},
    )
    timeline_path = tmp_path / "timeline.json"
    timeline_path.write_text(
        json.dumps(
            {
                "schema_version": 1,
                "scenario_id": scenario.scenario_id,
                "execution_status": "REALIZED",
                "timing_plan": plan.to_dict(),
                "program_map_binary_sha256": program_map["binary_sha256"],
                "actions": [],
                "dropped_timeline_events": 2,
                "details": {
                    "monitor_errors": 0,
                    "translation_oracle_error_count": 1,
                },
                "realized_events": [],
            }
        ),
        encoding="utf-8",
    )

    _, events, status, reasons = _load_dut_timeline(
        timeline_path, scenario, program_map
    )

    assert events == ()
    assert status == "invalid"
    assert "scenario_timeline_translation_oracle_errors=1" in reasons
    assert "scenario_timeline_dropped_events=2" in reasons


def test_funcov_case_evidence_records_dynamic_many_to_many_hits(tmp_path: Path) -> None:
    artifact_path = tmp_path / "case.funcov.json"
    _write_funcov(
        artifact_path,
        signature="fixed-contract",
        run_id="random-case",
        hit_bin_ids=("BIN-001", "BIN-003"),
    )

    evidence = _load_funcov_evidence([artifact_path])

    assert evidence["status"] == "eligible"
    assert evidence["observed_bin_ids"] == ["BIN-001", "BIN-003"]
    assert evidence["compatibility_contract"] == {
        "compatibility_signature": "fixed-contract",
        "definitions_sha256": "definitions",
        "definition_bin_count": 4,
    }
    assert evidence["eligible_artifact_paths"] == [str(artifact_path.resolve())]


def test_funcov_evidence_rejects_translation_oracle_errors(tmp_path: Path) -> None:
    artifact_path = tmp_path / "translation.funcov.json"
    _write_funcov(
        artifact_path,
        signature="fixed-contract",
        run_id="translation-case",
        hit_bin_ids=("BIN-001",),
    )
    payload = json.loads(artifact_path.read_text(encoding="utf-8"))
    payload["stats"] = {"translation_oracle": {"error_count": 1, "errors": [{"reason": "mismatch"}]}}
    artifact_path.write_text(json.dumps(payload), encoding="utf-8")

    evidence = _load_funcov_evidence([artifact_path])

    assert evidence["status"] == "ineligible"
    assert any(
        "translation_oracle_error_count=1" in reason
        for reason in evidence["reasons"]
    )


def test_funcov_evidence_rejects_unproved_speculation_despite_checker_pass(tmp_path: Path) -> None:
    artifact_path = tmp_path / "translation.funcov.json"
    _write_funcov(artifact_path, signature="fixed-contract", run_id="translation-case",
                  hit_bin_ids=("BIN-001",))
    payload = json.loads(artifact_path.read_text())
    payload["stats"] = {"translation_oracle": {
        "error_count": 0, "errors": [], "records": [
            {"kind": "cfvec_normal", "pc": 0x80000000},
            {"kind": "speculative_cfvec_exception", "pc": 0, "cycle": 10},
        ],
    }}
    artifact_path.write_text(json.dumps(payload))
    assert not load_artifact(artifact_path).eligible
    evidence = _load_funcov_evidence([artifact_path])
    assert evidence["status"] == "ineligible"
    assert evidence["eligible_artifact_paths"] == []
    assert evidence["observed_bin_ids"] == []
    assert any("unproved_speculative_cfvec_exceptions=1" in r for r in evidence["reasons"])


def test_case_realization_keeps_observed_funcov_bin_ids(tmp_path: Path) -> None:
    artifact_path = tmp_path / "case.funcov.json"
    _write_funcov(
        artifact_path,
        signature="fixed-contract",
        run_id="random-case",
        hit_bin_ids=("BIN-001", "BIN-003"),
    )
    evidence = _load_funcov_evidence([artifact_path])
    spec = ScenarioSpec(
        scenario_id="case",
        family="cfg_layout_mix",
        ordinal=0,
        program_seed=1,
        timing_seed=2,
        risk_tags=("control_flow",),
    )
    realized = evaluate_realization(
        spec,
        tuple(
            RealizedEvent(
                event="funcov.bin_hit",
                relation="bin_id",
                anchor=bin_id,
                count=1,
                evidence_source="compatible_functional_coverage_artifact",
            )
            for bin_id in evidence["observed_bin_ids"]
        ),
        evidence_stage="dut_program_plus_funcov",
        execution_status="completed",
        exit_code=0,
        observed_bin_ids=tuple(evidence["observed_bin_ids"]),
    )

    assert realized.observed_bin_ids == ("BIN-001", "BIN-003")
    assert [event.anchor for event in realized.realized_events] == ["BIN-001", "BIN-003"]
