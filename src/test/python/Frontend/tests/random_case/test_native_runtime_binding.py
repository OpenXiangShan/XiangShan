"""Offline runtime admission contracts; synthetic reports never assert DUT HIT."""
from copy import deepcopy
from pathlib import Path
from types import SimpleNamespace as NS
import json

import pytest

from env.core.frontend_env import FrontendEnv
from env.funcov.recorder import FrontendFuncovSampleHub
from env.funcov.toffee_bridge import ToffeeCoverageSink
from env.runtime.dut_factory import FakeDUTFrontend
from env.sequences import TranslationScenarioBuilder

from . import native_runtime_binding as binding
from .coverage_gap_audit import _definitions_from_campaign, build_gap_inventory
from .funcov_compare import build_report, load_artifact
from .test_native_funcov_adapter import native
from .test_random_bin_trace_dut import (
    _arm_strict_translation,
    _configure_translation_coverage,
    _oracle_health,
    _require_translation_coverage_mapping,
)
from .test_latest_environment_compatibility import mixed_pbmt_scenario
from .trusted_funcov import build_trusted_report, SAMPLER_SOURCES


@pytest.mark.parametrize(
    "recorder",
    [
        None,
        object(),
        FrontendFuncovSampleHub.__new__(FrontendFuncovSampleHub),
        NS(configure_program_address_mapping=None),
    ],
)
def test_missing_translation_coverage_api_is_not_a_bound_mapping(recorder):
    assert _configure_translation_coverage(recorder, ("page",)) is False


def test_translation_coverage_forwards_exact_page_outcomes_and_propagates_errors():
    pages = ({"va": 0x80000000, "pa": 0x80400000, "ok": True},)
    calls = []
    recorder = NS(configure_program_address_mapping=calls.append)

    assert _configure_translation_coverage(recorder, pages) is True
    assert calls == [pages]

    def reject(_outcomes):
        raise ValueError("invalid page mapping")

    with pytest.raises(ValueError, match="invalid page mapping"):
        _configure_translation_coverage(
            NS(configure_program_address_mapping=reject), pages
        )


def test_toffee_sink_binds_successful_translation_page_mapping_once():
    sink = ToffeeCoverageSink({("group", "point"): ("bin",)})
    pages = (
        {"va": 0x80000000, "pa": 0x80400000, "ok": True},
        {"va": 0x80001000, "pa": 0x80401000, "ok": True},
        {"va": 0x80002000, "pa": 0x80402000, "ok": False},
    )

    assert _configure_translation_coverage(sink, pages) is True
    assert sink.program_address_mapping == {
        0x80000000: 0x80400000,
        0x80001000: 0x80401000,
    }
    with pytest.raises(RuntimeError, match="already configured"):
        _configure_translation_coverage(sink, pages)


@pytest.mark.parametrize(
    "pages,match",
    [
        (({"va": 0x80000000, "pa": 0x80400000, "ok": False},), "no successful pages"),
        (({"va": 0x80000001, "pa": 0x80400000, "ok": True},), "matching page offsets"),
        (
            (
                {"va": 0x80000000, "pa": 0x80400000, "ok": True},
                {"va": 0x80000001, "pa": 0x80401001, "ok": True},
            ),
            "conflicting translation program page mapping",
        ),
    ],
)
def test_toffee_sink_rejects_incomplete_or_ambiguous_translation_mapping(
    pages, match
):
    sink = ToffeeCoverageSink({("group", "point"): ("bin",)})
    with pytest.raises(ValueError, match=match):
        _configure_translation_coverage(sink, pages)
    assert sink.program_address_mapping is None


@pytest.mark.parametrize("applied", [None, False, 0, 1, "true"])
def test_translation_coverage_mapping_requires_exact_true(applied):
    with pytest.raises(AssertionError, match="coverage mapping is unavailable"):
        _require_translation_coverage_mapping(applied)


def test_translation_coverage_mapping_accepts_only_bound_true():
    assert _require_translation_coverage_mapping(True) is None


def test_strict_random_translation_uses_public_api_and_keeps_missing_checks():
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    state = TranslationScenarioBuilder(env).build(mixed_pbmt_scenario())
    active = _arm_strict_translation(env, state)
    assert len(active["expected_ptw_requests"]) > 0
    assert len(active["expected_fetches"]) > 0
    with pytest.raises(AssertionError, match="missing_observation"):
        env.assert_translation_scenario()


@pytest.mark.parametrize("count,errors", [(False, []), ("0", []), (-1, []), (0, ""), (0, ()), (None, [])])
def test_oracle_health_never_normalizes_unknown_data(count, errors):
    with pytest.raises(ValueError):
        _oracle_health({"translation_oracle": {"error_count": count, "errors": errors}})


def test_oracle_health_preserves_real_failure():
    assert _oracle_health({"translation_oracle": {"error_count": 1, "errors": ["failed"]}}) == {
        "translation_oracle_error_count": 1, "translation_oracle_errors": ["failed"]}
    with pytest.raises(ValueError):
        _oracle_health({})


@pytest.fixture
def runtime(native, monkeypatch):
    root, raw, frozen, timeline = native
    frontend_root = root / "checkout/src/test/python/Frontend"
    for relative_path in SAMPLER_SOURCES.values():
        relative = Path(relative_path).relative_to("src/test/python/Frontend")
        p = frontend_root / relative
        p.parent.mkdir(parents=True, exist_ok=True)
        # The source parser is exercised with the real sampler text, pinned to
        # this tiny synthetic fixture instead of relying on working-tree Git.
        import env.funcov.registry as registry
        p.write_bytes((registry._frontend_root() / relative).read_bytes())
    p = frozen["provenance"]
    p.update(implementation_sha="c" * 40, design_baseline_sha="d" * 40,
             build_manifest_path=str(root / "manifest.json"), simulator="verilator")
    (root / "manifest.json").write_text("{}")
    coord = {key: p[value] for key, value in binding.COORD_FIELDS.items()}
    coord_path = root / "coord.json"
    binding.write_exclusive(coord_path, coord)
    inputs = {kind: Path(raw["run_metadata"]["execution"][kind + "_path"])
              for kind in ("asm", "bin", "trace")}
    for kind in ("generator", "scenario"):
        path = root / (kind + ".json")
        path.write_text("{}")
        inputs[kind] = path
    monkeypatch.setattr(binding, "_load_build", lambda contract: dict(p))
    env = NS(functional_coverage=NS(definitions=frozen["definitions"], provenance=p))
    contract_path = root / "runtime-frozen.json"
    args = dict(output=contract_path, coord_contract=coord_path,
                coord_sha256=binding.sha(coord_path), run_id="case", input_paths=inputs,
                minimum_dynamic_instructions=5000, frontend_root=frontend_root)
    return NS(root=root, raw=raw, timeline=timeline, provenance=p, env=env,
              args=args, coord=coord_path, source=frontend_root,
              contract=contract_path, inputs=inputs)


def complete(runtime):
    pin = binding.freeze_runtime_contract(runtime.env, **runtime.args)
    runtime.timeline["details"].update(native_runtime_contract={
        "path": str(runtime.contract), "sha256": pin}, native_runtime_postcheck=True,
        completed=True, pending_work=0)
    artifact = runtime.root / "native.funcov.json"
    binding.write_exclusive(artifact, runtime.raw)
    timeline_path = runtime.root / "timeline.json"
    binding.write_exclusive(timeline_path, runtime.timeline)
    bindings, expected = binding.bind_native_reports([artifact], timeline_path=timeline_path,
                                                     output_dir=runtime.root / "admission")
    record = {"name": "case", "execution_status": "completed", "qualification_pass": True,
              "eligible_functional_coverage_artifacts": [str(artifact)],
              "observed_bin_ids": ["test-hit"],
              "native_admission": {"scope": "current_native", "bindings": bindings,
                                   "expected_provenance": expected}}
    index_path = runtime.root / "index.json"
    index_pin = binding.write_admission_index([record], index_path)
    index = binding.load_admission_index(index_path, index_pin)
    return artifact, record, index_path, index_pin, index


def test_native_runtime_chain_qualifies_original_case_and_all_postgates(runtime):
    artifact, record, index_path, index_pin, index = complete(runtime)
    result = binding.load_current_artifact(artifact, index)
    assert result.eligible, result.eligibility_reason
    report = build_report([result], [], minimum_hit_rate=0.5, minimum_qualified_runs=1)
    assert report["qualification"]["pass"]
    campaign_path = runtime.root / "campaign.json"
    binding.write_exclusive(campaign_path, {"cases": [record], "repo_head": "synthetic"})
    trusted = build_trusted_report(campaign_path, minimum_hit_rate=0.5,
                                   minimum_qualified_runs=1, native_evidence_index=index)
    assert trusted["qualification"]["pass"]
    assert trusted["coverage"]["trusted_union_hit_bins"] == 1
    definitions = _definitions_from_campaign(campaign_path, index)
    gap = build_gap_inventory(definitions, trusted)
    assert gap["summary"]["definition_bins"] == 2
    assert binding.load_admission_index(index_path, index_pin)["canonical_hit_eligible"] is False
    # Ordinary mutable campaign post-gate bookkeeping has no cycle with the
    # independent, hash-pinned evidence index.
    campaign_path.write_text(json.dumps({"cases": [record], "post_run_gates": ["updated"]}))
    assert binding.load_current_artifact(artifact, index).eligible


@pytest.mark.parametrize("changed", ["coord", "sampler", "input", "build"])
def test_native_runtime_precontract_rejects_drift(runtime, changed, monkeypatch):
    pin = binding.freeze_runtime_contract(runtime.env, **runtime.args)
    if changed == "coord": runtime.coord.write_text("{}")
    elif changed == "sampler": next(runtime.source.rglob("*.py")).write_text("changed")
    elif changed == "input": runtime.inputs["bin"].write_bytes(b"changed")
    else: monkeypatch.setattr(binding, "_load_build", lambda c: {**runtime.provenance, "dut_build_sha256": "wrong"})
    with pytest.raises(ValueError): binding.verify_runtime_contract(runtime.contract, pin)


def test_native_runtime_freeze_rejects_coord_mismatch_before_evidence(runtime):
    runtime.coord.write_text(json.dumps({"source": "wrong"}))
    runtime.args["coord_sha256"] = binding.sha(runtime.coord)
    with pytest.raises(ValueError, match="provenance mismatch"):
        binding.freeze_runtime_contract(runtime.env, **runtime.args)
    assert not runtime.contract.exists()


@pytest.mark.parametrize("changed", ["index_pin", "sidecar", "artifact", "expected", "absent"])
def test_native_postgates_cannot_lose_or_change_pins(runtime, changed):
    artifact, _, index_path, pin, index = complete(runtime)
    if changed == "index_pin":
        with pytest.raises(ValueError): binding.load_admission_index(index_path, "0" * 64)
        return
    if changed == "sidecar": Path(index["artifacts"][str(artifact)]["sidecar_path"]).write_text("{}")
    elif changed == "artifact": artifact.write_text("{}")
    elif changed == "expected": index["artifacts"][str(artifact)]["expected_provenance"]["sampler_sha256"] = "wrong"
    else: index["artifacts"].clear()
    with pytest.raises((ValueError, KeyError)):
        binding.load_current_artifact(artifact, index)


def test_no_postrun_upgrade_of_unqualified_or_legacy_case(runtime):
    artifact, record, _, _, index = complete(runtime)
    record["qualification_pass"] = False
    with pytest.raises(ValueError): binding.write_admission_index([record], runtime.root / "empty.json")
    artifact.write_text(json.dumps({"definitions": [], "run_metadata": {}}))
    assert not load_artifact(artifact).eligible
    with pytest.raises(ValueError): binding.load_current_artifact(artifact, index)


@pytest.mark.parametrize("field,value", [("completed", False), ("completed", 1),
    ("pending_work", 1), ("pending_work", False), ("pending_work", "0"),
    ("native_runtime_postcheck", False)])
def test_current_runtime_requires_explicit_complete_idle_timeline(runtime, field, value):
    artifact, _, _, _, index = complete(runtime)
    item = index["artifacts"][str(artifact)]
    sidecar_path = Path(item["sidecar_path"])
    sidecar = json.loads(sidecar_path.read_text())
    timeline_path = Path(sidecar["scenario_timeline"]["path"])
    timeline = json.loads(timeline_path.read_text())
    timeline["details"][field] = value
    timeline_path.write_text(json.dumps(timeline))
    sidecar["scenario_timeline"]["sha256"] = binding.sha(timeline_path)
    sidecar_path.write_text(json.dumps(sidecar))
    item["sidecar_sha256"] = binding.sha(sidecar_path)
    with pytest.raises(ValueError): binding.load_current_artifact(artifact, index)
