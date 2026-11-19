"""Offline CLI coverage admission tests with synthetic native evidence."""

import json
from pathlib import Path
import sys
from types import SimpleNamespace as NS

import pytest

from . import coverage_gap_audit, funcov_compare, trusted_funcov
from . import native_runtime_binding as binding
from .test_native_funcov_adapter import native
from .test_trusted_funcov import _artifact, _definition
from .trusted_funcov import SAMPLER_SOURCES


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


REPORTS = (funcov_compare, trusted_funcov, coverage_gap_audit)


def invoke(monkeypatch, report, artifact, campaign, output, index=None, pin=None):
    args = [report.__file__, "--minimum-qualified-runs", "1",
            "--minimum-hit-rate", "0.5", "--output", str(output)]
    if report is funcov_compare:
        args += ["--random-artifact", str(artifact), "--require-qualified-union"]
    else:
        args += ["--campaign", str(campaign)]
        if report is trusted_funcov:
            args += ["--require-qualified-union"]
    if index is not None:
        args += ["--native-evidence-index", str(index)]
    if pin is not None:
        args += ["--native-evidence-index-sha256", pin]
    monkeypatch.setattr(sys, "argv", args)
    return report.main()


@pytest.fixture
def current_report_inputs(runtime):
    artifact, record, index, pin, _ = complete(runtime)
    campaign = runtime.root / "campaign.json"
    binding.write_exclusive(campaign, {"cases": [record], "repo_head": "unused-synthetic"})
    return artifact, record, campaign, index, pin


@pytest.mark.parametrize("report", REPORTS)
def test_cli_admits_only_the_pinned_current_native_report(
    runtime, current_report_inputs, monkeypatch, report
):
    artifact, _, campaign, index, pin = current_report_inputs
    output = runtime.root / "report.json"
    assert invoke(monkeypatch, report, artifact, campaign, output, index, pin) == 0
    result = json.loads(output.read_text())
    assert result["admission_scope"] == "current_native"
    assert result["canonical_hit_eligible"] is False
    assert result["qualification"]["pass"] is True
    assert "historical_thresholds_met" not in result["qualification"]
    if report is funcov_compare:
        assert result["random"]["union_hit_bins"] == 1
    else:
        assert result["coverage"]["trusted_union_hit_bins"] == 1


@pytest.mark.parametrize("report", REPORTS)
@pytest.mark.parametrize("drift", ["wrong_index_pin", "missing_pin", "sidecar", "artifact",
                                  "timeline", "contract", "input", "sampler"])
def test_cli_rejects_current_evidence_when_a_pin_no_longer_matches(
    runtime, current_report_inputs, monkeypatch, report, drift
):
    artifact, _, campaign, index, pin = current_report_inputs
    output = runtime.root / "report.json"
    if drift == "wrong_index_pin":
        pin = "0" * 64
    elif drift == "missing_pin":
        pin = None
    elif drift == "sidecar":
        entry = json.loads(index.read_text())["artifacts"][str(artifact)]
        Path(entry["sidecar_path"]).write_text("{}")
    elif drift in {"artifact", "timeline", "contract"}:
        target = {"artifact": artifact, "timeline": runtime.root / "timeline.json",
                  "contract": runtime.contract}[drift]
        target.write_text("{}")
    elif drift == "input":
        runtime.inputs["bin"].write_bytes(b"changed")
    else:
        next(runtime.source.rglob("*.py")).write_text("changed")
    with pytest.raises((ValueError, SystemExit)):
        invoke(monkeypatch, report, artifact, campaign, output, index, pin)
    assert not output.exists()


@pytest.mark.parametrize("report", REPORTS)
def test_cli_keeps_legacy_counts_diagnostic_with_no_index(tmp_path, monkeypatch, report):
    artifact = tmp_path / "legacy.json"
    _artifact(artifact, [_definition("BIN-001", "compact", "event"),
                         _definition("BIN-010", "clean", "event")])
    frontend = Path(__file__).resolve().parents[2]
    # Historical source lookup is exercised using the real sampler files.
    monkeypatch.setattr(trusted_funcov, "_git_source",
                        lambda root, revision, path: (frontend.parents[3] / path).read_text())
    campaign = tmp_path / "campaign.json"
    binding.write_exclusive(campaign, {"repo_head": "historical-synthetic", "cases": [{
        "name": "legacy", "execution_status": "completed", "qualification_pass": True,
        "eligible_functional_coverage_artifacts": [str(artifact)],
        "observed_bin_ids": ["BIN-001", "BIN-010"],
    }]})
    output = tmp_path / "diagnostic.json"
    assert invoke(monkeypatch, report, artifact, campaign, output) == (
        0 if report is coverage_gap_audit else 1
    )
    result = json.loads(output.read_text())
    assert result["admission_scope"] == "historical_diagnostic_only"
    assert result["canonical_hit_eligible"] is False
    assert result["qualification"]["pass"] is False
    assert result["qualification"]["historical_thresholds_met"] is True
    if report is funcov_compare:
        assert result["random"]["union_hit_bins"] == 2
    else:
        assert result["coverage"]["raw_union_hit_bins"] == 2


@pytest.mark.parametrize("report", (trusted_funcov, coverage_gap_audit))
@pytest.mark.parametrize("problem", ("missing_artifact", "reused_run"))
def test_campaign_cannot_count_a_case_without_unique_bound_evidence(
    runtime, current_report_inputs, monkeypatch, report, problem
):
    artifact, record, campaign, index, pin = current_report_inputs
    extra = {**record, "name": "another-case"}
    if problem == "missing_artifact":
        extra.update(eligible_functional_coverage_artifacts=[], observed_bin_ids=[])
    campaign.write_text(json.dumps({"cases": [record, extra]}))
    output = runtime.root / "report.json"
    with pytest.raises(ValueError, match="no indexed native artifact|reuses a native run"):
        invoke(monkeypatch, report, artifact, campaign, output, index, pin)
    assert not output.exists()


@pytest.mark.parametrize("report", REPORTS)
def test_given_current_index_never_falls_back_for_an_unindexed_report(
    runtime, current_report_inputs, monkeypatch, report
):
    _, record, campaign, index, pin = current_report_inputs
    unindexed = runtime.root / "unindexed-legacy.json"
    _artifact(unindexed, [_definition("BIN-010", "clean", "event")])
    record["eligible_functional_coverage_artifacts"] = [str(unindexed)]
    campaign.write_text(json.dumps({"cases": [record]}))
    output = runtime.root / "report.json"
    with pytest.raises((SystemExit, ValueError), match="not in the pinned"):
        invoke(monkeypatch, report, unindexed, campaign, output, index, pin)
    assert not output.exists()


@pytest.mark.parametrize("report", REPORTS)
def test_pin_does_not_override_a_failed_native_checker(runtime, monkeypatch, report):
    runtime.raw["run_metadata"]["checker"]["status"] = "fail"
    artifact, record, index, pin, _ = complete(runtime)
    campaign = runtime.root / "campaign.json"
    binding.write_exclusive(campaign, {"cases": [record]})
    output = runtime.root / "report.json"
    with pytest.raises((SystemExit, ValueError), match="native_checker_not_proved_clean"):
        invoke(monkeypatch, report, artifact, campaign, output, index, pin)
    assert not output.exists()


def test_comparison_does_not_count_two_reports_from_one_run_as_two_runs(
    runtime, current_report_inputs, monkeypatch
):
    artifact, record, _, _, _ = current_report_inputs
    second = runtime.root / "second.funcov.json"
    second.write_bytes(artifact.read_bytes())
    bindings, expected = binding.bind_native_reports(
        [artifact, second], timeline_path=runtime.root / "timeline.json",
        output_dir=runtime.root / "admission-pair",
    )
    record["eligible_functional_coverage_artifacts"] = [str(artifact), str(second)]
    record["native_admission"] = {
        "scope": "current_native", "bindings": bindings, "expected_provenance": expected,
    }
    index = runtime.root / "pair-index.json"
    pin = binding.write_admission_index([record], index)
    output = runtime.root / "report.json"
    monkeypatch.setattr(sys, "argv", [
        funcov_compare.__file__, "--random-artifact", str(artifact),
        "--random-artifact", str(second), "--native-evidence-index", str(index),
        "--native-evidence-index-sha256", pin, "--minimum-qualified-runs", "2",
        "--output", str(output), "--require-qualified-union",
    ])
    with pytest.raises(SystemExit, match="multiple artifacts for the same run"):
        funcov_compare.main()
    assert not output.exists()
