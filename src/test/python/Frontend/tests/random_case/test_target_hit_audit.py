import hashlib
import json

from .target_hit_audit import build_report, write_json_exclusive


def _write(path, payload):
    path.write_text(json.dumps(payload, sort_keys=True) + "\n", encoding="ascii")
    return path


def _sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _fixture(tmp_path, *, targets=("BIN-1", "BIN-2")):
    artifact = _write(
        tmp_path / "artifact.json",
        {
            "provenance": {
                "compatibility_signature": "contract",
                "definitions_sha256": "definitions",
                "sampler_sha256": "sampler",
                "verification_env_sha256": "environment",
            },
            "definitions": [{"bin_id": "BIN-1"}, {"bin_id": "BIN-2"}],
            "hits": {"one": {"bin_id": "BIN-1", "hits": 1}},
            "coverage_targets": {"bin_ids": list(targets)},
        },
    )
    qualification = _write(
        tmp_path / "qualification.json",
        {
            "recorded_compatibility_signature": "contract",
            "replay_artifact_qualification": {"pass": True},
            "dut_pass": {"pass": True},
            "inputs": {"artifact": {"path": str(artifact), "sha256": _sha(artifact)}},
        },
    )
    return artifact, qualification


def _no_quarantine(_definitions, _raw, *, source_loader):
    return {
        "active_rule_ids": [],
        "quarantined_definition_bin_ids": [],
        "quarantined_raw_hit_bin_ids": [],
    }


def test_audit_reports_only_predeclared_observed_hits(tmp_path):
    _, qualification = _fixture(tmp_path)
    report = build_report(
        qualification,
        source_loader=lambda _path: "",
        quarantine_builder=_no_quarantine,
        current_sampler_sha256="sampler",
        current_environment_sha256="environment",
    )
    # Use the real quarantine implementation with empty source only through a
    # fixture monkeypatch-like replacement below is unnecessary: its rules are inactive.
    assert report["qualification"]["pass"] is True
    assert report["qualification"]["trusted_coverage_hit_claimed"] is True
    assert report["target_result"]["trusted_hit_bin_ids"] == ["BIN-1"]
    assert report["target_result"]["unhit_bin_ids"] == ["BIN-2"]
    assert report["target_result"]["all_declared_targets_hit"] is False


def test_audit_fails_closed_on_artifact_change(tmp_path):
    artifact, qualification = _fixture(tmp_path)
    artifact.write_text(artifact.read_text() + "\n", encoding="ascii")
    report = build_report(
        qualification,
        source_loader=lambda _path: "",
        quarantine_builder=_no_quarantine,
        current_sampler_sha256="sampler",
        current_environment_sha256="environment",
    )
    assert report["qualification"]["pass"] is False
    assert "qualification_artifact_hash_mismatch" in report["qualification"]["reasons"]
    assert report["target_result"]["trusted_hit_bin_ids"] == []


def test_audit_does_not_promote_quarantined_target_hit(tmp_path):
    _, qualification = _fixture(tmp_path, targets=("BIN-1",))

    def quarantine(_definitions, _raw, *, source_loader):
        return {
            "active_rule_ids": ["fixture_rule"],
            "quarantined_definition_bin_ids": ["BIN-1"],
            "quarantined_raw_hit_bin_ids": ["BIN-1"],
        }

    report = build_report(
        qualification,
        source_loader=lambda _path: "",
        quarantine_builder=quarantine,
        current_sampler_sha256="sampler",
        current_environment_sha256="environment",
    )
    assert report["qualification"]["pass"] is True
    assert report["qualification"]["trusted_coverage_hit_claimed"] is False
    assert report["target_result"]["raw_hit_bin_ids"] == ["BIN-1"]
    assert report["target_result"]["trusted_hit_bin_ids"] == []
    assert report["target_result"]["quarantined_hit_bin_ids"] == ["BIN-1"]


def test_sidecar_writer_never_overwrites(tmp_path):
    output = tmp_path / "audit.json"
    write_json_exclusive(output, {"version": 1})
    try:
        write_json_exclusive(output, {"version": 2})
    except FileExistsError:
        pass
    else:
        raise AssertionError("exclusive writer overwrote an existing report")
