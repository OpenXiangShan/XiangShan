#!/usr/bin/env python3
"""Audit predeclared target-bin hits from a qualified replay artifact."""
from __future__ import annotations

import argparse
import hashlib
import json
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

try:
    from .trusted_funcov import build_quarantine_policy
except ImportError:
    from trusted_funcov import build_quarantine_policy

from env.funcov.recorder import (
    current_funcov_sampler_sha256,
    current_verification_environment_sha256,
)


REPO_ROOT = Path(__file__).resolve().parents[6]


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _load(path: Path) -> dict[str, Any]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(payload, dict):
        raise ValueError(f"expected JSON object: {path}")
    return payload


def build_report(
    qualification_path: Path,
    *,
    source_loader=None,
    quarantine_builder=build_quarantine_policy,
    current_sampler_sha256: str | None = None,
    current_environment_sha256: str | None = None,
) -> dict[str, Any]:
    qualification_path = qualification_path.resolve()
    qualification = _load(qualification_path)
    artifact_input = (qualification.get("inputs") or {}).get("artifact") or {}
    artifact_path = Path(str(artifact_input.get("path") or "/nonexistent")).resolve()
    expected_artifact_sha = str(artifact_input.get("sha256") or "")
    actual_artifact_sha = _sha256(artifact_path) if artifact_path.is_file() else "missing"
    artifact_binding_valid = bool(
        expected_artifact_sha and expected_artifact_sha == actual_artifact_sha
    )
    artifact = _load(artifact_path) if artifact_binding_valid else {}
    provenance = artifact.get("provenance") or {}
    definitions = [
        item for item in (artifact.get("definitions") or []) if isinstance(item, dict)
    ]
    definition_ids = {
        str(item.get("bin_id") or "") for item in definitions if item.get("bin_id")
    }
    raw_hit_ids = {
        str(item.get("bin_id") or "")
        for item in (artifact.get("hits") or {}).values()
        if isinstance(item, dict)
        and int(item.get("hits") or 0) > 0
        and str(item.get("bin_id") or "")
    }
    target_ids = {
        str(value) for value in ((artifact.get("coverage_targets") or {}).get("bin_ids") or [])
    }
    sampler_sha = current_sampler_sha256 or current_funcov_sampler_sha256()
    environment_sha = current_environment_sha256 or current_verification_environment_sha256()
    expected_signature = str(
        qualification.get("recorded_compatibility_signature") or ""
    )
    artifact_signature = str(provenance.get("compatibility_signature") or "")

    reasons: list[str] = []
    if not qualification.get("replay_artifact_qualification", {}).get("pass"):
        reasons.append("replay_artifact_not_qualified")
    if not qualification.get("dut_pass", {}).get("pass"):
        reasons.append("DUT_not_passed")
    if not artifact_binding_valid:
        reasons.append("qualification_artifact_hash_mismatch")
    if not expected_signature or artifact_signature != expected_signature:
        reasons.append("qualification_artifact_compatibility_signature_mismatch")
    if str(provenance.get("sampler_sha256") or "") != sampler_sha:
        reasons.append("sampler_source_drift")
    if str(provenance.get("verification_env_sha256") or "") != environment_sha:
        reasons.append("verification_environment_drift")
    if not target_ids:
        reasons.append("declared_target_bins_missing")
    unknown_targets = sorted(target_ids - definition_ids)
    if unknown_targets:
        reasons.append("declared_target_bins_outside_definitions")

    if source_loader is None:
        source_loader = lambda relative: (REPO_ROOT / relative).read_text(encoding="utf-8")
    quarantine = (
        quarantine_builder(definitions, raw_hit_ids, source_loader=source_loader)
        if definitions
        else {
            "active_rule_ids": [],
            "quarantined_definition_bin_ids": [],
            "quarantined_raw_hit_bin_ids": [],
        }
    )
    quarantined = set(quarantine["quarantined_definition_bin_ids"])
    raw_target_hits = target_ids & raw_hit_ids
    trusted_target_hits = raw_target_hits - quarantined
    eligible = not reasons
    return {
        "schema_version": 1,
        "generated_at_utc": datetime.now(timezone.utc).isoformat(),
        "policy": (
            "Only bins declared before execution, observed in a hash-bound independently "
            "qualified same-contract DUT artifact, and not quarantined by the current sampler "
            "policy are reported as trusted target hits."
        ),
        "inputs": {
            "qualification": {
                "path": str(qualification_path),
                "sha256": _sha256(qualification_path),
            },
            "artifact": {
                "path": str(artifact_path),
                "expected_sha256": expected_artifact_sha,
                "actual_sha256": actual_artifact_sha,
                "pass": artifact_binding_valid,
            },
            "audit_tool": {
                "path": str(Path(__file__).resolve()),
                "sha256": _sha256(Path(__file__).resolve()),
            },
            "quarantine_tool": {
                "path": str(Path(build_quarantine_policy.__code__.co_filename).resolve()),
                "sha256": _sha256(
                    Path(build_quarantine_policy.__code__.co_filename).resolve()
                ),
            },
        },
        "coverage_contract": {
            "compatibility_signature": artifact_signature,
            "definitions_sha256": str(provenance.get("definitions_sha256") or ""),
            "sampler_sha256": str(provenance.get("sampler_sha256") or ""),
            "verification_env_sha256": str(
                provenance.get("verification_env_sha256") or ""
            ),
        },
        "quarantine": quarantine,
        "target_result": {
            "declared_target_bin_ids": sorted(target_ids),
            "raw_hit_bin_ids": sorted(raw_target_hits),
            "trusted_hit_bin_ids": sorted(trusted_target_hits) if eligible else [],
            "unhit_bin_ids": sorted(target_ids - raw_target_hits),
            "quarantined_hit_bin_ids": sorted(raw_target_hits & quarantined),
            "declared_target_count": len(target_ids),
            "raw_hit_count": len(raw_target_hits),
            "trusted_hit_count": len(trusted_target_hits) if eligible else 0,
            "all_declared_targets_hit": bool(target_ids) and target_ids <= raw_target_hits,
        },
        "qualification": {
            "pass": eligible,
            "status": "eligible" if eligible else "not_eligible",
            "reasons": reasons,
            "trusted_coverage_hit_claimed": eligible and bool(trusted_target_hits),
        },
    }


def write_json_exclusive(path: Path, payload: dict[str, Any]) -> None:
    path = path.resolve()
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("x", encoding="utf-8") as stream:
        json.dump(payload, stream, indent=2, sort_keys=True)
        stream.write("\n")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--qualification", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    report = build_report(args.qualification)
    write_json_exclusive(args.output, report)
    result = report["target_result"]
    print(
        f"eligible={int(report['qualification']['pass'])} "
        f"trusted_hits={result['trusted_hit_count']}/{result['declared_target_count']} "
        f"output={args.output.resolve()}"
    )
    return 0 if report["qualification"]["pass"] else 1


if __name__ == "__main__":
    raise SystemExit(main())
