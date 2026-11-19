#!/usr/bin/env python3
"""Produce an actionable, denominator-preserving trusted-coverage gap inventory."""
from __future__ import annotations

import argparse
import json
from collections import Counter, defaultdict
from pathlib import Path
from typing import Any, Iterable

try:
    from .funcov_compare import load_artifact
    from .trusted_funcov import build_trusted_report
except ImportError:
    from funcov_compare import load_artifact
    from trusted_funcov import build_trusted_report


def _atomic_json(path: Path, payload: dict[str, Any]) -> None:
    path = path.resolve()
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(f".{path.name}.tmp")
    temporary.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    temporary.replace(path)


def _scenario_action(coverage_group: str, *, quarantined: bool) -> str:
    if quarantined:
        return "repair_semantic_sampler_then_replay"
    if coverage_group.startswith("icache_"):
        return "add_icache_timing_or_maintenance_scenario"
    if coverage_group.startswith("fetch_path"):
        return "add_fetch_path_transition_scenario"
    if any(token in coverage_group for token in ("mmio", "uncache", "translation")):
        return "add_translation_mmio_uncache_timing_scenario"
    if any(
        token in coverage_group
        for token in ("ibuffer", "instr_", "compact", "boundary", "predchecker", "cacheable")
    ):
        return "add_program_layout_redirect_backpressure_scenario"
    return "classify_with_coverage_owner_before_new_random_seed"


def build_gap_inventory(
    definitions: Iterable[dict[str, Any]], trusted_report: dict[str, Any]
) -> dict[str, Any]:
    """Classify every denominator bin, keeping shadow hits distinct from unhit bins."""
    coverage = dict(trusted_report.get("coverage") or {})
    quarantine = dict(trusted_report.get("quarantine") or {})
    raw_hits = set(coverage.get("raw_union_bin_ids") or [])
    trusted_hits = set(coverage.get("trusted_union_bin_ids") or [])
    quarantine_reasons = {
        str(bin_id): list(rule_ids)
        for bin_id, rule_ids in dict(quarantine.get("reasons_by_bin_id") or {}).items()
    }
    rules = {
        str(rule.get("rule_id")): str(rule.get("source_path"))
        for rule in quarantine.get("rules") or []
        if isinstance(rule, dict)
    }
    entries: list[dict[str, Any]] = []
    for definition in definitions:
        bin_id = str(definition.get("bin_id") or "")
        if not bin_id:
            raise ValueError("coverage definition has no bin_id")
        raw_hit = bin_id in raw_hits
        trusted_hit = bin_id in trusted_hits
        reason_ids = sorted(quarantine_reasons.get(bin_id, []))
        quarantined = bool(reason_ids)
        if trusted_hit:
            classification = "trusted_hit"
        elif raw_hit and quarantined:
            classification = "raw_hit_quarantined"
        elif raw_hit:
            # A raw hit that is absent from trusted union without an audit rule
            # is a contract error, not a legitimate coverage category.
            classification = "raw_hit_unexplained"
        else:
            classification = "unhit"
        coverage_group = str(definition.get("coverage_group") or "")
        entries.append(
            {
                "bin_id": bin_id,
                "coverage_group": coverage_group,
                "coverpoint": str(definition.get("coverpoint") or ""),
                "bin_name": str(definition.get("bin_name") or ""),
                "mapped_path": str(definition.get("mapped_path") or ""),
                "sample_event": str(definition.get("sample_event") or ""),
                "observe_object": str(definition.get("observe_object") or ""),
                "hit_rule": str(definition.get("hit_rule") or ""),
                "priority": str(definition.get("priority") or ""),
                "suggested_testcase": str(definition.get("suggested_testcase") or ""),
                "raw_hit": raw_hit,
                "trusted_hit": trusted_hit,
                "classification": classification,
                "quarantine_rule_ids": reason_ids,
                "sampler_source_paths": [rules[rule_id] for rule_id in reason_ids if rule_id in rules],
                "next_action": _scenario_action(coverage_group, quarantined=quarantined),
            }
        )
    entries.sort(key=lambda item: (item["classification"], item["coverage_group"], item["bin_id"]))
    classification_counts = Counter(entry["classification"] for entry in entries)
    action_counts = Counter(
        entry["next_action"] for entry in entries if entry["classification"] != "trusted_hit"
    )
    group_counts: dict[str, Counter[str]] = defaultdict(Counter)
    for entry in entries:
        group_counts[entry["coverage_group"]][entry["classification"]] += 1
    return {
        "schema_version": 1,
        "policy": (
            "All definition bins remain in scope. A toggle-shadow raw hit is a sampler "
            "repair obligation, not a covered bin and not a reason to shrink the denominator."
        ),
        "summary": {
            "definition_bins": len(entries),
            "classification_counts": dict(sorted(classification_counts.items())),
            "next_action_counts": dict(sorted(action_counts.items())),
            "coverage_group_counts": [
                {"coverage_group": group, **dict(sorted(counts.items()))}
                for group, counts in sorted(group_counts.items())
            ],
        },
        "bins": entries,
    }


def _definitions_from_campaign(campaign_path: Path, native_evidence_index: dict | None = None) -> list[dict[str, Any]]:
    campaign = json.loads(campaign_path.read_text(encoding="utf-8"))
    for record in campaign.get("cases") or []:
        if not record.get("qualification_pass"):
            continue
        for raw_path in record.get("eligible_functional_coverage_artifacts") or []:
            if native_evidence_index:
                try:
                    from .native_runtime_binding import current_metadata
                except ImportError:
                    from native_runtime_binding import current_metadata
                return current_metadata(Path(raw_path), native_evidence_index)["definitions"]
            payload = json.loads(Path(raw_path).read_text(encoding="utf-8"))
            definitions = payload.get("definitions") or []
            if not isinstance(definitions, list) or not definitions:
                raise ValueError(f"functional coverage artifact has no definitions: {raw_path}")
            return [dict(item) for item in definitions]
    raise ValueError("campaign has no qualified functional coverage artifact")


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Classify every trusted-functional-coverage gap by evidence and action"
    )
    parser.add_argument("--native-evidence-index", type=Path)
    parser.add_argument("--native-evidence-index-sha256")
    parser.add_argument("--campaign", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--minimum-hit-rate", type=float, default=0.99)
    parser.add_argument("--minimum-qualified-runs", type=int, default=100)
    return parser.parse_args()


def main() -> int:
    args = _parse_args()
    try:
        from .native_runtime_binding import load_admission_index, mark_coverage_report_scope
    except ImportError:
        from native_runtime_binding import load_admission_index, mark_coverage_report_scope
    native_index = load_admission_index(args.native_evidence_index, args.native_evidence_index_sha256)
    report = build_trusted_report(
        args.campaign,
        native_evidence_index=native_index,
        minimum_hit_rate=args.minimum_hit_rate,
        minimum_qualified_runs=args.minimum_qualified_runs,
    )
    inventory = build_gap_inventory(_definitions_from_campaign(args.campaign, native_index), report)
    inventory["campaign"] = report["campaign"]
    inventory["coverage_contract"] = report["coverage_contract"]
    inventory["coverage"] = report["coverage"]
    inventory["qualification"] = dict(report["qualification"])
    mark_coverage_report_scope(inventory, current_native=native_index is not None)
    _atomic_json(args.output, inventory)
    print(
        f"gap-audit definitions={inventory['summary']['definition_bins']} "
        f"counts={inventory['summary']['classification_counts']} "
        f"output={args.output.resolve()}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
