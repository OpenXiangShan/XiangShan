#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import json
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable


@dataclass(frozen=True)
class Artifact:
    path: Path
    run_id: str
    compatibility_signature: str
    definitions_sha256: str
    definition_bin_ids: frozenset[str]
    hit_bin_ids: frozenset[str]
    bin_descriptions: dict[str, dict[str, str]]
    eligible: bool
    eligibility_reason: str
    program_seed: int | None = None
    timing_seed: int | None = None
    replay_command: str = ""
    native_bin_count: int | None = None
    native_hit_bin_count: int | None = None
    admission_scope: str = "legacy_diagnostic_only"

    @property
    def seed_pair(self) -> tuple[int, int] | None:
        if self.program_seed is None or self.timing_seed is None:
            return None
        return (self.program_seed, self.timing_seed)


def _unique_paths(paths: Iterable[Path]) -> list[Path]:
    result: list[Path] = []
    seen: set[Path] = set()
    for path in paths:
        resolved = path.resolve()
        if resolved in seen:
            continue
        seen.add(resolved)
        result.append(resolved)
    return result


def discover_artifacts(explicit: Iterable[Path], roots: Iterable[Path]) -> list[Path]:
    paths = list(explicit)
    for root in roots:
        if not root.is_dir():
            raise ValueError(f"functional coverage root is not a directory: {root}")
        paths.extend(sorted(root.rglob("*.funcov.json")))
    return _unique_paths(paths)


def load_artifact(
    path: Path, *, native_sidecar: Path | None = None,
    native_sidecar_sha256: str | None = None,
) -> Artifact:
    if not path.is_file():
        raise ValueError(f"functional coverage artifact not found: {path}")
    raw = json.loads(path.read_text(encoding="utf-8"))
    if "groups" in raw:
        try:
            from .native_funcov_adapter import load_native_artifact
        except ImportError:
            from native_funcov_adapter import load_native_artifact
        return load_native_artifact(
            path, raw, sidecar=native_sidecar,
            sidecar_sha256=native_sidecar_sha256,
        )
    provenance = raw.get("provenance") or {}
    compatibility_signature = str(provenance.get("compatibility_signature") or "").strip()
    definitions_sha256 = str(provenance.get("definitions_sha256") or "").strip()
    definitions = raw.get("definitions") or []
    descriptions: dict[str, dict[str, str]] = {}
    for item in definitions:
        if not isinstance(item, dict):
            continue
        bin_id = str(item.get("bin_id") or "").strip()
        if not bin_id:
            continue
        descriptions[bin_id] = {
            "coverage_group": str(item.get("coverage_group") or ""),
            "coverpoint": str(item.get("coverpoint") or ""),
            "bin_name": str(item.get("bin_name") or ""),
            "suggested_testcase": str(item.get("suggested_testcase") or ""),
        }

    hit_bin_ids = frozenset(
        str(hit.get("bin_id") or "").strip()
        for hit in (raw.get("hits") or {}).values()
        if isinstance(hit, dict) and int(hit.get("hits") or 0) > 0 and str(hit.get("bin_id") or "").strip()
    )
    run = raw.get("run") or {}
    run_seeds = run.get("seeds") or {}
    if not isinstance(run_seeds, dict):
        run_seeds = {}

    def optional_int(value: object) -> int | None:
        if value is None or value == "":
            return None
        try:
            return int(value)
        except (TypeError, ValueError):
            return None

    program_seed = optional_int(run_seeds.get("test", run.get("seed")))
    timing_seed = optional_int(run_seeds.get("backend"))
    outcome = raw.get("outcome") or {}
    checker = raw.get("checker") or run.get("checker") or {}
    pytest_outcome = str(outcome.get("status") or run.get("pytest_outcome") or "").lower()
    checker_status = str(checker.get("status") or "").lower()
    checker_error_count = int(checker.get("error_count") or 0)
    stats = raw.get("stats") or {}
    translation_oracle = stats.get("translation_oracle") if isinstance(stats, dict) else {}
    if not isinstance(translation_oracle, dict):
        translation_oracle = {}
    translation_oracle_error_count = int(translation_oracle.get("error_count") or 0)
    oracle_records = translation_oracle.get("records") or []
    unresolved_exceptions = sum(
        isinstance(record, dict) and record.get("kind") == "speculative_cfvec_exception"
        for record in oracle_records
    )
    build_manifest_status = str(provenance.get("build_manifest_status") or "").lower()
    eligible = pytest_outcome in {"pass", "passed"} and checker_status == "pass"
    reasons: list[str] = []
    if not compatibility_signature:
        reasons.append("missing_compatibility_signature")
    if not definitions_sha256:
        reasons.append("missing_definitions_sha256")
    if build_manifest_status != "valid":
        reasons.append(f"build_manifest={build_manifest_status or 'unknown'}")
    if raw.get("artifact_type") == "diagnostic_observed_aggregate" or checker_status == "aggregate":
        reasons.append("aggregate_artifact_not_qualified")
    if checker_error_count != 0:
        reasons.append(f"checker_error_count={checker_error_count}")
    if translation_oracle_error_count != 0:
        reasons.append(
            f"translation_oracle_error_count={translation_oracle_error_count}"
        )
    if unresolved_exceptions:
        reasons.append(f"unproved_speculative_cfvec_exceptions={unresolved_exceptions}")
    if not descriptions:
        reasons.append("missing_definitions")
    unexpected_hits = sorted(hit_bin_ids - descriptions.keys())
    if unexpected_hits:
        reasons.append(f"hits_outside_definitions={len(unexpected_hits)}")
    if not eligible:
        reasons.append(f"outcome={pytest_outcome or 'unknown'}")
        reasons.append(f"checker={checker_status or 'unknown'}")
    eligible = eligible and not reasons
    return Artifact(
        path=path.resolve(),
        run_id=str(run.get("run_id") or raw.get("artifact_tag") or path.stem),
        compatibility_signature=compatibility_signature,
        definitions_sha256=definitions_sha256,
        definition_bin_ids=frozenset(descriptions),
        hit_bin_ids=hit_bin_ids,
        bin_descriptions=descriptions,
        eligible=eligible,
        eligibility_reason=",".join(reasons) if reasons else "eligible",
        program_seed=program_seed,
        timing_seed=timing_seed,
        replay_command=str(run.get("run_command") or ""),
    )


def _union(artifacts: Iterable[Artifact]) -> set[str]:
    result: set[str] = set()
    for artifact in artifacts:
        result.update(artifact.hit_bin_ids)
    return result


def _contract_groups(artifacts: Iterable[Artifact]) -> list[dict]:
    grouped: dict[tuple[str, str], list[Artifact]] = {}
    for artifact in artifacts:
        grouped.setdefault(
            (artifact.compatibility_signature, artifact.definitions_sha256), []
        ).append(artifact)
    result: list[dict] = []
    for (signature, definitions_sha256), members in sorted(grouped.items()):
        union = _union(members)
        definition_sets = {member.definition_bin_ids for member in members}
        definitions = next(iter(definition_sets)) if len(definition_sets) == 1 else frozenset()
        result.append(
            {
                "compatibility_signature": signature,
                "definitions_sha256": definitions_sha256,
                "artifact_count": len(members),
                "consistent_definition_set": len(definition_sets) == 1,
                "total_definition_bins": len(definitions) if definitions else None,
                "union_hit_bins": len(union),
                "hit_rate": (len(union) / len(definitions) if definitions else None),
                "hit_bin_ids": sorted(union),
            }
        )
    return result


def _artifact_reference(artifact: Artifact) -> dict:
    seed_pair = artifact.seed_pair
    return {
        "run_id": artifact.run_id,
        "artifact": str(artifact.path),
        "program_seed": artifact.program_seed,
        "timing_seed": artifact.timing_seed,
        "seed_pair": list(seed_pair) if seed_pair is not None else None,
        "replay_command": artifact.replay_command,
        "hit_bins": len(artifact.hit_bin_ids),
    }


def _coverage_guided_corpus(
    artifacts: list[Artifact],
    target_union: set[str],
) -> dict:
    """Build a deterministic coverage-equivalent subset without deleting evidence.

    Greedy set cover is followed by deterministic reverse deletion.  The result
    is an approximate minimum, not a proof of globally minimum cardinality.
    """

    if not artifacts:
        return {
            "status": "unavailable",
            "reason": "no_eligible_random_artifacts",
            "policy": "deterministic_greedy_set_cover_then_reverse_delete",
            "qualified_run_count": 0,
            "retained_run_count": 0,
            "discard_candidate_count": 0,
            "union_preserved": False,
            "retained_runs": [],
            "discard_candidates": [],
            "bin_repeatability": {},
        }

    candidates = sorted(artifacts, key=lambda item: (item.run_id, str(item.path)))
    uncovered = set(target_union)
    greedy_selected: list[tuple[Artifact, set[str]]] = []
    remaining = list(candidates)
    while uncovered:
        ranked = sorted(
            remaining,
            key=lambda item: (
                -len(item.hit_bin_ids & uncovered),
                -len(item.hit_bin_ids),
                item.run_id,
                str(item.path),
            ),
        )
        if not ranked:
            break
        selected = ranked[0]
        new_hits = set(selected.hit_bin_ids) & uncovered
        if not new_hits:
            break
        greedy_selected.append((selected, new_hits))
        uncovered.difference_update(selected.hit_bin_ids)
        remaining.remove(selected)

    retained = [artifact for artifact, _ in greedy_selected]
    for candidate in reversed([artifact for artifact, _ in greedy_selected]):
        others = [artifact for artifact in retained if artifact is not candidate]
        if target_union <= _union(others):
            retained.remove(candidate)

    retained_union = _union(retained)
    retained_paths = {artifact.path for artifact in retained}
    greedy_gain = {
        artifact.path: sorted(new_hits) for artifact, new_hits in greedy_selected
    }
    retained_records: list[dict] = []
    for selection_index, artifact in enumerate(retained):
        other_union = _union(item for item in retained if item is not artifact)
        irreplaceable = sorted(target_union - other_union)
        retained_records.append(
            {
                **_artifact_reference(artifact),
                "selection_index": selection_index,
                "greedy_new_bin_count": len(greedy_gain[artifact.path]),
                "greedy_new_bin_ids": greedy_gain[artifact.path],
                "irreplaceable_bin_count_in_retained_set": len(irreplaceable),
                "irreplaceable_bin_ids_in_retained_set": irreplaceable,
            }
        )
    discard_candidates = [
        {
            **_artifact_reference(artifact),
            "reason": "coverage_redundant_with_retained_set",
        }
        for artifact in candidates
        if artifact.path not in retained_paths
    ]

    artifact_support: dict[str, int] = {bin_id: 0 for bin_id in target_union}
    seed_pair_support: dict[str, set[tuple[int, int]]] = {
        bin_id: set() for bin_id in target_union
    }
    for artifact in artifacts:
        for bin_id in artifact.hit_bin_ids & target_union:
            artifact_support[bin_id] += 1
            if artifact.seed_pair is not None:
                seed_pair_support[bin_id].add(artifact.seed_pair)
    artifact_repeatable = sorted(
        bin_id for bin_id, count in artifact_support.items() if count >= 2
    )
    seed_repeatable = sorted(
        bin_id for bin_id, pairs in seed_pair_support.items() if len(pairs) >= 2
    )
    seed_singletons = sorted(
        bin_id for bin_id, pairs in seed_pair_support.items() if len(pairs) == 1
    )
    missing_seed_attribution = sorted(
        artifact.run_id for artifact in artifacts if artifact.seed_pair is None
    )
    support_histogram: dict[str, int] = {}
    for pairs in seed_pair_support.values():
        key = str(len(pairs))
        support_histogram[key] = support_histogram.get(key, 0) + 1

    return {
        "status": "available" if not uncovered and retained_union == target_union else "invalid",
        "reason": "" if not uncovered else "target_union_not_covered",
        "policy": "deterministic_greedy_set_cover_then_reverse_delete",
        "minimum_claim": "coverage_equivalent_approximation_not_global_cardinality_minimum",
        "deletion_policy": "report_only_no_artifacts_deleted",
        "goal_qualification_policy": (
            "all qualified goal runs remain in the 100-case gate; the retained set is only "
            "a future regression/minimization recommendation"
        ),
        "qualified_run_count": len(artifacts),
        "full_union_bins": len(target_union),
        "retained_run_count": len(retained),
        "retained_union_bins": len(retained_union),
        "discard_candidate_count": len(discard_candidates),
        "union_preserved": retained_union == target_union,
        "uncovered_bin_ids": sorted(target_union - retained_union),
        "retained_runs": retained_records,
        "discard_candidates": discard_candidates,
        "bin_repeatability": {
            "seed_attribution_complete": not missing_seed_attribution,
            "runs_missing_seed_pair": missing_seed_attribution,
            "distinct_seed_pairs": len(
                {artifact.seed_pair for artifact in artifacts if artifact.seed_pair is not None}
            ),
            "hit_bins": len(target_union),
            "bins_hit_by_multiple_artifacts": len(artifact_repeatable),
            "bins_hit_by_multiple_seed_pairs": len(seed_repeatable),
            "bins_hit_by_one_seed_pair": len(seed_singletons),
            "multi_seed_repeatability_rate": (
                len(seed_repeatable) / len(target_union) if target_union else None
            ),
            "seed_pair_support_histogram": dict(
                sorted(support_histogram.items(), key=lambda item: int(item[0]))
            ),
            "single_seed_bin_ids": seed_singletons,
        },
    }


def build_report(
    random_artifacts: list[Artifact],
    baseline_artifacts: list[Artifact],
    *,
    minimum_hit_rate: float = 0.99,
    minimum_qualified_runs: int = 0,
) -> dict:
    if not 0.0 <= minimum_hit_rate <= 1.0:
        raise ValueError("minimum_hit_rate must be in [0, 1]")
    if minimum_qualified_runs < 0:
        raise ValueError("minimum_qualified_runs must be >= 0")
    eligible_random = [artifact for artifact in random_artifacts if artifact.eligible]
    eligible_baseline = [artifact for artifact in baseline_artifacts if artifact.eligible]
    random_contracts = {
        (artifact.compatibility_signature, artifact.definitions_sha256)
        for artifact in eligible_random
    }
    strict_contract = next(iter(random_contracts)) if len(random_contracts) == 1 else None
    definition_sets = {artifact.definition_bin_ids for artifact in eligible_random}
    consistent_definitions = bool(eligible_random) and len(definition_sets) == 1
    strictly_comparable = strict_contract is not None and consistent_definitions
    comparable_baseline = (
        [
            artifact
            for artifact in eligible_baseline
            if (artifact.compatibility_signature, artifact.definitions_sha256) == strict_contract
            and artifact.definition_bin_ids == next(iter(definition_sets))
        ]
        if strictly_comparable
        else []
    )
    skipped_baseline = [
        artifact
        for artifact in baseline_artifacts
        if artifact not in comparable_baseline
    ]

    random_union = _union(eligible_random) if strictly_comparable else set()
    baseline_union = _union(comparable_baseline)
    definition_bin_ids = set(next(iter(definition_sets))) if strictly_comparable else set()
    unhit_bin_ids = sorted(definition_bin_ids - random_union)
    hit_rate = len(random_union) / len(definition_bin_ids) if definition_bin_ids else None
    known_bins: dict[str, dict[str, str]] = {}
    for artifact in [*eligible_random, *comparable_baseline]:
        known_bins.update(artifact.bin_descriptions)

    seen: set[str] = set()
    marginal: list[dict] = []
    for artifact in eligible_random if strictly_comparable else []:
        new_hits = sorted(artifact.hit_bin_ids - seen)
        seen.update(artifact.hit_bin_ids)
        marginal.append(
            {
                **_artifact_reference(artifact),
                "new_random_union_bins": len(new_hits),
                "new_bin_ids": new_hits,
                "cumulative_random_union_bins": len(seen),
            }
        )

    coverage_guided_corpus = (
        _coverage_guided_corpus(eligible_random, random_union)
        if strictly_comparable
        else {
            "status": "unavailable",
            "reason": "random_artifacts_are_not_strictly_comparable",
            "policy": "deterministic_greedy_set_cover_then_reverse_delete",
            "qualified_run_count": len(eligible_random),
            "retained_run_count": 0,
            "discard_candidate_count": 0,
            "union_preserved": False,
            "retained_runs": [],
            "discard_candidates": [],
            "bin_repeatability": {},
        }
    )

    random_only = sorted(random_union - baseline_union)
    baseline_only = sorted(baseline_union - random_union)
    both = sorted(random_union & baseline_union)
    qualification_reasons: list[str] = []
    if not eligible_random:
        qualification_reasons.append("no_eligible_random_artifacts")
    if len(random_contracts) > 1:
        qualification_reasons.append("multiple_random_compatibility_contracts")
    if eligible_random and not consistent_definitions:
        qualification_reasons.append("inconsistent_definition_sets")
    if len(eligible_random) < minimum_qualified_runs:
        qualification_reasons.append(
            f"qualified_runs={len(eligible_random)}<required={minimum_qualified_runs}"
        )
    if hit_rate is None:
        qualification_reasons.append("hit_rate_unavailable")
    elif hit_rate < minimum_hit_rate:
        qualification_reasons.append(
            f"hit_rate={hit_rate:.8f}<required={minimum_hit_rate:.8f}"
        )
    return {
        "schema_version": 2,
        "comparison_policy": (
            "the qualified random union requires exactly one compatibility signature, one definitions "
            "hash, and an identical definition set; baseline comparison uses that same contract"
        ),
        "strictly_comparable": strictly_comparable,
        "random_compatibility_contracts": [
            {
                "compatibility_signature": signature,
                "definitions_sha256": definitions_sha256,
            }
            for signature, definitions_sha256 in sorted(random_contracts)
        ],
        "random": {
            "input_count": len(random_artifacts),
            "eligible_count": len(eligible_random),
            "total_definition_bins": len(definition_bin_ids) if strictly_comparable else None,
            "union_hit_bins": len(random_union) if strictly_comparable else None,
            "hit_rate": hit_rate,
            "hit_bin_ids": sorted(random_union) if strictly_comparable else [],
            "unhit_bin_ids": unhit_bin_ids,
            "by_contract": _contract_groups(eligible_random),
        },
        "directed_baseline": {
            "input_count": len(baseline_artifacts),
            "comparable_count": len(comparable_baseline),
            "union_hit_bins": len(baseline_union),
            "hit_bin_ids": sorted(baseline_union),
            "comparable_artifacts": [
                {
                    "run_id": artifact.run_id,
                    "path": str(artifact.path),
                    "hit_bins": len(artifact.hit_bin_ids),
                }
                for artifact in comparable_baseline
            ],
        },
        "comparison": {
            "both_hit_bins": len(both),
            "random_only_bins": len(random_only),
            "baseline_only_bins": len(baseline_only),
            "both_bin_ids": both,
            "random_only_bin_ids": random_only,
            "baseline_only_bin_ids": baseline_only,
            "random_has_new_bins": bool(random_only),
        },
        "per_random_artifact_marginal_gain": marginal,
        "coverage_guided_corpus": coverage_guided_corpus,
        "qualification": {
            "pass": not qualification_reasons,
            "minimum_hit_rate": minimum_hit_rate,
            "minimum_qualified_runs": minimum_qualified_runs,
            "reasons": qualification_reasons,
        },
        "ineligible_random_artifacts": [
            {"path": str(item.path), "reason": item.eligibility_reason,
             **({"native_bin_count": item.native_bin_count,
                 "native_hit_bin_count": item.native_hit_bin_count}
                if item.native_bin_count is not None else {})}
            for item in random_artifacts
            if not item.eligible
        ],
        "skipped_baseline_artifacts": [
            {
                "path": str(item.path),
                "reason": (
                    item.eligibility_reason
                    if not item.eligible
                    else "incompatible_signature"
                ),
            }
            for item in skipped_baseline
        ],
        "bin_descriptions": {bin_id: known_bins.get(bin_id, {}) for bin_id in sorted(random_union | baseline_union)},
    }


def write_report(report: dict, output: Path) -> None:
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(json.dumps(report, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    csv_path = output.with_suffix(".csv")
    random_ids = set(report["random"]["hit_bin_ids"])
    baseline_ids = set(report["directed_baseline"]["hit_bin_ids"])
    descriptions = report["bin_descriptions"]
    with csv_path.open("w", encoding="utf-8", newline="") as stream:
        writer = csv.DictWriter(
            stream,
            fieldnames=(
                "Bin_ID",
                "Random_Hit",
                "Directed_Baseline_Hit",
                "Delta",
                "Coverage_Group",
                "Coverpoint",
                "Bin_Name",
                "Suggested_Testcase",
            ),
        )
        writer.writeheader()
        for bin_id in sorted(random_ids | baseline_ids):
            random_hit = bin_id in random_ids
            baseline_hit = bin_id in baseline_ids
            if random_hit and not baseline_hit:
                delta = "random_only"
            elif baseline_hit and not random_hit:
                delta = "baseline_only"
            else:
                delta = "both"
            description = descriptions.get(bin_id, {})
            writer.writerow(
                {
                    "Bin_ID": bin_id,
                    "Random_Hit": int(random_hit),
                    "Directed_Baseline_Hit": int(baseline_hit),
                    "Delta": delta,
                    "Coverage_Group": description.get("coverage_group", ""),
                    "Coverpoint": description.get("coverpoint", ""),
                    "Bin_Name": description.get("bin_name", ""),
                    "Suggested_Testcase": description.get("suggested_testcase", ""),
                }
            )


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Compare random functional coverage with compatible directed artifacts"
    )
    parser.add_argument("--native-evidence-index", type=Path)
    parser.add_argument("--native-evidence-index-sha256")
    parser.add_argument("--random-artifact", action="append", type=Path, default=[])
    parser.add_argument("--random-root", action="append", type=Path, default=[])
    parser.add_argument("--baseline-artifact", action="append", type=Path, default=[])
    parser.add_argument("--baseline-root", action="append", type=Path, default=[])
    parser.add_argument("--minimum-hit-rate", type=float, default=0.99)
    parser.add_argument("--minimum-qualified-runs", type=int, default=0)
    parser.add_argument(
        "--require-qualified-union",
        action="store_true",
        help="return non-zero unless the strict compatible union satisfies both thresholds",
    )
    parser.add_argument("--output", type=Path, required=True)
    return parser.parse_args()


def main() -> int:
    args = _parse_args()
    try:
        random_paths = discover_artifacts(args.random_artifact, args.random_root)
        baseline_paths = discover_artifacts(args.baseline_artifact, args.baseline_root)
        if not random_paths:
            raise ValueError("no random functional coverage artifacts found")
        try:
            from .native_runtime_binding import load_admission_index, load_current_artifact, mark_coverage_report_scope
        except ImportError:
            from native_runtime_binding import load_admission_index, load_current_artifact, mark_coverage_report_scope
        index = load_admission_index(args.native_evidence_index, args.native_evidence_index_sha256)
        random_artifacts = [
            load_current_artifact(path, index) if index is not None else load_artifact(path)
            for path in random_paths
        ]
        if index is not None and len({artifact.run_id for artifact in random_artifacts}) != len(random_artifacts):
            raise ValueError("current coverage union has multiple artifacts for the same run")
        report = build_report(
            random_artifacts,
            [load_artifact(path) for path in baseline_paths],
            minimum_hit_rate=args.minimum_hit_rate,
            minimum_qualified_runs=args.minimum_qualified_runs,
        )
    except (OSError, ValueError, json.JSONDecodeError) as exc:
        raise SystemExit(f"funcov comparison failed: {exc}") from exc
    mark_coverage_report_scope(report, current_native=index is not None)
    write_report(report, args.output)
    comparison = report["comparison"]
    print(
        f"random_union={report['random']['union_hit_bins']} "
        f"total_definitions={report['random']['total_definition_bins']} "
        f"hit_rate={report['random']['hit_rate']} "
        f"directed_union={report['directed_baseline']['union_hit_bins']} "
        f"random_only={comparison['random_only_bins']} "
        f"baseline_only={comparison['baseline_only_bins']} "
        f"report={args.output.resolve()}"
    )
    if args.require_qualified_union and not report["qualification"]["pass"]:
        print("qualification_failed=" + ";".join(report["qualification"]["reasons"]))
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
