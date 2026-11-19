#!/usr/bin/env python3
from __future__ import annotations

import argparse
import ast
import hashlib
import json
import re
import subprocess
from collections import defaultdict
from pathlib import Path
from typing import Any, Callable, Iterable

try:
    from .funcov_compare import Artifact, load_artifact
except ImportError:
    from funcov_compare import Artifact, load_artifact


REPORT_SCHEMA_VERSION = 1
REPO_ROOT = Path(__file__).resolve().parents[6]
SAMPLER_SOURCES = {
    "compact": "src/test/python/Frontend/env/funcov/py/ifu/compact_funcov.py",
    "mmio_nc_owner": "src/test/python/Frontend/env/funcov/py/ifu/mmio_nc_owner_funcov.py",
    "instr_uncache_owner": "src/test/python/Frontend/env/funcov/py/ifu/instr_uncache_owner_funcov.py",
    "cacheable_pipeline": "src/test/python/Frontend/env/funcov/py/ifu/cacheable_pipeline_funcov.py",
    "mmio_v3": "src/test/python/Frontend/env/funcov/py/ifu/mmio_v3_funcov.py",
}


def _sha256_bytes(payload: bytes) -> str:
    return hashlib.sha256(payload).hexdigest()


def _atomic_json(path: Path, payload: dict[str, Any]) -> None:
    path = path.resolve()
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(f".{path.name}.tmp")
    temporary.write_text(
        json.dumps(payload, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    temporary.replace(path)


def _git_source(repo_root: Path, revision: str, relative_path: str) -> str:
    result = subprocess.run(
        ["git", "show", f"{revision}:{relative_path}"],
        cwd=repo_root,
        check=True,
        capture_output=True,
        text=True,
    )
    return result.stdout


def _literal_assignment(source: str, name: str) -> Any:
    tree = ast.parse(source)
    for node in tree.body:
        if not isinstance(node, (ast.Assign, ast.AnnAssign)):
            continue
        targets = node.targets if isinstance(node, ast.Assign) else [node.target]
        if not any(isinstance(target, ast.Name) and target.id == name for target in targets):
            continue
        value = node.value
        if isinstance(value, ast.Call) and isinstance(value.func, ast.Name):
            if value.func.id == "frozenset" and len(value.args) == 1:
                value = value.args[0]
        return ast.literal_eval(value)
    raise ValueError(f"assignment not found or not literal: {name}")


def _prefix_order(prefixes: Iterable[str]) -> str:
    values = tuple(str(value) for value in prefixes)
    shadow = next((index for index, value in enumerate(values) if "__Vtogcov__" in value), None)
    semantic = next((index for index, value in enumerate(values) if "__Vtogcov__" not in value), None)
    if shadow is None:
        return "semantic_only"
    if semantic is None:
        return "shadow_only"
    return "shadow_first" if shadow < semantic else "semantic_first"


def _token_precedes(source: str, first: str, second: str) -> bool:
    first_index = source.find(first)
    second_index = source.find(second)
    return first_index >= 0 and second_index >= 0 and first_index < second_index


def _definition_indexes(
    definitions: Iterable[dict[str, Any]],
) -> tuple[dict[str, dict[str, Any]], dict[tuple[str, str], str]]:
    by_id: dict[str, dict[str, Any]] = {}
    by_key: dict[tuple[str, str], str] = {}
    for item in definitions:
        bin_id = str(item.get("bin_id") or "").strip()
        group = str(item.get("coverage_group") or "").strip()
        bin_name = str(item.get("bin_name") or "").strip()
        if not bin_id or not group or not bin_name:
            raise ValueError("functional coverage definition is missing bin_id/group/bin_name")
        key = (group, bin_name)
        if bin_id in by_id:
            raise ValueError(f"duplicate functional coverage bin ID: {bin_id}")
        if key in by_key:
            raise ValueError(f"duplicate functional coverage group/bin key: {key}")
        by_id[bin_id] = dict(item)
        by_key[key] = bin_id
    return by_id, by_key


def _rule_record(
    *,
    rule_id: str,
    source_path: str,
    reason: str,
    active: bool,
    candidate_bin_ids: Iterable[str],
    definition_bin_ids: set[str],
    raw_union: set[str],
) -> dict[str, Any]:
    candidates = sorted(set(str(bin_id) for bin_id in candidate_bin_ids))
    affected = sorted(set(candidates) & definition_bin_ids)
    raw_hits = sorted(set(affected) & raw_union)
    return {
        "rule_id": rule_id,
        "source_path": source_path,
        "reason": reason,
        "active": bool(active),
        "candidate_bin_count": len(candidates),
        "definition_bin_count": len(affected),
        "raw_hit_bin_count": len(raw_hits),
        "definition_bin_ids": affected,
        "raw_hit_bin_ids": raw_hits,
    }


def build_quarantine_policy(
    definitions: Iterable[dict[str, Any]],
    raw_union: set[str],
    *,
    source_loader: Callable[[str], str],
) -> dict[str, Any]:
    definitions = tuple(dict(item) for item in definitions)
    by_id, by_key = _definition_indexes(definitions)
    source_text = {name: source_loader(path) for name, path in SAMPLER_SOURCES.items()}
    source_audit: list[dict[str, Any]] = []

    compact_prefixes = tuple(
        _literal_assignment(source_text["compact"], "_IFU_INTERNAL_PREFIXES")
    )
    compact_order = _prefix_order(compact_prefixes)
    compact_keys = set(
        tuple(item)
        for item in _literal_assignment(source_text["compact"], "COMPACT_SAMPLER_BIN_KEYS")
    )
    compact_explicit_ids = set(re.findall(r"BIN-[0-9]+", source_text["compact"]))

    mmio_ifu_prefixes = tuple(
        _literal_assignment(source_text["mmio_nc_owner"], "_IFU_PREFIXES")
    )
    instr_uncache_prefixes = tuple(
        _literal_assignment(source_text["mmio_nc_owner"], "_INSTR_UNCACHE_PREFIXES")
    )
    mmio_ifu_order = _prefix_order(mmio_ifu_prefixes)
    instr_uncache_order = _prefix_order(instr_uncache_prefixes)
    mmio_explicit_ids = set(re.findall(r"BIN-[0-9]+", source_text["mmio_nc_owner"]))

    mmio_v3_prefixes = tuple(
        _literal_assignment(source_text["mmio_v3"], "_IFU_PREFIXES")
    )
    mmio_v3_order = _prefix_order(mmio_v3_prefixes)

    for name, relative_path in SAMPLER_SOURCES.items():
        content = source_text[name]
        source_audit.append(
            {
                "source": name,
                "source_path": relative_path,
                "source_sha256": _sha256_bytes(content.encode("utf-8")),
                "toggle_shadow_reference_count": content.count("__Vtogcov__"),
            }
        )
    for record in source_audit:
        if record["source"] == "compact":
            record["ifu_prefix_order"] = compact_order
        elif record["source"] == "mmio_nc_owner":
            record["ifu_prefix_order"] = mmio_ifu_order
            record["instr_uncache_prefix_order"] = instr_uncache_order
        elif record["source"] == "mmio_v3":
            record["ifu_prefix_order"] = mmio_v3_order

    compact_candidates = {
        by_key[key] for key in compact_keys if key in by_key
    } | compact_explicit_ids
    owner_groups = {
        "ifu_mmio_owner_v3",
        "ifu_nc_owner_v3",
        "ifu_instruncache_owner_v3",
    }
    owner_candidates = {
        bin_id
        for bin_id, item in by_id.items()
        if str(item["coverage_group"]) in owner_groups
    } | mmio_explicit_ids

    cache_source = source_text["cacheable_pipeline"]
    wb_shadow_only = (
        '"wb_redirect": (' in cache_source
        and '__Vtogcov__wbRedirect_valid' in cache_source
        and not any(
            token in cache_source
            for token in (
                "Frontend_top.Frontend.inner_ifu.io_toFtq_wbRedirect_valid",
                "Frontend_top.Frontend._inner_ifu_io_toFtq_wbRedirect_valid",
            )
        )
    )
    bpu_shadow_first = _token_precedes(
        cache_source,
        '__Vtogcov__io_fromFtq_flushFromBpu_s3_valid',
        'mainPipe.io_flushFromBpu_s3_valid',
    )
    late_fault_shadow = all(
        token in cache_source
        for token in (
            '__Vtogcov__io_toIBuffer_ready',
            '__Vtogcov__io_toIBuffer_bits_exceptionType_value',
            '__Vtogcov__io_toIBuffer_bits_enqEnable',
        )
    ) and not all(
        any(token in cache_source for token in candidates)
        for candidates in (
            (
                "Frontend_top.io_backend_toIBuf_decodeCanAccept",
                "Frontend_top.Frontend.inner_ifu.io_toIBuffer_ready",
            ),
            (
                "Frontend_top.Frontend._inner_ifu_io_toIBuffer_bits_exceptionType_value",
                "Frontend_top.Frontend.inner_ifu.io_toIBuffer_bits_exceptionType_value",
            ),
            (
                "Frontend_top.Frontend.inner_ifu.io_toIBuffer_bits_enqEnable_0",
                "Frontend_top.Frontend._inner_ifu_io_toIBuffer_bits_enqEnable",
            ),
        )
    )

    rules = [
        _rule_record(
            rule_id="compact_module_shadow_first",
            source_path=SAMPLER_SOURCES["compact"],
            reason=(
                "compact sampler resolves retained IFU paths from __Vtogcov__ before the "
                "semantic RTL signal; module-produced observations are quarantined"
            ),
            active=compact_order == "shadow_first",
            candidate_bin_ids=compact_candidates,
            definition_bin_ids=set(by_id),
            raw_union=raw_union,
        ),
        _rule_record(
            rule_id="mmio_nc_owner_shadow_first",
            source_path=SAMPLER_SOURCES["mmio_nc_owner"],
            reason=(
                "MMIO/NC/InstrUncache owner snapshot resolves IFU or InstrUncache "
                "__Vtogcov__ storage before semantic RTL storage"
            ),
            active=(
                mmio_ifu_order == "shadow_first"
                or instr_uncache_order == "shadow_first"
            ),
            candidate_bin_ids=owner_candidates,
            definition_bin_ids=set(by_id),
            raw_union=raw_union,
        ),
        _rule_record(
            rule_id="cacheable_wb_redirect_shadow_only",
            source_path=SAMPLER_SOURCES["cacheable_pipeline"],
            reason="wbRedirect coverage reads only Verilator toggle-shadow paths",
            active=wb_shadow_only,
            candidate_bin_ids=(
                by_key.get(("ifu_cacheable_flush", "wb_redirect_blocks"), ""),
            ),
            definition_bin_ids=set(by_id),
            raw_union=raw_union,
        ),
        _rule_record(
            rule_id="cacheable_bpu_flush_shadow_first",
            source_path=SAMPLER_SOURCES["cacheable_pipeline"],
            reason="BPU s3 flush candidate order selects toggle-shadow before semantic MainPipe signal",
            active=bpu_shadow_first,
            candidate_bin_ids=(
                by_key.get(("ifu_cacheable_flush", "bpu_match_blocks"), ""),
                by_key.get(("ifu_cacheable_flush", "bpu_miss_allows"), ""),
                "BIN-901",
            ),
            definition_bin_ids=set(by_id),
            raw_union=raw_union,
        ),
        _rule_record(
            rule_id="cacheable_late_fault_shadow_first",
            source_path=SAMPLER_SOURCES["cacheable_pipeline"],
            reason="late-fault delivery attribution reads toggle-shadow IBuffer fields",
            active=late_fault_shadow,
            candidate_bin_ids=("BIN-906",),
            definition_bin_ids=set(by_id),
            raw_union=raw_union,
        ),
    ]
    active_rules = [rule for rule in rules if rule["active"]]
    quarantine_ids = sorted(
        {
            bin_id
            for rule in active_rules
            for bin_id in rule["definition_bin_ids"]
        }
    )
    reasons_by_bin: dict[str, list[str]] = defaultdict(list)
    for rule in active_rules:
        for bin_id in rule["definition_bin_ids"]:
            reasons_by_bin[bin_id].append(str(rule["rule_id"]))

    return {
        "policy": (
            "a raw hit is trusted only when its producing sampler is not covered by an "
            "active toggle-shadow quarantine rule; quarantined bins remain in the denominator"
        ),
        "toggle_shadow_semantics": (
            "Verilator __Vtogcov__ fields are coverage bookkeeping storage, not an "
            "authoritative current-cycle RTL value"
        ),
        "source_audit": source_audit,
        "rules": rules,
        "active_rule_ids": [str(rule["rule_id"]) for rule in active_rules],
        "quarantined_definition_bin_count": len(quarantine_ids),
        "quarantined_definition_bin_ids": quarantine_ids,
        "quarantined_raw_hit_bin_ids": sorted(set(quarantine_ids) & raw_union),
        "reasons_by_bin_id": dict(sorted(reasons_by_bin.items())),
    }


def _definitions_from_artifact(path: Path) -> list[dict[str, Any]]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    definitions = payload.get("definitions") or []
    if not isinstance(definitions, list) or not definitions:
        raise ValueError(f"functional coverage artifact has no definitions: {path}")
    return [dict(item) for item in definitions]


def _provenance_from_artifact(path: Path) -> dict[str, Any]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    provenance = payload.get("provenance") or {}
    return dict(provenance) if isinstance(provenance, dict) else {}


def _coverage_group_summary(
    definitions: Iterable[dict[str, Any]],
    raw_union: set[str],
    trusted_union: set[str],
    quarantined: set[str],
) -> list[dict[str, Any]]:
    groups: dict[str, dict[str, Any]] = {}
    for item in definitions:
        group = str(item["coverage_group"])
        record = groups.setdefault(
            group,
            {
                "coverage_group": group,
                "definition_bins": 0,
                "raw_hit_bins": 0,
                "trusted_hit_bins": 0,
                "quarantined_definition_bins": 0,
            },
        )
        bin_id = str(item["bin_id"])
        record["definition_bins"] += 1
        record["raw_hit_bins"] += int(bin_id in raw_union)
        record["trusted_hit_bins"] += int(bin_id in trusted_union)
        record["quarantined_definition_bins"] += int(bin_id in quarantined)
    for record in groups.values():
        total = int(record["definition_bins"])
        record["raw_hit_rate"] = int(record["raw_hit_bins"]) / total
        record["trusted_hit_rate"] = int(record["trusted_hit_bins"]) / total
    return sorted(groups.values(), key=lambda item: str(item["coverage_group"]))


def _marginal_gain(
    artifacts: Iterable[Artifact], quarantined: set[str]
) -> list[dict[str, Any]]:
    raw_seen: set[str] = set()
    trusted_seen: set[str] = set()
    result: list[dict[str, Any]] = []
    for artifact in artifacts:
        raw_hits = set(artifact.hit_bin_ids)
        trusted_hits = raw_hits - quarantined
        raw_new = sorted(raw_hits - raw_seen)
        trusted_new = sorted(trusted_hits - trusted_seen)
        raw_seen.update(raw_hits)
        trusted_seen.update(trusted_hits)
        result.append(
            {
                "run_id": artifact.run_id,
                "artifact": str(artifact.path),
                "program_seed": artifact.program_seed,
                "timing_seed": artifact.timing_seed,
                "raw_hit_bins": len(raw_hits),
                "trusted_hit_bins": len(trusted_hits),
                "new_raw_union_bins": len(raw_new),
                "new_raw_bin_ids": raw_new,
                "cumulative_raw_union_bins": len(raw_seen),
                "new_trusted_union_bins": len(trusted_new),
                "new_trusted_bin_ids": trusted_new,
                "cumulative_trusted_union_bins": len(trusted_seen),
            }
        )
    return result


def build_trusted_report(
    campaign_path: Path,
    *,
    source_loader: Callable[[str], str] | None = None,
    minimum_hit_rate: float = 0.99,
    minimum_qualified_runs: int = 100,
    native_evidence_index: dict | None = None,
) -> dict[str, Any]:
    if not 0.0 <= minimum_hit_rate <= 1.0:
        raise ValueError("minimum_hit_rate must be in [0, 1]")
    if minimum_qualified_runs < 0:
        raise ValueError("minimum_qualified_runs must be >= 0")
    campaign_path = campaign_path.resolve()
    campaign = json.loads(campaign_path.read_text(encoding="utf-8"))
    try:
        from .native_runtime_binding import current_metadata, current_source_loader, load_current_artifact
    except ImportError:
        from native_runtime_binding import current_metadata, current_source_loader, load_current_artifact
    records = list(campaign.get("cases") or [])
    completed = [record for record in records if record.get("execution_status") == "completed"]
    manifest_qualified = [
        record for record in completed if record.get("qualification_pass")
    ]
    qualified: list[dict[str, Any]] = []
    artifacts: list[Artifact] = []
    rejected_cases: list[dict[str, Any]] = []
    current_run_ids: set[str] = set()
    for record in manifest_qualified:
        case_artifacts = [
            load_current_artifact(Path(path).resolve(), native_evidence_index)
            if native_evidence_index else load_artifact(Path(path).resolve())
            for path in record.get("eligible_functional_coverage_artifacts") or []
        ]
        ineligible = [artifact for artifact in case_artifacts if not artifact.eligible]
        if ineligible:
            rejected_cases.append(
                {
                    "case": str(record.get("name") or record.get("run_id") or ""),
                    "artifacts": [
                        {
                            "path": str(artifact.path),
                            "reason": artifact.eligibility_reason,
                        }
                        for artifact in ineligible
                    ],
                }
            )
            continue
        if native_evidence_index is not None:
            if not case_artifacts:
                raise ValueError("qualified current case has no indexed native artifact")
            run_ids = {artifact.run_id for artifact in case_artifacts}
            if len(run_ids) != 1 or run_ids & current_run_ids:
                raise ValueError("current campaign reuses a native run or mixes run identities")
            current_run_ids.update(run_ids)
        qualified.append(record)
        artifacts.extend(case_artifacts)
    contracts = {
        (artifact.compatibility_signature, artifact.definitions_sha256)
        for artifact in artifacts
    }
    definition_sets = {artifact.definition_bin_ids for artifact in artifacts}
    if artifacts and (len(contracts) != 1 or len(definition_sets) != 1):
        raise ValueError("qualified campaign artifacts do not share one strict coverage contract")
    if not artifacts:
        raise ValueError("campaign has no qualified functional coverage artifacts")

    native_contract = (current_metadata(artifacts[0].path, native_evidence_index)
                       if native_evidence_index else None)
    definitions = (native_contract["definitions"] if native_contract
                   else _definitions_from_artifact(artifacts[0].path))
    definition_ids = set(artifacts[0].definition_bin_ids)
    raw_union = set().union(*(set(artifact.hit_bin_ids) for artifact in artifacts))
    manifest_raw_union = set().union(
        *(set(record.get("observed_bin_ids") or []) for record in qualified)
    )
    if raw_union != manifest_raw_union:
        raise ValueError(
            "campaign observed_bin_ids disagree with eligible functional coverage artifacts"
        )

    repo_root = Path(str(campaign.get("repo_root") or REPO_ROOT)).resolve()
    revision = str(campaign.get("repo_head") or "").strip()
    if source_loader is None and native_evidence_index:
        source_loader = current_source_loader(artifacts[0].path, native_evidence_index)
    if source_loader is None:
        if not revision:
            raise ValueError("campaign repo_head is required for sampler source audit")
        source_loader = lambda relative_path: _git_source(repo_root, revision, relative_path)
    quarantine = build_quarantine_policy(
        definitions,
        raw_union,
        source_loader=source_loader,
    )
    quarantined = set(quarantine["quarantined_definition_bin_ids"])
    trusted_union = raw_union - quarantined
    provenance = (native_contract["provenance"] if native_contract
                  else _provenance_from_artifact(artifacts[0].path))
    total = len(definition_ids)
    complete = len(completed) == len(records)
    trusted_hit_rate = len(trusted_union) / total
    qualification_reasons: list[str] = []
    if not complete:
        qualification_reasons.append(
            f"completed_cases={len(completed)}<manifest_cases={len(records)}"
        )
    if len(qualified) < minimum_qualified_runs:
        qualification_reasons.append(
            f"qualified_cases={len(qualified)}<required={minimum_qualified_runs}"
        )
    if trusted_hit_rate < minimum_hit_rate:
        qualification_reasons.append(
            f"trusted_hit_rate={trusted_hit_rate:.8f}<required={minimum_hit_rate:.8f}"
        )

    return {
        "schema_version": REPORT_SCHEMA_VERSION,
        "report_status": "complete_baseline" if complete else "partial_live_baseline",
        "campaign": {
            "campaign_path": str(campaign_path),
            "repo_head": revision,
            "manifest_cases": len(records),
            "completed_cases": len(completed),
            "qualified_cases": len(qualified),
            "manifest_qualified_cases": len(manifest_qualified),
            "rejected_evidence_cases": len(rejected_cases),
            "failed_cases": len(completed) - len(qualified),
        },
        "evidence_rejections": rejected_cases,
        "coverage_contract": {
            "compatibility_signature": artifacts[0].compatibility_signature,
            "definitions_sha256": artifacts[0].definitions_sha256,
            "sampler_sha256": str(provenance.get("sampler_sha256") or ""),
            "verification_env_sha256": str(
                provenance.get("verification_env_sha256") or ""
            ),
            "signal_contract_sha256": str(
                provenance.get("signal_contract_sha256") or ""
            ),
            "qualified_artifact_count": len(artifacts),
        },
        "coverage": {
            "definition_bins": total,
            "raw_union_hit_bins": len(raw_union),
            "raw_union_hit_rate": len(raw_union) / total,
            "trusted_union_hit_bins": len(trusted_union),
            "trusted_union_hit_rate": trusted_hit_rate,
            "quarantined_raw_hit_bins": len(raw_union & quarantined),
            "raw_union_bin_ids": sorted(raw_union),
            "trusted_union_bin_ids": sorted(trusted_union),
            "unhit_or_untrusted_bin_ids": sorted(definition_ids - trusted_union),
        },
        "quarantine": quarantine,
        "coverage_groups": _coverage_group_summary(
            definitions,
            raw_union,
            trusted_union,
            quarantined,
        ),
        "per_qualified_artifact_marginal_gain": _marginal_gain(
            artifacts, quarantined
        ),
        "qualification": {
            "pass": not qualification_reasons,
            "minimum_hit_rate": minimum_hit_rate,
            "minimum_qualified_runs": minimum_qualified_runs,
            "reasons": qualification_reasons,
        },
    }


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Report raw and conservative trusted Frontend functional coverage"
    )
    parser.add_argument("--native-evidence-index", type=Path)
    parser.add_argument("--native-evidence-index-sha256")
    parser.add_argument("--campaign", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--require-complete", action="store_true")
    parser.add_argument("--minimum-hit-rate", type=float, default=0.99)
    parser.add_argument("--minimum-qualified-runs", type=int, default=100)
    parser.add_argument("--require-qualified-union", action="store_true")
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
    mark_coverage_report_scope(report, current_native=native_index is not None)
    _atomic_json(args.output, report)
    campaign = report["campaign"]
    coverage = report["coverage"]
    print(
        "trusted-funcov "
        f"status={report['report_status']} "
        f"completed={campaign['completed_cases']}/{campaign['manifest_cases']} "
        f"qualified={campaign['qualified_cases']} failed={campaign['failed_cases']} "
        f"raw={coverage['raw_union_hit_bins']}/{coverage['definition_bins']} "
        f"trusted={coverage['trusted_union_hit_bins']}/{coverage['definition_bins']} "
        f"quarantined_raw_hits={coverage['quarantined_raw_hit_bins']} "
        f"qualified_union={int(report['qualification']['pass'])} "
        f"output={args.output.resolve()}"
    )
    if args.require_complete and report["report_status"] != "complete_baseline":
        return 1
    if args.require_qualified_union and not report["qualification"]["pass"]:
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
