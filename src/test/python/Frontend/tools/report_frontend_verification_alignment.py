#!/usr/bin/env python3
"""Report the reproducible frontend verification-alignment state.

The report intentionally distinguishes the full testpoint-leaf denominator from
the coverage-bin registry.  It also excludes functional and line-coverage
artifacts unless their runtime provenance matches the selected DUT baseline.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import os
import shutil
import subprocess
import tempfile
from collections import Counter, defaultdict
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Iterable


FRONTEND_ROOT = Path(__file__).resolve().parents[1]
REPO_ROOT = FRONTEND_ROOT.parents[3]


PRIMARY_STATUSES = ("UNMAPPED", "MODELED", "PARTIAL", "HIT", "CLOSED")
OTHER_STATUSES = ("BLOCKED", "N-A", "SV_FUNCOV")
VALID_STATUSES = frozenset(PRIMARY_STATUSES + OTHER_STATUSES)
MAPPED_STATUSES = frozenset(
    set(PRIMARY_STATUSES) - {"UNMAPPED"} | {"BLOCKED", "SV_FUNCOV"}
)
_HIERARCHY_COLUMNS = ("一级测试点", "二级测试点", "三级测试点", "四级测试点", "五级测试点")
_LEAF_FIELDS = ("Condition", "Checkpoint", "Object")
TRACKING_FIELDS = (
    "v3_commit",
    "subject",
    "frontend_impact",
    "affected_testpoint_scope",
    "testpoint_refresh",
    "testcase_refresh",
    "funcov_refresh",
    "evidence",
    "status",
    "notes",
)
PROVENANCE_MATCH_FIELDS = (
    "simulator",
    "build_manifest_sha256",
    "dut_build_sha256",
    "dut_python_extension_sha256",
    "generated_rtl_sha256",
    "signal_contract_sha256",
    "dut_source_sha",
    "implementation_sha",
    "design_baseline_sha",
    "registry_sha256",
    "sampler_sha256",
    "source_sha_override",
    "source_delta_sha256",
    "source_delta_files",
    "source_delta_policy",
)


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _json_sha256(value: Any) -> str:
    payload = json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":"))
    return hashlib.sha256(payload.encode("utf-8")).hexdigest()


def _frontend_pylib_path(simulator: str) -> Path:
    simulator = str(simulator).strip().lower()
    if simulator not in {"verilator", "vcs"}:
        raise ValueError("simulator must be verilator or vcs")
    return REPO_ROOT / "build-frontend" / f"pylib-{simulator}"


def _read_csv(path: Path) -> tuple[list[str], list[dict[str, str]]]:
    with path.open(encoding="utf-8-sig", newline="") as handle:
        reader = csv.DictReader(handle)
        return list(reader.fieldnames or []), list(reader)


def _percent(count: int, denominator: int) -> float:
    return round((100.0 * int(count) / int(denominator)), 2) if denominator else 0.0


def _effective_hierarchy_paths(rows: list[dict[str, str]]) -> list[tuple[str, ...]]:
    context = [""] * len(_HIERARCHY_COLUMNS)
    paths: list[tuple[str, ...]] = []
    for row in rows:
        for index, field in enumerate(_HIERARCHY_COLUMNS):
            value = str(row.get(field) or "").strip()
            if value:
                context[index] = value
                for deeper in range(index + 1, len(context)):
                    context[deeper] = ""
        paths.append(tuple(value for value in context if value))
    return paths


def _is_executable_leaf(
    row: dict[str, str], path: tuple[str, ...], all_paths: list[tuple[str, ...]]
) -> bool:
    if not all(str(row.get(field) or "").strip() for field in _LEAF_FIELDS):
        return False
    if not path:
        return False
    return not any(
        len(candidate) > len(path) and candidate[: len(path)] == path
        for candidate in all_paths
    )


def summarize_testpoints(path: Path) -> dict[str, Any]:
    fields, rows = _read_csv(path)
    required = {*_LEAF_FIELDS, "coverage", "status"}
    missing = sorted(required - set(fields))
    if missing:
        raise ValueError(f"testpoint CSV lacks required columns: {missing}")

    hierarchy_paths = _effective_hierarchy_paths(rows)
    leaves = [
        (line, row, hierarchy_paths[line - 2])
        for line, row in enumerate(rows, start=2)
        if _is_executable_leaf(row, hierarchy_paths[line - 2], hierarchy_paths)
    ]
    status_counts: Counter[str] = Counter()
    mapped_leaf_count = 0
    schema_errors: list[dict[str, Any]] = []
    for line, row, hierarchy_path in leaves:
        leaf = " / ".join(hierarchy_path)
        status = str(row.get("status") or "").strip()
        coverage = str(row.get("coverage") or "").strip()
        if coverage:
            mapped_leaf_count += 1
        if not status:
            status_counts["UNSPECIFIED"] += 1
            schema_errors.append(
                {"line": line, "kind": "missing_status", "leaf": leaf, "coverage": coverage}
            )
            continue
        status_counts[status] += 1
        if status not in VALID_STATUSES:
            schema_errors.append(
                {"line": line, "kind": "invalid_status", "leaf": leaf, "status": status}
            )
        elif status == "UNMAPPED" and coverage:
            schema_errors.append(
                {"line": line, "kind": "unmapped_has_coverage", "leaf": leaf, "coverage": coverage}
            )
        elif status in MAPPED_STATUSES and not coverage:
            schema_errors.append(
                {"line": line, "kind": "modeled_status_without_coverage", "leaf": leaf, "status": status}
            )

    all_leaf_count = len(leaves)
    # N-A rows are explicit hierarchy/migration exclusions, not executable
    # verification obligations. Keep their count visible while using only
    # executable leaves as the biweekly progress denominator.
    denominator = all_leaf_count - int(status_counts["N-A"])
    counts = {status: int(status_counts[status]) for status in PRIMARY_STATUSES + OTHER_STATUSES}
    counts["UNSPECIFIED"] = int(status_counts["UNSPECIFIED"])
    modeled_progress = sum(counts[status] for status in ("MODELED", "PARTIAL", "HIT", "CLOSED"))
    return {
        "path": str(path),
        "all_leaf_count": all_leaf_count,
        "leaf_denominator": denominator,
        "non_executable_leaf_count": int(status_counts["N-A"]),
        "non_executable_leaf_percentage": _percent(int(status_counts["N-A"]), all_leaf_count),
        "status_counts": counts,
        "status_percentages": {status: _percent(count, denominator) for status, count in counts.items()},
        "coverage_mapped_leaf_count": mapped_leaf_count,
        "coverage_mapped_leaf_percentage": _percent(mapped_leaf_count, denominator),
        "modeled_progress_leaf_count": modeled_progress,
        "modeled_progress_leaf_percentage": _percent(modeled_progress, denominator),
        "schema_error_count": len(schema_errors),
        "schema_errors": schema_errors,
    }


def _git(args: Iterable[str]) -> str:
    result = subprocess.run(
        ["git", *args],
        cwd=REPO_ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    return result.stdout.strip()


def _git_is_ancestor(ancestor: str, descendant: str) -> bool | None:
    if not ancestor or ancestor in {"unavailable", "unknown"}:
        return None
    result = subprocess.run(
        ["git", "merge-base", "--is-ancestor", ancestor, descendant],
        cwd=REPO_ROOT,
        capture_output=True,
        text=True,
    )
    if result.returncode in {0, 1}:
        return result.returncode == 0
    return None


def verification_worktree_state() -> dict[str, Any]:
    """Report only changes that prevent a build from naming one git commit.

    An untracked file inside a submodule is intentionally ignored: it is not
    part of the superproject source used to build Frontend and may be local
    user data.  Any tracked or top-level untracked source change still makes a
    commit-only verification baseline invalid.
    """
    output = _git(
        [
            "status",
            "--porcelain=v1",
            "--untracked-files=all",
            "--ignore-submodules=untracked",
        ]
    )
    entries = []
    for line in output.splitlines():
        if not line:
            continue
        path = line[3:].split(" -> ", 1)[-1] if len(line) >= 4 else ""
        if path.startswith("data/runs/") or path.startswith("build-frontend/"):
            continue
        entries.append(line)
    return {
        "status": "clean" if not entries else "dirty",
        "changes": entries,
        "submodule_untracked_ignored": True,
    }


def _design_merges() -> list[dict[str, str]]:
    output = _git(["log", "--first-parent", "--merges", "--format=%H%x1f%P%x1f%ad%x1f%an%x1f%s", "--date=short"])
    merges = []
    for record in filter(None, output.splitlines()):
        commit, parents, date, author, subject = record.split("\x1f", maxsplit=4)
        parent_list = parents.split()
        if "origin/kunminghu-v3" not in subject or len(parent_list) != 2:
            continue
        merges.append(
            {
                "frontend_bt_merge": commit,
                "frontend_parent": parent_list[0],
                "v3_commit": parent_list[1],
                "date": date,
                "author": author,
                "subject": subject,
            }
        )
    return merges


def design_delta() -> dict[str, Any]:
    merges = _design_merges()
    latest = merges[0] if merges else None
    previous = merges[1] if len(merges) > 1 else None
    commits: list[dict[str, str]] = []
    if latest and previous:
        output = _git(
            [
                "log",
                "--format=%H%x1f%ad%x1f%an%x1f%s",
                "--date=short",
                f"{previous['v3_commit']}..{latest['v3_commit']}",
                "--",
                "src/main/scala/xiangshan/frontend",
            ]
        )
        for record in filter(None, output.splitlines()):
            commit, date, author, subject = record.split("\x1f", maxsplit=3)
            commits.append({"v3_commit": commit, "date": date, "author": author, "subject": subject})
    return {"latest_merge": latest, "previous_merge": previous, "frontend_commits": commits}


def load_tracking(path: Path) -> dict[str, Any]:
    if not path.exists():
        return {"path": str(path), "missing_file": True, "rows": {}, "schema_errors": []}
    fields, rows = _read_csv(path)
    missing = sorted(set(TRACKING_FIELDS) - set(fields))
    schema_errors = [f"missing_columns:{','.join(missing)}"] if missing else []
    indexed: dict[str, dict[str, str]] = {}
    for line, row in enumerate(rows, start=2):
        commit = str(row.get("v3_commit") or "").strip()
        if not commit:
            schema_errors.append(f"line_{line}:missing_v3_commit")
        elif commit in indexed:
            schema_errors.append(f"line_{line}:duplicate_v3_commit:{commit}")
        else:
            indexed[commit] = {field: str(row.get(field) or "").strip() for field in fields}
    return {"path": str(path), "missing_file": False, "rows": indexed, "schema_errors": schema_errors}


def expected_provenance(pilot_path: Path, simulator: str) -> dict[str, Any]:
    """Build the static portion of recorder provenance without DUT dependencies."""
    simulator = str(simulator).strip().lower()
    manifest_path = REPO_ROOT / "build-frontend" / f"frontend_build_manifest.{simulator}.json"
    manifest: dict[str, Any] = {}
    reasons: list[str] = []
    if not manifest_path.is_file():
        reasons.append("manifest_missing")
    else:
        try:
            manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
        except json.JSONDecodeError:
            reasons.append("manifest_invalid_json")
    required_manifest_fields = (
        "dut_source_sha",
        "dut_build_sha256",
        "dut_python_extension_sha256",
        "generated_rtl_sha256",
        "signal_contract_sha256",
        "build_config",
        "implementation_sha",
        "design_baseline_sha",
    )
    artifact_hashes = manifest.get("artifacts") if isinstance(manifest.get("artifacts"), dict) else {}
    for field in required_manifest_fields:
        if not str(manifest.get(field) or artifact_hashes.get(field) or "").strip():
            reasons.append(f"manifest_missing:{field}")
    if bool(manifest.get("source_tree_dirty", False)):
        reasons.append("source_tree_dirty")
    worktree = verification_worktree_state()
    if worktree["status"] != "clean":
        reasons.append("verification_worktree_dirty")
    manifest_status = "valid" if not reasons else "invalid"
    sampler_paths = {
        "functional_coverage.py": FRONTEND_ROOT / "env" / "functional_coverage.py",
        "funcov/__init__.py": FRONTEND_ROOT / "env" / "funcov" / "__init__.py",
        "funcov/py/ftq/sampler.py": FRONTEND_ROOT / "env" / "funcov" / "py" / "ftq" / "sampler.py",
        "funcov/py/ifu/sampler.py": FRONTEND_ROOT / "env" / "funcov" / "py" / "ifu" / "sampler.py",
        "funcov/py/icache/__init__.py": FRONTEND_ROOT / "env" / "funcov" / "py" / "icache" / "__init__.py",
        "funcov/py/icache/icache_mainpipe_funcov.py": FRONTEND_ROOT / "env" / "funcov" / "py" / "icache" / "icache_mainpipe_funcov.py",
        "funcov/py/icache/icache_prefetchpipe_funcov.py": FRONTEND_ROOT / "env" / "funcov" / "py" / "icache" / "icache_prefetchpipe_funcov.py",
        "funcov/py/icache/icache_missunit_funcov.py": FRONTEND_ROOT / "env" / "funcov" / "py" / "icache" / "icache_missunit_funcov.py",
        "funcov/py/icache/icache_waylookup_funcov.py": FRONTEND_ROOT / "env" / "funcov" / "py" / "icache" / "icache_waylookup_funcov.py",
        "funcov/py/icache/icache_hitmiss_funcov.py": FRONTEND_ROOT / "env" / "funcov" / "py" / "icache" / "icache_hitmiss_funcov.py",
    }
    missing_sampler = [str(path) for path in sampler_paths.values() if not path.is_file()]
    if missing_sampler:
        reasons.extend(f"sampler_missing:{path}" for path in missing_sampler)
    sampler_sha256 = _json_sha256(
        {label: _sha256(path) for label, path in sampler_paths.items() if path.is_file()}
    )
    return {
        "simulator": simulator,
        "build_manifest_path": str(manifest_path),
        "build_manifest_sha256": _sha256(manifest_path) if manifest_path.is_file() else "unavailable",
        "build_manifest_status": manifest_status,
        "build_manifest_reasons": reasons,
        "verification_worktree": worktree,
        "dut_source_sha": str(manifest.get("dut_source_sha") or "unavailable") if manifest_status == "valid" else "unavailable",
        "implementation_sha": str(manifest.get("implementation_sha") or "unavailable"),
        "design_baseline_sha": str(manifest.get("design_baseline_sha") or "unavailable"),
        "dut_build_sha256": str(manifest.get("dut_build_sha256") or artifact_hashes.get("dut_build_sha256") or "unavailable"),
        "dut_python_extension_sha256": str(manifest.get("dut_python_extension_sha256") or artifact_hashes.get("dut_python_extension_sha256") or "unavailable"),
        "generated_rtl_sha256": str(manifest.get("generated_rtl_sha256") or artifact_hashes.get("generated_rtl_sha256") or "unavailable"),
        "signal_contract_sha256": str(manifest.get("signal_contract_sha256") or artifact_hashes.get("signal_contract_sha256") or "unavailable"),
        "registry_sha256": _sha256(pilot_path),
        "sampler_sha256": sampler_sha256,
    }


def _is_passed_artifact(data: dict[str, Any]) -> bool:
    run = data.get("run") or {}
    checker = run.get("checker") or data.get("checker") or {}
    outcome = str(run.get("pytest_outcome") or data.get("outcome") or "").strip().lower()
    checker_status = str(checker.get("status") or "").strip().lower()
    return outcome in {"pass", "passed", "ok", "success", "successful"} and checker_status in {
        "pass",
        "passed",
        "ok",
        "success",
        "successful",
    }


def audit_artifacts(root: Path, expected: dict[str, Any]) -> dict[str, Any]:
    reason_counts: Counter[str] = Counter()
    eligible: list[dict[str, Any]] = []
    line_coverage_paths: set[Path] = set()
    scanned = 0
    for path in root.rglob("*.funcov.json") if root.exists() else ():
        scanned += 1
        try:
            data = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError):
            reason_counts["invalid_json"] += 1
            continue
        provenance = data.get("provenance") or {}
        reasons = []
        if str(expected.get("build_manifest_status") or "").lower() != "valid":
            reasons.append("current_manifest_invalid")
        if str(provenance.get("build_manifest_status") or "").lower() != "valid":
            reasons.append("artifact_manifest_invalid")
        for field in PROVENANCE_MATCH_FIELDS:
            if str(provenance.get(field) or "") != str(expected.get(field) or ""):
                reasons.append(f"provenance_mismatch:{field}")
        if not _is_passed_artifact(data):
            reasons.append("run_or_checker_not_passed")
        if reasons:
            reason_counts.update(set(reasons))
            continue
        eligible.append({"path": str(path), "data": data})
        line_path = str(data.get("line_coverage_path") or "").strip()
        if line_path:
            candidate = Path(line_path)
            if candidate.is_file():
                line_coverage_paths.add(candidate.resolve())

    hit_keys: set[tuple[str, str, str]] = set()
    for item in eligible:
        for hit in item["data"].get("hits") or []:
            if isinstance(hit, dict):
                key = (str(hit.get("group") or ""), str(hit.get("coverpoint") or ""), str(hit.get("bin") or ""))
                if all(key):
                    hit_keys.add(key)
    return {
        "scanned_artifact_count": scanned,
        "eligible_artifact_count": len(eligible),
        "excluded_artifact_count": scanned - len(eligible),
        "exclusion_reasons": dict(sorted(reason_counts.items())),
        "eligible_functional_hit_key_count": len(hit_keys),
        "line_coverage_paths": sorted(str(path) for path in line_coverage_paths),
    }


def summarize_line_coverage(paths: list[str]) -> dict[str, Any]:
    if not paths:
        return {"status": "unavailable", "reason": "no_baseline_compatible_line_coverage"}
    executable = shutil.which("verilator_coverage")
    if executable is None:
        return {"status": "unavailable", "reason": "verilator_coverage_not_found"}
    with tempfile.TemporaryDirectory(prefix="frontend-line-coverage-") as temp_dir:
        info_path = Path(temp_dir) / "coverage.info"
        result = subprocess.run(
            [executable, "--write-info", str(info_path), *paths],
            capture_output=True,
            text=True,
        )
        if result.returncode != 0 or not info_path.is_file():
            return {
                "status": "unavailable",
                "reason": "verilator_coverage_failed",
                "stderr": result.stderr.strip(),
            }
        total = 0
        hit = 0
        for line in info_path.read_text(encoding="utf-8", errors="replace").splitlines():
            if not line.startswith("DA:"):
                continue
            _, count_text = line[3:].split(",", maxsplit=1)
            total += 1
            if int(count_text) > 0:
                hit += 1
    return {
        "status": "available",
        "input_file_count": len(paths),
        "line_total": total,
        "line_hit": hit,
        "line_coverage_percent": _percent(hit, total),
    }


def build_report(args: argparse.Namespace) -> dict[str, Any]:
    testpoints = Path(args.testpoints).resolve()
    pilot = Path(args.pilot).resolve()
    tracking_path = Path(args.tracking).resolve()
    artifacts_root = Path(args.artifacts_root).resolve()
    leaves = summarize_testpoints(testpoints)
    expected = expected_provenance(pilot, args.simulator)
    delta = design_delta()
    tracking = load_tracking(tracking_path)
    tracked = tracking["rows"]
    delta_rows = []
    for commit in delta["frontend_commits"]:
        row = dict(commit)
        row["tracking"] = tracked.get(commit["v3_commit"])
        row["tracking_present"] = commit["v3_commit"] in tracked
        delta_rows.append(row)

    artifact_audit = audit_artifacts(artifacts_root, expected)
    line_coverage = summarize_line_coverage(artifact_audit["line_coverage_paths"])
    implementation_sha = str(expected.get("implementation_sha") or "")
    return {
        "report_schema_version": 1,
        "generated_at_utc": datetime.now(timezone.utc).isoformat(),
        "baseline": {
            "frontend_bt_commit": _git(["rev-parse", "HEAD"]),
            "frontend_bt_branch": _git(["branch", "--show-current"]),
            "simulator": str(expected.get("simulator") or args.simulator),
            "build_manifest_path": str(expected.get("build_manifest_path") or ""),
            "build_manifest_status": str(expected.get("build_manifest_status") or ""),
            "build_manifest_reasons": list(expected.get("build_manifest_reasons") or []),
            "verification_worktree": dict(expected.get("verification_worktree") or {}),
            "dut_source_sha": str(expected.get("dut_source_sha") or ""),
            "implementation_sha": implementation_sha,
            "design_baseline_sha": str(expected.get("design_baseline_sha") or ""),
            "implementation_reachable_from_head": _git_is_ancestor(implementation_sha, _git(["rev-parse", "HEAD"])),
            "pylib_path": str(_frontend_pylib_path(args.simulator)),
        },
        "testpoints": leaves,
        "design_delta": {
            "latest_merge": delta["latest_merge"],
            "previous_merge": delta["previous_merge"],
            "frontend_commits": delta_rows,
            "tracking_schema_errors": tracking["schema_errors"],
        },
        "functional_coverage_artifacts": artifact_audit,
        "code_coverage": line_coverage,
    }


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--testpoints",
        default=str(FRONTEND_ROOT / "docs" / "02_testpoint" / "Frontend_testpoint_0525_coverage_backannotated.csv"),
    )
    parser.add_argument(
        "--pilot",
        default=str(FRONTEND_ROOT / "docs" / "03_funcov_model" / "frontend_bt_functional_coverage_pilot.csv"),
    )
    parser.add_argument(
        "--tracking",
        default=str(FRONTEND_ROOT / "docs" / "03_funcov_model" / "frontend_design_change_tracking.csv"),
    )
    parser.add_argument("--artifacts-root", default=str(FRONTEND_ROOT / "data" / "runs"))
    parser.add_argument("--simulator", default=os.getenv("TB_FRONTEND_SIM", "verilator"))
    parser.add_argument("--output", help="Write the JSON report to this path instead of stdout.")
    return parser.parse_args()


def main() -> int:
    args = _parse_args()
    os.environ["TB_FRONTEND_SIM"] = str(args.simulator)
    report = build_report(args)
    payload = json.dumps(report, ensure_ascii=False, indent=2, sort_keys=True) + "\n"
    if args.output:
        output = Path(args.output)
        output.parent.mkdir(parents=True, exist_ok=True)
        output.write_text(payload, encoding="utf-8")
    else:
        print(payload, end="")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
