#!/usr/bin/env python3
"""Back-annotate functional-coverage evidence into the testpoint CSV.

The tool deliberately treats model/FakeDut artifacts as modeling evidence only.
Only an artifact with real DUT runtime statistics can move a leaf to HIT;
CLOSED remains an explicit human-acceptance state.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Iterable


_IMPORT_ROOT = Path(__file__).resolve().parents[1]
if str(_IMPORT_ROOT) not in sys.path:
    sys.path.insert(0, str(_IMPORT_ROOT))

from env.runtime.artifact_provenance import load_frontend_build_manifest  # noqa: E402
from env.funcov.recorder import (  # noqa: E402
    current_funcov_sampler_sha256,
    current_verification_environment_sha256,
    funcov_sampler_paths,
    verification_environment_paths,
)


STATUSES = {
    "UNMAPPED",
    "MODELED",
    "PARTIAL",
    "HIT",
    "CLOSED",
    "BLOCKED",
    "N-A",
    "SV_FUNCOV",
}
BIN_REFERENCE_RE = re.compile(
    r"^covergroup ([^,;]+), coverpoint ([^,;]+), bins ([^ (;]+) \((BIN-\d+)\)$"
)
POINT_REFERENCE_RE = re.compile(r"^covergroup ([^,;]+), coverpoint ([^,;()]+)$")
BIN_ID_RE = re.compile(r"^BIN-\d+$")

_PASS_OUTCOMES = {"pass", "passed", "ok", "success", "successful"}
_CHECKER_NOT_APPLICABLE = {
    "",
    "unknown",
    "unavailable",
    "disabled",
    "not_applicable",
    "not-applicable",
    "not_enabled",
    "not-enabled",
}
_REQUIRED_PROVENANCE = (
    "dut_source_sha",
    "dut_build_sha256",
    "dut_python_extension_sha256",
    "generated_rtl_sha256",
    "registry_sha256",
    "definitions_sha256",
    "sampler_sha256",
    "verification_env_sha256",
    "signal_contract_sha256",
    "build_manifest_sha256",
    "compatibility_signature",
    "build_config",
    "toolchain",
)
_SHA256_PROVENANCE = {
    "dut_build_sha256",
    "dut_python_extension_sha256",
    "generated_rtl_sha256",
    "registry_sha256",
    "definitions_sha256",
    "sampler_sha256",
    "verification_env_sha256",
    "signal_contract_sha256",
    "build_manifest_sha256",
    "compatibility_signature",
}
_COMPATIBILITY_FIELDS = (
    "simulator",
    "dut_source_sha",
    "implementation_sha",
    "design_baseline_sha",
    "source_sha_override",
    "source_delta_sha256",
    "source_delta_files",
    "source_delta_policy",
    "dut_build_sha256",
    "dut_python_extension_sha256",
    "generated_rtl_sha256",
    "registry_sha256",
    "sampler_sha256",
    "verification_env_sha256",
    "signal_contract_sha256",
    "build_config",
    "toolchain",
)
_REQUIRED_SEED_FIELDS = ("test", "backend", "icache", "ptw")
_RUNTIME_MANIFEST_FIELDS = (
    "simulator",
    "build_manifest_sha256",
    "dut_build_sha256",
    "dut_python_extension_sha256",
    "generated_rtl_sha256",
    "signal_contract_sha256",
    "dut_source_sha",
    "design_baseline_sha",
    "implementation_sha",
    "source_sha_override",
    "source_delta_sha256",
    "source_delta_files",
    "source_delta_policy",
    "build_config",
)
_RUNTIME_PROVENANCE_FIELDS = (
    "simulator",
    "implementation_sha",
    "design_baseline_sha",
    "source_sha_override",
    "source_delta_sha256",
    "source_delta_files",
    "source_delta_policy",
    "build_manifest_path",
)
_FRONTEND_ROOT = Path(__file__).resolve().parents[1]
_CANONICAL_REGISTRY = (
    _FRONTEND_ROOT
    / "docs"
    / "03_funcov_model"
    / "frontend_bt_functional_coverage_pilot.csv"
)


@dataclass(frozen=True)
class PilotBin:
    bin_id: str
    group: str
    coverpoint: str
    bin_name: str
    testcase: str


@dataclass(frozen=True)
class CoverageReference:
    kind: str
    group: str
    coverpoint: str
    bin_name: str | None = None
    bin_id: str | None = None


def _read_csv(path: Path) -> tuple[list[str], list[dict[str, str]]]:
    with path.open(encoding="utf-8-sig", newline="") as f:
        reader = csv.DictReader(f)
        return list(reader.fieldnames or []), list(reader)


def _write_csv(path: Path, fields: list[str], rows: Iterable[dict[str, str]]) -> None:
    with path.open("w", encoding="utf-8-sig", newline="") as f:
        writer = csv.DictWriter(
            f, fieldnames=fields, extrasaction="ignore", lineterminator="\n"
        )
        writer.writeheader()
        writer.writerows(rows)


def validate_pilot_schema(path: Path) -> dict[str, int]:
    """Validate global pilot identifiers independently of a module batch."""
    fields, rows = _read_csv(path)
    required = {"Bin_ID", "Coverage_Group", "Coverpoint", "Bin_Name", "建议试点用例"}
    missing = required - set(fields)
    if missing:
        raise ValueError(f"pilot missing columns: {sorted(missing)}")

    bin_ids: dict[str, int] = {}
    mapping_keys: dict[tuple[str, str, str], int] = {}
    legacy_ids: dict[str, int] = {}
    for line, row in enumerate(rows, start=2):
        bin_id = str(row["Bin_ID"] or "").strip()
        group = str(row["Coverage_Group"] or "").strip()
        point = str(row["Coverpoint"] or "").strip()
        bin_name = str(row["Bin_Name"] or "").strip()
        if not BIN_ID_RE.fullmatch(bin_id):
            raise ValueError(f"line {line}: invalid Bin_ID {bin_id!r}")
        if not group or not bin_name:
            raise ValueError(f"line {line}: incomplete coverage mapping for {bin_id}")
        if bin_id in bin_ids:
            raise ValueError(
                f"line {line}: duplicate Bin_ID {bin_id}; first defined at line {bin_ids[bin_id]}"
            )
        bin_ids[bin_id] = line

        mapping_key = (group, point, bin_name)
        if mapping_key in mapping_keys:
            raise ValueError(
                f"line {line}: duplicate coverage mapping {mapping_key}; "
                f"first defined at line {mapping_keys[mapping_key]}"
            )
        mapping_keys[mapping_key] = line

        legacy_id = str(row.get("Legacy_Bin_ID") or "").strip()
        if legacy_id:
            if not BIN_ID_RE.fullmatch(legacy_id):
                raise ValueError(f"line {line}: invalid Legacy_Bin_ID {legacy_id!r}")
            if legacy_id == bin_id:
                raise ValueError(f"line {line}: Legacy_Bin_ID must differ from Bin_ID")
            if legacy_id in legacy_ids:
                raise ValueError(
                    f"line {line}: duplicate Legacy_Bin_ID {legacy_id}; "
                    f"first defined at line {legacy_ids[legacy_id]}"
                )
            legacy_ids[legacy_id] = line

    return {
        "rows": len(rows),
        "bin_ids": len(bin_ids),
        "mapping_keys": len(mapping_keys),
        "legacy_ids": len(legacy_ids),
    }


def load_pilot(path: Path, *, bin_prefix: str | None = None) -> dict[str, PilotBin]:
    fields, rows = _read_csv(path)
    required = {"Bin_ID", "Coverage_Group", "Coverpoint", "Bin_Name", "建议试点用例"}
    missing = required - set(fields)
    if missing:
        raise ValueError(f"pilot missing columns: {sorted(missing)}")
    result = {}
    for row in rows:
        bin_id = row["Bin_ID"].strip()
        if bin_prefix is not None and not bin_id.startswith(bin_prefix):
            continue
        item = PilotBin(
            bin_id=bin_id,
            group=row["Coverage_Group"].strip(),
            coverpoint=row["Coverpoint"].strip(),
            bin_name=row["Bin_Name"].strip(),
            testcase=row["建议试点用例"].strip(),
        )
        if not item.coverpoint and bin_prefix is None:
            continue
        if not item.group or not item.coverpoint or not item.bin_name:
            raise ValueError(f"incomplete pilot row: {bin_id}")
        if bin_id in result:
            raise ValueError(f"duplicate pilot bin: {bin_id}")
        result[bin_id] = item
    return result


def parse_reference(value: str) -> CoverageReference | None:
    text = str(value or "").strip()
    match = BIN_REFERENCE_RE.fullmatch(text)
    if match:
        group, point, bin_name, bin_id = match.groups()
        return CoverageReference("bin", group, point, bin_name, bin_id)
    match = POINT_REFERENCE_RE.fullmatch(text)
    if match:
        group, point = match.groups()
        return CoverageReference("point", group, point)
    return None


def _pilot_bins_for_reference(
    ref: CoverageReference,
    pilot: dict[str, PilotBin],
) -> list[PilotBin]:
    if ref.kind == "bin":
        item = pilot.get(str(ref.bin_id))
        return [] if item is None else [item]
    return [
        item
        for item in pilot.values()
        if (item.group, item.coverpoint) == (ref.group, ref.coverpoint)
    ]


def validate_mapping(
    testpoint_path: Path,
    pilot: dict[str, PilotBin],
    *,
    bin_prefix: str | None = None,
    structure_errors: list[tuple[int, str]] | None = None,
) -> dict[str, int]:
    fields, rows = _read_csv(testpoint_path)
    required = {"coverage", "status", "testcase", "evidence"}
    missing = required - set(fields)
    if missing:
        raise ValueError(f"testpoint missing columns: {sorted(missing)}")

    mapped: dict[str, int] = {}
    point_owners: dict[tuple[str, str], int] = {}
    bin_owned_points: dict[tuple[str, str], int] = {}
    for line, row in enumerate(rows, start=2):
        status = row["status"].strip()
        if status and status not in STATUSES:
            raise ValueError(f"line {line}: invalid status {status!r}")
        coverage = row["coverage"].strip()
        if not coverage:
            continue
        ref = parse_reference(coverage)
        if ref is None:
            if bin_prefix is not None and bin_prefix not in coverage:
                continue
            message = (
                f"line {line}: leaf must bind exactly one covergroup/coverpoint or "
                "one covergroup/coverpoint/bin"
            )
            if structure_errors is not None:
                structure_errors.append((line, message))
                continue
            raise ValueError(message)
        point_key = (ref.group, ref.coverpoint)
        if ref.kind == "point":
            items = _pilot_bins_for_reference(ref, pilot)
            if not items:
                if bin_prefix is not None:
                    continue
                raise ValueError(f"line {line}: unknown pilot coverpoint {point_key}")
            if point_key in point_owners:
                raise ValueError(
                    f"line {line}: coverpoint {point_key} already owned by line "
                    f"{point_owners[point_key]}"
                )
            if point_key in bin_owned_points:
                raise ValueError(
                    f"line {line}: coverpoint {point_key} mixes point-level ownership "
                    f"with bin-level ownership at line {bin_owned_points[point_key]}"
                )
            point_owners[point_key] = line
            for item in items:
                mapped[item.bin_id] = line
            continue

        bin_id = str(ref.bin_id)
        if bin_prefix is not None and not bin_id.startswith(bin_prefix):
            continue
        expected = pilot.get(bin_id)
        if expected is None:
            raise ValueError(f"line {line}: unknown pilot bin {bin_id}")
        if (ref.group, ref.coverpoint, ref.bin_name) != (
            expected.group,
            expected.coverpoint,
            expected.bin_name,
        ):
            raise ValueError(f"line {line}: pilot mismatch for {bin_id}")
        if point_key in point_owners:
            raise ValueError(
                f"line {line}: coverpoint {point_key} mixes bin-level ownership "
                f"with point-level ownership at line {point_owners[point_key]}"
            )
        if bin_id in mapped:
            raise ValueError(
                f"line {line}: {bin_id} already owned by line {mapped[bin_id]}"
            )
        mapped[bin_id] = line
        bin_owned_points.setdefault(point_key, line)

    missing_bins = sorted(set(pilot) - set(mapped))
    if missing_bins:
        raise ValueError(f"pilot bins not back-annotated: {missing_bins}")
    return mapped


def _as_mapping(value: Any) -> dict:
    return value if isinstance(value, dict) else {}


def _collection_count(value: Any) -> int:
    if value is None or value is False or value == "":
        return 0
    if isinstance(value, (str, bytes)):
        return 1
    try:
        return len(value)
    except TypeError:
        return 1


def _file_sha256(path: Path) -> str | None:
    try:
        digest = hashlib.sha256()
        with path.open("rb") as handle:
            for chunk in iter(lambda: handle.read(1024 * 1024), b""):
                digest.update(chunk)
    except OSError:
        return None
    return digest.hexdigest()


def _json_sha256(value: Any) -> str:
    return hashlib.sha256(
        json.dumps(
            value,
            ensure_ascii=False,
            sort_keys=True,
            separators=(",", ":"),
        ).encode("utf-8")
    ).hexdigest()


def _current_sampler_sha256() -> str | None:
    if any(not path.is_file() for path in funcov_sampler_paths().values()):
        return None
    return current_funcov_sampler_sha256()


def _current_verification_env_sha256() -> str | None:
    paths = verification_environment_paths()
    if not paths or any(not path.is_file() for path in paths.values()):
        return None
    return current_verification_environment_sha256()


def _manifest_value_matches(field: str, recorded: Any, runtime: Any) -> bool:
    if field == "source_sha_override":
        return (
            isinstance(recorded, bool)
            and isinstance(runtime, bool)
            and recorded is runtime
        )
    if field == "source_delta_files":
        return (
            isinstance(recorded, list)
            and isinstance(runtime, list)
            and recorded == runtime
        )
    recorded_text = str(recorded or "").strip()
    runtime_text = str(runtime or "").strip()
    return (
        recorded_text == runtime_text
        if field == "build_config"
        else recorded_text.lower() == runtime_text.lower()
    )


def _compatibility_value_present(field: str, value: Any) -> bool:
    if field == "source_sha_override":
        return isinstance(value, bool)
    if field == "source_delta_files":
        return isinstance(value, list)
    if value is None:
        return False
    return str(value).strip().lower() not in {"", "unavailable", "unknown"}


def _runtime_manifest_gate(provenance: dict) -> list[str]:
    """Reload and bind evidence to the exact DUT build manifest it records."""
    manifest_text = str(provenance.get("build_manifest_path") or "").strip()
    if not manifest_text:
        return []
    manifest_path = Path(manifest_text)
    if not manifest_path.is_absolute():
        return ["invalid_provenance:build_manifest_path"]
    simulator = str(provenance.get("simulator") or "").strip().lower()
    if simulator not in {"verilator", "vcs"}:
        return ["invalid_provenance:simulator"]
    runtime = load_frontend_build_manifest(
        manifest_path.parent,
        manifest_path,
        simulator=simulator,
    )
    reasons: list[str] = []
    status = str(runtime.get("build_manifest_status") or "").strip().lower()
    if status != "valid":
        reasons.append(f"build_manifest_runtime:{status or 'missing'}")
    runtime_reasons = runtime.get("build_manifest_reasons")
    if not isinstance(runtime_reasons, list):
        reasons.append("build_manifest_runtime:invalid_reasons")
    else:
        reasons.extend(f"build_manifest_runtime:{reason}" for reason in runtime_reasons)
    for field in _RUNTIME_MANIFEST_FIELDS:
        if not _manifest_value_matches(
            field, provenance.get(field), runtime.get(field)
        ):
            reasons.append(f"build_manifest_runtime_mismatch:{field}")
    return reasons


def artifact_kind(raw: Any) -> str:
    if not isinstance(raw, dict):
        return "invalid"
    if raw.get("_artifact_errors"):
        return "invalid"
    stats = _as_mapping(raw.get("stats"))
    monitor = _as_mapping(stats.get("monitor"))
    cycles = monitor.get("cycles_total", 0)
    try:
        return "dut" if int(cycles) > 0 else "model"
    except (TypeError, ValueError):
        return "model"


def _as_int(value):
    try:
        return int(value)
    except (TypeError, ValueError):
        return None


def _first_value(*values):
    for value in values:
        if value is not None and str(value).strip() != "":
            return value
    return None


def _absolute_path_within(path_text: str, root_text: str) -> bool:
    path = Path(path_text)
    root = Path(root_text)
    if not path.is_absolute() or not root.is_absolute():
        return False
    try:
        path.resolve(strict=False).relative_to(root.resolve(strict=False))
    except (OSError, ValueError):
        return False
    return True


def _normalized_outcome(raw: dict) -> tuple[str | None, int | None]:
    run = _as_mapping(raw.get("run"))
    outcome = _as_mapping(raw.get("outcome"))
    pytest = _as_mapping(raw.get("pytest"))
    status = _first_value(
        run.get("pytest_outcome"),
        run.get("outcome"),
        run.get("status"),
        outcome.get("pytest_outcome") if isinstance(outcome, dict) else None,
        outcome.get("status") if isinstance(outcome, dict) else None,
        pytest.get("outcome") if isinstance(pytest, dict) else None,
        raw.get("pytest_outcome"),
    )
    status = None if status is None else str(status).strip().lower()
    exit_code = _as_int(
        _first_value(
            run.get("exit_code"),
            outcome.get("exit_code") if isinstance(outcome, dict) else None,
            pytest.get("exit_code") if isinstance(pytest, dict) else None,
            raw.get("exit_code"),
        )
    )
    return status, exit_code


def evaluate_artifact(raw: Any) -> dict:
    """Return the evidence gate decision for one funcov JSON artifact.

    A positive functional bin is not enough to establish a DUT hit.  The run
    must carry a current schema/provenance signature, a passing pytest outcome,
    and clean results from checks that are enabled for the scenario.  Missing
    or not-applicable checker metadata does not block a coverage ``HIT``;
    an explicit checker failure still does.
    """
    if not isinstance(raw, dict):
        return {
            "kind": "invalid",
            "eligible": False,
            "reasons": ["artifact_root_not_object"],
        }
    artifact_errors = raw.get("_artifact_errors")
    if artifact_errors:
        return {
            "kind": "invalid",
            "eligible": False,
            "reasons": [str(reason) for reason in artifact_errors],
        }

    stats = _as_mapping(raw.get("stats"))
    monitor = _as_mapping(stats.get("monitor"))
    cycles = _as_int(monitor.get("cycles_total")) or 0
    if cycles <= 0:
        return {"kind": "model", "eligible": False, "reasons": ["no_dut_cycles"]}

    reasons: list[str] = []
    if _as_int(raw.get("artifact_schema_version")) != 2:
        reasons.append("legacy_or_missing_schema")

    coverage_targets = raw.get("coverage_targets")
    if not isinstance(coverage_targets, dict):
        reasons.append("missing_coverage_targets")
    else:
        target_bin_ids = coverage_targets.get("bin_ids")
        if not isinstance(target_bin_ids, list) or not target_bin_ids:
            reasons.append("missing_coverage_targets:bin_ids")
        else:
            invalid_target_bin_ids = sorted(
                {
                    str(bin_id).strip()
                    for bin_id in target_bin_ids
                    if BIN_ID_RE.fullmatch(str(bin_id).strip()) is None
                }
            )
            if invalid_target_bin_ids:
                reasons.append(
                    "invalid_coverage_targets:bin_ids="
                    + ",".join(invalid_target_bin_ids)
                )

    provenance = _as_mapping(raw.get("provenance"))
    # A runtime manifest contract is active only when the artifact explicitly
    # names a build manifest. Unit/model artifacts may carry source metadata
    # without having a compiled-DUT manifest to validate.
    runtime_contract = bool(
        str(provenance.get("build_manifest_path") or "").strip()
        or (
            "build_manifest_status" in provenance
            and "build_manifest_reasons" in provenance
        )
    )
    required_provenance = (
        (*_REQUIRED_PROVENANCE, *_RUNTIME_PROVENANCE_FIELDS)
        if runtime_contract
        else _REQUIRED_PROVENANCE
    )
    for key in required_provenance:
        value = provenance.get(key)
        if value is None or str(value).strip() in {"", "unavailable", "unknown"}:
            reasons.append(f"missing_provenance:{key}")
        elif (
            key in _SHA256_PROVENANCE
            and re.fullmatch(r"[0-9a-fA-F]{64}", str(value).strip()) is None
        ):
            reasons.append(f"invalid_provenance:{key}")
    source_sha = str(provenance.get("dut_source_sha") or "").strip()
    if runtime_contract and str(
        provenance.get("simulator") or ""
    ).strip().lower() not in {"verilator", "vcs"}:
        reasons.append("invalid_provenance:simulator")
    if (
        source_sha not in {"", "unavailable", "unknown"}
        and re.fullmatch(r"(?:[0-9a-fA-F]{40}|[0-9a-fA-F]{64})", source_sha) is None
    ):
        reasons.append("invalid_provenance:dut_source_sha")
    for key in (
        ("implementation_sha", "design_baseline_sha") if runtime_contract else ()
    ):
        value = str(provenance.get(key) or "").strip()
        if (
            value not in {"", "unavailable", "unknown"}
            and re.fullmatch(r"(?:[0-9a-fA-F]{40}|[0-9a-fA-F]{64})", value) is None
        ):
            reasons.append(f"invalid_provenance:{key}")
    override = provenance.get("source_sha_override")
    if runtime_contract and not isinstance(override, bool):
        reasons.append("invalid_provenance:source_sha_override")
    policy = str(provenance.get("source_delta_policy") or "").strip()
    if runtime_contract and policy not in {"none", "observability_only"}:
        reasons.append("invalid_provenance:source_delta_policy")
    delta_files = provenance.get("source_delta_files")
    if runtime_contract and (
        not isinstance(delta_files, list)
        or any(not isinstance(item, str) for item in delta_files)
    ):
        reasons.append("invalid_provenance:source_delta_files")
    elif runtime_contract and not override and (policy != "none" or delta_files):
        reasons.append("source_delta_without_override")
    elif runtime_contract and override and policy != "observability_only":
        reasons.append("source_delta_policy_not_allowlisted")
    if all(
        _compatibility_value_present(field, provenance.get(field))
        for field in _COMPATIBILITY_FIELDS
    ):
        compatibility_payload = {
            field: provenance[field] for field in _COMPATIBILITY_FIELDS
        }
        expected_signature = hashlib.sha256(
            json.dumps(
                compatibility_payload,
                ensure_ascii=False,
                sort_keys=True,
                separators=(",", ":"),
            ).encode("utf-8")
        ).hexdigest()
        if (
            str(provenance.get("compatibility_signature") or "").strip().lower()
            != expected_signature
        ):
            reasons.append("compatibility_signature_mismatch")

    source_csv = str(raw.get("source_csv") or "").strip()
    if not source_csv:
        reasons.append("missing_source_csv")
    current_registry_sha256 = _file_sha256(_CANONICAL_REGISTRY)
    if current_registry_sha256 is None:
        reasons.append("current_registry_unavailable")
    elif (
        str(provenance.get("registry_sha256") or "").strip().lower()
        != current_registry_sha256
    ):
        reasons.append("registry_version_mismatch")

    current_sampler_sha256 = _current_sampler_sha256()
    if current_sampler_sha256 is None:
        reasons.append("current_sampler_unavailable")
    elif (
        str(provenance.get("sampler_sha256") or "").strip().lower()
        != current_sampler_sha256
    ):
        reasons.append("sampler_version_mismatch")

    current_verification_env_sha256 = _current_verification_env_sha256()
    if current_verification_env_sha256 is None:
        reasons.append("current_verification_env_unavailable")
    elif (
        str(provenance.get("verification_env_sha256") or "").strip().lower()
        != current_verification_env_sha256
    ):
        reasons.append("verification_env_version_mismatch")

    definitions = raw.get("definitions")
    if not isinstance(definitions, list):
        reasons.append("missing_or_invalid_definitions")
    elif str(
        provenance.get("definitions_sha256") or ""
    ).strip().lower() != _json_sha256(definitions):
        reasons.append("definitions_sha256_mismatch")
    manifest_status = str(provenance.get("build_manifest_status") or "").strip().lower()
    if manifest_status != "valid":
        reasons.append(f"build_manifest:{manifest_status or 'missing'}")
    manifest_reasons = provenance.get("build_manifest_reasons")
    if runtime_contract and (
        not isinstance(manifest_reasons, list) or manifest_reasons
    ):
        reasons.append("build_manifest_reasons_present")
    if runtime_contract:
        reasons.extend(_runtime_manifest_gate(provenance))

    run = _as_mapping(raw.get("run"))
    run_id = str(run.get("run_id") or "").strip()
    if not run_id:
        reasons.append("missing_run_id")
    for field in (
        "testcase_nodeid",
        "testcase_path",
        "testcase_sha256",
        "run_command",
        "artifact_root",
        "case_log_path",
        "funcov_path",
    ):
        value = str(run.get(field) or "").strip()
        if value in {"", "unknown", "unavailable"}:
            reasons.append(f"missing_run_metadata:{field}")
    testcase_sha256 = str(run.get("testcase_sha256") or "").strip()
    if (
        testcase_sha256 not in {"", "unknown", "unavailable"}
        and re.fullmatch(r"[0-9a-fA-F]{64}", testcase_sha256) is None
    ):
        reasons.append("invalid_run_metadata:testcase_sha256")
    if _as_int(run.get("seed")) is None:
        reasons.append("missing_run_metadata:seed")
    seeds = _as_mapping(run.get("seeds"))
    for field in _REQUIRED_SEED_FIELDS:
        if _as_int(seeds.get(field)) is None:
            reasons.append(f"missing_run_seed:{field}")

    if str(raw.get("testcase_name") or "").strip() == "test_bin_trace" or run.get(
        "bin_path"
    ):
        for field in ("bin_path", "bin_sha256", "trace_path", "trace_sha256"):
            value = str(run.get(field) or "").strip()
            if value in {"", "unknown", "unavailable"}:
                reasons.append(f"missing_run_metadata:{field}")
        for field in ("bin_sha256", "trace_sha256"):
            value = str(run.get(field) or "").strip()
            if (
                value not in {"", "unknown", "unavailable"}
                and re.fullmatch(r"[0-9a-fA-F]{64}", value) is None
            ):
                reasons.append(f"invalid_run_metadata:{field}")

    input_identities = [("testcase_path", "testcase_sha256")]
    if str(raw.get("testcase_name") or "").strip() == "test_bin_trace" or run.get(
        "bin_path"
    ):
        input_identities.extend(
            [("bin_path", "bin_sha256"), ("trace_path", "trace_sha256")]
        )
    asm_path = str(run.get("asm_path") or "").strip()
    if asm_path not in {"", "unknown", "unavailable"}:
        asm_sha256 = str(run.get("asm_sha256") or "").strip()
        if asm_sha256 in {"", "unknown", "unavailable"}:
            reasons.append("missing_run_metadata:asm_sha256")
        elif re.fullmatch(r"[0-9a-fA-F]{64}", asm_sha256) is None:
            reasons.append("invalid_run_metadata:asm_sha256")
        input_identities.append(("asm_path", "asm_sha256"))

    for path_field, hash_field in input_identities:
        path_text = str(run.get(path_field) or "").strip()
        recorded_sha256 = str(run.get(hash_field) or "").strip().lower()
        if path_text in {"", "unknown", "unavailable"}:
            continue
        path = Path(path_text)
        if not path.is_absolute():
            reasons.append(f"invalid_run_metadata:{path_field}")
            continue
        if not path.is_file():
            reasons.append(f"missing_input_file:{path_field}")
            continue
        if re.fullmatch(r"[0-9a-f]{64}", recorded_sha256) is None:
            continue
        actual_sha256 = _file_sha256(path)
        if actual_sha256 != recorded_sha256:
            reasons.append(f"input_sha256_mismatch:{path_field}")

    status, exit_code = _normalized_outcome(raw)
    if status not in _PASS_OUTCOMES:
        reasons.append(f"pytest_outcome:{status or 'missing'}")
    if exit_code != 0:
        reasons.append(f"exit_code:{'missing' if exit_code is None else exit_code}")

    errors = raw.get("errors") or []
    error_count = _collection_count(errors)
    if error_count:
        reasons.append(f"funcov_errors:{error_count}")
    monitor_error_count = _as_int(monitor.get("error_count")) or 0
    if monitor_error_count:
        reasons.append(f"monitor_errors:{monitor_error_count}")

    checker = _as_mapping(raw.get("checker")) or _as_mapping(run.get("checker"))
    checker_status = str(checker.get("status", "")).strip().lower()
    checker_error_count = _as_int(checker.get("error_count")) or 0
    checker_errors = checker.get("errors")
    if (
        checker_status not in _PASS_OUTCOMES
        and checker_status not in _CHECKER_NOT_APPLICABLE
    ):
        reasons.append(f"checker_status:{checker_status or 'missing'}")
    if checker_error_count or checker_errors:
        count = checker_error_count or _collection_count(checker_errors)
        reasons.append(f"checker_errors:{count}")

    artifact_root = str(run.get("artifact_root") or "").strip()
    if artifact_root:
        root_path = Path(artifact_root)
        if not root_path.is_absolute():
            reasons.append("invalid_run_metadata:artifact_root")
        elif not root_path.is_dir():
            reasons.append("missing_artifact_root")
    artifact_paths = {
        "waveform_path": str(raw.get("waveform_path") or "").strip(),
        "line_coverage_path": str(raw.get("line_coverage_path") or "").strip(),
        "case_log_path": str(run.get("case_log_path") or "").strip(),
        "funcov_path": str(run.get("funcov_path") or "").strip(),
    }
    for field, path_text in artifact_paths.items():
        if not path_text:
            reasons.append(f"missing_artifact:{field}")
            continue
        if artifact_root and not _absolute_path_within(path_text, artifact_root):
            reasons.append(f"artifact_outside_run:{field}")
            continue
        path = Path(path_text)
        if not path.is_file():
            reasons.append(f"missing_artifact_file:{field}")
            continue
        # An empty log is still a valid record for a quiet testcase.  The
        # evidence-bearing waveform, raw code coverage, and funcov JSON are
        # not useful when they contain no data.
        if field != "case_log_path":
            try:
                is_empty = path.stat().st_size == 0
            except OSError:
                is_empty = True
            if is_empty:
                reasons.append(f"empty_artifact_file:{field}")

    return {"kind": "dut", "eligible": not reasons, "reasons": reasons}


def load_artifacts(paths: Iterable[Path]) -> list[tuple[Path, dict, str]]:
    artifacts = []
    for path in paths:
        try:
            with path.open(encoding="utf-8") as f:
                raw = json.load(f)
        except (OSError, json.JSONDecodeError) as exc:
            raw = {
                "artifact_tag": path.stem,
                "_artifact_errors": [f"artifact_read_error:{type(exc).__name__}"],
            }
        if not isinstance(raw, dict):
            raw = {
                "artifact_tag": path.stem,
                "_artifact_errors": ["artifact_root_not_object"],
                "_artifact_root_type": type(raw).__name__,
            }
        artifacts.append((path, raw, artifact_kind(raw)))
    return artifacts


def _normalize_string_set(values: Any) -> set[str]:
    if values is None:
        return set()
    if isinstance(values, (str, int, float)):
        values = [values]
    result: set[str] = set()
    for value in values:
        text = str(value).strip()
        if text:
            result.add(text)
    return result


def _artifact_targets(raw: dict) -> dict[str, set[str]]:
    targets = raw.get("coverage_targets") or raw.get("targets") or {}
    if not isinstance(targets, dict):
        targets = {}
    return {
        "bin_ids": _normalize_string_set(
            _first_value(targets.get("bin_ids"), raw.get("target_bin_ids"))
        ),
        "hit_keys": _normalize_string_set(
            _first_value(targets.get("hit_keys"), raw.get("target_hit_keys"))
        ),
        "tp_ids": _normalize_string_set(
            _first_value(targets.get("tp_ids"), raw.get("target_tp_ids"))
        ),
        "testcases": _normalize_string_set(
            _first_value(targets.get("testcases"), raw.get("target_testcases"))
        ),
    }


def _has_explicit_targets(targets: dict[str, set[str]]) -> bool:
    return any(
        targets.get(key) for key in ("bin_ids", "hit_keys", "tp_ids", "testcases")
    )


def _target_matches_reference(
    raw: dict,
    tag: str,
    ref: CoverageReference,
    items: list[PilotBin],
    testcase: str,
) -> bool:
    targets = _artifact_targets(raw)
    bin_ids = {item.bin_id for item in items}
    hit_keys = {f"{item.group}::{item.coverpoint}::{item.bin_name}" for item in items}
    point_key = f"{ref.group}::{ref.coverpoint}"
    if targets["bin_ids"] or targets["hit_keys"]:
        return bool(
            bin_ids & targets["bin_ids"]
            or hit_keys & targets["hit_keys"]
            or point_key in targets["hit_keys"]
        )
    testcases = {item.testcase for item in items if item.testcase}
    if testcase:
        testcases.add(testcase)
    if testcases & targets["testcases"]:
        return True
    if _has_explicit_targets(targets):
        return False
    return any(candidate in tag for candidate in testcases)


def _target_matches(raw: dict, tag: str, item: PilotBin, bin_id: str, key: str) -> bool:
    """Preserve the bin-level target helper used by existing contract tests."""
    if (item.bin_id, f"{item.group}::{item.coverpoint}::{item.bin_name}") != (
        bin_id,
        key,
    ):
        return False
    return _target_matches_reference(
        raw,
        tag,
        CoverageReference(
            "bin",
            item.group,
            item.coverpoint,
            item.bin_name,
            item.bin_id,
        ),
        [item],
        item.testcase,
    )


def _dut_evidence_entry(raw: dict, tag: str, hit_count: int) -> str:
    stats = _as_mapping(raw.get("stats"))
    monitor = _as_mapping(stats.get("monitor"))
    run = _as_mapping(raw.get("run"))
    fields = [
        f"DUT:{tag}:hits={hit_count}",
        f"monitor_errors={_as_int(monitor.get('error_count')) or 0}",
    ]
    waveform = str(raw.get("waveform_path") or "").strip()
    if waveform:
        fields.append(f"waveform={waveform}")
    line_coverage = str(raw.get("line_coverage_path") or "").strip()
    if line_coverage:
        fields.append(f"dat={line_coverage}")
    run_id = str(run.get("run_id") or "").strip()
    if run_id:
        fields.append(f"run_id={run_id}")
    return ",".join(fields)


def build_artifact_audit(artifacts: Iterable[tuple[Path, dict, str]]) -> list[dict]:
    """Create a diagnostic summary for funcov artifacts without changing CSVs."""
    rows: list[dict] = []
    for path, raw, kind in artifacts:
        hits = _as_mapping(raw.get("hits"))
        targets = _artifact_targets(raw)
        gate = (
            evaluate_artifact(raw)
            if kind in {"dut", "invalid"}
            else {"eligible": False, "reasons": ["model_artifact"]}
        )
        run = _as_mapping(raw.get("run"))
        rows.append(
            {
                "path": str(path),
                "artifact_tag": str(raw.get("artifact_tag") or path.stem),
                "testcase_name": str(raw.get("testcase_name") or ""),
                "kind": kind,
                "eligible": bool(gate["eligible"]),
                "reasons": list(gate["reasons"]),
                "target_bin_ids": sorted(targets["bin_ids"]),
                "target_hit_keys": sorted(targets["hit_keys"]),
                "hit_key_count": len(hits),
                "positive_hit_key_count": sum(
                    1
                    for hit in hits.values()
                    if (_as_int(_as_mapping(hit).get("hits")) or 0) > 0
                ),
                "run_id": str(run.get("run_id") or ""),
            }
        )
    return rows


def _append_evidence(existing: str, entries: Iterable[str]) -> str:
    values = [item for item in str(existing or "").split("; ") if item]
    for entry in entries:
        if entry not in values:
            values.append(entry)
    return "; ".join(values)


def backannotate(
    testpoint_path: Path,
    pilot: dict[str, PilotBin],
    artifacts: Iterable[tuple[Path, dict, str]],
    *,
    apply: bool,
    bin_prefix: str | None = None,
    allow_testpoint_structure_errors: bool = False,
) -> dict[str, int]:
    fields, rows = _read_csv(testpoint_path)
    structure_errors: list[tuple[int, str]] = []
    validate_mapping(
        testpoint_path,
        pilot,
        bin_prefix=bin_prefix,
        structure_errors=structure_errors
        if allow_testpoint_structure_errors
        else None,
    )
    for _line, message in structure_errors:
        print(message, file=sys.stderr)
    artifacts = list(artifacts)
    counts = {
        "model": 0,
        "partial": 0,
        "hit": 0,
        "failed": 0,
        "closed_preserved": 0,
    }
    if allow_testpoint_structure_errors:
        counts["structure_errors"] = len(structure_errors)
        counts["structure_skipped_lines"] = len(
            {line for line, _message in structure_errors}
        )

    for row in rows:
        ref = parse_reference(row["coverage"])
        if ref is None:
            continue
        items = _pilot_bins_for_reference(ref, pilot)
        if not items:
            continue
        if bin_prefix and not any(item.bin_id.startswith(bin_prefix) for item in items):
            continue

        suggested_testcases = {item.testcase for item in items if item.testcase}
        if ref.kind == "bin":
            row["testcase"] = items[0].testcase
        elif not row["testcase"].strip() and len(suggested_testcases) == 1:
            row["testcase"] = next(iter(suggested_testcases))
        status = row["status"].strip() or "MODELED"
        if status == "CLOSED":
            counts["closed_preserved"] += 1
            continue
        if status in {"BLOCKED", "N-A"}:
            continue

        model_entries = []
        dut_entries = []
        rejected_entries = []
        dut_seen_for_testcase = False
        for path, raw, kind in artifacts:
            hits = _as_mapping(raw.get("hits"))
            hit_count = sum(
                _as_int(
                    _as_mapping(
                        hits.get(f"{item.group}::{item.coverpoint}::{item.bin_name}")
                    ).get("hits")
                )
                or 0
                for item in items
            )
            legacy_hit_count = max(
                (
                    _as_int(_as_mapping(hits.get(legacy_key)).get("hits")) or 0
                    for item in items
                    for legacy_key in (
                        f"{item.group}::{item.bin_name}",
                        f"{item.group}::::{item.bin_name}",
                    )
                ),
                default=0,
            )
            tag = str(raw.get("artifact_tag") or path.stem)
            target_matches = _target_matches_reference(
                raw,
                tag,
                ref,
                items,
                row["testcase"].strip(),
            )
            if kind in {"dut", "invalid"}:
                gate = evaluate_artifact(raw)
                if target_matches:
                    dut_seen_for_testcase = True
                if target_matches and gate["eligible"] and hit_count > 0:
                    dut_entries.append(_dut_evidence_entry(raw, tag, hit_count))
                elif kind == "dut" and target_matches and legacy_hit_count > 0:
                    rejected_entries.append(f"DUT_REJECTED:{tag}:legacy_hit_key")
                elif (
                    target_matches
                    and not gate["eligible"]
                    and (hit_count > 0 or gate["reasons"])
                ):
                    reason = ",".join(gate["reasons"])
                    prefix = "DUT_REJECTED" if kind == "dut" else "ARTIFACT_REJECTED"
                    rejected_entries.append(f"{prefix}:{tag}:{reason}")
            else:
                if target_matches or hit_count > 0:
                    model_entries.append(f"MODEL:{tag}")

        row["evidence"] = _append_evidence(
            row["evidence"], [*model_entries, *dut_entries, *rejected_entries]
        )
        if dut_entries:
            row["status"] = "HIT"
            counts["hit"] += 1
        elif dut_seen_for_testcase:
            row["status"] = "PARTIAL"
            counts["partial"] += 1
            if rejected_entries:
                counts["failed"] += 1
        else:
            row["status"] = "MODELED"
            counts["model"] += 1

    if apply:
        _write_csv(testpoint_path, fields, rows)
    return counts


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--pilot", type=Path, required=True)
    parser.add_argument("--testpoints", type=Path)
    parser.add_argument("--artifact", type=Path, action="append", default=[])
    parser.add_argument(
        "--bin-prefix", help="optional scoped Bin_ID prefix, for example BIN-5"
    )
    parser.add_argument(
        "--check", action="store_true", help="validate only; do not write"
    )
    parser.add_argument(
        "--allow-testpoint-structure-errors",
        action="store_true",
        help="skip malformed coverage leaf references and report their line numbers",
    )
    parser.add_argument(
        "--schema-check",
        action="store_true",
        help="validate global pilot identifiers only",
    )
    parser.add_argument(
        "--audit-json",
        type=Path,
        help="write per-artifact DUT gate/target diagnostics to this JSON file",
    )
    args = parser.parse_args()

    schema = validate_pilot_schema(args.pilot)
    if args.schema_check:
        print(" ".join(f"{key}={value}" for key, value in sorted(schema.items())))
        return 0
    if args.testpoints is None:
        parser.error("--testpoints is required unless --schema-check is used")

    pilot = load_pilot(args.pilot, bin_prefix=args.bin_prefix)
    artifacts = load_artifacts(args.artifact)
    if args.audit_json is not None:
        args.audit_json.parent.mkdir(parents=True, exist_ok=True)
        args.audit_json.write_text(
            json.dumps(build_artifact_audit(artifacts), ensure_ascii=False, indent=2),
            encoding="utf-8",
        )
    counts = backannotate(
        args.testpoints,
        pilot,
        artifacts,
        apply=not args.check,
        bin_prefix=args.bin_prefix,
        allow_testpoint_structure_errors=args.allow_testpoint_structure_errors,
    )
    print(" ".join(f"{key}={value}" for key, value in sorted(counts.items())))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
