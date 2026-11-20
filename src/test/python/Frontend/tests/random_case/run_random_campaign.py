#!/usr/bin/env python3
from __future__ import annotations

import argparse
import fcntl
import hashlib
import json
import os
import shutil
import signal
import subprocess
import sys
from types import SimpleNamespace
from datetime import datetime, timezone
from pathlib import Path

try:
    from .campaign_case_matrix import (
        _parse_family_count_overrides,
        _resolve_family_case_counts,
    )
    from .campaign_status import (
        _update_case_execution_summary,
        _record_post_run_gate,
        _finalize_campaign_status,
    )
    from .funcov_compare import load_artifact
    from .generate_random_asm import CaseSpec, FAMILIES, write_cases
    from .program_map import analyze_trace, build_program_map, realized_events
    from .scenario_contract import (
        RealizedEvent,
        ScenarioSpec,
        evaluate_realization,
        scenario_spec_from_dict,
        write_json_contract,
    )
    from .scenario_profiles import PAGE_TAIL_ATTRIBUTE_MODES, build_scenario_spec
    from .testpoint_catalog import DEFAULT_SOURCE as DEFAULT_TESTPOINT_SOURCE
    from .testpoint_catalog import build_catalog, write_catalog
except ImportError:
    from campaign_case_matrix import (
        _parse_family_count_overrides,
        _resolve_family_case_counts,
    )
    from campaign_status import (
        _update_case_execution_summary,
        _record_post_run_gate,
        _finalize_campaign_status,
    )
    from funcov_compare import load_artifact
    from generate_random_asm import CaseSpec, FAMILIES, write_cases
    from program_map import analyze_trace, build_program_map, realized_events
    from scenario_contract import (
        RealizedEvent,
        ScenarioSpec,
        evaluate_realization,
        scenario_spec_from_dict,
        write_json_contract,
    )
    from scenario_profiles import PAGE_TAIL_ATTRIBUTE_MODES, build_scenario_spec
    from testpoint_catalog import DEFAULT_SOURCE as DEFAULT_TESTPOINT_SOURCE
    from testpoint_catalog import build_catalog, write_catalog


FRONTEND_ROOT = Path(__file__).resolve().parents[2]
REPO_ROOT = FRONTEND_ROOT.parents[3]
RUN_CASE_SCRIPT = FRONTEND_ROOT / "scripts" / "run_baremode_asm_bin_trace.sh"
COMPARE_SCRIPT = Path(__file__).resolve().parent / "funcov_compare.py"
TRUSTED_FUNCOV_SCRIPT = Path(__file__).resolve().parent / "trusted_funcov.py"
BASELINE_AUDIT_SCRIPT = Path(__file__).resolve().parent / "baseline_audit.py"
GAP_AUDIT_SCRIPT = Path(__file__).resolve().parent / "coverage_gap_audit.py"
RANDOM_DUT_TEST = (
    Path(__file__).resolve().parent
    / "test_random_bin_trace_dut.py::test_random_bin_trace"
)


def _sha256(path: Path) -> str:
    if not path.is_file():
        return "unavailable"
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _derived_seed(master_seed: int, family: str, ordinal: int) -> int:
    payload = f"frontend-random-v1:{master_seed}:{family}:{ordinal}".encode("ascii")
    return int.from_bytes(hashlib.sha256(payload).digest()[:8], "big")


def _derived_timing_seed(timing_master_seed: int, family: str, ordinal: int) -> int:
    payload = f"frontend-random-timing-v1:{timing_master_seed}:{family}:{ordinal}".encode("ascii")
    return int.from_bytes(hashlib.sha256(payload).digest()[:8], "big")


def _atomic_json(path: Path, value: dict) -> None:
    temporary = path.with_name(f".{path.name}.tmp")
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    temporary.replace(path)


def _tool_identity(command: str) -> dict[str, str]:
    resolved = shutil.which(command)
    if resolved is None:
        return {"path": "unavailable", "version": "unavailable"}
    try:
        output = subprocess.run(
            [resolved, "--version"],
            check=False,
            capture_output=True,
            text=True,
            timeout=10,
        ).stdout.splitlines()
    except (OSError, subprocess.TimeoutExpired):
        output = []
    return {"path": resolved, "version": output[0].strip() if output else "unavailable"}


def _find_nemu(explicit: Path | None) -> Path | None:
    candidates: list[Path] = []
    if explicit is not None:
        candidates.append(explicit)
    for env_name in ("NEMU_EXEC", "TB_NEMU_EXEC"):
        if os.getenv(env_name, "").strip():
            candidates.append(Path(os.environ[env_name]).expanduser())
    candidates.extend(
        (
            REPO_ROOT / "ready-to-run" / "riscv64-nemu-interpreter",
            REPO_ROOT.parent / "frontend-bt-jiabowen" / "ready-to-run" / "riscv64-nemu-interpreter",
            REPO_ROOT.parent / "NEMU" / "build" / "riscv64-nemu-interpreter",
        )
    )
    for candidate in candidates:
        path = candidate.resolve(strict=False)
        if path.is_file() and os.access(path, os.X_OK):
            return path
    return None


def _run_with_timeout(command: list[str], env: dict[str, str], timeout_seconds: int) -> tuple[str, int]:
    process = subprocess.Popen(command, env=env, cwd=REPO_ROOT, start_new_session=True)
    try:
        return "completed", process.wait(timeout=timeout_seconds)
    except subprocess.TimeoutExpired:
        os.killpg(process.pid, signal.SIGTERM)
        try:
            process.wait(timeout=10)
        except subprocess.TimeoutExpired:
            os.killpg(process.pid, signal.SIGKILL)
            process.wait()
        return "timeout", 124


def _trace_details(case_root: Path) -> tuple[str, int, str]:
    traces = sorted((case_root / "inputs").glob("*.trace.jsonl"))
    if len(traces) != 1:
        return "unavailable", 0, "unavailable"
    trace = traces[0]
    with trace.open("rb") as stream:
        count = sum(1 for _ in stream)
    return str(trace.resolve()), count, _sha256(trace)


def _realized_event_from_dict(payload: dict) -> RealizedEvent:
    event = RealizedEvent(
        event=str(payload.get("event") or ""),
        count=int(payload.get("count") or 0),
        evidence_source=str(payload.get("evidence_source") or ""),
        relation=str(payload.get("relation") or "observed"),
        anchor=str(payload.get("anchor") or ""),
        first_index=(
            int(payload["first_index"])
            if payload.get("first_index") is not None
            else None
        ),
        first_cycle=(
            int(payload["first_cycle"])
            if payload.get("first_cycle") is not None
            else None
        ),
        first_pc=(
            int(payload["first_pc"])
            if payload.get("first_pc") is not None
            else None
        ),
        details=dict(payload.get("details") or {}),
    )
    event.validate()
    return event


def _load_dut_timeline(
    path: Path,
    scenario_spec: ScenarioSpec,
    program_map: dict,
) -> tuple[dict, tuple[RealizedEvent, ...], str, list[str]]:
    if not path.is_file():
        return {}, (), "missing", ["scenario_timeline_not_found"]
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError) as exc:
        return {}, (), "invalid", [f"scenario_timeline_parse_error={exc}"]
    if not isinstance(payload, dict):
        return {}, (), "invalid", ["scenario_timeline_root_is_not_an_object"]

    reasons: list[str] = []
    try:
        schema_version = int(payload.get("schema_version") or 0)
    except (TypeError, ValueError):
        schema_version = 0
    if schema_version != 1:
        reasons.append(f"scenario_timeline_schema={payload.get('schema_version')!r}")
    if str(payload.get("scenario_id") or "") != scenario_spec.scenario_id:
        reasons.append("scenario_timeline_scenario_id_mismatch")
    if str(payload.get("execution_status") or "") != "REALIZED":
        reasons.append(
            "scenario_timeline_execution_status="
            + str(payload.get("execution_status") or "missing")
        )
    expected_timing_plan = scenario_spec.parameters.get("timing_plan")
    if expected_timing_plan and payload.get("timing_plan") != expected_timing_plan:
        reasons.append("scenario_timeline_timing_plan_mismatch")
    if str(payload.get("program_map_binary_sha256") or "") != str(
        program_map.get("binary_sha256") or ""
    ):
        reasons.append("scenario_timeline_program_map_binary_mismatch")

    if scenario_spec.family == "translation_timing_cross":
        mapping = payload.get("program_address_mapping")
        if (not isinstance(mapping, dict) or mapping.get("schema_version") != 1
                or mapping.get("mode") != "static_translation" or mapping.get("valid") is not True):
            reasons.append("scenario_timeline_translation_program_mapping_missing_or_invalid")
        else:
            try:
                pages = mapping["pages"]
                if not isinstance(pages, list):
                    raise ValueError("pages must be a list")
                mapped_pages = {}
                for page in pages:
                    va, pa = page["va"], page["pa"]
                    if (type(va) is not int or type(pa) is not int or va < 0 or pa < 0
                            or va % 4096 or pa % 4096 or va in mapped_pages):
                        raise ValueError("invalid or duplicate page")
                    mapped_pages[va] = pa
                required_pages = {int(item["pc"]) & ~4095 for item in program_map.get("instructions", [])}
                if not required_pages.issubset(mapped_pages):
                    raise ValueError("missing program pages")
            except (KeyError, TypeError, ValueError):
                reasons.append("scenario_timeline_translation_program_pages_invalid")

    actions = payload.get("actions") or []
    if not isinstance(actions, list):
        reasons.append("scenario_timeline_actions_are_not_a_list")
        actions = []
    # Treat every malformed action as an invalid contract.  Keep the loader
    # total over JSON values so a truncated or hand-edited timeline cannot
    # abort campaign bookkeeping with an AttributeError.
    incomplete_actions: list[str] = []
    for index, item in enumerate(actions):
        if not isinstance(item, dict):
            incomplete_actions.append(f"index-{index}-not-an-object")
            continue
        action_id = str(item.get("action_id") or f"index-{index}")
        if str(item.get("status") or "") != "REALIZED":
            incomplete_actions.append(action_id)
    if incomplete_actions:
        reasons.append("scenario_timeline_incomplete_actions=" + ",".join(incomplete_actions))

    details = payload.get("details") or {}
    if not isinstance(details, dict):
        reasons.append("scenario_timeline_details_are_not_an_object")
    else:
        monitor_errors = details.get("monitor_errors")
        if type(monitor_errors) is not int or monitor_errors != 0:
            reasons.append(
                f"scenario_timeline_monitor_errors={details.get('monitor_errors')!r}"
            )
        oracle_errors = details.get("translation_oracle_error_count")
        if type(oracle_errors) is not int or oracle_errors != 0:
            reasons.append(
                "scenario_timeline_translation_oracle_errors="
                + repr(details.get("translation_oracle_error_count"))
            )

        if details.get("translation_oracle_errors") != [] or details.get("translation_oracle_stats_error"):
            reasons.append("scenario_timeline_translation_oracle_errors_missing_or_nonempty")

    dropped_events = payload.get("dropped_timeline_events")
    if type(dropped_events) is not int or dropped_events != 0:
        reasons.append(
            f"scenario_timeline_dropped_events={payload.get('dropped_timeline_events')!r}"
        )

    timeline_events: list[RealizedEvent] = []
    raw_events = payload.get("realized_events") or []
    if not isinstance(raw_events, list):
        reasons.append("scenario_timeline_realized_events_are_not_a_list")
    else:
        for index, raw_event in enumerate(raw_events):
            if not isinstance(raw_event, dict):
                reasons.append(f"scenario_timeline_event_{index}_is_not_an_object")
                continue
            try:
                event = _realized_event_from_dict(raw_event)
            except (TypeError, ValueError) as exc:
                reasons.append(f"scenario_timeline_event_{index}_invalid={exc}")
                continue
            if event.evidence_source != "dut_top_level_timeline":
                reasons.append(
                    f"scenario_timeline_event_{index}_source={event.evidence_source or 'missing'}"
                )
                continue
            timeline_events.append(event)
    status = "valid" if not reasons else "invalid"
    return payload, tuple(timeline_events) if status == "valid" else (), status, reasons


def _load_funcov_evidence(paths: list[Path], *, native_bindings: dict | None = None,
                          native_expected_provenance: dict | None = None,
                          require_current_native: bool = False) -> dict:
    evidence: list[dict] = []
    artifacts = []
    reasons: list[str] = []
    for path in paths:
        try:
            raw = json.loads(path.read_text())
            if require_current_native and "groups" not in raw:
                raise ValueError("legacy report is diagnostic only, not current native admission")
            binding = (native_bindings or {}).get(str(path.resolve()))
            artifact = load_artifact(path, **({
                "native_sidecar": Path(binding["sidecar_path"]),
                "native_sidecar_sha256": binding["sidecar_sha256"],
            } if binding is not None else {}))
            if artifact.eligible and native_expected_provenance is not None:
                if binding is None:
                    raise ValueError("expected native provenance requires explicit binding")
                sidecar = Path(binding["sidecar_path"])
                if _sha256(sidecar) != binding["sidecar_sha256"]:
                    raise ValueError("native sidecar changed during admission")
                reference = json.loads(sidecar.read_text())["frozen_contract"]
                frozen_path = Path(reference["path"])
                if not frozen_path.is_absolute():
                    frozen_path = sidecar.parent / frozen_path
                if _sha256(frozen_path) != reference["sha256"]:
                    raise ValueError("native frozen contract changed during admission")
                actual = json.loads(frozen_path.read_text())["provenance"]
                for field, expected in native_expected_provenance.items():
                    if actual.get(field) != expected:
                        raise ValueError("native current provenance mismatch: " + field)
        except (OSError, ValueError, TypeError, KeyError) as exc:
            reasons.append(f"funcov_parse_error={path}:{exc}")
            evidence.append(
                {
                    "path": str(path.resolve()),
                    "sha256": _sha256(path),
                    "eligible": False,
                    "eligibility_reason": f"parse_error={exc}",
                }
            )
            continue
        artifacts.append(artifact)
        evidence.append(
            {
                "path": str(path.resolve()),
                "sha256": _sha256(path),
                "run_id": artifact.run_id,
                "eligible": artifact.eligible,
                "eligibility_reason": artifact.eligibility_reason,
                "compatibility_signature": artifact.compatibility_signature,
                "definitions_sha256": artifact.definitions_sha256,
                "definition_bin_count": len(artifact.definition_bin_ids),
                "hit_bin_count": len(artifact.hit_bin_ids),
                **({"native_bin_count": artifact.native_bin_count,
                    "native_hit_bin_count": artifact.native_hit_bin_count}
                   if artifact.native_bin_count is not None else {}),
            }
        )
        if not artifact.eligible:
            reasons.append(f"funcov_ineligible={path}:{artifact.eligibility_reason}")

    if not paths:
        reasons.append("no_functional_coverage_artifacts")
    contracts = {
        (artifact.compatibility_signature, artifact.definitions_sha256)
        for artifact in artifacts
        if artifact.eligible
    }
    definition_sets = {
        artifact.definition_bin_ids for artifact in artifacts if artifact.eligible
    }
    if len(contracts) > 1:
        reasons.append("multiple_funcov_compatibility_contracts_within_case")
    if len(definition_sets) > 1:
        reasons.append("inconsistent_funcov_definition_sets_within_case")
    eligible_artifacts = [artifact for artifact in artifacts if artifact.eligible]
    observed_bin_ids = sorted(
        {
            bin_id
            for artifact in eligible_artifacts
            for bin_id in artifact.hit_bin_ids
        }
    )
    contract = (
        next(iter(contracts))
        if len(contracts) == 1 and len(definition_sets) == 1
        else None
    )
    return {
        "admission_scope": "current_native" if require_current_native or native_expected_provenance is not None else "diagnostic_frozen_artifact_only",
        "canonical_hit_eligible": False,
        "status": "eligible" if not reasons else "ineligible",
        "reasons": reasons,
        "artifacts": evidence,
        "eligible_artifact_paths": [str(artifact.path) for artifact in eligible_artifacts],
        "observed_bin_ids": observed_bin_ids,
        "compatibility_contract": (
            None
            if contract is None
            else {
                "compatibility_signature": contract[0],
                "definitions_sha256": contract[1],
                "definition_bin_count": len(next(iter(definition_sets))),
            }
        ),
    }


def audit_native_campaign(campaign_path: Path, binding_index: Path, index_sha256: str) -> dict:
    """Offline artifact admission only; never run/resume or rewrite a campaign.

    Coord supplies the index pin after freezing each completed report/sidecar.
    This result cannot upgrade the original case qualification or canonical HIT.
    """
    if not index_sha256 or _sha256(binding_index) != index_sha256:
        raise ValueError("native binding index hash missing or changed")
    index = json.loads(binding_index.read_text())
    if not isinstance(index, dict) or index.get("schema_version") != 1:
        raise ValueError("invalid native binding index schema")
    expected_provenance = index.get("expected_provenance")
    fields = ("compatibility_signature", "definitions_sha256", "dut_source_sha",
              "build_manifest_sha256", "dut_build_sha256", "dut_python_extension_sha256",
              "generated_rtl_sha256", "signal_contract_sha256", "registry_sha256",
              "sampler_sha256", "verification_env_sha256")
    if not isinstance(expected_provenance, dict) or set(expected_provenance) != set(fields):
        raise ValueError("native index requires exact current expected provenance")
    for field, value in expected_provenance.items():
        size = 40 if field == "dut_source_sha" else 64
        if not isinstance(value, str) or len(value) != size or any(c not in "0123456789abcdef" for c in value):
            raise ValueError("native index expected provenance invalid: " + field)
    campaign_hash = _sha256(campaign_path)
    if campaign_hash == "unavailable" or index.get("campaign_sha256") != campaign_hash:
        raise ValueError("native binding campaign hash mismatch")
    campaign = json.loads(campaign_path.read_text())
    cases = campaign.get("cases")
    if not isinstance(cases, list) or not cases:
        raise ValueError("native campaign requires explicit cases")
    paths = []
    names = set()
    for case in cases:
        name = case.get("name")
        if not isinstance(name, str) or not name or name in names:
            raise ValueError("native campaign duplicate/missing case identity")
        names.add(name)
        artifact_paths = case.get("functional_coverage_artifacts")
        hashes = case.get("functional_coverage_sha256")
        if (not isinstance(artifact_paths, list) or not artifact_paths
                or not isinstance(hashes, list) or len(artifact_paths) != len(hashes)):
            raise ValueError("native campaign requires recorded artifact hashes")
        for value, expected in zip(artifact_paths, hashes):
            path = Path(value)
            if not path.is_absolute():
                path = campaign_path.parent / path
            path = path.resolve()
            if str(path) in paths or _sha256(path) != expected:
                raise ValueError("native campaign artifact duplicate/hash mismatch")
            raw = json.loads(path.read_text())
            if raw.get("run_metadata", {}).get("run_id") != name:
                raise ValueError("native report/campaign run identity mismatch")
            # Bind seeds and real input identities, not merely the report filename.
            execution = raw.get("run_metadata", {}).get("execution", {})
            seeds = execution.get("seeds", {})
            if seeds.get("test") != case.get("spec", {}).get("seed") or seeds.get("backend") != case.get("timing_seed"):
                raise ValueError("native campaign program/timing seed mismatch")
            for native_kind, case_kind in (("asm", "source"), ("bin", "bin"), ("trace", "trace")):
                expected_input = case.get(case_kind + "_sha256")
                if not expected_input or execution.get(native_kind + "_sha256") != expected_input:
                    raise ValueError("native campaign input hash mismatch: " + case_kind)
            paths.append(str(path))
    bindings = {}
    rows = index.get("artifacts")
    if not isinstance(rows, list):
        raise ValueError("native binding index artifacts missing")
    for row in rows:
        path = Path(row["path"])
        if not path.is_absolute():
            path = campaign_path.parent / path
        key = str(path.resolve())
        if key in bindings:
            raise ValueError("native binding duplicate artifact")
        sidecar = Path(row["sidecar_path"])
        if not sidecar.is_absolute():
            sidecar = binding_index.parent / sidecar
        bindings[key] = {"sidecar_path": str(sidecar.resolve()),
                         "sidecar_sha256": row["sidecar_sha256"]}
    if set(bindings) != set(paths):
        raise ValueError("native binding inventory incomplete or unexpected artifact")
    evidence = _load_funcov_evidence([Path(path) for path in paths], native_bindings=bindings,
                                     native_expected_provenance=expected_provenance)
    if evidence["status"] != "eligible":
        evidence.update(observed_bin_ids=[], eligible_artifact_paths=[], compatibility_contract=None)
    if _sha256(campaign_path) != campaign_hash or _sha256(binding_index) != index_sha256:
        raise ValueError("native campaign/index changed during audit")
    return {"schema_version": 1, "scope": "native artifact admission only; no case qualification or HIT upgrade",
            "campaign_path": str(campaign_path.resolve()), "campaign_sha256": campaign_hash,
            "binding_index_sha256": index_sha256, "evidence": evidence,
            "original_case_qualification_unchanged": True, "canonical_hit_eligible": False}


def _translation_configuration_preflight(
    scenario_spec: ScenarioSpec, bin_path: Path, link_address: int,
) -> dict:
    """Validate real scenario/CSR contracts without constructing or stepping DUT."""
    if scenario_spec.family != "translation_timing_cross":
        return {"status": "not_applicable", "reasons": []}
    evidence = {
        "status": "invalid", "reasons": [], "binary_sha256": _sha256(bin_path),
        "scope": "configuration_only; not DUT execution or functional coverage",
    }
    try:
        # Direct CLI use does not otherwise put Frontend/env on sys.path.
        frontend_path = str(Path(__file__).resolve().parents[2])
        if frontend_path not in sys.path:
            sys.path.insert(0, frontend_path)
        from env.agents.ptw_agent import PTWAgent
        from env.model.page_table_model import PageTableModel
        from env.sequences import TranslationScenarioBuilder
        if __package__:
            from .translation_timing import build_translation_scenario
        else:
            from translation_timing import build_translation_scenario
        raw = scenario_spec.parameters["translation"]
        scenario = build_translation_scenario(
            scenario_spec, bin_path.read_bytes(), virtual_base=link_address,
            physical_base=int(raw["physical_base"]),
            guest_physical_base=int(raw["guest_physical_base"]),
        )
        evidence.update({
            "permission_layout": raw.get("permission_layout", "per_page_v1"),
            "page_count": scenario.page_count,
            "pmp_entry_count": len(scenario.pmp_entries),
            "pma_entry_count": len(scenario.pma_entries),
        })
        validator = TranslationScenarioBuilder(
            SimpleNamespace(ptw_agent=PTWAgent(PageTableModel()))
        )
        validator.validate(scenario)
        evidence["status"] = "valid"
    except (OSError, ValueError, TypeError, KeyError, ImportError) as exc:
        evidence["reasons"] = [f"{type(exc).__name__}: {exc}"]
    return evidence


def _validate_retained_translation_records(records, scenario_specs, link_address):
    """Fail before resume mutations if a retained translation result is stale.

    Do not silently turn a failed configuration check into an expensive rerun
    of identical inputs, or rewrite a historical qualification to look current.
    This does not establish DUT provenance compatibility for a new build.
    """
    for record in records:
        if record.get("qualification_pass") is not True:
            continue
        spec = scenario_specs[str(record["name"])]
        if spec.family != "translation_timing_cross":
            continue
        for kind in ("source", "bin", "trace", "program_map", "scenario_spec"):
            raw_path = record.get(kind + "_path")
            expected = record.get(kind + "_sha256")
            if (not raw_path or not expected or expected == "unavailable"
                    or _sha256(Path(raw_path)) != expected):
                raise ValueError(f"retained translation {record['name']}: {kind} hash missing or changed")
        config = _translation_configuration_preflight(spec, Path(record["bin_path"]), link_address)
        if config["status"] != "valid":
            raise ValueError(
                f"retained translation {record['name']}: configuration preflight failed: "
                + "; ".join(config["reasons"])
            )


def _case_record_after_run(
    record: dict,
    scenario_spec: ScenarioSpec,
    case_root: Path,
    status: str,
    exit_code: int,
    minimum_dynamic_instructions: int,
    mode: str,
    link_address: int,
    binary_base: int,
    binary_path: Path | None = None,
) -> dict:
    case_name = str(record["name"])
    bin_path = (
        binary_path
        if binary_path is not None
        else FRONTEND_ROOT / "tests" / "asm_cases" / "generated" / f"{case_name}.bin"
    )
    trace_path, trace_count, trace_sha256 = _trace_details(case_root)
    funcov_paths = sorted((case_root / "funcov").glob("*.funcov.json"))
    instruction_requirement_met = trace_count >= int(minimum_dynamic_instructions)
    program_map_path = case_root / "inputs" / f"{case_name}.program_map.json"
    trace_analysis_path = case_root / "inputs" / f"{case_name}.trace_analysis.json"
    realization_path = case_root / "inputs" / f"{case_name}.scenario_realization.json"
    timeline_path = case_root / "inputs" / f"{case_name}.scenario_timeline.json"
    configuration = _translation_configuration_preflight(scenario_spec, bin_path, link_address)
    configuration_path = case_root / "inputs" / f"{case_name}.configuration_preflight.json"
    if configuration["status"] != "not_applicable":
        write_json_contract(configuration_path, configuration)
    program_map_status = "unavailable"
    program_map_error = ""
    program_map: dict = {}
    trace_analysis: dict = {}
    # The DUT test writes its verified ProgramMap before simulation starts.
    # Preserve and revalidate that evidence even when the checker later fails.
    if bin_path.is_file() and trace_path != "unavailable":
        try:
            program_map = build_program_map(
                Path(record["source_path"]),
                bin_path,
                record,
                link_address=link_address,
                binary_base=binary_base,
            )
            write_json_contract(program_map_path, program_map)
            trace_analysis = analyze_trace(Path(trace_path), program_map)
            write_json_contract(trace_analysis_path, trace_analysis)
            if int(trace_analysis["missing_program_map_pc_count"]) != 0:
                raise ValueError(
                    "committed trace contains PCs absent from ProgramMap: "
                    + ", ".join(trace_analysis["missing_program_map_pcs"][:8])
                )
            program_map_status = "valid"
        except (OSError, ValueError, subprocess.SubprocessError) as exc:
            program_map_status = "invalid"
            program_map_error = str(exc)

    timeline_payload: dict = {}
    timeline_events: tuple[RealizedEvent, ...] = ()
    timeline_status = "not_applicable"
    timeline_reasons: list[str] = []
    funcov_evidence = {
        "status": "not_applicable",
        "reasons": [],
        "artifacts": [],
        "eligible_artifact_paths": [],
        "observed_bin_ids": [],
        "compatibility_contract": None,
    }
    native_admission = None
    if mode == "dut":
        timeline_payload, timeline_events, timeline_status, timeline_reasons = _load_dut_timeline(
            timeline_path,
            scenario_spec,
            program_map,
        )
        try:
            try:
                from .native_runtime_binding import bind_native_reports
            except ImportError:
                from native_runtime_binding import bind_native_reports
            bindings, expected = bind_native_reports(
                funcov_paths, timeline_path=timeline_path,
                output_dir=case_root / "inputs" / "native-admission",
            )
            funcov_evidence = _load_funcov_evidence(
                funcov_paths, native_bindings=bindings,
                native_expected_provenance=expected, require_current_native=True,
            )
            native_admission = {"scope": "current_native", "bindings": bindings,
                                "expected_provenance": expected}
        except (OSError, KeyError, TypeError, ValueError) as exc:
            funcov_evidence.update(status="ineligible", reasons=["native_runtime_binding=" + str(exc)])

    program_events = realized_events(trace_analysis) if trace_analysis else ()
    funcov_events = tuple(
        RealizedEvent(
            event="funcov.bin_hit",
            relation="bin_id",
            anchor=bin_id,
            count=1,
            evidence_source="compatible_functional_coverage_artifact",
        )
        for bin_id in funcov_evidence["observed_bin_ids"]
    )
    all_realized_events = (*program_events, *timeline_events, *funcov_events)

    realization_execution_status = status
    if exit_code == 0 and program_map_status != "valid":
        realization_execution_status = "infra"
    if mode == "dut" and exit_code == 0 and (
        timeline_status != "valid" or funcov_evidence["status"] != "eligible"
    ):
        realization_execution_status = "infra"
    realization = evaluate_realization(
        scenario_spec,
        all_realized_events,
        evidence_stage="dut_program_plus_funcov" if mode == "dut" else "nemu_program",
        execution_status=realization_execution_status,
        exit_code=exit_code,
        observed_bin_ids=tuple(funcov_evidence["observed_bin_ids"]),
        realized_episode_ids=tuple((trace_analysis.get("realized_episode_counts") or {}).keys()),
        artifact_paths={
            "scenario_spec": str(Path(record["scenario_spec_path"]).resolve()),
            "program_map": str(program_map_path.resolve()) if program_map_status == "valid" else "",
            "trace_analysis": str(trace_analysis_path.resolve()) if trace_analysis else "",
            "trace": trace_path,
            "scenario_timeline": (
                str(timeline_path.resolve()) if timeline_path.is_file() else ""
            ),
            **{
                f"funcov_{index}": path
                for index, path in enumerate(funcov_evidence["eligible_artifact_paths"])
            },
        },
        details={
            "configuration_preflight": configuration,
            "program_map_status": program_map_status,
            "program_map_error": program_map_error,
            "scenario_timeline_status": timeline_status,
            "scenario_timeline_reasons": timeline_reasons,
            "functional_coverage_status": funcov_evidence["status"],
            "functional_coverage_reasons": funcov_evidence["reasons"],
            "testpoint_realization_policy": (
                "planned testpoint IDs remain REQUESTED until a DUT timeline or compatible "
                "functional bin proves the individual scenario"
            ),
        },
    )
    write_json_contract(realization_path, realization.to_dict())
    qualification_pass = (
        exit_code == 0
        and instruction_requirement_met
        and program_map_status == "valid"
        and configuration["status"] in {"valid", "not_applicable"}
        and (mode != "dut" or timeline_status == "valid")
        and (mode != "dut" or funcov_evidence["status"] == "eligible")
        and realization.status == "REALIZED"
    )
    return {
        **record,
        "artifact_root": str(case_root.resolve()),
        "execution_status": status,
        "exit_code": exit_code,
        "bin_path": str(bin_path.resolve()),
        "bin_sha256": _sha256(bin_path),
        "trace_path": trace_path,
        "trace_sha256": trace_sha256,
        "dynamic_trace_instructions": trace_count,
        "minimum_dynamic_instructions": int(minimum_dynamic_instructions),
        "dynamic_instruction_requirement_met": instruction_requirement_met,
        "program_map_status": program_map_status,
        "program_map_error": program_map_error,
        "program_map_path": (
            str(program_map_path.resolve()) if program_map_status == "valid" else "unavailable"
        ),
        "program_map_sha256": (
            _sha256(program_map_path) if program_map_status == "valid" else "unavailable"
        ),
        "trace_analysis_path": (
            str(trace_analysis_path.resolve()) if trace_analysis else "unavailable"
        ),
        "scenario_realization_path": str(realization_path.resolve()),
        "scenario_realization_sha256": _sha256(realization_path),
        "scenario_realization_status": realization.status,
        "scenario_timeline_path": (
            str(timeline_path.resolve()) if timeline_path.is_file() else "unavailable"
        ),
        "scenario_timeline_sha256": _sha256(timeline_path),
        "scenario_timeline_status": timeline_status,
        "scenario_timeline_reasons": timeline_reasons,
        "scenario_timeline_execution_status": str(
            timeline_payload.get("execution_status") or "unavailable"
        ),
        "qualification_pass": qualification_pass,
        "configuration_preflight": configuration,
        "configuration_preflight_path": (
            str(configuration_path.resolve()) if configuration_path.is_file() else "unavailable"
        ),
        "functional_coverage_artifacts": [str(path.resolve()) for path in funcov_paths],
        "functional_coverage_sha256": [_sha256(path) for path in funcov_paths],
        "functional_coverage_status": funcov_evidence["status"],
        "functional_coverage_reasons": funcov_evidence["reasons"],
        "functional_coverage_evidence": funcov_evidence["artifacts"],
        "eligible_functional_coverage_artifacts": funcov_evidence[
            "eligible_artifact_paths"
        ],
        "native_admission": native_admission,
        "functional_coverage_contract": funcov_evidence["compatibility_contract"],
        "observed_bin_ids": funcov_evidence["observed_bin_ids"],
    }


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Generate and run a replayable long Frontend random campaign"
    )
    parser.add_argument("--family", action="append", choices=FAMILIES, default=[])
    parser.add_argument("--master-seed", type=lambda value: int(value, 0), required=False)
    parser.add_argument(
        "--timing-master-seed",
        type=lambda value: int(value, 0),
        default=None,
        help="independent timing-seed root; defaults to master-seed xor a fixed constant",
    )
    parser.add_argument("--count", type=int, default=4, help="case count per family")
    parser.add_argument(
        "--family-count",
        action="append",
        default=[],
        metavar="FAMILY=COUNT",
        help=(
            "explicit quota for one family; repeat as needed. When --family is "
            "omitted, only the named families are generated"
        ),
    )
    parser.add_argument("--blocks", type=int, default=48)
    parser.add_argument(
        "--iterations",
        type=int,
        default=10,
        help="loop count; 10 is the qualified default that keeps current CFG seeds above 5000 commits",
    )
    parser.add_argument("--max-call-depth", type=int, default=6)
    parser.add_argument("--branch-history-mode", choices=("loop_mask", "site8"), default="loop_mask")
    parser.add_argument("--ras-return-mode", choices=("private", "shared_leaf"), default="private")
    parser.add_argument("--indirect-target-mode", choices=("fixed", "history4", "history4_page"), default="fixed")
    parser.add_argument(
        "--directed-pattern",
        choices=(
            "none", "page_tail_pair", "page_tail_pair_dense", "page_tail_pair_sparse",
            "taken_cj_offset_ring", "taken_cj_offset_ring_permuted",
        ),
        default="none",
    )
    parser.add_argument(
        "--page-tail-attribute-mode",
        choices=PAGE_TAIL_ATTRIBUTE_MODES,
        default="alternating",
        help=(
            "page-tail translation attribute profile; rvi_nc_pair keeps the "
            "RVI probe's current and next pages non-cacheable"
        ),
    )
    parser.add_argument(
        "--page-tail-probe-repetitions",
        type=int,
        default=None,
        help=(
            "decouple in-loop page-tail pressure from the long-program loop count; "
            "omit to preserve historical behavior"
        ),
    )
    parser.add_argument(
        "--uncache-a-stall-cycles",
        type=int,
        default=0,
        help=(
            "after observing a real InstrUncache TL-A valid, hold ready low for "
            "this many stable cycles before requiring release fire"
        ),
    )
    parser.add_argument("--mode", choices=("generate", "nemu", "dut", "audit-native"), default="generate")
    parser.add_argument("--output-root", type=Path, required=True)
    parser.add_argument(
        "--resume",
        action="store_true",
        help="resume an existing campaign manifest, skipping cases that already passed qualification",
    )
    parser.add_argument("--native-binding-index", type=Path)
    parser.add_argument("--native-binding-index-sha256")
    parser.add_argument("--native-audit-output", type=Path)
    parser.add_argument("--nemu-exec", type=Path, default=None)
    parser.add_argument("--coord-contract", type=Path,
                        help="coord-published loader-valid runtime contract; required for DUT mode")
    parser.add_argument("--coord-contract-sha256",
                        help="explicit SHA256 pin for --coord-contract")
    parser.add_argument("--case-timeout-seconds", type=int, default=7200)
    parser.add_argument("--pytest-timeout-seconds", type=int, default=6400)
    parser.add_argument("--trace-max-cycles", type=int, default=0)
    parser.add_argument("--waveform", action="store_true")
    parser.add_argument("--stop-on-fail", action="store_true")
    parser.add_argument("--baseline-root", action="append", type=Path, default=[])
    parser.add_argument("--coverage-minimum-hit-rate", type=float, default=0.99)
    parser.add_argument("--coverage-minimum-qualified-runs", type=int, default=100)
    parser.add_argument(
        "--require-goal-coverage",
        action="store_true",
        help="fail DUT campaign unless the strict union reaches the random-goal thresholds",
    )
    parser.add_argument("--testpoint-source", type=Path, default=DEFAULT_TESTPOINT_SOURCE)
    parser.add_argument("--min-dynamic-instructions", type=int, default=5000)
    parser.add_argument("--link-address", type=lambda value: int(value, 0), default=0x80000000)
    parser.add_argument("--binary-base", type=lambda value: int(value, 0), default=0x80000000)
    return parser.parse_args()


def main() -> int:
    args = _parse_args()
    if args.mode == "audit-native":
        if not args.native_binding_index or not args.native_binding_index_sha256 or not args.native_audit_output:
            raise SystemExit("audit-native requires explicit index, SHA256 and fresh output")
        try:
            report = audit_native_campaign(args.output_root / "campaign.json",
                                           args.native_binding_index, args.native_binding_index_sha256)
            # Exclusive creation: never overwrite a campaign or historical artifact.
            with args.native_audit_output.open("x", encoding="utf-8") as stream:
                json.dump(report, stream, indent=2)
                stream.write("\n")
        except (OSError, AttributeError, KeyError, TypeError, ValueError) as exc:
            raise SystemExit("native audit rejected: " + str(exc)) from exc
        return 0 if report["evidence"]["status"] == "eligible" else 1
    if args.mode == "dut":
        if not args.coord_contract or not args.coord_contract_sha256:
            raise SystemExit("DUT mode requires --coord-contract and --coord-contract-sha256")
        try:
            try:
                from .native_runtime_binding import validate_coord_pin
            except ImportError:
                from native_runtime_binding import validate_coord_pin
            validate_coord_pin(args.coord_contract, args.coord_contract_sha256)
        except (OSError, ValueError, TypeError) as exc:
            raise SystemExit("DUT coord contract rejected before generation: " + str(exc)) from exc
    if args.native_binding_index or args.native_binding_index_sha256 or args.native_audit_output:
        raise SystemExit("native binding is post-run offline audit only; select --mode audit-native")
    try:
        requested_family_count_overrides = _parse_family_count_overrides(
            list(args.family_count)
        )
    except ValueError as exc:
        raise SystemExit(str(exc)) from exc
    resume_campaign: dict | None = None
    output_root = args.output_root.resolve()
    manifest_path = output_root / "campaign.json"
    campaign_lock = None
    if args.resume:
        if not manifest_path.is_file():
            raise SystemExit(f"--resume requires an existing campaign manifest: {manifest_path}")
        lock_path = output_root / ".campaign.lock"
        campaign_lock = lock_path.open("a+", encoding="utf-8")
        try:
            fcntl.flock(campaign_lock.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError as exc:
            campaign_lock.close()
            raise SystemExit(
                f"another resume process already owns campaign lock: {lock_path}"
            ) from exc
        try:
            resume_campaign = json.loads(manifest_path.read_text(encoding="utf-8"))
        except (OSError, ValueError) as exc:
            raise SystemExit(f"unable to load campaign manifest {manifest_path}: {exc}") from exc
        if not isinstance(resume_campaign, dict) or not isinstance(resume_campaign.get("cases"), list):
            raise SystemExit(f"invalid campaign manifest for --resume: {manifest_path}")
        manifest_master_seed = int(resume_campaign.get("master_seed"))
        if args.master_seed is not None and args.master_seed != manifest_master_seed:
            raise SystemExit("--master-seed does not match the existing campaign manifest")
        args.master_seed = manifest_master_seed
        manifest_timing_master_seed = int(resume_campaign.get("timing_master_seed"))
        if (
            args.timing_master_seed is not None
            and args.timing_master_seed != manifest_timing_master_seed
        ):
            raise SystemExit("--timing-master-seed does not match the existing campaign manifest")
        args.timing_master_seed = manifest_timing_master_seed
        parameters = dict(resume_campaign.get("parameters") or {})
        # Reuse the original generation and qualification contract.  Runtime-only
        # controls such as --stop-on-fail and --nemu-exec remain caller-selectable.
        args.mode = str(resume_campaign.get("mode") or args.mode)
        args.count = int(parameters.get("count_per_family", args.count))
        args.blocks = int(parameters.get("blocks", args.blocks))
        args.iterations = int(parameters.get("iterations", args.iterations))
        args.max_call_depth = int(parameters.get("max_call_depth", args.max_call_depth))
        args.branch_history_mode = str(parameters.get("branch_history_mode", "loop_mask"))
        args.ras_return_mode = str(parameters.get("ras_return_mode", "private"))
        args.indirect_target_mode = str(parameters.get("indirect_target_mode", "fixed"))
        args.directed_pattern = str(parameters.get("directed_pattern", "none"))
        args.page_tail_attribute_mode = str(
            parameters.get("page_tail_attribute_mode", "alternating")
        )
        retained_probe_repetitions = parameters.get("page_tail_probe_repetitions")
        args.page_tail_probe_repetitions = (
            int(retained_probe_repetitions)
            if retained_probe_repetitions is not None
            else None
        )
        args.uncache_a_stall_cycles = int(
            parameters.get("uncache_a_stall_cycles", 0)
        )
        args.min_dynamic_instructions = int(
            parameters.get("minimum_dynamic_instructions", args.min_dynamic_instructions)
        )
        args.link_address = int(parameters.get("link_address", args.link_address))
        args.binary_base = int(parameters.get("binary_base", args.binary_base))
        args.case_timeout_seconds = int(parameters.get("case_timeout_seconds", args.case_timeout_seconds))
        args.pytest_timeout_seconds = int(parameters.get("pytest_timeout_seconds", args.pytest_timeout_seconds))
        args.trace_max_cycles = int(parameters.get("trace_max_cycles", args.trace_max_cycles))
        args.waveform = bool(parameters.get("waveform", args.waveform))
        args.coverage_minimum_hit_rate = float(
            parameters.get("coverage_minimum_hit_rate", args.coverage_minimum_hit_rate)
        )
        args.coverage_minimum_qualified_runs = int(
            parameters.get("coverage_minimum_qualified_runs", args.coverage_minimum_qualified_runs)
        )
        args.require_goal_coverage = bool(parameters.get("require_goal_coverage", args.require_goal_coverage))
    if args.master_seed is None or args.master_seed < 0 or args.master_seed >= 1 << 64:
        raise SystemExit("--master-seed must fit in an unsigned 64-bit integer")
    timing_master_seed = (
        args.timing_master_seed
        if args.timing_master_seed is not None
        else args.master_seed ^ 0xD1B54A32D192ED03
    )
    if timing_master_seed < 0 or timing_master_seed >= 1 << 64:
        raise SystemExit("--timing-master-seed must fit in an unsigned 64-bit integer")
    if args.count <= 0:
        raise SystemExit("--count must be > 0")
    if args.min_dynamic_instructions < 5000:
        raise SystemExit("--min-dynamic-instructions must be >= 5000 for random-goal campaigns")
    if args.case_timeout_seconds <= 0 or args.pytest_timeout_seconds <= 0:
        raise SystemExit("timeouts must be > 0")
    if not 0.0 <= args.coverage_minimum_hit_rate <= 1.0:
        raise SystemExit("--coverage-minimum-hit-rate must be in [0, 1]")
    if args.coverage_minimum_qualified_runs < 0:
        raise SystemExit("--coverage-minimum-qualified-runs must be >= 0")
    if args.require_goal_coverage and args.mode != "dut":
        raise SystemExit("--require-goal-coverage is valid only with --mode dut")
    if args.link_address < args.binary_base:
        raise SystemExit("--link-address must be >= --binary-base")
    if not 0 <= args.uncache_a_stall_cycles <= 1024:
        raise SystemExit("--uncache-a-stall-cycles must be in [0, 1024]")
    if (
        args.page_tail_attribute_mode != "alternating"
        and args.directed_pattern
        not in {"page_tail_pair", "page_tail_pair_dense", "page_tail_pair_sparse"}
    ):
        raise SystemExit(
            "--page-tail-attribute-mode requires a page-tail pattern"
        )
    if args.uncache_a_stall_cycles and (
        args.page_tail_attribute_mode != "rvi_pbmt_nc_pair"
        or args.directed_pattern
        not in {"page_tail_pair", "page_tail_pair_dense", "page_tail_pair_sparse"}
    ):
        raise SystemExit(
            "--uncache-a-stall-cycles requires a page-tail pattern and "
            "--page-tail-attribute-mode rvi_pbmt_nc_pair"
        )
    if resume_campaign is not None:
        manifest_families = list(resume_campaign.get("families") or [])
        if args.family and args.family != manifest_families:
            raise SystemExit("--family does not match the existing campaign manifest")
        families = manifest_families
        manifest_family_case_counts = dict(
            resume_campaign.get("family_case_counts")
            or dict(resume_campaign.get("parameters") or {}).get("family_case_counts")
            or {family: args.count for family in families}
        )
        try:
            family_case_counts = {
                family: int(manifest_family_case_counts[family]) for family in families
            }
        except (KeyError, TypeError, ValueError) as exc:
            raise SystemExit("existing campaign has invalid family_case_counts") from exc
        for family, requested_count in requested_family_count_overrides.items():
            if family not in family_case_counts or family_case_counts[family] != requested_count:
                raise SystemExit("--family-count does not match the existing campaign manifest")
    else:
        try:
            families, family_case_counts = _resolve_family_case_counts(
                list(args.family),
                requested_family_count_overrides,
                args.count,
            )
        except ValueError as exc:
            raise SystemExit(str(exc)) from exc
    if not families:
        raise SystemExit("campaign must select at least one family")
    if any(count <= 0 for count in family_case_counts.values()):
        raise SystemExit("all family case counts must be > 0")
    # Reject incompatible opt-ins before creating any campaign artifacts.
    specs = []
    if resume_campaign is None:
        specs = [
            CaseSpec(
                family=family,
                seed=_derived_seed(args.master_seed, family, ordinal),
                ordinal=ordinal,
                blocks=args.blocks,
                iterations=args.iterations,
                max_call_depth=args.max_call_depth,
                branch_history_mode=args.branch_history_mode,
                ras_return_mode=args.ras_return_mode,
                indirect_target_mode=args.indirect_target_mode,
                directed_pattern=args.directed_pattern,
                page_tail_probe_repetitions=args.page_tail_probe_repetitions,
            )
            for family in families
            for ordinal in range(family_case_counts[family])
        ]
        try:
            for spec in specs:
                spec.validate()
        except ValueError as exc:
            raise SystemExit("invalid generator configuration: " + str(exc)) from exc
    source_root = output_root / "sources"
    case_runs_root = output_root / "cases"
    report_root = output_root / "report"
    if resume_campaign is not None:
        source_root.mkdir(parents=True, exist_ok=True)
        case_runs_root.mkdir(parents=True, exist_ok=True)
        report_root.mkdir(parents=True, exist_ok=True)
        campaign = resume_campaign
        records = list(campaign["cases"])
        scenario_specs = {}
        for record in records:
            case_name = str(record.get("name") or "")
            scenario_spec_path = Path(str(record.get("scenario_spec_path") or ""))
            if not case_name or not scenario_spec_path.is_file():
                raise SystemExit(f"resume case is missing scenario spec: {case_name}")
            try:
                scenario_specs[case_name] = scenario_spec_from_dict(
                    json.loads(scenario_spec_path.read_text(encoding="utf-8"))
                )
            except (OSError, ValueError, TypeError) as exc:
                raise SystemExit(f"invalid scenario spec for resume case {case_name}: {exc}") from exc
        try:
            _validate_retained_translation_records(records, scenario_specs, args.link_address)
        except ValueError as exc:
            raise SystemExit(f"resume rejected before artifact changes: {exc}") from exc
        campaign["status"] = "running"
        campaign.pop("completed_at_utc", None)
        campaign["post_run_gates"] = []
        for key in (
            "funcov_comparison_exit_code",
            "funcov_comparison_path",
            "trusted_funcov_exit_code",
            "trusted_funcov_path",
            "coverage_gap_inventory_path",
            "coverage_gap_audit_exit_code",
            "baseline_audit_exit_code",
            "baseline_audit_path",
        ):
            campaign.pop(key, None)
    else:
        if output_root.exists():
            raise SystemExit(f"refusing to reuse campaign root: {args.output_root}")
        source_root.mkdir(parents=True)
        case_runs_root.mkdir()
        report_root.mkdir()

        testpoint_catalog = build_catalog(args.testpoint_source)
        testpoint_catalog_path = output_root / "testpoint_catalog.json"
        write_catalog(testpoint_catalog, testpoint_catalog_path)

        records = write_cases(specs, source_root)
        scenario_specs = {}
        for record in records:
            spec = record["spec"]
            timing_seed = _derived_timing_seed(
                timing_master_seed,
                str(spec["family"]),
                int(spec["ordinal"]),
            )
            scenario_spec = build_scenario_spec(
                record,
                timing_seed=timing_seed,
                testpoint_catalog=testpoint_catalog,
                page_tail_attribute_mode=args.page_tail_attribute_mode,
                uncache_a_stall_cycles=args.uncache_a_stall_cycles,
            )
            scenario_spec_path = source_root / f"{record['name']}.scenario_spec.json"
            write_json_contract(scenario_spec_path, scenario_spec.to_dict())
            scenario_specs[str(record["name"])] = scenario_spec
            record["timing_seed"] = timing_seed
            record["scenario_spec_path"] = str(scenario_spec_path.resolve())
            record["scenario_spec_sha256"] = _sha256(scenario_spec_path)
            record["planned_testpoint_ids"] = list(scenario_spec.testpoint_ids)
            record["risk_tags"] = list(scenario_spec.risk_tags)
            record["generator_replay_command"] = (
                f"python3 {Path(__file__).resolve().parent / 'generate_random_asm.py'} "
                f"--family {spec['family']} --seed {spec['seed']} --count 1 "
                f"--ordinal-start {spec['ordinal']} --blocks {spec['blocks']} "
                f"--iterations {spec['iterations']} --max-call-depth {spec['max_call_depth']} "
                f"--indirect-target-mode {spec['indirect_target_mode']} "
                f"--directed-pattern {spec['directed_pattern']} "
                + (
                    f"--page-tail-probe-repetitions {spec['page_tail_probe_repetitions']} "
                    if spec.get("page_tail_probe_repetitions") is not None
                    else ""
                )
                + (f"--branch-history-mode {spec['branch_history_mode']} " if "branch_history_mode" in spec else "")
                + (f"--ras-return-mode {spec['ras_return_mode']} " if "ras_return_mode" in spec else "")
                + f"--out-dir <replay-output-dir>"
            )
        build_manifest = REPO_ROOT / "build-frontend" / "frontend_build_manifest.verilator.json"
        campaign = {
        "schema_version": 1,
        "campaign_kind": "frontend_random_discovery",
        "evidence_policy": "diagnostic discovery; deterministic replay and ctrl-goal acceptance required before backannotation",
        "created_at_utc": datetime.now(timezone.utc).isoformat(),
        "repo_root": str(REPO_ROOT),
        "repo_head": subprocess.run(
            ["git", "rev-parse", "HEAD"], cwd=REPO_ROOT, check=True, capture_output=True, text=True
        ).stdout.strip(),
        "master_seed": args.master_seed,
        "timing_master_seed": timing_master_seed,
        "families": families,
        "family_case_counts": family_case_counts,
        "planned_case_count": len(records),
        "mode": args.mode,
        "parameters": {
            "count_per_family": args.count,
            "family_case_counts": family_case_counts,
            "blocks": args.blocks,
            "iterations": args.iterations,
            "max_call_depth": args.max_call_depth,
            **({"branch_history_mode": args.branch_history_mode} if args.branch_history_mode != "loop_mask" else {}),
            **({"ras_return_mode": args.ras_return_mode} if args.ras_return_mode != "private" else {}),
            "indirect_target_mode": args.indirect_target_mode,
            "directed_pattern": args.directed_pattern,
            "page_tail_attribute_mode": args.page_tail_attribute_mode,
            "page_tail_probe_repetitions": args.page_tail_probe_repetitions,
            **(
                {"uncache_a_stall_cycles": args.uncache_a_stall_cycles}
                if args.uncache_a_stall_cycles
                else {}
            ),
            "minimum_dynamic_instructions": args.min_dynamic_instructions,
            "link_address": args.link_address,
            "binary_base": args.binary_base,
            "case_timeout_seconds": args.case_timeout_seconds,
            "pytest_timeout_seconds": args.pytest_timeout_seconds,
            "trace_max_cycles": args.trace_max_cycles,
            "waveform": args.waveform,
            "coverage_minimum_hit_rate": args.coverage_minimum_hit_rate,
            "coverage_minimum_qualified_runs": args.coverage_minimum_qualified_runs,
            "require_goal_coverage": args.require_goal_coverage,
        },
        "toolchain": {
            "gcc": _tool_identity(os.getenv("RISCV_GCC", "riscv64-unknown-elf-gcc")),
            "objcopy": _tool_identity(os.getenv("RISCV_OBJCOPY", "riscv64-unknown-elf-objcopy")),
            "objdump": _tool_identity(os.getenv("RISCV_OBJDUMP", "riscv64-unknown-elf-objdump")),
            "python": {"path": sys.executable, "version": sys.version.split()[0]},
        },
        "dut_build_manifest_path": str(build_manifest.resolve()),
        "dut_build_manifest_sha256": _sha256(build_manifest),
        "testpoint_catalog_path": str(testpoint_catalog_path),
        "testpoint_catalog_sha256": _sha256(testpoint_catalog_path),
        "testpoint_catalog_summary": testpoint_catalog["summary"],
        "cases": records,
        "post_run_gates": [],
            "status": "generated",
        }
    _update_case_execution_summary(
        campaign,
        planned_case_count=len(records),
        executed=[record for record in records if record.get("qualification_pass") is not None],
    )
    _atomic_json(manifest_path, campaign)

    if args.mode == "generate":
        print(f"generated={len(records)} campaign={manifest_path}")
        return 0

    nemu_exec = _find_nemu(args.nemu_exec)
    if nemu_exec is None:
        raise SystemExit("NEMU executable not found; pass --nemu-exec")
    if args.mode == "dut" and not (REPO_ROOT / "build-frontend").is_dir():
        raise SystemExit(
            "DUT mode requires a complete build-frontend directory in this isolated worktree; "
            "a read-only symlink to a compatible build is allowed"
        )

    record_by_name = {str(record["name"]): record for record in records}
    # A resumed campaign keeps all prior qualified records and only reruns
    # records without a completed qualification result (including prior failures).
    completed_by_name = {
        str(record["name"]): record
        for record in records
        if record.get("qualification_pass") is not None
    }
    executed: list[dict] = [
        completed_by_name[str(record["name"])]
        for record in records
        if str(record["name"]) in completed_by_name
    ]
    failures = sum(1 for record in executed if not bool(record.get("qualification_pass")))
    pending_records = [
        record
        for record in records
        if record.get("qualification_pass") is not True
    ]
    for record in pending_records:
        case_name = str(record["name"])
        case_root = case_runs_root / case_name
        if args.resume and case_root.exists():
            # The case runner intentionally refuses a non-empty root. Preserve
            # interrupted/failed artifacts under a timestamped name, then let
            # this attempt use a clean root. The old evidence remains recoverable.
            stale_root = case_runs_root / (
                f"{case_name}.stale-"
                f"{datetime.now(timezone.utc).strftime('%Y%m%dT%H%M%S%fZ')}"
            )
            shutil.move(str(case_root), str(stale_root))
            print(f"[random-campaign] preserved stale artifacts: {stale_root}", flush=True)
        program_map_path = case_root / "inputs" / f"{case_name}.program_map.json"
        timeline_path = case_root / "inputs" / f"{case_name}.scenario_timeline.json"
        binary_path = case_root / "inputs" / f"{case_name}.bin"
        seed = int(record["spec"]["seed"])
        timing_seed = int(record["timing_seed"])
        env = os.environ.copy()
        env.update(
            {
                "NEMU_EXEC": str(nemu_exec),
                "TB_NEMU_EXEC": str(nemu_exec),
                "PYTHON": sys.executable,
                "TB_RUN_DUT": "1" if args.mode == "dut" else "0",
                "TB_RUN_ID": case_name,
                "TB_ARTIFACT_DIR": str(case_root),
                "TB_COVERAGE_DIR": str(case_root / "coverage"),
                "TB_WAVEFORM_DIR": str(case_root / "waveforms"),
                "TB_FUNCOV_DIR": str(case_root / "funcov"),
                "TB_CASE_LOG_DIR": str(case_root / "logs"),
                "TB_ASM_BIN_PATH": str(binary_path),
                "TB_FUNCOV_TARGET_TESTCASES": "",
                "TB_PYTEST_TARGET": str(RANDOM_DUT_TEST),
                "TB_RANDOM_GENERATOR_MANIFEST": str(record["generator_manifest_path"]),
                "TB_RANDOM_SCENARIO_SPEC": str(record["scenario_spec_path"]),
                "TB_RANDOM_PROGRAM_MAP_PATH": str(program_map_path),
                "TB_RANDOM_TIMELINE_PATH": str(timeline_path),
                "TB_RANDOM_COORD_CONTRACT": str(args.coord_contract.resolve()) if args.coord_contract else "",
                "TB_RANDOM_COORD_CONTRACT_SHA256": str(args.coord_contract_sha256 or ""),
                "TB_SEED": str(seed),
                "TB_BACKEND_RANDOM_SEED": str(timing_seed),
                "TB_ENABLE_FST_DUMP": "1" if args.waveform else "0",
                "TB_TRACE_MAX_CYCLES": str(args.trace_max_cycles),
                "TB_PYTEST_TIMEOUT_SECS": str(args.pytest_timeout_seconds),
                "PYTEST_DISABLE_PLUGIN_AUTOLOAD": "1",
                "LINK_ADDR": hex(args.link_address),
                "NEMU_MEM_BASE": hex(args.binary_base),
                "TB_BASE_ADDR": hex(args.binary_base),
                "TB_RESET_VECTOR": hex(args.link_address),
                "TB_RUN_COMMAND": (
                    f"TB_SEED={seed} TB_BACKEND_RANDOM_SEED={timing_seed} "
                    f"TB_ASM_BIN_PATH={binary_path} "
                    f"bash {RUN_CASE_SCRIPT} {record['source_path']}"
                ),
            }
        )
        print(
            f"[random-campaign] RUN {case_name} program_seed={seed} "
            f"timing_seed={timing_seed} mode={args.mode}",
            flush=True,
        )
        status, exit_code = _run_with_timeout(
            ["bash", str(RUN_CASE_SCRIPT), str(record["source_path"])],
            env,
            args.case_timeout_seconds,
        )
        executed_record = _case_record_after_run(
            record,
            scenario_specs[case_name],
            case_root,
            status,
            exit_code,
            args.min_dynamic_instructions,
            args.mode,
            args.link_address,
            args.binary_base,
            binary_path,
        )
        completed_by_name[case_name] = executed_record
        executed = [
            completed_by_name[str(item["name"])]
            for item in records
            if str(item["name"]) in completed_by_name
        ]
        if not executed_record["qualification_pass"]:
            failures += 1
            reasons = []
            configuration = executed_record["configuration_preflight"]
            if configuration["status"] not in {"valid", "not_applicable"}:
                reasons.append("configuration_preflight=" + ",".join(configuration["reasons"]))
            if exit_code != 0:
                reasons.append(f"status={status} exit={exit_code}")
            if not executed_record["dynamic_instruction_requirement_met"]:
                reasons.append(
                    "dynamic_trace_too_short "
                    f"got={executed_record['dynamic_trace_instructions']} "
                    f"required={args.min_dynamic_instructions}"
                )
            if executed_record["program_map_status"] != "valid":
                reasons.append(
                    f"program_map={executed_record['program_map_status']} "
                    f"error={executed_record['program_map_error']}"
                )
            if executed_record["scenario_realization_status"] != "REALIZED":
                reasons.append(
                    f"scenario_realization={executed_record['scenario_realization_status']}"
                )
            if args.mode == "dut" and executed_record["scenario_timeline_status"] != "valid":
                reasons.append(
                    "scenario_timeline="
                    + executed_record["scenario_timeline_status"]
                    + " reasons="
                    + ",".join(executed_record["scenario_timeline_reasons"])
                )
            if args.mode == "dut" and executed_record["functional_coverage_status"] != "eligible":
                reasons.append(
                    "functional_coverage="
                    + executed_record["functional_coverage_status"]
                    + " reasons="
                    + ",".join(executed_record["functional_coverage_reasons"])
                )
            reason = "; ".join(reasons) or "qualification_failed"
            print(f"[random-campaign] FAIL {case_name} {reason}", flush=True)
        else:
            print(
                f"[random-campaign] PASS {case_name} trace_instructions="
                f"{executed_record['dynamic_trace_instructions']}",
                flush=True,
            )
        campaign["cases"] = [
            completed_by_name.get(str(item["name"]), item)
            for item in records
        ]
        _update_case_execution_summary(
            campaign,
            planned_case_count=len(records),
            executed=executed,
        )
        campaign["failure_count"] = failures
        campaign["status"] = "running"
        _atomic_json(manifest_path, campaign)
        if failures and args.stop_on_fail:
            break

    campaign["cases"] = [
        completed_by_name.get(str(item["name"]), item)
        for item in records
    ]
    _update_case_execution_summary(
        campaign,
        planned_case_count=len(records),
        executed=executed,
    )
    campaign["completed_at_utc"] = datetime.now(timezone.utc).isoformat()
    random_funcov = [
        Path(path)
        for record in executed
        if record["qualification_pass"]
        for path in record["eligible_functional_coverage_artifacts"]
    ]
    native_gate_arguments = []
    if args.mode == "dut" and random_funcov:
        try:
            from .native_runtime_binding import write_admission_index
        except ImportError:
            from native_runtime_binding import write_admission_index
        evidence_index = report_root / ("native-evidence-index-"
            + datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%S%fZ") + ".json")
        evidence_pin = write_admission_index(executed, evidence_index)
        native_gate_arguments = ["--native-evidence-index", str(evidence_index),
                                 "--native-evidence-index-sha256", evidence_pin]
        campaign["native_evidence_index"] = {"path": str(evidence_index.resolve()),
                                              "sha256": evidence_pin}
    if random_funcov:
        compare_command = [
            sys.executable,
            str(COMPARE_SCRIPT),
            "--output",
            str(report_root / "random_vs_directed_funcov.json"),
            "--minimum-hit-rate",
            str(args.coverage_minimum_hit_rate),
            "--minimum-qualified-runs",
            str(args.coverage_minimum_qualified_runs),
        ]
        compare_command.extend(native_gate_arguments)
        if args.require_goal_coverage:
            compare_command.append("--require-qualified-union")
        for artifact in random_funcov:
            compare_command.extend(("--random-artifact", str(artifact)))
        for baseline_root in args.baseline_root:
            compare_command.extend(("--baseline-root", str(baseline_root)))
        compare = subprocess.run(compare_command, check=False)
        campaign["funcov_comparison_exit_code"] = compare.returncode
        campaign["funcov_comparison_path"] = str(
            (report_root / "random_vs_directed_funcov.json").resolve()
        )
        _record_post_run_gate(
            campaign,
            gate_id="random_vs_directed_funcov",
            exit_code=compare.returncode,
        )
    else:
        _record_post_run_gate(
            campaign,
            gate_id="random_vs_directed_funcov",
            exit_code=None,
        )
    _atomic_json(manifest_path, campaign)
    if random_funcov:
        trusted_command = [
            sys.executable,
            str(TRUSTED_FUNCOV_SCRIPT),
            "--campaign",
            str(manifest_path),
            "--output",
            str(report_root / "trusted_funcov.json"),
            "--require-complete",
            "--minimum-hit-rate",
            str(args.coverage_minimum_hit_rate),
            "--minimum-qualified-runs",
            str(args.coverage_minimum_qualified_runs),
        ]
        trusted_command.extend(native_gate_arguments)
        if args.require_goal_coverage:
            trusted_command.append("--require-qualified-union")
        trusted = subprocess.run(trusted_command, check=False)
        campaign["trusted_funcov_exit_code"] = trusted.returncode
        campaign["trusted_funcov_path"] = str(
            (report_root / "trusted_funcov.json").resolve()
        )
        _record_post_run_gate(
            campaign,
            gate_id="trusted_funcov",
            exit_code=trusted.returncode,
        )
        _atomic_json(manifest_path, campaign)
        gap_inventory_path = report_root / "coverage_gap_inventory.json"
        gap_audit = subprocess.run(
            [
                sys.executable,
                str(GAP_AUDIT_SCRIPT),
                "--campaign",
                str(manifest_path),
                "--output",
                str(gap_inventory_path),
                "--minimum-hit-rate",
                str(args.coverage_minimum_hit_rate),
                "--minimum-qualified-runs",
                str(args.coverage_minimum_qualified_runs),
                *native_gate_arguments,
            ],
            check=False,
        )
        campaign["coverage_gap_inventory_path"] = str(gap_inventory_path.resolve())
        campaign["coverage_gap_audit_exit_code"] = gap_audit.returncode
        _record_post_run_gate(
            campaign,
            gate_id="coverage_gap_audit",
            exit_code=gap_audit.returncode,
        )
        _atomic_json(manifest_path, campaign)
    else:
        _record_post_run_gate(
            campaign,
            gate_id="trusted_funcov",
            exit_code=None,
        )
        _record_post_run_gate(
            campaign,
            gate_id="coverage_gap_audit",
            exit_code=None,
        )
    audit = subprocess.run(
        [
            sys.executable,
            str(BASELINE_AUDIT_SCRIPT),
            "--campaign",
            str(manifest_path),
            "--output",
            str(report_root / "baseline_audit.json"),
            "--require-complete",
        ],
        check=False,
    )
    campaign["baseline_audit_exit_code"] = audit.returncode
    campaign["baseline_audit_path"] = str(
        (report_root / "baseline_audit.json").resolve()
    )
    _record_post_run_gate(
        campaign,
        gate_id="baseline_audit",
        exit_code=audit.returncode,
    )
    failures = _finalize_campaign_status(campaign)
    _atomic_json(manifest_path, campaign)
    print(
        f"campaign_status={campaign['status']} cases={len(executed)} failures={failures} "
        f"manifest={manifest_path}"
    )
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
