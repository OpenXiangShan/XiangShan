"""Read native Toffee reports without rewriting them or inferring strict IDs.

Only an explicitly supplied, hash-pinned coord sidecar can bind a report to
frozen definitions/provenance and a completed scenario timeline. Qualification
here admits an artifact to random comparison; exact-target/canonical HIT review
is still separate. No sibling file is automatically trusted or discovered.
"""
from __future__ import annotations

import hashlib
import json
from pathlib import Path


def _sha(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _json_sha(value: object) -> str:
    return hashlib.sha256(json.dumps(
        value, ensure_ascii=False, sort_keys=True, separators=(",", ":"),
    ).encode()).hexdigest()


def _integer(value: object, *, minimum: int = 0) -> bool:
    return type(value) is int and value >= minimum


def _key(item: dict) -> tuple[str, str, str]:
    result = tuple(item.get(k) for k in ("coverage_group", "coverpoint", "bin_name"))
    if not all(isinstance(v, str) and v.strip() for v in result):
        raise ValueError("native definition requires full group/point/bin identity")
    return result


def _bound_json(reference: dict, parent: Path) -> dict:
    path = Path(reference["path"])
    if not path.is_absolute():
        path = parent / path
    if _sha(path) != reference["sha256"]:
        raise ValueError("bound file hash mismatch: " + str(path))
    result = json.loads(path.read_text())
    if not isinstance(result, dict):
        raise ValueError("bound JSON must be an object")
    return result


def load_native_artifact(path: Path, raw: dict, *, sidecar: Path | None = None,
                         sidecar_sha256: str | None = None):
    # Runtime import avoids a cycle with funcov_compare.load_artifact.
    try:
        from .funcov_compare import Artifact
    except ImportError:
        from funcov_compare import Artifact

    reasons: list[str] = []
    counts: dict[tuple[str, str, str], int] = {}
    try:
        if not isinstance(raw["groups"], list) or not raw["groups"]:
            raise ValueError("native groups missing")
        for group in raw["groups"]:
            for point in group["points"]:
                if point.get("dynamic_bin") is not False:
                    raise ValueError("native dynamic/unknown point cannot be mapped")
                for item in point["bins"]:
                    key = _key({"coverage_group": group["name"],
                                "coverpoint": point["name"], "bin_name": item["name"]})
                    if key in counts or not _integer(item.get("hints")):
                        raise ValueError("duplicate native identity or invalid hints")
                    counts[key] = item["hints"]
        if not counts or raw.get("bin_num_total") != len(counts):
            raise ValueError("native inventory count mismatch")
    except (AttributeError, KeyError, TypeError, ValueError) as exc:
        reasons.append("native_schema_invalid=" + str(exc))

    metadata = raw.get("run_metadata")
    metadata = metadata if isinstance(metadata, dict) else {}
    execution = metadata.get("execution")
    execution = execution if isinstance(execution, dict) else {}
    checker = metadata.get("checker")
    checker = checker if isinstance(checker, dict) else {}
    if (metadata.get("outcome") not in ("pass", "passed")
            or type(metadata.get("exit_code")) is not int or metadata["exit_code"] != 0):
        reasons.append("native_outcome_not_passed")
    if (checker.get("status") != "pass" or type(checker.get("error_count")) is not int
            or checker["error_count"] != 0 or checker.get("errors") != []):
        reasons.append("native_checker_not_proved_clean")
    run_id = metadata.get("run_id")
    if not isinstance(run_id, str) or not run_id.strip():
        reasons.append("native_run_id_missing")

    definitions: list[dict] = []
    provenance: dict = {}
    mapped: dict[tuple[str, str, str], str] = {}
    if sidecar is None:
        reasons.append("native_frozen_sidecar_missing")
    else:
        try:
            if not sidecar_sha256 or _sha(sidecar) != sidecar_sha256:
                raise ValueError("native sidecar must match explicitly pinned SHA256")
            binding = json.loads(sidecar.read_text())
            if not isinstance(binding, dict):
                raise ValueError("native sidecar must be an object")
            if binding.get("schema_version") != 1:
                raise ValueError("unsupported native sidecar schema")
            if binding["artifact_sha256"] != _sha(path) or binding["run_id"] != run_id:
                raise ValueError("native artifact/run binding mismatch")
            frozen = _bound_json(binding["frozen_contract"], sidecar.parent)
            definitions = frozen["definitions"]
            frozen_provenance = frozen["provenance"]
            if not isinstance(frozen_provenance, dict):
                raise ValueError("frozen provenance must be an object")
            provenance = frozen_provenance
            if not isinstance(definitions, list) or not definitions:
                raise ValueError("frozen definitions missing")
            ids: set[str] = set()
            for item in definitions:
                key = _key(item)
                bin_id = item.get("bin_id")
                if (key in mapped or not isinstance(bin_id, str) or not bin_id.strip()
                        or bin_id in ids):
                    raise ValueError("duplicate/empty strict mapping identity")
                mapped[key] = bin_id
                ids.add(bin_id)
            if set(mapped) != set(counts):
                raise ValueError("native/strict definition sets differ")
            if provenance.get("definitions_sha256") != _json_sha(definitions):
                raise ValueError("frozen definition hash mismatch")
            if provenance.get("build_manifest_status") != "valid":
                raise ValueError("frozen manifest is not valid")
            for field in ("compatibility_signature", "build_manifest_sha256", "dut_build_sha256",
                          "dut_python_extension_sha256", "generated_rtl_sha256",
                          "signal_contract_sha256", "registry_sha256", "sampler_sha256",
                          "verification_env_sha256"):
                value = provenance.get(field)
                if (not isinstance(value, str) or len(value) != 64
                        or any(c not in "0123456789abcdef" for c in value)):
                    raise ValueError("frozen provenance missing/invalid: " + field)
            source = provenance.get("dut_source_sha")
            if (not isinstance(source, str) or len(source) != 40
                    or any(c not in "0123456789abcdef" for c in source)):
                raise ValueError("frozen source SHA missing/invalid")
            for kind in ("asm", "bin", "trace"):
                input_path = Path(execution[kind + "_path"])
                if _sha(input_path) != execution[kind + "_sha256"]:
                    raise ValueError("native execution input changed: " + kind)
            timeline = _bound_json(binding["scenario_timeline"], sidecar.parent)
            details = timeline["details"]
            seeds = execution["seeds"]
            if not _integer(seeds.get("test")) or not _integer(seeds.get("backend")):
                raise ValueError("native seed pair missing/invalid")
            if (timeline["scenario_id"] != run_id
                    or timeline["program_map_binary_sha256"] != execution["bin_sha256"]
                    or timeline["timing_plan"]["timing_seed"] != seeds["backend"]):
                raise ValueError("native timeline identity mismatch")
            if (timeline["execution_status"] != "REALIZED"
                    or type(timeline.get("dropped_timeline_events")) is not int
                    or timeline["dropped_timeline_events"] != 0
                    or details.get("error") or details.get("error_type")):
                raise ValueError("native timeline not successfully completed")
            for field in ("monitor_errors", "translation_oracle_error_count"):
                if type(details.get(field)) is not int or details[field] != 0:
                    raise ValueError("native health missing/failed: " + field)
            if details.get("translation_oracle_errors") != [] or details.get("translation_oracle_stats_error"):
                raise ValueError("native oracle errors unknown/nonempty")
            minimum = frozen.get("minimum_dynamic_instructions")
            if (not _integer(minimum, minimum=5000)
                    or not _integer(details.get("golden_entries"), minimum=minimum)
                    or not _integer(details.get("golden_cursor"), minimum=details["golden_entries"])):
                raise ValueError("native golden completion/density not proved")
        except (AttributeError, OSError, KeyError, TypeError, ValueError) as exc:
            reasons.append("native_binding_invalid=" + str(exc))
    # Preserve raw counts for diagnosis even when strict IDs cannot be mapped.
    # Never fabricate BIN IDs or put an unqualified hit into the random union.
    complete_mapping = bool(mapped) and set(mapped) == set(counts)
    descriptions = {bin_id: dict(zip(
        ("coverage_group", "coverpoint", "bin_name"), key,
    )) for key, bin_id in mapped.items()} if complete_mapping else {}
    seeds = execution.get("seeds")
    seeds = seeds if isinstance(seeds, dict) else {}
    return Artifact(
        path=path.resolve(), run_id=str(run_id or path.stem),
        compatibility_signature=str(provenance.get("compatibility_signature", "")),
        definitions_sha256=str(provenance.get("definitions_sha256", "")),
        definition_bin_ids=frozenset(descriptions),
        hit_bin_ids=frozenset(mapped[k] for k, n in counts.items() if n > 0)
        if complete_mapping else frozenset(),
        bin_descriptions=descriptions, eligible=not reasons,
        eligibility_reason=",".join(reasons) if reasons else "eligible",
        program_seed=seeds.get("test") if _integer(seeds.get("test")) else None,
        timing_seed=seeds.get("backend") if _integer(seeds.get("backend")) else None,
        replay_command=str(execution.get("run_command") or ""),
        native_bin_count=len(counts), native_hit_bin_count=sum(n > 0 for n in counts.values()),
        admission_scope="native_frozen_contract",
    )
