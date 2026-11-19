"""Freeze real sampler/build inputs before replay; bind completed native reports.

This does not generate coverage, assign a DUT slot, or upgrade historical cases.
The test's real fixture supplies definitions/provenance, never report filenames.
"""
from __future__ import annotations

from dataclasses import asdict, is_dataclass
import hashlib
import json
from pathlib import Path


COORD_FIELDS = {
    "source": "dut_source_sha", "implementation": "implementation_sha",
    "design": "design_baseline_sha", "manifest_sha256": "build_manifest_sha256",
    "build": "dut_build_sha256", "signal": "signal_contract_sha256",
    "sampler": "sampler_sha256", "registry": "registry_sha256",
    "environment": "verification_env_sha256",
}
BUILD_FIELDS = ("dut_source_sha", "implementation_sha", "design_baseline_sha",
                "build_manifest_sha256", "dut_build_sha256", "dut_python_extension_sha256",
                "generated_rtl_sha256", "signal_contract_sha256")


def sha(path: Path) -> str:
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def json_sha(value) -> str:
    return hashlib.sha256(json.dumps(value, ensure_ascii=False, sort_keys=True,
                                    separators=(",", ":")).encode()).hexdigest()


def write_exclusive(path: Path, value: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("x", encoding="utf-8") as stream:
        json.dump(value, stream, ensure_ascii=False, sort_keys=True, indent=2)
        stream.write("\n")


def inventory(frontend_root: Path) -> dict[str, str]:
    paths = set()
    for subdir in ("env", "tests/random_case"):
        paths.update(p for p in (frontend_root / subdir).rglob("*")
                     if p.is_file() and p.suffix in {".py", ".sv", ".csv", ".yaml", ".json"}
                     and "__pycache__" not in p.parts)
    paths.update(frontend_root / rel for rel in (
        "Frontend_api.py", "Frontend_env.py", "conftest.py", "tests/conftest.py",
        "docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv"))
    return {str(p.relative_to(frontend_root)): sha(p) for p in sorted(paths) if p.is_file()}


def validate_coord_pin(path: Path, pin: str) -> dict:
    if len(pin) != 64 or sha(path) != pin:
        raise ValueError("coord contract SHA256 missing or changed")
    result = json.loads(path.read_text())
    if not isinstance(result, dict):
        raise ValueError("coord contract must be an object")
    return result


def _load_build(contract: dict) -> dict:
    from env.runtime.artifact_provenance import load_frontend_build_manifest
    p = contract["provenance"]
    return load_frontend_build_manifest(Path(contract["build_root"]),
                                       Path(p["build_manifest_path"]), simulator=p["simulator"])


def freeze_runtime_contract(env, *, output: Path, coord_contract: Path,
                            coord_sha256: str, run_id: str, input_paths: dict,
                            minimum_dynamic_instructions: int,
                            frontend_root: Path | None = None) -> str:
    from env.funcov.registry import _frontend_root
    frontend_root = Path(frontend_root or _frontend_root()).resolve()
    coord = validate_coord_pin(coord_contract, coord_sha256)
    hub = env.functional_coverage
    if hub is None:
        raise ValueError("native sampler fixture unavailable")
    definitions = [asdict(d) if is_dataclass(d) else dict(d) for d in hub.definitions]
    provenance = dict(hub.provenance)
    if provenance.get("build_manifest_status") != "valid":
        raise ValueError("real fixture build manifest is not valid")
    if provenance.get("definitions_sha256") != json_sha(definitions):
        raise ValueError("real fixture definitions hash mismatch")
    for field, source in COORD_FIELDS.items():
        if not coord.get(field) or coord[field] != provenance.get(source):
            raise ValueError("coord/real fixture provenance mismatch: " + field)
    if type(minimum_dynamic_instructions) is not int or minimum_dynamic_instructions < 5000:
        raise ValueError("native qualification requires at least 5000 committed instructions")
    if set(input_paths) != {"asm", "bin", "trace", "generator", "scenario"}:
        raise ValueError("native freeze requires exact asm/bin/trace/generator/scenario input set")
    inputs = {kind: {"path": str(Path(path).resolve()), "sha256": sha(Path(path))}
              for kind, path in input_paths.items()}
    contract = {"schema_version": 2, "scope": "actual fixture before random replay; no canonical HIT",
                "run_id": run_id, "definitions": definitions, "provenance": provenance,
                "minimum_dynamic_instructions": minimum_dynamic_instructions,
                "coord_contract": {"path": str(coord_contract.resolve()), "sha256": coord_sha256},
                "frontend_root": str(frontend_root),
                "build_root": str(frontend_root.parents[3] / "build-frontend"),
                "runtime_files_sha256": inventory(frontend_root), "inputs": inputs}
    build = _load_build(contract)
    if build.get("build_manifest_status") != "valid":
        raise ValueError("native freeze loader not valid")
    for field in BUILD_FIELDS:
        if build.get(field) != provenance.get(field):
            raise ValueError("loader/real fixture provenance mismatch: " + field)
    write_exclusive(output, contract)
    return sha(output)


def verify_runtime_contract(path: Path, pin: str) -> dict:
    if len(pin) != 64 or sha(path) != pin:
        raise ValueError("pre-run native contract missing or changed")
    contract = json.loads(path.read_text())
    if contract.get("schema_version") != 2:
        raise ValueError("expected a pre-run native contract")
    coord_ref = contract["coord_contract"]
    coord = validate_coord_pin(Path(coord_ref["path"]), coord_ref["sha256"])
    provenance = contract["provenance"]
    for field, source in COORD_FIELDS.items():
        if coord.get(field) != provenance.get(source):
            raise ValueError("coord/native provenance mismatch: " + field)
    if json_sha(contract["definitions"]) != provenance["definitions_sha256"]:
        raise ValueError("native definitions changed")
    if inventory(Path(contract["frontend_root"])) != contract["runtime_files_sha256"]:
        raise ValueError("runtime source changed during replay")
    for kind, item in contract["inputs"].items():
        if sha(Path(item["path"])) != item["sha256"]:
            raise ValueError("native input changed during replay: " + kind)
    build = _load_build(contract)
    if build.get("build_manifest_status") != "valid":
        raise ValueError("native post-run loader not valid")
    for field in BUILD_FIELDS:
        if build.get(field) != provenance.get(field):
            raise ValueError("native build changed during replay: " + field)
    return contract


def _runtime_timeline(timeline: dict, contract: dict) -> None:
    details = timeline["details"]
    if details.get("native_runtime_postcheck") is not True:
        raise ValueError("native runtime postcheck not proved")
    if contract["run_id"] != timeline["scenario_id"]:
        raise ValueError("native pre-run/timeline identity mismatch")
    if (details.get("completed") is not True or type(details.get("pending_work")) is not int
            or details["pending_work"] != 0):
        raise ValueError("native runtime completion/pending work not proved")


def bind_native_reports(paths: list[Path], *, timeline_path: Path,
                        output_dir: Path) -> tuple[dict, dict]:
    timeline = json.loads(timeline_path.read_text())
    details = timeline["details"]
    reference = details["native_runtime_contract"]
    contract_path = Path(reference["path"])
    contract = verify_runtime_contract(contract_path, reference["sha256"])
    _runtime_timeline(timeline, contract)
    bindings = {}
    for index, path in enumerate(paths):
        raw = json.loads(path.read_text())
        if "groups" not in raw:
            raise ValueError("current runtime requires native Toffee report; legacy is diagnostic only")
        metadata = raw["run_metadata"]
        if metadata["run_id"] != contract["run_id"]:
            raise ValueError("native report/pre-run identity mismatch")
        execution = metadata["execution"]
        for kind in ("asm", "bin", "trace"):
            if (execution.get(kind + "_sha256") != contract["inputs"][kind]["sha256"]
                    or Path(execution[kind + "_path"]).resolve() != Path(contract["inputs"][kind]["path"])):
                raise ValueError("native report/pre-run input mismatch: " + kind)
        sidecar = {"schema_version": 1, "artifact_sha256": sha(path),
                   "run_id": contract["run_id"], "frozen_contract": reference,
                   "scenario_timeline": {"path": str(timeline_path.resolve()), "sha256": sha(timeline_path)}}
        sidecar_path = output_dir / f"native-{index}.sidecar.json"
        write_exclusive(sidecar_path, sidecar)
        bindings[str(path.resolve())] = {"sidecar_path": str(sidecar_path.resolve()),
                                        "sidecar_sha256": sha(sidecar_path)}
    if not bindings:
        raise ValueError("native report missing")
    return bindings, contract["provenance"]


# This index pins immutable case evidence rather than the campaign JSON, whose
# post-gate bookkeeping changes. It cannot upgrade an unqualified original case.
def write_admission_index(records: list[dict], output: Path) -> str:
    artifacts = {}
    for record in records:
        if record.get("qualification_pass") is not True:
            continue
        admission = record.get("native_admission")
        if not isinstance(admission, dict) or admission.get("scope") != "current_native":
            raise ValueError("qualified current case has no native admission")
        for raw_path in record["eligible_functional_coverage_artifacts"]:
            path = str(Path(raw_path).resolve())
            binding = admission["bindings"][path]
            item = {**binding, "expected_provenance": admission["expected_provenance"]}
            if path in artifacts and artifacts[path] != item:
                raise ValueError("duplicate native artifact with conflicting admission")
            artifacts[path] = item
    if not artifacts:
        raise ValueError("no qualified native case evidence for post-run gates")
    write_exclusive(output, {"schema_version": 1, "scope": "current_native",
                             "canonical_hit_eligible": False, "artifacts": artifacts})
    return sha(output)


def load_admission_index(path: Path | None, pin: str | None) -> dict | None:
    if path is None and pin is None:
        return None
    if path is None or not pin or sha(path) != pin:
        raise ValueError("native evidence index requires an explicit matching SHA256")
    result = json.loads(path.read_text())
    if (result.get("schema_version") != 1 or result.get("scope") != "current_native"
            or result.get("canonical_hit_eligible") is not False
            or not isinstance(result.get("artifacts"), dict) or not result["artifacts"]):
        raise ValueError("invalid current native evidence index")
    return result


def mark_coverage_report_scope(report: dict, *, current_native: bool) -> None:
    """Keep historical threshold calculations separate from current admission."""
    report["admission_scope"] = "current_native" if current_native else "historical_diagnostic_only"
    report["canonical_hit_eligible"] = False
    if not current_native:
        qualification = report["qualification"]
        qualification["historical_thresholds_met"] = qualification["pass"]
        qualification["pass"] = False
        qualification["reasons"] = [*qualification["reasons"], "current_native_evidence_index_missing"]


def current_metadata(path: Path, index: dict) -> dict:
    item = index["artifacts"].get(str(path.resolve()))
    if item is None:
        raise ValueError("report is not in the pinned native evidence index")
    sidecar_path = Path(item["sidecar_path"])
    if sha(sidecar_path) != item["sidecar_sha256"]:
        raise ValueError("native sidecar changed before post-run gate")
    binding = json.loads(sidecar_path.read_text())
    if binding["artifact_sha256"] != sha(path):
        raise ValueError("native artifact changed before post-run gate")
    reference = binding["frozen_contract"]
    frozen_path = Path(reference["path"])
    if not frozen_path.is_absolute():
        frozen_path = sidecar_path.parent / frozen_path
    contract = verify_runtime_contract(frozen_path, reference["sha256"])
    if contract["provenance"] != item["expected_provenance"]:
        raise ValueError("native current post-gate provenance mismatch")
    timeline_ref = binding["scenario_timeline"]
    timeline_path = Path(timeline_ref["path"])
    if not timeline_path.is_absolute():
        timeline_path = sidecar_path.parent / timeline_path
    if sha(timeline_path) != timeline_ref["sha256"]:
        raise ValueError("native timeline changed before post-run gate")
    _runtime_timeline(json.loads(timeline_path.read_text()), contract)
    return contract


def load_current_artifact(path: Path, index: dict):
    try:
        from .funcov_compare import load_artifact
    except ImportError:
        from funcov_compare import load_artifact
    current_metadata(path, index)
    item = index["artifacts"][str(path.resolve())]
    if "groups" not in json.loads(path.read_text()):
        raise ValueError("legacy artifact cannot enter current native admission")
    artifact = load_artifact(path, native_sidecar=Path(item["sidecar_path"]),
                             native_sidecar_sha256=item["sidecar_sha256"])
    if not artifact.eligible:
        raise ValueError("current native report is ineligible: " + str(artifact.eligibility_reason))
    return artifact


def current_source_loader(path: Path, index: dict):
    contract = current_metadata(path, index)
    frontend_root = Path(contract["frontend_root"])
    hashes = contract["runtime_files_sha256"]
    prefix = "src/test/python/Frontend/"
    def load(relative_path: str) -> str:
        if not relative_path.startswith(prefix):
            raise ValueError("sampler source is outside frozen Frontend inventory")
        key = relative_path[len(prefix):]
        source = frontend_root / key
        if key not in hashes or sha(source) != hashes[key]:
            raise ValueError("sampler source is not hash-pinned to replay")
        return source.read_text()
    return load
