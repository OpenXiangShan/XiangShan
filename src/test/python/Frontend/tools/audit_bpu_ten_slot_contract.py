#!/usr/bin/env python3
"""Offline #6587 native inventory/source gate; never creates or clocks a DUT."""
from __future__ import annotations

import argparse
import json
from pathlib import Path
import subprocess
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from env.funcov.registry import decode_signal_inventory_name
from env.runtime.artifact_provenance import file_sha256, load_frontend_build_manifest
from env.support.bpu_v3_contract import BPU_V3_SIGNAL_GROUPS
from env.support.bpu_vbtb_contract import VBTB_SIGNAL_GROUPS


def read_inventory(path: Path) -> dict[str, dict]:
    records = {}
    current = None
    for line in path.read_text().splitlines():
        if line.startswith("  - name: "):
            name = decode_signal_inventory_name(line[10:])
            current = records.setdefault(name, {})
        elif current is not None and line.startswith("    ") and ": " in line:
            key, value = line.strip().split(": ", 1)
            current[key] = decode_signal_inventory_name(value)
    return records


def _native_offset(record: dict, width: int):
    if record.get("kind") not in {None, "direct"} or record.get("rtl_width") != str(width):
        return None
    offset = record.get("offset")
    # Inventory offsets are byte addresses, never expressions or negative values.
    return offset if offset is not None and str(offset).isdigit() else None


def resolve_native(records: dict[str, dict], paths: tuple[str, ...], width: int) -> dict:
    for path in paths:
        if "__Vtogcov__" in path or path not in records:
            continue
        record = records[path]
        if record.get("kind") == "expr" or record.get("rtl_width") != str(width):
            continue
        source = record.get("source", path)
        if record.get("kind") == "projection":
            # Only the exact retained RTL reduction is allowed; no arbitrary
            # expression, unresolved dtype, or toggle-state source can qualify.
            expr = f"0U != (IData)({source})"
            leaf = records.get(source, {})
            if (width == 1 and record.get("rtl_width") == "1"
                    and record.get("expr") == expr and "__Vtogcov__" not in source
                    and leaf.get("source", source) == source
                    and _native_offset(leaf, 16) is not None):
                return {"path": path, "source": source, "width": 1,
                        "source_width": 16, "offset": leaf["offset"],
                        "projection": "nonzero", "expr": expr}
            continue
        visited = set()
        while source in records and source not in visited:
            visited.add(source)
            node = records[source]
            if ("__Vtogcov__" in source or node.get("kind") not in {None, "direct"}
                    or node.get("rtl_width") != str(width)):
                break
            next_source = node.get("source", source)
            if next_source != source:
                source = next_source
                continue
            offset = _native_offset(node, width)
            if offset is not None:
                return {"path": path, "source": source, "width": width, "offset": offset}
            break
    return {"missing_native_probe": list(paths), "required_width": width}


def audit(inventory: Path, manifest: Path, compatibility: Path) -> dict:
    root = Path(__file__).resolve().parents[5]
    head = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=root, text=True).strip()
    actual = json.loads(manifest.read_text())
    ledger = json.loads(compatibility.read_text())
    runtime = load_frontend_build_manifest(manifest.parent, manifest, simulator="verilator")
    errors = list(runtime["build_manifest_reasons"])
    if ledger.get("frozen_verification_sha") != head:
        errors.append("compatibility_verification_sha_mismatch")
    if ledger.get("actual_dut_source_sha") != actual.get("dut_source_sha"):
        errors.append("compatibility_dut_source_mismatch")
    if ledger.get("actual_implementation_sha") != actual.get("implementation_sha"):
        errors.append("compatibility_implementation_mismatch")
    if ledger.get("design_baseline_sha") != actual.get("design_baseline_sha"):
        errors.append("compatibility_design_mismatch")
    if ledger.get("manifest_sha256") != file_sha256(manifest):
        errors.append("compatibility_manifest_hash_mismatch")
    if file_sha256(inventory) != actual["artifacts"].get("signal_contract_sha256"):
        errors.append("inventory_hash_mismatch")
    groups = {**{f"bpu_{k}": v for k, v in BPU_V3_SIGNAL_GROUPS.items()},
              **{f"vbtb_{k}": v for k, v in VBTB_SIGNAL_GROUPS.items()}}
    records = read_inventory(inventory)
    projections = {}
    for key, paths in groups.items():
        if key.endswith(("_state",)):
            width = 15
        elif key.endswith(("_selected", "_s2_selected", "_snapshot_mask", "_entry_mask", "_counter_mask", "_victim")):
            width = 16
        elif "branch_type_" in key:
            width = 2
        elif key.startswith("vbtb_sc_way_"):
            width = 3
        elif key.endswith(("_pred_way", "_train_way")):
            width = 4
        elif key.endswith(("_prediction_position", "_meta_position")):
            width = 5
        elif key.endswith("_prediction_target"):
            width = 50
        else:
            width = 1
        projections[key] = resolve_native(records, paths, width)
    missing = {k: v for k, v in projections.items() if "missing_native_probe" in v}
    return {"kind": "offline_bpu_ten_slot_contract", "verification_sha": head,
            "dut_source_sha": actual["dut_source_sha"], "implementation_sha": actual["implementation_sha"],
            "design_baseline_sha": actual["design_baseline_sha"],
            "manifest": str(manifest), "manifest_sha256": file_sha256(manifest),
            "manifest_status": runtime["build_manifest_status"],
            "compatibility_ledger": str(compatibility), "compatibility_ledger_sha256": file_sha256(compatibility),
            "inventory": str(inventory), "inventory_sha256": file_sha256(inventory),
            "contract_sha256": {name: file_sha256(root / "src/test/python/Frontend/env/support" / name)
                                for name in ("bpu_v3_contract.py", "bpu_vbtb_contract.py")},
            "probe_count": len(projections), "missing": missing, "errors": errors,
            "resolved_probes": projections, "gate_passed": not missing and not errors,
            "runtime_qualified": False, "exact_target_hit": False,
            "note": "Native presence/width and published build compatibility only; no runtime value, DUT run or HIT qualification."}


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--inventory", type=Path, required=True)
    parser.add_argument("--manifest", type=Path, required=True)
    parser.add_argument("--compatibility-ledger", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    result = audit(args.inventory, args.manifest, args.compatibility_ledger)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps({k: v for k, v in result.items() if k != "resolved_probes"}))
    return 0 if result["gate_passed"] else 1


if __name__ == "__main__":
    raise SystemExit(main())
