import ast
import csv
import re
from pathlib import Path

import pytest

from env.funcov.py.ifu import cacheable_pipeline_funcov, compact_funcov
from env.funcov.py.ifu import instr_uncache_owner_funcov as instr_uncache_sampler
from env.funcov.py.ifu import mmio_nc_owner_funcov as mmio_nc_owner_sampler
from env.funcov.py.ifu.owner_v3_funcov import (
    OWNER_V3_BIN_SPECS,
    OWNER_V3_BLOCKED_BIN_IDS,
    OWNER_V3_SOURCE_RULES,
)
from env.funcov.py.ifu.mmio_nc_owner_funcov import (
    MMIO_NC_OWNER_COVERPOINT,
    MMIO_NC_OWNER_SAMPLER_BIN_KEYS,
    MMIO_OWNER_GROUP,
    NC_OWNER_GROUP,
)
from env.funcov.py.ifu.instr_uncache_owner_funcov import (
    INSTR_UNCACHE_OWNER_COVERPOINT,
    INSTR_UNCACHE_OWNER_GROUP,
    INSTR_UNCACHE_OWNER_SAMPLER_BIN_KEYS,
)
from tools.backannotate_funcov import validate_pilot_schema


IFU_FUNCOV_BIN_IDS = {
    *(f"BIN-{index:03d}" for index in range(401, 416)),
    *(f"BIN-{index:03d}" for index in range(424, 433)),
    *(f"BIN-{index:03d}" for index in range(501, 542)),
    "BIN-824",
    *(f"BIN-{index:03d}" for index in range(832, 837)),
    *(f"BIN-{index:03d}" for index in range(889, 899)),
}

ICACHE_PREFETCH_FUNCOV_BIN_IDS = {
    *(f"BIN-{index:03d}" for index in range(650, 674)),
    *(f"BIN-{index:03d}" for index in range(677, 686)),
    *(f"BIN-{index:03d}" for index in range(777, 781)),
}

ICACHE_MISSUNIT_FUNCOV_BIN_IDS = {
    *(f"BIN-{index:03d}" for index in range(686, 717) if index not in (697, 716)),
    *(f"BIN-{index}" for index in range(1005, 1010)),
}

ICACHE_WAYLOOKUP_FUNCOV_BIN_IDS = {
    *(f"BIN-{index:03d}" for index in range(717, 759) if index not in (725, 729, 730)),
    "BIN-1010",
    "BIN-1011",
}

ICACHE_HITMISS_FUNCOV_BIN_IDS = {
    *(f"BIN-{index:03d}" for index in range(759, 769)),
}

IFU_CACHEABLE_PIPELINE_BIN_IDS = {
    *(f"BIN-{index:03d}" for index in range(801, 824)),
    *(f"BIN-{index:03d}" for index in range(825, 832)),
    *(f"BIN-{index:03d}" for index in range(837, 844)),
    *(f"BIN-{index:03d}" for index in range(844, 854)),
    *(f"BIN-{index:03d}" for index in range(854, 889)),
}

ICACHE_MAINPIPE_S2_ECC_BIN_IDS = {
    "BIN-641",
    "BIN-642",
    "BIN-645",
    *(f"BIN-{index:03d}" for index in range(770, 777)),
}

ICACHE_MAPPED_STATUSES = {"MODELED", "PARTIAL", "HIT"}
MMIO_NC_OWNER_BIN_IDS = {f"BIN-{index}" for index in range(1016, 1094)}
INSTR_UNCACHE_OWNER_BIN_IDS = {f"BIN-{index}" for index in range(1094, 1132)}

IFU_V3_OWNER_MARKER_BLOCK_LEAF_COUNTS = (139, 43, 42, 81)
IFU_V3_CANONICAL_BLOCK_RANGES = ((595, 770), (1026, 1142), (1184, 1243), (1245, 1349))
IFU_V3_CANONICAL_BLOCK_LEAF_COUNTS = (139, 81, 42, 81)
IFU_V3_FORBIDDEN_OWNER_TERMS = {
    "ifu_instr_boundary_v2",
    "targetFault",
    "target fault",
    "s3_valid",
    "s3_icacheMeta",
    "s3_reqIsUncache",
    "prevLast",
    "lastInstrIsHalfRvi",
    "fixedTwoFetchRange",
    "firstPredTakenIdx",
    "f3/s3",
    "s3 payload",
}
MODELED_RUNTIME_PRODUCER_GAP_BIN_IDS = frozenset(
    {
        "BIN-900",
        "BIN-921",
        "BIN-922",
        "BIN-927",
        "BIN-928",
        "BIN-951",
        "BIN-953",
        "BIN-955",
        "BIN-956",
    }
)


def test_active_pilot_has_global_unique_identifiers_and_mappings():
    repo_root = Path(__file__).resolve().parents[7]
    pilot_path = (
        repo_root
        / "src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv"
    )

    summary = validate_pilot_schema(pilot_path)

    assert summary == {"rows": 724, "bin_ids": 724, "mapping_keys": 724, "legacy_ids": 4}


def test_jiabowen_ifu_owner_blocks_follow_v3_rtl_baseline():
    repo_root = Path(__file__).resolve().parents[7]
    testpoint_path = (
        repo_root
        / "src/test/python/Frontend/docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv"
    )
    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        rows = list(csv.reader(handle))

    marker_indices = [
        index for index, row in enumerate(rows) if "@加柏文" in "".join(row)
    ]
    assert len(marker_indices) == 4

    owner_blocks = []
    for marker_index in marker_indices:
        marker = rows[marker_index]
        marker_depth = next(index for index, value in enumerate(marker[:5]) if value.strip())
        block_end = len(rows)
        for index in range(marker_index + 1, len(rows)):
            if any(rows[index][depth].strip() for depth in range(marker_depth + 1)):
                block_end = index
                break
        owner_blocks.append(rows[marker_index:block_end])

    assert tuple(
        sum(bool(row[4].strip()) for row in block) for block in owner_blocks
    ) == IFU_V3_OWNER_MARKER_BLOCK_LEAF_COUNTS

    for block in owner_blocks:
        assert "V3" in "".join(block[0])
        block_text = "\n".join("|".join(row) for row in block)
        assert not (IFU_V3_FORBIDDEN_OWNER_TERMS & set(re.findall(
            r"ifu_instr_boundary_v2|targetFault|target fault|s3_valid|s3_icacheMeta|"
            r"s3_reqIsUncache|prevLast|lastInstrIsHalfRvi|fixedTwoFetchRange|"
            r"firstPredTakenIdx",
            block_text,
        )))
        for row in block:
            if not row[4].strip():
                continue
            assert all(row[column].strip() for column in (5, 6, 7))
            assert row[9].strip() in {
                "UNMAPPED",
                "MODELED",
                "PARTIAL",
                "HIT",
                "BLOCKED",
            }
            assert row[9].strip() != "UNMAPPED"

    canonical_blocks = [
        rows[start - 1 : end] for start, end in IFU_V3_CANONICAL_BLOCK_RANGES
    ]
    assert (
        tuple(sum(bool(row[4].strip()) for row in block) for block in canonical_blocks)
        == IFU_V3_CANONICAL_BLOCK_LEAF_COUNTS
    )
    assert sum(IFU_V3_CANONICAL_BLOCK_LEAF_COUNTS) == 343
    for block in canonical_blocks:
        for row in block:
            if row[4].strip():
                assert row[9].strip() != "UNMAPPED"


def test_jiabowen_owner_event_bins_are_exactly_mapped_once():
    repo_root = Path(__file__).resolve().parents[7]
    testpoint_path = (
        repo_root
        / "src/test/python/Frontend/docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv"
    )
    owner_ids = {spec.bin_id for spec in OWNER_V3_BIN_SPECS}
    mapped = {}
    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        for row in csv.DictReader(handle):
            bin_ids = set(re.findall(r"BIN-\d+", row["coverage"])) & owner_ids
            if not bin_ids:
                continue
            assert len(bin_ids) == 1
            bin_id = bin_ids.pop()
            assert bin_id not in mapped
            assert row["status"] in {"MODELED", "HIT", "PARTIAL", "BLOCKED"}
            assert "MODEL:test_ifu_v3_owner_event_model" in row["evidence"]
            mapped[bin_id] = row

    assert set(mapped) == owner_ids
    assert {
        bin_id for bin_id, row in mapped.items() if row["status"] == "BLOCKED"
    } == OWNER_V3_BLOCKED_BIN_IDS


def _owner_runtime_producer_bin_ids() -> set[str]:
    result = {rule.bin_id for rule in OWNER_V3_SOURCE_RULES}
    for module in (
        cacheable_pipeline_funcov,
        compact_funcov,
        mmio_nc_owner_sampler,
    ):
        tree = ast.parse(Path(module.__file__).read_text(encoding="utf-8"))
        for function in (
            node
            for node in ast.walk(tree)
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
        ):
            has_runtime_mark = any(
                isinstance(node, ast.Call)
                and (
                    isinstance(node.func, ast.Name)
                    and node.func.id == "mark_owner_v3_checked"
                )
                for node in ast.walk(function)
            )
            if not has_runtime_mark:
                continue
            result.update(
                str(node.value)
                for node in ast.walk(function)
                if isinstance(node, ast.Constant)
                and isinstance(node.value, str)
                and re.fullmatch(r"BIN-\d+", node.value)
            )
    return result


def _resolve_instr_uncache_mark_indices(tree: ast.AST) -> set[int]:
    result: set[int] = set()

    def integer_constants(node: ast.AST) -> set[int]:
        if isinstance(node, ast.Constant) and isinstance(node.value, int):
            return {int(node.value)}
        if isinstance(node, ast.IfExp):
            return integer_constants(node.body) | integer_constants(node.orelse)
        return set()

    for function in (
        node for node in ast.walk(tree) if isinstance(node, ast.FunctionDef)
    ):
        assignments = {
            target.id: node.value
            for node in ast.walk(function)
            if isinstance(node, ast.Assign)
            for target in node.targets
            if isinstance(target, ast.Name)
        }
        for call in (
            node
            for node in ast.walk(function)
            if isinstance(node, ast.Call)
            and isinstance(node.func, ast.Name)
            and node.func.id == "_mark"
            and len(node.args) >= 2
        ):
            index_arg = call.args[1]
            result.update(integer_constants(index_arg))
            if not isinstance(index_arg, ast.Name):
                continue
            for loop in (
                node
                for node in ast.walk(function)
                if isinstance(node, ast.For)
                and isinstance(node.target, (ast.Tuple, ast.List))
                and node.target.elts
                and isinstance(node.target.elts[0], ast.Name)
                and node.target.elts[0].id == index_arg.id
                and isinstance(node.iter, ast.Name)
            ):
                values = assignments.get(loop.iter.id)
                if not isinstance(values, (ast.Tuple, ast.List)):
                    continue
                result.update(
                    int(item.elts[0].value)
                    for item in values.elts
                    if isinstance(item, (ast.Tuple, ast.List))
                    and item.elts
                    and isinstance(item.elts[0], ast.Constant)
                    and isinstance(item.elts[0].value, int)
                )
    return result


def _literal_recorder_mark_keys(repo_root: Path) -> set[tuple[str, str]]:
    result: set[tuple[str, str]] = set()
    sampler_root = repo_root / "src/test/python/Frontend/env/funcov"
    for path in sampler_root.rglob("*.py"):
        tree = ast.parse(path.read_text(encoding="utf-8"))
        result.update(
            (str(node.args[0].value), str(node.args[1].value))
            for node in ast.walk(tree)
            if isinstance(node, ast.Call)
            and isinstance(node.func, ast.Attribute)
            and node.func.attr == "mark"
            and len(node.args) >= 2
            and isinstance(node.args[0], ast.Constant)
            and isinstance(node.args[0].value, str)
            and isinstance(node.args[1], ast.Constant)
            and isinstance(node.args[1].value, str)
        )
    return result


def test_modeled_jiabowen_runtime_producer_gap_inventory_is_current():
    repo_root = Path(__file__).resolve().parents[7]
    pilot_path = (
        repo_root
        / "src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv"
    )
    testpoint_path = (
        repo_root
        / "src/test/python/Frontend/docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv"
    )
    with pilot_path.open(encoding="utf-8-sig", newline="") as handle:
        pilot_rows = {row["Bin_ID"]: row for row in csv.DictReader(handle)}
    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        rows = list(csv.DictReader(handle))

    owner_producers = _owner_runtime_producer_bin_ids()
    instr_tree = ast.parse(
        Path(instr_uncache_sampler.__file__).read_text(encoding="utf-8")
    )
    instr_indices = _resolve_instr_uncache_mark_indices(instr_tree)
    direct_keys = _literal_recorder_mark_keys(repo_root)
    missing = []
    for physical_line, row in enumerate(rows, start=2):
        if not any(
            start <= physical_line <= end
            for start, end in IFU_V3_CANONICAL_BLOCK_RANGES
        ):
            continue
        if not row["Checkpoint"].strip() or row["status"] != "MODELED":
            continue
        bin_ids = re.findall(r"BIN-\d+", row["coverage"])
        assert bin_ids, (physical_line, row["coverage"])
        bin_id = bin_ids[-1]
        pilot = pilot_rows[bin_id]
        if bin_id in {spec.bin_id for spec in OWNER_V3_BIN_SPECS}:
            produced = bin_id in owner_producers
        elif pilot["Coverage_Group"] == INSTR_UNCACHE_OWNER_GROUP:
            produced = int(pilot["Bin_Name"].rsplit("_", 1)[1]) in instr_indices
        else:
            produced = (pilot["Coverage_Group"], pilot["Bin_Name"]) in direct_keys
        if not produced:
            missing.append((physical_line, bin_id, pilot["Coverage_Group"], pilot["Bin_Name"]))

    # A producer gap is a closure-work inventory, not a MODELED status gate.
    assert {item[1] for item in missing} == MODELED_RUNTIME_PRODUCER_GAP_BIN_IDS


def test_legacy_bpu_ftq_rows_are_unmapped_and_cannot_enter_runtime_model():
    repo_root = Path(__file__).resolve().parents[7]
    pilot_path = (
        repo_root
        / "src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv"
    )

    with pilot_path.open(encoding="utf-8-sig", newline="") as handle:
        rows = list(csv.DictReader(handle))

    active = [row for row in rows if row["Coverpoint"].strip()]
    legacy_bpu_ftq = [row for row in rows if "旧BPU_FTQ" in row["映射测试点路径"]]

    assert len(active) == 566
    assert {row["Bin_ID"] for row in active} == {
        *(f"BIN-{index:03d}" for index in range(401, 424)),
        *(f"BIN-{index:03d}" for index in range(424, 433)),
        *(f"BIN-{index:03d}" for index in range(501, 542)),
        *(f"BIN-{index:03d}" for index in range(601, 619)),
        *(f"BIN-{index:03d}" for index in range(620, 627)),
        *(f"BIN-{index:03d}" for index in range(628, 632)),
        *(f"BIN-{index:03d}" for index in range(633, 637)),
        "BIN-638",
        "BIN-769",
        *(f"BIN-{index:03d}" for index in range(770, 781)),
        "BIN-641",
        "BIN-642",
        "BIN-645",
        *(f"BIN-{index:03d}" for index in range(674, 676)),
        *(f"BIN-{index:03d}" for index in range(650, 674)),
        *(f"BIN-{index:03d}" for index in range(677, 686)),
        *(f"BIN-{index:03d}" for index in range(686, 717) if index not in (697, 716)),
        *(f"BIN-{index:03d}" for index in range(717, 759) if index not in (725, 729, 730)),
        *(f"BIN-{index:03d}" for index in range(759, 781)),
        *(f"BIN-{index:03d}" for index in range(801, 1132)),
    }
    assert legacy_bpu_ftq
    assert all(not row["Coverpoint"].strip() for row in legacy_bpu_ftq)


def test_mmio_nc_owner_leaves_are_single_bin_and_match_registry():
    repo_root = Path(__file__).resolve().parents[7]
    pilot_path = (
        repo_root
        / "src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv"
    )
    testpoint_path = (
        repo_root
        / "src/test/python/Frontend/docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv"
    )

    with pilot_path.open(encoding="utf-8-sig", newline="") as handle:
        pilot_rows = {
            row["Bin_ID"]: row
            for row in csv.DictReader(handle)
            if row["Bin_ID"] in MMIO_NC_OWNER_BIN_IDS
        }
    assert set(pilot_rows) == MMIO_NC_OWNER_BIN_IDS
    assert {
        (row["Coverage_Group"], row["Bin_Name"])
        for row in pilot_rows.values()
    } == MMIO_NC_OWNER_SAMPLER_BIN_KEYS
    assert all(
        row["Coverpoint"] == MMIO_NC_OWNER_COVERPOINT
        for row in pilot_rows.values()
    )

    mapped_rows = {}
    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        for row in csv.DictReader(handle):
            bin_ids = set(re.findall(r"BIN-\d+", row["coverage"])) & MMIO_NC_OWNER_BIN_IDS
            if not bin_ids:
                continue
            assert len(bin_ids) == 1, row["coverage"]
            bin_id = bin_ids.pop()
            assert bin_id not in mapped_rows
            pilot = pilot_rows[bin_id]
            assert row["coverage"] == (
                f"covergroup {pilot['Coverage_Group']}, "
                f"coverpoint {pilot['Coverpoint']}, "
                f"bins {pilot['Bin_Name']} ({bin_id})"
            )
            assert row["status"] in {"MODELED", "HIT", "PARTIAL", "BLOCKED"}
            assert "MODEL:sample_mmio_nc_owner_coverage" in row["evidence"]
            mapped_rows[bin_id] = row

    assert set(mapped_rows) == MMIO_NC_OWNER_BIN_IDS
    assert sum(
        row["Coverage_Group"] == MMIO_OWNER_GROUP for row in pilot_rows.values()
    ) == 39
    assert sum(
        row["Coverage_Group"] == NC_OWNER_GROUP for row in pilot_rows.values()
    ) == 39


def test_instr_uncache_owner_leaves_are_complete_and_preserve_sv_models():
    repo_root = Path(__file__).resolve().parents[7]
    pilot_path = (
        repo_root
        / "src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv"
    )
    testpoint_path = (
        repo_root
        / "src/test/python/Frontend/docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv"
    )

    with pilot_path.open(encoding="utf-8-sig", newline="") as handle:
        pilot_rows = {
            row["Bin_ID"]: row
            for row in csv.DictReader(handle)
            if row["Bin_ID"] in INSTR_UNCACHE_OWNER_BIN_IDS
        }
    assert set(pilot_rows) == INSTR_UNCACHE_OWNER_BIN_IDS
    assert {
        (row["Coverage_Group"], row["Bin_Name"]) for row in pilot_rows.values()
    } == INSTR_UNCACHE_OWNER_SAMPLER_BIN_KEYS
    assert all(
        row["Coverage_Group"] == INSTR_UNCACHE_OWNER_GROUP
        and row["Coverpoint"] == INSTR_UNCACHE_OWNER_COVERPOINT
        for row in pilot_rows.values()
    )

    mapped_rows = {}
    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        for row in csv.DictReader(handle):
            bin_ids = (
                set(re.findall(r"BIN-\d+", row["coverage"]))
                & INSTR_UNCACHE_OWNER_BIN_IDS
            )
            if not bin_ids:
                continue
            assert len(bin_ids) == 1, row["coverage"]
            bin_id = bin_ids.pop()
            assert bin_id not in mapped_rows
            pilot = pilot_rows[bin_id]
            python_mapping = (
                f"covergroup {pilot['Coverage_Group']}, "
                f"coverpoint {pilot['Coverpoint']}, "
                f"bins {pilot['Bin_Name']} ({bin_id})"
            )
            assert python_mapping in row["coverage"]
            assert row["status"] in {"MODELED", "PARTIAL", "HIT"}
            assert "MODEL:sample_instr_uncache_owner_coverage" in row["evidence"]
            mapped_rows[bin_id] = row

    assert set(mapped_rows) == INSTR_UNCACHE_OWNER_BIN_IDS
    assert (
        sum(
            "covergroup frontend_mmio_fetch_cg" in row["coverage"]
            for row in mapped_rows.values()
        )
        == 34
    )
    assert sum(row["status"] == "HIT" for row in mapped_rows.values()) == 33
    assert sum(row["status"] == "MODELED" for row in mapped_rows.values()) == 5
    assert sum(row["status"] == "PARTIAL" for row in mapped_rows.values()) == 0


def test_pilot_schema_rejects_duplicate_bin_id(tmp_path):
    pilot_path = tmp_path / "pilot.csv"
    pilot_path.write_text(
        "Bin_ID,Coverage_Group,Coverpoint,Bin_Name,建议试点用例\n"
        "BIN-001,group_a,point_a,bin_a,case_a\n"
        "BIN-001,group_b,point_b,bin_b,case_b\n",
        encoding="utf-8-sig",
    )

    with pytest.raises(ValueError, match="duplicate Bin_ID BIN-001"):
        validate_pilot_schema(pilot_path)


def test_icache_mainpipe_s2_ecc_leaves_are_single_bin_and_match_registry():
    repo_root = Path(__file__).resolve().parents[7]
    pilot_path = (
        repo_root
        / "src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv"
    )
    testpoint_path = (
        repo_root
        / "src/test/python/Frontend/docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv"
    )
    with pilot_path.open(encoding="utf-8-sig", newline="") as handle:
        pilot_rows = {
            row["Bin_ID"]: row
            for row in csv.DictReader(handle)
            if row["Bin_ID"] in ICACHE_MAINPIPE_S2_ECC_BIN_IDS
        }
    assert set(pilot_rows) == ICACHE_MAINPIPE_S2_ECC_BIN_IDS

    mapped_rows = {}
    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        for row in csv.DictReader(handle):
            bin_ids = (
                set(re.findall(r"BIN-\d+", row["coverage"]))
                & ICACHE_MAINPIPE_S2_ECC_BIN_IDS
            )
            if not bin_ids:
                continue
            assert len(bin_ids) == 1, row["coverage"]
            bin_id = bin_ids.pop()
            assert bin_id not in mapped_rows, bin_id
            assert all(
                row[column].strip()
                for column in ("Condition", "Checkpoint", "Object")
            )
            pilot = pilot_rows[bin_id]
            assert row["coverage"] == (
                f"covergroup {pilot['Coverage_Group']}, "
                f"coverpoint {pilot['Coverpoint']}, "
                f"bins {pilot['Bin_Name']} ({bin_id})"
            )
            assert row["status"] in ICACHE_MAPPED_STATUSES
            mapped_rows[bin_id] = row

    assert set(mapped_rows) == ICACHE_MAINPIPE_S2_ECC_BIN_IDS


def test_ifu_predecode_and_two_fetch_leaves_are_single_bin_and_actionable():
    repo_root = Path(__file__).resolve().parents[7]
    pilot_path = (
        repo_root
        / "src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv"
    )
    testpoint_path = (
        repo_root
        / "src/test/python/Frontend/docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv"
    )

    with pilot_path.open(encoding="utf-8-sig", newline="") as handle:
        pilot_rows = {row["Bin_ID"]: row for row in csv.DictReader(handle) if row["Bin_ID"] in IFU_FUNCOV_BIN_IDS}

    assert set(pilot_rows) == IFU_FUNCOV_BIN_IDS

    mapped_rows = {}
    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        for row in csv.DictReader(handle):
            bin_ids = set(re.findall(r"BIN-\d+", row["coverage"])) & IFU_FUNCOV_BIN_IDS
            if not bin_ids:
                continue

            assert len(bin_ids) == 1, row["coverage"]
            bin_id = bin_ids.pop()
            assert bin_id not in mapped_rows, bin_id
            assert all(row[column].strip() for column in ("Condition", "Checkpoint", "Object"))
            assert "构造V3" not in row["Condition"] + row["Checkpoint"]

            pilot = pilot_rows[bin_id]
            expected_coverage = (
                f"covergroup {pilot['Coverage_Group']}, coverpoint {pilot['Coverpoint']}, "
                f"bins {pilot['Bin_Name']} ({bin_id})"
            )
            assert row["coverage"] == expected_coverage
            mapped_rows[bin_id] = row

    assert set(mapped_rows) == IFU_FUNCOV_BIN_IDS


def test_ifu_cacheable_pipeline_leaves_are_single_bin_and_match_registry():
    repo_root = Path(__file__).resolve().parents[7]
    pilot_path = (
        repo_root
        / "src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv"
    )
    testpoint_path = (
        repo_root
        / "src/test/python/Frontend/docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv"
    )

    with pilot_path.open(encoding="utf-8-sig", newline="") as handle:
        pilot_rows = {
            row["Bin_ID"]: row
            for row in csv.DictReader(handle)
            if row["Bin_ID"] in IFU_CACHEABLE_PIPELINE_BIN_IDS
        }
    assert set(pilot_rows) == IFU_CACHEABLE_PIPELINE_BIN_IDS

    mapped_rows = {}
    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        for row in csv.DictReader(handle):
            bin_ids = (
                set(re.findall(r"BIN-\d+", row["coverage"]))
                & IFU_CACHEABLE_PIPELINE_BIN_IDS
            )
            if not bin_ids:
                continue
            assert len(bin_ids) == 1, row["coverage"]
            bin_id = bin_ids.pop()
            assert bin_id not in mapped_rows, bin_id
            assert all(
                row[column].strip()
                for column in ("Condition", "Checkpoint", "Object")
            )

            pilot = pilot_rows[bin_id]
            expected_coverage = (
                f"covergroup {pilot['Coverage_Group']}, coverpoint {pilot['Coverpoint']}, "
                f"bins {pilot['Bin_Name']} ({bin_id})"
            )
            assert row["coverage"] == expected_coverage
            assert row["status"] in ICACHE_MAPPED_STATUSES
            mapped_rows[bin_id] = row

    assert set(mapped_rows) == IFU_CACHEABLE_PIPELINE_BIN_IDS


def test_icache_prefetch_leaves_are_single_bin_and_match_registry():
    repo_root = Path(__file__).resolve().parents[7]
    pilot_path = (
        repo_root
        / "src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv"
    )
    testpoint_path = (
        repo_root
        / "src/test/python/Frontend/docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv"
    )

    with pilot_path.open(encoding="utf-8-sig", newline="") as handle:
        pilot_rows = {
            row["Bin_ID"]: row
            for row in csv.DictReader(handle)
            if row["Bin_ID"] in ICACHE_PREFETCH_FUNCOV_BIN_IDS
        }
    assert set(pilot_rows) == ICACHE_PREFETCH_FUNCOV_BIN_IDS

    mapped_rows = {}
    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        for row in csv.DictReader(handle):
            bin_ids = (
                set(re.findall(r"BIN-\d+", row["coverage"]))
                & ICACHE_PREFETCH_FUNCOV_BIN_IDS
            )
            if not bin_ids:
                continue
            assert len(bin_ids) == 1, row["coverage"]
            bin_id = bin_ids.pop()
            assert bin_id not in mapped_rows, bin_id
            assert all(
                row[column].strip()
                for column in ("Condition", "Checkpoint", "Object")
            )

            pilot = pilot_rows[bin_id]
            expected_coverage = (
                f"covergroup {pilot['Coverage_Group']}, coverpoint {pilot['Coverpoint']}, "
                f"bins {pilot['Bin_Name']} ({bin_id})"
            )
            assert row["coverage"] == expected_coverage
            assert row["status"] in ICACHE_MAPPED_STATUSES
            mapped_rows[bin_id] = row

    assert set(mapped_rows) == ICACHE_PREFETCH_FUNCOV_BIN_IDS


def test_icache_missunit_leaves_are_single_bin_and_match_registry():
    repo_root = Path(__file__).resolve().parents[7]
    pilot_path = (
        repo_root
        / "src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv"
    )
    testpoint_path = (
        repo_root
        / "src/test/python/Frontend/docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv"
    )

    with pilot_path.open(encoding="utf-8-sig", newline="") as handle:
        pilot_rows = {
            row["Bin_ID"]: row
            for row in csv.DictReader(handle)
            if row["Bin_ID"] in ICACHE_MISSUNIT_FUNCOV_BIN_IDS
        }
    assert set(pilot_rows) == ICACHE_MISSUNIT_FUNCOV_BIN_IDS

    mapped_rows = {}
    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        for row in csv.DictReader(handle):
            bin_ids = set(re.findall(r"BIN-\d+", row["coverage"])) & ICACHE_MISSUNIT_FUNCOV_BIN_IDS
            if not bin_ids:
                continue
            assert len(bin_ids) == 1, row["coverage"]
            bin_id = bin_ids.pop()
            assert bin_id not in mapped_rows, bin_id
            assert all(row[column].strip() for column in ("Condition", "Checkpoint", "Object"))
            pilot = pilot_rows[bin_id]
            expected_coverage = (
                f"covergroup {pilot['Coverage_Group']}, coverpoint {pilot['Coverpoint']}, "
                f"bins {pilot['Bin_Name']} ({bin_id})"
            )
            assert row["coverage"] == expected_coverage
            assert row["status"] in ICACHE_MAPPED_STATUSES
            mapped_rows[bin_id] = row

    assert set(mapped_rows) == ICACHE_MISSUNIT_FUNCOV_BIN_IDS


def test_icache_waylookup_leaves_are_single_bin_and_match_registry():
    repo_root = Path(__file__).resolve().parents[7]
    pilot_path = (
        repo_root
        / "src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv"
    )
    testpoint_path = (
        repo_root
        / "src/test/python/Frontend/docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv"
    )

    with pilot_path.open(encoding="utf-8-sig", newline="") as handle:
        pilot_rows = {
            row["Bin_ID"]: row
            for row in csv.DictReader(handle)
            if row["Bin_ID"] in ICACHE_WAYLOOKUP_FUNCOV_BIN_IDS
        }
    assert set(pilot_rows) == ICACHE_WAYLOOKUP_FUNCOV_BIN_IDS

    mapped_rows = {}
    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        for row in csv.DictReader(handle):
            bin_ids = set(re.findall(r"BIN-\d+", row["coverage"])) & ICACHE_WAYLOOKUP_FUNCOV_BIN_IDS
            if not bin_ids:
                continue
            assert len(bin_ids) == 1, row["coverage"]
            bin_id = bin_ids.pop()
            assert bin_id not in mapped_rows, bin_id
            assert all(row[column].strip() for column in ("Condition", "Checkpoint", "Object"))
            pilot = pilot_rows[bin_id]
            expected_coverage = (
                f"covergroup {pilot['Coverage_Group']}, coverpoint {pilot['Coverpoint']}, "
                f"bins {pilot['Bin_Name']} ({bin_id})"
            )
            assert row["coverage"] == expected_coverage
            assert row["status"] in ICACHE_MAPPED_STATUSES
            mapped_rows[bin_id] = row

    assert set(mapped_rows) == ICACHE_WAYLOOKUP_FUNCOV_BIN_IDS


def test_icache_hitmiss_leaves_are_single_bin_and_match_registry():
    repo_root = Path(__file__).resolve().parents[7]
    pilot_path = (
        repo_root
        / "src/test/python/Frontend/docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv"
    )
    testpoint_path = (
        repo_root
        / "src/test/python/Frontend/docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv"
    )

    with pilot_path.open(encoding="utf-8-sig", newline="") as handle:
        pilot_rows = {
            row["Bin_ID"]: row
            for row in csv.DictReader(handle)
            if row["Bin_ID"] in ICACHE_HITMISS_FUNCOV_BIN_IDS
        }
    assert set(pilot_rows) == ICACHE_HITMISS_FUNCOV_BIN_IDS

    mapped_rows = {}
    with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
        for row in csv.DictReader(handle):
            bin_ids = set(re.findall(r"BIN-\d+", row["coverage"])) & ICACHE_HITMISS_FUNCOV_BIN_IDS
            if not bin_ids:
                continue
            assert len(bin_ids) == 1, row["coverage"]
            bin_id = bin_ids.pop()
            assert bin_id not in mapped_rows, bin_id
            assert all(row[column].strip() for column in ("Condition", "Checkpoint", "Object"))
            pilot = pilot_rows[bin_id]
            expected_coverage = (
                f"covergroup {pilot['Coverage_Group']}, coverpoint {pilot['Coverpoint']}, "
                f"bins {pilot['Bin_Name']} ({bin_id})"
            )
            assert row["coverage"] == expected_coverage
            assert row["status"] in ICACHE_MAPPED_STATUSES
            mapped_rows[bin_id] = row

    assert set(mapped_rows) == ICACHE_HITMISS_FUNCOV_BIN_IDS
