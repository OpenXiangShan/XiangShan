from __future__ import annotations

import json
from pathlib import Path

import pytest

from .trusted_funcov import (
    _prefix_order,
    _token_precedes,
    build_quarantine_policy,
    build_trusted_report,
)


def _definition(bin_id: str, group: str, bin_name: str) -> dict[str, str]:
    return {
        "bin_id": bin_id,
        "coverage_group": group,
        "coverpoint": "event",
        "bin_name": bin_name,
    }


def _sources(*, shadow_first: bool) -> dict[str, str]:
    ifu_prefixes = (
        '("dut.__Vtogcov__", "dut.")'
        if shadow_first
        else '("dut.", "dut.__Vtogcov__")'
    )
    return {
        "src/test/python/Frontend/env/funcov/py/ifu/compact_funcov.py": f"""
_IFU_INTERNAL_PREFIXES = {ifu_prefixes}
COMPACT_SAMPLER_BIN_KEYS = frozenset({{("compact", "event")}})
OWNER_BIN = "BIN-002"
""",
        "src/test/python/Frontend/env/funcov/py/ifu/mmio_nc_owner_funcov.py": f"""
_IFU_PREFIXES = {ifu_prefixes}
_INSTR_UNCACHE_PREFIXES = {ifu_prefixes}
""",
        "src/test/python/Frontend/env/funcov/py/ifu/instr_uncache_owner_funcov.py": "",
        "src/test/python/Frontend/env/funcov/py/ifu/cacheable_pipeline_funcov.py": """
SIGNALS = {
    "wb_redirect": ("dut.__Vtogcov__wbRedirect_valid",),
    "bpu": (
        "dut.__Vtogcov__io_fromFtq_flushFromBpu_s3_valid",
        "dut.mainPipe.io_flushFromBpu_s3_valid",
    ),
    "ready": ("dut.__Vtogcov__io_toIBuffer_ready",),
    "exception": ("dut.__Vtogcov__io_toIBuffer_bits_exceptionType_value",),
    "enq": ("dut.__Vtogcov__io_toIBuffer_bits_enqEnable",),
}
""",
        "src/test/python/Frontend/env/funcov/py/ifu/mmio_v3_funcov.py": """
_IFU_PREFIXES = ("dut.", "dut.__Vtogcov__")
""",
    }


def _artifact(path: Path, definitions: list[dict[str, str]]) -> None:
    path.write_text(
        json.dumps(
            {
                "artifact_type": "functional_coverage",
                "provenance": {
                    "compatibility_signature": "fixed-contract",
                    "definitions_sha256": "definitions-hash",
                    "build_manifest_status": "valid",
                    "sampler_sha256": "sampler-hash",
                    "verification_env_sha256": "env-hash",
                    "signal_contract_sha256": "signal-hash",
                },
                "definitions": definitions,
                "hits": {
                    "compact:event": {"bin_id": "BIN-001", "hits": 1},
                    "clean:event": {"bin_id": "BIN-010", "hits": 1},
                },
                "outcome": {"status": "passed"},
                "checker": {"status": "pass", "error_count": 0},
                "run": {
                    "run_id": "seed-1",
                    "seeds": {"test": 11, "backend": 22},
                },
            }
        )
        + "\n",
        encoding="utf-8",
    )


def test_signal_order_helpers_require_both_tokens() -> None:
    assert _prefix_order(("dut.__Vtogcov__", "dut.")) == "shadow_first"
    assert _prefix_order(("dut.", "dut.__Vtogcov__")) == "semantic_first"
    assert _token_precedes("shadow semantic", "shadow", "semantic")
    assert not _token_precedes("shadow only", "shadow", "semantic")


def test_quarantine_policy_keeps_semantic_first_mmio_v3_out() -> None:
    definitions = [
        _definition("BIN-001", "compact", "event"),
        _definition("BIN-002", "owner_misc", "event"),
        _definition("BIN-003", "ifu_mmio_owner_v3", "event"),
        _definition("BIN-004", "ifu_cacheable_flush", "wb_redirect_blocks"),
        _definition("BIN-005", "ifu_cacheable_flush", "bpu_match_blocks"),
        _definition("BIN-006", "ifu_cacheable_flush", "bpu_miss_allows"),
        _definition("BIN-901", "owner_misc", "bpu_owner"),
        _definition("BIN-906", "owner_misc", "late_fault"),
        _definition("BIN-010", "clean", "event"),
    ]
    sources = _sources(shadow_first=True)
    policy = build_quarantine_policy(
        definitions,
        {"BIN-001", "BIN-003", "BIN-004", "BIN-010"},
        source_loader=sources.__getitem__,
    )

    assert set(policy["active_rule_ids"]) == {
        "compact_module_shadow_first",
        "mmio_nc_owner_shadow_first",
        "cacheable_wb_redirect_shadow_only",
        "cacheable_bpu_flush_shadow_first",
        "cacheable_late_fault_shadow_first",
    }
    quarantined = set(policy["quarantined_definition_bin_ids"])
    assert "BIN-010" not in quarantined
    assert {"BIN-001", "BIN-002", "BIN-003", "BIN-004", "BIN-005", "BIN-006", "BIN-901", "BIN-906"} <= quarantined
    mmio_v3 = next(
        item for item in policy["source_audit"] if item["source"] == "mmio_v3"
    )
    assert mmio_v3["ifu_prefix_order"] == "semantic_first"


def test_build_trusted_report_preserves_denominator(tmp_path: Path) -> None:
    definitions = [
        _definition("BIN-001", "compact", "event"),
        _definition("BIN-002", "owner_misc", "event"),
        _definition("BIN-010", "clean", "event"),
    ]
    artifact_path = tmp_path / "seed-1.funcov.json"
    _artifact(artifact_path, definitions)
    campaign_path = tmp_path / "campaign.json"
    campaign_path.write_text(
        json.dumps(
            {
                "repo_root": str(tmp_path),
                "repo_head": "fixed-revision",
                "cases": [
                    {
                        "execution_status": "completed",
                        "qualification_pass": True,
                        "eligible_functional_coverage_artifacts": [
                            str(artifact_path)
                        ],
                        "observed_bin_ids": ["BIN-001", "BIN-010"],
                    }
                ],
            }
        )
        + "\n",
        encoding="utf-8",
    )
    sources = _sources(shadow_first=True)

    report = build_trusted_report(
        campaign_path,
        source_loader=sources.__getitem__,
    )

    assert report["report_status"] == "complete_baseline"
    assert report["coverage"] == {
        "definition_bins": 3,
        "raw_union_hit_bins": 2,
        "raw_union_hit_rate": 2 / 3,
        "trusted_union_hit_bins": 1,
        "trusted_union_hit_rate": 1 / 3,
        "quarantined_raw_hit_bins": 1,
        "raw_union_bin_ids": ["BIN-001", "BIN-010"],
        "trusted_union_bin_ids": ["BIN-010"],
        "unhit_or_untrusted_bin_ids": ["BIN-001", "BIN-002"],
    }
    assert report["per_qualified_artifact_marginal_gain"][0]["program_seed"] == 11
    assert report["per_qualified_artifact_marginal_gain"][0]["timing_seed"] == 22
    assert report["qualification"] == {
        "pass": False,
        "minimum_hit_rate": 0.99,
        "minimum_qualified_runs": 100,
        "reasons": [
            "qualified_cases=1<required=100",
            "trusted_hit_rate=0.33333333<required=0.99000000",
        ],
    }

    relaxed = build_trusted_report(
        campaign_path,
        source_loader=sources.__getitem__,
        minimum_hit_rate=1 / 3,
        minimum_qualified_runs=1,
    )
    assert relaxed["qualification"]["pass"] is True


def test_build_trusted_report_rejects_manifest_union_mismatch(tmp_path: Path) -> None:
    definitions = [
        _definition("BIN-001", "compact", "event"),
        _definition("BIN-010", "clean", "event"),
    ]
    artifact_path = tmp_path / "seed-1.funcov.json"
    _artifact(artifact_path, definitions)
    campaign_path = tmp_path / "campaign.json"
    campaign_path.write_text(
        json.dumps(
            {
                "cases": [
                    {
                        "execution_status": "completed",
                        "qualification_pass": True,
                        "eligible_functional_coverage_artifacts": [str(artifact_path)],
                        "observed_bin_ids": ["BIN-010"],
                    }
                ]
            }
        ),
        encoding="utf-8",
    )

    with pytest.raises(ValueError, match="campaign observed_bin_ids disagree"):
        build_trusted_report(
            campaign_path,
            source_loader=_sources(shadow_first=True).__getitem__,
        )
