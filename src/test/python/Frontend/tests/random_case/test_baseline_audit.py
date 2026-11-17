from __future__ import annotations

import json
from pathlib import Path

from .baseline_audit import build_baseline_audit


def test_baseline_audit_clusters_page_tail_recovery_with_trace_proof(
    tmp_path: Path,
) -> None:
    case_root = tmp_path / "case"
    log_dir = case_root / "logs"
    input_dir = case_root / "inputs"
    log_dir.mkdir(parents=True)
    input_dir.mkdir()
    (log_dir / "case.log").write_text(
        "redirect recovery first cfvec is not target: slot=0 "
        "actual_pc=0x8000200a target_pc=0x80001ffe cycle=100 cursor=2\n",
        encoding="utf-8",
    )
    trace_path = input_dir / "case.trace.jsonl"
    trace_path.write_text(
        "\n".join(
            json.dumps(item)
            for item in (
                {
                    "index": 0,
                    "pc": "0x80001ffa",
                    "size": 4,
                    "kind": "branch",
                    "taken": False,
                    "target_pc": "0x80001ffe",
                },
                {
                    "index": 1,
                    "pc": "0x80001ffe",
                    "size": 4,
                    "kind": "normal",
                    "target_pc": None,
                },
                {
                    "index": 2,
                    "pc": "0x80002002",
                    "size": 4,
                    "kind": "jump",
                    "target_pc": "0x8000200a",
                },
                {
                    "index": 3,
                    "pc": "0x8000200a",
                    "size": 4,
                    "kind": "normal",
                    "target_pc": None,
                },
            )
        )
        + "\n",
        encoding="utf-8",
    )
    campaign_path = tmp_path / "campaign.json"
    campaign_path.write_text(
        json.dumps(
            {
                "repo_head": "fixed",
                "cases": [
                    {
                        "name": "page-tail-failure",
                        "artifact_root": str(case_root),
                        "trace_path": str(trace_path),
                        "source_path": str(tmp_path / "case.S"),
                        "spec": {"seed": 11},
                        "timing_seed": 22,
                        "execution_status": "completed",
                        "exit_code": 1,
                        "qualification_pass": False,
                        "dynamic_trace_instructions": 6000,
                    }
                ],
            }
        ),
        encoding="utf-8",
    )

    report = build_baseline_audit(campaign_path)

    assert report["report_status"] == "complete_baseline"
    assert report["failure_clusters"] == [
        {
            "failure_signature": "redirect_recovery_page_tail",
            "count": 1,
            "case_names": ["page-tail-failure"],
        }
    ]
    failure = report["failures"][0]
    assert failure["actual_minus_target"] == 12
    assert failure["trace_sequence_proof"] == {
        "target_page_offset": 0xFFE,
        "target_instruction_crosses_page": True,
        "target_in_trace_context": True,
        "actual_in_trace_context": True,
        "target_precedes_actual": True,
        "predecessor_reaches_target": True,
        "target_trace_indexes": [1],
        "actual_trace_indexes": [3],
    }


def test_baseline_audit_classifies_unattributed_cfi_message(tmp_path: Path) -> None:
    case_root = tmp_path / "case"
    log_dir = case_root / "logs"
    log_dir.mkdir(parents=True)
    (log_dir / "case.log").write_text(
        "first mismatch has no attributable CFI for redirect; "
        "treat as env-model or DUT bug: queue_index=16 pc=0x80000102 "
        "target_pc=0x80000140 cycle=1898 cursor=590\n",
        encoding="utf-8",
    )
    campaign_path = tmp_path / "campaign.json"
    campaign_path.write_text(
        json.dumps(
            {
                "cases": [
                    {
                        "name": "unattributed",
                        "artifact_root": str(case_root),
                        "spec": {"seed": 1},
                        "timing_seed": 2,
                        "execution_status": "completed",
                        "exit_code": 1,
                        "qualification_pass": False,
                    }
                ]
            }
        ),
        encoding="utf-8",
    )

    report = build_baseline_audit(campaign_path)

    assert report["failures"][0]["failure_signature"] == "unattributed_cfi_redirect"
    assert report["failures"][0]["queue_index"] == 16
