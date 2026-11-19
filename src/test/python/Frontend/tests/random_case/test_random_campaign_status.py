from __future__ import annotations

import pytest

from . import campaign_status as campaign_runner


def test_campaign_aggregate_separates_case_failures_from_coverage_gates() -> None:
    campaign: dict = {"post_run_gates": []}
    executed = [
        {"name": "qualified", "qualification_pass": True},
        {"name": "checker-failed", "qualification_pass": False},
    ]

    case_failures = campaign_runner._update_case_execution_summary(
        campaign,
        planned_case_count=2,
        executed=executed,
    )
    campaign_runner._record_post_run_gate(
        campaign,
        gate_id="trusted_funcov",
        exit_code=1,
    )

    assert case_failures == 1
    assert campaign["execution"] == {
        "planned_case_count": 2,
        "executed_case_count": 2,
        "qualified_case_count": 1,
        "case_failure_count": 1,
        "complete": True,
    }
    assert campaign_runner._finalize_campaign_status(campaign) == 2
    assert campaign["case_failure_count"] == 1
    assert campaign["post_run_gate_failure_count"] == 1
    assert campaign["status"] == "fail"

    all_qualified = [
        {"name": "qualified-a", "qualification_pass": True},
        {"name": "qualified-b", "qualification_pass": True},
    ]
    campaign = {"post_run_gates": []}
    campaign_runner._update_case_execution_summary(
        campaign,
        planned_case_count=2,
        executed=all_qualified,
    )
    campaign_runner._record_post_run_gate(
        campaign,
        gate_id="trusted_funcov",
        exit_code=1,
    )

    assert campaign_runner._finalize_campaign_status(campaign) == 1
    assert campaign["case_failure_count"] == 0
    assert campaign["post_run_gate_failure_count"] == 1
    assert campaign["status"] == "fail"
