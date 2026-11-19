"""Pure campaign counters and status derived from persisted results."""
from __future__ import annotations


def _update_case_execution_summary(
    campaign: dict,
    *,
    planned_case_count: int,
    executed: list[dict],
) -> int:
    """Refresh case-level status without conflating it with campaign gates."""
    case_failures = sum(
        1 for record in executed if not bool(record.get("qualification_pass"))
    )
    summary = {
        "planned_case_count": int(planned_case_count),
        "executed_case_count": len(executed),
        "qualified_case_count": len(executed) - case_failures,
        "case_failure_count": case_failures,
        "complete": len(executed) == int(planned_case_count),
    }
    campaign["execution"] = summary
    # Keep the original top-level counters for simple consumers of schema v1,
    # but make their case-only meaning explicit in the new fields above.
    campaign["planned_case_count"] = summary["planned_case_count"]
    campaign["executed_case_count"] = summary["executed_case_count"]
    campaign["case_failure_count"] = case_failures
    return case_failures


def _record_post_run_gate(campaign: dict, *, gate_id: str, exit_code: int | None) -> None:
    gates = campaign.setdefault("post_run_gates", [])
    gates.append(
        {
            "gate_id": gate_id,
            "status": (
                "skipped" if exit_code is None else "pass" if exit_code == 0 else "fail"
            ),
            "exit_code": exit_code,
        }
    )


def _finalize_campaign_status(campaign: dict) -> int:
    """Derive all aggregate campaign state solely from persisted sub-results."""
    execution = dict(campaign.get("execution") or {})
    case_failures = int(execution.get("case_failure_count") or 0)
    incomplete = not bool(execution.get("complete"))
    gate_failures = sum(
        1
        for gate in campaign.get("post_run_gates", [])
        if isinstance(gate, dict) and gate.get("status") == "fail"
    )
    campaign["post_run_gate_failure_count"] = gate_failures
    campaign["failure_count"] = case_failures + gate_failures
    campaign["status"] = (
        "pass"
        if not incomplete and case_failures == 0 and gate_failures == 0
        else "fail"
    )
    return campaign["failure_count"]
