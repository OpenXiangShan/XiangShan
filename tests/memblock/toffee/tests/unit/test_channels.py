import pytest

from memblock_toffee.channels import ManagerAgent, ResponsePlan
from memblock_toffee.transactions import CompletionDisposition


class FakePolicy:
    def on_request(self, request: int) -> ResponsePlan[str]:
        return ResponsePlan(delay_cycles=request, response=f"response-{request}")


def test_manager_agent_delegates_to_independent_policy() -> None:
    manager = ManagerAgent(channel=object(), policy=FakePolicy())  # type: ignore[arg-type]
    plan = manager.plan(3)
    assert plan == ResponsePlan(delay_cycles=3, response="response-3")


def test_response_plan_rejects_invalid_cancellation() -> None:
    with pytest.raises(ValueError, match="delay"):
        ResponsePlan(delay_cycles=-1, response=None)
    with pytest.raises(ValueError, match="cancelled"):
        ResponsePlan(
            delay_cycles=1,
            response="payload",
            disposition=CompletionDisposition.CANCELLED,
        )
