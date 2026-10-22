import pytest

from memblock_toffee.transactions import (
    Completion,
    CompletionDisposition,
    OutstandingTable,
    TransactionKey,
    TransferEnvelope,
)


def test_outstanding_table_matches_out_of_order_exactly_once() -> None:
    table = OutstandingTable[str, str, str]("scoreboard")
    first = TransactionKey("rob", "a")
    second = TransactionKey("rob", "b")
    table.expect(first, "expected-a")
    table.expect(second, "expected-b")
    table.observe(second, "actual-b", cycle=4)
    table.observe(first, "actual-a", cycle=5)
    table.finish()
    assert table.outstanding == 0


def test_outstanding_table_rejects_duplicate_and_missing() -> None:
    table = OutstandingTable[str, str, str]("scoreboard")
    key = TransactionKey("rob", "a")
    table.expect(key, "expected")
    table.observe(key, "actual", cycle=3)
    table.observe(key, "duplicate", cycle=4)
    with pytest.raises(AssertionError, match="duplicate observation"):
        table.finish()

    missing = OutstandingTable[str, str, str]("scoreboard")
    missing.expect(key, "expected")
    with pytest.raises(AssertionError, match="missing completion"):
        missing.finish()


def test_outstanding_table_allows_explicit_cancellation() -> None:
    table = OutstandingTable[str, str, str]("scoreboard")
    key = TransactionKey("redirect", 7)
    table.expect(key, "expected")
    table.cancel(key, cycle=9)
    table.finish()
    assert table.outstanding == 0


def test_cancelled_completion_rejects_payload() -> None:
    table = OutstandingTable[str, str, str]("scoreboard")
    key = TransactionKey("redirect", 7)
    table.expect(key, "expected")
    table.observe(
        key,
        "unexpected",
        cycle=9,
        disposition=CompletionDisposition.CANCELLED,
    )
    with pytest.raises(AssertionError, match="carried a payload"):
        table.finish()


def test_cancelled_completion_without_payload_is_legal() -> None:
    table = OutstandingTable[str, str, str]("scoreboard")
    key = TransactionKey("redirect", 8)
    table.expect(key, "expected")
    table.observe(
        key,
        None,
        cycle=10,
        disposition=CompletionDisposition.CANCELLED,
    )
    table.finish()


def test_transfer_and_completion_contracts_reject_invalid_payloads() -> None:
    with pytest.raises(ValueError, match="cycle"):
        TransferEnvelope(key="request", payload="data", cycle=-1)
    with pytest.raises(ValueError, match="payload"):
        Completion(
            key="request",
            payload="data",
            cycle=3,
            disposition=CompletionDisposition.CANCELLED,
        )
