"""Shared assertions for backend reset scenarios."""


def assert_no_old_actions(record, new_pc):
    assert record["redirect"] == 0 and not any(record["resolves"]), record
    if record["commit"]:
        assert record["commit_start_pc"] is not None and record["commit_start_pc"] >= new_pc, record
