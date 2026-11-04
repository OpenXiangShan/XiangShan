"""Shared assertions for backend reset scenarios."""


def assert_no_old_actions(record, new_pc):
    assert record["redirect"] == 0 and not any(record["resolves"]), record
    # callRetCommit carries every semantic commit, including non-CFIs with
    # rasAction=0. Check its actual action and fresh DUT-published FTQ owner,
    # rather than incorrectly requiring all commit-valid lanes to stay zero.
    for payload in record["call_ret_payloads"]:
        assert payload["ras_action"] == 0, payload
        assert payload["ftq_start_pc"] is not None and payload["ftq_start_pc"] >= new_pc, payload
    if record["commit"]:
        assert record["commit_start_pc"] is not None and record["commit_start_pc"] >= new_pc, record
