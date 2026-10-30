#!/usr/bin/env python3
"""Qualify schema77 aggregates with independent offer/physical-edge histories.

This ledger owns source attempts and observes accepted lane identities. It does
not call the stream scheduler or derive expected counters from its decisions.
Transport-only histories do not claim DUT or full-CPU reachability coverage.
"""
from __future__ import annotations

from collections import Counter
import copy
import random
from pathlib import Path
import sys
import unittest
from unittest import mock

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "scripts"))
import verify_regression as verifier


def csv(values):
    return ",".join(map(str, values))


def change(result, field, index, value):
    result = copy.deepcopy(result)
    values = list(map(int, result[field].split(",")))
    values[index] = value
    result[field] = csv(values)
    return result


class SourceHistory:
    """Each token denotes one offer, with one terminal disposition."""

    def __init__(self):
        self.transport = [0] * 10
        self.ports = [0] * 16
        self.live = []
        self.accepted = {}
        self.pending = {}
        self.stalled = set()
        self.categories = {}
        self.initial_issues = self.sta_retries = self.vector_replays = self.graph_refetches = 0
        self.admitted = self.retired = self.redirects = self.next_token = 0

    def admit(self):
        assert len(self.live) < 8
        owner = self.admitted
        self.transport[6] += bool(self.stalled)
        self.admitted += 1
        self.live.append(owner)
        return owner

    def offer(self, owner, *kinds, retry=False, replay=False):
        assert owner in self.live
        assert len(kinds) == 1 or kinds == (1, 2)
        assert not retry or kinds == (1,)
        assert not replay or kinds == (3,)
        tokens = []
        self.transport[9] += 1
        for kind in kinds:
            assert kind in range(4)
            token = self.next_token
            self.next_token += 1
            self.pending[token] = (owner, kind)
            self.categories[token] = "retry" if retry else "replay" if replay else "initial"
            self.transport[0] += 1
            tokens.append(token)
        assert len(self.pending) <= 9
        self.transport[5] = max(self.transport[5], len(self.pending))
        return tokens

    def edge(self, *tokens):
        assert len(set(tokens)) == len(tokens)
        lanes = Counter(self.pending[token][1] for token in tokens)
        for kind, limit in enumerate((3, 2, 2, 2)):
            assert lanes[kind] <= limit
        if tokens:
            mask = sum(1 << kind for kind in lanes)
            self.ports[mask] += 1
        for token in tokens:
            self.accepted[token] = self.pending.pop(token)
            self.stalled.discard(token)
            self.transport[1] += 1
            category = self.categories[token]
            if category == "retry":
                self.sta_retries += 1
            elif category == "replay":
                self.vector_replays += 1
            else:
                self.initial_issues += 1
        # Retained offers have now survived an edge without being accepted.
        self.stalled.update(self.pending)

    def timeout(self, token):
        assert self.pending[token][1] == 3
        del self.pending[token]
        self.stalled.discard(token)
        self.transport[2] += 1

    def redirect(self, anchor):
        assert any(owner > anchor for owner in self.live)
        self.redirects += 1
        for token, (owner, _) in list(self.pending.items()):
            if owner > anchor:
                del self.pending[token]
                self.stalled.discard(token)
                self.transport[3] += 1
        for token, (owner, _) in list(self.accepted.items()):
            if owner > anchor:
                del self.accepted[token]
                self.transport[4] += 1
        self.live = [owner for owner in self.live if owner <= anchor]

    def retire(self, owner):
        assert self.live and self.live[0] == owner
        assert not any(age == owner for age, _ in self.pending.values())
        assert any(age == owner for age, _ in self.accepted.values())
        self.transport[7] += any(self.pending[token][0] > owner for token in self.stalled)
        self.live.pop(0)
        self.retired += 1

    def graph_helper(self, *, refetch=False):
        self.transport[8] += 1
        if refetch:
            self.graph_refetches += 1
        else:
            self.initial_issues += 1

    def result(self):
        assert not self.live and not self.pending
        stream, canceled = [0] * 12, [0] * 29
        stream[:3] = [int(self.admitted != 0), self.admitted, self.retired]
        stream[8:10] = [self.initial_issues, self.vector_replays]
        canceled[0] = self.redirects
        recovery = [0] * 20
        recovery[8] = self.graph_refetches
        return dict(constraint_schema=77, stream=csv(stream), stream_cancel=csv(canceled),
                    stream_scalar_retries=self.sta_retries, stream_recovery=csv(recovery),
                    stream_transport=csv(self.transport), stream_issue_ports=csv(self.ports))


def lane_history(mask, counts):
    history = SourceHistory()
    tokens = []
    # Same-owner STA/STD selections can fill both physical producer sets.
    paired = min(counts[1], counts[2])
    for _ in range(paired):
        tokens.extend(history.offer(history.admit(), 1, 2))
    for kind, count in enumerate(counts):
        for _ in range(count - (paired if kind in (1, 2) else 0)):
            tokens.extend(history.offer(history.admit(), kind))
    history.edge(*tokens)
    for owner in history.live.copy():
        history.retire(owner)
    result = history.result()
    assert result["stream_issue_ports"].split(",")[mask] == "1"
    return result


def retry_cancel_history(*, helpers=1):
    history = SourceHistory()
    head = history.admit()
    history.edge(*history.offer(head, 0))
    vector = history.admit()
    first = history.offer(vector, 3)[0]
    history.edge()
    younger = history.admit()  # Admission observes the retained vector offer.
    history.retire(head)       # Retirement observes the stalled younger offer.
    history.timeout(first)
    history.edge(*history.offer(vector, 3), *history.offer(younger, 0))
    history.retire(vector)
    history.retire(younger)
    accepted_tail = history.admit()
    history.edge(*history.offer(accepted_tail, 0))
    unaccepted_tail = history.admit()
    history.offer(unaccepted_tail, 1, 2)
    history.edge()
    history.redirect(younger)
    for _ in range(helpers):
        history.graph_helper()
    return history.result()


def waiting_history():
    # The checked scalar head completes before three attempts to offer its
    # younger vector: two preaccept timeouts and one real accepted edge.
    canceled = [0] * 29
    canceled[24] = 2
    ports = [0] * 16
    ports[1] = ports[8] = 1
    return dict(constraint_schema=77, transactions=2, target_concurrent=1000,
        target_concurrent_stream=1000, target_stream_min_instructions=1,
        target_stream_max_instructions=256, target_stream_store_pair=0,
        target_stream_dependency=0, target_scalar_fp=0, target_ops=csv([1] * 15),
        concurrent=csv([0] * 5), concurrent_ops=csv([0] * 5),
        stream="1,2,2,0,0,0,2,0,2,0,0,2", stream_ops="1,1,0,0,0",
        stream_issue="1,0,0,1,0,0", stream_vector_uops="1,0",
        stream_scalar_retries=0, stream_sta=csv([0] * 11),
        stream_store_pair=csv([0] * 5), stream_store_retirement=csv([0] * 10),
        stream_vstart_flush=csv([0] * 4), stream_history="0,2,0,0",
        stream_waiting_head="0,3", stream_dependencies=csv([0] * 9),
        stream_recovery=csv([0] * 20), stream_cancel=csv(canceled),
        stream_transport="4,2,2,0,0,1,0,0,0,4", stream_issue_ports=csv(ports))


def vector_wait_history(replays, overlap_count, timeouts=0, store_vector=False, canceled_unissued=False):
    """One slow STA, a vector obligation and independently timed feedback.

    Physical edges distinguish accepted replay from preaccept timeout. An
    optional younger two-uop vector generation is canceled before acceptance;
    it owns admitted uops but cannot contribute accepted overlap.
    """
    assert 0 <= overlap_count <= replays + 1
    history = SourceHistory()
    edge = 0
    def clock(*tokens):
        nonlocal edge
        edge += 1
        history.edge(*tokens)
        return edge
    scalar = history.admit()
    start = clock(*history.offer(scalar, 1))
    vector = history.admit()
    admissions = [clock()]
    if canceled_unissued:
        killed = history.admit()
        history.offer(killed, 3)
        admissions.append(clock())
        history.redirect(vector)
    for _ in range(timeouts):
        token = history.offer(vector, 3)[0]
        clock()
        history.timeout(token)
    vector_edges = [clock(*history.offer(vector, 3))]
    for _ in range(replays):
        vector_edges.append(clock(*history.offer(vector, 3, replay=True)))
    feedback = vector_edges[overlap_count] if overlap_count < len(vector_edges) else edge + 1
    while edge <= feedback:
        clock()
    clock(*history.offer(scalar, 2))
    history.retire(scalar)
    history.retire(vector)
    result = waiting_history()
    result.update(history.result())
    stream = list(map(int, result["stream"].split(",")))
    stream[3] = history.admitted - 1
    stream[5] = 1
    stream[6] = history.admitted
    stream[7] = 2
    stream[11] = 2
    issues = [0, 1, 1, int(not store_vector), int(store_vector), 0]
    uops = [0, 0]
    uops[int(store_vector)] = 1 + 2 * int(canceled_unissued)
    operations = [0, int(not store_vector), 1, int(store_vector), 0]
    sta = [1, 0, 1, 0, sum(start < cycle < feedback for cycle in admissions), 0, 0, 0,
           sum(start < cycle < feedback for cycle in vector_edges), 0, 1]
    canceled = list(map(int, result["stream_cancel"].split(",")))
    canceled[24] = history.admitted
    if canceled_unissued:
        canceled[4 if store_vector else 2] = 1
        canceled[13 if store_vector else 12] = 2
        canceled[22] = canceled[23] = 1
    retirement = [1, int(store_vector), int(store_vector), 0, 1 + int(store_vector),
                  0, 0, 0, 0, 1 + int(store_vector)]
    result.update(stream=csv(stream), stream_ops=csv(operations), stream_issue=csv(issues),
                  stream_vector_uops=csv(uops), stream_sta=csv(sta), stream_cancel=csv(canceled),
                  stream_store_retirement=csv(retirement), stream_waiting_head="0,0")
    assert sta[8] == overlap_count
    return result


class StreamTransportAccountingTest(unittest.TestCase):
    def test_vector_wait_overlap_counts_accepted_replays_and_preserves_legacy_scope(self):
        checked = 0
        for store in (False, True):
            for replays in range(9):
                for overlaps in range(replays + 2):
                    for timeouts in (0, 1, 3):
                        for canceled in (False, True):
                            result = vector_wait_history(replays, overlaps, timeouts, store, canceled)
                            verifier._check_stream_transport(result)
                            verifier._check_stream_accounting(result)
                            # The same source work's old helper-level observation
                            # contains at most its one initial vector-uop event.
                            legacy = change(result, "stream_sta", 8, min(overlaps, 1))
                            verifier._check_stream_accounting(dict(legacy, constraint_schema=76))
                            checked += 1
        print(f"stream_STA_vector_replay_histories={checked}")

    def test_vector_wait_overlap_cannot_use_timeouts_or_canceled_unissued_uops(self):
        for store in (False, True):
            for replays in (0, 1, 4, 9):
                for timeouts in (0, 1, 3):
                    for canceled in (False, True):
                        result = vector_wait_history(replays, replays + 1, timeouts, store, canceled)
                        corrupt = change(result, "stream_sta", 8, replays + 2)
                        with self.subTest(store=store, replays=replays, timeouts=timeouts, canceled=canceled):
                            verifier._check_stream_transport(corrupt)
                            with self.assertRaisesRegex(verifier.VerificationError, "overlaps exceed accepted work"):
                                verifier._check_stream_accounting(corrupt)
        legacy = dict(vector_wait_history(3, 4), constraint_schema=76)
        with self.assertRaisesRegex(verifier.VerificationError, "overlaps exceed accepted work"):
            verifier._check_stream_accounting(legacy)

    def test_physical_edge_masks_and_lane_capacity_boundaries(self):
        rng = random.Random(7701)
        checked = 0
        for mask in range(1, 16):
            # A mask records present kinds; it does not record lane multiplicity.
            for full in (False, True):
                counts = [(limit if full else 1) if mask & (1 << kind) else 0
                          for kind, limit in enumerate((3, 2, 2, 2))]
                verifier._check_stream_transport(lane_history(mask, counts))
                checked += 1
            for _ in range(16):
                counts = [rng.randint(1, limit) if mask & (1 << kind) else 0
                          for kind, limit in enumerate((3, 2, 2, 2))]
                verifier._check_stream_transport(lane_history(mask, counts))
                checked += 1
        print(f"stream_transport_source_histories={checked}")

    def test_retained_retry_cancel_and_graph_helper_history(self):
        result = retry_cancel_history()
        self.assertEqual(result["stream_transport"], "7,4,1,2,1,2,1,1,1,6")
        verifier._check_stream_transport(result)
        # Graph helpers remain outside session attempts and accepted-edge bins.
        many_helpers = retry_cancel_history(helpers=500)
        verifier._check_stream_transport(many_helpers)
        for field, index, value in (
                ("stream_transport", 0, 8), ("stream_transport", 1, 3),
                ("stream_transport", 2, 2), ("stream_transport", 3, 3),
                ("stream_transport", 4, 5), ("stream_transport", 5, 0),
                ("stream_transport", 5, 8), ("stream_transport", 5, 10),
                ("stream_transport", 6, 5), ("stream_transport", 7, 4),
                ("stream_transport", 9, 8), ("stream_transport", 9, 3),
                ("stream_cancel", 0, 0), ("stream_issue_ports", 0, 1)):
            with self.subTest(field=field, index=index, value=value):
                with self.assertRaises(verifier.VerificationError):
                    verifier._check_stream_transport(change(result, field, index, value))

    def test_accepted_sta_retry_and_vector_replay_keep_distinct_attempts(self):
        history = SourceHistory()
        store = history.admit()
        history.edge(*history.offer(store, 1, 2))
        # A checked STA miss permits a later retry for the same store owner.
        retry = history.offer(store, 1, retry=True)
        history.edge()
        load = history.admit()
        history.edge(*history.offer(load, 0))
        history.edge(*retry)
        history.retire(store)
        history.retire(load)
        vector = history.admit()
        history.edge(*history.offer(vector, 3))
        # Accepted vector replay is a fresh session attempt, unlike a timeout
        # before an acceptance; both accepted attempts remain in the histogram.
        history.edge(*history.offer(vector, 3, replay=True))
        history.retire(vector)
        history.graph_helper()
        history.graph_helper(refetch=True)
        result = history.result()
        self.assertEqual(result["stream_transport"], "6,6,0,0,0,2,1,0,2,5")
        self.assertEqual(list(map(int, result["stream_issue_ports"].split(",")))[8], 2)
        verifier._check_stream_transport(result)

    def test_architectural_issue_corruption_rejects_with_balanced_transport(self):
        history = SourceHistory()
        tokens = [token for _ in range(3) for token in history.offer(history.admit(), 0)]
        history.edge(*tokens)
        for owner in history.live.copy():
            history.retire(owner)
        history.graph_helper()
        history.graph_helper()
        history.graph_helper(refetch=True)
        base = history.result()
        verifier._check_stream_transport(base)
        corruptions = [change(base, "stream", 8, 6),
                       change(base, "stream", 9, 1),
                       change(base, "stream_recovery", 8, 2),
                       dict(base, stream_scalar_retries=1)]
        for bad in corruptions:
            # Every offer disposition, accepted lane, mask and stall observation
            # is unchanged. Only the independent architectural ledger differs.
            self.assertEqual(bad["stream_transport"], base["stream_transport"])
            self.assertEqual(bad["stream_issue_ports"], base["stream_issue_ports"])
            with self.subTest(bad=bad), self.assertRaisesRegex(verifier.VerificationError, "session acceptances"):
                verifier._check_stream_transport(bad)
        # Graph helpers are architectural issues excluded from session accepts.
        with self.assertRaisesRegex(verifier.VerificationError, "session acceptances"):
            verifier._check_stream_transport(change(base, "stream_transport", 8, 4))

    def test_histogram_rejects_underreported_and_overreported_lanes(self):
        for mask in range(1, 16):
            minimum = lane_history(mask, [int(bool(mask & (1 << kind))) for kind in range(4)])
            maximum = lane_history(mask, [limit if mask & (1 << kind) else 0
                                         for kind, limit in enumerate((3, 2, 2, 2))])
            for original, delta in ((minimum, -1), (maximum, 1)):
                wrong = copy.deepcopy(original)
                transport = list(map(int, wrong["stream_transport"].split(",")))
                transport[0] += delta
                transport[1] += delta
                # Keep unrelated attempt conservation and selection constraints
                # valid, so rejection must come from physical accepted edges.
                transport[9] = transport[0]
                transport[5] = min(transport[5], transport[0])
                wrong["stream_transport"] = csv(transport)
                with self.subTest(mask=mask, delta=delta):
                    with self.assertRaisesRegex(verifier.VerificationError, "kind sets"):
                        verifier._check_stream_transport(wrong)

    def test_empty_and_sparse_streams_do_not_require_multikind_acceptance(self):
        empty = SourceHistory().result()
        verifier._check_stream_transport(empty)
        sparse = lane_history(1, [1, 0, 0, 0])
        verifier._check_stream_transport(sparse)
        # A canceled offer and an accepted killed tail each need a redirect.
        for slot in (3, 4):
            wrong = change(sparse, "stream_transport", slot, 1)
            if slot == 3:
                wrong = change(wrong, "stream_transport", 0, 2)
                wrong = change(wrong, "stream_transport", 9, 2)
            with self.subTest(slot=slot), self.assertRaisesRegex(verifier.VerificationError, "redirect"):
                verifier._check_stream_transport(wrong)
        for wrong in (change(empty, "stream_transport", 8, 1),
                      change(empty, "stream", 0, 1),
                      change(sparse, "stream", 0, 0)):
            with self.assertRaises(verifier.VerificationError):
                verifier._check_stream_transport(wrong)
        # A nonempty epoch containing only an unaccepted timed-out attempt is
        # not a completed stream, even though its attempt ledger is balanced.
        no_acceptance = dict(sparse, stream_transport="1,0,1,0,0,1,0,0,0,1",
                             stream_issue_ports=csv([0] * 16))
        with self.assertRaisesRegex(verifier.VerificationError, "no persistent"):
            verifier._check_stream_transport(no_acceptance)

    def test_uint64_domain_and_malformed_observations(self):
        base = retry_cancel_history()
        limit = (1 << 64) - 1
        huge = change(lane_history(1, [1, 0, 0, 0]), "stream_issue_ports", 1, limit)
        for slot in (0, 1, 9):
            huge = change(huge, "stream_transport", slot, limit)
        huge = change(huge, "stream", 8, limit)
        verifier._check_stream_transport(huge)
        for field, width in (("stream_transport", 10), ("stream_issue_ports", 16)):
            for value in (None, True, [], 0, "", "x", csv([0] * (width - 1)),
                          csv([0] * (width + 1)), csv([-1] + [0] * (width - 1))):
                with self.subTest(field=field, value=value), self.assertRaises(verifier.VerificationError):
                    verifier._check_stream_transport({**base, field: value})
            missing = copy.deepcopy(base)
            del missing[field]
            with self.assertRaises(verifier.VerificationError):
                verifier._check_stream_transport(missing)
            for slot in range(width):
                with self.subTest(field=field, slot=slot), self.assertRaisesRegex(verifier.VerificationError, "uint64"):
                    verifier._check_stream_transport(change(base, field, slot, limit + 1))
        # The lower histogram sum must use unbounded arithmetic, not wrap at64.
        with self.assertRaisesRegex(verifier.VerificationError, "kind sets"):
            verifier._check_stream_transport(change(huge, "stream_issue_ports", 15, limit))

    def test_schema77_waiting_head_uses_attempts_and_legacy_helpers(self):
        base = waiting_history()
        verifier._check_stream_accounting(base)
        verifier._check_stream_transport(base)
        with self.assertRaises(verifier.VerificationError):
            verifier._check_stream_accounting(dict(base, constraint_schema=76))
        helpers = change(base, "stream_transport", 8, 2)
        verifier._check_stream_accounting(dict(helpers, stream_waiting_head="0,5"))
        for wrong in (dict(base, stream_waiting_head="0,4"),
                      dict(base, stream_waiting_head="2,0"),
                      dict(helpers, stream_waiting_head="0,6")):
            with self.assertRaisesRegex(verifier.VerificationError, "completed-head"):
                verifier._check_stream_accounting(wrong)

    def test_both_verifier_modes_require_new_fields_and_old_schemas_ignore_them(self):
        for checker in (verifier._check_mixed_accounting, verifier._check_mixed_coverage):
            with mock.patch.object(verifier, "_check_constraint_coverage"), self.subTest(mode=checker.__name__), self.assertRaisesRegex(verifier.VerificationError, "stream_transport"):
                checker({"constraint_schema": 77})
        for schema in range(77):
            verifier._check_stream_transport(dict(constraint_schema=schema,
                                                  stream_transport=None, stream_issue_ports=None))
        sparse = lane_history(1, [1, 0, 0, 0])
        verifier._check_mixed_accounting(dict(sparse, lq="1+0/1", sq="0+0/0"))


if __name__ == "__main__":
    unittest.main()
