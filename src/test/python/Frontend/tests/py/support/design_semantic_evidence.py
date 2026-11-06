"""Strict offline review contracts. No driver, mark, or runtime qualification.

Callers must bind actual same-run observations; synthetic tests exercise rules
only. Existing 09 counter equation API and 13 accepted-window API stay separate.
"""
from tests.py.support.gpa_register_evidence import GpaCountSnapshot, check_adjacent_gpa_count
from tests.py.support.ifu_exception_scope import assert_no_effective_ifu_exception


def _integer(record, name, low=0, high=None):
    value = record[name]
    assert type(value) is int and value >= low and (high is None or value <= high), name
    return value


def check_pbmt_fetch_evidence(record):
    """Check native PBMT/PMA/NC-vs-IO state, not ScenarioBuilder's coarse path."""
    stage = _integer(record, 's2xlate', high=3)
    s1 = _integer(record, 's1_pbmt', high=2)
    s2 = _integer(record, 's2_pbmt', high=2)
    mmio = _integer(record, 'pma_mmio', high=1)
    expected = s2 if stage == 2 else (s1 or s2) if stage == 3 else s1
    assert _integer(record, 'native_pbmt', high=2) == expected, 'PBMT stage priority'
    assert _integer(record, 'is_mmio', high=1) == mmio, 'PMA/isMmio identity'
    assert _integer(record, 'pma_probe_pa') == _integer(record, 'translated_pa'), 'PMA must use translated PA'
    uncache = bool(mmio or expected)
    wait = bool(mmio or expected == 2)
    assert _integer(record, 'uses_uncache', high=1) == int(uncache), 'fetch path'
    if uncache:
        assert _integer(record, 'uncache_req_valid', high=1) == 1, 'real uncache request required'
        assert record['state_after_request'] == ('WaitLastCommit' if wait else 'SendReq'), 'commit wait policy'
    return {'effective_pbmt': expected, 'pma_mmio': bool(mmio), 'uncache': uncache,
            'wait_last_commit': wait, 'runtime_qualified': False, 'canonical_promotion': False}


def check_a2_timeout_evidence(samples, context):
    """Count inner qualified fires across calibrated adjacent register boundaries.

    Counter equation alone cannot establish reachability. Require the entire
    saved-request/filter identity and outstanding-response ledger as well.
    """
    assert context['start_s2xlate'] in (0, 1, 3), 'onlyStage2 cannot start need_gpa'
    assert context['response_scope'] == 'tlb_inner', 'external agent logs cannot count'
    assert context['get_gpa_origin'] == 'filter_saved_request', 'outer PtwRespS2 has no getGpa'
    assert context['phase_calibrated'] is True, 'registered sampling phase unqualified'
    assert context['legal_reachability_proved'] is True, 'do not replay or inject responses to fill counter'
    assert context['single_port'] is True, 'multiport is an upstream qualification layer'
    assert len(samples) >= 513, 'need 511 qualified increments then a separate full-clear edge'
    states = [GpaCountSnapshot.from_observed(item) for item in samples]
    assert states[0].count == 0 and states[0].need_gpa == 1
    assert states[-2].count == 511 and states[-1].count == 0
    ids = []
    for index, (before, after) in enumerate(zip(states, states[1:])):
        proof = check_adjacent_gpa_count(before, after)
        if index < len(states) - 2:
            assert not proof['clear_priority'], 'early reset is not one 511-response timeout'
        if proof['qualifying_response'] and not proof['clear_priority']:
            event = samples[index]
            assert event['filter_saved_get_gpa'] == 1 and event['filter_identity_match'] is True
            assert event['outstanding_request_proved'] is True
            assert event['filter_vpn'] == context['pending_vpn']
            assert event['filter_s2xlate'] == context['start_s2xlate']
            ids.append(event['accepted_response_id'])
    assert len(ids) == 511, 'need 511 inner qualifying handshakes'
    assert len(set(ids)) == 511, 'duplicated response evidence cannot prove legal reachability'
    last = samples[-1]
    assert states[-1].need_gpa == 0
    assert _integer(last, 'resp_gpa_refill', high=1) == 0
    assert _integer(last, 'pending_vpn') == 0
    assert _integer(last, 'robidx_valid', high=1) == 0
    assert context['recovery_observed'] is True, 'clear alone does not prove forward progress'
    return {'qualified_inner_responses': 511, 'full_then_clear': True,
            'runtime_qualified': False, 'canonical_promotion': False}


def check_satp_window_and_delivery(window, priority, deliveries, context):
    """IFU window validity plus exact downstream no-fault witnesses.

    This API never upgrades the original satp FAIL. A mask/enq nonintersection
    proves only one IFU window; missing later outputs/phase/identity fail here.
    """
    for field in ("io_toIBuffer_bits_valid", "io_toIBuffer_bits_enqEnable",
                  "io_toIBuffer_bits_exceptionMask"):
        assert _integer(window, field) < (1 << 36), "native IFU lane mask width"
    scope = assert_no_effective_ifu_exception(window)
    assert _integer(window, "s2_useUncacheFetch", high=1) == 0, "uncache exception mux requires separate source proof"
    first = _integer(window, 's2_icacheMeta_0_exception_value', high=5)
    second = _integer(window, 's2_icacheMeta_1_exception_value', high=5)
    ill = _integer(priority, 'first_has_rvc_ill', high=1)
    rvc = _integer(priority, 'rvc_exception_type', high=5)
    assert rvc in (0, 4), 'RVC source encoding'
    assert not ill or rvc == 4, 'first-block illegal priority inconsistent'
    expected = first if first else rvc if ill else second if second else rvc
    assert _integer(window, 'io_toIBuffer_bits_exceptionType_value', high=5) == expected, 'exception priority/type'
    assert context['phase_calibrated'] is True, 'downstream phase unqualified'
    assert context['pending_lanes_at_end'] == [], 'downstream delivery not completed'
    assert context['checker_pass'] is True and context['monitor_pass'] is True
    assert context['oracle_armed'] is True and context['oracle_pass'] is True
    assert context['provenance_current'] is True
    assert context['post_context_recovery_pass'] is True
    active = [i for i in range(36) if scope['effective_entry_mask'] >> i & 1]
    expected_lanes = context['accepted_lanes']
    assert {entry['lane'] for entry in expected_lanes} == set(active), 'all accepted lanes must be identified'
    assert len(expected_lanes) == len(active), 'duplicate accepted lane identity'
    used = set()
    for entry in expected_lanes:
        assert len(entry['ftq']) == 2 and entry['ftq'][0] in (0, 1)
        candidates = [(i, item) for i, item in enumerate(deliveries)
                      if item['run_id'] == context['run_id'] and item['epoch'] == context['epoch']
                      and item['accept_cycle'] == context['accept_cycle'] and item['lane'] == entry['lane']
                      and item['ftq'] == entry['ftq'] and item['pc'] == entry['pc']]
        assert len(candidates) == 1, 'missing/ambiguous exact downstream lane witness'
        index, item = candidates[0]
        assert index not in used
        used.add(index)
        assert item['valid'] == item['ready'] == 1, 'output must be accepted'
        assert item['delivery_cycle'] >= context['accept_cycle']
        assert item['route'] in ('bypass', 'queue')
        assert item['exception_type'] == item['exception_vec'] == 0, 'later delivered exception'
        if item['route'] == 'bypass':
            assert item['bypass_exception_mask'] == 0, 'bypass injected an exception'
        else:
            assert item['queue_exception_owned'] == 0, 'queue recorded inactive exception'
    return {'accepted_ifu_window': scope, 'downstream_lanes_checked': len(active),
            'runtime_qualified': False, 'canonical_promotion': False}
