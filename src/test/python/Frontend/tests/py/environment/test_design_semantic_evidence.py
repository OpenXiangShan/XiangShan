"""Offline synthetic rule checks, never real DUT acceptance or canonical HIT."""
from copy import deepcopy
import pytest
from tests.py.support.design_semantic_evidence import (
    check_pbmt_fetch_evidence, check_a2_timeout_evidence, check_satp_window_and_delivery,
)
from tests.py.environment.test_ifu_exception_metadata_scope import _window


@pytest.mark.parametrize('s1', range(3))
@pytest.mark.parametrize('s2', range(3))
def test_reviewed_model_allstage_prefers_nonzero_s1(s1, s2):
    from env.model.page_table_model import PageTableModel
    table = PageTableModel('sv39')
    table.map_page(0x80200, 0x80400, pbmt=s1)
    table.map_stage2_page(0x80400, 0x80600, pbmt=s2)
    pa, ok, metadata = table.translate(0x80200000, s2xlate=3)
    assert ok and pa == 0x80600000
    assert metadata['fetch_path'] == {0: 'pma', 1: 'uncache', 2: 'mmio'}[s1 or s2]


def test_scenario_builder_coarse_uncache_does_not_distinguish_nc_from_io():
    from env.core.frontend_env import FrontendEnv
    from env.runtime.dut_factory import FakeDUTFrontend
    from env.sequences import TranslationScenario, TranslationScenarioBuilder, TranslationPte
    outcomes = []
    for pbmt in (1, 2):
        env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
        scenario = TranslationScenario(scenario_id='pbmt-coarse-' + str(pbmt), va=0x80200000,
            pa=0x80400000, payload=b'\x13\x00\x00\x00', s1_pte=TranslationPte(pbmt=pbmt))
        outcomes.append(TranslationScenarioBuilder(env).build(scenario).expected_page_outcomes[0])
    assert [item['expected_path'] for item in outcomes] == ['uncache', 'uncache']
    assert [item['fetch_path'] for item in outcomes] == ['uncache', 'mmio']


def pbmt_record(s1=1, s2=2, stage=3, mmio=0):
    selected = s2 if stage == 2 else (s1 or s2) if stage == 3 else s1
    return dict(s1_pbmt=s1, s2_pbmt=s2, s2xlate=stage, pma_mmio=mmio,
                native_pbmt=selected, is_mmio=mmio, pma_probe_pa=0x80600000,
                translated_pa=0x80600000, uses_uncache=int(bool(selected or mmio)),
                uncache_req_valid=1, state_after_request='WaitLastCommit' if mmio or selected == 2 else 'SendReq')


@pytest.mark.parametrize('s1', range(3))
@pytest.mark.parametrize('s2', range(3))
@pytest.mark.parametrize('mmio', (0, 1))
def test_native_pbmt_nc_io_pma_and_commit_wait_are_independent(s1, s2, mmio):
    result = check_pbmt_fetch_evidence(pbmt_record(s1, s2, mmio=mmio))
    assert result['effective_pbmt'] == (s1 or s2)
    assert result['wait_last_commit'] == bool(mmio or (s1 or s2) == 2)
    assert not result['canonical_promotion'] and not result['runtime_qualified']


@pytest.mark.parametrize('changes', [
    {'native_pbmt': 2}, {'state_after_request': 'WaitLastCommit'}, {'is_mmio': 1},
    {'pma_probe_pa': 0x80200000}, {'uses_uncache': 0}, {'uncache_req_valid': 0},
])
def test_coarse_path_or_wrong_s2_priority_cannot_qualify_nc(changes):
    with pytest.raises(AssertionError):
        check_pbmt_fetch_evidence({**pbmt_record(), **changes})


def test_nc_with_actual_mmio_must_wait_last_commit():
    record = pbmt_record(mmio=1)
    assert check_pbmt_fetch_evidence(record)['wait_last_commit']
    record['state_after_request'] = 'SendReq'
    with pytest.raises(AssertionError):
        check_pbmt_fetch_evidence(record)


@pytest.mark.parametrize('stage', (0, 1, 2))
def test_single_stage_policy_stays_separate_from_active_context(stage):
    assert check_pbmt_fetch_evidence(pbmt_record(stage=stage))['effective_pbmt'] == (2 if stage == 2 else 1)


def count_trace():
    samples = [dict(cycle=i, count=i, need_gpa=1, ptw_resp_valid=1,
                    ptw_resp_ready=1, ptw_resp_get_gpa=1, need_gpa_wire=0, need_gpa_state_clear=0,
                    filter_saved_get_gpa=1, filter_identity_match=True, outstanding_request_proved=True,
                    filter_vpn=0x80200, filter_s2xlate=3, accepted_response_id=i)
               for i in range(512)]
    samples[-1]['ptw_resp_valid'] = 0  # Full clear needs no 512th response.
    samples.append(dict(samples[-1], cycle=512, count=0, need_gpa=0, resp_gpa_refill=0,
                        pending_vpn=0, robidx_valid=0))
    context = dict(start_s2xlate=3, response_scope='tlb_inner', get_gpa_origin='filter_saved_request',
                   phase_calibrated=True, legal_reachability_proved=True, single_port=True,
                   pending_vpn=0x80200, recovery_observed=True)
    return samples, context


def test_511_inner_qualified_fires_then_clear_and_progress_contract():
    samples, context = count_trace()
    result = check_a2_timeout_evidence(samples, context)
    assert result['qualified_inner_responses'] == 511
    assert not result['runtime_qualified']


@pytest.mark.parametrize('changes', [
    {'start_s2xlate': 2}, {'response_scope': 'external_agent'},
    {'get_gpa_origin': 'outer_response_getGpa'}, {'phase_calibrated': False},
    {'legal_reachability_proved': False}, {'single_port': False}, {'recovery_observed': False},
])
def test_unqualified_a2_context_rejected(changes):
    samples, context = count_trace()
    with pytest.raises(AssertionError):
        check_a2_timeout_evidence(samples, {**context, **changes})


@pytest.mark.parametrize('mode', ['idle', 'wrong-get-gpa', 'wrong-stage', 'duplicate', 'early-clear',
                                 'missing-probe', 'missing-last-refill', 'no-outstanding', 'skipped-edge'])
def test_timeout_not_proved_by_cycles_agent_or_replayed_responses(mode):
    samples, context = count_trace()
    if mode == 'idle':
        for sample in samples[:-1]: sample['ptw_resp_valid'] = 0
    elif mode == 'wrong-get-gpa': samples[20]['ptw_resp_get_gpa'] = 0
    elif mode == 'wrong-stage': samples[20]['filter_s2xlate'] = 2
    elif mode == 'duplicate': samples[20]['accepted_response_id'] = samples[19]['accepted_response_id']
    elif mode == 'early-clear': samples[20]['need_gpa_state_clear'] = 1
    elif mode == 'missing-probe': del samples[20]['ptw_resp_get_gpa']
    elif mode == 'missing-last-refill': del samples[-1]['resp_gpa_refill']
    elif mode == 'no-outstanding': samples[20]['outstanding_request_proved'] = False
    elif mode == 'skipped-edge': samples[20]['cycle'] += 1
    with pytest.raises((AssertionError, KeyError)):
        check_a2_timeout_evidence(samples, context)


def satp_trace(route='queue'):
    window = _window()
    window["s2_useUncacheFetch"] = 0
    window['io_toIBuffer_bits_valid'] = window['io_toIBuffer_bits_enqEnable'] = 1
    context = dict(run_id='synthetic', epoch=1, accept_cycle=10, phase_calibrated=True,
                   pending_lanes_at_end=[], checker_pass=True, monitor_pass=True, oracle_armed=True,
                   oracle_pass=True, provenance_current=True, post_context_recovery_pass=True,
                   accepted_lanes=[dict(lane=0, ftq=[1, 7], pc=0x80200000)])
    delivery = dict(run_id='synthetic', epoch=1, accept_cycle=10, lane=0, ftq=[1, 7], pc=0x80200000,
                    valid=1, ready=1, delivery_cycle=11, route=route, exception_type=0, exception_vec=0,
                    bypass_exception_mask=0, queue_exception_owned=0)
    priority = dict(first_has_rvc_ill=0, rvc_exception_type=0)
    return window, priority, [delivery], context


@pytest.mark.parametrize('route', ['queue', 'bypass'])
def test_inactive_bus_only_qualifies_exact_later_clean_delivery(route):
    trace = satp_trace(route)
    assert check_satp_window_and_delivery(*trace)['downstream_lanes_checked'] == 1


@pytest.mark.parametrize('change', ['later-fault', 'wrong-pc', 'wrong-ftq-wrap', 'wrong-epoch',
    'missing-output', 'duplicated-output', 'not-accepted', 'pending', 'phase', 'oracle-unarmed',
    'bypass-error', 'queue-error', 'bad-priority', 'missing-priority', 'no-recovery', 'uncache'])
def test_mask_nonintersection_cannot_hide_later_error_or_missing_proof(change):
    window, priority, outputs, context = satp_trace('bypass' if change == 'bypass-error' else 'queue')
    if change == 'later-fault': outputs[0]['exception_vec'] = 1
    elif change == 'wrong-pc': outputs[0]['pc'] += 4
    elif change == 'wrong-ftq-wrap': outputs[0]['ftq'] = [0, 7]
    elif change == 'wrong-epoch': outputs[0]['epoch'] = 0
    elif change == 'missing-output': outputs.clear()
    elif change == 'duplicated-output': outputs.append(deepcopy(outputs[0]))
    elif change == 'not-accepted': outputs[0]['ready'] = 0
    elif change == 'pending': context['pending_lanes_at_end'] = [0]
    elif change == 'phase': context['phase_calibrated'] = False
    elif change == 'oracle-unarmed': context['oracle_armed'] = False
    elif change == 'bypass-error': outputs[0]['bypass_exception_mask'] = 1
    elif change == 'queue-error': outputs[0]['queue_exception_owned'] = 1
    elif change == 'bad-priority': priority.update(first_has_rvc_ill=1, rvc_exception_type=4)
    elif change == 'missing-priority': priority.clear()
    elif change == 'no-recovery': context['post_context_recovery_pass'] = False
    elif change == 'uncache': window['s2_useUncacheFetch'] = 1
    with pytest.raises((AssertionError, KeyError)):
        check_satp_window_and_delivery(window, priority, outputs, context)


@pytest.mark.parametrize("field", ["valid", "valid-and-enqueue", "exception-mask"])
def test_delivery_evidence_rejects_bits_outside_the_native_thirty_six_lanes(field):
    window, priority, outputs, context = satp_trace()
    if field == "exception-mask":
        window["io_toIBuffer_bits_exceptionMask"] |= 1 << 36
    else:
        window["io_toIBuffer_bits_valid"] |= 1 << 36
        if field == "valid-and-enqueue":
            window["io_toIBuffer_bits_enqEnable"] |= 1 << 36
    with pytest.raises(AssertionError, match="native IFU lane mask width"):
        check_satp_window_and_delivery(window, priority, outputs, context)


@pytest.mark.parametrize("route", ["queue", "bypass"])
def test_delivery_evidence_preserves_the_last_native_lane(route):
    window, priority, outputs, context = satp_trace(route)
    window["io_toIBuffer_bits_valid"] = window["io_toIBuffer_bits_enqEnable"] = 1 << 35
    context["accepted_lanes"][0]["lane"] = 35
    outputs[0]["lane"] = 35
    assert check_satp_window_and_delivery(window, priority, outputs, context)["downstream_lanes_checked"] == 1
