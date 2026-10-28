"""Offline synthetic log tests and a temporary assembler check; never DUT/corpus."""
import copy
from dataclasses import replace
import json
import shutil
import subprocess

import pytest

from . import ibuffer_exploration_audit as audit
from .generate_random_asm import CaseSpec, generate_case
from .program_map import build_program_map
from .scenario_coordinator import TimingPlan
from .test_native_funcov_adapter import native, _write
from .test_coverage_growth_audit import measured, _repin


def _logs():
    trigger = {'from_pc': 0x801e, 'target_pc': 0x8040, 'ready_cycle': 11, 'reason': 'mispredict'}
    action = {'action_id': 'pressure-0', 'kind': 'backend_can_accept_low',
              'start_cycle': 10, 'trigger_cycle': 10, 'end_cycle_inclusive': 13,
              'duration_cycles': 4, 'confirmed_low_cycles': 4,
              'completion_cycle': 13, 'status': 'REALIZED',
              'trigger_event': 'backend.redirect_queued', 'trigger_payload': trigger}
    events = [{'type': 'backend.redirect_queued', 'cycle': 10, 'payload': trigger},
              {'type': 'backend.redirect', 'cycle': 11,
               'payload': {'from_pc': 0x801e, 'target_pc': 0x8040, 'reason': 'mispredict'}},
              {'type': 'handshake.icache_d', 'cycle': 11,
               'payload': {'beat_idx': 0, 'address': 0x8000, 'denied': 0, 'corrupt': 0}}]
    pm = {'instructions': [{'pc': 0x801e, 'size': 4, 'mnemonic': 'jalr', 'indirect_jump': True},
                           {'pc': 0x8040, 'size': 2, 'mnemonic': 'c.nop'}]}
    return {'dropped_timeline_events': 0, 'actions': [action], 'timeline': events}, pm


def test_join_uses_linked_pc_and_retains_log_coincidence_without_rtl_claim():
    timeline, pm = _logs()
    pm['instructions'][0]['cacheline_offset'] = 62  # Cached annotation is not trusted.
    pm['instructions'][0]['crosses_fetch_block'] = False
    samples, failures = audit._logged_combinations(timeline, pm)
    assert not failures and len(samples) == 1
    sample = samples[0]
    assert sample['site']['offset64'] == 30 and sample['site']['cross32'] is True
    assert sample['combination'][-4:] == ['short_1_7', 'interior', 0, 'same_log_cycle']


@pytest.mark.parametrize('cycle,phase', [(10, 'start'), (11, 'interior'), (13, 'last'), (14, 'outside')])
def test_redirect_log_phase_is_based_on_actual_driven_cycle(cycle, phase):
    timeline, pm = _logs()
    timeline['actions'][0]['trigger_payload']['ready_cycle'] = 10
    timeline['timeline'][1]['cycle'] = cycle
    samples, failures = audit._logged_combinations(timeline, pm)
    assert not failures and samples[0]['combination'][8] == phase


@pytest.mark.parametrize('change', ['missing_driven', 'ambiguous_driven', 'non_cfi'])
def test_ambiguous_or_non_cfi_source_is_unresolved_not_a_new_combination(change):
    timeline, pm = _logs()
    if change == 'missing_driven': timeline['timeline'].pop(1)
    elif change == 'ambiguous_driven': timeline['timeline'].append(copy.deepcopy(timeline['timeline'][1]))
    else: pm['instructions'][0]['indirect_jump'] = False
    samples, failures = audit._logged_combinations(timeline, pm)
    assert not samples and len(failures) == 1


@pytest.mark.parametrize('change', ['dropped', 'bool_drop', 'missing_low', 'partial_low', 'duration',
                                    'status', 'queue_missing', 'queue_duplicate', 'beat',
                                    'health_missing', 'target_unmapped', 'overlap', 'action_duplicate'])
def test_invalid_or_incomplete_logs_fail_closed(change):
    timeline, pm = _logs()
    action = timeline['actions'][0]
    if change == 'dropped': timeline['dropped_timeline_events'] = 1
    elif change == 'bool_drop': timeline['dropped_timeline_events'] = False
    elif change == 'missing_low': del action['confirmed_low_cycles']
    elif change == 'partial_low': action['confirmed_low_cycles'] -= 1
    elif change == 'duration': action['duration_cycles'] += 1
    elif change == 'status': action['status'] = 'FAIL'
    elif change == 'queue_missing': timeline['timeline'].pop(0)
    elif change == 'queue_duplicate': timeline['timeline'].append(copy.deepcopy(timeline['timeline'][0]))
    elif change == 'beat': del timeline['timeline'][2]['payload']['beat_idx']
    elif change == 'health_missing': del timeline['timeline'][2]['payload']['denied']
    elif change == 'target_unmapped': action['trigger_payload']['target_pc'] = 0x9000
    elif change == 'overlap': pm['instructions'].append(copy.deepcopy(pm['instructions'][0]))
    elif change == 'action_duplicate': timeline['actions'].append(copy.deepcopy(action))
    with pytest.raises(ValueError):
        audit._logged_combinations(timeline, pm)


@pytest.fixture
def exploration(measured):
    root, campaign, _, _ = measured
    cases = json.loads(campaign.read_text())
    for case in cases['cases']:
        name = case['name']
        timeline_path = root/f'{name}.scenario_timeline.json'
        timeline = json.loads(timeline_path.read_text())
        logs, pm = _logs()
        # Current recipe is long-pressure; short logs above test general analysis only.
        action = logs['actions'][0]
        action.update(duration_cycles=1200, end_cycle_inclusive=1209,
                      completion_cycle=1209, confirmed_low_cycles=1200)
        timeline.update(logs)
        plan = replace(TimingPlan.from_seed(34, 'ibuffer_pressure_cross'),
                       maximum_backpressure_windows=1, backpressure_durations=(1200,)).to_dict()
        timeline['timing_plan'] = plan
        timeline['details']['cycles_run'] = 2000
        _write(timeline_path, timeline)
        pm.update(binary_text_verified=True, binary_sha256=case['bin_sha256'],
                  source_sha256=case['source_sha256'])
        scenario = {'scenario_id': name, 'family': 'ibuffer_pressure_cross',
                    'program_seed': case['spec']['seed'], 'timing_seed': 34,
                    'parameters': {'timing_plan': plan}}
        realization = {'scenario_id': name, 'status': 'REALIZED', 'missing_events': []}
        for kind, payload in [('program_map', pm), ('scenario_spec', scenario), ('scenario_realization', realization)]:
            path = root/f'{name}.{kind}.json'
            case[kind+'_path'] = path.name
            case[kind+'_sha256'] = _write(path, payload)
    _write(campaign, cases)
    _repin(measured)
    return measured


def _report(exploration):
    _, campaign, index, scope = exploration
    return audit.build_ibuffer_report(campaign, index, audit._sha(index), scope, audit._sha(scope))


def test_campaign_curve_counts_semantic_combination_once_and_keeps_cost(exploration):
    root = exploration[0]
    originals = {p: p.read_bytes() for p in root.iterdir() if p.is_file()}
    report = _report(exploration)
    assert report['status'] == 'logged_stimulus_audited'
    assert report['logged_combination_count'] == 1
    assert [p['new_logged_combinations'] for p in report['combination_curve']] == [1, 0]
    assert report['growth']['total_measured_cycles'] == 4000
    assert not report['canonical_hit_eligible'] and not report['rtl_overlap_proved']
    assert all(p.read_bytes() == original for p, original in originals.items())


@pytest.mark.parametrize('change', ['missing_hash', 'stale_map', 'wrong_binary', 'wrong_scenario',
                                    'failed_realization', 'wrong_plan'])
def test_campaign_requires_actual_map_scenario_realization_and_timing(exploration, change):
    root, campaign, index, _ = exploration
    payload = json.loads(campaign.read_text())
    case = payload['cases'][0]
    if change == 'missing_hash':
        del case['program_map_sha256']
    elif change == 'stale_map':
        (root/case['program_map_path']).write_text('{}')
    else:
        kind = 'program_map' if change == 'wrong_binary' else 'scenario_realization' if change == 'failed_realization' else 'scenario_spec'
        path = root/case[kind+'_path']
        body = json.loads(path.read_text())
        if change == 'wrong_binary': body['binary_sha256'] = '0'*64
        elif change == 'wrong_scenario': body['program_seed'] += 1
        elif change == 'failed_realization': body['status'] = 'FAIL'
        elif change == 'wrong_plan': body['parameters']['timing_plan']['backpressure_durations'][0] += 1
        case[kind+'_sha256'] = _write(path, body)
    _write(campaign, payload)
    _repin(exploration)
    with pytest.raises((ValueError, KeyError)):
        _report(exploration)


def test_cli_never_overwrites_output(exploration):
    root, campaign, index, scope = exploration
    output = root/'exploration.json'
    args = ['--campaign', str(campaign), '--binding-index', str(index),
            '--binding-index-sha256', audit._sha(index), '--scope', str(scope),
            '--scope-sha256', audit._sha(scope), '--output', str(output)]
    assert audit.main(args) == 0
    original = output.read_bytes()
    with pytest.raises(FileExistsError): audit.main(args)
    assert output.read_bytes() == original


@pytest.mark.skipif(any(shutil.which(x) is None for x in ('riscv64-unknown-elf-gcc', 'riscv64-unknown-elf-objcopy', 'riscv64-unknown-elf-objdump')),
                    reason='requires actual RISC-V assembler/ELF tools')
def test_ibuffer_actual_linked_cfi_offset_is_not_requested_padding(tmp_path):
    # Same fixed synthetic seed used by existing assembly tests, no search or replay.
    source, metadata = generate_case(CaseSpec(family='ibuffer_pressure_cross', seed=0xA55A,
                                             blocks=16, iterations=2, max_call_depth=4))
    asm, elf, binary = (tmp_path/name for name in ('case.S', 'case.elf', 'case.bin'))
    asm.write_text(source)
    subprocess.run(['riscv64-unknown-elf-gcc', '-march=rv64gc_zicsr_zifencei', '-mabi=lp64',
                    '-static', '-no-pie', '-nostdlib', '-nostartfiles', '-Wl,--build-id=none',
                    '-Wl,-Ttext=0x80000000', '-Wl,-e,_start', '-o', str(elf), str(asm)],
                   check=True, capture_output=True)
    subprocess.run(['riscv64-unknown-elf-objcopy', '-O', 'binary', '-j', '.text', str(elf), str(binary)],
                   check=True, capture_output=True)
    pm = build_program_map(asm, binary, metadata, link_address=0x80000000, binary_base=0x80000000)
    # Branch sites emit a 32-bit ANDI before their CFI; JALR sites emit LA first.
    branch_episodes = [e for e in metadata['episodes'] if e['parameters'].get('cfi_kind') == 'branch_rvi']
    assert branch_episodes, 'fixed test program should include conditional sites'
    for episode in branch_episodes:
        entry_pc = pm['labels'][episode['entry_labels'][0]]
        expected_pc = entry_pc + episode['parameters']['cfi_offset'] + 4
        sites = [i for i in pm['instructions'] if i['pc'] == expected_pc]
        assert len(sites) == 1 and sites[0]['conditional_branch'] is True
        assert sites[0]['episode_id'] == episode['episode_id']
        assert sites[0]['pc'] % 64 == (episode['parameters']['cfi_offset'] + 4) % 64
        assert audit._site(sites[0])['offset64'] != episode['parameters']['cfi_offset']


@pytest.mark.parametrize('change', ['empty_actions', 'empty_events'])
def test_no_evidence_is_not_a_zero_hit_success(change):
    timeline, pm = _logs()
    timeline['actions' if change == 'empty_actions' else 'timeline'] = []
    with pytest.raises(ValueError):
        audit._logged_combinations(timeline, pm)


@pytest.mark.parametrize('change', ['cost', 'plan_duration'])
def test_logged_pressure_must_match_bound_plan_and_budget(exploration, change):
    root = exploration[0]
    path = root/'run-0.scenario_timeline.json'
    payload = json.loads(path.read_text())
    if change == 'cost': payload['details']['cycles_run'] = 100
    else:
        payload['actions'][0].update(duration_cycles=1201, confirmed_low_cycles=1201,
                                     end_cycle_inclusive=1210, completion_cycle=1210)
    _write(path, payload)
    _repin(exploration)
    with pytest.raises(ValueError):
        _report(exploration)


def test_unreviewed_bin951_label_cannot_prove_rtl_overlap(exploration):
    path = exploration[0]/'run-0.scenario_timeline.json'
    payload = json.loads(path.read_text())
    payload['bin951_event_contract'] = {'sampled_events': [{'status': 'REALIZED'}]}
    _write(path, payload)
    _repin(exploration)
    report = _report(exploration)
    assert report['rtl_overlap_proved'] is False
    assert 'post-redirect target recovery' in report['unmeasured']
