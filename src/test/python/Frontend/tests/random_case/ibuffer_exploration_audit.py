"""Offline diversity of logged IBuffer stimuli; not RTL overlap or coverage HIT."""
from __future__ import annotations

import argparse
import json
from pathlib import Path

try:
    from .coverage_growth_audit import build_growth_report, _sha, _reference
    from .scenario_coordinator import TimingPlan
except ImportError:
    from coverage_growth_audit import build_growth_report, _sha, _reference
    from scenario_coordinator import TimingPlan


def _integer(value, label, minimum=0):
    if type(value) is not int or value < minimum:
        raise ValueError('missing/invalid integer: ' + label)
    return value


def _path(value, parent):
    path = Path(value)
    return path if path.is_absolute() else parent / path


def _case_file(case, kind, parent, refs):
    path, payload = _reference({'path': case[kind+'_path'], 'sha256': case[kind+'_sha256']}, parent)
    refs[str(path.resolve())] = case[kind+'_sha256']
    return payload


def _site(instruction):
    pc = _integer(instruction['pc'], 'instruction.pc')
    size = _integer(instruction['size'], 'instruction.size', 1)
    if size not in (2, 4) or pc % 2:
        raise ValueError('invalid instruction size/alignment')
    # Derive positions from linked PC/width, never from generator cfi_offset.
    return {'pc': pc, 'size': size, 'mnemonic': instruction.get('mnemonic'),
            'kind': ('call' if instruction.get('call') else 'return' if instruction.get('return')
                     else 'conditional_branch' if instruction.get('conditional_branch')
                     else 'indirect_jump' if instruction.get('indirect_jump')
                     else 'direct_jump' if instruction.get('direct_jump') else 'other'),
            'offset32': pc % 32, 'offset64': pc % 64,
            'cross32': pc//32 != (pc+size-1)//32,
            'cross64': pc//64 != (pc+size-1)//64,
            'cross4096': pc//4096 != (pc+size-1)//4096}


def _duration_band(duration):
    return 'short_1_7' if duration <= 7 else 'medium_8_1199' if duration < 1200 else 'long_1200_plus'


def _phase(cycle, start, end):
    if cycle == start:
        return 'start'
    if cycle == end:
        return 'last'
    return 'interior' if start < cycle < end else 'outside'


def _logged_combinations(timeline, program_map):
    """Join only complete, unambiguous logged queued/driven redirect records."""
    if timeline.get('dropped_timeline_events') != 0 or type(timeline.get('dropped_timeline_events')) is not int:
        raise ValueError('incomplete timeline')
    instructions = program_map.get('instructions')
    if not isinstance(instructions, list) or not instructions:
        raise ValueError('ProgramMap instructions missing')
    by_pc, end = {}, -1
    for instruction in instructions:
        site = _site(instruction)
        if site['pc'] < end:
            raise ValueError('overlapping/out-of-order ProgramMap')
        end = site['pc'] + site['size']
        by_pc[site['pc']] = site
    events = timeline.get('timeline')
    actions = timeline.get('actions')
    if not isinstance(events, list) or not isinstance(actions, list) or not actions:
        raise ValueError('logged actions/events missing')
    for event in events:
        _integer(event.get('cycle'), 'event.cycle')
    windows = []
    failures = []
    action_ids = set()
    previous_end = -1
    for action in actions:
        if action.get('kind') != 'backend_can_accept_low':
            raise ValueError('unexpected action kind for IBuffer family')
        action_id = action.get('action_id')
        if not isinstance(action_id, str) or not action_id or action_id in action_ids:
            raise ValueError('missing/duplicate action identity')
        action_ids.add(action_id)
        start = _integer(action.get('start_cycle'), 'action.start_cycle')
        duration = _integer(action.get('duration_cycles'), 'action.duration_cycles', 1)
        end = _integer(action.get('end_cycle_inclusive'), 'action.end_cycle_inclusive')
        if start <= previous_end:
            raise ValueError('overlapping/unordered pressure windows')
        previous_end = end
        confirmed = _integer(action.get('confirmed_low_cycles'), 'action.confirmed_low_cycles')
        if (end != start+duration-1 or action.get('trigger_event') != 'backend.redirect_queued'
                or action.get('trigger_cycle') != start or action.get('status') != 'REALIZED'
                or action.get('completion_cycle') != end or confirmed != duration):
            raise ValueError('pressure window is incomplete or not fully observed')
        trigger = action.get('trigger_payload') or {}
        pc = _integer(trigger.get('from_pc'), 'trigger.from_pc')
        target = _integer(trigger.get('target_pc'), 'trigger.target_pc')
        ready = _integer(trigger.get('ready_cycle'), 'trigger.ready_cycle')
        if ready < start or pc not in by_pc or target not in by_pc:
            raise ValueError('trigger not resolved to linked source/target')
        reason = trigger.get('reason')
        if not isinstance(reason, str) or not reason:
            raise ValueError('redirect reason missing')
        queues = [e for e in events if e.get('type') == 'backend.redirect_queued'
                  and e['cycle'] == start and e.get('payload') == trigger]
        if len(queues) != 1:
            raise ValueError('queued redirect identity is missing/ambiguous')
        driven = [e for e in events if e.get('type') == 'backend.redirect'
                  and e['cycle'] >= ready and e.get('payload', {}).get('from_pc') == pc
                  and e.get('payload', {}).get('target_pc') == target
                  and e.get('payload', {}).get('reason') == reason]
        if len(driven) != 1:
            failures.append({'action_id': action.get('action_id'), 'reason': 'driven_redirect_missing_or_ambiguous'})
            continue
        redirect = driven[0]
        rc = redirect['cycle']
        site = by_pc[pc]
        if site['kind'] == 'other':
            failures.append({'action_id': action.get('action_id'), 'reason': 'redirect_source_not_a_CFI'})
            continue
        beat_states = set()
        for event in events:
            if event.get('type') != 'handshake.icache_d' or not start <= event['cycle'] <= end:
                continue
            payload = event.get('payload') or {}
            beat = payload.get('beat_idx')
            if type(beat) is not int or beat not in (0, 1):
                raise ValueError('response beat identity missing')
            if any(type(payload.get(k)) is not int or payload[k] != 0 for k in ('denied', 'corrupt')):
                raise ValueError('response health missing/failed')
            _integer(payload.get('address'), 'response.address')
            relative = 'before' if event['cycle'] < rc else 'same_log_cycle' if event['cycle'] == rc else 'after'
            beat_states.add((beat, relative))
        if not beat_states:
            # Absence is a logged observation, not proof there were no RTL responses.
            beat_states.add(('none_logged', 'none_logged'))
        for beat, relative in sorted(beat_states, key=str):
            key = [site['kind'], site['size'], site['offset32'], site['offset64'],
                   site['cross32'], site['cross64'], site['cross4096'],
                   _duration_band(duration), _phase(rc, start, end), beat, relative]
            windows.append({'combination': key, 'action_id': action.get('action_id'),
                            'source_pc': pc, 'target_pc': target, 'redirect_log_cycle': rc,
                            'start_cycle': start, 'end_cycle': end, 'duration_cycles': duration,
                            'confirmed_low_cycles': confirmed, 'site': site})
    return windows, failures


def build_ibuffer_report(campaign_path, binding_index, index_sha256, scope_path, scope_sha256):
    campaign_path, binding_index = Path(campaign_path), Path(binding_index)
    growth = build_growth_report(campaign_path, binding_index, index_sha256, scope_path, scope_sha256)
    if growth['scope']['family'] != 'ibuffer_pressure_cross':
        raise ValueError('IBuffer scope required')
    refs = dict(growth['input_sha256'])
    cases = json.loads(campaign_path.read_text())['cases']
    index = json.loads(binding_index.read_text())
    rows = {str(_path(r['path'], campaign_path.parent).resolve()): r for r in index['artifacts']}
    seen, sites, curve, samples, failures = set(), set(), [], [], []
    for case in cases:
        program_map = _case_file(case, 'program_map', campaign_path.parent, refs)
        scenario = _case_file(case, 'scenario_spec', campaign_path.parent, refs)
        realization = _case_file(case, 'scenario_realization', campaign_path.parent, refs)
        if (program_map.get('binary_text_verified') is not True
                or program_map.get('binary_sha256') != case['bin_sha256']
                or program_map.get('source_sha256') != case['source_sha256']):
            raise ValueError('ProgramMap source/binary identity missing or changed')
        if (scenario.get('scenario_id') != case['name'] or scenario.get('family') != 'ibuffer_pressure_cross'
                or scenario.get('program_seed') != case['spec']['seed']
                or scenario.get('timing_seed') != case['timing_seed']):
            raise ValueError('scenario identity mismatch')
        if (realization.get('scenario_id') != case['name'] or realization.get('status') != 'REALIZED'
                or realization.get('missing_events') != []):
            raise ValueError('scenario realization not proved')
        artifact = _path(case['functional_coverage_artifacts'][0], campaign_path.parent)
        row = rows[str(artifact.resolve())]
        sidecar = _path(row['sidecar_path'], binding_index.parent)
        binding = json.loads(sidecar.read_text())
        _, timeline = _reference(binding['scenario_timeline'], sidecar.parent)
        plan = timeline.get('timing_plan')
        if not isinstance(plan, dict) or plan.get('profile') != 'ibuffer_pressure_cross':
            raise ValueError('complete IBuffer timing plan missing')
        TimingPlan.from_dict(plan).validate()
        if (plan.get('timing_seed') != case['timing_seed']
                or scenario.get('parameters', {}).get('timing_plan') != plan):
            raise ValueError('frozen scenario/timeline timing plan mismatch')
        actions = timeline.get('actions')
        if not isinstance(actions, list) or len(actions) > plan['maximum_backpressure_windows']:
            raise ValueError('pressure actions exceed timing plan')
        for ordinal, action in enumerate(actions):
            if action.get('duration_cycles') != plan['backpressure_durations'][ordinal]:
                raise ValueError('observed duration differs from frozen timing plan')
        if sum(a.get('confirmed_low_cycles', 0) for a in actions) > timeline['details']['cycles_run']:
            raise ValueError('pressure cost exceeds measured run cycles')
        windows, case_failures = _logged_combinations(timeline, program_map)
        before = len(seen)
        for sample in windows:
            key = json.dumps(sample['combination'], separators=(',', ':'))
            seen.add(key);sites.add((case['bin_sha256'], sample['source_pc']))
            samples.append({'run_id': case['name'], 'binary_sha256': case['bin_sha256'], **sample})
        failures.extend({'run_id': case['name'], **f} for f in case_failures)
        curve.append({'run_id': case['name'], 'new_logged_combinations': len(seen)-before,
                      'cumulative_logged_combinations': len(seen), 'distinct_linked_sites': len(sites),
                      'actual_action_count': len(timeline['actions']),
                      'logged_confirmed_pressure_cycles': sum(a['confirmed_low_cycles'] for a in timeline['actions']),
                      'configured_maximum_windows': plan['maximum_backpressure_windows'],
                      'configured_duration_cycles': plan['backpressure_durations'],
                      'unresolved_action_count': len(case_failures)})
    if any(_sha(Path(path)) != pin for path, pin in refs.items()):
        raise ValueError('bound evidence changed during exploration audit')
    return {'schema_version': 1, 'family': 'ibuffer_pressure_cross',
            'status': 'incomplete_logged_attribution' if failures else 'logged_stimulus_audited',
            'canonical_hit_eligible': False, 'rtl_overlap_proved': False,
            'growth': growth, 'logged_combination_count': len(seen),
            'combination_fields': ['CFI kind', 'width', 'offset32', 'offset64', 'cross32', 'cross64',
                                   'cross4096', 'pressure_duration_band', 'redirect_log_phase',
                                   'response_beat', 'response_log_relative_to_redirect'],
            'combination_curve': curve, 'logged_samples': samples, 'unresolved_actions': failures,
            'unmeasured': ['actual IBuffer fullness and capacity margin', 'payload stability under hold',
                           'same-edge redirect/response/full overlap', 'post-redirect target recovery',
                           'within-run strict-bin first hits', 'wall-clock cost and failed-run economics'],
            'policy': 'Unique logged combinations, no Cartesian denominator, no equality between log-cycle coincidence and RTL overlap',
            'input_sha256': refs}


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--campaign', type=Path, required=True)
    parser.add_argument('--binding-index', type=Path, required=True)
    parser.add_argument('--binding-index-sha256', required=True)
    parser.add_argument('--scope', type=Path, required=True)
    parser.add_argument('--scope-sha256', required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args(argv)
    report = build_ibuffer_report(args.campaign, args.binding_index, args.binding_index_sha256,
                                  args.scope, args.scope_sha256)
    with args.output.open('x', encoding='utf-8') as stream:
        json.dump(report, stream, indent=2);stream.write('\n')
    return 0 if not report['unresolved_actions'] else 1


if __name__ == '__main__':
    raise SystemExit(main())
