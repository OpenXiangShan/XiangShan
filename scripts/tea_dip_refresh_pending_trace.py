"""Validate refresh preparation calls against actual issued memory commands."""
import argparse
from collections import Counter, defaultdict
import csv
import json
from pathlib import Path

from tea_dip_gsim_refresh_pending_probe import FIELDS
from tea_dip_l2_return_trace import read
from tea_dip_query_endpoint_evidence import digest, verify


def analyze(folder, receipt_path, queries=()):
    folder, receipt_path = Path(folder).resolve(), Path(receipt_path).resolve()
    receipt = json.loads(receipt_path.read_text())
    result = json.loads((folder/'result.json').read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and result['original_process_closed']
    assert result['difftest_passed'] and result['exitcode'] == 0 and result['process'] == receipt['process']
    verify(folder, receipt['files'])
    probe = json.loads((folder/'probe-receipt.json').read_text())
    assert probe['closure_receipt_sha256'] == digest(receipt_path)
    verify(folder, probe['files'])
    build = json.loads((folder/'probe-build-proof.json').read_text())
    binding = build['refresh_pending_binding']
    assert build['source'] == result['source'] == receipt['source']
    assert build['binary_sha256'] == result['binary_sha256'] and binding['fields'] == FIELDS
    assert binding['actual_public_pending_state'] and binding['controller_cycle_command_asserted']
    with (folder/'memory-refresh-pending.csv').open() as stream:
        rows = [{k: int(v) for k, v in row.items()} for row in csv.DictReader(stream)]
    memory = read(folder/'load-memory-events.csv')
    boundary = [(r['core_cycle'], r['clean'], r['native_dump']) for r in rows if r['event'] == 0]
    assert boundary == [(r['core_cycle'], r['clean'], r['native_dump']) for r in memory if r['kind'] == 0]
    mapping, reverse, commands = {}, {}, []
    for row in rows:
        if row['event'] == 4:
            own, old = row['controller'], row['command_controller']
            assert own not in mapping or mapping[own] == old, 'Controller binding changed'
            assert old not in reverse or reverse[old] == own, 'Controller binding is not unique'
            mapping[own], reverse[old] = old, own
            commands.append(tuple(row[k] for k in ['core_cycle', 'step', 'command_controller', 'dram_cycle', 'command', 'rank', 'bankgroup', 'bank']))
    expected = [(r['core_cycle']+1 if r['core_cycle'] >= 0 else -1, r['step'], r['controller'], r['dram_cycle'], r['command'], r['rank'], r['bankgroup'], r['bank']) for r in memory if r['kind'] == 3]
    assert commands == expected, 'New command binding differs from original DRAM commands'
    by_tick = defaultdict(list)
    for row in rows:
        if row['event'] == 4:
            by_tick[row['controller'], row['dram_cycle']].append(row)
    pending, active, finishing, epochs = defaultdict(list), {}, {}, Counter()
    complete, counts = [], Counter()
    previous_core = -1
    for row in rows:
        assert row['core_cycle'] >= previous_core, 'Refresh core clock moved backwards'
        previous_core = row['core_cycle']
        event, controller = row['event'], row['controller']
        if event == 0:
            continue
        assert controller in mapping and row['pending'] in [0, 1]
        counts[str(event)] += 1
        rank, tick = row['rank'], row['dram_cycle']
        if event == 1:
            assert row['need'] in [0, 1]
            if row['need']:
                key = controller, rank
                assert key not in active, 'Duplicate pending rank refresh'
                epochs[key] += 1
                owner = dict(controller=mapping[controller], rank=rank, generation=epochs[key],
                             begin_core=row['core_cycle'], begin_dram=tick,
                             first_finish_core=None, finish_calls=0, invalid_finish_calls=0,
                             preparation_commands=[], issued_core=None, issued_dram=None,
                             right_censored=True)
                pending[controller].append(rank)
                active[key] = owner
            # false may be an internal same-object call and is not required.
        if event == 2:
            assert controller not in finishing and pending[controller] and rank == pending[controller][0]
            finishing[controller] = tick
            owner = active[controller, rank]
            owner['finish_calls'] += 1
            if owner['first_finish_core'] is None:
                owner['first_finish_core'] = row['core_cycle']
        if event == 3:
            assert finishing.pop(controller) == tick, 'Missing same-tick FinishRefresh entry'
            owner = active[controller, pending[controller][0]]
            if row['valid']:
                assert row['command'] in [5, 7] and rank == owner['rank']
                matches = [r for r in by_tick[controller, tick] if all(r[k] == row[k] for k in ['command', 'rank', 'bankgroup', 'bank'])]
                assert len(matches) == 1, 'FinishRefresh result was not issued'
                owner['preparation_commands'].append({k: row[k] for k in ['core_cycle', 'dram_cycle', 'command', 'rank', 'bankgroup', 'bank']})
            else:
                owner['invalid_finish_calls'] += 1
        expected_pending = bool(pending[controller])
        assert bool(row['pending']) == expected_pending, 'Public refresh waiting differs from enqueue/issue history'
        assert row['front_rank'] == (pending[controller][0] if expected_pending else -1), 'Wrong public pending rank'
        if event == 4 and row['command'] == 7:
            assert pending[controller] and pending[controller][0] == rank
            owner = active.pop((controller, rank))
            owner.update(issued_core=row['core_cycle'], issued_dram=tick,
                         pending_to_issue_core=row['core_cycle']-owner['begin_core'],
                         pending_to_issue_dram=tick-owner['begin_dram'], right_censored=False)
            assert owner['finish_calls'] == tick-owner['begin_dram']+1, 'Missing refresh preparation calls'
            assert owner['preparation_commands'][-1]['command'] == 7
            pending[controller].pop(0)
            complete.append(owner)
    assert not finishing
    assert counts['1'] >= len(complete) and sum(o['finish_calls'] for o in complete+list(active.values())) == counts['2'] == counts['3']
    phases = []
    dumps = [r['core_cycle'] for r in rows if r['event'] == 0 and r['native_dump']]
    for end in dumps:
        lo = max([r['core_cycle'] for r in rows if r['event'] == 0 and r['clean'] and r['core_cycle'] < end], default=-1)
        phases.append(dict(begin_core_exclusive=lo, end_core_inclusive=end,
                           refresh_enqueues=sum(lo < o['begin_core'] <= end for o in complete+list(active.values())),
                           issued_refreshes=sum(lo < o['issued_core'] <= end for o in complete),
                           completed_preparation_core_cycles=sum(o['pending_to_issue_core'] for o in complete if lo < o['issued_core'] <= end)))
    queried = []
    for controller, rank, cycle in queries:
        candidates = [o for o in complete+list(active.values()) if o['controller'] == controller and o['rank'] == rank and o['begin_core'] <= cycle and (o['issued_core'] is None or cycle <= o['issued_core'])]
        assert len(candidates) <= 1
        queried.append(dict(controller=controller, rank=rank, core_cycle=cycle,
                            pending_preparation=candidates[0] if candidates else None))
    return dict(passed=True, source=result['source'], controller_mapping=mapping,
                events=len(rows), event_counts=dict(counts), issued_command_marginals=len(commands),
                completed_refreshes=len(complete), pending_refreshes=len(active),
                complete=complete, pending=list(active.values()), phases=phases, queries=queried,
                input_sha256={p.name: digest(p) for p in [receipt_path, folder/'probe-receipt.json',
                    folder/'probe-build-proof.json', folder/'memory-refresh-pending.csv', folder/'load-memory-events.csv']},
                tool_sha256={Path(__file__).name: digest(Path(__file__))},
                scope='Actual external refresh enqueue, public pending/front state, FinishRefresh enter/return and issued command sequence. '
                      'Controller mapping and local clock are validated on every original issued command. '
                      'Preparation service may cross clean; incomplete refreshes remain censored. '
                      'The private is_in_ref bit is not directly recorded; no exclusive CPU loss or counterfactual savings are inferred.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--at', nargs=3, type=lambda v: int(v, 0), action='append', default=[])
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    args.output.write_text(json.dumps(analyze(args.folder, args.receipt, args.at), indent=2)+'\n')


if __name__ == '__main__':
    main()
