"""Validate watched command admission and actual readiness against issued commands."""
import argparse
from collections import Counter
import csv
import json
from pathlib import Path

from tea_dip_gsim_memory_eligibility_probe import FIELDS
from tea_dip_l2_return_trace import read
from tea_dip_memory_return_trace import transactions
from tea_dip_query_endpoint_evidence import digest, verify
from tea_dip_refresh_pending_trace import analyze as refresh_analysis


def analyze(folder, receipt_path):
    folder, receipt_path = Path(folder).resolve(), Path(receipt_path).resolve()
    refresh = refresh_analysis(folder, receipt_path)
    result = json.loads((folder/'result.json').read_text())
    build = json.loads((folder/'probe-build-proof.json').read_text())
    binding = build['memory_eligibility_binding']
    assert binding['fields'] == FIELDS and binding['new_RTL_state_bits'] == 0
    probe = json.loads((folder/'probe-receipt.json').read_text())
    verify(folder, probe['files'])
    with (folder/'memory-command-eligibility.csv').open() as stream:
        reader = csv.DictReader(stream)
        assert reader.fieldnames == FIELDS
        rows = [{k: int(v) for k, v in row.items()} for row in reader]
    raw = read(folder/'load-memory-events.csv')
    complete, pending, commands, rejected = transactions(raw)
    memory = {(m['id'], m['generation']): m for m in complete+pending}
    watches = binding['watches']
    assert rows and all(0 <= r['watch'] < len(watches) for r in rows)
    assert [r['core_cycle'] for r in rows] == sorted(r['core_cycle'] for r in rows)
    records = []
    for index, watch in enumerate(watches):
        transaction = memory[watch['memory_id'], watch['memory_generation']]
        assert transaction['address'] == watch['address'] and transaction['accepted_core'] == watch['begin_core']
        cmd = transaction['read_command']
        assert cmd is not None and cmd['controller'] == watch['controller'] and cmd['rank'] == watch['rank']
        assert cmd['core_cycle']+1 == watch['end_core']
        group = [r for r in rows if r['watch'] == index]
        for r in group:
            assert r['event'] in [1, 2] and r['valid'] in [0, 1]
            for key in ['address', 'controller', 'rank']:
                assert r[key] == watch[key], 'Watch identity mismatch'
            assert watch['begin_core'] <= r['core_cycle'] <= watch['end_core']
            assert r['input_command'] == cmd['command']
            assert all(r[key] == cmd[key] for key in ['bankgroup', 'bank', 'row', 'column']), 'Wrong input bank/row'
            waiting = [e for e in refresh['complete']+refresh['pending'] if e['controller'] == r['controller']
                       and e['begin_core'] <= r['core_cycle'] and (e['issued_core'] is None or r['core_cycle'] < e['issued_core'])]
            front = min(waiting, key=lambda e: e['begin_dram'])['rank'] if waiting else -1
            assert r['pending'] == bool(waiting) and r['front_rank'] == front, 'Wrong public pending state'
            assert (r['output_command'] in [0, 1, 4, 5]) if r['valid'] else (r['output_command'] == -1)
        admissions = [r for r in group if r['event'] == 1 and r['valid']]
        assert len(admissions) == 1, 'Watched transaction must have one successful AddCommand'
        admitted, = admissions
        queries = [r for r in group if r['event'] == 2]
        assert queries and all(r['dram_cycle'] > admitted['dram_cycle'] for r in queries)
        issued = [c for c in commands if c['controller'] == watch['controller'] and c['address'] == watch['address']
                  and watch['begin_core'] <= c['core_cycle']+1 <= watch['end_core']]
        for c in issued:
            actual = [r for r in queries if r['valid'] and r['dram_cycle'] == c['dram_cycle'] and r['output_command'] == c['command']]
            assert len(actual) == 1, 'Issued target command lacks its actual ready return'
            assert all(actual[0][key] == c[key] for key in ['rank', 'bankgroup', 'bank'])
        assert any(c['command'] in [0, 1] and c['core_cycle']+1 == watch['end_core'] for c in issued)
        # tRFC is read from the same saved model configuration, not a fixed workload assumption.
        timing = json.loads((folder/'memory-timing.json').read_text())
        duration, remainder = divmod(timing['tRFC']*timing['CPU_FREQ'], timing['DRAM_FREQ'])
        assert remainder == 0
        epochs = [e for e in refresh['complete'] if e['controller'] == watch['controller'] and e['rank'] == watch['rank']
                  and watch['begin_core'] < e['issued_core']+duration and e['begin_core'] < watch['end_core']]
        def phase(cycle):
            hit = [e for e in epochs if e['begin_core'] <= cycle < e['issued_core']+duration]
            assert len(hit) <= 1
            return ('preparation' if cycle < hit[0]['issued_core'] else 'tRFC') if hit else 'outside'
        counts = Counter((phase(r['core_cycle']), str(r['output_command'])) for r in queries)
        ready_issued = {(c['dram_cycle'], c['command']) for c in issued}
        unissued = [r for r in queries if r['valid'] and (r['dram_cycle'], r['output_command']) not in ready_issued]
        records.append(dict(watch=index, transaction=watch, API_to_command_queue_core=admitted['core_cycle']-watch['begin_core'],
                            command_admission=admitted, first_query=queries[0], last_query=queries[-1],
                            query_counts={p: dict((command, count) for (name, command), count in counts.items() if name == p)
                                          for p in sorted({p for p, _ in counts})},
                            actual_queries=queries, issued_commands=issued, valid_but_not_issued=unissued,
                            refresh_epochs=epochs))
    return dict(passed=True, source=result['source'], watched_transactions=len(watches), recorded_calls=len(rows),
                records=records, refreshes_validated=refresh['completed_refreshes'],
                input_sha256={name: digest(folder/name) for name in ['probe-receipt.json', 'probe-build-proof.json',
                    'memory-command-eligibility.csv', 'load-memory-events.csv', 'memory-timing.json']},
                tool_sha256={Path(__file__).name: digest(Path(__file__))},
                scope='Actual external readiness calls only, selected address/time/controller/rank windows. Invalid queries do not '
                      'identify an individual timing constraint. Valid-but-unissued does not isolate precharge arbitration. '
                      'Missing queries are not silently called invalid. No exclusive CPU loss, general frequency or counterfactual saving is inferred.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    args.output.write_text(json.dumps(analyze(args.folder, args.receipt), indent=2)+'\n')


if __name__ == '__main__':
    main()
