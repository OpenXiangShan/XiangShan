"""Audit accepted MBTB/provider-hit conditional branches, including excluded SC training."""
from collections import Counter, defaultdict
from pathlib import Path
import argparse
import heapq
import json
import sqlite3
from native_perf import read_blocks, unique_counters
from tea_dip_query_endpoint_evidence import digest, verify

COLUMNS = ['ID', 'TRAININGELIGIBLE', 'MISPREDICT', 'ACTUALTAKEN', 'USESC', 'SCPRED',
           'SUMABOVETHRES', 'TAGEFINALTAKEN', 'TAGEFINALVALID', 'TAGEUSEPROVIDER',
           'PROVIDERCTR', 'PROVIDERTAKEN', 'PROVIDERVALID', 'CFIPC', 'STARTPC_ADDR', 'STAMP', 'SITE']
BOOLS = [name for name in COLUMNS if name not in
         ['ID', 'PROVIDERCTR', 'CFIPC', 'STARTPC_ADDR', 'STAMP', 'SITE']]
COMMON = [name for name in COLUMNS if name not in ['ID', 'TRAININGELIGIBLE', 'SITE']]


def observe(row, cohorts):
    selected = bool(row['TRAININGELIGIBLE'])
    for cohort in ['all_matched', 'training_selected' if selected else 'training_excluded']:
        counts = cohorts[cohort]
        counts['rows'] += 1
        counts['training_eligible'] += selected
        counts['useSc'] += row['USESC']
        if not row['USESC']:
            continue
        sc = row['SCPRED'] == row['ACTUALTAKEN']
        provider = row['PROVIDERTAKEN'] == row['ACTUALTAKEN']
        counts['SC_correct_raw_provider_wrong'] += int(sc and not provider)
        counts['SC_wrong_raw_provider_correct'] += int(not sc and provider)
        counts['SC_raw_provider_both_correct'] += int(sc and provider)
        counts['SC_raw_provider_both_wrong'] += int(not sc and not provider)
        source = 'provider' if row['TAGEUSEPROVIDER'] else 'alternate' if row['TAGEFINALVALID'] else 'invalid'
        counts['useSc_' + source] += 1
        if not row['TAGEFINALVALID']:
            continue
        tage = row['TAGEFINALTAKEN'] == row['ACTUALTAKEN']
        for suffix, value in [('SC_correct_TAGE_wrong', sc and not tage),
                              ('SC_wrong_TAGE_correct', not sc and tage),
                              ('both_correct', sc and tage), ('both_wrong', not sc and not tage)]:
            counts[source + '_' + suffix] += int(value)


def plain(cohorts):
    return {name: dict(counts) for name, counts in sorted(cohorts.items())}


def analyze(folder, receipt_path):
    receipt = json.loads(receipt_path.read_text())
    result = json.loads((folder / 'result.json').read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and receipt['point'] == folder.name
    assert result['original_process_closed'] and result['difftest_passed'] and result['exitcode'] == 0
    assert all(result['sampler_flags'].values())
    assert result['source'] == receipt['source'] and result['process'] == receipt['process']
    verify(folder, receipt['files'])
    command = json.loads((folder / 'command.json').read_text())
    selection = set(command[command.index('--dump-select-db') + 1].split())
    tables = ['scMatchedConditional_' + str(i) for i in range(8)]
    assert selection.issuperset(tables + ['scCondTrace_' + str(i) for i in range(8)])
    blocks = sorted(read_blocks(folder / 'simulator_err.txt').items())
    assert len(blocks) == (2 if '-W' in command else 1)
    cohorts = defaultdict(Counter)
    pcs = defaultdict(lambda: defaultdict(Counter))
    table_counts = {}
    with sqlite3.connect((folder / 'simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        for table in tables:
            assert [r['name'] for r in db.execute('PRAGMA table_info(' + table + ')')] == COLUMNS

        def rows(lane):
            table = tables[lane]
            selected = iter(db.execute('SELECT * FROM scCondTrace_' + str(lane) + ' ORDER BY ID'))
            previous = -1
            table_counts[table] = 0
            for ordinal, row in enumerate(db.execute('SELECT * FROM ' + table + ' ORDER BY ID'), 1):
                assert row['ID'] == ordinal and row['STAMP'] > previous
                previous = row['STAMP']
                assert all(row[name] in [0, 1] for name in BOOLS)
                assert row['PROVIDERVALID'] and 0 <= row['PROVIDERCTR'] <= 7
                assert row['PROVIDERTAKEN'] == int(row['PROVIDERCTR'] >= 4)
                if row['TAGEUSEPROVIDER']:
                    assert row['TAGEFINALVALID'] and row['TAGEFINALTAKEN'] == row['PROVIDERTAKEN']
                use, sc, actual, provider = [row[k] for k in ['USESC', 'SCPRED', 'ACTUALTAKEN', 'PROVIDERTAKEN']]
                eligible = not (use and sc == actual and sc == provider)
                assert row['TRAININGELIGIBLE'] == int(eligible), 'Baseline SC training predicate changed'
                if eligible:
                    old = next(selected, None)
                    assert old is not None and all(row[name] == old[name] for name in COMMON), \
                        'Matched training subset differs from original selected SC trace'
                table_counts[table] = ordinal
                yield row['STAMP'], lane, row
            assert next(selected, None) is None, 'Old selected SC records are missing from matched trace'

        merged = iter(heapq.merge(*[rows(i) for i in range(8)]))
        pending = next(merged, None)
        phases = []
        labels = ['warmup', 'ROI'] if len(blocks) == 2 else ['coremark']
        for label, (stamp, block) in zip(labels, blocks):
            counters = unique_counters(block)

            def counter(suffix):
                value, = [v for key, v in counters.items() if key.endswith(suffix)]
                return value

            amount = counter('.sc.diag_sc_matched_cond_events')
            phase = defaultdict(Counter)
            opposition = defaultdict(Counter)
            opposition_pcs = defaultdict(lambda: defaultdict(Counter))
            first = last = None
            for _ in range(amount):
                assert pending is not None, 'Native matched count exceeds actual records'
                cycle, lane, row = pending
                first = cycle if first is None else first
                last = cycle
                observe(row, phase)
                observe(row, cohorts)
                observe(row, pcs[hex(row['CFIPC'])])
                if row['USESC'] and row['PROVIDERCTR'] in [0, 7] and row['SCPRED'] != row['PROVIDERTAKEN']:
                    observe(row, opposition)
                    observe(row, opposition_pcs[hex(row['CFIPC'])])
                pending = next(merged, None)
            assert phase['all_matched']['training_eligible'] == counter('.sc.diag_sc_cond_trace_events')
            if pending is not None and last is not None:
                assert last < pending[0], 'Native matched phase cut splits simultaneous lanes'
            phases.append(dict(phase=label, native_print_stamp=stamp, matched_events=amount,
                               first_private_stamp=first, last_private_stamp=last, cohorts=plain(phase),
                               saturated_provider_opposition=dict(
                                   cohorts=plain(opposition),
                                   by_pc={pc: plain(value) for pc, value in sorted(opposition_pcs.items(),
                                                                                  key=lambda item: int(item[0], 16))})))
        assert pending is None, 'Matched records remain after all native event counts'
    return dict(passed=True, point=folder.name, source=receipt['source'], tables=table_counts,
                cohorts=plain(cohorts), native_training_phases=phases,
                by_pc={pc: plain(value) for pc, value in sorted(pcs.items(), key=lambda item: int(item[0], 16))},
                selected_metadata_all_rows_joined=True,
                source_sha256={str(p): digest(p) for p in [receipt_path, Path(__file__)]},
                scope='All accepted conditional MBTB/provider-hit metadata in this recorded cohort, including '
                      'SC training exclusions. The eligible subset joins every old selected trace by lane, '
                      'private STAMP and common metadata. Invalid TAGE is excluded from direction comparisons. '
                      'Native ordinals define training phases; private STAMP is not calibrated to TEA. '
                      'This is not all retired/predicted branches, final BPU accuracy, causal overrides or saved '
                      'CPU cycles. No prediction-time weights are reconstructed from training SRAM responses.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    data = analyze(args.folder.resolve(), args.receipt.resolve())
    args.output.write_text(json.dumps(data, indent=2) + '\n')
    print(json.dumps(dict(passed=True, point=data['point'], cohorts=data['cohorts']), indent=2))


if __name__ == '__main__':
    main()
