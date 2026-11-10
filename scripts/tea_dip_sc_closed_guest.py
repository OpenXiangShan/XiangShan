"""Audit selected SC training records from one closed guest; keep ROI uncertainty explicit."""
from collections import Counter, defaultdict
from pathlib import Path
import argparse
import heapq
import json
import re
import sqlite3

from native_perf import read_blocks, unique_counters
from tea_dip_query_endpoint_evidence import digest, verify

COLUMNS = ['ID', 'SCCORRECT', 'SCWRONG', 'SCWRONGTAGEWRONG', 'SCCORRECTTAGECORRECT',
           'SCWRONGTAGECORRECT', 'SCCORRECTTAGEWRONG', 'MISPREDICT', 'ACTUALTAKEN',
           'USESC', 'SCPRED', 'SUMABOVETHRES', 'BIASRESP', 'GLOBALRESP_0', 'GLOBALRESP_1',
           'PATHRESP_0', 'PATHRESP_1', 'PROVIDERCTR', 'PROVIDERTAKEN', 'PROVIDERVALID',
           'CFIPC', 'STARTPC_ADDR', 'STAMP', 'SITE']
BOOLS = COLUMNS[1:12] + ['PROVIDERTAKEN', 'PROVIDERVALID']
FINAL_FIELDS = ['TAGEFINALTAKEN', 'TAGEFINALVALID', 'TAGEUSEPROVIDER']
FINAL_COLUMNS = COLUMNS[:17] + FINAL_FIELDS + COLUMNS[17:]
OUTCOMES = dict(SCCORRECTTAGEWRONG='sc_correct_tage_wrong',
                SCWRONGTAGECORRECT='sc_wrong_tage_correct',
                SCCORRECTTAGECORRECT='sc_correct_tage_correct',
                SCWRONGTAGEWRONG='sc_wrong_tage_wrong')
NATIVE = [*OUTCOMES.values(), 't1_use_sc', 't1_not_use_sc']



def training_handshake(blocks):
    """Keep actual valid training stalls separate from the ungated SC bank predicate."""
    phases = []
    labels = ['warmup', 'ROI'] if len(blocks) == 2 else ['coremark']
    for label, (stamp, block) in zip(labels, blocks):
        counters = unique_counters(block)

        def get(suffix):
            value, = [v for k, v in counters.items() if k.endswith(suffix)]
            return value

        blocked = get('.bpu.train_stall')
        conflict = get('.sc.t0_writeConflict')
        phases.append(dict(phase=label, native_print_stamp=stamp,
                           accepted_packets=get('.bpu.train_total'), blocked_valid_cycles=blocked,
                           sc_bank_conflict_condition_cycles=conflict,
                           SC_conditions_without_pending_train_lower_bound=max(0, conflict-blocked)))
    return dict(phases=phases,
                scope='BPU train_stall is valid && !ready; SC t0_writeConflict is a bank predicate without '
                      'train.valid gating. Their difference gives only a lower bound on SC conditions when '
                      'no train packet is pending. Global stalls include other predictors; neither count '
                      'is exclusive CPU loss or uniquely saved cycles.')


def final_tage_outcome(row):
    valid, provider = row['TAGEFINALVALID'], row['TAGEUSEPROVIDER']
    source = 'provider' if provider else 'alternate' if valid else 'invalid'
    counts = Counter(rows=1, useSc=row['USESC'])
    if valid and row['USESC']:
        sc = row['SCPRED'] == row['ACTUALTAKEN']
        tage = row['TAGEFINALTAKEN'] == row['ACTUALTAKEN']
        counts.update(dict(sc_correct_tage_wrong=int(sc and not tage),
                           sc_wrong_tage_correct=int(not sc and tage),
                           both_correct=int(sc and tage), both_wrong=int(not sc and not tage)))
    return source, counts


def trace_phases(db, blocks, has_final_tage=False):
    """Use the dedicated producer event count; legacy way counters cannot supply cuts."""
    names = {key for _, block in blocks for key in block
             if key.endswith('.sc.diag_sc_cond_trace_events')}
    if not names:
        return None
    assert len(names) == 1
    name, = names

    def rows(lane):
        for row in db.execute('SELECT * FROM scCondTrace_' + str(lane) + ' ORDER BY ID'):
            yield row['STAMP'], lane, row

    merged = iter(heapq.merge(*[rows(i) for i in range(8)]))
    pending = next(merged, None)
    phases = []
    labels = ['warmup', 'ROI'] if len(blocks) == 2 else ['coremark']
    for label, (stamp, block) in zip(labels, blocks):
        counters = unique_counters(block)
        amount = counters[name]
        observed = Counter()
        choices = defaultdict(Counter)
        first = last = None
        for _ in range(amount):
            assert pending is not None, 'Native event count exceeds actual SC records'
            cycle, lane, row = pending
            first = cycle if first is None else first
            last = cycle
            pending = next(merged, None)
            observed['useSc'] += row['USESC']
            if has_final_tage:
                source, counts = final_tage_outcome(row)
                choices[source].update(counts)
            for field, key in OUTCOMES.items():
                observed[key] += row[field]
        for key in ['useSc', *OUTCOMES.values()]:
            native = 't1_use_sc' if key == 'useSc' else key
            expected = sum(v for k, v in counters.items()
                           if re.search(r'\.sc\.' + native + r'\d+$', k))
            assert observed[key] == expected, (label, key, observed[key], expected)
        if pending is not None and last is not None:
            assert last < pending[0], 'Native phase cut splits simultaneous trace lanes'
        phases.append(dict(phase=label, native_print_stamp=stamp, trace_events=amount,
                           first_private_stamp=first, last_private_stamp=last,
                           selected_raw_provider_outcomes=dict(observed)))
        if has_final_tage:
            phases[-1]['final_tage_choice'] = {k: dict(v) for k, v in sorted(choices.items())}
    assert pending is None, 'SC records remain after all native event counts'
    return phases


def analyze(folder, receipt_path):
    receipt = json.loads(receipt_path.read_text())
    result = json.loads((folder / 'result.json').read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and receipt['point'] == folder.name
    assert result['original_process_closed'] and result['exitcode'] == 0 and result['difftest_passed']
    assert all(result['sampler_flags'].values())
    assert result['source'] == receipt['source'] and result['process'] == receipt['process']
    verify(folder, receipt['files'])
    command = json.loads((folder / 'command.json').read_text())
    selected = set(command[command.index('--dump-select-db') + 1].split())
    tables = ['scCondTrace_' + str(i) for i in range(8)]
    assert selected.issuperset(tables)
    blocks = sorted(read_blocks(folder / 'simulator_err.txt').items())
    assert len(blocks) == (2 if '-W' in command else 1)
    phases = []
    for label, (stamp, block) in zip(['warmup', 'ROI'] if len(blocks) == 2 else ['coremark'], blocks):
        counters = unique_counters(block)
        groups = {}
        for name in NATIVE:
            group = {int(match[1]): value for key, value in counters.items()
                     if (match := re.search(r'\.sc\.' + name + r'(\d+)$', key))}
            assert group and set(group) == set(range(len(group))), name
            groups[name] = group
        assert len({len(group) for group in groups.values()}) == 1
        retired, = [value for key, value in counters.items() if key.endswith('.rob.commitInstr')]
        phases.append(dict(phase=label, native_print_stamp=stamp, native_retired=retired,
                           native_way_counters=groups))
    native = {name: sum(sum(p['native_way_counters'][name].values()) for p in phases) for name in NATIVE}
    pcs = defaultdict(Counter)
    ctrs = defaultdict(Counter)
    totals, table_counts = Counter(), {}
    final_choices = defaultdict(Counter)
    final_pcs = defaultdict(lambda: defaultdict(Counter))
    with sqlite3.connect((folder / 'simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        columns = [r['name'] for r in db.execute('PRAGMA table_info(' + tables[0] + ')')]
        assert columns in [COLUMNS, FINAL_COLUMNS]
        has_final_tage = columns == FINAL_COLUMNS
        for table in tables:
            assert [r['name'] for r in db.execute('PRAGMA table_info(' + table + ')')] == columns
            previous_stamp, count = -1, 0
            for ordinal, row in enumerate(db.execute('SELECT * FROM ' + table + ' ORDER BY ID'), 1):
                assert row['ID'] == ordinal and row['STAMP'] >= previous_stamp
                previous_stamp, count = row['STAMP'], ordinal
                assert all(row[name] in [0, 1] for name in BOOLS)
                assert row['PROVIDERVALID'] == 1 and 0 <= row['PROVIDERCTR'] <= 7
                assert row['PROVIDERTAKEN'] == int(row['PROVIDERCTR'] >= 4)
                totals['rows'] += 1
                pc = hex(row['CFIPC'])
                pcs[pc]['rows'] += 1
                if has_final_tage:
                    assert all(row[name] in [0, 1] for name in FINAL_FIELDS)
                    if row['TAGEUSEPROVIDER']:
                        assert row['TAGEFINALVALID'] and row['TAGEFINALTAKEN'] == row['PROVIDERTAKEN']
                    source, counts = final_tage_outcome(row)
                    final_choices[source].update(counts)
                    final_pcs[pc][source].update(counts)
                if row['USESC']:
                    sc = row['SCPRED'] == row['ACTUALTAKEN']
                    tage = row['PROVIDERTAKEN'] == row['ACTUALTAKEN']
                    flags = dict(SCCORRECT=sc, SCWRONG=not sc,
                                 SCCORRECTTAGEWRONG=sc and not tage,
                                 SCWRONGTAGECORRECT=not sc and tage,
                                 SCCORRECTTAGECORRECT=sc and tage,
                                 SCWRONGTAGEWRONG=not sc and not tage)
                    assert all(row[name] == int(value) for name, value in flags.items())
                    assert not (sc and tage), 'Excluded by the selected training predicate'
                    totals['useSc'] += 1
                    pcs[pc]['useSc'] += 1
                    for field, name in OUTCOMES.items():
                        totals[name] += row[field]
                        pcs[pc][name] += row[field]
                        ctrs[row['PROVIDERCTR']][name] += row[field]
                else:
                    assert all(row[name] == 0 for name in ['SCCORRECT', 'SCWRONG', *OUTCOMES])
            table_counts[table] = count
        exact_phases = trace_phases(db, blocks, has_final_tage)
    assert totals['useSc'] == native['t1_use_sc']
    assert all(totals[name] == native[name] for name in OUTCOMES.values())
    trace_not_use = totals['rows'] - totals['useSc']
    data = dict(passed=True, point=receipt['point'], source=receipt['source'], tables=table_counts,
                totals=dict(totals), by_provider_ctr={str(k): dict(v) for k, v in sorted(ctrs.items())},
                by_pc={k: dict(v) for k, v in sorted(pcs.items(), key=lambda item: int(item[0], 16))},
                native_phases=phases, selected_native_margins_match=True,
                non_use_cohort=dict(trace_rows=trace_not_use, native_way_counts=native['t1_not_use_sc'],
                                    difference=trace_not_use-native['t1_not_use_sc']),
                source_sha256={str(p): digest(p) for p in [receipt_path, Path(__file__)]},
                scope='Whole recorded selected-training cohort, including warmup. Existing native way counts do not '
                      'identify all trace-event phase boundaries; private STAMP is not calibrated to TEA. '
                      'Training excludes correct agreeing useSc events. Per-PC and provider-CTR outcomes are selected '
                      'training counts, not overall branch accuracy, causal overrides or predicted saved cycles. '
                      'Native TAGE outcome names refer to the raw longest-history provider, not the final '
                      'UseAltOnNa-selected TAGE prediction. '
                      'Path/global/bias responses are old training-time entries, not prediction-time weights. '
                      'When trace_phase_cohorts is present, dedicated trace-valid native totals validate ordinal '
                      'training phases; this does not calibrate private STAMP to TEA or establish prediction accuracy.')
    data['training_handshake'] = training_handshake(blocks)
    if exact_phases is not None:
        data['trace_phase_cohorts'] = exact_phases
    if has_final_tage:
        data['final_tage_choice'] = dict(
            totals={k: dict(v) for k, v in sorted(final_choices.items())},
            by_pc={pc: {k: dict(v) for k, v in sorted(choices.items())}
                   for pc, choices in sorted(final_pcs.items(), key=lambda item: int(item[0], 16))},
            scope='Prediction metadata selects provider or alternate; invalid TAGE is excluded from outcome '
                  'comparisons. This remains a selected training cohort, not full prediction accuracy or '
                  'a causal count of SC overrides. Private STAMP remains uncalibrated to TEA.')
    return data


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    data = analyze(args.folder.resolve(), args.receipt.resolve())
    args.output.write_text(json.dumps(data, indent=2) + '\n')
    print(json.dumps(dict(passed=True, point=data['point'], totals=data['totals'],
                          non_use_cohort=data['non_use_cohort']), indent=2))


if __name__ == '__main__':
    main()
