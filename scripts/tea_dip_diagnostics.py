"""Supplement TEA/DIP with request-qualified native gates and FP issue stages.

Use only on closed simulator directories. Missing diagnostics remain unknown.
The provided source contract is evidence about that source, not proof of its
relationship to an arbitrary binary; retain the build/launch manifest as well.
"""
import argparse
import hashlib
import json
from pathlib import Path
import re
import sqlite3

from native_perf import read_blocks, unique_counters

GATES = {
    'diag_rename_blocked_gate_mask': ['IntFreelist', 'FpFreelist', 'VecFreelist', 'VlFreelist', 'Dispatch', 'Walk', 'LSQ'],
    'diag_rob_dispatch_gate_mask': ['ROBReservation', 'RAB', 'VtypeBuffer', 'BlockBackward', 'VecException'],
}


def digest(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def gate_histogram(fields, name, labels):
    matches = [key for key in fields if key.endswith('.' + name + '_sampled')]
    if not matches:
        return None
    assert len(matches) == 1, name
    prefix = matches[0][:-len('sampled')]
    bins = [fields[prefix + f'{i}_{i+1}'] for i in range(1 << len(labels))]
    sampled = fields[prefix + 'sampled']
    assert all(n >= 0 for n in bins) and sum(bins) == sampled, name
    assert fields[prefix + 'underflow'] == fields[prefix + 'overflow'] == 0, name
    assert sum(i * n for i, n in enumerate(bins)) == fields[prefix + 'sum'], name
    # Mask bins form one budget. Marginal gate counts overlap and do not.
    return dict(sampled=sampled, no_named_gate_samples=bins[0],
                overlapping_gate_samples=sum(n for i, n in enumerate(bins) if i.bit_count() > 1),
                masks=[dict(mask=i, gates=[label for bit, label in enumerate(labels) if i & (1 << bit)], samples=n)
                       for i, n in enumerate(bins) if n],
                marginal_samples={label: sum(n for i, n in enumerate(bins) if i & (1 << bit))
                                  for bit, label in enumerate(labels)})


def phase_counters(block):
    duplicated = [key for key, values in block.items() if '.diag_' in key and len(values) != 1]
    assert not duplicated, ('ambiguous diagnostic names', duplicated)
    counters = unique_counters(block)
    diagnostics = {key: value for key, value in counters.items() if '.diag_' in key}
    histograms = {name: gate_histogram(counters, name, labels) for name, labels in GATES.items()}
    fp = []
    for key in sorted(counters):
        if not key.endswith('.diag_fp_live_entry_cycles'):
            continue
        prefix = key[:-len('live_entry_cycles')]
        values = {state: counters[prefix + state + '_entry_cycles'] for state in
                  ['live', 'issued', 'blocked', 'src_wait', 'eligible']}
        assert values['live'] == sum(values[state] for state in ['issued', 'blocked', 'src_wait', 'eligible']), key
        fp.append(dict(module=key.rsplit('.', 1)[0], entry_cycles=values,
                       wb_conflict_entry_cycles=counters[prefix + 'wb_conflict_entry_cycles'],
                       selected_uops=counters[prefix + 'selected_uops'],
                       success_responses=counters[prefix + 'success_responses'],
                       cancelled_entries=counters[prefix + 'cancelled_entries']))
    return dict(gate_histograms=histograms, fp_issue_stages=fp or None, diagnostic_counters=diagnostics,
                diagnostic_availability='present' if diagnostics else 'missing',
                duplicate_native_names=sum(len(values) > 1 for values in block.values()))


def analyze(directory, source_root=None):
    directory = Path(directory)
    assert (directory / 'exitcode').read_text().strip() == '0', 'simulator must have exited successfully'
    stdout = (directory / 'simulator_out.txt').read_text()
    assert 'Difftest enabled' in stdout and 'EXCEEDING CYCLE/INSTR LIMIT' in stdout
    assert 'HIT CRITICAL' not in stdout
    periods = []
    for view in ['Tea', 'Dip']:
        assert re.search(r'^enable' + view + r'0 = 1$', stdout, re.M)
        match = re.search(r'^' + view.lower() + r'SamplePeriod0 = (\d+)$', stdout, re.M)
        assert match and int(match[1]) > 0
        periods.append(int(match[1]))
    assert periods[0] == periods[1], 'joint samples require matching periods'
    blocks = read_blocks(directory / 'simulator_err.txt')
    assert len(blocks) == 2, 'expected separate warmup and ROI dumps'
    warm, final = sorted(blocks)
    native = unique_counters(blocks[final])
    def rob(name):
        values = [value for key, value in native.items() if key.endswith('.rob.' + name)]
        assert len(values) == 1, name
        return values[0]
    cycles, instructions = rob('clock_cycle'), rob('commitInstr')
    assert cycles == final - warm and instructions > 0
    begin, end = warm + 1, final + 1
    with sqlite3.connect((directory / 'simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        summary = db.execute('SELECT * FROM TeaCycleSummary_0 WHERE CYCLE>=? AND CYCLE<=? ORDER BY CYCLE', (begin, end)).fetchall()
        assert len(summary) >= 2
        first, last = summary[0], summary[-1]
        span = last['CYCLE'] - first['CYCLE']
        exact = [last[f'STATES_{i}'] - first[f'STATES_{i}'] for i in range(4)]
        assert span > 0 and sum(exact) == span
        sampled = dict(db.execute('SELECT STATE,count(*) FROM Tea_0 WHERE CYCLE>=? AND CYCLE<? GROUP BY STATE', (first['CYCLE'], last['CYCLE'])).fetchall())
        assert sum(sampled.values()) > 0
        errors = {name: 100 * (sampled.get(i, 0) / sum(sampled.values()) - exact[i] / span)
                  for i, name in enumerate(['Computing', 'Stalled', 'Flushed', 'Drained'])}
        columns = {row['name'] for row in db.execute('pragma table_info(Tea_0)')}
        raw_valid = None
        if 'CONTEXT_HEADREASONVALID' in columns:
            raw_valid = db.execute('SELECT count(*) FROM Tea_0 WHERE CYCLE>=? AND CYCLE<? AND STATE=1 AND CONTEXT_HEADREASONVALID!=0', (begin, end)).fetchone()[0]
        grids = {}
        for table in ['Tea_0', 'Dip_0']:
            rows = [r[0] for r in db.execute(f'SELECT CYCLE FROM {table} WHERE CYCLE>=? AND CYCLE<? ORDER BY CYCLE', (begin, end))]
            assert len(rows) == len(set(rows)) and all(c % periods[0] == 0 for c in rows), table
            grids[table] = rows
        assert grids['Tea_0'] and grids['Tea_0'] == grids['Dip_0']
        expected = set(range((begin + periods[0] - 1) // periods[0] * periods[0], end, periods[0]))
        missing = expected - set(grids['Tea_0'])
        assert len(missing) <= 1 and missing <= {min(expected), max(expected)}, ('missing samples', missing)
        assert all(b - a == periods[0] for a, b in zip(grids['Tea_0'], grids['Tea_0'][1:])), 'missing interior samples'
    contract = None
    if source_root is not None:
        path = Path(source_root) / 'src/main/scala/xiangshan/backend/Backend.scala'
        zero_fed = 'ctrlBlock.io.robio.topdownIQInfoVec.foreach(x => x := 0.U.asTypeOf(x))' in path.read_text()
        contract = dict(path=str(path), sha256=digest(path), head_issue_observation_available=False if zero_fed else None,
                        scope='Provided source only; bind to the executed binary with its build/launch manifest.')
    return dict(directory=str(directory), cycles=cycles, instructions=instructions, cpi=cycles / instructions,
                sample_period=periods[0], samples=len(grids['Tea_0']), missing_endpoint_samples=sorted(missing),
                exact_interior=dict(begin=first['CYCLE'], end=last['CYCLE'], cycles=span,
                                    omitted_edge_cycles=cycles-span, sample_error_pp=errors),
                raw_head_reason_valid_stalled_samples=raw_valid, provided_source_contract=contract,
                phases={name: phase_counters(blocks[stamp]) for name, stamp in [('warmup', warm), ('roi', final)]},
                limits=['Sample grid integrity does not prove representative sampling or per-PC accuracy.',
                        'TEA/DIP and native counters are different observation budgets; do not add them.',
                        'Gate histograms count request exposure, not independent causal CPI savings.',
                        'Issue entry-cycles, selected uops and success responses have different units.',
                        'No producer-consumer edges, request latency segments or branch recovery timeline are collected.'],
                files={name: digest(directory / name) for name in ['simulator_out.txt', 'simulator_err.txt', 'simulator.db', 'exitcode']})


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('directory', type=Path)
    parser.add_argument('--source-root', type=Path)
    parser.add_argument('--output', required=True, type=Path)
    args = parser.parse_args()
    result = analyze(args.directory, args.source_root)
    args.output.write_text(json.dumps(result, ensure_ascii=False, indent=2) + '\n')
    print(json.dumps(dict(cpi=result['cpi'], diagnostic_availability=result['phases']['roi']['diagnostic_availability'],
                          provided_source_contract=result['provided_source_contract']), ensure_ascii=False))


if __name__ == '__main__':
    main()
