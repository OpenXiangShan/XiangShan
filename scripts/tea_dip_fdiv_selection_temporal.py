"""Cross-check closed FDIV observer dumps against the unchanged dense micro trace."""
import argparse
from datetime import datetime, timezone
import json
from pathlib import Path
import sqlite3

from native_perf import read_blocks, unique_counters
from tea_dip_micro_retirement import analyze as retirement_analysis
from tea_dip_query_endpoint_evidence import TABLES, digest, verify


def closed(root):
    receipt = json.loads((root / 'closure-receipt.json').read_text())
    assert receipt['ready'] and not receipt['controller_live'] and not receipt['original_guest_live']
    verify(root, receipt['files'])
    result = json.loads((root / 'observer/result.json').read_text())
    assert result['passed'] and result['difftest_passed'] and result['original_process_closed']
    assert all(result['sampler_flags'].values()) and result['period'] == 1
    return result


def analyze(evidence):
    baseline = evidence / 'closed'
    original = closed(baseline)
    audit = json.loads((baseline / 'audit.json').read_text())
    assert audit['passed'] and audit['observation_neutrality']['passed']
    names = audit['observation_neutrality']['exact_removed_new_native_names']
    assert len(names) == 4
    old_block, = read_blocks(baseline / 'observer/simulator_err.txt').values()
    old_counts = unique_counters(old_block)
    groups = {}
    for variant in ['temporal512', 'warmup250']:
        root = evidence / variant / 'closed'
        result = closed(root)
        for key in ['source', 'binary_sha256', 'instructions', 'cycles', 'observed_good_trap_pc']:
            assert result[key] == original[key], (variant, key)
        tables = {}
        for table in TABLES:
            def rows(folder):
                with sqlite3.connect((folder / 'observer/simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as db:
                    return db.execute('SELECT * FROM ' + table + ' ORDER BY ID').fetchall()
            left, right = rows(baseline), rows(root)
            assert left == right, (variant, table)
            tables[table] = len(left)
        snapshots = []
        cumulative = dict.fromkeys(names, 0)
        for stamp, block in sorted(read_blocks(root / 'observer/simulator_err.txt').items()):
            counts = unique_counters(block)
            values = {name: counts[name] for name in names}
            for name, value in values.items():
                assert value >= 0
                cumulative[name] += value
            def rob(suffix):
                value, = [v for name, v in counts.items() if name.endswith('.rob.' + suffix)]
                return value
            snapshots.append(dict(native_print_stamp=stamp, tea_exact_prefix_cycle=rob('tea_exact_endpoint_cycle'),
                                  tea_exact_prefix_retired=rob('tea_exact_endpoint_retired'),
                                  interval_counts=values, cumulative_counts=cumulative.copy()))
        assert cumulative == {name: old_counts[name] for name in names}
        groups[variant] = dict(tables_equal=True, table_rows=tables, snapshots=snapshots,
                               receipt_sha256=digest(root / 'closure-receipt.json'))
    selected, = [name for name in names if name.endswith('issueQueues_2.diag_fdiv_retry_extra_selected_uops')]
    assert old_counts[selected] == 1
    periodic = groups['temporal512']['snapshots']
    first_index = next(i for i, row in enumerate(periodic) if row['interval_counts'][selected])
    assert first_index > 0
    warmup = groups['warmup250']['snapshots']
    assert len(warmup) == 2 and warmup[0]['interval_counts'][selected] == 1 and warmup[1]['interval_counts'][selected] == 0
    lower = periodic[first_index - 1]['native_print_stamp']
    upper = min(periodic[first_index]['native_print_stamp'], warmup[0]['native_print_stamp'])
    dense = evidence.parent / 'period1-denormal-20261109' / 'closed'
    retirement = retirement_analysis(dense, 0x8000003c, 0x80000022, 'sampler')
    return dict(utc=datetime.now(timezone.utc).isoformat(), passed=True, source=original['source'],
                groups=groups, counter_visibility_native_print_interval=dict(after=lower, by=upper),
                first_retirement_time_difference=retirement['first_time_difference'],
                payload_retirement_cycle_delta=retirement['payload_cycle_delta'],
                retirement_frontiers=retirement['retirement_frontiers'],
                inputs_sha256={str(baseline/'closure-receipt.json'):digest(baseline/'closure-receipt.json'),
                               str(dense/'closure-receipt.json'):digest(dense/'closure-receipt.json'),
                               str(Path(__file__)):digest(Path(__file__))},
                scope='Same observer binary, image and period1; native dump/clean timing differs. All six full TEA/DIP tables and diagnostic totals must match. '
                      'The interval brackets counter visibility in native print time, not an exact IQ event cycle. TEA prefix and retirement time are separate observations. '
                      'No dynamic ROB identity or FU acceptance is measured; temporal ordering is not a unique causal explanation or saved-cycle estimate.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--evidence-dir', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    result = analyze(args.evidence_dir.resolve())
    args.output.write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps({key: result[key] for key in ['passed', 'counter_visibility_native_print_interval',
                                                  'first_retirement_time_difference', 'payload_retirement_cycle_delta']}, indent=2))


if __name__ == '__main__':
    main()
