"""Audit exact TEA ROI states against coarse snapshots and the separate DIP budget.

Input is a *closed, neutrality-checked* query-endpoint evidence.json. This tool never
opens a simulator database or interprets sampled PC attribution as a causal chain.
"""
from pathlib import Path
import argparse
import csv
import json

STATES = ('Computing', 'Stalled', 'Flushed', 'Drained')


def analyze(source):
    if source['pending']:
        return {'pending': True, 'database_read': False, 'rows': []}
    assert source['all_observation_pairs_neutral'], 'instrumentation neutrality failed'
    assert source['completed'] == source['expected'] == len(source['rows']) and source['rows']
    rows = []
    for record in source['rows']:
        assert record['observation_neutrality_valid'], record['point']
        comparison = record['comparison']
        before, after = comparison['windows']
        assert before['native_roi_cycle_duration_matches'] and after['native_roi_cycle_duration_matches']
        retired = [window['roi']['native_reported_retired'] for window in (before, after)]
        assert min(retired) > 0
        budgets = {}
        for part, field in [('interior', 'coarse_interior'), ('edge', 'newly_visible_edges'), ('exact', None)]:
            values = [window['roi'][field]['states'] if field else window['roi']['states']
                      for window in (before, after)]
            budgets[part] = {state: values[1][state] / retired[1] - values[0][state] / retired[0]
                             for state in STATES}
        for state in STATES:
            assert abs(budgets['interior'][state] + budgets['edge'][state] - budgets['exact'][state]) < 1e-12
            assert abs(budgets['exact'][state] - comparison['exact_state_delta_cpi'][state]) < 1e-12
        assert abs(sum(budgets['exact'].values()) - comparison['delta_cpi']) < 1e-12
        dip = comparison['dip_exclusive_delta_cpi']
        assert abs(sum(dip.values()) - comparison['delta_cpi']) < 1e-9
        edge_reversal = [state for state in STATES if budgets['interior'][state] * budgets['exact'][state] < 0]
        dip_tea_disagree = dip['Stall'] * budgets['exact']['Stalled'] < 0
        rows.append({
            'point': record['point'], 'speedup': record['performance_speedup'],
            'delta_cpi': comparison['delta_cpi'], 'retired': retired,
            'edge_cycles': [window['roi']['newly_visible_edges']['cycles'] for window in (before, after)],
            'retirement_counter_difference': [window['roi']['retirement_counter_difference'] for window in (before, after)],
            'state_delta_cpi': budgets, 'dip_exclusive_delta_cpi': dip,
            'coarse_to_exact_sign_reversal': edge_reversal,
            'dip_stall_vs_exact_tea_stalled_disagree': dip_tea_disagree,
        })
    return {'pending': False, 'database_read': False, 'rows': rows,
            'scope': ('Each TEA state is normalized by its own native ROI retired count. '
                      'Interior + previously omitted edges = exact ROI state, which closes to measured CPI. '
                      'DIP is a separate exclusive budget; native counters may overlap. '
                      'Direction disagreements identify a measurement blind spot, not a unique causal mechanism. '
                      'Per-PC dynamic critical path and host-time cause remain unobserved.')}


def write_chart(rows, path):
    import matplotlib
    matplotlib.use('Agg')
    import matplotlib.pyplot as plt
    figure, axes = plt.subplots(len(rows), 1, figsize=(11, 2.8 * len(rows)), squeeze=False)
    colors = ('#718ba3', '#e5a64e', '#263a4b')
    for axis, row in zip(axes[:, 0], rows):
        for offset, part, color in zip((-.25, 0, .25), ('interior', 'edge', 'exact'), colors):
            axis.bar([index + offset for index in range(len(STATES))],
                     [row['state_delta_cpi'][part][state] for state in STATES],
                     width=.23, color=color, label=part)
        axis.axhline(0, color='#74818b', linewidth=.8)
        axis.set_xticks(range(len(STATES)), STATES)
        axis.set_ylabel('Delta CPI')
        axis.set_title(f"{row['point']}  guest speedup {row['speedup']:+.3%}", loc='left')
        axis.legend(frameon=False, ncol=3, loc='upper right')
    figure.suptitle('Exact TEA ROI = coarse interior + formerly omitted edges', fontsize=14)
    figure.tight_layout()
    figure.savefig(path, dpi=160)
    figure.savefig(path.with_suffix('.pdf'))
    plt.close(figure)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--evidence', required=True, type=Path)
    parser.add_argument('--output-dir', required=True, type=Path)
    args = parser.parse_args()
    result = analyze(json.loads(args.evidence.read_text()))
    args.output_dir.mkdir(parents=True, exist_ok=True)
    (args.output_dir / 'resolution-audit.json').write_text(json.dumps(result, indent=2) + '\n')
    if not result['pending']:
        with (args.output_dir / 'resolution-audit.csv').open('w', newline='') as stream:
            writer = csv.writer(stream)
            writer.writerow(['point', 'state', 'interior_delta_cpi', 'edge_delta_cpi', 'exact_delta_cpi',
                             'dip_stall_delta_cpi', 'edge_sign_reversal'])
            for row in result['rows']:
                for state in STATES:
                    writer.writerow([row['point'], state, *(row['state_delta_cpi'][part][state]
                                                             for part in ('interior', 'edge', 'exact')),
                                     row['dip_exclusive_delta_cpi']['Stall'] if state == 'Stalled' else '',
                                     state in row['coarse_to_exact_sign_reversal']])
        write_chart(result['rows'], args.output_dir / 'resolution-audit.png')
    print(json.dumps({'pending': result['pending'], 'points': len(result['rows']),
                      'reversals': {row['point']: row['coarse_to_exact_sign_reversal'] for row in result['rows']}}, indent=2))


if __name__ == '__main__':
    main()
