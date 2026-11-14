"""Join recorded no-retirement intervals to TEA targets and final DIP slot fates.

Only source samples inside each gap and the native ROI are selected. All later
final records remain available. Exact gap lengths are separate from sampled
causes: a sampled target does not own every cycle of its surrounding gap.
"""
import argparse
from pathlib import Path
import sqlite3
from types import SimpleNamespace

import dip_analyze as dip
import tea_visualize as tea
from tea_dip_fate_state_joint import analyze as joint
from tea_dip_query_endpoint_evidence import digest
from tea_dip_wb_protocol_micro import write


def analyze(folder, receipt_path, top=5):
    full = joint(folder, receipt_path)
    begin, end = full['roi_begin'], full['roi_end']
    database = folder / 'simulator.db'
    samples = {}

    def sample(cycle, state, lanes, weight):
        samples[cycle] = dict(core_cycle=cycle, state=state, lane_weight=weight,
                              targets=[dict(pc=None if pc is None else hex(pc), psv=psv,
                                            events=tea.signature(psv)) for pc, psv in lanes])

    tea.load_data(database, folder.name,
                  SimpleNamespace(cycle_range=(begin, end), spec_checkpoint=False, bins=1),
                  sample_callback=sample)
    with sqlite3.connect(database.resolve().as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        gaps = list(db.execute('SELECT * FROM TeaNoCommit_0 WHERE START<? AND ENDCYCLE>? '
                               'ORDER BY ENDCYCLE-START DESC, START LIMIT ?', (end, begin, top)))
        contexts = {row['CYCLE']: row for row in db.execute(
            'SELECT * FROM Tea_0 WHERE CYCLE>=? AND CYCLE<? ORDER BY CYCLE,ID', (begin, end))}
        rows = list(db.execute('SELECT * FROM Dip_0 WHERE CYCLE>=? AND CYCLE<? ORDER BY CYCLE,ID', (begin, end)))
        assert set(samples) == contexts.keys() == {row['CYCLE'] for row in rows}
        width = full['dispatch_width']
        records = []
        for gap in gaps:
            length = gap['ENDCYCLE'] - gap['START']
            states = {state: gap['STATES_' + str(i)] for i, state in enumerate(tea.STATES[1:])}
            assert length > 0 and sum(states.values()) == length
            selected = [row for row in rows if gap['START'] <= row['CYCLE'] < gap['ENDCYCLE']]
            fates, details, selected_tea = None, None, []
            if selected:
                finals = db.execute('SELECT * FROM DipDelayedFinal_0 ORDER BY CYCLE,'
                                    'CASE KIND WHEN 1 THEN 0 WHEN 0 THEN 1 WHEN 3 THEN 2 ELSE 3 END,ID')
                profile, summary = dip.analyze(selected, finals, width)
                fates = {state: sum(count for (kind, _, _), count in profile.items()
                                    if dip.STATES[kind] == state) for state in dip.STATES}
                fates['Unresolved'] = summary['unresolved_slots']
                assert sum(fates.values()) == len(selected) * width
                details = [dict(state=dip.STATES[kind], cause_mask=cause,
                                cause=dip.cause_name(kind, cause), pc=hex(pc), slots=count)
                           for (kind, cause, pc), count in sorted(profile.items())]
                for row in selected:
                    cycle = row['CYCLE']
                    context = contexts[cycle]
                    selected_tea.append(dict(**samples[cycle], retired_before_sample=row['TOTALRETIRED'],
                                             retired_on_sample=row['RETIRED'], context={name: context[name] for name in context.keys()
                        if name.startswith(('CONTEXT_', 'TARGET'))}))
            records.append(dict(core_begin=gap['START'], core_exclusive_end=gap['ENDCYCLE'],
                                cycles=length, exact_states=states,
                                fully_inside_ROI=begin <= gap['START'] and gap['ENDCYCLE'] <= end,
                                left_censored=bool(gap['LEFTCENSORED']), right_censored=bool(gap['RIGHTCENSORED']),
                                source_samples=len(selected), TEA_samples=selected_tea,
                                DIP_final_slots=fates, DIP_final_profile=details))
    return dict(point=full['point'], source=full['source'], roi_begin=begin, roi_end=end,
                native_cycles=full['native_cycles'], native_retired=full['native_retired'],
                sample_period=full['sample_period'], dispatch_width=width,
                full_ROI_sampled_cycles=full['sampled_cycles'], gaps=records,
                receipt_sha256=full['receipt_sha256'], tool_sha256=digest(Path(__file__)),
                consumer_sha256={name: digest(Path(__file__).parent / name) for name in
                                 ['tea_dip_fate_state_joint.py', 'tea_visualize.py', 'dip_analyze.py']},
                scope='Longest recorded no-retirement gaps overlapping the native ROI, with full gap state counts '
                      'and censoring retained. A boundary gap includes cycles outside ROI; its state budget is not '
                      'clipped. TEA targets and DIP slot fates refer only to identical source samples inside gap '
                      'and ROI, with all later final records retained. Empty sampling populations remain null. '
                      'A sampled target or event does not explain the whole gap, and DIP observes dispatch slots, '
                      'not that same retirement target. Recorded gap thresholds can omit shorter intervals. '
                      'Gap lengths are not load-service latency or recoverable CPU cycles; no address, prefetch '
                      'ownership, request identity or cross-run instruction matching is inferred.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', required=True, type=Path)
    parser.add_argument('--receipt', required=True, type=Path)
    parser.add_argument('--output', required=True, type=Path)
    parser.add_argument('--top', type=int, default=5)
    args = parser.parse_args()
    write(args.output, analyze(args.folder.resolve(), args.receipt.resolve(), args.top))


if __name__ == '__main__':
    main()
