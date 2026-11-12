"""Separate sampled same-line MSHR candidates before/after the last downstream data beat.

RXDAT-last is an MSHR milestone, not the CPU's data-return or writeback identity.
The existing association reader first checks closed receipts and native cohorts.
"""
from collections import Counter
from pathlib import Path
import argparse
import hashlib
import json
import sqlite3

from tea_dip_load_mshr_association import analyze as associations


def return_cycles(rows):
    active, returned = {}, {}
    for row in rows:
        cycle, bank = row['NATIVECYCLE'], row['EVENTS_BANK']
        assert cycle == row['STAMP']
        # A return/free/next allocation on one edge belongs to the old owner.
        for entry in range(16):
            if row[f'EVENTS_RETURNED_{entry}_VALID']:
                start = active[bank, entry]
                assert cycle - start == row[f'EVENTS_RETURNED_{entry}_BITS']
                returned.setdefault((bank, entry, start), []).append(cycle)
        for entry in range(16):
            if row[f'EVENTS_FREED_{entry}_VALID']:
                start = active.pop((bank, entry))
                assert cycle - start == row[f'EVENTS_FREED_{entry}_BITS']
        if row['EVENTS_ALLOCATED_VALID']:
            key = bank, row['EVENTS_ALLOCATED_BITS_ENTRY']
            assert key not in active
            active[key] = cycle
    return returned


def annotate(samples, returned):
    counts = Counter()
    for row in samples:
        if row['status'] != 'same_line_live_owner_candidate':
            continue
        owner = row['owner']
        events = returned.get((owner['bank'], owner['entry'], owner['start']), [])
        if not events:
            stage = 'no_logged_last_data_event'
        elif len(events) != 1:
            stage = 'multiple_last_data_events'
        else:
            event, = events
            row['last_downstream_data_cycle'] = event
            stage = ('before_last_data' if row['sample_cycle'] < event else
                     'same_cycle_as_last_data' if row['sample_cycle'] == event else
                     'after_last_data_before_free')
        row['downstream_data_stage'] = stage
        counts[stage] += 1
    return dict(counts)


def analyze(folder, receipt):
    data = associations(folder, receipt)
    with sqlite3.connect((folder.resolve() / 'simulator.db').as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        returned = return_cycles(db.execute(
            'SELECT * FROM L2MshrLifecycle ORDER BY NATIVECYCLE,EVENTS_BANK'))
    data['source_sha256'][str(Path(__file__).resolve())] = hashlib.sha256(Path(__file__).read_bytes()).hexdigest()
    data['downstream_data_stage_counts'] = annotate(data['samples'], returned)
    data['owners_with_last_data_events'] = len(returned)
    data['last_data_event_count'] = sum(map(len, returned.values()))
    data['scope'] += (' Last-downstream-data events are tied to the owner generation, with same-edge '
                      'return/free/reallocation ordering checked. Before/at/after-last-data stages '
                      'do not establish CPU data return, request identity or recoverable CPI. '
                      'Owners without a logged event or with multiple events remain explicit.')
    return data


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', required=True, type=Path)
    parser.add_argument('--receipt', required=True, type=Path)
    parser.add_argument('--output', required=True, type=Path)
    args = parser.parse_args()
    data = analyze(args.folder, args.receipt)
    args.output.write_text(json.dumps(data, indent=2) + '\n')
    print(json.dumps({key: data[key] for key in ['passed', 'point', 'last_data_event_count',
                                               'downstream_data_stage_counts']}))


if __name__ == '__main__':
    main()
