"""Audit closed single-core BOP phase snapshots without assuming a TEA clock offset."""
import argparse
import json
from pathlib import Path
import sqlite3

from native_perf import read_blocks
from tea_dip_query_endpoint_evidence import digest, verify
from tea_dip_wb_protocol_micro import write

FIELDS = {'CONFBEFORE', 'CONFAFTER', 'THRESHOLD', 'TEACHERDISABLED', 'COMMITTEDTEACHERENABLED',
          'STUDENTSELECTED', 'EXTERNALLYENABLED', 'GATEBEFORE', 'GATEAFTER'}
BOOLS = FIELDS - {'CONFBEFORE', 'CONFAFTER', 'THRESHOLD'}


def audit_rows(rows):
    previous = None
    for row in rows:
        before, after, threshold = [row[key] for key in ['CONFBEFORE', 'CONFAFTER', 'THRESHOLD']]
        assert all(0 <= value <= 15 for value in [before, after, threshold])
        assert all(row[key] in (0, 1) for key in BOOLS)
        expected = min(15, before + 2) if row['TEACHERDISABLED'] else max(0, before - 1)
        assert after == expected, 'confidence update does not match the observed teacher feedback'
        assert row['GATEBEFORE'] == int(before < threshold)
        assert row['GATEAFTER'] == int(after < threshold)
        if previous is not None:
            assert row['ID'] == previous['ID'] + 1, 'missing confidence-update row'
            assert row['STAMP'] > previous['STAMP']
            assert before == previous['CONFAFTER'], 'confidence continuity lost between logged updates'
        previous = row
    pairs = max(0, len(rows) - 1)
    matches = sum(current['TEACHERDISABLED'] != prior['COMMITTEDTEACHERENABLED']
                  for prior, current in zip(rows, rows[1:]))
    return dict(previous_phase_feedback=dict(compared_pairs=pairs, matches=matches, mismatches=pairs-matches),
                updates=len(rows), closed_before=sum(not r['GATEBEFORE'] for r in rows),
                closed_after=sum(not r['GATEAFTER'] for r in rows),
                opened_on_update=sum(not r['GATEBEFORE'] and r['GATEAFTER'] for r in rows),
                closed_on_update=sum(r['GATEBEFORE'] and not r['GATEAFTER'] for r in rows),
                feedback_differs_from_committed_phase=sum(
                    r['TEACHERDISABLED'] == r['COMMITTEDTEACHERENABLED'] for r in rows), events=rows)


def analyze(folder, receipt_path):
    receipt = json.loads(receipt_path.read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and receipt['point'] == folder.name
    verify(folder, receipt['files'])
    roles = {}
    with sqlite3.connect((folder / 'simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        for role in ['vbop', 'pbop']:
            table = role + 'IssueGate'
            assert {r['name'] for r in db.execute('PRAGMA table_info(' + table + ')')} == FIELDS | {'ID', 'STAMP', 'SITE'}
            rows = [dict(row) for row in db.execute('SELECT * FROM ' + table + ' ORDER BY ID')]
            roles[role.upper()] = audit_rows(rows)
    native_frames = []
    for stamp, block in sorted(read_blocks(folder / 'simulator_err.txt').items()):
        generators = {}
        for role in ['vbop', 'pbop']:
            counts = {}
            for suffix in ['scoreTable.total_learn_phase', 'scoreTable.total_bop_disable', 'bop_phase_auto_disable']:
                values = [v for name, values in block.items() if name.endswith('.' + role + '.' + suffix) for v in values]
                value, = values
                counts[suffix] = value
            generators[role.upper()] = counts
        native_frames.append(dict(native_print_stamp=stamp, generators=generators))
    return dict(passed=True, point=folder.name, source=receipt['source'], roles=roles,
                native_frames=native_frames, receipt_sha256=digest(receipt_path),
                native_log_sha256=digest(folder / 'simulator_err.txt'), tool_sha256=digest(Path(__file__)),
                scope='Each row is an actual phaseEndPulse confidence update. The first recorded confidence may '
                      'be nonzero after warmup filtering. STAMP is the writer local counter, not ROB TEACYCLE '
                      'or native print time; no offset, native-window assignment or causal saved cycles inferred. '
                      'Adjacent row IDs must be contiguous even when confidence is saturated or zero. '
                      'Previous-phase feedback compares adjacent committed qualities; the first row has no prior observation. '
                      'Native score-table idle counts remain separate from pulse counts. Snapshot consistency '
                      'does not prove observation neutrality, request-level attribution or full-suite coverage.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    result = analyze(args.folder.resolve(), args.receipt.resolve())
    write(args.output, result)
    print(json.dumps({role: {k: v for k, v in data.items() if k != 'events'}
                      for role, data in result['roles'].items()}, indent=2))


if __name__ == '__main__':
    main()
