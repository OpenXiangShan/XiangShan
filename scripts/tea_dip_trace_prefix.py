"""Locate the first recorded disagreement in each of the six closed TEA/DIP tables.

An equal recorded prefix does not prove equal microarchitectural execution. Sparse
samples and delayed finals locate observations, not the first causal divergence.
"""
import argparse
from itertools import zip_longest
import json
from pathlib import Path
import sqlite3

from native_perf import read_blocks
from tea_dip_query_endpoint_evidence import TABLES, digest, verify
from tea_dip_wb_protocol_micro import write


def analyze(control, candidate):
    folders = [control, candidate]
    provenance, warmup_ends = [], []
    for folder in folders:
        receipt_path = folder / 'closure-receipt.json'
        receipt = json.loads(receipt_path.read_text())
        assert receipt['ready'] and not receipt['original_process_live'] and receipt['point'] == folder.name
        verify(folder, receipt['files'])
        blocks = read_blocks(folder / 'simulator_err.txt')
        assert len(blocks) == 2
        warmup_ends.append(min(blocks) + 1)
        provenance.append(dict(source=receipt['source'], receipt_sha256=digest(receipt_path)))
    commands = [json.loads((folder / 'command.json').read_text()) for folder in folders]
    for flag in ['-W', '-I', '-C', '-s', '-i', '--diff', '--cst-file', '--dump-select-db']:
        assert commands[0][commands[0].index(flag) + 1] == commands[1][commands[1].index(flag) + 1], flag
    tables = {}
    with sqlite3.connect((control / 'simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as left, \
            sqlite3.connect((candidate / 'simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as right:
        for table in TABLES:
            columns = [[row[1] for row in db.execute('PRAGMA table_info(' + table + ')')] for db in [left, right]]
            assert columns[0] == columns[1] and 'STAMP' in columns[0]
            names = columns[0]
            counts = [db.execute('SELECT count(*) FROM ' + table).fetchone()[0] for db in [left, right]]
            assert all(counts), table
            cursors = [db.execute('SELECT * FROM ' + table + ' ORDER BY ID') for db in [left, right]]
            prefix, previous_stamp, mismatch = 0, None, None
            for a, b in zip_longest(*cursors):
                if a != b:
                    records = [dict(zip(names, row)) if row is not None else None for row in [a, b]]
                    stamps = [row['STAMP'] if row is not None else None for row in records]
                    mismatch = dict(record_ordinal=prefix + 1, stamps=stamps,
                                    observed_during_warmup=[stamp < end if stamp is not None else None
                                                           for stamp, end in zip(stamps, warmup_ends)],
                                    differences={name: [row[name] if row is not None else None for row in records]
                                                 for name in names if (records[0] or {}).get(name) != (records[1] or {}).get(name)})
                    break
                prefix += 1
                previous_stamp = a[names.index('STAMP')]
            tables[table] = dict(rows=counts, equal_prefix_rows=prefix,
                                 previous_equal_record_stamp=previous_stamp, first_difference=mismatch,
                                 all_rows_equal=mismatch is None)
    return dict(point=control.name, provenance=provenance, native_ROI_begin=warmup_ends, tables=tables,
                all_six_tables_equal=all(item['all_rows_equal'] for item in tables.values()),
                tool_sha256=digest(Path(__file__)),
                scope='Complete recorded rows ordered by ID, including unmatched tails; STAMP is the record time. '
                      'A previous equal row is not a lower bound on physical divergence. Delayed finals record '
                      'later attribution outcomes; an earlier recorded difference does not identify its causal event. '
                      'Native warmup boundaries classify record time only. No live database is read.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--control', type=Path, required=True)
    parser.add_argument('--candidate', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    result = analyze(args.control.resolve(), args.candidate.resolve())
    write(args.output, result)
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
