"""Audit requested table presence in a verified closed TEA/DIP capture."""
import argparse
import json
from pathlib import Path
import sqlite3

from tea_dip_query_endpoint_evidence import digest, verify


def analyze(folder, receipt_path):
    folder, receipt_path = Path(folder).resolve(), Path(receipt_path).resolve()
    receipt = json.loads(receipt_path.read_text())
    assert receipt['ready'] and not receipt['original_process_live'], 'A closed guest receipt is required'
    verify(folder, receipt['files'])
    command = json.loads((folder / 'command.json').read_text())
    requested = sorted(set(command[command.index('--dump-select-db') + 1].split()))
    with sqlite3.connect((folder / 'simulator.db').as_uri() + '?mode=ro', uri=True) as database:
        actual = sorted(row[0] for row in database.execute("SELECT name FROM sqlite_master WHERE type='table'"))
    present = sorted(set(requested) & set(actual))
    missing = sorted(set(requested) - set(actual))
    return dict(input_verified=True, source=receipt['source'], point=receipt['point'],
                requested_tables=requested, actual_tables=actual, present_requested_tables=present,
                missing_requested_tables=missing, all_requested_tables_present=not missing,
                receipt_sha256=digest(receipt_path), tool_sha256=digest(Path(__file__)),
                scope='Verified naturally closed capture and exact table names selected in the actual command. '
                      'Missing means unavailable, not zero events or an inferred producer/configuration cause. '
                      'Table presence does not prove event semantics, complete phase coverage or native marginals. '
                      'Missing optional tables do not invalidate independently checked native CPI or TEA/DIP.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    result = analyze(args.folder, args.receipt)
    args.output.write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(dict(input_verified=result['input_verified'], point=result['point'],
                         all_requested_tables_present=result['all_requested_tables_present'],
                         missing_requested_tables=result['missing_requested_tables'])))


if __name__ == '__main__':
    main()
