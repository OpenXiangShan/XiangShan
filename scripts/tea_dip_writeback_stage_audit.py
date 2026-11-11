"""Audit producer timestamps and compressed-ROB identity in a closed InstTable0 trace."""
import argparse
import hashlib
import json
from pathlib import Path
import sqlite3


def analyze(database):
    database = database.resolve()
    mask = (1 << 64) - 1
    invalid = []
    counts = dict(writebacks=0, representative_writebacks=0, other_member_writebacks=0,
                  invalid_producer_timestamps=0, invalid_representative_timestamps=0,
                  zero_debug_seqnum_writebacks=0, nonzero_member_rank_writebacks=0,
                  different_ROB_generation_writebacks=0)
    members_by_fu = {}
    with sqlite3.connect(database.as_uri() + '?mode=ro', uri=True) as connection:
        connection.row_factory = sqlite3.Row
        rows = connection.execute(
            'SELECT ID, STAMP, ROBIDX, WBSEQNUM, SLOTSEQNUM, SLOTFUTYPE, SLOTISFORMER, '
            'SLOTDISPATCHTIME, WBTEARANK, SLOTTEARANK, WBROBFLAG, SLOTROBFLAG, '
            'WRITEBACKTIME, ISSUETIME, DISPATCHLATENCY, ENQRSLATENCY, '
            'SELECTLATENCY, ISSUELATENCY FROM InstTable0 ORDER BY ID')
        for row in rows:
            counts['writebacks'] += 1
            same_generation = row['WBROBFLAG'] == row['SLOTROBFLAG']
            same = same_generation and row['WBTEARANK'] == row['SLOTTEARANK']
            counts['zero_debug_seqnum_writebacks'] += row['WBSEQNUM'] == 0
            counts['nonzero_member_rank_writebacks'] += row['WBTEARANK'] != 0
            counts['different_ROB_generation_writebacks'] += not same_generation
            counts['representative_writebacks' if same else 'other_member_writebacks'] += 1
            if not same:
                fu = str(row['SLOTFUTYPE'])
                members_by_fu[fu] = members_by_fu.get(fu, 0) + 1
            issue = row['ISSUETIME'] & mask
            select = (issue - row['ISSUELATENCY']) & mask
            enqueue = (select - row['SELECTLATENCY']) & mask
            dispatch = (enqueue - row['ENQRSLATENCY']) & mask
            rename = (dispatch - row['DISPATCHLATENCY']) & mask
            times = [rename, dispatch, enqueue, select, issue, row['WRITEBACKTIME'] & mask]
            ordered = all(a <= b for a, b in zip(times, times[1:])) and all(times)
            representative_valid = ordered and dispatch == (row['SLOTDISPATCHTIME'] & mask)
            counts['invalid_producer_timestamps'] += not ordered
            counts['invalid_representative_timestamps'] += same and not representative_valid
            if (not ordered or (same and not representative_valid)) and len(invalid) < 8:
                invalid.append(dict(row=dict(row), reconstructed_stage_times=times,
                                    representative=same, producer_ordered=ordered))
    return dict(database=str(database), database_sha256=hashlib.sha256(database.read_bytes()).hexdigest(),
                counts=counts, other_member_writebacks_by_representative_FU=members_by_fu,
                first_invalid_examples=invalid,
                producer_ordering_passed=counts['writebacks'] > 0 and not counts['invalid_producer_timestamps'],
                representative_identity_and_ordering_passed=(counts['representative_writebacks'] > 0
                and not counts['invalid_representative_timestamps'])
                if counts['nonzero_member_rank_writebacks'] else None,
                scope='All recorded writebacks, including warmup and speculative events. Other members must '
                      'not overwrite the representative lifetime. Identity uses ROB generation/half/member rank; '
                      'debug_seqNum may be inactive. Nonzero member-rank coverage is required for a verdict. '
                      'This checks metadata identity/order, not '
                      'retirement coverage, CPU critical delay or recoverable cycles.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('database', type=Path, help='Collected, closed simulator.db with identity fields')
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    body = json.dumps(analyze(args.database), indent=2) + '\n'
    if args.output:
        args.output.write_text(body)
    else:
        print(body, end='')


if __name__ == '__main__':
    main()
