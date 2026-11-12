"""Validate L1 readline progress and conflict intersections in a closed guest.

Readline and per-lane conflict conditions are not unique CPU-critical requests.
A downsteam-wait intersection is only an opportunity bound, not saved cycles.
"""
import argparse
from collections import Counter
import csv
import json
from pathlib import Path
import sqlite3
from native_perf import read_blocks
from tea_dip_query_endpoint_evidence import digest, verify


def analyze(folder, receipt_path):
    folder = Path(folder).resolve()
    receipt = json.loads(Path(receipt_path).read_text())
    result = json.loads((folder/'result.json').read_text())
    assert receipt['ready'] and not receipt['original_process_live']
    assert result['original_process_closed'] and result['difftest_passed'] and result['exitcode'] == 0
    assert result['process'] == receipt['process'] and result['source'] == receipt['source']
    assert all(result['sampler_flags'].values())
    verify(folder, receipt['files'])
    probe = json.loads((folder/'probe-receipt.json').read_text())
    assert probe['closure_receipt_sha256'] == digest(Path(receipt_path))
    verify(folder, probe['files'])
    proof = json.loads((folder/'probe-build-proof.json').read_text())
    assert proof['passed'] and proof['original_files_unchanged'] and proof['binary_sha256'] == result['binary_sha256']
    rows = [{key:int(value) for key,value in row.items()} for row in csv.DictReader((folder/'l1-readline-probe.csv').open())]
    assert rows and all(a['native_cycle'] < b['native_cycle'] for a,b in zip(rows,rows[1:]))
    for row in rows:
        for key in ['clean','native_dump','log_enable','s1_valid','need_data','s2_ready','s1_fire','line_valid','line_ready','miss','probe']:
            assert row[key] in [0,1], key
        assert 0 <= row['rrl_mask'] < 8 and 0 <= row['rw_mask'] < 8
        assert row['line_valid'] == bool(row['s1_valid'] and row['need_data'])
        assert row['s1_fire'] == bool(row['s1_valid'] and row['s2_ready'] and (row['line_ready'] or not row['need_data']))
        assert not row['rrl_mask'] or row['line_valid']
    blocks = read_blocks(folder/'simulator_err.txt')
    boundaries = [row for row in rows if row['clean'] or row['native_dump']]
    with sqlite3.connect((folder/'simulator.db').as_uri()+'?mode=ro',uri=True) as db:
        old = list(db.execute('SELECT NATIVECYCLE,CLEAN,NATIVEDUMP,LOGENABLE FROM L2TxreqArbTrace WHERE CLEAN OR NATIVEDUMP ORDER BY ID'))
    assert [tuple(row[key] for key in ['native_cycle','clean','native_dump','log_enable']) for row in boundaries] == old
    assert {row['native_cycle'] for row in boundaries if row['native_dump']} == set(blocks)
    phases = []
    for end, block in blocks.items():
        begin = max([row['native_cycle'] for row in boundaries if row['clean'] and row['native_cycle'] < end], default=-1)
        selected = [row for row in rows if begin < row['native_cycle'] <= end]
        counts = Counter()
        sources = Counter()
        for row in selected:
            valid, ready = row['line_valid'], row['s2_ready']
            counts['readline_cycles'] += valid
            counts['progressing_readline_cycles'] += valid and row['s1_fire']
            counts['downstream_wait_readline_cycles'] += valid and not ready
            counts['array_wait_readline_cycles'] += valid and ready and not row['line_ready']
            counts['downstream_and_array_busy_readline_cycles'] += valid and not ready and not row['line_ready']
            counts['rrl_port_cycles'] += row['rrl_mask'].bit_count()
            counts['rrl_downstream_wait_port_cycles'] += row['rrl_mask'].bit_count() if not ready else 0
            counts['any_rrl_downstream_wait_cycles'] += bool(row['rrl_mask']) and not ready
            counts['rrl_downstream_without_rw_port_cycles'] += (row['rrl_mask'] & ~row['rw_mask'] & 7).bit_count() if not ready else 0
            if valid: sources[(row['source'],row['miss'],row['probe'])] += 1
        assert counts['readline_cycles'] == sum(counts[name] for name in ['progressing_readline_cycles','downstream_wait_readline_cycles','array_wait_readline_cycles'])
        checks = []
        def check(suffix, actual):
            values, = [values for name,values in block.items() if name.endswith('.bankedDataArray.'+suffix)]
            expected, = values
            assert actual == expected, (end,suffix,actual,expected)
            checks.append(dict(name=suffix,value=actual))
        check('data_array_read_line', counts['readline_cycles'])
        for lane in range(3):
            for mask,suffix in [('rrl_mask','data_array_rrl_bank_conflict_'),('rw_mask','data_array_rw_bank_conflict_')]:
                check(suffix+str(lane), sum(bool(row[mask] & (1 << lane)) for row in selected))
        phases.append(dict(begin_exclusive=begin,end_inclusive=end,counts=counts,native_marginals=checks,
            readline_sources=[dict(source=key[0],miss=key[1],probe=key[2],cycles=value) for key,value in sorted(sources.items())]))
    return dict(passed=True,rows=len(rows),phases=phases,native_marginals_equal=True,SQL_boundaries_equal=True,
        scope='Actual native-time L1 readline and rrl/rw port conditions over (last-clean,dump]. All seven existing native marginals and SQL clean/dump boundaries agree. Downstream-ready and array-ready remain separate, including their overlap. RRL/RW masks can overlap with other causes; this is neither a CPU loss partition nor unique demand/ROB-head identity. A positive intersection does not by itself justify a functional gate or predict saved CPI.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder',type=Path,required=True)
    args = parser.parse_args()
    print(json.dumps(analyze(args.folder,args.folder/'closure-receipt.json'),indent=2))

if __name__ == '__main__': main()
