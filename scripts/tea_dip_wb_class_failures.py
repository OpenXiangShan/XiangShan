"""Report closed FDIV WB candidate correctness failures without inventing an ROI CPI."""
from pathlib import Path
import argparse
import hashlib
import json
import re
import sqlite3
import sys


def digest(path):
    with path.open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', type=Path, required=True)
    args = parser.parse_args(); root = args.report_dir.resolve(); sys.path.insert(0, str(root))
    from native_perf import read_blocks
    evidence = root / 'evidence/fdiv-wb-class'
    source = json.loads((evidence / 'source.json').read_text())
    build = json.loads((evidence / 'build-state.json').read_text())
    spec = json.loads((evidence / 'stage/experiment.json').read_text())
    assert source['head'] == build['source'] == '1f45c4ec4b47c1e35c6b57b0ed2caa3349201c40' and build['phase'] == 'complete'
    failed = root / 'wb-classification/failed'; manifest = json.loads((failed / 'manifest.json').read_text()); rows = []
    for receipt_path in sorted((failed / 'closed-receipts').glob('*.json')):
        receipt = json.loads(receipt_path.read_text()); point = receipt['point']; directory = failed / point
        assert receipt['source'] == source['head'] and receipt['binary_sha256'] == build['binary_sha256']
        assert not receipt['original_process_live'] and receipt['exitcode'] != '0' and not receipt['result_present']
        process = json.loads((directory / 'process.json').read_text()); old = receipt['last_live_observation']
        assert process == receipt['process'] and all(process[k] == old[k] for k in ['pid', 'start_ticks', 'host', 'core']) and old['live']
        for name, entry in receipt['files'].items():
            path = directory / name
            assert entry == manifest[point + '/' + name] and path.stat().st_size == entry['bytes'] and digest(path) == entry['sha256']
        assert (directory / 'exitcode').read_text().strip() == receipt['exitcode'] and not (directory / 'result.json').exists()
        command = json.loads((directory / 'command.json').read_text()); case = next(c for c in spec['cases'] if c['point'] == point)
        for flag, value in [('-s', str(case['seed'])), ('-W', '1000000'), ('-I', '2000000'), ('-C', '100000000'), ('--diff', spec['reference']), ('--dump-select-db', 'Tea_0 TeaDelayedFinal_0 TeaCycleSummary_0 TeaNoCommit_0 Dip_0 DipDelayedFinal_0')]:
            assert command[command.index(flag) + 1] == value
        text = re.sub(r'\x1b\[[0-9;]*m', '', (directory / 'simulator_out.txt').read_text())
        assert 'Difftest enabled' in text and 'EXCEEDING CYCLE/INSTR LIMIT' not in text
        instructions, cycles = [int(v.replace(',', '')) for v in re.search(r'Core-0 instrCnt = ([\d,]+), cycleCnt = ([\d,]+)', text).groups()]
        assert instructions < spec['warmup']
        errors = [dict(register=m[0], reported_pc=m[1], expected=m[2], actual=m[3]) for m in re.findall(r'(\w+) different at pc = (0x[0-9a-fA-F]+), right = (0x[0-9a-fA-F]+), wrong = (0x[0-9a-fA-F]+)', text)]
        assert errors and 'ABORT at pc' in text
        with sqlite3.connect((directory / 'simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as db:
            tables = {name: db.execute('SELECT count(*) FROM ' + name).fetchone()[0] for name in ['Tea_0', 'TeaDelayedFinal_0', 'TeaCycleSummary_0', 'TeaNoCommit_0', 'Dip_0', 'DipDelayedFinal_0']}
        rows.append(dict(point=point,exitcode=receipt['exitcode'],instructions_before_abort=instructions,cycles_before_abort=cycles,difftest_mismatches=errors,trace_table_rows=tables,native_perf_blocks=len(read_blocks(directory/'simulator_err.txt')),intended_roi_available=False,performance_cpi=None,performance_speedup=None,closed_receipt_sha256=digest(receipt_path)))
    result = dict(source=source['head'],failed_closed_points=len(rows),rows=rows,correctness_passed=False if rows else None,consumer_sha256=digest(Path(__file__)),scope='Only closed original failed attempts with preserved argv/source/SHA and Difftest mismatches. Trace-table existence is not correctness or a valid1M ROI; no partial-run CPI, performance gain, causal producer identity or suite score.')
    out = root/'wb-classification/analysis'; out.mkdir(exist_ok=True); (out/'correctness-failures.json').write_text(json.dumps(result,indent=2)+'\n')
    print(json.dumps(result,indent=2))


if __name__ == '__main__':
    main()
