"""Compare closed default CI points across builds/seeds with the same recorded model sources."""
from datetime import datetime, timezone
from pathlib import Path
import argparse
import hashlib
import json
import re

from native_perf import read_blocks
from tea_dip_query_endpoint_evidence import TABLES, old_execution, verify


def digest(path):
    with Path(path).open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def point(folder, receipt_path):
    receipt = json.loads(receipt_path.read_text())
    assert receipt['ready'] and not receipt['original_process_live'], 'Original point must be closed'
    assert folder.name == receipt['point']
    verify(folder, receipt['files'])
    argv = receipt['last_live_process_observation']['args']
    for flag, expected in [('-W', '20000000'), ('-I', '40000000')]:
        assert argv[argv.index(flag) + 1] == expected, flag
    assert set(argv[argv.index('--dump-select-db') + 1].split()) == set(TABLES)
    stdout = (folder / 'simulator_out.txt').read_text()
    assert 'Difftest enabled' in stdout and 'EXCEEDING CYCLE/INSTR LIMIT' in stdout
    assert not any(marker in stdout for marker in ['HIT CRITICAL', 'HIT BAD TRAP', 'ABORT'])
    for name, value in [('enableTea0', 1), ('enableDip0', 1), ('teaSamplePeriod0', 1000), ('dipSamplePeriod0', 1000)]:
        assert re.search(r'^' + name + ' = ' + str(value) + '$', stdout, re.M), name
    seed = int(argv[argv.index('-s') + 1])
    assert int(re.search(r'Using seed = (\d+)', stdout)[1]) == seed
    native = read_blocks(folder / 'simulator_err.txt')
    assert len(native) == 2
    phases = []
    for stamp, block in native.items():
        def counter(suffix):
            values, = [values for name, values in block.items() if name.endswith('.rob.' + suffix)]
            value, = values
            return value
        cycles, retired = counter('clock_cycle'), counter('commitInstr')
        assert cycles > 0 and retired > 0
        phases.append(dict(stamp=stamp, cycles=cycles, retired=retired, CPI=cycles / retired))
    assert phases[0]['cycles'] == phases[0]['stamp'] + 1
    assert phases[1]['cycles'] == phases[1]['stamp'] - phases[0]['stamp']
    terminal = re.search(r'Core-0 instrCnt = ([\d,]+), cycleCnt = ([\d,]+)', stdout)
    assert terminal
    host = re.search(r'Host time spent:\s*([\d,]+)ms', stdout)
    result = dict(run_id=receipt['run_id'], source=receipt['source'], actual_seed=seed,
                  checkpoint=argv[argv.index('-i') + 1], artifact_sha256=receipt['artifact_sha256'],
                  phases=dict(zip(['warmup', 'ROI'], phases)),
                  terminal_counts=[int(value.replace(',', '')) for value in terminal.groups()],
                  printed_host_milliseconds=int(host[1].replace(',', '')) if host else None,
                  closed_receipt_sha256=digest(receipt_path))
    return receipt, result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ['before-dir', 'before-receipt', 'after-dir', 'after-receipt', 'source-equivalence', 'output']:
        parser.add_argument('--' + name, type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists(), 'Keep prior evidence; use a new output path'
    receipts, windows = zip(point(args.before_dir, args.before_receipt), point(args.after_dir, args.after_receipt))
    assert receipts[0]['point'] == receipts[1]['point']
    assert windows[0]['checkpoint'] == windows[1]['checkpoint']
    for artifact in ['reference', 'cst']:
        assert windows[0]['artifact_sha256'][artifact] == windows[1]['artifact_sha256'][artifact], artifact
    sources = json.loads(args.source_equivalence.read_text())['source_trees']
    trees = [sources[window['source']] for window in windows]
    required = {'src', 'build.mill', 'utility', 'difftest', 'XSCache', 'yunsuan', 'rocket-chip'}
    assert required <= trees[0].keys() and trees[0] == trees[1], 'Recorded model sources differ'
    # Receipt/argv/config/source checks all precede SQLite reads.
    comparison = old_execution(args.before_dir, args.after_dir, set())
    assert comparison['tables']['Tea_0']['rows'][0] > 0 and comparison['tables']['Dip_0']['rows'][0] > 0
    before_host, after_host = [window['printed_host_milliseconds'] for window in windows]
    result = dict(utc=datetime.now(timezone.utc).isoformat(), point=receipts[0]['point'],
                  before=windows[0], after=windows[1], recorded_model_sources_equal=True,
                  actual_seeds_equal=windows[0]['actual_seed'] == windows[1]['actual_seed'],
                  complete_native_and_six_table_comparison=comparison,
                  terminal_counts_equal=windows[0]['terminal_counts'] == windows[1]['terminal_counts'],
                  native_phase_delta_CPI={phase: windows[1]['phases'][phase]['CPI'] - windows[0]['phases'][phase]['CPI']
                                          for phase in ['warmup', 'ROI']},
                  current_over_before_host_elapsed=after_host / before_host if before_host and after_host else None,
                  source_equivalence_sha256=digest(args.source_equivalence), consumer_sha256=digest(__file__),
                  scope='Closed default20M+20M CI repetitions only. Source equivalence checks the supplied Git tree hashes; '
                        'it does not rebuild models. Equal observations at these seeds do not establish general seed '
                        'independence or identical dynamic uop prefixes. Host elapsed is whole-run host time, not RTL '
                        'CPI, paired tool overhead or a suite score.')
    args.output.write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
