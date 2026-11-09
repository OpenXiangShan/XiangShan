"""Analyze one closed point from the registered FDIV reboot recovery."""
import argparse
import json
from pathlib import Path
import sys

from native_perf import read_blocks
from tea_dip_fdiv_predict_counts import FIELDS, predict_counts
from tea_dip_fdiv_recovery_collect import metadata
from tea_dip_query_endpoint_evidence import digest, old_execution, pair, verify
from tea_dip_wb_protocol_micro import write


def analyze(evidence, case_name, report):
    params = metadata(evidence, case_name)
    stage = evidence / 'closed-points' / case_name
    receipt_path = stage / 'closure-receipt.json'
    if not receipt_path.exists():
        return dict(pending=True, case_name=case_name, database_read=False)
    receipt = json.loads(receipt_path.read_text())
    assert receipt['ready'] and not receipt['original_process_live']
    assert receipt['case_name'] == case_name and receipt['spec_sha256'] == params['launch']['spec_sha256']
    assert receipt['role'] == params['role'] and receipt['launch_boot_id'] == params['launch']['process']['boot_id']
    verify(stage, receipt['files'])
    assert digest(stage / 'experiment.json') == params['launch']['spec_sha256']
    assert digest(stage / 'run.py') == params['launch']['runner_sha256']
    measured, control = stage / 'measured', stage / 'control'
    actual = json.loads((measured / 'result.json').read_text())
    build = params['spec']['builds'][params['role']]
    assert actual['source'] == build['source'] and actual['binary_sha256'] == build['binary_sha256']
    assert actual['process'] == receipt['process'] and actual['original_process_closed']
    for directory in [control, measured]:
        result = json.loads((directory / 'result.json').read_text())
        assert result['exitcode'] == 0 and result['difftest_passed'] and all(result['sampler_flags'].values())
        assert result['original_process_closed'] and int((directory / 'exitcode').read_text()) == 0
    result = dict(pending=False, case_name=case_name, role=params['role'], point=params['case']['point'],
                  closure_receipt_sha256=digest(receipt_path), consumer_sha256=digest(Path(__file__)))
    if params['role'] == 'observer':
        assert json.loads((control / 'result.json').read_text())['source'] == 'ec964c8a623c8f72d32970cf984a8c8be8708d03'
        blocks = read_blocks(measured / 'simulator_err.txt')
        names = {name for block in blocks.values() for name in block if name.rsplit('.', 1)[-1] in FIELDS}
        assert len(names) == 16
        equality = old_execution(control, measured, names)
        assert equality['passed'], equality
        phases = [dict(native_print_stamp=stamp, modules=predict_counts(block))
                  for stamp, block in sorted(blocks.items())]
        assert len(phases) == 2
        count = sum(module['counts']['context_mask6_cycles'] for phase in phases for module in phase['modules'])
        gate = json.loads((stage / 'context-opportunity.json').read_text())
        assert not gate['pending'] and gate['old_native_six_tables_neutral']
        assert gate['context_mask6_cycles'] == count and gate['supported'] == (count > 0)
        result.update(observation_neutrality=equality, phases=phases, context_mask6_cycles=count,
                      supported=count > 0, performance_speedup=None,
                      scope='Necessary same-window context only; not a late-FSM, dynamic criticality or saved-cycle proof.')
    else:
        assert json.loads((control / 'result.json').read_text())['source'] == '8f8ece60ac8b08b358743bc9db15a5a227dde0b8'
        gate = json.loads((stage / 'context-opportunity.json').read_text())
        assert not gate['pending'] and gate['supported'] and gate['old_native_six_tables_neutral']
        sys.path.insert(0, str(report))
        from analyze_windows import extract
        from tea_dip_sampling_concordance import concordance
        comparison = pair(control, measured)
        audits = [extract(directory, variant)[0] for directory, variant in [(control, 'control'), (measured, 'candidate')]]
        for audit in audits:
            audit['point'] = params['case']['point']
        result.update(comparison=comparison, sampling_concordance=concordance(audits, comparison),
                      all_native_six_tables=old_execution(control, measured, set()),
                      scope='One original paired 1M+1M recovery point. Exact budgets, sampled attribution and native changes separate; no suite or physical timing claim.')
    result.update(database_read=True, validation_passed=True)
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--evidence-dir', type=Path, required=True)
    parser.add_argument('--case', required=True)
    parser.add_argument('--report-dir', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    value = analyze(args.evidence_dir.resolve(), args.case, args.report_dir.resolve())
    write(args.output, value)
    print(json.dumps({k: v for k, v in value.items() if k in ['pending', 'case_name', 'validation_passed', 'context_mask6_cycles', 'supported', 'scope']}, indent=2))


if __name__ == '__main__':
    main()
