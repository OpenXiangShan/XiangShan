"""Supplement closed turnover CI analyses with explicit schema and endpoint budgets."""
from pathlib import Path
from datetime import datetime, timezone
import argparse
import json
import sys

from native_perf import read_blocks
from tea_dip_query_endpoint_evidence import STATES, digest, endpoint, old_execution, verify
from tea_dip_turnover_ci_point import NAMESPACE, RUN, SOURCE, context

SCHEMA = 'evidence/fdiv-turnover-ci-analysis-20261108/formal-schema-replay.json'
SCHEMA_SHA256 = 'd55de671c15b9babd3d27d6e12da06dcb7cc89891935898ef18ab581402b523a'


def declared_names(data, reference):
    phases = data['native_phase_differences']
    assert len(phases) == len(reference['native_phase_differences']) == 2
    for phase, registered in zip(phases, reference['native_phase_differences']):
        assert phase['added_names'] == registered['added_names']
        assert not phase['removed_names']
    names = set(phases[0]['added_names'])
    assert len(names) == 362 and all('.diag_' in name or '.tea_exact_endpoint_' in name for name in names)
    return names


def analyze(root, point):
    partial = root / NAMESPACE / 'partial-analysis' / (point + '.json')
    receipt_path = root / NAMESPACE / 'raw/closed-receipts' / (point + '.json')
    if not partial.exists() or not receipt_path.exists():
        return dict(pending=True, point=point, database_read=False)
    data = json.loads(partial.read_text())
    receipt = json.loads(receipt_path.read_text())
    assert not data['pending'] and data['run_id'] == receipt['run_id'] == RUN
    assert data['source'] == receipt['source'] == SOURCE and data['comparison']['point'] == receipt['point'] == point
    assert not receipt['original_process_live'] and receipt['runner_success_lines']
    assert data['closed_receipt_sha256'] == digest(receipt_path)
    identity, baseline, rows, manifest = context(root)
    assert receipt['artifact_sha256'] == dict(emu=identity['emu_sha256'], reference=identity['reference_sha256'], cst=identity['cst_sha256'])
    assert data['baseline_manifest_sha256'] == digest(baseline / 'analysis/manifest.json')
    after = root / NAMESPACE / 'raw/results' / point
    before = baseline / 'raw/results' / point
    verify(after, receipt['files'])
    verify(baseline, {name: manifest['files'][name] for name in ['raw/results/' + point + '/' + file for file in receipt['files']]})
    schema_path = root / SCHEMA
    assert digest(schema_path) == SCHEMA_SHA256
    names = declared_names(data, json.loads(schema_path.read_text()))
    equality = old_execution(before, after, names)
    window = endpoint(after)
    sampled = {state: data['candidate']['metrics']['TEA_' + state] for state in STATES}
    budget = window['roi']['state_budget_per_native_reported_retired']
    bias = {state: sampled[state] - budget[state] for state in STATES} if budget is not None else None
    sys.path.insert(0, str(root))
    from analyze_probe_evidence_v2 import phase_counters
    phases = []
    for label, (stamp, block) in zip(['warmup', 'roi'], sorted(read_blocks(after / 'simulator_err.txt').items())):
        probes = phase_counters(block)
        phases.append(dict(phase=label, native_print_stamp=stamp, gate_histograms=probes['gate_histograms'], fp_issue_stages=probes['fp_issue_stages']))
    return dict(utc=datetime.now(timezone.utc).isoformat(), pending=False, database_read=True,
                point=point, run_id=RUN, source=SOURCE,
                source_sha256={str(p.relative_to(root)): digest(p) for p in [partial, receipt_path, schema_path]},
                consumer_sha256=digest(Path(__file__)),
                helper_sha256={name: digest(Path(__file__).with_name(name)) for name in ['tea_dip_query_endpoint_evidence.py', 'tea_dip_turnover_ci_point.py', 'native_perf.py']},
                phase_helper_sha256=digest(root / 'analyze_probe_evidence_v2.py'),
                raw_observation_equality=data['complete_observation_equality'],
                explicit_declared_schema_observation_equality=equality,
                candidate_endpoint=window, candidate_sampled_state_cpi=sampled,
                sampled_minus_endpoint_cpi=bias, candidate_native_phases=phases,
                scope='Supplementary closed-point audit; original partial analysis and raw schema result are unchanged. Exactly362 declared names match the fixed registered schema before explicit exclusion. Endpoint normalization uses native retirement; event phase and prefix identity remain unverified. Baseline841 lacks matching endpoints, so original state-delta bounds remain authoritative. Gate and entry-cycle counters overlap and do not estimate saved cycles. No partial-suite score or adoption claim.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', required=True, type=Path)
    parser.add_argument('point')
    args = parser.parse_args()
    root = args.report_dir.resolve()
    target = root / NAMESPACE / 'partial-diagnostics' / (args.point + '.json')
    if target.exists():
        print(json.dumps(dict(preserved=True, evidence=str(target))))
        return
    result = analyze(root, args.point)
    if not result['pending']:
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps({key: result[key] for key in ['pending', 'point', 'database_read']}, indent=2))


if __name__ == '__main__':
    main()
