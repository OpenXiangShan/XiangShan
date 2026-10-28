"""Offline completed-run coverage/cost metrics; never replay or grant HIT.

Native final counts do not contain first-hit times. The curve therefore changes
only at a qualified run's completion, and cannot describe growth within a run.
"""
from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path

try:
    from .run_random_campaign import audit_native_campaign
    from .funcov_compare import load_artifact
except ImportError:
    from run_random_campaign import audit_native_campaign
    from funcov_compare import load_artifact


def _sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _reference(reference, parent):
    path = Path(reference['path'])
    if not path.is_absolute():
        path = parent / path
    if _sha(path) != reference['sha256']:
        raise ValueError('measurement input hash mismatch: ' + str(path))
    return path, json.loads(path.read_text())


def _positive_integer(value):
    return type(value) is int and value > 0


def build_growth_report(campaign_path: Path, binding_index: Path, index_sha256: str,
                        scope_path: Path, scope_sha256: str, *, cycle_budgets=()):
    """Measure an existing, fully qualified single-family native campaign.

    The caller pins the explicit exploration scope; no canonical denominator is
    edited. Every case stays in cost accounting, including a repeated seed or a
    run with zero additional coverage. Any invalid case rejects the whole report.
    """
    campaign_path, binding_index, scope_path = map(Path, (campaign_path, binding_index, scope_path))
    if not scope_sha256 or _sha(scope_path) != scope_sha256:
        raise ValueError('scope must match explicit SHA256')
    scope = json.loads(scope_path.read_text())
    if not isinstance(scope, dict) or scope.get('schema_version') != 1:
        raise ValueError('invalid exploration scope schema')
    family, bins = scope.get('family'), scope.get('bin_ids')
    if not isinstance(family, str) or not family.strip():
        raise ValueError('scope requires one family')
    if (not isinstance(bins, list) or not bins
            or any(not isinstance(b, str) or not b.strip() for b in bins)
            or len(set(bins)) != len(bins)):
        raise ValueError('scope requires explicit nonempty unique bin IDs')
    scope_bins = set(bins)
    admission = audit_native_campaign(campaign_path, binding_index, index_sha256)
    if admission['evidence']['status'] != 'eligible':
        raise ValueError('native evidence is ineligible: ' + ','.join(admission['evidence']['reasons']))
    campaign = json.loads(campaign_path.read_text())
    index = json.loads(binding_index.read_text())
    rows = {}
    for row in index['artifacts']:
        artifact = Path(row['path'])
        if not artifact.is_absolute():
            artifact = campaign_path.parent / artifact
        rows[str(artifact.resolve())] = row
    seen, full_seen, seed_pairs = set(), set(), set()
    definitions = None
    cumulative_cycles = 0
    curve = []
    references = {str(campaign_path.resolve()): admission['campaign_sha256'],
                  str(binding_index.resolve()): index_sha256,
                  str(scope_path.resolve()): scope_sha256}
    for case in campaign['cases']:  # Preserve the recorded campaign order.
        if case.get('qualification_pass') is not True:
            raise ValueError('original case qualification not passed: ' + case['name'])
        if case.get('spec', {}).get('family') != family:
            raise ValueError('case is outside the single-family scope: ' + case['name'])
        paths = case['functional_coverage_artifacts']
        # v1 refuses to guess whether two reports describe duplicate or split work.
        if len(paths) != 1:
            raise ValueError('growth metrics require exactly one native report per case')
        artifact_path = Path(paths[0])
        if not artifact_path.is_absolute():
            artifact_path = campaign_path.parent / artifact_path
        row = rows[str(artifact_path.resolve())]
        sidecar = Path(row['sidecar_path'])
        if not sidecar.is_absolute():
            sidecar = binding_index.parent / sidecar
        if _sha(sidecar) != row['sidecar_sha256']:
            raise ValueError('native sidecar changed during measurement')
        binding = json.loads(sidecar.read_text())
        timeline_path, timeline = _reference(binding['scenario_timeline'], sidecar.parent)
        frozen_path, _ = _reference(binding['frozen_contract'], sidecar.parent)
        artifact = load_artifact(artifact_path, native_sidecar=sidecar,
                                 native_sidecar_sha256=row['sidecar_sha256'])
        if not artifact.eligible:
            raise ValueError('artifact changed or became ineligible')
        if definitions is None:
            definitions = artifact.definition_bin_ids
        if artifact.definition_bin_ids != definitions or not scope_bins <= definitions:
            raise ValueError('scope/definition mismatch')
        cycles = timeline['details'].get('cycles_run')
        if not _positive_integer(cycles):
            raise ValueError('positive measured timeline cycles_run required: ' + case['name'])
        hits = set(artifact.hit_bin_ids) & scope_bins
        new = hits - seen
        seen |= hits
        full_seen |= artifact.hit_bin_ids
        cumulative_cycles += cycles
        seed_pairs.add(artifact.seed_pair)
        curve.append({'run_id': case['name'], 'program_seed': artifact.program_seed,
                      'timing_seed': artifact.timing_seed, 'cycles_run': cycles,
                      'cumulative_cycles': cumulative_cycles,
                      'observed_scope_bin_ids': sorted(hits), 'new_scope_bin_ids': sorted(new),
                      'new_scope_bins': len(new), 'cumulative_scope_bins': len(seen),
                      'cumulative_scope_bin_ids': sorted(seen),
                      'cumulative_full_definition_bins': len(full_seen),
                      'new_scope_bins_per_million_cycles': len(new) * 1_000_000 / cycles})
        references.update({str(artifact_path.resolve()): _sha(artifact_path),
                           str(sidecar.resolve()): row['sidecar_sha256'],
                           str(timeline_path.resolve()): binding['scenario_timeline']['sha256'],
                           str(frozen_path.resolve()): binding['frozen_contract']['sha256']})
    budgets = list(cycle_budgets)
    if (any(not _positive_integer(b) for b in budgets)
            or budgets != sorted(set(budgets))
            or any(b > cumulative_cycles for b in budgets)):
        raise ValueError('budgets must be increasing positive measured cycle bounds; no extrapolation')
    budget_curve = []
    for budget in budgets:
        complete = [point for point in curve if point['cumulative_cycles'] <= budget]
        budget_curve.append({'budget_cycles': budget, 'completed_runs': len(complete),
                             'accounted_completed_run_cycles': complete[-1]['cumulative_cycles'] if complete else 0,
                             'observed_scope_bins_lower_bound': complete[-1]['cumulative_scope_bins'] if complete else 0,
                             'partial_run_coverage': 'unmeasured'})
    if any(_sha(Path(path)) != pin for path, pin in references.items()):
        raise ValueError('bound inputs changed during measurement')
    return {'schema_version': 1, 'status': 'measured_completed_run_native_counts',
            'canonical_hit_eligible': False, 'original_case_qualification_unchanged': True,
            'scope': scope, 'scope_sha256': scope_sha256,
            'expected_provenance': index['expected_provenance'],
            'full_definition_bins': len(definitions), 'scope_definition_bins': len(scope_bins),
            'run_count': len(curve), 'distinct_seed_pairs': len(seed_pairs),
            'total_measured_cycles': cumulative_cycles, 'scope_union_bin_ids': sorted(seen),
            'full_union_bin_ids': sorted(full_seen),
            'scope_hit_fraction': len(seen) / len(scope_bins),
            'scope_bins_per_million_cycles': len(seen) * 1_000_000 / cumulative_cycles,
            'completed_run_curve': curve, 'cycle_budget_lower_bounds': budget_curve,
            'within_run_curve': {'status': 'unmeasured', 'reason': 'final native hints have no per-bin first-hit cycle'},
            'wall_time_curve': {'status': 'unmeasured', 'reason': 'no bound measured wall duration in this contract'},
            'scenario_combination_curve': {'status': 'unmeasured', 'reason': 'no reviewed semantic combination observations bound'},
            'directed_budget_comparison': {'status': 'unmeasured', 'reason': 'requires compatible directed evidence and measured costs'},
            'limitations': ['No interpolation, first-hit inference, saturation claim or wall-time conversion',
                           'Native admission is not semantic probe review or canonical HIT',
                           'Zero marginal gain does not imply no useful training or no bug-finding value',
                           'Synthetic unit evidence proves tooling only, never real DUT effectiveness'],
            'input_sha256': references}


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--campaign', type=Path, required=True)
    parser.add_argument('--binding-index', type=Path, required=True)
    parser.add_argument('--binding-index-sha256', required=True)
    parser.add_argument('--scope', type=Path, required=True)
    parser.add_argument('--scope-sha256', required=True)
    parser.add_argument('--cycle-budget', type=int, action='append', default=[])
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args(argv)
    report = build_growth_report(args.campaign, args.binding_index, args.binding_index_sha256,
                                 args.scope, args.scope_sha256, cycle_budgets=args.cycle_budget)
    if args.output.resolve() in {Path(path) for path in report['input_sha256']}:
        raise ValueError('output must not overwrite an input')
    with args.output.open('x', encoding='utf-8') as stream:
        json.dump(report, stream, indent=2)
        stream.write('\n')
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
