"""Synthetic offline metrics only: no DUT, NEMU or campaign generation."""
import copy
import json

import pytest

from . import coverage_growth_audit as growth
from .test_native_funcov_adapter import native, _write


@pytest.fixture
def measured(native):
    root, raw, frozen, timeline = native
    cases, rows = [], []
    provenance = frozen['provenance']
    for ordinal, (cycles, bin_name) in enumerate(((100, 'hit'), (300, 'miss'))):
        name = f'run-{ordinal}'
        report = copy.deepcopy(raw)
        report['run_metadata']['run_id'] = name
        report['run_metadata']['execution']['seeds']['test'] += ordinal
        for b in report['groups'][0]['points'][0]['bins']:
            b['hints'] = int(b['name'] == bin_name)
        tl = copy.deepcopy(timeline)
        tl['scenario_id'] = name
        tl['details']['cycles_run'] = cycles
        artifact = root/f'{name}.funcov.json'
        binding = {'schema_version': 1, 'run_id': name,
                   'artifact_sha256': _write(artifact, report)}
        for key, payload in [('frozen_contract', frozen), ('scenario_timeline', tl)]:
            path = root/f'{name}.{key}.json'
            binding[key] = {'path': path.name, 'sha256': _write(path, payload)}
        sidecar = root/f'{name}.binding.json'
        rows.append({'path': artifact.name, 'sidecar_path': sidecar.name,
                     'sidecar_sha256': _write(sidecar, binding)})
        case = {'name': name, 'spec': {'seed': 12+ordinal, 'family': 'ibuffer_pressure_cross'},
                'timing_seed': 34, 'qualification_pass': True,
                'functional_coverage_artifacts': [artifact.name],
                'functional_coverage_sha256': [binding['artifact_sha256']]}
        for kind, source in [('source', 'asm'), ('bin', 'bin'), ('trace', 'trace')]:
            case[kind+'_sha256'] = report['run_metadata']['execution'][source+'_sha256']
        cases.append(case)
    campaign, index, scope = (root/p for p in ('campaign.json', 'index.json', 'scope.json'))
    index_payload = {'schema_version': 1, 'campaign_sha256': _write(campaign, {'cases': cases}),
                     'artifacts': rows,
                     'expected_provenance': {k: provenance[k] for k in (
                         'compatibility_signature', 'definitions_sha256', 'dut_source_sha',
                         'build_manifest_sha256', 'dut_build_sha256', 'dut_python_extension_sha256',
                         'generated_rtl_sha256', 'signal_contract_sha256', 'registry_sha256',
                         'sampler_sha256', 'verification_env_sha256')}}
    _write(index, index_payload)
    _write(scope, {'schema_version': 1, 'family': 'ibuffer_pressure_cross',
                   'bin_ids': ['test-hit', 'test-miss']})
    return root, campaign, index, scope


def _report(measured, **kwargs):
    _, campaign, index, scope = measured
    return growth.build_growth_report(campaign, index, growth._sha(index),
                                      scope, growth._sha(scope), **kwargs)


def _repin(measured):
    root, campaign, index, _ = measured
    payload = json.loads(index.read_text())
    for row in payload['artifacts']:
        artifact, sidecar = root/row['path'], root/row['sidecar_path']
        binding = json.loads(sidecar.read_text())
        binding['artifact_sha256'] = growth._sha(artifact)
        for key in ['frozen_contract', 'scenario_timeline']:
            binding[key]['sha256'] = growth._sha(root/binding[key]['path'])
        row['sidecar_sha256'] = _write(sidecar, binding)
    campaign_payload = json.loads(campaign.read_text())
    for case in campaign_payload['cases']:
        case['functional_coverage_sha256'] = [growth._sha(root/p) for p in case['functional_coverage_artifacts']]
    payload['campaign_sha256'] = _write(campaign, campaign_payload)
    _write(index, payload)


def test_completed_run_curve_uses_actual_cost_and_records_missing_axes(measured):
    root = measured[0]
    before = {p: p.read_bytes() for p in root.iterdir() if p.is_file()}
    report = _report(measured, cycle_budgets=[50, 100, 250, 400])
    assert report['run_count'] == report['distinct_seed_pairs'] == 2
    assert report['total_measured_cycles'] == 400
    assert report['scope_union_bin_ids'] == ['test-hit', 'test-miss']
    assert [p['cumulative_scope_bins'] for p in report['completed_run_curve']] == [1, 2]
    assert [p['observed_scope_bins_lower_bound'] for p in report['cycle_budget_lower_bounds']] == [0, 1, 1, 2]
    assert report['scope_bins_per_million_cycles'] == 5000
    for field in ['within_run_curve', 'wall_time_curve', 'scenario_combination_curve', 'directed_budget_comparison']:
        assert report[field]['status'] == 'unmeasured'
    assert not report['canonical_hit_eligible']
    assert all(p.read_bytes() == content for p, content in before.items())


def test_explicit_scope_does_not_shrink_full_definition_accounting(measured):
    scope = measured[3]
    payload = json.loads(scope.read_text())
    payload['bin_ids'] = ['test-hit']
    _write(scope, payload)
    report = _report(measured)
    assert report['full_definition_bins'] == 2 and report['scope_definition_bins'] == 1
    assert report['full_union_bin_ids'] == ['test-hit', 'test-miss']
    assert report['completed_run_curve'][1]['new_scope_bins'] == 0
    assert report['total_measured_cycles'] == 400


def test_repeated_seed_pair_still_consumes_measured_budget(measured):
    root, campaign, _, _ = measured
    payload = json.loads(campaign.read_text())
    payload['cases'][1]['spec']['seed'] = 12
    _write(campaign, payload)
    artifact = root/'run-1.funcov.json'
    raw = json.loads(artifact.read_text())
    raw['run_metadata']['execution']['seeds']['test'] = 12
    _write(artifact, raw)
    _repin(measured)
    report = _report(measured)
    assert report['distinct_seed_pairs'] == 1 and report['run_count'] == 2
    assert report['total_measured_cycles'] == 400


@pytest.mark.parametrize('cycles', [None, 0, -1, True, 1.2, '100'])
def test_missing_or_invalid_measured_cost_is_rejected(measured, cycles):
    path = measured[0]/'run-0.scenario_timeline.json'
    payload = json.loads(path.read_text())
    payload['details']['cycles_run'] = cycles
    _write(path, payload)
    _repin(measured)
    with pytest.raises(ValueError, match='cycles_run'):
        _report(measured)


@pytest.mark.parametrize('budgets', [[0], [True], [401], [100, 50], [100, 100]])
def test_invalid_or_extrapolated_budgets_are_rejected(measured, budgets):
    with pytest.raises(ValueError, match='budgets'):
        _report(measured, cycle_budgets=budgets)


@pytest.mark.parametrize('change', ['unqualified', 'family', 'unknown_bin', 'duplicate_scope',
                                    'missing_scope_pin', 'stale_timeline', 'stale_sampler',
                                    'failed_checker'])
def test_unqualified_or_unbound_inputs_never_produce_metrics(measured, change):
    root, campaign, index, scope = measured
    if change in ['unqualified', 'family']:
        payload = json.loads(campaign.read_text())
        case = payload['cases'][0]
        if change == 'unqualified': case['qualification_pass'] = False
        else: case['spec']['family'] = 'cfg_layout_mix'
        _write(campaign, payload)
        _repin(measured)
    elif change in ['unknown_bin', 'duplicate_scope']:
        payload = json.loads(scope.read_text())
        payload['bin_ids'] = ['not-a-definition'] if change == 'unknown_bin' else ['test-hit', 'test-hit']
        _write(scope, payload)
    elif change == 'missing_scope_pin':
        with pytest.raises(ValueError, match='scope'):
            growth.build_growth_report(campaign, index, growth._sha(index), scope, '')
        return
    elif change == 'stale_timeline':
        (root/'run-0.scenario_timeline.json').write_text('{}')
    elif change == 'stale_sampler':
        payload = json.loads(index.read_text())
        payload['expected_provenance']['sampler_sha256'] = 'c'*64
        _write(index, payload)
    elif change == 'failed_checker':
        artifact = root/'run-0.funcov.json'
        payload = json.loads(artifact.read_text())
        payload['run_metadata']['checker']['status'] = 'fail'
        _write(artifact, payload)
        _repin(measured)
    with pytest.raises(ValueError):
        _report(measured)


def test_cli_is_offline_exclusive_and_does_not_overwrite(measured):
    root, campaign, index, scope = measured
    output = root/'growth-report.json'
    args = ['--campaign', str(campaign), '--binding-index', str(index),
            '--binding-index-sha256', growth._sha(index), '--scope', str(scope),
            '--scope-sha256', growth._sha(scope), '--output', str(output)]
    assert growth.main(args) == 0
    original = output.read_bytes()
    with pytest.raises(FileExistsError):
        growth.main(args)
    assert output.read_bytes() == original
