"""Offline campaign admission with explicit coord pins; no replay subprocesses."""
import hashlib
import json
import sys

import pytest

from . import run_random_campaign as runner
from .test_native_funcov_adapter import native, _load


def _write(path, payload):
    path.write_text(json.dumps(payload))
    return hashlib.sha256(path.read_bytes()).hexdigest()


@pytest.fixture
def campaign_binding(native):
    assert _load(native).eligible
    root = native[0]
    artifact = root / 'toffee.funcov.json'
    execution = json.loads(artifact.read_text())['run_metadata']['execution']
    case = {'name': 'case', 'spec': {'seed': 12}, 'timing_seed': 34,
            'functional_coverage_artifacts': [artifact.name],
            'functional_coverage_sha256': [runner._sha256(artifact)],
            'qualification_pass': False}
    for kind, source in [('source', 'asm'), ('bin', 'bin'), ('trace', 'trace')]:
        case[kind + '_sha256'] = execution[source + '_sha256']
    campaign = root / 'campaign.json'
    index = {'schema_version': 1, 'campaign_sha256': _write(campaign, {'cases': [case]}),
             'artifacts': [{'path': artifact.name, 'sidecar_path': 'coord-sidecar.json',
                            'sidecar_sha256': runner._sha256(root / 'coord-sidecar.json')}]}
    provenance = json.loads((root / 'frozen.json').read_text())['provenance']
    index['expected_provenance'] = {key: provenance[key] for key in (
        'compatibility_signature', 'definitions_sha256', 'dut_source_sha',
        'build_manifest_sha256', 'dut_build_sha256', 'dut_python_extension_sha256',
        'generated_rtl_sha256', 'signal_contract_sha256', 'registry_sha256',
        'sampler_sha256', 'verification_env_sha256')}
    binding = root / 'binding-index.json'
    pin = _write(binding, index)
    return root, campaign, binding, pin


def test_native_campaign_binding_admits_evidence_without_changing_original(campaign_binding):
    root, campaign, index, pin = campaign_binding
    original = {p: p.read_bytes() for p in root.iterdir() if p.is_file()}
    report = runner.audit_native_campaign(campaign, index, pin)
    assert report['evidence']['status'] == 'eligible'
    assert report['evidence']['observed_bin_ids'] == ['test-hit']
    assert report['original_case_qualification_unchanged']
    assert not report['canonical_hit_eligible']
    assert all(p.read_bytes() == b for p, b in original.items())
    assert not json.loads(campaign.read_text())['cases'][0]['qualification_pass']


@pytest.mark.parametrize('change', ['missing_pin', 'stale_index', 'stale_campaign', 'missing_entry',
                                    'extra_entry', 'duplicate_entry', 'stale_artifact', 'wrong_case',
                                    'wrong_program_seed', 'wrong_timing_seed', 'wrong_input_hash',
                                    'missing_artifact_hash'])
def test_native_campaign_binding_rejects_mismatch(campaign_binding, change):
    root, campaign, index, pin = campaign_binding
    binding = json.loads(index.read_text())
    cases = json.loads(campaign.read_text())
    if change == 'missing_pin': pin = ''
    elif change == 'stale_index': index.write_text(index.read_text() + ' ')
    elif change == 'stale_campaign': campaign.write_text(campaign.read_text() + ' ')
    elif change == 'missing_entry': binding['artifacts'] = []
    elif change == 'extra_entry': binding['artifacts'].append({'path': 'unexpected', 'sidecar_path': 'x', 'sidecar_sha256': 'x'})
    elif change == 'duplicate_entry': binding['artifacts'] *= 2
    elif change == 'stale_artifact': (root / 'toffee.funcov.json').write_text('{}')
    elif change == 'wrong_case': cases['cases'][0]['name'] = 'other'
    elif change == 'wrong_program_seed': cases['cases'][0]['spec']['seed'] += 1
    elif change == 'wrong_timing_seed': cases['cases'][0]['timing_seed'] += 1
    elif change == 'wrong_input_hash': cases['cases'][0]['bin_sha256'] = 'changed'
    elif change == 'missing_artifact_hash': del cases['cases'][0]['functional_coverage_sha256']
    if change in ('wrong_case', 'wrong_program_seed', 'wrong_timing_seed', 'wrong_input_hash', 'missing_artifact_hash'):
        binding['campaign_sha256'] = _write(campaign, cases)
    if change not in ('missing_pin', 'stale_index', 'stale_campaign', 'stale_artifact'):
        pin = _write(index, binding)
    with pytest.raises(ValueError):
        runner.audit_native_campaign(campaign, index, pin)


@pytest.mark.parametrize('change', ['missing_sidecar', 'changed_sidecar', 'missing_mapping',
                                    'missing_monitor', 'failed_checker'])
def test_native_campaign_incomplete_health_is_ineligible(campaign_binding, change):
    root, campaign, index, pin = campaign_binding
    if change == 'missing_sidecar': (root / 'coord-sidecar.json').unlink()
    elif change == 'changed_sidecar': (root / 'coord-sidecar.json').write_text('{}')
    elif change == 'missing_mapping': (root / 'frozen.json').write_text('{}')
    elif change == 'missing_monitor':
        path = root / 'timeline.json'
        timeline = json.loads(path.read_text())
        del timeline['details']['monitor_errors']
        path.write_text(json.dumps(timeline))
    elif change == 'failed_checker':
        raw = json.loads((root / 'toffee.funcov.json').read_text())
        raw['run_metadata']['checker']['status'] = 'fail'
        digest = _write(root / 'toffee.funcov.json', raw)
        c = json.loads(campaign.read_text());c['cases'][0]['functional_coverage_sha256'] = [digest]
        b = json.loads(index.read_text());b['campaign_sha256'] = _write(campaign, c)
        side = json.loads((root / 'coord-sidecar.json').read_text());side['artifact_sha256'] = digest
        b['artifacts'][0]['sidecar_sha256'] = _write(root / 'coord-sidecar.json', side)
        pin = _write(index, b)
    result = runner.audit_native_campaign(campaign, index, pin)
    assert result['evidence']['status'] == 'ineligible'
    assert not result['evidence']['observed_bin_ids']


def test_campaign_cli_audit_never_finds_nemu_or_runs_pipeline(campaign_binding, monkeypatch):
    root, campaign, index, pin = campaign_binding
    def forbidden(*args, **kwargs):
        raise AssertionError('offline audit attempted replay/generation')
    monkeypatch.setattr(runner, '_find_nemu', forbidden)
    monkeypatch.setattr(runner, '_run_with_timeout', forbidden)
    monkeypatch.setattr(runner, 'write_cases', forbidden)
    output = root / 'native-admission.json'
    monkeypatch.setattr(sys, 'argv', ['run_random_campaign.py', '--mode', 'audit-native',
        '--output-root', str(root), '--native-binding-index', str(index),
        '--native-binding-index-sha256', pin, '--native-audit-output', str(output)])
    assert runner.main() == 0
    assert output.is_file()
    before = output.read_bytes()
    with pytest.raises(SystemExit, match='native audit rejected'):
        runner.main()
    assert output.read_bytes() == before


@pytest.mark.parametrize('field', [
    'compatibility_signature', 'definitions_sha256', 'dut_source_sha',
    'build_manifest_sha256', 'dut_build_sha256', 'dut_python_extension_sha256',
    'generated_rtl_sha256', 'signal_contract_sha256', 'registry_sha256',
    'sampler_sha256', 'verification_env_sha256'])
def test_old_or_different_provenance_cannot_enter_current_campaign(campaign_binding, field):
    root, campaign, index, pin = campaign_binding
    binding = json.loads(index.read_text())
    binding['expected_provenance'][field] = 'c' * len(binding['expected_provenance'][field])
    pin = _write(index, binding)
    result = runner.audit_native_campaign(campaign, index, pin)
    assert result['evidence']['status'] == 'ineligible'
    assert not result['evidence']['observed_bin_ids']
    assert not result['evidence']['eligible_artifact_paths']
    assert any('current provenance mismatch' in reason for reason in result['evidence']['reasons'])


@pytest.mark.parametrize('change', ['missing', 'missing_field', 'unexpected_field', 'bad_hash'])
def test_native_campaign_requires_complete_expected_current_contract(campaign_binding, change):
    root, campaign, index, pin = campaign_binding
    binding = json.loads(index.read_text())
    if change == 'missing': del binding['expected_provenance']
    elif change == 'missing_field': del binding['expected_provenance']['sampler_sha256']
    elif change == 'unexpected_field': binding['expected_provenance']['unknown'] = 'c' * 64
    elif change == 'bad_hash': binding['expected_provenance']['dut_source_sha'] = 'x' * 40
    pin = _write(index, binding)
    with pytest.raises(ValueError, match='expected provenance'):
        runner.audit_native_campaign(campaign, index, pin)
