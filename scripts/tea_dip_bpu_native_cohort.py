"""Partition committed BPU mispredict counters of a closed turnover CI point."""
from pathlib import Path
from datetime import datetime, timezone
import argparse
import json
from native_perf import read_blocks, unique_counters
from tea_dip_query_endpoint_evidence import digest, verify
from tea_dip_turnover_ci_point import NAMESPACE, RUN, SOURCE, context

FTQ = 'SimTop.cpu.l_soc.core_with_l2.core.frontend.inner.ftq.'


def cohorts(counters):
    def value(name):
        return counters[FTQ + name]
    def group(name):
        prefix = FTQ + name + '_'
        result = {key[len(prefix):]: count for key, count in counters.items() if key.startswith(prefix)}
        assert result
        return result
    total = value('commit_branch_mispredicts')
    partitions = {name: group('commit_branch_mispredicts_' + name) for name in ['reason', 's1_source', 's3_source']}
    assert all(sum(counts.values()) == total for counts in partitions.values())
    types = group('commit_branch_mispredicts_type')
    assert sum(types[name] for name in ['conditional', 'direct', 'indirect']) == total
    no_override = group('commit_branch_mispredicts_s1_mispred_s1_source')
    assert no_override.keys() == partitions['s1_source'].keys()
    with_override = {name: count - no_override[name] for name, count in partitions['s1_source'].items()}
    assert all(count >= 0 for count in with_override.values())
    conditional_reasons = group('commit_conditional_branch_mispredicts_reason')
    assert sum(conditional_reasons.values()) == types['conditional']
    branch_types = group('commit_branch_type')
    assert sum(branch_types[name] for name in ['conditional', 'direct', 'indirect']) == value('commit_branch_num')
    return dict(committed_branches=value('commit_branch_num'), committed_mispredict_entries=total,
                partitions=partitions, mispredict_types=types, branch_types=branch_types,
                conditional_mispredict_reasons=conditional_reasons,
                no_later_override_by_s1_source=no_override, with_s2_or_s3_override_by_s1_source=with_override)


def analyze(root, point):
    partial = root/NAMESPACE/'partial-analysis'/(point + '.json')
    receipt_path = root/NAMESPACE/'raw/closed-receipts'/(point + '.json')
    if not partial.exists() or not receipt_path.exists():
        return dict(pending=True, database_read=False, point=point)
    original = json.loads(partial.read_text())
    receipt = json.loads(receipt_path.read_text())
    assert not original['pending'] and original['run_id'] == receipt['run_id'] == RUN
    assert original['source'] == receipt['source'] == SOURCE
    assert original['comparison']['point'] == receipt['point'] == point
    assert not receipt['original_process_live'] and receipt['runner_success_lines']
    assert digest(receipt_path) == original['closed_receipt_sha256']
    _, baseline, _, manifest = context(root)
    assert digest(baseline/'analysis/manifest.json') == original['baseline_manifest_sha256']
    preflight_path = root/'evidence/bpu-commit-cohort-20261108/source-preflight.json'
    preflight = json.loads(preflight_path.read_text())
    assert preflight['all_four_frontend_files_byte_equal']
    for name, sha in preflight['baseline841_sha256'].items():
        assert digest(baseline/'source'/name) == sha == preflight['source_sha256'][name]
    windows = []
    for label, folder in [('control841', baseline/'raw/results'/point), ('candidate8130', root/NAMESPACE/'raw/results'/point)]:
        if label == 'control841':
            verify(baseline, {name: manifest['files'][name] for name in ['raw/results/' + point + '/' + name for name in ['simulator_out.txt', 'simulator_err.txt']]})
        else:
            verify(folder, {name: receipt['files'][name] for name in ['simulator_out.txt', 'simulator_err.txt']})
        phases = [dict(phase=phase, native_print_stamp=stamp, **cohorts(unique_counters(block)))
                  for phase, (stamp, block) in zip(['warmup', 'roi'], sorted(read_blocks(folder/'simulator_err.txt').items()))]
        assert len(phases) == 2
        windows.append(dict(variant=label, phases=phases))
    changes = []
    for a, b in zip(windows[0]['phases'], windows[1]['phases']):
        groups = {name: {key: b['partitions'][name][key] - count for key, count in values.items()}
                  for name, values in a['partitions'].items()}
        delta = b['committed_mispredict_entries'] - a['committed_mispredict_entries']
        assert all(sum(values.values()) == delta for values in groups.values())
        changes.append(dict(phase=a['phase'], delta_committed_branches=b['committed_branches']-a['committed_branches'],
                            delta_committed_mispredict_entries=delta, partition_deltas=groups,
                            with_override_by_s1_delta={key: b['with_s2_or_s3_override_by_s1_source'][key]-count
                                                       for key, count in a['with_s2_or_s3_override_by_s1_source'].items()}))
    return dict(utc=datetime.now(timezone.utc).isoformat(), pending=False, database_read=False, point=point,
                windows=windows, changes=changes, consumer_sha256=digest(Path(__file__)),
                source_sha256={str(p.relative_to(root)): digest(p) for p in [partial, receipt_path, preflight_path]},
                scope='Closed original841/8130 native logs only. Reason, S1-source and S3-source are separate partitions of committed mispredict FTQ entries, not disjoint CPU-cycle budgets. Call/return subtypes overlap direct/indirect. The counter named s1_mispred is gated by no S2/S3 override; its complement does not establish that S1 was correct. S3 source is metadata even when S3 did not override. BlameBpuSource is a metadata classifier, not proof of TAGE capacity failure or unique causality. No cross-partition joint event identity, stage-isolated gain or suite score.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', required=True, type=Path)
    parser.add_argument('point')
    args = parser.parse_args()
    root = args.report_dir.resolve()
    target = root/NAMESPACE/'partial-bpu-audit'/(args.point + '.json')
    if target.exists():
        print(json.dumps(dict(preserved=True, evidence=str(target))))
        return
    result = analyze(root, args.point)
    if not result['pending']:
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps({key: result[key] for key in ['pending', 'database_read', 'point', 'changes'] if key in result}, indent=2))


if __name__ == '__main__':
    main()
