"""Read VBOP entry-attributed TLB events separately from shared response observations.

The existing reqFilter.tlb_miss counter sees a broadcast response port. Entry
first-replay and drop counters are the two partitions of its tracked miss event.
Epoch counts are not a request-completion cohort or a unique-VPN miss rate.
"""
from pathlib import Path
import argparse
import hashlib
import json

from native_perf import read_blocks, unique_counters
from tea_dip_query_endpoint_evidence import verify


def counters(values):
    def scalar(name):
        value, = [v for key, v in values.items()
                  if key.endswith('.prefetcher.vbop.reqFilter.' + name)]
        return value
    first, drop = scalar('entry_miss_first_replay'), scalar('entry_miss_drop')
    return dict(request_valid_cycles=scalar('tlb_req'),
                shared_response_miss_observations=scalar('tlb_miss'),
                entry_first_replay_miss_events=first, entry_drop_miss_events=drop,
                entry_attributed_miss_events=first + drop,
                shared_response_exception_observations=scalar('tlb_excp'),
                entry_exception_drop_events=scalar('entry_excp'))


def analyze(folder, receipt_path):
    folder, receipt_path = folder.resolve(), receipt_path.resolve()
    receipt = json.loads(receipt_path.read_text())
    result = json.loads((folder / 'result.json').read_text())
    assert receipt['ready'] and not receipt['original_process_live']
    assert result['original_process_closed'] and result['difftest_passed'] and result['exitcode'] == 0
    assert receipt['process'] == result['process'] and receipt['source'] == result['source']
    verify(folder, receipt['files'])
    phases = [dict(native_dump_cycle=cycle, **counters(unique_counters(frame)))
              for cycle, frame in sorted(read_blocks(folder / 'simulator_err.txt').items())]
    assert phases
    paths = [folder / 'simulator_err.txt', receipt_path, Path(__file__).resolve()]
    return dict(passed=True, point=folder.name, source=result['source'], phases=phases,
                source_sha256={str(p): hashlib.sha256(p.read_bytes()).hexdigest() for p in paths},
                scope='DefaultConfig VBOP filter observations. tlb_req counts request-valid cycles; '
                      'tlb_miss/tlb_excp see the shared response broadcast and are not VBOP-owned '
                      'misses/exceptions. Entry-attributed miss events sum the mutually exclusive '
                      's3 first-replay/drop PopCount counters. They are neither unique misses nor '
                      'a completed cohort of same-epoch requests; pipeline/epoch boundaries can '
                      'make attributed events exceed requests in one interval. No miss rate, '
                      'CPU-cycle loss, PTW causality or architecture gain is inferred.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', required=True, type=Path)
    parser.add_argument('--receipt', required=True, type=Path)
    parser.add_argument('--output', required=True, type=Path)
    args = parser.parse_args()
    data = analyze(args.folder, args.receipt)
    args.output.write_text(json.dumps(data, indent=2) + '\n')
    print(json.dumps(dict(passed=True, point=data['point'], phases=data['phases'])))


if __name__ == '__main__':
    main()
