"""Reject native stage-latency sums that cannot represent elapsed time."""
import argparse
import hashlib
import json
from pathlib import Path
import re

from native_perf import read_blocks, unique_counters

def analyze(log):
    pattern = re.compile(r'\.rob\.[^.]+_latency_(dispatch|enq_rs|select|issue|execute|enq_rs_execute|commit)$')
    blocks = []
    for stamp, raw in sorted(read_blocks(log).items()):
        fields = {name: value for name, value in unique_counters(raw).items() if pattern.search(name)}
        negative = {name: value for name, value in fields.items() if value < 0}
        blocks.append(dict(stamp=stamp, observed_stage_fields=len(fields), rejected_elapsed_sums=negative,
                           usable_for_stage_attribution=False if negative else None,
                           status='invalid_elapsed_sums' if negative else 'not_validated' if fields else 'unavailable'))
    result = dict(log=str(log.resolve()), log_sha256=hashlib.sha256(log.read_bytes()).hexdigest(),
                  blocks=blocks,
                  scope='Numerical rejection only. A nonnegative aggregate does not prove timestamp wiring, applicability, ordering or criticality. No CPI or average service latency is inferred.',
                  source_sha256=hashlib.sha256(Path(__file__).read_bytes()).hexdigest())
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('log', type=Path, help='Closed simulator_err.txt')
    parser.add_argument('--output', type=Path)
    args = parser.parse_args()
    body = json.dumps(analyze(args.log), indent=2) + '\n'
    if args.output:
        args.output.write_text(body)
    else:
        print(body, end='')


if __name__ == '__main__':
    main()
