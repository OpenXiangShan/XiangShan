"""Validate actual load S1 events and native counter marginals in a closed guest.

A non-killed S1 event has fired in this supported generated model, whose ready
binding is proved during relink. Entry cycles and per-uop translation/replay
intervals are different populations; neither is an additive CPU delay budget.
"""
import argparse
from collections import Counter
import csv
import json
from pathlib import Path
import sqlite3

from native_perf import read_blocks
from tea_dip_query_endpoint_evidence import digest, verify

METRICS = ['valid','fire','fire_first_issue','tlb_miss','tlb_miss_first_issue']


def analyze(folder, receipt_path):
    folder = Path(folder).resolve()
    receipt = json.loads(Path(receipt_path).read_text())
    result = json.loads((folder/'result.json').read_text())
    assert receipt['ready'] and not receipt['original_process_live']
    assert result['original_process_closed'] and result['difftest_passed'] and result['exitcode'] == 0
    assert all(result['sampler_flags'].values()) and result['process'] == receipt['process']
    stat = Path('/proc',str(result['process']['pid']),'stat')
    assert not stat.exists() or stat.read_text().rsplit(')',1)[1].split()[19] != str(result['process']['start_ticks'])
    assert receipt['source'] == result['source']
    verify(folder,receipt['files'])
    probe = json.loads((folder/'probe-receipt.json').read_text())
    verify(folder,probe['files'])
    assert probe['closure_receipt_sha256'] == digest(Path(receipt_path))
    proof = json.loads((folder/'probe-build-proof.json').read_text())
    assert proof['passed'] and proof['original_files_unchanged'] and proof['binary_sha256'] == result['binary_sha256']
    assert len(proof['binding']['ready_bindings']) == 3
    blocks = read_blocks(folder/'simulator_err.txt')
    boundaries, events, offsets = [], [], Counter()
    with (folder/'lsu-s1-probe.csv').open() as stream:
        for raw in csv.DictReader(stream):
            row = {key:int(value) for key,value in raw.items()}
            if row['lane'] == 3:
                assert row['clean'] or row['native_dump']
                boundaries.append(row)
                continue
            assert 0 <= row['lane'] < 3 and row['rob_index'] < 160 and row['rob_flag'] in [0,1]
            # Fast replay ORs bit 2 with the retained original entrance.
            assert row['entrance'] in {value|extra for value in [1<<i for i in range(8)] for extra in [0,4]}
            assert row['instr_type'] in [1,2,4]
            assert not(row['tlb_hit'] and row['tlb_miss'])
            assert bool(row['paddr_effective']) == bool(row['tlb_hit'] or row['no_query'])
            assert row['core_cycle'] >= row['issue_cycle'] or row['instr_type'] == 4
            assert not events or (row['native_cycle'],row['lane']) > (events[-1]['native_cycle'],events[-1]['lane'])
            offsets[row['native_cycle']-row['core_cycle']] += 1
            events.append(row)
    assert events and len(offsets) == 1
    offset, = offsets
    with sqlite3.connect((folder/'simulator.db').as_uri()+'?mode=ro',uri=True) as db:
        sql = list(db.execute('SELECT NATIVECYCLE,CLEAN,NATIVEDUMP,LOGENABLE FROM L2TxreqArbTrace WHERE CLEAN OR NATIVEDUMP ORDER BY ID'))
    assert [tuple(row[field]for field in ['native_cycle','clean','native_dump','log_enable'])for row in boundaries] == sql
    assert {row['native_cycle']for row in boundaries if row['native_dump']} == set(blocks)
    phases = []
    for end, block in blocks.items():
        begin = max([row['native_cycle']for row in boundaries if row['clean'] and row['native_cycle'] < end],default=-1)
        counts = [Counter() for _ in range(3)]
        categories = Counter()
        for row in events:
            if not begin < row['native_cycle'] <= end:
                continue
            lane = counts[row['lane']]
            lane['valid'] += 1
            if row['killed']:
                categories['killed'] += 1
                continue
            lane['fire'] += 1
            # ChiselOHEnum.IsOneOf tests bits, including retained fast-replay bits.
            first = bool(row['entrance'] & (32|64))
            lane['fire_first_issue'] += first
            lane['tlb_miss'] += row['tlb_miss']
            lane['tlb_miss_first_issue'] += row['tlb_miss'] and first
            category = 'no_query' if row['no_query'] else 'tlb_hit' if row['tlb_hit'] else 'tlb_miss' if row['tlb_miss'] else 'unknown_translation'
            categories[category] += 1
        marginal = []
        for lane in range(3):
            for metric in METRICS:
                values, = [value for name,value in block.items() if name.endswith(f'.LoadUnit_{lane}.s1.{metric}')]
                expected, = values
                actual = counts[lane][metric]
                assert actual == expected,(end,lane,metric,actual,expected)
                marginal.append(dict(lane=lane,metric=metric,value=actual))
        assert sum(categories.values()) == sum(count['valid']for count in counts)
        phases.append(dict(begin_exclusive=begin,end_inclusive=end,marginal=marginal,translation_categories=categories,events=sum(count['valid']for count in counts)))
    return dict(passed=True,core_to_native_offset=offset,events=len(events),phases=phases,
                native_marginals_equal=True,boundary_rows_equal_SQL=True,
                scope='Per-lane S1 input-valid, non-killed fire and first/miss native marginals over (last-clean,dump], including dump cycle as printed by XSPerfAccumulate. Translation categories partition valid S1 entry cycles; noQuery is not a TLB hit. Supported generated model ready=true and next payload commitment are asserted in C++; same-uop cohort attribution requires separate TEA/final/WB lifetime matching. Not a CPU lost-cycle budget.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder',type=Path,required=True)
    args = parser.parse_args()
    print(json.dumps(analyze(args.folder,args.folder/'closure-receipt.json'),indent=2))


if __name__ == '__main__':
    main()
