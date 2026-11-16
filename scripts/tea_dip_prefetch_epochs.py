"""Read closed prefetch epochs with the producer's PE width and local clock semantics."""
import argparse
from collections import Counter
from contextlib import closing
from pathlib import Path
import json
import re
import sqlite3

from native_perf import read_blocks
from tea_dip_query_endpoint_evidence import digest, verify

ENGINES = ['Stream', 'Stride', 'Berti', 'SMS', 'VBOP', 'PBOP', 'TP']
ATTRIBUTES = {
    'DELTAMSHRHIT': 'delta_mshrHitSumPBOP',
    'DELTADEMANDCACHEHIT': 'delta_demandCacheHitSumPBOP',
    'DELTAL1PREFETCHCACHEHIT': 'delta_l1PrefetchCacheHitSumPBOP',
    'DELTAPOLLUTIONHOLD': 'delta_pollutionHoldSumPBOP',
    'DELTAPFMSHRHOLD': 'delta_pfMshrHoldSumPBOP',
    'DELTAPFREQBUFFERHOLD': 'delta_pfReqBufferHoldSumPBOP',
    'DELTAPFLATEINMSHR': 'delta_pfLateInMshrSumPBOP',
    'DELTAPFLATEINCACHE': 'delta_pfLateInCacheSumPBOP',
    'DELTAPFUSELESS': 'delta_pfUselessSumPBOP',
    'DELTATNOC': 'delta_tnocSumPBOP',
    'DELTATBUS': 'delta_tbusSumPBOP',
    'DELTATBANK': 'delta_tbankSumPBOP',
}
POSITIVE = ['DELTAMSHRHIT', 'DELTADEMANDCACHEHIT', 'DELTAL1PREFETCHCACHEHIT']
NEGATIVE = ['DELTAPOLLUTIONHOLD', 'DELTAPFMSHRHOLD', 'DELTAPFREQBUFFERHOLD']


def signed_pe(value, bits):
    assert isinstance(value, int) and 0 <= value < 1 << bits, 'PE exceeds actual producer width'
    return value - (1 << bits) if value & (1 << (bits - 1)) else value


def mode2_scores(epochs, attributes, bits, delay):
    """Replay c1 -> c2 -> c3 -> c4 -> PE, including epoch reset priority."""
    updates = Counter()
    for row in attributes:
        updates[row['STAMP'] + delay] += sum(row[k] for k in POSITIVE) - sum(row[k] for k in NEGATIVE)
    sampled = {row['STAMP']: row for row in epochs}
    assert len(sampled) == len(epochs), 'Duplicate epoch timestamp'
    resets = {stamp + 1 for stamp in sampled}
    pe, values = 0, {}
    for stamp in sorted(set(updates) | set(sampled) | resets):
        pe = 0 if stamp in resets else signed_pe((pe + updates[stamp]) % (1 << bits), bits)
        if stamp in sampled:
            values[sampled[stamp]['EPOCHID']] = pe
    return values


def pbop_policy(epochs, values, tlow):
    """Replay the existing mode0/2 active/level update, not a performance prediction."""
    active, level, previous_latency = True, 0, 0
    records = []
    for row in epochs:
        pe = values[row['EPOCHID']]
        latency = row['LATENCYCURR']
        latency_down = (latency < previous_latency and previous_latency - latency > (previous_latency >> tlow)) if tlow < 10 else latency < tlow
        if active:
            if pe > 0:
                level = min(3, level + 1)
            elif pe < 0:
                if level == 0:
                    active, level = False, 3
                else:
                    level -= 1
        elif pe > 0:
            active, level = True, 1
        elif level == 0:
            if latency_down:
                active, level = True, 0
        else:
            level -= 1
        previous_latency = latency
        records.append(dict(epoch_id=row['EPOCHID'], local_stamp=row['STAMP'], pe=pe,
                            latency=latency, post_epoch_active=active, post_epoch_level=level))
    return records


def analyze(folder, receipt_path):
    folder, receipt_path = Path(folder).resolve(), Path(receipt_path).resolve()
    receipt = json.loads(receipt_path.read_text())
    result = json.loads((folder / 'result.json').read_text())
    proof = json.loads((folder / 'prefetch-epoch-source-proof.json').read_text())
    assert receipt['ready'] and not receipt['original_process_live'] and result['original_process_closed']
    assert result['exitcode'] == 0 and result['difftest_passed'] and result['process'] == receipt['process']
    assert receipt['source'] == result['source'] == proof['source']
    assert result['binary_sha256'] == proof['binary_sha256'] and proof['passed']
    assert proof['supported_modes'] == [0, 2] and proof['pe_bits'] == 20 and proof['PBOP_index'] == 5
    assert proof['c1_to_visible_PE_cycles'] == 4
    verify(folder, receipt['files'])
    text = (folder / 'simulator_out.txt').read_text()
    modes = re.findall(r'^l2pf_controlMode0 = (\d+)$', text, re.M)
    assert len(modes) == 1 and int(modes[0]) in proof['supported_modes']
    mode = int(modes[0])
    tlow, = map(int, re.findall(r'^l2pf_tlow0 = (\d+)$', text, re.M))
    with closing(sqlite3.connect((folder / 'simulator.db').as_uri() + '?mode=ro', uri=True)) as db:
        db.row_factory = sqlite3.Row
        epochs = [dict(r) for r in db.execute('SELECT*FROM EpochRecordTable ORDER BY STAMP,ID')]
        attributes = [dict(r) for r in db.execute('SELECT*FROM LatencyAttributeTable_PBOP ORDER BY STAMP,ID')]
    assert epochs and attributes, 'Required epoch/attribute path did not fire'
    assert [r['EPOCHID'] for r in epochs] == list(range(len(epochs))), 'Missing or duplicate epoch'
    assert [r['STAMP'] for r in epochs] == sorted({r['STAMP'] for r in epochs})
    bits = proof['pe_bits']
    values = []
    for row in epochs:
        assert row['STAMP'] >= 0 and row['LATENCYCURR'] >= 0
        values.append({name: signed_pe(row['PE_' + str(i)], bits) for i, name in enumerate(ENGINES)})
    epoch_stamps = [r['STAMP'] for r in epochs]
    prior_epochs, index = 0, 0
    totals = Counter()
    for row in attributes:
        while index < len(epoch_stamps) and epoch_stamps[index] < row['STAMP']:
            prior_epochs += 1
            index += 1
        assert row['EPOCHID'] == prior_epochs, 'Attribute epoch/local timestamp mismatch'
        for field in ATTRIBUTES:
            assert isinstance(row[field], int) and 0 <= row[field] < 1 << bits
            totals[field] += row[field]
    shadow = mode2_scores(epochs, attributes, bits, proof['c1_to_visible_PE_cycles'])
    recorded_pbop = {r['EPOCHID']: v['PBOP'] for r, v in zip(epochs, values)}
    if mode == 0:
        assert all(all(pe == 0 for pe in row.values()) for row in values), 'Mode0 recorded score must remain zero'
    else:
        assert recorded_pbop == shadow, 'PBOP recorded score differs from four-stage c1 replay'
    policy = pbop_policy(epochs, recorded_pbop, tlow)
    blocks = read_blocks(folder / 'simulator_err.txt')
    assert len(blocks) == 2
    def native(name, block):
        entries, = [vs for key, vs in block.items() if key.endswith('.prefetchController.' + name)]
        value, = entries
        return value
    counts = [native('epochCount', block) for block in blocks.values()]
    assert sum(counts) == len(epochs), 'Logged/native epoch total mismatch'
    phases, begin = [], 0
    for (stamp, block), count in zip(blocks.items(), counts):
        chosen = policy[begin:begin + count]
        patterns = Counter((r['post_epoch_active'], r['post_epoch_level']) for r in chosen)
        for active in [False, True]:
            for level in range(4):
                name = f'epoch_parttenPBOP_{int(active)}_{level}'
                assert patterns[active, level] == native(name, block), ('PBOP state/native epoch pattern mismatch', stamp, name)
        phases.append(dict(native_print_stamp=stamp, epochs=count,
                           first_epoch=begin, end_epoch_exclusive=begin + count,
                           PBOP_positive=sum(r['pe'] > 0 for r in chosen), PBOP_negative=sum(r['pe'] < 0 for r in chosen),
                           PBOP_zero=sum(r['pe'] == 0 for r in chosen),
                           post_epoch_active=sum(r['post_epoch_active'] for r in chosen),
                           post_epoch_inactive=sum(not r['post_epoch_active'] for r in chosen),
                           native_overflow_events=native('other_peOverflowCountPBOP', block)))
        begin += count
    native_totals = {field: sum(native(name, block) for block in blocks.values()) for field, name in ATTRIBUTES.items()}
    assert dict(totals) == native_totals, 'Attribute/native full-capture delta sums differ'
    records = [dict(row, recorded_PE=score, PBOP_mode2_shadow=shadow[row['EPOCHID']], PBOP_policy=decision)
               for row, score, decision in zip(epochs, values, policy)]
    return dict(passed=True, source=result['source'], mode=mode, pe_bits=bits,
                local_stamp_domain=proof['stamp_domain'], epoch_rows=len(epochs), PBOP_attribute_rows=len(attributes),
                native_epoch_patterns_checked=16, native_attribute_sums_checked=len(ATTRIBUTES), phases=phases,
                records=records, attribute_delta_totals=dict(totals),
                PBOP_mode2_replay_matches_recorded=mode == 2,
                PBOP_shadow_negative_epochs=sum(pe < 0 for pe in shadow.values()),
                attribute_rows_not_settled_in_last_epoch=sum(r['STAMP'] + 4 > epoch_stamps[-1] for r in attributes),
                input_sha256={p.name: digest(p) for p in [receipt_path, folder / 'result.json', folder / 'simulator.db',
                    folder / 'simulator_out.txt', folder / 'simulator_err.txt', folder / 'prefetch-epoch-source-proof.json']},
                tool_sha256=digest(Path(__file__)),
                scope='Physical20-bit PE signs, same-controller local STAMP and recorded epoch ids; not TEA CYCLE. '
                      'Epoch native phase assignment uses exact logged/native counts and independently checked post-epoch pattern marginals. '
                      'Mode2 PBOP score is replayed through c1/c2/c3/c4 and epoch-reset priority. Mode0 shadow is arithmetic on '
                      'the observed always-on history, not an executed gating strategy or speedup prediction. '
                      'Native delta sums include the unsettled tail; PE scores and modeled latency rewards/costs are not exclusive CPU cycles.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    args.output.write_text(json.dumps(analyze(args.folder, args.receipt), indent=2) + '\n')


if __name__ == '__main__':
    main()
