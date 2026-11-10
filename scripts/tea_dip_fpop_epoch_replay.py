"""Replay original XSCache a041 FPOP epoch inputs against native state marginals.

EpochRecordTable exports signed20-bit PE as UInt. States inferred from complete
inputs and fixed reset semantics are not direct signal observations or CPU times.
"""
import argparse
from collections import Counter
import json
from pathlib import Path
import sqlite3

from native_perf import read_blocks
from tea_dip_fpop_native import ROLES, analyze as native_evidence
from tea_dip_query_endpoint_evidence import digest
from tea_dip_wb_protocol_micro import write


def replay(rows, tlow):
    states = {role: (True, 0) for role in ROLES}
    previous_latency = 0
    records, counts = [], Counter()
    for ordinal, row in enumerate(rows):
        assert row['ID'] == ordinal + 1 and row['EPOCHID'] == ordinal, 'Incomplete initial epoch trace'
        latency = row['LATENCYCURR']
        latency_down = (latency < previous_latency and previous_latency - latency > (previous_latency >> tlow)
                        if tlow < 10 else latency < tlow)
        roles = {}
        for index, role in enumerate(ROLES):
            raw_pe = row['PE_' + str(index)]
            assert 0 <= raw_pe < (1 << 20)
            pe = raw_pe - (1 << 20) if raw_pe >= (1 << 19) else raw_pe
            active, level = states[role]
            before = dict(active=active, level=level)
            reason = 'hold'
            if active:
                if pe > 0:
                    level = min(3, level + 1)
                    reason = 'positive'
                elif pe < 0:
                    if level == 0:
                        active, level, reason = False, 3, 'disable'
                    else:
                        level, reason = level - 1, 'decrease-level'
            elif pe > 0:
                active, level, reason = True, 1, 'prior-hit'
                counts['epoch_pfHitLagActiveCount' + role] += 1
            elif level == 0:
                if latency_down:
                    active, level, reason = True, 0, 'latency-drop'
                    counts['epoch_latencyDownActiveCount' + role] += 1
            else:
                level, reason = level - 1, 'cooldown'
            states[role] = (active, level)
            after = dict(active=active, level=level)
            roles[role] = dict(raw_PE=raw_pe, signed_PE=pe, before=before, after=after, reason=reason)
            counts['epoch_' + ('activeEpochCount' if active else 'inactiveEpochCount') + role] += 1
            counts['epoch_partten' + role + '_' + str(int(active)) + '_' + str(level)] += 1
        records.append(dict(epoch=ordinal, writer_stamp=row['STAMP'], latency=latency,
                            previous_latency=previous_latency, latency_down=latency_down, roles=roles))
        previous_latency = latency
    return records, counts


def analyze(folder, receipt_path):
    native = native_evidence(folder, receipt_path)
    constants = native['runtime_constants']
    assert constants['l2pf_controlMode0'] in [0, 2], 'Only original none/FPOP semantics are supported'
    with sqlite3.connect((folder / 'simulator.db').resolve().as_uri() + '?mode=ro', uri=True) as db:
        db.row_factory = sqlite3.Row
        rows = [dict(row) for row in db.execute('SELECT * FROM EpochRecordTable ORDER BY ID')]
    assert rows, 'Requires a complete selected epoch table'
    records, counts = replay(rows, constants['l2pf_tlow0'])
    blocks = read_blocks(folder / 'simulator_err.txt')
    checks = {}
    for role in ROLES:
        names = ['epoch_activeEpochCount' + role, 'epoch_inactiveEpochCount' + role,
                 'epoch_pfHitLagActiveCount' + role, 'epoch_latencyDownActiveCount' + role]
        names += ['epoch_partten' + role + '_' + str(active) + '_' + str(level)
                  for active in [0, 1] for level in range(4)]
        for name in names:
            observed = 0
            for block in blocks.values():
                values, = [values for key, values in block.items() if key.endswith('.prefetchController.' + name)]
                value, = values
                observed += value
            assert counts[name] == observed, (name, counts[name], observed)
            checks[name] = dict(replayed=counts[name], native=observed)
        assert counts['epoch_activeEpochCount' + role] + counts['epoch_inactiveEpochCount' + role] == len(rows)
    return dict(point=folder.name, source=native['source'], runtime_constants=constants,
                records=records, native_state_marginals=checks,
                receipt_sha256=native['receipt_sha256'], native_log_sha256=native['native_log_sha256'],
                database_sha256=digest(folder / 'simulator.db'), tool_sha256=digest(Path(__file__)),
                scope='Original a041 none/FPOP policy, PE20/degree2/reset active-level(true,0), complete epochs from0. '
                      'Replayed states are inferred, validated against full-window native active/inactive, recovery '
                      'and level marginals; they are not direct per-epoch gate-signal observations. '
                      'Writer STAMP is not used as a CPU cycle or ROI boundary. No request-level cause, '
                      'frozen-input counterfactual speedup or full-suite gain is inferred.')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--folder', type=Path, required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    assert not args.output.exists()
    result = analyze(args.folder.resolve(), args.receipt.resolve())
    write(args.output, result)
    print(json.dumps(dict(point=result['point'], records=len(result['records']),
                          verified_native_state_marginals=len(result['native_state_marginals'])), indent=2))


if __name__ == '__main__':
    main()
