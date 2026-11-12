"""Align core timestamps with the native log endpoint using shared dump snapshots.

GTimer and ChiselDB counters are local to their elaboration clock/reset. Equality
with a table's own STAMP does not establish cross-domain simultaneity. Supported
producers expose either same-dump exact TEA prefixes or clear/dump SQL summaries.
"""
from native_perf import read_blocks, unique_counters
from tea_dip_sql_roi import reconstruct


def core_native_alignment(db, native_path):
    frames = sorted(read_blocks(native_path).items())
    assert frames, 'Native dump snapshots are required to align core timestamps'
    anchors = []
    exact = []
    for stamp, block in frames:
        values = [v for k, v in unique_counters(block).items()
                  if k.endswith('.rob.tea_exact_endpoint_cycle')]
        exact.append((stamp, values))
    if any(values for _, values in exact):
        for stamp, values in exact:
            assert len(values) == 1, 'Missing or ambiguous same-dump core prefix'
            prefix, = values
            # The enabled exact prefix includes the current core cycle.
            anchors.append(dict(native_exclusive_end=stamp + 1, core_exclusive_end=prefix))
        method = 'native_same_dump_exact_TEA_prefix'
    else:
        columns = {r[1] for r in db.execute('PRAGMA table_info(TeaCycleSummary_0)')}
        assert 'CLEARED' in columns, 'No supported same-dump core clock anchor'
        rows = [dict(r) for r in db.execute('SELECT * FROM TeaCycleSummary_0 ORDER BY CYCLE,ID')]
        assert rows and all(not r['CLOSED'] and not r['CLEARED'] and
                            r['CYCLE'] == r['STAMP'] + 1 for r in rows)
        if len(frames) == 2:
            cycles = []
            for _, block in frames:
                value, = [v for k, v in unique_counters(block).items()
                          if k.endswith('.rob.clock_cycle')]
                cycles.append(value)
            assert sum(cycles) == frames[-1][0] + 1
            roi = reconstruct(cycles[0], cycles[1], rows)
            anchors.append(dict(native_exclusive_end=cycles[0], core_exclusive_end=roi['SQL_begin']))
        else:
            assert len(frames) == 1, 'Expected continuous or warmup-cleared capture'
            assert all(sum(r['STATES_' + str(i)] for i in range(4)) == r['CYCLE'] for r in rows)
        anchors.append(dict(native_exclusive_end=frames[-1][0] + 1, core_exclusive_end=rows[-1]['CYCLE']))
        method = 'shared_dump_SQL_summary_and_collection_prefix'
    offsets = {r['native_exclusive_end'] - r['core_exclusive_end'] for r in anchors}
    assert len(offsets) == 1, 'Core/native clock origin changes across dump anchors'
    offset, = offsets
    return dict(core_to_native_offset=offset, method=method, anchors=anchors,
                equation='native_cycle = core_cycle + core_to_native_offset')
