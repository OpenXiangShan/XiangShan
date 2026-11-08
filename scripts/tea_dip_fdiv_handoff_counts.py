"""Classify FDIV finishes using the IQ launch window three cycles earlier."""
from native_perf import unique_counters
from tea_dip_fdiv_turnover_counts import turnover_counts

PREFIX = 'diag_fdiv_handoff_p0_'
REASONS = ('no_pending_at_selection_cycles', 'pending_not_eligible_cycles',
           'eligible_wb_blocked_cycles', 'wb_eligible_ready_blocked_cycles',
           'selectable_not_selected_cycles', 'selected_no_start_cycles')
GATES = tuple(f'ready_gate_mask{mask}_cycles' for mask in range(1, 8))
FIELDS = tuple(PREFIX + field for field in
               ('finish_cycles', 'turnover_cycles', 'no_start_cycles', *REASONS,
                'turnover_unmatched_selection_cycles', *GATES))


def handoff_counts(block):
    values = {name: value for name, value in unique_counters(block).items()
              if name.rsplit('.', 1)[-1] in FIELDS}
    modules = sorted({name.rsplit('.', 1)[0] for name in values})
    assert len(values) == 34 and len(modules) == 2, 'Missing or ambiguous handoff counters'
    rows = []
    for module in modules:
        counts = {field.removeprefix(PREFIX): values[module + '.' + field] for field in FIELDS}
        assert min(counts.values()) >= 0
        assert counts['finish_cycles'] == counts['turnover_cycles'] + counts['no_start_cycles']
        assert sum(counts[key] for key in REASONS) == counts['no_start_cycles']
        assert sum(counts[key] for key in GATES) == counts['wb_eligible_ready_blocked_cycles']
        assert counts['turnover_unmatched_selection_cycles'] == 0, 'Three-cycle launch alignment failed'
        rows.append(dict(module=module, counts=counts,
                         no_start_reason_fractions={key: counts[key] / counts['no_start_cycles']
                                                    if counts['no_start_cycles'] else None for key in REASONS}))
    interface = turnover_counts(block)
    assert sum(row['counts']['finish_cycles'] for row in rows) == sum(row['counts']['consumed_finishes'] for row in interface)
    assert sum(row['counts']['turnover_cycles'] for row in rows) == sum(row['counts']['turnover_events'] for row in interface)
    return rows
