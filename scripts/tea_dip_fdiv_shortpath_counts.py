"""Classify effective extra ready forecasts and the concurrent next-IQ selection."""
from native_perf import unique_counters

PREFIX = 'diag_fdiv_shortpath_p0_'
REASONS = ('extra_no_pending_cycles', 'extra_pending_not_eligible_cycles',
           'extra_eligible_wb_blocked_cycles', 'extra_wb_eligible_ready_blocked_cycles',
           'extra_selectable_not_selected_cycles', 'extra_selected_cycles')
GATES = tuple(f'extra_ready_mask{mask}_cycles' for mask in range(1, 4))
FIELDS = tuple(PREFIX + field for field in ('accepted_start_cycles', 'extra_ahead_cycles', *REASONS, *GATES))


def shortpath_counts(block):
    values = {name: value for name, value in unique_counters(block).items() if name.rsplit('.', 1)[-1] in FIELDS}
    modules = sorted({name.rsplit('.', 1)[0] for name in values})
    assert len(values) == 22 and len(modules) == 2, 'Missing or ambiguous short-path counters'
    rows = []
    for module in modules:
        counts = {field.removeprefix(PREFIX): values[module + '.' + field] for field in FIELDS}
        assert min(counts.values()) >= 0
        assert counts['extra_ahead_cycles'] <= counts['accepted_start_cycles']
        assert sum(counts[key] for key in REASONS) == counts['extra_ahead_cycles']
        assert sum(counts[key] for key in GATES) == counts['extra_wb_eligible_ready_blocked_cycles']
        rows.append(dict(module=module, counts=counts,
                         extra_ahead_per_accepted_start=counts['extra_ahead_cycles'] / counts['accepted_start_cycles'] if counts['accepted_start_cycles'] else None,
                         extra_reason_fractions={key: counts[key] / counts['extra_ahead_cycles'] if counts['extra_ahead_cycles'] else None for key in REASONS}))
    return rows
