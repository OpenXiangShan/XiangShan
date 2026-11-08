"""Partition unit-only handoff blocks by the same t-3 start and wakeup context."""
from native_perf import unique_counters
from tea_dip_fdiv_handoff_counts import handoff_counts

PREFIX = 'diag_fdiv_predict_p0_'
FIELDS = tuple(PREFIX + f'context_mask{mask}_cycles' for mask in range(8))


def predict_counts(block):
    old = {row['module']: row for row in handoff_counts(block)}
    values = {name: value for name, value in unique_counters(block).items()
              if name.rsplit('.', 1)[-1] in FIELDS}
    modules = sorted({name.rsplit('.', 1)[0] for name in values})
    assert len(values) == 16 and set(modules) == set(old), 'Missing or ambiguous prediction-context counters'
    rows = []
    for module in modules:
        counts = {field.removeprefix(PREFIX): values[module + '.' + field] for field in FIELDS}
        denominator = old[module]['counts']['ready_gate_mask1_cycles']
        assert min(counts.values()) >= 0 and sum(counts.values()) == denominator
        assert counts['context_mask0_cycles'] == counts['context_mask1_cycles'] == 0, 'No attempt must grant wakeup permission'
        rows.append(dict(module=module, counts=counts, unit_only_no_start_finishes=denominator,
                         context_fractions={key: value/denominator if denominator else None for key, value in counts.items()}))
    return rows
