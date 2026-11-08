"""Read actual non-flushed scalar-FDIV interface events, never saved critical cycles."""
from native_perf import unique_counters

FIELDS = ('diag_fdiv_accepted_starts', 'diag_fdiv_consumed_finishes',
          'diag_fdiv_turnover_events', 'diag_fdiv_turnover_format_changes')


def turnover_counts(block):
    values = {name: value for name, value in unique_counters(block).items()
              if name.rsplit('.', 1)[-1] in FIELDS}
    modules = sorted({name.rsplit('.', 1)[0] for name in values})
    assert len(values) == 8 and len(modules) == 2, 'Missing or ambiguous turnover event counters'
    rows = []
    for module in modules:
        counters = {name.removeprefix('diag_fdiv_'): values[module + '.' + name] for name in FIELDS}
        starts, finishes, turnover, formats = (counters[name.removeprefix('diag_fdiv_')] for name in FIELDS)
        assert min(counters.values()) >= 0
        assert formats <= turnover <= min(starts, finishes), 'Turnover event subsets do not close'
        rows.append(dict(module=module, counts=counters,
                         fraction_of_starts_with_same_cycle_finish=turnover / starts if starts else None,
                         fraction_of_turnovers_changing_format=formats / turnover if turnover else None))
    return rows
