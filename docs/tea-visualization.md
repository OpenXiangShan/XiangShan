# TEA visualization and analysis

`scripts/tea_visualize.py` opens `Tea_0` and `TeaDelayedFinal_0` read-only and
produces PICS figures, hotspot tables, timelines, and summaries. It requires
Python 3, Matplotlib, and NumPy and accepts the current per-instruction schema.
See [TEA implementation](tea-implementation.md) for tracker and attribution rules.

## Basic use

Collect both sample and delayed-final tables. Other high-volume database tables
are unnecessary for TEA analysis; ordinary performance counters remain available
in simulator stdout/stderr.

```bash
python3 scripts/tea_visualize.py build/tea.db \
  --labels coremark --output build/tea-profile
```

Multiple database arguments are supported. `--labels`, `--elf`, and
`--spec-log-dir`, when provided, must have one value per database in matching
order. Use `-` to skip ELF annotation for one input. The default disassembler is
`riscv64-unknown-linux-gnu-objdump`; `--objdump` selects another executable.
`--top` limits displayed PCs, while CSV output retains all PCs. `--bins` controls
timeline binning.

## Sample windows and exclusions

`--cycle-range START END` selects boundaries in `[START, END)`. Delayed targets
are resolved against the complete final table, including final records outside
the selected interval.

For the default SPEC 20M-warmup/40M-total-instruction setup, `--spec-log-dir`
validates each input's `simulator_out.txt` and `simulator_err.txt`, successful
termination, two ROB counter reports, and the reset boundary. Warmup retirement
must be in `[20M, 20.1M)` and measured retirement in `[19.9M, 20.1M)`. Invalid
windows or measured windows without selected TEA samples are rejected. This
option cannot be combined with `--cycle-range`.

```bash
python3 scripts/tea_visualize.py /path/to/checkpoint/simulator.db \
  --spec-log-dir /path/to/checkpoint --spec-checkpoint --output build/tea-measured
```

`--spec-checkpoint` excludes the current archive's flash range
`[0x10000000, 0x10100000)` and restorer range `[0x80000000, 0x80000c00)`.
Unknown PCs retain their weight. Filtering happens after a Computing boundary's
retirement denominator is established, so excluded weight is not redistributed.

When logs contain `teaSamplePeriod0`, summaries report expected boundaries,
raw rows, duplicates, off-period rows, internal misses, and endpoint misses.
Checks preserve the original sampling phase. They do not fill missing samples
or alter attribution weights. Interpret endpoint differences alongside simulator
exit timing and the final performance dump.

## Weights and interpretation

Computing divides one boundary among all N architectural retirees, `1/N` each.
Stalled selects the oldest unfinished architectural member in the head entry,
then resolves its final PSV by allocation cycle, ROB index, and rank. Drained
uses the first real successor commit. Flushed uses the saved recovery cause.
The Stalled policy adapts to ROB compression; it is not a critical-path proof.

A complete PSV combination is one PICS category, rather than separate allocations
to every set bit. Cancelled, unresolved, and unknown attribution remain explicit
and retain weight. **No recorded event** means PSV zero, not an absence of stalls.
ST_LLC is not implemented.

Full PCs are restored from signed SQLite values before filtering and ELF
matching. Member masks are split by physical commit lane. `CYCLE` is the sample
boundary; `STAMP` does not identify delayed targets.

Event heatmaps and timelines use the same weights as PICS. Eight Computing
members with a given event collectively contribute one boundary, just as one
Stalled target does. Raw occurrence counts are also retained, but event bits can
overlap and must not be summed as disjoint cycle partitions. A final ST_TLB
signature may include events after the sampled boundary; it does not directly
measure the boundary's DTLB miss or page-walk latency.

## Outputs

Each input produces `pics.png/pdf`, `hotspots.csv`, `hotspot_states.csv`, timeline
figures/data, and `head_context.csv`. The output root includes comparison figures,
`summary.csv`, and `summary.json`.

`hotspot_states.csv` preserves every PC × state × PSV combination. Its
`sample_weight` is that combination's weight; `selected_weight_pct` uses all
post-filter weight, while `pc_weight_pct` uses all weight for that PC. These
separate frequently committed instructions from Stalled targets and recovery
causes. Summary quality fields include cancelled, unresolved, unknown, and
excluded weights.

When statistics tables are available, `cycle_summary.json` and
`no_commit_intervals.csv` report their whole observed collection window,
independently of sample ROI/PC filtering. `head_context.csv` follows sample
filters. Exact counters cover only the observed prefix; `collection_closed=false`
requires checking coverage against the final performance report.

## Measured-window comparisons

When all inputs have validated SPEC logs and sample periods, the tool also
exports `sampled_costs.png/pdf/csv`:

```text
sampled cycles per thousand instructions =
    sample_weight * sample_period * 1000 / measured_instructions
```

These are sampling estimates. Measured CPI is shown separately and does not
rescale the sampled bars or fill missing coverage. `hotspot_states.csv` adds the
same `sampled_cycles_per_ki` unit while retaining raw weights and percentages.

For the same checkpoint and instruction window, `--compare-first` uses the
first input as the baseline. It requires at least two inputs, matching SPEC log
directories, and logged sample periods. `sampled_cost_deltas.csv` contains
candidate-minus-baseline state, overlapping-event, quality, and measured total
cost differences. `hotspot_deltas.csv` orders per-PC/state/PSV differences by
absolute magnitude, including one-sided and unresolved categories.

Each run uses its own measured instruction count and period. `kind=measured`
comes from ROB counters; sampled categories are estimates. `Measured minus
captured` preserves the signed gap between measured total cycles and captured
sampled states, including excluded PCs, so the totals conserve on each side and
in the difference. This gap can include sampling endpoint effects and missing
samples; it is not a hardware stall category.

The caller must verify that binaries, checkpoints, seeds, configurations, and
instruction windows are comparable. Neither signature weights nor their deltas
prove recoverable cycles or translate directly into a benchmark/SPEC score.
