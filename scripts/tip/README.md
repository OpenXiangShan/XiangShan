# TIP Analysis Tool

This directory contains XiangShan's cycle-attribution implementation of Time-Proportional Instruction Profiling
(TIP). ChiselDB records every simulation cycle, and the offline analyzer applies the Oracle attribution policy from
the MICRO 2021 TIP paper. This is an exhaustive simulation profiler, not the paper's low-overhead PMU sampler.

## Inputs

The tool expects a ChiselDB sqlite file containing a `Tip_<hartid>` table from the current RTL with the default
eight commit lanes. Generate the RTL with `--with-chiseldb` and `--enable-difftest` (or `--full-basicdiff`), since
the architectural PC columns require the debug metadata enabled by difftest. When running the simulator, enable
database output with `--dump-db`, or select only TIP with `--dump-select-db Tip_0`. Databases generated before the
TIP attribution columns were added are rejected instead of being interpreted with physical-entry semantics.

If `--db` is omitted, the script uses the latest `build/*.db`.

## Usage

Run all analyses and generate CSV/PNG outputs:

```bash
python3 scripts/tip/tip.py all --db build/<your-db>.db
```

Run `all` with an ELF to additionally export symbolized commit reports:

```bash
python3 scripts/tip/tip.py all \
  --db build/<your-db>.db \
  --elf /path/to/program.elf
```

Use the latest database automatically:

```bash
python3 scripts/tip/tip.py all
```

Print only summaries:

```bash
python3 scripts/tip/tip.py summary --db build/<your-db>.db
```

Export only hotspots:

```bash
python3 scripts/tip/tip.py hotspots --db build/<your-db>.db --top 30
```

Generate only plots:

```bash
python3 scripts/tip/tip.py plot --db build/<your-db>.db
```

Export symbolized commit hotspots:

```bash
python3 scripts/tip/tip.py symbols \
  --db build/<your-db>.db \
  --elf /path/to/program.elf \
  --pc-bits 39 \
  --pc-mode sign
```

Example symbolized workflow

```bash
python3 scripts/tip/tip.py all \
  --db /path/to/2026-04-14@13:15:58.db \
  --elf /path/to/program.elf \
  --out-dir /path/to/tip-results \
  --top 20
```

## Options

Place options after the subcommand, as in the examples above.

- `--db`: explicit sqlite path
- `--hart`: hart id, default `0`
- `--out-dir`: output directory; by default, reports are placed under
  `$XDG_STATE_HOME/xiangshan/tip` (or `~/.local/state/xiangshan/tip`) in a
  directory identified by the resolved database path and hart
- `--top`: hotspot row count, default `20`
- `--elf`: ELF binary for symbol resolution (required for `symbols`; optional for `all`)
- `--sym-cache`: path to JSON cache for `nm`/`addr2line` (default `$XDG_CACHE_HOME/xiangshan/tip/symbols.json` or `~/.cache/xiangshan/tip/symbols.json`)
- `--pc-bits`: architectural PC width before ELF lookup, default `39`; use `48` for Sv48
- `--pc-mode`: extend the PC with `sign` for Sv39/Sv48 or `zero` for an unsigned/bare address, default `sign`

The defaults keep generated reports and symbol caches outside the source tree. Explicit paths still override them.
When an output directory is reused, known reports not produced by the current command are removed.
Symbol lookup does not apply PIE, ASLR, or other runtime relocation offsets.

## Outputs

`all` always generates the raw summary/hotspot CSVs and the standard plots. When `--elf` is provided (or when using
`symbols`), symbolized outputs are also produced.

CSV files:

- `time_profile.csv`: the primary `PC -> attributed cycles` TIP profile
- `tip_attribution_summary.csv`: total, attributed, and unresolved cycles plus attribution coverage
- `state_summary.csv`
- `commit_width_summary.csv`: architectural instructions committed per computing cycle
- `commit_type_summary.csv`
- `redirect_summary.csv`
- `replay_summary.csv`
- `pc_hotspots.csv`
- `pc_commit_type_hotspots.csv`
- `redirect_targets.csv`
- `replay_hotspots.csv`

`pc_hotspots.csv` and `pc_commit_type_hotspots.csv` are supplemental physical ROB-entry reports.
`replay_hotspots.csv` counts observed valid head residences.

Additional CSV files when `--elf` is provided (or when using `symbols`):

- `symbolized_time_profile.csv`
- `symbolized_commits.csv`

PNG files:

- `top_time_profile.png`
- `state_distribution.png`
- `commit_width_distribution.png`
- `top_pc_hotspots.png`
- `redirect_targets.png`

## Encodings

TIP state:

- `0 = computing`
- `1 = stalled`
- `2 = flushed`
- `3 = drained`

Cycle attribution:

- Computing: the cycle is divided evenly across every architectural instruction committed in that cycle. ROB
  compression and fusion are expanded using the architectural PC metadata captured before Rename removes followers.
- Stalled: the cycle is assigned to the oldest unfinished architectural instruction in the physical ROB head entry
  (`STALLPC`).
  Debug-only outstanding-writeback tracking preserves this identity when several instructions share a compressed slot.
- Flushed: cycles where the ROB is empty because of a redirect are assigned to the offending instruction retained in
  the OIR. Recovery cycles with instructions still in the ROB remain Computing or Stalled, as required by TIP.
- Drained: empty-ROB frontend cycles are buffered and assigned to the first architectural instruction of the first
  subsequent valid physical ROB entry (`FIRSTARCHPC`), independently of its dynamic stalled PC.

The sum of `cycles` in `time_profile.csv` plus `unattributed_cycles` equals `total_cycles`. Normally coverage is 100%.
Only a trailing drained interval with no subsequent instruction in the dump remains unresolved; it is reported in
`tip_attribution_summary.csv` rather than silently assigned to an unrelated PC.

Commit type:

- `0 = NORMAL`
- `1 = BRANCH`
- `2 = LOAD`
- `3 = STORE`

The supplemental commit-type and commit-PC reports count committed physical ROB entries. With ROB compression, an
entry can contain several instructions; its reported PC and commit type describe the first slot. Use
`time_profile.csv`, not these reports, for TIP hotspots.

Replay and miss reports count valid dynamic physical ROB-entry residences, excluding empty ROB cycles. The physical
entry identity (`ENTRYID` plus `ENTRYGENERATION`) is used for deduplication; changes to `STALLPC` do not start a new
residence. Sticky flags contribute at most once per entry, and cumulative `replayCnt` contributes its maximum value for
that entry. A new residence starts after an entry identity change, a timestamp gap, or a replay-counter reset. These
reports cover only residences observed while the entry is at the ROB head; they are not whole-program replay totals.
