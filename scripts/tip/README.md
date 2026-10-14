# TIP Analysis Tool

This directory contains offline analysis helpers for XiangShan TIP data dumped into ChiselDB sqlite files.

## Inputs

The tool expects a ChiselDB sqlite file containing a `Tip_<hartid>` table from the current RTL with the default
eight commit lanes. Generate the RTL with `--with-chiseldb` and `--enable-difftest` (or `--full-basicdiff`), since
the PC columns require the debug metadata enabled by difftest. When running the simulator, enable database output with
`--dump-db`, or select only TIP with `--dump-select-db Tip_0`.

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
- `--out-dir`: output directory, default `$XDG_STATE_HOME/xiangshan/tip` (or `~/.local/state/xiangshan/tip`)
- `--top`: hotspot row count, default `20`
- `--elf`: ELF binary for symbol resolution (required for `symbols`; optional for `all`)
- `--sym-cache`: path to JSON cache for `nm`/`addr2line` (default `$XDG_CACHE_HOME/xiangshan/tip/symbols.json` or `~/.cache/xiangshan/tip/symbols.json`)
- `--pc-bits`: architectural PC width before ELF lookup, default `39`; use `48` for Sv48
- `--pc-mode`: extend the PC with `sign` for Sv39/Sv48 or `zero` for an unsigned/bare address, default `sign`

The defaults keep generated reports and symbol caches outside the source tree. Explicit paths still override them.
Symbol lookup does not apply PIE, ASLR, or other runtime relocation offsets.

## Outputs

`all` always generates the raw summary/hotspot CSVs and the standard plots. When `--elf` is provided (or when using
`symbols`), symbolized outputs are also produced.

CSV files:

- `state_summary.csv`
- `commit_width_summary.csv`
- `commit_type_summary.csv`
- `redirect_summary.csv`
- `replay_summary.csv`
- `pc_hotspots.csv`
- `pc_commit_type_hotspots.csv`
- `redirect_targets.csv`
- `replay_hotspots.csv`

The PC hotspot CSVs count committed physical ROB entries. `replay_hotspots.csv` counts observed valid head residences.

Additional CSV files when `--elf` is provided (or when using `symbols`):

- `symbolized_commits.csv`

PNG files:

- `state_distribution.png`
- `commit_width_distribution.png`
- `top_pc_hotspots.png`
- `redirect_targets.png`

## Encodings

TIP state:

- `0 = computing`
- `1 = stalled`
- `2 = walk`
- `3 = drained`

Commit type:

- `0 = NORMAL`
- `1 = BRANCH`
- `2 = LOAD`
- `3 = STORE`

Commit reports count committed physical ROB entries. With ROB compression, an entry can contain several instructions;
its reported PC and commit type describe the first slot. These reports are not whole-program instruction hotspots.

Replay and miss reports count valid dynamic head-entry residences, excluding empty ROB cycles. Sticky flags contribute
at most once per entry, and cumulative `replayCnt` contributes its maximum value for that entry. A new residence starts
after a lane-0 commit, a PC change, a timestamp gap, or a replay-counter reset. These reports cover only residences
observed while the entry is at the ROB head; they are not whole-program replay totals.
