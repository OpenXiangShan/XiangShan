# TIP Analysis Tool

This directory contains offline analysis helpers for XiangShan TIP data dumped into ChiselDB sqlite files.

## Inputs

The tool expects a ChiselDB sqlite file containing a `Tip_<hartid>` table.

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
  --elf /path/to/program.elf
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

- `--db`: explicit sqlite path
- `--hart`: hart id, default `0`
- `--out-dir`: output directory, default `$XDG_STATE_HOME/xiangshan/tip` (or `~/.local/state/xiangshan/tip`)
- `--top`: hotspot row count, default `20`
- `--elf`: ELF binary for symbol resolution (required for `symbols`; optional for `all`)
- `--sym-cache`: path to JSON cache for `nm`/`addr2line` (default `$XDG_CACHE_HOME/xiangshan/tip/symbols.json` or `~/.cache/xiangshan/tip/symbols.json`)

The defaults keep generated reports and symbol caches outside the source tree. Explicit paths still override them.

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
- `>= 4 = FUSED_OR_OTHER`

Replay and miss reports count dynamic head-entry residences, not logged cycles. Sticky flags contribute at most once per
entry, and cumulative `replayCnt` contributes its maximum value for that entry. A new residence starts after a lane-0
commit, a PC change, a timestamp gap, or a replay-counter reset.
