# TIP Analysis Tool

This directory contains offline analysis helpers for XiangShan TIP data dumped into ChiselDB sqlite files.

## Inputs

The tool expects a ChiselDB sqlite file containing a `Tip_<hartid>` table.

If `--db` is omitted, the script uses the latest `build/*.db`.

## Usage

Run all analyses and generate CSV/PNG outputs:

```bash
python3 scripts/tip/tip.py all --db build/<your-db>.db --out-dir scripts/tip/results
```

Run `all` with an ELF to additionally export symbolized commit reports:

```bash
python3 scripts/tip/tip.py all \
  --db build/<your-db>.db \
  --elf /path/to/program.elf \
  --out-dir scripts/tip/results-with-symbols
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
python3 scripts/tip/tip.py plot --db build/<your-db>.db --out-dir scripts/tip/results
```

Export symbolized commit hotspots:

```bash
python3 scripts/tip/tip.py symbols \
  --db build/<your-db>.db \
  --elf /path/to/program.elf \
  --sym-cache scripts/tip/.symbol-cache.json \
  --out-dir scripts/tip/results-symbols
```

Example symbolized workflow

```bash
python3 scripts/tip/tip.py all \
  --db /path/to/2026-04-14@13:15:58.db \
  --elf /path/to/program.elf \
  --out-dir scripts/tip/results-symbols \
  --top 20
```

## Options

- `--db`: explicit sqlite path
- `--hart`: hart id, default `0`
- `--out-dir`: output directory, default `scripts/tip/results`
- `--top`: hotspot row count, default `20`
- `--elf`: ELF binary for symbol resolution (required for `symbols`; optional for `all`)
- `--sym-cache`: path to JSON cache for `nm`/`addr2line` (default `scripts/tip/.symbol-cache.json`)

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
