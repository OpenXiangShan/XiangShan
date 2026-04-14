# TIP CoreMark Workload Analysis Design

**Goal:** Extend `scripts/tip` so it can combine TIP sqlite data with CoreMark workload metadata from `coremark-riscv64-xs.elf` and CoreMark sources, then produce deeper attribution reports at the function, source-line, and workload-family levels.

**Context:** The existing TIP tool already summarizes state, commit width, redirects, replays, and raw PC hotspots from `Tip_<hartid>` tables. For the CoreMark run captured in `build/2026-04-14@13:15:58.db`, the current outputs only show raw PCs. The workload build directory at `/nfs/home/wujiabin/work/xs-env/nexus-am/apps/coremark/build` provides an ELF with symbols and debug line info, which is enough to turn those PCs into actionable workload-level diagnosis.

**Users:** XiangShan developers who have already collected TIP sqlite dumps and want to answer questions such as:

- which CoreMark functions or source lines dominate commit traffic
- whether replay and redirect hotspots concentrate in list, matrix, or state code
- which workload family is responsible for the most bank conflicts, cache misses, or control redirects

## Scope

In scope:

- Reuse the existing `scripts/tip` command-line tool rather than creating a parallel tool.
- Accept an ELF path and symbolize commit, replay, and redirect addresses.
- Add CoreMark-specific workload grouping based on function names and source file paths.
- Export CSVs for function-level and family-level attribution.
- Optionally generate a small number of family-level PNG plots.
- Keep the symbolization layer generic so it can be reused by future workloads.

Out of scope:

- Automatic workload detection for arbitrary binaries.
- HTML dashboards or interactive UIs.
- Full timeline reconstruction of benchmark phases.
- Multi-run comparison across multiple sqlite files.
- Persistent disassembly databases or instruction-by-instruction reports for the whole binary.

## Design Summary

The extension will add two new layers on top of the current TIP queries:

1. A generic symbolization layer:
   It resolves raw PCs to function names, source files, source lines, and symbol-relative offsets using the supplied ELF.

2. A CoreMark workload-attribution layer:
   It groups symbolized records into meaningful CoreMark families such as `list`, `matrix`, `state`, and `init_runtime`.

The resulting flow is:

`sqlite TIP data -> raw per-event rows -> symbolized rows -> CoreMark family aggregation -> CSV/PNG outputs`

This avoids changing the underlying TIP query model while making the results much more useful for workload diagnosis.

## CLI Shape

The existing subcommands stay in place:

- `summary`
- `hotspots`
- `plot`
- `all`

New subcommands will be added:

- `symbols`: export symbolized hotspot and event reports
- `workload`: export CoreMark workload-family and function summaries

New common arguments:

- `--elf`: path to the workload ELF, required for `symbols`, `workload`, and any `all` mode that requests these outputs
- `--workload-root`: root of the workload source tree, default inferred from the ELF location when possible
- `--sym-cache`: optional cache file for resolved PCs to reduce repeated `addr2line` lookups

Existing common arguments remain:

- `--db`
- `--hart`
- `--out-dir`
- `--top`

`all` will be extended so that:

- without `--elf`, it behaves like the current implementation
- with `--elf`, it additionally emits symbolized and workload-attribution outputs

## Module Structure

The implementation will keep the current split and add two focused modules:

- `scripts/tip/tip.py`
  - extend CLI parsing
  - orchestrate raw, symbolized, and workload outputs
- `scripts/tip/queries.py`
  - keep sqlite querying and raw row generation
  - add helpers that expose richer per-event rows for symbolization
- `scripts/tip/plot.py`
  - keep current plots
  - optionally add family-level plots
- `scripts/tip/symbolize.py`
  - wrap `nm`, `addr2line`, and optional `objdump` access
  - batch-resolve PCs and return normalized symbol records
  - handle cache load/store
- `scripts/tip/workloads/coremark.py`
  - classify functions and files into CoreMark workload families
  - aggregate symbolized rows into function and family summaries

This keeps the generic ELF resolution logic separate from CoreMark-specific rules.

## Symbolization Model

The generic symbolization layer will resolve any PC used by the TIP reports and attach:

- `pc`
- `function`
- `file`
- `line`
- `location`
- `family` if a workload classifier is active
- `symbol_offset`

Resolution tools:

- `riscv64-unknown-elf-addr2line -f -C` for function and source-line mapping
- `riscv64-unknown-elf-nm -n` for symbol-range lookup and stable function boundaries

The layer will batch unique PCs before calling external tools so repeated hotspots do not cause repeated lookups.

If a PC cannot be resolved cleanly, the row remains usable with fallback values such as:

- `function = <unknown>`
- `file = <unknown>`
- `line = 0`
- `family = other`

## CoreMark Workload Classification

CoreMark should not be treated as a simple time-ordered sequence of `list -> matrix -> state`. In this build, `iterate()` mainly invokes `core_bench_list()`, and `core_bench_list()` can call `calc_func()`, which in turn invokes matrix and state workloads. Because of that nesting, the most accurate attribution is function-family grouping, not coarse phase windows.

The initial CoreMark family mapping will be:

- `list`
  - `core_bench_list`
  - `core_list_*`
  - `cmp_*`
  - `calc_func`
  - `copy_info`
- `matrix`
  - `core_bench_matrix`
  - `matrix_*`
  - `core_init_matrix`
- `state`
  - `core_bench_state`
  - `core_state_*`
  - `core_init_state`
- `init_runtime`
  - `main`
  - `iterate`
  - `portable_*`
  - `start_time`
  - `stop_time`
  - `get_time`
  - `_trm_*`
  - `__am_*`
  - `printf_`
  - `uptime`
- `other`
  - anything not matched above

The classifier will primarily use function names. Source file path is a fallback, for example:

- `src/core_list_join.c` -> `list`
- `src/core_matrix.c` -> `matrix`
- `src/core_state.c` -> `state`

## Event Attribution Rules

Each report must use one clear attribution rule:

- commit hotspots:
  - attribute per commit slot by `COMMITS_INFO_i_DEBUG_PC`
- commit-type hotspots:
  - same as above, plus `COMMITS_INFO_i_COMMITTYPE`
- replay hotspots:
  - attribute by the associated committed PC in the row, using the current TIP convention based on `COMMITS_INFO_0_DEBUG_PC`
- redirect source:
  - attribute by `REDIRECT_PC`
- redirect target:
  - attribute by `REDIRECT_BITS_CFIUPDATE_TARGET`

The workload aggregation layer will sum these symbolized rows into:

- function-level event totals
- family-level event totals
- family-level ratios versus the workload total

## Outputs

Existing outputs remain unchanged.

New symbolized CSVs:

- `pc_hotspots_symbolized.csv`
- `pc_commit_type_hotspots_symbolized.csv`
- `redirect_targets_symbolized.csv`
- `replay_hotspots_symbolized.csv`

New workload CSVs:

- `workload_function_summary.csv`
- `workload_family_summary.csv`
- `workload_replay_summary.csv`
- `workload_redirect_summary.csv`

Potential new PNGs:

- `workload_family_commit.png`
- `workload_family_replay.png`

`workload_function_summary.csv` will answer "which exact functions are hottest".

`workload_family_summary.csv` will answer "which CoreMark family dominates commit traffic".

`workload_replay_summary.csv` and `workload_redirect_summary.csv` will answer "which family is causing replay or redirect pressure".

## Testing

Unit tests will extend `scripts/tip/tests/test_tip.py` to cover:

- symbolization of known synthetic PCs through a mocked resolver
- workload-family classification rules for representative CoreMark function names
- CLI behavior for `symbols` and `workload`
- CSV generation with family and function summary fields

Real-db smoke validation will use:

- database: `build/2026-04-14@13:15:58.db`
- ELF: `/nfs/home/wujiabin/work/xs-env/nexus-am/apps/coremark/build/coremark-riscv64-xs.elf`

Expected smoke-check behavior:

- raw TIP reports still work
- symbolized reports contain resolved CoreMark functions such as `core_list_reverse`, `matrix_mul_matrix`, and `core_init_state`
- workload summaries show useful separation across `list`, `matrix`, `state`, and `init_runtime`

## Error Handling

- Fail clearly if `--elf` is required but missing.
- Fail clearly if the ELF toolchain commands are unavailable.
- Continue with fallback placeholder symbols when a subset of PCs cannot be resolved.
- Validate that the workload classifier is only activated when the user requests it.
- Preserve current behavior for users who only want raw TIP analysis.
