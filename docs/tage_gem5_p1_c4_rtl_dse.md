# TAGE GEM5 P1 and C4 RTL Regression

## Scope

This document records an RTL regression for the two points selected from the
GEM5 8-table TAGE capacity DSE, with the default active TAGE parameters as the
baseline. The authoritative GEM5 source is commit
`08e5e2bb6117cc6898bac43028065811309de27e` and its report:

`https://github.com/OpenXiangShan/GEM5/blob/08e5e2bb6117cc6898bac43028065811309de27e/docs/Gem5_Docs/frontend/tage_capacity_8table_t2_t4_nsga2_dse_20260821.md`

The three RTL jobs use the same tested commit, `DefaultConfig`, `gsim`, and
the exact 12-benchmark filter:

```text
perlbench,bzip2,gcc,mcf,gobmk,hmmer,sjeng,libquantum,h264ref,omnetpp,astar,xalancbmk
```

The workflow input is `legacy-spec06-gcc15-1.0c`. It resolves to the GCC15
SPEC06 1.0c checkpoint profile at
`spec06_gcc15_rv64gcb_base_260604/json/checkpoints_all.json`. This filter
selects 35 workload configurations and 697 checkpoints.

## Parameter Mapping

GEM5 `tableSizes[i]` is the unbanked table's set count. RTL TAGE has four
banks, so the exact mapping for a GEM5 `tableSizes[i]` is:

```text
rtl_num_sets_log2[i] = log2(tableSizes[i] / 4)
```

All selected GEM5 table sizes divide by four. `ways` and `tagBits` map
one-for-one. The history lengths remain the RTL defaults
`[4,9,17,29,56,109,211,397]`.

| Point | RTL set log2 T0..T7 | ways T0..T7 | tag bits T0..T7 | GEM5 logical capacity |
| --- | --- | --- | --- | ---: |
| Baseline | `[9,9,9,9,9,9,9,9]` | `[2,2,2,2,2,2,2,2]` | `[13,13,13,13,13,13,13,13]` | 589824 bit, 72.000 KiB |
| P1, `trial_0748` | `[5,6,9,10,9,8,7,8]` | `[8,3,4,3,3,5,1,3]` | `[15,11,8,11,18,18,15,17]` | 672768 bit, 82.125 KiB |
| C4, `trial_0166` | `[6,6,9,10,9,9,8,9]` | `[2,3,2,3,3,2,2,1]` | `[15,18,16,11,18,11,13,13]` | 591104 bit, 72.15625 KiB |

The logical capacity formula is:

```text
sum(tableSizes[i] * numWays[i] * (tagBits[i] + 5))
```

The `+5` is GEM5's active-entry proxy: valid bit, 3-bit direction counter,
and useful bit. It is not an RTL area, timing, or power metric.

## RTL Support

The branch expands the Constantin TAGE domain to a superset of the GEM5
domain:

```text
per-bank set log2: 4..11
ways:              1..8
tag bits:          8..20, independently per table
```

The GEM5 solver has narrower per-table set domains: T0, T1, and T5--T7 map
to per-bank log2 4..9, while T2--T4 map to 5..11. The common RTL bound keeps
both classes representable; the selected P1 and C4 vectors are within their
respective GEM5 domains.

The original Constantin implementation had a global tag width, so neither
P1 nor C4 could be represented exactly. `TageTableConfig` now carries each
table's tag width, and folded-history selection, lookup, training, and
allocation all use that table's active tag mask.

The existing `tageTagWidth_<hart>` Constantin key remains a compatibility
fallback. A `tageTableTagWidth_<hart>_<table>` value of zero selects that
legacy global width; a nonzero per-table value takes precedence. The committed
DSE files set all eight per-table widths explicitly.

The physical RTL instantiates the maximum SRAM shape needed by the enabled
domain and masks active sets and ways at runtime. It also uses a separate
two-bit useful-counter SRAM. Therefore the GEM5 logical capacity above is an
active-entry comparison proxy only; it must not be read as synthesized RTL
storage or PPA equality.

Supporting the selected 18-bit tags raises the compiled maximum tag storage
width from 15 to 20 bits. Thus the baseline is a common-expanded-physical
control run, not a bit-identical elaboration of the historical default RTL.
The physical design already has eight per-way write-buffer ports; its
four-entry buffer depth is unchanged by this regression, including P1's eight
active ways. The three runs share this same source-level physical hardware
configuration, and the committed Constantin files are the only intended
active TAGE-configuration difference. Because each Constantin filename gives
the workflow a distinct `SPEC_DIR`, the jobs build separate emulator artifacts
unless an explicit cache is reused, so they should not be described as
byte-identical binary reuse.

## CI Contract

The committed files are:

| Point | Constantin file |
| --- | --- |
| Baseline | `configs/tage-dse/gem5-baseline.cst` |
| P1 | `configs/tage-dse/gem5-p1-trial-0748.cst` |
| C4 | `configs/tage-dse/gem5-c4-trial-0166.cst` |

The branch's `perf-trigger.yml` supports the legacy benchmark name. Its
template resolves a repository-relative Constantin file against
`GITHUB_WORKSPACE`, copies it into the shared `SPEC_DIR`, and passes that
absolute copy to distributed emulation workers. This prevents a worker from
resolving the path relative to the perf-trigger script directory.
Constantin jobs reject `open` workers because the external runner does not
copy the Constantin file to their separate filesystem. The selected `gsim`
configuration resolves to `node` workers and is supported.

The expected result directory for each run is:

```text
/nfs/home/cirunner/perf-report/cr<commit-date>-<short-sha>-DefaultConfig-<cst-stem>
```

Dispatch each of these three jobs with `expected_checkpoints=697`. The workflow
then fails before acceptance if the score reports less than `1.00/1.00`
coverage, anything other than `697/697` successful checkpoints, a nonempty
failed-checkpoint list, or a simulator-log count other than 697.

For acceptance, inspect the completed 697 checkpoints, the score file
`score-legacy-spec06-gcc15-1.0c.txt`, the copied Constantin file, and runtime
logs containing `[INFO] constant updated:` for every selected TAGE record.
The workflow also uploads a compact Constantin-evidence artifact containing
the full commit SHA, emulator and CST SHA-256 values, the copied CST, and a
per-checkpoint verification manifest. The manifest requires every one of the
25 CST records to appear exactly once with its expected value in every log.

## Historical GEM5 Evidence

The GEM5 report's 12-benchmark 1.0c analysis is prior evidence, not an RTL
result. It reports the following geometric means and diagnostic branch MPKI:

| Point | SPECint score/GHz | Delta vs GEM5 baseline | Branch MPKI | Delta |
| --- | ---: | ---: | ---: | ---: |
| Baseline | 18.689004 | 0.000000% | 7.234918 | 0.000000% |
| P1 | 18.797498 | +0.580524% | 6.965442 | -3.724661% |
| C4 | 18.731938 | +0.229726% | 7.099714 | -1.868768% |

P1 is the GEM5 solver's score-extreme point. C4 is a near-default-capacity
candidate, not a formal score/branch-mispredict Pareto point. The historical
GEM5 baseline and candidates used different source SHAs, so those figures are
not a strict TAGE-only A/B claim. The three RTL jobs below use one tested SHA.

## RTL Results

Pending CI dispatch and completed-archive audit.
