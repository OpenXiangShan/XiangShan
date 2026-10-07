# H2 single-checkpoint validation

On 2026-10-07 the user-authorized `bwaves_13153` run completed with DefaultConfig,
PDB64/LRU, fixed depth64, only release, observer enabled and complete trace enabled.
It used `c845cefbca9163543bccb97b68ef101c00e9b539` plus the recorded local change
making the trace default true. The running source and build were not changed.

Result directory, relative to this repository:
`cr1007030851-c845cefbc-H2Bwaves13153Trace_local`.
The exact executable, reference, source patch, checkpoint JSON and hashes are in
that directory and `h2-run-manifest.json`. The original `tmp/cr-run.sh` is unchanged.
The run-only copy reused the completed emulator build; it did not restart it.

The emulator ran on node009 from 03:09 to 10:08 CST. It enabled difftest and reached
the requested 40,000,000 total instructions without a difftest failure or RTL
assertion. The managed runner exited with status 0. Warmup was 20M instructions,
followed by 20M ROI. The performance sampling edge includes 20,000,016 warmup
commits and 20,000,000 ROI commits; these hardware samples are not identical to
the emulator's stopping-boundary instruction snapshot.

`2026-10-07-10-07-44.db` contains 11,612,233 observer event batches. Running
`scripts/check_pdb_victim_trace.py` on the complete table passed every FIFO update,
three-lane first-hit deletion, duplicate refresh, source/usage classification,
full-table eviction identity and occupancy check. Full-run totals were:

| Metric | Warmup | ROI | Total |
|---|---:|---:|---:|
| Capacity victims | 1,133,738 | 620,575 | 1,754,313 |
| Used victims | 49,271 | 18 | 49,289 |
| Unused victims | 1,084,467 | 620,557 | 1,705,024 |
| Demand queries | 15,841,753 | 12,498,431 | 28,340,184 |
| Used victim hits | 12 | 0 | 12 |
| Unused victim hits | 1,083,949 | 3,040 | 1,086,989 |
| Stream used victim hits | 9 | 0 | 9 |
| Stream unused victim hits | 1,083,882 | 3,040 | 1,086,922 |
| Duplicate insertions | 71 | 2 | 73 |
| Full FIFO evictions | 49,464 | 617,521 | 666,985 |
| FIFO entry cycles | 3,705,431,748 | 1,502,005,436 | 5,207,437,184 |
| FIFO occupancy at end | 244 | 256 | 256 |

Every phase event counter and occupancy integral matches the trace. The global
PERF timer is three cycles ahead of the local ChiselDB stamp: SimTop starts its
timer at top reset deassertion, while XSTop's three-stage `ResetGen` delays the
core/observer reset. This is visible in the generated `SimTop.sv`, `XSTop.sv`,
`ResetGen.sv` and `PDBVictimMonitor.sv`. Accordingly the warmup cut is local stamp
14,904,054 inclusive, for PERF time 14,904,057. Ignoring that offset moves one
victim and nine demand queries to the wrong phase, without changing total counts.
The independently integrated occupancy also matches exactly with this offset.

Evidence files are `h2-validation.json`, `h2-trace-check.json`,
`h2-phase-counters.json`, `h2-trace-phase-totals.json` (the unadjusted timestamp
split) and `h2-trace-occupancy-integral.json`. No checker mismatch was found.

This establishes single-checkpoint functional/trace validation. It does not
establish observer off/on performance equivalence, physical timing or area.
Those user-run validations remain outstanding. The current user goal explicitly
authorizes H3 implementation after this H2 check.
