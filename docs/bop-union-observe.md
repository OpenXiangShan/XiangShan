# Independent union BOP with original P/V observers

This experiment starts from `feat-onebop-only-bop` (`85cfc28629`) and keeps three independent learners.
The main repository and XSCache use branch `perf-bop-union-observe`; XSCache is pinned to `7b9ea447`.

| Instance | Address/training policy | Candidates | Emits requests |
| --- | --- | ---: | --- |
| `prefetcher.pbop` | Original physical BOP; immediate student bitmap updates; pool 8 | Original 37 | No |
| `prefetcher.vbop` | Original virtual BOP; delayed student bitmap updates; pool 4 | Original 72 | No |
| `prefetcher.ubop` | Independent virtual BOP; delayed student bitmap updates; pool 4 | V's original 72 followed by P's 37 | Yes |

UBOP owns its teacher, student pool, RR table, delay queue, request buffer and Constantin records.
It shares the virtual BOP implementation, but does not read either observer's learned state.
Original P/V candidate lists, scoring, hash, phase rules and student parameters are retained.
Their external emission enables are tied low; their training queues stay connected.
UBOP uses the existing `l2_pf_master_en`, `l2_vbop_en`, VBOP feedback degree and delay control.
Other data prefetchers remain disabled as in the only-BOP baseline.

Only UBOP uses the BOP TLB path and receives BOP responses. Assertions check that the original
engines never emit and that the original VBOP does not request translations.

## Performance counters

Each of `pbop`, `vbop`, `ubop` retains its own:

- `scoreTable.teacher_best_offset_*`: teacher recommendations, counted at phase boundaries.
- `student.student_best_offset_*`: student winners, counted at phase boundaries.
- `bop_train`, `bop_student_takeover`, phase/disable/stall counters.
- `prefetch_sent_issue_offset_*`, `bop_req`: actual issue counts; original P/V must stay zero.

`prefetcher` adds `ubop_train_accept`, `ubop_train_drop`, `prefetch_req_fromUBOP` and
`prefetch_req_selectUBOP`; the existing aggregate BOP counters include UBOP.
Each learner has its own four-entry input queue. Input acceptance and drops can differ;
the observers are not claimed to accept identical address/time sequences.

**Downstream source compatibility:** UBOP retains `MemReqSource.Prefetch2L2BOP` and the existing
`PfSource.BOP` metadata so the feedback controller, acknowledgments and L2 monitor remain compatible.
Consequently downstream counters named `l2prefetch*VBOP` represent UBOP in this experiment.
The original VBOP's own `bop_req` remains zero. No new global source enumeration is introduced.

## Comparisons at a shared clock

`prefetcher.bopCompare` records four counters for each comparison: `_comparable`, `_same`,
`_different`, `_unavailable`. For a comparison, `same + different = comparable`.
`unavailable` means the corresponding reference is disabled or not yet valid.

- `ubop_train_vs_{pbop,vbop}_student_*`: on UBOP training acceptance with its internal issue
  gate open, compare its chosen offset against the observer's current enabled student offset.
- `ubop_req_vs_{pbop,vbop}_student_*`: compare the actual accepted UBOP request offset against
  the observer's current enabled student offset **at emission time**.
- `ubop_req_vs_{pbop,vbop}_teacher_*`: compare against the currently installed teacher offset.
- `ubop_req_vs_{pbop,vbop}_choice_*`: compare against the observer's current effective choice,
  including teacher fallback and its internal issue gate, while ignoring its forced external disable.

When both students are enabled, `ubop_req_both_students_comparable` is split into
`ubop_req_same_both_students`, `ubop_req_same_only_pbop_student`,
`ubop_req_same_only_vbop_student`, and `ubop_req_same_neither_student`.

These are signed **numerical offset** comparisons, not proof of equal target PA, matching streams,
timely fills or performance benefit. Emission-time comparisons use the observers' live states;
they do not claim to recover their states at the earlier trigger of a buffered request.

## Optional ChiselDB observations

The standard ChiselDB generation/build/run controls must be enabled; ordinary performance
runs can use the counters above without collecting DBs. New observation tables set `basicDB = true`
(the table-generation gate in this repository); the global ChiselDB enable is still required.

- `BOPComparisonTable`: snapshots all three choices on any learner phase end, accepted training
  input, or actual UBOP emission. Each record includes teacher/committed-phase offsets,
  student validity/enable, effective offset/gate, training fire/address, actual sent offset/fire,
  and the three phase-end flags, at one common clock/STAMP.
- `pbopStudentPhaseTable`, `vbopStudentPhaseTable`, `ubopStudentPhaseTable`: capture every
  pool entry's valid/offset/confidence/current and previous coverage **before** phase clear or
  replacement, together with phase train count, teacher injection, best/worst/injection indices
  and selected enable. Existing per-engine `OffsetScoreTable` definitions are unchanged and
  retain their own original generation gates; enabling global ChiselDB alone does not enable them.

Fields are meaningful under their event flags: `trainAddr` on `trainFire`, `sentOffset` on
`sentFire`, and `teacherPhaseOffset` on `phaseEnd`. The P training address is PA; V/U use VA.
Comparison-table offsets are 16-bit signed values exported as two's-complement bit patterns.
Student-phase offsets use 7 bits for P and 10 bits for V/U; confidence is signed 2-bit.
Decode signed values before comparing negative offsets. Training-address snapshots can be large;
restrict any future DB run to the intended observation window.

No new SPEC or ablation simulation is launched as part of creating this branch. Compile and
elaboration results are recorded separately; runtime performance remains to be measured.

## Validation on 2026-11-12

- `XSCache.compile` passed with Scala 2.13.17 / Chisel 7.13.0 / Java 21.
- Prefetcher-only SystemVerilog generation passed with performance counters and ChiselDB enabled.
  Generated RTL contains independent P/V/U instances and all four new table writers.
- A second SystemVerilog generation passed with the DefaultConfig prefetcher combination
  (BOP, CDP and receiver), performance counters, Constantin enabled and ChiselDB disabled.
- Generated SQL for all four new tables executed successfully in an in-memory SQLite database;
  student pool schemas contain 8/4/4 entries for P/V/U.
- Static comparison against the only-BOP base confirmed unchanged P/V parameter blocks and
  exactly the old union's 109 distinct candidates, including scan order.

These are compile/connection/schema checks. A complete XiangShan emulator build, dynamic
assertion checks and workload performance measurements have not been run for this branch.
Local harness, logs and generated artifacts are saved in the sibling
`bop-union-observe-validation` directory; the temporary harness is not part of the source branch.

Code and this note were assisted by Codex (GPT-6; shell/Python/local source inspection).
