# BIN-973 Current-Harness Reachability Review

Date: 2026-09-02

## Scope And Provenance

This review concerns `BIN-973` only. Both artifacts use the current
Verilator DUT with implementation `1a32a9056d993233fa1bf3a394b16e8a762abf52`,
design baseline `e5c70547f3a966accf20a4b065ec1d8e33443180`, `DefaultConfig`,
and a valid build manifest. The signal-contract SHA-256 is
`0d285e37d4e3dd814981b3bb5ccbdb7bc7708310750cf6a1f31b6853536be831`.

The sampler now records two diagnostics without invoking an owner-leaf mark:

- `ifu_predchecker_taken_jalr_form_observation`: an actual
  `branchType=3 && predTaken=1` observation, including any co-resident
  PredChecker fault.
- `ifu_predchecker_correct_jalr_form_candidate`: the stricter subset with no
  PredChecker fault in the request. Only this subset contributes to the
  existing BIN-973 predicate.

Unit tests prove that a partial form set and a taken form co-resident with a
fault remain diagnostics, not coverage evidence. The focused tests pass
(`3 passed`); the complete compact sampler test file also passes (`53 passed`).

## Required Event

BIN-973 requires one current-DUT run to observe all of the following at the
registered PredChecker request boundary:

1. an RVI/RVC ordinary JALR (`rasAction=0`), CALL (`hasPush`), and RET
   (`hasPop`), each with `predTaken=1`;
2. no fault in the request that would invalidate the checked PredChecker
   transaction; and
3. the normal checker/monitor pass gates and explicit `BIN-973` target.

The diagnostic events are deliberately weaker than this requirement and are
not backannotation inputs.

## Current-DUT Results

`ctrl_bin973_rawform_head981_20260902_05` used the existing isolated-form
program. It completed with pytest pass, checker pass, and
`monitor.error_count=0`; the trace contains 192 indirect jumps. The exact
target was unhit. No raw taken JALR form and no no-fault candidate was
observed. The sampler instead recorded 145 `BIN-890` JALR-not-taken events
and two `BIN-891` RET-not-taken events.

`ctrl_bin973_head_aligned_head981_20260902_06` used the new independent
head-aligned program. It puts CALL, RET, and plain JALR at 64-byte block
heads, avoids a predecessor CFI in each form's block, and raises training to
96 iterations per form. This run also completed with pytest/checker pass and
`monitor.error_count=0`; it executed 480 indirect jumps. Again the exact
target was unhit, with no raw taken form and no no-fault candidate. It
recorded 385 JALR-not-taken events and one RET-not-taken event.

Both artifacts are retained under `data/runs/` and carry exact inputs,
sampler/contract hashes, waveform, code-coverage, and run provenance.

## Disposition

`BIN-973` remains `MODELED`. No denominator, status, or CSV evidence is
changed by this review.

The experiments rule out the previous tail-position layout and scheduling
seed as explanations in this trace-driven environment. They do not prove that
the RTL is globally unreachable: the runs show many architecturally executed
indirect jumps while the PredChecker request never exposes a taken indirect
form. Closing the point requires either a current-DUT run with a real BPU
training/resolve producer that yields the required request signal, or a
design/verification-environment review explaining why that producer is not
available to the standalone trace harness. Neither review evidence nor
diagnostic observations may be converted into a HIT.
