# Scalar Load Optional Metadata Oracle False Positive

## Classification

This was a MemBlock UT oracle bug, not an identified RTL bug. No RTL file was
changed.

## Observation

While adding malformed-PTE tests, the `sv48-nonleaf-pbmt` case returned the
expected load page fault, suppressed integer and floating-point register
writes, and issued no DCache or Uncache data request. The writeback also carried
`debug.isNCIO=1`, because the load pipeline derives that debug classification
from the PBMT metadata returned with the faulting translation.

The test failed only because `LoadScoreboard` expected `debug.isNCIO=0` even
though the transaction did not specify an expectation for that optional field.
The top-level debug classification has no architectural correctness contract
requiring zero on an exceptional writeback, so this was not sufficient evidence
of a CPU defect.

## Root Cause

`LoadTransaction` and `PrefetchTransaction` represent `expected_debug_is_mmio`,
`expected_debug_is_ncio`, and `expected_debug_is_perf_cnt` as `optional<bool>`.
The scalar-load scoreboard incorrectly converted an absent value to `false`
with `value_or(false)` and then always compared it. Store and vector monitors
already preserved the intended optional semantics.

## Correction And Guard

The scalar-load scoreboard now stores these expectations as optionals and
compares a sideband only when the transaction explicitly constrains it. Tests
that own a stable sideband contract, including MMIO and NC tests, continue to
set the expectation and therefore remain strict. Fault tests still require the
exact exception, no RF write, one terminal completion, and no data-manager
request.

After the correction, all 57 `translation-faults` cases passed, including 26
fresh-environment Sv39/Sv48 and 26 Sv39x4/Sv48x4
invalid/reserved/PBMT/NAPOT PTE encodings.

The encoding oracle follows the RISC-V privileged architecture's
[virtual-address translation process](https://docs.riscv.org/reference/isa/priv/supervisor.html#sv32algorithm)
and the ratified [Svpbmt encoding rules](https://docs.riscv.org/reference/isa/priv/supervisor.html#svpbmt).
