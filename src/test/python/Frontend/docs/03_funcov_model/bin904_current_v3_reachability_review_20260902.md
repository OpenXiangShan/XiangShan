# BIN-904 Current-V3 MainPipe Reachability Review

Date: 2026-09-02

## Scope And Provenance

This review concerns `BIN-904` only.  The reviewed standalone Verilator DUT
has implementation `1a32a9056d993233fa1bf3a394b16e8a762abf52`, V3 design
baseline `e5c70547f3a966accf20a4b065ec1d8e33443180`, and `DefaultConfig`.
The focused artifacts are retained under `data/runs/` and declare `BIN-904`
explicitly.  This document is review evidence, not HIT evidence.

BIN-904 requires a real MainPipe acceptance with all of these current-DUT
observations in one transaction:

1. `mainPipe.s0_fire=1`, FTQ `req(1).valid=1`, and WayLookup entry 1 valid;
2. at least one corresponding WayLookup entry reports MMIO/uncache;
3. `s0_realTwoFetchValid=0`; and
4. the following aggregate `toIfu` response has `info(1).valid=0`, with the
   same FTQ identity and no sampler/checker error.

The sampler retains those conditions unchanged.

## RTL And Environment Constraints

The literal cacheable-versus-uncache pair originally attempted across a 4 KiB
boundary cannot form an FTQ raw dual fetch: current `Ftq.scala` requires
`fetchReq(0).vPageNumber === fetchReq(1).vPageNumber` before setting
`req(1).valid`.  The Frontend PMA model also enforces a 4 KiB minimum NAPOT
region (`PMP_PMA_PLATFORM_GRAIN_BYTES=0x1000`).

On this RTL, `ICachePrefetchPipe` computes one `s1_isMmio` and one
`s1_itlbPbmt`, then copies each value to both WayLookup entries.  Thus a
natural same-page dual request does not provide independently configured
per-block PMA/PBMT attributes in the standalone environment.  `MainPipe`
still has the defensive OR reduction over its two entries, which is the
behavior BIN-904 intends to protect.

## Current-DUT Attempts

`ctrl_bin904_mixed_window_head981_20260902_02` used a cacheable-to-uncache
page-boundary stream.  It was checker-clean but did not form `req(1).valid`,
as expected from the FTQ same-page requirement.

`ctrl_bin904_samepage_uncache_head981_20260902_03` and `_04` used a static
same-page uncache mapping.  Both were checker-clean and unhit.  Their FTQ
evidence records `blocked_size` and `blocked_runahead`; static uncache
execution did not train a usable short dual candidate.

`ctrl_bin904_pma_transition_head981_20260902_09` used only normal testbench
interfaces: it first trained the existing 17+11 short-block loop on a
cacheable PMA mapping, then wrote the same PMA entry through
`write_pma_entry` (distributed CSR) as uncache and issued a backend redirect.
The cacheable stage reached `two_fetch_ftq_eligibility.eligible_dual` twice
(cycles 623 and 629).  The resulting waveform shows post-transition samples
with raw `req(1)`, WayLookup entry 1, and its MMIO bit high.  They did not
coincide with an accepting `mainPipe.s0_fire`; the IFU request subsequently
remained backpressured before an eligible aggregate-output check could occur.
The artifact is an exact-target pytest failure, not a passing coverage result.

The focused fixture remains in the tree as a strict `xfail` sentinel for this
review.  A future unexpected pass is an XPASS failure requiring evidence
review before any CSV change.

## Disposition

Keep `BIN-904` as `MODELED`; keep the strict denominator at `343`.  No
coverage status, CSV evidence, or backannotation is changed.  The experiments
do not establish a design bug or global RTL unreachability.  Closure requires
either a checker-clean current-DUT scenario where the full acceptance/output
transaction is observed, or a design-level decision that the current
per-block-attribute premise cannot be produced and the testpoint should be
re-specified without reusing the old HIT rule.
