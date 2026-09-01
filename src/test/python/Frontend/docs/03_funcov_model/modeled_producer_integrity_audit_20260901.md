# MODELED Producer Gap Audit

Date: 2026-09-01

## Scope And Provenance

This audit covers the fixed 343-leaf canonical @Jiabowen IFU denominator at
verification baseline `8fbed1908457efec7ea2453ca8271aea7fa2dc57`. The active
DUT remains the clean current-V3 build with implementation
`1a32a9056d993233fa1bf3a394b16e8a762abf52`, design baseline
`e5c70547f3a966accf20a4b065ec1d8e33443180`, and DUT SHA-256
`3269730cddcd752d4e9dff6cad3c7d4f4248ff14de510c66b613f5a271cfa395`.

The audit inventories whether each leaf marked `MODELED` already has a
bin-specific runtime producer. This is a closure-readiness check, not the
definition of `MODELED`: `MODELED` means the testpoint scenario, condition,
checkpoint, observation, and coverage mapping are represented. A missing
bin-specific producer therefore remains a work item and does not by itself
make the model or its mapped signals nonexistent. No DUT run, HIT promotion,
denominator change, or historical-artifact rewrite is part of this checkpoint.

## Result

The starting state was `300 HIT / 41 MODELED / 1 PARTIAL / 1 BLOCKED`.
After the BIN-1104 producer addition and the separate BIN-1004 design
disposition, 31 of the 40 MODELED leaves have a bin-specific executable
producer. Static review also confirmed that these apparently dynamic cases
are genuine producers:

- BIN-904 is selected through the MainPipe upstream-invariant transaction.
- BIN-949 and BIN-995 are emitted by the redirect-priority loop.
- InstrUncache leaves 036 and 037 are emitted by the TL user-attribute loop.

BIN-1104 / `instruncache_leaf_011` was the only missing producer in the
InstrUncache sampler. Commit `49b75a57a` adds a strict cross-cycle producer
that requires second-beat A fire before redirect, natural second-beat D and
InstrUncache response after redirect, resending clear, suppression of the old
FTQ/PC identity, and a distinct clean recovery identity. Backend and checker
redirect positives and leak/early-redirect/missing-D/timeout/reset negatives
pass in model tests. BIN-1104 remains `MODELED` until current-provenance,
explicit exact-target DUT evidence passes.

Nine generic owner leaves do not yet have a bin-specific runtime producer:
BIN-900, BIN-921, BIN-922, BIN-927, BIN-928, BIN-951, BIN-953, BIN-955, and
BIN-956. They remain `MODELED`; this AST producer audit found no signal-removal
evidence that would justify changing their mapping status. Their checked
generic owner-event path remains available, while exact-target DUT promotion
still requires a concrete producer, explicit target, passing checker, and
current-provenance artifact.

BIN-1004 is now `BLOCKED`, not nominally MODELED. Current RTL compares
FrontendTrigger PC/configuration only, ties `data` to zero, and does not use
`pds` in matching; the generated DUT optimizes the required semantic probes.
Closure requires an RTL/design disposition and cannot be manufactured by an
ordinary directed test.

The resulting fixed-denominator status is:

`300 HIT / 40 MODELED / 1 PARTIAL / 2 BLOCKED = 343`

The HIT ratio remains `300 / 343 = 87.46%`. Global denominator `879` remains
unchanged.

## Audit Contract

The schema contract discovers runtime producers from executable sampler AST,
including literal `recorder.mark` keys, owner source rules, dynamic owner
loops, conditional InstrUncache leaves, and the attribute-loop leaves. It
keeps the nine known gaps explicit without using that inventory to rewrite
testpoint status. `BLOCKED` owner leaves remain rejected by the generic event
handler so a bare event cannot manufacture a HIT.
