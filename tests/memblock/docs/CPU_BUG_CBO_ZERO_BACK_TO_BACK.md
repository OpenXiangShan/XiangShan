# CPU Bug: Back-to-Back CBO.ZERO Loses a Store Completion

## Status

Confirmed RTL bug, fixed by `f0e5ba75b`; the current HEAD includes the fix and
the directed regression. The fix remains in the verified runtime used for the
current benchmark and regression snapshot.

## Symptom

Two independent `CBO.ZERO` instructions to different cache lines, committed
back-to-back before the first Sbuffer line flush completes, lose one
architectural store completion. The StoreQueue/ROB does not drain, so a later
`io_wfi_safe` cannot become true and younger work remains blocked. A single
`CBO.ZERO` and two separately drained `CBO.ZERO`s both complete normally.

The observable failure from the MemBlock UT is:

```text
MEMBLOCK_CBO_ZERO_CONTRACTS_FAIL cycle=33333 phase=back-to-back-zero
reason=timed out waiting for scalar store writebacks sq=3/6
store_writebacks=5 sbuffer_empty=0 store_mmio_valid=0
```

One of the back-to-back completions was lost and the StoreQueue did not drain
after 16384/32768 cycles.

## Deterministic Reproducer

The directed case is in
[run_cbo_zero_contracts](../cpp/scenarios/maintenance_reset.inc). It first
executes one `CBO.ZERO` and a sequential two-`CBO.ZERO` control, then commits
two `CBO.ZERO`s to distinct nonzero lines before the first drains, followed by
an ordinary scalar store on a third line. The following store exercises the
prefix-FIFO lane behind a blocked zero completion.

The pre-fix run uses the current C++ harness with `LD_LIBRARY_PATH` selecting
the frozen model built from RTL revision
`ecae1b52b36634ccde561d2a1ca00b53a5f5bb40` (the only later change to this tree
before the fix was documentation):

```sh
cd XiangShan
LD_LIBRARY_PATH="$PWD/build/memblock/current-docs-validation/runtime:$PWD/build/memblock/tools/picker/build/dependence/xcomm/lib" \
  build/memblock/picker/UT_MemBlock/build/UTMemBlock_example \
  --test cbo-zero-contracts
```

After the fix, using the rebuilt model:

```sh
cd XiangShan
LD_LIBRARY_PATH="$PWD/build/memblock/picker/UT_MemBlock" \
  build/memblock/picker/UT_MemBlock/build/UTMemBlock_example \
  --test cbo-zero-contracts
```

Post-fix result:

```text
MEMBLOCK_CBO_ZERO_CONTRACTS_PASS cycle=676 cbo_zero_line=1
sequential_zero=2 back_to_back_zero=2 following_store=1 readback=1
dcache_requests=6 uncache_requests=0 dcache_request_stalls=1
dcache_response_delays=18
```

## Oracle and Contract

`CBO.ZERO` is a legal cache-block-management store-class instruction. Each
instruction must produce exactly one store/ROB completion, exactly once, and
must not require software serialization against another independent
`CBO.ZERO`. Two instructions to different lines, both at the ROB head in
architectural order, must both retire and must leave the StoreQueue able to
drain. The UT owns the two lines and the reference image independently; it
checks both store completions, normalized SQ dequeue, and zero bytes for both
lines after the operations drain.

The sequential control in the same scenario proves that the harness can
complete two independent `CBO.ZERO`s, so the failure is not a missing model
completion path or a reference-state error.

## Root Cause

[StoreQueue.scala](../../../src/main/scala/xiangshan/mem/lsqueue/StoreQueue.scala)
has one `CBO.ZERO` completion slot:

- `isCboZeroToSbVec` detects Sbuffer `wline` zero fires (line 996).
- `cboZeroUop` and `cboZeroSqIdx` are single `RegEnable` registers overwritten
  on every fire (lines 1003-1004).
- `cboZeroValid`/`cboZeroWaitFlushSb` represent one pending completion
  (lines 1005-1006).
- `io.cboZeroStout` emits one writeback (line 1089) and completes only
  `cboZeroSqIdx` (line 1104).
- The assertion at line 1008 rejects only two same-cycle fires. It does not
  reject a second fire on the next cycle while the first completion is still
  pending.

The Sbuffer has two input ports and can accept a second `CBO.ZERO` while the
first line is draining. The second fire overwrites `cboZeroUop`/`cboZeroSqIdx`;
the eventual single `cboZeroStout` completes the second SQ entry, while the
first remains allocated forever. The sequential control avoids the window, and
the prior single-`CBO.ZERO` tests never exercised it.

## Impact

Back-to-back cacheable `CBO.ZERO` can hang the StoreQueue/ROB, prevent the WFI
safe condition, and indefinitely block younger stores. The data path may zero
both lines, so a readback alone is not sufficient: the missing completion is
the architectural failure.

## Fix

At the StoreQueue-to-Sbuffer boundary, admit at most one `CBO.ZERO` per cycle
and hold later zero entries while a completion is pending. Non-`CBO` stores
behind a blocked zero lane are held by prefix backpressure; they do not bypass
the data-buffer prefix FIFO. The change is at
[StoreQueue.scala](../../../src/main/scala/xiangshan/mem/lsqueue/StoreQueue.scala)
lines 1340-1365:

- build `cboZeroDeqCandidate` for each Sbuffer port;
- use `PriorityEncoder` to select one zero candidate per cycle;
- block a candidate while `cboZeroValid` is set or while another zero
  candidate has priority;
- propagate the block as a prefix across `DatamoduleResultBuffer` lanes, so a
  following ordinary store cannot assert `deq(1).ready` while a blocked
  `CBO.ZERO` occupies `deq(0)`;
- drive `io.sbuffer(i).valid` and `dataBuffer.io.deq(i).ready` from the
  prefix block condition.

This serializes `CBO.ZERO` completion and keeps the data-buffer prefix
contract; ordinary stores behind a blocked zero wait rather than bypassing it.

## Post-Fix Verification

- `--test cbo-zero-contracts` passes with `sequential_zero=2
  back_to_back_zero=2 following_store=1` and no StoreQueue hang.
- `make -C tests/memblock unit` passes (284 tests).
- `make -C tests/memblock check-rtl` passes after regenerating RTL and
  re-preparing the model.
- The full frozen-runtime benchmark passes 85/85 scenarios (377.7 s,
  `build/memblock/agent-campaign-16/benchmark/results.json`), including the
  expanded `cbo-zero-contracts` entry.
- A six-seed finite coverage campaign (seeds 101..106) at 4608 actions per seed
  passes with the fixed runtime
  (`build/memblock/agent-campaign-12/coverage6/results.json`).
- A four-seed corner campaign (seeds 101..104) at 4608 actions per seed
  passes in 1985.0 s (`build/memblock/agent-campaign-12/corner4/results.json`).
- An 8-seed spec confidence run at 16384 actions per seed passes
  (`build/memblock/agent-campaign-13/spec-8x16384.json`).
- The configured 8-seed, 16384-action coverage regression passes
  (`build/memblock/agent-campaign-14/regression-coverage-8x16384.json`).
- See [RESULTS.md](RESULTS.md) for the current snapshot.

## Provenance

- Pre-fix RTL: `ecae1b52b36634ccde561d2a1ca00b53a5f5bb40`; no RTL changes
  between that revision and the pre-fix documentation HEAD `d504ba3ca`.
- RTL regeneration:
  `make verilog CONFIG=DefaultConfig NUM_CORES=1 ISSUE=E.b JVM_XMX=40G`.
- Model rebuild:
  `make -C tests/memblock prepare-rtl picker-harness JOBS=16` after forcing
  the prepared RTL timestamp, because the Picker targets do not encode the
  `build/rtl` source dependency.
- Fixed qualification runtime: `build/memblock/agent-campaign-16/runtime`.
- The reproducer uses the UT-owned line contents and completion/queue
  observations; no internal DUT signal supplies an expected value.
