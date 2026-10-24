# CPU Bug: CMO `flushAfter` Does Not Cancel a Younger Atomic's DCache Side Effect

## Status

Confirmed RTL bug, open as of `43b971f2a`. The focused reproducer is
`make -C tests/memblock cmo-atomic-flush` (or
`UTMemBlock_example --test cmo-atomic-flush`). The default benchmark excludes
this scenario (`benchmark_order: null`); the real benchmark passes 85/85 and
`cmo-contracts` and `atomic-contracts` still pass.

## Summary

A `CBO.FLUSH` completes on line A while a younger `AMOADD.D` to line B is in
flight. The CMO's `flushAfter` redirect is applied at the CMO's ROB, which is
older than the atomic. The redirect reaches `AtomicsUnit`, but only clears
`atom_override_xtval`; it does not reset the atomic FSM or cancel the DCache
atomic request/response. The DCache therefore applies the younger atomic's
result. A later load of line B returns `old + operand` instead of the old
memory value, so the squashed atomic's memory side effect is architecturally
visible.

## Reproduction

From `XiangShan/tests/memblock`:

```sh
make cmo-atomic-flush
# or, after building:
LD_LIBRARY_PATH=$PWD/../../build/memblock/picker/UT_MemBlock/build:$PWD/../../build/memblock/picker/UT_MemBlock:$PWD/../../build/memblock/tools/picker/build/dependence/xcomm/lib \
  ../../build/memblock/picker/UT_MemBlock/build/UTMemBlock_example --test cmo-atomic-flush
```

Deterministic failure (same data and cycle on repeated runs):

```text
MEMBLOCK_CMO_ATOMIC_FLUSH_FAIL cycle=5224 phase=cmo-atomic-rollback-fresh
  reason=mismatched load writeback lane=1 rob=3 pdest=252
  data=0x5f5e6e6d7d7c8c8b exception=0x0 ...
  expected_address=0x80490018 expected_op=0x3 expected_pdest=252
  expected_data=0x4e4d4c4b4a494847 ...
  atomic_writebacks=0
```

The reference memory at `0x80490018` is `0x4e4d4c4b4a494847`; the AMOADD.D
operand is `0x1111222233334444`. The observed fresh-load data is exactly
`old + operand`, proving that the younger atomic updated the DCache line even
though the CMO `flushAfter` redirect flushed it.

Stimulus sequence:

1. warm CMO line `0x80470000` and issue `CBO.FLUSH`;
2. hold the CMO response for 1024 cycles and satisfy its clean-line Probe;
3. issue a younger `AMOADD.D` at `0x80490018`;
4. complete the CMO store, then apply the `flushAfter` redirect at the CMO's
   ROB (`rob=1`, `flush_itself=false`);
5. issue a fresh load at the atomic address (`rob=3`), which must observe the
   old memory.

The CMO's backing line is unchanged and the reference memory for the atomic
line remains old; the manager never writes the atomic line.

## Oracle and contract

`CBO.FLUSH` is ordered before subsequent memory operations. Its `flushAfter`
redirect must squash younger uops, including atomics. A younger atomic that is
flushed must not leave a read/write side effect in the data cache; a later load
must return the pre-atomic memory value. This is the same ordering contract
already checked for a younger scalar load by the `flushpipe-overlap` phase in
`cmo-contracts`.

The atomic's speculative old-value register writeback itself is not the
oracle; the reproducer checks the post-flush DCache/memory state. The failing
fresh load is an architectural load issued after the redirect.

## Root cause

`src/main/scala/xiangshan/mem/pipeline/AtomicsUnit.scala` handles a redirect
only with:

```scala
when (io.redirect.valid) {
  atom_override_xtval := false.B
}
```

It does not call `resetFSM()`, does not suppress `io.dcache.req`/`resp`, and
does not invalidate an already accepted DCache atomic. The DCache MainPipe
performs the atomic line update without a redirect/kill input, so the cache
side effect is committed before the backend can discard the atomic's result.
`MemBlock.scala` connects `atomicsUnit.io.redirect <> redirect` (the raw
redirect); the CMO's `flushAfter` redirect is visible to the unit, but the unit
continues the in-flight atomic.

The missing piece is an atomic kill/rollback path, or a guarantee that an
atomic cannot issue its DCache request while an older CMO/fence redirect is
still possible. Resetting only the `AtomicsUnit` FSM after the DCache request
is insufficient because the line update has already occurred.

## Impact

A younger atomic squashed by a CMO `flushAfter` (and potentially other
redirects that reach the atomic unit while its DCache request is in flight)
leaves its result in the DCache. Later loads or atomics can observe the stale
result. This is an architectural memory-ordering and isolation failure; it can
also expose a value that should have been discarded.

## Proposed fix

1. Give the atomic DCache path a kill/flush input and ensure a redirect that
   flushes the atomic prevents the line update or restores the old line.
2. Alternatively, make the atomic request non-speculative with respect to
   older CMO/fence flushes (e.g., gate the atomic issue on the older
   `flushAfter` resolution or on the atomic reaching the ROB commit point).
3. In `AtomicsUnit`, reset the FSM and drop `io.dcache.resp` on a redirect that
   flushes the atomic; this is necessary but not sufficient without (1)/(2).

## Post-fix verification

After an RTL fix:

- `--test cmo-atomic-flush` must print `MEMBLOCK_CMO_ATOMIC_FLUSH_PASS`; the
  fresh load must return `0x4e4d4c4b4a494847`.
- `cmo-contracts`, `atomic-contracts`, `dcache-coherence`, and the
  `dcache-release` CMO/atomic compositions must still pass.
- Re-run the directed benchmark (`make benchmark-tests`) and a multi-seed
  regression; the new scenario is intentionally excluded from the default
  benchmark while the bug is open.

## Provenance

- RTL and UT revision: `43b971f2a` (`XiangShan` checkout).
- Reproducer source: `tests/memblock/cpp/scenarios/maintenance_reset.inc`,
  `run_cmo_atomic_flush`; catalog entry `cmo-atomic-flush`.
- Observed failure artifact: `/tmp/run-cmoatomic-new2.log` (local run; the
  deterministic command above is authoritative).
- The default eight-scenario benchmark remains green because the reproducer is
  excluded with `benchmark_order: null`.
