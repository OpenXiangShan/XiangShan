# CPU Bug: Vector Segment Redirect Ignored

## Reproducer

On complete ordered RTL SHA-256
`a7ddd8577d0982b8e3a3581cf3f74873813008ed039146d06b04d6a308aa9173`,
the `vector-segment` test issued a legal two-field segment load, waited until
its first cold DCache request was accepted with a 128-cycle response delay,
then asserted a self-flushing redirect for ROB 83. The flushed instruction
still completed and produced this deterministic failure:

```text
MEMBLOCK_VECTOR_SEGMENT_FAIL cycle=304 phase=redirect-cancel
reason=unexpected vector memory writeback lane=0 rob=83 op=0x80 exception=0x0
replay=0 flush_pipe=0 trigger=0 vec_wen=1 pdest=104 vl=2 vstart=0 eew=3
writebacks=0 requests=1
```

The one already accepted refill is permitted to finish. The architectural
violation is the normal vector-register writeback from the flushed producer.

## Required Behavior

MemBlock's redirect contract uses `RobPtr.needFlush`: `flushItself` cancels the
matching ROB entry, and every younger entry is also canceled. A canceled
vector segment operation must not issue new TLB, DCache, or SBuffer requests,
must not produce a uop writeback, RS feedback, or exception report, and must
leave the unit able to accept an unflushed successor.

## Root Cause

`MemBlock.scala` connected the backend redirect to `vSegmentUnit.io.redirect`,
but `VSegmentUnit.scala` never referenced that input. Its uop queue, FSM,
manager requests, store pipeline, and registered writeback/feedback outputs
therefore continued normally after a flush. Other vector split, merge, and FOF
buffers apply `robIdx.needFlush(io.redirect)` at enqueue, resident-entry, and
dequeue/writeback boundaries.

## Repair

The segment unit now applies the same redirect contract at every externally
visible boundary:

- same-cycle flushed input uops are not enqueued;
- a resident flushed instruction clears its queue and returns the FSM to idle;
- new DTLB, DCache, SBuffer, exception, and feedback traffic is suppressed;
- the SBuffer pipeline and registered writeback/feedback outputs are killed;
- a matching buffered FOF fix-VL uop is canceled.

An already accepted memory response remains ready so the shared cache manager
can drain it; its data is discarded after the FSM cancellation.

## Validation Status

After fresh DefaultConfig elaboration and a full Picker rebuild, the complete
ordered RTL SHA-256 is
`e3250bd4594a3f5594b2fe5e215ddf16b89dd121498a72953b2500eecf61fcf8`.
The repaired `vector-segment` test passed its original load/store/addressing
checks plus both redirect phases:

```text
MEMBLOCK_VECTOR_SEGMENT_PASS cycle=397 fields=2 elements=2
segment_load_writebacks=2 segment_store_writebacks=2 addressed_modes=3
addressed_load_writebacks=12 addressed_store_writebacks=6
segment_lsq_allocations=0 redirect_cancellations=1
redirect_survivor_writebacks=2 fof_redirect_cancellations=1
rtl_sha256=e3250bd4594a3f5594b2fe5e215ddf16b89dd121498a72953b2500eecf61fcf8
```

ROB 83 now produces no stale result after the single accepted delayed refill,
and ROB 84 completes both successor fields. A separate FOF segment cancellation
produces neither its two data writebacks nor its fix-VL writeback. The existing
`trigger-contracts`, `vector-segment-fof`, `vector-load`, and
`misaligned-stores` controls also pass on the repaired model. Frozen runtime
hashes are recorded in `RESULTS.md` after the accompanying UT update.
