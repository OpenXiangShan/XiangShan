# CPU BUG: PTW D-Channel Error Is Ignored

## Status

Confirmed and repaired locally on 2026-09-07. The failing generated
`DefaultConfig` MemBlock RTL had complete RTL SHA-256
`e3250bd4594a3f5594b2fe5e215ddf16b89dd121498a72953b2500eecf61fcf8`;
the repaired RTL has complete SHA-256
`97b1339a74d458a48a1c58fad766a22cc9dac000cb297e303501311bf47d3b39`.

## Trigger

1. Install a valid Sv39 page-table mapping for a scalar load.
2. Let the PTW issue its root-page `Get` request.
3. Return a legal two-beat `GrantData` response with matching opcode, size,
   source, and data, but assert the TileLink `denied` bit.
4. Observe the PTW and scalar load completion.

The new `ptw-errors` scenario can select the failing response by walk depth and
also covers `corrupt`, Sv48, and scalar stores.

## Expected

A failed physical read of a page-table entry must terminate the walk as the
access-fault class corresponding to the original memory operation. The bad PTE
data must not be cached or used to continue translation, and the original
access must not reach DCache or Uncache.

For the reproducer, the scalar load should write back load-access-fault bit
`0x20`, with neither integer nor floating-point RF enable asserted.

## Actual

The denied root read was treated exactly like a successful PTE read. The PTW
issued all three Sv39 memory requests, translated the address, accessed DCache,
and returned normal data:

```text
MEMBLOCK_PTW_ERRORS_FAIL phase=sv39-load-root-denied-execution
reason=mismatched load writeback lane=0 rob=0 pdest=180
data=0xdcdbdad9d8d7d6d5 exception=0x0 rf_wen=1
expected_exception=0x20 expected_rf_wen=0 ptw=3
```

## Root Cause

`L2TLB.scala` unconditionally accepts D-channel responses and assembles
`mem.d.bits.data`, but does not consume either `mem.d.bits.denied` or
`mem.d.bits.corrupt`. The completed data is forwarded to PTW, LLPTW, HPTW, or
Bitmap and may be refilled into the page-table cache as if it were clean.

The walkers already have architectural access-fault state for PMP/PMA failures,
but the L2TLB-to-walker memory response boundary has no bus-error indication.
Consequently no existing access-fault path can distinguish bad PTE data.

## Impact

- A page-table memory bus denial or corruption can be silently converted into
  a valid translation when the returned data happens to encode valid PTEs.
- Arbitrary returned data can instead produce an unrelated page fault, guest
  page fault, or physical address.
- Bad response data can enter the page-table cache and affect later accesses.
- The problem applies to host and nested walks and to load/store instruction
  classes; it is not limited to the scalar Sv39 reproducer.

## Repair Requirements

The D-channel error must be accumulated across every beat of the PTW block
read, forwarded with the completed response to PTW/LLPTW/HPTW/Bitmap, and
converted into the existing access-fault state at the original walk level.
An errored block must not refill the page-table cache. The repaired RTL must
pass root/intermediate/leaf denied/corrupt cases for Sv39 and Sv48 scalar loads
and stores, followed by the existing translation regression.

## Repair Verification

The repair accumulates `denied`/`corrupt` across the complete TileLink block
response, forwards one access-fault indication to PTW, LLPTW, HPTW, and Bitmap,
terminates the corresponding walk, and suppresses both page-table-cache and
bitmap-cache refill from bad data.

The rebuilt model passed all 16 directed cases: eight stage-1, four isolated
G-stage, two fully nested, and two bitmap reads; loads/stores and
denied/corrupt each split 8/8. It consumed exactly 45 PTW requests in 1,894
cycles and issued no DCache or Uncache request for the faulting architectural
access. The existing translation matrix, 118-case translation-fault suite,
58-case permission suite, 36-case PBMT suite, Bare degenerations, and ten
superpage cases also passed on the same RTL hash.
