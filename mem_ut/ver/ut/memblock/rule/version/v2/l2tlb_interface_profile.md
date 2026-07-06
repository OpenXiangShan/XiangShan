# V2 L2TLB interface profile

## Semantic Boundary

The common L2TLB rule still applies: the mem_ut `L2TLB_agent` models the
upstream DTLB-to-L2TLB request and L2TLB-to-DTLB response path. It must not be
used as an L2Cache/PTW/memory downstream model.

## Current Status

V2 generated Verilog is required before the exact request/response fields,
hierarchy path and takeover wiring can be finalized. This migration plan does
not complete V2 L2TLB agent adaptation.

## Required Follow-up

After V2 RTL generation:

1. Locate the DTLB to L2TLB request/response path in generated Verilog.
2. Compare it with `mem_ut/ver/ut/memblock/tb/*L2tlb*_connect.sv`.
3. Update interface/xaction/driver/monitor only under a dedicated V2 DUT
   adaptation plan.
