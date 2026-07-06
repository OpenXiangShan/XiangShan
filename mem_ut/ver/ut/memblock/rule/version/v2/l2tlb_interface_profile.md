# V2 L2TLB interface profile

## Semantic Boundary

The common L2TLB rule still applies: the mem_ut `L2TLB_agent` models the
upstream DTLB-to-L2TLB request and L2TLB-to-DTLB response path. It must not be
used as an L2Cache/PTW/memory downstream model.

## Current Status

V2 generated Verilog now exposes top-level `l2_tlb_req_*` request/response and
`l2_pmp_resp_*` ports. This migration plan does not complete V2 L2TLB agent
adaptation.

Observed top-level port family:

```text
build_memblock/rtl/MemBlockTop.sv
  l2_tlb_req_req_ready
  l2_tlb_req_req_valid
  l2_tlb_req_req_bits_vaddr
  l2_tlb_req_resp_valid
  l2_tlb_req_resp_bits_paddr_*
  l2_pmp_resp_ld/st/instr/mmio/atomic
```

## Required Follow-up

1. Locate the DTLB to L2TLB request/response path in generated Verilog.
2. Compare it with `mem_ut/ver/ut/memblock/tb/*L2tlb*_connect.sv`.
3. Update interface/xaction/driver/monitor only under a dedicated V2 DUT
   adaptation plan.
