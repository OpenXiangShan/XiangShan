# V3 L2TLB interface profile

## Semantic Boundary

The V3 mem_ut L2TLB agent models the upstream DTLB-to-L2TLB request and
L2TLB-to-DTLB response path. It is not an L2Cache/PTW/memory downstream model.

## Recorded Baseline

The recorded V3 mem_ut baseline is:

```text
1f96d06acbd75f00d619885ca27155810f72d922
```

Use generated V3 Verilog and the common L2TLB rule to refresh this profile if
V3 DUT interfaces change.
