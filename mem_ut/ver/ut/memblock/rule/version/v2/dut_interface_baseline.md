# V2 DUT interface baseline

## Authority

V2 DUT interface adaptation must use generated V2 Verilog as the authority:

```text
build_memblock/rtl/filelist.f
build_memblock/rtl/MemBlock.sv
build_memblock/rtl/MemBlockTop.sv
```

Scala source is only used to understand valid/ready, index and bundle semantics.

## Current Status

V2 Verilog has been generated successfully in this worktree. Full DUT
agent/interface adaptation is intentionally out of scope for the branch
migration plan.

Observed generated top-level facts:

```text
build_memblock/rtl/MemBlockTop.sv
  module MemBlockTop
  outer_cpu_halt
  l2_tlb_req_*
  l2_pmp_resp_*
```

Known follow-up mismatch example:

```text
Generated V2 top: outer_cpu_halt
Current UVM connect: cpuWfi / io_outer_cpu_wfi
```

This mismatch must be handled by the later V2 DUT adaptation plan, not by
silently changing generated V2 design RTL.

## Follow-up Checklist

Compare these testbench entry points against the generated Verilog:

```text
mem_ut/ver/ut/memblock/tb/top_tb.sv
mem_ut/ver/ut/memblock/tb/dut_inst.sv
mem_ut/ver/ut/memblock/tb/tc_if_connect.sv
mem_ut/ver/ut/memblock/tb/memblock_connect.sv
mem_ut/ver/ut/memblock/tb/*_agent_connect.sv
```

Every interface delta must be tracked in a later V2 DUT adaptation plan before
agent interface/xaction/driver/monitor changes are made.
