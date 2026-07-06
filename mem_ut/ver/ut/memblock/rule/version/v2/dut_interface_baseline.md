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

At profile creation time, V2 Verilog had not been generated yet in this worktree.
Full DUT agent/interface adaptation is intentionally out of scope for the
branch migration plan.

## Follow-up Checklist

After V2 RTL generation succeeds, compare these testbench entry points against
the generated Verilog:

```text
mem_ut/ver/ut/memblock/tb/top_tb.sv
mem_ut/ver/ut/memblock/tb/dut_inst.sv
mem_ut/ver/ut/memblock/tb/tc_if_connect.sv
mem_ut/ver/ut/memblock/tb/memblock_connect.sv
mem_ut/ver/ut/memblock/tb/*_agent_connect.sv
```

Every interface delta must be tracked in a later V2 DUT adaptation plan before
agent interface/xaction/driver/monitor changes are made.
