# V2 DUT 接口基线

## 权威来源

V2 DUT 接口适配必须以生成后的 V2 Verilog 为权威来源：

```text
build_memblock/rtl/filelist.f
build_memblock/rtl/MemBlock.sv
build_memblock/rtl/MemBlockTop.sv
```

Scala 源码只用于理解 `valid/ready`、index 和 bundle 语义，不能替代生成后 Verilog
作为接口基线。

## 当前状态

当前 worktree 已成功生成 V2 Verilog。完整 DUT agent/interface 适配不属于分支迁移
plan 的范围，应由后续 V2 DUT 适配 plan 单独处理。

已观察到的生成后顶层事实：

```text
build_memblock/rtl/MemBlockTop.sv
  module MemBlockTop
  outer_cpu_halt
  l2_tlb_req_*
  l2_pmp_resp_*
```

已知后续需要处理的不匹配示例：

```text
Generated V2 top: outer_cpu_halt
Current UVM connect: cpuWfi / io_outer_cpu_wfi
```

该不匹配必须在后续 V2 DUT 适配 plan 中处理，不能通过静默修改生成后的 V2 设计
RTL 来绕过。

## 后续检查清单

后续应将以下 testbench 入口与生成后 Verilog 对比：

```text
mem_ut/ver/ut/memblock/tb/top_tb.sv
mem_ut/ver/ut/memblock/tb/dut_inst.sv
mem_ut/ver/ut/memblock/tb/tc_if_connect.sv
mem_ut/ver/ut/memblock/tb/memblock_connect.sv
mem_ut/ver/ut/memblock/tb/*_agent_connect.sv
```

每一项 interface delta 都必须先记录到后续 V2 DUT 适配 plan，再修改 agent
interface、xaction、driver 或 monitor。
