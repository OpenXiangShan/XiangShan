# V2 DUT 接口基线

## 权威来源

V2 DUT 接口适配必须以生成后的 V2 Verilog 为权威来源：

```text
build/rtl/filelist.f
build/rtl/MemBlock.sv
```

Scala 源码只用于理解 `valid/ready`、index 和 bundle 语义，不能替代生成后 Verilog
作为接口基线。

## 当前状态

当前 worktree 已成功生成 V2 Verilog，`dut_inst.sv` 已按当前 `MemBlock` 顶层端口
闭合。后续重新生成 RTL 时仍需按本文基线重新检查 agent/interface/connect。

已观察到的生成后 DUT 顶层事实：

```text
build/rtl/MemBlock.sv
  module MemBlock
  io_outer_cpu_halt
  io_l2_tlb_req_*
  io_l2_pmp_resp_*
```

当前 profile 不存在 `build/rtl/MemBlockTop.sv`。接口检查和 filelist 均以
`build/rtl` 为唯一权威生成目录。

当前 halt/reset 控制连接状态：

```text
Generated V2 MemBlock: io_outer_cpu_halt
Current UVM connect/interface/xaction: io_outer_cpu_halt
Generated V2 MemBlock: io_reset_backend
Current UVM connect/interface/monitor: io_reset_backend
```

历史 `cpuWfi/io_outer_cpu_wfi` 命名不得重新作为 V2 当前接口使用。

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
