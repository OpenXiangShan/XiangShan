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

## 20260811 最新顶层接口闭合基线

本次以当前 `build/rtl/MemBlock.sv` 对 `dut_inst.sv` 完成逐端口集合、方向和位宽复核。动态端口
集合比较结果为空；后续不得恢复下列已删除端口或历史位宽。

| 分类 | 当前 V2 RTL 事实 | UVM 适配结论 |
|---|---|---|
| redirect 输入 | 新增 `io_redirect_bits_isVlsException`；RTL 在该标志为 1 时把 raw `level` 的有效值压为 0。 | `redirect_agent` 的 connect/interface/xaction/driver/monitor 均已覆盖；payload、anchor、cancel 对账和 `rob_need_flush()` 使用 VLS effective-level 语义。 |
| backend MSI | 新增 input `io_ooo_to_mem_backendToTopBypass_msiAck`，并直通为 output `io_outer_msi_ack`。 | input 归 `backendToTopBypass_agent` 驱动，output 归 `other_ctrl_agent` 只读采样；不得反向驱动 `io_outer_msi_ack`。 |
| vector writeback | 删除 `io_mem_to_ooo_writebackVldu_{0,1}_bits_vdIdx`；保留 `vdIdxInField[2:0]`。 | 从 `dut_inst`、vector-WB connect/interface/xaction/monitor 删除旧字段；scalar-only `writebackVldu valid` fail-fast 不变。 |
| TopDown | 删除 output `lqEmpty`、`sqEmpty` 和 input `noUopsIssued`；新增 output `replayAllocate`、`sqFull`、`sbFull`。 | `dut_inst.sv` 仅保留新 output wire/端口连接；当前无 UVM consumer，不新增无消费者 agent。 |
| MSI payload 宽度 | `io_fromTopToBackend_msiInfo_bits` 与 `io_mem_to_ooo_topToBackendBypass_msiInfo_bits` 均为 `[11:0]`。 | 顶层 tie-off、control agent interface/xaction/monitor 和 X/Z 检查均为 12 位；不得保留 `[12:0]` 兼容字段。 |

当前默认 `io_fromTopToBackend_msiInfo_valid=0` 且 bits 为 0；本环境没有该 top-level MSI input 的
agent stimulus owner。top-to-backend MSI output 的既有 control monitor 仍只做 observation/XZ，
analysis producer 保持 deferred，因而位宽更新不应被误写为新的 interrupt/MSI 测试功能。

建议从仓库根目录使用以下动态范围命令检查端口集合，避免 `dut_inst.sv` 行号变化后遗漏 `.clock`：

```bash
comm -3 \
  <(sed -n '58,1394p' build/rtl/MemBlock.sv |
    perl -ne 'if (/^\s*(?:input|output)\b/) { s/,\s*$//; @f=split; print "$f[-1]\n" }' |
    sort -u) \
  <(sed -n '/^MemBlock U_MEMBLOCK (/,$p' mem_ut/ver/ut/memblock/tb/dut_inst.sv |
    perl -ne 'if (/^\s*\.([A-Za-z_][A-Za-z0-9_]*)\s*\(/) { print "$1\n" }' |
    sort -u)
```

本基线对应以下独立功能提交：`765c7f0c07`、`85201f2505`、`625cfb5885`。远端 VCS
compile/run 结果不在本节声称，仍以 `verified_status.md` 和 implementation review 的后续记录为准。

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
