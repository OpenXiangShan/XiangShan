# RM 跨页故障后的 TLB 查询误报分析

## 1. 问题概述

本记录对应 V2 `memblock_dispatch_real_smoke_vseq` 的 20 笔小规模回归。仿真在
`uid=16` 处产生两条 `UVM_ERROR`，错误来自 RM 的 TLB 只读查询，不是 RTL 报错：

```text
UVM_ERROR .../memblock_rm_readonly_api.sv(590) [RM_READONLY_API]
  tlb_entry_exact_or_range query miss: uid=16 vpn=0x5647ceb ...
UVM_ERROR .../memblock_rm.sv(764) [RM_LS_COMPARE]
  uid 16 byte 3 active translation exact/range entry unavailable
```

日志：

```text
mem_ut/ver/ut/memblock/sim/rm_sv39_small2_20260828/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_mmu_sv39_smoke_seed=666666_rtl.log
```

该次运行使用 `wave=off`，因此没有 FSDB 波形；这是 RM 查询错误，不能据此判定 RTL
问题。

## 2. 复现条件与证据

`uid=16` 是 4B Load，虚拟地址为 `0x5647ceaffd`，访问字节范围为：

```text
byte 0..2: VPN 0x5647cea
byte 3:    VPN 0x5647ceb
```

日志显示 DUT 只发出第一 VPN 的 L2TLB 请求：

```text
accept L2TLB token=12 vpn=0x5647cea ...
fault feedback uid=16 ... exception_vec=0x2020
```

`0x2020` 的架构主 cause 是 Load Page Fault（`0x2000`）；低位 `0x20` 是 V2
LoadUnit 将 AF sideband OR 到 raw exception vector 的结果。首个 VPN 已经产生确定的
翻译故障后，DUT 不会继续请求第二个 VPN。

## 3. 根因

`memblock_rm::observer_build_commit_item()` 原先对 `item.size_bytes` 的每一个字节都执行
`resolve_tlb_entry_key_for_rm()`。即使当前 TLB entry 已经给出 PF/AF/GPF 或权限故障，循环
仍继续处理后续字节。跨页访问的后续 VPN 没有对应 L2TLB request/entry，RM 便把“DUT 按故障
语义提前终止翻译”误判成“参考模型表项缺失”。

这不是通过放宽 query miss 或静默补一条 TLB entry 解决的问题；那样会把没有发生的 DUT
请求引入 RM 状态，污染后续地址和异常判断。

## 4. 最优修改方案

1. 在每次字节翻译后计算当前 entry 的 `entry_translation_fault`，包含：
   - PMA/TLB AF；
   - S1 PF/AF、S2 GPF/GAF；
   - 根据冻结 privilege/`MXR`/`SUM`/PTE 权限推导出的阶段权限故障。
2. 一旦该值为 1，立即结束字节循环；保留已经聚合的架构 fault bit，不再查询后续 VPN。
3. 只有在没有任何翻译故障且所有访问字节都有 PA 时，才调用 PMA/PMP post-TLB facade。
4. Load 的 LDA writeback 与 ROB terminal exception 比较使用独立的 raw/cause 诊断变量，避免
   第二次比较覆盖第一次比较的诊断值。

该方案保持 TLB/PMA/PMP 的“按实际请求建立、按冻结上下文读取”原则，不读取
`main_view` 异常字段，也不修改 RTL。

## 5. 已实施代码变更

- `mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv`
  - 翻译循环在确定故障处停止，避免跨页后续 VPN 的假 query miss。
- `mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_ls_core.sv`
  - 后续补丁将 Load 的两次架构异常比较拆分诊断变量；比较语义不变。

## 6. 验证要求

重新编译并运行相同 seed 的小规模用例，确认：

- `uid=16` 不再产生 `RM_READONLY_API` query miss；
- `UVM_ERROR=0`、`UVM_FATAL=0`；
- 跨页且首个页面故障的访问仍得到正确架构 cause；
- 再进入 10000 笔回归前，保留 RM trace 和最终日志路径。

