# 严格 PBMT=00 100k 用例的 STA0 Trigger 元数据 RTL 失败记录

## 结论

本次失败不是 `RM_LS_COMPARE`，也不是 DCache responder 收尾逻辑。严格模式下的
`INT_WB_STA0_TRIGGER_PROVENANCE` 在 `727.8ns` 报出后立即结束仿真；此前没有
`RM_LS_COMPARE` 报错。

该错误对应 V2 `StoreMisalignBuffer` 的已确认 metadata 传播缺陷：普通、cacheable、非
NC/MMIO、非 CBO 的跨 16B 非对齐 scalar store 经由 MAB 最终走 STA0 writeback 时，输出
`trigger=4'h0`，但没有 `exceptionVec[3]`。`4'h0` 是 `BreakpointExp`，无 trigger 的
正确编码是 `TriggerAction.None=4'hf`，所以这是 DUT 输出 metadata 不自洽，而不是 RM
期望或测试框架随机激励错误。

本任务按约束在确认 RTL 问题后停止：不修改 RTL，不通过关闭 trigger checker 掩盖问题，也不继续
以该 strict run 判断 RM 或 DCache global-stop 修复。

## 回归场景

```text
tc=basicTest
ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq
cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k
seed=666666
mode=rm_scalar_misalign_stop_tail_20260906
```

该 cfg 的关键约束如下：

- `MEMBLOCK_MAIN_TRANS_NUM=100000`；只产生 scalar integer Load/Store，CBO 权重为 0。
- `mPBMTE/hPBMTE=0`，S1/S2 PBMT 只允许 `00`；地址范围位于 V2 PMA 的 cacheable DDR。
- `MEMBLOCK_BOUNDARY_CROSS_16B_SAME_LINE_WT=25`，保留跨 16B 非对齐 store。
- `MEMBLOCK_CHECK_TRIGGER_EN` 未被 testcase 覆盖，因此取 `plus.sv` 的默认严格值 1；
  `default.cfg` 的值也为 1。

本次输出绑定到 `UID0`、`ROB=0/0`。对应 store 的 `main_va=0x00000000863abedf`、2B
访问范围为 `0x00000000863abedf..0x00000000863abee0`，跨越 16B 边界
`0x00000000863abef0`，因此进入 StoreMisalignBuffer。

## 报错点

当前回归日志：

```text
UVM_FATAL @ 727.800ns
dispatch_monitor_event_adapter.sv:768
[INT_WB_STA0_TRIGGER_PROVENANCE]
STA0 trigger=0 without breakpoint needs uncache/CBO provenance
```

adapter 在 UID 已绑定后检查：`trigger==0 && !exceptionVec[3]`，同时主表指令不是 CBO，且
`debug_is_mmio/debug_is_ncio=0/0`。这是严格输出 monitor 的预期检出点；它不读取 RM golden
数据，也不改变 DUT 行为。

## 波形证据

本次 rerun 使用 `wave=off`。由于测试名、cfg 和 seed 与下列 strict trigger-on 波形一致，且当前
日志在同一 `727.8ns` 触发同一 fatal，使用该已有 FSDB 复核具体 DUT payload：

```text
mem_ut/ver/ut/memblock/sim/trigger_gate_output_only_on_20260905/wave/
tc=basicTest_ts=memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq_
cfg=tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k_seed=666666_rtl_output_only_on.fsdb
```

同一笔 transaction 的时序如下：

```text
670.3ns  StoreUnit 向 MAB enqueue：MemTrigger action=4'hf，
          但 s1_in.trigger=4'h0，MAB enqueue trigger=4'h0。
685.3ns  MAB 发出第一个 split child；700.3ns 发出第二个 split child。
695.3ns / 710.3ns  两个 child 返回，child trigger 已重新计算为 4'hf。
720.3ns  MAB final writeback 经 STA0 输出 trigger=4'h0。
727.8ns  monitor 完成 UID 绑定，执行严格 provenance 检查并报 fatal。
```

| 波形字段 | 720.3ns 的值 | 结论 |
| --- | --- | --- |
| `io_mem_to_ooo_writebackSta_0_valid` | `1` | 有效 STA0 writeback。 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_trigger` | `0` | 错误表现为 `BreakpointExp`。 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3` | `0` | 没有 breakpoint exception。 |
| `io_mem_to_ooo_writebackSta_0_bits_debug_isMMIO/isNCIO` | `0/0` | 不是 MMIO 或 NCIO 例外。 |
| `io_mem_to_ooo_writebackSta_0_bits_uop_robIdx` | `0/0` | 对应 UID0。 |
| `inner__7` | `1` | 顶层 STA0 mux 选择 MAB producer。 |
| `_inner_storeMisalignBuffer_io_writeBack_valid` | `1` | MAB 写回有效。 |
| `_inner_otherStoutConnect_io_out_valid` | `0` | 不是 StoreQueue MMIO/CBO producer。 |
| `_inner_StoreUnit_0_io_stout_valid` | `0` | 不是普通 StoreUnit stout producer。 |

建议从同一 ROB/uop 顺序观察以下路径：

```text
top_tb.U_MEMBLOCK._inner_StoreUnit_0_io_misalign_enq_req_valid
top_tb.U_MEMBLOCK._inner_StoreUnit_0_io_misalign_enq_req_bits_uop_trigger
top_tb.U_MEMBLOCK.inner_storeMisalignBuffer.req_uop_trigger
top_tb.U_MEMBLOCK._inner_storeMisalignBuffer_io_writeBack_valid
top_tb.U_MEMBLOCK._inner_storeMisalignBuffer_io_writeBack_bits_uop_trigger
top_tb.U_MEMBLOCK.inner__7
top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_valid
top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_trigger
top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_uop_exceptionVec_3
top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_debug_isMMIO
top_tb.U_MEMBLOCK.io_mem_to_ooo_writebackSta_0_bits_debug_isNCIO
```

## 根因链路

`StoreUnit` 会把按 memory trigger 计算后的结果写入 `s1_out.uop.trigger`，但 MAB enqueue
使用的是更早的 `s1_in`：

```text
StoreUnit: s1_out.uop.trigger := s1_trigger_action
StoreUnit: io.misalign_enq.req.bits.fromLsPipelineBundle(s1_in)
StoreMisalignBuffer: io.writeBack.bits.uop := req.uop
MemBlock: MAB writeBack -> stOut(0) -> writebackSta_0
```

普通 scalar issue 输入的早期 `s1_in.uop.trigger` 可以是 0；无 trigger 命中时本应在 `s1_out`
阶段改为 `None=4'hf`。MAB 保存的却是前者，final writeback 又直接复用保存的 parent `req.uop`，
因此 child StoreUnit 后续重新计算的 metadata 不会修复 STA0 的值。

相关源码位置：

- `src/main/scala/xiangshan/Bundle.scala:755-767`：`TriggerAction` 编码。
- `src/main/scala/xiangshan/backend/fu/NewCSR/Debug.scala:253-273`：无命中默认 `None`。
- `src/main/scala/xiangshan/mem/pipeline/StoreUnit.scala:401-437`：计算后的 `s1_out` 与使用
  `s1_in` 的 MAB enqueue 并存。
- `src/main/scala/xiangshan/mem/lsqueue/StoreMisalignBuffer.scala:598-607`：final writeback 复用
  `req.uop`。
- `src/main/scala/xiangshan/mem/MemBlock.scala:1388-1396`：MAB 到 STA0 mux。

更完整的 V2 专项根因记录见
`AI_DOC/analysis/rtl/v2/flows/store_misalign_trigger_metadata_propagation.md`。

## 处理边界

- 未修改 RTL。
- 未修改 RM；本次 strict run 在 adapter fatal 前终止，不能用它评价 RM。
- 未把 `MEMBLOCK_CHECK_TRIGGER_EN` 置为 0 后重跑，因为这只会隐藏已经确认的输出 metadata 错误，
  不构成对该严格 testcase 的通过。
- DCache `pre_stop_a_snapshot` 修复没有在本次 run 中走到 global stop，因此仍需要在 RTL metadata
  问题解除或单独诊断关闭后另行验证。

## 独立复核

独立 subagent 已复核当前日志、matching FSDB、Scala 与生成 RTL，结论为确认的 V2 DUT
`StoreMisalignBuffer` trigger metadata 传播缺陷，不是 RM、L2TLB、UVM monitor 或测试激励错误。
复核同时确认 MAB producer、`trigger=0`、`exceptionVec[3]=0`、`MMIO/NCIO=0/0`、child 的
重新计算结果，以及 final parent metadata 回传链路均成立。复核未修改任何文件。
