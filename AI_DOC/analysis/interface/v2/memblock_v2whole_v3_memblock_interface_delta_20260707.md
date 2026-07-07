# V2 整核 MemBlock 与 V3 MemBlock 顶层接口差异分析

## 1. 分析目的

本文按用户指定的 V2 整核生成 Verilog 重新分析 V2/V3 `MemBlock` 顶层接口差异，并重新判断当前
`mem_ut/ver/ut/memblock/tb/dut_inst.sv` 是否可作为 V2 接口适配基准。

本轮分析只生成接口差异文档，不修改 RTL 或 UVM 源码。

## 2. 本轮基准

| 类型 | 路径 | 说明 |
| --- | --- | --- |
| V2 整核 RTL 基准 | `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/build/rtl/MemBlock.sv` | 用户指定的 V2 整核生成 Verilog，本轮以它为 V2 权威基准 |
| V3 RTL 基准 | `/nfs/home/lixiangrui/work/memblock_ut/XiangShan/build_memblock/rtl/MemBlock.sv` | 当前可找到的 V3 memblock 生成 Verilog |
| 当前 DUT 实例 | `mem_ut/ver/ut/memblock/tb/dut_inst.sv` | 当前 `mem_ut` 测试环境实例化 `MemBlock U_MEMBLOCK` 的端口连接 |

当前 V3 worktree 中未找到：

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan/build/rtl/MemBlock.sv
```

因此本文使用 V2 整核 `build/rtl/MemBlock.sv` 与 V3 `build_memblock/rtl/MemBlock.sv` 对比。该基准与上一份基于
V2 `build_memblock/rtl/MemBlock.sv` 的分析不同，不能混用统计结论。

当前 `dut_inst.sv` 第 1364 行仍是：

```systemverilog
MemBlock U_MEMBLOCK (
```

所以 `dut_inst.sv` 的端口统计仍表示当前测试环境连接到 `MemBlock` 的实例端口。

## 3. 总体统计

| 项目 | 数量 | 说明 |
| --- | ---: | --- |
| V2 整核 `MemBlock` 顶层端口 | 1334 | 来自 `build/rtl/MemBlock.sv` |
| V3 `MemBlock` 顶层端口 | 1687 | 来自 V3 `build_memblock/rtl/MemBlock.sv` |
| 当前 `dut_inst.sv` 连接端口 | 1393 | 当前 UVM 环境实例连接端口 |
| V2/V3 同名端口 | 766 | 名字相同，不代表位宽相同 |
| V2-only 端口 | 568 | V2 整核有、V3 没有 |
| V3-only 端口 | 921 | V3 有、V2 整核没有 |
| 同名但方向/位宽不同 | 25 | 方向差异 0，位宽差异 25 |
| `dut_inst.sv` 中不存在于 V3 的端口 | 0 | 当前 `dut_inst.sv` 仍完全是 V3 `MemBlock` 子集 |
| V3 中未被 `dut_inst.sv` 连接的端口 | 294 | 当前 V3 baseline 本身未完整接出 |
| `dut_inst.sv` 中不存在于 V2 整核的端口 | 648 | 这些连接会直接阻塞 V2 整核 RTL 编译 |
| V2 整核中未被 `dut_inst.sv` 连接的端口 | 589 | 后续 V2 适配必须分类处理 |
| `dut_inst.sv` 与 V2/V3 同名且方向/位宽完全一致端口 | 720 | 可作为跨 V2/V3 直接复用候选 |

方向分布：

| 对象 | input | output | inout |
| --- | ---: | ---: | ---: |
| V2 整核 `MemBlock` | 748 | 586 | 0 |
| V3 `MemBlock` | 708 | 979 | 0 |

关键变化：

- 使用 V2 整核 `build/rtl/MemBlock.sv` 后，V2 顶层端口数是 1334，而不是上一轮 V2 `build_memblock/rtl/MemBlock.sv` 的 6751。
- 这说明上一轮 6751 个端口更可能来自 V2 `build_memblock` 生成入口或配置形态差异，不能继续作为当前“整核 V2 RTL”适配的主基准。
- 当前 `dut_inst.sv` 仍是 V3 形态子集：1393 个实例端口全部存在于 V3，但有 648 个不存在于 V2 整核。

## 4. V2 整核与 V3 的真实接口差异

### 4.1 V2-only 端口

V2 整核相比 V3 多出的 568 个端口主要集中在以下类型：

| 类型 | 数量 | 方向分布 | 影响判断 |
| --- | ---: | --- | --- |
| `io_ooo_to_mem_enqLsq_req_*` | 168 | input 168 | 重要。LSQ 入队请求形态不同，属于驱动侧核心输入。 |
| `io_ooo_to_mem_issueVldu_*` | 75 | input 73、output 2 | 重要。V2 vector load issue 仍是独立接口形态。 |
| `io_mem_to_ooo_writebackVldu_*` | 73 | output 73 | 重要。V2 vector load writeback 与 V3 vector writeback 不是同一组端口。 |
| `io_ooo_to_mem_issueLda_*` | 72 | input 69、output 3 | 重要。V2 load issue 仍使用 Lda 独立接口。 |
| `io_mem_to_ooo_writebackLda_*` | 62 | output 62 | 重要。V2 load writeback 独立于 V3 `intWriteback`。 |
| `io_mem_to_ooo_writebackSta_*` | 43 | output 43 | 重要。store address writeback 需要单独接入或豁免。 |
| `io_ooo_to_mem_issueSta_*` | 24 | input 22、output 2 | 重要。store address issue 需要单独适配。 |
| `io_ooo_to_mem_issueStd_*` | 16 | input 14、output 2 | 重要。store data issue 需要单独适配。 |
| `io_mem_to_ooo_lsqio_*` | 8 | output 8 | 重要。LSQ 状态和 MMIO 反馈输出。 |
| `io_mem_to_ooo_vstuIqFeedback_*` | 6 | output 6 | 重要。V2 vector store issue feedback 的额外字段。 |
| `io_mem_to_ooo_writebackStd_*` | 4 | output 4 | 重要。store data writeback，与 `issueStd` 成对。 |
| `io_ooo_to_mem_lsqio_*` | 3 | input 3 | 重要。V2 LSQ 控制侧输入，例如 pending/scommit 类状态。 |
| CSR control | 7 | input 7 | 重要。存在 V2/V3 命名风格差异。 |
| 其他控制 | 3 | input 3 | 包含 `isStoreException`、`sfence_bits_flushPipe`、`tlbCsr_priv_debug` 等 V2 控制字段。 |
| 其他 V2-only 顶层状态/旁路 | 3 | input/output 混合 | 包含少量 backend bypass 和顶层状态字段，逐端口见 detail 文档。 |
| TileLink/auto | 1 | output 1 | 需要确认是否为外部总线补充字段。 |

V2-only 端口数量比上一轮 6751 基准小很多，但剩余差异仍是 LSQ、issue、writeback、CSR 等功能接口，不是纯 debug 差异。

### 4.2 V3-only 端口

V3 相比 V2 整核多出的 921 个端口主要集中在以下类型：

| 类型 | 数量 | 方向分布 | 影响判断 |
| --- | ---: | --- | --- |
| `io_mem_to_ooo_intWriteback_*` | 158 | output 158 | V3 integer writeback 形态，V2 整核无同名端口。 |
| `io_mem_to_ooo_vecWriteback_*` | 152 | input 2、output 150 | V3 vector writeback 形态，V2 使用 Vldu writeback 等旧形态。 |
| `io_ooo_to_mem_vecIssue_*` | 127 | input 125、output 2 | V3 vector issue 形态，V2 使用 issueVldu 等接口。 |
| `io_ooo_to_mem_intIssue_*` | 126 | input 119、output 7 | V3 integer issue 形态，V2 使用 issueLda/Sta/Std 等接口。 |
| `io_mem_to_ooo_lsTopdownInfo_*` | 24 | output 24 | V3 更完整的 load/store topdown 观察信息。 |
| `io_mem_to_ooo_vlduIqFeedback_*` | 20 | output 20 | V3 vector load issue feedback 字段集合变化。 |
| `io_mem_to_ooo_mdpTrain_*` | 14 | output 14 | V3 内存依赖预测训练反馈。 |
| `io_mem_to_ooo_staIqFeedback_*` | 12 | output 12 | V3 store address issue feedback 字段集合变化。 |
| `io_mem_to_ooo_updateLFST_*` | 10 | output 10 | V3 StoreSet/LFST 更新反馈。 |
| `io_mem_to_ooo_wakeup_*` | 9 | output 9 | V3 wakeup 字段扩展。 |
| `io_mem_to_ooo_memoryViolation_*` | 6 | output 6 | V3 memory violation 字段扩展。 |
| `io_mem_to_ooo_vstuIqFeedback_*` | 6 | output 6 | V3 vector store issue feedback 字段集合变化。 |
| debug 相关 | 67 | output 67 | V3 debug/replay cause 观察信号。 |
| debug topdown 相关 | 7 | input 2、output 5 | V3 顶层 debug/topdown 交互字段。 |
| L2TLB/PMP 相关 `io_l2_*` | 48 | input 12、output 36 | V3 与 V2 整核在 L2TLB/PMP 请求/响应字段上存在差异。 |
| TileLink/auto | 46 | input 24、output 22 | V3 TileLink/user 字段和 V2 整核不同。 |
| `io_ooo_to_mem_enqLsq_resp_*` | 32 | output 32 | V3 LSQ enqueue response 展开方式不同。 |
| `io_ooo_to_mem_enqLsq_req_*` | 20 | input 20 | V3 仍有部分 V2 不存在的 LSQ request 字段。 |
| CSR control | 11 | input 11 | CSR control 命名和字段集合存在差异。 |
| 其他 V3-only 顶层状态/反馈 | 26 | input/output 混合 | 包含 LSQ 状态、`fetch_to_mem`、`memInfo`、`enqLsq_needAlloc`、backend bypass、hart/perf 和 CPU halt/WFI 类字段，逐端口见 detail 文档。 |

这说明 V2/V3 的核心差异不是端口数量单边膨胀，而是 issue/writeback 接口组织方式发生变化：V2 更偏 `issueLda/Sta/Std/Vldu`、`writebackLda/Sta/Std/Vldu` 拆分，V3 更偏 `intIssue/vecIssue`、`intWriteback/vecWriteback` 聚合。

## 5. 当前 `dut_inst.sv` 与 V2 整核的关系

当前 `dut_inst.sv` 有 648 个端口不存在于 V2 整核 `MemBlock`。这些端口如果不做 V2/V3 宏隔离或替换，会直接造成 VCS 端口不存在错误。

主要类型如下：

| 类型 | 数量 | 方向分布 | 说明 |
| --- | ---: | --- | --- |
| `io_mem_to_ooo_vecWriteback_*` | 142 | input 2、output 140 | V3 vector writeback 连接，V2 整核没有同名端口。 |
| `io_ooo_to_mem_vecIssue_*` | 127 | input 125、output 2 | V3 vector issue 连接，V2 整核使用旧 issue 拆分。 |
| `io_ooo_to_mem_intIssue_*` | 126 | input 119、output 7 | V3 integer issue 连接，V2 整核无同名端口。 |
| TileLink/auto | 46 | input 24、output 22 | V3 TileLink/user 字段与 V2 整核不同。 |
| `io_ooo_to_mem_enqLsq_resp_*` | 32 | output 32 | V3 LSQ enqueue response 字段。 |
| `io_mem_to_ooo_lsTopdownInfo_*` | 24 | output 24 | 当前 `dut_inst.sv` 已接 V3 topdown 观察字段，V2 整核没有同名端口。 |
| 其他 V3-only `io_*` | 23 | input 5、output 18 | 包含 fetch、hart、outer/top 状态等 V3 顶层字段。 |
| `io_ooo_to_mem_enqLsq_req_*` | 20 | input 20 | V3 LSQ request 字段。 |
| `io_mem_to_ooo_vlduIqFeedback_*` | 20 | output 20 | 当前 `dut_inst.sv` 已接 V3 vector load issue feedback 扩展字段。 |
| `io_mem_to_ooo_mdpTrain_*` | 13 | output 13 | 当前 `dut_inst.sv` 已接 V3 内存依赖预测训练字段的一部分。 |
| L2TLB/PMP 相关 `io_l2_*` | 12 | input 12 | V3 L2TLB request 字段，V2 整核不存在同名端口。 |
| `io_mem_to_ooo_staIqFeedback_*` | 12 | output 12 | 当前 `dut_inst.sv` 已接 V3 store address issue feedback 扩展字段。 |
| CSR control | 11 | input 11 | 包含 V2/V3 命名风格差异。 |
| `io_mem_to_ooo_updateLFST_*` | 10 | output 10 | 当前 `dut_inst.sv` 已接 V3 StoreSet/LFST 更新反馈字段。 |
| wakeup | 9 | output 9 | V3 wakeup 输出字段。 |
| debug topdown 相关 | 7 | input 2、output 5 | 当前 `dut_inst.sv` 已接 V3 顶层 debug/topdown 字段。 |
| `io_mem_to_ooo_vstuIqFeedback_*` | 6 | output 6 | 当前 `dut_inst.sv` 已接 V3 vector store issue feedback 扩展字段。 |
| `io_mem_to_ooo_memoryViolation_*` | 5 | output 5 | 当前 `dut_inst.sv` 已接 V3 memory violation 扩展字段的一部分。 |
| `io_memInfo_*` | 3 | output 3 | 当前 `dut_inst.sv` 已接 V3 memInfo 状态字段。 |

结论：即使用 V2 整核 `build/rtl` 重新作为基准，当前 `dut_inst.sv` 仍不能直接用于 V2。它依然是 V3 形态的实例连接模板。

## 6. V2 整核未被当前 `dut_inst.sv` 连接的端口

V2 整核中有 589 个端口未被当前 `dut_inst.sv` 连接，主要类型如下：

| 类型 | 数量 | 方向分布 | 影响判断 |
| --- | ---: | --- | --- |
| `io_ooo_to_mem_enqLsq_req_*` | 168 | input 168 | 编译可通过不代表功能正确；这些输入需要驱动或明确绑默认值。 |
| `io_ooo_to_mem_issueVldu_*` | 75 | input 73、output 2 | V2 vector load issue 需要接入对应 agent/interface。 |
| `io_mem_to_ooo_writebackVldu_*` | 73 | output 73 | V2 vector load writeback 需要 monitor/RM 观察策略。 |
| `io_ooo_to_mem_issueLda_*` | 72 | input 69、output 3 | V2 load issue 需要驱动和握手适配。 |
| `io_mem_to_ooo_writebackLda_*` | 62 | output 62 | V2 load writeback 是检查路径核心输出。 |
| `io_mem_to_ooo_writebackSta_*` | 43 | output 43 | V2 store address writeback 需要采集或豁免。 |
| `io_ooo_to_mem_issueSta_*` | 24 | input 22、output 2 | V2 store address issue 需要适配。 |
| `io_ooo_to_mem_issueStd_*` | 16 | input 14、output 2 | V2 store data issue 需要适配。 |
| TileLink/auto | 12 | output 12 | 包含 PTW/L2 buffer 输出，不能默认忽略。 |
| L2TLB/PMP 相关 `io_l2_*` | 9 | output 9 | 需要结合 V2 L2TLB profile 判断 agent 连接。 |
| `io_mem_to_ooo_lsqio_*` | 8 | output 8 | LSQ MMIO/状态输出，影响 monitor/RM。 |
| CSR control | 7 | input 7 | 需要版本宏或命名映射。 |

这些 589 个未连接端口中既有 DUT input，也有 DUT output。DUT input 需要明确由 testbench 驱动、绑默认值或通过版本宏隔离；DUT output 需要确认是否进入 monitor/RM/scoreboard，不能只为了编译通过而悬空。

## 7. V3 `dut_inst.sv` 原有遗漏仍存在

当前 `dut_inst.sv` 对 V3 的关系没有变化：

- `dut_inst.sv` 1393 个连接端口全部存在于 V3 `MemBlock`。
- V3 `MemBlock` 仍有 294 个端口未被 `dut_inst.sv` 连接。
- 这 294 个 V3 漏接端口仍集中在 `intWriteback`、debug、L2TLB/PMP、TileLink/PTW 和少量 vector writeback 输出。

需要注意：当前 `dut_inst.sv` 中已经声明了 `io_mem_to_ooo_intWriteback_*` 等 wire/reg，
但 `MemBlock U_MEMBLOCK` 实例端口列表没有连接 `.io_mem_to_ooo_intWriteback_*`。
因此这里的“V3 漏接 `intWriteback`”指的是实例端口未连接，不是 testbench 局部信号未声明。

因此，当前 `dut_inst.sv` 不是完整 V3 接口 baseline，也不是可直接迁移的 V2 整核接口 baseline。它只能作为“当前 V3 mem_ut 已接线子集”的历史参考。

## 8. 同名位宽差异

V2 整核与 V3 有 25 个同名端口位宽不同，方向差异为 0。主要集中在：

| 类型 | 示例 | V2 整核 | V3 | 影响 |
| --- | --- | --- | --- | --- |
| `robIdx_value` | `io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value` | `[7:0]` | `[8:0]` | ROB index 位宽不同，transaction/monitor/RM 不能硬编码。 |
| `fuType` | `io_ooo_to_mem_enqLsq_req_0_bits_fuType` | `[34:0]` | `[35:0]` | FU type 编码位宽不同。 |
| `ftqOffset` | `io_mem_to_ooo_memoryViolation_bits_ftqOffset` | `[3:0]` | `[4:0]` | 前端/异常相关 index 宽度不同。 |
| trace retire | `io_traceCoreInterfaceBypass_*_iretire` | `[6:0]` | `[7:0]` | trace 相关字段宽度不同。 |
| `pendingPtr` | `io_ooo_to_mem_lsqio_pendingPtr_value` | `[7:0]` | `[8:0]` | LSQ pointer/index 宽度不同。 |

后续适配应把这些宽度收敛到版本参数或版本 profile，不应在多个 agent 的 interface、transaction、driver、monitor 中散落硬编码。

除同名位宽差异外，还存在同语义字段的结构差异：

- ROB index：V2 多处是 8 bit，V3 多处是 9 bit。
- `fuType`：V2 `enqLsq_req_*_bits_fuType` 是 `[34:0]`，V3 是 `[35:0]`。
- `ftqOffset`：V2 常见 4 bit，V3 常见 5 bit。
- 前端索引字段：V2 更常见 `ftqPtr` 形态，V3 更常见 `ftqIdx` 或展开后的 flag/value 形态。
- uop 结构：V2 很多端口保留 `bits_uop_*` 嵌套命名，V3 在 issue 侧更多是扁平 `bits_*`，writeback 侧则出现 `toRob/toIntRf/toFpRf` 分支。

## 9. 逐端口差异清单

完整逐端口差异清单已单独生成：

```text
AI_DOC/analysis/interface/v2/memblock_v2whole_v3_port_diff_detail_20260707.md
```

该清单逐项列出：

- 568 个 V2-only 端口。
- 921 个 V3-only 端口。
- 25 个 V2/V3 同名但方向或位宽不同端口。

主文档只保留 review 和适配决策需要的接口族级分析；后续 coding 时需要逐端口检查，应以该 detail 文档作为查表入口。

## 10. 语义相近但组织方式不同的接口

以下接口族不是简单“无关差异”。它们在 V2/V3 中承担相近的内存后端功能，但外部端口组织、字段集合、channel 数量或 bundle 类型不同，不能只做端口名替换。

| V2 接口族 | V3 接口族 | 语义关系 | 证据与作用 | 适配判断 |
| --- | --- | --- | --- | --- |
| `issueLda`、`issueSta`、`issueStd`、`issueVldu` | `intIssue`、`vecIssue` | 语义相近，组织方式不同 | V2 `XSCore.scala` 将 backend 的 load/store/vector issue 分别接到 `memBlock.io.ooo_to_mem.issueLda/Sta/Std/Vldu`；V3 `XSCore.scala` 改为连接 `intIssue/vecIssue`。V3 `MemBlock.scala` 内部再通过 `params.hasLoadFu/hasStoreAddrFu/hasStdFu/hasVLoadFu` filter 出 `issueLda/Sta/Std/Vldu`。 | 需要 adapter 按 FU 类型和 channel 映射，不得把 `issueLda_0` 机械改名成某个固定 `intIssue_x_y`。 |
| `writebackLda`、`writebackSta`、`writebackStd`、`writebackVldu` | `intWriteback`、`vecWriteback` | 语义相近，组织方式不同 | V2 `XSCore.scala` 明确连接拆分后的 load/store/vector writeback；V3 `XSCore.scala` 连接聚合后的 `intWriteback/vecWriteback`。V3 `MemBlock.scala` 内部 filter 出 `writebackLda/Sta/Std/Vldu` 后再驱动内部单元。 | 需要按 writeback 来源和 FU 类型建立转换；monitor/RM 也要按版本识别不同 writeback transaction 结构。 |
| `enqLsq_req/needAlloc/resp` | `enqLsq_req/needAlloc/resp` | 名字相近，但字段集合和 channel 展开不同 | 两版都用于 dispatch/rename 到 LSQ 的入队请求、分配需求和 lq/sq index 返回；但 V2-only 仍有 168 个 `enqLsq_req` 字段，V3-only 仍有 20 个 `enqLsq_req` 和 32 个 `enqLsq_resp` 字段。 | 可复用事务语义，但字段不能直接复用。`fuType`、`robIdx`、exception/trigger/flushPipe 等字段需要版本化。 |
| `wakeup` / `otherFastWakeup` | `wakeup` | 部分语义相近，V2 有额外 fast wakeup 形态 | V2 `mem_to_ooo` 中有 `otherFastWakeup` 和 `wakeup`；V3 保留 `wakeup`，但 `otherFastWakeup` 不再作为同名顶层端口暴露。 | V2 需要保留 fast wakeup 观察或豁免；V3 不能用 `wakeup` 完全替代 V2 `otherFastWakeup`。 |
| `lsqio.loadMmio/loadMmioUop/storeMmio/storeMmioUop` | `lsqio.mmioBusy` | 都属于 LSQ/ROB MMIO 状态反馈，但语义粒度不同 | V2 `XSCore.scala` 将 `loadMmio/loadMmioUop/storeMmio/storeMmioUop` 接到 backend ROB LSQ IO；V3 只看到 `mmioBusy` 同类状态。 | 不是一一命名变化。V2 可精确给出 load/store MMIO uop，V3 更像忙状态摘要；scoreboard/RM 不能直接等价。 |
| CSR branch predictor enable | CSR branch predictor enable | 部分是命名风格变化，部分是字段集合变化 | V2 出现 `btb_enable/ras_enable/sc_enable/tage_enable/ubtb_enable`；V3 出现 `ubtbEnable/abtbEnable/mbtbEnable/tageEnable/scEnable/ittageEnable` 等。 | `ubtb/sc/tage` 可能是同义命名变化；`btb/ras` 与 `abtb/mbtb/ittage` 不是一一对应，需要按 CSR 定义确认。 |
| L2TLB/PMP request/response | L2TLB/PMP request/response | 同属地址翻译/权限检查接口，但字段集合不同 | 两版都有 `io_l2_tlb_req_*`、`io_l2_pmp_resp_*` 类端口，但 V3-only 有 48 个、V2 未接出还有 9 个。 | 必须按 V2/V3 `l2tlb_interface_profile.md` 分别适配，不能把 L2TLB agent 接成 L2Cache/PTW 下游模型。 |
| TileLink/auto 端口 | TileLink/auto 端口 | 同属外部总线/缓存通路，但 user 字段和 channel 字段不同 | V2 整核和 V3 都有 `auto_*`，但 V3-only 有 46 个，V2 未接出有 12 个，差异集中在 user 字段、PTW/L2 buffer、frontend bridge。 | 需要以实际 filelist 和 `MemBlockTop` 连接为准；不能按同名前缀默认等价。 |

### 10.1 issue channel 映射草表

下表是基于端口字段形态和 V3 `MemBlock.scala` filter 逻辑得到的近似映射，只能作为后续 coding 前的分析线索。真正修改前仍必须结合当前 V2/V3 Scala FU 参数、生成端口顺序和 `backendParams` 重新确认。

| V2 拆分接口 | 近似 V3 聚合接口 | 语义说明 | 风险 |
| --- | --- | --- | --- |
| `issueLda[0..2]` | `intIssue[0..2]` | load issue 输入，驱动 LoadUnit。 | channel 顺序推断自字段形态，需用 FU 参数确认。 |
| `issueSta[0..1]` | `intIssue[3..4]` | store address issue 输入，驱动 StoreUnit 地址侧。 | V3 issue bundle 字段更扁平，不能直接连 transaction。 |
| `issueStd[0..1]` | `intIssue[5..6]` | store data issue 输入，驱动 StdExeUnit。 | V3 `intIssue` 中可能混有额外 branch/flush 元信息。 |
| `issueVldu[0..1]` | `vecIssue[0..1]` | vector load/store issue 输入，V3 内部再按 VLoad FU filter。 | V3 `vecIssue` 还携带更多 vector spec/fpu 字段。 |

### 10.2 writeback 映射判断

V2 writeback 端口按执行单元类型拆分：

- `writebackLda`：load writeback。
- `writebackSta`：store address writeback。
- `writebackStd`：store data writeback，V2-only 有 4 个 output 字段。
- `writebackVldu`：vector load/store 相关 writeback。

V3 writeback 端口按 scheduler/writeback group 聚合：

- `intWriteback`：内部通过 `params.hasLoadFu/hasStoreAddrFu/hasStdFu` 区分 load、store address、store data 等来源，并包含 `toRob/toIntRf/toFpRf` 分支。
- `vecWriteback`：内部通过 `params.hasVLoadFu` 等区分 vector load/store 来源，字段中包含 `ready`、`data_N`、`debug_paddr/vaddr`、`vls_*` 和大量 vector spec/fpu 元信息。

因此，V2 `writeback*` 与 V3 `intWriteback/vecWriteback` 只能做语义 adapter，不能用单个 V2 transaction 直接承接 V3 聚合 writeback。

## 11. 完全不一致或版本特有接口作用

以下接口族不能认为只是名字不同。它们要么只在某个版本作为顶层端口暴露，要么语义粒度不同，需要分别定义在各版本中的作用和处理策略。

| 接口族 | 所属版本 | 数量/方向 | 在该版本中的作用 | 适配处理 |
| --- | --- | --- | --- | --- |
| `io_mem_to_ooo_intWriteback_*` | V3-only | 158 个 output | V3 聚合后的 integer/memory writeback 返回路径，对 backend/ROB/scoreboard 提供执行结果、异常、robIdx、pdest 等写回信息。 | V2 没有同名端口。V2 应从 `writebackLda/Sta/Std` 等拆分接口采集同类语义，不应新增 V3 名字到 V2 RTL。 |
| `io_mem_to_ooo_vecWriteback_*` | V3-only | 152 个，主要 output | V3 聚合后的 vector writeback 路径，覆盖 vector load/store 相关写回字段。 | V2 应使用 `writebackVldu` 等接口。V3 `vecWriteback` 的字段比 V2 `writebackVldu` 更聚合，需版本化 transaction。 |
| `io_ooo_to_mem_intIssue_*` | V3-only | 126 个，主要 input | V3 聚合后的 integer/memory issue 输入，内部再 filter 成 load/store address/store data 等执行通路。 | V2 使用 `issueLda/Sta/Std`；需要 adapter 依据 FU 类型拆分，不能固定端口替换。 |
| `io_ooo_to_mem_vecIssue_*` | V3-only | 127 个，主要 input | V3 聚合后的 vector issue 输入，内部再 filter 出 vector load 相关 issue。 | V2 使用 `issueVldu`；字段和 ready/valid channel 需要单独映射。 |
| `io_ooo_to_mem_issueLda/Sta/Std/Vldu_*` | V2-only | 约 187 个 input/output | V2 直接暴露 load、store address、store data、vector load issue 通路，分别驱动 LoadUnit、StoreUnit、StdExeUnit、VLSU。 | V3 顶层无同名接口。V2 适配必须新增这些 agent/connect 或通过 V2 版本宏接入。 |
| `io_mem_to_ooo_writebackLda/Sta/Std/Vldu_*` | V2-only | 约 182 个 output | V2 直接暴露各类执行单元的 writeback 输出，load/store/vector 分别成组。 | V3 顶层无同名接口。V2 monitor/RM 需要按拆分后的 writeback 采集。 |
| V2 `lsqio.loadMmio*` / `storeMmio*` | V2-only | 8 个 output | 向 backend/ROB 返回 load/store MMIO 及对应 uop 信息。 | V3 没有同等粒度同名端口，只能按 V3 `mmioBusy` 或其他状态重新定义检查策略。 |
| V3 `lsTopdownInfo_*` | V3-only | 24 个 output | V3 更完整的 load/store topdown 观察信息，例如 vaddr/paddr/cache miss/robIdx。 | V2 不应补同名端口；若 testcase 依赖 topdown，需要寻找 V2 等价观测源或关闭对应检查。 |
| V3 `mdpTrain_*` | V3-only | 14 个 output | V3 内存依赖预测训练反馈，含 ftq、robIdx、target 等信息。 | V2 没有同名顶层输出，不能直接适配为 V2 writeback 或 LSQ 状态。 |
| V3 `updateLFST_*` | V3-only | 10 个 output | V3 StoreSet/LFST 更新反馈，用于内存依赖预测相关状态。 | V2 若无等价状态源，应作为 V3-only 检查项隔离。 |
| V3 `staIqFeedback/vlduIqFeedback/vstuIqFeedback` 扩展 | V3-only | 38 个 output | V3 issue queue feedback 字段集合变化，包含 flushState、sourceType、robIdx、lq/sqIdx 等扩展。 | V2 有部分同类 feedback，但字段不同，需按版本 transaction 区分。 |
| V3 `wakeup_*` 扩展 | V3-only | 9 个 output | V3 wakeup 增加 vector 写使能类字段。 | V2 `wakeup`/`otherFastWakeup` 不能直接等价替换。 |
| V3 `memoryViolation_*` 扩展 | V3-only | 6 个 output | V3 memory violation 增加 runahead/ftq/store target 类字段。 | V2 memory violation 检查需要按 V2 字段集合保留。 |
| V3 debug/topdown 扩展 | V3-only | 67 个 debug output 及部分 `io_debugTopDown_*` | V3 暴露更多 replay cause、topdown、rob head 状态。 | V2 可保留调试能力缺口记录，不应阻塞基础功能，除非当前 testcase/RM 依赖这些字段。 |
| V2 `otherFastWakeup` | V2-only 或 V2 特有组织 | V2 `mem_to_ooo` 中存在 | V2 load/hybrid load fast wakeup 通路，提前反馈给 backend issue/调度。 | V3 顶层主要保留 `wakeup`。V2 若不接该接口，会影响调度相关可观测性或时序模型。 |
| `io_outer_cpu_wfi` / `outer_cpu_halt` | V3/V2 命名与 wrapper 差异 | V3 当前 `dut_inst` 有 `io_outer_cpu_wfi`，V2 profile 记录 `outer_cpu_halt` | 都与 core halt/wfi 类顶层状态相关，但暴露层级和命名不同。 | 必须确认实例化 `MemBlock` 还是 `MemBlockTop`，不能只做字符串替换。 |

## 12. 语义判断依据

本轮语义判断主要来自以下源码事实：

- V2 `src/main/scala/xiangshan/XSCore.scala` 中，backend 与 memBlock 直接连接 `issueLda/issueSta/issueStd/issueVldu` 和 `writebackLda/writebackSta/writebackStd/writebackVldu`。
- V2 `src/main/scala/xiangshan/mem/MemBlock.scala` 中，`ooo_to_mem` 直接定义 `issueLda/issueSta/issueStd/issueVldu`，`mem_to_ooo` 直接定义 `writebackLda/writebackSta/writebackStd/writebackVldu`、`otherFastWakeup`、`lsqio.loadMmio/loadMmioUop/storeMmio/storeMmioUop`。
- V3 `/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/XSCore.scala` 中，backend 与 memBlock 连接的是 `intIssue/vecIssue` 和 `intWriteback/vecWriteback`。
- V3 `/nfs/home/lixiangrui/work/memblock_ut/XiangShan/src/main/scala/xiangshan/mem/MemBlock.scala` 中，`intIssue/vecIssue` 在 MemBlock 内部通过 `params.hasLoadFu/hasStoreAddrFu/hasStdFu/hasVLoadFu` filter 成内部 `issueLda/issueSta/issueStd/issueVldu`；`intWriteback/vecWriteback` 也通过 `params.hasLoadFu/hasStoreAddrFu/hasStdFu/hasVLoadFu` filter 成内部 `writebackLda/Sta/Std/Vldu`。

因此，V2/V3 的主要差异可以概括为：

- V2：在 `MemBlock` 顶层直接暴露按执行单元类型拆分的 memory issue/writeback 接口。
- V3：在 `MemBlock` 顶层暴露按 scheduler/writeback group 聚合后的 issue/writeback 接口，进入 MemBlock 后再按 FU 类型拆分。

这类差异属于“语义相近但接口组织不同”，后续验证环境应通过 V2/V3 宏和 adapter/transaction 参数化隔离，而不是把某个 V3 端口硬映射到某个 V2 端口。

## 13. 与上一轮 6751 端口分析的关系

上一轮使用的是：

```text
build_memblock/rtl/MemBlock.sv
```

该 V2 `MemBlock` 顶层端口数为 6751。本轮使用的是用户指定的整核生成文件：

```text
build/rtl/MemBlock.sv
```

该 V2 `MemBlock` 顶层端口数为 1334。

两者端口规模差异非常大，说明 V2 生成入口、顶层 wrapper、配置或拆分方式不同。后续 V2 DUT 适配必须先统一采用哪一个 Verilog 产物作为仿真编译权威来源。若仿真实际 filelist 使用 `build/rtl/MemBlock.sv`，则本文是当前应优先使用的接口基准；若仿真实际 filelist 仍使用 `build_memblock/rtl/MemBlock.sv`，则不能忽略上一轮 6751 端口分析。

## 14. Review 结论

本轮以 V2 整核生成 Verilog 重新分析后，结论更新如下：

- V2 整核 `MemBlock` 与 V3 `MemBlock` 的端口规模差距不再是 6751 vs 1687，而是 1334 vs 1687。
- 当前 `dut_inst.sv` 仍是 V3 形态子集，不能直接作为 V2 整核 DUT 实例。
- 当前 `dut_inst.sv` 有 648 个端口不存在于 V2 整核，是 V2 编译适配的直接阻塞点。
- V2 整核还有 589 个端口未被当前 `dut_inst.sv` 连接，其中包含 LSQ enqueue、issue、writeback、L2TLB/PMP、TileLink/PTW 等重要接口。
- V2/V3 issue/writeback 组织方式明显不同，后续不应只做端口名替换，而应按 V2/V3 宏隔离和参数化方式重新组织 `dut_inst.sv`、agent connect、interface、transaction、driver 和 monitor。
- `issue*` 与 `intIssue/vecIssue`、`writeback*` 与 `intWriteback/vecWriteback` 是语义相近但组织方式不同的接口族；LSQ、CSR、L2TLB/PMP、TileLink/auto 中也存在部分同义字段和部分版本特有字段，必须逐字段判断。

建议后续 V2 适配先做两件事：

1. 确认远端编译实际使用的是 `build/rtl` 还是 `build_memblock/rtl`，并把 V2 profile 的权威来源更新到实际 filelist。
2. 在 `dut_inst.sv` 中先隔离 648 个 V3-only 连接，再按 V2 整核的 589 个未连接端口逐类补接或豁免。
