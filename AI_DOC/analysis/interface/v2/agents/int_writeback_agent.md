# V2 Int Writeback Agent 接口知识

## 版本元数据

| 项目 | 内容 |
|---|---|
| RTL 版本 | V2 |
| 分支 | `mem_ut_uvm_v2` |
| 核验 commit | `bd813bc3ed5b39581be966c6518788852890ff6f` |
| 设计基线 | `2acbf327cf7fb514593acc00d4c41117ec499e08`，见 V2 `branch_policy.md` |
| 权威源码 | `src/main/scala/xiangshan`、`build_memblock/rtl/MemBlock.sv`、V2 `memblock_rtl_profile.md` |
| 最后核验日期 | `2026-07-17` |

## Agent 职责和边界

`io_mem_to_ooo_int_wb_agent_agent` 观察 MemBlock 发往 Backend 的 scalar
LDA、STA、STD split writeback。本文只定义 V2 顶层真实字段和 RTL 语义；UID、LQ/SQ
补齐、generation、drop/fatal 策略属于测试框架 adapter，不是 RTL 接口事实。

## RTL 顶层 metadata 端口

| 端口 | `valid` | `trigger[3:0]` | `flushPipe` | `replayInst` | 主要来源 |
|---|---:|---:|---:|---:|---|
| `writebackLda_0` | 有 | 有 | 有 | 有 | LoadUnit0 或 AtomicsUnit override |
| `writebackLda_1` | 有 | 有 | 有 | 有 | LoadUnit1 或 LoadMisalignBuffer override |
| `writebackLda_2` | 有 | 有 | 有 | 有 | LoadUnit2/uncache lane |
| `writebackSta_0` | 有 | 有 | 有 | 无 | StoreUnit0、StoreMisalignBuffer、StoreQueue MMIO/CBO |
| `writebackSta_1` | 有 | 有 | 无 | 无 | 普通 StoreUnit1 |
| `writebackStd_0/1` | 有 | 无 | 无 | 无 | Std/Moud store-data FU |

`trigger` 的编码是 `0=BreakpointExp`、`1=DebugMode`、`15=None`。它不是 Bool。
Breakpoint 的异常语义由 `exceptionVec(breakPoint)` 承载，Debug Mode 由 ROB 显式检查
`TriggerAction.isDmode(trigger)`。STA0 的 StoreQueue uncache/CBO 路径会把 trigger 写成
0，但同时清 exceptionVec；该非规范默认值不会触发 ROB breakpoint，观察者必须联合来源和
exceptionVec 判断。

`flushPipe=1` 表示当前指令可以提交，ROB 在精确提交点产生 `flushAfter` 清年轻流水。
当前 split 端口中合法的动态来源是 STA0 的 StoreQueue CBO/CMO 写回；普通 Load/Store
路径明确清零。STA1 因只有恒 0 的普通 StoreUnit 来源，生成顶层没有该字段。

`replayInst=1` 表示当前指令不提交，ROB 产生 `flush` 并从当前指令自身重新取指。虽然
LDA0/1/2 顶层保留该 capability，当前 V2 scalar LoadUnit、LoadMisalignBuffer 和 Rename
路径均把它清零；当前 scalar LDA 有效写回应为 0。`HyldaCfg` 也声明 replay capability，
其 `HybridUnit` 在 forwarding 物理/虚拟地址 CAM 失配且仍处于正常 Load flow 时才会动态
置位；这属于独立的 `writebackHyuLda` 内部路径，不应套到 scalar LDA split lane。

## 为什么不同 port 的字段不同

`FuConfig` 先声明 compile-time capability：

| FuConfig | `trigger` | `flushPipe` | `replayInst` |
|---|---:|---:|---:|
| `LduCfg` | true | true | true |
| `StaCfg` | true | true | false |
| `StdCfg` | false | false | false |

`ExeUnitParams` 对一个执行单元包含的 FuConfig 做 OR，Backend 只为开启的 capability
连接可选 writeback 字段。随后 whole-core elaboration 和 firtool 根据每个 lane 的真实 mux
来源及常量继续裁剪。因此：

- 字段不存在表示当前生成配置没有该能力或该 lane 被证明恒为中性值；
- 字段存在只表示数据通路被保留，不能据此推断运行时一定会产生动作；
- STA0 因接入 StoreQueue CBO 写回而保留 `flushPipe`，STA1 的普通 StoreUnit 恒写 0，字段被裁掉；
- STD 不做地址 trigger，也不请求精确 flush/replay，所以三类 metadata 均不存在。

## 握手和采样

Scala 内部使用 `DecoupledIO(new MemExuOutput)`。当前 whole-core 生成的 `MemBlock.sv`
模块边界只保留 `valid` 和被 Backend 消费的 payload，没有暴露 `ready`。验证侧只在对应
`writeback*_<lane>_valid=1` 时采样同拍 payload；不能从无效拍 payload 推断动作。

## UVM 组件映射

| RTL 信号组 | interface/connect | monitor | raw/adapter 当前边界 |
|---|---|---|---|
| `writebackLda_0/1/2` | `io_mem_to_ooo_int_wb_agent_agent_interface.sv`、`io_mem_to_ooo_int_wb_agent_connect.sv` | `io_mem_to_ooo_int_wb_agent_agent_monitor::mon_data()` | 当前 raw 只保留 exception/key；metadata 保真和 guard 由 int-WB 适配 plan 处理 |
| `writebackSta_0/1` | 同上 | 同上 | STA0/STA1 capability 必须分别处理，不能共用统一字段模板 |
| `writebackStd_0/1` | 同上 | 同上 | 不得补造 trigger/flush/replay 字段 |

## 关联 Flow

- [Memory trigger flow](../../../rtl/v2/flows/memory_trigger_flow.md)：trigger 地址匹配、编码和 ROB 消费。
- [Memory flushPipe flow](../../../rtl/v2/flows/memory_flush_pipe_flow.md)：普通访存清零、STA0 CBO 动态置位和 `flushAfter`。
- [ROB 压缩与后端指令信息流](../../../rtl/v2/flows/rob_compress_and_backend_instruction_flow.md)：`replayInst`、ExceptionGen 和 ROB 精确 redirect。

## V2/V3 差异

本文只核验 V2。V3 的聚合 int writeback 端口和 metadata 不能套用本文的 split lane 表。

## 源码证据

- `src/main/scala/xiangshan/Bundle.scala:193-205,761-772`：三个字段的运行时语义和 trigger 编码。
- `src/main/scala/xiangshan/backend/fu/FuConfig.scala:415-463`：LDU/STA/STD capability。
- `src/main/scala/xiangshan/backend/exu/ExeUnitParams.scala:65-76`、`backend/Bundles.scala:711-737`：可选输出字段生成。
- `src/main/scala/xiangshan/Parameters.scala:468-492`：V2 STA/LDA/STD 执行单元配置。
- `src/main/scala/xiangshan/mem/MemBlock.scala:73-75,511-548,1358-1390`：split lane 与 override 来源。
- `src/main/scala/xiangshan/mem/pipeline/LoadUnit.scala:1560-1645`、`StoreUnit.scala:378-401`：普通路径清零和 trigger 生成。
- `src/main/scala/xiangshan/mem/pipeline/HybridUnit.scala:1168-1197`：HybridUnit replay producer 条件。
- `src/main/scala/xiangshan/mem/lsqueue/StoreQueue.scala:841-849,1054-1060`：STA0 uncache/CBO metadata。
- `src/main/scala/xiangshan/backend/Backend.scala:671-703`、`backend/rob/Rob.scala:578-630,1211-1227`：写回到 ExceptionGen/ROB 的消费者。
- `build_memblock/rtl/MemBlock.sv:831-950,30250-30510`：V2 顶层实际端口和 lane mux。

## 知识修订记录

| 日期 | commit | 旧结论 | 新结论 | 修订原因 | 影响范围 |
|---|---|---|---|---|---|
| 2026-07-17 | `bd813bc3ed5b39581be966c6518788852890ff6f` | 首次建立，无旧的 agent 长期文档 | 建立 V2 LDA/STA/STD split writeback metadata capability、运行时语义和 lane 差异 | 用户要求结合 Scala 源码解释 metadata 和端口差异 | V2 MemBlock int writeback agent |
| 2026-07-17 | `bd813bc3ed5b39581be966c6518788852890ff6f` | 只说明 scalar LDA 的 `replayInst` 恒 0，未交代 Scala 中合法动态 producer 的归属 | 补充 `HybridUnit` 的 forwarding CAM 失配 producer，并明确其属于独立 Hyu writeback、不能套到 LDA0/1/2 | 用户追问 replay/flush/trigger 的置位场景 | V2 int writeback 与 HybridUnit 边界 |

## 待确认项

- V3 对应接口未在本轮核验。
