# 当前 TEA 实现总览

## 范围

本文档整理当前 XiangShan2 中 TEA（Time-Proportional Event Analysis）的实际实现状态，覆盖：

- 代码落点
- 数据路径
- 事件来源
- sample selection 语义
- 运行时开关
- 当前已知限制

本文档描述的是“当前 RTL 行为”，不是论文原始语义的逐字复刻。论文语义与当前 RTL 的差异见：

- [`docs/superpowers/specs/2026-04-22-tea-paper-vs-rtl-comparison.zh-CN.md`](/nfs/home/wujiabin/work/xs-env/XiangShan2/docs/superpowers/specs/2026-04-22-tea-paper-vs-rtl-comparison.zh-CN.md)

## 代码结构

当前 TEA 相关实现主要分布在以下文件：

- [`src/main/scala/xiangshan/Tea.scala`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/Tea.scala)
  - `TeaEvent`
  - `TeaPsvOps`
  - `TeaBinders`
  - `TeaFrontend`
  - `TeaOIR`
  - `TeaEntry`
  - `TeaSampleSelector`
- [`src/main/scala/xiangshan/frontend/ibuffer/IBuffer.scala`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/frontend/ibuffer/IBuffer.scala)
  - 前端 `DR_L1` 事件绑定
- [`src/main/scala/xiangshan/backend/rob/Rob.scala`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/backend/rob/Rob.scala)
  - ROB 内的 `teaPsv` 存储
  - backend 事件 bit 绑定
  - OIR / overflow / selector 接线
  - `Tea_<hart>` ChiselDB 导出
- [`src/test/scala/xiangshan/backend/TeaHelperTest.scala`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/test/scala/xiangshan/backend/TeaHelperTest.scala)
  - helper-level regression tests

## 事件空间

当前 `TeaEvent` 预留了 9 个 bit：

- `DR_L1`
- `DR_TLB`
- `DR_SQ`
- `FL_MB`
- `FL_EX`
- `FL_MO`
- `ST_L1`
- `ST_TLB`
- `ST_LLC`

当前实现里真正已经接上线的主路径事件主要是：

- `DR_L1`
  - 前端 `ICacheMissBubble`
- `FL_MB`
  - control redirect / mispredict flush
- `ST_L1`
  - load 的 L1 DCache first miss
- `ST_TLB`
  - load 的 DTLB first miss

其余 event bit 目前主要仍是预留位或局部定义位宽用途，不应默认解读为已经完整实现。

## 数据路径

### 前端路径

前端在 IBuffer 侧构造 packet-level drain 事件：

- 当 fetch packet 的 `ICacheMissBubble` 为真时，生成 `TeaEvent.DR_L1`
- TEA PSV 绑定到 packet 中第一条 carrier instruction
- 该 `teaPsv` 进入 `IBufEntry`，随后进入 `CtrlFlow`

当前 IBuffer 的 carrier 选择使用 packet 级有效掩码，而不是等待“真正入队后的 first actual enqueued slot”。这反映的是当前实现语义，而不是论文语义增强版。

### 指令元数据传播

`teaPsv` 会从前端一路进入后端关键 bundle：

- `CtrlFlow`
- `DecodeInUop`
- `DecodeOutUop`
- `RenameOutUop`
- `EnqRobUop`
- `RobEntryBundle`

ROB entry 在 enqueue 时接收初始 `teaPsv`，后续再由 backend 事件继续置位。

### 后端事件绑定

ROB 中当前有两类主要 backend 事件绑定：

- load debug 路径
  - `s2_isDcacheFirstMiss` -> `ST_L1`
  - `s1_isTlbFirstMiss` -> `ST_TLB`
- redirect 路径
  - `debugIsCtrl` -> `FL_MB`

这些 bit 都直接并入 ROB entry 内保存的 `teaPsv`。

## Sample Selection

当前 TEA sample selection 复用 TIP 状态来源，但输出 TEA 独立样本。

### 四类输出状态

- `computing`
  - 输出所有 commit lane 的 `pcVec/psvVec`
- `stalled`
  - 输出 ROB head 的 `pc + psv`
- `oir`
  - 输出 OIR 保存的 flush 相关样本
- `drained_replay`
  - 不在 drained 当拍直接输出样本
  - 先累计 backlog
  - 再在后续满足条件时 replay

### drained / replay 语义

当前 drained 路径是一个“count + replay anchor”实现：

1. 当 `sampleFire && state == drained && !oir.valid` 时，不立刻出样本，只增加 `pendingDrainCount`
2. 后续若出现 `firstAllocValid && firstAllocPsv.orR`，记录：
   - `replayPc := firstAllocPc`
   - `replayPsv := firstAllocPsv`
3. 后续每次 `sampleFire` 且 `replayActive` 时，输出一条 `state=3` 的 replay 样本
4. 每输出一条 replay，`pendingDrainCount` 减一，直到清零

当前实现已收紧两点：

- `psv=0` 的分配不能充当 replay anchor
- replay 必须受 `sampleFire` 控制

这两点避免了空事件 replay 样本污染数据库。

## OIR 与 Overflow

当前 ROB 维护：

- `teaOir`
  - 用于保存 flush / redirect 相关的待输出样本
- `teaOverflow`
  - 由 ROB 侧维护，selector 只透传，不再在 selector 内自行综合 irregular spacing overflow

selector 内部 OIR 优先级高于 replay：

- 当 OIR 有效时，当拍优先输出 OIR
- replay backlog 不会因此丢失，只会顺延

## 运行时控制

当前运行时开关来自 Constantin：

- `enableTea<hart>`
  - 是否启用 TEA 导出
- `teaSamplePeriod<hart>`
  - TEA 的 sample period

`teaPsv` 每拍持续维护；只有在 `enableTea && teaSampleFire && sampleValid` 时才真正写 `Tea_<hart>` 表。

## 数据库导出

当前 ROB 会创建独立的 `Tea_<hart>` ChiselDB 表。

`TeaEntry` 主要字段包括：

- `cycle`
- `state`
- `validMask`
- `pcVec`
- `psvVec`
- `oirValid`
- `overflow`
- `pendingDrain`

其中：

- `state=0` 表示 computing
- `state=1` 表示 stalled
- `state=2` 当前实现里实际对应 OIR 样本
- `state=3` 表示 drained replay

## 当前已知限制

### 1. `drained` 不是论文语义的严格实现

论文要求 `drained` 时间归因到 `next-committing instruction`。当前 RTL 则是：

- backlog 先记账
- 再用后续 `firstAlloc` 建立 replay anchor

因此，`state=3` 更适合解释为：

- drain backlog 的 replay blame
- 由后续锚点指令承载

而不是严格意义上的 `next-committing instruction` blame。

### 2. `state=2` 在当前 TEA 表里是 OIR 语义

虽然上游状态来源复用了 TIP 四态，但 selector 当前把 OIR 输出硬编码成 `state=2`。这会导致：

- `Tea_0.state=2` 与 `Tip_0.state=2` 不能直接按同一语义解释

### 3. replay anchor 仍然是工程近似

收紧 `psv!=0` 后，空样本污染问题已经解决，但 replay anchor 仍然只是近似归因，而不是严格的论文级精确归因。

### 4. TEA 事件覆盖仍不完整

当前最稳定的实现主要集中在：

- `DR_L1`
- `FL_MB`
- `ST_L1`
- `ST_TLB`

完整 9-event paper-faithful 覆盖尚未全部落齐。

## 当前建议的分析口径

离线分析 `Tea_<hart>` 时，建议使用以下口径：

- `state=0/1/2` 可按当前样本 payload 直接分析
- `state=3` 单独分析，不要和普通热点混算
- 将 `state=3` 标记为：
  - `drain replay`
  - `anchor-attributed`

如果需要严格论文口径，应继续改 RTL，使 drained 样本与下一条真实 commit 建立绑定，而不是与 `firstAlloc` 建立绑定。
