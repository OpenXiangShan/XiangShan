# TEA Selector 精简重构设计

**目标：** 在不破坏 TEA 主骨架的前提下，精简 `TeaSampleSelector`，删除当前 selector 中与论文核心无关的 drained 时间重建逻辑，只保留论文要求的核心 time-proportional 行为。

## 背景

当前 TEA 实现的主路径已经成立：

- `teaPsv` 已沿 `IBuffer -> CtrlFlow -> StaticInst -> DecodedInst -> DynInst -> ROB` 传播
- ROB 已复用 `tip_state`
- ROB 已输出独立 `Tea_<hart>` 表
- helper 级测试当前可通过

现阶段复杂度主要集中在 [`src/main/scala/xiangshan/Tea.scala`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/Tea.scala) 的 `TeaSampleSelector`。现有 selector 不仅处理 `compute/stall/walk/drained` 四种 commit 语义，还额外维护：

- drained backlog 的精确计数
- drained 样本原始时间戳重建
- drained 样本 stride 推断
- irregular spacing 检测与 selector 内 overflow

这些逻辑使 selector 状态显著膨胀，但论文核心只要求：

1. compute 时按 commit 指令采样
2. stalled 时归因到 ROB head
3. flushed 时归因到 OIR
4. drained 时不要立刻错误归因，等第一条新指令出现后再归因

也就是说，当前 drained 时间重建属于“增强语义”，不是 TEA 核心。

## 重构原则

- 不改动 TEA 主路径，不新建平行实现
- 不重写 `Tea.scala`、`TeaHelperTest.scala` 的整体结构
- 优先删除 selector 内的次要状态，而不是重新设计 ROB/前端接口
- 保持现有 `TeaEntry` 和 `TeaSampleSelector` IO 尽可能稳定，降低连锁改动
- 允许收缩 drained replay 语义，但不能收缩到丢失 drained sample 数量

## 方案比较

### 方案 A：保留 drained sample 数量，删除时间重建

做法：

- 保留 drained backlog 的计数
- drained 时不记录精确时间间隔
- 第一条新分配指令到来后，按 backlog 条数逐拍回放 drained 样本
- `sample.cycle` 不再尝试恢复每个 drained 样本原始采样时间

优点：

- 明显降低 selector 状态复杂度
- 保留论文核心的样本数语义
- 对 ROB 接口和现有测试框架改动最小

缺点：

- drained 回放样本的时间戳不再精确等同于原始采样周期

### 方案 B：把 drained backlog 折叠成一个 pending bit

做法：

- drained 时只记“有无待回放”
- 第一条新指令到来后最多输出一条 drained 样本

优点：

- 实现最简单

缺点：

- drain 持续多个 sample period 时会丢失样本数量
- 已偏离 time-proportional 采样核心

### 方案 C：保留 first-drain timestamp，但去掉 stride 推断

做法：

- 保留 backlog 计数
- 仅记录第一次 drained 发生时的时间
- 回放时不再恢复间隔，只保留首样本时间标记

优点：

- 比方案 A 多保留一点时间信息

缺点：

- 仍引入不必要的 drained 时间状态
- 精简幅度不够明显

## 采用方案

采用 **方案 A**。

原因：

- 它是“最小修改原则”和“保留论文核心”之间的最佳平衡
- 可以删除 selector 中最难维护的一整组时间重建状态
- 不需要回退现有 `teaPsv` 传播、ROB 累积、OIR 路径

## 精简后 selector 职责

重构后 `TeaSampleSelector` 只负责以下四件事：

1. `compute`：输出本拍 commit 向量
2. `stall`：输出 ROB head 的 `pc + psv`
3. `walk / OIR`：优先输出 OIR
4. `drained`：累积待回放数量，并在第一条新分配指令到来后按数量回放

selector 不再负责：

- drained 样本原始时间戳重建
- drained spacing / stride 推断
- irregular drained spacing 检测
- 由上述逻辑触发的本地 overflow

## 状态收缩

### 保留状态

- `pendingDrainCount`
  - 记录 drain backlog 数量
- `replayActive`
  - 表示是否已拿到回放锚点指令
- `replayPc`
  - 回放 drained 样本时使用的归属 PC
- `replayPsv`
  - 回放 drained 样本时使用的归属 PSV
- `cycle`
  - 仍保留统一 sample 时间字段

### 删除状态

- `oldestDrainCycle`
- `lastDrainCycle`
- `drainStride`
- `drainStrideValid`
- `localOverflow`

## 精简后 drained 语义

### 记录阶段

当 `sampleFire && state == drained && !oir.valid` 时：

- 不产生 sample
- `pendingDrainCount += 1`

### 锚定阶段

当 `firstAllocValid && pendingDrainCount > 0 && !replayActive` 时：

- `replayActive := true`
- `replayPc := firstAllocPc`
- `replayPsv := firstAllocPsv`

### 回放阶段

当 `replayActive && pendingDrainCount > 0 && !oir.valid` 时：

- 每拍输出一条 drained sample
- sample 使用 `replayPc/replayPsv`
- `pendingDrainCount -= 1`
- 当计数降到 0 时，`replayActive := false`

### 与 OIR 的优先级

若 OIR 有效，则 OIR 优先于 drained replay：

- 本拍输出 OIR sample
- drained backlog 保持不变
- 下一拍继续 replay

这与当前实现的优先级一致，应保留。

## `cycle` 字段策略

`TeaEntry.cycle` 保留，但语义收缩：

- `compute/stall/walk`：仍记录 sample 实际输出时刻
- drained replay：记录“回放输出时刻”，不再试图恢复原始 drained sample 的采样时刻

这样做的理由：

- 不影响 TEA 主要用途，核心仍是 `pc + psv + state`
- 避免 selector 内引入额外时间恢复状态
- 若后续确实需要更强的 drain 时间分析，应在离线分析侧或 ROB 外部单独建模，而不是塞回 selector

## `overflow` 语义

重构后只保留外部输入的 `overflow`：

- OIR 覆盖冲突、上层冲突仍由 ROB 维护
- selector 不再因为 irregular drained spacing 产生内部 overflow

对应变化：

- `TeaEntry.overflow` 仍保留
- 其来源仅为 `io.overflow`

## 对现有代码的影响

### 需要修改

- [`src/main/scala/xiangshan/Tea.scala`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/Tea.scala)
  - 精简 `TeaSampleSelector`

### 不需要修改

- [`src/main/scala/xiangshan/frontend/IBuffer.scala`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/frontend/IBuffer.scala)
- [`src/main/scala/xiangshan/Bundle.scala`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/Bundle.scala)
- [`src/main/scala/xiangshan/backend/Bundles.scala`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/backend/Bundles.scala)
- [`src/main/scala/xiangshan/backend/rob/RobBundles.scala`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/backend/rob/RobBundles.scala)
- [`src/main/scala/xiangshan/backend/rob/Rob.scala`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/backend/rob/Rob.scala)
  - 除非 selector IO 因精简产生轻微命名调整，否则无需改动结构

## 测试策略

### 保留测试

- compute 输出 commit lanes
- stalled 输出 ROB head
- OIR 在 walk / 非 walk 时优先输出
- drained 样本在 `firstAlloc` 前不应错误输出
- 多个 drained sample 应按 backlog 数量逐拍 replay
- OIR 与 drained replay 同拍冲突时 OIR 优先
- 外部 overflow 仍能透传到输出 sample

### 删除或改写测试

以下测试依赖已删除的“时间重建增强语义”，应删除或重写：

- `should preserve spaced drained sample cycles when replaying deferred outputs`
- `should tolerate irregular drained sample spacing and surface selector overflow instead of asserting`
- 所有依赖 `oldestDrainCycle / drainStride / drainStrideValid / localOverflow` 的断言

### 新的 selector 测试重心

测试不再验证 drained replay 的精确 `cycle` 序列，而改为验证：

- 是否延迟
- 是否按数量 replay
- 是否使用正确 `pc/psv`
- 是否 obey OIR priority

## 风险与控制

### 风险 1：离线分析脚本依赖 drained replay 的精确 cycle

控制：

- 保留 `cycle` 字段，但文档化其语义变化
- 若脚本确实使用该字段，再单独调整脚本而不是把复杂逻辑加回 selector

### 风险 2：删掉 selector 内 overflow 后，原有某些保护消失

控制：

- 保留 ROB 侧 `teaOverflow`
- selector 不再负责“irregular spacing”类 overflow，本质上是职责收缩而不是功能缺失

### 风险 3：回放逻辑过度简化导致 backlog 漏发

控制：

- backlog 数量测试必须保留
- 长 backlog replay 测试必须保留

## 结论

这次重构不是重做 TEA，而是把 `TeaSampleSelector` 从“drained 时间重建器”收缩回“论文核心语义选择器”。

最终目标是：

- 保留 `compute/stall/walk/drained` 四种核心归因
- 保留 drained sample 数量
- 删除 drained 时间重建复杂度
- 最大限度减少改动范围
