# TEA 论文语义与当前 RTL 实现对照

## 目的

本文档用于对照 TEA 论文语义与当前 XiangShan2 RTL 实现，明确哪些部分与论文一致，哪些部分属于工程化近似，尤其关注 `drained` 状态下的归因语义。

## 结论摘要

当前 RTL 总体上复用了论文/TIP 的四态 time-proportional 选择框架，但 `drained` 路径并非论文语义的严格实现，而是一个带 `replay anchor` 的工程近似：

- 论文语义：`drained` 时间归因到 `next-committing instruction`
- 当前 RTL：`drained` backlog 归因到后续出现的 `firstAllocPc/firstAllocPsv` 锚点，再通过 replay 输出

因此，当前 `state=3` TEA 样本更准确应解释为：

`drain interval` 的 replay blame，由后续锚点指令承载，而不是严格意义上的 `next-committing instruction` 归因。

## 对照表

| 主题 | TEA 论文语义 | 当前 RTL 实现 | 一致性判断 | 备注/证据 |
| --- | --- | --- | --- | --- |
| 采样总体原则 | 在 commit 侧做 time-proportional attribution，采样当拍正在暴露延迟的指令 | 复用 TIP `tip_state` 和 commit/head/OIR 选择逻辑，在 ROB 侧输出 TEA 样本 | 基本一致 | 论文 Section 2；`src/main/scala/xiangshan/backend/rob/Rob.scala` |
| `compute` 归因 | 多条并行 commit 时，时间均摊到 committing instructions | `state=0` 时直接输出 `commitMask/commitPc/commitPsv` | 一致 | `src/main/scala/xiangshan/Tea.scala:147-155` |
| `stalled` 归因 | 归因到 head-of-ROB 且将来会 commit 的指令，即 next-committing instruction | `state=1` 时输出 `headPc/headPsv` | 一致 | 论文 Figure 1 / Section 2；`src/main/scala/xiangshan/Tea.scala:156-162` |
| `flushed` 归因 | 归因到导致 flush 的指令；它已提交，是 last-committed instruction | RTL 用 OIR 单独保存 flush 相关样本，并以 `oirValid` 优先输出 | 语义接近 | 论文 Figure 1 / Section 2；`src/main/scala/xiangshan/backend/rob/Rob.scala:938-944`，`src/main/scala/xiangshan/Tea.scala:133-139` |
| `drained` 归因对象 | `Drained: ... Time is hence attributed to the next-committing instruction.` | `drained` 时先累计 `pendingDrainCount`；等后续 `firstAllocValid && firstAllocPsv.orR` 时，用 `firstAllocPc/firstAllocPsv` 建立 replay 锚点 | 不严格一致 | 论文 Figure 1 / Section 2；`src/main/scala/xiangshan/Tea.scala:123-127,168-171` |
| `drained` 样本返回时机 | 论文实现描述为：`Stalled` / `Drained` 状态下，延后到下一条 µop commit 再返回 sample，以保证 PSV 更新完成 | 当前 RTL 不等待“下一条 commit”，而是在 selector 内等待 replay 条件满足，再按 `sampleFire` 输出 replay 样本 | 不一致 | 论文 Figure 4 implementation text；`src/main/scala/xiangshan/Tea.scala:124-145` |
| `drained` 保存的信息 | 论文语义强调最终样本应对应 next-committing 指令；未要求用 enqueue 锚点替代 | RTL 只保存 backlog 计数和一个 replay 锚点：`replayPc/replayPsv` | 工程化简化 | `src/main/scala/xiangshan/Tea.scala:110-113` |
| `drained` 时间戳语义 | 论文侧强调 time-proportional 归因，不要求 selector 内做额外历史时间重建 | 当前实现已删掉 drained 历史时间重建，`cycle` 表示 replay 发射时刻而非原始 drained 采样时刻 | 可接受的实现收缩 | `docs/superpowers/specs/2026-04-16-tea-selector-simplification-design.zh-CN.md` |
| `drained` 是否允许无事件锚点 | 论文示例中 `drained` 样本归因到真正的目标指令，且该指令携带触发事件 | 当前 RTL 已限制只有 `firstAllocPsv.orR` 才能建立锚点，避免 `psv=0` 的垃圾 replay | 修正后更合理，但仍非论文原义 | `src/main/scala/xiangshan/Tea.scala:124-127` |
| `drained` 数据解释方式 | 可近似解释为 next-committing 指令的 time-proportional blame | 更准确应解释为 drain backlog 的 replay blame，由锚点指令承载 | 需要单独标注 | 适用于离线分析与 DB 解读 |

## `drained` 差异的核心判断

### 论文要求

论文在对 commit state 的定义中明确给出：

- `drained` 时，ROB 因前端停顿而为空
- 时间应归因到 `next-committing instruction`

同时，论文的实现描述还指出：

- `Stalled` 和 `Drained` 状态的 sample 会延后到下一条 µop commit 再返回

这说明论文里的 `drained` 语义，本质上是“等真正的目标指令可被确定并提交相关信息后，再把这段时间归到它”。

### 当前 RTL 的实际语义

当前 RTL 并没有等待“下一条真正 commit 的 µop”：

1. `drained` 期间只累计 `pendingDrainCount`
2. 后续只要出现一条满足条件的 `firstAlloc`
3. 就把它的 `pc/psv` 记入 `replayPc/replayPsv`
4. 再在后续 `sampleFire` 时刻逐拍 replay

因此，当前 RTL 的 `drained` 语义不是“论文原义的直接实现”，而是：

- 用后续一条 eventful allocation 作为代理锚点
- 用 replay 机制保存 drained sample 的数量语义
- 放弃“严格 next-committing”这一级别的精确性

## 对离线分析的建议

在解析 `Tea_<hart>` 表时，建议对 `state=3` 单独处理：

- 不要直接把它当成严格的 next-committing blame
- 更适合标成 `drain replay / anchor-attributed`
- 若需要严格对齐论文语义，应进一步修改 RTL，使 `drained` 样本与下一条真实 commit 建立绑定，而不是与 `firstAlloc` 建立绑定

## 参考来源

- TEA 论文：`docs/Björn Gottschall 等 - 2023 - TEA Time-Proportional Event Analysis.pdf`
- 当前 selector 实现：`src/main/scala/xiangshan/Tea.scala`
- ROB 对 selector 的接线：`src/main/scala/xiangshan/backend/rob/Rob.scala`
- 仓库内部设计文档：
  - `docs/superpowers/specs/2026-04-15-tea-design.zh-CN.md`
  - `docs/superpowers/specs/2026-04-16-tea-selector-simplification-design.zh-CN.md`
