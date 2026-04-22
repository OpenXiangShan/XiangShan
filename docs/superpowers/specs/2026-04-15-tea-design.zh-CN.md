# TEA 机制硬件实现设计

**目标：** 在 XiangShan2 现有乱序内核中实现 TEA（Time-Proportional Event Analysis）硬件支持，为动态指令维护 PSV，并按 time-proportional 规则导出独立的 `Tea_<hart>` ChiselDB 样本表。

**范围：** 第一版只实现最小事件集 `DR_L1`、`ST_L1`、`ST_TLB`、`FL_MB`，保留现有 `Tip_<hart>` 导出链路，不考虑现有离线脚本兼容性，不实现 CSR/MMIO 读取接口。

## 背景与现状

仓库已经存在一条完整的 TIP 导出链路：

- [`Rob.scala`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/backend/rob/Rob.scala) 中定义了 TIP 四态和 `Tip_<hart>` ChiselDB 表。
- [`scripts/tip`](/nfs/home/wujiabin/work/xs-env/XiangShan2/scripts/tip) 提供了现有离线分析脚本。
- 前后端已有较丰富的 topdown/debug/perf 观测信号，例如前端 `ICacheMissBubble`、负载 `DebugLsInfo`、redirect 的 `debugIsCtrl` / `debugIsMemVio`。

TEA 与当前 TIP 的关系不是“替换”，而是“在共享 ROB 状态语义的前提下，增加每条动态指令的 PSV 绑定与独立样本导出”。因此第一版不修改 TIP 语义，也不把 TEA 样本混入 `Tip_<hart>` 表。

## 设计原则

- 不改变原有流水线功能语义。TEA 只读已有状态和事件信号，并更新旁路 metadata。
- 不把 TEA 放进关键时序路径。事件 bit 优先挂接已有 debug/perf/redirect 元数据。
- TIP 与 TEA 运行时可独立开关。
- TEA 采样周期可配置；`PSV` 每周期持续维护，但不要求每周期都产出样本。
- TEA 的状态编码复用现有 TIP 四态，不重新定义另一套状态机。

## 总体方案

第一版采用“共享内部逻辑、独立导出表”的方案：

- `Tip_<hart>` 保持现状，只补运行时 `enableTip` 门控。
- 新增 `Tea_<hart>`，仅记录 TEA 真正需要的样本字段。
- 共享 ROB 中的状态判定、head 指令信息、redirect/offending 信息。
- 新增 `tea_psv` 沿 `IBuffer -> Decode -> Rename -> Dispatch -> ROB` 传播。
- ROB 在 `teaSampleFire` 时做一次 TEA sample selection，并写入 `Tea_<hart>`。

## 状态语义

TEA 状态直接复用 TIP 当前编码和判定逻辑，不单独定义新的 `TeaState`：

- `computing`
- `stalled`
- `walk`
- `drained`

其中 `walk` 在语义上对应论文 TEA 中的 flushed / flush-recovery 区间。也就是说，TEA 与 TIP 的状态定义完全一致；TEA 的差别在于该状态下“采样哪条指令以及其 PSV 是什么”。

## 采样周期

TEA 不默认每周期采样，而是使用运行时可配置周期：

- `enableTea<hart>`：是否启用 TEA 样本导出
- `teaSamplePeriod<hart>`：样本周期，默认值设为 `1`

`tea_psv` 每周期持续更新；只有在 `enableTea && teaSampleFire` 时才真正产出一条 TEA 样本。

语义如下：

- `teaSamplePeriod = 1`：每周期采样，适用于 bring-up、波形检查和对照验证
- `teaSamplePeriod = N`：每 `N` 周期采样一次，适用于常规 TEA 运行

实现使用倒计时计数器，不使用取模运算。

## 事件与 PSV

第一版 PSV 位宽直接按最终 TEA 预留为 9 bit，但只实现 4 个事件：

- `DR_L1`：前端 ICache miss
- `FL_MB`：控制类 redirect / branch mispredict
- `ST_L1`：load 的 L1 DCache first miss
- `ST_TLB`：load 的 DTLB first miss

公共定义放到 `xiangshan` 顶层命名空间，对齐 `TopDownCounters` 的使用方式。这样前端和后端都可以直接引用 TEA 公共类型与 helper，不会形成前端依赖 backend 的方向性问题；同时不直接塞进 `package.scala`，而是放在一个独立顶层文件中：

- [`src/main/scala/xiangshan/Tea.scala`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/Tea.scala)

其中包含：

- `TeaEvent`
- `TeaPSV` 辅助函数
- `TeaEntry`
- `TeaOIR`

## PSV 传播路径

为避免新建并行数据结构，`teaPsv` 直接挂到现有 bundle 链上：

- [`CtrlFlow`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/Bundle.scala)
- [`DecodeInUop`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/backend/Bundles.scala)
- [`DecodeOutUop`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/backend/Bundles.scala)
- [`RenameOutUop` / `EnqRobUop`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/backend/Bundles.scala)
- [`RobEntryBundle`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/backend/rob/RobBundles.scala)

这样可保证：

- 前端事件在进入 ROB 前即可绑定
- 后端事件在 ROB 内持续累积
- 现有 enqueue/dispatch/commit 通路改动最小

## 前端事件绑定

第一版 `DR_L1` 不新增前端 miss 检测逻辑，直接复用已有 topdown 原因：

- 来源：[`FrontendTopDownBundle`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/frontend/FrontendBundle.scala) 中的 `TopDownCounters.ICacheMissBubble`
- 绑定点：[`IBuffer.scala`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/frontend/ibuffer/IBuffer.scala)

绑定策略：

- 以 fetch packet 为单位观察前端 miss
- packet 内第一条真正入队的指令继承 `DR_L1`
- 同 packet 其他指令 `teaPsv` 置零

这一策略与论文中“前端 miss 由首条代表指令继承”的思路一致，也与当前 XiangShan 前端包级数据流更匹配。

第一版不实现 `DR_TLB`，因为当前前端 `ITLB miss` 的精确 slot 绑定还需要额外核对，不适合与最小版一起落地。

## 后端事件绑定

### `ST_L1`

来源复用现有 load debug 聚合信号：

- [`DebugLsInfo.s2_isDcacheFirstMiss`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/backend/ctrlblock/LsInfo.scala)

写入位置：

- ROB 中对应 `robIdx` 的 `teaPsv`

### `ST_TLB`

来源复用：

- [`DebugLsInfo.s1_isTlbFirstMiss`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/backend/ctrlblock/LsInfo.scala)

写入位置同上。

### `FL_MB`

来源复用现有 redirect 元数据：

- `io.redirect.valid && io.redirect.bits.debugIsCtrl`

绑定对象：

- offending `robIdx` 对应的 ROB entry

第一版只把控制类 redirect 归为 `FL_MB`，不把异常类 flush 或 mem violation redirect 混入此位。

## OIR 设计

为了保证 `walk` 状态的时间归到 offending instruction，而不是 flush 后重新进入流水线的指令，ROB 新增轻量 OIR：

- `valid`
- `pc`
- `psv`

更新规则：

- 当 `FL_MB` 发生时，用 offending `robIdx` 的 `pc + teaPsv` 更新 OIR
- `walk` 状态采样时直接取 OIR
- OIR 被成功消费后清零
- 若未消费前又收到新的 flush，置 `teaOverflow`

第一版 OIR 不额外携带复杂标志位，只保留 TEA 最小版需要的信息。

## `Tea_<hart>` 表结构

`Tea_<hart>` 只记录 TEA 样本，不混入现有 TIP 的 commit/redirect/debug 杂项字段。

建议 `TeaEntry` 至少包含：

- `cycle: UInt(64.W)`
- `state: UInt(4.W)`
- `validMask: UInt(CommitWidth.W)`
- `pcVec: Vec(CommitWidth, UInt(VAddrBits.W))`
- `psvVec: Vec(CommitWidth, UInt(TeaEvent.width.W))`
- `oirValid: Bool()`
- `pendingDrain: Bool()`

说明：

- `state` 直接复用现有 TIP 编码
- `validMask` 在 `computing` 状态可包含多条 commit 指令
- `pcVec/psvVec` 在 `stalled/walk/drained` 状态通常仅 lane 0 有效
- `cycle` 为硬件显式周期计数，便于后处理直接计算样本间隔

## TEA sample selection

采样规则如下：

- `computing`
  - 采同周期所有 commit 指令
  - `validMask := commitValid`
  - 每个 valid lane 写入对应 `pc` 与 `teaPsv`
- `stalled`
  - 只采 ROB head 指令
- `walk`
  - 只采 OIR，对应 offending instruction
- `drained`
  - 不立即写样本，置 `pendingDrainSample`
  - 等第一条新 ROB entry 分配后，再用其 `pc + teaPsv` 补写样本

`drained` 延迟补写的原因是：

- 当 ROB 为空时，当拍没有明确归因对象
- 第一版只实现 `DR_L1` 作为前端 drain 相关事件，首条新入 ROB 指令已经携带前端 `teaPsv`

## TIP 与 TEA 的关系

TIP 与 TEA 独立导出，但共享部分内部观测逻辑。

TIP：

- 保持当前 `Tip_<hart>` schema
- 只额外增加 `enableTip<hart>` 门控，便于运行时关闭

TEA：

- 使用独立 `Tea_<hart>` 表
- 使用独立 `enableTea<hart>` 与 `teaSamplePeriod<hart>`

这样可以保证：

- TIP、TEA 可分别打开或关闭
- 不要求旧 `scripts/tip` 兼容 TEA
- 首版硬件接口更稳定，后续脚本可以围绕 `Tea_<hart>` 重新设计

## 计划修改文件

首版计划修改这些文件：

- [`src/main/scala/xiangshan/backend/rob/Rob.scala`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/backend/rob/Rob.scala)
- [`src/main/scala/xiangshan/backend/rob/RobBundles.scala`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/backend/rob/RobBundles.scala)
- [`src/main/scala/xiangshan/backend/Bundles.scala`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/backend/Bundles.scala)
- [`src/main/scala/xiangshan/Bundle.scala`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/Bundle.scala)
- [`src/main/scala/xiangshan/frontend/ibuffer/IBuffer.scala`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/frontend/ibuffer/IBuffer.scala)
- [`src/main/scala/xiangshan/Tea.scala`](/nfs/home/wujiabin/work/xs-env/XiangShan2/src/main/scala/xiangshan/Tea.scala)

首版不修改：

- CSR/MMIO 接口
- 现有 `scripts/tip`
- LLC miss 精确归因链
- 其余 5 个 TEA 事件

## 验证计划

验证分三层：

### 1. 编译与结构验证

- 确认 `teaPsv` 正确贯穿 `CtrlFlow -> DecodeInUop -> DecodeOutUop -> RenameOutUop -> EnqRobUop -> ROB`
- 确认 `Tea_<hart>` 表能在 ChiselDB 中生成
- 确认 `enableTea = 0` 时不写表

### 2. 定向行为验证

- `DR_L1`
  - 构造前端 icache miss
  - 检查首条入队指令 `teaPsv(DR_L1)=1`
- `ST_L1`
  - 构造 load first miss
  - 检查对应 ROB entry 置位
- `ST_TLB`
  - 构造 dtlb miss
  - 检查对应 ROB entry 置位
- `FL_MB`
  - 构造 branch mispredict
  - 检查 `walk` 状态样本来自 offending PC/OIR，而不是恢复后的第一条指令

### 3. 波形与数据库检查

- 打开 `enableTea=1`
- 运行简单 micro workload
- 检查 `Tea_<hart>` 中的：
  - `state`
  - `validMask`
  - `pcVec`
  - `psvVec`
  - `cycle`

## 延后项

以下内容明确不在第一版范围内：

- `DR_TLB`
  - 前端精确 slot 绑定仍需额外核对
- `DR_SQ`
  - 需要先定义 dispatch/LSQ backpressure 的责任指令
- `FL_EX`
  - 需要梳理异常类 flush 的 offending instruction 绑定
- `FL_MO`
  - 需要区分 mem violation redirect 与其它 replay
- `ST_LLC`
  - 当前缺乏与 `DebugLsInfo` 同等直接的精确 per-instruction LLC miss 归因链

这些事件将在最小版稳定后再扩展。

## 风险与控制

- 风险：前端 packet 级 `DR_L1` 绑定过粗
  - 控制：第一版明确采用“首条代表指令”策略，后续若需要更精细绑定再迭代
- 风险：`walk` 状态误归因到恢复后指令
  - 控制：通过 OIR 强制绑定 offending instruction
- 风险：采样周期过小导致数据库体积增长
  - 控制：运行时通过 `teaSamplePeriod<hart>` 调整
- 风险：TEA 影响原有功能路径
  - 控制：只复用已有观测信号和 ROB metadata，不改 commit/redirect/exception 原有控制逻辑
