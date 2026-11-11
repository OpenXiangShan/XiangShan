# MemBlock AMO 纯激励最小适配方案 Review

## 1. Review 信息

- Review 日期：2026-10-22
- Review 对象：`AI_DOC/plan/test_framework/plan/undo/memblock_amo_stimulus_only_adaptation_plan_20261022.md`
- Review 轮次：第一轮
- Review 类型：方案逻辑、源码一致性、改动范围和参数设计 review
- 历史结论：当时仅按“22 条基础原子指令”范围通过；该结论不满足“当前 RTL 原子功能完整”的后续要求。
- 当前状态：已被第二轮 review 替代。第二轮发现 AMOCAS、多子 uop、Q 双 writeback、atomic feedback 和 MOU/store 生命周期仍有 blocker，详见 `memblock_amo_stimulus_only_adaptation_plan_review_round2_20261022.md`。

## 2. 术语与 Review 边界

| 名词 | 本 review 中的含义 |
| --- | --- |
| 纯激励 | 只构造、调度、发送合法输入，并维护 sequence 收敛所需生命周期。 |
| 生命周期观察 | 使用 DUT writeback/exception/commit 事件决定何时继续发送，不判断返回数据正确性。 |
| 结果 checker | 比较 AMO old/new value、SC 成败或内存最终内容；不属于本 plan。 |
| blocker | 不修正会导致非法输入、状态卡死、错误重发或方案无法闭环的问题。 |

本 review 不要求实现 RM、scoreboard、coverage，也不要求在本轮修改测试源码。

## 3. 源码事实复核

### 3.1 可以复用现有 non-LSQ admission

`memblock_op_behavior_util::derive_op_behavior()` 已将 MOU 设置为：

```text
need_alloc=0
uses_lq=0
uses_sq=0
route_sta=1
route_std=1
commit_is_normal=1
```

`lsq_ctrl_model::commit_non_lsq_admission()` 会建立 active ROB/status owner，但 `commit_allocate()` 只有在 `uses_lq/uses_sq` 为 1 时才推进 pointer 和减少 free count。因此 plan 复用该路径是最小改法。

### 3.2 AMO 仍受公共 Dispatch `lsqCanAccept`

RTL `NewDispatch.scala` 的 `fromRename.ready` 对所有 dispatch 指令都与 `lsqCanAccept` 相与。LQ/SQ 的 `canAccept` 分别要求至少保留本拍最大 enqueue width，对当前 V2 是 LQ 6、SQ 4。

因此 plan 采用：

```text
admission 前检查 LQ>=6 且 SQ>=4
AMO admission 不消耗 entry
进入 IQ 后不重复检查
```

该分阶段描述正确。

### 3.3 v1.1 范围内的基础 AMO 最终只产生一次 LDA0 writeback

`MemBlock.scala` 中 `AtomicWBPort=0`，AtomicsUnit 输出覆盖 `ldaExeWbReqs(0)`。同一文件明确屏蔽 AMO 的 `writebackStd`。本节结论仅适用于当时排除 `AMOCAS.Q` 的 v1.1 范围；Q 的两次 writeback 由第二轮 review 补正。

因此 plan 不能沿用普通 store 的 STA/STD 双 real-WB 条件。plan v1.1 已要求：

```text
STA/STD 都真实 fire
最终完成只观察 writebackLda_0 atomic event
不伪造 writebackStd
```

该完成模型只服务激励生命周期，不属于结果 checker。

### 3.4 AMO 不应增加普通 store commit 计数

`memblock_op_behavior_util::is_scalar_rob_store_commit()` 已显式排除 `behavior.is_atomic`。plan 复用 normal ROB commit、但不增加 `scommit/pendingst`，与当前 RTL `commitType` 对 AMO 的分类一致。

## 4. Review 发现与修正

### R1：LR 不能无条件允许 STA 先发、STD 任意晚到

严重级别：必须修改。

原 plan v1.0 描述 STA/STD 可以任意先后或同周期 fire。该描述对 AtomicsUnit 能收集数据的一般能力成立，但不是当前最简单激励策略的稳定实现。

源码原因：

- AtomicsUnit 对普通 AMO、LR、SC 都保留一个 STD 数据槽。
- 普通 AMO/SC 的 DCache request 等待 `data_valid`。
- LR 的 DCache request 明确不等待 `data_valid`。
- 如果测试框架让 LR 的 STA 先发、STD 长时间不发，LR 可能先完成并复位 FSM，迟到 STD 会落入错误窗口。

最简单修正：第一阶段统一固定 `STD0 -> STA0`。它是合法后端调度顺序的子集，不需要新增 ready 组合状态，也不读取 DUT 内部 `data_valid`。

修正结果：plan v1.1 的 Flow、scheduler、约束、任务和验收标准均已改成两阶段发送。

### R2：不能仅凭软件 redirect stale cleanup 立即重发 in-flight AMO

严重级别：必须修改。

原 plan v1.0 允许 redirect 清理 owner 后释放 barrier，并以新 dynamic epoch 重发同一 UID。这个结论缺少 DUT idle 证据。

源码原因：`AtomicsUnit.scala` 中普通 `redirect.valid` 只清 `atom_override_xtval`，不会调用 `resetFSM()`；FSM 主要在最终 `io.out.fire` 后复位。

最简单修正：

1. STD 已发、STA 未发时，redirect 可以按软件状态清理，因为 AtomicsUnit 尚未启动主 FSM。
2. STA 已 fire、final writeback 前的外部 redirect 覆盖 owner时 fail-fast，不释放 barrier、不重发。
3. 正常 AMO fault 仍先产生 final exception writeback，再走 fault commit/redirect/terminal。

修正结果：plan v1.1 已更新 redirect/reset Flow、barrier helper、coding task 和验收标准。

### R3：完成观察不能被误写成 checker

严重级别：说明性问题，已满足。

plan 需要 monitor adapter 和 writeback status handler，否则不知道何时释放 barrier并停止 sequence；但这些组件只绑定 UID、epoch、异常和 terminal，不计算 old/new value。

plan 已在目标、抽象职责、非目标和协同章节明确排除 RM/checker/scoreboard，无需继续修改。

### R4：参数方案是否过度设计

严重级别：无问题。

plan 不新增 `AMO_ENABLE/MAX_ACTIVE/PORT_MASK/WAIT_* /AQ/RL`。单 owner、STA0/STD0 和内部等待条件都由固定策略或 DUT 自身实现。现有 op-class/opcode 权重、pipe limit、sequence enable 和 timeout 足够。

## 5. 改动范围 Review

最终方案没有新增 agent、driver、interface、xaction、testcase class 或 vseq。主要改动集中在：

```text
配置合法性
主表合法性
non-LSQ admission
单 atomic owner/barrier
STA/STD IQ owner gate
MOU 字段赋值
LDA0 final event 生命周期绑定
一个 preset cfg
```

这是基于现有 real-dispatch 流程的局部扩展。相比独立 AMO 直驱 sequence，它避免重复 TLB、DCache responder、global stop 和 commit 生命周期，改动方向合理。

## 6. 剩余实现风险

以下不是 plan blocker，但 coding review 必须重点检查：

1. atomic LDA0 分流必须发生在普通 LOAD/LQ owner 校验之前，否则仍会触发当前 `INT_WB_LDA0_AMO` 或缺失 LQ owner fatal。
2. `is_scalar_store_uid()` 当前把 MOU 包含在 store 类别中；coding 时必须审计所有调用点，避免 atomic 进入 store fault 双 WB 和 SQ release 逻辑。
3. atomic barrier 的 reset、terminal retire 和 STA 前 redirect 清理必须使用同一 owner identity，不能只比较 UID。
4. preset 使用随机 opcode 权重时不能保证一次 seed 覆盖全部 22 个 opcode；单 opcode 验证应使用权重 one-hot 或固定 seed 列表。
5. 本轮只完成方案和静态源码分析，尚未编译或仿真。

## 7. 历史结论与失效说明

plan v1.1 已满足以下目标：

- 不包含向量、抽象结果模型、RM、scoreboard 和 checker。
- 只扩展 AMO 合法激励生成、admission、issue 和生命周期收敛。
- 使用现有 real-dispatch 与 plus 参数体系。
- 参数有独立汇总章节，且不新增无必要 plus。
- 基础 AMO 与 AMOCAS 的支持边界明确。
- 驱动约束与 MemBlock 内部等待行为已分开。
- 第一轮 review 的必须修改项已修正。

本轮结论只对“排除 AMOCAS 的基础 22 条指令”成立，不能作为完整 AMO 适配的 coding 准入结论。完整性准入以第二轮 review 和 plan v1.2 为准。
