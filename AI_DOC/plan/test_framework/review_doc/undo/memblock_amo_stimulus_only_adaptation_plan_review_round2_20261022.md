# MemBlock AMO 纯激励最小适配方案第二轮 Review

## 1. Review 信息

- Review 日期：2026-10-22
- Review 对象：`memblock_amo_stimulus_only_adaptation_plan_20261022.md`
- Review 轮次：第二轮
- Review 目标：以“功能完整优先、完整后最小改动”为标准，重新检查 v1.1
- Review 边界：仅激励生成、发送和生命周期收敛；不包含 RM、scoreboard、结果 checker 和 coverage
- 最终结论：v1.1 不通过功能完整性 review；修订为 v1.2 后，方案级 blocker 已消除，可以进入 coding

## 2. 功能完整性判定标准

纯激励方案要被判定为完整，至少必须：

1. 支持当前 `AtomicsUnit` 接收的全部标量原子 opcode。
2. 发出每条指令要求的全部 STA/STD 子 uop 和正确 slot。
3. 正确处理所有 atomic feedback 和 final WB 数量。
4. normal、fault、commit、terminal、reset 均能收敛。
5. 不把 MOU 错接到普通 LOAD/LQ 或 STU/SQ 生命周期。
6. hit、miss/refill 和 error response 能继续使用现有 responder，不因激励框架缺状态而挂起。

不检查返回数据是否正确，不影响上述“激励功能完整”的判定。

## 3. Review Findings

### R2-1：排除 AMOCAS，支持范围不完整

严重级别：P0 blocker。

v1.1 只支持 LR、SC 和非 CAS AMO 共 22 条，并对 `AMOCAS.W/D/Q` fail-fast。当前 RTL 已明确支持三条 AMOCAS，因此不能称为完整适配。

修正：v1.2 支持 25 个 opcode，并新增 3 个 CAS plus 权重。新权重默认 0，full AMO smoke 显式设为 1，兼顾兼容性和完整覆盖。

### R2-2：AMOCAS.Q 有两次 final WB

严重级别：P0 blocker。

`AtomicsUnit` 在 `s_finish` 返回低半后进入 `s_finish2` 返回高半，之后才复位 FSM。v1.1 的“一条 AMO 等一次 LDA0 WB”会在第一次 WB 后提前 commit 和释放 barrier。

修正：owner 保存 `required_final_wb_count/seen_final_wb_count`。Q 必须收到两次，其余指令一次；fault 也必须聚合完所需次数后再 finalize。

### R2-3：MOU 被普通 store helper 误分类

严重级别：P0 blocker。

当前 `is_scalar_store_uid()` 返回 `STU || MOU`。如果只增加 AMO writeback 分支，MOU 仍会进入普通 store 双 real-WB、pending store fault、SQ release 和 redirect defer，最终等待不存在的 `writebackStd` 或访问不存在的 SQ owner。

修正：该 helper 改为 STU-only；MOU 使用 atomic 子 uop fire counter 和 final WB 聚合，不伪造普通 STA/STD real-WB。

### R2-4：LDA0 atomic 必须早于普通 LOAD/LQ attach 分流

严重级别：P0 blocker。

`AtomicWBPort=0`，atomic final result 复用 `writebackLda_0`。当前 adapter 先把 SCALAR_LDA 当 LOAD，再要求 LQ owner，并对 AMO 触发 `INT_WB_LDA0_AMO`。

修正：按 ROB owner找到主表 MOU 后，直接归一化为已有 `MEMBLOCK_WB_EVENT_SOURCE_ATOMIC_WB`，不进入普通 LOAD/LQ snapshot。

### R2-5：Atomic STA feedback 没有合法 SQ owner

严重级别：P0 blocker。

AtomicsUnit 对每个 STA 两拍后返回 `feedbackSlow`，`hit` 固定为 1。AMO 不分配 SQ，现有“STA feedback 必须 SQ-only 并按 SQ 查 owner”的规则会 fatal。

修正：barrier active 时按唯一 atomic owner、固定 STA0 port和反馈序号绑定；不使用 SQ 值，不调用普通 pass helper，不受 `STA_REAL_WB_PASS_EN` 控制，也不作为 final completion。Q 消费两次。正常释放 owner 前要求预期 feedback 已全部消费，避免迟到事件失去 owner。

### R2-6：slot 与物理 pipe 混用

严重级别：P1 blocker。

当前 `assign_issue_items()` 用物理 `pipe_idx` 覆盖 `uop_index`，而 AtomicsUnit 要求 `fuOpType[8:6]` 保存 slot。这样无法在固定 STA0/STD0 上发送 Q 的 slot 2/3。

修正：保留 `uop_index` 表示 slot，新增 `issue_pipe_index` 表示物理 pipe。本方案所有 atomic 片固定 pipe 0。

### R2-7：现有 queue delete 会误删剩余 CAS 片

严重级别：P1 blocker。

当前删除键只有 `target + uid + replay_seq`，第一片 STD fire 会删除同 UID 的其余 STD item。

修正：增加 `uid + target + replay_seq + dynamic_epoch + uop_index` exact delete；redirect/reset 仍可使用现有批量删除。

### R2-8：单 target 状态不能表达多片完成

严重级别：P1 blocker。

现有 `queued_sta/std`、`sta/std_dispatched` 和 target issue epoch 都是单份。第一片 CAS 子 uop 不能代表整个 target 已完成。

修正：唯一 atomic owner 保存 required/fired counter、expected slot 和 per-slot issue epoch；target 位只作为聚合状态，全部同 target 片 fire 后才置 dispatched。普通 target issue snapshot 最多发布一次，不允许后续片覆盖；`required_targets_done(MOU)` 使用 owner counter。

### R2-9：Atomic fault 需要 UID 级 writeback 证据

严重级别：P1 blocker。

现有 fault commit candidate 要求 `status.writeback` 或某个 target fault 位存在。atomic fault 又不能伪造普通 `sta_writeback/std_writeback`；若只设置 `fault/exception_pending`，可能无法进入 fault commit。

修正：全部 required final WB 聚合完成后设置 UID 级 `status.writeback=1`、`pass=0` 和 fault/exception 字段。它表示 atomic final event 已完整到齐，不代表普通 STA real-WB。

### R2-10：DCache responder 不需要 AMO 专用扩展

严重级别：非 blocker，确认最小改动。

现有 responder 已支持 `AcquireBlock`、`AcquirePerm`、`GrantData`、`GrantAck` 和 denied/corrupt。atomic miss 由 DUT DCache/MissQueue 转为相同 TileLink 流量。

处理：不新增 AMO responder，只在验证矩阵中加入 hit、miss/refill、denied、corrupt，确认都形成 final WB/fault 并 terminal。

### R2-11：in-flight external redirect fail-fast 可接受

严重级别：非 blocker，有明确适用边界。

合法方案要求 AMO 前所有更老 UID terminal，barrier 阻止年轻 UID admission/issue。因此正常主动激励不会在 AMO in-flight 时由其他普通指令制造 redirect。

`redirect.valid` 不直接复位 AtomicsUnit FSM。方案继续对“STA 已 fire、final WB 前覆盖 owner 的外部 redirect”fail-fast，避免错误释放和重发。若未来要专门覆盖该场景，需要新的 DUT-idle 可观测协议，不属于本次最小适配。

## 4. 最小改动性 Review

v1.2 为功能完整增加的不可省略状态只有：

```text
唯一 atomic owner
固定上限 STA/STD/final-WB/feedback counter
expected slot
slot 与 pipe 的一个独立字段
exact queue delete
3 个 CAS opcode 权重
```

方案没有增加：

- 通用多 owner 或乱序 atomic scheduler；
- 新 agent、driver、interface、testcase 或 vseq；
- AMO 专用 TLB/DCache responder；
- 内部 FSM、`data_valid`、`stIsEmpty` 探针；
- RM、checker、scoreboard；
- AMO enable、port mask、wait-state 等模式参数。

因此新增改动均由完整性要求直接产生，没有可安全删除的功能项。

## 5. 修订后关键执行序列

```text
基础原子：STD0 -> STA0 -> final WB
CAS.W/D： STD0 -> STD1 -> STA0 -> final WB
CAS.Q：   STD0 -> STD1 -> STD2 -> STD3 -> STA0 -> STA2
           -> final WB low -> final WB high
```

CAS.Q 的 STA slot 0 必须先于 slot 2。slot 2 若在 AtomicsUnit 空闲时先成为主输入，其带 slot 高位的 `fuOpType` 无法通过精确 `isAMOCASQ` 判断，后续第二 STA 接收和双 WB 都会失效。

## 6. 剩余实现风险

这些不是方案 blocker，但 coding review 必须检查：

1. atomic feedback 和 LDA0 final WB 分流必须位于普通 SQ/LQ owner 校验之前。
2. 第一片 fire 后不能清掉 target 聚合 queued 状态、覆盖 target snapshot 或标成整个 target 已完成。
3. Q 第一次 final WB 无论 normal/fault 都不能提前推进 commit。
4. atomic fault 必须发布 UID writeback，但不能发布虚假的 STA/STD real-WB。
5. `is_scalar_store_uid()` 语义改变后必须审计全部调用点和 soft test。
6. runtime reset、terminal retire 和 drain 必须使用同一 owner identity 清理。
7. nonblocking issue 模式下仍要保持每次只接受 expected slot，并按真实 fire 记账。
8. 本轮未做 coding、编译或仿真，最终功能仍需实现级验证。

## 7. 最终结论

原 v1.1 不具备完整 AMO 激励功能，不能直接进入 coding。

修订后的 plan v1.2 已补齐：

- 3 条 AMOCAS；
- CAS 多 STA/STD 子 uop；
- Q 两次 final WB；
- atomic feedback 无 SQ owner处理；
- slot/pipe 解耦和 exact delete；
- MOU 与普通 store 生命周期剥离；
- normal、fault、batch、redirect、reset、terminal 和 runtime drain。

第二轮结论：v1.2 无剩余方案级 blocker，可以进入 coding。coding 完成后必须再做实现级 review、编译和 hit/miss/error 仿真，才能最终确认功能实现完整。
