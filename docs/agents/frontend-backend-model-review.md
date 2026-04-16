# Frontend BackendModel 审阅笔记

本文档是对
`src/test/python/Frontend/env/backend_model.py`
的代码级审阅整理，目标是让读者在不通读整文件的情况下快速建立模型，并明确当前高价值风险点。

如果本文与
`docs/agents/frontend-backend-agent.md`
存在冲突，以后者语义约束为准；本文聚焦“当前实现读法与实现风险”。

## 模块总览

- `BackendModel` 维护四类核心状态：FTQ 生命周期、`semantic_queue`、`pending_resolves`、`pending_events`。
- 周期入口在 `plan_cycle_actions()`：采样 `cfVec`、更新 resolve、推进 instruction commit frontier、选择 FTQ-entry commit、选择 redirect、输出 callRetCommit。
- `semantic_queue` 是主数据面，每条指令保存路径状态、resolve 状态、commit 状态、FTQ 归属等语义信息。
- 指令级 commit 由 `_plan_instruction_commits_for_cycle()` 从 queue 头顺序推进，不再由 golden match 直接投影。
- FTQ-entry commit 由 `_plan_commit_entry_for_cycle()` 基于 queue 头 span 的语义条件发出。
- redirect 由 `_ready_redirect_for_cycle()` 选出，经 `_plan_redirect_payload()` 执行 flush/recovery 与对外驱动。

## 状态机关系（文字版）

- `cfVec(valid)` -> `semantic_queue.append`，建立指令语义条目。
- CFI 指令 -> 入 `pending_resolves`，到 ready_cycle 后进入 `resolve emitted`。
- `semantic_queue` 从头扫描：`correct-path` 且（非 CFI 或 CFI 已 resolve）且满足时序门槛 -> `rob_commit_state=committed`。
- 指令被 committed 后 -> 进入 `pending_queue_call_ret_commit_indices` -> 下一拍激活 callRetCommit 可见组。
- queue 头 FTQ span 满足条件（全部 correct + 全部 committed + 必要 resolve 已满足 + 无冲突 redirect）-> 发 FTQ-entry `commit` 并弹头。
- redirect(`flush_on_drive`) -> scoreboard flush + semantic wrong-path flush + recovery 目标建立。

## 风险清单（按严重度）

### 已修复（2026-04-16）

- `_ready_redirect_for_cycle()` 里“目标 PC 已观测到则丢弃 redirect”的分支已删除。
- 当前行为是：ready redirect 一律进入 `_plan_redirect_payload()`，因此会正常下发 DUT，并执行 redirect payload 路径的 flush 语义。

### 已修复（2026-04-16）

- callRetCommit emitted 回写已从 `queue_index` 切换到稳定身份匹配（`ftq_flag/ftq_value/ftq_offset`）。
- `queue_index` 仍保留在 `CommitInstruction` 中，仅用于兼容 legacy 组（缺少 `ftq_offset` 时）和调试。

### 已修复（2026-04-16）

- `pending_work_count()` 已补齐 callRetCommit 相关待处理队列（pending/scheduled/visible）的统计。

### 已修复（2026-04-16）

- 当 `golden_trace` 挂载且 semantic recovery/wait 进行中（如 `_semantic_recovery_target_pc` / `_golden_wait_pc`），即使 queue 暂空也会阻断 fallback commit，避免恢复窗口走 FTQ-only 门控。
- 非恢复窗口仍保留 fallback commit，以保持零指令 FTQ entry 的提交能力。

## 快速阅读路径

1. 先看 `__init__` 与 `plan_cycle_actions()`，建立状态与周期流程全局图。
2. 看 `_sample_cfvec()`，理解“观测 -> 入队 -> 路径语义标记”。
3. 看 `_ready_resolves_for_cycle()`，理解 resolve 触发和 mispredict 到 redirect 的连接点。
4. 看 `_plan_instruction_commits_for_cycle()`，理解 instruction commit frontier 的推进规则。
5. 看 `_plan_commit_entry_for_cycle()`，理解 FTQ-entry commit 的门控与弹头逻辑。
6. 看 `_ready_redirect_for_cycle()` 和 `_plan_redirect_payload()`，理解 redirect 选择、flush 与恢复过程。
7. 最后看 `_semantic_queue_flush_wrong_path()` / `_semantic_queue_pop_head()` / `_semantic_queue_remove_range()`，理解索引维护与状态一致性。

## 高误读点

- redirect payload 会读取输入 `level` 参与内部 `flush_itself` 语义判断，但最终驱动对外 `level` 固定为 `0`。
- `set_golden_trace()` 会把 resolve delay 覆盖到固定区间（当前默认 `[3,5]`），不是完全沿用构造参数。
- fallback commit 路径已统一为单-entry commit，但仍是独立分支；阅读时要明确它与 semantic queue 路径的切换条件。

## 建议修复顺序

- 当前风险清单项已完成修复；新增改动请按“先语义约束、再最小补丁、最后回归测试”顺序执行。
