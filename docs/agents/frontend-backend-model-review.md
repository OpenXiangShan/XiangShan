# Frontend BackendModel 审阅笔记

本文档是对
`src/test/python/Frontend/env/backend_model.py`
的代码级审阅整理，目标是让读者在不通读整文件的情况下快速建立模型，并明确当前高价值风险点。

如果本文与
`docs/agents/frontend-backend-agent.md`
存在冲突，以后者语义约束为准；本文聚焦“当前实现读法与实现风险”。

## 模块总览

- `BackendModel` 维护五类核心状态：FTQ 生命周期、`cfvec_queue`、`commit_queue`、`pending_resolves`、`pending_events`。
- 周期入口在 `plan_cycle_actions()`：采样 `cfVec`、更新 resolve、推进 instruction commit frontier、选择 FTQ-entry commit、选择 redirect、输出 callRetCommit。
- `cfvec_queue` 是观测数据面，每条指令保存路径状态、resolve 状态、FTQ 归属等语义信息。
- `commit_queue` 保存可提交正确路径指令的程序序，指令级 commit 由 `_plan_instruction_commits_for_cycle()` 推进。
- FTQ-entry commit 由 `_plan_commit_entry_for_cycle()` 基于 `commit_queue` 头部 span 的语义条件发出。
- redirect 由 mismatch 路径或显式注入事件进入 `pending_events`，再由 `_ready_redirect_for_cycle()` 选出，经 `_plan_redirect_payload()` 执行 flush/recovery 与对外驱动。

## 状态机关系（文字版）

- `cfVec(valid)` -> `cfvec_queue.append`，建立指令语义条目。
- 正确路径指令 -> `commit_queue.append`，进入提交语义域。
- CFI 指令 -> 入 `pending_resolves`，到 ready_cycle 后进入 `resolve emitted`。
- `commit_queue` 从头扫描：`correct-path` 且（非 CFI 或 CFI 已 resolve）且满足时序门槛 -> `rob_commit_state=committed`。
- 指令被 committed 后 -> 进入 `pending_queue_call_ret_commit_indices` -> 下一拍激活 callRetCommit 可见组。
- `commit_queue` 头 FTQ span 满足条件（全部 correct + 全部 committed + 必要 resolve 已满足 + 无冲突 redirect）-> 发 FTQ-entry `commit` 并弹头。
- 首次 mismatch -> 以该点作为 wrong-path 起点，继续接收 `cfVec` 但冻结 golden 消费，直到后续 redirect flush 清除 wrong-path 后缀。
- redirect(`flush_on_drive`) -> scoreboard flush + semantic wrong-path flush + recovery 目标建立。

## 当前实现风险与审阅关注点

本节只保留当前实现读法与稳定风险点，不记录按日期展开的修复日志。
时效性 incident、逐次 rerun 结论和阶段性状态变化，应放到专门的
case-note 或 spec 文档，而不是放在这里。

- redirect 选择阶段不应因为“目标 PC 已观测到”而静默丢弃 ready
  redirect；阅读时应确认 ready redirect 会继续经过
  `_plan_redirect_payload()`，并实际驱动 DUT 与执行语义 flush。
- callRetCommit 的 emitted 回写应以稳定身份
  `ftq_flag/ftq_value/ftq_offset` 为主，而不是把可变 `queue_index`
  当作语义主键；`queue_index` 只能作为兼容或调试辅助信息理解。
- `pending_work_count()` 必须覆盖 callRetCommit 相关待处理队列，否则
  quiescent 判断会低估系统仍在飞行的 backend 工作。
- 当 `golden_trace` 挂载且语义恢复窗口仍在进行时，即使 queue 暂空也不应
  让 fallback commit 绕过恢复门控；否则会把恢复窗口误退化为 FTQ-only
  提交路径。
- `commit_queue` 一致性应按 fail-fast 心智阅读：越界、重复、逆序或非
  `correct-path` 条目都应被视为建模或 DUT 语义错误，而不是可静默过滤的
  噪声。
- 首次 mismatch 若可以归因到 CFI，应尽快形成 redirect 排队；若连可归因
  CFI 都找不到，则更接近 env 建模错误或 DUT 错误，阅读时不应接受
  “继续挂起 wrong-path 观察看看”的宽松语义。
- mismatch 挂起期间，`cfvec_queue` 可以继续接收观测，但 golden 消费和
  `commit_queue` 语义不应在 redirect flush 之前被错误恢复。
- exception 不应被当作 FTQ-entry commit 的全局门控；否则会把更老、
  已满足语义条件的 entry 一并错误阻塞。
- 显式注入能力要按运行模式理解：bin/program-file 模式更接近真实 backend
  驱动，应禁止会扭曲语义边界的显式注入；非 bin 的直接装载模式才适合保留
  更强的注入自由度。

## 快速阅读路径

1. 先看 `__init__` 与 `plan_cycle_actions()`，建立状态与周期流程全局图。
2. 看 `_sample_cfvec()`，理解“观测 -> 入队 -> 路径语义标记”。
3. 看 `_ready_resolves_for_cycle()`，理解 resolve 的 ready 规则与对 commit 门控的影响。
4. 看 `_plan_instruction_commits_for_cycle()`，理解 instruction commit frontier 的推进规则。
5. 看 `_plan_commit_entry_for_cycle()`，理解 FTQ-entry commit 的门控与弹头逻辑。
6. 看 `_ready_redirect_for_cycle()` 和 `_plan_redirect_payload()`，理解 redirect 选择、flush 与恢复过程。
7. 最后看 `_cfvec_queue_flush_wrong_path()` / `_cfvec_queue_pop_head()` / `_cfvec_queue_remove_range()`，理解索引维护与状态一致性。

## 高误读点

- redirect payload 会读取输入 `level` 参与内部 `flush_itself` 语义判断，但最终驱动对外 `level` 固定为 `0`。
- `set_golden_trace()` 会把 resolve delay 覆盖到固定区间（当前默认 `[3,5]`），不是完全沿用构造参数。
- `resolve` 当前不再自动触发 redirect；redirect 主路径来自 mismatch 或显式注入事件。
- fallback commit 路径已统一为单-entry commit，但仍是独立分支；阅读时要明确它与 `cfvec_queue/commit_queue` 路径的切换条件。

## 建议修复顺序

- 当前风险清单项已完成修复；新增改动请按“先语义约束、再最小补丁、最后回归测试”顺序执行。
