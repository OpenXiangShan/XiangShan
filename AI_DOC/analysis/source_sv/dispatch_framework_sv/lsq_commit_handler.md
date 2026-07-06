# lsq_commit_handler.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/lsq_commit_handler.sv`

## 1. 文件定位与使用场景

ROB commit 和 LQ/SQ deq helper。它存在的原因是一条操作真正完成需要两个条件：ROB 侧可以 commit，LSQ 侧对应 LQ/SQ entry 也被 DUT deq 释放。这个 handler 把 pendingPtr 驱动、ROB commit 标记和 deq 资源释放放在一起。

输入是 status 表、DUT ctrl deq raw event，以及软件 LSQ 模型。输出是 `lsqcommit_agent_agent_xaction` 的 pendingPtr，和 status/active map/free count 的更新。

控制逻辑字段是 `commit_cursor_uid/last_pending_ptr`、status 的 `rob_commit/lsq_deq/success`、LSQ free count。pendingPtr 和 deq pointer 是接口 payload，但会被用来释放 active mapping。

字段：

- `commit_cursor_uid`：按 uid 顺序选择 ROB commit batch。
- `last_pending_ptr`：驱动 `lsqcommit_agent_agent_xaction` 的 pendingPtr 默认值。

函数：

- `bind_lsq_ctrl(ctrl)`、`ensure_handles()`：绑定 LSQ 模型。
- `uid_is_commit_candidate(uid)`：active、writeback/pass、required target done、无 fault/replay/redirect/flush、未 ROB commit。
- `advance_commit_cursor_past_done()`、`select_rob_commit_batch(uids)`：按顺序最多选 `MEMBLOCK_COMMIT_WIDTH` 个。cursor 只跳过 `success=1` 的 uid，不能把 `flushed` 当作完成项。
- `clear_lsqcommit_xaction()`、`build_lsqcommit_xaction()`：生成 pendingPtr transaction。
- `mark_rob_commit_uid()`、`mark_rob_commit_batch()`：置 `rob_commit`，无 LQ/SQ active 时置 `lsq_deq`，尝试 retire。
- `apply_dut_lq_deq(count,deq_ptr,ptr_is_next)`、`apply_dut_sq_deq()`：根据 DUT ctrl deq pointer 释放 active LQ/SQ，并更新软件 free count。
- `apply_raw_ctrl_deq()`：从 raw ctrl monitor sync 路径转换 deq。

## 2. 字段与函数/task 设计原理

`lsq_commit_handler` 负责 ROB commit 激励和 DUT LQ/SQ deq 回收。它不判断主表怎么生成，也不直接处理 writeback；它只看状态表里是否已经 pass，然后推进 commit/deq。

关键字段：

| 字段 | 含义 | 设计原理 |
|---|---|---|
| `data` | 公共 owner | 读取状态表、释放 active map。 |
| `lsq_ctrl` | 软件 LSQ 镜像 | DUT deq 后释放软件 free count，并检查 deq 指针。 |
| `commit_cursor_uid` | ROB commit 扫描游标 | commit 必须按 uid/ROB 年龄顺序推进，不能跳过前面未完成项。 |
| `last_pending_ptr` | 上一次发给 DUT 的 pendingPtr | 没有新 commit 时仍保持 pendingPtr，避免无意义抖动。 |

主要函数：

| 函数 | 参数 | 功能和设计原理 |
|---|---|---|
| `bind_lsq_ctrl(ctrl)`、`ensure_handles()` | LSQ model | 显式绑定或懒加载 LSQ model，避免 null。 |
| `report_deq_mismatch(msg)` | 错误信息 | 默认 fatal；`lsq_resync_on_mismatch` 打开时 warning，用于 debug 阶段临时继续。 |
| `uid_is_commit_candidate(uid)` | uid | active、writeback、pass、required targets done、无 fault/replay/redirect/flush 时才允许 ROB commit。 |
| `advance_commit_cursor_past_done()` | 无 | 只跳过已经 `success=1` 的 uid，保持顺序 commit。`flushed` 是 redirect reissue 中间态，不能跳过。 |
| `select_rob_commit_batch(uids)` | 输出 uid 队列 | 从 cursor 开始最多选 `MEMBLOCK_COMMIT_WIDTH` 个连续可 commit uid。遇到第一个不可 commit 就停，模拟 ROB 顺序提交。 |
| `clear_lsqcommit_xaction(tr)` | xaction | 初始化 pendingPtr 和 flushSb，避免旧值残留。 |
| `build_lsqcommit_xaction(tr,commit_uids,has_commit)` | 输出 xaction、uid 队列、是否有 commit | 根据可 commit 批次构造 commit agent transaction。 |
| `mark_rob_commit_uid(uid)`、`mark_rob_commit_batch(uids)` | uid/uid 队列 | 写状态表 `rob_commit`，并尝试 retire。 |
| `lq_deq_start_key(deq_ptr,count,ptr_is_next)`、`sq_deq_start_key(...)` | DUT deq ptr、数量、ptr 语义 | 兼容 DUT 返回“下一指针”或“起始指针”的两种语义。 |
| `apply_dut_lq_deq(count,deq_ptr,ptr_is_next)`、`apply_dut_sq_deq(...)` | deq 数量、指针、ptr 语义 | 检查 DUT deq 起点是否等于软件头指针，逐项反查 active uid，释放软件 LSQ 和 active map。 |
| `apply_raw_ctrl_deq(...)` | raw ctrl 字段 | 统一支持 raw monitor sync 输入路径，再复用 `apply_dut_lq_deq()` / `apply_dut_sq_deq()` 做资源释放。 |
