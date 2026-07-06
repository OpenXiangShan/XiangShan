# MemBlock Dispatch 后端接口闭环检查与实施方案

本文只检查后端到 MemBlock 顶层交互的测试框架闭环，不要求测试框架模拟 MemBlock 内部硬件队列。

本轮代码落点和源码片段说明见：

- `AI_DOC/analysis/framework_design/dispatch_backend_interface_closure_code_changes.md`

接口判断原则：

- 以当前生成 RTL `build_memblock/rtl/MemBlock.sv` 的 `module MemBlock` 顶层端口和 `mem_ut` 实际 connect/agent 暴露端口为准。
- Scala 源码中存在但当前 `MemBlock.sv` 顶层未暴露的接口，只作为语义背景和后续 RTL 刷新后的适配检查项。
- 这类 Scala-only/RTL-unused 接口不作为当前 mem_ut 必须实现的闭环缺陷，也不能要求 TB 驱动对当前 RTL 内部无影响的信号。

不纳入测试框架闭环范围：

- `LoadQueueReplay`、TLB miss queue、DCache MSHR、StoreQueue/SBuffer 内部状态机。
- MemBlock 内部因为 TLB/DCache/RAW/RAR 等原因产生的 replay 调度。
- 向量 LS 完整 replay/merge-buffer 行为，初版按标量 flow 优先。

测试框架需要闭环的是后端视角的外部接口行为：

- 后端发给 MemBlock：LSQ 入队、load/STA/STD issue、ROB/LSQ commit 指针、redirect、flushSb、CSR/TLB 控制。
- MemBlock 返回后端：writeback、STA/STD IQ feedback、`memoryViolation`、`lqDeq/sqDeq`、`sbIsEmpty`。
- 返回事件被 TB 采集后，必要时要驱动回对应后端输入接口，形成合法闭环。

当前 RTL 已暴露但不纳入本轮实施任务的返回信号：

- `lqCanAccept/sqCanAccept`
- `ldCancel`
- `lqCancelCnt/sqCancelCnt`
- `mdpTrain/updateLFST`

这些信号可保留为 monitor/X-check/coverage 背景，但本轮不再设置“降级采集策略”实施任务，也不要求它们影响普通标量 LS 发射、提交和 replay 闭环。

## 1. 当前已闭环项

### 1.1 LSQ 入队 canAccept 闭环

相关文件：

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqenq_dispatch_sequence.sv`
- `mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_driver.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/lsq_ctrl_model.sv`

现状：

- sequence 从主表选择待入队项。
- driver 设置 `memblock_dispatch_wait_can_accept=1`，持续驱动 request，等待 DUT `io_ooo_to_mem_enqLsq_canAccept`。
- canAccept 后采样 DUT 返回的 `lqIdx/sqIdx`，再由 `commit_allocate_with_resp()` 更新本地 LSQ 模型和 active map。
- redirect/flush epoch 变化时，driver 会中止本次入队确认。

结论：基本闭环。

注意点：

- 当前 `MemBlock.sv` 顶层未暴露 Scala `LsqEnqIO.iqAccept`，测试框架只能按当前 Verilog 接口闭环 `canAccept/resp`。
- 后续若 DUT 顶层重新暴露 `iqAccept`，需要把是否入队成功从单纯 canAccept 扩展为 canAccept + iqAccept 的组合确认。

### 1.2 issue ready/fire 闭环

相关文件：

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lintsissue_dispatch_sequence.sv`
- `mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_driver.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/issue_queue_scheduler.sv`

现状：

- sequence 从 `load/sta/std` 三个 issue queue 选择候选项。
- driver 设置 `memblock_dispatch_wait_ready=1`，保持 valid，按各端口 ready 清除已 fire 的 port。
- sequence 在 `finish_item()` 后调用 `mark_issue_fire()`，更新发射状态、删除队列项、记录 `issue_epoch/replay_seq`。

结论：接口 ready/fire 层面基本闭环。

边界：

- driver 内部逐端口记录 `memblock_dispatch_fired_mask`，sequence 只标记实际 ready/fire 的端口；若等待 ready 期间发生 redirect/flush，已 fire 端口补记 dispatch，未 fire 端口不删除队列。
- issue 侧已统一使用 `issue_blocked_by_global_flush()`，覆盖 `flush_in_progress/active_redirect/issue_freeze_ack/pending_redirect_drive/dispatch_flush_in_progress`。剩余风险是 redirect 邻近拍行为需要 directed 波形验证。

### 1.3 writeback / IQ feedback / replay 状态闭环

相关文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/dispatch_monitor_event_adapter.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/writeback_status_handler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/exception_redirect_replay_handler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`

现状：

- int writeback、STA/STD IQ feedback 由 monitor 采集后转换为 `memblock_wb_event_t`。
- STA/STD feedback 中 `hit=1` 作为 pass，`hit=0` 作为 replay。
- `flush_state` 只作为 PTW-back 元信息，不参与 replay 判定。
- replay 事件会调用 `mark_replay_pending()`，清理旧队列残留，设置 replay target，并增加 `replay_seq`。
- 重新 route 后，目标项重新进入 issue queue。

结论：后端 IQ replay 的简化闭环基本成立。

注意点：

- 不需要等待 MemBlock 内部 `LoadQueueReplay`、TLB hint 或 DCache wakeup。那些属于 DUT 内部硬件处理。
- 当前未显式记录 `flush_state` 覆盖信息。若后续需要 PTW-back 覆盖率，可在 event/status 中补充采集字段，但不能让它单独触发 replay。

### 1.4 DUT deq 采集与本地 LSQ free count 重同步

相关文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/dispatch_monitor_event_adapter.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/lsq_commit_handler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/lsq_ctrl_model.sv`

现状：

- `dispatch_monitor_event_adapter::apply_raw_ctrl_deq()` 消费 `lqDeq/sqDeq` 和 deq pointer。
- `lsq_commit_handler` 根据 DUT deq 释放本地 active LQ/SQ map，并调用 `lsq_ctrl.release_lq/release_sq()`。
- 本地 free count 以 DUT deq 为最终释放来源，不只靠本地预测。
- 该闭环只覆盖当前 `dispatch_raw_ctrl_t` 中已经传递的 `lqDeq/sqDeq`，不包含 `lqCancelCnt/sqCancelCnt`。

结论：deq 方向基本闭环。

## 2. 当前完成状态与剩余边界

### 2.1 MemBlock `memoryViolation` 到后端 `io.redirect` 回灌

现状：

- `dispatch_monitor_event_adapter` 能把 `io.mem_to_ooo.memoryViolation` 转换成 redirect event。
- `exception_redirect_replay_handler` 收到 redirect 后先调用 `request_redirect_flush()` 冻结 admission/issue/commit，再把 payload 投递到 `pending_redirect_drive_q`。
- `memblock_redirect_dispatch_sequence` 已挂到 `tc_base.sv` 的 `env.u_redirect_agent_agent.sqr.main_phase`，负责从 queue 取 payload 并真实驱动 DUT `io.redirect`。
- `tc_dispatch_real_smoke` 不再覆盖 redirect dispatch sequence；real smoke 通过 `MEMBLOCK_REDIRECT_SEQ_EN` 控制是否启用。
- redirect monitor feedback 已移除：`io_redirect_*` 是 TB/sequence 驱动 DUT 的 input 接口，不再回采后反馈 dispatch recovery；因此不再需要 self redirect 过滤。

实现说明：

- 当前 RTL 顶层 redirect 只有 `level` 和 `robIdx`，没有单独 `flushItself` 端口；Scala 语义为 `RedirectLevel.flushItself(level)=level(0)`。
- TB 内部 payload 仍保留 `flush_itself` 字段用于 `rob_need_flush()`。真实驱动时 `io_redirect_bits_level` 来自 payload；redirect recovery 触发源来自 DUT output ctrl monitor 的 `memoryViolation`，并由 `memoryViolation.bits.level` 派生 `flush_itself`。
- `MEMBLOCK_REDIRECT_SEQ_EN=0` 只适合无 redirect/memoryViolation 的场景；若 recovery handler 检测到 redirect event 会 fatal，避免 freeze 后 `pending_redirect_drive_q` 无人消费。

结论：代码层面已闭环，仍需要通过 directed memory violation case 或 real smoke 波形验证 `io_redirect_valid` pulse 和 flush 后状态一致性。

### 2.2 redirect/flush 阶段化冻结与恢复

现状：

- `request_redirect_flush()` 只进入 freeze/request 阶段，不在同一调用链内 apply flush。
- `exception_redirect_replay_handler::advance_active_redirect()` 等待 `redirect_drive_done_for()` 成立后才调用 `apply_redirect_flush()`。
- `redirect_drive_done_for()` 要求 redirect sequence 已完成 drive，且 dispatch service-cycle 已跨过 drive done cycle，避免同 delta 内边发射边清状态。
- `common_data_transaction::issue_blocked_by_global_flush()` 是统一阻塞入口，覆盖 `flush_in_progress/active_redirect/issue_freeze_ack/pending_redirect_drive/dispatch_flush_in_progress`。
- LSQ 入队、issue、commit sequence 均在选项或驱动前检查该 helper；LSQ/issue driver 也带 flush epoch/abort 保护。
- issue driver 在等待 ready 时先采样并记录已经 ready/fire 的端口，再处理 flush abort；sequence abort path 只补记 `fired_mask` 覆盖的端口，未 fire 项保留给 flush/replay 处理。

剩余边界：

- 该实现是 TB 级合法性闭环，不精确模拟后端 redirect 期间所有 ROB/rename/frontend 恢复细节。
- 若后续需要精确验证同拍多个真实接口 fire 与 redirect 的边界，需要增加 directed testcase 和波形验收。

结论：阶段化恢复已实现，当前风险主要是待 directed 验证覆盖。

### 2.3 `flushSb -> sbIsEmpty` directed 闭环

现状：

- `lsqcommit_agent` 有 `io_ooo_to_mem_flushSb` 字段。
- `io_mem_to_ooo_ctrl_agent` 能采集 `io_mem_to_ooo_sbIsEmpty`。
- `common_data_transaction` 已维护 `flushsb_scheduled_pending/flushsb_pending/flushsb_waiting_empty/flushsb_start_cycle/last_sb_is_empty`。
- `memblock_lsqcommit_dispatch_sequence` 已扩展 directed flushSb 入口：`MEMBLOCK_FLUSHSB_SEQ_EN=1` 且 `MEMBLOCK_FLUSHSB_REQUEST_CYCLE` 到达后，驱动 `io_ooo_to_mem_flushSb=1` 一个 cycle。
- `dispatch_monitor_event_adapter::apply_raw_ctrl_deq()` 会把 ctrl monitor 采到的 `sbIsEmpty` 写回公共状态。
- `MEMBLOCK_FLUSHSB_REQUEST_CYCLE` 会登记到公共 scheduled 状态；real smoke 在 request cycle 到达前不会提前收尾。若 due cycle 撞上 redirect/global flush，scheduled/pending 状态保留，等 flush 解除后再驱动 `flushSb`。
- 等待 `last_sb_is_empty=1` 后清 pending；超过 `MEMBLOCK_FLUSHSB_TIMEOUT` fatal。
- 默认 `MEMBLOCK_FLUSHSB_SEQ_EN=0`，普通标量 smoke 不自动发 flushSb。

剩余边界：

- 当前只有 plus cycle/API 触发入口，还没有把 fence/CBO 主表场景自动映射到 flushSb 请求。
- 覆盖 fence 或 flush-sbuffer directed case 前，需要补 testcase 或主表 op 到 `request_flushsb()` 的触发策略。

结论：接口闭环已实现为 directed 能力，默认关闭；自动场景触发属于后续覆盖增强。

### 2.4 ROB/LSQ commit 输入与当前 RTL 暴露端口存在语义差异

Scala `ooo_to_mem.lsqio` 定义包含：

- `lcommit`
- `scommit`
- `commit`
- `pendingPtr`
- `pendingPtrNext`

当前生成 RTL 和 `lsqcommit_agent` 只看到：

- `io_ooo_to_mem_lsqio_pendingPtr_flag`
- `io_ooo_to_mem_lsqio_pendingPtr_value`
- `io_ooo_to_mem_flushSb`

当前 `MemBlock.sv` 顶层未暴露：

- `io_ooo_to_mem_lsqio_lcommit`
- `io_ooo_to_mem_lsqio_scommit`
- `io_ooo_to_mem_lsqio_commit`
- `io_ooo_to_mem_lsqio_pendingPtrNext_flag/value`

因此这些不是当前顶层可驱动接口。

现状：

- `memblock_lsqcommit_dispatch_sequence` 只驱动 `pendingPtr`。
- `lsq_commit_handler` 只在 TB 状态表中标记 ROB commit，并依赖 DUT `lqDeq/sqDeq` 释放 LSQ。

结论：

- 按当前 RTL 端口，commit 输入闭环只能实现到 `pendingPtr` 级别。
- 这不是当前代码直接缺失字段，不能算作当前必须补齐的闭环缺陷，而是源码语义和生成 RTL 端口之间需要长期跟踪的适配点。
- 若后续 RTL 重新暴露 `lcommit/scommit/commit/pendingPtrNext`，必须扩展 agent/interface/sequence，并按源码 commit 语义驱动。

### 2.5 可选 PTW-back replay directed 增强

现状：

- 当前方案默认把 STA/STD feedback 中 `hit=0` 直接作为后端 replay 事件处理。
- `flush_state` 作为 PTW-back 元信息采集；`dispatch_monitor_event_adapter` 对 STA `hit=0 && flush_state=1` 置 `ptw_back_replay=1`。
- 已增加 plus 开关 `MEMBLOCK_REPLAY_WAIT_PTW_EN` 和 `MEMBLOCK_REPLAY_WAIT_PTW_TIMEOUT`。
- 当该开关为 0 时，保持当前行为：`hit=0` 立即进入 `mark_replay_pending()` 和重新 route。
- 当该开关为 1 时，`exception_redirect_replay_handler` 会把 PTW-back replay 放入 `ptw_wait_replay_q`，等待对应 uid 的 L2TLB responder response done 或 timeout 后，再调用现有 replay route 流程。
- `common_data_transaction` 维护 `tlb_response_done_by_uid[]`。`memblock_l2tlb_base_sequence` 命中 TLB 表并发出 response 后调用 `mark_tlb_response_done(uid)`，避免仅因 admission 阶段已建 TLB 表就立即释放等待 replay。

边界：

- 该机制是 TB 对后端 replay 激励的节流和合法性增强，不是模拟 MemBlock 内部 LoadQueue replay 队列。
- 初版只建议覆盖 STA feedback 的 PTW-back replay；load replay 属于 MemBlock 内部硬件重发路径，不纳入 TB 模拟。
- 它不是普通 dispatch 合法性闭环必需项，默认关闭，只在 directed PTW-back replay 场景中启用。
- 如果没有对应 L2TLB request/response，等待项会在 timeout 后 warning 并释放 replay，避免 directed 测试永久挂死。

结论：已有默认关闭的最小 directed 节流实现；它不等价于完整 PTW/L2TLB miss queue 建模，但满足“打开后不立即同拍重发、等待 response done 或 timeout 再入队”的简化合法性需求。

### 2.6 以下返回信号不纳入本轮闭环实施

以下信号当前 RTL 已暴露，部分 agent 也已经做了连接或 X 检查：

- `mdpTrain/updateLFST`
- `lqCanAccept/sqCanAccept`
- `ldCancel_0/1/2_ld2Cancel`
- `lqCancelCnt/sqCancelCnt`

判断：

- `mdpTrain/updateLFST` 属于训练/统计侧信息，不主导普通 dispatch 合法性。
- `lqCanAccept/sqCanAccept` 是 LQ/SQ 分项接收能力观测；当前 LSQ 入队闭环以 `io_ooo_to_mem_enqLsq_canAccept + enqLsq_resp` 为准，不用它替代入队成功条件。
- `ldCancel` 用于完整后端取消 load-dependent wakeup；当前 dispatch framework 不建模依赖 load 的后续非 LS uop。
- `lqCancelCnt/sqCancelCnt` 只在 directed redirect/cancel 精确重同步中需要；普通标量 flow 以 `lqDeq/sqDeq` 作为释放真源。

结论：这些项不作为本轮未闭环任务，也不进入本轮已落地任务范围。后续如果单独开启依赖建模、MDP/store-set 覆盖或 directed cancel 精确重同步，再另立任务处理。

## 3. 已落地任务对照

| 任务 | 当前状态 | 关键落点 | 仍需验证 |
|---|---|---|---|
| 任务 1：redirect 回灌 sequence | 已实现 | `memblock_redirect_dispatch_sequence.sv`、`pending_redirect_drive_q`、`mark_redirect_drive_done()`、`tc_base.sv` 默认挂接、`MEMBLOCK_REDIRECT_SEQ_EN/DRIVE_TIMEOUT` | directed memoryViolation case 中能看到真实 `io_redirect_valid` pulse，且 redirect monitor 不反馈 recovery、不产生二次 dispatch redirect event。 |
| 任务 2：redirect/flush 阶段化恢复 | 已实现 | `request_redirect_flush()` 只冻结，`advance_active_redirect()` 等待 drive done，`issue_blocked_by_global_flush()` 统一阻塞 admission/issue/commit，issue driver 用 `fired_mask` 记录已 accepted 端口 | redirect 相邻拍已 fire 与未 fire 端口的状态更新需要波形/日志验证。 |
| 任务 3：flushSb/sbIsEmpty directed 闭环 | 已实现为默认关闭的 directed 能力 | `request_flushsb()`、`mark_flushsb_driven()`、`update_sb_is_empty()`、`memblock_lsqcommit_dispatch_sequence` 驱动 `flushSb`，`MEMBLOCK_FLUSHSB_*` plus | 需要 directed testcase 打开 `MEMBLOCK_FLUSHSB_SEQ_EN` 并设置 request cycle，确认 `flushSb` pulse 后等待 `sbIsEmpty=1`。 |
| 任务 4：PTW-back replay directed 增强 | 已实现为默认关闭的最小节流 | `ptw_back_replay`、`ptw_wait_replay_q`、`MEMBLOCK_REPLAY_WAIT_PTW_*`、`tlb_response_done_by_uid[]`、L2TLB responder `mark_tlb_response_done(uid)` | 需要 directed STA `hit=0 && flush_state=1` case，确认开启后等待 response done 或 timeout 再重新入队。 |

## 4. 当前最小闭环结论

普通标量 load/store + 后端 replay + memory violation/redirect 的最小接口闭环已经具备：

1. LSQ 入队通过 `canAccept/resp` 建立 active ROB/LQ/SQ 映射。
2. LOAD/STA/STD issue 通过 DUT ready/fire 推进，并能在 redirect 邻近拍区分已 accepted 和未 accepted 端口。
3. writeback/IQ feedback/deq 经 monitor adapter 写回状态表。
4. memory violation/redirect 先冻结发射，再真实驱动 `io.redirect`，最后按 ROB 年龄 apply 软件 flush。
5. flushSb 和 PTW-back replay 是 directed 增强，默认关闭，不影响普通 smoke 合法性。

## 5. 剩余边界

- 当前不模拟完整前端 refetch、ROB/rename 恢复和 MemBlock 内部 LoadQueueReplay/TLB miss queue。
- flushSb 已有接口闭环，但 fence/CBO 主表场景到 `request_flushsb()` 的自动触发策略未展开。
- PTW-back replay 等待的是 L2TLB responder response done 或 timeout，不等价于完整 PTW 下游 miss/resp 建模。
- `lqCancelCnt/sqCancelCnt/ldCancel/mdpTrain/updateLFST` 不纳入本轮闭环，只保留为后续 directed 或 coverage 扩展项。
