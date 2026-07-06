# memblock_lsqenq_dispatch_sequence.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqenq_dispatch_sequence.sv`

## 1. 文件定位与使用场景

真实 LSQ 入队驱动 sequence，继承 `lsqenq_agent_agent_default_sequence`。它负责把主表中的 uid 送到 DUT 的 LSQ enqueue 接口。可以把它理解成“前门 admission”：只有这里成功后，uid 才会 active，才会建立 LQ/SQ 映射并置 `issue_ready`，后续才能发射；真实 TLB 映射由 L2TLB request/response 路径完成。

输入是 `common_data_transaction::main_table_by_uid[]`、`lsq_ctrl_model` 的预览分配结果和 DUT `canAccept/resp`。输出是 `lsqenq_agent_agent_xaction`，以及公共状态中的 `active/enq/lq/sq/issue_ready/issue queue`。真实 `tlb_mapped` 由 L2TLB responder 在 entry/PTE 回填后更新。

控制逻辑字段包括 seq enable/timeout、flush epoch、`needAlloc`、DUT `canAccept` 和公共 `dispatch_progress.max_enqueued_uid`。xaction 中的 `fuType/uopIdx/robIdx/lqIdx/sqIdx/numLsElem` 是发给 DUT 的 payload，同时 LQ/SQ key 会被 response 校验使用。

关键字段：

- enable/ready timeout/no-progress warning 阈值来自 `seq_csr_common`；LSQENQ 不再有专用 max_cycles、idle stop 或 start timeout。
- admission 起点：不再保存本地 `next_admit_uid`，每次从 `common_data_transaction::get_next_new_admit_uid()` 推导，即 `max_enqueued_uid + 1` 或首次 uid0。
- `apply_pending_lsq_cancels()`：redirect flush 后消费公共 `pending_lq_cancel_count/pending_sq_cancel_count`，回退软件 LSQ admission 镜像。

关键 task/function：

- `body()`：若 `MEMBLOCK_LSQENQ_SEQ_EN=0`，保持 idle 并返回，不回退父类随机 default sequence；否则等待主表并驱动 loop。
- `drive_lsqenq_loop()`、`send_lsqenq_cycle()`：每轮先尝试 non-LSQ admission，再收集 LSQ candidate，生成 xaction，等待 DUT canAccept；如果本轮没有 candidate，会发送 all-idle xaction 消费一个 driver 时钟，避免 zero-time forever loop。
- `admission_blocked_by_flush()`：flush 期间停止 admission。
- `next_uid_needs_lsq_admission()`：只检查公共高水位推导出的下一条 uid；redirect pending/flushed uid 允许重新 admission，active/enq/success/replay/exception uid 不允许 admission。
- `collect_lsq_candidates()`：最多取 `enq_per_cycle` 个连续 LSQ allocating uid，并预览 LQ/SQ 指针。
- `assign_lsqenq_slot()`、`set_need_alloc()`、`set_req_fields()`：填 0..7 slot 的 needAlloc、req valid、fuType、uopIdx、ROB/LQ/SQ、numLsElem。
- `get_resp_keys()`：读取 DUT resp LQ/SQ key。
- `confirm_lsq_candidates()`：若 flush epoch 未变化，用 `commit_allocate_with_resp()` 反校验并激活，随后 `complete_admission()`。`set_status_field(MEMBLOCK_STATUS_ENQ,1)` 会推进公共 `max_enqueued_uid`，并在 redirect reissue 重入队成功后清 `redirect_pending/flushed`。
- `complete_admission(uid)`：drain CSR，然后调用 `issue_queue_scheduler::prepare_issue_route_for_uid(uid)` 统一置 `issue_ready` 并 route issue。
- `admit_non_lsq_if_ready()`：AMO 类不分配 LSQ，但仍 active、置 `issue_ready` 并 route。

## 2. 调度关系与参数数据流

该 sequence 是主表进入真实 DUT LSQ/admission 流程的第一道接口驱动。它挂在 `env.u_lsqenq_agent_agent.sqr.main_phase`，由 `tc_base.sv` 默认配置；是否真正工作由 `MEMBLOCK_LSQENQ_SEQ_EN` 决定，默认 enable=0。默认关闭时 sequence 直接 idle 返回，不调用父类随机 default sequence，避免不生成 dispatch 主表的普通 testcase 误驱动 LSQENQ；真实 dispatch smoke 通过 testcase cfg 显式打开。

调度关系：

| 阶段 | 条件 | 动作 | 输出 |
|---|---|---|---|
| 等待主表 | `main_table_ready=1` | 开始 LSQ admission loop | 可消费主表 uid |
| 过滤 flush | `admission_blocked_by_flush()` 为 false | 允许继续入队 | 避免 redirect/flush 同拍产生非法新入队 |
| 选择 uid | `get_next_new_admit_uid()` 顺序推导 | 只从公共 `max_enqueued_uid + 1` 开始；redirect 后高水位回退，被 flush uid 自然成为下一条 | LSQ candidate 队列 |
| 预览资源 | `lsq_ctrl_model` 检查 LQ/SQ free count | 预分配 ROB/LQ/SQ key | xaction req slot |
| 驱动 DUT | `start_item/finish_item` | 发 `lsqenq_agent_agent_xaction` | DUT admission request |
| 确认 response | 读取 response LQ/SQ key | 校验与本地预览一致 | active map、status、TLB 表、issue queue |

参数数据流：

- `MEMBLOCK_LSQENQ_SEQ_EN`：总开关，默认 0；真实 dispatch smoke cfg 显式置 1。关闭时不驱动真实 LSQ enq flow。
- `MEMBLOCK_ENQ_PER_CYCLE`：固定模式下每拍最多选择多少个连续 uid 入队，必须在 `[1:MEMBLOCK_REAL_ENQ_WIDTH]` 内，超出 fatal。
- `MEMBLOCK_ENQ_PER_CYCLE_RAND_EN`：开启后每拍从 `[1:MEMBLOCK_REAL_ENQ_WIDTH]` 内均匀随机本拍 `max_enq`。
- `MEMBLOCK_REAL_LSQ_ENQ_MAX`、`MEMBLOCK_REAL_ENQ_WIDTH`：真实 LSQ enqueue slot 上限。`REAL_ENQ_WIDTH` 是兼容字段，必须等于 `REAL_LSQ_ENQ_MAX`，所有 slot 清理/填充/response 读取均跟随该宽度。
- `MEMBLOCK_LSQENQ_READY_TIMEOUT`：控制 driver 等待 DUT `canAccept` 的最大周期数。
- `MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES`：控制常驻 LSQENQ loop 连续无 admission progress 时的 warning 周期；只用于 debug，不作为退出条件。`drive_lsqenq_loop()` 的正常退出只看顶层置位的 `global_stop_requested`。
- `MEMBLOCK_PADDR_BASE/PADDR_RANGE`、PTE 权重、runtime CSR：在 L2TLB request 到来时由 by-key TLB builder 消费，用于生成 responder 后续要返回的 TLB entry。

状态输出：

- `status.active=1`、`status.enq=1`。`enq=1` 的边沿推进公共 `max_enqueued_uid`。
- active ROB/LQ/SQ 映射建立。
- status `issue_ready` 由 `issue_queue_scheduler::prepare_issue_route_for_uid(uid)` 统一置位并路由到 issue queue；uid 发射后预登记 `uid_tlb_record_by_uid[uid]`，L2TLB req 建/查 entry 后再置 `tlb_mapped`。
- 按 op behavior 路由到 LOAD/STA/STD issue queue。

因此，后续 `memblock_lintsissue_dispatch_sequence` 不再重新判断这个 uid 能不能 admission，它只消费已经被本 sequence 激活并路由好的 issue item。redirect reissue 时同一 uid 沿用主表 ROB/LQ/SQ key 重入队，本 sequence 不维护历史游标，只服从公共高水位。
