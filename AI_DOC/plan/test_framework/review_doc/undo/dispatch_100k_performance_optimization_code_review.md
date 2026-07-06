# Dispatch 100k Performance Optimization Code Review

本文档对应本轮实现：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_types.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/status_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqenq_dispatch_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/issue_queue_scheduler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/exception_redirect_replay_handler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/writeback_status_handler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/lsq_commit_handler.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_sequence.sv`

## 0. Review 阅读方式

本文档不再把源码片段作为主体，而是按“先解释改动，再用源码证明”的方式审查。每个关键改动点都尽量回答四个问题：

- 修改前逻辑是什么。
- 修改后逻辑是什么。
- 为什么修改后能解决问题。
- 哪些源码片段能支撑这个结论。

因此阅读时应先看每节的“修改前 / 修改后 / 正确性检查”，再看源码片段。源码片段只用于定位实现和核对状态变化，不要求读者直接从代码反推出设计意图。

本文档使用的关键术语如下。后续章节保留必要英文名，但会优先使用中文含义：

| 术语 | 通俗解释 | 对应代码对象 |
|---|---|---|
| LSQ admission | 一条 transaction 被送入 LSQ 入队接口，并拿到 LQ/SQ 分配结果的过程。 | `memblock_lsqenq_dispatch_sequence` |
| LSQ admission 上界 | 当前从 uid0 开始，已经连续有效完成 LSQ admission 的最后一个 uid。下一条新 admission uid 通常是它加 1。 | `dispatch_progress.max_enqueued_uid` |
| success 前缀 | 从 uid0 开始，已经连续最终完成的 uid 区间；它后面的第一个 uid 是当前最早未完成项。 | `dispatch_progress.success_prefix_uid` |
| active map | 用 ROB/LQ/SQ key 反查当前 active uid 的映射表。旧实例被 flush 后必须删除，避免旧 event 命中新状态。 | `uid_by_active_rob`、`uid_by_lq`、`uid_by_sq` |
| pending | 已经发现但尚未处理完成的等待态，例如等待 replay、redirect recovery 或 LSQ 软件镜像回退。 | `*_pending` 字段和 pending queue |
| dynamic epoch | 同一个静态 uid 被 redirect 后重新执行的版本号。它用于记录“这是第几轮动态实例”。 | `status.dynamic_epoch` |
| reissue | 同一个 uid 的旧动态实例被 redirect kill 后，重新从 LSQ admission 开始执行。 | `redirect_pending/flushed`、`prepare_uid_for_redirect_reissue()` |
| stale feedback | 已经采到但按 redirect 顺序应被覆盖的旧路径反馈。它不能再更新新状态。 | `requeue_events_not_flushed_by_redirect()`、`event_blocked_by_active_redirect()` |
| commit cursor | commit handler 当前检查到的 uid 位置。它只允许跨过已经 `success=1` 的 uid。 | `commit_cursor_uid` |

## 1. 公共 dispatch progress

功能：把 success 前缀和 LSQ admission 上界集中到 `common_data_transaction`，让 admission、route、redirect flush 共享同一个扫描边界，避免 10 万笔 testcase 中每拍从 0 全表扫描。

修改前逻辑：

- LSQ admission 依赖 sequence 本地游标。
- route 也有自己的扫描起点或隐含扫描范围。
- redirect flush 需要自己判断哪些 uid 要清理。
- completion 判断需要反复查看状态表。

问题是这些扫描边界不是同一个真源。redirect 发生后，如果 admission 回退了但 route 没回退，或者 route 跳过了某个被 flush 的 uid，就会出现漏重发、旧结果继续提交、或者 10 万笔场景每拍全表扫描的问题。

修改后逻辑：

- `success_prefix_uid` 表示从 uid0 开始连续完成后的第一个未完成 uid。
- `max_enqueued_uid` 表示 LSQ admission 上界，也就是当前连续有效入队到的最后一个 uid。
- route、redirect flush、completion 都从 `dispatch_progress` 取扫描边界。
- redirect 命中后只需要回退 `max_enqueued_uid`，后续 LSQ admission 会自然从老 uid 开始。

正确性检查：

| 检查点 | 预期行为 |
|---|---|
| admission 必须顺序推进 | `mark_uid_enqueued()` 要求新 uid 必须等于 `max_enqueued_uid + 1`。 |
| redirect 后能重新入队 | `rollback_max_enqueued_uid(oldest_flushed_uid)` 把 LSQ admission 上界回退到最老 flush uid 前一个。 |
| completion 不全表扫描 | `advance_success_prefix()` 只跨过连续 `success=1` 的前缀。 |
| 多模块边界一致 | admission、route、redirect、completion 都读取同一个 `dispatch_progress`。 |

源码片段：

```systemverilog
typedef struct {
    // 从0开始连续success后的第一个uid；route/redirect/reissue都从这里开始扫描。
    memblock_uid_t success_prefix_uid;
    // 当前连续有效LSQ admission上界；redirect后会回退到最老flush uid的前一个uid。
    memblock_uid_t max_enqueued_uid;
    // max_enqueued_uid是否有效；还没有任何uid成功admission时为0。
    bit            max_enqueued_uid_valid;
} memblock_dispatch_progress_t;
```

```systemverilog
// dispatch公共进度：所有admission/route/redirect扫描共享同一组边界，避免10万笔场景全表扫描。
memblock_dispatch_progress_t dispatch_progress;
```

字段说明：

| 字段 | 作用 | 调用关系 |
|---|---|---|
| `success_prefix_uid` | 从 uid0 开始连续 success 后的第一个未完成 uid。 | `advance_success_prefix()` 推进；route 和 redirect flush 用作 begin。 |
| `max_enqueued_uid` | LSQ admission 上界，即当前连续有效入队到的最后一个 uid，不是历史最大值。 | `mark_uid_enqueued()` 推进；redirect 命中后 `rollback_max_enqueued_uid()` 回退。 |
| `max_enqueued_uid_valid` | 标记是否已有 admission 成功。 | 首次 admission uid0 后置 1；回退到 uid0 前时清 0。 |

核心 helper：

```systemverilog
function void mark_uid_enqueued(input memblock_uid_t uid);
    ...
    if (uid != dispatch_progress.max_enqueued_uid + 1) begin
        `uvm_fatal("COMMON_DATA", ...)
    end
    dispatch_progress.max_enqueued_uid = uid;
endfunction

function void rollback_max_enqueued_uid(input memblock_uid_t oldest_flushed_uid);
    ...
    dispatch_progress.max_enqueued_uid = oldest_flushed_uid - 1;
endfunction
```

为什么添加：原来 route、LSQ admission、redirect flush 各自保存或隐含扫描进度，redirect 后很难保证回退一致。公共 progress 把“哪些 uid 已经连续完成”和“哪些 uid 当前仍是有效 admission 前缀”变成唯一事实源。

输入输出：

- 输入：uid、status.success、redirect flush 的 oldest uid。
- 输出：统一的 active scan begin/end 和下一条 admission uid。

## 2. LSQ admission 上界和 reissue

功能：删除 `memblock_lsqenq_dispatch_sequence` 本地 `next_admit_uid`，下一条 admission uid 永远从 `data.get_next_new_admit_uid()` 推导。redirect flush 回退高水位后，被 flush uid 自动成为下一条 admission uid。

这里需要先区分两个概念：

- `next_admit_uid`：旧实现中保存在 `memblock_lsqenq_dispatch_sequence` 里的本地游标。它表示“这个 LSQ enqueue sequence 下一次准备从哪个 uid 开始尝试 admission”。旧实现每次 admission 成功后递增这个游标；如果一批 LSQ 候选在等待 DUT ready 期间遇到 flush/redirect，则把它回退到本批第一个 uid。问题是这个游标只属于 LSQ sequence，本身不知道全局 success 前缀、redirect flush 后哪些 uid 需要重新执行，也不知道 issue route/commit handler 使用的扫描边界。10 万笔长流场景下，如果每个模块各自维护游标，redirect 后很容易出现 admission、route、commit 对“当前该从哪个 uid 继续”的理解不一致。
- `data.get_next_new_admit_uid()`：新实现中放在 `common_data_transaction` 的公共 helper。它不保存一个新的独立游标，而是从公共 dispatch progress 推导下一条 admission uid：如果还没有任何 uid 成功 admission，则返回 uid0；否则返回 `max_enqueued_uid + 1`。这里的 `max_enqueued_uid` 是“当前连续有效 LSQ admission 上界”，redirect flush 后会通过 `rollback_max_enqueued_uid(oldest_flushed_uid)` 回退到最老 flush uid 的前一个 uid。因此，被 flush 的老 uid 会自然重新成为下一条 admission uid。

换句话说，`next_admit_uid` 是 sequence 私有的“我上次扫到哪里了”；`get_next_new_admit_uid()` 是公共状态推导出的“当前全局 LSQ admission 前缀之后，下一条合法 uid 是谁”。本轮修改不是简单换函数名，而是把 admission 进度从局部变量迁移到 `common_data_transaction.dispatch_progress`，让 LSQ admission、redirect flush、issue route 和 success completion 共享同一事实源。

修改前逻辑：

```text
LSQ sequence 持有 next_admit_uid。
每次 admission 成功后 next_admit_uid++。
如果 LSQ xaction 等待 ready 时遇到 redirect/flush，需要 sequence 把 next_admit_uid 回退。
```

修改后逻辑：

```text
LSQ sequence 不再保存 next_admit_uid。
下一条 uid = data.get_next_new_admit_uid()。
get_next_new_admit_uid() 从公共 max_enqueued_uid 推导。
redirect flush 只回退公共 max_enqueued_uid。
```

正确性检查：

| 场景 | 修改前风险 | 修改后行为 |
|---|---|---|
| uid1 被 redirect flush，旧游标已到 uid3 | 如果本地游标没回退，uid1/uid2 可能被跳过 | LSQ admission 上界回退到 uid0，下一条 admission 自动是 uid1 |
| route 和 admission 同时工作 | route 可能按另一个边界扫描 | route 和 admission 都读公共 progress |
| 10 万笔顺序 admission | 多个游标维护成本高，debug 困难 | 只有公共 LSQ admission 上界这一个事实源 |

源码片段：

```systemverilog
uid = data.get_next_new_admit_uid();
if (uid < data.main_trans_num) begin
    status = data.get_status(uid);
    ...
    if (status.success || status.active || status.enq ||
        status.exception_pending || status.replay_pending) begin
        return 1'b0;
    end
    // redirect_pending/flushed表示旧动态实例已被kill；同uid现在允许按公共 LSQ admission 上界重新 admission。
    return 1'b1;
end
```

```systemverilog
uid = data.get_next_new_admit_uid() + uids.size();
...
if (status.success || status.active || status.enq ||
    status.exception_pending || status.replay_pending) begin
    break;
end
```

重入队成功后通过公共 setter 清状态：

```systemverilog
if (value && !old_value) begin
    mark_uid_enqueued(uid);
    // redirect reissue重新admission成功后，旧动态实例的flush标志不再阻塞route/commit。
    if (status.redirect_pending || status.flushed) begin
        status.redirect_pending = 1'b0;
        status.flushed          = 1'b0;
        status.issue_killed     = 1'b0;
    end
end
```

为什么添加：redirect 后不需要单独维护 pending 队列。只要 `max_enqueued_uid` 回退到最老 flush uid 前一项，原 admission 流程就会顺序重入队。

旧实现的问题举例：

```text
uid0、uid1、uid2 已经 admission，next_admit_uid 已经推进到 uid3。
随后 redirect flush 命中 uid1。
如果只靠 LSQ sequence 本地 next_admit_uid，必须由 redirect/driver/sequence 多处协同把它精确回退到 uid1。
一旦回退遗漏或和 route/commit 的扫描边界不一致，就可能跳过 uid1，或者让 uid2 的旧路径结果继续向后推进。
```

新实现的行为：

```text
redirect flush 命中 uid1
-> apply_redirect_flush_range() 找到 oldest_flushed_uid=uid1
-> rollback_max_enqueued_uid(uid1) 把公共 LSQ admission 上界回退到 uid0
-> get_next_new_admit_uid() 返回 uid1
-> LSQ admission 从 uid1 顺序重入队
```

输入输出：

- 输入：公共 LSQ admission 上界、status 当前状态、DUT LSQ response。
- 输出：active map、`status.enq=1`、`issue_ready=1`、公共 LSQ admission 上界推进。

调用关系：

| 调用顺序 | 函数 | 在本流程中的功能 |
|---|---|---|
| 1 | `send_lsqenq_cycle()` | LSQ admission 每拍主入口；负责先处理 pending cancel，再收集本拍可入队 uid，并向 LSQ enqueue agent 发送 transaction。 |
| 2 | `collect_lsq_candidates()` | 从 `data.get_next_new_admit_uid()` 开始，按顺序收集本拍可 admission 的 uid，并预览 LQ/SQ key 和资源是否足够。 |
| 3 | `commit_allocate_with_resp()` | LSQ enqueue fire 后，用 DUT 返回的 LQ/SQ key 校验软件模型预期，并把分配结果写回主表/status/active map。 |
| 4 | `data.set_status_field(MEMBLOCK_STATUS_ENQ, 1)` | 标记 uid admission 成功；该 setter 带副作用，会推进公共 LSQ admission 上界，并清 reissue 阻塞状态。 |
| 5 | `mark_uid_enqueued()` | 更新 `dispatch_progress.max_enqueued_uid`，并检查 admission 必须严格按 uid 顺序推进。 |

## 3. redirect flush bounded scan 和 reissue 准备

功能：redirect apply 阶段只扫描 `[success_prefix_uid, max_enqueued_uid + 1)`，命中 uid 不再终止，而是清旧动态实例、置 reissue pending、递增 `dynamic_epoch`。

修改前逻辑：

- redirect flush 可能按 `0..main_trans_num` 做全表扫描。
- flush 后的 uid 可以被当作终态处理。
- 某些旧路径的 issue/writeback/pass 状态可能没有统一清理。

修改后逻辑：

- redirect 只扫描已 admission 且尚未连续 success 的活跃窗口。
- 命中 redirect flush 的 uid 会调用 `prepare_uid_for_redirect_reissue()`。
- 该 uid 的旧动态实例会从 active map、issue queue、状态结果中清掉。
- 公共 LSQ admission 上界回退到最老 flush uid 前一个，保证后续从老 uid 顺序重入队。

正确性检查：

| 检查点 | 预期行为 |
|---|---|
| 不扫描 10 万全表 | 扫描范围为 `[success_prefix_uid, max_enqueued_uid + 1)`。 |
| flushed 不是完成 | 被 flush uid 设置 `redirect_pending/flushed`，但 `success=0`。 |
| 旧动态实例失效 | 清 active map、issue queue、writeback/pass/commit/deq 状态。 |
| 后续能重发 | LSQ admission 上界回退后，LSQ admission 再次拿到最老 flush uid。 |

源码片段：

```systemverilog
function void apply_redirect_flush_range(input memblock_redirect_payload_t redirect);
    advance_success_prefix();
    begin_uid = get_active_scan_begin_uid();
    end_uid   = get_active_scan_end_uid();

    // redirect flush只扫描已admission的活跃窗口；真正flush判断仍由ROB顺序语义决定。
    for (memblock_uid_t uid = begin_uid; uid < end_uid; uid++) begin
        status = get_status(uid);
        if (status.success || (!status.active && !status.writeback && !status.pass)) begin
            continue;
        end
        if (rob_order_util::rob_need_flush(status.get_rob_key(), redirect)) begin
            ...
            prepare_uid_for_redirect_reissue(uid, redirect);
        end
    end
    if (found_flushed) begin
        rollback_max_enqueued_uid(oldest_flushed_uid);
    end
endfunction
```

```systemverilog
function void prepare_uid_for_redirect_reissue(input memblock_uid_t uid,
                                               input memblock_redirect_payload_t redirect);
    ...
    // redirect命中的旧动态实例不再等待writeback/commit；清queue/map后等待同uid重新admission。
    if (status.active) begin
        retire_active_uid(uid);
    end else begin
        remove_uid_from_issue_queues(uid);
    end
    ...
    clear_uid_dispatch_result(uid);
    status.redirect_pending = 1'b1;
    status.flushed          = 1'b1;
    status.dynamic_epoch++;
    status.active           = 1'b0;
    status.success          = 1'b0;
endfunction
```

为什么添加：

- 原 `apply_redirect_flush()` 从 `0..main_trans_num` 全表扫描，不适合 100k。
- 原 flush 把 uid 退 active 并允许 flushed 作为终态，无法验证 redirect 后重新执行。
- 新 helper 把“清旧实例”和“回退高水位”拆开，语义更明确。

输入输出：

- 输入：DUT memoryViolation/redirect payload。
- 输出：被 flush uid 的 `redirect_pending=1/flushed=1/active=0/enq=0`，公共 LSQ admission 上界回退。

调用关系：

| 调用顺序 | 函数 | 在本流程中的功能 |
|---|---|---|
| 1 | `exception_redirect_replay_handler::advance_active_redirect()` | redirect recovery 的推进入口；检查 redirect drive 是否已经完成，完成后才允许真正修改公共状态。 |
| 2 | `data.apply_redirect_flush()` | 公共数据层 redirect flush 总入口；调用范围扫描、清 PTW wait replay、清 redirect drive queue，并释放全局 flush 标志。 |
| 3 | `apply_redirect_flush_range()` | 只扫描 `[success_prefix_uid, max_enqueued_uid + 1)` 活跃窗口，找出被 redirect 覆盖的 uid，并记录最老 flush uid。 |
| 4 | `prepare_uid_for_redirect_reissue()` | 对每个被 flush uid 清旧动态实例、删除队列/map、累计 LSQ cancel、设置 reissue pending 状态。 |
| 5 | `rollback_max_enqueued_uid()` | 在找到 flush uid 后回退公共 LSQ admission 上界，让下一轮 LSQ admission 从最老 flush uid 重新开始。 |

## 4. 软件 LSQ admission 镜像回退

功能：redirect flush 后，公共 LSQ admission 上界会回退；软件 `lsq_ctrl_model` 的 enq 指针也必须回退，否则重入队时 DUT response key 和本地预览 key 会不一致。

修改前逻辑：

- LSQ admission 成功后，软件 `lsq_ctrl_model` 会推进 LQ/SQ enqueue pointer 和 free count。
- redirect flush 清了公共状态后，如果软件 LSQ 镜像不回退，下一次 `preview_allocate()` 仍会从旧位置继续分配。

修改后逻辑：

- redirect 清理 uid 时统计被撤销的 LQ/SQ 元素数。
- 计数暂存在 `pending_lq_cancel_count/pending_sq_cancel_count`。
- LSQ enqueue sequence 下一拍开始时统一调用 `apply_pending_lsq_cancels()` 回退软件镜像。

正确性检查：

| 检查点 | 预期行为 |
|---|---|
| DUT 和软件模型 key 一致 | 重入队前先 `cancel_lq/cancel_sq` 回退本地指针。 |
| 不在 redirect handler 直接改 LSQ model | redirect handler 只累计 pending count，LSQ sequence 自己消费。 |
| 多元素向量/批量场景 | 使用 `main_tr.numLsElem` 累计需要回退的元素数量。 |

源码片段：

```systemverilog
// 记录需要回退的软件LSQ admission镜像；LSQ sequence恢复后统一调用cancel_lq/cancel_sq消费。
if (had_lq_mapping) begin
    pending_lq_cancel_count += main_tr.numLsElem;
end
if (had_sq_mapping) begin
    pending_sq_cancel_count += main_tr.numLsElem;
end
```

```systemverilog
function void memblock_lsqenq_dispatch_sequence::apply_pending_lsq_cancels();
    ensure_helpers();
    if (data.pending_lq_cancel_count != 0) begin
        // redirect flush后回退软件LSQ admission镜像，确保重入队仍与DUT response key一致。
        lsq_ctrl.cancel_lq(data.pending_lq_cancel_count);
        data.pending_lq_cancel_count = 0;
    end
    if (data.pending_sq_cancel_count != 0) begin
        // redirect flush后回退软件SQ admission镜像，后续uid从公共高水位顺序重入队。
        lsq_ctrl.cancel_sq(data.pending_sq_cancel_count);
        data.pending_sq_cancel_count = 0;
    end
endfunction
```

为什么添加：reissue 沿用主表 key，但 `commit_allocate_with_resp()` 仍会用当前软件 enq 指针预测 DUT response。flush 后不回退软件镜像，会导致下一次 admission 预测落在 flush 前的后续位置。

输入输出：

- 输入：被 flush uid 的 active LQ/SQ mapping 和 `numLsElem`。
- 输出：`lsq_ctrl_model` 的 `lq_enq_ptr/sq_enq_ptr/free_count` 回退。

调用关系：

| 调用顺序 | 函数 | 在本流程中的功能 |
|---|---|---|
| 1 | `prepare_uid_for_redirect_reissue()` | redirect 清理 uid 时判断旧实例是否占用 LQ/SQ，并把需要撤销的元素数累计到 pending cancel counter。 |
| 2 | `send_lsqenq_cycle()` | 下一拍 LSQ admission 主入口；在尝试新入队前先处理 pending cancel，避免软件镜像继续从旧指针分配。 |
| 3 | `apply_pending_lsq_cancels()` | 消费 `pending_lq_cancel_count/pending_sq_cancel_count`，调用 `lsq_ctrl.cancel_lq/cancel_sq` 回退软件 LSQ 镜像。 |
| 4 | `lsq_ctrl.cancel_lq()` / `lsq_ctrl.cancel_sq()` | 按环形 LQ/SQ 指针规则回退本地 enqueue pointer 和 free count，使后续预览 key 与 DUT flush 后状态一致。 |

## 5. route 扫描优化

功能：route 不再保存本地 success 前缀，每拍只扫描公共活跃窗口内最多 `MEMBLOCK_REAL_LSQ_ENQ_MAX` 个 uid。

修改前逻辑：

- issue route 维护自己的扫描前缀，或者按较大范围扫描状态表。
- redirect 发生后，route 前缀可能和 LSQ admission 上界不一致。
- 10 万笔场景下，每拍扫描过多 uid 会让 service loop 变重。

修改后逻辑：

- 每拍 route 前先推进公共 `success_prefix_uid`。
- 扫描范围限制在公共活跃窗口 `[success_prefix_uid, max_enqueued_uid + 1)`。
- 单拍最多扫描 `seq_csr_common::get_real_lsq_enq_max()` 个 uid，默认按真实 LSQ 入队上限控制。

正确性检查：

| 检查点 | 预期行为 |
|---|---|
| 不从 0 反复扫 | route 从 `success_prefix_uid` 开始。 |
| 不扫未入队 uid | route 到 `get_active_scan_end_uid()` 截止。 |
| 不漏 redirect reissue | redirect 回退 LSQ admission 上界后，被 flush uid 重新落入活跃窗口。 |
| 每拍扫描量受控 | `scanned < scan_limit` 限制 route 成本。 |

源码片段：

```systemverilog
data.advance_success_prefix();
begin_uid = data.get_active_scan_begin_uid();
end_uid   = data.get_active_scan_end_uid();
scan_limit = seq_csr_common::get_real_lsq_enq_max();
scanned = 0;
// route只在公共活跃窗口内做有限扫描，避免10万笔请求每拍全表遍历。
for (uid = begin_uid;
     uid < end_uid && scanned < scan_limit;
     uid++) begin
    route_uid(uid);
    scanned++;
end
```

为什么添加：route 原来维护本地前缀，redirect 后容易和 admission/flush 边界不一致。公共窗口保证 route 只看已 admission 且未连续 success 的区域。

输入输出：

- 输入：公共 progress、status 表。
- 输出：LOAD/STA/STD issue queue item。

调用关系：

| 调用顺序 | 函数 | 在本流程中的功能 |
|---|---|---|
| 1 | `route_all_issue_queues()` | real smoke 每拍调度入口；驱动 issue scheduler 扫描 ready uid 并填充 LOAD/STA/STD issue queue。 |
| 2 | `issue_queue_scheduler::route_all_ready_uids()` | 从公共活跃窗口开始，最多扫描 `real_lsq_enq_max` 个 uid，避免 100k 场景每拍全表扫描。 |
| 3 | `route_uid()` | 判断单个 uid 的 op 行为，决定需要进入 LOAD、STA、STD 中哪些发射队列。 |
| 4 | `route_target()` | 为某个具体 target 生成 issue queue item，删除同 target 旧残留项，并置对应 queued 状态。 |

## 6. commit cursor 和 success 终态

功能：commit cursor 只跳过 `success=1` 的 uid，不把 `flushed` 当完成。`try_retire_committed_uid()` 设置 success 时通过公共 setter 推进 success 前缀。

修改前逻辑：

- `flushed` 可能被当成一种“这条 transaction 不再需要处理”的状态。
- commit cursor 如果跳过 flushed uid，completion 可能继续向后推进。

修改后逻辑：

- `flushed=1` 只表示旧动态实例被 redirect kill。
- 被 flush 的 uid 必须重新 admission、issue、writeback、commit/deq。
- commit cursor 只跨过 `success=1` 的 uid。

正确性检查：

| 场景 | 正确行为 |
|---|---|
| uid1 被 flush | uid1 不能被 commit cursor 跳过。 |
| uid2 旧路径已经 pass | 如果 uid2 被 redirect 覆盖，必须清状态并重发。 |
| all success 判断 | 只有连续 `success=1` 才推进 `success_prefix_uid`。 |

源码片段：

```systemverilog
// flushed不是终态，不能被commit cursor当作完成项跳过；它必须先redirect reissue并最终success。
if (status.success) begin
    commit_cursor_uid++;
end else begin
    break;
end
```

```systemverilog
set_status_field(uid,
                 MEMBLOCK_STATUS_SUCCESS,
                 !status.fault &&
                 !status.exception_pending &&
                 !status.replay_pending &&
                 !status.redirect_pending &&
                 !status.flushed);
```

为什么添加：`flushed=1` 表示旧动态实例被 kill，不代表 transaction 已验证完成。只有 final commit/deq 后 `success=1` 才能推进公共 success 前缀。

输入输出：

- 输入：writeback/pass、ROB commit、LQ/SQ deq 状态。
- 输出：`success=1` 和 `dispatch_progress.success_prefix_uid` 推进。

## 7. redirect 优先的 event 仲裁

功能：real smoke 每轮 collect CSR、writeback/IQ feedback、exception/redirect raw event，并把标准事件交给对应 handler。normal pass/fault 在 `writeback_status_handler::handle_event()` 中直接落状态；replay/redirect/fault 入 `exception_event_q` 后只由 recovery handler 消费。writeback handler 保留 active redirect 防御过滤。

修改前逻辑：

- `service_monitor_once()` 可能先处理普通 writeback/pass，再处理 redirect。
- 如果同一拍既有 writeback 又有 memoryViolation/redirect，旧路径 pass 可能先写入状态表。

修改后逻辑：

- 每拍先 collect CSR、writeback、exception/redirect raw event；writeback/IQ feedback 在 collect 阶段已经由 `handle_event()` 分类。
- 再由 `exception_redirect_replay_task()` 消费 `exception_event_q`，选择 oldest redirect 或处理 replay/fault recovery。
- 当前简化实现删除了 `writeback_status_task()`；普通 pass 在 `writeback_status_handler::handle_event()` 中直接处理，fault/replay/redirect 入 `exception_event_q` 后只由 recovery handler 消费。
- 已经被 active redirect 覆盖的 feedback 会被过滤。

正确性检查：

| 检查点 | 预期行为 |
|---|---|
| 同拍 redirect/writeback | writeback 在 collect 阶段可能已先落 pass；redirect recovery 后通过 active redirect、flush/reissue 和 stale 过滤修正旧动态实例。 |
| 已存在 active redirect 时的被覆盖 pass | 不允许继续写成 `status.pass=1`。 |
| redirect 同批未覆盖 recovery 事件 | 重新排回 feedback queue，后续继续处理。 |
| active redirect 未完成 | writeback handler 过滤被 active redirect 覆盖的普通反馈。 |

源码片段：

```systemverilog
// writeback/IQ feedback/memoryViolation raw event 采集后立即进入
// writeback_status_handler::handle_event() 分类。normal pass/fault 在该入口落状态，
// replay/redirect/fault 入 exception_event_q 后只由 exception_redirect_replay_task() 消费。
collect_runtime_context_events();
collect_writeback_events();
collect_exception_and_redirect_events();
exception_redirect_replay_task();
```

```systemverilog
if (!event_is_redirect(wb_event) &&
    event_blocked_by_active_redirect(wb_event)) begin
    `uvm_info("WB_STATUS", ...)
    return 1'b0;
end
```

redirect 同批事件处理：

```systemverilog
if (event.has_rob &&
    rob_order_util::rob_need_flush(event.rob_key, redirect)) begin
    `uvm_info("EXC_REDIRECT", ...)
    continue;
end
data.exception_event_q.push_front(event);
```

为什么添加：MemBlock 可能同拍产生 memoryViolation/redirect 和 writeback。当前简化实现不再通过 `writeback_status_task()` 做同批普通 pass 延迟处理，因此需要保留 active redirect stale 过滤，并由 redirect flush/reissue 逻辑清理已被覆盖 uid 的旧动态实例结果。

输入输出：

- 输入：本批 recovery events 和 active redirect 状态。
- 输出：oldest redirect 被 request/push drive；被覆盖 recovery event 丢弃，未覆盖 recovery event 延后处理。

## 8. 完成条件

功能：`flushed` 不再是合法终态，real smoke 必须等所有 uid success。10 万笔场景下，每拍结束判断不能再全表扫描 status，因此完成条件改成“先检查全局队列/map/pending 状态为空，再用公共 success 前缀判断是否所有 uid 已连续 success”。

修改前逻辑：

- 完成条件可能遍历 `0..main_trans_num` 检查每个 status。
- `flushed` 曾被视为可跳过的状态。
- raw monitor queue 中最后一批事件如果还没消费，理论上可能被完成判断绕过。

修改后逻辑：

- 先检查 issue queue、feedback queue、raw monitor queue、active map、redirect/flush/PTW 等全局 pending 状态。
- 再推进公共 `success_prefix_uid`。
- 只有 `success_prefix_uid >= main_trans_num` 才结束。

正确性检查：

| 检查点 | 预期行为 |
|---|---|
| raw event 未消费 | `raw_monitor_queue_size()!=0` 时不能结束。 |
| issue queue 未空 | LOAD/STA/STD 任一队列非空时不能结束。 |
| redirect recovery 未完成 | active redirect、pending drive、flush 标志存在时不能结束。 |
| 10 万笔性能 | 正常完成判断不再每拍全表扫描。 |

源码片段：

```systemverilog
if (data.load_issue_q.size() != 0 ||
    data.sta_issue_q.size()  != 0 ||
    data.std_issue_q.size()  != 0) begin
    return 1'b0;
end
if (data.exception_event_q.size() != 0 ||
    memblock_sync_pkg::raw_monitor_queue_size() != 0) begin
    return 1'b0;
end
...
// 10万笔场景下不能每拍全表扫描；success前缀只能跨过真正success的uid，
// 因此到达main_trans_num即可说明所有transaction都已最终完成。
data.advance_success_prefix();
return data.dispatch_progress.success_prefix_uid >= data.main_trans_num;
```

为什么添加：redirect reissue 是固定框架行为，被 flush uid 必须重新 admission/issue/commit。允许 flushed 终态会漏掉恢复路径验证；而每拍从 0 扫到 `main_trans_num` 会让 10 万笔 testcase 的 service loop 成本过高。`success_prefix_uid` 只能由连续 `success=1` 推进，中间只要存在 active、redirect_pending、flushed、未 success 的 uid，前缀就停住，所以它可以作为 O(1) 完成判断的事实源。

输入输出：

- 输入：issue queue 是否为空、内部 feedback queue 是否为空、raw monitor queue 是否为空、active map 是否为空、redirect/flush/PTW wait 等全局状态、`dispatch_progress.success_prefix_uid`。
- 输出：是否结束 real smoke service loop。

当前限制：`report_unfinished_status()` 仍会全表扫描，这是 timeout/debug 路径，保留用于定位未完成 uid；正常每拍 `all_transactions_success()` 已不再全表扫描。

## 9. 补充覆盖审查：动态实例、旧事件过滤和状态清理

本节用于补齐本轮实现中容易被忽略但实际参与功能闭环的源码点。前面 1-8 节已经覆盖主流程，这里重点审查“redirect 后旧动态实例如何失效”“旧 feedback 为什么不会污染新状态”“公共状态为什么能支撑 100k 场景”。

### 9.1 `dynamic_epoch`

修改前逻辑：同一个 `uid` 在 redirect 前后没有显式动态实例版本标记。debug 时只能通过 `replay_seq/issue_epoch/active map` 推断这是不是旧实例事件。

修改后逻辑：redirect reissue 准备阶段递增 `dynamic_epoch`，表示同一个静态 uid 进入了下一轮动态执行。

正确性检查：`dynamic_epoch` 当前不作为 admission 或 completion 的推进条件，因此不会改变主流程；它主要用于 debug，明确记录“同一个 uid 已经因为 redirect 进入下一轮动态执行”。当前实现没有把 `dynamic_epoch` 写进 monitor event，也没有依赖它过滤旧 event。

源码片段：

```systemverilog
// 同一uid被redirect reissue后产生新动态实例；递增后可区分旧实例事件。
int unsigned        dynamic_epoch;
```

```systemverilog
status.dynamic_epoch++;
```

字段含义：`uid` 是测试框架主表里的静态编号，同一个 uid 在 redirect 后会重新 admission、重新 issue、重新等待 writeback/commit。从软件状态机角度看，这已经不是同一个动态实例。`dynamic_epoch` 就是给同一个 uid 的多轮动态实例做版本标记。

为什么添加：更准确地说，当前风险主要不是“状态已经清理后，raw queue 又晚到旧 feedback”。当前 service flow 会先采集 writeback 和 redirect raw event，再由 `exception_redirect_replay_task()` 消费 pending recovery 事件；普通 pass 已在 adapter 调用 `handle_event()` 时处理，因此主要依赖 active map、`issue_epoch`、`replay_seq`、`requeue_events_not_flushed_by_redirect()` 和 `event_blocked_by_active_redirect()` 过滤旧路径反馈。

`dynamic_epoch` 当前不参与上述过滤，因为 monitor event 里还没有携带该字段。它的实际价值是把“同一 uid 已经发生过 redirect reissue”显式记录下来，便于 debug；后续如果 driver/monitor event 能携带动态实例版本，再考虑用它做更强匹配。

在本特性中的作用：当 `prepare_uid_for_redirect_reissue()` 执行时递增，表示旧实例已经失效，新一轮 admission/issue 应被视为新的动态执行。它不直接推进 admission，也不直接决定 success；真正推进仍由 `success_prefix_uid` 和 `max_enqueued_uid` 完成。

### 9.2 `clear_uid_dispatch_result()`

修改前逻辑：redirect flush 需要清除多个状态位，但如果清理分散在不同 task 中，容易漏掉某些旧结果，例如 `pass/writeback/rob_commit/lsq_deq`。

修改后逻辑：集中用 `clear_uid_dispatch_result()` 清旧动态实例结果。它只清过程状态和结果状态，不删除主表 transaction，因此同一个 uid 后续还能重新 admission。

正确性检查：

| 检查点 | 预期行为 |
|---|---|
| 旧 pass 不污染新实例 | `writeback/pass/load_pass/sta_pass/std_pass` 全部清 0。 |
| 旧 commit/deq 不作为完成 | `rob_commit/lsq_deq/success` 清 0。 |
| 旧 issue item 不再可发射 | `issue_killed=1`，并配合队列清理阻断旧路径。 |
| 主表不丢失 | 只改 status，不删除 `main_table_by_uid`。 |

源码片段：

```systemverilog
function void clear_uid_dispatch_result(input memblock_uid_t uid);
    status_transaction status;

    status = get_status(uid);
    status.enq             = 1'b0;
    status.issue_ready     = 1'b0;
    status.queued_load     = 1'b0;
    status.queued_sta      = 1'b0;
    status.queued_std      = 1'b0;
    status.load_dispatched = 1'b0;
    status.sta_dispatched  = 1'b0;
    status.std_dispatched  = 1'b0;
    status.load_writeback  = 1'b0;
    status.sta_writeback   = 1'b0;
    status.std_writeback   = 1'b0;
    status.load_pass       = 1'b0;
    status.sta_pass        = 1'b0;
    status.std_pass        = 1'b0;
    status.writeback       = 1'b0;
    status.pass            = 1'b0;
    status.issue_killed    = 1'b1;
    status.exception_pending = 1'b0;
    status.replay_pending  = 1'b0;
    status.replay_target_load = 1'b0;
    status.replay_target_sta = 1'b0;
    status.replay_target_std = 1'b0;
    status.redirect_pending = 1'b0;
    status.rob_commit      = 1'b0;
    status.lsq_deq         = 1'b0;
    status.success         = 1'b0;
    status.fault           = 1'b0;
    status.load_fault      = 1'b0;
    status.sta_fault       = 1'b0;
    status.std_fault       = 1'b0;
    status.exception_vec   = '0;
    status.exception_vaddr = '0;
    status.exception_gpaddr = '0;
endfunction:clear_uid_dispatch_result
```

函数功能：清掉某个 uid 旧动态实例在 dispatch、issue、writeback、fault、commit/deq 上留下的结果位，但不删除主表 transaction 本身。主表是静态激励来源，redirect 后需要重新用同一个 uid 和主表内容再次进入 LSQ/admission/issue 流程。

为什么添加：如果 redirect 命中 uid2，而 uid3 之前已经在旧路径上写回 pass，那么 uid3 的 pass/writeback 不能继续作为最终结果。否则顺序 commit 可能把旧路径结果错误提交，导致访存违例场景漏测。该函数把旧动态实例的“过程结果”全部清空，保证后续必须重新执行。

输入输出：

- 输入：需要重发的 `uid`。
- 输出：该 uid 的旧 `enq/queued/dispatched/writeback/pass/fault/commit/deq/success` 状态被清空，`issue_killed=1` 用于阻止旧 issue item 继续被发射。

在调用链中的位置：

| 调用顺序 | 函数 | 在本流程中的功能 |
|---|---|---|
| 1 | `exception_redirect_replay_handler::advance_active_redirect()` | 检查 redirect drive 完成情况，完成后触发公共状态 flush。 |
| 2 | `common_data_transaction::apply_redirect_flush()` | redirect flush 的公共入口，负责调用范围清理并最终释放 flush 状态。 |
| 3 | `apply_redirect_flush_range()` | 在已 admission 的活跃窗口中找出所有被 redirect 覆盖的 uid。 |
| 4 | `prepare_uid_for_redirect_reissue()` | 对单个被 flush uid 做 reissue 准备。 |
| 5 | `retire_active_uid()` | uid 已 active 时，删除 ROB/LQ/SQ active map 并清 active 映射状态。 |
| 5 | `remove_uid_from_issue_queues()` | uid 尚未 active 或仍残留在 issue queue 时，删除 LOAD/STA/STD 队列中的旧待发射项。 |
| 6 | `clear_uid_dispatch_result()` | 清旧动态实例的 enq/queued/dispatched/writeback/pass/commit/deq/success 等结果位。 |

### 9.3 `prepare_uid_for_redirect_reissue()` 的完整职责

修改前逻辑：redirect 命中 uid 后，清 active map、清 issue queue、清 writeback/pass、回退 LSQ 软件镜像等动作没有统一入口，review 时很难确认是否所有旧路径痕迹都处理了。

修改后逻辑：`prepare_uid_for_redirect_reissue()` 成为 redirect reissue 的单一准备入口。它负责摘 active map、删 issue queue、累计 LSQ cancel、清旧状态、置 `redirect_pending/flushed`、递增 `dynamic_epoch`。

正确性检查：

| 检查点 | 预期行为 |
|---|---|
| 已 active 的 uid | 通过 `retire_active_uid()` 从 ROB/LQ/SQ active map 删除。 |
| 未 active 但还在 issue queue 的 uid | 通过 `remove_uid_from_issue_queues()` 删除旧待发射项。 |
| LSQ 软件镜像 | 按 `numLsElem` 累计 pending cancel。 |
| 后续重入队 | 设置 `redirect_pending/flushed`，等待 LSQ admission 上界回退后重新 admission。 |

源码片段：

```systemverilog
function void prepare_uid_for_redirect_reissue(input memblock_uid_t uid,
                                               input memblock_redirect_payload_t redirect);
    ...
    had_lq_mapping = status.active_lq_mapped;
    had_sq_mapping = status.active_sq_mapped;
    // redirect命中的旧动态实例不再等待writeback/commit；清queue/map后等待同uid重新admission。
    if (status.active) begin
        retire_active_uid(uid);
    end else begin
        remove_uid_from_issue_queues(uid);
    end
    // 记录需要回退的软件LSQ admission镜像；LSQ sequence恢复后统一调用cancel_lq/cancel_sq消费。
    if (had_lq_mapping) begin
        pending_lq_cancel_count += main_tr.numLsElem;
    end
    if (had_sq_mapping) begin
        pending_sq_cancel_count += main_tr.numLsElem;
    end
    clear_uid_dispatch_result(uid);
    status.redirect_pending = 1'b1;
    status.flushed          = 1'b1;
    status.dynamic_epoch++;
    status.active           = 1'b0;
    status.success          = 1'b0;
    status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
endfunction:prepare_uid_for_redirect_reissue
```

函数功能：这是 redirect reissue 的核心准备函数。它不是简单把 uid 标记为 flushed，而是把旧动态实例从 active map、issue queue、LSQ 软件镜像、状态结果中全部摘掉，然后让同一个 uid 处于“等待重新 admission”的状态。

为什么这么做：

- `retire_active_uid()`：如果 uid 仍在 active ROB/LQ/SQ map 中，需要移除 map，避免后续旧事件继续通过 ROB/LQ/SQ key 反查到这个 uid。
- `remove_uid_from_issue_queues()`：如果 uid 只是排在 LOAD/STA/STD issue queue 中但还没真正 fire，需要从队列删除，避免 redirect 后旧路径继续发射。
- `pending_lq_cancel_count/pending_sq_cancel_count`：记录软件 LSQ 镜像需要回退多少元素，保证下一轮 admission 的 LQ/SQ 分配和 DUT 侧 flush 后状态一致。
- `clear_uid_dispatch_result()`：清掉旧动态实例已经产生的 pass/writeback/commit/deq 结果。
- `redirect_pending/flushed`：表示这个 uid 不是完成，而是处于 redirect recovery 等待重新入队。
- `dynamic_epoch++`：记录该 uid 已经进入下一轮动态实例。

该函数在本特性中承担的作用：把“redirect 后重新发射”从一个模糊状态变成明确流程。后续 LSQ admission 会因为 LSQ admission 上界回退重新拿到该 uid；`set_status_field(MEMBLOCK_STATUS_ENQ, 1)` 会清掉 `redirect_pending/flushed/issue_killed`，让它重新进入正常 route/issue 路径。

### 9.4 `pending_lq_cancel_count` / `pending_sq_cancel_count`

修改前逻辑：redirect 清了 uid 状态，但软件 LSQ 模型已经推进过 LQ/SQ 指针。如果不回退，重发 uid 时本地预期 key 和 DUT flush 后 key 可能不一致。

修改后逻辑：redirect 阶段只累计待 cancel 数，LSQ enqueue sequence 在下一轮发送前调用 `apply_pending_lsq_cancels()`，由 LSQ 模型自己的接口完成回退。

正确性检查：这两个 counter 只代表测试框架软件镜像的回退请求，不代表 DUT 端口。它们在 `apply_pending_lsq_cancels()` 中消费后清 0，避免重复 cancel。

源码片段：

```systemverilog
int unsigned               pending_lq_cancel_count;
int unsigned               pending_sq_cancel_count;
```

```systemverilog
if (had_lq_mapping) begin
    pending_lq_cancel_count += main_tr.numLsElem;
end
if (had_sq_mapping) begin
    pending_sq_cancel_count += main_tr.numLsElem;
end
```

```systemverilog
function void memblock_lsqenq_dispatch_sequence::apply_pending_lsq_cancels();
    ensure_helpers();
    if (data.pending_lq_cancel_count != 0) begin
        // redirect flush后回退软件LSQ admission镜像，确保重入队仍与DUT response key一致。
        lsq_ctrl.cancel_lq(data.pending_lq_cancel_count);
        data.pending_lq_cancel_count = 0;
    end
    if (data.pending_sq_cancel_count != 0) begin
        // redirect flush后回退软件SQ admission镜像，后续uid从公共高水位顺序重入队。
        lsq_ctrl.cancel_sq(data.pending_sq_cancel_count);
        data.pending_sq_cancel_count = 0;
    end
endfunction:apply_pending_lsq_cancels
```

字段含义：这两个计数器不是 DUT 信号，而是测试框架软件 LSQ 镜像的回退请求。`pending_lq_cancel_count` 表示需要从本地 LQ admission 模型里撤销多少个元素；`pending_sq_cancel_count` 表示需要从本地 SQ admission 模型里撤销多少个元素。

为什么添加：redirect 后 DUT 会 flush 一段 ROB 后续指令，相当于对应 LQ/SQ admission 也不再有效。测试框架之前已经把这些 uid 分配给本地 `lsq_ctrl_model`，如果不回退，重发同一 uid 时本地模型会继续向后分配，导致预期 LQ/SQ key 与 DUT response 不一致。

在本特性中的作用：`prepare_uid_for_redirect_reissue()` 只累计 cancel 数，不直接操作 `lsq_ctrl_model`；真正回退在 `memblock_lsqenq_dispatch_sequence::send_lsqenq_cycle()` 开头执行。这样可以避免 redirect handler 和 LSQ admission sequence 并发直接改同一个模型对象。

### 9.5 `normalize_feedback_event()` / `resolve_uid_for_event()` 的 stale event 基础过滤

修改前逻辑：monitor raw event 可能只带 ROB/LQ/SQ key。如果不通过 active map 统一解析 uid，旧事件可能按不完整信息直接更新状态。

修改后逻辑：所有 feedback 先 normalize。`resolve_uid_for_event()` 通过 active ROB/LQ/SQ map 反查 uid，且多个 key 同时存在时必须反查到同一个 uid；反查失败则丢弃事件。

正确性检查：

| 检查点 | 预期行为 |
|---|---|
| 旧实例已 retire active | active map 查不到，event 被丢弃。 |
| ROB/LQ/SQ key 不一致 | 触发 fatal，避免错误归属 uid。 |
| replay 后事件缺快照 | replay_seq 非 0 且缺 issue_epoch/replay_seq 时丢弃或警告。 |
| 普通事件缺 uid | normalize 后补齐 uid、ROB key、issue_epoch、replay_seq。 |

源码片段：

```systemverilog
function bit normalize_feedback_event(input memblock_wb_event_t wb_event,
                                      output memblock_wb_event_t normalized_event);
    ...
    if (!resolve_uid_for_event(normalized_event, uid)) begin
        normalized_event = make_empty_wb_event();
        return 1'b0;
    end
    status = get_status(uid);
    normalized_event.uid     = uid;
    normalized_event.has_uid = 1'b1;
    ...
    if (!normalized_event.has_replay_seq) begin
        normalized_event.replay_seq = status.replay_seq;
        normalized_event.has_replay_seq = 1'b1;
    end
    return 1'b1;
endfunction:normalize_feedback_event
```

```systemverilog
function bit resolve_uid_for_event(input memblock_wb_event_t wb_event,
                                   output memblock_uid_t uid);
    ...
    if (wb_event.has_rob) begin
        if (!lookup_active_uid_by_rob(wb_event.rob_key, rob_uid)) begin
            return 1'b0;
        end
        ...
        uid = rob_uid;
        have_uid = 1'b1;
    end
    if (wb_event.has_lq) begin
        if (!lookup_active_uid_by_lq(wb_event.lq_key, lq_uid)) begin
            return 1'b0;
        end
        ...
    end
    if (wb_event.has_sq) begin
        if (!lookup_active_uid_by_sq(wb_event.sq_key, sq_uid)) begin
            return 1'b0;
        end
        ...
    end
    return have_uid;
endfunction:resolve_uid_for_event
```

函数功能：monitor 采到的 raw event 可能只有 ROB/LQ/SQ key，不一定直接带测试框架 uid。`resolve_uid_for_event()` 通过当前 active map 反查 uid，并要求 ROB/LQ/SQ 多个 key 如果同时存在必须指向同一个 uid；`normalize_feedback_event()` 在此基础上补齐 uid、ROB key、issue_epoch、replay_seq 等状态处理需要的字段。

为什么对 100k/redirect 重要：redirect reissue 后，旧动态实例会被 `retire_active_uid()` 从 active map 删除。旧 feedback 如果晚到，通常无法再通过 active ROB/LQ/SQ map 反查 uid，会在这里被丢弃。这样可以避免旧实例事件污染新实例状态。

当前边界：如果 DUT 很快复用相同 ROB/LQ/SQ key，旧事件理论上可能重新命中新实例。当前实现还依赖 `issue_epoch/replay_seq`、active redirect 覆盖过滤和顺序 admission 限制共同降低风险；`dynamic_epoch` 已预留给后续更强版本匹配。

### 9.6 `event_blocked_by_active_redirect()`

修改前逻辑：writeback handler 只看当前事件自身，可能在 active redirect 尚未完成时继续处理普通 pass。

修改后逻辑：旧的 pending queue 二次整理入口已经删除；writeback handler 处理单个普通事件时，检查该事件是否被 active redirect 覆盖。

正确性检查：被 active redirect 覆盖的事件会被丢弃，未覆盖的普通 pass 可按版本检查正常落表。

```systemverilog
function bit event_blocked_by_active_redirect(input memblock_wb_event_t wb_event);
    ensure_data();
    if (!data.active_redirect.valid || !wb_event.has_rob) begin
        return 1'b0;
    end
    return rob_order_util::rob_need_flush(wb_event.rob_key, data.active_redirect);
endfunction:event_blocked_by_active_redirect
```

函数功能：这组逻辑保证被当前 active redirect 覆盖的 writeback/pass 不会继续更新状态。`writeback_status_handler` 不再消费 `exception_event_q`；pending recovery 事件只由 `exception_redirect_replay_handler::process_pending_events()` 处理。

为什么添加：同一拍可能采到 writeback 和 redirect，或者上一阶段已经 request redirect 但还没 apply flush。此时如果普通 pass 先落状态表，就可能把稍后应该被 flush 的 uid 标成 pass/writeback。该防线保留在单事件处理入口上。

在本特性中的作用：配合 `service_monitor_once()` 的 collect + recovery 单消费者顺序，确保 pending recovery 由 exception handler 统一仲裁，同时普通 writeback 入口仍能过滤 active redirect 覆盖的旧反馈。

### 9.7 `requeue_events_not_flushed_by_redirect()`

修改前逻辑：同一批 feedback 中选出 redirect 后，非 redirect 事件如果简单全部放回队列，可能把已经被 redirect 覆盖的 pass 留给 writeback handler。

修改后逻辑：重新入队前用 `rob_need_flush()` 判断该事件是否被 selected redirect 覆盖。覆盖则丢弃；未覆盖才 push 回 queue。

正确性检查：

| 场景 | 预期行为 |
|---|---|
| uid1 redirect 覆盖 uid2 pass | uid2 pass 被 drop，uid2 后续重发。 |
| uid0 pass 不在 redirect 覆盖范围 | uid0 pass 重新入队，后续正常处理。 |
| redirect event 本身 | 不重复 requeue，避免重复 request redirect。 |

源码片段：

```systemverilog
function void requeue_events_not_flushed_by_redirect(input memblock_wb_event_t events[$],
                                                     input memblock_redirect_payload_t redirect);
    for (int idx = events.size(); idx > 0; idx--) begin
        memblock_wb_event_t wb_item;

        wb_item = events[idx - 1];
        if (event_is_redirect(wb_item)) begin
            continue;
        end
        if (wb_item.has_rob &&
            rob_order_util::rob_need_flush(wb_item.rob_key, redirect)) begin
            `uvm_info("EXC_REDIRECT",
                      $sformatf("drop stale feedback covered by redirect uid=%0d source=%0d rob=%0d/%0d",
                                wb_item.uid,
                                wb_item.source,
                                wb_item.rob_key.flag,
                                wb_item.rob_key.value),
                      UVM_LOW)
            continue;
        end
        data.exception_event_q.push_front(wb_item);
    end
endfunction:requeue_events_not_flushed_by_redirect
```

函数功能：当一批 feedback 事件中选出了 oldest redirect 后，这个函数把没有被该 redirect 覆盖的非 redirect 事件重新塞回队列；被 redirect 覆盖的事件直接丢弃。

为什么添加：redirect 不是只影响触发它自己的那条 transaction，而是影响 ROB 顺序上需要 flush 的后续 transaction。如果同一批 events 中还有 uid3 的 pass，但 oldest redirect 会 flush uid3，那么 uid3 的 pass 必须丢弃，不能等 writeback handler 后续处理。

在本特性中的作用：这是“redirect 后从最老 pending uid 重新执行”的事件侧配套逻辑。状态侧会清 uid，admission 侧会回退 LSQ admission 上界；事件侧必须丢掉旧路径 feedback，三者缺一不可。

### 9.8 `MEMBLOCK_STATUS_ENQ` / `MEMBLOCK_STATUS_SUCCESS` setter 的副作用

修改前逻辑：`status.enq/status.success` 只是普通 bit，其他模块需要额外记得更新 LSQ admission 上界或 success 前缀。

修改后逻辑：通过 `set_status_field()` 设置 `ENQ/SUCCESS` 时自动更新公共 progress。`ENQ` 成功推进 LSQ admission 上界，`SUCCESS` 成功推进 success 前缀。

正确性检查：所有新增代码必须通过公共 setter 更新这两个字段，不能直接写 `status.enq/status.success`。否则公共 progress 不会同步推进。

源码片段：

```systemverilog
MEMBLOCK_STATUS_ENQ: begin
    old_value = status.enq;
    status.enq = value;
    if (value && !old_value) begin
        mark_uid_enqueued(uid);
        // redirect reissue重新admission成功后，旧动态实例的flush标志不再阻塞route/commit。
        if (status.redirect_pending || status.flushed) begin
            status.redirect_pending = 1'b0;
            status.flushed          = 1'b0;
            status.issue_killed     = 1'b0;
        end
    end
end
```

```systemverilog
MEMBLOCK_STATUS_SUCCESS: begin
    status.success = value;
    if (value) begin
        advance_success_prefix();
    end
end
```

逻辑含义：这两个状态字段现在不再只是简单 bit 赋值。`ENQ` 从 0 到 1 表示 uid 成功完成 admission，所以必须推进 `max_enqueued_uid`，并且如果这是 redirect reissue，则清掉阻塞新实例的 `redirect_pending/flushed/issue_killed`。`SUCCESS` 从 0 到 1 表示 uid 真正完成最终闭环，所以可以尝试推进连续 success 前缀。

为什么添加副作用：如果每个 task 各自维护 LSQ admission 上界和 success cursor，很容易在 redirect/reissue 之后不一致。把副作用放在公共 setter 里，可以保证无论是 LSQ admission 还是 commit handler，只要通过公共状态接口更新字段，就会同步更新公共 progress。

风险检查：这要求后续新增代码不要绕过 `set_status_field()` 直接写 `status.enq/status.success`。否则公共 progress 不会更新。文档和源码注释已经把这两个字段标成带副作用的核心状态。

### 9.9 `lsq_commit_handler` 的 flushed 语义修正

修改前逻辑：commit cursor 可能把 `flushed` 当成可跳过状态。

修改后逻辑：commit cursor 只跳过 `success=1`。`flushed=1` 表示旧动态实例被 kill，必须等待 reissue 后最终 success。

正确性检查：redirect 后如果 uid1 被 flush，即使 uid2 旧路径已经 pass，success 前缀 也会停在 uid1，直到 uid1 重新完成。

源码片段：

```systemverilog
// flushed不是终态，不能被commit cursor当作完成项跳过；它必须先redirect reissue并最终success。
if (status.success) begin
    commit_cursor_uid++;
end else begin
    break;
end
```

函数功能：commit cursor 只跨过已经 `success=1` 的 uid。`flushed=1` 不再代表这条 transaction 可以结束。

为什么修改：本轮方案明确 redirect 后不是丢弃 transaction，而是同 uid 重新入队、重新发射、重新完成。如果 commit cursor 把 flushed 当成完成，后续完成判断可能越过被 flush 的 uid，导致 100k 流程继续向后跑但漏掉 reissue 验证。

### 9.10 文档同步覆盖范围

本次实现除了源码 review 文档，还同步更新了对应源码分析文档，覆盖文件如下：

- `AI_DOC/analysis/source_sv/dispatch_framework_sv/common_data_transaction.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/exception_redirect_replay_handler.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/issue_queue_scheduler.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/lsq_commit_handler.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/lsq_ctrl_model.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_dispatch_real_smoke_sequence.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_dispatch_types.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_lsqenq_dispatch_sequence.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/status_transaction.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/writeback_status_handler.md`

覆盖性结论：本轮源码修改涉及的 9 个 SV 文件在本 review 文档 1-9 节均已有对应源码和设计分析；关联 framework 分析文档也已同步更新。剩余未在本 review 文档逐函数展开的内容主要是原有函数的上下文逻辑，例如 issue candidate 选择、LSQ key advance、commit deq 原始流程，这些不是本轮新增/修改的核心逻辑，已保留在各自源码分析文档中。

## 10. 验证记录

已执行仿真：

```text
make eda_run tc=tc_dispatch_real_smoke cfg=tc_dispatch_real_smoke mode=base_fun wave=null timeout_ns=2000000
```

日志路径：

```text
mem_ut/ver/ut/memblock/sim/base_fun/log/tc_dispatch_real_smoke_666666_rtl_.log
```

结果：

```text
TEST CASE PASSED
UVM_WARNING : 0
UVM_ERROR   : 0
UVM_FATAL   : 0
```

验证结论：基础 real smoke flow 已通过，说明新增公共 progress、LSQ admission 上界、route 窗口、completion 判断没有破坏现有基础闭环。当前还建议后续补充 memoryViolation/redirect 定向 testcase，用于覆盖 `prepare_uid_for_redirect_reissue()`、LSQ admission 上界回退 和 stale feedback drop 的完整动态路径。
