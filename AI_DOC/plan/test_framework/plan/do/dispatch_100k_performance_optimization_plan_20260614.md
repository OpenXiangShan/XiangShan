# Dispatch 10万笔请求性能与 redirect reissue 优化实施方案

## 1. 目标

后续 testcase 需要支持 10 万笔访存 transaction。当前 dispatch flow 中多处存在 `0..main_trans_num` 全表扫描，在大规模请求下性能不可接受。

本方案解决两个问题：

1. redirect 后被 flush 的 transaction 不能直接取消，必须重新发射，保证访存违例恢复路径能被验证。
2. route、redirect flush、重新发射查表都不能每拍从 0 扫到 `main_trans_num`，需要使用 `common_data_transaction` 中的公共进度状态缩小扫描范围。

本方案不新增 `MEMBLOCK_REDIRECT_REFETCH_EN`。redirect reissue 是固定框架行为。

## 2. Scala/RTL 语义约束

XiangShan MemBlock 和后端通过 ROB redirect 判断当前 uop 是否需要 flush，典型逻辑是：

```scala
robIdx.needFlush(redirect)
```

load/store pipeline 内部在 redirect 命中时会 kill 对应动态 uop。例如 store pipeline 中会按 redirect 生成 kill：

```scala
val kill = robIdx.needFlush(io.redirect)
val fire = pipeIn.fire && !kill
```

这说明：

- 被 redirect 命中的动态 uop 会被 RTL kill。
- 被 kill 的旧动态 uop 不应该继续等待正常 writeback/commit。
- 真实系统中同一条程序指令会在 redirect recovery 后重新取指、重新分配 ROB/LQ/SQ、重新执行。
- 当前 mem_ut 简化模型不模拟完整 refetch/rename 重新分配，但被 redirect flush 的 transaction 仍必须沿用主表 key 作为新动态实例重新发射。

示例：

```text
uid1: store A
uid2: load  A
```

如果 `uid2` 先执行并触发 store-load RAW 相关违例，RTL 会 redirect/rollback。测试框架如果只把 `uid2` 标成 flushed 并结束，就只能验证 DUT 发出了 redirect，无法验证 redirect 后 `uid2` 重新执行并得到正确结果。

## 3. 状态语义

### 3.1 `status.success`

`status.success=1` 表示 transaction 最终提交/退休成功，不是写回成功。

成功路径仍要求：

```text
rob_commit=1
LQ/SQ active mapping 已释放
无 fault/replay/redirect/flushed pending
active=0
```

只有 `success=1` 的 uid 才能被公共 success 前缀永久跳过。

### 3.2 `status.flushed`

在 redirect reissue 模式下，`flushed=1` 不是 transaction 终态。

语义调整为：

```text
flushed=1 表示当前动态实例被 redirect kill。
该 uid 后续必须进入 redirect reissue 流程。
redirect reissue 重新激活后，需要清回 flushed=0。
```

也就是说：

```text
flushed=1, success=0, redirect_pending=1
```

是中间态，不是完成态。

### 3.3 `redirect_pending`

复用 `status_transaction` 现有字段：

```systemverilog
// 当前 uid 的旧动态实例已被 redirect flush，等待沿用主表 ROB/LQ/SQ key 后重发。
bit redirect_pending;
```

本方案不新增 `refetch_pending`。`redirect_pending` 已经能表达“当前 uid 等待 redirect 后重发”。

### 3.4 `dynamic_epoch`

复用或新增 `status_transaction` 中的动态实例版本字段：

```systemverilog
// 同一个 uid 经 redirect reissue 后会产生新的动态实例，用该版本号过滤旧实例事件。
int unsigned dynamic_epoch;
```

含义：

```text
uid2 第一次发射：dynamic_epoch=0
uid2 被 redirect flush：旧实例失效
uid2 沿用主表 ROB/LQ/SQ key 再次发射：dynamic_epoch=1
uid2 再次被 redirect flush 后重发：dynamic_epoch=2
```

它和其它版本字段的区别：

- `dynamic_epoch`：区分同一 `uid` 的不同动态实例，redirect reissue 时递增。
- `issue_epoch`：区分同一动态实例内某个 target 的不同 issue fire 记录，用于 writeback/replay 匹配。
- `replay_seq`：区分后端 replay 后同一 target 的重新发射轮次。
- `dispatch_flush_epoch`：全局 flush 版本，用于 driver 等待 ready 时发现自己跨过 redirect/flush。

`dynamic_epoch` 不是 DUT 接口字段，而是测试框架内部防止旧实例事件污染新实例的版本号。

## 4. 公共进度状态

### 4.1 结构体定义

将 route/redirect/reissue 共享的扫描边界合并成一个公共状态结构体，不再把相关字段零散放在各个 class 内。

建议结构体定义为：

```systemverilog
typedef struct {
    // 从0开始连续success后的第一个uid。
    // 例如 uid0、uid1 已 success，uid2 未完成，则 success_prefix_uid = 2。
    memblock_uid_t success_prefix_uid;

    // 当前连续有效 LSQ admission 高水位。
    // redirect 命中后允许回退到最老 flushed uid 的前一个 uid。
    memblock_uid_t max_enqueued_uid;

    // 标记 max_enqueued_uid 是否有效。
    bit max_enqueued_uid_valid;
} memblock_dispatch_progress_t;
```

结构体类型可放在 `memblock_dispatch_types.sv`，实例放在 `common_data_transaction.sv`：

```systemverilog
memblock_dispatch_progress_t dispatch_progress;
```

核心要求：新增核心字段时需要添加中文注释，说明字段含义和在测试框架中的作用。

### 4.2 `success_prefix_uid` 语义

`success_prefix_uid` 表示从 0 开始连续 success 后的第一个 uid。

它不是“任意已经 success 的最大 uid”。

示例：

```text
uid0 success
uid1 success
uid2 未完成
uid3 success
```

此时：

```text
success_prefix_uid = 2
```

不能设置为 4。否则 uid2 会被错误跳过，redirect reissue 或 route 都可能漏处理。

### 4.3 `max_enqueued_uid` 语义

`max_enqueued_uid` 表示当前连续有效 LSQ admission 高水位，不是历史最大入队 uid。

正常顺序入队时：

```text
uid0 入队成功 -> max_enqueued_uid = 0
uid1 入队成功 -> max_enqueued_uid = 1
uid2 入队成功 -> max_enqueued_uid = 2
```

redirect 命中后，如果最老被 flush 的 uid 是 `uid2`，则当前有效入队高水位需要回退：

```text
oldest_flushed_uid = 2
max_enqueued_uid   = 1
```

之后 LSQ admission 永远从：

```text
max_enqueued_uid + 1
```

开始顺序入队。此时 `uid2` 会先被重新入队；`uid2` 重入队成功后，`max_enqueued_uid` 推进到 2，然后再处理 `uid3`。

因此不需要额外保存“redirect 前历史入队到哪里”。redirect 后测试框架只维护当前有效入队进度，后续 uid 按当前进度重新推进。

扫描窗口统一使用右开区间：

```text
[success_prefix_uid, max_enqueued_uid + 1)
```

如果 `max_enqueued_uid_valid=0`，说明还没有 uid 成功入队，此时扫描窗口为空。

## 5. 公共 helper 设计

### 5.1 reset 逻辑

不单独新增 `reset_runtime_progress()`。直接在 `common_data_transaction::reset_all_tables()` 中清理公共进度状态：

```systemverilog
dispatch_progress.success_prefix_uid     = 0;
dispatch_progress.max_enqueued_uid       = 0;
dispatch_progress.max_enqueued_uid_valid = 1'b0;
```

### 5.2 `mark_uid_enqueued(uid)`

功能：记录某个 uid 已经成功进入 LSQ admission，并推进当前连续有效入队高水位。

建议放在 `common_data_transaction.sv`：

```systemverilog
function void mark_uid_enqueued(input memblock_uid_t uid);
    check_uid(uid, "mark_uid_enqueued");
    if (!dispatch_progress.max_enqueued_uid_valid) begin
        if (uid != 0) begin
            `uvm_fatal("COMMON_DATA",
                       $sformatf("first LSQ admission must be uid0, got uid=%0d", uid))
        end
        dispatch_progress.max_enqueued_uid = uid;
        dispatch_progress.max_enqueued_uid_valid = 1'b1;
        return;
    end
    if (uid != dispatch_progress.max_enqueued_uid + 1) begin
        `uvm_fatal("COMMON_DATA",
                   $sformatf("LSQ admission must be sequential: uid=%0d expected=%0d",
                             uid, dispatch_progress.max_enqueued_uid + 1))
    end
    dispatch_progress.max_enqueued_uid = uid;
endfunction
```

调用方式：推荐在 `set_status_field()` 内统一处理 `MEMBLOCK_STATUS_ENQ=1`：

```systemverilog
MEMBLOCK_STATUS_ENQ: begin
    status.enq = value;
    if (value) begin
        mark_uid_enqueued(uid);
    end
end
```

这样未来任何路径只要设置 `enq=1`，都会同步更新公共进度状态。由于本方案要求 LSQ admission 严格按 uid 顺序推进，如果出现跳号入队，应直接 fatal。

### 5.3 `rollback_max_enqueued_uid(uid)`

功能：redirect 命中后，把当前连续有效入队高水位回退到最老被 flush uid 的前一个 uid。

建议放在 `common_data_transaction.sv`：

```systemverilog
function void rollback_max_enqueued_uid(input memblock_uid_t oldest_flushed_uid);
    check_uid(oldest_flushed_uid, "rollback_max_enqueued_uid");
    if (oldest_flushed_uid == 0) begin
        dispatch_progress.max_enqueued_uid       = 0;
        dispatch_progress.max_enqueued_uid_valid = 1'b0;
        return;
    end
    dispatch_progress.max_enqueued_uid       = oldest_flushed_uid - 1;
    dispatch_progress.max_enqueued_uid_valid = 1'b1;
endfunction
```

该函数只回退 admission 高水位，不负责清状态。具体 uid 的状态清理由 `prepare_uid_for_redirect_reissue()` 完成。

### 5.4 `advance_success_prefix()`

功能：推进连续 success 前缀。

建议放在 `common_data_transaction.sv`：

```systemverilog
function void advance_success_prefix();
    status_transaction status;

    while (dispatch_progress.success_prefix_uid < main_trans_num) begin
        status = get_status(dispatch_progress.success_prefix_uid);
        if (!status.success) begin
            break;
        end
        dispatch_progress.success_prefix_uid++;
    end
endfunction
```

调用位置：

- `try_retire_committed_uid()` 设置 `status.success=1` 后。
- route 扫描前。
- redirect flush 范围扫描前。

### 5.5 `get_active_scan_begin_uid()`

功能：返回当前需要扫描的起点。

```systemverilog
function memblock_uid_t get_active_scan_begin_uid();
    return dispatch_progress.success_prefix_uid;
endfunction
```

所有小于该值的 uid 已经连续 success，不再参与 route、redirect flush、reissue 扫描。

### 5.6 `get_active_scan_end_uid()`

功能：返回当前需要扫描的终点，使用右开区间。

```systemverilog
function memblock_uid_t get_active_scan_end_uid();
    if (!dispatch_progress.max_enqueued_uid_valid) begin
        return dispatch_progress.success_prefix_uid;
    end
    return dispatch_progress.max_enqueued_uid + 1;
endfunction
```

### 5.7 `prepare_uid_for_redirect_reissue(uid, redirect)`

功能：处理单个 uid 被 redirect 命中后的状态清理和重发准备。

建议替代原来把 flushed uid 直接 retire 的逻辑：

```systemverilog
function void prepare_uid_for_redirect_reissue(input memblock_uid_t uid,
                                               input memblock_redirect_payload_t redirect);
    status_transaction status;

    status = get_status(uid);
    if (status.success) begin
        `uvm_fatal("COMMON_DATA",
                   $sformatf("redirect tries to flush already success uid=%0d", uid))
    end

    remove_uid_from_issue_queues(uid);
    remove_uid_from_active_maps(uid);

    clear_uid_dispatch_result(uid);
    status.flushed          = 1'b1;
    status.redirect_pending = 1'b1;
    status.dynamic_epoch++;
    status.active           = 1'b0;
    status.success          = 1'b0;
endfunction
```

该函数承担的职责：

- 删除 load/STA/STD issue queue 中该 uid 的残留项。
- 删除 active ROB/LQ/SQ map，避免旧动态实例继续被 monitor event 命中。
- 清理 queued/dispatched/writeback/pass/fault 等本轮动态实例结果。
- 标记 `redirect_pending=1`，说明该 uid 后续需要沿用主表 key 重新激活并发射。
- 递增 `dynamic_epoch`，让旧实例事件失效。
- 保留主表 transaction，不删除主表内容。

注意：当前 `clear_uid_dispatch_result()` 如果会清掉 `redirect_pending`，则必须在它之后重新置回：

```systemverilog
status.redirect_pending = 1'b1;
```

### 5.8 `apply_redirect_flush_range(redirect)`

功能：封装 redirect flush 范围扫描，替代原来 `0..main_trans_num` 全表循环。

建议让现有 `apply_redirect_flush()` 内部调用该 helper，或者直接改名合并。

```systemverilog
function void apply_redirect_flush_range(input memblock_redirect_payload_t redirect);
    status_transaction status;
    memblock_uid_t begin_uid;
    memblock_uid_t end_uid;
    memblock_uid_t oldest_flushed_uid;
    bit found_flushed;

    if (!redirect.valid) begin
        `uvm_fatal("COMMON_DATA", "apply_redirect_flush_range requires valid redirect")
    end

    advance_success_prefix();
    begin_uid = get_active_scan_begin_uid();
    end_uid   = get_active_scan_end_uid();
    found_flushed = 1'b0;

    for (memblock_uid_t uid = begin_uid; uid < end_uid; uid++) begin
        status = get_status(uid);
        if (!status.active) begin
            continue;
        end
        if (rob_order_util::rob_need_flush(status.get_rob_key(), redirect)) begin
            if (!found_flushed || uid < oldest_flushed_uid) begin
                oldest_flushed_uid = uid;
                found_flushed = 1'b1;
            end
            prepare_uid_for_redirect_reissue(uid, redirect);
        end
    end
    if (found_flushed) begin
        rollback_max_enqueued_uid(oldest_flushed_uid);
    end
endfunction
```

这里的 `[begin_uid, end_uid)` 只用于减少候选 uid 数量。真正判断是否 flush 仍必须使用：

```systemverilog
rob_order_util::rob_need_flush(status.get_rob_key(), redirect)
```

不能直接用 `uid > redirect_uid` 或 `uid >= redirect_uid` 判断，因为 redirect 语义来自 ROB 顺序，不来自主表 uid 顺序。

如果本次 redirect 命中了至少一个 uid，`apply_redirect_flush_range()` 需要记录最老被 flush 的 uid，并在清理完成后调用 `rollback_max_enqueued_uid(oldest_flushed_uid)`。这样后续 LSQ admission 会从该 uid 开始重新顺序入队。

## 6. route 扫描优化

### 6.1 删除 scheduler 本地 success 前缀

当前 `issue_queue_scheduler.sv` 中的本地字段：

```systemverilog
memblock_uid_t route_success_prefix_uid;
```

需要删除。route 进度统一使用：

```systemverilog
data.dispatch_progress.success_prefix_uid
```

或者通过 getter 获取，不建议 scheduler 自己维护一份进度。

### 6.2 `route_all_ready_uids()` 修改

`route_all_ready_uids()` 改成从公共扫描窗口开始，每拍最多扫描 `MEMBLOCK_REAL_LSQ_ENQ_MAX` 个 uid：

```systemverilog
function void route_all_ready_uids();
    int unsigned scanned;
    int unsigned scan_limit;
    memblock_uid_t uid;
    memblock_uid_t begin_uid;
    memblock_uid_t end_uid;

    ensure_data();
    if (data.issue_blocked_by_global_flush()) begin
        return;
    end

    data.advance_success_prefix();
    begin_uid  = data.get_active_scan_begin_uid();
    end_uid    = data.get_active_scan_end_uid();
    scan_limit = seq_csr_common::get_real_lsq_enq_max();

    scanned = 0;
    for (uid = begin_uid;
         uid < end_uid &&
         scanned < scan_limit;
         uid++) begin
        route_uid(uid);
        scanned++;
    end
endfunction
```

这样重新发射查主表状态时，也只查：

```text
success_prefix_uid .. max_enqueued_uid
```

不再从 0 扫到 `main_trans_num`。

### 6.3 `MEMBLOCK_REAL_LSQ_ENQ_MAX`

每拍 route 工作量直接用 `seq_csr_common.sv` 中的 `real_lsq_enq_max` 控制。

参数：

```text
MEMBLOCK_REAL_LSQ_ENQ_MAX=8
```

含义：

```text
当前 DUT 每拍 LSQ enqueue 总 slot 上限。
route_all_ready_uids() 每拍从公共 success 前缀后最多扫描该数量的 uid。
```

Scala 侧真实约束：

```text
LsqEnqIO.req/resp/needAlloc 宽度 = LSQEnqWidth
LSQEnqWidth = RenameWidth
LSQLdEnqWidth = min(LSQEnqWidth, backendParams.numLoadDp)
LSQStEnqWidth = min(LSQEnqWidth, backendParams.numStoreDp)
```

按默认参数可取 `8`。如果后续切到 6-wide DUT，只需要把该参数改成 6。

注意：`real_lsq_enq_max` 只控制 route 每拍扫描窗口，不表示本拍一定能成功发射 8 个 transaction。真实发射数量仍受 load/STA/STD pipe 数、DUT ready/fire、队列状态限制。

## 7. LSQ admission 更新点

当 uid 成功进入 LSQ admission 后，需要更新公共最大入队 uid。

当前 `lsq_ctrl_model.sv` 中有类似逻辑：

```systemverilog
data.activate_uid(uid, behavior.uses_lq, behavior.uses_sq);
data.set_status_field(uid, MEMBLOCK_STATUS_ENQ, 1'b1);
```

建议通过 `set_status_field(MEMBLOCK_STATUS_ENQ, 1'b1)` 自动调用：

```systemverilog
mark_uid_enqueued(uid);
```

这样不要求每个 LSQ admission 调用点都手动更新公共状态。

### 7.1 去掉本地 `next_admit_uid`

LSQ admission 必须按 uid 顺序入队。后续不再由 `memblock_lsqenq_dispatch_sequence` 维护本地 `next_admit_uid` 游标，新 uid 的入队起点统一由公共状态推导：

```systemverilog
function memblock_uid_t get_next_new_admit_uid();
    if (!dispatch_progress.max_enqueued_uid_valid) begin
        return 0;
    end
    return dispatch_progress.max_enqueued_uid + 1;
endfunction
```

这里的含义是：

```text
max_enqueued_uid 是当前连续有效 LSQ admission 高水位。
max_enqueued_uid + 1 是下一条需要 admission 的 uid。
```

redirect 后 `max_enqueued_uid` 允许回退到最老 flushed uid 的前一个 uid。这样 admission task 不需要区分“老 uid 重入队”和“新 uid 入队”，永远只处理 `max_enqueued_uid + 1`。

### 7.2 redirect 后顺序重入队

redirect flush 后，`apply_redirect_flush_range()` 会把 `max_enqueued_uid` 回退到最老 flushed uid 的前一个 uid。因此 LSQ admission task 不需要扫描一段 pending uid，也不需要额外恢复边界。

推荐 admission 选择顺序固定为：

```text
1. uid = get_next_new_admit_uid()
2. 检查 uid < main_trans_num
3. 检查该 uid 当前可以 admission
4. admission 成功后调用 set_status_field(uid, MEMBLOCK_STATUS_ENQ, 1'b1)
5. mark_uid_enqueued(uid) 将 max_enqueued_uid 推进到 uid
```

伪代码：

```systemverilog
function bit find_next_admit_uid(output memblock_uid_t uid);
    uid = get_next_new_admit_uid();
    return uid < main_trans_num;
endfunction
```

这样可以保证：

- 入队整体严格按 uid 顺序推进。
- redirect 后老 uid 通过 `max_enqueued_uid` 回退重新成为下一个 admission uid。
- 不需要本地 `next_admit_uid`。
- 不需要额外保存 redirect 前的历史入队边界。

### 7.3 重入队成功后的状态更新

当 redirect pending uid 重新 LSQ admission 成功后，需要清掉旧的 recovery 标志，并让它重新进入后续 route/issue 流程：

```systemverilog
data.activate_uid(uid, behavior.uses_lq, behavior.uses_sq);
data.set_status_field(uid, MEMBLOCK_STATUS_ENQ, 1'b1);
status.flushed          = 1'b0;
status.redirect_pending = 1'b0;
status.issue_ready      = 1'b1; // 或等待 TLB/补充字段完成后再置 1
```

重入队 uid 必须等于 `max_enqueued_uid + 1`。重入队成功后，`mark_uid_enqueued(uid)` 会把当前连续有效入队高水位推进到该 uid。后续下一拍继续从新的 `max_enqueued_uid + 1` 入队。

## 8. success 更新点

commit 成功闭环一般通过：

```systemverilog
try_retire_committed_uid(uid)
```

建议在该函数设置 `status.success=1'b1` 后调用：

```systemverilog
advance_success_prefix();
```

这样公共 success 前缀始终由最终成功状态驱动。

## 9. redirect 后重新发射边界

redirect 后不应取消被 flush 的 transaction，而是准备重新发射。

推荐语义：

- 被 redirect 命中的 uid：清理本轮动态执行结果，保留主表。
- `status.redirect_pending=1` 表示该 uid 需要按 redirect reissue 路径重新进入流程。
- 小于 `success_prefix_uid` 的 uid 已经连续 success，不再重发。
- 大于等于 `success_prefix_uid` 且小于 `max_enqueued_uid + 1` 的 uid，是 redirect/reissue/route 需要关注的活跃窗口。

如果 uid2 被 redirect flush，即使 uid3 曾经已经 writeback/pass，只要 uid3 还没有 success 且被 ROB redirect 命中，也需要清理 uid3 的本轮结果并重新发射。否则 store/load 地址相关场景下可能漏掉访存违例重检。

### 9.1 redirect reissue 到流水线的完整链路

redirect 后重新进入流水线的完整链路如下：

```text
1. raw monitor 采到 memoryViolation/redirect
2. service flow 统一收集本拍 raw event，并选择 oldest redirect
3. request_redirect_flush() 进入 freeze/recovery 状态，阻止新 route/commit/success
4. push_redirect_drive() 将 redirect payload 驱动给 DUT
5. redirect drive done 后调用 apply_redirect_flush_range()
6. apply_redirect_flush_range() 在 [success_prefix_uid, max_enqueued_uid + 1) 中查找被 rob_need_flush() 命中的 uid
7. 对命中 uid 调用 prepare_uid_for_redirect_reissue()
   - 清 load/STA/STD issue queue 残留
   - 清 active ROB/LQ/SQ map
   - 清 queued/dispatched/writeback/pass/fault
   - status.redirect_pending=1
   - status.flushed=1
   - status.active=0
   - status.enq=0
   - status.issue_ready=0
   - status.success=0
   - dynamic_epoch++
8. apply_redirect_flush_range() 将 max_enqueued_uid 回退到最老 flushed uid 的前一个 uid
9. LSQ admission task 使用 max_enqueued_uid + 1 作为下一条 admission uid
10. 如果该 uid 是 redirect_pending，沿用主表 robIdx/lqIdx/sqIdx key 重新 activate/enq
11. 重入队成功后清 redirect_pending/flushed，并重新置 issue_ready
12. route_all_ready_uids() 从公共 success 前缀开始扫描
13. 该 uid 满足 active/enq/issue_ready 后重新进入 load/STA/STD issue queue
14. issue queue scheduler 再按原有 load/STA/STD 发射规则送入流水线
```

其中 `max_enqueued_uid` 回退后，redirect pending uid 会自然成为 `max_enqueued_uid + 1`。因此不需要优先扫描 pending 队列，也不需要保存 redirect 前历史入队边界。

## 10. all_transactions_success 优化

redirect reissue 模式下，`flushed` 不是完成态。因此结束条件不能再接受 flushed uid。

结束条件应改为：

```text
issue queue empty
feedback/raw monitor queue empty
active map empty
redirect/flush/replay 全局状态为空
success_prefix_uid >= main_trans_num
```

本轮采用公共 success 前缀做 O(1) 判断，不新增多组计数器。原因是 `success_prefix_uid` 只能跨过从 uid0 开始连续 `success=1` 的 uid；只要中间存在 `flushed/redirect_pending/replay_pending/exception_pending/active/未 success` 的 uid，前缀就不会推进到 `main_trans_num`。

实现方式：

```systemverilog
data.advance_success_prefix();
return data.dispatch_progress.success_prefix_uid >= data.main_trans_num;
```

`all_transactions_success()` 默认不再全表扫描 10 万 uid。原全表扫描只保留给：

- debug cross-check
- timeout report
- fatal 前定位未完成 uid

## 11. redirect 优先与 stale event 过滤

### 11.1 基本原则

raw monitor 只负责采集 DUT 接口事实，不直接决定 transaction 是否成功。状态机处理 raw event 时必须遵循：

```text
redirect/memoryViolation 优先
writeback/pass 需要按 redirect 覆盖关系过滤
commit/success 前必须二次检查 redirect 状态
```

原因是 MemBlock 内部的 memory violation redirect 可能和 load writeback 同拍产生。源码中 `NewLoadUnit` S3 同一级会生成 rollback 和 writeback：

```scala
val rollbackValid = pipeIn.valid && (rarViolation || matchInvalid) && endPipe
val ldoutValid = pipeIn.valid && shouldWriteback && !isVector && endPipe
```

因此 TB raw monitor 同拍采到 `memoryViolation` 和 `writeback` 是合理现象。状态处理层不能简单按采集顺序把 writeback 当作最终有效结果。

### 11.2 同拍 memoryViolation + writeback 处理

当本拍同时采到 `memoryViolation` 和 writeback 时，不应无脑屏蔽所有 writeback，而是按 ROB flush 规则判断该 writeback 是否被当前 redirect 覆盖。

推荐处理规则：

```systemverilog
if (memory_violation.valid &&
    rob_order_util::rob_need_flush(wb_event.rob_key, memory_violation.redirect)) begin
    // 该 writeback 属于被 redirect flush 覆盖的旧动态实例，不更新 writeback/pass。
end else begin
    // 该 writeback 不被当前 redirect 覆盖，可以按正常 writeback 处理。
end
```

不能只用 `memoryViolation.valid` 屏蔽所有 writeback。原因是 redirect level 可能是 `flushAfter`，这种场景只 flush younger uop，不一定 flush 产生 redirect 的 uop 自身。是否被 flush 必须由 `rob_need_flush()` 统一判断。

### 11.3 先 pass 后 RAW redirect 的处理

另一类场景是：某个 load 已经正常 writeback/pass，但还没有按 ROB 顺序 commit 成功；后续 older store 上流水后发现 RAW 违例，MemBlock/LSQ 产生 memoryViolation redirect，要求这个 load 重新执行。

因此 `status.pass=1` 不能等同于 transaction 完成。只要 `status.success=0`，该 uid 被 older redirect 命中时，必须清理旧结果并进入 redirect reissue：

```text
status.writeback/pass 已经置高也要清掉
queued/dispatched 已经置高也要清掉
redirect_pending 置 1
flushed 置 1
success 保持 0
后续从公共 success 前缀附近重新检查并发射
```

### 11.4 推荐状态处理顺序

每个 service 周期建议固定为“统一收集、redirect 优先”。也就是先把本拍 raw event 全部收集起来，再决定本拍走 redirect 流程还是 normal writeback 流程。

```text
1. collect 所有 raw event
2. 先检查本批 event 是否存在 memoryViolation/redirect
3. 如果存在 redirect：
   3.1 选择 oldest redirect
   3.2 request_redirect_flush()，进入 freeze/recovery 状态
   3.3 push_redirect_drive()，驱动 redirect 给 DUT
   3.4 本批被 rob_need_flush() 覆盖的 writeback/pass 直接丢弃
   3.5 本批未被该 redirect 覆盖的 writeback/pass 可以 defer/requeue，等待 redirect recovery 后再处理
   3.6 本拍不调用 normal pass 状态更新
4. 如果本批没有 redirect，才处理 normal writeback/pass/replay/fault
5. redirect drive done 后调用 apply_redirect_flush_range()
6. 处理 commit/deq；commit 前再次检查该 uid 没有 redirect_pending/flushed/exception/replay
```

当前简化实现中已删除 `writeback_status_task()` 二次整理路径。monitor adapter 采到 writeback/feedback/memoryViolation 后直接调用 `writeback_status_handler::handle_event()` 分类，`exception_event_q` 只由 `exception_redirect_replay_handler::process_pending_events()` 消费；这样不会出现 writeback handler pop pending queue 后再 requeue replay/redirect/fault 的职责混用。

仍建议在 `handle_event()` 或 `mark_target_normal_pass()` 前保留一道轻量防御检查，防止后续其他路径绕过统一事件仲裁：

```systemverilog
if (data.active_redirect.valid &&
    wb_event.has_rob &&
    rob_order_util::rob_need_flush(wb_event.rob_key, data.active_redirect)) begin
    // 该 writeback 被当前 active redirect 覆盖，作为 stale event 丢弃。
    return 1'b0;
end
```

这道检查不是主流程依赖，而是防御兜底。主流程仍然是：有 redirect 时只走 redirect 更新流程，没有 redirect 时才处理 normal writeback。

commit/success 前的二次防线：

```systemverilog
if (status.redirect_pending || status.flushed ||
    (current_redirect_valid &&
     rob_order_util::rob_need_flush(status.get_rob_key(), current_redirect))) begin
    // 禁止 success；必要时进入 redirect reissue。
end else if (status.pass && status.rob_commit && status.lsq_deq) begin
    status.success = 1'b1;
end
```

### 11.5 当前 mem_ut 重发模型

当前 mem_ut 不模拟完整前端 refetch/rename/ROB/LSQ 重新分配。redirect reissue 后，同一个 `uid` 重新发射时应沿用主表中的 `robIdx/lqIdx/sqIdx` key，不应随机换成新的 key。

因此 `dynamic_epoch` 的作用不是配合新的 ROB/LQ/SQ key，而是区分同一个 `uid`、同一组 key 的不同动态执行轮次：

```text
uid2 第一次发射：dynamic_epoch=0
uid2 被 redirect flush：旧实例失效
uid2 重新发射：robIdx/lqIdx/sqIdx 沿用主表，dynamic_epoch=1
```

如果 monitor event 能携带或间接关联 `dynamic_epoch/issue_epoch/replay_seq`，状态处理层需要检查这些版本信息。若版本不匹配，丢弃并打印 `UVM_INFO/UVM_WARNING`。

## 12. 10万笔请求下性能风险点 review

### 12.1 route 全表扫描

风险：

```text
每拍 O(main_trans_num)
10万笔请求下非常重
```

必须修改：

- 删除 scheduler 本地 `route_success_prefix_uid`。
- 使用 `common_data_transaction.dispatch_progress.success_prefix_uid` 跳过已 success 前缀。
- 使用 `real_lsq_enq_max` 限制每拍扫描 uid 数，默认 8。
- redirect 后未 success 的 uid 会阻止 success 前缀推进，后续仍从该位置附近重新扫描。

### 12.2 redirect flush 全表扫描

风险：

```text
redirect 每次 0..main_trans_num 全表扫描
```

必须修改：

- 使用 `apply_redirect_flush_range()` 封装扫描。
- 扫描范围改为 `[success_prefix_uid, max_enqueued_uid + 1)`。
- 仍通过 `rob_order_util::rob_need_flush()` 判断是否真正 flush。

### 12.3 issue queue 规模

正常情况下 issue queue 不会膨胀到 10 万规模。原因是 route 的前置条件要求 `status.enq=1`，而 `enq` 只会在 LSQ admission 成功后置高。LSQ admission 又受 LSQ 容量、DUT canAccept/free count、每拍入队宽度共同限制。

因此主路径不强制新增 queue high watermark。第一版依赖 LSQ 容量和 `MEMBLOCK_REAL_LSQ_ENQ_MAX` 控制 route 规模即可。

如果后续调试中发现某些 testcase 或异常路径让 queue 积压过大，可以作为防御参数再新增：

```text
MEMBLOCK_ROUTE_QUEUE_HIGH_WATERMARK
```

该参数只作为可选保护：当 load/STA/STD queue 总量超过阈值时，临时暂停继续 route，避免 select/delete 成本升高。

### 12.4 `delete_issue_queue_entry()` 动态数组删除

当前删除会遍历 queue 并 `delete(idx)`。queue 大时会很重。

第一版先依赖 LSQ 容量和 route 每拍扫描窗口控制 queue 规模。如果仍然不足，再考虑：

- lazy invalidation
- 定期 compact
- per-target uid membership 表

### 12.5 `global_send_pri_en` 全局最高优先级扫描

`send_pri` 范围固定 0~100。大队列下后续可改成 priority bucket：

```text
load_issue_q_by_pri[0:100]
sta_issue_q_by_pri[0:100]
std_issue_q_by_pri[0:100]
```

第一版先依赖 LSQ 容量和 route 扫描窗口，不强制改 bucket。

### 12.6 timeout/report 全表打印

新增：

```text
MEMBLOCK_REPORT_UNFINISHED_LIMIT
```

默认只打印前 128 个未完成 uid，避免 10 万笔 timeout 时日志爆炸。

## 13. 推荐落地顺序

### 阶段 1：公共进度状态

必须做：

1. 定义 `memblock_dispatch_progress_t`。
2. 在 `common_data_transaction` 中新增 `dispatch_progress`。
3. 在 `reset_all_tables()` 中清理该结构体。
4. 新增 `mark_uid_enqueued()`、`rollback_max_enqueued_uid()`、`advance_success_prefix()`、`get_active_scan_begin_uid()`、`get_active_scan_end_uid()`、`get_next_new_admit_uid()`。
5. `set_status_field(MEMBLOCK_STATUS_ENQ, 1'b1)` 自动调用 `mark_uid_enqueued()`。
6. `try_retire_committed_uid()` 设置 success 后调用 `advance_success_prefix()`。

### 阶段 2：redirect reissue 语义修正

必须做：

1. redirect reissue 作为固定框架行为，不新增 plus/seq 参数控制开关。
2. 复用 `redirect_pending`，使用 `dynamic_epoch` 区分动态实例。
3. 新增或改造 `prepare_uid_for_redirect_reissue()`。
4. 修改 `apply_redirect_flush()`，内部使用 `apply_redirect_flush_range()`。
5. flush 命中的未 success uid 不终止，而是进入 redirect pending。
6. 已 writeback/pass 但未 success 的 flushed uid 必须清状态并重发。
7. success uid 如果被 redirect 命中，直接 fatal。

### 阶段 3：route 性能优化

必须做：

1. 删除 `issue_queue_scheduler` 本地 `route_success_prefix_uid`。
2. 删除 LSQ admission sequence 本地 `next_admit_uid`，下一条入队 uid 永远由 `max_enqueued_uid + 1` 推导。
3. redirect flush 后调用 `rollback_max_enqueued_uid(oldest_flushed_uid)`，让老 uid 自然成为下一条 admission uid。
4. `route_all_ready_uids()` 使用公共扫描窗口。
5. 每拍最多扫描 `seq_csr_common::get_real_lsq_enq_max()` 个 uid，默认 8。
6. 不把 queue high watermark 作为必须项；如后续发现异常路径导致 queue 积压，再作为防御参数添加。

### 阶段 4：O(1) 结束判断

已采用轻量 success-prefix 方案，不额外维护多组计数器：

1. `set_status_field(MEMBLOCK_STATUS_SUCCESS,1)` 会调用 `advance_success_prefix()`。
2. `all_transactions_success()` 先检查 issue queue、feedback/raw monitor queue、active map、redirect/flush/replay 全局状态为空。
3. 然后只调用 `advance_success_prefix()` 并判断 `success_prefix_uid >= main_trans_num`。
4. 全表扫描只保留在 `report_unfinished_status()` 等 debug/timeout 定位路径中。

### 阶段 5：进一步优化

按性能瓶颈选择：

- active uid list 加速 redirect flush
- ready worklist
- send_pri bucket queue
- lazy queue deletion

## 14. 关键约束

1. `success=1` 才是 transaction 终态。
2. `flushed=1` 在 redirect reissue 模式下不是终态，而是旧动态实例被杀。
3. redirect 命中的未 success uid 必须 redirect reissue。
4. 已 writeback/pass 但未 success 的 uid，如果被 older redirect 命中，必须清掉并重发。
5. 当前 mem_ut 简化模型下，被 redirect flush 后重发的 uid 必须沿用主表 ROB/LQ/SQ key，但不能复用旧 active map 状态。
6. 旧动态实例事件必须用 active map、issue_epoch、dynamic_epoch 过滤。
7. success 前缀只能跨过连续 success uid，不能跨过 flushed/redirect_pending uid。
8. route、redirect flush、reissue 查表都不能每拍从 0 扫描 10 万 uid。
9. redirect flush 扫描范围可以用 uid 窗口剪枝，但是否 flush 必须由 `rob_need_flush()` 判断。
10. route 必须依赖 LSQ admission 状态和 `MEMBLOCK_REAL_LSQ_ENQ_MAX` 控制规模，不能绕过 LSQ 容量限制直接把未来 uid 放入 issue queue。
11. LSQ admission 不再维护本地 `next_admit_uid`；下一条 admission uid 永远使用 `max_enqueued_uid + 1`。
12. `max_enqueued_uid` 是当前连续有效入队高水位，redirect 后允许回退到最老 flushed uid 的前一个 uid。

## 15. 最终结论

最终设计为：

```text
redirect reissue 是固定框架行为，不通过参数关闭
redirect flush 命中的 uid 不终止
未 success 的 flushed uid 全部进入 redirect_pending
从 common_data_transaction.dispatch_progress.success_prefix_uid 开始重新检查
只扫描到 max_enqueued_uid
直到所有 uid 都 success，test 才算完成
```

性能上采用：

```text
common_data_transaction.dispatch_progress
success_prefix_uid
max_enqueued_uid/max_enqueued_uid_valid
real_lsq_enq_max 默认8，控制每拍 route 扫描窗口
all_transactions_success 使用 success_prefix_uid 做 O(1) 结束判断
```

这样既能支撑 10 万笔请求，也能覆盖 store/load 同地址等访存违例场景下 redirect 后重新执行并得到正确结果的路径。

## 16. 代码落地同步记录

本轮已按该方案完成核心 SV 实现，并同步 `dispatch_framework_sv` 源码说明文档。

已落地要点：

1. `memblock_dispatch_types.sv` 新增 `memblock_dispatch_progress_t`，`common_data_transaction.sv` 新增 `dispatch_progress`。
2. `common_data_transaction.sv` 新增公共 helper：`mark_uid_enqueued()`、`rollback_max_enqueued_uid()`、`advance_success_prefix()`、`get_active_scan_begin_uid()`、`get_active_scan_end_uid()`、`get_next_new_admit_uid()`。
3. `set_status_field(MEMBLOCK_STATUS_ENQ,1)` 统一推进 `max_enqueued_uid`；`set_status_field(MEMBLOCK_STATUS_SUCCESS,1)` 统一推进 `success_prefix_uid`。
4. `memblock_lsqenq_dispatch_sequence.sv` 删除本地 `next_admit_uid`，LSQ admission 永远从公共 `max_enqueued_uid + 1` 推导。redirect pending/flushed uid 在公共高水位回退后可重新 admission。
5. `issue_queue_scheduler.sv` 删除本地 `route_success_prefix_uid`，`route_all_ready_uids()` 只扫描公共 `[success_prefix_uid, max_enqueued_uid + 1)` 窗口，并用 `MEMBLOCK_REAL_LSQ_ENQ_MAX` 限制每拍扫描数量。
6. `common_data_transaction.sv` 新增 `prepare_uid_for_redirect_reissue()` 和 `apply_redirect_flush_range()`。redirect flush 不再全表扫描，不再把 `flushed` 当作终态，而是清旧动态实例状态、置 `redirect_pending/flushed`、递增 `dynamic_epoch` 并回退 admission 高水位。
7. redirect reissue 清理会累加 `pending_lq_cancel_count/pending_sq_cancel_count`，LSQ admission sequence 恢复后调用 `lsq_ctrl.cancel_lq/cancel_sq` 回退软件 LSQ admission 镜像，保证同 uid 重入队时本地预测 key 与 DUT response key 一致。
8. `lsq_commit_handler.sv` 的 commit cursor 只跳过 `success=1`，不再跳过 `flushed`。
9. `memblock_dispatch_real_smoke_sequence.sv` 的 `service_monitor_once()` 负责 collect CSR、writeback/IQ feedback、exception/redirect raw event；normal pass/fault 在 collect 阶段经 `writeback_status_handler::handle_event()` 直接落状态，`exception_event_q` 只由 `exception_redirect_replay_task()` 消费；`all_transactions_success()` 不再接受 flushed 终态，并改为 success-prefix O(1) 判断。
10. `writeback_status_handler.sv` 保留 active redirect 防御过滤，并删除旧的 queue 二次整理入口；replay/redirect/fault 入队后不再由 writeback handler pop/requeue。

未在本轮强制落地的优化项：

- 未采用 `success_uid_count/active_uid_count/...` 多计数器方案；当前使用公共 `success_prefix_uid` 实现 `all_transactions_success()` O(1) 结束判断。全表扫描仅用于 timeout/debug 打印。
- issue queue lazy invalidation、priority bucket 和 timeout report limit 仍作为后续优化项。
