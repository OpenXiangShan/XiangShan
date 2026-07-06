# terminal_done 完成前缀重构 implementation review

状态：undo

创建日期：2026-06-28

关联 plan：`AI_DOC/plan/test_framework/plan/do/terminal_done_prefix_refactor_plan_20260627.md`

## 1. Review 结论

本轮实现把全局完成语义从 `success` 拆成 `success + terminal_done`：

- `success` 只表示 normal pass 结果。
- `terminal_done` 表示 uid 生命周期已经进入最终态，可以推进完成前缀。
- `success=0 && terminal_done=1` 是合法 fault/exception 终态。
- `global_stop_requested`、active scan begin、commit cursor 均改为基于 `terminal_done_uid`。

review 结论：实现与 plan 主体一致；未发现 blocking 问题。coding 中补充了一个 replay soft testcase 事件构造修复，用于让 soft replay smoke 真实进入 IQ feedback failed replay 路径，见第 12 章。

## 2. 修改前逻辑

修改前 `dispatch_progress.success_prefix_uid` 只会跨过 `status.success=1` 的 uid。fault/exception uid 即使已经完成 ROB commit 和 LSQ deq，也因为 `success=0` 无法推进 completion 前缀，主动 sequence 可能一直等不到 global stop。

旧行为可以概括为：

```text
normal pass：
  success=1
  success_prefix 可以推进

fault/exception：
  success=0
  success_prefix 不能推进
  fault uid 会卡住全局完成条件
```

## 3. 修改后逻辑

修改后将“正常成功结果”和“生命周期终态”拆开：

```text
normal pass commit/deq retire：
  success=1
  terminal_done=1

fault/exception commit/deq retire：
  success=0
  terminal_done=1

redirect/replay/flushed 中间态：
  success=0
  terminal_done=0
```

`terminal_done_uid` 从 uid0 开始只跨过连续 `terminal_done=1` 的 uid。这样 normal pass 和允许退休的 fault/exception 都能让测试闭环，但 checker 和日志仍能通过 `success` 区分是否正常通过。

## 4. 字段和枚举

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/status_transaction.sv`，字段定义和 reset。

函数功能简析：`status_transaction` 保存每个 uid 的运行期状态；本轮新增 `terminal_done` 表示该 uid 生命周期是否已经进入最终态，并在 reset 时清零。

```systemverilog
bit success;
bit terminal_done;

...

success           = 1'b0;
terminal_done     = 1'b0;
```

中文伪代码：

```text
在每个 uid 的 status 中保留两个结果字段：
  success 表示 normal pass 是否成功；
  terminal_done 表示 uid 生命周期是否已经结束。
reset 时同时清 success 和 terminal_done：
  新 uid 或重新初始化后，不能继承旧动态实例的成功或终态状态。
```

关键副作用：`terminal_done` 是后续 completion 前缀、commit cursor、end check 的共同依据。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_types.sv`，dispatch progress 和 status field 枚举。

函数功能简析：公共类型定义中将完成前缀命名为 `terminal_done_uid`，并给 `set_status_field/get_status_field` 增加统一字段入口。

```systemverilog
typedef struct {
    // 从0开始连续terminal_done后的第一个uid；route/redirect/reissue都从这里开始扫描。
    memblock_uid_t terminal_done_uid;
    memblock_uid_t max_enqueued_uid;
    bit            max_enqueued_uid_valid;
} memblock_dispatch_progress_t;

...

MEMBLOCK_STATUS_SUCCESS            = 19,
MEMBLOCK_STATUS_TERMINAL_DONE      = 20,
MEMBLOCK_STATUS_LOAD_WRITEBACK     = 21,
```

中文伪代码：

```text
dispatch progress 中的完成边界改名为 terminal_done_uid：
  它指向从 uid0 开始连续终态完成后的第一个未终态 uid；
  route、redirect、reissue 扫描窗口都从这个边界开始。
新增 MEMBLOCK_STATUS_TERMINAL_DONE：
  外部逻辑通过统一 status field API 读写 terminal_done；
  不再通过 success 间接表达完成。
```

关键副作用：旧 `success_prefix_uid` 不保留兼容入口，避免同一语义双字段维护。

## 5. 完成前缀和全局退出

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，函数 `advance_terminal_done_uid()`、`transaction_done()`、`request_global_stop_if_done()`。

函数功能简析：这些函数是全局完成判断的公共 owner。顶层 sequence 只调用 `request_global_stop_if_done()`，子 sequence 只读 global stop。

```systemverilog
function void advance_terminal_done_uid();
    status_transaction status;

    while (dispatch_progress.terminal_done_uid < main_trans_num) begin
        status = get_status(dispatch_progress.terminal_done_uid);
        if (!status.terminal_done) begin
            break;
        end
        dispatch_progress.terminal_done_uid++;
    end
endfunction:advance_terminal_done_uid

function bit transaction_done();
    advance_terminal_done_uid();
    return dispatch_progress.terminal_done_uid >= main_trans_num;
endfunction:transaction_done

function void request_global_stop_if_done();
    if (transaction_done()) begin
        global_stop_requested = 1'b1;
    end
endfunction:request_global_stop_if_done
```

中文伪代码：

```text
advance_terminal_done_uid 负责推进公共完成前缀：
  从当前 terminal_done_uid 指向的 uid 开始读取 status；
  如果该 uid 还没有 terminal_done，停止推进；
  如果该 uid 已 terminal_done，将 terminal_done_uid 加 1；
  循环直到遇到第一个未终态 uid 或超过 main_trans_num。
transaction_done 调用 advance_terminal_done_uid：
  先刷新公共完成前缀；
  如果 terminal_done_uid 已经达到 main_trans_num，说明主表所有 uid 都进入终态。
request_global_stop_if_done 调用 transaction_done：
  所有 uid 终态后置 global_stop_requested=1；
  子 sequence 后续只读该标志退出，不各自维护 completion 条件。
```

关键副作用：fault uid 只要在 commit/deq 后置 `terminal_done=1`，全局完成前缀就能越过它。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，函数 `set_status_field()` 和 `get_status_field()`。

函数功能简析：统一状态字段读写入口。`SUCCESS` 不再推进完成前缀，`TERMINAL_DONE` 才推进完成前缀。

```systemverilog
MEMBLOCK_STATUS_SUCCESS: begin
    status.success = value;
end
MEMBLOCK_STATUS_TERMINAL_DONE: begin
    status.terminal_done = value;
    if (value) begin
        advance_terminal_done_uid();
    end
end

...

MEMBLOCK_STATUS_SUCCESS:           return status.success;
MEMBLOCK_STATUS_TERMINAL_DONE:     return status.terminal_done;
```

中文伪代码：

```text
当调用方设置 SUCCESS：
  只更新 status.success；
  不推进 terminal_done_uid，因为 success 只表示 normal pass 结果。
当调用方设置 TERMINAL_DONE：
  更新 status.terminal_done；
  如果写入 1，立刻调用 advance_terminal_done_uid 推进完成前缀。
当调用方读取状态字段：
  SUCCESS 返回 normal pass 结果；
  TERMINAL_DONE 返回生命周期终态结果。
```

关键副作用：任何误用 `success` 判断 completion 的旧路径都会失效，必须改为 `terminal_done`。

## 6. replay/redirect/fault 中间态清理

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，函数 `mark_target_fault()`。

函数功能简析：fault 写回时只落 fault 和 exception pending，不直接设置终态；终态必须等 ROB commit/deq 前沿。

```systemverilog
status.fault             = 1'b1;
status.exception_pending = 1'b1;
status.exception_vec     = exception_vec;
status.pass              = 1'b0;
status.success           = 1'b0;
status.terminal_done     = 1'b0;
status.last_event_cycle  = cycle;
```

中文伪代码：

```text
fault event 通过 epoch/replay 检查后落表：
  设置 uid 级 fault 和 exception_pending；
  保存 exception_vec；
  清 pass 和 success，避免后续把 fault 当作 normal pass；
  清 terminal_done，说明 fault 刚采集时还不是生命周期终态；
  记录事件周期。
```

关键副作用：fault 落表后会阻塞 normal pass，但不会提前推进 completion。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，函数 `mark_replay_pending()`。

函数功能简析：STA/LOAD replay 是中间态，需要清除当前写回/pass/terminal 状态并准备重发。

```systemverilog
delete_issue_queue_entry(target, uid, 0, 1'b0);
status.replay_pending = 1'b1;
status.writeback      = 1'b0;
status.pass           = 1'b0;
status.success        = 1'b0;
status.terminal_done  = 1'b0;
status.last_event_cycle = cycle;
```

中文伪代码：

```text
mark_replay_pending 先删除对应 target 的旧 issue queue 项：
  避免旧发射项继续被选择。
设置 replay_pending=1：
  表示该 uid 正在等待 replay 重发。
清 writeback、pass、success 和 terminal_done：
  replay 是中间态，不允许被当作完成；
  后续必须重新 route、fire、writeback、commit 后才能终态。
更新 last_event_cycle：
  便于 debug replay 发生时间。
```

关键副作用：replay 不会误推进 `terminal_done_uid`。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，函数 `clear_uid_dispatch_result()` 和 `prepare_uid_for_redirect_reissue()`。

函数功能简析：redirect flush 旧动态实例时清空 dispatch/writeback/pass/fault/replay 结果，并禁止已经终态的 uid 被 redirect 重新 flush。

```systemverilog
status.success         = 1'b0;
status.terminal_done   = 1'b0;
status.fault           = 1'b0;
status.load_fault      = 1'b0;
status.sta_fault       = 1'b0;
status.std_fault       = 1'b0;

...

if (status.terminal_done) begin
    `uvm_fatal("COMMON_DATA",
               $sformatf("redirect tries to flush already terminal_done uid=%0d", uid))
end
```

中文伪代码：

```text
clear_uid_dispatch_result 清旧动态实例状态：
  清 success 和 terminal_done；
  清 uid 级 fault 和各 target fault；
  让该 uid 可以按新动态实例重新 admission/issue/writeback。
prepare_uid_for_redirect_reissue 开始前检查 terminal_done：
  如果 uid 已经终态，直接 fatal；
  终态 uid 不能被 redirect 当成活跃旧实例再 flush。
```

关键副作用：redirect 只处理未终态动态实例；已终态 uid 不参与 reissue。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，函数 `apply_redirect_flush_range()` 和 `activate_uid()`。

函数功能简析：redirect flush 和 LSQ admission 都把 `terminal_done` 当作不可再处理的终态保护。

```systemverilog
advance_terminal_done_uid();
begin_uid = get_active_scan_begin_uid();
end_uid   = get_active_scan_end_uid();
...
if (status.terminal_done || (!status.active && !status.writeback && !status.pass)) begin
    continue;
end

...

if (status.terminal_done) begin
    `uvm_fatal("COMMON_DATA", $sformatf("activate_uid got terminal_done uid=%0d", uid))
end
```

中文伪代码：

```text
apply_redirect_flush_range 先推进 terminal_done_uid：
  缩小 redirect 扫描窗口；
  已终态 uid 不再被扫描和 flush。
遍历 active window 时：
  如果 uid 已 terminal_done，跳过；
  如果 uid 不 active 且没有 writeback/pass，跳过；
  其余 uid 再按 ROB 顺序判断是否被 redirect 覆盖。
activate_uid 在新 admission 前检查 terminal_done：
  如果 uid 已终态，fatal；
  防止已完成 uid 被重新激活。
```

关键副作用：fault 终态 `success=0 && terminal_done=1` 也会被正确保护，不会因为 `success=0` 被误重发。

## 7. commit candidate 和 commit cursor

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/lsq_commit_handler.sv`，函数 `uid_is_normal_commit_candidate()`、`uid_is_fault_terminal_candidate()`、`uid_is_commit_candidate()`。

函数功能简析：commit candidate 拆成 normal pass 和 fault terminal 两类；global flush 仍是总 gating。

```systemverilog
function bit uid_is_normal_commit_candidate(input memblock_uid_t uid);
    status = data.get_status(uid);
    return status.active &&
           status.writeback &&
           status.pass &&
           data.required_targets_done(uid) &&
           !status.rob_commit &&
           !status.fault &&
           !status.exception_pending &&
           !status.replay_pending &&
           !status.redirect_pending &&
           !status.flushed &&
           !status.issue_killed;
endfunction:uid_is_normal_commit_candidate

function bit uid_is_fault_terminal_candidate(input memblock_uid_t uid);
    status = data.get_status(uid);
    if (!status.active || status.rob_commit ||
        status.replay_pending || status.redirect_pending ||
        status.flushed || status.issue_killed) begin
        return 1'b0;
    end
    if (!status.writeback &&
        !status.load_fault && !status.sta_fault && !status.std_fault) begin
        return 1'b0;
    end
    return status.fault ||
           status.exception_pending ||
           status.load_fault ||
           status.sta_fault ||
           status.std_fault;
endfunction:uid_is_fault_terminal_candidate

function bit uid_is_commit_candidate(input memblock_uid_t uid);
    if (data.issue_blocked_by_global_flush()) begin
        return 1'b0;
    end
    return uid_is_normal_commit_candidate(uid) ||
           uid_is_fault_terminal_candidate(uid);
endfunction:uid_is_commit_candidate
```

中文伪代码：

```text
uid_is_normal_commit_candidate 判断 normal pass 是否可提交：
  uid 必须 active；
  必须已经 writeback/pass；
  required_targets_done 必须满足；
  不能已经 rob_commit；
  不能有 fault、exception、replay、redirect、flushed 或 issue_killed。
uid_is_fault_terminal_candidate 判断 fault 是否可作为终态提交：
  uid 必须 active 且未 rob_commit；
  replay/redirect/flushed/issue_killed 这些中间态必须为 0；
  必须已有 writeback 或 target fault 落表，避免空 fault 候选；
  只要 uid 级 fault、exception_pending 或 target fault 任一存在，就允许作为 fault terminal candidate。
uid_is_commit_candidate 先检查 global flush：
  如果全局 flush/redirect 阻塞，直接返回 false；
  否则 normal candidate 或 fault terminal candidate 任一满足即可提交。
```

关键副作用：fault uid 有了 ROB commit 入口，但不会越过 replay/redirect/flushed 中间态。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/lsq_commit_handler.sv`，函数 `advance_commit_cursor_past_done()` 和 `select_rob_commit_batch()`.

函数功能简析：commit cursor 只跨过 `terminal_done`；batch selection 遇到 fault terminal candidate 后提交该 uid 并停止，避免更年轻 uid 越过 fault frontier。

```systemverilog
while (commit_cursor_uid < data.main_trans_num) begin
    status = data.get_status(commit_cursor_uid);
    if (status.terminal_done) begin
        commit_cursor_uid++;
    end else begin
        break;
    end
end

...

uids.delete();
if (data.issue_blocked_by_global_flush()) begin
    return;
end
advance_commit_cursor_past_done();
uid = commit_cursor_uid;
while (uid < data.main_trans_num && uids.size() < MEMBLOCK_COMMIT_WIDTH) begin
    if (data.get_status(uid).terminal_done) begin
        commit_cursor_uid = uid + 1;
        uid++;
        continue;
    end
    if (uid_is_normal_commit_candidate(uid)) begin
        uids.push_back(uid);
        uid++;
        continue;
    end
    if (uid_is_fault_terminal_candidate(uid)) begin
        uids.push_back(uid);
        break;
    end
    break;
end
```

中文伪代码：

```text
advance_commit_cursor_past_done 从当前 commit_cursor_uid 开始：
  如果当前 uid 已 terminal_done，cursor 前进一步；
  如果当前 uid 未 terminal_done，停止；
  flushed 不是终态，所以不能被 cursor 跳过。
select_rob_commit_batch 每拍先清空输出 uid 列表：
  防止上一拍 commit uid 残留。
如果 global flush 阻塞：
  不选择任何 commit。
调用 advance_commit_cursor_past_done：
  跳过已经终态的 uid。
从 commit_cursor_uid 开始顺序扫描：
  如果 uid 已 terminal_done，更新 cursor 并继续；
  如果 uid 是 normal candidate，加入 batch 后继续找下一个 uid；
  如果 uid 是 fault terminal candidate，加入 batch 后停止；
  如果 uid 两类 candidate 都不是，停止扫描，保证 ROB 顺序。
```

关键副作用：fault frontier 不会被后续 uid 越过；normal pass 仍可按 commit width 连续提交。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/lsq_commit_handler.sv`，函数 `mark_rob_commit_uid()`。

函数功能简析：commit 落表点设置 `rob_commit`，必要时直接置 `lsq_deq`，然后交给 `try_retire_committed_uid()` 判断是否进入终态。

```systemverilog
if (data.issue_blocked_by_global_flush()) begin
    `uvm_info("LSQ_COMMIT", $sformatf("skip ROB commit uid=%0d because redirect/flush is in progress", uid), UVM_LOW)
    return;
end
if (!uid_is_commit_candidate(uid)) begin
    `uvm_info("LSQ_COMMIT", $sformatf("skip non-eligible ROB commit uid=%0d ... terminal_done=%0d", uid, status.terminal_done), UVM_LOW)
    return;
end
fault_candidate = uid_is_fault_terminal_candidate(uid);
status.rob_commit       = 1'b1;
status.last_event_cycle = $time;
if (!status.active_lq_mapped && !status.active_sq_mapped) begin
    status.lsq_deq = 1'b1;
end
data.try_retire_committed_uid(uid);
advance_commit_cursor_past_done();
```

中文伪代码：

```text
mark_rob_commit_uid 重新读取 uid status：
  如果 global flush 正在阻塞，打印 info 后跳过；
  如果 uid 不再满足 commit candidate，打印 info 后跳过；
  这两个分支属于 commit 选择到落表之间的竞争窗口，不作为 warning。
记录 fault_candidate：
  便于日志区分 normal commit 和 fault terminal commit。
设置 rob_commit=1 并更新时间：
  表示该 uid 已经到达 ROB commit 阶段。
如果 uid 没有 active LQ/SQ map：
  直接设置 lsq_deq=1，因为无需等待 DUT deq。
调用 try_retire_committed_uid：
  根据 commit、deq、fault/replay/redirect 状态决定是否终态。
最后推进 commit cursor：
  新增 terminal_done uid 会被 cursor 跨过。
```

关键副作用：commit 落表和最终 retire 分离，符合 ROB commit 与 LQ/SQ deq 两条路径汇合的现有结构。

## 8. retire 收口

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，函数 `consume_fault_retire()`。

函数功能简析：fault/exception uid 在 ROB commit 且 LQ/SQ deq 后进入非成功终态。

```systemverilog
if (!status.fault && !status.exception_pending &&
    !status.load_fault && !status.sta_fault && !status.std_fault) begin
    `uvm_fatal("COMMON_DATA", $sformatf("consume_fault_retire called for non-fault uid=%0d", uid))
end
status.exception_pending = 1'b0;
set_status_field(uid, MEMBLOCK_STATUS_SUCCESS, 1'b0);
set_status_field(uid, MEMBLOCK_STATUS_TERMINAL_DONE, 1'b1);
retire_active_uid(uid);
```

中文伪代码：

```text
consume_fault_retire 先验证调用对象：
  如果 uid 没有 fault、exception_pending 或 target fault，fatal；
  该函数只能处理 fault/exception 终态。
清 exception_pending：
  fault 已经被 commit retire 消费，不再是 pending 中间态。
设置 success=0：
  保留非 normal pass 的结果语义。
设置 terminal_done=1：
  让 terminal_done_uid 可以越过该 uid。
调用 retire_active_uid：
  释放 active ROB/LQ/SQ map 并清 active。
```

关键副作用：合法 fault 不再卡住测试完成，且不会被伪装成 success。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，函数 `try_retire_committed_uid()`。

函数功能简析：这是 ROB commit 与 LQ/SQ deq 的最终收口，负责区分 redirect、replay/flushed、fault terminal 和 normal pass。

```systemverilog
if (!status.active || !status.rob_commit) begin
    return;
end
if (status.active_lq_mapped || status.active_sq_mapped) begin
    return;
end
if (active_redirect.valid &&
    rob_order_util::rob_need_flush(status.get_rob_key(), active_redirect)) begin
    prepare_uid_for_redirect_reissue(uid, active_redirect);
    return;
end
if (status.replay_pending || status.redirect_pending || status.flushed ||
    status.issue_killed) begin
    return;
end
if (status.fault || status.exception_pending ||
    status.load_fault || status.sta_fault || status.std_fault) begin
    consume_fault_retire(uid);
    return;
end
if (!status.pass || !required_targets_done(uid)) begin
    return;
end
set_status_field(uid, MEMBLOCK_STATUS_SUCCESS, 1'b1);
set_status_field(uid, MEMBLOCK_STATUS_TERMINAL_DONE, 1'b1);
retire_active_uid(uid);
```

中文伪代码：

```text
try_retire_committed_uid 首先检查 active 和 rob_commit：
  uid 不 active 或还没 ROB commit 时直接返回。
检查 active LQ/SQ map：
  如果 LQ/SQ 资源还没 deq，返回等待 deq monitor。
检查 active_redirect 覆盖：
  如果当前 redirect 覆盖该 uid，调用 prepare_uid_for_redirect_reissue；
  不设置 success 或 terminal_done。
检查 replay/redirect/flushed/issue_killed：
  这些都是中间态，直接返回等待 recovery 或 reissue。
检查 fault/exception：
  如果 fault、exception_pending 或 target fault 存在，调用 consume_fault_retire；
  该分支设置 success=0、terminal_done=1，并释放 active uid。
检查 normal pass 条件：
  如果 pass=0 或 required_targets_done 不满足，返回等待 writeback/pass 补齐。
normal pass retire：
  设置 success=1；
  设置 terminal_done=1，并推进 terminal_done_uid；
  调用 retire_active_uid 释放 active 状态。
```

关键副作用：redirect 覆盖优先于 fault/normal 终态；fault 和 normal 都必须等 commit/deq 汇合后才能终态。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`，函数 `end_test_check()`。

函数功能简析：最终一致性检查从“必须 success”改成“必须 terminal_done 且无中间态残留”。

```systemverilog
if (!status_by_uid[uid].terminal_done) begin
    `uvm_error("COMMON_DATA", $sformatf("uid=%0d is not terminal_done at end_test_check", uid))
end
if (status_by_uid[uid].active ||
    status_by_uid[uid].exception_pending ||
    status_by_uid[uid].replay_pending ||
    status_by_uid[uid].redirect_pending) begin
    `uvm_error("COMMON_DATA", $sformatf("uid=%0d has unfinished status at end_test_check", uid))
end
if (status_by_uid[uid].terminal_done &&
    (status_by_uid[uid].flushed ||
     status_by_uid[uid].issue_killed)) begin
    `uvm_error("COMMON_DATA", $sformatf("uid=%0d has terminal_done with stale intermediate state", uid))
end
```

中文伪代码：

```text
end_test_check 遍历每个 uid：
  如果 terminal_done=0，报 error；
  如果 active、exception_pending、replay_pending 或 redirect_pending 仍然存在，报 error；
  如果 terminal_done=1 但 flushed 或 issue_killed 仍然残留，报 error。
该检查不要求 success=1：
  success=0 && terminal_done=1 的 fault/exception 终态是合法完成形态；
  但任何 pending/active/flushed 中间态不能残留到测试结束。
```

关键副作用：fault 终态不会被误判为未完成，真正未清干净的状态仍会 fail。

## 9. admission、route 和 real smoke 退出

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/issue_queue_scheduler.sv`，函数 `route_all_ready_uids()`。

函数功能简析：route 扫描窗口从 `terminal_done_uid` 到 `max_enqueued_uid`，避免每拍全表扫描。

```systemverilog
if (data.issue_blocked_by_global_flush()) begin
    return;
end

data.advance_terminal_done_uid();
begin_uid = data.get_active_scan_begin_uid();
end_uid   = data.get_active_scan_end_uid();
```

中文伪代码：

```text
route_all_ready_uids 先检查全局 flush/redirect 阻塞：
  阻塞时直接返回，不 route 新 issue item。
未阻塞时调用 advance_terminal_done_uid：
  跳过已经终态的连续 uid。
读取 active scan begin/end：
  begin 来自 terminal_done_uid；
  end 来自 LSQ admission 高水位；
  后续只扫描这个有限 active window。
```

关键副作用：route 不再依赖 `success` 前缀，也不会扫描已经 fault terminal 的 uid。

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqenq_dispatch_sequence.sv`，函数 `next_uid_needs_lsq_admission()` 和 `collect_lsq_candidates()`。

函数功能简析：LSQ admission 判断已完成 uid 时使用 `terminal_done`，避免 fault 终态被重新 admission。

```systemverilog
if (status.terminal_done || status.active || status.enq ||
    status.exception_pending || status.replay_pending) begin
    return 1'b0;
end

...

if (status.terminal_done || status.active || status.enq ||
    status.exception_pending || status.replay_pending) begin
    break;
end
```

中文伪代码：

```text
next_uid_needs_lsq_admission 检查下一条 uid：
  如果 uid 已 terminal_done，不再 admission；
  如果 uid active、已 enq、exception_pending 或 replay_pending，也不能作为新 admission 候选。
collect_lsq_candidates 连续收集本拍候选：
  遇到 terminal_done/active/enq/exception/replay 中任一状态就停止；
  避免跨过不该 admission 的 uid。
```

关键副作用：fault terminal uid 不会因为 `success=0` 被误当作未完成重入 LSQ。

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_sequence.sv`，函数 `service_real_dispatch_flow()`、`all_transactions_terminal_done()`、`report_unfinished_status()`。

函数功能简析：真实 dispatch 主循环用 `terminal_done` 请求 global stop，timeout dump 不再把 `success=0` 单独视为未完成。

```systemverilog
service_monitor_once();
if (!data.is_global_stop_requested()) begin
    route_all_issue_queues();
end
void'(all_transactions_terminal_done());
if (data.is_global_stop_requested() &&
    !data.flushsb_request_pending()) begin
    break;
end

...

function bit memblock_dispatch_real_smoke_sequence::all_transactions_terminal_done();
    if (data == null || data.main_trans_num == 0) begin
        return 1'b0;
    end
    data.request_global_stop_if_done();
    return data.is_global_stop_requested();
endfunction:all_transactions_terminal_done

...

if (!status.terminal_done ||
    status.active ||
    status.exception_pending || status.replay_pending ||
    status.redirect_pending || status.flushed ||
    !status.enq || !status.issue_ready ||
    !status.writeback || !status.rob_commit || !status.lsq_deq) begin
    `uvm_info(get_type_name(), ...)
end
```

中文伪代码：

```text
service_real_dispatch_flow 每拍先服务 monitor/recovery：
  然后如果 global stop 还没请求，继续 route issue queue；
  调用 all_transactions_terminal_done 请求公共完成检查；
  如果 global stop 已请求且 flushSb 没有 pending，退出主循环。
all_transactions_terminal_done：
  data 为空或没有主表时返回 false；
  调用 request_global_stop_if_done，让 common_data 基于 terminal_done_uid 判断全表完成；
  返回 global_stop_requested。
report_unfinished_status：
  如果 uid 没有 terminal_done，或者仍有 active/pending/flushed/未 commit/deq 等状态，打印未完成状态；
  不再因为 success=0 单独打印 unfinished，因为 fault terminal 允许 success=0。
```

关键副作用：real smoke 退出条件和 debug dump 均以生命周期终态为准。

## 10. soft testcase 覆盖

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/soft_test/soft_test_memblock_dispatch_fault_smoke_sequence.sv`，task `body()` 和 `commit_and_deq_lsq()`。

函数功能简析：新增 software-only fault smoke，用 uid0 LOAD fault 验证 fault terminal candidate、commit/deq 和 terminal_done 前缀。

```systemverilog
build_directed_main_table();
admit_lsq_and_route_issue();
fire_all_issue_items(fired_items);
if (!find_fired_item(fired_items, 0, MEMBLOCK_ISSUE_TARGET_LOAD, fault_item)) begin
    `uvm_fatal(get_type_name(), "fault smoke did not fire directed LOAD item")
end
inject_fault_writeback_events(fired_items, 0, MEMBLOCK_ISSUE_TARGET_LOAD);
commit_and_deq_lsq();
check_fault_terminal_status(0, MEMBLOCK_ISSUE_TARGET_LOAD);
data.end_test_check();

...

commit_handler.build_lsqcommit_xaction(commit_tr, commit_uids, has_commit);
if (!has_commit || commit_uids.size() != 1 || commit_uids[0] != 0) begin
    `uvm_fatal(get_type_name(), ...)
end
commit_handler.mark_rob_commit_batch(commit_uids);
lq_deq_head = lsq_ctrl.lq_deq_ptr;
commit_handler.apply_dut_lq_deq(1, lq_deq_head, 1'b0);
```

中文伪代码：

```text
fault smoke body 构建定向主表：
  admission、route、fire 所有 issue item；
  查找 uid0 的 LOAD issue item，找不到则 fatal；
  对 uid0 LOAD 注入 fault writeback，其它 item 注入 pass；
  调用 commit_and_deq_lsq 驱动 commit/deq；
  检查 fault terminal 状态；
  最后调用 end_test_check 做公共一致性检查。
commit_and_deq_lsq 第一轮 build commit xaction：
  要求第一批只有 fault uid0；
  调用 mark_rob_commit_batch 标记 ROB commit；
  调用 apply_dut_lq_deq 模拟 LQ deq；
  这会触发 uid0 fault retire，验证 fault frontier 不被更年轻 uid 越过。
```

关键副作用：验证 fault candidate 加入 batch 后停止、fault uid deq 后 `success=0 && terminal_done=1`。

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/soft_test/soft_test_memblock_dispatch_fault_smoke_sequence.sv`，task `check_fault_terminal_status()`。

函数功能简析：检查 fault uid 和非 fault uid 的最终状态，并要求 `terminal_done_uid == main_trans_num`。

```systemverilog
if (uid == fault_uid) begin
    if (!status.terminal_done || status.success || !status.fault ||
        status.exception_pending || status.active ||
        !status.rob_commit || !status.lsq_deq) begin
        `uvm_fatal(get_type_name(), ...)
    end
    if (fault_target == MEMBLOCK_ISSUE_TARGET_LOAD && !status.load_fault) begin
        `uvm_fatal(get_type_name(), "expected load_fault on directed fault uid")
    end
end else if (status.active || !status.enq || !status.issue_ready ||
             !status.writeback || !status.pass || status.fault ||
             !status.rob_commit || !status.lsq_deq ||
             !status.success || !status.terminal_done) begin
    `uvm_fatal(get_type_name(), ...)
end
...
if (data.dispatch_progress.terminal_done_uid != data.main_trans_num) begin
    `uvm_fatal(get_type_name(), ...)
end
```

中文伪代码：

```text
遍历所有 uid：
  如果是 fault uid：
    要求 terminal_done=1；
    要求 success=0；
    要求 fault=1、exception_pending=0、active=0；
    要求 rob_commit=1 且 lsq_deq=1；
    对 LOAD fault 额外要求 load_fault=1。
  如果不是 fault uid：
    要求 active=0；
    要求 enq、issue_ready、writeback、pass、rob_commit、lsq_deq、success、terminal_done 全部满足；
    要求 fault=0。
遍历结束后检查 terminal_done_uid：
  必须等于 main_trans_num；
  证明 fault uid 没有卡住完成前缀。
```

关键副作用：覆盖 `success=0 && terminal_done=1` 的合法终态。

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/soft_test/soft_test_memblock_dispatch_replay_smoke_sequence.sv`，function `make_replay_wb_event()`。

函数功能简析：soft replay smoke 构造 STA IQ feedback miss 事件，用于验证 replay pending 不误置 terminal_done。

```systemverilog
wb_event.issue_epoch     = status.get_target_issue_epoch(item.target);
wb_event.has_issue_epoch = 1'b1;
wb_event.replay_seq      = status.replay_seq;
wb_event.has_replay_seq  = 1'b1;
wb_event.iq_feedback_valid  = 1'b1;
wb_event.iq_feedback_hit    = 1'b0;
wb_event.iq_feedback_failed = 1'b1;
wb_event.replay_valid    = 1'b1;
wb_event.cycle           = $time;
```

中文伪代码：

```text
make_replay_wb_event 构造 soft STA replay event：
  从当前 status 取 target issue_epoch；
  写入当前 replay_seq；
  设置 has_issue_epoch 和 has_replay_seq，保证 batch handler 能做 stale 过滤；
  设置 iq_feedback_valid=1、iq_feedback_hit=0、iq_feedback_failed=1；
  这些字段让事件和真实 raw IQ feedback STA miss 一致，能进入 handle_issue_feedback_event 的 failed 分支；
  设置 replay_valid=1，使 recovery handler 后续调用 mark_replay_pending。
```

关键副作用：replay smoke 不再只构造裸 replay_valid，而是覆盖真实 IQ feedback failed replay 路径。

源码位置：`mem_ut/ver/ut/memblock/seq/seq_pkg.sv` 和 `mem_ut/ver/ut/memblock/tc/tc_pkg.sv`，package include。

函数功能简析：将新增 fault smoke sequence/testcase 纳入编译 package。

```systemverilog
`include "soft_test_memblock_dispatch_fault_smoke_sequence.sv"

...

`include "soft_test_tc_dispatch_fault_smoke.sv"
```

中文伪代码：

```text
seq_pkg include 新增 fault smoke sequence：
  让 sequence class 进入 seq_pkg 编译作用域。
tc_pkg include 新增 fault smoke testcase：
  让 UVM_TESTNAME=tc_dispatch_fault_smoke 能找到 testcase class。
```

关键副作用：`make eda_run tc=tc_dispatch_fault_smoke mode=base_fun` 可以直接运行新增验证。

## 11. 文档同步

本轮同步的当前有效文档：

- `AI_DOC/project_management/mem_ut_test_framework_logic_build_rule.md`
- `AI_DOC/project_management/mem_ut_code_review_document_rule.md`
- `AI_DOC/mem_ut_flow_doc/load_sta_std_issue_flow.md`
- `AI_DOC/mem_ut_flow_doc/lsq_admission_flow.md`
- `AI_DOC/mem_ut_flow_doc/main_table_build_and_stimulus_flow.md`
- `AI_DOC/mem_ut_flow_doc/normal_pass_flow.md`
- `AI_DOC/mem_ut_flow_doc/redirect_flow.md`
- `AI_DOC/mem_ut_flow_doc/rob_commit_lq_sq_deq_flow.md`
- `AI_DOC/mem_ut_flow_doc/writeback_function_call_flow.md`
- `AI_DOC/mem_ut_flow_doc/fault_exception_flow.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/common_data_transaction.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_dispatch_real_smoke_sequence.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_dispatch_types.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/issue_queue_scheduler.md`
- `AI_DOC/analysis/framework_design/dispatch_backend_interface_closure_code_changes.md`
- `AI_DOC/web/memblock_dispatch_control_flow_callgraph.md`
- `AI_DOC/web/web_assets/memblock_dispatch_doc.js`
- `AI_DOC/web/memblock_dispatch_control_flow_callgraph/assets/app.js`
- `AI_DOC/web/memblock_dispatch_control_flow_callgraph_enhanced/assets/app.js`

同步结果：

- 当前有效 flow/rule 文档中不再残留 `success_prefix_uid`、`advance_success_prefix`、`all_transactions_success`。
- `fault_exception_flow.md` 已补充 fault 从 pending 到 `success=0 && terminal_done=1` 的 terminal retire 逻辑。
- `rob_commit_lq_sq_deq_flow.md` 已补充 normal candidate / fault terminal candidate / terminal_done cursor。
- subagent 复查发现的 analysis/web 旧 `success_prefix/all_transactions_success` 残留已全部同步为 `terminal_done_uid/all_transactions_terminal_done`。

## 12. Plan 对齐检查

关联 plan 已找到：

```text
AI_DOC/plan/test_framework/plan/do/terminal_done_prefix_refactor_plan_20260627.md
```

### 12.1 实现与 Plan 不一致项

未发现实现与 Plan 主体不一致项；当前 coding 行为与对应 plan 保持一致。

说明：

- plan 要求 `success_prefix_uid` 更名为 `terminal_done_uid`：已实现。
- plan 要求 `success` 不再推进 completion：已实现。
- plan 要求 fault terminal candidate：已实现。
- plan 要求 fault retire 后 `success=0 && terminal_done=1`：已实现。
- plan 要求 real smoke 改为 `all_transactions_terminal_done()`：已实现。
- plan 要求新增最小 fault directed testcase：已实现为 `tc_dispatch_fault_smoke`。

### 12.2 Plan 未说明但 Coding 落实的细节

#### 12.2.1 replay soft testcase 事件构造补齐 IQ feedback failed 字段

Plan 只要求 replay smoke 验证 replay 不误置 terminal_done，未详细描述 soft replay event 的字段构造。实际验证时发现 soft testcase 只设置 `replay_valid`，但 source 是 `MEMBLOCK_WB_EVENT_SOURCE_STA_FEEDBACK`；batch handler 会先进入 `handle_issue_feedback_event()`，该路径要求 `iq_feedback_valid=1 && iq_feedback_failed=1` 才会 push recovery event。真实 raw IQ feedback adapter 对 STA miss 也会这样设置，因此补齐该字段是对齐 DUT monitor 语义，不改变 terminal_done 主设计。

源码位置：`mem_ut/ver/ut/memblock/seq/virtual_sequence/soft_test/soft_test_memblock_dispatch_replay_smoke_sequence.sv`，function `make_replay_wb_event()`。

函数功能简析：构造和真实 STA IQ feedback miss 一致的 replay event。

```systemverilog
wb_event.iq_feedback_valid  = 1'b1;
wb_event.iq_feedback_hit    = 1'b0;
wb_event.iq_feedback_failed = 1'b1;
wb_event.replay_valid       = 1'b1;
```

中文伪代码：

```text
soft replay event 除 replay_valid 外，还补齐 IQ feedback miss 语义：
  iq_feedback_valid=1 表示这是 IQ feedback；
  iq_feedback_hit=0 表示本次 issue feedback 未命中成功；
  iq_feedback_failed=1 表示该 STA feedback 应进入 replay recovery；
  replay_valid=1 让 recovery handler 标记 replay_pending。
这样 soft testcase 事件路径与真实 convert_raw_iq_feedback 的 STA miss 行为一致。
```

处理结论：保持当前实现，并在本 review 中记录为 coding 验证阶段补充细节。

## 13. 静态检查

已执行：

```text
rg -n "success_prefix_uid|advance_success_prefix|all_transactions_success|success 前缀|如果写 success|已 success|已经 success" AI_DOC/mem_ut_flow_doc AI_DOC/project_management mem_ut/ver/ut/memblock/seq mem_ut/ver/ut/memblock/tc
```

结果：无命中。

已执行：

```text
rg -n "success_prefix_uid|advance_success_prefix|all_transactions_success|until all success|success_prefix|success 前缀|未 flushed uid .*success|均 success|必须 success|连续 success" AI_DOC/analysis AI_DOC/web
```

结果：无命中。

已执行：

```text
git diff --check -- <本轮 terminal_done 相关源码和文档>
```

结果：通过。

## 14. 仿真验证

已执行并通过：

| 命令 | 结果 |
|---|---|
| `make eda_compile tc=tc_sanity mode=base_fun` | 通过，KDB 0 error / 0 warning。 |
| `make eda_run tc=tc_sanity mode=base_fun` | 通过，UVM_ERROR=0 / UVM_FATAL=0；存在既有 main table wait warning。 |
| `make eda_run tc=tc_dispatch_real_smoke mode=base_fun cfg=tc_dispatch_real_smoke` | 通过，UVM_WARNING=0 / UVM_ERROR=0 / UVM_FATAL=0。 |
| `make eda_run tc=tc_dispatch_fault_smoke mode=base_fun` | 通过，UVM_WARNING=0 / UVM_ERROR=0 / UVM_FATAL=0。 |
| `make eda_run tc=tc_dispatch_replay_smoke mode=base_fun` | 通过，UVM_WARNING=0 / UVM_ERROR=0 / UVM_FATAL=0。 |

说明：`tc_sanity` 的 main table wait warning 是该 testcase 不构建 dispatch real smoke main table 的既有行为，本轮未引入新的 error/fatal。

## 15. 剩余风险

- store/mixed smoke 未在本轮最终收尾阶段重新执行；plan 将其列为建议补充验证，不是 mandatory gate。
- real DUT fault 场景仍依赖后续真正 fault 激励和 checker 扩展；本轮新增的是 software-only fault terminal_done 闭环验证。
- `success=0 && terminal_done=1` 后续如果引入 expected/unexpected fault 白名单，需要单独 plan 扩展 checker 语义。
