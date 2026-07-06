# terminal_done 完成前缀重构方案

状态：undo

创建日期：2026-06-27

整理日期：2026-06-28

关联源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/status_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_types.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/lsq_commit_handler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/issue_queue_scheduler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/writeback_status_handler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/dispatch_monitor_event_adapter.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/dispatch_monitor_batch_handler.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqenq_dispatch_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lintsissue_dispatch_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqcommit_dispatch_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/soft_test/soft_test_memblock_dispatch_smoke_sequence.sv`
- `AI_DOC/mem_ut_flow_doc/*.md`

## 1. 背景和最终目标

当前测试框架使用 `dispatch_progress.success_prefix_uid` 表示从 uid0 开始连续完成的前缀，并用它触发 `transaction_done()` 和 `global_stop_requested`。但现有 `advance_success_prefix()` 只检查 `status.success`：

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
endfunction:advance_success_prefix
```

文字伪代码：

```text
advance_success_prefix：
  从 success_prefix_uid 指向的 uid 开始检查。
  如果该 uid 的 success 为 0：
    停止推进前缀。
  如果该 uid 的 success 为 1：
    success_prefix_uid 加 1，继续检查下一个 uid。
```

问题是 `success=0` 不一定代表 uid 没完成。fault/exception 场景中，uid 可以已经到达最终终态，但不是正常成功结果：

```text
fault retire 后：
  success = 0
  terminal_done = 1
  active = 0
  exception_pending = 0
```

如果仍用 `success` 推进 completion，fault uid 会卡住 `success_prefix_uid`，主动 sequence 无法收到 `global_stop_requested`。

本方案最终目标：

```text
拆分两个语义：
  success：正常成功结果。
  terminal_done：uid 生命周期已经到达测试框架认可的终态。

全局 completion 改为：
  terminal_done_uid >= main_trans_num

fault/exception 允许作为非成功终态：
  success = 0
  terminal_done = 1
```

## 2. 关键语义和命名替换

| 语义 | 字段 | 含义 |
|---|---|---|
| 正常成功结果 | `status.success` | uid 没有 fault/replay/redirect/flushed，按正常路径完成。 |
| 最终完成状态 | `status.terminal_done` | uid 已经到达最终态，可以推进完成前缀。 |
| 完成前缀 | `dispatch_progress.terminal_done_uid` | 从 uid0 开始连续 `terminal_done=1` 后的第一个未终态 uid。 |

本次不保留旧完成前缀命名兼容：

| 旧命名 | 新命名 | 修改原因 |
|---|---|---|
| `success_prefix_uid` | `terminal_done_uid` | 边界表示连续终态完成，不再等同连续 success。 |
| `advance_success_prefix()` | `advance_terminal_done_uid()` | 推进依据改为 `terminal_done`。 |
| `success_prefix=%0d` | `terminal_done_uid=%0d` | 避免日志误导。 |
| `all_transactions_success()` | `all_transactions_terminal_done()` | real smoke 退出条件不再基于 success。 |

文字伪代码：

```text
normal pass retire：
  success = 1
  terminal_done = 1

fault retire：
  success = 0
  terminal_done = 1

replay/redirect/flushed 中间态：
  success = 0
  terminal_done = 0
```

## 3. 状态字段改造

### 3.1 `status_transaction` 新增字段

修改文件：`mem_ut/ver/ut/memblock/seq/base_seq/status_transaction.sv`

新增字段：

```systemverilog
bit terminal_done;
```

reset 中清零：

```systemverilog
terminal_done = 1'b0;
```

文字伪代码：

```text
status_transaction 新增 terminal_done：
  terminal_done=0 表示该 uid 还没到终态。
  terminal_done=1 表示该 uid 当前生命周期已经结束。
  reset、redirect/replay 重发准备、clear 动态实例时必须清零。
```

### 3.2 `memblock_status_field_e` 新增枚举

修改文件：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_types.sv`

新增：

```systemverilog
MEMBLOCK_STATUS_TERMINAL_DONE = 20,
```

说明：当前 `MEMBLOCK_STATUS_SUCCESS=19`，`MEMBLOCK_STATUS_LOAD_WRITEBACK=21`，使用空缺值 20，避免整体重排枚举。

文字伪代码：

```text
MEMBLOCK_STATUS_TERMINAL_DONE：
  作为 set_status_field/get_status_field 的统一入口。
  外部逻辑通过 status field API 读写 terminal_done。
```

### 3.3 `set_status_field()` / `get_status_field()` 同步

修改文件：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`

`MEMBLOCK_STATUS_SUCCESS` 不再推进完成前缀：

```systemverilog
MEMBLOCK_STATUS_SUCCESS: begin
    status.success = value;
end
```

新增 `MEMBLOCK_STATUS_TERMINAL_DONE`：

```systemverilog
MEMBLOCK_STATUS_TERMINAL_DONE: begin
    status.terminal_done = value;
    if (value) begin
        advance_terminal_done_uid();
    end
end
```

`get_status_field()` 也必须新增对应读取：

```systemverilog
MEMBLOCK_STATUS_TERMINAL_DONE: return status.terminal_done;
```

文字伪代码：

```text
set SUCCESS：
  只记录正常成功结果。
  不推进 completion。

set TERMINAL_DONE：
  记录 uid 已进入终态。
  如果置 1：
    推进 terminal_done_uid。

get TERMINAL_DONE：
  返回 status.terminal_done。
```

## 4. 完成前缀和全局退出改造

### 4.1 dispatch progress 字段改名

修改文件：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_types.sv`

当前：

```systemverilog
typedef struct {
    memblock_uid_t success_prefix_uid;
    memblock_uid_t max_enqueued_uid;
    bit            max_enqueued_uid_valid;
} memblock_dispatch_progress_t;
```

修改为：

```systemverilog
typedef struct {
    memblock_uid_t terminal_done_uid;
    memblock_uid_t max_enqueued_uid;
    bit            max_enqueued_uid_valid;
} memblock_dispatch_progress_t;
```

reset 初始化：

```systemverilog
dispatch_progress.terminal_done_uid = 0;
```

文字伪代码：

```text
terminal_done_uid：
  表示从 uid0 开始连续 terminal_done=1 后的第一个 uid。
  中间任意 uid 未 terminal_done 时，后续 uid 即使完成也不能越过它。
```

### 4.2 `advance_terminal_done_uid()`

新增/替换函数：

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
```

文字伪代码：

```text
advance_terminal_done_uid：
  从 terminal_done_uid 指向的 uid 开始检查。
  如果该 uid terminal_done=0：
    停止，说明最老未终态 uid 仍未结束。
  如果该 uid terminal_done=1：
    terminal_done_uid 加 1，继续检查下一个 uid。
```

### 4.3 `transaction_done()` 和 global stop

当前：

```systemverilog
function bit transaction_done();
    advance_success_prefix();
    return dispatch_progress.success_prefix_uid >= main_trans_num;
endfunction:transaction_done
```

修改后：

```systemverilog
function bit transaction_done();
    advance_terminal_done_uid();
    return dispatch_progress.terminal_done_uid >= main_trans_num;
endfunction:transaction_done
```

`request_global_stop_if_done()` 保持入口名，但语义改为所有 uid 终态完成：

```systemverilog
function void request_global_stop_if_done();
    if (transaction_done()) begin
        global_stop_requested = 1'b1;
    end
endfunction:request_global_stop_if_done
```

文字伪代码：

```text
transaction_done：
  先推进 terminal_done_uid。
  如果 terminal_done_uid >= main_trans_num：
    所有 uid 已到终态，返回 true。
  否则返回 false。

request_global_stop_if_done：
  如果 transaction_done 为 true：
    设置 global_stop_requested=1。
```

### 4.4 scan window helper

`get_active_scan_begin_uid()` 改为返回 `terminal_done_uid`：

```systemverilog
function memblock_uid_t get_active_scan_begin_uid();
    return dispatch_progress.terminal_done_uid;
endfunction
```

文字伪代码：

```text
get_active_scan_begin_uid：
  返回最老未 terminal_done uid。
  redirect/route/recovery 只扫描该 uid 之后的活跃窗口。
  已 terminal_done 的 uid 不再参与 redirect/replay/flush/reissue。
```

## 5. Retire 路径改造

### 5.1 terminal_done 置位白名单

`terminal_done` 只能在真正终态置 1。

允许置 `terminal_done=1`：

1. normal pass 已 ROB commit，LQ/SQ active mapping 已释放，且没有 `fault/exception_pending/replay_pending/redirect_pending/flushed`。
2. fault/exception 已 ROB commit，LQ/SQ active mapping 已释放，并且 retire 路径消费该 fault/exception。

禁止置 `terminal_done=1`：

1. `replay_pending=1`。
2. `redirect_pending=1`。
3. `flushed=1`。
4. active 实例仍需要 redirect/reissue 或 replay 恢复。

### 5.2 `try_retire_committed_uid()`

修改文件：`mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`

源码级伪代码：

```text
try_retire_committed_uid(uid):
  status = get_status(uid)

  如果 !status.active 或 !status.rob_commit：
    return

  如果 status.active_lq_mapped 或 status.active_sq_mapped：
    return

  如果 active_redirect.valid 且 rob_need_flush(status.rob_key, active_redirect)：
    prepare_uid_for_redirect_reissue(uid, active_redirect)
    return

  如果 status.replay_pending 或 status.redirect_pending 或 status.flushed：
    不设置 terminal_done
    return

  如果 status.fault 或 status.exception_pending：
    consume_fault_retire(uid)
    return

  如果 status.pass && required_targets_done(uid)：
    set_status_field(uid, MEMBLOCK_STATUS_SUCCESS, 1)
    set_status_field(uid, MEMBLOCK_STATUS_TERMINAL_DONE, 1)
    retire_active_uid(uid)
    return
```

中文文字伪代码：

```text
try_retire_committed_uid：
  只处理已经 active 且 ROB commit 的 uid。
  如果 LQ/SQ active map 还没释放，说明资源生命周期未结束，直接返回。
  如果当前 uid 会被 active redirect 覆盖，进入 redirect reissue 准备，不设置 terminal_done。
  如果 uid 还在 replay/redirect/flushed 中间态，不设置 terminal_done。
  如果 uid 有 fault/exception_pending，走 consume_fault_retire。
  如果 uid 是 normal pass 且 required target 都完成，设置 success=1、terminal_done=1 并释放 active 实例。
```

### 5.3 `consume_fault_retire()`

新增/拆分 fault retire helper：

```text
consume_fault_retire(uid):
  status = get_status(uid)

  如果 !status.fault && !status.exception_pending：
    fatal("consume_fault_retire called for non-fault uid")

  status.exception_pending = 0
  set_status_field(uid, MEMBLOCK_STATUS_SUCCESS, 0)
  set_status_field(uid, MEMBLOCK_STATUS_TERMINAL_DONE, 1)
  retire_active_uid(uid)
```

中文文字伪代码：

```text
consume_fault_retire：
  表示 fault 已经到达 ROB commit frontier，且 LQ/SQ 资源已经释放。
  不清 fault，保留给 final checker/debug。
  清 exception_pending，表示该 fault 已被 retire 路径消费，不再是未完成工作。
  设置 success=0、terminal_done=1。
  释放 active 实例。
```

### 5.4 direct status write 清单

| 函数 | 当前行为 | terminal_done 处理要求 |
|---|---|---|
| `mark_target_fault()` | 设置 `fault=1/exception_pending=1/success=0` | 不置 `terminal_done`。fault 采集时还不是终态，必须等 ROB commit retire。 |
| `mark_replay_pending()` | 设置 `replay_pending=1/success=0` | 必须清 `terminal_done=0`。replay 是中间态。 |
| `clear_uid_dispatch_result()` | 清 dispatch/writeback/pass/success/fault 等动态实例结果 | 必须清 `terminal_done=0`。这是重建动态实例前的统一清理。 |
| `prepare_uid_for_redirect_reissue()` | 清旧实例，置 `redirect_pending/flushed/success=0` | 必须清 `terminal_done=0`，入口必须禁止处理已 terminal_done uid。 |
| `reset_all_tables()` / `status_transaction::reset()` | 初始化状态 | 必须清 `terminal_done=0`。 |
| `try_retire_committed_uid()` normal pass | 设置 success 并 retire | 设置 `success=1` 和 `terminal_done=1`。 |
| `try_retire_committed_uid()` fault retire | 设置非成功终态并 retire | 设置 `success=0` 和 `terminal_done=1`，清 `exception_pending`。 |

文字伪代码：

```text
所有清动态实例的函数：
  success = 0
  terminal_done = 0

所有中间态事件函数：
  success = 0
  terminal_done = 0

所有真正 retire 终态函数：
  terminal_done = 1
  success 根据结果类型设置
```

## 6. Commit candidate 和 commit batch 改造

### 6.1 candidate 拆分

当前 `lsq_commit_handler::uid_is_commit_candidate()` 排除 fault/exception，fault uid 没有 commit retire 入口。需要拆成 normal candidate 和 fault terminal candidate。

修改文件：`mem_ut/ver/ut/memblock/seq/base_seq/lsq_commit_handler.sv`

源码级伪代码：

```text
uid_is_normal_commit_candidate(uid):
  status = get_status(uid)
  return status.active
      && status.writeback
      && status.pass
      && required_targets_done(uid)
      && !status.rob_commit
      && !status.fault
      && !status.exception_pending
      && !status.replay_pending
      && !status.redirect_pending
      && !status.flushed
      && !status.issue_killed

uid_is_fault_terminal_candidate(uid):
  status = get_status(uid)
  如果 !status.active：
    return false
  如果 status.rob_commit：
    return false
  如果 status.replay_pending 或 status.redirect_pending 或 status.flushed 或 status.issue_killed：
    return false
  如果 status.fault 或 status.exception_pending：
    return true
  如果 status.load_fault 或 status.sta_fault 或 status.std_fault：
    return true
  return false

uid_is_commit_candidate(uid):
  如果 global flush/redirect 阻塞：
    return false
  如果 uid_is_normal_commit_candidate(uid)：
    return true
  如果 uid_is_fault_terminal_candidate(uid)：
    return true
  return false
```

中文文字伪代码：

```text
normal candidate：
  只接受正常 pass 且没有 fault/exception/replay/redirect/flushed 的 uid。

fault terminal candidate：
  采用方案 A。
  任意 target fault 或 exception_pending 已落表即可成为终态候选。
  不等待其他 target pass/writeback。
  但 replay/redirect/flushed/issue_killed 仍属于恢复或清理中间态，不能直接提交。
```

### 6.2 fault target 完成规则

本方案采用方案 A：任意 target fault 即可 terminal commit。

```text
load_fault：
  可让该 uid 成为 fault terminal candidate。

sta_fault：
  可让该 uid 成为 fault terminal candidate。
  如果该 store/MOU 同时还有 STD target，不再等待 STD pass/writeback。

std_fault：
  可让该 uid 成为 fault terminal candidate。
  如果该 store/MOU 同时还有 STA target，不再等待 STA pass/writeback。

status.fault 或 status.exception_pending：
  也可让该 uid 成为 fault terminal candidate。
```

采用原因：fault/exception 表示 uid 已进入异常终态。继续等待同一 uid 的其他 target 可能等不到，导致 `terminal_done_uid` 卡住。commit frontier 按 uid 顺序消费，只有该 uid 到达提交前沿才会进入 fault retire，不会让更年轻 uid 越过更老 uid。

### 6.3 `select_rob_commit_batch()` 顺序约束

`select_rob_commit_batch()` 必须保持 ROB 顺序提交语义，不能因为当前最老 uid 是 fault candidate 就跳过它去提交后续 uid。

实现约束：

- 从 `commit_cursor_uid` 开始按 uid 顺序选择 commit batch。
- 如果当前 uid 已 `terminal_done`，允许 cursor 越过它。
- 如果当前 uid 是 normal commit candidate，允许加入 batch，并继续尝试后续 uid，直到达到 `MEMBLOCK_COMMIT_WIDTH`。
- 如果当前 uid 是 fault terminal candidate，必须允许该 uid 加入 batch。
- 如果当前 uid 既不是 normal candidate，也不是 fault terminal candidate，必须停止扫描，不能越过它提交后续 uid。
- 第一版 fault terminal candidate 加入 batch 后停止继续选择后续 uid，下一拍再处理后续 uid。

源码级伪代码：

```text
select_rob_commit_batch：
  先调用 advance_commit_cursor_past_done，越过已经 terminal_done 的 uid。
  从 commit_cursor_uid 开始循环选择。

  对当前 uid：
    如果 uid 已 terminal_done：
      推进 commit cursor。
      继续检查下一个 uid。

    如果 uid 是 normal commit candidate：
      把 uid 加入 commit batch。
      如果 batch 数量未达到 commit_width：
        继续检查下一个 uid。
      否则：
        停止选择。

    如果 uid 是 fault terminal candidate：
      把 uid 加入 commit batch。
      标记本 batch 包含 fault frontier。
      第一版停止继续选择后续 uid。

    如果 uid 既不是 normal candidate，也不是 fault terminal candidate：
      停止选择。
      后续 uid 即使已经 ready，也不能越过当前 uid 被提交。

  返回本拍可提交的 uid batch。
```

行为总结：

```text
当最老未完成 uid 是 fault terminal candidate：
  select_rob_commit_batch 必须选择它。
  不能因为它不是 normal pass 就跳过。
  不能让后续 uid 越过它提交。
  fault uid terminal_done 后，下一拍再继续推进后续 uid。
```

### 6.4 commit cursor

`advance_commit_cursor_past_done()` 从基于 `status.success` 改为基于 `status.terminal_done`：

```systemverilog
if (status.terminal_done) begin
    commit_cursor_uid++;
end else begin
    break;
end
```

文字伪代码：

```text
advance_commit_cursor_past_done：
  从 commit_cursor_uid 开始检查。
  如果当前 uid 已 terminal_done：
    commit cursor 加 1，跳过该终态 uid。
  如果当前 uid 未 terminal_done：
    停止，等待该 uid 成为 normal/fault commit candidate 或 recovery 处理。
```

## 7. redirect/replay/admission 交互

### 7.1 redirect/replay 不允许置 terminal_done

```text
redirect_pending/replay_pending/flushed：
  success = 0
  terminal_done = 0
  不推进 terminal_done_uid
```

文字伪代码：

```text
mark_replay_pending / prepare_uid_for_redirect_reissue / clear_uid_dispatch_result：
  清除当前动态实例的 pass/writeback/dispatch 状态。
  将 success 清 0。
  将 terminal_done 清 0。
  后续重新 admission/issue/writeback/commit 后才能再次 terminal_done。
```

### 7.2 LSQ admission skip 条件

`memblock_lsqenq_dispatch_sequence.sv` 中表达“不能再参与本轮动态实例”的 `status.success` 需要改成 `status.terminal_done`。

示例：

```systemverilog
if (status.terminal_done || status.active || status.enq ||
    status.redirect_pending || status.flushed) begin
    ...
end
```

文字伪代码：

```text
LSQ admission 查找候选 uid：
  如果 uid 已 terminal_done：
    跳过，不再 admission。
  如果 uid active/enq/redirect_pending/flushed：
    按现有逻辑跳过或等待 reissue。
  只有未终态且满足 admission 条件的 uid 才能进入 LSQ。
```

### 7.3 redirect flush 窗口

`apply_redirect_flush_range()` 先调用 `advance_terminal_done_uid()`，扫描窗口从 `terminal_done_uid` 开始。已 terminal_done 的 uid 不能再被 redirect flush。

源码级伪代码：

```text
apply_redirect_flush_range(redirect):
  advance_terminal_done_uid()
  begin_uid = get_active_scan_begin_uid()
  end_uid = get_active_scan_end_uid()
  for uid in [begin_uid, end_uid):
    status = get_status(uid)
    如果 status.terminal_done：
      continue
    如果该 uid 不是 active/writeback/pass 相关动态实例：
      continue
    如果 rob_need_flush(status.rob_key, redirect)：
      prepare_uid_for_redirect_reissue(uid, redirect)
```

文字伪代码：

```text
redirect flush：
  只能处理未 terminal_done 的动态实例。
  terminal_done 表示 uid 生命周期已经结束，因此不允许重新 activate，也不允许被 redirect flush。
  fault 终态是 success=0 && terminal_done=1，只检查 success 会漏掉这类终态，所以保护条件必须改为 terminal_done。
```

### 7.4 terminal_done 非法组合检查

非法组合：

```text
terminal_done=1 && active=1
terminal_done=1 && replay_pending=1
terminal_done=1 && redirect_pending=1
terminal_done=1 && flushed=1
terminal_done=1 && enq=1
terminal_done=1 && issue_ready=1
```

需要同步的保护点：

1. `activate_uid(uid)`：如果 `status.terminal_done=1`，必须 fatal。
2. `prepare_uid_for_redirect_reissue(uid, redirect)`：如果 `status.terminal_done=1`，必须 fatal。
3. `apply_redirect_flush_range()`：跳过/保护条件从 `success` 改为 `terminal_done`。
4. `end_test_check()`：发现终态 uid 仍有中间态字段，必须报错。

文字伪代码：

```text
activate_uid：
  如果 uid 已 terminal_done：
    fatal，禁止重新激活终态 uid。

prepare_uid_for_redirect_reissue：
  如果 uid 已 terminal_done：
    fatal，禁止 redirect flush 已终态 uid。
```

## 8. final checker 和 testcase 策略

### 8.1 `end_test_check()`

新增每个 uid 的 terminal_done 检查：

```systemverilog
if (!status_by_uid[uid].terminal_done) begin
    `uvm_error("COMMON_DATA", $sformatf("uid=%0d is not terminal_done at end_test_check", uid))
end
```

保留运行期状态清理检查：

```systemverilog
if (status_by_uid[uid].active ||
    status_by_uid[uid].exception_pending ||
    status_by_uid[uid].replay_pending ||
    status_by_uid[uid].redirect_pending) begin
    `uvm_error("COMMON_DATA", $sformatf("uid=%0d has unfinished status at end_test_check", uid))
end
```

文字伪代码：

```text
end_test_check：
  对每个 uid 检查 terminal_done。
  如果 terminal_done=0：
    报错，说明 testcase 收尾时仍有 uid 未进入终态。
  如果 active/exception_pending/replay_pending/redirect_pending 仍为 1：
    报错，说明终态清理不完整。
  success=0 不直接等同 unfinished。
```

### 8.2 real smoke 默认允许 fault 通过

用户已确认：real smoke 默认允许 unexpected fault 作为通过结果。

确认后的策略：

- real smoke final checker 不因为 `success=0 && fault=1` 报错。
- real smoke final checker 仍必须检查 `terminal_done=1`、`active=0`、`exception_pending=0`、`replay_pending=0`、`redirect_pending=0` 等运行期状态已清理。
- real smoke 不要求所有 uid `success=1`。

文字伪代码：

```text
real smoke final checker：
  对每个 uid：
    如果 terminal_done=0：
      报错。
    如果 active/replay_pending/redirect_pending/exception_pending 未清理：
      报错。
    如果 success=0 且 fault=1：
      不报错，因为 real smoke 默认允许 fault 终态通过。
```

### 8.3 最小 fault directed testcase

本次 coding 必须新增最小 fault directed testcase，用于证明 fault 不会卡住 completion。

验收要求：

```text
构造至少一个 fault uid。
该 uid 进入 fault/exception 状态后，仍能在 commit frontier 被消费为终态。
fault retire 后：
  success = 0
  terminal_done = 1
  exception_pending = 0
  active = 0
terminal_done_uid 能越过该 uid。
global_stop_requested 最终能置位。
final checker 按 terminal_done 完成语义判定通过。
```

说明：第一版不要求新增 expected fault 白名单机制。最小 directed testcase 优先构造 LOAD fault 或 STA fault；STD fault 不作为必选。

## 9. STD target fault 处理结论

当前测试框架对 `STD target fault` 有状态模型和 handler 支持，但常规 DUT 采集路径里 STD fault 不应作为主要期望来源。

### 9.1 状态模型支持 STD fault

```systemverilog
function memblock_status_field_e target_fault_field(input memblock_issue_target_e target);
    case (target)
        MEMBLOCK_ISSUE_TARGET_LOAD: return MEMBLOCK_STATUS_LOAD_FAULT;
        MEMBLOCK_ISSUE_TARGET_STA:  return MEMBLOCK_STATUS_STA_FAULT;
        MEMBLOCK_ISSUE_TARGET_STD:  return MEMBLOCK_STATUS_STD_FAULT;
        default: begin
            `uvm_fatal("COMMON_DATA", $sformatf("target_fault_field got target=%0d", target))
        end
    endcase
    return MEMBLOCK_STATUS_FAULT;
endfunction:target_fault_field
```

文字伪代码：

```text
target_fault_field：
  LOAD fault 映射到 load_fault。
  STA fault 映射到 sta_fault。
  STD fault 映射到 std_fault。
```

### 9.2 raw int writeback 理论支持 STD fault

`dispatch_monitor_event_adapter::convert_raw_int_wb()` 会把 port 5/6 映射成 STD target，并携带 `exception_vec`：

```systemverilog
case (raw.port_id)
    5, 6: begin
        wb_event.source = MEMBLOCK_WB_EVENT_SOURCE_STORE_WB;
        wb_event.target = MEMBLOCK_ISSUE_TARGET_STD;
        wb_event.has_lq = 1'b0;
        wb_event.has_sq = raw_sq_to_key(raw.sq_valid, raw.sq_flag, raw.sq_value, wb_event.sq_key);
    end
endcase
wb_event.has_exception = raw.exception_vec != '0;
wb_event.exception_vec = raw.exception_vec;
```

文字伪代码：

```text
convert_raw_int_wb：
  如果 raw int writeback 来自 port 5/6：
    设置 wb_event.target=STD。
    如果 exception_vec 非 0：
      后续 handler 可按 STD fault 处理。
```

### 9.3 常规 STD IQ feedback 不产生 STD fault/replay

STD IQ feedback miss 当前被显式丢弃：

```systemverilog
if (raw.is_std && !raw.hit) begin
    `uvm_warning("DISP_MON_ADAPT",
                 $sformatf("drop STD iq feedback miss port=%0d: MemBlock has no backend STD replay feedback path",
                           raw.port_id))
    return 1'b0;
end
```

文字伪代码：

```text
convert_raw_iq_feedback：
  如果 IQ feedback 表示 STD 且 hit=0：
    打印 warning 并丢弃。
  当前框架认为 MemBlock 没有 backend STD replay feedback path。
```

方案约束：

- 代码实现必须保留 `std_fault` 对方案 A 的兼容处理。
- 第一版最小 fault directed testcase 不强制构造 STD fault，优先选择 LOAD fault 或 STA fault。
- 后续如果要专项验证 STD fault，需要先确认 DUT 是否会在 STD real writeback 端口输出非零 `exception_vec`，以及该场景是否符合真实 store data path 异常语义。

## 10. 日志、dump 和文档同步

### 10.1 日志和 debug dump

需要同步 `terminal_done` 的日志：

1. no-progress 日志：`success_prefix` 改成 `terminal_done_uid`。
2. `retire_active_uid()`：日志打印 `success` 时同步打印 `terminal_done`。
3. `try_retire_committed_uid()`：日志打印 retire 结果时同步打印 `terminal_done`、`fault`、`exception_pending`。
4. `report_unfinished_status()` 或等价未完成 dump：判断 unfinished 时不能只看 `success=0`。

文字伪代码：

```text
report_unfinished_status：
  如果 terminal_done=1：
    如果 success=0：
      打印该 uid 是非成功终态。
    return。
  如果 terminal_done=0：
    打印 active/enq/replay/redirect/flushed/rob_commit/lsq_deq 等未完成状态。
```

### 10.2 文档同步范围

完成 coding 后必须同步以下文档：

- `AI_DOC/mem_ut_flow_doc/load_sta_std_issue_flow.md`
- `AI_DOC/mem_ut_flow_doc/rob_commit_lq_sq_deq_flow.md`
- `AI_DOC/mem_ut_flow_doc/redirect_flow.md`
- `AI_DOC/mem_ut_flow_doc/fault_exception_flow.md`
- `AI_DOC/mem_ut_flow_doc/main_table_build_and_stimulus_flow.md`
- `AI_DOC/mem_ut_flow_doc/writeback_function_call_flow.md`
- `AI_DOC/project_management/mem_ut_test_framework_logic_build_rule.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/*.md`
- `AI_DOC/web/**/assets/*.js`
- `AI_DOC/web/**/*.html`
- 任何仍出现 `success_prefix_uid`、`advance_success_prefix`、`all_transactions_success` 或“success 前缀”的当前有效文档。

历史快照文档如果保留旧逻辑，不强制改正文，但必须在文档开头或相关章节加注记：

```text
该文档描述 terminal_done 重构前的 success_prefix 旧逻辑，当前实现以 terminal_done_prefix_refactor_plan_20260627.md 为准。
```

## 11. 验收检查

### 11.1 静态检查

源码中不应再存在旧完成前缀命名：

```bash
rg -n "success_prefix_uid|advance_success_prefix|success_prefix" mem_ut/ver/ut/memblock/seq
rg -n "all_transactions_success" mem_ut/ver/ut/memblock/seq AI_DOC
```

文档中如果存在旧名，必须逐项归类为“已更新”“历史注记保留”或“待新增文档”。

### 11.2 编译仿真

基础验证：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
make eda_run tc=tc_sanity mode=base_fun
```

dispatch real smoke 验证：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_run tc=tc_dispatch_real_smoke mode=base_fun cfg=tc_dispatch_real_smoke
```

fault directed smoke 验证：

```text
运行新增最小 fault directed testcase。
确认 fault uid retire 后 terminal_done=1、success=0。
确认 terminal_done_uid 能越过 fault uid。
确认 global_stop_requested 最终置位。
确认 UVM_ERROR/UVM_FATAL 为 0。
```

建议补充验证矩阵：

| 场景 | 目的 | 验收点 |
|---|---|---|
| `tc_sanity` | 基础编译和无主表 flow 兼容 | 不因字段改名破坏默认 testcase。 |
| `tc_dispatch_real_smoke` | 真实 dispatch 主流程退出 | `global_stop_requested` 基于 `terminal_done_uid` 置位。 |
| 最小 fault directed smoke | fault 非成功终态 | fault retire 后 `success=0 && terminal_done=1`，`terminal_done_uid` 能越过 fault uid。 |
| replay smoke | 中间态保护 | replay 不误置 terminal_done，重发后最终推进。 |
| store/mixed smoke | STA/STD 组合 | store/mixed flow 不因 success 语义变化提前退出或卡住。 |

## 12. 风险和约束

1. `success` 不再驱动 completion。任何依赖 `status.success` 判断“是否完成”的逻辑都必须重新审查。
2. `terminal_done` 只能在 uid 真正终态时置位。redirect/replay/flushed 这种中间态不能置位。
3. fault retire 后必须清 `exception_pending`，否则 end check 会继续认为该 uid 未完成。
4. commit candidate 必须允许 fault terminal candidate，否则 fault uid 没有 commit retire 入口。
5. commit batch 不能越过 fault frontier，否则会破坏 ROB 顺序提交语义。
6. commit cursor 必须基于 `terminal_done` 跳过，否则 fault uid 仍会卡住 cursor。
7. 文档和日志必须同步改名，否则 debug 会继续把 terminal_done 边界误解为 success 边界。
8. `success=0 && terminal_done=1` 是合法组合，表示 fault/exception 等非正常成功终态。
9. real smoke 默认允许 fault 终态通过，但仍必须检查运行期状态清理完整。
10. 第一版不实现 expected fault 白名单机制；如后续需要区分 expected/unexpected fault，应另起专项 plan。
