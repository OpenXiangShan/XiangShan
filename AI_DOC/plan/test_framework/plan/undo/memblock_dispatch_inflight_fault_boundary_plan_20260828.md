# MemBlock fault 后 inflight issue candidate 记账方案（2026-08-28）

| 项目 | 内容 |
|---|---|
| 状态 | 可执行，等待本轮 coding 与远端验证 |
| 适用版本 | V2，分支 `mem_ut_uvm_v2` |
| 关联分析 | `AI_DOC/analysis/framework_design/memblock_rm_std_inflight_fault_issue_analysis_20260828.md` |
| 目标场景 | Sv39、U 态、`MEMBLOCK_MAIN_TRANS_NUM=10000` 的 real-dispatch 回归 |
| 失败根因 | fault 设置 `exception_pending` 后，已冻结且真实 fire 的 STD candidate 被普通资格检查拒绝，缺少 issue snapshot |
| 修改范围 | issue item 类型、issue scheduler、real dispatch issue sequence；不修改 RTL、RM、DUT interface 或 cfg |

## 1. 专有名词与抽象功能说明

| 英文术语 | 当前中文含义 | 对应代码对象 | 使用场景/示例 |
|---|---|---|---|
| `candidate` | scheduler 选中并冻结到当前 xaction 的 issue 候选 | `memblock_issue_q_item_t`、`fired_items` | UID84 STD、ROB `1/37`、SQ `1/39` |
| `inflight` | candidate 已送给 driver/DUT，但 framework 尚未完成 dispatched 记账 | `send_issue_cycle()` 与 driver `item_done()` 之间 | fault 发生在 candidate 送出之后 |
| `fire` | issue 端口同一采样边界满足 `valid && ready` | `record_dispatch_issue_fire()`、`fired_mask` | driver 置位一个 port bit |
| `issue snapshot` | 真实 fire 对应的 issue epoch、实例 flush epoch 和 dispatched 状态 | `mark_issue_snapshot()`、`status.*_issue_epoch` | adapter 归属 raw writeback 的前提 |
| `dynamic epoch` | 同一 UID 的 redirect/reissue 动态实例编号 | `status.dynamic_epoch`、新增 item 字段 | 旧 candidate 不得记到新实例 |
| `fault pending` | target fault 已落表但 UID 仍 active、尚未 fault retire | `status.fault`、`status.exception_pending` | STA fault 后 STD 仍可能已在 DUT 通路中 |
| `value-only` | raw writeback 只有 ROB value，没有 flag | `resolve_std_uid_by_rob_value_only()` | V2 STD raw 需 probe 两个 flag |
| `stale` | candidate 已被 redirect/flush/kill 或动态实例不再匹配 | scheduler eligibility 与 status flags | 只能 warning，不能建立 snapshot |

抽象功能说明：本 plan 为“fault 发生在 issue candidate 冻结与真实 fire 之间”的窗口增加一个受限记账分支。普通新 candidate 仍受 `exception_pending` 阻塞；只有当前 xaction 明确冻结、driver 明确报告真实 fire、UID 动态身份仍匹配且未被 recovery 淘汰时，才建立一次 issue snapshot。

## 2. 问题与目标 Flow

### 2.1 当前错误 Flow

```text
选择 candidate
  -> driver/DUT 通路已经出现 valid
  -> fault handler 设置 exception_pending
  -> driver 报告真实 fire
  -> 普通 is_issue_item_state_eligible() 拒绝 candidate
  -> std_dispatched/std_issue_epoch 保持 0
  -> value-only writeback 无法形成合法 owner
```

### 2.2 目标 Flow

```text
选择 candidate 并冻结 dynamic_epoch
  -> driver 报告真实 fire，持久 fired mask 置位
  -> sequence 先走原有 fire helper
  -> 若仅因 fault_pending 失败，校验冻结 descriptor/动态身份/queued 状态
  -> frozen-candidate helper 分配唯一 issue epoch 并建立 snapshot
  -> adapter 归属同 UID raw writeback
  -> fault 状态仍阻止正常 pass，最终按 fault 生命周期 retire
```

## 3. 设计边界与不变量

1. `record_dispatch_issue_fire()` 仍是唯一真实 fire 判据；driver 不直接写 `common_data_transaction`。
2. `is_issue_item_state_eligible()` 的正常仲裁语义不改变；新 helper 只由真实 fired candidate 的 fallback 调用。
3. `dynamic_epoch` 必须随 candidate 冻结，并与当前 status 精确比较；redirect/reissue 后旧 candidate 永远不能建立新 snapshot。
4. 只允许 active/enq/issue_ready、未 flush/redirect/kill/terminal 的 UID；任何 recovery 淘汰路径都返回 stale。
5. target 必须仍处于 queued、未 dispatched、未 writeback/pass/fault；不重复分配 issue epoch。
6. 新 helper 只在 `fault && exception_pending` 时放宽一项检查；不把任意异常、replay 或 global flush 当作可接受条件。
7. 高速路径只访问当前 candidate 和对应 status 字段；不扫描 `main_trans_num` 全表，不新增全局 raw owner map。
8. `mark_target_normal_pass()` 的 fault guard 保持不变，fault UID 不会因 STD 记账而变成正常成功。

## 4. 数据结构修改

### 4.1 `memblock_issue_q_item_t.dynamic_epoch`

抽象功能描述：该字段保存 candidate 创建时的动态实例编号，使 fire 记账能区分同一 UID 的旧实例和 redirect/reissue 后的新实例。它不是 issue epoch，也不随每次 target fire 递增。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv`，`memblock_issue_q_item_t`。

```systemverilog
typedef struct {
    memblock_uid_t            uid;
    memblock_rob_key_t        rob_key;
    memblock_issue_target_e   target;
    int unsigned              send_pri;
    longint unsigned          ready_cycle;
    int unsigned              replay_seq;
    int unsigned              dynamic_epoch;
    bit                       has_lqIdx;
    memblock_lq_key_t         lq_key;
    bit                       has_sqIdx;
    memblock_sq_key_t         sq_key;
    memblock_num_ls_elem_t    numLsElem;
    int unsigned              uop_index;
    int unsigned              uop_count;
} memblock_issue_q_item_t;
```

中文伪代码：

```text
在 issue queue item 中增加一个无符号实例编号字段。
该编号由 make_issue_item 从当前 UID status 快照，普通 queue copy、xaction candidate copy 和 fired_items copy 都保留它。
它只用于后续 identity 校验；不直接表示“已 fire”，也不替代 issue_epoch。
```

## 5. Scheduler 实现

### 5.1 `make_empty_item()` 与 `make_issue_item()`

抽象功能描述：`make_empty_item()` 建立完整的零值 item，保证新增字段有确定 reset/default；`make_issue_item()` 将当前 active UID 的状态和 LSQ key 冻结为一个可驱动 candidate，并保存动态实例编号。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/issue_queue_scheduler.sv`，`make_empty_item()`、`make_issue_item()`。

```systemverilog
item.replay_seq  = 0;
item.has_lqIdx   = 1'b0;
item.lq_key      = '{default:'0};
item.has_sqIdx   = 1'b0;
item.sq_key      = '{default:'0};
item.numLsElem   = '0;
item.uop_index   = 0;
item.uop_count   = 0;
...
item.replay_seq  = status.replay_seq;
item.has_lqIdx   = status.active_lq_mapped;
item.lq_key.flag = status.lqIdx_flag;
item.lq_key.value = status.lqIdx_value;
item.has_sqIdx   = status.active_sq_mapped;
item.sq_key.flag = status.sqIdx_flag;
item.sq_key.value = status.sqIdx_value;
```

中文伪代码：

```text
创建空 item 时把 dynamic_epoch 置零，避免未初始化字段进入比较。
从 active status 构造真实 candidate 时，在复制 replay_seq 和 LQ/SQ 映射的同一位置复制 status.dynamic_epoch。
后续 candidate 的 ROB、LSQ key、replay_seq、dynamic_epoch 共同描述一次动态实例；若 status 在 redirect 后改变，比较会失败。
```

### 5.2 `is_frozen_issue_item_fire_acceptable()`

抽象功能描述：该函数只判断一个已被当前 xaction 冻结、并已由 driver 报告 fire 的 candidate 是否仍可补建 issue snapshot。它不参与新候选选择，不修改任何状态；返回 1 仅表示可以进入下一 helper。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/issue_queue_scheduler.sv`，新增 `is_frozen_issue_item_fire_acceptable()`。

计划逻辑：

```systemverilog
function bit is_frozen_issue_item_fire_acceptable(input memblock_issue_q_item_t item);
    status_transaction       status;
    main_control_transaction main_tr;

    ensure_data();
    if (data.issue_blocked_by_global_flush() ||
        item.target == MEMBLOCK_ISSUE_TARGET_NONE ||
        !data.is_valid_uid(item.uid)) begin
        return 1'b0;
    end
    status = data.get_status(item.uid);
    if (!status.active || !status.enq || !status.issue_ready ||
        status.terminal_done || status.flushed || status.redirect_pending ||
        status.issue_killed || !status.fault || !status.exception_pending ||
        status.dynamic_epoch != item.dynamic_epoch || item.ready_cycle != 0 ||
        !data.target_replay_seq_match(status, item.target, item.replay_seq)) begin
        return 1'b0;
    end
    main_tr = data.get_main_transaction(item.uid);
    if (main_tr.get_rob_key() != item.rob_key || status.get_rob_key() != item.rob_key) begin
        return 1'b0;
    end
    case (item.target)
        MEMBLOCK_ISSUE_TARGET_LOAD: begin
            if (!status.queued_load || status.load_dispatched || status.load_writeback ||
                status.load_pass || status.load_fault || !status.active_lq_mapped ||
                item.has_lqIdx != status.active_lq_mapped || item.lq_key.flag != status.lqIdx_flag ||
                item.lq_key.value != status.lqIdx_value) begin
                return 1'b0;
            end
        end
        MEMBLOCK_ISSUE_TARGET_STA: begin
            if (!status.queued_sta || status.sta_dispatched || status.sta_writeback ||
                status.sta_pass || status.sta_fault || !status.active_sq_mapped ||
                item.has_sqIdx != status.active_sq_mapped || item.sq_key.flag != status.sqIdx_flag ||
                item.sq_key.value != status.sqIdx_value) begin
                return 1'b0;
            end
        end
        MEMBLOCK_ISSUE_TARGET_STD: begin
            if (!status.queued_std || status.std_dispatched || status.std_writeback ||
                status.std_pass || status.std_fault || !status.active_sq_mapped ||
                item.has_sqIdx != status.active_sq_mapped || item.sq_key.flag != status.sqIdx_flag ||
                item.sq_key.value != status.sqIdx_value) begin
                return 1'b0;
            end
        end
        default: return 1'b0;
    endcase
    return 1'b1;
endfunction:is_frozen_issue_item_fire_acceptable
```

中文伪代码：

```text
该逻辑承担“fault 后仍在飞行的 candidate 身份确认”，不承担普通 issue 仲裁。
先确认全局没有 flush、target 合法且 UID 仍存在；否则立即返回不可接受。
读取 UID status，要求 active/enq/issue_ready 且未 terminal、flush、redirect、kill；同时要求 fault 和 exception_pending 都为真，确保放宽只服务于 fault-inflight 场景。
比较 item.dynamic_epoch 与 status.dynamic_epoch，并要求 ready_cycle 已到期、target replay 序列仍匹配；动态实例或 replay 不匹配时返回 stale。
读取主表 ROB key，要求主表、status 和 item 三者一致。
按 target 检查对应 queued 位、dispatched/writeback/pass/fault 位、LQ/SQ mapping 和 key；任何 target 已完成或 mapping 改变都拒绝。
所有检查通过才返回 1；函数不分配 epoch、不出队、不写状态。
```

### 5.3 `mark_issue_fire_from_frozen_candidate()`

抽象功能描述：该函数将已通过身份检查的 frozen candidate 记为一次真实 issue fire。它是普通 `mark_issue_fire()` 的 fault-inflight 专用补充，不改变 candidate 的输入 payload，也不处理 RM writeback。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/issue_queue_scheduler.sv`，新增 `mark_issue_fire_from_frozen_candidate()`。

计划逻辑：

```systemverilog
function bit mark_issue_fire_from_frozen_candidate(input memblock_issue_q_item_t item);
    int unsigned issue_epoch;

    ensure_data();
    if (!is_frozen_issue_item_fire_acceptable(item)) begin
        return 1'b0;
    end
    issue_epoch = data.alloc_issue_epoch();
    data.mark_issue_snapshot(item.uid, item.target, issue_epoch);
    data.delete_issue_queue_entry(item.target, item.uid, item.replay_seq, 1'b1);
    set_target_queued(item.uid, item.target, 1'b0);
    set_target_dispatched(item.uid, item.target, 1'b1);
    data.clear_replay_target_after_fire(item.uid, item.target);
    return 1'b1;
endfunction:mark_issue_fire_from_frozen_candidate
```

中文伪代码：

```text
该逻辑承担一次且仅一次的 fault-inflight fire 状态转换。
先调用 frozen candidate 资格函数；若身份、动态 epoch、mapping 或 fault 条件不满足，返回失败且不改任何状态。
通过后从公共 data 分配单调 issue_epoch，并用 mark_issue_snapshot 写入 target issue epoch 和当前实例 flush epoch，同时重新开放 issue_killed 语义。
按 UID、target、candidate replay_seq 删除 issue queue entry，清除 queued 标志，置对应 target dispatched 标志，并清理已完成的 replay target。
返回成功后，adapter 可以用 ROB/SQ active map 找到该 UID；fault/exception_pending 本身保持不变，后续 pass guard 继续阻止正常成功。
```

## 6. Sequence 调用修改

### 6.1 `mark_fired_items()`

抽象功能描述：该 helper 消费当前 xaction 的 fired mask，把每个真实 fire port 映射回冻结 candidate，并保证 watcher 与 `finish_item()` 补偿最多记账一次。它不扫描主表，也不自行推导 UID。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_issue_dispatch_base_sequence.sv`，`mark_fired_items()`。

计划逻辑：

```systemverilog
if (data.issue_blocked_by_global_flush()) begin
    fire_marked = issue_sched.mark_issue_fire_already_accepted(fired_items[idx]);
end else begin
    fire_marked = issue_sched.mark_issue_fire(fired_items[idx]);
end
if (!fire_marked) begin
    fire_marked = issue_sched.mark_issue_fire_from_frozen_candidate(fired_items[idx]);
end
if (!fire_marked) begin
    `uvm_warning(get_type_name(), $sformatf("skip stale issue item uid=%0d target=%0d", ...))
end
bookkept_fired_mask[port_idx] = 1'b1;
```

中文伪代码：

```text
遍历当前 fired_items，先跳过 mask 未置位或已经 bookkept 的 port。
若全局 flush 已阻塞，先沿用 already_accepted helper；否则沿用普通 mark_issue_fire，保持原有正常路径。
原有 helper 失败时调用 frozen-candidate helper；该 fallback 只有真实 fired mask 驱动，且内部会拒绝 flush、redirect、动态 epoch 不匹配和非 fault 状态。
两个 helper 都失败时保留一次 stale warning，表示真实 fire 与当前软件状态不一致，不能静默吸收未知 output。
无论成功或 stale，给该 port 设置 bookkept 位，防止 finish 后补偿再次分配 issue epoch；下一轮 queue 是否重新出现由 recovery/status 流程决定。
```

### 6.2 `send_issue_cycle()` watcher 边界

抽象功能描述：`send_issue_cycle()` 负责冻结 candidate、启动持久 fired-mask watcher、等待 driver 完成，并在 finish 后做幂等补偿。已有 watcher 机制保持不变，本 plan 只把 fallback 记账纳入 watcher 调用链。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_issue_dispatch_base_sequence.sv`，`send_issue_cycle()`。

中文伪代码：

```text
选择 load/STA/STD candidate 后复制 dynamic_epoch，计算 candidate_mask 并清空本 xaction 的 fired mask 与 bookkept mask。
在 start_item 后启动持久条件 watcher；watcher 发现 candidate 内新增 fired bit 时调用 mark_fired_items，因此 driver 先置位还是 watcher 先等待都不会丢 fire。
finish_item 返回后读取最终 fired mask，只对未 bookkept 的位做一次补偿，然后停止 watcher。
若 driver 返回 candidate 外的 fired bit，保持既有 fatal；blocking item 未完成且没有 redirect/flush，也保持既有 fatal。
真实 fire 已被 fallback 记账时，has_fire 置位，未 fire candidate 不出队，正常 queue/recovery 生命周期继续负责后续处理。
```

## 7. 不变部分与失败策略

| 场景 | 处理 |
|---|---|
| 普通 candidate、无 fault | 继续使用 `mark_issue_fire()`，资格规则不变 |
| fault 后 frozen candidate 真实 fire | 通过新 helper 建立 snapshot |
| dynamic epoch 不匹配 | 一次 stale warning，不建立新 snapshot |
| UID 已 flush/redirect/kill/terminal | 拒绝；不能把旧 output 绑定到新实例 |
| target 已 writeback/pass/fault | 拒绝重复 fire 记账 |
| global flush 期间 | 先走既有 already-accepted 路径；新 helper 默认不绕过 global flush |
| fired mask 含未知 port | 保持既有 `uvm_fatal` |
| adapter 仍遇到零候选 | 保留严格 `INT_WB_STD_KEY`，继续收集日志/波形，不静默 drop |

## 8. 验证计划

1. 静态检查：`git diff --check`；`rg` 检查 `dynamic_epoch` 初始化、复制和比较点；确认没有 RTL/Scala 文件变更。
2. 编译：从 `mem_ut/ver/ut/memblock/sim` 运行独立 mode 的 `eda_compile`。
3. 固定 seed 回归：`wave=on`、`cfg=tc_dispatch_real_mmu_sv39_smoke`，使用 real-dispatch vseq。
4. 结果验收：目标完成 10000 笔，日志包含 `TEST CASE PASSED`，`UVM_ERROR=0`、`UVM_FATAL=0`，且不再出现 `INT_WB_STD_KEY`。
5. 失败闭环：新的 RM/framework 报错必须创建独立分析文档和对应 plan，按证据修改后重新编译/仿真；只有满足分析文档规定的 RTL 升级条件才启动 subagent 复核。

## 9. 文档、review 与提交

- coding 前保留本 plan 在 `undo`，coding 中若发现必要偏差，追加 `IMPLEMENTATION_DELTA` 章节，不覆盖原方案。
- coding 和验证完成后生成 `AI_DOC/plan/test_framework/review_doc/undo/memblock_dispatch_inflight_fault_boundary_implementation_review_20260828.md`，逐项覆盖 struct、scheduler、sequence diff、plan 对齐和额外细节。
- review 无 blocker、文档同步完成、仿真达标后，才将 plan 移到 `AI_DOC/plan/test_framework/plan/do/`。
- 只 stage 本轮相关文件并单独本地 commit；不提交用户已有无关修改，不 push。

## 10. RM 协同支持

本 plan 不实现 RM/checker/scoreboard。RM 继续从冻结 TLB/PMA/PMP context、sticky DCache ledger 和当前 issue snapshot 获取所需事实；本 plan 只保证 snapshot 在真实 fire 后存在。

## 11. 功能覆盖率协同支持

本 plan 不实现 coveragent/covergroup。后续 coverage 可以统计 `fault_pending`、`dynamic_epoch`、target 和 frozen-candidate fire 的交叉，但不作为本 plan 的实现内容。
