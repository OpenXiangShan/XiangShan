# V2 Stranded SQ Cancel Owner Implementation Review

评审日期：2026-10-12

关联 plan：`AI_DOC/plan/test_framework/plan/undo/memblock_v2_stranded_sq_cancel_owner_coding_plan_20261012.md`

关联独立 plan review：`AI_DOC/plan/test_framework/review_doc/undo/memblock_v2_stranded_sq_cancel_owner_coding_plan_review_20261012.md`

评审结论：approved SQ 行为实现无 blocker，record 3896 的 SQ cancel mismatch 已由有效真实 seed 证明修复；
完整 seed 随后暴露新的 LQ owner 生命周期 blocker，因此当前 plan 保持在 `undo`。这个归档 blocker 不等于
SQ 实现 blocker。

## 1. 术语与抽象功能说明

### 1.1 术语表

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
| --- | --- | --- | --- |
| `admission prefix` | 从 UID 0 开始连续完成 LSQ enqueue/admission 的软件前缀，其末端可以被 redirect 回退。 | `dispatch_progress.max_enqueued_uid` | record 3895 回退前缀后，uid 4116 仍持有 SQ owner，但已落在前缀外。 |
| `active map` | 用完整 ROB/LQ/SQ key 反查当前动态实例 UID 的权威活跃映射。 | `uid_by_active_rob`、`uid_by_lq`、`uid_by_sq` | `{sq flag=1,value=35}` 映射到 uid 4116。 |
| `owner` | 当前对某个物理 ROB/LQ/SQ key 负责的动态 UID 实例。 | active map 和 `status.active_*_mapped` | redirect 删除 owner 时必须同步计数、保存迟到事件身份并清 map。 |
| `snapshot` | redirect 删除任何 owner 前复制出的局部值队列，不拥有长期生命周期。 | `memblock_active_sq_owner_snapshot_t` | 保存 uid 4116 的 UID、dynamic epoch、完整 ROB/SQ key。 |
| `dynamic epoch` | 同一 UID 每次 redirect reissue 后递增的动态版本号。 | `status.dynamic_epoch` | 快照版本和处理时版本不同，说明 owner 已不是同一实例，必须 fatal。 |
| `redirect epoch` | 每次全局 redirect 的软件编号，也是本次 cancel 记账版本。 | `cancel_record.redirect_epoch`、`dispatch_flush_epoch` | `lsq_cancel_accounted_epoch` 等于当前 redirect epoch 时禁止重复记账。 |
| `reservation` | 测试框架对一次 LQ/SQ 分配动态实例的采样生命周期记录，不等于最终 writeback。 | `lsq_reservation_state`、`lsq_reservation_sample_seq` | launch 后先 pending；跨过 DUT sample 后成为 `DUT_VISIBLE`。 |
| `DUT_VISIBLE` | reservation 已跨过 DUT 接收边界，DUT 的物理 LSQ cancel 扫描可能观察到该 owner。 | `MEMBLOCK_LSQ_RESERVATION_DUT_VISIBLE` | uid 4116 sample=195546，早于 record 3896 cutoff=195570。 |
| `cutoff snapshot` | redirect 已作用到 LSQ 的水位样本，只用于允许软件开始 owner scan 和推进 drained watermark。 | `redirect_lsq_sample_seq` | 专项测试在 cutoff 样本写 held cancel level，不拿它做最终对账。 |
| `compare snapshot` | DUT cancel 输出应被精确比较的目标样本。 | `compare_snapshot_sample_seq` | reconcile 只消费 sample 等于 compare target 的 snapshot。 |
| `full key` | 同时包含 wrap flag 与 value 的 ROB/LQ/SQ 物理身份。 | `memblock_rob_key_t`、`memblock_sq_key_t` | 只比较 value 会在 wrap 后把不同物理代际误认为同一 owner。 |
| `stranded owner` | 前一轮 redirect 回退 admission prefix 后仍 active/mapped，下一轮又被覆盖但原 prefix scan 已访问不到的 owner。 | 本轮 SQ snapshot 补扫 | uid 4116 在 record 3896 被补记为 ordinary SQ cancel。 |
| `fault candidate` | fault ROB head 保留的独立 SQ 归因资源，不属于 ordinary owner 补扫。 | `fault_redirect_resource` | record 3896 ordinary=9 后，fault uid 3956 再贡献 1。 |
| `fail-fast` | 状态无法证明安全时立即以指定 UVM fatal 终止，不猜测、不吞掉错误。 | `LSQ_CANCEL_STRANDED_OWNER` | control、commit、terminal、fault、identity mismatch 均 fatal。 |
| `provenance ring` | 只保存最近有限条 stranded-owner 归因的调试环形队列。 | `sq_cancel_provenance_history_q` | 深度固定为 128，正常 owner 高频路径不写入。 |
| `expected fatal catcher` | 专项测试临时捕获指定 fatal 并计数的 UVM callback。 | `memblock_stranded_sq_cancel_expected_fatal_catcher` | 8 个 matcher 负向场景被捕获后，最终 UVM_FATAL 统计仍为 0。 |
| `soft sequence` | 不依赖新增 testcase、通过公共模型 API 构造软件状态和时序的专项 sequence。 | `soft_test_memblock_stranded_sq_cancel_sequence` | 构造真实两轮 redirect 与 matcher 边界。 |
| `vseq` | `basicTest` 通过 `+VSEQ_MAIN` 启动的 virtual sequence 入口。 | `memblock_stranded_sq_cancel_directed_vseq` | 只负责在 virtual sequencer 上启动 soft sequence。 |
| `complete-seed blocker` | 原问题通过后，真实 seed 继续执行才暴露的后续独立阻断。 | `uid_by_lq` 冲突日志 | `uid=4033` 申请 LQ key 时，existing owner 为 uid 4118。 |

### 1.2 本轮功能抽象边界

本轮只扩展 redirect 中频路径的 ordinary scalar SQ owner 收集范围：保留原 admission-prefix scan，并在
redirect 删除 owner 前冻结有限 `uid_by_sq`，随后补处理能被完整 identity、epoch、ROB/SQ key、reservation
和 cutoff 证明的 prefix 外 owner。它不改变 DUT cancel 接口，不放宽 reconcile，不把 fault candidate 合入
ordinary count，也不扩展到 LQ owner 修复。

## 2. Review 范围、修改前逻辑与结论

### 2.1 本次源码和文档范围

| 文件 | 本轮角色 | Review 覆盖结论 |
| --- | --- | --- |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv` | owner snapshot、matcher、redirect 主流程、通用诊断 | 全部功能 diff 已展开。 |
| `mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_stranded_sq_cancel_sequence.sv` | 两轮 redirect 和负向边界专项测试 | 关键 task、catcher 和未覆盖项已展开。 |
| `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_stranded_sq_cancel_directed_vseq.sv` | `basicTest` 专项入口 | 已展开。 |
| `mem_ut/ver/ut/memblock/seq/seq_pkg.sv` | include 顺序与 factory 可见性 | 已展开。 |
| coding plan、plan review、status、implementation review | 设计、授权、状态与实现复核 | 已对齐检查。 |

### 2.2 修改前逻辑

`apply_redirect_flush_range()` 只扫描 `[terminal_done_uid, max_enqueued_uid+1)`。uid 4116 已经由
`commit_allocate()` 推进过 admission prefix，并成为 `DUT_VISIBLE`；record 3895 回退 prefix 时其 ROB
`0/76` 不被 redirect `1/75` 覆盖，所以 owner 保留但落到新 prefix 外。record 3896 的 redirect `1/76`
覆盖 uid 4116，但原 prefix scan 只到 uid 3981，软件 ordinary SQ count 停在 8；DUT 物理 SQ 扫描得到
8 ordinary + 1 fault + 1 stranded = 10，旧 reconcile 因 delta 为 2 fatal。

### 2.3 修改后逻辑

redirect scan 开始前复制 `uid_by_sq`。原 prefix scan 先按既有顺序处理 fault head、inactive/terminal 和普通
被覆盖 UID；之后 matcher 只选择 prefix 外、同一 dynamic instance、ordinary scalar SQ、cutoff 前已
`DUT_VISIBLE` 且被本 redirect 覆盖的 owner。选中者复用 `prepare_uid_for_redirect_reissue()`，因此 cancel
计数、deleted-owner 记录、active map 删除和 reissue 状态转换仍只有一套真源。

### 2.4 正确性结论

- 不边遍历边删除 `uid_by_sq`；局部 snapshot 隔离了迭代和 owner 生命周期修改。
- prefix 内 owner由原 scan处理，补扫显式排除 `[begin_uid,end_uid)`，不会重复计数。
- full key、dynamic epoch、status/main transaction/map 三方一致后才允许处理。
- 未被 redirect 覆盖或 cutoff 前尚不可见的合法 owner保持 active，不猜测 cancel。
- fault candidate仍在 exact compare snapshot 阶段独立归因；真实 seed 证明 ordinary=9 后 fault=1。
- 补扫 owner位于 admission 水位之后时不会反向推进水位。
- redirect 路径最多扫描 `MEMBLOCK_SQ_SIZE` 个 active SQ owner，不进入每拍 service loop。

## 3. Snapshot 值类型与诊断状态

### 3.1 `memblock_active_sq_owner_snapshot_t`

抽象功能描述：该局部值类型保存 redirect scan 开始时的 SQ 动态实例身份，供原 prefix scan 删除 map 后仍能
验证 prefix 外候选；它不负责 reservation 分类、cancel 计数或长期 owner 生命周期。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv:57-65`，类型：
`memblock_active_sq_owner_snapshot_t`。

```systemverilog
typedef struct {
    bit                valid;
    memblock_uid_t     uid;
    int unsigned       dynamic_epoch;
    memblock_rob_key_t rob_key;
    memblock_sq_key_t  sq_key;
} memblock_active_sq_owner_snapshot_t;
```

中文伪代码：redirect 主流程创建局部 queue 后，每个有效 active SQ owner 写入一条值记录；记录保存 UID、
动态版本和完整 ROB/SQ key。后续 matcher 读取这些值验证 owner 是否仍是同一动态实例；函数返回后 queue
自动销毁，不需要 reset、清理或 end-test 审计。

关键副作用：无公共状态副作用；`valid` 只表示局部记录已填充。

### 3.2 有界 provenance ring

抽象功能描述：`record_sq_cancel_provenance()` 只在成功补记 stranded owner 时保存通用归因，供后续 cancel
对账失败打印最近历史；它不参与 owner 判定和计数。

源码位置：`common_data_transaction.sv:3014-3031`，函数：`record_sq_cancel_provenance()`。

```systemverilog
if (sq_cancel_provenance_history_q.size() >=
    MEMBLOCK_SQ_CANCEL_PROVENANCE_DEPTH) begin
    void'(sq_cancel_provenance_history_q.pop_front());
end
sq_cancel_provenance_history_q.push_back(history_entry);
```

中文伪代码：先把当前 cycle、sample、flush epoch、active record 和事件文本拼成一条记录；若队列已达到
固定深度 128，先删除最旧一条，再把新记录放到队尾。该 ring 只保存有限历史，不随 transaction 数量增长，
也不会改变 cancel record、active map 或 status。

## 4. `snapshot_dut_visible_sq_owners()`

抽象功能描述：该 helper 由 `apply_redirect_flush_range()` 在任何 owner 删除前调用，消费当前有限
`uid_by_sq` active map，输出局部 SQ owner identity snapshot；它只验证 active map 自洽，不负责判断
redirect 是否覆盖、reservation 是否已到 cutoff 或 owner 是否 ordinary。

源码位置：`common_data_transaction.sv:4047-4089`，函数：`snapshot_dut_visible_sq_owners()`。

```systemverilog
snapshots.delete();
foreach (uid_by_sq[map_key]) begin
    uid = uid_by_sq[map_key];
    status = get_status(uid);
    main_tr = get_main_transaction(uid);
    if (!status.active || !status.enq || !status.active_sq_mapped) begin
        `uvm_fatal("LSQ_CANCEL_STRANDED_OWNER",
                   $sformatf("active SQ map lifecycle mismatch uid=%0d active=%0d enq=%0d sq_mapped=%0d",
                             uid, status.active, status.enq,
                             status.active_sq_mapped))
    end
```

中文伪代码：先清空调用者提供的局部 snapshot queue，避免复用上一轮 redirect 内容；再遍历有限
`uid_by_sq`。每个 map entry 反查 UID 的 status 和主 transaction；若权威 SQ map 中的 UID 不是 active、
没有完成 ENQ 或 status 不承认 SQ mapping，立即 fail-fast，因为此时不能安全解释物理 owner。

```systemverilog
status_rob_key = status.get_rob_key();
status_sq_key.flag = status.sqIdx_flag;
status_sq_key.value = status.sqIdx_value;
if (rob_order_util::sq_to_map_key(status_sq_key) != map_key ||
    main_tr.get_rob_key() != status_rob_key ||
    main_tr.get_sq_key() != status_sq_key) begin
    `uvm_fatal("LSQ_CANCEL_STRANDED_OWNER",
               $sformatf("active SQ map identity mismatch uid=%0d rob=%0d/%0d sq=%0d/%0d map_key=%0h",
                         uid, status_rob_key.flag, status_rob_key.value,
                         status_sq_key.flag, status_sq_key.value,
                         map_key))
end
snapshot = '{default:'0};
snapshot.valid = 1'b1;
snapshot.uid = uid;
snapshot.dynamic_epoch = status.dynamic_epoch;
snapshot.rob_key = status_rob_key;
snapshot.sq_key = status_sq_key;
snapshots.push_back(snapshot);
```

中文伪代码：从 status 组装完整 ROB/SQ key；要求 SQ key 转换后的关联数组 key 等于当前迭代 key，并要求
主 transaction 的 ROB/SQ key 与 status 相同。任一身份不一致立即 fatal。通过后清零一条局部值，写入
valid、UID、dynamic epoch 和完整 keys，再放到 snapshot queue 尾部；本函数不删除 map，也不计数。

调用关系：

| 调用者 | 本 helper 输出 | 回到调用者后的影响 |
| --- | --- | --- |
| `apply_redirect_flush_range()` | `active_sq_snapshots[$]` | 原 prefix scan 可以安全删除 `uid_by_sq`；补扫仍持有删除前 identity。 |
| `run_matcher_boundary_scenarios()` | 单 owner snapshot | 负向测试可修改副本验证 epoch/key fail-fast，不直接伪造公共 map。 |

## 5. `redirect_sq_owner_snapshot_matches()`

抽象功能描述：该只读 matcher 在原 prefix scan 完成后，判断一条 snapshot 是否仍代表同一个、需要由本轮
redirect 补处理的 ordinary scalar SQ 动态实例。合法 survivor、prefix 已处理 owner 或 cutoff 后 owner 返回
0；公共状态不自洽、分类非法或重复记账立即 fatal；返回 1 时调用者才允许 reissue。

### 5.1 Prefix 排除与消费状态

源码位置：`common_data_transaction.sv:4093-4121`，函数：
`redirect_sq_owner_snapshot_matches()`。

```systemverilog
if (!snapshot.valid ||
    (snapshot.uid >= begin_uid && snapshot.uid < end_uid)) begin
    return 1'b0;
end
status = get_status(snapshot.uid);
if (!status.active || !status.active_sq_mapped) begin
    if (!status.active && !status.active_sq_mapped) begin
        return 1'b0;
    end
    `uvm_fatal("LSQ_CANCEL_STRANDED_OWNER",
               $sformatf("partially consumed SQ owner uid=%0d active=%0d sq_mapped=%0d",
                         snapshot.uid, status.active,
                         status.active_sq_mapped))
    return 1'b0;
end
```

中文伪代码：无效 snapshot 或 UID 位于原 prefix 范围时直接返回 0，保证补扫不重复处理原循环。随后读取
当前 status；若 active 和 SQ mapping 都已清除，说明原 prefix scan 已消费该 owner，正常返回 0；若只清掉
其中一项，说明 owner 生命周期被部分修改，立即 fatal，禁止把损坏状态当 survivor。

### 5.2 Dynamic identity 与 full-key 一致性

源码位置：`common_data_transaction.sv:4122-4145`，函数：
`redirect_sq_owner_snapshot_matches()` identity 分支。

```systemverilog
main_tr = get_main_transaction(snapshot.uid);
behavior = memblock_op_behavior_util::derive_op_behavior(main_tr);
status_rob_key = status.get_rob_key();
status_sq_key.flag = status.sqIdx_flag;
status_sq_key.value = status.sqIdx_value;
map_key = rob_order_util::sq_to_map_key(snapshot.sq_key);
if (status.dynamic_epoch != snapshot.dynamic_epoch ||
    status_rob_key != snapshot.rob_key ||
    status_sq_key != snapshot.sq_key ||
    main_tr.get_rob_key() != snapshot.rob_key ||
    main_tr.get_sq_key() != snapshot.sq_key ||
    !uid_by_sq.exists(map_key) ||
    uid_by_sq[map_key] != snapshot.uid) begin
    `uvm_fatal("LSQ_CANCEL_STRANDED_OWNER",
               $sformatf("SQ owner identity changed uid=%0d snapshot_epoch=%0d current_epoch=%0d snapshot_rob=%0d/%0d current_rob=%0d/%0d snapshot_sq=%0d/%0d current_sq=%0d/%0d",
                         snapshot.uid, snapshot.dynamic_epoch,
                         status.dynamic_epoch,
                         snapshot.rob_key.flag, snapshot.rob_key.value,
                         status_rob_key.flag, status_rob_key.value,
                         snapshot.sq_key.flag, snapshot.sq_key.value,
                         status_sq_key.flag, status_sq_key.value))
    return 1'b0;
end
```

中文伪代码：读取主 transaction 并推导操作行为；从当前 status 重建完整 keys。然后逐项要求当前 dynamic
epoch、status ROB/SQ key、主 transaction ROB/SQ key、`uid_by_sq` 中的完整 key 和 owner UID 都与 snapshot
一致。任何一项变化都说明快照已不是同一动态实例，立即 fatal；全部一致才进入 ordinary/fault 分类。

### 5.3 Ordinary 分类和重复记账保护

源码位置：`common_data_transaction.sv:4146-4173`，函数：
`redirect_sq_owner_snapshot_matches()` classification 分支。

```systemverilog
if (!status.enq || uid_is_control_marker(snapshot.uid) ||
    !behavior.uses_sq || behavior.uses_lq ||
    main_tr.numLsElem != memblock_num_ls_elem_t'(1)) begin
    `uvm_fatal("LSQ_CANCEL_STRANDED_OWNER",
               $sformatf("prefix-external owner is not an ordinary scalar SQ allocation uid=%0d enq=%0d control=%0d uses_lq/sq=%0d/%0d numLsElem=%0d",
                         snapshot.uid, status.enq,
                         uid_is_control_marker(snapshot.uid),
                         behavior.uses_lq, behavior.uses_sq,
                         main_tr.numLsElem))
    return 1'b0;
end
if (status.terminal_done || status.rob_commit || status.fault ||
    status.exception_pending || status.load_fault ||
    status.sta_fault || status.std_fault) begin
    `uvm_fatal("LSQ_CANCEL_STRANDED_OWNER",
               $sformatf("prefix-external owner has terminal/commit/fault semantics uid=%0d terminal=%0d rob_commit=%0d fault=%0d exception=%0d load/sta/std_fault=%0d/%0d/%0d",
                         snapshot.uid, status.terminal_done,
                         status.rob_commit, status.fault,
                         status.exception_pending, status.load_fault,
                         status.sta_fault, status.std_fault))
    return 1'b0;
end
if (status.lsq_cancel_accounted_epoch == redirect_epoch) begin
    `uvm_fatal("LSQ_CANCEL_STRANDED_OWNER",
               $sformatf("prefix-external owner remains active after current redirect accounting uid=%0d epoch=%0d",
                         snapshot.uid, redirect_epoch))
    return 1'b0;
end
```

中文伪代码：先要求 owner 仍完成 ENQ、不是 control marker、只使用 SQ 不使用 LQ，并且 scalar
`numLsElem` 精确等于 1；否则不能按 ordinary SQ 计数，立即 fatal。再拒绝 terminal、ROB commit、所有 fault
和 exception 语义，确保 fault candidate 仍由独立路径处理。最后检查当前 redirect epoch 是否已记账；已记账
但 owner 仍 active/mapped 表示重复计数或生命周期损坏，必须 fatal。

### 5.4 Redirect 覆盖和 reservation cutoff

源码位置：`common_data_transaction.sv:4174-4182`，函数：
`redirect_sq_owner_snapshot_matches()` selection 分支。

```systemverilog
if (!rob_order_util::rob_need_flush(snapshot.rob_key, redirect)) begin
    return 1'b0;
end
if (status.lsq_reservation_state != MEMBLOCK_LSQ_RESERVATION_DUT_VISIBLE ||
    !status.lsq_reservation_sample_valid ||
    status.lsq_reservation_sample_seq > redirect_lsq_sample_seq) begin
    return 1'b0;
end
return 1'b1;
```

中文伪代码：用完整 ROB key 和 redirect payload 判断 owner 是否被本轮覆盖；未覆盖就是合法 survivor，返回
0 并保持 active。被覆盖后再要求 reservation 已为 `DUT_VISIBLE`、sample 有效且 sample 不晚于本 record 的
LSQ cutoff；不满足时返回 0，不猜测 DUT 是否已拥有 entry。所有证明成立才返回 1，授权主流程补记并 reissue。

## 6. `apply_redirect_flush_range()` 主流程

抽象功能描述：该函数是 redirect 状态清理和软件 cancel 普通 owner 收集的唯一入口。它校验 active cancel
record 和 cutoff，先冻结 SQ owner，再运行原 prefix scan，最后补处理严格匹配的 prefix 外 owner，并启动既有
deleted-owner window；它不负责最终 exact compare snapshot 的 fault attribution。

### 6.1 Record/cutoff 校验和 snapshot 时机

源码位置：`common_data_transaction.sv:4185-4213`，函数：`apply_redirect_flush_range()`。

```systemverilog
record_idx = find_cancel_record_index_by_id(active_cancel_record_id);
if (record_idx < 0 ||
    !redirect_payload_equal(redirect, cancel_record_q[record_idx].redirect) ||
    !cancel_record_q[record_idx].redirect_anchor_valid ||
    memblock_sync_pkg::peek_current_dut_global_sample() <
        cancel_record_q[record_idx].redirect_lsq_sample_seq ||
    latest_drained_cancel_sample_seq <
        cancel_record_q[record_idx].redirect_lsq_sample_seq) begin
    `uvm_fatal("LSQ_CANCEL", "redirect flush scan started before its anchored LSQ sample boundary")
end
snapshot_dut_visible_sq_owners(active_sq_snapshots);
advance_terminal_done_uid();
begin_uid = get_active_scan_begin_uid();
end_uid   = get_active_scan_end_uid();
```

中文伪代码：按 active record id 定位 cancel record；要求 redirect payload 相同、monitor anchor 已绑定、当前
DUT sample 和已 drain cancel watermark 都到达 record 的 LSQ cutoff，否则立即 fatal。校验通过后先调用
snapshot helper 冻结所有 active SQ identity，再推进 terminal_done 前缀并计算原 admission scan 的 begin/end；
因此后续删除 map 不会破坏补扫输入。

### 6.2 原 prefix scan 保持既有职责

源码位置：`common_data_transaction.sv:4215-4257`，函数：`apply_redirect_flush_range()` prefix 分支。

```systemverilog
for (memblock_uid_t uid = begin_uid; uid < end_uid; uid++) begin
    status = get_status(uid);
    terminal_fault_head = status.active && status.rob_commit &&
                          !status.terminal_done &&
                          status.get_rob_key() == redirect.rob_key &&
                          (status.fault || status.exception_pending ||
                           status.load_fault || status.sta_fault ||
                           status.std_fault);
    if (terminal_fault_head) begin
        preserve_fault_uid_during_redirect(uid, redirect);
        continue;
    end
    if (status.terminal_done || (!status.active && !status.writeback && !status.pass)) begin
        remove_uid_from_issue_queues(uid);
        continue;
    end
    if (rob_order_util::rob_need_flush(status.get_rob_key(), redirect)) begin
        prepare_uid_for_redirect_reissue(uid, redirect);
    end
end
```

中文伪代码：按原 admission 范围逐 UID 读取 status。已提交的 fault head 调用
`preserve_fault_uid_during_redirect()`，该 helper 保留 fault owner并建立独立 fault resource；terminal 或完全
inactive UID 只清理残留 issue item后继续。普通 active UID 用完整 ROB 顺序判断是否被覆盖；被覆盖者调用统一
reissue helper完成 cancel记账和状态清理。该循环没有因本轮 feature 改变 fault/control/普通 owner 优先级。

### 6.3 Prefix 外 SQ owner 补扫

源码位置：`common_data_transaction.sv:4258-4293`，函数：`apply_redirect_flush_range()` supplemental 分支。

```systemverilog
foreach (active_sq_snapshots[snapshot_idx]) begin
    if (!redirect_sq_owner_snapshot_matches(
            active_sq_snapshots[snapshot_idx], redirect,
            cancel_record_q[record_idx].redirect_epoch,
            cancel_record_q[record_idx].redirect_lsq_sample_seq,
            begin_uid, end_uid)) begin
        continue;
    end
    status = get_status(active_sq_snapshots[snapshot_idx].uid);
    main_tr = get_main_transaction(active_sq_snapshots[snapshot_idx].uid);
    if (!found_flushed || active_sq_snapshots[snapshot_idx].uid < oldest_flushed_uid) begin
        oldest_flushed_uid = active_sq_snapshots[snapshot_idx].uid;
        found_flushed = 1'b1;
    end
    provenance = $sformatf(
        "stranded_owner record=%0d epoch=%0d uid=%0d dynamic_epoch=%0d rob=%0d/%0d sq=%0d/%0d reservation_sample=%0d cutoff=%0d numLsElem=%0d",
        cancel_record_q[record_idx].cancel_record_id,
        cancel_record_q[record_idx].redirect_epoch,
        active_sq_snapshots[snapshot_idx].uid,
        status.dynamic_epoch,
        active_sq_snapshots[snapshot_idx].rob_key.flag,
        active_sq_snapshots[snapshot_idx].rob_key.value,
        active_sq_snapshots[snapshot_idx].sq_key.flag,
        active_sq_snapshots[snapshot_idx].sq_key.value,
        status.lsq_reservation_sample_seq,
        cancel_record_q[record_idx].redirect_lsq_sample_seq,
        main_tr.numLsElem);
    record_sq_cancel_provenance(provenance);
    `uvm_info("LSQ_CANCEL", provenance, UVM_LOW)
    prepare_uid_for_redirect_reissue(active_sq_snapshots[snapshot_idx].uid,
                                     redirect);
end
```

中文伪代码：逐条读取删除前 snapshot，并把当前 record 的 redirect epoch、LSQ cutoff 和原 prefix 范围交给
matcher。返回 0 表示 prefix 已处理、合法 survivor 或尚未到 cutoff，直接继续；返回 1 后读取当前 status/main
transaction，更新本轮最老 flush UID，生成包含 record、epoch、UID、ROB/SQ key、reservation sample/cutoff 的
通用日志并写入 provenance ring，最后调用统一 reissue helper完成真正记账和 owner 删除。

### 6.4 Admission rollback 与 record 收尾

源码位置：`common_data_transaction.sv:4294-4314`，函数：`apply_redirect_flush_range()` finalize 分支。

```systemverilog
if (found_flushed && oldest_flushed_uid < end_uid) begin
    rollback_max_enqueued_uid(oldest_flushed_uid);
end
cancel_record_q[record_idx].active_scan_done = 1'b1;
if (redirect_deleted_lq_owner_by_key.num() != 0 ||
    redirect_deleted_sq_owner_by_key.num() != 0) begin
    start_redirect_deleted_owner_window(record_idx);
end else begin
    cancel_record_q[record_idx].software_count_finalized = 1'b1;
end
active_cancel_record_id_valid = 1'b0;
active_cancel_record_id = 0;
check_cancel_pending_aggregate();
```

中文伪代码：只有本轮最老 flush UID 位于原 prefix end 之前才回退 admission 水位；仅补扫 prefix 外 owner时
不回退，避免把水位反向推进。随后标记 active scan 完成；若存在 redirect-deleted LQ/SQ owner，启动短时窗口
等待 cancel/deq apply，否则立即标记软件计数 finalized。最后释放 active record ownership并核对 pending cancel
聚合计数，为 exact compare reconcile阶段准备状态。

### 6.5 关键子函数 `prepare_uid_for_redirect_reissue()`

抽象功能描述：该既有 helper 消费一个已证明应被 redirect 覆盖的 UID，把旧动态实例的 LSQ owner记账并删除，
再把同一 UID 转成待重新 admission 的新动态版本；它是 prefix scan 和本轮 supplemental scan 共享的唯一状态
转换入口。

源码位置：`common_data_transaction.sv:3841-3903`，函数：`prepare_uid_for_redirect_reissue()`。

```systemverilog
capture_redirect_deleted_owner(uid, memblock_sync_pkg::dispatch_flush_epoch);
note_lsq_cancel_for_uid(uid, memblock_sync_pkg::dispatch_flush_epoch);
cancel_waiting_uid_tlb_record_for_uid(uid, "redirect_flush");
if (status.active) begin
    retire_active_uid(uid);
end else begin
    remove_uid_from_issue_queues(uid);
end
void'(clear_uid_dispatch_result(uid));
status.redirect_pending = 1'b1;
status.flushed          = 1'b1;
status.dynamic_epoch++;
status.active           = 1'b0;
status.success          = 1'b0;
status.terminal_done    = 1'b0;
```

中文伪代码：先调用 `capture_redirect_deleted_owner()` 保存删除前完整物理身份，供迟到 deq/cancel apply 查询；
再调用 `note_lsq_cancel_for_uid()`验证 record、reservation/cutoff和防重 epoch，并增加软件 LQ/SQ cancel count。
随后取消该 UID 的 TLB wait；active owner走 `retire_active_uid()`统一删除 ROB/LQ/SQ map，非 active只清 issue
queue。清除旧 dispatch result 后设置 redirect_pending/flushed，dynamic epoch加一并清 active/success/terminal，
使同一 UID 后续从 admission 重新执行。

### 6.6 关键子函数 `note_lsq_cancel_for_uid()`

抽象功能描述：该既有 helper 将一个已被 redirect 覆盖且对 DUT 可见的 scalar owner记入当前 cancel record，
同时更新 pending cancel聚合和防重 epoch；它不删除 active map。

源码位置：`common_data_transaction.sv:3154-3223`，函数：`note_lsq_cancel_for_uid()`。

```systemverilog
if (status.lsq_reservation_state != MEMBLOCK_LSQ_RESERVATION_DUT_VISIBLE ||
    !status.lsq_reservation_sample_valid) begin
    `uvm_fatal("LSQ_CANCEL",
               $sformatf("uid=%0d mapped cancel lacks DUT-visible reservation state=%0d valid=%0d",
                         uid, status.lsq_reservation_state,
                         status.lsq_reservation_sample_valid))
end
if (status.lsq_reservation_sample_seq >
    cancel_record_q[record_idx].redirect_lsq_sample_seq) begin
    `uvm_fatal("LSQ_CANCEL",
               $sformatf("uid=%0d reservation sample=%0d is later than redirect LSQ cutoff=%0d",
                         uid, status.lsq_reservation_sample_seq,
                         cancel_record_q[record_idx].redirect_lsq_sample_seq))
end
if (status.active_sq_mapped) begin
    cancel_record_q[record_idx].software_cancel_sq_count += main_tr.numLsElem;
    pending_sq_cancel_count += main_tr.numLsElem;
end
status.lsq_cancel_accounted_epoch = redirect_epoch;
status.lsq_reservation_state = MEMBLOCK_LSQ_RESERVATION_CANCEL_ACCOUNTED;
check_cancel_pending_aggregate();
```

中文伪代码：要求 mapped owner reservation 已为 `DUT_VISIBLE` 且 sample有效；sample晚于当前 record 的 LSQ
cutoff立即 fatal。若有 SQ mapping，按 scalar `numLsElem` 增加 record ordinary SQ count 和 pending SQ apply
count；LQ mapping沿既有对称分支处理。完成后记录本 redirect epoch 已accounted，把 reservation 转为
`CANCEL_ACCOUNTED`，最后检查所有 pending计数之和与 record状态一致。

## 7. Fault Candidate 与失败诊断保持独立

### 7.1 Fault SQ 仍在 compare snapshot 阶段归因

抽象功能描述：`resolve_fault_sq_candidate_from_snapshot()` 比较 DUT exact snapshot 与 ordinary SQ count，决定
fault head 是被 cancel 还是等待 deq；本轮 supplemental scan只把漏掉的 ordinary owner从8补到9，不修改该
仲裁职责。

源码位置：`common_data_transaction.sv:2954-2988`，函数：
`resolve_fault_sq_candidate_from_snapshot()`。

```systemverilog
ordinary_sq_count = cancel_record_q[record_idx].software_cancel_sq_count;
if (snapshot.sq_cancel_count == ordinary_sq_count + 1) begin
    cancel_record_q[record_idx].software_cancel_sq_count++;
    pending_sq_cancel_count++;
    detach_fault_sq_mapping(fault_redirect_resource.uid);
    fault_redirect_resource.disposition =
        MEMBLOCK_FAULT_LSQ_DISPOSITION_CANCEL_APPLIED_WAIT_RECORD_APPLY;
end else if (snapshot.sq_cancel_count == ordinary_sq_count) begin
    fault_redirect_resource.disposition = MEMBLOCK_FAULT_LSQ_DISPOSITION_WAIT_SQ_DEQ;
end else begin
    dump_sq_cancel_reconcile_diag(record_idx, snapshot);
    `uvm_fatal("LSQ_CANCEL_RECONCILE",
               $sformatf("fault SQ candidate delta invalid record=%0d ordinary=%0d dut=%0d",
                         cancel_record_q[record_idx].cancel_record_id,
                         ordinary_sq_count, snapshot.sq_cancel_count))
end
```

中文伪代码：先读取 active scan完成后的 ordinary SQ count；DUT count恰好多1时，把这1归因给 fault head，
增加最终软件count和pending apply，并解除 fault SQ live mapping；DUT count等于ordinary时，fault entry等待正常
deq。其它差值先打印纯软件诊断再fatal。本轮真实seed中ordinary先由8补为9，DUT 10因此命中多1分支。

### 7.2 Reconcile fatal 前的纯软件诊断

抽象功能描述：`dump_sq_cancel_reconcile_diag()` 只在对账即将 fatal 时打印 cancel record、provenance ring、
redirect-deleted owner和当前 SQ status；它不访问不稳定 RTL层次，也不改变判定。

源码位置：`common_data_transaction.sv:3414-3427`，调用点：`service_cancel_reconcile()`。

```systemverilog
resolve_fault_sq_candidate_from_snapshot(record_idx, snapshot);
if (snapshot.lq_cancel_count !=
        cancel_record_q[record_idx].software_cancel_lq_count ||
    snapshot.sq_cancel_count !=
        cancel_record_q[record_idx].software_cancel_sq_count) begin
    dump_sq_cancel_reconcile_diag(record_idx, snapshot);
    `uvm_fatal("LSQ_CANCEL_RECONCILE",
               $sformatf("cancel mismatch record=%0d epoch=%0d software=%0d/%0d dut=%0d/%0d",
                         cancel_record_q[record_idx].cancel_record_id,
                         cancel_record_q[record_idx].redirect_epoch,
                         cancel_record_q[record_idx].software_cancel_lq_count,
                         cancel_record_q[record_idx].software_cancel_sq_count,
                         snapshot.lq_cancel_count,
                         snapshot.sq_cancel_count))
end
```

中文伪代码：exact compare snapshot到达且软件计数finalized后，先让 fault candidate完成独立归因；然后同时
比较 DUT/software LQ和SQ count。任一不相等时先调用 dump helper 输出纯软件状态，再触发原有 reconcile fatal。
计数相等时不调用 dump，因此诊断扫描不进入成功高频路径。

## 8. 专项 Soft Sequence

### 8.1 Sequence 入口和 helper 初始化

抽象功能描述：`body()` 初始化公共模型/helper，先运行真实两轮 redirect 主场景，再运行 matcher 负向边界；
它不启动随机主流程或新增 testcase。

源码位置：`soft_test_memblock_stranded_sq_cancel_sequence.sv:67-86`，task：`body()`、
`ensure_directed_helpers()`。

```systemverilog
task soft_test_memblock_stranded_sq_cancel_sequence::body();
    ensure_directed_helpers();
    run_two_redirect_stranded_owner_scenario();
    run_matcher_boundary_scenarios();
endtask

task soft_test_memblock_stranded_sq_cancel_sequence::ensure_directed_helpers();
    seq_csr_common::init();
    data = common_data_transaction::get();
    lsq_ctrl = lsq_ctrl_model::get();
    cancel_apply_seq = memblock_lsqenq_dispatch_base_sequence::type_id::create(
        "cancel_apply_seq");
    if (data == null || lsq_ctrl == null || cancel_apply_seq == null) begin
        `uvm_fatal(get_type_name(), "failed to initialize stranded SQ cancel directed helpers")
    end
endtask
```

中文伪代码：`body()`先获取测试所需公共单例和cancel apply helper；任一对象为空立即fatal，避免测试静默跳过。
初始化成功后按固定顺序执行两轮redirect正向场景和matcher负向场景。`cancel_apply_seq`只复用生产路径的
pending cancel apply逻辑，不直接改公共计数或map。

### 8.2 `execute_redirect_and_close_record()` 的 cutoff/compare 语义

抽象功能描述：该 task 用生产等价 API 建立 redirect request、drive done、monitor anchor、cutoff watermark、
active scan、exact compare和deleted-owner window收尾；输入是 redirect payload 和预期 SQ count，输出本 record
软件 count。它不把 cutoff snapshot当作最终 compare target。

源码位置：`soft_test_memblock_stranded_sq_cancel_sequence.sv:176-224`，task：
`execute_redirect_and_close_record()` 前半段。

```systemverilog
data.request_redirect_flush(redirect);
data.push_redirect_drive(redirect);
if (!data.try_pop_redirect_drive(driven_redirect) ||
    !data.redirect_payload_equal(driven_redirect, redirect)) begin
    `uvm_fatal(get_type_name(), "failed to claim directed redirect payload")
end
data.mark_redirect_drive_done(driven_redirect);
anchor_sample = memblock_sync_pkg::peek_current_dut_global_sample() + 1;
wait_for_dut_sample(anchor_sample, "redirect-anchor");
anchor = memblock_sync_pkg::make_empty_raw_redirect_anchor();
anchor.valid = 1'b1;
anchor.rob_flag = redirect.rob_key.flag;
anchor.rob_value = redirect.rob_key.value;
anchor.sample_seq = anchor_sample;
data.add_redirect_anchor(anchor);
data.service_cancel_reconcile();
```

中文伪代码：通过公共 API请求 redirect并放入drive queue，再从queue取回并核对payload，确认该task确实拥有本次
drive；标记drive done后等待下一DUT sample，构造包含level、ROB full key和sample的monitor anchor并加入
anchor FIFO。调用 reconcile service把anchor绑定到最老未锚定cancel record，不直接写record内部字段。

```systemverilog
cutoff_sample = data.cancel_record_q[record_idx].redirect_lsq_sample_seq;
compare_sample = data.cancel_record_q[record_idx].compare_snapshot_sample_seq;
if (cutoff_sample >= compare_sample) begin
    `uvm_fatal(get_type_name(),
               $sformatf("redirect cutoff=%0d must precede compare target=%0d",
                         cutoff_sample, compare_sample))
end
wait_for_dut_sample(cutoff_sample, "redirect-cutoff");
snapshot = memblock_sync_pkg::make_empty_raw_cancel_snapshot();
snapshot.sq_cancel_count = held_sq_count;
snapshot.sample_seq = cutoff_sample;
data.add_cancel_snapshot(snapshot);
data.service_cancel_reconcile();
if (!data.cancel_redirect_scan_ready(redirect)) begin
    `uvm_fatal(get_type_name(), "directed redirect did not reach scan-ready cutoff")
end
```

中文伪代码：从刚绑定的cancel record读取LSQ cutoff和exact compare target，若cutoff不早于compare立即fatal，
防止测试把两个时序点混用。等待cutoff后加入held count snapshot；reconcile只把它当baseline/watermark消费。
随后要求生产函数确认active scan已ready；测试不在cutoff样本伪造最终DUT期望。

```systemverilog
data.apply_redirect_flush(redirect);
observed_software_sq_count =
    data.cancel_record_q[record_idx].software_cancel_sq_count;
if (observed_software_sq_count != expected_sq_count) begin
    `uvm_fatal(get_type_name(),
               $sformatf("directed software SQ count mismatch expected=%0d observed=%0d",
                         expected_sq_count, observed_software_sq_count))
end
wait_for_dut_sample(compare_sample, "cancel-compare");
snapshot = memblock_sync_pkg::make_empty_raw_cancel_snapshot();
snapshot.sq_cancel_count = expected_sq_count;
snapshot.sample_seq = compare_sample;
data.add_cancel_snapshot(snapshot);
cancel_apply_seq.apply_pending_lsq_cancels();
data.service_cancel_reconcile();
```

中文伪代码：到达cutoff后调用正式 redirect flush入口，读取该record软件SQ count并与预期比较。再等待精确
compare sample，加入最终DUT snapshot；随后用生产cancel apply sequence消耗pending cancel，并调用reconcile
完成exact compare。最后的窗口service和pending聚合断言要求record、deleted-owner window及pending LQ/SQ
count全部关闭，否则fatal。

### 8.3 `run_two_redirect_stranded_owner_scenario()`

抽象功能描述：该 task 构造两个已 `DUT_VISIBLE` 的 scalar store；第一轮 redirect覆盖 uid0并自然回退
admission prefix、保留 uid1，第二轮再覆盖 uid1，验证 supplemental scan只计一次并删除owner。

源码位置：`soft_test_memblock_stranded_sq_cancel_sequence.sv:265-306`，task：
`run_two_redirect_stranded_owner_scenario()`。

```systemverilog
build_two_redirect_store_table();
admit_store_and_mark_visible(0);
admit_store_and_mark_visible(1);
if (!data.dispatch_progress.max_enqueued_uid_valid ||
    data.dispatch_progress.max_enqueued_uid != 1) begin
    `uvm_fatal(get_type_name(),
               "commit_allocate did not advance admission prefix through uid1")
end
first_redirect = '{default:'0};
first_redirect.valid = 1'b1;
first_redirect.rob_key.flag = 1'b1;
first_redirect.rob_key.value = 75;
execute_redirect_and_close_record(first_redirect, 0, 1, first_count);
```

中文伪代码：重置模型并创建 uid0=`ROB 1/76`、uid1=`ROB 0/76` 两个scalar store；逐个调用正式 allocate和
reservation sample API，证明 admission prefix真实推进到uid1，禁止直接改水位。第一轮 redirect设为`1/75`；
ROB wrap语义覆盖uid0但不覆盖uid1，正式redirect helper应计1并关闭record。

```systemverilog
if (first_count != 1 || victim_status.active ||
    !victim_status.redirect_pending || victim_status.dynamic_epoch != 1 ||
    survivor_status.dynamic_epoch != 0 || !survivor_status.active ||
    !survivor_status.enq || !survivor_status.active_sq_mapped ||
    survivor_status.lsq_reservation_state != MEMBLOCK_LSQ_RESERVATION_DUT_VISIBLE ||
    data.dispatch_progress.max_enqueued_uid_valid) begin
    `uvm_fatal(get_type_name(),
               "first redirect did not create a prefix-external DUT-visible survivor")
end
second_redirect.rob_key.flag = 1'b1;
second_redirect.rob_key.value = 76;
execute_redirect_and_close_record(second_redirect, 1, 1, second_count);
```

中文伪代码：第一轮后要求uid0已reissue且dynamic epoch加一，uid1仍active/enq/SQ-mapped、版本未变并保持
`DUT_VISIBLE`；同时 admission prefix已被正式rollback清空，使uid1成为prefix外owner。第二轮redirect设为
`1/76`，这次覆盖uid1；cutoff baseline为1且预期ordinary count为1，只有补扫能找到该owner。

```systemverilog
if (second_count != 1 || survivor_status.active ||
    survivor_status.active_sq_mapped ||
    !survivor_status.redirect_pending || survivor_status.dynamic_epoch != 1 ||
    data.uid_by_sq.num() != 0 ||
    data.sq_cancel_provenance_history_q.size() != 1) begin
    `uvm_fatal(get_type_name(),
               "second redirect did not consume the stranded owner exactly once")
end
```

中文伪代码：第二轮完成后要求软件SQ count精确为1；uid1不再active且SQ mapping已删除，redirect_pending置位，
dynamic epoch只增加一次。全局SQ active map必须为空，provenance ring必须只有一条成功补记记录；任一条件不符
都说明漏记、重复处理或owner未清理，测试立即fatal。

### 8.4 Expected-fatal catcher 与边界场景

抽象功能描述：catcher只捕获 ID 为 `LSQ_CANCEL_STRANDED_OWNER` 的预期 fatal，其他 fatal继续抛出；
`expect_matcher_fatal()`要求每个负向场景恰好触发一次。

源码位置：`soft_test_memblock_stranded_sq_cancel_sequence.sv:11-27`，class：
`memblock_stranded_sq_cancel_expected_fatal_catcher`。

```systemverilog
virtual function action_e catch();
    if (get_severity() == UVM_FATAL &&
        get_id() == "LSQ_CANCEL_STRANDED_OWNER") begin
        caught_count++;
        return CAUGHT;
    end
    return THROW;
endfunction
```

中文伪代码：每次UVM report进入catcher时，只有severity为fatal且report id精确匹配本feature时才增加计数并
返回CAUGHT，阻止该预期fatal结束测试；其它error/fatal原样THROW，保证专项测试不会掩盖无关失败。

源码位置：`soft_test_memblock_stranded_sq_cancel_sequence.sv:309-329`，task：
`expect_matcher_fatal()`。

```systemverilog
catcher = new({scenario_name, "_catcher"});
uvm_report_cb::add(null, catcher);
void'(data.redirect_sq_owner_snapshot_matches(snapshot, redirect,
                                              redirect_epoch, cutoff,
                                              0, 0));
uvm_report_cb::delete(null, catcher);
if (catcher.caught_count != 1) begin
    `uvm_fatal(get_type_name(),
               $sformatf("%s expected one stranded-owner fatal, got %0d",
                         scenario_name, catcher.caught_count))
end
```

中文伪代码：为当前场景创建独立catcher并临时注册到全局report callback；直接调用matcher，传入场景构造的
snapshot、redirect、epoch和cutoff。调用返回后立即删除callback，避免影响后续场景；若捕获数量不是精确1，
说明matcher未拒绝非法状态或重复fatal，测试自身触发fatal。

已执行负向覆盖：dynamic epoch mismatch、full SQ key mismatch、`rob_commit`、terminal、fault、exception、
already-accounted、control。另有返回0场景：redirect不覆盖、UID位于prefix、reservation sample晚于cutoff。

## 9. 专项 Vseq 与 Package 接入

### 9.1 `memblock_stranded_sq_cancel_directed_vseq::body()`

抽象功能描述：该 vseq 是 `basicTest` 的专项入口，只验证 virtual sequencer有效并启动 soft sequence；所有
建表、redirect和断言均由soft sequence负责。

源码位置：`memblock_stranded_sq_cancel_directed_vseq.sv:26-31`，task：`body()`。

```systemverilog
task memblock_stranded_sq_cancel_directed_vseq::body();
    soft_test_memblock_stranded_sq_cancel_sequence directed_seq;

    require_virtual_sqr();
    `uvm_do_on(directed_seq, p_sequencer)
endtask
```

中文伪代码：先调用 `require_virtual_sqr()` 检查 `p_sequencer` 已由 testcase正确注入；失败由基类fatal。
检查通过后通过标准UVM宏创建并把soft sequence启动到同一virtual sequencer；vseq不直接写公共status/map，
也不缓存agent sequencer或新增default sequence配置。

### 9.2 `seq_pkg.sv` include 顺序

抽象功能描述：package include顺序保证新soft sequence使用的`cancel_apply_seq`类型先定义，并保证新vseq在
`virtual_base_sequence`和soft sequence之后可见；它不改变filelist路径。

源码位置：`mem_ut/ver/ut/memblock/seq/seq_pkg.sv:97-98,127-133`。

```systemverilog
`include "memblock_lsqenq_dispatch_base_sequence.sv"
`include "soft_test_memblock_stranded_sq_cancel_sequence.sv"
`include "memblock_virtual_sequencer.sv"
`include "virtual_base_sequence.sv"
`include "memblock_mmu_sv39_csr_sequence.sv"
`include "memblock_mmu_sv39_pbmt0_non_nc_csr_sequence.sv"
`include "memblock_l2tlb_pbmt_toggle_csr_sequence.sv"
`include "memblock_pending_mmio_directed_vseq.sv"
`include "memblock_stranded_sq_cancel_directed_vseq.sv"
```

中文伪代码：编译package时先展开LSQ enqueue base sequence，使soft sequence字段
`memblock_lsqenq_dispatch_base_sequence cancel_apply_seq`的类型已声明；随后定义virtual sequencer和vseq基类，
最后展开专项vseq。现有`seq.f`已提供soft-test和virtual-sequence incdir，所以不新增重复filelist entry。

## 10. 调用关系与状态副作用

| 顺序 | 函数/task | 输入来源 | 输出或副作用 | 回到主流程后的影响 |
| --- | --- | --- | --- | --- |
| 1 | `apply_redirect_flush_range()` | active redirect、cancel record、公共状态 | 驱动整次 owner scan和record收尾 | 决定 software count何时finalized。 |
| 2 | `snapshot_dut_visible_sq_owners()` | `uid_by_sq`、status、main table | 生成局部值queue | 后续可安全删除active map。 |
| 3 | 原 prefix scan | admission begin/end、ROB顺序 | 处理fault head和prefix内owner | 保持旧行为优先。 |
| 4 | `redirect_sq_owner_snapshot_matches()` | snapshot、redirect、epoch、cutoff、prefix范围 | 返回0/1或fatal | 只有返回1才授权补处理。 |
| 5 | `record_sq_cancel_provenance()` | matcher通过的owner归因 | 写有界debug ring | 不影响判定。 |
| 6 | `prepare_uid_for_redirect_reissue()` | 已证明被覆盖的UID | 保存deleted owner、记账、删map、epoch++ | UID进入reissue状态。 |
| 7 | `note_lsq_cancel_for_uid()` | current cancel record和reservation | ordinary count++、pending count++、accounted epoch | exact compare可以对账。 |
| 8 | `resolve_fault_sq_candidate_from_snapshot()` | exact compare snapshot | fault contribution 0或1 | ordinary与fault职责保持分离。 |
| 9 | `service_cancel_reconcile()` | snapshot FIFO和finalized record | 对账成功或fatal | record进入observed/apply收尾。 |

## 11. Plan 对齐检查

对应 plan 已找到：
`AI_DOC/plan/test_framework/plan/undo/memblock_v2_stranded_sq_cancel_owner_coding_plan_20261012.md`。

### 11.1 已一致落实的 Plan 项

| Plan 要求 | 当前实现 | 结论 |
| --- | --- | --- |
| 根因采用“前一轮回退prefix但保留未覆盖owner，下一轮覆盖却扫描不到” | 两轮directed场景真实通过正式rollback形成prefix外survivor | 一致。 |
| redirect前冻结有限active SQ map | `snapshot_dut_visible_sq_owners()`遍历`uid_by_sq` | 一致。 |
| full ROB/SQ key、dynamic epoch和map三方一致 | matcher逐项fatal校验 | 一致。 |
| 只接受ordinary scalar SQ、`numLsElem==1` | matcher分类分支 | 一致。 |
| control/commit/terminal/fault/exception/accounted-active fail-fast | `LSQ_CANCEL_STRANDED_OWNER` | 一致。 |
| reservation必须cutoff前`DUT_VISIBLE` | matcher返回0，计数helper再次强校验 | 一致。 |
| 复用`prepare_uid_for_redirect_reissue()` | prefix和补扫共用同一helper | 一致。 |
| fault candidate独立归因 | exact compare阶段仍由既有resource处理 | 一致。 |
| `basicTest + 专项 vseq` | 新soft sequence和vseq已接入`seq_pkg` | 一致。 |
| 清除seed特化诊断 | affected source无`3800/4000/3880/676087581/uvm_hdl_read` | 一致。 |

### 11.2 实现与 Plan 不一致项

未发现实现与 Plan 不一致项；当前 coding 行为与对应 plan 保持一致。尚未执行的部分专项场景属于验证覆盖
缺口，单列在第 11.4 节，不改变本节的行为实现一致性结论。

### 11.3 Plan 未说明但 Coding 落实的细节

#### 11.3.1 Provenance ring 固定深度 128

细节功能：plan要求有界通用诊断，但未指定具体深度；coding选择128，足以保留最近多次redirect归因且不随
10k transaction增长。plan只要求“有界”，未覆盖具体容量；该 ring 只增强失败定位，不影响owner选择和
cancel计数。源码位置：`common_data_transaction.sv`，函数：`record_sq_cancel_provenance()`。

```systemverilog
if (sq_cancel_provenance_history_q.size() >=
    MEMBLOCK_SQ_CANCEL_PROVENANCE_DEPTH) begin
    void'(sq_cancel_provenance_history_q.pop_front());
end
sq_cancel_provenance_history_q.push_back(history_entry);
```

中文伪代码：每次新增stranded-owner归因时先检查ring是否已达到128条；已满就删除最旧记录，再把当前记录
加入队尾。该操作只修改诊断历史，不修改active map、cancel record或匹配结果。该固定深度无需回写plan正文。

#### 11.3.2 Cutoff/compare 顺序的测试内强断言

细节功能：plan区分cutoff和compare，但coding额外在directed helper中要求`cutoff_sample < compare_sample`，
防止未来延迟常量变化导致测试把baseline当target。plan没有要求该防御性断言；它只影响专项测试失败方式，
不改变生产reconcile。源码位置：`soft_test_memblock_stranded_sq_cancel_sequence.sv`，task：
`execute_redirect_and_close_record()`。

```systemverilog
cutoff_sample = data.cancel_record_q[record_idx].redirect_lsq_sample_seq;
compare_sample = data.cancel_record_q[record_idx].compare_snapshot_sample_seq;
if (cutoff_sample >= compare_sample) begin
    `uvm_fatal(get_type_name(),
               $sformatf("redirect cutoff=%0d must precede compare target=%0d",
                         cutoff_sample, compare_sample))
end
```

中文伪代码：读取同一cancel record的cutoff和exact compare target；若cutoff没有严格早于compare，专项测试
立即fatal，避免把held baseline样本误当成最终对账样本。该细节只保留在implementation review，无需回写plan。

#### 11.3.3 Prefix 外 owner不触发水位反向推进

细节功能：coding用`oldest_flushed_uid < end_uid`作为rollback guard，把plan中的文字边界落实成直接条件。
plan描述了prefix外owner不应改变admission水位，但未指定条件表达式；该guard保证只有原prefix内flush才回退。
源码位置：`common_data_transaction.sv`，函数：`apply_redirect_flush_range()`。

```systemverilog
if (found_flushed && oldest_flushed_uid < end_uid) begin
    rollback_max_enqueued_uid(oldest_flushed_uid);
end
```

中文伪代码：本轮存在被flush owner且最老UID仍位于原prefix end之前时，调用rollback helper把admission水位
回退到该UID之前；若只补扫prefix外owner，则不调用rollback，避免把已回退水位反向推进。该实现细化无需回写plan。

### 11.4 归档前验证覆盖缺口

以下项目是尚未执行的专项测试，不是 coding 与 plan 的行为不一致，也不是已实现 SQ 修复 blocker：

| Plan 测试要求 | 当前测试 | 缺口原因 | 处理结论 |
| --- | --- | --- | --- |
| 单独注入 ROB key mismatch并捕获fatal | 未单独执行；真实/正向只验证ROB覆盖和wrap survivor | soft test当前只修改dynamic epoch和SQ key副本 | 后续补充matcher expected-fatal场景。 |
| 单独注入`uid_by_sq` owner mismatch并捕获fatal | 未单独执行 | 直接改公共map会影响同sequence后续场景，需要独立reset/恢复 | 后续新增隔离场景，不在文档收尾中改行为代码。 |
| reservation非`DUT_VISIBLE`时返回0且保持owner | 当前只验证sample晚于cutoff返回0 | 测试遗漏pending/none状态分支 | 后续补充状态隔离场景。 |
| soft sequence内验证fault candidate与stranded ordinary并存 | soft sequence未构造fault resource；有效真实seed已证明ordinary 9 + fault 1 | 真实场景提供了更强DUT证据，但执行载体与plan不同 | 保留真实seed证据，并在后续专项补齐software-only复现。 |

## 12. 验证结果

### 12.1 静态与编译

- `git diff --check`：通过。
- affected source seed/UID/record特化诊断扫描：无残留。
- VCS compile/elaboration/link：成功，退出码0。
- 编译日志：`mem_ut/ver/ut/memblock/sim/base_fun/log/vcs_compile_rtl.log`。

### 12.2 专项 vseq

```text
make eda_run tc=basicTest ts=memblock_stranded_sq_cancel_directed_vseq \
  mode=base_fun note=fault3_directed
```

日志：`mem_ut/ver/ut/memblock/sim/base_fun/log/tc=basicTest_ts=memblock_stranded_sq_cancel_directed_vseq_cfg=default_seed=666666_rtl_fault3_directed.log`

结果：`stranded_owner record=2 epoch=2 uid=1`；8个预期fatal被catcher捕获；`TEST_PASS`、
`UVM_ERROR=0`、`UVM_FATAL=0`。

### 12.3 有效真实 seed `676087581`

临时 cfg 与主工作区原文件逐字一致；日志首行确认包含：

```text
+MEMBLOCK_MAIN_TRANS_NUM=10000
+MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=1
+MEMBLOCK_CHECK_TRIGGER_EN=0
```

命令：

```text
make eda_batch_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq \
  mode=base_fun cfg=tc_l2tlb_fault_2pct_no_trigger seed=676087581 \
  pl=UVM_LOW note=fault3_fix_valid
```

有效日志：`mem_ut/ver/ut/memblock/sim/base_fun/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=676087581_rtl_fault3_fix_valid.log`

关键证据：

```text
977968.700ns stranded_owner record=3896 epoch=3896 uid=4116 dynamic_epoch=0 rob=0/76 sq=1/35 reservation_sample=195546 cutoff=195570 numLsElem=1
977983.700ns fault SQ snapshot->cancel uid=3956 record=3896 ordinary=9 dut=10
```

record 3896 未再出现 `LSQ_CANCEL_RECONCILE`，SQ原问题闭环。仿真继续到`978120.700ns`后出现：

```text
UVM_FATAL [COMMON_DATA] lqIdx already active: uid=4033 existing_uid=4118
```

该 LQ 冲突是完整 seed 后续 blocker；现有证据尚未闭合 uid 4118 的 reservation、redirect覆盖、cutoff和
DUT cancel/deq归属，不能把SQ matcher直接复制到LQ路径。此前缺cfg、实际运行default 100 transaction的
`fault3_fix_final`日志无效，未用于本review。

### 12.4 正常 seed `666666`

命令：

```text
make eda_batch_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq \
  mode=base_fun cfg=tc_dispatch_real_smoke seed=666666 \
  pl=UVM_LOW note=fault3_normal
```

日志：`mem_ut/ver/ut/memblock/sim/base_fun/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_smoke_seed=666666_rtl_fault3_normal.log`

首行展开`+MEMBLOCK_MAIN_TRANS_NUM=1`和issue/enq/commit配置；结果`TEST_PASS`、`UVM_ERROR=0`、
`UVM_FATAL=0`。

## 13. Git Status 对比与非本次修改分类

### 13.1 本次 review 主题文件

| 状态 | 文件 | 分类与处理 |
| --- | --- | --- |
| `M` | `mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv` | 本次SQ owner行为和诊断实现，已完整review。 |
| `M` | `mem_ut/ver/ut/memblock/seq/seq_pkg.sv` | 本次soft sequence/vseq include接入，已review。 |
| `??` | `mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_stranded_sq_cancel_sequence.sv` | 本次专项测试，已review。 |
| `??` | `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_stranded_sq_cancel_directed_vseq.sv` | 本次专项vseq，已review。 |
| `??` | coding plan、plan review、implementation review、`fault_2pct_status_20261010.md` | 本次设计、授权、实现review和状态同步文档。 |

### 13.2 非正式 diff / 仿真产物

| 状态 | 文件 | 分类与处理 |
| --- | --- | --- |
| `??` | `mem_ut/ver/ut/memblock/sim/.eda_remote/basicTest_memblock_dispatch_real_smoke_vseq_base_fun.pid` | 既有远端任务pid，未删除、未修改、不得纳入正式diff。 |
| 已清理 | `mem_ut/ver/ut/memblock/seq/plus_cfg/tc_l2tlb_fault_2pct_no_trigger.cfg` | 本轮从主工作区临时复制的测试输入，测试后已删除。 |
| 已清理 | `mem_ut/ver/ut/memblock/sim/.envrc` | 本轮临时环境文件，测试后已删除。 |

当前 worktree 没有其它未分类的 `mem_ut` 源码修改。仿真log、wave和generated build目录由ignore规则管理，
不属于git status正式diff。

## 14. 最终 Review 结论与归档决定

SQ行为实现满足approved plan：snapshot范围有限、matcher证明闭合、ordinary/fault职责分离、原prefix行为保留、
真实record 3896从ordinary 8修正为9并与fault 1共同匹配DUT 10。未发现SQ实现blocker。

当前不归档的原因有两类：

1. 完整真实seed在SQ问题闭环后出现新的LQ owner冲突，这是完整seed后续blocker，不是SQ实现blocker。
2. plan列出的ROB-key mismatch、`uid_by_sq` owner mismatch、非`DUT_VISIBLE`和software-only fault coexist仍有
   专项覆盖缺口。

因此 coding plan和本 implementation review继续保留在`undo`；不提交git commit，不纳入既有pid。后续应对
LQ冲突单独诊断并形成独立plan/review，同时补齐上述software-only边界后再评估归档。
