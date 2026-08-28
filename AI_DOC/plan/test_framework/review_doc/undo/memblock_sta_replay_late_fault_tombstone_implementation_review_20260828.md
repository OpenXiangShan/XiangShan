# MemBlock STA replay 迟到 fault tombstone 实现级 Review（2026-08-28）

| 项目 | 内容 |
| --- | --- |
| Review 状态 | 代码实现与独立复审通过；10000 笔 Sv39/U 态回归待执行 |
| 版本 | V2，`mem_ut_uvm_v2` |
| 对应 plan | `AI_DOC/plan/test_framework/plan/do/memblock_sta_replay_late_fault_tombstone_coding_plan_20260828.md` |
| 问题分析 | `AI_DOC/analysis/framework_design/memblock_sta_replay_late_fault_tombstone_rm_issue_analysis_20260828.md` |
| 源码范围 | `status_transaction.sv`、`common_data_transaction.sv`、`writeback_status_handler.sv`、`dispatch_monitor_event_adapter.sv` |
| 不修改范围 | RTL、Scala、DUT interface、PMA/PMP、TLB 运行时表、DCache denied/corrupt ledger、cfg 和激励权重 |
| 独立 plan review | 已完成，最终结论为无必须修改项 |

## 1. 专有名词与职责

| 术语 | 中文含义 | 代码落点 | 本次使用场景 |
| --- | --- | --- |
| `STA` | Store Address 子操作，负责 store 地址与异常侧。 | `MEMBLOCK_ISSUE_TARGET_STA` | IQ `hit=0` 后等待重发或迟到异常。 |
| `tombstone` | 旧 STA 发射身份的有界历史记录。 | `sta_late_fault_tombstone_q` | raw fault 没有 current snapshot 时还原旧 identity。 |
| current snapshot | 当前动态实例的 STA issue identity。 | `sta_issue_epoch`、`replay_seq`、STA flush epoch | normal raw STA 的唯一归属。 |
| terminal fault | 已锁定异常结果的 UID 状态。 | `fault`、`exception_pending`、`sta_fault` | 后续 replay/fault 不得改写结果。 |
| PTW wait replay | 因 TLB/PTW 尚未可用而暂存的 replay。 | `ptw_wait_replay_q` | 不能在 terminal fault 后重新插入 stale item。 |
| dynamic epoch | redirect 后同一 UID 的动态实例版本。 | `status.dynamic_epoch` | 防止旧 tombstone 附着到重建实例。 |

## 2. Review 结论

本次实现解决的是测试框架生命周期问题，不改变 DUT 行为。旧实现把 STA IQ `hit=0` 与“必须等到一条 raw STA 写回”绑定：UID9 证明纯 replay 可以永远没有 raw，因此旧 drain 状态会永久保持 `sta_dispatched`，阻断 reissue。UID25 又证明旧 pipeline 可以在 replay 后迟到输出 `0x8080` fault，因此不能简单删除旧 identity。

实现采用“立即 replay + fault-only tombstone”的组合：IQ failed 时先保存旧 identity、立即进入既有 replay 流程；normal raw STA 仍严格使用 current snapshot；仅 exception raw 在 current snapshot 不存在时可从同 UID 的 tombstone 还原身份。PMA/PMP frozen context、TLB 结果和 DCache sticky ledger 均不在此路径读写。

## 3. 历史记录与创建路径

修改前没有保存 IQ `hit=0` 对应的旧 STA identity，且旧 drain 状态把 replay 开放依赖于 raw writeback。修改后以每 UID 有界 queue 保存能够由 V2 raw 可验证的 `ROB/SQ/dynamic epoch/flush` 约束，以及用于恢复事件的 `issue_epoch/replay_seq`。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/status_transaction.sv`，字段 `sta_late_fault_tombstone_q`。

```systemverilog
typedef struct {
    bit                  valid;
    memblock_rob_key_t   rob_key;
    memblock_sq_key_t    sq_key;
    int unsigned         issue_epoch;
    int unsigned         replay_seq;
    int unsigned         dynamic_epoch;
    int unsigned         target_flush_epoch;
    longint unsigned     create_cycle;
} memblock_sta_late_fault_tombstone_t;
```
中文伪代码：该结构保存一次 STA 发射的完整软件身份。IQ `hit=0` 时写入，adapter 只在 raw 是异常且 current identity 不成立时读取。`dynamic_epoch`、ROB、SQ 和 flush 下界共同限制记录只能服务原动态实例；`create_cycle` 在多条兼容记录时选最早者。

抽象功能描述：`capture_sta_late_fault_tombstone()` 在 STA IQ failed 已确认属于当前 active STA 后，冻结旧 identity 并保留原有 replay owner；它不等待 raw，也不设置 fault。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/writeback_status_handler.sv`，`handle_issue_feedback_event()` 的 STA failed 分支。

```systemverilog
if (wb_event.target == MEMBLOCK_ISSUE_TARGET_STA) begin
    if (!data.capture_sta_late_fault_tombstone(uid, issue_epoch, replay_seq,
                                                wb_event.cycle)) begin
        return 1'b0;
    end
    data.push_feedback_event(wb_event);
    return 1'b1;
end
```
中文伪代码：STA failed feedback 先请求公共状态表冻结旧 identity；若输入已失效则丢弃。冻结成功后无条件把原 feedback 交回既有 recovery queue，因此非 PTW wait 的场景能立即执行 `mark_replay_pending()`，不会再等待 raw STA。

正确性检查：history 只扫描单 UID 的有界 queue，容量上限使用 `MEMBLOCK_DUT_SQ_SIZE`。重复 IQ feedback 只复用相同 identity，队列满时 `uvm_fatal`，不淘汰旧记录，从而不会静默丢失迟到 fault。

## 4. Fault 身份还原与唯一写者

修改前所有 STA raw 直接严格 attach current snapshot；IQ failed 后 current STA 会被 replay 清除，UID25 的迟到 fault 因而报 `INT_WB_ATTACH`。修改后 adapter 对 STA exception raw 的顺序固定为 terminal duplicate、current probe、tombstone fallback、原 strict attach。normal raw 不进入 tombstone 路径。

抽象功能描述：`convert_raw_int_wb()` 在 batch handler 前为 raw STA 填充完整 UID、ROB、SQ、issue epoch 与 replay sequence；它只选择事件身份，不写 fault 状态。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv`，`convert_raw_int_wb()` 的 STA 分支。

```systemverilog
if (wb_event.has_exception) begin
    sta_fault_attached = try_attach_existing_sta_fault_snapshot(wb_event, raw.sample_flush_epoch);
    if (!sta_fault_attached)
        sta_fault_attached = try_attach_current_issue_snapshot(wb_event, 1'b1, raw.sample_flush_epoch);
    if (!sta_fault_attached)
        sta_fault_attached = attach_sta_late_fault_snapshot(wb_event, raw.sample_flush_epoch);
end
if (!sta_fault_attached)
    attach_current_issue_snapshot(wb_event, 1'b0, 1'b1, raw.sample_flush_epoch);
```
中文伪代码：异常 raw 先判断同 active UID 是否已经 terminal，命中时只补齐事件给下游幂等丢弃；否则优先尝试当前发射 identity，防止旧 history 抢占新实例 fault。仅当前 identity 不存在时查询同 UID tombstone；全部未命中时回到原严格 attach 并报可诊断 fatal。非异常 raw 从一开始就只走严格 current attach。

抽象功能描述：`mark_target_fault()` 是对外唯一写 fault 的入口；其 tombstone 子分支在 identity 已还原后撤销同 UID 的待发射工作并锁定异常结果，不负责 retire active map。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`，`mark_target_fault()` 与 `mark_sta_late_fault_from_tombstone()`。

```systemverilog
if (status.fault || status.exception_pending || status.terminal_done) begin
    return 1'b0;
end
if (target == MEMBLOCK_ISSUE_TARGET_STA &&
    mark_sta_late_fault_from_tombstone(uid, issue_epoch, replay_seq,
                                       exception_vec, cycle)) begin
    return 1'b1;
end
```
中文伪代码：先检查 UID 是否已经终态；重复 fault 只记录信息并保持原 exception vector。未终态的 STA fault 再尝试匹配 tombstone，命中后清 issue queue、PTW wait、replay mask 与 history，并写入一次 terminal fault；未命中才走原有 current conditional setter。这样 fault 的唯一写者和重复事件边界保持明确。

正确性检查：tombstone lookup 需要 active ROB map 命中，且 record 的 dynamic epoch、ROB、SQ 与当前 status 一致，raw sample flush epoch 不早于记录 epoch。redirect、status reset、current/late fault 与 active retire 都清 tombstone，防止 ROB/SQ reuse。

## 5. Replay 与 PTW 生命周期

修改前旧 drain 等待 raw；初版 tombstone 方案虽然清除了已存在 PTW wait item，但没有阻止同 batch 的 stale replay 在 fault 后再次进入 PTW queue。该队列会参与 `runtime_drain_complete()`，在 TLB 不 ready 且 timeout 为零时可能再次卡住。

抽象功能描述：`mark_replay_pending()` 只把仍属于当前动态实例的 replay 转为 reissue；它不负责从 PTW queue 入队。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`，`mark_replay_pending()`。

```systemverilog
if (!status.active || status.issue_killed || status.fault ||
    status.exception_pending || status.terminal_done ||
    !target_dispatched(status, target) ||
    status.get_target_issue_epoch(target) != issue_epoch ||
    !target_replay_seq_match(status, target, replay_seq)) begin
    return 1'b0;
end
```
中文伪代码：函数先确认 UID 仍 active、非 terminal，target 仍是同一发射，且 issue/replay generation 相同；任何 fault 后或 stale event 都直接返回，不再改变 issue queue 或 replay mask。

抽象功能描述：`push_ptw_wait_replay()` 是 PTW wait queue 的唯一写入口；它在入队前验证 replay 仍属于当前 dynamic instance，从源头拒绝 fault 后到达的 stale wait。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`，`push_ptw_wait_replay()`。

```systemverilog
if (!status.active || status.issue_killed || status.fault ||
    status.exception_pending || status.terminal_done ||
    status.redirect_pending || status.flushed ||
    !target_dispatched(status, target) ||
    status.get_target_issue_epoch(target) != issue_epoch ||
    !target_replay_seq_match(status, target, replay_seq)) begin
    return;
end
```
中文伪代码：PTW wait 入队前读取该 UID 当前 status。只要 UID 已 fault、terminal、redirect/flush，或者 target、issue epoch、replay sequence 与当前实例不一致，就记录低等级日志并不入队；通过后才执行既有去重和入队。此检查为 O(1) status 读取，不扫描主表或全局事件队列。

正确性检查：late/current STA fault 仍调用 `release_ptw_wait_replay(uid)` 删除已经存在的等待项；上面的入口 guard 阻止随后同 batch feedback 反向插回，从而令 `ptw_wait_replay_q.size()==0` 的终态条件可达。

## 6. Plan 对齐检查

对应 plan：`AI_DOC/plan/test_framework/plan/do/memblock_sta_replay_late_fault_tombstone_coding_plan_20260828.md`。

### 6.1 实现与 Plan 不一致项

未发现实现与 Plan 不一致项；current-first、fault-only history、bounded queue、terminal duplicate、PTW cleanup、fault replay guard 与 cleanup 生命周期均已按 plan 落实。

### 6.2 Plan 未说明但 Coding 落实的细节

独立复审发现 PTW stale replay 可在 fault 清队后重新入队，因此在 `push_ptw_wait_replay()` 增加 O(1) identity guard，并已回写正式 plan。该细节是避免同 batch 事件顺序导致 `runtime_drain_complete()` 永远为假的必要闭环，不改变普通 PTW replay 主路径。

## 7. 验证与风险

已完成检查：

| 检查 | 结果 |
| --- | --- |
| `git diff --check`（本功能源码和 plan） | 通过 |
| `rg "sta_replay_drain"`（SV/SVH） | 无残留 |
| 独立 plan review | 通过，无必须修改项 |
| 远端 `make eda_compile` | 通过，VCS `0 error(s), 0 warning(s)` |

待执行验证：使用 `basicTest`、`memblock_dispatch_real_smoke_vseq`、`tc_dispatch_real_mmu_sv39_smoke`、seed `666666` 运行 10000 笔 Sv39/U 态回归。通过标准为 `TEST CASE PASSED`、`UVM_ERROR=0`、`UVM_FATAL=0`，且 `terminal_done_uid=10000`。

剩余风险仅限 raw STA 接口不携带 generation 的固有限制：若后续出现 exception raw 无 active ROB、无 current snapshot 且无兼容 tombstone，必须保留日志和波形并按用户要求先做独立 RTL review；本次实现不修改 RTL，也不把异常 raw 静默丢弃。
