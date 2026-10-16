# MemBlock Load MMIO Redirect R+2 旧 Owner 修复实现 Review

评审日期：2026-10-12

关联 plan：`AI_DOC/plan/test_framework/plan/do/memblock_load_mmio_redirect_r2_stale_owner_fix_plan_20261012.md`

评审结论：通过，无 coding blocker。定向测试和正常real-smoke通过；真实失败seed被独立问题1在问题2事件前阻断，日志中未出现`MMIO_RESOLVE`，不能把该中止运行宣称为完整seed pass。

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| `loadMmio` | DUT说明某条scalar Load属于MMIO的旁带脉冲，不是Load数据写回 | `dispatch_raw_ctrl_t.load_mmio_valid[]` | redirect在sample R出现，旧请求脉冲可在R+2被monitor看到 |
| `active map` | 只保存当前有效动态实例的ROB到uid快速映射 | `uid_by_active_rob` | 普通raw分别probe flag 0/1，不扫描主表 |
| `redirect anchor` | monitor采到的外部redirect时序锚点 | `redirect_sample_seq`、`redirect_anchor_history_q` | R、R+1、R+2均相对该sample计算 |
| `redirect-deleted owner` | active map删除前保存的单代短时旧实例身份 | `redirect_deleted_lq_owner_by_key` | R+2时证明旧Load确实由同一redirect删除 |
| `activation epoch` | 某动态实例进入active状态时看到的flush版本 | `active_instance_flush_epoch`、`old_activation_flush_epoch` | 旧实例值必须严格小于本次redirect epoch |
| `dynamic epoch` | 同一uid每次redirect重发后的动态版本号 | `status.dynamic_epoch`、`old_dynamic_epoch` | 删除前旧值加1必须等于flush后status值 |
| `historical proof` | 用主表/status和deleted-owner联合得到的旧归属证据 | `find_unique_redirect_deleted_load_mmio_owner()` | 只有唯一候选时允许stale-drop |
| `stale-drop` | 确认raw属于已杀旧实例后丢弃raw且不写MMIO tag | `MEMBLOCK_MMIO_RESOLVE_STALE_DROP` | R+2唯一旧Load成功路径 |
| `slow path` | 仅精确R+2、无active owner时运行的全表证明路径 | `find_unique_redirect_deleted_load_mmio_owner()` | 正常高频MMIO raw不会进入 |
| `wrap ambiguity` | 同一ROB value的flag 0和flag 1都存在历史候选 | 完整`memblock_rob_key_t` | 两个候选必须fatal，不能猜测 |
| `expected fatal catcher` | 定向测试临时捕获且匹配指定fatal的callback | `memblock_pending_mmio_expected_fatal_catcher` | 验证R+3等拒绝边界且不污染最终UVM统计 |

本轮修改的抽象边界是：resolver只判断raw应写当前owner、应作为已证明旧raw丢弃，还是因归属不唯一而fatal；它不改变RTL、monitor epoch、redirect状态机、LSQ资源和MMIO tag协议。

## 2. Review 范围与修改前问题

源码范围：

- `common_data_transaction.sv`：保留前序诊断扫描，扩展Load overlap并增加R+2历史证明。
- `dispatch_monitor_event_adapter.sv`：保留`raw.cycle`诊断传参；CURRENT才进入tag staging，STALE_DROP不写tag。
- `memblock_dispatch_types.sv`：给既有单代deleted-owner补充删除前activation epoch。
- `soft_test_memblock_pending_mmio_directed_sequence.sv`：增加成功和拒绝边界定向覆盖。

修改前resolver只认可R和R+1与redirect重叠。真实问题中，redirect sample为R时active owner已在flush阶段删除，而旧`loadMmio`在R+2才出现；raw记录的是脉冲出现时的新flush epoch，因此既不能从active map找到owner，也不能按旧epoch规则丢弃，最终触发`MMIO_RESOLVE` fatal。

仅把窗口改成R+2并静默丢弃不正确，因为同一个ROB value可能已被新实例复用，也可能存在flag 0/1歧义或根本没有历史Load。实现必须证明“恰好一个、由同一次redirect删除、redirect前已激活且真实issue过的scalar Load”。

## 3. deleted-owner 证明字段

功能特性：复用现有单代deleted-owner窗口，并补足旧实例activation epoch，不创建第二套长期owner真源。

修改前逻辑：结构只保存uid、旧dynamic epoch、redirect epoch和record id；flush后status已清掉active activation epoch，R+2时无法再次证明旧实例在redirect前激活。

修改后逻辑：active map删除前保存`old_activation_flush_epoch`；捕获入口要求该值有效且严格小于redirect epoch，slow path再次校验。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv`，结构：`memblock_redirect_deleted_owner_t`。

抽象功能描述：该结构保存一次redirect正在清理的旧LQ/SQ owner身份，供迟到事件在active map已删除后做短时归属；它不延长owner生命周期，也不重建LSQ状态。

```systemverilog
typedef struct {
    bit            valid;
    memblock_uid_t uid;
    int unsigned   old_dynamic_epoch;
    int unsigned   old_activation_flush_epoch;
    int unsigned   redirect_epoch;
    int unsigned   cancel_record_id;
} memblock_redirect_deleted_owner_t;
```

中文伪代码：本结构在redirect扫描删除owner前被填充；`old_dynamic_epoch`标识被删动态实例版本，`old_activation_flush_epoch`保存它进入active时的flush版本，`redirect_epoch/cancel_record_id`绑定删除它的唯一redirect记录。窗口沿用既有清理逻辑，字段本身不会创建新状态生命周期。

源码位置：`common_data_transaction.sv`，函数：`capture_redirect_deleted_owner()`。

抽象功能描述：该函数在redirect active UID扫描阶段、active map删除前冻结旧owner身份；输入是uid和redirect epoch，副作用只写既有deleted-owner短时表。

```systemverilog
status = get_status(uid);
if (!status.active || !status.active_instance_flush_epoch_valid ||
    status.active_instance_flush_epoch >= redirect_epoch) begin
    `uvm_fatal("REDIRECT_DELETED_OWNER", ...)
end
owner.old_dynamic_epoch = status.dynamic_epoch;
owner.old_activation_flush_epoch = status.active_instance_flush_epoch;
owner.redirect_epoch = redirect_epoch;
owner.cancel_record_id = cancel_record_q[record_idx].cancel_record_id;
```

中文伪代码：先读取待删除uid的当前status；若它已经不active、没有activation provenance，或activation epoch不早于redirect，则立即fatal，禁止生成不可信历史。通过后把删除前dynamic epoch、activation epoch、本次redirect epoch和cancel record id写入同一短时owner记录，随后既有逻辑按LQ/SQ完整key登记该记录。

正确性检查：字段由删除前唯一active实例产生；读取时必须与同一record匹配；窗口清理条件未改变，因此不是新的长期deleted-owner map。

## 4. R+2 历史证明 Helper

功能特性：只在精确R+2且active owner为零时，从现有主表/status和单代deleted-owner中证明唯一旧Load。

修改前逻辑：没有R+2 slow path；active owner删除后直接fatal。

修改后逻辑：先验证deleted-owner窗口和cancel record，再扫描主表；候选必须是scalar Load、完整ROB key被redirect覆盖、当前inactive/flushed/killed/redirect-pending、曾真实issue，并与deleted LQ owner的uid、dynamic epoch、activation epoch、redirect epoch和record id完全一致。

源码位置：`common_data_transaction.sv`，函数：`find_unique_redirect_deleted_load_mmio_owner()`。

抽象功能描述：该只读helper消费raw ROB value和唯一overlap redirect provenance，返回是否存在恰好一个可信旧Load及其uid/完整ROB key；它不写tag、map、queue或redirect状态。

```systemverilog
if (!overlap_redirect.valid || overlap_redirect_epoch == 0 ||
    overlap_cancel_record_id == 0 ||
    !redirect_deleted_owner_window_active ||
    redirect_deleted_owner_epoch != overlap_redirect_epoch ||
    redirect_deleted_owner_record_id != overlap_cancel_record_id) begin
    proof_detail = $sformatf("redirect-deleted window mismatch ...");
    return 1'b0;
end
record_idx = find_cancel_record_index_by_id(overlap_cancel_record_id);
if (record_idx < 0 || !cancel_record_q[record_idx].valid ||
    !cancel_record_q[record_idx].redirect_anchor_valid ||
    !cancel_record_q[record_idx].active_scan_done ||
    !redirect_payload_equal(cancel_record_q[record_idx].redirect,
                            overlap_redirect)) begin
    return 1'b0;
end
```

中文伪代码：先要求当前deleted-owner窗口确实属于传入的redirect epoch和record id；不一致时返回失败原因。然后按record id定位cancel record，要求记录有效、已绑定monitor anchor、active扫描已完成且payload一致；任一条件失败都返回0，让caller走fatal，不尝试猜测owner。`find_cancel_record_index_by_id()`只负责在有界cancel队列定位记录，不修改队列。

源码位置：`common_data_transaction.sv`，函数：`find_unique_redirect_deleted_load_mmio_owner()`候选扫描。

抽象功能描述：该分支逐uid核对静态操作类型、flush后状态和删除前owner证据，并统计满足全部条件的候选数。

```systemverilog
if (behavior.kind != MEMBLOCK_OP_BEHAVIOR_LOAD ||
    !behavior.commit_is_load || !behavior.uses_lq ||
    status.active || !status.flushed || !status.issue_killed ||
    !status.redirect_pending || status.load_dispatched ||
    status.load_issue_epoch == 0 || status.dynamic_epoch == 0 ||
    status.active_instance_flush_epoch_valid) begin
    continue;
end
candidate_rob_key = status.get_rob_key();
if (candidate_rob_key != main_tr.get_rob_key() ||
    !rob_order_util::rob_need_flush(candidate_rob_key, overlap_redirect)) begin
    continue;
end
if (!lookup_redirect_deleted_lq_owner(candidate_lq_key, deleted_owner) ||
    deleted_owner.uid != candidate_uid ||
    deleted_owner.old_dynamic_epoch + 1 != status.dynamic_epoch ||
    deleted_owner.old_activation_flush_epoch >= overlap_redirect_epoch ||
    deleted_owner.redirect_epoch != overlap_redirect_epoch ||
    deleted_owner.cancel_record_id != overlap_cancel_record_id) begin
    continue;
end
matched_count++;
return matched_count == 1;
```

中文伪代码：按uid扫描现有主表和status；先过滤ROB value不同、非scalar Load、仍active、未flush、未kill、未进入redirect重发、未真实issue或flush后仍保留active provenance的记录。再要求status和主表的完整ROB key一致且确实被本次redirect覆盖。用status保留的完整LQ key查询既有deleted-owner；要求uid一致、旧dynamic epoch加1等于当前重发版本、旧activation epoch早于redirect、redirect epoch和record id一致。每个全条件候选加一，最终只有计数等于1才成功；0或大于1都返回失败，由caller fatal。

正确性检查：新active owner会使slow path入口不成立；flag 0/1两个历史候选使计数为2；Store或prefetch不满足Load行为；更早redirect记录不匹配epoch/record；R+3不会调用该helper。

## 5. Resolver 时序窗口与决策顺序

功能特性：Load overlap上界从R+1扩到R+2，但R+2本身不是stale-drop充分条件。

修改前逻辑：只接受`sample==R`或`sample==R+1`；R+2落入普通owner解析。

修改后逻辑：有界record/anchor扫描接受`R..R+2`；仍先probe两个active ROB key。R/R+1和仍存在旧active owner时沿用原规则；仅精确R+2且active计数为0时调用历史helper。

源码位置：`common_data_transaction.sv`，函数：`resolve_mmio_uid_by_rob_value()`的overlap发现。

抽象功能描述：resolver从raw sample provenance中选择唯一重叠redirect，再解析当前active owner；它返回CURRENT、STALE_DROP或触发fatal。

```systemverilog
if (!cancel_record_q[record_probe_idx].valid ||
    !cancel_record_q[record_probe_idx].redirect_anchor_valid ||
    raw_sample_seq < cancel_record_q[record_probe_idx].redirect_sample_seq ||
    raw_sample_seq > cancel_record_q[record_probe_idx].redirect_sample_seq +
        MEMBLOCK_LOAD_MMIO_REDIRECT_MAX_SAMPLE_OFFSET) begin
    continue;
end
overlap_redirect_match_count++;
if (overlap_redirect_match_count > 1) begin
    `uvm_fatal("MMIO_RESOLVE", ...)
end
```

中文伪代码：仅对Load扫描未完成redirect记录；sample不在R到R+2闭区间就跳过。每命中一个记录就增加overlap计数，超过一个立即fatal，避免同一raw被两次redirect解释。唯一命中时保存redirect payload、epoch、record id和anchor sample；未绑定anchor走同样边界并按既有FIFO规则与record配对。

源码位置：`common_data_transaction.sv`，函数：`resolve_mmio_uid_by_rob_value()`的R+2分支。

抽象功能描述：该分支把严格历史证明接入resolver决策树；成功只返回STALE_DROP，失败必须fatal。

```systemverilog
if (raw_sample_seq == overlap_redirect_sample_seq +
                          MEMBLOCK_LOAD_MMIO_REDIRECT_MAX_SAMPLE_OFFSET &&
    active_candidate_count == 0) begin
    if (find_unique_redirect_deleted_load_mmio_owner(...)) begin
        uid = deleted_uid;
        stale_reason = $sformatf("loadMmio sample=%0d is R+2 ...", raw_sample_seq);
        return MEMBLOCK_MMIO_RESOLVE_STALE_DROP;
    end
    stale_reason = $sformatf("cannot prove LOAD MMIO stale ownership at R+2 ...");
    `uvm_fatal("MMIO_RESOLVE", stale_reason)
end
```

中文伪代码：在overlap分支中先保留原有“唯一旧active Load且被redirect覆盖”的stale-drop。若未满足，再检查raw是否精确等于R+2且两个wrap key都没有active owner；满足时调用历史helper。helper唯一命中则返回STALE_DROP且不写tag；失败则把证明原因写入`stale_reason`并fatal。存在任何active owner、R/R+1证明失败或其它不兼容情况都走原overlap fatal；R+3不属于overlap，走普通解析并在无owner时fatal。

性能检查：正常raw仍只probe两个active key；新增全表扫描只出现在精确R+2、唯一redirect overlap且active owner为零的异常slow path。

## 6. Adapter 与诊断保留

源码位置：`dispatch_monitor_event_adapter.sv`，函数：`apply_raw_ctrl_mmio_tags()`。

抽象功能描述：adapter把monitor raw交给resolver，并只对CURRENT结果暂存tag；STALE_DROP只打印原因。它不决定owner证明规则。

```systemverilog
result = data.resolve_mmio_uid_by_rob_value(
    raw.load_mmio_rob_value[port], MEMBLOCK_MMIO_KIND_LOAD,
    raw.mmio_flush_epoch, raw.mmio_sample_seq, raw.cycle,
    uid, stale_reason);
if (result == MEMBLOCK_MMIO_RESOLVE_STALE_DROP) begin
    `uvm_info("MMIO_RAW", $sformatf("drop stale loadMmio ... reason=%s",
                                     stale_reason), UVM_LOW)
end else begin
    staged_tags.push_back(stage);
end
```

中文伪代码：把raw携带的ROB value、采样flush epoch、sample序号和cycle传给resolver。resolver返回STALE_DROP时只记录原因，不加入`staged_tags`；返回CURRENT时才检查同raw的Load/Store冲突并暂存tag，后续统一提交。Store继续调用同一resolver，但R+2窗口只在expected kind为Load时建立，因此Store行为未扩展。

前序诊断修改保留：最终普通owner解析fatal前调用`dump_mmio_resolve_failure_context()`，使用`raw.cycle`打印active map、redirect record、anchor和同ROB value的status候选。该扫描只在最终fatal路径，不进入成功高频路径，也不参与判定。

## 7. 定向测试实现与覆盖

抽象功能描述：`prepare_redirect_deleted_owner_window()`通过正式reservation、redirect drive、monitor anchor、cancel watermark和`apply_redirect_flush()`建立生产路径状态；测试不直接写active map、status内部字段、deleted-owner map或cancel record。

源码位置：`soft_test_memblock_pending_mmio_directed_sequence.sv`，task：`prepare_redirect_deleted_owner_window()`。

```systemverilog
launch_epoch = data.begin_lsq_reservation_launch(victim_uids[idx]);
data.mark_lsq_reservation_sampled(victim_uids[idx], launch_epoch,
                                  reservation_sample_seq);
establish_redirect_anchor(redirect, scenario_name, anchor_sample_seq);
while (data.latest_drained_cancel_sample_seq < anchor_sample_seq + 1) begin
    monitor_adapter.drain_lsq_timing_sidebands();
    monitor_adapter.service_lsq_timing_reconcile();
end
data.apply_redirect_flush(redirect);
if (!data.redirect_deleted_owner_window_pending()) begin
    `uvm_fatal(get_type_name(), ...)
end
```

中文伪代码：对每个victim调用公开reservation launch和sample API，证明它已对DUT可见；调用`establish_redirect_anchor()`经正式redirect request/drive完成和raw anchor队列建立record绑定；等待R+1 cancel watermark被adapter drain/reconcile；确认redirect扫描可执行后调用正式flush入口。若生产代码没有建立deleted-owner窗口则测试立即fatal，禁止用测试代码伪造窗口。

覆盖结果：

| 场景 | 预期 | 已验证结果 |
|---|---|---|
| 普通active Load | CURRENT并写MMIO tag | 通过 |
| R、R+1旧active Load | 保持原stale-drop | 通过 |
| R+2唯一旧Load | 历史证明成功、无tag | 通过 |
| R+3旧Load | `MMIO_RESOLVE` fatal | expected fatal已捕获 |
| R+2无历史owner | fatal | expected fatal已捕获 |
| R+2有redirect后新active owner | fatal且不写新tag | expected fatal已捕获 |
| R+2 flag 0/1两个历史候选 | fatal | expected fatal已捕获 |
| R+2历史对象为Store但raw为Load | fatal | expected fatal已捕获 |

测试数据固定为两个主表entry。该执行期调整避免同一个sequence中RM初始化后`main_trans_num`变化；单目标场景使用uid1，redirect只覆盖uid1，不改变验证语义。

## 8. Plan 对齐与执行期差异

与执行前原始plan一致的部分：只改测试框架；不改RTL和monitor epoch；正常active owner仍写tag；R+2只在唯一历史证明后stale-drop；R+3、无历史、多候选、新owner和非Load均fatal；高频路径不全表扫描；不新增长期owner map。

### 8.1 与 plan 不一致的实现

执行前plan正文没有明确要求给deleted-owner结构增加字段，实际实现增加`old_activation_flush_epoch`。原因是flush后的status已清除旧activation provenance；字段必须在删除前冻结，才能满足plan要求的“activation epoch早于redirect epoch”。该差异已按`IMPLEMENTATION_DELTA`记录，生命周期和清理边界不变，评审认为必要且无额外owner真源风险。

### 8.2 plan 未说明但实现补充的细节

定向sequence统一使用两个主表entry。首次运行中单entry场景和双entry wrap场景切换触发RM的主事务数量不变合同，因此改为固定规模。该调整只影响测试构造，已按`IMPLEMENTATION_DELTA`记录。

此外，前序诊断代码把`raw.cycle`传给resolver并在最终fatal前打印全表上下文。本修复保留该改动，诊断信息不参与R+2成功判定，也不改变采样epoch。

## 9. 验证结果

静态检查：`git diff --check`通过。

VCS编译：

```text
make eda_compile tc=basicTest ts=memblock_pending_mmio_directed_vseq \
  cfg=default mode=fault2_mmio_r2_fix_20261012 seed=666666
```

结果：编译、elaboration和link通过，0 error。后续增量运行遇到生成数据库`tdc.sdb is corrupted`，未删除产物，改用新mode重新完整构建。

最终定向运行：

```text
make eda_run tc=basicTest ts=memblock_pending_mmio_directed_vseq \
  cfg=default mode=fault2_mmio_r2_fix_v2_20261012 seed=666666
```

结果：`TEST_PASS`，`UVM_ERROR=0`，未捕获`UVM_FATAL=0`，5个预期`MMIO_RESOLVE` fatal均由catcher捕获。R+2正向日志明确显示`sample=22`是redirect `sample=20`的R+2，并唯一匹配`uid=1 ROB=0/1 epoch=1 record=1`。

日志：`mem_ut/ver/ut/memblock/sim/fault2_mmio_r2_fix_v2_20261012/log/tc=basicTest_ts=memblock_pending_mmio_directed_vseq_cfg=default_seed=666666_rtl.log`。

真实失败seed `37617171`：VCS编译、elaboration和link通过并开始仿真。自`295.900ns`起，日志持续出现独立问题1：
`LSQ_COMMIT hold pendingPtr before scalar fault dual real-WB uid=0 ... sta_wb=0 std_wb=0`。该状态一直持续到
手动停止前的`566245.900ns`；运行配置的UVM timeout为`100000000ns`，问题1已在问题2事件产生前阻断主流程。
截至停止点，日志中`MMIO_RESOLVE`出现次数为0。因此可确认本次运行没有复现原问题2fatal，但由于问题1提前
阻断，不能宣称该真实seed完整通过，也不能用它单独证明后续所有问题2时序均已执行。

日志：`mem_ut/ver/ut/memblock/sim/fault2_mmio_r2_fix_v2_20261012/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=37617171_rtl.log`。

正常real-smoke seed `666666`：旧mode再次增量编译时遇到VCS生成文件`tdc.sdb is corrupted`；未删除已有产物，
改用新mode `fault2_mmio_r2_fix_smoke_20261012`完成全新编译和仿真。结果为`TEST_PASS`、`UVM_ERROR=0`、
`UVM_FATAL=0`，在`1752.800ns`自然结束。

日志：`mem_ut/ver/ut/memblock/sim/fault2_mmio_r2_fix_smoke_20261012/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_smoke_seed=666666_rtl.log`。

## 10. 最终 Review 结论

代码判定顺序满足安全要求：active owner优先；只有精确R+2且无active owner才读历史；历史必须唯一绑定同一redirect record且证明为redirect前已激活、已issue、当前被kill的scalar Load。实现没有全局忽略`loadMmio`，没有把R+2时间条件当作充分条件，没有扩展Store行为，也没有新增长期deleted-owner状态。

源码、定向验证和正常real-smoke未发现blocker。真实失败seed的未完成原因已明确归属问题1，不属于本次
MMIO resolver修改；问题2修复可以归档，但待问题1修复后仍应再次完整复跑seed `37617171`。
