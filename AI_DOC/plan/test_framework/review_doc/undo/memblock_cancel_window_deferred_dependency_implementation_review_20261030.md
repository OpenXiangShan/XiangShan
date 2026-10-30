# Redirect取消窗口延迟出队依赖实现Review（2026-10-30）

状态：Boole于2026-10-30完成最终`FINAL_IMPLEMENTATION_REVIEW_PASS`，主agent复核通过，plan已归档do；随本记录单Bug提交。最终350及母任务尚未完成。
关联方案：`AI_DOC/plan/test_framework/plan/do/memblock_cancel_window_deferred_dependency_formal_plan_20261030.md`。

## 1. 术语与抽象功能说明

| 术语 | 当前含义及落点 | 示例 |
|---|---|---|
| owner / deleted owner | LSQ位置的当前实例及重定向删除前保存的旧实例 | UID1679旧store与record1658的年轻victim不同 |
| epoch / record | 动态或redirect代号、取消记录 | 位置重用不能匹配错误cancel_record_id |
| raw / FIFO | monitor完整事实及顺序等待应用的队列 | SQ等待STD时整条raw保留 |
| head / tail / key | 出队位置、分配位置、包含回绕位的位置 | cancel仅回退tail，不移动head |
| preflight / commit | 无资源释放的联合预检、预检通过后的提交 | LQ成功而SQ失败不能只释放LQ |
| finalize / snapshot / reconcile | 冻结软件取消数、DUT采样、严格对账 | 不能用DUT数覆盖软件期望 |
| sample / anchor | 统一采样序号及真实redirect时序凭证 | compare82232、窗口到期82233 |
| STA / STD / real-WB | store地址、数据、真实写回 | STA fault不能替代STD真实完成 |
| count-only / ptr-next | 仅数量出队、携带出队后位置 | V2 SQ由head顺推，LQ回退count得到start |

本次功能只修复框架生命周期依赖；真实事件、DUT/RTL、cfg、异常模型、深度上限不变。

## 2. 功能与修改前行为

旧窗口收尾用`deferred_raw_ctrl_q.size()!=0`阻止冻结。旧store等待STD使FIFO非空，即使它不影响当前记录的取消量，也会长期阻塞精确目标快照消费，最终第15条触发14上限。
原始波形与只读observer的责任证据保存在`AI_DOC/analysis/framework_design/memblock_cancel_snapshot_depth_seed551576067_20261030.md`；不将独立的STD等待本身直接判正确或RTL无Bug。

## 3. 只读关联查询

抽象功能描述：`lsq_commit_handler::deferred_ctrl_blocks_deleted_owner_window()`在窗口到期、计数尚未冻结时证明FIFO中待应用的出队是否与当前临时owner集合无关；它不拥有资源或事实消费权。
源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv`。

以下摘录门控，证明不在普通每拍路径扫描FIFO。

```systemverilog
ensure_handles();
if (!data.redirect_deleted_owner_window_active ||
    memblock_sync_pkg::deferred_raw_ctrl_q.size() == 0) return 1'b0;
record_idx = data.find_cancel_record_index_by_id(data.redirect_deleted_owner_record_id);
if (record_idx < 0 || !data.cancel_record_q[record_idx].valid) return 1'b1;
if (data.cancel_record_q[record_idx].software_count_finalized ||
    memblock_sync_pkg::peek_current_dut_global_sample() <
        data.redirect_deleted_owner_expire_sample) return 1'b0;
```

中文伪代码：本块决定是否需要关联证明。沿已有handle完成懒绑定；没有窗口或FIFO为空则无依赖，记录不存在时保守阻塞；已冻结或尚未到期无需扫描。`find_cancel_record_index_by_id()`仅查受限取消记录队列；采样getter只读取统一水位。在生产已绑定状态下无业务状态修改；`ensure_handles()`的原有懒绑定和lookup/getter基础合法性fatal保留，不把它们改成静默返回。

以下摘录同一raw的容量及指针能力门禁，证明不借另一个队列容量或旧指针放行。

```systemverilog
raw = memblock_sync_pkg::deferred_raw_ctrl_q[raw_idx];
if (!raw.valid || raw.lq_deq > lq_allocated ||
    raw.sq_deq > sq_allocated || raw.sq_deq > MEMBLOCK_DUT_ENSBUFFER_WIDTH ||
    (raw.sq_deq_ptr_valid && (!MEMBLOCK_DUT_HAS_SQ_DEQ_PTR || raw.sq_deq == 0)) ||
    (MEMBLOCK_DUT_HAS_SQ_DEQ_PTR && raw.sq_deq != 0 && !raw.sq_deq_ptr_valid))
    return 1'b1;
```

中文伪代码：本块对当前raw是否可解释做必要检查；无效raw、累计剩余分配数量不足、SQ宽度非法或指针能力不符均返回有关联/不能证明无关。局部allocated从模型容量减free得到，每条raw后减该条数量，不写真实free；无子函数副作用。

以下摘录LQ连续位置校验及查找，SQ指针能力存在时采用对应的start校验；V2 SQ没有指针，直接沿局部head推进。

```systemverilog
if (raw.lq_deq != 0 &&
    (!data.is_valid_lq_key(lq_ptr) ||
     lq_deq_start_key(lq_ptr, raw.lq_deq, 1'b1) != lq_head)) return 1'b1;
// 以下在LQ逐entry循环中。
if (data.lookup_redirect_deleted_lq_owner(lq_head, owner) ||
    !data.lookup_active_uid_by_lq(lq_head, uid)) return 1'b1;
status = data.get_status(uid);
if (!status.active || !status.active_lq_mapped ||
    status.lqIdx_flag != lq_head.flag || status.lqIdx_value != lq_head.value)
    return 1'b1;
lq_head = lsq_ctrl_model::advance_lq_key(lq_head, 1);
```

中文伪代码：本块解释raw覆盖的LQ位置。非零count要求合法ptr-next并能回退到局部head；逐位置命中当前deleted owner或找不到live owner即阻塞；live status完整位置不一致也阻塞，否则单步推进局部head。`lq_deq_start_key()`回退count解释硬件出队后指针，`lookup_*`只查身份，`advance_lq_key()`处理flag回绕，不释放资源。

以下为相同规则的SQ逐entry分支，证明count-only仍检查完整物理位置。

```systemverilog
if (data.lookup_redirect_deleted_sq_owner(sq_head, owner) ||
    !data.lookup_active_uid_by_sq(sq_head, uid)) begin
    return 1'b1;
end
status = data.get_status(uid);
if (!status.active || !status.active_sq_mapped ||
    status.sqIdx_flag != sq_head.flag || status.sqIdx_value != sq_head.value)
    return 1'b1;
sq_head = lsq_ctrl_model::advance_sq_key(sq_head, 1);
```

中文伪代码：先沿既有绑定取得唯一LSQ模型；无窗口/空FIFO快速返回；记录缺失保守阻塞，保留窗口函数原fatal；未到期或已冻结不扫描。
复制head与已分配容量到局部变量；按FIFO逐raw核对有效性、数量容量和指针能力。LQ按ptr-next回退count核对连续start，V2 SQ直接用局部head。逐entry发现本记录deleted owner或缺少有效live owner则阻塞；否则核对status完整位置并单步推进局部head，扣减局部容量后看下一raw。零deq不移动head，但不停止检查后续raw。
`lookup_redirect_deleted_*_owner()`读取当前window的record/epoch；`lookup_active_uid_by_*()`按map定位live UID，基础map腐坏仍可fatal；`get_status()`读取当前状态；`advance_*_key()`维护回绕位；这些调用不消费FIFO或释放资源。

正确性：相关或不确定一律保守，真出队仍走原联合预检与STD屏障；无关raw可留在FIFO，取消仅作用于独立年轻tail区域。实际扫描复杂度为O(FIFO长度+当前LQ/SQ容量)，只在到期未冻结窗口进行，不扫描历史主表、不复制FIFO或新增缓存。

## 4. 收尾与调用接入

抽象功能描述：`common_data_transaction::service_redirect_deleted_owner_window()`根据当前采样与LSQ查询结论管理取消数冻结、应用后owner清理；它不推导出队位置。
源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`。

```systemverilog
if (!cancel_record_q[record_idx].software_count_finalized &&
    current_sample >= redirect_deleted_owner_expire_sample) begin
    if (deferred_deq_blocks) return;
    cancel_record_q[record_idx].software_count_finalized = 1'b1;
    check_cancel_pending_aggregate();
end
```

中文伪代码：无窗口退出；查当前record并保留原基础合法性fatal；到期后若存在关联依赖则等待，否则冻结并核对aggregate。原software_applied屏障及清理完全保留。
`check_cancel_pending_aggregate()`验证pending等于尚未应用记录之和，不回填；`cleanup_completed_cancel_records()`仍等待应用/观测完成及引用解除。

抽象功能描述：`memblock_main_dispatch_auto_build_main_table_base_sequence::service_monitor_once()`在本拍完整raw应用尝试之后传入只读依赖，不改变后续异常与对账顺序。
源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_auto_build_main_table_base_sequence.sv`。

```systemverilog
data.service_redirect_deleted_owner_window(
    memblock_sync_pkg::peek_current_dut_global_sample(),
    monitor_commit_handler.deferred_ctrl_blocks_deleted_owner_window());
```

中文伪代码：既有timing采集和semantic事件消费结束后，由同一个commit handler只读计算依赖，公共表按当前sample收尾；随后才finalize store fault、处理exception、第二次采集sideband、执行一次严格对账。未重排事件、未增加第二LSQ owner。

## 5. 专项测试与资源闭环

抽象功能描述：`soft_test_memblock_pending_mmio_directed_sequence::run_cancel_window_deferred_dependency_scenario()`在纯软件专项中通过生产admission/reservation/redirect建立窗口，以注入raw验证依赖判断和释放守恒；不在真实DUT flow注入事实。
源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_pending_mmio_directed_sequence.sv`。

第一段为正式出队尝试后的原子拒绝断言，省略号不代表连续调用取消应用。

```systemverilog
monitor_adapter.apply_deferred_ctrl_updates_batch(pending);
if (lsq_ctrl.lq_deq_ptr != lq_head || lsq_ctrl.sq_deq_ptr != sq_head ||
    lsq_ctrl.lq_free_count != MEMBLOCK_LQ_SIZE - 3 ||
    lsq_ctrl.sq_free_count != MEMBLOCK_SQ_SIZE - 3 ||
    lsq_ctrl.lq_enq_ptr != lq_tail || lsq_ctrl.sq_enq_ptr != sq_tail ||
    memblock_sync_pkg::deferred_raw_ctrl_q.size() != 1 ||
    memblock_sync_pkg::deferred_raw_ctrl_q[0] != raw ||
    data.pending_lq_cancel_count != 2 || data.pending_sq_cancel_count != 2 ||
    data.redirect_deleted_lq_owner_by_key.num() != 2 ||
    data.redirect_deleted_sq_owner_by_key.num() != 2) begin
    `uvm_fatal(get_type_name(), "blocked full raw partially released or dependency query mutated state")
end
```

中文伪代码：本块调用生产adapter尝试消费队首，STD尚未真实WB使联合预检失败；之后检查两侧head/tail/free、FIFO原raw、pending和deleted owner都保持原值，否则fatal。adapter内部仍先预检两队列及deleted身份，全部成功才提交并pop，不为软件专项添加半边释放入口。

第二段在相关late-deq已扣减并冻结、或无关分支已直接冻结后才执行，不与上一摘录相邻。

```systemverilog
enq.apply_pending_lsq_cancels();
enq.apply_pending_lsq_cancels();
if (!data.cancel_record_q[record_idx].software_applied ||
    lsq_ctrl.lq_deq_ptr != lq_head || lsq_ctrl.sq_deq_ptr != sq_head ||
    lsq_ctrl.lq_enq_ptr != lsq_ctrl_model::rewind_lq_key(lq_tail, expected_cancel) ||
    lsq_ctrl.sq_enq_ptr != lsq_ctrl_model::rewind_sq_key(sq_tail, expected_cancel) ||
    lsq_ctrl.lq_free_count != MEMBLOCK_LQ_SIZE - (related_deq ? 0 : 1) ||
    lsq_ctrl.sq_free_count != MEMBLOCK_SQ_SIZE - (related_deq ? 0 : 1) ||
    data.pending_lq_cancel_count != 0 || data.pending_sq_cancel_count != 0) begin
    `uvm_fatal(get_type_name(), "cancel did not apply only the remaining tail/free amount exactly once")
end
```

中文伪代码：本块验证生产enqueue应用取消的幂等性。连续调用两次仍只有第一次应用剩余取消量，记录applied为真，head不变，tail按expected_cancel回退、free仅恢复一次、pending为零；`rewind_*_key()`只计算断言期望，不更新资源。随后原task让旧store真实WB完成并经FIFO出队，检查最终满容量，排除cancel和deq重复释放。

中文伪代码：四组合各建6个交替store/load实例，保留最老两个实例、redirect删除四个年轻实例形成每队列两个victim；可选择在末尾位置开始分配以覆盖回绕。
建立旧store缺STD真实WB屏障；注入无关或混合关联完整raw，先证明不发生LQ/SQ半边释放，再解除STD阻塞。关联场景先消费一个deleted owner并同量扣record/pending；无关场景先冻结并只取消年轻tail。调用生产enqueue取消应用两次，证明仅第一次释放剩余量且不推进head；旧store后续正常deq后容量恢复到满。零deq夹层不能遮住后续相关事件。
`apply_deferred_ctrl_updates_batch()`仍是生产FIFO成功后pop的路径；`apply_pending_lsq_cancels()`仍是唯一取消资源应用入口；本task不替代真实seed验收。

已执行初版专项：fresh compile PASS，四组合marker齐全，六项ERROR/FATAL全零，371ns正常结束，controller rc0/unknown0。证据：`sim/mother_cancel_dependency_protocol_20261030_results/protocol_acceptance.json`。
独立review要求补缺owner、错record/epoch、跨raw累计容量及拆开的快速退出负例；这些负例现已在独立fixture补齐，生产2540项输入保持冻结，未最终验收。

执行续记：为不改变在跑原seed的2540项输入，补充负例落到ignored独立fixture派生sequence，生产源码保持不变；r2 fresh compile PASS但运行372ns报`RM_LS_COMMIT main transaction count changed after RM initialization`，controller rc1/unknown0。
该次新fixture从前四场景6条表改成5条表违反既有RM初始化合同；仅改fixture保持6条（3个live SQ、3个deleted LQ），不改RM或屏蔽检查。r2失败保留，不计软件或DUT PASS。
r3已fresh compile及controller rc0 PASS、unknown0，四组合与18个负向只读探针均通过、六项ERROR/FATAL零；`sim/mother_cancel_dependency_protocol_r3_20261030_results/protocol_acceptance.json`保留marker和日志hash。每个探针序列化小结构确认FIFO、map、record/pending、head/tail/free及相关status无查询副作用，临时异常状态均恢复。
Boole独立复核r3日志与2541输入，正式`SOFTWARE_CONTRACT_REVIEW_PASS`，391ns finish；不替代真实seed验收。

## 6. 动态验证状态

原`basicTest / memblock_dispatch_real_smoke_vseq / tc_l2tlb_fault_2pct_no_trigger / seed551576067`，MAIN10000、timeout100000000ns、无临时plusarg：fresh compile及完整10000次RM比较严格PASS，2524588.5ns finish，六项错误0、controller rc0/unknown0。结果`sim/mother_cancel_dependency_original551576067_20261030_results/strict_acceptance.json`，日志SHA256=`594e9ed0e2a5f8d3460109ffa082d85ed86894fef22334ff50d0d94ee271a38e`。
另一独立mode的10项修复专项全部严格PASS，目录`sim/mother_cancel_dependency_focused_20261030_results`：8个Sv39/CSR/PBMT/SFENCE/cancel邻接及原1705742557、1992896882各10000笔；基础virtual及MAIN1真实smoke亦零错误通过，controller全部退出，unknown0。
统一`check_final_gate.py`已得`CANCEL_DEPENDENCY_DYNAMIC_GATE_PASS`：12次真实DUT＋1次空virtual＋四软件组合及18只读负例。2540项核心相对bba3008仅本轮4个SV变化，软件另追加fixture1；两coverage binary均按输入/指纹/hash绑定。Boole独立最终review PASS并核验23项artifact及79项证据hash；随本记录单Bug提交，不代表母350或最终覆盖率完成。

## 7. Plan对齐检查

当前主实现与正式方案一致。独立方案、生产静态、软件契约、readiness及最终实现review均PASS，统一动态门禁PASS；原失败证据保留。

### 实现与Plan不一致项

主生产行为未发现不一致。初版专项负向缺口已经r3补齐且获独立review PASS，三个真实长seed均已严格通过；母350验证仍未执行。

### Plan未说明但Coding落实的细节

`IMPLEMENTATION_DELTA`已记录：专项使用稍后定义的生产enqueue类，因此在原专项include内增加同package类型前置声明，不调整package顺序、不增加运行期对象所有者。最终review必须核对该声明及仅本Bugfixture入口。
补充负例放ignored独立fixture以保持长跑源输入冻结；r2表数量错误仅修fixture，r3重编译通过。恢复脚本在正式review门禁前不执行；readiness独立review已PASS，并绑定全部脚本、文档、日志及二进制证据，提交必须恰含4SV与7份本Bug文档。

## 8. 非本次修改的逻辑分析

四项已有V2知识库脏文件保持不动，不纳入本Bug提交：`AI_DOC/analysis/interface/v2/agents/int_writeback_agent.md`、`AI_DOC/analysis/rtl/v2/flows/rob_compress_and_backend_instruction_flow.md`、`AI_DOC/analysis/rtl/v2/index.md`、未跟踪`AI_DOC/analysis/rtl/v2/flows/backend_memory_writeback_flow.md`。
忽略目录下fixture、编译和仿真证据不作生产代码；本轮文档更新仅对应Bug、方案、实现review、flow及母状态。提交前重新逐项核对git status和diff。
