# STA coalesced feedback replay 生命周期修复实现 Review（2026-10-19）

| 项目 | 内容 |
| --- | --- |
| Review 状态 | 源码、专项测试和原失败 seed复测通过；100-seed回归进行中 |
| 关联 plan | `AI_DOC/plan/test_framework/plan/undo/memblock_sta_coalesced_feedback_replay_lifecycle_fix_plan_20261019.md` |
| 关联 plan review | `AI_DOC/plan/test_framework/review_doc/undo/memblock_sta_coalesced_feedback_replay_lifecycle_fix_plan_review_20261019.md` |
| 关联缺陷 | `AI_DOC/buglist/rm/v2/rm_buglist_2026-W43_20261019.md` |
| 基线 commit | `8e45befc10bc1ab93d465ebc3ac16a91d6b750c6` |
| 修改范围 | STA failed feedback的tombstone history与replay recovery事件；定向replay smoke |
| 非目标 | DUT RTL、Scala、接口定义、PTW/redirect仲裁、cfg权重和异常频率 |

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
| --- | --- | --- | --- |
| `tombstone` | 为迟到STA fault保留的旧issue身份记录，不代表replay已经完成。 | `sta_late_fault_tombstone_q` | 首次failed保存`issue_epoch=4/replay_seq=0`。 |
| `coalescing` | 同一动态实例、ROB和SQ只保留最早tombstone，限制history容量。 | `capture_sta_late_fault_tombstone()` | 第二轮failed复用首条history，但本轮replay仍需执行。 |
| `recovery event` | 送入公共feedback queue、驱动replay状态推进的事件。 | `push_feedback_event()` / `handle_replay_event()` | 最终调用`mark_replay_pending()`。 |
| `current identity` | 当前发射实例的`issue_epoch + replay_seq`组合。 | `status_transaction` | 第一条事件推进后，同轮重复事件变成stale。 |
| `identity guard` | 只允许当前有效发射实例修改状态的检查。 | `mark_replay_pending()` | 旧epoch或旧replay sequence返回0。 |
| `stable scope` | 同一dynamic epoch、ROB和SQ代表的逻辑请求范围。 | tombstone coalesce条件 | 多轮STA replay仍属于同一stable scope。 |

本次实现的抽象边界是：tombstone helper只管理迟到fault history，writeback handler负责把每条通过
current identity校验的failed feedback交给recovery owner；recovery handler继续独占PTW等待、
redirect-first仲裁和`mark_replay_pending()`调用。

## 2. Review 范围与修改前问题

修改前，`capture_sta_late_fault_tombstone()`返回成功且`created_new=0`时，caller把“history没有新增”
误解为“本轮replay已经存在”，提前返回并跳过`push_feedback_event()`。真实seed中UID0第一次STA
failed正常推进到`replay_seq=1`；第二次failed被history coalesce后没有recovery event，STA一直保持
dispatched且未完成，ROB head无法terminal，最终0 compare。

Review覆盖两个源码修改点：

1. `writeback_status_handler.sv`恢复每轮有效failed feedback的recovery事件。
2. `soft_test_memblock_dispatch_replay_smoke_sequence.sv`补充同轮重复和跨轮coalesce覆盖。

## 3. STA failed feedback恢复recovery事件

功能特性：history是否新增与当前replay是否需要推进是两个独立判定；capture成功即表示当前failed
feedback有效，必须排入recovery queue。

修改前逻辑：`tombstone_created==0`时记录duplicate日志并直接返回，当前事件不入队。

修改后逻辑：capture失败仍按stale丢弃；capture成功后`created_new`只决定日志文本，无论新建还是
coalesce都调用`push_feedback_event()`。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/writeback_status_handler.sv`，函数：
`handle_issue_feedback_event()`。

抽象功能描述：该函数消费monitor已经补齐UID、ROB、SQ、issue epoch和replay sequence的feedback；
它负责验证当前STA failed并排队recovery，不直接修改replay状态。

```systemverilog
if (!data.capture_sta_late_fault_tombstone(uid,
                                           issue_epoch,
                                           replay_seq,
                                           wb_event.cycle,
                                           tombstone_created)) begin
    `uvm_info("WB_STATUS",
              $sformatf("drop stale STA replay feedback uid=%0d issue_epoch=%0d replay_seq=%0d",
                        uid, issue_epoch, replay_seq),
              UVM_LOW)
    return 1'b0;
end
if (!tombstone_created) begin
    `uvm_info("WB_STATUS",
              $sformatf("coalesced STA late-fault tombstone uid=%0d issue_epoch=%0d replay_seq=%0d; enqueue current replay with retained tombstone",
                        uid, issue_epoch, replay_seq), UVM_LOW)
end else begin
    `uvm_info("WB_STATUS",
              $sformatf("capture STA late-fault tombstone uid=%0d issue_epoch=%0d replay_seq=%0d; enqueue replay immediately",
                        uid, issue_epoch, replay_seq),
              UVM_LOW)
end
data.push_feedback_event(wb_event);
return 1'b1;
```

中文伪代码：先调用tombstone helper校验当前feedback身份并尝试保存history；返回失败表示事件已经stale，
打印丢弃日志后返回0。返回成功后，根据`created_new`打印首次capture或history coalesce日志，但两个分支
都继续执行。随后调用`push_feedback_event()`把当前failed事件送入公共recovery queue，并返回已处理。
该函数不调用`mark_replay_pending()`，PTW等待和redirect仲裁仍由原recovery handler执行。

正确性检查：

- 完全重复事件都可入队，但第一条执行`mark_replay_pending()`后清除dispatched并递增`replay_seq`；
  后续重复事件由identity guard返回0，不会重复推进。
- 同stable scope、不同`replay_seq`的failed是新一轮有效失败，capture可以coalesce history，但事件必须入队。
- capture失败的旧epoch、terminal、fault或redirect事件仍不入队。
- 未修改STD、STA hit成功、real writeback、fault落表、PTW wait和redirect-first逻辑。

## 4. 定向sequence覆盖两类coalesce

功能特性：定向测试既验证完全重复feedback不会重复推进，也验证跨replay轮次的coalesced feedback不会
被history逻辑吞掉。

修改前逻辑：sequence只执行一轮failed/replay，无法覆盖本次真实seed的第二轮coalesce。

修改后逻辑：首次发射连续注入两条完全相同failed，统一处理后应只推进一次；第一次replay重新发射后
再注入failed，应推进到第二轮replay；最终拒绝两轮旧writeback并接受当前writeback。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_dispatch_replay_smoke_sequence.sv`，
task：`body()`。

抽象功能描述：该task通过公开的feedback/recovery/issue/writeback接口构造两轮STA replay，检查queue、
identity guard、tombstone history和最终完成状态；它不直接改status内部字段。

```systemverilog
submit_raw_sta_iq_feedback(first_sta_item, 1'b0);
submit_raw_sta_iq_feedback(first_sta_item, 1'b0);
exception_redirect_replay_task();
check_replay_pending_state(1, old_sta_issue_epoch, old_replay_seq);
check_sta_tombstone_history(1, old_sta_issue_epoch, old_replay_seq);

fire_replay_sta_item(1, first_replay_sta_item);
status = data.get_status(1);
first_replay_issue_epoch = status.get_target_issue_epoch(MEMBLOCK_ISSUE_TARGET_STA);
first_replay_seq = status.replay_seq;

submit_raw_sta_iq_feedback(first_replay_sta_item, 1'b0);
exception_redirect_replay_task();
check_replay_pending_state(1, first_replay_issue_epoch, first_replay_seq);
check_sta_tombstone_history(1, old_sta_issue_epoch, old_replay_seq);
```

中文伪代码：首次STA发射后连续提交两条同identity failed，再运行正式recovery task；第一条把UID置为
replay pending并递增sequence，第二条因identity已经变化而被忽略。检查状态只推进一次，history只有
首条记录。随后正式重发STA，读取新issue epoch和replay sequence；提交第二轮failed并再次运行recovery，
确认新identity合法推进，同时history仍保留第一次记录且长度为1。

源码位置：同文件，function：`check_sta_tombstone_history()`。

抽象功能描述：该只读检查确认history有界且保留首个late-fault身份，同时与当前active ROB/SQ映射一致；
它不修改tombstone、map或replay状态。

```systemverilog
if (status.sta_late_fault_tombstone_q.size() != 1) begin
    `uvm_fatal(get_type_name(),
               $sformatf("STA tombstone history size mismatch uid=%0d size=%0d",
                         uid, status.sta_late_fault_tombstone_q.size()))
end
tombstone = status.sta_late_fault_tombstone_q[0];
if (!tombstone.valid ||
    tombstone.issue_epoch != retained_issue_epoch ||
    tombstone.replay_seq != retained_replay_seq ||
    tombstone.dynamic_epoch != status.dynamic_epoch ||
    tombstone.rob_key != status.get_rob_key() ||
    !status.active_sq_mapped ||
    tombstone.sq_key.flag != status.sqIdx_flag ||
    tombstone.sq_key.value != status.sqIdx_value) begin
    `uvm_fatal(get_type_name(), "STA tombstone identity mismatch")
end
```

中文伪代码：读取UID状态并要求tombstone队列长度严格为1；再取唯一记录，依次检查valid、保留的首次
issue epoch/replay sequence、当前dynamic epoch、ROB key和SQ key。任何不一致立即fatal，证明多轮
coalesce没有增长history、改写首条身份或脱离当前active映射。

最终阶段先给第二次重发记录IQ hit，再分别提交第一次和第一次replay的旧writeback，均应被stale guard
拒绝；只有第二次replay的当前writeback完成，最终`replay_seq=old+2`。

## 5. Plan 对齐检查

### 5.1 与 plan 不一致的实现

未发现。实现保持capture失败丢弃、capture成功统一入队、recovery owner不变、重复事件由identity guard
过滤，与执行前plan一致。

### 5.2 Plan 未说明但实现补充的细节

未增加新的生产逻辑。定向检查函数同时核对active ROB/SQ映射，这是测试完整性加强，不改变plan的功能
边界，也不需要`IMPLEMENTATION_DELTA`。

## 6. 验证结果

- `git diff --check`：通过。
- 编译 mode：`fault3_sta_replay_lifecycle_fix_20261019`，VCS编译成功。
- 专项测试：`tc_dispatch_replay_smoke / virtual_base_sequence / seed=666666`通过；
  `UVM_ERROR=0`、`UVM_FATAL=0`、`Quit count=0/10`、正常`$finish @ 1265.300ns`；日志证明
  第二轮coalesced failed推进到`replay_seq=2`。
- 原失败seed：`1646764510`按原cfg完整执行10000请求并通过；`TEST_PASS`、
  `RM_LS_TRACE_COMPARE=10000`、`UVM_ERROR=0`、`UVM_FATAL=0`、`Quit count=0/10`、
  正常`$finish @ 2421373.000ns`。
- 100-seed回归：结果目录
  `mem_ut/ver/ut/memblock/sim/fault3_100seed_sta_replay_fix_8e45befc1_20261019_results`，进行中。

## 7. 当前 Review 结论

生产代码修改是最小职责修复：history coalesce不再吞掉当前recovery事件，所有状态推进仍经过既有
recovery owner和identity guard。专项与原失败seed未发现重复replay、stale writeback误接收或生命周期
残留。当前无源码blocker；待100个唯一seed全部终态后更新最终统计并执行plan/review归档。
