# MemBlock L2TLB WAITING UID 逻辑扇出修复 Implementation Review

评审日期：2026-10-19

关联plan：`AI_DOC/plan/test_framework/plan/undo/memblock_l2tlb_waiting_uid_logical_fanout_fix_plan_20261019.md`

评审结论：实现与专项、原失败seed复验通过，可以恢复100-seed回归。

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| `physical filter entry` | DUT中保存一个待翻译key的物理槽位 | V2 `PTWNewFilter` | 多个同VPN请求可合并到一个槽位 |
| `logical UID bucket` | 测试框架按`vpn/s2xlate`保存的逻辑等待UID集合 | `uid_waiting_by_vpn_s2xlate` | 第33个同VPN UID仍可登记 |
| `fanout` | 一次翻译response完成多个匹配逻辑UID | `complete_waiting_uid_records_by_response()` | 31个剩余UID同时完成 |
| `prune` | 删除record已失效、完成或取消的悬挂索引 | `prune_uid_waiting_index_bucket()` | 计数前先清理stale handle |
| `active window` | ROB中可同时存在的动态请求上界 | `MEMBLOCK_ROB_SIZE` | 超过160表示框架生命周期泄漏 |

## 2. 容量检查实现

### 2.1 `prune_uid_waiting_index_bucket()`

抽象功能描述：该函数在查询或插入逻辑UID前清除已失效索引，并检查剩余WAITING UID是否超过ROB
活跃窗口；它不完成translation、不修改DUT token，也不取消有效record。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`，函数：
`prune_uid_waiting_index_bucket()`。

```systemverilog
if (uid_waiting_by_vpn_s2xlate[shape_key].size() == 0) begin
    uid_waiting_by_vpn_s2xlate.delete(shape_key);
end else if (uid_waiting_by_vpn_s2xlate[shape_key].size() >
             MEMBLOCK_ROB_SIZE) begin
    `uvm_fatal("COMMON_DATA",
               $sformatf("L2TLB WAITING logical UID bucket exceeds ROB active-window capacity caller=%s size=%0d max=%0d",
                         caller,
                         uid_waiting_by_vpn_s2xlate[shape_key].size(),
                         MEMBLOCK_ROB_SIZE))
end
```

中文伪代码：函数先从后向前删除没有有效WAITING record的索引；若bucket变空则删除整个key；若清理后
仍超过ROB容量则fatal，说明完成/取消路径发生泄漏。旧实现用DFilter的32项物理容量检查逻辑UID，现已替换。

### 2.2 `add_waiting_uid_to_index()`

抽象功能描述：该函数把一个已验证的WAITING record登记到对应shape bucket，并拒绝重复UID或超过逻辑
活跃窗口；它不分配物理filter entry，也不把UID绑定到唯一response token。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`，函数：
`add_waiting_uid_to_index()`。

```systemverilog
if (uid_waiting_by_vpn_s2xlate[shape_key].size() >=
    MEMBLOCK_ROB_SIZE) begin
    `uvm_fatal("COMMON_DATA",
               $sformatf("L2TLB WAITING logical UID capacity overflow uid=%0d size=%0d max=%0d",
                         uid,
                         uid_waiting_by_vpn_s2xlate[shape_key].size(),
                         MEMBLOCK_ROB_SIZE))
end
uid_waiting_by_vpn_s2xlate[shape_key].push_back(uid);
```

中文伪代码：先调用prune清除悬挂索引，再逐项检查当前UID没有重复；已有160个有效逻辑UID时拒绝第161个，
否则追加。第33个同VPN UID不再因物理DFilter容量而fatal。

## 3. 专项实现

抽象功能描述：`check_l2tlb_waiting_uid_logical_fanout()`建立33个同shape WAITING UID，验证登记、部分
取消和response multicast完整闭环；它临时建立最小UID/status数组，结束后恢复software-only vseq原状态。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_rm_provenance_followup_sequence.sv`，task：
`check_l2tlb_waiting_uid_logical_fanout()`。

```systemverilog
for (int unsigned offset = 0; offset < TEST_UID_COUNT; offset++) begin
    uid = TEST_UID_BASE + offset;
    data.init_status_for_uid(uid);
    record = memblock_uid_tlb_record::type_id::create(
        $sformatf("rm_l2tlb_fanout_uid_%0d", uid));
    record.init_context(uid, TEST_VPN, 2'd0, 1'b0, csr,
                        TEST_SAMPLE + offset);
    data.uid_tlb_record_by_uid[uid] = record;
    data.add_waiting_uid_to_index(uid, record);
end
```

中文伪代码：循环创建33个不同UID但相同VPN和`s2xlate`的WAITING record，为每个UID建立最小status，
再通过生产helper登记。第33次调用直接覆盖旧fatal边界。

```systemverilog
record.mark_canceled();
data.remove_waiting_uid_from_index(
    uid, record, "check_l2tlb_waiting_uid_logical_fanout cancel");
...
match_count = data.complete_waiting_uid_records_by_response(entry, csr);
```

中文伪代码：先把2个record转为CANCELED并调用公共remove helper，确认bucket从33缩为31；随后构造匹配
response，调用生产multicast完成其余31个UID并逐个移除索引，最后确认bucket消失并恢复临时数组。

## 4. 正确性与风险检查

1. DFilter的物理pending/request容量和plus参数未修改。
2. response仍使用既有全表raw-hit判定，扩大索引上界不会放宽payload匹配。
3. duplicate UID、record state、record uid和shape一致性检查全部保留。
4. 高频request-fire只扫描同shape bucket，未引入全表扫描。
5. 第160项允许、第161项fatal，索引仍有明确有限上界。
6. 专项首轮因software-only场景`main_trans_num=0`失败；修正仅在task内部临时建立33项数组并恢复，
   未启动dispatch或改变产品逻辑。

## 5. 验证结果

- 编译mode：`fault3_l2tlb_waiting_uid_fanout_fix_20261019`，VCS/KDB 0 error、0 warning。
- 专项：`basicTest + memblock_rm_provenance_followup_directed_vseq + seed=666666`，`TEST_PASS`、
  `UVM_ERROR=0`、`UVM_FATAL=0`、正常`$finish @265.300ns`。
- 原失败seed：`292095580`，原cfg、10000请求，`TEST_PASS`、`RM_LS_TRACE_COMPARE=10000`、
  `UVM_ERROR=0`、`UVM_FATAL=0`、`Quit count=0/10`、正常`$finish @2535895.500ns`。
- 原fatal在`793675.000ns`、第33个同VPN UID登记处；修复版越过该时点并最终完成10000笔。

原seed日志：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/fault3_l2tlb_waiting_uid_fanout_fix_20261019/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=292095580_rtl.log`

## 6. Plan 对齐检查

关联plan：`AI_DOC/plan/test_framework/plan/undo/memblock_l2tlb_waiting_uid_logical_fanout_fix_plan_20261019.md`。

### 实现与 Plan 不一致项

无。容量语义、33项专项、部分cancel、response multicast、原seed复验均按plan完成。

### Plan 未说明但 Coding 落实的细节

software-only vseq没有主表，专项临时保存`main_trans_num/main_table_by_uid/status_by_uid`，建立33项最小数组，
结束后恢复原值。该细节只为调用生产completion API提供status，不改变被测逻辑。
