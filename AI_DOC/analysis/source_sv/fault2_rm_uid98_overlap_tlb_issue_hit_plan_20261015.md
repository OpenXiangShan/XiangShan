# 问题2后续：UID98重叠TLB issue-hit修复方案

日期：2026-10-15

## 1. 现象

真实seed `37617171`中，UID98最终没有发出新的L2TLB request，DUT从L1 TLB得到
正常翻译并返回数据，exception为0。RM却冻结历史`token=0 / sample=108 /
generation=1`的精确4KB response；该项的PTE/权限语义会导出Load Page Fault
`0x2000`，但其显式`has_effective_fault()`为0。

同一VPN此前还命中过`token=5 / sample=102 / generation=2`的正常大页response。
因此该失败不是PMA或DUT异常丢失，而是RM在重叠TLB历史中选错了payload。

## 2. 根因

DUT L1 TLB允许精确4KB项和更大页项同时raw-hit，读路径会处理重叠命中。测试框架
`find_completed_tlb_response_for_issue()`却在精确key存在时立即返回，不把精确项与
range候选放入统一覆盖范围仲裁。UID98因此长期选择精确faulting项，忽略正常大页项。

`uid_tlb_record`中的issue-hit snapshot能够证明“本次issue没有新request且至少有一条
历史response可命中”，但原先单条snapshot不足以表示重叠候选。

## 3. 修复

1. 仅在RM readonly API的no-request-fire路径中枚举completed-response range index。
2. 只接受`response_sample <= uid_wait_start_sample`的候选，禁止读取issue之后的response。
3. 把冻结snapshot作为兜底候选，避免同anchor后来被覆盖后丢失issue时证据。
4. 对全部raw-match候选复用已有coverage rank，唯一选择覆盖范围最大的entry。
5. 用选中entry重新计算request-specific PPN/GVPN；真实request-completed路径保持不变。
6. 最大rank不唯一时返回不可解析，不根据DUT actual exception反推expected。

## 4. Directed验收

构造与UID98一致的场景：正常2MB response先完成，精确4KB PF response后完成，随后
UID issue但不产生新L2TLB request。先确认旧snapshot确实选择精确PF，再确认RM resolver
选择issue时可见的正常大页、`range_hit=1`且translation-derived字段有效。

随后使用新唯一mode完成编译、RM provenance directed、RM ledger/NaN-box directed，
最后以原始cfg和seed `37617171`自然跑完，要求UID98通过且不回归其余RM检查。

## 5. 最终验证结果

唯一mode `fault2_rm_fp_nanbox_uid98_20261015_c`完成编译，未出现VCS compile error。

Provenance directed日志：

```text
mem_ut/ver/ut/memblock/sim/fault2_rm_fp_nanbox_uid98_20261015_c/log/
tc=basicTest_ts=memblock_rm_provenance_followup_directed_vseq_cfg=tc_dispatch_real_smoke_seed=777777_rtl_rm_uid98_overlap_directed_20261015_c.log
```

该测试构造两个raw-hit候选，旧snapshot冻结精确4KB faulting项，新resolver唯一选择
覆盖更宽的正常项；仿真自然结束于`200.400ns`，`UVM_ERROR=0`、`UVM_FATAL=0`。

真实seed日志：

```text
mem_ut/ver/ut/memblock/sim/fault2_rm_fp_nanbox_uid98_20261015_c/log/
tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=37617171_rtl_rm_uid98_overlap_real_seed37617171_20261015_c.log
```

UID98的最终仲裁为`candidates=2`、`selected_token=2`、`selected_sample=110`、
`selected_generation=2`、`selected_rank=3`；冻结项为`token=0/generation=1`。
RM据此得到PA `0x0cd69546`和exception 0，data与DUT均为
`0xffffffffffffa72b`，比较PASS。完整真实seed自然结束于`3282013.400ns`，
`RM_LS_TRACE_COMPARE=10000`、`Quit count=0/10`、`UVM_ERROR=0`、`UVM_FATAL=0`。

自review确认：修复仅作用于no-request-fire issue-hit路径；request-completed路径和
跨页fallback保持原行为。候选只允许使用issue边界前的response，最大rank不唯一时
fail closed，不读取DUT actual来决定expected。
