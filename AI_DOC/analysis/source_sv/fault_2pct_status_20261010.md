# fault_2pct 当前状态

**首次记录时间**：2026-10-10 17:55 CST
**最近更新时间**：2026-10-12 CST
**历史分支基线**：mem_ut_uvm_v2，2026-10-10 核验 commit 为 7a2c1123a
**本轮三 worktree 基线**：`e3916a0cd92346930e4566373f4db2bb68a060a8`
**当前状态**：问题 1 已完成方案 review、coding、编译、首个失败 seed 的专项验证和正常 seed 回归；
问题 2 已在独立 worktree 完成 plan、独立 review、coding、专项测试和正常 seed，真实 seed `37617171`
在到达原 MMIO 事件前被问题 1 的早期阻断终止，因此不能宣称完整真实 seed 通过；问题 3 已完成 approved
SQ 范围的 coding、编译、专项 vseq 和正常 seed。有效 seed `676087581` 已证明 record 3896 的 SQ cancel
mismatch 修复，但在 137ns 后暴露新的独立 LQ owner 冲突，因此完整 seed 尚未通过，问题 3 plan 暂留
`undo`。该 LQ 冲突是完整 seed 后续 blocker，不是 SQ 实现 blocker。

## 配置

- cfg：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/seq/plus_cfg/tc_l2tlb_fault_2pct_no_trigger.cfg`
- 磁盘上的当前值：`MEMBLOCK_PMA_PMP_MODEL_EN=1`
- 每笔仿真：`MEMBLOCK_MAIN_TRANS_NUM=10000`
- pf/af/gpf/gaf 的 raw 权重都是 1
- `MEMBLOCK_CHECK_TRIGGER_EN=0`
- PBMT 权重固定为 0

正在跑的 1000 种子批次是 2026-10-09 约 18:02 启动的，进程号 `246401`。它在启动时已经把 plusarg 读进内存。磁盘上的 cfg 之后才改成 `PMA_PMP_MODEL_EN=1`，所以这批正在跑的仿真仍是旧的 `0`。新起的重跑才是 `1`。

## 1000 种子批次

- 主机：eda01
- 调度进程：`246401`，命令 `bash ./run_fault_2pct_batch.sh`，到 17:55 已运行 23 小时 52 分
- 目录：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/fault_2pct_no_trigger_20261009`
- 种子文件：同目录 `seeds.txt`，1000 个不重复种子
- `status/` 已有最终结论 319 个：PASS 269，FAIL 49，TIMEOUT 1
- 尚未结束约 681 个
- `summary.txt` 仍是更早的手写摘要（pass=15 fail=2），不能当当前结果

判定脚本曾经把 `TEST_PASS` 误标成 FAIL，已经按日志重标并改过 `run_fault_2pct_batch.sh`。当前 `status/` 是修订后的脚本写的。

## 两个失败种子

### 37617171

PMA=1、UVM_HIGH 重跑已结束，错误复现。

- 全量日志：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/fault_2pct_no_trigger_20261009/rerun_pma1/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=37617171_rtl_full.log`
- 汇总：`UVM_ERROR : 2`，`UVM_FATAL : 1`
- 841.9ns：uid 0 是 LHU（`fu_op=0x5`），VA `0x8d34a1cf` 翻成 PA `0xd34a1cf`，2 字节且首地址奇数。模型 `expected_exception=0x0`、`allNormalCacheable=0`、`allPbmtPma=1`。DUT 写回 `exception=0x20`，即 load access fault。
- 原因：`LoadUnit` 对非对齐且 `s2_actually_all_mmio` 的 load 置 bit 5。RM 在 `hd_misalign && allPbmtPma` 时不预测异常。随后 RM 按普通 load 读 backing，地址没有已初始化行，报 query miss。这是预测错误后的连带报错。
- 868.4ns：`MMIO_RESOLVE`，load MMIO，ROB value 132，不是 uid 0 的 ROB `0/71`。raw epoch 和 current epoch 都是 1，没有活跃 owner。现有丢弃条件要求 `raw_epoch < current_epoch`，所以 fatal。还没有日志证明 ROB 132 一定属于这次 redirect。

PMA=0 时同一组错误也出现过。打开开关没有消掉这条失败。

### 676087581

PMA=1、UVM_HIGH 重跑也已经结束，原错误复现。重跑进程当前不在。

- 全量日志：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/fault_2pct_no_trigger_20261009/rerun_pma1/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=676087581_rtl_full.log`
- 977983.7ns：`LSQ_CANCEL_RECONCILE`，record 3896，`ordinary=8`，`dut=10`
- 原因：fault SQ 只 arm 一个候选。对账只接受 DUT cancel 数等于 ordinary 或 ordinary+1。这次是 +2，而且日志不是 “second fault resource candidate”，说明第二个多出来的 SQ 没有进入 arm。不能把检查放宽成接受 +2，也不能只把单个槽改成队列就认为已经解释了这条日志。

这条失败不经过 PMA/PMP 查询。开关从 0 改成 1 后，同一 record、同一计数仍然 fatal。

## 修改方案

方案文档：

`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/AI_DOC/analysis/source_sv/fault_2pct_two_seed_fix_plan_20261009.md`

方案要点：

1. 仅在 PMA 模型已判定、非对齐、尚无其他异常、PBMT 为 PMA 且整段 MMIO 时，load 预测 bit 5、store 预测 bit 7。不要把所有 `c=0` 都当成 access fault。
2. `MMIO_RESOLVE` 只在能证明 raw 属于本次 redirect 已 flush/retire 的唯一 uid 时丢弃。证明不了就保持 fatal。
3. SQ cancel 先加只读诊断，确认多余的是 fault head 还是普通 SQ，再决定是否改队列。

以上仅是 2026-10-10 当时的历史状态，不代表 2026-10-12 当前实现结论。问题 1、问题 2 已分别在独立
worktree 完成行为修改；问题 3 的 SQ 范围行为修改也已完成，当前未通过的是完整 seed 后续暴露的独立 LQ
owner 冲突。

## 2026-10-12 三问题并行处理状态

本轮按三个独立问题拆分，并从当前仓库 `HEAD=e3916a0cd92346930e4566373f4db2bb68a060a8`
建立三个临时 worktree。原工作区中的未提交文件、仿真日志和 cfg 保持不动，各 worktree 只引用它们
作为只读复现基准。

| 问题 | 当前判断 | 本轮范围 | 分支 | worktree | 当前状态 |
| --- | --- | --- | --- | --- | --- |
| 问题 1：非对齐 PMA/MMIO 异常预测 | 根因已明确：RM 没有把满足 DUT `all_mmio` 语义的非对齐访问预测为 Access Fault。 | 新建 RM 专项解决方案，独立 review 无 blocker 后 coding，并用 seed `37617171` 专项验证。 | `codex/fault1-pma-mmio-20261012` | `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault1_pma_mmio_20261012` | review、coding、VCS 编译、seed `37617171` 专项验证及正常 seed `1461454841` 回归均已完成。Store directed 证据尚未执行。 |
| 问题 2：同 epoch 无主 MMIO raw | 根因已确认：ROB value `132` 是被 redirect 清除的旧 Load `uid=61, ROB=0/132`；`LoadQueueUncache.loadMmio` 在 redirect 后 `R+2` 才输出，而 resolver 只覆盖 `R/R+1` 且只查询已删除旧 owner 的 active map。 | resolver 只在精确 `R+2`、无 active owner 时，用同一 redirect record 的短生命周期 deleted-owner 和 activation/dynamic epoch 证明唯一旧 scalar Load；证明成功才 stale-drop，否则保持 fatal。 | `codex/fault2-mmio-owner-20261012` | `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault2_mmio_owner_20261012` | plan、独立 review、coding、专项测试和正常 seed 均完成；真实 seed `37617171` 在原 MMIO 事件前被问题 1 阻断，未形成完整 seed pass 证据。 |
| 问题 3：SQ cancel 少归因一个物理 entry | 根因已修正并确认：`uid=4116` 已经由 `commit_allocate()` 推进 admission prefix；前一轮 redirect 3895 回退 prefix，但因 ROB wrap 语义不覆盖该 `DUT_VISIBLE` owner 而保留它。下一轮 3896 覆盖它时，原 prefix scan 已无法访问。 | redirect 前快照有限 `uid_by_sq`，在原 prefix scan 后只补处理经完整 identity、epoch、ROB/SQ key、reservation/cutoff 和 ordinary 分类证明的 prefix 外 scalar SQ owner。 | `codex/fault3-sq-cancel-20261012` | `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault3_sq_cancel_20261012` | coding、编译、专项 vseq、正常 seed 已完成。有效 seed 中 record 3896 已由 ordinary 9 + fault 1 对齐 DUT 10；随后新出现 `uid=4033 existing_uid=4118` LQ owner 冲突，完整 seed 仍 fatal，plan 未归档。 |

上表为 2026-10-12 当前状态。问题 2 已完成受限 R+2 historical proof 行为修复，未全局忽略 `loadMmio`，
证明失败仍 fatal；问题 3 已完成 SQ owner 行为修复，未放宽 cancel reconcile。问题 3 plan 留在 `undo` 的
原因是完整 seed 后续 LQ blocker和仍待补齐的专项覆盖，不是 SQ coding blocker。

## 2026-10-10 历史 Review

要求是独立 subagent 审查方案直到无误。实际结果：

- `01a123a2-c817-7ba1-baba-389df8c5dd40`：503，未完成审查
- `01a123b2-9274-7781-a45c-2385396fc714`：503，未完成审查
- 更早的批量仿真 subagent `01a11e83-f964-7482-ab4b-dd0175299726`：429，与本次 review 无关

截至 2026-10-10，当时的方案文档还不能标成 review 通过；2026-10-12 的新一轮 review 结果见下节。

## 2026-10-12 并行处理进展

### 问题 1 review 结论

专项方案：

`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault1_pma_mmio_20261012/AI_DOC/plan/rm_plan/undo/memblock_rm_pma_mmio_misaligned_access_fault_plan_20261012.md`

第一轮 review 阻止了未经证明的逐 byte 首 cause、混合 PBMT/PMA 和跨边界异常聚合。修订方案只处理首个
seed 已证明的最小场景：PMA model 开启、非对齐、当前无其他异常、整笔所有有效 byte 均为 PBMT-PMA，
且 readonly PMA/PMP view 对每个 byte 都有效、`C=0`、当前方向无基础 AF。Load 补 bit 5，Store 补
bit 7。混合 PBMT、跨页、跨 PMA entry 和 MisalignBuffer 后续分段均明确排除。第二轮独立 review 无
blocker，已经授权 coding。

### 问题 1 coding 与验证

实际只修改：

- `mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv`
- `mem_ut/ver/ut/memblock/env/src/memblock_rm/rm_ls_core.sv`

实现遵守 review 后的最小边界：只有 PMA model 开启、访问非对齐、当前没有其他异常、所有有效 byte
均为 PBMT-PMA，且每 byte PMA/PMP readonly view 均有效、已决、`normal_cacheable=0`、当前方向无基础
AF 时，Load 补 bit 5、Store 补 bit 7。Load 比较先核对 RM/LDA/status exception，只有三者均为 0
时才读取 initialized backing，避免异常预测错误先被 query miss 遮蔽。

`git diff --check` 和 VCS 编译均通过。seed `37617171` 的专项日志为：

`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault1_pma_mmio_20261012/mem_ut/ver/ut/memblock/sim/fault1_pma_mmio_20261012/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=37617171_rtl.log`

关键结果：

```text
uid=0
allPbmtPma=1
allPmaMmio=1
expected_exception=0x20
DUT exception=0x20
result=PASS
UVM_ERROR=0
```

uid 0 不再出现 `initialized_backing query miss`。868.4 ns 仍发生问题 2 的独立 `MMIO_RESOLVE` fatal，
不属于问题 1 修复失败。

正常 seed `1461454841` 使用当前 PMA model=1 配置完成 10000 笔回归：

```text
TEST_PASS
RM_LS_COMPARE=10000
UVM_ERROR=0
UVM_FATAL=0
$finish at 2455737.100ns
```

日志：

`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault1_pma_mmio_20261012/mem_ut/ver/ut/memblock/sim/fault1_pma_mmio_20261012/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=1461454841_rtl_normal_seed.log`

方案要求的非对齐 Store bit 7 directed 证据尚未执行，必须作为残余验证保留，不能写成已覆盖。

### 问题 2 根因与证据

诊断复跑日志：

`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault2_mmio_owner_20261012/mem_ut/ver/ut/memblock/sim/fault2_mmio_diag/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=37617171_rtl_owner_diag.log`

- redirect anchor 为 sample `132`，LSQ 生效为 `133`，MMIO raw 为 `134`，compare 为 `135`。
- 唯一经历过生命周期的 ROB value `132` Load 是 `uid=61, ROB=0/132`。
- 它被 redirect `ROB=0/71` 覆盖，随后变成 `flushed=1, killed=1, active=0, terminal_done=0`，并从
  active ROB map 删除。
- RTL `LoadQueueUncache` 使用 `RegNext(s1_valid && s1_req.mmio)` 产生 `loadMmio`，因此旧请求可以在
  redirect 后的 `R+2` 继续形成 pulse。
- raw 在 pulse 出现时才冻结 epoch，所以得到 `raw_epoch=current_epoch=1`；这不代表它是 redirect 后的新实例。
- resolver overlap 只接受 `R` 或 `R+1`，本次 `R+2` 不命中；普通路径又只能查 active map，因此 fatal。

根因不是“真正无主 raw”或 ROB 复用，而是框架少建模一拍 `loadMmio` 残留时序，并且旧 owner 证明依赖
已被 redirect scan 删除的 active map。只把窗口改成 `R+2` 仍不充分，后续修复还必须使用受限保存的
历史 owner provenance 证明身份。

### 问题 2 coding 与验证

正式 plan 已归档到：

`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault2_mmio_owner_20261012/AI_DOC/plan/test_framework/plan/do/memblock_load_mmio_redirect_r2_stale_owner_fix_plan_20261012.md`

实现保留 active owner 优先级；只有 Load raw 精确位于 redirect `R+2` 且两个 wrap key 都没有 active owner
时，才用同一 cancel record 的 redirect-deleted LQ owner、旧 activation epoch、dynamic epoch、完整 ROB/LQ
key、真实 issue 和 flush/kill 状态证明唯一旧 scalar Load。唯一证明成功返回 stale-drop且不写 MMIO tag；
零候选、多候选、R+3、Store或任一 provenance不一致仍触发 `MMIO_RESOLVE` fatal。

- 专项 pending-MMIO vseq：`TEST_PASS`、`UVM_ERROR=0`、未捕获 `UVM_FATAL=0`，5 个负向 fatal 均被精确捕获。
- 正常 real-smoke seed `666666`：`TEST_PASS`、`UVM_ERROR=0`、`UVM_FATAL=0`。
- 真实 seed `37617171`：在原 MMIO 事件前被问题 1 的早期 scalar fault 阻断，停止前没有出现
  `MMIO_RESOLVE`；该结果不能写成问题 2 的完整真实 seed pass，待问题 1 基线合入后仍需重跑。

### 问题 3 根因与证据

有效诊断复跑日志：

`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault3_sq_cancel_20261012/mem_ut/ver/ut/memblock/sim/base_fun/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=676087581_rtl_fault3_pathfix_uvmlow.log`

日志已确认 `MAIN_TRANS_NUM=10000`，issue、redirect、LSQ enqueue、commit 均开启，PMA model=1。record
`3896` 的实际分量为：

```text
ordinary=8
fault=1                 # uid=3956, ROB=1/76, SQ=0/40
stranded_active_owner=1 # uid=4116, ROB=0/76, SQ=1/35
dut=10
lastEnqCancel=0
```

8 个 ordinary owner 为 `uid=3957,3959,3963,3968,3970,3974,3976,3979`。额外的 `uid=4116`
在失败点仍满足 `active=1, sq_mapped=1, reservation_state=2, accounted_epoch=0,
redirect_covers=1, fault=0`。DUT 的 10 个物理 cancel 槽正好由上述 8 个 ordinary、1 个 fault 和该
active owner 组成；`lastEnqCancel=0` 排除了 redirect 当拍 enqueue。

后续源码复核纠正了早期诊断中的一个错误：`commit_allocate()` 在设置 ENQ status 时已经通过
`mark_uid_enqueued()` 把 `uid=4116` 推进 admission prefix；`complete_admission()` 只开放 issue route，
不负责推进该 prefix。

真实两轮时序为：record `3895` 的 redirect 回退 admission prefix，但其 ROB `1/75` 不覆盖
`uid=4116` 的 ROB `0/76`，所以该 `DUT_VISIBLE` SQ owner 保持 active/mapped，并落到回退后的 prefix 外。
record `3896` 的 ROB `1/76` 随后覆盖它，但原 prefix scan 只到 uid 3981，无法再访问 uid 4116。DUT 按
物理 SQ 全表执行 `needCancel`，正确把它计入。

根因是框架假设“所有 active UID 必然位于当前连续 admission prefix 内”，该假设被前一轮 redirect 回退
prefix、同时保留未覆盖 owner 的组合打破。不是 DUT 多算，不是第二个 fault resource，也不能通过简单接受
`ordinary+2` 修复。

### 问题 3 coding 与验证

正式方案和独立 review：

- `AI_DOC/plan/test_framework/plan/undo/memblock_v2_stranded_sq_cancel_owner_coding_plan_20261012.md`
- `AI_DOC/plan/test_framework/review_doc/undo/memblock_v2_stranded_sq_cancel_owner_coding_plan_review_20261012.md`

实现只在 redirect 中频路径冻结有限 `uid_by_sq`，保留原 admission-prefix scan，再补处理 prefix 外、仍能由
`active && enq && active_sq_mapped`、scalar `numLsElem==1`、完整 ROB/SQ key、dynamic epoch、
`DUT_VISIBLE` reservation sample 和 redirect cutoff 严格证明的 ordinary SQ owner。control、commit、terminal、
fault、exception 和当前 epoch 已 accounted 但仍 active/mapped 的 owner 全部 fail-fast。成功 owner 复用
`prepare_uid_for_redirect_reissue()`，没有放宽 reconcile，也没有修改 fault candidate 的独立归因。

VCS 编译通过；`basicTest + memblock_stranded_sq_cancel_directed_vseq` 专项测试通过，两轮 redirect 自然形成
stranded owner，并覆盖 8 个 expected-fatal 边界，最终 `UVM_ERROR=0/UVM_FATAL=0`。正常 seed `666666`
使用 `tc_dispatch_real_smoke.cfg` 通过。

有效 seed `676087581` 使用从主工作区临时带入且内容一致的 fault cfg，日志首行确认包含
`+MEMBLOCK_MAIN_TRANS_NUM=10000`、`+MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=1` 和
`+MEMBLOCK_CHECK_TRIGGER_EN=0`。有效日志：

`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault3_sq_cancel_20261012/mem_ut/ver/ut/memblock/sim/base_fun/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=676087581_rtl_fault3_fix_valid.log`

关键结果：

```text
977968.700ns stranded_owner record=3896 epoch=3896 uid=4116 ... sq=1/35
977983.700ns fault SQ snapshot->cancel uid=3956 record=3896 ordinary=9 dut=10
```

原 `LSQ_CANCEL_RECONCILE` 已消失。但 seed 继续运行到 `978120.700ns` 后出现新的独立状态冲突：

```text
UVM_FATAL [COMMON_DATA] lqIdx already active: uid=4033 existing_uid=4118
```

当前证据只证明 `uid_by_lq` 残留 owner 冲突，尚未闭合 uid 4118 的 reservation、redirect cutoff、ROB 覆盖和
DUT cancel/deq 归属。该 LQ 行为超出已 review 的 SQ-only plan，不能直接复制 SQ matcher。问题 3 的原 SQ
mismatch 已修复，但完整 seed 最终仍为 `UVM_FATAL=1`；implementation review 和 plan 因此继续留在
`undo`，等待单独的 LQ 诊断/方案/review。
