# fault_2pct 两个失败种子的原因与修改方案

**日期**：2026-10-09
**版本/分支**：mem_ut_uvm_v2 / 7a2c1123a
**配置**：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/seq/plus_cfg/tc_l2tlb_fault_2pct_no_trigger.cfg`
**当前开关**：`MEMBLOCK_PMA_PMP_MODEL_EN=1`
**责任类别**：测试框架 / RM。本文不把这两处失败登记为 RTL 缺陷。

本文只规定已由日志和源码共同证明的修改。不能把 `C=0` 一律猜成 access fault，也不能把 SQ cancel 对账放宽成接受任意 `+2`。

## 1. 开关本身

`MEMBLOCK_PMA_PMP_MODEL_EN` 只控制参考模型，不改变 DUT。

- 为 0 时，`read_pma_pmp_af_for_rm` 在翻译成功后直接返回允许访问，不查表，也不冻结 issue context。`all_bytes_normal_cacheable` 保持 `observer_build_commit_item` 里的初值 0，这只表示没查，不是判定为 MMIO。
- 为 1 时，第一次真实 issue 冻结特权级和 PMA/PMP 代。翻译成功后按表预测 access fault：PMP 未命中时仅 `priv_mode > 1` 默认允许；PMA 未命中时 `r/w/c` 为 0，load/store 预测为 access fault。命中项的 `c=0` 只写入 `normal_cacheable=0`，现有代码明确不把它变成 access fault。
- page fault 的 bit 13/15 来自 PTE 和 `MEMBLOCK_L2TLB_S*_PF/AF/GPF/GAF_1_WT`，不来自这个开关。

因此把开关从 0 改成 1 是正确的基线，但不足以消除下面两个失败。`37617171` 在开关为 1 时已经复现同一组错误。

## 2. seed 37617171

**日志**：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/fault_2pct_no_trigger_20261009/rerun_pma1/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=37617171_rtl_full.log`

uid 0 是 scalar load，`fu_op=0x5`，对应 `MEMBLOCK_LSUOP_LHU`（9'd5），宽度 2 字节。VA `0x8d34a1cf`，Sv39 翻成 PA `0xd34a1cf` 到 `0xd34a1d0`，`pa_mask=0x3`。首地址 bit 0 为 1，半字访问非对齐。841.9ns 的翻译记录是 `allPbmtPma=1`、`allNormalCacheable=0`、`hdMisalign=1`、`expected_exception=0x0`。同一拍 DUT 写回 `exception=0x20`。

`0x20` 是 `ExceptionNO.loadAccessFault`，编号 5；`loadAddrMisaligned` 是 bit 4，`storeAccessFault` 是 bit 7。DUT 在 `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/src/main/scala/xiangshan/mem/pipeline/LoadUnit.scala` 约 1351 行把非对齐且 `s2_actually_all_mmio` 的 load 并进 `loadAccessFault`。`s2_actually_all_mmio` 约在 1219 行，条件是 `!tlbMiss && !s2_un_access_exception && (Pbmt.isPMA(s2_pbmt) && !s2_pmp.st && s2_pmp.mmio || s2_in.mmio)`。`pmp.mmio` 表示 PMA `c=0`。同一文件里 `loadAddrMisaligned` 只在 `s2_actually_pbmt_nc` 时置位，所以本种子只看到 `0x20`，不是 `0x30`。Store 侧 `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/src/main/scala/xiangshan/mem/pipeline/StoreUnit.scala` 约 494 行把 `s2_actually_all_mmio && (isMisalign || isFrmMisAlignBuf)` 并进 `storeAccessFault`。

RM 的 `observer_should_expect_addr_misaligned` 在 `hd_misalign_enabled && all_bytes_pma_pbmt` 时返回 0。本种子 `hdMisalign=1` 且 `allPbmtPma=1`，所以 RM 不置 bit 4。开关为 1 时翻译 trace 仍是 `expected_exception=0x0` 且 `allNormalCacheable=0`。由此可以确定模型没有把这次访问预测成 access fault，同时把可缓存性判成了 0；日志没有打印 PMA hit index，不能把具体表项编号写成已观测事实。若完全未命中 PMA，`init_entry` 会让 `r=0`，load 会得到 bit 5，与 `expected_exception=0x0` 矛盾，因此这次不是“未命中导致 r=0”的路径。随后 `observer_capture_load_backing` 看到期望异常为 0，调用 `read_initialized_backing_for_rm`。该函数禁止为了查询创建 memory line，PA 又不在已初始化 DRAM 中，于是报 backing miss。backing miss 是错误预测之后的连带报错，不是第一原因。

868.4ns 的 `MMIO_RESOLVE` 是另一条失败。`kind=1` 是 `MEMBLOCK_MMIO_KIND_LOAD`，ROB value 是 132，不是 uid 0 的 ROB `0/71`。uid 0 已在 865.9ns 按 fault 退役。fatal 文本给出 `raw_epoch=1`、`current_epoch=1`、`active=0`、`newer=0`。`dispatch_flush_epoch` 初始为 0，`request_redirect_flush` 才会把它加 1。因此两边都是 1，不满足函数里 `raw_epoch < current_epoch` 的旧请求丢弃条件。这只能证明 raw 记录的 epoch 已经等于递增后的 current，不能单独证明 ROB 132 属于哪一个 uid。

### 修改 1：非对齐 MMIO 预测为 access fault

文件：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv`

函数：`observer_build_commit_item`，在现有 `observer_should_expect_addr_misaligned` 调用之后、trace 发出之前。

只在下面条件同时成立时置 access fault，不置 misaligned bit：

1. `MEMBLOCK_PMA_PMP_MODEL_EN=1`，并且本次 PMA/PMP 查询已经 `af_decided`。
2. 访问非对齐。
3. 此时 `expected_exception` 仍为 0，也就是还没有 page fault、guest fault 或 PMA/PMP access fault。
4. `all_bytes_pma_pbmt==1`。PBMT-NC 不走这条路径，继续由现有 bit 4/6 逻辑处理。
5. 每个有效字节都是 PMA MMIO：`normal_cacheable==0`，且该字节没有 load/store access fault。模型关闭时 `all_bytes_normal_cacheable` 初值就是 0，不得拿它推断 MMIO。
6. load 置 bit 5，store 置 bit 7。本种子 DUT 向量是 `0x20`，所以这条路径不要同时置 bit 4/6。

伪代码：

```text
if (pma_model_en && pma_decided && misaligned &&
    expected_exception == 0 && all_bytes_pma_pbmt && all_bytes_mmio) begin
  expected_exception[store_access ? 7 : 5] = 1
end
```

`observer_capture_load_backing` 已有 `expected_exception != 0` 则直接返回。预测改对后，backing query miss 不应再出现。另外在该函数入口增加保护：若调用者已经拿到 DUT `exception_vec != 0`，即使期望仍为 0，也禁止读 backing，改为报告 exception mismatch。这只防止误导性的 query miss，不能代替上面的预测。

保持不变的路径：对齐的 `c=0` 访问仍不因此变成 access fault；PBMT-NC 的 misaligned 仍只走现有 bit 4 逻辑；已有 TLB fault 的访问不在这里追加 AF。

### 修改 2：同 epoch 的无主 MMIO raw

文件：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`

函数：`resolve_mmio_uid_by_rob_value`，最终 fatal 之前。

`lookup_active_uid_by_rob` 只查仍活跃的 uid，不能用来找已经 flush 或 retire 的 uid。实现时扫描 `get_status(uid)`，不要假设存在按 ROB 反查历史的接口。

仅当下面证明全部成立时返回 `MEMBLOCK_MMIO_RESOLVE_STALE_DROP`：

1. `active_candidate_count==0` 且 `newer_candidate_count==0`。
2. `raw_epoch == dispatch_flush_epoch`，且该 epoch 对应 `cancel_record_q` 里已有 redirect。
3. 扫描 status 后存在且仅存在一个 uid，其 ROB value 等于 raw 的 ROB value，行为种类与 raw 的 load/store 一致，并且 `flushed==1` 或 `terminal_done==1`。
4. 该 uid 的 `last_event_cycle` 不晚于 raw 的 sample。若 status 没有可与 `raw_sample_seq` 直接比较的 sample 字段，就不能实现这条 drop，应保持 fatal，并在 fatal 文本里打印候选 uid。
5. 该 redirect 的 `flush_itself=1`，并且 `rob_order_util` 判定 raw 的 ROB 被这个 redirect 覆盖。

零个匹配保持现有 fatal。多个匹配也 fatal，并打印 uid 列表。禁止仅因 `active==0` 就丢弃 raw。本方案还没有日志证明 ROB 132 一定满足第 3 到第 5 条；修改 2 是条件性丢弃，不是已经证实能消掉这条 fatal。

## 3. seed 676087581

**原失败日志**：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/sim/fault_2pct_no_trigger_20261009/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=676087581_rtl.log`

977983.7ns，`LSQ_CANCEL_RECONCILE`，record 3896，`ordinary=8`，`dut=10`。这条 fatal 位于 `resolve_fault_sq_candidate_from_snapshot` 的 `SQ_CANDIDATE` 分支，不是 “second fault resource candidate” fatal。因此这次运行只 arm 了一个 fault SQ，没有走到第二个 arm。

DUT 的 `sq_cancel_count` 来自 monitor 的 `io_mem_to_ooo_sqCancelCnt`。软件侧 `software_cancel_sq_count` 由 `note_lsq_cancel_for_uid` 累加普通仍映射的 SQ。`preserve_fault_uid_during_redirect` 对 fault head 的 SQ 不调用 `note_lsq_cancel_for_uid`，只调用一次 `arm_fault_redirect_resource_candidate`。对账因此只接受两种结果：

- DUT 等于 ordinary：候选改为 `WAIT_SQ_DEQ`。
- DUT 等于 ordinary+1：把这一个候选计入 cancel。
- 其他值 fatal。

`ordinary=8`、`dut=10` 表示除了这一个已 arm 的候选之外，还有一个 DUT cancel 没有进入 software count，也没有进入 fault resource。把单个 `fault_redirect_resource` 改成队列并不能单独解释这次日志，因为第二个 uid 根本没有进入 `arm_fault_redirect_resource_candidate`。不能把检查放宽为接受 `+2`。

### 修改 3：先定位未入账的 SQ，再决定是否改成队列

文件：`common_data_transaction.sv` 的 `resolve_fault_sq_candidate_from_snapshot`。

在现有 fatal 之前增加一次只读诊断，不改变计数，不改变 DUT 激励：

1. 打印 record、redirect ROB、ordinary、DUT count、已 arm 候选的 uid 和 SQ key。
2. 扫描当前仍 `active_sq_mapped` 的 uid，标出是否已计入 `software_cancel_sq_count`，是否是 fault head，以及其 ROB 是否被本 redirect 覆盖。
3. 诊断文本必须包含这些 uid。然后再执行现有 fatal。

用 seed `676087581`、`MEMBLOCK_PMA_PMP_MODEL_EN=1`、`UVM_HIGH`、无波形重跑。只有诊断证明存在第二个被 redirect 覆盖、仍映射、但既没有 ordinary 入账也没有 arm 的 fault SQ uid 时，才实施下面的结构修改：

- 将 `fault_redirect_resource` 从单个对象改为按 `cancel_record_id` 索引的候选队列。
- `arm_fault_redirect_resource_candidate` 对同一 record 的新 uid 追加候选，不再对第二个 uid fatal。
- `resolve_fault_sq_candidate_from_snapshot` 的期望值为 `ordinary + 本 record 中 disposition 仍为 SQ_CANDIDATE 的候选数`。等于该值时全部记为 cancel；等于 ordinary 时全部改为 `WAIT_SQ_DEQ`；其他值仍 fatal。
- `note_fault_sq_deq` 必须按 uid 和 SQ key 更新队列中的对应项，不能只看旧的单个槽。

若诊断显示多余的是普通 SQ，而不是 fault head，则不得做上述队列修改。那时应修 `note_lsq_cancel_for_uid` 漏计的分支，并在文档中追加该分支的源码位置后再改。

## 4. 明确不做的修改

- 不改 RTL/Chisel。两条失败都由 RM 或测试框架主动 `uvm_fatal/uvm_error`。
- 不把所有 `c=0` 访问改成 access fault。对齐的不可缓存访问必须保持可完成。
- 不在 `MEMBLOCK_PMA_PMP_MODEL_EN=0` 时用 `allNormalCacheable==0` 推断 MMIO。
- 不把 SQ 对账改成 `dut <= ordinary+2` 或忽略 delta。
- 不把 `MMIO_RESOLVE` 的同 epoch 无主 raw 一律丢弃。

## 5. 验证

1. `37617171`：期望不再出现 backing query miss；`expected_exception` 为 `0x20`，并与 DUT 写回一致。若仍出现 `MMIO_RESOLVE`，用修改 2 的匹配条件核对 ROB 132。
2. `676087581`：先只加诊断重跑，确认多余 cancel 的 uid 类别，再决定是否改队列。
3. 至少再跑一个原来 `TEST_PASS` 的 seed，确认对齐 cacheable load/store 的 `expected_exception` 不被修改 1 改掉。
