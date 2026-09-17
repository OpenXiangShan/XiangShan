# STA tombstone replay 合并修复计划（2026-09-15）

| 项目 | 内容 |
| --- | --- |
| 状态 | 已完成独立 review，待 coding |
| 版本 | V2，`mem_ut_uvm_v2` |
| 关联缺陷 | `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/AI_DOC/buglist/rm/v2/rm_buglist_2026-W38_20260914.md` |
| 修改范围 | `common_data_transaction.sv` 的 STA late-fault tombstone capture 逻辑 |
| 非目标 | RTL、Scala、DUT interface、L2TLB response、RM 比较公式、cfg/plusarg、cross-16B 随机权重 |
| 复现入口 | `basicTest + memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq`，cfg `tc_dispatch_real_mmu_sv39_pbmt0_non_nc_100k`，seed `856436350`，请求数 100 |

## 专有名词与抽象功能说明

| 术语 | 中文含义 | 代码落点 | 示例 |
| --- | --- | --- | --- |
| STA | Store Address 子操作，是 scalar store 的地址/翻译执行侧。 | `MEMBLOCK_ISSUE_TARGET_STA` | UID15 的 `issueSta_0`。 |
| IQ feedback | StoreUnit 对 issue queue 的慢反馈；`hit=0` 表示本次 STA 不可直接完成。 | `io_mem_to_ooo_staIqFeedback_*` | TLB miss 或 cross-16B replay 都可产生 `hit=0`。 |
| tombstone | 已进入 replay 的旧 STA 身份记录，仅供稍后到达的 fault raw 还原 UID generation。 | `status.sta_late_fault_tombstone_q` | UID15 的首条 replay identity。 |
| dynamic epoch | 同一 UID 经 redirect 后重建的动态实例版本。 | `status.dynamic_epoch` | redirect 后旧 record 必须清除。 |
| stable scope | 一个 active STA 在当前动态实例内不会变化的 `(dynamic_epoch, ROB, SQ)` 三元组。 | `status.dynamic_epoch`、`get_rob_key()`、`sqIdx` | UID15 为 `(0, 0/15, 0/9)`。 |
| replay sequence | 同一 UID 每次进入 replay 后递增的短期版本号。 | `status.replay_seq` | 不再用它为同一 stable scope 重复分配 history 槽位。 |

抽象功能描述：`capture_sta_late_fault_tombstone()` 在 STA IQ `hit=0` 已被 adapter 唯一归属后，为可能迟到的 fault 保存最小身份信息；它只决定是否保留 history，不负责触发 replay、驱动 DUT 或写入 fault。

## 目标功能 Flow

```text
STA IQ feedback hit=0
  -> 校验 current STA snapshot、active ROB/SQ map
  -> 完全相同 issue/replay identity？
       是：维持原严格一致性校验并返回
       否：当前 stable scope 已有最早 tombstone？
            是：保留最早 record，不增加 queue 长度并返回
            否：创建首条 tombstone
  -> 原有 feedback queue / mark_replay_pending() 继续处理 replay

迟到 STA fault raw
  -> 原有 current-first / tombstone fallback
  -> read_sta_late_fault_tombstone() 仍选择最早 compatible record
  -> mark_target_fault() 唯一写入 terminal fault
```

该改变只收敛同一 stable scope 的冗余 history；它不改变任何 IQ feedback 的消费、replay 请求、issue fire、DUT valid/ready 或 fault terminal 规则。

## 修改前问题

当前 capture helper 对每个不同的 `(dynamic_epoch, issue_epoch, replay_seq)` 都 push 一条 record。cross-16B store 在 StoreUnit 中可持续返回 `feedbackSlow.hit=0`，但该接口未带原因位；因此 framework 合法地继续走既有 replay。这些重试具有相同的 active ROB/SQ/dynamic identity，且 fallback 读取侧本来只选最早 record，新增的后续 records 不增加 late-fault 的可判定信息。

当重试达到 `MEMBLOCK_DUT_SQ_SIZE=56` 时，当前容量 guard 发出 fatal，导致测试框架在 DUT 正常 replay 过程中失败。

## 主实现 Flow

### `capture_sta_late_fault_tombstone()`

抽象功能描述：该函数由 `writeback_status_handler::handle_issue_feedback_event()` 在 STA failed feedback 时调用。它验证当前 active snapshot，并仅在该 active dynamic instance 尚无 late-fault recovery anchor 时创建 record；返回成功后 caller 仍按现有流程把 feedback 放入 recovery queue。

**路径**：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`

**输入**：已归属的 UID、当前 `issue_epoch`、当前 `replay_seq`、event cycle。

**输出/副作用**：首次 stable scope 写入一个 tombstone；完全重复或同 stable scope 的后续 replay 不修改 queue；非法 active map/identity 保持 fatal。

源码级伪代码：

```text
validate current status, STA dispatch, epoch/replay identity and active ROB/SQ map
for each existing tombstone of this UID:
  if same dynamic epoch and same issue epoch and replay sequence:
    require ROB/SQ/flush identity also equal
    return success
  if same dynamic epoch and same ROB and SQ:
    require current flush epoch is not earlier than retained earliest epoch
    return success  // retain its earliest identity
require queue size below existing compile-time bound
append new tombstone with current identity and creation cycle
return success
```

中文文字伪代码：

该函数在每个 STA failed feedback 的事件路径中只维护“能否让迟到 fault 找回当前 UID”的最小锚点。它首先使用现有 status、STA 发射状态、issue epoch、replay sequence 和 active ROB/SQ map 排除 stale 或损坏输入；这些失败分支保持原来的返回或 fatal 行为。随后先检查完全相同的 issue/replay identity，若发现重复则继续核对 ROB、SQ 和 flush epoch，避免真正的状态错配被合并。若 identity 已因一次合法 replay 改变，但 dynamic epoch、ROB 和 SQ 仍相同，函数先确认当前 flush epoch 没有早于保留 record；它不要求两个 epoch 相等，因为一个未被 flush 的老 UID 可以跨无关年轻 redirect 存活。检查通过后函数不再增加 record，因为读取端本来只会选择这组 stable scope 中最早的 record；调用者仍会把本次 feedback 交给原有 replay handler。只有该 stable scope 首次出现时才检查既有容量并写入 record。该函数不调用 `mark_replay_pending()`；该子函数仍由 recovery handler 消费 feedback 后负责清当前 STA 发射状态、递增 replay sequence 和重新开放 issue。

## 正确性与边界

1. **迟到 fault 不丢失**：首条 record 的 `issue_epoch`、`replay_seq`、`target_flush_epoch` 和 `create_cycle` 保持不变。`read_sta_late_fault_tombstone()` 既有“最早 compatible record”规则继续有效。
2. **不放宽 normal writeback**：只有 exception fault 的 adapter fallback 可读 tombstone；normal STA raw 继续要求 current snapshot。
3. **不跨 dynamic instance**：redirect/reset/terminal retire 的原有清理继续删除 history；不同 `dynamic_epoch` 不合并。
4. **不掩盖状态损坏**：完全相同 issue/replay identity 却 ROB、SQ 或 flush 不一致仍 fatal；changed issue/replay identity 的同 stable scope 也必须拒绝 flush epoch 回退；不同 stable scope 仍可使用原有容量 guard 暴露异常。
5. **性能**：这是每个 IQ feedback 的有界 queue 扫描，最大 56 项，且替代了原本相同的扫描与不断 push；不扫描 `main_trans_num` 或全局 map。
6. **激励不变**：不修改 cross-16B 权重，也不将 `hit=0` 静默丢弃或伪造 normal pass。

## 验证计划

1. 对修改范围执行 `git diff --check`，确认只改 tombstone capture 语义和关联文档。
2. 远端重新编译 mode `pbmt0_non_nc_100k_xstatefix_1000seed_20260915`。
3. 新增或复用定向测试：同 stable scope 连续两次以上 STA failed feedback 后，history 长度保持为 1，且每次 feedback 仍进入既有 replay recovery；随后注入/复用一笔与首条 identity 对应的 late STA fault，确认仍能命中首条 anchor 并终止 UID。
4. 新增或复用生命周期测试：redirect 后的不同 dynamic epoch、reset 和 terminal retire 均清除 history，不能跨实例复用首条 anchor。
5. 用原始 cfg、原始 cross-16B 权重和 `seed=856436350` 重跑 100 请求；要求无 `STA_LATE_TOMBSTONE` overflow，且 `UVM_ERROR=0`、`UVM_FATAL=0`、`TEST_PASS`。
6. 如该 seed 暴露后续不同根因，保留本 bug 章节的原始证据，按新的根因分别登记和处理，不用扩大本修复掩盖它。
7. 修复 seed 通过后，从头执行 1000 个不重复随机 seed 的原场景回归；仍关闭 `MEMBLOCK_HARD_XZ_CHECK_EN` 与 `MEMBLOCK_CHECK_TRIGGER_EN`。任一新 RM/测试框架问题按本流程新建或更新周文件；RTL 候选必须先经独立 RTL review 确认。

## 文档与 Review 产物

- 缺陷记录：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/AI_DOC/buglist/rm/v2/rm_buglist_2026-W38_20260914.md`
- 独立方案 review：`/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/AI_DOC/plan/test_framework/review_doc/undo/memblock_sta_tombstone_replay_coalescing_plan_review_20260915.md`；由 `/root/sta_tombstone_review` 于 `2026-09-15` 完成，结论为条件通过，本文已纳入其 flush epoch 和验证补充。
- coding implementation review：待新增至 `AI_DOC/plan/test_framework/review_doc/undo/`
