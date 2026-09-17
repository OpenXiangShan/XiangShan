# STA tombstone replay 合并计划独立评审（2026-09-15）

| 项目 | 内容 |
| --- | --- |
| 评审对象 | `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/AI_DOC/plan/test_framework/plan/undo/memblock_sta_tombstone_replay_coalescing_plan_20260915.md` |
| 关联缺陷 | `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/AI_DOC/buglist/rm/v2/rm_buglist_2026-W38_20260914.md` |
| 评审人 | `/root/sta_tombstone_review` |
| 评审日期 | `2026-09-15` |
| 评审结论 | 条件通过；将 flush epoch 单调性和 late-fault/lifecycle 验证补入 plan 后，可进入 coding。 |

## 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
| --- | --- | --- | --- |
| `tombstone` | 已发生 STA replay 的旧身份锚点，只供迟到 fault 重建身份。 | `status.sta_late_fault_tombstone_q` | UID15 的第一次 `hit=0`。 |
| `stable scope` | 未发生自身 redirect 的 active STA 身份范围，由动态版本、ROB 和 SQ 共同限定。 | `dynamic_epoch + rob_key + sq_key` | `(0, 0/15, 0/9)`。 |
| `replay sequence` | 每次 replay 后递增的短期版本号，表示当前重新发射轮次。 | `status.replay_seq` | 同一 UID 从 1 增至 55。 |
| `target flush epoch` | STA issue snapshot 对应的 flush 下界；存活的老 UID 可以跨无关年轻 redirect。 | `target_flush_epoch` | 新值可大于最早 record，但不能倒退。 |
| `late fault fallback` | 当前 STA snapshot 不再存在时，exception raw 使用 tombstone 还原旧 identity 的分支。 | `read_sta_late_fault_tombstone()` | old STA fault 在 replay 后到达。 |

抽象功能描述：本评审只判断测试框架 history 的归属、容量和生命周期是否正确；不修改或重新定义 DUT 的 StoreUnit feedback 协议。

## 评审范围与事实核验

评审覆盖 `capture_sta_late_fault_tombstone()`、`read_sta_late_fault_tombstone()`、StoreUnit feedback 的已生成 RTL 语义和 seed `856436350` 的 FSDB。

- `common_data_transaction.sv:1887` 的 history capacity guard 是首个 fatal 来源，不是 DUT assertion 或 RM compare。
- FSDB 中 `top_tb.U_MEMBLOCK._inner_dtlb_st_tlb_st_io_requestor_0_resp_bits_miss` 在 `1885.2ns` 后为 0，而 `top_tb.U_MEMBLOCK._inner_StoreUnit_0_io_feedback_slow_bits_hit` 仍为 0。
- `build/rtl/StoreUnit.sv:1407` 将 feedback hit 定义为 `io_feedback_slow_bits_hit_r & ~s2_misalignNeedReplay`；故 cross-16B misalign replay 的 `hit=0` 是 RTL 已定义语义。
- `read_sta_late_fault_tombstone()` 已按 `dynamic_epoch + ROB + SQ` 过滤并以最早 `create_cycle` 选择 record。后续 replay 的不同 `issue_epoch/replay_seq` 不会提高 raw late fault 的可判定性。

结论：该问题属于测试框架对合法 replay 的 history 容量策略错误，不是 RTL 问题，因此无需记录 RTL 修复方案或修改 RTL。

## 方案评审

### 正确部分

同一个 stable scope 只保留最早 tombstone 是正确的最小修改。首条 record 继续保存原始 `issue_epoch`、`replay_seq`、`target_flush_epoch` 和 `create_cycle`，既有 fault reader 仍可按原有规则找到它；每次 failed feedback 仍继续进入现有 recovery queue，不会把 replay 静默吞掉。

### 必须补充的边界

1. 完全相同的 `issue_epoch/replay_seq` 仍必须严格检查 ROB、SQ 和 flush epoch；否则可能把状态表损坏误当重复事件。
2. changed issue/replay 的同 stable scope 不应要求 flush epoch 相等，因为一个老 UID 可以跨无关年轻 redirect 存活；但当前 epoch 小于 retained 最早 epoch 必须 fatal，避免状态快照回退被掩盖。
3. coalesce 分支不得覆盖首条 record 的任意 identity 字段，也不得改变 fault writer、issue queue 或 replay queue 的所有权。
4. 验证必须覆盖多次 coalesce 后的 late-fault fallback，以及 redirect/reset/terminal retire 的既有 cleanup，不能只证明 overflow 消失。

上述约束已写回被评审 plan 和关联 bug 记录。

## 性能与状态生命周期评审

该路径位于 IQ feedback 事件处理，扫描对象仅为当前 UID 的有界 queue，最大为 `MEMBLOCK_DUT_SQ_SIZE=56`；未引入 `main_trans_num` 全表扫描。coalesce 后通常在首条 record 处返回，避免 queue 持续增长。

redirect、reset、terminal retire 和 fault consume 的既有 `clear_sta_late_fault_tombstones()` 不应修改；它们仍是跨 dynamic epoch 复用的边界。normal STA writeback 继续严格依赖 current snapshot，只有 exception late-fault fallback 可读取 history。

## 最终结论

在 plan 已纳入上述四项约束后，独立评审无阻塞项。可实施的最小代码改动仅位于 `capture_sta_late_fault_tombstone()` 的已有 queue 遍历中。
