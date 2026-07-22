# soft_test_memblock_dispatch_replay_smoke_sequence.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_dispatch_replay_smoke_sequence.sv`

## 1. 文件定位与使用场景

该 sequence 是 software-only replay smoke。它不驱动真实 STA IQ feedback pin，而是向
`memblock_sync_pkg::raw_iq_feedback_q` 注入与 V2 monitor 相同的 SQ-only raw，再调用公共
monitor batch 路径，验证 adapter、writeback handler 和 replay recovery 的闭环。

它验证的当前流程是：

```text
首次 STA issue fire
  -> 注入 hit=0 的 SQ-only raw IQ feedback
  -> adapter 用 active SQ map/current status 补齐 event
  -> replay recovery 清旧 STA 状态并递增 replay_seq
  -> STA 重新 route/fire，获得新的 issue_epoch
  -> 注入 hit=1 的 SQ-only raw IQ feedback
  -> 设置 sta_issue_feedback_success
  -> 两个旧快照 real-WB 被状态保护拒绝
  -> 当前快照 real-WB 完成 STA
  -> ROB commit 和 LSQ deq 后检查终态
```

## 2. 关键函数

- `submit_raw_sta_iq_feedback(item,hit)`：创建 SQ-only raw，写入真实 `sq_key/hit/cycle`，然后
  调用 `collect_monitor_event_batch()`；不直接构造完整 `memblock_wb_event_t`。
- `fire_replay_sta_item(uid,item)`：从 replay 后 STA issue queue 选择唯一候选并调用原
  scheduler fire API，使 status 保存新的 `issue_epoch`。
- `make_pass_wb_event_with_snapshot(item,issue_epoch,replay_seq)`：仅用于构造 stale real-WB
  快照，检查旧 epoch 或旧 replay sequence 不能修改当前状态。
- `check_replay_pending_state()`：检查 miss recovery 已清除 STA dispatched/writeback/pass，
  并置 `replay_pending/replay_target_sta`、递增 `replay_seq`。
- `check_replay_final_status()`：检查 current STA IQ hit 和 real-WB 已闭环，replay pending 已清，
  store 的 STA/STD target 都已完成。

## 3. 与真实 DUT flow 的边界

该 sequence 只替代 raw producer，不替代 adapter、batch handler、writeback handler 或 recovery
handler。真实 DUT flow 的 raw 由
`io_mem_to_ooo_iq_feedback_agent_agent_monitor::mon_data()` 产生；两条路径都必须满足
`sq_valid=1`、`rob_valid=0`、`lq_valid=0`。VSTU unsupported fatal、同拍 IQ/int-WB 排序和
ctrl/deq 延后仍由公共源码负责，不在 soft test 中复制实现。
