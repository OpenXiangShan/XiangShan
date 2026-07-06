# soft_test_memblock_dispatch_replay_smoke_sequence.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/soft_test/soft_test_memblock_dispatch_replay_smoke_sequence.sv`

## 1. 文件定位与使用场景

软件 replay 闭环 smoke，继承 software smoke。它专门验证 replay 状态机，而不是验证真实 DUT replay 来源。核心思想是手工构造一个 replay event，让某个已发射 target 回到 issue queue，再用旧 epoch/旧 replay_seq 的 pass 验证 stale event 会被过滤。

输入是软件 smoke 已经建好的主表、发射项和手工 wb event；输出是 status 中 replay mask、replay_seq、pass/success 的变化。

流程重点：

- 找到 uid=1 的 STA 首次发射项。
- 其他 target 先 pass。
- 对该 STA 注入 replay event，调用 `exception_redirect_replay_task()`。
- 检查 `replay_pending/replay_target_sta/replay_seq+1`。
- 重新 route 并 fire STA replay item。
- 注入两个 stale pass，分别用旧 epoch 或旧 replay seq，检查被忽略。
- 注入正确 pass，检查 replay 关闭和最终状态。

函数：

- `make_replay_wb_event(item)`：构造 replay event。
- `make_pass_wb_event_with_snapshot(item,issue_epoch,replay_seq)`：构造指定快照的 pass，用于 stale 检查。
- `check_replay_pending_state()`、`check_stale_pass_ignored()`、`check_replay_final_status()`。
