# soft_test_memblock_dispatch_smoke_sequence.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/soft_test/soft_test_memblock_dispatch_smoke_sequence.sv`

## 1. 文件定位与使用场景

软件端到端 smoke，不驱动真实 DUT 接口。它的用途是先验证公共框架自己的状态机是否能闭环：主表能生成，LSQ 软件分配能推进，issue queue 能发射，pass/replay/commit/deq 能让 uid 最终 success。它不检查 DUT valid/ready 时序。

输入是 directed 主表和 helper API；输出是 status 表最终状态。这个 sequence 适合改 helper 后做快速 sanity，因为失败通常说明公共状态机本身有问题，而不是 DUT 接口握手问题。

流程：

1. `build_directed_main_table()` 建 load/store/AMO 三个手动条目。
2. `admit_lsq_and_route_issue()` 对每个 uid 调 LSQ admission，然后调用本 sequence 内部的 `prepare_issue_route_for_uid()` 标记 `issue_ready` 并 route。
3. `fire_all_issue_items()` 循环选择并调用本 sequence 内部的 `mark_issue_item_fire()`，软件模拟 issue fire。
4. `inject_writeback_events()` 对 fired item 构造 pass event。
5. `commit_and_deq_lsq()` 构造 commit xaction、mark ROB commit，并软件释放 1 个 LQ 和 1 个 SQ。
6. `check_final_status()` 检查 per-target pass、全局 success、active map 为空、LSQ free count 恢复。

关键函数：

- `make_directed_transaction()`：构造 load/store/AMO 主表条目。
- `make_pass_wb_event(item)`：带 uid/ROB/issue_epoch/replay_seq 的 pass event。
- `all_required_targets_dispatched()`：确认 required target 都 dispatched。
- `prepare_issue_route_for_uid(uid)`：soft-only helper。采集 runtime CSR event 后调用 `issue_queue_scheduler::prepare_issue_route_for_uid(uid)`，用于软件 admission 后设置 `issue_ready` 并 route。真实 DUT LSQ admission flow 不调用该 soft helper，而是在 `memblock_lsqenq_dispatch_sequence` 中直接调用 scheduler。
- `select_issue_candidates(load_items, sta_items, std_items)`：soft-only helper。转发到 `issue_queue_scheduler::select_issue_candidates()`，供 software smoke 从 issue queue 中挑选本拍模拟发射项。真实 issue driver 在 `memblock_lintsissue_dispatch_sequence` 中直接调用 scheduler。
- `mark_issue_item_fire(item, fired)`：soft-only helper。转发到 `issue_queue_scheduler::mark_issue_fire()`，用于软件模拟 issue item 已被接收。真实 issue driver 根据 DUT fired mask 调 scheduler 更新状态。
- `submit_writeback_event(wb_event)`：soft-only synthetic event 入口。直接调用 `writeback_status_handler::handle_event()` 注入 pass/replay 等事件。真实 DUT flow 的 writeback 事件来自 DUT output monitor，不走该入口。
