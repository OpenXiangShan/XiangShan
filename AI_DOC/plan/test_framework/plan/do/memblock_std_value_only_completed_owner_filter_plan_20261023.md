# MemBlock STD value-only 已完成 owner 候选过滤修复计划

## 1. Plan 定位、专有名词与抽象功能说明

本 plan 修复 V2 `writebackStd` 只携带 ROB value 时，测试框架在 ROB 环绕后把已经消费过 STD
真实写回的旧 active owner 再次计入候选，导致两个 ROB flag 都命中的问题。责任范围限定为 int-WB
adapter 的 value-only owner 归一化，不修改 RTL、DUT 激励、ROB/SQ 生命周期或 RM 比较语义。

| 专有名词 | 当前中文含义 | 代码落点 | 场景示例 |
|---|---|---|---|
| `value-only` | raw 只包含 ROB value，不包含 wrap flag | `dispatch_raw_int_wb_t.rob_value_only_without_flag` | value=12 可能对应 `0/12` 或 `1/12` |
| `active owner` | 完整 ROB key 当前仍保留在 active map 的 UID | `uid_by_active_rob` | 已 ROB commit 但 SQ 未 deq 的 store 仍可能 active |
| `completion owner` | 本动态实例唯一设置 `std_writeback/std_pass` 的真实 STD 写回 | `writeback_status_handler` | UID492 在 18168.2ns 已完成 STD target |
| `candidate` | 可由当前 raw 合法完成 STD target 的未完成 active owner | `probe_std_candidate()` | 新 UID652 已 dispatch 且 `std_writeback=0` |
| `target done` | STD target 已经真实写回，或已 pass/fault，不能再消费第二个完成事件 | `status.std_writeback/std_pass/std_fault` | `std_writeback=1` 但仍在 fault barrier 的旧 UID 也不再参与候选集合 |

抽象功能描述：`probe_std_candidate()` 在 adapter 解析一条 value-only STD raw 时，对一个可能的
ROB flag 做有限 active-map 探测，并只返回仍可消费当前 STD 完成事件的动态实例。它不选择两个有效
未完成候选中的任意一个，也不删除 active owner、清理 SQ 或吞掉无法证明归属的 raw。

## 2. 问题证据与责任边界

### 2.1 原始随机 seed

```text
tc=tc_dispatch_real_smoke
cfg=default
ts=virtual_base_sequence
seed=1545815340
MAIN_TRANS_NUM=1000
```

日志：

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_memblock_regression_closure_20261023/mem_ut/ver/ut/memblock/sim/default_issue_enable_random10_20261023/status/seed_1545815340.final.log
```

时间线：

1. `18168.200ns`：UID492，ROB `1/12` 已通过真实 STD writeback，`std_writeback/std_pass` 已置位。
2. `19195.700ns`：UID492 ROB commit，但 SQ 尚未 deq，因此 active ROB/SQ map 合法保留。
3. 新 UID652 使用环绕后的 ROB `0/12`，在 `19315.700ns` 从 STD port fire。
4. `19318.200ns`：新 STD raw value=12 到达。旧 UID492 与新 UID652 都通过当前 probe，触发
   `INT_WB_STD_KEY` 双候选 fatal。

### 2.2 历史回归 seed

历史 `seed=1396766687` 同样表现为：旧 UID62 的 STD target 已完成但 SQ 生命周期未结束，新 UID222
使用另一 ROB flag 的相同 value；当前 probe 把两者都计为合法候选，触发相同 fatal。

该问题属于测试框架。DUT 输出的是新 fire store 的合法 value-only STD writeback；歧义来自 adapter
遗漏“target 已完成”的候选资格检查。RM 尚未收到归一化 event，不是 expected-value 计算或比较错误。

## 3. 目标功能 Flow

```text
收到 value-only STD raw
  -> 固定 probe ROB flag=0
       -> 无 active owner：不命中
       -> owner 的 `std_writeback/std_pass/std_fault` 任一置位：不命中
       -> owner 当前、已 dispatch、epoch/SQ identity 合法：命中
  -> 固定 probe ROB flag=1，执行同一规则
  -> 恰好一个未完成候选：恢复完整 ROB/SQ/issue identity
  -> 零个未完成候选：保持 INT_WB_STD_KEY fatal
  -> 两个未完成候选：保持 INT_WB_STD_KEY fatal
```

## 4. 源码修改方案

修改位置：

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_memblock_regression_closure_20261023/mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv
```

修改函数：`probe_std_candidate()`。

抽象功能描述：该函数读取一个完整 ROB key 的 active owner 和当前 target 状态，判断该 owner 是否仍能
消费本次 STD real-WB。返回命中时继续复用 `fill_current_issue_snapshot()` 完成 active、dispatch、
sample flush epoch、issue epoch 和 SQ owner 校验；返回不命中时不改变任何公共状态。

源码级伪代码：

```text
构造当前 flag/value 的完整 ROB key。
如果 active ROB map 没有 owner，返回不命中。
读取该 UID 的 status。
如果 `std_writeback`、`std_pass` 或 `std_fault` 任一已经置位，返回不命中。
调用 fill_current_issue_snapshot()：
  校验 owner 当前有效、STD 已真实 dispatch、raw sample epoch 不早于当前 STD instance、
  issue epoch 非零且 SQ key 仍属于该 UID；
  校验通过则把完整 UID/ROB/SQ/issue identity 写入 candidate 并返回命中；
  否则返回不命中。
```

中文文字伪代码：adapter 对每个 ROB flag 只查询一次 active map。找到 owner 后，先检查该 UID 的 STD
target 是否已经由此前真实写回完成，或者已经进入 pass/fault 状态；已完成 owner 即使仍因 SQ deq 未到
而保持 active，也不再有资格消费第二条 STD 完成事件，因此直接从本次候选集合排除。特别是 fault
store 可能先置 `std_writeback`，再等待另一侧 real-WB 后置 `std_fault`，所以不能只调用当前
`target_entry_done()`。未完成 owner 继续进入既有
`fill_current_issue_snapshot()`，由该 helper 校验 active 生命周期、dispatch 状态、sample flush epoch、
issue epoch 和 SQ identity，并把通过校验的完整身份返回给上层。整个函数不写 status、不删除 map，
不会改变 DUT 激励或资源释放顺序。

## 5. 必须保持的边界

- 不按 UID 大小、ROB flag、ROB age 或端口号任选候选。
- 两个候选都尚未完成且都合法时继续 fatal，因为接口没有足够 provenance 区分真实 owner。
- 零候选继续 fatal 或进入已有 redirect/fault/tombstone 受限分支，不新增静默 drop。
- 不提前 retire 旧 UID，不删除 active ROB/SQ map，不改变 SQ deq 或 ROB commit 规则。
- 不修改 `fill_current_issue_snapshot()` 的通用 LOAD/STA/STD 合同。
- 候选过滤必须直接检查 `status.std_writeback || status.std_pass || status.std_fault`；不能仅复用
  `target_entry_done(status, STD)`，因为该 helper 当前只覆盖 pass/fault，无法覆盖 fault barrier 中间态。
- 不新增全表扫描；高频 raw 路径仍只做两个固定 ROB key 查询，复杂度保持 O(1)。
- 不修改 RTL、Scala/Chisel、DUT interface、cfg 随机分布或 testcase 激励。

## 6. 文档同步

- 更新本周 V2 RM/测试框架 Buglist，记录两个 seed 的同根因证据、独立责任分类和修复状态。
- 更新 `AI_DOC/analysis/source_sv/dispatch_framework_sv/dispatch_monitor_event_adapter.md`，明确 STD
  candidate 还要求 target 未完成。
- 更新 `AI_DOC/mem_ut_flow_doc/writeback_function_call_flow.md` 与
  `AI_DOC/mem_ut_flow_doc/load_sta_std_issue_flow.md` 中 value-only 双 probe 的候选定义。
- coding 和专项测试完成后新增 implementation review；review 文档保持在 `review_doc/undo`。

## 7. 验证计划

1. 当前源码重新 `--compile`，确认 VCS/elaboration/link 通过。
2. 原首错 `seed=1545815340` 使用相同 tc/cfg/ts、1000 transaction 和 issue-enable plusarg，要求 PASS。
3. 历史同簇 `seed=1396766687` 使用相同场景复测，要求 PASS。
4. 覆盖 `std_writeback=1、std_pass=0、std_fault=0` 的 fault barrier 中间态，确认不被重新绑定。
5. default coverage row 至少 10 个新随机 seed，要求全部 PASS；若出现独立首错，按 Buglist 规则另行闭环。
6. 检查日志确认旧 completed owner 被过滤，新未完成 owner正常取得 STD pass，且没有新增
   `INT_WB_STD_KEY`、重复写回或 SQ/ROB owner mismatch。
7. 恢复 default runner 修复的随机门禁；该门禁通过后，STD 修复和 default runner 修复分别独立提交。

## 8. 验收标准

- 两个已知 seed 不再发生双候选 fatal，并完整闭环为 PASS。
- 至少 10 个新随机 seed 全部 PASS。
- 零候选与两个未完成候选的 fail-fast 合同保持不变。
- `git diff --check`、方案 review、implementation review 均通过。
- plan 完成后从 `plan/undo` 移至 `plan/do`，并生成独立本地 commit。
