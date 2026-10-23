# MemBlock STD value-only 已完成 owner 候选过滤方案 Review

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象 | 使用场景/示例 |
|---|---|---|---|
| `value-only` | STD raw 只有 ROB value，没有 wrap flag | `dispatch_raw_int_wb_t.rob_value` | value=12 需探测 `0/12` 和 `1/12` |
| `active owner` | 仍保留在 active ROB map 中的 UID | `uid_by_active_rob` | ROB commit 后等待 SQ deq 的 store 仍可能 active |
| `target completion` | 当前 STD 动态实例已完成真实 STD 写回或已进入 pass/fault | `std_writeback/std_pass/std_fault` | fault barrier 可能先有 `std_writeback=1` |
| `candidate` | 可以消费当前 raw 的唯一未完成 STD owner | `probe_std_candidate()` | 新 UID 被保留，旧完成 UID 被排除 |

抽象功能说明：本 review 独立检查 value-only STD owner 归一化方案是否能在 ROB 环绕和 SQ 延迟释放
同时发生时唯一绑定当前 raw，并确认方案不会修改 DUT 激励、active map 生命周期或 RM 结果语义。

## 2. Review 范围

关联计划：

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_memblock_regression_closure_20261023/AI_DOC/plan/test_framework/plan/undo/memblock_std_value_only_completed_owner_filter_plan_20261023.md
```

检查对象：

- `dispatch_monitor_event_adapter::probe_std_candidate()` 的候选资格。
- `fill_current_issue_snapshot()` 的既有 epoch/SQ/dispatch 校验是否保持不变。
- `uid_by_active_rob`、`std_writeback/std_pass/std_fault` 和 retire/SQ deq 生命周期。
- 零候选、双未完成候选、redirect/tombstone stale raw 边界。

## 3. 首轮独立 Review 结论与修订

首轮独立责任分类 reviewer 于 `2026-10-23` 确认责任属于测试框架，且确认基本修复方向正确；但首轮
方案为只排除 `std_pass/std_fault`，被判为 `REVIEW_FAIL`。

首轮 blocker：

```systemverilog
target_entry_done(STD) == status.std_pass || status.std_fault
```

该判断遗漏 fault barrier 中间态。真实 STD fault writeback 可能先设置 `std_writeback=1`，待 STA
真实写回后才 finalize `std_fault=1`。如果 adapter 只看 `std_pass/std_fault`，该旧 owner 仍会被第二条
value-only raw 误计入候选。

修订后的方案明确要求：

```systemverilog
if (status.std_writeback || status.std_pass || status.std_fault)
    return 1'b0;
```

该判断只读当前 status，不删除 owner、不修改 SQ/ROB map；正常未完成候选仍交给
`fill_current_issue_snapshot()` 做既有完整校验。

## 4. 修订方案检查

### 4.1 责任边界

原始失败 `seed=1545815340` 的 UID492 在 `18168.200ns` 已完成 STD real-WB，`19195.700ns` 才 ROB
commit 且仍保留 SQ mapping；新 UID652 在 `19315.700ns` fire，`19318.200ns` 的 raw value=12 同时
命中两个 ROB flag。active owner 延迟释放符合测试框架的 SQ deq/ROB retire 顺序，问题是 adapter 候选
过滤遗漏已完成 target，不是 RTL 输出错误，也不是 RM expected 计算错误。

### 4.2 功能正确性

修订后每个 flag 的 probe 顺序为：

1. active map 无 owner，返回不命中。
2. owner 的 `std_writeback/std_pass/std_fault` 任一置位，返回不命中。
3. 其余 owner 进入原有 `fill_current_issue_snapshot()`，校验 active、STD dispatch、sample epoch、
   issue epoch 和 SQ identity。
4. 上层仅接受恰好一个候选；零候选和两个未完成候选继续保持 fail-fast。

`std_writeback` 是必要的 target-consumption 标记，`std_pass/std_fault` 是终态保护标记；三者联合检查
覆盖正常完成和 fault 双 real-WB barrier，不把 SQ/ROB retire 状态误当作 STD completion。

### 4.3 性能与生命周期

该修改在现有两个固定 ROB key probe 中读取单个 status，保持每条 raw O(1)，不引入全表扫描。过滤只读
状态，不会提前 retire active owner、释放 SQ mapping、清除 issue queue 或改变 DUT 端口握手。

### 4.4 边界检查

- 两个 owner 都未完成且 snapshot 合法：继续 `INT_WB_STD_KEY`，禁止按 UID/ROB age 猜测。
- 两个 owner 都已完成：零候选，继续原有 fatal；除非进入既有 redirect/tombstone 明确 stale 分支。
- redirect/tombstone raw：仍先经过已有 stale-drop 路径，新增过滤不改变其顺序。
- reissue：旧动态实例的 `std_writeback` 会由既有清理路径失效，新实例独立重新记录 issue epoch。

## 5. Review 结论

首轮方案存在 blocker，已通过 `std_writeback` 联合判断修订。修订方案责任边界清晰、状态所有权正确、
高频路径复杂度不变，未发现新的必须修改项。

**REVIEW_PASS（以修订后的方案为准）**

修订后才允许进入 coding。专项门禁必须包含原始 seed、有效 issue-enable 配置下的历史 seed、fault
barrier 中间态覆盖，以及至少 10 个 default row 随机 seed。
