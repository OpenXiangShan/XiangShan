# MemBlock STA terminal 后迟到 IQ feedback 消费实现级 Review（2026-08-28）

| 项目 | 内容 |
| --- | --- |
| Review 状态 | 代码与独立 plan review 通过；10000 笔回归待执行 |
| 版本 | V2，`mem_ut_uvm_v2` |
| 对应 plan | `AI_DOC/plan/test_framework/plan/do/memblock_sta_terminal_iq_feedback_drop_coding_plan_20260828.md` |
| 问题分析 | `AI_DOC/analysis/framework_design/memblock_sta_terminal_iq_feedback_rm_issue_analysis_20260828.md` |
| 源码范围 | `mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv` |
| 非目标 | RTL、Scala、DUT interface、PMA/PMP、TLB、DCache ledger、sequence、cfg 和 scheduler |

## 1. 专有名词与职责

| 术语 | 中文含义 | 代码落点 | 示例 |
| --- | --- | --- |
| IQ feedback | StoreUnit 返回的 SQ-only `hit` 回应。 | `convert_raw_iq_feedback()` | UID9 的 `hit=1`。 |
| active SQ map | 通过 SQ key 定位 active UID 的关联数组。 | `uid_by_sq` | SQ `0/3` 定位 UID9。 |
| terminal UID | 已锁定 fault/exception/terminal_done 的 UID。 | `status.fault`、`exception_pending`、`terminal_done` | late `0x8080` 已被消费的 UID9。 |
| terminal drop | 已经证明属于 terminal UID 的 raw 被消费、不再变成 UVM event。 | `try_drop_terminal_sta_iq_feedback()` | 防止 terminal 后 IQ feedback 触发 strict attach fatal。 |
| strict attach | 非 terminal raw 绑定 current snapshot 的原有校验。 | `attach_current_issue_snapshot()` | 无 owner、SQ 不一致、未发射等情况仍 fatal。 |

## 2. Review 结论

本次改动只补齐 adapter 对 terminal 后物理迟到 IQ feedback 的处理。此前 UID9 的 old STA fault
在 `882.8ns` 已通过 tombstone 终止，但在此前已经 physical fire 的 STA 仍于 `890.3ns` 返回
`hit=1`。旧 adapter 直接要求 `sta_dispatched=1`，把可证明属于 terminal UID 的 feedback 误报为
`IQ_FEEDBACK_ATTACH` fatal。

修改后先以 SQ active map 与 canonical SQ 验证归属；只有 UID 已 terminal 才消费 raw。所有无法
证明归属或仍属非 terminal 动态实例的 feedback 都仍进入原 strict attach。该策略不修改 DUT、
不改变 RM exception 比较，也不触及 PMA/PMP、TLB 或 DCache 状态。

## 3. 修改前后对比

| 场景 | 修改前 | 修改后 |
| --- | --- | --- |
| terminal UID，SQ 可验证 | 直接 strict attach，`sta_dispatched=0` 后 fatal。 | 记录 `STA_IQ_TERMINAL_DROP` 并消费 raw。 |
| SQ 无 active owner | strict attach 报错。 | 保持 strict attach 报错。 |
| canonical SQ 不一致 | strict attach 报错。 | 保持 strict attach 报错。 |
| 非 terminal，`sta_dispatched=0` | strict attach 报错。 | 保持 strict attach 报错。 |
| 正常 active STA feedback | 生成完整 feedback event。 | 行为不变。 |

## 4. 核心实现

抽象功能描述：`try_drop_terminal_sta_iq_feedback()` 在 converter 已完成 raw STA/SQ-only 协议检查后，
验证 raw SQ 的 active owner 与 terminal 状态；命中时只消费该 raw，不写 status、queue、map、
tombstone 或 replay。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv`，函数 `try_drop_terminal_sta_iq_feedback()`。

```systemverilog
if (!data.lookup_active_uid_by_sq(wb_event.sq_key, uid)) begin
    return 1'b0;
end
status = data.get_status(uid);
canonical_sq.flag = status.sqIdx_flag;
canonical_sq.value = status.sqIdx_value;
if (!status.active_sq_mapped || canonical_sq != wb_event.sq_key ||
    !(status.fault || status.exception_pending || status.terminal_done)) begin
    return 1'b0;
end
return 1'b1;
```
中文伪代码：helper 先通过 SQ map 查 UID。没有 owner 时返回未命中，caller 随后保留原 strict fatal；找到 owner 后再核对 status 保存的 SQ。SQ 一致且 UID 已 fault、exception pending 或 terminal done 时才返回命中，表示该 raw 已可安全消费；其它任何条件都不放宽。

抽象功能描述：`convert_raw_iq_feedback()` 把 raw IQ response 转为完整 UVM event；新增调用只截获 terminal raw，未命中时仍执行既有 current snapshot attach、字段完备性检查与 replay 标记。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv`，函数 `convert_raw_iq_feedback()`。

```systemverilog
wb_event.has_sq = raw_sq_to_key(raw.sq_valid, raw.sq_flag, raw.sq_value,
                                wb_event.sq_key);
if (try_drop_terminal_sta_iq_feedback(wb_event)) begin
    return 1'b0;
end
attach_current_issue_snapshot(wb_event);
```
中文伪代码：converter 已验证 raw 必须是 STA 且只带 SQ 后，先构造 SQ key。若 helper 确认 terminal ownership，返回 0，使已经 destructive pop 的 raw 不进入 batch。否则沿用严格 attach，把 UID、ROB、SQ、issue epoch 和 replay sequence 补齐，再继续原有 success/replay 流程。

## 5. 正确性与性能检查

正确性边界：terminal drop 不依赖 `sta_dispatched`，因为 terminal 是更强的状态边界；但不能只因
`sta_dispatched=0` 丢弃 event。通过 `lookup_active_uid_by_sq()` 和 canonical SQ 对比后才允许 drop，
因此 SQ reuse、无 owner 或状态表不一致仍保留 fatal。helper 不读取 raw generation、不伪造 epoch，也不
使 fault 后反馈重新入 replay/PTW queue。

性能：每个 STA IQ raw 多一次关联数组查询和一次 status 获取，均为 O(1)。没有扫描 main table、
tombstone history、exception event queue 或 PTW wait queue。

## 6. Plan 对齐检查

对应 plan：`AI_DOC/plan/test_framework/plan/do/memblock_sta_terminal_iq_feedback_drop_coding_plan_20260828.md`。

### 6.1 实现与 Plan 不一致项

未发现实现与 Plan 不一致项。helper 的 raw 结构前置条件、SQ owner/canonical 验证、terminal 条件、
converter return 语义和非 terminal strict attach 保留均与 plan 一致。

### 6.2 Plan 未说明但 Coding 落实的细节

helper 的入口额外复核 `source=STA_FEEDBACK`、`target=STA`、`has_sq=1`、无 ROB/LQ。这是 API
边界保护，避免未来错误调用将其它 raw 静默消费；它不改变 plan 的行为目标，也不需要扩展到其它模块。

## 7. 验证与剩余风险

已完成：`git diff --check` 通过；独立 plan review 通过、无必须修改项；远端
`make eda_compile` 通过，VCS 为 `0 error(s), 0 warning(s)`。

待执行：同 seed 运行 10000 笔 Sv39/U 态回归。必须看到 UID9 对应 `STA_IQ_TERMINAL_DROP`，不再出现
`IQ_FEEDBACK_ATTACH`，并最终满足 `TEST CASE PASSED`、`UVM_ERROR=0`、`UVM_FATAL=0` 和
`terminal_done_uid=10000`。

剩余风险是 raw SQ-only 接口没有 generation：在 active map 已移除后到达的超迟到 IQ feedback 仍会
保持原 fatal，因为无法安全区分 resource reuse 与旧响应。这是有意保留的可诊断边界，不是本次可安全
放宽的情况。
