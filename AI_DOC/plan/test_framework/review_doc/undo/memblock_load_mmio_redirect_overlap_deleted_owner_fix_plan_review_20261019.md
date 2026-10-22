# MemBlock Load MMIO Redirect Overlap 已删除 Owner Plan Review

评审日期：2026-10-19

关联 plan：`AI_DOC/plan/test_framework/plan/undo/memblock_load_mmio_redirect_overlap_deleted_owner_fix_plan_20261019.md`

评审结论：通过，可以 coding。

## 1. 术语与抽象功能说明

| 术语 | 中文含义 | 代码落点 | Review关注点 |
|---|---|---|---|
| `overlap window` | redirect与迟到Load MMIO可能重叠的`R..R+2`窗口 | resolver中的sample比较 | 时间命中不能单独作为丢弃依据 |
| `deleted owner` | redirect删除旧owner时保留的单代证据 | `redirect_deleted_lq_owner_by_key` | 必须绑定同一epoch和record |
| `historical proof` | 多个既有状态联合完成的唯一旧实例证明 | `find_unique_redirect_deleted_load_mmio_owner()` | 零个或多个候选都必须fatal |
| `stale-drop` | 丢弃已证明的旧raw且不写tag | resolver返回值 | 不得改变DUT激励或伪造完成事件 |

`resolve_mmio_uid_by_rob_value()` 的抽象职责是解析raw归属，不负责修正DUT事件或重建owner。
`find_unique_redirect_deleted_load_mmio_owner()` 的抽象职责是只读证明旧实例，不负责修改生命周期。

## 2. 根因 Review

失败日志显示raw与redirect同为sample `489999`，active候选已为0，但`uid=7358`的status、完整
ROB key、cancel record和deleted-owner窗口能够唯一证明该Load被record `11058`删除。当前源码只在
`raw_sample_seq == redirect_sample_seq + 2`时调用历史helper，因此首错由测试框架的调用时点限制导致，
不是DUT功能错误，也不是owner证据缺失。

## 3. 方案完整性 Review

1. **R/R+1遗漏**：plan已覆盖redirect先于raw处理时active owner消失的两种新增合法顺序。
2. **新owner风险**：只允许`active_candidate_count==0`进入历史证明；存在新active owner继续fatal。
3. **wrap歧义**：helper要求唯一完整ROB key候选，两个flag历史候选不能通过。
4. **无历史风险**：没有同record deleted-owner时helper返回失败并fatal，不按窗口静默drop。
5. **非Load风险**：静态behavior、LQ使用和真实issue均为强制条件。
6. **过期窗口**：R+3不属于overlap，保持原owner解析/fatal路径。
7. **状态副作用**：成功仅返回`STALE_DROP`，adapter不写tag；helper不修改map、queue或status。
8. **性能**：全表扫描只进入异常slow path，正常高频路径不变。

## 4. 实现约束

- 只修改历史helper的调用门槛和诊断文本，不删除或弱化helper内部条件。
- 定向测试必须使用公开redirect/LSQ owner API建立deleted-owner，不能直接伪造内部map。
- 保留现有active-owner、survivor、新owner、wrap、非Load和R+3测试。
- 不修改DUT RTL、monitor epoch采样和Store MMIO语义。

## 5. 最终结论

方案是对既有R+2修复的处理顺序补全，改动边界小，证据门槛未放宽。没有发现会吞掉当前owner、
掩盖RTL错误、扩大redirect生命周期或改变DUT激励的blocker，可以进入coding和专项验证。
