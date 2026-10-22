# STA coalesced feedback replay 生命周期修复计划独立评审（2026-10-19）

| 项目 | 内容 |
| --- | --- |
| 评审对象 | `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/AI_DOC/plan/test_framework/plan/undo/memblock_sta_coalesced_feedback_replay_lifecycle_fix_plan_20261019.md` |
| 关联缺陷 | `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/XiangShan/AI_DOC/buglist/rm/v2/rm_buglist_2026-W43_20261019.md` |
| 评审人 | 独立只读 Codex reviewer |
| 评审日期 | `2026-10-19` |
| 评审结论 | 通过；根因和最小修复成立，需保留重复事件、late fault及生命周期清理边界。 |

## 术语与抽象功能说明

| 术语 | 中文含义 | 评审关注点 |
| --- | --- | --- |
| coalesced feedback | tombstone未新增但当前 feedback仍有效。 | 不能因此跳过 recovery event。 |
| recovery owner | 唯一推进 replay状态的 handler。 | 继续通过 `handle_replay_event()` 调用 `mark_replay_pending()`。 |
| identity guard | 用 current epoch/replay过滤旧事件。 | 保证完全重复 event不重复生效。 |

抽象功能描述：本评审判断 no-progress是否由测试框架首发、修复是否恢复 replay所有权且不
改变 DUT激励或掩盖 RTL异常。

## 证据与责任判断

`583.000ns` 的首次 failed创建 tombstone并进入 recovery；`613.000ns` 的第二次 failed已是
`issue_epoch=19/replay_seq=1`，证明第一次 replay完成了状态推进和重新发射。第二次 capture
返回成功且 coalesce，但 caller在 `created_new=0` 分支提前返回，未调用
`push_feedback_event()`。随后 STA保持 dispatched，issue scheduler不会重建 item，UID0虽完成
STD仍不能 terminal。

日志没有 DUT assertion、协议错误、X/Z首发点或 UID0 fault/redirect。两次 feedback均由当前
active ROB/SQ/dynamic identity归属，首个确定错误动作是测试框架丢失 recovery event。因此
责任分类为测试框架，不是 RTL。

## 方案评审

最小修复正确：capture失败仍丢弃；capture成功后 `created_new` 只控制日志，并无条件把当前
feedback送入 recovery queue。不得在 writeback handler直接调用 `mark_replay_pending()`，否则
会绕过 PTW wait和redirect-first仲裁。

必须保持以下边界：

1. 完全重复 feedback仍入队，第一条改变 identity后其余项由 current identity guard丢弃。
2. 同 stable scope不同 replay sequence各自产生有效 replay，history仍只保留首条。
3. late fault落表后排队 replay不能复活 UID。
4. redirect/reset/terminal cleanup、fault writer、tombstone容量和 DUT激励不变。
5. 不得通过 replay上限、强制 pass、关闭 cross-16B或降低异常频率规避。

## 文档路由结论

W38问题是 tombstone history无界增长；本问题是 `2026-10-16` 集成代码中
`created_new` gate吞掉 recovery event，根因分支和失败表现不同。应在 W43新建章节并关联
W38，不改写 W38原始根因。

## 最终结论

方案无阻塞项，可按当前 coding授权实施。验证必须先通过 replay smoke和失败 seed，再恢复
100-seed回归；未完成这些验证前状态只能是“已修复待验证”。
