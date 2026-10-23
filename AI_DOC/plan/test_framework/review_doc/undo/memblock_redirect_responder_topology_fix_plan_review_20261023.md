# memblock legacy real-smoke redirect responder 拓扑修复 Review

## 1. Review 范围

关联计划：
`AI_DOC/plan/test_framework/plan/do/memblock_redirect_responder_topology_fix_plan_20261023.md`

reviewer：独立只读 Codex reviewer；日期：`2026-10-23`。

Review 覆盖 testcase default-sequence 覆盖关系、legacy/VSEQ 两种 responder ownership、redirect drive queue、
monitor anchor 和 cancel reconcile 生命周期。

## 2. 已确认事实

- `tc_base` 原本安装 queue-driven redirect responder。
- `tc_dispatch_real_smoke` 的同路径后写配置覆盖了该 responder。
- 失败 seed 已创建 active redirect/cancel record，但没有 redirect monitor anchor。
- 专项诊断时 local queue 为连续全零 baseline，证明 depth fatal 不是 target count mismatch。
- 恢复 responder 后无需修改 cancel queue/reconcile 数据语义。

## 3. Review 结论

独立只读 review 未发现功能性 blocker。责任层应保留在派生 testcase：删除错误 override，让 legacy
real-smoke 继承 `tc_base` 的唯一 queue-driven responder。`basicTest` 不继承 `tc_base`，显式 VSEQ 自己
启动唯一 responder，因此两种 topology 不会形成双 consumer。

review 建议已采纳：最终代码删除 override，不在派生 testcase 重复写相同 wrapper。

提交门禁包括原 seed、22 个已知有效 depth seed、随机 cancel-reconcile 和显式 VSEQ 拓扑验证。
原结果中 `default.cfg` 的 5 个 seed 因 issue sequence 关闭却强制生成 1000 笔事务而无进展，属于独立
回归配置冲突，不得归入本方案通过数。

## 4. 风险检查

| 风险 | Review 结论 |
|---|---|
| legacy 双 responder | 同一 config-db key 只保留一个 default sequence；删除 override 后由基类单点配置 |
| explicit VSEQ 双 responder | `basicTest` 不继承 `tc_base`，VSEQ 是唯一 queue consumer |
| responder stop hang | responder 以 global stop、pending queue 和 active redirect 为退出条件，需专项验证 |
| 掩盖 cancel mismatch | cancel queue/reconcile/target/deadline 均未修改 |

## 5. 禁止方案

- 增大 `MEMBLOCK_CANCEL_SNAPSHOT_QUEUE_MAX_DEPTH`；
- 屏蔽 depth fatal；
- 在 record owner 不明确时压缩 snapshot；
- 为绕过缺失 anchor 修改 exact target 或 deadline。
