# Frontend BT 验证阶段进展摘要 V3

## 当前进展

当前 Frontend BT 文档与主线闭环工作已经进入“文档资产收敛 + 试点闭环口径稳定”阶段。

- 已完成 V3 策略、测试点、功能覆盖率建模和交接文档向当前 `src/test/python/Frontend/docs/` 的迁移。
- 已把旧 `frontend_bt/testbench/docs` 中的测试台设计文档迁入当前目录，当前 doc worktree 已可独立继续迭代文档。
- 当前主线 pilot testcase 已形成真实 DUT 证据链，`BIN-004` 与 `BIN-012` 均已有可引用的 funcov / waveform / line coverage 产物。

## 当前问题

现阶段的关键问题已经不再是“旧文档是否齐全”，而是：

- 如何继续把更多 L0/L1 bin 补成经理可验收材料。
- 如何把环境 agent 新结论稳定回灌到当前 doc worktree。
- 对 `BIN-012`，如何继续收敛 translation-aware checker 的 `INSTR_MISMATCH` 边界。

## 下一步计划

下一阶段建议优先围绕“扩大已打通 bin 的验收面”推进。

1. 继续把 `BIN-001`、`BIN-002`、`BIN-003`、`BIN-014` 从“历史闭环已完成”推进到“主线已验收”。
2. 对 `BIN-007`、`BIN-013` 保持当前谨慎口径：
   - `BIN-007` 已有 testcase 尝试，但当前 evidence 未成立
   - `BIN-013` 当前仍缺 testcase / artifact 落地
3. 继续对齐 active pilot CSV、V3 funcov 建模基线和当前 testcase 名称。
4. 对 `BIN-012` 保持当前文档口径：
   - raw funcov 异常类型命中是 `af`
   - 不提前写成 `pf`
5. 后续新增 pilot 点时，直接在当前 `docs/` 下迭代，不再回到旧目录维护。
