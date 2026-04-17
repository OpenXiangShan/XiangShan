# Frontend BT Context Handoff V3

## 1. 目标

本文件用于在当前 doc worktree 中快速恢复 Frontend BT 文档工作的上下文，并直接衔接后续文档演进。

当前总目标是：

- 以 Kunminghu V3 `Frontend` 为 DUT，推进前端 BT 级验证。
- 在当前 `src/test/python/Frontend/docs/` 下持续维护 Frontend BT 的策略、测试点、funcov 和验收文档。
- 让后续 Frontend BT 文档工作不再依赖旧的独立目录。
- 围绕主线已打通的 pilot 点，继续扩展经理可验收的闭环文档。

## 2. 当前分工与定位

当前团队分工中：

- 环境 agent 负责 `env/`、`tests/` 和必要构建链路。
- 文档 agent 负责：
  - `src/test/python/Frontend/docs/`
  - 测试点 CSV
  - 覆盖率建模文档
  - 验收和迁移说明文档
- 当前文档工作不再自己判环境运行状态，而是消费主控目录和环境 agent 给出的真实 DUT 结果。

## 3. 当前已完成内容

### 3.1 Frontend BT 测试点文档

当前目录已经具备：

- `02_测试点分解/Frontend_功能测试点分解_层级版_V3.csv`
- `02_测试点分解/Frontend_测试点分解_V3.csv`
- `02_测试点分解/Frontend_测试点分解_V3_README.md`

这些文件已足够作为后续 Frontend BT 测试点继续维护的主基线。

### 3.3 功能覆盖率建模已启动

已完成文件：

- `03_功能覆盖率建模/Frontend_BT_功能覆盖率建模方案_V3.md`
- `03_功能覆盖率建模/Frontend_BT_功能覆盖率映射_初版_V3.csv`
- `03_功能覆盖率建模/Frontend_BT_功能覆盖率试点清单_V3.csv`
- `frontend_bt_functional_coverage_pilot.csv`

已明确的策略：

- V3 文档保留为建模基线。
- 当前 active pilot CSV 以 `frontend_bt_functional_coverage_pilot.csv` 为准。
- 当前 active CSV 已对齐到主线 testcase 名称。

### 3.4 汇报材料已准备

当前目录已经具备：

- `04_汇报与交接/Frontend_BT_验证阶段进展摘要_V3.md`
- `frontend_bt_pilot_acceptance_status.md`
- `05_试点闭环与主线迁移/Frontend_BT_首轮覆盖率试点打通总结.md`

## 4. 当前推荐的最小闭环

V3 阶段建议优先打通以下 6 类基础场景：

1. `reset` 启动 + 非 MMIO 顺序取指
2. backend `ctrl redirect` 恢复
3. 单次 MMIO 取指
4. ITLB 单次 miss + PTW 正常返回
5. PMP deny 导致 access fault
6. backend `canAccept` 反压 + IBuffer 恢复

截至当前，文档层已经确认：

- `BIN-004` 已通过
- `BIN-012` 已通过
- `BIN-012` 当前 raw funcov 异常类型口径是 `af`

## 5. 下一步工作重点

优先级建议如下：

1. 继续把 `BIN-004`、`BIN-012` 的验收文档写实并保持与环境结论一致
2. 继续把 `BIN-001`、`BIN-002`、`BIN-003`、`BIN-014` 从历史闭环材料推进到主线闭环材料
3. 把 `BIN-007`、`BIN-013` 继续维持为第二批候选，不提前写成通过
4. 继续清理残留的旧目录引用
5. 继续维护当前 doc worktree 的索引和 read order

## 6. 当前关键问题

当前已经识别出的关键问题包括：

1. `BIN-012` 的 `INSTR_MISMATCH` 仍是当前 checker 边界，需要持续跟踪
2. 部分 L0/L1 bin 还没有写成经理验收文档
3. 当前 active CSV 与 V3 基线文件还需要继续同步维护

## 7. 建议优先读取的文件顺序

建议按以下顺序读取：

1. `README.md`
2. `frontend_bt_coverage_migration_notes.md`
3. `frontend_bt_pilot_acceptance_status.md`
4. `05_试点闭环与主线迁移/Frontend_BT_首轮覆盖率试点打通总结.md`
5. `05_试点闭环与主线迁移/Frontend_BT_批量覆盖率闭环矩阵.md`
6. `05_试点闭环与主线迁移/Frontend_BT_第二批候选_BIN007_BIN013_证据评估.md`
7. `07_覆盖率量化与可视化/Frontend_BT_当前覆盖率量化基线_2026-04-17.md`
8. `03_功能覆盖率建模/Frontend_BT_功能覆盖率建模方案_V3.md`
9. `02_测试点分解/Frontend_功能测试点分解_层级版_V3.csv`

如果需要再继续向细节下钻，再补读：

-- `03_功能覆盖率建模/Frontend_BT_功能覆盖率映射_初版_V3.csv`
-- `06_测试台设计/`

## 8. 当前文件清单

当前这条工作线最关键的产物包括：

- `02_测试点分解/Frontend_功能测试点分解_层级版_V3.csv`
- `03_功能覆盖率建模/Frontend_BT_功能覆盖率建模方案_V3.md`
- `03_功能覆盖率建模/Frontend_BT_功能覆盖率映射_初版_V3.csv`
- `03_功能覆盖率建模/Frontend_BT_功能覆盖率试点清单_V3.csv`
- `frontend_bt_functional_coverage_pilot.csv`
- `frontend_bt_pilot_acceptance_status.md`
- `05_试点闭环与主线迁移/Frontend_BT_首轮覆盖率试点打通总结.md`
- `05_试点闭环与主线迁移/Frontend_BT_批量覆盖率闭环矩阵.md`
- `05_试点闭环与主线迁移/Frontend_BT_第二批候选_BIN007_BIN013_证据评估.md`
- `07_覆盖率量化与可视化/Frontend_BT_当前覆盖率量化基线_2026-04-17.md`

## 9. 预期衔接方式

后续新的文档工作不应从“重新解释历史背景”开始，而应直接进入：

- 当前 active CSV 与 V3 基线的对齐
- 下一批 bin 的验收文档补齐
- 当前主线新 evidence 的回灌

本 handoff 文档的目的，是让后续文档演进直接从当前 `docs/` 出发，而不是再回旧目录恢复上下文。
