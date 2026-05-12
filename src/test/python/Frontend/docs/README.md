# Frontend BT 文档索引

当前 `src/test/python/Frontend/docs/` 已作为 Frontend BT 文档的主工作目录使用。

目标是让后续文档演进、测试点扩展、覆盖率建模和经理验收，不再依赖旧的独立 `frontend_bt` 目录或 `uc_agent_workspace` 目录。

## 1. 当前推荐读取顺序

如果是恢复上下文，建议按下面顺序阅读：

1. `frontend_bt_coverage_migration_notes.md`
2. `frontend_bt_pilot_acceptance_status.md`
3. `05_试点闭环与主线迁移/Frontend_BT_首轮覆盖率试点打通总结.md`
4. `05_试点闭环与主线迁移/Frontend_BT_批量覆盖率闭环矩阵.md`
5. `05_试点闭环与主线迁移/Frontend_BT_首批L0闭环样例_BIN001_BIN002_BIN003_BIN014.md`
6. `05_试点闭环与主线迁移/Frontend_BT_第二批候选_BIN007_BIN013_证据评估.md`
7. `05_试点闭环与主线迁移/Frontend_BT_当前活跃L0_Bin状态总表.md`
8. `05_试点闭环与主线迁移/Frontend_BT_环境补证据需求清单.md`
9. `07_覆盖率量化与可视化/Frontend_BT_当前覆盖率量化基线_2026-04-17.md`
10. `07_覆盖率量化与可视化/Frontend_BT_覆盖率量化基线_2026-04-17.csv`
11. `07_覆盖率量化与可视化/Frontend_BT_覆盖率报告命令速查.md`
12. `frontend_bt_functional_coverage_pilot.csv`
13. `03_功能覆盖率建模/Frontend_BT_功能覆盖率建模方案_V3.md`
14. `02_测试点分解/Frontend_功能测试点分解_层级版_V3.csv`

## 2. 当前目录结构

### 2.1 当前主线闭环文档

- `frontend_bt_functional_coverage_pilot.csv`
  - 当前主线实际使用的试点 funcov CSV。
  - 已按主线 pytest testcase 名称对齐。
- `frontend_bt_pilot_acceptance_status.md`
  - `BIN-004` / `BIN-012` 当前经理验收口径。
- `frontend_bt_coverage_migration_notes.md`
  - 文档迁移状态、当前 canonical 目录和后续维护约定。

### 2.2 迁入的策略与测试点基线

- `01_验证策略及方案/`
  - 迁入的 V3 策略基线文档。
- `02_测试点分解/`
  - 完整测试点分解 CSV 和说明。
- `03_功能覆盖率建模/`
  - V3 覆盖率建模方案、映射和试点清单。
- `04_汇报与交接/`
  - 当前可直接使用的进展摘要和 handoff 文档。

### 2.3 迁入的旧 testbench 设计沉淀

- `05_试点闭环与主线迁移/`
  - 首轮 bin 打通总结、批量闭环矩阵、首批 L0 闭环样例、第二批候选评估、当前 L0 状态总表、环境补证据需求清单、阶段进展沉淀。
- `06_测试台设计/`
  - 从旧 `frontend_bt/testbench/docs` 迁入的 T01-T09 设计文档。
- `07_覆盖率量化与可视化/`
  - 当前 Python Frontend BT 环境的覆盖率能力评估、量化口径和可视化缺口。
  - 当前覆盖率量化基线与后续记录方式。
  - 覆盖率 HTML 报告相关命令速查。
  - 可直接复用的 CSV 量化基线文件。

## 3. 当前使用约定

1. 当前要继续演进 Frontend BT 文档时，优先更新本目录，不再回写旧目录。
2. 当前主线真实回归证据目录使用：
   - `/nfs/home/jiabowen/ai_workspace/XiangShan_frontend_bt_jiabowen/src/test/python/Frontend/data/`
3. 对 `BIN-012`，当前文档口径必须保持：
   - raw funcov 的异常类型命中是 `frontend_exception_type::af`
   - 不提前写成 `pf`

## 4. 说明

本目录没有迁入旧目录中的 PDF / XLSX 二进制附件，因为对应的 Markdown / CSV 已足够支持继续演进与维护。
