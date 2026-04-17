# Frontend BT Coverage Migration Notes

## 1. 目标

本文件用于说明 Frontend BT 文档资产迁移到当前 `src/test/python/Frontend/docs/` 的状态。

迁移完成后的目标是：

1. 策略、测试点、覆盖率建模、交接和试点闭环文档都在当前 `docs/` 内可继续维护。
2. 后续文档演进不再依赖旧的独立 `frontend_bt` 目录或 `uc_agent_workspace` 目录。
3. 当前主线真实状态以：
   - 当前 `docs/`
   - 当前主线 testcase / funcov recorder
   - 主控目录的真实 DUT 证据
   为准。

## 2. 已迁入的文档层

截至 2026-04-17，当前 `docs/` 已经具备以下层次：

### 2.1 当前主线闭环文档

- `frontend_bt_functional_coverage_pilot.csv`
- `frontend_bt_pilot_acceptance_status.md`
- `frontend_bt_coverage_migration_notes.md`
- `05_试点闭环与主线迁移/Frontend_BT_首轮覆盖率试点打通总结.md`

### 2.2 从旧文档体系迁入的基线资产

- `01_验证策略及方案/`
- `02_测试点分解/`
- `03_功能覆盖率建模/`
- `04_汇报与交接/`
- `06_测试台设计/`

这些目录已经覆盖了你提出的关键文档类型：

- 测试方案
- 验证策略
- 测试点
- 功能覆盖率建模
- 汇报/交接材料
- 旧 frontend_bt testbench 设计沉淀

## 3. 当前 canonical 使用方式

### 3.1 当前 active 文件

当前维护 Frontend BT 试点闭环时，优先以以下文件为准：

- `README.md`
- `frontend_bt_functional_coverage_pilot.csv`
- `frontend_bt_pilot_acceptance_status.md`
- `05_试点闭环与主线迁移/Frontend_BT_首轮覆盖率试点打通总结.md`

### 3.2 当前主线真实证据目录

当前真实 DUT 证据目录使用：

- `/nfs/home/jiabowen/ai_workspace/XiangShan_frontend_bt_jiabowen/src/test/python/Frontend/data/`

这里保存当前可引用的：

- `*.fst`
- `*.dat`
- `*.funcov.json`
- `*.funcov.summary.csv`
- `*.funcov.unhit.csv`

## 4. 当前迁移后的真实状态

### 4.1 文档侧

当前 doc worktree 已经达到以下状态：

1. 旧的 V3 策略/测试点/funcov 建模资产已经落到当前目录。
2. 旧 `frontend_bt/testbench/docs` 的测试台设计文档已经落到当前目录。
3. 当前 `BIN-004` / `BIN-012` 的经理口径已经可以直接在当前目录下维护。

### 4.2 主线闭环侧

当前主线已知基线为：

1. `BIN-004` 已通过。
2. `BIN-012` 已通过。
3. `BIN-012` 的当前异常语义口径保持为：
   - raw funcov 命中 `frontend_exception_type::af`
   - 不提前写成 `pf`

## 5. 未迁入内容说明

当前没有迁入旧目录中的 PDF / XLSX 二进制附件，原因是：

1. 现有 Markdown / CSV 已足够支撑继续演进。
2. 当前工作重点是把可编辑、可追踪、可迭代的文档留在主目录。

如果后续确实需要把某个二进制附件也纳入当前目录，再单独补。

## 6. 后续维护原则

1. 文档变更优先在当前 `src/test/python/Frontend/docs/` 下进行。
2. 新的环境结论应优先回灌到：
   - `frontend_bt_pilot_acceptance_status.md`
   - `frontend_bt_functional_coverage_pilot.csv`
   - `05_试点闭环与主线迁移/Frontend_BT_首轮覆盖率试点打通总结.md`
3. 当前目录是文档演进主目录，不再把旧目录当成继续维护的 canonical 位置。
