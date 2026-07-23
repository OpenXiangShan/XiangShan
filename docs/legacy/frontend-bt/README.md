# Frontend BT 历史参考包

> 状态：历史参考，非当前规范。
>
> 适用范围：解释旧 `uc_agent_workspace` 中的历史问题、IFU 设计意图、
> 验证资产演进和测试点来源。当前实现、命令、测试状态和验收结果必须以
> node031 的 `frontend-bt-jiabowen` 当前分支为准。

## 文档组成

1. `01_历史问题与风险基线.md`
   - 从真实历史任务、Bug 分析和覆盖性评估中提炼风险、根因和验证缺口。
2. `02_IFU历史设计意图与验证要点.md`
   - 汇总旧 IFU spec 中仍有解释力的模块职责、关键边界和当前复核点。
3. `03_验证资产演进与迁移说明.md`
   - 说明旧 UCAgent/UVM 资料如何演进到当前 Python/toffee 真实 DUT 主线。
4. `04_旧测试点来源与当前主表关系.md`
   - 解释旧 CSV/XLSX、旧 Bin 与当前 canonical 主表/registry 的关系。

## 当前事实源

当前 Frontend BT 的 canonical 文档输入和可执行输入位于 node031 新环境：

- 策略基线：`src/test/python/Frontend/docs/01_验证策略及方案/Frontend_BT_验证方案.md`
- 测试点主表：`src/test/python/Frontend/docs/02_测试点分解/Frontend_testpoint_0525_coverage_backannotated.csv`
- coverage registry：`src/test/python/Frontend/docs/03_功能覆盖率建模/frontend_bt_functional_coverage_pilot.csv`
- 闭环规则：`src/test/python/Frontend/docs/03_功能覆盖率建模/skills.md`
- recorder/predicate：`src/test/python/Frontend/env/functional_coverage.py`、`src/test/python/Frontend/env/funcov.py`
- 自动反标：`src/test/python/Frontend/tools/backannotate_funcov.py`
- testcase、汇编用例和回归脚本：`src/test/python/Frontend/tests/`、`src/test/python/Frontend/tests/asm_cases/`、`src/test/python/Frontend/scripts/`
- 运行约束：`docs/agents/frontend-verification.md`，以及当前 RTL、生成接口和 build manifest

历史资料不得建立第二套主表、coverage registry、sampler 或运行流程；本包只解释来源和风险。

## 使用原则

1. 历史问题的根因和触发条件可用于提出测试假设。
2. 历史接口、信号、状态机和优先级必须对照当前 RTL 复核。
3. 历史测试点只能用于来源追溯，不能直接回标当前状态。
4. 历史波形、旧 schema artifact 和阶段汇报不能证明当前闭环。
5. 当前文档与历史资料冲突时，以当前 RTL、canonical 文档和设计 owner 结论为准。

## 原始资料位置与恢复

旧资料的完整归档、闭环整理和裁剪快照分别由 node031 仓库中的以下 tag 固定：

| tag | 对应提交 | 用途 |
|---|---|---|
| `archive/frontend-bt-legacy-source-20260723` | `da4e22041` | 完整归档旧工作区和 IFU 文档 |
| `archive/frontend-bt-legacy-closure-20260723` | `ccdf66227` | 闭环材料进入主线前的整理快照 |
| `archive/frontend-bt-legacy-pruned-20260723` | `b7e4c56fa` | 裁剪阶段汇报、旧测试台和重复快照后的快照 |

这些提交不是当前 `frontend-bt` 分支的祖先；tag 才是恢复原文的稳定入口。审计时按文件只读查看，例如
`git show archive/frontend-bt-legacy-source-20260723:<path>`，不要把整套旧资料恢复到 active 文档目录。

## 维护规则

- 本目录只接受跨版本仍有解释力的历史事实或来源说明。
- 不记录当前进展、临时命令、人员排期或一次性汇报材料。
- 新发现若已成为当前实现规则，应更新 canonical 文档，而不是扩充本目录。
