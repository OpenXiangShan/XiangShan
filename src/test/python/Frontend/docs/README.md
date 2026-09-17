# Frontend BT 文档索引

当前工作目录维护验证策略、测试点和功能覆盖率的 canonical 文档。旧过程报告的有效结论进入验证方案、闭环规范及可执行检查；运行过程和旧状态留在 artifact/Git 历史，不作为当前执行指令。

## 当前事实源

1. `03_funcov_model/skills.md`
   - 测试点驱动验证、功能覆盖率、真实 DUT artifact、反标和人工验收规范。
2. `02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv`
   - 唯一测试点、状态、testcase 和 evidence 主表。
3. `03_funcov_model/frontend_bt_functional_coverage_pilot.csv`
   - 唯一 active coverage registry；`pilot` 仅为兼容保留的历史文件名。
4. `04_alignment/Frontend_BT_设计验证对齐材料模板.md`
   - 设计与验证周度对齐材料模板，包含版本基线、验证状态、变更影响和遗留事项。

## 保留目录

- `01_testplan/`
  - [Frontend BT 验证方案](01_testplan/Frontend_BT_验证方案.md)：策略基线；第 2.3 节维护 IFU 集成约束、待 review 问题和验收方向，不复制覆盖率状态表。
- `02_testpoint/`
  - `Frontend_testpoint_0525.xlsx`：原始 Excel 测试点基线，只作来源追溯。
  - `Frontend_testpoint_0525_coverage_backannotated.csv`：当前唯一可维护测试点主表。
- `03_funcov_model/`
  - `skills.md`：当前闭环规范。
  - `frontend_bt_functional_coverage_pilot.csv`：当前唯一 coverage registry，与测试点、sampler 和 testcase 同步迭代，保留原路径。
  - 不放逐 bin 排查日志、阶段覆盖率快照或已结束的迁移记录。
- `04_alignment/`
  - `Frontend_BT_设计验证对齐材料模板.md`：只包含字段、统计口径和表格的对齐模板，待评审后填写实际数据。

## 使用约定

1. 不建立 registry 副本、个人测试点表或第二套功能覆盖率 sampler。
2. 新模型必须同步更新测试点、registry、sampler、testcase 和一致性测试。
3. 真实回归 evidence 写入 `src/test/python/Frontend/data/`；不同 DUT、registry 或 sampler 签名的结果不得合并。
4. 历史试点结果和旧 artifact 不得直接升级当前设计版本的 `HIT` 或 `CLOSED`。
5. `Frontend_testpoint_0525_coverage_backannotated.csv` 的每条记录必须占一行，单元格内禁止真实换行；需要换行时改写成分号或空格串联，不要让物理行号和 `csv.reader` 逻辑记录号分离。
6. 临时排查过程保存在 run artifact 或工作日志中，不持续向 `03_funcov_model/` 新增阶段 MD。需要长期保留的 review 结论应注明适用 SHA 和证据位置，进入现有正式文档或可执行检查；完成沉淀的过程 MD 可删除，由 Git 历史追溯，不能从旧报告中的 `current`、`Complete` 或 HIT 数字推导当前 DUT 状态。

## 已沉淀的历史记录

2026-09-10 整理的八份旧报告不再保留工作树副本。`v3_mainline_migration_gate`、`v3_e5c_runtime_risk_reaudit` 的接口/风险约束进入验证方案第 2.3.1 节及闭环规范第 11.1 节；`modeled_producer_integrity_audit` 的方法进入闭环规范第 7.2 节。BIN-814、904、940、973、1095/1096 的五份评审进入验证方案第 2.3.2 节；[funcov 实现说明](../env/funcov/README.md) 链接对应 sampler、unit、contract 和 DUT canary。

八份原文均可在仓库根目录只读查看，无需 checkout 或恢复工作树：

```bash
git show 1045f5761a423ffb20b45d9ec3b69886d243f39f:src/test/python/Frontend/docs/03_funcov_model/<原文件名>
```

原文件名可用 `git ls-tree --name-only 1045f5761:src/test/python/Frontend/docs/03_funcov_model/` 查询。旧报告中的 run_id、波形位置和实验过程由此追溯，实际 artifact 未删除。
