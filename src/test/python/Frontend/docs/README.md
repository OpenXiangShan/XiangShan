# Frontend BT 文档索引

当前目录只保留验证策略基线和功能覆盖率主线的 canonical 文档，不再保留阶段汇报、试点闭环、旧测试台设计或历史量化快照。

## 当前事实源

1. `03_funcov_model/skills.md`
   - 测试点驱动验证、功能覆盖率、真实 DUT artifact、反标和人工验收规范。
2. `02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv`
   - 唯一测试点、状态、testcase 和 evidence 主表。
3. `03_funcov_model/frontend_bt_functional_coverage_pilot.csv`
   - 唯一 active coverage registry；`pilot` 仅为兼容保留的历史文件名。

## 保留目录

- `01_testplan/`
  - Frontend BT 验证策略基线。
- `02_testpoint/`
  - `Frontend_testpoint_0525.xlsx`：原始 Excel 测试点基线，只作来源追溯。
  - `Frontend_testpoint_0525_coverage_backannotated.csv`：当前唯一可维护测试点主表。
- `03_funcov_model/`
  - `skills.md`：当前闭环规范。
  - `frontend_bt_functional_coverage_pilot.csv`：当前唯一 coverage registry。

## 使用约定

1. 不建立 registry 副本、个人测试点表或第二套功能覆盖率 sampler。
2. 新模型必须同步更新测试点、registry、sampler、testcase 和一致性测试。
3. 真实回归 evidence 写入 `src/test/python/Frontend/data/`；不同 DUT、registry 或 sampler 签名的结果不得合并。
4. 历史试点结果和旧 artifact 不得直接升级当前设计版本的 `HIT` 或 `CLOSED`。
5. `Frontend_testpoint_0525_coverage_backannotated.csv` 的每条记录必须占一行，单元格内禁止真实换行；需要换行时改写成分号或空格串联，不要让物理行号和 `csv.reader` 逻辑记录号分离。
