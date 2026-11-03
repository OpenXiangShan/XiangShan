# Frontend BT 文档索引

本目录存放 Frontend BT 的验证方案、测试点、功能覆盖率规范和对齐材料。

## 文档入口

- `01_testplan/`
  - [Frontend BT 验证方案](01_testplan/Frontend_BT_验证方案.md)：策略基线；第 2.3 节维护 IFU 集成约束、待 review 问题和验收方向，不复制覆盖率状态表。
  - [方案版本索引](01_testplan/README.md)：保留 V1.0～V1.3 的正文、原始 Word/PDF、BPU 专题和共享参考资料。
  - [V1.3 验证方案](01_testplan/v1.3/Frontend_BT_验证方案_v1.3.md)：组内修订稿，Word 为内容源；测试点状态仍以当前主表为准。
  - [V0.1 验证设计](01_testplan/verification_design/v0.1/Frontend_BT_验证设计_场景推导与测试点分解_v0.1.md)：独立评审稿，保留场景推导与分解方法。
- `02_testpoint/`
  - [测试点拆分准则](02_testpoint/testpoint-decomposition-guidelines.md)：测试点拆分方法与完备性检查。
  - `Frontend_testpoint_v1.0.csv`：测试点主表。
- `03_funcov_model/`
  - `skills.md`：测试点驱动验证与覆盖率闭环规范。
  - `frontend_bt_functional_coverage_pilot.csv`：功能覆盖率 registry。
- `04_alignment/`
  - `Frontend_BT_设计验证对齐材料模板.md`：设计与验证对齐材料模板。

`Frontend_testpoint_v1.0.csv` 的每条记录占一行；单元格内不要使用真实换行。
