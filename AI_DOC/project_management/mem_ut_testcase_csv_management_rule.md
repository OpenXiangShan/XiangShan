# mem_ut 测试用例 CSV 清单管理规则

本文约束 `mem_ut` 新增测试用例时的 CSV 清单分类、存放位置、必填字段和结果维护方式。

## 1. 触发条件

出现以下任一情况时，必须执行本规则：

- 新增真实 DUT 测试用例、testcase preset cfg 或真实 DUT 端到端测试场景。
- 新增 soft test、software-only 测试、框架自测或定向负向检查场景。
- 在已有 testcase 中新增可独立说明用途和判定结果的测试子场景。
- 删除、重命名、拆分或合并已有测试用例。

## 2. 存放目录

所有测试用例清单 CSV 统一放在：

```text
AI_DOC/seq/
```

仓库实际文档根目录名称为 `AI_DOC`，因此不得另建大小写不同的 `AI/doc/seq` 目录。

测试用例 CSV 不得放入：

- `AI_DOC/analysis/testcase_flow/`
- 仿真输出目录。
- 临时 work 目录或个人目录。

`AI_DOC/analysis/testcase_flow/` 仍用于保存测试场景流程、原理和源码分析文档，不用于保存用例清单 CSV。

## 3. 分类要求

真实 DUT 与 soft/framework 用例必须使用两份独立 CSV 文件，不得混合记录：

```text
<feature>_real_dut_testcases_<YYYYMMDD>.csv
<feature>_soft_framework_testcases_<YYYYMMDD>.csv
```

- 真实 DUT CSV：记录会拉起真实 DUT 仿真、通过 DUT interface 发激励并检查 DUT 结果的用例。
- soft/framework CSV：记录不依赖真实 DUT 执行结果的框架自测、参考模型定向测试、API 单元测试和负向检查。

## 4. 用例登记粒度

- 每个新测试用例必须单独占一行。
- 同一个 UVM testcase 中包含多个可独立说明用途和结果的定向子场景时，每个子场景也必须单独占一行。
- 不得使用“覆盖全部相关场景”等笼统条目代替具体用例登记。
- 用例名称必须与源码 testcase、cfg 名称或清晰稳定的逻辑子场景名称对应。

## 5. CSV 字段

每份 CSV 至少包含：

```text
testcase_name,purpose
```

推荐使用以下完整字段：

```text
test_type,testcase_name,entry,cfg,purpose,expected_result,status
```

字段含义：

- `test_type`：`real_dut` 或 `soft_framework`。
- `testcase_name`：测试用例或独立测试子场景名称。
- `entry`：UVM testcase、virtual sequence 或执行入口。
- `cfg`：使用的 plusarg cfg；不适用时填写 `default` 或留空。
- `purpose`：该用例验证的功能、异常或约束行为。
- `expected_result`：预期写回、异常、内存效果或 checker 结果。
- `status`：待执行、通过、失败或受阻状态；推荐在状态中附带执行日期。

## 6. 同步维护要求

- 新增用例源码或 cfg 时，必须在同一轮修改中同步更新对应 CSV。
- 用例执行完成后，必须及时更新 `status`，不得只在聊天回复或仿真日志中记录结果。
- 用例重命名、删除、拆分或合并时，必须同步修改 CSV。
- CSV 中记录的入口、cfg、用途和实际实现必须保持一致。
- 新增真实 DUT 和 soft/framework 用例时，两类 CSV 都要检查；没有新增的类别不需要添加空占位行。

## 7. 完成检查

完成用例和 CSV 修改后，至少检查：

1. CSV 能被标准 CSV 工具正确解析。
2. 真实 DUT 与 soft/framework 用例没有混在同一文件。
3. 每个新增用例或独立子场景都有对应行。
4. `testcase_name` 没有重复。
5. `purpose` 能明确说明验证目标。
6. 已执行用例的 `status` 与日志结果一致。
7. 不再存在应迁移到 `AI_DOC/seq` 的新用例 CSV。

格式检查命令：

```bash
git diff --check -- AI_DOC/seq AI_DOC/project_management/mem_ut_testcase_csv_management_rule.md AGENTS.md
```
