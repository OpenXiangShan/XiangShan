# mem_ut_uvm_v2 AI_DOC 同步执行 Review

## 1. Review 范围

关联 plan：

- `AI_DOC/plan/test_framework/plan/undo/ai_doc_mem_ut_uvm_v2_sync_plan_20260706.md`

本次只在目标工作树 `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2_mem_ut_uvm_v2`
执行文档和规则同步，不修改源工作树，不提交，不 push。

修改范围：

- `AI_DOC/**`
- `AGENTS.md`
- `mem_ut/ver/ut/memblock/rule/**/*.md`

未修改范围：

- RTL、UVM 源码、仿真脚本、`build_memblock/**`、`coupledL2/**`、`openLLC/**`

## 2. 执行前分支确认

| 工作树 | 要求分支 | 实际分支 | 结论 |
| --- | --- | --- | --- |
| `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2` | `mem_ut_uvm` | `mem_ut_uvm` | 通过 |
| `/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2_mem_ut_uvm_v2` | `mem_ut_uvm_v2` | `mem_ut_uvm_v2` | 通过 |

源工作树仅用于读取 plan、规则和 AI_DOC 文件清单；未向源工作树写入文件。

## 3. 同步范围和文件数量

| 项目 | 数量 |
| --- | ---: |
| 源工作树 `AI_DOC` 文件数 | 146 |
| 目标工作树同步前 `AI_DOC` 文件数 | 16 |
| 同步前源有而目标缺失的 `AI_DOC` 文件数 | 135 |
| 同步后、创建本 review 前目标 `AI_DOC` 文件数 | 151 |
| 创建本 review 后目标 `AI_DOC` 文件数 | 152 |

同步方式：

1. 使用源工作树 `AI_DOC` 文件清单作为完整性基线。
2. 使用 `rsync -a --ignore-existing` 只补齐目标缺失文件。
3. 不删除目标 V2 专属文件。
4. 同路径差异文件按 plan 的差异策略逐个合并或保留。

## 4. V2 专属保护清单

以下目标已有 V2 专属文档在同步后仍保留：

| 文件 | 处理结论 |
| --- | --- |
| `AI_DOC/analysis/interface/v2/memblock_v2_dut_interface_delta_seed_20260706.md` | 保留 |
| `AI_DOC/analysis/rtl/v2/memblock_rtl_generation_result_20260706.md` | 保留 |
| `AI_DOC/analysis/rtl/v2/mem_ut_uvm_v2_migration_v3_baseline_20260706.md` | 保留 |
| `AI_DOC/plan/test_framework/plan/do/mem_ut_uvm_v2_branch_migration_plan_20260706.md` | 保留 |
| `AI_DOC/plan/test_framework/review_doc/undo/mem_ut_uvm_v2_branch_migration_implementation_review_20260706.md` | 保留 |

本 review 文档自身是本次 V2 同步新增文档：

- `AI_DOC/plan/test_framework/review_doc/undo/ai_doc_mem_ut_uvm_v2_sync_review_20260706.md`

## 5. Rule profile 保护清单

目标工作树保留以下版本 profile 文件，源工作树没有同路径文件时不删除：

| 目录 | 文件 |
| --- | --- |
| `mem_ut/ver/ut/memblock/rule/version/v2` | `branch_policy.md`、`dut_interface_baseline.md`、`l2tlb_interface_profile.md`、`memblock_rtl_profile.md`、`verified_status.md` |
| `mem_ut/ver/ut/memblock/rule/version/v3` | `branch_policy.md`、`dut_interface_baseline.md`、`l2tlb_interface_profile.md`、`memblock_rtl_profile.md`、`verified_status.md` |

这些文件属于 V2/V3 版本 profile 路由，不属于源侧 AI_DOC 补齐范围。

本轮 review 后续修正中，已将上述 10 个当前有效规则 profile 的说明性正文中文化。
路径、branch 名、commit、命令、配置名、类名、文件名和日志关键字按语言规则保留
英文或代码原文。

## 6. 同路径差异采用策略

### 6.1 AI_DOC 同路径差异

| 文件 | 采用策略 | 原因 |
| --- | --- | --- |
| `AI_DOC/project_management/ai_doc_file_management_rule.md` | 合入源侧 `ai_doc_language_rule.md` 引用，最终与源侧一致 | V2 必须同步中文文档语言规则入口 |
| `AI_DOC/memblock_rtl生成规则.md` | 保留目标 V2/V3 profile 路由、`MEMBLOCK_XS_HOME` 和 worktree 隔离内容 | RTL 生成入口、firtool、main class、产物标准随版本变化，不能硬套源侧 V3-only 文本 |
| `AI_DOC/mem_ut远端编译仿真方案与流程.md` | 保留通用远端 flow，同时改为当前 worktree 相对路径和 `MEMBLOCK_XS_HOME` 规则 | 避免 V2 worktree 误读同级旧 `XiangShan` worktree 路径 |

### 6.2 memblock rule 同路径差异

| 文件 | 采用策略 | 原因 |
| --- | --- | --- |
| `memblock_code_comment_rule.md` | 保留目标中 `AI_DOC/analysis` 路径修正，其余规则沿用源侧普通规则 | 旧 `AI_DOC/design_plan` 路径已迁移 |
| `memblock_l2tlb_agent_rule.md` | 保留 V2/V3 `l2tlb_interface_profile.md` 读取要求和新 AI_DOC 路径，其余语义沿用源侧 | L2TLB/DTLB 连接层级随版本变化 |
| `memblock_latest_dut_adapt_rule.md` | 保留 V2/V3 `dut_interface_baseline.md` 读取要求，其余适配流程沿用源侧 | DUT 顶层端口和内部层级是版本相关基线 |
| `memblock_parameter_management_rule.md` | 保留 `AI_DOC/analysis`、`AI_DOC/plan` 路径修正，其余规则沿用源侧 | 参数管理规则本身不应恢复旧路径 |
| `memblock_sequence_add_rule.md` | 保留已修正的 plan 路径，其余 sequence 规则沿用源侧 | sequence plan 已迁移到 `AI_DOC/plan/test_framework/plan` |
| `memblock_update_code_rule.md` | 保留 `kunminghu-v2`/`kunminghu-v3` profile 化更新流程 | 当前目标分支 `mem_ut_uvm_v2` 默认应走 V2 branch profile |

### 6.3 AGENTS.md 合并结果

`AGENTS.md` 同时保留：

- V2/V3 profile 路由。
- `MEMBLOCK_XS_HOME / MEMBLOCK_PROJECT` 路径隔离规则。
- 源侧 `AI_DOC/project_management/ai_doc_language_rule.md` 文档入口。
- 文档语言规则触发条件：新增、修改或复查项目内说明性文档前必须先读该规则。
- `kunminghu-v2` 和 `kunminghu-v3` 的 profile 化更新规则。

## 7. 旧路径和语言检查

旧 `AI_DOC/design_plan` 命中不是当前有效规则路径残留，分为三类：

1. `AI_DOC/project_management/ai_doc_file_management_rule.md` 中的旧路径检查命令模式。
2. 本次同步 plan 和 V2 branch migration plan 中用于描述“旧路径必须修正”的历史文本。
3. 历史 review 中记录目录搬迁事实的说明。

当前 `mem_ut/ver/ut/memblock/rule` 下未保留旧 `AI_DOC/design_plan` 作为有效同步目标。

同步后保留的英文主要来自以下内容：

- 路径、命令、branch 名、文件名、类名、宏名和工具名。
- 历史 review、历史网页或日志中的原始英文标题、代码片段和输出。

这些属于 `ai_doc_language_rule.md` 允许保留的历史或标识符内容。本次新增 review 正文使用中文。

当前有效的 10 个版本 profile 不按“历史英文正文”豁免处理；已逐个改为中文说明文档：

- `mem_ut/ver/ut/memblock/rule/version/v2/branch_policy.md`
- `mem_ut/ver/ut/memblock/rule/version/v2/dut_interface_baseline.md`
- `mem_ut/ver/ut/memblock/rule/version/v2/l2tlb_interface_profile.md`
- `mem_ut/ver/ut/memblock/rule/version/v2/memblock_rtl_profile.md`
- `mem_ut/ver/ut/memblock/rule/version/v2/verified_status.md`
- `mem_ut/ver/ut/memblock/rule/version/v3/branch_policy.md`
- `mem_ut/ver/ut/memblock/rule/version/v3/dut_interface_baseline.md`
- `mem_ut/ver/ut/memblock/rule/version/v3/l2tlb_interface_profile.md`
- `mem_ut/ver/ut/memblock/rule/version/v3/memblock_rtl_profile.md`
- `mem_ut/ver/ut/memblock/rule/version/v3/verified_status.md`

## 8. 验证命令和结果

最终验证在目标工作树执行：

```bash
comm -23 <(cd ../XiangShan_V2/AI_DOC && find . -type f | sort) <(cd AI_DOC && find . -type f | sort)
comm -13 <(cd ../XiangShan_V2/AI_DOC && find . -type f | sort) <(cd AI_DOC && find . -type f | sort)
rg -n "AI_DOC/design_plan|AI_DOC/project_management/.*todo|AI_DOC/project_management/.*plan|AI_DOC/project_management/.*review" AI_DOC AGENTS.md mem_ut/ver/ut/memblock/rule
rg -n "kunminghu-v3|kunminghu-v2|mem_ut_uvm|mem_ut_uvm_v2|MEMBLOCK_PROJECT|MEMBLOCK_XS_HOME" AGENTS.md AI_DOC mem_ut/ver/ut/memblock/rule
git diff --check -- AI_DOC AGENTS.md mem_ut/ver/ut/memblock/rule
rg -n "## (Scope|Authority|Current Status|Branch Mapping|Update Flow|Non-goals|Semantic Boundary|Version Facts|Expected Output|Known Failure Pattern|Recorded Baseline|Verification Standard)|\\b(This|The|After|Do not|Every|Compare|Use generated|Recorded pass log|Pass standard)\\b" mem_ut/ver/ut/memblock/rule/version/v2 mem_ut/ver/ut/memblock/rule/version/v3
```

记录结果：

| 检查项 | 结果 |
| --- | --- |
| 源到目标 `AI_DOC` 缺失检查 `comm -23` | 0 项 |
| 目标相对源 `AI_DOC` 额外文件 `comm -13` | 6 项，均为 V2 专属保护文档或本 review |
| 旧路径/项目管理复合检查命中 | 39 项，其中项目管理规则入口命中属于预期结果 |
| 单独旧 `AI_DOC/design_plan` 命中 | 21 项，均为历史说明或检查模式，未作为当前有效 rule 目标 |
| 版本/profile 关键词检查 | 命中均来自 profile 路由、V2/V3 分支说明或路径隔离规则 |
| 版本 profile 英文段落检查 | 0 项，未发现旧英文章节标题或整段英文说明残留 |
| `git diff --check -- AI_DOC AGENTS.md mem_ut/ver/ut/memblock/rule` | 通过 |

未执行远端编译/仿真；本 plan 只同步文档和规则，不修改 RTL 或 UVM 源码。

## 9. AC-1 到 AC-7 结论

| 验收项 | 结论 | 说明 |
| --- | --- | --- |
| AC-1 文档完整性 | 通过 | 源到目标 `AI_DOC` 的 `comm -23` 为空 |
| AC-2 V2 专属文档保护 | 通过 | 5 个既有 V2 专属文档和本 review 均保留 |
| AC-3 规则差异可解释 | 通过 | AI_DOC、memblock rule 和 AGENTS.md 差异均在本文列出采用策略 |
| AC-4 规则以之前版本为主 | 通过 | 普通规则沿用源侧，版本/profile/worktree 差异保留目标 V2 内容 |
| AC-5 目录拆分符合约束 | 通过 | 仅保留 `AI_DOC/analysis/rtl/v2` 等 V2 专属分析目录，未新增无必要顶层拆分 |
| AC-6 语言规则同步 | 通过 | `ai_doc_language_rule.md` 已同步，`AGENTS.md` 和 `ai_doc_file_management_rule.md` 已引用 |
| AC-7 验证命令通过 | 通过 | 文件完整性、旧路径、profile 关键词和 diff 格式检查已记录 |

## 10. 剩余风险

1. 源工作树本身存在未提交和未跟踪的 AI_DOC 文档，本次已按 `find AI_DOC -type f` 纳入同步，但后续源侧若继续新增未跟踪文档，需要再次同步。
2. 历史 review 和迁移 plan 中仍会出现旧 `AI_DOC/design_plan` 字样，属于历史说明；若后续要做到全文零命中，需要另起历史文档清理任务。
3. 本次未验证 V2 RTL 生成或远端编译仿真，后续应按独立 V2 RTL/DUT 适配 plan 处理。

## 11. 本 agent 最终复查结论

本 agent 在 subagent 完成同步后执行了最终复查：

1. 复查源到目标 `AI_DOC` 缺失清单，`comm -23` 为空。
2. 复查目标额外 `AI_DOC` 文件，结果只包含 5 个 V2 专属保护文档和本 review 文档。
3. 复查 10 个 `mem_ut/ver/ut/memblock/rule/version/{v2,v3}` profile，确认当前有效规则正文已中文化，英文仅保留在路径、branch、commit、命令、配置名和固定术语中。
4. 复查 `AGENTS.md`，确认同时保留 V2/V3 profile 路由、`MEMBLOCK_XS_HOME / MEMBLOCK_PROJECT` 路径隔离规则和 `ai_doc_language_rule.md` 入口。
5. 复查 `AI_DOC/project_management/ai_doc_file_management_rule.md`，确认已引用 `ai_doc_language_rule.md`。
6. 复查 `git diff --check -- AI_DOC AGENTS.md mem_ut/ver/ut/memblock/rule`，结果通过。

最终结论：最后一轮 review 未发现必须修改项，本文 plan 的文档同步和规则一致性要求已闭环。
