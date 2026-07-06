# mem_ut_uvm_v2 AI_DOC 全量同步与规则一致性 Plan

## 目标说明

本文规划将当前 `mem_ut_uvm` 工作树中的 `AI_DOC` 文档库完整同步到
`mem_ut_uvm_v2` 工作树，并整理规则文件差异，保证 V2 分支后续文档、规则和
review 输入不遗漏历史资料。

当前同步目标不是修改 RTL、UVM 源码或执行 V2 DUT 适配。本 plan 只处理文档、
规则、索引和路径路由，为后续 V2 RTL 生成与 V2 DUT 适配提供完整资料基线。

同步原则：

1. `AI_DOC` 文档必须在 `mem_ut_uvm_v2` 中全量存在，不允许遗漏当前
   `mem_ut_uvm` 中已有的分析、flow、plan、review、web、skills 和项目管理文档。
2. 规则文件以当前/之前版本要求为主，除非差异属于版本、分支、路径或
   worktree 隔离问题。
3. 有冲突或内容不一致的文件必须在本 plan 和最终 review 文档中标注差异点、
   采用版本和原因。
4. RTL 分析文档允许按 `v2`、`v3` 拆分；其他文档默认不按 V2/V3 拆目录，
   只有确实存在版本专属内容时才在文件名或正文中标注版本。
5. 同步后的说明性文档必须使用中文。英文只允许保留在路径、命令、日志、
   branch 名、文件名、类名、信号名、固定术语、历史英文网页标题或必须原样保留的
   外部内容中；如果保留历史英文正文或英文网页版本，必须在最终 review 中列清单并说明原因。

## 当前基线

### 源工作树

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2
当前分支：mem_ut_uvm
AI_DOC 文件数：145
```

当前源工作树存在未提交文档规则改动：

```text
M  AGENTS.md
M  AI_DOC/project_management/ai_doc_file_management_rule.md
?? AI_DOC/project_management/ai_doc_language_rule.md
?? AI_DOC/plan/test_framework/plan/undo/mem_ut_uvm_v2_branch_migration_plan_20260706.md
```

执行本 plan 前必须冻结源 `AI_DOC` 基线：已确认属于文档规则更新的未提交或未跟踪
`AI_DOC` 文件必须纳入同步范围，或先在源分支提交后再同步。不得因为文件未跟踪而遗漏
`ai_doc_language_rule.md`、本迁移 plan 或其他已确认文档。

### 目标工作树

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2_mem_ut_uvm_v2
目标分支：mem_ut_uvm_v2
AI_DOC 文件数：16
```

目标工作树已有 V2 专属文档，不能被源工作树同名缺失状态误删：

```text
AI_DOC/analysis/interface/v2/memblock_v2_dut_interface_delta_seed_20260706.md
AI_DOC/analysis/rtl/v2/memblock_rtl_generation_result_20260706.md
AI_DOC/analysis/rtl/v2/mem_ut_uvm_v2_migration_v3_baseline_20260706.md
AI_DOC/plan/test_framework/plan/do/mem_ut_uvm_v2_branch_migration_plan_20260706.md
AI_DOC/plan/test_framework/review_doc/undo/mem_ut_uvm_v2_branch_migration_implementation_review_20260706.md
```

## 验收标准

- AC-1：文档完整性
  - 正向检查：`mem_ut_uvm_v2` 中包含源工作树 `AI_DOC` 下全部应同步文件。
  - 反向检查：`comm -23` 必须为空，不允许以“暂不同步”作为通过条件。
  - 如果 RTL 分析文件发生 `AI_DOC/analysis/rtl/v3` 路径迁移，必须提供源路径到目标路径
    的迁移映射表，并证明内容没有遗漏；不能直接让 `comm -23` 残留缺失项。

- AC-2：V2 专属文档保护
  - 正向检查：V2 专属迁移、V2 RTL 生成结果和 V2 interface delta 文档仍保留。
  - 反向检查：同步过程中不得用源工作树缺失状态删除 V2 专属文件。

- AC-3：规则差异可解释
  - 正向检查：所有同路径但内容不同的规则文件均在本 plan 的差异表中列出。
  - 正向检查：最终 review 文档记录每个差异采用的版本和原因。
  - 反向检查：不得出现“文件不同但未说明原因”的规则文件。

- AC-4：规则以之前版本为主
  - 正向检查：普通管理规则、测试框架规则、agent/cfg/sequence 等非版本差异内容，
    以当前 `mem_ut_uvm` 的既有规则为主。
  - 例外检查：只有版本 profile、`kunminghu-v2`/`kunminghu-v3`、`mem_ut_uvm_v2`、
    `MEMBLOCK_XS_HOME`、worktree 名和旧 `AI_DOC/design_plan` 路径修正允许采用
    V2 差异。

- AC-5：目录拆分符合约束
  - 正向检查：`AI_DOC/analysis/rtl/v2`、`AI_DOC/analysis/rtl/v3` 可用于 RTL
    版本分析。
  - 反向检查：`mem_ut_flow_doc`、`plan/test_framework`、`review_doc`、`web`、
    `skills`、`project_management` 不新增无必要的 V2/V3 顶层拆分目录。

- AC-6：语言规则同步
  - 正向检查：`AI_DOC/project_management/ai_doc_language_rule.md` 存在于
    `mem_ut_uvm_v2`。
  - 正向检查：`AGENTS.md` 和 `ai_doc_file_management_rule.md` 均引用该规则。
  - 反向检查：同步后的说明性文档不得新增英文正文；历史英文正文或英文网页版本必须在
    review 文档中列出保留原因。

- AC-7：验证命令通过
  - 正向检查：`git diff --check -- AI_DOC AGENTS.md mem_ut/ver/ut/memblock/rule`
    无格式错误。
  - 正向检查：路径差异、旧路径残留和缺失文件检查均有记录。

## 路径边界

### 最大范围

允许修改以下路径：

```text
AI_DOC/**
AGENTS.md
mem_ut/ver/ut/memblock/rule/**/*.md
```

如需为 V2/V3 profile 补齐规则入口，可修改：

```text
mem_ut/ver/ut/memblock/rule/version/**
```

### 最小范围

最低必须完成：

```text
AI_DOC 全量同步
AI_DOC/project_management/ai_doc_language_rule.md 同步
AGENTS.md 文档入口同步
规则差异表和最终 review 文档
```

### 禁止范围

本 plan 不修改以下内容：

```text
src/**
scripts/generate_memblock_rtl.sh
build.sc
mem_ut/ver/ut/memblock/**/*.sv
mem_ut/ver/ut/memblock/sim/**
build_memblock/**
coupledL2/**
openLLC/**
```

如果执行过程中发现必须修改源码或 RTL 生成脚本，应停止本文档同步任务，另起
V2 RTL 生成或 V2 DUT 适配 plan。

## 同步目录策略

### 必须从源工作树同步到 V2 的目录

实际同步以源工作树 `find AI_DOC -type f` 生成的全量文件清单为准。下列目录只是当前
已知文档类别摘要，不作为排除条件；即使当前某些目录为空，例如
`AI_DOC/analysis/testcase_flow` 或 `AI_DOC/plan/rm_review_doc`，后续执行时也必须按
全量清单判断是否需要同步。

```text
AI_DOC/analysis/framework_design/**
AI_DOC/analysis/interface/**
AI_DOC/analysis/rtl/**
AI_DOC/analysis/source_sv/**
AI_DOC/analysis/testcase_flow/**
AI_DOC/mem_ut_flow_doc/**
AI_DOC/plan/rm_plan/**
AI_DOC/plan/rm_review_doc/**
AI_DOC/plan/test_framework/plan/**
AI_DOC/plan/test_framework/review_doc/**
AI_DOC/project_management/**
AI_DOC/skills/**
AI_DOC/web/**
AI_DOC/memblock_rtl生成规则.md
AI_DOC/mem_ut远端编译仿真方案与流程.md
```

### RTL 分析目录规则

RTL 分析允许版本拆分：

```text
AI_DOC/analysis/rtl/v2/**
AI_DOC/analysis/rtl/v3/**
```

本次同步默认不迁移源工作树已有 V3 RTL 分析路径，只保留 V2 已有的
`AI_DOC/analysis/rtl/v2` 文档并补齐源工作树文档。若后续确实要迁移到
`AI_DOC/analysis/rtl/v3`，必须另起路径迁移清单，更新所有引用链接，并在 review 中
列出源路径到目标路径的映射，证明内容没有遗漏。

### 其他目录规则

除 RTL 分析外，以下目录默认保持通用目录，不按 V2/V3 再拆：

```text
AI_DOC/mem_ut_flow_doc
AI_DOC/plan/test_framework/plan
AI_DOC/plan/test_framework/review_doc
AI_DOC/plan/rm_plan
AI_DOC/analysis/source_sv
AI_DOC/analysis/framework_design
AI_DOC/web
AI_DOC/skills
AI_DOC/project_management
```

如果某个文件确实只适用于 V2 或 V3，应优先在文件名和文档开头标注适用版本，
不要先建立大范围版本目录。

## V2 保护文件清单

同步时必须保护以下 V2 专属文件：

| 文件 | 处理方式 | 原因 |
| --- | --- | --- |
| `AI_DOC/analysis/interface/v2/memblock_v2_dut_interface_delta_seed_20260706.md` | 保留 | V2 DUT interface 初始差异分析 |
| `AI_DOC/analysis/rtl/v2/memblock_rtl_generation_result_20260706.md` | 保留 | V2 RTL 生成结果记录 |
| `AI_DOC/analysis/rtl/v2/mem_ut_uvm_v2_migration_v3_baseline_20260706.md` | 保留 | V2 迁移前 V3 基线记录 |
| `AI_DOC/plan/test_framework/plan/do/mem_ut_uvm_v2_branch_migration_plan_20260706.md` | 保留 | 已执行的 V2 分支迁移 plan |
| `AI_DOC/plan/test_framework/review_doc/undo/mem_ut_uvm_v2_branch_migration_implementation_review_20260706.md` | 保留 | V2 分支迁移 implementation review |

## V2 专属规则 Profile 保护清单

同步时必须保护以下 V2 目标工作树已有的 profile 文件。这些文件属于版本、路径和已验证
状态差异，不在源工作树中存在时不能删除。

| 文件 | 处理方式 | 原因 |
| --- | --- | --- |
| `mem_ut/ver/ut/memblock/rule/version/v2/branch_policy.md` | 保留 | V2 工作分支与 `kunminghu-v2` 上游关系 |
| `mem_ut/ver/ut/memblock/rule/version/v2/dut_interface_baseline.md` | 保留 | V2 DUT interface 基线 |
| `mem_ut/ver/ut/memblock/rule/version/v2/l2tlb_interface_profile.md` | 保留 | V2 L2TLB/DTLB 连接差异 |
| `mem_ut/ver/ut/memblock/rule/version/v2/memblock_rtl_profile.md` | 保留 | V2 RTL 生成入口和产物规则 |
| `mem_ut/ver/ut/memblock/rule/version/v2/verified_status.md` | 保留 | V2 已验证状态 |
| `mem_ut/ver/ut/memblock/rule/version/v3/branch_policy.md` | 保留 | V3 工作分支与 `kunminghu-v3` 上游关系 |
| `mem_ut/ver/ut/memblock/rule/version/v3/dut_interface_baseline.md` | 保留 | V3 DUT interface 基线 |
| `mem_ut/ver/ut/memblock/rule/version/v3/l2tlb_interface_profile.md` | 保留 | V3 L2TLB/DTLB 连接基线 |
| `mem_ut/ver/ut/memblock/rule/version/v3/memblock_rtl_profile.md` | 保留 | V3 RTL 生成入口和产物规则 |
| `mem_ut/ver/ut/memblock/rule/version/v3/verified_status.md` | 保留 | V3 已验证状态 |

## 已知差异与采用策略

### AI_DOC 同路径差异

| 文件 | 差异类型 | 采用策略 | 必须标注点 |
| --- | --- | --- | --- |
| `AI_DOC/project_management/ai_doc_file_management_rule.md` | 语言规则入口差异 | 采用源工作树新增语言规则入口，并同步到 V2 | V2 当前缺少 `ai_doc_language_rule.md` 引用 |
| `AI_DOC/memblock_rtl生成规则.md` | 版本 profile 与路径差异 | 以源规则为基础，合入 V2 profile 路由、`MEMBLOCK_XS_HOME` 和 V2 构建差异 | V2 不能硬套 V3 firtool/main class/config |
| `AI_DOC/mem_ut远端编译仿真方案与流程.md` | worktree 路径差异 | 保留通用远端 flow，采用 V2 的 `MEMBLOCK_XS_HOME` 路径隔离修正 | 避免 V2 误读同级 `XiangShan` worktree RTL |

### memblock rule 同路径差异

| 文件 | 差异类型 | 采用策略 | 必须标注点 |
| --- | --- | --- | --- |
| `memblock_code_comment_rule.md` | 旧路径修正 | 采用 V2 中 `AI_DOC/analysis/...` 路径修正，其余规则以源工作树为主 | 旧 `AI_DOC/design_plan` 已不符合当前目录规则 |
| `memblock_l2tlb_agent_rule.md` | 版本 profile + 旧路径修正 | 合入 V2 profile 读取要求和 `AI_DOC/analysis` 路径修正，其余语义以源工作树为主 | L2TLB/DTLB 连接点可能随 V2/V3 变化 |
| `memblock_latest_dut_adapt_rule.md` | 版本 profile 差异 | 合入 V2/V3 `dut_interface_baseline.md` 读取要求，其余适配流程以源工作树为主 | DUT 顶层端口和内部层级版本相关 |
| `memblock_parameter_management_rule.md` | 旧路径修正 | 采用 V2 中已修正的 `AI_DOC/analysis`、`AI_DOC/plan` 路径，其余规则以源工作树为主 | diff 检查范围应覆盖 `AI_DOC/analysis` 和 `AI_DOC/plan` |
| `memblock_sequence_add_rule.md` | 旧 plan 路径修正 | 采用 V2 中已修正的 plan 路径，其余 sequence 规则以源工作树为主 | 不恢复旧 `AI_DOC/design_plan` 路径 |
| `memblock_update_code_rule.md` | 版本 profile 差异 | 采用 V2/V3 profile 化更新流程，不保留 V3-only `kunminghu-v3` 硬编码 | 当前分支为 `mem_ut_uvm_v2` 时默认 rebase `kunminghu-v2` |

### AGENTS.md 差异

| 差异点 | 采用策略 | 原因 |
| --- | --- | --- |
| V2 已有 version profile 路由，源工作树没有完整路由 | 合入路由，但通用规则描述仍以源工作树为主 | V2/V3 共存需要按分支选择 profile |
| 源工作树已有 `ai_doc_language_rule.md` 入口，V2 缺失 | 同步到 V2 | 后续文档必须默认中文 |
| V2 已将 `MEMBLOCK_PROJECT` 扩展为 `MEMBLOCK_XS_HOME / MEMBLOCK_PROJECT` | 采用 V2 路径隔离修正 | 多 worktree 共存时避免读错 RTL |
| 源工作树仍有 V3-only 更新描述 | 在 V2 中改为 profile 化描述 | V2 分支必须默认跟随 `kunminghu-v2` |

## 执行步骤

### 1. 执行前状态快照

在源工作树执行：

```bash
cd /nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2
git status --short --branch
test "$(git branch --show-current)" = "mem_ut_uvm"
find AI_DOC -type f | sort > /tmp/mem_ut_ai_doc_source_files.txt
find mem_ut/ver/ut/memblock/rule -type f -name '*.md' | sort > /tmp/mem_ut_rule_source_files.txt
```

在 V2 工作树执行：

```bash
cd /nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2_mem_ut_uvm_v2
git status --short --branch
test "$(git branch --show-current)" = "mem_ut_uvm_v2"
find AI_DOC -type f | sort > /tmp/mem_ut_ai_doc_v2_before_files.txt
find mem_ut/ver/ut/memblock/rule -type f -name '*.md' | sort > /tmp/mem_ut_rule_v2_before_files.txt
```

如果任一 `test "$(git branch --show-current)" = ...` 失败，必须停止；不能在错误分支上同步。
如果 V2 工作树存在非本文档同步相关脏改动，应先停止并记录，不能直接覆盖。

### 2. 生成缺失清单和差异清单

在源工作树执行：

```bash
comm -23 \
  <(cd AI_DOC && find . -type f | sort) \
  <(cd ../XiangShan_V2_mem_ut_uvm_v2/AI_DOC && find . -type f | sort) \
  > /tmp/mem_ut_ai_doc_missing_in_v2.txt

comm -13 \
  <(cd AI_DOC && find . -type f | sort) \
  <(cd ../XiangShan_V2_mem_ut_uvm_v2/AI_DOC && find . -type f | sort) \
  > /tmp/mem_ut_ai_doc_v2_only.txt

for f in $(comm -12 \
  <(cd AI_DOC && find . -type f | sort) \
  <(cd ../XiangShan_V2_mem_ut_uvm_v2/AI_DOC && find . -type f | sort)); do
  cmp -s "AI_DOC/$f" "../XiangShan_V2_mem_ut_uvm_v2/AI_DOC/$f" || echo "${f#./}"
done > /tmp/mem_ut_ai_doc_same_path_diff.txt
```

同样生成 rule 差异：

```bash
comm -23 \
  <(cd mem_ut/ver/ut/memblock/rule && find . -type f -name '*.md' | sort) \
  <(cd ../XiangShan_V2_mem_ut_uvm_v2/mem_ut/ver/ut/memblock/rule && find . -type f -name '*.md' | sort) \
  > /tmp/mem_ut_rule_missing_in_v2.txt

comm -13 \
  <(cd mem_ut/ver/ut/memblock/rule && find . -type f -name '*.md' | sort) \
  <(cd ../XiangShan_V2_mem_ut_uvm_v2/mem_ut/ver/ut/memblock/rule && find . -type f -name '*.md' | sort) \
  > /tmp/mem_ut_rule_v2_only.txt

for f in $(comm -12 \
  <(cd mem_ut/ver/ut/memblock/rule && find . -type f -name '*.md' | sort) \
  <(cd ../XiangShan_V2_mem_ut_uvm_v2/mem_ut/ver/ut/memblock/rule && find . -type f -name '*.md' | sort)); do
  rel="${f#./}"
  cmp -s "mem_ut/ver/ut/memblock/rule/$rel" \
    "../XiangShan_V2_mem_ut_uvm_v2/mem_ut/ver/ut/memblock/rule/$rel" || echo "$rel"
done > /tmp/mem_ut_rule_same_path_diff.txt
```

### 3. 同步 AI_DOC 缺失文件

从源工作树向 V2 工作树同步缺失文档，原则是只补缺失，不删除 V2 专属文件：

```bash
rsync -a --ignore-existing \
  /nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/AI_DOC/ \
  /nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2_mem_ut_uvm_v2/AI_DOC/
```

如果后续需要让同路径文档内容一致，不使用全量覆盖命令，应按“已知差异与采用策略”
逐个文件合并。

### 4. 合并项目管理规则

在 V2 工作树中处理：

1. 新增或同步 `AI_DOC/project_management/ai_doc_language_rule.md`。
2. 更新 `AI_DOC/project_management/ai_doc_file_management_rule.md`，加入语言规则入口。
3. 保持 `mem_ut_test_framework_plan_review_rule.md`、
   `mem_ut_test_framework_plan_execution_rule.md`、
   `mem_ut_code_review_document_rule.md` 等测试框架规则以源工作树为主。
4. 检查同步后的说明性文档正文为中文；若保留历史英文正文或英文网页版本，必须在最终
   review 中列清单并说明原因。

### 5. 合并 memblock rule 差异

在 V2 工作树中逐个处理差异文件：

```text
memblock_code_comment_rule.md
memblock_l2tlb_agent_rule.md
memblock_latest_dut_adapt_rule.md
memblock_parameter_management_rule.md
memblock_sequence_add_rule.md
memblock_update_code_rule.md
```

处理要求：

1. 普通行为规则、流程规则、参数分类规则以源工作树为主。
2. V2/V3 profile 读取要求保留。
3. `kunminghu-v2`、`mem_ut_uvm_v2`、`MEMBLOCK_XS_HOME`、worktree 路径隔离相关内容保留。
4. 旧 `AI_DOC/design_plan` 路径修正保留，不回退到旧路径。
5. 每个文件合并后用 `diff -u` 与源工作树比较，确认剩余差异都属于本 plan 表格中的允许差异。

### 6. 合并 AGENTS.md

V2 工作树的 `AGENTS.md` 必须同时具备：

1. 源工作树已有的全部文档入口。
2. `AI_DOC/project_management/ai_doc_language_rule.md` 入口和触发条件。
3. V2/V3 version profile 路由。
4. `MEMBLOCK_XS_HOME / MEMBLOCK_PROJECT` 路径隔离规则。
5. `kunminghu-v2` 和 `kunminghu-v3` 的 profile 化更新规则。

禁止把 V2 `AGENTS.md` 简单覆盖为源工作树版本；否则会丢失 V2 profile 路由。
也禁止把源工作树新增语言规则入口遗漏在 V2。

### 7. 目录拆分整理

只允许对 RTL 分析做版本拆分：

```text
AI_DOC/analysis/rtl/v2
AI_DOC/analysis/rtl/v3
```

如果执行者决定将源工作树的 V3 RTL 分析迁入 `AI_DOC/analysis/rtl/v3`，必须：

1. 使用 `git mv` 移动已跟踪文件。
2. 更新 `AI_DOC`、`AGENTS.md` 和相关 plan/review 中引用的旧路径。
3. 在最终 review 文档中列出每个移动文件。

除 RTL 分析外，其他目录先保持当前通用结构，不新建 `v2`/`v3` 分裂目录。

### 8. 验证

在 V2 工作树执行：

```bash
cd /nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2_mem_ut_uvm_v2

comm -23 \
  <(cd ../XiangShan_V2/AI_DOC && find . -type f | sort) \
  <(cd AI_DOC && find . -type f | sort)

comm -13 \
  <(cd ../XiangShan_V2/AI_DOC && find . -type f | sort) \
  <(cd AI_DOC && find . -type f | sort)

rg -n "AI_DOC/design_plan|AI_DOC/project_management/.*todo|AI_DOC/project_management/.*plan|AI_DOC/project_management/.*review" AI_DOC AGENTS.md mem_ut/ver/ut/memblock/rule

rg -n "kunminghu-v3|kunminghu-v2|mem_ut_uvm|mem_ut_uvm_v2|MEMBLOCK_PROJECT|MEMBLOCK_XS_HOME" AGENTS.md AI_DOC mem_ut/ver/ut/memblock/rule

git diff --check -- AI_DOC AGENTS.md mem_ut/ver/ut/memblock/rule
```

预期结果：

1. 第一条 `comm -23` 必须为空；如果本次执行了 RTL `v3` 路径迁移，必须另以迁移映射表
   证明源文件内容已在新路径存在，不能留下未解释缺失项。
2. 第二条 `comm -13` 只包含 V2 专属保护文件，或 review 中明确说明的 V2 新增文档。
3. 旧 `AI_DOC/design_plan` 路径无未解释残留。
4. 版本相关命中均能解释为 profile 路由或版本专属文档。
5. `git diff --check` 通过。

### 9. 提交建议

建议分两笔本地 commit，便于 review：

```text
commit 1:
docs(ai_doc): sync historical AI_DOC into mem_ut_uvm_v2

内容：
  AI_DOC 缺失文档补齐
  V2 专属文档保护
  不包含规则合并的大改

commit 2:
docs(rules): align mem_ut_uvm_v2 docs rules with version profiles

内容：
  AGENTS.md
  AI_DOC/project_management/*.md
  mem_ut/ver/ut/memblock/rule/**/*.md
  最终 review 文档
```

如果实际 diff 很小，也可以合并为一笔 commit；但必须保证 commit message 和 review
文档清楚说明“全量同步”和“规则差异合并”两类变更。

## 最终 review 文档要求

执行完成后必须新增中文 review 文档，建议路径：

```text
AI_DOC/plan/test_framework/review_doc/undo/ai_doc_mem_ut_uvm_v2_sync_review_20260706.md
```

review 文档必须包含：

1. review 范围。
2. 同步前后 `AI_DOC` 文件数量。
3. V2 专属保护文件清单。
4. 源工作树缺失到 V2 的文件清单摘要。
5. 同路径差异文件清单。
6. 每个规则差异采用源工作树还是 V2 差异的原因。
7. 是否存在旧路径残留。
8. 是否存在英文说明性正文；如有历史英文正文或英文网页版本，说明保留原因和后续处理建议。
9. 执行过的验证命令和结果。
10. 最终结论：是否满足本文 AC-1 到 AC-7。

## 风险与处理

| 风险 | 影响 | 处理 |
| --- | --- | --- |
| 用源工作树全量覆盖 V2 | 丢失 V2 profile 和迁移记录 | 只先补缺失，再逐个合并同路径差异 |
| 把所有文档按 V2/V3 拆目录 | 后续查找和链接维护复杂 | 只允许 RTL 分析版本拆分，其他保持通用 |
| 规则文件未标注差异 | review 无法判断是否误改 | 差异表和最终 review 必须逐项说明 |
| 保留旧 `AI_DOC/design_plan` 路径 | 链接断裂、规则失效 | 旧路径必须修正到 `AI_DOC/analysis` 或 `AI_DOC/plan` |
| V2 未同步中文语言规则 | 后续 review/plan 继续出现英文正文 | 同步 `ai_doc_language_rule.md` 并更新 `AGENTS.md` |

## 后续衔接

本文完成后，才允许继续执行以下独立任务：

1. V2 RTL 生成能力修复与验证。
2. V2 RTL interface delta 复核。
3. 基于 V2 RTL 的 mem_ut UVM agent/interface/transaction/driver/monitor 适配 plan。
4. V2 远端编译仿真 smoke 验证。

这些任务不得混入本文档同步 commit。
