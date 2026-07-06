# AI_DOC 文件分类管理规则

本文约束 XiangShan 仓库内 `AI_DOC` 文档库的目录分类、文件命名、归档流转和链接维护规则。后续新增、移动、整理或 review `AI_DOC` 下设计文档、plan、review、flow、网页和分析文档时，必须先阅读本文。

新增或修改文档时，还必须遵循：

- `AI_DOC/project_management/ai_doc_language_rule.md`

默认所有面向阅读者的说明性文档使用中文，英文仅保留在代码标识符、路径、命令、日志、报错和固定术语中。

## 1. 目录职责

`AI_DOC` 按文档用途分类，不按历史任务或临时会话分类。

```text
AI_DOC/
  plan/
    测试框架/
      plan/
        undo/
        do/
      review_doc/
        undo/
        do/
    rm_plan/
      undo/
      do/
    rm_review_doc/
      undo/
      do/
  mem_ut_flow_doc/
  web/
  analysis/
    rtl/
    interface/
    source_sv/
    testcase_flow/
    framework_design/
  project_management/
  skills/
```

目录含义：

- `AI_DOC/plan/test_framework/plan`：mem_ut 测试框架设计、重构、实现方案和 TODO plan。
- `AI_DOC/plan/test_framework/review_doc`：mem_ut 测试框架 review、annotated review、checklist 和复查记录。
- `AI_DOC/plan/rm_plan`：RM、scoreboard、参考模型、TLB/RM check 相关 plan。
- `AI_DOC/plan/rm_review_doc`：RM、scoreboard、参考模型相关 review/checklist。
- `AI_DOC/mem_ut_flow_doc`：只放 mem_ut 测试框架 flow 文档，例如 issue、writeback、redirect、replay、sfence、LSQ admission flow。
- `AI_DOC/web`：只放生成网页、网页资源、可静态打开的 HTML/JS/CSS/模板和网页稿。
- `AI_DOC/analysis`：放分析类文档，按 `rtl`、`interface`、`source_sv`、`testcase_flow`、`framework_design` 等类别继续拆分。
- `AI_DOC/project_management`：只放项目规则、文档规则、管理规则和长期执行规范，不放具体 feature plan、TODO、review 或源码分析。
- `AI_DOC/skills`：只放可复用 skill。

## 2. Plan 归档规则

Plan 文件必须进入对应 `plan/*/undo` 或 `plan/*/do`。

- 新建 plan 默认放入 `undo`。
- 完成 coding、同步文档并完成必要验证后，必须把对应 plan 从 `undo` 移到 `do`。
- 若 plan 只完成了部分 coding，仍放在 `undo`，并在文档内写明当前落地状态。
- TODO 类 plan 也放入对应 `undo`，不要放在 `project_management`。
- 测试框架 plan 和 RM plan 不能混放；涉及 RM/scoreboard/check 的放 `rm_plan`。

Plan 文件名必须带日期。推荐格式：

```text
<主题>_YYYYMMDD.md
```

如果原文件名已有明确日期，可以保留原日期；如果没有日期，移动或新建时必须补上当前处理日期。

## 3. Review 文档归档规则

Review 文档必须进入对应 `review_doc/undo` 或 `review_doc/do`。

- 新建 review 默认放入 `undo`。
- Review 文档不因 coding 完成自动移动。
- 只有用户明确要求归档、关闭或移动 review 时，才把 review 从 `undo` 移到 `do`。
- checklist、annotated review、implementation review、code review 都属于 review_doc。
- RM/scoreboard/TLB check 相关 review 放 `rm_review_doc`。

## 4. Flow 文档规则

`AI_DOC/mem_ut_flow_doc` 只放 flow 文档，不放 plan、TODO、review 或源码分析。

Flow 文档必须描述测试框架真实函数调用链、队列流转、状态表变化和端到端行为。具体写作要求遵循：

- `AI_DOC/project_management/mem_ut_flow_document_rule.md`

后续生成或整理测试框架 flow 文档时，默认落点为：

```text
AI_DOC/mem_ut_flow_doc/<flow_name>.md
```

## 5. Web 文档规则

生成网页、网页资源和 HTML 入口统一放在：

```text
AI_DOC/web
```

网页目录移动后必须检查相对路径，包括：

- `href`
- `src`
- CSS/JS 资源路径
- markdown 到 HTML 的跳转路径

网页同步要求遵循：

- `AI_DOC/project_management/mem_ut_web_doc_sync_rule.md`

## 6. 分析文档规则

分析文档进入 `AI_DOC/analysis`，按分析对象分类：

- RTL/Scala 行为分析：`AI_DOC/analysis/rtl`
- interface、transaction 字段分析：`AI_DOC/analysis/interface`
- SystemVerilog 源码函数/字段分析：`AI_DOC/analysis/source_sv`
- testcase 或场景流程分析：`AI_DOC/analysis/testcase_flow`
- 测试框架架构或设计原理分析：`AI_DOC/analysis/framework_design`

分析文档不放入 `plan`，除非它明确包含后续 coding 步骤、验收标准和待执行任务。

## 7. Project Management 目录规则

`AI_DOC/project_management` 只放规则类文件。允许内容包括：

- 文档分类管理规则。
- flow/review/web 文档编写规则。
- 参数管理规则。
- 测试框架逻辑构建规则。
- 长期项目执行规范。

禁止放入：

- 单个 feature 的 plan。
- TODO 文档。
- review/checklist。
- 源码分析。
- 临时讨论记录。

如果发现上述文件，应迁移到 `plan`、`mem_ut_flow_doc`、`analysis` 或 `web` 对应目录。

## 8. 链接维护规则

移动或重命名文档后，必须检查并更新旧路径引用。

最低检查命令：

```bash
rg -n "AI_DOC/design_plan|AI_DOC/project_management/.*todo|AI_DOC/project_management/.*plan|AI_DOC/project_management/.*review" AI_DOC AGENTS.md
```

重点检查：

- `AGENTS.md` 中的文档入口。
- `AI_DOC/project_management/*.md` 规则中的路径。
- plan/review 文档中引用的源码分析文档路径。
- 网页模板和 HTML 中的相对链接。

如果移动的是规则文件，必须同步更新 `AGENTS.md` 中的规则入口和触发条件。

如果 plan 中引用的是未来需要新增的文档，必须在正文中明确写成“待新增文档”或“后续新增”，避免后续链接检查时被误认为迁移断链。

## 8.1 链接检查建议

完成大规模文档移动后，建议额外执行本地链接检查：

```bash
python3 <本地链接检查脚本>
```

检查范围至少覆盖：

- Markdown 链接，形如方括号标题加圆括号路径。
- 反引号中的 AI_DOC/... 路径。
- `AGENTS.md` 中的规则入口。

允许保留的缺失项只有明确标注为“待新增文档”或“后续新增”的未来产物。

## 9. AGENTS.md 同步规则

后续新增或调整 `AI_DOC/project_management` 下的项目规则文件时，必须同步在仓库根目录 `AGENTS.md` 中添加或更新入口，至少包含：

- 规则文件链接。
- 触发条件。
- 必须优先阅读的要求。

## 10. 执行流程

整理文档库时按以下流程执行：

1. 使用 `find AI_DOC` 和 `rg` 盘点当前文件和旧路径引用。
2. 建立目标目录。
3. 使用 `git mv` 迁移已跟踪文件，未跟踪文件使用普通 `mv`。
4. 按本文规则补齐 plan 文件日期。
5. 更新规则文件、`AGENTS.md` 和旧路径引用。
6. 使用 `rg` 复查旧路径残留。
7. 执行本地 Markdown 链接检查，确认迁移导致的断链已修复。
8. 使用 `git diff --check -- AI_DOC AGENTS.md` 检查格式问题。
9. 输出迁移摘要和剩余风险。

## 11. 后续可优化项

当前目录已经能按用途管理文档。随着文档数量继续增加，建议后续补充：

- `AI_DOC/README.md`（后续新增）：作为文档库总索引，列出 plan、review、flow、analysis、web、project_management 的入口和当前状态。
- `AI_DOC/plan/README.md`（后续新增）：列出各 plan 的状态、日期、所属模块和是否完成 coding。
- `AI_DOC/analysis/README.md`（后续新增）：列出源码分析、接口分析、RTL 分析的索引，避免只靠目录名查找。
- 链接检查脚本：固化到 `AI_DOC/project_management` 或 `scripts`，避免每次手写临时脚本。
- 文档状态字段：新 plan/review 可在文件头部统一写 `状态`、`创建日期`、`归档条件`、`关联源码`，便于后续自动生成索引。
