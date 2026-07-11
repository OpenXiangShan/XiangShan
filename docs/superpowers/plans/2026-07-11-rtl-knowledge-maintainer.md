# RTL Knowledge Maintainer Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** 在仓库内建立按 V2/V3、agent/flow 分类，并能在每次 RTL/Scala 分析后校正和增量维护知识的强制工作流。

**Architecture:** 使用 `AI_DOC/project_management` 保存项目级约束，使用 `AI_DOC/skills/rtl-knowledge-maintainer` 保存可执行工作流和模板，通过 `AGENTS.md` 自动路由。知识库采用版本隔离的 interface-agent 与 rtl-flow 长期文档，首次实现后立即回填本轮 flushPipe/SFENCE/CBO/trigger 分析。

**Tech Stack:** Markdown、YAML、Codex skill、Git、`rg`、skill-creator validation scripts。

## Global Constraints

- 项目文档正文默认使用中文。
- 用户指定版本优先；版本信息冲突时停止写入并确认。
- V2/V3 知识严格隔离，文档记录 branch、commit、权威源码和核验日期。
- 每个 agent 或 flow 使用一份长期文档，不按问题或日期重复创建报告。
- 修改旧知识时保留用户无关改动，以当前版本权威源码为准并记录修订原因。
- 不修改与本任务无关的现有工作树内容。

---

### Task 1: 建立项目管理规则和 AGENTS 路由

**Files:**
- Create: `AI_DOC/project_management/rtl_knowledge_base_management_rule.md`
- Modify: `AGENTS.md`

**Interfaces:**
- Consumes: `docs/superpowers/specs/2026-07-11-rtl-knowledge-maintainer-design.md`
- Produces: 所有 RTL/Scala 分析任务必须执行的版本路由、知识检索、分类、合并、冲突处理和完成标准。

- [ ] **Step 1: 编写项目管理规则**

写入设计规格第 3 至第 10 节的强制规则，并明确 skill 位置为 `AI_DOC/skills/rtl-knowledge-maintainer/SKILL.md`。

- [ ] **Step 2: 更新 AGENTS.md**

增加规则文档和 skill 的链接、触发条件以及“回答用户前必须更新知识库”的要求。

- [ ] **Step 3: 检查文档格式**

Run: `git diff --check -- AI_DOC/project_management/rtl_knowledge_base_management_rule.md AGENTS.md`

Expected: 无输出，退出码为 0。

- [ ] **Step 4: 提交任务**

```bash
git add AI_DOC/project_management/rtl_knowledge_base_management_rule.md AGENTS.md
git commit -m "docs: require RTL knowledge base maintenance"
```

### Task 2: 创建仓库内 skill 和模板

**Files:**
- Create: `AI_DOC/skills/rtl-knowledge-maintainer/SKILL.md`
- Create: `AI_DOC/skills/rtl-knowledge-maintainer/agents/openai.yaml`
- Create: `AI_DOC/skills/rtl-knowledge-maintainer/references/document_templates.md`

**Interfaces:**
- Consumes: Task 1 的项目管理规则。
- Produces: 可触发的 `rtl-knowledge-maintainer` skill，以及 agent/flow 文档标准模板。

- [ ] **Step 1: 使用 skill-creator 初始化目录**

Run:

```bash
python3 /nfs/home/lixiangrui/.codex/skills/.system/skill-creator/scripts/init_skill.py rtl-knowledge-maintainer \
  --path AI_DOC/skills \
  --resources references \
  --interface 'display_name=RTL Knowledge Maintainer' \
  --interface 'short_description=按 RTL 版本维护 agent 与 flow 知识库' \
  --interface 'default_prompt=分析当前 RTL/Scala 问题，并按版本、agent 或 flow 更新项目知识库。'
```

Expected: 创建 skill 目录、`SKILL.md`、`agents/openai.yaml` 和 `references/`。

- [ ] **Step 2: 实现 SKILL.md**

写入命令式流程：版本确认、读取项目规则、检索 index/全文、源码分析、旧知识比较、分类合并、修订记录、索引维护、验证和用户交付。

- [ ] **Step 3: 实现 document_templates.md**

提供完整的 agent 接口模板和内部 flow 模板，均包含版本元数据、源码证据、交叉引用、版本差异、修订记录及待确认项。

- [ ] **Step 4: 验证 skill**

Run:

```bash
python3 /nfs/home/lixiangrui/.codex/skills/.system/skill-creator/scripts/quick_validate.py \
  AI_DOC/skills/rtl-knowledge-maintainer
```

Expected: 输出 skill validation passed，退出码为 0。

- [ ] **Step 5: 提交任务**

```bash
git add AI_DOC/skills/rtl-knowledge-maintainer
git commit -m "feat: add RTL knowledge maintainer skill"
```

### Task 3: 创建 V2/V3 知识库骨架和索引

**Files:**
- Create: `AI_DOC/analysis/interface/v2/index.md`
- Create: `AI_DOC/analysis/interface/v3/index.md`
- Create: `AI_DOC/analysis/rtl/v2/index.md`
- Create: `AI_DOC/analysis/rtl/v3/index.md`
- Create directories: corresponding `agents/` and `flows/`

**Interfaces:**
- Consumes: Task 2 的分类和模板规则。
- Produces: 版本隔离的知识检索入口。

- [ ] **Step 1: 创建四个索引**

每个索引记录版本范围、权威 profile、文档表格、关键词、覆盖模块和交叉引用规则；空分类明确写“当前暂无条目”。

- [ ] **Step 2: 验证版本隔离和链接**

Run:

```bash
rg -n "V2|V3|agents/|flows/" \
  AI_DOC/analysis/interface/{v2,v3}/index.md \
  AI_DOC/analysis/rtl/{v2,v3}/index.md
```

Expected: 四个索引均明确各自版本和目标子目录，无跨版本正文复用。

- [ ] **Step 3: 提交任务**

```bash
git add AI_DOC/analysis/interface/v2/index.md AI_DOC/analysis/interface/v3/index.md \
  AI_DOC/analysis/rtl/v2/index.md AI_DOC/analysis/rtl/v3/index.md
git commit -m "docs: initialize versioned RTL knowledge indexes"
```

### Task 4: 按 V2 flow 回填本轮 RTL 分析

**Files:**
- Modify if appropriate: `AI_DOC/mem_ut_flow_doc/sfence_flow.md`
- Create or Modify: `AI_DOC/analysis/rtl/v2/flows/memory_flush_pipe_flow.md`
- Create or Modify: `AI_DOC/analysis/rtl/v2/flows/memory_trigger_flow.md`
- Modify: `AI_DOC/analysis/rtl/v2/index.md`

**Interfaces:**
- Consumes: 当前 `mem_ut_uvm_v2` 分支、V2 profile、Scala 权威源码以及 Task 2 模板。
- Produces: `uop.flushPipe`、SFENCE、CBO/CMO、memory trigger 的长期 V2 知识。

- [ ] **Step 1: 核验 V2 元数据和现有知识**

Run:

```bash
git branch --show-current
git rev-parse HEAD
rg -n "flushPipe|sfence|CBO|CMO|TriggerAction|memory trigger" \
  AI_DOC/analysis AI_DOC/mem_ut_flow_doc
```

Expected: 分支为 `mem_ut_uvm_v2`；获得当前 commit；识别可整合的旧知识，避免重复文档。

- [ ] **Step 2: 创建或更新 memory flush flow**

记录 Decode/Issue/MemBlock/Writeback/ExceptionGen/ROB 的传播链，区分 SFENCE 的 Decode 静态置位、普通 Load/Store 清零、StoreQueue CBO 动态置位以及 LoadUnit 局部 `s3_flushPipe`。

- [ ] **Step 3: 创建或更新 memory trigger flow**

记录 `TriggerAction` 编码、CSR 配置、Load/Store S1 地址匹配、Breakpoint/DebugMode 动作、写回和 ROB 精确异常处理。

- [ ] **Step 4: 同步现有 sfence flow 和 V2 索引**

只在现有 `sfence_flow.md` 中补充必要交叉引用和版本边界，不覆盖用户已有无关修改；把两个 V2 flow 加入索引。

- [ ] **Step 5: 验证知识内容**

Run:

```bash
rg -n "版本|分支|commit|权威源码|flushPipe|TriggerAction|修订记录" \
  AI_DOC/analysis/rtl/v2/flows/*.md
git diff --check -- AI_DOC/analysis/rtl/v2 AI_DOC/mem_ut_flow_doc/sfence_flow.md
```

Expected: 两个 flow 均包含元数据、源码证据和修订记录；格式检查无输出。

- [ ] **Step 6: 提交任务**

```bash
git add AI_DOC/analysis/rtl/v2 AI_DOC/mem_ut_flow_doc/sfence_flow.md
git commit -m "docs: capture V2 memory flush and trigger flows"
```

### Task 5: 最终验证和交付

**Files:**
- Verify: `AGENTS.md`
- Verify: `AI_DOC/project_management/rtl_knowledge_base_management_rule.md`
- Verify: `AI_DOC/skills/rtl-knowledge-maintainer/`
- Verify: `AI_DOC/analysis/interface/{v2,v3}/`
- Verify: `AI_DOC/analysis/rtl/{v2,v3}/`

**Interfaces:**
- Consumes: Tasks 1-4 的全部产物。
- Produces: 可发现、可执行、版本隔离并已完成首次回填的知识维护体系。

- [ ] **Step 1: 运行 skill validation**

Run: `python3 /nfs/home/lixiangrui/.codex/skills/.system/skill-creator/scripts/quick_validate.py AI_DOC/skills/rtl-knowledge-maintainer`

Expected: validation passed。

- [ ] **Step 2: 检查路径引用和占位符**

Run:

```bash
rg -n "PLACEHOLDER|\[Brief Description\]|AI_DOC/design_plan" \
  AI_DOC/skills/rtl-knowledge-maintainer \
  AI_DOC/project_management/rtl_knowledge_base_management_rule.md \
  AI_DOC/analysis/interface/{v2,v3} AI_DOC/analysis/rtl/{v2,v3}
```

Expected: 无模板占位符或旧路径残留。

- [ ] **Step 3: 最终格式和状态检查**

Run:

```bash
git diff --check -- AI_DOC AGENTS.md docs/superpowers
git status --short
```

Expected: 格式检查通过；状态只包含用户原有改动和本计划明确产物。

- [ ] **Step 4: 向用户交付**

说明 skill、规则、索引和首次回填文档位置；报告验证结果，并明确未修改用户无关源码。
