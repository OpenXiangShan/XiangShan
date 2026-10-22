# RTL 分析知识库维护规则

本文约束已触发的 RTL 知识库维护。只有满足对应版本的显式触发条件时，才必须基于明确版本和权威源码，并在回答用户前把结论合并到对应版本的长期知识库。V2 默认不触发。

执行本规则时还必须遵循：

- `AI_DOC/project_management/ai_doc_file_management_rule.md`
- `AI_DOC/project_management/ai_doc_language_rule.md`
- `AI_DOC/skills/rtl-knowledge-maintainer/SKILL.md`
- `mem_ut/ver/ut/memblock/rule/version/<v2|v3>/branch_policy.md`
- `mem_ut/ver/ut/memblock/rule/version/<v2|v3>/memblock_rtl_profile.md`

## 1. 触发条件

### 1.1 共同前提

只有用户提示词明确要求结合 Scala/Chisel 源码分析、解释、追踪或比较 RTL 功能行为时，才可能执行本规则。典型表达包括：

- “结合 Scala 源码分析这个 RTL 信号何时置高”。
- “从 Chisel 实现追踪 MemBlock/LSQ/DCache/TLB/ROB 的内部 RTL flow”。
- “结合 V2/V3 Scala 源码比较 pipeline、状态机、queue、exception、redirect、replay、flush 或 writeback 行为”。

以下任务默认不触发本规则，也不得因此更新 RTL 知识库：

- 只分析测试框架、UVM、SystemVerilog 验证代码、sequence、driver、monitor、RM、scoreboard、plan 或项目管理文档。
- 只解释计划条目、测试策略、验证环境 capability 或软件模型行为，即使其中引用了 RTL 信号名。
- 只要求查看接口表、日志、波形或仿真结果，但没有要求结合 Scala/Chisel 源码分析 RTL 行为。
- Codex 为辅助理解自行查看少量 Scala/RTL 源码，但用户没有提出上述显式要求。

非触发任务仍按其对应的测试框架、flow、review 或接口文档规则处理，但不得写入 `AI_DOC/analysis/rtl/<version>/flows`。同一提示词同时包含测试框架分析和 RTL 源码分析时，只有对应版本的知识库触发条件已经满足，才把其中 RTL 行为结论更新到 RTL 知识库。

### 1.2 V2 额外条件

目标版本为 V2 时，默认不触发本规则，也不得更新 V2 RTL 知识库。

V2 必须同时满足 1.1，并且当前提示词出现明确要求更新文档或知识库的同类词语，才允许触发。同类词语包括：

- 更新文档、写入文档、同步文档、沉淀文档、维护文档。
- 更新知识库、写入知识库、同步知识库、沉淀知识库、维护知识库。
- 明确要求把本次分析结论写入或更新长期知识文档。
- 语义相同的英文要求，例如 `update documentation` 或 `update the knowledge base`。

以下情况不触发 V2 知识库流程：

- 只有分析、解释、追踪、比较或查看源码，没有上述更新文档或知识库意图。
- “继续”“看看”“分析一下”等没有更新文档意图的后续指令。
- “更新代码”“更新最新代码”“更新到 kunminghu-v2”等代码同步表述。单独的“更新”不能代替“更新文档”或“更新知识库”。
- 当前分支是 `mem_ut_uvm_v2`，但提示词没有更新文档或知识库类词语。

版本尚未判定时，用户指定 V2，或未指定版本且当前分支为 `mem_ut_uvm_v2`，先按本条判断。不满足时停止知识库流程，不得写入 `AI_DOC/analysis/**/v2`。普通源码分析可以继续。

同时比较 V2 和 V3 时，V2 侧仍受本条约束。没有更新文档或知识库类词语时，不得写入 V2 文档。

### 1.3 V3

目标版本为 V3 且满足 1.1 时，按原流程触发。V3 不额外要求更新文档或知识库类词语。

## 2. 版本与权威来源

版本判定优先级：

1. 用户明确指定 V2/V3 时，以用户指定为准。
2. 用户未指定时，根据当前分支和对应 `branch_policy.md` 判定。
3. 核对对应 `memblock_rtl_profile.md`、实际 RTL/Scala 路径和当前 commit。
4. 用户指定、分支、profile 或源码版本冲突时，停止知识写入并向用户确认。

每份知识文档必须记录：版本、分支、核验 commit、权威源码、最后核验日期。V2/V3 内容严格分开，不得把一个版本的结论当作另一版本的事实。

## 3. 知识库分类

### 3.1 顶层接口按 agent 分类

MemBlock/DUT 顶层端口、Bundle、方向、位宽、握手和 UVM 覆盖关系写入：

```text
AI_DOC/analysis/interface/<v2|v3>/agents/<agent_name>.md
```

每个 agent 维护一份长期文档。若现有 agent 无法覆盖，先按信号功能、驱动方和消费者分析归属，再提出扩展或新增 agent；禁止只按端口名前缀机械分类。

### 3.2 内部功能按 flow 分类

内部 pipeline、状态机、queue、仲裁、exception、redirect、replay、flush、writeback 和跨模块调用链写入：

```text
AI_DOC/analysis/rtl/<v2|v3>/flows/<flow_name>.md
```

新分析属于已有 flow 时，合并到对应阶段；属于其前置或后续阶段时，扩展 flow 边界。只有具备独立入口、状态演进和终止条件且无法自然归入现有 flow 时才新建文档。

一个问题跨多个 agent/flow 时分别更新。主文档保存完整逻辑，相关文档保存摘要和交叉链接。

## 4. 强制分析流程

1. 确定版本、分支、commit 和权威源码。
2. 阅读对应版本的 `index.md`，用 `rg` 检索关键词、字段名、模块名和旧结论。
3. 阅读所有相关 agent/flow 文档，不能只检索标题。
4. 从真实源码追踪定义、关键赋值、上游生产者、下游消费者、寄存器边界、置位条件、清除条件和优先级。
5. 区分同名局部变量、配置能力开关和运行时字段。
6. 将新结论与旧知识逐项比较。
7. 更新已有长期文档或在确认无法归类时新增文档。
8. 更新对应 `index.md`、交叉引用和核验元数据。
9. 执行格式、链接和版本隔离检查。
10. 回答用户并列出本轮更新的知识库文件。

## 5. 旧知识处理

- 结论一致：直接补充源码证据、时序、边界和例外，不重复追加同义段落。
- 部分一致：保留仍成立内容，修改失效或表述过度的部分。
- 结论不一致：以当前版本权威源码为准修改正文，并在“知识修订记录”中写明旧结论、新结论、原因、commit 和影响范围。
- 无法确定：不得覆盖旧结论；在“待确认项”记录冲突源码、缺失证据和验证方法。
- V2/V3 不一致：分别记录为版本差异，不作为同版本知识冲突处理。

修改后正文只能保留一个当前有效结论，不得在文末追加与正文矛盾的新结论。若目标文档已有用户未提交修改，只做最小范围合并；无法安全合并时停止并报告。

## 6. 文档结构

Agent 接口文档至少包含：

- 版本元数据。
- agent 职责和边界。
- RTL 端口/字段表。
- 握手与时序。
- interface/transaction/connect/monitor/driver 映射。
- 关联 flow、版本差异、源码证据、知识修订记录、待确认项。

内部 flow 文档至少包含：

- 版本元数据和 flow 范围。
- 入口、出口和完成条件。
- Mermaid 主流程图及按源码顺序的文字伪代码。
- pipeline stage、状态/队列变化、分支优先级和异常路径。
- 关联 agent/flow、版本差异、源码证据、知识修订记录、待确认项。

模板位于：

```text
AI_DOC/skills/rtl-knowledge-maintainer/references/document_templates.md
```

## 7. 索引规则

以下目录分别维护 `index.md`：

```text
AI_DOC/analysis/interface/v2
AI_DOC/analysis/interface/v3
AI_DOC/analysis/rtl/v2
AI_DOC/analysis/rtl/v3
```

索引至少包含文档链接、关键词、覆盖模块、入口信号/函数和关联文档。新增、重命名或扩大 flow/agent 范围时必须同步更新索引。

## 8. 验证要求

最低检查：

```bash
rg -n "<分析关键词>" AI_DOC/analysis/interface/<version> AI_DOC/analysis/rtl/<version>
git diff --check -- AI_DOC AGENTS.md
```

还需人工确认：

- 文档版本与源码版本一致。
- 源码路径、类名、字段名和条件真实存在。
- 没有把 compile-time capability 当成 runtime assertion。
- 没有跨 V2/V3 混写。
- 旧知识冲突已经修订或明确标记待确认。

## 9. 完成标准

单次已触发的 RTL 知识库维护必须同时满足：

1. 已确认版本、分支、commit 和权威源码。
2. 已检索并阅读对应版本旧知识。
3. 已把结论更新到正确的 agent/flow 长期文档。
4. 已更新必要的索引、交叉引用和修订记录。
5. 已完成格式与一致性检查。
6. 已在用户答复中给出结论和知识库变更位置。

任一项缺失时，不得宣称该次知识库维护完成。V2 未出现更新文档或知识库类词语时视为未触发；可以直接完成源码分析，不得因此写入 V2 知识库，也不得因未更新知识库而拒绝完成分析。
