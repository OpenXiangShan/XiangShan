# RTL 分析知识库维护规则

本文约束 XiangShan `mem_ut` 项目中所有 RTL/Scala 接口与功能行为分析。每次分析必须基于明确版本和权威源码，并在回答用户前把结论合并到对应版本的长期知识库。

执行本规则时还必须遵循：

- `AI_DOC/project_management/ai_doc_file_management_rule.md`
- `AI_DOC/project_management/ai_doc_language_rule.md`
- `AI_DOC/skills/rtl-knowledge-maintainer/SKILL.md`
- `mem_ut/ver/ut/memblock/rule/version/<v2|v3>/branch_policy.md`
- `mem_ut/ver/ut/memblock/rule/version/<v2|v3>/memblock_rtl_profile.md`

## 1. 触发条件

用户要求分析、解释、追踪或比较以下任一内容时必须执行本规则：

- RTL、Scala/Chisel 模块、信号、字段、端口、Bundle 或参数。
- MemBlock/LSQ/DCache/TLB/ROB 等模块的内部行为。
- pipeline、状态机、queue、仲裁、exception、redirect、replay、flush、writeback 等 flow。
- V2/V3 接口或行为差异。

即使用户只要求口头解释，也必须在本轮完成知识库检索、校正和更新；不得只在聊天中保留结论。

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

单次 RTL/Scala 分析必须同时满足：

1. 已确认版本、分支、commit 和权威源码。
2. 已检索并阅读对应版本旧知识。
3. 已把结论更新到正确的 agent/flow 长期文档。
4. 已更新必要的索引、交叉引用和修订记录。
5. 已完成格式与一致性检查。
6. 已在用户答复中给出结论和知识库变更位置。

任一项缺失时，不得宣称分析完成。
