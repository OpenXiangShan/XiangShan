# RTL 分析知识库维护 Skill 设计

## 1. 目标

为 XiangShan `mem_ut` 项目建立一套强制执行的 RTL/Scala 分析知识沉淀机制。每次回答 RTL 接口或内部功能问题时，必须先确定 V2/V3 版本，复用并校正已有知识，最后把可靠结论合并到对应版本的长期知识文档中，避免分析结果只保留在对话或按日期产生重复报告。

## 2. 交付物

采用“项目管理规则 + 仓库内 skill + 文档模板 + AGENTS.md 入口”的组合方案：

- `AI_DOC/project_management/rtl_knowledge_base_management_rule.md`：定义强制规则、版本路由、分类、冲突处理和完成标准。
- `AI_DOC/skills/rtl-knowledge-maintainer/SKILL.md`：把规则转化为每次 RTL/Scala 分析必须执行的操作流程。
- `AI_DOC/skills/rtl-knowledge-maintainer/agents/openai.yaml`：提供 skill 的发现和默认提示信息。
- `AI_DOC/skills/rtl-knowledge-maintainer/references/document_templates.md`：保存 interface-agent 和 internal-flow 两类知识模板。
- `AGENTS.md`：增加规则入口和触发条件，要求 RTL/Scala 分析优先使用该规则与 skill。

## 3. 知识库目录

```text
AI_DOC/analysis/
├── interface/
│   ├── v2/
│   │   ├── index.md
│   │   └── agents/<agent_name>.md
│   └── v3/
│       ├── index.md
│       └── agents/<agent_name>.md
└── rtl/
    ├── v2/
    │   ├── index.md
    │   └── flows/<flow_name>.md
    └── v3/
        ├── index.md
        └── flows/<flow_name>.md
```

目录职责：

- `interface/<version>/agents`：记录 DUT 顶层接口、Bundle、方向、位宽、握手语义和 agent 归属。
- `rtl/<version>/flows`：记录内部流水线、状态机、队列、仲裁、异常、回滚、写回、flush 及跨模块调用链。
- 每个 agent 或 flow 对应一份长期维护文档，不按问题或日期重复新建报告。
- `index.md` 记录文档入口、关键词、覆盖模块和交叉引用，作为分类检索入口。

## 4. 版本路由

版本判定按以下优先级执行：

1. 用户明确指定 V2/V3 时，以用户指定为准。
2. 用户未指定时，根据当前分支及对应 `branch_policy.md` 判定。
3. 核对对应版本 profile、RTL/Scala 权威来源和当前 commit。
4. 用户指定、分支、profile 或权威源码发生冲突时，停止知识库写入并向用户确认，不得猜测版本。

每份知识文档必须记录：

- RTL 版本。
- 当前分支。
- 核验 commit。
- 权威 RTL/Scala 来源。
- 最后核验日期。

V2 与 V3 的结论分别维护。版本间差异不视为旧知识错误，通过交叉链接记录差异，禁止把一个版本的结论直接复制为另一个版本的结论。

## 5. 分类判定

### 5.1 顶层接口知识

以下内容归入 `interface/<version>/agents/<agent_name>.md`：

- MemBlock/DUT 顶层端口。
- Bundle 字段、方向和位宽。
- valid/ready 等握手关系。
- UVM agent、interface、transaction、monitor、driver 和 connect 的映射。

如果现有 agent 无法覆盖该接口，先分析信号语义，再提出新 agent 或扩展 agent 的分类建议，不按端口前缀机械分类。

### 5.2 内部 flow 知识

以下内容归入 `rtl/<version>/flows/<flow_name>.md`：

- 模块内部流水线和 stage 传播。
- 状态机、队列、仲裁和优先级。
- exception、redirect、replay、flush、writeback 等功能行为。
- 跨模块调用链和关键状态副作用。

新分析属于已有 flow 时，必须整合进对应阶段。新行为是已有 flow 的前置或后续阶段时，扩展该 flow 的边界。只有具备独立入口、状态演进和终止条件，且无法自然归入现有 flow 时，才允许新增 flow 文档。

一个问题跨多个 agent 或 flow 时，分别更新相关文档；选择一个主文档保存完整结论，其他文档保存必要摘要和交叉链接。

## 6. 每次分析的强制流程

```text
收到 RTL/Scala 分析问题
  → 确认 V2/V3 和权威源码
  → 检索对应版本 index 和全文知识库
  → 阅读相关 agent/flow 旧知识
  → 从真实源码定义、赋值点、连接和消费者完成分析
  → 对比新结论与旧知识
      ├─ 一致：补充证据、条件、时序或边界
      ├─ 部分一致：保留有效内容并修订不准确部分
      ├─ 不一致：按当前权威源码修订并记录原因
      └─ 无对应知识：分类后新增长期文档
  → 更新 index、交叉引用和最后核验信息
  → 检查链接、格式及版本隔离
  → 回答用户，并列出本次更新的知识库文件
```

源码分析至少覆盖：

- 字段或模块定义。
- 所有关键赋值点。
- 上游生产者和下游消费者。
- combinational/register 时序边界。
- assertion 条件、优先级和清除条件。
- 与相近同名信号的区别。
- 版本特有行为和未确认边界。

## 7. 旧知识合并与冲突处理

- 一致：不重复堆叠结论，直接增强已有章节的证据和边界条件。
- 部分一致：保留仍成立的内容，只修订失效或表述过度的部分。
- 不一致：以当前版本权威源码为准修改旧结论，并在“知识修订记录”中记录旧结论、新结论、原因、commit 和影响范围。
- 无法确定：不得覆盖旧结论；标记“待确认”，列出冲突源码、缺失证据和下一步验证方法。
- 禁止仅在文末追加与正文矛盾的新段落；修改后正文必须只有一个当前有效结论。

修改已有知识前必须尊重用户未提交改动。若目标文档已有与本次任务无关的修改，应进行最小范围合并；无法安全合并时向用户说明冲突。

## 8. 文档模板要求

### 8.1 Agent 接口文档

至少包含：版本元数据、agent 职责、RTL 顶层端口表、握手和时序、UVM 组件映射、字段语义、关联 flow、版本差异、源码证据、知识修订记录和待确认项。

### 8.2 内部 Flow 文档

至少包含：版本元数据、flow 范围、入口/出口、Mermaid 主流程图、按源码顺序的文字伪代码、关键阶段、状态/队列变化、分支优先级、异常与 flush 行为、关联 agent/flow、版本差异、源码证据、知识修订记录和待确认项。

## 9. 异常处理

- 无法确定版本：停止写入并询问用户。
- 找不到权威源码：报告缺失路径，不以生成 RTL、旧文档或记忆替代源码结论。
- 旧知识与当前源码冲突：先确认版本和 commit，再决定修订或记录版本差异。
- 分类不明确：检索相邻 flow；仍不明确时在最接近的 flow 中记录待分类项，不立即创建含糊的新 flow。
- 文档存在未提交修改：只编辑本次相关段落；不能安全合并时停止并报告。

## 10. 验证与完成标准

实现阶段需要验证：

- skill 目录通过 `quick_validate.py`。
- `agents/openai.yaml` 与 `SKILL.md` 内容一致。
- V2/V3 interface 和 rtl 索引目录完整。
- 模板包含版本元数据、源码证据和修订记录。
- `AGENTS.md` 包含规则入口、触发条件和优先阅读要求。
- `rg` 检查没有错误的旧路径引用。
- `git diff --check -- AI_DOC AGENTS.md` 通过。

单次 RTL 分析只有同时满足以下条件才算完成：

1. 已确定版本、分支、commit 和权威源码。
2. 已检索并对比对应版本旧知识。
3. 已更新或新增正确分类的知识文档。
4. 已在需要时更新索引和交叉引用。
5. 当前正文不存在互相矛盾的有效结论。
6. 已向用户说明分析结论和本次知识库修改位置。

## 11. 首次知识回填

skill 和管理规则实现并验证后，必须立即用新规则整理本轮已经完成的 RTL 分析，不把聊天内容直接复制为一篇临时报告。

首次回填范围包括：

- `DynInst.uop.flushPipe` 的字段语义和后端传递路径。
- 普通 Load/Store 流水中 `uop.flushPipe` 的赋值行为。
- StoreQueue 执行 CBO/CMO 时动态置高 `uop.flushPipe` 的条件。
- SFENCE 在 Decode/Fence FU 中产生和传递 `flushPipe` 的路径。
- CBO 与 SFENCE 最终汇合到 ROB `flushAfter` 的共同机制及两者差异。
- memory trigger 的 `TriggerAction` 编码、Load/Store 地址匹配条件和 ROB 异常/Debug Mode 消费路径。

回填前必须按 V2 路由规则核对当前分支、profile、commit 和 Scala 权威来源。内容应优先整合进已有相关 flow；仅当现有 flow 无法承载时才新增长期 flow 文档。所有受影响索引和交叉引用必须同步更新。
