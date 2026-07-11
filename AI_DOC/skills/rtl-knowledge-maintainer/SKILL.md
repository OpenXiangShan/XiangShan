---
name: rtl-knowledge-maintainer
description: 分析并维护 XiangShan mem_ut 项目的版本化 RTL/Scala 知识库。用户要求分析、解释、追踪或比较 RTL、Scala/Chisel 模块、信号、字段、端口、Bundle、pipeline、状态机、队列、异常、redirect、replay、flush、writeback、MemBlock/LSQ/DCache/TLB/ROB 行为或 V2/V3 差异时使用；每次都要核对版本和权威源码，校正已有知识，并按顶层 interface-agent 或内部 rtl-flow 分类更新仓库文档。
---

# RTL 知识库维护

把每次 RTL/Scala 分析变成对应版本长期知识的增量更新。不得只回答用户而不维护知识库。

## 强制读取

开始分析前完整阅读：

- `AI_DOC/project_management/rtl_knowledge_base_management_rule.md`
- `AI_DOC/project_management/ai_doc_file_management_rule.md`
- `AI_DOC/project_management/ai_doc_language_rule.md`
- 对应版本的 `mem_ut/ver/ut/memblock/rule/version/<v2|v3>/branch_policy.md`
- 对应版本的 `mem_ut/ver/ut/memblock/rule/version/<v2|v3>/memblock_rtl_profile.md`

创建或重构知识文档时读取 [references/document_templates.md](references/document_templates.md)。

## 执行流程

### 1. 确定版本和源码基线

按以下优先级确定 V2/V3：

1. 使用用户明确指定版本。
2. 未指定时读取当前分支和 `branch_policy.md`。
3. 用 `memblock_rtl_profile.md` 核对权威 RTL/Scala 来源。
4. 记录 `git rev-parse HEAD`、分支和核验日期。

用户指定、分支、profile 或源码基线冲突时，停止知识库写入并询问用户。不得猜测版本。

### 2. 检索旧知识

先读取对应版本索引：

```text
AI_DOC/analysis/interface/<version>/index.md
AI_DOC/analysis/rtl/<version>/index.md
```

再用 `rg` 搜索：

- 用户问题关键词。
- 模块、类、字段、端口和函数原名。
- 同义行为，例如 flush、redirect、replay、exception、writeback。

阅读全部相关 agent/flow 文档。禁止只根据文件名判断没有旧知识。

### 3. 追踪真实源码

至少确认：

- 定义和类型。
- compile-time capability 与 runtime value 的区别。
- 所有关键赋值或生成条件。
- 上游生产者和下游消费者。
- combinational 与 register 边界。
- 置位、保持、覆盖和清除条件。
- 分支优先级、异常路径和被 kill/flush 条件。
- 同名局部变量或其他模块字段是否属于同一信号。

所有结论必须能回溯到当前版本权威源码。旧文档只能作为检索线索，不能替代源码证据。

### 4. 选择知识分类

顶层端口、Bundle、位宽、方向、握手和 UVM 映射写入：

```text
AI_DOC/analysis/interface/<version>/agents/<agent_name>.md
```

内部 pipeline、状态机、queue、仲裁、异常、redirect、replay、flush、writeback 和跨模块行为写入：

```text
AI_DOC/analysis/rtl/<version>/flows/<flow_name>.md
```

优先合并到现有 flow。只有新行为具有独立入口、状态演进和终止条件，且无法自然归入已有 flow 时才新建文档。不得用本次问题标题创建临时报告。

跨多个 agent/flow 时分别更新；主文档保留完整分析，相关文档写摘要和交叉链接。

### 5. 比较并更新旧知识

- 一致：增强已有正文的证据、时序和边界。
- 部分一致：保留有效内容，直接修订失效部分。
- 不一致：以当前版本权威源码修订正文，并在“知识修订记录”记录旧结论、新结论、原因、commit 和影响。
- 无法确定：保留旧结论，在“待确认项”记录冲突证据和验证方法。
- 版本不同：分别维护 V2/V3，并记录版本差异链接。

更新前检查目标文档的未提交改动。只做与本次分析相关的最小编辑；无法安全合并时停止并报告。

### 6. 更新索引与交叉引用

新增、重命名或扩大 agent/flow 范围时，更新对应版本 `index.md`。索引条目至少包含：

- 文档链接。
- 关键词。
- 覆盖模块。
- 入口信号或函数。
- 关联 agent/flow。

### 7. 验证并交付

运行：

```bash
git diff --check -- AI_DOC AGENTS.md
```

确认版本元数据、源码路径和交叉链接正确，正文只有一个当前有效结论，且没有跨版本混写。

最终答复必须包含：

- RTL 分析结论。
- 关键置位/触发条件和功能影响。
- 版本、分支、commit 和源码基线。
- 本轮更新或新增的知识库文件链接。
- 仍待确认的边界或冲突；没有则明确说明没有。

## 禁止事项

- 不得把 `FuConfig` 等能力开关直接解释为运行时信号置高。
- 不得把 V2 结论写入 V3 文档或反向写入。
- 不得为每次问题创建带日期的重复分析报告。
- 不得只在旧文档末尾追加与正文矛盾的新结论。
- 不得覆盖用户已有的无关修改。
- 不得在知识库未更新时宣称分析完成。
