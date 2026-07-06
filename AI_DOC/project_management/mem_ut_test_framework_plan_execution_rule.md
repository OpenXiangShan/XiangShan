# mem_ut 测试框架 Plan 执行通用规则

本文约束基于 `AI_DOC/plan/test_framework/plan/undo/*.md` 执行 mem_ut 测试框架 coding 的完整流程。后续用户要求“按照某个测试框架 plan 实现代码”“执行 plan”“根据 plan 修改代码并生成 review”“plan coding 完成后归档”时，必须先阅读本文。

## 1. 适用范围

适用于所有 mem_ut 测试框架 plan 的执行，包括但不限于：

- `common_data_transaction`、status table、issue queue、redirect/replay/fault、commit/deq、TLB/DCache responder 等运行期逻辑修改。
- sequence、handler、scheduler、adapter、monitor service loop、公共 helper、参数化入口修改。
- plan 执行后需要同步 flow 文档、analysis 文档、review 文档和 plan 归档的任务。

不适用：

- 只写 plan、不 coding 的任务。
- 只做源码问题分析、不落代码的任务。
- RM/scoreboard 专项 plan。RM 类 plan 需要按 RM 目录和规则单独处理。

## 2. 执行前准备

### 2.1 必读规则

执行测试框架 plan 前，必须读取：

1. `AI_DOC/project_management/ai_doc_file_management_rule.md`
2. `AI_DOC/project_management/mem_ut_test_framework_logic_build_rule.md`
3. `AI_DOC/project_management/mem_ut_code_review_document_rule.md`
4. 本文档

如果本轮涉及 testcase、virtual sequence、agent sequence 调度或通过 makefile 选择仿真场景，还必须读取：

- `AI_DOC/project_management/mem_ut_virtual_sequence_rule.md`

如果本轮涉及 flow 文档同步，还必须读取：

- `AI_DOC/project_management/mem_ut_flow_document_rule.md`

如果本轮涉及参数新增、重命名或删除，还必须读取：

- `mem_ut/ver/ut/memblock/rule/memblock_parameter_management_rule.md`

如果本轮涉及 L2TLB responder、DTLB/L2TLB request/response 或 TLB 表项，还必须读取：

- `mem_ut/ver/ut/memblock/rule/memblock_l2tlb_agent_rule.md`

### 2.2 工作区基线

执行 coding 前必须先检查工作区：

```bash
git status --short
```

若用户明确要求“先提交当前修改”，必须在 coding 前完成本地 commit。提交前需要：

1. 识别当前修改是否属于本轮准备提交范围。
2. 不把无关脏改动混入提交。
3. 对未跟踪但属于本轮文档/plan 的文件一并纳入。
4. 使用清晰 commit message，说明这是执行 plan 前的基线提交。

如果存在无关脏改动且无法确认归属，不得擅自提交或回滚，必须向用户说明。

## 3. Plan 审查与修正

执行 coding 前必须审查 plan 本身是否可执行。

Plan 生成、修改或评审阶段的通用规则见：

- `AI_DOC/project_management/mem_ut_test_framework_plan_review_rule.md`

执行 coding 前必须确认 plan 已满足该规则。

检查项：

1. plan 是否在正确目录：`AI_DOC/plan/test_framework/plan/undo`。
2. plan 是否带日期。
3. plan 是否列出关联源码、修改目标、参数/字段/函数方案、伪代码、验证计划。
4. plan 是否包含尚未实现的其他 plan 功能。如果包含，只能保留依赖边界；具体字段、函数、数据结构和实现细节必须移到对应 plan。
5. plan 是否和当前源码一致。如果当前源码已变化，必须先修正 plan 或在 plan 中新增“执行前修正”章节。
6. plan 是否存在影响语义正确性的风险，例如状态字段语义混用、redirect/replay/fault 顺序不清、退出条件不闭环、旧参数未删除。
7. plan 是否误把 coverage/checker/RM 职责塞进测试框架激励构造主流程；如果有，必须按 plan review 规则拆分职责。
8. plan 中新增的关键逻辑是否优先复用 SystemVerilog/UVM 已有能力；如果不是，必须按 plan review 规则说明原因。

如果执行前审查发现 plan 不完善：

- 必须停止后续 coding，不得继续按不完整 plan 实现。
- 必须向用户返回审查结果，列出 blocker、风险点、缺失逻辑和建议解决方案。
- 不得自行把解决方案直接写入 plan，除非用户明确批准“按该方案补充/修改 plan”。
- 用户批准前，不允许在代码里实现 plan 未定义且会影响架构语义的隐藏逻辑。
- 用户批准后，才能把修正内容落入该 plan 或对应专项 plan，再重新审查 plan 是否可执行。

如果 plan 合理完整：

- 严格按 plan 执行。
- coding 中发现 plan 需要调整时，可以同步更新 plan，但新增或变更内容必须使用本文规定的特殊标记，不能伪装成执行前原始 plan。
- coding 中的 plan 调整必须在最终 review 文档中作为“plan 与实现差异”或“plan 未说明但实现补充的细节”说明。

## 4. Coding 执行规则

### 4.1 subagent 与本 agent 分工

如果用户要求 subagent 执行：

1. subagent 负责按 plan 直接修改代码和相关文档。
2. 本 agent 负责 review subagent 修改，不得只转述 subagent 结论。
3. 本 agent 必须用 `git diff`、`rg`、关键源码阅读和必要仿真结果独立确认。
4. 如果 review 发现遗漏、风险或文档不一致，必须继续修改直到闭环。

如果未要求 subagent，也仍应按本文流程自查。

### 4.2 修改范围控制

执行 plan 时必须遵守：

- 只修改 plan 关联范围内的源码和文档。
- 不保留用户明确要求删除的旧逻辑或兼容层。
- 不引入 plan 未要求的新机制，除非先更新 plan 并说明原因。
- 不把尚未实现的其他 plan 功能混入本 plan coding。
- 涉及参数时必须同步 `env/plus.sv`、`seq_csr_common.sv`、`seq/plus_cfg/default.cfg` 和相关 preset cfg。
- 涉及 sequence 时必须检查 `seq_pkg.sv`、`seq.f`、testcase/package/filelist 是否需要同步。

### 4.3 逻辑一致性检查

代码修改后必须按 plan 逐项对照：

1. 每个新增字段是否有 reset、设置、清理、读取者。
2. 每个新增函数是否有调用点、返回值处理、fatal/warning 策略。
3. 每个旧字段/旧函数/旧参数是否按 plan 删除或改名，不保留冗余兼容逻辑。
4. redirect/replay/fault/flush/sfence 等 recovery 路径是否仍闭环。
5. 主动 sequence 退出条件、被动 responder 生命周期、global stop/drain 行为是否仍一致。
6. 是否存在同一语义多字段重复维护。
7. 高频路径是否避免不必要全表扫描。
8. end check 是否能发现残留状态，但不会把合法终态误判为未完成。

必须用 `rg` 检查旧命名、旧参数、旧函数调用是否残留。

### 4.4 Coding 中调整 plan 的特殊标记

coding 中如果发现 plan 需要调整，必须在 plan 中新增带特殊标记的章节或条目，禁止把新增内容直接混入原 plan 正文导致后续 review 无法识别差异。

推荐章节名：

```text
## 执行中补充/修正（IMPLEMENTATION_DELTA）
```

推荐条目标记：

```text
[IMPLEMENTATION_DELTA]
来源：coding 中发现当前源码/验证结果要求调整。
原 plan：描述执行前 plan 的原始方案。
实现调整：描述本轮实际采用的方案。
原因：说明为什么必须调整。
影响范围：列出源码、文档、验证和风险。
```

规则：

1. `IMPLEMENTATION_DELTA` 内容不是执行前原始 plan 的一部分。
2. review 文档检查 plan 一致性时，必须把 `IMPLEMENTATION_DELTA` 识别为执行中变更，不能当作原 plan 已经覆盖该实现。
3. 所有与执行前 plan 不一致的实现，即使已经写入 `IMPLEMENTATION_DELTA`，也必须在 review 文档“与 plan 不一致的实现”章节中说明。
4. 所有执行前 plan 未说明但 coding 中新增的必要细节，即使已经写入 `IMPLEMENTATION_DELTA`，也必须在 review 文档“plan 未说明但实现补充的细节”章节中说明。
5. 如果用户要求先审查 plan 而非执行 coding，发现不完善时不得写 `IMPLEMENTATION_DELTA`；必须先返回问题和解决方案，等待用户批准。

## 5. 文档同步规则

代码修改完成后，必须同步相关文档。

至少检查：

- 执行的 plan 本身。
- 对应 flow 文档：`AI_DOC/mem_ut_flow_doc/*.md`。
- 对应 analysis 文档：`AI_DOC/analysis/**`。
- 参数或规则变更涉及的 `AI_DOC/project_management/*.md` 或 `mem_ut/ver/ut/memblock/rule/*.md`。
- 网页资产：`AI_DOC/web/**`，如果旧流程图或交互图仍展示旧字段/旧函数。

文档同步原则：

1. 当前有效 flow/analysis/web 文档必须反映最新实现。
2. 历史 review/plan 如果保留旧逻辑正文，必须加注记说明该文档描述旧实现，以最新 plan/review 为准。
3. 不允许 flow 文档继续描述已删除逻辑。
4. 不允许 review 文档漏掉本轮实际代码修改。

## 6. 验证规则

### 6.1 静态检查

至少执行：

```bash
git diff --check -- <本轮修改范围>
rg -n "<旧字段|旧参数|旧函数|旧文档路径>" <相关目录>
```

### 6.2 基础仿真

默认至少执行：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=basicTest ts=virtual_base_sequence mode=base_fun
make eda_run tc=basicTest ts=virtual_base_sequence mode=base_fun
```

如果修改影响真实 dispatch 主流程，至少增加：

```bash
make eda_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=base_fun cfg=tc_dispatch_real_smoke
```

如果 plan 指定专项 testcase，必须运行对应 testcase。若因环境或时间无法运行，必须在最终回复和 review 文档中明确未验证项和风险。

## 7. Review 文档生成规则

coding、文档同步和基础验证完成后，必须生成源码 review 文档。

Review 文档落点：

```text
AI_DOC/plan/test_framework/review_doc/undo/<feature>_implementation_review_YYYYMMDD.md
```

Review 文档必须遵循：

- `AI_DOC/project_management/mem_ut_code_review_document_rule.md`

额外要求：

1. 必须写明关联 plan 路径。
2. 必须检查实现是否和执行前原始 plan 完全一致。
3. 如果 plan 中存在 `IMPLEMENTATION_DELTA` 特殊标记，review 时必须把它识别为执行中变更，不能当作执行前原始 plan 的一部分。
4. 如果实现和执行前原始 plan 不一致，必须新增“与 plan 不一致的实现”章节，逐条说明：plan 原逻辑、当前实现、修改原因、源码和中文伪代码。
5. 如果实现包含执行前 plan 未写但 coding 中必要的细节，必须新增“plan 未说明但实现补充的细节”章节，逐条说明功能、原因、源码和中文伪代码。
6. 如果没有不一致或额外细节，也必须在对应章节写明“均与计划保持一致”。
7. review 文档必须覆盖 `git diff` 中每个有逻辑意义的源码修改点，不能只总结大类。
8. 生成 review 后，必须用 `git diff` 对照检查是否有代码逻辑没有进入 review 文档。

## 8. Plan 归档规则

当且仅当以下条件全部满足时，才能把 plan 从 `undo` 移到 `do`：

1. plan 对应 coding 已完成。
2. 相关文档已同步。
3. 基础验证已完成，或未完成原因和风险已记录。
4. review 文档已生成并通过自查。
5. 本 agent review 确认无 blocker。

移动规则：

```bash
git mv AI_DOC/plan/test_framework/plan/undo/<plan>.md AI_DOC/plan/test_framework/plan/do/<plan>.md
```

如果 plan 是未跟踪文件，可以使用普通 `mv`，然后 `git add` 新路径。

移动后必须检查旧路径引用并更新。

## 9. 最终 Commit 规则

完成 plan 归档和 review 文档后，提交本轮修改。

提交前必须执行：

```bash
git status --short
git diff --check -- <本轮修改范围>
```

`git add` 范围应只包含本轮相关源码、文档、plan 移动和 review 文档，不得混入无关修改。

commit message 必须包含 plan 主题或 plan 文件名，例如：

```text
mem_ut: implement terminal_done_prefix_refactor_plan_20260627
```

默认只做本地 commit，不自动 push。

## 10. 最终回复要求

最终回复必须包含：

1. 基于哪个 plan 执行。
2. 主要代码修改点。
3. 主要文档同步点。
4. 运行过的检查和仿真结果。
5. review 文档路径。
6. plan 是否已移动到 do。
7. commit hash。
8. 未完成项或剩余风险；如果没有，明确说明无已知 blocker。
