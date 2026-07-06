# mem_ut 测试框架 Plan 生成与评审规则

本文约束 `AI_DOC/plan/test_framework/plan` 下 mem_ut 测试框架 plan 的生成、修改、重构和评审。后续用户要求“生成测试框架 plan”“审查测试框架 plan”“重构测试框架 plan”“分析某个测试框架方案是否完善”时，必须先阅读本文。

本文适用于 plan 阶段，不要求进入 coding。执行 coding 的完整流程仍按：

- `AI_DOC/project_management/mem_ut_test_framework_plan_execution_rule.md`

如果 plan 涉及 testcase、virtual sequence、agent sequence 调度或通过仿真入口选择场景，还必须读取：

- `AI_DOC/project_management/mem_ut_virtual_sequence_rule.md`

---

## 1. 基本定位

测试框架 plan 的首要目标是构建测试激励行为。

生成或评审测试框架 plan 时，必须按以下定位组织内容：

1. 测试框架 plan 描述如何生成、调度、驱动和记录 transaction、sequence、参数、状态和响应事件。
2. 覆盖率收集、scoreboard/checker 对比、RM 参考模型判断属于对应组件职责，不作为测试框架 plan 的主目标。
3. plan 可以记录 coverage/checker/RM 的后续衔接点，但不能把测试框架激励构造方案写成 coverage/checker/RM 实现方案。
4. 分析地址、权限、异常、redirect、replay 等激励 plan 时，优先判断“该激励是否能被框架稳定构造并合法驱动”，再判断是否需要其他组件补 coverage 或 check。

如果用户明确说明某个 plan 只用于扩展激励空间，评审时不应以“没有覆盖率目标”或“没有 checker 闭环”为 blocker；只能把这些项列为后续组件工作或剩余风险。

---

## 2. 激励合法性判断

测试框架 plan 评审中，不能把“当前没有 coverage/checker”误判为“激励不合法”。

本文中的“非法激励”不是指会让 DUT 触发异常的激励。

如果 DUT 明确支持接收某类输入，并通过异常、fault、redirect、replay、drop、ignore、nack 或其他架构/微架构定义行为吸收该输入，则该激励不应仅因为触发异常而被判为非法，也不应把 DUT 合法异常结果视为 DUT 错误。

一个激励组合只有在以下情况下才应被判为不应生成或需要禁止：

1. 违反 DUT 接口协议或输入约束。
2. 违反当前测试框架 transaction、sequence、driver、responder 的状态生命周期。
3. 会导致环境内部状态不可恢复，例如必要索引无法分配、队列无法消费、终态无法闭环。
4. 与该 plan 明确声明的激励目标不一致，且会让参数名、标签或 debug 输出表达错误含义。
5. 该激励属于测试框架 plan 明确复刻、代理或约束的 DUT/Scala 行为流，但相较对应 Scala 源码行为，该组合不会由该行为流产生。

第 5 条仅在测试框架 plan 明确在框架内复刻、代理或约束某段 DUT/Scala 行为流时适用，例如入队、发射、LSQ 分配、commit、deq、redirect/replay 恢复等，并且 plan 声称生成结果代表该行为流可能产生的组合。

此类判断只验证框架复刻出的输入/输出组合是否与对应 Scala 源码行为一致，不要求测试框架 plan 证明 DUT 正确性，也不要求对与激励生成无关的微架构行为做完整证明。例如测试框架模拟 DUT 后端入队和发射模式时，若某个入队/发射组合在 Scala dispatch、issue、LSQ 或执行单元选择逻辑中不可能出现，则该组合应视为非法激励，plan 必须优化避免或报错。

非法激励判断应按以下顺序进行：

1. 先判断 DUT 输入层是否合法：是否满足公开接口协议、握手时序、编码约束和输入前置条件。违反这些约束的输入属于 DUT 输入层非法，即使 DUT 实现可能 drop、ignore 或 nack，也不应作为合法激励驱动。
2. 在输入层合法的前提下，判断该输入是否触发 DUT 定义的异常、fault、redirect、replay、drop、ignore 或 nack。若该响应属于架构/微架构定义行为，则该激励是合法异常激励。
3. 仅当测试框架声称模拟某段 DUT/Scala 行为流时，再判断 DUT 行为模拟层是否合法。

非法激励判断分两层：

```text
DUT 输入层非法：
  DUT 接口、协议或输入约束不支持该激励，测试框架不应驱动。

DUT 行为模拟层非法：
  测试框架声称模拟某段 Scala/DUT 行为，但生成了该 Scala/DUT 行为不会产生的组合，
  测试框架不应把这种组合当作合法行为激励。
```

反例：

```text
合法异常激励：
  访存地址、权限或属性组合使 DUT 触发 page fault、access fault、misaligned exception。
  只要 DUT 支持接收并用定义行为处理该输入，就不是非法激励。

非法行为激励：
  测试框架模拟 DUT 入队/发射路径，却生成 Scala 源码中不会出现的 fuType/fuOpType/LSQ flow/
  issue port/numLsElem 组合。
```

如果激励能被稳定构造并合法驱动，但 coverage/checker 暂未覆盖，应在 plan 中写为：

```text
后续 coverage/checker/RM 衔接点
```

而不是作为测试框架 plan 的 blocker。

---

## 3. Plan 内容边界

测试框架 plan 应重点写清：

1. 新增或修改的 transaction 字段、参数、枚举、权重、fixed override。
2. 生成顺序、随机约束、directed 控制、fallback/fatal 策略。
3. sequence、handler、scheduler、adapter、driver/responder 的职责边界。
4. 运行期状态生命周期，包括创建、更新、消费、flush/replay/redirect 恢复和结束条件。
5. 测试框架自身的激励有效性检查机制，例如参数组合是否合法、是否成功生成目标 transaction、标签与实际生成字段是否一致、fixed 模式是否命中目标激励。
6. debug dump、基础统计和必要标签记录。

激励有效性检查只检查生成过程是否自洽，包括约束是否满足、目标 transaction 是否生成、fixed override 是否命中、字段/标签/debug 统计是否一致、框架内部状态是否闭环。不得根据 DUT observed result、RM expected value、scoreboard compare 或 coverage bin 命中情况判断该激励是否通过或正确。

debug dump 和基础统计仅用于记录生成命中率、fallback/retry/drop 次数、标签分布等框架运行信息，不得写成功能覆盖率 coverpoint、cross 或覆盖率达标规则。

测试框架 plan 不应展开实现：

1. coverage bin 的完整定义。
2. scoreboard/checker 的比较规则。
3. RM 模型的期望值推导。
4. 与本激励生成无关的 DUT 微架构证明。

DUT 正确行为相关的 checker、scoreboard、RM 对比、coveragent 或功能覆盖率机制，必须只放在 plan 末尾的协同支持章节中说明，不能混入测试框架本体实现步骤。

测试框架 plan 若涉及 DUT 正确行为 check 或 coveragent，应在文档末尾增加以下章节：

```text
## RM 协同支持

本 plan 不实现 RM/checker/scoreboard。
本节只记录后续 RM/checker/scoreboard 可使用的字段、事件、标签或统计入口。

## 功能覆盖率协同支持

本 plan 不实现 coveragent/covergroup。
本节只记录后续功能覆盖率可使用的字段、事件、标签或交叉维度。
```

规则：

1. `RM 协同支持` 只记录后续正确性检查组件可能需要的观测点、字段、事件或接口，不写 checker/RM 算法实现。
2. `功能覆盖率协同支持` 只记录后续 coveragent 可能采样的字段、标签和交叉维度，不写 covergroup 代码实现。
3. 两个章节必须明确标注“本 plan 不需要实现”。
4. 测试框架本体需要实现的检查只限于激励有效性检查，不承担 DUT 结果正确性判断。

如果确实需要这些内容，应只写接口和后续 plan 落点，例如：

```text
本 plan 只记录 boundary_profile 字段，coverage 后续在对应 covergroup plan 中使用该字段交叉统计。
```

---

## 4. UVM/SystemVerilog 复用原则

生成 plan 或评审 plan 的具体代码实现方案时，必须优先评估并复用项目已有模式、SystemVerilog/UVM 内建能力和公共 helper。若项目已有模式与通用 UVM 机制不同，以项目一致性优先。

优先复用对象包括但不限于：

1. `uvm_object` 的 `copy`、`clone`、`compare`、`do_print`、`convert2string`、field automation，或项目已有的等价手写模式。field automation 仅在项目风格允许且不会破坏可控性/性能时使用。
2. UVM reporting：`uvm_info`、`uvm_warning`、`uvm_error`、`uvm_fatal`，不要自造日志等级体系。
3. SystemVerilog queue、dynamic array、associative array、enum、constraint、`randomize()`、`dist`、`inside`、`with`。
4. UVM config/resource、sequence/sequencer/driver 的标准交互模式。
5. 项目内已经存在的公共 helper、参数 getter、状态表访问函数和 transaction 派生函数。

如果 plan 选择自定义以下逻辑，必须在 plan 中说明原因：

```text
随机权重选择
对象拷贝/比较/打印
队列或 map 管理
字符串格式化和 dump
生命周期调度
状态查找索引
```

允许自定义的条件：

1. UVM/SystemVerilog 内建能力不能满足当前语义。
2. 项目已有兼容模式要求使用本地 helper。
3. 性能路径上必须避免通用机制的额外开销。
4. 需要统一项目日志、参数或状态生命周期。
5. 生命周期调度或状态查找索引承载框架特定不变量，现有 helper 无法直接表达。

即使自定义，也应在 plan 中说明：

```text
为什么不用 UVM/SV 自带机制
复用哪些已有项目 helper
自定义逻辑的输入、输出、副作用和失败策略
生命周期调度或状态查找索引的状态来源、更新点和恢复策略
```

---

## 5. 正式可 coding Plan 整理规则

当用户要求“把某个 plan 整理成正式版本”“整理成可以 coding 的 plan”“按 flow 形式重构 plan”“将初步方案整理成正式 plan”时，必须执行本节规则。

### 5.1 适用目标

正式可 coding plan 的目标不是保留问答过程或讨论记录，而是把已确认的初步方案整理成可直接执行 coding 的功能 flow 方案。

正式 plan 必须满足：

1. 包含初步 plan 中所有仍合理的目标、参数、规则、约束、失败策略、验证要求和文档同步要求。
2. 删除或改写初步讨论中的问答式表达、历史反复、临时疑问和已经被推翻的旧方案。
3. 文档结构完全按 flow 规则组织，直接描述“最终要实现的功能 flow 和每个 flow 中的逻辑”。
4. 每个关键函数、task、helper、参数和状态字段必须说明输入来源、输出结果、状态副作用、失败策略和后续影响。
5. 文字伪代码必须符合 `AI_DOC/project_management/mem_ut_flow_document_rule.md` 和 `AI_DOC/project_management/mem_ut_test_framework_logic_build_rule.md` 中的规则。

### 5.2 本 agent 与 subagent 分工

正式 plan 整理必须采用以下分工：

```text
本 agent：
  负责阅读初步 plan、关联规则、相关源码或 flow 文档。
  负责生成正式可 coding plan。
  负责根据 review 反馈修改正式 plan。
  负责最终确认正式 plan 没有遗漏、冲突和逻辑问题。

subagent：
  负责独立 review 本 agent 生成的正式 plan。
  不负责替代本 agent 做最终判断。
  review 必须覆盖内容完整性、功能逻辑、测试框架整体影响和性能风险。
```

如果用户明确要求“subagent 执行，本 agent review”，则按用户要求调整执行人；但最终仍必须由本 agent 对正式 plan 做最后 review 和结论确认。

### 5.3 正式 Plan 格式规则

正式 plan 必须按 flow 文档方式组织，不得写成问答式或讨论式文档。

文件生成规则：

```text
将初步 plan 整理成正式可 coding plan 时，必须新建正式 plan 文档。
不得在 review 通过前直接覆盖或删除初步 plan 文档。

正式 plan 建议命名为：
  <feature>_formal_plan_<YYYYMMDD>.md
  或 <feature>_coding_plan_<YYYYMMDD>.md

正式 plan 生成后：
  subagent review 正式 plan。
  如果 review 发现问题，本 agent 修改正式 plan 文档。
  修改后继续 review。
  只有最后一轮 review 明确无必须修改项后，才允许删除、移动或归档原初步 plan 文档。

如果用户明确要求保留初步 plan：
  不删除原初步 plan。
  在正式 plan 或最终回复中说明原初步 plan 保留原因。
```

推荐结构：

```text
1. Plan 定位
   说明该 plan 是测试框架激励生成、调度、状态维护、debug 统计还是其它类型。

2. 目标功能 Flow 总览
   用 Mermaid 或文字 flow 描述最终功能入口、主流程、关键分支和退出路径。

3. 专有名词和字段语义
   解释新增 enum、profile、mode、参数、状态字段和标签的权威来源。

4. 参数与配置 Flow
   描述 plus/cfg 参数、默认值、合法性检查、权重选择和 directed 使用方式。

5. 主流程实现 Flow
   按真实执行顺序描述入口函数、关键 helper、分支、状态更新和失败策略。

6. 关键 helper 细节
   每个新增或修改 helper 单独成节，包含职责、输入、输出、副作用和文字伪代码。

7. 失败策略与非法激励处理
   说明 UVM_FATAL/UVM_ERROR/UVM_WARNING/retry/fallback/drop 的触发条件和原因。

8. 验证与 smoke 方案
   给出最小仿真、directed 参数组合、预期日志或 dump 检查点。

9. 与初步 plan 差异说明
    本章只服务 review，不作为 coding 实现依据。
```

格式要求：

1. 正式 plan 正文应描述最终方案，不保留“是否可以”“建议考虑”“可能需要”等未决表述。
2. 若某项仍是后续扩展，必须明确写成“后续扩展，不属于本 plan 实现范围”。
3. 不得把 review 问题清单散落在实现 flow 中；review 发现的问题应转化为正式方案约束。
4. 函数、task、helper 的文字伪代码必须按真实执行顺序展开，不能只列函数名。
5. 如果正式 plan 涉及已有源码，函数名、字段名、参数名必须与源码或计划新增名称一致。
6. 正式 plan 中所有函数 flow、helper flow 和关键分支 flow 的文字伪代码必须遵循
   `AI_DOC/project_management/mem_ut_flow_document_rule.md`、
   `AI_DOC/project_management/mem_ut_code_review_document_rule.md` 和
   `AI_DOC/project_management/mem_ut_test_framework_logic_build_rule.md` 中的文字伪代码规则。
   文字伪代码不能只写“调用 xxx()”，凡出现子函数调用，必须简短说明该子函数在当前 flow 中负责什么内部逻辑功能、输入来源、输出或状态副作用，以及返回值如何影响后续分支。
7. 正式 plan 不要求单独设置“性能与可扩展性分析”章节。
   如果 review 发现性能问题，必须在功能完全满足的前提下，把更高性能、更简单的逻辑直接写入主流程实现 Flow 或关键 helper 细节中。
   最后一章 `与初步 plan 差异说明` 记录修改目的、修改前/后的逻辑行为和差异影响。
8. 正式 plan 不要求固定设置 “Debug/统计/RM/覆盖率协同支持” 章节。
   只有当该功能确实需要给 debug、RM、checker、scoreboard 或 coveragent 留后续衔接入口时，才在 plan 末尾增加对应协同支持章节。
   协同支持章节只能描述后续可使用的字段、事件、标签或统计入口，不能实现 DUT 正确性 checker、RM 或 covergroup。

### 5.4 Subagent Review 必查内容

subagent review 必须至少检查以下内容：

```text
1. 内容完整性：
   正式 plan 是否包含初步 plan 的全部合理内容和规则。
   是否遗漏参数、默认值、开关、权重、fixed/direct 模式、失败策略、验证要求或文档同步要求。
   是否错误删除了用户明确确认过的约束。

2. 测试框架整体角度：
   该方案是否破坏主表生成、sequence 调度、状态生命周期、driver/responder、debug dump 或参数管理的一致性。
   是否把 coverage/checker/RM 职责混入测试框架激励生成主流程。
   是否存在非法激励判断错误，把 DUT 合法异常激励误判为非法，或把框架不支持行为误放行为合法激励。

3. 功能本身角度：
   目标功能 flow 是否闭环。
   选择顺序、候选构造、权重随机、fallback/fatal、标签同步和校验策略是否自洽。
   每个关键分支是否说明触发条件和后续行为。
   新增 helper 的输入、输出、副作用是否清楚。

4. 性能优化角度：
   是否引入每 transaction 全表扫描、重复大集合遍历、无界 retry、无界 queue 搜索或运行期高复杂度逻辑。
   是否可通过候选缓存、索引、有限窗口、预构建表或已有 helper 降低复杂度。
   性能优化是否没有牺牲激励语义或 debug 可解释性。
   如果存在更高性能且逻辑更简单的解法，review 不能只要求新增分析章节；必须要求把该解法写入正式 plan 的主 flow 或关键 helper 中。
   正式 plan 的目标是在功能完全满足的前提下，直接采用性能高、逻辑简单的最终实现方案。

5. 文档格式角度：
   正式 plan 是否按 flow 规则整理。
   是否仍残留问答式内容、讨论过程、互相矛盾的旧结论或临时方案。
   文字伪代码是否足够具体，读者不读源码也能复现控制流。
   文字伪代码是否符合文字伪代码规则：不能只罗列函数名；调用子函数时必须简短说明子函数在当前 flow 中负责的内部逻辑功能、输入来源、输出或状态副作用。
   如果差异章节涉及新增函数，是否在差异分析后提供新增函数的详细文字伪代码。
   如果差异章节涉及修改函数，是否用文字伪代码分别描述修改前逻辑和修改后逻辑。
```

### 5.5 Review-Modify 循环

正式 plan 必须执行 review-modify 循环：

```text
本 agent 生成正式 plan。
subagent 执行 review。

如果 subagent 发现问题：
  本 agent 修改正式 plan。
  本 agent 不得只在回复中解释，必须把合理问题落回 plan 正文。
  修改后再次请求 subagent review。

重复上述流程，直到最新一轮 subagent review 没有必须修改的问题。

最后一轮无问题后：
  本 agent 再做最终 review。
  本 agent 在最终回复中说明正式 plan 已通过最后一轮 review。
```

如果 subagent 反馈中存在本 agent 判断不合理或与用户要求冲突的建议，本 agent 必须在最终回复中说明拒绝原因；若该冲突会影响 coding，必须在正式 plan 的差异说明或风险章节中记录。

### 5.6 与初步 Plan 差异说明章节

正式 plan 最后一章必须是：

```text
## 与初步 plan 差异说明
```

该章只用于审稿 review，不作为 coding 实现依据。

该章只允许描述功能实现层面的差异，不描述文档组织形式差异。

必须描述的差异类型包括：

```text
1. 实现功能差异：
   新增、删除或改变了哪些测试框架功能行为。

2. 功能代码实现逻辑差异：
   关键入口、选择顺序、候选构造、状态更新、fallback/fatal、标签同步等逻辑如何变化。

3. 函数/helper 差异：
   新增哪些函数/helper，删除或不再新增哪些函数/helper，复用哪些已有函数/helper。
   对新增函数/helper，必须简短说明该函数/helper 的功能、输入、输出和副作用。
   对修改函数/helper，必须说明修改前逻辑和修改后逻辑。

4. 参数和配置差异：
   新增、删除或改变哪些 plus/cfg 参数、默认值、合法性检查和 directed 控制方式。

5. 行为边界差异：
   哪些场景仍保持旧行为，哪些场景进入新行为，哪些路径明确 fatal/error/default。
```

禁止写成以下内容：

```text
只说明“把讨论整理成正式 flow”。
只说明“章节重新组织”。
只说明“删除问答式表述”。
只说明“伪代码移到某章节”。
```

该章必须使用文字伪代码描述：

```text
修改目的：
  说明为什么需要修改功能行为、实现逻辑、函数/helper 或参数。

修改前逻辑行为：
  按初步 plan 或当前实现的原始行为描述入口、关键分支、状态更新、函数调用和失败策略。
  如果描述中出现子函数调用，必须简短说明该子函数在旧逻辑中的功能和副作用。

修改后逻辑行为：
  按正式 plan 的最终行为描述入口、关键分支、状态更新、函数调用和失败策略。
  如果描述中出现子函数调用，必须简短说明该子函数在新逻辑中的功能和副作用。

差异影响：
  说明差异是否改变功能语义、参数语义、默认行为、函数/helper 落点、失败策略、性能路径、验证方式或文档同步范围。
```

如果差异章节涉及新增函数/helper，必须在差异分析后补充该新增函数/helper 的详细文字伪代码。

新增函数/helper 详细文字伪代码要求：

```text
函数名：
  写明新增函数/helper 名称。

添加原因：
  写明为什么必须新增该函数/helper，不能只写“为了整理 flow”。

功能说明：
  写明该函数/helper 的输入、输出、读取或修改的 transaction 字段、queue、cache、map、参数或状态副作用。

详细文字伪代码：
  先说明该函数/helper 在当前功能 flow 中承担什么角色。
  再按执行顺序描述关键判断、循环、候选构造、状态更新、fatal/error/warning、return 和 fallback。
  凡调用子函数，必须简短说明该子函数在本函数中负责什么内部逻辑功能、输入来源、输出或状态副作用。
  最后说明本函数/helper 的返回值或副作用如何影响后续 flow。
```

如果差异章节涉及修改已有函数/helper，必须用文字伪代码分别描述修改前和修改后的函数逻辑。

修改函数/helper 差异要求：

```text
函数名：
  写明被修改函数/helper 名称。

修改原因：
  写明为什么需要修改该函数/helper 的功能逻辑。

修改前文字伪代码：
  按旧逻辑执行顺序描述入口检查、关键分支、子函数调用、状态更新和失败策略。
  凡调用子函数，必须说明该子函数在旧逻辑中的功能和副作用。

修改后文字伪代码：
  按新逻辑执行顺序描述入口检查、关键分支、子函数调用、状态更新和失败策略。
  凡调用子函数，必须说明该子函数在新逻辑中的功能和副作用。

差异影响：
  写明该函数/helper 修改后对上层 flow、参数语义、失败策略、性能路径或 debug 标签的影响。
```

如果正式 plan 与初步 plan 没有功能差异，也必须写：

```text
本次整理不改变功能行为、实现逻辑、函数/helper、参数和失败策略。
```

### 5.7 结束条件

正式可 coding plan 只有在满足以下条件后才算完成：

1. 正式 plan 已按 flow 规则整理，正文不是问答式。
2. 初步 plan 的全部合理内容和规则已被吸收。
3. subagent 最后一轮 review 未发现必须修改的问题。
4. 本 agent 完成最终 review。
5. `与初步 plan 差异说明` 章节已补齐。
6. 文档通过基础格式检查，例如 markdown 代码块闭合、无明显冲突表述。

---

## 6. 评审问题清单

生成或评审测试框架 plan 时，至少回答以下问题：

1. 该 plan 的目标是激励生成、调度驱动、状态维护、响应建模还是 debug/统计。
2. plan 是否把 coverage/checker/RM 职责混入测试框架主流程。
3. 如果某个激励组合被禁止，禁止原因是 DUT 输入层非法、DUT 行为模拟层非法、框架状态生命周期非法、plan 目标/标签/debug 语义不一致，还是仅仅缺少 coverage/checker。
4. 新增参数和 fixed override 是否能稳定构造目标激励。
5. fixed 模式是否禁止静默 fallback，或明确说明 fallback 不会破坏测试意图。
6. plan 是否把 DUT 正确行为 check 放入末尾 `RM 协同支持`，并明确本 plan 不实现。
7. plan 是否把 coveragent/功能覆盖率内容放入末尾 `功能覆盖率协同支持`，并明确本 plan 不实现。
8. 测试框架本体是否只实现激励有效性检查，而非 DUT 结果正确性判断。
9. 新增字段是否只有一个权威来源，是否避免同一语义多字段重复维护。
10. 新增 helper 是否优先复用 UVM/SystemVerilog 或项目已有 helper。
11. 自定义随机、拷贝、比较、打印、队列或索引逻辑是否说明不用已有机制的原因。
12. plan 是否给出失败策略：fatal、warning、retry、fallback 或 drop。
13. 如果 plan 明确模拟某段 DUT/Scala 行为，是否列出对应源码依据，并说明测试框架不会把源码中不可能出现的行为组合标记为合法行为激励。
14. plan 是否给出最小验证或 smoke 方案；如果只生成 plan，不要求执行仿真，也应写清后续验证入口。
15. sequence、handler、scheduler、adapter、driver/responder 的职责边界是否清楚，是否避免同一状态或同一决策被多个组件重复维护。
16. 运行期状态生命周期是否覆盖创建、更新、消费、flush/replay/redirect 恢复和结束条件。
17. debug dump、基础统计和标签是否只服务于激励生成可观测性，而没有混入 coverage/checker 语义。
18. 如果 plan 新增 function、task、class method、helper 或参数，是否同步满足对应外部规则文件要求。

---

## 7. 与其他规则的关系

如果 plan 提出新增 function、task、class method 或关键 helper，还必须满足：

- `AI_DOC/project_management/mem_ut_test_framework_logic_build_rule.md` 中的 “Plan 文档新增函数描述规则”

如果 plan 涉及参数新增、重命名或删除，还必须满足：

- `mem_ut/ver/ut/memblock/rule/memblock_parameter_management_rule.md`

如果 plan 涉及 AI_DOC 文件新建、移动或归档，还必须满足：

- `AI_DOC/project_management/ai_doc_file_management_rule.md`
