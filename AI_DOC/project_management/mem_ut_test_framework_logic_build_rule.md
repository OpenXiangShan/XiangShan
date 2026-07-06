# mem_ut 测试环境逻辑构建规则

本文记录 mem_ut 测试框架在新增、重构或 review 逻辑时的性能与结构规则。目标不是过早优化，而是避免在测试用例构建、sequence 主循环、monitor service loop 或公共状态维护中引入不必要的大批量扫描，导致仿真随 transaction 数量放大后变慢或行为难以定位。

## 1. 适用范围

本规则适用于 mem_ut 测试环境中的运行期逻辑和测试用例构建逻辑，包括但不限于：

- 主表、状态表、TLB 表、issue queue、redirect/replay/fault queue 的维护逻辑。
- active sequence、responder sequence、monitor service loop、handler、adapter、scheduler。
- testcase 构建 directed transaction、地址相关关系、RAW/违例概率提升逻辑。
- 公共 helper、状态推进函数、查找函数、过滤函数、结束条件判断函数。

不适用或低优先级的场景：

- 编译期 filelist/package 组织。
- 单次初始化或单次结束检查中的小规模校验。
- debug dump、失败后报错打印、文档生成类脚本。

## 2. 路径分级原则

新增逻辑前必须先判断该逻辑处在哪类路径上。

### 2.1 高频运行期路径

每拍调用、每个 monitor event 调用、每次 issue/commit/writeback service 调用的逻辑属于高频路径。

要求：

- 不应默认使用全表扫描。
- 优先使用 cursor、prefix、关联数组 map、queue size、pending counter、active window 等低成本状态。
- 如果必须扫描，扫描范围必须有明确上界，且上界应接近真实可活跃窗口，而不是 `main_trans_num` 全表。

典型例子：

```text
不推荐：
  每拍扫描 uid=0..main_trans_num-1 判断是否还有 pending work。

推荐：
  主动发射型 sequence 正常退出使用 terminal_done_uid >= main_trans_num；
  commit/issue 候选选择使用 cursor、queue、active map 或有限 active window。
```

### 2.2 中频事件路径

redirect、replay、sfence/hfence、fault recovery、batch monitor drain 等离散事件属于中频路径。

要求：

- 可以接受按 active window、当前 queue 或当前 TLB entry 集合扫描。
- 不能把中频事件 helper 放进每拍主循环中反复调用。
- 如果事件可能在压力测试中高频出现，应评估是否需要额外索引或队列化。

典型例子：

```text
sfence/hfence 失效可以遍历当前 tlb_entry_by_key；
但不应每拍为了判断是否有 TLB 工作而遍历完整 TLB 表。
```

### 2.3 构建期路径

主表生成、directed case 构造、地址相关关系构造属于构建期路径。

要求：

- 小规模 testcase 可以使用简单扫描。
- 如果该扫描嵌套在每个 uid 的生成过程中，必须评估是否变成 O(N^2)。
- 对于 load/store 候选、同地址候选、同进程 TLB 候选等可复用集合，优先在构建过程中维护 pool 或索引。

典型例子：

```text
不推荐：
  每生成一个 store，都全表扫描已有 load 来选择同地址参考。

推荐：
  生成 load 时把 uid 放入 load_uid_pool；
  生成 store 时直接从 load_uid_pool 随机选择参考 uid。
```

### 2.4 结束检查和 debug 路径

`end_test_check()`、失败 dump、未完成 transaction 报告属于低频路径。

要求：

- 可以全表扫描。
- 扫描结果应服务 debug，可打印 uid、状态字段、queue size、active map size 等信息。
- 不应因为这些低频扫描而引入复杂状态维护。

## 3. 优先选择的高效结构

### 3.1 Prefix

适用于按 uid 顺序最终完成的全局进度。

推荐场景：

- 判断 active driving testcase 是否全部完成。
- 判断从 uid=0 开始连续进入终态的 transaction 数量。终态可以是正常成功，也可以是框架允许的 fault/exception retire。

示例原则：

```text
主动发射型 sequence 正常退出：
  data.advance_terminal_done_uid();
  done = data.dispatch_progress.terminal_done_uid >= data.main_trans_num;
```

该方式比每拍扫描所有 uid 判断完成状态更清晰，也能自然覆盖 redirect/replay 后尚未完成的 transaction；`success` 只表示正常通过结果，不再作为全局退出前缀。

### 3.2 Cursor

适用于按顺序推进的候选选择。

推荐场景：

- ROB commit pointer。
- LSQ commit/deq 顺序推进。
- 主表顺序 admission。

原则：

- cursor 只能向前推进，除非 redirect/flush 明确要求恢复。
- cursor 推进规则必须和 DUT 的 wrap、flag/value、flush 语义一致。
- 不要用全表扫描替代本来有顺序语义的 cursor。

### 3.3 Associative Map

适用于通过外部事件反查 uid 或 transaction。

推荐场景：

- ROB/LQ/SQ event 反查 uid。
- TLB lookup key 查 entry。
- active resource id 查状态。

原则：

- monitor event 到来时应通过 map 定位，不应全表扫 uid 匹配 robIdx/lqIdx/sqIdx。
- map 的插入和删除必须跟资源生命周期一致，例如 enqueue/activate 时插入，commit/deq/retire 时删除。

### 3.4 Queue 和 Pending Counter

适用于“已经准备好等待消费”的工作项。

推荐场景：

- issue queue。
- redirect drive queue。
- exception/replay/fault queue。
- PTW wait replay queue。

原则：

- 如果只需要知道是否有待处理工作，优先使用 `queue.size()` 或 pending counter。
- 如果需要选择最老或最高优先级元素，优先在入队时保存必要轻量字段，例如 uid、target、priority、epoch。
- 不要每拍从主表重新推导 queue 中本应已有的信息。

### 3.5 Pool / Index

适用于构建期重复选择候选集合。

推荐场景：

- directed RAW/地址相关构造。
- load/store 同地址参考选择。
- 同进程、同 VPN、同 ASID/VMID 的 TLB 构造。

原则：

- 构建时顺手维护候选 uid pool。
- 查询时从 pool 中随机或按约束选择。
- 如果 pool 需要删除或失效，必须定义生命周期；如果生命周期不清晰，先使用窗口扫描实现并限制范围。

## 4. 允许扫描的边界

扫描不是完全禁止，但必须满足以下条件之一：

1. 扫描发生在初始化、建表完成检查、测试结束检查或失败 debug 路径。
2. 扫描对象是小队列，例如当前 issue queue、pending redirect queue、batch event list。
3. 扫描范围是明确受限的 active window，而不是完整历史表。
4. 扫描频率低，例如 redirect、sfence、fault recovery 等离散事件。
5. 高效实现会显著增加状态一致性风险，而扫描实现更简单可靠。

如果选择扫描实现，代码或方案中必须说明：

- 扫描对象是什么。
- 扫描频率是多少。
- 最坏扫描窗口多大。
- 为什么不使用 cursor/map/prefix/pool。
- 后续在什么条件下需要优化。

## 5. 不推荐的实现模式

以下模式默认不允许进入高频路径：

- 每拍扫描 `uid=0..main_trans_num-1` 判断是否还有 work。
- 每个 monitor event 全表扫描 uid 匹配 ROB/LQ/SQ。
- 每拍调用“是否还有 runtime activity”这类宽泛 helper，内部再扫描多个表和队列。
- 为了退出 sequence，引入与最终完成语义无关的 `max_cycles`。
- 构建每个 transaction 时都从头扫描完整主表选择候选。
- 在多个 helper 中重复维护含义相同的计数或状态，导致同步成本高于扫描成本。

## 6. 主动与被动 flow 的退出规则

主动发射型 flow：

```text
正常退出：
  terminal_done_uid >= main_trans_num

长时间无推进：
  使用 no-progress / idle-stop 阈值报 uvm_error；
  不作为正常退出条件；
  最终由 UVM timeout 兜底。
```

被动响应型 flow：

```text
有 request/response progress：
  idle_count = 0

连续无 progress：
  idle_count++
  达到 idle_stop_cycle 后退出
```

固定 `max_cycles` 不应作为正常退出条件。若已有 max cycle，应按 flow 类型迁移到上述规则。

## 7. 方案和 Review 必查项

新增或修改测试框架逻辑时，方案和 review 至少回答以下问题：

1. 该逻辑是高频路径、中频事件路径、构建期路径，还是结束/debug 路径。
2. 是否存在每拍或每事件全表扫描。
3. 如果存在扫描，扫描窗口是否受限，是否能改成 prefix/cursor/map/queue/pool。
4. 是否把正常退出条件和 debug timeout 混在一起。
5. 是否新增了 `max_cycles` 作为正常退出条件。
6. 是否重复维护了多个表达同一语义的状态字段或计数器。
7. 是否有明确的状态生命周期，包括插入、激活、消费、删除、flush/replay 恢复。
8. 是否新增过度/二次防御性检查；如果新增，是否受 `MEMBLOCK_MAIN_PERMISSION_DEBUG_CHECK_EN` 控制。
9. 是否需要同步更新 flow 文档、源码分析文档或参数管理文档。

## 8. 过度/二次防御性检查规则

过度/二次防御性检查指正常设计流程已经保证不会触发，但为了定位未知风险、旧逻辑残留、异常状态污染或未来重构误用而额外添加的检查。

典型特征：

- 正常主路径已有明确前置保证，例如主表构建已做一致性检查，后续 hit 路径再次比较同一组字段。
- 检查失败通常表示环境内部状态被破坏、旧实例未清理、调试场景强行注入或未来修改引入了不一致。
- 该检查不是功能正确性的唯一防线；关闭后不应改变正常 responder、driver、handler 的主路径行为。

规则：

1. 这类检查必须受 `MEMBLOCK_MAIN_PERMISSION_DEBUG_CHECK_EN` 控制，默认关闭。
2. 开关关闭时，不允许因为二次防御性检查影响主路径状态更新、response drive、queue 消费或 testcase pass/fail。
3. 开关打开时，检查失败应优先使用 `uvm_error` 报告，除非该错误会导致后续数据结构不可恢复，才允许 `uvm_fatal`。
4. 代码和 plan/review 文档必须明确说明该检查属于 debug/二次防御逻辑，不是正常功能路径的必要条件。
5. 如果某个检查是协议、接口或状态生命周期的基础合法性检查，例如 null handle、非法 uid、非法 enum、out-of-bound index、driver 必要接口缺失，则不属于过度/二次防御性检查，可以不受该 debug 开关控制。

示例：

```text
需要受 MEMBLOCK_MAIN_PERMISSION_DEBUG_CHECK_EN 控制：
  TLB hit 已有 entry 时，再次把 entry 的 tlbAF/tlbPF/tlbGPF/PBMT
  与主表 canonical 权限做一致性比对。

不需要受该开关控制：
  get_main_transaction(uid) 发现 uid 非法或 main_table_by_uid[uid] 为空；
  L2TLB responder 获取不到必要 virtual interface；
  参数权重全 0 导致无法随机选择。
```

## 9. Plan 文档新增函数描述规则

当 `AI_DOC/plan/test_framework/plan/undo` 或 `do` 中的 plan 文档提出新增 function、task、class method 或关键 helper 时，必须在 plan 中同时写清以下内容：

1. 函数目的：说明为什么需要新增该函数，它解决哪个测试框架问题，属于构建期、运行期、事件处理还是 debug 路径。
2. 函数功能：说明输入是什么、输出是什么、会读取或修改哪些公共状态、queue、map、transaction 字段或 driver item。
3. 源码级伪代码：用 `text` 或目标语言代码块按将来 coding 的控制流写出判断、循环、return、fatal/warning、状态更新和子函数调用。
4. 中文文字伪代码：紧跟伪代码后，用中文按执行顺序解释该函数在当前 feature/flow 中承担的功能、每个关键分支为什么这样走、调用到的子函数在本函数中负责什么，以及返回值或副作用如何影响后续逻辑。
5. 子函数说明：如果该函数内部调用新的或已有关键 helper，不能只列 helper 名称，必须说明 helper 的输入来源、输出含义和状态副作用；简单 helper 可以在母函数文字伪代码中一句话说明，复杂 helper 必须单独成节。

Plan 文档中的中文文字伪代码必须遵循 `AI_DOC/project_management/mem_ut_flow_document_rule.md` 与 `AI_DOC/project_management/mem_ut_code_review_document_rule.md` 中已有的文字伪代码规则：不能只写概念摘要，不能只罗列函数名，必须让读者不读 SystemVerilog 细节也能按文字复现该段控制流。

新增函数描述模板：

```text
函数名：xxx()
函数目的：为什么新增该函数，解决哪个问题。
输入：列出入参和关键全局状态来源。
输出/副作用：列出返回值、状态字段、queue/map、driver item 或日志副作用。
源码级伪代码：
  1. ...
  2. ...
中文文字伪代码：
  先说明该函数在当前 feature 中承担什么角色；
  再按执行顺序解释每个判断、赋值、循环、return 和子函数调用；
  最后说明本函数调用到的子函数在当前流程中的职责和副作用。
```

## 10. 具体方案落点规则

本文只记录通用逻辑构建规则，不记录某个具体 flow 或 testcase 的实现方案。

如果 review 中发现具体性能问题，例如 issue pending 扫描、LSQ commit pending 扫描、主表地址参考选择或 TLB entry 失效扫描，应把对应修改方案写入 `AI_DOC/plan/test_framework/plan/undo` 下的专项设计文档。

规则文档中只保留判断准则：

```text
高频路径避免全表扫描；
构建期重复选择候选时优先维护窗口、pool 或索引；
被动 responder 使用 idle_stop_cycle；
主动发射型 flow 使用公共完成条件；
固定 max_cycles 不作为正常退出条件。
```
