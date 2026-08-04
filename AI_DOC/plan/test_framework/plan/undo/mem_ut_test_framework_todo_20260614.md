# mem_ut 测试框架 TODO

本文记录 mem_ut/MemBlock 测试框架当前明确不支持、简化支持或后续需要补齐的事项。

## 1. Vector LS 支持

状态：当前不支持。

现象：

- `lsq_ctrl_model::derive_op_behavior()` 遇到 vector LS fuType 会 `uvm_fatal`。
- vector IQ feedback 当前在 adapter 中 drop。
- vector writeback 当前在 `writeback_status_handler` 中 fatal。

TODO：

- 补齐 vector load/store 的 LSQ admission、issue、writeback、replay、commit/deq 和状态追踪闭环。
- 支持 `numLsElem > 1` 的 LQ/SQ 范围映射，而不是只保存 base key。
- 支持 vector feedback/event 根据范围内 `lqIdx/sqIdx` 反查 uid。

## 2. Atomic/MOU 完整闭环

状态：当前V2 scalar主动主流程不支持，不是简化支持。V2适配执行方案不新增compile capability；默认AMO权重改为0，显式权重和random/boundary/manual/fixed AMO/MOU由运行期参数及主表语义检测在admission前fatal。只有本节闭环全部完成后，才能删除这些运行期拒绝分支并重新开放AMO激励。

现象：

- 当前 AMO/MOU 不分配普通 LQ/SQ。
- 底层 `derive_op_behavior()` 已具备 route 到 STA/STD 的分类基础，但当前 V2 主动主流程会在 admission 前运行期 fatal，不会实际进入 issue route。
- `uop_count` 会记录 AMOCAS 等理论需要几个地址侧或数据侧 uop。
- issue queue 不会按 `uop_count` 展开多个 item，也没有完整 atomic writeback/AMOCAS 多 uop 闭环。

TODO：

- 明确 AMO/LR/SC/AMOCAS 在 DUT 中是否应分配 LQ/SQ、如何 writeback、如何 commit。
- 如果 AMOCAS 需要多个 STA/STD uop，issue queue 需要按 `uop_count` 展开或引入可追踪的 multi-uop item。
- 补齐 `MEMBLOCK_WB_EVENT_SOURCE_ATOMIC_WB` 的真实事件来源和状态转移。
- 建立AMO/LR/SC从主表生成、LSQ/atomic admission、issue uop展开、真实feedback/writeback、ROB commit、资源释放到terminal的端到端状态生命周期。
- 明确redirect/replay/flush、异常写回和fault retire时atomic pending uop、ROB映射及任何LQ/SQ/atomic私有资源如何失效、重建或释放。
- 为普通LR/SC/AMO W/D和AMOCAS W/D/Q分别建立directed testcase；验收不得只看到issue accepted，必须覆盖真实完成事件、两类异常路径、commit/deq或专用资源释放以及最终terminal收敛。
- 闭环实现完成并通过回归前，不得仅把`MEMBLOCK_OP_CLASS_AMO_WT`改成非0来宣称支持；完成后应删除V2初始化和`validate_main_table_entry()`中的AMO/MOU运行期拒绝，而不是增加compile capability开关。

### 2.1 AMO 非对齐专项 TODO

状态：当前 `main_table_addr_scenario_plan` 第一版只允许 AMO/LR/SC/AMOCAS 使用 `boundary_profile=ALIGNED`。AMO 非对齐没有作为普通 load/store `boundary_profile` 的一部分实现，应单独建立专项。

RTL 行为边界：

- AMO/LR/SC/AMOCAS 由 `AtomicsUnit` 接管，不走普通 load/store 的 split、readWholeBank、SQ cross16 或 UnalignQueue 路径。
- 非自然对齐 AMO 不是测试框架层面的非法激励；如果 DUT 接收该请求并触发 misaligned exception，这仍属于可构造的 DUT 行为刺激。
- 测试框架只有在生成出的 atomic 组合不符合 DUT atomic 源码可产生的入队/发射/多 uop 行为时，才应把它视为非法激励并避免或报错。

TODO：

- 单独建立 AMO 非对齐专项 plan，覆盖 LR/SC/普通 AMO W/D、AMOCAS W/D/Q 的地址自然对齐约束和非自然对齐构造方式。
- 第一阶段可只开放 LR/SC/普通 AMO W/D 的非对齐激励；AMOCAS 非对齐需等待 atomic multi-uop 展开、STA/STD 协同和 atomic writeback 闭环明确后再开放。
- 地址生成侧按 atomic 语义 size 构造非自然对齐 effective address/vaddr；物理地址仍由 L2TLB/物理窗口约束保证落在合法 paddr 范围内。
- 生成后只做激励自洽检查：`fuType/fuOpType`、atomic size、addr alignment、`uop_count`、STA/STD 协同状态和专项标签必须一致。
- 不在该 TODO 中实现 DUT exception checker、RM 对比或功能覆盖率达标；后续应在对应 RM/checker/coveragent 专项中协同支持。

## 3. 多元素 LSQ 范围映射

状态：当前不完整，只保存 base LQ/SQ key。

### 3.1 多元素 LSQ 是否只有向量采用

从当前 Scala 源码看，`numLsElem > 1` 主要是 vector LS 路径使用，标量 load/store 进入 LSQ 时按 1 个元素处理。

依据：

- `Rename.scala` 中，`u.numLsElem` 只有在 `isVlsType && !isfofFixVlUop` 时写入计算出的 `numLsElem`，否则写 0。
- `Dispatch.scala` 中，发给 `enqLsq` 时有明确转换：`enqLsqIO.req(i).bits.numLsElem := Mux(isVlsType(i), numLsElem(i), 1.U)`。注释也说明非 vector 传给 IQ 的 `numLsElem` 是 0，但送 LSQ 时为了计算方便，scalar 和 FLOW=1 的 vector 都按 1。
- `LSQWrapper.scala` 用 `FuType.isLoad/isVNonsegLoad` 和 `FuType.isStore/isVNonsegStore` 判断是否需要 LQ/SQ，并把 `numLsElem` 加入 `loadQueueElem/storeQueueElem`。这说明 LSQ admission 的资源消耗单位就是 `numLsElem`。

结论：

- 标量 load/store：LSQ admission 中 `numLsElem=1`，分配 1 个 LQ 或 SQ entry。
- vector non-segment load/store：LSQ admission 中 `numLsElem` 可能大于 1，分配一段连续 LQ 或 SQ entries。
- AMO/MOU：当前 Dispatch 对 `isAMOVec` 不发 `enqLsq`，测试框架里也简化为 `need_alloc=0`。
- segment/vector FOF fix-up 相关路径在 Dispatch 中有特殊 gating，当前测试框架没有完整支持。

### 3.2 多元素进入 LSQ 时分配几个 idx

如果 `numLsElem=N`，硬件按一条 dispatch/enq 请求分配一个 base idx，同时占用连续 N 个 queue entries。

load 侧依据：

- `VirtualLoadQueue.scala` 中 `enqLowBound = req.bits.lqIdx`，`enqUpBound = req.bits.lqIdx + req.bits.numLsElem`。
- 对每个 LQ entry index `i`，如果 `i` 落在 `[lqIdx, lqIdx + numLsElem)` 范围内，就会被该请求分配。
- `io.enq.resp(i)` 返回的是 base `lqIdx`，后续 vector split 使用 `issueUop.lqIdx + splitIdx` 形成具体子 uop 的 LQ index。

store 侧依据：

- `NewStoreQueue.scala` 同样用 `enqLowBound = sqIdx` 和 `enqUpBound = sqIdx + numLsElem`。
- 对每个 SQ entry index `i`，如果落在 `[sqIdx, sqIdx + numLsElem)` 范围内，就会被该请求分配。
- `VSplit.scala` 中 vector store 使用 `issueUop.sqIdx + splitIdx`，vector load 使用 `issueUop.lqIdx + splitIdx`。

结论：

- 分配 idx 的数量：`numLsElem` 个。
- 返回给上游的 idx：base `lqIdx/sqIdx`。
- 实际占用 queue entry：从 base 开始连续 `numLsElem` 个，跨环形队列时按 ptr flag/value wrap 规则处理。
- 后续 vector split 子请求使用 `base + splitIdx` 定位每个具体元素对应的 LQ/SQ entry。

### 3.3 对测试框架的影响

当前 mem_ut 只保存 base `lqIdx/sqIdx`，没有保存 `[base, base + numLsElem)` 的范围映射。因此如果后续要支持 vector LS，必须补齐：

- `uid -> LQ/SQ range` 状态字段。
- `lqIdx/sqIdx -> uid` 的范围反查，而不是只记录 base key。
- redirect/cancel 时按范围释放或回滚。
- deq/commit 时按范围推进和校验。
- monitor event 如果回的是范围内任意 idx，都能正确匹配到 uid 和子 uop。

## 4. STD Backend Replay

状态：Scala 源码已确认，标量 STD 没有 backend replay feedback 路径；测试框架当前对 STD miss warning/drop 是符合标量 DUT 路径的。

现象：

- STA miss 可通过 store address 路径反馈给后端 IQ，并转 replay。
- STD 只负责 store data 写入 SQ 和标量 STD toRob writeback，不产生 `feedbackSlow`/backend replay。
- `mark_replay_pending()` 对 STD replay 不进入重发流程。

Scala 源码依据：

- `StdExeUnit.scala`：`StdExeUnitIO` 只有 `out.toRob`、`atomicData`、`sqData`，没有 `feedBackSlow` 或 replay 输出；标量 STD 通过 `io.out.toRob.valid` 写 ROB，通过 `io.sqData.valid` 写 SQ data。
- `NewStoreUnit.scala`：store address path 生成 `io.feedBackSlow`，`sourceType := RSFeedbackType.tlbMiss`，用于 TLB miss/translation not ready 后让 RS/IQ replay。
- `MemBlock.scala`：STD 侧连接为 `lsq.io.std.storeDataIn(i) := stdExeUnits(i).io.sqData`；STA 侧连接为 `stu.io.feedBackSlow <> io.mem_to_ooo.staIqFeedback(i).feedbackSlow`。
- `Region.scala`：`staFeedback.feedbackSlow` 驱动 store address IQ 的 `failed/finalSuccess`，没有对应的标量 STD IQ feedback replay 通路。

结论：

- 标量 store 的 backend replay 属于 STA/store address 侧，不属于 STD/store data 侧。
- 当前测试框架不需要支持真实 scalar STD backend replay；STD miss warning/drop 不应作为 TODO blocker。
- 后续仅当 DUT 新增 vector store、atomic/MOU 或其他 STD-like replay feedback 路径时，再补专门的事件来源、重新入队规则和 STA/STD 双队列协同关系。

## 5. FuType/FuOpType 覆盖

状态：当前 V2 主动主流程只支持已闭环的普通 scalar load/store 和现有 software prefetch 简化路径；AMO/MOU、AMOCAS、CBO 只有编码/分类基础，均在 admission 前运行期 fatal，不能列为已支持组合。

现象：

- 非 LDU/STU/MOU 的 `fuType` 没有 fallback，会 fatal。
- LDU/STU/MOU 下非法 `fuOpType` 会 fatal。
- 当前 V2 主表默认随机权重只开放普通标量 load/store 和 software prefetch；AMO 权重改为 0，显式非零权重或手工 AMO/MOU 会在 admission 前 fatal。
- CBO 可以被 `lsq_ctrl_model::is_cbo_fuoptype()` 识别，但当前默认权重为 0；显式非零权重或手工 CBO 会在 admission 前 fatal，不再进入 store-like 主流程。
- AMOCAS 常量和识别逻辑存在，但随机主表默认不会生成 AMOCAS，也没有完整多 uop 闭环。

### 5.1 当前编码/分类基础与本轮支持边界

当前框架围绕 MemBlock LSQ admission、issue、writeback、commit/deq 主流程建模，不覆盖整个后端所有 FU。

当前已闭环支持范围：

- `FuType.ldu`
  - 普通 load：`lb/lh/lw/ld/lbu/lhu/lwu`。
  - software prefetch：`prefetch_i/prefetch_r/prefetch_w`。
- `FuType.stu`
  - 普通 store：`sb/sh/sw/sd`。

仅具备编码/分类基础、当前不支持进入主动主流程的范围：

- `FuType.stu` 下的 `cbo_zero/cbo_clean/cbo_flush/cbo_inval`。
- `FuType.mou` 下的普通 LR/SC/AMO W/D，以及 AMOCAS W/D/Q。
- 上述未支持组合由 V2 运行期参数检查和主表 admission 前语义检查 fail-fast；编码常量或 classifier 存在不代表功能闭环。

### 5.2 Scala 支持但当前未完整支持的组合

#### Vector LS

涉及 `FuType.vldu/vstu/vsegldu/vsegstu`。

Scala 中这些属于 vector load/store 路径，会涉及 `numLsElem > 1`、vector split、LQ/SQ range 映射、vector feedback 和 vector writeback。当前测试框架遇到 vector LS 会 fatal。

当前不支持原因：

- 主表和状态表只保存 base `lqIdx/sqIdx`，没有保存 `[base, base + numLsElem)` 的范围映射。
- monitor event 反查只按单个 LQ/SQ key 建模，不能覆盖 vector 子元素。
- vector IQ feedback/writeback 当前没有完整状态闭环。

#### Hypervisor Load/Store

Scala `LSUOpType` 支持：

- HLV/HLVX load：`hlvb/hlvh/hlvw/hlvd/hlvbu/hlvhu/hlvwu/hlvxhu/hlvxwu`。
- HSV store：`hsvb/hsvh/hsvw/hsvd`。

当前测试框架未支持：

- `lsq_ctrl_model::is_load_fuoptype()` 只认普通 load，不认 HLV/HLVX。
- `lsq_ctrl_model::is_store_fuoptype()` 只认普通 store，不认 HSV。

原因是 HLV/HSV 不只是 size 不同，还会影响 TLB 请求的 hypervisor 相关字段，例如 `hyperinst`、两阶段翻译和异常语义。当前框架的 TLB 表、CSR runtime state、异常判断还没有把这类访问作为独立 flow 建模。

#### AMOCAS

Scala 中 `AMOCAS.W/D/Q` 是合法 MOU op，`AtomicsUnit.scala` 中有明确多 uop 行为：

- 普通 AMO/LR/SC：通常 1 个 STA uop 和 1 个 STD/data uop。
- AMOCAS.W/D：需要更多 STD/data uop。
- AMOCAS.Q：需要 2 个 STA uop 和 4 个 STD/data uop。

当前测试框架状态：

- `memblock_dispatch_types.sv` 已定义 `MEMBLOCK_LSUOP_AMOCAS_W/D/Q`。
- `lsq_ctrl_model::is_amocas_*()` 可识别 AMOCAS，并能给出理论 `atomic_sta_uop_count/atomic_data_uop_count`。
- `random_amo_fuoptype()` 默认不生成 AMOCAS。
- issue queue 当前没有按 `uop_count` 展开多个 item，也没有完整 atomic writeback/AMOCAS 多 uop 闭环。

因此 AMOCAS 目前不能认为已经完整支持。

### 5.3 不属于当前 MemBlock LSQ 主流程的 FuType

例如 `alu/csr/fence/brh/jmp/fp/vector arith` 等非 `LDU/STU/MOU` FuType 当前会 fatal。

这不是当前测试框架的遗漏，而是建模边界：当前框架验证的是 MemBlock 访存路径，不负责完整后端执行单元验证。后续如果要覆盖 fence/sfence 或 CSR 对 MemBlock 的影响，应作为独立 flow 建模，而不是默认塞进 LSQ dispatch 主流程。

TODO：

- 后续新增 FU 或新增 LSU op 时，必须先在 `lsq_ctrl_model::derive_op_behavior()` 和主表模板中补合法组合。
- 不建议默认 fallback 到 load-like/store-like，否则容易把非法激励误当成合法行为。
- 如果要支持 HLV/HSV，需要同步补 TLB/CSR/异常语义，不应只把 op 加到合法列表。
- 如果要支持 AMOCAS，需要补多 STA/STD uop 展开、状态追踪和 atomic writeback 闭环。
- 如果要支持 vector LS，需要先完成多元素 LSQ range 映射。

## 6. CBO/Prefetch 专项语义

状态：Prefetch 当前按 load-like 简化建模；CBO 只有 store-like 分类基础，当前 V2 主动主流程在 admission 前运行期 fatal。

### 6.1 这个功能怎么理解

CBO 和 Prefetch 都是“访存相关操作”，但它们不是普通 load/store。

- 普通 load/store 的核心目标是读写程序语义上的数据，并且通常有明确的 ROB writeback、LSQ commit/deq 和异常行为。
- Prefetch 的核心目标是提前把 cache line 拉近，属于性能提示。它可能访问 TLB/DCache/MissQueue，但一般不应该像普通 load 一样产生 architectural load data writeback。
- CBO 的核心目标是 cache block operation，例如 clean/flush/inval/zero，操作对象是 cache block 状态或 cache line 内容，不是普通 store 写某几个 byte 的数据路径。

因此“CBO/Prefetch 专项语义”指的是：不能只因为它们复用 LDU/STU 管线入口，就完全按普通 load/store 的完成条件、异常条件、commit/deq 条件来验证。后续需要把它们作为专项 flow，明确它们在 DUT 中到底应该如何入队、如何发射、如何完成、是否写回、是否参与普通 commit/deq，以及异常/redirect/replay 如何表现。

现象：

- prefetch 当前复用 LDU/load-like 路径。
- CBO 的底层分类可映射到 STU/store-like 路径，但当前 V2 运行期边界阻止其实际进入该路径。
- 特殊完成语义、异常语义、commit/deq 专项覆盖还不是完整闭环。

### 6.2 Prefetch 当前简化点

Scala 依据：

- `LSUOpType` 定义了 `prefetch_i/prefetch_r/prefetch_w` 和 `isPrefetch()`。
- MemBlock 中存在 software prefetch 到 frontend、load prefetch request、store prefetch request、DCache/MissQueue prefetch source 等路径。
- DCache/MissQueue 中 prefetch 会被作为 prefetch source 处理，并存在 late prefetch ignore、prefetch merge、prefetch entry 限制等行为。

当前测试框架简化：

- 主表中 `MEMBLOCK_OP_CLASS_PREFETCH` 被设置为 `FuType.ldu + MEMBLOCK_LSQ_FLOW_LOAD`。
- `lsq_ctrl_model` 将 prefetch 标记为 `MEMBLOCK_OP_BEHAVIOR_PREFETCH`，但仍沿用 load-like admission/route 框架。
- 当前没有单独区分 prefetch 是否应该产生普通 load writeback、是否应该进入普通 load commit/deq、miss/merge/late prefetch 的专项行为。

V2 源码已经确定的 fault 边界：

- hardware prefetch 由 `LoadUnit/StoreUnit` 的硬件预取输入进入；`LoadUnit.scala:1537` 和
  `StoreUnit.scala:412` 排除其普通 LSQ/writeback valid，因此它没有普通 ROB/LQ/SQ fault uid，
  不进入 scalar fault token/watchdog。
- software prefetch 仍是软件 LDU uop，但 `LoadUnit.scala:1231-1237` 会清除普通访存异常
  （ECC error interrupt/特定 trigger 另按其专用语义处理），所以不能把它当作普通 demand load
  fault 直接送入 scalar fault terminal。
- 因此第 6 节的后续工作重点是分别定义 software/hardware prefetch 的 accepted、ignore、merge、
  writeback/commit/deq 和 monitor 语义，而不是把两者合并成一个 scalar fault flow。

后续需要补：

- Prefetch 发射后完成条件：是否等待 DTLB/DCache 反馈，还是只要 request accepted 就认为完成。
- Prefetch 专项异常边界：hardware prefetch 不建立普通 fault uid；software prefetch 的普通访存
  exception 已由 DUT 清除，剩余 ECC/trigger/平台特定异常是否需要单独观测仍待专项定义。
- Prefetch 与普通 load/store 的 merge 行为：命中已有 MSHR 或被普通 demand request 覆盖时，测试框架如何判定成功。
- software prefetch.i 到 frontend、hardware prefetch train、L2/L3 prefetch sender 的 monitor 和 scoreboard 闭环。
- prefetch 专项 testcase 需要区分“仅提示成功”和“普通 load-like pass”两类完成语义，避免把 prefetch 错当成 demand load 验证。

### 6.3 CBO当前状态与完整闭环TODO

状态：当前V2 scalar主动主流程不支持CBO完整闭环。V2适配执行方案不新增compile capability；CBO默认权重保持0，显式权重和random/boundary/manual/fixed CBO由独立运行期语义检测在admission前fatal。现有源码中的CBO识别和store-like route只说明能够分类，不表示功能已经支持。

Scala/DUT侧已经存在`cbo_zero/clean/flush/inval`编码和cache-line级路径；当前框架只保留“合法op识别+store-like route”的分类基础，并在主表 admission 前拒绝执行。

当前实现现状：

- `memblock_dispatch_types.sv` 已定义 `MEMBLOCK_LSUOP_CBO_ZERO/CLEAN/FLUSH/INVAL`。
- `lsq_ctrl_model::is_cbo_fuoptype()` 能识别 CBO，并在 `derive_op_behavior()` 中归类为 `MEMBLOCK_OP_BEHAVIOR_CBO`。
- 当前随机主表默认不会生成 CBO；显式非零 CBO 权重会在参数校验阶段 fatal，手工/boundary/fixed CBO 会在主表 admission 前 fatal，因此不会继续按 STU/store-like 路径执行。

后续需要补：

- 为 CBO 增加显式 testcase / directed helper，而不是只依赖手工改 `fuOpType`。
- 区分 `cbo_zero` 与 `cbo_clean/flush/inval` 的完成语义、是否需要普通 store data 路径、是否有特殊异常。
- 明确cache-line粒度、uncache/MMIO、fault/redirect/replay下CBO的期望行为，并补对应scoreboard。
- 建立CBO从主表directed生成、地址翻译/权限、admission、STA或专用issue、DCache/CBO ack或其它真实完成事件、ROB commit、SQ或专用资源释放到terminal的端到端生命周期。
- 分开定义CBO.ZERO与CBO.CLEAN/FLUSH/INVAL是否需要STD数据侧、是否进入普通SQ、如何与flushSb/SBuffer/uncache交互，禁止继续用普通store的STA+STD pass作为统一完成条件。
- 明确CBO在redirect/replay/flush及异常路径下的pending状态、ack去重、资源回收和重新发射规则，并建立对应directed testcase。
- 闭环完成并通过回归前，保持V2初始化和`validate_main_table_entry()`中的CBO运行期拒绝；完成后删除拒绝分支并开放专项激励，不增加compile capability开关。

### 6.4 CBO/Prefetch 完成语义完整 checker 化

状态：当前未完成。

这里的“完整 checker 化”不是指主表能生成 `CBO` 或 `PREFETCH`，而是测试框架能够基于 DUT 真实事件判断：

- 这条 CBO/prefetch 什么时候算完成。
- 完成来源是否合法。
- 是否应该写回 ROB/RF、是否应该进入 LQ/SQ commit/deq。
- 异常、replay、redirect、drop/ignore 是否符合该操作本身语义。
- end check 时是否能区分“已合法完成”和“被普通 load/store pass 误判完成”。

Prefetch 需要补的 checker：

- 区分 `prefetch_i`、data prefetch、hardware prefetch train、L2/L3 prefetch sender 等不同来源。
- 明确 prefetch 发射后完成条件：只要 request accepted 即完成，还是必须等 DTLB/DCache/MissQueue 反馈。
- 明确 TLB miss、page fault、PMP/PMA fault、DCache miss、MSHR merge、late prefetch ignore 时分别如何判定 pass/drop/replay/fault。
- 明确 prefetch 是否允许产生普通 load writeback、是否参与普通 load commit/deq；不允许把 prefetch 简单按 demand load pass 处理。
- 增加对应 monitor/scoreboard/end check，确保 prefetch 专项路径不会被普通 load-like 状态更新掩盖。

CBO 需要补的 checker：

- 区分 `cbo_zero` 与 `cbo_clean/cbo_flush/cbo_inval` 的完成来源和数据侧需求。
- 明确是否需要等待 DCache/L2 `CBOAck`、StoreQueue CBO FSM writeback 或其他 CMO 专用完成事件。
- 明确 CBO 是否需要 STD data 侧；如果只需要 dummy data 或不应走普通 store data 写 SQ，需要在 checker 中单独建模。
- 明确 CBO 的 ROB `scommit`、SQ deq、flushSb/sbuffer/uncache 交互何时允许推进。
- 明确 uncache/MMIO、TLB/PMP fault、cache op ack 异常、redirect/replay 下的 pass/fault/recovery 判定。
- 增加 CBO 专项 testcase 和 scoreboard，避免仅用普通 store 的 STA/STD pass 作为 CBO 完成条件。
- 无 redirect 的当前 scalar 主流程不得支持 early CBO fault：CBO entry 的 `wline=1` 使通用
  SBuffer handshake 不置 `completed`，CMO request 又要求 `!hasException`；若后续要覆盖该场景，
  必须复用现有真实 redirect/cancel owner 驱动 DUT redirect 并等待 `sqCancelCnt`，不得软件伪造
  SQ deq/free-count。该项只约束测试框架生命周期，不新增 RM/scoreboard 实现。

## 7. 地址场景分类覆盖

状态：当前只显式支持 `aligned`，还不支持把普通 misalign、跨 16B、跨 4K 作为独立地址分类来生成和统计。

现象：

- `memblock_dispatch_base_sequence::apply_legal_addr_template()` 当前固定把 `src_0` 放到 64B 对齐的地址槽位，`imm=0`，即只生成 legal aligned 地址。
- 现有 plus/cfg 中没有 `aligned/misalign/cross16B/cross4K` 这一类地址场景开关或权重。
- 随机主表虽然能随机 `load/store/prefetch/amo`，但地址维度没有进一步细分成边界场景。

当前支持结论：

- `aligned`：支持。随机主表默认就是这一类。
- `普通 misalign`：不支持独立分类生成。
- `跨 16B`：不支持独立分类生成。
- `跨 4K`：不支持独立分类生成。

原因：

- `apply_legal_addr_template()` 只从 `[aligned_base, upper]` 中按 64B 步长取地址，没有故意制造 `src_0 + imm` 的非自然对齐访问。
- 当前 `random_load_fuoptype()/random_store_fuoptype()` 只决定访问 size/op，不会联动地址模板去命中 16B 或 4K 边界。
- 主表/状态表里也没有专门的“地址场景标签”，因此 testcase 结束后无法按这四类做覆盖归类。

TODO：

- 给主表生成器增加地址场景枚举，例如 `ALIGNED / MISALIGN / CROSS_16B / CROSS_4K`。
- 在 `apply_legal_addr_template()` 之外新增按场景选址的 helper，避免把边界生成逻辑散落到 testcase。
- 增加 plus/cfg 权重或 directed 开关，用于控制各地址场景比例。
- 在主表 transaction 或状态表中记录地址场景标签，便于 end check、统计和覆盖报表。
- `CBO/prefetch` 建议保留在 `op_class` 维度；不要混入普通 load/store 地址边界分类里做等价处理。
- `prefetch_i` 与 data prefetch 的路径差异：`prefetch_i` 可能走到 frontend/ifetch prefetch 相关输出，不应简单等价于 data load。

### 7.1 CBO地址场景与完成语义补充

以下内容补充第6.3节。2026-08-04 已完成 DCache responder 侧 `CBOClean/CBOFlush/CBOInval` 命中
`Probe(toB/toN) -> C response -> CBOAck` 闭环；当前 TODO 仍只讨论 V2 主表/LSQ 主动 CBO、CBO.ZERO、
异常和 commit/deq 的完整指令流。当前源码仍有 store-like 分类，但 V2 适配执行方案会在 admission 前
运行期拒绝 CBO；这些条目不表示当前允许主表驱动 CBO。

Scala依据：

- `LSUOpType` 定义了 `cbo_zero/cbo_clean/cbo_flush/cbo_inval`。
- `isCboAll()` 同时覆盖 `cbo_zero` 和 clean/flush/inval。
- Store address path 中 CBO 会被识别为 cache block operation，mask 可能覆盖整个 cache line。
- DCache/MissQueue 中存在 CBO ack 等专项响应行为。

当前测试框架简化：

- CBO 当前被 `is_cbo_fuoptype()` 识别为 STU/store-like。
- `derive_op_behavior()` 将 CBO 标记为 `MEMBLOCK_OP_BEHAVIOR_CBO`，但仍使用 store admission、STA/STD route、store commit/deq 的简化路径。
- 主表随机生成默认没有 CBO op_class；手动表可以构造，但状态闭环不是专项语义。

后续需要补：

- CBO.ZERO 和 CBO.CLEAN/FLUSH/INVAL 的路径差异：前者更像写 cache line，后者更像 cache maintenance。
- CBO 是否需要 STD 数据侧：普通 store 需要 STA+STD，部分 CBO 可能不应按普通 store data 写 SQ 建模。
- CBO 完成条件：是否需要等待 DCache/CBO ack，是否产生普通 store writeback 或 SQ writeback。
- CBO 异常语义：地址翻译异常、权限异常、cache op ack 异常如何进入 ROB/redirect/replay。
- CBO commit/deq：是否按普通 store 的 SQ commit/deq 推进，还是存在 CMO 专用完成条件。

TODO：

- 对照 Scala 源码补 CBO/prefetch 的完成条件、异常来源、是否写回、是否参与普通 load/store commit/deq。
- 建立专项 testcase，避免只验证普通 load/store 类似路径。
- 给 CBO 和 Prefetch 分别建立独立 op_class 或至少独立 behavior 分支，避免后续误把专项行为混在普通 load/store pass 条件里。

## 8. V2 `tlbCsr_priv_debug` debug-mode 权限/异常建模 TODO

状态：当前只做 snapshot only，不做完整行为建模。

当前本轮 V2 适配策略：

- `csr_ctrl_agent` monitor 已采样 `io_ooo_to_mem_tlbCsr_priv_debug`，并完成 X/Z 检查。
- raw CSR/runtime snapshot 已保存 `priv_debug`，默认值为 `1'b0`，表示当前 smoke/main flow 默认不进入 debug mode。
- 当前 sequence、主表、异常 directed 激励、pass/fault、terminal 和 L2TLB lookup 均不消费 `priv_debug`。
- 当前测试框架不根据 `priv_debug` 生成、禁止或筛选 debug-mode 权限/异常激励。

源码语义边界：

- CSR 侧 `io.tlb.debug := debugMode`，MemBlock 将该字段作为 `tlbCsr.priv.debug` 下发。
- DTLB/L2TLB PMP/PMA 检查会消费 `tlbcsr.priv.debug`，例如 `MemBlock.scala` 和 `L2TLB.scala` 中传入 `PMPCheckerEnv.debug`。
- RTL `PMPChecker` 中 `io_check_env_debug` 参与 PMP/PMA 匹配条件，尤其会影响 debug 地址区间相关判断。

为什么不在本轮实现：

- 完整支持不是单纯字段补齐，而是 debug mode 下 PMP/PMA、debug ROM/debug address、权限异常标签和 directed 激励合法性的专项建模。
- 测试框架本轮目标是生成自洽激励，不承担完整参考模型职责；当前 smoke/main flow 不构造 debug-mode 访存场景，因此不应把 `priv_debug` 混入普通 pass/fault 判断。
- 若只把 `priv_debug` 接入 pass/fault，而没有定义 testcase 入口、地址范围、PMP/PMA 配置和 debug-mode 标签，容易把普通权限场景误判成 debug 权限场景。

后续 TODO：

- 新建 `CSR runtime/priv debug` 专项 plan，明确 debug-mode 访存 testcase 的入口和默认关闭策略。
- 在 raw CSR/runtime snapshot 已保存 `priv_debug` 的基础上，定义哪些 sequence/helper 可以消费该字段。
- 建立 debug-mode 地址场景标签，例如普通地址、debug ROM/debug address、PMP/PMA 受 debug 影响的地址区间。
- 明确 `priv_debug=1` 时哪些访问只作为合法激励生成，不由测试框架判断 DUT 正确性；哪些 directed 标签必须保证和 debug-mode CSR 上下文一致。
- 若需要主动构造 debug-mode PMP/PMA fault/pass 场景，补充 testcase 配置、PMP/PMA 环境准备、主表标签和激励自洽检查。
- 不在该 TODO 中实现 RM/scoreboard 正确性比较；若后续需要检查 DUT 结果正确性，应另建 RM/checker/coverage 专项。

## 9. V2 `sfence_bits_flushPipe` standalone 职责边界（已关闭，非 TODO）

状态：当前 V2 MemBlock standalone 适配已经闭环，不需要增加完整 core pipeline flush 模型。

当前实现与源码边界：

- `fence_agent` 以 soft 默认 `0` 构造该字段，支持 directed 原值驱动、打印、比较和 valid payload X/Z 检查。
- `dispatch_raw_sfence_t` 和 `decode_raw_sfence()` 不消费该字段；软件 TLB entry invalidation 继续只由 `sfence_valid` 和 `rs1/rs2/addr/id/hv/hg` 决定。
- Decode 为 SFENCE/HFENCE 设置 `blockBackward=1` 和 `flushPipe=1`；Fence FU 把同一个 uop 属性分别送入 sfence payload 和写回。
- 完整 core 中只有 ROB 在该指令到队头、可提交且无异常时产生 `RedirectLevel.flushAfter`，再由 CtrlBlock/global redirect 清除年轻指令。
- MemBlock 不根据 `sfence_bits_flushPipe` 本地暂停 LSQ enqueue/issue，也不从该位产生年轻 uid kill、replay、queue 回滚或 terminal 状态。

关闭结论：

- 测试框架不得仅因 `sfence_bits_flushPipe=1` 人工暂停 driver、构造 redirect、清 active mapping 或改变 pass/fail/terminal。
- directed `flushPipe=1` 不要求仅在 quiescent 窗口发送；它在当前 standalone DUT 边界内与 `0` 的差异只保留在接口观测，不形成额外软件行为。
- 如果 DUT 通过已有独立 redirect 接口真实产生 redirect，继续由既有 redirect/replay owner 按真实事件处理，不能把该结果反推为由 fence agent 创建。
- 只有未来验证对象扩展到包含 ROB/CtrlBlock/global redirect 的完整 core 环境时，才应在新的全核集成计划中验证 `flushAfter`；这不是当前 MemBlock V2 测试框架适配项。

## 11. V2 MMIO load/store directed 与 `pendingMMIOld` 建模 TODO

状态：已有专项 execution plan 承接 ROB mmio 状态表标签与 `pendingMMIOld` 支持，路径为 `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_pending_mmio_load_sideband_execution_plan_20260710.md`。该专项已完成并归档；当前 TODO 继续记录完整 MMIO directed、地址属性、store MMIO 后续 RM/checker 边界。DUT `loadMmio/loadMmioUop.robIdx_value` 和 `storeMmio/storeMmioUop.robIdx_value` monitor 回填已落地，不再作为待确认项。

当前本轮 V2 适配策略：

- `pendingPtr` 后续 coding 按 `modeled_rob_deq_ptr` 驱动：普通 commit xaction 发送 commit 前 head 并校验等于 commit batch head uid 的完整 ROB key，普通 commit 完成后推进到 batch 后 next head，idle/default 周期发送推进后的 head。
- `pendingst` 后续 coding 按 head uid 是否为 scalar store 驱动。
- `scommit` 后续 coding 按本拍 commit batch 中 scalar store 数量驱动。
- `pendingMMIOld` 由状态表 MMIO load 标签驱动；该标签默认由 DUT `loadMmio/loadMmioUop.robIdx_value` output monitor 回填，不从 `PBMT/pmaAF/corrupt/denied` 直接推导，因为这些字段不等价于 ROB entry 的 `mmio` bit。
- `loadMmio/loadMmioUop` 进入 raw ctrl 后固定探测 `{flag=0,value}` 和 `{flag=1,value}` 两个完整 active ROB map key，唯一命中并通过生命周期检查后回填 status MMIO load 标签；不得扫描 active window，也不进入 pass/fail、terminal 或 deq 判断。
- `storeMmio/storeMmioUop` 使用同一固定双 key 方法回填 status MMIO store 标签；本轮不把 store tag 作为 pass/fail、terminal、commit 或 pending 公共状态判断条件。

为什么不在本轮实现完整 MMIO directed：

- 当前 `main_control_transaction` 没有明确的 `is_mmio` 或等价 MMIO load/store 标签。
- V2 DUT 的 `pendingMMIOld` 源自 ROB head entry 的 `mmio` bit，完整闭环需要先定义 load/store 如何在主表、TLB/PMA/PBMT、writeback/debug 字段和 ROB sideband 之间传递 MMIO 语义。
- 直接把 `PBMT`、`pmaAF`、`denied/corrupt` 当作 MMIO 会混淆内存属性、access fault 和 response error，可能生成错误激励或错误 sideband。
- `loadMmioUop/storeMmioUop` output 只有 ROB value，无 flag；二者反查 uid 必须固定探测 `{flag=0,value}` 与 `{flag=1,value}` 两个完整 active ROB map key，并由 ROB mmio tag 专项定义 map 插入/删除、采样 flush epoch、redirect/replay/flush 后动态实例失效及无法唯一命中时的 fail-fast 策略；不得退回 active-window 或主表扫描。

后续 TODO：

- 执行 `mem_ut_v2_pending_mmio_load_sideband_execution_plan_20260710.md`，补齐状态表统一 ROB mmio tag、DUT `loadMmio/loadMmioUop` 与 `storeMmio/storeMmioUop` monitor 回填、固定双 key 反查、setter API和 `lsq_commit_handler` head sideband consumer。
- 后续若要构造完整 V2 MMIO directed testcase，再新增 MMIO directed stimulus plan，定义地址属性、TLB/PMA/PBMT/responder 配置和 testcase 入口。
- directed/testcase 显式指定 MMIO 标签只作为后续 debug 或 directed override；默认来源已经固定为 DUT `loadMmio/loadMmioUop` 和 `storeMmio/storeMmioUop` monitor 回填。
- `pendingMMIOld` 统一按当前 modeled ROB head 每拍派生，与 writeback/pass 和普通 commit batch 是否为空解耦。尚未 writeback 的 tagged MMIO load head 自然形成 sideband-only 周期；该周期不得设置 `rob_commit`、推进 commit cursor/modeled head、deq、pass/fail或terminal，不再建立第二套特殊 commit 状态机。
- `loadMmio/loadMmioUop` 与 `storeMmio/storeMmioUop` raw ctrl 字段由 ROB mmio tag 专项定义，必须说明 ROB value-only 反查来源、生命周期和 flush/replay/redirect 后失效规则。
- 定义 MMIO load/store 对 uncache responder、writeback、SQ MMIO FSM、commit/deq、terminal 收敛的影响。
- 默认关闭 directed MMIO；未开启专项时，不把 MMIO sideband 接入 pass/fail 或 terminal。
- 不在该 TODO 中实现 RM/scoreboard 正确性比较；若后续要判断 DUT MMIO 顺序或异常处理正确性，应另建 RM/checker/coverage 专项。

## 12. LSQ issue hold、压力模式与 boundary vseq TODO

状态：当前 V2 LSQ enqueue 适配只补齐 scalar request 字段、6 个物理 slot、单拍最多
6 个 load element/4 个 store element 的 admission gate、V2 E0/E1 发送时序和 redirect 后重试。
当前不新增 issue hold、LSQ 压力模式或 boundary directed vseq。该边界由
`AI_DOC/plan/test_framework/plan/do/mem_ut_v2_lsq_enqueue_framework_adapt_final_plan_20260714.md`
定义。

本节中的“边界”是 LSQ 容量、admission 门限和 enqueue/redirect 时序边界，不是第 7 节的
misalign、跨 16B 或跨 4K 地址边界。以下三项都是后续测试场景基础设施，不是 V2 DUT
interface 字段，也不表示当前普通 LSQ enqueue/issue flow 不可用。

当前行为边界：

- LOAD/STA/STD issue sequence 继续按现有 scheduler、DUT ready 和真实 fire 运行，不存在测试专用
  `force_idle` 或 `issue_start_hold` 分支。
- 正常 enqueue 可以在 DUT backpressure、issue 延迟或其它真实运行条件下自然形成 LQ/SQ 压力；
  当前只是不能稳定、定向地把占用推进到指定门限。
- V2 admission 必须始终保持
  `load_free >= tentative_load_elements + 6` 和
  `store_free >= tentative_store_elements + 4`。任何压力或边界场景都不得绕过该 gate、伪造
  enqueue acceptance 或直接修改软件 LQ/SQ 分配状态。
- 当前 pass/fail、ROB commit、LSQ deq 和 terminal owner 保持不变；三项专项后续实现也只能控制
  场景调度，不得建立第二套完成状态机。

### 12.1 Issue hold TODO

Issue hold 是测试框架主动暂停 LOAD/STA/STD issue 发送、让已 enqueue 项暂时留在 LQ/SQ 中的
测试控制。它不是 DUT backpressure，不是 issue interface 字段，也不得复用
`sfence_bits_flushPipe` 表达。

后续 TODO：

- 建立独立专项 plan，定义 issue hold 的唯一运行期 owner、置位点、释放条件以及 reset、redirect、
  replay、global stop 下的清理规则；不得由多个 sequence 或 testcase 直接写同一状态。
- issue 主循环只允许 O(1) 读取 hold 状态。hold 生效时继续执行现有 uid route 和
  `advance_issue_queue_delays()`，只抑制新 issue transaction 的发送并保持 interface idle；不得从
  issue queue 删除 item、标记 fire 或伪造 DUT ready。这样 hold 只增加队列驻留时间，不会在释放后
  再额外重复 transaction 原有的 `ready_cycle` 延迟。
- hold 期间 monitor event、redirect/replay handler 和已有状态清理必须继续运行；hold 不能冻结整个
  dispatch service loop。
- intentional hold 期间不得把“没有 issue fire”误报为 DUT no-progress。释放后恢复现有 watchdog，
  但 hold 周期本身也不得计作真实 progress。
- 所有控制参数默认关闭。若作为公共可配置行为，参数必须走
  `env/plus.sv -> seq_csr_common.sv -> getter`；动态 active 状态属于运行期状态，不得存入 plus、
  env cfg 或 transaction 字段。
- hold 释放后继续使用原 issue scheduler 和 fire 处理，最终仍由既有 terminal/global-stop 合同退出。

### 12.2 LSQ 压力模式 TODO

压力模式用于稳定制造 LQ/SQ 高占用，而不是提高硬件结构上限或强制 DUT 接收非法 batch。最小方案
应通过“继续合法 enqueue + 暂停 issue”接近 admission 门限，避免同时修改 commit/deq 主逻辑。

后续 TODO：

- 定义默认关闭的 pressure profile，至少包含 load/store 目标、目标 occupancy 或 free-entry 门限、
  最大场景准备等待时间和 hold 释放策略；公共参数按参数管理规则统一进入公共 runtime 参数链。
- 场景构建使用普通 scalar load/store，保持每条 scalar transaction 的 `numLsElem=1`；不得为了提高
  占用引入当前未支持的 vector、AMO、MOU 或 CBO。
- 压力 controller 只根据参数化 LQ/SQ counter、当前 tentative batch 和 O(1) hold 状态判断是否达到
  目标，不得每拍扫描完整主表或状态表。
- 达到目标后必须释放 issue hold，让原 issue、writeback、commit/deq 和 terminal flow 自然排空；
  不新增 commit hold 或 deq hold 作为默认实现。
- 场景准备阶段超过明确上限仍未达到 directed 目标时必须报告场景构造失败，不能静默退化成普通
  smoke。该上限只约束 directed setup，不得替代主动 flow 的正常 terminal 退出条件。
- 可记录目标 occupancy、最大 occupancy、gate 阻塞次数和 hold 周期等激励有效性统计；这些是 debug
  统计，不是 DUT 正确性 checker 或功能覆盖率达标条件。

### 12.3 Boundary directed vseq TODO

Boundary vseq 是协调主表生成、LSQ enqueue、issue hold、redirect 和必要 responder sequence 的顶层
定向场景。它用于稳定命中容量门限和 E0/E1 时序边界，不直接驱动 DUT interface，也不替代各
agent 的 base sequence/driver。

后续 TODO：

- 新增继承 `virtual_base_sequence` 的 LSQ boundary vseq，通过 `basicTest` 和
  `ts=<boundary_vseq>` 选择，并只使用 `p_sequencer.<agent>_sqr` 启动所需 child sequence。
- directed case 至少覆盖：单拍恰好 6 个 scalar load element、超过 6 个 load candidate 时跨拍保留、
  单拍恰好 4 个 scalar store element、超过 4 个 store candidate 时跨拍保留。
- 覆盖 load/store admission 的门限两侧：free entry 恰好满足
  `tentative + 6/4 reserve` 时允许构造 batch，少 1 时不得把该 candidate 放入本拍 request。
- 覆盖 redirect 位于 launch 前、E0 drive 后到 E1 sample 前以及 E1 sample 边界的场景，沿用 V2
  enqueue plan 已定义的 abort/retry/epoch 语义，不新增另一套 redirect handler。
- vseq 需要积累占用时只能使用第 13.1 节定义的 issue hold API；不得层次化 force interface、直接
  改 issue queue、LQ/SQ counter 或软件 allocation map。
- 每个 directed case 结束后释放 hold 并等待既有 terminal/global-stop 收敛；early return、reset 或
  fatal 前必须清理场景 active 状态，不能遗留到下一 case。
- 后续若需要 RM/checker/coverage，只消费该 vseq 产生的 case 标签、实际 batch 数量、free-entry
  snapshot 和 redirect phase 标签；本 TODO 不实现 DUT 正确性比较或 covergroup。

### 12.4 后续专项完成边界

只有 issue hold、pressure controller 和 boundary vseq 的 owner、参数、运行期状态、清理与退出合同
形成独立可 coding plan 并通过 review 后，才能从 V2 LSQ enqueue 适配 plan 的“不支持”列表中移除。
专项默认关闭时，当前普通 scalar enqueue、issue、writeback、commit/deq、redirect/replay 和 terminal
行为必须保持完全不变。

## 13. V2 L2TLB S1/S2 PTE 权限独立建模 TODO

状态：当前只完成 L2TLB response 权限字段链适配，尚未独立建模 S1/S2 两阶段 PTE 权限。

当前能力边界：

- `memblock_tlb_entry` 只有一套 `pte_d/a/g/u/x/w/r` 等 PTE 属性。
- `memblock_l2tlb_base_sequence::fill_dtlb_resp_from_entry()` 使用同一套 `entry.pte_*` 同时填充
  `s1_entry_perm_*` 和 `s2_entry_perm_*`。
- V2 active takeover 路径中的 `s2_entry_perm_g/u` 已由 `entry.pte_g/pte_u` 驱动，不是常量 0；
  interface、xaction、driver 和 connect 字段链是否完整仍由
  `mem_ut_v2_l2tlb_response_permission_adapt_execution_plan_20260708.md` 负责核对。
- 当前共享字段模型可以发送基础 S1/S2 response，但不能构造“S1 允许而 S2 拒绝”或“S1/S2 的
  `G/U/A/D/R/W/X` 属性不同”等两阶段定向场景，也不能据此宣称已完成独立 stage2 权限建模。
- 当前 `s2_gpf` 继续来自 `entry.tlbGPF`，`s2_gaf` 继续保持 0；这只表示当前能力边界，不代表
  GPF/GAF 和 stage2 legal leaf 已完整派生。

后续 TODO：

- 新建 S1/S2 PTE 权限独立建模专项 plan；保持现有
  `DTLB -> L2TLB_agent request`、`L2TLB_agent -> DTLB response` responder 方向，不得改接顶层
  `io_l2_tlb_req_*` 或改造成 L2Cache/PTW/memory 下游模型。
- 将 `memblock_tlb_entry` 扩展为两套明确的 S1/S2 PTE 属性，例如 `s1_pte_*` 与 `s2_pte_*`，并定义
  初始化默认值、随机约束、合法化规则、复制/compare/print 和 reset 生命周期。
- 更新 TLB entry builder、`uid` 记录和 debug dump，使其保存实际用于 response 的两阶段权限，
  避免调试信息继续只显示一套共享 `pte_*`。
- 更新 `fill_dtlb_resp_from_entry()`：`s1_entry_perm_*` 只读取 S1 PTE，`s2_entry_perm_*` 只读取
  S2 PTE；禁止通过发送前临时改写共享字段来伪造阶段差异。
- 分别定义 S1/S2 的 legal leaf、`R/W/X/U/G/A/D/V/N/PBMT` 约束，以及 PF、AF、GPF、GAF 的
  来源和优先级；在这些规则确定前不得用局部 fixup 清除 `tlbGPF` 或强制生成“合法”stage2 entry。
- 复查 sfence/hfence entry match 与 global-entry 判断，使每类失效操作读取语义对应阶段的 `G`
  等属性；同步确认 lookup key 只使用 `vpn/s2xlate/asid/vmid`，`csr_update_seq` 仅用于 runtime
  语义变化追踪，不属于 lookup key。
- 增加 directed testcase，至少覆盖 S1/S2 权限相同、仅 S1 拒绝、仅 S2 拒绝、S1/S2 `G/U`
  不同以及 GPF/GAF 场景；response 字段和状态记录必须能区分 fault 来自哪个阶段。

完成边界：

- 当前 V2 `s2_entry_perm_g/u` 字段链适配不依赖本 TODO，字段链核对通过后可独立完成和归档。
- 本 TODO 只扩展 responder 生成的两阶段权限语义和对应状态元数据，不建立第二套 TLB lookup、
  LSQ、pass/fail 或 terminal 主流程。
- 若后续需要判断 DUT 两阶段翻译结果是否正确，应另由 RM/checker/coverage 专项消费 S1/S2 状态；
  不能把 responder 能生成独立权限等同于参考模型检查闭环完成。

## 14. Fault 指令支持边界与 standalone 资源释放 TODO

### 14.1 专有名词与抽象功能说明

本节中的专有名词先统一定义，避免把 ROB 语义和 MemBlock 本地资源语义混在一起：

- **fault token**：测试框架为当前 ROB head 的异常实例建立的等待记录。它表示该 uid 已进入
  fault convergence（异常收敛）路径，不表示一次 normal ROB commit，也不表示 LQ/SQ 已经释放。
- **real deq**：由 DUT `io_mem_to_ooo_lqDeq/sqDeq` 输出、被 ctrl monitor 采样并进入 raw queue 的
  LQ/SQ 物理出队事件。只有该事件才能推进软件 LQ/SQ pointer、free count 和 active mapping。
- **cancel**：redirect 后 DUT 输出的 `lqCancelCnt/sqCancelCnt`，表示被 redirect 清掉的队列项数量。
  cancel 与 deq 是两类不同事件，observed cancel 不能再次调用软件 `cancel_lq/cancel_sq()`。
- **natural drain**：DUT 已经把 fault entry 标记为可完成，随后通过真实 writeback/handshake 产生
  `lqDeq` 或 `sqDeq` 的本地资源释放路径；它不是软件表直接删除 entry。
- **early fault**：请求尚未发出就因异常被 DUT 的发送 gate 阻止的场景。该场景可能没有 natural
  drain，只能依赖 ROB exception redirect/cancel 或保持不支持。
- **fault-head drain watchdog**：只负责防止 fault token 无限等待真实 deq/cancel 的超时保护。它
  记录 uid、异常向量、ROB/LQ/SQ key 和最近 raw，不生成任何 DUT 事件。

抽象功能描述：`fault-head drain watchdog` 由 LSQ commit/deq owner 在 fault token 建立后维护；它
 观察 monitor raw 的 deq/cancel 和 active mapping 是否收敛，收敛后交给既有 retire helper，超时则
  `uvm_fatal`。`apply_raw_ctrl_deq()` 的职责仍只是消费已经观察到的 DUT raw deq，并调用现有
  pointer/map release helper；它不能被 watchdog 或 fault sequence 当作预测释放 API。

### 14.2 V2 源码确认的 fault 释放分类

V2 源码的关键事实如下：

- `src/main/scala/xiangshan/mem/lsqueue/VirtualLoadQueue.scala:140-156` 只在 entry 已
  `committed` 且位于连续队头时产生 `lqDeq`；scalar entry 由真实 LDU writeback 的
  `updateAddrValid` 路径置 `committed`，vector entry 才由 vector feedback/FLUSH 路径置位。
- `src/main/scala/xiangshan/mem/lsqueue/StoreQueue.scala:335-347` 的物理 `sqDeq` 条件是连续
  `allocated && completed`，不是 `scommit`；`StoreQueue.scala:1132-1164` 负责 ROB head 的
  `committed` 标记，二者可以跨拍。
- `src/main/scala/xiangshan/mem/lsqueue/StoreQueue.scala:1158-1160` 对已 commit 的 NC 异常
  entry 可直接置 `completed`；未 commit 的 fault store 则由 redirect 的 `needCancel` 路径释放，
  `StoreQueue.scala:1493-1524` 在延迟后输出 `sqCancelCnt`。
- `src/main/scala/xiangshan/mem/lsqueue/LoadQueueUncache.scala:122-143` 表明 MMIO load 只有
  `pendingMMIOld && robIdx == pendingPtr` 才能发请求；NC load 不依赖该 sideband。无前置异常的
  NC/MMIO load 才会额外进入 uncache entry；请求前已有 exception 的 load 不进入 uncache request。
  scalar VLQ 的 `committed` 条件见 `VirtualLoadQueue.scala:249-264`，其中要求真实 LDU event、
  `!need_rep && updateAddrValid && !isvec`，不能用 vector FLUSH 术语替代。
- `src/main/scala/xiangshan/mem/pipeline/AtomicsUnit.scala:68-89,398-425` 的 AMO/MOU 完成由
  `AtomicsUnit.io.out.fire` 驱动，不使用普通 LQ/SQ deq。
- `src/main/scala/xiangshan/mem/vector/VSegmentUnit.scala:240-360,903-932` 的 segment LS 有
  独立 finish/writeback 状态机，不等同于普通 LSQ entry。

据此，当前测试框架的 fault 支持边界如下：

| 指令/路径 | 普通 LQ/SQ 资源 | fault 时可用的 DUT 释放来源 | 当前边界 |
|---|---|---|---|
| scalar cacheable load | 始终有 VLQ/LQ entry | 真实 LDU fault writeback 且 `updateAddrValid=1` 后的 `lqDeq`；scalar 不依赖 vector FLUSH feedback | 计划支持等待真实 raw；当前真实 fault smoke/watchdog 尚未完成，不能软件伪造 deq |
| scalar cacheable/NC store | SQ | 已进入 `hasException` 清理路径时的真实 `sqDeq`；若 redirect 先到则是 `sqCancelCnt` | 计划支持 normal scalar fault drain；当前真实 fault smoke/watchdog 未完成，deq/cancel 由 monitor 真值决定 |
| scalar NC/MMIO load（请求/响应路径） | 始终有 VLQ/LQ entry；无前置异常时另分配 `LoadQueueUncache` entry | NC response/writeback，或 MMIO 的 `pendingMMIOld+pendingPtr` 放行后 response/writeback，再由 VLQ `lqDeq` | 计划支持真实完成路径；MMIO 属性/错误 response 注入仍受第 10、11 节边界约束，当前 fault smoke 未完成 |
| scalar NC/MMIO load（前置异常） | 仍有 VLQ/LQ entry，但不分配 `LoadQueueUncache` entry | 真实 scalar LDU fault event 的 `updateAddrValid` 使 VLQ entry 可提交，随后真实 `lqDeq`；测试框架预填而未形成 DUT event 时没有可等待 deq | 必须与 synthetic `exceptionVec` 路径分开，不能假设 uncache response 或伪造 `lqDeq` |
| scalar MMIO store | SQ | request/response 后 `mmioStout.fire -> completed -> sqDeq`；请求前普通 scalar fault 可走 exception drain | 计划支持且不把 `scommit` 当作 deq 门槛；CBO early fault 另行处理，当前真实 fault smoke 未完成 |
| hardware prefetch | 不形成普通 ROB/LQ/SQ fault uid；由 prefetch source 直接进入硬件预取路径 | prefetch request/ignore/merge 反馈；不产生普通 LQ fault drain | 不进入 scalar fault token/watchdog，也不作为普通 load writeback/terminal |
| software prefetch | 可能复用软件 LDU uop/load-like入口，但不是 demand load fault语义 | V2 `LoadUnit` 清除普通访存异常（ECC error interrupt 另行处理），完成/忽略由 prefetch 专项语义决定 | 当前只按第 6 节简化建模；不进入 scalar fault token/watchdog，不把无 UID response 当作 load fault terminal |
| 普通 vector load/store | LQ/SQ 可能占用多 entry | vector merge/feedback、`vecMbCommit`、真实 deq 或 redirect cancel | 当前不支持，见第 1、3 节和本节 15.3 |
| segment vector LS | 不按普通 scalar LQ/SQ 闭环 | `VSegmentUnit` 独立 finish/writeback/exception path | 当前不支持，不能套用 scalar watchdog 完成 |
| AMO/LR/SC/AMOCAS（`FuType.mou`/MOU route） | AtomicsUnit 私有状态，不是普通 LQ/SQ | atomic 多 uop 的真实 `AtomicsUnit.io.out.fire` | 当前不支持，不能等待 LQ/SQ deq |
| HLV/HLVX/HSV | 可能复用 LSQ，但需要 hypervisor 语义 | 依赖两阶段翻译、权限和异常上下文 | 当前不支持，不能只扩普通 load/store 枚举 |
| CBO，尤其请求前 early fault | SQ/CMO 专用状态 | 未提交 entry 需要 ROB exception redirect -> `sqCancelCnt`；`CBO.ZERO/wline=1` 不保证普通 completed | standalone 当前不支持，必须 fail-fast，不能伪造 `sqDeq` |

这里的“计划支持 scalar fault”仅表示测试框架最终应能等待和消费 DUT 已产生的真实释放事件，
不表示当前代码已经完成真实 fault smoke 或 watchdog，也不表示测试框架承担 ROB 全核异常重定向
或 RM 正确性判断。完整 Core 中架构 fault 仍由 ROB exception redirect 清理 faulting ROB entry
和年轻指令；standalone 只在本地 LSQ 能自然 drain 时省略这部分全核建模。

scalar store 的 `hasException=1` 不是单独的出队条件，必须保留以下前置关系：

- cacheable fault：`pendingPtr` 覆盖 ROB key 后 entry 才能变成 `committed`，随后经过
  DataBuffer/SBuffer 的异常空写 handshake 置 `completed`，再产生 `sqDeq`。
- NC fault：`pendingPtr` 形成 `isCommit` 后，`isCommit && nc && hasException` 才直接置
  `completed`；不需要伪造 `scommit`。
- 请求前的普通 scalar MMIO fault：`StoreUnit` 在 exception 时清除 MMIO 属性，转入通用异常
  drain；response 后才发现的 MMIO fault 则仍需先满足 `pendingst + pendingPtr` 发出请求，随后
  `mmioStout.fire` 才置 `completed`。

对应源码为 `src/main/scala/xiangshan/mem/lsqueue/StoreQueue.scala:1132-1164`、
`StoreQueue.scala:1158-1160` 和 `src/main/scala/xiangshan/mem/pipeline/StoreUnit.scala:532-544`。

还必须区分 fault 来源：

- 已分配到 LQ/SQ 后由 STA/LDA、TLB/PMP/PMA、misalign 或真实 responder 结果产生的异常，只有在
  DUT 真正给出 writeback、deq 或 cancel 时，才能进入本节的 scalar fault drain。
- enqueue 前由测试框架直接写入 `exceptionVec` 的 directed 值可能使 DUT 不分配 uncache entry，或
  在进入 LSQ 前被过滤；它只能作为“输入异常激励”记录，不能预先假定会出现 `lqDeq/sqDeq`。
  若 testcase 必须验证该路径，应新建对应 directed fault plan；当前 software-only synthetic event
  不能充当 DUT 释放证据。

### 14.3 不支持指令的后续修改方案

以下条目是后续专项的可执行落点；在专项完成前，admission 必须保持现有运行期 `uvm_fatal`，
不能因为 enum/classifier 已存在就把指令静默转成 scalar load/store。

1. **Vector LS（`vldu/vstu/vsegldu/vsegstu`）**
   - 扩展主表/状态表保存 `[base, base + numLsElem)` 的 LQ/SQ range、每个子元素的 owner 和
     `uopIdx`，并让 monitor event 能按范围反查 uid。
   - 普通 vector load 补 merge buffer/FLUSH feedback 到真实 `lqDeq` 的链路；普通 vector store
     补 `vecMbCommit`、异常 drain、真实 `sqDeq/sqCancelCnt` 链路。
   - segment LS 单独建立 `VSegmentUnit` finish/writeback owner，不复用 scalar LQ/SQ terminal
     条件；FOF 首元素 fault 保留 ROB 可见异常并通过 vector feedback/异常路径收敛，非首元素
     fault 不形成普通 ROB fault而只缩短 `vl`；FOF fix-VL/last-uop 由 `VfofBuffer` 或
     `VSegmentUnit` writeback，不能假设存在普通 LSQ entry。
   - segment FOF 只由 `VSegmentUnit` 私有状态管理；不得把 segment FOF 的异常、writeback 或
     terminal 事件映射成 scalar LQ/SQ fault。
   - redirect、replay、flush 时按 range 和动态实例释放/重建全部 mapping；专项完成前保持 vector
     admission fatal。

2. **AMO/LR/SC/AMOCAS（`FuType.mou`/MOU route）**
   - 建立独立 atomic issue route 和 `uop_count` 展开：普通 AMO/LR/SC 为 1 STA + 1 data uop，
     AMOCAS.W/D 为 1 STA + 2 data uop，AMOCAS.Q 为 2 STA + 4 data uop。
   - 增加 atomic issue、DTLB/DCache、真实 `AtomicsUnit.io.out.fire`、异常/redirect、重复
     writeback 和最终专用资源释放的状态 owner；不得用 LQ/SQ `lqDeq/sqDeq` 代替 atomic 完成。
   - 每个 atomic 子 uop 必须有独立 correlation key；AMOCAS.Q 只有所有必需 writeback 都完成后
     才能形成 instruction terminal。专项完成前维持 AMO/MOU admission fatal。

3. **HLV/HLVX/HSV**
   - 增加 hypervisor CSR/runtime snapshot、`hyperinst`、两阶段翻译和异常字段的链路；分别定义
     HLV/HLVX 与 HSV 的 legal issue/response 组合。
   - 更新合法性检查、主表标签和 fault/redirect 生命周期；禁止仅把 fuOpType 加入普通
     `is_load_fuoptype()/is_store_fuoptype()`。
   - 专项完成前保持 admission fatal，并在 TODO 中保留“非支持组合不得静默 scalar 化”的约束。

4. **CBO 与 early CBO fault**
   - 先建立 CBO.ZERO 与 CLEAN/FLUSH/INVAL 的独立完成语义，明确 CMO ack、flushSb、uncache、
     ROB commit 和 SQ/专用资源释放的关系。
   - early CBO fault（尤其 `CBO.ZERO` 的 `wline` 路径）必须复用真实 redirect/cancel owner：观察
     redirect sample/anchor 后等待 `sqCancelCnt`，再清理软件 mapping；不得调用 `apply_dut_sq_deq()`、伪造 `scommit` 或直接
     修改 SQ free count。
   - 在 redirect/cancel owner、responder 和状态表闭环前，任何请求前 CBO exception 都在 admission
     或 fault-head watchdog 处 `uvm_fatal`；不能让它进入一个永远等待 `sqDeq` 的 active 状态。
     未来若支持该路径，必须在现有 redirect owner 增加“fault anchor cancel”模式：fault anchor
     保留 fault 结果并等待 cancel 收敛，不重新 admission；只有年轻 uid 沿用既有 reissue 逻辑。

5. **software-only synthetic fault**
   - 现有 `soft_test_memblock_dispatch_fault_smoke_sequence.sv` 只能作为 ledger/token 单元测试，
     必须在 testcase、plan 和 review 中明确标注 software-only；其中直接调用
     `apply_dut_lq_deq()/apply_dut_sq_deq()` 不得作为真实 DUT fault 支持证据。
   - 若需要真实 fault smoke，新增/改造独立 sequence：只提交 fault writeback 或真实 fault input，
     等 ctrl monitor raw `lqDeq/sqDeq` 或 redirect/cancel snapshot `lqCancelCnt/sqCancelCnt`，不在 sequence
     中直接释放公共 mapping。

### 14.4 普通 scalar fault 后续修改方案（待 coding）

1. **真实 raw 是唯一释放入口**

   `io_mem_to_ooo_ctrl_agent_agent_monitor` 采样的 raw 经过
   `memblock_sync_pkg` FIFO、redirect-first/deferred 规则和 `dispatch_monitor_event_adapter` 后，
   才调用 `lsq_commit_handler::apply_raw_ctrl_deq()`。该函数只消费 count/pointer 已真实出现的
   DUT event；不能由 fault sequence、`scommit`、`status.rob_commit` 或 watchdog 代替。

2. **fault token 与 normal commit 分流**

   - normal commit batch 只包含连续、已正常 writeback/pass 的 uid；`scommit` 只统计本批 scalar
     store 数量。
   - fault uid 不进入 normal `commit_uids`，可由 `mark_fault_rob_commit_uid()` 建立 fault token，
     但 token 不推进 SQ/LQ pointer，也不代表 deq 已发生。
   - 没有 redirect 命中的 natural-drain fault 中，fault token 与 real deq 允许任意先后；只有
     mapping 已释放且 fault terminal 条件满足时，`try_retire_committed_uid()` 才形成
     `success=0/terminal_done=1`，随后再推进 modeled head。
   - 若 redirect/cancel 命中 fault uid，当前 `prepare_uid_for_redirect_reissue()` 会清除 fault、
     `rob_commit` 并增加 `dynamic_epoch`，因此不能直接形成 fault terminal；应沿现有 reissue/
     cancel 生命周期等待新实例。只有未来启用 15.3 定义的 fault-anchor 模式时，才允许保留旧
     fault 结果并在 cancel 收敛后形成非成功 terminal。
   - `pendingPtr` 可以继续发布当前 modeled head；不得因为 fault token 缺少 `scommit` 就伪造提交或
     释放资源。

3. **增加 fault-head drain watchdog**

   抽象功能描述：watchdog 绑定 fault uid 的动态实例，逐拍读取 raw deq/cancel、active mapping、
   fault token 和最近 activity；在合法释放事件到达后停止计时，若超过既有 no-progress/watchdog
     阈值仍无任何真实释放来源，则打印完整上下文并 `uvm_fatal`。

   文字伪代码：

   ```text
   fault writeback/raw fault 到达
     -> mark_target_fault(uid, target)
   -> 当前 uid 成为 modeled ROB head 后建立 fault token
   -> 每个 service cycle 先消费 deferred/raw ctrl 的真实 lqDeq/sqDeq
   -> 若有 redirect/cancel snapshot，先按 redirect epoch 对账，再消费真实 lqCancelCnt/sqCancelCnt；observed count 不重复回退
   -> 若 cancel 命中 fault uid：
        若 fault-anchor 模式已启用 -> 保留 fault 结果，等待 cancel 收敛后进入非成功 terminal
        否则 -> 按现有 redirect owner 清旧实例、失效 fault token，等待同 uid reissue；不得 terminal
   -> 若是 natural-drain 且 mapping 全部释放、token/动态实例仍匹配
        -> try_retire_committed_uid(uid, success=0)
        -> terminal_done，推进 cursor
     否则
        -> 更新 watchdog 最近 raw/activity
        -> 超时 uvm_fatal(uid、fault type、ROB/LQ/SQ key、sideband、raw snapshot)
   ```

   watchdog 不创建 xaction、不调用 `apply_dut_lq_deq()`/`apply_dut_sq_deq()`，也不把“没有 raw”
   当作零计数释放。对请求前 CBO fault，超时是预期的 unsupported 诊断；对 scalar load/store，
   超时表示 responder、DUT handshake、redirect 或 monitor 链路存在问题。

   cancel snapshot 只由既有 cancel/reconcile owner 消费：软件 cancel count 负责一次性恢复
   free count，DUT observed count 只做同一 redirect epoch 的核对；两者不能由 deq handler 重复应用。

   为避免 raw 被消费后丢失来源，ctrl monitor 必须在每个 post-reset sample（包括 count 为 0 的
   idle raw）冻结 `ctrl_sample_seq`，并将该字段随 `dispatch_raw_ctrl_t -> deferred raw -> adapter`
   原样传递；不能只在 MMIO valid 时生成 sample 序号。`status_transaction` 和 fault/cancel record
   必须增加最小 release provenance：
   `release_kind`（real LQ/SQ deq 或 real LQ/SQ cancel）、`release_sample_seq`（deq raw 的采样序号）
   和 `release_cancel_epoch`（cancel snapshot 对应 redirect epoch）。ctrl raw consumer 只写
   `release_kind/release_sample_seq` 的 real-deq 分支（来源为 raw 的 `ctrl_sample_seq`），cancel/reconcile owner 只写 cancel 分支；
   `lsq_commit_handler` 的 fault token 保存当前动态实例的值。redirect 清理旧实例时先把旧 provenance
   归档到 fault/cancel record，再递增 `dynamic_epoch`；新实例必须从空 provenance 开始，不能复用旧
   sample/epoch。watchdog 读取这些持久状态，而不是逐拍轮询已经消费掉的 raw queue。

   `ctrl_sample_seq` 的 coding 链必须完整覆盖：`dispatch_raw_ctrl_t` typedef、
   `make_empty_raw_ctrl()` 清零、ctrl monitor 每拍赋值、deferred/raw adapter copy、compare/print
   和 package reset；不得只在 monitor 的临时变量中保存。建议在 `memblock_dispatch_types.sv` 定义
   `memblock_fault_release_kind_e`（`NONE/REAL_LQ_DEQ/REAL_SQ_DEQ/REAL_LQ_CANCEL/REAL_SQ_CANCEL`），
   在 `status_transaction.reset()` 和每次新 `dynamic_epoch` admission 时清为 `NONE`，由 raw deq
   consumer 或 cancel/reconcile owner 作为唯一写者更新。

   redirect/cancel 的旧实例归档必须由 redirect owner 在清理 status 前完成，最小记录为
   `{uid, old_dynamic_epoch, redirect_epoch, resource_kind, rob_key, lq_key, sq_key}`。文字伪代码：

   ```text
   redirect scan 命中 active uid
     -> 读取并保存 old_dynamic_epoch 与仍存在的 LQ/SQ resource
     -> push fault/cancel archive(uid, old_dynamic_epoch, redirect_epoch, resource)
     -> 既有 owner 清旧 status/map，dynamic_epoch++，准备同 uid reissue
   cancel snapshot 到达
     -> 先按 snapshot.sample_seq/redirect_epoch 匹配父 cancel record，并比较聚合 LQ/SQ count
        与该 record 在 redirect scan 阶段按 resource_kind 累计的软件 count
     -> 聚合 count 对账成功后，由父 cancel record 标记其关联的旧实例 archive 集合
        release_kind=REAL_*_CANCEL、release_cancel_epoch 和 observed count
     -> 不把 DUT 聚合 count 假装成 per-UID key，也不回写已经重建的新 dynamic instance 的 status_transaction
   watchdog/token 查询
     -> 只接受 uid + 当前 dynamic_epoch 的 provenance
     -> 旧 archive 未对账时保持 pending，不能被新实例 terminal 消费
   ```

4. **scalar fault 的最终检查**

   - 检查 fault UID 的 `fault/exception_pending`、真实 target fault、deq/cancel 来源和
     `terminal_done`；不把 `status.rob_commit` 单独解释成 normal commit。
   - 检查 active ROB/LQ/SQ map、软件 free count 与 DUT raw 事件收敛；不得通过直接写 status 或
     直接调用 release helper 使检查通过。
   - normal scalar load/store 的 pass/fail/terminal 主逻辑保持不变；本专项只补 fault 事件的真实
     释放来源和超时保护，不新增 RM 或 scoreboard。

### 14.5 本轮完成边界

- 当前代码只有 raw deq consumer、fault token 等基础设施，普通 scalar load/store 的真实 fault
  smoke、release provenance 和 fault-head watchdog 仍未完成；这些必须作为待 coding 项补入既有
  LSQ MMIO/status/SQ deq owner（若原 execution plan 已归档，则新增 follow-up plan，不创建第二个
  SQ deq/fault owner）。新增 coding 必须落在既有 `ctrl monitor -> raw queue -> adapter ->
  lsq_commit_handler` owner 上，不建立第二套 fault 状态机。
- vector LS、AMO/MOU、HLV/HLVX/HSV、CBO（尤其 early CBO fault）和完整 atomic/segment fault
  生命周期继续保持 TODO/运行期 fatal。
- DCache/SBuffer D-error response 已由专项 responder 实现，但 scalar fault drain 仍只消费 DUT
  真实可观察的完成/异常事件；该能力不把 error bit 直接写入 fault、commit/deq 或 terminal 状态。
- 本节不修改 RM、scoreboard、coverage 或 DUT RTL；后续专项必须在完成真实完成/取消事件链路并
  通过 review 后，才能从对应 admission fatal 边界移除。
