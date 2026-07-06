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

状态：当前是简化支持，不是完整 atomic 协议支持。

现象：

- 当前 AMO/MOU 不分配普通 LQ/SQ。
- 当前会 route 到 STA/STD。
- `uop_count` 会记录 AMOCAS 等理论需要几个地址侧或数据侧 uop。
- issue queue 不会按 `uop_count` 展开多个 item，也没有完整 atomic writeback/AMOCAS 多 uop 闭环。

TODO：

- 明确 AMO/LR/SC/AMOCAS 在 DUT 中是否应分配 LQ/SQ、如何 writeback、如何 commit。
- 如果 AMOCAS 需要多个 STA/STD uop，issue queue 需要按 `uop_count` 展开或引入可追踪的 multi-uop item。
- 补齐 `MEMBLOCK_WB_EVENT_SOURCE_ATOMIC_WB` 的真实事件来源和状态转移。

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

状态：当前只支持已建模组合。

现象：

- 非 LDU/STU/MOU 的 `fuType` 没有 fallback，会 fatal。
- LDU/STU/MOU 下非法 `fuOpType` 会 fatal。
- 当前主表随机生成只覆盖普通标量 load/store、software prefetch 和普通 LR/SC/AMO W/D。
- CBO 可以被 `lsq_ctrl_model::is_cbo_fuoptype()` 识别，但当前随机主表默认不会生成 CBO，且后续按 store-like 简化处理。
- AMOCAS 常量和识别逻辑存在，但随机主表默认不会生成 AMOCAS，也没有完整多 uop 闭环。

### 5.1 当前已建模组合

当前框架围绕 MemBlock LSQ admission、issue、writeback、commit/deq 主流程建模，不覆盖整个后端所有 FU。

已建模范围：

- `FuType.ldu`
  - 普通 load：`lb/lh/lw/ld/lbu/lhu/lwu`。
  - software prefetch：`prefetch_i/prefetch_r/prefetch_w`。
- `FuType.stu`
  - 普通 store：`sb/sh/sw/sd`。
  - CBO：`cbo_zero/cbo_clean/cbo_flush/cbo_inval` 可识别，但按 store-like 简化处理。
- `FuType.mou`
  - 普通 LR/SC/AMO W/D：`lr_w/sc_w/amo*_w/lr_d/sc_d/amo*_d`。
  - AMOCAS W/D/Q：常量和 `is_amocas_*` 判断存在，但随机生成和多 uop 闭环未完整覆盖。

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

状态：当前按 load-like/store-like 简化建模。

### 6.1 这个功能怎么理解

CBO 和 Prefetch 都是“访存相关操作”，但它们不是普通 load/store。

- 普通 load/store 的核心目标是读写程序语义上的数据，并且通常有明确的 ROB writeback、LSQ commit/deq 和异常行为。
- Prefetch 的核心目标是提前把 cache line 拉近，属于性能提示。它可能访问 TLB/DCache/MissQueue，但一般不应该像普通 load 一样产生 architectural load data writeback。
- CBO 的核心目标是 cache block operation，例如 clean/flush/inval/zero，操作对象是 cache block 状态或 cache line 内容，不是普通 store 写某几个 byte 的数据路径。

因此“CBO/Prefetch 专项语义”指的是：不能只因为它们复用 LDU/STU 管线入口，就完全按普通 load/store 的完成条件、异常条件、commit/deq 条件来验证。后续需要把它们作为专项 flow，明确它们在 DUT 中到底应该如何入队、如何发射、如何完成、是否写回、是否参与普通 commit/deq，以及异常/redirect/replay 如何表现。

现象：

- prefetch 当前复用 LDU/load-like 路径。
- CBO 当前复用 STU/store-like 路径。
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

后续需要补：

- Prefetch 发射后完成条件：是否等待 DTLB/DCache 反馈，还是只要 request accepted 就认为完成。
- Prefetch 异常语义：TLB miss、page fault、PMP/PMA fault 是否应像普通 load 一样反馈，还是被静默丢弃/转换。
- Prefetch 与普通 load/store 的 merge 行为：命中已有 MSHR 或被普通 demand request 覆盖时，测试框架如何判定成功。
- software prefetch.i 到 frontend、hardware prefetch train、L2/L3 prefetch sender 的 monitor 和 scoreboard 闭环。
- prefetch 专项 testcase 需要区分“仅提示成功”和“普通 load-like pass”两类完成语义，避免把 prefetch 错当成 demand load 验证。

### 6.3 CBO 当前简化点

Scala/DUT 侧已经存在 `cbo_zero/clean/flush/inval` 编码和 cache-line 级路径，但当前框架只做了“合法 op 识别 + store-like route”。

当前实现现状：

- `memblock_dispatch_types.sv` 已定义 `MEMBLOCK_LSUOP_CBO_ZERO/CLEAN/FLUSH/INVAL`。
- `lsq_ctrl_model::is_cbo_fuoptype()` 能识别 CBO，并在 `derive_op_behavior()` 中归类为 `MEMBLOCK_OP_BEHAVIOR_CBO`。
- 当前随机主表默认 `MEMBLOCK_OP_CLASS_STORE -> random_store_fuoptype()`，不会随机生成 CBO；即使手工指定 CBO，也仍按 STU/store-like 路径处理。

后续需要补：

- 为 CBO 增加显式 testcase / directed helper，而不是只依赖手工改 `fuOpType`。
- 区分 `cbo_zero` 与 `cbo_clean/flush/inval` 的完成语义、是否需要普通 store data 路径、是否有特殊异常。
- 明确 cache-line 粒度、uncache/MMIO、fault/redirect/replay 下 CBO 的期望行为，并补对应 scoreboard。

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

### 6.3 CBO 当前简化点

Scala 依据：

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
