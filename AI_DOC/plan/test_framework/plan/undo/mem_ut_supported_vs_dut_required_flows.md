# mem_ut 当前支持 Flow 与 DUT 实际场景 Flow 对比

## 1. 分析目标

本文档用于回答两个问题：

1. 当前 `mem_ut` MemBlock 测试框架已经支持哪些 flow。
2. 结合 XiangShan Scala 源码，真实 DUT MemBlock 实际存在或需要覆盖哪些场景 flow，当前测试框架还有哪些缺口。

本文档只分析测试框架能力边界，不修改 RTL/DUT 行为。

## 2. 结论摘要

当前测试框架已经形成一条标量 real DUT 闭环：

```text
主表生成
-> LSQ admission
-> L2TLB responder 按 runtime CSR + VPN/S2xlate 回复 PTW
-> LOAD/STA/STD issue dispatch
-> int writeback / STA/STD IQ feedback / memoryViolation raw monitor batch 采集
-> normal pass / replay / fault / redirect 状态处理
-> LSQ commit/deq/cancel 状态同步
-> redirect drive + flush/refetch
-> end_test_check
```

当前支持重点是标量 integer load/store、部分 prefetch/CBO/AMO 的简化建模，以及 memory violation redirect/replay 的框架级恢复流程。

主要缺口是：

- vector load/store 真实路径未支持。DUT 有 `vecIssue`、`vecWriteback`、`vlduIqFeedback/vstuIqFeedback`、`VSplit/VMergeBuffer/VSegmentUnit` 等完整路径，但测试框架当前遇到 vector LS 会 fatal 或 drop。
- AMO/LR/SC/AMOCAS 只做简化 route，不完整支持 AtomicsUnit 的独占状态、flush sb、AMOCAS 多 uop、真实 atomic writeback 闭环。
- CBO/prefetch 当前按 load-like/store-like 简化，未覆盖 CBO clean/flush/inval/zero、software prefetch.i、hardware prefetch train、uncache/MMIO 特殊语义。
- LSQ 多元素范围映射不完整。当前只保存 base LQ/SQ key，未覆盖 vector split 后多个元素对应多个 LSQ entry 的完整关系。
- vec writeback、vec IQ feedback、vector exception/FOF/segment atomic flow 不支持。
- DCache/sbuffer/uncache/MMIO 数据返回和 store buffer 真实排空语义大多是外围简化，不是完整数据通路验证。

## 3. 当前测试框架支持的 Flow

### 3.1 主表生成 Flow

入口：

- `memblock_dispatch_real_smoke_sequence::body()`
- `memblock_dispatch_base_sequence::build_main_table()`
- `build_random_main_table()`
- `import_manual_main_table()`

关键文件：

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/main_control_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`

支持程度：已支持。

当前能力：

- 支持随机主表和手动主表。
- 随机主表可按权重生成 `INT_LOAD`、`FP_LOAD`、`STORE`、`PREFETCH`、`AMO` 等 op class。
- 支持地址复用注入，用于提升 load/store 地址相关和 memory violation 概率。
- 每个 uid 对应主表静态 entry，状态由 `status_by_uid` 管理。

限制：

- 当前主表虽然有 AMO/PREFETCH/CBO 抽象类型，但后续 flow 对这些类型多为简化支持。
- vector LS 默认不支持，`lsq_ctrl_model::derive_op_behavior()` 遇到 vector LS 会 fatal。
- 主表中的 `uid` 是测试框架静态编号；redirect 后同一 uid 重新 admission/issue，但不是完整模拟 ROB 重新分配体系。

### 3.2 LSQ Admission Flow

入口：

- `memblock_lsqenq_dispatch_sequence::body()`
- `drive_lsqenq_loop()`
- `send_lsqenq_cycle()`
- `collect_lsq_candidates()`
- `confirm_lsq_candidates()`
- `complete_admission()`

关键文件：

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqenq_dispatch_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/lsq_ctrl_model.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/*`

支持程度：标量 load/store 已支持；atomic/prefetch/CBO 简化；vector 不支持。

当前能力：

- 根据主表 uid 顺序向 DUT `enqLsq` 入队。
- 使用 `lsq_ctrl_model` 维护软件 LQ/SQ free count 和 enq/deq pointer。
- 入队时同时参考本地 free count 和 DUT canAccept/ready。
- 入队成功后 `activate_uid_by_behavior()` 建立 ROB/LQ/SQ active map。
- redirect flush 后通过 `pending_lq_cancel_count/pending_sq_cancel_count` 回滚本地 LSQ admission 镜像。
- 扫描窗口使用 `seq_csr_common::get_real_lsq_enq_max()` 控制，避免 10 万笔场景每拍全表扫描。

限制：

- 当前 `lsq_ctrl_model::derive_op_behavior()` 对 vector LS fatal。
- AMO/MOU 当前 `need_alloc=0`，不分配普通 LQ/SQ；这和真实 AtomicsUnit/flush sb 语义不是完整等价。
- CBO/prefetch 虽能走 LDU/STU 抽象，但未覆盖特殊完成、异常、uncache/MMIO 语义。

### 3.3 L2TLB/PTW Responder Flow

入口：

- `memblock_l2tlb_base_sequence::body()`
- `drive_l2tlb_loop()`
- `send_l2tlb_cycle()`
- `data.get_or_create_tlb_entry_by_req()`
- `fill_dtlb_resp_from_entry()`

关键文件：

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_l2tlb_base_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/tlb_map_builder.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_tlb_entry.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/mmu_csr_runtime_state.sv`
- `mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/*`

支持程度：基本支持 DTLB->L2TLB/PTW 回复；异常 PTE 组合可参数化但覆盖仍有限。

当前能力：

- L2TLB agent 接管的是 DTLB 和 L2TLB/PTW 之间的接口，不是 L2TLB 和 L2Cache 的接口。
- 监测 DUT 发出的 PTW request，采样 `vpn/s2xlate`。
- 从 runtime CSR 镜像取得 ASID/VMID 等上下文，按 `{vpn, asid, vmid, s2xlate}` 查找或创建 TLB entry。
- 通过 `pre_pkt_gap` 注入响应延迟。
- 若查表失败，当前直接 `uvm_fatal`，不再生成 missing fallback response。

限制：

- 主要服务当前接管的 L2TLB port；多 port、多 request 并发、真实 PTW miss/refill 队列行为不是完整模型。
- PTE 权限/异常组合有基础参数化，但对所有 page fault/access fault/guest page fault 场景覆盖仍需专项用例。
- CSR runtime 依赖 monitor 采样最新 CSR；如果 CSR agent 覆盖不完整，对 TLB key 的准确性会受影响。

### 3.4 Issue Dispatch Flow

入口：

- `memblock_lintsissue_dispatch_sequence::body()`
- `drive_dispatch_issue_loop()`
- `send_issue_cycle()`
- `issue_queue_scheduler::select_issue_candidates()`
- `issue_field_assigner::assign_issue_item_fields()`
- `issue_queue_scheduler::mark_issue_fire()`

关键文件：

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lintsissue_dispatch_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/issue_queue_scheduler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/issue_field_assigner.sv`
- `mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/*`

支持程度：标量 `LOAD/STA/STD` issue 已支持；vector issue 不支持；atomic 多 uop未完整展开。

当前能力：

- `issue_queue_scheduler` 维护 `load_issue_q/sta_issue_q/std_issue_q`。
- 支持 `send_pri`/`send_pri_std` 优先级和非优先级并行发射。
- 支持 issue delay。
- 支持 redirect/flush 期间冻结 route/issue。
- driver 返回 fired mask 后，只对真正 fire 的 item 调 `mark_issue_fire()`。
- 对 redirect 边界拍支持 partial fire 补记。
- 发射后从 issue queue 删除 item，状态表记录 dispatched/pass/writeback/replay 等阶段。

限制：

- 当前只驱动 int issue 的 load/STA/STD 端口，不驱动 DUT `vecIssue`。
- `memblock_issue_q_item_t.uop_count` 会记录 AMOCAS 等理论需要几个 uop，但 issue queue 不按 `uop_count` 展开多个 item。
- 端口映射是测试框架抽象：LOAD 使用 0/1/2，STA 使用 3/4，STD 使用 5/6；没有覆盖 HybridUnit、vector issue、segment issue 的全部场景。

### 3.5 Writeback / IQ Feedback / Redirect Monitor Flow

入口：

- `memblock_dispatch_real_smoke_sequence::service_monitor_once()`
- `collect_runtime_context_events()`
- `collect_monitor_event_batch()`
- `dispatch_monitor_event_adapter::collect_writeback_events_batch()`
- `dispatch_monitor_event_adapter::collect_ctrl_redirect_events_batch()`
- `dispatch_monitor_batch_handler::process_monitor_event_batch()`
- `writeback_status_handler::handle_event()`
- `exception_redirect_replay_handler::process_pending_events()`

关键文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/dispatch_monitor_event_adapter.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/dispatch_monitor_batch_handler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/writeback_status_handler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/exception_redirect_replay_handler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_sync_pkg.sv`

支持程度：标量 int writeback、STA/STD feedback、memoryViolation redirect 已支持；vec writeback/vec feedback 不支持或仅 drop/fatal。

当前能力：

- monitor 先把 raw int writeback、IQ feedback、ctrl memoryViolation 放入 raw queue。
- service loop 一次收集成同一个 semantic batch。
- batch handler 先 normalize，再做 active redirect 过滤和同批 redirect-first 仲裁。
- normal pass 更新状态表。
- replay/fault/redirect 进入 `exception_event_q`，由 exception handler 统一处理。

限制：

- `vecWriteback`、`vlduIqFeedback`、`vstuIqFeedback` 不是当前闭环支持重点。
- 对 load replay、backend replay、fault 的状态恢复是测试框架级简化，不模拟所有 backend 复杂策略。
- DCache 数据、uncache、MMIO、sbuffer 写回的真实数据一致性不是当前 writeback handler 的覆盖目标。

### 3.6 LSQ Commit / Deq / Cancel Flow

入口：

- `memblock_lsqcommit_dispatch_sequence::body()`
- `send_lsqcommit_cycle()`
- `lsq_commit_handler::build_lsqcommit_xaction()`
- `lsq_commit_handler::mark_rob_commit_batch()`
- `dispatch_monitor_event_adapter::apply_raw_ctrl_deq()`
- `lsq_commit_handler::apply_raw_ctrl_deq()`

关键文件：

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqcommit_dispatch_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/lsq_commit_handler.sv`
- `mem_ut/ver/ut/memblock/agent/lsqcommit_agent_agent/src/*`
- `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/*`

支持程度：基础 commit/deq/cancel 同步支持；复杂 ROB/flush/sbuffer 边界简化。

当前能力：

- commit sequence 驱动 `lcommit/scommit/commit/pendingPtr/pendingPtrNext/flushSb`。
- ctrl monitor 采样 `lqDeq/sqDeq/lqCancelCnt/sqCancelCnt/sbIsEmpty/memoryViolation`。
- 本地 LSQ free count 以 DUT deq/cancel 为最终真源进行同步。
- `flushSb` 支持 request/drive/wait empty/timeout。

限制：

- 真实 ROB commit、store exception、vector LS exception、MMIO busy 等更复杂交互未完整建模。
- 目前 success 更偏向“测试框架闭环完成”，不是完整架构提交语义。

### 3.7 Redirect / Flush / Refetch Flow

入口：

- `dispatch_monitor_batch_handler::process_monitor_event_batch()`
- `exception_redirect_replay_handler::process_pending_events()`
- `request_redirect_flush()`
- `prepare_uid_for_refetch()`
- `prepare_redirect_refetch_from_uid()`
- `memblock_redirect_dispatch_sequence::drive_redirect_payload()`

关键文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/exception_redirect_replay_handler.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_redirect_dispatch_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`

支持程度：支持 memoryViolation redirect 后冻结、驱动 redirect、清理并从最老 pending uid 顺序重新入队。

当前能力：

- active redirect 期间 route/issue/admission 被阻塞。
- redirect 覆盖范围内事件不落 pass/writeback 状态。
- redirect 后从最老 `redirect_pending` uid 顺序重入队。
- 通过 `dispatch_flush_epoch` 和 fired mask 处理 redirect 边界拍 issue fire。

限制：

- 不是完整 backend redirect/ROB recovery 模型，只保证测试激励合法和状态不冲突。
- 对同拍 writeback + memoryViolation 的优先级已通过 batch redirect-first 处理，但更复杂多 redirect 同拍场景仍是抽象处理。

### 3.8 Backend Replay Flow

入口：

- `dispatch_monitor_event_adapter::convert_raw_iq_feedback()`
- `writeback_status_handler::handle_issue_feedback_event()`
- `common_data_transaction::mark_replay_pending()`
- `issue_queue_scheduler::route_uid()`

关键文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/writeback_status_handler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/issue_queue_scheduler.sv`

支持程度：STA/LOAD 等 target 级 replay 状态支持；STD backend replay 当前不是完整支持。

当前能力：

- replay event 会清对应 target dispatched/queued 状态，增加 `replay_seq`。
- route 时只重新入队 replay 指定 target。
- fire 后清 replay target。

限制：

- DUT 内部 load replay queue / TLB replay / DCache miss replay 的完整机制没有模拟，只处理后端反馈到测试框架的 replay 抽象。
- STD miss/replay 当前不是完整闭环，部分路径可能 warning/drop。

### 3.9 CSR / Sfence / Hfence Flow

入口：

- `dispatch_monitor_event_adapter::drain_csr_events()`
- `dispatch_monitor_event_adapter::drain_sfence_events()`
- `common_data_transaction::apply_raw_csr_runtime()`
- `common_data_transaction::apply_raw_sfence()`

关键文件：

- `mem_ut/ver/ut/memblock/seq/base_seq/dispatch_monitor_event_adapter.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/mmu_csr_runtime_state.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/agent/fence_agent_agent/src/*`

支持程度：runtime CSR snapshot 和 sfence/hfence entry invalidation 基础支持。

当前能力：

- CSR runtime 使用 monitor 采集值，不使用初始 csr_common 静态值。
- `collect_runtime_context_events()` 先同步 CSR runtime，再 FIFO 消费 sfence/hfence。
- TLB by-key table 可根据 sfence/hfence 做 entry 级失效。

限制：

- 依赖 CSR/fence agent 采样完整性。
- 对所有 CSR mode 切换、satp/vsatp/hgatp 变化和 PTW resp cancel 的真实时序只是抽象建模。

### 3.10 Software Smoke Flow

入口：

- `soft_test_memblock_dispatch_smoke_sequence`
- `soft_test_memblock_dispatch_replay_smoke_sequence`

关键文件：

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/soft_test/*`

支持程度：软件闭环验证支持，不代表真实 DUT flow。

当前能力：

- 直接构造 pass/replay event，验证公共状态表、queue、handler 的软件逻辑。

限制：

- 不严格驱动真实 DUT interface。
- 不应用作真实 MemBlock 行为覆盖依据。

## 4. Scala 源码显示的 DUT 实际场景 Flow

### 4.1 MemBlock 顶层接口场景

源码依据：

- `src/main/scala/xiangshan/mem/MemBlock.scala`
- `src/main/scala/xiangshan/XSCore.scala`

DUT MemBlock 真实接口包括：

- `ooo_to_mem.enqLsq`
- `ooo_to_mem.intIssue`
- `ooo_to_mem.vecIssue`
- `ooo_to_mem.lsqio.lcommit/scommit/commit/pendingPtr/pendingPtrNext`
- `ooo_to_mem.sfence`
- `ooo_to_mem.flushSb`
- `mem_to_ooo.intWriteback`
- `mem_to_ooo.vecWriteback`
- `mem_to_ooo.staIqFeedback/hyuIqFeedback/vlduIqFeedback/vstuIqFeedback`
- `mem_to_ooo.memoryViolation`
- `mem_to_ooo.lqCancelCnt/sqCancelCnt/lqDeq/sqDeq/sbIsEmpty`

测试框架当前覆盖了其中的 `enqLsq`、标量 `intIssue`、标量 `intWriteback`、STA/STD feedback、ctrl/commit/redirect 主要路径；未完整覆盖 `vecIssue/vecWriteback/vlduIqFeedback/vstuIqFeedback/hyuIqFeedback`。

### 4.2 标量 Load Flow

源码依据：

- `MemBlock.scala` 中 `issueLda` 连接 `newLoadUnits(i).io.ldin`
- `NewLoadUnit.scala`
- `LSQWrapper.scala`
- `LoadQueue*.scala`

DUT 场景：

```text
LSQ enq 分配 LQ
-> intIssue load
-> LoadUnit TLB/PMP/DCache/forward
-> LQ write
-> intWriteback / exception / replay / memoryViolation
-> ROB commit
-> LQ deq
```

测试框架支持程度：已支持基础标量 load 闭环。

缺口：

- DCache/forward/uncache/MMIO 数据路径没有完整参考模型。
- load 内部 replay queue、tlbreplay、RAR/RAW nuke 只通过外部 feedback/redirect 抽象体现。

### 4.3 标量 Store Flow

源码依据：

- `MemBlock.scala` 中 `issueSta/issueStd` 连接 `storeUnits` 和 `stdExeUnits`
- `NewStoreUnit.scala`
- `NewStoreQueue.scala`

DUT 场景：

```text
LSQ enq 分配 SQ
-> STA issue 计算地址/TLB/PMP/异常/RS feedback
-> STD issue 提供 store data
-> StoreQueue 合并地址和数据
-> commit 后写 sbuffer/DCache
-> SQ deq
```

测试框架支持程度：基础 store STA/STD route 和 feedback 支持。

缺口：

- store data 与 StoreQueue/sbuffer/DCache 的真实闭环没有完整建模。
- STD backend replay 不完整。
- unaligned store、MMIO、uncache、CBO store-like 特殊行为缺少专项支持。

### 4.4 Memory Violation Redirect Flow

源码依据：

- `MemBlock.scala` 中 `allRedirect = newLoadUnits.map(_.io.rollback) ++ lsq.io.nack_rollback ++ lsq.io.nuke_rollback`
- `Redirect.selectOldestRedirect(allRedirect)`
- `io.mem_to_ooo.memoryViolation := oldestRedirect`

DUT 场景：

```text
Load/LSQ 检测 RAW/RAR/nack/nuke
-> 选择最老 redirect
-> mem_to_ooo.memoryViolation
-> backend redirect
-> younger uop flush/refetch
```

测试框架支持程度：支持 memoryViolation 转 redirect，并冻结/清理/重入队。

缺口：

- 不完整模拟 backend ROB 重命名、rename snapshot、真实 refetch 生成新 robIdx 的行为。
- 当前策略是同 uid 重新 admission/issue，保证 MemBlock 激励合法，不等价于完整 CPU 前后端恢复。

### 4.5 TLB/PTW/Sfence Flow

源码依据：

- `MemBlock.scala` 中 DTLB、PTW、`ptw.io.sfence <> sfence`
- `ptw_resp_v` 会受 `sfence.valid` 和 `tlbcsr.satp/vsatp/hgatp.changed` 影响
- `dtlbRepeater = PTWNewFilter(...)`

DUT 场景：

```text
Load/Store/Prefetch DTLB req
-> PTW/L2TLB req
-> PTW resp refill
-> sfence/CSR mode change 可取消或过滤 response
-> replay/继续执行
```

测试框架支持程度：基础 L2TLB responder + runtime CSR + sfence invalidation 支持。

缺口：

- PTW filter/repeater 的多 request、多 hit vector、fenceDelay、CSR changed cancel 时序未完整复刻。
- prefetch TLB、store TLB、vector TLB 多端口覆盖不足。

### 4.6 Atomic / LR / SC / AMOCAS Flow

源码依据：

- `package.scala` 中 `AMOOpType` 定义 LR/SC/AMO/AMOCAS。
- `MemBlock.scala` 中 `atomicsUnit`、`state === s_atomics(i)`、AMO 使用 load_0 TLB，atomic 期间禁用硬件 prefetch，并要求 load unit 无 in-flight。

DUT 场景：

```text
MOU/AMO issue
-> AtomicsUnit 接管
-> 可能等待 sbuffer empty / flush sbuffer
-> 使用 TLB/DCache
-> feedback/writeback
-> LR/SC reservation 或 AMOCAS 多 uop
```

测试框架支持程度：简化支持。

缺口：

- 不完整支持 LR/SC reservation。
- 不完整支持 AMOCAS 多 uop 分片。
- 不完整支持 AtomicsUnit 状态机、flush sbuffer、atomic writeback 和异常语义。

### 4.7 CBO / Prefetch Flow

源码依据：

- `package.scala` 中 prefetch 和 CBO op 编码。
- `NewStoreUnit.scala` 中 CBO cache-line 行为、prefetch request 入口。
- `MemBlock.scala` 中 hardware prefetcher、software prefetch to frontend、L1/L2/L3 prefetch sender。

DUT 场景：

```text
software prefetch.i/r/w 或 CBO clean/flush/inval/zero
-> load/store-like pipe 或 prefetcher
-> TLB/PMP/cache/sbuffer 特殊处理
-> 可能无普通 writeback 或有特殊 exception
```

测试框架支持程度：简化支持。

缺口：

- CBO clean/flush/inval/zero 语义未专项建模。
- prefetch.i 到 frontend、hardware prefetch train、L2/L3 prefetch sender 未作为闭环覆盖。
- prefetch 通常不等价于普通 load pass，当前按 load-like 简化可能掩盖特殊完成语义。

### 4.8 Vector Load/Store / Segment / FOF Flow

源码依据：

- `package.scala` 中 `VlduType/VstuType`。
- `Parameters.scala` 中 `VecLoadPipelineWidth/VecStorePipelineWidth/VecMemDispatchWidth/VecMemDispatchMaxNumber`。
- `MemBlock.scala` 中 `vecIssue`、`vecWriteback`、`VSplit`、`VMergeBuffer`、`VfofBuffer`、`VSegmentUnit`、`vlduIqFeedback/vstuIqFeedback`。

DUT 场景：

```text
vecIssue
-> VSplit 分成多个 element/uop
-> LoadUnit/StoreUnit 标量管线执行 element
-> VMergeBuffer/VfofBuffer/VSegmentUnit 合并结果或反馈
-> vecWriteback / vector IQ feedback
```

测试框架支持程度：不支持。

缺口：

- 不驱动 `vecIssue`。
- 不采集/处理 `vecWriteback`。
- 不处理 `vlduIqFeedback/vstuIqFeedback`。
- 不维护 vector 多元素 LQ/SQ 范围。
- 不支持 FOF、segment atomic、vector exception/vstart/vl/vstart 更新。

## 5. 支持程度对比表

| Flow | DUT 实际存在 | 当前测试框架支持 | 支持程度 | 主要缺口 |
|---|---:|---:|---|---|
| 随机/手动主表 | 是 | 是 | 已支持 | vector/特殊 op 后续闭环不足 |
| 标量 LSQ admission | 是 | 是 | 已支持 | 多元素/复杂 cancel 简化 |
| 标量 load issue/writeback | 是 | 是 | 基础支持 | DCache/forward/uncache 数据路径简化 |
| 标量 store STA/STD | 是 | 是 | 基础支持 | StoreQueue/sbuffer/data merge 不完整 |
| STA IQ feedback | 是 | 是 | 基础支持 | fast/slow、HYU 覆盖不足 |
| STD feedback/pass | 是 | 是 | 简化支持 | STD real writeback/replay 不完整 |
| memoryViolation redirect | 是 | 是 | 支持 | backend refetch/ROB recovery 简化 |
| backend replay | 是 | 是 | 简化支持 | load replay queue / STD replay / PTW replay 细节不足 |
| LSQ commit/deq/cancel | 是 | 是 | 基础支持 | ROB/store exception/vector commit 简化 |
| L2TLB/PTW response | 是 | 是 | 基础支持 | 多端口、CSR changed cancel、异常组合不足 |
| CSR runtime | 是 | 是 | 基础支持 | 依赖 monitor 覆盖 |
| sfence/hfence | 是 | 是 | 基础支持 | fenceDelay/PTW filter 细节不足 |
| AMO/LR/SC | 是 | 部分 | 简化支持 | reservation、AtomicsUnit、flush sbuffer 不完整 |
| AMOCAS 多 uop | 是 | 部分 | 简化支持 | 只记录 uop_count，不展开完整 uop |
| CBO clean/flush/inval/zero | 是 | 部分 | 简化支持 | cache-line/uncache/MMIO/异常语义不足 |
| software/hardware prefetch | 是 | 部分 | 简化支持 | frontend prefetch、prefetch train、L2/L3 sender 未闭环 |
| vector load/store | 是 | 否 | 不支持 | vecIssue/vecWriteback/vector feedback/多元素 LSQ |
| vector segment/FOF | 是 | 否 | 不支持 | VSegmentUnit/VfofBuffer/vstart/vl 未支持 |
| MMIO/uncache | 是 | 否/部分 | 缺口 | uncache buffer/MMIO busy/exception 未专项支持 |

## 6. 建议后续补齐优先级

### P0：保持当前标量 real DUT 闭环稳定

- 持续保证 `collect_monitor_event_batch()` redirect-first 仲裁。
- 继续维护 LSQ admission/issue/commit 的有限扫描，避免 10 万笔性能退化。
- 完善 normal pass/replay/redirect/fault flow 文档和网页 callgraph。

### P1：补齐标量边界场景

- STD real writeback / store data complete / sbuffer deq 闭环。
- unaligned store、MMIO、uncache load/store。
- TLB page fault/access fault/guest page fault 权限组合专项。
- load replay queue / PTW replay / DCache miss replay 更细粒度建模。

### P2：补齐 AMO/CBO/prefetch

- AtomicsUnit 状态机、flush sbuffer、LR/SC reservation。
- AMOCAS 多 uop issue 展开和 writeback 匹配。
- CBO clean/flush/inval/zero 的独立完成和异常语义。
- software prefetch.i、hardware prefetch train、L2/L3 prefetch sender 的 monitor/scoreboard。

### P3：新增 vector LS 框架

- 新增 vec issue sequence 或扩展现有 dispatch sequence 支持 `vecIssue`。
- 建立 vector 多元素 LSQ range model。
- 支持 `VSplit/VMergeBuffer/VfofBuffer/VSegmentUnit` 对应 feedback/writeback。
- 支持 vector exception、FOF、segment atomic、vstart/vl 更新。

## 7. 关键源码依据

测试框架侧：

- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqenq_dispatch_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lintsissue_dispatch_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_lsqcommit_dispatch_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_redirect_dispatch_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_l2tlb_base_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/lsq_ctrl_model.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/issue_queue_scheduler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/dispatch_monitor_event_adapter.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/dispatch_monitor_batch_handler.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`

Scala DUT 侧：

- `src/main/scala/xiangshan/XSCore.scala`
- `src/main/scala/xiangshan/Parameters.scala`
- `src/main/scala/xiangshan/package.scala`
- `src/main/scala/xiangshan/mem/MemBlock.scala`
- `src/main/scala/xiangshan/mem/lsqueue/*.scala`
- `src/main/scala/xiangshan/mem/pipeline/NewLoadUnit.scala`
- `src/main/scala/xiangshan/mem/pipeline/NewStoreUnit.scala`
- `src/main/scala/xiangshan/mem/pipeline/AtomicsUnit.scala`
- `src/main/scala/xiangshan/mem/vector/*.scala`
