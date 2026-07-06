# memblock IO 接口公共驱动逻辑方案

## 目标和范围

本文档给出 `memblock` 验证输入的公共驱动逻辑方案。目标不是让每个 agent 独立随机，而是在现有 UVM 环境之上增加一层统一的 `virtual sequence + common state model`，由公共状态变量决定各接口输入，并保证 `lsqenq`、`intIssue`、`vecIssue`、TLB/PTW、DCache/Sbuffer TileLink 响应、redirect/fence/csr 控制之间满足 XiangShan `MemBlock` 的合法性。

端口依据：

- 顶层真实接口以 `build_memblock/rtl/MemBlock.sv` 为准。
- `DCache.sv`、`Sbuffer.sv` 的 module 输入用于理解内部模块输入和子模块 UT 扩展，不直接等同于当前 MemBlock 顶层 UVM agent 可驱动端口。
- Scala 源码只作为逻辑和约束来源，重点参考 `src/main/scala/xiangshan/mem/MemBlock.scala`、`src/main/scala/xiangshan/mem/lsqueue/LSQWrapper.scala`、`src/main/scala/xiangshan/backend/dispatch/Dispatch.scala`、`src/main/scala/xiangshan/backend/rename/Rename.scala`、`src/main/scala/xiangshan/mem/vector/VSplit.scala`、`src/main/scala/xiangshan/mem/vector/VSegmentUnit.scala`、`src/main/scala/xiangshan/backend/fu/FuType.scala`、`src/main/scala/xiangshan/Parameters.scala`。
- 端口字段清单见 `AI_DOC/analysis/interface/memblock验证输入的程序框架.md`。

当前 UVM 环境中，输入 agent 已有 driver/sequencer/monitor，输出 agent 为 monitor-only。公共驱动层不替换生成式 driver，而是集中生成合法 `*_xaction` 并送入各 agent sequencer。

## 当前可控接口边界

### OOO 到 MemBlock 输入

- `lsqenq_agent_agent`：驱动 `io_ooo_to_mem_enqLsq_needAlloc_*` 与 `req_*`，通过 monitor 观察 `canAccept` 和 `resp_*`。该接口只覆盖 dispatch 会送入 LSQ 的非 AMO、非 segment、非 FOF-fix-VL mem uop。
- `lintsissue_agent_agent`：驱动 7 个 scalar mem issue 口。`intIssue_0/1/2` 是 load 口，`intIssue_3/4` 是 STA 口，`intIssue_5/6` 是 STD 口。
- `vecissue_agent_agent`：驱动 2 个 vector mem issue 口。`vecIssue_0_0` 有 `fuType`，`vecIssue_1_0` 没有显式 `fuType`，segment 类场景应只从 0 口发起。
- `lsqcommit_agent_agent`：当前 xaction 只覆盖 `pendingPtr` 和 `flushSb`，完整 commit/lcommit/scommit 端口后续若补齐可接入同一状态模型。
- `redirect_agent_agent`、`fence_agent_agent`、`csr_ctrl_agent_agent`、`other_ctrl_agent_agent`：驱动 flush、TLB CSR、prefetch、sbuffer timeout、reset/backend 状态等控制输入。

### Memory/TileLink 环境响应

- `dcache_agent_agent`：DUT 输出 `auto_inner_dcache_client_out_a/c/e`，UVM driver 只驱动环境侧 `a_ready`、`b_valid`、`c_ready`、`d_valid`、`e_ready` 及 B/D payload。D response 必须由 monitor 看到的 A/C 请求派生。
- `sbuffer_agent_agent`：DUT 输出 `auto_inner_buffers_out_a`，UVM driver 驱动 `a_ready` 和 D response。D response 必须匹配 Sbuffer A 请求的 source/size/opcode。
- `itlb_agent_agent`：当前为 fetch-to-mem ITLB/PTW 风格接口。统一地址模型可复用给 DTLB/ITLB 响应，响应必须由实际请求触发。

当前部分 agent monitor 已采样信号，但实际 `mon_item_port.write(mon_tr)` 仍在 TODO 注释块内，至少包括 `lsqenq_agent_agent_monitor.sv`、`vecissue_agent_agent_monitor.sv`、`dcache_agent_agent_monitor.sv`、`sbuffer_agent_agent_monitor.sv`。公共状态机真正落地前，必须先让关键 monitor 在握手 fire 或有效事件发生时送出 transaction，否则公共模型无法确认 LSQ 接收、issue fire、TL outstanding 和响应完成。

## 公共控制状态变量列表

| 状态对象 | 关键变量 | 控制行为 |
| --- | --- | --- |
| `scenario_ctrl` | `scenario_kind`、`txn_mix`、`max_inflight`、`allow_vec`、`allow_segment`、`allow_atomic`、`allow_redirect`、`allow_fault`、`allow_replay`、`allow_backpressure` | 决定本轮生成 load/store/vector/flush/fault 的比例和上限 |
| `reset_ctrl` | `reset_backend_done`、`warmup_cycles`、`idle_after_reset` | 所有 driver 必须等 `memblock_sync_pkg::reset_backend_done` 后开始发 item |
| `rob_state` | `rob_head`、`rob_tail`、`rob_flag`、`live_rob[352]`、`rob_age_seq`、`rob_status` | 分配 `robIdx`，判断 older/younger，redirect kill，commit 回收 |
| `lq_state` | `lq_tail`、`lq_flag`、`live_lq[72]`、`lq_owner_rob`、`lq_status` | 分配 load/vector-load 的 `lqIdx`，关联 load issue、writeback、deq |
| `sq_state` | `sq_tail`、`sq_flag`、`live_sq[56]`、`sq_owner_rob`、`sq_status`、`sq_snapshot` | 分配 store/vector-store 的 `sqIdx`，为 load 提供 older-store 快照 |
| `uop_table` | `txn_id`、`kind`、`robIdx`、`lqIdx`、`sqIdx`、`uopIdx`、`fuType`、`fuOpType`、`issue_port`、`numLsElem` | 跨 agent 的统一事务身份，禁止各 agent 独立猜字段 |
| `issue_port_state` | `ld_port_busy[3]`、`sta_port_busy[2]`、`std_port_busy[2]`、`vec_port_busy[2]`、`last_issue_cycle` | 控制每周期各 issue 口是否可用，避免同口重复驱动 |
| `addr_model` | `vaddr`、`vpn`、`paddr`、`ppn`、`page_attr`、`cacheable`、`nc/mmio`、`alias`、`line_addr` | 生成虚实地址、TLB/PTE、DCache/Sbuffer TL 地址并保持一致 |
| `mem_model` | `byte_mem[paddr]`、`line_data`、`store_mask`、`load_expected_data` | 生成 load 返回、store 更新、forward 期望 |
| `tlb_state` | `satp/vsatp/hgatp`、`priv`、`sfence_pending`、`ptw_outstanding[vpn]`、`fault_plan` | 只响应真实 TLB/PTW 请求，并保持 CSR/fence 约束 |
| `dcache_tl_state` | `a_outstanding[source]`、`c_outstanding[source]`、`sink_credit`、`d_resp_queue`、`probe_plan` | DCache TL A/C/E/B/D 合法响应和 backpressure |
| `sbuffer_tl_state` | `a_outstanding[source]`、`d_resp_queue`、`sbuffer_occ_est`、`flush_pending` | Sbuffer TL A/D 合法响应、store drain、flush empty |
| `sbuffer_ctrl_state` | `sqempty`、`mshr_store_empty`、`force_write`、`sbuffer_timeout` | 控制 sbuffer flush/drain 与强制写出 |
| `vec_state` | `vl`、`vstart`、`vsew`、`vlmul`、`vm`、`vmask`、`nf`、`veew`、`vuopIdx`、`lastUop`、`segment_active` | 控制 vector load/store split、mask、segment、FOF 合法性 |
| `flush_state` | `redirect_valid`、`redirect_level`、`redirect_rob`、`flushSb`、`sfence`、`cmo_flush`、`atomic_flush`、`killed_txn` | 统一处理 redirect/fence/atomic/cmo，保证互斥和事务取消 |
| `csr_state` | `l2_pf_store_only`、`uncache_write_outstanding_enable`、`cache_error_enable`、`flush_l2_enable`、`trigger_enable` | 生成 CSR 控制输入，限制 uncache/prefetch/error 行为 |

## 公共状态机

公共驱动层建议按事务生命周期维护以下状态，所有 agent 只能消费该状态机产生的 item，不再直接独立 randomize 关键字段。

| 状态 | 进入条件 | 允许动作 | 退出条件 |
| --- | --- | --- | --- |
| `TXN_NEW` | scenario 创建事务 | 分配 `txn_id`、选择类型和地址 | 索引分配完成 |
| `TXN_ALLOCATED` | `robIdx/lqIdx/sqIdx` 或 AMO/segment 专用上下文已分配 | 发送 `lsqenq` 或标记为绕过普通 LSQ enqueue | `lsqenq` 被接收，或专用场景进入 issue |
| `TXN_LSQ_ACCEPTED` | monitor 捕获 `canAccept` 和 `resp_*` 匹配 | 发 scalar/vector issue | issue fire 或被 redirect kill |
| `TXN_ISSUED` | issue valid-ready fire | 等待 TLB/PTW、DCache/Sbuffer、feedback/writeback | 外部请求捕获、replay、异常或 kill |
| `TXN_MEM_OUTSTANDING` | 捕获 DCache/Sbuffer TL A/C 请求 | 生成匹配 D/B/E 流程，更新 memory model | TL 响应完成 |
| `TXN_DONE` | writeback、store drain、异常或 no-op 完成 | 允许 commit/deq 回收 | ROB/LQ/SQ 回收 |
| `TXN_KILLED` | redirect/fence/exception 命中该事务 | 禁止新 issue 和新期望；只完成协议必要响应 | 所有已发外部请求被清理 |

公共状态机需要有一个全局 `epoch_id`。reset、redirect、sfence、CSR changed、flushSb、atomic flush、cmo flush 都会推进或标记 epoch；任何 responder 在返回 TLB/TL 响应前必须检查请求所属 epoch 是否仍然有效。

## 索引分配规则

### ROB

- `RobSize = 352`，`robIdx_value` 范围为 `0..351`，`robIdx_flag` 在 value 环绕时翻转。
- 公共分配器维护 `rob_tail`，只有当对应 ROB 项已 commit 或被 redirect kill 后才可复用。
- 同一微操作在 `lsqenq`、`intIssue/vecIssue`、writeback 期望、commit 状态中必须使用同一个 `robIdx`。
- redirect 判断采用 XiangShan `RobPtr.needFlush` 语义：`RedirectLevel.flushAfter` 只杀 younger，`RedirectLevel.flush` 杀 redirect 自身和 younger。公共模型必须按 flag/value 的循环年龄关系更新 `killed_txn`。

### LQ

- `VirtualLoadQueueSize = 72`，`lqIdx_value` 范围为 `0..71`，flag 在 value 环绕时翻转。
- load、vector load 必须分配 live LQ 项；store-only 不得凭空占用 LQ。
- `lsqenq` 请求中的 `lqIdx` 是期望分配值，monitor 观察到的 `enqLsq_resp_*_lqIdx` 必须与公共模型一致；后续 issue 优先使用实际 resp 捕获值。
- LQ 项在 load 完成并被 commit/deq 后回收；若 redirect kill，该项立即标记可回收，但需要等待不会再产生对应响应。

### SQ

- `StoreQueueSize = 56`，`sqIdx_value` 范围为 `0..55`，flag 在 value 环绕时翻转。
- store、vector store 必须分配 live SQ 项；load 的 `sqIdx` 是发射时的 older-store 快照，不得指向尚未分配的 younger store。
- AMO 使用 `mou`/AtomicsUnit 路径，不走 dispatch 端普通 `enqLsq.req.valid`，但 issue payload 仍携带 `sqIdx/lqIdx` 字段用于 feedback。公共模型应把 AMO 作为独立 scenario 串行执行：先冻结普通 store drain 之外的新事务，再等待 atomic flush_sbuffer 与 DCache atomic 请求闭环。
- STA 和 STD 必须使用同一 `robIdx/sqIdx`。STD 不得在没有 SQ 分配的情况下单独发出。
- SQ 项在 store commit、进入 Sbuffer 并完成必要 drain 后回收；redirect kill 会取消未提交 store，已进入 Sbuffer 的 store 只能通过合法 flush/drain 清空。

## 通用生成流程

1. reset 结束后，公共 vseq 先发送安全默认 CSR/other_ctrl：`hartId` 固定，错误输入为 0，`reset_backend=0`，TL B/Probe 默认关闭，DCache/Sbuffer `a_ready` 默认可按场景打开。
2. `scenario_ctrl` 选择一笔或一组 mem 事务，调用 index allocator 分配 ROB/LQ/SQ，调用地址模型分配 vaddr/paddr/page_attr。
3. `lsqenq_builder` 只为普通 scalar load/store、非 segment vector load/store 生成 `lsqenq_agent_agent_xaction`。只有在 monitor 观察 `canAccept=1` 且 `resp_*` 与公共分配器一致后，才推进该事务到 `TXN_LSQ_ACCEPTED`。
4. `issue_builder` 根据事务类型生成 scalar `lintsissue` 或 vector `vecissue` item。普通 load/store 的 issue payload 必须引用已被 LSQ 接收的 idx；AMO、segment、FOF-fix-VL 走专用 scenario，不得伪造普通 `lsqenq` 接收。
5. `tlb_responder` 只根据 monitor 捕获到的 TLB/PTW 请求产生响应；响应 tag/asid/vmid/ppn/perm/fault 与 `addr_model` 一致。
6. `tl_responder` 只根据 DCache/Sbuffer monitor 捕获到的 A/C 请求产生 D/B 响应；source、size、sink、data、denied/corrupt 与 outstanding 表一致。
7. monitor 观察 writeback/feedback/LSQ deq/store debug 后更新 `uop_table`、`lq_state`、`sq_state` 和 `mem_model`。
8. commit/flush/redirect 由 `flush_state` 统一仲裁。任何被 kill 的事务不得再被 issue_builder 发起；已发出的环境响应若必须返回，也只能返回协议合法且不会重新激活已 kill 事务的响应。

## 各接口合法性规则

### LSQ Enqueue

- `needAlloc[1:0]` 是 LQ/SQ 分配 bit-mask：bit0 送 LoadQueue，bit1 送 StoreQueue。普通 load 和非 segment vector load 使用 `2'b01`，普通 store 和非 segment vector store 使用 `2'b10`。
- 参考 `Dispatch.scala`，`enqLsq.req.valid = fromRename.fire && !isAMO && !isSegment && !isfofFixVlUop`。因此 AMO、segment load/store、`isVleff && lastUop` 不能通过普通 `lsqenq_builder` 发有效请求，必须交给专用 scenario 处理。
- 初版公共生成器禁止 `2'b11`，避免同一 uop 同时占用 LQ/SQ；若后续发现某类合法指令确实需要双分配，应作为单独 scenario 显式打开并补齐回收规则。
- `req_i_valid=1` 时，`needAlloc_i` 至少有一位为 1；`needAlloc_i=0` 时对应 req payload 应保持安全默认值。
- 每周期最多 8 路 enqueue；初版建议只放开 1 路，待单事务闭环稳定后再增加多路。放开多路后，公共模型必须用 `numLsElem` 计算同周期后续端口的 LQ/SQ offset。
- 同周期多路请求的 `robIdx` 不能重复；占用多个 LQ/SQ entry 的 vector 请求会覆盖 `[idx, idx + numLsElem)` 连续区间，同周期不同请求的区间不能重叠。
- `fuType` 与 `needAlloc` 一致：bit0 置位时使用 `ldu/vldu`，bit1 置位时使用 `stu/vstu`。`vsegldu/vsegstu/mou` 不作为普通 enqueue 有效请求。
- `numLsElem` 直接参与 LQ/SQ 分配 offset 计算。scalar load/store 在 `enqLsq` 中按 1 个 LS element 处理；vector unit-stride 在 rename 阶段保守取 `VecMemUnitStrideMaxFlowNum=2`，非 unit-stride 由 `GenRealFlowNum(instType, emul, lmul, eew, sew)` 推导，当前合法范围为 1..16。
- LSQ 内部用 `lqIdx/sqIdx + numLsElem` 判断环形区间并可能跨 flag。公共分配器必须在分配时预留完整区间，不能只预留起始 idx。

### Scalar intIssue

- `intIssue_0/1/2` 是 load 口。必须携带 live `robIdx` 和 live `lqIdx`，`sqIdx` 使用发射时 older-store 快照。`src_0 + imm` 派生 load vaddr，并必须能在 `addr_model` 中找到一致 paddr。
- load 的 `waitForRobIdx` 只有在 `loadWaitBit=1` 时有效，且必须指向一个 live 且 older 的 ROB；若对应 ROB 被 redirect kill，等待关系要同步清除。
- load 的 `rfWen/fpWen` 只能选择一个或均为 0，普通整数 load 使用 `rfWen=1/fpWen=0`。
- `intIssue_3/4` 是 STA 口。必须携带 live `robIdx/sqIdx`，`fuType=stu` 或 `mou`，`src_0 + imm` 派生 store vaddr。`isFirstIssue=1` 只用于该 store 第一次 STA 发射，replay 重新发射时置 0。
- `intIssue_5/6` 是 STD 口。必须携带同一 store 的 `robIdx/sqIdx`，`src_0` 作为 store data 来源。STD 可与 STA 同周期或晚于 STA，不能早于 LSQ 分配。
- AMO 使用 `fuType=mou`，从 STA 类 issue 进入 `AtomicsUnit`，需要对应 store data 已经能被 `storeDataIn` 看到。AMO 会请求 flush_sbuffer 并复用 loadUnit(0) 的 TLB 端口；公共控制器应禁止并发发射新的 atomic，并等待 atomic TLB、DCache atomic 和 flush_sbuffer 完成。

### vecIssue

- `fuOpType[8:7] == 2'b01` 表示 vector load，`2'b10` 表示 vector store；其他编码不得作为 vector mem issue 发出。
- `vecIssue_0_0_bits_fuType` 必须与 `fuOpType` 一致：`vldu/vsegldu` 对应 load，`vstu/vsegstu` 对应 store。`vecIssue_1_0` 没有显式 `fuType`，不得用于 segment 首发。
- 非 segment vector load/store 可使用 0/1 两个口；segment 只能从 `issueVldu.head` 进入，公共模型应使用 0 口并保持 1 口空闲，直到 segment 事务完成或被 kill。
- 非 segment vector load 必须有 live `lqIdx`，非 segment vector store 必须有 live `sqIdx`；vector load 也必须携带 older-store `sqIdx` 快照。
- segment load/store 不走普通 `lsqenq`，由 `VSegmentUnit` 自己串行处理 TLB、cache/sbuffer 和 flush_sbuffer。公共模型必须把 `segment_active=1` 期间的 `vecIssue_1_0`、普通 Sbuffer flush 和 AMO flush 互斥。
- `vl > 0` 时要求 `vstart < vl`；`vl == 0` 场景可作为 no-op 覆盖，但不得期待 DCache/Sbuffer 请求。
- `sew8/sew16/sew32/sew64` 必须是 `vsew` 的 one-hot 解码。`veew`、`nf` 与 `fuOpType`、segment 类型一致；segment field 数为 `nf + 1`。
- `vm=1` 表示 mask 不屏蔽元素，`vmask` 可全 1；`vm=0` 时 `vmask` 和 `maskVecGen` 必须一致，不能让 `maskVecGen` 指向无效元素。
- `isVleff && lastUop` 在 dispatch 端不会发普通 `enqLsq.req.valid`，会进入 FOF/fix-VL 相关路径。初版建议作为独立 scenario 串行执行，不与普通 vector load/store 混发。
- `vuopIdx/lastUop/numLsElem` 必须描述同一条 vector 指令的拆分顺序。非最后 uop 不得提前释放 ROB/LQ/SQ；最后 uop 完成后才允许进入 commit 回收。

### TLB/PTW 和 CSR

- TLB/PTW 响应必须匹配 monitor 捕获到的请求 `vpn/s2xlate`，不能盲发。
- `satp/vsatp/hgatp_changed`、`priv_virt_changed` 或 `sfence_valid` 会触发 TLB flush；公共模型在 flush 窗口内不得对旧请求补发可 refill 的正常响应。
- 正常可访问 load 返回 `perm_r=1`，store/AMO 返回 `perm_w=1` 且 `perm_a/perm_d` 满足场景需要。故障场景用 `pf/af/gpf/gaf`，同时禁止继续生成对应 DCache/Sbuffer 数据响应。
- `addr_model` 中同一 `vaddr` 的 paddr/page_attr 在同一事务生命周期内必须稳定，除非该 scenario 明确测试 sfence/CSR 切换。

### DCache TileLink 响应

- `auto_inner_dcache_client_out_a`、`c`、`e` 是 DUT 输出，公共模型通过 monitor 捕获；driver 只驱动 `a_ready/c_ready/e_ready` 和 B/D 响应。
- 对每个 A fire 建立 `a_outstanding[source]`。在 D fire 前，不得复用同一 source 产生无关响应。
- D response 的 `source`、`size` 必须等于请求；读类请求返回 data 来自 `mem_model`，写类请求在响应接受后按 mask 更新 `mem_model`。
- D opcode 必须由 A/C opcode 派生：读类返回 AccessAckData，写类返回 AccessAck，Acquire/Release 类按 TL 协议返回 Grant/ReleaseAck。普通 UT baseline 不允许随机混用 opcode。
- 普通 baseline 禁用 B probe：`b_valid=0`。若开启 probe，B 地址必须对齐 cacheline，后续 C release 必须被 monitor 捕获并更新 `c_outstanding`。
- D `denied/corrupt` 只在 fault/error scenario 中开启，并要同步更新预期 writeback/exception，不得随机污染普通 load/store。
- 若 D response 带 sink，必须等待 DUT E channel fire 后释放 sink credit。

### Sbuffer TileLink 响应

- `auto_inner_buffers_out_a` 是 Sbuffer 对外写请求，公共模型通过 monitor 捕获；driver 只驱动 `a_ready` 和 D response。
- D response 必须匹配 A 请求的 `source/size`。普通 store writeback 使用 ack，不返回随机 data。
- `sbuffer_tl_state` 估计 occupancy 和 pending store。`a_ready=0` 用于 backpressure scenario，不能长期拉低导致 flush 永不完成，除非该 case 明确验证 timeout。
- Scala 中 `atomicsFlush = atomicsUnit.flush_sbuffer.valid || vSegmentUnit.flush_sbuffer.valid`，RTL assert 只禁止 `fenceFlush && atomicsFlush && cmoFlush` 三类同时有效，并没有强制 pairwise 互斥。公共生成器建议采用更严格的单来源 flush 策略：同一周期只主动制造 fence、atomic/segment、CMO 中的一类 flush，便于归因和清理 pending 事务。
- `sqempty/mshr_store_empty/force_write/sbuffer_timeout` 需要和 Sbuffer pending 状态一致：如果还有 pending D response，不能把 empty 类信号提前声明为完成。

### Redirect、Fence 和其他控制

- redirect 有效时，公共模型立即冻结新发 issue，先按 `robIdx` 年龄标记 killed，再决定是否继续完成已经发给外部 memory 的协议响应。
- `flushSb` 只由 fence 类场景或 commit 已允许的 flush 发起；不得和 atomic/cmo flush 同周期重叠。
- `sfence` 影响 TLB/PTW，不应同时随机改变无关 CSR 字段。`satp_changed` 类 CSR 变化需要与 `sfence` 共享同一 TLB flush 处理。
- `io_memInfo_sqFull/lqFull/dcacheMSHRFull` 是控制反馈类输入，若置 1，公共生成器应停止对应新事务分配或降低发射，不得继续强行填满队列。
- 错误输入 `dcacheError/uncacheError` 默认保持 0，只在 cache error scenario 中短脉冲打开，并同步期待错误上报。

## 跨接口依赖矩阵

| 依赖 | 规则 |
| --- | --- |
| `lsqenq -> intIssue/vecIssue` | issue 必须引用已分配并被 LSQ 接收的 `robIdx/lqIdx/sqIdx` |
| `robIdx -> redirect` | redirect 后 younger 事务不得继续生成新的 issue/TLB/TL 响应期望 |
| `lqIdx -> load wb/deq` | load 完成、异常或 kill 后才能释放 LQ |
| `sqIdx -> STA/STD/Sbuffer` | STA/STD 同一 sq；进入 Sbuffer 后等待 TL ack 或 flush drain |
| `vaddr -> TLB -> paddr` | issue 源地址、TLB PTE、DCache/Sbuffer paddr 必须同源 |
| `DCache A -> D` | D response 只能由已捕获 A 请求派生，source/size/data/opcode 一致 |
| `Sbuffer A -> D` | Sbuffer D ack 只能响应已捕获 A 请求 |
| `CSR/sfence -> TLB` | CSR changed 或 sfence 时清除旧 TLB outstanding/refill |
| `fence/atomic/cmo flush` | 同周期互斥，flush 期间禁止制造新的未受控 store drain |
| `vector split -> LSQ/Sbuffer` | `numLsElem/vuopIdx/lastUop/nf/vl/vstart` 必须描述同一 vector 指令 |
| `monitor FIFO -> TL responder` | DCache/Sbuffer responder 必须从 monitor transaction 建 outstanding，不能只看 driver 自身历史 |

## 规则生成器接口

公共 vseq 应提供一组稳定 API，scenario 只调用 API，不直接写 agent 字段：

| API | 输入 | 输出/副作用 |
| --- | --- | --- |
| `alloc_mem_txn(kind, flow_hint)` | `scalar_load/scalar_store/vload/vstore/amo/segment/fof_fixvl` | 分配 `txn_id/robIdx`，按 kind 分配或保留 LQ/SQ 上下文 |
| `bind_addr(txn_id, addr_attr)` | cacheable、mmio、alias、fault plan | 固定 `vaddr/paddr/page_attr`，建立 PTE 与 memory model |
| `emit_lsqenq(txn_id)` | 普通 load/store/vload/vstore | 生成 `lsqenq_agent_agent_xaction`，等待 monitor resp 后更新 idx |
| `emit_scalar_issue(txn_id, port)` | load/STA/STD/AMO | 生成 `lintsissue_agent_agent_xaction`，检查端口类型和 idx |
| `emit_vec_issue(txn_id, port)` | vload/vstore/segment/fof | 生成 `vecissue_agent_agent_xaction`，检查 vtype、mask、flow |
| `accept_tlb_req(req)` | monitor 捕获 vpn/s2xlate | 生成匹配 PTW/TLB response 或 fault |
| `accept_dcache_a(req)` | DCache A fire | 建立 `dcache_tl_state.a_outstanding[source]` |
| `accept_sbuffer_a(req)` | Sbuffer A fire | 建立 `sbuffer_tl_state.a_outstanding[source]` |
| `make_tl_d_resp(req_id)` | outstanding entry | 生成 D response item，并在 handshake 后更新 memory/sink 状态 |
| `apply_redirect(robIdx, level)` | redirect 输入 | 标记 killed txn、冻结新发、清理旧 epoch |

`*_builder` 输出的 xaction 应禁用或覆盖 agent xaction 中宽松的随机约束。尤其是 DCache/Sbuffer driver 的 `DRV_RAND` 模式只能用于 ready/backpressure，不能用于 B/D payload。

## 推荐 UVM 实现结构

建议新增以下公共层文件。sequence 类文件必须遵循 `mem_ut/ver/ut/memblock/rule/memblock_sequence_add_rule.md`：base sequence 和公共状态/helper 放入 `mem_ut/ver/ut/memblock/seq/base_seq`，virtual/scenario/responder sequence 放入 `mem_ut/ver/ut/memblock/seq/virtual_sequence`。非 sequence 的共享组件可继续放在 `common` 或 env 内部。

- `seq/base_seq/memblock_common_state.sv`：公共状态池，包含 ROB/LQ/SQ、地址模型、TL outstanding、CSR/flush/vector 状态。
- `seq/base_seq/memblock_idx_allocator.sv`：ROB/LQ/SQ 环形指针和年龄比较工具。
- `seq/base_seq/memblock_addr_model.sv`：vaddr/paddr/PTE/page_attr/memory_model。
- `seq/base_seq/memblock_vseq_base.sv`：公共 vseq 基类，读取 plus 参数并初始化公共控制状态。
- `seq/virtual_sequence/memblock_common_vseq.sv`：持有各 input agent sequencer handle，统一调度各 builder。
- `seq/virtual_sequence/memblock_lsqenq_builder.sv`、`seq/virtual_sequence/memblock_scalar_issue_builder.sv`、`seq/virtual_sequence/memblock_vec_issue_builder.sv`：把 `uop_table` 转为现有 `*_xaction`。
- `seq/virtual_sequence/memblock_tl_responder.sv`：监听 dcache/sbuffer monitor FIFO，按 outstanding 表发 B/D 响应 item。
- `seq/virtual_sequence/memblock_flush_ctrl.sv`：统一 redirect/fence/sfence/flushSb/CSR changed 的约束和状态更新。
- `seq/virtual_sequence/memblock_tl_monitor_bridge.sv`：若不直接修改现有 monitor，则增加桥接组件，从 interface 采样 TL fire 并写入公共 FIFO。

现有 `memblock_common_xaction` 只负责打包各 agent transaction，可保留为日志/scoreboard 容器；真正的合法性应放在公共状态池和 builder 中。

落地时还需要把 test 从每个 agent 的 `default_sequence` 独立驱动，切换到一个 `memblock_common_vseq` 统一启动。未接入公共 vseq 的 agent 默认只允许发送 idle 或安全 ready 信号。

公共 vseq、base sequence 和 transaction 默认控制参数可从 `plus.sv` 提取，例如 `max_inflight`、scenario mix、默认 response gap、backpressure/fault 使能。新增 plus 字段时必须同步 `env/plus.sv`、`seq/plus_cfg/default.cfg` 和 plus 迁移计划；环境组件开关仍归 `user_cfg.local.sv`。

## 场景模板

### `basic_scalar_load_hit`

1. 分配 ROB/LQ，SQ 快照取当前 SQ tail。
2. `lsqenq` 发送 load 分配，等待 resp。
3. `intIssue_0` 发送 load，`src_0 + imm = vaddr`。
4. TLB responder 返回 allow PTE。
5. 捕获 DCache A Get，按 `mem_model[paddr]` 返回 D AccessAckData。
6. monitor 检查 int writeback，再 commit 并释放 LQ/ROB。

### `basic_scalar_store_to_sbuffer`

1. 分配 ROB/SQ。
2. `lsqenq` 发送 store 分配。
3. `intIssue_3` 发 STA，`intIssue_5` 发 STD，二者共享 `robIdx/sqIdx`。
4. TLB responder 返回 store 可写 PTE。
5. 捕获 Sbuffer A Put，返回 D ack，更新 `mem_model`。
6. commit 后释放 SQ/ROB。

### `basic_amo_serial`

1. 创建 AMO 事务，分配 ROB 和 AMO 专用 `sqIdx` 上下文，但不发送普通 `lsqenq`。
2. 通过 STA 类 issue 口发 `fuType=mou`，同时保证 STD/store data 已准备。
3. 等待 AtomicsUnit 发起 flush_sbuffer；公共模型保持普通 store/vector/segment 不再进入 Sbuffer。
4. TLB responder 返回 AMO 地址翻译，DCache responder 响应 atomic 请求。
5. 观察 atomic writeback/feedback 后释放 ROB，并清除 atomic flush 状态。

### `store_then_load_forward`

1. 先分配并发出同地址 store，保持 SQ live。
2. 再分配 younger load，load 的 `sqIdx` 使用该 store 的 SQ 快照。
3. 若 DUT 走 forward，则不应期待外部 DCache Get；若 DUT replay，则等待反馈后按 replay 规则重新发 load。
4. 最终 load 数据必须等于 store mask/data 合成结果。

### `vector_unit_stride_load`

1. 分配 ROB/LQ，设置 `fuType=vldu`、`fuOpType=VlduType.vle`、合法 `vl/vstart/vsew/vlmul/vmask`。
2. `numLsElem` 由 vector split 模型派生，`vuopIdx` 从 0 递增，最后一个 uop 置 `lastUop=1`。
3. 捕获一组 DCache A Get 并按 element/line 返回数据。
4. 直到最后 uop merge/writeback 后释放 LQ/ROB。

### `vector_store_or_segment`

1. 普通 vector store 使用 `fuType=vstu`、`fuOpType=VstuType.vse/vsse/vsuxe/vsoxe`，分配 ROB/SQ。
2. segment 使用 `fuType=vsegldu/vsegstu`，不发送普通 `lsqenq`，只从 `vecIssue_0_0` 发，`nf` 表示 field 数减 1，期间禁止 `vecIssue_1_0` 混发 segment。
3. vector store 最终应进入 Sbuffer A，D ack 后更新 `mem_model`。

### `redirect_kill`

1. 构造多笔 live ROB，至少包含已 enq 未 issue、已 issue 未响应、已进入 TL outstanding 三类。
2. 发 redirect 到中间 ROB。
3. 公共模型标记 younger killed，禁止其新 issue；对已发 TL 请求只完成协议必要响应，不再期待正常 writeback。
4. older 事务继续完成并 commit。

## 约束优先级

1. 先保证索引一致：`robIdx/lqIdx/sqIdx/uopIdx/numLsElem` 由公共分配器产生。
2. 再保证地址一致：issue 源地址、TLB 响应、DCache/Sbuffer TL 地址都来自同一 `addr_model`。
3. 再保证协议一致：DCache/Sbuffer D response 必须匹配实际 A/C 请求。
4. 最后叠加扰动：backpressure、fault、replay、redirect、flush、probe、uncache、CSR changed。

普通随机字段如果无法由上述模型解释，应固定为安全默认值，而不是独立 randomize。

## 分阶段落地建议

1. 第一阶段只支持 `basic_scalar_load_hit`、`basic_scalar_store_to_sbuffer`，单周期单事务，B probe 关闭，fault/redirect 关闭。
2. 第二阶段加入 store-load forward、DCache/Sbuffer backpressure、TL latency 随机化。
3. 第三阶段加入 vector unit-stride load/store，先禁用 segment/FOF。
4. 第四阶段加入 segment、FOF、AMO、sfence、CSR changed、redirect kill。
5. 第五阶段放开多路 enqueue、多 issue 口并发、多 outstanding source 和 probe。

每阶段验收标准都是：所有输入均由公共状态变量解释，monitor 捕获的输出可以回写公共状态，不能出现某个 agent 独立随机导致其它接口无法匹配的事务。
