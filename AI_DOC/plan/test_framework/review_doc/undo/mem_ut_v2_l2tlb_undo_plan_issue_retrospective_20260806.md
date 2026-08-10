# V2 L2TLB Undo Plan 问题回溯（2026-08-06）

范围：本回溯只审查 `L2TLB_agent` 作为 DTLB 上游 responder 时，是否能稳定接收 DTLB request、冻结并驱动
L2TLB response、在 flush 后停止旧回包并为后续 request 建立正确新 payload。它不把 L2TLB 当作
L2Cache/PTW/memory 下游模型，也不评价 DUT 的 checker、scoreboard 或 coverage。

关联 plan：

- `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2tlb_response_random_payload_plan_20260729.md`
- `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2tlb_sfence_flush_token_timing_correction_plan_20260805.md`
- `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_l2tlb_range_lookup_napot_plan_20260806.md`
- `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_sfence_hfence_stage_aware_live_entry_invalidation_plan_20260804.md`

## 术语与抽象功能说明

| 术语 | 当前文档中的中文含义 | 代码/计划落点 | 使用场景 |
|---|---|---|---|
| responder | 测试框架替代 L2TLB，向上游 DTLB 返回翻译 response 的唯一 sequence。 | `memblock_l2tlb_base_sequence` | 它接收 DTLB request，不访问 L2Cache 或 memory。 |
| request fire | `req_valid && req_ready` 已在一个 DUT sample 真正握手。 | `capture_fired_request()` | C0 同拍看到 fence 后，该 request 已被 DUT 接收。 |
| token | 一次 request fire 对应的独立 pending response 账本。 | `pending_q`、`driving_req` | 两个相同 key request 也必须有两个 token。 |
| flush barrier | 从顶层 monitor 观察到 event 到 DTLB filter 实际清空之间的延迟记录。 | `barrier_q` | C0 event 在 V2 的实际边界是 C4。 |
| live entry | 测试框架保存一份 canonical raw response payload 的表项。 | `tlb_entry_by_key` | fence 删除后，下一次 request 才能重新随机。 |
| range index | 由映射形状反查 live entry anchor 候选的辅助表。 | 最终为 `tlb_anchor_keys_by_range_key`；`tlb_anchor_key_by_range_key` 仅是旧单值设计名 | exact miss 时避免扫描完整 live table。 |
| raw hit | DUT `PtwRespS2.hit()` 对 response 与请求 VPN/GVPN 的内容匹配。 | `PtwRespS2.hit()`、`entry_matches_request_raw()` | 决定一个 range entry 能否复用给当前 request。 |
| raw protocol GVPN anchor | allStage 构造期由 S1 raw payload 和 VPN 按 DUT 原始拼接规则得到的 S2 tag 来源；它不是模型 normal PPN。 | `derive_allstage_raw_s2_tag()`、`entry.s2_tag` | 非 LEGAL 非规范 NAPOT 仍需驱动稳定 `s2_tag`，但 derived PPN 可以保持 invalid。 |
| dispatch-active | dispatch service 已启动、允许建立 live entry 的运行态。 | `dispatch_l2tlb_lookup_active=1`、`dispatch_monitor_event_adapter` | raw fence 入 FIFO 并由 adapter 在 C4 删除命中的 entry。 |
| no-dispatch | testcase 没有 dispatch service 的固定拓扑。 | `dispatch_l2tlb_lookup_active=0` | 合同保证 `req.fire=0`、`tlb_entry_by_key` 为空，raw fence 直接丢弃。 |
| adapter raw-fence owner | dispatch-active 下唯一可 destructive pop/decode/schedule/apply raw fence 的组件。 | `dispatch_monitor_event_adapter` | L2TLB response sequence 只处理 token/UID barrier，不读取 raw FIFO。 |
| UID waiting instance | 一个 logical UID 在本次真实 issue 后等待 TLB payload 的独立实例。 | `uid_tlb_wait_epoch`、`uid_tlb_wait_state` | C4 只取消已观察到 pre-anchor request fire 的旧实例；真 reissue 才建立新 epoch。 |
| UID request-fire marker | 当前 UID 等待实例首次被 responder 观察到对应 DTLB request fire 的 DUT global sample；0 表示未观察。 | `uid_tlb_first_request_fire_sample_seq`、`uid_waiting_by_vpn_s2xlate` | marker=0 的新等待不能被旧 C4 barrier 误取消。 |
| response-to-UID multicast | 一笔已经 external fire 的 response 按 DUT raw hit 同时回填零个、一个或多个等待实例。 | `complete_waiting_uid_records_by_response()` | token 仍一笔一计账，UID 不与 token 一对一绑定。 |
| response-visible CSR | response fire 当前 sample 中 `PTWNewFilter` 实际读取的 top CSR C-2 history。 | `entry_matches_uid_at_response()` | UID issue-time CSR 只留历史，不能代替本次 raw-hit CSR。 |
| DUT global sample | 由唯一 CSR monitor 在每个 post-reset `posedge mon_cb` 推进一次的 testcase 内单调周期编号。 | `advance_dut_global_sample()`、CSR history、barrier due、raw fence、UID wait timestamp | 同一 DUT cycle 的 posedge monitor 和 negedge service 都读取同一个编号，以正确识别 C-2/C4。 |
| sample anchor/probe | driver 在当前 `drv_cb` 内用本拍 clocking-block 时间等待 CSR monitor 建立同拍 sample，并在 anchor 成功后才读取 current sample 的有限检查；不跨到下一 clock。 | `wait_for_l2tlb_sample_anchor()`、`wait_for_dut_sample_ready_at_drv_cb()` | probe 未完成时只送 inactive，不用上一拍 sample 解释当前 VIF。 |
| metadata latch | driver 对上一拍已驱动 item 的 kind、generation、reset epoch、baseline tag 和真实 ready/fire/resp_valid 的轻量快照，不是 UVM item 句柄。 | `last_driven_*` fields | 上一拍 item 已 `item_done()` 后仍可用 latch 做 final/stop 判断，但不能再次 `item_done()`。 |
| sample producer barrier | CSR/fence monitor 都已报告当前 sample 已完成采样；无事件也必须报告完成。 | `sample_producer_done_mask`、`lifecycle_event_published_seq` | 两个 producer 收齐后才允许 response owner 解释本拍 event；无 reason 时只发布 watermark。 |
| reset coordinator | 统一建立 `reset_epoch`、按 topology 收集 CSR/fence monitor/L2TLB monitor/response/adapter reset ack 的共享协调者。 | `reset_l2tlb_runtime_state()`、`reset_required_ack_mask` | 所有支持 topology 固定包含 CSR/FENCE/MONITOR；`NO_OWNER` 仍产生三者 ack，但不 re-arm responder。 |
| reset ack tuple | MONITOR 对一次 reset 返回的 `{reset_epoch, transport_sample_seq}`；seq 必须来自本次同步处理的 reset-active sample，并严格晚于上次 MONITOR reset ack。 | `l2tlb_monitor_reset_ack_epoch/transport_sample_seq` | 该 tuple 与 final-inactive settled 的 epoch/seq 分开维护，不能互相代替。 |
| response history cursor | response owner 已按 event_seq 消费到的位置；adapter 不维护该 cursor。 | `response_owner_event_cursor` | service 停顿后从 cursor+1 按原始 sample 回放迟到 event。 |
| sample TLM wrapper | agent 内部传递已冻结 transport sample 的私有 `uvm_object` 包装；consumer 只可取副本。 | `L2tlb_agent_agent_transport_sample` | driver 预留同一 wrapper 到单槽 mailbox 后同步调用 monitor `analysis_imp.write()`。 |
| sample mailbox | driver 与唯一 response owner 的单槽 sample 传输状态机。 | `EMPTY -> PUBLISHED -> CONSUMED/DROPPED -> EMPTY` | terminal 状态由 owner ack，driver 在后续真实 `drv_cb` 回收；不是 monitor FIFO。 |
| direct writer | 一个 runtime state 的唯一直接清理和 ack/proof 写者；reset coordinator 只请求和等待。 | response owner、driver、fence monitor、adapter、CSR monitor、L2TLB monitor | coordinator 不能代替 driver 清 mailbox 或代替 adapter 清 raw fence。 |
| reset-quiescent | driver 对指定 reset epoch 已完成 stale item/slot 清理并已回 RESPONSE ack 后的保持态。 | `L2tlb_agent_agent_driver` 本地状态 | 保持 inactive/reset sample，不重复发布 semantic mailbox 或重复 ack；reset release 后由 baseline 重新打开。 |
| final inactive item | driver 在真实 `drv_cb` 对匹配 `RELEASE_FINAL_INACTIVE`、current owner/generation/reset epoch 的 item 冻结，且 `sampled_req_ready=0 && sampled_req_fire=0 && sampled_resp_valid=0` 才完成；sequence 创建或 `finish_item()` 返回不等于完成。 | `final_inactive_item_done` | 任一 metadata 或采样条件不满足均 fatal；完成后才能进入 release closing。 |
| final mailbox recycle proof | final sample 已被唯一 owner 终态确认后，driver 在下一真实 `drv_cb` 回收该 slot 的证明；它不是 monitor settled，也不是 sequence ack。 | `l2tlb_release_final_inactive_transport_sample_seq`、`l2tlb_transport_sample_recycle_done_seq` | release gate 要求二者相等且 mailbox EMPTY；没有下一 item 也必须完成这次回收。 |
| release closing/grant（历史字段摘要） | admission-close 后由 owner 记录 closing 的状态，以及 parent 对已收敛 owner 发放的一次释放授权。 | `l2tlb_release_admission_closed`、`l2tlb_lifecycle_release_closing`、`l2tlb_release_granted` | 本行保留历史用词；当前代码落点只允许 `l2tlb_release_closing`，且 release 还必须等待 final mailbox recycle proof。 |
| release-grantable | parent 发 grant 与 owner 清 claim 前共用的只读 release 前置条件。 | `release_grantable(owner, reset_epoch)` | 它同时检查 final inactive/monitor settled、final mailbox recycle proof/mailbox EMPTY、closing、response/adapter/raw-intake drain、当前 reset epoch ack 与非 reset 状态。 |
| final sample terminal ack | final inactive item 跨过 driver 采样后产生的 frozen mailbox sample 必须由 owner 先建立 closing、再终态确认。 | `sampled_final_inactive_proof_*`、`ack_l2tlb_transport_sample()` | final proof 并不自动清 slot；owner 在 F ack，driver 在 F+1 recycle。 |
| release-state wakeup | final ack 后 owner 等待 parent grant 或 runtime reset 的 package 级 `uvm_event`；它不保存 lifecycle 真值。 | `l2tlb_release_state_changed_ev`、`wait_for_l2tlb_release_grant_or_reset()` | parent 原子写 grant 后 trigger，reset coordinator 发布新 epoch 后 trigger；owner 醒来后重新核对 state。 |

#### 审核修改意见（迟到 event 处理收紧）

上方 `response history cursor` 和 `record_l2tlb_flush_barrier()` 保留的是早期“service 停顿后可回放迟到 event”的历史描述，
不能作为当前 coding 规则。单 owner 审核后固定如下：既有 `acceptance_opened_since_reset==0`，且本拍与既有状态都没有
ready/fire/response、token、driving response、barrier/hold 或 `WAITING` UID 时，旧 event 只可作为 pre-ready baseline 推进 cursor/last-seen，
并从 current sample 起设置 4 拍 `pre_ready_hold_until_sample`；它不建立 C0/C4 cancel。一旦 acceptance 已开放，
`event.sample_seq < current_sample` 必须在任何 barrier/token/UID 修改前 `uvm_fatal`，`event.sample_seq > current_sample` 同样 fatal。
当前 sample 的 event 才能按其原始 C0 建 barrier。该修正记录在
`mem_ut_v2_l2tlb_single_owner_lifecycle_optimization_review_20260807.md` 与 timing correction `undo` plan，后续 coding 以二者为准。
response owner 在本拍语义结算后是唯一可回收 `event_seq <= response_owner_event_cursor` history 前缀的写者；CSR/fence producer
只 append/merge，队列满且没有已消费前缀时必须 fatal，`last_allocated_l2tlb_event_seq` 不回绕。

#### 最终 release 术语补充

本表只补充后续审核确立的规范含义，不删除上方历史术语或历史字段名。后续 coding 以本表和文末审核补充为准。

| 术语 | 当前文档中的中文含义 | 代码/计划落点 | 使用场景 |
|---|---|---|---|
| `admission-settled watermark` | owner 已完成一个真实 `drv_cb` sample 的 request capture 与 UID registration 的可检查证明。 | `l2tlb_owner_admission_settled_sample_seq` | parent 的 negedge global stop 只停止 routing；owner 先处理此前 ready 窗口的 fire，再据此封口。 |
| `close_requested` | owner 在已结算当前 sample admission 后写入的软件 admission 封口；parent 不直接写。 | `l2tlb_release_admission_close_requested` | 写入后任一后续 UID registration/token capture helper 都必须 fatal，即使仍在同一 sample。 |
| `close_generation` | owner 每次 close request 的 testcase 内单调编号；runtime reset 作废当前请求但不回绕该编号。 | `l2tlb_release_admission_close_generation`、item/grant generation | 防止 reset 前的 stop/final item 误匹配 reset 后的 release。 |
| `close request reset epoch` | close request 绑定的 runtime reset epoch。 | `l2tlb_release_admission_close_reset_epoch` | parent 只等待等于 current epoch 的 close，不能复用 reset 前 close。 |
| `RELEASE_STOP` | owner 为已确认 close request 送出的本地 stop metadata；该 item 必须实际驱动 `ready=0`。 | `l2tlb_release_item_kind`、`l2tlb_release_item_generation` | driver 以匹配 owner/generation 且 `sampled_req_fire=0` 的真实 `drv_cb` sample 确认 transport close。 |
| `admission_closed/cutoff` | 已匹配 `RELEASE_STOP` 跨过真实 sample 后得到的 DUT transport 关闭确认及其 sample 编号。 | `l2tlb_release_admission_closed`、`l2tlb_release_admission_closed_generation`、`l2tlb_release_admission_cutoff_sample_seq` | 它允许已建立 token/UID 继续 drain；不承担软件 admission 的首次封口；同 generation 第二次 stop 必须 fatal。 |
| `raw-fence intake close` | fence monitor 已处理 close request 后的一个完整 raw sample，且后续不再接收有效 raw fence 的证明。 | `l2tlb_raw_fence_producer_settled_sample_seq`、`l2tlb_raw_fence_intake_closed_*` | parent 既要等 adapter queue drain，也要等当前 epoch/generation 的 intake close，避免 grant 后出现 C4 delete。 |
| `RELEASE_FINAL_INACTIVE` | response drain 后的独立 final item metadata；必须匹配 current owner/generation/reset epoch，且真实 sample 的 `sampled_req_ready=0 && sampled_req_fire=0 && sampled_resp_valid=0`。 | `l2tlb_release_item_kind`、`l2tlb_release_final_inactive_item_done` | 只有 driver 真实采样全部谓词后，owner 才可写 canonical `l2tlb_release_closing`。 |
| `post-reset baseline` | reset release 后当前 epoch 先驱动并在 `baseline_sent_sample_seq` 之后真实采样的 `NORMAL/inactive` item。 | driver local `post_reset_baseline_pending/baseline_sent_sample_seq`、transport sample 的 baseline tag、`l2tlb_post_reset_baseline_done_epoch/sample_seq` | sequence 只消费 driver 发布的 baseline-required/proof-pending；baseline 尚未完成时，当前 epoch 的 stop/final item 不得创建或确认。 |
| `item_done()` transport release | 已由 `try_next_item()` 返回的 UVM item 的唯一 sequencer 释放动作。 | `seq_item_port.item_done()` | stale item 要先执行一次再丢弃；未取得 item 的 NO_ITEM/abort 路径不调用它。 |
| canonical `release closing` | owner 已完成 final inactive、但尚未清 claim 的短状态；历史 `l2tlb_lifecycle_release_closing` 不是第二字段。 | `l2tlb_release_closing`、owner/generation | parent 发 grant 和 owner 原子清 claim 前的共同条件之一。 |

#### 审核修改意见（历史 release 字段标识）

上方“release closing/grant（历史字段摘要）”中的 `l2tlb_lifecycle_release_closing` 保留用于描述旧方案，不能作为当前代码落点；
当前唯一规范字段是 `l2tlb_release_closing`。同理，上方旧 release-grantable 表述中的“双 drain”只表示历史摘要，当前
release gate 还必须检查 raw-fence intake close、精确 final sample settled、final mailbox recycle proof 和 mailbox EMPTY。
后续 coding 只读取本文后续补充、单 Owner 审核稿和 timing correction plan 的 canonical state table。

#### 审核修改意见（grant 判定与 closing/recycle 顺序）

上方 `release-grantable` 和 `begin_l2tlb_release_closing()` 的简写不应被理解为当前可执行时序。当前 canonical 规则为：
`release_grantable(owner, epoch)` 只读取 admission、final/monitor、mailbox recycle/EMPTY、drain/intake、required ack 和非 reset
条件，**不得读取** `release_granted`、grant owner、grant epoch 或 grant generation；parent 先在该谓词为真时写入匹配
owner/epoch/generation 的 grant，owner 仅在“匹配 grant 且同一谓词仍为真”时清 claim。

closing 的顺序固定为：driver 写 final proof -> monitor 同步 settled -> owner 写 `l2tlb_release_closing` -> 下一真实
`drv_cb` driver 回收 final mailbox 并写 recycle proof -> parent 以 closing 与 recycle/EMPTY 都成立的 `release_grantable()` 发 grant。
因此 closing 不等待 recycle；但 release grant/claim 清除必须等待 recycle。该审核意见覆盖上方“final mailbox 已回收后写 closing”的历史简写，
后续 coding 以 timing correction plan 和单 owner review 为准。

### 关键函数抽象功能

| 函数/helper | 在本文中的抽象职责 |
|---|---|
| `advance_dut_global_sample()` | 由 CSR monitor 每个 post-reset posedge 唯一推进 global sample，并发布该 sample 的 CSR history；不修改 token、UID 或 live entry。 |
| `mark_l2tlb_sample_producer_done()` | 登记 CSR/fence producer 已完成当前 sample；required mask 收齐后发布 event-ready watermark，不推进 sample、不消费 raw FIFO。 |
| `record_l2tlb_flush_barrier()` | 以 event 原始 sample 建立 C0/C4 barrier；迟到 event 仍用原 anchor，不把当前 sample当新 anchor。 |
| `apply_due_l2tlb_flush_barriers()` | 在 due sample 取消仍 pending 的旧 token/符合 marker 的 UID；不替 adapter 删除 live entry。 |
| `wait_for_dut_sample_ready_at_drv_cb()` | driver 在单个真实 `drv_cb` 内完成有界 sample anchor/probe，返回 `READY/NOT_READY`；它不跨 clock 等待，也不推进 global sample。 |
| `L2tlb_agent_agent_monitor::write()` / `write_transport_sample()` | monitor 的同步 `analysis_imp` 入口；只消费 driver freeze 的 wrapper 副本并输出既有观测，不读 VIF、不改 mailbox、不重算 fire。 |
| `ack_l2tlb_transport_sample()` | 唯一 response owner 对已处理或 reset/abort 丢弃的 PUBLISHED sample 做一次 CONSUMED/DROPPED CAS；它不清 payload，driver 在后续 `drv_cb` 才回收 EMPTY。 |
| `reserve_and_publish_l2tlb_transport_sample()` / `recycle_l2tlb_transport_sample_at_drv_cb()` | driver 先原子预留 PUBLISHED wrapper、同步通知 monitor，后者在每个后续 `drv_cb` 回收 terminal slot；final slot 回收时写 recycle proof，即使没有下一 item 也不能跳过。 |
| `get_owned_item_or_abort()` | driver 每个 `drv_cb` 通过 `try_next_item()` 非阻塞轮询；NO_ITEM 仍驱动 inactive/publish sample，保证 reset-active sample、MONITOR ack 和 final recycle 有唤醒来源。 |
| `close_l2tlb_admission_for_release()` / `confirm_l2tlb_admission_closed_at_drv_cb()` | 前者仅由 owner 在 READY sample 已 capture 后写 close request/stop item，后者仅由 driver 在匹配 stop sample 写 transport cutoff；parent 不写 close。 |
| `begin_l2tlb_release_closing()` | owner 只在 final sample 已由 driver/monitor 确认且 final mailbox 已回收后写 closing；不清 claim、不发 grant。 |
| `reset_l2tlb_runtime_state()` | 按 reset epoch 和 ack mask 协调各职责清理旧状态；所有支持 topology 都完成 FENCE/CSR/MONITOR ack，ENABLED 再按 mask 完成 RESPONSE/ADAPTER，不启动第二个 responder。 |
| `mark_l2tlb_final_inactive_at_drv_cb()` | driver 在真实 `drv_cb` 对匹配 `RELEASE_FINAL_INACTIVE`/owner/generation/reset epoch 的 final item 冻结 `sampled_req_ready/fire/resp_valid`；只有三项均为 0 才写 final done，否则 fatal。 |
| `release_grantable()` | 只读复核 ENABLED owner 是否可获得/消费最终 release grant；要求 final settled、final mailbox recycle proof、mailbox EMPTY、drain/intake/ack 等条件，不写 grant、不清 claim。 |
| `release_l2tlb_lifecycle_owner()` | 在 final inactive、closing、grant、final mailbox 已回收、双 drain、active claim、global stop 和 `!reset_active` 均满足时清 claim；失败保持 claim。 |

#### 审核修改意见（关键函数表的 closing/recycle 顺序）

上方函数表中 `begin_l2tlb_release_closing()` 写成“final mailbox 已回收后写 closing”是早期摘要，不能作为 coding 条件。
当前顺序固定为：driver final proof -> monitor settled -> owner 写 `l2tlb_release_closing` -> 下一真实 `drv_cb` driver 回收 final mailbox。
因此 `begin_l2tlb_release_closing()` 只等待 final proof 与 monitor settled；recycle proof/mailbox `EMPTY` 只由随后 parent/owner 复核
`release_grantable()` 时要求。唯一可 coding 的详细伪代码位于
`AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2tlb_sfence_flush_token_timing_correction_plan_20260805.md` 的
`begin_l2tlb_release_closing()`、`release_grantable()` 与 normal body exit 小节；本意见与第 86-96 行的 canonical 时序一致，覆盖表中该行旧表述。

## 审查准则

本轮仅把下列情况认定为必须修复的问题：

1. 会使 responder 漏掉真实 request fire、错误回填 UID、或在 DUT 已 flush 后继续回复旧 token。
2. 会使 fence 后的新 request 复用已经失效的 live payload。
3. 会使测试框架的 range reuse 与 DUT `PtwRespS2.hit()` 不同，导致 response 不能命中或错误命中 DTLB request。
4. 会让同一计划组对同一字段或生命周期给出互相不能同时实现的规则。

不把“未实现 DCache owner”“未建立最终 PAddr”“没有 RM/checker/coverage”作为本专项 blocker；这些不属于
仅提供 L2TLB response 的 responder 职责。

## 问题回溯

### 1. C0 同拍 flush 的 token 不能被立即当作 killed

问题是什么：payload/range plan 仍保留“同拍 flush fire 被立即取消”或 `record_flush_killed_request()` 的描述，
而 timing correction plan 已规定 V2 DTLB filter 在 C4 才真正 flush。原 `select_due_response()` 还以
`accept_flush_event_seq == last_seen_flush_event_seq` 判断 token 是否陈旧，会在 C0 新 event 出现后错误拒绝所有旧 token。

怎么理解：C0 的 ready 来自上一拍 driver item；若 C0 同时看到 `req_valid=1` 和 fence，该 request 已进入 DUT
filter。C0 直接把它删掉，就相当于测试框架假装 DUT 没接收它。正确行为是 C0 建 token，C1-C3 仍允许它返回 response；
C4 时仅取消还没有完成的 pending token。仍 `WAITING` 的 UID instance 也不能在 C0 先取消，否则 C0 的真实 request
无法保留自己的等待上下文。

为什么必须修复：这会漏掉 DUT 已接受的 request，或让仍在等待的 UID instance 在 response 到达时被提前取消；两种情况都会让
“只由 L2TLB response 驱动 memblock”失去 request/response 对应关系。

解法：

1. `record_l2tlb_flush_barrier()` 只记录 C0 barrier 并关闭后续 ready，不删除 token 或 UID waiting instance。
2. C0 fire 用观察新 event 前保存的 `fire_visible_event_seq` 正常 capture，并保留已经建立的 UID `WAITING` instance。
3. `apply_due_l2tlb_flush_barriers()` 在 C4 取消仍在 `pending_q` 的旧 token；删除前调用
   `cancel_waiting_uid_records_for_flush()`。
4. 同一 C4 barrier 只取消仍为 `WAITING`、且 `uid_tlb_first_request_fire_sample_seq != 0` 并不晚于 barrier anchor 的
   旧 UID instance；已 `COMPLETED` instance 保持历史，marker=0 的等待不被该 barrier 误取消，新 response只能回填
   真正 reissue 后的新 epoch。
5. `select_due_response()` 改为按 barrier 的 due sample 判断 token 能否在真正 flush 前完成，删除“与 latest event
   序号不相等即 fatal”的判断。

已回写：random payload、range lookup 和 timing correction 三份 plan。

#### 审核修改意见（单 owner 最终合同）

单 owner 只规定 response lifecycle 的唯一运行期写者，不改变 C0/C4 的 DUT 时序：C0 同拍真实
`req_valid && req_ready` 仍建立独立 token，C1-C3 允许完成，C4 才取消未完成旧 token。不得因为只有一个 owner
而把 C0 fire 合并、延迟建 token，或由 owner release 清理 C4 以前已经真实进入 DUT 的 request。

### 2. raw fence 的消费者必须按 dispatch 拓扑固定

问题是什么（早期泛化方案）：为了让 standalone responder 也能处理 fence，早期方案把 `raw_sfence_q` 的消费权迁给
`memblock_l2tlb_base_sequence`，并引入 owner inactive gap、high-watermark 与启动 reconcile。这个方案把 response
token/UID 生命周期与 dispatch live-entry 生命周期重新耦合，而且与当前 testcase 合同不相符。

怎么理解：当前合同已经保证：

```text
dispatch_monitor_capture_en = 1
no-dispatch -> 不会出现 dispatch -> L2TLB 的 request fire
no-dispatch -> 不创建 tlb_entry_by_key
```

因此 no-dispatch 中不存在“已有 live entry 等待 fence 删除”的对象。即使 monitor 采到 fence，也没有旧 payload 能在以后
被该 testcase 复用；让 standalone responder 保存、解析或延迟消费 raw fence，只会创建没有消费者的额外生命周期。

为什么必须修复：若仍让 L2TLB sequence drain raw FIFO，L2TLB owner release 会错误依赖 adapter 的 queue，且后续
dispatch-active 路径可能出现两个 destructive consumer。反过来，若 no-dispatch 仍允许 raw fence 入 FIFO，就会留下
没有 live entry、也没有必要 C4 delete 的悬挂工作。

源码复核还发现一条更具体的旧直通路径：
`memblock_main_dispatch_auto_build_main_table_base_sequence::service_monitor_once()` 调用
`memblock_dispatch_base_sequence::collect_runtime_context_events()`，后者原来在同步 CSR 后继续调用
`dispatch_monitor_event_adapter::drain_sfence_events()`；该 helper 直接 `pop_raw_sfence()` 并调用
`common_data_transaction::apply_raw_sfence()`，后者再调用立即删除的 `apply_sfence_invalidate()`，在 C0 立即删表，
绕过 C4。只在职责表中写“adapter 是唯一 consumer”而不删除/收敛这些调用，实际 coding 仍会保留第二条 destructive path。

最终解法：

1. `dispatch_monitor_capture_en=1` 保持为 monitor 采样开关，不能作为 raw FIFO 入队或 consumer gate。
2. 由 testcase/dispatch coordinator 在 service 启动前设置固定的 `dispatch_l2tlb_lookup_active`：dispatch testcase 为 1，
   standalone/no-dispatch 为 0；它不是 plusarg、connect takeover 或 response owner 状态。仅其为 1 时
   `push_raw_sfence()` 才保留 FIFO item。
3. dispatch-active 时，`dispatch_monitor_event_adapter` 是唯一 `peek -> decode -> schedule -> pop` raw fence 的组件，并在
   raw 自己的 C0 `sample_seq` 对应的 C4 调用 `apply_due_sfence_invalidate()`。
   `collect_runtime_context_events()` 只调用 `drain_csr_events()`；`service_monitor_once()` 每个 sample 恰好一次调用
   `service_l2tlb_sfence_events()`。旧 `drain_sfence_events()`、`apply_raw_sfence()` 与直接调用的
   `apply_sfence_invalidate()` 必须删除或不可达。
4. no-dispatch 时 `dispatch_l2tlb_lookup_active=0`；raw fence 直接丢弃，`raw_sfence_q` 与
   `sfence_invalidate_pending_q` 必须保持为空。L2TLB sequence 不实现 raw matcher、FIFO drain、C4 live-entry delete 或
   high-watermark reconcile；若它观察到 `req_valid && req_ready`，立即 `uvm_fatal`，不建立 token 或 live entry。
5. 若未来 testcase 明确允许 `dispatch -> no-dispatch -> dispatch` 切换，切换前必须显式清空 raw FIFO、pending invalidate
   queue，并调用 `clear_dispatch_l2tlb_live_entries()`；当前固定拓扑不实现 owner handoff fallback。

已回写：SFENCE/HFENCE stage-aware live-entry plan 与 token timing plan；no-dispatch directed 场景改为断言
`req.fire=0`、`tlb_entry_by_key` 为空且 raw fence 不入 FIFO。

复查结论：raw fence 的唯一 destructive consumer 是 dispatch adapter，不是 L2TLB response lifecycle owner。response
sequence 仍独占 token、response 和 UID cancel；adapter 独占 live entry/range index 的 C4 删除。两条 flow 共用 frozen
anchor/due，但不互扫 queue，也不依赖彼此的 service 调用顺序。

#### 审核修改意见（单 owner 拓扑）

本计划组固定为“一个 testcase 只启动一个 L2TLB responder sequence，并只 claim 一次 lifecycle owner”。因此不再允许
由第二个 responder 接管 `raw_sfence_q`、token、UID 或 live entry。这里的单 owner 只收缩 response sequence 的运行期
所有权，不把多个真实 request fire 合并为一个 token。

最终 consumer 责任保持如下：

```text
dispatch-active：
  fence/CSR monitor
    -> 用同一个 DUT global sample 发布 lifecycle event
    -> raw fence 仅在 dispatch_l2tlb_lookup_active=1 时入 raw_sfence_q
  dispatch_monitor_event_adapter
    -> 唯一 peek/decode/schedule/pop raw fence
    -> C4 删除 live entry 与 range index
  唯一 L2TLB responder owner
    -> 唯一维护 token、driving response、barrier 与 UID cancel
    -> 不读取或清理 raw_sfence_q / sfence_invalidate_pending_q

no-dispatch：
  raw fence 不入 FIFO；req.fire、live entry、adapter pending delete 均必须为 0。
```

本轮禁止 testcase 内 `dispatch -> no-dispatch -> dispatch` 切换。原文中“未来可切换拓扑”的描述只保留为后续专项背景，
不是本计划组的 directed 或 coding 路径。global stop 也不能仅等待 response owner：父 flow 必须同时等待 adapter 的
`raw_sfence_q`、待绑定 raw context 和 `sfence_invalidate_pending_q` 清零；但 response owner 不得为此 pop 或扫描 adapter
队列。

### 3. `onlyStage1/allStage` 的 S1 sector range hit 规则写错

问题是什么：range lookup plan 将 `noS2xlate` 和 `onlyStage1` 的 normal level-0 S1 sector 统一按
`valididx[]/pteidx[]` 展开和匹配。

怎么理解：V2 的 `PtwRespS2.hit()` 不是所有 `s2xlate` 都走同一套 sector 逻辑。

- `noS2xlate` 调用 `PtwSectorResp.hit()`，base-page 确实查看目标 `valididx[]`。
- `onlyStage1/allStage` 使用 `Cat(s1.entry.tag, s1.addr_low)` 作为 response anchor；normal level-0 不读取
  `valididx[]` 或 `pteidx[]`。

因此，把所有模式都按 valid sector 展开会让 framework 复用一个 DUT 不会命中的 payload，或拒绝一个 DUT 会命中的
payload。

为什么必须修复：range reuse 的目的就是生成能命中 DTLB filter 的 raw response；匹配语义错误会直接改变 response
是否解决 request。

解法：

1. `noS2xlate` 的 normal level-0 index 按每个 `valididx=1` sector 注册。
2. `onlyStage1` 的 normal level-0 index 只注册 `Cat(s1_tag, s1_addr_low)` 的一个 anchor。
3. `allStage` 的 effective normal level-0 同样只按该 S1 anchor；NAPOT/superpage 仍按既有组合 shape。
4. `pteidx[]` 保留为 one-hot payload consistency 检查，不进入 raw-hit 判定。

已回写：range lookup plan 的 index 构造、raw matcher、说明和 directed 场景。

### 4. live entry 删除后没有统一清理 range index

问题是什么（旧单值索引阶段）：stage-aware plan 的 C4 伪代码只删除 `tlb_entry_by_key[delete_key]`，而 range lookup
plan 已为同一 entry 建立旧单值 `tlb_anchor_key_by_range_key`。若只删 canonical table，range index 会留下指向不存在
anchor 的悬挂 key。

怎么理解：fence 后 B request exact miss 时，range index 仍可能返回旧 A 的 anchor；后续访问 canonical table 时只能
得到 null/miss，最终不是错误 fatal，就是错误复用。

为什么必须修复：range index 是 responder 的 lookup 路径一部分；删表不删索引会使 fence 后任何范围查询失去一致性。

解法：新增唯一的 `delete_live_tlb_entry_by_anchor_key()`：先读取 entry 的 `range_index_keys` 并调用
`unregister_tlb_range_index()`，再删除 canonical entry。SFENCE/HFENCE 与其它逐 entry delete 一律调用它；reset 全清可以
同时清两张 map，不必逐 entry 扫描。

已回写：range lookup plan 与 SFENCE/HFENCE stage-aware plan。

### 5. 持久化 `s1_paddr/s2_paddr` 没有可靠输入且与 range hit 冲突

问题是什么：random payload plan 曾要求 live entry、pending 和 UID record 保存 `s1_paddr/s2_paddr`，但 L2TLB
`PtwReq` 只有 VPN/GVPN 和 `s2xlate`，没有 page offset；range plan 则已规定不能保存这两个字段。

怎么理解：A/B 位于同一 NAPOT/superpage 时会复用同一 raw entry，却可具有不同的 resolved PPN 低位。若持久保存
A 的 byte PAddr，再给 B 复用，会把 A 的 offset/派生地址误当成 B 的结果；更根本的是 responder 没有取得 offset 的来源。

为什么必须修复：虽然 PAddr 不驱动 L2TLB response wire，但两份 plan 对数据模型的要求无法同时 coding，且错误的
持久字段很容易被后续 UID/debug consumer 当成可靠地址使用。

解法：不在 live entry、pending 或 UID record 持久保存 `s1_paddr/s2_paddr`。若 debug 需要页基址，只在 dump helper
临时计算 `{resolved_ppn, 12'b0}`；range hit 的 request-specific resolved PPN 只存在本次 pending 的 derived 字段，
不回写 canonical entry。

已回写：random payload plan，与 range lookup plan 对齐。

### 6. no-dispatch 不需要 inactive-gap / high-watermark 修复

问题是什么（已收缩的历史疑问）：早期方案假设 L2TLB lifecycle owner 自然退出后，后续新 owner 仍可能复用前一段
dispatch 建立的 live entry。因此它提出 inactive-gap raw fence 缓存和 high-watermark reconcile。

怎么理解：这个假设需要“没有 dispatch service 时仍能建立或保留并继续复用 `tlb_entry_by_key`”。当前合同明确否定它：
no-dispatch 时没有 `req.fire`，也就没有 entry 创建。因此 no-dispatch 的 fence 不需要记录为可恢复的 live-entry
删除事件；不存在需要在下一 owner 启动时补删的旧随机 payload。

处理结论：不实现 inactive-gap FIFO、high-watermark、`reconcile_live_entries_on_l2tlb_owner_start()` 或 L2TLB owner
handoff 清表。固定拓扑下，dispatch-active 的 adapter 在 C4 完成自身删除；no-dispatch 的 raw fence 不入 FIFO。
如果未来改变为可切换拓扑，采用显式 `clear_dispatch_l2tlb_live_entries()` 的一次性切换动作，而不是为常规 responder
owner handoff 增加一套持续 reconcile 状态。

#### 审核修改意见（单 owner 拓扑）

本项在当前 testcase 合同下不再是“需要防御的 handoff 问题”，而是明确不支持的场景。一个 owner 从启动后的 CSR warm-up
持续服务到 global stop；中途 `release -> claim`、`kill -> restart`、第二个 sequence claim 都是 topology 错误并
`uvm_fatal`。因此 coding 不得新增 inactive-gap FIFO、high-watermark、owner-session、启动 reconcile 或“把未完成 UID
交给下一 owner”的字段。

保留的最小逻辑只有：首次启动和每次 DUT reset 后，现有唯一 owner 重新完成 CSR history warm-up 与 flush baseline；
这不是 owner handoff，也不清理或重新认领 live entry。reset 由统一 reset epoch 清空旧 token/UID/barrier/raw fence/
pending invalidate/live entry/range index 后重新开始，固定 `dispatch_l2tlb_lookup_active` 不变。

### 7. 一个 L2TLB token 不能被错误建模成只属于一个 UID

问题是什么：random payload/range plan 曾要求每个 request token 通过 request-origin claim 只绑定一个 UID，
并禁止 response completion 扫描或回填其它 UID。

怎么理解：DUT interface 的 `PtwReq` 本来没有 UID；更关键的是 V2 `PTWNewFilter` 收到一笔 `PtwRespS2` 后，
会对 load/store/prefetch 三组 filter 的全部有效 entry 做 `resp.hit(vpn, asid, vasid, vmid)`。因此一笔 response
可以同时解除多个相同 key 或同一 NAPOT/superpage 范围内的 DTLB request。L2TLB 仍会为每次 request fire 分别
完成计数，所以 responder 的 token 不能合并；但 UID 回填不能反过来限制为一对一。若只回填某一个 UID，DUT 已经
收到 response 的其它 UID 会继续处于测试框架的“未映射”状态，可能阻塞后续 issue 或造成状态与 DUT 脱节。

为什么必须修复：这是 responder 输出被 DTLB 消费后的直接状态错误，和 checker/coverage 无关。它会让测试框架
漏记一笔真实已命中的翻译，或在 range hit 情况下错误只给 anchor UID 赋 payload。

解法：保留“每次 request fire 一个 token、每个 token 都必须最终 response/cancel”的账本；删除 token 到唯一 UID
的 claim 字段。response complete 时扫描低频 `uid_tlb_record_by_uid`，只对 `WAITING` record 调用
`entry_matches_uid_at_response()`：它用 record 自己的 VPN/s2xlate 和 response fire 当拍的 response-visible C-2 CSR
构造临时 key，再调用 `entry_matches_request_raw()` 复刻 DUT raw hit。命中 0/1/多个 UID 都合法；多个命中时复制相同
raw payload，但每个 UID 的 resolved PPN/GVPN 按自己的 VPN 和 response-visible mode 独立派生。C4 flush 只取消
`WAITING && uid_tlb_first_request_fire_sample_seq != 0 &&
uid_tlb_first_request_fire_sample_seq <= barrier.anchor_sample_seq` 的旧 instance；marker=0 或晚于 anchor 的
WAITING instance 必须保留。真 reissue 才递增 UID waiting epoch。这样既保留 token 守恒，也避免 flush 后旧 UID
被新 response 回填。

已回写：random payload、range lookup、timing correction 三份 plan。

### 8. request fire 不能直接使用当拍 runtime latest CSR

问题是什么：现有 `capture_fired_request()` 在 request fire 后调用 `get_mmu_csr_snapshot()`，它读取的是顶层 monitor
刚发布的 latest CSR。plan 也只写“request-time CSR snapshot”，没有说明该 snapshot 与 DTLB filter 的两拍延迟对齐。

怎么理解：V2 `MemBlock.scala` 先执行 `tlbcsr = RegNext(RegNext(io.ooo_to_mem.tlbCsr))`，才把 CSR 送给
`PTWNewFilter`。所以 responder 在 C 观察到的 L2TLB request 实际使用的是顶层 C-2 的 ASID、VMID、mode 和 root。
如果 C0 同拍发生 CSR change，直接取 C0 latest 会为仍属于旧 address space 的 request 建立新 key/payload；response
虽然被 driver 发出，却不会按 DUT 当前 filter context 命中。

为什么必须修复：lookup key、S1/S2 mode、level 合法性、tag/context 和 entry generation 都以该 snapshot 为输入。
CSR 边界把它们全部建错时，responder 会错误 reuse 或 build payload，属于接口功能错误。

解法：CSR monitor 每个 post-reset sample 发布完整的固定 3 项 history，不仅在 payload changed 时更新。新增
`get_l2tlb_request_csr_snapshot(dut_sample_seq)`，固定返回 DUT global sample C-2；history warm-up 前保持 ready=0，fire 时
无 C-2 项立即 fatal，禁止 fallback 到 latest。latest CSR 仍用于 startup/monitor/flush event，不与 request snapshot
混用。

已回写：timing correction plan、range lookup plan、random payload plan 的 request snapshot 定义。

#### 审核修改意见（单 owner 最终合同）

本问题的最终时基不是 owner 私有计数，也不是只保存变化时刻的 latest snapshot。CSR monitor 每个 post-reset
posedge 唯一推进 global sample 并写固定深度 C-2 history；fence/其它 monitor、driver、adapter 和 negedge service
只能读取该 sample。request fire 缺少 C-2 history 必须 fatal，不得退回 runtime latest 或 UID issue-time CSR。

### 9. C4 flush 同拍的 response 不能记为有效 completion

问题是什么：timing plan 曾允许 selector 在 response completion sample 等于 barrier due sample 时发送 response，
并在 C4 先 `complete_driving_response()`、再取消 pending token。

怎么理解：V2 当前实例是 `PTWNewFilter`：external response 先经一拍 `RegNext` 送入多个 `PTWFilterEntry`，
C4 due 的 entry flush 与 MemBlock 外层回填屏蔽使该同拍 response 不形成可信 DTLB completion。旧 `PTWFilter`
在 `when(flush)` 中清 `ptwResp_valid` 的实现是相同边界的历史对照，不能误写成当前实例路径。若测试框架仍把 C4
response 标为 complete 并回填 UID，就会把 DUT 实际没有得到的翻译误记为已完成。

为什么必须修复：C4 正是 live entry 删除和旧 token cancel 边界；把同拍 response 当完成会产生 UID stale map，
也使 token 的 cancel/complete 分类与 DUT filter 状态不一致。

解法：selector 的合法条件改为 `completion_sample < barrier.due`，实现为发现
`due_filter_flush_sample <= completion_sample` 时拒绝候选。C4 不允许 `driving_req`，若仍存在直接 fatal；C4 只取消
pending token，以及 `WAITING && uid_tlb_first_request_fire_sample_seq != 0 &&
uid_tlb_first_request_fire_sample_seq <= barrier.anchor_sample_seq` 的 UID；marker=0 或晚于 anchor 的 WAITING
instance 保持等待；adapter 在同一 C4 独立删除 live entry。C1-C3 的旧 response 仍按原有规则允许完成。

已回写：timing correction、stage-aware live-entry、random payload/range 验收边界。

#### 审核修改意见（单 owner 最终合同）

单 owner 不改变 C4 的严格截止：external response fire 的 sample 必须严格早于 barrier due；C4 同拍的
`resp_valid && resp_ready` 属于实现时序错误，必须 fatal，不能记为 token/UID completion，也不能由 owner 数量变化放宽该边界。

### 10. UID 回填不能使用 UID 建立时冻结的 CSR

问题是什么：早期 multicast 描述只要求用 UID record 的 `lookup_key/csr_snapshot` 重放 raw hit。它与 V2
`PTWNewFilter` 的实际行为不一致：response external fire 时，filter 使用该 response sample 当前可见的 CSR，即顶层
CSR 的 C-2 history，而不是 UID 初次 issue 时保存的 context。

怎么理解：C0 顶层 CSR 从 ASID=A 切到 ASID=B 后，C2/C3 返回一笔旧 token 的 raw response。UID A 的 record 虽然仍保存
ASID=A，但 DUT 在 C2/C3 已按 B 判断 response 是否命中。若 framework 继续按 A 回填，UID A 会被标为 `COMPLETED`，
而 DUT 实际没有接受这笔翻译；反过来 global response 仍可能按 B 命中，因此不能仅凭 token 的创建时上下文判定。

为什么必须修复：这会把 DUT 未收到 response 的 UID 错记为已映射，后续可能跳过真实 reissue 或使用错误的 translation
状态。它直接破坏 L2TLB responder 的 response-to-UID bookkeeping，与 checker/coverage 无关。

解法：`complete_driving_response()` 在每次 external fire 读取当前 DUT global sample 的 C-2 CSR。新增
`entry_matches_uid_at_response(entry, record, response_filter_csr_snapshot)`，只用 record 的 VPN/s2xlate 与该 snapshot
构造临时 key 并调用 raw matcher；不覆盖 record 的 issue-time CSR。无 UID match 仍正常 complete token，UID 保持
`WAITING`，由 C4 cancel 或未来真实 raw hit 处理。若 response C-2 history 不存在立即 `uvm_fatal`，不得退回 runtime
latest 或 record 的旧 CSR。

已回写：random payload、timing correction、range lookup、stage-aware live-entry 四份 plan。

#### 审核修改意见（单 owner 最终合同）

response owner 仍按 response fire 当拍的 DUT global sample 读取 C-2 CSR，使用它重放 raw hit；单 owner 只保证该
读取由唯一 owner 执行，不允许把 UID 建立时的旧 CSR 作为替代，也不允许 payload owner 或 adapter 另建 CSR 镜像。

### 11. owner handoff 不能使用 sequence 本地 sample 解释 C-2/C4

问题是什么：CSR history、raw fence 与 flush event 已用 `get_dut_sample_seq($time)` 产生全局 sample，但 timing plan
仍把 request capture、barrier due 和 UID wait timestamp 写成 sequence 本地 `sample_seq`。现有
`initialize_lifecycle_state()` 会在每个新 owner 启动时把该局部计数归零。

怎么理解：owner A 在 DUT global sample=100 后自然退出，owner B 启动时局部 sample 又从 0 开始。若 B 拿本地 2 去查
C-2 CSR history，就会查到完全错误的早期 sample；若用本地 4 判定 fence due，也会把全局 C104 的 flush 错当成当前
或永远不处理。

为什么必须修复：response lifecycle owner 的自然交接仍是允许路径。错误时基会导致 ready 长期关闭、错误 fatal，或把
旧 token/UID cancel 错误地落在新 lifecycle；这是直接 response 功能错误。live entry 的 C4 删除则由 dispatch adapter
独立使用同一全局 sample，不随 response owner 交接。

解法：把 `get_dut_sample_seq($time)` 定为唯一跨组件时基。CSR history、flush event/barrier anchor/due、raw fence
timestamp、request/response C-2 查询、UID `uid_wait_start_sample_seq`、`uid_tlb_first_request_fire_sample_seq` 与 pending
cancel 全部使用 DUT global sample；sequence 局部计数仅允许用于本 owner debug。owner startup 只建立 token/UID 的
latest-event baseline，不能重新锚已有 barrier，也不执行 live-entry reconcile。

已回写：timing correction、random payload、range lookup 和 stage-aware live-entry plan。

#### 审核修改意见（单 owner 拓扑）

问题的核心保留为“C-2/C4 必须使用唯一 DUT 周期编号”，不再以 owner A/B 交接作为触发例子。当前源码的
`get_dut_sample_seq($time)` 会在不同仿真时间推进，若 posedge monitor 与 negedge service 都调用它，同一 DUT cycle
会得到两个编号，C0/C4 会错位；单 owner 不能消除此问题。

最终最简方案是新增唯一的 `advance_dut_global_sample()`：仅 CSR monitor 的 post-reset `posedge mon_cb` 调用一次并推进
global sample，随后在同一个 sample 发布 CSR history 和 fence/CSR lifecycle event。fence monitor、L2TLB driver、
dispatch adapter、redirect/ctrl monitor 与 negedge service 一律调用 `peek_current_dut_global_sample()`，不得再推进。
raw 的 `sample_time` 只记录 monitor 采样来源，不能要求它与 negedge service 的 `$time` 相等。global sample 在同一
testcase 内单调递增，`clear_raw_monitor_queues()` 不能将其清零；reset 只清 history valid 和各 flow 的 pending state，
不需要额外 reset epoch 来比较 C-2/C4。

每个 global sample 的 service 合同为：CSR/FENCE 同拍先合并为一个 lifecycle event record；dispatch adapter 和唯一
L2TLB owner 各最多消费一次该 sample 的非破坏性 event 视图。若 service 发现 raw 的 sample 大于当前 global sample、
或同一 owner 对同一 sample 重复推进自身状态，立即 `uvm_fatal`；不同相位读取同一 sample 不报错。

#### 审核修改意见补充（复位时基最终修正）

上面的历史解法保留了“不得使用 owner 本地 sample”的核心，但其中“reset 不需要额外 reset epoch”的表述不再是最终合同。
运行期 reset 必须由共享 reset coordinator 建立唯一 `reset_epoch` 并收集 CSR/fence/L2TLB monitor/response/adapter ack；
所有支持 topology 固定包含 CSR/FENCE/MONITOR；global sample 本身继续
单调保留，不因 reset 归零。单 owner 只取消 owner handoff，不取消 reset epoch；coding 以本补充及单 Owner 生命周期审核稿
为准，原段落作为历史回溯保留。

### 12. raw fence gate 不能使用 capture、connect 或 response owner 状态

问题是什么：`dispatch_monitor_capture_en` 恒为 1，但它只表示 monitor 会采样；`l2tlb_responder_active` 只表示
connect takeover；`l2tlb_lifecycle_owner_claimed` 只表示 token/UID response owner 已 claim。三者都不能证明当前有
dispatch live-entry table 或 adapter service loop。

怎么理解：若用其中任何一个字段作为 `push_raw_sfence()` gate，no-dispatch 也会把 fence 留入 FIFO，但该拓扑不存在
`req.fire`、live entry 与必要的 C4 delete。若同时允许 response owner 和 adapter pop，则一个 raw event 又会有两个
destructive consumer。

最终解法：唯一入队条件为 `item.valid && dispatch_l2tlb_lookup_active`。该状态为 1 时 adapter 是唯一 consumer；为 0
时 raw fence 直接丢弃并要求 raw FIFO/pending invalidate queue 为空。L2TLB lifecycle release 只检查 token、barrier、
driving response 和 WAITING UID，绝不检查或清理 adapter queue。`do_kill()` 与 `phase_ended()` 仍必须经过同一 response
release gate，不能直接释放未收敛的 token/UID owner。

已回写：stage-aware live-entry、timing correction 与 random payload plan；range lookup plan 仅引用 adapter 的最终 entry
delete 生命周期，不拥有 raw FIFO。

#### 审核修改意见（单 owner 拓扑）

本项继续保留，但删除“response owner release 后是否由其它 owner 处理 raw fence”的讨论。raw FIFO 的写入 gate 只由
固定 topology 决定，唯一 destructive consumer 永远是 adapter。`dispatch_monitor_capture_en` 仍仅表示 monitor capture，
`l2tlb_responder_active` 仅表示 connect takeover，`l2tlb_lifecycle_owner_claimed` 仅表示唯一 responder 正在服务；三者
均不能作为 raw fence gate 或 consumer 选择条件。

adapter 与 response owner 在 global stop 前都必须独立 drain 到静止：adapter 负责 raw/context/pending invalidate，
response owner 负责 token/driving/barrier/UID。父 flow 只查询两个只读 `*_drain_done()` 状态后退出，不能为了统一退出而
让其中一方清理另一方的 queue。

最终审核补充：lifecycle event history 只由 response owner 通过单一 cursor 消费；adapter 不建立第二个 history cursor，
只消费自己的 `raw_sfence_q`，并使用 raw 中冻结的 `lifecycle_event_seq/sample_seq` 做 provenance 诊断。这样 raw FIFO 与
response event history 各自只有一个 destructive/advancing owner，no-owner 时 event history 可在 watermark 发布后直接回收。

#### 审核修改意见（NO_OWNER event history 回收写权）

上段“no-owner 时直接回收 event history”会让 producer 获得 response history 的第二个 destructive writer，不能作为 coding 规则。
当前 `DISABLED/NO_OWNER + NO_DISPATCH` 没有 response token、UID 或 raw-fence FIFO consumer，因此只发布本拍 reason/watermark，
不分配 event sequence、不创建/追加 event history record，也不调用 retire/pop。正常 ENABLED runtime 中只有 response owner 可按 cursor
回收 history；CSR monitor 仅在 runtime reset 的 direct-writer 清理中清整张 history。该意见覆盖上段 no-owner 直接回收的历史简写。

### 13. range index 单值映射无法表达重叠 candidate

问题是什么（历史单值索引设计）：range lookup plan 一方面要求同一 request 可能命中多个 canonical entry，并按 coverage rank 选择
最大范围；另一方面把旧名 `tlb_anchor_key_by_range_key` 定义成一个 range key 对应单个 anchor。若第二个 entry 注册
相同 shape，旧规则只能覆盖第一个 anchor 或直接 fatal，既不能收集 candidate，也无法实现“唯一最大范围选中、最大
范围并列 fatal”的最终策略。

怎么理解：辅助 index 只是把请求导向可能命中的 bucket，不是最终仲裁结果。一个 1 GiB superpage 和一个内部
64 KiB NAPOT，或两个相同覆盖形状的不同 entry，都可能落在同一个规范化 bucket；必须保留 bucket 内全部有限
anchor，再回到 canonical table 做 raw matcher 和 rank 选择。

为什么必须修复：单值 index 会静默丢失 entry，导致 lookup 结果依赖注册顺序，而不是计划规定的 coverage rank；
删除其中一个 entry 还可能误删另一个 entry 的索引。

最终解法：把 index value 改为有界 anchor 列表 `tlb_anchor_keys_by_range_key[key][$]`，由
`MEMBLOCK_TLB_RANGE_CANDIDATE_MAX` 限制每个 bucket 的最大候选数；注册追加、重复 anchor fatal，删除只移除
对应 anchor，列表为空才删除 key。range lookup 遍历该有限列表并对每个 canonical entry 调用 raw matcher，之后按
`512G > 1G > 2M > 64K > 4K` rank 选择唯一最大项，最大 rank 并列才 fatal。

已回写：range lookup plan 的 index 数据模型、注册/反注册伪代码、候选查询说明和上限配置。

### 14. owner release 未覆盖 UID WAITING 与 phase 结束清理

问题是什么：原有 release-safe 描述主要检查 pending token、driving response 与 barrier，但 UID record 是独立表；若旧
动态实例仍为 `WAITING`，仅清空 token queue 就释放 owner，迟到 response 仍可能回填旧 UID。当前 driver
`phase_ended()` 还直接调用 `try_release_l2tlb_lifecycle_owner()`，可能绕过 sequence 的收敛检查。

怎么理解：token 是接口 request 账本，UID waiting instance 是 dispatch 状态账本，两者不是一对一；token 为空不代表所有
UID 已经完成或取消。phase 结束也不应把“connect 已 takeover”当作“sequence 已安全释放”。

为什么必须修复：这会让 owner handoff 或 phase 结束后留下旧 UID waiting 状态，后续 response-to-UID multicast 可能把
旧动态实例错误置为 `TLB_MAPPED`，破坏 redirect/reissue 边界。

解法：新增 `check_l2tlb_release_uid_waiting()`，仅在 release/kill/phase-ended 的低频路径扫描完整
`uid_tlb_record_by_uid`。它只统计 `record_valid=1 && uid_tlb_wait_state==WAITING` 的 record；不以 `pte_valid`、
token、key、UID 年龄或 owner 名称再过滤，`COMPLETED/CANCELED` 和无效/null record 不计入。统计值非 0 时打印每个
UID 的 waiting epoch、start sample 与 key，并 `uvm_fatal` 拒绝 release。当前单-owner 合同明确禁止把未完成 UID
等待实例转交给下一 owner，因此不额外引入 owner-session 字段或第二套交接语义。C4/reset cancel 和安全 response
complete 必须先把实例转为 `CANCELED/COMPLETED`。redirect 的 `clear_uid_dispatch_result()` 必须同步关闭旧 UID
waiting epoch，真实 reissue 才递增新 epoch。driver `phase_ended()` 只允许驱动最终 idle 并报告状态，不能直接
release；若仍有 token、driving response、barrier 或上述范围内的 WAITING UID，按 lifecycle fatal 处理。
【历史原文，不可执行】进入最终检查前，必须先由 parent 写 `close_requested`，再由 owner 的真实 `ready=0` sample 在
`sampled_req_fire=0` 时确认 `admission_closed/cutoff`；这一 transport close 才关闭 UID wait 注册。
`begin_l2tlb_lifecycle_release_closing()` 只在 admission 已 closed、drain 和 final inactive 已完成后写 closing，
不承担关闭 admission 的职责。检查与 `try_release` 在同一 sequence 控制流紧邻执行，成功时原子清 closing/owner claim。
closing 后仍发生 issue 注册是 topology 错误，必须 fatal，不能让“检查后新 WAITING UID”逃过 release gate。

#### 审核修改意见（本段 release 次序为历史描述，不可执行）

上方第 515-521 行保留旧问题分析，不能作为 coding 顺序：`phase_ended()` 只检查/report，不能驱动 final idle；parent 只写
`global_stop_requested` 并停止 routing，不能写 `close_requested`。唯一 owner 必须在 driver 发布的 READY transport sample 中先完成
capture/UID registration，再写 `close_requested`；该写入立即封闭新的 UID/token registration。后续 driver 的
`RELEASE_STOP` sample 只确认 transport `admission_closed/cutoff`，不承担第一次封闭 UID registration 的职责。最终 release
还必须通过 timing plan 的 raw-intake-close、final-inactive 完整谓词、closing/grant 与 epoch ack gate。

已回写：random payload、SFENCE/HFENCE live-entry 和 timing correction plan。`phase_ended()` 只允许最终 idle/report，
不得直接调用 `try_release_l2tlb_lifecycle_owner()`；该约束与 sequence 的 release gate 同源。

#### 审核修改意见（单 owner 拓扑）

release 的目的从“交接给下一 owner”收缩为“本 testcase 在 global stop 后安全结束”。唯一 owner 只能在以下条件同时满足时
release 一次：已经关闭新 UID 注册，`pending_q`、`driving_req`、`barrier_q` 为空，所有有效 UID record 都不是
`WAITING`，且父 flow 已确认 adapter drain 完成。response owner 本身不读取 adapter queue；父 flow 负责组合两个
只读 drain 结果。

普通 C4 对 `marker=0` 的 UID 保持 `WAITING` 是正确的，因为它尚未被观察到 request fire，不能被旧 fence 猜测取消。
其明确写者是 dispatch redirect/kill owner：发生 redirect 或取消该 UID 时无条件将当前 attempt 置 `CANCELED` 并移除
shape index；reset 取消全部 WAITING；正常 raw-hit response 置 `COMPLETED`。若 global stop 到来时仍有 marker=0
WAITING，release gate 必须打印 UID、epoch、key、wait-start sample 后 `uvm_fatal`，不得静默清表。

上段关于 `phase_ended()` 或 `do_kill()` 的可执行描述均由后续审核意见覆盖：`phase_ended()` 只能检查/报告，
不能驱动 final idle；active owner 的 `do_kill()` 无论静止与否都必须 `uvm_fatal` 并保留 claim。最终 inactive、
admission close 和 release 只能由正常 global-stop 主循环完成。这样不需要 handoff 恢复逻辑，也不会掩盖未收敛 token/UID。

#### 审核修改意见补充（异常退出最终修正）

为消除“`do_kill()` 在静止时可释放”与最终合同的歧义，当前 active owner 的 `do_kill()` 无论 token 是否已经静止、
是否看似已获得 grant，都不是 release caller，必须 `uvm_fatal` 并保留 claim。只有正常 global-stop 主循环在真实
final inactive item 完成、closing 建立、parent grant 到达且双 drain/`!reset_active` 成立后，才调用唯一 release helper；
`phase_ended()` 仍只检查和报告。该补充不删除前一段历史描述，coding 时以后续单 Owner 审核稿为准。

#### 审核修改意见补充（`phase_ended()` 最终驱动边界）

本问题上方所有“`phase_ended()` 驱动 final idle”或“final idle/report”的表述均只保留为旧方案记录，不能作为可执行路径。
`phase_ended()` 是 function，只能检查/报告并在 active claim 存在时报告 lifecycle fatal；最终 inactive item 只能由 owner 的
global-stop 主循环在真实 `drv_cb` 边界驱动、采样并置 `final_inactive_item_done`。它不 release、不驱动接口 task，也不能替 owner
补最后一个 cycle item。

### 15. C4 UID cancel 不能用 issue-time wait start 代替 request fire

问题是什么：`register_uid_tlb_record_on_issue()` 在 dispatch issue 时建立 `WAITING`，而真正能证明该等待实例已进入
L2TLB responder 生命周期的事件是 DTLB `valid && ready` request fire。旧 plan 用
`uid_wait_start_sample_seq < barrier.due_sample` 取消 UID，因此 C0 fence 后、C4 前才 issue 或尚未出现 L2TLB request
fire 的新等待，也会被同一旧 barrier 错误取消。

怎么理解：UID record 是 dispatch 侧账本，`PtwReq` 又没有 UID。issue 发生不等于对应 translation request 已经被
L2TLB responder 接收；只看 issue 时间，框架会把“还没有进入本次 responder request 生命周期”的新等待误当成
filter 中的旧 request。相反，C0 真正 fire 的 request 必须在 C4 前保留其 UID 上下文，并在未完成时正确取消。

为什么必须修复：错误取消会使 fence 后仍可能合法发出的 request 失去 UID 回填资格；若不区分 fire，也会让 UID
cancel 时序与 token 的 C0/C4 合同脱节。这是仅提供 L2TLB response 时测试框架自身的状态错误，不依赖 DCache、RM
或 checker。

最终解法：UID record 新增 `uid_tlb_first_request_fire_sample_seq`，WAITING 建立时写 0；
`uid_waiting_by_vpn_s2xlate` 维护 WAITING UID 的有限 shape 候选。`capture_fired_request()` 已冻结 C-2 CSR/key 和
pending snapshot 后，调用 `mark_waiting_uid_records_on_request_fire()`，只查询同 `{vpn,s2xlate}` 的候选并用本次
request key 确认，随后为候选写首个 fire sample；这不是 token-to-UID 绑定，仍允许一个 fire/response 关联多个 UID。
C4 的 `cancel_waiting_uid_records_for_flush()` 只取消 `WAITING && marker!=0 && marker<=barrier.anchor_sample_seq`
的实例，marker=0 或晚于 anchor 的实例保持 WAITING；reset 继续取消所有 WAITING。complete/cancel/reissue 必须同步
维护该 shape index，避免 request-fire 高路径扫描完整 UID 表。

已回写：random payload、timing correction、range lookup 与 stage-aware live-entry plan，并新增“C0 后 marker=0
等待在 C4 保持 WAITING”的 directed 场景。

#### 审核修改意见（单 owner 最终合同）

`uid_tlb_first_request_fire_sample_seq` 仍是每个 UID waiting attempt 的唯一 C4 取消依据。单 owner 不意味着 issue
就等于 request fire；marker=0 的等待必须保留，直到真实 request fire、redirect/kill 或 reset 由相应 owner 明确终结。

### 16. NAPOT 编码校验不能越过 PTE profile 边界

问题是什么：random payload plan 已规定只有无 fault `LEGAL` stage 的 `PTE.N=1` 才将 PPN low-4 确定性写成
`4'b1000`；MIXED/EXCEPTION_BIASED 要保留 profile 后的 raw PPN。range lookup plan 的早期
`validate_normal_napot_payload()` 却把所有无 fault `PTE.N=1` 都当作 LEGAL 编码检查，导致非 LEGAL profile
不是被错误 fatal，就是被实现者错误强制合法化。

怎么理解：`PTE.N` 决定 DUT raw hit 的 NAPOT 覆盖形状，PPN low-4 是正常地址派生能否被模型解释的编码条件。
两者不是同一件事。MIXED/EXCEPTION_BIASED 可以让 DUT 接收保留的 raw payload 并按 raw NAPOT shape 命中；但当
low-4 不是 `4'b1000` 时，测试框架没有资格把它伪装成一个可靠的 normal resolved PPN/DCache 地址。

为什么必须修复：若 validator 一律 fatal，plus profile 的非 LEGAL 随机语义无法生效；若一律修成 `4'b1000`，又会
悄悄覆盖用户要求保留的 raw 随机字段。二者都会使 responder 输出与所选 profile 不一致。

最终解法：无 effective fault 的任意 `PTE.N=1` 仍统一把 final level 收敛为 0。只有 LEGAL stage 校验并写入
正确 NAPOT PPN 编码；MIXED/EXCEPTION_BIASED 只校验 level 与 S1 sector one-hot/split 一致性，raw low-4 原样保留。
range matcher 仍按 DUT 可见 raw NAPOT shape 工作。`napot_raw_ppn_is_model_resolvable()` 对非 canonical non-LEGAL
payload 返回 false，pending/UID 的 derived PPN 保持 invalid 并记录 `UVM_LOW`，绝不改写 response wire 或拒绝 raw range hit。

已回写：random payload plan 的 PPN/resolver/验收规则，以及 range lookup plan 的 NAPOT validator、derived helper 和
directed 场景。

### 17. allStage 非规范 NAPOT 的 `s2_tag` 来源不能依赖 invalid derived PPN

问题是什么：random payload plan 已允许 MIXED/EXCEPTION_BIASED 的非 canonical NAPOT 保留 raw PPN，并把
`request_derived_valid` 置为 0；但同一 plan 又要求 allStage normal leaf 的 `s2_tag` 必须从
`s1_resolved_ppn` 派生。range lookup 命中后还要求固定复制 `s2_tag`。这样在非 canonical NAPOT 场景中，
实现者既不能生成协议必需的 S2 tag，又无法遵守“raw payload 不改写”的约束。

怎么理解：`request_derived_valid=0` 的含义只是测试框架不把该 raw PPN 当作可证明的 normal 地址，不代表 DUT
response 的 tag 字段可以不填。V2 allStage 的 ordinary `PtwRespS2.hit()` 主要按 S1 tag/组合 level/N 匹配，
但 `s2_tag` 仍是 response payload、S2 context/HFENCE 和 debug 的固定字段。它必须有一个稳定、位宽合法的 raw
协议来源，而且不能随着 range hit 的 request VPN 改写。

为什么必须修复：若直接跳过 `s2_tag` 或静默填零，DUT 可能收到无法关联的 response；若为了填 tag 强行把 raw PPN
修成 canonical，又会覆盖用户要求保留的非 LEGAL 随机 payload；若把无效 derived PPN 当作正常 GPA，则会错误
建立模型地址语义。

最终解法：新增构造期 helper `derive_allstage_raw_s2_tag()`。它使用构造期 request VPN、S1 raw PPN、level、
PTE.N 和 sector split，按 DUT `PtwSectorResp.genPPN()` 的原始位拼接规则得到 raw protocol GVPN anchor，按冻结
S2 `hgatp.mode` 的 29/38 位宽检查可表示性，成功后写入 live entry 的 `s2_tag`。helper 不执行 NAPOT canonical
校验、不调用 LEGAL fixup、不改写 raw PPN，也不静默截断高位；超出接口/GPA 位宽直接 `uvm_fatal`。canonical
LEGAL normal leaf 中该 anchor 与 `s1_resolved_ppn` 相同；非 LEGAL 非规范 NAPOT 中仍保存并驱动 `s2_tag`，但
`request_derived_valid=0`，pending/UID 不生成或消费 request-specific resolved PPN。range hit、snapshot、UID
copy 和 driver 只复制固定 `s2_tag`，不重新调用该 helper。

已回写：random payload plan 的术语、tag 字段规则、helper/payload 建立顺序和验收条件；range lookup plan 的
字段边界、derived helper 说明和 directed 场景。

## 单 owner 拓扑优化后的统一方案

> **历史章节标记：** 本节及其原始伪代码是问题回顾中的旧汇总草案。它们保留用于解释当时为什么需要修正，
> 不是可执行 coding 合同；后续 coding 只能依据单 Owner 生命周期审核稿和 timing correction `undo` plan 的最新 canonical
> state table。文中若出现“最终 coding 合同”、直接 `peek`、parent 直接 close 或仅双 drain，均以下方审核修改意见为准。

### 1. 固定拓扑与最小状态

本计划组的最终 coding 合同是：每个 testcase 只创建一个 `memblock_l2tlb_base_sequence`，该 sequence 在 CSR
history warm-up 完成后只 claim 一次 L2TLB lifecycle owner，并持续服务到 global stop 后所有相关 flow 静止，再
release 一次。DUT reset 不触发第二个 owner；reset 只取消旧周期工作，由同一个 sequence 重新完成 warm-up 后继续服务。

#### 审核修改意见（历史段落降级）

本段标题和正文是当时的汇总草案，保留用于问题回溯；其中“最终 coding 合同”不再是当前实现权威。后续 coding
必须改读单 Owner 审核稿和 timing correction plan 的 canonical state table，尤其遵守 owner 立即 claim、bounded
anchor/probe、reset epoch、baseline、driver ack 和 raw-fence intake close 规则。

#### 审核修改意见（claim 时点最终修正）

上段“warm-up 完成后只 claim 一次”保留为历史收敛描述，不能被 coding 理解为 warm-up 是 claim 的前置条件。
最终合同是：sequence 启动并完成私有状态初始化后立即且只 `claim` 一次；CSR history warm-up 只限制 `ready` 开放，
期间 driver 持续驱动 inactive/`ready=0`。这样第二个 sequence 会在真正驱动前被拒绝，且同一 owner 在 reset 后只 re-arm，
不二次 claim。

禁止第二个 responder claim、global stop 前 release、`do_kill()` 后 restart、owner handoff、inactive-gap FIFO、
high-watermark、`reconcile_live_entries_on_l2tlb_owner_start()` 和本轮 dispatch topology 切换。出现这些路径是
topology 合同错误，必须 `uvm_fatal`。单 owner 只表示 response lifecycle 的动态所有权唯一，不表示只有一笔 request，
也不允许把多个真实 `req_valid && req_ready` 合并成一个 token。

保留的最小状态分工：

```text
唯一 L2TLB responder owner：
  pending_q、driving_req、flush barrier、request token、UID request-fire marker/cancel。

dispatch_monitor_event_adapter：
  dispatch-active 时唯一消费 raw_sfence_q，登记/执行 C4 live-entry 与 range-index 删除。

parent dispatch/coordinator：
  只读取两边的 drain_done 状态，决定 global stop 是否可以结束；不替任何一边消费队列。
```

### 2. 唯一 global sample

**`advance_dut_global_sample()` 抽象功能描述：** 该同步 helper 由 CSR monitor 在每个 post-reset `posedge mon_cb`
调用一次，为本 DUT 周期建立唯一单调 sample；它向 CSR history、fence event 和其它 consumer 提供时基，不读取或
修改 token、UID 或 live entry。

**以下说明只用于回顾旧实现，不能直接 coding。** 当前 `get_dut_sample_seq($time)` 以调用时 `$time` 推进编号。若 posedge monitor 和 negedge service 都调用推进接口，
同一 DUT 周期会被错误编号两次。因此最终方案要求：

```text
CSR monitor 每个 post-reset posedge：
  调用 advance_dut_global_sample($time)，只推进一次 global sample。
  用该 sample 写完整 CSR history；CSR change 用该 sample 发布/合并 lifecycle event。

fence monitor 同一 posedge：
  读取 peek_current_dut_global_sample()，把 sample 写入 raw fence；有效 fence 与同 sample CSR change 合并 event。

L2TLB driver、dispatch adapter、其它 monitor、negedge service：
  只读取 peek_current_dut_global_sample()，不得推进 sample。
```

#### 审核修改意见（global sample 旧段落不可执行）

上方旧文字保留用于说明“唯一 advance”目标，但不能直接 coding：L2TLB driver 在读取 current sample 前必须先在同一
`drv_cb` 完成 bounded `sample anchor/probe`；只有 anchor 成功后才允许 `peek_current_dut_global_sample()`。该要求覆盖
上方“只读取 peek”的简写，不改变 CSR monitor 是唯一 advance 写者。

`sample_time` 只记录 raw 的实际 monitor 采样来源，不作为不同 phase 间相等判断。`clear_raw_monitor_queues()` 不清零
global sample；reset 清 CSR history valid、event baseline、token/barrier/UID、raw/context pending、live entry 和
range index，避免旧 C4 工作在 reset 后删除新 entry。这样同一 testcase 内无需增加 reset epoch。

> **审核降级：** 上一句“无需增加 reset epoch”只描述旧方案对 C-2/C4 算术的局部判断，不能作为 runtime reset
> 实现依据；运行期 raw/context/pending/live/release 必须使用下方补充定义的 `reset_epoch` 和分职责 ack。

#### 审核修改意见补充（runtime reset epoch 最终修正）

上段“不需要额外 reset epoch”只针对 C-2/C4 的 sample 算术，不表示运行期 reset 可以没有 epoch。最终合同保持 global
sample 不因 reset 清零，同时由共享 reset coordinator 为每次 runtime reset 分配唯一 `reset_epoch`，并按
`reset_required_ack_mask` 收敛 CSR/fence/L2TLB monitor/response/adapter ack；所有支持 topology 固定包含 CSR/FENCE/MONITOR。
该 epoch 还隔离旧周期的 cancel、context clear 与 release grant，
防止任何旧清理或 grant 混入 reset 后周期；coding 以本补充和单 Owner 生命周期审核稿为准。

### 3. C0/C4、reset 与 global stop 的联合顺序

**以下 C0/C4/reset/global-stop 伪代码同样是历史版本，不是当前执行顺序。** 当前执行顺序必须先经过 driver 唯一采样、
同拍 bounded anchor/probe、严格 watermark 判定、reset epoch/ack、baseline、stop/final inactive 和 raw-intake-close gate。

```text
每个 global sample：
  1. L2TLB owner 先确认上一拍 external response；C4 due 时禁止 fire，其他拍用 response C-2 CSR 完成 token 和 UID raw-hit 多播。
  2. 保存 fire_visible_event_seq，再登记本拍 flush barrier；C0 不删除已 fire token/UID。
  3. C4 due 时取消仍 pending 的旧 token，以及 marker!=0 且不晚于 anchor 的 WAITING UID。
  4. 按本拍真实 req fire 用 request C-2 CSR 建立独立 token 和 snapshot，并写 UID marker。
  5. 生成下一拍 ready/response；C4 不得生成 driving response。
  6. dispatch-active 时 adapter 独立登记/执行 C4 live-entry 与 range-index 删除。

reset：
  先停止新 admission；response owner 取消 token、driving response、barrier 和全部 WAITING UID；
  adapter 清 raw fence/context pending、sfence_invalidate_pending_q、canonical table 和 range index；
  最后清 CSR history valid 并重新 warm-up。旧 reset 前的 C4 work 不得继续执行。

global stop：
  response owner 继续服务到 token/driving/barrier/UID 全部收敛；adapter 继续服务到 raw/context/pending-invalidate 全部清空。
  parent 只有在 l2tlb_response_drain_done() 与 dispatch_l2tlb_live_entry_drain_done() 同时为 1 时结束 flow。
```

#### 审核修改意见（旧双 drain 结束条件不可执行）

上方 global-stop 伪代码中的“双 drain 即结束”只保留为历史缺陷说明。当前 parent 还必须等待当前 reset epoch/close
generation 的 raw-fence intake close、driver final-inactive 完整谓词和 release closing/grant；仅 queue 瞬时为空不能结束
flow。具体条件以 timing correction plan 的 `release_grantable()` 为准。

`marker=0` 的 UID 在普通 C4 保持 `WAITING`；redirect/kill owner 取消它时写 `CANCELED`，reset 取消全部等待，
正常 raw-hit response 写 `COMPLETED`。global stop 时仍为 `WAITING` 是未完成状态，必须打印 UID、epoch、key 和 sample
后 `uvm_fatal`，不能静默清表或 release。

### 4. 问题覆盖矩阵

| 回溯问题 | 单 owner 后的处理 | 最终结论 |
|---|---|---|
| 1 C0 token | 与 owner 数量无关。 | 保留 C0 建 token、C4 才 cancel。 |
| 2 raw fence consumer | 删除 responder handoff 讨论。 | adapter 唯一消费；parent 联合等待 adapter drain。 |
| 3 S1 sector hit | 与 owner 无关。 | 保留 raw matcher 与 payload consistency 分离。 |
| 4 entry/index delete | 与 owner 无关。 | 保留统一 delete helper。 |
| 5 不持久化 PAddr | 与 owner 无关。 | 保留 request-specific derived 边界。 |
| 6 inactive-gap/high-watermark | 单 owner 下不存在 owner gap。 | 不实现；第二 owner/restart fatal。 |
| 7 token/UID 一对一 | 单 owner 不等于单 token。 | 保留每 fire token 与 UID multicast。 |
| 8 request C-2 CSR | 与 owner 无关。 | 保留；统一使用 global sample。 |
| 9 C4 response | 与 owner 无关。 | 保留 C4 strict cutoff。 |
| 10 response-visible CSR | 与 owner 无关。 | 保留 response C-2 raw hit。 |
| 11 local sample/handoff | 删除 owner A/B 场景。 | 保留唯一 global sample；各 phase 只读不推进。 |
| 12 raw fence gate | 固定 dispatch topology。 | 保留 dispatch-active gate 和 adapter 唯一 consumer。 |
| 13 range 多 candidate | 与 owner 无关。 | 保留有界候选列表。 |
| 14 release/WAITING | 从 handoff 收缩为 testcase final release。 | 保留 release gate，并明确 marker=0 写者和 parent 双 drain。 |
| 15 request-fire marker | 与 owner 无关。 | 保留 marker-based C4 cancel。 |
| 16 NAPOT profile | 与 owner 无关。 | 保留 LEGAL/non-LEGAL 边界。 |
| 17 allStage raw `s2_tag` | 与 owner 无关。 | 保留 raw protocol anchor。 |

## Plan 对齐检查

四份 undo plan 的职责边界现已统一：

| 主题 | 唯一责任方 | 关键规则 | 权威章节 |
|---|---|---|---|
| request/token 生命周期 | L2TLB lifecycle sequence | C0 fire 建 token；C4 取消仍 pending 的旧 token；每个 accepted fire 保持独立账本。 | timing correction plan：第 3、4 节 |
| UID 回填生命周期 | `common_data_transaction` 的 response/cancel helper | response 以 response-visible C-2 CSR 的 raw hit 多播至 WAITING UID；C4 只取消 pre-anchor request-fire marker 已建立的旧 instance，不把 token 当 UID owner。 | random payload plan：`Response token 与 UID 多播回填`；range lookup plan：`删除与 UID 边界` |
| request/response CSR 对齐 | CSR monitor history + L2TLB lifecycle sequence | request fire 与 response UID multicast 都固定使用各自 DUT global sample 的 top C-2 snapshot；latest CSR 和 UID issue-time CSR 不参与当前 raw-hit 判定。 | timing correction plan：4.0、4.3、4.5 |
| 全局时基 | CSR monitor + `memblock_sync_pkg` | CSR monitor 每个 post-reset posedge 唯一推进 DUT global sample；其它 monitor、driver、adapter 和 negedge service 只读该 sample。CSR history、barrier anchor/due、raw fence timestamp、UID wait timestamp 都使用它。 | timing correction plan：单 owner 审核修正 |
| raw fence 消费与 C4 删除登记 | `dispatch_monitor_event_adapter` | `service_monitor_once() -> service_l2tlb_sfence_events()` 每个 dispatch sample 恰好一次；仅 `dispatch_l2tlb_lookup_active=1` 时入队。旧 `drain_sfence_events() -> apply_raw_sfence()` 必须不可达。 | stage-aware live-entry plan：`Raw Fence 与采样上下文 Flow` |
| no-dispatch 边界 | testcase 固定拓扑合同 | coordinator 在首个 post-reset monitor sample 前固定 `dispatch_l2tlb_lookup_active=0`，reset 不清该 topology state；`req.fire=0`、`tlb_entry_by_key` 为空，raw fence 不入 FIFO；不实现 inactive-gap/high-watermark/reconcile。 | stage-aware live-entry plan：`Raw Fence 与采样上下文 Flow` |
| owner release（历史摘要，不可执行） | 唯一 L2TLB lifecycle sequence + parent/coordinator | parent 先在本 sample admission 结算后写 close request；owner 的真实 `ready=0` sample 且无 fire 后写 admission closed/cutoff，随后 drain token/UID/barrier；final inactive/closing 后，parent 等待 adapter raw/context/pending-invalidate drain 并发 owner/epoch grant。双方不互相消费队列。 | 本行只保留历史回顾，不能作为 coding 写者合同。 |
| owner release（当前唯一合同） | parent + 唯一 owner + driver + fence monitor + adapter | parent 只写 global stop/停止 routing；owner 在 READY `drv_cb` 完成既有 fire/UID capture 后写 close request/stop item；driver 唯一确认 cutoff/final transport seq，且在后续 `drv_cb` 回收 final mailbox；fence monitor 写 intake close，adapter/owner 分别 drain；所有 final settled、recycle、mailbox EMPTY、ack 条件成立后 parent 才 grant，owner 最后清 claim。 | timing correction plan：单 owner 审核修正；random payload plan：`check_l2tlb_release_uid_waiting()` |
| live entry 删除 | `common_data_transaction` | 通过统一 delete helper 同时删 canonical entry 与 range index。 | stage-aware live-entry plan：`apply_due_sfence_invalidate()`；range lookup plan：`删除与 UID 边界` |
| ordinary range reuse | `common_data_transaction` | 复刻各 `s2xlate` 的 `PtwRespS2.hit()` raw 语义。 | range lookup plan：`Range Lookup Flow` |
| NAPOT profile 边界 | payload builder + derived helper | 任意无 fault NAPOT level=0；仅 LEGAL 固定/校验 `4'b1000`，non-LEGAL raw PPN 保留且非 canonical 时 derived PPN invalid。 | random payload plan：`LEGAL NAPOT PPN 直接编码`；range lookup plan：`normal-leaf NAPOT 检查` |
| allStage raw tag 边界 | payload builder | `s2_tag` 由 raw protocol GVPN anchor 生成并固定复制；derived invalid 不阻止 tag 驱动，也不产生 normal PPN 消费。 | random payload plan：`derive_allstage_raw_s2_tag()`；range lookup plan：`request_derived` 规则 |
| driver payload | pending snapshot | 只驱动 raw fields，不驱动 derived PPN/PAddr；due=C4 时不允许 external response fire。 | random payload plan：`数据模型与响应建立`；timing correction plan：4.4、4.5 |

#### 审核修改意见（Plan 对齐表中的旧 owner release 摘要）

上方 `owner release` 行保留原始回顾，不作为当前写者合同。当前方案是：parent 只写 `global_stop_requested` 并停止 routing；
唯一 owner 在真实 `drv_cb` 结算后写 `close_requested` 并生成带 current `item_reset_epoch` 的 stop item；driver 从真实
`drv_cb` 冻结并唯一写 `admission_closed/cutoff`；adapter 直接清自己的 raw/context/pending-invalidate/live-entry 状态并回 ack；
parent 最终同时等待 response drain、adapter queue drain、raw-fence intake close、final inactive、closing 和 grant 条件。双方不互相消费队列。

## 当前审核结论

本轮单 owner 复查补齐了唯一 global sample 推进点、reset 取消旧 C4 工作、parent 的双 flow global-stop 收敛、
marker=0 UID 的明确写者，以及禁止 handoff/restart/topology transition 的合同。四份 undo plan 的单 owner 审核修正
现已写入对应文档，但尚未 coding、compile 或仿真验证；当前源码仍保留旧 `drain_sfence_events()`、C0 kill、
`phase_ended()` 直接 release 等路径，不能误标为已实现。

> **历史状态声明：** 上述“已写入对应文档”只表示方案文字已回写，并不表示源码实现完成；本段不能作为 coding 完成证明。

剩余明确边界：CSR mode/root 变化仍由 testcase 按既有合同配套构造 SFENCE/flush；本专项不增加第二套 CSR 自动失效。
DCache owner、最终 byte PAddr、RM/checker/coverage 仍是后续专项，不属于本计划组。

## 最终覆盖声明（单 Owner 复核后追加）

本节不删除或改写上文的历史问题、旧函数名和当时结论；它只声明后续 coding 的最终覆盖关系。四份关联 `undo` plan
已经写入单 owner 审核约束，但尚未 coding/验证：timing correction plan 覆盖 token/UID、global sample、reset/release；
stage-aware plan 覆盖 raw fence producer/watermark/adapter C4 delete；range lookup plan 覆盖 range candidate/index；
random payload plan 覆盖 response payload/UID multicast。共同合同是：一个 testcase 只有一个 responder owner、CSR monitor
是唯一 global sample 推进者、response owner 是 flush history 的唯一 cursor 持有者、adapter 只消费 raw fence FIFO，并且 C0 建 barrier/C4 才取消旧工作。

历史段落中出现的 `get_dut_sample_seq()`、`peek_latest_dut_sample_seq()`、"reset 不需要 epoch" 或仅等待双 drain 的表述，
仅用于说明旧方案为什么不足，不能直接作为 coding 依据。最终 coding 必须采用 `advance_dut_global_sample()` 与
`peek_current_dut_global_sample()`：CSR monitor 唯一 advance，其它 producer 等 anchor 后 peek，所有旧 `get` 调用迁移完后删除；
`peek_latest` 如暂时保留只能作为无状态 peek wrapper。runtime reset 保留单调 global sample/event_seq：CSR monitor 清 CSR producer done、
history/watermark/CSR context baseline，fence monitor 清 raw producer/intake-close/context-dedup，adapter 清 raw/context/pending/live，
response owner 清 response cursor baseline；再按当前 reset epoch 的 required ack 收敛。

> 审核修改意见（紧邻下方历史原文）：下段“parent 已先写 close request”是旧方案摘要，不可执行。
> 当前唯一规则是 parent 只写 `global_stop_requested` 并停止 routing；唯一 owner 在下一真实
> `sample_ready=READY` 的 `drv_cb` 完成本拍 request/UID capture 后，才写 admission-settled 与 close request。

> **以下段落为历史摘要，不可执行：** 它保留“parent 已先写 close request”的旧表述，仅用于追溯曾经的冲突。

最终 release 只适用于 `ENABLED + DISPATCH_ACTIVE`：parent 已先写 close request、driver 已在真实 stop sample 确认
admission closed，且 owner 已真实完成 final inactive sample 并建立 closing 后，parent 与 owner
共用 `release_grantable(owner, current_reset_epoch)` 复核 response/adapter drain、当前 epoch ack 和非 reset 状态；parent 才可发 grant，
owner 才可原子清 claim。`DISABLED/NO_OWNER + NO_DISPATCH` 必须保持 claim=0，不发送 final inactive、closing 或 grant。四份
专项 plan 已写入上述单 owner 对齐约束，但尚未 coding、compile 或仿真验证；旧 lifecycle/handoff 名称只保留在历史解释中，coding 仅以当前审核稿和关联 `undo` plan 为权威。

#### 审核修改意见（旧 parent close 与 release 摘要不可执行）

本段仍是历史覆盖摘要。当前 parent 只能写 `global_stop_requested` 并停止 routing；owner 必须在真实 sample 结算后写
`close_requested`，driver 才能在匹配 stop item 的 `sampled_req_ready=0 && sampled_req_fire=0` 边界确认 cutoff。
reset release 的 tagged NORMAL/inactive baseline、final item 的完整 `sampled_req_ready=0 && sampled_req_fire=0 &&
sampled_resp_valid=0`、精确 final sample settled、final mailbox recycle proof 与 mailbox EMPTY 以及 raw-fence intake close
均是 release gate 的必要条件；不要采用本段省略这些条件的简写。

#### 最终审核补充：两阶段 admission close 覆盖旧 release 描述

#### 审核修改意见（本节 parent close 文字为历史，不可执行）

本节下方保留的“parent 在 global-stop sample 写 `close_requested`”仅用于说明旧方案的竞态，不是当前合同，不能作为
coding 指令。当前唯一可执行规则是：parent 只写 `global_stop_requested` 并停止 routing；唯一 owner 在下一真实
`sample_ready=READY` 的 `drv_cb` 先 capture 已存在 fire/UID，再写 admission-settled 与 close request。若本节后续历史叙述
仍使用“parent close”的省略说法，均由本审核意见覆盖。

#### 审核修改意见（下列“最终合同”段落为旧方案回顾，不可执行）

紧随其后的段落虽使用“最终合同”字样，但其中“parent 先结算 admission、再写 `close_requested`”是本回顾文档保留的
旧方案，不能作为 coding 指令。当前唯一权威仍是 owner-side seal：parent 只写 global stop；唯一 owner 在 READY `drv_cb`
完成本拍 capture/UID registration 后，才写 `admission_settled`、`close_requested` 和 generation。下列原文不删除，
只用于追踪它为何被后续方案取代。

> 审核修改意见（紧邻下方历史原文）：下段虽写“最终合同”，但仍保留 parent 写 `close_requested` 的旧方案，
> 不可执行。当前 parent 只写 `global_stop_requested`；owner 在 READY `drv_cb` 完成本拍 capture/UID registration 后写 close request。

> **以下整段是历史原文，不可执行：** 仅保留旧字段和旧顺序的回顾，不能覆盖紧随其后的当前合同。

上文所有把 `release_closing`、`begin_l2tlb_lifecycle_release_closing()` 或 parent 的 close helper 说成“直接关闭新 UID 注册”
的表述，只保留历史问题/旧解法上下文，不能作为 coding 逻辑。最终合同是：parent 在 global-stop sample 先结算已有
admission，再写 `close_requested` 和单调 generation；该 flag 写入之后任何新的 UID registration/token capture 都必须
`uvm_fatal`，此前已完成的 registration/fire 保持合法，不回溯清理。owner 下一带有匹配 generation 的 `RELEASE_STOP` item
在真实 `drv_cb` 以冻结的 `sampled_req_fire=0`、`ready=0` 确认 `admission_closed/cutoff_sample`；cutoff 是 transport
关闭的确认边界，不是 UID admission 首次关闭的时点。owner 完成 response drain 后发送独立的 `RELEASE_FINAL_INACTIVE` item，
只有 driver 真实采样匹配 kind/generation 的 `ready=0/resp_valid=0` 后才建立 closing；adapter drain 与 owner 流程并行，
parent 在双 drain、closing、grant owner/epoch/generation 条件都成立后发 grant。该顺序不改变“单 owner、多个 outstanding token”
的边界，也不表示源码已经实现。相关简称和旧字段名统一按单 Owner 审核稿的 canonical state table 解释，不得新建别名状态。

**当前替代合同：** parent 不写 `close_requested`；唯一 owner 只能在 READY `drv_cb` 已结算本拍 fire/UID 后写
admission-settled、close request 与 generation，driver 是 admission cutoff/final transport seq 的唯一写者。final item 被 monitor
同步处理并由 owner 终态确认后，driver 仍必须在下一真实 `drv_cb` 回收该 mailbox slot，写 final recycle proof；只有该 proof
等于 final transport seq、mailbox EMPTY、response/adapter drain、raw-fence intake close、ack 与 non-reset 条件都成立时，parent
才可发 grant，owner 才可清 claim。

#### 审核修改意见补充（admission seal 与旧 metadata 的二次复核）

本补充不删除上文历史文字。上文凡写成“`admission_closed/cutoff` 才关闭 UID wait 注册”、“closing 后才禁止 issue 注册”或
“从下一 sample 才禁止注册”的地方，都只保留为当时方案的回顾；后续 coding 以这里的更严格边界为准：唯一 owner 必须在
READY `drv_cb` 完成当前 sample 的 request capture/UID registration，再写 `close_requested`。parent 只写 global stop，不得
写 close request。从该写入动作之后，任何 `capture_fired_request()`、
`register_uid_tlb_record_on_issue()` 或等价新 helper 的调用都必须在修改 token/UID 前 `uvm_fatal`，即使调用仍在同一 global sample。
`admission_closed/cutoff` 只证明 DUT transport 的匹配 `RELEASE_STOP` item 已真实以 `ready=0 && sampled_req_fire=0` 跨过
`drv_cb` 边界，不能推迟软件 admission seal。

`RELEASE_STOP` 与 `RELEASE_FINAL_INACTIVE` 必须同时携带当前不回绕 `close_generation`。非 reset 状态下，driver 看到这两类
metadata 却没有匹配的 close request/owner/generation 时必须 `uvm_fatal`；普通 idle、flush hold 或 reset 前遗留 item 不能仅因
`ready=0` 被解释为 stop/final。runtime reset 作废 close request、admission close、冻结 item metadata、final inactive generation、
closing owner/generation 和 grant owner/epoch/generation，但不回绕 close generation。历史名
`l2tlb_lifecycle_release_closing` 仅是旧拼写；后续实现只声明 `l2tlb_release_closing`。本补充与单 Owner 生命周期审核稿及
timing correction plan 同为后续 coding 的权威，历史段落不再提供可执行 release 时序。

#### 审核修改意见补充（二次时序复核：owner-side admission seal）

本补充继续保留上文历史文字，不改写其中“parent 写 close request”的回顾结论。结合当前 `service_real_dispatch_flow()` 在
negedge 运行、L2TLB driver 在 `drv_cb` 驱动下一周期 `ready` 的实际时序，parent 在 negedge 直接封口会晚于此前已经驱动的
`ready=1`；该 ready 仍可能在下一 posedge 形成合法 request fire。最终方案因此收敛为：parent 只写
`global_stop_requested` 并停止新 routing，唯一 owner 在下一真实 `drv_cb` 先完整 capture 该 fire、更新 UID，再写
`l2tlb_owner_admission_settled_sample_seq=current_sample`。只有该 watermark 已达到当前 sample，owner 才能写
`close_requested`、`close_request_reset_epoch=current_reset_epoch` 和新的 `close_generation`，并在同一拍生成
`RELEASE_STOP/ready=0` item。

这样 `close_requested` 写入后的 capture/register 仍然一律 fatal，但 seal 前由旧 ready 窗口导致的 fire 不会被误杀。
runtime reset 清除 watermark、close/admission/final/closing/grant 的 active metadata；若 global stop 持续为 1，reset release 后
同一 owner 必须重新发布 watermark 并为新 reset epoch 建新的 close request，parent 不得复用 reset 前 close/grant。
driver 还必须保存 `admission_closed_generation`，并拒绝同 generation 的第二个 `RELEASE_STOP` 或
`RELEASE_FINAL_INACTIVE` sample。上述结论覆盖本文所有历史 parent-close 描述；后续 coding 以单 Owner 审核稿和 timing correction
plan 的 owner-side seal 伪代码为准。

#### 审核修改意见补充（reset item 与 sample-ready 边界复核）

本补充继续保留上文历史文字，不把旧 release 描述改写为当前实现。后续 coding 还必须遵守：

1. `wait_for_dut_sample_ready_at_drv_cb()` 是同一 `drv_cb` 内的有限 probe，返回 `READY/NOT_READY`；`NOT_READY` 不能
   跨 clock 阻塞。driver 先冻结本拍 VIF 值，若非 reset sample 未 ready，则要求 request fire/response valid 均为 0，
   送 inactive、不得建立 admission-settled/stop/final，并 `continue` 到下一拍重新采样。
2. `RELEASE_STOP`/`RELEASE_FINAL_INACTIVE` item 必须同时冻结 `item_reset_epoch`。reset-active 时 driver 清本地旧 item、
   强制 inactive，不调用 stop/final confirm；reset release 后发现旧 epoch item 只能记录 stale 诊断并丢弃，不能误匹配新 generation。
3. response owner 只清自己的 token/UID/barrier 和 driver 本地 item；fence monitor 清 raw producer/intake-close/context-dedup 并回 FENCE ack；
   adapter 清 raw fence、待绑定 context、pending invalidate、live entry/range index 并回报 adapter ack；reset coordinator 不直接清这些状态。package 只提供共享状态和检查 helper，不改变这些直接写者边界。
4. `admission_closed/owner/generation/cutoff` 与 final-inactive done 的运行期直接写者仍是 driver 从真实 `drv_cb` 冻结
   item 后调用的 helper；owner 只写 close request、生成带 epoch 的 item，parent 只写 global stop/release grant。

#### 审核修改意见补充（末轮一致性：driver 确认、warm-up stop 与 raw intake close）

本补充不删除或改写上文历史叙述。后续 coding 以单 Owner 生命周期审核稿和 timing correction plan 的 canonical
state table 为准，并追加以下三条不可省略的收敛条件：

1. `admission_closed/owner/generation/cutoff` 的唯一直接写者是 `L2tlb_agent_agent_driver`。owner 只在结算本拍
   request/UID 后写 close request 并生成 `RELEASE_STOP`；driver 在真实 `drv_cb` 冻结该 item、`ready=0` 与
   `sampled_req_fire=0` 后调用 confirm helper。任何上文“owner 确认/写 admission closed”的表述只表示 owner 发起
   stop，不授权 owner 直接写该状态。
#### 审核修改意见（紧邻下方第 2 条历史原文，不可执行）

下段“NOT_READY 时写 admission-settled 并建立 `RELEASE_STOP`”为旧方案。当前 `NOT_READY` 只冻结本拍 request
fire/response valid，二者必须均为 0；只驱动普通 inactive，不写 admission-settled、close、stop 或 final，下一真实
`drv_cb` 重新采样。第 2 条原文仅保留用于解释旧方案为何被取代。
2. 【历史原文，不可执行】reset release 后 global sample 已存在但 CSR history 或 lifecycle event 尚未 ready 的 warm-up sample，不能只写
   “drive inactive 后等待”。owner 必须冻结本拍 request fire/response valid，要求二者均为 0，写 admission-settled；若
   global stop 已请求，则在该 sample 建立当前 epoch 的 `RELEASE_STOP`，随后 `continue` 到下一真实 `drv_cb`，不带旧
   VIF 值进入 token/UID/history 路径。

当前替代规则：`sample_valid=0`、`NOT_READY` 或 post-reset baseline proof 未完成时，一律只发送当前 epoch 的
`NORMAL/inactive`，要求冻结 fire/response 均为 0；不得写 admission-settled、close、stop 或 final。只有 baseline proof
已完成且同一 driver sample 返回 READY、owner 已完成本拍真实 fire/UID registration 后，才允许写 admission-settled 并按
global stop 建立 `RELEASE_STOP`。
3. adapter queue drain 不是 release 的充分条件。fence monitor 必须在 close request 后先完整处理一个 raw sample，再写
   与 current reset epoch/current close generation 匹配的 `raw-fence intake closed`；之后新的有效 raw fence 必须 fatal。
   parent 的 `release_grantable()` 同时要求 response drain、adapter raw/context/pending-invalidate drain 和该 intake-close
   proof，防止 grant 后出现无人消费的 C4 live-entry delete 工作。

以上是历史“仅双 drain”描述的审核补充，不表示当前源码已实现。相关字段在 runtime reset 与正常 release 时清活跃值，
`close_generation` 仍保持 testcase 内单调不回绕。

#### 审核修改意见补充（末轮时序、reset transport 与 final-fire 复核）

本补充不删除或改写上方任一历史问题及其旧解法。后续 coding 以 timing correction plan 和单 Owner 审核稿的最新
canonical flow 为准；本节只覆盖历史文字没有展开的四个实现边界：

1. driver 到达 `drv_cb` 后先冻结本拍 VIF 和 item metadata，再把本拍 anchor 交给
   `wait_for_dut_sample_ready_at_drv_cb()`。helper 仅在同一仿真时刻完成 bounded CSR anchor/NBA/producer probe；
   anchor 成功后才可以 `peek_current_dut_global_sample()`。禁止先 peek 再 probe，否则 CSR monitor 后调度时会把上一 sample
   错用于当前 VIF。
2. reset-active 或 `item_reset_epoch != current_reset_epoch` 的 item 若已经由 `try_next_item()` 返回，driver 必须先
   `seq_item_port.item_done()` 恰好一次，再驱动 inactive、丢弃句柄并进入下一 `drv_cb`。该路径不得调用 stop/final confirm；
   若尚未取得 item 而 abort，则不调用 `item_done()`。正常 send 路径也恰好一次 `item_done()`，但需保留轻量 metadata latch
   等下一真实 sample 冻结 fire。
3. reset coordinator 只分配 epoch、发起请求和等待 ack。response owner 直接清 token/driving/barrier/UID 后只发布 owner-reset-done；
   driver 清本地 item 后唯一写 response ack；fence monitor 直接清 raw producer settled/intake-close/context-dedup 并写 FENCE ack；
   dispatch adapter 直接清 raw/context/pending invalidate/live entry/range index 并写 adapter ack；CSR monitor 直接清
   history/CSR context 并写 CSR ack。任何“adapter/reset coordinator 共同清队列”或“owner/driver 共同写 response ack”的旧表述均非当前写者合同。
4. reset release 必须先完成 current epoch 的 `NORMAL/inactive` baseline，得到 fire=0/resp_valid=0 的 driver proof；
   baseline 前 current epoch 的 stop/final item 是 fatal。之后才能创建 close/stop。`RELEASE_FINAL_INACTIVE` 与
   `RELEASE_STOP` 一样必须使用同一真实 `drv_cb` 冻结的 `sampled_req_fire`；final 确认条件固定为
   `sampled_req_fire=0 && sampled_req_ready=0 && sampled_resp_valid=0`，缺任何一项都不能写 final-inactive done。
   final sample 的 sequence terminal ack 也不等于 release：driver 必须在下一真实 `drv_cb` 回收 slot 并写 recycle_done_seq，
   release gate 要求该 seq 等于 final transport seq 且 mailbox EMPTY。

这些条件只约束测试框架的采样、UVM transport 和生命周期状态，不改变 L2TLB payload、C0/C4 flush、range matcher 或
adapter 的 stage-aware delete 语义；当前仍未 coding、compile 或仿真验证。

#### 审核修改意见补充（warm-up watermark 与 mailbox/owner provenance 的最终裁决）

本节是对上文历史方案的审核意见，不删除上文原始问题和旧解法。上文凡写成“sample 未 READY 但已完成无 fire 判定即可写
`admission-settled`、建立 `RELEASE_STOP`”的内容均为不可执行历史描述；它与当前 canonical 合同冲突，coding 不得采用。
最终规则是：`sample_valid=0`、`NOT_READY` 或 post-reset baseline proof 未完成时，只发送/采样 current epoch 的
`NORMAL/inactive`，不写 admission watermark、close request、stop 或 final；只有 baseline proof 完成且同一 driver sample
返回 `READY`，并完成本拍真实 fire/UID registration 后，owner 才能写 `admission-settled`，再建立 close request。

同时，transport sample 必须冻结 `reset_active/current_reset_epoch` 和上一 item 的 `item_owner_name`。driver 是唯一 transport
VIF sampler 和 sample publisher；现有 `L2tlb_agent_agent_monitor::mon_data()` 不得再独立 `@mon_cb` 读取同一组 transport
信号，只能消费 driver sample 的只读副本。sample mailbox 使用单槽 `EMPTY -> PUBLISHED -> CONSUMED/DROPPED` 协议：driver
先原子预留 PUBLISHED wrapper，再同步执行 monitor `analysis_imp.write()`，返回后才通知 sequence 按 sample_seq 消费或 abort 丢弃；
reset coordinator 不直接改 mailbox。driver 每个 `drv_cb` 先 recycle terminal slot，再以 `try_next_item()` 非阻塞轮询，NO_ITEM
仍发布 inactive/reset sample；因此 reset ack 和 final recycle 不依赖新 sequence item。final inactive 的 driver 确认必须比较
冻结的 item owner、generation、reset epoch 和同一 sample 的 ready/fire/response 三项谓词，不能只读取当前 package owner 状态。

reset coordinator 只分配 epoch、发 reset request、等待各 direct writer ack；response owner、driver、fence monitor、adapter、
CSR monitor 分别清理自己的状态，response-owner cursor 只能由 response owner 对齐，CSR monitor 不得改写它。上述审核意见覆盖
本回顾文档中更早的 parent 直接 close、coordinator 直接清队列或 warm-up 直接 stop 的简写；后续 coding 只以单 owner review
和对应 timing correction `undo` plan 的 canonical 规则为准。

`DISABLED/NO_OWNER + NO_DISPATCH` 不启动 responder sequence、不会 claim 或创建 token，但不是“不运行任何采样逻辑”：
L2TLB driver 必须保留 passive sampler 分支，固定 `ready=0/resp_valid=0`、不调用 `get_next_item()`、不发布 sequence mailbox，
只向 monitor 发布 immutable analysis sample。monitor 的旧 `mon_data()` transport 读取路径应迁移为同步
`write_transport_sample()`；非 reset 时看到 `req_valid=1` 立即 fatal。这样仍保持
driver 是唯一 transport sampler，同时不会漏掉 no-owner 下因 ready 固定为 0 而永远不 fire 的非法 request。

driver-to-monitor 的落点是 agent 内部同步 TLM：agent `connect_phase()` 先把 sequencer slot owner 显式绑定到 driver，
driver 在该 slot 预留 freeze 后的
`L2tlb_agent_agent_transport_sample`，`L2tlb_agent_agent::connect_phase()` 连接到 monitor 的
`transport_sample_imp`；monitor 的 `write_transport_sample()` 在同一次 `analysis_port.write()` 调用中消费 sample，
不再 `@mon_cb` 读取 VIF。该连接不增加 env/RM 外部 producer，也不建立 monitor FIFO/backlog；sequence mailbox 与 monitor
共享同一个 wrapper handle，但 wrapper 的 private frozen payload 只能通过 getter 读取副本，sequence/driver 独占 mailbox 终态管理。
driver 同时把 final proof `{valid, epoch, transport_sample_seq}` 冻结在 sample 中；monitor 的 reset ack 返回独立的
`{reset_epoch, reset_sample_transport_seq}` tuple，两个 seq 不复用。final sample 被 sequence terminal ack 后，driver 还必须在
下一真实 `drv_cb` 回收 slot 并写 `recycle_done_seq`；release gate 同时要求该值等于 final transport seq 与 mailbox EMPTY。

`L2tlb_agent_agent_transport_sample` 的最小接口合同是：driver 在发布前通过 setter 填充 private `payload_data`，调用
`freeze()` 后只允许 consumer 通过 `get_payload(output payload_copy)` 读取；freeze 后任一 setter/write 都必须
`uvm_fatal`。`analysis_imp.write()` 是同步函数，返回即表示 monitor 已完成 epoch、4-state 和 diagnostic 处理；因此 reset
ack 只需等待带匹配 epoch+transport seq 的 reset sample 被处理，不需要另建 FIFO drain task。

mailbox 的 `transport_sample_seq` 与 DUT global sample 必须分离：前者由 driver 每次发布递增，即使 `sample_valid=0`、
`dut_sample_seq=0` 也不能重复；normal consumer 与 reset/abort drain 通过同一个 CAS 竞争 `CONSUMED/DROPPED` 的唯一终态。
driver 是唯一把 terminal slot 回收为 EMPTY 的写者；ENABLED topology 的 RESPONSE reset ack 只有 owner reset-done、本地 stale
item 清理和 mailbox EMPTY 全部成立后才能发送，driver 随后进入本 epoch reset-quiescent，不再发布新的 semantic mailbox sample，
防止 reset re-arm 留下旧 sample。

#### 当前状态更正（2026-08-10）

本回溯文档上方所有“四份 `undo` plan 均尚未 coding/验证”的表述均是当时的历史状态，保留用于解释问题演进，不能作为
当前项目状态。当前权威状态如下：

| 专项 | 当前路径与状态 |
|---|---|
| response random payload | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2tlb_response_random_payload_plan_20260729.md`；coding、compile、定向 smoke 和独立末轮 review 已完成。 |
| token timing correction | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2tlb_sfence_flush_token_timing_correction_plan_20260805.md`；coding、compile、smoke 和独立末轮 review 已完成。 |
| stage-aware live-entry invalidation | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_sfence_hfence_stage_aware_live_entry_invalidation_plan_20260804.md`；coding、静态检查、远端 compile、基础 smoke 与 real-dispatch smoke 已完成。 |
| range lookup/NAPOT | `AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_l2tlb_range_lookup_napot_plan_20260806.md`；仍是待执行专项。 |

单 owner 审核合同不改变这些专项的归档状态。后续 coding 仅可针对仍在 `plan/undo` 的专项执行；已归档的 `plan/do` 文档只用于
确认已实现行为、历史决策和回归边界，不能被重新解释成新的 coding 入口。本文中的旧 API、旧 reset/release 摘要也继续仅为
历史材料；当前实现语义以单 owner 审核稿和对应专项文件头的状态说明为准。
