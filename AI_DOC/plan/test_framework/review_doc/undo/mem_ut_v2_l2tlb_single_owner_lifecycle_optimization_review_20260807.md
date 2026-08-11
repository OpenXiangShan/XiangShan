# V2 L2TLB 单 Owner 生命周期优化问题与方案审核

| 项目 | 内容 |
|---|---|
| 文档类型 | `review_doc/undo`；问题回顾和优化方案说明，不是独立 coding plan |
| 目标版本 | V2 (`mem_ut_uvm_v2`) |
| 当前状态 | 审核文档，不独立承载 coding、编译或仿真；本文件不能作为任一关联专项已完成实现的证明，具体状态以各专项文件头和 implementation review 为准。 |
| 审核目标 | 在一个 testcase 只运行一个 L2TLB responder owner 的前提下，收敛生命周期、时基、flush、reset 和退出边界 |
| 语义边界 | `L2TLB_agent` 仍表示 DTLB -> L2TLB request 和 L2TLB -> DTLB response，不表示 L2Cache/PTW 下游模型 |
| 关联实现文档 | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2tlb_response_random_payload_plan_20260729.md`、`AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2tlb_sfence_flush_token_timing_correction_plan_20260805.md`、`AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2tlb_range_lookup_napot_plan_20260806.md`、`AI_DOC/plan/test_framework/plan/do/mem_ut_v2_sfence_hfence_stage_aware_live_entry_invalidation_plan_20260804.md` |

本文把待优化问题和优化后的行为分开描述。它不替代四份专项的字段、payload、range 或 stage matcher 方案；后续新增或
未完成的 coding 遇到生命周期冲突时以本文的审核结论为准，具体功能以对应专项的当前路径和文件头状态执行。

## 1. 术语与抽象功能说明

| 术语 | 当前含义 | 代码落点 | 示例 |
|---|---|---|---|
| `lifecycle owner` | 当前 testcase 中唯一维护 L2TLB request token、pending response 和 UID 回填生命周期的 sequence。 | `memblock_l2tlb_base_sequence`、`memblock_sync_pkg` | 一条 sequence 在整个 testcase 内服务所有真实 request fire。 |
| `claim` | sequence 登记为 owner 的动作；它不是一笔 DUT request。 | `try_claim_l2tlb_lifecycle_owner()` | 第二条 sequence 不能同时 claim。 |
| `owner_claimed_once` | 本 testcase 曾经成功 claim 过的永久标记；最终 release 后仍保持为 1。 | package 级 lifecycle state | 防止 release 后旧状态被第二条 sequence 接管。 |
| `token` | 每次 `req_valid && req_ready` 真实握手对应的一笔 response 账本。 | `pending_q`、`driving_req` | 两次相同 VPN fire 仍产生两个 token。 |
| `UID waiting instance` | 一个 UID 当前执行版本等待 L2TLB response 的动态实例。 | UID TLB record 的 `wait_state/wait_epoch` | 同一 UID reissue 时建立新的 waiting epoch。 |
| `request-fire marker` | 该 UID waiting instance 首次被 responder 观察到真实 request fire 的 global sample；0 表示尚未观察到。 | `uid_tlb_first_request_fire_sample_seq` | C4 只取消已经真实进入 DUT 的旧 waiting instance。 |
| `dispatch topology` | testcase 是否运行 dispatch live-entry 查表 flow 的固定模式。 | `dispatch_l2tlb_lookup_active` | 一个 testcase 启动后不能从 dispatch-active 改成 no-dispatch。 |
| `no-owner topology` | `MEMBLOCK_L2TLB_SEQ_EN=0`，不启动主动 responder、不 claim、不建立 response token。 | `seq_csr_common::get_l2tlb_seq_en()` | 不需要 L2TLB response 的 testcase 才能使用。 |
| `responder mode` | testcase 对 L2TLB responder 的静态选择：`ENABLED` 时必须有唯一 owner，`DISABLED` 时必须是零 owner。 | testcase lifecycle state | 它先于 `DEFAULT/EXPLICIT` start mode 判断。 |
| `no-dispatch` | dispatch live-entry flow 未启动；本轮合同下不建立 TLB live entry 和 raw fence 删除队列。 | `dispatch_l2tlb_lookup_active=0` | 如果此模式观察到 request fire，说明启动配置或 DUT 连接错误。 |
| `global sample` | testcase 内单调 DUT sample 编号；唯一由 CSR monitor 的 post-reset `posedge mon_cb` 推进一次，其它组件只读取。 | `advance_dut_global_sample()`、`peek_current_dut_global_sample()` | C0、C4、CSR history 和 raw event 使用同一编号。 |
| `sample-ready` | 当前 posedge 的 CSR history 与同 sample lifecycle event 都已发布，C-2 consumer 可以安全读取。 | `csr_history_published_seq`、`lifecycle_event_published_seq` | `drv_cb` 本身不保证 CSR monitor、fence monitor 已完成发布。 |
| `sample-ready probe result` | 当前 `drv_cb` 的同拍发布窗口结束后，helper 返回 `READY` 或 `NOT_READY`；`NOT_READY` 只结束当前拍，不跨 clock 阻塞。 | `wait_for_dut_sample_ready_at_drv_cb()` | warm-up 只送 inactive；下一拍重新锁存 VIF，不能沿用上一拍 item 的 fire/response。 |
| `sample anchor/probe` | driver 在唯一 `drv_cb` 时钟边界锁定本拍，再在同一仿真时刻内以固定 delta 上限等待 CSR/fence producer 发布；它不等待下一拍。 | `wait_for_dut_sample_ready_at_drv_cb()`、`MEMBLOCK_L2TLB_SAMPLE_PROBE_MAX_DELTA` | probe 成功后才允许读取 current sample；超出 delta 只返回 `NOT_READY`，连续超限由 watchdog 报错。 |
| `analysis_imp` | UVM 的同步 analysis 接收端；producer 调用 `write()` 时，consumer 在同一调用中处理 sample，不形成额外排队。 | `transport_sample_imp` | 本专项用它代替 transport FIFO；`write()` 返回表示该 sample 已完成 monitor 处理。 |
| `transport sample` | driver 唯一冻结的不可变 sample，包含同拍 4-state transport 值、`sample_valid/sample_ready_result`、`sampled_reset_active/sampled_reset_epoch`、frozen final proof 和上一 item 的 owner/generation/kind metadata；sequence 与 monitor 只能消费它。 | `memblock_l2tlb_drv_sample_t` | reset coordinator 唯一发布 live reset state，driver 唯一把它复制进 sample；reset/epoch、final proof 和 item owner 是 sample provenance，consumer 不能用当前 package owner、live reset 或下一拍状态回填。 |
| `sample TLM wrapper` | agent 内部 TLM 传递的 `uvm_object`，只包装一份已 freeze 的 `memblock_l2tlb_drv_sample_t`；payload 为 private，只能通过 getter 取得副本。 | `L2tlb_agent_agent_transport_sample` | driver analysis port、sequence mailbox 和 monitor analysis imp 同步观察同一 payload；freeze 后 consumer 不得修改。 |
| `sample mailbox` | driver 与 response-owner sequence 之间的单槽有界传输；每份 sample 必须有唯一终态。 | `EMPTY -> PUBLISHED -> CONSUMED/DROPPED -> EMPTY`、`transport_sample_seq` ack | slot 保存 wrapper handle；同一 sequence 在正常语义完成后、任何可能等待 driver 的 `start_item/finish_item` 前 CAS 写 CONSUMED，reset/abort 写 DROPPED；driver 在下一 `drv_cb` 唯一回收为 EMPTY。 |
| `post-reset baseline` | reset release 后当前 epoch 必须先完成的一次 `NORMAL/inactive` item 采样；它建立新 epoch 的无 fire/无 response transport 基线。 | driver local `post_reset_baseline_pending/baseline_sent_sample_seq/last_driven_is_post_reset_baseline`、共享 proof `l2tlb_post_reset_baseline_done_epoch/sample_seq` | driver 发送带本地 tag 的 NORMAL item 并记录发送 sample；严格在更晚真实 sample 以 `ready=0/fire=0/resp_valid=0` 写 proof；proof 前不允许确认 stop/final item，即使 parent 已请求 global stop。 |
| `item_done() transport release` | `try_next_item()` 取得的 UVM item 无论正常发送还是 stale 丢弃都必须完成一次 sequencer 握手释放。 | `seq_item_port.item_done()` | 清本地句柄不能代替 UVM item 完成，否则 sequencer 会被旧 item 卡住；NO_ITEM 不调用 `item_done()`。 |
| `metadata latch` | driver 对上一拍已驱动 item 的 kind、generation、reset epoch、baseline tag 及真实 ready/fire/resp_valid 的轻量本地快照；它不是 sequencer item 句柄。 | `last_driven_*` fields | `item_done()` 后仍保留该 latch 供下一 `drv_cb` 确认；清 latch 不得再次 `item_done()`。 |
| `4-state transport sample` | driver 从同一 `mon_cb` 采样并以 `logic` 保存的 0/1/X/Z 请求、ready、response 值；在确认无 X/Z 后才计算 fire。 | `memblock_l2tlb_drv_sample_t` 的 `logic` 字段、`validate_l2tlb_transport_sample_4state()` | X/Z 不能先转换为 2-state 0；final/stop 必须使用同一份冻结 sample。 |
| `C0/C4` | C0 是 monitor 观察 flush 的 sample；C4 是 V2 filter 完成 flush 的 due sample。 | flush event/barrier | C0 已 fire 的 request 在 C1-C3 仍可完成，C4 才取消未完成旧工作。 |
| `flush barrier` | 从 C0 到 C4 的延迟失效记录；C0 只建立 barrier，不立即删 token。 | `barrier_q` | barrier 到期时关闭 response fire 并取消旧 pending。 |
| `flush event history` | 按 sample 保存、只供 response owner 按顺序读取的有界 flush 事件队列；adapter 不读取该队列，而使用 raw fence 自带的 event/sample provenance。 | package event history、response owner cursor | ready 尚未开放时可只做启动 baseline；ready 已开放后 event 必须属于当前 sample，不能把 service 停顿积压的旧 event 回放为新的 C0。 |
| `cursor` | response owner 已处理到的 event 序号；不是 raw FIFO 的 pop 指针。 | `response_owner_event_cursor` | adapter 只消费 `raw_sfence_q`，不维护第二个 history cursor。 |
| `EVENT_SEQ_NONE` | 没有 response-side lifecycle event 的固定哨兵值。 | `memblock_sync_pkg::MEMBLOCK_L2TLB_EVENT_SEQ_NONE=0` | `DISABLED/NO_OWNER + NO_DISPATCH` 只发布 reason/watermark，不创建 event history 或 barrier。 |
| `acceptance-opened state` | 当前 reset epoch 中 responder 已真实生成过可接受 `next_ready=1` 的既有 active-event 时基标记。 | `memblock_l2tlb_base_sequence::acceptance_opened_since_reset` | 标记为 1 后，任何 `event.sample_seq < current_sample` 都是 producer/service 时序错误；普通 flush 不清它，不能以 ready opportunity 代替它。 |
| `pre-ready event baseline` | `acceptance_opened_since_reset==0` 时，对早于 current sample 的 event 做 cursor 对齐并启动保守 hold 的动作。 | response-owner event consumer、`pre_ready_hold_until_sample` | 该动作要求无 fire/response/token/barrier/UID waiting；不建立 C0/C4 cancel，但从 current sample 起保持 ready=0 共 4 拍，不用于 active responder 复原。 |
| `ready opportunity` | reset 或 flush/pre-ready hold 解除后，已经真实生成一拍合法 `next_ready=1` 的独立状态。 | `ready_opportunity_since_lifecycle_block` | reset/任何 hold 建立时清零；生成 `next_ready=1` 时置位，置位前不累计 idle-stop 诊断。 |
| `event history retire` | ENABLED topology 下 response owner 在 cursor 连续推进后回收已经消费的 event history 前缀。 | `retire_l2tlb_event_history_prefix()` | producer 只 append/merge；只有 owner 可 pop `event_seq <= cursor`，队列满直接 fatal。runtime reset 由 CSR monitor 按 direct-writer 合同清表；`NO_OWNER/NO_DISPATCH` 则不创建 history record。 |
| `runtime reset` | testcase 运行期间 DUT reset；不等于 testcase 初始建表。 | reset coordinator、reset epoch | 清理旧 token/live entry，但保留 owner 和 global sample。 |
| `reset epoch` | 一次 reset 的唯一编号；同一 reset 被多个 monitor 看到时只建立一次。 | `l2tlb_reset_epoch` | response/driver、fence monitor、adapter、CSR 各自对同一 epoch ack。 |
| `ack` | 某个职责已完成该 reset epoch 自己负责的清理的确认。 | `l2tlb_response_reset_ack_epoch`、`l2tlb_fence_reset_ack_epoch`、`l2tlb_adapter_reset_ack_epoch`、`l2tlb_csr_reset_ack_epoch` | 所有必需 ack 到齐前不得重新开放 ready；CSR 与 fence monitor ack 始终存在，response/adapter 按 topology 计入。 |
| `direct writer` | 一个运行期状态只能由其职责组件直接清理并写入 ack/proof；协调者只请求、等待和只读汇总。 | response owner、driver、fence monitor、adapter、CSR monitor | reset coordinator 不能替 driver 回收 mailbox，也不能替 adapter 清 raw FIFO。 |
| `reset-quiescent` | driver 已完成指定 reset epoch 的 stale-item/slot 清理并回 RESPONSE ack 后的本地保持态。 | `L2tlb_agent_agent_driver` 本地状态 | 仍驱动 inactive/reset sample，但不再发布 semantic mailbox sample 或重复写 ack；reset release 后由 baseline 重新打开。 |
| `sample producer barrier` | 一个 global sample 的必需采样者都已报告“本拍已处理”；即使本拍没有事件也必须报告完成。CSR/fence monitor 各自只写自己的 producer bit。 | `sample_producer_done_mask`、`lifecycle_event_published_seq` | CSR 和 fence monitor 都完成后，才允许 owner/adapter 解释该 sample；没有事件时只发布空 reason。 |
| `watermark` | 表示某个 sample 的 producer 已处理到这里的单调完成标记，不等于一定存在 flush event。 | `lifecycle_event_published_seq` | 无 CSR change/fence 的普通 sample 也会推进 watermark。 |
| `provenance` | raw event 或 response 携带的原始采样来源信息，用于关联和诊断，不自动改变语义。 | `sample_seq`、`lifecycle_event_seq` | adapter 用 raw fence provenance 计算 C4，不读取 response history。 |
| `reset coordinator` | 统一观察 `rst_n`、分配 reset epoch 并收集各职责 ack 的共享协调者；它不依赖 responder 是否启动。 | `begin_l2tlb_reset_epoch()`、`reset_required_ack_mask` | `NO_OWNER` 时仍由 fence/CSR/L2TLB monitor 各自完成 FENCE/CSR/MONITOR ack。 |
| `drain` | 某个职责拥有的 token、barrier 或 raw 工作已经清空并稳定。 | response/adapter drain helper | parent 还必须额外确认 raw-fence intake close，不能只凭两侧 queue drain 授权 release。 |
| `release grant` | parent 发给静止 owner 的最终释放授权。 | `l2tlb_release_granted` | driver 或 phase callback 不能自行制造 grant。 |
| `release-grantable` | 当前 owner 已满足可以由 parent 发放最终 release grant 的只读联合谓词；它不读写任何 grant 字段，也不清 claim。 | `release_grantable(owner_name, reset_epoch)` | parent 先以它为真写 grant；owner 再以“匹配 grant 且该谓词仍为真”原子 release，避免自等待。 |
| `owner admission-settled watermark` | owner 已在真实 `drv_cb` 完成本拍 request capture 与 UID registration 的完成标记；它不是 transport close。 | `l2tlb_owner_admission_settled_sample_seq` | parent 的 negedge global stop 只提出停止意图；owner 下一 posedge 先结算此前 `ready=1` 窗口的 fire，再写 admission seal。 |
| `release admission request` | parent 的 `global_stop_requested` 是停止意图；唯一 owner 在下一真实 `drv_cb` 已结算本拍 admission 后写入 close request，并在同一拍投递 `ready=0` stop item；它不等于 transport 已关闭。 | `l2tlb_release_admission_close_requested`、`request_owner_name`、`close_request_sample_seq`、`close_request_reset_epoch` | seal 前已 fire 的 UID/token 保持合法；flag 写入后的任一后续 helper 调用不得新建 UID，即使仍在同一 sample。 |
| `release admission close` | owner 生成的 stop item 经真实 `drv_cb` 采样后，由 driver 以冻结的 `sampled_req_ready=0 && sampled_req_fire=0` 确认的 transport 关闭状态；它先于 drain。 | `l2tlb_release_admission_closed`、`admission_owner_name`、`admission_closed_generation`、`cutoff_sample_seq` | cutoff sample 不产生新 fire；此前已真实 fire 的工作继续 drain，之后禁止新 token/UID。 |
| `release generation` | owner 每次建立 close request 时分配的 testcase 内单调编号；它绑定 stop item、final inactive item、closing 与 grant。 | `l2tlb_release_admission_close_generation` | reset 作废当前 close 状态但不回绕该编号，旧 item 不能匹配 reset 后新 release。 |
| `release item kind` | 仅在 sequence/driver transaction 中携带的本地 lifecycle metadata，不驱动 DUT wire；每个 stop/final item 同时冻结 owner、generation 和 reset epoch。 | `l2tlb_release_item_kind`、`l2tlb_release_item_owner_name`、`l2tlb_release_item_reset_epoch` | `RELEASE_STOP` 只确认 admission close；`RELEASE_FINAL_INACTIVE` 才允许置 final-inactive done；driver 必须比较冻结的 item owner，不能只比较当前 package owner；旧 epoch item 必须 `item_done()` 后丢弃。 |
| `grant reset epoch` | grant 写入时冻结的 reset epoch；grant 只能被同 epoch owner 消费。 | `l2tlb_release_grant_reset_epoch` | reset 与 global stop 重叠时，旧 grant 不能释放新 epoch owner。 |
| `event sequence baseline` | 不随 event history 清表的最后已分配 event 序号。 | `last_allocated_l2tlb_event_seq` | reset 后 cursor 对齐 baseline，下一条 event 从 baseline+1 分配。 |
| `release closing` | owner 已在真实 `drv_cb` 边界完成最终 inactive item、且 monitor 已同步确认该精确 sample settled、但尚未清 active claim 的短状态。 | `l2tlb_release_closing` | 只有该状态、grant 和 drain 同时成立时才可 release；`l2tlb_lifecycle_owner_claimed==0` 是 release 完成的唯一权威。 |
| `ack mask` | 当前启动拓扑下必须完成的 reset 清理职责集合；CSR、FENCE 和 MONITOR ack 始终存在，response/adapter ack 按拓扑加入。 | `reset_required_ack_mask` | `DISABLED/NO_DISPATCH` 仍等待 FENCE/CSR/MONITOR ack，不等待不存在的 response/adapter owner。 |
| `final inactive item` | 匹配 `RELEASE_FINAL_INACTIVE`、冻结的 item owner/current owner、generation/reset epoch 的 item，在真实 `drv_cb` 冻结并同时满足 `sampled_req_ready=0 && sampled_req_fire=0 && sampled_resp_valid=0`；仅 sequence `finish_item()` 返回不等于已完成。 | `final_inactive_item_done/final_inactive_transport_sample_seq` | 任一 metadata、冻结 owner provenance 或三项采样谓词不满足都 fatal；driver 确认该边界并同步发布 sample，monitor `write()` 返回并标记该序号 settled 后，才能建立 `release closing`。 |
| `monitor reset ackable` | monitor 已同步处理指定 epoch、指定 transport 序号的 reset-active sample，且不再处理 sample 的 reset ack 条件；该 seq 必须严格晚于上次 reset ack。 | `monitor_reset_sample_processed_epoch/transport_sample_seq`、`monitor_reset_ackable(epoch, reset_sample_seq)` | 它只决定 MONITOR reset ack，不要求 final inactive；reset ack tuple 与 final tuple 独立。 |
| `monitor final sample settled` | monitor 已同步处理指定 epoch、指定 transport 序号的 final inactive sample 的 release 条件。 | `monitor_final_settled_epoch`、`monitor_final_settled_transport_sample_seq` | 它只决定 final/release，不能代替 reset ack。 |
| `monitor active epoch` | monitor 自己接受 transport sample 的当前 reset epoch，由 reset request 更新。 | `monitor_active_epoch` | monitor 用它与 frozen `sampled_reset_epoch` 比较，不在消费 sample 时重新读取 live package reset。 |
| `reset ack tuple` | monitor 对 runtime reset 返回的 `{reset_epoch, transport_sample_seq}`；seq 来自本次 reset-active sample，且严格递增。 | `l2tlb_monitor_reset_ack_epoch/transport_sample_seq`、`monitor_reset_ack_floor_transport_sample_seq` | coordinator 同时核对 epoch 和 seq；该 seq 不复用 final settled seq。 |
| `frozen final proof` | driver 在 final-inactive confirm 成功的同一 `drv_cb` 写入 working sample 并 freeze 的完成元数据。 | `sampled_final_inactive_proof_valid/epoch/transport_sample_seq` | monitor 只从该 tuple 建 final settled；旧 epoch proof 不会跨 reset 复用。 |
| `re-arm` | 仅 `ENABLED` topology 的同一个 owner 在 runtime reset 后重新等待 CSR history warm-up 并开放 ready。 | L2TLB owner reset flow | re-arm 不重新 claim；`DISABLED/NO_OWNER` 不 re-arm。 |
| `NBA` | SystemVerilog nonblocking assignment 完成更新的仿真区域。 | `uvm_wait_for_nba_region()` | 采样接口后等待 NBA，再检查同一 sample 的 history。 |
| `live entry` | `tlb_entry_by_key` 中可被后续 lookup 复用的 canonical TLB response payload。 | `common_data_transaction` | dispatch-active fence 到 C4 删除 entry 和 range index。 |
| `adapter` | 将 dispatch raw fence 转成 live-entry 失效工作的组件。 | `dispatch_monitor_event_adapter` | fence monitor 负责入队；adapter 是 raw fence 唯一 destructive consumer。 |
| `raw-fence intake close` | fence monitor 已在完整处理一个 close request 之后的 raw sample 后，停止接收新的 raw fence 的证明；它与当前 reset epoch 和 close generation 绑定。 | `l2tlb_raw_fence_producer_settled_sample_seq`、`l2tlb_raw_fence_intake_closed_*` | C10 owner close；monitor 先处理 C11 最后一条可能由 stop 前驱动的 raw，再在 C11 close；C12 valid raw fatal。 |
| `final sample terminal ack` | final inactive item 被 driver 真实采样后形成的 frozen transport sample 仍占用单槽 mailbox；owner 必须先建立 closing，再把这份 sample 标为 `CONSUMED`。 | `sampled_final_inactive_proof_*`、`ack_l2tlb_transport_sample()` | F 拍 final proof/monitor settled/closing/ack；F+1 driver recycle，不能把 `finish_item()` 当成 terminal ack。 |
| `release-state wakeup` | 唤醒已经确认 final sample、但不再等待 semantic mailbox 的 owner 的 package 级 `uvm_event`；它不保存 grant、不是第二个 release state。 | `l2tlb_release_state_changed_ev`、`wait_for_l2tlb_release_grant_or_reset()` | parent 原子写完整 grant 后 trigger，reset coordinator 发布新 epoch 后 trigger；owner 醒来后重新读 canonical state。 |

#### 生命周期状态唯一命名

下表是本审核稿及其关联 `undo` plan 的唯一状态命名权威。实现只能声明“规范字段”列；后续文字伪代码为易读可使用
“阅读别名”列，但它们必须是同一字段的局部阅读别名，绝不能另建 state。历史文档中的
`l2tlb_lifecycle_release_closing` 一律视为 `l2tlb_release_closing` 的旧拼写，不得与之并存。

| 规范字段 | 阅读别名 | 作用与生命周期 |
|---|---|---|
| `l2tlb_owner_admission_settled_sample_seq` | `admission_settled_sample_seq` | 仅 owner 在 `sample_ready_result==READY` 的非 reset `drv_cb` 完成本拍 capture/UID 更新后置为当前 global sample；sample_valid=0、NOT_READY、baseline 未完成均不得写入；parent 只能读取，不能用 negedge 顺序冒充该证明；reset 和正常 release 均清除。 |
| `l2tlb_release_admission_close_requested` | `close_requested` | 仅 owner 在 `admission_settled_sample_seq==current_sample` 且 global stop 已请求后置位；任何后续 registration/capture 调用均非法。runtime reset 作废该请求。 |
| `l2tlb_release_admission_request_owner_name` | `request_owner_name` | close request 所属唯一 owner。 |
| `l2tlb_release_admission_close_request_sample_seq` | `close_request_sample_seq` | close request 写入时的 global sample，只作 provenance 和 stop-sample 新鲜度检查。 |
| `l2tlb_release_admission_close_reset_epoch` | `close_request_reset_epoch` | close request 所属 reset epoch；parent 只等待它等于 current epoch 的 close，不能复用 reset 前状态。 |
| `l2tlb_release_admission_close_generation` | `close_generation` | 每次 close request 单调递增；testcase-start 清零，runtime reset 不回绕。 |
| `l2tlb_release_admission_closed` | `admission_closed` | 仅当前 `close_generation` 的 `RELEASE_STOP` item 在真实 `drv_cb` 无 fire 时置位；它确认 transport close，不是软件 admission 的首次封口。 |
| `l2tlb_release_admission_owner_name` | `admission_owner_name` | 已确认 transport close 的 owner。 |
| `l2tlb_release_admission_closed_generation` | `admission_closed_generation` | driver 确认 stop 时写入的 generation；与 `close_generation` 不等立即 fatal，重复 stop 也 fatal。 |
| `l2tlb_release_admission_cutoff_sample_seq` | `cutoff_sample_seq` | 已确认 `RELEASE_STOP` item 的采样 sample。 |
| `l2tlb_raw_fence_producer_settled_sample_seq` | `raw_fence_settled_sample_seq` | 仅 fence monitor 在已处理本 sample 的 raw/event 后写入；它证明 producer 已经跨过该 sample，不消费 raw FIFO。 |
| `l2tlb_raw_fence_intake_closed` / `l2tlb_raw_fence_intake_closed_reset_epoch` / `l2tlb_raw_fence_intake_closed_generation` / `l2tlb_raw_fence_intake_cutoff_sample_seq` | `raw_fence_intake_closed` / `raw_fence_closed_epoch` / `raw_fence_closed_generation` / `raw_fence_cutoff_sample` | 仅 fence monitor 在 close request 后的首个完整 raw sample 写入；必须匹配 current epoch/current close generation。reset 和正常 release 清活跃字段；closed 后新有效 raw fence fatal。 |
| `l2tlb_release_item_kind` | `item_kind` | xaction/driver 本地 metadata：`NORMAL`、`RELEASE_STOP` 或 `RELEASE_FINAL_INACTIVE`；另有只与 NORMAL 组合的 `is_post_reset_baseline` tag；不进入 DUT interface。 |
| `l2tlb_release_item_owner_name` | `item_owner_name` / `sampled_item_owner_name` | sequence 写入 xaction、driver 在取得 item 后冻结并复制到 transport sample；final/stop 只能匹配同一 owner，不能在采样时从 live package owner 回填；reset/stale item 只丢弃，不确认 release。 |
| `l2tlb_release_item_generation` | `item_generation` | item 携带的 close generation，driver 仅接受与当前 generation 相等的 stop/final item。 |
| `l2tlb_release_item_reset_epoch` | `item_reset_epoch` | item 生成时冻结的 reset epoch；reset 中或 re-arm 后若未发送 item epoch 不是 current epoch，driver 必须 `item_done()` 后丢弃；已采样 latch 只忽略、不确认 stop/final，也不得二次 `item_done()`。 |
| `l2tlb_release_final_inactive_item_done` | `final_inactive_done` | 仅 `RELEASE_FINAL_INACTIVE` item 真实采样后置位。 |
| `l2tlb_release_final_inactive_generation` | `final_inactive_generation` | 证明 final inactive 属于当前 close generation。 |
| `l2tlb_release_final_inactive_transport_sample_seq` | `final_inactive_transport_sample_seq` | driver 在匹配 owner/generation/epoch 且真实采样满足 `ready=0/fire=0/resp_valid=0` 时，与 `final_inactive_done` 同时写入；monitor 只用该冻结序号确认精确 final sample settled，reset 时清除。 |
| `l2tlb_transport_sample_recycle_done_seq` | `mailbox_recycle_done_seq` | driver 在 final sample 已收到 CONSUMED/DROPPED 后，于后续真实 `drv_cb` 将该单槽从 terminal 回收为 EMPTY 时写入 final sample 的 transport 序号；它证明 release gate 不会等待尚未回收的 terminal slot，reset 时清除。 |
| `l2tlb_release_closing` | `release_closing` | owner 的 response drain 与匹配 final inactive 后建立；旧 `l2tlb_lifecycle_release_closing` 仅为历史别名。 |
| `l2tlb_release_closing_owner_name` / `l2tlb_release_closing_generation` | `closing_owner_name` / `closing_generation` | 证明 closing 归属当前 owner/current generation。 |
| `l2tlb_release_granted` | `release_granted` | parent 的一次最终授权，不关闭 admission、不清 claim。 |
| `l2tlb_release_grant_owner_name` / `l2tlb_release_grant_reset_epoch` / `l2tlb_release_grant_generation` | `grant_owner_name` / `grant_reset_epoch` / `grant_generation` | owner 只消费 owner、epoch、generation 全部匹配的 grant。 |

### 1.1 关键函数抽象功能

| 函数/task | 抽象功能 |
|---|---|
| `initialize_l2tlb_testcase_lifecycle()` | 在 testcase 启动前以 `responder_mode`、`topology`、`start_mode`、`needs_response`、`connect_takeover_active` 和 `reset_required_ack_mask` 为唯一输入锁定 lifecycle；相同输入可幂等返回，不同输入 fatal；不处理运行期 reset。ack mask 固定包含 `CSR/FENCE/MONITOR`，`RESPONSE/ADAPTER` 仅按 topology 加入。 |
| `try_claim_l2tlb_lifecycle_owner()` | 在静态启动条件已确认后登记唯一动态 owner；不创建 token，也不清理业务状态。 |
| `wait_for_dut_sample_ready_at_drv_cb()` | 在 L2TLB driver 的 posedge 采样边界锁存接口值，以本拍 anchor 在同一 `drv_cb` 内完成 bounded NBA/producer probe；anchor 成功后才读取 current sample，并返回 `sample_valid/sample_seq/READY/NOT_READY`；不跨 clock 阻塞。 |
| `wait_for_l2tlb_sample_anchor()` | 当 fence/其它 monitor 先于 CSR monitor 被唤醒时，等待 CSR monitor 为同一 `mon_cb` 发布 global sample；它只等待/读取，不推进 sample。 |
| `L2tlb_agent_agent_monitor::write()` / `write_transport_sample()` | `analysis_imp` 实际调用 `write(sample_ref)`，它只转调无等待的 `write_transport_sample(sample_ref)`；后者通过 getter 消费 driver 已冻结的 wrapper 副本，以 monitor-local active epoch 判断样本新旧，并只从 frozen final proof 建 settled，不再独立读取 transport VIF、live reset/final 状态、推进 sample 或重算 fire。 |
| `L2tlb_agent_agent::connect_phase()` | 在 agent 内部连接 driver 的 `transport_sample_ap` 到 monitor 的 `transport_sample_imp`；不新增 env/RM 外部连接。 |
| `get_owned_item_or_abort()` | 在每个真实 `drv_cb` 以 `try_next_item()` 非阻塞轮询一个 cycle item；无 item 返回 `NO_ITEM` 并驱动/采样 inactive，reset/phase 请求可在下一边界唤醒处理；已取得 item 由 `item_owned_by_driver` 标记，任何 stale/abort 分支恰好一次 `item_done()`。它不跨 clock 阻塞。 |
| `reserve_and_publish_l2tlb_transport_sample()` | driver 在调用同步 analysis port 前先以单槽状态原子预留并发布 wrapper handle，再同步调用 monitor；这样 reset/abort 观察到的 slot 不会在发布窗口中仍显示 EMPTY。它只发布 transport 事实，不执行 token/UID 语义。 |
| `recycle_l2tlb_transport_sample_at_drv_cb()` | driver 在每个后续 `drv_cb` 优先把已由 sequence 标记的 CONSUMED/DROPPED slot 回收为 EMPTY；若该 terminal 序号是 final sample，同时写 `mailbox_recycle_done_seq`。没有下一 item 时仍执行该回收，不等待 sequencer。 |
| `request_l2tlb_driver_service_wakeup()` | reset coordinator/owner 只登记下一 `drv_cb` 必须处理 reset/inactive 的请求；driver 通过非阻塞轮询观察该请求并继续发布 reset sample，coordinator 不直接修改 mailbox。 |
| `ack_l2tlb_transport_sample()` / `l2tlb_transport_sample_mailbox_empty()` | response-owner sequence 在正常纯软件语义完成后、任何 `start_item/finish_item` 前写一次 CONSUMED；同一 sequence 的 reset/abort drain 写一次 DROPPED。driver 在下一 `drv_cb` 校验并把 slot 回收为 `EMPTY`；release gate 和 RESPONSE reset ack 都只接受 mailbox empty。 |
| `discard_stale_l2tlb_item()` | 对已取得但 reset/epoch 已失效且不能驱动的 item 执行 inactive 驱动和一次 UVM transport 释放；不确认任何 lifecycle 状态。 |
| `get_l2tlb_request_csr_snapshot()` | 按 global sample 取得 V2 DTLB filter 所见的 C-2 CSR 深拷贝；缺失时返回无效。 |
| `note_l2tlb_flush_event()` | 在 `ENABLED + DISPATCH_ACTIVE` 将同 sample 的 CSR/fence 事实合并为一个有序 event，供 response owner 按 cursor 读取；`DISABLED/NO_OWNER + NO_DISPATCH` 只发布 per-sample reason/watermark 并返回 `memblock_sync_pkg` 的 `localparam longint unsigned MEMBLOCK_L2TLB_EVENT_SEQ_NONE=0`，不创建 history。adapter 只使用 dispatch-active raw fence 中冻结的 event/sample provenance。 |
| `mark_l2tlb_sample_producer_done()` | 为指定 global sample 登记 CSR 或 fence producer 已完成本拍采样；required mask 收齐后发布 `lifecycle_event_published_seq`；不推进 sample、不消费 raw queue。 |
| `begin_l2tlb_reset_epoch()` | 由共享 reset coordinator 只负责检测一次 `rst_n` 下降沿并分配/复用 `reset_epoch`；具体清理由 `reset_l2tlb_runtime_state()` 按 ack mask 执行。 |
| `reset_l2tlb_runtime_state()` | 按 reset epoch 和 ack mask 请求并等待各职责完成自己的清理：response owner 清 token/UID/barrier 并以 mailbox CAS drain 已发布 sample，driver 清本地 stale item并把 terminal mailbox slot 回收 EMPTY 后唯一写 response ack，L2TLB monitor 先更新 monitor-local active epoch 和 reset-ack seq floor、通过同步 `analysis_imp` 消费匹配 reset sample，只有 `monitor_reset_ackable(epoch, reset_sample_seq)` 才回 MONITOR ack，fence monitor 清 raw producer/context-dedup 并回 FENCE ack，adapter 清 raw/context/live entry，CSR monitor 清 history；coordinator 不直接清其它职责队列，只等待 ack，`NO_OWNER` 时仍完成 CSR/FENCE/MONITOR ack，不释放 owner。 |
| `record_l2tlb_flush_barrier()` | 把 C0 flush 转成 C4 due barrier，暂时关闭后续 admission，不立即取消 C0 已 fire 工作。 |
| `apply_due_l2tlb_flush_barriers()` | 在 C4 取消仍未完成的旧 token 和符合 marker 条件的 UID；不负责替 adapter 删除 live entry。 |
| `service_l2tlb_sfence_events()` | adapter 独占 raw fence 的 peek、解码、排程、pop 和 C4 live-entry/range-index 删除。 |
| `cancel_unbound_uid_tlb_records_at_release()` | 仅在 driver 已确认 admission cutoff，且 `pending_q`、driving slot、barrier 均为空时，由 response owner 将 `uid_tlb_first_request_fire_sample_seq==0` 的 `WAITING` UID 显式转为 `CANCELED` 并移出 bounded index；它证明该 UID 在本 epoch 从未形成真实 DTLB -> L2TLB request，不构造 response、不改 token，也不调用任何 DUT deq/apply helper。marker 非零的 `WAITING` 不得由此 helper 清理。 |
| `l2tlb_response_drain_done()` | 在上述 release-time unbound cleanup 之后，只读判断 response owner 的 pending/driving/barrier 与剩余 `WAITING` 是否收敛；残留的 marker 非零 `WAITING` 必须打印 uid/key/epoch/fire sample 后 fatal。cutoff 前的 marker=0 不属于 drain failure，不能提前取消。 |
| `dispatch_l2tlb_live_entry_drain_done()` | 只读判断 adapter 的 raw/context/pending invalidate 是否收敛；它不替代 raw producer close，no-dispatch 时由固定拓扑直接成立。 |
| `close_dispatch_raw_fence_intake_for_release()` | fence monitor 在完整处理 close request 后的一个 raw sample 后，写 current epoch/generation 的 raw producer close；它不 pop FIFO、不删除 live entry。 |
| `grant_l2tlb_final_release()` | parent 在 global stop 后继续 service，等待 owner 在当前 epoch 完成 close/stop；仅当 driver 已确认 admission closed 且指定 owner/current epoch 的 `release_grantable()` 成立时，原子写完整 owner/epoch/generation grant 后 trigger `l2tlb_release_state_changed_ev`；`NO_OWNER` 不写 grant，也不代替 owner 清理队列。 |
| `close_l2tlb_admission_for_release()` | owner 在真实 `drv_cb` 已完成当前 sample capture/UID 更新后写 close request，并要求本拍生成 stop item；它不直接确认 transport/cutoff、不清队列、不发 final inactive/grant。 |
| `mark_l2tlb_post_reset_baseline_at_drv_cb()` | driver 在 current epoch 的 NORMAL/inactive item 真实采样为 fire=0/ready=0/resp_valid=0 后写 baseline done proof；owner 只能读取该 proof，不能用 reset release 或 `finish_item()` 替代。 |
| `mark_l2tlb_final_inactive_at_drv_cb()` | driver 以真实 `drv_cb` 冻结的 `RELEASE_FINAL_INACTIVE` item、item-owner/current-owner provenance、generation/reset epoch、transport sample 序号和 `sampled_req_ready/fire/resp_valid` 结果确认 final inactive；三项必须均为 0，否则 fatal；它只写 final done/序号，不清 claim/grant。 |
| `release_l2tlb_lifecycle_owner()` | owner 在 global stop、已关闭 admission、final inactive、owner/epoch 均匹配的 closing/grant、response/adapter queue drain、raw-fence intake 已按当前 epoch/generation closed、current reset epoch ack 和 `!reset_active` 再次确认后原子清 active claim；保留 `owner_claimed_once`，失败不清 claim。 |
| `begin_l2tlb_release_closing()` | owner 在 admission 已关闭、response drain 已完成、driver 已于真实 `drv_cb` 边界完成最终 inactive item，且 `monitor_final_sample_settled(epoch, seq)` 已成立后建立 closing 状态；它不清 claim，也不驱动额外接口值。 |
| `wait_for_l2tlb_release_grant_or_reset()` | owner 在 final sample 已建立 closing 且 terminal ack 后，只等待 parent 的 grant 或 runtime reset；它使用 persistent wakeup 避免丢失同 delta grant，醒来后重新检查 canonical reset/grant，不读取 VIF、不等待新的 semantic mailbox。 |
| `release_grantable()` | 只读判断指定 ENABLED owner 与传入 epoch 是否等于当前 reset epoch、是否已停止 admission、完成最终 inactive sample、`monitor_final_sample_settled(epoch, final_transport_sample_seq)` 已成立、建立 closing、完成 response/adapter queue drain、完成匹配 generation 的 raw-fence intake close、`mailbox_recycle_done_seq == final_transport_sample_seq` 且 transport sample mailbox 已回收为 EMPTY，并已收齐当前 epoch 必需 ack；它不得读取 `release_granted` 或任一 grant metadata。parent 发 grant 与 owner 原子 release 前复核共用它，但 owner 的原子 release 额外检查匹配 grant。 |

## 2. 审核范围与总体结论

本审核只关注 L2TLB responder 的所有权和运行期生命周期。当前源码证据包括：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv:167-220`：`do_kill()` 直接 release，`body()` 在
  `enable` 判断、claim、局部初始化之间没有统一 testcase coordinator。
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv:222-393`：driver loop 使用 sequence 私有
  `sample_seq`，并在 C0 处理 flush、request fire 和 response。
- 同文件 `:570-668`：`record_flush_killed_request()` 和 `handle_l2tlb_flush_event()` 会在 C0 删除 pending 或伪造
  canceled token；selector 还用 latest event 序号拒绝 token。
- `mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_driver.sv:36-46`：`phase_ended()` 直接清 owner。
- `mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv:405-428,535-553,630-646`：当前 claim/release、
  sample 和 raw queue clear 都只有简单 latest/清零语义。
- `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv:322-333`：service 仍调用
  `drain_sfence_events()`；`dispatch_monitor_event_adapter.sv:843-850` 会直接 pop raw fence 并删表。
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_auto_build_main_table_base_sequence.sv:64-78`：global stop
  当前只等待 `flushSb` pending 清除。
- `tc_base.sv:60`、`tc_dispatch_real_smoke.sv:82-114`、
  `memblock_dispatch_real_smoke_vseq.sv:116-139` 和
  `memblock_dispatch_real_cancel_reconcile_vseq.sv:116-144`：default 与 explicit 启动路径并存，必须在 testcase
  启动拓扑上明确互斥。

总体结论：`responder_mode=ENABLED` 的 testcase 只 claim 一个 owner，`responder_mode=DISABLED` 的 testcase 必须保持零
owner；“单 owner”不等于“单 outstanding”。每次真实 request fire 仍须
独立建立 token；一个 response 仍可能按 DUT raw-hit 语义完成多个 UID waiting instance。需要修复的是状态所有权和
时序边界，不是把多个 request 合并成一个 response。

## 3. 待优化问题与优化后方案

### 3.1 `MEMBLOCK_L2TLB_SEQ_EN=0` 没有明确的 no-owner 边界

**待优化问题：** 当前 `memblock_l2tlb_base_sequence::body()` 在 `enable=0` 时直接返回（源码 `:182-186`），但
没有 testcase 级规则说明此时谁负责 response，也没有在 testcase 需要 L2TLB response 时提前报错。这样可能出现测试框架静默不启动
responder，DUT request 永久等待。

**影响：** `SEQ_EN=0` 不是“换一种 owner”，而是没有 owner。若主 flow 仍期待 DTLB request/response，失败会表现为后续
状态卡住，根因却被掩盖。

**优化后方案：** 在 `DEFAULT/EXPLICIT` 之前增加静态 `responder_mode=ENABLED/DISABLED`。将
`MEMBLOCK_L2TLB_SEQ_EN=0` 明确定义为 `DISABLED/NO_OWNER`：不启动 responder、不 claim、不建 pending token、不发
release grant。testcase coordinator 在主 flow 启动前声明是否需要 L2TLB response：

```text
testcase_start:
  读取 l2tlb_seq_en 和 connect takeover capability，锁定 responder_mode。
  若 l2tlb_seq_en == 0：登记 DISABLED/NO_OWNER。
    若 testcase/主 flow 声明需要 L2TLB response：立即 uvm_fatal；不要启动会等待 response 的 flow。
    否则不配置 default/explicit responder、不 claim、不发送 release grant，response drain 固定为真。
  若 l2tlb_seq_en == 1：登记 ENABLED；先确认 connect takeover active，再进入 owner 启动流程。
  NO_OWNER 期间（非 reset sample 且完成 X/Z 诊断后）由 L2TLB monitor 检查 req_valid；只要看到 req_valid==1 就立即 uvm_fatal，打印 sample、VPN、s2xlate 和启动拓扑。
  不等待 req_valid&&req_ready fire，因为 disabled driver 的 ready 固定为 0，等待 fire 会永久等不到错误。
```

`NO_OWNER` 与 `NO_DISPATCH` 分开：前者由 sequence enable 决定；后者只表示 dispatch live-entry flow 未开启。当前支持矩阵中
`NO_DISPATCH` 只能与 `DISABLED/NO_OWNER` 配对；若 testcase 确实需要 responder，必须使用 `DISPATCH_ACTIVE`，不能把
`dispatch_l2tlb_lookup_active=0` 当作无条件兼容模式。

本轮最小支持范围进一步收紧：普通 `NO_DISPATCH` testcase 必须选择 `DISABLED/NO_OWNER`，不允许 L2TLB default sequence
仅靠 `idle_stop_cycle` 自行退出。若未来需要 standalone responder，必须新增该 testcase 的明确 parent，负责 stop、
response drain、最终 inactive 和 release grant；在该 parent 出现前，不把 standalone responder 当作当前可执行 flow。

### 3.2 claim 时点晚于启动校验且可能被后续初始化覆盖

**待优化问题：** 当前 sequence 先 `configure_from_plus()`、`ensure_context()`、检查 connect，再 claim，之后又调用
`initialize_lifecycle_state()`（源码 `:193-201`）。未来若初始化 helper 扩展为清 package 状态，可能在刚 claim 后清掉 owner；
同时 CSR warm-up 需求容易被误写成 claim 的前置条件。

**影响：** owner claim 的生存边界不稳定，第二条 sequence 可能在第一条尚未完整初始化时进入；CSR 尚未就绪也可能导致 claim
被延迟，造成启动拓扑判断不确定。

**优化后方案：** 所有需要读取 plus 配置的 testcase 在 `build_phase` 调用一次
`seq_csr_common::reload_from_plus()`；该函数完成命令行参数读取、校验和 getter 状态初始化。`end_of_elaboration_phase`
只调用 testcase lifecycle initializer，不读取 plus、不执行 task。initializer 的 `responder_mode`、`topology`、
`start_mode` 和 `needs_response` 都是显式输入，也是该状态的唯一写入入口；已初始化且所有输入完全相同则直接返回，
输入不同则 fatal，不得重复清 owner/sample。sequence 私有队列初始化完成后立即 claim。
claim 不等待 CSR history warm-up。claim 成功后 owner 保持 active，但在 history 未 ready 前只驱动 inactive/ready=0。

```text
testcase build/start:
  build_phase -> seq_csr_common::reload_from_plus()
  end_of_elaboration_phase -> initialize_l2tlb_testcase_lifecycle(
      responder_mode, topology, start_mode, needs_response, connect_takeover_active, reset_required_ack_mask)
  若已经 initialized 且所有输入相同：直接返回；不同：fatal
  校验 responder_mode、SEQ_EN、connect takeover、default/explicit 唯一路径

owner start:
  读取 enable，校验 testcase start mode；DISABLED 时不得到达本分支
  校验 connect takeover、VIF 和 data context
  清理自己的 pending/driving/计数器（不清 package claim）
  立即 try_claim；失败即 fatal
  等 CSR history C-2 warm-up；期间 ready=0，不创建 token
  history ready 后才开放 ready
```

### 3.3 default sequence 与 explicit virtual sequence 可能双启动

**待优化问题：** `tc_base.sv:60` 配置 L2TLB default sequence；real smoke/cancel virtual sequence 又在各自
`start_core_dispatch_flow()` 中用 `uvm_do_on` 启动 `memblock_l2tlb_base_sequence`。当前 claim 失败只能在第二条 sequence
已经启动后诊断，不能明确指出启动配置错误。

**影响：** 两条 sequence 可能争用同一 sequencer，或在 owner claim 前后各自创建本地状态；即使最终 claim 阻止双驱动，报错位置也晚且
可能留下 driver item。

**优化后方案：** testcase start coordinator 固定 `DEFAULT` 或 `EXPLICIT` 模式并在启动前拒绝另一条路径：

```text
DEFAULT 模式：保留 agent main_phase default sequence；所有 vseq 不启动 L2TLB sequence。
EXPLICIT 模式：将 L2TLB default sequence 设为 inert/不配置；只允许一个 vseq 分支 uvm_do_on。
若两种模式都可达：在首个 sequence start 前 uvm_fatal，而不是等待 claim 失败。
```

`owner_claimed_once` 是运行期最后一道保护，不是启动拓扑检查的替代品。`tc_base`、
`tc_dispatch_real_smoke`、`basicTest + VSEQ_MAIN` 和 cancel vseq 必须显式登记自己的模式。

`basicTest` 不能仅因某个类继承 `virtual_base_sequence` 就假定它使用 explicit L2TLB owner。解析 `VSEQ_MAIN` 后，
testcase/vseq capability 必须明确返回 `DEFAULT`、`EXPLICIT` 或 `DISABLED`，再由 testcase coordinator 配置唯一启动路径。

`initialize_l2tlb_testcase_lifecycle()` 的抽象职责是建立 testcase 级静态合同，而不是启动 sequence 或处理 runtime
reset。它的输入必须完整包含 `responder_mode`、`dispatch_topology`、`start_mode`、`needs_response`、
`connect_takeover_active` 和 `reset_required_ack_mask`；其中 `CSR`、`FENCE` 和 `MONITOR` ack 必须始终存在，`RESPONSE`/`ADAPTER` ack 是否加入
由固定 topology 决定。函数先验证支持矩阵和 response 需求，再一次性写入 topology、owner-start 状态和 ack mask；如果
已经初始化，只有输入元组完全相同才幂等返回，任何字段不同都 `uvm_fatal`。它不 claim owner、不清 token、不推进 global
sample，也不因 runtime reset 再次执行。

```text
initialize_l2tlb_testcase_lifecycle(inputs):
  校验 responder_mode、dispatch_topology、start_mode 与 connect takeover 的组合。
  校验 DISABLED/NO_OWNER 不能声明 needs_response；ENABLED 必须配 DISPATCH_ACTIVE。
  计算并校验 reset_required_ack_mask，强制包含 CSR、FENCE 与 MONITOR；RESPONSE/ADAPTER 只按 topology 加入。
  若尚未 initialized：保存全部输入，清 testcase-start 专属状态并置 initialized=1。
  若已 initialized 且输入逐项相同：直接返回，不重复清理或 claim。
  若已 initialized 但任一输入不同：uvm_fatal，固定 topology/lifecycle 不允许重写。
```

### 3.4 local sample 不能作为跨组件 C-2/C4 时基

**待优化问题：** L2TLB sequence 在 `drive_l2tlb_loop()` 中执行 `sample_seq++`（源码 `:222-229`），而 CSR/fence/其它
monitor 使用 package `get_dut_sample_seq($time)`。同一 posedge 因调用先后不同可能得到不一致的 sample；package 当前又只有
latest event，没有历史队列。

**影响：** request fire、CSR C-2 snapshot、C0 anchor、C4 due 和 adapter 删除边界可能错拍；服务暂停时中间 flush event 还会
丢失。

**优化后方案：** 新增 `advance_dut_global_sample()`，只允许 CSR monitor 在每个 post-reset `posedge mon_cb` 调用一次；
它推进 global sample 并为该 sample 建立 CSR history。fence、redirect、ctrl 等同拍 monitor 先通过
`wait_for_l2tlb_sample_anchor($time)` 等待 CSR monitor 发布同一 `mon_cb` 的 sample，再调用
`peek_current_dut_global_sample()` 取得编号并写入 raw。L2TLB driver/sequence、adapter 与 negedge service 一律只读
`peek_current_dut_global_sample()`，不得维护本地计数或再次推进 sample；若在完成同拍 NBA/发布屏障后仍没有该 sample，才
`uvm_fatal`，不能自行补推进。
旧 API 不得留下第二套语义：`get_dut_sample_seq()` 不能改成任何调用者都可推进的兼容接口，必须删除或仅在编译期迁移期报错；
`peek_latest_dut_sample_seq()` 如暂时保留，只能无副作用地转调 `peek_current_dut_global_sample()`，不得维护另一份 latest。
coding 必须迁移当前所有调用点：CSR monitor 改为唯一 `advance` 写者；`fence_agent_agent_monitor`、
`redirect_agent_agent_monitor`、`io_mem_to_ooo_ctrl_agent_agent_monitor` 和任何新增 posedge raw producer 改为
`wait_for_l2tlb_sample_anchor()+peek`；`memblock_lsqenq_dispatch_base_sequence`、
`common_data_transaction` 与 pending-MMIO soft-test 等只读 consumer 改为 `peek`。迁移完成后工程内不得再有
`get_dut_sample_seq()` 调用；这是全局时基 API 的一次性替换，不是只改 L2TLB sequence 的局部重命名。

### 3.5 `drv_cb` 不保证 CSR history 已发布，需要 sample-ready barrier

**待优化问题：** L2TLB interface 的 `drv_cb` 是 posedge clocking block（`L2tlb_agent_agent_interface.sv:84-152`）。到达
`@drv_cb` 只能说明接口采样边界到了，不能保证 CSR monitor 已在同一 posedge 完成 history 写入；当前 sequence 直接在 NBA 后读取
latest runtime CSR（`memblock_l2tlb_base_sequence.sv:260-308`）。

**影响：** 本拍 request 可能使用未发布或旧的 CSR；在 reset 后更可能用 reset 前 snapshot 误开放 ready。

**优化后方案：** 新增抽象 task `wait_for_dut_sample_ready_at_drv_cb()`，其职责是在 driver 已锁存当前 VIF 后，以本拍
clocking-block anchor 在同一个 `drv_cb` 的 NBA/producer 发布窗口内先确认 CSR monitor 已建立本拍 global sample，随后才读取
`peek_current_dut_global_sample()` 并探测 history 与 CSR/fence lifecycle event 是否已经发布。固定最多执行
`MEMBLOCK_L2TLB_SAMPLE_PROBE_MAX_DELTA=2` 次 delta/NBA probe；不允许等待下一 clock。它返回
`sample_valid/sample_seq/READY/NOT_READY`，不跨到下一 clock edge 阻塞、不推进 sample，也不创建 token。`READY` 时 owner 才解释本拍；
`NOT_READY` 或 sample 未建立时本拍只送 inactive/warm-up item，下一拍重新锁存 VIF。这样不会为了等 sideband 而复用旧接口值、
也不会在 driver 早于 CSR monitor 时把上一 sample 误当成本拍。

```text
在 drv_cb：
  driver 是唯一物理 transport sampler：只在 `@drv_cb` 等待一次，并从同一 posedge 的 `mon_cb` latch 读取 req/ready/response；
    sequence 不再独立等待 `@drv_cb` 或读取 VIF。driver 先锁存 drv_cb 的 req/response/item metadata，不读取下一拍 VIF。
  将本拍 anchor 传入 helper；helper 先在同一 drv_cb 的 NBA/协调窗口内等待一次 CSR anchor/sideband 发布机会，
    anchor 成功后才 peek current global sample，并返回 sample_valid、sample_seq 与 READY/NOT_READY；helper 不等待下一 clock edge，不自行推进 sample。
  若 reset/inactive：发送 inactive，不解释 request。
  若 sample_valid=0：要求冻结的 req_fire=0、resp_valid=0，否则 uvm_fatal；只送普通 inactive，不写 admission-settled/close；立即 continue。
  driver 在 reset release 后只通过 transport sample 发布 baseline_required/baseline_proof_pending；sequence 不读取 driver local flag。
  若上一拍 metadata latch 带 baseline tag：driver 仅在 sample_seq > baseline_sent_sample_seq 的本拍以 frozen ready=0、fire=0、resp_valid=0 写 baseline done proof。
  若 sample.baseline_required=1 且 sample.baseline_proof_pending=0：sequence 发送并在 transaction 中 tag current epoch NORMAL/inactive baseline；done proof 只能在下一有效 drv_cb 写入，
    本拍不建立 RELEASE_STOP；proof pending 时不得重复发 baseline 或建立 stop/final。
  若 sample_ready_result=NOT_READY 且 global sample 已建立：若任一 watermark > sample 或两者均 == sample，uvm_fatal；
    只有至少一个 watermark < sample 才是合法 NOT_READY。要求冻结的 req_fire=0、resp_valid=0，否则 uvm_fatal；
    只送普通 inactive；不得在该拍建立 RELEASE_STOP；连续 NOT_READY 超过 8 个 sample 则 watchdog fatal，立即 continue 到下一 drv_cb。
  若 sample_ready_result=READY：检查 csr_history_published_seq == sample 且 lifecycle_event_published_seq == sample；
    两者任一不满足说明 probe 实现违反同拍返回合同，uvm_fatal，不能再跨拍等待。
  event watermark 已发布后先保存旧 owner event cursor；仅 current sample 的合并 event 可以按原始 C0 建 barrier。
  `acceptance_opened_since_reset=0` 且无 active work 时，旧 event 只作 cursor baseline；该标记为 1 后旧 event 或任意 future event 均 fatal，
  随后用回放前的旧 cursor 归属 C0 request fire。
  negedge service 只 peek sample，不调用 get。
```

CSR monitor 是 global sample 和 CSR history 的唯一写者，也是唯一允许调用
`advance_dut_global_sample()` 的组件。其它真实 posedge producer 不得调用任何会分配或推进 sample 的旧 `get` helper；它们
只能在 CSR monitor 已建立当前 sample 后调用 `peek_current_dut_global_sample()`，再向 sample producer barrier 报告自身已完成。
因此 producer 的调度顺序不会改变 sample 编号，缺少当前 sample 时也不能由其它 producer“补推进”。

`wait_for_l2tlb_sample_anchor()` 的抽象功能是消除同一 posedge 中 monitor 的调度先后差异。调用者传入本次
`mon_cb` 的采样时间，只等待 CSR monitor 对该时间建立 global sample；它不推进 sample、不发布 CSR、不设置 producer done。
在当前 sample 的 NBA/协调窗口结束后仍未观察到匹配 anchor，才判定 CSR monitor 未按合同工作并 `uvm_fatal`。

`mark_l2tlb_sample_producer_done()` 的抽象功能是收集当前 sample 的 producer 完成位，而不是收集事件数量：

```text
mark_l2tlb_sample_producer_done(sample_seq, producer_kind):
  要求 sample_seq == peek_current_dut_global_sample()；否则 uvm_fatal。
  要求 producer_kind 属于当前 topology 的 required producer mask；否则 uvm_fatal。
  若该 producer 在本 sample 已报告完成：uvm_fatal，禁止重复推进其 bit。
  设置 sample_producer_done_mask[producer_kind]。
  若 required mask 尚未收齐：保持 lifecycle_event_published_seq 不变并返回“未 ready”。
  若 required mask 收齐：发布本 sample 的合并 reason_mask 和 lifecycle_event_published_seq；若本拍没有任何 reason，
                         只发布空 event-ready watermark，不向 event history 追加空记录，返回“ready”。
  不调用 advance、不 pop raw fence、不建立 token、不删除 live entry。
```

这里“本拍没有事件也要报告完成”是屏障的关键：例如 fence monitor 本拍没有 `sfence.valid`，仍需把
`FENCE_PRODUCER_DONE` 置位；这只表示它已经确认本拍没有 fence，不会凭空生成 `FENCE` reason。

### 3.6 runtime latest 不能替代固定深度 C-2 history

**待优化问题：** 当前 `publish_runtime_csr_snapshot()` 只在 payload changed 时更新（`memblock_sync_pkg.sv`），
L2TLB sequence 的 `drain_csr_runtime_events()` 读取 latest。V2 filter 使用两级顶层 CSR pipeline，response-visible CSR
必须按当前 sample 回看 C-2。

**影响：** CSR 变化后，request/response 可能使用错误的 ASID/VMID；UID 回填和 raw hit 结果会随 service 调度变化。

**优化后方案：** CSR monitor 每个 post-reset sample 都写固定深度 history（V2 至少保存当前和前两拍），同时维护一般 runtime
latest；在 CSR/fence 对同 sample 的 lifecycle event 合并完成后，才写 `csr_history_published_seq` 与
`lifecycle_event_published_seq`。`get_l2tlb_request_csr_snapshot(sample)` 只返回 `sample-2` 的深拷贝；缺失时在 ready 未开放阶段
保持 inactive，已 fire 后缺失则 fatal。UID 回填使用 response fire sample 的 C-2，不使用 UID issue-time CSR。

### 3.7 flush event 只有 latest，不能支持多 consumer 和 service 停顿

**待优化问题：** 当前 `note_l2tlb_flush_event()` 仅递增 `l2tlb_flush_event_seq` 并覆盖 latest（`memblock_sync_pkg.sv:~417-553`）；
CSR 与 fence 同 sample 可能重复产生 event。早期方案还把 event history 当成 active responder 可任意补回放的 backlog，service 停顿后会
用旧 C0 影响已开放 ready 的新 token/UID。

**影响：** response owner 若跳过中间 C0，会漏建 token barrier；若反过来在 ready 已开放后补回放旧 C0，又会立即执行已经过期的 C4 cancel，
错误取消该旧 anchor 之后新建的 token/UID。raw fence 与 lifecycle event 的 provenance 也可能错配，从而在错误的 sample 删除
live entry 或错误地保留旧 token。

**优化后方案：** `note_l2tlb_flush_event(sample, reason)` 保存有界 event history；同 sample 只合并 reason mask，不重复创建
event；CSR/fence monitor 都完成本 sample 发布后，由 lifecycle event publisher 写 `lifecycle_event_published_seq=sample`。只有
response owner 按 `response_owner_event_cursor` 顺序读取；adapter 不读取或回收该 history，只消费 raw fence FIFO，并使用 raw
冻结的 `lifecycle_event_seq/sample_seq` 做关联和诊断。history 满时直接 fatal，避免 producer 替 response owner 回收；
`NO_OWNER/NO_DISPATCH` 没有 response consumer 时，只发布 reason/watermark，不创建或追加 response event history record。

event consumer 在修改 barrier/token/UID 前执行 freshness gate：`event.sample_seq > current_sample` 一律 fatal；
`event.sample_seq == current_sample` 才按原始 C0 建 barrier；`event.sample_seq < current_sample` 只在本 reset epoch 从未采到过
`req_ready=1`、且当前无 ready/fire/response、pending/driving/barrier/active hold/WAITING UID 时作为 pre-ready baseline。连续旧 record 必须在一次
cursor 扫描中作为一个 baseline batch 对齐并只设置一次 `pre_ready_hold_until_sample >= current_sample + 4`；若其后出现 current-sample record，
仍必须按原始 C0 建 barrier，不能被 baseline 吞掉。只有本次没有 current-sample event 时才立即 retire baseline batch 并结束本拍；否则在
完整 C0-C4 语义结算后统一 retire。在此边界前禁止首次 `next_ready=1`，同时清 `ready_opportunity_since_lifecycle_block` 和 idle 诊断。只要 `acceptance_opened_since_reset != 0`，迟到 event
必须立即 fatal，不能登记已过期 due 后同拍 cancel。response owner 在本拍语义处理后唯一回收 `event_seq <= cursor` 的 history 前缀；
producer 只 append/merge，history 满且无可回收前缀 fatal。这样 event history 保留 startup/reset warm-up 的保守 4 拍对齐能力，
但不会把 active responder 的 service 停顿伪装成可恢复的正常时序。

### 3.8 C0 同拍 flush 不能立即 kill token

**待优化问题：** `handle_l2tlb_flush_event()` 在 C0 删除 `pending_q`，并调用 `record_flush_killed_request()` 伪造 canceled token
（`memblock_l2tlb_base_sequence.sv:570-616`）。`select_due_response()` 还以 token 的旧 event 序号直接 fatal
（`:618-668`）。

**影响：** C0 的 `req_valid && req_ready` 已经被 DUT 接收，却被框架当作未接收；C1-C3 合法 response 被漏掉。

**优化后方案：** C0 只创建 `flush barrier(anchor=C0,due=C4)` 并关闭后续 ready；C0 真实 fire 仍创建独立 token。C1-C3
允许旧 token response fire；C4 选择器禁止产生本拍 external response，仍 pending 的旧 token 才转 canceled。UID 只取消
`WAITING && marker!=0 && marker<=anchor`，marker=0 保持 waiting，不猜测其是否进入 DUT。C4 若已存在将要 fire 的 response，先在
上一拍 selector 排除；当前 sample 仍检测到 valid fire 则 fatal，不能把错误的 driver 时序伪装成成功或取消。

### 3.9 raw fence 消费者与 dispatch topology 混淆

**待优化问题：** `collect_runtime_context_events()` 仍调用 `drain_sfence_events()`（`memblock_dispatch_base_sequence.sv:322-333`），
adapter 直接 pop raw 并调用 `apply_raw_sfence()`（`dispatch_monitor_event_adapter.sv:843-850`）。这与“adapter 唯一 destructive
consumer、C4 执行删除”的目标冲突。

**影响：** raw fence 在 C0 被提前删除 live entry；response owner 和 adapter 还可能双重消费同一个 raw queue。

**优化后方案：** dispatch-active 时只有 adapter 负责 `peek -> decode -> schedule -> pop -> C4 delete`；response owner 只消费
独立的 flush event history，维护 token/barrier/UID，不读取 raw fence FIFO。`collect_runtime_context_events()` 只同步 CSR，service
loop 每个 sample 调用一次 `service_l2tlb_sfence_events()`。

`raw_sfence_q` 的职责必须拆开描述：fence monitor 是唯一 raw producer，按 `dispatch_l2tlb_lookup_active=1` 将已带 global
sample 的 raw 入队；adapter 是唯一 destructive consumer，负责其 `peek/decode/schedule/pop` 和 C4 删除。adapter 不消费
`flush event history`，只验证 raw 自带的 `lifecycle_event_seq/sample_seq` provenance；response owner 与 parent 都不得替代二者
入队、pop 或清队列。

### 3.10 no-dispatch、no-owner 和 connect capability 没有分层

**待优化问题：** 当前 `l2tlb_responder_active` 只表示 connect takeover，`MEMBLOCK_L2TLB_SEQ_EN` 只在 sequence body 内决定
是否 return；三者没有形成静态拓扑合同。

**影响：** sequence 不运行、connect 不接管、dispatch 不建表三种情况可能被混为一谈，导致错误等待或无 owner 运行。

**优化后方案：** 三个状态分开检查，并用固定支持矩阵避免等待不存在的职责：

```text
SEQ_EN=0：NO_OWNER，不 claim；主 flow需要 response -> 启动前 fatal。
SEQ_EN=1 且 connect takeover=0：配置错误，启动前 fatal。
dispatch_l2tlb_lookup_active=0：NO_DISPATCH，不入 raw fence FIFO，不建 live entry；在 DISABLED/NO_OWNER 下 monitor 看到
  req_valid==1 即 fatal，不等待 request fire。
dispatch_l2tlb_lookup_active=1：允许 adapter 建表/消费 raw fence；唯一 responder owner仍单独维护 token。
```

当前支持矩阵：

| responder mode | dispatch topology | 本轮状态 | 必需 reset ack |
|---|---|---|---|
| `DISABLED/NO_OWNER` | `NO_DISPATCH` | 支持；不启动 responder sequence、不建立 live entry；driver 保持 passive sampler，固定 inactive 并监测非法 req_valid | `CSR + FENCE + MONITOR`；adapter/response 标记 N/A |
| `ENABLED` | `DISPATCH_ACTIVE` | 支持；一个 responder owner 加一个 adapter | `CSR + FENCE + MONITOR + RESPONSE + ADAPTER` |
| `ENABLED` | `NO_DISPATCH` | 当前不支持；需要 standalone parent 专项 | 启动前 fatal |
| `DISABLED` | `DISPATCH_ACTIVE` | 当前不支持；不能让 dispatch flow 静默产生未响应 request | 启动前 fatal |

其中 `reset_required_ack_mask` 由 testcase initializer 一次写入，reset coordinator 只按该 mask 等待；不能等待不存在的
sequence 或 adapter。普通 no-dispatch testcase 使用第一行；未来 standalone responder 必须另建 parent 后才能使用第三行。

四份关联专项 plan 必须共享这张矩阵。不得再把 `ENABLED + NO_DISPATCH` 写为“只有 response drain 的现有支持路径”；它是
未来 standalone parent 专项完成前的启动前 fatal。

`NO_OWNER` 与 `NO_DISPATCH` 不是同义词：前者没有 responder，后者是 dispatch live-entry flow 的固定关闭状态。若某个
testcase 需要真正的 L2TLB response，必须显式选择 responder-active topology，不能静默依赖 no-dispatch。

`CSR`、`FENCE` 和 `MONITOR` 是所有支持拓扑的必需 reset producer/ack。即使是 `DISABLED/NO_OWNER + NO_DISPATCH`，CSR/fence/L2TLB monitor 和共享 reset
coordinator 仍然运行并清理各自 history/producer context、发布 reset epoch ack；该 ack 不依赖 L2TLB responder sequence、driver ready 或
dispatch adapter。只有 `RESPONSE` 和 `ADAPTER` ack 可以因拓扑被标记为 N/A/已满足。这样 no-owner 不会因等待不存在的
sequence 卡住 reset 收敛，也不会被错误地当成一个空 responder。

global-stop parent 必须只读取已经固定的 `l2tlb_responder_enabled()`、`l2tlb_dispatch_active()` 和
`l2tlb_testcase_needs_response`，不得再从 connect takeover `l2tlb_responder_active` 或 runtime plus
`MEMBLOCK_L2TLB_SEQ_EN` 推导当前 topology。`DISABLED/NO_OWNER + NO_DISPATCH` 直接结束 parent 的 L2TLB 等待分支，
不等待 claim、不发 grant；反之 enabled/dispatch-active 但没有 response 需求或没有 dispatch 都是初始化合同错误，必须 fatal。

### 3.11 runtime reset 没有统一 epoch 和 ack 闭环

**待优化问题：** 当前 reset 分支只在 L2TLB sequence 内调用 `cancel_outstanding_by_reset()` 并清本地 flags（`:265-287`）；
`clear_raw_monitor_queues()` 还会清 `dut_sample_seq`（`memblock_sync_pkg.sv:630-646`）。没有 response、adapter、CSR 三方
对同一次 reset 的 ack，也没有 reset release 后的统一 re-arm 条件。

**影响：** reset 前 token 的旧 due 可能在 reset 后误删新 entry；sample 归零会使旧 event 与新 event 复用相同编号；adapter
pending invalidate 或 raw fence 可能残留。

**优化后方案：** `reset_backend_done` 只表示初始启动完成，不能作为 runtime reset 检测来源。由始终运行的共享 reset
coordinator 在采样到 `rst_n` 下降沿时建立唯一 `reset_epoch`；同一次低电平期间其它观察者复用该 epoch，
即使当前 topology 为 `DISABLED/NO_OWNER` 也能完成 CSR/FENCE/MONITOR reset ack。testcase-start 初始化 `reset_epoch=0` 时，
`required_reset_acks_done(0)` 必须定义为真，表示初始状态已直接建立而不是等待一次不存在的 runtime reset；从首个
runtime reset 起才按递增 epoch 等待实际 required ack。
若 `reset_required_ack_mask` 含 RESPONSE，response owner 独占清 token/UID/barrier 并发布 owner-reset-done，driver 清已取得的本地 stale item后唯一写 response ack；若含 ADAPTER，adapter 独占清 raw/context/pending
invalidate/live entry/range index 并 ack；fence monitor 始终独占清 raw producer settled/intake-close/context-dedup 并回 FENCE ack；reset coordinator 与 response owner都只能提交/等待 adapter/fence reset，不得直接调用上述清理 helper；CSR monitor 无条件使 history invalid 并 ack。dispatch service 在 reset 期间不能仅 `continue`，
必须执行 adapter/fence reset-ack；no-dispatch 时 adapter
按 `reset_required_ack_mask` 标记 N/A/已满足。所有 required ack 到齐且 `rst_n` 恢复后，只有 `ENABLED` owner 才等 C-2 history
warm-up 再重新开放 ready；`DISABLED/NO_OWNER` 保持无 owner，不重新 claim、不 re-arm。不论 topology 如何，均不清
`owner_claimed_once`、不重置 global sample/topology。reset 一开始必须清除
`owner_admission_settled_sample_seq`、`close_requested`、`request_owner_name`、`close_request_sample_seq`、
`close_request_reset_epoch`、当前 close 的 owner/generation expectation、`admission_closed`、`admission_owner_name`、
`admission_closed_generation`、`cutoff_sample_seq`、冻结的 item kind/generation/reset_epoch、
`final_inactive_item_done`、`final_inactive_generation`、`release_closing`、`closing_owner_name`、`closing_generation`、
`release_granted`、`grant_owner_name`、`grant_reset_epoch`、`grant_generation`、raw-fence producer settled/intake closed
及其 epoch/generation/cutoff 和 parent release pending；
`close_generation` 计数器本身保持单调不回绕。这样 reset 不会让旧 stop/final/grant metadata 误匹配新一轮 release，避免 reset
发生在 closing/grant 阶段时直接越过 re-arm。reset 与 global stop 重叠时，parent 在 reset_active 期间只继续 reset/adapter service，
不写 close/grant；先完成本 epoch cancel/ack。reset release 后，driver/sequence 必须先发送带 baseline tag 的 current epoch
`NORMAL/inactive` item，并按正常发送路径 `item_done()` 一次且记录 `baseline_sent_sample_seq`；严格在更晚真实 `drv_cb` 冻结该上一拍 latch 的 ready=0/fire=0/resp_valid=0
后才清 `post_reset_baseline_pending`、发布 baseline proof。只有 baseline proof 完成且 sample-ready 为 READY 的后续处理边界才允许重新发布 admission-settled watermark，
并为当前 reset epoch 创建新的 close generation/stop item，之后重新计算 drain、
发送 final inactive、建立 closing、等待 parent 重新 grant，不能因 reset直接退出 owner。

```text
检测 reset:
  rst_n 从 1 到 0 -> reset coordinator begin_reset_once()
      -> 只分配/发布新的 reset_epoch、置 reset_active=1、停止新 ready/response，并向各 direct writer 发 reset request；
         coordinator 不直接作废 admission、raw-fence、driver item、response token、adapter live state 或 CSR history；release generation counter 不回绕
  若 reset_required_ack_mask 含 RESPONSE：response_owner_reset(epoch) -> 取消旧 token/UID/barrier -> response_owner_reset_done=epoch
      driver_reset(epoch) -> 对已 get_next_item 的 stale item 精确 item_done()、清本地 item expectation、驱动 inactive
                         -> 观察 owner_reset_done 后唯一写 response_ack=epoch
  否则：response_ack=epoch（N/A，不启动或唤醒 responder）
  若 reset_required_ack_mask 含 ADAPTER：adapter_reset(epoch) -> 清 raw/context/pending invalidate/live/index -> adapter_ack=epoch
  否则：adapter_ack=epoch（N/A，不建立 adapter 工作）
  fence_monitor_reset(epoch) -> 清 raw producer settled/intake-close/context-dedup -> fence_ack=epoch
  reset coordinator 通知 CSR monitor 执行 csr_reset(epoch)
      -> CSR monitor 只清 history valid、CSR sample_producer_done_mask、当前 event-ready watermark、event history 和 CSR context dedup baseline，
         保留不随 history 清空的 last_allocated_l2tlb_event_seq，完成后回 csr_ack=epoch；response owner 在自己的 reset handler 中
         将 response_owner_event_cursor 和 last_seen_flush_event_seq 对齐该 baseline，不能由 CSR monitor 改写 response-owner cursor
  每个职责仅在自己的清理已完成后写 ack；按 reset_required_ack_mask 收齐 ack 且 rst_n 恢复 -> reset_active=0
  reset_active 从 1 变为 0 的那个 CSR monitor callback：只完成 reset release，不调用 advance_dut_global_sample()、
    不创建 CSR history 或 producer-done sample；因为同一 posedge 若 fence monitor 先运行，它仍会正确看到 reset-active 并跳过 FENCE done。
  下一真实 posedge 才是第一个 post-reset global sample：CSR monitor advance/history/done 与 fence monitor anchor/done
    都在 reset 已经解除的前提下执行，producer barrier 必须从这一拍同时起步。
  若 responder_mode == ENABLED：同一 owner re-arm -> 先完成 current epoch NORMAL/inactive baseline -> 等 C-2 history/sample-ready=READY -> ready=0/1 按正常流程恢复；
    若 global stop 仍为 1，owner 只能在 baseline 后的下一 READY drv_cb 发布 admission-settled，再建立当前 epoch 的 close/RELEASE_STOP
  若 responder_mode == DISABLED：保持 NO_OWNER，不 claim、不 re-arm、不恢复 responder ready
```

`reset_all_tables()` 只负责 testcase 建立主表；不能再承担 runtime reset，也不能清 package owner/sample。

`reset_l2tlb_runtime_state(reset_epoch, reset_required_ack_mask)` 的抽象职责是让同一个 reset epoch 的各个职责分别完成
自己的清理并回报 ack。它不判断是否存在 responder owner，也不把 `NO_OWNER` 转换成空的 response owner：在
`DISABLED/NO_DISPATCH` 下仍执行 fence producer/context、CSR reset/history 和 monitor 同步 sample 处理，并等待 FENCE/CSR/MONITOR ack，response/adapter 按 mask 直接记为 N/A；在
`ENABLED/DISPATCH_ACTIVE` 下再等待 response、fence、adapter 和 CSR 四类 ack 全部到齐。
testcase-start 的 `reset_epoch=0` 是“尚未发生 runtime reset”基线：`required_reset_acks_done(0)=1`，且
`reset_required_ack_mask(0)` 不等待 RESPONSE/ADAPTER/FENCE/MONITOR。这不是 runtime mask 的例外：runtime reset 从 epoch 1 开始，
每次都固定要求 CSR/FENCE/MONITOR，RESPONSE/ADAPTER 再按 topology 加入；release grant 与 release helper 均只能接受
传入 epoch 等于 `current_reset_epoch` 的条件。

`last_allocated_l2tlb_event_seq` 不能随着 history clear 清零。runtime reset 由 CSR monitor 清空 CSR event history、CSR producer done/watermark
与 CSR context dedup baseline，由 fence monitor 清空 raw producer settled/intake-close 与 fence context dedup baseline，由 adapter 清空
raw/context/pending-invalidate/live-entry；随后将 `response_owner_event_cursor` 与 `last_seen_flush_event_seq` 都置为该 baseline；
下一条 event 仅能分配 `baseline + 1`。这使旧 event 不会在 reset 后被重新消费，也不需要从空 history 推断“尾部”。

### 3.12 global stop 只等待主表/flushSb 不够

**待优化问题：** `memblock_main_dispatch_auto_build_main_table_base_sequence::service_real_dispatch_flow()` 当前在
`global_stop_requested && !flushsb_request_pending()` 时退出（`:64-78`），没有等待 L2TLB token、UID waiting、barrier 或 adapter
pending invalidate。

**影响：** parent 结束 phase 后可能杀掉仍需返回/取消的 response，或留下未删除的 live entry；后续 `phase_ended()` 又可能错误
替它 release owner。

**优化后方案：** release 顺序固定为“parent 提出 global stop -> owner 在真实 sample 封闭 admission 并收敛旧工作 -> parent 发 grant -> owner 最后 release”：
parent 请求 global stop 后停止新 routing，但不在 negedge 直接写 close request；此前 posedge 已驱动的 `ready=1` 仍可能在下一拍 fire。
owner 在下一真实 `drv_cb` 先 capture 该 fire、完成 UID 更新；仅当该 sample 为 READY 且 baseline 已完成时写 admission-settled watermark，再在同一拍写 close request、分配单调
release generation 并生成带 current reset epoch 的 `RELEASE_STOP/ready=0` item。close request 写入后不再接受新的 UID registration/token capture。driver 确认 admission cutoff 后，owner 继续处理/取消已有工作；只有 `pending_q`、driving slot 和 barrier 都为空时，才调用 `cancel_unbound_uid_tlb_records_at_release()`，将 marker=0 的 UID 显式转为 `CANCELED`。marker 非零但仍为 `WAITING` 的 UID 必须 fatal，不能借 release 清表。随后 response drain 才成立并发送带 current reset epoch 的 `RELEASE_FINAL_INACTIVE` kind 和 generation 的独立 item；driver
在真实 `drv_cb` 边界确认该 item 并置 final-inactive done，monitor 再同步确认相同 epoch/transport 序号 settled 后，owner 才调用 `begin_l2tlb_release_closing()`；其中 `final_inactive_item_done` 只能由 driver 在
真实 `drv_cb` 边界完成匹配 kind/generation 的 inactive item 后置位，不能由 sequence 创建 item 或 `finish_item()` 返回直接伪造。parent 与 owner 都使用
`release_grantable(owner, current_reset_epoch)` 统一复核当前 `close_requested/request_owner_name`、匹配 generation 的
`admission_closed`、final inactive、精确的 `monitor_final_sample_settled(current_reset_epoch, final_inactive_transport_sample_seq)`、`mailbox_recycle_done_seq == final_inactive_transport_sample_seq`、closing、response/adapter drain、与 current epoch/current close generation 匹配的 raw-fence intake closed、transport sample mailbox 已回收为 EMPTY、当前 epoch required ack 和 `!reset_active`；只有 parent
得到真值才写 `release_granted=1`，owner 获 grant 后以同一谓词复核并调用统一 release helper 清 active claim。parent 最后等待
`l2tlb_lifecycle_owner_claimed==0` 后结束。no-dispatch 时 adapter drain 由 ack mask 固定为真，但支持的普通 no-dispatch
是 `NO_OWNER`，不发送 grant；未来 standalone parent 也必须遵守同一顺序。

final item 不能在 `finish_item()` 后由 owner 直接等待 package 的 `final_inactive_done` 或 grant：driver 确认 final 时会把同一 frozen
transport sample 置为 mailbox `PUBLISHED`，而 recycle 以 owner 的 terminal ack 为前置。正确的终端交接是：owner 回到 sample
consumer，验证该 sample 的 frozen final proof 与 owner/epoch/generation，确认 monitor 已 settled，先写 closing、再对 final sample
写 `CONSUMED`；driver 才在下一真实 `drv_cb` 变为 `EMPTY` 并写 recycle proof。之后 parent 以完整 gate 写 grant 并 trigger
`l2tlb_release_state_changed_ev`，owner 通过 `wait_for_l2tlb_release_grant_or_reset()` 醒来并复核 grant/gate 后清 claim。该等待
不依赖新 semantic mailbox sample，因 final recycle 后 driver 不再为了唤醒 owner 而发布第二份语义 sample。

为消除 release scan 与新 UID 注册之间的竞态，顺序进一步固定为：parent 在 `global_stop_requested` 首次成立的 negedge
只停止 routing 并继续 service，不能在此时直接封闭 admission，因为本拍前驱动的 ready 可能仍在下一 `drv_cb` 形成真实 fire。
owner 在下一真实 `drv_cb` 先让该 fire/UID register 正常结算；只有该 sample 为 READY 且 baseline 已完成时，才原子写 `admission_settled_sample_seq`；只有该 watermark 等于
current sample 时才调用 `close_l2tlb_admission_for_release(owner, current_sample)`，写入 `close_requested/request_owner/close_request_sample_seq/
close_request_reset_epoch/close_generation`，并在同一 driver item 生成 `item_kind=RELEASE_STOP、item_generation=close_generation、item_reset_epoch=current_reset_epoch、req_ready=0`。
该 seal 不能提前写 `admission_closed` 或伪造 cutoff。close request 写入后的任何 capture/register 调用均 `uvm_fatal`；此前已经完成的 fire/UID
registration 保持合法。driver 在随后的真实 `drv_cb` 边界冻结 item kind/generation/reset_epoch、ready 与 fire；仅匹配的 RELEASE_STOP 且
`sampled_req_ready=0 && sampled_req_fire=0`、sample 晚于 close request 时才由 driver 原子写入 `admission_closed/owner/generation/cutoff`。此后 owner 先 drain token/driving/barrier；三者皆空时只取消 marker=0 的 unbound UID，marker 非零的残留 `WAITING` 立即 fatal。完成该 release-time cleanup 后 response drain 才成立，owner 再
发送独立的 `RELEASE_FINAL_INACTIVE` item；driver 真实采样匹配 owner/generation/reset_epoch 的 `sampled_req_ready=0 && sampled_req_fire=0 && sampled_resp_valid=0` 后才置 final-inactive
done，monitor 同步确认同一 epoch/transport 序号 settled 后，`begin_l2tlb_release_closing()` 才只写 closing，绝不执行 `try_release`。adapter drain 与 owner 流程并行；fence monitor
必须在 close request 后完整处理一个 raw sample，再写匹配 epoch/generation 的 intake closed。parent 最后用带
`owner + reset_epoch + generation` 的 grant 统一等待 response drain、adapter queue drain 和 raw-intake closed；owner 只在三者匹配时最终清 claim。

`grant_l2tlb_final_release()` 的抽象职责是 parent 对已经静止的 owner 发放一次最终释放授权。它只读取各方状态，
不清 `pending_q`、不清 UID、也不替 owner 发送最后一个接口 item。其最小判定顺序为：

```text
grant_l2tlb_final_release():
  若 responder_mode == DISABLED：确认不存在 claimed owner，保持 grant=0，返回“无需释放”。
  若 global_stop_requested 首次成立：parent 只停止 routing；继续 service，等待 owner 在当前 reset epoch 的真实 drv_cb
    先写 admission-settled watermark、再写 close request/generation，并等待 driver 确认匹配 RELEASE_STOP 的 admission_closed。
  若 !release_grantable(current_owner, current_reset_epoch)：保持 grant=0并继续 service。
  该谓词内部统一要求 global stop、当前 owner claim、新 admission 已关闭、匹配 generation 的 final inactive、closing、response/adapter drain、
  与当前 epoch/generation 匹配的 raw-fence intake closed、
  transport sample mailbox 已回收为 EMPTY、
  !reset_active 和本 epoch required reset ack 已收齐；parent 不得在本函数外复制或省略其中条件。
  若已经发过同一 owner 的 grant：不得重复写，直接返回已发放状态。
  写 release_granted=1、grant_owner_name=current_owner、grant_reset_epoch=current_reset_epoch、grant_generation=current close_generation，
    并保留 owner claim，等待 owner 在自己的时序边界消费 grant。
```

owner 消费 grant 后，`release_l2tlb_lifecycle_owner()` 才能清 claim；parent 以
`l2tlb_lifecycle_owner_claimed==0` 判断完成，不能引入或依赖另一个 release-done 镜像位。

为避免四份关联专项使用不同函数名，后续 coding 只保留两个对外阶段：
`begin_l2tlb_release_closing(owner)` 建立 closing，`release_l2tlb_lifecycle_owner(owner)` 完成最终释放。
时序 plan 中的 `prepare_l2tlb_lifecycle_owner_release()` 是第二个阶段内部的只读检查 helper，
`try_release_l2tlb_lifecycle_owner()` 是其内部原子写入 primitive；它们都不是独立 release 入口，不能被 driver、parent、
`do_kill()` 或 `phase_ended()` 直接调用。

### 3.13 `phase_ended()` 和 `do_kill()` 不能成为隐式 release

**待优化问题：** sequence `do_kill()`（`:167-177`）和 driver `phase_ended()`（`L2tlb_agent_agent_driver.sv:36-46`）当前
直接调用 release helper。`phase_ended()` 是 function，不能执行需要时钟边界的 `drive_idle()` task。

**影响：** 未收敛 token/UID 被隐藏，下一条 sequence 可能接管；同时 function 中无法可靠完成最后一个 idle item。

**优化后方案：** 正常路径必须在 phase 结束前由 owner 发最终 inactive 并获得 parent grant。任何 active-owner `do_kill()` 都
`uvm_fatal` 且不 release，不保留 granted 例外，避免 kill 绕过最后一个 interface sample。`phase_ended()` 也只检查/报告：已 claim
即 fatal 并保留 claim，不驱动、不 release。driver 现有 `get_owned_item_or_abort()` 只可在下一 `drv_cb` 的主循环发送 idle，
不能在 `phase_ended()` 这个 function 中调用 task。正常情况下只有 owner 的 global-stop 主循环在 grant 后、最终 inactive item 已
完成、closing 已建立时调用唯一 release helper；`do_kill()` 和 `phase_ended()` 永远不是 release 入口。

### 3.14 token、UID、raw fence 和 live entry 的写者边界不清

**待优化问题：** 旧方案容易把一个 token 固定绑定一个 UID，或让 response sequence pop adapter raw fence；reset/stop 又由 parent
代替 owner 清队列。

**影响：** V2 `PtwRespS2` 没有 UID，DUT 一笔 response 可能命中多个 filter entry；跨组件清理会造成重复 cancel、漏回填或双重
删除。

**优化后方案：** 固定唯一写者：

| 对象 | producer / 唯一写者 | 其他组件允许的操作 |
|---|---|---|
| `pending_q`、`driving_req`、response token | L2TLB owner | parent 只读 drain |
| UID waiting/cancel/completion | L2TLB owner 与既有 UID helper | payload 专项提供 matcher，不绑定 token |
| `raw_sfence_q` | fence monitor 入队；adapter 唯一 destructive consumer | owner、parent 只读独立 lifecycle event，不 pop |
| invalidate pending | dispatch adapter | owner、parent 不读取或修改其队列内容 |
| `tlb_entry_by_key`、range index | adapter 的 C4 delete helper、payload/range builder | response owner只复制 snapshot |
| release grant | parent coordinator | owner消费并 release |

每次 request fire 仍独立建 token；response complete 扫描合法 waiting UID，允许 0/1/多个命中；C4 不静默取消 marker=0 的 UID。
它在 admission cutoff 前保持 `WAITING`，因为未来仍可能观察到真实 request fire；仅在 cutoff 已确认且 token/driving/barrier 全空后，
response owner 才用 `cancel_unbound_uid_tlb_records_at_release()` 将该 unbound record 显式转为 `CANCELED`。marker 非零的残留
`WAITING` 仍是未完成真实 request，global stop 必须 fatal，不能静默释放。

### 3.15 `MEMBLOCK_L2TLB_IDLE_STOP_CYCLE` 不能触发 active owner 退出

**待优化问题：** 当前 sequence 在 `idle_count >= idle_stop_cycle` 时置 `stopping=1`，无 outstanding 时自行退出（源码
`memblock_l2tlb_base_sequence.sv:332-380`）。这与“owner 从第一次 ready 持续到 parent release grant”的单 owner 合同冲突，
dispatch-active flow 可能在主表仍运行时失去 responder。

**影响：** 后续 DTLB request 只能看到 disabled driver 的 `ready=0`，但 sequence 已经自然结束；失败表现为等待卡死，且
`phase_ended()` 不能可靠补发最后 idle。

**优化后方案：** 对所有 `responder_mode=ENABLED`，`MEMBLOCK_L2TLB_IDLE_STOP_CYCLE` 只保留为 no-progress 诊断阈值：达到阈值时
打印一次/按周期节流 `uvm_warning` 或 `uvm_error`，清零诊断计数并继续保持 owner/ready service；不得设置 `stopping`、不得
生成正常退出 item、不得触发 release。唯一正常 stop 来源是 parent 的 `global_stop_requested`；真正无进展最终由 UVM timeout 或
专门 watchdog 报告。`DISABLED` 模式没有 responder sequence，也不累计该计数。

### 3.16 `release_closing` 不能由 release helper 自己等待自己设置

**待优化问题：** 若 release helper 要求 `release_closing=1`，但该位又只由 release helper 写入，会产生前置条件循环：owner
永远无法合法调用 release。

**影响：** global stop 会停在“等待 closing”而 owner 又无法进入写 closing 的状态，response/adapter 已经 drain 也不能结束 testcase。
若为绕开死锁放宽 release gate，parent、driver 或 `phase_ended()` 就可能直接清 claim，重新引入未经过最终 inactive sample 的错误 release。

**优化后方案：** 新增 owner 专用的 `begin_l2tlb_release_closing()`，唯一在 driver 已完成带
`RELEASE_FINAL_INACTIVE` kind、当前 owner/generation/reset epoch，且冻结的
`sampled_req_ready=0 && sampled_req_fire=0 && sampled_resp_valid=0` 的最终 inactive item真实 `drv_cb` 边界、且 monitor 已同步确认精确 epoch/transport sample settled 后调用。它显式检查 global stop、
admission 已关闭、response drain、owner/generation、final inactive 已真实采样和尚未 closing，然后写入 closing/name/generation；
不清 claim。parent grant 到达后，`release_l2tlb_lifecycle_owner()` 以
`release_grantable(owner, current_reset_epoch)` 复核所有相同条件，再原子清 owner-owned active claim 和 closing；grant 由 parent/reset grant coordinator 保留到 testcase-start 或 reset 的对应写者清理边界，不能由 owner release helper 清除。`l2tlb_lifecycle_owner_claimed==0` 是唯一 release-complete
判据，不新增 `owner_release_done` 镜像位。这样不存在“release helper 等待自己写 closing”的循环，也没有两套完成状态。

```text
owner drain 成立：
  发送 item_kind=RELEASE_FINAL_INACTIVE、current owner/generation/reset epoch 匹配、ready=0/resp_valid=0 的最终 item
  driver 在真实 drv_cb 边界确认匹配 kind/generation/reset_epoch 且 sampled_req_ready=0 && sampled_req_fire=0 && sampled_resp_valid=0
    -> final_inactive_item_done=1；任一条件不满足立即 uvm_fatal
  等待 monitor_final_sample_settled(current_reset_epoch, final_inactive_transport_sample_seq)=1；否则按 bounded watchdog 报错
  begin_l2tlb_release_closing(owner)
  等 parent release grant
release_l2tlb_lifecycle_owner(owner):
    要求 grant_owner==owner、grant_reset_epoch==current_reset_epoch、grant_generation==current close_generation、release_granted=1，且 release_grantable(owner, current_reset_epoch)=1；
    任一不满足则 uvm_fatal，不清 claim。
    全部满足后原子清 owner-owned active claim/closing；不清 parent-owned grant，也不清 driver/fence monitor 的 transport close proof
  parent 观察 l2tlb_lifecycle_owner_claimed==0 -> release 完成
```

#### 审核修改意见（final sample ack 与 grant 唤醒闭环）

上方“等待 final inactive/monitor settled 后建立 closing”的摘要若被实现成 sequence 在 `finish_item()` 后阻塞等待，仍会死锁：
driver 的 final proof 位于一份尚未被 owner 终态确认的 `PUBLISHED` mailbox sample 中，driver 无法先 recycle，parent 的
`release_grantable()` 又要求 recycle proof 和 `EMPTY`。后续 coding 以以下完整顺序为准：

```text
owner 发现 response drain
  -> 投递唯一 RELEASE_FINAL_INACTIVE item，随后回到 wait_l2tlb_transport_sample()
driver 在 F 的真实 drv_cb 采样 final
  -> 写 frozen final proof，先同步 monitor settled，再保持该 final sample 为 PUBLISHED
owner 消费 F 的 final sample
  -> 校验 proof/owner/epoch/generation 和 monitor settled
  -> begin_l2tlb_release_closing() 写 closing
  -> ack_l2tlb_transport_sample(F, CONSUMED)
  -> 不再等待新 semantic sample；只等待 release-state wakeup
driver 在 F+1 的真实 drv_cb
  -> 回收 F 的 terminal slot 为 EMPTY，并写 mailbox_recycle_done_seq=F
parent 在完整 release_grantable() 为真时
  -> 原子写完整 grant metadata，随后 trigger l2tlb_release_state_changed_ev
owner 醒来
  -> reset 优先；否则核对 matching grant 与同一无-grant-metadata gate 后原子清 claim
```

`release_grantable()` 在上述任一阶段均不得读 `release_granted`、grant owner、grant epoch 或 grant generation；只有 owner 的
最终原子 release 会额外读取匹配 grant。reset coordinator 在原子发布新 reset epoch/active、作废旧 grant 后也 trigger 同一 wakeup，
使 owner 返回既有 reset sample/re-arm 路径，而非误消费旧 epoch grant。

### 3.17 flush event history 无界或同拍重复

**待优化问题：** event 只保存 latest 或无界 push，无法表达 CSR/fence 同拍合并；若把有界 history 误当成 active responder 的
跨拍回放队列，又会掩盖 consumer 停止。

**影响：** service 停顿会造成中间 C0 漏处理；若事后补回放，已过期 C4 会错误取消 C0 后已经建立的新 token/UID。无 owner 或
consumer 停止时，history 无界增长又会把生命周期问题推迟到随机的容量耗尽时才暴露。

**优化后方案：** `note_l2tlb_flush_event(sample, reason)` 同 sample OR reason、跨 sample 分配递增 event_seq；保存固定
compile-time 上限。dispatch-active 时只有 response owner 维护有效 history cursor；历史满且最早 event 尚未被该 cursor 消费时
立即 fatal，并打印 cursor、队列深度和 sample。adapter 不读取或回收该 history，只消费 raw fence FIFO，并使用 raw 中冻结的
`lifecycle_event_seq/sample_seq` 做关联和诊断。CSR/fence 生产者完成同 sample 的 reason 合并后才发布 event-ready；若本拍
没有 reason，只发布 watermark，不追加空 event。event consumer 仅接受 `event.sample_seq==current_sample` 进入 barrier；未曾
`acceptance_opened_since_reset=0` 且没有 active work 时的旧 event 只作 baseline cursor 对齐；该标记为 1 后的旧 event 或任何 future event 均 fatal。
`NO_OWNER/NO_DISPATCH` 时没有 response consumer，watermark 发布后不创建本拍 event record，避免 producer 获得第二个回收写权或让队列无界增长。该队列只保存
ENABLED response lifecycle 事件，不承担 payload 或主表状态。

### 3.18 Driver 到 monitor 的 TLM FIFO 会引入第二条 sample 生命周期

**待优化问题：** 早期草案把 driver 冻结的 transport sample 同时放入 sequence mailbox 和
`uvm_tlm_analysis_fifo`。这样同一个 sample 虽然共享 wrapper handle，却有两个独立的消费时刻：sequence 可以先完成
mailbox ack，monitor 仍可能在 FIFO 中积压；reset 还需要同时处理正常 `get()`、FIFO drain 和 MONITOR ack。

**影响：** FIFO 的空并不等于 final sample 已被 monitor 处理；reset drain 与正常消费可能重复处理或丢弃同一 sample，
FIFO 的容量/反压又会形成 mailbox 之外的第二个调度边界。若 driver 回收 slot 时清空 public payload，monitor 仍持有的
wrapper 会被修改，导致 epoch、fire 或 owner provenance 失真。这是 transport/lifecycle 实现问题，不是 L2TLB
request/response 语义变化。

**优化后方案：** 删除 monitor transport FIFO，改用同步 `uvm_analysis_imp`。driver 仍是唯一物理采样者；每个
`drv_cb` 创建并 freeze 一个 wrapper，在 ENABLED 模式先以同一个 `sample_ref` 预留 sequence 单槽 mailbox，再通过
`transport_sample_ap.write(sample_ref)` 同步交给 monitor。monitor 的 `write_transport_sample()` 在 `write()` 内完成
epoch、4-state、reset 和既有 analysis transaction 处理，返回时本 sample 已处理完毕。wrapper 的 payload 改为 private，
driver 只能在 freeze 前写，monitor/sequence 只能通过 getter 读取副本；mailbox 回收只清 slot handle 和终态，不清 frozen
payload。response-owner sequence 对 normal sample 在纯软件语义完成后、任何 `start_item/finish_item` 前 ack CONSUMED；
同一 sequence 的 reset/abort 路径 ack DROPPED，driver 只回收 EMPTY。driver 在 final-inactive confirm 成功时，把
`{proof_valid, proof_epoch, proof_transport_sample_seq}` 写入同一 working sample 后再 freeze；monitor 不读取 live reset 或
live final-done 来推导该 proof。final sample 不是 normal sample 的例外：owner 必须先用 frozen proof 建 closing、随后 ack
CONSUMED；若它直接等待 recycle/grant，slot 仍 PUBLISHED，driver 无法写 recycle proof。final ack 后 owner 仅等待
`l2tlb_release_state_changed_ev` 的 grant/reset 唤醒，不要求 driver 再发布 semantic sample。

#### 审核修改意见（mailbox 预留、final 回收与 reset 唤醒）

上面的状态流还必须补上一个时序闭环：driver 不能先调用 analysis port、再把 sample 放入 mailbox，
因为 reset coordinator 可能在这两个动作之间看到 mailbox 仍为 `EMPTY` 并错误完成 RESPONSE ack。当前可执行顺序固定为：

```text
每个 driver drv_cb：
  先回收上一 slot 的 CONSUMED/DROPPED terminal；若 terminal_seq == final_inactive_transport_sample_seq，
    写 mailbox_recycle_done_seq=terminal_seq；没有 sequencer item 也必须执行这一步。
  确认 slot=EMPTY 后，先把本拍 frozen wrapper handle 原子登记为 PUBLISHED；
  再同步调用 transport_sample_ap.write(sample_ref)，monitor write 返回后 sample 才算已处理；
  若本拍是 final 后的回收边界，不再向 sequence mailbox 发布新的 semantic sample，只保持 inactive 并继续诊断采样。
  用 try_next_item() 非阻塞检查下一 item；没有 item 时驱动/发布 inactive，不能阻塞在 get_next_item()，
    这样 reset request 能在下一 drv_cb 生成 reset-active sample，monitor 才能回 MONITOR ack。
```

`reserve_and_publish_l2tlb_transport_sample()` 是上述“先预留 slot、再同步发布”的唯一 driver helper；
`recycle_l2tlb_transport_sample_at_drv_cb()` 是 terminal 到 `EMPTY` 及 final recycle proof 的唯一写者。
reset coordinator 只设置 reset request/wakeup 标志并等待 direct-writer ack，不直接改 mailbox；sequence 在收到
reset sample 后以同一 CAS 把 `PUBLISHED` 置为 `DROPPED`。若 ENABLED owner 在 bounded watchdog 内不消费该 sample，
必须 `uvm_fatal`，不能由 coordinator 静默清槽。

因此 release gate 除了 `mailbox == EMPTY`，还必须要求
`mailbox_recycle_done_seq == final_inactive_transport_sample_seq`；final sample ack 后至少跨过一个真实后续
`drv_cb` 回收边界，driver 即使没有下一 item 也不能退出 service loop。该边界解决“owner 等 grant、driver 等 item、
mailbox 永远 PUBLISHED”的循环等待。

**关键函数抽象功能：**

- `L2tlb_agent_agent_monitor::write()` / `write_transport_sample()`：前者是 `analysis_imp` 的实际接收入口，只转调后者；
  后者同步消费 driver sample 并产生既有 monitor analysis 输出，不读 VIF、不推进 global sample、不改 sample 或 mailbox。
- `reset_l2tlb_transport_monitor_state()`：登记 reset epoch 和上次 reset ack seq floor，并在后续 `sampled_reset_active=1` 且 epoch/transport 序号匹配且严格递增的 reset sample 被同步处理后回 MONITOR ack；
  不执行 FIFO drain。
- `monitor_reset_ackable(epoch, reset_sample_seq)`：只判断匹配 reset sample 的 epoch+seq 是否处理完成、monitor 空闲且 seq 严格递增；不读取 final 状态。
- `monitor_final_sample_settled(epoch, seq)` / `l2tlb_transport_monitor_drain_done(epoch, seq)`：只判断精确 epoch/transport 序号的 final sample 是否已 settled；不检查不存在的 FIFO，也不参与 reset ack。

```text
driver 每个 drv_cb：
  sample_ref = new wrapper
  fill_payload(sample_ref, frozen_4state_and_metadata)
  sample_ref.freeze()
  若 ENABLED：先以 sample_ref 和 transport_sample_seq 原子预留 mailbox.slot，state = PUBLISHED
  transport_sample_ap.write(sample_ref)       // 同步调用 monitor.write()
  若本拍是 final sample 后的回收边界：不再向 semantic mailbox 发布新 sample，只保持 inactive/diagnostic

monitor.write(sample_ref)：
  monitor_processing = 1
  payload_copy = sample_ref.get_payload()
  只用 monitor-local active_epoch 比较 frozen sampled_reset_epoch；stale drop，future epoch fatal
  reset sample 记录 reset_sample_processed_epoch/transport_sample_seq，不转发普通 transaction；normal sample 转发既有 transaction
  仅 frozen final proof valid，且 proof_epoch/seq 与本 payload 的 frozen epoch/seq 一致时，记录 final_settled_epoch/seq
  monitor_processing = 0
  若 monitor_reset_ackable(pending_epoch, reset_sample_processed_transport_sample_seq)：回带 epoch+seq 的 MONITOR ack

release_grantable：
  要求 monitor_final_sample_settled(current_reset_epoch, final_inactive_transport_sample_seq) == 1
  不再等待 transport FIFO empty
```

本修正不增加 `memblock_env`、RM 或 scoreboard 的外部 producer，也不改变 sample mailbox 的终态协议、L2TLB payload、
C0/C4 barrier、range matcher 或 UID multicast。本节只说明跨专项合同；该 transport 修改的源码、compile 与 smoke 状态
以 timing correction plan 和对应 implementation review 的记录为准，不在本文重复判定。

## 4. 优化后的完整行为

### 4.1 启动

```text
testcase start
  -> 初始化 package lifecycle 和固定 topology/start mode
  -> 检查 responder_mode、SEQ_EN/connect capability
  -> DISABLED：登记 NO_OWNER；需要 response 则启动前 fatal，不配 responder、不发 grant
  -> SEQ_EN=1：sequence 私有状态初始化后立即 claim，一次成功后保持 owner
  -> CSR history 未达到 C-2 warm-up：只驱动 inactive/ready=0
  -> sample-ready 后开放 ready
```

### 4.2 一个 DUT sample 的处理顺序

```text
posedge drv_cb
  -> 先冻结该 drv_cb 边界的 request valid/ready/fire、response valid、VPN/s2xlate 与 lifecycle item metadata，并保存本拍 sample anchor time
  -> 在同一 drv_cb 内调用 bounded sample-anchor/probe；该 probe 只等待本拍 NBA/producer 发布窗口，不等待下一 clock edge，返回 sample_valid、sample_seq 和 READY/NOT_READY
  -> 只有 sample_valid=1 后才调用/取得 `peek_current_dut_global_sample()`；不得在 probe 前读取 current sample，避免 driver 早于 CSR monitor 时使用旧 sample
  -> reset 优先：发送 inactive，按 reset epoch ack，不处理普通 token
  -> 若 sample_valid=0：要求冻结 request fire 与 response valid 均为 0；驱动当前 epoch 的普通 inactive，不能写
     admission-settled/close，也不能消费旧 history；下一拍重新冻结 VIF
  -> driver 在 transport sample 中发布 baseline_required/baseline_proof_pending；sequence 不读取 driver local pending
  -> 若上一拍 latch 带有 baseline tag：driver 仅在 sample_seq > baseline_sent_sample_seq 时冻结其 ready=0/fire=0/resp_valid=0 并写 done proof；
     该动作确认上一拍已驱动的 NORMAL/inactive baseline，不驱动新接口值
  -> 若 sample.baseline_required=1 且 sample.baseline_proof_pending=0：sequence 构造 current epoch 的 NORMAL/inactive item 并置 baseline tag；
     不写 proof、不创建 RELEASE_STOP；proof pending 时只保持 inactive，下一有效 drv_cb 才能确认 baseline
  -> 若 sample_ready=NOT_READY 且 sample_valid=1：要求冻结 request fire 与 response valid 均为 0；不写 admission-settled，
     只送普通 inactive；不得在该拍创建 RELEASE_STOP；下一拍重新 anchor/peek
     不读取旧 history 或带着旧采样进入 token/UID 路径
  -> 保存既有 `acceptance_opened_since_reset`；它仅在真正生成 next_ready=1 时置位且普通 flush 不清，再保存旧 owner event cursor
  -> 对 cursor 后 event 做 freshness gate：current sample 才登记 C0 barrier；ready 尚未开放且无 ready/fire/response、pending/driving/
     barrier/hold/WAITING UID 时，旧 event 只作 pre-ready baseline，推进 cursor 并从 current sample 起保守 hold 4 拍、清 ready opportunity；
     ready 已开放后发现旧 event 在任何 barrier/token/UID 修改前 fatal；future event 同样 fatal
  -> 若上一 driving response 将在 C4 被采样，禁止 selector 产生它；已发生则 fatal；C0 request fire 仍建立独立 token，归属使用旧 cursor
  -> C1-C3 完成到期旧 response
  -> C4 禁止 response fire，取消仍 pending 的旧 token/符合 marker 的 UID
  -> 完成本拍 request capture/UID 更新后，若 sample-ready=READY 且 baseline 已完成，owner 写 admission_settled_sample_seq；若 parent 已置 global stop、baseline 已完成
     且尚未 seal，owner 在同一 drv_cb 写当前 epoch 的 close request 并把下一 item 设为 RELEASE_STOP/ready=0
  -> dispatch-active adapter 独立在 C4 删除 live entry/range index
  -> 本拍 event/token/UID 结算后由唯一 owner 回收已消费 history 前缀；根据 stop、hold、history 和容量决定下一 item 的 ready/response。
     hold/baseline 前保持 ready=0；真正生成 next_ready=1 后才置 ready opportunity，随后才允许累计 idle-stop 诊断
negedge dispatch service
  -> 只 peek global sample
  -> 同一 sample 调用 CSR semantic sync 和 adapter fence service 各一次
```

### 4.3 reset 与结束

```text
runtime reset
  -> 一次 reset_epoch 协调 response/fence monitor/adapter/CSR 清理并 ack
  -> driver 在 reset-active 的真实 drv_cb 忽略旧 reset epoch 的已采样 stop/final metadata latch（其原 item 已正常 item_done）；对已 get_next_item 的未发送 stale item 先 item_done 后强制 inactive；旧 item 不确认 close/final，reset release 后不重放
  -> 保留 owner_claimed_once、active topology 和 global sample
  -> reset release 后：ENABLED topology 由同一 owner re-arm、重新 warm-up且不二次 claim；DISABLED/NO_OWNER 保持无 owner，不 re-arm、不恢复 ready

global stop
  -> 若 topology 为 DISABLED/NO_OWNER：要求 claim=0；不发送 final inactive、不建立 closing、不发 grant；继续 passive driver/monitor
     service，完成当前 runtime reset 的 CSR、FENCE、MONITOR 必要 ack 后结束；不等待不存在的 RESPONSE/ADAPTER ack
  -> 若 topology 为 ENABLED：parent 在 negedge 只停止 routing；若 current nonzero reset epoch 的 baseline proof 尚未完成，owner 先发送 tagged NORMAL/inactive 并等待下一有效 drv_cb 的 fire=0/ready=0/resp_valid=0 proof；之后 owner 还必须等待 sample-ready=READY，再在真实 drv_cb 结算此前 ready 窗口的 fire/UID，写 admission_settled 与当前 epoch 的 close_requested/close_generation，并生成 RELEASE_STOP/ready=0
  -> close request 写入后任何新的 UID/token capture 立即 fatal；driver 在真实 drv_cb 只用匹配 RELEASE_STOP kind/generation 和 fire=0 写 admission_closed/cutoff
  -> fence monitor 先处理 close request 后的一个完整 raw sample，再写匹配 current reset epoch/current close generation 的 raw-fence intake closed；closed 后有效 raw fence fatal
  -> intake closed 写入前 fence monitor 不能随 phase end 退出；若没有后续 sample，parent 只能报 lifecycle timeout/fatal，不能绕过该 proof 发 grant
  -> owner 先 drain token/driving/barrier；三者皆空后对 marker=0 的 unbound UID 执行一次 release-time CANCELED 转换并删除 index，
     marker!=0 的残留 WAITING 立即 fatal；随后 response drain 才成立并发送独立 RELEASE_FINAL_INACTIVE item
  -> driver 在真实 drv_cb 采样匹配 final item，且冻结的 `sampled_req_fire=0`、`ready=0`、`resp_valid=0` 后置
     final_inactive_item_done 与 final_inactive_transport_sample_seq；driver 先预留 mailbox slot，再同步调用 monitor analysis_imp，
     monitor 应已处理该 sample
  -> owner 消费这个 final PUBLISHED sample，读取 frozen proof；若 monitor 未 settled 则 fatal，settled 后先建立 release closing、再写 final terminal CONSUMED ack
  -> 下一真实 drv_cb 先回收已 terminal 的 final mailbox slot 并写 mailbox_recycle_done_seq；没有下一 item 也必须执行，之后
     adapter queue drain 与 owner 流程并行；parent 与 owner 都调用 release_grantable(owner, current_reset_epoch)：要求 response/adapter drain、
     匹配 generation 的 raw-fence intake closed、monitor_final_sample_settled(current_reset_epoch, final_inactive_transport_sample_seq)、
     mailbox_recycle_done_seq == final_inactive_transport_sample_seq、transport sample mailbox 已回收为 EMPTY、final inactive、closing、
     !reset_active 和当前 epoch 必需 ack 已收齐
  -> parent 仅在该谓词为真时原子发一次 release grant 并 trigger release-state wakeup；owner 获唤醒后以同一谓词复核并原子清 active claim
  -> parent 确认 claim=0 后结束
```

## 5. 17 个历史问题的责任归属

| 回顾问题 | 本文处理 | 具体字段/算法归属 |
|---|---|---|
| 1 C0 同拍 token 立即 kill | 采用 C0 barrier/C4 cancel | timing correction + 本文生命周期 |
| 2 raw fence 多消费者 | adapter 唯一 destructive consumer | stage-aware + 本文职责边界 |
| 3 S1 sector range hit | 只引用最终 matcher | range/stage-aware |
| 4 删除 entry 未清 range index | 要求统一 delete helper | range/stage-aware |
| 5 持久 `s1_paddr/s2_paddr` | 本文不保存 | random payload/range |
| 6 no-dispatch handoff/gap | 单 owner 固定 topology，删除 handoff | 本文 |
| 7 token 错绑 UID | 明确每 fire 独立 token、response 可多 UID | random payload |
| 8 request 用 latest CSR | 要求 C-2 history | timing/random payload |
| 9 C4 response 误记成功 | C4 禁止 fire | timing correction |
| 10 UID 使用 issue-time CSR | response-visible C-2 回填 | timing/random payload |
| 11 owner/local sample | global sample + 单 owner re-arm | 本文/timing |
| 12 raw fence gate | topology coordinator 固定 | stage-aware + 本文 |
| 13 range index 单值 | 使用有界 candidate list | range |
| 14 release 漏 WAITING/phase 清理 | cutoff 后只取消 marker=0 的 unbound UID；marker 非零 WAITING 继续 fatal，再进行 response/adapter queue drain + grant，异常不直 release | 本文/timing；另需 raw-fence intake close |
| 15 marker 用 wait-start | 只使用 request-fire marker | timing/random payload |
| 16 NAPOT 越过 profile | 不在本文改 payload | random payload/range |
| 17 allStage anchor 依赖 derived PPN | 不在本文改 builder | random payload/range |

本轮复核补充：active owner 的 `acceptance_opened_since_reset=1` 后收到旧 `event.sample_seq`，不属于新的 multi-owner/handoff 问题，
而是第 7 项 event-history 时基边界的遗漏。本文第 3.7/3.17 与 timing correction plan 已将其收紧为：current sample 才建 barrier，
pre-ready 且无 active work 的旧 event 只作 cursor baseline，active 旧 event/future event 一律 fatal。

上述矩阵表示“问题没有遗漏”，不表示本文实现了四份专项中的 payload 或 matcher。任何专项若仍保留 owner handoff、
local sample、C0 immediate kill 或 driver direct release 的可执行描述，需按本文审核结论修正其文字。

## 6. 源码落点与职责边界（供后续 coding 对照）

| 源码落点 | 需要调整的行为 | 不负责的内容 |
|---|---|---|
| `memblock_sync_pkg.sv` | testcase lifecycle、responder mode、topology、claimed-once、global sample、CSR/event history、reset epoch/ack mask、owner admission-settled、close request/admission close/cutoff、raw-fence intake close、final transport/recycle scalar proof、release grant/closing | 不构造 TLB payload；不保存 wrapper handle、不直接清 response/fence/adapter 状态 |
| `csr_ctrl_agent_agent_monitor.sv` | 每个 post-reset sample 唯一推进 global sample、发布 CSR history；与 fence event 合并完成后发布 sample-ready | 不消费 raw fence、不决定 response hit |
| `fence_agent_agent_monitor.sv` | 使用已推进的同一 sample 记录 fence event/raw provenance，按 topology 入 raw FIFO；global stop 后在完整 raw sample 边界写 intake close | 不删除 live entry、不推进 sample |
| `memblock_l2tlb_base_sequence.sv` | 移除直接 `@drv_cb`、VIF 读取和 local sample++；改为消费 driver mailbox sample 后执行 claim、sample-ready、event freshness（`acceptance_opened_since_reset=1` 后拒绝旧 event）、C0/C4、reset re-arm、response drain、在 request capture 后发布 admission-settled 并为当前 epoch 写 close request、生成 RELEASE_STOP/RELEASE_FINAL_INACTIVE metadata、最终 release；每拍由 owner 回收已消费 event 前缀；reset 清 response 状态、`acceptance_opened_since_reset`、pre-ready hold/ready opportunity 后发布 owner-reset-done | 不 pop adapter raw fence、不实现 range matcher、不直接写 response ack |
| `L2tlb_agent_agent_monitor.sv` | 声明 `transport_sample_imp`；`analysis_imp` 的 `write(sample_ref)` 只转调无等待 `write_transport_sample(sample_ref)`，以 getter 消费 immutable wrapper；`run_phase()` 停止旧 `mon_data()` transport loop。`DISABLED/NO_OWNER` 时消费 passive driver sample 并对 req_valid 立即 fatal；active 模式发布既有观测 raw | 不独立 `@mon_cb` 读取 transport VIF、不推进 sample、不重算 fire、不创建 token |
| `L2tlb_agent_agent.sv` | `connect_phase()` 连接 `drv.transport_sample_ap` 到 `mon.transport_sample_imp`，并显式调用 `drv.bind_transport_slot_owner(sqr)`；失败、重复连接或绑定缺失 fatal | 不修改 DUT connect，不向 env/RM 添加第二个 transport producer |
| `L2tlb_agent_agent_xaction.sv`、`L2tlb_agent_agent_driver.sv` | xaction 定义 private frozen wrapper；driver 每个真实 VIF `drv_cb` 先回收 terminal mailbox，再以 `try_next_item()` 非阻塞轮询并冻结 item kind/generation/reset_epoch/fire/ready/resp_valid；driver 唯一确认 baseline/stop/final、final transport seq 和 recycle proof。DISABLED 时走 passive sampler，不取 sequencer item、不建 semantic mailbox/token、固定 inactive且仅发布 monitor analysis sample；reset 中每个 ENABLED topology 已取得 stale item 必须 `item_done()` 后清本地状态并强制 inactive，reset release 后先完成 NORMAL/inactive baseline 并写 done proof | 不释放 owner、不在 function 驱动、不把 metadata 接到 DUT wire |
| `L2tlb_agent_agent_sequencer.sv` | 保存唯一单槽 wrapper handle、`EMPTY/PUBLISHED/CONSUMED/DROPPED` 状态、sample event 和 CAS/recycle helper；driver 预留后同步通知 monitor，monitor 返回后才唤醒 sequence 消费。 | 不保存跨 testcase lifecycle truth、不保存第二个 queue 或 monitor backlog。 |
| `memblock_dispatch_base_sequence.sv` | 移除 `drain_sfence_events()` 直通调用 | 不改变 adapter matcher |
| `dispatch_monitor_event_adapter.sv` | 唯一 raw fence service、C4 delete、adapter drain/reset ack | 不改 response token |
| `memblock_main_dispatch_auto_build_main_table_base_sequence.sv` | stop 后停止 routing并继续 service；等待 owner/driver 完成当前 epoch close/admission closed；只调用完整 `release_grantable(owner, current_reset_epoch)`，其中匹配 generation 的 raw-intake close、final inactive、monitor final settled、final mailbox recycle proof/mailbox EMPTY、closing、response/adapter queue drain、当前 epoch required ack、grant 前 non-reset 条件均必须成立，才发送带 owner/epoch/generation 的 grant，并等待 claim=0 | 不代替 owner 清 pending |
| `tc_base.sv`、`basicTest.sv` 和具体 testcase/vseq | build_phase reload plus；唯一 setter 在启动前锁定 mode/topology/ack mask | 不根据 claim 失败猜测拓扑 |

### 6.1 运行期状态唯一写者

```text
package lifecycle coordinator：responder mode、owner claim/release、topology、reset epoch/ack mask、sample/event history、response-owner history cursor、状态字段存储/只读检查；它只请求/等待 reset ack，不直接清 response token、driver item 或 adapter raw/live 状态，也不作为 admission close 的普通运行期直接写者。
CSR monitor：唯一推进 global sample，发布 CSR history 与 CSR reset ack。
fence monitor：以当前 global sample 入队 raw fence、参与同 sample lifecycle event reason 合并；唯一在完整 raw sample 后写 raw-fence producer settled/intake close。
L2TLB owner：pending/driving/token、response barrier、UID marker/cancel/complete、owner-reset-done；唯一在真实 drv_cb 的 capture 后写 admission-settled 和 close request，并生成带 current reset epoch 的 stop/final item。
L2TLB driver：唯一从真实 `drv_cb` 冻结 fire/ready/item metadata，并写 post-reset baseline done proof、`admission_closed/owner/generation/cutoff`、final-inactive done/transport sample 序号和 mailbox recycle proof；sequence 先把 `item_owner_name` 写入 xaction，driver 再冻结为 `last_driven_item_owner_name` 并复制到 sample 的 `sampled_item_owner_name`；确认 final 时还必须检查 frozen owner、reset snapshot 和 fire=0；每个 `drv_cb` 先回收 terminal mailbox，即使没有下一 item 也继续运行；reset 时只对本地 stale item 执行一次 `item_done()` 后清理并驱动 inactive，在 owner reset-done、slot EMPTY 后进入 reset-quiescent，唯一写 response ack，后续 reset-active sample 不再重新发布 semantic mailbox。
L2TLB monitor：唯一通过 `transport_sample_imp.write()` 同步处理 driver sample；reset request 更新 monitor-local active epoch 和 ack seq floor，
reset 时按 epoch drop stale/future-check，以 `monitor_reset_sample_processed_epoch/transport_sample_seq` 和
`monitor_reset_ackable(epoch, reset_sample_seq)` 在匹配 reset sample 已处理、无 processing 且 seq 严格递增后写带 epoch+seq 的 MONITOR ack；
release 时只用 `monitor_final_settled_epoch/transport_sample_seq` 和 `monitor_final_sample_settled(epoch, seq)` 确认精确 final sample。
二者是独立状态，reset ack 不依赖 final settled，final settled 也不写 reset ack；monitor 不修改 frozen wrapper payload。
reset snapshot：reset coordinator 是 live `reset_active/current_reset_epoch` 的唯一发布者，driver 是把它复制到 `sampled_reset_active/sampled_reset_epoch` 的唯一 sample 发布者；sequence、monitor 和确认 helper 不得自行读取 live 值解释旧 sample。
reset coordinator：按 ack mask 请求/等待 response、fence monitor、adapter、CSR 和 L2TLB monitor reset；不直接改任何职责队列。
dispatch adapter：唯一清 raw fence/context/pending invalidate/live entry/range index 并回报 adapter ack，负责 raw fence destructive 消费和 C4 删除；不写 owner admission state、不消费 lifecycle event history。
parent：global stop、停止新 routing、drain 只读汇总、release grant；不得直接写 close request。
```

## 7. 与原测试框架逻辑对比及修改类型

| 修改类型 | 原有逻辑 | 修改原因 | 优化后逻辑 |
|---|---|---|---|
| 生命周期功能修正 | claim 只检查当前 active；release 后可再次 claim。 | 旧状态无法证明旧 owner 已完成，可能出现错误 handoff。 | testcase 首次 claim 后置 `owner_claimed_once`；第二次 claim/handoff 直接 fatal。 |
| 启动功能修正 | default sequence 和 explicit vseq 都可能启动同类 responder。 | claim 失败太晚，且可能先产生 driver item。 | testcase start 固定 DEFAULT 或 EXPLICIT；第二路径在启动前 fatal。 |
| no-owner 边界新增 | `SEQ_EN=0` 只让 sequence body return。 | 需要 response 的 flow 会静默卡住。 | `responder_mode=DISABLED/NO_OWNER` 明确禁止 owner/grant；需要 response 的 flow 启动前 fatal。 |
| 时基功能修正 | L2TLB sequence 使用 local `sample_seq++`，其他 monitor 使用 package latest。 | C0/C4/CSR history 可能错拍。 | CSR monitor 唯一推进 global sample；其它 producer 与 service 只 peek。 |
| CSR 功能修正 | runtime latest 只在 payload changed 时更新，直接用于 request/response。 | V2 filter 使用固定 C-2 CSR。 | 每 sample 保存 history；request/response 按 C-2 深拷贝读取。 |
| event 功能修正 | 只保存 latest flush event，或把 history 当成 active service 停顿后的可回放 backlog。 | 同拍重复会漏事件；旧 C0 回放又会错杀其后新 token/UID。 | 同 sample 合并 reason，跨 sample 有界 history；只有 response owner 维护 cursor。ready 未开放时旧 event 仅作无工作 baseline，ready 已开放后 event 必须等于 current sample；adapter 只消费带 provenance 的 raw FIFO。 |
| transport/lifecycle 功能修正 | driver sample 通过 monitor FIFO 异步消费，release/reset 依赖 FIFO drain/empty。 | FIFO backlog、wrapper payload 生命周期和 reset 并发消费会造成 sample 丢失、重复处理或错误 release。 | driver 通过 analysis port 同步调用 monitor `analysis_imp.write()`；mailbox 与 analysis 共享同一 frozen wrapper handle，release 只检查 monitor processing/settled，不再引入 FIFO drain。 |
| flush 功能修正 | C0 立即删 pending，并伪造 `record_flush_killed_request()`。 | C0 request 已真实 fire，V2 到 C4 才 flush。 | C0 建 barrier；C1-C3 可完成；C4 禁止 fire 并取消未完成旧 token/UID。 |
| consumer 功能修正 | dispatch service 直接调用 `drain_sfence_events()`。 | C0 提前删 live entry且可能双消费。 | adapter 唯一 destructive consumer，按 raw anchor 到 C4 删除 canonical/range index。 |
| reset 功能修正 | 通用 raw clear 清 global sample，owner 本地处理 reset。 | 旧 event/due 可能影响新周期。 | 以 `rst_n` 下降沿建立 reset epoch，协调 response/fence monitor/adapter/CSR ack；保留 owner/topology/global sample。 |
| stop/release 功能修正 | main flow 在主表 terminal 和 flushSb 完成后退出。 | L2TLB/adapter 仍可能有 pending 工作；parent negedge 也不能追溯关闭此前已驱动的 ready 窗口，raw FIFO 瞬时为空也不能证明 fence monitor 不会再入队，reset 后旧 item 还可能残留；final sample 若没有 owner terminal ack，driver 无法 recycle，grant 也会互等。 | parent 只提出 global stop/停止 routing；driver 先完成本拍 bounded anchor/probe 后才读取 current sample；owner 在下一真实 sample capture 已发 fire 后写 admission-settled 与当前 epoch close request。reset release 先完成 NORMAL/inactive baseline，下一 drv_cb 才允许 stop；每个 stale item 必须 `item_done()` 后强制 inactive 丢弃。driver 确认 admission cutoff 且 token/driving/barrier 清空后，owner 只把 marker=0 的 unbound UID 转为 `CANCELED`；marker 非零 WAITING 继续 fatal。driver 只接受匹配 generation/epoch 的 stop，且 final item同样要求冻结 fire=0/ready=0/resp_valid=0；owner 消费 frozen final sample 后先写 closing、再 CONSUMED ack，driver 下一 drv_cb 回收 mailbox 并写 recycle proof；fence monitor 再完整处理一个 raw sample并写同 epoch/generation intake close，closed 后 raw valid fatal；adapter 并行 drain；parent 仅在完整 `release_grantable(owner, current_reset_epoch)` 为真时原子写 grant 并 trigger wakeup，该谓词还要求 mailbox EMPTY、当前 epoch required ack 和非 reset；owner 唤醒后最后清 claim。 |
| idle-stop 功能修正 | `IDLE_STOP_CYCLE` 会让 active sequence 自行 stopping/退出。 | 破坏 owner 持续到 parent grant 的合同。 | 只输出 no-progress 诊断；global stop 是唯一正常退出触发。 |
| 异常路径功能修正 | `do_kill()`、driver `phase_ended()` 直接 release。 | 隐藏未收敛状态，function 也不能驱动时钟任务。 | active owner 的 `do_kill()` 和 `phase_ended()` 都只 fatal/保留 claim；唯一 release 要求 final inactive、closing、grant、response/adapter queue drain 和 raw-fence intake close。 |
| 保持不变 | 每次 request fire 的 token 数量、latency/reorder、payload、range candidate 和 UID multicast 语义。 | 单 owner 只收敛所有权，不改变接口请求/响应语义。 | 继续由四份专项负责。 |

### 7.1 关键函数的逻辑变化

**`send_l2tlb_cycle()` 抽象功能：** 按一个 global sample 完成接口采样、reset/flush 判断、response completion、request
capture 和下一拍 item 生成；它不处理 adapter raw fence 的 destructive 消费。

```text
旧行为：drv_cb -> local sample++ -> 读 latest flush -> C0 删除 pending/伪造 canceled -> 选择 response -> 发送 item。
新行为：drv_cb 锁存输入 -> 同拍 bounded anchor/probe -> sample_valid=0 或 NOT_READY 时冻结 fire/response 为 0、发送 inactive 并结束本拍
  -> anchor 成功后才 peek global sample -> baseline tag 尚未完成时先发送/确认 NORMAL baseline -> READY 时读取 NBA/history+event sample-ready
  -> 保存旧 event cursor -> reset 优先处理 -> current event 建 C0 barrier；pre-ready 旧 event 只对齐 cursor，`acceptance_opened_since_reset=1` 后旧 event fatal
  -> 真实 fire 以旧 cursor 独立建 token
  -> C1-C3 完成合法 response -> C4 禁止 fire并 cancel旧工作
  -> 若 frozen sample 是 matching RELEASE_FINAL_INACTIVE：走 dedicated final 分支，先检查 monitor settled，写 closing，ack final sample；下一 drv_cb recycle 后只等待 grant/reset wakeup，不再生成下一 item
  -> 其余 sample 才按 stop/hold/history/capacity 生成下一 item。
```

**`reset_l2tlb_runtime_state()` 抽象功能：** 由统一 reset coordinator 使所有旧周期对象失效并等待 ack；它不重新
claim。reset detector、response owner、adapter 和 CSR monitor 的调用顺序必须按第 3.11 节执行。

**`service_l2tlb_sfence_events()` 抽象功能：** 在 dispatch service 中按 raw sample 安排 C4 live-entry 删除；它只修改
adapter 所属 queue/map，不修改 response owner 的 pending token。

## 8. 关联专项文档对齐结论

| 文档 | 本文给出的统一约束 |
|---|---|
| `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2tlb_response_random_payload_plan_20260729.md` | payload builder、raw/derived copy 和 UID response multicast 仍由该归档文档负责；不得新增第二个 owner/token queue。 |
| `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2tlb_sfence_flush_token_timing_correction_plan_20260805.md` | C0/C4、C-2 history、response due、warm-up stop、raw-fence intake close 与 reset/release 使用本文 global sample/owner 边界。 |
| `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2tlb_range_lookup_napot_plan_20260806.md` | range candidate、rank、NAPOT 和统一 delete helper 已完成并归档（`6a1b2d947e`）；adapter 只调用 helper。 |
| `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_sfence_hfence_stage_aware_live_entry_invalidation_plan_20260804.md` | stage matcher、raw fence decode、topology gate 和 raw intake close 由该归档文档负责；raw fence destructive consumer 固定为 adapter。 |

四份专项的当前状态必须以各自文件头为准，不能继续概括为“四份 `undo` 均未 coding”：timing correction 与 response
random payload 已归档至 `plan/do`，并记录了完成的 compile/smoke 与独立终审；stage-aware 已归档至 `plan/do`，已完成静态检查、
远端 compile、基础 smoke 和 real-dispatch smoke；range lookup/NAPOT 也已归档至 `plan/do`，核心实现为 `6a1b2d947e`。
本文只提供跨专项的单 owner 生命周期合同，不代替上述状态记录，也不把任一归档 plan 重新变成待执行 plan。当前没有待执行的
L2TLB coding 专项；四份已归档 `plan/do` 仅作为已实现行为和历史决策的证据，不作为新的重复 coding 入口。

## 9. 审核结论与未完成边界

当前明确需要优化的逻辑包括：responder mode/唯一 owner 启动与 claim、`SEQ_EN=0` no-owner、global sample/CSR history、C0/C4 barrier、
raw fence 单一消费者与 intake close、runtime reset epoch/ack、response/adapter queue drain/release 和异常退出保护。它们均属于测试框架生命周期逻辑，不能由
payload 或 RM 专项隐式补齐。

本文仍未形成独立 coding 实现，不能把审核结论本身当作 compile/smoke 通过证明；但四份关联 L2TLB 专项均已完成并归档。
本文保留在
`AI_DOC/plan/test_framework/review_doc/undo`，继续作为历史问题与跨专项合同的复核材料；各专项 plan/review 的归档与验证
状态只以对应文档头和 implementation review 为准。

## 10. 末轮一致性修正

本节是对上文历史段落和早期摘要的最终优先级说明，不改变其问题回溯用途。后续 coding 必须同时满足：

```text
drv_cb sample：
  冻结 VIF/item metadata
  -> 同拍 bounded anchor/probe
  -> anchor 成功后 peek current global sample
  -> 再解释 CSR history/event/token；禁止 peek 在 probe 前发生。

reset / stale item：
  get_next_item 已返回 stale item
  -> drive inactive
  -> item_done() 恰好一次
  -> 丢弃本地句柄；不调用 stop/final confirm。
  未取得 item 的 abort 路径不调用 item_done()。

reset release：
  current epoch NORMAL/inactive baseline 在 baseline_sent_sample_seq 之后的真实 ready=0/fire=0/resp_valid=0 sample
  -> driver 写 baseline done proof
  -> 下一 drv_cb 才允许 close/RELEASE_STOP。

final inactive：
  matching generation/reset epoch 的 RELEASE_FINAL_INACTIVE
  + frozen sampled_req_fire=0
  + sampled_req_ready=0
  + sampled_resp_valid=0
  -> driver 唯一写 final_inactive_done。
```

reset coordinator 仅负责 epoch、请求和 ack 收敛；response owner 直接清 token/UID/barrier 后发布 owner-reset-done，driver 清本地 item 后唯一写 response ack，
fence monitor 直接清 raw producer settled/intake-close/context-dedup 并写 FENCE ack，adapter 直接清 raw/context/pending-invalidate/live-entry/range-index，CSR monitor 直接清 history/CSR context。上述 direct-writer
边界优先于任何“coordinator 清 adapter queue”或“response owner/driver 共同清 token”的旧描述。本节只规定跨专项的
生命周期语义；实际实现、compile、smoke 和 runtime blocker 的状态必须分别以对应 `plan/do`、`plan/undo` 与
implementation review 记录为准。

## 11. 2026-08-10 审核补充

### 11.1 reset release 不是可丢弃的空拍

审核发现“CSR 在 release callback 不发布 sample”的临时方案不满足单 owner 的 global-sample 合同：CSR 先运行时，
fence monitor 已看到 reset inactive，却没有同拍 anchor 可读。最终方案把 release 边定义为第一个 post-reset sample；
若 fence 先运行，它只在同一 NBA/delta 窗口等待 CSR 结束 reset，随后仍按同拍 anchor 写 FENCE producer done。
因此任何调度顺序都不会留下半个 producer barrier，也不会漏掉 release 边的 `sfence.valid`。

### 11.2 no-owner 必须同时禁止 grant 和历史 owner 残留

审核发现 parent 不能仅依赖“当前是否 claim”来判断 grant 时机。对于 `DISABLED/NO_OWNER + NO_DISPATCH`，
`l2tlb_lifecycle_owner_claimed`、`l2tlb_owner_claimed_once` 和 `l2tlb_release_granted` 都必须为 0；否则说明本 testcase
错误启动过 owner 或留下了 release 状态，应当 fatal。只有 `ENABLED + DISPATCH_ACTIVE + needs_response` 且仍持有真实
claim 的 lifecycle 才允许 parent 发 `grant_l2tlb_final_release()`。

这两项实现和重新验收由
`mem_ut_v2_l2tlb_sfence_flush_token_timing_correction_implementation_review_20260809.md` 第 12 节作为权威记录；
本审核文档继续只提供问题归类和跨专项生命周期约束。
