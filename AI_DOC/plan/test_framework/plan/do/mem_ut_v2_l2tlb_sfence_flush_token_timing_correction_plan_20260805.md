# V2 L2TLB SFENCE/HFENCE Token 时序修正执行 Plan

| 项目 | 内容 |
|---|---|
| 状态 | coding 完成；P0/P1 及其后发现的 baseline provenance/epoch-0 blocker 均已修复，explicit/base compile 与 smoke 均通过，独立终审已明确 `FINAL PASS`；本文件已归档至 `plan/do`。 |
| 目标版本 | V2 (`mem_ut_uvm_v2`) |
| 关联归档 plan | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2tlb_response_permission_adapt_execution_plan_20260708.md` |
| 关联 live-entry plan | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_sfence_hfence_stage_aware_live_entry_invalidation_plan_20260804.md` |
| 共享生命周期审核结论 | `AI_DOC/plan/test_framework/review_doc/undo/mem_ut_v2_l2tlb_single_owner_lifecycle_optimization_review_20260807.md`；本文件服从其单 owner、sample、reset、response/adapter queue drain、raw-fence intake close 和 release grant 合同 |
| 权威源码 | `src/main/scala/xiangshan/mem/MemBlock.scala`、`src/main/scala/xiangshan/cache/mmu/Repeater.scala` |
| 修改范围 | `memblock_sync_pkg.sv`、fence/CSR monitor、`memblock_dispatch_base_sequence.sv`、`memblock_main_dispatch_auto_build_main_table_base_sequence.sv`、`dispatch_monitor_event_adapter.sv`、`memblock_l2tlb_base_sequence.sv`、`L2tlb_agent_agent_xaction.sv`、`L2tlb_agent_agent_driver.sv`、相关 testcase/vseq 启动协调；不修改 DTLB/L2TLB interface payload |

## 1. 专有名词与抽象功能说明

| 术语 | 含义与状态落点 | 示例 |
|---|---|---|
| request fire | 同一 sample 中锁存到 `req_valid && req_ready` 的真实 DTLB 到 L2TLB 握手。每次 fire 必须有一个 token。 | C0 的 ready 来自 C-1 cycle item；即使 C0 同拍采到 SFENCE，只要二者为 1，C0 request 已被 DUT 接收。 |
| token | `memblock_l2tlb_pending_req` 的动态 request 账本对象，位于 `pending_q` 或 `driving_req`。 | token 保存 C0 的 VPN、DTLB-side CSR snapshot、entry snapshot 和 due sample；它不拥有唯一 UID。 |
| flush barrier | monitor 观察到一笔 SFENCE/HFENCE 或翻译 CSR changed 后建立的延迟取消记录。 | C0 event 建立 `due_filter_flush_sample=C4`。 |
| flush event record | 按 sample 聚合的不可变 lifecycle event，含唯一 `event_seq`、`reason_mask` 与 anchor sample。 | 同 sample 的 `CSR_CHANGE|FENCE` 只是一条 record 和一个 barrier。 |
| anchor sample | monitor 首次采到该 flush event 的 sample 序号。 | C0 是本 event 的 anchor。 |
| due filter flush sample | V2 DTLB filter 真正完成 flush 的最早 sample。 | 两级 `RegNext` 加 `FenceDelay=2`，所以 C0 的 due 为 C4。 |
| DUT global sample | 由 CSR monitor 在每个 post-reset `posedge mon_cb` 唯一调用 `advance_dut_global_sample()` 推进的 testcase 内单调连续采样序号；其它 monitor、driver、adapter 和 service 只读。 | CSR history、flush event/barrier、raw fence、token/UID 等跨组件比较的唯一时基。 |
| flush epoch | 由单调 `event_seq` 标识的一次 flush barrier。token 保存接受时最后已消费的 event 序号，用于判定它是否早于某个 barrier。 | C0 新 event 的序号为 9；接受时序号小于 9 的 token 在 C4 仍未完成则取消。 |
| driving response | 已写入上一 cycle item、等待当前 DUT sample 确认外部 L2TLB response fire 的唯一 response slot。 | 对 C0 barrier，外部 fire 必须严格早于 C4；不得在 C3 发起一个 C4 fire。 |
| DTLB-side CSR snapshot | 与当前 `PTWNewFilter` 输入拍对齐的固定深度 CSR 历史项。 | C sample 的 L2TLB request 使用顶层 C-2 snapshot。 |
| response-visible CSR snapshot | 当前 external response fire 时 `PTWNewFilter` 内 `PTWFilterEntry` 实际读取的 filter CSR。 | response 在 C2 fire 时使用 top C0 的 CSR；它可与 token/UID 建立时冻结的旧 context 不同。 |
| UID request-fire marker | 一个 WAITING UID 首次被 responder 观察到对应 DTLB request fire 的 DUT global sample；0 表示尚未观察。 | C0 fire 的 UID 可在 C4 cancel；C0 后未 fire 的 UID 不应仅因 issue 时间被取消。 |
| connect takeover active | L2TLB force/connect 已接管的静态 capability。 | `l2tlb_responder_active` 在 connect `initial` 中设定；它不表示 sequence 仍在运行。 |
| lifecycle owner claimed | 当前 L2TLB sequence 从 claim 到 release 的动态 response token/UID 所有权。 | `l2tlb_lifecycle_owner_claimed` | 防止两个 sequence 同时驱动 response 或改写 token/UID 账本；它不拥有 raw fence。 |
| NO_OWNER passive sampler | `DISABLED/NO_OWNER` 下仍运行的 driver 物理采样分支；它不启动 sequence、不取 sequencer item、不 claim，也不建 token。 | `L2tlb_agent_agent_driver` passive mode | 它固定驱动 `ready=0/resp_valid=0`，发布 analysis sample 供 monitor 对 `req_valid` 做 fail-fast；它不是 responder。 |
| dispatch topology contract | testcase/dispatch coordinator 在 dispatch service 启动前设置的固定运行态。 | `dispatch_l2tlb_lookup_active` | 当前只支持 `ENABLED + DISPATCH_ACTIVE` 或 `DISABLED/NO_OWNER + NO_DISPATCH`；不是 plusarg、connect 或 owner 镜像。 |
| lifecycle release-safe | 当前 token/UID owner 可无歧义退出的静止状态。 | `is_l2tlb_lifecycle_release_safe()` | token、barrier 与全部有效 UID `WAITING` record 都为空后才可 release；raw fence/invalidate 由 adapter 独立收敛。 |
| sample coordinator | CSR monitor 唯一推进 global sample 并发布本拍 CSR history；CSR/fence reason 合并后发布 lifecycle event-ready，所有其它组件只读取。 | `advance_dut_global_sample()`、`peek_current_dut_global_sample()`、`csr_history_published_seq`、`lifecycle_event_published_seq` | CSR monitor、fence monitor、L2TLB driver 位于同一 posedge 域，history 与 event-ready 后 L2TLB 才解释本拍 token。 |
| sample anchor wait | 同拍 monitor 先后不确定时，等待 CSR monitor 为当前 `mon_cb` 建立 global sample 的只读等待。 | `wait_for_l2tlb_sample_anchor($time)` | fence monitor 先醒来时等待，不自行 advance，也不因正常调度顺序立即 fatal。 |
| sample producer barrier | 当前 sample 的 CSR/fence producer 都已报告完成；没有事件也要报告“已检查完本拍”。每个 producer 只写自己的 mask bit，CSR monitor 不代写 FENCE bit。 | `sample_producer_done_mask`、`mark_l2tlb_sample_producer_done()` | required mask 收齐前不得消费本拍 event；空 reason 不会被误造为 flush。 |
| adapter drain done | adapter-owned raw fence 工作已经静止的只读状态；它只说明现有队列为空，不证明 fence monitor 已停止产生新 raw。 | `dispatch_l2tlb_live_entry_drain_done()` | `raw_sfence_q`、待绑定 context 和 `sfence_invalidate_pending_q` 都为空。 |
| raw-fence intake settle/close | fence monitor 已处理一个完整 sample，随后在与当前 owner close generation、reset epoch 匹配的边界封闭新的 raw fence 输入。 | `l2tlb_raw_fence_producer_settled_sample_seq`、`l2tlb_raw_fence_intake_closed_*` | close request 于 C10 建立；fence monitor 先处理 C11 仍可能由 stop 前驱动的 raw，再在 C11 封闭 intake；C12 新 raw 必须 fatal。 |
| reset barrier | reset 时阻止新 admission、取消旧周期工作并重新 warm-up 的统一边界。 | reset coordinator、response reset helper、live-entry reset helper | reset 前登记的 C4 work 不能在 reset 后删除新建 entry。 |
| CSR history ready | 当前 posedge 的 CSR history 已发布的只读状态。 | `csr_history_published_seq` | 只能说明 C-2 history 可查，不代表同 sample 的 fence reason 已完成合并。 |
| sample-ready barrier | 当前 posedge 的 CSR history 与 lifecycle event watermark 都已发布的只读状态。 | `csr_history_published_seq`、`lifecycle_event_published_seq`、`wait_for_dut_sample_ready_at_drv_cb()` | driver 的同拍 probe 返回 `READY` 后才处理 C-2 history/C0 event；`NOT_READY` 结束本拍，不跨 clock 等待。 |
| sample-ready probe result | 在当前 `drv_cb` 已完成接口锁存、NBA 和 producer 发布窗口后返回 `READY` 或 `NOT_READY` 的结果；`NOT_READY` 不跨到下一 clock edge 阻塞。 | `wait_for_dut_sample_ready_at_drv_cb()` | `NOT_READY` 只走单拍 inactive/warm-up 分支；下一拍重新采样，不带旧 VIF 值继续处理。 |
| metadata latch | driver 对上一拍已驱动 item 的 kind、generation、reset epoch、baseline tag 及真实 fire/ready/resp_valid 采样值的轻量本地快照；它不是 sequencer item 句柄。 | `last_driven_*` fields | `item_done()` 后仍保留该 latch 供下一 `drv_cb` 确认；清理 latch 不得再次 `item_done()`。 |
| transport sample latch | driver 在唯一 `drv_cb` 事件边界读取同一 posedge 的 `mon_cb` 输入采样，并冻结不可变 4-state VIF 包、reset 状态/epoch、final proof 和生成该 item 的 owner provenance；sequence 与 monitor 只消费它，不再直接读取 VIF 或 live lifecycle 状态。 | `memblock_l2tlb_drv_sample_t`、`publish_l2tlb_transport_sample()` | 一个 sample 同时携带 req/resp、metadata、owner、reset、global sample、probe result；token capture 与 final confirm 使用同一份数据。 |
| sample TLM wrapper | 把不可变 transport sample 作为 agent 内部 TLM 数据传递的 `uvm_object` 包装；其唯一 payload 是冻结的 `memblock_l2tlb_drv_sample_t`。 | `L2tlb_agent_agent_transport_sample` | driver analysis port、sequence mailbox 和 monitor analysis imp 同步观察同一份 frozen payload；wrapper freeze 后禁止任何 consumer 改写。 |
| analysis imp | UVM 的同步 analysis 接收端；driver 调用 analysis port 的 `write()` 时，monitor 在同一调用中处理 wrapper，不形成第二个 sample 队列。 | `transport_sample_imp`、`L2tlb_agent_agent_monitor::write()` | `write()` 返回时 monitor 已完成该 sample 的 epoch/4-state/diagnostic 处理。 |
| monitor reset ackable | monitor 已同步处理指定 epoch、指定 transport 序号的 reset-active sample 且当前不在处理 sample 的 reset ack 条件。 | `monitor_reset_sample_processed_epoch/transport_sample_seq`、`monitor_reset_ackable(epoch, reset_sample_seq)` | 它只服务 runtime reset ack，不等待 final inactive；reset ack 的 seq 与 final settled 的 seq 独立。 |
| monitor final sample settled | monitor 已同步处理指定 epoch、指定 transport 序号的 final inactive sample 的 release 条件。 | `monitor_final_settled_epoch`、`monitor_final_settled_transport_sample_seq`、`monitor_final_sample_settled(epoch, seq)` | 它只服务 release gate，不能代替 MONITOR reset ack。 |
| monitor active epoch | monitor 自己接受 sample 的当前 reset epoch；由 monitor reset request 更新，不是 consumer 对某份 sample 重新读取的 live package reset。 | `monitor_active_epoch` | 用于把 incoming frozen sample 判为 current/stale/future。 |
| reset ack tuple | monitor 对一次 runtime reset 返回的 `{reset_epoch, transport_sample_seq}`；seq 必须来自本次已处理的 reset-active sample，且与上次 reset ack 严格递增。 | `l2tlb_monitor_reset_ack_epoch`、`l2tlb_monitor_reset_ack_transport_sample_seq` | coordinator 不能用旧 epoch 的 ack 冒充新 reset；该 seq 与 final-settled seq 分开保存。 |
| frozen final proof | driver 在确认 `RELEASE_FINAL_INACTIVE` 的同一 `drv_cb` 写入 transport sample 的 final 完成元数据；它不是 live package 字段的事后读取。 | `sampled_final_inactive_proof_valid/epoch/transport_sample_seq` | monitor 只依此标记 final settled；reset 后旧 proof 不会被新 epoch 当作完成。 |
| semantic consumer | 消费 transport sample、执行 C0/C4、token/UID、baseline/close 和下一 item 构造的 sequence 逻辑。 | `memblock_l2tlb_base_sequence::process_l2tlb_transport_sample()` | 它不能调用 `@drv_cb`、`mon_cb` 或任何 VIF getter；下一 item 仍通过原 UVM sequencer 交给 driver。 |
| 4-state sample | 保留 `logic` 的 0/1/X/Z 采样值；在计算 fire 前先检查 X/Z。 | `sampled_req_valid/ready/fire/resp_valid` | X/Z 不能被 2-state `bit` 隐式转换成 fire=0。 |
| post-reset baseline | reset release 后当前 epoch 必须先完成的一次 `NORMAL/inactive` driver item 采样；它证明新 epoch 已重新建立无 fire/无 response 的 transport 基线。 | driver local `post_reset_baseline_pending`、`baseline_sent_sample_seq`、`last_driven_is_post_reset_baseline`、共享只读 proof `l2tlb_post_reset_baseline_done_epoch/sample_seq` | driver 发送带本地 tag 的 NORMAL/inactive item 时记录发送 sample；严格在 `sample_seq > baseline_sent_sample_seq` 的下一真实 sample 以无 fire/无 response 证明完成 baseline；proof 前不能接受或确认 `RELEASE_STOP/RELEASE_FINAL_INACTIVE`。 |
| `item_done()` transport release | `try_next_item()` 取得的 UVM item 无论正常发送还是 stale 丢弃都必须完成一次 sequencer 握手释放。 | `seq_item_port.item_done()` | stale item 不能只从 driver 本地句柄清除，否则 sequencer 会被旧 item 卡住。 |
| release-grantable | 指定 ENABLED owner 已满足最终 release grant 的全部只读条件；不发 grant、不清 claim。 | `release_grantable(owner_name, reset_epoch)` | parent 发 grant 与 owner 原子 release 前使用同一谓词，包含 response/adapter drain、monitor 同步消费完成、final recycle proof 和 mailbox EMPTY。 |
| owner admission-settled watermark | owner 已在一个 `sample_ready_result==READY` 的真实 `drv_cb` sample 完成本拍 request capture 与 UID registration 的完成标记；它不是 transport close。 | `l2tlb_owner_admission_settled_sample_seq` | parent 的 negedge global-stop 只提出停止意图；owner 下一 READY sample 先结算此前 ready 窗口的 fire，再依据该 watermark 写 admission seal。 |
| release admission request | parent 的 `global_stop_requested` 是停止意图；唯一 owner 在下一真实 `drv_cb` 已结算本拍 admission 后写入 close request，并在同一拍投递 `ready=0` stop item；它不等于 transport 已关闭。 | `l2tlb_release_admission_close_requested`、`close_request_sample_seq`、`close_request_reset_epoch` | seal 写入前已 fire 的 UID/token 仍是旧工作；seal 写入后的任一后续 helper 调用不得再注册新 UID，即使仍在同一 sample。 |
| release admission close | owner 生成的 stop `ready=0` item 在真实 `drv_cb` 被采样后，由 driver 以冻结的 `sampled_req_fire=0` 确认的 responder transport 关闭状态；它早于 drain 和 release closing。 | `l2tlb_release_admission_closed`、`l2tlb_release_admission_cutoff_sample_seq` | cutoff sample 的 `ready=0` 不会 fire；其前已真实 fire 的工作继续 drain；之后 transport 不得再有新 fire。 |
| release generation | owner 为每个 close request 分配的单调 testcase 内编号；它关联 stop、final inactive、closing 与 grant。 | `l2tlb_release_admission_close_generation` | runtime reset 作废当前 close request，但不回绕 generation。 |
| release item kind | 只在 xaction/driver 内使用的本地 metadata；每个 lifecycle item 同时冻结 owner、generation 和 reset epoch。 | `l2tlb_release_item_kind`、`l2tlb_release_item_owner_name`、`l2tlb_release_item_reset_epoch`、`is_post_reset_baseline` | `RELEASE_STOP` 与 `RELEASE_FINAL_INACTIVE` 都可为 ready=0，但职责完全不同；baseline tag 只允许与 `NORMAL` 组合，driver final/stop 校验冻结 owner，且所有 metadata 都绑定生成它的 reset epoch。 |
| grant reset epoch | release grant 所属的 runtime reset epoch；grant 只能被同一 epoch 的 owner 消费。 | `l2tlb_release_grant_reset_epoch` | reset 与 global stop 重叠时，旧 epoch grant 不能释放 re-arm 后 owner。 |
| event sequence baseline | 不随 event history 清空的最后已分配 event 序号；用于 reset 后重新对齐 cursor/dedup。 | `last_allocated_l2tlb_event_seq` | history 清空后，下一条 event 从 baseline+1 分配，旧 event 不会被重新消费。 |
| `EVENT_SEQ_NONE` | 没有 response-side lifecycle event 的固定哨兵值。 | `memblock_sync_pkg::MEMBLOCK_L2TLB_EVENT_SEQ_NONE=0` | `DISABLED/NO_OWNER + NO_DISPATCH` 只发布 reason/watermark，`note_l2tlb_flush_event()` 返回该值，不创建 history 或 barrier。 |
| acceptance-opened state | 当前 reset epoch 中 responder 曾经真实生成过可接受 `next_ready=1` 的既有 active-event 时基标记；它不是 DUT wire。 | `memblock_l2tlb_base_sequence::acceptance_opened_since_reset`，只在 reset/re-arm 时清零 | 为 1 后旧 sample event 不能再作 startup baseline；普通 flush/pre-ready hold 不得清它，也不能用 ready opportunity 替代它。 |
| pre-ready event baseline | `acceptance_opened_since_reset==0` 时，对早于当前 sample 的 event 只推进 cursor/诊断，并设置独立的保守 ready hold；不建立 C0/C4 barrier 或取消工作。 | response owner 的 event-history 消费分支 | 该分支要求本拍和既有状态均无 fire、response、token 或 UID waiting；它不能用于 active responder 的 service 停顿恢复。 |
| pre-ready hold | startup/reset 尚未 acceptance-opened 时因旧 event 建立的 4 拍 ready 抑制边界；不代表发生了可取消的 C0 barrier。 | `pre_ready_hold_until_sample` | current sample 加 `MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES` 后才允许生成首次 ready opportunity。 |
| ready opportunity | lifecycle block/hold 解除后，owner/driver 真实生成一拍合法 `next_ready=1` 的独立标记；它与 ready 电平和 idle 计数分离。 | `ready_opportunity_since_lifecycle_block` | reset/flush 清 0；生成合法 ready item 后置 1，之后才允许累计 idle-stop 诊断。 |
| event history retire | ENABLED topology 下 response owner 在 cursor 连续前进后回收已经消费的 history 前缀；producer 不 pop history。runtime reset 时 CSR monitor 按 reset direct-writer 合同清整张 history；`DISABLED/NO_OWNER + NO_DISPATCH` 则根本不创建 history record。 | `retire_l2tlb_event_history_prefix()`、CSR monitor reset handler | 正常运行先回收 `event_seq <= response_owner_event_cursor`，再允许 producer 使用空出的容量；序号 baseline 永不回绕。 |
| final sample terminal ack | final inactive item 跨过 driver 采样后生成的 frozen transport sample；它仍是单槽 mailbox 的一份 `PUBLISHED` sample，必须由 owner 先完成 closing 再以 `CONSUMED` 终态确认。 | `sampled_final_inactive_proof_*`、`ack_l2tlb_transport_sample()` | driver 在 F 确认 final；owner 在同一 final sample 建 closing 并 ack；driver 在 F+1 才能 recycle。 |
| release-state wakeup | 只用于唤醒已经 ack final sample、等待 parent grant 或 runtime reset 的 owner 的 package 级 `uvm_event`；它不保存 grant，不是第二份生命周期状态。 | `l2tlb_release_state_changed_ev`、`wait_for_l2tlb_release_grant_or_reset()` | parent 原子写 grant 后 trigger；reset coordinator 原子发布 reset epoch/active 后 trigger；owner 醒来后总是重新读取 canonical state。 |
| direct writer | 一个运行期状态只能由其职责组件直接清理并写 ack/proof；coordinator 只请求和等待。 | response owner、driver、fence monitor、adapter、CSR monitor | reset coordinator 不能代替 driver 回收 mailbox，也不能代替 adapter 清 raw FIFO。 |
| reset-quiescent | driver 已完成当前 reset epoch 的本地 stale-item/slot 清理并已回 RESPONSE ack 后的保持态；仍持续驱动 inactive/reset sample，但不再发布 sequence semantic mailbox sample。 | `L2tlb_agent_agent_driver` 本地状态 | 直到 reset release 和 post-reset baseline 重新建立前，不会重复回 ack 或重新投递旧 item。 |

### 1.1 固定同步边界与诊断上限

以下上限是测试框架同步协议的固定编译期/local 常量，不新增 runtime plusarg，也不代表 DUT 的硬件容量：

| 常量 | 固定值 | 作用 |
|---|---:|---|
| `MEMBLOCK_L2TLB_SAMPLE_PROBE_MAX_DELTA` | `2` | 一个 `drv_cb` 内最多执行两次 delta/NBA probe；probe 不允许等待 `@posedge`、`@mon_cb` 或下一次 `drv_cb`。 |
| `MEMBLOCK_L2TLB_SAMPLE_NOT_READY_MAX_SAMPLES` | `8` | 连续 `NOT_READY` 或 sample anchor 未建立的最大真实 sample 数；超过后 `uvm_fatal` 并打印 anchor、sample、两个 watermark、reset epoch。 |
| `MEMBLOCK_L2TLB_BASELINE_MAX_SAMPLE_DISTANCE` | `8` | baseline item 已发送后，等待下一真实 sample 完成 proof 的最大 sample 距离；超过后 `uvm_fatal`。 |
| `MEMBLOCK_L2TLB_SAMPLE_MAILBOX_DEPTH` | `1` | driver 与 sequence 的 transport sample mailbox 深度；发布前必须确认上一份 sample 已消费，禁止覆盖未消费 sample。monitor 通过同步 analysis imp 消费，不存在第二个 FIFO 深度。 |

这些值只用于暴露 monitor/driver/sequence 调度或生命周期未收敛，不能把 watchdog 超时当成正常 idle-stop，
也不能通过增大上限来掩盖 producer 没有发布本拍 sample 的错误。

**`record_l2tlb_flush_barrier()` 抽象功能描述：** 在 monitor 同拍观察到 flush 时记录其 anchor、due sample 和
event 序号，并立即阻止框架在后续边界继续开放 request ready。它不删除 `pending_q`、不改写 live entry、也不把
当前 sample 已 fire 的 request 伪装成 canceled。

**`apply_due_l2tlb_flush_barriers()` 抽象功能描述：** 在每个 DUT global sample 的完成 response 步骤之后，处理已到
DTLB filter 实际清空边界的 barrier，取消仍未完成的旧 token 与 UID waiting instance 并完成生命周期账本。
它不使用 `sfence.bits.flushPipe`；selector 必须保证 due sample 不存在将要在本拍 fire 的 `driving_req`。

**`wait_for_dut_sample_ready_at_drv_cb()` 抽象功能描述：** 该 helper 在 driver 已锁存当前 `drv_cb` 接口值后，
以该 `drv_cb` 的 sample anchor 为输入，只等待同一仿真时刻内的 NBA、CSR anchor 和 CSR/fence producer 发布窗口；它在
完成 anchor 后才读取无副作用的 current sample，并返回 `sample_valid/sample_seq/READY/NOT_READY`。它不跨到下一 clock edge
阻塞、不推进 global sample、不消费 raw fence。返回 `READY` 才允许主循环读取 C-2 history、回放 event、处理 token/UID；
返回 `NOT_READY` 或 `sample_valid=0` 时本拍送 inactive，并在下一拍重新锁存 VIF，不能复用旧 sample 或旧 item 的 fire。

```text
wait_for_dut_sample_ready_at_drv_cb(drv_anchor_time, output dut_sample_seq, output sample_valid):
  dut_sample_seq = 0；sample_valid = 0；result = NOT_READY。
  probe_count = 0；
  while (probe_count <= MEMBLOCK_L2TLB_SAMPLE_PROBE_MAX_DELTA) begin
    若 CSR monitor 已为 drv_anchor_time 建立本拍 anchor：break；
    若 probe_count == MEMBLOCK_L2TLB_SAMPLE_PROBE_MAX_DELTA：break；
    uvm_wait_for_nba_region()；#0；probe_count++；
  end
  仅在当前 drv_cb 的 bounded NBA/producer window 等待；不得执行 @posedge、@mon_cb 或跨到下一 drv_cb。
  若 CSR monitor 尚未为 drv_anchor_time 建立本拍 anchor：
    consecutive_not_ready_samples++；若超过 MEMBLOCK_L2TLB_SAMPLE_NOT_READY_MAX_SAMPLES：uvm_fatal；
    返回 NOT_READY；调用者只允许用已冻结 VIF 驱动 inactive，不能调用 peek 或写 admission-settled/close。
  anchor 已建立后：
    dut_sample_seq = peek_current_dut_global_sample()；若为 0 则 uvm_fatal（anchor 与 sample 编号不一致）。
    sample_valid = 1。
  若 csr_history_published_seq > dut_sample_seq 或 lifecycle_event_published_seq > dut_sample_seq：
    uvm_fatal；producer watermark 超前于同拍 sample，说明 sample 发布顺序或 global sample 绑定错误。
  若 csr_history_published_seq < dut_sample_seq 或 lifecycle_event_published_seq < dut_sample_seq：
    consecutive_not_ready_samples++；若超过 MEMBLOCK_L2TLB_SAMPLE_NOT_READY_MAX_SAMPLES：uvm_fatal；
    result = NOT_READY；返回；调用者下一 drv_cb 必须重新 anchor，不能保留本拍 dut_sample_seq 作为下一拍输入。
  // 只有两个 watermark 都严格等于当前 sample 才是 READY。
  若 csr_history_published_seq == dut_sample_seq 且 lifecycle_event_published_seq == dut_sample_seq：
    consecutive_not_ready_samples = 0；result = READY；返回。
  // 上面的严格比较已经处理所有 watermark 关系；这里仅保留防御性不可达分支。
  否则：
    consecutive_not_ready_samples++；若超过 MEMBLOCK_L2TLB_SAMPLE_NOT_READY_MAX_SAMPLES：uvm_fatal；
    result = NOT_READY；返回；调用者下一 drv_cb 必须重新 anchor，不能保留本拍 dut_sample_seq 作为下一拍输入。
```

### 唯一 transport sampler 与 sequence consumer

本 plan 固定 `L2tlb_agent_agent_driver` 为唯一物理 transport sampler。它是唯一允许在
`@vif.drv_mp.drv_cb` 上等待时钟、读取同一 posedge 已冻结的 `vif.mon_mp.mon_cb` 输入、冻结 req/resp 和 lifecycle metadata 的组件；
`memblock_l2tlb_base_sequence` 不再独立等待 `@l2tlb_vif.drv_cb`，只消费 driver 发布的
`memblock_l2tlb_drv_sample_t`。这样 sequence 的 token capture、C4 cancel、baseline/close 以及 driver 的
stop/final confirm 都引用同一拍、同一份 immutable sample latch。

**`publish_l2tlb_transport_sample()` 抽象功能描述：** driver 在真实 clocking-block 边界冻结一份 transport sample，
附带当前 global sample、sample-ready 结果、上一拍 item metadata 和 4-state req/resp 值，并发布一个可被唯一
semantic consumer 取得的 sample。它不执行 token/UID 语义，不推进 global sample，也不修改 adapter state。

**`wait_l2tlb_transport_sample()` 抽象功能描述：** sequence 按顺序取得 driver 发布的不可变 sample；它不读取 VIF、
不推进 sample、不重新计算 fire。若 sample producer/consumer 拓扑不匹配或队列出现重复/跳过，立即 fatal。

```text
L2TLB driver（唯一物理采样者）：
  每个 @vif.drv_cb：
    1. 先调用 recycle_l2tlb_transport_sample_at_drv_cb()：若上一 slot 已是 CONSUMED/DROPPED，按序号回收为 EMPTY；
       若该序号等于 final_inactive_transport_sample_seq，写 l2tlb_transport_sample_recycle_done_seq。此步骤不等 sequencer item，
       final sample 后也必须在下一真实 drv_cb 执行。随后以 try_next_item() 非阻塞轮询下一 item；无 item 时走 inactive，
       不得阻塞在 get_next_item()，这样 reset request 一定能在下一 drv_cb 被观察并生成 reset-active sample。
    2. 只等待一次 @vif.drv_cb；不再额外执行 @vif.mon_cb。通过同一 posedge 的
       vif.mon_mp.mon_cb 冻结 req_valid、req_ready、VPN、s2xlate、resp_valid 和 response payload；
       所有 interface 值先保留为 logic 4-state，上一拍 item metadata 从 driver local latch 复制。
    3. 调用 wait_for_dut_sample_ready_at_drv_cb(anchor_time, ...)，完成同一时刻 bounded probe；
       probe 结束后构造 memblock_l2tlb_drv_sample_t(sample_seq、sample_valid、READY/NOT_READY、VIF latch、metadata latch)。
    4. 在发布前调用 validate_l2tlb_transport_sample_4state()：req_valid/req_ready/resp_valid 为 X/Z 时 fatal（reset-active
       的 idle 分支也必须明确记录 reset，不得把 X/Z 当作 0）；已知值才计算
       sampled_req_fire = (req_valid === 1'b1) && (req_ready === 1'b1)。
    5. 由 driver 直接依据 sample latch 确认 `admission_closed`、baseline done 或 final-inactive；确认结果写回 package。
       若本拍 final-inactive confirm 成功，同时把 `{valid=1, epoch=sampled_reset_epoch, transport_sample_seq}` 写入尚未 freeze 的
       working sample；否则写 valid=0/zero。此后不再修改 working sample。
    6. 将完整 working sample 封装为 sample_ref、调用 freeze()；若 ENABLED，先以 sample_ref/transport_sample_seq 原子
       写 mailbox PUBLISHED，再调用 transport_sample_ap.write(sample_ref) 同步处理 monitor。这样 reset/abort 不会在 analysis write
       与 mailbox publish 之间错误看到 EMPTY。若为 final sample 后的 recycle-only 边界，只向 monitor 发布 inactive diagnostic sample，
       不创建新的 semantic mailbox sample。DISABLED/NO_OWNER 始终只向 monitor 发布。
       任一已 PUBLISHED sample 必须由唯一 response owner 终态确认；driver 不得覆盖 slot。随后处理当前 sequencer item 的
       abort/stale/normal transport，严格一次 item_done。

sequence（唯一 semantic consumer）：
  while owner 仍有效：
    sample_ref = wait_l2tlb_transport_sample()；不得再执行 @drv_cb、读取 l2tlb_vif 或调用 VIF getter。
    sample_ref.get_payload(sample)；只取得冻结 struct 的本地副本，sequence 不得写 sample_ref。
    若 sample.baseline_required=1 且 proof 尚未完成，构造带 current epoch tag 的 NORMAL/inactive/baseline item；
      driver 记录 baseline_sent_sample_seq，下一真实 sample 才能确认 proof。
    对该 sample 的纯软件语义、token/UID/close 决策完成后，且在任何可能等待 driver 的 start_item/finish_item 前，调用
      ack_l2tlb_transport_sample(sample.transport_sample_seq, CONSUMED)。
    若 reset/abort 分支不执行正常语义，则由同一 response-owner sequence 在返回前调用
      ack_l2tlb_transport_sample(sample.transport_sample_seq, DROPPED)。
    若 reset/sample_valid/READY/NOT_READY 分支需要 inactive，则仅在已写 terminal ack 后构造 NORMAL 或 lifecycle item 并通过 start_item/finish_item 交给 driver。
    若 sample 携带匹配 `RELEASE_FINAL_INACTIVE` 的 frozen final proof：它不是普通 idle sample。
      owner 先核对 owner/epoch/generation/transport_sample_seq 与 monitor final-settled，调用 begin_l2tlb_release_closing()；
      随即对该 final sample 写 CONSUMED，禁止再 start_item/finish_item 或等待另一份 semantic sample；
      然后只等待 package 的 release-state wakeup。下一真实 drv_cb 由 driver 回收该 terminal slot，parent 随后发 grant；
      grant 或 reset 唤醒后再重新读取 canonical grant/reset state，不能把 final proof 或 mailbox ack 当作 grant。
  若 sample_ready=READY，则按 sample 中冻结的旧 event cursor 执行 current-sample C0 或 pre-ready baseline freshness gate，
    再处理 C0/C4、token/UID 和 response C-2 snapshot，
      再构造下一 cycle item；若 global stop 已提出，按 baseline/close gate 生成带 metadata 的 stop item。
  只通过 item transaction 向 driver 提供下一拍输出；不直接确认 admission/final，也不消费 raw fence queue。
```

`sample mailbox slot` 是单向 driver->sequence 的单槽 mailbox；slot 状态固定为 `EMPTY -> PUBLISHED -> CONSUMED`
或 `PUBLISHED -> DROPPED`。slot 保存 `transport_sample_seq` 和同一个
`L2tlb_agent_agent_transport_sample sample_ref` handle；driver 是 sample publish/PUBLISHED 的唯一写者，response owner
就是该唯一 L2TLB sequence，也是 CONSUMED/DROPPED 的唯一写者：正常路径完成纯软件语义后、任何
`start_item/finish_item` 前以 sample_seq 原子确认 CONSUMED；reset/abort 时由同一 sequence 的 mailbox-drain task 以同一规则确认
DROPPED。driver 是 `CONSUMED/DROPPED -> EMPTY` 的唯一复用写者，并在同一 bounded
driver 边界校验 sample_seq 后只清 slot handle、terminal ack 和状态，不清 wrapper 内冻结的 payload；wrapper payload 的
只读性不因 mailbox 回收而改变。sequence 在此之前不得写第二次终态。driver 在 slot 未回到 EMPTY 前不得覆盖或发布下一 sample；reset coordinator 不直接改 slot。发布与 abort 同拍时，
已发布 sample 必须由 sequence 恰好一次标记 DROPPED，未发布 sample 不得伪造 drop。任何重复 ack、sample_seq 不匹配或 watchdog
超时均 `uvm_fatal`，不能留下满槽让 owner 静默卡住。sample latch 的 VIF 字段使用 `logic`，metadata 字段可使用 enum/整数；
两者不能混用为一份 2-state transaction。

为避免 reset 与发布同拍时遗漏 PUBLISHED slot，driver 的 publish 是一个不可拆分的顺序：先确认/预留 EMPTY slot 并写入
frozen `sample_ref + transport_sample_seq + PUBLISHED`，再调用同步 `transport_sample_ap.write()`。`write()` 是 function，
不会跨 clock 让 sequence 在 monitor 未处理前取得 sample；reset coordinator 即使在其后运行，也只能观察 PUBLISHED 并通知
response owner 用 CAS 置 DROPPED。coordinator 自己不能清 slot。ENABLED owner 必须在 watchdog 内消费 PUBLISHED reset sample，
否则 `uvm_fatal`；这不是允许 coordinator 代替 consumer 的例外。

driver 必须使用 `try_next_item()` 而非跨拍阻塞 `get_next_item()`。无 item 时仍在每个 `drv_cb` 驱动 inactive、冻结并发布
transport sample；收到 reset request 后下一 `drv_cb` 必须发布 `sampled_reset_active=1` 的 sample。这样 monitor 的 reset ack
和 sequence 的 DROPPED ack 都有确定的唤醒来源，不能出现 owner claim 保留但 driver 永久等待 item 的死锁。

slot 的 `PUBLISHED` 表示 handle 已经被原子预留，不能直接表示 sequence 可以立即读取。driver 必须在同步
`transport_sample_ap.write()` 返回后才触发 `new_sample_event`；sequence 只在该 event 后调用 get helper。这样既让 reset
在 publish window 中看到非 EMPTY slot，又保证 sequence 不会在 monitor 处理完成前写 CONSUMED/DROPPED。

```text
memblock_l2tlb_drv_sample_t:
  longint unsigned transport_sample_seq // driver 每次发布递增，含 sample_valid=0；仅用于 mailbox/ack 顺序
  longint unsigned dut_sample_seq       // sample_valid=0 时允许为 0；否则等于 CSR monitor 的 DUT global sample
  bit sample_valid
  bit sampled_reset_active
  longint unsigned sampled_reset_epoch
  enum {READY, NOT_READY} sample_ready_result
  logic sampled_req_valid
  logic sampled_req_ready
  logic sampled_resp_valid
  logic [37:0] sampled_req_vpn
  logic [1:0] sampled_req_s2xlate
  bit sampled_req_fire              // 只能在三项 logic 已通过 X/Z 检查后由 driver 写入
  item_kind/generation/reset_epoch/baseline_tag previous_item_metadata
  string sampled_item_owner_name       // 从同一 item 的 xaction 字段冻结，不能从当前 package owner 回填
  bit baseline_required             // driver local pending 的只读镜像，供 sequence 生成 tagged NORMAL/inactive
  bit baseline_proof_pending        // baseline item 已发送、等待更晚真实 sample proof 的只读镜像
  longint unsigned baseline_sent_sample_seq
  bit sampled_final_inactive_proof_valid
  longint unsigned sampled_final_inactive_proof_epoch
  longint unsigned sampled_final_inactive_proof_transport_sample_seq
```

monitor-local reset ack state is separate from final state:

```text
monitor_active_epoch
monitor_reset_pending_epoch
monitor_reset_sample_processed_epoch
monitor_reset_sample_processed_transport_sample_seq
monitor_reset_ack_floor_transport_sample_seq
monitor_last_reset_ack_transport_sample_seq
l2tlb_monitor_reset_ack_epoch
l2tlb_monitor_reset_ack_transport_sample_seq
```

中文文字伪代码：reset request 到达时，monitor 保存当前请求 epoch，并把上一次已经回报的 reset ack transport 序号作为
本次 floor；收到 `sampled_reset_active=1` 的 frozen sample 后，同时记录该 sample 的 epoch 和
`transport_sample_seq`。只有这两个值与 pending request 匹配、monitor 已退出 processing，且 seq 严格大于 floor 时，
monitor 才写 ack 的 epoch/seq。final settled 使用另一组 epoch/seq 字段，不能复用 reset ack tuple。

实现时 mailbox 的 payload 类型必须是 `L2tlb_agent_agent_transport_sample`，而不是再次复制一份
`memblock_l2tlb_drv_sample_t`；analysis port 和 mailbox 必须传递同一个 `sample_ref` handle。driver 回收 EMPTY 只置
`slot.sample_ref=null`，不得通过 handle 修改已 freeze 的 wrapper 内容。

`sampled_final_inactive_proof_*` 仅由 driver 在同一 `drv_cb` 的 final-inactive confirm 成功后填入工作 payload，再调用
`freeze()` 发布：valid=1 时 epoch 必须等于 `sampled_reset_epoch`，transport 序号必须等于本 sample 的
`transport_sample_seq`；非 final sample 的 valid=0、其余字段置 0。monitor/sequence 不得在 freeze 后从 live
`final_inactive_done` 或 live reset 推导或改写该 proof。

`reset_active` 与 `current_reset_epoch` 的 live 值唯一由 reset coordinator 发布；driver 在每个 `drv_cb` 原子复制这两个值到
sample 的 `sampled_reset_active/sampled_reset_epoch`，之后 sequence、monitor 和确认 helper 只读取冻结字段，不能读取 live reset
值自行解释该 sample。`sampled_req_valid`、`sampled_req_ready` 与 `sampled_resp_valid` 必须来自同一 `mon_cb` latch，不能把 driver
上一拍计划输出、sequence 的 transaction 字段或不同 clocking block 的值混合成 fire。`sampled_req_fire` 不是额外采样值，
只能由已验证的 `logic` 三态输入以 case equality 计算。mailbox 的 publish/consume 必须带连续序号检查：重复、跳号、覆盖未消费
sample 或 sequence 消费已标记 dropped 的 sample 均为 `uvm_fatal`。

**`ack_l2tlb_transport_sample()` 抽象功能描述：** sequence 对已经处理或因 abort/reset 丢弃的 sample 做一次带 sample_seq
的终态确认；它不清 payload、不推进 global sample。driver 在下一 `drv_cb` 看到该确认后回收 slot；该序号是每次发布递增的
`transport_sample_seq`，不使用可能为 0 的 `dut_sample_seq`，
`l2tlb_transport_sample_mailbox_empty()` 才返回 1。

```text
sequence acknowledge_l2tlb_transport_sample(transport_sample_seq, terminal_kind):
  通过 compare-and-swap 取得 `slot.state==PUBLISHED && slot.transport_sample_seq==transport_sample_seq` 的唯一终态写权；
  terminal_kind 只能为 CONSUMED 或 DROPPED；CAS 失败时若 slot 已有同一 terminal ack 则重复 ack fatal，否则 epoch/seq 不匹配 fatal。
  原子写 slot.state=terminal_kind、slot.terminal_transport_sample_seq=transport_sample_seq；sequence 不清 payload、不直接写 EMPTY。

driver 在下一真实 drv_cb 的 publish 前：
  若 slot.state==CONSUMED 或 DROPPED：要求 terminal_transport_sample_seq==slot.transport_sample_seq；只清 mailbox slot 的 sample handle、
  terminal ack 和状态，原子写 slot.state=EMPTY；不得清零或修改 wrapper 的 frozen payload。monitor 已在 driver 的同步 analysis_port.write()
  调用中完成该 sample 的消费，不能把 mailbox handle 清理误认为修改 wrapper 内容。
  若 slot.state==PUBLISHED：上一 sample 尚未消费，禁止覆盖并按 mailbox watchdog 诊断；
  若 slot.state==EMPTY：允许发布本拍 sample 并写 PUBLISHED。
```

```text
driver final recycle：
  每个 drv_cb 的第一步执行普通 terminal recycle；不等待 try_next_item()。
  若被回收的 terminal_transport_sample_seq == final_inactive_transport_sample_seq：
    写 l2tlb_transport_sample_recycle_done_seq=terminal_transport_sample_seq；
    该边界之后只保持 inactive/monitor diagnostic，不再向 semantic mailbox 投递新 sample。
  release_grantable() 同时要求 mailbox EMPTY 且 recycle_done_seq==final_inactive_transport_sample_seq。

driver reset recycle：
  每个 reset-active drv_cb 先执行 terminal recycle；若 response_owner_reset_done_epoch==current_reset_epoch、
    本地 fetched/stale item 已清、slot=EMPTY 且本 epoch response ack 尚未发送：
      设置 driver_reset_quiescent_epoch=current_reset_epoch；不在该拍再次发布 semantic mailbox sample；
      只驱动 inactive，并由 driver 唯一写 l2tlb_response_reset_ack_epoch。
  quiescent 期间继续保持 inactive，不重复 ack；reset release 时由 coordinator 清 quiescent，重新进入 baseline flow。
  因此 response ack 发生在本 epoch 最后一 slot 回收之后，不会出现 ack 后同一 drv_cb 又把 slot 写回 PUBLISHED。
```

**`close_dispatch_raw_fence_intake_for_release()` 抽象功能描述：** 该 fence-monitor helper 在已经完整采样当前 raw-fence
input 后，为当前 owner close generation 写入 raw producer 的关闭证明。它不 pop raw FIFO、不删除 live entry、不修改 response token；
adapter 仍独占后续 C4 schedule/delete。

本文后续伪代码中的 `admission_settled_sample_seq`、`close_requested`、`request_owner_name`、`close_request_reset_epoch`、
`close_generation`、`admission_closed`、`admission_closed_generation`、`raw_fence_settled_sample_seq`、`raw_fence_intake_closed`、
`raw_fence_closed_epoch`、`raw_fence_closed_generation`、`raw_fence_cutoff_sample`、`item_kind`、`item_generation`、`item_reset_epoch`、
`release_closing`、`grant_owner_name` 等均是
`mem_ut_v2_l2tlb_single_owner_lifecycle_optimization_review_20260807.md`“生命周期状态唯一命名”表所列规范字段的阅读别名；
实现不得为任一别名创建第二份 package state。

## 2. 修改原因与 V2 时序合同

归档 plan 当前在 monitor 的 C0 event 到达时立即删除 `pending_q`，并将 C0 同拍 request fire 直接记为
`flush-killed`。这早于 V2 DUT 的实际 filter flush，产生两类错误：

1. C0 的 `valid && old_ready` 是已发生的真实 request fire；立即丢弃会让测试框架丢失 DUT 已接收的 request。
2. C1-C3 内该 token 仍可能收到合法 response；提前删 token 后，环境会找不到 response 的账本或错误匹配新 entry。

V2 权威路径为：

```text
C0：顶层 monitor 采到 io.ooo_to_mem.sfence 或翻译 CSR changed
  -> MemBlock.scala: RegNext(RegNext(io.ooo_to_mem.sfence/tlbCsr))
  -> C2 到达 PTWNewFilter 输入
  -> Repeater.scala: DelayN(sfence.valid || CSR changed, FenceDelay=2)
  -> C4 清 PTWFilterEntry.v 与 inflight_counter
```

`sfence.bits.flushPipe` 不改变上述条件。V2 MemBlock 的 load/store/prefetch DTLB 是 `TLBNonBlock`，其
`dtlb.io.flushPipe` 固定驱动为 `false.B`。同一条 Fence uop 的 writeback `ctrl.flushPipe` 是送往完整 Core ROB
的独立扇出；只有该 uop 到 ROB head 并提交后，ROB 才会产生 `flushAfter` redirect。standalone MemBlock
测试框架不能把此字段当作“立即删除 DTLB/L2TLB token”的请求。

## 3. 状态与所有权 Flow

### 3.1 新增/调整状态

在 monitor/common package 与 `memblock_l2tlb_base_sequence` 的 token/UID owner 之间维护以下状态：

```text
memblock_l2tlb_flush_event_history[$]
  每项保存 event_seq / reason_mask / sample_seq / sample_time；队列有固定 compile-time 上限。
  `memblock_sync_pkg` 新增 `localparam longint unsigned MEMBLOCK_L2TLB_EVENT_SEQ_NONE = 0;`；它是 no-owner 无 history 的
  返回哨兵，不是一个可分配 event，也不进入 cursor、barrier 或 raw-fence FIFO。
  last_allocated_l2tlb_event_seq 独立于队列保存；每次新 sample 分配 event 时递增，history clear 不将其归零。
  response_owner_event_cursor 与 last_seen_flush_event_seq 只表示“已经处理到的最后 event_seq”，不以空 history 的数组下标表示。
  sample_seq 只由 CSR monitor 的 advance_dut_global_sample() 产生，不能使用 sequence 本地计数或其它 producer 推进。
  `MEMBLOCK_L2TLB_EVENT_SEQ_NONE=0` 表示没有 response-side history record。`note_l2tlb_flush_event(sample_seq, sample_time, reason)`
  在 ENABLED topology 对同 sample 复用同一项并 OR reason_mask；在 `DISABLED/NO_OWNER + NO_DISPATCH` 只记录本拍 reason，返回
  `MEMBLOCK_L2TLB_EVENT_SEQ_NONE`，不分配 event_seq 或写 history。fence monitor 只在 dispatch-active 时把非 0 event_seq 冻结到
  raw.lifecycle_event_seq，供 dispatch adapter 将 raw fence 与同一个 C4 live-entry delete 绑定。
  CSR/fence 同 sample producer 均完成后发布 lifecycle_event_published_seq=sample_seq。dispatch-active 时只有 response owner
  维护有效 history cursor；adapter 不读取或回收 event history，只消费 raw fence FIFO，并使用 raw 冻结的 event/sample provenance。
  response owner 是唯一允许在 cursor 连续推进后回收 `event_seq <= response_owner_event_cursor` 前缀的组件；producer 不 pop、不重排、不清空 history。
  history 满时 producer 直接报告容量/时序错误；即使 cursor 已越过队首但 owner 尚未在本拍 retire，也不能由 producer 代替 owner 回收。
  `last_allocated_l2tlb_event_seq` 独立保存且不回绕。
  若本 sample 没有 CSR_CHANGE/FENCE reason，producer barrier 仍发布 lifecycle_event_published_seq，但不创建空 history record。
  `DISABLED/NO_OWNER + NO_DISPATCH` 时没有 response history consumer；producer 只发布本拍 reason/watermark，不分配 event_seq、
  不创建或追加 history record，也不调用 retire/pop。该模式没有 responder token/UID 或 raw fence FIFO，因而不需要保留 response-side
  event；adapter 始终只消费 dispatch-active 时的 raw fence FIFO，不建立 history cursor。

memblock_l2tlb_flush_barrier barrier_q[$]
  event_seq / reason_mask       对应 flush event record 的唯一编号与原因位
  anchor_sample_seq            monitor 观察到该 event 的 DUT global sample
  due_filter_flush_sample_seq  anchor + MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES

accept_flush_event_seq         已存在于每个 token；表示该 token fire 前 sequence 已消费的最新 event_seq
accept_hold_until              barrier_q 中最大 due；期间下一 cycle item 的 ready 必须为 0
pre_ready_hold_until_sample   仅 startup/reset 未 acceptance-opened 的旧 event baseline 设置的独立 hold；期间 ready 必须为 0，不能当作 barrier due
ready_opportunity_since_lifecycle_block  reset/flush hold 后是否已经真实生成一拍合法 next_ready=1；为 0 时不得累计 idle-stop 诊断
due_barrier_this_sample        本 DUT global sample 是否存在 `due == dut_sample_seq` 的 barrier；它只禁止本拍选择新的旧 response，不等同 ready hold

l2tlb_owner_admission_settled_sample_seq
  唯一由 L2TLB owner 在 `sample_ready_result==READY` 的真实 drv_cb sample 中、完成本拍 request fire capture 与 UID marker/registration 后写入。
  它只证明 owner 已结算该 READY sample 的 admission，不推进 global sample、不改变 ready、不关闭 transport。reset、sample_valid=0
  或 NOT_READY/warm-up 均清为/保持为 0；不得用“本拍没有 fire”替代 READY。global stop 若在 warm-up 提出，必须先完成 current
  epoch baseline，等到下一 READY sample 再发布 watermark 和建立 close request。

l2tlb_release_admission_close_requested / l2tlb_release_admission_request_owner_name /
l2tlb_release_admission_close_request_sample_seq / l2tlb_release_admission_close_reset_epoch /
l2tlb_release_admission_close_generation
  parent 只通过 global_stop_requested 提出停止意图，不能在 negedge 直接写这些字段。唯一 owner 在随后真实 drv_cb 的
  sample_ready_result 必须为 READY，且 capture/UID registration 已结算、owner_admission_settled_sample_seq 等于当前 global sample 后置位；它是软件 admission seal，
  不是已经完成的 DUT transport close。写 request 时分配不回绕的 close_generation，并冻结 current_reset_epoch 与 request sample。
  这样已经在 parent negedge 前驱动的 ready=1 窗口仍先被 owner 如实 capture，seal 之后的同拍后续 helper 调用才全部非法。
  owner 在同一个 drv_cb 生成下一 cycle 的 item，强制 req_ready=0、item_kind=RELEASE_STOP、item_generation=close_generation、
  item_reset_epoch=current_reset_epoch；
  该 item 允许携带一笔已存在的旧 response，不是 final inactive item。

l2tlb_release_admission_closed / l2tlb_release_admission_owner_name /
l2tlb_release_admission_closed_generation / l2tlb_release_admission_cutoff_sample_seq
  唯一由 L2TLB driver 在随后的真实 drv_cb 冻结带当前 close_generation 的 RELEASE_STOP item、req_ready=0 与
  sampled_req_fire=0 后，调用 package confirm helper 原子置位。owner 只生成 stop item 并读取该确认结果，不直接写这些字段。
  cutoff_sample_seq 固定为这个 ready=0 已被采样的 sample；它先于 drain、final inactive 与 release_closing。
  cutoff 前已经真实 fire 的 request 允许继续完成；cutoff sample 的 stop item 不得 fire。closed 后任何 request fire 或
  capture_fired_request() 新 token 分支均为 uvm_fatal。
  仅 req_valid 不构成 admission，DUT 在 ready=0 后保持 valid 不能被误报为 fire。

l2tlb_raw_fence_producer_settled_sample_seq / l2tlb_raw_fence_intake_closed /
l2tlb_raw_fence_intake_closed_reset_epoch / l2tlb_raw_fence_intake_closed_generation /
l2tlb_raw_fence_intake_cutoff_sample_seq
  唯一由 `fence_agent_agent_monitor` 写入。每个非 reset sample 在它已采样 `sfence.valid`、完成 raw 入队或确认本拍没有
  raw 后写 producer-settled watermark；该 watermark 不消费 FIFO。若当前 dispatch-active 的 owner close request 已存在，且本 monitor
  已完整处理一个严格晚于 `close_request_sample_seq` 的 sample，则该 monitor 将 raw-fence intake 绑定当前
  `close_request_reset_epoch/close_generation` 并置 closed。这个“晚一完整 sample”的要求保留 stop 前已驱动 input 的最后一次
  raw 采样机会；closed 后任何新的有效 raw fence 都是 routing/monitor 合同错误，`push_raw_sfence()` 必须 uvm_fatal。
  adapter 不写 intake closed，只继续 drain 已入队 raw/context/pending invalidate；`dispatch_l2tlb_live_entry_drain_done()` 也不以
  队列瞬时为空代替该 closed proof。runtime reset 清除上述 active metadata；同一 owner re-arm 后必须按新 reset epoch/new generation
  再次封闭，不能复用 reset 前 closed 状态。global stop 后 fence monitor 仍必须运行到该 producer close sample，不能先随 phase
  退出，否则 parent 必须把它诊断为 lifecycle timeout 而不是发放 grant。

l2tlb_release_granted / l2tlb_release_grant_owner_name / l2tlb_release_grant_reset_epoch
  / l2tlb_release_grant_generation
  parent 仅在 release_grantable(owner, current_reset_epoch) 为真时写 grant，并同时冻结 grant_owner_name、
  grant_reset_epoch 和 grant_generation。grant 不是 admission close，也不清 owner claim。
  owner 仅在 grant owner、grant reset epoch、grant generation 都匹配当前 owner/current_reset_epoch/current close_generation 时消费；
  runtime reset 作废 active grant。

l2tlb_release_state_changed_ev
  这是 package 级 `uvm_event`，只负责唤醒 final sample 已经 terminal ack 的 owner；它不保存 owner、epoch、generation、grant 或
  mailbox 状态，所有真值仍以上述 canonical fields 为准。parent 在同一原子动作中写完 grant 的四个字段后才 trigger；reset coordinator
  在原子发布 reset_active/current_reset_epoch 并作废当前 grant 后 trigger。owner 醒来后必须重新检查 reset 优先于 grant，且只能在
  grant 与 `release_grantable()` 都成立时清 claim。driver recycle、monitor settled、adapter drain 不 trigger 该 event，避免把普通
  progress 错当成 grant。

l2tlb_release_item_owner_name / l2tlb_release_item_kind / l2tlb_release_item_generation / l2tlb_release_item_reset_epoch / is_post_reset_baseline
  xaction/driver 的本地 lifecycle metadata，只允许 NORMAL、RELEASE_STOP、RELEASE_FINAL_INACTIVE 三值；另有
  `string item_owner_name`，由 sequence 写入 xaction，driver 在 get_next_item 返回后复制到
  `last_driven_item_owner_name`，再复制到 transport sample 的 `sampled_item_owner_name`；这些字段不驱动 DUT wire。
  `is_post_reset_baseline=1` 只能出现在 `item_kind=NORMAL` 且 `req_ready=0/resp_valid=0` 的 baseline item；它不驱动 DUT wire。
  driver 采样的 item owner/kind/generation/reset_epoch/baseline tag 必须与该 item 一同冻结，不能用当前 package owner、当前
  sequence 计划或下一拍 item 推断。
  reset_active 时旧 epoch 的 stop/final item 只能由 driver 在 `try_next_item()` 返回后精确 `item_done()`、丢弃并记诊断，不能确认任何
  release state；reset release 后也不能重放旧 item。当前 epoch 的 stop/final 在 post-reset baseline proof 产生前同样非法。

l2tlb_release_final_inactive_item_done / l2tlb_release_final_inactive_generation / final_inactive_transport_sample_seq
  只有 item_kind=RELEASE_FINAL_INACTIVE、owner/current close_generation/current reset epoch 全部匹配，且冻结的
  sampled_req_ready=0、sampled_req_fire=0、sampled_resp_valid=0 的 item 在真实 drv_cb 完成采样后才由 driver 置位。普通 idle、flush hold 和 RELEASE_STOP 都不得置该字段；同 generation 的
  第二次 final item 也必须 uvm_fatal，不能被幂等接受。

l2tlb_transport_sample_recycle_done_seq
  driver 在 final sample 已被 sequence 标记 CONSUMED/DROPPED 后，于下一真实 drv_cb 优先把 terminal slot 回收为 EMPTY，
  并把该 final sample 的 transport_sample_seq 写入该字段；没有下一 sequencer item 也必须执行回收。它是 release gate 的
  final mailbox 回收证明，runtime reset 清除，不能由 sequence 或 reset coordinator 代写。

memblock_l2tlb_csr_history[0:2]
  每个 post-reset top CSR DUT global sample 保存 {sample_seq, complete raw CSR snapshot, valid}。
  由 CSR monitor 无条件轮转写入，不以 payload changed 为条件；`runtime_csr_snapshot` 仍独立保留 latest 视图。
  `get_l2tlb_request_csr_snapshot(dut_sample_seq)` 固定返回 sample=dut_sample_seq-2 的完整 copy。
  history 未 warm-up 时 responder 保持 ready=0；已经 fire 却无 C-2 项是接口时序合同破坏，uvm_fatal。

uid_tlb_first_request_fire_sample_seq / uid_waiting_by_vpn_s2xlate
  UID wait 建立时 marker=0 并加入 `{vpn,s2xlate}` 候选索引。每次真实 request fire 后，capture 使用本次
  C-2 CSR/key 仅查询同 shape 的 WAITING UID 并写 marker；complete/cancel 时移除索引项。该 index 不建立
  token-to-UID 绑定，也不替代 response complete 的全表 raw-hit multicast。

owner_start_baseline_done      本 lifecycle owner 是否已完成 event history 的 startup cursor 对齐；早于 current sample 的 event 只能在
                              尚未 acceptance-opened 且没有 active work 时作为 pre-ready baseline 跳过，不能建立补发 barrier。
                              它只服务 token/UID owner startup，不能复用 acceptance_opened_since_reset，
                              也不扫描、清理或 reconcile live entry。

l2tlb_lifecycle_owner_claimed / owner_name
                              由 `try_claim_l2tlb_lifecycle_owner()` 在本 sequence service 前取得，
                              由已收敛的 release gate 最后释放；它不是 `l2tlb_responder_active` 的镜像。

l2tlb_owner_claimed_once
                              testcase 级永久 claim 记录；testcase-start 清零，第一次 claim 置 1，runtime reset 和
                              正常 release 都不清零。它阻止 release 后第二个 sequence 再次 claim。

l2tlb_release_granted
                              parent 仅在 admission 已由 driver 确认关闭、response/adapter queue drain、raw-fence intake 已按当前
                              epoch/generation 封闭、final inactive、closing、current reset epoch ack 完整且 !reset_active 时发放一次带 owner/epoch 的 release grant；owner 随后原子清
                              l2tlb_lifecycle_owner_claimed。claim==0 是 release 完成的唯一权威，
                              不建立第二个 release-complete 镜像状态。driver/phase_ended 不能自行设置或绕过该握手。

lifecycle_release_safe        pending_q 为空、driving_req 无效、barrier_q 为空，且
                              check_l2tlb_release_uid_waiting() 统计的全部有效 UID WAITING 实例数为 0；
                              它是正常 global-stop release 的共同前置条件。`do_kill()` 和 `phase_ended()` 不是 release
                              入口。当前不允许把等待 UID
                              转交给下一 owner，也不按 owner 名称、token 或 key 缩小统计范围。

l2tlb_release_closing / l2tlb_release_closing_owner_name / l2tlb_release_closing_generation
                              admission 已 closed、旧工作已 drain、final inactive item 已在真实 drv_cb 采样且 monitor 已同步确认该精确 sample settled 后设置的短暂
                              package 级 closing 状态。软件 admission 已在更早的 close_requested 写入时封闭；closing 只证明 final release
                              前置条件成立，不得被当成新的 admission seal。成功 release 时与 owner claim 一并清除，非正常路径直接 uvm_fatal。
```

#### 审核修改意见：release 状态字段的唯一先后关系

上面的状态表中，`admission_settled_sample_seq`、`close_requested`、`admission_closed`、`release_closing` 和 `release_granted` 不能互相替代。
后续 coding 统一采用以下顺序；历史段落中把 `closing` 描述成“关闭新 UID 注册”的文字，只保留为旧方案记录：

```text
global_stop 首次成立
  -> parent 在 negedge 只停止新的 dispatch routing，并继续 monitor/adapter service；不得直接改写 close state
  -> owner 下一真实 drv_cb 先完整处理此前 driver 已经驱动的 ready 窗口：完成 request fire capture 与 UID registration
       -> 写 admission_settled_sample_seq=current_sample
       -> close_l2tlb_admission_for_release(owner, current_sample)
          写 close_requested=1、close_request_sample_seq=current_sample、close_request_reset_epoch=current_reset_epoch、单调 close_generation
          从该写入动作之后禁止所有新的 UID registration/token capture；不回溯取消此前已经完成的 mutation
          在同一 drv_cb 生成下一 item 为 item_kind=RELEASE_STOP、req_ready=0
  -> driver 在真实 drv_cb 观察并冻结 stop item：
       读取同一边界冻结的 sampled_item_kind/sample_item_generation/sampled_item_reset_epoch/sampled_req_fire/sampled_req_ready
       仅 RELEASE_STOP + current generation + current reset epoch + ready=0 + fire=0 + sample晚于close request
       才写 admission_closed=1、admission_closed_generation=current generation、cutoff_sample_seq
  -> 已建立的 token/UID 继续 drain；任何 close-request 后或 cutoff 后的新 fire/UID registration 都 fatal
  -> owner 的 response drain 完成后发送独立的 RELEASE_FINAL_INACTIVE item
  -> driver 真实采样该 final item 后置 final_inactive_done/current generation；monitor 同步确认相同 epoch/transport sample settled
  -> owner 再调用 begin_l2tlb_release_closing()；该 helper 只写 closing，不清 claim、不发 grant
  -> adapter drain 与以上 owner 流程并行；parent 独占等待其完成
  -> parent 与 owner 共同调用 release_grantable(owner, current_reset_epoch)
  -> parent 写带 owner/epoch/generation 的 release grant
  -> owner 再核对 owner/epoch/generation/grantable 后原子清 claim
```

`admission_settled_sample_seq` 是 owner 对当前 sample admission 已结算的可检查证明；parent 不拥有它，不能以自己的
negedge service 顺序替代。`close_requested` 是 owner 在该 watermark 已达到当前 sample 后建立的 admission seal：此前已经完成的 mutation 保持合法，但
该 flag 写入后的任何 capture/register 调用都非法；`admission_closed` 才是 transport 不再接受新 request 的权威边界；
`release_closing` 只表示 owner 的最终 inactive 已采样且 response drain 已收敛；adapter queue drain 和 raw-fence intake close 仍可并行；
`release_granted` 只是 parent 的一次授权。任何旧文字若与该顺序冲突，coding 以本审核意见和后面的完整伪代码为准。

`initialize_lifecycle_state()` 和 reset 分支将 `owner_start_baseline_done` 清为 0；第一次 post-reset service sample
完成 event-history cursor baseline 后置 1。当前单-owner 合同中该字段只表示首次启动或同一 owner reset re-arm，不表示
owner handoff。sequence 局部 `sample_seq` 只能留作 debug，绝不能参与 CSR history、barrier due、raw fence、token 或 UID
时间比较。

sequence 在进入 service loop 前必须先 `try_claim_l2tlb_lifecycle_owner(get_full_name(), ...)`；claim 失败立即
`uvm_fatal`，避免两个 sequence 同时驱动 L2TLB response 或改写 token/UID。`l2tlb_responder_active` 只说明 connect
takeover 已建立，不能替代 response lifecycle claim；raw fence 的入队/消费规则由 stage-aware plan 的 dispatch-active
topology 单独定义。

`pending_q`、`driving_req`、accepted/completed/canceled counter 的 owner 仍只有 L2TLB sequence。fence/CSR
monitor 只发布 non-destructive event history；live-entry invalidation flow 只维护自身待删除 queue，不能直接修改
L2TLB token 或 counter。fence 不是 CSR 字段，CSR context 绑定不等于 raw fence 合并；同 sample 的
CSR_CHANGE 与 FENCE 仅在 token lifecycle 的 `reason_mask` 中合并。

`dispatch_l2tlb_lookup_active` 由 testcase/dispatch coordinator 在 dispatch service 启动前设置，并在固定拓扑 testcase
期间保持不变：dispatch service 为 1；当前 no-dispatch 只与 `DISABLED/NO_OWNER` 配对。它不是 `dispatch_monitor_capture_en`、
`l2tlb_responder_active` 或 `l2tlb_lifecycle_owner_claimed` 的镜像；这些状态不能互相推导。no-dispatch 时仍可
按既有 monitor 合同发布 non-destructive flush sideband，但 raw fence 不进入 `raw_sfence_q`，且不存在 live-entry
删除工作。该 topology state 在 reset 时保持，不得因 `clear_raw_monitor_queues()` 被清零；未在首个 post-reset
monitor sample 前设置或在固定拓扑中被改写时立即 `uvm_fatal`。

`get_l2tlb_request_csr_snapshot()` 是访问 `PTWNewFilter` 可见 CSR 的唯一 history 读取入口：request capture
用它构造 key/build payload，response complete 用同一 helper 取得 response-visible CSR 并决定 UID raw hit。
它不更新 `mmu_csr_state`、不改变 flush event，也不把 latest runtime CSR 当作历史项；后者只能继续用于 startup、
monitor 观察和生成新的 top-side CSR 状态。固定三项 history 已覆盖 V2 两级 `RegNext`，不建立无界 CSR queue。

唯一 lifecycle owner 在第一次 ready 前将自己的 event cursor 对齐到已经 event-ready 的 history 尾部；这只跳过 owner 启动前
已经完成的 lifecycle event，不能重锚 barrier、不能清 live entry。reset 后由同一 owner 重新执行该 cursor baseline；不存在
owner A/B 交接，也不执行 live-entry reconcile。若启动时存在早于 current sample 的未消费 event，只能在尚未 acceptance-opened、
且没有 fire/response/token/driving/WAITING UID 时作为 pre-ready baseline 推进 cursor/last-seen；不得建立补发的 startup
barrier 或补做 C4 cancel。live entry 属于 dispatch adapter，不由 response owner handoff 复用或清理。这里的“当前 sample”固定指
`peek_current_dut_global_sample()`；不得因 sequence 本地计数或不同 phase 的 `$time` 重新解释 C-2 或 C4。

连续 SFENCE/HFENCE 或不同 sample 的 CSR change 允许产生多个 barrier；同 sample 的 CSR_CHANGE 与 FENCE
只产生一个带组合 reason mask 的 barrier。任何未到期 barrier 均保持 ready=0；每笔 barrier 在各自 due sample 只取消
`accept_flush_event_seq < barrier.event_seq` 的仍 pending token。这样不会因为第二笔 flush 重复记账，也不会在第一个
barrier 后过早开放 ready。

### 3.2 主循环顺序

在 `drive_l2tlb_loop()` 中将历史 flush 分支替换为以下顺序：

```text
进入 service loop 前：
  try_claim_l2tlb_lifecycle_owner(get_full_name(), current_owner)；失败则 uvm_fatal。

driver 每个 `drv_cb` sample：
  冻结上一 cycle item 对应的 4-state req_valid、req_ready、VPN、s2xlate、resp_valid 与 item metadata；
  调用 wait_for_dut_sample_ready_at_drv_cb(sample_anchor_time, dut_sample_seq, sample_valid)，完成同一 drv_cb 的 bounded probe；
  anchor 成功后才 peek current sample，不能在 probe 前读取 current sample，也不能等待下一 clock。
  调用 validate_l2tlb_transport_sample_4state()；未知 req/ready/resp valid 直接 fatal，已知值才形成 sampled_req_fire。
  driver 依据上一拍 metadata latch 做 reset/stop/final/baseline 的真实 transport confirm，将冻结的
  memblock_l2tlb_drv_sample_t 封装为不可变 wrapper sample_ref 并发布；driver 不执行 token/UID 语义。
  若 responder_mode==DISABLED：进入 passive sampler 分支，固定驱动 ready=0/resp_valid=0，不调用 get_next_item、不发布 sequence mailbox；
    仅把同一 immutable sample 送 analysis port。monitor 在该 sample 非 reset 且 req_valid==1 时立即 fatal。

sequence 只在 sequencer 的 `new_sample_event` 已由 driver synchronous analysis write 返回后触发时，通过
`wait_and_get_published_slot()` 消费一个 `L2tlb_agent_agent_transport_sample sample_ref`，再以 getter 取得
`memblock_l2tlb_drv_sample_t` 本地副本（以下为 semantic consumer flow）：
  每个分支的 terminal ack 都由该同一 response-owner sequence 写入：normal sample 完成纯软件语义后写 CONSUMED，
    reset/abort sample 写 DROPPED；任何分支都必须在可能等待 driver 的 start_item/finish_item 前完成 ack。
  若 sample.sampled_reset_active：owner 要求 sample.sampled_reset_epoch 与本 item/owner reset expectation 一致；取消/清理自己的 response token、driving、barrier 和 UID WAITING，并发布 owner-reset-done；
    调用 ack_l2tlb_transport_sample(sample.transport_sample_seq, DROPPED)；driver 已清本地 item 后唯一回 response ack；fence monitor、adapter、CSR monitor 按各自 direct-writer contract 清理并 ack；sequence 只等待结果，continue 到下一 sample。
  若 !sample_valid（包括尚未产生 post-reset global sample）：
    要求 sample 中 frozen req_fire=0 且 resp_valid=0；否则 uvm_fatal。
    只构造普通 inactive item；不能写 admission-settled、close 或 token/UID 状态；在 start_item/finish_item 前调用
      ack_l2tlb_transport_sample(sample.transport_sample_seq, CONSUMED)，再 continue 到下一 sample。
  若 sample.baseline_proof_pending=1 且 sample.previous_item.is_post_reset_baseline=1：
    driver 已在该 sample 边界调用 mark_l2tlb_post_reset_baseline_at_drv_cb()；sequence 只读取 proof，不重复写 proof。
  若 sample.baseline_required=1 且 sample.baseline_proof_pending=0（尚未发送带 tag 的 baseline）：
    要求 sample 中 frozen req_ready=0、req_fire=0 且 resp_valid=0；否则 uvm_fatal。
    构造 current reset epoch 的 `NORMAL/inactive/is_post_reset_baseline=1` item；此时不得写 done proof，也不得创建 RELEASE_STOP；
      在 start_item/finish_item 前调用 ack_l2tlb_transport_sample(sample.transport_sample_seq, CONSUMED)，再 continue 到下一 sample。
  若 sample.baseline_proof_pending=1（baseline 已发送但尚未取得更晚真实 proof）：
    只构造普通 inactive；不得重复投递第二个 baseline、不得创建 RELEASE_STOP/RELEASE_FINAL_INACTIVE；
    若 sample.dut_sample_seq-baseline_sent_sample_seq 超过 MEMBLOCK_L2TLB_BASELINE_MAX_SAMPLE_DISTANCE：uvm_fatal；
      在 start_item/finish_item 前调用 ack_l2tlb_transport_sample(sample.transport_sample_seq, CONSUMED)，再 continue 到下一 sample。
  若 sample_ready_result == NOT_READY：
    这是一条 global sample 已建立但 C-2 history/event 尚未 ready 的 warm-up sample。
    若 csr_history_published_seq > dut_sample_seq 或 lifecycle_event_published_seq > dut_sample_seq：uvm_fatal；
      producer 不能发布未来 watermark。
    若 csr_history_published_seq == dut_sample_seq 且 lifecycle_event_published_seq == dut_sample_seq：uvm_fatal；
      两个 watermark 都严格相等时 helper 必须返回 READY。
    否则至少一个 watermark 严格小于 dut_sample_seq；累计 NOT_READY watchdog，超过固定上限则 uvm_fatal。
    要求 sample 中 frozen req_fire=0 且 resp_valid=0；否则 uvm_fatal，不能静默丢失已经跨越本拍的 request/response。
    不写 l2tlb_owner_admission_settled_sample_seq；该 watermark 只允许在 `sample_ready_result==READY` 且 token/UID 语义已完整结算后写入。
    只构造普通 inactive/ready=0 item；即使 global_stop_requested=1，也延迟到下一 READY sample 创建 RELEASE_STOP；
      在 start_item/finish_item 前调用 ack_l2tlb_transport_sample(sample.transport_sample_seq, CONSUMED)。
    不读取 C-2 history、不回放 event、不处理 token/UID/adapter；continue 到下一 sample，不能带着旧采样值进入普通路径。
  若 sample.sampled_final_inactive_proof_valid=1：
    这必须是 owner 此前投递的唯一 RELEASE_FINAL_INACTIVE 的 transport sample；要求 sample_ready_result==READY、
      sampled_final_inactive_proof_epoch==current_reset_epoch、
      sampled_final_inactive_proof_transport_sample_seq==sample.transport_sample_seq、
      sampled_item_owner_name/current close_generation/item_reset_epoch 均匹配，且 frozen ready/fire/resp_valid 均为 0；否则 uvm_fatal。
    要求 l2tlb_response_drain_done()==1 且 monitor_final_sample_settled(current_reset_epoch,
      sample.transport_sample_seq)==1；monitor 的同步 analysis_imp.write() 已在 sequence 被唤醒前完成，若此处不成立是 transport 合同错误。
    调用 begin_l2tlb_release_closing(owner_name)：它只写 closing/name/generation，不检查 mailbox EMPTY、不等待 recycle、
      不发 grant、不再处理 C0/C4/token/UID。
    立即调用 ack_l2tlb_transport_sample(sample.transport_sample_seq, CONSUMED)；该 final slot 仍为 PUBLISHED，只有这一 ack
      才允许下一真实 drv_cb 的 driver 写 recycle proof 并回收为 EMPTY。不得在 ack 前等待 final/recycle/grant，不得再投递 normal 或 lifecycle item。
    wait_result = wait_for_l2tlb_release_grant_or_reset(owner_name, current_reset_epoch, close_generation)：
      若返回 RESET_PENDING，回到 loop 顶部等待 driver 发布的 reset semantic sample，由既有 reset 分支清理/re-arm；
      若返回 GRANTED，调用 release_l2tlb_lifecycle_owner(owner_name) 原子清 claim 并退出；其它返回值为 fatal。
    此 dedicated final 分支必须在普通 event/C0/C4/token 路径之前结束，不能把 final sample 当作可继续服务的 idle sample。
  读取 C-2 DTLB-side CSR history 项和 response-owner event history cursor；runtime latest 只用于 startup/debug，
         不用于 request/response key 或 UID raw-hit 判定。

  若本 lifecycle 尚未提供过首次 ready：
    若尚未完成 owner-start baseline：只对齐 last_seen event_seq/cursor；早于 current sample 的未消费 event 仅可按后续
                                  pre-ready 规则处理，不访问 live table、raw FIFO 或 pending invalidate queue。
    baseline 只执行一次；它不提前 return。C-2 CSR history 已 warm-up 后，仍由正常容量/CSR 条件产生首次 ready。

  pre_ready_baseline_batch_allowed = (acceptance_opened_since_reset == 0) &&
    (sampled_req_ready==0) && (sampled_req_fire==0) && (sampled_resp_valid==0) &&
    pending_q.empty && !driving_valid && barrier_q.empty &&
    (accept_hold_until < dut_sample_seq) && (pre_ready_hold_until_sample < dut_sample_seq) &&
    (全部 UID WAITING 数量 == 0)。
  pre_ready_baseline_batch_seen = 0；该标记只覆盖同一次 cursor 扫描中连续的 `event.sample_seq < dut_sample_seq` 启动期旧事件，
    不能跨 sample 累积，也不能用于 acceptance 已开放后的 active service。
  current_sample_event_seen = 0；该标记只表示本次扫描是否已经把 `event.sample_seq == dut_sample_seq` 按标准 C0 建 barrier，
    不能用 pre-ready baseline 或 event cursor 大小推导。
  保存 fire_visible_event_seq = response_owner_event_cursor；它代表本拍新 C0 event 之前 token 可见的最后 event。
  从 response_owner_event_cursor+1 依次检查 event history：
    要求 event_seq 连续；event.sample_seq > dut_sample_seq 为 uvm_fatal。
    若 event.sample_seq < dut_sample_seq：
      若 acceptance_opened_since_reset != 0：
        在建立 barrier、修改 token/UID 或推进 cursor 前立即 uvm_fatal，打印 event/current sample、cursor、acceptance-opened 状态、
          pending/driving/UID 状态；active responder 不允许用迟到 C0 回溯影响已经开放的 request。
      否则（startup/reset re-arm 仍未开放 ready）：
        若 pre_ready_baseline_batch_seen==0：要求 pre_ready_baseline_batch_allowed==1；否则 uvm_fatal，打印 barrier/hold/pending/driving/UID 和 frozen transport 状态。
        仅记录 pre-ready baseline 诊断并推进 response_owner_event_cursor/last_seen_flush_event_seq，置 pre_ready_baseline_batch_seen=1；
          不调用 record_l2tlb_flush_barrier()、不建立 response token/due、不中止/取消任何工作。
        继续本次 event-history 扫描；不得提前结束整个 sample，否则第二条同样旧的 event 会在下一拍被已有 hold 误判为非法。
    event.sample_seq == dut_sample_seq：
      调用 record_l2tlb_flush_barrier(event, dut_sample_seq)，以该同拍 C0 建 barrier；不得改用未来 sample 重锚。
      推进 response_owner_event_cursor，不创建第二个 adapter cursor，并置 current_sample_event_seen=1。
  若 pre_ready_baseline_batch_seen==1：
    原子设置 pre_ready_hold_until_sample=max(旧值, dut_sample_seq + MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES)，清
      ready_opportunity_since_lifecycle_block 和 idle_count；不为旧 event 建 barrier、不修改 token/UID。
    若 current_sample_event_seen==0：
      调用 retire_l2tlb_event_history_prefix(response_owner_event_cursor) 回收刚连续消费的前缀；构造普通 inactive/ready=0 item，
        在 start_item/finish_item 前 ack 当前 transport sample，然后结束本次 sample。这样多个启动期旧 event 只建立一次完整 4 拍 hold，
        不会跨拍累积 hold 或在第二条 event 上误报 fatal。
    若 current_sample_event_seen==1：该 event 已按上一分支建立标准 C0/C4 barrier；继续本拍正常 C0-C4 流程，
      最后的统一 retire 在全部 response/token/UID 语义结算后执行。旧 event 仍只贡献一次 pre-ready hold，不能吞掉 current C0。
  dispatch-active 时，`service_monitor_once()` 在 `collect_runtime_context_events()`（只同步 CSR）之后，
    每个 dispatch sample 恰好调用一次 `dispatch_monitor_event_adapter::service_l2tlb_sfence_events()`。
    该入口对 raw_sfence_q 执行 peek -> decode -> schedule -> pop，并按 raw 自己的 C0 sample 登记 C4 live-entry delete。
    L2TLB sequence 不读取、pop、schedule 或 apply raw fence；no-dispatch 时 raw fence 根本不入 FIFO。
    旧 `drain_sfence_events()` 直接 pop 并调用 `apply_raw_sfence()` 的路径必须删除/废弃，不能与新入口并存。
  先调用 apply_due_l2tlb_flush_barriers(dut_sample_seq)：
    对每个 due <= dut_sample_seq 的 barrier，扫描 pending_q；
    仅删除 accept_flush_event_seq < barrier.event_seq 的 token；
    每个删除 token 只增加一次 flush_canceled_count，不回填 UID record；
    在本 barrier 的 pending token 已处理后，调用 cancel_waiting_uid_records_for_flush(barrier)，
    只取消 first_request_fire_sample <= barrier.anchor_sample_seq 的仍未完成 UID waiting instance；
    marker=0 或晚于 anchor 的 WAITING UID 保持等待；
    删除已消费 barrier；若仍有 barrier，继续 hold。

  若 driving_valid：
    若本拍存在 due_barrier_this_sample：uvm_fatal；selector 不得让 response 在
      filter flush 同拍 fire。
    否则取得 response_filter_csr_snapshot = get_l2tlb_request_csr_snapshot(dut_sample_seq)；无效则 uvm_fatal，不能以
      runtime latest 或 token/UID 冻结 CSR 替代；调用 complete_driving_response(snapshot)，token 正常 complete，UID 是否完成
      按该 snapshot 的 raw hit 决定。

  L2TLB monitor 消费 passive driver 的 analysis sample 检查 no-owner 边界：在非 reset sample 且完成 X/Z 诊断后，若
    responder_mode==DISABLED 且 sampled_req_valid==1，立即 uvm_fatal；不能等待 req_valid && req_ready，因为 passive driver 的 ready 固定为 0。
  根据 active owner 本拍锁存的 valid && ready 判断 request fire：
    若 !dispatch_l2tlb_lookup_active：该组合不应到达 active owner；uvm_fatal，当前支持矩阵中 ENABLED+NO_DISPATCH 为启动前 fatal，
    不建立 token、pending snapshot 或 live entry。
    即使本拍刚观察到 flush event，真实 fire 仍调用 capture_fired_request() 正常建 token；
    token 的 accept_flush_event_seq 固定为 fire_visible_event_seq，不能写成本拍新 event_seq；
    capture 必须取 C-2 DTLB-side CSR history 建 key/build payload；history 无效时 fire 仍按既有合同 uvm_fatal。

  完成本拍的 response、flush、request fire capture 和 UID marker/registration 后：
    调用 retire_l2tlb_event_history_prefix(response_owner_event_cursor)；只有连续已消费的 event 前缀被回收后，producer 才能
      在后续 sample 使用释放的 history 容量；该调用不清 barrier、不改 token/UID 状态。
    owner 原子写 l2tlb_owner_admission_settled_sample_seq=dut_sample_seq；同一 sample 只允许写一次，重复或倒退均 uvm_fatal。
    若 global_stop_requested=1、reset_active=0 且 close_requested=0：调用 close_l2tlb_admission_for_release(owner, dut_sample_seq)，
      该 helper 只允许读取刚写入的 watermark，并为本次 reset epoch 分配 close_generation。
      当前 cycle_tr 随后必须被标记为 RELEASE_STOP、ready=0；不能先送出 normal ready=1 item 再写 seal。
    若 close_requested=1：后续代码不得再次执行 capture_fired_request()/UID registration；若出现代表 owner 内部顺序错误，uvm_fatal。

  当任一 barrier 未到期、本拍刚观察 event 或 pre-ready hold 尚未到期：
    下一 cycle item 强制 ready=0；`dut_sample_seq <= max(accept_hold_until, pre_ready_hold_until_sample)` 时不得生成 ready=1；
    只要 due_barrier_this_sample=0，仍按既有 latency 选择并驱动旧 pending response；
    因此 barrier 只关闭新 request admission，C1-C3 不得把 resp_valid 一并错误清零。
  当本拍处理过 due barrier 或 pre-ready hold：
    下一 cycle item resp_valid=0；只有 barrier_q 已空时才可按容量、CSR、stop 重新计算 ready。
    dispatch adapter 独立在同一 due sample 调用 apply_due_sfence_invalidate() 删除 live entry；
    两者只共享 frozen anchor/due，不扫描或修改对方队列，也不依赖 UVM service 调用先后。

  构造下一 cycle item 时：
    若 lifecycle hold 尚未结束、global stop 已请求或 baseline/sample-ready 未完成：保持 next_ready=0，ready_opportunity_since_lifecycle_block 保持 0。
    否则按既有容量/CSR 条件计算 next_ready；只有真正生成 next_ready=1 的 item 时，若 acceptance_opened_since_reset==0 则置 1，
      并置 ready_opportunity_since_lifecycle_block=1。
    ready_opportunity_since_lifecycle_block==0 时 idle_count 不得累计；置 1 后才允许沿用既有 idle-stop 诊断计数。

  若 stopping/global stop 已请求：
    下一 cycle item 保持 ready=0；但继续 service C1-C3 的合法旧 response 与 C4 token/UID barrier 工作。
    只有 lifecycle_release_safe=1 时才设置 should_exit；不得因 pending_q 已空而绕过 barrier 或 UID 等待实例提前退出。

  正常 READY sample 的 response、flush、request fire、UID 和 close 语义全部结算后，在构造并通过 start_item/finish_item
    发送下一 cycle item 前，调用 ack_l2tlb_transport_sample(sample.transport_sample_seq, CONSUMED)。该 ack 只释放
    mailbox slot，不表示下一 item 已被 driver 采样，也不确认 admission/final。

自然退出 service loop 前：
  response owner 先发布 response_drain_done，并保持 claim/驱动 inactive，等待 parent 的 l2tlb_release_granted。
  只有 pending_q/barrier_q 已收敛、driving_req 无效且没有 WAITING UID 时才允许最终 release；
  不得让 connect takeover active 代替该收敛检查。raw FIFO 与 pending invalidate queue 由 adapter 独立收敛。
```

这一定义了 C0-C4 的明确行为：

| sample | 框架行为 |
|---|---|
| C0 | 记录 barrier，下一 item 关闭 ready。C0 已 fire 的 request 正常 capture。可选择一个旧 response 驱动至 C1。 |
| C1 | ready=0，不再接受新 request；已存在 token 可以正常完成 response。 |
| C2-C3 | ready=0，不再接受新 request；已存在 token 可以正常完成 response，但 UID multicast 必须用该 response sample 的 C-2 filter CSR 重放 raw hit，不能用 UID 的旧 CSR。 |
| C4 | 不允许任何旧 response 在本拍 external fire；L2TLB token/UID owner 取消仍在 `pending_q` 的旧 token，以及 marker 不为 0 且不晚于 barrier anchor 的 WAITING UID instance。dispatch-active 时，adapter 独立删除命中的 live entry。 |
| C5 | 没有后续 barrier 时，生成 ready=1 的下一 cycle item；该 ready 在 C6 才可能形成新的 request fire，且使用 fence 后 live entry。 |

## 4. 关键 helper 修改

### 4.0 `publish_l2tlb_csr_history()` 与 `get_l2tlb_request_csr_snapshot()`

**抽象功能描述：** 前者由 CSR monitor 在每个 post-reset DUT sample 写入有限深度的顶层 CSR 历史；后者由
L2TLB lifecycle owner 在 request fire 或 response fire 时读取与 `PTWNewFilter` 当前输入对齐的历史项。两者
只解决 filter CSR 上下文的时间对齐，不发布 flush、不删除 entry、也不替代 runtime latest CSR。

```text
publish_l2tlb_csr_history(raw_csr, dut_sample_seq):
  要求 raw_csr.valid 且 dut_sample_seq 为全局单调连续序号；否则 uvm_fatal。
  深拷贝完整 raw_csr、dut_sample_seq 到固定环形 history[3]。
  即使 payload 与上一拍相同也必须写入；不得只在 CSR changed 时写入。

get_l2tlb_request_csr_snapshot(dut_sample_seq):
  target_sample = dut_sample_seq - MEMBLOCK_DUT_L2TLB_CSR_PIPE_STAGES  // V2 固定 2
  在 history 中查找 sample_seq == target_sample 的唯一项。
  若找不到、目标项无效或出现多个同 sample 项：返回 invalid。
  找到时深拷贝为 mmu_csr_runtime_state 并返回。
```

中文文字伪代码：`MemBlock.scala` 的 `tlbcsr = RegNext(RegNext(io.ooo_to_mem.tlbCsr))` 使顶层 C0 的 CSR
在 C2 才成为 `PTWNewFilter` 的输入。因而在 responder 当前 sample C 接受 `PtwReq` 时，ASID/VMID、mode 和
root 只能取 C-2；同样地，response 在 C fire 时，当前 `PTWNewFilter` 的 `PTWFilterEntry` 也是用 C-2 的 CSR 判断它命中
哪些 filter entry。拿 C 的 runtime latest 会在紧邻 CSR change 的 C0-C3 窗口把旧 request 误建成新 address-space
payload，或把 DUT 未接收的 response 错回填 UID。history 只保留 3 项，既覆盖两拍延迟也不会成为无界队列。
首次 ready 必须等到 C-2 项存在；正常运行中 request/response fire 却取不到项属于 monitor/service 时序失配，
不能静默退回 latest。这里 C、C-2 都是 DUT global sample；owner 切换后的局部 `sample_seq=0/1/2` 不能当作
top-side CSR history 的 C-2 坐标。

### 4.1 `record_l2tlb_flush_barrier()`

**抽象功能描述：** 将已经通过 freshness 检查的 monitor event 转换为响应端的延迟 barrier，使 ready 在实际
filter flush 前保持关闭；它不执行 token cancel。

```text
record_l2tlb_flush_barrier(event, current_dut_sample_seq):
  要求 event.event_seq 大于 last_seen_flush_event_seq，且 reason_mask 非 0、只包含 CSR_CHANGE/FENCE，否则 uvm_fatal。
  要求 event.sample_seq == current_dut_sample_seq；future event 不能被提前回放，迟到 event 也不能由 active responder 补回放，均必须 uvm_fatal。
  创建 barrier，due = event.sample_seq + MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES；延迟消费不得用当前 sample 重锚。
  barrier 保存 event_seq/reason_mask/anchor/due；push barrier_q。
  更新 last_seen_flush_event_seq 与 accept_hold_until=max(旧值, due)。
  清 ready_opportunity_since_lifecycle_block 和 idle_count；不得清 acceptance_opened_since_reset，后者继续承担 active event freshness。
  不扫描 pending_q；不修改 accepted/completed/flush_canceled_count；不读取 sfence.bits.flushPipe。
```

同 sample 的 `CSR_CHANGE` 与 `FENCE` 在调用本 helper 前已合成为同一个 `reason_mask` record，因此只调用一次。
不同 sample 的 record 才分别调用并建立各自 barrier。

### 4.1.1 `retire_l2tlb_event_history_prefix()`

**抽象功能描述：** `retire_l2tlb_event_history_prefix()` 由唯一 response owner 在本拍 event、token/UID 和 response
语义全部结算后，回收已经由 `response_owner_event_cursor` 连续消费的 history 前缀。它只释放已消费记录的存储，不改变
event 序号、barrier、raw fence 或 adapter 状态；CSR/fence producer 不调用该回收接口。

```text
retire_l2tlb_event_history_prefix(response_owner_event_cursor):
  要求 cursor 不倒退且不超过 last_allocated_l2tlb_event_seq，否则 uvm_fatal。
  while history 非空 && history.front.event_seq <= response_owner_event_cursor：
    要求 front.event_seq 连续且不大于 cursor；否则 uvm_fatal。
    history.pop_front()；记录 retired_event_seq；不修改 last_allocated_l2tlb_event_seq。
  若 history 仍满：
    要求 history.front.event_seq > response_owner_event_cursor；否则 uvm_fatal（已消费前缀不应残留）。
    返回 FULL_NO_RETIRED_PREFIX；下一次 producer 需要追加新 event 时必须 uvm_fatal，producer 不得代替 owner pop 或覆盖记录。
  否则返回当前 history 深度和 retired_event_seq。
```

中文文字伪代码：response owner 先用 cursor 完成本拍 event 的消费和 token/UID 处理，再从队首连续删除所有
已处理的 event；删除只影响队列占用，不会让序号重新从 0 开始。若队首序号大于 cursor，说明仍有未消费事件，不能删除；
若队列满且没有可回收前缀，直接报告容量错误，不能静默覆盖。该 helper 的唯一写者是 response owner，producer 只负责
同拍合并或追加 event。

### 4.2 `apply_due_l2tlb_flush_barriers()`

**抽象功能描述：** 在 DTLB filter 已到达实际 flush 边界时释放框架账本中仍存活的旧 request，避免 responder
继续回复 DUT 已清除的 filter entry；它处理 `pending_q`，并在 token 删除后取消仍未完成的 UID waiting instance。
`driving_req` 必须已为空，因为 selector 不允许 due sample fire。UID cancel 只处理 responder 已实际观察到
request fire 的等待实例，不能用 issue sample 代替 DTLB request 生命周期。

```text
apply_due_l2tlb_flush_barriers(dut_sample_seq):
  caller先计算 due_barrier_this_sample，确保本拍不会在删除前选择新的 response。
  对 barrier_q 中每个 due <= dut_sample_seq 的 barrier：
    从 pending_q 尾到头检查 token.accept_flush_event_seq < barrier.event_seq。
    每个命中 token 从 pending_q 删除，flush_canceled_count 加一，并记录 token/event/anchor/due。
    不调用 update_uid_tlb_records_by_entry()；不回退 accepted_count；不触碰 driving_req。
    调用 cancel_waiting_uid_records_for_flush(barrier)：
      它只取消仍为 WAITING、first_request_fire_sample 非 0 且不晚于 barrier.anchor_sample 的 UID instance；
      marker=0 或晚于 anchor 的 record 保持 WAITING，COMPLETED record 保持不变。
    移除该 barrier。
  调用 check_l2tlb_lifecycle_accounting("due_flush")。
  若 barrier_q 非空：继续保持 ready=0。
```

从尾到头删除复用当前 queue 的局部操作，不需要建立第二份 token map；`DFILTER_SIZE=32` 是明确上界。每个 token
只能由第一个满足条件的 barrier 删除，删除后不再位于 queue，后续 barrier 不会重复计数。

### 4.3 `capture_fired_request()` 的 event 归属与 CSR 对齐修正

**抽象功能描述：** 对已经在 sample 边界发生的 request fire 冻结与 DTLB filter 对齐的 CSR/key/entry snapshot。
flush event 即使同拍到达，也不能倒写该 fire 的事件归属或把 runtime latest 错当成 request CSR。

```text
capture_fired_request(..., fire_visible_event_seq, dut_sample_seq):
  若 close_requested=1：
    uvm_fatal；close request 写入前已经完成的 fire 不回溯处理，但该 flag 写入后的任何本 helper 调用都不允许
    token allocation，不能仅用 sample 大小比较放过同 sample 的晚到调用。
  若 admission_closed=1：
    uvm_fatal；transport cutoff 后不得新建 token。
  fire_visible_event_seq 必须是本拍读取新 flush event 前保存的 last_seen_flush_event_seq。
  csr_snapshot = get_l2tlb_request_csr_snapshot(dut_sample_seq)  // 固定为 top CSR 的 C-2 项
  若 csr_snapshot 无效：uvm_fatal；不得回退到 get_mmu_csr_snapshot()/runtime latest。
  按既有逻辑创建 token、entry snapshot 和 response payload，并只用 csr_snapshot 建 lookup key。
  token.accept_flush_event_seq = fire_visible_event_seq。
  token.request_fire_sample_seq = dut_sample_seq。
  调用 mark_waiting_uid_records_on_request_fire(token)：该 helper 查询有限的 {vpn,s2xlate} WAITING 候选，
    用本次 csr_snapshot 构造 key 后写入首个 request-fire marker；它不把 token 绑定到单 UID。
  push pending_q，accepted_count 加一并检查账本。
  不调用 record_flush_killed_request()；该 helper 从本 flow 删除。
```

旧 `record_flush_killed_request()` 删除：它把 C0 的真实 fire 当作同拍立即被 filter kill，与 V2 C4 flush
合同冲突。未发生 `valid && ready` 的 request 不建 token；已 fire 的 request 则只能在 response complete、due flush
cancel 或 reset cancel 之一完成分类，禁止 silent drop。

### 4.4 `select_due_response()` 的 barrier 可见性修正

**抽象功能描述：** `select_due_response()` 在选择下一拍要驱动的 response 时，判断 token 是否还能在某个已知
flush barrier 真正到期**严格之前**完成。它不以“token 的 accept event 是否等于 latest event”作为陈旧判据；
C0 前已接受的 token 可在 C1-C3 返回，但不得在 C4 filter flush 同拍 fire。

现有 `pending.accept_flush_event_seq != last_seen_flush_event_seq` 的直接 fatal 必须删除。该比较会在 C0 记录 barrier
后把所有 C0 前 token，包括 C0 同拍真实 fire 的 token，都错误视为不能在 C1-C3 返回，违背本 plan 的 C4 合同。

```text
token_can_complete_before_due(token, completion_dut_sample_seq):
  对 barrier_q 中每个 barrier：
    若 token.accept_flush_event_seq < barrier.event_seq
       且 barrier.due_filter_flush_sample <= completion_dut_sample_seq：
        return 0  // C4 同拍 fire 会被 filter flush 覆盖，不能作为有效 completion。
  return 1

select_due_response(next_dut_sample_seq):
  保留既有 latency/reorder 的 eligible token 选择。
  对候选 token 调用 token_can_complete_before_due(token, next_dut_sample_seq)。
  不可完成的 token 不得选择；若有其它 eligible token，继续按既有顺序/随机规则选择。
  若 next_sample 就是某 barrier 的 due，禁止选择；不得把 item 放入一个会在 C4 external fire 的 driving slot。
```

中文文字伪代码：flush epoch 只标记 token 是否属于某次未来取消范围，不能在 barrier 尚未到期时立即阻断 response。
因此 selector 只检查“是否存在早于或等于本次 external fire sample 的 due barrier”，而不比较全局最新 event
序号。C0 event 后的旧 token 可在 C1-C3 正常进入 driving slot；C4 不允许有 driving slot，随后取消仍位于
`pending_q` 的旧 token。这既保持每个真实 fire 的 token 守恒，也避免把同拍被 `when(flush)` 清掉的 response
错误记为 UID completion。

### 4.5 `complete_driving_response()` 的 response-visible CSR 修正

**抽象功能描述：** `complete_driving_response()` 在 external response 已跨越当前 sample 的 DUT 边界后完成该
token 的响应账本，并把同一份 raw response 交给 UID multicast helper。它不重新建 entry、不修改 token 的
request-time CSR，也不以 UID issue-time CSR 猜测 filter 是否命中。

```text
complete_driving_response(response_filter_csr_snapshot):
  要求当前 sample 不是任何旧 token 的 barrier due；否则由主循环在调用前 fatal。
  要求 response_filter_csr_snapshot == top CSR 的 current_sample-2 history 项；无效则 uvm_fatal。
  外部 response fire 已成立：completed_count 加一、清 driving_req。
  调用 complete_waiting_uid_records_by_response(driving_req,
                                                  response_filter_csr_snapshot)。
  helper 返回 0/1/多个 UID completion 都合法。
  不因 UID 命中数为 0 将已 fire token 改记为 canceled；当前 PTWNewFilter entry 可能因 CSR 已变化而不匹配，
  此时 UID 保持 WAITING，并由既有 C4 barrier cancel 或后续真正命中的 response 处理。
```

中文文字伪代码：C0 CSR change 后，旧 token 若在 C2/C3 external fire，agent 仍完成一次真实 L2TLB
response；但 filter 的 raw hit 已使用新的 C-2 CSR。若旧 raw tag 因新 ASID/VMID 不命中，DUT 不产生该 UID 的
filter completion，框架也只能让该 UID 保持 `WAITING`，不能按它创建时冻结的旧 key 标为 `COMPLETED`。
这不是把 selector 的 deadline 从 C4 缩到 C2：token 输出是否 fire 仍按真实接口和 C4 严格截止决定；仅 UID
回填必须复刻 response fire 当拍的 DUT matcher。

### 4.6 `release_l2tlb_lifecycle_owner()` / `do_kill()` 不能绕过收敛 gate

**抽象功能描述：** 该 release flow 只在当前 owner 已经没有 token、barrier 或 UID waiting instance 时
释放动态 claim。它防止 testcase 结束后仍遗留无归属的 response 生命周期账本；它不把 connect takeover capability
清零，也不拥有 raw fence、live entry 或 adapter 的 C4 删除工作。

**`begin_l2tlb_release_closing()` 抽象功能描述：** 该 package helper 在当前 owner 完成正常 stop/drain、且 driver 已在真实
`drv_cb` 边界完成 final inactive item、monitor 已同步确认该 epoch/sample settled 后，
验证此前已经关闭的 admission 边界并记录 closing。它不关闭 admission、不执行 `try_release`、不删除 token、UID、barrier 或 live entry。

**`close_l2tlb_admission_for_release()` 抽象功能描述：** 该 owner helper 在 parent 已置 `global_stop_requested` 后，
只在自己刚完成当前真实 `drv_cb` sample 的 request capture/UID registration 时登记 close request，并要求本拍生成的下一 cycle item
发送 `ready=0` 停止 admission。这样 parent 的 negedge stop 意图不会追溯取消已被 driver 驱动的 ready 窗口；close request
写入后立即封闭所有后续 dispatch/UID issue helper 调用。它不等待 drain、不发送 final inactive、
不发 grant，也不立即宣布 responder transport 已关闭；后者只能由 driver 在真实 `drv_cb` 冻结该 `ready=0` item 后确认。

**`confirm_l2tlb_admission_closed_at_drv_cb()` 抽象功能描述：** 该 package helper 只由 L2TLB driver 在真实 `drv_cb` 调用；driver
冻结 stop item 的
`RELEASE_STOP` metadata、`req_ready=0` 且本拍没有 request fire 后，确认 transport admission 已关闭并冻结 cutoff sample。
owner 不调用它，也不清 token/UID、不发 grant；它只为后续 drain、final inactive 和 release 提供不可回退的关闭边界。

driver 调用该 helper 前必须先做本地 metadata prefilter：若该 sample 的 `sampled_reset_active=1`，或上一拍 metadata latch 的
`sampled_item_reset_epoch != sampled_reset_epoch/expected_reset_epoch`，则清除该**已采样的上一拍 latch**、驱动 inactive 并直接返回；不能把旧
sample 交给 helper 触发新 epoch 的 fatal/确认。该上一拍 item 已在正常 send 路径完成 `item_done()`，此处不得第二次释放它。
尚未发送、但已经由 `try_next_item()` 返回的 stale item 另按下方 `discard_stale_l2tlb_item()` 精确 `item_done()` 一次。

**`mark_l2tlb_final_inactive_at_drv_cb()` 抽象功能描述：** 该 driver helper 只确认已经被 owner 明确标记为
`RELEASE_FINAL_INACTIVE` 的独立 item 已经跨过真实接口采样边界。它不会把 flush hold、普通 idle 或允许携带旧 response 的
`RELEASE_STOP` 当作 final inactive，也不清 claim/grant。
driver 在调用前同样必须依据该 sample 的 `sampled_reset_active` 或 `item_reset_epoch != sampled_reset_epoch` 忽略**已采样 metadata latch**并驱动 inactive；
该 item 已沿正常 send 路径完成 `item_done()`，不得二次释放。只有 current epoch item 才进入 final-inactive 校验；
尚未发送的 fetched stale item 仍由 `discard_stale_l2tlb_item()` 完成唯一的 `item_done()`。

```text
close_l2tlb_admission_for_release(owner_name, current_sample):
  要求 current_sample==peek_current_dut_global_sample() 且 current_sample!=0；否则 uvm_fatal。
  若 responder_mode == DISABLED：要求 claim=0，保持 admission_closed=0 并返回；不得制造 closing/grant。
  要求 global_stop_requested=1、!reset_active、owner_name 是当前 claimed owner；否则 uvm_fatal。
  若 current_reset_epoch != 0：要求 l2tlb_post_reset_baseline_done_epoch==current_reset_epoch；否则 uvm_fatal。
    epoch 0 是 testcase-start 基线，不额外引入 runtime-reset baseline gate；非零 epoch 的 baseline proof 只由 driver 在当前 epoch 的
    NORMAL/inactive item 真正采样到 fire=0/resp_valid=0 后写入，owner 不得用 sequence finish 或 reset release 代替。
  要求 l2tlb_owner_admission_settled_sample_seq==current_sample；该字段只由本 owner 在 request fire capture/UID marker
    全部完成后写入。parent 不得调用本 helper，也不得以 negedge service 顺序替代该检查。
  若 close_requested 已为 1：
    若 request_owner_name != owner_name 或 close_request_reset_epoch!=current_reset_epoch：uvm_fatal。
    返回已存在的 close_generation；不得二次分配 generation 或重复投递 stop item。
  close_generation++；若回绕为 0：uvm_fatal。
  原子写 close_requested=1、request_owner_name=owner_name、close_request_sample_seq=current_sample、
    close_request_reset_epoch=current_reset_epoch、current close_generation。
  当前 sample 已经完成的 admission 不回溯判错；从该写入动作之后的任意 capture/register 调用均 fatal。
  owner 在下一 cycle item 驱动 req_ready=0、item_kind=RELEASE_STOP、item_generation=close_generation、
    item_reset_epoch=current_reset_epoch；
    item 可携带一笔已存在的旧 response，本 helper 不写 admission_closed/cutoff_sample_seq。
  已存在 token/UID 仍由正常 drain/cancel 完成，不能在此 helper 中直接清表。
```

```text
confirm_l2tlb_admission_closed_at_drv_cb(owner_name, dut_sample_seq,
                                         sampled_reset_active, sampled_reset_epoch,
                                         expected_reset_epoch,
                                         sampled_item_owner_name, sampled_item_kind, sampled_item_generation, sampled_item_reset_epoch,
                                         sampled_req_fire, sampled_req_ready):
  调用者必须是 L2tlb_agent_agent_driver 的真实 drv_cb 主循环；否则 uvm_fatal。
  driver 在调用前若 sampled_reset_active=1、sampled_item_owner_name 为空、sampled_item_reset_epoch!=sampled_reset_epoch 或
  sampled_reset_epoch!=expected_reset_epoch：丢弃已采样 metadata latch、驱动 inactive 并 return；
    该 item 已在正常 send 路径完成 item_done，runtime reset/旧 epoch 期间不得进入本 helper、不得二次 item_done，也不得确认 close。
  若 sampled_item_kind != RELEASE_STOP：return；普通 idle/flush-hold ready=0 item 不得确认 admission close。
  若 sampled_item_owner_name != owner_name：uvm_fatal；stop item 的 owner provenance 必须来自同一 xaction/sample，不能用当前 owner 修补。
  若 !close_requested || request_owner_name!=owner_name || close_request_reset_epoch!=expected_reset_epoch：
    uvm_fatal；非 reset 状态下旧或伪造 stop item 不得被静默忽略。
  若 sampled_item_generation != close_generation：uvm_fatal；旧 stop item 不能确认当前 close。
  若 sampled_item_reset_epoch != expected_reset_epoch：uvm_fatal；reset 前 stop item 不能确认当前 close。
  若 dut_sample_seq <= close_request_sample_seq：uvm_fatal；stop 必须在 close request 之后的真实 sample 被采样。
  若 sampled_req_ready != 0：uvm_fatal；标记为 RELEASE_STOP 的 item 必须实际驱动 ready=0。
  sampled_req_fire 必须是 driver 从该真实 drv_cb 边界冻结的 `req_valid && req_ready` 结果，不能由
    `sampled_req_valid` 在 helper 内重新推导，也不能读取下一拍接口值。
  若 sampled_req_fire != 0：uvm_fatal；ready=0 sample 不得形成新的 request fire。
  若 admission_closed 已为 1：
    uvm_fatal；同一 close generation 只能有一个真实 RELEASE_STOP sample，不得幂等接受第二个 stop item。
  原子写 admission_closed=1、admission_owner_name=owner_name、admission_closed_generation=close_generation、cutoff_sample_seq=dut_sample_seq。
  此后任何 request fire 或 capture_fired_request() 新 token 分支均为 uvm_fatal；仅 req_valid 可以保持为 1。
```

`sampled_req_fire` 的来源必须在 driver 的 clocking-block 采样边界明确冻结：它表示该边界 DUT 实际看到的
`req_valid && req_ready`，不是 sequence item 是否已经 `finish_item()`，也不是 owner 计划驱动的 ready 值。
如果 driver 无法提供该冻结结果，coding 必须先补齐 driver 的采样接口，不能把 `sampled_req_valid && sampled_req_ready`
作为替代条件。

**`get_owned_item_or_abort()` 非阻塞 service 规则：** 本 helper 只能在每个真实 `drv_cb` 已完成 mailbox recycle、
VIF freeze 与 reset snapshot 后调用一次。它不再把 `get_next_item()` 与 abort thread 并发等待；否则 reset 时 driver
可能永久卡在 sequencer 而不能发布 reset-active sample、回收 final mailbox 或推动 MONITOR ack。

```text
get_owned_item_or_abort(phase, output req, output got_item, output aborted):
  req=null；got_item=0；aborted=0；item_owned_by_driver=0。
  若 phase 已进入 ENDED/JUMPING/CLEANUP/DONE：
    aborted=1；返回；明确未取得 item，不调用 item_done()。
  调用 seq_item_port.try_next_item(req) 一次；该调用不跨 clock 等待。
  若 req==null：
    返回 NO_ITEM；调用者本拍继续 drive_idle、发布 frozen transport sample，并在下一 drv_cb 再轮询。
  否则：
    item_owned_by_driver=1；got_item=1；返回。
  reset_active 不是 aborted：driver 仍需取得已经可见的 item 后按 stale 规则 item_done，或在 NO_ITEM 时发布 reset-active sample。
```

`item_owned_by_driver=1` 仍是 UVM transport 所有权的唯一判据。driver 对该 item 的正常、stale、reset 或 phase-end 分支都
恰好一次 `seq_item_port.item_done()`；已发送并完成 `item_done()` 的上一拍 metadata latch 不属于 owned item，清 latch
不得再次调用 `item_done()`。NO_ITEM 不再是 fatal，也不能让 driver 停止时钟 service；它只表示本拍没有新的 sequence 输出。

**`discard_stale_l2tlb_item()` 抽象功能描述：** 该 driver-local task 用于终结已经从 sequencer 取得、但因 reset 或 epoch
失效而绝不能再驱动到 DUT 的 transaction。它只完成 UVM transport 释放和本地 inactive 驱动；不确认 stop/final、不修改
token/UID/barrier，也不替 adapter 清 raw/live 状态。

```text
driver 在每个 drv_cb 完成 mailbox recycle 与 VIF 冻结后轮询 item：
  try_next_item(req) 成功后，记 item_owned_by_driver=1；若没有 item，驱动 inactive、发布本拍 sample 并进入下一 drv_cb。
  若 reset_active 或 req.item_reset_epoch != current_reset_epoch：
    discard_stale_l2tlb_item(req, reason)
      -> drive_idle()
      -> seq_item_port.item_done()             // 仅这一次释放该 get_next_item item
      -> req = null；item_owned_by_driver=0
      -> 不调用 stop/final confirm；continue 到下一 drv_cb。
  若 req 是 current epoch 的 RELEASE_STOP/RELEASE_FINAL_INACTIVE 且 post_reset_baseline_done_epoch != current_reset_epoch：
    uvm_fatal；当前 epoch sequence 在 baseline 前错误地产生 release item。
  若 req 是 current epoch 的 NORMAL/inactive baseline 且 req.is_post_reset_baseline=1 且 post_reset_baseline_pending=1：
    正常 drive；记录 baseline_sent_sample_seq=当前 driver 的 `dut_sample_seq`；在严格更晚且 `sample_valid=1` 的真实 drv_cb
      以 frozen ready=0/fire=0/resp_valid=0 写 baseline done proof；若 dut_sample_seq-baseline_sent_sample_seq
      超过 MEMBLOCK_L2TLB_BASELINE_MAX_SAMPLE_DISTANCE：uvm_fatal；
    对该正常 item 调用一次 seq_item_port.item_done()，并清 item_owned_by_driver。
  若 req.is_post_reset_baseline=1 但 item_kind != NORMAL 或 req 的 reset epoch 不匹配：
    uvm_fatal；baseline metadata 不能挂在 stop/final 或旧 epoch item 上。
其它 current epoch item：
    按既有 send_pkt() 路径正常 drive；该路径调用一次 seq_item_port.item_done()，并清 item_owned_by_driver。

任何分支都不得对同一 req 调用两次 item_done()；只有 try_next_item 实际返回 req 后才能调用 item_done()。
若 get_owned_item_or_abort() 返回 NO_ITEM 或 phase abort 且未取得 req，不调用 item_done()。
`item_done()` 只释放 sequencer 对象，不清 driver 的上一拍 metadata latch；driver 必须保留已驱动 item 的
kind/generation/reset_epoch 轻量快照，供下一真实 drv_cb 冻结 fire/ready/resp_valid 后确认 baseline、stop 或 final。
```

**`mark_l2tlb_post_reset_baseline_at_drv_cb()` 抽象功能描述：** 该 driver helper 只在 runtime reset release 后，
把一笔 current epoch 的 `NORMAL/inactive` item 已跨过真实接口采样边界这一事实发布给 owner。它不处理 token、UID、
close 或 release；owner 仅把它作为允许下一拍创建 stop item 的只读前置条件。

```text
mark_l2tlb_post_reset_baseline_at_drv_cb(dut_sample_seq,
                                         sampled_reset_active, sampled_reset_epoch,
                                         expected_reset_epoch,
                                         sampled_item_kind, sampled_item_reset_epoch, sampled_is_post_reset_baseline,
                                         baseline_sent_sample_seq,
                                         sampled_req_ready, sampled_req_fire, sampled_resp_valid):
  调用者必须是 L2TLB driver 的真实 drv_cb 主循环；否则 uvm_fatal。
  若 !post_reset_baseline_pending：return。
  若 sampled_reset_active：return；保持 pending，等待 reset release 后的新 sample。
  若 sampled_item_kind != NORMAL 或 !sampled_is_post_reset_baseline 或 sampled_item_reset_epoch != sampled_reset_epoch：uvm_fatal。
  若 sampled_reset_epoch != expected_reset_epoch：return；保持 pending，不能用旧 epoch sample 写 proof。
  若 dut_sample_seq <= baseline_sent_sample_seq：return；保持 pending，必须等待发送边界之后的真实 sample。
  若 dut_sample_seq-baseline_sent_sample_seq > MEMBLOCK_L2TLB_BASELINE_MAX_SAMPLE_DISTANCE：uvm_fatal；
  若 dut_sample_seq==0 或 sampled_req_ready!=0 或 sampled_req_fire!=0 或 sampled_resp_valid!=0：uvm_fatal。
  原子写 l2tlb_post_reset_baseline_done_epoch=sampled_reset_epoch、l2tlb_post_reset_baseline_sample_seq=dut_sample_seq；
  清 driver-local post_reset_baseline_pending。
  close_l2tlb_admission_for_release() 只读上述 epoch proof；本 helper 不创建 RELEASE_STOP。
```

```text
mark_l2tlb_final_inactive_at_drv_cb(owner_name, dut_sample_seq,
                                    sampled_reset_active, sampled_reset_epoch,
                                    expected_reset_epoch,
                                    transport_sample_seq,
                                    sampled_item_owner_name, sampled_item_kind, sampled_item_generation, sampled_item_reset_epoch,
                                    sampled_req_ready, sampled_req_fire, sampled_resp_valid):
  driver 在调用前若 sampled_reset_active=1、sampled_item_owner_name 为空、sampled_item_reset_epoch!=sampled_reset_epoch 或
    sampled_reset_epoch!=expected_reset_epoch：丢弃已采样 metadata latch、驱动 inactive 并 return；
    该 item 已在正常 send 路径完成 item_done，runtime reset/旧 epoch 期间任何 metadata 都不得确认 final inactive。
  若 sampled_item_kind != RELEASE_FINAL_INACTIVE：return。
  若 sampled_item_owner_name != owner_name：uvm_fatal；final item 必须携带并冻结生成它的 owner provenance，不能只比较 package 当前 owner。
  要求 close_requested=1、close_request_reset_epoch==expected_reset_epoch、admission_closed=1、
    admission_closed_generation==close_generation、request/admission owner 均为 owner_name；否则 uvm_fatal。
  若 sampled_item_generation != close_generation：uvm_fatal。
  若 sampled_item_reset_epoch != expected_reset_epoch：uvm_fatal；reset 前 final item 不能完成新 epoch release。
  若 dut_sample_seq <= cutoff_sample_seq：uvm_fatal；final inactive 不能与 stop item 混用同一采样边界。
  sampled_req_fire 必须是 driver 从同一真实 drv_cb 冻结的 `req_valid && req_ready` 结果；不能由 helper 重新推导，
    也不能读取下一拍值。
  若 sampled_req_ready != 0 或 sampled_req_fire != 0 或 sampled_resp_valid != 0：uvm_fatal；final inactive 必须为
    ready=0/fire=0/resp_valid=0。
  若 final_inactive_done=1：uvm_fatal；同一 close generation 只能有一个真实 RELEASE_FINAL_INACTIVE sample。
  原子写 final_inactive_done=1、final_inactive_generation=close_generation、final_inactive_transport_sample_seq=transport_sample_seq；
  release gate 必须确认 `monitor_final_sample_settled(expected_reset_epoch, transport_sample_seq)=1` 后才允许 grant；
    该检查不参与 runtime reset ack。
```

owner 只能在 `l2tlb_response_drain_done()=1` 后投递 `RELEASE_FINAL_INACTIVE`。driver 仅检查 item metadata 和
真实接口采样，不读取 adapter queue；因此 adapter drain 可以与 stop/final-inactive/closing 并行，仍由 parent 在
`release_grantable()` 中统一等待。

```text
begin_l2tlb_release_closing(owner_name):
  要求 owner_name 是当前 claimed owner，且 sequence 已进入 stopping/global-stop release 路径；否则 uvm_fatal。
  要求 global_stop_requested=1、close_requested=1、close_request_reset_epoch==current_reset_epoch、request_owner_name==owner_name、
    l2tlb_release_admission_closed=1、admission_closed_generation==close_generation 且 admission_owner_name==owner_name；否则 uvm_fatal。
  要求 l2tlb_response_drain_done()=1；否则 uvm_fatal。
  要求 final_inactive_done=1 且 final_inactive_generation==close_generation；否则 uvm_fatal。
  要求 monitor_final_sample_settled(current_reset_epoch, final_inactive_transport_sample_seq)=1；否则 uvm_fatal。
  若 closing flag 已为 1：
    若 closing_owner_name!=owner_name 或 closing_generation!=close_generation：uvm_fatal。
    return。
  设置 release_closing=1、closing_owner_name=owner_name、closing_generation=close_generation。
  此时 `close_requested` 早已封闭 UID admission；若 register_uid_tlb_record_on_issue() 仍被调用，必须作为冗余防御性
    uvm_fatal，不能把失败原因归为 closing 新建了 admission gate。
```

**`wait_for_l2tlb_release_grant_or_reset()` 抽象功能描述：** 该 owner-local helper 只在 final transport sample 已完成
closing 和 terminal ack 后等待 parent 的 grant 或 runtime reset。它不等待 driver mailbox、不读取 VIF、不写 grant、也不把
recycle proof 当作 release；被唤醒后只返回“应走 reset re-arm”或“可执行最终 release”两种结果。

```text
wait_for_l2tlb_release_grant_or_reset(owner_name, expected_reset_epoch, expected_generation):
  forever：
    若 reset_active==1 或 current_reset_epoch != expected_reset_epoch：
      return RESET_PENDING；不清 claim、不直接清 token/UID，调用者回到既有 reset sample consumer 路径。
    若 l2tlb_release_granted==1：
      要求 grant_owner_name==owner_name、grant_reset_epoch==expected_reset_epoch、grant_generation==expected_generation；否则 uvm_fatal。
      要求 release_grantable(owner_name, expected_reset_epoch)==1；否则 uvm_fatal。
      return GRANTED。
    调用 l2tlb_release_state_changed_ev.wait_ptrigger()；
      该 UVM persistent-trigger wait 避免 parent 在本 helper 的状态检查与等待注册之间同 delta 写 grant 时丢失唤醒。
      event 只由“grant fields 原子写完”或“reset epoch/active 原子发布完”触发；醒来后必须回到循环重新读取 canonical state，
      不能把 event 本身解释为 grant。
```

中文文字伪代码：final sample 的 terminal ack 交给 driver 后，driver 下一 `drv_cb` 会回收 slot，而 parent 只有在该回收
proof 与其它 gate 同时满足时才写 grant。因此 owner 不应继续等待另一份 semantic mailbox sample，也不应轮询 VIF；它只等待
package 的状态变化通知。通知到达后先看 reset，因为 reset 会作废旧 generation 的 grant；未 reset 时再核对 grant 的完整
provenance 与无 grant metadata 的 `release_grantable()`。这样 final sample 的 `PUBLISHED -> CONSUMED -> EMPTY` 和
`closing -> recycle -> grant -> claim=0` 保持单向，不会形成 owner 等 grant、driver 等 terminal ack 的循环。

**`close_dispatch_raw_fence_intake_for_release()` 抽象功能描述：** 该 helper 由 fence monitor 在本 sample 的 raw input
已经完整处理后，为当前 global-stop generation 建立“后续不得再入队 raw”的生产者关闭证明。它不判断 adapter queue 是否为空，
也不执行 C4 删除。

**`dispatch_raw_fence_intake_closed_for_release()` 抽象功能描述：** 该只读 helper 供 release gate 检查当前 raw producer
是否已按指定 reset epoch/close generation 停止；它不写 state、不替代 adapter queue drain。

```text
close_dispatch_raw_fence_intake_for_release(dut_sample_seq):
  调用者必须是 fence_agent_agent_monitor 在本 sample 已采样 sfence.valid、完成 push_raw_sfence() 或确认本拍无 raw、
    并写 l2tlb_raw_fence_producer_settled_sample_seq=dut_sample_seq 后；其它调用者为 uvm_fatal。
  若 !dispatch_l2tlb_lookup_active：return；当前 NO_DISPATCH 不建 raw FIFO/intake seal。
  若 reset_active 或 !close_requested：return；运行态没有 global-stop close 时不得封闭 raw intake。
  要求 close_request_reset_epoch==current_reset_epoch；否则 uvm_fatal。
  若 dut_sample_seq <= close_request_sample_seq：return；必须保留 close request 当拍及其后第一个完整 raw sample 的采样机会。
  若 raw_fence_producer_settled_sample_seq!=dut_sample_seq：uvm_fatal；不能只凭 service 空队列宣布生产者已停止。
  若 raw_fence_intake_closed=1：
    要求 raw_fence_closed_epoch==current_reset_epoch 且 raw_fence_closed_generation==close_generation；否则 uvm_fatal。
    return；同一 release 的 closed 是 level 状态，不重复写入。
  原子写 raw_fence_intake_closed=1、raw_fence_closed_epoch=current_reset_epoch、
    raw_fence_closed_generation=close_generation、raw_fence_cutoff_sample=dut_sample_seq。

dispatch_raw_fence_intake_closed_for_release(close_generation, reset_epoch):
  若 !dispatch_l2tlb_lookup_active：return 1。
  return raw_fence_intake_closed && raw_fence_closed_epoch==reset_epoch &&
         raw_fence_closed_generation==close_generation && raw_fence_cutoff_sample>close_request_sample_seq。
```

中文文字伪代码：fence monitor 不在 parent 的 negedge stop 当下停收 raw，因为该时刻以前已经驱动到 VIF 的 fence
仍可能在下一真实 monitor sample 才被观察到。monitor 先完整处理这一次 raw，再写当前 generation 的 intake closed。
之后 adapter 只收敛已经存在的 raw/context/C4 work；若后续又看到有效 raw，说明 routing 并未真正停止，必须直接报错，
不能在 parent 已发 grant 后悄悄把新工作遗留给不存在的 adapter service。

**`release_grantable()` 抽象功能描述：** 该只读 helper 统一 parent 发放 release grant 与 owner 最终清 claim 前的
收敛判断。它不访问或清理任何队列，输入只包括当前 claimed owner 和当前 reset epoch；它不得读取
`l2tlb_release_granted`、grant owner、grant epoch 或 grant generation，避免 parent 等待尚未写出的 grant；`DISABLED/NO_OWNER` 永远返回 0。

```text
release_grantable(owner_name, reset_epoch):
  若 responder_mode != ENABLED：return 0。
  若 reset_epoch != current_reset_epoch：return 0。
  若 owner_name 不是当前 claimed owner：return 0。
  return global_stop_requested && close_requested && close_request_reset_epoch==current_reset_epoch && request_owner_name==owner_name &&
         l2tlb_release_admission_closed && admission_owner_name==owner_name && admission_closed_generation==close_generation &&
         final_inactive_done && final_inactive_generation==close_generation &&
         release_closing && closing_owner_name==owner_name && closing_generation==close_generation &&
         l2tlb_response_drain_done() && dispatch_l2tlb_live_entry_drain_done() &&
         dispatch_raw_fence_intake_closed_for_release(close_generation, current_reset_epoch) &&
         l2tlb_transport_monitor_drain_done(current_reset_epoch, final_inactive_transport_sample_seq) &&
         l2tlb_transport_sample_recycle_done_seq == final_inactive_transport_sample_seq &&
         l2tlb_transport_sample_mailbox_empty() &&
         required_reset_acks_done(current_reset_epoch) && !reset_active。
```

```text
is_l2tlb_lifecycle_release_safe(owner_name):
  要求 owner_name 是当前 claimed owner。
  调用 common_data.check_l2tlb_release_uid_waiting(waiting_uid_count)：
    它在低频 release 路径扫描全部 record_valid 的 UID record，并只统计 state=WAITING 的实例；
    不以 pte_valid、token、owner 名称或 lookup key 过滤。
  返回 pending_q 为空 && driving_req 无效 && barrier_q 为空 &&
       waiting_uid_count == 0 && l2tlb_transport_sample_mailbox_empty()==1。

normal body exit:
  要求 parent 已提出 global stop，且 owner 已在某个真实 drv_cb 完成本拍 admission settle 后调用
    close_l2tlb_admission_for_release(owner_name, current_sample)，随后由 driver 在真实 drv_cb 调用
    confirm_l2tlb_admission_closed_at_drv_cb()；否则 uvm_fatal。
  stopping 时继续发送 normal inactive/必要旧 response item，直到 is_l2tlb_lifecycle_release_safe()=1；
  不得仅因 outstanding_count()==0 设置 should_exit。
  check_l2tlb_lifecycle_accounting()。
  若 !is_l2tlb_lifecycle_release_safe(): uvm_fatal（表示实现绕过了上述继续 service 规则）。
  投递 item_kind=RELEASE_FINAL_INACTIVE、item_generation=close_generation、item_reset_epoch=current_reset_epoch、
    ready=0、resp_valid=0 的独立 item；
    finish_item() 返回后只把 sequence-local release phase 置为 `FINAL_SENT`，随后必须回到
    wait_l2tlb_transport_sample()；不得在这里阻塞等待 `final_inactive_done`、recycle 或 grant。
  driver 在真实 drv_cb 确认 final 后会把 frozen final proof 写入同一份 `PUBLISHED` sample；sequence 必须在主循环 dedicated
    final 分支消费这份 sample，核对 monitor settled，先调用 begin_l2tlb_release_closing(owner_name)，再对这份 sample 写 CONSUMED。
  terminal ack 后，driver 才能在下一真实 drv_cb 写 recycle proof 并清 mailbox；parent 的 grant_l2tlb_final_release() 在完整
    release_grantable() 为真时原子写 grant metadata 并触发 l2tlb_release_state_changed_ev。
  dedicated final 分支调用 wait_for_l2tlb_release_grant_or_reset()：GRANTED 时才调用内部检查 helper
    prepare_l2tlb_lifecycle_owner_release(owner_name)，成功后在同一 sequence 控制流、无等待拍地调用内部原子 helper
    try_release_l2tlb_lifecycle_owner(owner_name)；RESET_PENDING 时回到 reset sample flow，不得把 reset 前 final/proof 当作新 epoch release。
  该原子 helper 再次要求 owner claim=1、closing_owner/grant_owner 都匹配、closing_generation/grant_generation==close_generation、
    grant_reset_epoch==current_reset_epoch、release_granted=1，且
    release_grantable(owner_name, current_reset_epoch)=1；
    任一不满足则 uvm_fatal 并保留 claim。
    全部满足后只原子清 response owner 自己拥有的 owner claim、owner_admission_settled_sample_seq、close request 和 closing 字段；
    不清 driver-owned admission_closed/cutoff/final-inactive metadata，不清 fence-monitor-owned raw-fence intake close，也不清
    parent-owned grant metadata。上述状态由各自 direct writer 保留到 testcase-start 或 runtime reset 的对应清理边界；close_generation
    counter 保持单调，claim==0 是唯一 release-complete 判据。

do_kill():
  若本 sequence 未 claim：直接执行既有 kill 收尾。
  若已 claim：do_kill 不是 release 入口；直接 uvm_fatal 并保留 claim，避免绕过 final inactive sample。
  若已 claim 但不安全：uvm_fatal；不得直接 void'(try_release...)、不得清 FIFO 后伪装为自然退出、不得把 owner
  交给后续 sequence。

phase_ended()/driver 清理：
  只检查并报告 owner 状态；不得驱动接口、不得直接调用 try_release 绕过上述 gate。若 owner 仍 claimed，必须报告
  phase/lifecycle fatal；不得把 phase 结束当作 release grant。
  若 phase 结束时仍有 token、barrier 或 WAITING UID，报告 lifecycle fatal；不得把
  connect takeover 的静态 capability 当作已完成 release。
```

中文文字伪代码：当前源码的 `do_kill()` 为避免 driver 卡在 `get_next_item()` 而直接 release owner；但这只对
已经静止的 sequence 正确。若它仍有 request token、barrier 或 UID waiting instance，直接 release 会让 testcase
结束路径遗留无归属的 response 生命周期状态。最小且可诊断的规则是：正常测试不得在 non-quiescent responder 上
`stop_sequences()/kill()`；发生即 fatal。reset 已经在 service loop 完成 reset cancel 且全部队列清空后，才可满足
同一 release gate，不建立绕过该 gate 的特殊 release 分支。UID 检查采用整个 `uid_tlb_record_by_uid` 的有效
`WAITING` 集合，而不是含义不清的“owner 相关”子集；单-owner 合同下若存在任一等待实例，就说明 testcase 尚未安全收敛。
closing flag 使全表检查与 release 处于同一个 admission-closed 边界；若此时仍有 dispatch issue 注册 UID，说明
global stop/sequence 拓扑违反合同，必须 fatal 而非在 testcase 结束时静默遗留新等待。

本 plan 的 release API 只有两个对外阶段：`begin_l2tlb_release_closing(owner_name)` 建立 closing，随后由
`release_l2tlb_lifecycle_owner(owner_name)` 执行最终释放。`prepare_l2tlb_lifecycle_owner_release()` 只是后者内部的
只读检查 helper，`try_release_l2tlb_lifecycle_owner()` 只是后者内部的原子写入 primitive；二者都不是 sequence、driver、
parent 或 `do_kill()/phase_ended()` 可直接调用的第三条 release 入口。

## 5. Live Entry 失效协同

`apply_due_sfence_invalidate()` 所属的 stage-aware live-entry plan 同样在 C0 仅登记、C4 才删除
`tlb_entry_by_key`。这样 C0 已 fire token 可以从 fence 前 entry 冻结 response snapshot；C4 之后新 request 才会
miss 并建立 generation 更高的 entry。

两个 flow 的固定顺序为：

```text
C0：L2TLB token/UID owner 保存 C0 fire 的 entry snapshot；dispatch-active 时 adapter 独立登记 raw fence 的
    entry-invalidate pending record。
C4：L2TLB token/UID owner 确认无 driving response -> cancel pending old token 和具备 pre-anchor request-fire marker 的
    WAITING UID；adapter 独立调用公共 `apply_due_sfence_invalidate()` 删除 live entry。
```

live-entry flow 不得扫描 `pending_q`、修改 `flush_canceled_count` 或依据 `entry_generation` 删除 UID history；
L2TLB lifecycle flow 不得自己重新实现 S1/S2 fence matcher。两者只共享 anchor/due 的 V2 常量定义。

## 6. 失败策略与非目标

| 条件 | 处理 |
|---|---|
| active lifecycle 中 event 的 `sample_seq` 大于当前 global sample，或同一 sample 被同一 consumer 重复推进 | 状态变更前 `uvm_fatal`，不以当前拍重锚 barrier；`sample_time` 仅作 monitor 来源记录，不要求等于 negedge service 的 `$time`。 |
| flush event history 的 `event_seq` 跳过未消费值、同 sample 的不同 reason 未合并/同 reason 重复，或 reason mask 含 `CSR_CHANGE/FENCE` 之外的位 | `uvm_fatal`；同 sample 只能按 reason mask 合并一个 lifecycle barrier。 |
| due barrier 处理时出现有效 `driving_req` | selector 时序错误，`uvm_fatal`；不允许把会在 filter flush 同拍 fire 的 response 记为完成。 |
| token 的 event 序号大于当前已知 event 序号 | `uvm_fatal`；token 归属被错误倒写。 |
| selector 仅因 `accept_flush_event_seq != last_seen_flush_event_seq` 拒绝旧 token | 禁止该实现；必须按 barrier due sample 判断是否仍可完成。 |
| 已 fire 的 request 或 external response fire 取不到 `dut_sample_seq-2` CSR history | `uvm_fatal`；不得退回 runtime latest、UID frozen CSR 或 sequence 本地 `sample_seq`。 |
| 任何 CSR history、barrier due、raw-fence timestamp、token/UID 时间比较使用 lifecycle owner 的本地 `sample_seq`，或非 CSR monitor 路径推进 global sample | 禁止该实现；只有 CSR monitor 调用 `advance_dut_global_sample()`。fence/redirect/ctrl monitor、L2TLB driver/sequence、adapter 和 negedge service 只能调用 `peek_current_dut_global_sample()`。 |
| `dispatch_monitor_capture_en`、`l2tlb_responder_active` 或 `l2tlb_lifecycle_owner_claimed` 被用作 raw fence FIFO gate/consumer 判定 | 禁止该实现。仅 `dispatch_l2tlb_lookup_active=1` 时 raw fence 入 FIFO，且 `dispatch_monitor_event_adapter` 是唯一 consumer。 |
| `dispatch_l2tlb_lookup_active=0` 时 raw FIFO 或 pending invalidate queue 非空 | `uvm_fatal`；DISABLED/NO_OWNER 的 no-dispatch 合同下没有 req_valid 或 live entry，不允许遗留 adapter 工作。 |
| DISABLED/NO_OWNER 的 no-dispatch passive sampler sample 观察到 `req_valid==1` | `uvm_fatal`；passive driver 的 ready 固定为 0，monitor 只消费该唯一 driver sample，不能等待 `req_valid && req_ready` 才发现错误。 |
| 旧 `collect_runtime_context_events() -> drain_sfence_events() -> apply_raw_sfence()` 可达、旧 `apply_sfence_invalidate()` 仍可被直接调用，或一个 dispatch sample 多次调用 raw-fence service | 静态 review/运行期均失败；只允许 `service_monitor_once() -> service_l2tlb_sfence_events()` 这一条 C4-aware 路径。 |
| owner release 时 token、barrier 或 WAITING UID 尚未收敛，parent 尚未确认 adapter queue drain/raw-fence intake close，final inactive/closing 未成立、reset active，或未收到 release grant | `uvm_fatal` 并禁止 release；唯一 owner 不进行 handoff，未完成工作不能交给下一 sequence。adapter queue drain 与 raw-fence intake close 可与 owner final item 并行，但只能由 parent 的统一 gate 等待。 |
| C4 cancel 以 UID issue/wait-start sample 判断，或取消 marker=0 的 WAITING UID | 禁止该实现；只能使用本次等待实例的 first request-fire marker 与 barrier anchor 判断。 |
| active-owner `do_kill()` 直接调用 `try_release_l2tlb_lifecycle_owner()` | `uvm_fatal` 并保留 claim；无论是否 quiescent/granted，`do_kill()` 都不能为避免 driver blocking 静默释放。 |
| normal stop/idle 只检查 `outstanding_count()==0` 就退出 | 禁止该实现；ready 保持 0 并继续 service，直到 barrier 与 WAITING UID 也全部收敛；parent 还要等待 adapter 的 raw/context/pending-invalidate drain。 |
| global stop 已请求、但 stop `ready=0` item 尚未经过真实 `drv_cb` 采样 | 只允许完成 close request 写入前同 sample 已建立的工作；close request 写入后的所有 capture/register helper 调用立即 fatal。不得把 close request 当成已关闭 transport，也不得用 closing/grant 替代 admission-close 确认。 |
| `confirm_l2tlb_admission_closed_at_drv_cb()` 未取得冻结的 `sampled_req_fire` | `uvm_fatal`；不能用 `req_valid`、sequence item 状态或未来接口值猜测该边界是否真的无 fire。 |
| stop/final item 的 `item_kind` 不是预期值、generation 不匹配、stop sample 不晚于 close request，或普通 ready=0 item 被当作 stop/final | `uvm_fatal`；不得用 ready 电平猜测 lifecycle 身份，也不得置位 `final_inactive_item_done`。 |
| reset | 先停止 admission；response owner 清 token/driving/barrier/全部 WAITING UID，fence monitor 清 raw producer/context-dedup，adapter 清 raw/context/pending invalidate 与 live table/index，CSR monitor 清 history valid；各职责按 epoch 回 ack，由同一 owner重新 warm-up；旧 C4 work 不得继续执行。 |
| testcase 已经成功 claim 过 owner 后再次 claim，或 release 前没有 parent grant | `uvm_fatal`；`owner_claimed_once` 只在 testcase-start 清零，runtime reset/release 不清除。 |
| `sfence.bits.flushPipe=1` | 不单独触发 token cancel、ready hold 或 entry delete。 |

本 plan 不建模 ROB `flushAfter` redirect，不暂停 standalone LSQ driver，不改变 main/status table、pass/fail、
terminal 或 L2TLB response payload。它只修正 L2TLB responder 对 V2 DTLB filter flush 的 token 和 ready 时序。

## 单 owner 审核修正（最终 coding 约束）

本 plan 的 `lifecycle owner` 是 testcase 级唯一 response owner：一个 testcase 只启动并 claim 一次
`memblock_l2tlb_base_sequence`，从首次 ready 到 global stop 持续存在。第二个 claim、提前 release、kill 后 restart、
owner handoff、inactive-gap、高水位 reconcile 和 dispatch topology transition 均不属于本 plan，发生时必须 `uvm_fatal`。
单 owner 不改变每个真实 `req_valid && req_ready` 独立分配 token，也不改变一个 response 可多播到多个 UID 的规则。

### 唯一 sample 推进规则

**`advance_dut_global_sample()` 抽象功能描述：** 该同步 helper 由 CSR monitor 在每个 post-reset `posedge mon_cb` 唯一调用，
为本 DUT 周期推进一个 global sample，并作为 CSR history、flush event、C0/C4 barrier 的统一时基；它不修改 response 或
live-entry 状态。

```text
CSR monitor：每个 reset 已经在 callback 入口前解除的 post-reset posedge 调用 advance_dut_global_sample($time) 一次，
  发布该 sample 的 CSR history。若本 callback 观察到 reset_active 从 1 变为 0，只完成 reset release，不 advance、不发布 history/done；
  下一真实 posedge 才是第一个由 CSR/FENCE producer 同时参与的 post-reset sample。
fence/redirect/ctrl 等同 posedge monitor：先调用 wait_for_l2tlb_sample_anchor($time)，再读取 peek_current_dut_global_sample()
  并把该返回值写入 raw；不得推进 sample。只在本 sample NBA/发布窗口结束后仍没有 anchor 时 uvm_fatal。
CSR monitor 和 fence monitor 每个 sample 都调用 mark_l2tlb_sample_producer_done(sample_seq, producer_kind)，
即使本拍没有 CSR change 或 sfence.valid 也要报告“已完成采样”。
required producer mask 收齐后，协调器才按已登记 reason 合并并写 lifecycle_event_published_seq=sample_seq；没有事件时 reason_mask=0。
L2TLB driver/sequence：同一 posedge 不调用推进 helper；先以当前 drv_cb anchor 在同拍 bounded 窗口确认 CSR sample 已建立，
再读取 current sample，并检查 csr_history_published_seq 与 lifecycle_event_published_seq 是否覆盖该已确认 sample。
dispatch adapter 和 negedge service：只读取 peek_current_dut_global_sample()，不得再次推进 sample。
若 raw.sample_seq > current sample：uvm_fatal；已消费 event 用 event_seq 去重，不因其 sample 小于 current 再次报错。
sample_time 只记录 monitor 采样来源，不参与跨 phase 的相等判断。
```

迁移规则必须覆盖现有旧 API 调用者而非只新增新函数：`get_dut_sample_seq()` 完全退出运行期调用，不能作为兼容的
隐式 advance；CSR monitor 是唯一 `advance_dut_global_sample()` 调用者。所有 posedge raw producer（当前 fence、redirect、
ctrl monitor）先等待 anchor 后 peek；所有只读 consumer（当前 `memblock_lsqenq_dispatch_base_sequence`、
`common_data_transaction`、pending-MMIO soft-test、adapter/service）直接 peek。`peek_latest_dut_sample_seq()` 若在过渡期保留，
只能是 `peek_current_dut_global_sample()` 的无状态 wrapper，迁移完成后删除。代码 review 以工程内不存在
`get_dut_sample_seq()` 调用作为完成条件。

当前调用者迁移清单如下；这是实际 coding 落点，不以泛化检索替代逐项修改：

| 当前位置 | 当前职责/旧调用 | 迁移后调用 |
|---|---|---|
| `memblock_sync_pkg.sv:535-553` | `get_dut_sample_seq()` 推进、`peek_latest_dut_sample_seq()` 读取 | 提供唯一 `advance_dut_global_sample()` 与无副作用 `peek_current_dut_global_sample()`；旧 `get` 删除，旧 `peek_latest` 仅可暂存为 wrapper 后删除。 |
| `csr_ctrl_agent_agent_monitor.sv` | 当前尚未成为 sample 唯一写者 | 每个 post-reset `posedge mon_cb` 调用一次 `advance_dut_global_sample($time)`。 |
| `fence_agent_agent_monitor.sv`、`redirect_agent_agent_monitor.sv:68`、`io_mem_to_ooo_ctrl_agent_agent_monitor.sv:208,240` | 同拍 raw producer，后两者调用旧 `get` | `wait_for_l2tlb_sample_anchor($time)` 后 `peek_current_dut_global_sample()`，不得推进。 |
| `memblock_lsqenq_dispatch_base_sequence.sv:720` | sequence 调用旧 `get` | 只 `peek_current_dut_global_sample()`；无 anchor 时按所属 sample contract 等待，不得补推进。 |
| `common_data_transaction.sv:504,508,1799,2023` | 只读比较使用旧 `peek_latest` | 直接改为 `peek_current_dut_global_sample()`。 |
| `soft_test_memblock_pending_mmio_directed_sequence.sv:156,292,332,385` | directed helper 使用旧 `peek_latest` | 直接改为 `peek_current_dut_global_sample()`；等待 sample 的 event 控制对象改为新 global sample。 |
| `memblock_l2tlb_base_sequence.sv` | stop/final item 只靠 ready=0 形态区分 | 为每个 lifecycle transaction 写入 `item_owner_name=current_owner`、`l2tlb_release_item_kind`、当前 close generation 和生成时 `l2tlb_release_item_reset_epoch`；非零 reset epoch 且 baseline proof 未完成时先投递 `item_kind=NORMAL/is_post_reset_baseline=1` 的 inactive item；response drain 后只投递独立 `RELEASE_FINAL_INACTIVE`。 |
| `L2tlb_agent_agent.sv` | `connect_phase()` 为空，driver 到 monitor 没有 sample 订阅链路，driver 也没有明确 sequencer slot owner | 在 agent 内部连接 `drv.transport_sample_ap -> mon.transport_sample_imp`，并调用 `drv.bind_transport_slot_owner(sqr)`；连接或绑定失败/重复连接 fatal。该连接不新增 env/RM 外部端口。 |
| `L2tlb_agent_agent_monitor.sv`、`L2tlb_agent_agent_xaction.sv`、`L2tlb_agent_agent_driver.sv` | monitor 当前在独立 `@mon_cb` 读取 transport VIF，与 driver 的唯一采样合同冲突；driver 也无法证明 ready=0 item 的 lifecycle 身份，reset 后可能遗留旧 item | 将 transport 采样唯一收敛到 driver：sequence 把 `item_owner_name` 写入 xaction，driver 在 `try_next_item()` 返回后复制到 `last_driven_item_owner_name`，再在 sample 中发布 `sampled_item_owner_name`；monitor 删除/禁用独立 `@mon_cb` transport 读取，改为消费 driver sample analysis 数据，仅保留非 transport 的诊断/连接检查。xaction 同时增加 kind/generation/reset_epoch/item owner/`is_post_reset_baseline` 本地 metadata；driver 从真实 `drv_cb` 冻结 metadata/fire/ready/resp_valid，正常 epoch 才调用 stop-confirm/final-inactive/baseline helper；每个 reset-active 或 stale epoch item 在 `try_next_item()` 返回后必须一次 `item_done()`、强制 inactive、丢弃且不确认；reset release 后先完成 tagged NORMAL/inactive baseline；metadata 不连接 DUT wire。 |
| `L2tlb_agent_agent_sequencer.sv` | 当前没有 driver-to-sequence immutable sample 的单槽所有者，driver 也可能阻塞在 `get_next_item()`，使 reset/final 回收没有独立 wakeup。 | 新增唯一 `l2tlb_transport_slot_t` 与 reserve/get/ack/recycle helper；slot 在 monitor 同步处理后才通知 sequence 消费。driver 每个 `drv_cb` 先 recycle 再 `try_next_item()`；无 item 仍服务 reset/inactive sample。wrapper handle 仅在 sequencer slot 保存，package 只保存 scalar proof。 |

本表之外新增的 posedge raw producer 也必须走 anchor+peek；新增的 service/sequence consumer 只能 peek。任何新 caller 都不能重新引入
可推进 sample 的兼容 helper。

### 4.6 现有 L2TLB monitor 的唯一采样落点

**`L2tlb_agent_agent_monitor::write_transport_sample()` 抽象功能描述：** 该 monitor 只把 driver 已冻结的 transport sample
转换为既有 analysis transaction/诊断事件，避免第二个组件在同一 `mon_cb` 再次读取 request/response wire。它不推进 global
sample、不生成 response token、不确认 release 状态；该函数通过同步 `analysis_imp` 调用，返回时本 sample 已完成处理。

```text
driver 唯一在 drv_cb 冻结 mon_cb 的 request/response 4-state 值、reset_active/current_reset_epoch 和 item-owner metadata
  -> 创建并 freeze 一个 wrapper sample_ref；ENABLED 时先以同一 sample_ref handle 原子预留 sequence mailbox，再通过 analysis port 同步发布 sample_ref
L2TLB monitor
  -> 不再执行 @vif.mon_mp.mon_cb，不再逐字段读取 transport VIF
  -> 通过 analysis_imp 的 write(sample_ref) 消费 driver sample，getter 取得副本后转换/转发原有观测 transaction
  -> 仅做原有 X/Z/连接诊断；不得重算另一份 fire、sample_seq、reset epoch 或 lifecycle event
sequence
  -> 消费 mailbox sample，并在完成或 abort 后以 transport_sample_seq 回写 CONSUMED/DROPPED ack
```

analysis port 与 sequence mailbox 必须引用同一份 immutable sample provenance；monitor 的诊断副本不能成为第二个
transport producer。若当前 monitor analysis consumer 不能迁移到 sample analysis port，则本轮必须显式关闭该 transport
analysis 发布，并在 build/connect 时 fatal，而不能保留独立 `@mon_cb` 采样作为“旁路”。

### 4.7 Driver-to-monitor TLM 连接

本节是后续 coding 的目标落点；当前源码仍可能保留 monitor 独立 `mon_cb` 采样、空的 `connect_phase()` 或旧的 sequence
sample 路径，本 plan 尚未 coding、compile 或 smoke。review 只检查本节是否能直接指导迁移，不把当前未实现状态误判为
本轮 plan 已完成。

**`L2tlb_agent_agent_transport_sample` 抽象功能描述：** 此 `uvm_object` 只包装一份已经 freeze 的
`memblock_l2tlb_drv_sample_t`；它使 sequence mailbox 与 UVM analysis TLM 共享同一个只读 payload，不新增第二份
transport 事实或第二个采样序号。

**`L2tlb_agent_agent_monitor::write_transport_sample()` 抽象功能描述：** monitor 通过同步
`uvm_analysis_imp` 接收 driver sample，在 `write()` 调用内按现有 monitor 的 transaction/diagnostic 格式转发观察结果。
它不等待 clock、不读 VIF、不改 wrapper 或 mailbox state；`write()` 返回时本 sample 已处理完毕，不产生 monitor backlog。

```text
L2tlb_agent_agent_pkg.sv：
  在 package imports 中显式 `import memblock_sync_pkg::*;`；该 package 已在 `cfg/tb.f` 的 agent filelist 前编译，
  因此 xaction 可使用共享 lifecycle enum/标量，但 `memblock_sync_pkg` 不得反向引用 agent typedef/class。

L2tlb_agent_agent_xaction.sv：
  在本文件的 include guard 内、`class L2tlb_agent_agent_xaction` 之前定义
    `typedef struct { ... } memblock_l2tlb_drv_sample_t;`
  该 struct 保存本 plan 规定的 4-state VIF、sample/probe、reset snapshot、frozen item metadata 与 final proof；
  它不得保存 agent handle、mailbox handle 或任何 `uvm_object`。因为本文件在 agent package 中先于 driver/monitor/sequencer include，
  三者可直接使用同一个 typedef；`seq_pkg` 在 agent package 后编译并 import 本 package，因此 sequence 也只见到这一份类型。
  随后声明 L2tlb_agent_agent_transport_sample extends uvm_object
    -> 内部保存 local memblock_l2tlb_drv_sample_t payload_data
    -> 内部保存 local bit frozen
    -> driver 通过 fill_payload()/freeze() 完成构造
    -> consumer 只能调用 get_payload(output memblock_l2tlb_drv_sample_t payload_copy) 读取副本
    -> 不暴露 public payload；freeze 后任何 setter/fill/write 尝试均 uvm_fatal

L2tlb_agent_agent_driver.sv：
  声明 uvm_analysis_port #(L2tlb_agent_agent_transport_sample) transport_sample_ap
  在 build_phase 创建 transport_sample_ap
  每个 drv_cb 只创建一次 wrapper，driver fill 完全部 payload 后调用 freeze()
  若 ENABLED：先把同一 sample_ref handle 与 transport_sample_seq 原子写入单槽 sequence mailbox=PUBLISHED，再 transport_sample_ap.write(sample_ref)
  若 DISABLED/NO_OWNER：只 transport_sample_ap.write(sample_ref)，不进 sequence mailbox、不调用 get_next_item
  analysis_port.write() 返回前 monitor 的同步 write() 必须完成；driver 不重用或修改 sample_ref

L2tlb_agent_agent_monitor.sv：
  声明 uvm_analysis_imp #(L2tlb_agent_agent_transport_sample, L2tlb_agent_agent_monitor) transport_sample_imp
  在 build_phase 创建 transport_sample_imp
  实现 function void write(L2tlb_agent_agent_transport_sample sample_ref)：只转调 write_transport_sample(sample_ref)
  write_transport_sample(sample_ref) 是无等待 function：get_payload(payload_copy) -> 校验 epoch/4-state -> 填充原有 mon_tr 并同步 write
  run_phase() 不再启动 mon_data() 的 transport loop；若保留 run_phase，仅允许非 transport 的诊断/连接检查，不得读取 transport VIF
  删除 mon_data() 对 transport FIFO 的 get、@vif.mon_mp.mon_cb 与全部 transport wire 读取
  monitor 只修改自己的 processing/settled/diagnostic 状态，不修改 sample_ref、mailbox slot 或 sample payload

L2tlb_agent_agent.sv：
  connect_phase(): drv.transport_sample_ap.connect(mon.transport_sample_imp)，并调用 drv.bind_transport_slot_owner(sqr)
    把同一个 sequencer slot owner 显式注入 driver；driver 不通过未定义的层级路径查找 sequencer。
  若 driver/monitor 任一端缺失、重复连接或 connect 失败：uvm_fatal
```

#### 可编译对象归属与单槽访问接口

为避免 `memblock_sync_pkg.sv` 在 agent package 之前编译时持有 agent class handle，本 plan 不把
`L2tlb_agent_agent_transport_sample` handle 放进 `memblock_sync_pkg`。具体落点固定如下：

```text
L2tlb_agent_agent_xaction.sv：
  在 xaction class 前先定义 `memblock_l2tlb_drv_sample_t` typedef，再定义
  L2tlb_agent_agent_transport_sample；只在这里定义 transport struct、wrapper 的 private payload/freeze/getter。
  wrapper/struct 均不移动到 `memblock_sync_pkg`，避免 package 编译环。

L2tlb_agent_agent_sequencer.sv：
  定义 l2tlb_transport_slot_t：state、transport_sample_seq、terminal_transport_sample_seq、sample_ref handle、
    new_sample_event、slot_recycled_event 和 semaphore。
  提供 reserve_publish_slot()、wait_and_get_published_slot()、ack_published_slot()、
    recycle_terminal_slot_at_drv_cb()、slot_is_empty() 五个 task/function；driver/唯一 owner sequence 只能调用这些接口，
    不允许直接写 slot 成员。

L2tlb_agent_agent_driver.sv：
  通过本 agent 的 sequencer handle 调用 reserve_publish_slot()；成功预留 PUBLISHED 后同步调用 transport_sample_ap.write()，
    write 返回后 trigger new_sample_event。每个 drv_cb 开头调用 recycle_terminal_slot_at_drv_cb()；
    该 helper 在 final terminal 时同步更新 package 中的 scalar recycle_done_seq，不把 wrapper handle 写入 package。
  main_phase/get_owned_item_or_abort() 改为 try_next_item() 轮询；NO_ITEM 走 drive_idle + transport sample publish，
    reset request 不依赖 sequence 再投递 item 才能唤醒。

memblock_l2tlb_base_sequence.sv：
  class 声明 `uvm_declare_p_sequencer(L2tlb_agent_agent_sequencer)`；只通过 p_sequencer 的
    wait_and_get_published_slot()/ack_published_slot() 读取或终态确认 sample；reset/abort 仍由同一 owner 调用
    ack_published_slot(..., DROPPED)。final recycle 后 sequence 不再要求 driver 投递新的 cycle item。

memblock_sync_pkg.sv：
  只保存跨 agent/sequence 的标量 lifecycle state，例如 reset request epoch、final transport sample seq、
    recycle_done_seq、ack/proof；不得声明 wrapper handle、analysis FIFO 或第二个 sample queue。
```

`new_sample_event` 的触发点在 synchronous `analysis_imp.write()` 返回之后，因此 monitor 已处理 sample 时 sequence 才能取得
mailbox handle；这不改变 `EMPTY -> PUBLISHED -> CONSUMED/DROPPED -> EMPTY` 的唯一状态机。`semaphore` 仅保护
reserve/CAS/recycle 的原子性，不是额外 queue 或跨拍 backlog。任何 helper 若试图在 `PUBLISHED` 以外状态取得/确认 sample，
或在 final recycle 后重新发布 semantic slot，均 `uvm_fatal`。

**`reset_l2tlb_transport_monitor_state()` 抽象功能描述：** monitor 登记 reset epoch，并在后续 driver 同步发布的
reset-active sample 中丢弃旧 epoch observation、检查 future epoch；它不直接清理 FIFO（本方案没有 FIFO），不修改 frozen
wrapper payload。匹配 reset sample 已处理后只通过 `monitor_reset_ackable(epoch, reset_sample_seq)` 回 MONITOR ack，不依赖 final sample。

**`monitor_reset_ackable(epoch, reset_sample_seq)` 抽象功能描述：** 只读判断指定 reset epoch 和 transport sample 序号的 reset-active sample 是否已经
由 monitor 同步处理且 monitor 当前不在处理 sample；它还要求该 seq 严格晚于上一次 reset ack 的 seq。它只服务 runtime reset
ack，不读取 final-inactive 状态。

**`monitor_final_sample_settled()` 抽象功能描述：** 只读判断指定 epoch 和精确 transport sample 序号的 final inactive
sample 是否已由 monitor 同步处理；判据只来自 driver 冻结在 sample 中的 final proof，不读取 live reset 或 live
`final_inactive_done`。它只服务 release gate，不写 reset ack。

**`l2tlb_transport_monitor_drain_done()` 抽象功能描述：** 调用 `monitor_final_sample_settled(epoch, seq)` 判断 monitor 是否已
处理指定 final inactive sample；由于 analysis imp 是同步调用，不存在 FIFO empty 条件。它不清 mailbox、不修改 sample。

```text
monitor reset request(epoch):
  monitor_active_epoch=epoch；monitor_reset_pending_epoch=epoch；monitor_reset_ack_floor_transport_sample_seq=monitor_last_reset_ack_transport_sample_seq；
  清 monitor_reset_sample_processed_epoch/transport_sample_seq、monitor-local final-settled epoch/seq 和 diagnostic reset baseline；
    不直接清其它职责状态

monitor write_transport_sample(sample_ref):
  monitor_processing=1
  payload_copy = sample_ref.get_payload()
  若 payload_copy.sampled_reset_epoch < monitor_active_epoch：记录 stale，不转发旧 mon_tr
  若 payload_copy.sampled_reset_epoch > monitor_active_epoch：uvm_fatal
  若 payload_copy.sampled_reset_active：记录 monitor_reset_sample_processed_epoch=payload_copy.sampled_reset_epoch、
    monitor_reset_sample_processed_transport_sample_seq=payload_copy.transport_sample_seq，不转发普通 mon_tr
  否则校验 4-state 后转换并同步转发既有 mon_tr
  更新 monitor_last_transport_sample_seq
  若 payload_copy.sampled_final_inactive_proof_valid==1：
     要求 payload_copy.sampled_reset_active==0、payload_copy.sampled_final_inactive_proof_epoch==payload_copy.sampled_reset_epoch、
       payload_copy.sampled_final_inactive_proof_transport_sample_seq==payload_copy.transport_sample_seq；否则 uvm_fatal
     写 monitor_final_settled_epoch=payload_copy.sampled_final_inactive_proof_epoch、
       monitor_final_settled_transport_sample_seq=payload_copy.sampled_final_inactive_proof_transport_sample_seq
  monitor_processing=0
  若 monitor_reset_pending_epoch!=0 &&
     monitor_reset_ackable(monitor_reset_pending_epoch, monitor_reset_sample_processed_transport_sample_seq)：写
    l2tlb_monitor_reset_ack_epoch=monitor_reset_pending_epoch、l2tlb_monitor_reset_ack_transport_sample_seq=
      monitor_reset_sample_processed_transport_sample_seq、monitor_last_reset_ack_transport_sample_seq=
      monitor_reset_sample_processed_transport_sample_seq，并清 pending；普通 current-epoch sample 不能代替 reset sample ack

monitor_reset_ackable(epoch, reset_sample_seq):
  return monitor_processing==0 && monitor_reset_sample_processed_epoch==epoch &&
         monitor_reset_sample_processed_transport_sample_seq==reset_sample_seq &&
         reset_sample_seq>monitor_reset_ack_floor_transport_sample_seq

monitor_final_sample_settled(epoch, transport_sample_seq):
  return monitor_processing==0 && monitor_final_settled_epoch==epoch &&
         monitor_final_settled_transport_sample_seq==transport_sample_seq

l2tlb_transport_monitor_drain_done(epoch, transport_sample_seq):
  return monitor_final_sample_settled(epoch, transport_sample_seq)
```

agent 内部 observer 连接不需要修改 `memblock_env.sv`、RM 或 scoreboard 的外部 analysis 接口；monitor 原有
对环境的 analysis 输出保持原语义。sequence mailbox 的终态/EMPTY 回收仍只由 sequence 和 driver 管理，monitor 没有写权。

`clear_raw_monitor_queues()` 不清零 runtime global sample。testcase 初始化若需要清零只能在 monitor 启动前调用专用
testcase-start helper；runtime reset 不得重置 sample。reset 统一按以下顺序处理：停止 admission，response owner 取消
token/driving/barrier/UID，driver 清本地 item 后回 response ack，adapter 清 raw fence/context 和 `sfence_invalidate_pending_q`、
再清 canonical live table 与 range index，最后由 CSR monitor 清 CSR history valid 并由同一 owner warm-up；旧 reset 前的 C4 工作不得作用于 reset 后新 entry。
即使 `DISABLED/NO_OWNER + NO_DISPATCH` 没有 response/adapter owner，CSR monitor、fence monitor、L2TLB monitor 和
reset coordinator 仍必须完成 CSR/FENCE/MONITOR reset ack；response/adapter ack 按 `reset_required_ack_mask` 标记为 N/A，
不得等待不存在的 sequence。

### Global stop 与 release

response owner 的 `l2tlb_response_drain_done()` 只覆盖 token、driving response、barrier 和全部 UID `WAITING`；
adapter 的 `dispatch_l2tlb_live_entry_drain_done()` 只覆盖已存在的 raw fence、待绑定 context 和 pending invalidate，
不能证明 fence monitor 不会在下一 sample 新入队。global stop 后，fence monitor 必须先完整处理 close request 后的第一个 raw
sample，再写与 current reset epoch/current close generation 匹配的 `l2tlb_raw_fence_intake_closed`；`release_grantable()` 同时等待
这个 intake proof 与 adapter queue drain，不能用其中任一项替代另一项。
`memblock_main_dispatch_auto_build_main_table_base_sequence::service_real_dispatch_flow()` 是唯一 parent/global-stop
  coordinator：它在 `global_stop_requested` 首次成立时停止新的 dispatch routing，但不得在 negedge 直接写 close request。
已在此前 posedge 驱动的 `ready=1` 可能在下一 drv_cb 真实 fire，因此唯一 owner 必须先 capture 该 fire、发布
`l2tlb_owner_admission_settled_sample_seq`，再在同一 drv_cb 写 close request 并生成 stop item。parent 在 driver 真实确认
`admission_closed` 前持续 service，不得提前发 grant 或将 global stop/close request 当 cutoff。确认 close 后继续调用 monitor/adapter service；只有
`release_grantable(current_owner, current_reset_epoch)` 返回真时才置
`l2tlb_release_granted=1`、`l2tlb_release_grant_owner_name=current_owner` 和
`l2tlb_release_grant_reset_epoch=current_reset_epoch`、`l2tlb_release_grant_generation=current close_generation`，随后等待唯一 owner 清除
`l2tlb_lifecycle_owner_claimed` 才退出。parent 必须在上述 grant metadata 同一原子写入完成后 trigger
`l2tlb_release_state_changed_ev`，使已 terminal-ack final sample 的 owner 不依赖新的 semantic mailbox sample 即可复核 grant；
不得在 trigger 前单独写 `release_granted` 或只写部分 provenance。`L2tlb_agent_agent_driver::phase_ended()` 只检查/report，不得驱动接口或直接调用
`try_release_l2tlb_lifecycle_owner()`；任何 active-owner `do_kill()` 都必须 fatal 并保留 owner claim。

`final_inactive_item_done` 只能由 L2TLB driver 在真实 `drv_cb` 边界采样带有
`item_kind=RELEASE_FINAL_INACTIVE`、current owner、`item_generation=current close_generation`、`item_reset_epoch=current_reset_epoch` 的
`sampled_req_ready=0/sampled_req_fire=0/sampled_resp_valid=0` item 后置位；普通 idle、flush-hold 或 `RELEASE_STOP` 都不能置位。sequence 的 `finish_item()` 返回、
phase callback 或 parent 观察到队列为空都不能单独置位该状态。这样 release grant 不会早于最后一个 DUT interface sample。

adapter 的 raw/context/pending-invalidate drain 与 owner 的 response drain、stop/final-inactive/closing 可以并行进行；
fence monitor 的 raw-intake close 也与 token drain 并行，但必须在 parent grant 前完成。response owner 不读取 adapter queue，
parent 只在 `release_grantable()` 内等待 response drain、adapter queue drain 和匹配 generation 的 raw-intake closed proof。

`grant_l2tlb_final_release()` 的抽象功能是 parent 只读汇总收敛状态并发放一次带 epoch 的 grant，不负责清理 owner 的 token/UID、
发送 final inactive 或写 admission seal。其判定顺序为：global stop 后等待 owner 在当前 reset epoch 的真实 drv_cb 写 close request，
再等待 driver 确认 admission closed；当前 owner claim、final inactive、精确 monitor final-sample settled、final mailbox recycle proof、closing、response/adapter drain、raw-fence intake closed、transport sample mailbox EMPTY、
当前 epoch required reset ack 和非 reset 状态全部由 `release_grantable(owner_name, current_reset_epoch)` 统一复核；只有真值才原子写完整 grant owner/
reset epoch/generation 并随后 trigger `l2tlb_release_state_changed_ev`。owner 在原子清 claim 前也再次调用相同谓词并核对 grant epoch。
任一条件不满足都保持 grant=0并继续 service；`DISABLED/NO_OWNER` 不发送 grant。grant 已发过时不得
重复发放，owner 消费 grant 后才清 claim，parent 只以 claim==0 判断完成。

普通 C4 不取消 `uid_tlb_first_request_fire_sample_seq==0` 的 WAITING UID；redirect/kill owner 写 `CANCELED`，reset
取消全部 WAITING，raw-hit response 写 `COMPLETED`。global stop 仍有 WAITING 必须 fatal，不能清表伪装收敛。

### 启动拓扑与 reset 的唯一协调入口

`reset_l2tlb_lifecycle_for_same_owner()` 是 sequence/dispatch 侧的调用包装；实际 reset 状态和 epoch 由共享的
`reset_l2tlb_runtime_state(reset_epoch, reset_required_ack_mask)` 维护。两者不能各自清理同一队列，也不能形成两个
  reset owner：包装只提交当前 epoch，shared coordinator 负责按 ack mask 协调 response、fence monitor、adapter、CSR 和 L2TLB monitor。

`initialize_l2tlb_testcase_lifecycle()` 的抽象功能是锁定 testcase 的静态 responder/topology 合同。输入包括
`responder_mode`、`dispatch_topology`、`start_mode`、`needs_response`、`connect_takeover_active` 和
  `reset_required_ack_mask`；它校验 `DISABLED/NO_OWNER + NO_DISPATCH`、`ENABLED + DISPATCH_ACTIVE` 等支持组合，并强制
ack mask 包含 CSR、FENCE 和 MONITOR；RESPONSE/ADAPTER 按 topology 加入。相同输入可幂等返回，任一输入变化必须 `uvm_fatal`；它不 claim owner、不推进 sample、不处理 runtime reset。

**`reset_l2tlb_lifecycle_for_same_owner()` 抽象功能描述：** 该 helper 由 reset coordinator 在 runtime reset 观察到后
按固定顺序清除旧 L2TLB 生命周期工作，并把已经 claim 的同一 responder 留在 CSR warm-up 状态。它不调用 release、不清
`owner_claimed_once`，也不重新启动 sequence。

#### Runtime reset 直接写者合同

`reset_l2tlb_lifecycle_for_same_owner()` 只是协调入口。reset coordinator 只建立/复用 `reset_epoch`、发出各职责 reset
request 并等待 ack；它不得直接清除下列职责拥有的 queue、history 或 response 状态。具体写者固定为：

| 状态/对象 | reset 直接写者 | reset 动作 |
|---|---|---|
| live `reset_active/current_reset_epoch` 与 driver sample 中的 `sampled_reset_active/sampled_reset_epoch` | reset coordinator；driver 是 sample 字段的唯一发布者 | coordinator 只在 reset 边界发布 live epoch/active；driver 在同一 `drv_cb` 原子复制到 immutable sample。sequence、monitor、stop/final/baseline helper 只使用冻结 sample 字段解释该拍；不得由 consumer 重新读取 live reset 或从 item epoch 推断 sampled reset。 |
| `pending_q`、`driving_req`、`barrier_q`、全部 UID `WAITING`、owner admission/close request、`acceptance_opened_since_reset`、`pre_ready_hold_until_sample`、`ready_opportunity_since_lifecycle_block`、`idle_count` | response owner | cancel/写 `CANCELED`，清自己的 close/admission 字段，将 acceptance/pre-ready hold/ready opportunity/idle 诊断全部清为初始值，发布 `response_owner_reset_done_epoch`；不写 response ack。 |
| 已发送 item 的 `last_driven_*` metadata、未发送 fetched stale item、`admission_closed/cutoff`、`final_inactive_transport_sample_seq`、`l2tlb_transport_sample_recycle_done_seq`、冻结的 final proof、transport sample slot 的终态回收 | L2TLB driver | 清/忽略 metadata latch；只在 final confirm 成功后把 final proof 与 final transport seq 写入尚未 freeze 的 working sample；已由 `try_next_item()` 取得但未发送的 item 精确 `item_done()` 一次；reset/abort 的 sequence drain 通过 CAS 写 DROPPED，或正常 consumer 已写 CONSUMED 后，driver 在每个 drv_cb 优先回收 slot 到 EMPTY，并在 final terminal 时写 recycle_done_seq；仅 owner reset-done、driver 本地清理完成且 mailbox empty 后唯一写 response ack。 |
| `raw_sfence_q`、待绑定 context、pending invalidate、live entry/range index | dispatch adapter | 清自己的 raw/context/live 状态并写 adapter ack；不修改 response token/UID，也不清 fence monitor 的 producer 状态。 |
| `raw_fence_producer_settled_sample_seq`、raw-fence intake close active fields、fence monitor 本地 context/dedup baseline | fence monitor | 清本地 producer watermark/intake-close/context-dedup 状态并写 `l2tlb_fence_reset_ack_epoch`；不修改 adapter-owned FIFO/live table。 |
| monitor active epoch、processing、reset-sample processed epoch/transport seq、reset ack epoch/transport seq、final-settled epoch/transport seq | L2TLB monitor | reset request 更新 monitor-local active epoch 和 reset-ack seq floor；`analysis_imp.write()` 只比较 incoming frozen sample 与该 local epoch，旧 epoch drop、future epoch fatal。`monitor_reset_ackable(epoch, reset_sample_seq)` 只以匹配 reset sample 的 epoch+seq、`monitor_processing=0` 和严格递增 seq 写 MONITOR ack；`monitor_final_sample_settled(epoch, seq)` 只读取 frozen final proof，供 release gate 使用。release 不等待 FIFO；不修改 wrapper payload。 |
| CSR history、CSR producer done mask、event-ready watermark、event history、CSR context dedup baseline | CSR monitor | 清 history/CSR context 并写 CSR ack；不清 global sample、owner claim、fence producer state 或 adapter queue。 |
| `release_granted` 及 grant metadata | parent/reset grant coordinator | 作废当前 epoch 的 grant expectation；不代替 owner 清 claim。 |

```text
testcase-start：
  仅 testcase-start helper 清 global sample、owner_claimed_once、close request/admission close、冻结 item metadata、
  release_granted/grant owner+epoch+generation、release_closing/owner+generation 和 final-inactive done/generation；
  不创建第二个 release-complete 镜像状态，因为 claim==0 是唯一 release-complete 判据。
  初始化 reset_epoch=0、monitor_active_epoch=0、monitor_last_reset_ack_transport_sample_seq=0，并使 required_reset_acks_done(0)=1、reset_required_ack_mask(0) 不等待 RESPONSE/ADAPTER/FENCE/MONITOR；这只是“尚未发生 runtime reset”的虚拟基线，不是 runtime topology mask 的例外。epoch>=1 的每次 runtime reset 固定要求 CSR/FENCE/MONITOR，RESPONSE/ADAPTER 再按 topology 加入。
  在启动任何 responder 前固定 dispatch_l2tlb_lookup_active。
  选择 legacy default sequence 或 explicit vseq 的 uvm_do_on(l2tlb_seq, ...)，二者同时可达立即 uvm_fatal。

runtime reset：
  reset coordinator 建立/复用唯一 reset_epoch，原子发布 reset_active/current_reset_epoch、作废当前 grant 后 trigger
  l2tlb_release_state_changed_ev，再向 response owner、driver、fence monitor、adapter、CSR monitor、L2TLB monitor 和 parent/grant coordinator 发 reset request；
  它只保存 epoch/ack 期望，不直接作废各职责的 queue/history/active state。保留 close_generation counter 单调不回绕，
  停止 new admission；ENABLED topology 的 owner 只负责自己的 response/token/UID、admission/closing reset 请求并驱动 inactive；NO_OWNER 不启动 sequence/semantic responder，但 driver 保持 passive sampler，不取 item、不回 RESPONSE ack。
  coordinator 同时调用 request_l2tlb_driver_service_wakeup(reset_epoch)；这只是标记 driver 下一真实 drv_cb 必须继续 service，
  不是 mailbox 写权。driver 采用 try_next_item()，即使 sequence 没有新 item 也会在下一 drv_cb 驱动 inactive、冻结
  sampled_reset_active=1 的 sample、先同步交给 monitor、再通知 ENABLED owner 的 mailbox consumer；因此 MONITOR ack 不能依赖
  owner 先产生 cycle item。若这一 reset sample 在 wakeup watchdog 内未出现，或 ENABLED owner 未对 PUBLISHED sample 终态确认，均 uvm_fatal。
  各职责按“Runtime reset 直接写者合同”分别作废 owner admission/close、driver transport metadata、fence producer state、adapter raw/live、CSR history/context
  以及 parent grant expectation；任何职责未完成自己的清理前不得回 ack。
  L2TLB driver 在 reset-active 的真实 drv_cb 将上一 item 的 stop/final metadata latch 标记为 stale、清空本地 sampled expectation，
  强制驱动 `ready=0/resp_valid=0`；该 latch 对应 item 已在正常发送时 `item_done()`，不得二次释放，也不得调用 stop-confirm/final-confirm。
  若 reset-active 时另有已经 `try_next_item()` 返回但未发送的 req，则该 req 必须走 `discard_stale_l2tlb_item()`，不能留到 reset release 后重放。
  若 ack mask 含 RESPONSE：response owner 独占执行 `cancel_outstanding_by_reset()`，取消 pending/driving token、barrier，并由
  `cancel_all_waiting_uid_for_reset()` 将全部 WAITING UID 写 CANCELED；owner 只写 `response_owner_reset_done_epoch`，不直接写 response ack。
  reset/abort 与正常 consumer 都只能调用同一个 `ack_l2tlb_transport_sample()` CAS：先成功者决定 CONSUMED 或 DROPPED，
  后续第二次 ack 一律 fatal；reset drain 若见 PUBLISHED 必须请求 DROPPED，若已是 CONSUMED/DROPPED 则只等待 driver 回收，
  不能覆盖 terminal state。L2TLB driver 只清自己的本地 item expectation；每个已经由 `try_next_item()` 取出的 stale item 都必须先调用一次
  `seq_item_port.item_done()`，再丢弃句柄并驱动 inactive，不能让 sequencer 保留未完成 item。driver 观察到
  `response_owner_reset_done_epoch==current_reset_epoch`、本地 item 已清且 `l2tlb_transport_sample_mailbox_empty()==1` 后，进入本 epoch 的
  reset-quiescent local state；该 drv_cb 不再发布 semantic mailbox sample，只驱动 inactive，并作为 RESPONSE ack 的唯一写者回报 response ack。
  后续 reset-active drv_cb 保持 quiescent/inactive、不重复 ack；因此 reset coordinator 在 required RESPONSE ack 到达前不会 re-arm 一个仍有
  PUBLISHED/terminal sample 的 mailbox，reset release 时才清 quiescent 并重新进入 baseline。
  若 ack mask 含 ADAPTER：由 dispatch adapter 独占清 raw_sfence_q、待绑定 context、sfence_invalidate_pending_q，并调用
  `clear_dispatch_l2tlb_live_entries()` 删除 canonical entry/range index；reset coordinator 只发起请求、等待 adapter ack，不能直接改这些队列/map。
  若 ack mask 含 FENCE：由 fence monitor 清 raw producer settled/intake-close active fields 与 fence-local context/dedup baseline，并回报 FENCE ack；不能清 adapter-owned FIFO/live table。
  若 ack mask 含 MONITOR：由 L2TLB monitor 清 monitor-local processing/final-settled/reset-sample 状态并登记 reset pending epoch，
  保存本次 `monitor_reset_ack_floor_transport_sample_seq=monitor_last_reset_ack_transport_sample_seq`；
  后续匹配 reset sample 通过同步 `analysis_imp.write()` 消费且 `monitor_reset_ackable(epoch, reset_sample_seq)` 成立后才回报
  MONITOR ack，同时写入独立的 `l2tlb_monitor_reset_ack_epoch/transport_sample_seq`，
  不能清 sequence mailbox 或修改 wrapper payload。
  保存 last_allocated_l2tlb_event_seq；由 CSR monitor 清 L2TLB CSR history valid、CSR sample_producer_done_mask、当前 lifecycle event-ready watermark、event history、CSR context dedup baseline。
  response owner 将自己拥有的 response_owner_event_cursor 与 last_seen_flush_event_seq 对齐 last_allocated_l2tlb_event_seq；下一条 event 从 baseline+1 分配。
  各职责完成自己的清理后才写 ack；由对应 owner/driver/fence-monitor/adapter/CSR/parent 写者分别作废 owner_admission_settled_sample_seq、release_admission_close_requested/
  request_owner_name/close_request_sample_seq/close_request_reset_epoch、release_admission_closed/admission_owner_name/
  admission_closed_generation/cutoff_sample_seq、冻结的 release item kind/generation/reset_epoch、
  release_final_inactive_item_done/release_final_inactive_generation/final_inactive_transport_sample_seq/l2tlb_transport_sample_recycle_done_seq、release_closing/closing_owner_name/closing_generation、
  release_granted/release_grant_owner_name/release_grant_reset_epoch/release_grant_generation、raw-fence producer settled/intake closed
  及其 epoch/generation/cutoff 与 parent release-pending；其中 raw-fence producer/intake 字段只能由 fence monitor 作废，
  raw FIFO/context/pending/live fields 只能由 adapter 作废，不能由 coordinator 或 response owner 代写；
  close_generation 计数器保持单调，不清 global sample、owner_claimed_once 或 active claim。
  reset coordinator 通知 CSR monitor、fence monitor 和 L2TLB monitor 即使在 DISABLED/NO_OWNER + NO_DISPATCH 下也必须回报 CSR/FENCE/MONITOR reset ack；不存在的 response/adapter职责按 ack mask 记为 N/A。
  仅 ENABLED topology 的同一 claimed owner 等新的 csr_history_published_seq warm-up 后重新开放 ready；DISABLED/NO_OWNER
  保持无 owner，不 re-arm、不恢复 responder ready；passive sampler 继续固定 inactive 并仅发布 analysis sample。仅 ENABLED 的 reset release 第一个 driver service 边界由 driver 写
  post_reset_baseline_pending=1、baseline_sent_sample_seq=0，并在发布给 sequence 的 transport sample 中置 baseline_required=1；
  sequence 只能经该 sample 投递 tagged NORMAL/inactive baseline。若 global_stop_requested 在 reset 前已为 1，则 parent 在 reset_active 期间
  不写 close/grant；reset release 后 owner/sequence 必须先投递 current reset epoch 的 NORMAL/inactive baseline item，
  driver 发送 baseline item 时先记录 baseline_sent_sample_seq；在严格更晚的真实 drv_cb 以冻结的
  `ready=0/fire=0/resp_valid=0` 写 proof 后才清 `post_reset_baseline_pending`。该 item 已在发送路径完成一次 `item_done()`，
  proof 路径不得二次释放。下一 drv_cb 重新完成 sample anchor/probe 且结果为 READY 后，owner 才能发布 admission-settled 并建立新
  close_generation/RELEASE_STOP；parent 只等待这轮新 close，不得复用 reset 前 metadata。
  若 sequencer 返回 reset 前的 RELEASE_STOP/RELEASE_FINAL_INACTIVE item，driver 记录 stale-item 诊断、驱动 inactive、调用一次
  `seq_item_port.item_done()` 并丢弃句柄，再取下一 item；若返回 current epoch 的 stop/final item 但 baseline 尚未完成，则
  `uvm_fatal`，不能确认任何新 close/final 状态。
```

`memblock_main_dispatch_auto_build_main_table_base_sequence::service_real_dispatch_flow()` 仅在 dispatch-active 下是
global-stop coordinator；它必须在 `global_stop_requested` 后停止 routing、继续 service，并等待 owner 在真实 drv_cb 已结算
本拍 admission 后写 close request；parent 自己不得写 close state。再等待 driver 在真实 `ready=0 && sampled_req_fire=0`
边界确认 admission closed，继续 service adapter，等待 response/adapter queue drain、匹配 current reset epoch/current close generation 的 raw-fence intake close、final mailbox recycle proof 与 transport sample mailbox EMPTY、
`final_inactive_item_done=1`、`monitor_final_sample_settled(current_reset_epoch, final_inactive_transport_sample_seq)=1`、`l2tlb_release_closing=1`、required reset ack 收齐且 `!reset_active`，才置带 owner/epoch 的
release grant，再等唯一 owner 原子清除 claim==0。当前支持矩阵中 no-dispatch 仅允许 DISABLED/NO_OWNER，不建 live entry/raw fence FIFO，
不启动 responder sequence、不发送 release grant；NO_OWNER passive sampler 仍运行以监测非法 req_valid；ENABLED+NO_DISPATCH 在启动前 fatal。`memblock_dispatch_real_smoke_vseq` 和
  `memblock_dispatch_real_cancel_reconcile_vseq` 显式启动 L2TLB sequence
时，`tc_base` 不得同时配置其 default sequence；legacy testcase 使用 default sequence 时不得再通过 vseq 启动它。

global-stop 分支判断 `NO_OWNER` 时只读取 `l2tlb_responder_enabled()`、`l2tlb_dispatch_active()` 和
`l2tlb_testcase_needs_response` 这三个 testcase lifecycle 真源。不得使用 connect capability
`l2tlb_responder_active` 或 `MEMBLOCK_L2TLB_SEQ_EN` plus 作为第二拓扑权威：前者只表示 wire 是否被接管，后者只参与
testcase-start 选择，二者都不能说明本 testcase 是否已经启动 responder owner。

## 7. 与原测试框架逻辑对比

| 类型 | 原逻辑 | 修改原因 | 修改后逻辑 |
|---|---|---|---|
| 功能逻辑修正 | C0 观察 event 后立即删除 pending，C0 fire 记为 killed。 | 早于 DUT C4 filter flush，丢失已接收 request。 | C0 正常建 token；C4 才取消仍 pending 的旧 token。 |
| 功能逻辑修正 | flush hold 期间同时禁止 ready 和 response。 | C1-C3 可能仍有合法旧 response。 | hold 只关闭 ready；C4 前允许旧 token response 完成。 |
| 功能逻辑修正 | response 可在 C4 flush 同拍 fire 并记为 complete。 | `PTWNewFilter` 的 entry flush 与 MemBlock 外层回填屏蔽使该 response 不形成可信 DTLB completion；旧 `PTWFilter` 的 `ptwResp_valid` 清零仅是对照。 | selector 使用严格 `< due`；C4 不允许 driving response。 |
| 功能逻辑修正 | request fire 直接取 runtime latest CSR。 | 顶层 CSR 到 `PTWNewFilter` 有两级 `RegNext`，CSR change 附近会用新 context 错建旧 request。 | CSR monitor 维护 3 项 history；capture 固定取 C-2 DTLB-side snapshot。 |
| 功能逻辑修正 | live entry 可在 C0 立即删除。 | C0 fire 与删除有表项建模竞态。 | C0 只登记，C4 由 adapter 与 token/UID cancel 同步独立删除 live entry；两侧不依赖 service 先后顺序。 |
| 删除错误逻辑 | `record_flush_killed_request()` 处理同拍 fire。 | 将顶层观察时间错误等同 filter kill 时间。 | 删除该 helper；所有 fire 进入正常 token 守恒账本。 |
| 字段边界澄清 | 可把 `sfence.bits.flushPipe` 视为 request flush 输入。 | V2 DTLB `io.flushPipe=false.B`，该字段属于 Fence 写回到 ROB 的独立路径。 | token flow 只消费 monitor flush event 及其 4 拍 due 边界。 |
| 生命周期功能修正 | owner 在 stop sample 自己确认/写 `admission_closed`，或不同文档未明确唯一写者。 | sequence 不能以 driver 实际冻结的 interface sample 替代 transport 事实；reset 后旧 item 也不能误匹配。 | owner 仅写 close request/投递带 `item_reset_epoch` 的 `RELEASE_STOP`；driver 在真实 `drv_cb` 冻结 kind/generation/reset_epoch/ready/fire 后调用 confirm helper，作为 `admission_closed/owner/generation/cutoff` 唯一直接写者；reset-active/stale item 必须 `item_done()` 后丢弃。 |
| 生命周期功能修正 | history/event 未 ready 时只驱动 inactive 并等待。 | driver 若先于 CSR monitor 读取 sample 会错用上一拍；reset re-arm 下若直接生成 stop，则可能绕过当前 epoch 的 transport 基线。 | driver 先在同一 `drv_cb` bounded anchor/probe，确认 sample 后才 peek；sample 未建立或 NOT_READY 时冻结 fire/response 为 0 并只送 inactive。reset release 后先完成 NORMAL/inactive baseline，下一 drv_cb 才允许 global stop 建立 `RELEASE_STOP`。 |
| 生命周期功能修正 | adapter queue 瞬时为空即可视为 release drain。 | fence monitor 仍可能在 grant 后入队 raw，留下无人服务的 C4 delete。 | fence monitor 在 close request 后完整处理一个 raw sample，再写与 current epoch/generation 匹配的 intake close；`release_grantable()` 同时要求 response drain、adapter queue drain 与 intake close，closed 后 raw valid fatal。 |
| 生命周期功能修正 | `NOT_READY`/sample 无 anchor 时发送的普通 inactive 会覆盖上一 baseline item 的 provenance。 | 随后真实 sample 无法确认该 baseline，非零 epoch 的公共 release proof 可能永远缺失。 | baseline proof pending 期间，同 epoch 的普通 NORMAL/inactive 或 driver idle 均保留上一 baseline provenance，直到严格更晚真实 sample 证明完成或新 reset epoch 作废。 |
| 生命周期功能修正 | testcase 启动的 epoch 0 会沿用 runtime-reset public-proof 调用路径。 | `mark_l2tlb_post_reset_baseline_done()` 明确拒绝 epoch 0，导致 startup baseline 被误报非法。 | epoch 0 只完成 driver 本地 baseline 收敛，不写公共 proof；close/release gate 对 epoch 0 保持既有豁免，非零 runtime-reset epoch 才写/读公共 proof。 |

本专项是对已归档 L2TLB lifecycle plan 的功能逻辑修正，不是纯字段改名。其余多 outstanding、response
latency、payload、permission 和主表控制行为保持不变。

## 执行中补充/修正（IMPLEMENTATION_DELTA）

### [IMPLEMENTATION_DELTA] no-dispatch testcase 不启动 responder

- 来源：coding 前编译/启动拓扑复查发现，公共 preset 默认保留
  `MEMBLOCK_L2TLB_SEQ_EN=1`，但 `tc_sanity` 等 legacy testcase 没有 DTLB dispatch 主流程。
- 原 plan：只规定 no-dispatch topology 不启动 semantic responder，但未明确公共 plus 已开启时的收敛动作。
- 实现调整：`tc_base::initialize_l2tlb_testcase_lifecycle()` 和
  `basicTest::initialize_l2tlb_testcase_lifecycle()` 将
  `needs_response` 定义为 `MEMBLOCK_L2TLB_SEQ_EN && dispatch_topology_active`。
  无 dispatch 时选择 `RESPONDER_DISABLED/START_DISABLED`，保留 connect takeover 的静态能力值，输出一条
  `uvm_info`，不配置 default sequence，也不 claim lifecycle owner；有 dispatch 时仍按原 plus 和 explicit/default
  启动合同执行。
- 原因：公共默认 plus 表示环境具备该 responder 行为的配置，不代表每个 testcase 都存在可服务的 DTLB request
  stream。若将该 plus 单独作为运行拓扑判定，会使无 dispatch smoke 在初始化阶段误报 fatal。
- 影响范围：仅影响 testcase topology 初始化和 L2TLB sequence 是否启动；不改变 active connect wire 驱动、request
  token、response payload、SFENCE/HFENCE matcher 或 live entry 生命周期。

### [IMPLEMENTATION_DELTA] sample coordinator 不得覆盖 topology

- 来源：subagent 复核发现 `initialize_l2tlb_sample_coordinator()` 与 testcase
  `end_of_elaboration` 之间没有语言级 happens-before 保证；若其晚执行，可能清掉已冻结的
  `dispatch_l2tlb_lookup_active` 或已注册的 adapter service。
- 原 plan：要求 testcase-start helper 初始化共享时基，但未明确 topology 字段和 adapter ownership 的唯一写者。
- 实现调整：sample coordinator 不再清零 testcase topology、`dispatch_l2tlb_lookup_active` 或 adapter service
  ownership；若在 lifecycle 已初始化、owner 已 claim 或 adapter 已注册后再次调用，直接 `uvm_fatal`。L2TLB
  responder body 额外要求 `l2tlb_responder_enabled() && l2tlb_dispatch_active()`，不允许错误拓扑静默运行。
- 原因：connect takeover 只表示 response wire capability，testcase lifecycle 才决定是否存在 dispatch service；
  通用 sample reset 不能成为第二个 topology owner。
- 影响范围：只收紧启动初始化和错误诊断；不改变 runtime reset 的各职责清理、response token、payload 或 fence
  删除时序。

### [IMPLEMENTATION_DELTA] final proof 与 mailbox PUBLISHED 状态分离

- 来源：explicit dispatch smoke 在全部请求完成并置 global stop 后，L2TLB owner 未能完成 release；静态时序审查确认
  final item 被 driver 采样后，final proof sample 没有进入 semantic mailbox。
- 原 plan：规定 final inactive 的 frozen transport sample 必须作为一份 `PUBLISHED` sample 交给 owner `CONSUMED`，
  但没有明确 final-confirm helper 不得提前修改 mailbox state。
- 实现调整：`mark_l2tlb_final_inactive_at_drv_cb()` 只记录 final item 已在真实 `drv_cb` 被采样的
  epoch/generation/transport sequence，不再写 `l2tlb_transport_sample_mailbox_empty_state=0`。
  同一 callback 随后的 `publish_transport_sample()` 成功占用 sequencer slot 后，仍由既有
  `mark_l2tlb_transport_sample_mailbox_nonempty()` 唯一写入 PUBLISHED/non-empty；owner ack 后由下一真实
  `drv_cb` recycle 返回 EMPTY。
- 原因：final confirm 发生时，上一 transport slot 已被本拍 recycle，final proof 的 wrapper 尚未 reserve/PUBLISHED。
  提前标记 non-empty 会使 publish predicate 自己否决 final sample，sequence 无法 begin closing 或 ack，release grant
  永远无法满足 recycle/mailbox EMPTY 条件。
- 影响范围：只修复 final release 的 driver-to-sequence handoff；不改变 stop/final 的 DUT 采样条件、owner grant 条件、
  token/UID drain、adapter/fence drain 或普通 transport sample 的单槽协议。

### [IMPLEMENTATION_DELTA] stop/final terminal item 的二次采样收敛

- 来源：末轮 transport/mailbox review 发现，`release_close_requested` 置位后，sequence 原实现每拍都把
  当前 transaction 重标为 `RELEASE_STOP`，并清零 response valid；如果同一拍已有 due response，stop item
  会吞掉该 response，下一拍还会再次触发 stop confirm。另一个边界是 final item 后若 sample 暂时为
  `NOT_READY`，driver 的 idle item 会覆盖 final metadata，使 final proof 无法再次确认。
- 实现调整：
  1. close request 发出且 `l2tlb_release_admission_closed=0` 时只发送一次带 generation/epoch 的
     `RELEASE_STOP`；close 已被真实 driver sample 确认后，后续 inactive item 改用 `NORMAL` metadata，
     但保留当前 response transaction 的 `io_ptw_resp_valid` 和 payload，直到真实 response fire。
  2. final inactive proof 在 sequence 的 `NOT_READY` 分支之前处理；它只要求 frozen sample 有效、ready/fire/
     response 均为无效、monitor 已同步处理该 final sample，不能被 CSR/event watermark 暂时未就绪吞掉。
  3. driver 保留 final item provenance 到对应 transport sample 完成 recycle，recycle 后才清 metadata；因此
     一个 final sample 只能 confirm 一次，后续 callback 不会重复写 final state。
  4. final sample recycle、mailbox EMPTY、transport slot EMPTY 且 lifecycle owner 已释放后，driver 自然返回；
     不再依赖 UVM phase 结束强杀采样线程。
- 原因：`RELEASE_STOP` 是一次 transport admission cutoff，不是持续 level；final proof 是一次性 terminal
  handoff，不是可被后续 idle item 覆盖的 live level。保留 response 和 provenance 可以同时满足旧 response
  drain 与一次性 close/final 校验。
- 影响范围：只收敛 release stop/final 的重复确认、response 保留和 driver 退出；不改变 C0/C4 flush barrier、
  token/UID 账本、CSR snapshot、payload matcher 或普通 response latency。

### [IMPLEMENTATION_DELTA] final eligibility 必须等待本地 barrier 消费

- 来源：末轮独立 lifecycle review 发现，global stop 后若 pending token 和 UID waiting 都为空，但 C0 已登记的
  `barrier_q` 尚未到 due sample，原 final 条件仍可能成立。owner 随后进入 release grant 等待，不再执行本地
  C4 barrier 消费。
- 实现调整：final inactive 的准入条件增加 `barrier_q.size()==0`；`check_l2tlb_lifecycle_accounting("owner_release")`
  同时把非空 `barrier_q` 视为未收敛并打印数量后 fatal。close 后在 barrier due 前继续发送普通 inactive item，直到
  `apply_due_l2tlb_flush_barriers()` 消费本地 barrier，再建立 final。
- 原因：adapter 的 live-entry 删除和 sequence 的 token/barrier 账本是两个独立消费者；response/UID 为空不能证明
  C4 已完成。final 必须同时等待二者收敛，避免 release 后留下未处理的本地 barrier。
- 影响范围：只收紧 final/release gate；不改变 C0 建 barrier、C4 due 延迟、旧 response 选择或 UID cancel 规则。

### [IMPLEMENTATION_DELTA] flush hold 只关闭新的 request admission

- 来源：末轮 review 发现 `send_l2tlb_cycle()` 的 response selector 附加了 `!hold_active`，且在 hold 期间只要
  `cycle_tr.io_ptw_resp_valid=1` 就 fatal。这样 C1-C3 已经在 C0 之前或 C0 同拍建立的旧 token 无法完成合法 response，
  与 V2 filter 在 C4 才完成 flush 的时序矛盾。
- 修改前逻辑：`hold_active=1` 同时禁止 `next_ready` 与 `select_due_response()`，即使旧 token 的 due 在 C1-C3 也不允许发
  response。
- 修改后逻辑：

  ```text
  构造下一 cycle item：
    若当前没有 driving response，且当前 sample 不是 C4 due：
      从旧 pending token 中选择 next sample 不会落到 C4 的 response
    next_ready = 非 stop、非 close、CSR/history ready、非 hold、未满 outstanding
    不因为 hold 清除已选择或已 driving 的 response valid
  ```

  C4 的 `due_barrier_this_sample` 和 `select_due_response(next_sample)` 内的 due 检查仍禁止 response fire；因此本 delta
  只恢复 C1-C3 的旧 response drain，不允许 C4 绕过 filter flush。
- 状态副作用：`pending_q/driving_req` 在 C1-C3 可正常由 response fire 推进；`accept_hold_until_sample` 仍只控制新
  `req_ready`，不改变 token 的 C4 cancel 条件。

### [IMPLEMENTATION_DELTA] post-reset baseline 必须成为公共 release proof

- 来源：末轮 review 发现 driver 虽验证了 baseline 后的无活动 sample，但只清本地
  `post_reset_baseline_pending`。`close_l2tlb_admission_for_release()` 与 `release_grantable()` 没有读取共享 proof，
  因而其他 lifecycle consumer 无法证明当前 reset epoch 已建立 transport 基线。
- 修改前逻辑：baseline 只在 driver 私有字段中完成；close/release 可能仅凭局部时序进入 stop/final。
- 修改后逻辑：

  ```text
  driver 观察到 baseline 后严格更晚的有效 sample：
    断言 ready=0、request fire=0、response valid=0
    调用 mark_l2tlb_post_reset_baseline_done(reset_epoch, dut_sample_seq)
    再清 driver 本地 baseline pending

  close_l2tlb_admission_for_release(owner, sample)：
    epoch 非 0 且共享 baseline proof 缺失 -> uvm_fatal

  release_grantable(owner, epoch)：
    epoch 非 0 且共享 baseline proof 缺失 -> 返回 0
  ```

- 状态副作用：`l2tlb_post_reset_baseline_done_*` 成为当前 epoch 的唯一共享 proof；reset direct writer 仍负责清它。
  epoch 0 保持历史 startup 行为，不会被该 gate 误阻塞。

### [IMPLEMENTATION_DELTA] baseline provenance 不得被 `NOT_READY` 普通 inactive 覆盖

- 来源：独立终审发现 baseline item 已送出、但下一 `drv_cb` 尚无 global sample 或 producer watermark 为
  `NOT_READY` 时，sequence 会送一个未带 baseline tag 的 `NORMAL/inactive` item。旧
  `update_last_driven_metadata()` 随即覆盖 `last_item_is_post_reset_baseline`，后续真实 sample 无从证明
  先前 baseline。
- 修改前逻辑：只保护 `RELEASE_FINAL_INACTIVE` provenance；`got_item=0` 或同 epoch 的普通 NORMAL/inactive
  都会把 pending baseline metadata 清成普通 idle。
- 修改后逻辑：在 `post_reset_baseline_pending=1`、上一 item 是当前 epoch tagged baseline、且 reset 未激活时：
  同 epoch 的非 baseline `NORMAL/inactive` item 或 driver idle 不更新 metadata；前者的 `ready` 与 `resp_valid` 必须均为 0，
  否则直接 `uvm_fatal`。其它 item 仍按原规则写入；一旦严格更晚
  real sample 完成 baseline proof，或 reset 开始新 epoch，再由既有路径清除/替换 provenance。

  ```text
  update_last_driven_metadata(item, got_item, epoch, reset_active):
    若 baseline proof pending 且 previous item 是同 epoch tagged baseline：
      若本轮是 idle，或本轮 item 是同 epoch NORMAL/inactive 且没有 baseline tag：
        断言 item.ready=0 且 item.resp_valid=0
        保留 previous baseline provenance；返回
    按既有 final provenance 和普通 item 规则更新 metadata

  sample_previous_vif():
    仅在严格更晚且 sample_valid 的 sample 中读取保留的 baseline provenance
    ready/fire/resp_valid 必须均为 0
    epoch 非 0：写公共 baseline proof
    清 driver-local pending
  ```

- 原因：`NOT_READY` 只表示 semantic producer 尚未齐全，不代表前一拍 driver item 从 VIF 消失。baseline proof 是
  transport 时序事实，必须保留到真实 sample 才能确认。
- 状态副作用：不新增 DUT wire 或第二份 item；只延长 driver private metadata 的生命周期。sequence 仍只发送一份 tagged
  baseline，所有后续 waiting item 仍会正常 `item_done()`，不会重复发送或重复确认 baseline。

### [IMPLEMENTATION_DELTA] epoch 0 不调用 runtime-reset baseline public proof

- 来源：同一终审发现 testcase startup 使用 virtual epoch 0，而公共 helper
  `mark_l2tlb_post_reset_baseline_done()` 的合同只接受非零 runtime-reset epoch。若 driver 对 epoch 0 调用它，会在
  正常启动路径产生错误 fatal。
- 修改前逻辑：baseline 的“严格更晚真实 sample”分支没有区分 epoch 0 与 runtime reset epoch，可能将 epoch 0 传给
  public proof helper。
- 修改后逻辑：driver 对 epoch 0 仍等待同样的无 ready/fire/response 真实 sample，并清自身
  `post_reset_baseline_pending`；但跳过 `mark_l2tlb_post_reset_baseline_done()`。`update_reset_quiescent()` 也在
  runtime coordinator 尚未创建非零 epoch 时直接返回，不累计 runtime-reset watchdog 或写 RESPONSE ack。

  ```text
  baseline proof sample 到达：
    检查 ready=0、fire=0、resp_valid=0
    若 sampled_reset_epoch != 0：
      写 l2tlb_post_reset_baseline_done(epoch, sample)
    否则：
      只完成 startup 的 driver-local baseline
    清 post_reset_baseline_pending
  ```

- 原因：epoch 0 是 testcase-start 基线，不是一次由 reset coordinator 发布的 runtime-reset transaction；把二者混用会
  违反 public helper 的 epoch ownership。
- 状态副作用：非零 epoch 的 close/release 仍强制读取唯一公共 proof；epoch 0 的原有 startup close/release 行为不变，
  不会因新增 gate 或 watchdog 误阻塞。

### [IMPLEMENTATION_DELTA] reset release 同拍必须完成完整 producer-barrier sample

- 来源：上一版 delta 试图在 reset 从 active 变为 inactive 的 CSR callback 中跳过 global sample，避免 fence monitor
  先看到 reset 时漏写 FENCE done。但基础 smoke 在 `170ns` 证明该方案仍有反向调度竞态：CSR 先结束 reset 并跳过
  anchor，随后执行的 fence monitor 已看到 reset inactive，调用 `wait_for_l2tlb_sample_anchor($time)` 后找不到 anchor 而 fatal。
- 被替换的逻辑：CSR monitor 在 release callback 中 `continue`，该 callback 不建立 CSR sample；fence monitor 对同拍
  reset 状态没有二次确认。该方案既可能产生缺 FENCE done 的半个 sample，也可能产生 fence 等待不存在 anchor 的空拍。
- 修改后逻辑：release 边本身就是首个 post-reset semantic sample。CSR monitor 仍是唯一 writer：只有 reset 已真正结束时，
  它立即推进 global sample、发布 CSR history 并写 CSR producer done。fence monitor 若一开始看到 reset active，先完成自己的
  reset 清理/ack，再在同一仿真时刻的 NBA/delta 区域重新读取 reset；若 CSR 已在该窗口结束 reset，则继续绑定同拍 anchor，
  正常采样 `sfence` 并写 FENCE producer done；若 reset 仍 active，才结束本拍。

  ```text
  CSR monitor：
    end_l2tlb_runtime_reset()
    若 reset 仍 active：本拍不发布 sample
    否则：推进 global sample，发布 CSR history，写 CSR producer done

  fence monitor：
    若本拍初始看到 reset active：清 fence 私有 reset 状态并写 ack
    在同一时间的 NBA/delta 重新检查 reset
    若 reset 仍 active：结束本拍
    否则：等待同拍 CSR anchor，采样 sfence，始终写 FENCE producer done
  ```

- 原因：两类 monitor 的 clocking-block callback 没有固定先后顺序；必须让两种顺序都形成完整的同拍
  CSR/FENCE producer pair，不能通过静默丢弃 reset-release 边规避竞态。若该边 `sfence.valid=1`，它也必须被正常记录为
  C0，不能静默 drop。
- 影响范围：只修改 reset release 的 monitor 同拍同步；global sample 不回绕，C-2 history、token、UID、C0/C4 和
  payload 语义保持不变。

### [IMPLEMENTATION_DELTA] `NO_OWNER` 的 stop 与 request fail-fast 使用固定 topology

- 来源：归档后 review 发现 no-dispatch testcase 即使 runtime plus 仍为 1，也会按 lifecycle 初始化为
  `DISABLED/NO_OWNER`；parent 若仍以 connect capability/plus 等待 claim，会把合法 no-owner 误报为 claim timeout。
  同时 passive driver 固定 `ready=0`，若 monitor 不检查 `req_valid`，误配置 request 会永久悬挂。
- 修改后逻辑：global-stop parent 只以 `l2tlb_responder_enabled()`、`l2tlb_dispatch_active()` 和
  `l2tlb_testcase_needs_response` 选择分支。合法 `DISABLED/NO_OWNER + NO_DISPATCH` 立即结束 L2TLB wait，
  不等待 claim、不发 grant；任何不支持的组合 fatal。L2TLB monitor 在非 reset、已初始化 topology 下发现
  disabled responder 的 frozen `sampled_req_valid==1` 时立即 fatal，并打印 transport sample、DUT sample、VPN、s2xlate 和 topology。
- stop 分支还必须先检查 no-owner 不变量：`l2tlb_lifecycle_owner_claimed`、`l2tlb_owner_claimed_once` 与
  `l2tlb_release_granted` 均为 0。只有 `ENABLED + DISPATCH_ACTIVE + needs_response` 的 topology，且当前真的仍有
  owner claim，parent 才可调用 `grant_l2tlb_final_release()`。
- 原因：connect takeover 与 plus 不是 testcase 当前 responder 生命周期真源；fail-fast 也不能等待不会发生的
  `valid && ready` fire。
- 影响范围：只收紧错误 topology 诊断和 no-owner stop；不改变 active responder 的 token、response、flush 或 release 流程。

### [IMPLEMENTATION_DELTA] `phase_ended()` 保持 fail-fast

- 来源：单 owner 合同要求 active owner 只能通过 global-stop/final inactive/grant 正常释放；driver 的 `phase_ended()`
  虽然不再 release，却仍只报 `uvm_error`，会让 testcase 在保留 claim 的情况下继续结束。
- 修改后逻辑：若 phase callback 仍看到 claimed owner，直接 `uvm_fatal` 并保留 claim；该 function 不驱动 idle、
  不确认 stop/final，也不调用 release helper。
- 原因：phase 结束时仍持有 owner 代表 lifecycle 未收敛，是不可恢复的测试框架状态而非普通诊断。
- 影响范围：只改变异常终止等级；正常 global-stop release 行为不变。

### [IMPLEMENTATION_DELTA] 同步 V2 responder 与 SFENCE flow 文档

- 来源：执行规则要求当前 flow 文档反映真实调用链；原 plan 主要描述 coding 落点，没有列出完整 flow 文档同步动作。
- 原 plan：保留已有 SFENCE/L2TLB 分析文档作为背景，不改变源码行为描述。
- 实现调整：在 `AI_DOC/mem_ut_flow_doc/tlb_l2tlb_responder_flow.md` 增加 V2 当前 transport/lifecycle 合同，
  在 `AI_DOC/mem_ut_flow_doc/sfence_flow.md` 标明旧直接 drain 链路为 entry-level 历史基线，并指向当前 responder flow。
- 原因：避免文档继续把独立 live-VIF 采样、旧 `drain_sfence_events()` 和 sequence 直接消费 latest flush 描述成 V2 当前行为。
- 影响范围：仅更新 flow 文档职责和调用链说明，不增加新的 DUT payload、token 或 adapter 逻辑。

### [IMPLEMENTATION_DELTA] P2：close/cutoff 必须在 token 分配前拒绝 request fire（待 coding）

- 来源：追加 lifecycle review 发现，`send_l2tlb_cycle()` 已在当前 frozen sample 观察到 `request_fire()` 后调用
  `capture_fired_request()`；该函数先创建 `pending`、递增 `next_request_token`，并可能通过 lookup 创建 live entry，直到
  `mark_waiting_uid_records_on_request_fire()` 才由 UID helper 检查 admission seal。若 close 已经写入，fatal 发生在
  软件 token/live-entry 副作用之后，违反本 plan 的“close/cutoff 后不创建新工作”合同。
- 术语边界：`close_requested` 是 owner 在已经结算本 sample 既有 admission 后写入的软件封口；
  `admission_closed/cutoff` 是 driver 后续真实采样 `ready=0 && fire=0` 后写入的接口关闭证明。二者都不删除已经在
  close 前真实 fire 的 token；旧 token 仍按 response、C4 cancel 或 reset cancel 之一收敛。
- 修改后逻辑：在 `capture_fired_request()` 的第一段、任何 `pending` object、live entry、request token 或 UID marker
  创建之前执行以下 guard：

  ```text
  capture_fired_request():
    若 sequence.release_close_requested==1：uvm_fatal。
    调用 data.check_l2tlb_uid_registration_open("capture_fired_request")：
      release_admission_close_requested / admission_closed / release_closing 任一为 1 -> uvm_fatal。
    仅 guard 全部通过后：
      执行既有 outstanding 检查；
      创建 pending、递增 token、lookup/build live entry、写 UID request-fire marker；
      push pending_q 并更新 accepted 账本。
  ```

- 同拍顺序保持不变：本拍 frozen `request_fire` 来自此前已经驱动的 `ready`。若此时 close 尚未写入，必须先正常
  `capture_fired_request()`，随后 owner 才能写 close request 并发送 `RELEASE_STOP`；不得为了提前检查而把 global-stop/
  close 分支移到 fire capture 前，否则会错误丢弃关闭前已被 DUT 接收的合法 request。只有已经存在 close/cutoff 的后续
  sample 再观察到 fire 才属于不支持状态，且必须在任何软件分配前 fail-fast。
- 影响范围：仅收紧非法 post-close fire 的 fail-fast 时点；不改变 C0 同拍 fire、C1-C3 response、C4 cancel、
  admission cutoff 的 driver 写者、token 账本、payload 或 ROB redirect 边界。
- 当前状态：已完成 coding；`capture_fired_request()` 在 token/pending/UID 状态分配前检查 local/shared admission cutoff。
  同拍先观察到的合法 fire 仍在 close 写入前完成 capture。

### [IMPLEMENTATION_DELTA] P2：pre-ready baseline 与 active flush hold 分离（待 coding）

- 来源：追加 lifecycle review 发现，owner 启动时可能看到已经在更早 sample 产生的历史 event；当前 sequence 主要使用
  `accept_hold_until_sample` 处理该情况，但没有独立的 owner-start baseline 状态和严格空状态证明。
- 问题含义：启动前历史 event 只是需要对齐 event cursor 的旧记录，不代表当前 sample 发生了新的 C0 flush。若把它直接写入
  `accept_hold_until_sample`，就会把“启动清场”与“运行期 DUT filter 清理”混为一谈，可能在未确认 token/UID 为空时开放 ready，
  或错误建立 barrier/cancel 工作。
- 修改前逻辑：发现 `event.anchor_sample_seq < current_sample` 且 `acceptance_opened_since_reset==0` 时，直接更新
  `accept_hold_until_sample`；没有 `owner_start_baseline_done`、没有独立 `pre_ready_hold_until_sample`，也没有一次性严格检查
  request/response/token/UID 状态是否为空。
- 修改后状态：将当前未使用的 `baseline_pending` 替换为 `owner_start_baseline_done`，并新增
  `pre_ready_hold_until_sample`。二者只由 response owner 在 testcase start/reset re-arm 清零和推进；driver 已有的
  `post_reset_baseline_pending`/公共 transport baseline proof 继续由 driver 维护，不能与 owner event baseline 合并。
- 修改后逻辑：

  ```text
  owner 第一次开放 ready 前：
    若 owner_start_baseline_done==0：
      要求 sampled_req_fire==0；
      要求 sampled_resp_valid==0；
      要求 pending_q 为空、driving_valid==0、barrier_q 为空；
      要求全部有效 UID WAITING 数量为 0；任一条件不满足 -> uvm_fatal。

      从 response_owner_event_cursor+1 连续检查历史 event：
        event.sample_seq < current_sample：只推进 event cursor/last_seen，
          不调用 handle_l2tlb_flush_event()，不建立 C0/C4 barrier，不取消 token/UID；
        event.sample_seq == current_sample：按正常 C0 event 建 barrier；
        event.sample_seq > current_sample：uvm_fatal。

      若本次只消费了启动前历史 event：
        pre_ready_hold_until_sample = max(
          pre_ready_hold_until_sample,
          current_sample + MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES);
        owner_start_baseline_done = 1'b1；
        本拍继续发送 ready=0 的普通 inactive item。

  ready 计算：
    hold_until = max(accept_hold_until_sample, pre_ready_hold_until_sample)；
    hold_until 未到期或 owner-start baseline 未完成时，next_ready=0；
    只有正常生成首个 next_ready=1 后，才置 acceptance_opened_since_reset=1。
  ```

- 语义边界：`pre_ready_hold_until_sample` 只表示启动前历史 event 的保守等待，不参与 C4 token/UID cancel；
  `accept_hold_until_sample` 仍只表示真实运行期 C0 flush barrier。两者都可能使 ready 为 0，但不能互相替代。
- 影响范围：仅补齐 owner 启动时的 event cursor、ready 基线和空状态检查；不扫描或清理 dispatch adapter 的 live entry/raw fence，
  不改变 driver transport baseline、active C0/C4、token payload、response latency 或 ROB redirect 边界。
- 当前状态：已完成 coding；response owner 使用独立的 `owner_start_baseline_done` 与
  `pre_ready_hold_until_sample` 对齐启动历史 event，并在首次开放 ready 前验证空状态。

### [IMPLEMENTATION_DELTA] P2：重复 reason 与 event sequence 跳号必须 fail-fast（待 coding）

- 来源：追加 lifecycle review 发现，`note_l2tlb_flush_event()` 对同 sample 的 reason 直接执行 OR，无法区分“同拍两个不同
  flush 原因的合法合并”和“同一个 producer/monitor 重复发布相同原因”；`get_l2tlb_event_after()` 只检查
  `event_seq > cursor`，可能静默跳过中间 event。
- 术语边界：`reason_mask` 表示 event 的来源原因，`CSR_CHANGE` 与 `FENCE` 是两个独立 reason；`event_seq` 是每个新
  lifecycle event 的单调编号；`response_owner_event_cursor`/`last_seen_flush_event_seq` 表示 response owner 已连续消费到的
  event 编号。它们不代表 DUT 的 CSR 值、fence 数量或 token 数量。
- 修改前逻辑：

  ```text
  同 sample 已有 reason_mask=CSR_CHANGE，新来 CSR_CHANGE：
    直接 OR，静默接受重复 reason。

  cursor=3，history 下一条 event_seq=5：
    只因 5 > 3 就返回 event 5，event 4 被静默跳过。
  ```

- 修改后逻辑：

  ```text
  note_l2tlb_flush_event(sample, new_reason):
    校验 new_reason 非零且只包含 CSR_CHANGE/FENCE；
    若同 sample 已有 event：
      若 old_reason_mask & new_reason != 0：uvm_fatal，报告重复 reason/sample/event_seq；
      否则：允许不同 reason 合并，old_reason_mask |= new_reason；不分配第二个 event_seq；
    若没有同 sample event：按既有单调分配规则创建一个新 event。

  get_l2tlb_event_after(cursor):
    找到第一条 event 后，计算 expected_seq = cursor + 1；
    若 event.event_seq != expected_seq：uvm_fatal，报告 cursor/expected/actual/history；
    只有严格连续时才把 event 返回给 owner。
  ```

- 无 dispatch topology 时不创建 response-side event history，但仍需对当前 sample 的 producer reason mask 做相同的
  重叠检查，防止重复 CSR/FENCE reason 被 watermark OR 静默隐藏。不同 producer 在同 sample 上报不同 reason 仍然允许形成
  一个合并后的 sample reason mask。
- 影响范围：只加强 event provenance、reason 去重和 cursor 完整性检查；不改变同 sample 不同 reason 的合法合并、C0/C4
  barrier 数量、token payload、response latency 或 active responder 的正常调度。
- 当前状态：已完成 coding；`note_l2tlb_flush_event()` 对同 sample reason 做重叠检查，
  `get_l2tlb_event_after()` 要求 event sequence 严格连续；无 dispatch topology 同样维护 reason overlap 检查。

## 历史验证记录与重新验收要求

- implementation review：`AI_DOC/plan/test_framework/review_doc/undo/mem_ut_v2_l2tlb_sfence_flush_token_timing_correction_implementation_review_20260809.md`。
- 修复 P0/P1 前的 explicit smoke：`memblock_dispatch_real_smoke_vseq`，仿真在 `482.800ns` 输出 `TEST_PASS`，
  `UVM_ERROR=0`、`UVM_FATAL=0`。
- 修复 P0/P1 前的基础 smoke：`virtual_base_sequence`，仿真在 `265.300ns` 输出 `TEST_PASS`，
  `UVM_ERROR=0`、`UVM_FATAL=0`。
- 以下为 P0/P1 修复后、最新两个 baseline blocker 修复前的历史重新验证记录：

  ```text
  make eda_compile tc=basicTest ts=memblock_dispatch_real_smoke_vseq \
    mode=l2tlb_timing_p01_20260809 cfg=tc_dispatch_real_smoke
  make eda_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq \
    mode=l2tlb_timing_p01_20260809 cfg=tc_dispatch_real_smoke wave=off \
    plus_arg='+MEMBLOCK_MAIN_TRANS_NUM=2'
  make eda_compile tc=basicTest ts=virtual_base_sequence mode=base_fun
  make eda_run tc=basicTest ts=virtual_base_sequence mode=base_fun wave=off
  ```

  explicit smoke 在 `482.800ns`、base smoke 在 `265.300ns` 均输出 `TEST_PASS`，且均为
  `UVM_ERROR=0`、`UVM_FATAL=0`；两次 compile exit code 均为 0。
- 最新的 baseline provenance 与 epoch-0 修复改动 driver 关键时序分支，因此上述历史结果不能作为最终验收。已用同一组
  explicit/base compile 与 smoke 命令完成重新执行：

  | 验收项 | compile | smoke 结果 |
  |---|---|---|
  | explicit dispatch | `eda_compile` exit code 0 | `482.800ns TEST_PASS`，`UVM_ERROR=0`、`UVM_FATAL=0` |
  | base no-dispatch | `eda_compile` exit code 0 | `265.300ns TEST_PASS`，`UVM_ERROR=0`、`UVM_FATAL=0` |

  远端 VCS 的 `KDB-OPTIONS` 与旧 `.nfs*` 清理提示均未产生编译 error 或 UVM error/fatal；末轮独立 review 已明确
  给出 `FINAL PASS`，满足归档条件。
