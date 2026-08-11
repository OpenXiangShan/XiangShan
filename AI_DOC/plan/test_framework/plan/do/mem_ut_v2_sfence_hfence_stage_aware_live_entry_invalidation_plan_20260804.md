# V2 SFENCE/HFENCE 分阶段 Live Entry 失效专项 Plan

状态：`已完成 coding，已归档`。本轮已完成静态检查、远端 `eda_compile`、基础 smoke 与 real-dispatch smoke。

共享 lifecycle 约束：`AI_DOC/plan/test_framework/review_doc/undo/mem_ut_v2_l2tlb_single_owner_lifecycle_optimization_review_20260807.md`。
本 plan 只负责 raw fence/context 与 live-entry/range-index 删除，不重新定义 owner、global sample、runtime reset、global stop 或 release。
runtime reset 的 direct-writer 边界：fence monitor 独占 raw producer settled/intake-close/context-dedup 并回 FENCE ack；
dispatch adapter 独占 raw FIFO、待绑定 context、pending invalidate、live entry/range index 并回 ADAPTER ack；CSR monitor 独占
CSR history/context 并回 CSR ack；reset coordinator 只请求/等待。`raw.reset_epoch`、context epoch 和 pending invalidate epoch 必须一致。
共享 lifecycle 另外固定要求 L2TLB monitor 对匹配 reset-active transport sample 回 MONITOR ack；本 plan 不写该 ack，但不能把
FENCE/CSR/MONITOR ack 当成完整 reset 收敛。
本 plan 不自行裁剪 global-stop/release 条件：最终只复用 timing plan 的
`release_grantable(owner, current_reset_epoch)`，其中 final inactive、monitor final settled、final mailbox recycle proof、mailbox `EMPTY`、
response/adapter drain、raw-fence intake close、当前 epoch required reset ack 和 `!reset_active` 缺一不可；该谓词不包含也不得读取 grant metadata。
parent 只有在该 gate 为真时才写入匹配 owner/epoch/generation 的 grant，owner 再以 grant 与同一 gate 共同完成 release。

## 专有名词与抽象功能说明

本 plan 只修改 mem_ut 对 `SFENCE.VMA`、`HFENCE.VVMA` 与 `HFENCE.GVMA` 的 live TLB entry 失效流。目标是让
`tlb_entry_by_key` 的删除使用 fence 采样时的上下文和 S1/S2 各自的地址范围；不改变 L2TLB pending response、
driver、flush hold、UID 历史或 DCache memory model。

| 术语 | 本 plan 中的含义 | 代码落点 | 示例 |
|---|---|---|---|
| `raw fence` | monitor 采到的一条离散 fence 事实，包含操作数字段和采样上下文。 | `dispatch_raw_sfence_t`、`raw_sfence_q` | 两拍连续的 fence 必须保留为两条 FIFO item。 |
| `fence flush reason` | fence monitor 因实际 fence 指令加入 L2TLB lifecycle flush 的原因位；它不是 CSR 字段，且 raw fence 仍独立保留。 | `note_l2tlb_flush_event()` 的 `reason_mask.FENCE` | 同 sample 若同时有 CSR change，则同一 barrier 的 reason mask 同时置两位。 |
| `CSR change flush reason` | CSR monitor 因 `satp/vsatp/hgatp/priv_virt` 改变加入 L2TLB lifecycle flush 的原因位；它只表示上下文变化，不代表发生了 fence 指令。 | `note_l2tlb_flush_event()` 的 `reason_mask.CSR_CHANGE` | 不进入 raw fence FIFO，只与同 sample 的 FENCE 合并 lifecycle barrier。 |
| `sampled CSR context` | 与 raw fence 同一 DUT sample 冻结的虚拟化状态，而不是 drain 时的最新 CSR。 | `dispatch_raw_sfence_t` 的 context 字段、`memblock_sync_pkg` 的按 sample 绑定状态 | VMID=A 时发出的 VVMA，即使之后切到 VMID=B，仍只匹配 A。 |
| `S1 stage` | HS/S-stage 或 VS-stage 的 GVA/VA 到 GPA/PA 翻译。 | `s1_tag/s1_level/s1_asid/s1_vmid/s1_pte_g` 及 S1 sector 字段 | `HFENCE.VVMA` 只检查 S1 字段。 |
| `G-stage` | GPA 到最终 supervisor physical address 的第二阶段翻译。 | `s2_tag/s2_level/s2_vmid/s2_pte_n` | `HFENCE.GVMA` 只检查 S2 字段。 |
| `allStage entry` | 同一个 live entry 同时保存 S1 与 S2 translation payload 的组合 entry。 | `memblock_tlb_entry`，`s2xlate=allStage` | S1 或 G-stage 任一 fence 命中后都删除这一整个 entry。 |
| `dispatch-active` | dispatch service 已启动，允许建立页表、接受 DTLB request 和保存 live entry 的运行态。 | `dispatch_l2tlb_lookup_active` | 此状态为 1 时，adapter 才消费 raw fence。 |
| `no-dispatch` | 当前 testcase 没有 dispatch service 的固定拓扑。 | testcase 合同、`dispatch_l2tlb_lookup_active=0` | 当前只与 `DISABLED/NO_OWNER` 配对；必须保证 `req_valid=0` 且 `tlb_entry_by_key` 为空；raw fence 直接丢弃。 |
| `dispatch topology contract` | 由 testcase/dispatch coordinator 在 service 启动前设置的固定运行态，不是 plusarg、CSR 或 response owner 镜像。 | `dispatch_l2tlb_lookup_active` | `ENABLED + DISPATCH_ACTIVE` 或 `DISABLED/NO_OWNER + NO_DISPATCH`；固定拓扑期间禁止切换。 |
| `adapter raw-fence owner` | dispatch-active 下唯一允许 peek/decode/schedule/pop raw fence 的组件。 | `dispatch_monitor_event_adapter` | L2TLB responder sequence 不读取 `raw_sfence_q`。 |
| `EVENT_SEQ_NONE` | 没有 response-side lifecycle event 的固定哨兵值。 | `memblock_sync_pkg::MEMBLOCK_L2TLB_EVENT_SEQ_NONE=0` | `DISABLED/NO_OWNER + NO_DISPATCH` 不建 response history/barrier；被丢弃的 raw fence 只能携带该值。 |
| `owner-side admission seal` | parent 的 global stop 后，L2TLB owner 在真实 `drv_cb` 结算此前 ready 窗口的 fire，写 admission-settled watermark 和当前 epoch close request，并生成 `RELEASE_STOP/ready=0`。 | `l2tlb_owner_admission_settled_sample_seq`、timing plan 的 `close_l2tlb_admission_for_release()` | adapter 只并行 drain raw fence，不写 admission state，也不能用 C4 delete 替代 stop item。 |
| `post-reset baseline` | reset release 后当前 epoch 先完成的一次 `NORMAL/inactive` transport sample；它是 release item 可以重新出现前的最小 driver 基线。 | timing plan 的 driver local `post_reset_baseline_pending` 与 done proof | baseline 前当前 epoch 的 stop/final item 是 lifecycle producer 错误，不能由 adapter 或 driver 静默解释。 |
| `item_done()` transport release | stale item 已经从 UVM sequencer 取得后，用于结束该 item 的 transport 握手的标准调用。 | `L2tlb_agent_agent_driver` | reset/stale item 先 `item_done()` 再丢弃，避免 sequencer 保留阻塞 item；本 plan 不直接调用它。 |
| `raw-fence intake close` | fence monitor 在已经完整采样 close request 之后的一个 raw sample 后，停止接收新的 raw fence；它与当前 reset epoch 和 close generation 绑定。 | `l2tlb_raw_fence_producer_settled_sample_seq`、`l2tlb_raw_fence_intake_closed_*` | C10 owner close 后，monitor 先处理 C11 最后一条可能由 stop 前驱动的 raw，再在 C11 关闭；C12 valid raw 必须 fatal。 |
| `FENCE ack` | fence monitor 已完成当前 runtime reset 中自身 producer settled/intake-close/context-dedup 清理的 epoch 确认。 | `l2tlb_fence_reset_ack_epoch`、`reset_required_ack_mask` | 它不表示 raw FIFO 已清空；adapter ack 仍独立证明 raw/context/pending/live 已清。 |
| `MONITOR ack` | L2TLB monitor 同步消费匹配 reset-active transport sample 后的 reset tuple；它不能被 FENCE/CSR ack 或 final settled 代替。 | `l2tlb_monitor_reset_ack_epoch/transport_sample_seq`、timing plan `monitor_reset_ackable()` | 所有 topology 的 runtime reset 都要求该 ack；本 stage plan 不直接写它。 |
| `sample mailbox` | driver 与唯一 response owner 的单槽 frozen transport sample 传递；monitor 以同步 analysis imp 读取同一 wrapper，不形成第二个 FIFO。 | `L2tlb_agent_agent_sequencer` 的 `EMPTY -> PUBLISHED -> CONSUMED/DROPPED -> EMPTY` | stage matcher/adapter 不读写 slot；owner ack 后由 driver 在后续 `drv_cb` 唯一回收。 |
| `final mailbox recycle proof` | final sample 已被 owner 终态确认后，driver 在下一真实 `drv_cb` 回收单槽 mailbox 的证明。 | `l2tlb_release_final_inactive_transport_sample_seq`、`l2tlb_transport_sample_recycle_done_seq` | release 要求两个 seq 相等且 mailbox 已为 `EMPTY`；adapter queue 空不能替代它。 |
| `direct writer` | 对一个运行期状态直接清理并写 ack/proof 的唯一职责组件；coordinator 只发请求和等待。 | driver、L2TLB monitor、fence monitor、adapter、CSR monitor、response owner | 本 plan 的 fence/adapter 只能清其所属对象，不得替 driver/monitor 写 transport proof 或 ack。 |
| stale drop / future fatal | 旧 reset epoch 的 raw/context/pending work 只能记录后丢弃；大于 current epoch 的 work 属于不可能的提前发布，必须报错。 | `raw.reset_epoch`、`context_reset_epoch`、pending record `reset_epoch` | 防止 reset 前 C4 work 在 reset 后重新绑定或删除新 epoch entry。 |
| `entry build provenance` | live entry 创建时冻结的 stage-active、S1/S2 mode/root、CSR context sequence 与 generation。 | `s1_stage_active/s2_stage_active`、`s1_translation_mode_at_build/s2_translation_mode_at_build`、`s1_root_ppn_at_build/s2_root_ppn_at_build`、`csr_context_seq_at_build/entry_generation` | CSR 后续切到 Sv48 不得改变一个既有 Sv39 entry 的 level-3 mask。 |
| `architecture matcher` | 按 RISC-V 的目标 stage、地址范围和 ID 语义做最小失效范围匹配。 | 新的 `sfence_match_entry()` 主路径 | 1 GiB S1 映射与 4 KiB S2 映射分别按各自 level 比较。 |
| `V2 widening` | 仅为复现 V2 local DTLB 保守 over-fence 而额外扩大地址匹配范围的独立层。 | 后续明确的 V2 profile 分支 | V2 `HFENCE.VVMA` 可忽略非 x0 `rs1` 地址，但仍不能跨 stage 或 VMID。 |
| `rs1=x0` / `rs2=x0` | fence instruction 的源寄存器编号为 x0；分别表示全部地址与全部 ASID/VMID，不是 operand 数值为 0。 | `raw.rs1` / `raw.rs2`，解码为 `ignore_addr` / `ignore_id` | 非 x0 寄存器中恰好携带数值 0 仍是精确地址/ID 匹配。 |

本 plan 中修改或新增的关键函数职责如下。详细控制流在后续章节展开。

| 函数/task | 抽象功能描述 |
|---|---|
| `csr_ctrl_agent_agent_monitor::mon_data()` | 每个 post-reset DUT sample 发布可供 fence 使用的不可变 CSR context；它不删除 TLB entry，也不消费 fence FIFO。 |
| `fence_agent_agent_monitor::mon_data()` | 采样有效 fence，并以本 sample 的 context 构造 raw fence；它不按当前 CSR 自行解释 fence。 |
| `memblock_sync_pkg::publish_sfence_csr_context()` | 协调 CSR monitor 与 fence monitor 的同拍发布顺序，把同一 sample 的 context 绑定到 raw fence。 |
| `memblock_sync_pkg::push_raw_sfence()` / `peek_raw_sfence()` | 前者仅在 dispatch-active 时保留 FIFO item 并完成 context 绑定；后者供 adapter 在 schedule 成功前查看队首。 |
| `memblock_sync_pkg::close_dispatch_raw_fence_intake_for_release()` | 由 fence monitor 在完整处理 close 后的一个 raw sample 后，写当前 epoch/generation 的 producer-close 证明；它不消费 FIFO。 |
| `memblock_dispatch_base_sequence::collect_runtime_context_events()` | 每个 dispatch service sample 只同步 CSR runtime；不得消费 raw fence。它把 raw fence service 交给下面的 adapter 唯一入口。 |
| `memblock_main_dispatch_auto_build_main_table_base_sequence::service_monitor_once()` | 在 CSR runtime 同步完成后，每个 dispatch sample 只调用一次 adapter 的 raw-fence service，再继续其它 monitor batch；不直接调用旧 fence helper。 |
| `dispatch_monitor_event_adapter::service_l2tlb_sfence_events()` | dispatch-active 下的唯一 raw-fence service 入口；顺序调用 drain、schedule 和 C4 due apply，保证不存在第二个 destructive consumer。 |
| `dispatch_monitor_event_adapter::drain_l2tlb_sfence_events()` | dispatch-active 下唯一按 FIFO 顺序取得已经绑定 context 的 raw fence，并登记 live-entry 删除；未绑定 context 的队首不会被丢弃。 |
| `common_data_transaction::decode_raw_sfence()` | 把 raw 字段和冻结 context 规范化为带目标 stage 的内部 payload。 |
| `common_data_transaction::sfence_match_entry()` | 先选目标 stage，再用该 entry 冻结的 stage mode 调用对应 S1 或 S2 地址 matcher，最后判断 ASID/VMID；不读取当前 `mmu_csr_state`。 |
| `common_data_transaction::validate_frozen_stage_level()` | 校验 entry 的 stage-active、创建时 translation mode 与 level 组合；只读 entry provenance，结构不一致时报告 fatal。 |
| `common_data_transaction::schedule_sfence_invalidate()` | 将已解码 fence 登记为 C4 到期的 live-entry 删除工作；它不在 monitor 观察拍删除 entry。 |
| `common_data_transaction::apply_due_sfence_invalidate()` | 只在已经到达 filter flush 边界时扫描 live table 并统一删除命中 entry；它不修改 UID record、main table 或 pending response snapshot。 |
| `common_data_transaction::clear_dispatch_l2tlb_live_entries()` | dispatch adapter 在自己清空 adapter pending work 后批量清 canonical entry 与 range index；reset coordinator 只请求/等待 adapter ack。它不处理 token 或 UID。 |
| `memblock_l2tlb_base_sequence::release_l2tlb_lifecycle_owner()` | 通过内部 release-safe 检查后消费 parent grant 并清 active claim；`prepare_l2tlb_lifecycle_owner_release()` 只是内部检查 helper，raw fence/invalidate queue 由 adapter 独立收敛。 |

## 范围、前置字段与不变边界

### 1. 现有问题和修改目标

当前 `common_data_transaction::sfence_match_entry()` 使用 `key.vpn`、共享 `entry.level` 与共享 `entry.pte_g`。
这会把请求侧 GVA/VPN 错当成 G-stage GPA/GVPN，并在 `allStage` 下把一套组合 level 用于两类 fence；同时它在
`hv=1` 路径读取消费时的 `mmu_csr_state.hgatp_vmid`，CSR 切换后可能按错误 VMID 删除 entry。

本 plan 将匹配真源改为已经在
`AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2tlb_response_random_payload_plan_20260729.md` 规定的两套字段：

```text
S1 matcher：s1_tag / s1_level / s1_pte_n / s1_valididx[] /
    s1_asid / s1_vmid / s1_pte_g / s1_stage_active /
    s1_translation_mode_at_build
S1 payload consistency：s1_addr_low / s1_ppn_low[] / s1_pteidx[]
S2：s2_tag / s2_level / s2_pte_n / s2_vmid /
    s2_stage_active / s2_translation_mode_at_build
共同 provenance：s1_root_ppn_at_build / s2_root_ppn_at_build /
    csr_context_seq_at_build / entry_generation
```

上述 S1/S2 entry 字段、`copy_from()` 与 UID record 的迁移是本 plan 的前置条件。`s1_addr_low`、
`s1_ppn_low[]` 与 `s1_pteidx[]` 的 build/copy/drive 一致性由 random payload plan 的专用 helper 检查；
`sfence_match_entry()` 及其 range helper 不读取这些字段，payload 异常不得伪装成“地址不命中”。
`*_stage_active` 只标识由 `s2xlate` 选择的 response stage；inactive stage 不构造 stage payload。active stage
必须是本 framework 支持的 paged mode；若已存在 entry 的 active stage frozen mode 为 Bare，则这是输入合同破坏，
range helper 立即 `uvm_fatal`，不得把它当作 inactive 或以 level=0 继续匹配。
mode/root/context sequence/generation 都不加入 lookup key，不触发自动清表，也不替代既有 flush lifecycle；它们
只用于构造期 provenance、range helper、日志与同 key 重建追溯。本 plan 不重新定义 response payload 随机、PPN
生成、permission、PBMT 或 L2TLB request lookup key。entry 中已有的 `priv_mode` 只表示执行/特权上下文，不能
代替 S1/S2 translation mode，也不能用于 level-3 合法性或 fence mask。

| 文件 | 本专项修改职责 |
|---|---|
| `mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv` | 扩展 raw fence 的 `reset_epoch`/context epoch 字段；实现同 sample/context epoch 绑定、`peek_raw_sfence()`、raw-fence intake close/query 和 reset 请求/ack 协同。 |
| `mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_monitor.sv` | 每个 post-reset sample 发布 fence context。 |
| `mem_ut/ver/ut/memblock/agent/fence_agent_agent/src/fence_agent_agent_monitor.sv` | 为有效 fence 写入 sample provenance，并继续经已有 raw FIFO 入口发布；global stop 后负责在完整 raw sample 边界写 intake close。 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv` | 增加目标 stage 枚举和携带冻结 context/S1 VPN/S2 GVPN 的内部 fence payload 字段。 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv` | dispatch-active 下唯一消费 `raw_sfence_q`，负责 decode、C4 schedule 与 due live-entry delete。 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv` | `collect_runtime_context_events()` 只保留 CSR drain，移除旧 `drain_sfence_events()` 调用，不能成为 raw fence consumer。 |
| `mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_auto_build_main_table_base_sequence.sv` | 在 dispatch service sample 中调用一次 `service_l2tlb_sfence_events()`；这是 adapter raw-fence service 的唯一调度入口。 |
| `mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv` | 不读取、pop、schedule 或 apply raw fence；仅维护 request token、response、UID 与 C4 cancel。no-dispatch 观察到 request fire 时只执行合同断言并 `uvm_fatal`，不建 token/live entry。 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_tlb_entry.sv` | 持有 S1/S2 build provenance；`copy_from()` 与 `copy_entry_fields()` 逐字段复制 mode/root/context/generation。 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv` | 重写 decode、S1/S2 range helper、stage-aware matcher 与 live entry 删除调用。 |

### 2. 保持不变的生命周期

```text
dispatch-active：
  fence monitor
    -> raw_sfence_q FIFO
    -> dispatch_monitor_event_adapter::service_l2tlb_sfence_events()（每个 dispatch sample 一次）
       -> drain_l2tlb_sfence_events()
       -> decode_raw_sfence()
       -> schedule_sfence_invalidate()
       -> sfence_invalidate_pending_q（携带 raw reset_epoch，到 C4）
       -> apply_due_sfence_invalidate()
    -> tlb_entry_by_key 删除命中的 live entry

no-dispatch：
  保证 req_valid=0 且 tlb_entry_by_key 为空；不能只检查 ready=0 导致的 req.fire=0
    -> raw fence 不入 FIFO，直接丢弃
    -> 不执行 raw matcher、schedule、C4 live-entry delete 或 owner-handoff reconcile
    -> 若 L2TLB sequence 观察到 valid && ready，则立即 uvm_fatal，不建立 token、pending snapshot 或 live entry

fence/CSR monitor：
  -> 继续发布 L2TLB token lifecycle 所需的 flush sideband
```

以下行为不修改：

- `raw_sfence_q` 继续是 FIFO，不能降级为 latest snapshot；但只有 `dispatch_l2tlb_lookup_active=1` 时才入队。
- `dispatch_l2tlb_lookup_active` 由 testcase/dispatch coordinator 在启动 dispatch service 前写入并在本 testcase 内保持不变：
  dispatch service 为 1，standalone/no-dispatch 为 0。它不是 plusarg、`dispatch_monitor_capture_en`、
  `l2tlb_responder_active` 或 `l2tlb_lifecycle_owner_claimed` 的别名；monitor 采样和 L2TLB token flush sideband
  仍按各自既有规则运行。
- `collect_runtime_context_events()` 继续同步 `drain_csr_events()` 供后续 request 使用，但必须删除其中的旧
  `drain_sfence_events()` 调用；它不是 raw fence consumer。dispatch-active 下由
  `memblock_main_dispatch_auto_build_main_table_base_sequence::service_monitor_once()` 每个 sample 只调用一次
  `dispatch_monitor_event_adapter::service_l2tlb_sfence_events()`，adapter 是 raw fence 的唯一 consumer。L2TLB
  lifecycle sequence 不再触碰该 FIFO。
- 只删除 `tlb_entry_by_key`。`main_table_by_uid`、`status_by_uid`、`uid_tlb_record_by_uid` 不删除。
- 已冻结的 pending response snapshot 不因 live entry 删除而被覆盖；是否取消仍由 L2TLB flush lifecycle 决定。
  同一 C4 sample 中 token/UID cancel 与 live-entry delete 由各自 owner 独立完成，不能互相调用或依赖 UVM
  service 先后顺序。
- `flushPipe` 保持 fence transaction 到 DUT 的透明驱动与 X/Z 诊断，不进入 raw fence payload，也不参与 stage 匹配。
- 同 key 在 entry 存活时复用原 payload；被匹配删除后，后续 request 才允许重新建 entry 并重新随机。
- no-dispatch 是不建表的固定测试拓扑，不支持 `dispatch -> no-dispatch -> dispatch` 的隐式复用。运行中改写
  `dispatch_l2tlb_lookup_active` 必须 `uvm_fatal`；本 plan 不实现 topology transition，也不建立 L2TLB owner
  inactive-gap/high-watermark/reconcile 机制。
- `entry_generation`、`csr_context_seq_at_build` 与 S1/S2 mode/root 只随 live entry 快照和删除日志传递；它们不
  改变 pending response 是否发送、取消记账或 ready hold 的既有生命周期决定。
- 本 plan 不新增 DCache owner、最终 PA、`final_paddr_valid` 或 permission validity 消费逻辑。
- UID `WAITING` 实例属于 L2TLB lifecycle 的 release-safe 条件：owner 释放或被 kill 前，所有等待实例必须已经在
  C4/reset cancel 中转为 `CANCELED`，或在安全 response complete 中转为 `COMPLETED`；不能只检查 token queue 为空。
  统计范围固定为整个 `uid_tlb_record_by_uid` 中 `record_valid=1 && state==WAITING` 的 record，不按 owner 名称、
  token、key 或 `pte_valid` 过滤，也不允许把未完成 record 转交给下一 owner。
  `phase_ended()` 只检查并报告 owner 状态，不能驱动最后 idle、不能绕过 sequence 的 release gate；若 owner 仍
  持有未收敛工作，必须报告 lifecycle fatal 并保留 claim。只有正常 global-stop 主循环可完成 final inactive/closing/release。

## Raw Fence 与采样上下文 Flow

### 1. 字段和单一发布者

`dispatch_raw_sfence_t` 保留现有 `valid/rs1/rs2/addr/id/hv/hg/cycle`，新增以下冻结字段：

| 字段 | 写入者 | 消费者 | 作用 |
|---|---|---|---|
| `sample_seq`、`sample_time` | fence monitor，读取 CSR monitor 已推进的 `peek_current_dut_global_sample()` | context 协调、日志、adapter raw-fence drain | 标识 fence 真实采样点。 |
| `reset_epoch` | fence monitor 从 shared lifecycle 当前 epoch 冻结；CSR context 同时保存同值 | context bind、adapter schedule/apply、reset 清理和 release gate | raw fence、待绑定 context 和 C4 invalidate work 只能在同一 runtime reset epoch 内关联；旧 epoch 不能删除新 entry，未来 epoch 直接 fatal。 |
| `lifecycle_event_seq` | dispatch-active 时 fence monitor 从 `note_l2tlb_flush_event(..., FENCE)` 的非 0 返回值冻结；`DISABLED/NO_OWNER + NO_DISPATCH` 返回 `memblock_sync_pkg::MEMBLOCK_L2TLB_EVENT_SEQ_NONE=0`，且 raw 会被丢弃。 | adapter schedule、C4 delete 日志 | 作为 raw fence 与 response lifecycle event 的 provenance；adapter 不用它读取或消费 event history，C4 anchor 仍取 raw 的 `sample_seq`。 |
| `context_valid`、`context_reset_epoch` | `memblock_sync_pkg` context 协调器按当前 sample/epoch 绑定 | adapter raw-fence drain、decode | 表示 raw fence 已绑定同 sample、同 reset epoch 的 CSR context；epoch 不一致不能补绑定。 |
| `priv_virt_at_sample` | CSR monitor 经协调器冻结 | decode | 区分普通 `SFENCE.VMA` 是 HS/S-stage 还是 VS/S1-stage。 |
| `hgatp_vmid_at_sample` | CSR monitor 经协调器冻结 | VS matcher | `SFENCE.VMA(V=1)` 与 `HFENCE.VVMA` 的隐含 VMID。 |
| `satp_mode_at_sample`、`vsatp_mode_at_sample`、`hgatp_mode_at_sample` | CSR monitor 经协调器冻结 | debug、失败日志 | 保留本次 fence 的翻译 mode provenance，不从当前 CSR 重推。 |
| `csr_sample_seq` | CSR monitor 经协调器冻结 | debug、一致性检查 | 标识产生该 context 的 CSR sample。 |

不在 raw fence 中复制 `flushPipe`，也不新增当前 CSR 的可变 handle。raw event 中的 context 字段是唯一的
fence 解释真源。`raw.reset_epoch` 与 `context_reset_epoch` 必须在绑定、排程和删除前使用 4-state/整数一致性检查，
不得把旧 epoch raw 重新绑定到新 epoch context。`csr_sample_seq` 必须来自 CSR monitor 已在本拍通过
`advance_dut_global_sample()` 建立的逐 sample 序号，并与 raw 的 `sample_seq` 相等；该 helper 只由 CSR monitor 调用，
fence monitor 和 adapter 只能读取 `peek_current_dut_global_sample()`；不得复用只在 CSR payload 改变时递增的
`runtime_csr_snapshot_seq`。
raw 中冻结的 `satp/vsatp/hgatp.mode_at_sample` 只用于 fence provenance、debug 与一致性诊断；它们不覆盖
live entry 的 `*_translation_mode_at_build`，也不参与既有 entry 的 level/mask 选择。

同步包接口保持最小变更：扩展现有 `push_raw_sfence(raw)` 处理同 sample、同 reset epoch context 的立即绑定或等待绑定；新增
`peek_raw_sfence(raw)` 查看队首，保留现有 `pop_raw_sfence(raw)` 只在
`schedule_sfence_invalidate()` 成功登记 C4 work 后调用。runtime reset 的 CSR monitor reset helper 是
`l2tlb_sfence_csr_context` 的唯一清除者；adapter reset helper 只清 `raw_sfence_q`、adapter drain proof、
pending invalidate 与 live-entry/range-index，fence monitor reset helper 只清本地 producer watermark/intake-close。
这样旧 context 不会关联到新 fence，且没有两个 direct writer 竞争同一 context；`clear_raw_monitor_queues()` 不得清 global sample。

`dispatch_l2tlb_lookup_active` 是 testcase topology state，须由 testcase-start coordinator 在首个 post-reset monitor
sample 前通过一个最小 package setter 固定写入；初始未设置或之后被改写均为 `uvm_fatal`。本轮固定 topology，不实现
`dispatch -> no-dispatch -> dispatch` transition，也不为未来 transition 保留本专项可执行路径。该 state 不是
raw monitor queue 的内容，`clear_raw_monitor_queues()` 不清零它：同一 testcase 的 reset 前后仍保持 dispatch-active
或 no-dispatch 的原拓扑。运行中任何 topology 改写均为 fatal；本 plan 不给 future transition 提供执行路径。

```text
set_dispatch_l2tlb_lookup_active(active, topology_name):
  若 topology_state_valid 且 active 与已保存值不同：uvm_fatal；固定拓扑不得切换。
  保存 active、topology_name，并置 topology_state_valid=1。

push_raw_sfence(raw):
  若 !topology_state_valid：uvm_fatal；monitor 在拓扑未确定时采样属于 testcase 启动错误。
  若 !raw.valid：return。
  若 raw.reset_epoch < current_reset_epoch：记录 stale raw 并丢弃，不进入 FIFO；若 raw.reset_epoch > current_reset_epoch：uvm_fatal。
  若 !dispatch_l2tlb_lookup_active：直接丢弃 raw。
  若 l2tlb_raw_fence_intake_closed=1：uvm_fatal；global-stop release 已封闭 raw intake 后不能再静默入队新 fence。
  否则按同 sample、同 reset_epoch context 绑定规则入 raw_sfence_q；绑定 epoch 不一致时不得等待到未来再重绑，直接丢弃旧 raw 或 fatal future raw。
```

`common_data_transaction` 的 live-entry 删除只在 dispatch-active 期间由 adapter 的
`apply_due_sfence_invalidate()` 推进。no-dispatch 不建立 live entry，因此不保存 L2TLB owner inactive-gap
high-watermark，也不建立跨 owner reconcile 状态。

runtime reset 的唯一顺序由 shared lifecycle plan 按 ack mask 定义：存在 response owner 时先停止 admission 并由 response owner
独占 cancel token/driving/barrier/全部 WAITING UID；dispatch adapter 独占清 raw fence、待绑定 context、pending invalidate，并在 dispatch-active
时调用 `clear_dispatch_l2tlb_live_entries("runtime reset")` 删除 canonical entry/range index；fence monitor 独占清
`raw_fence_producer_settled_sample_seq`、intake-close active metadata 和 fence-local context/dedup；
reset coordinator 只发起请求并等待 adapter/fence ack，不能直接清上述 adapter/fence-owned queue/map。CSR monitor 独占使 CSR history、sample producer done mask
和当前 event-ready watermark 无效。该流程不
release 或重新 claim owner，也不清 global sample 或 fixed topology state。普通 `clear_raw_monitor_queues()` 不得单独
替代这一完整 reset 收敛顺序。

在 `DISABLED/NO_OWNER + NO_DISPATCH` 下，response owner 和 adapter 不存在，reset coordinator 仍请求 fence monitor、CSR monitor 与 L2TLB monitor 清理并等待
FENCE/CSR/MONITOR ack；fence monitor 清自己的 producer/context 状态，CSR monitor 独占执行 CSR history/context 清理，L2TLB monitor
消费 passive driver 的 reset-active sample 后回 MONITOR ack。response/adapter ack 按 shared
`reset_required_ack_mask` 标记 N/A，不能等待不存在的 sequence，也不执行
owner re-arm。CSR/fence monitor 的 sample producer barrier 仍按每拍合同完成，不能因为没有 raw FIFO consumer 而跳过 watermark。

### 1.1 Fence event 与 CSR change event 的边界

Fence 指令不在 CSR 中。CSR monitor 发布的是翻译上下文或 CSR change event，fence monitor 发布的是实际
`SFENCE/HFENCE` 指令 event；`publish_sfence_csr_context()` 只负责把同 sample 的 CSR 快照绑定给 raw fence，
不改变二者的来源或 raw 数据。仅在 L2TLB token lifecycle 层，V2 的 filter flush 条件对二者使用同一个
`sfence.valid || CSR changed` 管线，因此同 sample 可以合并为一个 barrier。

```text
CSR monitor 观察 satp/vsatp/hgatp/priv_virt changed
  -> note_l2tlb_flush_event(sample_seq, sample_time, CSR_CHANGE)
     ENABLED + DISPATCH_ACTIVE：分配/合并 response history event
     DISABLED/NO_OWNER + NO_DISPATCH：只记录本拍 reason 并返回 `memblock_sync_pkg::MEMBLOCK_L2TLB_EVENT_SEQ_NONE`，不创建 history

fence monitor 观察 sfence.valid
  -> event_seq = note_l2tlb_flush_event(sample_seq, sample_time, FENCE)
  -> ENABLED + DISPATCH_ACTIVE：raw.lifecycle_event_seq = event_seq，push_raw_sfence(raw)
  -> DISABLED/NO_OWNER + NO_DISPATCH：不创建 response history；raw 不入 FIFO，event_seq=`memblock_sync_pkg::MEMBLOCK_L2TLB_EVENT_SEQ_NONE` 只可用于本拍诊断
```

在 `ENABLED + DISPATCH_ACTIVE` 下，当两类 event 的 `sample_seq` 相同时，`note_l2tlb_flush_event()` 复用 event history 中该 sample 的
`event_seq/sample_seq/sample_time`，并将 `reason_mask` 做 OR：`CSR_CHANGE | FENCE`。该 event 只建立一个 barrier，
只有不同 sample 才递增 event sequence 并建立新 barrier。重复加入同一 reason 表示 monitor 重复发布，必须
`uvm_fatal`；不同 reason 的 OR 不丢失来源，并在日志中同时输出。`DISABLED/NO_OWNER + NO_DISPATCH` 不建 response barrier/history，
只完成 per-sample reason/watermark 发布。raw fence FIFO 仍只保存完整 fence 本身，CSR
change event 不进入该 FIFO；因此 fence 的
addr/ASID/VMID 精确 live-entry matcher 不会被 CSR change 替代或吞掉。

同一 sample 的 reason 合并必须经过 sample producer barrier：CSR monitor 和 fence monitor 都通过
`mark_l2tlb_sample_producer_done(sample_seq, producer_kind)` 报告“本 sample 已采样”，即使本拍没有对应 event 也要
报告完成；同步包在 required producer mask 收齐后才写
`lifecycle_event_published_seq=sample_seq`。L2TLB owner、adapter 和 negedge service 在该标志发布前不得消费本 sample
的 event 或据此建立 C0 barrier，避免 fence monitor 晚于 CSR monitor 时漏掉 `FENCE` reason。

### 2. `publish_sfence_csr_context()`

**抽象功能描述：** `publish_sfence_csr_context()` 由 CSR monitor 在每个 post-reset sample 调用，发布该 sample
的不可变 context，并解决 CSR monitor 与 fence monitor 同拍执行顺序不确定的问题。它不更新
`common_data_transaction::mmu_csr_state`，也不从 FIFO 删除 event。

**目的：** 现有 `runtime_csr_snapshot` 只保留 latest 且只在 payload 改变时递增，不能代表一条已排队 fence 的
执行时上下文。该 helper 为有 fence 的 sample 绑定一份不可变 context，同时不维护按周期无限增长的 CSR 历史表。

**输入：** 当前 `dispatch_raw_csr_t`、由 CSR monitor 在本拍调用 `advance_dut_global_sample($time)` 后得到的
`sample_seq`、当前 shared `reset_epoch`、`$time`。该 helper 自身不推进 sample；如果由非 CSR monitor 传入未建立的
sample 或传入 epoch 不是 current epoch，必须 `uvm_fatal`。

**输出/副作用：** 发布当前 sample 的临时 context；若同 sample 的 raw fence 已在等待，则补齐其 context 字段并置
`context_valid=1`。只保留当前 sample context 和与 `raw_sfence_q` 一一对应的待绑定项，raw event pop 后不保留
额外 history。

```text
publish_sfence_csr_context(raw_csr, sample_seq, reset_epoch, sample_time):
  确认 raw_csr.valid 且 sample_seq/sample_time 单调合法。
  确认 reset_epoch == current_reset_epoch 且 reset_active=0；否则旧 epoch context 只能丢弃并记录，未来 epoch 直接 uvm_fatal。
  从 raw_csr 提取 priv_virt、hgatp_vmid、satp/vsatp/hgatp mode 和 CSR sample 序号。
  将它登记为 `{sample_seq, reset_epoch}` 对应的 immutable context；同一 sample/epoch 重复发布必须幂等或 fatal，不能覆盖不同 payload。
  若该 sample 已有等待 context 的 raw fence：
    只有 raw.reset_epoch == reset_epoch 才把上述字段复制到该 raw fence；置 context_valid/context_reset_epoch。
    epoch 不一致的 raw 只能丢弃（旧）或 fatal（未来），不得跨 reset 重新绑定。
  不修改 raw_sfence_q 的 FIFO 顺序，不删除任何 TLB entry。
```

中文文字伪代码：CSR monitor 每拍都先取得本拍统一 sample 序号，再把本拍真实 CSR 值交给协调器；这一步独立于
semantic raw capture gate 和 CSR payload 是否变化。若 fence monitor 已先把同拍 fence 放入 FIFO，协调器只补齐
该 item 的冻结字段，不能以更新后的下一拍 CSR 代替。若本拍没有 fence，协调器仅保留当前 sample 的短暂 context，
不会累积每拍一份长期历史。

### 3. `fence_agent_agent_monitor::mon_data()`

**抽象功能描述：** `mon_data()` 在有效 fence sample 构造 raw event，并把它交给同步包绑定同 sample CSR context
后入 FIFO。它继续产生现有 L2TLB lifecycle flush sideband，但不自行判断 HS/VS/G-stage；global stop 期间它还负责在完整
处理一个 close 后 raw sample 后封闭新的 raw intake，adapter 仍独占 FIFO drain 与 C4 delete。

**输入：** `io_ooo_to_mem_sfence_valid/rs1/rs2/addr/id/hv/hg`，以及当前 `$time`。

**输出/副作用：** 产生带 `sample_seq/sample_time/reset_epoch` 的 `dispatch_raw_sfence_t`；仅在 dispatch-active 时 context
成功绑定后 item 留在 `raw_sfence_q`，等待 adapter 消费。no-dispatch 时不入 FIFO。

```text
mon_data() 的每个 post-reset sample：
  先等待 wait_for_l2tlb_sample_anchor($time)，消除 CSR/fence monitor 同拍调度先后；若在该 sample 的 NBA/发布窗口结束后
  仍没有 CSR monitor 建立的 anchor，则 uvm_fatal，不自行推进 sample。
  sample_seq = peek_current_dut_global_sample()；sample_time = $time；reset_epoch = peek_current_reset_epoch()。
  若 dispatch_l2tlb_lookup_active=1 且 l2tlb_raw_fence_intake_closed=1：
    若 sfence.valid：uvm_fatal；release 已封闭 raw intake，不能先 note event 再留下无人 drain 的 raw。
    跳过 raw/event 创建，但仍执行本 sample 的 producer-done 与 settled watermark。
  否则若 sfence.valid：
    event_seq = note_l2tlb_flush_event(sample_seq, sample_time, FENCE)；若同 sample 已有 CSR_CHANGE，只 OR 到同一 lifecycle event。
    建立空 raw fence，复制 rs1/rs2/addr/id/hv/hg、service cycle、sample_seq、sample_time 和 reset_epoch。
    将 event_seq 写入 raw.lifecycle_event_seq。
    调用 push_raw_sfence(raw)：
      若 dispatch_l2tlb_lookup_active=0：丢弃 raw，不建立 FIFO item；同时要求 event_seq==memblock_sync_pkg::MEMBLOCK_L2TLB_EVENT_SEQ_NONE，
        因为 `DISABLED/NO_OWNER + NO_DISPATCH` 不得分配 response history record。
      若 dispatch_l2tlb_lookup_active=1 且当前 sample context 已发布：立即复制 context 并入 FIFO；
      若 dispatch_l2tlb_lookup_active=1 且 CSR monitor 尚未发布同 sample context：按该 sample 暂存等待绑定，FIFO 顺序不变。
    不读取 mmu_csr_state，不将 priv_virt 默认写为 0，不把 flushPipe 写入 raw。

mon_data() 的每个 sample 收尾：
  不论 sfence.valid 是否为 1，调用 mark_l2tlb_sample_producer_done(sample_seq, FENCE_PRODUCER)，报告 FENCE producer 已完成。
  在本 sample 的 raw 处理与 producer-done 调用后，由 fence monitor 写 l2tlb_raw_fence_producer_settled_sample_seq=sample_seq。
  若 dispatch-active、close_requested=1、close_request_reset_epoch==current_reset_epoch、
     sample_seq > close_request_sample_seq 且 raw-fence intake 尚未 closed：
    调用 close_dispatch_raw_fence_intake_for_release(sample_seq)，将 closed/reset_epoch/generation/cutoff 绑定到当前 close。
    该调用只在本 monitor 已完整接收本 sample raw 后发生，不能由 parent、adapter 或 responder sequence 代写；
    close proof 同时冻结 current reset_epoch/current close_generation。
  CSR monitor 与 fence monitor 的 required producer mask 收齐后，才允许同步包发布 lifecycle_event_published_seq。
本拍没有 CSR_CHANGE/FENCE 时仍发布该 sample 的 event-ready watermark，但不创建空 raw/event history record；
`DISABLED/NO_OWNER + NO_DISPATCH` 下无 response history consumer，producer 只发布本拍 reason/watermark，不分配或追加 event history record，
也不直接回收该队列；adapter 不维护 lifecycle history cursor，只在 dispatch-active 时消费 raw fence FIFO。
```

中文文字伪代码：valid 明确为 1 时，monitor 先保留原有的 L2TLB flush 事件通知，再为本 fence 记录统一 sample
序号。同步包负责消除两个 monitor 的执行先后差异：CSR 已先到则直接绑定，CSR 后到则等待同 sample 的发布；两种
顺序最终得到同一份 raw context。这里的“绑定 context”不等于合并 raw 数据；仅 L2TLB lifecycle event 的
`reason_mask` 可以同时含 `CSR_CHANGE|FENCE`。若 context 永远未到，L2TLB raw-fence drain 会按失败策略报错，而不是把 fence 静默当成
`priv_virt=0` 的普通 SFENCE。CSR context 发布不受 `dispatch_monitor_capture_en` 限制，
`note_l2tlb_flush_event()` 也继续保持 L2TLB token lifecycle 的非破坏 sideband 语义。当前 testcase 合同规定
`dispatch_monitor_capture_en` 恒为 1，但它不是 raw FIFO 的 consumer gate；`push_raw_sfence()` 的入队条件为
`item.valid && dispatch_l2tlb_lookup_active`。当前 no-dispatch 只与 `DISABLED/NO_OWNER` 配对；monitor 必须保证 `req_valid=0`、
没有 live entry，故 raw fence 直接丢弃，
不需要由 standalone responder drain 或在后续 testcase phase 重新解释。

### 4. `dispatch_monitor_event_adapter::service_l2tlb_sfence_events()` / `drain_l2tlb_sfence_events()`

**抽象功能描述：** `service_l2tlb_sfence_events()` 是 dispatch service 每个 sample 的唯一调度入口；它先调用
`drain_l2tlb_sfence_events()` 登记新 C4 work，再调用 `apply_due_sfence_invalidate()` 处理已经到期的 work。
`collect_runtime_context_events()`、L2TLB response sequence 以及任何其它 monitor helper 都不能调用旧的直接
消费路径。旧 `dispatch_monitor_event_adapter::drain_sfence_events()` 与
`common_data_transaction::apply_raw_sfence()` 必须删除/废弃；现有立即删除 API
`common_data_transaction::apply_sfence_invalidate()` 也必须改为 `apply_due_sfence_invalidate()` 的内部删除路径，
不能保留任何可从 monitor/sequence 直接调用的 C0 immediate-delete 入口。

```text
memblock_main_dispatch_auto_build_main_table_base_sequence::service_monitor_once():
  tick_dispatch_service_cycle()
  collect_runtime_context_events()       // 只 drain_csr_events()
  monitor_adapter.service_l2tlb_sfence_events()  // 本 sample 唯一 raw-fence 调用
  继续 drain_lsq_timing_sidebands、monitor batch、replay 与 `service_lsq_timing_reconcile()`；该 reconcile 只属于
  LSQ cancel/redirect timing sideband，不是 L2TLB owner handoff 或 live-entry reconcile。

dispatch_monitor_event_adapter::service_l2tlb_sfence_events():
  current_sample = peek_current_dut_global_sample()
  若 current_sample==0：return；等待首个 post-reset sample，不得 pop/schedule/delete。
  若 lifecycle_event_published_seq < current_sample：return；本 sample producer 尚未全部完成，adapter 不得解释或消费 raw。
  若 lifecycle_event_published_seq > current_sample：uvm_fatal；当前 consumer 使用了错误 sample watermark。
  drain_l2tlb_sfence_events()            // 只 schedule，不直接删表
  apply_due_sfence_invalidate(current_sample, peek_current_reset_epoch())  // 只处理当前 epoch 的 C4 due；negedge service 不推进 global sample
```

### 4.1 `dispatch_monitor_event_adapter::drain_l2tlb_sfence_events()`

**抽象功能描述：** 该 adapter helper 是 dispatch-active 下 raw fence 的唯一 consumer。它按 FIFO 顺序取得已经
绑定 context 的 raw fence，登记 C4 到期的 live-entry 删除；它不修改 L2TLB pending token、UID waiting state 或
response driver。

**输入：** `raw_sfence_q` 队首 item、当前 `peek_current_dut_global_sample()`/`peek_current_reset_epoch()`、`dispatch_l2tlb_lookup_active`。

**输出/副作用：** dispatch-active 时成功登记 future invalidate 并 pop 一个 raw fence；context 尚未绑定时保留队首。
no-dispatch 时 FIFO 必须为空，非空即表示拓扑 gate 失效并 `uvm_fatal`。

```text
dispatch_monitor_event_adapter::drain_l2tlb_sfence_events():
  current_sample = peek_current_dut_global_sample()。
  若 lifecycle_event_published_seq < current_sample：return，不得 pop/schedule/due-delete。
  若 lifecycle_event_published_seq > current_sample：uvm_fatal；helper 不得依赖外层 caller 已检查。
  仅 lifecycle_event_published_seq == current_sample 时允许继续 drain。
  若 !dispatch_l2tlb_lookup_active：
      要求 raw_sfence_q 为空；否则 uvm_fatal。
      return。
  while peek_raw_sfence(raw):
    若 raw.reset_epoch < current_reset_epoch：pop 并记录 stale raw，不能交给 decode/schedule；
    若 raw.reset_epoch > current_reset_epoch：uvm_fatal；未来 epoch raw 不能提前进入当前 live table flow。
    若 raw.context_valid 且 raw.context_reset_epoch != raw.reset_epoch：uvm_fatal；context 不能跨 epoch 绑定。
    若 !raw.context_valid：
        若 peek_current_dut_global_sample() == raw.sample_seq：return；
        否则 uvm_fatal，输出 raw sample、当前 sample、hv/hg、addr、id。
    decode_raw_sfence(raw) 后调用 schedule_sfence_invalidate(payload, raw.sample_seq, raw.reset_epoch, raw.lifecycle_event_seq)。
    pop_raw_sfence(consumed)：仅在 schedule 成功后弹出同一队首；pop 失败为 uvm_fatal。

adapter 每个 dispatch service sample：
  只能由 service_l2tlb_sfence_events() 调用一次 drain_l2tlb_sfence_events()；
  再对到期 C4 record 调用 apply_due_sfence_invalidate(dut_sample_seq, peek_current_reset_epoch())。
```

中文文字伪代码：同拍 C0 fence 的 context 若尚未绑定，adapter 可在下一 sample drain，但 due 仍由 raw 自己的
`sample_seq` 计算，不能把 C1 当作 anchor。raw FIFO 只有 adapter 一位破坏性 consumer，因此 full dispatch 不会
竞争 pop。L2TLB sequence 同拍独立处理 token cancel；adapter 删除 live entry 不覆盖 pending snapshot，二者无需
互扫队列。

#### `clear_dispatch_l2tlb_live_entries()`

**抽象功能描述：** 该 helper 仅是一个由 dispatch adapter 在 reset 服务中使用的批量清表工具，用于清除 dispatch-owned live entry/index，
防止 reset 后复用旧随机 payload。当前固定 topology testcase 不因 topology 切换调用它。它不取消 token、不改 UID、不生成 fence event。

```text
clear_dispatch_l2tlb_live_entries(reason):
  要求 dispatch adapter 已清 raw_sfence_q、待绑定 context 与 sfence_invalidate_pending_q；否则 uvm_fatal。
  清空 tlb_anchor_keys_by_range_key 与 tlb_entry_by_key。
  输出 reason 与删除数量。
```

中文文字伪代码：当前 testcase 拓扑固定，因此 no-dispatch 不会建立此表。dispatch adapter 在清空自己的 pending work 后
调用本 helper 删除 canonical entry 和 range index，reset coordinator 只等待其 ack；它不会改变 `dispatch_l2tlb_lookup_active`。任何运行中 topology 改写均为
合同错误，不能用 L2TLB sequence 的 owner handoff 或 high-watermark 补救旧 entry。

## 事件解码与分阶段匹配 Flow

### 1. `decode_raw_sfence()`

**抽象功能描述：** `decode_raw_sfence()` 将已绑定 context 的 raw fence 转为内部 payload，并确定本次失效的唯一
目标 stage。它只做语义规范化，不扫描 TLB 表。

**输入：** `dispatch_raw_sfence_t raw`。

**输出/副作用：** 返回 `memblock_sfence_payload_t`，其中包含 `ignore_addr/ignore_id`、目标 stage、规范化的
S1 VPN 或 S2 GVPN、原始 ID 和冻结 VM context；不修改公共 table。

| raw 条件 | 解码结果 | 允许匹配的 `s2xlate` | 地址和 ID 解释 |
|---|---|---|---|
| `hg=0,hv=0,priv_virt_at_sample=0` | HS/S-stage `SFENCE.VMA` | `noS2xlate` | `addr >> 12` 为 VA VPN；`id` 为 ASID。 |
| `hg=0,hv=0,priv_virt_at_sample=1` | VS/S1-stage `SFENCE.VMA` | `onlyStage1`、`allStage` | `addr >> 12` 为 GVA VPN；`id` 为 VS-ASID，VMID 使用冻结值。 |
| `hv=1,hg=0` | VS/S1-stage `HFENCE.VVMA` | `onlyStage1`、`allStage` | `addr >> 12` 为 GVA VPN；`id` 为 VS-ASID，VMID 使用冻结值。 |
| `hg=1,hv=0` | G-stage `HFENCE.GVMA` | `onlyStage2`、`allStage` | `GPA = {addr, 2'b00}`，再取 GPA/GVPN；`id[13:0]` 为 VMID。 |

#### Fence 地址位宽输入合同

本 framework 只生成并处理落在 V2 DUT 实际支持地址空间内的 fence。所有测试框架生成的 fence 地址、S1/S2
tag、PPN 与派生地址，均由对应的 compile-time DUT/profile 位宽约束在 S1 VA/GVA、S2 GPA 与 HPA 的有效范围内；
不得通过 plus 参数、directed 覆盖或随机 fallback 生成超范围值。

`HFENCE.GVMA` 的 raw `addr` 是 `GPA >> 2`。上述 GPA 位宽合同保证其高于 DUT 支持范围的位恒为 0，因此
`GPA = {raw.addr, 2'b00}` 不会丢失任何受支持地址位；本 plan 不为 50-bit raw `addr` 的超范围左移建立额外
随机、截断或 alias 行为。S1 的 `SFENCE.VMA/HFENCE.VVMA` 地址也必须在各自冻结 `satp/vsatp` mode 可表示的
VA/GVA 范围内；仅限制 HPA/PPN 不能替代这条 S1 输入约束。

`rs1=x0` 表示所有地址，raw `addr` 不参与该合同或 matcher。`rs1!=x0` 而 monitor 实际观察到超出上述
S1 VA/GVA、S2 GPA 或 HPA 范围的输入时，立即 `uvm_fatal` 并输出 raw 地址、target stage、冻结 mode 与
支持位宽；不建立 non-canonical `no-op` 分支，不截断后继续匹配，也不登记 live-entry 删除工作。该输入在
正常测试框架生成路径中不可达。

```text
decode_raw_sfence(raw):
  若 raw.valid=0 或 raw.context_valid=0，uvm_fatal。
  若 raw.reset_epoch != current_reset_epoch 或 raw.context_reset_epoch != raw.reset_epoch：uvm_fatal；
    stale raw 必须已由 adapter drain 在调用本 helper 前丢弃，future/mixed epoch raw 不能解码。
  若 raw.hv 与 raw.hg 同时为 1，uvm_fatal。
  复制 rs1/rs2 为 ignore_addr/ignore_id，并复制冻结 context。
  若 hg：target=G_STAGE；在 rs1 非 x0 时先验证 GPA 位宽，再以 {addr, 2'b00} 得到 GPA/GVPN；
         在 rs2 非 x0 时仅保存 id[13:0] 作为指定 VMID。
  否则若 hv 或 priv_virt_at_sample：target=VS_STAGE；在 rs1 非 x0 时先验证 S1 VA/GVA 位宽，再取 addr>>12 得到 VPN。
  否则：target=HS_STAGE；在 rs1 非 x0 时先验证 S1 VA/GVA 位宽，再取 addr>>12 得到 VPN。
  返回 payload；rs1=x0 时地址字段不参与后续 matcher，rs2=x0 时 ID 不参与后续 matcher。
```

中文文字伪代码：解码先拒绝不可能的 `hv && hg`，防止旧实现“优先按 hg”掩盖接口错误。随后仅由 raw 的冻结
`priv_virt_at_sample` 区分普通 SFENCE 的 HS 与 VS 语义。GVMA 的地址必须先恢复 GPA，再生成 GVPN；不能把
`addr` 当普通 VPN。GVMA 指定 VMID 只比较 V2 支持的低 14 位，不能把 16-bit raw `id` 直接与 `s2_vmid` 比较。
`rs1/rs2` 的布尔值只描述寄存器是否为 x0，payload 数值即使为零也不会改变精确匹配语义。

### 2. `sfence_match_entry()`

**抽象功能描述：** `sfence_match_entry()` 判断一个 live entry 是否属于已解码 fence 的架构作用域。它只读取
payload 与 entry 的 S1/S2 字段及创建时 provenance，返回匹配布尔值；不删除 entry，不读取当前
`mmu_csr_state`。

**输入：** `memblock_sfence_payload_t payload`、`memblock_tlb_lookup_key_t key`、`memblock_tlb_entry entry`。

**输出/副作用：** 返回 match/not-match；entry 为 null、stage 编码异常时 `uvm_fatal`，正常不命中不报错。

```text
sfence_match_entry(payload, key, entry):
  确认 payload 有效且 entry 非空。
  先按 target_stage 检查 key.s2xlate 是否属于本次 fence 的允许集合；不属于则返回不命中。
  case payload.target_stage
    HS_STAGE:
      确认 entry.s1_stage_active；否则 uvm_fatal。
      addr_ok = payload.ignore_addr || sfence_s1_addr_match(entry, payload.s1_vpn,
                                                            entry.s1_translation_mode_at_build)。
      id_ok = payload.ignore_id || (!entry.s1_pte_g && entry.s1_asid == payload.id)。
      vmid_ok = 1。

    VS_STAGE:
      确认 entry.s1_stage_active；否则 uvm_fatal。
      addr_ok = payload.ignore_addr || sfence_s1_addr_match(entry, payload.s1_vpn,
                                                            entry.s1_translation_mode_at_build)。
      vmid_ok = entry.s1_vmid == payload.hgatp_vmid_at_sample。
      id_ok = payload.ignore_id || (!entry.s1_pte_g && entry.s1_asid == payload.id)。

    G_STAGE:
      确认 entry.s2_stage_active；否则 uvm_fatal。
      addr_ok = payload.ignore_addr || sfence_s2_addr_match(entry, payload.s2_gvpn,
                                                            entry.s2_translation_mode_at_build)。
      vmid_ok = payload.ignore_id || (entry.s2_vmid == payload.id)。
      id_ok = 1。
  endcase
  返回 addr_ok && id_ok && 本 stage 所需 vmid_ok。
```

中文文字伪代码：匹配器先由解码结果选择 stage，再限制哪类 `s2xlate` entry 有资格参与。S1 路径只使用
`s1_*` 字段，指定 ASID 时跳过 `s1_pte_g=1` 的 global mapping；VS 路径无论 `rs2` 是否为 x0 都必须满足
采样时 VMID。G-stage 路径只比较 S2 VMID，不引用 `pte_g` 或 S1 ASID。每个匹配结果均由自身 stage 的地址范围
与 ID 条件构成，绝不以 `min(s1_level,s2_level)` 或共享 `key.vpn/entry.level` 代替。range helper 的 mode 输入
只能取 entry 的 `*_translation_mode_at_build`；不得用 raw fence mode、drain 时 current CSR 或
`csr_context_seq_at_build` 重新推导。`entry_generation` 仅随日志携带，不参与返回 true/false 的条件。

### 3. 精确匹配判定顺序与 `rs1/rs2` 四种组合

本节描述的是默认 architecture matcher。V2 local DTLB 的 over-fence widening 不适用本节；若后续启用 widening，
只能在本节地址条件已经选定目标 stage、VMID 和 ID 作用域之后扩大 `addr_ok`，不能改变下列 ID 或 global 规则。

#### 3.1 通用判定顺序

1. `raw.rs1=1` 表示 instruction 的 `rs1=x0`，解码后 `payload.ignore_addr=1`；`raw.rs1=0` 才表示必须用
   payload 中的实际地址做精确范围匹配。
2. `raw.rs2=1` 表示 instruction 的 `rs2=x0`，解码后 `payload.ignore_id=1`；`raw.rs2=0` 时即使 `id=0`，
   仍是指定数值为 0 的 ASID/VMID，不是“全部 ID”。
3. 先按 target stage 过滤 entry：HS 只允许 `noS2xlate`；VS 只允许 `onlyStage1/allStage`；G-stage 只允许
   `onlyStage2/allStage`。不符合的 entry 不读取另一 stage 字段，直接不命中。
4. 仅在 `rs1!=x0` 时计算地址范围：HS/VS 用 `addr >> 12` 得到 VA/GVA VPN；GVMA 先执行
   `GPA={addr, 2'b00}`，再得到 GPA/GVPN。地址先满足本章 DUT 位宽输入合同，才允许进入 range matcher；
   `rs1=x0` 时地址条件恒为真，不因 payload 地址数值为 0 或其它值改变。
5. 仅在 `rs2!=x0` 时执行指定 ASID/VMID 条件。HS/VS 的指定 ASID 不能删除 `s1_pte_g=1` entry；G-stage 没有
   `PTE.G` 过滤，只按 S2 VMID。
6. 对 `allStage`，只要该 event 在其目标 stage 的全部条件为真，就把同一个 live key 判为命中；后续删除整个
   `tlb_entry_by_key[key]`，不分别保留 S1 或 S2 一半字段。

为了简化下表，定义：

```text
S1_RANGE = sfence_s1_addr_match(entry, payload.s1_vpn)
           // s1_tag/s1_level/s1_pte_n 与 S1 sector 字段共同表示的覆盖范围。
S2_RANGE = sfence_s2_addr_match(entry, payload.s2_gvpn)
           // s2_tag/s2_level/s2_pte_n 表示的 G-stage 覆盖范围。
VS_CTX   = entry.s1_vmid == payload.hgatp_vmid_at_sample
```

#### 3.2 HS/S-stage `SFENCE.VMA`：`priv_virt_at_sample=0`

前置条件始终为 `key.s2xlate==noS2xlate`。任何 `onlyStage1`、`onlyStage2` 或 `allStage` entry 都不属于该
普通 HS fence 的精确作用域。

| `rs1` | `rs2` | 精确命中条件 | `s1_pte_g=1` 的结果 |
|---|---|---|---|
| x0 | x0 | 所有 `noS2xlate` live entry；不比较地址或 ASID。 | 命中。 |
| x0 | 非 x0 | 不比较地址；要求 `!s1_pte_g && s1_asid==id`。 | 不命中。 |
| 非 x0 | x0 | 要求 `S1_RANGE`；不比较 ASID。 | 命中，只要地址范围覆盖。 |
| 非 x0 | 非 x0 | 要求 `S1_RANGE && !s1_pte_g && s1_asid==id`。 | 不命中。 |

中文解释：`rs2=x0` 表示所有 ASID，因此包含 global mapping；只有指定 ASID 时，架构才不要求失效 global mapping。
`rs1=x0` 不比较地址，代表该 ASID 或全部 ASID 的所有 S1 translation；非 x0 地址则只命中覆盖该 VPN 的 entry。

#### 3.3 VS/S1-stage：guest `SFENCE.VMA` 或 `HFENCE.VVMA`

本表适用于 `hg=0` 且 `(hv=1 || priv_virt_at_sample=1)` 的 event。前置条件始终为
`key.s2xlate inside {onlyStage1, allStage}`，且所有四种 `rs1/rs2` 组合都必须满足 `VS_CTX`。这使同一
VS-ASID 在不同 VMID 下的 entry 不会互相删除。

| `rs1` | `rs2` | 精确命中条件 | `s1_pte_g=1` 的结果 |
|---|---|---|---|
| x0 | x0 | `VS_CTX`；不比较 GVA 或 VS-ASID。 | 命中。 |
| x0 | 非 x0 | `VS_CTX && !s1_pte_g && s1_asid==id`；不比较 GVA。 | 不命中。 |
| 非 x0 | x0 | `VS_CTX && S1_RANGE`；不比较 VS-ASID。 | 命中，只要 GVA 范围覆盖。 |
| 非 x0 | 非 x0 | `VS_CTX && S1_RANGE && !s1_pte_g && s1_asid==id`。 | 不命中。 |

中文解释：普通 guest `SFENCE.VMA` 与 `HFENCE.VVMA` 的差别只在 event 的产生路径；在本 matcher 中二者都按
VS-stage 处理。VMID 不是来自消费时 CSR，而是 raw fence 采样时的 `hgatp_vmid_at_sample`；因此 `rs2=x0`
只放宽 VS-ASID，不会跨 VMID 删除。

#### 3.4 G-stage `HFENCE.GVMA`

前置条件始终为 `key.s2xlate inside {onlyStage2, allStage}`。此处 `id` 是 VMID，不是 ASID，且不使用
`s1_pte_g`、`s1_asid` 或任何 S1 地址字段。

| `rs1` | `rs2` | 精确命中条件 | 全局位处理 |
|---|---|---|---|
| x0 | x0 | 所有 `onlyStage2/allStage` live entry；不比较 GPA/GVPN 或 VMID。 | 不读取 `s1_pte_g`。 |
| x0 | 非 x0 | 不比较 GPA/GVPN；要求 `s2_vmid==id`。 | 不读取 `s1_pte_g`。 |
| 非 x0 | x0 | 要求 `S2_RANGE`；不比较 VMID。 | 不读取 `s1_pte_g`。 |
| 非 x0 | 非 x0 | 要求 `S2_RANGE && s2_vmid==id`。 | 不读取 `s1_pte_g`。 |

中文解释：GVMA 的 `rs1` 编码 GPA 的右移两位形式，必须先还原再得到 GVPN；不能把它直接送入
`sfence_s1_addr_match()`。`rs2=x0` 代表所有 VMID，`rs2` 非 x0 时才指定一个 VMID；G-stage 不存在将 S1
global mapping 排除在外的 ASID 规则。

### 4. S1/S2 地址范围 helper

#### `sfence_s1_addr_match()`

**抽象功能描述：** `sfence_s1_addr_match()` 判断 S1 fence VPN 是否位于 live entry 的 S1 映射范围。它是纯
range helper，不访问 CSR、FIFO 或 table。

**输入：** `s1_tag`、`s1_level`、`s1_pte_n`、`s1_valididx[]`、`s1_translation_mode_at_build` 与 fence VPN。

**输出/副作用：** 返回布尔值，无状态副作用。

进入范围比较前调用 `validate_frozen_stage_level(S1, entry.s2xlate, entry.s1_stage_active,
entry.s1_translation_mode_at_build, entry.s1_level)`。该 helper 只验证 entry 创建时已经冻结的字段：Sv39 只允许
`0/1/2`，Sv48 允许 `0/1/2/3`；Bare active stage、未知 mode、active 标志与 `s2xlate` 不一致或 mode/level
不兼容时 `uvm_fatal`，不回读 current CSR，也不降级到 base page。

```text
sfence_s1_addr_match(entry, fence_vpn, s1_translation_mode_at_build):
  先验证 entry.s1_stage_active=1 且 mode 不是 Bare；否则 uvm_fatal。
  调用 validate_frozen_stage_level()；它只检查冻结 mode/level 的合法组合，失败则 uvm_fatal。
  若 s1_pte_n=1：按 entry 的 NAPOT 编码形成覆盖范围，判断 fence_vpn 是否落入该范围。
  否则若 s1_level 为 superpage：按该 S1 level 忽略由输入 VPN 覆盖的低位，比较剩余 tag 范围。
  否则：由 fence_vpn 取得目标 S1 sector；比较该 sector 的 tag 前缀与 s1_valididx[]。
  返回范围是否覆盖 fence_vpn。
```

中文文字伪代码：该 helper 必须与 S1 entry 构造使用同一份 sector/NAPOT 编码规则。base page 不能只比较
`s1_tag`，还要确认目标 VPN 对应的 sector 在 `s1_valididx[]` 中有效；`ppn_low`、`addr_low` 和 `pteidx`
是 response payload 一致性字段，不能参与 fence 地址命中。superpage 按 S1 自己的 level 扩大覆盖范围；NAPOT
按其编码范围匹配。它不因 `allStage` 的 S2 level 更小而缩窄 S1 范围。

| S1 payload 状态 | `S1_RANGE` 的精确判定 |
|---|---|
| `s1_pte_n=1` | 调用与 entry tag/PTE.N 编码共用的 NAPOT range helper 形成首尾 VPN 范围；不得读取 split PPN、`addr_low` 或 `pteidx`，也不能把 `s1_level` 当作普通 superpage 再二次缩窄。 |
| `s1_pte_n=0,s1_level=0` | 先按 `fence_vpn` 找到目标 S1 sector；只有 tag 前缀匹配且该 sector 的 `s1_valididx[]` 有效时命中。`s1_addr_low/s1_ppn_low[]/s1_pteidx[]` 不参与本判定。 |
| `s1_pte_n=0,s1_level=1` | 比较去除低 9 位 VPN 后的 S1 tag 前缀，覆盖 2 MiB 的 S1 范围。 |
| `s1_pte_n=0,s1_level=2` | 比较去除低 18 位 VPN 后的 S1 tag 前缀，覆盖 1 GiB 的 S1 范围。 |
| `s1_pte_n=0,s1_level=3` | 比较去除低 27 位 VPN 后的 S1 tag 前缀，覆盖 Sv48 支持的 512 GiB S1 范围；当前 S1 mode 不支持该 level 时属于 entry 构造错误，不在 matcher 中降级匹配。 |

上表中的“去除低位”只表示由该 stage 的输入 VPN 补齐的 superpage 低位不参与 tag 比较；实际比较位宽仍以 entry
创建时冻结的 `s1_translation_mode_at_build` 为准。matcher 不重新读取 current CSR、raw fence mode 或
`csr_context_seq_at_build`，也不把 S2 level 或 S2 tag 混入 S1 范围判断。

#### `sfence_s2_addr_match()`

**抽象功能描述：** `sfence_s2_addr_match()` 判断 G-stage fence GVPN 是否位于 live entry 的 S2 映射范围。
它是纯 range helper，不读取 S1 tag 或当前 CSR。

**输入：** `s2_tag`、`s2_level`、`s2_pte_n`、`s2_translation_mode_at_build` 与 GVPN。

**输出/副作用：** 返回布尔值，无状态副作用。

该 helper 同样先调用 `validate_frozen_stage_level(S2, entry.s2xlate, entry.s2_stage_active,
entry.s2_translation_mode_at_build, entry.s2_level)`。它使用 Sv39x4/Sv48x4 的 stage-specific 合法 level 集合；
不从当前 `hgatp.mode` 或 fence sample 的 mode 推导 mask。

```text
sfence_s2_addr_match(entry, fence_gvpn, s2_translation_mode_at_build):
  先验证 entry.s2_stage_active=1 且 mode 不是 Bare；否则 uvm_fatal。
  调用 validate_frozen_stage_level()；它只检查冻结 mode/level 的合法组合，失败则 uvm_fatal。
  若 s2_pte_n=1：按 S2 NAPOT 编码范围匹配 fence_gvpn。
  否则：按 s2_level 的 base-page 或 superpage 覆盖范围比较 s2_tag 与 fence_gvpn。
  返回范围是否覆盖 fence_gvpn。
```

中文文字伪代码：S2 helper 的输入是由 `HFENCE.GVMA` 的 `{addr, 2'b00}` 恢复的 GPA/GVPN，不能接收普通 S1 VPN。
它只使用 S2 的 level 和 NAPOT 状态，因此 allStage 的 G-stage fence 仍按 S2 page size 判断，不会被 S1 的
superpage 覆盖范围放大或缩小。

| S2 payload 状态 | `S2_RANGE` 的精确判定 |
|---|---|
| `s2_pte_n=1` | 调用与 S2 entry 构造共用的 NAPOT range helper，以 S2 NAPOT raw 编码判断 GVPN 是否落在映射范围。 |
| `s2_pte_n=0,s2_level=0` | 完整比较 base-page GVPN 与 `s2_tag`，只命中同一 4 KiB G-stage page。 |
| `s2_pte_n=0,s2_level=1` | 比较去除低 9 位 GVPN 后的 S2 tag 前缀，覆盖 2 MiB G-stage 范围。 |
| `s2_pte_n=0,s2_level=2` | 比较去除低 18 位 GVPN 后的 S2 tag 前缀，覆盖 1 GiB G-stage 范围。 |
| `s2_pte_n=0,s2_level=3` | 比较去除低 27 位 GVPN 后的 S2 tag 前缀，覆盖 Sv48x4 支持的 512 GiB G-stage 范围；Sv39x4 entry 不应生成该 level。 |

S2 的输入 GVPN 已由 `decode_raw_sfence()` 从同一条 GVMA 的 `{addr, 2'b00}` 派生；不得从 `key.vpn`、
`s1_resolved_ppn` 或消费时 CSR 重新推导一个替代 GVPN。level=3 是否可用、tag 有效宽度与 superpage mask
只按 entry 的 `s2_translation_mode_at_build` 判定；不得用 raw fence mode 或当前 `hgatp.mode` 变化既有 entry
的范围。

#### `validate_frozen_stage_level()`

**抽象功能描述：** `validate_frozen_stage_level()` 在 S1/S2 range helper 消费 entry 前，验证 stage-active、
`s2xlate`、创建时 translation mode 与 level 的组合是否仍是构造期允许的编码。它只读取 entry 已冻结的标量，
不查询 CSR、不修改 entry/table；合法时返回成功，结构性矛盾时 `uvm_fatal`。

```text
validate_frozen_stage_level(target_stage, s2xlate, stage_active, translation_mode_at_build, level):
  确认 target_stage 对应的 stage_active=1，并确认 s2xlate 属于该 target stage 的允许集合；否则 uvm_fatal。
  若 translation_mode_at_build 为 Bare：uvm_fatal；active Bare request 不属于本 framework 支持输入。
  若 target_stage=S1：Sv39 只接受 level 0/1/2，Sv48 接受 level 0/1/2/3。
  若 target_stage=S2：Sv39x4 只接受 level 0/1/2，Sv48x4 接受 level 0/1/2/3。
  其它 mode、level 越界或 stage/mode 不一致时 uvm_fatal；不把 level 截断成 0，也不调用 current CSR 修复。
  返回成功。
```

该 helper 的结果只允许对应 range matcher 继续计算地址范围；它不会删除 entry、改变 generation、取消 pending
response 或触发新的随机构造。

### 5. `schedule_sfence_invalidate()` / `apply_due_sfence_invalidate()` 与全 entry 删除

**抽象功能描述：** `schedule_sfence_invalidate()` 在 dispatch adapter 消费一条已解码 fence 时，只将其登记为按 V2
filter flush 时刻到期的删除工作。`apply_due_sfence_invalidate()` 由 dispatch adapter 在每个 dispatch service sample 调用，
只处理到期记录并扫描 live table 统一删除。前者不扫描 table，后者是中频事件路径，允许扫描
`tlb_entry_by_key`，但不能扫描 main transaction table。

**输入：** 前者输入已完成 context 绑定的 `memblock_sfence_payload_t`、其 raw DUT global `sample_seq`、`reset_epoch` 与同 sample 的
`lifecycle_event_seq`；后者输入当前统一 DUT global `sample_seq` 与 `reset_epoch`。

**输出/副作用：** 前者只 push `sfence_invalidate_pending_q`；后者返回到期时实际删除的数量并只修改
`tlb_entry_by_key`。

```text
schedule_sfence_invalidate(payload, anchor_dut_sample_seq, reset_epoch, lifecycle_event_seq):
  要求 payload.valid、anchor_dut_sample_seq 为 raw fence 的冻结 DUT global sample、reset_epoch==current_reset_epoch，
    且 lifecycle_event_seq 非 0；旧 epoch 工作记录诊断后丢弃，未来 epoch 或字段矛盾时 uvm_fatal。
  保存已经绑定 context 的 payload 与 reset_epoch，计算 due_filter_flush_sample = anchor_dut_sample_seq + MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES。
  将 lifecycle_event_seq 连同 payload/due 加入按 due sample 递增的 sfence_invalidate_pending_q；此处不得删除 live entry。

apply_due_sfence_invalidate(dut_sample_seq, current_reset_epoch):
  对每个 due_filter_flush_sample <= dut_sample_seq 的记录：
    若 record.reset_epoch < current_reset_epoch：丢弃并记录 stale reset work，不调用 matcher；
    若 record.reset_epoch > current_reset_epoch：uvm_fatal；不能让未来 epoch work 提前删除 entry。
    只有 record.reset_epoch == current_reset_epoch 才继续：
    对 tlb_entry_by_key 的每个 live key 调用 sfence_match_entry(payload, key, entry)。
    将所有命中的 key 与 entry_generation 放入 delete_keys 临时 queue。
    遍历结束后逐个调用 delete_live_tlb_entry_by_anchor_key(delete_key, "SFENCE/HFENCE")；
    该公共 helper 先用 entry.range_index_keys 精确反注册 secondary range index，再删除 canonical entry。
    记录 anchor/due sample、target stage、删除数量、地址/ID 范围与被删 generation。
  返回本拍 delete_keys 的总数。
```

中文文字伪代码：顶层 monitor 在 C0 看到 SFENCE/HFENCE 时，DUT 的 DTLB filter 尚未清空；MemBlock 的两级
`RegNext` 与 `PTWNewFilter` 的两拍 `DelayN` 使真实边界为 C4。因而 C0 只能登记待失效 payload，不能先删除
`tlb_entry_by_key` 再让同拍已经 `valid && ready` 的 L2TLB request 以错误的新 entry 建表。到 C4 时，L2TLB
lifecycle owner 已保证没有与 filter flush 同拍 fire 的 response，并独立取消未完成旧 token 以及已经观察到
`first_request_fire_sample <= barrier.anchor` 的 WAITING UID；adapter 在同一 due sample 删除 live entry。两者
不互相调用，也不依赖 UVM service 先后顺序。
先收集后删除可避免在 associative array 遍历中改变当前集合。删除必须经过 range plan 定义的统一 helper，
否则 `tlb_anchor_keys_by_range_key` 会保留指向已删除 canonical key 的悬挂候选，下一次 exact miss 会错误 fatal。
`allStage` 只是一个保存双阶段字段的 live object：
S1 matcher 命中或 S2 matcher 命中后，都会把同一个 key 与其当时的 `entry_generation` 放入 `delete_keys`，最终
删除完整 entry；不得只清其中的 `s1_*` 或 `s2_*` 字段。generation 只作删除日志和随后同 key rebuild 的追溯，
不能据此删除 UID history、main/status table 或已冻结 pending snapshot；它们保持不变。

### 5.1 与 L2TLB token 生命周期的时序边界

`sfence_invalidate_pending_q` 只属于 live-entry invalidation flow；它不保存、取消或完成 L2TLB request token。
L2TLB pending/driving token 的唯一 owner 是
`AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2tlb_sfence_flush_token_timing_correction_plan_20260805.md` 定义的 L2TLB lifecycle flow。两者共享
同一个 `due_filter_flush_sample`，但不得互相扫描或修改对方队列。

C1-C3 的 external response completion 同样只由该 lifecycle flow 处理。其 response-to-UID multicast 必须用
response fire 当拍 `peek_current_dut_global_sample()-2` 的 response-visible CSR 重放 raw hit；本 plan 不匹配 UID、不读取
UID issue-time CSR，也不因 live entry 被登记删除而提前改变 pending snapshot。

`sfence_invalidate_pending_q` 的唯一写者是 `common_data_transaction::schedule_sfence_invalidate()`，唯一删除者是
`common_data_transaction::apply_due_sfence_invalidate()`；该 queue 归 `common_data_transaction` 持有。仅在
dispatch-active 时，`dispatch_monitor_event_adapter` 在 drain 成功后调用前者，并在到期 C4 调用后者。L2TLB lifecycle
sequence 不读取 raw FIFO，也不调用 live-entry matcher。两侧共用同一个 due sample：token owner 在 C4 禁止 response 并
取消旧 token/UID；adapter 在 C4 删除 live entry。pending snapshot 已冻结，因此二者无需互扫队列或依赖 UVM service
执行先后。no-dispatch 不存在此 queue 的 item；若观察到 item 即 `uvm_fatal`。

```text
C0：monitor 采到 SFENCE/HFENCE；adapter 登记 live-entry invalidate，L2TLB lifecycle 登记 token flush barrier，下一 cycle item 关闭 ready。
C0：若上一 cycle ready=1 且 request valid=1，该 request 已真实 fire；L2TLB owner 正常捕获旧 entry snapshot。
C1-C3：已捕获 token 可以正常返回 response；live entry 仍保留，禁止本 flow 提前删除。
C4：L2TLB owner 不允许 response 在本拍 fire，取消仍 pending 的旧 token，以及已观察到
    `first_request_fire_sample <= barrier.anchor` 的 WAITING UID；adapter 同拍应用 entry 删除。
C5：仅在没有后续 flush barrier 时重新开放 ready；新的 request 才可以使用 fence 后重建的 live entry。
```

`sfence.bits.flushPipe` 不参与上述 queue 的删除条件。它是同一 Fence uop 写回 ROB 的控制字段，完整 Core 中由
ROB 提交后产生的 `flushAfter` redirect 清除年轻指令；V2 MemBlock 的 non-block DTLB `io.flushPipe` 固定为
`false.B`，不能据此在 responder 内直接杀 request 或 token。

## V2 DUT Over-Fence 边界

本 plan 明确选择 architecture matcher 作为默认实现。这里的 live entry 是测试框架的“已完成翻译
logical model”，不是 V2 `TLBStorage` 的 local L1 entry，也不是 `PageTableCache` 的 L2 cache entry。因此，
本 matcher 只按 entry 自己冻结的 S1/S2 payload、地址范围和 ID 语义做精确失效；它不声称逐项复刻任何一处
V2 内部缓存实现。

V2 `TLBStorage.scala` 的虚拟态 `SFENCE.VMA`/`HFENCE.VVMA`、以及 `HFENCE.GVMA` 存在局部保守
over-fence；`PageTableCache.scala` 的 `HFENCE.GVMA x0,*` 又有不同的 `onlyStage1` 清理规则。两者的
eligible `s2xlate` 集合和地址行为都不同，不能抽象成“只把 `addr_ok` 设为 true”的通用开关。

本专项不实现上述任一 over-fence，也不增加 runtime/compile profile 开关。若后续要验证某个具体 V2
cache 的 source-equivalent flush，必须另建专项，明确该模型对应 `TLBStorage` 还是 `PageTableCache`，并定义
目标 entry 类别、eligible `s2xlate`、地址与 VMID/ASID 规则；不得修改本 logical live-entry matcher。

本专项不通过共享 `entry.level`、`key.vpn` 或 `pte_g` 实现 widening。`allStage` 的 S1/S2 命中始终使用各自
stage 的 level/tag/N/sector 字段，任一 stage 命中均删除完整 logical entry。

## 执行中补充/修正（IMPLEMENTATION_DELTA）

### [IMPLEMENTATION_DELTA] adapter 的 C4 work 真源

**来源：** 执行前独立 review 发现原 adapter 仍保留私有 `pending_l2tlb_sfence_q`，并调用已经移除的
`apply_raw_sfence()`，导致无法编译且与 C4 queue 的唯一 owner 冲突。

**原 plan：** adapter 应作为 raw FIFO 的唯一 destructive consumer，但未把当前旧私有 queue 的迁移落点写成
实际 API。

**实现调整：** 删除 adapter 私有 pending queue，改为：

```text
adapter peek raw FIFO 队首
  -> 校验 reset epoch、event provenance 和同 sample CSR context
  -> data.decode_raw_sfence(raw)
  -> data.schedule_sfence_invalidate(...)
  -> schedule 成功后 pop 同一队首
  -> 每个 service sample 调用 data.apply_due_sfence_invalidate(...)
```

`common_data_transaction::sfence_invalidate_pending_q` 是唯一 C4 work 真源；release drain proof 同时检查该
queue 和 raw FIFO 都为空。这样 adapter 不再拥有第二份 due 状态，也不存在 C0 立即删除入口。

### [IMPLEMENTATION_DELTA] 唯一调度点与冻结字段检查

**来源：** review 发现 `collect_runtime_context_events()` 仍直接 service raw fence，且 `rs1=x0` 时 matcher 会绕过
stage/mode/level 结构检查。

**实现调整：** `collect_runtime_context_events()` 只同步 CSR；
`memblock_main_dispatch_auto_build_main_table_base_sequence::service_monitor_once()` 在其后每个 sample 恰好调用一次
`service_l2tlb_sfence_events()`。matcher 在完成 `s2xlate` eligibility 判断后，无论 `rs1` 是否为 x0 都先调用
`validate_frozen_stage_level()`；S1 matcher 只读取 `tag/level/PTE.N/valididx`，不再为地址命中读取
`ppn_low` 或 `pteidx`。

### [IMPLEMENTATION_DELTA] CSR context 队尾绑定与 reset writer 边界

**来源：** review 发现 CSR context 发布逐项扫描 raw FIFO，且 CSR reset 和 adapter reset 都清同一 context。

**实现调整：** 一个 fence interface 每个 sample 最多产生一条 raw，FIFO 按 sample 有序；CSR monitor 只检查/绑定
当前 sample 的队尾，旧 sample 仍未绑定即 fatal。CSR monitor 是 `l2tlb_sfence_csr_context` 的 reset clear 唯一
writer；adapter reset 只清 raw FIFO、live entry 和 adapter proof。

### [IMPLEMENTATION_DELTA] 本轮验证结果

- `git diff --check` 通过。
- `rg` 确认 `apply_raw_sfence`、`apply_sfence_invalidate`、`drain_sfence_events` 和 adapter 私有 pending queue 无可达残留。
- `make eda_compile tc=basicTest ts=virtual_base_sequence mode=base_fun` 通过。
- VCS 曾遗留一个占用 `base_fun/exec/simv.daidir/work.lib++/tdc.sdb` 的 orphan `vcs1`，使 `eda_run` 的冗余重编译报告
  `VFS_SDB_ERROR`。终止该遗留工具进程并删除该生成数据库后，重新 `eda_compile` 通过；这不是 SystemVerilog 或 DUT
  行为失败。
- 在已成功编译的 `simv` 上执行 `make eda_batch_run tc=basicTest ts=virtual_base_sequence mode=base_fun` 通过，
  `UVM_ERROR=0`、`UVM_FATAL=0`。
- 在同一已编译 `simv` 上执行
  `make eda_batch_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=base_fun cfg=tc_dispatch_real_smoke` 通过，
  覆盖真实 dispatch、L2TLB responder、writeback、ROB commit 和 LQ deq，`UVM_ERROR=0`、`UVM_FATAL=0`。

## 失败策略与定向验证

### 1. 失败策略

| 条件 | 行为 |
|---|---|
| `hv && hg` 同时为 1 | `uvm_fatal`；接口 event 没有唯一目标 stage。 |
| raw fence 在下一个 DUT sample 到来后仍无同 sample CSR context | `uvm_fatal`；保留 sample、addr、id、hv/hg 与当前 sample 日志，不能默认 `priv_virt=0`。 |
| `raw.context_valid=0` 但仍在同一个 sample | 不 pop FIFO，本轮 return，等待同拍协调完成。 |
| `rs1!=x0` 且 fence 地址超出 V2 DUT 支持的 S1 VA/GVA、S2 GPA 或 HPA 位宽 | `uvm_fatal`；这是生成/接口合同破坏，不做 non-canonical `no-op`、截断或 live-entry 删除。 |
| null live entry、未知 target stage、schedule 成功后 FIFO pop 失败 | `uvm_fatal`；公共状态无法可靠继续。 |
| live entry `entry_generation==0`、stage-active 与 `s2xlate` 不一致或 frozen mode/level 不兼容 | `uvm_fatal`；不使用 `priv_mode`、current CSR 或默认 level=0 修复。 |
| 地址、ASID 或 VMID 正常不匹配 | 正常不删除，不报错。 |

### 2. Directed 场景

- HS `SFENCE.VMA`：在 `noS2xlate` entry 上分别覆盖 `x0/x0`、`x0/指定 ASID`、`指定 VA/x0`、
  `指定 VA/指定 ASID` 四种组合；每种组合同时放入同地址/异地址、同 ASID/异 ASID 与 `s1_pte_g=0/1` entry。
- guest `SFENCE.VMA` 与 `HFENCE.VVMA`：在同一 S1 tag、相同 VS-ASID 但 VMID 不同的
  `onlyStage1/allStage` entry 上，分别覆盖上述四种 `rs1/rs2` 组合，验证 `rs2=x0` 仍不能跨采样 VMID 删除。
- `HFENCE.GVMA`：在 `onlyStage2/allStage` entry 上分别覆盖 `x0/x0`、`x0/指定 VMID`、
  `指定 GPA/x0`、`指定 GPA/指定 VMID` 四种组合，验证 `{addr, 2'b00}` GPA 还原、DUT 位宽内的 S2 range、
  `id[13:0]` VMID 与 S2 VMID 的组合条件。
- 在上述三组场景中额外覆盖非 x0 寄存器携带数值 0，确认它仍是指定地址或指定 ASID/VMID，而不是 all-address/all-ID。
- `allStage` 下建立 `s1_level=2`、`s2_level=0` entry，验证 S1 fence 按 S1 1 GiB 范围命中，GVMA 按 S2 4 KiB 范围命中。
- `s1_pte_g=1` 时，指定 ASID 的 HS/VS fence 不删除；`rs2=x0` 时可以删除。GVMA 不读取该字段。
- S1 superpage、S1 NAPOT、S1 sector 边界，以及 S2 superpage/NAPOT 边界。
- 建立同 key 的 S1 Sv39（或 S2 Sv39x4）entry 后切换对应 current CSR 到 Sv48（或 Sv48x4）再延迟 drain，
  验证 matcher 仍只按 entry 的冻结 mode 解释 level/mask；不得借 current CSR 把旧 entry 当成 level-3 可用范围。
- active Bare stage 不应向本 L2TLB responder 产生已接受 request；本 plan 不构造或匹配其 payload。若 monitor
  观察到 active stage 的 frozen mode 为 Bare，按 framework 输入合同 `uvm_fatal`。`onlyStage1/onlyStage2` 中
  未参与 response 的另一 stage 只保持初始化默认值，不进入 payload 构造或 matcher。
- VMID=A 时生成 `SFENCE.VMA(V=1)` 或 `HFENCE.VVMA`，随后切到 VMID=B 再延迟 drain；只删除 VMID=A entry。
- dispatch-active：adapter 必须独立 drain raw FIFO，并在 C4 删除 canonical entry 与其 range index。随后同 key request
  必须 miss/build 新 generation，不能命中悬挂 index 或复用旧 payload。
- no-dispatch：`dispatch_monitor_capture_en=1` 仍可采样 fence，但 `dispatch_l2tlb_lookup_active=0` 时 raw fence 必须
  不入 FIFO；当前支持矩阵只允许 `DISABLED/NO_OWNER + NO_DISPATCH`，L2TLB monitor 断言 `req_valid=0` 且
  `tlb_entry_by_key` 为空。disabled driver 的 ready 为 0，不能只断言 `req.fire=0`。该拓扑不建立 live-entry delete、owner handoff 或
  high-watermark directed 场景。
- 当前固定 topology testcase 不覆盖 dispatch topology 切换；若运行中改写 `dispatch_l2tlb_lookup_active`，必须
  `uvm_fatal`。本 plan 不实现 topology-transition 专项；不允许用 `l2tlb_responder_active`、L2TLB owner release 或
  inactive-gap reconcile 隐式处理。
- static consumer check：`memblock_dispatch_base_sequence::collect_runtime_context_events()` 不得调用
  `drain_sfence_events()`；`dispatch_monitor_event_adapter::drain_sfence_events()`、
  `common_data_transaction::apply_raw_sfence()` 与旧立即删除 `apply_sfence_invalidate()` 不得存在可达调用。
  每个 dispatch sample 必须恰好一次进入 `service_l2tlb_sfence_events()`，否则报 `uvm_fatal` 或静态 review failure。
- idle/global stop 在 C0 fence 后到来：L2TLB token owner 仍需等 token/UID/barrier 收敛；adapter 独立等 raw FIFO 和
  C4 pending invalidate 收敛。除此之外，fence monitor 必须先完整处理 close request 后的一个 raw sample，写与 current
  reset epoch/current close generation 匹配的 raw-fence intake closed；队列瞬时为空不代表生产者已经停止。二者均不能静默跳过各自的 C4 工作。
  fence monitor 在 intake closed 真实写入前不得因 global stop/phase end 自然退出；若该 sample 未出现而 parent 已停止 service，按
  lifecycle timeout/fatal 处理，不能发放 release grant。
- global stop 的 admission close 由 shared timing plan 统一处理：parent 只停止 routing；唯一 owner 在下一真实 `drv_cb`
  先结算此前已驱动 ready 窗口的 UID/token capture，写 admission-settled 后写当前 epoch 的
  `close_requested`/generation；该 flag 写入后新的 UID/token capture 立即非法。reset release 后必须先完成 current epoch 的
  NORMAL/inactive baseline；baseline 前不会生成 stop。owner 的下一带匹配 generation 的携带 current reset epoch/generation 的
  `RELEASE_STOP` item 经真实 `drv_cb` 由 driver 确认 `sampled_req_ready=0`、`sampled_req_fire=0`、owner/generation/reset epoch 匹配后才得到 `admission_closed/cutoff`；reset 前 stale item 必须
  `item_done()` 后丢弃，不确认新 epoch。
  adapter 不写这些状态，也不得把自身 raw FIFO drain、C4 delete 或 release closing 当成关闭 L2TLB request admission 的信号。
  owner 的独立 `RELEASE_FINAL_INACTIVE` kind/generation/reset_epoch 由 timing plan 的 driver helper 以冻结的
  current owner 匹配且 `sampled_req_ready=0`、`sampled_req_fire=0`、`sampled_resp_valid=0` 采样确认；adapter drain 可与
  owner stop/final/closing 并行，fence monitor 的 intake close 也可并行；final sample 还必须被 monitor settled，并在 sequence terminal ack 后由 driver 在
  下一真实 `drv_cb` 回收 mailbox、写 final recycle proof。parent 必须同时等待 response drain、adapter queue drain、与 owner generation 匹配的 intake closed、
  final recycle proof 和 mailbox EMPTY；stage adapter 不读取或生成 owner metadata。
- 删除命中 entry 后，相同 key 重新 request 必须新建 entry 并得到新的 `entry_generation`；旧
  `uid_tlb_record_by_uid` 与已冻结 pending snapshot 保持各自原 generation/provenance 不变。
- `hv && hg` 与 context 缺失路径必须得到预期 `uvm_fatal`，验证框架不会静默使用 current CSR。

本 plan 不实现 scoreboard、RM 或 covergroup。上述场景只验证测试框架的 raw 采样、stage 选择、范围匹配、
删除边界与状态保持是否符合本 plan。
