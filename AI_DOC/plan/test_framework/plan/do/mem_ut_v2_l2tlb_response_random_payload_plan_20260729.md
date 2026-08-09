# V2 L2TLB Response 随机 Payload 扩展 Plan

状态：`do`，coding、文档同步、compile 与定向 smoke 已完成；独立末轮 review 已 `FINAL PASS`。基础 `tc_sanity` 仍有已知 DCache L2 flush 无关失败，已在 implementation review 中记录为本专项之外的回归风险；本文件作为 response payload 的唯一 coding 权威。

共享 lifecycle 约束：`AI_DOC/plan/test_framework/review_doc/undo/mem_ut_v2_l2tlb_single_owner_lifecycle_optimization_review_20260807.md`。
本 plan 不重新定义 owner、global sample、runtime reset、global stop 或 release；其 payload/UID 逻辑必须服从该文件。

## 专有名词、目标与边界

本 plan 扩展 V2 `L2TLB_agent` 的 response payload 随机能力：首次 lookup miss 创建某个 key 的 entry 时
随机生成 S1/S2 fault、level、PPN、PTE permission、PTE.N 与 PBMT；再按 `s2xlate` 形成可驱动的两阶段
payload。无 fault 的 normal leaf payload 按两阶段翻译语义生成 PPN；fault payload 只保证 response
接口可编码，不把伴随 PPN 当作正常数据访问地址。fault 同时保存 raw/effective 两层语义；同一 key 后续命中时
逐字段复用既有 entry，不重新随机。

本 plan 不修改既有 request fire 记账、response latency、顺序/乱序调度、driver hold、flush sideband 或
lifecycle owner 的所有权规则。首次建 entry 的随机结果写入 live entry；每次 accepted request 仅把该 entry
复制为 pending snapshot，后续 pending/driver 重试不得重新随机。为避免 flush 前 UID 被 flush 后 response
错误回填，本 plan 为 UID record 增加等待实例状态：既有 lifecycle 在 C4 到期时只取消已经观察到
旧 L2TLB request fire 的 `WAITING` 实例，但不新增第二个 flush lifecycle 或改变已完成 response 的时序归属。

### 本 plan 的 Coding 权威边界

执行本专项时，本文件是 response payload 改造的唯一 coding plan。coding 只需遵循本 `undo` plan 和本 plan
列出的当前源码落点，不需要回读
`AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2tlb_response_permission_adapt_execution_plan_20260708.md`。
后者只保留已归档的 lifecycle 历史说明，不能再为 S1/S2 字段、fault、PPN、permission、PBMT、sector 或
driver payload 提供第二套实现规则。

为使 coding 不依赖旧 plan，本 plan 固定以下既有 lifecycle 边界：已有 request fire/token 建立、pending
queue、latency/reorder、driver sample complete、flush/reset cancel 和 lifecycle owner 的外部时序由共享 lifecycle plan
统一定义；
payload 改造只能在 entry build、snapshot copy、UID 回填和 response drive 中替换数据字段及其校验，不能新增
第二个 pending queue、token owner 或 flush owner。实际 API 形参和调用点以本 plan 列出的当前源码为准。

SFENCE/HFENCE 的 token 时序以
`AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2tlb_sfence_flush_token_timing_correction_plan_20260805.md` 为准：monitor 在 C0 看到 event 时，
同拍已经发生的 `valid && ready` request 仍必须建立 token；C4 到期时才取消仍在 `pending_q` 的旧 token，
并取消已经观察到旧 L2TLB request fire 且仍未被已完成 response 覆盖的 UID 等待实例。一个 token 不拥有唯一 UID，因为一笔
`PtwRespS2` 可以同时命中多个 DTLB filter entry。本 plan 不得保留 `record_flush_killed_request()` 或
“C0 同拍 fire 直接取消”的旧语义。

本 plan 不拥有 lifecycle owner 的 release。只有 owner 的正常 global-stop 主循环可以调用 timing plan 的唯一
`release_grantable(owner, current_reset_epoch)`；该谓词必须同时要求 final inactive item 已完成、精确 final sample 已由 monitor
settled、driver 已在后续真实 `drv_cb` 写 final mailbox recycle proof 且 mailbox 为 `EMPTY`、release admission 的 transport 已由
cutoff sample 确认关闭、release closing、response/adapter queue drain、与当前 epoch/generation 匹配的 raw-fence intake close、
`required_reset_acks_done(current_reset_epoch, reset_required_ack_mask)` 和 `!reset_active`。该谓词不包含且不得读取 `release_granted` 或任一 grant metadata：parent 只有在
它为真时才写入匹配 owner/epoch/generation 的 grant，owner 再同时校验 grant 与该谓词后清 claim。本 plan 不得省略其中任一条件或另建局部 release gate；`do_kill()` 与
driver `phase_ended()` 永远不是 release caller，active owner 遇到它们必须 `uvm_fatal` 并保留 claim。payload 随机完成也不能
绕过 pending token、driving response、barrier 或 UID `WAITING` 检查。raw fence 与 pending invalidate 属 dispatch adapter 的
独立职责；response owner 不处理其队列，但 parent 的共享 release gate 必须确认 adapter 已 drain 且 fence monitor 已停止新 raw intake。

release gate 的 UID 检查范围固定为整个 `uid_tlb_record_by_uid`：凡 `record_valid=1` 且
`uid_tlb_wait_state==WAITING` 的 record 都必须已经收敛，不按 token、lookup key、UID 年龄或 owner 名称再做
筛选。当前测试框架同一时刻只允许一个 L2TLB lifecycle owner，且不支持把未完成 UID 等待实例交给下一 owner；
因此任何遗留 `WAITING` 都是本 owner 未完成的生命周期工作，必须阻止 release。该全表扫描只发生在正常 global-stop 的
低频 release 路径，不进入每拍 ready/response selector。parent 的 global stop 只停止新 routing；唯一 owner 在下一真实
`drv_cb` 先完成此前已驱动 ready 窗口的 request capture/UID registration，写 admission-settled watermark 后调用
`close_l2tlb_admission_for_release()` 写当前 epoch close request；从该 flag 写入之后，UID setter 和 request capture 的任何后续调用
都必须 `uvm_fatal`，此前已经完成的 registration/fire 不回溯取消。owner 的 `ready=0`、`RELEASE_STOP` item 经真实
`drv_cb` 由 driver 采样且 `sampled_req_ready=0`、`sampled_req_fire=0`、owner/generation/reset epoch 匹配后才冻结 transport cutoff sample。owner 仅在 response drain 后
发送独立的 `RELEASE_FINAL_INACTIVE` item；final item 也必须由 driver 以同一真实 `drv_cb` 冻结的
`sampled_req_ready=0`、`sampled_req_fire=0`、`sampled_resp_valid=0` 且 owner/generation/reset epoch 匹配后确认；sequence terminal ack 后，driver 仍必须在下一真实 `drv_cb` 回收该 mailbox 并写 recycle proof。stop/final item 同时冻结 current reset epoch，reset-active 或 stale epoch item 由 driver
精确执行一次 `item_done()` 后丢弃并强制 inactive，不能确认新 close/final；reset release 后先完成 current epoch 的
`NORMAL/inactive` baseline，baseline 完成前不允许创建/确认 stop 或 final；driver 真实采样后 owner 才允许调用
`begin_l2tlb_release_closing()`；adapter drain 与
这些 owner 步骤并行，由 parent 在共享 release gate 等待。closing 不执行 release，也不把旧 item 当作 final inactive。

### 本次新增术语

review 中的简称 `s1_mode_at_build/s2_mode_at_build` 在本 plan 统一采用更明确的字段名
`s1_translation_mode_at_build/s2_translation_mode_at_build`，以免与 S1/S2 PTE profile mode 参数混淆。

| 术语 | 含义 | 必要性 |
|---|---|---|
| `live entry` | `tlb_entry_by_key` 中当前可被 lookup hit 复用的 `memblock_tlb_entry`。 | 它是随机 payload 与创建时翻译 provenance 的唯一长期载体。 |
| `pending snapshot` | 每次 accepted L2TLB request 从命中 live entry 逐字段复制出的独立 response 快照。 | live entry 后续被 fence 删除或 CSR 更新时，已接受 request 的 payload 不能被回写。 |
| `UID record` | `uid_tlb_record_by_uid` 中保存发射上下文与最终绑定 payload 的历史记录。 | 它保留某个 UID 实际使用的 entry provenance，不能用后续 current CSR 覆盖。 |
| `s1_translation_mode_at_build` / `s2_translation_mode_at_build` | 新 entry 创建时分别冻结的 S1/G-stage 翻译 mode。 | level 合法性、fence 的 tag 宽度和 superpage mask 必须使用创建时 mode，不能读取消费时 CSR。 |
| `s1_pte_mode_at_build` / `s2_pte_mode_at_build` | 新 entry 创建时分别冻结的 S1/S2 PTE profile mode。 | `LEGAL`、`MIXED`、`EXCEPTION_BIASED` 的后续 LEGAL fixup 与 NAPOT 可解析性必须使用 entry 自己的 profile，不能在 pending、UID 或 response complete 阶段重新读取 plus。 |
| `S1 GVA VPN width` | 由冻结的 S1 `satp/vsatp.mode` 决定的输入 GVA page-number 有效位宽。 | Sv39 为 `39-12=27` 位，Sv48 为 `48-12=36` 位；只用于 S1 输入地址解释。 |
| `S2 GPA/GVPN width` | 由冻结的 S2 `hgatp.mode` 决定的 GPA/GVPN 有效位宽。 | Sv39x4 为 `GPAddrBitsSv39x4-offLen=29` 位，Sv48x4 为 `GPAddrBitsSv48x4-offLen=38` 位；不能用 S1 位宽替代。 |
| `s1_stage_active` / `s2_stage_active` | 该 request 的 `s2xlate` 是否要求 S1/S2 response stage。 | inactive stage 不进入该 stage 的 payload 构造；active stage 必须是受支持的 paged mode，Bare request 不进入本 framework。 |
| `s1_root_ppn_at_build` / `s2_root_ppn_at_build` | 创建 entry 时实际选用的 S1/S2 页表根 PPN provenance。 | 与 mode、tag/context 一起定位同 key 但页表根已改变的历史 payload；不参与 lookup key。 |
| `csr_context_seq_at_build` | 创建 entry 的 DTLB-side request `csr_snapshot.update_seq`。 | 用于将 payload 与创建时 CSR 版本关联；它不是 lookup key，也不单独触发失效。 |
| `entry_generation` | 每次新建或 fence 后重建 live entry 时分配的全局单调编号。 | 区分同 key 的不同时代 entry；它不能用 `csr_context_seq_at_build` 替代。 |
| `fault_raw_*` | 四个 fault 在 lookup miss 时独立抽取的原始随机结果。 | 保留完整随机历史，供 snapshot、UID 和 debug 使用，不直接驱动 DUT。 |
| `fault_effective_*` | 按 `s2xlate`、plus 权重和默认优先级收敛后的四类 TLB fault 结果。 | 它驱动 S1 PF、迁移后的 S1 AF、S2 GPF 与 S2 GAF；`pmaAF` 不属于这四类。 |
| `fault_stage_selected` | 当前 entry 被选中的终止性 fault 所属 stage；无 fault 时为 `NONE`。 | 记录 fault 归属；任一 effective fault 存在时，两套 payload 统一进入 fault passthrough，不改变 DCache lifecycle。 |
| `fault passthrough` | 任一 effective fault 存在时保留两套 stage 的 profile 后伴随 payload，不执行正常翻译合法化。 | 允许 DUT 观察 fault 与随机 PTE/PPN/PBMT 的组合，但仍受接口编码约束。 |
| `normal leaf payload` | 无 effective fault，且 active S1 response（若存在）是有效叶 PTE 的 payload；`onlyStage2` 没有 S1 时按其正常 S2 leaf 路径处理。 | 这是本 plan 唯一按 `genPPN()` 派生正常 S1/S2 地址的场景；legacy `pmaAF` 不改变该构造分类，只在最终 S1 AF 输出置位。 |
| `s1_resolved_ppn` | normal leaf 下由 S1 raw PPN、level、NAPOT、sector 与 request VPN 派生的完整 S1 PPN。 | 它对应 DUT `stage1.genPPN(request_vpn)`；`allStage` 时只作为 S2 lookup 的 GVPN 输入，不表示异常 `gpaddr`。 |
| `raw protocol GVPN anchor` | 构造 response 时按 DUT 原始 PPN/VPN 拼接规则得到的 allStage S2 tag 输入；它只保证协议字段可编码，不表示模型已经得到可消费的 resolved PPN。 | `derive_allstage_raw_s2_tag()` 的临时结果，最终写入 `entry.s2_tag`；非 LEGAL 非规范 NAPOT 仍可有该 anchor，但 `request_derived_valid` 保持 0。 |
| `S1 sector PPN split` | V2 S1 interface 把一个 canonical PPN 拆为 `s1_entry_ppn_raw` 的高位部分和每个 sector 的 `s1_ppn_low[8]` 低 3 位。 | `s1_entry_ppn_raw` 不是完整 PPN；NAPOT 判断和 `genPPN()` 必须按选中 sector 将两者联合解释。 |
| `s1_addr_low` / `s1_pteidx[8]` | `s1_addr_low` 是 3-bit 的数值 sector 位置；`s1_pteidx[8]` 是其对应的 8-bit one-hot Bool payload。 | 两者不能互相复用为同一种数据；DUT 用 `OHToUInt(s1_pteidx)` 选出实际 PTE/sector。 |
| `有效 S1 non-leaf PTE` | `s1_pte_v=1` 且 `s1_pte_r/s1_pte_w/s1_pte_x` 均为 0、并且没有 effective fault 的 S1 payload。 | 它是页表下一级指针而非最终数据页。本 plan 不建模其 `genGVPN()`/异常 GPA 路径，必须 fail-fast，不能误当 normal leaf。 |
| `request token` | 每个 accepted L2TLB request 分配的单调内部编号。 | 标识一笔 response 实例；不是 DUT interface 字段，不能由 lookup key 替代。 |
| `lifecycle owner` | 当前唯一负责 request token、pending queue、response 完成和 flush/reset cancel 时序的 responder sequence。 | 本 plan 只替换其携带的 payload，不新增第二个 owner。 |
| `post-reset baseline` | reset release 后当前 epoch 先完成的一次 `NORMAL/inactive` driver sample。 | timing plan 的 driver local pending 与 baseline done proof | baseline 先建立无 fire/无 response transport 边界；其前不能创建或确认 stop/final。 |
| `item_done()` transport release | 已由 `try_next_item()` 返回的 transaction 的 UVM sequencer 释放动作。 | `L2tlb_agent_agent_driver` | stale epoch item 必须执行一次后才丢弃；本 payload plan 不直接调用它。 |
| `admission-settled watermark` | owner 已在真实 `drv_cb` 完成本拍 request capture 与 UID registration 的完成标记。 | `l2tlb_owner_admission_settled_sample_seq` | parent 的 global stop 不直接关闭此前 ready 窗口；owner 先写该 watermark，才可封闭 admission。 |
| `owner-side admission seal` | owner 在当前 sample 已结算后写 `close_requested`、当前 reset epoch 和单调 generation，并在同一拍生成 `RELEASE_STOP/ready=0`。 | timing plan 的 `close_l2tlb_admission_for_release()` | seal 后 UID setter/capture 立即 fatal；`admission_closed/cutoff` 只确认下一拍 transport 已关闭。 |
| `raw-fence intake close` | fence monitor 在完整处理 close request 后的一个 raw sample 后，写当前 reset epoch/close generation 的 producer-close 证明。 | `l2tlb_raw_fence_intake_closed_*` | parent 的 release gate 既等 adapter queue drain，也等该 proof；response owner 不消费 raw FIFO。 |
| `UID 等待实例` | 一个逻辑 UID 在一次 issue 后、尚未获得或已失去 TLB payload 的等待状态。 | `uid_tlb_wait_epoch`、`uid_tlb_wait_state` | C4 仅取消已观察到 pre-anchor request fire 的旧实例；同一 UID 真正再次 issue 时递增 epoch。 |
| `UID request-fire marker` | 该 UID 等待实例已经被 responder 观察到对应 DTLB `valid && ready` request fire 的首个 DUT global sample；它不是 token-to-UID 绑定。 | `uid_tlb_first_request_fire_sample_seq`、`uid_waiting_by_vpn_s2xlate` | C0 fire 的 UID 在 C4 可取消；C0 后尚未 fire 的等待实例不被同一 barrier 误取消。 |
| `response-to-UID multicast` | 一笔已经驱动的 `PtwRespS2` 用与 DUT 相同的 raw hit 语义匹配全部 `WAITING` UID record。 | `complete_waiting_uid_records_by_response()` | 一个 response 可以同时使多个同 key 或同 range UID 变为 `COMPLETED`。 |
| `DTLB-side CSR snapshot` | 与当前 L2TLB request 所在的 DTLB filter 输入拍对齐的 CSR 历史快照。 | `get_l2tlb_request_csr_snapshot()` | 顶层 C0 CSR 在两级 `RegNext` 后 C2 才到达 filter，因此 C0 request 使用 C-2 snapshot。 |
| `response-visible CSR snapshot` | response external fire 的当前 `PTWNewFilter.io.csr` 所见 CSR 历史快照。 | `complete_driving_response()` 以当前 DUT global sample 查询 C-2 项。 | C0 CSR change 后，C2 response hit 已按新 ASID/VMID 判断，不能继续按 UID 建立时的旧 CSR 回填。 |
| `公共构造入口` | `common_data_transaction::build_tlb_entry_for_key_with_csr()`。 | lookup miss 的唯一公开 entry 创建入口；负责调用 builder core、分配 generation 并返回完整 entry。 |
| `builder core` | `tlb_map_builder::build_payload_for_key_with_csr()`。 | 只构造本 plan 定义的 S1/S2 payload；不管理 live table、pending queue、token 或 generation。 |
| `legacy builder wrapper` | `tlb_map_builder::build_tlb_entry_for_req()`。 | 保留旧调用签名；只把 `vpn/s2xlate` 转为 key 后转调 builder core，不再执行旧共享 PTE/地址构造。 |
| `MONITOR ack` | L2TLB monitor 同步消费匹配 reset-active transport sample 后返回的 reset 确认；它不等于 final sample settled。 | `l2tlb_monitor_reset_ack_epoch/transport_sample_seq`、timing plan `monitor_reset_ackable()` | 当前 reset epoch 的 required ack 未收齐时，release 不能发生。 |
| `sample mailbox` | driver 与唯一 response owner 之间的单槽 transport sample 状态机；monitor 通过同步 analysis imp 观察同一个 frozen wrapper，不形成 FIFO。 | `L2tlb_agent_agent_sequencer` 的 `EMPTY -> PUBLISHED -> CONSUMED/DROPPED -> EMPTY` slot | owner ack 终态，driver 在后续 `drv_cb` 唯一回收；payload helper 不得直接写 slot。 |
| `final mailbox recycle proof` | final sample 已被 owner 终态确认后，driver 在下一真实 `drv_cb` 回收该 slot 的证明。 | `l2tlb_release_final_inactive_transport_sample_seq`、`l2tlb_transport_sample_recycle_done_seq` | 两个序号相等且 mailbox 为 `EMPTY` 才满足 release 的 transport 回收条件。 |
| `direct writer` | 对一个 runtime state 直接清理并写 ack/proof 的唯一职责组件；coordinator 只请求和等待。 | driver、monitor、response owner、fence monitor、adapter、CSR monitor | 本 payload plan 只能读取完整 release 谓词，不能替代 driver 回收 slot 或替代 monitor 回 ack。 |

### 构造入口与旧 Builder 收敛

当前实际调用是：

```text
common_data_transaction::build_tlb_entry_for_key_with_csr()
  -> tlb_map_builder::build_tlb_entry_for_req()
  -> update_addr_fields() / choose_paddr() / randomize_pte_bits()
  -> 全局 fixup_pte_legal()
```

旧 `build_tlb_entry_for_req()` 若直接反向调用同名的
`common_data_transaction::build_tlb_entry_for_key_with_csr()`，会因当前公共入口已调用旧 builder 而递归。
因此本 plan 使用一个 builder 私有 core 收敛为单向调用，不引入第二个 live-table owner：

```text
lookup miss
  -> common_data_transaction::build_tlb_entry_for_key_with_csr(key, csr_snapshot)
  -> tlb_map_builder::build_payload_for_key_with_csr(key, csr_snapshot)
  -> 完整新 S1/S2 entry payload

旧调用者
  -> tlb_map_builder::build_tlb_entry_for_req(vpn, s2xlate, csr_snapshot)
  -> csr_snapshot.make_lookup_key({26'b0, vpn}, s2xlate)
  -> build_payload_for_key_with_csr(key, csr_snapshot)
```

抽象功能描述：`build_payload_for_key_with_csr()` 在 lookup miss 的公共入口已经冻结 key 和 CSR snapshot 后，
只创建并填充一份新格式 `memblock_tlb_entry`；它不插入 `tlb_entry_by_key`、不分配
`entry_generation`、不创建 token，也不改变 flush/reset 生命周期。

```text
build_payload_for_key_with_csr(key, csr_snapshot):
  创建并 reset 新 entry，写入 key、stage activity、冻结 mode/root 与 CSR provenance。
  先随机 raw PTE 字段并冻结 profile、候选 level；再选择 effective fault。
  只有无 effective fault 的 LEGAL stage 执行 PTE/level 合法化；最后构造 raw PPN、sector 与 PBMT。
  执行 BUILD 一致性校验后返回 entry。
  不调用 update_addr_fields()、choose_paddr()、randomize_pte_bits() 或旧全局 fixup_pte_legal()。
```

旧 `build_tlb_entry_for_req()` 仅保留源码签名兼容；正常 lookup-miss 路径不再调用它，任何需要插入
`tlb_entry_by_key` 或取得 `entry_generation` 的调用者必须改用公共构造入口。上述四个旧 helper 可以保留给
非本专项的历史代码，但不得参与新 entry 创建，也不得作为 random payload 构造失败时的 fallback。

### 旧 Fault 字段迁移与 `pmaAF` 边界

旧四类之外的 `pmaAF` 不自动迁入本 plan 的四类 L2TLB fault，因为它可能表示 PMA/PMP 访问异常。本 plan 只做
以下明确字段迁移：

```text
legacy tlbPF   -> fault_effective_s1_pf
legacy tlbAF   -> fault_effective_s1_af
legacy tlbGPF  -> fault_effective_s2_gpf
新 S2 GAF     -> fault_effective_s2_gaf（仅由本 plan 的新随机/定向入口生成）
```

上述箭头是旧 entry 字段、snapshot/UID copy 字段和 response mapping 的单向迁移规则，不是运行期
`legacy_* || fault_raw_*` 候选合并。迁移完成后不得同时保留旧 `tlbPF/tlbAF/tlbGPF` 与新四类 fault 作为
两套并行 response 数据模型。

`pmaAF` 保留原有独立字段和尾端 response 语义：live entry、pending snapshot 与 UID record 继续逐字段 copy
该值；它不进入 `fault_raw_*`、`fault_effective_*`、fault 权重、优先级或 `fault_stage_selected`。本 plan 不新增
`main_control_transaction.pmaAF` 到 entry 的建表写者、不新增 PMA/PMP model、plus 或 directed 注入入口；默认仍为
0。现有或未来的独立 PMA/PMP 专项若直接提供 `entry.pmaAF=1`，`fill_dtlb_resp_from_entry()` 保持：

```text
if entry.pmaAF && (fault_effective_s1_pf || fault_effective_s1_af ||
                   fault_effective_s2_gpf || fault_effective_s2_gaf):
  uvm_fatal("L2TLB_PMA_FAULT_MIX", key, pmaAF, fault_effective_*)

io_ptw_resp_bits_s1_pf  = fault_effective_s1_pf
io_ptw_resp_bits_s1_af  = fault_effective_s1_af || entry.pmaAF
io_ptw_resp_bits_s2_gpf = fault_effective_s2_gpf
io_ptw_resp_bits_s2_gaf = fault_effective_s2_gaf
```

四个 `fault_effective_*` 仍保持至多一个为 1；`pmaAF` 是四类模型外的 legacy sideband，不参与该选择，
但在最终 response 前必须与四类 effective fault 互斥。该 fatal 只拒绝当前未建模的 mixed PMA/TLB fault；
单独 `pmaAF=1` 继续得到原有 S1 AF 输出。本 plan 不以新随机方式生成这种组合。
`pmaAF` 只保留上述 S1 AF response 尾端，不触发本 plan 的 `fault_stage_selected`、normal-leaf/PAddr 派生、
DCache owner 或异常提交建模；这些 PMA/PMP 专有语义留给后续独立专项。

## 一、公共 plus 参数

所有新参数走 `env/plus.sv -> seq_csr_common.sv -> getter -> L2TLB sequence`；默认 preset 保留现有
稳定 response 行为，testcase 通过 `seq/plus_cfg/*.cfg` 选择特定随机 profile。

### 1. S1/S2 level 权重使能

新增全局布尔 plus `MEMBLOCK_L2TLB_LEVEL_WEIGHT_EN`，合法值仅为 `0/1`，只控制 S1/S2 level 的四档权重是否
参与 entry 构造，不控制 fault、PTE、PBMT 或其他字段的随机。其默认值为 `0`。

| `MEMBLOCK_L2TLB_LEVEL_WEIGHT_EN` | S1 level | S2 level | level 权重参数 |
|---:|---|---|---|
| `0` | 固定为 `0` | 固定为 `0` | 不参与选择或权重合法性检查，也不因全 0 而报错 |
| `1` | 对当前有效 S1 stage 按权重选择 | 对当前有效 S2 stage 按权重选择 | 启用并按冻结的 request-time CSR mode 过滤候选 |

`LEVEL_WEIGHT_EN=1` 时，sequence 配置阶段必须检查
`MEMBLOCK_MAIN_MEM_RANGES_EN=0`；否则立即 `uvm_fatal`，不得接受 request 后才报错。该组合表示 DCache/Uncache
responder 采用 48-bit sparse memory 服务最终物理地址，不把 `MEMBLOCK_PADDR_BASE/RANGE` 当作随机 level
response 的严格可访问窗口。`MEMBLOCK_MAIN_MEM_RANGES_EN=0` 本身不改变 TLB PPN payload。无 effective fault
的 LEGAL normal leaf 路径中，PPN 构造必须满足 interface 位宽、superpage 对齐、NAPOT 编码、S1 GVA VPN 宽度及 S2
GPA/GVPN 宽度约束；MIXED/EXCEPTION_BIASED 的非 canonical NAPOT raw payload 只是不产生 model-derived PPN。
fault passthrough 路径只检查每个已驱动字段可由接口位宽表示，不执行 allStage/onlyStage2 GPA/GVPN 语义校验，
也不把 fault PPN 转成正常物理地址。

新增纯校验 helper `check_l2tlb_payload_weight_cfg()`，在 sequence 启动、开启 request ready 前完成可静态判断的
组合、取值范围与 PBMT 全零检查。

抽象功能描述：`check_l2tlb_payload_weight_cfg()` 在 responder 尚未接受任何 request 时读取已经冻结的 getter
参数，拒绝无法形成有效随机分布的配置；它只报告配置问题，不选择 level/PBMT、不修改 live entry、pending
snapshot 或 driver 状态。依赖冻结 request-time CSR mode 的 level 候选全零检查由实际选择前的 level flow 完成。

### 2. Fault 为 1 的权重

为以下四个字段分别提供“随机为 1”的 plus 权重：

| 字段 | 参数名 |
|---|---|
| `s1_pf` | `MEMBLOCK_L2TLB_S1_PF_1_WT` |
| `s1_af` | `MEMBLOCK_L2TLB_S1_AF_1_WT` |
| `s2_gpf` | `MEMBLOCK_L2TLB_S2_GPF_1_WT` |
| `s2_gaf` | `MEMBLOCK_L2TLB_S2_GAF_1_WT` |

每个 lookup miss 的新 entry 均先按四组权重独立取得四个 `fault_raw_*` 结果，再进入 `s2xlate` 修正；不因某个
stage 当前不生效而跳过其随机取值。`fault_raw_*` 与 `fault_effective_*` 都写入 live entry，lookup hit、
pending snapshot 和 driver 重驱均直接复用已保存的两组结果。

### 3. Level、PTE.N、permission 与 PBMT 权重

| 功能 | 参数命名规则 | 数量 |
|---|---|---:|
| S1/S2 level | `MEMBLOCK_L2TLB_S{1,2}_LEVEL_{0,1,2,3}_WT` | 8 组 |
| S1/S2 PTE.N 为 1 | `MEMBLOCK_L2TLB_S{1,2}_PTE_N_1_WT` | 2 组 |
| S1 `R/W/X/U/G/A/D/V` 为 1 | `MEMBLOCK_L2TLB_S1_PTE_{R,W,X,U,G,A,D,V}_1_WT` | 8 组 |
| S2 `R/W/X/U/G/A/D` 为 1 | `MEMBLOCK_L2TLB_S2_PTE_{R,W,X,U,G,A,D}_1_WT` | 7 组 |
| S1/S2 PBMT 三种编码 | `MEMBLOCK_L2TLB_S{1,2}_PBMT_{0,1,2}_WT` | 6 组 |

布尔字段的 `_1_WT` 合法范围为 `0..100`，其随机为 0 的权重固定为 `100 - _1_WT`；因此 `_1_WT=0`
表示永不随机为 1，`_1_WT=100` 表示必定随机为 1。所有 `PBMT_*_WT` 必须是 `0..100` 的非负整数；
`LEVEL_WEIGHT_EN=1` 时，参与选择的 `LEVEL_*_WT` 也必须是 `0..100` 的非负整数。多值权重是同一分布内
各候选的**相对权重**，总和不要求为 100；权重为 0 只表示该候选不参加本次分布，不表示另一个字段被置 1。

每个 stage 的 level 和 PBMT 都是标量枚举：一次 lookup miss 对一个 stage 的某一类字段只产生一个结果，
不能把 `LEVEL_0/1/2/3_WT` 或 `PBMT_0/1/2_WT` 当作彼此独立的 bit 随机，也不适用 fault 的“多个为 1 后按
优先级收敛”逻辑。实现必须直接复用 SystemVerilog `dist` 完成一次多选一选择，不实现自定义的“累加总权重
→ 随机阈值 → 遍历候选”选择器。

`check_l2tlb_payload_weight_cfg()` 在 sequence 配置阶段检查 PBMT 权重均在 `0..100`，并检查 S1/S2 各自
三项 PBMT 权重不全为 0；任一失败立即 `uvm_fatal`，不静默回退为 PBMT 0。`LEVEL_WEIGHT_EN=0` 时，八个
level 权重刻意不参与 level 选择和合法性检查，包含范围检查；`LEVEL_WEIGHT_EN=1` 时才检查参与选择的
`LEVEL_*_WT` 范围，且当前有效 stage 经冻结 request-time CSR mode 过滤后的 level 候选权重不能全为 0。后者
只能在首次遇到该 mode 的选择前检查，失败时 `uvm_fatal` 并打印 stage、CSR mode、有效候选和四档原始权重；
不执行固定值 fallback 或重新随机。

### 4. 旧共享 PTE 参数迁移为 S1，并新增 S2 可驱动字段参数

现有 `MEMBLOCK_TLB_PTE_<FIELD>_1_WT` 参数不再表示共享 PTE。S1 迁移并重命名为
`MEMBLOCK_L2TLB_S1_PTE_<FIELD>_1_WT`，其中 `<FIELD>` 覆盖 `R/W/X/U/G/A/D/N/V`；S2 只新增实际可驱动的
`MEMBLOCK_L2TLB_S2_PTE_<FIELD>_1_WT`，其中 `<FIELD>` 覆盖 `R/W/X/U/G/A/D/N`，明确不包含 `V`。

V2 response interface 只有 `io_ptw_resp_bits_s1_entry_v`，没有 `s2_entry_v`。因此
`MEMBLOCK_L2TLB_S2_PTE_V_1_WT`、`s2_pte_v`、对应 getter/preset/debug 字段和任何 model-only consumer
均不得新增或保留；不得用常量、复制 S1 值或重解释 `s2_gpf/gaf` 来伪造一个 S2 `V` 字段。S1 的
`MEMBLOCK_L2TLB_S1_PTE_V_1_WT` 保留，并只控制 S1 `entry_v` payload。

旧 `MEMBLOCK_TLB_PTE_<FIELD>_0_WT` 不迁移，也不保留兼容扫描；新规则统一由
`100 - MEMBLOCK_L2TLB_S{1,2}_PTE_<FIELD>_1_WT` 得到随机为 0 的权重。所有旧名称的定义、加载、
`seq_csr_common` 字段/getter、preset 和 consumer 必须在同一变更中删除或改为新名称，避免存在两套参数
权威。

原 `MEMBLOCK_TLB_PTE_MODE` 删除并拆分为 `MEMBLOCK_L2TLB_S1_PTE_MODE` 与
`MEMBLOCK_L2TLB_S2_PTE_MODE`，不保留旧名称兼容。每一套 mode 独立取
`LEGAL/MIXED/EXCEPTION_BIASED`，只影响本 stage 的 permission profile 及 `LEGAL` 合法化，不得把 S1
mode 或修正结果传播到 S2。需要两阶段使用同一 profile/合法化策略时，由 testcase preset 将两个新参数设成
相同值。

旧共享 level 控制 `MEMBLOCK_TLB_LEVEL_MODE`、`MEMBLOCK_TLB_LEVEL_FIXED_VALUE`、
`MEMBLOCK_TLB_LEVEL_RANDOM_LOW/HIGH` 不迁移为共享兼容参数；统一由
`MEMBLOCK_L2TLB_LEVEL_WEIGHT_EN` 与本 plan 的
`MEMBLOCK_L2TLB_S1_LEVEL_{0,1,2,3}_WT`、`MEMBLOCK_L2TLB_S2_LEVEL_{0,1,2,3}_WT` 替代。旧 PBMT
控制参数/consumer 同样分别迁移为 S1/S2 三组 PBMT 权重及其对应 getter，不保留旧共享入口。

`MEMBLOCK_L2TLB_LEVEL_WEIGHT_EN` 默认固定为 `0`，因此默认运行不使用 level 权重、S1/S2 均为 4 KiB
`level=0`。默认值固定如下，以保持基础 response 为 4 KiB、normal PBMT 的稳定行为：

| 参数组 | S1 默认值 | S2 默认值 |
|---|---|---|
| PTE mode | `MEMBLOCK_L2TLB_S1_PTE_MODE=0`（`LEGAL`） | `MEMBLOCK_L2TLB_S2_PTE_MODE=0`（`LEGAL`） |
| level 权重 | `LEVEL_0_WT=100`，`LEVEL_1/2/3_WT=0` | `LEVEL_0_WT=100`，`LEVEL_1/2/3_WT=0` |
| PBMT 权重 | `PBMT_0_WT=100`，`PBMT_1/2_WT=0` | `PBMT_0_WT=100`，`PBMT_1/2_WT=0` |
| fault 为 1 权重 | `S1_PF_1_WT=0`，`S1_AF_1_WT=0` | `S2_GPF_1_WT=0`，`S2_GAF_1_WT=0` |
| PTE.N 为 1 权重 | `S1_PTE_N_1_WT=0` | `S2_PTE_N_1_WT=0` |

S1 的 `R/W/X/U/G/A/D/V` 默认 `_1_WT` 保持旧共享 PTE 默认概率，并折算到本 plan 的 0–100
权重规则：`R=89`、`W=86`、`X=80`、`U=11`、`G=11`、`A=89`、`D=89`、`V=90`。S2 只对
`R/W/X/U/G/A/D` 使用相同的前七项默认值，不定义 `V` 权重。对应随机为 0 的权重继续由
`100 - _1_WT` 派生；同名 S1/S2 可驱动字段默认值相同，但后续可由各自 plus 独立覆盖。

## 二、Fault 随机与 `s2xlate` 优先级修正

### Fault 选择职责

每个新建 entry 先独立随机 `fault_raw_s1_pf/fault_raw_s1_af/fault_raw_s2_gpf/fault_raw_s2_gaf`，再按当前
request 的 `s2xlate` 和优先级生成 `fault_effective_*`。raw 结果只用于历史、UID 和 debug；四类 modeled
fault 只有 effective 结果驱动对应 DUT fault interface。`pmaAF` 保持独立 legacy sideband，按本章定义仅参与
S1 AF 的尾端 OR。entry 的 effective fault 始终至多一个字段为 `1`，同 key hit 直接复用 raw/effective
两组结果以及全部 S1/S2 payload。

本 plan 采用以下固定默认优先级，仅作为“多个原始随机字段同时为 1 且权重相同”时的确定性 tie-break：

| 默认优先级 | 字段 | 对应 plus 权重 |
|---:|---|---|
| 4 | `s2_gaf` | `MEMBLOCK_L2TLB_S2_GAF_1_WT` |
| 3 | `s1_af` | `MEMBLOCK_L2TLB_S1_AF_1_WT` |
| 2 | `s1_pf` | `MEMBLOCK_L2TLB_S1_PF_1_WT` |
| 1 | `s2_gpf` | `MEMBLOCK_L2TLB_S2_GPF_1_WT` |

`s2xlate` 对应的有效候选字段如下：

| `s2xlate` | 有效候选字段 | 非生效字段处理 |
|---|---|---|
| `noS2xlate` / `onlyStage1` | `fault_raw_s1_pf`、`fault_raw_s1_af` | `fault_effective_s2_gpf/s2_gaf=0`；S2 raw 保留 |
| `onlyStage2` | `fault_raw_s2_gpf`、`fault_raw_s2_gaf` | `fault_effective_s1_pf/s1_af=0`；S1 raw 保留 |
| `allStage` | 四个 raw fault | 四个字段统一参与 effective 唯一 fault 选择 |

唯一 fault 选择规则：

1. 对每个 lookup miss 的新 entry，四个 raw fault 都先按各自 `_1_WT` 完成随机；raw 值不被后续步骤改写。
2. 按 `s2xlate` 形成 effective 候选；非生效候选的 effective 值为 0，但对应 raw 值继续保留。若剩余候选
   没有任何字段为 1，则四个 effective fault 均为 0。
3. 若只剩一个为 1 的候选，直接选中该字段。
4. 若有多个为 1 的候选，先比较这些候选对应的 plus 权重，权重最大的字段胜出。
5. 若最高权重仍有多个候选，按上表默认优先级选择唯一字段。
6. 先把四个 effective fault 初始化为 0，再只把胜出字段置为 1；`fault_stage_selected` 根据胜出字段归类
   为 `S1`、`S2` 或 `NONE`。该清零只作用于 effective interface 值，不作用于 raw/debug 值。

这是 testbench 的统一随机收敛规则，不试图用一个扁平四字段表复刻所有 PTW/LLPTW/HPTW 内部 producer
分支；debug 必须同时记录 raw fault、effective 候选、各字段权重、最终选中字段、`fault_stage_selected`
和 `s2xlate`。

### Effective fault 对 payload 合法化的边界

抽象功能描述：fault payload gate 在 effective fault 已选择后，决定当前 entry 是否继续执行正常翻译的
PTE 语义合法化。它只改变 payload 构造分支，不删除 entry、不取消 pending request，也不建立 DCache owner。

1. 先随机两套 raw permission/PTE.N 并调用一次 `apply_pte_profile()`，再按冻结 translation mode 选择候选 level。
   此时尚未构造 PPN、sector 或 PBMT。
2. 完成唯一 effective fault 选择。`fault_stage_selected == NONE` 时，只有 `LEGAL` stage 执行
   `fixup_pte_legal()`，并在 `PTE.N=1` 时把最终 level 收敛为 0。
3. effective fault 已确定后才构造 raw PPN、S1 sector split 和 PBMT。`fault_stage_selected != NONE` 时，两套
   stage 保留已冻结的 raw PTE/profile 和候选 level，不执行 `PTE.N -> level=0` 或 `LEGAL` permission/V/A/D
   合法化；随后生成的 raw PPN、sector 与 PBMT 只满足 response 接口编码范围。`fault_stage_selected` 只记录
   胜出 fault 属于 S1 还是 S2，不改变另一套 payload 的保存范围。
4. `fault_stage_selected != NONE` 时只检查 response interface 的字段位宽和编码范围。不得为了 fault PPN
   额外执行 `genGVPN()`、异常 `gpaddr`、allStage S2 GVPN 范围或最终 PAddr 的精确派生；这些值不决定
   DUT 是否把本请求送往 DCache。
5. 任一 effective fault 被选中后，`s1_resolved_ppn/s2_resolved_ppn` 若因既有数据结构而保留，只能作为无
   功能语义的确定性 debug 值；不能建立正常 DCache owner、memory line、地址 alias，不能作为 fault 清除或
   response complete 条件。DCache 是否有请求只由 DUT 实际行为决定。
6. `fault_stage_selected == NONE` 且 active S1 payload 为 `V=1 && R=W=X=0` 时，立即 `uvm_fatal` 并输出
   lookup key、S1 raw PPN、level 和 PTE profile。该组合是有效 non-leaf PTE；本 plan 既不模拟继续 page walk，
   也不模拟它的 `genGVPN()`/异常 GPA，不能静默按 leaf PPN 返回。

## 三、独立 S1/S2 Level 与 PPN 构造

### Level/PPN 构造职责

S1/S2 level 不再从同一 entry level/PPN 复制。`MEMBLOCK_L2TLB_LEVEL_WEIGHT_EN` 决定两阶段的 level
是固定 4 KiB，还是按各自四档权重在 lookup miss 时随机一次；lookup hit、pending snapshot 和 driver
重驱始终复用已经确定的 level，不得再次随机。

### Level 选择与 CSR mode 过滤

| `LEVEL_WEIGHT_EN` | 行为 |
|---:|---|
| `0` | `s1_level=0`、`s2_level=0`；两套 level 权重完全忽略。 |
| `1` | 只对当前 `s2xlate` 的有效 stage 按其 level 权重选择；非生效 stage 的 level 固定为 `0`，不产生没有翻译语义的随机 level。 |

有效 stage 的候选必须从 DTLB-side request CSR snapshot 的对应 mode 过滤后选择。entry 构造开始时先冻结
`s1_stage_active/s2_stage_active`、`s1_translation_mode_at_build/s2_translation_mode_at_build` 与对应 root PPN；本节及后续所有
level、PPN、fence range consumer 都只读取这些冻结字段，不在 entry 建立后重新读取 current CSR。

| `s2xlate` | `s1_stage_active` 与 S1 mode/root 来源 | `s2_stage_active` 与 S2 mode/root 来源 |
|---|---|---|
| `noS2xlate` | `1`；`satp.mode/satp.ppn` | `0`；mode/root 字段清零 |
| `onlyStage1` | `1`；`vsatp.mode/vsatp.ppn` | `0`；mode/root 字段清零 |
| `onlyStage2` | `0`；mode/root 字段清零 | `1`；`hgatp.mode/hgatp.ppn` |
| `allStage` | `1`；`vsatp.mode/vsatp.ppn` | `1`；`hgatp.mode/hgatp.ppn` |

`*_stage_active` 只用于区分由 `s2xlate` 排除的 inactive stage 与需要构造的 active stage。entry builder 在随机
payload 前必须验证每个 active stage 的冻结 translation mode 属于本 plan 支持的 paged mode；active S1/S2 的
mode 为 Bare 时，该 request 不应到达 L2TLB responder，立即 `uvm_fatal`，不创建 live entry、pending snapshot 或
UID payload。inactive stage 保持初始化默认 payload，不进入本 stage 的 level/PPN/PTE/profile/PBMT 构造；这不改变
本 plan 已定义的四个 `fault_raw_*` debug 采样及其 effective 候选屏蔽规则。

| 有效 stage mode | 可选择 level |
|---|---|
| S1/S2 Bare | active stage 不支持；在 entry builder 的输入检查处 `uvm_fatal`，不进入 level 选择。 |
| S1 Sv39 / S2 Sv39x4 | `0`、`1`、`2` |
| S1 Sv48 / S2 Sv48x4 | `0`、`1`、`2`、`3` |

`LEVEL_WEIGHT_EN=1` 时，先过滤掉冻结 mode 不支持的 level，再对剩余权重进行一次加权选择；若剩余候选
权重全为 0，必须 `uvm_fatal` 并打印 stage、CSR mode 和四档权重。无 effective fault 的正常翻译路径中，
`s1_pte_n=1` 或 `s2_pte_n=1` 才将对应最终 level 修正为 `0`；存在任一 effective fault 时，两套 stage
均保留已选候选 level，不因 fault 再做该语义覆盖。

#### Level 的单次 `dist` 选择与失败策略

抽象功能描述：level 选择 flow 在 lookup miss 创建有效 stage 的 payload 时，以该 entry 已冻结的
`*_translation_mode_at_build` 和该 stage 四项 getter 权重产生一个候选 level；它只返回一个临时枚举结果，后续仍由 PTE.N 修正和 PPN construction
helper 负责形成最终 level/raw PPN，不维护第二套 level 状态。

1. `LEVEL_WEIGHT_EN=0` 或当前 `*_stage_active=0` 时，直接返回 `level=0`，不调用 `dist`，也不检查四项
   level 权重。若 `*_stage_active=1` 且冻结 mode 为 Bare，则此前的 entry builder 输入检查必须已经
   `uvm_fatal`；level helper 不提供 Bare fallback。
2. 否则先按该 stage 已冻结的 `*_translation_mode_at_build` 形成候选集：Sv39/Sv39x4 为 `{0,1,2}`，Sv48/Sv48x4 为
   `{0,1,2,3}`。不支持的 level
   不得进入本次 `dist` 分布，也不得通过后续截断或重随机补救。
3. 对候选集中每个值读取对应的 `LEVEL_<n>_WT`；若总权重为 0，则在随机前 `uvm_fatal`。这是配置/上下文
   不可构造错误，不是 DUT fault 激励，也不允许回退到 `level=0`。
4. 总权重非 0 时，以等价于 `chosen_level dist { candidate_level := candidate_weight, ... }` 的单次标准
   SystemVerilog `dist` 约束随机一个 `chosen_level`。单次调用只能得到一个 level；不做 priority、第二次选择
   或 retry。
5. 若 `fault_stage_selected == NONE`，按本 plan 已定义的 `PTE.N=1 -> final level=0` 规则覆盖临时结果；
   覆盖不重新选择、不要求 level 0 权重非 0。若存在任一 effective fault，则 S1/S2 均保留临时 level。

该 flow 处于 lookup-miss 构建期，不在每拍 responder 主循环或 pending/driver 重驱路径执行。随机器失败、
mode 不能映射到候选集或参数值越界均属于结构性错误，立即 `uvm_fatal`；不建立有限或无界的重新随机循环。

### 原始 PPN payload 与最终解析 PPN

每个 stage 必须保存并区分以下两类字段：

```text
s1_entry_ppn_raw：
  写入 S1 response interface 的 PPN 高位部分；V2 sector 编码中它表示 canonical S1 PPN 的 `[高位:3]`，
  不是包含低 3 位的完整 PPN。

s1_ppn_low[0:7]：
  分别写入八个 `s1_ppn_low_*` interface 字段；每项是对应 sector 的 canonical S1 PPN `[2:0]`。

s2_entry_ppn_raw：
  写入 S2 response interface 的原始 PTE PPN payload；S2 不使用 S1 的 sector `ppn_low[]` 拆分规则。

s1_resolved_ppn / s2_resolved_ppn：
  仅在无 effective fault 的 normal leaf 路径中，用原始 PPN、最终 level、请求 VPN/GVPN、PTE.N 与
  S1 sector 字段合成的完整翻译 PPN；不独立随机，只能由固定 helper 派生。
  entry build 时它们是构造期局部值，可用于形成初次 response 的 S2 tag；range hit 后的权威值必须是
  pending.request_s1_resolved_ppn/request_s2_resolved_ppn，不能复用 live entry 的 anchor 值。
```

无 effective fault 的 normal leaf 翻译路径中，level 决定最终 PPN 中由输入 VPN/GVPN 补齐的低位：level `1/2/3`
分别补齐低 `9/18/27` 位；raw PPN 按对应 superpage 对齐编码。任意 profile 的 `PTE.N=1` 都将最终 level
收敛为 `0`。但仅该 stage 为 `LEGAL` 时，才必须先以 `s1_addr_low` 选中的 sector 重组成 canonical PPN 低 4 位：
`{s1_entry_ppn_raw[0], s1_ppn_low[s1_addr_low][2:0]} == 4'b1000`；不得把 `s1_entry_ppn_raw[3:0]`
当作 canonical PPN 的低 4 位。S2 的 LEGAL NAPOT 同样必须使 `s2_entry_ppn_raw[3:0]==4'b1000`。
MIXED/EXCEPTION_BIASED 的无 fault `PTE.N=1` 保留 profile 后的 raw PPN 低位，不由本 plan 强制改成合法编码；若
低位恰好符合 `4'b1000`，可以正常派生 model PPN，否则 raw response 仍可驱动/lookup，但 model derived PPN 必须保持无效。
存在任一 effective fault 时，S1/S2 均不执行上述语义合法化，不因 fault 覆写已生成的 raw PPN、level 或 PTE.N；
只须满足接口位宽和字段可表示性。两套 fault-path resolved PPN 若被既有数据结构保存，只作为确定性 debug 值，
不作为正常 DCache 映射资格，也不要求与 DUT `genGVPN()` 生成的异常 GPA 一致。

#### LEGAL NAPOT PPN 直接编码

抽象功能描述：`apply_legal_napot_ppn_encoding()` 在 normal leaf 的最终 level 已确定、S1 sector split 或 S2
raw response PPN 写入之前，为本 stage 的 LEGAL NAPOT payload 直接写入 V2 支持的 PPN 低位编码。它只修正
canonical PPN 编码，不随机 permission、不改变 fault 选择、不派生 resolved PPN。

输入：stage、该 stage 的 PTE mode、`fault_stage_selected`、最终 level、该 stage `pte_n`、待编码的 canonical PPN。

输出/副作用：仅在满足 LEGAL NAPOT 前置条件时写 canonical PPN `[3:0]=4'b1000`；否则不修改 PPN。

~~~text
apply_legal_napot_ppn_encoding(stage, pte_mode, fault_stage_selected,
                                final_level, pte_n, ref canonical_ppn):
    若 fault_stage_selected != NONE：直接返回；fault passthrough 不合法化 PPN。
    若 pte_mode != LEGAL 或 pte_n == 0：直接返回。
    若 final_level != 0：uvm_fatal；PTE.N=1 的无 fault LEGAL stage 未被 level fixup 收敛为 0。
    canonical_ppn[3:0] = 4'b1000。

S1 调用后：
    将已经编码的 canonical_ppn 交给 build_s1_sector_payload() split。
    因此 selected sector 必须得到 s1_entry_ppn_raw[0]=1 和
    s1_ppn_low[s1_addr_low]=3'b000。

S2 调用后：
    将 canonical_ppn 直接写入 s2_entry_ppn_raw；其 [3:0] 必须为 4'b1000。
~~~

中文文字伪代码：LEGAL stage 的 NAPOT 编码是构造期直接赋值，不是“先随机错误低位、再由 range checker 修复”。
`PTE.N=1 -> final_level=0` 仍由既有 level 收敛规则完成；本 helper 随后直接固定低四位。S1 必须先固定完整
canonical PPN，再拆为 `s1_entry_ppn_raw + s1_ppn_low[]`，不能只改其中一侧；S2 没有 sector split，可直接写 raw
PPN 低四位。MIXED/EXCEPTION_BIASED 的无 fault normal leaf 继续走既有 PPN 构造规则，effective-fault payload
继续走 passthrough；本 helper 不对这两类路径额外施加 LEGAL NAPOT fixup。

`resolve_s1_ppn()` 只在 normal leaf 且 NAPOT raw 编码可解释的路径以 request GVA VPN、`s1_entry_ppn_raw`、S1 sector
raw 字段、S1 level 和 S1 PTE.N 生成 `s1_resolved_ppn`；request GVA VPN 的有效位宽只由冻结的
`s1_translation_mode_at_build` 决定：Sv39 为 `39 - 12 = 27` 位，Sv48 为 `48 - 12 = 36` 位。Bare active stage
在进入 resolver 前已被拒绝。`resolve_s2_ppn()` 同样只在 normal leaf 且 NAPOT raw 编码可解释的路径以
`s2_entry_ppn_raw`、S2 level、S2 PTE.N 及 S2 输入 GVPN 生成 `s2_resolved_ppn`；S2 输入 GVPN 的有效位宽只由冻结的 `s2_translation_mode_at_build`
（request-time `hgatp.mode`）决定：Sv39x4 为 `GPAddrBitsSv39x4 - offLen = 29` 位，Sv48x4 为
`GPAddrBitsSv48x4 - offLen = 38` 位。Bare active stage 在进入 resolver 前已被拒绝。S1 的 27/36 位不能作为 S2
GPA/GVPN 的位宽，S2 的 29/38 位也不能反过来解释 S1 GVA VPN。

`napot_raw_ppn_is_model_resolvable(stage_fields)` 是 resolver 的前置检查：`PTE.N=0` 直接返回真；`LEGAL && PTE.N=1`
要求正确 `4'b1000` 编码，若不满足则 `uvm_fatal`，因为 builder 漏掉了确定性合法化；MIXED/EXCEPTION_BIASED
的 `PTE.N=1` 若 raw low-4 不是 `4'b1000`，返回假并只记录 `UVM_LOW`，不得改写 raw PPN、重新随机或把它当作
normal DCache 地址。range raw matcher 仍可按 DUT 可见 `PTE.N`/tag shape 使用该 entry。

`onlyStage2` 的 normal leaf S2 输入 GVPN 直接来自 request VPN；在调用 `resolve_s2_ppn()`、生成 `s2_tag` 或建立
正常 S2 派生值前，必须按冻结的 `hgatp.mode` 使用 29/38 位 GPA/GVPN 约束，不能套用 S1 Sv39/Sv48 的
27/36 位规则。request 本身若带有超出当前 S2 mode 的高位，必须保留原始高位并沿用 DUT 的 GPF 路径，
不得静默截断后伪装成范围内 GVPN；该请求不作为正常的 in-range S2 地址消费。

`allStage` 的 normal leaf 必须分开处理 protocol tag 与模型 derived 两类结果：先按 S1 mode 解释构造期
request GVA VPN，并由 S1 raw PPN、level、PTE.N 和 sector 字段调用 `derive_allstage_raw_s2_tag()`；该 helper
按 S2 `hgatp.mode` 检查 raw protocol GVPN 的高位，Sv39x4 只允许低 29 位，Sv48x4 只允许低 38 位，并将结果
固定写入 `s2_tag`。随后才判断 S1/S2 NAPOT raw PPN 是否 model-resolvable：可解释时从
`s1_resolved_ppn` 得到 request-specific S2 输入 GVPN 并调用 `resolve_s2_ppn()`；不可解释时只保持
`request_derived_valid=0`，不得撤销、重算或覆盖已写入的 `s2_tag`。因此 canonical normal leaf 中 raw tag
与 `s1_resolved_ppn` 一致，而 non-LEGAL 非 canonical NAPOT 中仍有合法 response tag、但没有可消费的
model PPN。S1 mode 与 S2 mode 不同的组合也必须按各自规则独立检查，不能用 S1 的 27/36 位替代 S2 的
29/38 位。上述 normal resolver 规则不适用于 fault passthrough；fault 不从 PPN 派生正常 S2 lookup 输入、
最终 GPA/HPA 或其范围判断，只驱动既有协议所需的 request-matching tag/context，且不得把它解释成已建立的
正常物理地址映射。

`PtwReq` 只携带 VPN/GVPN，不携带 page offset。因此 response entry、pending snapshot 与 UID record 不持久保存
`s1_paddr/s2_paddr`，也不从 L2TLB request 推导 byte PAddr。若 debug 需要页基地址，只能在 dump helper 临时计算
`{resolved_ppn, 12'b0}`；它不进入 response wire、UID copy 或 DCache state。fault passthrough 同样不构造或消费
正常物理地址。V2 `PageTableWalker.scala` 在 `onlyStage2` 与 `allStage` 分支均按
`hgatp.mode` 使用 `GPAddrBitsSv39x4=41`（去掉 12 位 page offset 后为 29 位）或
`GPAddrBitsSv48x4=50`（去掉 12 位 page offset 后为 38 位）检查 GPA 高位，本 plan 必须保持这一来源关系。

### DCache 地址范围与实际请求边界

L2TLB response sequence 只负责生成和冻结 S1/S2 payload，不根据 response 预先建立 DCache PPN owner、
memory line 或其它 DCache 注册状态。DCache memory model 只在 DUT 实际产生并握手 `DCache A.fire/C.fire`
后，按现有 responder 流程检查地址、返回数据或懒分配 backing line；fake/非法 PTE 本身不会直接触发
memory model 分配。

`LEVEL_WEIGHT_EN=0` 时，保持既有严格 memory-range 行为：如果 DUT 后续真的发出 DCache 请求，现有
`MEMBLOCK_MAIN_MEM_RANGES_EN=1` 的 responder 仍检查 `MEMBLOCK_PADDR_BASE/RANGE`。PPN 构造可继续使用
该窗口作为实际请求可服务的生成约束，但这不是 L2TLB response 阶段的 owner 注册。

`LEVEL_WEIGHT_EN=1` 时已强制 `MEMBLOCK_MAIN_MEM_RANGES_EN=0`，实际 DCache/Uncache responder 对落在 DUT
物理地址位宽内的地址采用 sparse memory 服务，不再因 `MEMBLOCK_PADDR_BASE/RANGE` 窗口拒绝请求。此路径
仍必须检查最终地址不越过 DUT 物理地址位宽；effective fault（包括 `s2_gaf`）禁止正常翻译消费，
不触发对 S1/S2 raw PPN、resolved PPN、permission、PTE.N 或 PBMT 的 fault-specific 清零/覆写。

无 fault LEGAL normal leaf 的 PPN 构造 helper 必须按已经选定的合法 level 直接生成符合 superpage/NAPOT、S1 GVA
VPN、S2 GPA/GVPN、interface 位宽和物理地址位宽的 raw PPN，再固定派生 resolved PPN。MIXED/EXCEPTION_BIASED
normal leaf 保留其 raw PPN；仅在 NAPOT 编码可解释时生成 model resolved PPN，否则 derived 字段保持无效。存在
effective fault 时，两套 stage 只执行接口位宽/可表示性检查，
不通过重随机或 fault-specific fixup 改写 payload；不得建立任何“随机失败后重选 raw PPN/level”的 retry loop。
`uvm_fatal` 只用于结构性错误：`LEVEL_WEIGHT_EN` 与 `MAIN_MEM_RANGES_EN` 组合非法、mode 过滤后 level
权重全为 0、参数值非法，normal leaf 的目标接口/物理地址位宽无法编码结果，或出现未声明的有效 S1
non-leaf PTE；报错必须输出 `s2xlate`、stage、mode、level、输入 VPN/GVPN 与 `MEMBLOCK_PADDR_BASE/RANGE`。

### S1 sector PPN 派生字段

S1 response 是 sector payload，不只传输 `s1_entry_ppn_raw`。以下字段必须按 V2 的实际 interface 类型保存，
不能沿用旧的共享 `ppn_low/pteidx` 表示：

| 字段 | 数据类型与接口语义 | 生成规则 |
|---|---|---|
| `s1_addr_low` | 3-bit 数值 sector 位置。 | 由当前 S1 request VPN 的低 3 位确定。 |
| `s1_entry_ppn_raw` | canonical S1 PPN 的高位 `[高位:3]`。 | 从同一 canonical S1 PPN source split 得到。 |
| `s1_ppn_low[8]` | 8 个独立 3-bit PPN 低位字段。 | 每个有效 sector 从其 canonical sector PPN 的 `[2:0]` 得到；不得用 `s1_entry_ppn_raw[2:0] + idx` 伪造。 |
| `s1_valididx[8]` | 8 个 Bool 的 sector 有效 mask。 | 由本次 S1 page/sector payload 的实际有效范围派生；它不是 PTE 选择编码。 |
| `s1_pteidx[8]` | 8 个 Bool 的 one-hot PTE 选择编码。 | 必须满足 `s1_pteidx[i] == (i == s1_addr_low)`；恰好一个 bit 为 1。 |

抽象功能描述：`build_s1_sector_payload()` 在 entry 建构期把同一份 S1 canonical PPN、当前 request 的
`s1_addr_low` 和 page shape 转为可直接驱动的 sector fields。它拥有 S1 PPN split、`valididx` 与 one-hot
`pteidx` 的编码一致性；它不重新随机 PPN、不决定 fault，只读取已选定的 PTE mode/fault 结果决定是否验证
LEGAL NAPOT 编码，也不在 driver 时重新计算 sector 状态。
其中 `s1_canonical_ppn` 仅是该 helper 的建构期局部输入，不作为第三套 entry/snapshot/UID 持久字段；持久
权威 payload 仍只有 split 后的 `s1_entry_ppn_raw + s1_ppn_low[]`。

```text
build_s1_sector_payload(s1_canonical_ppn, s1_addr_low, s1_level, s1_pte_n,
                        s1_pte_mode, fault_stage_selected, sector_valid_shape):
  s1_sector_idx = s1_addr_low。
  将 s1_canonical_ppn 的 `[高位:3]` 写入 s1_entry_ppn_raw。
  按同一 sector_valid_shape 为每个 sector 生成 s1_ppn_low[i] 与 s1_valididx[i]；
  任何有效 sector 的 ppn_low 均来自其 canonical sector PPN 的 `[2:0]`，不从数值 index 推导。
  清零全部 s1_pteidx[i]，仅置 s1_pteidx[s1_sector_idx] = 1。
  若 !$onehot(s1_pteidx)：uvm_fatal；不得把数值 index 写进 s1_pteidx[]，也不得容许全零或 multi-hot。
  若 normal leaf && fault_stage_selected == NONE && s1_pte_mode == LEGAL && s1_pte_n == 1：
      校验 s1_level == 0；
      以 {s1_entry_ppn_raw[0], s1_ppn_low[s1_sector_idx]} 重组 canonical PPN[3:0]；
      若 LEGAL normal leaf 的结果不是 4'b1000：uvm_fatal；正确编码必须已由
      apply_legal_napot_ppn_encoding() 在 split 前直接写入；
      不得检查 s1_entry_ppn_raw[3:0]，也不得只修改 ppn_low 或只修改 entry_ppn 的一侧。
  fault passthrough 不执行 normal NAPOT/level 合法化，但仍按本函数完成 PPN split 与 one-hot pteidx 编码。
```

#### `validate_s1_sector_payload_consistency()`

抽象功能描述：`validate_s1_sector_payload_consistency()` 在 S1 sector payload 已生成、复制或即将驱动时，
确认 split PPN、目标 sector、valid mask 与 one-hot PTE 选择仍属于同一份冻结 payload。它只验证 payload
完整性，不计算 fence 地址命中、不随机、不修改 entry、snapshot、UID 或 driver 状态。

```text
validate_s1_sector_payload_consistency(entry, phase, optional source_entry,
                                       optional build_canonical_ppn):
  若 !entry.s1_stage_active：返回。

  要求 s1_pteidx[] 为 one-hot，且 OHToUInt(s1_pteidx) == s1_addr_low；
  要求 s1_valididx[s1_addr_low] == 1。

  若 phase == BUILD：
    要求 {s1_entry_ppn_raw, s1_ppn_low[s1_addr_low]} 与 build_canonical_ppn 的
    对应 split 编码一致。

  若 phase == COPY：
    要求 destination 的 s1_addr_low、s1_entry_ppn_raw、全部 s1_ppn_low[]、
    全部 s1_valididx[] 与全部 s1_pteidx[] 逐字段等于 source_entry。

  若 phase == DRIVE：
    不重新计算 PPN/sector；后续 driver 只能逐 wire 驱动已验证 snapshot 的字段。

  任一要求失败：uvm_fatal("L2TLB_S1_SECTOR_PAYLOAD", phase 和完整字段 dump)。
```

本 helper 不传入 fence VPN，也不得被 `sfence_s1_addr_match()` 调用。fence 命中只读取 tag、level、PTE.N、
`valididx`、ASID 与 VMID；payload 一致性错误必须在本 helper 的调用边界失败，不能伪装成 fence 不命中。

`resolve_s1_ppn()` 只在 normal leaf 路径使用上述冻结字段。它先校验 `s1_pteidx` one-hot，确认
`OHToUInt(s1_pteidx) == s1_addr_low`，再以选中 sector 的 `s1_ppn_low[]` 与 `s1_entry_ppn_raw` 重组 PPN。
普通 page、superpage 与 NAPOT 分支均从这个联合值出发；NAPOT 分支按 DUT 的 tag/VPN 补齐规则生成最终 PPN，
不能把拆分后的 `entry_ppn` 单独当作完整 PPN。`noS2xlate` 的 `PtwSectorResp.hit()` 以 `s1_valididx[]`
决定 base-page sector 是否可命中；`onlyStage1/allStage` 的 `PtwRespS2.hit()` 则以
`Cat(s1_tag, s1_addr_low)` 作为 response anchor，不得用 `valididx[]` 或 `pteidx[]` 改写该 raw-hit 语义。
两种路径都不得用数值 PTE index 投影生成接口字段。

live entry、pending snapshot 与 UID record 必须逐项保存 `s1_ppn_low[8]`、`s1_valididx[8]` 和
`s1_pteidx[8]`。`copy_from()`/`copy_entry_fields()` 只作同类型逐 bit 复制，不进行 `index -> one-hot`、
`one-hot -> index` 或 `!= 0` 的隐式转换；每次 copy 后必须调用
`validate_s1_sector_payload_consistency(destination, COPY, source)`，若 active S1 的 split PPN、valid mask、
one-hot 或 selected-index 对应关系被破坏，立即 `uvm_fatal`。

payload 建立顺序固定为：配置组合检查 → `s2xlate` 有效 stage 识别与创建时 provenance 冻结 → raw fault 和
raw PTE/level/PPN/permission/PBMT 随机 → S1/S2 `apply_pte_profile()` → `fault_effective_*` 唯一选择 → level mode 过滤 →
检查无 fault 有效 S1 payload 不为 non-leaf → 按 `fault_stage_selected` 分支执行 normal leaf 的 PTE.N/PPN/LEGAL
合法化 → 对无 fault LEGAL stage 调用 `apply_legal_napot_ppn_encoding()` 直接写 NAPOT PPN low-4 →
`build_s1_sector_payload()`/S1/S2 raw payload split，或保留 fault passthrough 的 raw payload 并完成其 S1
sector split/one-hot 编码 → normal leaf 的 allStage 先调用 `derive_allstage_raw_s2_tag()` 生成并校验 protocol
`s2_tag`，再按 NAPOT raw 可解释性决定是否生成 request-specific resolved PPN/GVPN；fault 只生成协议必需
tag/context，不进入 normal resolver → normal leaf 地址位宽/既有窗口规则检查 → 同时冻结 raw/effective fault
与全部 S1/S2 payload 到 live entry → snapshot。L2TLB sequence 不创建 DCache owner；fault 或 derived invalid
均不改变后续 DCache lifecycle。

## 四、独立 PTE Permission 随机

### PTE Permission 构造职责

S1 独立随机 `R/W/X/U/G/A/D/V` 八个字段；S2 独立随机 `R/W/X/U/G/A/D` 七个实际可驱动 permission
字段，每一位都使用对应的“为 1”权重参数。生成器不得把 S1 permission 镜像到 S2，或把任意 S2
permission 常量化。S1 的 `V` 只驱动 `s1_entry_v`；S2 不存在 `V` 参数、模型字段或 interface 驱动。
原始随机阶段允许产生 `W=1,R=0` 等非规范组合；无 effective fault 时，最终是否合法化由本 stage 的 mode 决定；
存在 effective fault 时，两套 stage 统一保留 profile 后的伴随 payload。`MEMBLOCK_L2TLB_S1_PTE_MODE` 与
`MEMBLOCK_L2TLB_S2_PTE_MODE` 分别控制本 stage 的 PTE profile：`0=LEGAL`、`1=MIXED`、
`2=EXCEPTION_BIASED`。三种 mode 都必须先调用本 stage 的 `apply_pte_profile(stage_fields, mode, UNKNOWN)`；
该调用负责落实本 mode 的随机/profile 语义，但不把 MIXED/EXCEPTION_BIASED 自动合法化。只有某一 stage
为 `LEGAL` 且 `fault_stage_selected == NONE` 时，才继续调用该 stage 的
`fixup_pte_legal(stage_fields, UNKNOWN)`，由该 helper 修正 `W=1,R=0`、无 `R/W/X` 的有效 PTE，并将
该 stage 的 A/D 强制为 `1/1`；S1 同时强制 `V=1`。存在 effective fault 时 S1/S2 即使 mode 为 `LEGAL` 也跳过该正常翻译合法化，
以保留两套异常伴随 payload。MIXED/EXCEPTION_BIASED 不调用通用 `derive_ad_bits()` 合法化，A/D 在 UNKNOWN 下
保留各自独立权重随机结果，即使为 0 也允许作为异常/非规范激励。两套 stage 始终保留自己的 mode
和随机/profile 结果。profile 与合法化 helper 必须显式接受
stage 字段组：S1 字段组包含 `V`，S2 字段组不包含 `V`，不得读取、写入或推导不存在的 `s2_pte_v`。

当前 L2TLB request 只有 `vpn/s2xlate`，不携带 load/store access kind。因此本阶段 S1、S2 均固定以
`MEMBLOCK_TLB_ACCESS_UNKNOWN` 调用各自的 PTE profile helper；只有 mode 为 `LEGAL` 且
`fault_stage_selected == NONE` 时才调用合法化 helper，不新增 access-kind sideband。`UNKNOWN` 只表示当前没有可靠的 load/store 类型，不改变
MIXED/EXCEPTION_BIASED 保留 A/D 独立权重随机结果的规则。后续若接口能够提供可靠的 request-time access
kind，再单独扩展该 mode 的 load/store 定向行为。

#### PTE profile truth table

`UNKNOWN` 下 A、D 的原始结果固定为各自按对应 `_A_1_WT`、`_D_1_WT` 独立随机；不得因为 UNKNOWN
推断 load/store，也不得让 `EXCEPTION_BIASED` 以未定义的 access-kind 覆盖其中一位。profile 对 R/W/X/U/G 的
既有 mode 行为保持不变；下表只规定 V/A/D 的最终结果与 fixup 次数。

| effective fault | stage PTE mode | stage | `apply_pte_profile(..., UNKNOWN)` | V/A/D 最终结果 | `fixup_pte_legal()` |
|---|---|---|---|---|---|
| 存在 | 任意，含 `LEGAL` | S1 | 恰好调用一次 | V、A、D 均保留 profile 后的原始结果；A/D 原始值独立按权重随机 | 不调用 |
| 存在 | 任意，含 `LEGAL` | S2 | 恰好调用一次 | A、D 均保留 profile 后的原始结果；A/D 原始值独立按权重随机；无 V 字段 | 不调用 |
| 无 | `LEGAL` | S1 | 恰好调用一次 | 最终强制 `V=1,A=1,D=1` | 恰好调用一次 |
| 无 | `LEGAL` | S2 | 恰好调用一次 | 最终强制 `A=1,D=1`；无 V 字段 | 恰好调用一次 |
| 无 | `MIXED` | S1/S2 | 恰好调用一次 | A、D 保持 UNKNOWN 下独立权重随机的结果；S1 V 也保留自身权重结果 | 不调用 |
| 无 | `EXCEPTION_BIASED` | S1/S2 | 恰好调用一次 | A、D 保持 UNKNOWN 下独立权重随机的结果；S1 V 也保留自身权重结果 | 不调用 |

这里的“无 fault + LEGAL S1 必定 V=1”仅适用于已通过 paged-mode 输入检查的 active S1 stage。inactive stage
没有 S1 payload，不调用 profile/fixup；active Bare stage 不进入本 framework，不能进入 profile 或依靠
`mode==0` 作为 inactive fallback。

PTE profile 的调用顺序固定为：

```text
随机本 stage 的原始 permission/PTE 字段
  -> apply_pte_profile(stage_fields, mode, UNKNOWN)
  -> fault_stage_selected == NONE 且 mode == LEGAL
       ? fixup_pte_legal(stage_fields, UNKNOWN)
       : 保留 profile/异常伴随结果
  -> 冻结到 S1/S2 live entry 和 response snapshot
```

`fixup_pte_legal()` 只能在 `fault_stage_selected == NONE` 且 stage 为 `LEGAL` 时调用，不得把 S1 字段复制到 S2；
若复用现有 helper，必须拆出或增加 mode guard，保证 `derive_ad_bits()` 的 A/D=1/1 规则只作用于正常
`LEGAL` stage，并在 S1 LEGAL 路径将 `s1_pte_v` 直接置为 1。该 helper 是确定性 fixup，禁止再次调用
`apply_pte_profile()`、重新 randomize 或读取新的权重；否则会覆盖已经冻结的 UNKNOWN 原始随机结果。
`EXCEPTION_BIASED` 在 UNKNOWN 下不承诺 load/store 定向 A/D bias，A/D 仅保留独立权重随机结果，不能被后续
通用合法化覆盖。

抽象功能描述：`fixup_pte_legal()` 只在一次 profile 已完成后的无 fault LEGAL stage 中，收敛该 stage 的
最终 PTE 为本 plan 定义的合法 leaf 形状。它不负责 profile 选择、随机、fault 选择、level/PPN/NAPOT 构造或
S1/S2 字段复制。

输入：本 stage 已完成 `apply_pte_profile(..., UNKNOWN)` 的字段组、stage 标识、PTE mode、
`fault_stage_selected`。

输出/副作用：仅确定性改写本 stage 的 `R/W/X/A/D`，并在 S1 改写 `V`；不读取或修改另一 stage。

```text
fixup_pte_legal(stage_fields, stage, pte_mode, fault_stage_selected):
  若 fault_stage_selected != NONE 或 pte_mode != LEGAL：uvm_fatal；调用者违反 profile/fault 分支合同。

  // 以下赋值只消费已经 profile 后的字段，不触发新的随机分布。
  若 stage_fields.W == 1 && stage_fields.R == 0：stage_fields.R = 1。
  若 stage_fields.R == 0 && stage_fields.W == 0 && stage_fields.X == 0：stage_fields.R = 1。
  stage_fields.A = 1。
  stage_fields.D = 1。
  若 stage == S1：stage_fields.V = 1。
  若 stage == S2：不得读取、写入或检查 V。

  不调用 apply_pte_profile()、randomize()、任何权重 getter 或 PPN/level helper。
  返回；调用者随后执行 LEGAL NAPOT PPN 编码、sector split 和 payload freeze。
```

中文文字伪代码：profile 是“生成一次模式化原始字段”，fixup 是“在同一份字段上做一次确定性收敛”。
因此一个无 fault LEGAL stage 的顺序固定为“原始字段随机 -> `apply_pte_profile()` 一次 ->
`fixup_pte_legal()` 一次”，不能从 fixup 回跳到 profile。S1 的 `V=1`、两阶段的 `A=D=1` 只在这条 LEGAL
无 fault 路径成立；effective fault、MIXED、EXCEPTION_BIASED 均不会进入本 helper。

#### `reject_unsupported_valid_s1_nonleaf()`

抽象功能描述：`reject_unsupported_valid_s1_nonleaf()` 在 entry 的 raw/effective fault 与 S1 permission profile
已经冻结后，拒绝本 plan 未建模的“无 fault 有效 non-leaf”payload；它不修改 PTE、PPN、token、UID 或 driver，
只防止后续 normal PPN helper 把页表指针误作数据页映射。

```text
if fault_stage_selected == NONE
   && s1_stage_active
   && s1_pte_v == 1
   && !(s1_pte_r || s1_pte_w || s1_pte_x):
  uvm_fatal("L2TLB_UNSUPPORTED_VALID_S1_NONLEAF", key/profile/raw_ppn/level)

otherwise:
  return without changing payload
```

该 helper 不拒绝已经携带 `fault_effective_*` 的伴随 raw permission/PPN，也不新增 `genGVPN()`、异常 GPA 或
下一级 page-walk 模型。后续若需覆盖 valid non-leaf 的 G-stage page-table-access fault 与 `gpaddr/tval2`，必须
新建专项 plan；不能删除该 fatal 后把它静默当作 normal leaf。

随机 permission、raw/effective fault、level、PTE.N 修正或 fault passthrough 后的 response 一起写入 response
snapshot；`fill_dtlb_resp_from_entry()` 负责把四个 effective fault 写入对应 fault interface，并保留
`s1_af = fault_effective_s1_af || entry.pmaAF` 的 legacy PMA tail；S1/S2 payload 仍分别来自各自冻结字段，
driver 只驱动已经冻结的结果。

## 五、独立 PBMT 随机与阶段修正

### PBMT 构造职责

`s1_entry_pbmt` 与 `s2_entry_pbmt` 分别使用三组 PBMT 权重随机赋值，共六组参数。不新增或保存
`effective_pbmt`；后续确有需要读取最终阶段 PBMT 的 consumer，必须按当前 response 的 `s2xlate` 直接选择
相应 raw 字段：

| `s2xlate` | 后续 consumer 按需读取的 raw PBMT 字段 |
|---|---|
| `noS2xlate` / `onlyStage1` | 使用 S1 PBMT |
| `onlyStage2` | 使用 S2 PBMT |
| `allStage` | S1 PBMT 非零时使用 S1；S1 为零时使用 S2 |

S1/S2 PBMT 始终使用各自字段和各自三组 plus 权重随机，不能为了选择阶段来源而覆写、复制或合并
raw `s1_entry_pbmt/s2_entry_pbmt`。interface 继续驱动两个独立 raw PBMT 字段；本 plan 的 debug 同时输出
两套 raw 值与 `s2xlate`，但不折叠为第三个 effective PBMT 字段。

### PBMT 的单次 `dist` 选择与失败策略

抽象功能描述：PBMT 选择 flow 在 lookup miss 创建 entry 时，分别以 S1 和 S2 各自三项 getter 权重产生一个
raw PBMT 编码，并写入各自的 payload 字段；它不依据 `s2xlate` 复制、合并或重选两个 stage 的结果，后续
consumer 只按既定阶段语义读取它们。

1. 对 S1 和 S2 分别执行一次等价于 `chosen_pbmt dist { 0 := PBMT_0_WT, 1 := PBMT_1_WT, 2 := PBMT_2_WT }`
   的标准 SystemVerilog `dist` 随机。每个 stage 每次只写入一个 `0/1/2` 编码；两个 stage 的两次选择彼此
   独立。
2. 配置阶段已由 `check_l2tlb_payload_weight_cfg()` 保证每一 stage 的三项权重至少一项非 0。因此正常选择
   路径不允许因“没有候选”而静默固定为 PBMT 0，也不需要通过重新随机寻找可用编码。
3. PBMT 的三项是枚举候选而非独立 flag：例如 `PBMT_1_WT` 与 `PBMT_2_WT` 同时非 0 只表示二者均有机会被
   单次选择，不表示一次 response 可同时得到两个 PBMT 值，也不需要优先级比较。
4. getter 值越界、三项全 0 或 `randomize()` 无法满足该标准分布时立即 `uvm_fatal` 并打印 stage、三项
   权重与 `s2xlate`；不得实现 retry、fallback 或由另一个 stage 的 PBMT 覆盖当前结果。

## 六、数据模型、响应建立与可观测性

### 数据模型与响应职责

旧 `memblock_tlb_entry.pte_*` 不保留、不兼容：统一重命名为 S1 的 `s1_pte_*`，并新增只包含实际
可驱动字段的 S2 `s2_pte_r/s2_pte_w/s2_pte_x/s2_pte_u/s2_pte_g/s2_pte_a/s2_pte_d` 与 `s2_pte_n`；
不得新增 `s2_pte_v`。所有旧 consumer 同步改读 S1 `s1_pte_*`；不得留下含义模糊的共享 `pte_*` 字段。旧共享
`paddr/ppn/level/pte_n/pbmt` 及 S1 sector 相关 `addr_low/ppn_low/valididx/pteidx` 也不保留。

PPN 迁移后不得再使用含义不清的 `s1_ppn/s2_ppn` 单一字段，而是保存：

```text
s1_entry_ppn_raw：写入 S1 response interface 的 canonical PPN 高位 `[高位:3]`，必须与 `s1_ppn_low[]` 联合解释。
s2_entry_ppn_raw：写入 S2 response interface 的完整非-sector PPN payload。
s1_resolved_ppn / s2_resolved_ppn：仅 normal leaf 中由 raw payload、level、VPN/GVPN、NAPOT 和 S1 sector 派生的完整 PPN；
  live entry 若保留仅作首次 anchor debug，request-specific 值属于 pending。
```

S1 sector 的 `s1_addr_low/s1_ppn_low[]/s1_valididx[]/s1_pteidx[]` 同样属于 S1 raw payload；S2 保留独立的
`s2_level/s2_pte_n/s2_pbmt` 等 response 字段。live entry、pending response snapshot 和 UID record 必须保存
`fault_raw_*` 与 `fault_effective_*` 两套 fault，以及两套完整 S1/S2 raw PPN、level、PTE.N、permission 与
PBMT。live entry 的 resolved PPN 若保留，只能表示首次建立 anchor 的 debug 值；range hit 的本次 request
resolved PPN 只保存于 pending 的 request-specific derived 字段，UID record 如需保存该派生值也只能从该 pending
字段复制。任何位置都不得持久保存或复制 byte PAddr。`pmaAF` 作为四类 fault 之外的 legacy sideband 同样逐字段保存；它不改写
`fault_raw_*`、`fault_effective_*` 或 `fault_stage_selected`。resolved 字段只可由 helper 派生，不能在 snapshot、
UID copy 或 driver 中重新随机。

其中 S1 sector 字段类型固定为：`s1_addr_low` 为 `bit [2:0]`，`s1_ppn_low[8]` 为八个 `bit [2:0]`，
`s1_valididx[8]` 为八个 `bit`，`s1_pteidx[8]` 为八个 `bit` 的 one-hot 向量。不得保留旧的
`bit [2:0] pteidx[8]` 数值数组，也不得用 `s1_pteidx[i] != 0` 投影为 response Bool。若实现需要数值
sector index，只能读取或保存独立的 `s1_addr_low`，不能污染 interface payload 的 one-hot 字段。

S1/S2 tag 与上下文字段必须作为明确的 payload 数据字段保存：

```text
s1_tag / s1_asid / s1_vmid
s2_tag / s2_vmid

fault_raw_s1_pf / fault_raw_s1_af / fault_raw_s2_gpf / fault_raw_s2_gaf
fault_effective_s1_pf / fault_effective_s1_af / fault_effective_s2_gpf / fault_effective_s2_gaf
fault_stage_selected
pmaAF  // legacy PMA sideband；不属于四个 raw/effective fault

s1_stage_active / s2_stage_active
s1_translation_mode_at_build / s2_translation_mode_at_build
s1_pte_mode_at_build / s2_pte_mode_at_build
s1_root_ppn_at_build / s2_root_ppn_at_build
csr_context_seq_at_build / entry_generation
```

每个 `memblock_l2tlb_pending_req` 除既有 `request_token/lookup_key/entry_snapshot` 外，必须保存：

```text
pending_entry_generation
```

其中 `pending_entry_generation` 必须等于 `entry_snapshot.entry_generation`；它是 response token 的冻结
payload 时代，不能在完成、flush 或 driver 重驱时重新从 live table 读取。是否有 UID record 命中由 response
complete 时的 raw matcher 决定；prefetch、duplicate response 或本轮没有对应 issued UID 时，命中数为 0 是正常行为。

字段类型与来源固定为：`fault_stage_selected` 为 `NONE/S1/S2` 三值 enum，`uid_tlb_wait_state` 为
`WAITING/COMPLETED/CANCELED` 三值 enum，`request_token/pending_entry_generation` 为
`longint unsigned`，`uid_tlb_wait_epoch` 为 `int unsigned`，`uid_wait_start_sample_seq` 与
`uid_tlb_first_request_fire_sample_seq` 为 `longint unsigned`，`*_stage_active` 为 `bit`，
`pmaAF` 为 `bit`，
`*_translation_mode_at_build` 与对应 CSR mode 同宽
（当前为 4 bit），`*_root_ppn_at_build` 与对应 CSR root PPN 同宽（当前为 44 bit），
`*_pte_mode_at_build` 为容纳 `LEGAL/MIXED/EXCEPTION_BIASED` 的 2 bit enum，
`csr_context_seq_at_build` 为 `int unsigned`，`entry_generation` 为 `longint unsigned`。这些字段不能用
`priv_mode`、共享 `level` 或可变的 `mmu_csr_state` handle 代替。
它们是测试框架内部 entry/snapshot/UID 数据字段，不新增 DUT response interface port，也不新增 plus 参数。

其中 `s1_stage_active/s2_stage_active` 只由 `s2xlate` 决定；`*_translation_mode_at_build` 与
`*_root_ppn_at_build` 只从该 request fire 冻结的 CSR snapshot 选取。inactive stage 的 mode/root 固定清零；
若 active stage 的 mode 为 0（Bare），在 entry 插入前 `uvm_fatal`，不保留可供后续 consumer 使用的 Bare entry。
`csr_context_seq_at_build` 固定为
`csr_snapshot.update_seq`，用于 provenance 与 debug；它不是 key 的组成部分，也不作为自动 invalidation 条件。

`entry_generation` 由 `common_data_transaction` 持有、在本次仿真中单调递增的计数器分配；初始化后，普通
reset/flush 清空 table 时不得回退或复用旧 generation。仅在 lookup miss 的
`build_tlb_entry_for_key_with_csr()` 成功生成并即将插入 `tlb_entry_by_key` 时递增一次并写入 entry；lookup hit
只更新既有 `last_hit_cycle`，绝不改写 generation、mode、root 或 CSR context sequence。fence/flush 删除 entry 后，
后续相同 key 的 request 再次 miss 并重建时获得新的 generation；因此它不能由 `csr_context_seq_at_build` 替代，
因为同一 CSR version 下也可能发生 fence 后重建。

`memblock_tlb_entry::copy_from()` 必须逐字段复制上述 provenance（包括 `*_pte_mode_at_build`）、S1 sector split PPN、one-hot mask 与 `pmaAF`
到 pending snapshot；
`memblock_uid_tlb_record::copy_entry_fields()` 只在 `complete_waiting_uid_records_by_response()` 的 raw hit
校验通过后逐字段复制 payload 到对应 UID record。response driver 只能使用 `pending.entry_snapshot`，发包时不得重新读取 current CSR 或重新计算
stage mode/root/generation。pending record 已有的 `csr_snapshot` 仍表示该 request fire 的 CSR context；
`entry_snapshot` 中的 `*_at_build` 与 generation 表示被命中的 live entry 创建 context，二者必须同时保存。
`copy_entry_fields()` 不得覆盖 UID record 的 `uid_tlb_wait_epoch/uid_tlb_wait_state/uid_wait_start_sample_seq/
uid_tlb_first_request_fire_sample_seq`，这些字段只由 issue、request-fire marker、complete 或 cancel flow 更新。

迁移校验要求：entry 构造时若 stage-to-CSR source mapping 无法确定，或 lookup hit 发现 `entry_generation==0`、
active 标志与 `s2xlate` 不一致，或 snapshot/UID copy 后任一 provenance 字段不相等，必须在 response drive 前
`uvm_fatal`；不得用 `priv_mode`、current CSR 或默认零值补齐，也不得静默重新随机。lookup hit 不得把 entry 的
冻结 mode/root 与当前 request CSR 比较后 fatal，因为 CSR 变化在对应 SFENCE/flush 消费前仍由既有 lifecycle 处理。

S1 tag 从按 S1 mode 解释的 request GVA VPN/S1 sector 语义派生；onlyStage2 的 S2 tag 从 request GVPN 派生。
allStage 的 normal leaf S2 tag 必须由 `derive_allstage_raw_s2_tag()` 生成：该 helper 使用构造期 request VPN、
S1 raw PPN、level、PTE.N 和 sector split，按 DUT `genPPN()` 的原始位拼接规则得到 raw protocol GVPN anchor，并按冻结
`hgatp.mode` 的 29/38 位宽做可表示性检查。它不调用 `napot_raw_ppn_is_model_resolvable()`，不执行 LEGAL
合法化，也不把 raw anchor 当作 resolved PPN。因而 normal LEGAL/canonical 路径中 raw anchor 与
`s1_resolved_ppn` 相同；MIXED/EXCEPTION_BIASED 的非 canonical NAPOT 路径仍能生成并保存 `s2_tag`，但
`request_derived_valid=0`，不生成或消费 request-specific resolved PPN。S1 ASID/VMID 与 S2 VMID 分别使用
request-time 对应 CSR context。effective fault 不进入 normal resolver；它只生成协议所需、位宽可表示的
request-matching tag/context，raw PPN、permission、PBMT 和 fault payload 仍按 passthrough 保存。

`s1_tag/s2_tag` 是 live entry 的固定 response anchor。range hit、pending copy、UID copy 和 driver 重驱均只复制
该字段，不能因本次 request 的 VPN/GVPN 或 `request_derived_valid` 改写。`s2_tag` 即使由 raw protocol anchor
得到，也只服务 DUT response、S2 context/HFENCE 与 debug；它不授予模型建立 DCache owner 或最终物理地址的资格。

#### `derive_allstage_raw_s2_tag()`

抽象功能描述：该 helper 在 allStage normal leaf 的 entry build 阶段把 S1 raw payload 转换为 DUT 可接收的 S2
tag anchor。它只生成并校验 protocol 字段，不判断该 payload 是否适合模型地址消费，也不修改任何 raw PTE 字段。

输入：构造期 request GVA VPN、已完成 split 的 S1 raw payload、冻结的 S2 `hgatp.mode`。

输出/副作用：返回 raw protocol GVPN anchor；调用者将其写入新 entry 的 `s2_tag`。超出 DUT S2 GVPN 位宽或
interface 可表示范围时 `uvm_fatal`；不执行重随机、不截断高位、不改变 `request_derived_valid`。

```text
derive_allstage_raw_s2_tag(build_vpn, s1_fields, s2_mode):
  raw_gvpn = DUT_genPPN_from_raw_s1(s1_fields, build_vpn)
             // 按 level/NAPOT/sector 的原始拼接规则组合，不检查 low-4 是否 canonical。
  gvpn_width = (s2_mode == Sv39x4) ? 29 :
               (s2_mode == Sv48x4) ? 38 : fatal(unsupported mode)
  若 raw_gvpn 的高位超出 gvpn_width：uvm_fatal；不得静默截断。
  将 raw_gvpn 的有效位放入 s2_tag，其余 interface 高位按 DUT 约定补 0。
  返回 raw_gvpn。
```

中文文字伪代码：helper 先用与 DUT `PtwSectorResp.genPPN()` 相同的原始位拼接得到 GPA/GVPN。即使 S1
`PTE.N=1` 且 MIXED/EXCEPTION_BIASED 的 PPN low-4 不是 `4'b1000`，该拼接仍可产生一个协议 tag；此时它只是
response anchor，不是经过合法性证明的 normal PPN。helper 再按冻结的 S2 mode 检查高位，避免把不支持的 GPA
静默截断到低位。只有 helper 成功返回后才写 `entry.s2_tag`；后续 `populate_pending_request_derived()` 仍可
因 NAPOT 编码不可解释而把 request-specific derived 标为 invalid。range hit 不再次调用该 helper。

抽象功能描述：`common_data_transaction::build_tlb_entry_for_key_with_csr()` 在 DTLB-side CSR snapshot 已冻结且
lookup miss 时，作为唯一公开入口调用 builder core，随后为已完成构造的 entry 分配 generation；它不决定 pending
cancel，不删除旧 entry，也不读取完成时 CSR。

```text
common_data_transaction::build_tlb_entry_for_key_with_csr(key, csr_snapshot):
  创建 tlb_map_builder，并调用 builder.build_payload_for_key_with_csr(key, csr_snapshot)。
  builder core 已完成 stage activity、mode/root、CSR provenance、raw/effective fault、
  S1/S2 permission/PPN/sector 与 BUILD 一致性校验；返回 null 或校验失败则 fatal。
  仅在 builder core 成功返回后分配 next_entry_generation 并写入 entry_generation。
  返回 entry；调用者插入 tlb_entry_by_key。
```

抽象功能描述：`common_data_transaction::allocate_tlb_entry_generation()` 只为已经完成 payload 构造、即将
进入 live table 的新 entry 分配身份编号。它不查询或修改 lookup key，不删除旧 entry，也不参与 pending/driver
调度。

```text
allocate_tlb_entry_generation():
  读取 common_data_transaction.next_tlb_entry_generation。
  若递增会溢出或结果为 0，uvm_fatal，不能回绕复用旧编号。
  递增计数器并返回新值。
  普通 reset/flush 只清理 table 和生命周期状态，不清零该计数器；完整新仿真重新初始化对象时才重新开始。
```

该 helper 的返回值只写入新 live entry，随后由 `copy_from()` 和 `copy_entry_fields()` 复制；hit、pending 重驱、
UID 二次回填和 fence matcher 都不能再次调用它。相同 lookup key 的后续 hit 不得重新调用任何 fault、level、
PPN、permission 或 PBMT 随机 helper。

L2TLB responder 在 lookup miss 创建 live entry 时完成“冻结 provenance -> 配置检查 -> raw 随机 -> effective fault
选择 -> normal leaf 合法化与 PPN 派生，或 fault passthrough raw payload 保留 -> generation 分配”流程。lookup hit 不改写 entry 中任何 raw/effective fault 或 S1/S2 payload；每次 accepted
request 都从命中的 live entry 构造独立 pending snapshot，pending queue 和 driver 只传递该 snapshot，因此同一
key 的每次 response 与第一次 miss 的 payload 完全一致。该流程不建立 DCache owner，也不触发或模拟 DCache
memory allocation。

### Lookup key 与 CSR 翻译上下文失效

同一 live entry 的复用 key 固定为 request-time 的 `vpn + asid + vmid + s2xlate`。该 key 不包含
`satp/vsatp/hgatp` 的 mode 或 root PPN，故不得把“key 相同”单独视为 CSR 翻译上下文仍相同。

凡 runtime CSR 变化改变任一翻译根或阶段配置，包括 `satp/vsatp/hgatp` 的 mode、root PPN、或触发对应
地址空间切换的上下文变化，CSR testcase 必须按既有框架规则构造对应的 SFENCE/flush。既有 L2TLB
lifecycle 负责 entry 失效、pending cancel 和 flush hold；本 plan 不新增 CSR 变化检测、entry 删除、
flush sideband 或第二个 lifecycle owner，只在该 lifecycle 到期时同步取消对应 UID waiting instance。创建时字段
冻结 provenance，供 level/fence/debug/UID payload 追溯使用；它们不得
在 CSR `update_seq` 改变时自动清表、拒绝同 key hit 或覆盖已有 pending snapshot。

ASID、VMID 或 `s2xlate` 改变本身会形成不同 key；mode/root PPN 改变即使 key 位相同，也依赖既有
SFENCE/flush 规则切断复用。本 plan 只把此事实作为随机 entry 复用的 testcase 前提。fence matcher 读取
entry 自身的冻结 mode 做 range 宽度/level 合法性判断，不读取 current CSR；`entry_generation` 用于日志、
debug、fence/request 前后同 key 重建的可追溯性以及 UID payload provenance，但不参与 live entry 的 lookup match 条件。

### 同 key 的 payload 复用边界

抽象功能描述：`lookup_or_build_tlb_entry()` 负责在 live table 中决定“复用已有 payload”还是“创建新 payload”。
它只在 lookup miss 时调用一次构造/随机 flow；hit 时返回原 entry，不重新选择 fault、不重新执行 GAF 分支，
也不重新派生或修正 S1/S2 level、PPN、permission、PTE.N 和 PBMT。

```text
key = request.vpn + request.asid + request.vmid + request.s2xlate

if tlb_entry_by_key contains key:
    entry = existing live entry
    保留 fault_raw_* / fault_effective_*
    保留独立 pmaAF、全部 S1/S2 raw/resolved payload 与 provenance
else:
    entry = build_tlb_entry_for_key_with_csr(key, request.csr_snapshot)
    插入 tlb_entry_by_key

accepted request:
    pending = {request_token, entry.copy_from(), entry_generation}
    validate_s1_sector_payload_consistency(pending.entry_snapshot, COPY, entry, null)
    driver 只发送 pending_snapshot
response sample complete:
    取得本 response sample 的 C-2 response-visible CSR snapshot
    用与 DUT 相同的 raw hit 语义回填全部仍为 WAITING 的 UID record
```

只要 live entry 未被既有 reset、flush、SFENCE/HFENCE 或其它显式失效删除，相同 key 的后续 request 必须
得到与第一次 miss 完全相同的 S1/S2 payload，即使这些 payload 包含 `s2_gaf` 伴随的非规范随机字段。entry
删除后，相同 key 的下一次 request 才是新 miss，允许重新随机并获得新的 `entry_generation`。CSR mode/root
改变但对应失效尚未被既有 lifecycle 消费时，不自动清表、不拒绝 hit，仍复用旧 entry；CSR testcase 必须按既有
规则构造并消费对应 fence/flush。旧 pending snapshot 继续使用旧 entry 的 payload，新 entry 的随机结果只供
失效后的新 request 使用。

### Response token 与 UID 多播回填

#### 功能与绑定边界

L2TLB request DUT interface 不携带 UID，且 V2 `PTWNewFilter` 在一笔 `PtwRespS2` 返回时会对全部
有效 filter entry 执行内容匹配。因此 token 是“已经被 L2TLB 接受的一次 request fire”的响应账本，
不是“唯一 UID 的所有者”。同一 response 可以同时完成多个同 key UID，也可以完成 NAPOT/superpage
覆盖范围内的多个 UID；同一 token 的后续重复 response 在 DUT 已经清掉这些 filter entry 后也可能不再
命中任何 UID，但 token 仍必须正常 complete。

旧 `update_uid_tlb_records_by_entry(key, entry)` 的问题不是“扫描多个 UID”本身，而是它只比较 exact key，
并且不区分 flush 前后等待实例。新的 `complete_waiting_uid_records_by_response()` 仍在 response 完成这一低频
路径扫描 `uid_tlb_record_by_uid`，但必须用 `entry_matches_request_raw()` 的 V2 `PtwRespS2.hit()` 语义和
**本 response fire 时 filter 可见的 C-2 CSR**判断是否回填。UID 自己冻结的 key/CSR context 只保留为 issue
历史、waiting epoch 和 debug 真源，不能代替 response 时当前 `PTWNewFilter` 内 `PTWFilterEntry` 读取的 current `io.csr`。
这样多播只发生在 DUT 也会命中的 UID 上，而不是按“同 key 最早 UID”或旧 CSR 猜测。

#### UID record 与 pending token 字段

`memblock_uid_tlb_record` 新增或迁移以下内部字段：

```text
uid_tlb_wait_epoch             // 每次 UID 新建或重新发起 TLB 等待时单调递增
uid_tlb_wait_state             // WAITING / COMPLETED / CANCELED
uid_wait_start_sample_seq      // 本次等待实例建立的统一 DUT sample 序号
uid_tlb_first_request_fire_sample_seq  // 本等待实例首次观察到对应 L2TLB request fire 的 sample；0=尚未观察
```

`pte_valid` 只表示 payload 已成功复制。`CANCELED` UID 保持 `pte_valid=0`，但已经失去被后续 response
回填的资格；只有该 UID 真正再次 issue 并建立新等待实例时，才递增 `uid_tlb_wait_epoch` 并回到 `WAITING`。
历史 UID record 可以保留 debug，不得因仍为 `pte_valid=0` 自动重新参与新 response 的匹配。

redirect/reissue 也必须经过同一状态边界：旧动态实例被 `clear_uid_dispatch_result()` 清理后，其对应 UID
record 先标为 `CANCELED`（或由后续真实 issue 原子地递增 epoch 并重建为 `WAITING`）；不得仅依赖
`status.active/flushed` 判断回填资格。这样迟到的旧 response 不会把旧动态实例重新置为 `TLB_MAPPED`，而真实 reissue
仍可通过新的 waiting epoch 接收 response。该 CANCELED 转换还必须从
`uid_waiting_by_vpn_s2xlate` 移除旧 UID；真实 reissue 以 marker=0 的新 epoch 重新插入，不能复用旧 marker。

`memblock_l2tlb_pending_req` 只保留其自身的 `request_token`、`pending_entry_generation`、request key、
DTLB-side CSR snapshot、raw entry snapshot 和 derived 字段；不得增加 token 到单 UID 的字段。pending queue
重排只能改变 response 发送顺序，不能改变 token 的 raw payload。

UID 等待维护一个有界二级索引 `uid_waiting_by_vpn_s2xlate[shape_key]`，其中 `shape_key` 只由
`record.vpn + record.s2xlate` 构成。每个 bucket 的有效 WAITING UID 数不能超过
`MEMBLOCK_DUT_L2TLB_DFILTER_SIZE`；超过时 `uvm_fatal`，因为 V2 DTLB filter 的总物理 entry 上限就是该值，
框架不能把无 UID 的单个 `PtwReq` 再可靠地区分为更多并发等待。`register_uid_tlb_record_on_issue()` 插入 WAITING UID，安全
`COMPLETED/CANCELED` 转换时移除；它只用于 request-fire 标记候选查询，不用于 response completion 的最终 raw-hit
判定。这样 `capture_fired_request()` 只遍历同 VPN/`s2xlate` 的等待候选，不扫描完整
`uid_tlb_record_by_uid`；response complete 和 release 仍按各自低频职责使用全表或有限候选集合。

#### `check_l2tlb_release_uid_waiting()` 的全表统计

抽象功能描述：`check_l2tlb_release_uid_waiting()` 只在 L2TLB lifecycle 正常 global-stop release 前，
统计仍未收敛的 UID 等待实例并输出诊断。它不建立 token、不回填 payload、不改变 UID 状态，也不按 owner 名称推断
归属；当前单-owner 合同要求所有有效 `WAITING` record 都在 release 前清零。

```text
check_l2tlb_release_uid_waiting(output int unsigned waiting_count):
  waiting_count = 0
  foreach uid_tlb_record_by_uid[uid]:
      record = uid_tlb_record_by_uid[uid]
      若 record 为 null 或 !record.record_valid：continue
      若 record.uid_tlb_wait_state != WAITING：continue
      waiting_count++
      记录 uid、uid_tlb_wait_epoch、uid_wait_start_sample_seq、lookup key 和 pte_valid 供 release 日志使用
  返回 waiting_count

release caller:
  调用本 helper。
  若 waiting_count != 0：以 UVM_FATAL 拒绝 release；不得把这些 record 标为 CANCELED、不得清表伪装收敛、
                     也不得把它们交给下一 lifecycle owner。
```

中文文字伪代码：`pte_valid` 不是统计 gate，虽然正常 `WAITING` 的 `pte_valid` 应为 0，但状态机异常时仍必须被
检查发现。`COMPLETED` 与 `CANCELED` 只保留历史，不计入等待数；无效或 null record 同样不计入。这个 helper 是
release 路径的诊断检查，不替代 C4/reset 的 cancel，也不改变 response-to-UID multicast 的全表 raw-hit 匹配范围。

#### `register_uid_tlb_record_on_issue()` 与等待实例建立

抽象功能描述：`register_uid_tlb_record_on_issue()` 为 UID 建立可等待 TLB payload 的上下文和新的等待 epoch；
它不把 UID 绑定到某一条 L2TLB response，也不创建 live entry 或 pending token。

```text
register_uid_tlb_record_on_issue(uid):
  current_sample = peek_current_dut_global_sample()。
  若 close_requested=1：
      uvm_fatal；close request 前已经完成的 registration 保持合法，但 flag 写入后的本 helper 调用都不得创建或幂等复用
      WAITING UID，不能仅用 sample 大小比较放过同 sample 的晚到调用。
  若 memblock_sync_pkg.l2tlb_release_admission_closed=1：
      uvm_fatal；global-stop transport cutoff 后不能新建 WAITING UID。
  closing=1 仍为冗余防御性 fatal；关闭 admission 的权威时点不是 closing。
  若当前 UID 已有 WAITING record 且 key/context 相同：幂等返回，绝不重置已有等待实例。
  若当前 UID 仍为 WAITING 却试图建立不同 key/context：uvm_fatal；必须先由 complete/cancel 关闭旧实例。
  冻结 UID 的 request-time lookup key/CSR context。
  uid_tlb_wait_epoch++；不得回绕为 0。
  uid_tlb_wait_state = WAITING；pte_valid = 0。
  uid_wait_start_sample_seq = current_sample。
  若该值为 0：说明首个 post-reset CSR monitor sample 尚未发布，uvm_fatal；不得由 issue 路径推进 global sample。
  uid_tlb_first_request_fire_sample_seq = 0。
  若 uid_waiting_by_vpn_s2xlate[{vpn,s2xlate}] 已有 MEMBLOCK_DUT_L2TLB_DFILTER_SIZE 个有效 WAITING UID：
      uvm_fatal；不得静默丢弃、覆盖或全表 fallback。
  将 uid 插入 uid_waiting_by_vpn_s2xlate[{vpn, s2xlate}]；重复插入为 uvm_fatal。
```

#### `mark_waiting_uid_records_on_request_fire()`

抽象功能描述：该 helper 在 responder 已观察到一笔真实 DTLB request fire 后，为可能被该 request/后续 response
命中的 WAITING UID 记录 request-fire provenance。它只写入取消边界所需的 sample 标记，不分配 token、不选择
唯一 UID、不改变 response multicast 的命中语义。

```text
mark_waiting_uid_records_on_request_fire(pending):
  shape_key = {pending.vpn, pending.s2xlate}
  从 uid_waiting_by_vpn_s2xlate[shape_key] 取得有限候选 UID 列表。
  foreach candidate_uid:
      record = uid_tlb_record_by_uid[candidate_uid]
      若 record 为 null、!record.record_valid 或 state != WAITING：继续并清理悬挂索引项。
      candidate_key = pending.csr_snapshot.make_lookup_key(record.vpn, record.s2xlate)
      若 candidate_key != pending.request_lookup_key：继续。
      若 record.uid_tlb_first_request_fire_sample_seq == 0：
          record.uid_tlb_first_request_fire_sample_seq = pending.request_fire_sample_seq
  不建立 candidate_uid -> pending.request_token 的绑定；一个 fire 可以标记多个 WAITING record。
```

中文文字伪代码：request fire 的可见信息只有 VPN、`s2xlate` 与 C-2 DTLB-side CSR；因此先用二级索引缩小
候选，再用同一 CSR 生成 request key 做一致性确认。标记只表示该等待实例确实进入过 L2TLB request 生命周期，
不是说该 token 只属于它。未观察到 request fire 的 WAITING record 保持 marker=0，后续真实 request 到来时再标记；
这类 record 不会被本次 C4 barrier 按 responder 规则误取消。

#### response complete 与 flush/reset cancel

抽象功能描述：`complete_waiting_uid_records_by_response()` 在 response 已在安全 sample 被 DUT 接收后，
以该 sample 中 `PTWNewFilter` 实际可见的 CSR 重放 raw hit，并将同一份 raw snapshot 多播给所有命中的
`WAITING` UID。它不改变 token lifecycle；一个 token 对应零个、一个或多个 UID completion 都是正常情况。

`entry_matches_uid_at_response()` 的抽象功能描述：该纯 helper 用 UID 的 `vpn/s2xlate` 和 response fire 当拍的
filter CSR 构造临时 lookup key，再调用 `entry_matches_request_raw()`。它只回答该 response 是否会被 DUT 的
filter 接收，不改写 UID 的 issue-time key、CSR snapshot、waiting epoch 或 live entry。

```text
complete_waiting_uid_records_by_response(pending, response_filter_csr_snapshot):
  要求 response_filter_csr_snapshot 是当前 response sample 的 top C-2 history 项；无效则 uvm_fatal，
  不得退回 record.csr_snapshot 或 runtime latest。
  match_count = 0
  foreach uid_tlb_record_by_uid[uid]:
      record = uid_tlb_record_by_uid[uid]
      若 record 不是有效 WAITING instance：continue
      若 !entry_matches_uid_at_response(pending.entry_snapshot, record,
                                        response_filter_csr_snapshot)：continue
      record.copy_entry_fields(pending.entry_snapshot)  // 只复制 raw payload/provenance
      populate_uid_record_derived(record, pending.entry_snapshot,
                                  response_filter_csr_snapshot)
      record.uid_tlb_wait_state = COMPLETED
      record.pte_valid = 1
      从 uid_waiting_by_vpn_s2xlate[{record.vpn, record.s2xlate}] 移除 record.uid
      set_status_field(record.uid, MEMBLOCK_STATUS_TLB_MAPPED, 1)
      记录 response sample、response-visible CSR sample/update sequence 与 record issue-time CSR sequence
      match_count++
  若 match_count == 0：记录 no-UID/duplicate-response UVM_LOW 信息
  返回 match_count
```

`populate_uid_record_derived()` 必须用 **该 UID 自己**的 `record.vpn/s2xlate` 派生 normal-leaf PPN/GVPN；不能把
`pending.request_*` 直接复制给范围内的其它 UID。其 mode/位宽解释使用本次 raw hit 实际采用的
`response_filter_csr_snapshot`，而 `record.csr_snapshot` 保持 issue-time 历史，不得被覆盖。若 response-visible
mode 与 raw snapshot 的派生前提无法兼容，derived 字段保持无效并记录 `UVM_LOW`，不能为了 debug 强行按旧 CSR
生成地址。effective fault 时同样保持 derived 字段无效。该扫描只在 response complete 的低频事件执行，最大对象为
当前 UID record 集合；不进入每拍 ready/selector 路径。

抽象功能描述：`cancel_waiting_uid_records_for_flush()` 在 filter 的 C4 due 边界失效尚未获得 payload 的旧 UID
等待实例。它不依赖 token-to-UID 绑定，不删除 UID 历史 record，也不回滚已 `COMPLETED` 的 record。

```text
cancel_waiting_uid_records_for_flush(barrier):
  foreach uid_tlb_record_by_uid[uid]:
      record = uid_tlb_record_by_uid[uid]
      若 record.state == WAITING &&
         record.uid_tlb_first_request_fire_sample_seq != 0 &&
         record.uid_tlb_first_request_fire_sample_seq <= barrier.anchor_sample_seq:
          record.uid_tlb_wait_state = CANCELED
          record.pte_valid = 0
          从 uid_waiting_by_vpn_s2xlate[{record.vpn, record.s2xlate}] 移除 record.uid
          记录 uid/wait_epoch/barrier event
      若 record.state == WAITING && record.uid_tlb_first_request_fire_sample_seq == 0：
          保持 WAITING，记录“尚未观察到 L2TLB request fire”，不因该 barrier 取消
  reset 时对所有 WAITING record 执行同样的 CANCELED 转换，并清空 uid_waiting_by_vpn_s2xlate。
```

`apply_due_l2tlb_flush_barriers()` 先删除旧 pending token，再调用该 helper。C0 只登记 barrier，不能提前
取消 UID；C4 到期前由已经安全完成的 response 多播得到的 `COMPLETED` record 保持完成，只有已经观察到
`request fire sample <= barrier.anchor_sample_seq` 且仍为 `WAITING` 的旧 instance 才转为 `CANCELED`。未观察到
request fire 的等待实例由后续 request、redirect 或 reset flow 处理，不由该 L2TLB barrier 猜测其 DTLB filter 状态。
同一 logical UID 真正重新 issue 时由
`register_uid_tlb_record_on_issue()` 建立新 `uid_tlb_wait_epoch`，旧 `CANCELED` record 不参与新 response。

runtime reset 不复用上述 C4 条件：由共享 lifecycle plan 按 reset epoch 将全部 `WAITING` record 写为 `CANCELED`，清空
`uid_waiting_by_vpn_s2xlate`，并与 token/driving/barrier、adapter raw/context/pending invalidate/live-entry/range-index、
CSR history 的固定顺序一起收敛。该 reset 不 release owner、不清 global sample 或 `owner_claimed_once`；只有 ENABLED topology
的同一 owner 在 CSR history warm-up 后继续服务，DISABLED/NO_OWNER 不启动或 re-arm responder。payload plan 只提供 UID 状态转换和 index 清理，
不另建第二套 reset owner 或直接清 adapter queue。

独立 permission 的数据链必须显式实现：

```text
S1/S2 独立 plus/getter
  -> LEVEL_WEIGHT_EN 与 MAIN_MEM_RANGES_EN 组合检查
  -> S1/S2 独立 raw 随机、effective fault 选择与 mode/fixup 或 fault passthrough
  -> active S1 的 build_s1_sector_payload()：split PPN、valididx 与 one-hot pteidx 编码
  -> build/copy/drive 三个边界调用 validate_s1_sector_payload_consistency()
  -> normal leaf 的 resolved PPN 派生，或 fault raw PPN 按已编码 sector payload 原样冻结
  -> lookup miss 创建的 live memblock_tlb_entry（含 stage active、冻结 mode/root、CSR context seq、generation、s1_pte_* / s2_pte_*）
  -> lookup hit 逐字段复用 live entry，不重新随机
  -> 每次 request 的 copy_from()/clone/debug 逐字段冻结到 pending snapshot
  -> fill_dtlb_resp_from_entry()
  -> response sample complete 后按 raw hit 多播回填所有 WAITING UID record
  -> s1_entry_perm_* / s2_entry_perm_* 各自 interface 字段
```

`fill_dtlb_resp_from_entry()` 必须只以 S1 `s1_pte_*` 驱动 S1 payload（其中 `s1_pte_v` 映射到
`s1_entry_v`），只以不含 V 的 S2 `s2_pte_*` 驱动 S2 permission，以
`s1_entry_ppn_raw/s2_entry_ppn_raw` 驱动对应 interface PPN 字段，并分别使用
`s1_tag/s1_asid/s1_vmid` 与 `s2_tag/s2_vmid` 驱动对应 response context 字段。`s1_resolved_ppn` 与
`s2_resolved_ppn` 不直接驱动 interface；它们只在 normal leaf 用于 allStage S2 输入和 debug，且 range hit 的
debug 值必须来自 pending.request_*，不能使用 entry anchor 的派生值。fault response 不消费它们，也不要求建立异常
`gpaddr`。对八个 S1 sector wire，driver 在发送前必须调用
`validate_s1_sector_payload_consistency(pending.entry_snapshot, DRIVE, null, null)`，随后逐项直接驱动：
`s1_ppn_low_i = entry.s1_ppn_low[i]`、`s1_valididx_i = entry.s1_valididx[i]`、
`s1_pteidx_i = entry.s1_pteidx[i]`。driver 严禁使用 `(entry.pteidx[i] != 0)`、数值 index 转 Bool 或从 driver
重算 PPN/sector。
S1 PF、S2 GPF 和 S2 GAF 分别直接映射对应的 `fault_effective_*`；S1 AF 固定为
`fault_effective_s1_af || pending.entry_snapshot.pmaAF`，以保留四类模型外的 legacy PMA tail。驱动前若
`pending.entry_snapshot.pmaAF=1` 且任一 `fault_effective_*` 为 1，必须以 `L2TLB_PMA_FAULT_MIX` 报
`uvm_fatal`，不得把两个 fault 同时送入 DUT。`fault_raw_*` 只能通过 snapshot、UID 和 debug 观测，不能把 raw
fault 重新送入 DUT。该 helper 只能读取 pending snapshot/entry 中已冻结的字段，不得读取 current CSR 或重新调用
随机/合法化 helper。
`fixup_pte_legal()` 必须改为只对指定 stage 的 `LEGAL` PTE 字段组运行的 helper，不能先修正 S1 再复制给
S2。该 helper 的 S1 字段组包含 `s1_pte_v`，S2 字段组不包含 V；S2 只能修正实际存在的 R/W/X 等字段，
不得读取、写入或推导 `s2_pte_v`。其 A/D 强制规则也只在 `fault_stage_selected == NONE` 且该 stage mode
为 `LEGAL` 时执行；同一条件下 S1 必须同时将 `s1_pte_v` 强制为 1。

### UID record 双阶段字段迁移

`memblock_uid_tlb_record` 同步删除旧共享 `ppn/pte_*/level/pbmt/pte_n` 记录字段，迁移为独立的
`s1_entry_ppn_raw/s1_resolved_ppn/s1_pte_r/s1_pte_w/s1_pte_x/s1_pte_u/s1_pte_g/s1_pte_a/s1_pte_d/s1_pte_v`
及 `s1_level/s1_pbmt/s1_pte_n`，以及不含 V 的
`s2_entry_ppn_raw/s2_resolved_ppn/s2_pte_r/s2_pte_w/s2_pte_x/s2_pte_u/s2_pte_g/s2_pte_a/s2_pte_d`
及 `s2_level/s2_pbmt/s2_pte_n`，并新增 request-specific `request_gvpn`；必要的 S1 sector raw 字段也按 S1 语义保存。UID record 不得增加
`s2_pte_v` 或以其它字段代替它。
UID record 中的 `s1_resolved_ppn/s2_resolved_ppn` 是该 UID 所绑定 request 的 derived 字段，不是 live entry 的
anchor debug 字段；它们只可在 token complete 时从 `pending.request_*` 复制。若 `request_derived_valid=0`，
record 的 derived 字段保持无效默认值，不能由 raw snapshot 或 current CSR 补算。
其中 S1 sector raw 字段必须明确包含 `s1_addr_low`、`s1_ppn_low[8]`、`s1_valididx[8]` 与
one-hot `s1_pteidx[8]`；不得把旧的数值 `pteidx` 数组继续复制到 UID record。
UID record 同时保存 `s1_tag/s1_asid/s1_vmid/s2_tag/s2_vmid`，以及
`fault_raw_s1_pf/fault_raw_s1_af/fault_raw_s2_gpf/fault_raw_s2_gaf` 和
`fault_effective_s1_pf/fault_effective_s1_af/fault_effective_s2_gpf/fault_effective_s2_gaf`，并额外逐字段保留
legacy `pmaAF`；不得把 `pmaAF` 改名、折叠或迁入上述四类 fault。
UID record 还必须保存 `fault_stage_selected`、`s1_stage_active/s2_stage_active`、
`s1_translation_mode_at_build/s2_translation_mode_at_build`、
`s1_pte_mode_at_build/s2_pte_mode_at_build`、
`s1_root_ppn_at_build/s2_root_ppn_at_build`、`csr_context_seq_at_build` 与 `entry_generation`。
UID record 还必须保存 `uid_tlb_wait_epoch`、`uid_tlb_wait_state`、`uid_wait_start_sample_seq` 与
`uid_tlb_first_request_fire_sample_seq`。其中后者在该等待实例尚未观察到对应 L2TLB request fire 时为 0，
一旦观察到首个 fire 就冻结为该 fire 的 DUT global sample；它只用于 C4 取消边界，不是 token 或 UID owner。
其中 `entry_generation` 是实际复制的 response payload provenance，不承担 UID 与 token 一对一关联；
`uid_tlb_wait_epoch` 在 UID 每次新的 TLB 等待上下文建立时递增，不能复用或回绕。

| UID 等待状态 | `pte_valid` | 可接受操作 |
|---|---:|---|
| `WAITING` | `0` | 等待任一会被 DUT raw hit 的 response；marker=0 表示 responder 尚未观察到其 request fire，允许一个 response 同时命中多个 WAITING record。 |
| `COMPLETED` | `1` | payload 已冻结；不得被后续 response 覆写。 |
| `CANCELED` | `0` | 保留历史/debug，但不得参与任意 response 回填；仅真实 reissue 才产生新等待 epoch。 |

`copy_entry_fields()` 必须仅在 `WAITING` record 已通过 `entry_matches_uid_at_response()` 后，逐字段复制
两套 fault、独立 `pmaAF`、两套 S1/S2 raw payload 及创建时 provenance；不能从其中一套覆盖另一套，也不得重新读取
UID 回填时的 current CSR。它不得改写 `uid_tlb_wait_epoch/uid_tlb_wait_state/uid_wait_start_sample_seq`；调用者在
copy 成功后必须调用 `validate_s1_sector_payload_consistency(record, COPY, pending.entry_snapshot, null)`，再以该 UID 自己的
VPN/GVPN 调用 `populate_uid_record_derived()`，最后将 state 转为 `COMPLETED`。record 已有的 `csr_snapshot` 继续表示 UID 发射/request-time 上下文；
新增的 `*_at_build` 与 `entry_generation` 表示它实际绑定的 live entry 创建上下文，二者必须并列保留，不能互相覆盖。

后续只有在该 UID record 不含 effective fault 时，才允许按其 `s2xlate` 选择最终物理地址或最终 translation 属性：

| `s2xlate` | 最终 translation 字段来源 |
|---|---|
| `noS2xlate` / `onlyStage1` | `uid_record.s1_*` |
| `onlyStage2` / `allStage` | `uid_record.s2_*` |

本 plan 不新增或保存折叠的 `final_paddr`、`final_paddr_valid`。后续若有 RM/checker 需要最终阶段地址，
应在对应专项中按该 UID record 的 `s2xlate` 直接选择 S1 或 S2 PAddr，并定义自身的有效性语义；不得让
L2TLB response sequence 的随机 payload 预先影响 DCache responder。UID record 的 debug、检查与两阶段
行为分析始终保留并输出 S1/S2 两套原始字段，不折叠成共享记录。

debug/基础统计至少记录：lookup miss/hit、`s2xlate`、`entry_generation`、`csr_context_seq_at_build`、
S1/S2 stage-active、冻结 mode/root、`LEVEL_WEIGHT_EN`、`MAIN_MEM_RANGES_EN`、四个 `fault_raw_*`、四个
`fault_effective_*` 与独立 `pmaAF`、
S1/S2 level、raw/resolved PPN、PTE.N、S1/S2 permission、S1/S2 raw PBMT、各权重 profile、
`s1_addr_low/s1_ppn_low[8]/s1_valididx[8]/s1_pteidx[8]`、选中 sector 重组的 canonical PPN[3:0]、
`request_token/pending_entry_generation/uid_tlb_wait_epoch/uid_tlb_wait_state/uid_wait_start_sample_seq/
uid_tlb_first_request_fire_sample_seq`、UID completion/cancel 数
及 complete/cancel 原因。若同 key 重建，日志必须能同时看到旧 entry 已删除的 generation 与新 entry 的
generation，及旧 UID waiting instance 已取消、新 waiting instance 已建立，便于区分 fence 前后 payload。
DCache responder 的实际 A/C fire、range check 和 backing-line 懒分配继续由既有 memory responder 独立记录；
该记录只验证激励生成是否符合本 plan，不承担 DUT checker 或 scoreboard 功能。

## 七、验收范围

- 四种 `s2xlate` 均能生成 response；非生效字段的 `fault_effective_*` 为 0，四个 effective fault 至多一个为 1，
  对应 raw fault 仍完整保留。`pmaAF` 不参与该四类权重或选择；单独 `pmaAF=1` 保持 S1 AF legacy tail，
  `pmaAF=1` 与任一 effective fault 并发必须 `uvm_fatal(L2TLB_PMA_FAULT_MIX)`。
- 多个原始随机候选同时为 1 时，先按对应 plus 权重选择；最高权重相同时按默认优先级选择，且 debug
  可追溯原始候选、权重与最终结果。
- 每个 lookup miss 的新 entry 都完成四个 raw fault 的随机采样和一次 effective fault 选择；同 key lookup hit
  不重新随机、不重新执行 GAF 分支，且每次 request 的 snapshot 都逐字段保留命中 entry 的 raw/effective fault
  和完整 S1/S2 payload。
- 正常 lookup miss 只能经 `common_data_transaction::build_tlb_entry_for_key_with_csr()` 调用
  `tlb_map_builder::build_payload_for_key_with_csr()` 创建新 entry；旧
  `build_tlb_entry_for_req()` 仅验证其 key 转换后转调 builder core。新 entry 构造期间命中
  `update_addr_fields()`、`choose_paddr()`、`randomize_pte_bits()` 或旧全局 `fixup_pte_legal()` 的调用必须从
  builder core 移除，不得回退到共享 PTE/地址路径。
- `pmaAF` 默认仍为 0，且本 plan 不生成其写者。对独立 PMA/PMP 专项直接写入的 legacy entry，pending/UID copy
  必须保持同值，response 的 S1 AF 必须等于 `fault_effective_s1_af || pmaAF`；不得把它写入四个 raw/effective
  fault、S2 GAF 或 fault priority。
- 新 entry 的 `s1_stage_active/s2_stage_active`、冻结 S1/S2 translation mode、PTE profile mode、root、`csr_context_seq_at_build` 与
  `entry_generation` 均由同一 DTLB-side request CSR snapshot/lookup miss 建立；pending snapshot 立即逐字段保存
  这些值，UID record 仅在对应 raw-hit response complete 后保存这些值，driver 不读取 current CSR 重建它们。
- inactive stage 固定 `*_stage_active=0` 且 mode/root/payload 为初始化默认值，不进入 stage payload 构造。
  active stage 的 Bare mode 不属于本 framework 支持输入；必须在建 entry 前 `uvm_fatal`，不得用 level=0 或
  随机 raw payload 伪装为可响应翻译。
- active S1 的 normal leaf、NAPOT 与 fault passthrough 均必须在 BUILD、COPY、DRIVE 三个边界调用
  `validate_s1_sector_payload_consistency()`。定向负例分别篡改 selected `ppn_low`、one-hot `pteidx`、
  `addr_low`、selected `valididx` 或 copy 后任一数组元素，均必须在对应边界 `uvm_fatal`；不得等到 SFENCE
  matcher 返回不命中才暴露错误。
- 同 key hit 只更新 hit 统计，不改变 `entry_generation` 或任一冻结 provenance；fence/flush 删除后再次 miss
  的相同 key 必须获得新的 `entry_generation`，即使 `csr_context_seq_at_build` 恰好相同。
- CSR mode/root 改变但对应 fence 尚未被既有 lifecycle 消费时，命中 entry 仍保留旧 mode/root/generation，不能
  被 current CSR 覆盖；fence 删除并重建后，新 entry 才记录新的 request-time mode/root/context，且 generation 更高。
- `MEMBLOCK_L2TLB_LEVEL_WEIGHT_EN=0` 时，两阶段 level 均固定为 `0`；即使 level 权重全 0 或被 testcase
  覆盖，也不参与选择、不随机、不报权重错误。
- `MEMBLOCK_L2TLB_LEVEL_WEIGHT_EN=1` 且 `MEMBLOCK_MAIN_MEM_RANGES_EN=1` 时，在 request ready 开启前
  `uvm_fatal`；`LEVEL_WEIGHT_EN=1` 且 `MAIN_MEM_RANGES_EN=0` 时，无 effective fault 的 S1/S2 仅能在各自
  CSR mode 支持的 level 中按权重命中，PTE.N 命中时对应 level 为 0。LEGAL stage 的 raw PPN 必须为合法
  NAPOT 编码；MIXED/EXCEPTION_BIASED 保留 raw 编码，非 canonical 时 model-derived PPN 无效。存在 effective
  fault 时两套 stage 保留候选 level/PTE.N/raw PPN，不执行 fault-specific 语义覆写，但仍满足接口编码约束。
- 所有 `PBMT_*_WT` 均处于 `0..100`；`LEVEL_WEIGHT_EN=1` 时参与选择的 `LEVEL_*_WT` 也处于 `0..100`。
  S1/S2 的 level、PBMT 各自每次只经一次标准 `dist` 选择一个枚举值。PBMT 三项全 0、有效 mode 过滤后的
  level 权重全 0、启用后的参数越界或随机约束不可满足时必须 `uvm_fatal`；不得按 fault 优先级收敛、静默
  fallback 或建立 retry loop。
- interface PPN 始终来自 raw PPN payload；normal leaf 的 resolved PPN 必须与 level、VPN/GVPN、NAPOT 和
  S1 sector 字段一致。effective fault 只检查 raw interface 字段可编码；其 resolved PPN 即使保留也只作 debug，
  不要求与 `genGVPN()` 或异常 GPA 一致，不作为正常 DCache 映射资格。onlyStage2 的 request GVPN 与 allStage
  的 S2 GVPN 仅在 normal leaf 按 `hgatp.mode` 约束为 Sv39x4=29 位、Sv48x4=38 位；allStage 的 `s2_tag`
  由 `derive_allstage_raw_s2_tag()` 的 raw protocol anchor 生成，canonical normal leaf 时它与
  `s1_resolved_ppn` 相同，non-LEGAL 非 canonical NAPOT 时仍可存在但不使 derived PPN 变为有效；任何路径都不得
  因位宽截断改变 raw tag。
- normal leaf 的 LEGAL S1 NAPOT directed 场景必须覆盖 sector `0/3/7`：`s1_pte_n=1` 时仅能在 level `0` 生效，
  并以 `{s1_entry_ppn_raw[0], s1_ppn_low[s1_addr_low]}` 检查/形成 `4'b1000`；不得以
  `s1_entry_ppn_raw[3:0]` 判断 NAPOT。pending snapshot、UID record 与 driver 发出的联合编码必须完全相同。
  MIXED/EXCEPTION_BIASED 的非 canonical NAPOT 低位必须保留 raw 值，raw range hit 合法但 request/UID derived
  PPN 保持 invalid。
- 每个 active S1 response 的 `s1_pteidx[8]` 必须 one-hot，且唯一高位等于 `s1_addr_low`；`s1_valididx[8]`
  仍按 sector 有效范围独立保存。directed 检查必须确认 response 的八个 `s1_pteidx_*` wire 是 direct Bool
  映射，不出现旧逻辑的 `0,1,1,1,1,1,1,1` 数值投影、多热或全零编码。
- `fault_stage_selected == NONE` 时，active S1 的 `V=1 && R=W=X=0` 必须由
  `reject_unsupported_valid_s1_nonleaf()` 报 `uvm_fatal`。有效 non-leaf 不得被 `resolve_s1_ppn()` 当普通 leaf
  处理；effective fault 伴随的 raw non-leaf permission 仍允许按 fault passthrough 驱动。
- L2TLB response 不建立 DCache PPN owner、DCache memory line 或其它 DCache 注册状态。DCache responder
  只在 DUT 实际 `A.fire/C.fire` 后按既有流程处理请求；`LEVEL_WEIGHT_EN=0` 保留严格 memory-range
  responder 行为，`LEVEL_WEIGHT_EN=1` 与 `MAIN_MEM_RANGES_EN=0` 组合使用 sparse memory 服务。
- PPN helper 不含 raw PPN/level 的 retry loop；合法配置下直接生成并派生 payload。只有配置组合、过滤后
  权重或目标编码不可表达时才 `uvm_fatal`，不得把 DCache 窗口不命中当作随机重选条件。
- S1 的八个 `R/W/X/U/G/A/D/V` 字段、S2 的七个 `R/W/X/U/G/A/D` 字段和三种 PBMT 均可由对应 plus
  权重驱动；S2 不提供 `V` 参数或字段，S1/S2 不发生意外镜像。
- S2 canonical PPN 在任何路径均不得超过 V2 38-bit response wire；S1/S2 VMID 在写 response 前均不得有
  `[15:14]` 非零。任一条件不满足均 `uvm_fatal`，不得截断后继续驱动，因为 14-bit response VMID 会被 DUT
  零扩展比较且无法匹配 16-bit CSR VMID。
- PTE profile truth table 必须被定向验证：无 effective fault 的 LEGAL S1 无论 V/A/D 原始权重为何，最终均为
  `V=1,A=1,D=1`；无 effective fault 的 LEGAL S2 最终均为 `A=1,D=1`，且全流程不存在 S2 V。
  在 MIXED、EXCEPTION_BIASED 与 effective-fault 三组路径中，分别设置 `A_1_WT=0,D_1_WT=100` 与
  `A_1_WT=100,D_1_WT=0`，验证 UNKNOWN 下 A/D 各自独立随机且不被 profile/fixup 覆盖。effective-fault + LEGAL
  还必须验证不执行 V/A/D fixup，保留 profile 后的原始结果。
- 同一 lookup key（`vpn + asid + vmid + s2xlate`）在未被 reset/flush/SFENCE/HFENCE 或其它显式失效删除前，
  后续 response 的 S1/S2 payload、raw/effective fault 和 provenance 必须与第一次 miss 完全相同；key miss 或
  entry 被删除后重建时才允许重新随机。旧 pending snapshot 不被新 entry 覆盖。
- 每个 accepted L2TLB request 都保留唯一 `request_token` 与 `pending_entry_generation`，但这两个字段只描述
  response token，不描述唯一 UID。response complete 必须扫描全部 `WAITING` UID record，并用
  `entry_matches_request_raw()` 选择 DUT 也会命中的记录；允许 0/1/多个 completion，禁止只比较 exact key 或按
  UID 年龄猜测。
- response complete 后仅将通过 `entry_matches_uid_at_response(pending.entry_snapshot, record, response C-2 CSR)` 的
  `WAITING` UID 转为 `COMPLETED` 并置 `TLB_MAPPED`。同一 raw snapshot 覆盖多个 UID 时，raw payload/provenance
  相同，而 derived PPN/GVPN 必须按每个 UID 自己的 VPN、response-visible CSR 独立生成。无 UID match 的
  prefetch/duplicate response 只记录 info，正常完成 token；response C-2 history 缺失必须 `uvm_fatal`。
- SFENCE/HFENCE/flush 在 C4 due 时只取消 `uid_tlb_first_request_fire_sample_seq != 0` 且不晚于本 barrier anchor 的
  `WAITING` UID instance；marker=0 或晚于 anchor 的等待实例不被本 barrier 猜测取消。reset 仍取消所有
  `WAITING` instance。C4 是外部 response fire 的严格截止点，已在 C4 前安全完成的 record 保持 `COMPLETED`，
  符合 marker 条件的其余旧 instance 转为 `CANCELED`。
- flush 的 directed 场景必须同时覆盖：flush 前的 `pending_q` token、C0 同拍已经 fire 的 request、一个 response
  在 C4 前同时 raw-hit 两个 UID、以及没有任何 UID match 的 duplicate response。C4 到期时仍在 `pending_q` 的
  token 与具备 pre-anchor request-fire marker 的 WAITING UID instance 都必须取消；marker=0 的 UID 必须保持
  WAITING。同 key 重建后的新 response 不得回填旧 CANCELED instance。
- C0 CSR change 后旧 token 在 C2/C3 response fire 的 directed 场景必须覆盖：response-visible C-2 CSR 下的 raw
  mismatch 只能 complete token、UID 保持 WAITING；若该 UID 已有 pre-anchor request-fire marker 才在 C4 cancel；global 或新 context 仍 raw-hit 时才允许 UID
  complete。每个 testcase 固定一个 responder owner；posedge monitor 产生 C-2 history 的 global sample，negedge sequence 只读取
  `peek_current_dut_global_sample()`，不得使用局部计数或 owner handoff 重新解释 history。
- 同 key 在 flush 后重建时，新 request 使用新的 `entry_generation` 和新的 UID waiting epoch；旧 `CANCELED` UID
  命中回填 helper 时必须被 state gate 拒绝并记录 UVM_LOW，不得静默改写。
- CSR 翻译根或 mode 变化场景由 testcase 按既有规则同时构造 SFENCE/flush；本 plan 不实现对应
  invalidation 逻辑，只在既有 lifecycle 到期时同步取消 UID waiting instance。`csr_context_seq_at_build`、
  mode/root provenance 与 generation 不得变成自动清表、自动拒绝 hit 或第二个 lifecycle owner。
- 既有 L2TLB token 分配、latency、reorder、flush/reset 与 driver hold 的时序由 shared lifecycle/timing plan 定义；本 plan
  只为既有 token 增加 UID multicast-complete/cancel bookkeeping。`idle_stop` 只可用于输出低频 idle 诊断，不能关闭 admission、不能设置 stopping，
  不得在 `global_stop_requested=0` 时退出/释放唯一 owner，也不得绕过 timing plan 的完整
  `release_grantable(owner, current_reset_epoch)`：final inactive、monitor final settled、final mailbox recycle proof/mailbox EMPTY、closing、
  response/adapter queue drain、raw-fence intake close、当前 epoch required reset ack 和 `!reset_active` 必须同时成立；该 gate 返回真后，
  parent 才写匹配 grant，owner 再以 `grant && release_grantable()==1` 清 `claim`。
- global stop 的两阶段 admission close 由 timing plan 定义：driver 必须先在同一 `drv_cb` 完成 bounded sample anchor/probe，
  reset release 后先完成 current epoch NORMAL/inactive baseline；parent 才只提出 global stop/停止 routing。baseline 后 owner 在下一真实
  `drv_cb` 完成本拍 capture/UID registration 后写 admission-settled 与当前 epoch 的 `close_requested`，该 flag 同时封闭后续
  UID setter/capture 调用；owner 的 stop `RELEASE_STOP` item 经真实 `drv_cb` 由 driver 采样且 `sampled_req_fire=0`、generation/reset epoch 匹配后
  才写 `admission_closed/cutoff`；final item 同样要求 frozen fire=0/ready=0/resp_valid=0，且 terminal ack 后 driver 必须在下一真实 `drv_cb` 完成 final mailbox recycle proof。stale item 必须 `item_done()` 后丢弃。本 plan 的 UID setter 同时消费 close-request 过渡边界和 transport cutoff 边界，
  不得把 closing、grant 或 payload complete 当作 admission-close 替代物。

## 后续实现落点

- `mem_ut/ver/ut/memblock/env/plus.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg` 及新增 testcase preset cfg
- `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_tlb_entry.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/tlb_map_builder.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv`

本 plan 只定义测试框架 response 激励生成；不实现 Scala/RTL 正确性 checker、scoreboard 或功能覆盖率。

## 执行中补充/修正（IMPLEMENTATION_DELTA）

### 1. PTE profile provenance 冻结

[IMPLEMENTATION_DELTA]

来源：首轮实现 review 发现原 plan 虽定义 S1/S2 profile 参数，却没有把 profile 作为 entry 的长期 provenance。

原 plan：pending/UID 只显式冻结 translation mode、root 和 CSR sequence；后续 NAPOT/profile consumer 可能重新读取
当前 plus 配置。

实现调整：新增并逐字段复制 `s1_pte_mode_at_build/s2_pte_mode_at_build`。builder 在 lookup miss 时读取一次
`seq_csr_common::get_l2tlb_pte_mode()`；`finalize_pte_fields()`、NAPOT 可解析性检查、pending snapshot 与 UID
payload 仅消费该冻结值。

原因：同一个 live entry 的 payload 必须与创建时 profile 保持一致。若 testbench 后续修改 plus 或切换 preset，
已接受 request 的 LEGAL fixup/NAPOT 判断不能改用新 profile。

影响范围：`memblock_tlb_entry.sv`、`tlb_map_builder.sv`、`common_data_transaction.sv`；不改变 token、pending queue
或 lifecycle owner。

中文文字伪代码：

```text
lookup miss 创建 entry 时：读取一次每个 stage 的 PTE profile，并写入 entry。
pending/UID copy 时：逐字段复制这个 profile。
后续需要判断 LEGAL、MIXED 或 EXCEPTION_BIASED 时：只读取 entry 或 snapshot 中冻结的 profile；不再读取全局 plus。
```

### 2. raw/effective 构造顺序固定

[IMPLEMENTATION_DELTA]

来源：首轮实现 review 发现原 plan 的文字把 raw PPN/PBMT 放在 effective fault 选择之前，容易让 fault-path
意外经过 LEGAL normal-leaf fixup。

原 plan：raw PTE、level、PPN、permission、PBMT 一并生成后才描述 fault gate。

实现调整：固定为“raw PTE + profile、候选 level -> effective fault -> 无 fault LEGAL fixup/final level -> raw
PPN/sector/PBMT”。fault-path 不执行 normal LEGAL fixup 或 `PTE.N` level 收敛。

原因：fault response 可以携带 raw payload，但不得被 normal leaf 的合法化逻辑覆写。

影响范围：仅 entry build 的 helper 调用顺序；不改变 request acceptance、response latency 或 UID multicast。

中文文字伪代码：

```text
先生成两套原始 PTE 并选择候选 level。
选择唯一 effective fault。
没有 fault 时，仅 LEGAL stage 收敛 PTE 和最终 level；有 fault 时保留原始字段。
最后以已确定的字段写 PPN、S1 sector split 和 PBMT；所有路径仍做 wire 宽度检查。
```

### 3. response 宽度 fail-fast

[IMPLEMENTATION_DELTA]

来源：V2 response wire 的 S2 PPN 为 38 bit、VMID 为 14 bit；原 plan 只笼统要求“可编码”，没有定义所有构造与
drive 路径的统一失败策略。实现复查同时发现每个 stage 只有两个 fault 候选，原实现临时使用四项数组并重复
检查索引。

实现调整：`encode_s2_entry_ppn()` 对完整 44-bit canonical PPN 的 `[43:38]` 非零直接报错；builder 建表时和
`fill_dtlb_resp_from_entry()` drive 前均检查 VMID `[15:14]`。`seq_csr_common` 的 S1/S2 fault weight 容器收敛
为各两个候选，配置校验只遍历真实候选。

原因：截断会产生不可追踪的假 payload；尤其 DUT 会把 14-bit response VMID 零扩展后与 16-bit CSR VMID 比较，
高两位非零的 CSR 值不可能命中。

影响范围：仅输入/response 字段合法性检查；不新增地址模型或 fault 行为。

### 4. 无真实 request-fire 的 UID 候选在 release cutoff 收敛

[IMPLEMENTATION_DELTA]

来源：针对真实 dispatch smoke 验证发现，issue 事件会先建立 UID TLB 候选，但 Bare/DTLB hit 路径可能根本不产生
DTLB -> L2TLB request。原实现把该候选一直保持为 `WAITING`，release gate 因而把“没有待回复请求”误报为未完成
L2TLB 生命周期。

原 plan：issue 时建立 `WAITING`；`marker=0` 的实例在 C4 不因 flush 猜测取消，release 检查对所有剩余
`WAITING` 直接 fatal。

实现调整：issue 时仍保留候选记录和 bounded index，不改变 `WAITING`、C4 或 response multicast 语义。只有在
owner 已经通过真实 transport sample 完成 admission cutoff，且 `pending_q`、driving slot 和 barrier 均为空时，调用
`cancel_unbound_uid_tlb_records_at_release()`：对 `uid_tlb_first_request_fire_sample_seq==0` 的候选显式记录
“本 epoch 未观察到真实 L2TLB request”，转为 `CANCELED` 并从 index 删除；对 marker 非零的 WAITING 仍打印完整
上下文并 `uvm_fatal`。

原因：`marker=0` 在 admission 已关闭后证明该 UID 没有进入 L2TLB responder 的 request-fire 账本，不能要求一个
不存在的 response；而 marker 非零代表 DUT 已接受过真实 request，必须继续等待/完成，不能借 release 清理掩盖缺口。

影响范围：仅 `common_data_transaction` 的 release-time UID 收敛和 L2TLB owner final drain；不调用
`apply_dut_*`、不构造 response、不修改 token/pending/latency/flush 时序，也不允许在 cutoff 前取消候选。

中文文字伪代码：

```text
issue：建立 UID TLB candidate，marker = 0。
真实 DTLB->L2TLB fire：把匹配 candidate 标记为 marker != 0，继续按真实 response 回填。
C4：只取消 marker != 0 且属于旧 barrier 的 WAITING；marker = 0 继续保留。
release cutoff + token/barrier 全空：
    marker = 0 -> 显式 CANCELED（证明没有 L2TLB request）；
    marker != 0 且仍 WAITING -> uvm_fatal，打印 uid/key/epoch/fire sample。
```

### 5. `idle_stop` 只做诊断，不能关闭 active owner

[IMPLEMENTATION_DELTA]

来源：执行时复查发现共享 lifecycle 基线仍保留了旧分支：`idle_count` 达到
`MEMBLOCK_L2TLB_IDLE_STOP_CYCLE` 后会写 `stopping=1`。这与本 plan 已经声明的唯一 owner 合同冲突，
可能在 dispatch 仍会继续产生 DTLB request 时提前关闭 ready。

原 plan：`idle_stop` 只作为低频 no-progress 诊断，`global_stop_requested` 是唯一正常退出来源。

实现调整：保留原有 `idle_count` 计数条件，但达到阈值时只打印一次 `uvm_warning` 并将计数饱和在阈值；
不写 `stopping`、不关闭 admission、不发送 release item。只有观察到公共 `global_stop_requested` 才进入既有
admission close、pending drain 和 final inactive 流程。任何 progress、CSR/flush hold、outstanding 或尚未开放 ready
仍会将诊断计数清零。

原因：无请求间隔不是 owner 已完成的证明。若 idle 阈值自行停止 responder，下一笔合法 DTLB -> L2TLB request
只能看到 `ready=0`，测试框架会将自身提前退出伪装成 DUT 无响应。

影响范围：仅 `memblock_l2tlb_base_sequence::send_l2tlb_cycle()` 的 idle watchdog；不改 payload、token、
pending queue、latency、flush C4、UID multicast 或 release gate 条件。

中文文字伪代码：

```text
当前 sample 没有 progress、没有 lifecycle block、没有 outstanding，且已至少发出一拍 ready：
    若 idle_count 尚未到阈值：递增计数；
    若刚到阈值：打印一次 no-progress warning；保持 owner、ready 服务和 admission 原样。
任意 progress、flush/CSR block、outstanding 或 global stop：清零 idle_count。
只有 global_stop_requested：置 stopping，进入已有 release close 和排空流程。
```
