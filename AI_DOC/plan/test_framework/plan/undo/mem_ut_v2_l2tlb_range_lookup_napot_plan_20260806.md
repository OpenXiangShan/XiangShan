# V2 L2TLB 普通范围 Lookup 与 NAPOT Payload 复用专项 Plan

状态：`undo`，仅为待执行设计；尚未 coding、compile、smoke 或仿真验证。

共享 lifecycle 约束：`AI_DOC/plan/test_framework/review_doc/undo/mem_ut_v2_l2tlb_single_owner_lifecycle_optimization_review_20260807.md`。
本 plan 只负责 exact/range/NAPOT candidate 与 index，不重新定义 owner、sample、reset、global stop 或 release。
normal global-stop/release 只能复用 timing plan 的完整 `release_grantable(owner, current_reset_epoch)`，不得因为本专项只维护
range index 而摘取部分条件；该谓词同时要求 final inactive、monitor final settled、final mailbox recycle proof 与 mailbox `EMPTY`、
response/adapter drain、raw-fence intake close、当前 epoch required reset ack 和 `!reset_active`，但不包含也不得读取 grant metadata。parent 只有在该 gate
为真时才写入匹配 owner/epoch/generation 的 grant，owner 再以 grant 与同一 gate 共同完成 release。

关联 plan：

- AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2tlb_response_random_payload_plan_20260729.md
- AI_DOC/plan/test_framework/plan/do/mem_ut_v2_sfence_hfence_stage_aware_live_entry_invalidation_plan_20260804.md

## 专有名词与抽象功能说明

本 plan 只扩展测试框架 L2TLB responder 对普通 DTLB request 的 payload 复用查找。它不建模
L2Cache/PTW/memory 下游，也不改变 DUT DTLB 实际补齐 PPN 低位的职责。

| 术语 | 当前含义 | 代码落点 | 示例 |
|---|---|---|---|
| request key | 一笔 request fire 的 VPN、ASID、VMID、s2xlate 组成的请求身份。 | pending.request_lookup_key | 同一 NAPOT 范围内 A/B 的 request key 不同。 |
| entry anchor key | canonical live entry 第一次建立时的 key。 | entry.lookup_key、pending.entry_anchor_key | A 首次建立 entry，B range hit 时 anchor 仍为 A。 |
| canonical live entry | 保存唯一 raw response payload 的 live 表项。 | common_data_transaction.tlb_entry_by_key | raw tag、PPN、permission、fault、generation 的唯一真源。 |
| exact hit | request key 与 anchor key 完全相同的直接命中。 | tlb_entry_by_key[request_key] | 同一 VPN 再次访问。 |
| range hit | request key 不同，但 entry 的 raw hit 形状覆盖请求。 | range index + raw matcher | A 建立 64 KiB NAPOT，A+7 命中。 |
| range index | 从规范化 range shape 找到有限 anchor 候选列表的辅助 map。 | `tlb_anchor_keys_by_range_key` | 禁止每个 request 扫描完整 live table；同一 bucket 可保存多个候选。 |
| lookup shape | context、level、NAPOT 和规范化 VPN/GVPN 组成的覆盖描述。 | memblock_tlb_range_index_key_t | level 1 屏蔽低 9 个 VPN bit。 |
| raw response payload | 实际驱动 PtwRespS2 response 的 tag、PPN、level、N、permission、PBMT、fault 与 sector 字段。 | live entry、pending.entry_snapshot、driver | range hit 时原样复用。 |
| model derived field | 由本次 request VPN/GVPN 计算的模型镜像值。 | pending.request_* | 只服务 allStage 一致性、位宽检查和 debug。 |
| raw protocol GVPN anchor | allStage entry build 时由 S1 raw payload 和构造期 VPN 拼接出的 `s2_tag` 来源。 | random payload plan 的 `derive_allstage_raw_s2_tag()`；结果持久化为 `entry.s2_tag` | 非 LEGAL 非规范 NAPOT 时 tag 仍可驱动，但 request derived 保持 invalid。 |
| effective allStage shape | V2 PtwRespS2.hit 对两阶段 response 合成后的 lookup 粒度。 | derive_allstage_lookup_shape() | S1 1 GiB 与 S2 2 MiB 的组合为 2 MiB。 |
| range coverage rank | exact miss 时用于比较多个 raw-hit candidate 覆盖范围的固定优先级。 | `memblock_tlb_range_coverage_rank_e`、get_tlb_range_match_coverage_rank() | 1 GiB superpage 的 rank 高于 64 KiB NAPOT，因此两者重叠时选 1 GiB。 |
| fault passthrough | effective fault 时保留 raw PTE 字段，不构造 normal GPA/HPA。 | random payload plan 的 fault 分支 | 可以 raw range hit，但不计算 resolved PPN。 |
| response-visible CSR snapshot | external response fire 当拍，`PTWNewFilter` 实际用于 raw hit 的顶层 CSR C-2 历史项。 | timing plan 的 `get_l2tlb_request_csr_snapshot(dut_sample_seq)` | 不能用 UID issue 时冻结的 CSR 判断该 UID 是否被本次 response 命中。 |
| logical UID attempt | 一个逻辑 UID 的一次可回填 TLB 等待实例。 | `uid_tlb_wait_epoch`、`uid_tlb_wait_state` | UID A 的旧等待被 fence 取消后，真实再次 issue 的仍是 UID A，但已是新的 attempt。 |
| UID request-fire marker | 当前 UID attempt 首次被 responder 观察到相应 DTLB request fire 的全局 sample；0 表示还未观察到。 | `uid_tlb_first_request_fire_sample_seq`、`uid_waiting_by_vpn_s2xlate` | C0 已 fire 的等待可在 C4 cancel；C0 后尚未 fire 的等待不按旧 barrier cancel。 |
| UID 等待重建 | 已 `CANCELED` 或 `COMPLETED` 的 logical UID 在真正再次 issue 时建立新的 `WAITING` attempt。 | `register_uid_tlb_record_on_issue()` | 递增 waiting epoch，冻结新的 key/context；不要求 request-origin claim。 |
| entry generation | canonical live entry 的版本，而不是 logical UID 的重发编号。 | `entry.entry_generation`、`pending.pending_entry_generation` | retry 一定有新 token/bind epoch；只有命中 entry 已删除并重建时才有新 generation。 |
| MONITOR ack | L2TLB monitor 同步消费指定 reset-active transport sample 后返回的 reset epoch/transport seq 确认；它不等于 final settled。 | `l2tlb_monitor_reset_ack_epoch/transport_sample_seq`、timing plan `monitor_reset_ackable()` | runtime reset 的 required ack 未收齐时不能 reopen 或 release。 |
| sample mailbox | driver 与唯一 response owner 使用的单槽 frozen transport sample 状态机；monitor 通过同步 analysis imp 消费同一 wrapper，不建立 FIFO。 | `L2tlb_agent_agent_sequencer` 的 `EMPTY -> PUBLISHED -> CONSUMED/DROPPED -> EMPTY` slot | range lookup 不直接写 slot；owner ack 后只能由 driver 在后续真实 `drv_cb` 回收。 |
| final mailbox recycle proof | final sample 已由 owner 终态确认后，driver 在下一真实 `drv_cb` 把 terminal slot 回收为 `EMPTY` 的证明。 | `l2tlb_release_final_inactive_transport_sample_seq`、`l2tlb_transport_sample_recycle_done_seq` | release 需要两个序号相等且 mailbox 为 `EMPTY`；range delete 不能替代该 proof。 |
| direct writer | 一个 runtime state 的直接清理/ack/proof 写者；reset coordinator 和本 range 专项只能请求或读取。 | driver、monitor、adapter、fence/CSR monitor、response owner | range helper 只删除 canonical entry/index，不能自行回 MONITOR ack 或回收 mailbox。 |

关键函数职责：

| 函数/helper | 抽象功能描述 |
|---|---|
| common_data_transaction::get_or_create_tlb_entry_by_req_with_snapshot() | 先查 exact map，exact miss 后查询 range index；返回 canonical entry、anchor key 与结果，不创建 response token。 |
| common_data_transaction::find_tlb_range_hit_by_req() | 以有限个 shape key 找到 raw-hit candidate，并按覆盖范围选择唯一最大 candidate。 |
| common_data_transaction::get_tlb_range_match_coverage_rank() | 从 candidate 实际参与 raw match 的 S1、S2 或 allStage shape 得到固定覆盖范围 rank；它不比较 payload 内容。 |
| common_data_transaction::entry_matches_request_raw() | 复刻 V2 PtwRespS2.hit 的 request 内容匹配，不计算 PPN。 |
| common_data_transaction::register_tlb_range_index() | 新 entry 完整建立后生成并注册它的有限 range index key。 |
| common_data_transaction::unregister_tlb_range_index() | entry 删除前精确移除其全部 range index key。 |
| common_data_transaction::delete_live_tlb_entry_by_anchor_key() | 所有 explicit live-entry 删除共用的原子入口；先反注册 range key，再删除 canonical entry。 |
| common_data_transaction::build_entry_range_index_keys() | 从一个 canonical entry 的 raw response shape 生成有限 index key；不查询 table、不修改 payload。 |
| common_data_transaction::validate_normal_napot_payload() | 在 normal leaf 注册前验证 S1/S2 NAPOT 的 level、sector；仅对 LEGAL profile 验证确定性 raw PPN 编码。 |
| common_data_transaction::populate_pending_request_derived() | normal leaf 时在 pending 中计算本次 request 的模型专用 PPN/GVPN，不驱动 DUT。 |
| memblock_l2tlb_base_sequence::capture_fired_request() | 为每次 request fire 建立独立 token/snapshot，记录 request key 与 anchor key，并只用 raw snapshot 构造 response。 |
| common_data_transaction::mark_waiting_uid_records_on_request_fire() | 用有限 `{vpn,s2xlate}` 等待索引为确实被观察到的 request 记录 UID request-fire marker；不把 token 绑定给唯一 UID。 |
| common_data_transaction::complete_waiting_uid_records_by_response() | 复用 random payload plan 已定义的 helper，以 response fire 当拍的 C-2 CSR 和 DUT 相同的 raw hit 对所有 WAITING UID record 多播回填；它不改变 token 个数。 |
| common_data_transaction::cancel_waiting_uid_records_for_flush() | 复用 random payload plan 已定义的 helper，在 C4 due 边界只取消已观察到 pre-anchor request fire 的旧 WAITING instance；它不重建 live entry。 |

## 目标、依据与不变边界

### 1. 需要解决的问题

当前 lookup 只检查 tlb_entry_by_key[request_key]。首次 VPN=A 建立 superpage/NAPOT entry 后，
VPN=B 即使位于 A 的覆盖范围内也会被当作 miss，并生成新的随机 payload。

V2 DUT 的 DTLB filter 对 response 按 s2xlate + PtwRespS2.hit(vpn, asid, vasid, vmid) 做内容匹配，
不是按 FIFO 顺序或 request ID 匹配。DTLB 收到 raw response 后才调用 stage1.genPPN(vpn) 和
stage2.genPPNS2(gvpn) 生成实际 PPN；PtwReq 只有 vpn/s2xlate，没有 byte offset。

### 2. 最终目标

1. tlb_entry_by_key 仍是 raw payload 的唯一真源，exact-key 仍是最快路径。
2. exact miss 时，以 V2 raw hit 语义查找 superpage、S1/S2 NAPOT、S1 sector 和 allStage 组合范围。
3. range hit 复用 raw payload 与 entry_generation；每笔 request 仍有独立 request_token、UID waiting epoch 和 pending snapshot。
   同一 logical UID 被取消后真实重发时必定分配新的 token/waiting epoch；仅当命中的 canonical entry 已删除并重建时，
   新 attempt 才获得新的 entry_generation。
4. request-specific resolved PPN/GVPN 只保存于 pending，绝不回写 live entry 或驱动 response wire。
5. lookup 使用有限个 associative-map 查询，不扫描完整 tlb_entry_by_key。
6. reset、SFENCE/HFENCE 或其它 explicit entry delete 时，canonical table 与 range index 同步删除。

### 3. 不属于本 plan 的范围

- 不新增 plus 参数。
- 不修改 latency、reorder、ready hold、flush token cancel、driver sample 或 lifecycle owner；只在既有 token
  complete/cancel hook 中接入 random payload plan 已定义的 UID multicast/waiting bookkeeping。
- 不修改 SFENCE/HFENCE 的 stage-aware matcher；只接入其最终 entry delete 生命周期。
- 不新增 DCache owner、memory line、最终 PAddr valid 或 DUT checker/RM/coverage。
- 不让 responder 推断或保存 request 的 byte PAddr。

## 数据模型与 Range Index

### 1. request 与 anchor 字段

memblock_l2tlb_pending_req 的 lookup_key 重命名为 request_lookup_key，不保留旧名称兼容。
除 random payload plan 已规定的 UID waiting 字段外，本专项新增字段：

~~~text
memblock_tlb_lookup_key_t entry_anchor_key
memblock_tlb_lookup_result_e lookup_result
bit request_derived_valid
<PPN width> request_s1_resolved_ppn
<GVPN width> request_gvpn
<PPN width> request_s2_resolved_ppn
~~~

lookup_result 固定为：

~~~text
MEMBLOCK_TLB_LOOKUP_EXACT_HIT
MEMBLOCK_TLB_LOOKUP_RANGE_HIT
MEMBLOCK_TLB_LOOKUP_MISS_BUILD
~~~

request_lookup_key 始终描述本次 request，用于 token audit、range lookup 和 response-to-UID raw match。
entry_anchor_key 描述被复用 canonical entry，用于 range index/debug/provenance。range hit 时两者允许不同。

pending 只沿用 random payload plan 的 `pending_entry_generation`；它不持有 UID 字段。

UID record 必须保存 `uid_tlb_wait_state`、`uid_tlb_wait_epoch`、`uid_wait_start_sample_seq` 与
`uid_tlb_first_request_fire_sample_seq`；这些字段使用 CSR monitor 已推进的 global sample，negedge sequence
只使用 `peek_current_dut_global_sample()`，不得用 lifecycle owner 本地 `sample_seq` 推进时基。等待建立时 first-fire marker 为 0；只有 `capture_fired_request()` 观察到匹配 request 后才冻结其
首个 fire sample。
`request_lookup_key` 是本次 request 的 lookup/audit key；`entry_anchor_key` 只标识 range hit 实际复用的
canonical payload，不能把它当 UID 的 request key。旧 attempt 被取消后，`CANCELED` record 保留历史/debug，
不得因 `pte_valid=0` 自动重新参与回填。真实 reissue 再次调用 `register_uid_tlb_record_on_issue()`，递增
`uid_tlb_wait_epoch` 并建立新的 WAITING context。UID record 中已经完成 payload 的 `entry_generation` 是历史
provenance，不得被重建等待状态清除。

### 1.1 DTLB filter 对齐的 CSR 快照

`PtwReq` 不带 ASID、VMID、mode 或 root PPN；这些字段必须由与 DTLB filter 当前输入同一拍的 CSR 得到。
V2 `MemBlock.scala` 在顶层 `io.ooo_to_mem.tlbCsr` 与 `PTWNewFilter` 之间存在两级 `RegNext`，因此 responder
在 sample C 看到 L2TLB request fire 时，必须使用顶层 CSR sample C-2，而不是 runtime latest。

统一实现由 timing plan 的 `get_l2tlb_request_csr_snapshot(dut_sample_seq)` 负责：CSR monitor 每个 post-reset DUT
global sample 都向固定深度为 3 的 history 发布完整 snapshot；request capture 与 response complete 都以当前全局 sample
查询 C-2。history 未 warm-up、序号不连续或目标项无效时，保持 `ready=0`；若已经发生 request fire 或 external response
fire 仍取不到对应历史项，`uvm_fatal`。request lookup/build 使用 request C-2；UID multicast 使用 response C-2，二者
都不得退回 runtime latest 或 UID 旧 CSR。runtime latest 继续服务 CSR monitor、startup 与 flush event 观察，不能互换。

不在 L2TLB live entry、pending 或 UID record 持久保存 s1_paddr/s2_paddr。接口没有 page offset；
若 debug 需要页基地址，只在 dump helper 中临时计算：

~~~text
page_base = {resolved_ppn, 12'b0}
~~~

### 2. raw 与 derived 的唯一来源

live entry 和 pending.entry_snapshot 保存 canonical raw response：

~~~text
S1:
  s1_tag/s1_asid/s1_vmid/s1_entry_ppn_raw/s1_level/s1_pte_n/
  s1_pte_*/s1_pbmt/s1_addr_low/s1_ppn_low[]/s1_valididx[]/s1_pteidx[]

S2:
  s2_tag/s2_vmid/s2_entry_ppn_raw/s2_level/s2_pte_n/s2_pte_*/s2_pbmt

共同字段:
  fault_raw_* / fault_effective_* / fault_stage_selected /
  stage-active / frozen CSR provenance / entry_generation
~~~

s1_tag/s2_tag 是固定 response anchor。range hit 时不能改成当前 request 的 tag。allStage normal leaf 的
`s2_tag` 在首次 build 时由 random payload plan 的 `derive_allstage_raw_s2_tag()` 写入；该 helper 的 raw protocol
GVPN anchor 与 `request_derived_valid` 是两套语义：前者保证 response wire 有稳定、可表示的 S2 tag，后者只表示
本次 request 是否可生成模型 normal PPN。range lookup、pending derived helper 和 driver 都不得重新计算或覆盖
`s2_tag`。

现有 live entry 的 s1_resolved_ppn/s2_resolved_ppn 若仍保留，只表示首次 build request 的 anchor debug 值；
range hit 不能读取它们作为当前 request 的结果。当前 request 的权威派生值只能是 pending.request_* 字段。

### 3. secondary index

在 memblock_dispatch_types.sv 新增 packed memblock_tlb_range_index_key_t：

| 字段 | 含义 |
|---|---|
| range_kind | S1、S2 或 ALLSTAGE。 |
| s2xlate | entry 的翻译组合。 |
| asid_global | S1 global 映射标志；为 1 时 asid 固定为 0。 |
| asid | 非 global S1 的 ASID/VS-ASID；S2-only 固定为 0。 |
| vmid | onlyStage1/allStage 的 S1 VMID，或 onlyStage2 的 S2 VMID；noS2xlate 固定为 0。 |
| level | 此 key 表示的 lookup level。 |
| napot | 此 key 是否按 V2 64 KiB NAPOT 屏蔽低 4 个 VPN/GVPN bit。 |
| normalized_vpn | 规范化后的 52-bit VPN/GVPN。 |

common_data_transaction 新增：

~~~text
memblock_tlb_lookup_key_t
  tlb_anchor_keys_by_range_key[memblock_tlb_range_index_key_t][$]
~~~

一个 range key 可以对应多个不同 anchor。index value 只保存这些 anchor key，取 payload 时必须逐个回到
`tlb_entry_by_key[anchor_key]`；不能在注册阶段因为同 shape 已存在就覆盖或 fatal。为避免高频 lookup
出现无界候选队列，单个 range key 的 anchor 数量受 `MEMBLOCK_TLB_RANGE_CANDIDATE_MAX` 编译期上限限制，
超过上限立即 `uvm_fatal`；该上限至少覆盖当前 DUT filter 的最大并发窗口。range lookup 只扫描命中 shape
bucket，不扫描完整 live table。

在 `memblock_dispatch_types.sv` 新增：

~~~text
typedef enum int unsigned {
    MEMBLOCK_TLB_COVERAGE_4K,
    MEMBLOCK_TLB_COVERAGE_64K,
    MEMBLOCK_TLB_COVERAGE_2M,
    MEMBLOCK_TLB_COVERAGE_1G,
    MEMBLOCK_TLB_COVERAGE_512G
} memblock_tlb_range_coverage_rank_e;
~~~

enum 数值由小范围到大范围单调递增，只能用于 exact-miss 的 range candidate 选择；不得影响 exact hit、
raw matcher、response wire 或 UID waiting state。

memblock_tlb_entry 新增 range_index_keys[$]。它记录已注册 index key，仅用于删除时精确反注册。
同一 range key 允许保存多个不同 anchor；candidate 数量由
`MEMBLOCK_TLB_RANGE_CANDIDATE_MAX` 编译期上限约束，超过上限直接 `uvm_fatal`，避免高频 lookup 引入无界
候选集合。

frozen translation mode、root PPN 和 CSR context sequence 不加入 range index。它们仍由 entry 保存，用于
mode/level 合法性检查、debug 与 generation provenance；CSR mode/root 改变后的旧 entry 是否失效，继续只由既有
SFENCE/HFENCE lifecycle 决定，不能因 index 查询自动拒绝命中。

规范化规则：

| shape | normalized_vpn |
|---|---|
| `noS2xlate` normal level 0 | 保留完整 VPN；S1 sector 对每个 `valididx=1` 的完整 sector VPN 注册一个 key。 |
| `onlyStage1/allStage` 的 effective normal level 0 | 使用 `Cat(s1_tag, s1_addr_low)` 形成唯一 response anchor VPN；不按 `valididx[]` 展开。 |
| level 1 | 清零低 9 bit。 |
| level 2 | 清零低 18 bit。 |
| level 3 | 清零低 27 bit。 |
| NAPOT | 清零低 4 bit。 |

每次 request 最多查询 NAPOT level-0 加 normal level 0/1/2/3 的有限 shape；S1 再查询指定
ASID 与 global 两个 bucket。不得遍历完整 live table。

build_entry_range_index_keys() 的抽象功能描述：该纯 helper 从已经完成构造的 canonical entry 生成其有限的
raw range shape key。它只读 entry，不访问 table/index，不分配 generation，也不修正 normal 或 fault payload。

~~~text
case entry.s2xlate:
  noS2xlate:
      从 S1 raw tag、ASID/global、level/N 与 sector 字段构造 key。
      normal level-0：为每个 s1_valididx=1 的 sector 生成一个完整 VPN key。
      superpage/NAPOT：以 Cat(s1_tag, 3'b000) 作为 sector-base anchor 后生成规范化前缀 key；
      NAPOT 清零低 4 bit，不读取 s1_addr_low/pteidx 选择范围。
  onlyStage1:
      从 S1 raw tag、ASID/global、VMID、level/N 与 s1_addr_low 构造 key。
      normal level-0：只为 Cat(s1_tag, s1_addr_low) 生成一个完整 VPN key；不读取 valididx/pteidx 选择候选。
      superpage/NAPOT：以 Cat(s1_tag, s1_addr_low) 生成规范化前缀 key；NAPOT 清零低 4 bit。
  onlyStage2:
      从 S2 raw tag、VMID、level/N 构造一个规范化 GVPN key。
  allStage:
      调用 derive_allstage_lookup_shape()，以 Cat(s1_tag, s1_addr_low) 的 S1 response anchor、
      VS-ASID/global、S1 VMID 和组合 shape 构造一个 key。
      effective normal level-0 保留该完整 anchor；effective NAPOT 或 superpage 才按组合 shape 规范化低位。
  default:
      uvm_fatal
返回 key 列表。
~~~

中文文字伪代码：一个 entry 只登记它实际代表的 raw 覆盖形状，查询侧才枚举所有可能 shape。`noS2xlate` 的
`PtwSectorResp.hit()` 会查看 `valididx[]`，因此 base-page sector 可展开多个完整 VPN；`onlyStage1/allStage` 的
`PtwRespS2.hit()` 则用 `Cat(s1_tag, s1_addr_low)` 作 response anchor，不查看 `valididx[]/pteidx[]`，因此只能登记
一个 base-page anchor。S1 non-global entry 只登记自己的 ASID bucket，global entry 只登记 global bucket；查询时
同时查两者。fault entry 也生成 key，但 shape 必须按 DUT raw hit 实际选择的 level/N 解释，不能为方便索引而修正其 payload。

## Range Lookup Flow

~~~mermaid
flowchart TD
    A[DTLB request fire] --> B[capture_fired_request]
    B --> C[冻结 CSR 并建立 request key]
    C --> D{exact key 命中}
    D -->|是| E[复用 canonical entry]
    D -->|否| F[find_tlb_range_hit_by_req]
    F --> G{range candidate 数}
    G -->|1| E
    G -->|0| H[build 新 canonical entry]
    G -->|大于 1| P[按 coverage rank 选唯一最大 candidate]
    P -->|唯一最大| E
    P -->|最大 rank 并列| X[uvm_fatal]
    H --> I[register_tlb_range_index]
    I --> E
    E --> J[深拷贝 raw entry 到 pending snapshot]
    J --> K[冻结 generation/flush version]
    K --> L[normal leaf 时生成 model derived 字段]
    L --> M[driver 只驱动 raw snapshot]
    M --> N[进入既有 pending/latency/flush flow]
~~~

函数调用 Flow 图整体文字伪代码：

~~~text
1. request 在 valid && ready 边界 fire 后，sequence 冻结 VPN、s2xlate 与 CSR snapshot。
2. common_data 先查 exact map；命中时直接复用对应 canonical entry。
3. exact miss 时，range helper 只查询有限 shape 的 secondary index，再以 raw matcher 确认候选。
4. 0 个候选是正常 miss；建立一次新随机 payload、插入 canonical table 后注册 index。
5. 1 个候选是 range hit；raw payload 和 generation 不变。
6. entry 被深拷贝到 request 专属 pending snapshot，冻结 pending entry generation 和 accept flush event version。
7. token 不绑定唯一 UID。normal leaf 时才按当前 VPN/GVPN 计算 model derived 字段；response complete 时再用
   response fire 当拍的 response-visible C-2 CSR，以 DUT raw hit 语义匹配全部 WAITING UID record；不得使用
   UID 建立时冻结的 key/CSR 判断本次命中。
8. driver 只从 raw snapshot 填 response。DUT DTLB 后续按自己的 request VPN/GVPN 补齐实际 PPN。
9. pending latency、reorder、flush cancel、response complete 和 UID 回填继续使用既有 lifecycle；token 仍逐笔
   complete/cancel，而 UID completion 按 raw hit 允许一次 response 多播到 0/1/多个 waiting record。
~~~

### 1. get_or_create_tlb_entry_by_req_with_snapshot()

抽象功能描述：该函数在 DTLB-side CSR snapshot 已冻结后选择 exact/range canonical entry 或建立新 entry。
它返回 request key、anchor key、entry 与 lookup result，不创建 pending token。

输入：VPN、s2xlate、CSR snapshot。

输出/副作用：返回 canonical entry；hit 只更新时间戳，miss 插入 entry 并注册 index。

~~~text
request_key = csr_snapshot.make_lookup_key(vpn, s2xlate)

if tlb_entry_by_key contains request_key:
    entry = tlb_entry_by_key[request_key]
    anchor_key = request_key
    result = EXACT_HIT
    update entry.last_hit_cycle
    return success

if find_tlb_range_hit_by_req(request_key, csr_snapshot,
                              anchor_key, entry):
    result = RANGE_HIT
    update entry.last_hit_cycle
    return success

entry = build_tlb_entry_for_key_with_csr(request_key, csr_snapshot)
insert_tlb_entry(request_key, entry)
if !register_tlb_range_index(request_key, entry):
    delete just-inserted entry
    uvm_fatal
anchor_key = request_key
result = MISS_BUILD
return success
~~~

中文文字伪代码：该函数不能用 current CSR 替代 request snapshot。exact hit 不修改 raw payload，也不访问
range fallback。exact miss 时调用 range helper；其作用是从辅助 index 取得已存在的 canonical entry，而不是
重新随机。只有没有候选时才 build。新 entry 必须先完整建立 raw payload/generation，再注册 index；任一注册失败
必须回滚局部插入，不能残留半注册状态或降级为全表扫描。

### 2. find_tlb_range_hit_by_req()

抽象功能描述：该 helper 在 exact miss 后，按 request context 查询有限个 range index key，复核 raw candidate，
并选择覆盖范围唯一最大的 candidate。它只读 table/index，不创建 entry、不随机、不修改 pending queue。

输入：request key、冻结 CSR snapshot。

输出/副作用：返回 found、anchor key 和 entry；0 个候选是正常 miss；多个 candidate 中唯一最大覆盖范围是
正常 range hit，最大覆盖范围并列才是 fatal。

~~~text
根据 request.s2xlate 构造有限 query shape：
  noS2xlate/onlyStage1：
      查询 S1 的 NAPOT、level 0/1/2/3；
      对每个 shape 查询指定 ASID 与 global bucket。
  onlyStage2：
      查询 S2 的 NAPOT、level 0/1/2/3。
  allStage：
      查询 ALLSTAGE 的 NAPOT、level 0/1/2/3；
      使用 VS-ASID/global 和 VMID。

对每个命中的 index bucket 中的 anchor key：
    同一 anchor 去重。
    确认 canonical table 中存在该 anchor，否则 uvm_fatal。
    调用 entry_matches_request_raw(entry, request_key, csr_snapshot)。
    若 index 命中但 raw matcher 不命中，uvm_fatal。
    若 raw matcher 命中：
        rank = get_tlb_range_match_coverage_rank(entry)。
        将 {anchor key, entry, rank, lookup shape} 加入 candidate。

0 个 candidate：返回 found=0。
1 个 candidate：返回 anchor key 和 entry。
多个不同 candidate：
    找到最大 rank。
    若只有一个 candidate 具有最大 rank：
        用 uvm_info(..., UVM_LOW) 打印 request key、全部 candidate 的 anchor/generation/shape/rank、
        被选 candidate 和“最大覆盖范围优先”原因。
        返回该 candidate。
    否则：
        uvm_fatal，打印全部具有最大 rank 的 anchor/generation/shape；不得依赖 map 遍历顺序。
~~~

中文文字伪代码：range index 只缩小候选，不是最终真相；最终真相仍由 raw matcher 决定。S1 normal
level-0 sector 通过完整 VPN 命中，superpage/NAPOT 通过前缀命中。多个 canonical payload 覆盖同一个
exact-miss request 时，先按实际覆盖范围选更大的映射：例如同一上下文下 1 GiB superpage 与其内部的
64 KiB NAPOT 都命中时，选择 1 GiB entry 并以 `UVM_LOW` 记录。只有多个不同 entry 的最大覆盖范围相同，
当前测试框架才没有可复现的仲裁依据，必须 fatal；不得把较小映射、payload 内容或 associative-map 遍历顺序
当作隐式优先级。

### 2.1 get_tlb_range_match_coverage_rank()

抽象功能描述：该纯 helper 根据一个已通过 raw matcher 的 canonical entry 计算其实际覆盖范围 rank，供
`find_tlb_range_hit_by_req()` 在有限 candidate 集合中做确定性选择。它不读取 current CSR，不修改 entry、index、
pending 或 response payload。

输入：canonical entry。

输出/副作用：返回 `memblock_tlb_range_coverage_rank_e`；stage 组合或 raw level/N 结构不合法时 fatal，
无其它副作用。

~~~text
对 noS2xlate/onlyStage1：
    使用 S1 raw level/N/sector shape。
对 onlyStage2：
    使用 S2 raw level/N shape。
对 allStage：
    调用 derive_allstage_lookup_shape() 得到 effective_level/effective_n。

按实际 match shape 返回：
    level=3：512 GiB rank。
    level=2：1 GiB rank。
    level=1：2 MiB rank。
    level=0 且 NAPOT effective：64 KiB rank。
    level=0 且非 NAPOT：4 KiB rank。

fault entry：
    使用 entry_matches_request_raw() 实际采用的 raw level/N 规则；raw level 非 0 时优先按 level，
    只有 raw level=0 且 raw N=1 时返回 64 KiB rank。
~~~

中文文字伪代码：rank 只描述 entry 对当前 request 的地址覆盖大小，不代表 page table walk 优先级，也不改写
raw response。排序固定为 `512 GiB > 1 GiB > 2 MiB > 64 KiB NAPOT > 4 KiB`。S1 sector 的 normal level-0
entry 只覆盖一个 4 KiB page。allStage 不能分别比较 S1/S2 的原始 level，必须先使用
`derive_allstage_lookup_shape()` 的组合结果，确保选择规则和 ordinary lookup 的实际 raw hit 语义一致。

### 3. entry_matches_request_raw()

抽象功能描述：该 helper 按 V2 PtwRespS2.hit 的 response 内容语义判断 entry 是否覆盖请求。它只读取
raw tag、level、N、sector、ASID/VMID、stage activity，不生成 PPN。

输入：canonical entry、request key、调用点冻结的 filter-visible CSR snapshot。

输出/副作用：返回命中与否；结构性字段不一致时 fatal，无其它副作用。

~~~text
确认 entry.s2xlate == request_key.s2xlate 且 stage-active 合法。

case entry.s2xlate:
  noS2xlate:
      检查 satp ASID 或 s1_pte_g。
      用 request VPN 执行 S1 `PtwSectorResp.hit()` 语义的 sector/superpage/NAPOT raw range match；
      仅 normal level-0 读取目标 sector 的 valididx。
  onlyStage1:
      检查 VS-ASID 或 s1_pte_g，以及 S1 VMID。
      用 request GVA VPN 执行 `PtwRespS2.hit()` 语义的 S1 raw range match；normal level-0 仅比较
      Cat(s1_tag, s1_addr_low)，不以 valididx/pteidx 扩大或拒绝命中。
  onlyStage2:
      检查 S2 VMID。
      将 request VPN 解释为 GVPN，执行 S2 level/NAPOT raw range match。
  allStage:
      检查 VS-ASID 或 s1_pte_g，以及 S1 VMID。
      shape = derive_allstage_lookup_shape(entry)。
      用 shape 和 {s1_tag, s1_addr_low} 比较 request GVA VPN；effective normal level-0 同样不读取
      valididx/pteidx。
  default:
      uvm_fatal
~~~

中文文字伪代码：ordinary lookup 不能调用 SFENCE/HFENCE matcher。fence 的职责是按目标 stage 删除
entry；allStage response hit 的职责是按组合 page granularity 接收普通 request。`noS2xlate` 的 S1 level-0
必须检查目标 `valididx`；`onlyStage1/allStage` 的 `PtwRespS2.hit()` level-0 则只匹配 response 的
`{s1_tag, s1_addr_low}` anchor。one-hot `pteidx` 是 payload 编码一致性检查，不是普通 raw-hit 条件。
superpage/NAPOT 按覆盖范围比较。CSR mode/root 变化本身不自动拒绝旧 entry，已有 SFENCE/HFENCE lifecycle 才是失效边界。
本 helper 有两个受限调用点：request exact/range lookup 传入 request fire 的 DUT global C-2 CSR；response-to-UID
multicast 不复用 UID 保存的 `request_lookup_key`，而是以该 UID 的 `vpn/s2xlate` 和 response fire 的 DUT global C-2 CSR
构造临时 key 后调用本 helper。后者由 random payload plan 的 `entry_matches_uid_at_response()` 封装，不能改写 record
的 issue-time context。

derive_allstage_lookup_shape() 的抽象功能描述：该纯 helper 从同一 allStage entry 的两套 raw level/N 字段
计算 V2 ordinary lookup 的最终粒度。它不改写 S1/S2 字段，也不生成 resolved PPN。

~~~text
effective_level = min(s1_level, s2_level)
effective_n =
    (s1_pte_n && s2_level != 0) ||
    (s2_pte_n && s1_level != 0) ||
    (s1_pte_n && s2_pte_n)

if effective_level != 0:
    使用 level 的 2 MiB/1 GiB/512 GiB 前缀比较
else if effective_n:
    使用 VPN[high:4] 比较，覆盖 64 KiB
else:
    比较完整 4 KiB VPN
~~~

中文文字伪代码：S1 1 GiB 与 S2 2 MiB 的组合范围是 2 MiB。S1 NAPOT 与 S2 4 KiB 的组合范围是
4 KiB，S1 NAPOT 不得把较小 S2 映射扩大。s2_tag/s2_level 仍是 raw response 与 HFENCE.GVMA 的字段，
不能因为本次 allStage range hit 而被改写。

### 4. register_tlb_range_index() 和 unregister_tlb_range_index()

抽象功能描述：register helper 在一个完整 canonical entry 已建立后注册有限个 shape key；unregister helper
在该 entry 删除前精确移除同一批 key。两者不拥有 pending cancel 或 flush owner。

输入：anchor key、canonical entry。

输出/副作用：更新 `tlb_anchor_keys_by_range_key` 和 entry.range_index_keys。

~~~text
register_tlb_range_index(anchor_key, entry):
    确认 entry.lookup_key == anchor_key 且 canonical table 已存在该 entry。
    keys = build_entry_range_index_keys(entry)
    对 keys 中每一项：
        若 anchor_key 已在 index[key]：uvm_fatal，禁止重复注册同一 entry。
        若 index[key].size() >= MEMBLOCK_TLB_RANGE_CANDIDATE_MAX：uvm_fatal。
        将 anchor_key 追加到 index[key]，保持已有 anchor 不变。
        记录 entry.range_index_keys.push_back(key)。
    中途失败时删除本次已经写入的 key，清空 range_index_keys，并返回失败。

unregister_tlb_range_index(anchor_key, entry):
    对 entry.range_index_keys 的每一项：
        若 index 不存在或其中没有 anchor_key：uvm_fatal。
        从 index[key] 删除 anchor_key；若删除后为空，再删除该 index key。
    清空 entry.range_index_keys。

delete_live_tlb_entry_by_anchor_key(anchor_key, delete_reason):
    确认 canonical table 中存在 anchor_key 且 entry 非空；否则 uvm_fatal。
    调用 unregister_tlb_range_index()，它只删除该 entry 已登记的全部 index key。
    删除 tlb_entry_by_key[anchor_key]。
    输出 anchor key、entry_generation、delete_reason；不触碰 pending、UID 或 token counter。
~~~

中文文字伪代码：S1 sector entry 可以有多个 valid sector，因此需要多个 index key。删除不能重新推导或扫描
table 猜测 key，而是使用 entry 已保存的 key 列表。所有 SFENCE/HFENCE 和其它逐 entry delete 路径都必须通过
`delete_live_tlb_entry_by_anchor_key()`，从而先反注册再删除 canonical entry；不得在任意 caller 中直接
`tlb_entry_by_key.delete()`。reset 全清可以同时 `tlb_anchor_keys_by_range_key.delete()` 与 `tlb_entry_by_key.delete()`，
不需要逐 entry 扫描。

### 5. populate_pending_request_derived()

抽象功能描述：该 helper 在 raw entry 已复制进 pending 后，为 normal leaf request 生成测试框架内部的
PPN/GVPN 镜像。它不改 raw snapshot、不驱动 interface，fault 不进入该 helper 的 resolver 分支。

输入：pending.vpn、pending.s2xlate、pending.entry_snapshot、frozen stage mode。

输出/副作用：更新 pending.request_derived_valid 和 pending.request_* derived 字段。

~~~text
默认 request_derived_valid=0，并清零 request_* derived 字段。

若 effective fault 存在：
    直接返回。

若 entry 不是支持的 normal leaf：
    走既有 non-leaf fatal 规则或返回。

若任一有效 stage 的 PTE.N=1 且 napot_raw_ppn_is_model_resolvable(stage_fields) 为假：
    记录 UVM_LOW，保留 request_derived_valid=0 并直接返回。
    不改写 raw PPN、不重随机、不拒绝 range raw hit。

case s2xlate:
  noS2xlate/onlyStage1:
      request_s1_resolved_ppn = resolve_s1_ppn(raw snapshot, pending.vpn)
  onlyStage2:
      request_gvpn = pending.vpn
      request_s2_resolved_ppn = resolve_s2_ppn(raw snapshot, request_gvpn)
  allStage:
      request_s1_resolved_ppn = resolve_s1_ppn(raw snapshot, pending.vpn)
      request_gvpn = request_s1_resolved_ppn
      按 frozen hgatp mode 检查 29/38-bit GVPN 宽度
      request_s2_resolved_ppn = resolve_s2_ppn(raw snapshot, request_gvpn)

request_derived_valid=1
~~~

中文文字伪代码：resolver 是对 DUT DTLB 计算的模型镜像，不替代 DUT。S1 NAPOT 必须联合
s1_entry_ppn_raw[0] 和选中 s1_ppn_low；S2 NAPOT 使用完整 s2_entry_ppn_raw。LEGAL profile 的 NAPOT
编码在 build 时已被固定为可解释值；MIXED/EXCEPTION_BIASED 若保留的 raw low-4 不是 `4'b1000`，本 helper
只把 derived 标为 invalid，仍保留 raw response/range-hit 行为。当前 VPN/GVPN 只写入 pending，不能改 live entry
或 response tag。allStage 的 `s2_tag` 已在首次 build 通过 raw protocol anchor 固定，即使本 helper 因非 canonical
NAPOT 直接返回也不受影响。PtwReq 没有 offset，因此本 helper 不生成 byte PAddr。

### 6. capture_fired_request()

抽象功能描述：此 sequence 函数为每次已经发生的 request fire 建立独立 pending token/snapshot。C0 同拍观察到
flush barrier 不会取消已经 fire 的 request；它继续拥有 latency/due/flush 记账，不拥有 live table 或 range index
生命周期。

输入：采样到的 request、与 DTLB filter 对齐的 CSR 历史快照、common_data_transaction。

输出/副作用：pending_q 新增一个 request，保存 request key、anchor key、lookup result 和 raw snapshot。

~~~text
调用者在读取新 flush event 前保存 fire_visible_event_seq。即使本拍随后记录 C0 barrier，只要 request_fire 为真，
仍调用本函数、创建 pending；不得调用或保留 record_flush_killed_request()。

创建 pending 并分配唯一 request_token。
保存 sampled VPN、s2xlate，以及 get_l2tlb_request_csr_snapshot(dut_sample_seq) 返回的 DTLB-side CSR snapshot。
该 snapshot 固定取顶层 CSR sample 的 C-2 历史项；不得在本拍直接调用 get_mmu_csr_snapshot() 或使用 latest runtime CSR。
冻结 pending.accept_flush_event_seq=fire_visible_event_seq；它可以小于本拍刚观察到的 barrier event_seq。

调用 get_or_create_tlb_entry_by_req_with_snapshot()：
    得到 request_lookup_key、entry_anchor_key、live entry、lookup_result。

确认 request_lookup_key 与 pending CSR snapshot 一致。
不再要求 entry_anchor_key == request_lookup_key。

深拷贝 live entry 到 pending.entry_snapshot。
pending.pending_entry_generation = pending.entry_snapshot.entry_generation。
调用 populate_pending_request_derived(pending)。
pending.request_fire_sample_seq = dut_sample_seq。
调用 mark_waiting_uid_records_on_request_fire(pending)：它只查询
uid_waiting_by_vpn_s2xlate[{pending.vpn,pending.s2xlate}] 的候选，并以 pending C-2 CSR/key 确认后写入首个
request-fire marker；不把 pending.request_token 绑定给任何唯一 UID。

创建 response xaction。
调用 fill_dtlb_resp_from_entry(pending.entry_snapshot, response xaction)。
继续既有 due sample、pending_q 入队和 token accounting。
~~~

中文文字伪代码：A/B range hit 可以拥有同一 generation，却绝不能共享 token 或 pending object。entry_anchor_key
只记录实际复用的 raw entry。UID completion 不从 token 推断唯一 origin，而是在 response complete 时按 raw hit
多播；request fire 仅写 UID 取消边界 marker，不改变这条多播规则。此处 raw hit 以 response fire 的 DUT global C-2
CSR 判断，而不是 UID 的 issue-time CSR。若 logical UID A 的旧等待被 flush cancel，A 真实再次 issue 时建立新的 waiting epoch。若 fence 已删除
A 原先命中的 canonical entry，则重建得到新 generation；若该 entry 未被本次失效删除，则 generation 可以保持不变，
不能把 generation 当作 retry counter。
fill_dtlb_resp_from_entry 只读 raw snapshot，禁止读取 request_s1_resolved_ppn、request_gvpn、
request_s2_resolved_ppn 或 responder 侧推测的 PAddr。

## NAPOT、Fault、冲突与生命周期规则

### 1. normal-leaf NAPOT 检查

validate_normal_napot_payload() 在新 normal leaf entry 建立完成、注册 range index 前执行。

抽象功能描述：该 helper 只验证无 effective fault 的 normal leaf 是否具备 V2 可支持的 NAPOT raw 编码。
它不随机、不改写 PPN、不处理 fault passthrough；检查失败意味着 responder 无法生成可解释的 normal response。
尤其对 `LEGAL` stage，它验证 random payload plan 的 `apply_legal_napot_ppn_encoding()` 已在 split 前直接完成编码，
不能在这里补写或重新 split PPN。

~~~text
若 entry 存在 effective fault：直接返回，不执行 normal NAPOT 检查。
若 active S1 的 s1_pte_n 为 1：
    检查 s1_level==0、pteidx one-hot 与 selected sector 一致。
    若 s1_pte_mode==LEGAL：再检查联合 PPN low-4 编码为 4'b1000。
若 active S2 的 s2_pte_n 为 1：
    检查 s2_level==0。
    若 s2_pte_mode==LEGAL：再检查 s2_entry_ppn_raw[3:0]==4'b1000。
任一检查失败：uvm_fatal，打印 anchor key、stage、level、N、raw PPN 与 selected sector。
~~~

中文文字伪代码：此检查位于新 entry 注册之前，确保不把语义非法的 normal NAPOT 映射写进 range index。
无 effective fault 的 `LEGAL` stage 若 `PTE.N=1`，构造期必须已经将 final level 固定为 `0`，并直接写入
正确编码：S1 先将 canonical PPN `[3:0]` 置为 `4'b1000` 后 split，S2 直接将 `s2_entry_ppn_raw[3:0]` 置为
`4'b1000`。本 helper 只确认该结果，不得在 check 失败后修正 PPN。MIXED/EXCEPTION_BIASED 的无 fault
`PTE.N=1` 仍要求 level=0 和 S1 sector payload 自洽，但保留 raw PPN 低位；它不因非 canonical low-4 报错或被
强制合法化。range hit 继续按 DUT 可见 raw NAPOT shape 复用该 entry；只有模型 derived PPN 不可解释时保持 invalid。
fault raw 字段遵循用户确认的 passthrough 规则，因此不进入此 helper。

| stage/profile | PTE.N=1 时必须满足的条件 |
|---|---|
| S1 LEGAL | s1_level=0；s1_pteidx[] one-hot；OHToUInt(s1_pteidx)=s1_addr_low；{s1_entry_ppn_raw[0], s1_ppn_low[s1_addr_low]}=4'b1000。 |
| S1 MIXED/EXCEPTION_BIASED | s1_level=0；s1_pteidx[] one-hot；OHToUInt(s1_pteidx)=s1_addr_low；PPN low-4 保留 raw 值。 |
| S2 LEGAL | s2_level=0；s2_entry_ppn_raw[3:0]=4'b1000。 |
| S2 MIXED/EXCEPTION_BIASED | s2_level=0；PPN low-4 保留 raw 值。 |

S1 不得检查 s1_entry_ppn_raw[3:0]，它不包含完整 PPN 低四位。V2 只支持 64 KiB NAPOT；
本 plan 不增加任意 NAPOT size 或对应 plus 参数。

### 2. fault payload range hit

本 plan 选择允许 fault payload 以 raw response 进行 range hit：

~~~text
raw tag/level/N/sector/context 按 entry_matches_request_raw() 匹配；
raw fault、PPN、permission、PBMT 原样复用；
pending.request_derived_valid 保持 0；
不调用 resolve_s1_ppn()/resolve_s2_ppn()；
不建立 GPA/HPA、DCache owner 或 memory line。
~~~

明确的 A/B 复用规则如下：

~~~text
A request 首次 miss，建立 canonical fault entry E：
    E 保存 raw/effective fault 与原始 tag/level/N/sector/context/payload。

B request 到达：
    若 B 是 exact hit：保持 exact-hit 优先规则。
    若 B 是 exact miss，且与 E 的 s2xlate、ASID/global、VMID 一致，并通过 E 的 raw
    NAPOT/superpage/sector matcher：
        B 是 E 的 fault range hit。
        B 的 pending snapshot 逐字段复制 E 的 raw fault payload 与 entry_generation。
        pending.request_derived_valid=0；不得为 B 重新随机、resolve PPN 或建立 DCache state。
    若 context 或 raw shape 不匹配：
        E 不是 B 的 candidate，继续既有 range miss/build 流程。
~~~

fault 不是 range candidate 的降级条件。多个 raw-hit candidate 的选择仍只按 coverage rank：若 fault entry 是
唯一最大范围 candidate，就选择它并输出 `UVM_LOW`；不得因为它带 fault 而改选较小的 normal entry。最大 rank
并列时仍按通用 overlap 规则 `uvm_fatal`。

effective fault 的 raw N/level/PPN 继续遵循 random payload plan 的 passthrough 规则。range lookup 不强制
PTE.N -> level=0，也不强制 PPN low bits 为 4'b1000；它只按 DUT 可见 raw shape 匹配。

fault raw shape 的 index 规则必须与 PtwRespS2.hit 的实际选择一致：若 raw level 不为 0，则最终比较使用该 level
对应的 tag 前缀，raw N 不额外把 range 扩展为 NAPOT；只有 raw level 为 0 且 raw N 为 1 时，index 才使用
NAPOT 的低 4 bit mask。该规则只决定 lookup index/match 形状，不修正或覆盖 fault response 将要驱动的 raw N。

### 3. 重叠策略

~~~text
exact key hit：
    exact entry 优先，不进入 range fallback。

exact miss：
    0 个 range candidate：正常 miss/build。
    1 个 range candidate：range hit。
    多个不同 anchor candidate：
        先按实际 coverage rank 选择范围最大的 candidate。
        最大 rank 唯一：range hit，并以 UVM_LOW 记录重叠选择。
        最大 rank 并列：uvm_fatal。
~~~

这是一项测试框架确定性策略，不模拟 DUT cache 的 set/way multi-hit priority。exact hit 仍直接优先，不进入
range fallback。range overlap 时只比较 entry 的实际地址覆盖范围：`512 GiB > 1 GiB > 2 MiB > 64 KiB > 4 KiB`。
例如 1 GiB superpage 和其内部 64 KiB NAPOT 同时命中时，选择 1 GiB candidate，并输出 `UVM_LOW`。最大 rank
并列的 fatal 必须打印 request key、全部 candidate 的 anchor key、generation、S1/S2 tag、level、PTE.N、
ASID、VMID、fault、lookup shape 和 rank。

### 4. 删除与 UID 边界

~~~text
new entry build 成功
  -> insert_tlb_entry(anchor key, entry)
  -> register_tlb_range_index(anchor key, entry)

SFENCE/HFENCE/其它 explicit entry delete
  -> delete_live_tlb_entry_by_anchor_key(anchor key, reason)
     -> unregister_tlb_range_index(anchor key, entry)
     -> tlb_entry_by_key.delete(anchor key)

reset_all_tables
  -> 只清主表、live entry 与 range index；不管理 global sample、owner claim/claimed_once、release grant/closing 或 reset ack。
     testcase-start lifecycle initializer 单独初始化 lifecycle state；DUT runtime reset 不调用本分支，改由同一 owner re-arm
     流程按 shared reset epoch 清旧工作；ENABLED topology 由同一 owner re-arm 并保留 global sample、claimed_once 与 active claim，
     DISABLED/NO_OWNER 完成 FENCE/CSR/MONITOR ack，不启动或 re-arm responder
  -> tlb_anchor_keys_by_range_key.delete()
  -> tlb_entry_by_key.delete()

DUT runtime reset
  -> shared lifecycle reset coordinator 只发起 epoch reset 并等待 ack；dispatch adapter 独占清 raw/context/pending invalidate，
     再调用 delete_live_tlb_entry_by_anchor_key() 或等价统一 helper 清 canonical entry 与全部 range index；fence monitor/CSR monitor
     分别清自己的 producer/context 状态并回 FENCE/CSR ack；L2TLB monitor 消费匹配 reset-active transport sample 后回 MONITOR ack；
     response/adapter 是否需要 ack 仍由固定 topology 的 required mask 决定。正常 release 不从本 reset 描述推导，而是统一等待
     `release_grantable(owner, current_reset_epoch)` 的 final inactive、monitor settled、final recycle/mailbox EMPTY、所有 required ack、
     drain/intake 和 non-reset 条件；该 gate 为真后 parent 才发匹配 grant，owner 再消费 grant 完成 release；不调用 reset_all_tables，不重新 claim owner。
~~~

SFENCE/HFENCE 命中 allStage entry 后仍删除整个 combined entry 及其全部 range index key；不得只删 S1 或 S2
一部分 index。range lookup 不新增 pending cancel、flush owner 或 ready hold。

range index 的每个 bucket 使用编译期有限的 anchor 列表，不使用单值覆盖。注册、反注册和查询都必须保持
列表有界；同一 key 下的多个不同 anchor 交给 `find_tlb_range_hit_by_req()` 按 coverage rank 选择，不能在
index 写入时隐式丢弃较早 entry。

UID retry 复用 random payload plan 的 waiting-instance flow。每个 flush event 的 C0 观察和 C4 到期取消必须分开：

~~~text
record_l2tlb_flush_barrier(C0 event)
  -> 只记录 barrier 和关闭后续 ready，不取消 pending 或 UID waiting instance
  -> C0 已 fire 的 request 使用 fire_visible_event_seq 正常创建 token

apply_due_l2tlb_flush_barriers(C4 due barrier)
  -> 删除仍在 pending_q 的旧 P
  -> 调用 cancel_waiting_uid_records_for_flush(barrier)，只将 first_request_fire_sample <= barrier.anchor 的
     WAITING 旧 instance 置 CANCELED；marker=0 的等待保持 WAITING
  -> 已在 C4 前被 response raw-hit 多播完成的 UID 保持 COMPLETED
  -> 旧 P 或旧 CANCELED instance 均不得在 flush 后完成或回填

logical UID A 确实再次需要 TLB request
  -> register_uid_tlb_record_on_issue(A) 冻结新 context，递增 uid_tlb_wait_epoch，state=WAITING，pte_valid=0
  -> 新 request fire 分配新 request_token，并写 uid_tlb_first_request_fire_sample_seq
  -> 任一随后 raw-hit 的 response 通过 complete_waiting_uid_records_by_response() 回填 A 的新 epoch
~~~

UID complete 的 raw payload 来自 pending snapshot；若 UID record 保存 request-specific resolved PPN，必须用该
UID 自己的 VPN/GVPN 派生，且 mode/位宽解释必须使用本次 response fire 的 response-visible DUT global C-2 CSR；不能
使用 live entry 的 anchor-derived 值、另一笔 pending 的 derived 字段或 record 的 issue-time CSR。若该 response-visible
mode 与 raw snapshot 的派生前提不兼容，derived 字段保持 invalid 并记录 `UVM_LOW`，但 raw UID multicast 结果不回退。
已在 flush 边界前完成并由 DUT 安全接收的 response 保持既有“先 complete、后处理 flush”顺序，不回滚其
`COMPLETED` record；只有真实新的 TLB 等待才能对该 logical UID 建立新 epoch。

## Directed 验收与实现落点

Directed 场景至少覆盖：

1. S1 NAPOT：在无 effective fault 的 LEGAL S1 stage，`PTE.N=1` 时 final level 必须为 `0`，并在 sector split 前
   直接使 canonical PPN low-4=`4'b1000`；因此 `s1_entry_ppn_raw[0]=1`、
   `s1_ppn_low[s1_addr_low]=3'b000`。anchor low-4 为 0，low-4 为 3、7、15 的 request 全部 range hit；
   raw payload/generation 相同，token 独立。
   同时覆盖无 effective fault 的 LEGAL S2 stage：`PTE.N=1`、final level=0 时
   `s2_entry_ppn_raw[3:0]=4'b1000`，且不得由 validate helper 事后修正。
   另覆盖 MIXED/EXCEPTION_BIASED 的无 fault `PTE.N=1`：final level 仍为 0，但非 `4'b1000` raw low-4
   必须原样保留、不得 fatal；raw NAPOT range hit 仍可复用 payload，`request_derived_valid=0`。
2. S1 NAPOT 边界外：anchor+16 pages 必须 miss/build，generation 不同。
3. onlyStage2 S2 NAPOT：同 VMID 范围内 hit、范围外 miss、不同 VMID miss。
4. S1 sector：`noS2xlate` 下 valid sector hit、invalid sector miss，确认 valididx 未被 index 绕过；
   `onlyStage1/allStage` 的 normal level-0 则只允许 `Cat(s1_tag, s1_addr_low)` anchor 命中，邻近 sector 必须 miss，
   且 one-hot pteidx 只作为 payload consistency 检查而非 range 条件。
5. S1/S2 superpage：mode 支持的 level 1/2/3 的范围内/外边界。
6. allStage：S1 1 GiB + S2 2 MiB、S1 NAPOT + S2 superpage、S1 superpage + S2 NAPOT、S1 NAPOT + S2 4 KiB。
   另覆盖 S1 为 MIXED/EXCEPTION_BIASED 非 canonical 64 KiB NAPOT、S2 为 superpage 的组合：首次 build 必须
   固定 raw protocol `s2_tag`；范围内 B request 复用该 tag/raw payload/generation，`request_derived_valid=0`，
   不生成 request resolved PPN/GVPN，driver 仍只发送 raw snapshot。
7. fault range hit：A 首次建立 S1 fault、S2 GPF 或 S2 GAF entry 后，B 处于 A 的 raw NAPOT/superpage/sector
   覆盖范围内且为 exact miss 时，必须复用 A 的 raw fault payload/generation；pending 不产生 derived PPN/GVPN。
8. 重叠 candidate，唯一最大范围：先建立 64 KiB NAPOT entry，再对同一 1 GiB 区间内、但位于该 NAPOT 范围外
   的 VPN 建立 1 GiB superpage entry；随后请求 NAPOT 范围内的非-anchor page。两个 raw matcher 都命中，必须
   选择 1 GiB entry，并输出包含两个 anchor/generation/rank 的 `UVM_LOW`。
9. 重叠 candidate，最大范围并列：两个不同 anchor 以相同覆盖范围命中同一 exact-miss request 时必须 `uvm_fatal`；
   不得按 payload、创建时间或 associative-map 遍历顺序选择。
10. SFENCE/HFENCE 删除后：canonical table 与 index 同时移除；旧 pending 先由既有 flush lifecycle cancel，
   已观察到 `first_request_fire_sample <= barrier.anchor` 的 WAITING UID attempt 转为 `CANCELED`，未观察到 fire 的
   等待保持 `WAITING`。同一 logical UID 重发时必须得到新 token/new waiting epoch，
   且旧 CANCELED instance 不得回填；由于命中的 entry 已删除，重新建 entry 后 generation 必须不同。
11. 同 key 多 UID：两个 UID 同时 WAITING 时，一笔 raw-hit response 必须同时 complete 两个 UID；若二者位于同一
    NAPOT/superpage，raw payload/generation 相同、derived PPN/GVPN 各自按 UID VPN 计算。后续重复 token response
    没有 WAITING UID match 时正常完成且只记录 info。
12. 无同拍 request fire 的 flush：一个在 C0 前已经观察到 request fire、但 C0 没有新的 fire 且仍为 WAITING 的
    UID attempt 必须在 C4 取消；同一 logical UID 随后再次 issue 时必须以新 waiting epoch 建立 context，不能命中
    旧 CANCELED instance。另覆盖 C0 后 marker=0 的 WAITING UID 在 C4 保持 WAITING，不能被旧 barrier 误取消。
13. driver：A/B range hit 的 raw response wire 完全相同；仅 pending model-derived 字段允许不同，且不得进入
    interface drive 路径。
14. response-visible CSR：C0 改变 ASID 或 VMID 后，旧 token 在 C2/C3 external response fire。若该 response 在
    response sample 的 C-2 CSR 下不再 raw-hit UID A，则 token 仍正常 complete、A 保持 `WAITING`；若 A 已有
    `first_request_fire_sample <= barrier.anchor`，则在 C4 due 转为 `CANCELED`。若 raw tag 与新的 context 仍匹配
    （例如 S1 global），A 才 `COMPLETED`。任何已 fire response
    缺少其 DUT global C-2 history 必须 `uvm_fatal`，C4 fire 仍严格禁止。
15. 单 owner reset re-arm：在 ENABLED topology 下，同一个 responder owner 在 DUT global sample 大于 100 时经历一次 reset，
    清理旧 token、UID、barrier、live entry/index 与 raw pending 后重新等待 CSR history warm-up；它不得重新 claim、启动第二个
    owner 或使用局部计数重锚 C-2/C4。DISABLED/NO_OWNER 完成 FENCE/CSR/MONITOR reset ack，不 re-arm responder。negedge service 只读取
    `peek_current_dut_global_sample()`。

基础 debug 统计记录 exact hit、range hit、miss build、fault range hit、candidate count、request key、
anchor key、generation、s2xlate、lookup shape、candidate coverage rank、重叠选择次数、被选 anchor、
最大-rank 并列次数、request_derived_valid、request_token、uid_tlb_wait_epoch、uid_tlb_wait_state、
uid_tlb_first_request_fire_sample_seq、每 response 的 UID match 数
及 complete/cancel 原因。它们只服务激励可观测性，不作为
coverage/checker/pass-fail 条件。

| 文件 | 修改职责 |
|---|---|
| mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv | 新增 range kind、lookup result enum、coverage rank enum、packed range index key，以及 `MEMBLOCK_TLB_RANGE_CANDIDATE_MAX` 有界配置。 |
| mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_tlb_entry.sv | 增加 range_index_keys；明确 live resolved PPN 只作 anchor debug；删除/停止使用 responder 侧 s1_paddr/s2_paddr 持久字段。 |
| mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv | 增加 secondary index、exact/range lookup、raw matcher、注册/反注册和统一 entry delete helper；维护 UID waiting shape index、request-fire marker 与 complete/cancel 清理。 |
| mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv | 扩展 pending request/anchor/result/derived 字段，在 capture 中冻结与 DTLB filter 对齐的 CSR snapshot、flush version、generation 与 request-fire sample，保证 driver 只读 raw snapshot。 |
| random payload plan | coding 前同步其同-key 唯一复用与 live resolved/PAddr 描述，改为本 plan 的 exact + range 规则。 |
| SFENCE/HFENCE plan | coding 前同步 entry delete 前反注册 range index 的调用边界，不改变其 stage-aware matcher；固定 topology 下不实现 owner handoff 或 topology transition。 |

实施顺序：先完成 random payload plan 的 S1/S2 raw 字段迁移、UID waiting/multicast-complete/cancel 和 copy 规则，
再实现 range index 与 entry lifecycle，随后修改 pending/UID copy，最后接入 SFENCE/HFENCE 删除路径和 directed testcase。完成 coding 后按 V2 L2TLB
responder 规则执行静态检查、远端 compile 和 smoke。

本 plan 不实现 RM/checker/scoreboard 或功能覆盖率；后续组件可使用 lookup_result、request/anchor key、
generation、lookup shape 和 request_derived_valid 作为观测字段。
