# V2 L2TLB Response 随机 Payload 实现 Review

关联执行 plan：`AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2tlb_response_random_payload_plan_20260729.md`

当前状态：代码、文档同步、VCS compile 与定向 real-dispatch smoke 已完成；独立末轮 review 已 `FINAL PASS`，关联 plan 可归档。

## 1. 术语与抽象功能说明

| 英文术语 | 当前含义 | 代码落点 | 示例 |
|---|---|---|---|
| `live entry` | 当前 lookup 表中可被后续 request 命中的一份长期 response payload | `tlb_entry_by_key`、`memblock_tlb_entry` | 同一 key 第二次命中时复用第一次随机结果 |
| `pending snapshot` | request fire 时从 live entry 复制出的独立快照 | `memblock_l2tlb_pending_req.entry_snapshot` | C0 建表、C1 fence 删除，C0 已接受 request 仍使用快照 |
| `UID record` | 发射 UID 的 TLB 等待和完成历史 | `uid_tlb_record_by_uid`、`memblock_uid_tlb_record` | 一个 response 可同时完成多个 WAITING UID |
| `raw fault` | 每个 fault 候选独立随机得到的原始结果 | `fault_raw_*` | S1 PF 和 S2 GPF 可同时 raw=1，但不同时驱动 DUT |
| `effective fault` | 按 `s2xlate`、权重和优先级收敛后的唯一 DUT fault | `fault_effective_*` | `s2_gaf` 胜出时只驱动 S2 GAF |
| `PTE profile` | 控制 raw PTE 如何保留或做 LEGAL 合法化的 testcase 模式 | `LEGAL/MIXED/EXCEPTION_BIASED` | entry 建立后即使 plus 改变，旧 entry 仍使用旧 profile |
| `profile provenance` | payload 创建时冻结的 profile 来源记录 | `s1_pte_mode_at_build/s2_pte_mode_at_build` | pending、UID 和 NAPOT 检查逐字段复制 |
| `sector split` | V2 S1 PPN 被拆成高位 `entry_ppn_raw` 和 8 个低位 sector 字段 | `s1_entry_ppn_raw`、`s1_ppn_low[8]` | selected sector 由 `s1_addr_low` 选取 |
| `one-hot pteidx` | 8 个 Bool 中只有一个为 1 的 sector 选择编码 | `s1_pteidx[8]` | `addr_low=3` 时只有 `pteidx[3]=1` |
| `canonical PPN` | 可按 PTE.N/NAPOT 规则解释的完整页号 | builder 的临时 `canonical_ppn` | LEGAL NAPOT 的低四位固定为 `1000` |
| `request-derived PPN/GVPN` | 针对本次 request VPN 从冻结 raw payload 派生的调试/后续消费值 | pending/UID `request_*` 字段 | 同一 superpage 的两个 VPN 可有不同 derived PPN |
| `request token` | 每次 `valid && ready` fire 的独立响应实例编号 | `pending_q`、`request_token` | token 不等于唯一 UID |
| `raw matcher` | 复现 DUT response 的 tag/level/sector/ASID/VMID 内容匹配 | `entry_matches_request_raw()` | 不按 exact lookup key 或 UID 年龄猜测命中 |
| `response-visible C-2 CSR` | DUT response filter 在该 sample 实际看到的 CSR 历史快照 | `get_request_csr_snapshot()` | CSR 改变后旧 response 可能完成 token 但不回填 UID |
| `entry generation` | live entry 每次新建时的单调时代编号 | `entry_generation`、`next_tlb_entry_generation` | fence 删除后同 key 重建必须得到更大编号 |
| `lifecycle owner` | 唯一管理 request token、pending、response、flush/reset 和 release 的 responder sequence | `memblock_l2tlb_base_sequence` | 本专项只替换 payload，不新增 owner |

抽象功能说明：本专项把 V2 L2TLB responder 的 response 数据从旧的共享 PTE/PPN 表示改成独立的 S1/S2
不可变 payload。lookup miss 负责一次性生成并验证 payload，request fire 复制 snapshot，response drive 只消费
snapshot；UID 回填和 matcher 只使用已冻结字段与 DUT 对齐的 CSR history，不改变原有 token、latency、flush
和 owner 生命周期。

最小流程示例：C0 request A miss 时创建 generation 1 的 entry；C1 request A fire 时复制 generation 1 到
pending；C2 fence 删除 live table 不会修改 pending；response 完成时仍从 generation 1 的 snapshot 驱动，且
按 response sample 的 C-2 CSR 对所有 WAITING UID 做 raw match。

## 2. Review 范围与结论

本轮检查了以下源码和配置：

- `mem_ut/ver/ut/memblock/env/plus.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/tlb_map_builder.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_tlb_entry.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv`
- `seq/plus_cfg/default.cfg` 及 `tc_dispatch_real*.cfg` preset

实现已覆盖 plan 的主要功能：独立 S1/S2 payload、四类 raw/effective fault、独立 level/PPN/PBMT、S1
sector one-hot、profile provenance、request-specific derived 字段、V2 宽度保护和 response drive 链路。
未改变 L2TLB 的方向边界：request 仍是 DTLB -> L2TLB agent，response 仍是 L2TLB agent -> DTLB。

## 3. 参数入口与配置检查

### 3.1 `plus` 与 preset 参数

源码位置：`mem_ut/ver/ut/memblock/env/plus.sv`，`plus` 字段定义和 `load()`。

抽象功能描述：plus 层声明并读取本专项的运行期随机配置；它只提供参数，不创建 TLB entry，也不参与
request/response 生命周期。

修改前逻辑：旧框架用 `MEMBLOCK_TLB_PTE_*`、单一 `MEMBLOCK_TLB_LEVEL_*` 和单一 `MEMBLOCK_TLB_PTE_MODE`
控制一套共享 PTE/level。

修改后逻辑：参数按 S1/S2、fault、level、PTE field、PBMT 分组；S2 不声明 V；默认配置保持 level 0、fault
关闭和 LEGAL profile 的稳定 smoke 行为。

```systemverilog
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_L2TLB_S1_PF_1_WT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_L2TLB_S2_GAF_1_WT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_L2TLB_S1_PTE_MODE, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_L2TLB_S2_PTE_MODE, int, 0)
```
中文伪代码：plus 声明每个可配置字段并给出默认值；`load()` 从 plusarg 读取同名值；没有 plusarg 时保留默认
稳定配置。这里不直接随机，也不写 entry/pending 状态。

### 3.2 `seq_csr_common::check_l2tlb_payload_weight_cfg()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv`。

抽象功能描述：该 helper 在 responder 开放 ready 前验证随机参数的范围和结构组合；它只报告不可构造配置，
不修改 payload 或 lifecycle。

```systemverilog
if (l2tlb_s1_pbmt_wt[0] + l2tlb_s1_pbmt_wt[1] + l2tlb_s1_pbmt_wt[2] == 0 ||
    l2tlb_s2_pbmt_wt[0] + l2tlb_s2_pbmt_wt[1] + l2tlb_s2_pbmt_wt[2] == 0)
    `uvm_fatal("SEQ_CSR_CFG", "S1/S2 PBMT weights must not be all zero");
if (l2tlb_level_weight_en && main_mem_ranges_en)
    `uvm_fatal("SEQ_CSR_CFG", "LEVEL_WEIGHT_EN requires MAIN_MEM_RANGES_EN=0");
```
中文伪代码：逐项检查 weight 是否在 0 到 100；检查 PTE mode 是否为三个已定义枚举；检查每个 PBMT 分布至少
有一个非零候选；检查随机 level 与严格物理地址窗口没有同时启用。任一结构错误立即 fatal，避免 ready 开放后
才产生不可编码 response。

修改原因：旧参数检查只覆盖共享 PTE/level，无法发现 V2 分阶段参数冲突。数组现在按真实候选数量保存，S1/S2
fault 各有两个候选，避免重复检查虚构的第三、第四项。

## 4. Builder 构造 flow

### 4.1 `common_data_transaction::build_tlb_entry_for_key_with_csr()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`。

抽象功能描述：这是 lookup miss 的公共建表入口。它接收已经冻结的 lookup key 和 DTLB-side CSR snapshot，调用
builder core 得到 detached payload，成功后分配 entry generation；它不管理 pending、token 或 owner。

```systemverilog
entry = builder.build_payload_for_key_with_csr(key, csr_snapshot);
if (entry == null) `uvm_fatal("COMMON_DATA", "builder returned null payload entry");
entry.lookup_key = key;
entry.s2xlate = key.s2xlate;
entry.entry_generation = allocate_tlb_entry_generation();
return entry;
```
中文伪代码：入口先检查 CSR snapshot 和 builder 是否有效；builder 完成所有 stage payload 与 BUILD 校验后返回；
入口再写 key/阶段字段并分配单调 generation；调用方随后才把对象插入 live map。若 builder 失败，不插入半成品。

### 4.2 `tlb_map_builder::freeze_stage_context()` 与 `freeze_pte_profile_modes()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/tlb_map_builder.sv`。

抽象功能描述：两个 helper 在 payload 随机前确定 active stage、translation mode/root、ASID/VMID 和 PTE profile
provenance。inactive stage 保持 reset 默认值，active Bare 或无法编码的 VMID 立即失败。

```systemverilog
entry.s1_pte_mode_at_build = seq_csr_common::get_l2tlb_pte_mode(1'b1);
entry.s2_pte_mode_at_build = seq_csr_common::get_l2tlb_pte_mode(1'b0);
```
中文伪代码：根据 `s2xlate` 选择 S1/S2 的 CSR 来源并冻结 mode/root；对 active stage 验证 paged mode；对需要
S2 的请求检查 VMID 高两位；然后各 active stage 读取一次 PTE profile 并写入 entry。之后所有 profile consumer
只从 entry 字段读取，pending/UID copy 不重新取 plus。

### 4.3 `fill_pte_fields()`、`fill_faults()`、`finalize_pte_fields()`

源码位置：`tlb_map_builder.sv` 的同名函数及 `build_payload_for_key_with_csr()`。

抽象功能描述：这组 helper 完成 payload 的分阶段构造。`fill_pte_fields()` 产生 raw PTE；`fill_faults()` 保存四个
raw fault 并按 `s2xlate` 选出一个 effective fault；`finalize_pte_fields()` 只在无 fault 的 LEGAL stage 做确定性
合法化。它们不删除 entry，也不影响 responder 时序。

```systemverilog
fill_pte_fields(1'b1, 1'b1, entry);
entry.s1_level = choose_level(1'b1, 1'b1, entry.s1_translation_mode_at_build);
fill_faults(key.s2xlate, entry);
finalize_pte_fields(1'b1, entry);
```
中文伪代码：先对 active stage 的每个 PTE bit 按独立权重采样，并记录 raw profile；再为每个 fault 候选采样 raw
值，屏蔽不属于当前 `s2xlate` 的候选，按权重和固定优先级保留唯一 effective fault。若没有 fault，LEGAL
stage 才强制 leaf 所需的 R/A/D/V；若有 fault，则保留 raw PTE 和候选 level，不执行 normal fixup。该顺序确保
fault response 不被正常 leaf 合法化覆盖。

### 4.4 `choose_level()`、`make_canonical_ppn()` 与 `encode_s2_entry_ppn()`

源码位置：`tlb_map_builder.sv`。

抽象功能描述：这些 helper 分别选择一次合法 level、形成 deterministic raw canonical PPN，并把 S2 PPN 编码到
V2 38-bit wire。它们不建立 DCache 地址 owner；不能通过截断或 retry 隐藏不可编码结果。

```systemverilog
if (!active || !seq_csr_common::get_l2tlb_level_weight_en()) return 2'd0;
if (!std::randomize(chosen) with { chosen dist {0 := w0, 1 := w1, 2 := w2, 3 := w3}; })
    `uvm_fatal("L2TLB_PAYLOAD_RANDOM", "failed to randomize level");
if (|raw_ppn[43:38]) `uvm_fatal("L2TLB_PAYLOAD_S2_PPN_WIDTH", ...);
```
中文伪代码：disabled 或 inactive stage 固定 level 0；enabled 时按冻结 mode 过滤候选并单次 `dist`。PPN
在严格 range 或 sparse 模式下确定生成，再按 level 对齐。S2 完整 PPN 的高 6 位只要有一位非零就 fatal，
不截断成错误 response。

### 4.5 S1 sector 与 NAPOT helper

源码位置：`tlb_map_builder::build_s1_sector_payload()`、`apply_legal_napot_ppn_encoding()`、
`memblock_tlb_entry::validate_s1_sector_payload_consistency()`。

抽象功能描述：先形成完整 canonical PPN，再拆成 V2 S1 sector wire；校验 one-hot、valididx 和低位一致性。
LEGAL NAPOT 只在无 fault、最终 level 0 时固定低四位 `1000`；MIXED/EXCEPTION_BIASED 的非 canonical raw
payload 保留但 derived PPN 标记无效。

```systemverilog
entry.s1_entry_ppn_raw = canonical_ppn[43:3];
entry.s1_ppn_low[idx] = canonical_ppn[2:0];
entry.s1_pteidx[entry.s1_addr_low] = 1'b1;
```
中文伪代码：对每个 sector 先清零；superpage/NAPOT 或 selected sector 才置 valid，并把同一 canonical
低三位写入有效 sector；只把 `addr_low` 对应的 pteidx 置 1。BUILD、COPY、DRIVE 都重新校验，任一 drift/multi-hot
立即 fatal，而不是等 matcher 失败。

## 5. 数据模型、快照与 UID 生命周期

### 5.1 `memblock_tlb_entry::reset()`、`copy_from()`

源码位置：`memblock_tlb_entry.sv`。

抽象功能描述：`reset()` 建立所有 S1/S2、provenance 和 fault 字段的默认状态；`copy_from()` 在 pending 或 UID
回填时制作不可变逐字段副本，并在复制前后检查 source/destination 一致性。

```systemverilog
source.check_inactive_stage_defaults("COPY_SOURCE");
source.validate_s1_sector_payload_consistency("COPY_SOURCE");
s1_pte_mode_at_build = source.s1_pte_mode_at_build;
s2_pte_mode_at_build = source.s2_pte_mode_at_build;
```
中文伪代码：复制前先验证 source 没有 inactive payload 或 sector 破坏；再复制 stage 字段、profile provenance、
fault、PPN、权限、PBMT、generation 和 sector arrays；复制后再次验证 destination。任何 null、缺字段、one-hot
错误或 provenance drift 都 fatal。

### 5.2 UID WAITING index 与 release 检查

源码位置：`common_data_transaction.sv` 的 `add_waiting_uid_to_index()`、`remove_waiting_uid_from_index()`、
`mark_waiting_uid_records_on_request_fire()`、`check_l2tlb_release_uid_waiting()`。

抽象功能描述：这些 helper 维护 WAITING UID 的有限候选索引，并在真实 request fire、response complete、flush/reset
和 owner release 时保持状态闭环。索引只加速候选筛选，不把 token 绑定到 UID。

```systemverilog
shape_key = make_uid_tlb_wait_shape_key(record.vpn, record.s2xlate);
uid_waiting_by_vpn_s2xlate[shape_key].push_back(uid);
```
中文伪代码：UID issue 建立 WAITING 后按 VPN/s2xlate 放入 bounded bucket；真实 fire 只扫描该 bucket，并用 C-2
CSR 重建 canonical key；response 逐个 raw-hit 完成 UID 并移除索引；cancel 同样先标记 CANCELED 再移除；release
前全表统计仍为 WAITING 的 record，直接 fatal，避免无限等待。

## 6. Request-specific derived 与 raw matcher

### 6.1 `derive_tlb_request_fields()` / `populate_uid_record_derived()`

源码位置：`common_data_transaction.sv`。

抽象功能描述：从本次 request 的冻结 response snapshot 和 response-visible C-2 CSR 派生本 request 的 S1/S2 PPN/GVPN。
它不改 entry、不读取后续 current CSR，也不把 fault payload 当作正常地址；不可解析的 non-LEGAL NAPOT 只留下 invalid
derived 标志。

```systemverilog
if (entry.has_effective_fault()) return;
...
record.request_derived_valid = derived_valid;
record.request_s1_resolved_ppn = derived_s1_ppn;
```
中文伪代码：先校验 stage shape、mode provenance、leaf 条件和 VPN/GVPN 位宽；fault 或不满足 normal leaf 条件时
保持输出 invalid；否则结合 selected sector、level、PTE.N/NAPOT 和本 request VPN/GVPN 派生结果。UID helper
只在 raw matcher 命中后复制 payload，再写入该 UID 的 request-derived 字段。

### 6.2 `entry_matches_request_raw()`

源码位置：`common_data_transaction.sv`。

抽象功能描述：复现 V2 `PtwRespS2.hit()`/S2 hit 的内容匹配，按 `s2xlate` 选择 S1/S2 anchor、level、sector、
ASID/VMID/global 规则；它是纯函数，不修改 UID、table 或 queue。

```systemverilog
return ((entry.s1_asid == response_asid) || entry.s1_pte_g) &&
       s1_sector_response_matches_request(entry, request_vpn);
```
中文伪代码：noS2xlate 使用 S1 sector matcher；onlyStage1 使用 S1 anchor、ASID/VMID 和 level；onlyStage2
使用 S2 tag/VMID；allStage 使用 S1 anchor 与两 stage 的最小有效 level/NAPOT 形状。response-visible C-2 CSR
只提供比较上下文，不覆盖 entry 的冻结 profile/root。匹配成功后才允许 UID multicast。

## 7. Pending 与 response driver

### 7.1 `capture_fired_request()`

源码位置：`memblock_l2tlb_base_sequence.sv`。

抽象功能描述：在真实 request fire 边界建立独立 token，复制命中的 live entry，并保存 generation 和 request-specific
derived 字段。它不在 pending 队列中重新随机，也不依赖 live table 的后续变化。

```systemverilog
pending.entry_snapshot.copy_from(live_entry);
pending.pending_entry_generation = pending.entry_snapshot.entry_generation;
data.derive_tlb_request_fields(pending.entry_snapshot, {14'b0, pending.vpn}, ...);
```
中文伪代码：检查 live entry 存在且 generation 非零；复制 snapshot；确认 pending generation 与 live generation
一致；以该 request 的 VPN 和 C-2 CSR 派生 PPN/GVPN；最后创建 response transaction 并加入既有 pending flow。

### 7.2 `fill_dtlb_resp_from_entry()` 与 `clear_l2tlb_xaction()`

源码位置：`memblock_l2tlb_base_sequence.sv`。

抽象功能描述：clear helper 每拍把 transaction 清成 inactive 默认；fill helper 将 pending snapshot 的完整 V2
S1/S2 payload 写入 xaction，并在 drive 前执行 PMA/fault、VMID、inactive stage 和 sector 校验。

```systemverilog
resp.io_ptw_resp_bits_s1_entry_perm_g = entry.s1_pte_g;
resp.io_ptw_resp_bits_s2_entry_perm_g = entry.s2_pte_g;
resp.io_ptw_resp_bits_s1_pf = entry.fault_effective_s1_pf;
resp.io_ptw_resp_bits_s2_gaf = entry.fault_effective_s2_gaf;
```
中文伪代码：先清空全部 response/request 字段；drive 时拒绝 pmaAF 与 modeled fault 混合、拒绝不可编码 VMID，
检查 inactive stage 和 S1 sector；随后逐字段驱动 S1/S2 tag、权限、level、PPN、PBMT、fault。S2 不驱动 V，S1
`pteidx` 直接使用 one-hot Bool；不从 current CSR 或旧共享字段补值。

## 8. 修改类型与旧逻辑对比

### 8.1 仅字段/参数适配

1. 旧共享 `pte_*`、`ppn`、`level`、`pbmt` 拆成 S1/S2 字段；旧 `tlbPF/tlbAF/tlbGPF` 映射为四类
   `fault_effective_*`，新增 S2 GAF；V2 不存在的 S2 V 不创建。
2. S1 `pteidx[8]` 从数值数组改为 Bool one-hot，`ppn_low` 从 sector index 伪造值改为 canonical PPN 低位。
3. 新 plus 参数经 `plus.sv -> seq_csr_common -> getter -> builder`，default/preset 同步；S2 fault、level、PTE、
   PBMT 分开配置。
4. pending 增加 `pending_entry_generation` 和 request-derived debug 字段；entry/UID 增加 stage active、mode/root、
   profile provenance 和 raw/effective fault 字段。

### 8.2 新增或改变的功能逻辑

| 原有逻辑 | 新逻辑 | 原因 |
|---|---|---|
| `build_tlb_entry_for_req()` 先写共享地址/PTE，再统一 fixup | wrapper 只转调新的 `build_payload_for_key_with_csr()`；新 builder 按 raw PTE/profile -> candidate level -> effective fault -> LEGAL fixup/final level -> PPN/sector/PBMT 构造 | 避免旧共享 helper 抢先写 entry，并防止 fault payload 被 normal fixup 覆盖 |
| 同 key 只保存一套随机 payload，S1/S2 共享字段 | 每个 stage 独立随机并冻结；hit/snapshot/driver 逐字段复用 | V2 response 的 stage、宽度和语义不同 |
| matcher 主要按 framework exact key/共享 level 比较 | 按 V2 raw response anchor、level、sector、ASID/VMID/global 规则匹配；allStage 使用 S1 anchor | 复现 DUT filter 的内容命中，允许一个 response multicast 多个 UID |
| live entry 的 resolved PPN 可被后续 request 复用 | 每个 pending/UID 按自己的 VPN/GVPN 派生；fault/noncanonical NAPOT 保持 invalid | superpage/NAPOT 的 derived 值具有 request-specific 语义，不能用首次 anchor 冒充 |
| UID 回填按简单遍历或 token 归属 | WAITING UID 建立 bounded shape index；request fire 和 response complete 都用状态/CSR/raw matcher；一个 token 不绑定一个 UID | 支持多 outstanding 和 response 广播，避免误回填/重复回填 |
| release 前对所有 WAITING 一律 fatal | admission cutoff 后只将从未观察到 request fire 的 candidate 显式标记为 CANCELED；marker 非零 WAITING 仍 fatal | Bare/DTLB hit 不会产生 L2TLB response，不能把合法无请求候选误报为 drain 缺失 |
| `idle_count` 到阈值后置 `stopping=1` 并走 release | 阈值只打印一次 no-progress warning，计数饱和；仅 `global_stop_requested` 可以置 `stopping` | active dispatch 在暂时无 request 时仍必须保有 responder/ready，不能自行关闭 admission |
| 超过 response wire 宽度时隐式截断 | S2 PPN、VMID 在 build/drive fail-fast | 保证 DUT 可观察字段不被截断成错误语义 |

这些逻辑变化不改变 request fire 记账、latency/reorder、driver hold、flush/reset 时序或唯一 lifecycle owner。

## 9. Plan 对齐与执行中补充

计划与实现一致的部分：S1/S2 独立 payload、fault raw/effective、PTE/NAPOT/PBMT、sector 校验、generation、
request-derived 字段、V2 response drive 和旧 builder 收敛均已实现。

执行中补充项已在 plan 的 `IMPLEMENTATION_DELTA` 记录：

- 冻结 `s1_pte_mode_at_build/s2_pte_mode_at_build` 并贯穿 copy/UID/consumer；
- 固定 raw/effective/fixup/PPN 构造顺序；
- 统一 S2 PPN 与 VMID fail-fast；
- fault weight 容器按真实两个候选收敛；
- admission cutoff 后对 marker=0 的无真实 request-fire UID candidate 显式收敛，marker 非零仍严格 fatal；
- `MEMBLOCK_L2TLB_IDLE_STOP_CYCLE` 收敛为诊断阈值，不再触发 owner stop/release。

当前未发现 token owner、pending queue、response latency 或 L2TLB 方向被改成下游 L2Cache/PTW 模型的情况。

## 10. 验证记录

已通过：

```text
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
make eda_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq \
  mode=base_fun cfg=tc_dispatch_real_smoke \
  plus_arg='+MEMBLOCK_L2TLB_IDLE_STOP_CYCLE=1'
```

结果：VCS Q-2020.03-SP2 elaboration 报告 `0 error(s), 0 warning(s)`。定向 real-dispatch smoke 在 idle
阈值为 1 时只输出一次 `no-progress diagnostic`，之后仍完成 LoadUnit issue、DCache writeback、ROB commit、LQ deq 和
release-time marker=0 candidate 收敛；日志含 `TEST CASE PASSED`，且 `UVM_ERROR=0`、`UVM_FATAL=0`。

已执行但未通过：

```text
make eda_run tc=tc_sanity mode=base_fun
```

该 testcase 没有 dispatch/L2TLB request topology；在 200060.300ns 因
`dcache_mem__access_base_sequence` 报 `L2 flush request was withdrawn before DRAIN completed`，最终
`UVM_FATAL=1`。故不能把该基础 smoke 记为通过。该路径不进入本专项的 builder、request fire、pending、response
或 UID release 逻辑；问题来自已提交的 DCache L2 flush state machine，保留为本专项之外的回归风险。

## 11. Review 结论

### 11.1 真实 smoke 暴露的 UID release 问题及修正

针对真实 dispatch smoke 的日志检查发现：

```text
LoadUnit issue fire -> DCache writeback -> lqDeq
无 DTLB->L2TLB request fire / L2TLB response accept
issue-time UID record 仍为 WAITING
```

根因是 issue-time TLB record 同时承担了“可能需要 L2TLB response 的候选”和“已进入 L2TLB request 生命周期”两种
语义。Bare 或 DTLB hit 时前者存在、后者不存在；release gate 若直接对所有 `WAITING` fatal，就会把合法无请求路径误报为
L2TLB drain failure。

本轮新增 `common_data_transaction::cancel_unbound_uid_tlb_records_at_release()`。其抽象职责是：在 owner 已通过真实
transport sample 关闭 admission，且 pending/driving/barrier 已清空后，收敛那些从未观察到真实 request fire 的 issue-time
候选。实现逐个检查 `uid_tlb_first_request_fire_sample_seq==0`，显式标记 `CANCELED`、移除 bounded index 并打印
UVM_LOW 诊断；已观察 request fire 的 marker 非零 WAITING 不会被该 helper 清理，仍由 response 或 C4/reset 规则完成，
release 前残留时继续 `uvm_fatal`。

中文文字伪代码：

```text
若 admission 尚未 cutoff：不清理 marker=0 candidate，允许未来 request fire 绑定。
若 cutoff 已确认且 token/barrier 全空：
    marker=0 -> 记录“无真实 L2TLB request”并转 CANCELED；
    marker!=0 且 WAITING -> 保留并 fatal，不能伪造 response 或 token 完成。
```

该修正只改变无 request 候选的 release-time bookkeeping；不改变 V2 response payload、token 分配、pending queue、latency、
flush C4 边界或 L2TLB agent 的 DTLB/L2TLB 方向。

### 11.2 idle watchdog 不能提前结束唯一 responder owner

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv`，
`send_l2tlb_cycle()` 的 idle 计数分支。

抽象功能描述：该分支只在 L2TLB responder 已经开放 ready、当前没有任何 token/response/flush/CSR 阻塞时记录长期
无进展。它给测试人员提供卡住线索，不决定生命周期归属，也不负责关闭 request admission 或释放 owner。

修改前逻辑：计数达到 `MEMBLOCK_L2TLB_IDLE_STOP_CYCLE` 后直接置 `stopping=1`。后续代码把 ready 拉低并开始
release，即使 dispatch 主流程尚未产生下一笔 DTLB request。

修改后逻辑：计数仅在从小于阈值变为等于阈值时输出一次 `uvm_warning`，之后饱和。出现 progress、lifecycle block、
outstanding 或 global stop 时清零；只有公共 `global_stop_requested` 可置 `stopping=1` 并进入原有 release flow。

中文文字伪代码：

```text
若当前属于可诊断的连续空闲：
    若计数未到阈值：递增；
    若本拍刚达到阈值：打印一次 warning；不改变 stopping、ready、pending 或 owner。
若出现任何实际进展、阻塞、未完成 token 或 global stop：清零诊断计数。
global stop 到来时：由既有分支关闭 admission 并排空 token；idle warning 自身绝不触发该分支。
```

定向 smoke 用 `MEMBLOCK_L2TLB_IDLE_STOP_CYCLE=1` 验证：在 issue 前确实出现一次 warning，但 owner 保持运行，
后续 LSU request 和完整 testcase 仍成功结束，说明该诊断不再误当正常 exit。

## 12. 独立终审与最终结论

独立只读 Codex reviewer 已逐项检查当前 worktree 的 plan、S1/S2 payload、fault/profile、sector/raw matcher、
builder wrapper、UID request-fire/C4/release、idle watchdog、参数入口、interface/profile/flow 和 Scala 语义。其结论为
`FINAL PASS`，未发现实现 blocker。原生 subagent 并发额度未释放时，终审改由独立 one-shot Codex reviewer 执行；原始
本地审查输出保存在未纳入提交的 `.humanize/skill/2026-08-10_01-01-22-117232-90435fa2/output.md`。

本 agent 随后复核了该结论、当前 `git diff`、V2 `PtwSectorResp/PtwRespS2` matcher 语义和所有专项文档落点，确认：

- payload plan 未把尚未执行的 stage-aware SFENCE matcher、range/NAPOT index 或第二个 lifecycle owner 混入本次实现；
- L2TLB 方向仍为 DTLB -> L2TLB request 与 L2TLB -> DTLB response；
- 基础 `tc_sanity` 的 DCache L2 flush 失败仍被明确保留为本专项外风险，没有被误记为通过；
- `git diff --check` 通过，未发现格式 blocker。

最终结论：本专项无已知 blocker，关联 plan 可以从 `undo` 归档到 `do` 并独立本地提交。
