# MemBlock L2TLB 大小页重叠分配保护实现 Review

状态：REVIEW_PASS，无剩余必须修改项

Review 日期：2026-10-21

基线提交：`f32aa0e04a3264cf4974e112d1fbb7f924c48824`

对应 Plan：
`AI_DOC/plan/test_framework/plan/do/memblock_l2tlb_overlapping_page_allocation_guard_plan_20261020.md`

## 1. 术语与抽象功能说明

### 1.1 术语表

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| canonical live entry | 当前会参与 DUT L2TLB raw-hit 的权威表项 | `tlb_entry_by_key` | 一个 2MB entry 发布后可被范围内其他 VPN 复用 |
| coverage shape | DUT raw-hit 实际覆盖范围，不等同于单一 level | `memblock_tlb_range_coverage_rank_e` | 4KB、64KB NAPOT、2MB、1GB、512GB |
| parent occupancy | 某候选区域已被其他 live owner 占用的索引事实 | `tlb_parent_occupancy_count` | 1GB 候选发现区域内已有 4KB owner 后被屏蔽 |
| owner shape | 已有 owner 自己的真实覆盖范围 | `owner_shape` | 区分相邻 2MB owner 与真正覆盖当前地址的 1GB owner |
| parent region | owner 为防止后续反向覆盖而登记的候选区域 | `parent_region` | 4KB owner 登记到其所在 64KB/2MB/1GB/512GB 区域 |
| context scope | ASID/global/VMID 隔离方式 | `memblock_tlb_context_scope_e` | global 候选查询 `ANY_ASID`，S2-only 只按 VMID |
| prepared payload | 已冻结上下文/PTE/fault，但尚未确定 shape 和 PPN 的对象 | `MEMBLOCK_TLB_PAYLOAD_PREPARED` | parent mask 查询只接受该阶段 |
| finalized payload | shape、tag、PPN、sector、PBMT 均已物化，可分配 generation 并发布 | `MEMBLOCK_TLB_PAYLOAD_FINALIZED` | `publish_live_tlb_entry()` 的必要条件 |
| atomic publication | 预检查完成后在同一零时间函数内连续发布主表和索引 | `publish_live_tlb_entry()` | 不暴露“主表已存在、索引未登记”的半状态 |
| frozen provenance | UID issue 时冻结的 response token/sample/generation/payload | `issue_hit_payload` 等字段 | RM 不因后续更宽历史 entry 改写该 UID 的翻译 |
| translation pending | L2TLB request 已 fire，但 response 尚未完成 | `uid_tlb_translation_pending_for_rm()` | RM commit poll 暂缓比较，而不是误报 query miss |
| global fallback | 尚未发布的随机 global 候选被跨 ASID owner 阻塞时清 G 重算 | `derive_allowed_tlb_shape_mask_with_global_fallback()` | 不修改任何 live owner |

### 1.2 核心函数抽象职责

`prepare_tlb_payload_context()`冻结 stage、CSR、PTE profile、PTE 字段和 fault；不选择 shape、
不生成 PPN，也不发布 live 状态。

`derive_allowed_tlb_shape_mask_with_global_fallback()`读取固定数量的 parent count，给 builder
返回可选 shape；只允许修改 detached prepared entry 的随机 G 位。

`finalize_tlb_payload_shape_and_payload()`按原有权重在 allowed mask 内选择最终组合并生成完整
payload；不登记 canonical/range/parent 索引。

`publish_live_tlb_entry()`是 canonical live entry 的唯一发布入口；`delete_live_tlb_entry_by_anchor_key()`
是统一删除入口，两者共同保证所有权生命周期对称。

`observer_build_commit_item()`按 UID 冻结翻译构造 RM 期望；L2TLB response pending 时返回重试，
翻译 fault 已成立时不再通过 PMA/PMP 追加访问异常。

## 2. Review 范围与结论

本次 review 覆盖以下本方案源码：

- `memblock_dispatch_types.sv`
- `memblock_tlb_entry.sv`
- `tlb_map_builder.sv`
- `common_data_transaction.sv`
- `mmu_csr_runtime_state.sv`
- `memblock_rm_readonly_api.sv`
- `memblock_rm.sv`
- `soft_test_l2tlb_range_lookup_sequence.sv`
- `soft_test_l2tlb_ppn_reuse_sequence.sv`
- `soft_test_rm_provenance_followup_sequence.sv` 中本方案相关部分
- `soft_test_tc_rm_pbmt_effective_fault.sv`
- `basicTest.sv`

最终结论：实现建立了 canonical live one-hot 不变量，大小页只能合法复用或在创建前被屏蔽；
没有给 DUT 增加不存在的 priority 选择，也没有修改 RTL/Scala。第三轮 review 首次发现权限专项断言
不够严格，修正为完整 24-bit 异常向量相等并重新编译、仿真、独立复审后通过。

## 3. 修改前后行为与正确性

### 3.1 修改前逻辑

1. exact/range miss 后 builder 可随机创建覆盖已有小页的新大页。
2. 多个 live raw-hit 候选被测试框架按“最大页”选择，但 DUT `Mux1H` 实际会按位 OR payload。
3. builder 一次性决定 PTE/level/PPN，common data 无法在 shape 选择前施加 live 占用约束。
4. 主表、range index 分步发布，删除路径也没有 parent owner 所有权。
5. RM commit 早于 L2TLB response 时会把正常 pending 当成 query miss；未对齐跨页访问可能查询
   DUT 从未请求的第二页。

### 3.2 修改后逻辑

1. 先 exact/range lookup；只有真正 miss 才进入两阶段 builder。
2. prepared context 查询 parent occupancy，屏蔽会与已有 owner 重叠的 shape。
3. final payload 再做一次索引复核，然后原子发布 canonical/range/parent 三类状态。
4. Fence/reset/显式删除统一反注册 owner；snapshot copy 不复制索引所有权。
5. issue-hit 在 capture 时按 sample 唯一化，RM 只消费冻结结果。
6. request 已 fire 但 response 未完成时 RM 有界重试；权限 PF/GPF 与未对齐/PMA 停止边界按 DUT
   实际行为建模。

### 3.3 正确性检查

| 检查点 | 实现依据 | 结论 |
|---|---|---|
| 小页先建、大页后建 | 候选 shape 查询区域内所有更小/同尺寸 owner | 大页被屏蔽 |
| 大页先建、小页后建 | 小页候选查询真实更大 owner 的自身区域 | 命中已有大页或拒绝重叠 |
| 同祖先但不相交 | key 同时保存 `owner_shape` 与 `parent_region` | 不误阻塞 |
| global/ASID | global 用 `ANY_ASID`；non-global 查 exact ASID 与 global | 任一方 global 时冲突，异 ASID non-global 可共存 |
| onlyStage1/2/allStage VMID | `current_vmid()` 对 1/2/3 均取 `hgatp_vmid` | lookup key 与 DUT matcher 一致 |
| snapshot 所有权 | `copy_from()` 清 range/parent ownership queue | 不发生重复撤销 |
| 正常路径性能 | 固定 5 个 shape、每 shape 固定 context key 查询 | 无 live 全表扫描 |
| fault 异常独占 | PMA/PMP 查询受翻译 fault 门控，专项精确比较完整向量 | PF/GPF 不叠加 Load AF |

## 4. 数据结构与 Payload 阶段

### 4.1 Parent key 与 payload phase

抽象功能描述：parent key 同时编码 owner 的真实大小、登记区域和翻译上下文；payload phase
则阻止 prepared、finalized、live 三种语义被混用。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv`、
`memblock_tlb_entry.sv`，字段定义。

```systemverilog
typedef struct packed {
    memblock_tlb_range_kind_e          range_kind;
    bit [1:0]                          s2xlate;
    bit [15:0]                         vmid;
    memblock_tlb_context_scope_e       context_scope;
    bit [15:0]                         asid;
    memblock_tlb_range_coverage_rank_e owner_shape;
    memblock_tlb_range_coverage_rank_e parent_region;
    bit [51:0]                         normalized_vpn;
} memblock_tlb_parent_occupancy_key_t;

typedef enum int unsigned {
    MEMBLOCK_TLB_PAYLOAD_RESET     = 0,
    MEMBLOCK_TLB_PAYLOAD_PREPARED  = 1,
    MEMBLOCK_TLB_PAYLOAD_FINALIZED = 2
} memblock_tlb_payload_phase_e;
```

中文伪代码：parent key 先记录当前 owner 实际覆盖多大，再记录它为哪个候选区域占位，并把
ASID/global/VMID 作用域一起放入关联数组 key。entry 新建或 reset 时处于 RESET；prepare 完成后
进入 PREPARED；只有 shape、PPN 和全部 payload 生成完才进入 FINALIZED。后续 helper 根据阶段拒绝
非法调用，不把构建过程态当成 live 状态。

关键副作用：`reset()`清空两类 ownership queue；`copy_from()`复制 payload phase 和 provenance，
但主动清空 range/parent queue，因此 snapshot 不拥有公共索引。

## 5. 两阶段 Builder 与重叠约束

### 5.1 Prepare/finalize 边界

抽象功能描述：prepare 冻结随机上下文，finalize 只在 allowed mask 内完成 shape 和 payload，避免
common data 为读取 occupancy 而依赖尚未定义的 builder 内部状态。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/tlb_map_builder.sv`，
`prepare_tlb_payload_context()`、`finalize_tlb_payload_shape_and_payload()`。

```systemverilog
entry.check_inactive_stage_defaults("PREPARE");
entry.payload_phase = memblock_tlb_entry::MEMBLOCK_TLB_PAYLOAD_PREPARED;

if (prepared_entry.payload_phase !=
        memblock_tlb_entry::MEMBLOCK_TLB_PAYLOAD_PREPARED) begin
    `uvm_fatal("TLB_BUILDER", "finalize got inconsistent prepared payload")
end
...
prepared_entry.payload_phase =
    memblock_tlb_entry::MEMBLOCK_TLB_PAYLOAD_FINALIZED;
```

中文伪代码：prepare 先完成 stage、CSR、PTE 和 fault 冻结并标记 PREPARED。finalize 入口首先确认
对象仍是同一 key 的 PREPARED payload；然后生成合法 shape、tag、PPN、sector、权限和 PBMT；全部
一致性检查通过后才标记 FINALIZED。任一阶段不匹配立即 fatal，不修改 live 表。

### 5.2 固定次数 overlap query

抽象功能描述：该 helper 为一个候选 shape 枚举所有可能与它 raw-hit 重叠的 parent key，不扫描
canonical 表或 owner 集合。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`，
`build_tlb_overlap_query_keys()`。

```systemverilog
for (int unsigned owner_idx = int'(MEMBLOCK_TLB_COVERAGE_4K);
     owner_idx <= int'(candidate_shape); owner_idx++) begin
    build_tlb_parent_query_keys_for_region(
        request_vpn, prepared_entry,
        memblock_tlb_range_coverage_rank_e'(owner_idx),
        candidate_shape, shape_keys);
end
for (int unsigned owner_idx = int'(candidate_shape) + 1;
     owner_idx <= int'(MEMBLOCK_TLB_COVERAGE_512G); owner_idx++) begin
    build_tlb_parent_query_keys_for_region(
        request_vpn, prepared_entry,
        memblock_tlb_range_coverage_rank_e'(owner_idx),
        memblock_tlb_range_coverage_rank_e'(owner_idx), shape_keys);
end
```

中文伪代码：先枚举所有不大于候选的 owner shape，并统一查询候选自身区域，从而发现区域内部
已有 child 或同尺寸 owner。再枚举所有更大 owner shape，并查询更大 owner 自身归一化区域，从而
判断它是否真的覆盖当前地址。两组循环上界均为五种固定 shape；返回的 key 交给 count 查询，正常
路径不遍历 owner witness。

正确性：相邻 2MB owner 虽共享 1GB 祖先，但候选 2MB 只查询自己的 2MB 区域，不会误阻塞；已有
1GB owner 时，小页候选查询该 1GB owner 的真实区域，能够发现覆盖。

## 6. 原子发布与删除生命周期

### 6.1 `publish_live_tlb_entry()`

抽象功能描述：该函数是新 live entry 唯一可见入口；先完成结构、重叠、容量和重复 owner 预检查，
再连续写主表、range index 和 parent index。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`，
`publish_live_tlb_entry()`。

```systemverilog
validate_normal_napot_payload(anchor_key, entry);
void'(validate_no_live_tlb_overlap(anchor_key, entry));
build_entry_range_index_keys(entry, range_keys);
build_tlb_parent_occupancy_keys(entry, parent_keys);
...
tlb_entry_by_key[anchor_key] = entry;
foreach (range_keys[key_idx]) begin
    tlb_anchor_keys_by_range_key[range_keys[key_idx]].push_back(anchor_key);
    entry.range_index_keys.push_back(range_keys[key_idx]);
end
foreach (parent_keys[key_idx]) begin
    tlb_parent_occupancy_count[parent_keys[key_idx]]++;
    tlb_parent_owner_set[owner_key] = 1'b1;
    entry.parent_occupancy_keys.push_back(parent_keys[key_idx]);
end
```

中文伪代码：先验证 payload 和 live overlap，再预生成 range/parent key 并检查重复、容量和 owner/count
一致性。只有全部检查通过后才把 entry 放入 canonical 表，随后逐项登记 range anchor、parent count、
owner set，并把实际登记 key 保存回 entry。函数中间没有时间推进，因此其他请求看不到半发布状态。

### 6.2 统一删除

抽象功能描述：删除入口按 entry 自己保存的 ownership 精确撤销两个索引，再删除 canonical 和
completed cache；不重新推导 shape，也不扫描完整表。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`，
`delete_live_tlb_entry_by_anchor_key()`。

```systemverilog
entry = tlb_entry_by_key[key];
unregister_tlb_range_index(key, entry);
unregister_tlb_parent_occupancy_index(key, entry);
tlb_entry_by_key.delete(key);
delete_completed_tlb_entry_by_anchor_key(key, reason);
```

中文伪代码：先取得 canonical entry；range helper 从各 bucket 删除且只允许找到一个 anchor；parent
helper 删除 `{parent,anchor}` owner 并递减 count，count 到零时删 key；之后删除 canonical entry，
最后清理同 anchor 的 completed cache。任一 ownership 丢失、重复或下溢都会 fatal。

## 7. RM Provenance、Pending 与异常边界

### 7.1 冻结 issue-hit provenance

抽象功能描述：RM 不再扫描 completed 历史并选择最大页，只验证并消费 issue 时冻结的唯一 payload。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_rm_readonly_api.sv`，
`resolve_uid_issue_hit_entry_for_rm()`。

```systemverilog
if (record.issue_hit_response_sample_seq > record.uid_wait_start_sample_seq ||
    record.issue_hit_entry_generation !=
        record.issue_hit_payload.entry_generation ||
    !data.entry_matches_request_raw(record.issue_hit_payload, request_vpn,
                                    record.s2xlate, record.csr_snapshot)) begin
    return report_query_miss("uid_tlb_issue_hit",
        $sformatf("uid=%0d frozen issue-hit provenance/raw match is inconsistent",
                  record.uid));
end
copy_tlb_entry(record.issue_hit_payload, entry_view);
```

中文伪代码：先确认 response sample 不晚于 UID 等待边界、generation 与冻结 payload 一致、冻结
payload 对该请求仍满足 raw matcher；任一条件不符就报告真实 query miss。全部一致时直接复制冻结
payload 和 request-derived PPN，不查询后来替换的 live/completed entry。

### 7.2 Permission fault 与 PMA/PMP 门控

抽象功能描述：首字节 PMA/PMP 只在翻译没有 PF/GPF/AF 时参与未对齐停止判断；翻译已经失败时
保持原异常，不补写 Load/Store AF。

源码位置：`mem_ut/ver/ut/memblock/env/src/memblock_rm/memblock_rm.sv`，
`observer_build_commit_item()`。

```systemverilog
if (byte_index == 0 && misaligned &&
    seq_csr_common::get_pma_pmp_model_en() &&
    !stage_one_fault && !stage_two_fault &&
    !access_fault && item.pa_valid_mask[0]) begin
    ...
end
...
if (!access_fault && !stage_one_fault && !stage_two_fault &&
    item.pa_valid_mask == required_pa_mask &&
    seq_csr_common::get_pma_pmp_model_en()) begin
    ...
end
```

中文伪代码：每个字节先根据冻结 TLB payload 计算 stage fault。只有首字节存在有效 PA、访问确实
未对齐且三类 fault 均未成立时，才读取一字节 PMA/PMP 事实；完整尺寸 PMA/PMP 查询也要求翻译
完全成功。若翻译 fault、PMA fault、MMIO 或 DUT 未对齐停止条件成立，循环停止，不查询不存在的
后续页。

### 7.3 严格异常向量专项

抽象功能描述：专项构造允许访问但 C=0/MMIO 的首 PA，证明权限 PF/GPF 是唯一异常，而不是仅证明
Load AF 位为零。

源码位置：`mem_ut/ver/ut/memblock/tc/src/soft_test/soft_test_tc_rm_pbmt_effective_fault.sv`，
`check_rm_production_commit_path()`。

```systemverilog
if (!env.rm.observer_build_commit_item(main_view, status_view, item,
                                       translation_pending) ||
    item.expected_exception !== 24'h002000) begin
    `uvm_fatal(get_type_name(),
               "S1 permission PF was not preserved exclusively over PMA MMIO")
end
...
if (!env.rm.observer_build_commit_item(main_view, status_view, item,
                                       translation_pending) ||
    item.expected_exception !== 24'h200000) begin
    `uvm_fatal(get_type_name(),
               "S2 permission GPF was not preserved exclusively over PMA MMIO")
end
```

中文伪代码：UID5 构造 S1 permission fault 后，要求整个 24-bit 向量严格等于 bit13；UID6 构造
S2 permission fault 后，要求整个向量严格等于 bit21。case inequality 同时拒绝额外异常位和 X/Z，
因此 Load AF 或其他异常只要叠加都会使专项 fatal。

## 8. 调用关系与文件覆盖

### 8.1 主调用关系

| 顺序 | 函数/对象 | 当前职责 | 主要副作用 |
|---|---|---|---|
| 1 | `get_or_create_l2tlb_entry_by_req_with_snapshot()` | exact/range/miss 总入口 | hit 只读，miss 进入 builder |
| 2 | `prepare_tlb_payload_context()` | 冻结上下文 | entry 进入 PREPARED |
| 3 | `derive_allowed_tlb_shape_mask_with_global_fallback()` | 查询 parent count | 返回 mask，必要时仅清 detached G |
| 4 | `finalize_tlb_payload_shape_and_payload()` | 生成完整 payload | entry 进入 FINALIZED |
| 5 | PPN reuse helper | 仅对新 4KB miss 做既有复用 | 不修改 hit entry |
| 6 | `publish_live_tlb_entry()` | 原子发布 | canonical/range/parent 同步可见 |
| 7 | Fence/reset/delete | 统一撤销 live owner | count/owner/canonical 对称清理 |
| 8 | UID capture/read-only API | 冻结并读取 provenance | RM 不重扫历史 |
| 9 | `observer_build_commit_item()` | 生成 RM 期望 | pending 重试、异常边界收敛 |

### 8.2 其余本方案文件

| 文件 | 实现作用 | Review 结论 |
|---|---|---|
| `soft_test_l2tlb_range_lookup_sequence.sv` | 10 种 child-parent、disjoint、global/ASID/VMID、allStage/N、lifecycle、expected-fatal | 使用生产 builder/publish/delete；非法注入仅进入只读检查 |
| `soft_test_l2tlb_ppn_reuse_sequence.sv` | directed superpage payload 补齐 FINALIZED 契约 | 不绕过统一 publish |
| `soft_test_rm_provenance_followup_sequence.sv` | 本方案部分改用完整 builder/publish/delete，历史选择改为冻结 sample | waiting UID fanout 部分另属其他方案 |
| `mmu_csr_runtime_state.sv` | onlyStage1/2/allStage key 均使用 `hgatp_vmid` | 与 DUT hit 条件一致 |
| `basicTest.sv` | report phase 输出 parent occupancy 统计 | 不改变 pass 判定 |

## 9. 验证结果

### 9.1 Range/生命周期专项

- mode：`l2tlb_overlap_guard_v16_20261021`
- 日志：`mem_ut/ver/ut/memblock/sim/l2tlb_overlap_guard_v16_20261021/log/tc=tc_l2tlb_range_lookup_smoke_ts=virtual_base_sequence_cfg=default_seed=2026102010_rtl.log`
- 结果：`TEST CASE PASSED`，预期 catcher 为 fatal=2、warning=4，最终 `UVM_ERROR=0`、
  `UVM_FATAL=0`、Quit `0/10`、正常 `$finish`。

### 9.2 RM 权限/PMA/PBMT 专项

- 最终 mode：`l2tlb_overlap_guard_v20_exact_exception_20261021`
- 日志：`mem_ut/ver/ut/memblock/sim/l2tlb_overlap_guard_v20_exact_exception_20261021/log/tc=tc_rm_pbmt_effective_fault_ts=virtual_base_sequence_cfg=default_seed=2026102011_rtl.log`
- UID5：VA `0x812aa001`、PA `0x20010001`、严格异常 `0x002000`。
- UID6：VA `0x812bb001`、PA `0x20010001`、严格异常 `0x200000`。
- 结果：`TEST CASE PASSED`、`UVM_ERROR=0`、`UVM_FATAL=0`、Quit `0/10`、正常 `$finish`。

### 9.3 原失败 seed 10000 笔

- mode：`l2tlb_overlap_guard_v17_seed_20261021`
- seed：`850748471`
- 日志：`mem_ut/ver/ut/memblock/sim/l2tlb_overlap_guard_v17_seed_20261021/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=850748471_rtl.log`
- UID494 未对齐跨页仅解析首字节，期望与 DUT 均为 `0x20`，未出现第二页 query miss。
- `RM_LS_TRACE_COMPARE=10000`、`TEST_PASS`、`UVM_ERROR=0`、`UVM_FATAL=0`、Quit `0/10`、
  正常 `$finish`。
- parent 统计：`lookup=71159`、`block_64k=961`、`block_2m=2341`、`block_1g=2468`、
  `n_override=10`、`forced_4k=0`。

### 9.4 独立 Review 迭代

1. 主体重叠索引、builder、lifecycle、RM 门控 review 无功能缺陷。
2. 独立 reviewer 发现 UID5/6 断言未约束其他异常位，结论 `REVIEW_FAIL`。
3. 修改为完整 24-bit case inequality 后重新编译并复跑专项。
4. 第二个独立只读 reviewer 返回 `REVIEW_PASS`，确认唯一问题已修复且其余结论不变。
5. 第三个独立 reviewer 质疑 overlap/allStage 动态证据和 PF/GPF 场景缺少 PMA 查询计数；前者已由
   v16 range 专项和 v17 原失败 seed 日志覆盖，后者需要按生产语义判断：翻译 fault 成立后必须跳过
   `read_pma_pmp_af_for_rm()`，不能要求 fault 场景执行该查询。
6. 最终独立仲裁 reviewer 返回 `REVIEW_PASS`：`memblock_rm.sv` 在 PMA/PMP 查询前先累积三类翻译
   fault，且仅在三者均为 0 时进入查询；v20 专项先建立能够产生差异的 PMA/MMIO 前置条件，再对
   UID5/6 完整异常向量作严格断言。现有静态门控、动态输入和完整输出检查已形成闭环，生产 query
   counter 仅属于可选防御性增强，不是关闭本方案的必需项。

## 10. 风险边界

1. parent occupancy 只约束 canonical live entry，不复制到 completed/pending/UID snapshot。
2. owner set 全扫描仅允许在 fatal witness 路径；正常 miss 只查固定数量 count。
3. global fallback 只允许 PREPARED、generation=0、无 ownership 的 detached entry。
4. forced 4KB/N override 只改变 shape/N，不重新随机 fault、权限、G、PBMT 或 CSR。
5. 本轮验证完成专项和原失败 seed；Plan 中建议的后续 100-seed 回归不属于本实现关闭条件。

## 11. Plan 对齐检查

### 11.1 实现与 Plan 不一致项

执行中发现的 `owner_shape`、global fallback、onlyStage1 VMID、payload phase、pending/retry、
未对齐跨页停止和 PF/GPF-PMA 门控均已作为 `IMPLEMENTATION_DELTA` 回写 Plan。

未发现实现与 Plan 不一致项；当前 coding 行为与对应 Plan 保持一致。

### 11.2 Plan 未说明但 Coding 落实的细节

最终严格异常向量断言已回写 Plan 的最终验证记录。除已回写的执行中增量外，未发现 Plan 未说明但
Coding 额外落实的细节；当前 coding 细节均已在对应 Plan 中覆盖。

## 12. 非本次修改的逻辑分析

### 12.1 git status 对比结论

当前工作区含大量其他任务或用户修改。本 review 只覆盖第 2 节列出的本方案文件和其中对应逻辑。

| 类别 | 文件/目录 | 判断 | 原因 |
|---|---|---|---|
| 其他功能逻辑 | `soft_test_memblock_dispatch_replay_smoke_sequence.sv` | 另行 review | STA replay lifecycle 方向 |
| 其他功能逻辑 | `soft_test_memblock_pending_mmio_directed_sequence.sv`、`writeback_status_handler.sv` | 另行 review | load-MMIO redirect/deleted-owner 方向 |
| 混合文件中的其他逻辑 | `soft_test_rm_provenance_followup_sequence.sv` 的 waiting UID fanout | 另行 review | L2TLB waiting UID logical fanout 方案 |
| 文档整理 | `AI_DOC/analysis/rtl/v2/*`、RTL knowledge 文件 | 非本次逻辑 | StoreQueue/知识库维护 |
| 测试配置 | `tc_l2tlb_fault_2pct_no_trigger.cfg` | 其他任务输入 | 本轮只消费该 cfg，不修改其内容 |
| 仿真产物 | `mem_ut/ver/ut/memblock/sim/*`、`verdiLog/*` | 非源码 review | 编译、日志、波形和用户已有删除项 |
| 用户文档 | DOCX、figure、BT 验证方案等未跟踪文件 | 非本次逻辑 | 不读取、不覆盖、不提交 |

## 13. 最终结论

L2TLB 大小页重叠保护实现满足 Plan：live 表在同一请求上下文中保持至多一个 raw-hit owner，
发布/删除/reset/copy 生命周期对称，正常路径无全表高频扫描，RM pending 与异常停止边界和 DUT 行为一致。
三类验证均通过，独立复审无剩余 blocker。
