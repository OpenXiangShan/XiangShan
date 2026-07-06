# memblock_tlb_entry.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_tlb_entry.sv`

对应 review 清单：

- [memblock_tlb_entry_review.md](../../../plan/test_framework/review_doc/undo/memblock_tlb_entry_review.md)

## 1. 文件定位与使用场景

`memblock_tlb_entry.sv` 定义两类对象：

- `memblock_tlb_entry`：live TLB cache 的一条表项，按 `{vpn, asid, vmid, s2xlate}` key 唯一存放在 `common_data_transaction::tlb_entry_by_key[]`。
- `memblock_uid_tlb_record`：某个 uid 发射时的 TLB 上下文记录，存放在 `common_data_transaction::uid_tlb_record_by_uid[]`，用于 debug、追溯和 PTW-back replay 等待判断。

这两个对象拆开的原因是：同一个进程上下文下，多条 transaction 可能访问同一个 VPN，它们应该复用同一条 live TLB/PTE 映射；但每个 uid 仍需要保留“发射时使用的 VPN、ASID/VMID、S2xlate、CSR 快照，以及最终回填到自己的 PTE 内容”，方便后续定位是哪条 transaction 等到了哪条 TLB response。

## 2. memblock_tlb_entry

`memblock_tlb_entry` 是 L2TLB responder 真正用来回包的表项。

关键字段：

| 字段 | 含义 | 使用场景 |
|---|---|---|
| `lookup_key` | `{vpn,asid,vmid,s2xlate}` | `tlb_entry_by_key[]` 的 key，也回填 S1/S2 tag。 |
| `vaddr/paddr/vpn/ppn` | 虚拟/物理地址与页号 | `paddr/ppn` 用于构造 L2TLB response。 |
| `pte_r/w/x/u/g/a/d/n/v`、`pbmt` | PTE 权限与属性 | 由权重随机生成，再通过模式化修正。 |
| `tlbAF/tlbPF/tlbGPF/pmaAF` | fault/exception 注入位 | 回填到 response 的 access/page/guest fault。 |
| `asid/vmid/s2xlate/priv_mode` | runtime CSR 上下文 | 保证同 VPN 在不同进程或翻译阶段下不会混用。 |
| `addr_low/ppn_low/valididx/pteidx` | response 派生字段 | 填充 L2TLB agent xaction 的 S1 index 类 payload。 |
| `create_cycle/last_hit_cycle` | 建表与命中时间 | debug 和后续覆盖率统计。 |

主要函数：

| 函数 | 输入 | 输出/副作用 | 作用 |
|---|---|---|---|
| `reset()` | 无 | 清空字段 | 创建或复用对象前恢复默认状态。 |
| `update_addr_fields(vaddr_i,paddr_i)` | 虚拟/物理地址 | 更新 `vpn/ppn/addr_low` 并派生 index 字段 | 把地址变成 L2TLB response 可直接使用的页级字段。 |
| `apply_csr_state(csr_state,s2xlate_i)` | runtime CSR、req 的 `s2xlate` | 写入 key、ASID、VMID、priv mode | 建表必须使用当前 CSR 真值，不使用初始 CSR 或 plus 配置。 |
| `derive_index_fields()` | 无 | 派生 `valididx/pteidx/ppn_low/level` | 生成 response 中的 S1 index payload，并按参数选择 level。 |
| `choose_level()` | 无 | 生成 `level` | 支持固定值、合法随机和简化推导三种模式。 |
| `apply_pte_profile(access_kind)` | 访问类型 | 调整 PTE 组合 | 根据 `LEGAL/MIXED/EXCEPTION_BIASED` 模式决定回复更偏正常还是更偏异常。 |
| `derive_ad_bits(access_kind)` | 访问类型 | 修正 `A/D` | 统一处理 `A=0 -> D=0`，并按 load/store/unknown 区分 `D` 位语义。 |
| `fixup_pte_legal(access_kind)` | 访问类型 | 修正 PTE bit | 先应用模式化 profile，再做最小一致性修正。 |

## 2.1 后续增强建议

当前 `memblock_tlb_entry.sv` 已经落了第一版增强：`level` 已参数化，PTE 修正已经模式化，`A/D` 也拆成了独立 helper。下面这部分描述的是这套实现的设计意图和后续还可以继续增强的方向。

### A. `level` 不建议长期固定为 `2'd0`

当前源码中 `derive_index_fields()` 直接把：

- `level = 2'd0`

这意味着当前所有 L2TLB response 都按同一个页级返回。这个值更像“先让框架跑通”的固定占位，而不是完整语义建模。

当前实现：

- `seq_csr_common.sv` 已新增：
  - `tlb_level_mode`
  - `tlb_level_fixed_value`
  - `tlb_level_random_low`
  - `tlb_level_random_high`
- `choose_level()` 已支持三种模式：
  - `FIXED`
  - `RANDOM`
  - `DERIVED`
- 当前 `DERIVED` 仍是简化实现，用 `addr_low[2]` 做轻量推导，不代表最终真实页级语义。

这样做的原因是：

- `level` 是 response payload 的真实语义字段，不适合一直硬编码成 `2'd0`。
- 但当前也不必一开始就做复杂页级推导；先参数化，再逐步收紧语义，成本最低。

### B. `fixup_pte_legal()` 目前过于偏向“正常合法回复”

当前 `fixup_pte_legal()` 的设计目标是把随机出的 PTE 收敛到一个较正常的组合：

- `V=0` 时清掉 `R/W/X/A/D`
- `W=1,R=0` 时自动把 `R` 拉高
- `R/W/X` 全 0 时自动补一个 `R=1`
- `A=0` 时强制 `D=0`

这种方式适合默认 smoke，但问题是：

- 它会把很多原本可以用于 fault/exception 场景的位组合修掉。
- 当前所有 L2TLB response 会天然偏向“能正常访问”的页。
- 后续如果想覆盖非法 PTE 组合、page fault、guest fault、access fault 等路径，就不够用了。

当前实现已经把这部分改成“模式化”而不是单一路径 fixup：

- `tlb_pte_mode = LEGAL`
  - 保持当前风格，优先生成正常合法页。
- `tlb_pte_mode = MIXED`
  - 合法与异常/非法组合混合，按权重控制比例。
- `tlb_pte_mode = EXCEPTION_BIASED`
  - 优先制造 fault 或异常相关组合，用于专项场景。

当前实现方式：

- `apply_pte_profile()`：先根据模式和访问类型决定要不要保留更多异常/非法组合。
- `fixup_pte_legal()`：再做必要的最小一致性修正。

这样既能保留默认稳定性，也能给异常激励留入口。

### C. `A/D` 位需要单独建模，不能只靠最后一句修正

当前 `A/D` 只做了一个一致性规则：

- `A=0 -> D=0`

这保证了最低限度的位关系，但还不足以支撑完整测试，因为：

- `A` 位常用于 access fault / page fault 相关路径。
- `D` 位对 store 类访问更关键，对 load 通常不是重点。
- 如果完全靠随机 + 最后修正，很容易得到大量“看起来合法但对测试没价值”的组合。

当前实现已经把 `A/D` 单独建模：

- 保留当前权重入口：
  - `tlb_pte_a_1_wt / tlb_pte_a_0_wt`
  - `tlb_pte_d_1_wt / tlb_pte_d_0_wt`
- 新增 helper：
  - `derive_ad_bits(access_kind)`

推荐语义：

- 合法模式：
  - 仍优先保证最小一致性。
  - `A=0 -> D=0`。
  - `LOAD` 下 `D` 被压成 0。
  - `STORE` 下 `D` 要求与 `W` 兼容。
  - `UNKNOWN` 是当前 builder 默认路径，表示暂时不知道真实访问类型；为了保证默认 L2TLB 回复更偏正常可通过，`V=1` 时直接把 `A/D` 都置 1。
- 混合模式：
  - 提高 `A=0`、`D=0` 出现概率，便于覆盖 fault 路径。
- 异常偏置模式：
  - 对 load 场景优先制造 `A=0`。
  - 对 store 场景优先制造 `A=0` 或 `D=0`。

这里建议尽量区分 `load-like` 和 `store-like`。当前代码已经留好了访问类型入口：

- `D` 位对 store 场景的语义更强。
- 如果不区分访问类型，很多 `D` 位随机结果对 load 回复没有实际覆盖价值，只会增加噪声。
- 当前 `tlb_map_builder` 先用 `MEMBLOCK_TLB_ACCESS_UNKNOWN` 调 `fixup_pte_legal()`，这样接口已经准备好，后续真正拿到 load/store 语义后可以直接升级成更精细的调用。
- `UNKNOWN` 不再按 store-like 保守修正 `D`，而是作为默认 smoke/正常回复路径使用：有效 PTE 下强制 `A=1,D=1`。

### D. 推荐落地顺序

如果后续准备增强 `memblock_tlb_entry.sv`，建议按下面顺序做，成本最低：

1. 先把 `level` 参数化
   - 这一项已经完成
   - 当前仍可继续增强 `DERIVED` 模式的真实语义

2. 再把 `fixup_pte_legal()` 模式化
   - 这一项已经完成
   - 当前已经支持 `LEGAL / MIXED / EXCEPTION_BIASED`

3. 最后补 `A/D` 专项 helper
   - helper 已落地
   - 下一步重点是把真实 load/store 语义传给 builder，而不是一直用 `UNKNOWN`

### E. 当前实现与后续目标的关系

当前版本仍然合理，因为它优先保证：

- lookup key 正确
- response payload 字段能完整回填
- 默认 smoke 可稳定闭环

但如果要把它作为“可覆盖正常 + 异常 + 非法组合”的 TLB responder 基础层，上述增强是必要的。

## 3. memblock_uid_tlb_record

`memblock_uid_tlb_record` 不参与 L2TLB lookup 命中仲裁。它只记录 uid 自己的上下文和最终 PTE 回填结果。

关键字段：

| 字段 | 含义 | 使用场景 |
|---|---|---|
| `uid`、`record_valid` | 记录归属与有效位 | uid 发射后由 `register_uid_tlb_record_on_issue()` 创建。 |
| `pte_valid` | PTE 是否已回填 | PTW-back replay 等待逻辑可用它判断该 uid 是否已经等到 L2TLB response。 |
| `vpn/s2xlate/is_hypervisor_inst` | 发射时的翻译上下文 | 用于和 L2TLB request 生成的 key 匹配。 |
| `asid/vmid/lookup_key` | 发射时 CSR 派生 key | `update_uid_tlb_records_by_entry()` 用它匹配 live entry。 |
| `csr_snapshot` | 发射时 runtime CSR 备份 | debug 使用，避免后续 CSR 变化后看不回当时上下文。 |
| `paddr/ppn/PTE/fault` | 回填后的 entry 内容 | 记录该 uid 最终看到的 PTE/PFN/异常信息。 |
| `issue_cycle/pte_update_cycle` | 发射和回填时间 | debug、覆盖率和等待超时分析。 |

主要函数：

| 函数 | 输入 | 输出/副作用 | 作用 |
|---|---|---|---|
| `reset()` | 无 | 清空 uid record | 复用对象前清状态。 |
| `init_context(uid,vpn,s2xlate,is_hypervisor_inst,csr_snapshot)` | uid、VPN、翻译阶段、CSR 快照 | 设置 `record_valid=1`，清 `pte_valid`，生成 `lookup_key/asid/vmid` | uid 发射到流水线后先登记上下文，但此时可能还没有 L2TLB response。 |
| `copy_entry_fields(entry)` | live TLB entry | 回填 PTE/PFN/fault 并置 `pte_valid=1` | L2TLB responder 建表或命中后，把同 key 的 entry 复制到所有匹配 uid record。 |

## 4. 设计边界

- 不再保留 `tlb_transaction.sv`，也不再维护 `tlb_table_by_uid[]` 或 `key -> uid` 反向索引。
- `tlb_entry_by_key[]` 可以被后续 `sfence/hfence` entry 级逻辑失效，但 `uid_tlb_record_by_uid[]` 是历史追踪数据，默认保留到 testcase 结束。
- `csr_update_seq` 不参与 TLB 命中控制；进程/阶段变化通过 key 中的 ASID、VMID、S2xlate 自然区分。
