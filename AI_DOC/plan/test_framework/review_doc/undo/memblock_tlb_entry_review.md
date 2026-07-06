# memblock_tlb_entry 实现 Review 清单

本文档用于 review `memblock_tlb_entry.sv`、`tlb_map_builder.sv` 以及相关参数入口的实现细节，方便逐项确认当前实现是否符合预期。

对应源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_tlb_entry.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/tlb_map_builder.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`
- `mem_ut/ver/ut/memblock/env/plus.sv`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg`

## 1. 本次实现目标

本次修改的目标是把原先偏“最小合法跑通”的 TLB entry 生成逻辑，扩成一版可配置、可 review 的基础实现，重点覆盖三件事：

1. `level` 不再固定写死为 `2'd0`，改成可参数化控制。
2. `fixup_pte_legal()` 不再只有单一“收敛到正常页”的路径，改成支持 `LEGAL / MIXED / EXCEPTION_BIASED` 三种模式。
3. `A/D` 位从“只在最后一句简单修正”改成独立 helper，明确 `A=0 -> D=0`，并为后续按 load/store 区分建模留接口。

## 2. 新增参数入口

本次新增参数如下：

| 参数 | 位置 | 当前作用 |
|---|---|---|
| `MEMBLOCK_TLB_LEVEL_MODE` | `plus.sv` / `default.cfg` / `seq_csr_common` | 选择 `level` 生成模式：`0=fixed`、`1=random`、`2=derived` |
| `MEMBLOCK_TLB_LEVEL_FIXED_VALUE` | 同上 | fixed 模式下返回的 `level` |
| `MEMBLOCK_TLB_LEVEL_RANDOM_LOW` | 同上 | random 模式下 `level` 下界 |
| `MEMBLOCK_TLB_LEVEL_RANDOM_HIGH` | 同上 | random 模式下 `level` 上界 |
| `MEMBLOCK_TLB_PTE_MODE` | 同上 | 选择 PTE 修正模式：`0=LEGAL`、`1=MIXED`、`2=EXCEPTION_BIASED` |

review 时需要确认：

- `level` 的合法范围目前是否接受 `0..3`。
- `PTE_MODE` 三档是否满足 testcase 需求。
- 这些参数是否还需要在 testcase cfg 中增加专项 preset。

## 3. 代码实现拆解

### 3.1 `memblock_tlb_entry::choose_level()`

当前支持三种模式：

- `FIXED`
  - 直接返回 `tlb_level_fixed_value`
- `RANDOM`
  - 在 `tlb_level_random_low..tlb_level_random_high` 之间随机
- `DERIVED`
  - 当前是简化实现：`addr_low[2] ? 1 : 0`

review 重点：

- `DERIVED` 目前只是轻量推导，不代表最终页级语义。
- 如果 DUT 真实依赖 `level` 精确语义，后续需要替换成更贴近页大小或 stage 类型的推导。

### 3.2 `memblock_tlb_entry::apply_pte_profile()`

这是“模式化入口”，决定本条 entry 偏正常还是偏异常。

- `LEGAL`
  - 不主动制造异常组合
- `MIXED`
  - 保留部分异常/非法组合的可能性
  - 当前主要允许 `V=0` 时不强制清空全部权限，只先清 `A/D`
- `EXCEPTION_BIASED`
  - 按访问类型更积极地降低 `A/D`
  - store 场景更容易得到 `A=0`、`D=0`
  - load 场景更容易得到 `A=0` 且 `D=0`

review 重点：

- `MIXED` 和 `EXCEPTION_BIASED` 当前属于第一版实现，已经能制造更多异常组合，但还不是完整 fault 模板库。
- 如果后续需要覆盖更具体的 fault 原因，建议再细分 dedicated template。

### 3.3 `memblock_tlb_entry::derive_ad_bits()`

这是本次 `A/D` 位的核心 helper。

当前行为：

- `V=0` 时：
  - `A=0`
  - `D=0`
- `A=0` 时：
  - 强制 `D=0`
- `STORE`
  - `D` 需要和 `W` 兼容
- `LOAD`
  - `D=0`
- `UNKNOWN`
  - 当前作为默认正常回复路径处理，`V=1` 时直接设置 `A=1,D=1`

review 重点：

- `A=0 -> D=0` 已显式固化。
- `LOAD` 把 `D` 压成 0 是为了减少无意义噪声。
- 由于当前 L2TLB responder 路径还拿不到真实 load/store 语义，builder 暂时传的是 `UNKNOWN`；因此 `UNKNOWN` 需要偏正常回复，避免默认 L2TLB 回复因为 A/D 随机过低而制造非预期异常。

### 3.4 `memblock_tlb_entry::fixup_pte_legal()`

当前变成两段式：

1. `apply_pte_profile(access_kind)`
2. `derive_ad_bits(access_kind)`，并在 `LEGAL` 模式下额外做最小一致性修正：
   - `W=1,R=0` 时补 `R=1`
   - `R/W/X` 全 0 时补一个 `R=1`

review 重点：

- `LEGAL` 模式仍保持当前 smoke 友好行为。
- `MIXED/EXCEPTION_BIASED` 不再强制把所有组合都修回“正常页”。

## 4. Builder 调用路径

当前调用路径是：

1. `common_data_transaction::build_tlb_entry_for_key()`
2. `tlb_map_builder::build_tlb_entry_for_req()`
3. `memblock_tlb_entry::fixup_pte_legal(MEMBLOCK_TLB_ACCESS_UNKNOWN)`

这说明当前第一版已经把“访问类型”接口留出来了，但还没有真正从请求语义上传 `LOAD/STORE`。

review 时要确认：

- 当前先传 `UNKNOWN` 是否接受。
- 是否要在后续任务里把 access kind 从主表、issue path 或 TLB request 上文继续传到 builder。

## 5. 当前实现的优点

- 不再把 `level` 永久写死。
- 不再把所有 PTE 都收敛成“正常合法页”。
- `A/D` 位逻辑比之前清晰很多，后续能继续扩。
- 参数入口已经统一接到 `plus.sv -> seq_csr_common.sv -> default.cfg`。

## 6. 当前残余风险

1. `DERIVED` 模式还是简化版
   - 只能表达轻量差异，不能代表真实页级语义。

2. `UNKNOWN` access kind 仍然较多
   - 这意味着 `D` 位还没有完全按真实 load/store 请求区分。

3. 异常模式还不是完整 fault 模板
   - 当前只是把“更容易制造异常组合”的通道打开了。

## 7. 建议 review 问题

建议 review 时按下面问题逐项确认：

1. `level` 的合法范围是不是 `0..3`，还是应该进一步限制。
2. `DERIVED` 模式是否需要更贴近实际页级推导。
3. `LEGAL/MIXED/EXCEPTION_BIASED` 三档是否满足 testcase 需求。
4. `LOAD` 场景下强制 `D=0` 是否接受。
5. 后续是否需要把 access kind 从真实请求语义继续上传到 builder。
