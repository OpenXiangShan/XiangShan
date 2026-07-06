# 主表权限地址范围 profile 控制方案

状态：undo

创建日期：2026-06-28

关联旧方案：

- `AI_DOC/plan/test_framework/plan/undo/main_table_permission_consistency_plan_20260626.md`

关联源码：

- `mem_ut/ver/ut/memblock/seq/base_seq/main_control_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_tlb_entry.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/tlb_map_builder.sv`
- `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_l2tlb_base_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/mem_base_sequence.sv`
- `mem_ut/ver/ut/memblock/env/plus.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg`

## 1. 背景

旧方案尝试让 `main_control_transaction` 中的以下字段直接控制 L2TLB 和 DCache responder：

```systemverilog
rand bit       tlbAF;
rand bit       tlbPF;
rand bit       tlbGPF;
rand bit [1:0] PBMT;
rand bit       corrupt;
rand bit       denied;
```

执行前审查发现该方案存在真实 DUT 粒度不匹配问题：

- 跨 4K 时，一条 uid transaction 可能被拆成两笔 TLB/VPN 请求。
- 跨 cacheline 时，一条 uid transaction 可能被拆成两笔 DCache/cacheline 访问。
- TLB/DCache 命中会复用已有 entry/cacheline，后续同地址访问不一定再次下发 responder request。
- 因此“每条 transaction 直接携带一组权限并期望每次都生效”的模型不稳定。

本方案改为 **地址范围 profile 控制**：权限先绑定到地址范围，uid 通过选择地址范围间接选择权限。这样更贴近硬件行为：TLB 权限属于 VPN/page，DCache corrupt/denied 属于 PA/cacheline 或 memory response。

## 2. 总体目标

第一版目标：

1. 增加主表权限总开关。关闭时保持当前测试框架行为：TLB/DCache responder 继续使用现有随机/默认建表和 memory model 逻辑，不受主表字段控制。
2. 开启主表权限模式后，主表 transaction 通过选择 permission profile 对应地址范围来控制 TLB/DCache response。
3. 增加两个场景控制选项：跨 4K 和跨 cacheline。
4. TLB/DCache 权限均提供两份：主权限和第二份权限。第二份只在对应跨界场景打开并且访问确实产生第二个 fragment 时启用。
5. 默认不改变现有 `tc_sanity` / smoke 行为。

## 3. 参数方案

### 3.1 模式开关

新增公共测试框架 plus 参数：

| 参数 | 默认值 | 含义 |
|---|---:|---|
| `MEMBLOCK_MAIN_PERMISSION_EN` | 0 | 主表权限地址范围模式总开关。为 0 时保持当前随机 responder 行为。 |
| `MEMBLOCK_MAIN_TLB_PERMISSION_EN` | 0 | 主表 TLB 权限子开关。必须总开关同时为 1 才生效。 |
| `MEMBLOCK_MAIN_DCACHE_PERMISSION_EN` | 0 | 主表 DCache 权限子开关。必须总开关同时为 1 才生效。 |
| `MEMBLOCK_MAIN_PERMISSION_DEBUG_CHECK_EN` | 0 | 过度/二次防御检查开关。只用于 hit 复用一致性诊断，不改变主路径行为。 |

落点：

- `mem_ut/ver/ut/memblock/env/plus.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg`

### 3.2 跨界场景控制参数

新增两个控制选项：

| 参数 | 默认值 | 含义 |
|---|---:|---|
| `MEMBLOCK_MAIN_PERMISSION_CROSS_4K_EN` | 0 | 主表权限模式下，随机主表生成时强制定向构造跨 4K 访问；TLB 子开关开启时启用第二份 TLB 权限。 |
| `MEMBLOCK_MAIN_PERMISSION_CROSS_CACHELINE_EN` | 0 | 主表权限模式下，随机主表生成时强制定向构造跨 DCache cacheline 访问；DCache 子开关开启时启用第二份 DCache 权限。 |

约束：

```text
if !MEMBLOCK_MAIN_PERMISSION_EN:
  CROSS_4K_EN 和 CROSS_CACHELINE_EN 不改变当前随机 responder 行为。

if MEMBLOCK_MAIN_PERMISSION_EN && !CROSS_4K_EN:
  地址生成默认避免跨 4K。

if MEMBLOCK_MAIN_PERMISSION_EN && !CROSS_CACHELINE_EN:
  地址生成默认避免单独跨 DCache cacheline；
  但如果 CROSS_4K_EN=1，跨 4K 天然带来的 DCache 双 cacheline 不应被禁止。
```

中文文字伪代码：

总开关决定是否进入主表权限地址范围模式。总开关关闭时，所有新增 profile、跨界和第二份权限都不参与 responder 行为，测试框架保持当前随机建表和 memory model 结果。总开关打开后，第一版把跨 4K / 跨 cacheline 开关定义为随机主表生成的定向强制开关：开关打开时，自动生成的 uid 默认构造对应跨界访问；手动表不被强制改地址，只按实际地址是否跨界来选择主权限或第二份权限。跨 4K 开关控制 TLB tail fragment 是否可使用第二份 TLB 权限；跨 cacheline 开关控制 DCache tail fragment 是否可使用第二份 DCache 权限。需要注意，跨 4K 会自然跨 cacheline；此时即使跨 cacheline 开关关闭，也只表示不启用 `corrupt2/denied2`，不是禁止 DCache 拆成两条 cacheline。

### 3.3 子开关门控语义

源码级伪代码：

```text
main_perm_en   = get_main_permission_en()
tlb_perm_en    = main_perm_en && get_main_tlb_permission_en()
dcache_perm_en = main_perm_en && get_main_dcache_permission_en()

need_tlb_addr_owner    = tlb_perm_en || dcache_perm_en
need_tlb_perm_check    = tlb_perm_en
need_dcache_addr_owner = dcache_perm_en
need_dcache_perm_check = dcache_perm_en

cross_4k_for_generation:
  仅在 main_perm_en && need_tlb_addr_owner && CROSS_4K_EN 时对随机主表强制生效。

cross_cacheline_for_generation:
  仅在 main_perm_en && dcache_perm_en && CROSS_CACHELINE_EN 时对随机主表强制生效。
```

中文文字伪代码：

TLB 子开关只控制 TLB 权限字段是否覆盖 L2TLB entry，以及同 VPN 的 TLB 权限是否做一致性检查。DCache 子开关只控制 DCache `corrupt/denied` 是否覆盖 memory response，以及同 cacheline 的 DCache 权限是否做一致性检查。DCache 权限需要 PA，因此只开 DCache 子开关时仍必须建立 TLB 地址 owner 和确定性 PPN，但不覆盖 TLB 权限字段，也不检查 `tlbAF/tlbPF/tlbGPF/PBMT`。只开 TLB 子开关时，不建立 DCache permission owner，不改写 `corrupt/denied`。两个子开关都关闭时，总开关即使打开，也不应改变 responder 行为。

## 4. 主表 transaction 字段扩展

### 4.1 保留现有主权限字段

现有字段作为主权限：

```systemverilog
rand bit       tlbAF;
rand bit       tlbPF;
rand bit       tlbGPF;
rand bit [1:0] PBMT;
rand bit       corrupt;
rand bit       denied;
```

含义：

- 默认用于第一个 TLB fragment / 第一个 DCache fragment。
- 非跨界场景下，也是唯一生效权限。

### 4.2 新增第二份 TLB 权限字段

建议新增：

```systemverilog
rand bit       tlb2AF;
rand bit       tlb2PF;
rand bit       tlb2GPF;
rand bit [1:0] PBMT2;
```

含义：

- 只在 `MEMBLOCK_MAIN_PERMISSION_EN && MEMBLOCK_MAIN_TLB_PERMISSION_EN && MEMBLOCK_MAIN_PERMISSION_CROSS_4K_EN` 且该 transaction 实际跨 4K 时使用。
- 用于第二个 VPN / tail TLB fragment。
- 非跨 4K 或跨 4K 开关关闭时不应被 responder 使用。

### 4.3 新增第二份 DCache 权限字段

建议新增：

```systemverilog
rand bit       corrupt2;
rand bit       denied2;
```

含义：

- 只在 `MEMBLOCK_MAIN_PERMISSION_EN && MEMBLOCK_MAIN_DCACHE_PERMISSION_EN && MEMBLOCK_MAIN_PERMISSION_CROSS_CACHELINE_EN` 且该 transaction 实际跨 DCache cacheline 时使用。
- 用于第二条 cacheline / tail DCache fragment。
- 非跨 cacheline 或跨 cacheline 开关关闭时不应被 responder 使用。

### 4.4 字段含义文字伪代码

```text
uid transaction 权限字段：
  tlbAF/tlbPF/tlbGPF/PBMT 是主 TLB 权限。
  tlb2AF/tlb2PF/tlb2GPF/PBMT2 是跨 4K tail TLB 权限。
  corrupt/denied 是主 DCache 权限。
  corrupt2/denied2 是跨 cacheline tail DCache 权限。
```

中文文字伪代码：

每条 uid transaction 最多提供两份 TLB 权限和两份 DCache 权限。主权限总是作为第一个访问片段的默认值；第二份权限只在对应跨界开关打开并且地址确实跨界时启用。这样既能保持普通场景简单，也能表达“跨 4K 第二页使用不同 TLB 权限”和“跨 cacheline 第二条 cacheline 使用不同 DCache 错误属性”。

## 5. 地址范围 profile 模型

### 5.1 Profile 定义

建议在测试框架中定义 profile record：

```systemverilog
typedef struct {
    int unsigned profile_id;
    bit          tlbAF;
    bit          tlbPF;
    bit          tlbGPF;
    bit [1:0]    PBMT;
    bit          corrupt;
    bit          denied;
} memblock_main_permission_profile_t;
```

Profile 表示一个权限组合。例如：

```text
profile 0: TLB normal + DCache normal
profile 1: TLB PF + DCache normal
profile 2: TLB AF + DCache normal
profile 3: TLB GPF + DCache normal
profile 4: TLB normal + DCache corrupt
profile 5: TLB normal + DCache denied
```

第一版 profile 可以由主表 transaction 字段动态生成，不必一开始暴露复杂 profile 配置文件。后续如需要固定地址池，可以把 profile 扩展成显式 range 配置。

### 5.2 地址范围 owner

建议派生两类 owner：

```systemverilog
typedef struct {
    memblock_uid_t uid;
    bit            is_second;
    bit [63:0]     va;
    bit [51:0]     vpn;
    bit            tlbAF;
    bit            tlbPF;
    bit            tlbGPF;
    bit [1:0]      PBMT;
    bit [63:0]     ppn;
    bit            ppn_valid;
} memblock_tlb_addr_owner_t;

typedef struct {
    memblock_uid_t uid;
    int unsigned   owner_id;
    bit            is_second;
    bit [63:0]     line_va;
    bit [63:0]     line_pa;
    bit            line_pa_valid;
    bit            corrupt;
    bit            denied;
} memblock_dcache_addr_owner_t;
```

含义：

- TLB owner 按 VPN 表示该 VPN 对应的 canonical TLB 权限。
- DCache owner 按 cacheline 表示该 cacheline 对应的 canonical DCache 错误属性。
- `is_second=1` 表示该 owner 来自跨界 tail 片段，使用第二份权限。

建议维护的索引：

```systemverilog
memblock_tlb_addr_owner_t    tlb_owner_by_vpn[bit [51:0]];
memblock_dcache_addr_owner_t dcache_owner_by_line_va[bit [63:0]];
memblock_dcache_addr_owner_t dcache_owner_by_line_pa[bit [63:0]];
memblock_dcache_addr_owner_t dcache_owner_pool[int unsigned];
int unsigned                 dcache_owner_ids_by_uid[memblock_uid_t][$];
```

其中 `dcache_owner_pool` 保存 DCache owner 本体，`owner_id` 是 pool 下标；`dcache_owner_by_line_va` 和 `dcache_owner_by_line_pa` 保存可回查到 owner 的副本或 id，coding 时二者必须保持一致。第一版使用确定性 PPN，因此 `dcache_owner_by_line_pa` 应在主表 owner 构建完成后预计算建立，用于 DCache/SBuffer responder 按 PA 查 owner。构建期先用 `line_va` 检查 VA 层一致性，再用 `deterministic_unique_ppn(vpn)` 直接计算 `line_pa`；运行期 responder 只用 `line_pa` 查最终响应属性。`dcache_owner_ids_by_uid` 作为按 uid 追踪 owner 的辅助索引，当前第一版不是主路径必需索引，后续如果保留 runtime 回填增强时可复用。

### 5.3 为什么叫地址范围 profile

```text
uid 不直接强行覆盖 responder。
uid 先选择/生成一个地址。
该地址落入某个 VPN 和某个 cacheline。
VPN/cacheline 再绑定到权限 profile。
responder 按真实 request 地址查 profile。
```

中文文字伪代码：

地址范围 profile 把权限从“某条 uid 的私有字段”转成“某段地址的属性”。uid 仍然能通过选择地址来定向控制权限，但 responder 只根据真实硬件请求地址查权限。这样命中复用时，后续同 VPN 或同 cacheline 的访问自然继承同一个 profile，不会出现同一地址范围前后权限冲突。

## 6. 主表构建流程

### 6.1 地址生成规则

主表权限模式开启后，地址生成需要遵守：

```text
if CROSS_4K_EN == 0:
  默认选择不跨 4K 的 VA。

if random_mode && CROSS_4K_EN == 1:
  强制选择跨 4K 的 VA；
  第二个 VPN 使用 tlb2AF/tlb2PF/tlb2GPF/PBMT2。

if CROSS_CACHELINE_EN == 0:
  默认选择不单独跨 DCache cacheline 的 VA/PA。
  如果 CROSS_4K_EN == 1，允许跨 4K 自然产生的两条 DCache cacheline。

if random_mode && CROSS_CACHELINE_EN == 1:
  强制选择跨 cacheline 的 VA；
  第二条 cacheline 使用 corrupt2/denied2。

if manual_mode:
  不强制改用户地址；
  只按用户地址实际是否跨界展开 parts。
```

第一版建议只在主表权限模式下启用这些定向约束；模式关闭时，不改变现有主表地址随机方式。

### 6.2 构建 owner 表

源码级伪代码：

```text
build_main_permission_addr_owners(manual_mode):
  if !get_main_permission_en():
    return

  tlb_perm_en    = get_main_tlb_permission_en()
  dcache_perm_en = get_main_dcache_permission_en()
  need_tlb_addr_owner    = tlb_perm_en || dcache_perm_en
  need_dcache_addr_owner = dcache_perm_en

  tlb_owner_by_vpn.delete()
  dcache_owner_by_line_va.delete()
  dcache_owner_by_line_pa.delete()
  dcache_owner_ids_by_uid.delete()

  for uid in [0, main_trans_num):
    tr = get_main_transaction(uid)
    access_va = tr.src_0 + tr.imm
    size_bytes = decode_size_bytes(tr)

    if need_tlb_addr_owner:
      tlb_parts = split_by_4k(access_va, size_bytes)
      for i in [0, tlb_parts.size):
        owner = make_tlb_owner(uid, tlb_parts[i])
        fill_tlb_owner_permission_by_part_index(owner, tr, i)
        if tlb_perm_en:
          insert_or_check_tlb_owner_permission(owner, manual_mode)
        else:
          reserve_tlb_addr_owner_without_permission_check(owner)

    if need_dcache_addr_owner:
      dcache_parts = split_by_cacheline(access_va, size_bytes)
      for i in [0, dcache_parts.size):
        owner = make_dcache_owner(uid, dcache_parts[i])
        fill_dcache_owner_permission_by_part_index(owner, tr, i)
        insert_or_check_dcache_owner_permission(owner, manual_mode)
```

中文文字伪代码：

该 helper 负责把 transaction 展开成 VPN/cacheline owner。随机主表构建时，它不是等整张主表完成后才统一执行，而是每生成一个 uid 后立即执行检查和 reserve，让后续 uid 能看到更老 owner；手动主表导入时，则在整张表导入完成后按 uid 顺序重建 owner 表并检查冲突。函数先判断主表权限总开关，关闭时直接返回。TLB 子开关开启时，TLB owner 同时承担地址记录和权限一致性检查；DCache 子开关开启时，需要 TLB 地址 owner 提供确定性 PPN，同时建立 DCache owner 并检查 `corrupt/denied` 一致性。只开 DCache 子开关时不会覆盖或检查 TLB 权限，只用 TLB owner 记录 VPN 到 PPN 的映射。只开 TLB 子开关时不会建立 DCache permission owner，也不会改写 `corrupt/denied`。

### 6.3 owner 插入和冲突处理

源码级伪代码：

```text
fill_tlb_owner_permission_by_part_index(owner, tr, part_index):
  if part_index == 0:
    owner.permission = tr.tlbAF/tlbPF/tlbGPF/PBMT
  else if get_cross_4k_en():
    owner.permission = tr.tlb2AF/tlb2PF/tlb2GPF/PBMT2
  else:
    owner.permission = tr.tlbAF/tlbPF/tlbGPF/PBMT

fill_dcache_owner_permission_by_part_index(owner, tr, part_index):
  if part_index == 0:
    owner.permission = tr.corrupt/denied
  else if get_cross_cacheline_en():
    owner.permission = tr.corrupt2/denied2
  else:
    owner.permission = tr.corrupt/denied

insert_or_check_tlb_owner_permission(owner, manual_mode):
  key = owner.vpn
  if !tlb_owner_by_vpn.exists(key):
    tlb_owner_by_vpn[key] = owner
    return

  old = tlb_owner_by_vpn[key]
  if same_tlb_permission(old, owner):
    return

  if manual_mode:
    fatal("same VPN has inconsistent TLB permission")
  else:
    canonicalize_current_uid_permission_to_owner(owner.uid, owner, old)
    rebuild owner from current transaction
    record owner as normalized alias

insert_or_check_dcache_owner_permission(owner, manual_mode):
  key = owner.line_va
  if !dcache_owner_by_line_va.exists(key):
    dcache_owner_by_line_va[key] = owner
    dcache_owner_ids_by_uid[owner.uid].push_back(owner.owner_id)
    return

  old = dcache_owner_by_line_va[key]
  if same_dcache_permission(old, owner):
    return

  if manual_mode:
    fatal("same cacheline has inconsistent DCache permission")
  else:
    canonicalize_current_uid_permission_to_owner(owner.uid, owner, old)
    rebuild owner from current transaction
    record owner as normalized alias
```

中文文字伪代码：

权限填充函数根据 fragment index 决定使用主权限还是第二份权限。第 0 个 fragment 总是使用主权限；tail fragment 只有在对应跨界开关开启时使用第二份权限，否则仍使用主权限。TLB owner 以 VPN 为 key 插入；DCache owner 以 cacheline VA 为 key 插入。第一次出现的地址 owner 是 canonical owner。后续如果遇到同 VPN 或同 cacheline，并且权限一致，直接接受；如果权限不一致，手动表模式直接报 fatal，因为真实 DUT hit 复用无法保证后续不同权限生效；随机表模式不能只改临时 owner，必须把当前 transaction 中对应的主权限或第二份权限字段也改成最早 owner 权限，然后再重新展开 owner，避免主表字段和 owner 表不一致。DCache owner 首次登记时还要把 owner id 记录到 `dcache_owner_ids_by_uid[uid]`，用于调试和后续扩展；第一版 `line_pa` 主路径通过遍历 `dcache_owner_pool` 预计算，不依赖 runtime PPN 回填。

### 6.3.1 归一化回写字段规则

源码级伪代码：

```text
canonicalize_current_uid_permission_to_owner(uid, current_owner, canonical_owner):
  tr = get_main_transaction(uid)

  if current_owner is TLB owner:
    if current_owner.is_second && get_cross_4k_en():
      tr.tlb2AF  = canonical_owner.tlbAF
      tr.tlb2PF  = canonical_owner.tlbPF
      tr.tlb2GPF = canonical_owner.tlbGPF
      tr.PBMT2   = canonical_owner.PBMT
    else:
      tr.tlbAF   = canonical_owner.tlbAF
      tr.tlbPF   = canonical_owner.tlbPF
      tr.tlbGPF  = canonical_owner.tlbGPF
      tr.PBMT    = canonical_owner.PBMT

  if current_owner is DCache owner:
    if current_owner.is_second && get_cross_cacheline_en():
      tr.corrupt2 = canonical_owner.corrupt
      tr.denied2  = canonical_owner.denied
    else:
      tr.corrupt  = canonical_owner.corrupt
      tr.denied   = canonical_owner.denied
```

中文文字伪代码：

归一化必须回写主表 transaction 字段。当前冲突 owner 如果代表 head fragment，就回写主权限字段；如果代表 tail fragment 且对应跨界开关开启，就回写第二份权限字段；如果 tail fragment 没有启用第二份权限，则仍回写主权限字段。这样 load/store 地址复用、跨 4K、跨 cacheline 场景都不会出现 owner 表和 transaction 字段不一致。



### 6.4 地址复用场景的权限一致性要求

主表权限模式开启后，地址复用不仅要复用地址，也必须复用该地址对应的权限 owner。需要覆盖以下四类主表构建场景：

```text
load  复用 store 地址：ld_after_store / load_from_store
load  复用 load  地址：ld_after_load  / load_from_load
store 复用 store 地址：st_after_store / store_from_store
store 复用 load  地址：st_after_load  / store_from_load
```

无论是哪一类复用，只要复用后落到相同 VPN 或相同 cacheline，就必须满足：

```text
相同 VPN：
  当前 TLB part 对应的主权限或第二份 TLB 权限必须和已有 VPN owner 一致。

相同 cacheline：
  当前 DCache part 对应的主权限或第二份 DCache 权限必须和已有 cacheline owner 一致。
```

源码级伪代码：

```text
apply_permission_for_reused_addr(cur_uid, ref_uid, cur_tr, ref_tr):
  cur_parts = split_addr_to_tlb_and_dcache_parts(cur_tr)
  ref_parts = split_addr_to_tlb_and_dcache_parts(ref_tr)

  for each cur_tlb_part:
    if ref or existing owner has same vpn:
      if !same_tlb_permission(cur_part, owner):
        if manual_mode:
          fatal("same VPN reused with different TLB permission")
        else:
          canonicalize_current_uid_permission_to_owner(cur_uid, cur_tlb_part, owner)

  for each cur_dcache_part:
    if ref or existing owner has same cacheline:
      if !same_dcache_permission(cur_part, owner):
        if manual_mode:
          fatal("same cacheline reused with different DCache permission")
        else:
          canonicalize_current_uid_permission_to_owner(cur_uid, cur_dcache_part, owner)
```

中文文字伪代码：

该逻辑在主表地址复用完成后运行。函数先把当前 uid 和被复用 uid 的地址都展开成 TLB parts 和 DCache parts。只要当前 uid 的某个 VPN 已经有 owner，当前 TLB part 对应的主权限或第二份权限就必须和 owner 一致；只要当前 uid 的某条 cacheline 已经有 owner，当前 DCache part 对应的主权限或第二份权限就必须和 owner 一致。手动表模式下，不一致说明用户手动构造了同地址不同权限，直接 fatal 并打印当前 uid、参考 uid、VPN/cacheline 和两边权限。随机表模式下，不一致时调用 `canonicalize_current_uid_permission_to_owner()` 回写当前 uid 对应字段，保证地址复用后不会出现真实 DUT hit 复用无法表达的同地址不同权限。

地址复用场景的处理顺序：

```text
1. 根据 RAW/地址相关策略选择 ref_uid。
2. 当前 uid 复用 ref_uid 地址。
3. 如果主表权限模式开启：
     检查当前 uid 与 ref_uid/owner 的 VPN 权限一致性；
     检查当前 uid 与 ref_uid/owner 的 cacheline 权限一致性；
     随机表自动改当前 uid 权限到 owner；
     手动表发现不一致直接 fatal。
4. 再进入 owner reserve 或 owner 兼容检查。
```

中文文字伪代码：

地址复用不能只复制 `src_0/imm/VA`。在主表权限模式下，复用地址等价于复用该地址已建立的权限语义。因此 load 复用 store、load 复用 load、store 复用 store、store 复用 load 四类路径都要在复用地址后同步处理权限。随机表可以自动把当前 uid 权限归一到已有 owner，保持场景可运行；手动表必须报错，让用户显式修正同 VPN 或同 cacheline 的权限冲突。

### 6.5 第一版 VA 分配策略：按 uid 独立 page / page pair 确定性分配

第一版不采用复杂随机搜索。主表权限模式开启后，地址生成优先使用确定性公式，让每个 uid 默认占用独立 page 或 page pair，从源头降低 VPN/cacheline owner 冲突概率。

推荐公式：

```text
普通不跨界：
  va = base_va + uid * 0x1000

跨 cacheline：
  va = base_va + uid * 0x1000 + 60

跨 4K：
  va = base_va + uid * 0x2000 + 4092

同时跨 4K + 跨 cacheline：
  va = base_va + uid * 0x2000 + 4092
```

说明：

- `uid * 0x1000` 让普通场景和跨 cacheline 场景每个 uid 使用独立 4K page。
- `uid * 0x2000` 让跨 4K 场景每个 uid 使用独立 page pair，避免 uidN 的 tail page 和 uidN+1 的 head page 重叠。
- `+60` 是 8B 访问时的示例，表示低 4B 在当前 cacheline、尾 4B 进入下一条 cacheline。coding 时不能固定写死 60，应实现 `make_cross_cacheline_offset(size_bytes)`：选择 `split_low_bytes` 满足 `1 <= split_low_bytes < size_bytes`，offset 为 `64 - split_low_bytes`；8B 默认可取 `split_low_bytes=4`，得到 offset 60。
- `+4092` 是 8B 访问时的示例，表示低 4B 在当前 4K page、尾 4B 进入下一页。coding 时不能固定写死 4092，应实现 `make_cross_4k_offset(size_bytes)`：选择 `split_low_bytes` 满足 `1 <= split_low_bytes < size_bytes`，offset 为 `4096 - split_low_bytes`；8B 默认可取 `split_low_bytes=4`，得到 offset 4092。
- `size_bytes=1` 无法构造跨 cacheline 或跨 4K 访问；如果跨界开关要求此类场景，应重新选择 size 或报配置错误。
- owner 表检查仍必须保留，但第一版 owner 检查主要作为防御和手动表冲突诊断，不作为常规随机 retry 主路径。

中文文字伪代码：

第一版地址分配不先做随机 retry，而是直接根据 uid 生成稳定地址。普通场景和跨 cacheline 场景每个 uid 使用独立 page，因此 VPN 默认不重叠；跨 4K 场景每个 uid 使用独立 page pair，因此 head/tail 两个 VPN 都不会和相邻 uid 重叠。这样生成出的地址天然更容易满足 TLB owner 和 DCache owner 兼容条件。后续仍会展开 TLB parts 和 DCache parts 并执行 owner 检查，如果出现冲突，说明手动表、base 设置或后续增强逻辑破坏了独立分配假设，应按手动/随机模式处理。

### 6.6 VA 分配接受条件：TLB owner 和 DCache owner 必须同时兼容

即使使用确定性地址分配，主表权限模式仍必须检查候选 VA 是否同时满足 TLB 和 DCache owner 兼容条件。对于每个 uid，候选 VA 必须先展开成 TLB parts 和 DCache parts，并且两边 owner 检查都通过后才能接受。

接受条件：

```text
candidate_va 可分配给 uid
  = cross_4k 开关约束满足
  && cross_cacheline 开关约束满足
  && 如果 TLB 子开关开启，所有 TLB parts 的 VPN owner 权限兼容
  && 如果 DCache 子开关开启，所有 DCache parts 的 cacheline owner 权限兼容
```

源码级伪代码：

```text
allocate_main_permission_va(uid, tr):
  size_bytes = decode_size_bytes(tr)
  tlb_perm_en    = get_main_tlb_permission_en()
  dcache_perm_en = get_main_dcache_permission_en()
  need_tlb_addr_owner = tlb_perm_en || dcache_perm_en

  if need_tlb_addr_owner && get_cross_4k_en():
    candidate_va = base_va + uid * 0x2000 + make_cross_4k_offset(size_bytes)
  else if dcache_perm_en && get_cross_cacheline_en():
    candidate_va = base_va + uid * 0x1000 + make_cross_cacheline_offset(size_bytes)
  else:
    candidate_va = base_va + uid * 0x1000

  tlb_parts    = split_by_4k(candidate_va, size_bytes)
  dcache_parts = split_by_cacheline(candidate_va, size_bytes)

  cross_4k       = tlb_parts.size > 1
  cross_cacheline = dcache_parts.size > 1

  if !get_cross_4k_en() && cross_4k:
    fatal("main permission generated unexpected cross-4K VA")

  // 注意：跨 4K 天然会跨 64B cacheline，因为 4K 边界也是 cacheline 边界。
  // 因此 cross_4k_en=1 时，即使 cross_cacheline_en=0，也允许 dcache_parts.size > 1；
  // 只是第二条 cacheline 是否使用 corrupt2/denied2 仍由 cross_cacheline_en 决定。
  if !get_cross_cacheline_en() && cross_cacheline && !cross_4k:
    fatal("main permission generated unexpected cross-cacheline VA")

  tlb_ok    = !tlb_perm_en    || check_all_tlb_owner_compatible(tlb_parts, tr)
  dcache_ok = !dcache_perm_en || check_all_dcache_owner_compatible(dcache_parts, tr)

  if !(tlb_ok && dcache_ok):
    handle_owner_conflict(uid, tr, tlb_parts, dcache_parts)

  if need_tlb_addr_owner:
    reserve_tlb_owners(tlb_parts, tr)
  if dcache_perm_en:
    reserve_dcache_owners(dcache_parts, tr)
  return candidate_va
```

中文文字伪代码：

该函数负责为当前 uid 分配一个不会破坏已有地址 owner 语义的 VA。随机主表中，跨 4K 开关打开时强制选择跨 4K 地址；否则 DCache 子开关和跨 cacheline 开关同时打开时强制选择跨 cacheline 地址；其他情况选择普通独立 page 地址。随后根据访问 size 展开 TLB parts 和 DCache parts。如果跨界开关关闭却生成出不允许的跨界地址，说明地址 helper 有错误，直接报错。权限兼容检查受子开关控制：TLB 子开关关闭时不检查 TLB 权限；DCache 子开关关闭时不检查 DCache 权限。只要某个子开关开启，对应 owner 兼容检查必须通过或完成随机表归一化后通过，才能登记 owner 并返回 VA。

### 6.7 owner 冲突处理策略

源码级伪代码：

```text
handle_owner_conflict(uid, tr, tlb_parts, dcache_parts):
  if manual_mode:
    fatal("manual main table VA conflicts with existing VPN/cacheline owner")

  if random_mode:
    canonicalize_current_uid_permission_to_owner(uid, tr, tlb_parts, dcache_parts)
    recheck tlb_ok and dcache_ok
    if !(tlb_ok && dcache_ok):
      fatal("main permission canonicalization failed")
```

中文文字伪代码：

owner 冲突表示当前 uid 生成或复用的地址已经落入已有 VPN/cacheline owner，但当前 uid 权限和 owner 不一致。手动表模式下，这属于用户显式构造的同地址不同权限，直接 fatal。随机表模式下，不应保留同地址不同权限；框架需要把当前 uid 对应的 TLB/DCache 权限字段归一化到已有 owner 权限，然后重新检查 TLB 和 DCache owner 是否都兼容。如果归一化后仍不兼容，说明跨 4K、跨 cacheline 或 owner 表维护存在更深层不一致，应 fatal 暴露问题。

### 6.8 TLB owner 兼容检查

源码级伪代码：

```text
check_all_tlb_owner_compatible(tlb_parts, tr):
  for i in [0, tlb_parts.size):
    vpn = tlb_parts[i].vpn

    if i == 0:
      expected = tr.tlbAF/tlbPF/tlbGPF/PBMT
    else:
      if get_cross_4k_en():
        expected = tr.tlb2AF/tlb2PF/tlb2GPF/PBMT2
      else:
        expected = tr.tlbAF/tlbPF/tlbGPF/PBMT

    if !tlb_owner_by_vpn.exists(vpn):
      continue

    old = tlb_owner_by_vpn[vpn]
    if !same_tlb_permission(old, expected):
      return 0

  return 1
```

中文文字伪代码：

TLB owner 兼容检查按当前 uid 真实会访问的 VPN 列表逐项检查。第一个 VPN 使用主 TLB 权限；第二个及后续 VPN 只有在跨 4K 开关打开时使用第二份 TLB 权限，否则仍使用主权限。如果某个 VPN 还没有 owner，则该 VPN 可以被当前 uid 占用；如果已经有 owner，则当前期望权限必须和旧 owner 一致，否则说明该候选 VA 会制造同 VPN 不同权限冲突，检查失败。

### 6.9 DCache owner 兼容检查

源码级伪代码：

```text
check_all_dcache_owner_compatible(dcache_parts, tr):
  for i in [0, dcache_parts.size):
    line = dcache_parts[i].line_va

    if i == 0:
      expected = tr.corrupt/denied
    else:
      if get_cross_cacheline_en():
        expected = tr.corrupt2/denied2
      else:
        expected = tr.corrupt/denied

    if !dcache_owner_by_line_va.exists(line):
      continue

    old = dcache_owner_by_line_va[line]
    if !same_dcache_permission(old, expected):
      return 0

  return 1
```

中文文字伪代码：

DCache owner 兼容检查按当前 uid 真实会访问的 cacheline 列表逐项检查。第一条 cacheline 使用主 DCache 权限；第二条及后续 cacheline 只有在跨 cacheline 开关打开时使用第二份 DCache 权限，否则仍使用主权限。如果某条 cacheline 还没有 owner，则该 cacheline 可以被当前 uid 占用；如果已经有 owner，则当前期望 `corrupt/denied` 必须和旧 owner 一致，否则该候选 VA 会制造同 cacheline 不同错误属性冲突，检查失败。

### 6.10 owner reserve 顺序

源码级伪代码：

```text
reserve_owners_after_va_accept(uid, tr, tlb_parts, dcache_parts):
  // 只在 TLB 和 DCache 兼容检查都通过后调用。
  reserve_tlb_owners(tlb_parts, tr)
  reserve_dcache_owners(dcache_parts, tr)
```

中文文字伪代码：

owner reserve 只能在候选 VA 被接受后执行，不能在 TLB 检查通过但 DCache 检查失败时提前登记 TLB owner，也不能在 DCache 检查通过但 TLB 检查失败时提前登记 DCache owner。这样避免一次失败候选污染 owner 表。登记顺序本身不表达优先级；真正的接受条件是 TLB 和 DCache 两边都兼容。

### 6.11 offset helper 语义

源码级伪代码：

```text
make_cross_cacheline_offset(size_bytes):
  if size_bytes <= 1:
    fatal("size_bytes cannot cross cacheline")
  split_low_bytes = choose_default_split_low_bytes(size_bytes)
  return 64 - split_low_bytes

make_cross_4k_offset(size_bytes):
  if size_bytes <= 1:
    fatal("size_bytes cannot cross 4K")
  split_low_bytes = choose_default_split_low_bytes(size_bytes)
  return 4096 - split_low_bytes
```

中文文字伪代码：

offset helper 只负责生成一个能让访问跨过边界的低位偏移。`split_low_bytes` 必须满足 `1 <= split_low_bytes < size_bytes`，保证访问一部分落在 head fragment，另一部分落在 tail fragment。8B 访问第一版可默认选择 `split_low_bytes=4`，因此跨 cacheline offset 为 60，跨 4K offset 为 4092。`size_bytes=1` 无法跨界，必须重新随机 size 或报配置错误。

### 6.12 reserve helper 语义

源码级伪代码：

```text
reserve_tlb_owners(tlb_parts, tr):
  for each tlb_part:
    owner = make_tlb_owner(...)
    fill_tlb_owner_permission_by_part_index(owner, tr, part_index)
    if tlb_perm_en:
      insert_or_check_tlb_owner_permission(owner, manual_mode)
    else:
      reserve_tlb_addr_owner_without_permission_check(owner)

reserve_dcache_owners(dcache_parts, tr):
  for each dcache_part:
    owner = make_dcache_owner(...)
    fill_dcache_owner_permission_by_part_index(owner, tr, part_index)
    insert_or_check_dcache_owner_permission(owner, manual_mode)
```

中文文字伪代码：

reserve helper 负责把已经接受的 VA parts 正式写入 owner 表。TLB reserve 在 TLB 子开关开启时做权限检查；如果只是 DCache 子开关需要 PPN，则只登记 VPN 地址 owner，不检查也不覆盖 TLB 权限。DCache reserve 只有 DCache 子开关开启时执行，负责登记 `line_va` owner 和 uid 到 owner id 的索引。

## 7. L2TLB responder 接入

### 7.1 模式关闭行为

```text
if !main_permission_en || !main_tlb_permission_en:
  不用主表字段覆盖 entry 的 tlbAF/tlbPF/tlbGPF/PBMT；
  如果 dcache_permission_en=0，也不需要确定性 PPN；
  如果 dcache_permission_en=1，仍需要确定性 PPN；line_pa 已在构建期预计算。
```

中文文字伪代码：

主表权限总开关关闭时，L2TLB responder 完全保持当前行为。TLB 子开关关闭时，不用主表 TLB 权限字段覆盖 entry，也不做 TLB 权限一致性检查；但如果 DCache 子开关开启，DCache responder 需要 PA cacheline owner，因此 L2TLB responder 仍要使用确定性 PPN。由于 PPN 可由 VPN 直接计算，`line_pa` 在构建期已经预计算完成，runtime 不再把回填作为主流程。TLB 和 DCache 子开关都关闭时，默认回归仍走当前随机建表方式。

### 7.2 模式开启行为

源码级伪代码：

```text
on_l2tlb_get_or_create(req):
  key = make_tlb_key(req)
  main_perm_en = get_main_permission_en()
  tlb_perm_en = main_perm_en && get_main_tlb_permission_en()
  dcache_perm_en = main_perm_en && get_main_dcache_permission_en()
  need_owner_ppn = tlb_perm_en || dcache_perm_en
  debug_check_en = main_perm_en && get_main_permission_debug_check_en()

  if hit entry:
    if need_owner_ppn && debug_check_en:
      expected_ppn = deterministic_unique_ppn(req.vpn)
      if entry.ppn != expected_ppn:
        error("main permission hit entry PPN is not deterministic owner PPN")
    if tlb_perm_en && debug_check_en:
      check_hit_entry_with_tlb_owner(req.vpn, entry)
    return entry

  entry = build_random_or_default_entry(key)

  if need_owner_ppn:
    entry.ppn = deterministic_unique_ppn(req.vpn)

  if tlb_perm_en:
    if find_tlb_owner_by_vpn(req.vpn, owner):
      entry.tlbAF  = owner.tlbAF
      entry.tlbPF  = owner.tlbPF
      entry.tlbGPF = owner.tlbGPF
      entry.PBMT   = owner.PBMT
    else:
      warning("no TLB owner for main permission VPN")

  insert entry
  return entry
```

中文文字伪代码：

L2TLB responder 仍先按现有 key 查 `tlb_entry_by_key`。主表权限模式下，只要 TLB 或 DCache 任一子开关开启，就需要稳定 PPN：miss 新建 entry 时先沿用现有 builder 创建基础 entry，再把 `entry.ppn` 覆盖成 `deterministic_unique_ppn(req.vpn)`。如果 TLB 子开关开启，再用 VPN owner 覆盖 `tlbAF/tlbPF/tlbGPF/PBMT`。命中已有 entry 时不覆盖 entry；只有在 `main_permission_en && MEMBLOCK_MAIN_PERMISSION_DEBUG_CHECK_EN` 同时为 1 时，才检查 hit entry 的 PPN 是否等于确定性 PPN，并在 TLB 子开关开启时检查 TLB 权限是否和 owner 一致。第一版 `line_pa` 在构建期预计算，L2TLB runtime 不再负责回填 `line_pa`。第一版不新增 strict mode 参数，找不到 owner 时使用 `uvm_warning`，真正防御性一致性检查受 `MEMBLOCK_MAIN_PERMISSION_DEBUG_CHECK_EN` 控制。

## 8. DCache/SBuffer responder 接入

### 8.1 模式关闭行为

```text
if !main_permission_en || !main_dcache_permission_en:
  dcache/sbuffer responder 保持当前 memory model 行为；
  corrupt/denied 仍由当前测试框架已有逻辑决定。
```

中文文字伪代码：

主表权限模式关闭或 DCache 子开关关闭时，DCache/SBuffer responder 不查主表 owner 表，也不覆盖 memory model 产生的 `corrupt/denied`。这保证默认回归不受新方案影响。

### 8.2 模式开启行为

源码级伪代码：

```text
override_dcache_error_from_main(req_pa, corrupt, denied):
  if !(main_permission_en && main_dcache_permission_en):
    return 0

  line_pa = cacheline_base(req_pa)
  if !find_dcache_owner_by_line_pa(line_pa, owner):
    warning("no dcache owner for line_pa")
    return 0

  corrupt = owner.corrupt
  denied  = owner.denied
  return 1
```

中文文字伪代码：

DCache/SBuffer responder 仍先执行现有 memory model，得到基础 data/corrupt/denied。主表权限和 DCache 子开关开启后，用 request PA 对齐到 cacheline base，并查找对应 DCache owner。找到 owner 时，用 owner 的 `corrupt/denied` 覆盖 response；找不到 owner 时，保留 memory model 结果并打印 warning，避免非主表来源访问导致 responder 卡死。

## 9. 构建期 PPN / line_pa 预计算

第一版使用确定性 PPN，因此不需要等 L2TLB responder runtime hit/miss 后再回填 `line_pa`。主表 owner 构建完成后，应直接按 VPN 预计算 TLB owner 的 PPN 和 DCache owner 的 line PA。

源码级伪代码：

```text
precompute_main_permission_pa_owners():
  if !main_permission_en:
    return

  need_owner_ppn = get_main_tlb_permission_en() || get_main_dcache_permission_en()
  if !need_owner_ppn:
    return

  for each tlb_owner in tlb_owner_by_vpn:
    ppn = deterministic_unique_ppn(tlb_owner.vpn)
    tlb_owner.ppn = ppn
    tlb_owner.ppn_valid = 1

  if !get_main_dcache_permission_en():
    return

  dcache_owner_by_line_pa.delete()
  for each dcache_owner in dcache_owner_pool:
    vpn = vpn_from_va(dcache_owner.line_va)
    ppn = deterministic_unique_ppn(vpn)
    dcache_owner.line_pa = {ppn, dcache_owner.line_va[11:0]}
    dcache_owner.line_pa_valid = 1
    upsert_dcache_owner_by_line_pa(dcache_owner)

upsert_dcache_owner_by_line_pa(dcache_owner):
  key = dcache_owner.line_pa
  if !dcache_owner_by_line_pa.exists(key):
    dcache_owner_by_line_pa[key] = dcache_owner
    return

  old = dcache_owner_by_line_pa[key]
  if same_dcache_permission(old, dcache_owner):
    return

  // 第一版 deterministic_unique_ppn 应避免 PA alias 冲突。
  // 这里属于 debug/防御路径，受 MEMBLOCK_MAIN_PERMISSION_DEBUG_CHECK_EN 控制。
  if get_main_permission_debug_check_en():
    error("same line_pa has inconsistent DCache permission")

find_dcache_owner_by_line_pa(line_pa, owner):
  if !dcache_owner_by_line_pa.exists(line_pa):
    return 0
  owner = dcache_owner_by_line_pa[line_pa]
  return 1
```

中文文字伪代码：

主表权限模式开启后，如果 TLB 或 DCache 任一子开关开启，框架在 owner 构建期直接给每个 TLB owner 计算 `ppn = deterministic_unique_ppn(vpn)`。如果 DCache 子开关开启，框架遍历 DCache owner pool，从 `line_va` 直接得到 VPN，再用确定性 PPN 组合出 `line_pa = {ppn, line_va[11:0]}`，随后写入 `dcache_owner_by_line_pa`。跨 4K 时 head/tail 两个 VPN 会自然对应不同 PPN，DCache owner 的 `line_va` 属于哪个 VPN，就使用哪个 VPN 的 PPN。DCache/SBuffer responder 后续只通过 `find_dcache_owner_by_line_pa()` 按 PA cacheline 查询 owner。由于第一版使用确定性唯一 PPN，正常不应出现两个不同 VA 映射到同一 line PA 且权限不同；如果 debug check 开启时发现该情况，应报错暴露 owner 或 PPN 生成问题。runtime L2TLB 不再负责 `line_pa` 回填，只负责 miss 时生成同样的确定性 PPN，hit 时在 debug 模式下做一致性检查。

### 9.1 确定性 PPN 生成函数

源码级伪代码：

```text
deterministic_unique_ppn(vpn):
  ppn_base = get_main_permission_ppn_base_or_default()
  ppn = ppn_base + stable_hash_or_low_bits(vpn)
  return ppn
```

中文文字伪代码：

确定性 PPN 函数用于主表权限模式下稳定地把 VPN 映射到 PPN。第一版不要求复杂页表 profile，只要求同一个 VPN 每次得到同一个 PPN，不同 VPN 默认不会映射到同一 PA cacheline。具体 coding 可以使用固定 base 加 VPN 低位或简单 hash，但必须保证在当前主表规模内不会制造不必要 PA alias。

## 10. pmaAF 第一版处理

`pmaAF` 暂不纳入本方案第一版。

建议策略：

```text
check_pmaAF_supported_for_main_permission(tr):
  if main_permission_en && tr.pmaAF == 1:
    uvm_fatal("pmaAF is not supported by main permission range profile plan")
```

中文文字伪代码：

`pmaAF` 属于 PMA/访问 fault 语义，不能简单等价为 TLB entry `tlbAF` 或 DCache `denied`。第一版主表权限模式开启后，在主表 transaction 随机完成并进入 owner 构建前执行检查；如果发现主表 transaction 设置 `pmaAF=1`，直接 fatal，避免把 PMA fault 和 TLB/DCache response 错误混用。

## 11. 验证计划

静态检查：

```bash
git diff --check -- mem_ut/ver/ut/memblock AI_DOC
rg -n "MEMBLOCK_MAIN_PERMISSION|CROSS_4K|CROSS_CACHELINE|tlb2AF|corrupt2|dcache_owner" mem_ut/ver/ut/memblock AI_DOC
```

基础编译与默认行为：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
make eda_run tc=tc_sanity mode=base_fun
```

定向验证建议：

1. 所有新开关默认 0，确认默认 smoke 行为不变。
2. 开启主表权限和 TLB 子开关，非跨 4K 地址使用主 TLB 权限。
3. 开启跨 4K，构造跨页访问，确认 head VPN 使用主 TLB 权限，tail VPN 使用第二份 TLB 权限。
4. 开启主表权限和 DCache 子开关，非跨 cacheline 地址使用主 DCache 权限。
5. 开启跨 cacheline，构造跨 cacheline 访问，确认第一条 cacheline 使用 `corrupt/denied`，第二条 cacheline 使用 `corrupt2/denied2`。
6. 构造同 VPN 不同 TLB 权限冲突：手动表 fatal，随机表归一化。
7. 构造同 cacheline 不同 DCache 权限冲突：手动表 fatal，随机表归一化。
8. 构造 `load复用store / load复用load / store复用store / store复用load` 四类地址复用，确认随机表权限自动跟随 owner，手动表冲突 fatal。
9. 构造 hit 复用场景，确认后续同 VPN/cacheline 不期待 responder 再次下发，而是由 canonical owner 保证一致性。
10. 子开关矩阵：
    - `MAIN=1, TLB=1, DCACHE=0`：只覆盖 TLB 权限，不建立 DCache response override。
    - `MAIN=1, TLB=0, DCACHE=1`：建立 TLB 地址 owner 和确定性 PPN，但不覆盖 TLB 权限；DCache 按 line PA 覆盖 `corrupt/denied`。
    - `MAIN=1, TLB=1, DCACHE=1`：TLB 权限和 DCache 权限都由主表 owner 控制。
    - `MAIN=1, CROSS_4K=1, CROSS_CACHELINE=0`：允许 DCache 两条 cacheline，但 tail cacheline 仍使用主 `corrupt/denied`。

## 12. Coding Checklist

- [ ] 新增主表权限模式参数和 getter：`MEMBLOCK_MAIN_PERMISSION_EN`、`MEMBLOCK_MAIN_TLB_PERMISSION_EN`、`MEMBLOCK_MAIN_DCACHE_PERMISSION_EN`、`MEMBLOCK_MAIN_PERMISSION_DEBUG_CHECK_EN`。
- [ ] 新增跨界控制参数和 getter：`MEMBLOCK_MAIN_PERMISSION_CROSS_4K_EN`、`MEMBLOCK_MAIN_PERMISSION_CROSS_CACHELINE_EN`。
- [ ] `main_control_transaction` 新增 `tlb2AF/tlb2PF/tlb2GPF/PBMT2/corrupt2/denied2`，并补充中文注释。
- [ ] 主表构建阶段在主表权限模式下按跨界开关约束地址生成。
- [ ] 新增 TLB owner 和 DCache owner 表及构建函数。
- [ ] 新增 owner 冲突检查和随机表归一化逻辑。
- [ ] 主表地址复用路径同步权限复用：覆盖 load复用store、load复用load、store复用store、store复用load。
- [ ] L2TLB responder 在模式开启时按 VPN owner 覆盖 entry 权限。
- [ ] L2TLB hit debug check 受 `MEMBLOCK_MAIN_PERMISSION_DEBUG_CHECK_EN` 控制。
- [ ] DCache/SBuffer responder 在模式开启时按 line PA owner 覆盖 `corrupt/denied`。
- [ ] `pmaAF=1` 在主表权限模式下 fatal。
- [ ] 实现跨 4K 天然跨 cacheline 的特殊处理：`CROSS_4K_EN=1` 时允许 DCache parts 大于 1，第二份 DCache 权限是否启用由 `CROSS_CACHELINE_EN` 决定。
- [ ] 主表权限模式下选择确定性唯一 PPN 映射，并在构建期预计算 `line_pa`；runtime L2TLB 不做主流程回填。
- [ ] 明确第二份权限字段随机生成规则：默认复用主字段权重，directed/manual 可显式覆盖。
- [ ] 同步 `AI_DOC/mem_ut_flow_doc/tlb_l2tlb_responder_flow.md`。
- [ ] 同步 `AI_DOC/mem_ut_flow_doc/dcache_sbuffer_memory_responder_flow.md`。
- [ ] 同步参数管理相关说明和 default cfg。
- [ ] 完成静态检查、编译和基础仿真。

## 13. 复审修正项：当前 coding 前必须补清的语义

本节是 2026-06-28 对本 plan 重新梳理后的修正项。第 1 到第 12 章主线可用，但 coding 前必须遵守本节规则，否则仍可能出现同地址不同权限、跨界开关冲突或 PA alias 漏检。

### 13.1 跨 4K 与跨 cacheline 的关系

跨 4K 访问在 DCache 侧天然也会跨 cacheline，因为 4K page 边界按 64B cacheline 对齐。因此两个开关不是完全物理独立：

```text
CROSS_4K_EN=1:
  TLB parts 一定可能有两份 VPN。
  DCache parts 也可能有两份 cacheline。

CROSS_CACHELINE_EN=0:
  不表示禁止 cross_4K 产生的 DCache 两份 cacheline；
  只表示第二份 DCache 权限 corrupt2/denied2 不启用，第二条 cacheline 仍使用主 corrupt/denied。

CROSS_4K_EN=1 && CROSS_CACHELINE_EN=1:
  TLB tail 使用 tlb2AF/tlb2PF/tlb2GPF/PBMT2；
  DCache tail 使用 corrupt2/denied2。
```

中文文字伪代码：

跨 4K 是更强的跨界场景。只要地址跨 4K，DCache 看到的访问通常也跨过 64B cacheline。测试框架不能在 `CROSS_4K_EN=1` 且 `CROSS_CACHELINE_EN=0` 时因为 DCache parts 大于 1 就 fatal；正确行为是允许 DCache 拆成两条 cacheline，但第二条 cacheline 是否启用第二份 DCache 权限仍由 `CROSS_CACHELINE_EN` 决定。这样两个开关语义保持可控：一个控制 TLB 第二份权限，一个控制 DCache 第二份权限。

### 13.2 随机构建和手动表构建的处理时机

源码级伪代码：

```text
build_random_main_table():
  for uid in uid order:
    randomize tr permission fields
    if main_permission_en:
      allocate_main_permission_va(uid, tr)
      reserve owner immediately after VA accepted
    push tr into main table

import_manual_main_table():
  import all transactions first
  if main_permission_en:
    build owner table in uid order
    if same VPN/cacheline permission conflict:
      fatal
```

中文文字伪代码：

随机主表生成时，地址和权限是测试框架自己生成的，因此应按 uid 顺序边生成、边检查、边 reserve owner。这样后续 uid 能看到更老 uid 的 owner，并自动归一化地址复用产生的权限冲突。手动表导入时，用户已经给定地址和权限，测试框架不应静默改用户意图；应在导入完成后按 uid 顺序重建 owner 表，发现相同 VPN 或相同 cacheline 权限不一致时直接 fatal。

### 13.3 PA alias 与构建期 line_pa 预计算

当前 owner 构建阶段主要按 `line_va` 检查 DCache owner，但真实 DCache/memory responder 按 PA/cacheline 工作。如果两个不同 VA 最终翻译到同一个 PA cacheline，也会出现同一 cacheline 不同 `corrupt/denied` 的风险。

第一版明确采用策略 A：主表权限模式下使用确定性唯一 PPN 映射。策略 B 只作为后续可选增强，不进入本次 coding 主路径。

策略 A：主表权限模式下使用确定性唯一 PPN 映射。

```text
build_tlb_entry_for_owner(vpn):
  ppn = deterministic_unique_ppn(vpn)
```

中文文字伪代码：

主表权限模式开启时，L2TLB entry 的 PPN 不再完全随机，而是由 VPN 或 owner 派生出唯一 PPN，保证不同 VPN 默认不会映射到同一 PA cacheline。这样 line_va owner 和 line_pa owner 基本保持一一对应，降低 PA alias 风险。

后续可选策略 B：保留随机 PPN，但 runtime 获得 PA 后做二次检查。

```text
update_tlb_owner_ppn_for_random_ppn_mode(vpn, ppn):
  update line_pa for related dcache owners
  check_dcache_line_pa_owner_consistency()
```

中文文字伪代码：

如果后续继续允许随机 PPN，则每次 runtime 获得 PPN 后，都必须按 `line_pa` 重新检查 DCache owner。一旦发现不同 VA 映射到同一 PA cacheline 且 `corrupt/denied` 不一致，手动表 fatal，随机表把较新的 owner 归一化到最老 line_pa owner。否则 DCache responder 按 PA 查询时仍可能遇到同 PA 不同权限。第一版不实现该策略，第一版采用构建期预计算 `line_pa`。

### 13.4 第二份权限字段的生成和控制入口

新增的第二份权限字段必须有明确生成规则：

```text
main_permission_en=0:
  tlb2AF/tlb2PF/tlb2GPF/PBMT2/corrupt2/denied2 被忽略。

main_permission_en=1 && 对应跨界开关=0:
  第二份字段不参与 responder 行为；
  随机构建时建议默认复制主字段，减少 debug 噪声。

main_permission_en=1 && 对应跨界开关=1:
  第二份字段参与 tail fragment；
  手动表可以显式设置；
  随机表需要有独立权重或明确复用主字段权重。
```

建议第一版采用：

```text
TLB 第二份字段：
  默认复用主 TLB 字段权重随机。
  directed/manual case 可显式覆盖 tlb2AF/tlb2PF/tlb2GPF/PBMT2。

DCache 第二份字段：
  默认复用主 DCache 字段权重随机。
  directed/manual case 可显式覆盖 corrupt2/denied2。
```

中文文字伪代码：

第二份权限字段不能只定义字段而不定义生成方式。总开关关闭时它们完全无效；跨界开关关闭时它们也不应影响 responder。跨界开关开启时，tail fragment 读取第二份字段。随机主表可以先复用主字段权重生成第二份字段，保证不用新增过多 plus 参数；定向或手动表则可以直接写具体值，用于构造“第一次异常 / 第二次正常”或“第一次正常 / 第二次异常”的场景。

### 13.5 当前方案完善度结论

```text
已完善：
  主表权限总开关和默认关闭语义。
  TLB/DCache 子开关。
  跨 4K / 跨 cacheline 两个控制选项。
  主权限 / 第二份权限字段划分。
  地址复用四类场景的权限一致性。
  TLB owner 与 DCache owner 同时兼容的 VA 接受条件。
  随机表归一化、手动表 fatal 的基本策略。

已整合到主线且 coding 必须落实：
  子开关门控语义：TLB 权限覆盖、DCache 权限覆盖、TLB 地址 owner/PPN 需求分开处理。
  跨 4K 天然跨 cacheline 的特殊处理。
  随机/手动主表构建的不同处理时机。
  PPN 唯一映射接入 L2TLB responder 主线。
  构建期 line_pa owner 表预计算流程。
  第二份权限字段的随机/手动控制入口。
```

中文文字伪代码：

当前方案方向已经成立，并且 subagent 复审提出的子开关门控、PPN 主线接入、`line_pa` 表更新、跨界开关强制语义、helper 说明等关键点已经整合回主线章节。后续 coding 时应以第 1 到第 12 章主线流程为准，第 13 章作为复审摘要和约束索引使用。

### 13.6 后续确认项：当前方案剩余风险

以下问题不阻塞当前方案方向，但 coding 前需要确认或在实现中明确默认选择，避免后续实现分叉。

#### 13.6.1 DCache-only 场景是否受 CROSS_4K_EN 强制影响

当前主线语义：

```text
main_permission_en=1
TLB_PERMISSION_EN=0
DCACHE_PERMISSION_EN=1
CROSS_4K_EN=1
```

此时因为 DCache 权限需要 PA，框架仍会建立 TLB 地址 owner 和确定性 PPN。因此 `CROSS_4K_EN=1` 会让随机主表生成跨 4K 地址。

待确认：

```text
是否接受 DCache-only 场景也被 CROSS_4K_EN 强制构造跨 4K？

方案 A：接受。
  DCache-only 也可以验证跨 4K 自然带来的两条 cacheline 行为。

方案 B：不接受。
  CROSS_4K_EN 只在 TLB_PERMISSION_EN=1 时强制生效；DCache-only 只受 CROSS_CACHELINE_EN 控制。
```

建议默认：采用方案 A。原因是跨 4K 也会影响 DCache cacheline 拆分，DCache-only 场景保留该覆盖能力更完整。

#### 13.6.2 TLB addr owner 无权限登记语义

只开 DCache 子开关时，TLB addr owner 只用于确定性 PPN，不用于 TLB 权限检查。

coding 前需要明确：

```text
reserve_tlb_addr_owner_without_permission_check(owner):
  key = owner.vpn
  if tlb_owner_by_vpn.exists(key):
    复用已有 addr owner；
    不检查 tlbAF/tlbPF/tlbGPF/PBMT；
    不新增重复 owner。
  else:
    插入 VPN addr owner；
    ppn 后续由 deterministic_unique_ppn(vpn) 预计算。
```

中文文字伪代码：

该 helper 只表达“这个 VPN 需要稳定 PPN”，不表达 TLB 权限语义。同 VPN 多个 uid 共用同一个确定性 PPN；如果 TLB 子开关关闭，主表里的 TLB 权限字段不参与一致性检查，也不应被归一化。

#### 13.6.3 DCache owner map 建议统一存 owner_id

当前文档允许 `dcache_owner_by_line_va` / `dcache_owner_by_line_pa` 保存 owner 副本或 id。coding 时如果保存副本，容易出现 pool、VA map、PA map 三份状态不同步。

建议 coding 采用：

```systemverilog
memblock_dcache_addr_owner_t dcache_owner_pool[int unsigned];
int unsigned                 dcache_owner_id_by_line_va[bit [63:0]];
int unsigned                 dcache_owner_id_by_line_pa[bit [63:0]];
```

文字伪代码：

```text
查询 line_va:
  owner_id = dcache_owner_id_by_line_va[line_va]
  owner    = dcache_owner_pool[owner_id]

查询 line_pa:
  owner_id = dcache_owner_id_by_line_pa[line_pa]
  owner    = dcache_owner_pool[owner_id]
```

建议默认：正式实现使用 `owner_id`，不要存 owner 副本。

#### 13.6.4 deterministic_unique_ppn() 公式需要固定

当前文档只写了 `ppn_base + stable_hash_or_low_bits(vpn)`。coding 前需要把公式固定为可证明在当前主表规模内不碰撞的形式，并加 debug collision check。

建议第一版：

```text
deterministic_unique_ppn(vpn):
  ppn = MAIN_PERMISSION_PPN_BASE + compact_vpn_index(vpn)
```

其中 `compact_vpn_index(vpn)` 可以由 owner 构建期按首次出现 VPN 分配递增 index，而不是直接使用 hash。

文字伪代码：

```text
build deterministic ppn index:
  for each vpn in tlb_owner_by_vpn insertion order:
    if !ppn_index_by_vpn.exists(vpn):
      ppn_index_by_vpn[vpn] = next_ppn_index
      next_ppn_index++

ppn = ppn_base + ppn_index_by_vpn[vpn]
```

待确认：

```text
是否新增 MEMBLOCK_MAIN_PERMISSION_PPN_BASE plus 参数？

方案 A：新增参数，默认固定安全 base。
方案 B：不新增参数，第一版使用代码内固定 base。
```

建议默认：第一版可先不新增 plus 参数，使用代码内固定 base；如后续需要和其他地址空间避让，再参数化。

#### 13.6.5 precompute_main_permission_pa_owners() 调用时机

该函数必须在 DUT 可能发出 L2TLB/DCache request 前完成。

建议主表构建顺序明确为：

```text
build/randomize main table
apply address reuse and permission canonicalization
build/reserve TLB owner and DCache owner
precompute_main_permission_pa_owners()
mark main_table_ready
start LSQ enqueue / issue / responder service
```

中文文字伪代码：

`precompute_main_permission_pa_owners()` 不能放到 responder 第一次请求到来时懒执行。否则 DCache/SBuffer responder 可能先收到 PA 请求，却查不到 `dcache_owner_by_line_pa`。

#### 13.6.6 手动表跨界开关关闭时的跨界地址处理

当前文档说“手动表不强制改用户地址，只按实际地址是否跨界展开 parts”。还需要确认当跨界开关关闭但手动表给出跨界地址时如何处理。

待确认：

```text
手动表中 CROSS_4K_EN=0 但实际地址跨 4K：
  方案 A：fatal，要求用户打开 CROSS_4K_EN。
  方案 B：允许，按实际跨 4K 展开，但 tail 仍使用主 TLB 权限。

手动表中 CROSS_CACHELINE_EN=0 但实际地址跨 cacheline：
  方案 A：fatal，要求用户打开 CROSS_CACHELINE_EN。
  方案 B：允许，按实际跨 cacheline 展开，但 tail 仍使用主 DCache 权限。
```

建议默认：采用方案 B。原因是手动表的地址是用户显式意图，跨界开关只控制第二份权限是否启用，不应强制否定用户提供的地址；但如果 debug check 开启，可以打印 info/warning 提醒该跨界 tail 使用主权限。
