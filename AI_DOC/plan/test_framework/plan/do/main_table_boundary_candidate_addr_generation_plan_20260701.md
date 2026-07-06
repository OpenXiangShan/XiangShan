# 主表 boundary_profile 候选表与地址生成专项方案

本文从主表 `boundary_profile` 临时修改方案中拆出两类实现细节：

```text
1. profile/op_class/fuOpType 候选表生成策略。
2. final_vaddr + nonzero imm12 地址生成策略。
```

本文只描述测试框架如何生成有效激励，不实现 DUT 正确性 checker、RM 对比或 coveragent。

顶层使能原则：

```text
只有 MEMBLOCK_BOUNDARY_PROFILE_GEN_EN = 1 时，才启用本文的候选表和 boundary_profile 地址生成流程。

MEMBLOCK_BOUNDARY_PROFILE_GEN_EN = 0 时：
  不构造 profile_cache/op_cache/fuop_cache；
  不调用 generate_boundary_op_from_cache()；
  不调用 apply_boundary_addr_template()；
  主表仍按原测试框架逻辑生成，即继续走原 randomize_main_transaction() 中的
  apply_minimal_op_template() + apply_legal_addr_template() 等既有路径。

该开关用于保证非对齐/跨边界激励生成是显式 opt-in，默认不改变旧 smoke/regression 行为。
```

参数建议：

```text
MEMBLOCK_BOUNDARY_PROFILE_GEN_EN 默认值为 0。

0：保持原主表随机生成逻辑，不启用本文候选表和新地址生成。
1：启用 boundary_profile 候选表、profile-first 抽样和 final_vaddr 地址模板。
```

主表入口分支：

```text
randomize_main_transaction(tr):
  初始化 uid/rob/lq/sq/默认异常响应等原有字段。

  如果 MEMBLOCK_BOUNDARY_PROFILE_GEN_EN == 0：
    调用原 apply_minimal_op_template(tr)。
    调用原 apply_legal_addr_template(tr)。
    继续执行原 send_pri/delay/update_vaddr/validate 流程。
    return。

  如果 MEMBLOCK_BOUNDARY_PROFILE_GEN_EN == 1：
    确保 boundary candidate cache 已 build。
    调用 generate_boundary_op_from_cache(tr)。
    调用 apply_boundary_addr_template(tr, tr.boundary_profile, size_bytes)。
    调用 check_boundary_profile(tr)。
    继续执行 send_pri/delay/update_vaddr/validate 流程。
```

---

## 1. 候选表生成策略

候选表用于替代“op/fuOpType 随机失败后 retry/fallback”的旧流程。

核心原则：

```text
1. MEMBLOCK_BOUNDARY_PROFILE_GEN_EN 是顶层 gate；开关关闭时完全旁路本文流程。
2. build once, sample many：配置读取后先构造合法候选表，生成 transaction 时只查表抽样。
3. profile/op_class/fuOpType 选择阶段不靠 retry 兜底。
4. directed 通过设置单一 profile/op_class/fuOpType 权重实现，不需要 fixed_en/fixed_value 特殊分支。
5. 禁用某类激励必须通过 profile 权重、op_class 权重或 gate 控制，不能通过把某个 op_class/profile 下所有 fuOpType 权重清 0 实现。
6. 地址生成采用构造式模板，不使用 retry；防御性 check 失败直接 UVM_FATAL。
```

### 1.1 候选表结构

候选表采用分层动态数组，不要求固定二维/三维数组：

```text
profile_cache[]
  profile_entry
    profile
    profile_weight
    op_cache[]
      op_entry
        op_class
        op_class_weight
        fuop_cache[]
          fuop_entry
            fuOpType
            size_bytes
            cfg_fuop_weight
            effective_weight
            use_default
```

语义：

```text
每个 profile_entry 都有自己的 op_cache。
每个 op_entry 都有自己的 fuop_cache。

生成时按层索引：
  profile_idx = weighted_pick_index(profile_cache.weights)
  op_idx      = weighted_pick_index(profile_cache[profile_idx].op_cache.weights)
  fuop_idx    = weighted_pick_index(profile_cache[profile_idx].op_cache[op_idx].fuop_cache.effective_weights)

不合法组合不进入 cache。
因此生成阶段选到的 op_class 必然支持该 profile，选到的 fuOpType 必然支持该 profile + op_class。
```

### 1.2 build 阶段：构造动态候选表

触发时机：

```text
test/sequence 启动后；
plusarg/cfg 已完成读取；
MEMBLOCK_BOUNDARY_PROFILE_GEN_EN == 1；
正式生成 main transaction 之前。

如果 MEMBLOCK_BOUNDARY_PROFILE_GEN_EN == 0：
  跳过 build_boundary_candidate_cache()，保持原测试框架主表生成路径。
```

输入：

```text
MEMBLOCK_BOUNDARY_PROFILE_GEN_EN
MEMBLOCK_BOUNDARY_*_WT
op_class 权重
fuOpType 权重
MEMBLOCK_STORE_CROSS_8B_WITHIN_16B_EN
boundary_profile_supported_for_op()
boundary_profile_supported_for_fuop()
default_fuop_by_op_profile[op_class][profile]
```

文字伪代码：

```text
build_boundary_candidate_cache():
  如果 MEMBLOCK_BOUNDARY_PROFILE_GEN_EN == 0：
    不构造候选表，直接返回 BYPASS。

  初始化 profile_cache 为空。

  遍历所有 boundary_profile。
  读取 profile_weight。
  如果 profile_weight == 0：
    该 profile 被禁用，不加入 profile_cache。
    继续下一个 profile。

  创建 profile_entry(profile, profile_weight)。

  对该 profile 遍历所有 op_class。
  读取 op_class_weight。
  如果 op_class_weight == 0：
    该 op_class 被禁用，不加入 profile_entry.op_cache。
    继续下一个 op_class。
  如果 boundary_profile_supported_for_op(op_class, profile) 为 false：
    不加入 profile_entry.op_cache。
    继续下一个 op_class。
  如果 op_class 是 STORE 且 profile 是 CROSS_8B_WITHIN_16B，
     且 MEMBLOCK_STORE_CROSS_8B_WITHIN_16B_EN == 0：
    不加入 profile_entry.op_cache。
    继续下一个 op_class。

  创建 op_entry(op_class, op_class_weight)。

  枚举该 op_class 的原始 fuOpType 列表。
  对每个 fuOpType：
    推导 size_bytes。
    如果 boundary_profile_supported_for_fuop(op_class, fuOpType, profile, size_bytes) 为 false：
      丢弃该 fuOpType。
    如果返回 true：
      加入 legal_fuop_list。

  如果 legal_fuop_list 为空：
    该 op_class/profile 没有任何语义合法 fuOpType。
    因为 op_class 权重、profile 权重和 gate 已经放行该组合，
    该情况表示支持矩阵或 fuOpType 枚举表不一致。
    UVM_FATAL。

  读取 legal_fuop_list 中每个 fuOpType 的 cfg_fuop_weight。
  如果至少一个合法 fuOpType 的 cfg_fuop_weight > 0：
    只把 cfg_fuop_weight > 0 的 fuOpType 加入 op_entry.fuop_cache。
    对每个普通候选设置：
      cfg_fuop_weight = 原始配置权重；
      effective_weight = cfg_fuop_weight；
      use_default = 0。

  如果 legal_fuop_list 中所有 cfg_fuop_weight 都等于 0：
    这是配置错误，不表示禁用该 op_class/profile。
    打印 UVM_ERROR。
    读取 default_fuop_by_op_profile[op_class][profile]。
    default_fuOpType 必须同时满足：
      属于该 op_class 的原始 fuOpType 集合；
      能推导合法 size_bytes；
      boundary_profile_supported_for_fuop(op_class, default_fuOpType, profile, size_bytes) 为 true。
    任一条件失败则 UVM_FATAL。
    将 default_fuOpType 加入 op_entry.fuop_cache，设置 use_default = 1。
    default entry 的抽样策略：
      build 阶段为该 default entry 设置 effective_weight = 1。
    cfg_fuop_weight 保留原始配置权重 0，用于 dump/debug；
    weighted_pick_index() 只使用 effective_weight。

  如果 op_entry.fuop_cache 非空：
    将 op_entry 加入 profile_entry.op_cache。

  遍历完该 profile 的所有 op_class 后：
    如果 profile_entry.op_cache 为空：
      UVM_FATAL，指出 profile 权重非零但无合法 op_class/fuOpType 候选。
    否则将 profile_entry 加入 profile_cache。

  遍历完所有 profile 后：
    如果 profile_cache 为空：
      UVM_FATAL，要求配置至少打开一个可生成 boundary_profile。
```

建议 build 完成后打印一次 candidate cache 摘要，debug 开关关闭时不逐条 transaction 打印：

```text
profile / profile_weight
  op_class / op_class_weight
    fuOpType / size_bytes / cfg_fuop_weight / effective_weight / use_default
```

### 1.3 generate 阶段：只查表加权抽样

触发时机：

```text
每次生成一个 main_control_transaction 时，且 MEMBLOCK_BOUNDARY_PROFILE_GEN_EN == 1。

如果 MEMBLOCK_BOUNDARY_PROFILE_GEN_EN == 0：
  不进入本函数，主表按原测试框架逻辑生成。
```

输入：

```text
已经构造完成的 profile_cache。
```

文字伪代码：

```text
generate_boundary_op_from_cache(tr):
  如果 MEMBLOCK_BOUNDARY_PROFILE_GEN_EN == 0：
    UVM_FATAL。该函数不应在关闭开关时被调用。

  profile_idx = weighted_pick_index(profile_cache.weights)。
  profile_entry = profile_cache[profile_idx]。
  tr.boundary_profile = profile_entry.profile。

  op_idx = weighted_pick_index(profile_entry.op_cache.weights)。
  op_entry = profile_entry.op_cache[op_idx]。
  tr.op_class = op_entry.op_class。

  fuop_idx = weighted_pick_index(op_entry.fuop_cache.effective_weights)。
  fuop_entry = op_entry.fuop_cache[fuop_idx]。
  如果 fuop_entry.use_default == 1：
    记录本次使用 default fuOpType。
  tr.fuOpType = fuop_entry.fuOpType。
  size_bytes = fuop_entry.size_bytes。

  调用已有 helper 派生 fuType、lsq_flow、numLsElem 和其他 op_class 相关字段。
  本函数只负责选择并填写 profile/op_class/fuOpType/size 相关字段。
  不生成地址，不调用 apply_boundary_addr_template()。
  不调用 check_boundary_profile()。
  返回 size_bytes 和 use_default 状态给主入口。
```

generate 阶段禁止：

```text
1. 禁止在 MEMBLOCK_BOUNDARY_PROFILE_GEN_EN=0 时调用本文流程。
2. 禁止遍历全集寻找新的 op_class/fuOpType 候选。
3. 禁止用 retry 处理 op_class/fuOpType 不合法。
4. 禁止 fallback 到 ALIGNED。
5. 禁止改写 boundary_profile。
```

### 1.4 weighted_pick_index(weights[$])

本方案使用动态候选表，`std::randomize() with dist` 和 `randcase` 不适合动态展开候选项。因此主路径统一使用一个小 helper：

```text
weighted_pick_index(weights[$])
```

该 helper 只负责按权重返回 index，不做合法性过滤，不理解 profile/op_class/fuOpType 语义。

示例：

```systemverilog
int unsigned weights[$];
int profile_idx;

weights.delete();
foreach (profile_cache[i]) begin
    weights.push_back(profile_cache[i].profile_weight);
end

profile_idx = weighted_pick_index(weights);
selected_profile = profile_cache[profile_idx].profile;
```

helper 逻辑：

```systemverilog
function int weighted_pick_index(input int unsigned weights[$]);
    int unsigned total;
    int unsigned r;
    int unsigned acc;

    total = 0;
    foreach (weights[i]) begin
        total += weights[i];
    end

    if (total == 0) begin
        `uvm_fatal("WEIGHT_PICK", "total weight is zero")
    end

    r = $urandom_range(total - 1, 0);

    acc = 0;
    foreach (weights[i]) begin
        acc += weights[i];
        if (r < acc) begin
            return i;
        end
    end

    `uvm_fatal("WEIGHT_PICK", "unreachable weighted pick state")
    return -1;
endfunction
```

`weights[$]` 的顺序必须和候选 cache 的顺序一致。helper 返回 index 后，必须回到对应 cache 读取真正的 profile/op_class/fuOpType。

固定枚举、候选项不变的简单场景可以使用 `std::randomize() with dist`。本方案主路径因为候选数量随配置和支持矩阵变化，使用统一 `weighted_pick_index()` 更直接。

### 1.5 fuOpType 权重全 0 的处理

```text
如果某个 op_class/profile 下存在合法 fuOpType，
但这些合法 fuOpType 的配置权重全部为 0：
  这是配置错误。
  打印 UVM_ERROR，指出 op_class/profile 下合法 fuOpType 权重全 0。
  选择 default_fuop_by_op_profile[op_class][profile] 继续生成。
  default 候选的 cfg_fuop_weight 保留为 0；
  default 候选的 effective_weight 必须设置为 1，供 weighted_pick_index() 使用。

default fuOpType 必须同时满足：
  属于该 op_class 的原始 fuOpType 集合；
  能推导合法 size_bytes；
  boundary_profile_supported_for_fuop(op_class, default_fuOpType, profile, size_bytes) 为 true。
任一条件失败则 UVM_FATAL。
```

语义说明：

```text
fuOpType 权重全 0 不表示禁用该 op_class/profile。
它表示用户配置存在问题。
实现不能因为 fuOpType 权重全 0 就把该 op_class/profile 当作被禁用并静默丢弃。

禁用某类激励应使用：
  MEMBLOCK_BOUNDARY_*_WT = 0
  op_class 权重 = 0
  MEMBLOCK_STORE_CROSS_8B_WITHIN_16B_EN = 0
```

职责分层：

```text
profile 权重、op_class 权重和 gate 决定“某类组合是否允许生成”。
fuOpType 权重只决定“在已经允许生成的 op_class/profile 组合内部，选择哪个具体 fuOpType”。

因此 fuOpType 权重不是组合级禁用开关。
如果组合已经被 profile/op_class/gate 放行，但该组合下所有合法 fuOpType 权重为 0，
说明配置上下层语义冲突，应按配置错误处理。
```

示例：

```text
如果想禁用 STORE x CROSS_8B_WITHIN_16B：
  推荐使用 MEMBLOCK_STORE_CROSS_8B_WITHIN_16B_EN = 0；
  或关闭 STORE op_class 权重；
  或关闭 MEMBLOCK_BOUNDARY_CROSS_8B_WITHIN_16B_WT。

不允许通过“把 STORE x CROSS_8B_WITHIN_16B 下所有 store fuOpType 权重设为 0”
来表达禁用。该配置会触发 UVM_ERROR，然后选择 default legal store fuOpType 继续生成。
```

默认 fuOpType 表要求：

```text
每个可能触发 all-zero default recovery 的可生成 op_class/profile 组合必须有一个 default legal fuOpType。
default fuOpType 必须属于该 op_class 原始 fuOpType 集合，能推导合法 size_bytes，并经过 boundary_profile_supported_for_fuop() 校验。
default fuOpType 只能保证该 op_class/profile 组合仍有确定候选，不能静默掩盖配置错误；因此必须配套 UVM_ERROR。
default fuOpType 的选择路径必须绕开“全 0 权重”问题：cfg_fuop_weight 保留 0，effective_weight 设置为 1。
```

### 1.6 性能说明

候选表 build 阶段只执行一次：

```text
O(profile_count * op_class_count * fuOpType_count)
```

当前规模大约是：

```text
6 * 6 * 几十个 fuOpType
```

每条 transaction 生成阶段只做三次加权抽样：

```text
O(profile_cache.size + selected_profile.op_cache.size + selected_op.fuop_cache.size)
```

实际开销远小于 UVM transaction、driver、DUT 仿真、TLB 响应和波形记录。实现注意点：

```text
1. candidate cache 只 build 一次。
2. candidate cache dump 只在启动时打印一次，或受 debug 开关控制。
3. weighted_pick_index() 不打印日志。
4. 地址生成防御性 check 只记录 pass/fatal 状态，不逐次打印噪声日志。
5. 如果后续候选规模明显变大，可在 cache entry 中预存 total_weight，避免每次 pick 重算。
```

## 2. 为什么地址生成不使用 retry

op/fuOpType 选择是离散候选问题，可以通过候选表做到确定可选：

```text
profile -> op_class -> fuOpType/size
```

地址生成也应采用构造式模板：

```text
profile + size_bytes -> 合法 final_vaddr 范围 -> 合法 page/line/bank/k/offset -> final_vaddr
final_vaddr + nonzero imm12 -> 合法 src_0
```

因此规则正确时，一次生成就应满足目标 `boundary_profile`。
规则错误时，继续 retry 只会掩盖模板或约束 bug。

第一版 Sv39 正 canonical 和 no-wrap 必须由采样范围保证：

```text
final_vaddr < 2^39
end_vaddr = final_vaddr + size - 1 < 2^39
end_vaddr >= final_vaddr
```

各 profile 的 page/line/bank/k/offset 也必须由模板范围保证：

```text
CROSS_16B_SAME_LINE: bank 只能在不会跨 64B 的范围内选。
CROSS_CACHELINE_SAME_4K: line 不能选到会跨 4K 的最后一条 line。
CROSS_4K: page_base 不能选 Sv39 正 canonical 空间最后一个无法容纳 tail 的 page。
```

`src_0 + imm12` 拆分也必须由约束保证：

```text
imm12 != 0
imm_sext = sign_extend_imm12(imm12)
src_0 = final_vaddr - imm_sext 不回绕
update_vaddr() 后 tr.vaddr == final_vaddr
```

防御性检查只用于发现规则或实现错误：

```text
如果 final_vaddr/end_vaddr 非 Sv39 正 canonical：UVM_FATAL。
如果 end_vaddr 回绕：UVM_FATAL。
如果 update_vaddr() 后 tr.vaddr != final_vaddr：UVM_FATAL。
如果 classify_boundary_profile() != 目标 boundary_profile：UVM_FATAL。
```

结论：

```text
不新增 MEMBLOCK_BOUNDARY_ADDR_GEN_RETRY_MAX。
地址生成不 retry。
check 失败说明模板规则、约束或实现需要修改，直接 UVM_FATAL。
不 fallback 到 ALIGNED。
不改写 boundary_profile。
```

## 3. 当前实现问题

当前 `apply_legal_addr_template()` 的地址来源是物理窗口：

```systemverilog
base  = seq_csr_common::get_paddr_base();
range = seq_csr_common::get_paddr_range();

tr.src_0 = aligned_base + (slot_pick << 6);
tr.imm   = 64'h0;
tr.update_vaddr();
```

而 `update_vaddr()` 是：

```systemverilog
vaddr = src_0 + sign_extend_imm12(imm);
```

所以当前等价于：

```text
vaddr = 从物理窗口中选出的 64B 对齐地址
imm   = 0
```

这隐含 identity-like 思路，不符合当前环境定义：虚拟地址不来自物理窗口，L2TLB 会把虚拟地址映射到合法物理地址。

---

## 4. 新地址生成模型

核心流程：

```text
1. 根据 boundary_profile + size 生成目标 final_vaddr。
   final_vaddr 是 RTL 看到的 effective address / virtual address。

2. final_vaddr 第一版限制在 Sv39 正 canonical 区域：
   final_vaddr[63:39] == 0
   即 final_vaddr < 2^39。

3. 不从 MEMBLOCK_PADDR_BASE/PADDR_RANGE 采样 final_vaddr。
   地址生成侧不检查、不推导合法物理地址；物理 backing 由 L2TLB/page backing 组件负责。

4. 随机生成非 0 imm12。
   imm_sext = sign_extend_imm12(imm12)。

5. 反推：
   src_0 = final_vaddr - imm_sext。

6. 调用 update_vaddr 后必须满足：
   tr.vaddr == final_vaddr。

7. 最后做 classify/check：
   final_vaddr + size - 1 不回绕；
   final_vaddr/end_vaddr 均满足 Sv39 正 canonical；
   profile 分类结果等于目标 boundary_profile。

CROSS_4K 的 head/tail 物理 backing 不属于本文地址生成检查范围，
也不作为地址生成侧的检查或错误处理条件。
```

这样 `imm` 不是恒 0，同时还能精确制造目标非对齐模式。

---

## 5. final_vaddr 模板

这里是“模板约束随机”，不是纯随机加检查。模板必须只在合法范围内采样；
如果某个 `profile/size` 不存在合法模板范围，`boundary_profile_supported_for_fuop()` 必须提前过滤掉该组合。

公共约束：

```text
vaddr_limit = 2^39
end_vaddr = final_vaddr + size - 1
要求 end_vaddr < vaddr_limit
要求 end_vaddr >= final_vaddr
```

各 profile 的构造范围：

```text
ALIGNED:
  普通 load/store final_vaddr = random size_bytes aligned vaddr
  PREFETCH/CBO whole-line 操作 size_bytes=64，因此 final_vaddr = random 64B aligned vaddr
  要求 final_vaddr % size == 0
  要求 end_vaddr 不跨 16B/64B/4K

MISALIGN_WITHIN_8B:
  合法 size: 2/4
  base8 = random 8B aligned vaddr
  offset in [1, 8 - size]
  额外要求 offset % size != 0
  final_vaddr = base8 + offset
  该范围保证 !aligned 且 end_vaddr 仍在同一个 8B block 内

CROSS_8B_WITHIN_16B:
  合法 size: 2/4/8
  base16 = random 16B aligned vaddr
  k in [max(1, size - 8), size - 1]
  final_vaddr = base16 + 8 - k
  该范围保证跨 8B，但 end_vaddr 仍在同一个 16B block 内

CROSS_16B_SAME_LINE:
  合法 size: 2/4/8
  line_base = random 64B aligned vaddr
  bank in [0, 1, 2]
  k in [1, size - 1]
  final_vaddr = line_base + bank * 16 + 16 - k
  额外要求 final_vaddr + size - 1 < line_base + 64
  该范围保证跨 16B，但不跨 64B cacheline

CROSS_CACHELINE_SAME_4K:
  合法 size: 2/4/8
  page_base = random 4K aligned vaddr
  line in [0, 62]
  k in [1, size - 1]
  final_vaddr = page_base + line * 64 + 64 - k
  额外要求 final_vaddr + size - 1 < page_base + 4096
  该范围保证跨 64B，但不跨 4K

CROSS_4K:
  合法 size: 2/4/8
  page_base = random 4K aligned vaddr
  k in [1, size - 1]
  final_vaddr = page_base + 4096 - k
  要求 page_base + 4096 + size - k - 1 < vaddr_limit
  该范围保证跨 4K，且 tail 仍在 Sv39 正 canonical 范围内
```

说明：

```text
MISALIGN_WITHIN_8B 不支持 size=8，因为不存在既 misalign 又不跨 8B 的 8B 访问；
size=2/4 时 offset 还必须满足 offset % size != 0，保证自然非对齐。
CROSS_8B_WITHIN_16B 的 k 下界必须使用 max(1, size - 8)，避免 size 扩展后跨 16B。
CROSS_16B_SAME_LINE / CROSS_CACHELINE_SAME_4K 必须用 end_vaddr 约束防止升级到更大边界 profile。
CROSS_4K 不能选择 Sv39 正 canonical 空间最后一个无法容纳 tail 的 page_base。
```

---

## 6. PADDR window 用法

本文地址生成侧不使用 `MEMBLOCK_PADDR_BASE/RANGE` 采样、过滤或检查 `final_vaddr`。

职责边界：

```text
boundary_profile 地址生成：
  只负责生成 final_vaddr/src_0/imm，并保证 vaddr/end_vaddr 命中目标 profile。

L2TLB/page backing 组件：
  负责虚拟地址到合法物理地址的 backing。
```

因此本文不检查：

```text
head_vaddr/tail_vaddr 是否能 build TLB entry；
choose_paddr(head_vaddr) 是否落在 PADDR_BASE/RANGE 内；
choose_paddr(tail_vaddr) 是否落在 PADDR_BASE/RANGE 内；
head/tail PPN 是否连续或同属性。
```

这些检查不属于本地址生成方案；也不作为地址生成侧的检查或错误处理条件。

## 7. 推荐实现形态

```text
apply_boundary_addr_template(tr, boundary_profile, size_bytes):

  如果 MEMBLOCK_BOUNDARY_PROFILE_GEN_EN == 0：
    UVM_FATAL。该函数不应在关闭开关时被调用；
    关闭时应走原 apply_legal_addr_template()。

  final_vaddr = gen_final_vaddr_by_profile(boundary_profile, size_bytes)

  // gen_final_vaddr_by_profile() 必须通过范围约束保证 Sv39 正 canonical 和 no-wrap。
  end_vaddr = final_vaddr + size_bytes - 1
  if add_overflow or !is_sv39_positive_canonical(final_vaddr) or !is_sv39_positive_canonical(end_vaddr):
    UVM_FATAL  // 模板约束 bug

  imm12 = random_nonzero_imm12_constrained(final_vaddr)
  imm_sext = sign_extend_imm12(imm12)

  if final_vaddr - imm_sext underflow/overflow:
    UVM_FATAL  // imm/src_0 约束 bug

  src_0 = final_vaddr - imm_sext

  tr.src_0 = src_0
  tr.imm   = imm12
  tr.update_vaddr()

  if tr.vaddr != final_vaddr:
    UVM_FATAL  // src_0/imm 拆分 bug

  if classify(tr.vaddr, size_bytes) != boundary_profile:
    UVM_FATAL  // profile 模板或支持矩阵 bug

  return success
```

结论：

```text
地址生成应以虚拟 final_vaddr 的 boundary_profile 为主。
PADDR_BASE/RANGE 不参与 final_vaddr 采样、过滤或检查。
imm 用随机非零 imm12，再反推 src_0。
这样既满足 RTL 非对齐路径构造，也避免 identity-like 地址假设。
```

## 执行中补充/修正（IMPLEMENTATION_DELTA）

[IMPLEMENTATION_DELTA]
来源：coding 中检查当前源码参数体系后发现，本 plan 描述了 `fuOpType 权重` 语义，但当前公共 plus/cfg 尚无逐 fuOpType 权重参数命名和既有 getter。
原 plan：候选表中每个 `fuOpType` 读取 `cfg_fuop_weight`，当某个 `op_class/profile` 下所有合法 `fuOpType` 权重为 0 时打印 `UVM_ERROR`，再使用 `default_fuop_by_op_profile`。
实现调整：第一版 `get_fuop_weight(op_class, fuOpType)` 对所有语义合法候选返回 1，因此合法 `fuOpType` 在同一 `op_class/profile` 内等权随机；保留 `cfg_fuop_weight/effective_weight/use_default` 字段和 all-zero/default 处理框架，后续只需补具体 plus 参数和 getter 即可启用逐 fuOpType 权重。
原因：避免在本 plan 中临时发明大量未评审的 fuOpType 级 plus 参数；同时候选表结构已经给后续扩展留好接口。
影响范围：`memblock_dispatch_base_sequence.sv::get_fuop_weight()` 当前返回 1；本轮验证不覆盖 fuOpType 权重全 0 的 `UVM_ERROR + default` 运行路径，因为没有可配置入口能制造该场景。

[IMPLEMENTATION_DELTA]
来源：coding 中发现既有 `apply_addr_reuse_window()` 会复制历史 transaction 地址，可能破坏本 plan 已经构造出的目标 `boundary_profile`。
原 plan：顶层入口只说明开关关闭时走旧 `apply_legal_addr_template()`，开关打开时走 `apply_boundary_addr_template()`。
实现调整：当 `MEMBLOCK_BOUNDARY_PROFILE_GEN_EN=1` 时，`build_random_main_table()` 不调用 `prune_recent_uid_q()` 和 `apply_addr_reuse_window()`；当开关为 0 时保持旧地址复用窗口逻辑。
原因：本 plan 第一版目标是精确生成所选 `boundary_profile`，地址复用属于另一个 directed 地址相关机制，会改变最终 `src_0/imm/vaddr`，导致标签和实际激励不一致。
影响范围：boundary 模式下不再生成 load-after-store/load-after-load/store-after-store 地址复用关系；默认模式不受影响。

[IMPLEMENTATION_DELTA]
来源：coding 中为了避免 CBO 被普通 STORE 规则误处理，将 CBO 从 STORE 中拆成独立 `op_class`。
原 plan：PREFETCH/CBO 第一版只做 `ALIGNED` 合法性校验，不参与普通 load/store size/classify；未限定 CBO 必须复用 STORE op_class。
实现调整：新增 `MEMBLOCK_OP_CLASS_CBO` 和 `MEMBLOCK_OP_CLASS_CBO_WT`，默认权重为 0；`MEMBLOCK_OP_CLASS_STORE` 只表示普通 store，CBO 走 `MEMBLOCK_LSQ_FLOW_CBO`，并且只支持 `ALIGNED` profile。
原因：CBO RTL 行为是 whole-line 操作，和普通 store 非对齐/跨 16B/跨 cacheline 路径不同；拆成独立 op_class 能让候选表和 validate 逻辑更清晰。
影响范围：默认配置 `MEMBLOCK_OP_CLASS_CBO_WT=0`，旧 smoke 默认行为不生成 CBO；需要 CBO aligned 激励时显式打开该权重。

[IMPLEMENTATION_DELTA]
来源：coding 中复查 `ALIGNED` 模板时发现，原 plan 中 `ALIGNED: random 64B aligned vaddr` 容易误导普通 load/store aligned 激励全部变成 cacheline 对齐。
原 plan：`ALIGNED` 模板写成 64B 对齐，同时要求 `final_vaddr % size == 0`。
实现调整：普通 load/store 的 `ALIGNED` 按 `size_bytes` 自然对齐生成；当 `size_bytes >= 64` 时才按 64B 对齐。PREFETCH/CBO 第一版 `derive_size_bytes()` 返回 64，因此它们仍会生成 64B 对齐 whole-line 地址。
原因：普通 load/store 的对齐语义是访问 size 自然对齐，不应强制所有 aligned load/store 都落到 cacheline 起始地址；否则会降低普通 aligned 地址分布粒度，也和 `classify_boundary_profile()` 的自然对齐判断不一致。
影响范围：`gen_final_vaddr_by_profile(ALIGNED)` 使用 `align_bytes = (size_bytes >= 64) ? 64 : size_bytes`；非对齐和跨边界 profile 不受影响。
