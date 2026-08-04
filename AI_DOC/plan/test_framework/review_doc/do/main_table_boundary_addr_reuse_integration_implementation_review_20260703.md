# main table boundary addr reuse integration implementation review

关联 plan：

```text
AI_DOC/plan/test_framework/plan/do/main_table_boundary_addr_reuse_integration_formal_plan_20260703.md
```

本文 review 本轮 coding 是否按正式 plan 落地。review 范围只覆盖测试框架激励生成、参数控制、地址复用和 flow 文档同步；不覆盖 DUT checker、RM、scoreboard 或 covergroup。

## 1. 修改结论

本轮实现已落地以下功能：

```text
1. normal 和 boundary 两条主表生成路径共用 fuOpType plus 权重。
2. boundary candidate cache 支持 op_class/profile 下合法 fuOpType 权重全 0 时 UVM_ERROR + default fuOpType。
3. build_random_main_table() 在 normal/boundary 模式下都调用 apply_addr_reuse_window()。
4. boundary 模式地址复用后调用 sync_boundary_profile_after_addr_reuse() 同步最终 boundary 标签。
5. 地址复用命中后支持 keep_ref_size 权重；命中时按 ref_size 在 target_op_class 内选择同 size fuOpType。
6. PREFETCH ref_tr 不进入 keep_ref_size 路径。
7. flow 文档和 plus 参数管理规则文档已同步。
```

## 2. 参数入口

源码位置：`mem_ut/ver/ut/memblock/env/plus.sv`，参数定义。

函数功能简析：`plus.sv` 是 runtime plusarg 输入解析层，新增参数在这里定义默认值并进入命令行读取路径。

```systemverilog
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_LOAD_FUOP_LB_WT, int, 1)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_STORE_FUOP_SD_WT, int, 1)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_PREFETCH_FUOP_R_WT, int, 1)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_CBO_FUOP_CLEAN_WT, int, 1)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_AMO_FUOP_AMOADD_D_WT, int, 1)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ADDR_REUSE_KEEP_REF_SIZE_EN_1_WT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ADDR_REUSE_KEEP_REF_SIZE_EN_0_WT, int, 1)
```

中文伪代码：

```text
该参数定义组为主表激励生成提供新的 directed 控制入口。
LOAD/STORE/PREFETCH/CBO/AMO 的 fuOpType 权重默认都是 1，表示不改变旧默认随机分布。
keep_ref_size 的 1 权重默认是 0，0 权重默认是 1，表示默认关闭同 size 复用增强，保持旧地址复用行为。
这些参数由 plus.sv 命令行读取逻辑加载，随后由 seq_csr_common 缓存并提供 getter。
```

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`，函数：`get_fuop_weight()`。

函数功能简析：该函数是 sequence 读取 fuOpType 权重的正式入口，把 op_class/fuOpType 映射到对应 plus 配置值。

```systemverilog
static function int unsigned get_fuop_weight(input memblock_op_class_e op_class,
                                             input bit [8:0] fuOpType);
    check_initialized("get_fuop_weight");
    case (op_class)
        MEMBLOCK_OP_CLASS_INT_LOAD,
        MEMBLOCK_OP_CLASS_FP_LOAD: begin
            case (fuOpType)
                MEMBLOCK_LSUOP_LB:  return load_fuop_lb_wt;
                MEMBLOCK_LSUOP_LH:  return load_fuop_lh_wt;
                MEMBLOCK_LSUOP_LW:  return load_fuop_lw_wt;
                MEMBLOCK_LSUOP_LD:  return load_fuop_ld_wt;
                default:            return 0;
            endcase
        end
        MEMBLOCK_OP_CLASS_STORE: begin
            case (fuOpType)
                MEMBLOCK_LSUOP_SB: return store_fuop_sb_wt;
                MEMBLOCK_LSUOP_SH: return store_fuop_sh_wt;
                MEMBLOCK_LSUOP_SW: return store_fuop_sw_wt;
                MEMBLOCK_LSUOP_SD: return store_fuop_sd_wt;
                default:           return 0;
            endcase
        end
        default: begin
            return 0;
        end
    endcase
endfunction:get_fuop_weight
```

中文伪代码：

```text
该函数先检查 seq_csr_common 已完成初始化，避免 sequence 在 plus 配置加载前读取权重。
如果 op_class 是 INT_LOAD 或 FP_LOAD，就根据具体 load fuOpType 返回对应 LOAD_FUOP 权重；不属于 load 枚举的 fuOpType 返回 0。
如果 op_class 是 STORE，就根据 store fuOpType 返回对应 STORE_FUOP 权重；不属于 store 枚举的 fuOpType 返回 0。
PREFETCH、CBO、AMO 在源码中也按同样方式映射到各自权重；这里摘录 load/store 证明核心路径。
返回 0 表示该 fuOpType 不参与本次 weighted pick。
```

正确性检查：

```text
default.cfg、plus.sv、seq_csr_common.sv 三处新增参数 key 已做一致性检查，缺失输出为空。
normal 模式下，如果某个 op_class 权重非 0 且该类 fuOpType 权重全 0，validate_and_clamp() 会 UVM_FATAL。
boundary 模式下，op_class/profile 下合法 fuOpType 权重全 0 留给 candidate cache 做 UVM_ERROR + default。
```

## 3. 主表生成与地址复用

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`，task：`build_random_main_table()`。

函数功能简析：这是随机主表构建入口，负责建 boundary cache、逐 uid 生成 transaction、执行地址复用、写入公共主表。

```systemverilog
if (seq_csr_common::get_boundary_profile_gen_en()) begin
    build_boundary_candidate_cache();
end else begin
    boundary_candidate_cache_built = 1'b0;
    boundary_profile_cache.delete();
end

for (int unsigned idx = 0; idx < main_trans_num_i; idx++) begin
    uid = data.alloc_uid();
    tr = main_control_transaction::type_id::create($sformatf("main_uid_%0d", uid));
    randomize_main_transaction(tr, uid, rob_key);
    prune_recent_uid_q(recent_load_uid_q, uid, addr_ref_window);
    prune_recent_uid_q(recent_store_uid_q, uid, addr_ref_window);
    apply_addr_reuse_window(tr, uid, recent_load_uid_q, recent_store_uid_q);
    if (seq_csr_common::get_boundary_profile_gen_en()) begin
        sync_boundary_profile_after_addr_reuse(tr,
            $sformatf("random uid=%0d boundary after addr reuse", uid));
    end
    data.set_main_transaction(uid, tr);
    push_recent_uid(tr, uid, recent_load_uid_q, recent_store_uid_q);
end
```

中文伪代码：

```text
该 task 先根据 boundary 开关决定是否构建 boundary candidate cache。
如果 boundary 开关关闭，就清空候选表和 built 标志，避免旧 cache 污染 normal 路径。
循环中每次分配一个新 uid，并创建对应 main_control_transaction。
randomize_main_transaction() 先生成初始 op、地址、优先级、delay 和 ROB 信息；boundary 模式下初始 op/地址来自 candidate cache 和 boundary 地址模板。
随后无论 normal 还是 boundary 模式，都清理 recent load/store uid queue，再调用 apply_addr_reuse_window() 尝试制造地址相关激励。
如果 boundary 模式打开，地址复用后调用 sync_boundary_profile_after_addr_reuse()，把 boundary 标签更新为最终 transaction 的真实 vaddr/size 分类。
最后把最终 transaction 写入 common_data_transaction 主表，并把当前 uid 放入 recent queue 供后续复用。
```

修改前逻辑：

```text
boundary 模式跳过 apply_addr_reuse_window()，因此 MEMBLOCK_ADDR_REUSE_EN_* 在 boundary 模式下不生效。
```

修改后逻辑：

```text
normal 和 boundary 模式都调用 apply_addr_reuse_window()。
boundary 模式在复用后只同步标签，不恢复旧 profile，不重做地址模板。
```

## 4. keep_ref_size 地址复用

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`，函数：`apply_addr_reuse_window()`。

函数功能简析：该函数按地址复用权重选择是否复用、按 kind 选择参考队列，并在选到 ref_tr 后决定是否保持参考 transaction 的访问 size。

```systemverilog
keep_ref_size = rand_weighted2(seq_csr_common::get_addr_reuse_keep_ref_size_en_1_wt(),
                               seq_csr_common::get_addr_reuse_keep_ref_size_en_0_wt()) == 0;

if (!keep_ref_size || ref_tr.op_class == MEMBLOCK_OP_CLASS_PREFETCH) begin
    tr.op_class = target_op_class;
    apply_minimal_op_template(tr);
    fixup_after_addr_reuse(tr, ref_tr, 1'b1, reuse_caller);
    return;
end

ref_size = derive_size_bytes(ref_tr.op_class, ref_tr.fuOpType);
target_fuOpType = choose_fuop_by_op_class_and_size(target_op_class, ref_size, reuse_caller);
tr.op_class = target_op_class;
apply_op_class_template(tr, target_fuOpType);
fixup_after_addr_reuse(tr, ref_tr, 1'b1, reuse_caller);
```

中文伪代码：

```text
该分支只在地址复用已经命中并且成功取得 ref_tr 后执行。
先用 KEEP_REF_SIZE_EN_1_WT 和 KEEP_REF_SIZE_EN_0_WT 随机决定是否保持参考 size。
如果 keep_ref_size 未命中，或者 ref_tr 是 PREFETCH，就走旧地址复用路径：在目标 op_class 内按 fuOpType 权重重新选操作，然后复制 ref_tr 的 src_0/imm。
PREFETCH 被排除，是因为框架把 PREFETCH/CBO 作为整 cacheline 分类，不作为普通 load/store 同 size 复用参考。
如果 keep_ref_size 命中且 ref_tr 不是 PREFETCH，就从 ref_tr.op_class/ref_tr.fuOpType 派生 ref_size。
随后 choose_fuop_by_op_class_and_size() 在 target_op_class 里筛选 size 等于 ref_size 的合法 fuOpType，并按 fuOpType 权重选择。
apply_op_class_template() 用选出的 target_fuOpType 写入 fuType、lsq_flow、fuOpType、numLsElem。
fixup_after_addr_reuse() 复制参考地址并重新计算 vaddr，同时检查最终 transaction 自洽。
```

正确性检查：

```text
keep_ref_size 不直接 copy ref_tr.fuOpType，因此 STORE->LOAD 或 LOAD->STORE 复用不会把不属于目标 op_class 的 fuOpType 写入 transaction。
如果目标 op_class 没有 ref_size 对应的合法 fuOpType，choose_fuop_by_op_class_and_size() 会 UVM_FATAL。
如果同 size 合法候选权重全 0，会 UVM_ERROR 后使用该 op_class/size 的 default fuOpType，并校验 membership 和 size。
```

## 5. boundary 标签同步

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`，函数：`sync_boundary_profile_after_addr_reuse()`。

函数功能简析：该函数只在 boundary 模式下执行，用最终 transaction 的 op 和地址重新计算 boundary 标签。

```systemverilog
size_bytes = derive_size_bytes(tr.op_class, tr.fuOpType);
if (size_bytes == 0) begin
    `uvm_fatal(get_type_name(), ...)
end

tr.update_vaddr();
actual_profile = classify_boundary_profile(tr.vaddr, size_bytes);
if (actual_profile == MEMBLOCK_BOUNDARY_PROFILE_UNKNOWN) begin
    `uvm_fatal(get_type_name(), ...)
end

tr.boundary_size_bytes = size_bytes;
tr.boundary_profile = actual_profile;
validate_main_table_entry(tr, caller);
```

中文伪代码：

```text
该函数先从最终 tr.op_class/tr.fuOpType 派生最终访问 size。
如果 size 为 0，说明地址复用后的 op 模板非法，直接 fatal。
然后调用 tr.update_vaddr()，用最终 src_0/imm 重新计算 vaddr。
classify_boundary_profile() 用最终 vaddr 和 size 重新判断 ALIGNED、MISALIGN、跨 16B、跨 cacheline 或跨 4K。
如果分类失败，说明最终地址或 size 不自洽，直接 fatal。
函数写回 boundary_size_bytes 和 boundary_profile，使主表标签表达最终真实激励形态。
最后调用 validate_main_table_entry() 检查 fuType、lsq_flow、fuOpType、numLsElem 等主表字段是否合法。
```

正确性检查：

```text
sync 不修复、不 fallback、不重新生成地址。
地址复用后 profile 从 CROSS_4K 变 ALIGNED 是允许的，标签会记录最终形态。
directed case 如需保持初始 boundary profile，必须关闭地址复用。
```

## 6. Flow 与规则文档同步

已同步：

```text
AI_DOC/mem_ut_flow_doc/main_table_boundary_profile_generation_flow.md
mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md
```

已检查无需修改：

```text
AI_DOC/project_management/mem_ut_parameter_management.md
mem_ut/ver/ut/memblock/rule/memblock_parameter_management_rule.md
```

检查结论：

```text
mem_ut_parameter_management.md 和 memblock_parameter_management_rule.md 已有公共参数统一规则：
  plus.sv -> seq_csr_common.sv -> getter -> sequence/helper。
本轮新增参数符合该规则，无需为每个具体参数重复列举。
plus_demo_migration_plan.md 的参数分类表已补充 fuOpType、boundary 和 keep_ref_size 参数类别。
```

## 7. 实现与 Plan 不一致项

未发现必须回改的实现与 Plan 不一致项；当前 coding 行为与对应 plan 的目标 flow 保持一致。

可接受差异：

```text
default_fuop_by_op_class_and_size() 当前额外支持 PREFETCH/CBO/AMO 的 default size 查询。
plan 当前只要求地址复用 target_op_class 为 INT_LOAD/STORE 时使用同 size fuOpType 选择。
当前 apply_addr_reuse_window() 的 target_op_class 仍只会是 INT_LOAD 或 STORE，因此额外分支不会改变本轮功能行为。
保留原因是 helper 与 derive_size_bytes() 的 op_class 覆盖范围一致，后续扩展时更容易复用。
```

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`，函数：`default_fuop_by_op_class_and_size()`。

```systemverilog
MEMBLOCK_OP_CLASS_PREFETCH: begin
    if (size_bytes == 64) begin
        return MEMBLOCK_LSUOP_PREFETCH_R;
    end
end
MEMBLOCK_OP_CLASS_CBO: begin
    if (size_bytes == 64) begin
        return MEMBLOCK_LSUOP_CBO_CLEAN;
    end
end
MEMBLOCK_OP_CLASS_AMO: begin
    case (size_bytes)
        4: return MEMBLOCK_LSUOP_AMOADD_W;
        8: return MEMBLOCK_LSUOP_AMOADD_D;
    endcase
end
```

中文伪代码：

```text
该额外分支为 PREFETCH/CBO 的 64B 分类和 AMO 的 4B/8B 分类提供 default fuOpType。
本轮地址复用不会把 PREFETCH/CBO/AMO 作为 target_op_class，因此这些分支不会被 keep_ref_size 主路径调用。
如果未来扩展地址复用 target 到这些 op_class，这些 default 仍必须重新结合专项 plan 审查。
当前处理结论是保留，不需要回改源码或 plan。
```

## 8. Plan 未说明但 Coding 落实的细节

细节 1：`choose_fuop_by_op_class_and_size()` 通过枚举来源保证 membership。

```text
plan 要求 keep_ref_size 选择的 target_fuOpType 必须属于 target_op_class。
当前实现中，正常候选来自 enumerate_fuoptypes(target_op_class)，天然满足 membership；
default fallback 分支再显式检查 fuop_belongs_to_op_class() 和 derive_size_bytes()。
```

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`，函数：`choose_fuop_by_op_class_and_size()`。

```systemverilog
enumerate_fuoptypes(op_class, fuops);
foreach (fuops[i]) begin
    if (derive_size_bytes(op_class, fuops[i]) == size_bytes) begin
        legal_count++;
        weight = get_fuop_weight(op_class, fuops[i]);
        if (weight != 0) begin
            legal_fuops.push_back(fuops[i]);
            weights.push_back(weight);
        end
    end
end

if (legal_fuops.size() == 0) begin
    default_fuop = default_fuop_by_op_class_and_size(op_class, size_bytes);
    if (!fuop_belongs_to_op_class(op_class, default_fuop) ||
        derive_size_bytes(op_class, default_fuop) != size_bytes) begin
        `uvm_fatal(get_type_name(), ...)
    end
    return default_fuop;
end
```

中文伪代码：

```text
该函数先枚举目标 op_class 支持的所有 fuOpType。
对每个 fuOpType 派生 size，只保留 size 等于目标 size 的候选。
对于同 size 候选，再读取 fuOpType 权重；权重非 0 的候选进入 weighted pick。
如果目标 op_class 下没有任何同 size fuOpType，说明配置或调用逻辑非法，函数会 fatal。
如果存在同 size 候选但权重全 0，函数打印 UVM_ERROR 并选择 op_class/size 对应 default。
default 返回后仍检查它属于目标 op_class 且 size 匹配，检查失败则 fatal。
```

## 9. 验证记录

静态检查：

```text
git diff --check -- 本轮修改文件：通过。
Markdown fence 偶数检查：通过。
default.cfg key 对 plus.sv key 缺失检查：输出为空。
旧 fuOpType 均匀随机残留 grep：未发现 random_*_fuoptype() 内继续使用旧 $urandom_range。
```

编译：

```text
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
结果：通过，VCS compile/elab/link 完成。
```

运行 smoke：

```text
1. normal fuOpType directed smoke：
   tc_dispatch_real_smoke, MAIN_TRANS_NUM=8, boundary 关闭，INT_LOAD 只打开 LW 权重。
   结果：real dispatch smoke main table ready: main_trans_num=8。
   后续 100us PH_TIMEOUT，原因是 MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=0 导致 issue/commit 无进展；主表构建已完成。

2. boundary + addr_reuse + keep_ref_size smoke：
   使用临时 cfg 展开参数，boundary 打开，CROSS_CACHELINE_SAME_4K 权重为 1，地址复用和 keep_ref_size 强制开启。
   结果：built boundary candidate cache with 1 profile entries；real dispatch smoke main table ready: main_trans_num=16。
   后续 100us PH_TIMEOUT，原因同上。

3. boundary 合法 fuOpType 权重全 0 default smoke：
   使用临时 cfg 展开参数，地址复用关闭，LOAD_FUOP_* 全 0。
   结果：打印 boundary cache config error UVM_ERROR，随后每个 uid uses default boundary fuOpType，并完成 main table ready。
   后续 100us PH_TIMEOUT，原因同上。
```

验证风险：

```text
本轮 smoke 重点验证主表生成路径，不验证 DUT 完整执行通过。
tc_sanity 不构建随机主表，不适合作为本功能 runtime smoke。
长 plus_arg 多参数在远端 wrapper 中会被转义，boundary smoke 改用临时 cfg 文件验证；临时 cfg 已删除，不进入最终修改。
boundary candidate cache 的“fuOpType 权重全 0 -> UVM_ERROR + default”只覆盖 boundary 初始候选生成；
如果同时强制地址复用且首条没有 ref，fallback 会走 normal apply_minimal_op_template()，
此时同类 fuOpType 权重全 0 会按 normal 路径 fatal。
正式 plan 的 all-zero default smoke 关闭地址复用，本轮按该条件验证通过。
```
