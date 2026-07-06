# 主表地址复用 recent-window 实现 Review

## 0. 术语、字段和参数落点说明

本节先把后文反复出现的专业名词、字段和参数说清楚，避免只看到变量名但不知道它在测试框架中代表什么。

### 0.1 术语说明

| 术语 | 通俗解释 | 代码落点 |
|---|---|---|
| `uid` | 测试框架主表中的静态编号，一条主表 transaction 一个 uid。redirect/replay 后仍用同一个 uid 追踪这条主表请求。 | `memblock_uid_t`、`data.alloc_uid()`、`main_table_by_uid[uid]` |
| ROB | DUT 后端乱序提交队列的索引。测试框架用 `{flag,value}` 表达 ROB 环形回绕后的顺序。 | `memblock_rob_key_t`、`rob_order_util::rob_advance()`、`tr.robIdx_flag/value` |
| LSQ | Load/Store Queue，也就是 DUT 中 load queue 和 store queue 的统称。当前地址复用窗口用 LSQ 容量做保守上限。 | `MEMBLOCK_LQ_SIZE`、`MEMBLOCK_SQ_SIZE`、`seq_csr_common::get_addr_ref_window_max()` |
| recent queue | 主表生成过程中的轻量候选列表，只保存最近 load/store 的 uid，不保存 transaction 内容。 | `recent_load_uid_q`、`recent_store_uid_q` |
| after 场景 | 当前 uid 在某类历史 uid 之后复用地址，例如 `LOAD_AFTER_STORE` 表示当前 load 复用历史 store 地址。 | `memblock_addr_reuse_kind_e` |
| fallback | 某个 after 场景命中但对应 recent queue 没有候选时，不复制地址，只把当前 transaction 修正成方案规定的兜底类型。 | `apply_addr_reuse_window()` 的 `else` 分支 |
| `fuType` | DUT 功能单元类型，决定这条请求走 LDU、STU 还是 MOU 等执行单元。 | `tr.fuType` |
| `fuOpType` | LSU 具体操作编码，例如 LB/LW/SD/AMO 等。它必须和 `fuType/op_class/lsq_flow` 匹配。 | `tr.fuOpType`、`random_load_fuoptype()` 等 |
| `lsq_flow` | 测试框架抽象的 LSQ 流程类型，用于后续 admission/issue 判断这条请求按 load、store、atomic 还是 CBO 类处理。 | `tr.lsq_flow`、`memblock_lsq_flow_e` |
| `numLsElem` | 一条 transaction 需要占用的 LSQ 元素数量。当前标量 load/store/prefetch 固定 1，AMO 模板保持 0。 | `tr.numLsElem`、`derive_op_behavior()` |

### 0.2 新增参数和字段生命周期

| 对象 | 类型 | 谁设置 | 谁读取 | 生命周期/副作用 |
|---|---|---|---|---|
| `MEMBLOCK_ADDR_REUSE_EN_1_WT/EN_0_WT` | plus 参数 | `plus.sv` 默认值或命令行 cfg | `seq_csr_common` 快照后由 `apply_addr_reuse_window()` 使用 | 控制每条 transaction 是否尝试地址复用；默认 1:19，约 5% 命中。全 0 会 fatal。 |
| 四类 `MEMBLOCK_ADDR_REUSE_*_WT` | plus 参数 | `plus.sv` 默认值或命令行 cfg | `select_addr_reuse_kind()` | 控制命中地址复用后选择哪类 after 场景；全 0 会 fatal。 |
| `MEMBLOCK_ADDR_REF_WINDOW_FIXED/SMALL/MEDIUM/LARGE` | plus 参数 | `plus.sv` 默认值或命令行 cfg | `choose_addr_ref_window()`、`prune_recent_uid_q()` | 控制 recent queue 的 uid 距离窗口。固定窗口大于 0 时直接使用，否则按三段权重随机。 |
| `MEMBLOCK_ROB_START_*` | plus 参数 | `plus.sv` 默认值或命令行 cfg | `choose_rob_start_key()` | 控制 uid0 的 ROB 起始 value，初始 flag 固定 0。 |
| `recent_load_uid_q` | 局部 queue | `push_recent_uid()` 在每条 uid 写入主表后追加 | `apply_addr_reuse_window()`、`random_pick_recent_uid()`、`prune_recent_uid_q()` | 只在一次 `build_random_main_table()` 调用内有效。保存窗口内历史 load uid，作为后续 load/store 复用地址候选。 |
| `recent_store_uid_q` | 局部 queue | `push_recent_uid()` 在每条 uid 写入主表后追加 | `apply_addr_reuse_window()`、`random_pick_recent_uid()`、`prune_recent_uid_q()` | 只在一次 `build_random_main_table()` 调用内有效。保存窗口内历史 store uid，作为后续 load/store 复用地址候选。 |

这些 recent queue 不是新的事实源。真正 transaction 内容仍由 `data.get_main_transaction(ref_uid)` 从主表读取；queue 只负责让候选查找从全表扫描变成窗口内 uid 查找。

### 0.3 文档结构说明

第 1 到第 4 章按功能特性描述“修改前、修改后、正确性检查”，其中穿插的源码片段用于就地说明关键行为。第 8 章按 review 规则集中补齐完整调用关系、关键 helper 输入输出、副作用和源码级伪代码；第 9 到第 11 章完成 Plan 对齐、不一致项和额外 coding 细节检查。因此审查时应以“特性章节 + 第 8 章源码支撑 + 第 9/10/11 章 Plan 对齐”作为完整闭环。

## 1. 特性一：旧后处理替换为主表生成期 recent-window

### 修改前逻辑

旧实现先完整生成 `main_table_by_uid[]`，再调用 `inject_ls_addr_reuse_by_fuoptype()` 扫主表。这个入口用 `MEMBLOCK_LD_TO_ST_ADDR_REUSE_WT` 和 `MEMBLOCK_ST_TO_LD_ADDR_REUSE_WT` 两个方向权重控制复用，并通过 `select_load_addr_ref()` / `select_prior_store_addr_ref()` 从主表里挑参考 transaction。

问题是后处理阶段已经丢失了“当前 uid 生成时的近邻窗口”语义，旧 `select_load_addr_ref()` 甚至会从全表中排除当前 uid 后任选 load，可能选到未来 uid 或距离当前很远的 uid；这既不符合 LSQ 中仍可能共存的场景，也让大表生成接近 O(N^2)。

### 修改后逻辑

`build_random_main_table()` 现在边生成边维护两个 recent queue：`recent_load_uid_q` 和 `recent_store_uid_q`。每条 transaction 完成基础随机后，先按 uid 距离窗口 prune，再按 enable 和四类 after 场景权重尝试复用，写入主表后按最终类型入队。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`，函数/task：`build_random_main_table`

函数功能简析：这是随机主表入口。输入是本轮 transaction 数量，副作用是 reset 公共表、连续分配 uid、生成主表、初始化 status 并置 main table ready。

```systemverilog
task memblock_dispatch_base_sequence::build_random_main_table(input int unsigned main_trans_num_i);
    memblock_rob_key_t rob_key;
    memblock_uid_t recent_load_uid_q[$];
    memblock_uid_t recent_store_uid_q[$];
    int unsigned addr_ref_window;

    data.reset_all_tables(main_trans_num_i);
    rob_key = choose_rob_start_key();
    addr_ref_window = choose_addr_ref_window();

    for (int unsigned idx = 0; idx < main_trans_num_i; idx++) begin
        memblock_uid_t uid;
        main_control_transaction tr;

        uid = data.alloc_uid();
        tr = main_control_transaction::type_id::create($sformatf("main_uid_%0d", uid));
        randomize_main_transaction(tr, uid, rob_key);
        prune_recent_uid_q(recent_load_uid_q, uid, addr_ref_window);
        prune_recent_uid_q(recent_store_uid_q, uid, addr_ref_window);
        apply_addr_reuse_window(tr, uid, recent_load_uid_q, recent_store_uid_q);
        data.set_main_transaction(uid, tr);
        push_recent_uid(tr, uid, recent_load_uid_q, recent_store_uid_q);
        rob_key = rob_order_util::rob_advance(rob_key, 1);
    end

    init_status_for_main_table();
    data.check_main_table_complete();
endtask:build_random_main_table
```

中文伪代码：

1. 清空并重新分配公共主表/状态表。
2. 选择 uid0 的 ROB 起始 key，只随机 value，flag 固定为 0。
3. 选择本轮地址复用 uid 窗口。
4. 对每个 uid：创建并随机化 transaction。
5. 从 load/store recent queue 队头删除超出 uid 窗口的候选。
6. 对当前 transaction 尝试一次地址复用。
7. 把修正后的 transaction 写入主表。
8. 按最终类型把当前 uid 推入 load 或 store recent queue。
9. ROB key 按 ROB 环形顺序推进。
10. 循环结束后初始化 status 表并检查主表完整性。

### 通俗原理

recent queue 可以理解为“最近还可能在 LSQ 中共存的 load/store uid 影子列表”。它不保存完整 transaction，只保存 uid。真正复制地址时仍通过 `data.get_main_transaction(ref_uid)` 读取主表事实源。这样每条 transaction 只访问窗口内候选，不再扫完整主表。

### 正确性检查

- 候选距离由 `cur_uid - ref_uid <= addr_ref_window` 保证。
- 当前 transaction 入 recent queue 使用的是地址复用后的最终类型，不会把被改成 store 的 uid 放进 load queue。
- `data.set_main_transaction()` 在 `push_recent_uid()` 之前执行，后续 uid 通过主表读取参考项时能拿到完整 transaction。
- 手动主表导入不再调用旧后处理入口，避免 directed 表被隐式改写。

### 替换类修改说明

| 必填项 | 内容 |
|---|---|
| 旧对象 A 是什么 | `inject_ls_addr_reuse_by_fuoptype()` 是旧主表生成后处理 task；`select_load_addr_ref()` 和 `select_prior_store_addr_ref()` 是旧参考项选择 helper；`MEMBLOCK_LD_TO_ST_ADDR_REUSE_WT`、`MEMBLOCK_ST_TO_LD_ADDR_REUSE_WT`、`MEMBLOCK_MANUAL_ADDR_REUSE_EN` 是旧 plus 参数。 |
| A 原来的行为 | 随机主表生成完成后再扫主表，根据 load/store 方向权重挑参考 transaction，然后覆盖当前项地址。 |
| A 的问题 | 后处理不知道“当前 uid 生成时附近有哪些候选”，容易选到未来 uid 或距离很远的 uid；手动 directed 主表也可能被隐式改写；全表查找在大规模主表下成本更高。 |
| 新对象 B 是什么 | `build_random_main_table()` 内部的 `recent_load_uid_q/recent_store_uid_q`、`apply_addr_reuse_window()` 和 `memblock_addr_reuse_kind_e`。 |
| B 如何替代 A | 主表边生成边维护 recent queue，每条 uid 只从窗口内历史 uid 中选参考；手动导入路径 `import_manual_main_table()` 不进入地址复用逻辑。 |
| 状态一致性来源 | 主表 `data.set_main_transaction(uid, tr)` 仍是 transaction 事实源，recent queue 只保存 uid 句柄。 |
| 例子 | 如果 uid20 是 load，旧实现可能从 uid80 的 load 复制地址；新实现只能从 uid20 之前且仍在 `addr_ref_window` 内的 recent queue 选参考。 |
| 处理结论 | 删除旧后处理语义，保留主表生成期 recent-window 作为唯一地址复用入口。 |

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`，task：`build_main_table()`

函数功能简析：`build_main_table()` 是主表准备入口。输入来自 `seq_csr_common::get_use_manual_main_table()` 和 `get_main_trans_num()`，副作用是选择手动导入或随机生成主表。

```systemverilog
task memblock_dispatch_base_sequence::build_main_table();
    if (data == null) begin
        data = common_data_transaction::get();
    end

    if (seq_csr_common::get_use_manual_main_table()) begin
        import_manual_main_table();
    end else begin
        build_random_main_table(seq_csr_common::get_main_trans_num());
    end
endtask:build_main_table
```

中文伪代码：

1. 先确保公共数据对象 `data` 存在。
2. 如果 plus 配置打开手动主表，则调用 `import_manual_main_table()`。
3. 如果没有打开手动主表，则调用 `build_random_main_table()`，随机生成主表并执行 recent-window 地址复用。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`，task：`import_manual_main_table()`

函数功能简析：`import_manual_main_table()` 只导入用户配置的 directed 表项。输入是 `manual_main_table_by_rob`，副作用是重置公共表、按 ROB key 排序导入 transaction、初始化状态表；它不调用地址复用 helper，因此不会改写手动配置地址。

```systemverilog
task memblock_dispatch_base_sequence::import_manual_main_table();
    int unsigned manual_num;
    int unsigned rob_keys[$];

    if (data == null) begin
        data = common_data_transaction::get();
    end
    manual_num = manual_main_table_by_rob.num();
    if (manual_num == 0) begin
        `uvm_fatal(get_type_name(), "manual main table mode requires at least one configured entry")
    end

    data.reset_all_tables(manual_num);
    foreach (manual_main_table_by_rob[rob_key]) begin
        rob_keys.push_back(rob_key);
    end
    rob_keys.sort();
    foreach (rob_keys[idx]) begin
        int unsigned rob_key;
        memblock_uid_t uid;
        main_control_transaction tr;

        rob_key = rob_keys[idx];
        tr = manual_main_table_by_rob[rob_key];
        if (tr == null) begin
            `uvm_fatal(get_type_name(), $sformatf("manual main table rob_key=%0d got null transaction", rob_key))
        end
        uid = data.alloc_uid();
        tr.uid = uid;
        tr.post_manual_config(1'b1);
        validate_main_table_entry(tr, $sformatf("manual rob_key=%0d", rob_key));
        data.set_main_transaction(uid, tr);
    end

    init_status_for_main_table();
    data.check_main_table_complete();
endtask:import_manual_main_table
```

中文伪代码：

1. 先确保公共数据对象 `data` 存在。
2. 读取手动表项数量；如果数量为 0，直接 fatal。
3. 按手动表项数量重置公共主表和状态表。
4. 收集所有手动表 ROB key 并排序。
5. 按排序后的 ROB key 逐项取出 transaction。
6. 如果 transaction 为空，直接 fatal。
7. 为手动 transaction 分配 uid，调用 `post_manual_config()` 做手动配置后的派生字段修正。
8. 调用 `validate_main_table_entry()` 检查合法性。
9. 将 transaction 写入主表。
10. 循环结束后初始化状态表并检查主表完整性。
11. 整个路径没有调用 `apply_addr_reuse_window()`，因此不会隐式复制或改写手动配置地址。

## 2. 特性二：地址复用场景、queue 维护与类型修正

### 修改前逻辑

旧入口只按“store 复用 load 地址”和“load 复用 prior store 地址”两个方向做后处理，且类型不会在复用阶段改变。它无法表达 LOAD_AFTER_LOAD、STORE_AFTER_STORE，也不能在候选为空时按方案规定把当前项修正成参考侧 fallback 类型。

### 修改后逻辑

新增四类场景枚举 `memblock_addr_reuse_kind_e`，分别表达当前 transaction 在历史 load/store 之后复用地址。`apply_addr_reuse_window()` 每次 enable 命中后只选择一种场景执行。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_types.sv`，类型：`memblock_addr_reuse_kind_e`

类型功能简析：该 enum 是地址复用场景的公共编码，被 base sequence 的随机选择和 review/web 文档引用。

```systemverilog
typedef enum int unsigned {
    MEMBLOCK_ADDR_REUSE_LOAD_AFTER_STORE  = 0,
    MEMBLOCK_ADDR_REUSE_LOAD_AFTER_LOAD   = 1,
    MEMBLOCK_ADDR_REUSE_STORE_AFTER_LOAD  = 2,
    MEMBLOCK_ADDR_REUSE_STORE_AFTER_STORE = 3
} memblock_addr_reuse_kind_e;
```

中文伪代码：

1. `LOAD_AFTER_STORE` 表示当前 load 尝试复用历史 store 地址。
2. `LOAD_AFTER_LOAD` 表示当前 load 尝试复用历史 load 地址。
3. `STORE_AFTER_LOAD` 表示当前 store 尝试复用历史 load 地址。
4. `STORE_AFTER_STORE` 表示当前 store 尝试复用历史 store 地址。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`，函数：`apply_addr_reuse_window`

函数功能简析：输入是当前 transaction、当前 uid、recent load/store queue；副作用是可能修正当前 transaction 类型并复制参考地址。

```systemverilog
function void memblock_dispatch_base_sequence::apply_addr_reuse_window(input main_control_transaction tr,
                                                                       input memblock_uid_t cur_uid,
                                                                       ref memblock_uid_t recent_load_uid_q[$],
                                                                       ref memblock_uid_t recent_store_uid_q[$]);
    memblock_addr_reuse_kind_e kind;
    memblock_uid_t ref_uid;
    main_control_transaction ref_tr;

    if (rand_weighted2(seq_csr_common::get_addr_reuse_en_1_wt(),
                       seq_csr_common::get_addr_reuse_en_0_wt()) != 0) begin
        return;
    end

    kind = select_addr_reuse_kind();
    case (kind)
        MEMBLOCK_ADDR_REUSE_LOAD_AFTER_STORE: begin
            if (random_pick_recent_uid(recent_store_uid_q, ref_uid, 1'b0)) begin
                ref_tr = data.get_main_transaction(ref_uid);
                set_transaction_ls_kind(tr, 1'b1);
                fixup_after_addr_reuse(tr, ref_tr, 1'b1, $sformatf("load_after_store uid=%0d ref_uid=%0d", cur_uid, ref_uid));
            end else begin
                set_transaction_ls_kind(tr, 1'b0);
                fixup_after_addr_reuse(tr, null, 1'b0, $sformatf("load_after_store fallback uid=%0d", cur_uid));
            end
        end
        MEMBLOCK_ADDR_REUSE_LOAD_AFTER_LOAD: begin
            set_transaction_ls_kind(tr, 1'b1);
            if (random_pick_recent_uid(recent_load_uid_q, ref_uid, 1'b1)) begin
                ref_tr = data.get_main_transaction(ref_uid);
                fixup_after_addr_reuse(tr, ref_tr, 1'b1, $sformatf("load_after_load uid=%0d ref_uid=%0d", cur_uid, ref_uid));
            end else begin
                fixup_after_addr_reuse(tr, null, 1'b0, $sformatf("load_after_load fallback uid=%0d", cur_uid));
            end
        end
        MEMBLOCK_ADDR_REUSE_STORE_AFTER_LOAD: begin
            if (random_pick_recent_uid(recent_load_uid_q, ref_uid, 1'b0)) begin
                ref_tr = data.get_main_transaction(ref_uid);
                set_transaction_ls_kind(tr, 1'b0);
                fixup_after_addr_reuse(tr, ref_tr, 1'b1, $sformatf("store_after_load uid=%0d ref_uid=%0d", cur_uid, ref_uid));
            end else begin
                set_transaction_ls_kind(tr, 1'b1);
                fixup_after_addr_reuse(tr, null, 1'b0, $sformatf("store_after_load fallback uid=%0d", cur_uid));
            end
        end
        MEMBLOCK_ADDR_REUSE_STORE_AFTER_STORE: begin
            set_transaction_ls_kind(tr, 1'b0);
            if (random_pick_recent_uid(recent_store_uid_q, ref_uid, 1'b1)) begin
                ref_tr = data.get_main_transaction(ref_uid);
                fixup_after_addr_reuse(tr, ref_tr, 1'b1, $sformatf("store_after_store uid=%0d ref_uid=%0d", cur_uid, ref_uid));
            end else begin
                fixup_after_addr_reuse(tr, null, 1'b0, $sformatf("store_after_store fallback uid=%0d", cur_uid));
            end
        end
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("unsupported addr reuse kind=%0d", kind))
        end
    endcase
endfunction:apply_addr_reuse_window
```

中文伪代码：

1. 先按 `addr_reuse_en_1_wt/addr_reuse_en_0_wt` 随机，未命中则直接返回。
2. 命中后随机四类复用场景。
3. 如果是 `LOAD_AFTER_STORE`，从 store recent queue 选参考；选到则当前项改成 load 并复制地址，没选到则按 fallback 改成 store。
4. 如果是 `LOAD_AFTER_LOAD`，当前项改成 load；选到 load 参考则复制地址，没选到只保持 load 类型并校验。
5. 如果是 `STORE_AFTER_LOAD`，从 load recent queue 选参考；选到则当前项改成 store 并复制地址，没选到则按 fallback 改成 load。
6. 如果是 `STORE_AFTER_STORE`，当前项改成 store；选到 store 参考则复制地址，没选到只保持 store 类型并校验。
7. 任何未知 enum 都直接 fatal，避免新枚举漏处理。
8. 每个有效分支最后都通过 `fixup_after_addr_reuse()` 更新 `vaddr` 并调用主表校验。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`，函数：`set_transaction_ls_kind`

函数功能简析：这是第一层类型修正 helper。输入是 transaction 和目标 load/store 类型，副作用是重建 op class、fuType、fuOpType、lsq_flow、numLsElem。

```systemverilog
function void memblock_dispatch_base_sequence::set_transaction_ls_kind(input main_control_transaction tr,
                                                                       input bit make_load);
    if (make_load) begin
        tr.op_class = MEMBLOCK_OP_CLASS_INT_LOAD;
    end else begin
        tr.op_class = MEMBLOCK_OP_CLASS_STORE;
    end
    apply_minimal_op_template(tr);
    tr.update_vaddr();
endfunction:set_transaction_ls_kind
```

中文伪代码：

1. 如果目标是 load，把 op class 设成 INT_LOAD。
2. 如果目标是 store，把 op class 设成 STORE。
3. 调用 `apply_minimal_op_template()` 重新生成合法 fuType/fuOpType/lsq_flow/numLsElem。
4. 重新计算 vaddr，保持地址派生字段自洽。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`，函数：`fixup_after_addr_reuse`

函数功能简析：这是第二层复用后修正 helper。输入是当前 transaction、可选参考 transaction、是否复制地址；副作用是复制 `src_0/imm`、更新 `vaddr` 并校验。

```systemverilog
function void memblock_dispatch_base_sequence::fixup_after_addr_reuse(input main_control_transaction tr,
                                                                      input main_control_transaction ref_tr,
                                                                      input bit copy_addr,
                                                                      input string caller);
    if (copy_addr) begin
        tr.src_0 = ref_tr.src_0;
        tr.imm   = ref_tr.imm;
    end
    tr.update_vaddr();
    validate_main_table_entry(tr, caller);
endfunction:fixup_after_addr_reuse
```

中文伪代码：

1. 如果要求复制地址，则使用调用方传入的参考 transaction 复制地址字段。
2. 把参考 transaction 的 `src_0` 和 `imm` 复制到当前 transaction。
3. 调用 `update_vaddr()` 重新派生虚拟地址。
4. 调用 `validate_main_table_entry()` 检查 op class、fuType、fuOpType、lsq_flow、numLsElem 和 vaddr 是否一致。

### 字段/函数为什么添加

| 对象 | 添加原因 | 承担作用 |
|---|---|---|
| `recent_load_uid_q/recent_store_uid_q` | 避免混合队列每次筛选 load/store，也避免全表扫描。 | 保存窗口内可复用的历史 load/store uid。 |
| `prune_recent_uid_q()` | 需要按 uid 距离淘汰，而不是按 queue size 淘汰。 | 删除超过 `addr_ref_window` 的候选。 |
| `random_pick_recent_uid()` | 四类复用对候选删除策略不同。 | 同类型复用可删除候选，跨类型复用保留候选。 |
| `set_transaction_ls_kind()` | 地址复用可能改变当前 transaction 类型。 | 第一层修正 op class 与合法 LS 模板。 |
| `fixup_after_addr_reuse()` | 类型和地址复制需要分层，方便校验每层职责。 | 第二层复制地址、更新 vaddr 并校验。 |

### 正确性检查

- `validate_main_table_entry()` 会检查 load/store template，确保 `fuType/fuOpType/lsq_flow` 匹配。
- `is_load_main_tr()` 和 `is_store_main_tr()` 使用最终 op class 分类，避免入错 recent queue。
- fallback 分支不复制地址，只做类型修正，让当前项成为后续候选。

## 3. 特性三：plus 参数链路与默认约 5% enable

### 修改前逻辑

旧参数为 `MEMBLOCK_LD_TO_ST_ADDR_REUSE_WT`、`MEMBLOCK_ST_TO_LD_ADDR_REUSE_WT` 和 `MEMBLOCK_MANUAL_ADDR_REUSE_EN`，参数名直接绑定旧后处理方向。保留它们会导致同一个功能存在旧入口和新入口两套语义。

### 修改后逻辑

删除旧 key 兼容入口，新增 enable 权重、四类场景权重、窗口权重和 ROB 起点参数。默认 `MEMBLOCK_ADDR_REUSE_EN_1_WT=1`、`MEMBLOCK_ADDR_REUSE_EN_0_WT=19`，即每条 transaction 约 5% 概率尝试地址复用；默认四类场景只打开 `LOAD_AFTER_STORE` 和 `STORE_AFTER_LOAD`，优先提高 load/store 同地址相关和潜在违例概率，同类复用保留为可配置扩展但默认关闭。

源码位置：`mem_ut/ver/ut/memblock/env/plus.sv`，字段定义组

字段功能简析：这些字段是命令行 plusarg 的唯一入口，由 `seq_csr_common` 快照和合法化。

```systemverilog
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ADDR_REUSE_EN_1_WT, int, 1)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ADDR_REUSE_EN_0_WT, int, 19)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ADDR_REUSE_LOAD_AFTER_STORE_WT, int, 1)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ADDR_REUSE_LOAD_AFTER_LOAD_WT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ADDR_REUSE_STORE_AFTER_LOAD_WT, int, 1)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ADDR_REUSE_STORE_AFTER_STORE_WT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ADDR_REF_WINDOW_FIXED, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ADDR_REF_WINDOW_SMALL_WEIGHT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ADDR_REF_WINDOW_MEDIUM_WEIGHT, int, 0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ADDR_REF_WINDOW_LARGE_WEIGHT, int, 1)
```

中文伪代码：

1. 定义地址复用 enable 的 1/0 权重。
2. 定义四类 after 复用场景权重。
3. 定义固定窗口和 small/medium/large 窗口权重。
4. 默认固定窗口为 0，使用 large 权重选择接近 LSQ 容量上限的窗口。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`，函数：`validate_and_clamp`

函数功能简析：该函数统一检查参数合法性。这里新增地址复用和 ROB 起点参数的权重全零检查、窗口 clamp 和 ROB value clamp。

```systemverilog
fatal_if_all_zero2("addr_reuse_en weights", addr_reuse_en_1_wt, addr_reuse_en_0_wt);
fatal_if_all_zero4("addr_reuse kind weights",
                   addr_reuse_load_after_store_wt,
                   addr_reuse_load_after_load_wt,
                   addr_reuse_store_after_load_wt,
                   addr_reuse_store_after_store_wt);
if (addr_ref_window_fixed > 0) begin
    clamp_int("addr_ref_window_fixed", addr_ref_window_fixed, 1, get_addr_ref_window_max());
end else begin
    fatal_if_all_zero3("addr_ref_window weights",
                       addr_ref_window_small_weight,
                       addr_ref_window_medium_weight,
                       addr_ref_window_large_weight);
end
clamp_int("rob_start_fixed_value", rob_start_fixed_value, 0, MEMBLOCK_ROB_SIZE - 1);
```

中文伪代码：

1. 地址复用 enable 权重不能全 0。
2. 四类地址复用场景权重不能全 0。
3. 如果用户指定固定窗口，则把它 clamp 到 `[1:min(LQ_SIZE,SQ_SIZE)]`。
4. 如果不指定固定窗口，则 small/medium/large 权重不能全 0。
5. ROB 固定起始 value 必须落在 `[0:MEMBLOCK_ROB_SIZE-1]`。

### 正确性检查

- `seq/plus_cfg/default.cfg` 已删除旧 key 并补齐所有新 key。
- cfg key 一致性脚本输出为空，说明 default/testcase cfg 中没有未声明 key。
- `rg` 检查确认旧 key 和旧函数名不再出现在代码、source_sv、flow/web 活文档范围内。

## 4. 特性四：ROB 起始 value 随机化

### 修改前逻辑

随机主表固定从 `robIdx_flag=0, robIdx_value=0` 开始，后续 uid 线性推进。这不覆盖 uid0 落在 ROB 中间或接近 wrap 的场景。

### 修改后逻辑

`choose_rob_start_key()` 支持固定起点和三段随机起点。默认固定 value 0，所以默认行为保持兼容。初始 flag 不随机，固定为 0；后续 flag 只由 `rob_order_util::rob_advance()` 在 value wrap 时翻转。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`，函数：`choose_rob_start_key`

函数功能简析：输入来自 `seq_csr_common` getter，输出 uid0 使用的 ROB key。

```systemverilog
function memblock_rob_key_t memblock_dispatch_base_sequence::choose_rob_start_key();
    memblock_rob_key_t key;
    int unsigned sel;
    int unsigned mid_lo;
    int unsigned mid_hi;
    int unsigned near_lo;

    key.flag = 1'b0;
    if (seq_csr_common::get_rob_start_fixed_en()) begin
        key.value = seq_csr_common::get_rob_start_fixed_value();
        return key;
    end

    sel = rand_weighted3(seq_csr_common::get_rob_start_zero_wt(),
                         seq_csr_common::get_rob_start_mid_wt(),
                         seq_csr_common::get_rob_start_near_wrap_wt());
    case (sel)
        0: key.value = '0;
        1: begin
            mid_lo = MEMBLOCK_ROB_SIZE / 4;
            mid_hi = (MEMBLOCK_ROB_SIZE * 3) / 4;
            if (mid_hi <= mid_lo) begin
                key.value = mid_lo;
            end else begin
                key.value = $urandom_range(mid_hi, mid_lo);
            end
        end
        default: begin
            near_lo = (MEMBLOCK_ROB_SIZE > 8) ? (MEMBLOCK_ROB_SIZE - 8) : 0;
            key.value = $urandom_range(MEMBLOCK_ROB_SIZE - 1, near_lo);
        end
    endcase
    return key;
endfunction:choose_rob_start_key
```

中文伪代码：

1. 先把初始 flag 固定为 0。
2. 如果固定模式打开，返回用户指定的 fixed value。
3. 如果随机模式打开，按 zero/mid/near-wrap 三段权重选择 value。
4. zero 返回 0，mid 在 ROB 中间范围随机，near-wrap 在靠近 ROB 尾部范围随机。
5. 返回的 key 交给主表循环，后续每条 uid 都用 `rob_advance()` 连续推进。

### 正确性检查

- `seq_csr_common` 对 fixed value 做 `[0:MEMBLOCK_ROB_SIZE-1]` clamp。
- 默认参数固定为 0，不改变既有默认 smoke 行为。
- 初始 flag 不随机，避免构造一个没有历史上下文的跨 flag 初始态。

## 5. 文档和网页同步

同步了以下活文档：

- `AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_dispatch_base_sequence.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/seq_csr_common.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/plus.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_dispatch_types.md`
- `AI_DOC/web/memblock_dispatch_control_flow_callgraph.md`
- `AI_DOC/web/web_assets/memblock_dispatch_doc.js`
- `AI_DOC/web/memblock_dispatch_control_flow_callgraph/assets/app.js`
- `AI_DOC/web/memblock_dispatch_control_flow_callgraph_enhanced/assets/app.js`
- `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`

同步内容包括：旧后处理入口删除、主表生成期 recent-window 说明、新 helper 函数目录、`memblock_addr_reuse_kind_e` 类型说明、plus 参数分组说明、网页 flow lane 和直接调用关系。

## 6. 验证结果

已执行检查：

```bash
for f in mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg mem_ut/ver/ut/memblock/seq/plus_cfg/tc_dispatch_real*.cfg; do
  sed 's/[#].*$//; s,//.*$,,; s/^+//; /^[[:space:]]*$/d; s/=.*//' "$f"
done | rg '^MEMBLOCK_' | sort -u > /tmp/memblock_cfg_keys.txt
rg -n '`MEMBLOCK_PLUS_ARGS_DEFINE\(MEMBLOCK_|load_(bit|hex64|int|string)\("MEMBLOCK_' mem_ut/ver/ut/memblock/env/plus.sv \
  | sed -n 's/.*MEMBLOCK_/MEMBLOCK_/p' | sed 's/[^A-Za-z0-9_].*$//' | sort -u > /tmp/memblock_plus_keys.txt
comm -23 /tmp/memblock_cfg_keys.txt /tmp/memblock_plus_keys.txt
```

中文伪代码：

1. 从 default cfg 和 testcase cfg 提取所有 `MEMBLOCK_*` key。
2. 从 `plus.sv` 提取所有声明和加载的 `MEMBLOCK_*` key。
3. 用 `comm -23` 找 cfg 中存在但 plus 未声明的 key。
4. 输出为空表示 cfg key 没有缺失声明。

结果：输出为空。

```bash
rg -n "LD_TO_ST|ST_TO_LD|MANUAL_ADDR_REUSE|inject_ls_addr_reuse_by_fuoptype|select_load_addr_ref|select_prior_store_addr_ref|get_ld_to_st|get_st_to_ld|get_manual_addr" \
  mem_ut/ver/ut/memblock AI_DOC/analysis/source_sv AI_DOC/mem_ut_flow_doc AI_DOC/web -S
```

中文伪代码：

1. 在源码、source_sv、flow、web 文档中搜索旧 key、旧 getter 和旧函数名。
2. 如果还有结果，说明旧入口没有完全迁移。

结果：无匹配。

远端 VCS 编译已完成：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
```

中文伪代码：

1. 从 memblock sim 目录调用远端/本地 Makefile 编译入口。
2. VCS 重新解析 `plus_pkg` 和 `seq_pkg`，重编译相关 partition。
3. 生成 `base_fun/exec/simv` 并完成 Verdi KDB elaboration。

结果：命令退出码 0，最终日志包含 `Verdi KDB elaboration done and the database successfully generated: 0 error(s), 0 warning(s)`。过程中有 clock skew warning 和 KDB option warning，但未导致编译失败。

启动运行检查已执行：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_run tc=tc_sanity mode=base_fun
```

中文伪代码：

1. Makefile 先确认 `simv` 可用。
2. 展开 `default.cfg` 中的新地址复用和 ROB 起点 plusarg。
3. 启动 `tc_sanity`，观察 plus 打印和 UVM 启动日志。

结果：仿真成功启动并打印新 key：`MEMBLOCK_ADDR_REUSE_EN_*`、四类 `MEMBLOCK_ADDR_REUSE_*`、`MEMBLOCK_ADDR_REF_WINDOW_*`、`MEMBLOCK_ROB_START_*`。默认 cfg 下 `MEMBLOCK_LSQENQ_SEQ_EN=0`、`MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=0`、`MEMBLOCK_LSQCOMMIT_SEQ_EN=0`，tc_sanity 持续 heartbeat 到 1.2ms 未自然收敛；为避免后台长跑，手动终止该 run，Makefile 显示 `Terminated`。该 run 用于验证启动和参数加载，不作为完整 pass/fail 回归结果。

## 7. 风险边界

- `STORE_AFTER_LOAD` 候选为空时按方案 fallback 为 load，这会让 enable 命中但无候选的 transaction 类型分布略受四类场景权重影响；这是方案定义行为。
- 当前窗口上限使用 `min(LQ_SIZE,SQ_SIZE)`，没有按参考队列分别使用 LQ/SQ 上限；这是第一版保守实现。
- VCS 编译已通过，说明当前 SystemVerilog 工具接受 queue `ref` 参数和 enum helper；完整功能闭环仍需要打开真实 LSQ enqueue / issue / commit 的 targeted cfg 或回归用例验证地址复用场景实际进入 DUT。

## 8. 调用关系与覆盖性检查

### 8.1 主表生成调用关系

| 调用顺序 | 函数/task | 输入 | 输出/副作用 | 在本流程中的功能 |
|---|---|---|---|---|
| 1 | `build_main_table()` | `use_manual_main_table`、`main_trans_num` | 选择随机生成或手动导入 | 主表准备总入口。 |
| 2 | `build_random_main_table(main_trans_num_i)` | 随机主表条目数 | reset 公共表，填充主表和 status 表 | 本特性主入口，边生成主表边执行 recent-window 地址复用。 |
| 3 | `choose_rob_start_key()` | `MEMBLOCK_ROB_START_*` 参数 | uid0 的 ROB key | 选择 uid0 的 `robIdx.value`，初始 flag 固定 0。 |
| 4 | `choose_addr_ref_window()` | `MEMBLOCK_ADDR_REF_WINDOW_*` 参数 | uid 距离窗口 | 决定 recent queue 中候选能保留多远。 |
| 5 | `randomize_main_transaction()` | `tr`、`uid`、当前 `rob_key` | 基础合法 transaction | 随机 op class、地址、优先级、delay，并套最小合法模板。 |
| 6 | `prune_recent_uid_q()` | recent queue、当前 uid、窗口 | 删除过期候选 | 保证参考候选满足 `cur_uid - ref_uid <= addr_ref_window`。 |
| 7 | `apply_addr_reuse_window()` | 当前 transaction、uid、recent load/store queue | 可能修正类型并复制地址 | 本特性核心逻辑；每条 transaction 最多尝试一种地址复用场景。 |
| 8 | `data.set_main_transaction()` | uid、最终 transaction | 写入公共主表 | 把最终 transaction 写入事实源，供后续候选读取和状态表追踪。 |
| 9 | `push_recent_uid()` | 最终 transaction、uid | uid 进入 load/store recent queue | 当前项成为后续 uid 的轻量候选。 |
| 10 | `rob_order_util::rob_advance()` | 当前 ROB key、step=1 | 下一条 uid 的 ROB key | 保持 ROB 环形顺序和 wrap 规则一致。 |
| 11 | `init_status_for_main_table()` | 已完成主表 | 为每个 uid 建 status | 让后续 LSQ admission、issue、writeback、commit 都能通过 uid 查状态。 |
| 12 | `data.check_main_table_complete()` | 主表和 status 表 | fatal 或通过 | 检查主表生成完整性。 |

### 8.2 地址复用子调用关系

| 调用顺序 | 函数 | 输入 | 输出/副作用 | 子函数职责 |
|---|---|---|---|---|
| 1 | `apply_addr_reuse_window()` | 当前 transaction、uid、recent queues | 可能修改 `op_class/fuType/fuOpType/lsq_flow/src_0/imm/vaddr` | 地址复用场景入口。 |
| 2 | `rand_weighted2()` | enable 权重 | 0 表示启用，1 表示不启用 | 用 `MEMBLOCK_ADDR_REUSE_EN_1_WT/EN_0_WT` 控制每条 transaction 是否尝试复用。 |
| 3 | `select_addr_reuse_kind()` | 四类场景权重 | `memblock_addr_reuse_kind_e` | 选择 LOAD_AFTER_STORE、LOAD_AFTER_LOAD、STORE_AFTER_LOAD 或 STORE_AFTER_STORE。 |
| 4 | `random_pick_recent_uid()` | 参考 queue、是否删除候选 | 是否选到 `ref_uid` | 从窗口内 uid 候选随机选参考；同类型复用可删除候选，跨类型复用保留候选。 |
| 5 | `set_transaction_ls_kind()` | 当前 transaction、目标 load/store 类型 | 重套合法 load/store 模板 | 第一层类型修正，避免地址复用改类型后字段不一致。 |
| 6 | `fixup_after_addr_reuse()` | 当前 transaction、参考 transaction、是否复制地址 | 更新 `src_0/imm/vaddr` 并校验 | 第二层地址修正和合法性检查。 |
| 7 | `validate_main_table_entry()` | 最终 transaction | fatal 或通过 | 检查 ROB 范围、vector 限制、op template、`numLsElem`、`vaddr` 等一致性。 |

### 8.3 关键子函数展开

#### 8.3.1 `choose_addr_ref_window()`

| 项目 | 内容 |
|---|---|
| 功能目的 | 为本轮随机主表选择一个 uid 距离窗口。窗口越大，可复用的历史 load/store uid 越多。 |
| 输入 | `seq_csr_common` 中的 `addr_ref_window_fixed`、small/medium/large 权重和 `get_addr_ref_window_max()`。 |
| 输出/副作用 | 返回 `addr_ref_window`；不修改公共状态。 |
| 调用者 | `build_random_main_table()`。 |
| 内部调用 | `rand_weighted3()` 用三段权重选择 small/medium/large 区间。 |
| 在流程中的作用 | 决定 `prune_recent_uid_q()` 的淘汰边界，避免候选跨度无限扩大。 |

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`，函数：`choose_addr_ref_window()`

```systemverilog
function int unsigned memblock_dispatch_base_sequence::choose_addr_ref_window();
    int unsigned fixed_window;
    int unsigned max_window;
    int unsigned sel;
    int unsigned lo;
    int unsigned hi;

    fixed_window = seq_csr_common::get_addr_ref_window_fixed();
    max_window = seq_csr_common::get_addr_ref_window_max();
    if (fixed_window > 0) begin
        return fixed_window;
    end

    sel = rand_weighted3(seq_csr_common::get_addr_ref_window_small_weight(),
                         seq_csr_common::get_addr_ref_window_medium_weight(),
                         seq_csr_common::get_addr_ref_window_large_weight());
    case (sel)
        0: begin
            lo = 1;
            hi = max_window / 4;
        end
        1: begin
            lo = max_window / 4;
            hi = max_window / 2;
        end
        default: begin
            lo = max_window / 2;
            hi = max_window;
        end
    endcase

    if (lo < 1) begin
        lo = 1;
    end
    if (hi < lo) begin
        hi = lo;
    end
    if (hi > max_window) begin
        hi = max_window;
    end
    return $urandom_range(hi, lo);
endfunction:choose_addr_ref_window
```

中文伪代码：

1. 先读取固定窗口和最大窗口。
2. 如果固定窗口大于 0，直接返回固定窗口。
3. 如果不使用固定窗口，就按 small/medium/large 权重随机选择一个窗口区间。
4. small 使用 `[1, max/4]`，medium 使用 `[max/4, max/2]`，large 使用 `[max/2, max]`。
5. 对 `lo/hi` 做边界修正，保证 `lo >= 1`、`hi >= lo`、`hi <= max_window`。
6. 在修正后的 `[lo, hi]` 内随机返回窗口值。

#### 8.3.2 `select_addr_reuse_kind()`

| 项目 | 内容 |
|---|---|
| 功能目的 | 在地址复用 enable 命中后，选择本次 transaction 使用哪一种 after 场景。 |
| 输入 | 四类 `MEMBLOCK_ADDR_REUSE_*_WT` 权重。 |
| 输出/副作用 | 返回 `memblock_addr_reuse_kind_e`；不修改 transaction。 |
| 调用者 | `apply_addr_reuse_window()`。 |
| 内部调用 | `rand_weighted4()`。 |
| 在流程中的作用 | 把“是否复用地址”和“复用哪类历史项”拆开，便于 testcase 通过 plus 调整场景比例。 |

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`，函数：`select_addr_reuse_kind()`

```systemverilog
function memblock_addr_reuse_kind_e memblock_dispatch_base_sequence::select_addr_reuse_kind();
    int unsigned sel;

    sel = rand_weighted4(seq_csr_common::get_addr_reuse_load_after_store_wt(),
                         seq_csr_common::get_addr_reuse_load_after_load_wt(),
                         seq_csr_common::get_addr_reuse_store_after_load_wt(),
                         seq_csr_common::get_addr_reuse_store_after_store_wt());
    case (sel)
        0: return MEMBLOCK_ADDR_REUSE_LOAD_AFTER_STORE;
        1: return MEMBLOCK_ADDR_REUSE_LOAD_AFTER_LOAD;
        2: return MEMBLOCK_ADDR_REUSE_STORE_AFTER_LOAD;
        default: return MEMBLOCK_ADDR_REUSE_STORE_AFTER_STORE;
    endcase
endfunction:select_addr_reuse_kind
```

中文伪代码：

1. 用四类场景权重调用 `rand_weighted4()`，得到 0 到 3 的选择值。
2. 选择值 0 返回 `LOAD_AFTER_STORE`。
3. 选择值 1 返回 `LOAD_AFTER_LOAD`。
4. 选择值 2 返回 `STORE_AFTER_LOAD`。
5. 其他选择值返回 `STORE_AFTER_STORE`。

#### 8.3.3 `prune_recent_uid_q()`

| 项目 | 内容 |
|---|---|
| 功能目的 | 删除 recent queue 中距离当前 uid 太远的历史候选。 |
| 输入 | 一个 recent queue、当前 uid、窗口大小。 |
| 输出/副作用 | 直接修改传入的 queue，弹出过期 uid。 |
| 调用者 | `build_random_main_table()`。 |
| 内部调用 | `pop_front()`。 |
| 在流程中的作用 | 保证后续随机选择的参考 uid 一定满足窗口约束。 |

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`，函数：`prune_recent_uid_q()`

```systemverilog
function void memblock_dispatch_base_sequence::prune_recent_uid_q(ref memblock_uid_t uid_q[$],
                                                                  input memblock_uid_t cur_uid,
                                                                  input int unsigned addr_ref_window);
    while (uid_q.size() > 0) begin
        if ((cur_uid - uid_q[0]) > addr_ref_window) begin
            uid_q.pop_front();
        end else begin
            break;
        end
    end
endfunction:prune_recent_uid_q
```

中文伪代码：

1. 只要 queue 非空，就检查队头 uid。
2. 如果当前 uid 和队头 uid 的距离大于窗口，说明队头已经过期，弹出队头。
3. 如果队头仍在窗口内，因为 queue 按 uid 递增追加，后续元素也不会更老，直接停止。

#### 8.3.4 `random_pick_recent_uid()`

| 项目 | 内容 |
|---|---|
| 功能目的 | 从窗口内候选中随机选择一个参考 uid。 |
| 输入 | recent queue 和 `delete_after_pick`。 |
| 输出/副作用 | 通过 `ref_uid` 输出参考 uid，返回是否选中；当 `delete_after_pick=1` 时会删除被选候选。 |
| 调用者 | `apply_addr_reuse_window()`。 |
| 内部调用 | `$urandom_range()`、`uid_q.delete(idx)`。 |
| 在流程中的作用 | 支持同类型复用“用掉候选”和跨类型复用“保留候选”两种策略。 |

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`，函数：`random_pick_recent_uid()`

```systemverilog
function bit memblock_dispatch_base_sequence::random_pick_recent_uid(ref memblock_uid_t uid_q[$],
                                                                     output memblock_uid_t ref_uid,
                                                                     input bit delete_after_pick);
    int unsigned idx;

    ref_uid = '0;
    if (uid_q.size() == 0) begin
        return 1'b0;
    end
    idx = $urandom_range(uid_q.size() - 1, 0);
    ref_uid = uid_q[idx];
    if (delete_after_pick) begin
        uid_q.delete(idx);
    end
    return 1'b1;
endfunction:random_pick_recent_uid
```

中文伪代码：

1. 默认把输出 `ref_uid` 清成 0。
2. 如果 queue 为空，返回 0，表示没有候选。
3. 如果 queue 非空，在 `[0, size-1]` 范围随机一个下标。
4. 把该下标对应的 uid 输出给 `ref_uid`。
5. 如果 `delete_after_pick` 为 1，就从 queue 中删除这个候选。
6. 返回 1，表示成功选到参考 uid。

#### 8.3.5 `push_recent_uid()`

| 项目 | 内容 |
|---|---|
| 功能目的 | 当前 transaction 写入主表后，把当前 uid 按最终类型放入对应 recent queue。 |
| 输入 | 最终 transaction、当前 uid、load/store recent queue。 |
| 输出/副作用 | 可能向 load queue 或 store queue 追加 uid。 |
| 调用者 | `build_random_main_table()`。 |
| 内部调用 | `is_load_main_tr()`、`is_store_main_tr()`。 |
| 在流程中的作用 | 让当前 uid 成为后续 uid 的候选，并保证候选类型以地址复用后的最终 transaction 为准。 |

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`，函数：`push_recent_uid()`

```systemverilog
function void memblock_dispatch_base_sequence::push_recent_uid(input main_control_transaction tr,
                                                               input memblock_uid_t uid,
                                                               ref memblock_uid_t recent_load_uid_q[$],
                                                               ref memblock_uid_t recent_store_uid_q[$]);
    if (is_load_main_tr(tr)) begin
        recent_load_uid_q.push_back(uid);
    end else if (is_store_main_tr(tr)) begin
        recent_store_uid_q.push_back(uid);
    end
endfunction:push_recent_uid
```

中文伪代码：

1. 检查最终 transaction 是否是 load 类。
2. 如果是 load 类，把当前 uid 追加到 `recent_load_uid_q`。
3. 如果不是 load 类，再检查是否是 store 类。
4. 如果是 store 类，把当前 uid 追加到 `recent_store_uid_q`。
5. 如果两类都不是，不追加；后续地址复用不会把它作为 load/store 候选。

#### 8.3.6 `validate_main_table_entry()`

| 项目 | 内容 |
|---|---|
| 功能目的 | 对每条生成或修正后的主表 transaction 做最终一致性检查。 |
| 输入 | transaction 和调用者字符串。 |
| 输出/副作用 | 正常返回表示合法；非法时 `uvm_fatal` 停止，避免错误激励进入后续流程。 |
| 调用者 | `randomize_main_transaction()`、`fixup_after_addr_reuse()`、手动主表导入检查路径。 |
| 内部调用 | `validate_main_transaction()`、`is_vector_ls_main_tr()`、`derive_op_behavior()`、`is_*_fuoptype()`。 |
| 在流程中的作用 | 是地址复用后最后一道防线，防止类型修正只改一部分字段导致 DUT 激励不合法。 |

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`，函数：`validate_main_table_entry()`

```systemverilog
function void memblock_dispatch_base_sequence::validate_main_table_entry(input main_control_transaction tr,
                                                                         input string caller);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), $sformatf("%s got null transaction", caller))
    end
    if (!tr.validate_main_transaction()) begin
        `uvm_fatal(get_type_name(), $sformatf("%s got invalid derived fields uid=%0d", caller, tr.uid))
    end
    if (tr.robIdx_value >= MEMBLOCK_ROB_SIZE) begin
        `uvm_fatal(get_type_name(), $sformatf("%s uid=%0d robIdx_value=%0d exceeds ROB size=%0d",
                                              caller, tr.uid, tr.robIdx_value, MEMBLOCK_ROB_SIZE))
    end
    if (is_vector_ls_main_tr(tr)) begin
        `uvm_fatal(get_type_name(), $sformatf("%s uid=%0d vector LS is not supported in Task5", caller, tr.uid))
    end
    begin
        memblock_op_behavior_t behavior;
        behavior = derive_op_behavior(tr);
        if (tr.numLsElem != behavior.num_ls_elem) begin
            `uvm_fatal(get_type_name(), $sformatf("%s uid=%0d numLsElem=%0d expected=%0d",
                                                  caller, tr.uid, tr.numLsElem, behavior.num_ls_elem))
        end
    end

    case (tr.op_class)
        MEMBLOCK_OP_CLASS_INT_LOAD,
        MEMBLOCK_OP_CLASS_FP_LOAD: begin
            if (tr.fuType != MEMBLOCK_FUTYPE_LDU ||
                tr.lsq_flow != MEMBLOCK_LSQ_FLOW_LOAD ||
                !is_load_fuoptype(tr.fuOpType)) begin
                `uvm_fatal(get_type_name(), $sformatf("%s uid=%0d has illegal load template", caller, tr.uid))
            end
        end
        MEMBLOCK_OP_CLASS_STORE: begin
            if (tr.fuType != MEMBLOCK_FUTYPE_STU ||
                tr.lsq_flow != MEMBLOCK_LSQ_FLOW_STORE ||
                !is_store_fuoptype(tr.fuOpType)) begin
                `uvm_fatal(get_type_name(), $sformatf("%s uid=%0d has illegal store template", caller, tr.uid))
            end
        end
        MEMBLOCK_OP_CLASS_PREFETCH: begin
            if (tr.fuType != MEMBLOCK_FUTYPE_LDU ||
                tr.lsq_flow != MEMBLOCK_LSQ_FLOW_LOAD ||
                !is_prefetch_fuoptype(tr.fuOpType)) begin
                `uvm_fatal(get_type_name(), $sformatf("%s uid=%0d has illegal prefetch template", caller, tr.uid))
            end
        end
        MEMBLOCK_OP_CLASS_AMO: begin
            if (tr.fuType != MEMBLOCK_FUTYPE_MOU ||
                tr.lsq_flow != MEMBLOCK_LSQ_FLOW_ATOMIC ||
                !is_amo_fuoptype(tr.fuOpType)) begin
                `uvm_fatal(get_type_name(), $sformatf("%s uid=%0d has illegal AMO template", caller, tr.uid))
            end
        end
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("%s uid=%0d has unsupported op_class=%0d",
                                                  caller, tr.uid, tr.op_class))
        end
    endcase
endfunction:validate_main_table_entry
```

中文伪代码：

1. 如果 transaction 为空，直接 fatal。
2. 调用 transaction 自身派生字段检查；失败则 fatal。
3. 检查 ROB value 是否超过 ROB 容量；超过则 fatal。
4. 当前 Task5 不支持 vector LS，检测到 vector LS 直接 fatal。
5. 调用 `derive_op_behavior()` 推导该 transaction 应有的行为，并检查 `numLsElem` 是否匹配。
6. 按 `op_class` 分支检查模板：load 必须是 LDU/LOAD/合法 load `fuOpType`。
7. store 必须是 STU/STORE/合法 store `fuOpType`。
8. prefetch 必须是 LDU/LOAD/合法 prefetch `fuOpType`。
9. AMO 必须是 MOU/ATOMIC/合法 AMO `fuOpType`。
10. 不支持的 `op_class` 直接 fatal。

#### 8.3.7 `apply_minimal_op_template()`

| 项目 | 内容 |
|---|---|
| 功能目的 | 根据 `op_class` 重建一组最小合法的 `fuType/fuOpType/lsq_flow/numLsElem` 字段。 |
| 输入 | 已经设置好 `op_class` 的 `main_control_transaction tr`。 |
| 输出/副作用 | 直接修改传入 transaction 的执行单元、LSQ flow、具体操作编码和 LSQ 元素数。非法输入会 fatal。 |
| 调用者 | `randomize_main_transaction()` 和 `set_transaction_ls_kind()`。 |
| 内部调用 | `random_load_fuoptype()`、`random_store_fuoptype()`、`random_prefetch_fuoptype()`、`random_amo_fuoptype()`。 |
| 在流程中的作用 | 地址复用可能把当前 transaction 从 load 改 store 或从 store 改 load；该函数保证改类型后不会残留旧类型字段。 |

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`，函数：`apply_minimal_op_template()`

```systemverilog
function void memblock_dispatch_base_sequence::apply_minimal_op_template(input main_control_transaction tr);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "apply_minimal_op_template got null transaction")
    end

    case (tr.op_class)
        MEMBLOCK_OP_CLASS_INT_LOAD,
        MEMBLOCK_OP_CLASS_FP_LOAD: begin
            tr.fuType   = MEMBLOCK_FUTYPE_LDU;
            tr.lsq_flow = MEMBLOCK_LSQ_FLOW_LOAD;
            tr.fuOpType = random_load_fuoptype();
            tr.numLsElem = 5'd1;
        end
        MEMBLOCK_OP_CLASS_STORE: begin
            tr.fuType   = MEMBLOCK_FUTYPE_STU;
            tr.lsq_flow = MEMBLOCK_LSQ_FLOW_STORE;
            tr.fuOpType = random_store_fuoptype();
            tr.numLsElem = 5'd1;
        end
        MEMBLOCK_OP_CLASS_PREFETCH: begin
            tr.fuType   = MEMBLOCK_FUTYPE_LDU;
            tr.lsq_flow = MEMBLOCK_LSQ_FLOW_LOAD;
            tr.fuOpType = random_prefetch_fuoptype();
            tr.numLsElem = 5'd1;
        end
        MEMBLOCK_OP_CLASS_AMO: begin
            tr.fuType   = MEMBLOCK_FUTYPE_MOU;
            tr.lsq_flow = MEMBLOCK_LSQ_FLOW_ATOMIC;
            tr.fuOpType = random_amo_fuoptype();
            tr.numLsElem = 5'd0;
        end
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("unsupported op_class=%0d", tr.op_class))
        end
    endcase
endfunction:apply_minimal_op_template
```

中文伪代码：

1. 如果 transaction 为空，直接 fatal。
2. 如果 op class 是整数 load 或浮点 load，把 `fuType` 设为 LDU、`lsq_flow` 设为 LOAD，随机一个合法 load `fuOpType`，并把 `numLsElem` 设为 1。
3. 如果 op class 是 store，把 `fuType` 设为 STU、`lsq_flow` 设为 STORE，随机一个合法 store `fuOpType`，并把 `numLsElem` 设为 1。
4. 如果 op class 是 prefetch，把 `fuType` 设为 LDU、`lsq_flow` 设为 LOAD，随机一个合法 prefetch `fuOpType`，并把 `numLsElem` 设为 1。
5. 如果 op class 是 AMO，把 `fuType` 设为 MOU、`lsq_flow` 设为 ATOMIC，随机一个合法 AMO `fuOpType`，并把 `numLsElem` 设为 0。
6. 如果 op class 不在支持范围内，直接 fatal。

### 8.4 Plan 关键项源码证据映射

| Plan 关键项 | 源码证据位置 | 伪代码/说明所在章节 |
|---|---|---|
| recent queue 只保存 uid | `build_random_main_table()` 的 `memblock_uid_t recent_load_uid_q[$]` 和 `recent_store_uid_q[$]` | 第 1 章源码块、第 8.1 调用表 |
| 窗口按 uid 距离 | `prune_recent_uid_q()` | 第 8.3.3 |
| 支持四类 after 场景 | `memblock_addr_reuse_kind_e` 和 `select_addr_reuse_kind()` | 第 2 章源码块、第 8.3.2 |
| 同类型复用删除候选、跨类型复用保留候选 | `random_pick_recent_uid(..., delete_after_pick)` | 第 8.3.4 |
| 当前项按最终类型入 recent queue | `push_recent_uid()` | 第 8.3.5 |
| 类型修正分层 | `set_transaction_ls_kind()`、`fixup_after_addr_reuse()`、`apply_minimal_op_template()` | 第 2 章源码块、第 8.3.7 |
| 手动主表不执行地址复用 | `build_main_table()`、`import_manual_main_table()` | 第 1 章“替换类修改说明” |
| 新 plus 参数和默认 5% enable | `plus.sv` 参数定义、`seq_csr_common::validate_and_clamp()` | 第 3 章源码块、第 0.2 参数表 |
| ROB 起点固定/随机 value | `choose_rob_start_key()` | 第 4 章源码块 |
| 最终合法性检查 | `validate_main_table_entry()` | 第 8.3.6 |

### 8.5 修改文件覆盖性

| 文件 | 覆盖结论 |
|---|---|
| `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv` | 已覆盖主表生成、地址复用、类型修正、ROB 起点和旧后处理删除。 |
| `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv` | 已覆盖新增参数快照、getter、合法性检查和 clamp/fatal 策略。 |
| `mem_ut/ver/ut/memblock/env/plus.sv` | 已覆盖新增 plus 参数和旧 plus key 删除。 |
| `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_types.sv` | 已覆盖新增 `memblock_addr_reuse_kind_e`。 |
| `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg` | 已覆盖默认参数值。 |
| `AI_DOC/analysis/source_sv/dispatch_framework_sv/*.md` | 已同步 base sequence、seq csr、plus 和公共类型说明。 |
| `AI_DOC/web/*callgraph*` | 已同步网页调用关系、函数说明和地址复用流程 lane。 |

## 9. Plan 对齐检查

对应 plan 路径：

- `AI_DOC/plan/test_framework/plan/do/main_table_addr_reuse_window_plan_20260614.md`

已按该 plan 检查以下核心要求：

| Plan 要求 | 当前实现 | 结论 |
|---|---|---|
| 使用 `recent_load_uid_q` / `recent_store_uid_q`，queue 只保存 uid。 | `build_random_main_table()` 中维护两个 `memblock_uid_t` queue。 | 已落实。 |
| 窗口按 uid 距离，不按 queue size。 | `prune_recent_uid_q()` 使用 `cur_uid - uid_q[0] > addr_ref_window` 淘汰。 | 已落实。 |
| 支持四类场景枚举。 | `memblock_addr_reuse_kind_e` 已包含四类 after 枚举。 | 已落实。 |
| 同类型复用取候选后删除，跨类型复用保留候选。 | `random_pick_recent_uid(..., delete_after_pick)` 按场景传入 `1'b1/1'b0`。 | 已落实。 |
| 当前项写主表后再按最终类型入 recent queue。 | `data.set_main_transaction()` 在 `push_recent_uid()` 前。 | 已落实。 |
| 类型修正拆成两个层次。 | `set_transaction_ls_kind()` 修正类型模板，`fixup_after_addr_reuse()` 复制地址并校验。 | 已落实。 |
| 手动主表默认不执行地址复用 directed 修正。 | `import_manual_main_table()` 不再调用任何地址复用入口。 | 已落实。 |
| 删除旧后处理 task 和旧参数入口。 | 旧 task、旧 getter、旧 plus key 已从活代码删除。 | 已落实。 |
| ROB 起始 value 支持固定/随机，初始 flag 固定 0。 | `choose_rob_start_key()` 实现固定/三段随机 value，`key.flag=0`。 | 已落实。 |
| 默认 enable 约 5%，同类复用默认 0。 | `EN_1=1`、`EN_0=19`、`LOAD_AFTER_LOAD=0`、`STORE_AFTER_STORE=0`。 | 已落实。 |

## 10. 实现与 Plan 不一致项

### 10.1 all-zero 权重处理策略比 Plan 更严格

| 项目 | 内容 |
|---|---|
| Plan 原有逻辑 | plan 第 11 节写到 enable 权重全 0 时 warning 并回退到 `EN_0=1`，枚举权重全 0 时 warning 并回退到跨类型默认权重。 |
| 当前源码逻辑 | `seq_csr_common::validate_and_clamp()` 对 enable 权重、四类场景权重和窗口权重全 0 使用 `fatal_if_all_zero*()`，直接 fatal。 |
| 不一致原因 | coding 采用更严格的参数错误暴露策略，避免 testcase 配置错误被 warning fallback 静默掩盖。该策略与此前 plus 权重非法值保护“clamp 或 fatal”的项目规则一致，但和本 plan 的 warning fallback 描述不完全一致。 |
| 源码位置 | `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`，函数：`validate_and_clamp()`。 |

```systemverilog
fatal_if_all_zero2("addr_reuse_en weights", addr_reuse_en_1_wt, addr_reuse_en_0_wt);
fatal_if_all_zero4("addr_reuse kind weights",
                   addr_reuse_load_after_store_wt,
                   addr_reuse_load_after_load_wt,
                   addr_reuse_store_after_load_wt,
                   addr_reuse_store_after_store_wt);
if (addr_ref_window_fixed > 0) begin
    clamp_int("addr_ref_window_fixed", addr_ref_window_fixed, 1, get_addr_ref_window_max());
end else begin
    fatal_if_all_zero3("addr_ref_window weights",
                       addr_ref_window_small_weight,
                       addr_ref_window_medium_weight,
                       addr_ref_window_large_weight);
end
```

中文伪代码：

1. 检查地址复用 enable 的两个权重是否全 0；如果全 0，立即 fatal。
2. 检查四类地址复用场景权重是否全 0；如果全 0，立即 fatal。
3. 如果用户设置了固定窗口，则把固定窗口 clamp 到合法范围。
4. 如果没有设置固定窗口，则检查 small/medium/large 窗口权重是否全 0；如果全 0，立即 fatal。

处理结论：

```text
保持当前源码实现；建议后续回写 plan，把 all-zero 权重策略从 warning fallback 更新为 fatal，
这样配置错误会在 seq_csr_common 初始化阶段暴露，定位比随机化失败更直接。
```

## 11. Plan 未说明但 Coding 落实的细节

### 11.1 `apply_minimal_op_template()` 显式重置 `numLsElem`

| 项目 | 内容 |
|---|---|
| 细节功能 | load/store/prefetch 模板中显式设置 `numLsElem=5'd1`，AMO 模板保持 `numLsElem=5'd0`。 |
| 为什么 plan 未覆盖 | plan 只写了“复用现有 apply_minimal_op_template 或等价公共 helper”，没有展开 AMO 随机后又被地址复用改成 load/store 时的旧字段残留问题。 |
| 在本特性中的作用 | 如果某条 transaction 初始随机为 AMO，`apply_minimal_op_template()` 会把 `numLsElem` 设为 0；地址复用可能再把它改成 load/store。显式重置可防止 load/store 仍携带 AMO 的 `numLsElem=0`，避免 `validate_main_table_entry()` fatal。 |
| 源码位置 | `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`，函数：`apply_minimal_op_template()`。 |

```systemverilog
case (tr.op_class)
    MEMBLOCK_OP_CLASS_INT_LOAD,
    MEMBLOCK_OP_CLASS_FP_LOAD: begin
        tr.fuType   = MEMBLOCK_FUTYPE_LDU;
        tr.lsq_flow = MEMBLOCK_LSQ_FLOW_LOAD;
        tr.fuOpType = random_load_fuoptype();
        tr.numLsElem = 5'd1;
    end
    MEMBLOCK_OP_CLASS_STORE: begin
        tr.fuType   = MEMBLOCK_FUTYPE_STU;
        tr.lsq_flow = MEMBLOCK_LSQ_FLOW_STORE;
        tr.fuOpType = random_store_fuoptype();
        tr.numLsElem = 5'd1;
    end
    MEMBLOCK_OP_CLASS_PREFETCH: begin
        tr.fuType   = MEMBLOCK_FUTYPE_LDU;
        tr.lsq_flow = MEMBLOCK_LSQ_FLOW_LOAD;
        tr.fuOpType = random_prefetch_fuoptype();
        tr.numLsElem = 5'd1;
    end
    MEMBLOCK_OP_CLASS_AMO: begin
        tr.fuType   = MEMBLOCK_FUTYPE_MOU;
        tr.lsq_flow = MEMBLOCK_LSQ_FLOW_ATOMIC;
        tr.fuOpType = random_amo_fuoptype();
        tr.numLsElem = 5'd0;
    end
endcase
```

中文伪代码：

1. 如果 op class 是整数 load 或浮点 load，把功能单元设为 LDU、LSQ flow 设为 LOAD，随机合法 load `fuOpType`，并把 `numLsElem` 设为 1。
2. 如果 op class 是 store，把功能单元设为 STU、LSQ flow 设为 STORE，随机合法 store `fuOpType`，并把 `numLsElem` 设为 1。
3. 如果 op class 是 prefetch，把功能单元设为 LDU、LSQ flow 设为 LOAD，随机合法 prefetch `fuOpType`，并把 `numLsElem` 设为 1。
4. 如果 op class 是 AMO，把功能单元设为 MOU、LSQ flow 设为 ATOMIC，随机合法 AMO `fuOpType`，并把 `numLsElem` 设为 0。

是否需要回写 plan：

```text
建议回写 plan 的类型修正章节，明确 apply_minimal_op_template() 必须覆盖 numLsElem，
因为地址复用会改变 op_class，不能假设旧随机模板字段仍然合法。
```

## 12. 非本次修改的逻辑分析

### 12.1 `git status` 对比结论

已按 review 规则执行 `git status --short` 对比。本 review 的主题是“主表地址复用 recent-window 实现”，覆盖范围限定为地址复用生成逻辑、地址复用相关 plus/seq_csr 参数、公共类型枚举、默认 cfg、对应 source_sv/web 文档同步和本 review 文档。

本次 review 已覆盖的文件和逻辑：

| 文件/目录 | 覆盖结论 |
|---|---|
| `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv` | 覆盖 `build_random_main_table()`、地址复用 helper、ROB 起点、手动主表不执行地址复用、旧后处理删除。 |
| `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_types.sv` | 覆盖 `memblock_addr_reuse_kind_e`。 |
| `mem_ut/ver/ut/memblock/env/plus.sv` | 覆盖地址复用和 ROB 起点 plus 参数。 |
| `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv` | 覆盖地址复用和 ROB 起点参数快照、getter、clamp/fatal 检查。 |
| `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg` | 覆盖地址复用默认约 5% enable、四类场景默认权重、窗口和 ROB 起点默认值。 |
| `AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_dispatch_base_sequence.md`、`memblock_dispatch_types.md`、`plus.md`、`seq_csr_common.md` | 覆盖地址复用相关源码分析文档同步。 |
| `AI_DOC/web/memblock_dispatch_control_flow_callgraph.md`、`AI_DOC/web/web_assets/memblock_dispatch_doc.js`、`AI_DOC/web/memblock_dispatch_control_flow_callgraph/assets/app.js`、`AI_DOC/web/memblock_dispatch_control_flow_callgraph_enhanced/assets/app.js` | 覆盖地址复用相关网页调用关系同步。 |
| `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md` | 覆盖地址复用 plus 参数迁移说明。 |
| `AI_DOC/plan/test_framework/plan/do/main_table_addr_reuse_window_plan_20260614.md` | 覆盖本次 plan 从 undo 移到 do 的状态。 |
| `AI_DOC/plan/test_framework/review_doc/do/main_table_addr_reuse_window_review_20260614.md` | 本 review 文档本身。 |

### 12.2 `git status` 中非本次 review 主题的修改

下表记录当前工作区存在但不属于本次地址复用 review 的修改。这些修改没有纳入本 review 的功能正确性分析，需要由对应功能的 review 文档或后续任务单独处理。

| 类别 | 文件/目录 | 判断 | 原因 |
|---|---|---|---|
| 仓库/agent 配置 | `AGENTS.md` | 非本次逻辑 | 当前任务只涉及 mem_ut 地址复用 review 文档，`AGENTS.md` 属于仓库协作规则调整。 |
| AI_DOC 目录搬迁 | 大量 `AI_DOC/design_plan/* -> AI_DOC/analysis/*`、`AI_DOC/mem_ut_flow_doc/*`、`AI_DOC/web/*` rename | 非本次功能逻辑 | 这些是文档目录结构调整或历史文档搬迁；除 12.1 列出的地址复用相关文档外，不作为本 review 的源码逻辑覆盖对象。 |
| 其他 plan/review 文档 | `dispatch_100k_performance_optimization_*`、`dispatch_backend_interface_closure_*`、`dispatch_plan_v2_*`、`sfence_hfence_tlb_invalidate_review_checklist.md` 等 | 另行 review | 属于 100k 性能、backend closure、dispatch v2、sfence/hfence 等其他主题，不属于地址复用 recent-window。 |
| 其他项目管理规则 | `AI_DOC/project_management/mem_ut_flow_document_rule.md`、`mem_ut_web_doc_sync_rule.md`、`ai_doc_file_management_rule.md`、`mem_ut_test_framework_logic_build_rule.md` | 非本次地址复用逻辑 | 这些规则涉及 flow 文档、网页同步或 AI_DOC 管理；本次只修改 `mem_ut_code_review_document_rule.md` 以补充 git status 对比规则。 |
| 100k/global-stop 公共状态 | `mem_ut/ver/ut/memblock/seq/base_seq/common_data_transaction.sv` | 另行 review | 当前 diff 中新增 `global_stop_requested` 和 `transaction_done()/request_global_stop_if_done()`，属于 active sequence/global stop/100k 收敛逻辑，不属于主表地址复用。 |
| issue route work 判断 | `mem_ut/ver/ut/memblock/seq/base_seq/issue_queue_scheduler.sv` | 另行 review | 后续 active sequence/global stop review 已确认 `uid_has_pending_route_work()` 未接入主流程并删除；该变化不属于地址复用主表生成逻辑。 |
| 同文件混合修改 | `mem_ut/ver/ut/memblock/env/plus.sv`、`mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv` | 部分覆盖，部分另行 review | 本 review 只覆盖其中地址复用/ROB 起点参数；同文件中 `MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES`、`MEMBLOCK_*MAX_CYCLES/IDLE_STOP/START_TIMEOUT` 删除、`MEMBLOCK_LSQENQ_SEQ_EN` 默认值变化等属于 active sequence 退出策略调整，需要另行 review。 |
| active sequence 退出策略 | `memblock_dispatch_real_smoke_sequence.sv`、`memblock_lsqenq_dispatch_sequence.sv`、`memblock_lintsissue_dispatch_sequence.sv`、`memblock_lsqcommit_dispatch_sequence.sv` | 另行 review | 当前 diff 涉及 global stop、no-progress warning、idle item、start timeout 删除等 active sequence 生命周期调整，不属于地址复用 recent-window。 |
| testcase cfg 退出参数清理 | `tc_dispatch_real_*.cfg` 系列 | 另行 review | 当前 diff 删除 max_cycles/start_timeout/idle_stop 并加入 `MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES`，属于 sequence 退出策略配置，不属于地址复用默认参数。 |
| 新增 flow/todo 文档 | `AI_DOC/mem_ut_flow_doc/load_sta_std_issue_flow.md`、`mem_ut_data_persistence_todo_20260614.md`、`mem_ut_supported_vs_dut_required_flows.md`、`active_sequence_global_stop_lifecycle_review_20260614.md` | 另行 review | 属于 issue flow、数据持久化、支持场景或 active sequence 生命周期文档，不属于本 review。 |
| 仿真/生成产物 | `build_memblock/`、`coupledL2/`、`openLLC/`、`verdiLog/`、`novas.conf`、`novas.rc`、`.codex` | 非源码 review | 这些是生成输出、工具配置或本地工作目录，不应作为本次源码 review 覆盖对象。 |

### 12.3 是否存在本次逻辑遗漏

基于本次 `git status` 对比，未发现“属于主表地址复用 recent-window 但未在本 review 文档分析”的源码逻辑遗漏。

需要注意的是，当前工作区包含多组并行修改。本 review 只证明地址复用 recent-window 相关修改已经被分析；`global_stop_requested`、active sequence 退出策略、100k 扫描优化、文档目录搬迁等应由各自 review 文档单独确认。
