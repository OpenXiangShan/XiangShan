# send_pri/pipe mode 实现 Review

## 1. Review 范围

关联 plan：

- `AI_DOC/plan/test_framework/plan/do/send_pri_pipe_mode_plan_20260616.md`

本次 review 覆盖的修改文件：

- `mem_ut/ver/ut/memblock/env/plus.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/issue_queue_scheduler.sv`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/tc_dispatch_real_mixed_sta_wb_smoke.cfg`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/tc_dispatch_real_mixed_wb_smoke.cfg`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/tc_dispatch_real_multi_store_wb_smoke.cfg`
- `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/seq_csr_common.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/issue_queue_scheduler.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_dispatch_base_sequence.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_dispatch_types.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_lintsissue_dispatch_sequence.md`
- `AI_DOC/mem_ut_flow_doc/load_sta_std_issue_flow.md`
- `AI_DOC/web/memblock_dispatch_control_flow_callgraph.md`
- `AI_DOC/web/memblock_dispatch_control_flow_callgraph/assets/app.js`
- `AI_DOC/web/memblock_dispatch_control_flow_callgraph_enhanced/assets/app.js`
- `AI_DOC/web/web_assets/memblock_dispatch_doc.js`

## 2. 功能特性

本次实现把旧的单一 send_pri 控制拆成两个语义：

| 新对象 | 通俗解释 | 代码落点 |
|---|---|---|
| `MEMBLOCK_SEND_PRI_MODE_EN` | 是否启用 send_pri 仲裁体系。关闭时 priority 只取默认值，issue 只按 ROB 年龄。 | `plus.sv`、`seq_csr_common.sv` |
| `MEMBLOCK_GLOBAL_SEND_PRI_EN_WT` | send_pri 模式下，每拍是否做跨 LOAD/STA/STD 全局最大 priority 过滤的采样权重。 | `plus.sv`、`seq_csr_common.sv` |
| `MEMBLOCK_*_PIP_NUM_LIMIT` | LOAD/STA/STD 每拍最多发射数量的配置上限。 | `plus.sv`、`seq_csr_common.sv`、`plus_cfg/*.cfg` |
| `MEMBLOCK_*_PIP_NUM_RANDOM_EN` | 是否每拍在 `[1:LIMIT]` 内随机实际 pipe 数。 | `plus.sv`、`seq_csr_common.sv` |

修改前逻辑：

- priority 生成和全局过滤绑定在旧全局开关上，无法表达“比较 priority 但不做跨队列 global filter”。
- `get_load_pip_num()` 等 getter 固定返回配置值，调度路径无法每拍随机 pipe 数。

修改后逻辑：

- `randomize_send_pri_value()` 只看 `get_send_pri_mode_en()`。
- `issue_queue_scheduler::select_issue_candidates()` 中 `compare_pri` 和 `use_global_pri` 分离。
- `sample_*_pip_num()` 是调度路径唯一读取本拍 pipe 数的入口。
- 找不到 global priority 时，`use_global_pri` 退回 0，避免空队列或无 eligible item 时误过滤。

### 2.1 替换对象完整说明

本次属于典型替换类修改，旧对象和新对象的关系如下。

| 旧对象 | 旧对象是什么 | 旧行为 | 旧对象问题 | 新对象 | 替代方式 | 状态一致性来源 | 例子 |
|---|---|---|---|---|---|---|---|
| `MEMBLOCK_SEND_PRI_EN` / `send_pri_en` / `get_send_pri_en()` | 一个 runtime bit 参数、一个 `seq_csr_common` 静态快照字段、一个 getter。 | 同时控制主表是否随机生成 `send_pri`，也控制 issue select 是否跨 LOAD/STA/STD 三个队列做全局最大 priority 过滤。 | 一个 bit 同时表达两个不同问题：是否比较 priority、是否全局过滤。这样无法表达“每个队列内部按 priority 仲裁，但不跨队列做全局过滤”。 | `MEMBLOCK_SEND_PRI_MODE_EN` / `send_pri_mode_en` / `get_send_pri_mode_en()` 和 `MEMBLOCK_GLOBAL_SEND_PRI_EN_WT` / `global_send_pri_en_wt` / `sample_global_send_pri_en()` | 主表 priority 生成只读 `get_send_pri_mode_en()`；issue select 先读 `get_send_pri_mode_en()` 得到 `compare_pri`，再用 `sample_global_send_pri_en()` 决定本拍是否启用 global filter。 | `seq_csr_common` 是参数快照真源；主表生成、issue scheduler 都只通过它读取参数。 | `MEMBLOCK_SEND_PRI_MODE_EN=1`、`MEMBLOCK_GLOBAL_SEND_PRI_EN_WT=0` 时，主表仍随机 priority，LOAD/STA/STD 各队列内部先按 priority 选，但不会强制三个队列只发全局最大 priority。 |
| `MEMBLOCK_LOAD_PIP_NUM` / `MEMBLOCK_STA_PIP_NUM` / `MEMBLOCK_STD_PIP_NUM` | 三个 runtime int 参数，旧 getter 名为 `get_*_pip_num()`。 | 固定返回每拍 LOAD/STA/STD 最多发射数量。调用点看名字容易把它理解成“本拍实际采样结果”。 | 后续要支持每拍随机 pipe 数时，固定配置值和本拍实际值混在一起，调用点不清晰。 | `MEMBLOCK_*_PIP_NUM_LIMIT`、`MEMBLOCK_*_PIP_NUM_RANDOM_EN`、`get_*_pip_num_limit()`、`sample_*_pip_num()` | cfg 中配置 LIMIT 和 RANDOM_EN；调度路径必须调用 `sample_*_pip_num()`，其它只需要配置上限的路径才读 `get_*_pip_num_limit()`。 | `seq_csr_common` 保存 limit 和 random_en；`validate_and_clamp()` 把 limit 收敛到真实 DUT pipe 数范围内。 | `MEMBLOCK_LOAD_PIP_NUM_LIMIT=3`、`MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN=1` 时，LOAD 每拍实际可选数量在 `[1:3]` 内随机；关闭随机时固定为 3。 |

### 2.2 术语解释

| 术语 | 通俗解释 | 代码落点 |
|---|---|---|
| send_pri 仲裁体系 | issue 选候选时把 `send_pri` 当作优先级；priority 高的 item 优先，priority 相同再按 ROB 年龄。 | `MEMBLOCK_SEND_PRI_MODE_EN`、`issue_queue_scheduler::item_is_better()` |
| global priority filter | 本拍先跨 LOAD/STA/STD 三个队列找所有可发射 item 的最大 `send_pri`，再只允许这个最大 priority 的 item 参与各队列选择。 | `MEMBLOCK_GLOBAL_SEND_PRI_EN_WT`、`sample_global_send_pri_en()`、`find_global_max_send_pri()` |
| pipe LIMIT | 某类 issue target 本拍最多允许选择多少条候选的配置上限，不等于随机模式下的本拍实际数量。 | `load_pip_num_limit`、`sta_pip_num_limit`、`std_pip_num_limit` |
| sample pipe num | 每拍真正用于 issue select 的 LOAD/STA/STD 数量；随机开关关闭时等于 LIMIT，打开时在 `[1:LIMIT]` 内随机。 | `sample_load_pip_num()`、`sample_sta_pip_num()`、`sample_std_pip_num()` |
| eligible item | 当前拍允许被 issue select 看到的队列项，需要 active/enq/TLB/flush/replay/delay/target 状态都满足。 | `issue_queue_scheduler::is_issue_item_eligible()` |

## 3. `plus.sv` 参数入口

源码位置：`mem_ut/ver/ut/memblock/env/plus.sv`，函数/task：参数声明区。

函数功能简析：`plus` class 保存命令行 plusarg 的默认值和解析后值。这里新增/替换字段，让运行命令和 cfg 能表达 send_pri 三模式和 pipe 随机模式。

修改后源码：

```systemverilog
// LOAD/STA/STD issue pipe配置上限；随机开关关闭时固定使用该上限，打开时每拍从[1:上限]采样。
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_LOAD_PIP_NUM_LIMIT, int, 3)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_STA_PIP_NUM_LIMIT, int, 2)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_STD_PIP_NUM_LIMIT, int, 2)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN, bit, 1'b0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_STA_PIP_NUM_RANDOM_EN, bit, 1'b0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_STD_PIP_NUM_RANDOM_EN, bit, 1'b0)

// send_pri仲裁总开关；关闭时主表使用default priority，issue只按ROB age。
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_SEND_PRI_MODE_EN, bit, 1'b1)
// send_pri模式下每拍进入global priority filter的权重，范围[0:100]。
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_GLOBAL_SEND_PRI_EN_WT, int, 0)
```

中文伪代码：

这段逻辑负责声明新的 runtime plus 参数，使 cfg 和命令行可以控制 issue priority 与 pipe 数采样。
先把旧 pipe 数名字迁移为 LIMIT，表示配置上限；再新增 RANDOM_EN，表示本拍是否随机实际发射数量；随后新增 send_pri 总模式开关和 global 采样权重。
这些字段由 `reload_from_cmdline()` 解析，后续 `seq_csr_common::load_from_plus()` 快照并检查合法性。

关键分支/副作用说明：

- 删除旧 `MEMBLOCK_SEND_PRI_EN` 入口，避免它继续表达“是否随机 priority”和“是否 global filter”两个语义。
- 默认 `MEMBLOCK_SEND_PRI_MODE_EN=1`、`MEMBLOCK_GLOBAL_SEND_PRI_EN_WT=0`，即默认进入 send_pri + non-global 模式。

源码位置：`mem_ut/ver/ut/memblock/env/plus.sv`，函数/task：`reload_from_cmdline()`。

函数功能简析：该函数从命令行读取 plusarg，覆盖参数默认值。输入来自 VCS plusarg；副作用是更新 `plus::MEMBLOCK_*` 静态字段。

修改后源码：

```systemverilog
load_int("MEMBLOCK_LOAD_PIP_NUM_LIMIT", MEMBLOCK_LOAD_PIP_NUM_LIMIT);
load_int("MEMBLOCK_STA_PIP_NUM_LIMIT", MEMBLOCK_STA_PIP_NUM_LIMIT);
load_int("MEMBLOCK_STD_PIP_NUM_LIMIT", MEMBLOCK_STD_PIP_NUM_LIMIT);
load_bit("MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN", MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN);
load_bit("MEMBLOCK_STA_PIP_NUM_RANDOM_EN", MEMBLOCK_STA_PIP_NUM_RANDOM_EN);
load_bit("MEMBLOCK_STD_PIP_NUM_RANDOM_EN", MEMBLOCK_STD_PIP_NUM_RANDOM_EN);

load_bit("MEMBLOCK_SEND_PRI_MODE_EN", MEMBLOCK_SEND_PRI_MODE_EN);
load_int("MEMBLOCK_GLOBAL_SEND_PRI_EN_WT", MEMBLOCK_GLOBAL_SEND_PRI_EN_WT);
```

中文伪代码：

这段逻辑负责把新增参数从命令行读入 `plus` 静态字段。
函数按参数组顺序读取 pipe LIMIT，再读取 pipe RANDOM_EN；随后读取 send_pri 模式开关和 global 权重；这些读取不会做最终范围检查，只完成字符串 plusarg 到 typed 字段的转换。
后续 `seq_csr_common::load_from_plus()` 会读取这些字段，`validate_and_clamp()` 会做非负、范围和权重合法性处理。

关键分支/副作用说明：

- 不保留旧参数兼容，因此旧 `+MEMBLOCK_SEND_PRI_EN` 和旧 `+MEMBLOCK_*_PIP_NUM` 不再被解析。
- cfg key 必须同步迁移，否则 Makefile 展开出的旧 plusarg 将不再影响框架。

## 4. `seq_csr_common.sv` 参数快照与采样

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`，函数/task：字段定义区。

函数功能简析：字段定义区保存参数快照。输入来自 `plus::MEMBLOCK_*`，读取者是主表生成、issue scheduler 和真实 sequence。

修改后源码：

```systemverilog
// LOAD/STA/STD issue pipe配置上限；由plus加载并被真实pipe数clamp。
// 调度路径不直接把它当本拍发射数，必须通过sample_*_pip_num()按随机开关采样。
static int unsigned load_pip_num_limit = 3;
static int unsigned sta_pip_num_limit = 2;
static int unsigned std_pip_num_limit = 2;
// pipe随机开关：0表示每拍固定返回*_pip_num_limit；1表示每拍在[1:limit]内随机。
static bit          load_pip_num_random_en = 1'b0;
static bit          sta_pip_num_random_en = 1'b0;
static bit          std_pip_num_random_en = 1'b0;

// send_pri仲裁总开关：0时主表使用default priority，issue只按ROB age；1时主表随机priority且issue比较priority。
static bit          send_pri_mode_en = 1'b1;
// send_pri模式下本拍启用global priority filter的随机权重；sample_global_send_pri_en()读取它。
static int unsigned global_send_pri_en_wt = 0;
```

中文伪代码：

这段逻辑负责定义新的参数真源快照。
先保存三类 target 的 pipe LIMIT，再保存三类 RANDOM_EN；调度时不直接读 LIMIT，而是调用 sample 函数获得本拍实际数量；随后保存 send_pri 总开关和 global 权重。
这些字段没有运行期清除动作，只有 `load_from_plus()` 和 `reload_from_plus()` 会刷新；getter/sample 函数读取它们并先检查初始化状态。

关键分支/副作用说明：

- 旧对象 `send_pri_en` 是单个 bit，原来既影响 priority 生成，也影响 issue global filter。
- 新对象 `send_pri_mode_en` 只表示是否使用 send_pri 仲裁体系；新对象 `global_send_pri_en_wt` 只表示本拍 global filter 的采样概率。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`，函数/task：`load_from_plus()`。

函数功能简析：该函数从 `plus` 拷贝最终参数并做非负入口检查。输入来自 `plus::MEMBLOCK_*`；副作用是刷新 `seq_csr_common` 静态快照。

修改后源码：

```systemverilog
load_pip_num_limit          = get_non_negative_int("MEMBLOCK_LOAD_PIP_NUM_LIMIT", plus::MEMBLOCK_LOAD_PIP_NUM_LIMIT);
sta_pip_num_limit           = get_non_negative_int("MEMBLOCK_STA_PIP_NUM_LIMIT", plus::MEMBLOCK_STA_PIP_NUM_LIMIT);
std_pip_num_limit           = get_non_negative_int("MEMBLOCK_STD_PIP_NUM_LIMIT", plus::MEMBLOCK_STD_PIP_NUM_LIMIT);
load_pip_num_random_en      = plus::MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN;
sta_pip_num_random_en       = plus::MEMBLOCK_STA_PIP_NUM_RANDOM_EN;
std_pip_num_random_en       = plus::MEMBLOCK_STD_PIP_NUM_RANDOM_EN;

send_pri_mode_en            = plus::MEMBLOCK_SEND_PRI_MODE_EN;
global_send_pri_en_wt       = get_non_negative_int("MEMBLOCK_GLOBAL_SEND_PRI_EN_WT", plus::MEMBLOCK_GLOBAL_SEND_PRI_EN_WT);
```

中文伪代码：

这段逻辑负责把 plus 参数转换成 sequence 可读的统一快照。
函数先读取三个 pipe LIMIT，并拒绝负数；再读取三个 RANDOM_EN bit；随后读取 send_pri 模式 bit 和 global 权重，global 权重同样先拒绝负数。
`get_non_negative_int()` 在发现负数时 fatal，避免负数转成 unsigned 后变成巨大配置值；真正上下界 clamp 在 `validate_and_clamp()` 中完成。

关键分支/副作用说明：

- 这里不读取旧 `MEMBLOCK_SEND_PRI_EN`，旧命令行不会再改变新逻辑。
- `global_send_pri_en_wt` 允许 0 和 100，分别表示永不 global 和总是 global。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`，函数/task：`validate_and_clamp()`。

函数功能简析：该函数统一检查参数边界。输入是已经加载的静态字段；副作用是对可收敛字段 clamp，或对不合法组合 fatal。

修改后源码：

```systemverilog
clamp_int("send_pri_default", send_pri_default, 0, 100);
clamp_int("send_pri_std_default", send_pri_std_default, 0, 100);
clamp_int("global_send_pri_en_wt", global_send_pri_en_wt, 0, 100);
clamp_int("load_pip_num_limit", load_pip_num_limit, 1, real_load_pipe_num);
clamp_int("sta_pip_num_limit", sta_pip_num_limit, 1, real_sta_pipe_num);
clamp_int("std_pip_num_limit", std_pip_num_limit, 1, real_std_pipe_num);
if (send_pri_mode_en) begin
    fatal_if_all_zero3("send_pri weights", send_pri_low_wt, send_pri_mid_wt, send_pri_high_wt);
    fatal_if_all_zero3("send_pri_std weights", send_pri_std_low_wt, send_pri_std_mid_wt, send_pri_std_high_wt);
end
```

中文伪代码：

这段逻辑负责收敛 send_pri 和 pipe 参数。
先把普通和 STD 默认 priority clamp 到 0 到 100；再把 global 权重 clamp 到 0 到 100；随后把 LOAD/STA/STD LIMIT clamp 到真实 DUT pipe 数范围；最后只有在 send_pri 模式开启时，才检查 low/mid/high 权重不能全 0。
`clamp_int()` 会打印 warning 并修改字段；`fatal_if_all_zero3()` 会在随机权重无定义时停止仿真。

关键分支/副作用说明：

- 非 send_pri 模式不使用 low/mid/high 权重，因此不应因为权重全 0 fatal。
- pipe LIMIT 被 clamp 后，`sample_*_pip_num()` 的随机范围一定非空。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`，函数/task：getter/sample 组。

函数功能简析：这些函数是调度路径读取参数的正式入口。输入是静态快照；输出是配置上限、随机开关或本拍采样值；没有状态清除副作用。

修改后源码：

```systemverilog
static function int unsigned sample_load_pip_num();
    check_initialized("sample_load_pip_num");
    if (load_pip_num_random_en) begin
        return $urandom_range(load_pip_num_limit, 1);
    end
    return load_pip_num_limit;
endfunction:sample_load_pip_num

static function bit sample_global_send_pri_en();
    check_initialized("sample_global_send_pri_en");
    if (!send_pri_mode_en || global_send_pri_en_wt == 0) begin
        return 1'b0;
    end
    if (global_send_pri_en_wt >= 100) begin
        return 1'b1;
    end
    return $urandom_range(99, 0) < global_send_pri_en_wt;
endfunction:sample_global_send_pri_en
```

中文伪代码：

这段逻辑负责把配置值转换成本拍调度实际使用值。
`sample_load_pip_num()` 先检查 `seq_csr_common` 已初始化；如果 LOAD pipe 随机开关为 1，就在 1 到 LOAD LIMIT 内随机返回；否则直接返回 LOAD LIMIT。STA/STD 同理。
`sample_global_send_pri_en()` 先检查初始化；如果 send_pri 模式关闭或 global 权重为 0，返回 0；如果权重大于等于 100，返回 1；其他情况在 0 到 99 内抽样，小于权重则返回 1。

关键分支/副作用说明：

- 调度路径使用 sample 函数，不再使用 limit getter 当作本拍数量。
- `sample_global_send_pri_en()` 在 send_pri 模式关闭时强制返回 0，保证 non-send_pri 模式不会误进入 global filter。

## 5. 主表 priority 生成

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`，函数/task：`randomize_send_pri_value(is_std)`。

函数功能简析：该函数在主表生成阶段为普通 LOAD/STA 和 STD 分别产生 priority。输入是 `is_std` 和 `seq_csr_common` 参数；输出是 0 到 100 的 priority。

修改后源码：

```systemverilog
if (!seq_csr_common::get_send_pri_mode_en()) begin
    if (is_std) begin
        return seq_csr_common::get_send_pri_std_default();
    end
    return seq_csr_common::get_send_pri_default();
end

if (is_std) begin
    sel = rand_weighted3(seq_csr_common::get_send_pri_std_low_wt(),
                         seq_csr_common::get_send_pri_std_mid_wt(),
                         seq_csr_common::get_send_pri_std_high_wt());
end else begin
    sel = rand_weighted3(seq_csr_common::get_send_pri_low_wt(),
                         seq_csr_common::get_send_pri_mid_wt(),
                         seq_csr_common::get_send_pri_high_wt());
end
```

中文伪代码：

这段逻辑负责让主表 priority 只受 send_pri 总模式控制。
函数先读取 `get_send_pri_mode_en()`；如果返回 0，STD 使用 `send_pri_std_default`，其它 target 使用 `send_pri_default` 并立即返回；如果返回 1，则按 STD 或普通 target 选择对应的 low/mid/high 权重，再调用 `rand_weighted3()` 选区间。
`rand_weighted3()` 的返回值决定后续落在 low/mid/high priority 区间，最终返回 0..33、34..66 或 67..100。

关键分支/副作用说明：

- 旧逻辑把 priority 随机化和 global filter 开关绑定；新逻辑解除绑定。
- non-global send_pri 模式下仍会随机 priority，供各 target 队列内部仲裁使用。

## 6. issue 调度仲裁

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/issue_queue_scheduler.sv`，函数/task：`select_target_candidates()`。

函数功能简析：该函数从单个 target 队列里挑最多 `max_count` 个 eligible item。输入包括 target、采样后的 pipe 数、是否比较 priority、是否启用 global filter；输出是 selected 队列。

修改后源码：

```systemverilog
function void select_target_candidates(input memblock_issue_target_e target,
                                       input int unsigned max_count,
                                       input bit compare_pri,
                                       input bit use_global_pri,
                                       input int unsigned global_pri,
                                       output memblock_issue_q_item_t selected[$]);
    int selected_indices[$];

    selected.delete();
    if (max_count == 0) begin
        return;
    end

    while (selected.size() < max_count) begin
        int best_idx;
        memblock_issue_q_item_t best_item;
        bit found;

        best_idx = -1;
        best_item = make_empty_item();
        found = 1'b0;
        for (int idx = 0; idx < get_target_queue_size(target); idx++) begin
            memblock_issue_q_item_t item;

            if (index_already_selected(idx, selected_indices)) begin
                continue;
            end
            item = get_target_queue_item(target, idx);
            if (!is_issue_item_eligible(item)) begin
                continue;
            end
    if (use_global_pri && item.send_pri != global_pri) begin
        continue;
    end
    if (!found || item_is_better(item, best_item, compare_pri)) begin
        best_idx = idx;
        best_item = item;
        found = 1'b1;
    end
        end

        if (!found) begin
            break;
        end
        selected.push_back(best_item);
        selected_indices.push_back(best_idx);
    end
endfunction:select_target_candidates
```

中文伪代码：

这段逻辑负责在单个 LOAD/STA/STD 队列内挑选本拍候选。
函数先清空输出；如果 `max_count` 为 0 直接返回；随后循环直到 selected 达到 `max_count`；每轮扫描 target 队列，跳过本拍已选 index、不可发射 item、以及 global 模式下 priority 不等于 `global_pri` 的 item；对剩余 item，用 `item_is_better(item,best,compare_pri)` 决定是否更新最佳项；扫描结束若没找到候选则退出，否则把最佳项压入输出并记录 index。
`item_is_better()` 在 `compare_pri=1` 时先比 priority，再比 ROB 年龄；在 `compare_pri=0` 时只比 ROB 年龄。

关键分支/副作用说明：

- `compare_pri` 和 `use_global_pri` 分离后，可表达 local priority 模式。
- `use_global_pri=1` 只负责过滤非全局最大 priority，不再隐含“是否比较 priority”。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/issue_queue_scheduler.sv`，函数/task：`select_issue_candidates()`。

函数功能简析：该函数是每拍 issue 选择总入口。输入是公共 issue queue 和 `seq_csr_common` 参数；输出是 LOAD/STA/STD 三类候选数组；可能副作用是 flush 阻塞时置 `issue_freeze_ack`。

修改后源码：

```systemverilog
compare_pri = seq_csr_common::get_send_pri_mode_en();
use_global_pri = compare_pri && seq_csr_common::sample_global_send_pri_en();
if (use_global_pri && !find_global_max_send_pri(global_pri)) begin
    use_global_pri = 1'b0;
end
select_target_candidates(MEMBLOCK_ISSUE_TARGET_LOAD,
                         seq_csr_common::sample_load_pip_num(),
                         compare_pri,
                         use_global_pri,
                         global_pri,
                         load_items);
```

中文伪代码：

这段逻辑负责决定本拍是 non-send-pri、send-pri local，还是 send-pri global。
函数先确保公共数据存在，并清空三个输出队列；如果全局 flush 阻塞 issue，则设置 `data.issue_freeze_ack=1` 并返回；随后读取 `get_send_pri_mode_en()` 得到 `compare_pri`；再调用 `sample_global_send_pri_en()`，只有 compare_pri 为 1 且采样命中时才尝试 global 模式；如果 global 模式找不到任何 eligible item 的最大 priority，就把 `use_global_pri` 改回 0；最后分别调用 `sample_load_pip_num()`、`sample_sta_pip_num()`、`sample_std_pip_num()`，并把采样结果传给 `select_target_candidates()`。
`find_global_max_send_pri()` 在三个队列所有 eligible item 中找最大 priority；`select_target_candidates()` 按单队列规则输出候选。

关键分支/副作用说明：

- `compare_pri=0/use_global_pri=0`：非 send_pri 模式，只按 ROB age。
- `compare_pri=1/use_global_pri=0`：send_pri local 模式，各队列内部先比 priority。
- `compare_pri=1/use_global_pri=1`：send_pri global 模式，先全局过滤，再按 priority/ROB age。
- 找不到 `global_pri` 时回退 non-global，避免 empty eligible set 误导致三个输出都被过滤。

## 7. cfg 与文档同步

源码位置：`mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg`，函数/task：默认 runtime cfg。

函数功能简析：默认 cfg 给 Makefile runtime cfg 路径提供可见参数。输入来自文件内容；副作用是仿真命令展开 plusarg。

修改后源码：

```text
+MEMBLOCK_LOAD_PIP_NUM_LIMIT=3
+MEMBLOCK_STA_PIP_NUM_LIMIT=2
+MEMBLOCK_STD_PIP_NUM_LIMIT=2
+MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN=0
+MEMBLOCK_STA_PIP_NUM_RANDOM_EN=0
+MEMBLOCK_STD_PIP_NUM_RANDOM_EN=0
+MEMBLOCK_SEND_PRI_MODE_EN=1
+MEMBLOCK_GLOBAL_SEND_PRI_EN_WT=0
```

中文伪代码：

这段配置负责定义默认调度模式。
运行时先设置三类 pipe LIMIT 为 DUT 默认上限；再关闭三类 RANDOM_EN，使 sample 函数固定返回 LIMIT；随后开启 send_pri 模式，但把 global 权重设为 0，使默认行为成为 send_pri local 模式。
testcase cfg 中旧 `MEMBLOCK_*_PIP_NUM=1` 被等价替换为 `MEMBLOCK_*_PIP_NUM_LIMIT=1`，保持固定单 pipe smoke 行为。

关键分支/副作用说明：

- `tc_dispatch_real_mixed_sta_wb_smoke.cfg`、`tc_dispatch_real_mixed_wb_smoke.cfg`、`tc_dispatch_real_multi_store_wb_smoke.cfg` 只迁移参数名，不打开随机。
- `plus_demo_migration_plan.md`、source_sv、flow、web 文档同步为新参数和新调用链。

## 8. 正确性检查

| 场景 | 配置 | 预期行为 | 检查点 |
|---|---|---|---|
| 非 send_pri | `MEMBLOCK_SEND_PRI_MODE_EN=0` | 主表 priority 使用 default，issue 只按 ROB age | `randomize_send_pri_value()` default 分支，`compare_pri=0` |
| send_pri local | `MEMBLOCK_SEND_PRI_MODE_EN=1`、`MEMBLOCK_GLOBAL_SEND_PRI_EN_WT=0` | 主表随机 priority，各队列内部按 priority/age | `sample_global_send_pri_en()` 返回 0，`compare_pri=1/use_global_pri=0` |
| send_pri global | `MEMBLOCK_SEND_PRI_MODE_EN=1`、`MEMBLOCK_GLOBAL_SEND_PRI_EN_WT=100` | 三队列先过滤全局最大 priority | `sample_global_send_pri_en()` 返回 1，`find_global_max_send_pri()` 成功 |
| global 空候选 | global 采样命中但无 eligible item | 回退 non-global，不误过滤 | `if (use_global_pri && !find_global_max_send_pri(...)) use_global_pri=0` |
| 固定 pipe | `*_PIP_NUM_RANDOM_EN=0` | 本拍数量等于 LIMIT | `sample_*_pip_num()` 返回 limit |
| 随机 pipe | `*_PIP_NUM_RANDOM_EN=1` | 本拍数量在 `[1:LIMIT]` | `sample_*_pip_num()` 调 `$urandom_range(limit,1)` |

## 9. 非本次修改逻辑分析

- `route_all_ready_uids()`、`route_uid()`、`route_target()` 的路由边界没有修改，仍由 status 的 active/enq/tlb/replay/flush 状态决定是否进入 issue queue。
- `is_issue_item_eligible()` 没有修改，发射前仍会检查 active、enq、TLB、flush/redirect/exception、replay_seq、ready_cycle 和 target dispatched/pass 状态。
- `mark_issue_fire()` 和 `mark_issue_fire_already_accepted()` 没有修改，DUT 接受后的队列删除、queued/dispatched 状态更新、issue_epoch 分配仍保持原逻辑。
- `issue_field_assigner` 和 `memblock_lintsissue_dispatch_sequence::assign_issue_items()` 没有修改，因此本次只改变候选选择，不改变 payload 字段映射。
- `MEMBLOCK_REAL_*` 真实端口上限语义没有修改，仍用于 clamp pipe LIMIT，防止软件请求超过 DUT 支持宽度。

## 10. 验证方式

已执行轻量检查：

```bash
rg -n "\bMEMBLOCK_SEND_PRI_EN\b|\bMEMBLOCK_LOAD_PIP_NUM\b|\bMEMBLOCK_STA_PIP_NUM\b|\bMEMBLOCK_STD_PIP_NUM\b|\bget_load_pip_num\b|\bget_sta_pip_num\b|\bget_std_pip_num\b|\bget_send_pri_en\b" mem_ut/ver/ut/memblock AI_DOC/analysis/source_sv/dispatch_framework_sv AI_DOC/mem_ut_flow_doc AI_DOC/web -S
```

结果：无匹配，说明本任务相关源码、source_sv、flow、web 文档中旧参数/旧 getter 已清理。

```bash
for f in mem_ut/ver/ut/memblock/seq/plus_cfg/*.cfg; do
  sed 's/[#].*$//; s,//.*$,,; s/^+//; /^[[:space:]]*$/d; s/=.*//' "$f"
done | rg '^MEMBLOCK_' | sort -u > /tmp/memblock_cfg_keys.txt
rg -n '`MEMBLOCK_PLUS_ARGS_DEFINE\(MEMBLOCK_|load_(bit|hex64|int|string)\("MEMBLOCK_' mem_ut/ver/ut/memblock/env/plus.sv \
  | sed -n 's/.*MEMBLOCK_/MEMBLOCK_/p' | sed 's/[^A-Za-z0-9_].*$//' | sort -u > /tmp/memblock_plus_keys.txt
comm -23 /tmp/memblock_cfg_keys.txt /tmp/memblock_plus_keys.txt
```

结果：输出为空，说明 cfg 中 MEMBLOCK key 均在 `plus.sv` 声明/解析。

已执行 diff 格式检查：

```bash
git diff --cached --check
```

结果：通过，未发现 whitespace error。

已执行简单真实 DUT smoke：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_dispatch_real_mixed_wb_smoke mode=base_fun cfg=tc_dispatch_real_mixed_wb_smoke wave=null
make eda_run tc=tc_dispatch_real_mixed_wb_smoke mode=base_fun cfg=tc_dispatch_real_mixed_wb_smoke wave=null
```

结果：

- 编译通过，重编译了本次相关的 `plus_pkg/seq_pkg` 分区以及受影响分区。
- 仿真日志 `base_fun/log/tc_dispatch_real_mixed_wb_smoke_666666_rtl_.log` 显示 `TEST CASE PASSED`。
- UVM 汇总：`UVM_WARNING=0`、`UVM_ERROR=0`、`UVM_FATAL=0`。
- 日志确认 cfg 已展开新参数：`MEMBLOCK_LOAD_PIP_NUM_LIMIT=1`、`MEMBLOCK_STA_PIP_NUM_LIMIT=1`、`MEMBLOCK_STD_PIP_NUM_LIMIT=1`。

## 11. Plan 对齐检查

已找到并移动对应 plan：

- 原路径：`AI_DOC/plan/test_framework/plan/undo/send_pri_pipe_mode_plan_20260616.md`
- 当前路径：`AI_DOC/plan/test_framework/plan/do/send_pri_pipe_mode_plan_20260616.md`

对齐结果：

- 已删除旧 `MEMBLOCK_SEND_PRI_EN` 运行语义。
- 已新增 `MEMBLOCK_SEND_PRI_MODE_EN` 和 `MEMBLOCK_GLOBAL_SEND_PRI_EN_WT`。
- 已将 pipe 参数迁移到 `*_PIP_NUM_LIMIT` 并新增 `*_PIP_NUM_RANDOM_EN`。
- 已在 `seq_csr_common` 添加字段、加载、校验、getter 和 sample 函数。
- 已让主表 priority 生成改用 `get_send_pri_mode_en()`。
- 已让 scheduler 拆分 `compare_pri` 和 `use_global_pri`，并使用 sample pipe 函数。
- 已更新 default cfg、相关 testcase cfg、source_sv、flow 和 web 文档。

## 12. 实现与 Plan 不一致项

未发现实现与 Plan 不一致项；当前 coding 行为与对应 plan 保持一致。

核对依据：

| Plan 要求 | 当前实现 | 结论 |
|---|---|---|
| 删除旧 `MEMBLOCK_SEND_PRI_EN` 体系。 | `plus.sv` 不再声明/解析 `MEMBLOCK_SEND_PRI_EN`；`seq_csr_common.sv` 不再维护 `send_pri_en`；scheduler 不再调用 `get_send_pri_en()`。 | 一致 |
| 新增 `MEMBLOCK_SEND_PRI_MODE_EN` 和 `MEMBLOCK_GLOBAL_SEND_PRI_EN_WT`。 | `plus.sv` 声明并解析两个参数；`seq_csr_common.sv` 快照、clamp 并提供 getter/sample。 | 一致 |
| pipe 参数更名为 `*_PIP_NUM_LIMIT`，并新增随机开关。 | cfg、plus 入口、`seq_csr_common` 字段和 sample 函数均已迁移。 | 一致 |
| `select_target_candidates()` 新增 `compare_pri`，区分队列内 priority 比较和 global filter。 | 函数签名已有 `compare_pri` 与 `use_global_pri` 两个输入，`item_is_better()` 只读取 `compare_pri`。 | 一致 |
| `select_issue_candidates()` 每拍采样 global 模式和 pipe 数。 | 函数调用 `sample_global_send_pri_en()`、`sample_load_pip_num()`、`sample_sta_pip_num()`、`sample_std_pip_num()`。 | 一致 |

## 13. Plan 未说明但 Coding 落实的细节

### 13.1 global 空候选回退保护

细节功能：global 模式采样命中时，如果三个 issue queue 中没有任何 eligible item，当前实现会把 `use_global_pri` 回退为 0，而不是保留一个无效 `global_pri` 去过滤队列。

为什么 plan 未覆盖：plan 描述了 global/non-global 主路径，但没有单独展开“采样命中但当前无可发射 item”的空集合边界。

在本特性中的作用：避免空队列或全部 item 还在 delay/replay/flush 保护状态时，`global_pri` 未被有效赋值却被传入 `select_target_candidates()`，导致误过滤或 debug 困难。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/issue_queue_scheduler.sv`，函数/task：`select_issue_candidates()`。

修改后源码：

```systemverilog
use_global_pri = compare_pri && seq_csr_common::sample_global_send_pri_en();
if (use_global_pri && !find_global_max_send_pri(global_pri)) begin
    use_global_pri = 1'b0;
end
```

中文伪代码：

这段逻辑在 issue select 阶段负责处理 global filter 的空集合边界。
函数先根据 send_pri 模式和 global 权重采样结果得到 `use_global_pri`；如果它为 1，就调用 `find_global_max_send_pri()` 到三个队列里查找当前所有 eligible item 的最大 priority；如果查找失败，说明本拍没有任何可用于 global filter 的候选，于是把 `use_global_pri` 清为 0，让后续选择按 non-global 路径运行。
`find_global_max_send_pri()` 的返回值决定 `global_pri` 是否有效；失败时不继续使用 `global_pri`。

是否需要回写 plan：已在 plan 的实现细节和 review 中记录，当前不需要再修改源码。

### 13.2 非 send_pri 模式跳过 priority 权重全 0 fatal

细节功能：`send_pri_mode_en=0` 时，不再要求 `send_pri_low/mid/high` 权重必须非零。

为什么 plan 未覆盖：plan 主要描述新参数拆分，没有单独说明关闭 send_pri 模式时权重检查也应该跟随关闭。

在本特性中的作用：非 send_pri 模式下主表使用 default priority，不会调用 low/mid/high 权重随机；此时允许用户把随机权重设为 0，避免对未使用配置做无意义 fatal。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`，函数/task：`validate_and_clamp()`。

修改后源码：

```systemverilog
if (send_pri_mode_en) begin
    fatal_if_all_zero3("send_pri weights", send_pri_low_wt, send_pri_mid_wt, send_pri_high_wt);
    fatal_if_all_zero3("send_pri_std weights", send_pri_std_low_wt, send_pri_std_mid_wt, send_pri_std_high_wt);
end
```

中文伪代码：

这段逻辑在参数校验阶段负责让权重合法性检查跟随 send_pri 模式开关。
函数先判断 `send_pri_mode_en`；如果为 1，说明后续主表生成会用 low/mid/high 权重随机 priority，因此必须检查普通 priority 权重和 STD priority 权重都不能全 0；如果 `send_pri_mode_en` 为 0，说明主表直接使用 default priority，跳过这两个 fatal 检查。
`fatal_if_all_zero3()` 只在权重真正会被使用时参与保护，避免关闭模式下被未使用参数阻塞仿真。

是否需要回写 plan：作为 coding 合理边界保留在 review 中即可。

## 14. 非本次修改的逻辑分析

### 14.1 git status 对比结论

本次 review 主题覆盖的文件已经在第 1 章列出；这些文件均属于 send_pri/pipe mode 代码、cfg、文档、plan 迁移或 review 文档。

`git status --short` 中仍存在但不属于本次 review 主题的修改：

| 类别 | 文件/目录 | 判断 | 原因 |
|---|---|---|---|
| flow 文档其它章节 | `AI_DOC/mem_ut_flow_doc/fault_exception_flow.md`、`flushsb_test_flow.md`、`lsq_admission_flow.md`、`normal_pass_flow.md`、`push_feedback_event_writeback_flow.md`、`redirect_flow.md`、`replay_flow.md`、`sfence_flow.md`、`writeback_function_call_flow.md` | 非本次逻辑 | 这些文件是其它 flow 文档修改，未参与本次 send_pri/pipe mode 代码 diff。 |
| 其它 plan 文档 | `AI_DOC/plan/test_framework/plan/do/dispatch_100k_performance_optimization_plan_20260614.md` 等 | 非本次逻辑 | 属于 100k、dispatch v2、数据持久化等其它计划，不纳入本 review 功能正确性分析。 |
| 其它 review 文档 | `active_sequence_global_stop_lifecycle_review_20260614.md` 删除、`main_table_addr_reuse_window_review_20260614.md` 删除、`dispatch_plan_v2_*` 修改 | 非本次逻辑 | 属于其它 review 文档整理或历史修改，本次没有 stage 进 send_pri commit。 |
| 项目管理规则 | `AI_DOC/project_management/mem_ut_code_review_document_rule.md`、`mem_ut_flow_document_rule.md` | 非本次逻辑 | 是 review/flow 文档规则调整，不是本次 send_pri/pipe mode 实现逻辑。 |
| 未跟踪文档 | `AI_DOC/mem_ut_flow_doc/*_flow.md` 若干未跟踪文件、`AI_DOC/analysis/rtl/lsu_port_replay_constraint_analysis.md` | 非本次逻辑 | 属于其它新增分析文档，未纳入本 review 覆盖范围。 |
| 仿真/生成产物 | `build_memblock/`、`coupledL2/`、`openLLC/`、`verdiLog/`、`novas.*` | 非源码 review | 属于生成输出或工具产物，不纳入源码 review。 |

### 14.2 本次 commit 边界

本次已提交 commit 为：

```text
ab1591ad5 feat(memblock): split send priority and pipe sampling controls
```

该 commit 只包含第 1 章列出的 send_pri/pipe mode 相关文件；上述非本次修改未包含在该 commit 中。
