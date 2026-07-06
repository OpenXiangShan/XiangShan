# send_pri 三模式与 issue pipe 数随机化修改方案

## 1. 背景

当前 mem_ut 的 LOAD/STA/STD issue 选择逻辑只支持两种行为：

1. `MEMBLOCK_SEND_PRI_EN=0`：不比较 `send_pri`，每个 queue 内只按 ROB age 选择更老请求。
2. `MEMBLOCK_SEND_PRI_EN=1`：跨 LOAD/STA/STD 三个 queue 找全局最大 `send_pri`，本拍只允许该 priority 的 item 参与选择，同 priority 再按 ROB age。

当前实现无法表达“启用 send_pri，但不做全局过滤，只在各自 queue 内按 priority 选择”的模式。因此需要把“是否比较 priority”和“是否跨 queue 做全局 priority filter”拆开。

同时，当前 `seq_csr_common::get_load_pip_num()`、`get_sta_pip_num()`、`get_std_pip_num()` 固定返回配置值，名字容易被误解为“本拍实际 pipe 数”。后续需要把它们改名为 `get_load_pip_num_limit()`、`get_sta_pip_num_limit()`、`get_std_pip_num_limit()`，明确表示配置的最大上限/固定值；本拍实际可发射 pipe 数由 `sample_load_pip_num()`、`sample_sta_pip_num()`、`sample_std_pip_num()` 每拍采样得到。

## 2. 当前源码位置

### 2.1 plus 参数入口

文件：`mem_ut/ver/ut/memblock/env/plus.sv`

当前相关位置：

- `24-34`：主表数量、LSQ enqueue、LOAD/STA/STD pipe 数参数。
- `48-58`：当前全局 send_pri 相关参数。
- `165-190`：`reload_from_cmdline()` 解析上述参数。

当前参数：

```systemverilog
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_LOAD_PIP_NUM, int, 3)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_STA_PIP_NUM, int, 2)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_STD_PIP_NUM, int, 2)

`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_SEND_PRI_DEFAULT, int, 50)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_SEND_PRI_LOW_WT, int, 1)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_SEND_PRI_MID_WT, int, 8)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_SEND_PRI_HIGH_WT, int, 1)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_SEND_PRI_STD_DEFAULT, int, 50)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_SEND_PRI_STD_LOW_WT, int, 1)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_SEND_PRI_STD_MID_WT, int, 8)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_SEND_PRI_STD_HIGH_WT, int, 1)
```

### 2.2 seq 参数缓存

文件：`mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`

当前相关位置：

- `23-25`：`load_pip_num/sta_pip_num/std_pip_num` 缓存。
- `40-51`：当前 `send_pri_en` 和 send_pri 默认值/权重缓存；修改后删除 `send_pri_en`，新增 `send_pri_mode_en/global_send_pri_en_wt`。
- `163-193`：`load_from_plus()` 从 plus 读取配置。
- `294-327`：`validate_and_clamp()` 对 send_pri 默认值和 pipe 数做范围检查。
- `479-492`：当前 `get_load_pip_num/get_sta_pip_num/get_std_pip_num` 固定返回配置值；修改后重命名为 `get_load_pip_num_limit/get_sta_pip_num_limit/get_std_pip_num_limit`。
- `544-587`：send_pri 相关 getter。

### 2.3 主表 send_pri 生成

文件：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`

当前相关位置：

- `338-339`：`randomize_main_transaction()` 调用 `randomize_send_pri_value()` 填 `send_pri/send_pri_std`。
- `721-746`：`randomize_send_pri_value()`。

当前逻辑：

```text
如果 send_pri_en=0：
  send_pri 直接使用 default。
否则：
  根据 low/mid/high 权重随机到 [0:33]/[34:66]/[67:100]。
```

问题：当前把“主表是否随机 priority”和“issue 是否全局 priority filter”绑定到同一个 `send_pri_en`，职责不清晰。

### 2.4 issue queue 选择

文件：`mem_ut/ver/ut/memblock/seq/base_seq/issue_queue_scheduler.sv`

当前相关位置：

- `293-305`：`item_is_better(candidate, best, compare_pri)`，`compare_pri=1` 时先比较 `send_pri`，否则只比较 ROB age。
- `343-372`：`find_global_max_send_pri()`，跨 LOAD/STA/STD 三个 queue 找最大 eligible priority。
- `374-420`：`select_target_candidates()`，从单个 target queue 中选择本拍候选。
- `422-453`：`select_issue_candidates()`，当前用 `get_send_pri_en()` 决定是否启用全局 priority。

当前核心逻辑：

```systemverilog
use_global_pri = seq_csr_common::get_send_pri_en() && find_global_max_send_pri(global_pri);

select_target_candidates(LOAD, seq_csr_common::get_load_pip_num(), use_global_pri, global_pri, load_items);
select_target_candidates(STA,  seq_csr_common::get_sta_pip_num(),  use_global_pri, global_pri, sta_items);
select_target_candidates(STD,  seq_csr_common::get_std_pip_num(),  use_global_pri, global_pri, std_items);
```

问题：`use_global_pri` 同时控制两件事：

- 是否跳过非全局最大 priority 的 item。
- 是否在 `item_is_better()` 中比较 `send_pri`。

因此当前无法支持“各自 queue 内比较 priority，但不做全局 priority filter”。

## 3. 目标模式

修改后支持三种 issue 选择模式。

### 3.1 非 send_pri 模式

控制条件：

```text
MEMBLOCK_SEND_PRI_MODE_EN=0
```

行为：

```text
主表生成：
  send_pri     = MEMBLOCK_SEND_PRI_DEFAULT
  send_pri_std = MEMBLOCK_SEND_PRI_STD_DEFAULT

issue 选择：
  LOAD/STA/STD 各自 queue 独立选择。
  不比较 send_pri。
  只按 ROB age 选择更老 item。
```

用途：

- 作为最简单、最稳定的默认调试模式。
- 复现旧的非 priority 行为。

### 3.2 send_pri + global_send_pri 模式

控制条件：

```text
MEMBLOCK_SEND_PRI_MODE_EN=1
本拍随机 global_send_pri=1
```

行为：

```text
主表生成：
  send_pri/send_pri_std 按 low/mid/high 权重随机生成。

issue 选择：
  先跨 load_issue_q/sta_issue_q/std_issue_q 找所有 eligible item 的最大 send_pri。
  本拍三个 queue 都只允许 send_pri == global_pri 的 item 参与选择。
  同 priority 再按 ROB age 选择更老 item。
```

用途：

- 制造跨 LOAD/STA/STD 的全局发射优先级压力。
- 验证高 priority 请求是否可以压过更老但 priority 低的请求。

### 3.3 send_pri + non-global_send_pri 模式

控制条件：

```text
MEMBLOCK_SEND_PRI_MODE_EN=1
本拍随机 global_send_pri=0
```

行为：

```text
主表生成：
  send_pri/send_pri_std 按 low/mid/high 权重随机生成。

issue 选择：
  不跨 queue 找全局最大 priority。
  LOAD queue 内部按 send_pri 先选最大 priority，同 priority 再按 ROB age。
  STA queue 内部按 send_pri 先选最大 priority，同 priority 再按 ROB age。
  STD queue 内部按 send_pri_std 先选最大 priority，同 priority 再按 ROB age。
```

用途：

- 验证每类 issue target 自己的 priority 仲裁。
- 避免 global priority 导致某一类 queue 长时间被另一个 queue 的高 priority 压住。

## 4. 新增和调整 plus 参数

### 4.1 send_pri 模式参数

建议新增：

```systemverilog
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_SEND_PRI_MODE_EN, bit, 1'b1)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_GLOBAL_SEND_PRI_EN_WT, int, 0)
```

参数含义：

| 参数 | 默认值 | 含义 |
|---|---:|---|
| `MEMBLOCK_SEND_PRI_MODE_EN` | `1` | 是否启用 send_pri 仲裁体系。为 0 时主表使用 default priority，issue 只按 ROB age。 |
| `MEMBLOCK_GLOBAL_SEND_PRI_EN_WT` | `0` | 每次 issue select 前随机 `global_send_pri=1` 的权重；`global_send_pri=0` 的权重是 `100 - 该值`。默认 0 表示 send_pri 模式下全部走 non-global_send_pri。 |

`MEMBLOCK_GLOBAL_SEND_PRI_EN_WT` 的随机规则：

```systemverilog
global_send_pri dist {
  1'b1 := MEMBLOCK_GLOBAL_SEND_PRI_EN_WT,
  1'b0 := 100 - MEMBLOCK_GLOBAL_SEND_PRI_EN_WT
};
```

合法范围：

```text
0 <= MEMBLOCK_GLOBAL_SEND_PRI_EN_WT <= 100
```

### 4.1.1 删除旧 send_pri 单 bit 参数

不做旧参数兼容，直接删除旧 `MEMBLOCK_SEND_PRI_EN` 体系，避免两个参数同时表达 global 行为。

需要删除：

- `plus.sv` 中的 `MEMBLOCK_SEND_PRI_EN` 声明和解析。
- `seq_csr_common.sv` 中的 `send_pri_en` 静态字段。
- `seq_csr_common::get_send_pri_en()`。
- `default.cfg` 和 testcase cfg 中的 `+MEMBLOCK_SEND_PRI_EN=...`。
- 文档/网页中旧的 `send_pri_en` 描述。

新逻辑统一使用：

- `MEMBLOCK_SEND_PRI_MODE_EN`：控制是否启用 send_pri 仲裁体系。
- `MEMBLOCK_GLOBAL_SEND_PRI_EN_WT`：控制 send_pri 模式下每拍是否走 global priority 的随机权重。

### 4.2 send_pri 数值生成参数

保留现有参数：

```text
MEMBLOCK_SEND_PRI_DEFAULT
MEMBLOCK_SEND_PRI_LOW_WT
MEMBLOCK_SEND_PRI_MID_WT
MEMBLOCK_SEND_PRI_HIGH_WT

MEMBLOCK_SEND_PRI_STD_DEFAULT
MEMBLOCK_SEND_PRI_STD_LOW_WT
MEMBLOCK_SEND_PRI_STD_MID_WT
MEMBLOCK_SEND_PRI_STD_HIGH_WT
```

修改后的含义：

```text
MEMBLOCK_SEND_PRI_MODE_EN=0：
  主表生成时直接使用 DEFAULT，不使用 low/mid/high 权重。

MEMBLOCK_SEND_PRI_MODE_EN=1：
  主表生成时使用 low/mid/high 权重随机 send_pri。
```

注意：`send_pri_std` 只用于 STD target。LOAD 和 STA 使用 `send_pri`。

### 4.3 pipe 数随机/固定参数

按确认后的简化规则，只新增三个模式开关：

```systemverilog
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN, bit, 1'b0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_STA_PIP_NUM_RANDOM_EN, bit, 1'b0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_STD_PIP_NUM_RANDOM_EN, bit, 1'b0)
```

参数含义：

| 参数 | 默认值 | 含义 |
|---|---:|---|
| `MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN` | `0` | 为 0 时 LOAD 每拍固定使用 `MEMBLOCK_LOAD_PIP_NUM_LIMIT` 最大值；为 1 时每拍从 `[1:MEMBLOCK_LOAD_PIP_NUM_LIMIT]` 随机。 |
| `MEMBLOCK_STA_PIP_NUM_RANDOM_EN` | `0` | 为 0 时 STA 每拍固定使用 `MEMBLOCK_STA_PIP_NUM_LIMIT` 最大值；为 1 时每拍从 `[1:MEMBLOCK_STA_PIP_NUM_LIMIT]` 随机。 |
| `MEMBLOCK_STD_PIP_NUM_RANDOM_EN` | `0` | 为 0 时 STD 每拍固定使用 `MEMBLOCK_STD_PIP_NUM_LIMIT` 最大值；为 1 时每拍从 `[1:MEMBLOCK_STD_PIP_NUM_LIMIT]` 随机。 |

将旧 `MEMBLOCK_LOAD_PIP_NUM/MEMBLOCK_STA_PIP_NUM/MEMBLOCK_STD_PIP_NUM` 更名为 `MEMBLOCK_LOAD_PIP_NUM_LIMIT/MEMBLOCK_STA_PIP_NUM_LIMIT/MEMBLOCK_STD_PIP_NUM_LIMIT`，默认值直接设置为当前 RTL 最大上限：

```text
MEMBLOCK_LOAD_PIP_NUM_LIMIT = MEMBLOCK_REAL_LOAD_PIPE_NUM = 3
MEMBLOCK_STA_PIP_NUM_LIMIT  = MEMBLOCK_REAL_STA_PIPE_NUM  = 2
MEMBLOCK_STD_PIP_NUM_LIMIT  = MEMBLOCK_REAL_STD_PIPE_NUM  = 2
```

它们的新含义是“默认非随机模式下的固定最大值，随机模式下的随机上限”：

```text
*_PIP_NUM_RANDOM_EN=0：
  本拍可选数量 = MEMBLOCK_*_PIP_NUM_LIMIT

*_PIP_NUM_RANDOM_EN=1：
  本拍可选数量 = $urandom_range(MEMBLOCK_*_PIP_NUM_LIMIT, 1)
```

合法性：

```text
MEMBLOCK_LOAD_PIP_NUM_LIMIT 必须在 [1:MEMBLOCK_REAL_LOAD_PIPE_NUM]
MEMBLOCK_STA_PIP_NUM_LIMIT  必须在 [1:MEMBLOCK_REAL_STA_PIPE_NUM]
MEMBLOCK_STD_PIP_NUM_LIMIT  必须在 [1:MEMBLOCK_REAL_STD_PIPE_NUM]
```

默认模式：

```text
MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN=0
MEMBLOCK_STA_PIP_NUM_RANDOM_EN=0
MEMBLOCK_STD_PIP_NUM_RANDOM_EN=0
```

即默认每拍使用 LOAD/STA/STD 的最大配置数量，不做随机。

## 5. 文件级修改方案

### 5.1 修改 `plus.sv`

文件：`mem_ut/ver/ut/memblock/env/plus.sv`

修改点 1：将旧 `MEMBLOCK_LOAD_PIP_NUM/STA/STD` 更名为 `MEMBLOCK_LOAD_PIP_NUM_LIMIT/MEMBLOCK_STA_PIP_NUM_LIMIT/MEMBLOCK_STD_PIP_NUM_LIMIT`，并在其后增加：

```systemverilog
// 为 0 时固定使用 MEMBLOCK_*_PIP_NUM_LIMIT 最大值；为 1 时每拍从 [1:MEMBLOCK_*_PIP_NUM_LIMIT] 随机。
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN, bit, 1'b0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_STA_PIP_NUM_RANDOM_EN, bit, 1'b0)
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_STD_PIP_NUM_RANDOM_EN, bit, 1'b0)
```

修改点 2：在 send_pri 参数处增加：

```systemverilog
// send_pri 仲裁总开关。为 0 时主表使用 default priority，issue 只按 ROB age。
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_SEND_PRI_MODE_EN, bit, 1'b1)
// send_pri 模式下，每拍随机 global_send_pri=1 的权重，范围 [0:100]。
`MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_GLOBAL_SEND_PRI_EN_WT, int, 0)
```

修改点 3：删除旧参数：

```systemverilog
// 删除旧参数，避免与 MEMBLOCK_GLOBAL_SEND_PRI_EN_WT 语义重复。
- `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_SEND_PRI_EN, bit, 1'b0)
```

同时删除 `reload_from_cmdline()` 中：

```text
load_bit("MEMBLOCK_SEND_PRI_EN", MEMBLOCK_SEND_PRI_EN)
```

修改点 4：`reload_from_cmdline()` 中增加：

```text
load_bit("MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN", MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN)
load_bit("MEMBLOCK_STA_PIP_NUM_RANDOM_EN", MEMBLOCK_STA_PIP_NUM_RANDOM_EN)
load_bit("MEMBLOCK_STD_PIP_NUM_RANDOM_EN", MEMBLOCK_STD_PIP_NUM_RANDOM_EN)

load_bit("MEMBLOCK_SEND_PRI_MODE_EN", MEMBLOCK_SEND_PRI_MODE_EN)
load_int("MEMBLOCK_GLOBAL_SEND_PRI_EN_WT", MEMBLOCK_GLOBAL_SEND_PRI_EN_WT)
```

### 5.2 修改 `seq_csr_common.sv`

文件：`mem_ut/ver/ut/memblock/seq/base_seq/seq_csr_common.sv`

新增字段：

```systemverilog
// issue pipe 数随机开关：0 固定使用配置最大值，1 每拍从 [1:配置值] 随机。
static bit load_pip_num_limit_random_en = 1'b0;
static bit sta_pip_num_limit_random_en  = 1'b0;
static bit std_pip_num_limit_random_en  = 1'b0;

// send_pri 仲裁总开关：0 表示不比较 send_pri，只按 ROB age。
static bit send_pri_mode_en = 1'b1;
// 每拍随机 global_send_pri=1 的权重，范围 [0:100]。
static int unsigned global_send_pri_en_wt = 0;
```

`load_from_plus()` 增加：

```text
load_pip_num_limit_random_en = plus::MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN
sta_pip_num_limit_random_en  = plus::MEMBLOCK_STA_PIP_NUM_RANDOM_EN
std_pip_num_limit_random_en  = plus::MEMBLOCK_STD_PIP_NUM_RANDOM_EN

send_pri_mode_en = plus::MEMBLOCK_SEND_PRI_MODE_EN
global_send_pri_en_wt = get_non_negative_int("MEMBLOCK_GLOBAL_SEND_PRI_EN_WT",
                                             plus::MEMBLOCK_GLOBAL_SEND_PRI_EN_WT)
```

`validate_and_clamp()` 增加：

```text
clamp_int("global_send_pri_en_wt", global_send_pri_en_wt, 0, 100)

如果 send_pri_mode_en=1：
  fatal_if_all_zero3("send_pri weights", send_pri_low_wt, send_pri_mid_wt, send_pri_high_wt)
  fatal_if_all_zero3("send_pri_std weights", send_pri_std_low_wt, send_pri_std_mid_wt, send_pri_std_high_wt)
否则：
  不要求 low/mid/high 权重非零，因为该模式不使用权重
```

新增 getter/sample 函数：

```systemverilog
static function bit get_send_pri_mode_en();
static function int unsigned get_global_send_pri_en_wt();
static function bit sample_global_send_pri_en();

static function bit get_load_pip_num_random_en();
static function bit get_sta_pip_num_random_en();
static function bit get_std_pip_num_random_en();

static function int unsigned sample_load_pip_num();
static function int unsigned sample_sta_pip_num();
static function int unsigned sample_std_pip_num();

static function int unsigned get_load_pip_num_limit();
static function int unsigned get_sta_pip_num_limit();
static function int unsigned get_std_pip_num_limit();
```

`sample_global_send_pri_en()` 伪代码：

```text
sample_global_send_pri_en:
  check_initialized
  如果 send_pri_mode_en=0：
    return 0
  如果 global_send_pri_en_wt == 0：
    return 0
  如果 global_send_pri_en_wt >= 100：
    return 1
  return ($urandom_range(99, 0) < global_send_pri_en_wt)
```

`sample_*_pip_num()` 伪代码：

```text
sample_load_pip_num:
  check_initialized
  如果 load_pip_num_limit_random_en=0：
    return load_pip_num_limit
  return $urandom_range(load_pip_num_limit, 1)

sample_sta_pip_num:
  如果 sta_pip_num_limit_random_en=0：
    return sta_pip_num_limit
  return $urandom_range(sta_pip_num_limit, 1)

sample_std_pip_num:
  如果 std_pip_num_limit_random_en=0：
    return std_pip_num_limit
  return $urandom_range(std_pip_num_limit, 1)
```

重命名 `get_load_pip_num_limit/get_sta_pip_num_limit/get_std_pip_num_limit()`：

```text
不保留原名字，避免误解。
将 get_load_pip_num_limit/get_sta_pip_num_limit/get_std_pip_num_limit 重命名为：
  get_load_pip_num_limit()
  get_sta_pip_num_limit()
  get_std_pip_num_limit()

这些 limit getter 只返回配置的上限/固定值。
issue scheduler 必须改用 sample_*_pip_num() 获取本拍实际数量。
其他文档或 debug 如果只想查看配置值，应使用 get_*_pip_num_limit()。
```

### 5.3 修改 `memblock_dispatch_base_sequence.sv`

文件：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_dispatch_base_sequence.sv`

修改函数：`randomize_send_pri_value()`

当前逻辑用 `get_send_pri_en()` 决定是否随机 priority，需要改成用 `get_send_pri_mode_en()`。

本节只给出行为概要，完整可 coding 的逐行文字伪代码见 [5.6.5 `memblock_dispatch_base_sequence.sv` 主表 priority 生成伪代码](#565-memblock_dispatch_base_sequencesv-主表-priority-生成伪代码)。

修改后概要伪代码：

```text
randomize_send_pri_value(is_std):
  如果 send_pri_mode_en=0：
    如果 is_std：
      return send_pri_std_default
    否则：
      return send_pri_default

  如果 send_pri_mode_en=1：
    如果 is_std：
      sel = rand_weighted3(send_pri_std_low_wt,
                           send_pri_std_mid_wt,
                           send_pri_std_high_wt)
    否则：
      sel = rand_weighted3(send_pri_low_wt,
                           send_pri_mid_wt,
                           send_pri_high_wt)

    sel=0：return random [0:33]
    sel=1：return random [34:66]
    sel=2：return random [67:100]
```

这样主表 priority 生成只由 `MEMBLOCK_SEND_PRI_MODE_EN` 控制，不再由“本拍是否 global”控制。

### 5.4 修改 `issue_queue_scheduler.sv`

文件：`mem_ut/ver/ut/memblock/seq/base_seq/issue_queue_scheduler.sv`

核心修改：把 `compare_pri` 和 `use_global_pri` 拆开。

#### 5.4.1 修改 `select_target_candidates()` 参数

当前签名：

```systemverilog
function void select_target_candidates(input memblock_issue_target_e target,
                                       input int unsigned max_count,
                                       input bit use_global_pri,
                                       input int unsigned global_pri,
                                       output memblock_issue_q_item_t selected[$]);
```

建议改成：

```systemverilog
function void select_target_candidates(input memblock_issue_target_e target,
                                       input int unsigned max_count,
                                       input bit compare_pri,
                                       input bit use_global_pri,
                                       input int unsigned global_pri,
                                       output memblock_issue_q_item_t selected[$]);
```

内部逻辑修改：

```text
for each item in target queue:
  如果 index 已选，跳过
  如果 item 不 eligible，跳过
  如果 use_global_pri=1 且 item.send_pri != global_pri，跳过
  如果 !found 或 item_is_better(item, best_item, compare_pri)，更新 best
```

关键点：

```text
compare_pri=0, use_global_pri=0：
  非 send_pri 模式，只按 ROB age。

compare_pri=1, use_global_pri=0：
  local send_pri 模式，每个 queue 内先比 priority。

compare_pri=1, use_global_pri=1：
  global send_pri 模式，先全局过滤，再同 priority/ROB age。
```

#### 5.4.2 修改 `select_issue_candidates()`

本节只给出调度结构概要，完整可 coding 的逐行文字伪代码见 [5.6.6 `issue_queue_scheduler.sv` 调度选择伪代码](#566-issue_queue_schedulersv-调度选择伪代码)。

修改后概要伪代码：

```text
select_issue_candidates:
  ensure_data
  清空输出数组

  如果 issue_blocked_by_global_flush():
    data.issue_freeze_ack = 1
    return

  send_pri_mode = seq_csr_common::get_send_pri_mode_en()
  sampled_global_send_pri = seq_csr_common::sample_global_send_pri_en()

  compare_pri = send_pri_mode
  use_global_pri = send_pri_mode && sampled_global_send_pri

  如果 use_global_pri:
    found_global = find_global_max_send_pri(global_pri)
    如果 !found_global:
      use_global_pri = 0

  load_max = seq_csr_common::sample_load_pip_num()
  sta_max  = seq_csr_common::sample_sta_pip_num()
  std_max  = seq_csr_common::sample_std_pip_num()

  select_target_candidates(LOAD, load_max, compare_pri, use_global_pri, global_pri, load_items)
  select_target_candidates(STA,  sta_max,  compare_pri, use_global_pri, global_pri, sta_items)
  select_target_candidates(STD,  std_max,  compare_pri, use_global_pri, global_pri, std_items)
```

可选 debug：

```text
UVM_HIGH 打印本拍 mode：
  send_pri_mode
  sampled_global_send_pri
  use_global_pri
  global_pri
  load_max/sta_max/std_max
```

### 5.5 修改 `default.cfg`

文件：`mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg`

建议在 pipe 参数后增加：

```text
+MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN=0
+MEMBLOCK_STA_PIP_NUM_RANDOM_EN=0
+MEMBLOCK_STD_PIP_NUM_RANDOM_EN=0
```

建议在 send_pri 参数处增加：

```text
+MEMBLOCK_SEND_PRI_MODE_EN=1
+MEMBLOCK_GLOBAL_SEND_PRI_EN_WT=0
```

删除旧参数：

```text
- +MEMBLOCK_SEND_PRI_EN=0
```

默认配置下：

```text
send_pri 模式开启；
每拍 global_send_pri 权重为 0；
因此默认走 send_pri + non-global_send_pri 模式；
LOAD/STA/STD 每拍 pipe 数默认取配置最大值。
```

## 5.6 可直接 coding 的文字伪代码

本节把上面的修改拆成逐文件、逐函数的文字伪代码，coding 时按这里实现即可。

本节文字伪代码统一遵循项目管理规则中的三段式要求：

```text
1. 先说明该逻辑在本方案中承担的功能。
2. 再按源码计划执行顺序逐行描述判断、赋值、循环、return、fatal/warning、状态更新和队列操作。
3. 如果调用子函数，必须说明该子函数在当前逻辑中的职责、输入来源、返回值或副作用，不能只列函数名。
```

### 5.6.1 `plus.sv` 伪代码

目标：删除旧 `MEMBLOCK_SEND_PRI_EN`，新增 send_pri 模式参数和 pipe 随机开关参数。

```text
plus.sv 参数声明区：
  将 MEMBLOCK_LOAD_PIP_NUM 更名为 MEMBLOCK_LOAD_PIP_NUM_LIMIT，默认 3。
  将 MEMBLOCK_STA_PIP_NUM 更名为 MEMBLOCK_STA_PIP_NUM_LIMIT，默认 2。
  将 MEMBLOCK_STD_PIP_NUM 更名为 MEMBLOCK_STD_PIP_NUM_LIMIT，默认 2。

  在 MEMBLOCK_STD_PIP_NUM_LIMIT 后添加：
    MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN，bit，默认 0。
    MEMBLOCK_STA_PIP_NUM_RANDOM_EN，bit，默认 0。
    MEMBLOCK_STD_PIP_NUM_RANDOM_EN，bit，默认 0。

  删除：
    MEMBLOCK_SEND_PRI_EN。

  在 send_pri 参数组前添加：
    MEMBLOCK_SEND_PRI_MODE_EN，bit，默认 1。
    MEMBLOCK_GLOBAL_SEND_PRI_EN_WT，int，默认 0。

  保留：
    MEMBLOCK_SEND_PRI_DEFAULT。
    MEMBLOCK_SEND_PRI_LOW_WT。
    MEMBLOCK_SEND_PRI_MID_WT。
    MEMBLOCK_SEND_PRI_HIGH_WT。
    MEMBLOCK_SEND_PRI_STD_DEFAULT。
    MEMBLOCK_SEND_PRI_STD_LOW_WT。
    MEMBLOCK_SEND_PRI_STD_MID_WT。
    MEMBLOCK_SEND_PRI_STD_HIGH_WT。
```

```text
plus.sv::reload_from_cmdline():
  读取 MEMBLOCK_LOAD_PIP_NUM_LIMIT。
  读取 MEMBLOCK_STA_PIP_NUM_LIMIT。
  读取 MEMBLOCK_STD_PIP_NUM_LIMIT。

  新增读取：
    load_bit("MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN", MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN)
    load_bit("MEMBLOCK_STA_PIP_NUM_RANDOM_EN", MEMBLOCK_STA_PIP_NUM_RANDOM_EN)
    load_bit("MEMBLOCK_STD_PIP_NUM_RANDOM_EN", MEMBLOCK_STD_PIP_NUM_RANDOM_EN)

  删除读取：
    load_bit("MEMBLOCK_SEND_PRI_EN", MEMBLOCK_SEND_PRI_EN)

  新增读取：
    load_bit("MEMBLOCK_SEND_PRI_MODE_EN", MEMBLOCK_SEND_PRI_MODE_EN)
    load_int("MEMBLOCK_GLOBAL_SEND_PRI_EN_WT", MEMBLOCK_GLOBAL_SEND_PRI_EN_WT)

  保留读取：
    MEMBLOCK_SEND_PRI_DEFAULT 和 low/mid/high 权重。
    MEMBLOCK_SEND_PRI_STD_DEFAULT 和 std low/mid/high 权重。
```

### 5.6.2 `seq_csr_common.sv` 字段与参数加载伪代码

目标：把旧 `send_pri_en` 拆成 `send_pri_mode_en` 和 `global_send_pri_en_wt`；把 pipe 配置值和本拍采样值分离。

```text
seq_csr_common 字段区：
  保留：
    load_pip_num_limit = 3。
    sta_pip_num_limit = 2。
    std_pip_num_limit = 2。
    real_load_pipe_num = 3。
    real_sta_pipe_num = 2。
    real_std_pipe_num = 2。

  新增：
    load_pip_num_limit_random_en = 0。
    sta_pip_num_limit_random_en = 0。
    std_pip_num_limit_random_en = 0。

  删除：
    send_pri_en。

  新增：
    send_pri_mode_en = 1。
    global_send_pri_en_wt = 0。

  保留：
    send_pri_default。
    send_pri_low_wt/mid_wt/high_wt。
    send_pri_std_default。
    send_pri_std_low_wt/mid_wt/high_wt。
```

```text
seq_csr_common::load_from_plus():
  原有 load_pip_num_limit/sta_pip_num_limit/std_pip_num_limit 从 plus 读取逻辑保留。
  原有 real_load_pipe_num/real_sta_pipe_num/real_std_pipe_num 从 plus 读取逻辑保留。

  新增：
    load_pip_num_limit_random_en = plus::MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN
    sta_pip_num_limit_random_en = plus::MEMBLOCK_STA_PIP_NUM_RANDOM_EN
    std_pip_num_limit_random_en = plus::MEMBLOCK_STD_PIP_NUM_RANDOM_EN

  删除：
    send_pri_en = plus::MEMBLOCK_SEND_PRI_EN

  新增：
    send_pri_mode_en = plus::MEMBLOCK_SEND_PRI_MODE_EN
    global_send_pri_en_wt = get_non_negative_int("MEMBLOCK_GLOBAL_SEND_PRI_EN_WT",
                                                 plus::MEMBLOCK_GLOBAL_SEND_PRI_EN_WT)

  保留：
    send_pri_default = get_non_negative_int(...)
    send_pri_low_wt/mid_wt/high_wt = get_non_negative_int(...)
    send_pri_std_default = get_non_negative_int(...)
    send_pri_std_low_wt/mid_wt/high_wt = get_non_negative_int(...)
```

### 5.6.3 `seq_csr_common.sv` 合法性检查伪代码

目标：保证 pipe 上限不超过 RTL 上限，global 权重在 0 到 100，send_pri 权重只在 send_pri 模式下检查。

```text
seq_csr_common::validate_and_clamp():
  保留已有真实 RTL 上限检查：
    fatal_if_zero("real_load_pipe_num", real_load_pipe_num)
    fatal_if_zero("real_sta_pipe_num", real_sta_pipe_num)
    fatal_if_zero("real_std_pipe_num", real_std_pipe_num)

  保留已有 pipe 上限 clamp：
    clamp_int("load_pip_num_limit", load_pip_num_limit, 1, real_load_pipe_num)
    clamp_int("sta_pip_num_limit", sta_pip_num_limit, 1, real_sta_pipe_num)
    clamp_int("std_pip_num_limit", std_pip_num_limit, 1, real_std_pipe_num)

  新增 global 权重 clamp：
    clamp_int("global_send_pri_en_wt", global_send_pri_en_wt, 0, 100)

  保留 default priority clamp：
    clamp_int("send_pri_default", send_pri_default, 0, 100)
    clamp_int("send_pri_std_default", send_pri_std_default, 0, 100)

  修改权重全 0 检查条件：
    如果 send_pri_mode_en == 1：
      fatal_if_all_zero3("send_pri weights",
                         send_pri_low_wt,
                         send_pri_mid_wt,
                         send_pri_high_wt)
      fatal_if_all_zero3("send_pri_std weights",
                         send_pri_std_low_wt,
                         send_pri_std_mid_wt,
                         send_pri_std_high_wt)
    否则：
      不检查 send_pri 权重全 0。
      原因是非 send_pri 模式只使用 default，不使用 low/mid/high 权重。
```

### 5.6.4 `seq_csr_common.sv` getter 和 sample 函数伪代码

目标：删除旧 getter，新增模式 getter、配置上限 getter、本拍随机采样函数。

逻辑功能：

```text
本节定义 seq_csr_common 对外提供的配置读取接口。
getter 类函数只返回已经加载并校验过的配置值；sample 类函数会在每拍 issue select 前产生本拍实际使用值。
这样可以把“配置上限是什么”和“本拍实际用几个 pipe/是否 global priority”拆开，避免旧 get_* 函数同时承担静态配置和动态采样两种语义。
```

```text
删除函数：
  get_send_pri_en()

新增函数 get_send_pri_mode_en():
  1. 调用 check_initialized("get_send_pri_mode_en")。
     该子函数检查 seq_csr_common 是否已经完成 plus/default cfg 加载；如果未初始化，会报错阻止读取未定义配置。
  2. 返回 send_pri_mode_en。
     返回值表示主表是否随机 priority、issue 仲裁是否启用 priority 比较。

新增函数 get_global_send_pri_en_wt():
  1. 调用 check_initialized("get_global_send_pri_en_wt")。
     该子函数保证读取发生在配置加载之后。
  2. 返回 global_send_pri_en_wt。
     返回值是 0 到 100 的权重，表示 send_pri 模式下每拍采样为 global priority 仲裁的概率。

新增函数 sample_global_send_pri_en():
  1. 调用 check_initialized("sample_global_send_pri_en")。
     该子函数保证本拍采样使用的是已经加载和 clamp 过的配置。
  2. 如果 send_pri_mode_en == 0：
     返回 0。
     含义是非 send_pri 模式下禁止 global priority，后续 select 只按 ROB age。
  3. 如果 global_send_pri_en_wt == 0：
     返回 0。
     含义是 send_pri 模式开启，但本方案默认不跨 LOAD/STA/STD 三个 queue 做全局最高 priority 过滤。
  4. 如果 global_send_pri_en_wt >= 100：
     返回 1。
     含义是每拍都强制走 global priority 模式。
  5. 调用 $urandom_range(99, 0) 生成 rand_value。
     该随机值落在 0 到 99，用于和百分比权重比较。
  6. 如果 rand_value < global_send_pri_en_wt：
     返回 1，表示本拍走 global priority。
  7. 否则返回 0，表示本拍走 non-global priority。
```

```text
重命名配置上限 getter：
  get_load_pip_num_limit() 改名为 get_load_pip_num_limit()
  get_sta_pip_num_limit() 改名为 get_sta_pip_num_limit()
  get_std_pip_num_limit() 改名为 get_std_pip_num_limit()

get_load_pip_num_limit():
  1. 调用 check_initialized("get_load_pip_num_limit")。
  2. 返回 load_pip_num_limit。
     该值表示 LOAD 本拍采样函数允许返回的最大 pipe 数，也是默认非随机模式下直接使用的 pipe 数。

get_sta_pip_num_limit():
  1. 调用 check_initialized("get_sta_pip_num_limit")。
  2. 返回 sta_pip_num_limit。
     该值表示 STA 本拍采样函数允许返回的最大 pipe 数，也是默认非随机模式下直接使用的 pipe 数。

get_std_pip_num_limit():
  1. 调用 check_initialized("get_std_pip_num_limit")。
  2. 返回 std_pip_num_limit。
     该值表示 STD 本拍采样函数允许返回的最大 pipe 数，也是默认非随机模式下直接使用的 pipe 数。
```

```text
新增 pipe 随机开关 getter：
  get_load_pip_num_random_en():
    1. 调用 check_initialized("get_load_pip_num_random_en")。
    2. 返回 load_pip_num_limit_random_en。
       返回 0 表示 LOAD 每拍固定使用 load_pip_num_limit 最大值；返回 1 表示每拍在 [1:load_pip_num_limit] 内随机。

  get_sta_pip_num_random_en():
    1. 调用 check_initialized("get_sta_pip_num_random_en")。
    2. 返回 sta_pip_num_limit_random_en。
       返回 0 表示 STA 每拍固定使用 sta_pip_num_limit 最大值；返回 1 表示每拍在 [1:sta_pip_num_limit] 内随机。

  get_std_pip_num_random_en():
    1. 调用 check_initialized("get_std_pip_num_random_en")。
    2. 返回 std_pip_num_limit_random_en。
       返回 0 表示 STD 每拍固定使用 std_pip_num_limit 最大值；返回 1 表示每拍在 [1:std_pip_num_limit] 内随机。
```

```text
新增本拍 pipe 数采样函数：
  sample_load_pip_num():
    1. 调用 check_initialized("sample_load_pip_num")。
       该子函数保证 load_pip_num_limit 和 load_pip_num_limit_random_en 已经从 plus/cfg 加载完成。
    2. 如果 load_pip_num_limit_random_en == 0：
       返回 load_pip_num_limit。
       含义是默认非随机模式下，本拍 LOAD 最多选择配置上限数量的 LOAD item。
    3. 如果 load_pip_num_limit_random_en == 1：
       调用 $urandom_range(load_pip_num_limit, 1)。
       返回 [1:load_pip_num_limit] 内的随机值。
       含义是随机模式下，本拍 LOAD 尝试驱动的 port 数随机变化，但不会为 0。

  sample_sta_pip_num():
    1. 调用 check_initialized("sample_sta_pip_num")。
    2. 如果 sta_pip_num_limit_random_en == 0：
       返回 sta_pip_num_limit。
    3. 如果 sta_pip_num_limit_random_en == 1：
       调用 $urandom_range(sta_pip_num_limit, 1)，返回 [1:sta_pip_num_limit] 内的随机值。

  sample_std_pip_num():
    1. 调用 check_initialized("sample_std_pip_num")。
    2. 如果 std_pip_num_limit_random_en == 0：
       返回 std_pip_num_limit。
    3. 如果 std_pip_num_limit_random_en == 1：
       调用 $urandom_range(std_pip_num_limit, 1)，返回 [1:std_pip_num_limit] 内的随机值。
```

### 5.6.5 `memblock_dispatch_base_sequence.sv` 主表 priority 生成伪代码

目标：主表是否随机 `send_pri` 只由 `MEMBLOCK_SEND_PRI_MODE_EN` 控制，不再受 global/local issue 模式影响。

逻辑功能：

```text
本节定义主表生成阶段如何填写 send_pri/send_pri_std。
send_pri_mode_en=0 时，主表 priority 不参与仲裁，字段只填默认值，保证字段合法但不引入随机优先级影响。
send_pri_mode_en=1 时，主表 priority 按 low/mid/high 权重随机生成，后续 issue scheduler 可以用它做 queue 内或全局 priority 仲裁。
```

```text
memblock_dispatch_base_sequence::randomize_send_pri_value(is_std):
  1. 读取 seq_csr_common::get_send_pri_mode_en()。
     该子函数返回当前 testcase 是否启用 send_pri 仲裁体系。

  2. 如果 get_send_pri_mode_en() 返回 0：
     表示非 send_pri 模式，主表不需要随机 priority。
     如果 is_std == 1：
       调用 seq_csr_common::get_send_pri_std_default()，返回 STD 专用默认 priority。
     否则：
       调用 seq_csr_common::get_send_pri_default()，返回 LOAD/STA 默认 priority。
     这两个 default getter 的职责是读取已经加载并 clamp 过的默认值，保证字段落在合法范围内。

  3. 如果 get_send_pri_mode_en() 返回 1：
     表示 send_pri 模式开启，需要根据权重随机 low/mid/high 桶。

  4. 如果 is_std == 1：
     分别调用 get_send_pri_std_low_wt()、get_send_pri_std_mid_wt()、get_send_pri_std_high_wt()。
     这些 getter 提供 STD data issue 使用的 priority 桶权重。
     调用 rand_weighted3(std_low, std_mid, std_high) 生成 sel。
     rand_weighted3 的职责是在三个权重桶中随机选择一个桶，返回 0/1/2。

  5. 如果 is_std == 0：
     分别调用 get_send_pri_low_wt()、get_send_pri_mid_wt()、get_send_pri_high_wt()。
     这些 getter 提供 LOAD 和 STA 使用的 priority 桶权重。
     调用 rand_weighted3(low, mid, high) 生成 sel。

  6. 如果 sel == 0：
     调用 $urandom_range(33, 0)，返回 low priority 范围 [0:33] 内的随机值。

  7. 如果 sel == 1：
     调用 $urandom_range(66, 34)，返回 mid priority 范围 [34:66] 内的随机值。

  8. 如果 sel 是其他值：
     调用 $urandom_range(100, 67)，返回 high priority 范围 [67:100] 内的随机值。
```

```text
memblock_dispatch_base_sequence::randomize_main_transaction():
  1. 保持原有 transaction 随机化逻辑不变。
     该阶段仍负责生成 fuType/fuOpType、load/store 类型、地址、ROB/LQ/SQ 相关字段等主表内容。

  2. 保持原有 op template 修正逻辑不变。
     该逻辑负责把主表字段修正成当前 memblock 测试框架支持的合法操作组合。

  3. 调用 randomize_send_pri_value(0)，将返回值写入 tr.send_pri。
     这里的 is_std=0 表示该字段服务 LOAD/STA 侧 priority。

  4. 调用 randomize_send_pri_value(1)，将返回值写入 tr.send_pri_std。
     这里的 is_std=1 表示该字段服务 STD 侧 priority。

  5. 如果 send_pri_mode_en=0：
     两个字段都来自 default，后续 issue scheduler 即使看到字段，也不会用 priority 仲裁。

  6. 如果 send_pri_mode_en=1：
     两个字段按 low/mid/high 权重随机，后续 issue scheduler 可以根据模式决定 queue 内比较或 global filter。
```

### 5.6.6 `issue_queue_scheduler.sv` 调度选择伪代码

目标：复用 `item_is_better()` 和 `find_global_max_send_pri()`，但把 `compare_pri` 和 `use_global_pri` 拆成两个独立控制。

逻辑功能：

```text
本节定义 issue scheduler 每拍如何从 LOAD/STA/STD 三个 issue queue 中选出要驱动到 DUT 的 item。
compare_pri 只表示“单个 queue 内是否先比较 priority”；use_global_pri 表示“本拍是否先跨三个 queue 找全局最高 priority，并只允许这个 priority 的 item 发射”。
这两个控制拆开后，可以支持三种模式：非 send_pri、send_pri 但各 queue 独立、send_pri 且全局最高 priority 过滤。
```

```text
issue_queue_scheduler::item_is_better(candidate, best, compare_pri):
  1. 输入 candidate、best、compare_pri。
     candidate 是当前扫描到的候选 item。
     best 是当前已找到的最优 item。
     compare_pri 表示本次比较是否启用 priority。

  2. 如果 compare_pri == 1：
     先比较 candidate.send_pri 和 best.send_pri。
     如果 candidate.send_pri > best.send_pri：
       返回 1，表示 candidate 比 best 更应该被选中。
     如果 candidate.send_pri < best.send_pri：
       返回 0，表示 best 保持不变。

  3. 如果 compare_pri == 0，或者两个 item 的 send_pri 相同：
     调用 item_is_older(candidate, best)。
     item_is_older 的职责是按 ROB age 判断哪个 item 更老。
     返回 item_is_older 的结果，保证同 priority 或非 send_pri 模式下仍按 ROB 顺序仲裁。
```

```text
issue_queue_scheduler::find_global_max_send_pri(global_pri):
  1. 初始化 found=0，global_pri=0。
     found 表示三个 queue 中是否至少存在一个当前可发射的 item。

  2. 遍历 load_issue_q。
     对每个 LOAD item 调用 is_issue_item_eligible(item)。
     is_issue_item_eligible 的职责是过滤 flush/redirect/replay 后不应发射的旧 item，以及尚未 ready、已经 dispatched 或状态不合法的 item。
     如果 item eligible 且 found=0：
       设置 global_pri=item.send_pri，found=1。
     如果 item eligible 且 item.send_pri > global_pri：
       更新 global_pri=item.send_pri。

  3. 遍历 sta_issue_q，执行与 LOAD 相同的 eligible 检查和最大 priority 更新。

  4. 遍历 std_issue_q，执行与 LOAD 相同的 eligible 检查和最大 priority 更新。
     STD item 使用的是 item 中已经展开好的 send_pri 值；该值来自主表 send_pri_std。

  5. 如果 found == 1：
     返回 1，表示找到了本拍 global priority 值，输出参数 global_pri 有效。

  6. 如果 found == 0：
     返回 0，表示三个 queue 没有任何 eligible item。
     调用者 select_issue_candidates() 会取消 use_global_pri，避免拿无效 global_pri 过滤全部 item。
```

```text
issue_queue_scheduler::select_target_candidates(target,
                                                max_count,
                                                compare_pri,
                                                use_global_pri,
                                                global_pri,
                                                selected):
  1. 清空输出队列 selected。
     selected 是本 target 本拍最终选出的 item 列表，会被 sequence 后续转换成 driver xaction。

  2. 如果 max_count == 0：
     直接 return。
     max_count 是本拍该 target 最多尝试驱动的 pipe 数；正常配置下 sample_*_pip_num() 不返回 0，但保留该分支作为防御。

  3. 创建 selected_indices 空队列。
     selected_indices 记录本函数本轮 while 中已经选过的 queue index，防止同一个 queue entry 被重复选中。

  4. 当 selected.size < max_count 时进入 while 循环。
     每一轮 while 最多选择一个当前 target 的最佳 item。

  5. 在每轮 while 开始时设置 found=0、best_idx=-1、best_item=空值。
     found 表示本轮是否找到一个可选 item。

  6. for 循环从 idx=0 扫描到 get_target_queue_size(target)-1。
     get_target_queue_size(target) 的职责是返回 LOAD/STA/STD 对应 issue queue 当前长度。

  7. 如果 idx 已经存在于 selected_indices：
     continue。
     这个分支避免本函数在同一拍同一个 target 中重复选择同一个 queue entry。

  8. 调用 get_target_queue_item(target, idx) 读取 item。
     该 helper 根据 target 从 load_issue_q/sta_issue_q/std_issue_q 中取出对应 index 的 item。

  9. 调用 is_issue_item_eligible(item)。
     该 helper 检查 item 当前是否仍能发射，包括 uid 是否 active/enq/issue_ready、是否被 flush/redirect/replay 阻塞、replay_seq 是否匹配、target 是否已经 dispatched 等。
     如果返回 0：
       continue，不让该 item 参与本拍选择。

  10. 如果 use_global_pri == 1 且 item.send_pri != global_pri：
      continue。
      这个分支实现 global send_pri 模式：本拍只允许全局最高 priority 的 item 参与 LOAD/STA/STD 各自选择。

  11. 如果 found == 0：
      将当前 item 作为本轮临时 best。
      设置 best_idx=idx、best_item=item、found=1。

  12. 如果 found == 1：
      调用 item_is_better(item, best_item, compare_pri)。
      该 helper 根据 compare_pri 决定先比 send_pri 还是只比 ROB age。
      如果返回 1：
        更新 best_idx=idx、best_item=item。

  13. for 循环结束后，如果 found == 0：
      break。
      表示当前 target queue 没有更多 eligible item，本 target 本拍选择结束。

  14. 如果 found == 1：
      将 best_item push 到 selected。
      将 best_idx push 到 selected_indices。
      然后继续下一轮 while，直到 selected.size 达到 max_count 或没有更多 eligible item。
```

```text
issue_queue_scheduler::select_issue_candidates(load_items, sta_items, std_items):
  1. 调用 ensure_data()。
     该 helper 确保 scheduler 已经绑定 common_data_transaction；后续 eligible 检查、状态读取和 queue 访问都依赖 data。

  2. 清空输出队列 load_items、sta_items、std_items。
     这三个队列是本拍要交给 sequence/driver 的候选集合。

  3. 调用 data.issue_blocked_by_global_flush()。
     该 helper 检查当前是否处于 redirect/flush 全局阻塞状态。
     如果返回 1：
       设置 data.issue_freeze_ack = 1。
       直接 return。
       含义是 active redirect/flush 期间不再选择新 item，防止旧 epoch item 继续发射。

  4. 调用 seq_csr_common::get_send_pri_mode_en()，得到 send_pri_mode。
     send_pri_mode 表示本 testcase 是否启用 priority 仲裁体系。

  5. 调用 seq_csr_common::sample_global_send_pri_en()，得到 global_mode。
     该 helper 在 send_pri 模式下按 global_send_pri_en_wt 每拍随机决定是否走 global priority。

  6. 设置 compare_pri = send_pri_mode。
     如果 send_pri_mode=0，后续 item_is_better() 只比较 ROB age。
     如果 send_pri_mode=1，后续 item_is_better() 先比较 send_pri，同 priority 再比较 ROB age。

  7. 设置 use_global_pri = send_pri_mode && global_mode。
     只有 send_pri 模式开启且本拍采样为 global 时，才允许跨 queue 全局 priority 过滤。

  8. 如果 use_global_pri == 1：
     调用 find_global_max_send_pri(global_pri)。
     该 helper 扫描 LOAD/STA/STD 三个 queue 中所有 eligible item，输出本拍全局最高 priority。
     如果 find_global_max_send_pri 返回 0：
       设置 use_global_pri = 0。
       含义是当前没有 eligible item，不使用无效 global_pri 过滤。

  9. 调用 seq_csr_common::sample_load_pip_num() 得到 load_max。
     该 helper 根据固定/随机模式返回本拍 LOAD 最多选择数量。

  10. 调用 seq_csr_common::sample_sta_pip_num() 得到 sta_max。
      该 helper 根据固定/随机模式返回本拍 STA 最多选择数量。

  11. 调用 seq_csr_common::sample_std_pip_num() 得到 std_max。
      该 helper 根据固定/随机模式返回本拍 STD 最多选择数量。

  12. 调用 select_target_candidates(LOAD, load_max, compare_pri, use_global_pri, global_pri, load_items)。
      该 helper 从 load_issue_q 中选出本拍 LOAD 候选，最多 load_max 个。

  13. 调用 select_target_candidates(STA, sta_max, compare_pri, use_global_pri, global_pri, sta_items)。
      该 helper 从 sta_issue_q 中选出本拍 STA 候选，最多 sta_max 个。

  14. 调用 select_target_candidates(STD, std_max, compare_pri, use_global_pri, global_pri, std_items)。
      该 helper 从 std_issue_q 中选出本拍 STD 候选，最多 std_max 个。
```

三种模式在这段逻辑里的展开结果：

```text
非 send_pri 模式：
  send_pri_mode=0
  compare_pri=0
  use_global_pri=0
  每个 target queue 只按 ROB age 选。

send_pri + non-global 模式：
  send_pri_mode=1
  global_mode=0
  compare_pri=1
  use_global_pri=0
  每个 target queue 内部先比 send_pri，再比 ROB age。

send_pri + global 模式：
  send_pri_mode=1
  global_mode=1
  compare_pri=1
  use_global_pri=1
  先跨三个 queue 找 global_pri。
  每个 target queue 只允许 item.send_pri == global_pri 的 item 参与选择。
  同 global_pri 内再按 ROB age。
```

### 5.6.7 调用点替换伪代码

目标：删除旧函数名后，所有调用点必须改到新函数，避免编译残留。

逻辑功能：

```text
本节用于指导全仓替换，核心目标是消除旧 MEMBLOCK_SEND_PRI_EN/send_pri_en/get_send_pri_en() 的单一语义。
替换时必须按调用点真实含义选择新接口：主表生成看 send_pri_mode_en；issue 每拍调度看 sample_global_send_pri_en()；pipe 配置上限读取看 *_limit；本拍发射数量看 sample_*。
```

```text
全仓搜索：
  get_send_pri_en()

处理：
  1. 在 randomize_send_pri_value() 中替换为 get_send_pri_mode_en()。
     原因是主表生成阶段只需要知道 priority 字段是否随机，不应该关心本拍 issue 是否 global。

  2. 在 select_issue_candidates() 中拆成两个读取：
     调用 get_send_pri_mode_en() 得到 send_pri_mode。
     调用 sample_global_send_pri_en() 得到本拍 global_mode。
     原因是 issue 选择阶段同时需要知道是否比较 priority，以及本拍是否跨 queue 做 global filter。

  3. 如果发现其他调用点：
     先判断该调用点是在“生成字段”还是“本拍仲裁”。
     如果是生成字段或决定是否使用 priority 值：
       使用 get_send_pri_mode_en()。
     如果是每拍选择是否做全局最高 priority 过滤：
       使用 sample_global_send_pri_en()。
     如果语义不属于这两类：
       需要在 review 中单独说明，不能机械替换。
```

```text
全仓搜索：
  get_load_pip_num_limit()
  get_sta_pip_num_limit()
  get_std_pip_num_limit()

处理：
  1. 如果调用点是 issue scheduler 选择本拍候选数量：
     替换为 sample_load_pip_num()。
     替换为 sample_sta_pip_num()。
     替换为 sample_std_pip_num()。
     原因是本拍实际尝试驱动的 port 数需要支持随机模式。

  2. 如果调用点只是打印配置值、文档统计、debug 或合法性说明：
     替换为 get_load_pip_num_limit()。
     替换为 get_sta_pip_num_limit()。
     替换为 get_std_pip_num_limit()。
     原因是这些场景需要展示配置上限，而不是触发一次新的随机采样。

  3. 如果调用点既影响调度又用于日志：
     调度路径使用 sample_*_pip_num()。
     日志中记录本拍采样结果变量，例如 load_max/sta_max/std_max。
     不要在日志里再次调用 sample_*，避免日志打印值和真实调度值不一致。
```

```text
全仓搜索：
  MEMBLOCK_SEND_PRI_EN

处理：
  1. 在 plus.sv 中删除 MEMBLOCK_SEND_PRI_EN 的声明。
     该旧参数同时表达“是否使用 priority”和“是否 global”，语义过粗，不能继续保留。

  2. 在 plus.sv 的 reload_from_cmdline() 中删除 MEMBLOCK_SEND_PRI_EN 的解析。
     避免命令行旧参数继续影响新配置。

  3. 在 default.cfg 中删除 +MEMBLOCK_SEND_PRI_EN=...。
     新默认配置统一写成 +MEMBLOCK_SEND_PRI_MODE_EN=1 和 +MEMBLOCK_GLOBAL_SEND_PRI_EN_WT=0。

  4. 在 testcase cfg 中删除旧配置行，并按旧 testcase 真实目的改写：
     如果旧值为 1，且目的是强制全局最高 priority：
       写 +MEMBLOCK_SEND_PRI_MODE_EN=1。
       写 +MEMBLOCK_GLOBAL_SEND_PRI_EN_WT=100。
     如果旧值为 0，且目的是完全不使用 send_pri：
       写 +MEMBLOCK_SEND_PRI_MODE_EN=0。
     如果旧值为 0，且目的是使用 priority 但不跨 queue 全局过滤：
       写 +MEMBLOCK_SEND_PRI_MODE_EN=1。
       写 +MEMBLOCK_GLOBAL_SEND_PRI_EN_WT=0。

  5. 同步 flow 文档和网页文档。
     文档中不能继续把 MEMBLOCK_SEND_PRI_EN 描述成唯一 send_pri 控制源。
```

### 5.6.8 `default.cfg` 和 testcase cfg 伪代码

目标：默认启用 send_pri、本地 priority，pipe 数默认使用配置最大值；特殊 testcase 可显式开启 pipe 随机或强制 global。

逻辑功能：

```text
本节定义默认配置和 testcase 覆盖配置。
默认配置选择 send_pri 模式开启、global 权重为 0，也就是默认走“各 queue 内部按 priority 仲裁，不跨 queue 做全局最高 priority 过滤”。
pipe 默认随机开关为 0，表示每拍固定使用配置上限作为尝试驱动数量。
```

```text
default.cfg：
  1. 删除 +MEMBLOCK_SEND_PRI_EN=0。
     旧参数不再作为配置入口。

  2. 使用 +MEMBLOCK_LOAD_PIP_NUM_LIMIT=3。
     该值表示 LOAD pipe 配置上限；默认 sample_load_pip_num() 返回 3，随机模式下在 [1:3] 内采样。

  3. 使用 +MEMBLOCK_STA_PIP_NUM_LIMIT=2。
     该值表示 STA pipe 配置上限；默认 sample_sta_pip_num() 返回 2，随机模式下在 [1:2] 内采样。

  4. 使用 +MEMBLOCK_STD_PIP_NUM_LIMIT=2。
     该值表示 STD pipe 配置上限；默认 sample_std_pip_num() 返回 2，随机模式下在 [1:2] 内采样。

  5. 新增 +MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN=0。
     表示默认 LOAD 每拍固定使用 LOAD_PIP_NUM 最大值，不随机。

  6. 新增 +MEMBLOCK_STA_PIP_NUM_RANDOM_EN=0。
     表示默认 STA 每拍固定使用 STA_PIP_NUM 最大值，不随机。

  7. 新增 +MEMBLOCK_STD_PIP_NUM_RANDOM_EN=0。
     表示默认 STD 每拍固定使用 STD_PIP_NUM 最大值，不随机。

  8. 新增 +MEMBLOCK_SEND_PRI_MODE_EN=1。
     表示默认主表生成 priority，issue scheduler 默认启用 priority 比较。

  9. 新增 +MEMBLOCK_GLOBAL_SEND_PRI_EN_WT=0。
     表示默认每拍 global priority 采样为 0，走 non-global send_pri 模式。
```

```text
如果某个 testcase 需要开启 pipe 数随机：
  1. 添加 +MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN=1。
  2. 添加 +MEMBLOCK_STA_PIP_NUM_RANDOM_EN=1。
  3. 添加 +MEMBLOCK_STD_PIP_NUM_RANDOM_EN=1。
  4. 这会让 sample_*_pip_num() 每拍在 [1:MEMBLOCK_*_PIP_NUM_LIMIT] 内随机。

如果某个 testcase 需要完全关闭 send_pri：
  1. 添加 +MEMBLOCK_SEND_PRI_MODE_EN=0。
  2. 主表 send_pri/send_pri_std 使用 default。
  3. issue scheduler 的 compare_pri=0，只按 ROB age 仲裁。

如果某个 testcase 需要每拍都走 global priority：
  1. 添加 +MEMBLOCK_SEND_PRI_MODE_EN=1。
  2. 添加 +MEMBLOCK_GLOBAL_SEND_PRI_EN_WT=100。
  3. sample_global_send_pri_en() 每拍返回 1。
  4. select_issue_candidates() 每拍调用 find_global_max_send_pri()，并用 global_pri 过滤三个 queue。

如果某个 testcase 需要随机混合 global/local：
  1. 添加 +MEMBLOCK_SEND_PRI_MODE_EN=1。
  2. 添加 +MEMBLOCK_GLOBAL_SEND_PRI_EN_WT=<0到100之间的值>。
  3. sample_global_send_pri_en() 每拍按该权重随机返回 1 或 0。
  4. 返回 1 的拍走 global priority，返回 0 的拍走 non-global priority。
```

## 6. 修改后完整程序流

### 6.1 主表生成

```text
build_main_table
  -> build_random_main_table
    -> randomize_main_transaction
      -> randomize_send_pri_value(is_std=0)
        如果 send_pri_mode_en=0：返回 MEMBLOCK_SEND_PRI_DEFAULT
        如果 send_pri_mode_en=1：按 low/mid/high 权重随机 send_pri
      -> randomize_send_pri_value(is_std=1)
        如果 send_pri_mode_en=0：返回 MEMBLOCK_SEND_PRI_STD_DEFAULT
        如果 send_pri_mode_en=1：按 std low/mid/high 权重随机 send_pri_std
```

### 6.2 本拍 issue select

```text
route_all_issue_queues / lintsissue flow
  -> issue_sched.select_issue_candidates(load_items, sta_items, std_items)
    -> sample_global_send_pri_en()
    -> sample_load_pip_num()
    -> sample_sta_pip_num()
    -> sample_std_pip_num()
    -> find_global_max_send_pri()       仅 use_global_pri=1 时调用
    -> select_target_candidates(LOAD)
      -> is_issue_item_eligible()
      -> item_is_better(compare_pri)
    -> select_target_candidates(STA)
      -> is_issue_item_eligible()
      -> item_is_better(compare_pri)
    -> select_target_candidates(STD)
      -> is_issue_item_eligible()
      -> item_is_better(compare_pri)
```

## 7. 关键场景例子

假设队列中有：

```text
LOAD: uid=3 send_pri=60, uid=4 send_pri=20
STA : uid=5 send_pri=80
STD : uid=6 send_pri_std=70
```

### 7.1 非 send_pri 模式

```text
MEMBLOCK_SEND_PRI_MODE_EN=0
```

行为：

```text
LOAD 内选更老 ROB。
STA 内选更老 ROB。
STD 内选更老 ROB。
send_pri=80 不会压过更老的低 priority 请求。
```

### 7.2 global send_pri 模式

```text
MEMBLOCK_SEND_PRI_MODE_EN=1
本拍 global_send_pri=1
```

行为：

```text
全局最大 priority 是 STA uid=5 的 80。
本拍只允许 send_pri==80 的 item 参与选择。
LOAD uid=3 的 60 和 STD uid=6 的 70 都被本拍过滤。
```

### 7.3 non-global send_pri 模式

```text
MEMBLOCK_SEND_PRI_MODE_EN=1
本拍 global_send_pri=0
```

行为：

```text
LOAD queue 内部选 send_pri=60 的 uid=3。
STA queue 内部选 send_pri=80 的 uid=5。
STD queue 内部选 send_pri_std=70 的 uid=6。
三个 queue 不互相压制。
```

## 8. 验收点

### 8.1 默认配置验收

默认 cfg：

```text
+MEMBLOCK_SEND_PRI_MODE_EN=1
+MEMBLOCK_GLOBAL_SEND_PRI_EN_WT=0
+MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN=0
+MEMBLOCK_STA_PIP_NUM_RANDOM_EN=0
+MEMBLOCK_STD_PIP_NUM_RANDOM_EN=0
```

预期：

```text
主表 send_pri/send_pri_std 按 low/mid/high 权重随机。
每拍 issue select 走 non-global_send_pri。
LOAD/STA/STD 每拍实际选择数量默认分别等于 LOAD_PIP_NUM、STA_PIP_NUM、STD_PIP_NUM。
```

### 8.2 非 send_pri 模式验收

配置：

```text
+MEMBLOCK_SEND_PRI_MODE_EN=0
```

预期：

```text
主表 send_pri     全部等于 MEMBLOCK_SEND_PRI_DEFAULT。
主表 send_pri_std 全部等于 MEMBLOCK_SEND_PRI_STD_DEFAULT。
issue select 不比较 priority，只按 ROB age。
```

### 8.3 全局 send_pri 模式验收

配置：

```text
+MEMBLOCK_SEND_PRI_MODE_EN=1
+MEMBLOCK_GLOBAL_SEND_PRI_EN_WT=100
```

预期：

```text
每拍都走 global_send_pri。
find_global_max_send_pri() 每拍有效队列非空时会被使用。
三个 queue 只发全局最大 priority 的 item。
```

### 8.4 pipe 随机模式验收

配置：

```text
+MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN=1
+MEMBLOCK_STA_PIP_NUM_RANDOM_EN=1
+MEMBLOCK_STD_PIP_NUM_RANDOM_EN=1
```

预期：

```text
sample_load_pip_num() 在 [1:MEMBLOCK_LOAD_PIP_NUM_LIMIT] 内随机。
sample_sta_pip_num()  在 [1:MEMBLOCK_STA_PIP_NUM_LIMIT] 内随机。
sample_std_pip_num()  在 [1:MEMBLOCK_STD_PIP_NUM_LIMIT] 内随机。
每拍尝试驱动数量可以小于配置最大值，但不能为 0。
```


## 9. 风险与注意事项

### 9.1 删除旧参数后的配置迁移

旧 cfg 如果设置了：

```text
+MEMBLOCK_SEND_PRI_EN=1
```

需要改成：

```text
+MEMBLOCK_SEND_PRI_MODE_EN=1
+MEMBLOCK_GLOBAL_SEND_PRI_EN_WT=100
```

旧 cfg 如果设置了：

```text
+MEMBLOCK_SEND_PRI_EN=0
```

需要根据原意拆成两种：

```text
完全不使用 send_pri：
  +MEMBLOCK_SEND_PRI_MODE_EN=0

使用 send_pri，但不跨 queue 做 global filter：
  +MEMBLOCK_SEND_PRI_MODE_EN=1
  +MEMBLOCK_GLOBAL_SEND_PRI_EN_WT=0
```

### 9.2 权重全 0 检查

只有 `MEMBLOCK_SEND_PRI_MODE_EN=1` 时才需要检查 low/mid/high 权重不能全 0。非 send_pri 模式不使用权重，不应因为权重配置为 0 导致 fatal。

### 9.3 pipe 随机不能返回 0

`sample_*_pip_num()` 随机范围必须是 `[1:MEMBLOCK_*_PIP_NUM_LIMIT]`，不能返回 0。返回 0 会导致该 target 本拍完全不发射，和“随机使用可用 pipe 数”的需求不一致。

### 9.4 pipe 随机对旧 testcase 的影响

本方案默认：

```text
+MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN=0
+MEMBLOCK_STA_PIP_NUM_RANDOM_EN=0
+MEMBLOCK_STD_PIP_NUM_RANDOM_EN=0
```

默认随机开关为 0 时，LOAD/STA/STD 每拍使用配置最大值，保持旧 testcase 的固定节奏。如果某个 smoke/testcase 需要提高 pipe 数变化覆盖，应在该 testcase cfg 中显式设置：

```text
+MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN=1
+MEMBLOCK_STA_PIP_NUM_RANDOM_EN=1
+MEMBLOCK_STD_PIP_NUM_RANDOM_EN=1
```

### 9.5 文档同步

实现代码后需要同步更新：

- `AI_DOC/mem_ut_flow_doc/load_sta_std_issue_flow.md`
- `AI_DOC/web/memblock_dispatch_control_flow_callgraph_enhanced`
- plus 参数说明表/项目管理规则中关于 send_pri 和 pipe 数的章节

## 10. 推荐 coding 顺序

1. 修改 `plus.sv`，增加参数声明和命令行解析。
2. 修改 `seq_csr_common.sv`，增加参数缓存、合法性检查、getter/sample 函数。
3. 修改 `memblock_dispatch_base_sequence.sv`，将主表 priority 生成控制从 `get_send_pri_en()` 改成 `get_send_pri_mode_en()`。
4. 修改 `issue_queue_scheduler.sv`，拆分 `compare_pri` 与 `use_global_pri`，并接入本拍随机 `global_send_pri` 和可选随机 pipe 数。
5. 修改 `default.cfg`，补充默认参数。
6. 更新 flow 文档和网页文档。
7. 运行 compile/smoke，重点覆盖：
   - 非 send_pri 模式。
   - non-global send_pri 默认模式。
   - global send_pri 强制模式。
   - pipe 默认非随机模式和 pipe 随机模式。
