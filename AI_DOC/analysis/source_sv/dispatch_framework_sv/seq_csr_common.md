# `seq_csr_common` 源码分析

## 1. 角色

源码：`mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv`

`seq_csr_common` 是公共测试框架参数快照和 getter 集中层。它从 `plus.sv` 读取 testcase runtime 行为
参数，检查参数组合，并把受硬件资源约束的使用量收敛到 `memblock_compile_params.svh` 给出的编译期上限。
它不保存 transaction、主表、状态表、monitor raw event 或 CSR runtime 状态。

## 2. 参数归属

| 类别 | 示例 | 规则 |
|---|---|---|
| 编译期 DUT 结构 | ROB/LQ/SQ width、LSQ slot、load/store enqueue width、issue pipe 数 | 只从 compile macro/localparam 读取，不建立 runtime plus 镜像 |
| runtime testcase 使用量 | `MEMBLOCK_ENQ_PER_CYCLE`、LOAD/STA/STD pipe limit | 可以通过 plus 配置，但不得超过编译期资源 |
| runtime 行为概率 | enqueue 数量权重、op/PTE 权重 | 由 plus 配置并在 snapshot 中校验 |
| runtime 状态 | status table、TLB entry、raw queue | 不属于本类 |

## 3. 初始化流程

```text
seq_csr_common::init/reload_from_plus：
  调用 plus::load_all 读取命令行和 cfg；
  把 runtime 参数保存到 static snapshot；
  调用 check_compile_param_consistency 检查编译期结构；
  调用 apply_runtime_resource_limits 检查或 clamp testcase 使用量；
  最后置 initialized，后续 getter 只读稳定快照。
```

`check_compile_param_consistency()` 检查 V2 关键结构非零，并确认：

- load/store enqueue width 不大于总 slot 数。
- 当前显式六slot字段链要求slot/load/store compile tuple精确为6/6/4；非默认覆盖在激励前fatal。
- `MEMBLOCK_DUT_UOP_IDX_W == $clog2(MEMBLOCK_DUT_MAX_UOP_SIZE+1)`。
- `MAX_LS_ELEM == VLEN/8`。
- `NUM_LS_ELEM_W == $clog2(MAX_LS_ELEM)+1`。
- DUT FuType 宽度可以由内部 FuType 容器无损表示，且各 one-hot bit 不重复、不越界。

## 4. LSQ 每拍数量控制

固定模式：

```text
MEMBLOCK_ENQ_PER_CYCLE_RAND_EN=0：
  get_enq_per_cycle 返回 MEMBLOCK_ENQ_PER_CYCLE；
  固定值必须位于 1..MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM。
```

随机模式使用三类总权重：

| 参数 | 默认值 | 含义 |
|---|---:|---|
| `MEMBLOCK_ENQ_PER_CYCLE_ZERO_WEIGHT` | 0 | 返回 0，主动插入一拍全零 idle |
| `MEMBLOCK_ENQ_PER_CYCLE_MIDDLE_WEIGHT` | -1 | `-1` 为 AUTO，有效值派生为物理 slot 数减 1 |
| `MEMBLOCK_ENQ_PER_CYCLE_MAX_WEIGHT` | 1 | 返回物理最大 slot 数 |

`apply_runtime_resource_limits()` 在随机模式下执行以下检查：

```text
MIDDLE 原始值只能为 -1 或非负数；
AUTO 解析后保存 effective_middle_weight；
使用 longint unsigned 逐项累计三类权重，避免 32-bit 加法溢出；
三类权重不能全为 0；ZERO 可以是唯一非零类别，`1/0/0` 表示合法的 idle-only 配置；
物理 slot 数不大于 1 时，MIDDLE 权重必须为 0。
```

`get_enq_per_cycle()` 使用两阶段 SystemVerilog 内建随机化：

```text
第一阶段 std::randomize(sample_class) dist：
  按 ZERO/MIDDLE/MAX 三类总权重选择类别；
第二阶段：
  ZERO 返回 0；
  MAX 返回编译期物理 slot 数；
  MIDDLE 在 1..MAX-1 内均匀随机一个值；
任一 randomize 失败时 uvm_fatal，不回退到另一种随机算法。
```

V2 默认 effective 权重为 `0/5/1`，因此开启随机模式后仍保持旧 `1..6` 每个值等概率；只有 testcase
显式提高 ZERO 权重时才会产生主动空拍。

## 5. 与 LSQ admission 的关系

`get_enq_per_cycle()` 返回的是本拍公共 slot 目标上限，不保证实际 packet 达到该数量。
`memblock_lsqenq_dispatch_base_sequence::collect_lsq_candidates()` 仍会按以下条件截断：

- 连续 uid 前缀和主表尾部。
- scalar-only `numLsElem=1`。
- load element 不超过编译期 6。
- store element 不超过编译期 4。
- LQ/SQ 实际 free count。

返回 0 时 candidate 函数在读取 uid/pointer/free count 前结束，不修改公共状态；sequence 只发送一拍 idle。

## 6. V2 timeout 边界

`MEMBLOCK_LSQENQ_READY_TIMEOUT` 仍保留在公共 plus 基线中供其它版本/旧入口使用。
`load_from_plus()` 在 V2 也会解析该值并通过 `get_non_negative_int()` 检查非负；
`validate_and_clamp()` 只有在 compile capability 声明存在 accept-response 时才对零值 warning/clamp。
V2 LSQ enqueue sequence 不读取该 getter，也不调用 `wait_lsq_can_accept()`，因为 V2 顶层没有对应
ready/response。
其它真实握手 timeout 的语义不受本次 LSQ V2 适配影响。
