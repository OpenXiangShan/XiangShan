# plus.sv 源码分析

本文档对应源码：

- `mem_ut/ver/ut/memblock/env/plus.sv`

## 1. 文件定位与使用场景

这是 memblock UT 的命令行 plusarg 解析入口。它定义 `plus::MEMBLOCK_*` 静态字段，并在 `plus` 对象构造时从命令行读取 `+MEMBLOCK_*=...`。可以把它理解成“原始参数仓库”：它知道 Makefile cfg 或用户 `plus_arg` 提供了什么值，但不负责 dispatch 框架内部怎么使用这些值。

输入是仿真命令行 plusarg，包括 Makefile 通过 `cfg=<cfg_name>` 从
`${plus_file}/${cfg}.cfg` 展开出的 `+MEMBLOCK_*=...` 参数和用户追加的 `plus_arg`。Makefile
展开 cfg 时会先过滤掉被用户 `plus_arg` 覆盖的同名 key，保证用户命令行显式指定值优先。
输出是 `plus::MEMBLOCK_*`
静态变量。它本身不做 dispatch 主表生成、issue 调度或 L2TLB 回复；这些动作都在后续 helper
中完成。

与 `seq_csr_common` 的关系：

- `plus.sv` 负责解析和保存原始命令行参数。
- `seq_csr_common` 负责把 `plus::MEMBLOCK_*` 拷贝成 dispatch 框架快照，做合法性检查、clamp，并通过 getter 提供给 helper。
- testcase 不再直接写 `plus::MEMBLOCK_*` 默认值；需要 testcase preset 时，由运行命令或回归
  ini 显式指定 `cfg=<cfg_name>`。
- 后续 helper 不直接读 `plus::MEMBLOCK_*`，而是读 `seq_csr_common::get_*()`，这样所有 helper 看到的是同一份已经检查过的配置。

解析实现上，`plus.sv` 统一通过 `$value$plusargs({name, "=%s"}, value)` 读取
`+KEY=VALUE`。这样不依赖 SV testcase 自己读 cfg 文件，也不依赖仿真器把 cfg 文件作为
UVM cmdline processor 的参数来源。整数、bit 和 64-bit hex 再由 `load_int/load_bit/load_hex64`
做格式检查。

## 2. 主表地址复用与 ROB 起点参数

本轮新增主表生成期 recent-window 地址复用参数，用于替代旧的主表生成后全表扫描注入。`plus.sv` 只保存原始 plusarg，真正读取和 clamp 在 `seq_csr_common.sv` 中完成。

| 参数 | 默认值 | 含义 |
|---|---|---|
| `MEMBLOCK_ADDR_REUSE_EN_1_WT` | `1` | 地址复用 enable=1 的权重。和 `EN_0_WT=19` 配合，默认每条随机 transaction 约 5% 概率尝试复用。 |
| `MEMBLOCK_ADDR_REUSE_EN_0_WT` | `19` | 地址复用 enable=0 的权重。 |
| `MEMBLOCK_ADDR_REUSE_LOAD_AFTER_STORE_WT` | `1` | 当前项改成 load，并尝试复用窗口内历史 store 地址的场景权重。 |
| `MEMBLOCK_ADDR_REUSE_LOAD_AFTER_LOAD_WT` | `0` | 当前项改成 load，并尝试复用窗口内历史 load 地址的场景权重；默认关闭。 |
| `MEMBLOCK_ADDR_REUSE_STORE_AFTER_LOAD_WT` | `1` | 当前项改成 store，并尝试复用窗口内历史 load 地址的场景权重。 |
| `MEMBLOCK_ADDR_REUSE_STORE_AFTER_STORE_WT` | `0` | 当前项改成 store，并尝试复用窗口内历史 store 地址的场景权重；默认关闭。 |
| `MEMBLOCK_ADDR_REF_WINDOW_FIXED` | `0` | 固定 uid 距离窗口。为 0 时改用 small/medium/large 权重选择窗口。 |
| `MEMBLOCK_ADDR_REF_WINDOW_SMALL_WEIGHT` | `0` | small 窗口权重。 |
| `MEMBLOCK_ADDR_REF_WINDOW_MEDIUM_WEIGHT` | `0` | medium 窗口权重。 |
| `MEMBLOCK_ADDR_REF_WINDOW_LARGE_WEIGHT` | `1` | large 窗口权重。默认选择较大窗口，上限由 `min(MEMBLOCK_LQ_SIZE, MEMBLOCK_SQ_SIZE)` clamp。 |
| `MEMBLOCK_ROB_START_FIXED_EN` | `1` | ROB 起点固定模式开关。默认打开，保持从 value 0 开始。 |
| `MEMBLOCK_ROB_START_FIXED_VALUE` | `0` | 固定模式下 uid0 的 `robIdx.value`。 |
| `MEMBLOCK_ROB_START_ZERO_WT` | `1` | 随机模式下选择 zero 起点的权重。 |
| `MEMBLOCK_ROB_START_MID_WT` | `0` | 随机模式下选择 ROB 中间区间起点的权重。 |
| `MEMBLOCK_ROB_START_NEAR_WRAP_WT` | `0` | 随机模式下选择接近 ROB wrap 区间起点的权重。 |

设计注意：

- 初始 `robIdx.flag` 第一版固定为 0，只随机 `value`；后续 wrap 仍由 `rob_order_util::rob_advance()` 翻转。
- 手动主表导入不读这些地址复用参数，不会隐式改写 directed transaction。
- 旧 `MEMBLOCK_LD_TO_ST_ADDR_REUSE_WT`、`MEMBLOCK_ST_TO_LD_ADDR_REUSE_WT`、`MEMBLOCK_MANUAL_ADDR_REUSE_EN` 已删除，不保留兼容入口。

## 3. TLB 与 recovery 闭环参数

本轮还补了 TLB entry 生成参数，用于控制 `memblock_tlb_entry.sv` 的 `level` 和 PTE 修正模式：

| 参数 | 默认值 | 含义 |
|---|---|---|
| `MEMBLOCK_TLB_LEVEL_MODE` | `0` | `level` 生成模式：`0=fixed`、`1=random`、`2=derived`。 |
| `MEMBLOCK_TLB_LEVEL_FIXED_VALUE` | `0` | fixed 模式下返回的 `level`。 |
| `MEMBLOCK_TLB_LEVEL_RANDOM_LOW` | `0` | random 模式下 `level` 下界。 |
| `MEMBLOCK_TLB_LEVEL_RANDOM_HIGH` | `2` | random 模式下 `level` 上界。 |
| `MEMBLOCK_TLB_PTE_MODE` | `0` | PTE 修正模式：`0=LEGAL`、`1=MIXED`、`2=EXCEPTION_BIASED`。 |

| 参数 | 默认值 | 含义 |
|---|---|---|
| `MEMBLOCK_REDIRECT_SEQ_EN` | `1` | 是否启用真实 `io.redirect` 回灌 sequence。关闭时 redirect responder 只发 idle；若仍出现 redirect/memoryViolation event，recovery handler 会 fatal，避免 payload 无法消费。 |
| `MEMBLOCK_REDIRECT_DRIVE_TIMEOUT` | `1000` | redirect sequence 空闲等待 active payload 的本地 idle loop timeout。 |
| `MEMBLOCK_REDIRECT_FREEZE_TIMEOUT` | `1000` | freeze 后等待 redirect drive/apply 的 service-cycle timeout。 |
| `MEMBLOCK_FLUSHSB_SEQ_EN` | `0` | 是否允许 directed `flushSb/sbIsEmpty` 机制。该开关不自动发 pulse。 |
| `MEMBLOCK_FLUSHSB_REQUEST_CYCLE` | `0` | 非 0 时，commit sequence 在 0-based cycle index 到达该值后调用 `request_flushsb()`，发一个 directed `flushSb` pulse。 |
| `MEMBLOCK_FLUSHSB_TIMEOUT` | `1000` | flushSb 发出后等待 `sbIsEmpty=1` 的 service-cycle timeout。 |
| `MEMBLOCK_REPLAY_WAIT_PTW_EN` | `0` | 是否启用 PTW-back replay 等待队列。默认关闭，replay 立即重新入队。 |
| `MEMBLOCK_REPLAY_WAIT_PTW_TIMEOUT` | `1000` | PTW-back replay 等待释放的 service-cycle timeout。 |

`MEMBLOCK_FLUSHSB_SEQ_EN=1` 只是授权 directed 机制。真正发起有两种方式：

- 代码中的 directed flow 显式调用 `common_data_transaction::request_flushsb()`。
- 命令行或 testcase 设置 `MEMBLOCK_FLUSHSB_REQUEST_CYCLE` 为非 0，用最小入口触发一次 pulse。该入口要求 `MEMBLOCK_FLUSHSB_SEQ_EN=1` 且 `MEMBLOCK_LSQCOMMIT_SEQ_EN=1`；等待 request cycle 期间不计入 commit sequence idle-stop，并且 scheduled pending 状态会阻止 real smoke 提前结束。
