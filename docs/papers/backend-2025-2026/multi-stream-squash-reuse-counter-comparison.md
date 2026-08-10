# Squash Log Performance Counter Comparison

本文对比 CoreMark 2 iterations 下 `2 streams x 64 entries` 和
`2 streams x 128 entries` 两种 profiling Squash Log 配置。两次运行均正常到达
`HIT GOOD TRAP`，执行 663,689 条指令、295,134 个周期，IPC 均为 2.248772。

## Counter Comparison

| 计数器                                |   2x64 |  2x128 |           变化 |
| ------------------------------------- | -----: | -----: | -------------: |
| `msr_log_truncated`                 |    397 |     54 |         -86.4% |
| `msr_squashed_inst_not_logged`      | 12,793 |  2,124 |         -83.4% |
| `msr_log_overwritten`               |  1,193 |  1,216 |          +1.9% |
| `msr_static_hit_inst`               | 63,225 | 70,593 | +7,368，+11.7% |
| `msr_completed_static_hit_inst`     | 24,210 | 25,975 |  +1,765，+7.3% |
| `msr_completed_alu_static_hit_inst` | 15,321 | 16,779 |  +1,458，+9.5% |
| `msr_log_with_static_hit`           |  2,000 |  2,001 |             +1 |
| `msr_log_with_completed_static_hit` |  1,923 |  1,924 |             +1 |

## Counter Definitions

### `msr_log_truncated`

统计被单个 stream 深度截断的误预测 redirect 次数。

一次误预测产生的 squash 指令数大于 `MsrEntriesPerStream` 时，该计数器加 1。
因此，`2x64` 配置的判断阈值是 64，`2x128` 配置的判断阈值是 128。
它统计的是 redirect 事件数，而不是被丢弃的指令数。

### `msr_squashed_inst_not_logged`

统计因单个 stream 容量不足而没有写入 Squash Log 的指令总数。每次误预测的
增量为：

```text
max(squashed_instruction_count - entries_per_stream, 0)
```

`2x128` 将未记录指令从 12,793 条降低到 2,124 条，说明 64-entry stream
确实丢失了较多候选。

### `msr_log_overwritten`

每次产生一个非空 squash stream 时，硬件按 round-robin 选择一个 stream 槽位。
如果被选择的槽位仍包含至少一个未匹配的有效 entry，该计数器加 1。

它统计的是发生覆盖的 stream 次数，不统计被覆盖的 entry 数量，也不表示被覆盖的
entry 一定能够复用。`2x128` 的数值略高，是因为更深的 stream 更可能在下一次轮转
覆盖时仍保留尚未消费的 entry。

### `msr_static_hit_inst`

统计新指令进入 ROB 时，按以下条件命中有效 Squash Log entry 的指令总数：

```text
log.valid
&& log.pc == current.pc
&& log.instruction == current.instruction
```

只对一条指令的 `firstUop` 进行匹配。一个 Log entry 命中后立即失效，最多统计一次；
同周期多个 rename lane 也不能重复使用同一个 entry。如果存在多个相同的静态匹配，
选择逻辑优先选取已经完成的 entry。

该计数器不要求旧指令已经执行完成，因此只表示静态路径重新遇到了相同 PC 和指令编码。

### `msr_completed_static_hit_inst`

它是 `msr_static_hit_inst` 的子集，额外要求被选中的旧 Squash Log entry 在被 squash
时已经完成 writeback：

```text
static_hit && logged_entry.completed
```

这是当前 profiling 中最接近“旧执行结果可能被复用”的计数器。`2x128` 下共有
25,975 次命中，相比 `2x64` 增加 1,765 次。

不过它仍未检查源寄存器版本，因此不是最终可安全复用的指令数量。

### `msr_completed_alu_static_hit_inst`

它是 `msr_completed_static_hit_inst` 的进一步子集，要求旧指令的功能单元类型属于 ALU：

```text
static_hit
&& logged_entry.completed
&& logged_entry.is_alu
```

纯整数 ALU 指令没有 load/store 的内存顺序问题，是后续加入 RGID 检查和真实结果复用时
较适合作为第一阶段目标的指令集合。

### `msr_log_with_static_hit`

统计生命周期内至少发生过一次静态命中的 stream 实例数。同一个 stream 即使命中多条
指令也只加 1；stream 槽位被新 redirect 重新分配时，其“已经命中过”状态会清零。

该计数器用于回答“有多少段被 squash 的路径后来至少被重新遇到一次”，而
`msr_static_hit_inst` 回答的是“这些路径一共命中了多少条指令”。

### `msr_log_with_completed_static_hit`

统计生命周期内至少命中过一条已完成旧指令的 stream 实例数。一个 stream 中命中多条
已完成指令仍只加 1。

该计数器用于判断复用机会在不同 stream 之间的分布。`2x64` 和 `2x128` 的结果只相差
1，说明增加深度主要增加了已命中 stream 内部的后续命中数量，没有显著增加能够发生
重汇合的 stream 数量。

## Interpretation

将每个 stream 从 64 项增加到 128 项后，截断事件减少 86.4%，未记录指令减少
83.4%。新增记录的 10,669 条指令中，有 7,368 条后来发生了 PC 和指令编码命中，
说明 64-entry 配置截断了较多有用的静态候选。

stream 级命中数基本不变，而 instruction 级命中明显增加，说明更深的 Log 主要保留了
重汇合位置之后更长的指令序列。与此同时，覆盖次数没有下降，因为 stream 数量仍为 2；
stream 深度和 stream 数量解决的是两个不同的容量限制。

## Accuracy Boundary

这些计数器只用于 profiling，没有真正复用旧结果。静态命中目前没有检查：

- 当前源寄存器 RGID 是否与旧指令相同；
- 旧目的物理寄存器是否仍被保留；
- load/store 的内存版本和顺序是否仍然有效；
- 命中的新指令之后是否最终提交；
- WPB reconvergence 和 stream 内 instruction offset 是否严格对齐。

因此，`msr_completed_static_hit_inst` 和 `msr_completed_alu_static_hit_inst` 应解释为
“已完成错误路径工作的静态命中机会”，不能直接解释为最终可复用数量或 IPC 收益。
加入 source RGID 比较后得到的命中数，才更接近论文定义的安全复用候选。
