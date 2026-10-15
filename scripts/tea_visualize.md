# TEA 可视化分析

架构成员跟踪、四状态归因、采样数据库及 PICS 权重公式见
[TEA 实现机制](../docs/tea-implementation.md)。

`scripts/tea_visualize.py` 以只读方式读取新实现的 `Tea_0` 和
`TeaDelayedFinal_0`，生成 PNG、PDF、热点 CSV、时间线 CSV 和汇总 JSON。
依赖 Python 3、Matplotlib、NumPy。本脚本只接受当前逐指令 schema。

```bash
python3 scripts/tea_visualize.py reports/tea-members-on/tea.db \
  --labels coremark --output reports/tea-members-plots
```

可以输入多个数据库；`--labels`、`--elf` 按输入顺序逐一对应。
`--elf` 可省略，也可用 `-` 跳过某个数据库的反汇编。
默认反汇编工具为 `riscv64-unknown-linux-gnu-objdump`，可通过
`--objdump` 指定路径。`--top` 控制图中热点数，CSV 始终包含所有 PC。
`--bins` 控制时间线分箱数。

`--cycle-range START END` 过滤原始采样边界的半开区间；延迟终结记录仍从
完整数据库关联，因此窗口外退休的指令不会被误报为 unresolved。
`--spec-checkpoint` 排除当前 SPEC archive 的 flash
`[0x10000000, 0x10100000)` 和 restorer `[0x80000000, 0x80000c00)`。
未知 PC 仍保留权重。

归因规则：

- Computing：展开实际退休的所有架构指令，每条获得 `1/N`。PC 过滤在
  分母计算之后执行，不把被过滤指令的权重转移给其他指令。
- Stalled：按 allocationCycle、robIndex、rank 关联采样时锁定的最老未完成成员。
  这是适应 ROB 压缩的扩展口径，不是严格关键路径判定。
- Drained：关联边界之后第一个实际提交的首条架构指令。
- Flushed：使用整个恢复阶段保留的原因 PC/PSV。

Cancelled、Unresolved、Unknown target/cause 在图表、CSV 和总权重中单列，
不会静默丢弃或解释为 PSV=0。`No recorded event` 只表示 PSV 为零，不能据此
认定没有停顿。当前支持八个事件；ST_LLC 未实现。

`VALIDMASK_n` 是按物理提交项分块的成员 mask，避免 SQLite 对 64 位无符号
mask 的精度损失。`CYCLE` 是采样边界，`STAMP` 不用于关联。
`PCVEC_n` 和 `MEMBER_PC` 是按地址转换模式扩展后的 64 位 PC；SQLite 返回的
负整数会在聚合、过滤和 ELF 匹配前恢复为无符号地址。
汇总的 rows 是窗口内全部边界，selected_rows 和事件数经过 PC 过滤；
sample_weight 包含未完成归因。等周期、无丢样时状态比例可用于估计时间占比，
PSV bit 次数仍是指令签名出现次数，不是独立事件发生次数。
lane 计数包含取消、未解析和未知目标各自的一份占位归因；这些占位项不贡献事件 bit。

图形形式参考 *TEA: Time-Proportional Event Analysis*（ISCA 2023）的 PICS。
每个输入生成 `pics`、`hotspots.csv`、`timeline` 和 `timeline.csv`；输出根目录
生成 `comparison`、`summary.csv` 和 `summary.json`。
