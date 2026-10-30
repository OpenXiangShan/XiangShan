# TEA 可视化分析

架构成员跟踪、四状态归因、采样数据库及 PICS 权重公式见
[TEA 实现机制](../docs/tea-implementation.md)。

`scripts/tea_visualize.py` 以只读方式读取新实现的 `Tea_0` 和
`TeaDelayedFinal_0`，生成 PNG、PDF、热点 CSV、时间线 CSV 和汇总 JSON。
依赖 Python 3、Matplotlib、NumPy。本脚本只接受当前逐指令 schema。

仅分析 TEA 时，仿真可传 `--dump-select-db 'Tea_0 TeaDelayedFinal_0'`，
保留采样及延迟终结表，避免同时记录逐次写回、访存和总线等大量数据库日志。
性能计数器仍从 stdout/stderr 获取；需要其他数据库证据时再增加对应表名。

```bash
python3 scripts/tea_visualize.py reports/tea-members-on/tea.db \
  --labels coremark --output reports/tea-members-plots
```

可以输入多个数据库；`--labels`、`--elf` 按输入顺序逐一对应。
`--elf` 可省略，也可用 `-` 跳过某个数据库的反汇编。
默认反汇编工具为 `riscv64-unknown-linux-gnu-objdump`，可通过
`--objdump` 指定路径。`--top` 控制图中热点数，CSV 始终包含所有 PC。
`--bins` 控制时间线分箱数。
`hotspot_states.csv` 保留所有 PC × 状态 × PSV 组合，即使 `--top` 只显示少量热点。
其中 `sample_weight` 是该组合的采样权重，`selected_weight_pct` 的分母是本次输入
过滤后全部权重，`pc_weight_pct` 的分母是该 PC 的全部状态/PSV 权重。这样可区分
高频 Computing 指令、Stalled 归因目标和 Flushed 恢复原因；不同状态不会合并后
被解释成同一种瓶颈。Stalled 仍遵循下述最老未完成成员口径，不是依赖链证明。

`--cycle-range START END` 过滤原始采样边界的半开区间；延迟终结记录仍从
完整数据库关联，因此窗口外退休的指令不会被误报为 unresolved。
对默认 SPEC 20M 预热 / 40M 总指令 CI，可用 `--spec-log-dir DIR ...`，
按数据库顺序提供含 `simulator_out.txt`、`simulator_err.txt` 的目录。脚本校验
成功结束、两次 ROB 计数器及重置边界，再为每个数据库选取独立的测量区间；
要求预热计数在 [20M, 20.1M)、测量计数在 [19.9M, 20.1M)；错误窗口或
测量区间没有选中 TEA 样本的输入会被拒绝。区间、指令数和周期数写入汇总，
不能与 `--cycle-range` 同用。PC 排除仍由 `--spec-checkpoint` 单独控制。
日志含 `teaSamplePeriod0` 时，还会报告原始采样行数、期望边界数、重复、偏离周期、
内部缺样和端点缺样数量。此检查按从 cycle 0 开始、持续开启的固定周期采样计算，
不改变归因权重，也不自动补齐或截断端点；端点差异需结合仿真退出和最终 PERF dump 检查。

所有输入均有已验证的 SPEC 日志和采样周期时，另生成 `sampled_costs.png/pdf/csv`，
以 `sample_weight × sample_period × 1000 / measured_instructions` 表示每千条退休指令的
采样周期当量，方便对照不同 CPI 的运行。四状态与 Excluded PC 单列，保留取消、
未解析和未知归因；事件 bit 仍可重叠。图中的黑色叉号来自实际计数器总 CPI，
采样柱状图不缩放到该总量，不补齐缺样；覆盖差异需查看 summary 的采样质量字段。
`hotspot_states.csv` 同时增加 `sampled_cycles_per_ki`，而原始权重和百分比保持不变。
这些值是采样归因估计，不能解释为可消除的停顿周期。

对同一切片、相同指令窗口的基线与候选运行，可增加 `--compare-first`，将第一个
输入作为基线，其余输入逐个与它比较。需要至少两个数据库和各自的 `--spec-log-dir`，
且每份日志包含采样周期。`sampled_cost_deltas.csv` 输出状态、重叠事件、未完成归因
及实际总周期/千指令的候选减基线差值；其中 `kind=measured` 的总量直接来自 ROB
计数器，状态、事件和未完成归因是采样当量。`hotspot_deltas.csv` 按 PC × 状态 × PSV 的采样当量差值
绝对值排序，保留只在一侧出现的热点、Unknown 和未完成归因。两侧分别用自己的
指令数、采样周期换算，不用百分比相减，也不把缺样补到实测 CPI。脚本不证明两次
运行使用相同切片或相同配置，调用者必须核对二进制来源、切片、种子和指令窗口；
这些差值不能直接换算为 benchmark/SPEC 分数。
`kind=sampling` 的 `Measured minus captured` 单列实际总周期与采样状态总量
（含 Excluded PC）的差，保留其正负号。状态总量加该项应与实际总量逐侧及差分守恒；
该差包括采样端点误差和丢样，不解释为硬件停顿或可回收周期。

```bash
python3 scripts/tea_visualize.py /path/to/checkpoint/simulator.db \
  --spec-log-dir /path/to/checkpoint --spec-checkpoint --output reports/tea-measured
```

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

事件热图和时间线使用与 PICS 相同的采样权重。`summary` 与 `timeline.csv` 同时
保留原始次数和新增的 `*_weight`：Computing 中八个成员各带某事件，与一个
Stalled 目标带该事件，均贡献总权重 1；不会因提交宽度而让前者放大八倍。
`state_*_weight` 和 `cancelled_weight` / `unresolved_weight` / `unknown_weight`
提供同口径的状态与质量分解，`excluded_weight` 单列 PC 过滤排除的权重。
事件 bit 可重叠，不能求和当作停顿周期分区，也不能把签名权重直接当作可消除周期。
ST_TLB 等执行期签名会在指令存活期间累积；Stalled 关联的是目标最终签名。
因此带 ST_TLB 的采样权重不能直接当作采样当时的 DTLB miss 或页表遍历延迟，
需要对照同一测量窗口的 replay、PTW 和 Top-down 计数器判断。

图形形式参考 *TEA: Time-Proportional Event Analysis*（ISCA 2023）的 PICS。
每个输入生成 `pics`、`hotspots.csv`、`hotspot_states.csv`、`timeline` 和 `timeline.csv`；输出根目录
生成 `comparison`、`summary.csv` 和 `summary.json`。

新增的低开销模式与四表导出见[实现文档第 11 节](../docs/tea-implementation.md#11-低开销周期统计与队首上下文)。
`head_context.csv` 随样本的 ROI/PC 过滤；`cycle_summary.json` 与 `no_commit_intervals.csv`
独立报告全部采集窗口，不随这些过滤变化。精确计数只覆盖 JSON 中 observed_end 之前的启用周期，
collection_closed=false 时不得声称覆盖完整运行。旧数据库仍支持原来的图表分析。
