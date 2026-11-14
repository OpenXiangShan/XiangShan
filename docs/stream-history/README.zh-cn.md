# Stream PC 历史过滤实验实现

基线提交：`a253c1163982ae0b485b3a2ed9b61253d980d837`。三个分支的
`StreamHistoryParams.policy` 是编译期选择，运行时只实例化一套表。

| 方案 | 长度学习 | 开启过滤 | 真实 demand 超长 | 生产状态预算 |
|---|---|---|---|---|
| S1 / B114 | 完整段长度取最大；新段长处于旧预测的上半区才增加置信度 | L1 C≥3、预测 2–32 region；L2/L3 C≥1 | 历史 C 清零，当前段继续统计，结束后重新学 | 8139 bit / 1018 B |
| S2 / B036 | 增长立即提交；连续两次有效完整段给出同一个短长度才缩短 | 三层 C≥1 | 当前段放行，旧历史保留，段结束时更新 | 7691 bit / 962 B |
| S3 / B052 | 保存最近出现的两个不同完整长度，取大值预测 | 三层 C≥1 | 当前段放行，段结束时更新 | 4017 bit / 503 B |

S3 重复出现同一短长度不会挤掉另一个长长度。S2 的第二长度是待确认短长度；
中间的另一个有效长度会替换/取消该确认，截断样本不提交。C 是段长证据强度，
不是预取准确率。S1 的长度小于旧预测一半时 C 回到 1，预测仍取较大值。

L3 按本次 RTL 实现要求复用 L2 的开启规则，但按 L3 自己的真实目标地址独立过滤。
此前 42 点探索的指标只覆盖 L1/L2，不能据此声称 L3 的收益已验证。

## 接口、时序和长度统计

- 所有 `io.train_req.fire` 更新已有 PC；miss 或 Stream-prefetch hit 才允许为新 PC 分配。
  原有 Stream 的有效训练筛选、bitmap、cnt、active、深度和宽度均保留。本版不做提前激活。
- 区域大小 1024 B；一项只保存完整前沿 region、有限跨度、首块 region 内偏移、方向。
  相邻前进将跨度加一；同 region、前沿后退一步均不加。跨度是首尾距离，段长是跨度加一。
  更大跳跃结束旧段并开始新段，不把偶发一步回访认作结束。该规则是硬件近似，非程序循环真值。
- 首次分配时左截断；替换丢弃当前段；未知方向、跨度溢出和不能表示的向上偶数取整结果不学习。
  起始方向由相对首块至少 2 block 的位移确认。反向使用相同逻辑。
- 当前段起点由 `frontier ± span` 重建；只在结束时更新历史，当前段 1/2/3 region 的下界不覆盖历史。
- 门控在 native S2，使用其真正生成的目标 region/bitmap。该版本 bitmap 本身限制在一个 region，
  因而 region 边界过滤整组不会产生“同 bitmap 部分越界”。L1/L2/L3 各自有效流水线独立。
- S0 更新在时钟沿提交；S2 读取已经提交的 live 表。保存两级 slot/epoch/progress/valid 快照。
  对应项被替换、换段、flush 时失配即放行，不使用同槽的新段历史；同一周期 S0 的写入不会提前影响 S2。
  2 bit epoch 对两级、一周期至多一次更新的流水线足够，不能随意扩展流水线延迟而不复核。
- Stream PC 显式接入和 Stride 相同的 OOO PC 选择路径。历史跨 context 禁止共享：比较 satp/vsatp/hgatp
  的 mode、ASID/VMID、PPN，加数据虚拟化/特权；响应 sfence/相关 changed 信号。清除 history 和训练队列，
  排空队列期间历史放行。关闭 Stream 的原控制器 flush 也清历史。

## 存储与面积口径

每项：tag、40-bit frontier、span、4-bit initial offset、2-bit direction、valid/censored/overflow、
2-bit epoch、history length、history direction、2-bit C；S2/S3 各加第二长度。

| 项目 | S1 | S2 | S3 |
|---|---:|---:|---:|
| 项数 × 每项位数 | 64 × 115 | 64 × 108 | 32 × 102 |
| 替换 PLRU | 48 | 48 | 24 |
| 两级 slot/epoch/valid/progress | 20 | 20 | 18 |
| context 比较寄存器 | 195 | 195 | 195 |
| PC 传递保守预算 | 500 | 500 | 500 |
| 控制余量（含 flush 和 L3 独立有效寄存器） | 16 | 16 | 16 |

`storageBits` 和实际 Bundle 位宽在 elaboration 时检查。以上是生产状态预算，**不是综合面积**：
四路 tag 比较、mux、region 加减/比较，以及读写端口还有组合逻辑成本；尚未做综合/时序收敛。
S2/S3 是 24-bit 部分 PC tag，存在别名风险。S1 保留完整 tag。

所有新增诊断寄存器只在 `enablePerfPrint` 时存在：64-bit XSPerf 计数器、每层 8-region 的过滤地址
观察表、每个 MSHR 的来源/首次 demand 标记。关闭性能打印后不会进入上述生产状态。不要用带这些
诊断状态的仿真网表宣称完整面积小于 1 KB。

## 计数器和 weight

所有 `history_lN_*_blocks` 都是候选 bitmap block 事件，可能重复，既不是成功回填 F，也不是越界真值。

| 计数器 | 含义/用途 |
|---|---|
| `history_lN_candidate/blocked/allowed_blocks` | 过滤前候选、历史拦截、历史放行；三者可作守恒检查 |
| `history_lN_applied_groups` | 真正启用历史判断的候选组 |
| `history_lN_c0..3_candidate/blocked_blocks` | 分置信度观察干预覆盖和强度 |
| `history_lN_bypass_*` | identity、方向、progress、overflow、overrun、confidence、length 条件未满足；原因可重叠 |
| `history_train/hit/allocate/admission_bypass/capacity_censor` | PC 访问、准入、容量替换压力 |
| `history_complete/censored_close/unknown_direction_close/unrepresentable_close` | 可学习的完成段及排除原因；排除原因可重叠 |
| `history_length_exact/under/over/*_regions` | 仍保留有效同方向历史的完整段与量化后真实段长对比；不是全部段误差率 |
| `history_rounding_extra_regions` | 向上偶数取整引入的 region 数；和预测误差分开 |
| `history_demand_overrun/direction_mismatch` | 当前实际访问的矛盾事件。S2/S3 可能一段多次出现，不能当作段数 |
| `history_short_pending/short_commit/grow_commit` | S2 的待确认、缩短提交、增长提交 |
| `history_distinct_insert/confirm` | S3 双不同长度的更新/确认 |
| `history_lN_shadow_demand_blocks` | 有限观察表里被过滤的虚拟 block 后来被 accepted demand 访问，观察到一次即清除 |
| `history_lN_shadow_eligible_demand_blocks` | 上述 demand 同时满足 native 训练筛选；包括 miss 和 Stream hit，**不等于 miss 数** |
| `history_lN_shadow_unobserved_eviction/flush_blocks` | 有限观察窗删去而未观察到 demand 的块，属于截断，**不等于无用** |
| `history_stream_primary` | L1 MSHR 最初由 Stream primary 请求获得的事务数 A |
| `history_stream_refill_success/error` | 保留原 Stream 来源，最后一个 data beat 回填成功 F 或出错的事务数；不把 A 当 F |
| `history_stream_mshr_cycles/wait_data_cycles` | Stream 原始事务的 entry-cycle / 等待 data 的 entry-cycle，跨 entry 求和 |
| `history_stream_first_demand_merge/demand_wait_cycles/refill_with_demand` | 首次 demand 合并、合并后等数据的时间、带 demand 的成功回填 |
| `history_stream_a_stall_cycles` | 原 Stream MSHR 在 A 通道被阻塞的 entry-cycle |

继续使用已有 StreamMonitor 的 Sent、Nack、HitInCache、HitInMSHR、Late、Useless，以及
MissQueue 的 demand rejection、no_free_entry，队列 TLB/PMP/page-fault 等计数器。
Nack 不等于物理 MSHR 满；候选过滤不等于真正节省同量 F，TLB 检查和队列合并仍会改变它。
不同采样窗口的 hit/use 事件与该窗口 F 不一定属于同一批块，不能据此构造精确 cohort H/F。
精确边界、误拦与同一 F cohort 的 H/F 仍须 trace/lifetime 关联；有限 shadow 不替代它。

可以先平等比较 `减少的越界F - w × 段内有用F误拦`，w∈{1,2,3,5}，全部除以同一原 F；
这些是假设评分，不是 IPC 预测。随后做相同指令窗口/相同配置的策略开启与关闭 A/B，比较 cycles、
实际 F、demand stall/MLP、late、无用块和上述资源时间，按程序组留出验证后才能拟合 weight。
单份计数器无法识别“被拦请求若放行会节省多少周期”的因果关系。不要以候选节省量或 shadow 命中
直接拟合宣称准确的性能收益。`StreamHistoryBaselineConfig` 提供禁用历史的匹配对照配置。

## 测试与复现

- `scripts/stream-history/test.sh`：用冻结 V19 Python 模型生成定向/随机向量，逐周期对比三个层级门控
  及有效项状态；并将原生 Stream 与本方案放在同一 test top，对比握手、位图与目标，确认只抑制候选。
  每个分支按默认 policy 编译。包括正反向、首段截断、近似证据、确认缩短、双不同长度、重复/回退、
  溢出/取整、PC 容量冲突、ineligible 准入/更新、flush 和 S0/S2 重叠。
- `scripts/stream-history/build-emu.sh`：DefaultConfig，1 个模拟 hart、4 个 host emulation threads，
  构建前输出资源检查。应先核实可用内存至少 128 GB，并按环境调整 `-j`；不启用 ChiselDB。
- `AM_HOME=/path/to/nexus-am make -C tests/stream-history-am ARCH=riscv64-xs` 编译自校验小程序。
  用新生成的 `build/emu -i .../stream-history-smoke-riscv64-xs.bin --diff ready-to-run/riscv64-nemu-interpreter-so`
  执行。必须同时检查 GOOD TRAP/校验和及 `history_*` 计数器确实活动，不能只看退出状态。
- `AM_HOME=/path/to/nexus-am scripts/stream-history/run-am.sh 32,33,34,35` 可完成编译、核对四个物理核、运行及计数器守恒/覆盖检查；CPU ID 按现场资源替换。
- 本次用户目录放置于 `../am/stream-history`；仓库内 `tests/stream-history-am` 是相同可复现源文件。
  所有临时文件都在各 worktree 的 `build/history-scratch`，不使用 `/tmp`。

`reference/compact.py` 与 `common.py` 为先前探索中已交叉验证的 V19 模型原样副本；它们支持
比本次三个方案更多的探索配置。测试只加载 `selection.json` 的 S1/S2/S3；L3 调用 L2 规则。
无效项的 payload 不做比较（未分配寄存器没有算法语义），valid 和所有有效项字段均比较。

本实现与报告由 Codex / GPT-6 辅助生成；需要人工 review。功能 smoke 不证明 SPEC 性能收益，
本阶段没有重跑 42 CPT 或进行策略性能调优。
