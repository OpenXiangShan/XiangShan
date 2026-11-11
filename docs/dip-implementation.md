# 香山 DIP：Dispatch 端逐指令周期栈

基于当前 TEA 的 debug/ChiselDB/Constantin 基础设施实现 ASPLOS 2026 DIP 的 Dispatch 归因规则。论文依据为作者稿 `dip-asplos26-aam.pdf` 的 Figure 3、Section 3.1–3.2。

采集器按固定周期间隔采样并分类槽位，只跟踪被采中的 ROB slot 和前端后继。数据库保存采样点及必要的终结/重归因事件，不导出逐周期 Dispatch/ROB 生命周期。尚未实现论文的 CSR 和采样中断；没有将 BOOM 的 49 B 开销作为香山开销。

## 归因规则

Dispatch 宽度为 `RenameWidth`，单位为 uop；每个样本中每个槽位贡献 `1/RenameWidth` 个采样周期。部分发射遗留的物理 lane 空洞不减少可用容量。

- **Base**：每条实际发射的 uop 占一个槽位，归给自己的 PC。融合按 leader uop 的 PC 归因。
- **Stall**：剩余槽位归给当前最老、尚未发射的 uop，保留 ROB、各类 IQ、LQ/SQ、序列化、LSQ 恢复和分派带宽的多热原因。Rename 无法供给时记录 freelist/LSQ 原因，等待下一条进入 Dispatch 的 uop。
- **FrontEnd**：无阻塞目标的空槽等待下一条进入 Dispatch 的 uop，并从其 TEA 前端事件区分 ICache miss、ITLB miss 和 bubble。
- **Misspeculation**：以上三类均为暂定；目标被 flush 清除时，重归到造成 flush 的 PC，区分误预测、异常、内存顺序、其他 flush 和中断。无目标的前端等待也会被 flush 覆盖。恢复原因保持至新的 uop 进入 Dispatch，并排除 redirect 清空寄存器之前仍可见的旧 valid。若造成误预测的年轻分支随后又被更老 redirect 清除，槽位会继续重归到更老原因；恢复期间原因已退休的样本可直接定案。

正确路径目标在对应 ROB slot 真实退休时定案。生命周期身份包含 ROB 索引、环绕 flag 和 Former/Latter；同一压缩 slot 内多成员/多 uop 共享退休命运，保留各自 PC。自身清除的指令不必再次在原 slot 退休；对应或更后的 slot 退休可证明更老上下文已稳定。窗口结束仍未定案或未获得后继的槽位记为 `unresolved_slots`，不静默丢弃、不当作零。

原因位 `ROB` 对应 `!enqRob.canAcceptForDispatch`，表示 ROB 侧准入失败，可能包含 RAB、VTypeBuffer 或序列化限制，不等同于纯物理容量满。`LQ/SQ` 来自 Dispatch 的 LSQ 边界阻塞，Rename 阶段的 LSQ 预分配失败合并为 `LSQ`；`DispatchPolicy` 表示发射带宽/分派策略限制。

`resolved_slots + unresolved_slots = sampled_cycles * dispatch_width`。PICS 的权重单位是采样周期；没有将周期性样本当作全量周期数。多热原因组合保持为一个互斥的 PICS 项，避免重复累计。

## 构建与使用

在仓库根目录构建 GSIM：

```bash
NOOP_HOME="$PWD" make gsim GSIM=1 CONFIG=DefaultConfig NUM_CORES=1 \
  EMU_THREADS=1 WITH_TEA=1 WITH_DIP=1 WITH_CHISELDB=1 WITH_CONSTANTIN=1 -j48
```

Verilator 使用 `make emu`，其余功能开关相同。需要 DRAMsim3 时增加 `WITH_DRAMSIM3=1 DRAMSIM3_HOME=...`。

创建运行配置，例如 `build/dip.cst`：

```text
enableDip0 1
dipSamplePeriod0 1
enableTea0 1
teaSamplePeriod0 1
```

`enableDip` 独立于 `enableTea`；DIP 使用 `WITH_DIP=1`，TEA 使用 `WITH_TEA=1`。DIP-only 构建不生成 TEA 成员 tracker、selector 或周期统计；两者共用前端 PSV sideband。周期为 1 可作逐周期验证，默认周期为 256。关闭采样后，已采目标继续跟踪并输出必要事件，以便已有样本定案。

CI 中设置 `enable_dip=true`、`cst_file=scripts/ci/dip-period1000.cst`，保持 `enable_tea`、`enable_db`、`enable_rolling` 为 false，即只导出 `Dip_0` 和 `DipDelayedFinal_0`。同时选择 `enable_tea` 时导出 DIP 和四张 TEA 表，并在 CST 中启用两种采样器；选择 `enable_db` 或 `enable_rolling` 时导出全部可用表。

```bash
build/emu -s 0 -i ready-to-run/coremark-2-iteration.bin \
  --diff ready-to-run/riscv64-nemu-interpreter-so --max-instr 5000 \
  --cst-file build/dip.cst --dump-select-db 'Dip_0 DipDelayedFinal_0 Tea_0 TeaDelayedFinal_0' \
  --db-path build/dip.db
python3 scripts/dip_analyze.py build/dip.db --output build/dip-profile
```

模拟器产物路径以实际构建输出为准。分析必须同时保留两张 DIP 表：

| 表 | 内容 |
| --- | --- |
| `Dip_0` | 仅在采样点记录周期、该周期之前的累计退休数、当周期退休数，以及各槽位的状态、原因、PC 和目标身份 |
| `DipDelayedFinal_0` | 被采目标的提交、相关 flush、前端后继绑定，以及确认自身清除原因稳定的提交水位 |

原生采集器为每个 ROB Former/Latter slot 保存是否被引用、环绕 flag、首次被引用的周期 generation 和是否作为恢复原因。未被采样引用的分派/提交不产生记录。前端等待共享一个 generation；遇到下一条 Dispatch 指令时，仅输出一次绑定。同一目标可被任意多个周期样本引用，不占用逐样本硬件队列。flush 只在影响已采工作时输出；离线将其重归到原因目标，保留后续更老 flush 再次重归的能力。generation 避免 ROB 索引复用与旧样本串联。

DIP 从仿真开始采集，预热期间也维护目标和恢复历史。EMU 在原有预热 dump/clean 周期结束后删除 TEA/DIP 表中的预热记录；预热未完成就退出时，这些表导出为空。正式窗口保留跨边界目标的历史，`CYCLE`、采样相位和 `TOTALRETIRED` 不在边界重置。数据库已排除预热，分析时保留默认 `--warmup-instr 0`；该参数仍可按仿真开始后的累计退休数进一步筛选。`--end-cycle C` 设定排他的采样终点；更晚的终结事件仍参与归因。输出 `pics.csv` 按 PC、最终状态和多热原因组合聚合；`summary.json` 给出采样数、重归因数、各状态周期和未定案槽位。

稀疏日志不再提供任意非采样周期的退休前缀。`sampled_span_*` 只描述首尾入选采样点之间（含首尾周期）的真实退休数、周期数和 IPC，不将其冒充整个测量 ROI。整段性能数据使用模拟器原生统计；原生最终摘要存在流水延迟，分析时仍以其 `cycleCnt` 设置排他采样终点。
