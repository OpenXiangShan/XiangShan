# 香山 DIP：Dispatch 端逐指令周期栈

基于当前 TEA 的 debug/ChiselDB/Constantin 基础设施实现 ASPLOS 2026 DIP 的 Dispatch 归因规则。论文依据为作者稿 `dip-asplos26-aam.pdf` 的 Figure 3、Section 3.1–3.2。

当前是用于 RTL 仿真验证的实现：RTL 按固定周期间隔采样并分类槽位，完整生命周期 trace 供离线 DIT 定案；尚未实现论文的 CSR、采样中断和低存储开销 DIT。没有将 BOOM 的 49 B 开销作为香山开销。

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
NOOP_HOME="$PWD" proxychains -q make gsim GSIM=1 CONFIG=DefaultConfig NUM_CORES=1 \
  EMU_THREADS=1 WITH_TEA=1 WITH_CHISELDB=1 WITH_CONSTANTIN=1 -j48
```

Verilator 使用 `make emu`，其余功能开关相同。需要 DRAMsim3 时增加 `WITH_DRAMSIM3=1 DRAMSIM3_HOME=...`。

创建运行配置，例如 `build/dip.cst`：

```text
enableDip0 1
dipSamplePeriod0 1
enableTea0 1
teaSamplePeriod0 1
```

`enableDip` 独立于 `enableTea`；两者共用 `WITH_TEA=1` 的 elaboration 开关。周期为 1 可作逐周期验证，默认周期为 256。关闭采样后生命周期 trace 继续跟踪，以便已有样本定案。

```bash
build/emu -s 0 -i ready-to-run/coremark-2-iteration.bin \
  --diff ready-to-run/riscv64-nemu-interpreter-so --max-instr 5000 \
  --cst-file build/dip.cst --dump-select-db 'Dip_0 Tea_0 TeaDelayedFinal_0' \
  --db-path build/dip.db
python3 scripts/dip_analyze.py build/dip.db --output build/dip-profile
```

模拟器产物路径以实际构建输出为准。必须保留 `Dip_0` 的完整 trace，不能先按采样周期过滤生命周期记录。可用 `--warmup-instr N` 从 trace 的真实架构退休计数确定预热边界，配合 `--end-cycle C` 设定排他的测量终点，输出同一 ROI 的退休数、周期数和 IPC。终点后的记录仍用于样本定案。模拟器最终摘要经过流水，强制 dump 也会多执行尾部周期，因此与摘要比较时须使用其 `cycleCnt` 作为终点，不能直接累计整个数据库。输出 `pics.csv` 按 PC、最终状态和多热原因组合聚合；`summary.json` 给出采样数、重归因数、各状态周期和未定案槽位。
