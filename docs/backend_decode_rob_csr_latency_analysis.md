# Backend Decode→ROB 与 CSR 延迟优化分析

## 1. 分析范围与结论摘要

- 分支：`kunminghu-v3`
- 基线提交：`ff4720da42bf8a3a6eef996b0a712cf029523578`
- 分析日期：2026-09-06
- 范围：Decode、Rename、Dispatch、ROB 入队/完成/异常，以及 CSR 执行和 ROB→CSR→FTQ 重定向路径。
- 方法：静态阅读 Chisel RTL 与相关提交历史；没有使用综合 STA、门级仿真或 workload profile。因此本文的“省 N 拍”是满足所列条件时的路径收益，不代表整机 IPC 一定按相同比例提升。

当前无停顿、普通标量指令从 Decode 输出到真正写入 ROB，有 3 道显式时序边界：

```text
T0 Decode 输出
   │ PipelineConnect
T1 Rename（同步 RAT 读数据在本拍参与重命名）
   │ PipeGroupConnect
T2 Dispatch（资源、BusyTable、IQ/LSQ、顺序约束判断）
   │ RegNext / RegEnable
T3 ROB enqueue
```

这里最值得优先做的并不是直接合并 Decode/Rename/Dispatch，而是增加窄旁路：

| 优先级 | 候选 | 条件性收益 | 主要理由 |
|---|---|---:|---|
| P0 | ROB-head exception/flush 快速路径 | 1 拍 | 已实现：旁路 ExceptionGen 已仲裁的 `out`，不绕过 oldest-tree |
| P0 | 无 RF 写回 CSR 的 early completion | 最多 3 拍 | CSR wrapper 的 3 拍主要服务提前唤醒；`rfWen=false` 没有消费者要唤醒 |
| P0/P1 | replay-only 提前发 FTQ redirect | 2～3 拍 | replay 目标在 ROB flush 后很早已知，不需要等待 CSR trap target |
| P1 | 普通无异常 WB→ROB-head completion bypass | 1 拍 | 保留原状态写回，仅前递 head 的“已完成”判断 |
| P1 | Dispatch→ROB 紧凑 allocation/payload 拆分 | ROB 可见性 1 拍 | ROB 编号已在 Rename 推测产生，可避免完整宽 payload 穿越反馈路径 |
| P1/P2 | 简单 CSR fast-read 子路径 | 1 拍 | 普通同步 CSR 当前固定经过 `idle→finish`；只优化小范围高频读 |
| P2 | trap target 预计算/缓存或末级合并 | 1 拍 | 当前 trap target 专门加过时序级，必须在 STA 约束下优化 |
| P2 | RAT 地址更早发起并重定时 | 1 拍 | 当前 Decode→Rename 级与同步 RAT 读严格对齐，需结构性前移而非删寄存器 |
| P2 | CSR→Decode 小范围写旁路 | 1 拍（恢复路径） | 避免等待完整 CSR 状态总线的时序寄存器 |
| P2/P3 | CSR hazard scoreboard / 提交态写入 | workload 相关，多拍 | 减少 CSR 串行等待，但架构状态、异常精确性和旁路复杂度高 |

这些收益存在重叠，不能相加。例如 CSR 异常既可能受 wrapper 延迟影响，也可能受 ExceptionGen 和 ROB-head 判断影响；做完一处旁路后，另一处的可见收益会改变。

## 2. 当前 Decode→ROB 路径

### 2.1 Decode→Rename：1 拍，但承担同步 RAT 读延迟

`CtrlBlock.scala:617-630` 使用 `PipelineConnect` 保存 Decode 输出；`PipelineConnect` 的实现是 valid/data 寄存器（`utility/PipelineConnect.scala:100-119`），因此这是明确的一拍边界。

这一拍同时被 RAT 使用：

- Decode 直接用输出 uop 的 `lsrc` 驱动 RAT 地址，并在下游不 ready 时 hold，见 `DecodeStage.scala:268-295`。
- RenameTable 将读地址寄存后读取 `spec_table`，见 `RenameTable.scala:122-157`。
- Rename 在下一拍使用 `intReadPortsData/fpReadPortsData/...` 生成 `psrc`，见 `Rename.scala:190-206, 547-553`。

因此，简单删除 Decode→Rename 寄存器并不能无代价省一拍：同步 RAT 数据不会在同一拍提前出现，还会打破 uop 与 RAT 返回数据的对齐。

### 2.2 Rename→Dispatch：1 拍，当前不适合直接合并

`CtrlBlock.scala:706-721` 用 `PipeGroupConnect` 建立组级流水。其内部保存整组 valid/data，见 `PipeGroupConnect.scala:110-160`。

Rename 本身已有较长逻辑：自由表分配、多个 RAT 写端口、组内 RAW bypass、move elimination、ROB/LSQ 编号和压缩。源码在 `Rename.scala:683-701` 明确把 move elimination 的 `psrc→pdest→后续 psrc` 链标为 critical path。

Dispatch 又在同一拍处理：

- BusyTable 源操作数状态；
- IQ/LSQ 容量与类型选择；
- `waitForward` / `blockBackward` 顺序约束；
- StoreSet/LFST 返回；
- ROB、LSQ 与 IQ 的原子出队条件。

相关集中逻辑见 `Dispatch.scala:700-725, 728-877`。直接合并 Rename 和 Dispatch 会把自由表/RAT bypass、BusyTable、资源选择和 ready 反馈串成更长组合路径，风险大于一拍的潜在收益。

### 2.3 Dispatch→ROB：1 拍，是可研究的结构性候选

`CtrlBlock.scala:727-742` 对 `dispatch.io.enqRob.needAlloc/req.valid/req.bits` 再做一次 `RegNext/RegEnable`，ROB 下一拍才实际入队。

这里有两个有利条件：

1. ROB index 已在 Rename 推测分配，见 `Rename.scala:335-343, 492`；不是等 ROB 的 `resp` 后才产生。
2. ROB 的 `resp` 是由 `allocatePtrVec` 组合产生（`Rob.scala:201-204`），当前 Dispatch 代码没有消费 `io.enqRob.resp`，主要依赖的是 `canAccept/isEmpty`。

但不能把整条 `EnqRobUop` 直接接到 ROB。ROB 入队同时更新 ROB entry、RAB、VTypeBuffer、ExceptionGen、`hasWaitForward/hasBlockBackward` 等状态；而 Dispatch 的 valid 又依赖 ROB、IQ、LSQ 和顺序约束。直接接线容易形成高扇出或 ready/credit 长反馈。

更可行的设计是：

- 在 Dispatch fire 时发送窄 allocation metadata：`valid/robIdx/firstUop/numWB/needFlush/ftqIdx/ftqOffset` 等提交必需字段；
- debug、trace 或其他宽 payload 保持下一拍写入，并用 `payloadReady` 防止不完整 entry 到达 ROB 头；
- 用本地 credit/skid buffer 隔离 ROB `canAccept` 到 Dispatch 的反馈；
- IQ、LSQ、ROB 三方仍必须保持同一 uop 的原子接受语义；redirect 时同时清除 reservation 和晚到 payload。

该方案可以让“ROB allocation 可见性”提前 1 拍，但不保证所有指令都提前提交；ROB 很深时，它更多改善占用反馈和极短依赖测试中的延迟。

## 3. 当前 CSR 执行延迟

### 3.1 当前活跃实现

`FuConfig.scala:312-324` 的 `CsrCfg.fuGen` 实例化 `fu/wrapper/CSR.scala`，后者再实例化 `NewCSR`。`backend/fu/CSR.scala` 是旧实现，不是当前 `CsrCfg` 的主路径。

### 3.2 普通 CSR 的固定流水

对于非 IMSIC 异步访问的普通 CSR，`NewCSR.scala:1081-1148` 的状态机执行：

```text
C0  io.in.fire，state=idle，组合地产生 normalCSRValid/读值/异常信息
C1  state=finish，NewCSR.io.out.valid
C4  CSR wrapper.io.out.valid（在 NewCSR valid 后固定 DelayN 3）
C5  ROB 看到经 CtrlBlock 再寄存一拍的 writeback
```

异步 AIA/IMSIC 访问还会停在 `s_waitIMSIC`，其延迟取决于响应时间。XRET 是例外：wrapper 在 `isXRetReg` 时绕过固定 3 拍，见 `wrapper/CSR.scala:320-348`。

普通 CSR 的 3 拍不能直接改为 0。`wrapper/CSR.scala:317-363` 同时提供：

- `outValidAhead3Cycle := csrModOutValid`；
- 提前的 `rfWen/pdest`；
- 三拍后对齐的 data、exception、flush、robIdx 和最终 WB valid。

`ExeUnit.scala:368-400` 使用这些 ahead 信号做 uncertain wakeup。提交 `8a5c09f05` 也明确是为 CSR/除法器 uncertain wakeup 引入该结构。只缩短 `DelayN` 会使 wakeup、RF 写回、ROB completion 和异常信息错拍。

### 3.3 NewCSR 自身的一拍是时序切分

`NewCSR.scala:1099-1166` 中，普通访问即使组合结果已在 `normalCSRValid` 时形成，也要先从 `s_idle` 进入 `s_finish`，随后才拉高 `io.out.valid`。提交 `7071df62b` 的说明是“add 1 cycle to csr read/write ... to fix timing”，因此这一拍是有意的时序修复，不应全局删除。

### 3.4 CSR 的顺序约束

通用 CSRRW/CSRRS/CSRRC 及立即数形式在 `DecodeUnit.scala:211-217` 默认设置 `noSpec`（即 `waitForward`）和 `blockBack`，会等待前序并阻挡后序。纯读形式在 `DecodeUnit.scala:218-240` 进一步区分：多数普通只读访问可流水，而 fflags/fcsr/vstart/status 和 AIA/interrupt 相关 CSR 仍有更强约束。

`NewCSR/CSROoORead.scala:5-55` 也保存了一份顺序读分类，但在当前提交中只被 Rename import，未发现实际逻辑引用；实施优化时应以 Decode 最终产生的 `waitForward/blockBackward` 为准，并考虑删除或重新接通这份重复配置，避免两处策略漂移。

Dispatch 对这些标志的实际阻塞逻辑在 `Dispatch.scala:738-739, 833-881`，ROB 也维护 `hasWaitForward/hasBlockBackward`。所以 CSR 的多拍损失不只来自 FU 固定流水，也可能来自等待 ROB 清空；后者是流量相关延迟，不能用一个固定拍数概括。

## 4. 推荐优化方案

### 4.1 P0：ROB-head exception/flush 快速路径（已实现，省 1 拍）

现有异常写回路径较深：

1. 所有执行单元 WB 在 `CtrlBlock.scala:134-147` 先统一寄存 1 拍；
2. ExceptionGen 对 WB 分组，各组取 oldest 后寄存，再跨组取 oldest 后寄存，见 `ExceptionGen.scala:97-120`；
3. ROB 使用 ExceptionGen 的持久 `state`，并在 head 判断中对 `commit_w` 额外做两拍保护，见 `Rob.scala:608-623`。

全局 oldest 选择对任意位置的多个并发异常是必要的。首版实现没有从 raw WB 直接旁路，因为那样需要在 ROB 中重新构造多端口 oldest 仲裁；实际选择 `exceptionGen.io.out` 作为安全旁路源。该信号已经完成 WB 分组及跨组 oldest 归并，只比持久 `exceptionGen.io.state` 早一拍。

实现位于 `Rob.scala` 的 ROB-head exception/flush 判断处：

- `exceptionGen.io.out.valid` 且完整 `RobPtr`（value 和 flag）等于当前 `deqPtr` 时才形成 fast candidate；
- 若持久 state 已命中 ROB head，仍由 state 优先，保证原慢路径行为不变；
- 向量 load exception 明确排除 fast path，继续等待 state 与 RAB 的 partial-result/vstart 恢复路径；
- fast 和 state 通过统一的 `deqExceptionData` 选择 exception vector、trigger、single-step、flush/replay/satp、fetch-fault、FTQ 和向量元数据，避免 valid 提前而数据仍来自旧 state；
- 原 state 路径保留两拍 `commit_w` 稳定保护，fast path 使用一拍保护，与 `out` 相对 state 提前一拍的时序对应；
- flush-after 发生时锁存选中数据的 `isVset`，避免 fast redirect 清空 ExceptionGen 后丢失下一拍的 vtype 恢复通知；
- 使用 `XSError` 检查 fast source 的 head/state/vector eligibility、fast/state bundle 一致性，以及 fast flush 只能消费 valid、writeback-complete、`needFlush` 的 ROB head；
- 增加 `exception_head_fast_path` 计数器，用于后续统计动态命中次数。

因此当前实现的确定收益是：对于 state 尚未命中、`out` 已命中 ROB head、且不是向量 load exception 的 exception/flush/replay，ROB 的 `flushOut` 最多提前 **1 拍**。原文估计的第 2 拍需要继续旁路 ExceptionGen 内部 oldest-tree 或 raw WB，这不在本次安全实现范围内。

验证结果（2026-09-06）：

- `mill -i xiangshan.compile`：通过；
- `NOOP_HOME=$PWD make emu -j 64`：通过；
- `./build/emu -i ready-to-run/coremark-2-iteration.bin --diff ready-to-run/riscv64-nemu-interpreter-so`：退出码 0，`HIT GOOD TRAP`，663,687 instructions / 296,042 cycles，IPC 2.241868。

### 4.2 P0：无 RF 写回 CSR 的 early ROB completion（最多省 3 拍）

Decode 最终会将 `rd=x0` 的整数写回关闭，见 `DecodeUnit.scala:1212`。对于普通 CSR access，如果 `rfWen=false`，没有消费者需要 uncertain wakeup，也没有 RF data 必须与三拍预测对齐，但当前仍与有结果 CSR 一样等待 wrapper 的 3 拍。

建议从 `csrModOutValid` 分出 `csrEarlyRobComplete`：

- 首期条件建议为 `isCSRAcc && !rfWenReg && !isXRetReg`，并排除 WFI、ECALL/EBREAK、XRET 等 system op；
- completion 携带 `robIdx` 以及已经确定的 illegal/virtual exception、`flushPipe`、`satpFlush`；
- AIA/IMSIC 可以继续等实际 `csrModOutValid`，而不是在请求时提前完成；
- 原三拍后通道只负责 RF data/普通 WB；early completion 命中的指令必须抑制第二次 ROB `uopNum` 递减；
- redirect kill、调试信息、性能计数和 CSR 写副作用仍沿原路径保持一致。

该优化只旁路 NewCSR 输出后的 3 拍，不删除 NewCSR 为时序加入的 `idle→finish`。对于 `CSRW ..., rd=x0` 一类无数据消费者操作，理论上可让 ROB 最多提前 3 拍看到完成；若后面仍被 head/flush 路径限制，端到端收益会小于 3 拍。

### 4.3 P0/P1：replay-only 快速 FTQ redirect（预计 2～3 拍）

`CtrlBlock.scala:403-425` 已把 ROB flush 到前端写成明确的 T0～T5：

```text
T0 ROB flushOut
T1 读出 flush PC，同时 ROB exception.valid
T2 CSR redirect.valid
T3 CSR exception.valid
T4 CSR trapTarget
T5 FTQ redirect
```

但 replay 并不需要 CSR trap target。`CtrlBlock.scala:410-413` 在 T1 已得到 replay 自身 PC；ROB 也在 `Rob.scala:619, 645-671` 区分 `replayInst`。当前所有 ROB flush 共用固定 T5 出口，使 replay 白等 CSR 链。

建议：

- ROB→CtrlBlock 显式携带 flush reason，避免仅靠 `level/interrupt/satpFlush` 猜测 replay；
- replay 在 T1 取得 `s1_robFlushPc` 后进入独立一拍 skid，目标为原指令 PC；
- 与 BJU/load replay/普通 ROB flush 的优先级保持“最老优先”，并确保前端只接收一次；
- 后端现有 `s1_s3_redirect` 清流水时序不变，只提前前端重取；
- exception/interrupt 仍等待 CSR trap target；satp/fence/普通 `flushPipe` 首期仍走慢路径，因为必须保证 CSR/TLB/状态更新先传播。

保守实现可在 T2 发给 FTQ，相对当前 T5 省 3 拍；若仲裁或接口必须再寄存一级，仍可省约 2 拍。

### 4.4 P1：普通无异常 WB→ROB-head completion bypass（预计 1 拍）

CtrlBlock 当前把 raw WB 统一寄存后再送 ROB（`CtrlBlock.scala:134-147, 799-804`）。ROB 随后更新 entry 的 `uopNum`，提交侧从寄存的 `robDeqGroup` 读取 `commit_w`（`Rob.scala:220-273, 1049-1069`）。因此“最后一个 uop 本拍写回、该 entry 正好在 head”仍可能多等一拍。

可以计算 `effectiveHeadCommitW`：

- raw WB 的 `robIdx == deqPtr`；
- WB 数量正好覆盖 head 剩余 `uopNum`；
- FU 明确不产生 exception/flush/replay/redirect，或这些信息已在同拍确定为 false；
- 无 redirect kill、无 MMIO/向量部分完成等特殊条件。

该信号只前递提交判定，原 WB 寄存和 ROB entry 更新继续执行。首期仅覆盖 ALU 等确定无异常的单 uop 指令，风险显著低于给所有 WB 建立全功能 bypass。

需要特别注意 commit 同拍会驱动 RAB/free-list、trace/difftest 和前端 commit，因此必须验证这些消费者不依赖“ROB entry 已经在上一拍写好”的隐含条件。

### 4.5 P1：Dispatch→ROB allocation/payload 拆分（ROB 可见性省 1 拍）

具体结构建议见 2.3。推荐实施顺序：

1. 先统计 `dispatch fire→rob enqueue` 对 ROB-full 和短程序延迟的实际贡献；
2. 建立 ROB allocation credit，断开 `canAccept` 的长组合反馈；
3. 只提前最小 architectural metadata；
4. 再决定是否值得把完整 payload 同拍送入。

关键断言包括：分配的 `robIdx` 必须等于 ROB 期望指针；每个 Dispatch fire 必须且只能产生一个对应 ROB/RAB/VTB/LSQ 动作；redirect 后不能出现晚到 payload 写入已复用 entry。

### 4.6 P1/P2：简单 CSR fast-read（预计 1 拍）

`NewCSR` 的 `normalCSRValid` 在请求拍已经得到普通读值和权限结果，但全局状态机为了时序统一在下一拍才输出。可以只给一小组高频、低扇入、非 AIA、无写副作用 CSR 建立 fast-read bank，例如经过 profile 证明热点的只读 ID/config CSR。

可选实现：

- 复制少量只读 CSR 到靠近 CSR wrapper 的小寄存器组；
- fast path 在输入拍完成地址匹配和权限判断，下一边界直接进入 wrapper 的三拍结果管线；
- 其他 CSR 和任何写操作继续走原 `NewCSR` 状态机；
- fast/slow 输出必须保持互斥，并共享 redirect kill 与异常编码。

这一项只能省 NewCSR 内部 1 拍，且历史提交已经说明原级用于修复时序，所以是否可行必须由综合结果决定。不要直接让所有 `normalCSRValid` 组合穿到 WB。

### 4.7 P2：trap target 局部预计算或缓存（预计 1 拍）

`NewCSR.scala:1191-1207` 把 `pcFromXtvec` 寄存后再做目标地址故障检查，并用 `trapTargetUpdate` 对齐。提交 `689514dc8` 明确为 `pcFromXtvec→TrapTvalMod` 增加流水级，说明这是已知时序敏感路径。

可研究两个较安全方向：

- 缓存各 privilege mode 下的 `xtvec` base/mode，并在 ROB exception cause 可用时并行计算 vectored offset；CSR 写 xtvec 时更新缓存。
- 保持 NewCSR 内部流水不动，仅在 STA 允许时合并 CtrlBlock 的 T4→T5 最后一级。

必须保留 delegation、debug/NMI、虚拟化模式、vectored interrupt 以及目标地址 IPF/IAF/IGPF 判断。由于状态组合复杂，优先级低于 replay 快速路径。

### 4.8 P2：RAT 预读进一步前移（预计 1 拍，高风险）

当前设计已经做了“在 Decode 输出、fusion 前发 RAT 地址”的优化（`DecodeStage.scala:273-295`），所以没有一拍空闲寄存器可直接删除。

若仍要压缩 Decode→Rename，只能做结构性改动：

- 从 Decode 输入原始 instruction 直接提取 rs1/rs2/rs3/vd，早于完整 decode 发 RAT 地址；
- 将 RAT 返回值与对应 uop/epoch 一起缓存；
- 对 complex decode、fusion、vector reverse、stall/hold 和 redirect 做重放或修正；
- 仅为简单标量指令建立 fast lane，复杂指令保留原路径。

这会把收益限定为简单指令的 1 拍，同时增加双路径验证成本。只有当 cycle counter 证明 Decode→ROB 延迟是核心瓶颈且 STA 允许时才值得尝试。

### 4.9 P2：CSR→Decode 精确写旁路（恢复路径省 1 拍）

完整 CSR 状态传播上有为时序加入的寄存器：

- `wrapper/CSR.scala:428`：`csrToDecode := RegNext(csrMod.io.toDecode)`，提交 `9548abc92` 明确是 timing fix；
- `CtrlBlock.scala:547`：`decode.io.csrCtrl := RegNext(io.csrCtrl)`。

不建议删除整个宽总线寄存器。可以对少数影响 Decode 的 CSR 写生成窄事件旁路，例如 privilege/FS/VS/singlestep/fusion-enable 的新值和有效位；Decode 在 flush 后第一拍优先选择旁路值，随后回到已寄存的完整状态。这样可能缩短 CSR 写后恢复取指/解码 1 拍，同时不把完整 CSR 状态总线拉成长路径。

### 4.10 P2/P3：CSR scoreboard 或提交态写入（收益 workload 相关）

若 workload 中 CSR 密度较高，最大的损失可能不是固定 1～3 拍，而是 `waitForward + blockBackward` 导致的 ROB 排空和后序阻塞。进一步优化需要：

- 按 CSR 地址或状态域维护 pending-write scoreboard；
- 允许与旧写无冲突的 CSR read 提前执行；
- 对同地址读提供最新值旁路，或维护 speculative CSR shadow；
- 对外部副作用 CSR、AIA claim、计数器、PMP、satp/hgatp、debug/interrupt 状态保留严格顺序；
- redirect 时回滚 speculative 状态，或把真正写 CSR 延迟到提交。

它可能消除不定数量的等待拍，但接近一次 CSR 子系统重构。应先用性能计数器确认 `waitForward/blockBackward` 是实际热点，再投入实现。

## 5. 不建议直接做的改动

### 5.1 直接删除 Decode→Rename 寄存器

会破坏同步 RAT 的请求/响应对齐，并把 decode/fusion、RAT、free-list 和组内 bypass 串在一起。若要省拍，应做 4.8 的前移/fast lane。

### 5.2 直接删除 Rename→Dispatch 寄存器

会合并两段已有明显复杂度的关键路径，并把 Dispatch 资源 ready 反馈到 Rename。收益虽是 1 拍，但 Fmax 风险很高。

### 5.3 直接把 CSR wrapper 的 `DelayN(..., 3)` 改成 0 或 1

这三拍与 uncertain wakeup 协议成套存在。必须同时重新定义提前唤醒距离、取消机制、RF/ROB 对齐和异常返回；否则会出现消费者提前读取错误数据或 ROB 错误完成。

### 5.4 直接恢复 NewCSR 组合输出

该级由提交 `7071df62b` 为时序加入。只适合用小范围 fast-read bank 绕开，不适合全局回退。

### 5.5 直接删除 trap target 或 CSR→Decode 的寄存器

相关寄存器都可追溯到明确的 timing fix。全局删除大概率以 Fmax 损失换取单次事件的一拍，不一定有净收益。

### 5.6 无差别放开 CSR 乱序

AIA claim、interrupt pending、计数器、PMP、地址翻译和 privilege CSR 有外部或全局副作用。没有 scoreboard/shadow/commit 机制时，简单清除 `waitForward/blockBackward` 会破坏精确状态。

## 6. 实施顺序建议

### 阶段 A：先测量，不改架构

增加按事件打点的 cycle counter/histogram：

- Decode fire → ROB enqueue；
- CSR `io.in.fire` → `csrModOutValid` → wrapper WB → ROB completion；
- raw WB → ROB `commit_w`；
- WB exception/replay → ROB `flushOut`；
- ROB `flushOut` → FTQ redirect；
- CSR 因 `waitForward` 等 ROB empty 的周期数；
- CSR `blockBackward` 阻塞的后序 uop 数量。

计数应按普通 CSR read、CSR write rd=x0、CSR read with rd、AIA、XRET、exception、replay、satp/fence 分桶。

### 阶段 B：低侵入旁路

1. replay-only FTQ 快速重定向；
2. 无 RF 写回 CSR early completion；
3. 普通无异常 head-WB completion bypass；
4. ROB-head exception 快速路径。

这些方案都可保留原路径作为校验和 fallback。建议在 debug 配置下同拍比较 fast/slow 结果，在 slow 结果到达时断言 robIdx、异常类型、flush target 和完成次数一致。

### 阶段 C：结构优化

1. Dispatch→ROB allocation/payload 拆分；
2. profile 驱动的 CSR fast-read bank；
3. CSR→Decode 窄旁路；
4. 最后才考虑 RAT fast lane 或 CSR scoreboard。

## 7. 验证清单

### 7.1 功能与精确异常

- 跑完整 difftest、随机指令和长时间回归。
- 定向覆盖 illegal/virtual instruction、ECALL/EBREAK、single-step、trigger、interrupt/NMI/debug entry。
- 覆盖同拍多个 WB，确保最老异常胜出；覆盖 head 与非 head 同拍异常。
- 覆盖 redirect 与 early completion/WB 同拍，确认被杀 uop 不更新 ROB。
- 覆盖 ROB index wrap-around，不能只比较 index value 而忽略 flag/epoch。
- 覆盖向量部分异常、vstart、fof、segment、MMIO；首版 fast path 建议明确排除这些类型。

### 7.2 CSR 定向测试

- `CSRR`：普通只读、状态相关、counter、非法地址和权限不足。
- `CSRW/CSRRS/CSRRC rd=x0`：无 RF 写回但有/无 CSR 写副作用。
- `satp/vsatp/hgatp`、PMP、fcsr/frm/fflags、vstart/vcsr、AIA/IMSIC claim。
- MRET/SRET/MNRET/DRET、WFI、ECALL/EBREAK。
- CSR 写后立即取决于新权限/状态的指令，以及 CSR 写后 redirect。

### 7.3 ROB/IQ/LSQ 一致性断言

- `dispatch fire == ROB alloc == 对应 IQ/LSQ 接受`（按指令类型投影）。
- 每个 ROB entry 的 completion 计数只递减一次且不下溢。
- payload 未完成的 ROB entry 永远不能 commit/walk。
- fast redirect 和 slow redirect 对同一事件最多发出一次。
- fast exception 命中时，后到的 ExceptionGen 结果必须完全一致。

### 7.4 时序与性能

- 对每个方案单独跑综合/STA，重点观察 Decode→RAT、Rename bypass、Dispatch ready、WB→ROB head、CSR read mux 和 CSR→FTQ。
- 同时记录 Fmax、面积、功耗和 IPC；不能只比较拍数。
- 分别测试 ROB 空/满、CSR 稀疏/密集、异常/replay 密集 workload，确认优化命中率。

## 8. 最终建议

若目标是用较小改动尽快获得真实收益，建议先实现“replay-only 快速重定向”和“无 RF 写回 CSR early completion”。二者绕开的都是已经存在但对该特定类型不必要的等待，而且能保留原慢路径。

第二批做 ROB-head 的 completion/exception 旁路：它们对短依赖链和异常恢复有直接收益，但需要更严格的同拍提交与精确异常验证。

Decode→Rename、Rename→Dispatch 以及 NewCSR/trap target 的寄存器都承担明确的同步存储器或 timing-cut 作用，不应作为第一批直接删除。Dispatch→ROB 可以省 1 拍，但建议通过 credit + 紧凑 metadata 拆分实现，而不是把完整 Dispatch 组合逻辑直连 ROB。
