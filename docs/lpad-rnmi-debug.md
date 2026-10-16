# LPAD 在 RNMI 和 Debug 状态下的 ELP 处理

本文只讨论 Zicfilp（Landing Pad）在 RNMI 和 Debug 入口中的状态保存、清零与恢复。普通 M/HS/VS trap 的流程与这里相同，但保存字段不同；普通 trap 的总路径见 [lpad-rtl-design.md](lpad-rtl-design.md)。

代码基线是当前 `SE-kunminghu-v3` 分支。文中“当前已完成”表示可以从 RTL 连接直接确认的内容；“缺少或待确认”表示当前源码没有闭合对应信号，或者还需要仿真/规范核对。

## 1. 先分清三份状态

LPAD 相关的 ELP 不是一个寄存器在所有阶段复用，而是有三个时间边界：

| 状态 | 保存位置 | 描述 |
| --- | --- | --- |
| 推测 ELP | `SpecZicfilp.speculativeELP` | Decode 当前推测指令流是否期待下一条 LPAD |
| 架构 ELP | `ArchZicfilp.archELP` | 已经提交的程序是否仍期待下一条 LPAD |
| trap/debug 保存值 | `MPELP/SPELP/MNPELP/DCSR.PELP` | 进入处理程序前，软件返回时需要恢复的架构 ELP |

这三者不能混用。Decode 可以已经把推测 ELP 清零，但目标 LPAD 还没有提交；此时架构 ELP 仍可能为 1。

```text
受保护 JALR 提交       → archELP = 1
目标 LPAD 被 Decode 接收 → speculativeELP = 0
目标 LPAD 尚未提交      → archELP 仍为 1
```

发生 RNMI 或 Debug 入口时，保存值必须来自架构边界，而不是 Decode 的推测值。

## 2. RNMI 入口和 MNRET 返回应该做什么

假设已经提交了需要 LPAD 的间接跳转，当前状态为：

```text
archELP = 1
```

RNMI 入口应该按以下顺序工作：

```text
1. 保存进入 RNMI 前的 archELP 到 mnstatus.MNPELP
2. 清除实时 archELP，使 RNMI handler 从 ELP=0 开始
3. RNMI handler 正常执行，不继承原程序的 LPAD 期待状态
4. MNRET 读取 MNPELP
5. 根据返回目标特权级的 LPE 决定是否恢复 ELP
6. 返回目标重新 Decode；若恢复为 1，第一条指令必须是合法 LPAD
```

恢复值的逻辑是：

```text
restoredELP = MNPELP && LPE(返回目标模式)
```

如果返回目标已经关闭 LPE，恢复值应为 0；如果仍开启 LPE 且 `MNPELP=1`，恢复值为 1。

### 当前 RTL 已完成的 RNMI 部分

`ArchZicfilp` 在 [CtrlBlock.scala](../src/main/scala/xiangshan/backend/CtrlBlock.scala) 只接收一个通用的 `rob.io.exception.valid`，并没有单独的 RNMI 输入：

```scala
arch.io.trap := rob.io.exception.valid
```

当 RNMI 确实通过 ROB 的 trap 接口表示时，`ArchZicfilp` 的通用 trap 分支会清零 `archELP`。清零逻辑在 [archZicfilp.scala](../src/main/scala/xiangshan/backend/archZicfilp.scala)：

```scala
when (io.trap) {
  archELP := false.B
}
```

因此当前实现是“RNMI 复用 ROB trap 时可以清零”，不是“RNMI 有独立的 ELP 清零通路”。如果某种 NMI 入口绕过 `rob.io.exception.valid`，当前代码不会清零 `archELP`。ROB 在异常边界先锁存架构 ELP，代码在 [Rob.scala](../src/main/scala/xiangshan/backend/rob/Rob.scala)。

RNMI 的 CSR 保存字段在 [MachineLevel.scala](../src/main/scala/xiangshan/backend/fu/NewCSR/MachineLevel.scala) 中定义：

```scala
val MNPELP = RO(9).withReset(0.U)
```

进入 RNMI 的 [TrapEntryMNEvent.scala](../src/main/scala/xiangshan/backend/fu/NewCSR/CSREvents/TrapEntryMNEvent.scala) 将当前 ELP 写入 `MNPELP`：

```scala
out.mnstatus.bits.MNPELP := current.ZicfilpELP.getOrElse(false.B)
```

`MNRET` 事件会把 `MNPELP` 清零；[NewCSR.scala](../src/main/scala/xiangshan/backend/fu/NewCSR/NewCSR.scala) 同时计算：

```scala
legalMNret ->
  (mnstatus.regOut.MNPELP && targetLPE(...))
```

随后 [CSR.scala](../src/main/scala/xiangshan/backend/fu/wrapper/CSR.scala) 将 `retELP` 放进 xRET redirect，CtrlBlock 再把它送到 Decode 的 `SpecZicfilp`。

因此，RNMI 路径的主要 RTL 结构已经存在：

```text
ROB 架构 ELP 快照
    → TrapEntryMNEvent
    → MNPELP
    → MNRET/restoredELP
    → redirect.ZicfilpRetELP
    → SpecZicfilp
```

### RNMI 仍需要确认的地方

第一，`ArchZicfilp` 当前的清零输入是：

```scala
arch.io.trap := rob.io.exception.valid
```

所以必须确认所有 RNMI 入口都通过 ROB 的 `exception.valid` 进入 CSR。如果某个 NMI 入口绕过 ROB，当前架构 ELP 就不会被清零。

第二，内部异常号 17 可能沿 MNCAUSE 原样输出。Zicfiss 使用内部异常位 17，最终架构 cause 应为 18；普通 M/HS/VS TrapEntry 会做这个转换，但 [TrapEntryMNEvent.scala](../src/main/scala/xiangshan/backend/fu/NewCSR/CSREvents/TrapEntryMNEvent.scala) 当前直接使用：

```scala
out.mncause.bits.ExceptionCode := highPrioTrapNO
```

如果 Zicfiss 异常触发 RNMI 或 double-trap 的 MN 路径，需要确认 `mncause` 是否应为架构值 18，而不是内部值 17。这是 RNMI 的异常报告回归项。

## 3. Debug 入口和 DRET 返回应该做什么

Debug 入口的目标状态与 RNMI 相同，但保存位置不同：

```text
1. 保存进入 Debug 前的 archELP 到 dcsr.PELP
2. 清除实时 archELP
3. Debug handler 从 ELP=0 开始执行
4. DRET 读取 dcsr.PELP
5. 根据返回目标模式 LPE 计算恢复值
6. 返回目标重新接受 LPAD 检查
```

恢复规则仍然是：

```text
restoredELP = dcsr.PELP && LPE(返回目标模式)
```

Debug 入口可能由外部 Debug interrupt、触发器、单步或 EBREAK 产生；它不一定是 ROB 的架构异常。因此 Debug 入口不能只依赖 `rob.io.exception.valid` 来清除 ELP。

### 当前 RTL 已完成的 Debug 部分

`DcsrBundle` 定义了 PELP 字段，并在 `HasZicfilp` 为真时允许写入：

```scala
val PELP = RO(18).withReset(0.U)

if (hasZicfilp) {
  PELP.setRW()
}
```

代码在 [DebugLevel.scala](../src/main/scala/xiangshan/backend/fu/NewCSR/DebugLevel.scala)。

Debug 入口事件 [TrapEntryDEvent.scala](../src/main/scala/xiangshan/backend/fu/NewCSR/CSREvents/TrapEntryDEvent.scala) 会保存当前 ELP：

```scala
out.dcsr.bits.PELP := current.ZicfilpELP.getOrElse(false.B)
```

`NewCSR` 的恢复值逻辑包含 DRET：

```scala
legalDret ->
  (dcsr.regOut.PELP && targetLPE(...))
```

`CSR.scala` 对合法 DRET 设置 `ZicfilpXRetValid` 和 `ZicfilpRetELP`，所以 DRET 的 redirect 恢复链路已经存在。

### Debug 入口清零通路

当前 `ArchZicfilp` 保留 ROB trap 输入，同时增加了从 NewCSR 返回的入口脉冲：

```scala
arch.io.trap := rob.io.exception.valid
```

但 Debug 入口由 NewCSR 内部的：

```scala
entryDebugMode := debugMod.io.out.hasDebugTrap && !debugMode
trapEntryDEvent.valid := entryDebugMode
```

产生。NewCSR 将 Debug、RNMI 和 double-trap 入口合并为 `zicfilpEntryClear`，经 `CSRFileIO → Backend → CtrlBlock` 连接到 `ArchZicfilp.entryClear`。ArchZicfilp 在 `trap || entryClear` 时清除实时 `archELP`。

入口清零后的状态是：

```text
进入 Debug 前：archELP = 1
Debug 入口：   dcsr.PELP = 1，archELP 清为 0
Debug handler：从 ELP=0 开始
```

对应代码链为：

```scala
NewCSR.status.zicfilpEntryClear
  → CSRFileIO.zicfilpEntryClear
  → Backend
  → CtrlBlock.fromCSR.zicfilpEntryClear
  → ArchZicfilp.io.entryClear
```

ArchZicfilp 的状态优先级为：

```scala
when (io.trap || io.entryClear) {
  archELP := false.B
}.elsewhen (io.xret.valid) {
  archELP := io.xret.bits
}.elsewhen (hasCommit) {
  archELP := commitELP
}
```

保存值仍来自入口前架构 ELP，不能取清零后的值。Debug 入口与 ROB trap 同周期时，清零条件合并为一个优先分支。

另外，当前 `DretEvent` 的 `dcsr` 更新字段主要包括 `V` 和 `PRV`，没有明显的 `PELP` 清零赋值。需要结合 Zicfilp 规范确认 DRET 是否要求消费并清除 PELP；即使允许保留，也应补一条明确注释和回归，避免下一次 Debug 入口或重复 DRET 使用旧值。

## 4. RNMI/Debug 的完整状态表

| 阶段 | RNMI 应有动作 | 当前 RNMI | Debug 应有动作 | 当前 Debug |
| --- | --- | --- | --- | --- |
| 入口前 | 读取架构 ELP | 已由 ROB/ArchZicfilp 提供 | 读取架构 ELP | 已由 TrapEntryDEvent 读取 |
| 入口保存 | 写 `MNPELP` | 已接通 | 写 `DCSR.PELP` | 已接通 |
| handler 开始 | 实时 ELP 清 0 | ROB trap 或 `zicfilpEntryClear` | 实时 ELP 清 0 | `zicfilpEntryClear` 已接通 |
| handler 执行 | 不继承原 LPAD 期待 | 已有入口清零链 | 不继承原 LPAD 期待 | 已有入口清零链 |
| 返回值 | `MNPELP && targetLPE` | 已接通 | `DCSR.PELP && targetLPE` | 已接通 |
| 返回 redirect | 恢复 Spec ELP | 已接通 | 恢复 Spec ELP | 已接通 |
| 保存字段清理 | MNRET 清 `MNPELP` | 已有 | DRET 是否清 `PELP` | 需要确认 |
| 异常报告 | cause/tval 正确 | MNCAUSE 的内部 17 需验证 | Debug CAUSE 使用 Debug cause | 不使用软件异常号 |

## 5. 建议的最小验证矩阵

### RNMI

1. `archELP=0` 时进入 RNMI：`MNPELP=0`，handler 不要求 LPAD，MNRET 返回后 ELP 仍为 0。
2. `archELP=1` 时进入 RNMI：`MNPELP=1`，handler 从 ELP=0 开始。
3. MNRET 返回到 LPE=1 的目标：恢复 `speculativeELP=1`，目标必须为 LPAD。
4. MNRET 返回到 LPE=0 的目标：恢复 ELP=0。
5. Zicfiss 内部异常 17 进入 RNMI/double-trap：检查 `mncause` 是否为架构 cause 18，tval 是否为 3。

### Debug

1. 外部 Debug interrupt 在 `archELP=1` 时进入：`DCSR.PELP=1`。
2. 进入 Debug 后立即执行普通指令：确认实时 ELP 已为 0，不产生 LPAD software-check。
3. DRET 返回到 LPE=1：恢复 `speculativeELP=1`。
4. DRET 返回到 LPE=0：恢复 ELP=0。
5. 触发器、单步、EBREAK 分别进入 Debug：都应保存同一个入口前的架构 ELP，并清除实时 ELP。
6. Debug 入口与 ROB trap 同周期：保存值必须是入口前架构 ELP，不能取清零后的值。

当前代码已经具备 RNMI/Debug 的 CSR 字段、入口清零和 xRET 恢复骨架；RNMI 的最终异常号转换以及入口/返回时序仍需要上述回归确认。
