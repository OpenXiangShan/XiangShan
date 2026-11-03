# MemBlock 中 AMO、LR/SC 的后端与访存执行流程

## 1. 文档目的与范围

本文结合当前 `XiangShan` 源码，说明 `AMO`、`LR`、`SC` 相关指令在后端和 `MemBlock` 中分别承担什么功能，以及一条原子指令从解码到提交的完整执行过程。

本文重点回答以下问题：

- 后端为什么要把原子指令识别成独立的 `FuType.mou`？
- `STA`、`STD` 分别给原子指令提供什么信息？
- 原子指令为什么不进入普通 `LQ/SQ`？
- `MemBlock` 如何收集地址和数据，并控制 `AtomicsUnit`？
- `AMO`、`LR`、`SC` 在 `DCache MainPipe` 中分别做了什么？
- 原子执行期间有哪些串行化、排空、重放和写回约束？
- 测试框架应该观察哪些事件，哪些行为不能按普通 load/store 建模？

本文不讨论向量访存；`AMOCAS` 只在需要解释原子通路的多 uop 机制时简要提及。

主要源码范围：

- `XiangShan/src/main/scala/xiangshan/backend/decode/DecodeUnit.scala`
- `XiangShan/src/main/scala/xiangshan/backend/decode/DecodeUnitComp.scala`
- `XiangShan/src/main/scala/xiangshan/backend/decode/UopInfoGen.scala`
- `XiangShan/src/main/scala/xiangshan/backend/dispatch/NewDispatch.scala`
- `XiangShan/src/main/scala/xiangshan/backend/issue/Scheduler.scala`
- `XiangShan/src/main/scala/xiangshan/backend/rob/Rob.scala`
- `XiangShan/src/main/scala/xiangshan/backend/rob/RobBundles.scala`
- `XiangShan/src/main/scala/xiangshan/mem/MemBlock.scala`
- `XiangShan/src/main/scala/xiangshan/mem/pipeline/AtomicsUnit.scala`
- `XiangShan/src/main/scala/xiangshan/cache/dcache/mainpipe/MainPipe.scala`
- `XiangShan/src/main/scala/xiangshan/cache/dcache/mainpipe/AtomicsReplayUnit.scala`
  （仅用于确认该类当前未被实例化，不能把它当作现行 replay 数据通路）
- `XiangShan/src/main/scala/xiangshan/cache/dcache/mainpipe/AMOALU.scala`
- `XiangShan/src/main/scala/xiangshan/cache/dcache/DCacheWrapper.scala`

---

## 2. 先建立一个直观认识

### 2.1 AMO、LR、SC 的软件语义

普通 `load` 是“从内存读数据”，普通 `store` 是“把数据写入内存”。原子指令的关键区别是：读、判断、修改、写入之间不能被其他处理器或其他访存操作插入到中间。

以 `AMOADD` 为例，架构上可以理解为：

```text
old = Memory[address]
new = old + rs2
Memory[address] = new
rd = old
```

因此，`AMOADD` 同时具有：

- 读旧值的效果；
- 写新值的效果；
- 把旧值返回整数寄存器的效果；
- 不可分割的原子性要求。

`LR` 和 `SC` 是一对配合使用的指令：

```text
LR:
  rd = Memory[address]
  建立 reservation

SC:
  如果 reservation 仍然有效且地址匹配：写入 rs2，rd = 0
  否则：不写内存，rd = 1
```

这里的 `reservation` 可以通俗理解为“处理器暂时标记了这个 cache block，表示我准备基于刚才读到的值尝试写回”。从体系结构角度看，其他核的竞争写入不能被软件假定为不影响 SC；但在当前香山 RTL 中，同 reservation block 的一致性 probe 会先被暂缓，并不会在到达时直接清除 reservation。源码中明确清除 reservation 的条件主要是 SC 尝试、本地 release/writeback、计数器超时和复位。

### 2.2 香山的实现思路

香山没有把原子指令简单拆成“普通 load + 普通 store”两条独立访存操作，而是使用专门的原子路径：

```text
Decode
  -> FuType.mou
  -> STA 地址 uop + STD 数据 uop
  -> MemBlock 识别原子操作
  -> AtomicsUnit 收集并串行控制
  -> DTLB / PMP / PMA / PBMT 检查
  -> 排空 SBuffer 和 Uncache
  -> DCache atomic port
  -> MainPipe / AMOALU / MissQueue / coherence
  -> Atomic writeback
  -> ROB 完成和提交
```

其中，`STA` 和 `STD` 不是两条架构指令：

- `STA` 主要提供地址 `rs1` 及原子指令控制信息；
- `STD` 主要提供参与运算或写入的源数据 `rs2`；
- 对普通 `AMO/LR/SC`，这两条是同一条架构指令的两个执行侧副本；
- 由于不分配普通 `LQ/SQ`，它们也不会生成普通 load/store 的队列生命周期。

---

## 3. 后端实现了什么

### 3.1 解码：统一归类为 `FuType.mou`

在 `DecodeUnit.scala` 的 RV64A 解码表中，`AMOADD`、`AMOXOR`、`AMOSWAP`、`AMOAND`、`AMOOR`、`AMOMIN`、`AMOMAX`、`LR`、`SC` 等都被解码为：

```scala
FuType.mou
```

同时设置：

```scala
xWen      = true
noSpec    = true
blockBack = true
```

源码位置：

- `XiangShan/src/main/scala/xiangshan/backend/decode/DecodeUnit.scala:237-265`

这些控制信号的含义是：

| 控制项 | 通俗含义 | 对原子指令的作用 |
|---|---|---|
| `FuType.mou` | 选择原子访存执行类别 | 后续进入专用原子路径 |
| `xWen` | 写整数寄存器 | AMO 返回旧值，LR 返回读出的值，SC 返回成功/失败码 |
| `noSpec` | 不进行普通投机执行 | 需要等待前序指令满足顺序要求 |
| `blockBack` | 阻止年轻指令继续向后端流动 | 防止年轻指令越过原子操作 |

### 3.2 普通原子指令是一个架构 uop

普通 `AMO/LR/SC` 默认：

```text
numUops = 1
numWB   = 1
```

源码位置：

- `DecodeUnit.scala:836-840`

因此，后端最终仍然只把它作为一个 ROB 中的架构指令管理。它虽然在内存执行侧同时需要地址和数据两个输入，但不会因此变成两条可独立提交的指令。

`AMOCAS.W/D/Q` 是特殊情况：前端会拆成多个执行 uop，再通过 `fuOpType` 高位编码成
AtomicsUnit 槽号，用于收集比较值、写入值和多个目的寄存器。这个机制不改变本文对
普通 `AMO/LR/SC` 的主流程结论。

### 3.3 调度：复制到 STA 和 STD 执行入口

Memory Scheduler 会根据功能单元配置，把同一个原子架构 uop 分发到：

- 地址执行侧：`STA`；
- 数据执行侧：`STD`。

这里不是前端重新生成了两条互不相关的指令，而是 Memory Scheduler 把同一个
`DynInst` 同时写入地址 IQ 和数据 IQ：

```scala
stdIQEnq.valid := staValidFromDispatch(i)
stdIQEnq.bits  := staIQEnq.bits
```

随后只把 STD 使用的源操作数改成原 STA 输入的 `src(1)`：

```scala
stdIQEnq.bits.psrc(0) := staIQEnq.bits.psrc(1)
```

源码位置：

- `Scheduler.scala:372-383`
- `Scheduler.scala:461-509`

因此，STA 和 STD 在进入各自 IQ 时天然携带相同的：

- `robIdx`；
- `fuType/fuOpType`；
- 原子操作类型；
- `uopIdx`；
- 动态指令调试和恢复身份。

对于普通 `AMO/LR/SC`，`AtomicsUnit.scala:83-92` 描述的统一拆分是：

```text
1 个 STA uop
1 个 STD uop
```

它们需要在后端分别等待：

- `rs1` 地址计算完成；
- `rs2` 数据准备完成。

但“统一存在一个 STD 执行副本”不等于所有指令都必须等待 STD 数据。普通
AMO 和 SC 必须等所需数据收齐，LR 的 DCache request 则在 RTL 中明确绕过
`data_valid`，因为 LR 不使用 `rs2` 修改内存。

### 3.4 乱序发射后 STA/STD 如何保持对应关系

STA IQ 和 STD IQ 是独立调度的，因此同一条原子指令允许出现两种合法顺序：

```text
顺序 A：STA1 先到 AtomicsUnit，STD1 后到
顺序 B：STD1 先到 AtomicsUnit，STA1 后到
```

AtomicsUnit 对两类输入采用不同的接收方式：

- STA 使用 `Decoupled` 入口；`io.in.fire` 后锁存 `uop/rs1/pdest` 并启动状态机；
- STD 使用多个 `Valid` 入口；根据 `fuOpType` 高位携带的原子槽号锁存数据；
- `stdCnt` 统计已经收到的数据片段；
- `data_valid` 表示当前原子操作需要的数据已经收齐。

源码位置：

- `AtomicsUnit.scala:44-48`
- `AtomicsUnit.scala:119-180`
- `AtomicsUnit.scala:474-495`

如果 STA 先到，地址翻译、权限检查和 SBuffer 排空可以先执行；到达
`s_cache_req` 后，普通 AMO/SC 会等待 `data_valid`。如果 STD 先到，数据先被
保存，STA 到达后再启动状态机。

需要特别注意：当前 AtomicsUnit **没有在 STD 入口显式执行**：

```text
storeDataIn.robIdx == 已锁存STA.robIdx
```

这样的多事务查表匹配。它主要使用 `fuOpType` 高位的原子槽号决定数据写入
`rd_l/rs2_l/rd_h/rs2_h`
中的哪个位置。因此，“STA1 已经进入 AtomicsUnit，却收到 AMO2 的 STD2”必须由
后端协议保证不会发生。

这个保证来自原子指令的串行化属性：

- Decode 对 AMO/LR/SC 设置 `noSpec=true`、`blockBack=true`；
- Rename 将其传递为 `waitForward` 和 `blockBackward`；
- `waitForward` 使原子指令在 Dispatch 前等待 ROB 为空，即更老指令已经提交离开 ROB；
- `blockBackward` 阻止更年轻指令继续进入后端；
- ROB 在带 `blockBackward` 的原子指令离开、ROB 重新为空前不会解除阻塞。

源码位置：

- `DecodeUnit.scala:237-265`
- `Rename.scala:306-307`
- `NewDispatch.scala:784-819`
- `Rob.scala:186-193`
- `Rob.scala:393-460`

所以当前设计采用的是：

```text
后端保证同一时刻只有当前这一组原子 STA/STD 可以到达
AtomicsUnit 只收集这一组操作数，不维护多 AMO 匹配表
```

普通 store 的 STD 也不会误入 AtomicsUnit，因为 MemBlock 只转发
`FuType.storeIsAMO` 的 STD 数据。

对于 AMOCAS，多份 STD 可以跨周期、跨 STD 端口到达。这里必须区分两套编号：

```text
DecodedInst.uopIdx：前端拆分出的 uop 顺序编号
fuOpType高位：      AtomicsUnit 使用的 STA/STD 数据槽号
```

AtomicsUnit 实际使用 `fuOpType >> AMOFuOpWidth` 选择槽位，而不是直接使用
`DecodedInst.uopIdx`：

| 前端 split uopIdx | 原子槽号 | 输入数据 | STA 行为 |
|---:|---:|---|---|
| W/D 0 | 1 | `X(rs2)` -> `rs2_l` | 丢弃 STA 副本 |
| W/D 1 | 0 | `X(rd)` -> `rd_l`，同时携带 `rs1` | 保留地址 STA |
| Q 0 | 1 | `X(rs2)` -> `rs2_l` | 丢弃 STA 副本 |
| Q 1 | 0 | `X(rd)` -> `rd_l`，同时携带 `rs1` | 保留低半返回 STA |
| Q 2 | 3 | `X(rs2+1)` -> `rs2_h` | 丢弃 STA 副本 |
| Q 3 | 2 | `X(rd+1)` -> `rd_h` | 保留高半返回 STA |

这些槽号只负责标识“同一原子操作中的哪个数据片段”，不能代替 `robIdx/owner`
对不同原子操作做匹配。

### 3.5 Dispatch：不分配普通 LQ/SQ

在 `NewDispatch.scala:688-700`，原子指令不被归入普通 load/store 的 LSQ allocation：

```scala
enqLsqIO.needAlloc(i) := 0.U
```

因此，普通 `AMO/LR/SC`：

- 不占用 `LoadQueue` entry；
- 不占用 `StoreQueue` entry；
- 不参加普通 load 的依赖检查和 load replay 生命周期；
- 不参加普通 store 的 SQ commit 生命周期；
- 不通过 `scommit` 释放普通 SQ 项。

这也是测试环境不能把 AMO 伪装成“一条 load transaction 加一条 store transaction”的原因。

#### 不分配 LQ/SQ 不等于完全绕过 LSQ 容量反压

当前 Dispatch 的公共 ready 条件仍包含：

```scala
fromRename(i).ready :=
  allowDispatch(i) && !uopBlockByIQ(i) && thisCanActualOut(i) && lsqCanAccept
```

而：

```scala
lsqCanAccept := enqLsqIO.canAccept
```

因此 AMO 虽然 `needAlloc=0`、也不会向 LQ/SQ 发出有效分配请求，但仍可能因为公共
`lsqCanAccept=0` 暂停在 Dispatch。`LsqEnqCtrl` 使用影子空闲计数保守判断：

```scala
val ldCanAccept = lqCounter >= lqAllocNumber +& lqMaxAllocate.U
val sqCanAccept = sqCounter >= sqAllocNumber +& sqMaxAllocate.U
```

对只包含 AMO 的 Dispatch 组，`lqAllocNumber/sqAllocNumber` 为 0，但仍要求 LQ/SQ
保留至少一组最大入队宽度对应的空闲余量。这是公共 Dispatch 门控带来的保守反压，
不是因为 AMO 自身需要 LSQ entry。

同时，`waitForward` 看到 ROB 为空只表示更老架构指令已经提交，不表示 SQ、SBuffer、
Uncache 已经全部为空。尤其 SQ 的 `scommit` 来自 MemBlock 的 `sqDeq`，而不是 ROB
commit；已经提交的旧 store 仍可能留在 SQ，随后再进入 SBuffer/Uncache。

源码位置：

- `NewDispatch.scala:447-451`
- `NewDispatch.scala:526-534`
- `NewDispatch.scala:673-700`
- `LSQWrapper.scala:339-415`

### 3.6 ROB：等待原子 writeback，而不是等待 SQ commit

解码阶段生成 `commitType` 时，AMO 被显式排除在普通 store 之外：

```scala
decodedInst.commitType := Cat(
  isLs | isVls,
  (isStore && !isAMO) | isVStore | isBranch
)
```

源码位置：

- `DecodeUnit.scala:946-953`

所以 MOU 的 `commitType` 是 `NORMAL`，不是 `LOAD` 或 `STORE`。ROB 中：

- `scommit` 只统计 `CommitType.STORE`，原子指令不会增加 `scommit`；
- `stdWritebacked` 对 MOU 入 ROB 时直接满足，不等待普通 STD writeback；
- 真正的完成条件是 AtomicsUnit 发出的最终 atomic writeback，使该 ROB entry 的 `uopNum` 减到 0。

相关源码：

- `Rob.scala:838-843`
- `Rob.scala:1009-1059`
- `RobBundles.scala:84-100`
- `MemBlock.scala:521-530`

因此，原子指令的提交逻辑可以概括为：

```text
原子执行完成
  -> AtomicsUnit 产生一个 MOU writeback
  -> 整数结果或 SC 状态进入写回网络
  -> ROB 的 uopNum 归零
  -> ROB 按普通架构指令退休
```

---

## 4. MemBlock 的公共原子执行流程

### 4.1 第一步：识别原子 STA

`MemBlock` 在 `issueSta` 和 `issueHya` 入口检查：

```scala
FuType.storeIsAMO(uop.fuType)
```

源码位置：

- `MemBlock.scala:1799-1803`

一旦识别到原子操作，MemBlock 会：

1. 把该入口的 `ready` 改接到 `AtomicsUnit.io.in.ready`；
2. 禁止对应的普通 `StoreUnit` 或 `HybridUnit` 接收这个 uop；
3. 记录当前使用的是哪一个原子入口；
4. 把 MemBlock 状态切换到 `s_atomics`。

这保证一个原子操作执行期间，普通路径不会同时消费同一条原子 uop。

### 4.2 第二步：收集 STD 数据

MemBlock 使用：

```scala
st_data_atomics(i) = stData(i).valid && FuType.storeIsAMO(...)
```

来识别原子数据输入，并将其送入：

```scala
atomicsUnit.io.storeDataIn(i)
```

普通 store 数据则继续送往 LSQ 的 `storeDataIn`，原子数据不会进入普通 SQ 数据路径。源码位置：

- `MemBlock.scala:532-534`
- `MemBlock.scala:1163-1165`
- `MemBlock.scala:1826-1829`

### 4.3 第三步：AtomicsUnit 接管 DTLB 端口

AtomicsUnit 使用 `loadUnit(0)` 的 DTLB requestor：

```scala
val amoTlb = dtlb_ld(0).requestor(0)
```

当 MemBlock 处于原子状态时：

- LoadUnit 0 的普通 load writeback 被暂时关掉；
- AtomicsUnit 接管 LoadUnit 0 的 TLB 端口；
- 硬件 prefetch request 被关闭；
- 设计通过断言确保 LoadUnit 0 没有未完成的普通输出。

源码位置：

- `MemBlock.scala:1844-1857`

这说明原子 TLB 请求不是一条独立的完整 TLB 通路，而是复用了已有端口，因此测试时必须考虑端口占用和时序切换。

### 4.4 第四步：TLB、对齐和权限检查

AtomicsUnit 状态机从：

```text
s_invalid
  -> s_tlb_and_flush_sbuffer_req
  -> s_pm
```

开始执行地址翻译和权限检查。

TLB command：

```scala
isLr ? TlbCmd.atom_read : TlbCmd.atom_write
```

源码位置：

- `AtomicsUnit.scala:454-462`

地址对齐检查：

- W：4 字节对齐；
- D：8 字节对齐；
- Q：16 字节对齐。

源码位置：

- `AtomicsUnit.scala:250-256`

异常归类上：

- `LR` 主要使用 load 类异常；
- `SC/AMO` 主要使用 store 类异常；
- 页故障、guest page fault、access fault、breakpoint 和硬件错误会通过原子写回/异常路径进入后端。

随后 `s_pm` 阶段检查：

- PMP 权限；
- PMA 属性；
- PBMT 属性；
- MMIO、IO、NC 区域。

当前源码中，MMIO、IO、NC 原子访问不会继续走普通 uncache atomic 流程，而是被转换为 access fault 条件处理。这个行为属于当前实现约束，测试框架不能默认所有原子地址都可以访问外设区域。

### 4.5 第五步：进入 AtomicsUnit 后排空旧 store 路径

原子 STA 被 MemBlock/AtomicsUnit 接收时，**不要求** SBuffer、Uncache 已经为空。
`io.in.fire` 发生后，AtomicsUnit 先锁存原子地址 uop，并进入
`s_tlb_and_flush_sbuffer_req`。在这个状态中，TLB 查询和旧 store 路径 flush 可以并行
启动：

```scala
io.flush_sbuffer.valid := !sbuffer_empty && (...)
```

MemBlock 把以下两者都纳入空状态判断：

```scala
stIsEmpty = sbuffer.io.flush.empty && uncache.io.flush.empty
```

这里的 `sbuffer.io.flush.empty` 不仅表示 SBuffer 自己为空，还包含
`sbuffer.io.sqempty`，因此实际等待的旧 store 路径近似为：

```text
StoreQueue + SBuffer + Uncache
```

只有 `stIsEmpty` 成立，AtomicsUnit 才会在权限检查完成后进入：

```text
s_cache_req
```

如果当时还没有排空，则先进入：

```text
s_wait_flush_sbuffer_resp
```

等待排空后再转到 `s_cache_req`。所以“必须为空”的检查点是 **DCache atomic request
发出之前**，不是测试框架向 MemBlock 发射 STA/STD 之前，也不是 AtomicsUnit 接受
原子 STA 之前。

相关源码：

- `AtomicsUnit.scala:299-313`
- `AtomicsUnit.scala:467-472`
- `MemBlock.scala:1781-1792`

通俗地说，测试框架可以先把 AMO 交给 MemBlock；MemBlock 接住 AMO 后，由内部状态机
请求并等待旧 store 路径排空，最后才真正访问 DCache。这样既不会把内部排空责任错误
推给激励端，也能避免原子操作越过更老的 store。

### 4.6 第六步：构造 DCache 原子请求

AtomicsUnit 将 `LSUOpType` 转换为 DCache 命令：

| `LSUOpType` | DCache command |
|---|---|
| `lr_w/lr_d` | `M_XLR` |
| `sc_w/sc_d` | `M_XSC` |
| `amoswap` | `M_XA_SWAP` |
| `amoadd` | `M_XA_ADD` |
| `amoxor` | `M_XA_XOR` |
| `amoand` | `M_XA_AND` |
| `amoor` | `M_XA_OR` |
| `amomin` | `M_XA_MIN` |
| `amomax` | `M_XA_MAX` |
| `amominu` | `M_XA_MINU` |
| `amomaxu` | `M_XA_MAXU` |

源码位置：

- `AtomicsUnit.scala:491-527`

请求中同时携带：

- cache block 地址；
- 虚地址；
- block 内的 word index；
- `amo_data`：参与运算或写入的数据；
- `amo_mask`：W/D/Q 的字节掩码；
- `amo_cmp`：比较交换类操作的比较值。

### 4.7 第七步：DCache MainPipe 执行

原子请求通过 DCache 的 `atomic_req` 进入 MainPipe，并与 probe、refill、store 请求统一仲裁：

- `MainPipe.scala:132-138`
- `MainPipe.scala:253-268`

MainPipe 负责：

1. 查询 tag、coherence metadata 和 data array；
2. 判断命中和当前一致性权限是否足够；
3. 命中时读取旧数据；
4. 普通 AMO/LR miss 时进入 MissQueue 获取 cache line 和权限；SC miss 直接形成失败条件；
5. 对 AMO 执行运算；
6. 对成功的写操作更新 data array 和 coherence metadata；
7. 返回旧值或 SC 成功/失败状态。

#### 4.7.1 这里不存在第三条普通 Load 流水

从后端看，原子指令有 STA 地址侧和 STD 数据侧；但内存旧值不是由另一条 LDA
uop 读取。AtomicsUnit 把地址、操作类型和 `rs2` 合成一个 `atomic_req`，旧值由
DCache MainPipe 内部直接读取 Data SRAM。

因此正确的数据流是：

```text
后端操作数准备：STA(rs1) + STD(rs2)
                         |
                         v
                  一个 atomic_req
                         |
                         v
DCache内部执行：读取old -> AMOALU/比较 -> 条件写入new -> 返回old/SC状态
```

而不是：

```text
LDA读 + STA写地址 + STD写数据，三条独立访存流水并行完成
```

#### 4.7.2 MainPipe 的逐级执行

普通 AMO 命中情况下可以按以下阶段理解。

**S0：仲裁并读取 Tag/Meta**

`atomic_req` 与 probe、refill、普通 store 共同进入 MainPipe 仲裁：

```scala
arbiter(
  in = Seq(io.probe_req, io.refill_req, store_req, io.atomic_req),
  ...
)
```

源码位置：

- `MainPipe.scala:251-275`

**S1：读取 Data SRAM 旧值**

MainPipe 对所有 AMO 设置：

```scala
val amo_need_data = !s0_req.probe && s0_req.isAMO
```

并通过 `data_readline` 读取 cache line：

```scala
io.data_readline.valid := s1_valid && s1_need_data
```

源码位置：

- `MainPipe.scala:285-311`
- `MainPipe.scala:826-830`

这说明 AMOSWAP 也必须读取旧值，因为旧值需要返回 `rd`；即使 `rd=x0`，原子
读改写语义仍不能退化成普通 store。

**S2：检查 hit、写权限、miss 和 replay**

MainPipe 使用 tag 命中和 coherence permission 共同生成 `s2_hit`。普通 AMO/LR 在没有
数据或所需权限时可以进入 MissQueue，资源不足时返回 replay；SC 是例外，它在 S2 被
直接允许进入 S3，并由 `!s3_hit` 参与 `s3_sc_fail`，不会按普通 AMO miss 路径申请
MissQueue。

源码位置：

- `MainPipe.scala:447-501`

**S3a：取得旧值并完成运算**

Data SRAM 的旧值进入：

```scala
amoalu.io.lhs := s3_data_word
amoalu.io.rhs := s3_req.amo_data
```

普通 AMO 为满足 AMOALU 时序，会在 S3 内部多停一拍。`do_amoalu` 计算并锁存
合并后的新 cache line，`s3_s_amoalu` 记录运算已经完成：

```scala
val do_amoalu = amo_wait_amoalu && s3_valid && !s3_s_amoalu
val s3_amo_data_merged_reg = RegEnable(s3_amo_data_merged, do_amoalu)
when (do_amoalu) { s3_s_amoalu := true.B }
```

源码位置：

- `MainPipe.scala:545-567`
- `MainPipe.scala:613-728`

**S3b：写入新值并返回旧值**

只有 AMOALU 已完成，并且 Data/Meta 写端口满足 ready 条件，普通 AMO 才能离开
S3：

```scala
val s3_amo_can_go = ... &&
  (io.data_write.ready || !update_data) &&
  (s3_s_amoalu || !amo_wait_amoalu)
```

新值通过 `data_write` 写回 Data SRAM，旧值通过 `atomic_resp` 返回：

```scala
io.data_write.bits.data := s3_amo_data_merged_reg
atomic_hit_resp.data := Mux(s3_sc, s3_sc_fail.asUInt, s3_data_quad_word)
```

源码位置：

- `MainPipe.scala:742-762`
- `MainPipe.scala:876-901`
- `MainPipe.scala:963-979`

所以普通 AMO 的直观时序是：

```text
S0   查Tag/Meta
S1   读Data SRAM旧值
S2   检查hit和一致性写权限
S3a  old和rs2进入AMOALU，锁存new
S3b  new写Data SRAM，同时把old返回AtomicsUnit
```

#### 4.7.3 哪些动作可以并行，哪些不能

可以重叠的是：

- STA 地址准备和 STD 数据准备；
- 地址到达后的 TLB 查询、权限检查、SBuffer flush 与等待 STD 数据；
- DCache 流水中不同 set、不同请求在没有冲突时的常规流水化活动。

不能任意并行的是：

- AMO 的旧值读取、结果计算和新值写回，因为 `new` 依赖 `old`；
- SC 的 reservation 检查与条件写入；
- AMOCAS 的旧值比较与条件写入。

因此原子性不是“读写在同一个周期发生”，而是：

> 微架构内部按照读、算、写的先后顺序推进，但整个序列由同一个 atomic
> transaction 和同一个 MainPipe 上下文保护，对外不能观察或插入中间状态。

MainPipe 的 `s3_valid/s3_ready` 会在运算或写端口未准备好时保持当前请求；同 set
冲突检查会阻止冲突请求进入；coherence metadata 保证当前核先获得所需权限。
这些机制和 AtomicsUnit 的单事务状态机共同保证原子性，而不是 AMOALU 单独保证。

### 4.8 第八步：处理 miss、replay 和 response

对于普通 AMO/LR，AtomicsUnit 接收到 DCache response 后：

- `miss && replay`：回到 `s_cache_req`，重新发送；
- `miss` 但已进入 MissQueue：等待 MissQueue 完成后重新推进；
- 非 miss：锁存数据、错误信息和 response id，进入写回阶段。

源码位置：

- `AtomicsUnit.scala:337-369`
- `DCacheWrapper.scala:1467-1472`

因此原子操作不是“一次请求必然一次响应”。测试环境必须允许：

```text
请求 -> miss -> replay -> 再请求 -> response
```

而不能以单周期或固定延迟模型判断失败。

SC miss 直接形成 SC 失败结果，不套用上述 MissQueue 重试描述。源码虽然保留了
`AtomicsReplayEntry` 类，但当前工程中没有其实例；现行 checker 应以 AtomicsUnit、
MainPipe 和 MissQueue 的实际连接为准。

### 4.9 第九步：Atomic writeback 和 MemBlock 退出

MemBlock 将 AtomicsUnit 的输出接到 `AtomicWBPort`，并覆盖该端口原本的普通 load writeback：

```scala
atomicWritebackOverride = Mux(
  atomicsUnit.io.out.valid,
  atomicsUnit.io.out.bits,
  loadUnits(AtomicWBPort).io.ldout.bits
)
```

源码位置：

- `MemBlock.scala:520-530`

当 `atomicsUnit.io.out.valid` 时，MemBlock 状态从 `s_atomics` 回到 `s_normal`。输出保持：

```scala
io.out.bits.uop.fuType := FuType.mou.U
```

因此后端可以根据 `fuType.mou` 识别这是原子结果，而不是普通 load 结果。
需要注意，AtomicsUnit 自身只有在该输出真正 `fire` 后才执行 `resetFSM()`；如果写回
端反压，它仍停留在完成状态并保持结果。

---

## 5. 三类指令的具体流程

### 5.1 AMO：读旧值、运算、写新值、返回旧值

#### 执行步骤

以 `AMOADD.W/D` 为例：

```text
1. Decode 将指令标记为 FuType.mou。
2. 后端生成一个架构 uop，并分别准备 STA 地址和 STD 数据。
3. Dispatch 不分配 LQ/SQ。
4. MemBlock 从 issueSta 识别该 uop，切换到 s_atomics。
5. AtomicsUnit 收集 rs1 和 rs2。
6. AtomicsUnit 发起 TLB、对齐、PMP/PMA/PBMT 检查。
7. AtomicsUnit 请求排空 SBuffer/Uncache。
8. DCache MainPipe 查询 cache line 和写权限。
9. 命中时读取旧值，AMOALU 计算 new = OP(old, rs2)。
10. 新值写回 DCache data array，旧值作为 response 返回。
11. AtomicsUnit 对 W/D 结果做必要的位宽处理。
12. 旧值通过 AtomicWBPort 写入 rd。
13. ROB 完成并提交。
```

#### MainPipe 中的关键行为

`MainPipe.scala:567-728` 中：

- 普通 AMO（不含 LR、SC、CAS）进入 `AMOALU`；
- `AMOALU` 使用 cache 中的旧数据作为 `lhs`；
- 使用请求中的 `amo_data` 作为 `rhs`；
- 根据命令选择加法、逻辑、最小值或最大值；
- 使用 `amo_mask` 合并到 cache line 对应 word；
- 只有获得写权限且操作允许时，才更新 data array。

AMO 的返回值不是运算后的 `new`，而是操作前的 `old`：

```scala
atomic_hit_resp.data := Mux(s3_sc, s3_sc_fail.asUInt, s3_data_quad_word)
```

源码位置：

- `MainPipe.scala:876-901`

#### AMO 的主要约束

- 必须等待 STD 数据收齐，`data_valid` 之前不能发请求；
- 必须先完成地址翻译和权限检查；
- 必须等待 SBuffer/Uncache 排空；
- DCache miss 可能触发 MissQueue 和 replay；
- 不能把 AMO 当成普通 store，因为它还必须返回旧值；
- 不能把 AMO 当成普通 load，因为它必须持有写权限并更新 cache；
- 当前实现对 `aq/rl` 没有单独微架构控制，而是统一采用保守串行化。

### 5.2 LR：读数据并建立 reservation

#### 执行步骤

```text
1. Decode 将 LR 标记为 FuType.mou。
2. 后端仍按统一 store split 结构准备 STA 地址侧和 STD 执行副本；LR 架构上不使用 rs2 做运算。
3. MemBlock 不分配 LQ/SQ，并进入 s_atomics。
4. AtomicsUnit 使用 TlbCmd.atom_read 发起地址翻译。
5. 检查 LR 对齐、load page fault、load access fault 和权限。
6. 排空 SBuffer/Uncache。
7. LR 的 DCache request 明确绕过 `data_valid`，只受 `block_lr` 和 `s_cache_req` 控制。
8. DCache MainPipe 执行 M_XLR。
9. 读取 cache 中旧数据；命中或 miss/refill 成功后建立 reservation。
10. 旧数据返回 AtomicsUnit，并按 W/D 规则写回 rd。
11. Atomic writeback 使 ROB 完成。
```

#### reservation 的建立

MainPipe 在成功 LR 时：

```scala
lrsc_count := (LRSCCycles - 1).U
lrsc_addr  := get_block_addr(s3_req.addr)
```

源码位置：

- `MainPipe.scala:626-649`

注意 reservation 保存的是 cache block 地址，而不是简单的字节地址。因此同一 cache block 内的地址比较会受到 block 粒度影响。

#### LR 的关键约束

- LR 不能带来普通 LQ entry；
- LR 虽然可能仍有统一生成的 STD 执行副本，但功能请求不等待 `data_valid`；
- LR 必须先成功完成地址翻译；
- LR 成功后才建立 reservation，TLB fault 或 DCache 错误不能建立有效 reservation；
- LR 请求可能因为 `block_lr` 被暂缓，用于缓解 LR storm；
- 若外部 probe 正在处理同一 reservation block，ProbeQueue 会暂时阻塞该 probe，避免建立 reservation 的竞争窗口。

### 5.3 SC：检查 reservation，成功时写入

#### 执行步骤

```text
1. Decode 将 SC 标记为 FuType.mou。
2. 后端准备 STA 地址和 STD 写入数据。
3. MemBlock 不分配 LQ/SQ，并进入 s_atomics。
4. AtomicsUnit 使用 TlbCmd.atom_write 发起地址翻译。
5. 检查 SC 对齐、store page fault、store access fault 和权限。
6. 排空 SBuffer/Uncache。
7. DCache MainPipe 执行 M_XSC。
8. 检查 reservation 是否有效、block 地址是否匹配、当前请求是否命中。
9. 检查失败：不写 data array，返回 1。
10. 检查成功：写入 rs2，返回 0，并清除 reservation。
11. 返回值通过 AtomicWBPort 写入 rd。
12. ROB 完成并提交。
```

#### SC 失败条件

源码中：

```scala
s3_sc_fail := s3_sc && (!s3_lrsc_addr_match || !s3_hit)
```

源码位置：

- `MainPipe.scala:614-624`

因此当前实现下，SC 失败至少包括：

1. reservation 已失效；
2. reservation block 地址与 SC 地址不匹配；
3. SC 没有命中 DCache。

SC 的写掩码也显式受 `s3_sc_fail` 控制：

```scala
Mux(s3_req.word_idx === i.U && !s3_sc_fail, s3_req.amo_mask, 0.U)
```

源码位置：

- `MainPipe.scala:695-710`

所以 SC 失败不会产生部分写入。

#### SC 的主要约束

- SC 不能按普通 store 的方式进入 SBuffer；
- SC 失败也必须产生一次原子结果写回，结果是 `1`，而不是“没有响应”；
- SC 成功结果是 `0`；
- SC 需要 STD 数据，不能在 `data_valid` 之前发送请求；
- reservation 地址按 cache block 比较；
- 本地 release/writeback、计数器超时以及此前已执行过 SC，都会使后续 SC 失败；
- 同 reservation block 的外部 probe 在当前实现中先被阻塞，不是直接把本次 SC 判失败；
- 当前实现要求 SC 命中，miss 情况不能直接视为成功。

### 5.4 AMOCAS：比较成功才交换

CAS 是 Compare-And-Swap，可以通俗理解为：

```text
old = Memory[address]
if old == expected:
  Memory[address] = newValue
rd = old
```

在当前 RISC-V AMOCAS 编码和香山实现中：

- `expected` 来自指令执行前的 `rd`；
- `newValue` 来自 `rs2`；
- DCache 读出的 `old` 最终写回 `rd`；
- 比较失败时内存保持不变，但旧值仍写回 `rd`。

AtomicsUnit 最终构造：

```scala
pipe_req.amo_data := genWdataAMO(rs2, uop.fuOpType)
pipe_req.amo_cmp  := genWdataAMO(rd,  uop.fuOpType)
```

源码位置：

- `AtomicsUnit.scala:94-98`
- `AtomicsUnit.scala:525-537`
- `MainPipe.scala:616-624`
- `MainPipe.scala:711-726`

#### 为什么 AMOCAS.W/D 需要 2 个 STD

一个 STD uop 一次提供一个 XLEN 寄存器值。W/D CAS 需要两个独立输入：

```text
STD piece 0：rd   -> expected/compare value
STD piece 1：rs2  -> new/swap value
```

所以 W/D 需要两个 STD。它不是执行两次内存写，而是把比较值和替换值分别送入
AtomicsUnit。

DecodeUnitComp 将 AMOCAS.W/D 拆成两个 uop：

```text
uop A：读取 rs2，fuOpType 高位编码 data index 1
uop B：读取 rs1 和旧 rd，fuOpType 高位编码 data index 0
```

其中只有包含 `rs1` 和最终 `pdest` 的 uop B 进入 STA IQ；uop A 的 STA 副本通过
`isDropAmocasSta` 丢弃，但它的 STD 副本保留。

源码位置：

- `DecodeUnitComp.scala:212-246`
- `NewDispatch.scala:455-458`
- `Scheduler.scala:381-383`

#### 为什么 AMOCAS.Q 需要 4 个 STD

Q 的操作宽度是 128 bit，而一个整数寄存器和一个 STD uop 都是 64 bit。CAS 同时
需要一个 128-bit 比较值和一个 128-bit 替换值：

```text
expected = {X(rd+1),  X(rd)}
newValue = {X(rs2+1), X(rs2)}
```

因此需要四个 64-bit STD 数据片：

| AtomicsUnit 数据槽 | 来源 | 含义 |
|---|---|---|
| `rd_l`，index 0 | `X(rd)` | 比较值低 64 bit |
| `rs2_l`，index 1 | `X(rs2)` | 替换值低 64 bit |
| `rd_h`，index 2 | `X(rd+1)` | 比较值高 64 bit |
| `rs2_h`，index 3 | `X(rs2+1)` | 替换值高 64 bit |

这四份输入合计 256 bit，但真正访问的内存宽度仍是 128 bit：其中 128 bit 是
比较值，另外 128 bit 是候选新值。

AMOCAS.Q 还需要两个 STA uop，因为 128-bit 旧值要分别写回两个 64-bit 目的物理
寄存器：

```text
STA index 0：保存低 64-bit 返回值的 pdest
STA index 2：保存高 64-bit 返回值的 pdest
```

AtomicsUnit 在 `s_finish` 写回低 64 bit，然后进入 `s_finish2` 写回高 64 bit。

#### AMOCAS.Q 的两个 STA 可以任意顺序到达吗

不可以。槽 0 的 STA 必须先启动 AtomicsUnit，槽 2 的 STA 只能在之后补充高 64 bit
返回值对应的 `pdest`：

```text
STA槽0：携带有效rs1、主uop和低半pdest，负责启动状态机
STA槽2：只补充高半pdest，不能单独启动原子事务
```

如果槽 2 在 AtomicsUnit 空闲时先到，它会被当成主输入锁存，但其 `fuOpType` 高位已
带槽号 2，无法通过精确的 `isAMOCASQ(uop.fuOpType)` 判断，后续槽 0 也不能按 Q 的
第二 STA 路径被接收。因此后端和测试框架都必须保证：

```text
AMOCAS.Q first_sta_slot == 0
sta_slot_2 may_fire only after sta_slot_0
```

源码位置：

- `UopInfoGen.scala:239-245`
- `DecodeUnitComp.scala:247-278`
- `AtomicsUnit.scala:79-98`
- `AtomicsUnit.scala:146-180`
- `AtomicsUnit.scala:401-417`
- `AtomicsUnit.scala:474-477`
- `package.scala:645-647`

---

## 6. reservation 的失效和一致性配合

### 6.1 时间失效

`lrsc_count` 每周期递减：

```scala
when (lrsc_count > 0.U) {
  lrsc_count := lrsc_count - 1.U
}
```

源码位置：

- `MainPipe.scala:633-644`

当计数器低于 `LRSCBackOff` 时，`lrsc_valid` 变为无效。

### 6.2 本地 release/writeback 失效

`DCacheWrapper` 将 writeback 请求与 reservation block 地址比较，生成：

```scala
invalid_resv_set
```

源码位置：

- `DCacheWrapper.scala:1592-1598`

MainPipe 收到这个信号后清除 reservation。也就是说，本地 cache line 被释放时，不再保留对该 block 的 LR 标记。

这里的 writeback 是 DCache cache line 的 release、替换或向下级缓存写回，不是整数
寄存器 writeback。reservation 依赖 L1 DCache 仍在本地跟踪该 cache block 及其一致性
状态；一旦该 line 被 release/writeback，本地可能失去该 line 或其一致性权限，其他
master 也可能随后取得权限并修改数据。此时如果仍允许旧 reservation 支持 SC 成功，
就可能基于已经失去本地跟踪依据的状态完成错误写入。

因此当前实现采用保守规则：即使 release 后不一定已经发生外部写入，只要该 block
离开本地可靠跟踪范围，就清除 reservation，让后续 SC 失败并重新从 LR 开始。

### 6.3 SC 会消费 reservation

MainPipe 对 LR/SC 的 reservation 更新逻辑是：

```scala
when (s3_valid && (s3_lr || s3_sc)) {
  when (s3_can_do_amo && s3_lr) {
    lrsc_count := (LRSCCycles - 1).U
    lrsc_addr := get_block_addr(s3_req.addr)
  }.otherwise {
    lrsc_count := 0.U
  }
}
```

只有成功 LR 会重新装载计数器。SC 不满足内部的 `s3_lr` 分支，因此只要 SC 到达该
处理点，无论最终成功还是失败，都会把 `lrsc_count` 清零。

可以把一次 reservation 理解为“一次 SC 尝试资格”：

```text
LR成功：产生一次资格
SC成功：完成写入并消费资格
SC失败：不写内存，但同样消费资格
```

这样设计可以避免在内存或一致性状态已经发生变化后，软件不重新读取数据却反复使用
同一次 LR 的旧判断执行多个 SC。SC 返回失败后，处理器不会自动重新执行整组 LR/SC：

```text
SC最终失败 -> 不写内存 -> rd返回1 -> reservation清除
            -> 软件检测非零结果 -> 重新执行LR -> 重新计算 -> 再执行SC
```

若软件在没有新 LR 的情况下直接再次执行 SC，由于 `lrsc_valid=false`，后一个 SC
仍然失败。需要区分 DCache miss/replay 等尚未形成架构结果的内部重试：内部 replay
可以继续处理同一个 SC；一旦 SC 已经向 `rd` 返回失败，本次架构事务和 reservation
就已经结束。

源码位置：

- `MainPipe.scala:614-638`
- `MainPipe.scala:876-901`

### 6.4 外部 probe 竞争

MainPipe 输出：

```text
lrsc_locked_block
update_resv_set
```

并连接到 ProbeQueue。ProbeQueue 会在 reservation 建立或 probe 命中同一 block 时暂停相关 probe，源码位置：

- `DCacheWrapper.scala:1577-1578`
- `Probe.scala:187-200`

这是为了避免 LR 建立 reservation 的同一时间窗口被 probe 插入，破坏 LR/SC 语义。

需要区分“体系结构上的外部竞争”与“当前 RTL 的直接状态更新条件”：

- ProbeQueue 看到同 block probe 时，先保持该 probe，等待 `lrsc_locked_block.valid` 解除；
- MainPipe 没有把 `probe_valid` 直接接成 `lrsc_count := 0`；
- reservation 若因超时、SC 尝试或本地 writeback 被清除，之前暂缓的 probe 才能继续；
- 因此 checker 不应建模成“probe 一出现，reservation 当拍失效”。

---

## 7. 当前实现中的统一约束行为

### 7.1 原子指令执行时 MemBlock 进入资源接管状态

`MemBlock` 使用：

```scala
val s_normal +: s_atomics = Enum(StaCnt + HyuCnt + 1)
```

原子 uop 进入后，MemBlock 处于对应的 `s_atomics` 状态，直到 AtomicsUnit 输出结果。

这意味着：

- 同一时间不应有多个普通原子操作同时占用 AtomicsUnit；
- 接收原子 STA 的对应 StoreUnit/HybridUnit 入口改由 AtomicsUnit 接管；
- AtomicsUnit 的 `feedbackSlow` 被转发给当前地址 issue queue；
- 原子返回前不能把状态提前恢复为普通模式。

`s_atomics` 本身不是一个负责扫描、杀掉全部普通访存的全局开关。AMO 执行窗口内之所以
没有新的普通 load/store 与它并行进入 MemBlock，主要依靠 Decode/Dispatch/ROB 的
`waitForward + blockBackward` 屏障；MemBlock 状态负责的是原子专用端口和局部资源接管。

### 7.2 前后端串行化

所有基础原子指令都带有 `noSpec=true` 和 `blockBack=true`。相关逻辑在：

- `NewDispatch.scala:795-819`
- `Rob.scala:192-193`
- `Rob.scala:393-460`

整体效果是：

```text
更老指令提交并使ROB为空
  -> 原子指令才能Dispatch到后端
  -> 同组中位于原子指令之后的年轻指令被blockBackward挡住
  -> 原子进入ROB后，后续指令继续被hasBlockBackward挡在Dispatch之前
  -> 原子写回、提交并离开ROB后，后续流动恢复
```

这种做法比仅实现某一个内存排序位更保守，但会牺牲并行度。

#### 7.2.1 AMO 前面的普通访存如何处理

`waitForward` 在 Dispatch 处检查 ROB 是否为空：

```scala
blockedByWaitForward(0) := !io.enqRob.isEmpty && isWaitForward(0)
```

因此 AMO 不是进入 MemBlock 后再逐个等待旧 load/store，而是在进入后端执行窗口
之前，就要求更老指令已经完成并离开 ROB。对更老 store，AtomicsUnit 在执行前还
要处理“已经提交，但仍留在 SQ/SBuffer/Uncache”的部分：AMO 可以先被 MemBlock 和
AtomicsUnit 接收，之后 AtomicsUnit 再发起 flush，并在真正发送 DCache atomic request
之前等待旧 store 路径排空。

所以前序处理包含两个层次：

```text
后端顺序层：等待更老指令离开ROB
MemBlock内部层：AMO进入后请求排空SQ/SBuffer/Uncache，再访问DCache
```

第二层是 DUT 内部执行行为，不是测试 driver 发射 AMO 的前置条件。

#### 7.2.2 AMO 后面的普通访存是取消还是阻止进入

正常路径下不是“年轻指令已经执行，再把它们取消”。`blockBackward` 会在 Dispatch
组内阻止原子指令后面的 uop 输出；原子进入 ROB 后，`hasBlockBackward` 又阻止后续
指令继续 enqueue。

源码位置：

- `NewDispatch.scala:795-819`
- `Rob.scala:186-193`
- `Rob.scala:393-460`
- `CtrlBlock.scala:707-720`

因此正常时序是：

```text
older普通访存全部完成
        |
        v
AMO独占后端可见窗口并执行
        |
        v
AMO提交离开ROB
        |
        v
younger普通访存才进入后端
```

只有异常、redirect 或 pipeline walk 等恢复场景才涉及取消/清理；那不是建立 AMO
隔离窗口的常规手段。

#### 7.2.3 MemBlock 原子状态额外做了什么

MemBlock 的 `s_atomics` 状态并没有单独扫描并取消所有年轻 load/store。年轻指令
本来就应被后端 Dispatch 屏障挡住。MemBlock 在该状态下主要做局部资源接管：

- 原子 STA 改送 AtomicsUnit，不进入普通 StoreUnit/HybridUnit；
- AtomicsUnit 接管 LoadUnit 0 的 DTLB 端口；
- 暂停 LoadUnit 0 的普通 writeback 接收；
- 禁止硬件 prefetch request；
- 把 AtomicsUnit feedback 转发到当前 STA/HYA issue 入口；
- 断言 LoadUnit 0 没有残留的普通输出。

源码位置：

- `MemBlock.scala:1794-1829`
- `MemBlock.scala:1839-1871`

所以“原子执行期间没有普通访存从后端发射进来”在架构效果上成立，但主要保证者是
Decode/Dispatch/ROB 的串行化协议，MemBlock 状态负责专用端口和局部资源接管。

### 7.3 LR 完成后不会占住 AtomicsUnit 等待 SC

LR 和 SC 是两条独立架构指令、两次独立 AtomicsUnit 事务：

```text
LR：进入AtomicsUnit -> 读数据/建立reservation -> 写回 -> resetFSM
中间普通指令：可以在LR提交后继续执行
SC：之后重新进入AtomicsUnit -> 检查reservation -> 条件写入 -> 写回
```

普通 LR 到达 `s_finish` 后拉高 `io.out.valid`。MemBlock 看到该 valid 时先把自己的
状态恢复为 `s_normal`；AtomicsUnit 只有在写回端真正握手 `io.out.fire` 后才调用
`resetFSM()` 并回到 `s_invalid`。若写回端反压，AtomicsUnit 仍停在 `s_finish`，不能
视为已经空闲。由于后端 `blockBackward` 尚未解除，此时年轻普通访存也不会趁这个
间隙进入。

源码位置：

- `AtomicsUnit.scala:401-430`
- `MemBlock.scala:1818-1820`

LR 与后续 SC 之间保留的不是整个 AtomicsUnit，而只是 DCache MainPipe 中的轻量
reservation 状态：

```scala
val lrsc_count = RegInit(...)
val lrsc_addr  = Reg(UInt())
```

成功 LR 设置 reservation block 地址和计数器；计数器随后逐周期递减。SC 到达时
检查 reservation 是否仍有效且 block 地址匹配。SC 尝试后会清除 reservation。

源码位置：

- `MainPipe.scala:609-644`
- `Parameters.scala:872-876`

reservation 有效期间不会占住 AtomicsUnit、TLB 端口或整个 MemBlock，也不会禁止
所有普通 load/store。它只带来局部 DCache 约束：

- 同 reservation block 的 probe 会被暂缓；
- 本地 release/writeback 命中该 block 会使 reservation 失效；
- 新 LR 可能受 `block_lr` 暂缓，避免 LR storm；
- reservation 超时后自动失效。

源码位置：

- `Probe.scala:87-96`
- `Probe.scala:187-200`
- `DCacheWrapper.scala:1573-1598`

所以 LR/SC 不是“LR 一直锁住资源直到 SC”，而是“LR 先完成并释放执行资源，只在
DCache 保留一个有时限、可失效的 reservation 标记”。

### 7.4 `aq/rl` 当前没有单独控制路径

`aq` 是 acquire 排序位，约束原子操作之后的访存不能越过该原子操作；`rl` 是 release
排序位，约束原子操作之前的访存不能拖到该原子操作之后。两位组合可以表达 relaxed、
acquire、release 和 acquire-release 四种排序强度。

当前代码没有把原子指令的 `aq`、`rl` 位保存为独立 uop 控制字段，也没有在
AtomicsUnit 或 DCache request 中区分四种组合。完整原始指令仍保存在 `DynInst.instr`
中，因此编码位并非从数据结构中消失，但当前 MemBlock/AtomicsUnit 没有读取
`uop.instr(26)` 或 `uop.instr(25)` 来选择不同执行策略。指令匹配模式对相关位采用通配
方式，`LSUOpType` 也只表示操作类型和宽度。

MemBlock 的 `issueSta/issueStd/issueHya` 都使用 `MemExuInput`，接口上不存在独立的：

```text
atomic_aq
atomic_rl
```

AtomicsUnit 向 DCache 发送的 `AtomicWordIO/MainPipeReq` 同样没有独立 `aq/rl` 字段；其
`cmd` 只区分 LR、SC、AMO 运算类型和访问宽度。

因此当前实现可以理解为：

```text
不同 aq/rl 编码进入同一条原子执行路径，
由 noSpec、blockBack、SBuffer flush 和 ROB 顺序共同提供保守排序。
```

测试时应验证四种编码都能执行，但不应假设当前 RTL 会暴露不同的内部时序策略。

因此，当前 MemBlock 接口级测试框架的结论是：

```text
需要：保留原始instr[26:25]，覆盖aq/rl = 00、01、10、11
不需要：按aq/rl选择不同的Dispatch、barrier、issue、flush或DCache request行为
```

四种编码统一复用现有原子 owner、公共 Dispatch admission、atomic issue barrier 和最终
完成检查。测试框架不得凭空增加 DUT 接口上不存在的 `aq/rl` 握手，也不得要求四种组合
产生不同的 MemBlock 内部时序。只有未来 RTL 增加独立字段并真正消费 `aq/rl` 时，才需要
把它们升级为 driver 调度条件和差异化 checker 规则。

源码位置：

- `backend/Bundles.scala:178-223`
- `backend/Bundles.scala:963-979`
- `mem/MemBlock.scala:118-124`
- `mem/pipeline/AtomicsUnit.scala:44-54`
- `mem/pipeline/AtomicsUnit.scala:498-537`

### 7.5 TLB 端口复用约束

AtomicsUnit 使用 LoadUnit 0 的 DTLB port，并且在发起第一个 TLB 请求后暂缓接受响应，以避免把硬件预取产生的响应误当成当前原子请求的响应。

源码位置：

- `AtomicsUnit.scala:238-244`
- `MemBlock.scala:1844-1856`

测试环境必须保留 TLB request/response 的请求归属，不能只看到一个 `resp.valid` 就默认它属于当前原子指令。

### 7.6 DCache 请求不是固定延迟

原子请求可能经历：

- DCache hit；
- cache miss；
- MissQueue 资源不足；
- coherence 权限获取；
- replay；
- refill 后重新执行。

因此测试环境应该使用 request/response 关联和状态机判断完成，不应使用固定若干周期后强制结束。

### 7.7 异常必须按产生阶段检查

对齐、TLB、PMP/PMA/PBMT 和 trigger 等异常在 atomic request 发送前已经确定。此类异常
使 AtomicsUnit 进入 finish 路径，不应为该事务产生 DCache atomic request 或 data write。

DCache ECC/TL 等错误不同：它们随 MainPipe response 返回，此时请求已经进入 DCache，
当前 `data_write.valid` 也没有被 `s3_error_wb` 统一直接门控。因此不能写成“只要出现
DCache error，就一定没有发生 data-array write”；checker 必须按具体错误类型、请求阶段
和 RTL 实际时序验证。

测试检查点包括：

- 异常类型和 `xtval`/异常地址正确；
- request 前异常不产生该事务的 DCache 更新；
- DCache 返回错误按 MainPipe 的真实写入门控和 response 时序检查；
- ROB 能够接收异常写回并触发后续控制流处理；
- redirect 后清理被 flush 的软件 IQ 片段，但不能断言 redirect 会直接 `resetFSM()`；当前
  AtomicsUnit 的 redirect 分支只清除 `atom_override_xtval`。

---

## 8. 当前 mem_ut 支持状态与职责边界

本章只说明当前 `mem_ut/ver/ut/memblock` 的支持状态。后续章节分别给出输入协议、
软件后端模型、DUT 内部检查、参考模型和 responder 规则，避免把所有内容都叫作
“测试框架发射约束”。

### 8.1 当前状态：已有分类骨架，但主动原子流程仍被禁用

当前框架已经定义 AMO/MOU 编码、权重和行为分类，但 V2 scalar 主流程仍明确禁止
原子事务进入完整生命周期。

运行期配置要求：

```systemverilog
if (op_class_amo_wt != 0) begin
  `uvm_fatal("SEQ_CSR_CFG",
             "MEMBLOCK_OP_CLASS_AMO_WT must be 0 until the V2 scalar atomic flow is complete")
end
```

主表和普通 scalar split issue 字段生成也会对 AMO/MOU fail-fast。因此当前必须保持：

```text
MEMBLOCK_OP_CLASS_AMO_WT = 0
手工/random/boundary构造的AMO/MOU在admission前fail-fast
不能只打开权重就宣称原子流程已经受支持
```

源码位置：

- `seq/base_seq_help/seq_csr_common.sv:744-755`
- `seq/base_seq_help/memblock_dispatch_base_sequence.sv:2690-2707`
- `seq/base_seq_help/issue_field_assigner.sv:68-83`
- `seq/base_seq_help/dispatch_monitor_event_adapter.sv:194-201`

### 8.2 已有骨架和主要缺口

框架已经有以下抽象：

- `fuType=MOU` 和 LR/SC/AMO/AMOCAS `fuOpType` 常量；
- `MEMBLOCK_OP_BEHAVIOR_ATOMIC` 分类；
- `need_alloc=0`、`uses_lq=0`、`uses_sq=0`；
- `route_sta=1`、`route_std=1`；
- `commit_is_normal=1`；
- 普通 AMO/LR/SC 的 `1 STA + 1 STD` 描述；
- AMOCAS.W/D 的 `1 STA + 2 STD` 描述；
- AMOCAS.Q 的 `2 STA + 4 STD` 描述；
- issue item 中预留的 `uop_index/uop_count`。

但这些字段目前主要是行为描述，还没有形成端到端原子闭环：

- `issue_queue_scheduler::route_target()` 仍要求 STA/STD 存在 `active_sq_mapped`，而原子
  `uses_sq=0`，直接放开 fail-fast 后仍可能被静默跳过；
- `uop_count` 尚未展开成真实片段；
- `uop_index` 同时承担物理端口和片段编号语义，无法正确表达 AMOCAS；
- 原子没有独立 owner、barrier、片段 mask 和 final writeback 生命周期；
- 现有普通 load/store RM 不能直接表达 RMW、CAS 和 LR/SC reservation。

需要分离三个编号：

```text
issue_port_idx    // 本次从哪个物理issue端口发射
decoded_uop_idx   // 后端拆分得到的DynInst/uop编号
atomic_piece_idx  // fuOpType高位编码的AtomicsUnit槽号
```

### 8.3 规则分类总表

| 规则类别 | 约束或观察对象 | 能否阻止 STA/STD issue |
|---|---|---|
| MemBlock 物理输入协议 | 端口能力、Decoupled 握手、owner、片段编号和 AMOCAS.Q STA 顺序 | 能，非法输入不得驱动 |
| 软件后端模型策略 | Dispatch admission、`lsqCanAccept`、ROB/IQ barrier、redirect 后未发片段清理 | 能，但只在对应后端阶段生效 |
| DUT 内部执行检查 | AtomicsUnit、TLB、排空、DCache request、miss/replay、writeback | 不能，只观察和检查 |
| 架构参考模型 | RMW、CAS、LR/SC reservation、最终内存和寄存器结果 | 不能，只预测和比对 |
| Responder 职责 | TLB、下级存储、一致性、反压和排空推进 | 不能修改 driver 合法输入协议 |

特别注意：`lsqCanAccept`、ROB 为空和 atomic barrier 并不是 MemBlock 物理 issue 接口的
握手信号。它们只在测试框架选择模拟真实 Dispatch/ROB/IQ 行为时，作为软件后端模型
规则存在。

---

## 9. MemBlock 物理输入接口约束

本章只回答：测试框架从 `issueSta/issueStd/issueHya` 向 MemBlock 驱动原子事务时，输入
本身必须满足什么协议。DUT 内部何时排空、何时发 TLB/DCache 请求，不属于本章。

### 9.1 端口能力和 Decoupled 协议

Driver 必须根据当前 DUT 参数生成 MOU-capable port mask，原子 STA 只能投递到真实后端
可路由 `MouCfg` 的端口。当前默认参数在 `STA0/STA1` 配置了 `MouCfg`；MemBlock 的泛化
代码也会扫描 `issueHya`，但这不代表当前真实后端一定能把 MOU 发到任意 HYA 端口。

所有输入必须遵守 Decoupled 协议：

```text
valid && !ready -> valid保持，bits保持稳定
只有valid && ready才能记为该片段已被DUT接收
```

MemBlock 使用 `Mux1H` 将所有原子 `issueSta/issueHya` 汇入 AtomicsUnit，并把同一个
`ready` 返回原子入口。为避免多个入口同周期同时握手却只能选择一个主输入，driver 必须
保证：

```text
每周期所有issueSta + issueHya入口中：原子STA valid最多一个，fire最多一个
```

这也适用于 AMOCAS.Q：槽 0 STA 和槽 2 STA 不能从两个端口同周期 fire。

源码位置：

- `Parameters.scala:469-476`
- `MemBlock.scala:1795-1829`

### 9.2 原子 owner 和单事务输入窗口

测试框架必须为每个动态原子事务建立唯一 owner，建议至少包含：

```text
atomic_owner = uid + rob_key + dynamic_epoch
```

在一个原子输入窗口内：

- 同时只能存在一个 active atomic owner；
- 所有 STA/STD 片段必须属于该 owner；
- 不能只凭 `fuType=MOU` 把两个片段判为同一事务；
- queue key 至少包含 `owner + target(STA/STD) + atomic_piece_idx`；
- 同一片段只能成功握手一次；
- redirect 后已经被软件后端判定 stale 的片段不得再次 issue。

AtomicsUnit 没有多个动态原子事务的 `robIdx` 匹配表。正常激励必须由 driver 保证不跨
owner 混配；跨 owner STA/STD 只能作为非法协议负向测试。

### 9.3 STA/STD 片段数量和槽号

| 指令类型 | STA 片段 | STD 片段 | 数据含义 |
|---|---:|---:|---|
| LR/SC/普通 AMO | 1 | 1 | 统一的地址和数据副本；LR 内部请求不等待 STD |
| AMOCAS.W/D | 1 | 2 | 比较值 `rd`、候选新值 `rs2` |
| AMOCAS.Q | 2 | 4 | `rd/rs2/rd+1/rs2+1` 四个 64-bit 数据片 |

必须满足：

```text
普通AMO/LR/SC：STA槽0一次，STD槽0一次
AMOCAS.W/D：STA槽0一次，STD槽0/1各一次
AMOCAS.Q：STA槽0先于STA槽2
AMOCAS.Q：STD槽0/1/2/3可任意顺序，但同owner、不可重复、最终完整
```

AMOCAS.Q 的槽 0 STA 携带主地址/uop 和低半返回 `pdest`，负责启动 AtomicsUnit；槽 2
只补充高半返回 `pdest`。若 AtomicsUnit 空闲时先接收槽 2，其带槽号的 `fuOpType` 不能
通过主 uop 的精确 `isAMOCASQ` 判断，后续槽 0 也无法补救。因此 `slot 0 -> slot 2` 是
输入协议硬约束，不是性能建议。

### 9.4 成对 enqueue 与独立 issue

对于一个普通原子 `DynInst`，Memory Scheduler 在同一次 scheduler enqueue 中产生对应的
STA 和 STD 副本，不能等 STA 已经 issue 后才创建 STD。进入 IQ 后，两侧按各自源操作数
ready 独立发射，因此必须允许：

```text
STA先到、STD后到
STD先到、STA后到
STA和STD同周期从不同类型端口到达
多个STD跨周期、跨STD端口到达
```

这里不能写成“一个原子 owner 的全部 2/4 个 AMOCAS 片段必须同周期 enqueue”。AMOCAS
由多个 split `DynInst` 组成，不同 split uop 可以跨周期进入 IQ；约束是第一片进入真实 IQ
前已经建立同一个 owner/barrier，后续片段不能换 owner、重复或越过年轻事务。

源码位置：

- `backend/issue/Scheduler.scala:372-384`
- `backend/issue/Scheduler.scala:461-509`
- `mem/pipeline/AtomicsUnit.scala:73-180`

### 9.5 Driver 不得读取的 DUT 内部条件

以下条件都不能成为合法 STA/STD issue 的前置 gate：

```text
SQ/SBuffer/Uncache是否为空
stIsEmpty是否为1
TLB是否完成，PMP/PMA/PBMT是否通过
STD是否已经全部收齐
DCache atomic request是否出现
MainPipe是否正在处理probe/refill/miss/replay
LR reservation是否有效
aq/rl采用哪一种组合
原子进入IQ后lsqCanAccept是否再次变低
```

Driver 只负责提供合法输入；DUT 必须自己等待内部条件。否则测试会绕开“AMO 被接收后
能否正确请求排空、等待数据并处理反压”的真实功能。

### 9.6 输入协议断言

建议直接实现：

```text
atomic_sta_valid_count(issueSta + issueHya) <= 1
atomic_sta_fire_count(issueSta + issueHya)  <= 1

atomic_input_port -> mou_capable_port_mask[issue_port_idx]
atomic_active     -> incoming_piece.owner == atomic_owner
atomic_piece_fire -> !received_mask[target][atomic_piece_idx]
valid && !ready   -> $stable(bits)

amocas_q_first_sta_fire  -> sta_slot == 0
amocas_q_sta_slot_2_fire -> sta_slot_0_already_accepted
amocas_q_std_fire        -> slot inside {0,1,2,3}
amocas_q_std_fire        -> same_owner && !std_received_mask[slot]
```

不得实现：

```text
atomic_sta_issue_fire -> stIsEmpty
atomic_std_issue_fire -> stIsEmpty
atomic_issue_fire     -> tlb_done && dcache_ready
atomic_active         -> no_probe && no_refill
```

---

## 10. 软件后端模型约束

本章适用于测试框架模拟真实 Dispatch、ROB 和 IQ 的部分。它解释“什么事务可以进入真实
IQ 镜像”，不是 MemBlock 物理输入接口自身的 ready/valid 条件。

### 10.1 Dispatch/admission 与 `lsqCanAccept`

AMO 自身不分配普通 LQ/SQ entry，但真实 Dispatch 的 `fromRename.ready` 仍经过公共
`lsqCanAccept`。因此软件模型应分阶段处理：

```text
Dispatch/admission进入真实IQ之前：检查公共modeled_lsqCanAccept
原子片段已经进入IQ之后：STA/STD issue不再检查lsqCanAccept
```

当前配置的稳态公共 headroom 是：

1. `sq_free_count < 4` 时，Load、Store、AMO 都不能完成 Dispatch admission；
2. `lq_free_count < 6` 时，Load、Store、AMO 同样阻塞；
3. 两侧恢复公共 headroom 后，AMO admission 可以成功；
4. AMO admission 不改变 LQ/SQ pointer 和 free count；
5. AMO 进入 IQ 后，即使软件 LSQ 状态变化，也不能重新用 `lsqCanAccept` 阻止其片段 issue。

如果模型支持同拍多条混合 Dispatch，还必须计算本拍 Load/Store 的实际预分配，不能把
固定 `6/4` 阈值当成完整的逐拍 admission 算法。周期精确模型还需考虑 `RegNext` 和
redirect 对影子计数更新的延迟；非周期精确模型至少要保留上述阶段边界。

源码位置：

- `backend/dispatch/NewDispatch.scala:446-451`
- `backend/dispatch/NewDispatch.scala:688-700`
- `mem/lsqueue/LSQWrapper.scala:385-415`

### 10.2 原子 barrier 的建立

`waitForward` 表示原子开始 Dispatch/admission 前，更老架构 owner 已经离开 ROB。框架
必须在第一原子片段进入真实 IQ 镜像前建立 barrier，不能等 STA/STD 已经 fire 后才建立。

```text
atomic_barrier_open
  -> no_older_uncommitted_rob_owner
  -> no_older_live_issue_item
  -> no_older_execution_waiting_for_writeback
```

这里不包含已经提交、但仍残留在 DUT StoreQueue/SBuffer/Uncache 的旧 store。它们由 DUT
内部排空，不能反向阻止原子输入。

### 10.3 barrier 期间的年轻事务

`blockBackward` 对软件模型的含义是：

```text
atomic_barrier_active
  -> no_younger_backend_iq_enqueue
  -> no_younger_memory_issue_fire
  -> no_second_atomic_owner_iq_enqueue_or_issue
```

年轻事务可以预生成并保存在主表或 `pre-route backlog`，但不能进入被定义为真实硬件 IQ
镜像的队列，也不能在 barrier 解除前产生 issue fire。

### 10.4 barrier 解除和 redirect

正常路径下，barrier 在软件后端模型确认该原子离开 ROB、到达 normal commit/terminal
之后解除，不能在 STA fire、STD fire、数据收齐、第一次 DCache request 或只看到 final
AtomicWB 时提前解除。

redirect 路径必须区分软件状态和 DUT 内部状态：

- 软件后端清理被 `robIdx.needFlush()` 命中的尚未发射 IQ 片段，并禁止 stale epoch 再 issue；
- barrier 是否解除取决于被 flush 的软件 owner 是否已经从模型中清除；
- 不能据此断言 redirect 会直接取消正在执行的 AtomicsUnit 事务；
- 当前 `AtomicsUnit` 的 redirect 逻辑只清除 `atom_override_xtval`，没有调用 `resetFSM()`；
- 在途原子如何收敛必须按 RTL 实际输出检查，不能由 driver 擅自丢弃 DUT 响应。

源码位置：

- `backend/dispatch/NewDispatch.scala:795-819`
- `backend/rob/Rob.scala:186-193`
- `backend/rob/Rob.scala:393-460`
- `mem/pipeline/AtomicsUnit.scala:420-431`

### 10.5 LQ/SQ owner 与完成状态

原子不创建普通 LQ/SQ owner：

```text
atomic_uses_lq = 0
atomic_uses_sq = 0
atomic_has_lq_owner = 0
atomic_has_sq_owner = 0
```

因此不能用普通 `lqIdx/sqIdx`、`sqDeq` 或 `scommit` 判断原子完成。软件状态建议独立表达：

```text
atomic_owner_active
atomic_sta_enqueued_mask
atomic_std_enqueued_mask
atomic_sta_issued_mask
atomic_std_issued_mask
atomic_final_writeback_seen
atomic_normal_commit_seen
atomic_terminal
```

DCache request、replay、reservation 等是 DUT/RM 状态，不应放入 issue scheduler 的
发射前置条件。

### 10.6 `aq/rl`

当前 MemBlock、AtomicsUnit 和 DCache atomic request 没有独立 `aq/rl` 控制接口，也未读取
`DynInst.instr[26:25]` 改变内部时序。因此软件后端模型只保留原始编码，不根据四种组合
改变 admission、barrier、issue、flush 或 DCache 行为。

`aq/rl=00/01/10/11` 可以作为可选的编码穿透覆盖，但不是 MemBlock 功能覆盖硬要求。未来
RTL 真正增加并消费独立字段后，再升级成差异化调度和 checker 规则。

### 10.7 软件后端断言

```text
dispatch_admission && !modeled_lsqCanAccept
  -> no_backend_iq_enqueue

amo_dispatch_admitted
  -> lq_pointer_unchanged && sq_pointer_unchanged
  -> lq_free_count_unchanged && sq_free_count_unchanged

first_atomic_piece_iq_enqueue
  -> atomic_barrier_already_active

atomic_barrier_active
  -> no_younger_backend_iq_enqueue
  -> no_younger_memory_issue_fire
  -> no_second_atomic_owner

atomic_piece_already_in_iq && !modeled_lsqCanAccept
  -> issue仍只由正常IQ ready/端口仲裁决定

redirect_flushes_owner
  -> stale_unissued_iq_pieces_removed
  -> stale_piece_never_issue
```

---

## 11. DUT 内部原子运行行为与 Checker 规则

本章描述 MemBlock 接收合法 STA/STD 之后，RTL 内部必须完成的工作。这些规则由白盒
checker 或 monitor 验证，不得反向控制 driver。

### 11.1 原子识别与数据收集

- 原子 STA 被识别后进入 `s_atomics` 路径，不送入普通 StoreUnit；
- STD 根据 `fuOpType` 高位写入相应原子数据槽；
- LR 的 DCache 请求不等待 `data_valid`；
- SC、普通 AMO 和 AMOCAS 必须等所需数据片齐全后才能发送 DCache request；
- `data_valid -> required_atomic_piece_mask_complete` 是 DUT/RM checker，不是 driver gate；
- 普通 store STD 必须继续走普通 store-data 路径，不能误送 AtomicsUnit。

### 11.2 TLB、权限和请求类型

Checker 应验证：

- LR 使用原子读权限类型，SC/AMO/AMOCAS 使用原子写权限类型；
- 地址对齐、TLB、PMP/PMA/PBMT 和 trigger 检查在 DCache request 前完成；
- 这些前端异常可以直接进入 finish，不要求一定观察到 DCache atomic request；
- TLB miss 和响应延迟不能按固定周期假设。

### 11.3 旧 store 路径排空

原子 STA 可以在旧 store 路径非空时被 MemBlock 接收。接收后 AtomicsUnit 发起 flush 并
等待内部聚合空状态：

```scala
stIsEmpty = sbuffer.io.flush.empty && uncache.io.flush.empty
```

其中 `sbuffer.io.flush.empty` 本身由 `empty && io.sqempty` 经寄存路径生成，已经包含 SQ
排空条件。因此 checker 应按 RTL 的聚合信号及其寄存延迟检查，不要发明同周期的
`sq_empty && sbuffer_empty && uncache_empty` 三个独立接口条件。

正确检查关系是：

```text
atomic_sta_accepted && !stIsEmpty -> legal
atomic_flush_wait && !stIsEmpty   -> flush_request按RTL延迟出现
atomic_dcache_req.fire            -> AtomicsUnit观察到合法的stIsEmpty条件
```

源码位置：

- `MemBlock.scala:1781-1792`
- `MemBlock.scala:1839-1840`
- `sbuffer/Sbuffer.scala:550-552`
- `mem/pipeline/AtomicsUnit.scala:292-301`

### 11.4 DCache MainPipe、miss 和 replay

DCache MainPipe 负责读取旧值、取得一致性权限、执行 AMOALU/CAS 比较、条件写入并返回
旧值或 SC 结果。原子请求与 probe、refill、普通 store 在 MainPipe 仲裁，原子 active
不代表整个 DCache 冻结。

需要区分：

- 普通 AMO/LR miss 可以进入 MissQueue，资源不足时返回 replay；
- SC 在 S2 被直接允许进入 S3，`!s3_hit` 直接参与 `s3_sc_fail`，不能套用“所有原子 miss
  都进入 MissQueue”的描述；
- 同一 owner 的内部重试可以产生多次 request，但只能形成一次架构内存更新；
- 当前实际 replay 路径以 AtomicsUnit、MainPipe 和 MissQueue 为准；源码中的
  `AtomicsReplayEntry` 类没有被当前设计实例化，不能据它制定 checker。

源码位置：

- `cache/dcache/mainpipe/MainPipe.scala:403-407`
- `cache/dcache/mainpipe/MainPipe.scala:472-489`
- `cache/dcache/mainpipe/MainPipe.scala:614-624`
- `mem/pipeline/AtomicsUnit.scala:347-369`

### 11.5 异常和写入检查

异常必须分阶段判断，不能统一写成“任何 DCache error 都禁止 data array 写入”：

- 对齐、TLB、PMP/PMA/PBMT、trigger 等在 atomic request 前确定的异常，不应产生该原子的
  DCache data write；
- DCache ECC/TL 等错误是在 MainPipe 已执行后随 response 返回，当前 `data_write.valid`
  并没有被 `s3_error_wb` 统一直接门控，checker 必须按具体错误类型和 RTL 时序检查；
- SC/CAS 比较或 reservation 失败不得产生部分写入；
- replay 不得导致重复写入或重复 final writeback。

### 11.6 Atomic writeback 和事务收敛

Checker 应跟踪：

```text
atomic_req_count
atomic_replay_pending
atomic_wb_count
atomic_final_writeback
atomic_internal_terminal
```

普通 AMO/LR/SC 最终产生一个合法 MOU 架构结果；AMOCAS.Q 产生低、高两次合法结果
writeback。STA fire、STD fire、数据收齐、TLB 完成、第一次 request 或 miss/replay 都不能
单独视为事务完成。

redirect 不能简单断言 AtomicsUnit 立即回到 idle。软件 IQ stale 片段清理属于第 10 章；
DUT 在途状态按实际 FSM 和最终输出检查。

### 11.7 LR/SC reservation 的微架构检查

LR 成功后只在 DCache MainPipe 保存有限时 reservation，不占住 AtomicsUnit、TLB 端口或
整个 MemBlock。LR 与后续 SC 之间允许执行普通访存。

DUT checker 应按微架构时机观察：

- 成功 LR 建立 cache-block 粒度 reservation；
- reservation 因计数器超时、本地 cache-line release/writeback、复位而失效；
- SC 进入 MainPipe S3 时消费 reservation，无论最终成功还是失败；
- 同 reservation block 的 probe 在当前实现中由 ProbeQueue 暂缓，不是 probe 出现当拍
  直接清 reservation；
- cache-line release/writeback 与整数寄存器 writeback/AtomicWB 不是同一事件。

这里“SC 在 S3 消费 reservation”是 DUT 微架构检查；架构 RM 在最终 SC 结果处更新软件
可见的 reservation 状态，两者不能混成一个周期断言。

---

## 12. 架构参考模型规则

### 12.1 AMO 和 CAS

普通 AMO 必须建模成一次不可分割的 RMW：

```text
old = memory[address]
new = op(old, rs2)
memory[address] = new
rd = old
```

AMOCAS：

```text
old = memory[address]
if old == compare:
  memory[address] = newValue
else:
  memory保持不变
rd = old
```

参考模型只在确认事务形成最终架构结果时提交一次更新，不能在 STA fire、STD fire 或
第一次 DCache request 时提前更新。W 操作在 RV64 下还要检查返回值符号扩展。

### 12.2 LR/SC

架构 RM 建议维护：

```text
reservation_valid
reservation_block_addr
reservation_expire_state
```

规则为：

```text
LR成功          -> 返回old，建立reservation，不写memory
SC成功          -> 写rs2，rd=0，reservation在架构模型中结束
SC失败          -> memory不变，rd=1，reservation在架构模型中结束
没有新LR再次SC -> 继续失败
```

内部 replay 不是 SC 架构失败：

- `atomic_replay_pending` 表示同一个 SC 仍在硬件内部重试；
- 只有 final writeback `rd=1` 才表示软件可见的 SC 失败；
- replay 过程中不能重复更新 memory、清两次 owner 或产生重复最终结果。

### 12.3 结果模型不得限制输入

RMW 计算、reservation 有效性、SC/CAS 成败、DCache error 和 writeback 次数只能决定期望
结果，不能决定 driver 是否允许发出一个本来合法的 STA/STD 片段。

---

## 13. Responder 职责与覆盖计划

### 13.1 Responder 职责

环境应通过 DUT 实际暴露的边界提供合法响应和反压：

- TLB responder 支持 miss、重复请求和非固定响应延迟；
- 下级存储 responder 支持 miss、refill 和资源反压；
- 一致性 responder 允许 probe/release 流程正常推进；
- SBuffer/Uncache flush 期间不能人为阻止旧事务排空；
- 原子执行期间不能强行禁止 DCache 内部 probe、refill、replace/writeback。

若 `probe_req/refill_req` 只是 DCache 内部接口而不是 mem_ut 顶层边界，responder 应驱动
真实外部一致性接口，白盒 checker 只观察内部 probe/refill，不能越层直接驱动内部信号。

### 13.2 输入协议覆盖

- 合法 MOU-capable STA 端口分别发射；
- 非 MOU-capable 端口作为负向协议测试；
- 同周期尝试两个原子 STA 入口，checker 必须报错；
- STA 先、STD 后；STD 先、STA 后；二者同周期；
- AMOCAS 多片段跨周期、跨 STD 端口到达；
- AMOCAS.Q STA 槽 0 后槽 2；槽 2 抢先作为负向测试；
- AMOCAS.Q 四个 STD 的多种排列、重复槽、缺槽和跨 owner；
- `valid && !ready` 时 bits 稳定；
- redirect 后 stale 软件 IQ 片段不再 issue。

### 13.3 软件后端模型覆盖

- `sq_free_count < 4` 时 Load、Store、AMO admission 被公共 gate 阻塞；
- `lq_free_count < 6` 时同样阻塞；
- headroom 恢复后 AMO admission 成功且不改变 LQ/SQ pointer/free count；
- AMO 进入 IQ 后再拉低 `modeled_lsqCanAccept`，已入队 STA/STD 仍可正常 issue；
- 第一原子片段进入真实 IQ 前 barrier 已建立；
- barrier active 时年轻事务可以留在 pre-route backlog，但不能进入真实 IQ；
- normal commit 后放行年轻事务；
- redirect 只清软件 stale 片段，不假设 AtomicsUnit 同周期复位。

### 13.4 DUT 内部与结果覆盖

- AMO 在 SQ/SBuffer/Uncache 非空时仍先被接收，随后观察 flush、排空和 atomic request；
- 前端地址/权限异常不产生 atomic request；
- 普通 AMO/LR 的 hit、miss、MissQueue 和 replay；
- SC hit 成功、reservation 无效、地址不匹配和 miss 失败；
- replay 后 memory 只更新一次，最终 writeback 次数正确；
- LR 后插入普通访存，再执行成功或失败 SC；
- reservation 超时、本地 cache-line release/writeback、SC 消费；
- 同 block probe 被暂缓并在 reservation 解除后继续；
- AMOCAS.W/D/Q 比较成功和失败；
- W 返回值的 RV64 符号扩展；
- DCache 内部 probe/refill 与原子请求仲裁；
- `aq/rl` 四组合仅作为可选编码穿透覆盖，不要求差异化 MemBlock 行为。

### 13.5 约束审计结论

| 原规则或需求 | 结论 | 正确归属 |
|---|---|---|
| STA/STD 同 owner、槽号唯一 | 保留 | MemBlock 输入协议 |
| 一个普通原子 DynInst 的 STA/STD 成对 enqueue | 保留 | 软件 Scheduler 模型 |
| 一个 AMOCAS owner 的全部片段必须同周期 enqueue | 取消 | split DynInst 可跨周期，但 owner/barrier 必须一致 |
| 所有原子 STA/HYA 入口同周期可各发一个 | 取消 | 合计最多一个原子 STA valid/fire |
| 任意 STA/HYA 端口都可发 MOU | 取消 | 按 DUT 参数生成 MOU-capable port mask |
| 原子 barrier 在第一片进入真实 IQ 前建立 | 保留 | 软件后端模型 |
| 年轻事务不能预生成或保存在 backlog | 取消 | 可留在 pre-route backlog，不得进入真实 IQ |
| 原子 issue 前等待 SQ/SBuffer/Uncache 为空 | 取消 | DUT 接收后自行 flush 和等待 |
| `lsqCanAccept=0` 阻止 AMO admission | 保留 | 公共 Dispatch/admission gate |
| AMO 入 IQ 后继续用 `lsqCanAccept` 阻止 issue | 取消 | 不得在 issue 阶段重复检查 |
| AMO 分配普通 LQ/SQ entry | 取消 | allocation 数为 0，独立 owner 生命周期 |
| AMOCAS.Q STA 槽 0 先于槽 2 | 保留 | MemBlock 输入协议 |
| AMOCAS.Q STD 必须 0/1/2/3 固定顺序 | 取消 | 任意顺序，同 owner、唯一、最终完整 |
| redirect 直接取消 AtomicsUnit 并清空 FSM | 取消 | 只清软件 stale 片段；DUT 按实际 FSM 检查 |
| 原子 active 时禁止 probe/refill/replay | 取消 | Responder 继续推进，checker 验证仲裁 |
| LR 一直占住 AtomicsUnit 等待 SC | 取消 | LR 完成释放执行资源，仅保留 DCache reservation |
| 任意 DCache error 都禁止 data write | 取消 | 按错误产生阶段和 RTL 实际门控检查 |
| `aq/rl` 改变 MemBlock 发射/flush 时序 | 取消 | 当前只做可选编码覆盖 |

---

## 14. 一句话总结

测试框架真正必须限制的是：**把什么样的 STA/STD、从哪个合法端口、以什么 owner、槽号、
顺序和握手方式送进 MemBlock，以及软件后端模型何时允许它们进入 IQ。**

SQ/SBuffer/Uncache 排空、TLB/权限、DCache request、miss/replay、reservation、原子计算和
writeback 都是 **MemBlock 接收输入后的 DUT 内部行为**，只能由 checker、参考模型和
responder 验证，不能被反向写成 driver 发射前置条件。
