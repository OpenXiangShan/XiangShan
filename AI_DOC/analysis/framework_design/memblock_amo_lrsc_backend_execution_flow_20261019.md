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
4. miss 时进入 MissQueue 获取 cache line 和写权限；
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

MainPipe 使用 tag 命中和 coherence permission 共同生成 `s2_hit`，AMO 命中后
才进入 S3。没有数据或写权限时进入 MissQueue；资源不足时返回 replay。

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

AtomicsUnit 接收到 DCache response 后：

- `miss && replay`：回到 `s_cache_req`，重新发送；
- `miss` 但已进入 MissQueue：等待 MissQueue 完成后重新推进；
- 非 miss：锁存数据、错误信息和 response id，进入写回阶段。

源码位置：

- `AtomicsUnit.scala:337-369`
- `AtomicsReplayUnit.scala:64-127`
- `DCacheWrapper.scala:1467-1472`

因此原子操作不是“一次请求必然一次响应”。测试环境必须允许：

```text
请求 -> miss -> replay -> 再请求 -> response
```

而不能以单周期或固定延迟模型判断失败。

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

当前代码中没有把原子指令的 `aq`、`rl` 位保存为独立 uop 字段，也没有在 AtomicsUnit 或 DCache request 中区分四种组合。指令匹配模式对相关位采用通配方式，`LSUOpType` 也只表示操作类型和宽度。

因此当前实现可以理解为：

```text
不同 aq/rl 编码进入同一条原子执行路径，
由 noSpec、blockBack、SBuffer flush 和 ROB 顺序共同提供保守排序。
```

测试时应验证四种编码都能执行，但不应假设当前 RTL 会暴露不同的内部时序策略。

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

### 7.7 异常优先于 DCache 更新

对齐、TLB、PMP/PMA/PBMT、trigger 或 DCache 错误发生时，AtomicsUnit 会设置异常并直接进入 finish 路径，不应该继续把普通 AMO 新值写进 DCache。

测试检查点应同时确认：

- 异常类型正确；
- `xtval`/异常地址正确；
- 没有产生错误的 data array 写入；
- ROB 能够接收异常写回并 redirect；
- AtomicsUnit 能在 redirect 后清理内部状态。

---

## 8. 当前 mem_ut 测试框架的原子支持边界和职责划分

本章只描述当前 `mem_ut/ver/ut/memblock` 测试框架的真实支持边界和后续实现必须
遵守的约束，不把 RTL 已支持等同于 UVM 已经闭环支持。

这里必须先区分三类规则，不能都写成“测试框架发射约束”：

| 类型 | 作用对象 | 是否限制 issue driver |
|---|---|---|
| 后端发射协议约束 | `issueSta/issueStd/issueHya` 上的 owner、片段、顺序和有效周期 | 是 |
| DUT 内部行为检查 | AtomicsUnit 状态、flush、TLB、DCache request、replay、writeback | 否，只观察和检查 |
| 参考模型结果规则 | RMW、LR/SC reservation、异常、最终内存和寄存器结果 | 否，用于预测和比对 |

当前框架模拟的是后端向 MemBlock 发射 uop 的行为。因此合法激励只需要满足后端在
issue 接口上能产生的协议；不能要求 driver 预先替 MemBlock 完成内部排空、TLB、权限
检查或 DCache 仲裁。

本章审计后明确取消下面这个错误 driver 约束：

```text
错误约束：SQ/SBuffer/Uncache非空时禁止发射AMO
```

SQ/SBuffer/Uncache 排空属于 AtomicsUnit 接收 AMO 后的内部流程，不能作为 STA/STD issue
前置条件。`lsqCanAccept` 则必须分阶段处理：如果框架模拟真实后端 Dispatch/admission，
Load、Store 和 AMO 进入 IQ 前都要经过公共 `modeled_lsqCanAccept`；原子片段一旦完成
admission 并进入 IQ，后续 STA/STD issue fire 不得再次使用 `lsqCanAccept` 进行阻塞。

### 8.1 当前状态：有分类骨架，但主动原子流程被显式禁用

当前框架已经定义 AMO/MOU 编码、权重和行为分类，但 V2 scalar 主流程仍明确禁止
原子事务进入完整生命周期。

第一层禁止在运行期配置：

```systemverilog
if (op_class_amo_wt != 0) begin
  `uvm_fatal("SEQ_CSR_CFG",
             "MEMBLOCK_OP_CLASS_AMO_WT must be 0 until the V2 scalar atomic flow is complete")
end
```

第二层禁止在主表校验：

```systemverilog
if (tr.op_class == MEMBLOCK_OP_CLASS_AMO ||
    tr.fuType == MEMBLOCK_FUTYPE_MOU ||
    tr.lsq_flow == MEMBLOCK_LSQ_FLOW_ATOMIC) begin
  `uvm_fatal(..., "requested unsupported scalar atomic")
end
```

第三层禁止在普通 scalar split issue 字段生成：

```systemverilog
if (main_tr.fuType == MEMBLOCK_FUTYPE_MOU || behavior.is_atomic) begin
  `uvm_fatal(..., "atomic/MOU issue ... is outside scalar split issue scope")
end
```

源码位置：

- `seq/base_seq_help/seq_csr_common.sv:744-755`
- `seq/base_seq_help/memblock_dispatch_base_sequence.sv:2690-2707`
- `seq/base_seq_help/issue_field_assigner.sv:68-83`
- `seq/base_seq_help/dispatch_monitor_event_adapter.sv:194-201`

因此当前约束是：

```text
MEMBLOCK_OP_CLASS_AMO_WT 必须为 0
手工/random/boundary 构造的 AMO/MOU 必须在 admission 前 fail-fast
不能仅打开权重就宣称已经支持原子指令
```

### 8.2 当前已有的原子行为骨架

尽管主动流程被禁用，框架已经有以下抽象：

- `fuType=MOU` 和各类 LR/SC/AMO/AMOCAS `fuOpType` 常量；
- `MEMBLOCK_OP_BEHAVIOR_ATOMIC` 分类；
- `need_alloc=0`、`uses_lq=0`、`uses_sq=0`；
- `route_sta=1`、`route_std=1`；
- `commit_is_normal=1`；
- 普通 AMO/LR/SC 的 `1 STA + 1 STD` 数量描述；
- AMOCAS.W/D 的 `1 STA + 2 STD` 数量描述；
- AMOCAS.Q 的 `2 STA + 4 STD` 数量描述；
- issue item 中预留的 `uop_index/uop_count`。

源码位置：

- `memblock_op_behavior_util.sv:55-88`
- `memblock_op_behavior_util.sv:176-198`
- `issue_queue_scheduler.sv:106-119`
- `issue_queue_scheduler.sv:200-225`
- `memblock_issue_dispatch_base_sequence.sv:303-340`
- `memblock_dispatch_types.sv:765-810`

这些字段目前主要是“行为描述”，并没有形成端到端原子闭环。尤其 `uop_count`
只被写入一个 issue item，尚未展开成多个真实子 uop。

此外还有两个直接阻塞点：

- `issue_queue_scheduler::route_target()` 当前要求 STA/STD 必须存在
  `active_sq_mapped`，而原子事务明确 `uses_sq=0`，所以即使删除 AMO fail-fast，原子
  STA/STD 仍会被静默跳过；
- 当前 `uop_index` 同时被当作 issue 物理端口/元素编号使用，不能直接承担 AMOCAS
  原子槽号语义。

建议把身份拆开表达：

```text
issue_port_idx    // 本次从哪个物理 issue 端口发射
decoded_uop_idx   // 前端 split uopIdx
atomic_piece_idx  // fuOpType 高位编码的 AtomicsUnit 槽号
```

### 8.3 后端 issue 侧的 STA/STD owner 对应约束

后续开放原子激励时，测试框架必须为每条原子事务建立唯一 owner。建议身份至少由：

```text
uid + rob_key + dynamic_epoch
```

组成，并在 STA、STD、feedback、replay、atomic writeback 和 redirect 清理全过程保持
一致。

必须允许：

```text
同一owner：STA先到、STD后到
同一owner：STD先到、STA后到
AMOCAS同一owner的多个STD跨周期、跨端口到达
AMOCAS.Q的STD槽0/1/2/3按任意顺序到达
```

必须禁止：

```text
AtomicsUnit已接收STA(owner A)，随后向其发送STD(owner B)
同一atomic_piece_idx重复发送
缺少某个必需atomic_piece_idx却提前认为data ready
redirect后的旧dynamic_epoch子uop污染新实例
```

当前 RTL 依赖后端串行化避免跨 AMO 混配，AtomicsUnit 本身没有完整 `robIdx` 匹配
表。因此 UVM issue scheduler 不应只检查 `fuType=MOU`，而应保证实际驱动到接口上的
STA/STD 属于同一个原子 owner。故意构造跨 owner 输入只能作为非法协议负向测试，不能
混入正常随机激励。

建议增加的 issue 协议 checker/assertion：

```text
atomic_active -> incoming_std.owner == atomic_owner
atomic_active -> incoming_sta.owner == atomic_owner
each_required_atomic_piece_idx_received_once
new_atomic_owner -> previous_atomic_terminal_or_flushed
amocas_q_first_sta_slot == 0
amocas_q_sta_slot_2 -> sta_slot_0_already_accepted
amocas_q_std_fire -> piece_idx inside {0,1,2,3}
amocas_q_std_fire -> same_owner && !piece_idx_already_received
```

`data_valid -> required_atomic_piece_mask_complete` 属于 DUT/RM 数据收集检查，不是 issue
driver 的输入约束。这里需要明确区分：AMOCAS.Q 的两个 STA 存在严格顺序要求，必须
`slot 0 -> slot 2`；四个 STD 没有固定先后顺序，可以是任意排列，但每个槽必须恰好
接收一次，并且全部属于当前原子 owner。

### 8.4 子 uop 数量和完成条件约束

框架当前的 `atomic_sta_uop_count/atomic_data_uop_count` 必须真正驱动 queue 展开和
完成计数，不能只作为日志字段。

| 类型 | STA 数量 | STD 数量 | STD 数据含义 |
|---|---:|---:|---|
| LR/SC/普通 AMO | 1 | 1 | 统一 split 数据槽；LR 功能请求不等待它 |
| AMOCAS.W/D | 1 | 2 | `rd` 比较值、`rs2` 替换值 |
| AMOCAS.Q | 2 | 4 | `rd/rs2/rd+1/rs2+1` 四个 64-bit 片段 |

需要修正当前框架的行为包括：

- `route_target()` 按 `uop_count` 展开多个 item；
- 原子 STA/STD 绕开普通 `active_sq_mapped` gate，使用独立 atomic owner 生命周期；
- queue 查重 key 加入 `atomic_piece_idx`，允许同 owner/target 的多个合法片段；
- 分离 `issue_port_idx`、`decoded_uop_idx` 和 `atomic_piece_idx`，不能把槽号当物理端口号；
- fire 一个子 uop 不能立即把整个 STA/STD target 标为 dispatched；
- 每个 target 使用 count 或 bitmask 跟踪已发射片段；
- atomic final completion 以 AtomicsUnit 最终 writeback 为准，不以 STD issue accept 为准；
- AMOCAS.Q 必须等待两次结果 writeback 后再完成整个架构指令；
- AMOCAS.Q 必须先发槽 0 STA，再允许槽 2 STA 补充第二个 `pdest`；
- AMOCAS.Q 的 STD 槽 0/1/2/3 允许乱序发射，但 queue/RM 必须使用 received mask 防止
  重复、遗漏和跨 owner 混配。

相关当前缺口位置：

- `issue_queue_scheduler.sv:200-225`
- `issue_queue_scheduler.sv:665-784`
- `common_data_transaction.sv:8357-8429`
- `common_data_transaction.sv:8821-8837`
- `AI_DOC/plan/test_framework/plan/undo/atomic_multi_uop_todo_20260614.md`

### 8.5 issue 模型不创建普通 LQ/SQ owner

原子事务必须与普通 load/store 生命周期分离：

| 普通路径假设 | 原子路径要求 |
|---|---|
| load 分配 LQ | 原子不分配 LQ |
| store 分配 SQ | 原子不分配 SQ |
| store 通过 `scommit` 释放 | 原子不产生普通 `scommit` |
| STD writeback 表示 store data 完成 | 原子 STD 只是提供操作数 |
| LDA0 返回表示 load 完成 | 原子不能进入普通 LOAD/LQ 生命周期 |
| load/store deq 负责资源回收 | 原子由 final MOU writeback 和 ROB normal commit 收敛 |

因此 status/RM 需要独立表达：

```text
atomic_sta_pieces_ready
atomic_std_pieces_ready
atomic_req_sent
atomic_replay_pending
atomic_final_writeback
atomic_terminal
```

不能复用普通 `lqIdx/sqIdx` 是否存在来判断原子事务是否完成。

必须明确：原子 issue item 没有普通 LQ/SQ owner：

```text
atomic_uses_lq = 0
atomic_uses_sq = 0
atomic_has_lq_owner = 0
atomic_has_sq_owner = 0
```

当前测试框架虽然最终从 MemBlock issue 接口驱动 DUT，但软件模型还承担后端
Dispatch/admission 和 IQ 发射行为，因此必须把 `lsqCanAccept` 放在正确的阶段：

```text
Dispatch/admission进入真实IQ模型之前：检查公共modeled_lsqCanAccept
原子片段已经进入IQ之后：STA/STD issue fire不再检查lsqCanAccept
```

真实 RTL 的 `lsqCanAccept` 同时要求 LQ 和 SQ 保留最大单拍分配宽度对应的公共
headroom。当前配置中 `lqMaxAllocate=6`、`sqMaxAllocate=4`。对自身分配数为 0 的 AMO，
稳态建议行为是：

1. `sq_free_count < 4` 时，Load、Store、AMO 都不能完成 Dispatch admission；
2. `lq_free_count < 6` 时，Load、Store、AMO 同样全部阻塞；
3. LQ、SQ 两侧都恢复公共 headroom 后，AMO admission 可以成功；
4. AMO admission 不改变任何 LQ/SQ pointer 和 free count，因为 AMO 的 allocation 数为 0；
5. AMO 已进入 IQ 后，即使软件 LSQ 状态变化，也不能重新用 `lsqCanAccept` 阻止其
   STA/STD issue。

如果框架需要周期精确复制 RTL，还应建模 `RegNext` 和 redirect 延迟更新 `t2_update`；
如果只做稳态 admission 建模，至少必须保持“公共 Dispatch gate”和“原子 IQ issue gate”
这两个阶段的边界。

源码位置：

- `backend/dispatch/NewDispatch.scala:446-451`
- `backend/dispatch/NewDispatch.scala:688-700`
- `mem/lsqueue/LSQWrapper.scala:385-415`

此外不能断言：

```text
atomic_issue_owner_created -> LQ完全为空 && SQ完全为空
```

ROB 为空只保证 older 指令已经架构提交。更老 store 的 SQ entry 可能尚未通过
MemBlock `sqDeq` 释放，释放后数据还可能继续停留在 SBuffer/Uncache。

### 8.6 原子读改写结果参考模型

参考模型必须把普通 AMO 作为一次不可分割的 RMW 更新：

```text
old = memory[address]
new = op(old, rs2)
memory[address] = new
writeback = old
```

更新时机不能放在 STA fire、STD fire 或第一次 DCache request 上，而应放在确认原子
操作成功完成且没有异常/失败之后。对于 miss/replay，同一个架构原子事务只能提交
一次内存更新。

特殊规则：

- LR：只读并建立 reservation，不修改 memory；
- SC 成功：写 `rs2`，返回 0；
- SC 失败：不修改 memory，返回 1；
- AMOCAS 比较成功：写 `newValue`；
- AMOCAS 比较失败：不修改 memory；
- AMO/AMOCAS 无论是否写入，都返回操作前的旧值。

### 8.7 issue 顺序约束与 DUT 结果检查

issue scheduler 必须模拟 RTL 能在 MemBlock issue 接口上形成的保守原子窗口：

```text
原子owner开始issue前：更老的后端执行owner已经完成/提交
原子owner active期间：只允许继续发射该owner尚未发送的STA/STD片段
原子owner正常提交/terminal，或redirect flush完成后：年轻普通访存或下一条原子才允许issue fire
```

这里限制的不只是 **issue fire**，还包括真实后端 IQ 镜像的 route/enqueue。sequence
可以预先生成年轻事务，主表或独立的 pre-route pending queue 也可以保存它们；但如果
某个软件队列被定义为真实 STA/STD/Load IQ 的镜像，年轻事务在原子窗口内不能进入该
队列，更不能被驱动到 MemBlock。若某一层还负责模拟真实 Dispatch/LSQ enqueue，则
该层也应按 `blockBackward` 阻止年轻事务进入对应 DUT 接口。不能把这些后端顺序职责
混写成 AtomicsUnit 内部约束。

以下属于 DUT/RM 检查，不是 driver 发射前置条件。对 replay：

- 允许 `request -> miss/replay -> request -> response`；
- replay 不能重复收集同一 STD 数据片；
- replay 不能重复更新 memory 或重复 final writeback；
- redirect/flush 必须按 owner 和 dynamic epoch 清理所有未完成子 uop。

对异常：

- 对齐、TLB、PMP/PMA/PBMT、trigger 或 DCache error 优先于内存更新；
- 异常原子事务不得产生错误的 data write；
- 异常 writeback/redirect 后不能残留 atomic owner、子 uop bitmask 或 replay token。

### 8.8 LR/SC reservation 结果模型，不是 issue 阻塞条件

测试框架不能把 LR 到 SC 之间建模成“AtomicsUnit 一直 busy”。LR final writeback 后应
释放原子执行 owner，只在 DCache/RM 层保留 reservation：

```text
reservation_valid
reservation_block_addr
reservation_expire_cycle或剩余计数
```

LR 和 SC 之间允许正常发射、执行其他普通指令。reservation 的存在不能阻止这些指令
issue，也不能要求下一条必须是 SC；它只影响后续 SC 的预期成功/失败结果。

reservation 应在以下条件下失效：

- SC 尝试完成；
- reservation 超时；
- 本地 release/writeback 命中该 block；
- 硬件复位。

对于 coherence probe，当前 RTL 的检查模型应是：

```text
probe命中reservation block
  -> ProbeQueue暂缓probe
  -> reservation因SC/超时/本地writeback解除
  -> probe继续执行
```

因此不能仅因观察到同 block probe，就立即在参考模型中清除 reservation。若以后 RTL
改为“probe 直接打断 reservation”，再把它作为新的显式失效条件加入模型。

测试框架必须区分 cache line writeback 与寄存器/执行单元 writeback：只有
`DCacheWrapper` 中同 reservation block 的 release/writeback 条件，才能触发这里的
`invalid_resv_set`；不能因为观察到普通整数 writeback、load writeback 或 AtomicWB
就清除 reservation。

SC 对 reservation 的 checker 约束为：

```text
任意SC形成最终架构结果 -> reservation_valid清零
SC成功 -> memory只更新一次，rd=0
SC失败 -> memory保持不变，rd=1
SC失败后没有新LR又执行SC -> 必须继续失败
SC失败后软件重试 -> 必须从新的LR重新建立reservation
```

内部 replay 与架构失败必须使用不同状态表达：

- `atomic_replay_pending`：同一个 SC 仍在硬件内部重试，不能提前清 owner 或向软件报失败；
- `atomic_final_writeback(rd=1)`：SC 已形成架构失败，必须清 reservation 并结束该事务；
- replay 过程中不能重复写内存，也不能重复产生 final writeback；
- SC 架构失败后，参考模型不能自动生成新的 reservation，必须等待新的 LR 事件。

SC checker 必须分别验证：

- 有效且地址匹配时成功写入并返回 0；
- 无 reservation、已超时、地址不匹配或 cache 条件失败时不写并返回 1。

### 8.9 建议观察点和必测场景

建议观察顺序：

```text
1. Decode/主表：fuType=MOU，fuOpType和宽度合法。
2. Dispatch/admission：经过公共modeled_lsqCanAccept；AMO成功admit但不分配LQ/SQ。
3. Issue准备：建立原子owner和issue barrier，不创建LQ/SQ owner。
4. Issue：STA/STD owner一致，atomic_piece_idx不重复、不缺失；进入IQ后不重复检查lsqCanAccept。
5. MemBlock：进入s_atomics，原子STA不进普通StoreUnit。
6. TLB：LR使用atom_read，其余使用atom_write。
7. Flush：允许AMO在SQ/SBuffer/Uncache非空时先被接收；随后观察内部flush，确认排空后才发atomic request。
8. DCache：cmd、addr、word_idx、mask、amo_data、amo_cmp正确。
9. Replay：重试不重复写内存、不重复收数据。
10. Reservation：LR建立，SC按有效性和地址判断。
11. Writeback：AMO/LR返回旧值，SC返回0/1，CAS返回旧值。
12. ROB：normal commit，不错误增加scommit。
13. Redirect/异常：阻止错误写入并清理所有atomic状态。
```

第 5 至 13 项是 DUT/RM 检查，不应成为第 2 至 4 项 admission/发射动作的前置条件。
尤其不能等待第 7 项中的空状态成立后才允许第 4 项 issue。

必测场景：

- STA 先到、STD 后到；
- STD 先到、STA 后到；
- AMOCAS.Q 的 STA 槽 0 先到、槽 2 后到；
- AMOCAS.Q 的 STD 槽 0/1/2/3 采用多种乱序排列到达，每槽只接收一次；
- 故意让 AMOCAS.Q STA 槽 2 在槽 0 之前 fire，issue checker 必须报错；
- 故意构造跨 owner STA/STD，checker 必须报错；
- 每一种 W/D AMO 运算的 hit、miss 和 replay；
- LR 后插入普通指令，再执行成功 SC；
- LR 后 reservation 超时、本地 release/writeback、地址不匹配导致 SC 失败；
- SC 成功后不插入新 LR，连续第二次 SC 必须失败；
- SC 失败后不插入新 LR，连续第二次 SC 必须失败；
- SC 失败后重新执行 LR/计算/SC，验证新 reservation 可以独立成功或失败；
- 区分 DCache 内部 replay 与最终 `rd=1`，内部 replay 不能被误判为架构 SC 失败；
- 同 reservation block 的 probe 被暂缓，并在 reservation 解除后继续；
- AMOCAS.W/D/Q 比较成功和比较失败；
- AMOCAS 多 STD 跨周期、跨端口到达；
- W 结果在 RV64 下的符号扩展；
- 对齐、TLB、PMP/PMA/PBMT 和 DCache error；
- TLB、flush、DCache、writeback 各阶段的 redirect；
- `aq/rl` 四种编码进入当前统一保守原子路径。

### 8.10 测试框架约束审计结论

| 原规则或需求 | 审计结论 | 正确落点 |
|---|---|---|
| STA/STD 共享同一 atomic owner | 保留 | issue scheduler/RM |
| 原子全部必需片段在 route 时建立，之后可独立 issue | 保留 | issue scheduler |
| 原子 barrier 在片段进入真实 IQ 模型前建立 | 保留 | route/issue scheduler |
| 原子 active 时年轻事务不能进入真实 IQ、不能 issue fire | 保留 | route/issue scheduler |
| 年轻事务不能预先生成或保存在输入 backlog | 取消 | 主表/pre-route pending 可保存，但不能进入真实 IQ 镜像 |
| 原子 issue 前必须等待 SQ/SBuffer/Uncache 为空 | 取消 | 改为 DUT 接收后 flush、checker 检查 atomic request 时机 |
| `lsqCanAccept=0` 时禁止 AMO Dispatch/admission | 保留 | Load/Store/AMO 进入真实 IQ 模型前共用 modeled gate |
| AMO 已进入 IQ 后仍用 `lsqCanAccept` 阻止 STA/STD issue | 取消 | admission 成功后不能在 issue fire 阶段重复检查 |
| AMOCAS.Q 的 STA 槽 0 必须先于槽 2 | 保留 | issue scheduler/checker |
| AMOCAS.Q 的四个 STD 必须按 0/1/2/3 固定顺序发射 | 取消 | 可任意排列，但必须同 owner、每槽唯一且最终收齐 |
| AMO 为自己分配 LQ/SQ entry 或 owner | 取消 | 原子走独立 owner 和完成生命周期 |
| 原子期间禁止 probe/refill/miss/replay | 取消 | responder 继续响应，checker 检查仲裁与只完成一次 |
| LR 后一直占用 AtomicsUnit 等待 SC | 取消 | LR 完成释放 owner，只保留 reservation 结果模型 |
| final Atomic writeback 后立即放行年轻 issue | 修改 | 正常路径等 normal commit/terminal；redirect 路径等 flush 和 stale 清理完成 |
| RMW、reservation、异常和 DCache request 时机限制 driver | 取消 | 这些只属于 RM/checker，不作为 STA/STD 发射前置条件 |

---

## 9. STA/STD 双队列的后端发射约束与 DUT 检查汇总

本章集中描述由“一个 `DynInst` 在 Memory Scheduler 内部扇出为 STA/STD 队列项”以及
“原子指令通过 `waitForward/blockBackward` 建立独占后端窗口”推导出的测试框架约束。

当前 `memblock_ut` 直接在 MemBlock issue 接口构造和发送事务，没有真实
Decode/Dispatch/ROB/IssueQueue 替它生成合法 issue 流。因此：

```text
sequence / issue scheduler：只负责生成合法的后端issue行为
RM / checker：观察并验证MemBlock、AtomicsUnit、DCache内部行为
responder：提供协议允许的响应和反压，保证排空、miss/replay等流程可以推进
```

不能为了让 DUT 更容易通过检查而让 driver 等待内部状态；否则会屏蔽“AMO 已接收后
能否正确请求排空并等待”的真实功能。

### 9.1 一个架构原子事务，多个执行片段

测试框架不能把 STA 和 STD 建模成两条独立架构指令。它们必须共享一个原子 owner：

```text
atomic_owner = uid + rob_key + dynamic_epoch
```

该 owner 必须贯穿：

```text
STA/STD片段创建
issue queue等待和发射
TLB请求与响应
DCache atomic request/replay/response
Atomic writeback
redirect/flush清理
最终terminal
```

普通原子的片段数量为：

| 指令类型 | STA | STD | 说明 |
|---|---:|---:|---|
| LR/SC/普通 AMO | 1 | 1 | LR 的 DCache 请求不等待 STD 数据 |
| AMOCAS.W/D | 1 | 2 | 比较值 `rd` 和候选新值 `rs2` |
| AMOCAS.Q | 2 | 4 | 两个 128-bit 输入拆成四个 64-bit 数据片 |

所有片段只能改变 owner 内部的局部进度，不能独立提交、独立释放 owner 或分别形成
架构完成。

### 9.2 成对创建不等于同时发射

对普通 `AMO/LR/SC`，RTL 的设计意图是 Dispatch 只有在 STA IQ 和 STD IQ 都 ready 时
才完成一次原子 uop 传输：

```scala
io.fromDispatch.uops(sta).ready := staReady(i) && stdReady(i)
```

Memory Scheduler 随后把地址侧输入复制到 STD 入队口，并把 STD 的源 0 改为原始
`src(1)`：

```scala
stdIQEnq.bits := staIQEnq.bits
stdIQEnq.bits.psrc(0) := staIQEnq.bits.psrc(1)
```

因此测试框架应把流程分成两个阶段：

```text
逻辑route/enqueue：一次性创建该owner要求的全部STA/STD片段
实际issue：各片段按自己的源操作数ready情况独立发射
```

必须允许：

```text
STA先发射，STD后发射
STD先发射，STA后发射
STA和STD同周期从不同端口发射
AMOCAS多个STD跨周期、跨物理端口发射
```

必须禁止：

```text
先创建STA，等STA fire后才创建STD
任一片段fire后立即把整个原子owner标为dispatched或terminal
因STD先到就要求AtomicsUnit已经拥有有效地址
```

“普通 AMO/SC 在数据未齐时不能发送 DCache request”由 DUT checker 检查；issue driver
只负责按合法顺序提供片段，不能直接控制或替 AtomicsUnit 发送 DCache request。

如果验证范围包含真实 Scheduler 入队接口，建议增加：

```text
ordinary_atomic_dispatch_fire
  -> sta_enq.fire && std_enq.fire

sta_enq.fire == std_enq.fire
  // 被isDropAmocasSta显式丢弃的AMOCAS STA副本除外
```

源码位置：

- `backend/issue/Scheduler.scala:372-384`
- `backend/issue/Scheduler.scala:461-509`

### 9.3 原子 issue 窗口前后的队列隔离

AMO 的 `waitForward` 要求其 Dispatch 前 ROB 为空。因此框架开始向 MemBlock 发射某个
原子 owner 时，不应再有更老、尚未完成的后端执行 owner 或待发 IQ 片段：

```text
atomic_issue_window_open
  -> all_older_arch_owners_terminal_or_committed
  -> no_older_live_issue_item
  -> no_older_execution_waiting_for_writeback
```

这里的“更老执行 owner”不包含已经提交后残留在 DUT StoreQueue/SBuffer/Uncache 中的
store 数据。它们属于 MemBlock 内部待排空状态，不能用来阻止原子 STA/STD issue。

原子 issue barrier 应在该 owner 的 STA/STD 片段 route 到后端 IQ 模型之前建立。不能
等 STA 或 STD 已经 fire 后才设置 barrier，否则这段空窗可能让年轻普通访存被错误
route 或发射。

AMO 的 `blockBackward` 又会阻止年轻指令继续进入 ROB 和 IssueQueue。因此原子 active
窗口内，issue scheduler 不得把年轻普通访存或第二个原子 owner 驱动到 MemBlock：

```text
atomic_active
  -> no_younger_load_issue_fire
  -> no_younger_sta_std_issue_fire
  -> no_second_atomic_owner_issue_fire
```

框架可以预先生成这些年轻 transaction，也可以把软件描述保存在主表或独立 pre-route
pending queue；禁止的是把它们 route 到用于镜像真实后端 IQ 的队列，或者在原子窗口
结束前产生真实 issue fire。只有明确不代表硬件 IQ 的输入 backlog 中存在年轻项才是
合法的，不能把两类队列混为一谈。

在相关测试框架队列中，所有仍有效的原子 STA/STD 项都必须属于当前 owner：

```text
atomic_active && sta_item.valid -> sta_item.owner == atomic_owner
atomic_active && std_item.valid -> std_item.owner == atomic_owner
```

普通 AMO 通常只有一个 STA 项和一个 STD 项。AMOCAS 可以同时存在多个片段，但它们
必须全部属于同一个 owner。被 redirect/flush 清除的 stale item 不得再次发射。

正常路径下，barrier 只能在框架建模的 atomic normal commit/terminal 后解除，不能在
STA fire、STD fire、数据收齐、第一次 DCache request 或仅看到 final Atomic writeback
后提前解除。若原子 owner 被 redirect/flush 杀掉，则可以在 flush 已应用且该 owner 的
stale STA/STD、request token 和等待响应全部失效后解除，不要求一个不会再发生的 normal
commit。

本层不要求 LQ/SQ/SBuffer/Uncache 为空。`lsqCanAccept` 只在原子 owner 和片段进入真实
IQ 模型之前作为公共 Dispatch/admission gate；admission 成功后，不得在 STA/STD issue
fire 阶段重新检查。AMO 通过该公共 gate 时仍不创建 LQ/SQ owner，也不修改 LQ/SQ
pointer 或 free count。

源码位置：

- `backend/dispatch/NewDispatch.scala:795-819`
- `backend/rob/Rob.scala:186-193`
- `backend/rob/Rob.scala:393-460`
- `backend/CtrlBlock.scala:707-720`

### 9.4 STA/STD 匹配和片段编号

AtomicsUnit 没有维护多个原子事务的 `robIdx` 匹配表，而是依靠后端串行化保证当前
只收集一个原子 owner。因此测试框架必须比 RTL 更显式地维护 owner 和片段身份：

```text
incoming_sta.owner == atomic_owner
incoming_std.owner == atomic_owner
each_required_atomic_piece_idx_received_once
```

`data_ready -> required_atomic_piece_mask_complete` 应在 DUT/RM checker 中实现，不能用于
限制某个合法 STD 片段何时从后端 issue。

必须分离三个不同概念：

```text
issue_port_idx    // 本次选择的物理issue端口
decoded_uop_idx   // 前端拆分uop编号
atomic_piece_idx  // fuOpType高位编码的AtomicsUnit槽号
```

queue key 至少应包含：

```text
owner + target(STA/STD) + atomic_piece_idx
```

不能把 `issue_port_idx` 当成原子数据槽号，也不能仅凭 `fuType=MOU` 判断两个片段属于
同一动态原子事务。

AMOCAS.Q 还必须满足：

```text
first_sta_slot == 0
sta_slot_2 may_fire only after sta_slot_0 accepted

std_slot_order may be any permutation of {0,1,2,3}
each std_slot must be received exactly once
each std_slot.owner must equal atomic_owner
```

槽 0 STA 携带地址、主 uop 和低半返回 `pdest`，负责启动 AtomicsUnit；槽 2 STA 只
补充高半返回 `pdest`，不能单独启动事务。与 STA 不同，四个 STD 通过 `fuOpType` 高位
选择 `rd_l/rs2_l/rd_h/rs2_h` 寄存器，因此可以跨周期、跨物理端口乱序到达；乱序不等于
可以重复、遗漏或混入其他 owner。

这个 STA 顺序不是性能建议，而是接口正确性要求。AtomicsUnit 空闲时会把第一个
`io.in.fire` 的 STA 锁存为主 `uop`；如果槽 2 抢先，其 `fuOpType` 高位非 0，完整等值
判断 `isAMOCASQ(uop.fuOpType)` 失败。状态机离开空闲态后也不会再开放 AMOCAS.Q 的第二
STA 接收路径，导致后续槽 0 无法补救。

源码位置：

- `mem/pipeline/AtomicsUnit.scala:73-98`
- `mem/pipeline/AtomicsUnit.scala:119-180`
- `mem/pipeline/AtomicsUnit.scala:474-477`

### 9.5 原子窗口不等于整个访存系统冻结

真实后端 AMO Dispatch 前要求更老架构指令离开 ROB，但不代表整个 LSQ 和写缓冲已经为空。
已提交的旧 store 仍可能依次停留在：

```text
StoreQueue
SBuffer
Uncache
```

StoreQueue 的释放计数 `scommit` 来自 MemBlock `sqDeq`，不是 ROB commit。因此允许出现：

```text
ROB已经为空
  -> older store已经架构提交
  -> older store仍在SQ等待dequeue
  -> 随后进入SBuffer/Uncache并被原子flush流程排空
```

因此 issue driver 不能要求：

```text
atomic_sta_issue_fire requires sq_empty && sbuffer_empty && uncache_empty
atomic_std_issue_fire requires sq_empty && sbuffer_empty && uncache_empty
```

正确的职责拆分是：

```text
Driver：
  不读取stIsEmpty来决定是否发射STA/STD
  SQ/SBuffer/Uncache非空时也允许完成合法的atomic issue握手

DUT：
  接收原子STA后建立AtomicsUnit owner
  旧store路径非空时请求flush并等待
  排空完成后才允许发DCache atomic request

Checker：
  检查atomic request的发送时机，不反向约束driver

Environment/Responder：
  允许SQ/SBuffer/Uncache继续排空
  正确处理DCache仲裁、miss/replay、probe和refill
```

对应的 DUT 检查点是：

```text
atomic_dcache_req.fire
  -> sq_empty
  -> sbuffer_empty
  -> uncache_empty
  -> address_translation_and_permission_check_done

atomic_dcache_req.fire && !is_lr
  -> required_std_data_complete
```

对于对齐、TLB、PMP/PMA/PBMT 或 trigger 异常，AtomicsUnit 可以直接异常完成而不发送
DCache request。因此 checker 不能无条件要求“AMO 接收后最终一定出现 atomic request”。

原子 active 期间禁止的是新的年轻后端普通访存，不代表 DCache 内部完全停止。环境仍
必须允许并正确响应：

```text
probe
refill
miss/replay
replace/writeback
MainPipe仲裁和反压
```

不能添加如下过强约束：

```text
atomic_active -> no_probe && no_refill
```

硬件 prefetch 在 MemBlock 原子资源接管期间应被 DUT 禁止；普通 store STD 也必须继续
走普通 LSQ/store-data 路径，不能因与原子 STD 同端口而误送 AtomicsUnit。这两项属于
RTL route checker，不是 issue driver 等待条件。

推荐断言：

```text
atomics_store_data.valid -> FuType.storeIsAMO(fuType)
normal_store_std -> never_route_to_atomicsUnit
```

### 9.6 RM 完成跟踪、replay 和 issue barrier 释放

以下事件都不能单独代表原子事务完成：

```text
STA fire
STD fire
全部STD片段收齐
TLB完成
第一次atomic request发送
DCache miss或内部replay
```

RM/checker 应分别维护：

```text
atomic_sta_piece_mask
atomic_std_piece_mask
atomic_req_sent
atomic_replay_pending
atomic_wb_count
atomic_final_writeback
atomic_terminal
```

普通 AMO/LR/SC 以最终 MOU writeback 形成架构结果；AMOCAS.Q 必须等待低、高两次结果
writeback 都完成。但 issue barrier 仍应等框架建模的 normal commit/terminal 后解除；
final writeback 只说明原子具备提交条件，不能单独放行年轻 issue。redirect/flush 路径则
必须等 flush 应用和旧 owner 清理完成后解除。

对 miss/replay，同一个 owner 可以多次发送请求，但必须满足：

```text
同一数据片不能重复收集
memory最多提交一次更新
final writeback最多产生一次
AMOCAS.Q的两个合法结果writeback除外
```

异常或 redirect 必须按 `owner + dynamic_epoch` 清理所有未完成片段、请求 token 和
replay 状态，旧事务后续到达的响应不得污染新 owner。

### 9.7 建议直接实现的 issue 断言、DUT 断言和覆盖场景

后端 issue 模型断言：

```text
dispatch_admission && !modeled_lsqCanAccept
  -> no_backend_iq_enqueue

amo_dispatch_admitted
  -> lq_pointer_unchanged && sq_pointer_unchanged
  -> lq_free_count_unchanged && sq_free_count_unchanged

ordinary_atomic_created
  -> atomic_issue_barrier_active
  -> sta_piece_exists && std_piece_exists

atomic_active
  -> all_live_atomic_pieces.owner == atomic_owner

atomic_active
  -> no_younger_backend_iq_enqueue
  -> no_younger_memory_issue_fire
  -> no_second_atomic_owner_issue_fire

incoming_atomic_piece
  -> piece.owner == atomic_owner

atomic_issue_barrier_release
  -> (atomic_normal_commit && atomic_terminal) ||
     (atomic_redirect_flush_applied && stale_atomic_state_cleared)

amocas_q_sta_slot_2_fire
  -> sta_slot_0_already_accepted

amocas_q_first_sta_fire
  -> sta_slot == 0

amocas_q_std_fire
  -> std_slot inside {0,1,2,3}
  -> std_owner == atomic_owner
  -> !std_received_mask[std_slot]

amocas_q_data_complete
  -> std_received_mask == 4'b1111
```

不得实现以下 issue driver 断言或 gate：

```text
atomic_sta_issue_fire -> st_path_empty
atomic_std_issue_fire -> st_path_empty
atomic_piece_already_in_iq && !lsqCanAccept -> block_atomic_issue_fire
atomic_active         -> no_probe && no_refill
```

最后一条 `lsqCanAccept` 相关禁用规则只针对已经进入 IQ 的片段；在
Dispatch/admission 阶段仍必须执行公共 `modeled_lsqCanAccept` gate。

DUT/RM checker 断言：

```text
atomic_sta_accepted && !st_path_empty
  -> legal

data_ready
  -> required_atomic_piece_mask_complete

atomic_req.fire
  -> sq_empty && sbuffer_empty && uncache_empty

atomic_req.fire && !is_lr
  -> data_ready

atomic_active && flush_wait_state && !st_path_empty
  -> flush_request_observed
```

`flush_request_observed` 需要按 MemBlock 中的 `RegNext` 路径允许实现对应的周期延迟，不应
写成错误的零延迟组合断言。异常原子允许在没有 `atomic_req.fire` 的情况下结束。

必须覆盖：

- 普通 AMO 的 STA/STD 成对创建；
- AMO issue owner 不创建任何 LQ/SQ owner，也不增加普通 LSQ allocation count；
- `sq_free_count < 4` 时 Load、Store、AMO 的 Dispatch admission 都被阻塞；
- `lq_free_count < 6` 时 Load、Store、AMO 的 Dispatch admission 都被阻塞；
- 两侧恢复公共 headroom 后 AMO admission 成功，且 LQ/SQ pointer、free count 均不变化；
- AMO 已进入 IQ 后改变软件 LSQ 状态，使 `modeled_lsqCanAccept` 拉低，STA/STD 仍可按
  其他正常 issue 条件发射；
- ROB 已空但旧 store 仍在 SQ/SBuffer，确认不能错误断言整个访存系统已经为空；
- STA 先发、STD 后发，以及 STD 先发、STA 后发；
- STA/STD 同周期从不同端口发射；
- AMO STA 被接收时 SQ、SBuffer 或 Uncache 非空，观察 flush 和排空后才发 atomic request；
- 不等待 `stIsEmpty`，driver 仍可正常完成原子 STA/STD issue 握手；
- 原子 active 期间 pre-route pending queue 中存在年轻事务，但真实 IQ 镜像中没有这些项；
- 原子 active 期间故意尝试 route 或驱动年轻 load/store/AMO，issue 协议 checker 必须报错；
- 跨 owner STA/STD 混配，checker 必须报错；
- redirect 后 stale STA/STD 或 response 到达，不得污染新 owner；
- DCache probe/refill 与原子请求发生仲裁和反压；
- miss/replay 后只更新一次 memory、只产生合法次数的 final writeback；
- AMOCAS.W/D/Q 多片段跨周期、跨物理端口发射；
- AMOCAS.Q 槽 0 STA 先于槽 2 STA，并完成两次结果 writeback；
- AMOCAS.Q 四个 STD 使用多种顺序排列，验证同 owner、每槽唯一和最终 mask 完整；
- AMOCAS.Q 槽 2 STA 抢先、STD 重复槽和跨 owner STD 作为负向协议测试，checker 必须报错。

---

## 10. 一句话总结

在当前香山实现中，后端负责把原子指令识别成不可投机、阻塞后继指令的 `MOU` 操作，并分别准备地址和数据；`MemBlock` 负责收集 STA/STD、排空旧 store、完成 TLB 和权限检查，再把操作交给 `AtomicsUnit`；`DCache MainPipe` 负责在一致性权限下完成原子读改写、LR reservation 和 SC 成败判断；最后通过专用 Atomic writeback 把旧值或状态返回给整数寄存器，并让 ROB 完成提交。

最重要的测试原则是：**原子指令不是普通 load，也不是普通 store，而是一条不占 LQ/SQ、需要专用状态机、可能 replay、必须保证读改写不可分割并且一定产生整数结果的独立访存操作。**
