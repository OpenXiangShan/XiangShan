# LSU Port、MMIO/NC、非对齐与 Load Replay 约束源码分析

## 1. 总结结论

LSU 请求第一次上流水时，port 由后端 issue queue 和 `MemBlock` 静态连线决定；MMIO/NC 或非对齐不是在 `MemBlock` 中重新选择另一个 LDU/STA port。

非对齐请求的约束最强：load/store 的 tail 都由当前 pipeline 的 S1 回灌到同一个 LDU/STA 的 S0，并且在 S0 仲裁中优先级最高。因此如果 head 在 LDU1 或 STA0 识别出需要 tail，tail 仍然进入 LDU1 或 STA0，不会换 port。

MMIO/NC 请求的约束不是“只能走某个 LSU port”，而是“识别后只能走 uncache/LSQ/SQ 专门后处理路径”。load MMIO/NC 写入 LQ 后进入 `LoadQueueUncache`，返回后通过 replay/bypass 回 load pipeline；store MMIO/NC 写入 SQ 后，等 SQ head 且 committed，再由 StoreQueue uncache FSM 发送 `M_XWR`，不会走普通 sbuffer cacheable 写路径。

memblock 内部 load replay 不要求回第一次发射 port。`LoadQueueReplay` 的 replay port 由 replay entry index 对 `LoadPipelineWidth` 取余分组决定：port `rport` 只从 index 形如 `k * LoadPipelineWidth + rport` 的 replay entry 里选择。也就是说 replay port 不是随便选，也不是 first issue port，而是 replay queue entry remainder 约束。

## 2. 第一次上流水 port 如何绑定

后端参数里，LDU/STA/STD 是独立的 issue block：

- `LDU0/LDU1/LDU2` 分别是独立 `IssueBlockParams`，每个 block 只挂一个 LDU exu。
- `STA0/STA1` 分别是独立 `IssueBlockParams`。
- `STD0/STD1` 分别是独立 `IssueBlockParams`。

源码位置：

- `src/main/scala/xiangshan/Parameters.scala:360-379`
- `src/main/scala/xiangshan/backend/Region.scala:40-42`

`MemBlock` 中先按 FU 类型从 `intIssue` 过滤出 load/store address/store data 端口，然后按 index 静态接到对应执行单元：

```scala
val issueLda = intIssue.filter(_.bits.params.hasLoadFu)
val issueSta = intIssue.filter(_.bits.params.hasStoreAddrFu)
val issueStd = intIssue.filter(_.bits.params.hasStdFu)

newLoadUnits(i).io.ldin <> issueLda(i)
stu.io.stin             <> issueSta(i)
stdExeUnits(i).io.in    <> issueStd(i)
```

源码位置：

- `src/main/scala/xiangshan/mem/MemBlock.scala:445-447`
- `src/main/scala/xiangshan/mem/MemBlock.scala:839-844`
- `src/main/scala/xiangshan/mem/MemBlock.scala:954-979`

这说明第一次上流水时，LDU/STA/STD port 是 issue queue deq port 到 MemBlock execution unit 的静态绑定，不是识别 MMIO/NC、非对齐之后再动态换 port。

## 3. Load 非对齐：tail 必须回同一个 LDU port

`NewLoadUnit` S0 的请求源仲裁中，`unalignTail` 是最高优先级，排在 high-priority replay、fast replay、scalar issue 之前：

```scala
/**
  * Request sources arbitration, in order of priority:
  * 0. unalign tail inject from s1
  * 1. high-priority replay from LRQ, including NC / MMIO replay
  * ...
  * 6. loads issued from IQ
  */
val unalignTail,
  replayHiPrio,
  fastReplay,
  replayLoPrio,
  ...
  scalarIssue = Wire(DecoupledIO(new LoadStageIO))
```

源码位置：

- `src/main/scala/xiangshan/mem/pipeline/NewLoadUnit.scala:89-107`

S1 发现当前 load 是 `unalignHead` 后，用当前请求内容构造 tail，并把地址切到下一个 16B bank：

```scala
val unalignTailInjectValid = pipeIn.valid && !kill && in.unalignHead.get
val unalignTail = Wire(io.unalignTail.bits.cloneType)
connectSamePort(unalignTail, in)
unalignTail.entrance := LoadEntrance.unalignTail.U
unalignTail.vaddr := ((vaddr >> DCacheVWordOffset) + 1.U) << DCacheVWordOffset
unalignTail.fullva := ((in.fullva >> DCacheVWordOffset) + 1.U) << DCacheVWordOffset
unalignTail.noQuery.get := false.B
```

源码位置：

- `src/main/scala/xiangshan/mem/pipeline/NewLoadUnit.scala:648-665`

`NewLoadUnit` 内部把 S1 的 tail 接回同一个 S0：

```scala
s0.io.unalignTail <> s1.io.unalignTail
```

源码位置：

- `src/main/scala/xiangshan/mem/pipeline/NewLoadUnit.scala:1889`

因此 load 非对齐 tail 的 port 约束是：哪个 LDU port 识别出 head，tail 就回哪个 LDU port。

## 4. Store 非对齐：tail 必须回同一个 STA port

`NewStoreUnit` S0 的请求源仲裁也把 `unalignTail` 放在最高优先级：

```scala
/**
  * Arbitrate all S0 request sources with the following priority:
  * 0. unaligned tail injected from S1
  * 1. vector store elements produced by VSplit
  * 2. scalar store requests issued from the issue queue
  * 3. hardware prefetch requests
  */
val unalignTail,
  vectorIssue,
  scalarIssue,
  prefetchReq = Wire(DecoupledIO(new StoreStageIO))
```

源码位置：

- `src/main/scala/xiangshan/mem/pipeline/NewStoreUnit.scala:58-75`

S1 发现 store cross page 非对齐时，构造 tail 并回灌：

```scala
val unalignTailInjectValid = fire && isUnalignHead
val unalignTail = Wire(io.unalignTail.bits.cloneType)
connectSamePort(unalignTail, in)
unalignTail.entrance := StoreEntrance.unalignTail.U
unalignTail.vaddr := ((vaddr >> DCacheVWordOffset) + 1.U) << DCacheVWordOffset
unalignTail.cross16Byte.get := true.B
```

源码位置：

- `src/main/scala/xiangshan/mem/pipeline/NewStoreUnit.scala:385-398`

`NewStoreUnit` 内部同样是 S1 tail 接回同一个 S0：

```scala
s0.io.unalignTail <> s1.io.unalignTail
```

源码位置：

- `src/main/scala/xiangshan/mem/pipeline/NewStoreUnit.scala:933`

因此 store 非对齐 tail 的 port 约束是：哪个 STA port 识别出 head，tail 就回哪个 STA port。

## 5. Load MMIO/NC：识别后走 LoadQueueUncache，不换 LDU port

LoadUnit 写 LQ 时会记录 `nc/mmio`：

```scala
lqWrite.nc := in.nc.get || in.isNCReplay()
lqWrite.mmio := in.mmio.get
lqWrite.memBackTypeMM := !in.pmp.get.mmio
```

源码位置：

- `src/main/scala/xiangshan/mem/pipeline/NewLoadUnit.scala:1465-1467`

`LoadQueue` 从所有 load S3 port 接收 `ldin(w)`，如果 `rep_info.mmioOrNc` 为 1，就写入 `LoadQueueUncache`：

```scala
for ((buff, w) <- uncacheBuffer.io.req.zipWithIndex) {
  val ldinBits = io.ldu.ldin(w).bits
  buff.valid := io.ldu.ldin(w).valid && ldinBits.rep_info.mmioOrNc
  buff.bits := ldinBits
}
```

源码位置：

- `src/main/scala/xiangshan/mem/lsqueue/LoadQueue.scala:263-267`

`LoadQueueUncache` 的 req/bypass 都是 `Vec(LoadPipelineWidth, ...)`，说明所有 LDU port 的 MMIO/NC load 都进入统一 uncache buffer 逻辑，不是转发到某一个固定 LDU port：

```scala
val req = Vec(LoadPipelineWidth, Flipped(Decoupled(new LqWriteBundle)))
val bypass = Flipped(Vec(LoadPipelineWidth, new UncacheBypass))
```

源码位置：

- `src/main/scala/xiangshan/mem/lsqueue/LoadQueueUncache.scala:284-294`

入队条件要求请求没有被 flush、没有异常、除 MMIO/NC 本身外不需要其它 replay：

```scala
val s2_need_replay = s2_req.map { req =>
  req.rep_info.need_rep && !req.rep_info.mmioOrNc
}
s2_enqueue(w) := s2_valid(w) && !s2_has_exception(w) &&
                 !s2_need_replay(w) && (s2_req(w).mmio || s2_req(w).nc)
```

源码位置：

- `src/main/scala/xiangshan/mem/lsqueue/LoadQueueUncache.scala:350-363`

当 uncache 数据返回后，bypass 在当前 replay port `w` 上用 `lqIdx` 匹配：

```scala
ncMatch(w)(i) := e.io.ncOut.valid &&
  io.bypass(w).s0Req.valid && io.bypass(w).s0Req.bits.isNCReplay &&
  e.io.ncOut.bits.uop.lqIdx === io.bypass(w).s0Req.bits.lqIdx

mmioMatch(w)(i) := e.io.mmioOut.valid &&
  io.bypass(w).s0Req.valid && io.bypass(w).s0Req.bits.isMMIOReplay &&
  e.io.mmioOut.bits.uop.lqIdx === io.bypass(w).s0Req.bits.lqIdx
```

源码位置：

- `src/main/scala/xiangshan/mem/lsqueue/LoadQueueUncache.scala:496-548`

LoadUnit S0 对 uncache replay 不再访问 DCache，而是走 uncache bypass/forward：

```scala
val noDCacheAccessUncacheReplay = isUncacheReplay
val noDCacheAccess = noDCacheAccessSwInstrPrefetch || noDCacheAccessUncacheReplay

val uncacheForwardReqValid = replayHiPrio.fire && replayHiPrio.bits.isUncacheReplay()
uncacheBypassReq.lqIdx := replayHiPrio.bits.uop.lqIdx
uncacheBypassReq.isNCReplay := replayHiPrio.bits.isNCReplay()
uncacheBypassReq.isMMIOReplay := replayHiPrio.bits.isMMIOReplay()
```

源码位置：

- `src/main/scala/xiangshan/mem/pipeline/NewLoadUnit.scala:345-376`

所以 load MMIO/NC 的真实约束是：识别后进入 `LoadQueueUncache -> uncache -> wakeup/replay -> bypass`，不是换一个专用 LDU port。

## 6. Store MMIO/NC：识别后走 SQ uncache FSM，不走 sbuffer

StoreUnit S2 根据 TLB/PBMT/PMP 识别 NC/MMIO：

```scala
val isNC = tlbHit && tlbAccessible && Pbmt.isNC(pbmt)
val isMMIO = tlbHit && tlbAccessible && (Pbmt.isIO(pbmt) || Pbmt.isPMA(pbmt) && pmp.mmio)
val isUncache = isNC || isMMIO
val killDCache = kill || hasException || isUncache
```

源码位置：

- `src/main/scala/xiangshan/mem/pipeline/NewStoreUnit.scala:630-646`

识别结果写到 SQ：

```scala
io.toSqAddrRe.memBackTypeMM := memBackTypeMM
io.toSqAddrRe.mmio := isMMIO
io.toSqAddrRe.nc := isNC
io.toSqAddrRe.hasException := fire && hasException
```

源码位置：

- `src/main/scala/xiangshan/mem/pipeline/NewStoreUnit.scala:676-684`

StoreQueue 把 STA S2 的 `nc/mmio/memBackTypeMM` 汇总成 `memoryType`：

```scala
dataEntries(i).memoryType := Cat(!memBackTypeSet, mmioSet, ncSet || !memBackTypeSet)
```

源码位置：

- `src/main/scala/xiangshan/mem/lsqueue/NewStoreQueue.scala:1704-1718`
- `src/main/scala/xiangshan/mem/lsqueue/NewStoreQueue.scala:1737-1766`

StoreQueue uncache FSM 只处理 SQ head 上已经完整、无异常、已提交的不可 cacheable entry：

```scala
private val uncacheCanHandle = !isCacheable(headDataEntry.memoryType) && !headCtrlEntry.isCbo &&
  headCtrlEntry.allValid && !headCtrlEntry.hasException && headCtrlEntry.allocated && headCtrlEntry.committed

io.toUncacheBuffer.req.valid := uncacheState === UncacheState.sendReq
io.toUncacheBuffer.req.bits.cmd := MemoryOpConstants.M_XWR
io.toUncacheBuffer.req.bits.vaddr := headDataEntry.vaddr
io.toUncacheBuffer.req.bits.addr := headDataEntry.paddr
io.toUncacheBuffer.req.bits.nc := isNC
```

源码位置：

- `src/main/scala/xiangshan/mem/lsqueue/NewStoreQueue.scala:947-1004`

普通 sbuffer 路径会被不可 cacheable entry 阻断：

```scala
// when deq is MMIO/NC/CMO request, don't need to write sbuffer.
uncacheStall(i) := !isCacheable(dataEntry.memoryType)
toSbufferValid(i) := !uncacheStall(i) && ...
```

源码位置：

- `src/main/scala/xiangshan/mem/lsqueue/NewStoreQueue.scala:1062-1082`
- `src/main/scala/xiangshan/mem/lsqueue/NewStoreQueue.scala:1141-1169`

因此 store MMIO/NC 的真实约束是：STA port 只负责地址流水识别和写 SQ；后续必须由 SQ head/committed 的 uncache FSM 处理，不走普通 sbuffer cacheable 写路径。

### 6.1 Store 非对齐 MMIO/NC 异常约束

StoreUnit S2 还规定了非对齐 MMIO/NC 的异常行为：

```scala
val afUnalignMMIO = isMMIO && !align
val af = afInaccessible || afVectorUncache || afCboUncache || afUnalignMMIO
val am = !align && isScalar && isNC && !pmpInaccessible
val hasException = in.hasException.get || af || am

stageInfo.uop.exceptionVec(storeAddrMisaligned) := am
stageInfo.uop.exceptionVec(storeAccessFault) := af
```

源码位置：

- `src/main/scala/xiangshan/mem/pipeline/NewStoreUnit.scala:635-642`
- `src/main/scala/xiangshan/mem/pipeline/NewStoreUnit.scala:655-663`

含义：

- 非对齐 MMIO store 产生 store access fault。
- 非对齐 scalar NC store 产生 store addr misaligned。
- 这些场景不会作为普通 uncache store 正常进入后续写路径。

## 7. Load replay port 约束

`LoadQueueReplay` 的 replay port 不是 first issue port，而是 replay entry index remainder 分组。

源码首先要求 replay queue 深度能整除 load port 数：

```scala
require((LoadQueueReplaySize % LoadPipelineWidth) == 0)
def getRemBits(input: UInt)(rem: Int): UInt = {
  VecInit((0 until LoadQueueReplaySize / LoadPipelineWidth).map(i => {
    input(LoadPipelineWidth * i + rem)
  })).asUInt
}
```

源码位置：

- `src/main/scala/xiangshan/mem/lsqueue/LoadQueueReplay.scala:391-398`

每个 replay output port `rport` 只从自己 remainder 分组的 entry 中恢复 one-hot：

```scala
s0_oldestSel := VecInit((0 until LoadPipelineWidth).map(rport => {
  ...
  for (i <- 0 until LoadQueueReplaySize / LoadPipelineWidth) {
    oldestBitsVec(i * LoadPipelineWidth + rport) := oldestSel(i)
  }
  oldest.bits := oldestBitsVec.asUInt
  oldest
}))
```

源码位置：

- `src/main/scala/xiangshan/mem/lsqueue/LoadQueueReplay.scala:494-520`

这段逻辑等价于：

```text
for each replay_port rport:
  candidate_entries = { entry | entry.index % LoadPipelineWidth == rport }
  replay_port[rport] 只从 candidate_entries 中选择一个可 replay entry
```

因此：

- 如果某个 replay entry index 是 5，`LoadPipelineWidth=3`，则它只能从 replay port 2 发出。
- 这不等价于“回第一次发射 port”；entry index 由 `LoadQueueReplay` 分配或复用。

`LoadQueueReplay` 分配新 entry 时，普通新 replay 使用 freelist 分配；如果是 load replay 再次入队，则复用 `schedIndex`：

```scala
val enqIndex = Mux(enq.bits.isLoadReplay, enq.bits.schedIndex, freeList.io.allocateSlot(offset))
```

源码位置：

- `src/main/scala/xiangshan/mem/lsqueue/LoadQueueReplay.scala:633-665`

`C_UNCACHE` replay 具有高优先级，并映射到 `LoadEntrance.replayHiPrio`：

```scala
val hasHigherPriority = cause(i)(LoadReplayCauses.C_DM) ||
                        cause(i)(LoadReplayCauses.C_FF) ||
                        cause(i)(LoadReplayCauses.C_UNCACHE)

replay_req(i).bits.entrance := Mux(
  s2_replayCauses(LoadReplayCauses.C_DM) || s2_replayCauses(LoadReplayCauses.C_UNCACHE),
  LoadEntrance.replayHiPrio.U,
  LoadEntrance.replayLoPrio.U
)
replay_req(i).bits.uncacheReplay.get := s2_replayCauses(LoadReplayCauses.C_UNCACHE)
replay_req(i).bits.ncReplay.get := s2_replayCauses(LoadReplayCauses.C_UNCACHE) && s2_nc
```

源码位置：

- `src/main/scala/xiangshan/mem/lsqueue/LoadQueueReplay.scala:31-47`
- `src/main/scala/xiangshan/mem/lsqueue/LoadQueueReplay.scala:442-461`
- `src/main/scala/xiangshan/mem/lsqueue/LoadQueueReplay.scala:562-600`

结论：memblock 内部 load replay 有 port 约束，但这个约束是 replay queue entry remainder 分组，不是 first issue port，也不是 MMIO/NC 专用固定 port。
