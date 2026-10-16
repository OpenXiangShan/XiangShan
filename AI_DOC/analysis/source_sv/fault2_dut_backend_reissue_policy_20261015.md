# 问题2：DUT后端在Uncache-full rollback后的重新发射机制

日期：2026-10-15

分析范围：XiangShan Scala DUT源码，以及问题2测试框架中的
`issue_queue_scheduler.sv`。本文只分析行为，不修改RTL或测试框架代码。

## 1. 结论摘要

### 1.1 五个问题的直接回答

1. **DUT发射调度不存在随机优先级，也不会在redirect后重新随机。**
   Scala后端没有`send_pri`字段、随机数或随机仲裁。Load在dispatch时按各Load
   Issue Queue当前占用数选择较空的IQ；进入IQ后，由年龄矩阵选取“当前可发射请求中
   最老的请求”。

2. **rollback后的老Load不会收到“ROB头/MMIO/pendingPtr专用最高优先级”。**
   `LoadQueueUncache`产生的是`RedirectLevel.flush`，它会flush触发rollback的Load自身
   和所有更年轻指令。该Load不是保留原IQ entry等待重试，而是从原PC重新取指、
   rename、dispatch，再作为一个新的动态ROB/IQ实例执行；ROB enqueue pointer回退后，
   新实例的`robIdx flag/value`可能复用原数值，但它不是原IQ entry原地复活。重新进入某个Load IQ后，它会
   得到普通的年龄优先权，但没有ROB-head boost，也没有pendingPtr到Issue Queue的反向
   控制线。

3. **DUT通过结构性机制缓解饥饿，而不是靠随机重抽。**
   主要机制包括：
   - full rollback flush触发者自身和全部年轻请求，直接清掉造成压力的年轻Uncache entry；
   - frontend从触发者PC重新取指，触发者在重新取指后缀中天然最老；
   - 每个Load IQ使用oldest-eligible年龄仲裁；
   - 多个Load同拍到达Uncache时先按ROB年龄排序，空闲entry优先分给老请求；
   - dispatch将可去多个LDU的请求优先分配到较空的IQ。

   这些机制显著降低长期饥饿概率，但源码中没有跨所有Load IQ的全局ROB-age仲裁，也
   没有bounded-fairness计数器，所以不能从源码证明“最多重试N次”。

4. **`pendingPtr`只控制已经分配Uncache entry的MMIO请求何时真正发送。**
   它不参与Memory IQ issue选择，也不参与Uncache entry分配。Load可以在尚未到ROB头时
   发射并占用Uncache entry，然后停在entry的`s_idle`等待`pendingPtr`命中。

5. **UID862的单次或有限次Uncache-full rollback在真实DUT中可以自然发生；固定同一批
   年轻请求凭`send_pri`反复压过老请求一万多次，不符合DUT真实调度机制。**
   DUT不具备测试框架的持久化随机优先级。真实DUT只有在老Load重新取指后仍暂时不
   eligible，而跨多个Load IQ的年轻独立Load继续发射并重新占满16项时，才可能再次full。
   一旦老Loadready并进入对应IQ，它在该IQ中受到年龄优先，且同拍Uncache分配也按ROB
   年龄排序。因此，日志中的`10073`次同一逻辑UID重复`subtype=5`更像测试框架固定
   `send_pri`放大的恢复调度饥饿，不是Scala后端的自然等价行为。

## 2. Uncache-full rollback如何产生

### 2.1 Load流水线把MMIO/NC请求送入LoadQueueUncache

`LoadQueue`直接把Load S3输出连接到Uncache buffer：

- `src/main/scala/xiangshan/mem/lsqueue/LoadQueue.scala:293-307`

```scala
uncacheBuffer.io.redirect <> io.redirect
...
for ((buff, w) <- uncacheBuffer.io.req.zipWithIndex) {
  val ldinBits = io.ldu.ldin(w).bits
  buff.valid := io.ldu.ldin(w).valid && !ldinBits.nc_with_data
  buff.bits := ldinBits
}
```

`LoadQueueUncache`共有16项，参数定义见：

- `src/main/scala/xiangshan/Parameters.scala:167-178`

```scala
VirtualLoadQueueSize: Int = 72,
LoadUncacheBufferSize: Int = 16,
RobSize: Int = 160,
```

### 2.2 分配逻辑不看pendingPtr

- `src/main/scala/xiangshan/mem/lsqueue/LoadQueueUncache.scala:345-383`

```scala
val s1_sortedVec = HwSort(VecInit(io.req.map {
  case x => DataWithPtr(x.valid, x.bits, x.bits.uop.robIdx)
}))
...
s2_enqueue(w) := s2_valid(w) && !s2_has_exception(w) &&
                 !s2_need_replay(w) && (s2_req(w).mmio || s2_req(w).nc)
...
val offset = PopCount(s2_enqueue.take(w))
s2_enqValidVec(w) := s2_enqueue(w) && freeList.io.canAllocate(offset)
s2_enqIndexVec(w) := freeList.io.allocateSlot(offset)
```

这里有两个重要事实：

1. 同拍多个Load先按`robIdx`排序，因此有少量空位时，老请求优先获得entry。
2. entry分配条件只看请求合法性和freeList，不看`pendingPtr`。

此外，上游握手始终ready：

- `LoadQueueUncache.scala:349-351`

```scala
io.req.zipWithIndex.foreach { case (r, i) =>
  r.ready := true.B
}
```

因此，Uncache buffer满时不会通过ready反压Load pipeline；它接受“这次执行已经到达”的
事实，再为无法分配entry的请求产生rollback。

### 2.3 满队列请求产生flush-self redirect

- `LoadQueueUncache.scala:529-591`

```scala
val reqNeedCheck = VecInit((0 until LoadPipelineWidth).map(w =>
  s2_enqueue(w) && !s2_enqValidVec(w)
))
...
redirect.valid := reqNeedCheck(i)
redirect.bits.robIdx := reqSelUops(i).robIdx
redirect.bits.level := RedirectLevel.flush
...
io.rollback.valid := GatedValidRegNext(oldestRedirect.valid &&
  !oldestRedirect.bits.robIdx.needFlush(io.redirect) &&
  !oldestRedirect.bits.robIdx.needFlush(lastCycleRedirect) &&
  !oldestRedirect.bits.robIdx.needFlush(lastLastCycleRedirect))
```

如果同拍有多个请求分配失败，模块先选ROB年龄最老的一个产生rollback。

`RedirectLevel.flush`表示flush触发者自身：

- `src/main/scala/xiangshan/package.scala:179-186`
- `src/main/scala/xiangshan/backend/rob/RobBundles.scala:210-213`

```scala
def flushAfter = "b0".U
def flush      = "b1".U
def flushItself(level: UInt) = level(0)

val flushItself = redirect.bits.flushItself() && this === redirect.bits.robIdx
redirect.valid && (flushItself || isAfter(this, redirect.bits.robIdx))
```

所以Uncache-full不是只清年轻请求，而是清除：

```text
触发full的Load自身 + 所有ROB中更年轻的指令
```

## 3. rollback如何进入后端redirect链

调用链如下：

```text
LoadQueueUncache.io.rollback
  -> LoadQueue.io.nack_rollback(0)
  -> LSQWrapper.io.nack_rollback
  -> MemBlock选择最老memory redirect
  -> mem_to_ooo.memoryViolation
  -> Backend/CtrlBlock.fromMem.violation
  -> loadReplay
  -> RedirectGenerator
  -> CtrlBlock.s1_s3_redirect / s2_s4_redirect
  -> frontend、rename/dispatch、IssueQueue、DataPath、MemBlock flush
```

关键连接：

- `LoadQueue.scala:311-312`

```scala
io.nack_rollback(0) := uncacheBuffer.io.rollback
```

- `src/main/scala/xiangshan/mem/MemBlock.scala:1437-1454`

```scala
val allRedirect = loadUnits.map(_.io.rollback) ++
  hybridUnits.map(_.io.ldu_io.rollback) ++
  lsq.io.nack_rollback ++ lsq.io.nuke_rollback
val oldestOneHot = selectOldestRedirect(allRedirect)
val oldestRedirect = WireDefault(Mux1H(oldestOneHot, allRedirect))
io.mem_to_ooo.memoryViolation := oldestRedirect
```

- `src/main/scala/xiangshan/backend/CtrlBlock.scala:213-218`

```scala
private val memViolation = io.fromMem.violation
val loadReplay = Wire(ValidIO(new Redirect))
loadReplay.valid := GatedValidRegNext(memViolation.valid)
loadReplay.bits := RegEnable(memViolation.bits, memViolation.valid)
```

- `CtrlBlock.scala:315-329`

```scala
redirectGen.io.loadReplay <> loadReplay
val loadRedirectOffset = Mux(memViolation.bits.flushItself(), 0.U, ...)
...
redirectGen.io.loadReplay.bits.cfiUpdate.target := loadRedirectPcRead
```

由于Uncache-full是`flushItself()`，redirect目标偏移为0，即从该Load自身PC重新取指。

- `CtrlBlock.scala:703-706, 751-760`

```scala
PipeGroupConnect(renameOut, dispatch.io.fromRename,
  s1_s3_redirect.valid, dispatch.io.toRenameAllFire, "renamePipeDispatch")
dispatch.io.redirect := s1_s3_redirect
...
io.toIssueBlock.flush <> s2_s4_redirect
io.toDataPath.flush := s2_s4_redirect
io.toExuBlock.flush := s2_s4_redirect
```

因此，原Load IQ entry会被flush，并不会原地保留等待某个“retry grant”。

## 4. 重新取指后如何dispatch到Load IQ

默认配置有三个独立标量Load IQ，每个16项、2路enqueue：

- `src/main/scala/xiangshan/Parameters.scala:467-486`

```scala
IssueBlockParams(Seq(ExeUnitParams("LDU0", ...)),
  numEntries = 16, numEnq = 2, numComp = 12)
IssueBlockParams(Seq(ExeUnitParams("LDU1", ...)),
  numEntries = 16, numEnq = 2, numComp = 12)
IssueBlockParams(Seq(ExeUnitParams("LDU2", ...)),
  numEntries = 16, numEnq = 2, numComp = 12)
```

能在多个LDU执行的Load不是随机选择IQ。dispatch比较各候选IQ占用数，形成从较空到较满的
顺序，并按rename slot轮转分配：

- `src/main/scala/xiangshan/backend/dispatch/NewDispatch.scala:359-390`

```scala
compareMatrix(i)(j) := issueQueueCount(exuidx(i)) < issueQueueCount(exuidx(j))
...
IQSort(i) := compareMatrix.map(x => PopCount(x) === (iqNum - 1 - i).U)
...
val minIQSel_ith = IQSort(i % iqNum)
```

- `NewDispatch.scala:417-435`

```scala
u := Mux(renameIn(i).valid,
  Mux(fuTypeOH(i).asUInt.orR,
    Mux1H(fuTypeOH(i), minIQSelAll)(Mux1H(fuTypeOH(i), popFuTypeOH(i))),
    Mux1H(fuTypeOHSingle(i), uopSelIQSingle)),
  0.U.asTypeOf(u))
```

这属于确定性的负载均衡，不是随机优先级。需要注意：三个Load IQ之间没有统一的全局
oldest选择，一个IQ中的老Load不能阻止另外两个IQ发射年轻Load。

## 5. Load IQ内部如何选择发射请求

### 5.1 可发射条件

- `src/main/scala/xiangshan/backend/issue/EntryBundles.scala:39-45`

```scala
def srcReady: Bool = VecInit(srcStatus.map(_.srcState)
  .map(SrcState.isReady)).asUInt.andR

def canIssue: Bool = srcReady && !issued && !blocked
```

仲裁不是简单选ROB头，而是从`canIssue`集合中选择最老项。因此，最老指令源操作数未ready
时，年轻且ready的Load允许越过它发射。

### 5.2 年龄矩阵选择oldest eligible

- `src/main/scala/xiangshan/backend/issue/AgeDetector.scala:36-48, 92-100`

```scala
// age(i)(j): entry i enters queue before entry j
val age = Seq.fill(numEntries)(Seq.fill(numEntries)(RegInit(false.B)))
...
def getOldestCanIssue(..., canIssue: UInt): UInt = {
  VecInit((0 until numEntries).map(i => {
    (VecInit((0 until numEntries).map(j => get(i, j))).asUInt |
      ~canIssue).andR & canIssue(i)
  })).asUInt
}
```

Load IQ在`IssueQueue.scala:521-575`调用`NewAgeDetector`和`AgeDetector`选择年龄最老的
可发射entry。不存在`send_pri`，也没有随机数。

该IQ有enqueue entry、simple entry和complex entry三级结构。默认Load IQ参数是
`numEntries=16, numEnq=2, numComp=12`，即2个enqueue、2个simple、12个complex。
选择逻辑优先complex，再simple，再enqueue：

- `src/main/scala/xiangshan/backend/issue/IssueQueue.scala:553-574`

```scala
selValid := compEntryOldestSel.get(i).valid ||
            simpEntryOldestSel.get(i).valid ||
            enqEntryOldestSel(i).valid
selOH := Cat(
  compEntryOldestSel.get(i).bits,
  Fill(params.numSimp, !compEntryOldestSel.get(i).valid) &
    simpEntryOldestSel.get(i).bits,
  Fill(params.numEnq, !compEntryOldestSel.get(i).valid &&
    !simpEntryOldestSel.get(i).valid) & enqEntryOldestSel(i).bits)
```

所以严格地说，DUT不是在整个16项IQ内直接比较ROB号，而是在各entry类别内维护年龄，
再按entry类别组合选择。entry会从enqueue/simple向complex迁移，长期驻留请求最终进入
稳定entry集合。源码仍没有固定的每事务随机优先级。

### 5.3 发射失败时IQ entry可再次变为可发射

- `EntryBundles.scala:385-392`

```scala
entryUpdate.status.issued := MuxCase(status.issued, Seq(
  (commonIn.deqSel && !cancelBypassVec.asUInt.orR) -> true.B,
  (srcCancelByLoad || respIssueFail) -> false.B,
))
```

一般Load replay/资源阻塞可通过feedback把`issued`清零，在IQ中重试。但Uncache-full不是
普通IQ-local replay；它产生全局flush-self redirect，原IQ entry最终被redirect清除。

## 6. pendingPtr的真实作用

ROB生成MMIO pending信息：

- `src/main/scala/xiangshan/backend/rob/Rob.scala:838-853`

```scala
io.lsq.pendingMMIOld := RegNext(io.commits.isCommit &&
  io.commits.info(0).commitType === CommitType.LOAD &&
  deqPtrEntryValid && deqPtrEntry.mmio)
...
io.lsq.pendingPtr := RegNext(deqPtr)
```

Uncache entry只在状态机发送阶段检查它：

- `LoadQueueUncache.scala:117-135`

```scala
val pendingld = GatedValidRegNext(io.rob.pendingMMIOld)
val pendingPtr = GatedRegNext(io.rob.pendingPtr)
val canSendReq = req_valid && !needFlush && Mux(
  req.nc, true.B,
  pendingld && req.uop.robIdx === pendingPtr
)

when (canSendReq) {
  uncacheState := s_req
}
```

这说明`pendingPtr`的边界非常明确：

```text
Memory IQ issue            不受pendingPtr控制
Load pipeline执行          不受pendingPtr控制
Uncache entry分配          不受pendingPtr控制
已分配MMIO entry发总线请求 受pendingPtr控制
```

所以年轻MMIO Load可以先执行并占住Uncache entry，只是停在`s_idle`，直到自己成为ROB头。
这正是16项可能被年轻MMIO占满的结构原因。

## 7. DUT如何缓解Uncache entry饥饿

### 7.1 flush-self直接清理年轻entry

每个Uncache entry按ROB年龄响应redirect：

- `LoadQueueUncache.scala:84-100, 128-160`

```scala
val needFlush = req_valid && req.uop.robIdx.needFlush(io.redirect)
...
when (flush) {
  req_valid := false.B
}
```

释放条件包括redirect flush：

- `LoadQueueUncache.scala:511-525`

```scala
when ((e.io.mmioSelect && e.io.mmioOut.fire) ||
      e.io.ncOut.fire || e.io.flush) {
  freeMaskVec(i) := true.B
}
freeList.io.free := freeMaskVec.asUInt
```

因此，full rollback本身会制造空位，而不是只让老请求无条件重试。

### 7.2 重新取指后的程序顺序

redirect目标是触发Load自身PC。该Load在重新取指的后缀中排第一，年轻请求只能从其后
重新进入rename/dispatch。它没有专用boost，但天然比同一后缀内年轻请求更早生成新的
动态ROB/IQ实例。ROB指针恢复后可以重新使用相同的数值，因此“robIdx相同”不等于
“原IQ entry没有被flush”。

### 7.3 IQ内oldest eligible和Uncache同拍ROB排序

老Loadready后，在它所在IQ中年龄仲裁优先；即使多条Load同拍抵达Uncache，
`HwSort(...robIdx)`也让老请求先分配剩余entry。

### 7.4 不确定边界

源码没有提供以下强保证：

- 三个Load IQ之间没有全局oldest arbiter；
- pendingPtr不会阻止年轻Load issue或占用Uncache entry；
- 没有按retry次数提升优先级的aging counter；
- oldest选择只覆盖当前eligible集合，老请求source未ready时不会阻止年轻请求；
- Uncache总线长期不返回时，已有entry可能长期占用。

所以真实DUT中可以出现多次full rollback，例如：

```text
R0：老MMIO Load A尚未ready。
R1：其他Load IQ中的年轻独立MMIO连续发射，占满16项。
R2：A变为ready并执行，但无entry，产生full rollback。
R3：flush A和年轻后缀，释放entry，从A的PC重新取指。
R4：若A重新进入IQ后仍因新的依赖/端口条件暂时不eligible，
    其他IQ的年轻MMIO仍可能再次抢先占位。
```

但没有测试框架那种“每次恢复都保留同一组固定随机高优先级，因此必然按同一顺序
重演”的硬编码条件。

## 8. 与测试框架固定send_pri逐项对比

| 项目 | Scala DUT | 测试框架 |
|---|---|---|
| 优先级来源 | IQ entry年龄和ready状态 | 每个main transaction创建时随机一次`send_pri` |
| redirect后是否重新随机 | 不存在随机字段 | 不重新随机，继续复制`main_tr.send_pri` |
| 发射比较顺序 | IQ内oldest eligible；多个独立IQ并行 | `send_pri`优先，只有相同priority才比较ROB年龄 |
| full rollback触发者 | `RedirectLevel.flush`，自身和年轻请求被flush并重新取指 | 逻辑UID及主表事务继续存在，recovery后重新route |
| ROB头专用issue boost | 无 | 无；高`send_pri`可覆盖ROB年龄 |
| pendingPtr作用 | 只允许匹配的已分配entry发送MMIO请求 | 框架通过状态模型控制，但`send_pri`仲裁不读取DUT pendingPtr |
| 公平性来源 | flush-self、程序顺序、IQ年龄、ROB排序分配 | 固定priority模式下无retry aging，可能重复相同不利顺序 |

测试框架证据：

- `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv:667-669`

```systemverilog
tr.send_pri     = randomize_send_pri_value(1'b0);
tr.send_pri_std = randomize_send_pri_value(1'b1);
tr.delay        = randomize_delay_value();
```

- `mem_ut/ver/ut/memblock/seq/base_seq_help/issue_queue_scheduler.sv:67-84`

```systemverilog
main_tr = data.get_main_transaction(uid);
...
item.send_pri = (target == MEMBLOCK_ISSUE_TARGET_STD) ?
                main_tr.send_pri_std : main_tr.send_pri;
item.replay_seq = status.replay_seq;
```

每次重新建立issue item都从main table复制原值，没有调用
`randomize_send_pri_value()`。

- `issue_queue_scheduler.sv:345-357`

```systemverilog
if (compare_pri) begin
  if (candidate.send_pri > best.send_pri) return 1'b1;
  if (candidate.send_pri < best.send_pri) return 1'b0;
end
return item_is_older(candidate, best);
```

- `issue_queue_scheduler.sv:491-513`

```systemverilog
compare_pri = seq_csr_common::get_send_pri_mode_en();
...
select_target_candidates(..., compare_pri, ...);
```

当`MEMBLOCK_SEND_PRI_MODE_EN=1`时，一个年轻请求只要`send_pri`更高，就可以稳定压过老
请求；redirect/recovery不会改变该排序。这与DUT的年龄矩阵机制不是同一模型。

## 9. 对UID862重复subtype=5的责任判断

需要先区分命名：Scala `LoadQueueUncache`只输出`Valid[Redirect]`，源码里没有
`subtype=5`字段。`subtype=5`是测试框架/日志对Uncache-full rollback原因的分类。

### 9.1 DUT中可能自然发生的部分

- 年轻MMIO可以在未到ROB头时发射并占用entry；
- pendingPtr只让ROB头对应entry发总线请求；
- 16项全部占用时，新到达MMIO会rollback；
- 在极端依赖、跨IQ并行和Uncache长延迟条件下，同一静态指令可能经历多次重新取指和
  full rollback。

### 9.2 当前框架现象不等价于DUT的部分

- DUT full redirect会flush触发者自身和所有年轻实例；框架保留逻辑UID/main transaction；
- DUT重新取指后没有持久化随机priority；框架恢复后继续使用相同`send_pri`；
- DUT在每个IQ内选择oldest eligible；框架先比较`send_pri`，ROB年龄只是tie-break；
- DUT同拍Uncache entry分配先按ROB年龄排序；框架发射顺序可以先被`send_pri`改写。

因此，对现有`10073`次重复更准确的判断是：

> Uncache-full这一资源场景来源于真实DUT结构，但测试框架的固定`send_pri`恢复策略把
> 一次可恢复的资源冲突放大成了高度可重复的饥饿循环。Scala源码不支持“真实后端也会
> 按相同固定随机排序重演10073次”的推断。

### 9.3 仍需波形确认的边界

若要对“真实DUT是否也会在该seed中多次自然full”做最终签核，需要full-core波形至少观察：

```text
redirect.level和robIdx
frontend redirect target PC
重新rename后的动态实例及robIdx（允许指针数值复用）
三个LDU IQ的valid/canIssue/oldest select
Load source-ready状态
LoadQueueUncache s1_sortedVec/s2_enqValidVec/freeList
16个entry的owner和state
ROB pendingMMIOld/pendingPtr
```

当前memblock UT主要由测试框架直接建模并驱动issue/recovery，现有UID862日志不能单独证明
full-core frontend/rename/IQ会采用同样重放顺序。

## 10. 最终判断

1. Scala DUT没有随机issue，也没有redirect后重新随机。
2. DUT没有ROB头或pendingPtr到Issue Queue的硬优先通路。
3. DUT靠flush-self重新取指、程序顺序、IQ oldest-eligible和Uncache ROB排序缓解饥饿。
4. pendingPtr只门控已分配MMIO entry发请求，不门控issue和entry分配。
5. 真实DUT可以发生Uncache-full rollback，也不能从源码排除有限次重复；但当前UID862
   因固定`send_pri`重复一万多次的机制属于测试框架调度失真，不能作为DUT自然行为处理。
