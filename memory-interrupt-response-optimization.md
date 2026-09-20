# 访存非 MMIO 提前响应中断方案

本文记录当前实现和后续验证边界。目标是让已经确认属于普通 cacheable memory 的 load/store 提前把对应 ROB 表项更新为可响应中断，从而减少中断等待访存提交的时间。

## 1. 现状

当前 ROB 在指令入队时设置 `interrupt_safe`。位置在 `src/main/scala/xiangshan/backend/rob/Rob.scala` 的 `interrupt_safe` 逻辑：

```scala
val allow_interrupts = !CommitType.isLoadStore(...) &&
                       !FuType.isFence(...) &&
                       !FuType.isCsr(...) &&
                       !FuType.isVset(...) &&
                       !FuType.isAMO(...)
```

因此所有 load/store 都会被当成不安全，直到它们离开 ROB。队首中断条件为：

```scala
val intrEnable = intrBitSetReg &&
                 !hasWaitForward &&
                 deqPtrEntry.interrupt_safe &&
                 !deqHasFlushed
```

原先 `interrupt_safe` 没有被访存单元更新；当前实现增加了独立的访存状态更新通路，ROB 表项中的 `mmio` 仍不承担这条路径的分类功能。

访存单元已经能够在地址翻译和 PMA/PBMT 检查后区分普通内存、NC 和 MMIO：

- load：`src/main/scala/xiangshan/mem/pipeline/NewLoadUnit.scala`
- store：`src/main/scala/xiangshan/mem/pipeline/NewStoreUnit.scala`
- store queue：`src/main/scala/xiangshan/mem/lsqueue/NewStoreQueue.scala`

## 2. 需要保证的语义

`interrupt_safe = 1` 的含义不能简单理解为“不是 MMIO”。它应表示：

1. 当前指令被中断后可以丢弃并重新执行；
2. 重执行不会重复对外部设备产生访问；
3. 当前指令不会在中断之后才产生一个必须优先处理的同步异常；
4. 当前 ROB 表项仍然属于这条指令，没有被 redirect 或 flush 掉。

因此第一版建议只放开以下指令：

| 类型 | 第一版是否允许提前置 `interrupt_safe` | 原因 |
| --- | --- | --- |
| 标量、cacheable、无异常 load | 允许 | DCache load 可以丢弃后重执行，不产生外部设备副作用 |
| 标量、cacheable、无异常 store | 允许，但必须与 StoreQueue 预提交联动 | 尚未预提交的 store 可以被冲刷并重执行；已预提交的 store 不再可取消 |
| MMIO load/store | 不允许 | 重执行可能重复读写设备 |
| PBMT-NC load/store | 不允许 | 走 uncache 路径，完成和副作用时序不同 |
| TLB miss、page fault、access fault、misalign | 不允许 | 同步异常必须优先于异步中断 |
| 向量 load/store | 第一版不允许 | 一个 ROB 项可能对应多个访存操作，异常和完成边界更复杂 |
| AMO/LR/SC | 不允许 | 具有原子内存语义，不能按普通 load/store 处理 |

这里的“无异常”必须包含地址阶段和数据响应阶段已经可见的异常，例如 load 的 tag error、hardware error 等。若某个异常还可能在后续阶段出现，就不能在当前阶段置安全。

## 3. 数据通路

增加一条从 MemBlock 到 ROB 的轻量状态更新通路，不复用现有的完整 writeback。完整 writeback 发生得太晚，无法达到提前响应的目的。

当前实现已在 `RobBundles.scala` 增加：

```scala
class RobMemStateUpdate(implicit p: Parameters) extends XSBundle {
  val robIdx = new RobPtr
  val interruptSafe = Bool()
}
```

通过 `ValidIO(new RobMemStateUpdate)` 传递状态。访存流水线产生置位事件，StoreQueue 产生清除事件：

- `interruptSafe = 1`：把对应 ROB 表项置为可响应中断；
- `interruptSafe = 0`：store 已经实际预提交，不再允许从该指令自身响应中断。

当前实现也通过异常 writeback 清除此前置位的 `interrupt_safe`。同周期既有置位又有清除时，清除优先。replay 本身不产生同步异常，仍由原有 replay/redirect 路径处理。

接口方向为：

```text
NewLoadUnit / NewStoreUnit
        -> MemBlock.mem_to_ooo
        -> XSCore
        -> Backend / CtrlBlock
        -> Rob
```

如果一个周期可能同时有多个 load/store 完成分类，接口应使用 `Vec`。当前宽度为 `LduCnt + StaCnt + 1`，最后一个端口用于 StoreQueue 的预提交清除事件。更新不需要 ready；它只描述一次状态变化，被 redirect 杀掉的请求直接丢弃即可。

## 4. load 的产生时机

load 的 `isNC`、`isMMIO` 和部分异常在 `NewLoadUnit` 中已经计算：

```scala
val isNC = tlbHit && tlbAccessable && Pbmt.isNC(pbmt)
val isMMIO = tlbHit && tlbAccessable &&
             (Pbmt.isIO(pbmt) || Pbmt.isPMA(pbmt) && pmp.mmio)
val isUncache = isNC || isMMIO
```

当前在 `NewLoadUnitS2` 的地址分类结果稳定后产生更新，条件为：

```scala
loadSafeCandidate = valid &&
                     scalarLoad &&
                     !prefetch &&
                     !unaligned &&
                     tlbHit &&
                     !isUncache &&
                     !exception
```

但不能只看 `isMMIO = 0`。需要确认：

- TLB/PMP/PMA 结果已经最终确定；
- `exceptionVec` 没有地址、权限、对齐、硬件错误；
- 当前 load 没有 `isMMIOReplay` 或 `isNCReplay`；
- DCache response 中不会再补充一个需要优先处理的异常。

未对齐 load 会拆成多个请求，第一版不提前置位，避免第一段地址成功而后续段仍可能异常。

load 的更新内容只需要带 `uop.robIdx` 和 `interruptSafe`，不需要把完整 load 数据复制到 ROB。

## 5. store 的产生时机

store 的 PMA/PBMT 分类在 `NewStoreUnit` 中得到，并通过 `toSqAddrRe` 送入 StoreQueue。StoreQueue 又在 `staReValid` 时把 `mmio`、`nc`、`hasException` 等信息写入自己的表项。

当前在 `StoreUnitS2` 完成 PMA/PBMT 分类后产生更新。此时请求已经通过 store pipeline 的地址阶段；未对齐 store 会拆成多个请求，第一版不提前置位。

store 的安全条件可以写成：

```scala
storeSafeCandidate = fire &&
                      scalarStore &&
                      !prefetch &&
                      !cbo &&
                      !splitUnaligned &&
                      !isMMIO &&
                      !isNC &&
                      !hasException
```

普通 cacheable store 存在 ROB 退休前的预提交路径。预提交后的 store 可能进入 SBuffer，已经不能按“中断后丢弃并重执行”处理。因此当前实现还需要两条联动规则：

1. ROB 中存在待处理中断或 StoreQueue 正在进行 redirect 恢复时，Physical StoreQueue 禁止新的预提交；
2. store 实际发生预提交时，Physical StoreQueue 使用表项中保存的精确 `robIdx` 向 ROB 发送 `interruptSafe = 0`。该 store 此后等待正常退休，再在下一条安全指令处响应后来到达的中断。

这些规则处理同周期竞争：如果中断先到，store 不得预提交；如果 store 已经先发生预提交，ROB 不得再从该 store 自身响应中断。

对于 MMIO/NC store，必须继续保持不安全。特别是 StoreQueue 的 uncache 状态机可能已经进入 `sendReq`、`waitResp` 或 `writeback`，这时不能允许 ROB 抢先响应中断。

## 6. ROB 内部更新方式

ROB 入队时仍然把所有 load/store 初始化为不安全。收到早期访存更新后，再修改对应表项：

```scala
when (update.valid &&
      robEntries(idx).valid &&
      !update.bits.robIdx.needFlush(io.redirect) &&
      !enqOH.asUInt.orR) {
  robEntries(idx).interrupt_safe := update.bits.interruptSafe
}
```

实现时要明确以下优先级：

1. redirect/flush 产生时，杀掉的表项不能再被更新；
2. 新分配的 ROB 表项重新初始化为不安全；
3. 同周期既收到 safe 又收到 fault/replay 时，撤销安全状态优先；
4. 只有当前表项有效时才接受更新。

现有 ROB 表项主要按 `robIdx.value` 索引，因此还要特别防止旧请求在 ROB 槽位复用后写入新指令。第一版可以沿用现有 writeback 的 redirect 检查，并增加断言；更严格的做法是在表项中保存完整 ROB pointer/tag，更新时比较完整 pointer。

更新写入后，`robDeqGroup` 下一次读取即可看到新的 `interrupt_safe`。这样会多一个寄存器周期，但实现简单、时序风险低。不要一开始就做队首组合旁路，除非性能测量确认这一拍是主要瓶颈。

## 7. 与异常、replay 和中断的优先级

早期置安全后，必须保证后续发现异常时能撤销它。建议增加如下规则：

```text
同步异常已知
    -> interrupt_safe = 0
    -> 先走原有 exceptionGen / replay 流程

没有异常，且已确认 cacheable
    -> interrupt_safe = 1
    -> 允许 ROB 在队首响应中断
```

中断条件本身仍然保留：

```scala
intrEnable = intrBitSetReg &&
             !hasWaitForward &&
             deqPtrEntry.interrupt_safe &&
             !deqHasFlushed
```

第一版不建议改动 `RobDeqPtrWrapper` 的中断结构，也不建议让中断和 flushPipe 共用一套安全标志。访存 safe 只解决“普通 cacheable load/store 是否可以被中断”的问题。

## 8. 处理 redirect 和流水线时序

CtrlBlock 会延迟和过滤 writeback：

```scala
delayedNotFlushedWriteBack
```

新的访存状态更新也必须经过相同的 redirect 过滤，或者在 ROB 入口再次检查 `robIdx.needFlush(io.redirect)`。否则可能发生以下错误：

```text
T0: load 完成 cacheable 分类，发出 safe update
T1: 更老的分支 mispredict，load 被 flush
T2: safe update 仍到达 ROB，错误地修改了已复用的 ROB 槽位
```

建议在 RTL 中加入断言，保证被 redirect 的 `robIdx` 不会更新 `interrupt_safe`。

## 9. 推荐的分阶段实现

### 阶段一：当前实现

当前已增加 load/store 单元到 ROB 的 safe update，验证重点为：

- store 在提交前遇到中断可以被冲刷；
- trap 返回后 store 只执行一次；
- MMIO/NC store 仍然等待完成。

### 阶段二：验证普通标量 cacheable load

重点验证 DCache tag error、hardware error、load replay 和 RAW/RAR violation。

### 阶段三：优化延迟

记录从 `csr.intrBitSet` 到 `rob.flushOut.valid` 的周期数。如果 ROB 表项更新后还固定多等一拍，再考虑队首 safe update 的组合旁路。旁路会增加 ROB 队首和 MemBlock 之间的时序路径，应单独评估。

## 10. 必须覆盖的验证场景

1. 中断到达时，普通 cacheable load 在 ROB 队首但尚未提交：应响应中断，trap 返回后 load 重执行。
2. 中断到达时，普通 cacheable store 在 ROB 队首但尚未提交：应响应中断，外部只看到一次 store。
3. MMIO load/store 正在 uncache 请求、响应或 writeback：不得提前响应中断。
4. PBMT-NC load/store：不得按普通 cacheable memory 处理。
5. load 后续发现 page fault、access fault、misalign、tag error 或 hardware error：同步异常必须优先。
6. load 先置 safe、随后发现同步异常：异常必须清除 safe，不能被中断抢先。
7. safe update 与 branch mispredict、load violation、ROB redirect 同周期：被冲刷的指令不能更新 ROB。
8. ROB 槽位循环复用后，旧 load/store 的 update 不能修改新指令。

建议增加以下性能计数器：

- `rob_mem_interrupt_safe_set`
- `rob_mem_interrupt_safe_clear`
- `rob_interrupt_wait_load`
- `rob_interrupt_wait_store`
- `rob_interrupt_wait_mmio`
- `interrupt_arrival_to_flushout_cycles`

这些计数器可以区分收益来自“访存提前变安全”，还是仍然被异常、replay、`waitForward` 或 CSR/trap 路径延迟。

## 11. 结论

推荐的最小改动是：

```text
访存单元完成 cacheable/NC/MMIO 分类
        -> 产生带 robIdx 的 RobMemStateUpdate
        -> 经过 redirect 过滤
        -> 更新 ROB.interrupt_safe
        -> 队首下一次读取时允许中断
```

第一版只放开无异常的标量 cacheable load/store，保留 MMIO、NC、AMO、向量访存和所有未决异常的限制。这样可以验证中断响应速度收益，同时避免把设备副作用、同步异常优先级和 flushPipe 语义混在一次改动中。
