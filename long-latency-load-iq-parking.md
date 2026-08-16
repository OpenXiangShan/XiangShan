# 香山长延迟 Load 感知的 IQ 分层与 Parking

本文记录相关性能计数器、当前实现、瓶颈证据、推荐改法和硬件代价。

分析对象：

- XiangShan commit：`ff4720da4`
- 后端源码：`/nfs/home/zhaozukang/source/XiangShan/src/main/scala/xiangshan/backend`
- 与后端直接相连的 Load pipeline/LSQ：`xiangshan/mem`
- 性能报告：`/nfs/home/cirunner/perf-report/cr260806-c7b373bdb-DefaultConfig`

## 1. 结论

“长延迟 load 感知的 IQ 分层/parking”涉及的已有计数器不少，真正需要重点观察的约 15～20 个。当前性能报告可以证明：

1. `mcf` 的 load miss/replay 很多；
2. LDU IQ 中长期存在大量 valid 但不 ready 的 load；
3. Dispatch 受到很强的 IQ 反压；
4. ROB head 经常等待 load。

但现有计数器还不能证明：

> IQ 中这些不 ready entry 有多少是长延迟 load 的直接或间接消费者，以及把它们 parking 后究竟能释放多少有效容量。

源码核对后还需要纠正一个重要认识：

> 当前标量 load 自己不会在 LDU IQ 中一直等到 Cache miss 返回。load 成功送入 MemBlock 后，原 LDU IQ entry 就会释放。长期占用各类 IQ 的主要对象，是等待 load 结果的后续消费者，以及地址本身尚未 ready 的年轻 load，而不是已经确认 miss 的那条 load 自己。

因此，不建议做“miss load IQ entry 提前释放”。正确的研究对象是长延迟 load 的消费者隔离。

推荐顺序：

1. 增加精确计数器；
2. 增加 long-load scoreboard，先做 shadow profiling；
3. 利用现有 3 个 LDU IQ 做低成本 steering 原型；
4. 数据证明收益后，再实现 4～8 entry 的 dependent queue；
5. 第一版不做全 IQ CAM 扫描和 miss 后批量迁移。

## 2. 现有性能计数器

### 2.1 Dispatch 周期级阻塞

代码位置：`backend/dispatch/Dispatch.scala:889-897`。

| 计数器 | 含义 | 口径 |
|---|---|---|
| `stall_cycle` | Dispatch 有 valid uop，但整个 Dispatch 不能前进 | cycle |
| `stall_cycle_iq` | Dispatch 阻塞，并且至少一个 uop 被 IQ 阻塞 | cycle |
| `stall_cycle_rob` | ROB 无法接收导致 Dispatch 阻塞 | cycle |
| `stall_cycle_lsqFull` | LSQ 无法接收导致 Dispatch 阻塞 | cycle |
| `stall_cycle_allowDispatch` | 特殊 dispatch policy 导致阻塞 | cycle |

最重要的是：

```scala
XSPerfAccumulate(
  "stall_cycle_iq",
  dispatchBlock && uopBlockByIQ.asUInt.orR
)
```

`uopBlockByIQ` 在 `Dispatch.scala:614-621` 产生，综合考虑：

- uop 选择了哪个 IQ；
- 对应 IQ enqueue port 是否 ready；
- 同周期分配到该 IQ 的 uop 数是否超过 `numEnq`。

这组计数器是判断 parking 是否减少真实周期损失的第一入口。

### 2.2 IQ 阻塞的 TopDown 归因

代码位置：`backend/dispatch/Dispatch.scala:1038-1072` 附近。

| 计数器 | 含义 |
|---|---|
| `LoadIQFullStall` | 阻塞 uop 被归因到 load IQ |
| `StoreIQFullStall` | 阻塞 uop 被归因到 store IQ |
| `IntIQFullStallAlu` | ALU 类 IQ 阻塞归因 |
| `IntIQFullStallBrh` | 分支类 IQ 阻塞归因 |
| `IntIQFullStallOther` | 其他整数 IQ 阻塞归因 |
| `IQEnqPolicyStall` | IQ 尚未达到容量阈值，但 enqueue/entry 迁移 policy 不能接收 |
| `IQEnqPolicyStallIssued` | enqueue 区域存在 issued entry，导致 IQ 不能继续接收 |
| `BalanceDispatchPolicyStallLoad` | load IQ 平衡策略未能让当前 load dispatch |
| `BalanceDispatchPolicyStallStore` | store IQ 平衡策略未能让当前 store dispatch |

这些不是严格互斥的 cycle counter，而是 TopDown 的 uop/slot 归因。一个周期可以累计多个 slot，所以计数值可能大于周期数，不能直接解释成性能损失百分比。

正确用法是：

- 用 `stall_cycle_iq` 判断真实 IQ 阻塞周期；
- 用 `LoadIQFullStall`、`IQEnqPolicyStall` 等判断阻塞性质；
- 不把多个归因计数直接相加。

### 2.3 IQ occupancy、ready 和 issue

代码位置：`backend/issue/IssueQueue.scala:1114` 附近。

| 计数器 | 含义 |
|---|---|
| `valid_cnt` | 当前 IQ 中有效 entry 数的 histogram |
| `valid_cnt_hist_futype_load` | 当前有效 load entry 数 |
| `ready_cnt` | 当前 `valid && canIssue` 的 entry 数 |
| `ready_cnt_hist_futype_load` | 当前可 issue 的 load entry 数 |
| `enq_entry_valid_cnt` | enqueue 区域占用 |
| `other_entry_valid_cnt` | simple/complex/other 区域占用 |
| `issue_instr_pre_count` | issue 延迟前选出的 uop 数 |
| `issue_instr_count` | 实际从 IQ 送出的 uop 数 |
| `*_ldCancel_src*_cnt` | source 因 load cancel 恢复为 not-ready 的次数 |
| `*_og0Cancel_src*_cnt` | 预测唤醒后在 og0 被取消的次数 |
| `*_wakeup_iq_from_exu*_cnt` | entry 从指定 EXU 收到的 wakeup 次数 |

它们可以区分：

```text
IQ 物理容量真的满
vs.
IQ 中有大量 not-ready entry
vs.
IQ 尚有空间，但内部 enqueue/迁移结构不能接收
```

它们还不能回答：

- not-ready source 的 producer 是否为已经确认 miss 的 load；
- 一个 miss load 对应多少个在途消费者；
- 去掉这些消费者后能释放多少 IQ entry。

### 2.4 ROB head 与 load latency

代码位置：

- `backend/rob/Rob.scala:1362-1400`
- `backend/rob/Rob.scala:1413-1437`
- `backend/rob/Rob.scala:1540-1556`

| 计数器 | 含义 |
|---|---|
| `waitLduCycle` | ROB head 是 LDU 且尚未 writeback 的周期 |
| `waitLoadCycle` | ROB head 是 load commit type 且尚未 writeback 的周期 |
| `MemNotReadyStall` | ROB-head load 尚未记录为 issued 的 TopDown 归因 |
| `LoadL2Stall` | ROB-head load 为 L1 miss，等待 L2 路径 |
| `LoadL3Stall` | L2 miss，等待 L3 路径 |
| `LoadMemStall` | L3 miss，等待更低层内存 |
| `LoadTLBStall` | TLB miss/replay 归因 |
| `LoadMSHRReplayStall` | MSHR 相关 replay 归因 |
| `LoadVioReplayStall` | memory violation replay 归因 |
| `load_instr_cnt` | commit 的 load 指令数 |
| `load_latency_execute` | load 从 issue 到 writeback 的延迟总和 |
| `load_latency_commit` | load 从 writeback 到 commit 的等待总和 |

ROB-head load 原因的优先级大致为：

```text
L3 miss → LoadMemStall
L2 miss → LoadL3Stall
L1 miss → LoadL2Stall
未 issue → MemNotReadyStall
TLB/MSHR/violation replay → 对应 replay stall
其他 → LoadL1Stall
```

注意：

- `load_latency_execute` 包含 load 的实际执行/等待部分；
- `load_latency_commit` 是结果 writeback 后在 ROB 中等待提交的时间，不是 Cache latency；
- parking 不直接缩短 DRAM 延迟，直接目标是降低 `stall_cycle_iq`、提高独立 uop 的 dispatch/issue 机会和 IPC。

### 2.5 Load pipeline 和 replay

代码位置：

- `mem/pipeline/NewLoadUnit.scala:1218` 附近
- `mem/lsqueue/LoadQueueReplay.scala:990` 附近

| 计数器 | 含义 |
|---|---|
| `ldin_valid` | Load Unit 输入有效 |
| `ldin_block` | Load Unit 输入有请求但不能接收 |
| `stall_dcache` | Load Unit 请求 DCache 时被阻塞 |
| `dcache_miss` | DCache miss 事件，包含 replay 后再次 miss |
| `dcache_miss_first_issue` | 第一次执行就发生 DCache miss |
| `replay_fire` | 从 Load Replay Queue 再次进入 Load Unit |
| `fast_replay_fire` | fast replay 次数 |
| `replay_dcache_miss` | 因 DCache miss 进入/保持 replay 的事件数 |
| `replay_dcache_miss_over4_times` | 同一 load 因 DCache miss replay 超过 4 次 |
| `replay_tlb_miss` | 因 TLB miss replay |
| `replay_bank_conflict` | 因 bank conflict replay |
| `replay_forward_fail` | 因 store-forward 条件不满足 replay |

这些计数器能说明 load miss/replay 压力，但没有把 miss producer 与后端 IQ consumer 关联起来。

## 3. mcf 样本的证据

样本：`mcf_6463_0.0229526`。

完整仿真：

```text
instrCnt = 40,000,000
cycleCnt = 23,285,309
IPC      = 1.717821
```

报告中途清过一次性能计数器。最后一组 counter 的采样区间约为 `11,160,414` cycle，不能把最后一组 counter 除以完整的 `23,285,309` cycle。

### 3.1 Dispatch 与 ROB

最后一个统计区间：

```text
stall_cycle_iq = 5,908,484 ≈ 52.9% cycle
waitLduCycle   = 6,091,781 ≈ 54.6% cycle

stall_cycle_rob     = 0
stall_cycle_lsqFull = 4,085
```

这说明 mcf 的直接容量瓶颈不是 ROB 或 LSQ，而是 IQ 侧反压和 load 等待。

### 3.2 LDU IQ occupancy 与 ready

DefaultConfig 有三个独立 LDU IQ，每个 20 entry：

```text
LDU0 valid_cnt_mean ≈ 11.02 / 20
LDU1 valid_cnt_mean ≈ 10.95 / 20
LDU2 valid_cnt_mean ≈ 10.84 / 20
```

ready load 分布：

| IQ | ready=0 的周期 | ready≤1 的周期 |
|---|---:|---:|
| LDU0 | 74.9% | 95.4% |
| LDU1 | 76.1% | 95.9% |
| LDU2 | 76.6% | 96.2% |

即大部分时间：

```text
每个 LDU IQ 中约有 11 个 valid load，
但能发射的只有 0～1 个。
```

这说明增加 oldest picker 不是 mcf 的第一优先级。问题更像是大量 load 的地址 source 尚未 ready，或者 IQ 内部 enqueue/迁移结构受阻。

另外，平均 occupancy 约 11/20，并没有物理填满，但仍然出现很强的 `LoadIQFullStall` 和 `IQEnqPolicyStall`。因此应重点检查：

- enqueue 区域是否存在 issued entry；
- simple/complex entry 迁移是否受限；
- Dispatch 平衡策略是否把 uop 分到了当前不能接收的 IQ；
- 空闲 entry 是否对当前 enqueue port 真正可用。

不能简单把问题概括为“20 entry 太少”。

### 3.3 miss、replay 与 latency

最后一个统计区间三个 Load Unit 合计约为：

```text
dcache_miss_first_issue ≈ 0.933M
dcache_miss             ≈ 1.925M
replay_fire             ≈ 1.528M
fast_replay_fire        ≈ 0.392M
replay_dcache_miss      ≈ 0.798M
```

load 延迟：

```text
load_instr_cnt        = 4,679,714
load_latency_execute  = 51,709,522
load_latency_commit   = 132,007,366

平均 execute latency ≈ 11.05 cycle/load
平均 commit latency  ≈ 28.21 cycle/load
```

可以确定 load miss/replay 压力很大，但还不能由此推导出 parking 的理论收益。还缺的关键数据是：

> 有多少 IQ entry 的唯一未就绪 source，是一条已确认的 long-miss load？

## 4. 当前 load、wakeup 和 cancel 实现

当前标量 load 的主要流程是：

```text
Rename / Dispatch
       ↓
LDU IQ 等待地址 source ready
       ↓
LDU IQ issue
       ↓
请求被 MemBlock 接收，原 LDU IQ entry 释放
       ↓
LDU S0 对 pdest 做预测性 wakeup
       ↓
S1/S2/S3 检查 TLB、DCache、forward、bank conflict
       ├── 成功：writeback，消费者继续执行
       └── miss/replay：发出 ldCancel，消费者恢复为 not-ready
                              ↓
                         load 进入 LRQ
                              ↓
                         replay 时再次预测性 wakeup
```

### 4.1 标量 load 的原 IQ entry 何时释放

在 `backend/Region.scala:527-534`，load 成功送入 MemBlock 时产生：

```scala
snResp.finalSuccess := toMem.fire
snResp.lqIdx := toMem.bits.lqIdx
```

在 `backend/issue/EntryBundles.scala:210-217`，LDU IQ 使用 `lqIdx` 匹配 response，并在 final success 后清除 entry：

```scala
common.deqSuccess := status.issued && finalSuccess &&
  !common.srcLoadCancelVec.asUInt.orR
```

所以标量 load 的 IQ entry 不会一直保留到 Cache 数据返回。

### 4.2 消费者为什么会先醒来又被取消

在 `mem/pipeline/NewLoadUnit.scala:388-402`，Load Unit S0 对刚进入流水线的标量 load 产生预测性 wakeup。这个 wakeup 主要携带寄存器写使能和 `pdest`。

在 S2/S3 发现 DCache miss、TLB miss、forward fail、bank conflict 等问题后，`NewLoadUnit` 通过 `cancel` 撤销依赖链。`MemBlock.scala:879-881` 将它送入后端：

```scala
ldCancel(i).ld2Cancel := newLoadUnits(i).io.cancel
wakeup(i) := newLoadUnits(i).io.wakeup
```

这套机制支持 hit load 的早唤醒，同时保证 miss/replay 时能够恢复 source busy 状态。

### 4.3 现有 `loadDependency` 的真实含义

当前参数为：

```text
LoadPipelineWidth   = 3
LoadDependencyWidth = 2
```

每个 source 保存 3 条 load lane、每条 lane 2 bit 的 dependency mask。`LoadShouldCancel` 根据 `ld1Cancel/ld2Cancel` 与相应 bit 判断是否撤销 wakeup。

相关代码：

- `backend/rename/BusyTable.scala:96-176`
- `backend/issue/EntryBundles.scala:207-325`
- `backend/Bundles.scala:1840-1845`

它表达的是：

```text
这个 source 最近是否由某条 load pipeline 预测唤醒，
在未来一两级收到 load cancel 时是否应撤销。
```

它不表达：

```text
这个 source 的 producer 是哪个 ROB/LQ 中的 load，
该 load 是否已经确认 L1/L2/DRAM miss。
```

dependency bit 会随流水每拍左移并很快归零。因此现有 `loadDependency` 可以用于短时预测唤醒取消，却不能充当 long-load producer tag，也不能准确定位长期等待的消费者。

## 5. 建议新增的计数器

第一轮只做 profiling，不改变流水行为。

### 5.1 每个 IQ

建议增加：

```text
valid_not_ready
valid_wait_long_load
valid_wait_only_long_load
valid_wait_long_load_src0
valid_wait_long_load_src1
ready_not_selected
iq_enq_blocked_with_long_load_waiter
long_load_wakeup_to_issue_latency
long_load_consumer_cancel
long_load_consumer_flush
```

| 计数器 | 建议定义 |
|---|---|
| `valid_not_ready` | `valid && !canIssue` 的 entry 数/周期 |
| `valid_wait_long_load` | 至少一个 not-ready source 命中 long-load scoreboard |
| `valid_wait_only_long_load` | 除 long-load source 外，其他 source 都已 ready |
| `valid_wait_long_load_srcN` | 按 source 位置拆分 long-load 等待 |
| `ready_not_selected` | entry 已 ready，但当周期没有被 picker 选中 |
| `iq_enq_blocked_with_long_load_waiter` | IQ 不能接收，同时内部存在 long-load waiter |
| `long_load_wakeup_to_issue_latency` | long load 最终完成后，消费者到实际 issue 的延迟 |
| `long_load_consumer_cancel` | long-load consumer 被预测唤醒后又 cancel 的次数 |
| `long_load_consumer_flush` | long-load consumer 等待期间被 flush 的次数 |

最关键的是 `valid_wait_only_long_load`。它最接近“如果把等待长延迟 load 的 consumer 移出普通 IQ，可以释放多少关键 entry”。

### 5.2 Dispatch

建议增加真正的 cycle counter：

```text
stall_cycle_iq_with_long_load_waiter
stall_cycle_iq_without_long_load_waiter
```

再增加事件/slot counter：

```text
dispatch_long_load_dependent
dispatch_long_dep_to_reserved_iq
dispatch_long_dep_fallback
dispatch_long_dep_no_alternative_iq
```

这样才能回答 IQ 阻塞周期中，有多少真的与 long-load consumer 占用重叠。

### 5.3 Load pipeline

需要明确的 miss 与 completion 通知/统计：

```text
long_load_miss_notify
long_load_complete_notify
long_load_outstanding_count
miss_to_complete_latency_hist
miss_consumer_count
```

通知最好携带：

```text
pdest
寄存器类型
ROB index
LQ index
是否 first miss
```

现有 `MemWakeUpBundle` 只有 write-enable 和 `pdest`，没有 ROB/LQ，也没有“确认 miss”或“最终完成”的显式语义。

## 6. 推荐的分阶段实现

### 6.1 第一阶段：Long-load scoreboard + shadow profiling

在 Load Unit S2 确认 `cause(C_DM)`，且 load 需要写寄存器时，按 `pdest` 设置 long-miss 状态：

```text
longMissInt[pdest] = 1
longMissFp[pdest]  = 1
```

在 load 最终成功 writeback 时清除。物理寄存器被 Rename 重新分配时也必须清除，避免 flush 后旧 miss 状态污染新 producer。

DefaultConfig 物理寄存器规模约为：

```text
整数物理寄存器：224
浮点物理寄存器：256
```

状态本身约 480 bit，主要成本不是 bit 数，而是：

- 3 个 Load Unit 的 set/clear 更新冲突；
- Rename allocation 清除；
- Dispatch/IQ 多 source 查询的读端口和扇出；
- pdest 重用和 flush 语义。

第一版 scoreboard 只驱动性能计数器，不改变调度行为。IQ 用 source `psrc` 查询：

```scala
srcWaitLong := srcIsReg && longMissTable(psrc)
```

实际 miss 到 LDU S2 才确认。因此：

- miss 确认后仍在 IQ 中等待的 consumer 可以通过 `psrc` 动态识别；
- 紧跟 producer 的 consumer 可能已经提前进入 IQ；
- 仅靠 Dispatch steering 无法避免这些 consumer 初始占用 IQ。

### 6.2 第二阶段：复用三个 LDU IQ 做 steering

DefaultConfig 已有三个同构的标量 LDU IQ。可以先做低成本原型：

```text
LDU0/LDU1：优先普通 load
LDU2：优先地址依赖 long-miss load 的 younger load
```

使用软隔离而不是硬分区：

```text
long-dependent load 优先进入 LDU2；
LDU2 超过高水位时 fallback 到 LDU0/LDU1；
LDU0/LDU1 压力很大且 LDU2 空闲时，普通 load 也可借用 LDU2；
任何分类错误都不能造成永久阻塞。
```

优点：

- 不增加完整 IQ；
- 不迁移已经入队的 uop；
- 复用现有 wakeup、cancel、flush 和 picker；
- 容易加开关做 A/B 对照；
- 硬件和验证代价低于真正 parking queue。

局限：

- 只是隔离，不减少总 entry 占用；
- 只覆盖 load consumer，不能隔离 ALU/branch/store consumer；
- miss 确认较晚，部分 consumer 已经进入普通 IQ；
- LDU2 保留过多可能伤害 L1-hit 密集 workload。

因此必须保留 fallback 和动态水位。

### 6.3 第三阶段：4～8 entry 的 LDU dependent queue

如果 shadow profiling 显示 `valid_wait_only_long_load` 很高，而且第二阶段 steering 有收益，再实现一个小型 dependent queue。

第一版只支持 scalar LDU consumer，因为标量 load 地址通常只有一个整数寄存器 source，比通用 ALU consumer 简单。

基本流程：

```text
Dispatch 检测 load 地址 psrc 命中 longMiss scoreboard
          ↓
dependent queue 有空间：进入 dependent queue
          ↓
producer load 最终 writeback：按 pdest 唤醒
          ↓
consumer 转入普通 LDU IQ
          ↓
正常 select / issue
```

队列满时必须 fallback 到普通 LDU IQ，或者使用正常 Dispatch backpressure，不能丢指令或等待一个永远不会到来的专用事件。

dependent queue 至少要支持：

- 保存完整 uop/payload；
- `psrc` wakeup 匹配；
- producer load 的最终 writeback；
- load cancel 和 replay；
- ROB redirect/flush；
- parked 期间其他 source 的 wakeup；
- 向普通 IQ 的迁移握手；
- queue 满 fallback；
- 年龄和饥饿控制；
- pdest 重用保护。

只在原 IQ entry 增加一个 `parked` bit 没有明显容量收益，因为 entry 仍被占用。真正释放普通 IQ 容量，必须把完整 uop 放到独立结构中。

### 6.4 最后才考虑通用 consumer parking

如果 LDU-only dependent queue 确实有收益，再扩展到 ALU/STA consumer。通用 consumer 更复杂，因为：

- 一条 uop 可能有两个或更多寄存器 source；
- 可能同时等待多个 producer；
- wakeup 来源更多；
- ALU IQ 数量多且功能集合不同；
- 向哪个 IQ 迁移涉及 FU mask、负载平衡和年龄；
- wakeup CAM 和广播扇出明显增大。

第一版不建议支持向量、CSR、异常 uop 或所有整数 IQ。

## 7. 不建议的第一版方案

### 7.1 miss 后扫描并批量迁移全部消费者

这需要：

```text
miss pdest
  ↓
与全部 IQ entry 的全部 psrc 做 CAM 比较
  ↓
同周期找出多个 consumer
  ↓
写入 parking queue并清除多个原 IQ entry
```

代价包括大量 CAM、多 entry 删除、parking queue 多写端口、age matrix 更新，以及同周期 wakeup/cancel/flush 冲突。这是高代价方案，不适合最小实验。

### 7.2 miss load 自己提前释放 LDU IQ entry

没有必要。当前标量 load 在被 MemBlock 接收后已经释放 LDU IQ entry，Cache miss 和后续 replay 状态主要由 Load Queue Replay 等内存侧结构保存。

### 7.3 只扩大普通 IQ

mcf 中每个 LDU IQ 平均只占约 11/20，却仍有强反压，表明还有 enqueue port、内部迁移和 Dispatch policy 问题。直接扩大 IQ 可能只会容纳更多 not-ready uop，并增加 wakeup/select 面积和功耗。

### 7.4 把所有 load-dependent uop 都 parking

在 miss 确认前，大多数 load 仍可能是 L1 hit。如果所有 load consumer 都 parking，会给 hit load 依赖链增加迁移延迟，可能伤害 `lbm`、`gromacs`。分类必须基于确认 miss、miss predictor 或自适应策略，并保留 fallback。

## 8. 硬件与验证代价

| 方案 | 代价 | 主要成本 |
|---|---|---|
| 增加普通 IQ/Dispatch 计数器 | 低 | counter/histogram |
| miss/completion 通知 | 低～中 | MemBlock 到 Backend 的新接口及对齐 |
| long-load scoreboard + shadow profiling | 低～中 | 多端口 set/clear、查询扇出、pdest 重用 |
| 复用 LDU0/1/2 做 steering | 中 | Dispatch 分类、动态水位和 fallback |
| IQ 增加 `longWait`/quota 状态 | 中 | entry 状态和 picker mask，但不释放 entry |
| 4～8 entry LDU dependent queue | 中高 | 完整 uop 存储、wakeup、flush、cancel、迁移 |
| 通用 ALU/LDU parking queue | 高 | 多 source、多 IQ、FU mask、广播和年龄维护 |
| miss 后 CAM 扫描并批量迁移 | 很高 | 全 IQ CAM、多写端口、age matrix 和恢复 |

真正昂贵的不是增加一个 bit，而是：

1. 如何可靠标识 producer；
2. 如何找到 consumer；
3. 如何在不破坏 wakeup/cancel/flush 的情况下迁移 consumer；
4. 如何保证分类错误时仍然前进；
5. 如何避免新增逻辑进入 Dispatch 或 wakeup 关键路径。

## 9. 推荐实验顺序

### 实验 A：只加 profiling

工作内容：

1. 实现 long-load scoreboard；
2. 只统计，不改变行为；
3. 增加 `valid_wait_long_load`、`valid_wait_only_long_load`；
4. 增加 `stall_cycle_iq_with_long_load_waiter`；
5. 按 IQ 和 FU type 分开统计。

优先 workload：

```text
mcf
xalancbmk
milc
gcc_g23 / gcc_s04
soplex_ref
```

回归 workload：

```text
lbm
gromacs
gamess_*
```

进入下一阶段的条件：

- long-load consumer 占用显著；
- `valid_wait_only_long_load` 不是很小；
- 它与 `stall_cycle_iq` 有明显周期重叠；
- 不是只有极少数 SimPoint 出现。

### 实验 B：三个 LDU IQ 的依赖感知 steering

1. LDU2 优先接收 long-load-dependent address uop；
2. LDU0/LDU1 优先普通 load；
3. 设置高低水位；
4. 保留双向 fallback；
5. 增加 steering/fallback counter。

这是最合适的第一版功能改动，因为结构改动小、容易关闭，也最容易判断“隔离”本身是否有价值。

### 实验 C：小型 LDU dependent queue

只有实验 A/B 明确有收益后再实现：

- 先用 4 entry，再比较 8 entry；
- 只支持一个 long-load source；
- 先支持 scalar LDU；
- 保留普通 IQ fallback；
- 对 queue 满、flush、producer 完成和 pdest 重用写断言。

## 10. 验收指标

必须同时观察：

```text
加权 IPC
stall_cycle_iq
stall_cycle_iq_with_long_load_waiter
valid_wait_long_load / valid_wait_only_long_load
每个 IQ 的 valid_cnt / ready_cnt
dispatch fallback
load replay / cancel
load execute latency
load commit latency
时序、面积和功耗
```

直接成功标准：

1. IPC 上升；
2. `stall_cycle_iq` 下降；
3. 普通 uop 可用的 IQ entry 增加；
4. replay/cancel 没有异常增加；
5. `lbm/gromacs` 等非长延迟主导程序不明显回归。

`MemNotReadyStall`、`load_latency_execute` 是二级指标，不应规定必须下降。parking 本身不缩短 DRAM latency，它的作用是让独立工作和其他 miss 更早进入流水线。

`load_latency_commit` 表示 writeback 后等待提交的时间，也不是直接优化对象，合理目标是“不明显回归”。

不能只看以下某一个指标下降就宣布成功：

```text
LoadIQFullStall
IQEnqPolicyStall
IQ occupancy
```

因为它们可能只是归因改变、指令转移到别的队列，或者用更低 occupancy 换来了额外迁移延迟。

## 11. 最终建议

这个方向有研究价值，但完整的通用 parking queue 不是低成本优化。最现实的路线是：

```text
确认 miss 的 pdest scoreboard
        ↓
消费者占用和 stall 重叠 profiling
        ↓
复用三个 LDU IQ 做依赖感知 steering
        ↓
4～8 entry 的 LDU dependent queue
        ↓
数据充分后再考虑通用 ALU/STA consumer parking
```

第一步最重要的不是写 queue，而是用新增计数器回答：

> 把等待已确认 long-miss load 的消费者移出普通 IQ，理论上到底能释放多少关键 entry、覆盖多少 `stall_cycle_iq`？

如果 shadow profiling 的结果很小，这个方向应及时停止；如果 `mcf/xalancbmk/milc` 中覆盖率高，并且与真实 IQ stall 强相关，再投入独立 dependent queue才合理。
