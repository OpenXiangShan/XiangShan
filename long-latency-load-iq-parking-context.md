# 长延迟 Load IQ Parking：续接上下文

这是一份供后续继续分析或实现时使用的压缩上下文。完整分析见同目录：

- `long-latency-load-iq-parking.md`

## 当前目标

研究 XiangShan 后端中“长延迟 load 的消费者占据普通 IQ，导致独立指令无法进入”的问题，并判断是否值得实现 dependent/parking queue。

用户当前最关心的问题是：

> load 是 producer，consumer 已经因为等待 load 卡在 IQ 中。这个 consumer 到底应该怎么处理？

## 仓库和数据

- 当前实现仓库：`/nfs/home/zhaozukang/kmh-v3/xs-env/XiangShan`
- 分析时的 commit：`ff4720da4`
- 另一个同 commit 的源码目录：`/nfs/home/zhaozukang/source/XiangShan`
- 性能报告：`/nfs/home/cirunner/perf-report/cr260806-c7b373bdb-DefaultConfig`
- 原始分析文档：`/nfs/home/zhaozukang/source/feature/long-latency-load-iq-parking.md`

## 已核实的关键事实

### 1. 原始标量 load 不会长期占据 LDU IQ

标量 load 成功进入 MemBlock 后，`Region.scala` 通过 `snResp.finalSuccess + lqIdx` 让原 LDU IQ entry 释放。

关键代码：

- `src/main/scala/xiangshan/backend/Region.scala:527-534`
- `src/main/scala/xiangshan/backend/issue/EntryBundles.scala:210-217`

因此真正长期占用 IQ 的主要是：

- 等待 load 数据的 consumer；
- 地址 source 尚未 ready 的 younger load；
- 其他因依赖链被 load miss/replay 拖住的 uop。

不要实现“miss load 自己提前释放 LDU IQ entry”，当前标量路径已经较早释放。

### 2. 现有 `loadDependency` 不能充当 long-miss producer tag

当前：

```text
LoadPipelineWidth   = 3
LoadDependencyWidth = 2
```

它记录的是 load 预测唤醒后未来一两级的 cancel 依赖，用于匹配 `ld1Cancel/ld2Cancel`。它不是 ROB/LQ/pdest 的长期 producer 身份。

关键代码：

- `backend/rename/BusyTable.scala:96-176`
- `backend/issue/EntryBundles.scala:207-325`
- `backend/Bundles.scala:1840-1845`

所以需要新增 long-load scoreboard 或 producer tag，不能直接把现有 `loadDependency` 当作 parking 分类依据。

### 3. mcf 的证据

样本：`mcf_6463_0.0229526`。

```text
完整仿真：40M 指令，23,285,309 cycle，IPC 1.717821
```

报告中途清过计数器；最后统计区间约为 11,160,414 cycle。

```text
stall_cycle_iq = 5,908,484，约 52.9%
waitLduCycle   = 6,091,781，约 54.6%
stall_cycle_rob = 0
stall_cycle_lsqFull = 4,085
```

三个 LDU IQ 均为 20 entry，平均占用约 10.8～11.0；但 ready=0 的周期约 75%～77%，ready≤1 的周期约 95%～96%。

最后区间：

```text
dcache_miss_first_issue ≈ 0.933M
dcache_miss             ≈ 1.925M
replay_fire             ≈ 1.528M
fast_replay_fire        ≈ 0.392M
replay_dcache_miss      ≈ 0.798M
load execute latency    ≈ 11.05 cycle/load
load commit latency     ≈ 28.21 cycle/load
```

结论：load miss/replay、not-ready IQ 占用和 IQ 反压同时存在；但当前计数器尚未证明这些 not-ready entry 中有多少由已确认 long-miss load 造成。

## 对“卡在 IQ 的 consumer 怎么处理”的直接答案

### 核心原则

如果只在原 IQ entry 中增加 `parked` bit，entry 仍占普通 IQ 容量，几乎没有解决容量反压。

要释放普通 IQ entry，必须：

```text
把 consumer 的完整 uop 从普通 IQ 原子地搬到一个小型 Parking Queue；
load 最终完成后更新 consumer 的 source ready；
然后把 consumer 限速迁回它可执行的普通 IQ。
```

Parking Queue 是较便宜的等待结构：不参加每周期 oldest-ready select，也不需要完整的普通 IQ issue 端口。

### 第一版建议只处理一种 consumer

只处理：

```text
标量 load address consumer，且唯一未就绪 source 是已确认 long-miss load。
```

建议判定：

```scala
isCandidate = uop.isScalarLoad &&
  src0IsReg &&
  !src0Ready &&
  longMissTable(src0Psrc) &&
  allOtherSourcesReady
```

这样 Parking Queue 只需要等待一个 `{regType, pdest}`，不必复制通用 ALU IQ 的完整多源 wakeup 网络。

### Parking entry 至少保存什么

```text
完整 uop / payload
ROB index
目标 IQ mask 或原 IQ ID
wait source index
wait producer pdest
寄存器类型
各 source ready 状态
valid / releaseReady
```

如果后续支持通用 ALU consumer，还要保存多个 `psrc`、多个 ready bit，并接入全部相关 wakeup。

### producer 如何标记为 long miss

在 Load Unit S2 确认 DCache miss，例如 `cause(C_DM)` 时，设置：

```text
longMissInt[pdest] = 1
longMissFp[pdest]  = 1
```

在最终成功 writeback 时清除。Rename 重新分配该物理寄存器时也要清除，防止 flush 后 pdest 重用污染新指令。

更严谨的 tag 可以是：

```text
{regType, pdest, generation}
```

或同时携带 producer ROB/LQ 信息。但最小原型可先用 pdest scoreboard，加严格的 flush/allocation 清除断言。

### 新 dispatch 的 consumer 怎么进入 Parking Queue

这是最低风险的第一版功能：

```text
Dispatch 查询所有 source psrc
       ↓
若唯一未就绪 source 命中 longMiss scoreboard
       ↓
Parking Queue 有空位：直接进入 Parking Queue，不进入普通 IQ
       ↓
Parking Queue 满：fallback 到普通 IQ，或按普通规则阻塞 Dispatch
```

它不需要从现有 IQ 抽取 entry，改动和验证成本较低。

局限是 miss 在 LDU S2 才确认，较早 dispatch 的 consumer 已经进入普通 IQ，第一版覆盖不到它们。

### 已经卡在普通 IQ 的 consumer 怎么搬走

这是第二版功能。不要同周期扫描并搬走全部 consumer，建议每个 IQ 每周期最多 eviction 一个：

```text
IQ entry 动态查询 longMiss scoreboard
       ↓
候选条件：valid && !issued && !canIssue &&
          唯一未就绪 source 命中 longMiss
       ↓
从候选中选择 oldest 一个
       ↓
Parking Queue ready 时，复制完整 entry
       ↓
只有 parkEnq.fire 后才清除原 IQ entry
```

原子性规则：

```text
先确认 Parking Queue 成功接收，再清除原 IQ entry；
Parking Queue 满时 entry 留在原 IQ；
不能先清除再尝试写入。
```

建议接口：

```scala
val parkDeq = Decoupled(new ParkEntry)
```

内部清除条件类似：

```scala
clearByPark := parkDeq.valid && parkDeq.ready && parkSelOH(entryIdx)
```

这一版需要修改 entry clear、AgeDetector 更新和 entry 输出，风险高于 Dispatch 直接分流。

### load 完成时怎么唤醒 parked consumer

不要使用 LDU S0 的预测性 wakeup作为释放条件，因为 miss/replay 后它可能被 cancel。

Parking Queue 应等待最终成功 writeback：

```text
final load WB: {regType, pdest}
       ↓
与所有 parking entry 的 waitPdest 比较
       ↓
匹配 entry 的 wait source 置 ready
       ↓
entry.releaseReady = 所有 source 均 ready
```

同一 load 可以匹配多个 consumer，所以可同周期把多个 entry 标成 `releaseReady`。但不必同周期全部迁回普通 IQ。

### 怎么迁回普通 IQ

```text
releaseReady entries
       ↓
选择 oldest 一个或少量 entry
       ↓
根据 FU mask / IQ occupancy 选择目标 IQ
       ↓
与 Dispatch enqueue 仲裁
       ↓
目标 IQ 真正接收后清除 Parking entry
```

建议每周期迁回 1 个，最多 2 个，以免 load 返回时多个 consumer 同时涌入，增加 enqueue 端口和时序压力。

仲裁必须避免饥饿：可以让 ready parking entry 获得周期性优先级，或在等待超过阈值后提升优先级。

### replay、cancel、flush 怎么处理

- **load replay**：consumer 继续留在 Parking Queue，不释放。
- **预测 wakeup/cancel**：Parking Queue 最小版忽略预测 wakeup，只认最终 writeback，因此不需要反复 wake/cancel。
- **ROB flush**：每个 entry 用 `robIdx.needFlush(redirect)` 清除。
- **producer 被 flush**：它的 younger consumer 也应被同一 redirect 清除；scoreboard 还必须清掉 producer 状态。
- **pdest 重用**：在新物理寄存器 allocation 时清 scoreboard；必要时引入 generation bit。
- **queue 满**：fallback 到普通 IQ，保证功能和前进性。

## 推荐实现步骤

### Step 0：只加计数器

先实现 long-miss scoreboard，但只用于统计：

```text
valid_wait_long_load
valid_wait_only_long_load
stall_cycle_iq_with_long_load_waiter
dispatch_long_load_dependent
```

若 `valid_wait_only_long_load` 很小，不进入功能实现。

### Step 1：只分流新 dispatch 的 scalar LDU consumer

- 4 entry Parking Queue；
- 只接收唯一未就绪 source 命中 longMiss 的标量 load；
- 只认 final WB；
- 每周期最多迁回 1 条；
- queue 满 fallback。

这是建议的最小可复现功能版本。

### Step 2：增加普通 LDU IQ 的后台 eviction

- 每个 LDU IQ 每周期最多找 1 个候选；
- 全局每周期最多搬 1 条到 Parking Queue；
- `parkEnq.fire` 后才清原 entry；
- 保留 ROB age/公平性信息。

### Step 3：数据有效后扩到 ALU/STA consumer

这会引入多 source wakeup、更多目标 IQ 和更大的 CAM/广播开销，不应作为第一版。

## 性能收益来自哪里

Parking 不会让 producer load 更早返回，也不会消除依赖本身。收益只来自：

```text
等待 long load 的 consumer 不再占用昂贵的普通 IQ entry
        ↓
更多独立 uop 可以 Dispatch
        ↓
更多独立计算、地址生成或其他 cache miss 可以提前执行
        ↓
提高 MLP / 后端并行度，降低 stall_cycle_iq
```

如果普通 IQ 本来不缺 entry，或者绝大多数 consumer 在 miss 确认前就已进入 IQ且没有 eviction，收益会很小。

## 验收指标

必须同时看：

```text
IPC
stall_cycle_iq
stall_cycle_iq_with_long_load_waiter
valid_wait_only_long_load
Parking Queue occupancy
park enqueue / release / migrate / fallback
每个 IQ valid_cnt / ready_cnt
load replay / cancel
flush 和 pdest reuse 断言
Fmax / area / power
```

重点 workload：`mcf`、`xalancbmk`、`milc`、部分 `gcc`、`soplex_ref`。

回归组：`lbm`、`gromacs`、`gamess_*`。

## 当前文件状态

- 完整分析已经复制到当前仓库根目录：`long-latency-load-iq-parking.md`
- 本文件是当前对话和技术判断的压缩上下文。
- 尚未修改 RTL。
- 下一步若开始实现，先做 Step 0 profiling，不直接做完整 Parking Queue。
