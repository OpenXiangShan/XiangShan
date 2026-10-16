# Fault 2 主阻塞：Uncache Load Queue 满与 UID862 重试根因

## 1. 结论

问题2当前主阻塞不是 `LoadQueueUncache` 状态机永久卡死，而是测试框架的 issue/recovery 调度制造了一个可重复的资源饥饿环：

1. UID862 是当前最老的 MMIO Load，ROB=`1/133`。
2. DUT 的 16 个 Uncache Load Queue entry 已被比 UID862 年轻的 MMIO Load 占满。
3. 这些年轻 entry 只有到 ROB 头时才能离开 `state0(s_idle)`；当前 `pendingPtr` 指向 UID862，因此它们都不能发送 Uncache 请求。
4. UID862 自己没有 entry 可分配，DUT 按设计产生 Uncache-full rollback，即日志中的 `subtype=5`。
5. redirect 能正常 flush 年轻 entry，freeList 也能正常回收；但测试框架 recovery 后继续使用主表中固定不变的随机 `send_pri`，优先级比较又先于 ROB 年龄，年轻请求会再次抢在 UID862 前面占满 16 个 entry。
6. 上述过程重复，表现为 UID862 长时间重试。UID862 最终被接收、写回并提交 PASS，说明它不是永久 DUT livelock。

责任判断：

| 模块 | 判断 | 依据 |
| --- | --- | --- |
| DUT `LoadQueueUncache` | 未发现功能错误 | full 时产生 rollback；redirect 后 flush；freeList 回收；最终 UID862 能正常发请求、写回、提交 |
| 测试框架 commit sideband | 未发现直接错误 | `pendingPtr=UID862` 与 `pendingMMIOld` 的语义符合当前 ROB 头是 MMIO Load |
| 测试框架 issue/recovery 调度 | 主要责任 | `send_pri` 高于 ROB age；redirect 重放不改变主表 priority，缺少 retry aging/fairness，允许年轻 MMIO 反复抢占有限 Uncache entry |

当前置信度为高。现有 UID862 真实运行没有记录每个候选的 `send_pri` 数值，因此“具体是哪 16 个 priority 值压过 UID862”不能逐项从日志还原；已直接证明的是同一组年轻 ROB owner 在多轮恢复后反复重建、框架启用了 priority-first 调度且 priority 固定在主表 transaction 上。把 `send_pri` 判为重建顺序的主导因素属于代码与运行现象一致的高置信度机制判断，仍需要一次仅关闭 priority 的 A/B 运行做最终因果签核。

## 2. 证据文件

### 2.1 UID862 直接日志

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault2_mmio_owner_20261012/mem_ut/ver/ut/memblock/sim/fault2_sq_deq_guard_compile_20261015_s/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=37617171_rtl_review29_sq_deq_guard_real_1_98_1_seed37617171_20261015_u.log
```

该运行是 seed `37617171` 的真实随机用例，覆盖 UID862 从反复 subtype=5 到最终 PASS 的全过程，但没有 FSDB。

### 2.2 可用真实 seed FSDB

```text
/nfs/home/lixiangrui/work/memblock_ut/XiangShan_V2/.codex_fault2_mmio_owner_20261012/mem_ut/ver/ut/memblock/sim/fault2_r2_survivor_real_1_98_1_20261014_c/wave/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_l2tlb_fault_2pct_no_trigger_seed=37617171_rtl_real_1_98_1_seed37617171_20261014_c.fsdb
```

文件约 34 MiB，只覆盖到约 `159970.9ns`，不能直接观察 UID862 在 `6.09ms` 的现场。本文仅用它验证相同 seed、相同 DUT 上 Uncache-full、redirect flush 和 freeList 回收的真实 RTL 时序。

FSDB 抽取文件：

```text
/tmp/fault2_uncache_full_8450_8700.csv
```

使用的主要层次：

```text
top_tb/U_MEMBLOCK/inner_lsq/loadQueue/uncacheBuffer
top_tb/U_MEMBLOCK/inner_lsq/loadQueue/uncacheBuffer/entries_[0:15]
top_tb/U_MEMBLOCK/inner_lsq/loadQueue/uncacheBuffer/freeList
top_tb/U_MEMBLOCK/inner_uncache
```

## 3. UID862 直接日志链

### 3.1 首次和最后一次 Uncache-full rollback

- 首次：`257258.400ns`，UID862，ROB=`1/133`，`subtype=5`。
- 最后一次：`6091243.400ns`，UID862，ROB=`1/133`，`subtype=5`。
- 共出现 `10073` 次 UID862 subtype=5 重试。

`subtype=5` 的 provenance 明确显示：

```text
candidate input=3
uncache_known=0x7
uncache_free=0x0
```

即 rollback 来源是 Uncache enqueue 无空闲 entry，不是 LoadUnit replay、memory violation 或 raw store 冲突。

### 3.2 `6088370.900ns` 的资源快照

```text
rob=1/133
lq_valid=0xffff
lq_free_mask=0x0000
state_count=16/0/0/0
top_valid=0x0000
inflight=0x0000
idResp=0
lsqResp=0
A.valid=0
D.valid=0
```

16 个 entry 的 ROB 为：

```text
1/136, 0/14, 1/143, 0/40,
0/36, 0/50, 0/75, 0/12,
0/15, 1/137, 0/16, 1/138,
0/23, 0/70, 0/24, 0/3
```

以 ROB 大小 160、当前老请求 `1/133` 为基准，上述 16 项都位于它之后，均是年轻请求。关键特征是：

- 16 项全部 valid，但全部是 `state0`。
- 没有任何 Uncache A/D、idResp 或 LSQ response 活动。
- 16 项中没有 ROB=`1/133`，即最老 UID862 没有获得 Uncache entry。

这不是“总线返回慢导致队列满”，而是“年轻 MMIO owner 占着 entry 等待 ROB 头，真正的 ROB 头反而进不来”。

### 3.3 同一组年轻 owner 长期重复重建

日志不只在 `6088370.900ns` 看到一次满队列。从 `305415.900ns` 开始，到
`6088370.900ns` 为止，`UNCACHE_PROGRESS` 周期性快照一直重复以下 16 个 ROB owner，
仅 entry 槽位顺序有轻微变化：

```text
1/136, 1/137, 1/138, 1/143,
0/3, 0/12, 0/14, 0/15,
0/16, 0/23, 0/24, 0/36,
0/40, 0/50, 0/70, 0/75
```

每次快照都同时满足 `lq_valid=0xffff`、`free_mask=0`、`state_count=16/0/0/0`，
而 pending ROB 始终是 `1/133`。这说明不是 16 个随机 entry 偶然长延迟，也不是
Uncache response 丢失；redirect/recovery 后，相同的静态年轻请求集合被重新 issue 并
再次占据 Uncache owner。

计数也能区分两个容易混淆的资源：

```text
UID862 issue-hit                       10116 次
UID862 subtype=5                      10073 次
UID862 普通 LQ live deq mapping       10088 次
UID862 redirect-deleted LQ mapping        9 次
```

日志里的 `lq=flag/index` 是普通 Load Queue mapping，不等价于
`LoadQueueUncache` 的 16 个 owner entry。UID862 多次拿到普通 LQ mapping、随后由
`lqDeq` 释放，与它长期拿不到 Uncache owner 并不矛盾。真正的 Uncache 请求直到
`6091870.900ns` 才由 responder 接收。

### 3.4 最终退出阻塞

最后一轮链如下：

| 时间 | 事件 |
| --- | --- |
| `6091215.900ns` | UID862 再次 issue |
| `6091243.400ns` | 最后一次 UID862 subtype=5 rollback |
| `6091790.900ns` | UID862 再次 issue；本轮不再成为 full rollback winner |
| `6091823.400ns` | 框架观察到 UID862 普通 LQ mapping，并由 lqDeq sideband 释放该 mapping；这不是 Uncache entry 释放 |
| `6091870.900ns` | Uncache responder 接受地址 `0xa5f1b46`，source owner count=1 |
| `6091933.400ns` | UID862 从固定 MMIO writeback lane LDA2 正常写回 |
| `6091940.900ns` | UID862 ROB 正常 commit |
| `6091941.900ns` | RM data/exception compare PASS |

最终成功证明：

- pending sideband 最终能让 UID862 entry 从 idle 继续推进。
- Uncache request/response 和 MMIO writeback 通路可用。
- UID862 的数据、异常、ROB owner 均正确。

因此长时间不前进不是 DUT entry 状态损坏，也不是 pending pointer 永久错误。

## 4. FSDB 同类完整时序链

该 FSDB 没有覆盖 6.09ms 的 UID862，但覆盖了同一类 16-entry full 场景，可以直接验证 DUT 的 full/flush/free 行为。

| 时间 | 真实信号状态 | 解释 |
| --- | --- | --- |
| `8530.900ns` | 16 entry valid；15 项 `state=00(s_idle)`，entry3=`state=10(s_resp)`；`canAllocate=000`；pending ROB=`0/105` | 队列满，只有当前 ROB 头对应 transaction 在响应，其余年轻 MMIO 等待 |
| `8540.900ns` | rollback ROB=`1/76`；freeMask 仍为 0 | 新请求因没有 entry 产生 full rollback |
| `8575.900ns` | redirect ROB=`1/76` | rollback 被转换为 redirect/recovery |
| `8605.900ns` | entry3 完成或被 flush；`free=0x0008` | 已完成 entry 正常释放 |
| `8620.900ns` | 新一笔 full rollback，ROB=`0/152` | 压力继续，但不是同一 entry 状态机卡死 |
| `8640.900ns` | redirect ROB=`0/152`；多个 `needFlush=1`；`free=0x767d` | redirect 覆盖的 idle entries 当拍产生 flush/free |
| `8645.900ns` | 对应 entry `req_valid` 清零；freeMask=`0x767d` | freeList 在下一拍观察到可用项 |
| `8660.900ns` | `canAllocate=111`；pendingPtr 从 `0/105` 推进到 `0/106` | DUT 已重新允许三路分配，commit sideband 同时正常推进 |
| `8685.900ns` | 下一次 redirect；`free=0x8982` | 其余 owner 被覆盖并释放 |
| `8690.900ns` | 16 个 entry 全部 invalid | 资源能够完全清空，无残留 valid/state owner |

这条波形排除了以下 DUT 故障假设：

- entry 在 `state0` 收到 redirect 后无法 flush；
- `req_valid` 清除但 freeList 不回收；
- freeMask 已释放但 allocate 端仍永久关闭；
- Uncache A/D 或 LSQ response 正在占用资源却被日志漏记。

## 5. 代码机制

### 5.1 `state0` 是合法等待态

文件：`src/main/scala/xiangshan/mem/lsqueue/LoadQueueUncache.scala`

- 第 68 行定义 `s_idle :: s_req :: s_resp :: s_wait`。
- 第 117 行说明 `s_idle` 等待 MMIO 到达 ROB head。
- 第 122-127 行的 `canSendReq` 要求普通 MMIO 同时满足：

```text
req_valid
!needFlush
pendingMMIOld
entry.robIdx == pendingPtr
```

因此 `state_count=16/0/0/0` 不表示 16 个状态机坏死，只表示 16 个 valid entry 都不是当前可发送的 ROB 头。

### 5.2 full rollback 是 DUT 设计行为

同一文件第 561-591 行：若 load pipeline 要 enqueue，但 `s2_enqValidVec` 表明分配失败，则从失败请求中选最老 ROB，生成 `RedirectLevel.flush` rollback。日志 subtype=5 正是该来源。

### 5.3 测试框架优先比较随机 priority

文件：`mem_ut/ver/ut/memblock/seq/base_seq_help/issue_queue_scheduler.sv`

- 第 345-357 行：`compare_pri=1` 时先比较 `send_pri`；只有 priority 相同才比较 ROB age。
- 第 491-501 行：`MEMBLOCK_SEND_PRI_MODE_EN` 直接控制 load candidate 是否按 priority 比较。

文件：`mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv`

- 第 667 行：每个主表 transaction 创建时随机一次 `send_pri`。
- 第 2440-2465 行：priority 从低/中/高区间随机生成。
- redirect/recovery 只重置动态状态，不重新生成主表，因此同一 UID 重放时 priority 不变。

真实 cfg 明确启用：

```text
+MEMBLOCK_SEND_PRI_MODE_EN=1
+MEMBLOCK_GLOBAL_SEND_PRI_EN_WT=1
+MEMBLOCK_SEND_PRI_LOW_WT=1
+MEMBLOCK_SEND_PRI_MID_WT=1
+MEMBLOCK_SEND_PRI_HIGH_WT=1
```

因此该环境并不保证最老 load 先 issue。对普通 cacheable load 这主要是乱序压力；对只能在 ROB 头发送、且只有 16 个 owner entry 的 MMIO load，这会形成资源倒置。

## 6. 完整因果链

以 UID862 为例：

```text
UID862 成为 ROB 头，框架发布 pendingPtr=1/133 + pendingMMIOld=1
    -> 年轻 MMIO 已占满 16 个 Uncache entry
    -> 年轻 entry.robIdx != 1/133，所以全部停在 s_idle
    -> UID862 issue 到 LQ Uncache enqueue，但 freeMask=0
    -> DUT 产生 subtype=5 rollback(ROB=1/133)
    -> redirect flush UID862 及其后的年轻状态，DUT 正常释放 entry
    -> 框架 recovery，把被删除请求重新放回 issue 流程
    -> 固定随机 send_pri 再次让部分年轻 MMIO 先于 UID862 issue
    -> 16 个 entry 再次被年轻 MMIO 填满
    -> UID862 再次 full rollback
```

该环不是“DUT 不释放”，而是“释放后框架又按同一不公平顺序重新填满”。最终某轮调度没有在 UID862 前填满 16 项，UID862 获得 entry 后正常完成。

## 7. 最小解决方案

### 7.1 功能随机回归的最小 cfg 方案

```text
+MEMBLOCK_SEND_PRI_MODE_EN=0
```

关闭后所有请求使用 default priority，scheduler 回退到 ROB age 比较，最老 UID862 会优先于年轻 MMIO issue。该方案不改 DUT，也不减少 MMIO/Uncache、redirect 或异常本身的随机覆盖，仅消除与真实 ROB/issue 公平性不一致的 priority 饥饿。

随机 priority 模式应保留为独立压力用例，而不应作为要求完整跑完的基础功能回归默认调度。

### 7.2 长期框架方案

若必须在功能回归中保留随机 priority，至少增加一种 bounded fairness：

1. 同一 UID 连续收到 subtype=5 达到阈值后，临时提升为 load target 的最高有效 priority，直到成功进入 Uncache entry。
2. 或当当前 `pendingMMIOld` 的 ROB head 正在 retry 时，禁止更年轻的 MMIO load 抢占新的 Uncache entry。
3. redirect recovery 后，对导致 full rollback 的最老 UID 设置一次性 age boost；成功 issue 后立即清除。

不建议修改 DUT 来放宽 `entry.robIdx == pendingPtr`，因为这会允许年轻 MMIO 在 ROB 头之前产生不可回滚的外部访问；也不建议吞掉 subtype=5，因为 full rollback 是 DUT 正确的资源恢复协议。

## 8. 遗漏场景与签核要求

现有证据仍有一个边界：UID862 的直接运行没有 FSDB，而现有 FSDB 只覆盖早期同类场景。若要求对 UID862 本身做逐信号波形签核，需要重新运行 seed `37617171` 并 dump 至至少：

```text
6091780ns .. 6091945ns
```

最少应包含：

- 16 个 `entries[*].req_valid/uncacheState/robIdx/needFlush/flush`
- `io.rob.pendingMMIOld/pendingPtr`
- freeList `freeMask/canAllocate/doAllocate/free`
- Uncache `req/idResp/resp` 和 TL A/D
- rollback/redirect
- MMIO writeback lane LDA2

建议额外在 issue scheduler 诊断中打印当拍 load 候选的 `uid/ROB/send_pri/selected`，并做一次 A/B：同 seed 仅把 `MEMBLOCK_SEND_PRI_MODE_EN` 从 1 改为 0。若 UID862 的 subtype=5 长循环消失，即可把当前高置信度结论提升为直接对照签核。

## 9. 一句话说明

UID862 是排在队首、真正有资格发 MMIO 的请求，但候车区 16 个位置都被后面的请求提前占了；后面的请求因为没到队首又不能发车。DUT 每次都能清场，但测试框架清场后仍按同一随机优先级让后面的请求先占位置，所以 UID862 一直重试，直到某一轮终于先抢到一个位置后正常完成。
