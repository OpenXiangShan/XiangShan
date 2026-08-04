# DCache/Uncache 返回延迟分组控制专项 Plan

> 执行完成说明（2026-08-04）：本 plan 已落地 DCache/Uncache 独立 response record、四档 delay、
> 可选乱序返回、DCache 动态 sink、ReleaseData reservation 与 Uncache opcode 白名单。验证结果和
> 与原 plan 的三项执行中修正在
> `AI_DOC/plan/test_framework/review_doc/undo/mem_ut_dcache_uncache_response_delay_control_implementation_review_20260804.md`。
> 本文件归档到 `plan/do` 后保留原始设计内容与 `IMPLEMENTATION_DELTA`，当前行为以 flow、源码分析
> 和 implementation review 为准。

## 专有名词与抽象功能说明

- `admission delay`：从 responder 已确认 A-channel `fire` 到 response record 进入本通道 ready queue 的固定等待；
  DCache 为 2 拍，Uncache 为 0 拍。
- `response scheduling delay`：response record 进入 ready queue 后，由本通道延迟权重选择的额外等待时间；
  它不改变 A-channel 握手，也不包含 DUT 对已经有效 D response 施加的 `D.ready` backpressure。
- `scheduler timer`：每个通道独立维护的一轮返回调度计时器。timer 在该通道首次存在可调度 record 时
  采样一次 response scheduling delay；到期前不得选择 D response，到期后才执行一次返回仲裁。
- `0 cycle`：表示没有额外的 response scheduling delay；它仍然要经过 admission、ready queue 和“入队当拍不可返回”
  的正常边界，不是组合路径直返。
- `DCache`：`auto_inner_dcache_client_out_a/d` coherent TileLink 通道。其 L2 responder 已有
  `pending_d_due_cycle` 延迟调度。
- `Uncache`：当前 `auto_inner_buffers_out_a/d` 的 A/D-only TL edge，源码类名仍为
  `sbuffer_mem_access_base_sequence`；本专项按实际 Uncache/MMIO memory responder 语义描述，
  不新增不存在的独立 SBuffer 通道。
- `outstanding response`：已完成请求 `fire`、已经建立回复记录、但尚未完成最后一个 D-channel beat 的在途响应。
  DCache 和 Uncache 分别维护自己的 outstanding 账本。
- `effective outstanding max`：本拍某通道实际可接受的最大在途 response 数。它不大于 compile-time
  物理上限；本专项中 DCache 和 Uncache 都固定按 16 笔 response record 管理。DCache 的 16 笔是
  `Grant/GrantData`、`CBOAck`、`ReleaseAck` 共用的一张 response record 表；Grant 的 sink 等待
  账本不占用该表，实际并发数由 DUT 的 A/C-channel `fire` 自然决定。
- `ready queue`：某个通道内已经满足最早返回边界、等待该通道返回调度器选择的 response 队列。
- `eligible_cycle`：response record 允许参加返回仲裁的最早周期。它用于隔离“入队”和“可被选中”两个事件，
  保证本拍新入队的 record 不会被本拍再次选中。
- `staging/incoming queue`：保存本拍新完成 admission 或 delay 到期、但尚未开放给本拍仲裁的 record 的暂存队列。
  实现时可以使用独立暂存队列，也可以直接写入 ready queue 并依靠 `eligible_cycle` 过滤；本 plan 推荐后者。
- `dynamic sink`：DCache `Grant/GrantData` response 分配的 TileLink `sink` 标识。每笔需要
  `GrantAck` 的 response 使用独立有效 sink，收到匹配 `GrantAck` 后释放。
- `ORDERED/REORDER`：通道内 response 选择模式。`ORDERED` 从 ready queue 队首返回；
  `REORDER` 从该通道已经 ready 的 response 中随机选取。DCache 和 Uncache 独立配置。
- `opcode 白名单`：Uncache responder 在真实 A-channel `fire` 后接受的请求类型集合。当前 V2
  `auto_inner_buffers_out_a_*` 只允许 `PutFullData`、`PutPartialData` 和 `Get`；白名单外的
  TileLink opcode 不建立 response record，而是立即报告框架输入非法。
- `response kind`：Uncache response record 保存的语义类别，只能是 `STORE_ACK` 或
  `LOAD_DATA`。它由 A opcode 一次解码决定，后续延迟、乱序选择和 D backpressure 只移动该 record，
  不得重新解释其读写语义。
- `D hold watchdog`：仅在 Uncache 已选中一笔 D response、`D.valid=1` 而 DUT 长期不给
  `D.ready` 时记录的一次性诊断计数器。它不超时中止、不释放 response record，也不改变正常
  backpressure 合同。
- `error normalization`：在 response record 创建时，把公共 memory backend 返回的原始
  `denied/corrupt` 收敛为当前 D opcode 合法的 TileLink 字段组合。它不是错误注入，不改变
  error 来源，也不创建 exception 事件。
- `error-injection plan`：D response 错误位的权重、一次采样和格式归一化的唯一所有者，本文通过
  `apply_uncache_d_error_injection()` 调用，不重复保存其参数或随机逻辑。

本专项采用新的默认返回行为：DCache 和 Uncache 默认均启用 `1..10` cycle 的 SMALL 延迟档位；
`0 cycle`、`10..100 cycle`、`101..1000 cycle` 只有在对应权重被显式配置为非零时才参与随机。默认
`REORDER_EN=0`，因此默认仍按各自通道队列顺序返回。

## 支持功能与修改原因

现有 `MEMBLOCK_L2_RSP_DELAY_*_WT` 只覆盖 DCache responder，且只有三档延迟；Uncache A/D 回复在 A 接收后立即
构造，无法表达独立的 memory return latency。本专项将 DCache 延迟扩展为四档，并为 Uncache 新增独立的四档
返回延迟权重，使 cached 与 uncache 访问可以分别覆盖 `0 cycle`、短、中、长四类返回等待。新默认值明确为
DCache/Uncache 的 SMALL 权重为 `1`、其余三档为 `0`，因此两侧默认都随机等待 `1..10` cycle。
这会改变旧 smoke 中 DCache 约 `3..5` cycle 以及 Uncache 即时回复的默认时序，相关 testcase 必须按 response
record 的实际返回边界等待，不能继续假设旧固定时序。

同时，当前 responder 仍偏向单笔 pending 模型，无法覆盖 DUT 支持的多 outstanding 和乱序返回场景。
本专项将 DCache 与 Uncache 拆成两套独立 response pipeline：两侧分别管理 outstanding 上限、
ready queue、返回延迟、顺序/乱序选择和 backpressure 保持。任一通道满、等待 D.ready 或等待
GrantAck 时，不阻塞另一通道的请求接收和 response 返回。

其中 DCache response record 与 Grant sink 是两层独立资源：所有 D reply 先竞争统一的 16 笔
record；只有 `Grant/GrantData` 还需要一个可用 sink。最后一个 D.fire 立即归还 record，而该 Grant 的
sink 继续保留到匹配 E.fire。因此已有多个未确认 GrantAck 时，只会耗尽 Grant sink 并阻塞新的 Acquire，
不会把 CBOAck/ReleaseAck 的 response record 容量一并占住。

### DCache 返回延迟权重

| 参数 | 新随机区间 | 默认权重 | 作用范围 |
|---|---:|---:|---|
| `MEMBLOCK_L2_RSP_DELAY_ZERO_WT` | `0 cycle` | `0` | DCache |
| `MEMBLOCK_L2_RSP_DELAY_SMALL_WT` | `1..10` cycle | `1` | DCache |
| `MEMBLOCK_L2_RSP_DELAY_MEDIUM_WT` | `10..100` cycle | `0` | DCache |
| `MEMBLOCK_L2_RSP_DELAY_LARGE_WT` | `101..1000` cycle | `0` | DCache |

### Uncache 返回延迟权重

| 参数 | 新随机区间 | 默认权重 | 作用范围 |
|---|---:|---:|---|
| `MEMBLOCK_UNCACHE_RSP_DELAY_ZERO_WT` | `0 cycle` | `0` | Uncache |
| `MEMBLOCK_UNCACHE_RSP_DELAY_SMALL_WT` | `1..10` cycle | `1` | Uncache |
| `MEMBLOCK_UNCACHE_RSP_DELAY_MEDIUM_WT` | `10..100` cycle | `0` | Uncache |
| `MEMBLOCK_UNCACHE_RSP_DELAY_LARGE_WT` | `101..1000` cycle | `0` | Uncache |

`10` 同时属于 small/medium 的用户指定区间；先按权重选择档位，再在该档位内均匀随机，因此不存在
歧义。DCache 与 Uncache 各自四档权重允许任意单档为非零，但不得四档全零。默认四档配置为
`ZERO=0、SMALL=1、MEDIUM=0、LARGE=0`；用户将其他档位配置为非零后，才改变默认延迟分布。

### Outstanding 与返回选择参数

| 参数 | 默认行为 | 作用范围 |
|---|---|---|
| `MEMBLOCK_L2_RSP_REORDER_EN` | `0` 表示顺序返回，`1` 表示乱序返回 | DCache |
| `MEMBLOCK_UNCACHE_RSP_REORDER_EN` | `0` 表示顺序返回，`1` 表示乱序返回 | Uncache |

以上参数属于公共测试框架 runtime 行为参数，通过 `plus.sv -> seq_csr_common -> getter` 读取。
通道真实物理上限使用 compile-time DUT 宏描述：

```text
MEMBLOCK_DUT_DCACHE_A_MAX_OUTSTANDING = 16
MEMBLOCK_DUT_UNCACHE_MAX_OUTSTANDING  = 16
```

前者对应 V2 DCache 的 16 个普通 miss/Acquire source，不能用顶层 6-bit source 编码宽度推导为 64；
后者对应 `UncacheBufferSize=16`，不能仅用顶层 4-bit source 宽度替代实际准入规则。两个 outstanding
上限均由 compile-time 宏固定，不建立 runtime plus 镜像，也不得建立第二套硬件结构权威。测试框架不读取或推导
DUT 的 Uncache outstanding CSR，也不按 MMIO/NC 属性主动缩小该队列容量；DUT 是否实际发送多笔请求，
由 DUT 自身的 A-channel `fire` 行为决定。

## 公共延迟采样 Flow

抽象功能描述：`sample_dcache_response_delay()` 和 `sample_uncache_response_delay()` 分别为已经被接受的
DCache/Uncache 请求选择首拍 D response 到期周期；它们只返回 delay，不创建 transaction、修改主存或改变任一
通道握手。

```text
读取 DCache 的四个 MEMBLOCK_L2_RSP_DELAY_*_WT：
  四项全零：uvm_fatal；
  使用 std::randomize(... dist ...) 选择 ZERO/SMALL/MEDIUM/LARGE；
  在选中档位的区间内随机一个 cycle 数并返回。

DCache response scheduler：
  在没有正在驱动的 D response、没有运行中 scheduler timer，且存在 eligible record 时，
  按 MEMBLOCK_L2_RSP_DELAY_*_WT 为下一轮 DCache 返回调度选择一次 delay；
  timer 到期后，只从 DCache ready queue 的 eligible record 中选择一笔 response 返回。

Uncache response scheduler：
  在没有正在驱动的 D response、没有运行中 scheduler timer，且存在 eligible record 时，
  读取 Uncache 的四个 MEMBLOCK_UNCACHE_RSP_DELAY_*_WT；
  四项全零：uvm_fatal；
  使用 std::randomize(... dist ...) 选择 ZERO/SMALL/MEDIUM/LARGE；
  在选中档位的区间内随机一个 cycle 数并返回；
  timer 到期后，只从 Uncache ready queue 的 eligible record 中选择一笔 response 返回。
```

将当前 DCache 私有的 `sample_l2_response_delay()` 重构为 `sample_dcache_response_delay()`；再新增
`sample_uncache_response_delay()` 作为 Uncache 专用入口。两者共享同一套档位选择算法，但分别读取各自的
四档权重。延迟采样只更新区间和调度时间；DCache 的 D beat 内容、GrantAck 副作用、Hint、Probe
或 `cached_alias_by_line` 生命周期不因延迟档位本身改变。

## Uncache Opcode 准入 Flow

抽象功能描述：`decode_uncache_a_opcode()` 在 Uncache A-channel 真实握手后，把请求分类为普通
store、普通 load 或当前 DUT 不会合法产生的 opcode。它只返回 `response kind` 或失败，不访问主存、
不修改 outstanding 账本，也不产生 D-channel payload。

### 修改原因

旧 `is_store_opcode()` 只把 opcode `0/1` 识别为 store，所有其它值都会落入 load 分支并获得
`AccessAckData`。V2 `Uncache.scala` 只会产生 `PutFullData`、`PutPartialData` 和 `Get`；因此把
`ArithmeticData`、`LogicalData`、`Hint`、`Acquire*`、`CBO*` 静默伪装为 load，不是扩展支持，
而是错误隐藏。该检查属于 responder 输入合法性，必须在建立 response record 之前完成。

### `decode_uncache_a_opcode()` 详细逻辑

输入：已在本拍 A.fire 固定的 `opcode/source/address/size/param/mask/data` 快照。

输出：`STORE_ACK`、`LOAD_DATA` 或 `uvm_fatal`；没有状态副作用。

```text
decode_uncache_a_opcode(a_opcode)：
  case a_opcode:
    PutFullData(0)、PutPartialData(1)：
      返回 STORE_ACK；

    Get(4)：
      返回 LOAD_DATA；

    其它值：
      uvm_fatal，日志包含 opcode、source、address、size、param；
      不调用主存读写 helper；
      不创建 response record；
      不改变 outstanding、ready queue、timer 或 D hold；
```

这里不新增 `ArithmeticData/LogicalData/Hint/Acquire*/CBO*` 的模拟。它们不是当前 V2 Uncache
TL-UL 端口的合法 DUT producer；后续只有设计新增真实 producer，且另行定义 request、data update、
error、LSQ completion 生命周期后，才可扩展白名单。NC/MMIO `cbo.zero` 若被 DUT 拆成多个普通 store，
外部看到的仍是合法 `Put*`，无需在本 helper 中识别 CBO。

### Uncache response record 创建修改

抽象功能描述：`create_uncache_response_record()` 对已通过 opcode 准入的 A.fire 请求完成一次主存访问，
并创建不可在同拍返回的 response record。它复用既有 response delay/outstanding 管理，不负责 opcode
校验或后续 D-channel 仲裁。

```text
Uncache A.fire：
  kind = decode_uncache_a_opcode(已固定的 A 快照)；

  kind == STORE_ACK：
    以 A address/mask/data 执行 sparse memory 写；
    record.d_opcode = AccessAck；
    record.d_data = 0；

  kind == LOAD_DATA：
    以 A address/mask 执行 sparse memory 读；
    record.d_opcode = AccessAckData；
    record.d_data = 读出的 64-bit data；

  两种合法 kind：
    调用错误专项的 error-injection helper
    apply_uncache_d_error_injection(kind, 主存返回的 denied/corrupt)；
    record 保存原 A source、size、归一化后的 denied/corrupt 和 kind；
    按既有 Uncache admission 规则放入 ready queue；
    设置 eligible_cycle = t + 1；
```

不得再通过 `is_store = (opcode == 0 || opcode == 1)` 的布尔 fallback 推导 D opcode。`response kind`
在 record 创建时固定；D.ready=0 时 driver 只保持该 record 的既有 payload，不重新调用 decode 或主存访问。

### Uncache error 字段专项依赖

抽象功能描述：`apply_uncache_d_error_injection()` 由
`AI_DOC/plan/test_framework/plan/do/mem_ut_v2_dcache_d_error_weight_adapt_plan_20260803.md` 实现，负责将 memory backend error 与
Uncache error 权重的单次采样结果合并，并映射为当前 D opcode 合法的 payload。本 response-delay plan
只在 response record 创建时调用它一次；不持有权重、不重复实现归一化，也不在 D hold 期间再次调用。

```text
本 plan：
  先完成 Uncache opcode 白名单，输出 STORE_ACK 或 LOAD_DATA；
  再把 kind、backend_denied、backend_corrupt 传入 error-injection helper；
  保存 helper 返回的 denied/corrupt 到本条 response record；

error-injection plan：
  用 MEMBLOCK_UNCACHE_DENIED_WT/MEMBLOCK_UNCACHE_CORRUPT_WT 一次采样；
  AccessAckData：合并采样结果并保证 denied -> corrupt；
  AccessAck：只允许 denied，corrupt 固定 0；
  D ready hold：不重新采样。
```

因此本 plan 不再单独定义 `normalize_uncache_d_error()`。错误专项必须保证 `AccessAckData` 的
`denied -> corrupt` 和 `AccessAck.corrupt=0`；DUT 内部对 denied/access fault、corrupt/hardware error
的消费仍不在两个 responder plan 中实现。

## DCache/Uncache 独立返回 Flow

抽象功能描述：DCache 和 Uncache responder 在各自请求真实 `fire` 后建立 response record；
record 先进入本通道 outstanding 账本，再按本通道的 admission 边界、ready queue、延迟调度和
顺序/乱序策略完成 D-channel 返回。两个通道的接收、计时、队列、仲裁和 backpressure 保持互不共享。

```text
DCache response record 准入：
  Grant/GrantData、CBOAck、ReleaseAck 共用固定 16 笔 DCache response record；
  A-channel Acquire/CBO 或 C-channel Release/ReleaseData 只有在将建立的 D response record 有空位时才 fire；
  record 已满时，对应 A.ready 或 C.ready=0，等待任一 response 的最后一个 D.fire 释放 record；

DCache Acquire A.fire：
  先要求共享 DCache response record 有空位；
  再要求 Grant sink pool 有空位；
  两者都满足才接收并建立 Grant/GrantData record；
  record 已满阻塞所有需要新 D response 的 DCache 输入；sink 已满只阻塞需要 Grant 的 Acquire，
  不阻塞可建立 CBOAck 或 ReleaseAck 的 CBO/Release 路径；
  记录 fire 周期 t，并先固定等待 2 拍；
  在 t+2 将 record 放入 DCache ready queue，同时设置 eligible_cycle = t+3；
  因此该 record 最早在 t+3 参加返回仲裁。

DCache CBO A.fire / C Release(数据或无数据).fire：
  只要求共享 DCache response record 有空位；
  分别建立 CBOAck 或 ReleaseAck record，不分配 Grant sink；
  后续复用同一 DCache admission、ready queue、delay、ORDERED/REORDER 和 D hold 流程。

Uncache 请求真实 fire：
  如果 Uncache response record 数量小于 MEMBLOCK_DUT_UNCACHE_MAX_OUTSTANDING（固定为 16），
  先执行 opcode 白名单解码；仅 `PutFullData/PutPartialData/Get` 可以接收并建立 Uncache response record；
  其它 opcode 立即 uvm_fatal，不能伪装为 load record；
  否则保持 Uncache A.ready=0，直到已有 record 的 D response 完成并释放容量；
  不增加额外 admission delay，在 t 拍放入 Uncache ready queue，同时设置 eligible_cycle = t+1；
  因此该 record 最早在 t+1 参加返回仲裁。

每个通道独立运行 response scheduler：
  没有当前 D hold、没有运行中 timer 且 ready queue 存在 eligible record 时：
    按本通道权重采样一次 response scheduling delay；
    设置本通道 timer 的 due cycle；
  timer 未到期：
    保持 timer，不重新采样 delay，也不选择 response；
  timer 到期后：
    根据本通道 order mode 只从 eligible record 中选择 response；
    ORDERED：选择 ready queue 中最早的 eligible record；
    REORDER：从 eligible_cycle <= current_cycle 的 response 中随机选一笔；
    驱动选中 response 的 D.valid 和 payload，并清除本轮 timer；

D.ready=0：
  保持当前通道正在驱动的 D.valid/payload；
  不重新采样 timer、不重复选择、不重复释放 outstanding；
  不影响另一通道继续接收或返回。

D 最后一个 beat fire：
  删除对应 response record，立即归还共享 DCache/Uncache response capacity；
  更新本通道 outstanding 计数；
  DCache Grant/GrantData 才进入 GrantAck 等待账本，其 sink 直到 E.fire 匹配后释放；
  CBOAck、ReleaseAck 与 Uncache AccessAck/AccessAckData 不保留 sink。
```

### Uncache D.ready 长期等待诊断

抽象功能描述：`service_uncache_d_hold_watchdog()` 只观察当前正在驱动的 Uncache D response 是否
长期没有发生 D.fire，并在达到固定阈值时打印一次诊断。它不参与 admission、延迟随机、返回仲裁、
主存访问或 response record 生命周期。

本轮使用 sequence-local 固定阈值，不新增 plus 参数：

```systemverilog
localparam int unsigned UNCACHE_D_READY_WARN_CYCLES = 1000;
int unsigned d_hold_cycles;
bit          d_hold_timeout_reported;
```

不复用 `MEMBLOCK_DISPATCH_READY_TIMEOUT`、`MEMBLOCK_LSQENQ_READY_TIMEOUT` 或全局 active-sequence
no-progress 参数。它们描述 issue 或全局调度等待，不能准确表示“当前这一笔 Uncache D response 被
D.ready 阻塞”的周期数。

```text
service_uncache_d_hold_watchdog(current_d_hold)：
  reset，或 current_d_hold 无效：
    d_hold_cycles = 0；
    d_hold_timeout_reported = 0；
    返回；

  current_d_hold.D.fire：
    D response 由既有 scheduler 正常完成和释放；
    d_hold_cycles = 0；
    d_hold_timeout_reported = 0；
    返回；

  current_d_hold.D.valid == 1 且 D.ready == 0：
    d_hold_cycles++；
    若 d_hold_cycles >= UNCACHE_D_READY_WARN_CYCLES 且尚未报告：
      uvm_warning，打印 source、D opcode、size、address、response kind、
      denied/corrupt、record 建立周期和当前等待周期数；
      d_hold_timeout_reported = 1；

  D.ready 为 X/Z：
    继续由已有接口 X/Z 检查 fail-fast；
    watchdog 不把未知 ready 解释为普通 backpressure；
```

当前旧阻塞式 `sbuffer_mem_access_base_sequence::body()` 在等待 `D.ready` 的循环内维护同一计数；
本专项改为非阻塞 scheduler 后，由每拍处理 current D hold 的分支调用该 helper。两种实现只能在
每个 driver clocking 边界加一次计数，不能因同一拍重复 `send_sbuffer_xaction()` 而重复累加。

达到阈值后仍继续保持原 D payload 并等待 D.fire；不得清 D.valid、删除 response record、释放
outstanding、修改 main memory、改变 global stop、pass/fail 或 terminal。该逻辑只提供卡住时的
一次性定位信息，不把正常长 backpressure 判为 DUT 功能失败。

### 入队与返回仲裁的周期边界

本专项明确采用“队列保存 record，`eligible_cycle` 控制可选周期”的实现方式，不通过额外
`pre_pkt_gap/post_pkt_gap` 或阻塞式 `repeat(delay)` 插入握手空拍。

```text
每个通道每拍开始：
  先执行本拍返回仲裁；
  只扫描该通道 ready queue 中 eligible_cycle <= current_cycle 的 record；
  本拍新加入的 record 即使已经写入 ready queue，也不能命中本拍仲裁。

本拍请求/延迟处理完成后：
  将新 record 写入对应通道 ready queue；
  为该 record 设置 eligible_cycle；
  下一拍由对应 scheduler 重新扫描。
```

每个通道的 `scheduler timer` 只在“准备发起一轮返回调度”时采样一次 delay，而不是每个周期重复随机。
重复采样会不断推迟 due cycle，使 ready queue 在某些权重配置下长期得不到返回机会。DCache 和 Uncache 的
timer、due cycle 和随机权重读取完全独立；任一侧等待 delay 或等待 D.ready 时，另一侧仍可独立启动或完成自己的返回调度。

因此“当拍不能被选中”不是依赖 `if` 判断跳过某个偶然句柄，而是由 record 的时间戳成为统一约束：

- Uncache 在周期 `t` 真实 fire 并入队，`eligible_cycle=t+1`；在选择 `0 cycle` 调度延迟时，最早周期 `t+1` 返回；
- DCache 在周期 `t` 真实 fire，固定 admission delay 到周期 `t+2` 入队，`eligible_cycle=t+3`；在选择 `0 cycle`
  调度延迟时，最早周期 `t+3` 返回；
- 若实现采用独立 `staging/incoming queue`，则本拍只对进入仲裁前的 ready queue 快照仲裁，拍末再合并暂存项，
  其可见效果必须与上述 `eligible_cycle` 规则一致。

这里的 DCache “固定延迟 2 拍”定义为从真实 A-channel fire 到进入 ready queue 的 admission delay；
随机延迟档位定义为 ready record 进入返回调度后的额外等待策略。总的 A-fire 到 D-valid 延迟由两者共同决定，
并且还要叠加 D.ready backpressure；实现不得把随机 delay 误当成新的 A-channel gap。

不得用 `pre_pkt_gap/post_pkt_gap` 或阻塞 `repeat(delay)` 伪造延迟；前者不是该 responder 的时序
所有权，后者会使 reset、global stop 和 D.ready 观察滞后。使用每通道 record queue、ready queue
和非阻塞 cycle counter 表达延迟与返回选择。

## Dynamic Sink Flow

抽象功能描述：DCache 侧对需要 `GrantAck` 的 `Grant/GrantData` response 分配动态 sink，并用 sink
把 D-channel response 和后续 E-channel `GrantAck` 关联起来。Uncache 如当前协议不需要 GrantAck，
不额外引入 sink 账本。

```text
DCache Grant/GrantData response record 创建：
  已通过共享 response record 准入后，从空闲 sink pool 分配一个 sink；
  将 sink 写入 response payload；
  在 response record 中保存 sink owner。

DCache Grant/GrantData 最后一拍 D.fire：
  删除 response record，归还统一的 16 笔 response capacity；
  将该 sink 转入 grant_ack_wait 表；sink 仍不可复用。

观察到 E.fire：
  按 E.bits.sink 查找 grant_ack_wait 表；
  命中后完成 cached_alias_by_line 更新；
  释放该 sink，允许后续 DCache Grant/GrantData 复用。
```

sink 分配只服务 DCache TileLink `Grant/GrantData` 生命周期；`ReleaseAck`、`CBOAck` 和 Uncache
`AccessAck/AccessAckData` 不进入 GrantAck sink 表，也不因其他 Grant 等待 E.fire 而失去自身的
response record 准入机会。

## 生命周期与保持不变边界

- reset 清除 DCache 和 Uncache 各自的 outstanding record、ready queue、当前驱动保持状态和
  DCache sink/GrantAck 等待账本。
- global stop 必须等待两个通道的 outstanding、ready queue、当前 D hold 和 DCache GrantAck
  等待账本均收敛后再退出，不能因为 A 已被接受就提前结束 sequence。
- DCache 和 Uncache 各自维护 response record；DCache 内部的 `Grant/GrantData`、`CBOAck`、`ReleaseAck`
  必须共用固定 16 笔 record，不能再为 CBO 或 Release 建立旁路容量。两通道之间不得共享 response
  payload、source、sink、delay timer、ready queue 或 D handshake owner。共享的只有 delay 采样算法，
  不共享权重参数。
- 主存数据读写、byte mask、DCache GrantAck、Probe、L2 flush 和 memory overlay 的正常语义不因本专项
  改变。Uncache opcode 从旧的“非 store 即 load”隐式 fallback 改为显式白名单，但合法 `Put*/Get` 的
  读写和 source/size 回传保持不变。Uncache `denied/corrupt` 的格式归一化与 runtime injection 由
  `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_dcache_d_error_weight_adapt_plan_20260803.md` 统一负责；本 plan 仅在 record 创建时调用它。
- Uncache D hold watchdog 只产生一次 `uvm_warning`，不构成 timeout completion 或 error recovery；
  正常长 backpressure 仍必须等真实 D.fire 后才释放 response record。
- `MEMBLOCK_DELAY_0_WT`、`MEMBLOCK_DELAY_1_20_WT`、`MEMBLOCK_DELAY_21_50_WT` 保持 dispatch 内部
  delay 参数，不能接入本专项。

## 参数与文档同步

- `env/plus.sv`、`seq_csr_common.sv`、`seq/plus_cfg/default.cfg` 保留同名字段并更新中文注释、
  参数作用范围和四档区间说明。
- `env/plus.sv`、`seq_csr_common.sv`、`seq/plus_cfg/default.cfg` 需要同步新增 Uncache 的四档返回延迟权重入口
  和 DCache/Uncache 顺序/乱序返回选择入口。DCache/Uncache outstanding 上限只由 compile-time DUT 宏和
  typed localparam 提供，不建立 runtime plus 镜像。
- 已有 DCache L2 responder 专项文档和 preset 中的权重名称需要更新为四档；`SMALL_WT` 的说明改为 `1..10`，新增 `ZERO_WT` 说明为 `0 cycle`。
- 新增/更新 Uncache memory responder flow 文档时，使用 Uncache 语义说明 `auto_inner_buffers_out_a/d`，
  并备注历史源码类名为 `sbuffer_mem_access_base_sequence`。

## 与原测试框架逻辑对比和修改类型总结

原逻辑：DCache 使用旧的约 `3..5` cycle 延迟分布，Uncache 在 A.fire 后直接建立回复；DCache/Uncache
主要按单笔 pending 状态处理。

修改后：

```text
DCache Acquire/CBO/Release fire -> 共用 16 笔 response record -> 固定 admission 2 拍 -> ready queue -> 默认随机 1..10 拍 -> D response
Uncache A.fire   -> opcode 白名单 -> admission 0 拍 -> ready queue -> 默认随机 1..10 拍 -> D response
最后一个 D.fire 归还 response record；仅 Acquire 的 Grant sink 继续等待 E.fire。两侧分别维护
outstanding、timer、ready queue 和 backpressure；默认 ORDERED，显式开启后才 REORDER。
```

本次属于默认时序和响应调度功能变更，不是单纯字段适配。新增 Uncache delay、DCache/Uncache
response record、多 outstanding 和独立 scheduler；DCache/Uncache 的 D payload、GrantAck/E 生命周期、
memory 数据内容和 Probe/CBO 状态语义不因延迟档位改变。另有一项最小防御性逻辑修改：旧
`is_store_opcode()` 将全部非 store opcode 静默当作 load；修改后只允许 V2 实际产生的 `Put*/Get`，
白名单外 opcode 在 response record 创建前 `uvm_fatal`。该修改不新增业务 opcode 支持，也不改变
正常 scalar MMIO/NC load/store 的主框架控制行为。另新增 sequence-local 的 Uncache D hold warning
watchdog：旧逻辑在 D.ready 长期为 0 时只会等待、没有定位信息；新逻辑在固定 1000 个 driver
clocking 边界后打印一次 response 快照，但仍保持并等待真实 D.fire。它是 debug 防御性修改，不是
timeout abort、pass/fail 或 terminal 逻辑变更。Uncache D error 字段的协议归一化及随机注入已由
`AI_DOC/plan/test_framework/plan/do/mem_ut_v2_dcache_d_error_weight_adapt_plan_20260803.md` 统一完成：它扩展的是合法 D payload 的
错误激励，不改变本 plan 的延迟调度、LSQ 异常优先级或主框架控制行为。

## 执行中补充/修正（IMPLEMENTATION_DELTA）

### 1. outstanding 上限的编译期权威

`[IMPLEMENTATION_DELTA]`

- 来源：当前项目参数规则明确要求物理 port、buffer 或 outstanding 深度不能建立 runtime plus 镜像。
- 原 plan：参数同步段仍列出 DCache/Uncache outstanding 上限的 plus 入口，与前文“compile-time 宏固定 16”冲突。
- 实现调整：在 `memblock_compile_params.svh` 增加 `MEMBLOCK_DUT_DCACHE_A_MAX_OUTSTANDING=16` 和
  `MEMBLOCK_DUT_UNCACHE_MAX_OUTSTANDING=16`，并在 `memblock_dispatch_types.sv` 暴露 typed localparam。
  DCache/Uncache responder 直接消费这些编译期值；runtime 参数仅保留四档 delay weight 和 reorder enable。
- 原因：防止 testcase plus 宣称改变 DUT 能接收的物理 in-flight 深度，形成第二权威。
- 影响范围：参数定义、response record 的容量检查和相关参数文档；不改变运行期随机行为。

### 2. Uncache error helper 的跨专项调用边界

`[IMPLEMENTATION_DELTA]`

- 来源：`apply_uncache_d_error_injection()` 的实现和权重属于
  `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_dcache_d_error_weight_adapt_plan_20260803.md`；本 response-delay
  plan 完成时当前源码尚不存在该 helper，后续专项已在同一 record 创建点实现它。
- 原 plan：要求 response-delay plan 在 record 创建时调用该 helper；若直接实现会把后续错误注入功能混入
  本专项并使单专项提交无法独立编译。
- 实现调整：本专项先在 `create_uncache_response_record()` 固定 response kind、memory backend 返回的
  `denied/corrupt`、source/size/data 和 record 生命周期；对于当前无数据 `AccessAck`，保持 corrupt 为 0。
  后续 error-injection plan 只在这个 record 创建点接入一次 helper，覆盖 backend error 与 injection weight，
  不改动 scheduler、queue 或 D hold。
- 原因：保证 delay/outstanding 专项可独立编译验证，同时保留错误位的唯一后续写入点，避免重复采样。
- 影响范围：本专项不新增 error plus、不实现权重随机或 error normalization；合法 backend error 的当前传递
  保持最小行为，后续专项负责完整协议归一化。

```text
本专项创建 Uncache response record：
  解码 Put*/Get；
  调用 shared memory backend，保存当前 backend denied/corrupt；
  AccessAck 固定 corrupt=0；
  建立 delay/eligible/queue record；

后续 error-injection 专项：
  只在同一 record 创建点调用一次 error helper；
  覆盖 record 的 denied/corrupt；
  D hold 或 scheduler 取出 record 时不再随机。
```

### 3. Hint 与最终 D response 的绑定

`[IMPLEMENTATION_DELTA]`

- 来源：在 `REORDER` 模式下，scheduler timer 启动时看到的候选 record 与 timer 到期时最终选出的
  record 可以不同；若在 timer 启动时直接发送 Hint，会让 Hint 的 source/isKeyword 与实际 D response 脱钩。
- 原 plan：只要求 Hint 生命周期不因 delay 档位改变，没有规定 timer 与 REORDER 并存时 Hint 的绑定时点。
- 实现调整：`accept_dcache_a_request()` 仍只对 `AcquireBlock` 采样一次 Hint 并把字段写入所属
  response record；`service_dcache_response_scheduler()` 只有在 timer 到期、真正把该 record 转为
  `current_d_record` 时，才将 Hint 写入 `dcache_hint_q`。同一轮 `service_hint()` 输出该 Hint；D.ready
  backpressure 不会重发，也不会改写 D payload。
- 原因：保证任何已发送 Hint 都对应本轮实际选择的 GrantData，避免乱序返回下的跨 record 错配；新的
  `0 cycle` 档位也不再需要伪造旧的固定提前拍数。
- 影响范围：只改变 Hint 的 responder 内部排期点，不改变 Hint 是否采样、payload 来源、D response
  内容、GrantAck/E 生命周期或主表/LSQ 控制逻辑。
