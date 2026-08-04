# DCache 多 Probe Alias 状态支持简易 Plan

| 项目 | 内容 |
|---|---|
| 状态 | coding 已完成，归档至 `do` |
| 目标版本 | V2 |
| 适用对象 | `dcache_mem__access_base_sequence` 轻量 L2 responder |
| 关联 plan | `mem_ut_dcache_multi_probe_tob_control_plan_20260730.md`、`mem_ut_v2_dcache_cbo_probe_closure_plan_20260731.md` |
| 目标 | 在多 Probe、CBO 和轻量 L2 flush 场景下正确保存、使用和切换 DCache VIPT alias |

## 1. 专有名词与抽象功能说明

| 名词 | 本 plan 中的含义 | 状态落点 | 示例 |
|---|---|---|---|
| `physical line` | 64-byte 对齐物理 cache line；memory 数据仍只按该地址索引 | `line_addr` | `0x1000` |
| `active alias` | 当前 DCache 有效副本使用的 alias | `cached_line_record.active_alias` | line `0x1000` 当前 alias 为 `2` |
| `alias conflict` | 同一 physical line 的新 A 请求携带了不同于当前有效副本的 alias | alias transition record | 当前 alias=`2`，新 A alias=`1` |
| `line record` | 保存 physical line 的长期轻量缓存状态，不是完整 L2 directory | `cached_line_by_addr[line_addr]` | 供随机 Probe、CBO 和 alias conflict 查询 |
| `probe record` | 保存一笔已创建 B Probe 的短期请求状态 | pending Probe queue | 保存旧 alias、`toB/toN`、来源和 C reply 状态 |
| `deferred acquire` | 已经真实 A.fire，但必须先完成旧 alias Probe 才能继续构造 D reply 的 A 请求 | alias transition record | 新 alias A 请求不能被丢弃或提前回复 |
| `Probe(toN)` | 要求 DCache 失效当前副本 | `target_cap=toN` | alias conflict、flush、CBOFlush/CBOInval |
| `Probe(toB)` | 要求 DCache 降为 Branch 但继续保留副本 | `target_cap=toB` | 随机 Probe、CBOClean |

## 2. 支持范围与边界

本 plan 在已有多 Probe responder 上增加 alias 生命周期支持：

```text
1. A GrantAck 后记录当前 line 的 active alias。
2. B Probe 使用被 Probe 的旧 active alias 填充 b_data[2:1]。
3. 同一 physical line 出现新 alias 时，先完成旧 alias 的 Probe(toN)，再继续新 A 的 D response。
4. Probe(toN) 与 Probe(toB) 根据各自 target_cap 删除或保留 line record。
5. C Release/ReleaseData 使对应 line record 失效。

alias conflict 判断只作用于 V2 alias 语义有效的 Acquire 类 A request（例如 `AcquireBlock`/`AcquirePerm`）
以及已确认携带 `user_alias` 的请求。`Get`、`Hint`、CBO 和其他不参与 alias transition 的 A request
继续沿用各自原有分类，不因 alias 字段不同而强制进入 alias-resolution。
```

保持以下边界：

- memory backing/overlay 仍只以 physical line 为 key，alias 不参与 memory data key；
- 不建立完整 CoupledL2 directory、set/way、replacement、其他 client coherence 或动态 sink；
- 不新增 alias 随机参数；alias 只来自 DUT A/C 接口和已有 line record；
- 当前 V2 B `data[0]` 仍固定为 `0`，但 responder 必须继续接受 `ProbeAck` 和 `ProbeAckData`；
- `dirty` 与 B `needData` 不合并为一个控制位。本专项只记录 `may_return_data` 作为 C response 接收边界，不用它随机驱动 B `data[0]`。

## 3. 状态结构扩展

### 3.1 `cached_line_record`

抽象功能描述：`cached_line_record` 保存一个 physical line 的长期 alias 状态，供新 A 请求、随机 Probe、CBO
和 flush 使用；它不保存每笔 B Probe 的短期 target_cap，也不承担 D response queue 的职责。

```text
cached_line_record {
    line_addr
    active_alias
    alias_valid
    may_return_data
    data_valid
    lifecycle_state
    deferred_acquire_valid
    deferred_acquire       // 完整 A payload，直到新 GrantAck/E.fire 前保留
}
```

字段规则：

```text
line_addr：64-byte 对齐 physical line；关联数组 key 仍是 line_addr。
active_alias：当前有效副本的 alias；值 0 合法。
alias_valid：不能用 active_alias==0 替代 valid 判断；失效后清零或删除 record。
may_return_data：仅表示此 line 的 Probe C reply 允许走 ProbeAckData 路径；
                 不能直接作为 B.data[0] 的赋值来源。
data_valid：只表示 shared memory/overlay 中是否已保存该 line 的可靠 ProbeAckData；它与 alias_valid
            独立。`ProbeAckData.corrupt=1` 时清零，不表示 DCache 没有完成 toB/toN 协议动作。
lifecycle_state：至少区分 ACTIVE、PROBE_PENDING、ALIAS_CONFLICT、GRANT_WAIT_E、INVALID。
deferred_acquire_valid/deferred_acquire：只在 ALIAS_CONFLICT 到新 GrantAck/E.fire 期间有效；即使旧
alias 已被标记 INVALID，也不得提前删除该 transition record。
```

现有 `cached_alias_by_line[line_addr] = alias` 迁移为 `cached_line_by_addr[line_addr] = record`。
所有原有按 line 选取候选的逻辑继续以关联数组 key 遍历，不扫描主表或 memory backing。

### 3.2 `probe_record`

抽象功能描述：`probe_record` 保存一笔已创建 B Probe 的请求属性，直到唯一匹配的 C `ProbeAck` 或完整
`ProbeAckData` 收敛。多 Probe 时 target 权限必须保存在每笔 record，不能用单一全局变量覆盖。

```text
probe_record {
    probe_token      // 建立 probe_record 时分配、整个 B/C 生命周期保持不变
    line_addr
    probe_alias
    target_cap       // toN 或 toB
    probe_owner      // RANDOM / FLUSH / CBO / ALIAS_CONFLICT
    batch_id         // 所属随机 Probe batch；FLUSH/CBO/ALIAS_CONFLICT 使用对应 owner 标记
    may_return_data
    state
}
```

`probe_token` 是本笔 Probe 生命周期的唯一内部标识；`target_cap` 是后续 C report param 校验、line
删除/保留和 deferred acquire 解锁的依据。C channel 不一定携带该内部 token，因此不能假设可以直接从
C payload 读取 token；实现必须用当前 C 可观察字段（至少 line，必要时 source/其他协议字段）筛选唯一
候选，再通过该候选的 token/owner 访问对应状态。若筛选出 0 笔或多笔候选，必须报错，不能按物理地址
静默删除任意 line。

当收到 `ProbeAckData` 第一拍时，sequence 额外保存 `c_assembly_probe_token`；该 token 在第二拍完成前
不能被其他 Probe 或 C response 覆盖。

共享 Probe queue 的容量固定为 16 笔，不建立 runtime plus 镜像。不同 physical line 可以同时存在多个
`probe_record`；同一 physical line 仍只能存在一笔未收敛 Probe。

## 4. Alias 主 Flow

### 4.1 正常 A 请求

抽象功能描述：`accept_dcache_a_request()` 在 A.fire 后查询同 physical line 的 line record，决定可直接进入
既有 D reply 流程，还是必须先排空旧 alias。它不改变 A fire 事实，也不丢弃已接受 A payload。

```text
计算 A.ready：
  查 cached_line_by_addr[line]；
  如果该 line 已有 deferred acquire、未完成 Probe、或处于 GRANT_WAIT_E/其他非 ACTIVE 状态：
    A.ready = 0；等待已有 owner 收敛；
  如果该请求会触发 alias conflict 且 probe_record 数量已达到 16：
    A.ready = 0；等待 Probe record 释放；不发生 A.fire；
  否则按既有 A.ready 规则继续仲裁。

A.fire(line, new_alias)：
  使用 A.fire 时的 line 和 new_alias 再次查询 cached_line_by_addr[line]；

  无有效 record：
    沿用现有 A -> pending D -> E GrantAck 流程；
    在对应 GrantAck 后建立 active_alias=new_alias 的 ACTIVE record；

  record.active_alias == new_alias：
    沿用现有 A -> pending D -> E GrantAck 流程；
    GrantAck 后刷新该 record 的 ACTIVE 状态；

  record.active_alias != new_alias：
    此时请求必须是 alias 语义有效的 Acquire，且 line 必须仍为 ACTIVE、没有其他 deferred acquire/Probe；
    保存完整 A request 为 deferred acquire；
    record.lifecycle_state = ALIAS_CONFLICT；
    调用 shared Probe service(line, record.active_alias, toN, ALIAS_CONFLICT)；
    不覆盖旧 active_alias；
    不提前生成新 A 的 D response；
```

### 4.2 Alias conflict 收敛

抽象功能描述：`complete_alias_conflict_probe()` 在旧 alias 的 `Probe(toN)` 已收到合法 C reply 后，使旧副本失效，
并把已经保存的 new-alias A 请求交回既有 D response 流程。它不直接伪造 Grant，也不跳过 E GrantAck。

```text
alias-conflict Probe C 完成：
  标记旧 alias 无效，但保留 line record 中的 deferred_acquire；
  取回该 line 对应 deferred acquire，并建立新 Acquire 的既有 D response owner；
  调用既有 A response builder；

新 A 的 Grant/GrantData 完成并收到匹配 E GrantAck：
  建立 active_alias = deferred A.user_alias；
  lifecycle_state = ACTIVE；
  清 deferred_acquire_valid/deferred_acquire；
```

这样 `B.data[2:1]` 始终定位旧 DCache 副本，而新 alias 只在新 GrantAck 后成为 active alias。

## 5. 多 Probe、CBO 与 Flush 复用 Flow

### 5.1 共用 Probe service

抽象功能描述：共用 Probe service 从 line record 读取当前 `active_alias`，建立带 token、target_cap 和 owner
的 probe record，并按照已有 B valid/ready 机制驱动 Probe。它不决定随机权重，也不直接删除 line record。

```text
submit_probe(line, target_cap, probe_owner)：
  要求 line record 为 ACTIVE，或 probe_owner=ALIAS_CONFLICT 时为 ALIAS_CONFLICT；
  要求该 line 没有未完成 Probe；
  要求共享 probe_record 数量小于 16；容量不足时返回“不可提交”，由调用方保持 A.ready=0
  或停止当前随机 batch，不能创建半条记录，也不能报 capacity fatal；
  创建 probe_record(probe_token=allocate_probe_token(),
                    probe_alias=record.active_alias, target_cap, probe_owner)；
  record.lifecycle_state = PROBE_PENDING；
  B payload：
    opcode = Probe；
    param = probe_record.target_cap；
    address = line；
    data[2:1] = probe_record.probe_alias；
    data[0] = 0；
  B.fire 后等待该 probe_record 的 C reply；
```

### 5.2 C Probe reply

抽象功能描述：`complete_probe_reply()` 先按 Probe owner 和唯一 token 候选定位 pending probe，再校验 C 的
physical line、协议字段以及该 record 保存的旧 alias期望值，最后根据 target_cap 决定 line record 的状态。
它不根据 C `echo_isKeyword` 重排 data。旧 alias 是 B request 建立时保存的内部期望值；如果 C payload 没有
alias 字段，则不得伪造“从 C 读取旧 alias”，而应校验当前 C 与该 pending record 的唯一关联。

```text
C ProbeAck/ProbeAckData 完成：
  先由当前 C owner 和 C 可观察字段（至少 physical line，必要时 source）筛选唯一 probe_record；
  通过该唯一 record 取得 probe_token/probe_owner，并校验 line_addr 和 B.fire 时固化的 probe_alias 期望值；
  若无唯一匹配，或 line 与 pending record 不一致：报错并保留状态；
  若 C payload 不携带 alias，则只校验 pending record 中由 B.fire 固化的 probe_alias，不把 C 的缺失字段当作匹配成功；

  如果是 ProbeAckData：
    第一拍必须 C.fire，并将当前 probe_record.probe_token 写入 c_assembly_probe_token；
    第二拍也必须 C.fire，且只能匹配 c_assembly_probe_token 对应的同一 line/owner；
    其他 C response 不得插入、覆盖或完成该 assembly；
    两 beat 完整后：
      corrupt=0：调用现有 physical-line writeback/overlay helper，并置 data_valid=1；
      corrupt=1：不调用 writeback helper，置 data_valid=0，并报 uvm_error；
    无论 corrupt 与否，都继续执行下面的 target_cap/owner 生命周期更新；
    两 beat 校验完成且 corrupt 结果已处理后，立即执行下面的 target_cap/owner 生命周期更新；
    该更新不得以 overlay 写入成功为前提；

  target_cap == toB：
    C.param 只接受 TtoB/BtoB；
    保留 cached_line_record；
    lifecycle_state = ACTIVE；

  target_cap == toN：
    C.param 只接受 TtoN/BtoN；
    如果 probe_owner == ALIAS_CONFLICT：标记旧 alias 无效但保留 deferred_acquire；
    否则删除或标记 cached_line_record 为 INVALID；

  probe_owner == ALIAS_CONFLICT 且 target_cap == toN：
    调用 complete_alias_conflict_probe()；

  删除 probe_record；
```

`ProbeAckData` 继续复用现有两拍 C assembly；`corrupt` 只阻止数据写入，不阻止 Probe 协议收敛。
对 `toN`，line 仍失效并可解除 alias deferred acquire/flush/CBO wait；对 `toB`，line 保留 alias
但 `data_valid=0`，后续轻量 responder 不得把这次 corrupt data 当作可靠 overlay 数据。本 plan 不改变
C data beat 顺序。

### 5.3 调度与不可抢占规则

已有协议事务一旦成为 owner，不得被新 Probe 抢占。`C` 两拍 assembly、pending `D`、已发出的 `B`
Probe 等待 C、GrantAck 等待 `E` 都必须先完成或按既有错误策略收敛。没有不可抢占 owner 时，新的
Probe 创建策略按以下优先级选择：

```text
ALIAS_CONFLICT Probe
    > CBO Probe
    > FLUSH Probe
    > RANDOM Probe
```

该优先级只决定“谁可以创建下一笔 Probe”，不改变当前 C assembly、D hold 或 E hold 的协议完成优先级。
同一 physical line 同时只能有一个未收敛 Probe；其他来源的请求进入等待状态，不能覆盖现有
`probe_record` 或静默复用其 token。

### 5.4 CBO 与 L2 flush

```text
CBOClean hit：
  submit_probe(line, toB, CBO)；
  C reply 完成后保留 active alias，再返回 CBOAck。

CBOFlush/CBOInval hit：
  submit_probe(line, toN, CBO)；
  C reply 完成后删除 line，再返回 CBOAck。

L2 flush snapshot：
  只复制 lifecycle_state=ACTIVE 的 line record；
  每条调用 submit_probe(line, toN, FLUSH)；
  每条 C reply 完成后失效对应 line。
```

建立 flush snapshot 前，必须先收敛已有 alias-resolution pending、普通 Probe、C assembly、D hold 和
E hold；alias-resolution pending 不能被 flush 清除、覆盖或改成 flush owner。snapshot 只保存建立时已
存在的 ACTIVE record，flush 开始后由新 GrantAck 建立的 alias record 不加入本轮 snapshot。flush 中同一
line 若已有 alias Probe 在途，不能再创建 flush Probe，必须等待原 Probe 完成后再决定该 line 是否仍在
本轮 snapshot 中。

随机 Probe 继续使用已有 `MEMBLOCK_L2_PROBE_*` 参数选择 line 与 `toB/toN`。正在 alias transition、等待 C
或等待 GrantAck 的 line 不可被同一 batch 再次选择。

## 6. 与现有逻辑的关系

| 现有逻辑 | 本 plan 的变化 |
|---|---|
| `cached_alias_by_line` 只保存 alias | 替换为保存 alias、valid、data-response 边界和生命周期的 line record |
| B Probe 从全局 `pending_probe_alias` 取 alias | 每笔 probe record 保存 `probe_alias` 和 `target_cap` |
| Probe C reply 一律按 `toN` 删除 line | 按 `target_cap` 保留 `toB` 或删除 `toN` |
| 新 A alias 可覆盖已有 map alias | alias conflict 先 Probe 旧 alias，旧副本收敛后再服务新 A |
| CBO/flush 各自处理 line | 统一调用 Probe service，仍保留各自的 toB/toN 策略 |

本 plan 不改变：A/C/D/E 基本握手、GrantData `echo_isKeyword`、C data two-beat assembly、主存 physical
line key、response delay、动态 sink/outstanding 专项的职责。

## 7. 关联计划与文档同步

- 多 Probe/toB plan 落地前必须先实现本 plan 的 line/probe record 与 alias conflict 基础能力；
- CBO Probe closure plan 复用本 plan 的 Probe service 和 `target_cap`，不得另建第二套 alias 状态；
- multi-Probe/flush plan 必须复用本 plan 的 `probe_token`、`probe_owner`、同 line 单 Probe 约束和
  alias-resolution 优先级；
- coding 后同步更新 DCache L2 responder flow、DCache agent interface 分析和对应 implementation review；
- 本 plan 不实现 RM、scoreboard 或 covergroup。

## 8. 与初步 plan 差异说明

本次补充不改变三种 alias 场景的功能目标，只把 coding 时必须遵守的生命周期和匹配条件明确化。

### 8.1 实现功能差异

修改前仅要求保存 `line_addr/alias/target_cap` 并完成 Probe 闭环；修改后增加：

```text
建立 Probe record -> 分配稳定 probe_token
C response -> 用 owner/可观察字段唯一定位 record -> 再使用 token 访问状态
同 line 已有 Probe 或 deferred acquire -> 禁止重复 A/Probe
flush snapshot -> 先等待 alias pending，且不吸收 flush 开始后的新 alias
```

这不是新增 DUT 协议字段，而是补齐测试框架内部状态生命周期，避免多 Probe 或 flush 时误清理、误匹配。

### 8.2 调度和 helper 差异

`submit_probe()` 从“创建带 target 的 Probe 状态”扩展为同时创建唯一 token；`complete_probe_reply()` 从
“按 pending Probe 处理 C response”扩展为先完成唯一候选匹配，再进行 `ProbeAckData` 两 beat 收集和
physical-line writeback，最后执行 toB 保留、toN 失效或 alias deferred acquire 解锁。

已有 C assembly、pending D 和 GrantAck/E owner 仍优先完成；`ALIAS_CONFLICT > CBO > FLUSH > RANDOM` 只用于
没有不可抢占 owner 时的新 Probe 创建。

### 8.3 行为边界

仍保持 A/C/D/E 握手、B `data[0]=0`、memory physical-line key 和动态 sink/outstanding 专项边界不变。
C payload 不携带 alias 时不虚构 C alias 字段；旧 alias 只作为 B request 的内部期望值参与关联校验。

## 执行中补充/修正（IMPLEMENTATION_DELTA）

`[IMPLEMENTATION_DELTA]`

- 来源：执行前复核发现 multi-Probe、CBO 和 l2Flush plan 都要求复用 alias plan 的
  `cached_line_record/probe_record/token`，而本 plan 又写成“已有多 Probe responder”，形成循环依赖。
- 原 plan：alias state、Probe service、CBO/flush 调用关系在同一文档描述，未单独声明最小可独立落地的
  lifecycle foundation。
- 实现调整：本 plan 的首个提交只建立唯一共享 foundation：`cached_line_record`、固定 16 笔
  `probe_record` queue、稳定 `probe_token`、单 B hold/多笔 `WAIT_C`、C line->token 唯一匹配、
  ProbeAckData 两拍 token 锚定，以及 alias conflict 的真实
  `A.fire -> Probe(toN) -> C completion -> deferred Acquire -> GrantAck` 闭环。
- 兼容边界：已有 `MEMBLOCK_L2_PROBE_ENABLE_WT` 的 legacy random Probe 继续只生成单笔 `toN`；
  foundation 的 `target_cap`、`probe_owner` 与 `submit_probe()` 已支持后续 `toB/CBO/FLUSH` 调用，
  但本提交不新增 multi-batch 权重、不驱动 `io_l2_flush_done`、不建立 CBO deferred context。
- 原因：先使所有后续 Probe policy 使用同一套 record/token/C assembly owner，避免三个专项各自维护
  `pending_probe_*` 或 line map，造成同 line 重复 Probe、C response 误匹配或 alias 覆盖。
- 影响范围：DCache responder 内部状态与握手仲裁；不修改 interface、agent、主表、LSQ、Uncache、
  response delay、动态 sink 或 D-error 专项。后续 multi-Probe/flush、CBO plan 必须在此 foundation 上
  扩展，不能恢复旧单 Probe owner。

## 执行结果

- 已将 `cached_alias_by_line` 与单一 `pending_probe_*` 状态迁移为
  `cached_line_by_addr`、`probe_record_q`、稳定 `probe_token` 和单一 B hold。
- 已实现不同 alias `Acquire` 的 `A.fire -> deferred Acquire -> Probe(toN) old alias -> C completion ->
  normal Grant/GrantData -> E.fire new alias ACTIVE` 闭环；旧 alias 不会在新 Grant 前被覆盖。
- 已实现 `ProbeAckData` 两拍 token 锚定和 `corrupt` 的“跳过 overlay 写回但继续协议收敛”行为。
- legacy random Probe 仍只产生单笔 `Probe(toN)`；multi-batch/toB、CBO 和 l2Flush 没有混入本实现。
- 验证：独立 `alias_foundation` 目录完成 VCS 编译；独立非分区 `alias_smoke` 目录运行
  `basicTest/memblock_dispatch_real_smoke_vseq` 通过，`UVM_ERROR=0`、`UVM_FATAL=0`。
