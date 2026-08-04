# mem_ut V2 DCache CBO 命中 Probe 闭环专项 Plan

> **Alias 状态复用**：本 plan 的 CBO `Probe(toB/toN)` 必须复用
> `mem_ut_dcache_multi_probe_alias_state_plan_20260803.md` 定义的 `cached_line_record`、`probe_record`
> 和共用 Probe service；不得维护独立 alias map 或覆盖其他 Probe 的 target_cap。

| 项目 | 内容 |
|---|---|
| 状态 | `undo`，仅完成方案设计，尚未 coding |
| 目标版本 | V2 |
| 测试框架入口 | `dcache_mem__access_base_sequence::body()` |
| 主要修改文件 | `mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv` |
| 方案原则 | 复用现有 Probe、C assembly 和 DCache 共用 response record scheduler，不建立完整 L2 directory/MESI/CHI 模型 |
| 创建日期 | 2026-07-31 |

## 1. 术语与抽象功能说明

| 术语 | 含义 |
|---|---|
| `CBOClean` | 对指定 cache line 执行 clean。命中时要求上层副本降为共享，使用 `Probe(toB)`；line 仍保留 |
| `CBOFlush` | 对指定 cache line 执行写回并失效。命中时使用 `Probe(toN)`，完成后删除 line 候选记录 |
| `CBOInval` | 对指定 cache line 执行失效。命中时使用 `Probe(toN)`，完成后删除 line 候选记录 |
| `cached_alias_by_line` | 旧逻辑中的 `{64B 物理 cache line 地址 -> DCache alias}` 视图；coding 时必须迁移到 alias plan 的 `cached_line_record`，不得在 CBO plan 中另建 alias map |
| `CBO hit` | CBO 地址按 64B 对齐后在 alias plan 的 `cached_line_by_addr` 中存在有效 `cached_line_record` |
| `CBO miss` | CBO 地址按 64B 对齐后不存在有效 `cached_line_record` |
| `Probe(toB)` | L2 要求 DCache 将 line 降为共享但继续保留有效副本 |
| `Probe(toN)` | L2 要求 DCache 使 line 失效 |
| `Probe owner` | 负责一笔 B Probe 发送以及对应 C `ProbeAck/ProbeAckData` 收敛的唯一状态所有者 |
| `C assembly` | 对 `ProbeAckData` 的两个 32B C beat 进行收集、校验和写回处理的状态 |
| `CBOAck` | L2 对 DCache CBO A request 的最终 D-channel 单拍完成响应；不等待 E `GrantAck` |
| `direct CBOAck` | CBO miss 时不需要 Probe，直接建立 `CBOAck` pending response |
| `DCache response record` | 返回延迟专项统一管理的固定 16 笔 D reply 容量；`Grant/GrantData`、`CBOAck`、`ReleaseAck` 共用，CBOAck 不分配 sink |

本 plan 中所有函数描述先说明抽象职责，再说明状态和控制流。这里的“支持 CBO Probe 闭环”只表示
测试框架对当前已知 DCache cache line 进行最小、可收敛的 Probe 交互，不表示复刻完整 CoupledL2 的
directory、dirty owner、MSHR、CHI 下游写回或多 client 仲裁。

## 2. 当前问题与目标

当前 `dcache_mem__access_base_sequence` 已支持：

```text
CBOClean/CBOFlush/CBOInval A.fire
    -> 建立单拍 CBOAck
    -> CBOAck.fire 后：Clean 保留 map，Flush/Inval 删除 map
```

当前缺失：

```text
CBOClean 命中 cached_alias_by_line
    -> 没有 Probe(toB)

CBOFlush/Inval 命中 cached_alias_by_line
    -> 没有 Probe(toN)
```

这会使测试框架只验证了 CBO response opcode 和影子表删除，没有验证 CBO 触发的 B/C coherent
交互，也没有验证 CBOAck 必须等待 Probe 完成后才发送。

本 plan 的目标是：

1. CBO 地址命中 `cached_alias_by_line` 时，按 CBO 类型发起正确的 `Probe(toB/toN)`。
2. 等待对应的 `ProbeAck` 或完整 `ProbeAckData` 后，再生成 `CBOAck`。
3. `CBOClean` 保留 line 候选记录；`CBOFlush/CBOInval` 在 Probe 完成后删除 line 候选记录。
4. CBO 地址未命中时保持当前直接 `CBOAck` 行为。
5. 不改变现有普通 Acquire、随机 Probe、Release、Uncache、主表、TLB 和 LSQ 流程。

## 3. 范围与最小改动边界

### 3.1 本轮支持

```text
CBOClean hit    -> Probe(toB) -> ProbeAck/Data -> CBOAck
CBOFlush hit    -> Probe(toN) -> ProbeAck/Data -> 删除 line -> CBOAck
CBOInval hit    -> Probe(toN) -> ProbeAck/Data -> 删除 line -> CBOAck
CBO miss        -> CBOAck
```

Probe 使用 alias plan 的 `cached_line_record.active_alias` 填充 B channel alias payload。Probe 的 B/C handshake、
`ProbeAckData` 两 beat 收集和已有主内存写回 helper 继续复用现有实现。

### 3.2 本轮不支持

- 完整 L2 directory、MESI 状态、dirty/owner 精确建模。
- CBO 期间的多 client Probe、多个 CBO 并发、动态 sink 或完整 CHI 下游事务。
- `needData` 随机策略；`ProbeAckData` 是否出现由 DUT 决定，responder 必须能收敛两种合法 C response。
- 改变现有普通随机 Probe 的默认权重和调度策略。
- 新增 CBO plus 参数、CBO 专用 agent、monitor 或 virtual sequence。
- CBO request 的主动伪造。当前 CBO A request 仍由 DUT CMOUnit 产生，本 plan 只完善 responder。

## 4. 状态设计

### 4.1 复用与新增状态

优先复用现有（其中 alias 状态和 Probe owner/token 统一由 alias plan 的共享 service 提供）：

```text
pending_d_valid / pending_d_kind
pending_d_cbo_opcode / pending_d_line_addr / pending_d_source
pending_probe_b_valid / waiting_probe_c
pending_probe_line / pending_probe_alias
probe_record / probe_token / probe_owner
c_assembly_owner / c_assembly_*
cached_line_record / cached_alias_by_line 兼容访问视图
```

新增最小状态：

```text
bit                 cbo_context_valid;
bit                 pending_cbo_probe_valid;
bit [3:0]           pending_cbo_probe_opcode;
bit [47:0]          pending_cbo_probe_line;
bit [1:0]           pending_cbo_probe_cap;
probe_token_t pending_cbo_probe_token; // 使用 alias plan 定义的内部 token 类型
```

实现时如现有 `pending_probe_*` 已能完整保存 CBO line/alias，可不重复增加 line/alias 字段；新增状态
只保留 CBO deferred context（CBO opcode、line 和关联 token）。CBO A.fire 必须调用共享
`submit_probe(..., CBO)`，由共享 service 创建 `probe_record.probe_owner=CBO`；不得用 CBO 本地枚举覆盖
`ALIAS_CONFLICT/FLUSH/RANDOM`，也不得同时维护两套普通 Probe/CBO Probe pending payload 镜像。

状态生命周期：

```text
CBO context 无效：没有已接受的 CBO deferred request。
CBO context 有效：保存 CBO opcode、line 和共享 probe_record.probe_token，C response 完成后转入 CBOAck pending。
共享 Probe service 的 owner 由 probe_record.probe_owner 统一维护。
```

本 CBO flow 同一时刻只允许一个 CBO deferred context；全局 Probe service 可以保存多笔不同 line 的
record/token。Alias conflict/CBO/flush/random 的新建优先级由 alias plan 统一管理；CBO Probe 在途时，禁止新的 CBO A request 和普通随机 Probe 复用同一 line；已有
当前拍已确认的 transaction 继续按既有规则完成。C channel 的 `Release/ReleaseData` 仍沿用现有
等待 Probe 时的兼容路径，可以先完成对应 `ReleaseAck`；该 ReleaseAck 不得清除 CBO Probe owner，
也不得替代最终 CBOAck。

`cbo_context_valid` 覆盖 CBO A.fire 后的完整生命周期：direct CBOAck pending、CBO Probe B hold、等待
C response、C assembly 以及 CBOAck D hold。该标志只有在 CBOAck.fire 后清除。context 有效期间，如果
新的 A request 是 CBO opcode，则保持 `A.ready=0`；DUT 可以保持 `A.valid`，不能把未 fire 的 valid 当作
第二笔 CBO 已接受。不得建立 CBO context queue，也不得覆盖当前 `pending_cbo_probe_token`。

所有 CBOAck 必须向
`mem_ut_dcache_uncache_response_delay_control_plan_20260730.md` 的共享 DCache response record scheduler
申请容量：CBO A.fire 前统一预检并保留一个 CBO response record；CBO hit 在 Probe 等待期间也占用该
record，Probe 完成后只填充该 record 的 CBOAck payload。record 满时所有 CBO A 都不开放 A.ready。
CBOAck 不申请 Grant sink，其他 Grant 等待 E.fire 不得单独阻塞它。

## 5. CBO A request 接收逻辑

### 5.1 `accept_dcache_a_request()`

抽象功能：在真实 A.fire 后分类 DCache coherent request。对于 CBO，保存原始 opcode、source 和
cache line 地址，并根据共享 `cached_line_record` 的 ACTIVE 命中结果决定直接回复或进入 Probe 闭环。

原逻辑：

```text
CBO A.fire
    -> pending_d_kind = CBO_ACK
    -> 选择 response delay
    -> 等待 D.ready
    -> CBOAck.fire 后更新 map
```

修改后伪代码：

```text
计算 CBO A.ready（A.fire 前）：
  line = line_addr64(req.address)
  如果 cbo_context_valid 且 req.opcode 属于 CBOClean/CBOFlush/CBOInval：
    A.ready = 0；等待当前 CBOAck 完成；
  如果 req.opcode 属于 CBOClean/CBOFlush/CBOInval，且共享 DCache response record 已达到 16：
    A.ready = 0；等待已有 response 的最后一个 D.fire 释放 record；不发生 CBO A.fire；
  如果 line 有 cached_line_record 但 lifecycle_state != ACTIVE，或已有 probe_record/deferred_acquire：
    A.ready = 0；等待已有 owner 收敛；
  否则 A.ready 沿用现有 CBO 仲裁。

accept_dcache_a_request(req, accept_cycle)（仅在真实 A.fire 后调用）：
  检查 size == 6、地址 64B 对齐、source == 17；
  要求 cbo_context_valid == 0；建立唯一 cbo_context，并保存 opcode/source/line；
  保存 pending CBO 的 opcode/source/line；

  if line 没有有效 cached_line_record：
    // 保持当前 miss 行为
    复用已保留的共享 response record，设置 kind=CBO_ACK；
    由 shared response scheduler 选择 due cycle；
    return

  if cached_line_by_addr[line].lifecycle_state != ACTIVE
     || line 已有 probe_record
     || cached_line_by_addr[line].deferred_acquire_valid:
    该分支理论上不会发生；如果 DUT 已在不允许的状态下产生 A.fire，报 uvm_error/fatal，不能覆盖现有状态；
    return

  要求 submit_probe() 返回有效 token；如果 queue 已满而仍发生该分支，报错且不得建立 CBOAck；

  // CBO hit 行为
  pending_cbo_probe_valid = 1
  pending_cbo_probe_opcode = req.opcode
  pending_cbo_probe_line = line

  if req.opcode == CBOClean:
    pending_cbo_probe_cap = toB
  else if req.opcode == CBOFlush || req.opcode == CBOInval:
    pending_cbo_probe_cap = toN
  else:
    fatal

  pending_cbo_probe_token = 共享 submit_probe(line, pending_cbo_probe_cap, CBO) 返回的 probe_token；
  B hold、B payload、B.valid 和 B.fire 均由 shared Probe service 根据该 record 调度；
  // 此时不建立 CBOAck pending D，必须等 Probe C response 完成
```

CBO hit 的 `A.fire` 只通过共享 service 建立 `probe_record(probe_owner=CBO)`，不得同时建立
`pending_d_valid=CBO_ACK`，否则 sequence
可能在 Probe 尚未完成时提前发送 CBOAck。

CBO miss 虽不建立 Probe record，仍必须建立 `cbo_context_valid`，直到 direct CBOAck.fire 后清除；因此
miss 路径同样不允许下一笔 CBO A.fire。

## 6. CBO Probe B channel 逻辑

### 6.1 `build_pending_probe_xaction()` 或现有 Probe builder

抽象功能：根据 Probe owner 生成稳定的 B-channel payload，并在 B.ready 未打开时保持 payload 不变。

修改后伪代码：

```text
build_probe_payload(cycle_xact)：
  cycle_xact.b_valid = shared Probe service 当前选中 record 的 b_hold_valid
  cycle_xact.b_bits_opcode = Probe
  cycle_xact.b_bits_size = 6
  cycle_xact.b_bits_source = 0
  cycle_xact.b_bits_address = active_probe_record.line_addr
  cycle_xact.b_bits_mask = all_ones
  cycle_xact.b_bits_data = 0
  cycle_xact.b_bits_data[2:1] = active_probe_record.probe_alias
  cycle_xact.b_bits_param = active_probe_record.target_cap
```

当前随机 Probe 仍按原规则使用 `toN`，其默认行为不因本 plan 改变。CBO Probe 的 cap 只能由 CBO
opcode 决定：Clean 为 `toB`，Flush/Inval 为 `toN`，不得使用随机 `Probe(toB)` 权重覆盖 CBO 语义。

### 6.2 B.fire 状态转移

```text
如果 shared Probe service 当前 b_hold_valid && b_ready：
  shared Probe service 将当前 record 的 b_hold 标记为已 fire
  waiting_probe_c = 1
  保留 active_probe_record.probe_token/probe_owner、pending_probe_line、pending_cbo_probe_opcode
  不删除 cached_line_by_addr[pending_probe_line]；由 Probe 完成阶段按 target_cap 处理
```

B.fire 只代表 Probe 已发送，不能代表 DCache 已完成降级/失效，也不能提前发送 CBOAck 或删除 map。

## 7. C channel response 逻辑

### 7.1 `start_c_assembly()`

抽象功能：接收 Probe 的第一拍 C response，校验 line/size/response 参数，建立单拍或多拍 C owner，
并区分普通 Probe 和 CBO Probe 的完成后动作。

修改后伪代码：

```text
收到 ProbeAck：
  检查 waiting_probe_c == 1
  通过共享 Probe service 的唯一 C owner/可观察字段定位 probe_record；
  检查 address == active_probe_record.line_addr、size == 6，并使用 record 保存的 probe_token/active_alias
    期望值；
  按 active_probe_record.target_cap 检查 C.param 的合法集合；
  如果 active_probe_record.probe_owner == CBO：
    要求 active_probe_record.probe_token == pending_cbo_probe_token；不等时报错且不得建立 CBOAck；
  直接调用 complete_probe_response()

收到 ProbeAckData：
  检查 waiting_probe_c == 1
  通过共享 Probe service 的唯一 C owner/可观察字段定位 probe_record；
  检查 address/size/source 合法，并使用 record 保存的 probe_token/active_alias 期望值；
  按 active_probe_record.target_cap 检查 C.param 的合法集合；
  如果 active_probe_record.probe_owner == CBO：
    要求 active_probe_record.probe_token == pending_cbo_probe_token；不等时报错且不得建立 CBOAck；
  建立 c_assembly_owner = PROBE
  保存 c_assembly_probe_token = active_probe_record.probe_token，以及 owner、line；
  第二拍 C.fire 前必须匹配同一 c_assembly_probe_token，其他 C response 不得插入或覆盖；
  收齐两拍后调用 complete_probe_response()

其它 C opcode：
  按现有 Release/ReleaseData 路径处理；不能误认为 CBO Probe response
```

对于命中 `cached_line_by_addr` 的 `Probe(toB)`，本专项只接受 `TtoB/BtoB`；对于 `Probe(toN)`，
只接受 `TtoN/BtoN`。`NtoN` 表示目标 line 未持有有效副本，与 map hit 的建模前提矛盾；如果 DUT
返回 `NtoN`，说明 map 生命周期或 responder 状态已不一致，本专项报错而不是静默当作成功。实现应使用
集中定义的 TileLink response 常量，不把参数数值散落在 CBO 分支中。

### 7.2 `complete_probe_response()`

抽象功能：在 ProbeAck 或完整 ProbeAckData 完成后，更新必要的轻量状态，并决定是否建立 CBOAck。

修改后伪代码：

```text
complete_probe_response():
  if active_probe_record.probe_owner != CBO:
    不在本 CBO helper 处理 ProbeAckData 数据；交回 shared Probe service；
    该 service 只执行一次数据提交、line 生命周期更新和 record/C assembly 释放；
    本 CBO plan 不建立 CBOAck；
    return

  如果是 ProbeAckData 且无 corrupt：
    通过 shared memory store 提交完整 ProbeAckData 的 overlay 写事件，不得修改 main_mem；
    将对应 cached_line_record.data_valid 置 1；
  如果是 ProbeAckData 且有 corrupt：
    不提交 overlay 写事件，也不得修改 main_mem，将对应 cached_line_record.data_valid 置 0；
    报 uvm_error，但仍继续协议收敛；

  if active_probe_record.probe_owner == CBO:
    要求 active_probe_record.probe_token == pending_cbo_probe_token；不等时报错且不得建立 CBOAck；
    if pending_cbo_probe_opcode == CBOClean:
      保留 cached_line_by_addr[pending_probe_line] 的 active alias
    else if pending_cbo_probe_opcode == CBOFlush ||
            pending_cbo_probe_opcode == CBOInval:
      使 cached_line_by_addr[pending_probe_line] 失效
    else:
      fatal

    清 waiting_probe_c、C assembly 和 CBO Probe payload 状态
    填充 A.fire 时已保留的共享 response record：
      kind=CBO_ACK；opcode=CBOAck；source=17；size=6；
      由 shared response scheduler 选择 complete_cycle 之后的 due cycle；
    清除 active_probe_record；保留 cbo_context_valid，直到下面的 CBOAck.fire 完成
```

CBOAck 必须在 Probe response 完成后建立，随后仍由现有 D-channel hold/fire 逻辑发送。ProbeAckData
`corrupt=1` 只禁止数据写入，不能禁止 CBOAck 建立或使 CBO context 永久等待。CBOAck.fire 后
清理 pending D 和 `cbo_context_valid`，不等待 E GrantAck；CBO miss 的 direct CBOAck 也使用同一清理规则。

如果 CBO hit 但 Probe C response 在途，不能由 `process_d_fire()` 的旧 direct-CBO 分支提前删除或清理
CBO 状态。direct-CBO 分支只适用于 CBO miss。

## 8. 主循环仲裁与状态优先级

### 8.1 仲裁优先级

沿用共享 Probe service 的 owner/token；已建立的协议 owner 不可被新 Probe 抢占，空闲时按 alias plan
统一的 Probe 创建优先级选择新 owner：

```text
1. pending D response：继续保持 D payload，直到 D.fire
2. GrantAck owner：只开放 E.ready
3. C assembly owner：优先继续接收第二拍 C
4. 任意 Probe waiting C：优先接收匹配的 ProbeAck/ProbeAckData；同时沿用既有规则接收 Release/ReleaseData
5. 任意 Probe B pending：保持 B payload，直到 B.fire
6. 新 Probe 创建：ALIAS_CONFLICT > CBO > FLUSH > RANDOM
7. 新 C Release/ReleaseData
8. 新 CBO/Acquire A
9. 空闲时才允许随机 Probe
```

若已有任意同 line Probe 在途，新的 CBO A request 必须等待该 Probe 完成；CBO 的 deferred context
只允许一个，但它必须通过共享 `probe_record` 保存 token。CBO Probe 在途期间允许既有
Release/ReleaseData 被接收并生成 ReleaseAck，但 ReleaseAck 与 CBOAck 仍由现有 D response owner
串行发送。不得建立 CBO 专用的第二套 Probe queue。

### 8.2 CBO miss 与 hit 的统一出口

```text
CBO miss：
  A.fire -> pending CBOAck -> D.fire -> 清状态

CBO hit：
  A.fire -> CBO Probe B.fire -> C ProbeAck/Data 完成
         -> 更新/删除 map -> pending CBOAck -> D.fire -> 清状态
```

两条路径最终都只产生一拍 `CBOAck`，source 保持原 CBO source `17`，不产生 E GrantAck，也不产生
Hint。CBO hit 不改变 CBO response 的 opcode、source 和 D-channel 基本格式。

## 9. 修改文件与职责

本专项 coding 只修改：

```text
mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv
```

修改职责：

1. 通过共享 Probe service 建立 CBO `probe_record`，并增加关联 token 的最小 pending CBO context。
2. 扩展 CBO A.fire 分支，按 map 命中决定 direct ack 或 Probe。
3. 让现有 Probe builder 使用 CBO 指定的 `toB/toN`。
4. 让现有 C response assembly 区分随机 Probe 与 CBO Probe。
5. 让 CBO Probe 完成后再建立 CBOAck。
6. 保持普通 Acquire、Release、随机 Probe、Hint、Uncache 和主内存 helper 不变。

不修改：

- interface、transaction、driver、monitor 和 agent connect；
- plus 参数、cfg preset、virtual sequence 和 testcase；
- 主表、TLB、LSQ/ROB/状态表；
- 不建立第二份 alias map；coding 实体统一使用 alias plan 的 `cached_line_by_addr[line]` 与
  `cached_line_record`，旧 `cached_alias_by_line` 仅可作为兼容查询视图；
- 完整 L2 RM/scoreboard。

如果 `Probe(toB)` 所需的 V2 C response 参数常量当前未集中定义，只允许在同一个
`dcache_mem__access_base_sequence` class 内补充命名 localparam，不新增全局接口字段或运行期参数。

## 10. 与原测试框架逻辑对比和修改类型总结

### 10.1 保持不变的字段/协议适配

- CBO A opcode 继续使用 V2 `CBOClean=12`、`CBOFlush=13`、`CBOInval=14`。
- CBO source 继续使用 `17`。
- CBO response 继续使用单拍 D `CBOAck=8`。
- CBOAck 继续不等待 E `GrantAck`，不产生 Hint。
- physical line 仍是 64B 对齐 key；alias 存放在共享 `cached_line_record.active_alias`，不再以旧
  `line -> alias` 结构作为第二权威。

### 10.2 新增的功能逻辑

原逻辑是：CBO A.fire 后不检查缓存候选表，直接进入 CBOAck response；命中 line 只在 CBOAck.fire
时执行 Clean 保留或 Flush/Inval 删除。

新增逻辑是：

```text
CBO A.fire
  -> 查询共享 cached_line_by_addr[line] 的 ACTIVE record
  -> miss：保持原 direct CBOAck
  -> hit：建立共享 probe_record(probe_owner=CBO, probe_token)
          Clean 使用 Probe(toB)
          Flush/Inval 使用 Probe(toN)
          等待 ProbeAck/ProbeAckData
          完成后更新共享 cached_line_record
          最后发送 CBOAck
```

新增的必要原因是：真实 CBOAck 依赖上层 Probe、C response 和必要数据处理完成；如果命中 line 时
直接返回 CBOAck，测试框架无法验证 DCache 对 CBO 的 B/C coherent 交互，也会把响应时序提前。

### 10.3 主体逻辑是否改变

本专项属于局部功能逻辑新增，不改变测试框架主表驱动、LSQ/ROB 控制、普通 DCache 请求处理或退出
逻辑。主体改动仅限 DCache responder 内部的 CBO 生命周期：

```text
旧：CBO -> D response
新：CBO -> 可选 B/C Probe -> D response
```

普通 Probe 仍为原 owner；CBO Probe 仅增加来源标记和完成后的 CBOAck 转移，不建立完整 directory。

## 11. RM 协同支持

本 plan 不实现 RM、scoreboard 或 checker。

后续组件如果需要观察 CBO 事件，可使用：

```text
CBO A.fire：opcode、source、line address
CBO Probe B.fire：toB/toN、line address、alias
Probe C 完成：ProbeAck/ProbeAckData、line address、corrupt
CBOAck D.fire：source、denied、corrupt
```

## 12. 功能覆盖率协同支持

本 plan 不实现 coveragent 或 covergroup。

后续覆盖可使用以下分类维度：

```text
CBOClean/CBOFlush/CBOInval
CBO hit/miss
Probe(toB)/Probe(toN)
ProbeAck/ProbeAckData
CBOAck 前是否经历 Probe
```
