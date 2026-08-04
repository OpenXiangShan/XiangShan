# DCache 轻量 L2 Response、Hint 与 Probe Flow

## 1. 术语与抽象功能说明

本 flow 描述 `dcache_mem__access_base_sequence` 对 V2 coherent TileLink A/B/C/D/E 的轻量
responder 行为。它只模拟 MemBlock 对外的 L2 交接，不实现完整 L2 directory、MSHR、替换或权限目录。
当前已具备 alias conflict 所需的共享 line/probe 生命周期基础：同一 physical line 的新 alias Acquire
会先 Probe(toN) 旧 alias，再继续原 A 请求；每笔 Probe 使用稳定 token 匹配 C reply。本 flow 已支持
随机 multi-batch `Probe(toB/toN)` 与轻量 `l2Flush` snapshot；CBO Probe closure 仍由独立专项扩展。

| 术语 | 当前含义 | 代码落点 | 生命周期 |
|---|---|---|---|
| `response record` | 已真实 A/C `fire`、等待最后一个 D beat 的 DCache 回复记录 | `dcache_rsp_q`、`current_d_record` | 建立于 A/C fire，最后一个 D.fire 后释放 record 容量 |
| `D-error snapshot` | 本次 coherent D reply 的 `denied/corrupt` 固定值 | `dcache_response_record_t::denied/corrupt` | 建立 GrantData/CBOAck record 时采样一次，D hold 或多 beat 不重采样 |
| `eligible_cycle` | record 最早允许参加 D 返回仲裁的逻辑拍 | `dcache_response_record_t::eligible_cycle` | DCache 真实请求在 `t` fire 后最早 `t+3` 可选 |
| `scheduler timer` | 每次选择 D response 前按 delay weight 抽一次等待的定时器 | `dcache_rsp_timer_active/due_cycle` | 无 D hold 且存在 eligible record 时启动；选出 record 后清除 |
| `current D hold` | 已被 scheduler 选中、正在 D channel 保持 payload 的唯一 record | `current_d_record/current_d_valid` | D.ready=0 时保持；最后一个 D.fire 时结束 |
| `dynamic sink` | 每个 Grant/GrantData 独占的 TileLink sink 标识 | `dcache_response_record_t::sink` | Acquire 接收时分配；E.fire 匹配后复用 |
| `GrantAck wait` | 已完成最后一个 D beat、但尚未收到匹配 E.fire 的 Grant owner | `grant_ack_wait_q` | 最后 D.fire 入队；E.fire 后删除 |
| `C reservation` | ReleaseData 第一个 C beat 为未来 ReleaseAck 预留的 response slot | `c_assembly_response_reserved` | 首 beat 建立；第二 beat 完整收集后原子转换为 ReleaseAck record |
| `Hint record` | 已和某条最终选出的 GrantData 绑定、等待本拍输出的 Hint sideband | `dcache_hint_q` | scheduler 选中 GrantData 时入队；`service_hint()` 单拍消费 |
| `fire` | 上一拍 responder 驱动的 valid/ready 与当前 DUT 采样值同为 1 | `a_fire/b_fire/c_fire/d_fire/e_fire` | 只有 fire 可以创建、推进或释放协议状态 |
| `line record` | physical line 的唯一轻量 alias 生命周期记录；不是完整 L2 directory | `cached_line_by_addr[line_addr]` | GrantAck 后 ACTIVE；Probe、alias conflict、GrantAck 等阶段更新 |
| `probe record` | 一笔 B Probe 的稳定请求身份和 target 权限 | `probe_record_q` | submit 时创建；合法 C reply 完成后删除 |
| `probe token` | 测试框架内部唯一 Probe 标识，不在 C payload 中传输 | `dcache_probe_token_t`、`c_assembly_probe_token` | 建 record 时分配；两拍 ProbeAckData 到齐前保持不变 |
| `probe batch` | 一次随机开始后建立的一组互不重复的随机 Probe record | `dcache_probe_record_t::batch_id` | 建立时写入；所有 record C 收敛后该 batch 自然结束 |
| `flush snapshot` | `l2Flush` DRAIN 完成时固定下来的 ACTIVE line 集合 | `l2_flush_snapshot_line_q` | 只在一次 DRAIN->PROBE 边界建立；新 Grant 不会追加入本轮 |
| `flush state` | 轻量 L2 flush 的 level-request 本地状态机 | `l2_flush_state` | `IDLE -> DRAIN -> PROBE -> DONE -> IDLE` |
| `deferred Acquire` | 已 A.fire 但必须先清除旧 alias 的新 alias Acquire | `line_record.deferred_acquire` | alias conflict 时保存；旧 alias Probe(toN) 收敛后交回普通 A response builder |

## 2. 调用 Flow

```mermaid
flowchart TD
    A[dcache_mem__access_base_sequence::body] --> B[drv_cb sample + begin_shared_mem_sample]
    B --> C[确认上一 item 的 A/B/C/D/E fire]
    C --> D[process_d_fire / process_e_fire]
    C --> E[start_c_assembly / consume_c_beat]
    C --> F[accept_dcache_a_request]
    C --> R[service_l2_flush]
    F --> P[start_alias_conflict / submit_probe]
    P --> Q[service_probe_b_hold / build_probe_b_xaction]
    Q --> E
    F --> G[enqueue_dcache_response]
    E --> G
    G --> H[service_dcache_response_scheduler]
    H --> I[build_current_d_xaction]
    H --> J[service_hint]
    R --> S[capture_l2_flush_snapshot / submit_probe(toN)]
    C --> K[try_start_probe multi-batch]
    I --> L[send_dcache_xaction]
    J --> L
    K --> L
    L --> M[dcache_agent_agent_driver::main_phase]
    M --> B
```

### 2.1 函数调用 Flow 图整体文字伪代码

```text
每个 drv_cb 边界：
  先提交上一 sample 已确认的 shared-memory 写 batch，并采样 A/B/C/D/E 与 `io_outer_l2_flush_en`；
  用上一拍已驱动 item 和当前采样确认 fire；
  D.fire 推进或结束当前 D hold，Grant 最后一拍转入 GrantAck wait；
  E.fire 必须按 sink 匹配 GrantAck wait，命中后更新 line record 为 ACTIVE；
  C.fire 用 line 筛选唯一 WAIT_C probe record；ProbeAckData 的第一拍锁定 token，第二拍只允许同 token
  继续 assembly，Release 类最终建立 ReleaseAck record；
  A.fire 解码 Acquire/CBO；若 Acquire alias 与 ACTIVE line record 不同，先保存 deferred Acquire 并建立
  Probe(toN)，否则沿用 Grant、GrantData 或 CBOAck record 创建；
  调用 service_l2_flush：flush request 首先 DRAIN 已建立 owner；全部收敛后复制 ACTIVE line snapshot，
  逐条 submit Probe(toN)，全部 C 收敛后进入 DONE 并保持 io_l2_flush_done=1 到 request 撤销；
  scheduler 只看进入本拍前已经存在且到期的 record，独立计时后按顺序或乱序选一条成为 current D hold；
  被选中的 GrantData 如有 Hint，才在同一返回轮次送入 Hint queue；
  DRAIN/PROBE 时 A.ready=0；DONE 时恢复普通 A 准入，但随机 Probe 仍暂停；
  仅在 IDLE 且无其它 Probe owner 时，按 batch 参数建立互不重复的 random Probe(toB/toN) record；
  按当前 D hold、GrantAck wait、probe B hold、Probe/C assembly 和 A/C 准入构造下一 item；
  driver 立即写 clocking output；下一 drv_cb 再确认该 item 是否真正 fire。
```

## 3. 逐拍握手与准入

### 3.1 `body()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`。

抽象功能描述：`body()` 是 DCache responder 的唯一时序 owner。它在 clocking-block 采样边界确认
上一拍 item 的协议结果，维护所有 response、sink、Hint、Probe 与 C assembly 状态，并驱动下一拍
item；它不读取 monitor analysis port，也不修改 dispatch 主表、LSQ 或 terminal。

文字伪代码：

```text
等待 drv_cb；调用 begin_shared_mem_sample；采样 A/B/C/D/E valid/ready、`io_outer_l2_flush_en` 和 reset；
任一握手位在非 reset 时为 X/Z：fatal；
从全零 idle item 开始；

若 reset：清 response queue、D hold、timer、GrantAck wait、Hint、Probe、C assembly 和 cache map；发送 idle；
否则：
  根据上一 item 确认 D/E/C/B/A fire，并按 D -> E -> C -> B -> A 顺序消费；
  先调用 service_l2_flush：已有 A/B/C/D/E owner 继续自然收敛；DRAIN/PROBE 阶段禁止新的 A.fire；
  记录本拍开始时 dcache_rsp_q.size()，作为 scheduler 可见上界；
  调用 scheduler；若存在 current D hold，填充 D.valid 和稳定 payload；
  GrantAck wait 非空时才打开 e_ready；
  C assembly、已发送 Probe 的 C reply、Probe B hold、普通 Release C 和普通 A 依协议优先级准入；
  `service_probe_b_hold()` 只把已建 record 中的一笔变成稳定 B payload；IDLE 且无其它 owner 时
  `try_start_probe()` 按 batch 数量和 toB 权重建立互不重复 record；PROBE 状态由 snapshot 驱动固定 toN；
  最后叠加本拍 Hint，并仅在 flush DONE 驱动 io_l2_flush_done=1。

global stop：停止新的随机 Probe 和新 A 准入，只排空现有协议状态；若已经观察到 L2 flush request，
仍必须先完成该 level handshake，不能提前退出。所有 queue、timer、D hold、GrantAck、Hint、Probe、
C assembly、armed snapshot 与 flush state 收敛后发送最后一个 idle 并退出。
```

`response_visible_count` 在处理 A/C fire 前取得。因此本拍新建的 D response record 即使其 `eligible_cycle`
已经满足，也不会被同拍 scheduler 再次选择，保证 A/C 接收和 D 返回之间至少有一个明确的 driver
边界。

### 3.2 `can_accept_dcache_a_request()` 与 `can_accept_dcache_release_c_request()`

抽象功能描述：两个 helper 只判断本拍是否可以接受会产生 D response 的输入，不创建 record 或访问
memory。A 的 Acquire 还必须有空闲 sink；alias conflict Acquire 还要确认共享 Probe record queue 有容量，
以保证一旦 A.fire 就一定能建立 deferred owner；CBO/Release 只占用统一 response record 容量。

```text
AcquireBlock/AcquirePerm：line 不处于 deferred/Probe/GrantAck 中间态，response record 未满且存在空闲 sink -> 可接受；
若是不同 alias 的 Acquire：还要求同 line 无 Probe 且 probe_record_q 少于 16；
CBOClean/CBOFlush/CBOInval：response record 未满 -> 可接受；
Release/ReleaseData：response record 未满 -> 可接受；
其它 A opcode：fatal；ProbeAck/Data 不需要 ReleaseAck capacity。
```

DCache 的 `Grant/GrantData`、`CBOAck`、`ReleaseAck` 共用
`MEMBLOCK_DUT_DCACHE_A_MAX_OUTSTANDING=16` 条 response record。record 满时只对会新增 D response
的 A/C 输入拉低 ready；已经进入两拍 ReleaseData assembly 的第二 beat 使用首 beat reservation，不能
因表接近满而半完成。

## 4. Response Record 与 D Scheduler

### 4.1 `accept_dcache_a_request()` 与 `enqueue_dcache_response()`

抽象功能描述：`accept_dcache_a_request()` 只消费真实 A.fire，完成 coherent A opcode、line 对齐、
source、param 和 memory 读取检查，并生成一个语义已固定的 response record；
`enqueue_dcache_response()` 是统一容量检查和入队入口。

```text
AcquireBlock：读取两个 32B memory beat，建立两拍 GrantData；分配动态 sink；保留 alias、isKeyword、一次 Hint 采样和一次 D-error snapshot；若 denied 命中则强制 corrupt=1；
AcquirePerm：建立单拍 Grant；分配动态 sink；
CBO：建立单拍 CBOAck；不分配 sink；denied/corrupt 由各自权重独立采样并保存在 record；
所有 record：eligible_cycle = accept_cycle + 3；入 dcache_rsp_q；
```

不同 alias 的 Acquire 是例外：它已发生 A.fire，不能直接入 `dcache_rsp_q`。`start_alias_conflict()`
先预留一个 future response slot 和 sink，保存完整 A payload 到 `line_record.deferred_acquire`，并为旧 alias
创建 `Probe(toN)` record。旧 alias C reply 合法收敛后，才释放预留并将同一份 deferred A payload 重新交给
`accept_dcache_a_request()` 建立正常 Grant/GrantData；新 alias 直到对应 E.fire 才成为 ACTIVE。

这里的 `+3` 表示 V2 DCache responder 的固定两拍 admission 后，最早下一拍可参加返回仲裁。
它不使用 `pre_pkt_gap/post_pkt_gap`，也不阻塞 sequence 的主循环。

### 4.2 `sample_dcache_response_delay()`

抽象功能描述：该函数只为一轮 DCache response scheduling 选择额外等待拍数；不建立 transaction、
不访问 memory，也不改变 A/C/D/E 握手。

| 参数 | 区间 | 默认权重 |
|---|---|---:|
| `MEMBLOCK_L2_RSP_DELAY_ZERO_WT` | `0` | 0 |
| `MEMBLOCK_L2_RSP_DELAY_SMALL_WT` | `1..10` | 1 |
| `MEMBLOCK_L2_RSP_DELAY_MEDIUM_WT` | `10..100` | 0 |
| `MEMBLOCK_L2_RSP_DELAY_LARGE_WT` | `101..1000` | 0 |

`seq_csr_common` 在启动 responder 前拒绝四档权重全零。随机 delay 是 response record 已到期后的
额外等待，不能改变 A.fire，也不能绕过 D.ready backpressure。

### 4.3 `service_dcache_response_scheduler()`

抽象功能描述：该函数是 DCache response queue 到唯一 current D hold 的仲裁 owner。它只移动 record
和 timer；不重新解码 opcode、不重读 memory、不释放 sink。

```text
若已有 current D hold：直接返回；
若无运行中 timer 且 visible record 中存在 eligible record：
  调用 sample_dcache_response_delay；记录 timer due cycle；
timer 未到：保持 timer；
timer 到：
  ORDERED：选择最早 eligible record；
  REORDER：从已到期 eligible record 中随机选择；
  从 dcache_rsp_q 删除，写入 current_d_record/current_d_valid；
  若该 record 带 hint_pending：写入 dcache_hint_q，并清 record 内标记；
  清本轮 timer。
```

`MEMBLOCK_L2_RSP_REORDER_EN=0` 为顺序返回；为 `1` 时只在当前已到期、且本拍前可见的
record 集合内随机。D.ready=0 时 scheduler 不重新抽 delay，也不选择第二条 record，
`build_current_d_xaction()` 持续使用同一份 `current_d_record` 输出稳定 payload。

### 4.4 `build_current_d_xaction()` 与 `process_d_fire()`

抽象功能描述：前者把 current D hold 映射为本拍 TileLink D payload；后者只在真实 D.fire 后推进
beat 或释放 record。二者不决定 scheduler 的选择和 A/C 准入。

```text
GrantData：按 beat_idx 输出两个 32B beat；第一个 D.fire 只递增 beat_idx；
最后一个 GrantData D.fire 或 Grant D.fire：把 line/alias/sink 转入 grant_ack_wait_q；释放 current record；
CBOAck：当前 direct-ack 路径最后 D.fire 后按 CBO opcode 删除对应 line record（clean 保留）；释放 record；
ReleaseAck：最后 D.fire 后释放 record；
```

`MEMBLOCK_L2_GRANTDATA_DENIED_WT/CORRUPT_WT` 和
`MEMBLOCK_L2_CBO_ACK_DENIED_WT/CORRUPT_WT` 都经过 `seq_csr_common` 读取。它们不改变
response record 的准入、延迟、sink 或 Hint 逻辑。GrantData 的两个 D beat 与任何 D.ready hold
只复用 record 中已保存的错误位；CBOAck 仍按原 source 和 opcode 完成 cached line 动作。

最后一个 D.fire 立即归还 response record capacity，但 Grant sink 仍属于 `grant_ack_wait_q`，直到 E.fire
匹配后才可分配给新的 Acquire。

## 5. Dynamic Sink、Hint 与 E Channel

### 5.1 `allocate_grant_sink()` 与 `process_e_fire()`

抽象功能描述：`allocate_grant_sink()` 在 16 个 compile-time sink 槽中选择未被 queued/current Grant 或
GrantAck wait 占用的编号；`process_e_fire()` 按 DUT E sink 唯一完成 GrantAck owner，而不是按地址猜测。

```text
Acquire record 创建时：分配未用 sink 并写入 D payload；
Grant 最后 D.fire：D record 删除，{line_addr, line_alias, sink} 写入 grant_ack_wait_q；
E.fire：E.bits.sink 必须已知；按 sink 查 grant_ack_wait_q；
  命中：record_cached_line(line_addr, line_alias)，把 line record 置为 ACTIVE，删除 wait record，sink 释放；
  无命中：fatal。
```

`E.valid` 在没有 GrantAck owner 时也是协议错误。`E.bits.sink` 在 E.fire 为 X/Z 会立即 fatal，
不能因二态转换被误匹配到 sink 0。

### 5.2 `service_hint()`

抽象功能描述：该函数只把已绑定最终 GrantData 的 Hint record 映射到单拍
`io_l2_hint_*` sideband；不参与 GrantAck、cache map 或 response capacity。

`sample_hint_enable()` 只在 `AcquireBlock` 真实接受时采样一次。scheduler 在最终选中该条
GrantData 时才把 Hint 放入 `dcache_hint_q`，随后同一轮 `service_hint()` 输出。
因此 REORDER 不会发生 Hint 属于 A、D response 却属于 B 的错配；`D.ready=0` 也不会重复发送 Hint。
`io_l2_flush_done` 由 `l2_flush_state==DONE` 唯一驱动为 1；其它状态为 known-zero。driver 只检查该
sideband 已知，不把合法的 DONE level 误报为非法非零值。

## 6. C Channel、Probe 与 ReleaseAck

### 6.1 `start_c_assembly()`、`consume_c_beat()` 与 `complete_release_c_assembly()`

抽象功能描述：这些 task 负责 coherent C response 的 header 校验、两拍 data assembly、overlay 写回和
ReleaseAck record 建立。它们不选择 D 返回时间，也不占用 Grant sink。

```text
Release：真实 C.fire 后直接建立 ReleaseAck record；
ReleaseData 首 beat：检查容量，置 c_assembly_response_reserved，保存 header/data；
ReleaseData 第二 beat：检查 opcode/address/source/size/param 连续性；收齐后：
  若未见 corrupt：两个 32B beat 进入 DCache overlay write batch；
  删除 cached line；将 reservation 原子转换为 ReleaseAck record；
```

`ProbeAckData` 同样必须收齐两个 beat 后才写回 overlay；第一拍由 `start_c_assembly()` 通过 physical
line 唯一找到 `WAIT_C` probe record，保存其 `c_assembly_probe_token` 并把该 record 转为
`C_ASSEMBLY`。第二拍由 `consume_c_beat()` 重新确认同一 token/line/固定 header；`corrupt=1` 时跳过
writeback、将 line record 的 `data_valid` 清零并报告 `uvm_error`，但仍调用 `complete_probe_record()` 完成
toN/toB 生命周期，不能让 owner 永久残留。

`submit_probe()` 是 B/C 生命周期的唯一创建入口：它从 ACTIVE line record 复制旧 alias、target cap、owner
和随机 batch_id，
分配 token 并保证同一 line 不会有第二笔未完成 Probe。`service_probe_b_hold()` 只将一个 QUEUED record
变为 B hold；`build_probe_b_xaction()` 在 backpressure 时持续读取同一 record 输出稳定 payload；B.fire 后该
record 进入 WAIT_C。

随机 policy 仅在 `MEMBLOCK_L2_PROBE_EN=1` 且 responder 空闲时工作：先按
`MEMBLOCK_L2_PROBE_PRE_START_WT` 选择是否开始 batch，再按 ONE/MID/LARGE 权重选择 `1`、`2..6` 或
`7..15` 条 line，最后逐 line 按 `MEMBLOCK_L2_PROBE_TO_B_WT` 选择 `toB/toN`。被选 line 会立即建立
record，因此同 batch 不会重复；16 条 record 满时停止继续建立，不报 fatal。该 EN 只控制随机激励，
不能阻止 DUT 已发起的 `l2Flush` 完成功能。

`service_l2_flush()` 是轻量 flush 的唯一状态 owner：DRAIN 等待已有 D/E/B/C/assembly/Probe 收敛，
再一次性扫描 `cached_line_by_addr` 建 snapshot；PROBE 对 snapshot 中每条 line 提交 `toN`；DONE 只保持
done level，直到 `io_outer_l2_flush_en=0` 清空 flush-local snapshot 回到 IDLE。它不调用
`clear_runtime_state()`，不会取消正常 D/E/Probe owner，也不模拟完整 CoupledL2 set/way 扫描。

## 7. Reset、Stop 与边界

`clear_runtime_state()` 在 reset 时清除 `dcache_rsp_q`、timer、current D hold、GrantAck wait、Hint queue、
C reservation、Probe 和 cache map。shared backing/overlay 的清空只由 testcase 的 memory lifecycle owner
在 responder 启动前完成；reset 不应重新清空公共 memory。

global stop 的退出条件包括：flush state 已回到 IDLE、无 queued/current D record、无运行 timer、无 GrantAck
wait、无 Hint、无 B hold、`probe_record_q` 为空、无 C assembly/reservation、无 armed A/C snapshot 和当前
未处理 A/C valid。
已完成 GrantAck 的 ACTIVE line record 是历史状态，不阻止退出。

本 flow 不拥有主表、LSQ admission、issue、writeback、commit/deq、redirect/replay、pass/fail 或 terminal。
Uncache TL-UL A/D responder 的独立 queue 和 delay 见
`AI_DOC/mem_ut_flow_doc/dcache_sbuffer_memory_responder_flow.md`。

## 8. 修改类型总结

本轮不是字段更名，而是 responder 内部响应调度增强：

- 从单一 `pending_d_*` 和固定 sink 0 改为 DCache 统一 response record、动态 sink 与 GrantAck wait queue；
- 由三档 DCache delay 扩展为四档，并新增 runtime 顺序/乱序返回选择；
- `ReleaseData` 新增首 beat reservation，避免两拍 C transaction 在 response capacity 满时半完成；
- Hint 从独立 pending 状态改为附着在 GrantData record，最终选中 D response 后才输出；
- 单一 `pending_probe_*`/`waiting_probe_c` 替换为共享 `probe_record_q`、稳定 token、单 B hold 和按 line 的
  C reply 唯一匹配；ProbeAckData 首拍锁定 token，第二拍不能被其它 C response 覆盖；
- `cached_alias_by_line` 替换为包含 alias、valid、data 边界、lifecycle 与 deferred Acquire 的 line record；
  不同 alias Acquire 先 Probe(toN) 旧 alias，再恢复原始 A response，避免覆盖仍有效的旧 DCache 副本；
- 旧 `MEMBLOCK_L2_PROBE_ENABLE_WT` 单百分比门替换为 `MEMBLOCK_L2_PROBE_EN`、batch start/count/toB 六个
  参数；随机 policy 从单笔 toN 扩展为多笔互不重复 `toB/toN`，但仍复用同一 Probe record/token/B hold；
- 新增 flush-local `IDLE/DRAIN/PROBE/DONE` 状态机：采样外部 level request，DRAIN 后固定 ACTIVE line
  snapshot，逐条 Probe(toN)，完成后保持 `io_l2_flush_done` 到 request 撤销；
- 保留原有 A/C/E/Probe/overlay 和 global-stop 所有权，不把 responder 状态写入主表或 status table。

当前默认 delay 为 `1..10` cycle、顺序返回。D-error 随机注入已由
`AI_DOC/plan/test_framework/plan/do/mem_ut_v2_dcache_d_error_weight_adapt_plan_20260803.md` 实现：它只扩展 response record 的
合法 D payload，不改变本 flow 的调度或主框架控制行为。alias conflict、随机 multi-batch/toB 与轻量
L2 flush 已实现；CBO Probe closure 仍由独立 plan 实现，不能在本 flow 中宣称已覆盖。
