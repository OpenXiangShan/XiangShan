# mem_base_sequence.sv 源码分析

## 1. 术语与职责边界

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`。

该文件包含公共 sparse memory 后端、DCache coherent responder 和历史类名为 SBuffer 的 Uncache
TL-UL responder。它们是被动 memory-facing sequence：只在 DUT request 的真实 handshake 后建立回复，
不拥有主表、uid/status、LSQ、issue、commit、redirect/replay 或 testcase pass/fail。

| 术语 | 代码对象 | 当前含义 |
|---|---|---|
| `backing` | `main_mem` | 懒初始化的初始 memory 数据，DUT write 不直接覆盖它 |
| `overlay` | `write_overlay_mem/write_overlay_byte_valid` | 已完成 memory-facing 写的 byte 覆盖层 |
| `response record` | `dcache_rsp_q`、`uncache_rsp_q` | 已接受、还未完成最后一个 D beat 的一条回复 |
| `current D hold` | 两个 sequence 的 `current_d_record/current_d_valid` | 当前 D.valid 正在保持的唯一 record |
| `eligible_cycle` | record 字段 | 新 record 最早能被 scheduler 仲裁的逻辑拍 |
| `dynamic sink` | DCache `sink/grant_ack_wait_q` | Grant 与后续 E GrantAck 的唯一关联键 |
| `armed snapshot` | `armed_a_req_xact/armed_c_req_xact` | valid 等待 ready 时保存的 payload，仅用于下一 sample 的 fire/stability check |
| `write batch` | `dcache_write_batch/uncache_write_batch` | 当前 sample 已确认写，下一 sample 提交 overlay 的暂存集合 |
| `line record` | `cached_line_by_addr[line_addr]` | physical line 的唯一 alias 生命周期状态，不是完整 L2 directory |
| `probe record` | `probe_record_q` | B Probe 的 token、batch、旧 alias、target cap、owner 和 B/C 阶段 |
| `probe batch` | `dcache_probe_record_t::batch_id` | 一次随机 policy 选择出的互不重复 Probe 集合；非随机 owner 的 batch_id 为 0 |
| `CBO context` | `cbo_context_valid/pending_cbo_probe_*` | 一笔 CBO A.fire 到 CBOAck D.fire 的唯一 responder 生命周期；hit 要等待 Probe C 完成 |
| `CBO reservation` | `cbo_response_reserved` | A.fire 为未来 CBOAck 占用一笔 response capacity，之后原子转换为 record |
| `flush snapshot` | `l2_flush_snapshot_line_q` | DRAIN 后固定的 ACTIVE physical line 集合，不随之后的新 Grant 更新 |
| `flush state` | `l2_flush_state` | 轻量 L2 flush 的 `IDLE/DRAIN/PROBE/DONE` level-request 状态机 |
| `deferred Acquire` | `line_record.deferred_acquire` | 已 A.fire、等待旧 alias Probe(toN) 收敛的原始新 alias A payload |

## 2. 公共 Memory 后端

### 2.1 `initialize_shared_memory_state()`

抽象功能描述：该 task 由每 testcase 的唯一 memory lifecycle owner 在 responder 启动前调用，清空
shared backing、overlay、write batch，并设置是否启用严格物理地址范围。DCache/Uncache 本身只在 legacy
topology 尚未初始化时调用同一入口兜底，不能分别重置静态 memory。

```text
设置 strict-range 开关和 PADDR base/range；
清空 main_mem、overlay、byte-valid 和两类 write batch；
记录 lifecycle 已初始化；
```

### 2.2 `begin_shared_mem_sample()` 与 `shared_mem_access_task()`

抽象功能描述：前者推进共享写的可见性边界，后者执行 byte-mask memory 访问并把真实写分流到对应
batch。二者不创建 TileLink response、不修改 DCache sink/Probe state。

```text
begin_shared_mem_sample：提交上一拍 DCache batch，再提交上一拍 Uncache batch；
read：byte-valid 命中 overlay 就读 overlay，否则读/懒初始化 backing；
write：按 owner 把 address/mask/data 写到本拍 batch，不直接修改 backing；
```

同拍 DCache C writeback 与 Uncache store 的冲突顺序固定为“DCache 先、Uncache 后”。读只看上一轮
已提交 view，因此不会取到同拍尚未完成 TileLink handshake 的写数据。

## 3. DCache Coherent Responder

### 3.1 状态结构

`dcache_mem__access_base_sequence` 用一组 response record 代替旧的单一 `pending_d_*`：

```text
dcache_rsp_q：尚未被选中的 Grant/GrantData/CBOAck/ReleaseAck；
current_d_record：唯一 D hold，仍占 response capacity；
grant_ack_wait_q：D 已完成但 E 尚未确认的 Grant，继续占 sink；
dcache_hint_q：已绑定最终 GrantData、等待输出的 Hint；
c_assembly_response_reserved：ReleaseData 首 beat 预留的 ReleaseAck capacity；
cbo_response_reserved：CBO A.fire 预留的 CBOAck capacity；
cbo_context_valid/pending_cbo_probe_*：单笔 CBO opcode/source/line/error snapshot 与命中 Probe token；
cached_line_by_addr：GrantAck 后形成的 ACTIVE alias line，以及 Probe/alias conflict/GrantAck 中间态；
probe_record_q：所有已创建 Probe 的唯一生命周期 owner；B channel 同时只保持其中一笔；
l2_flush_snapshot_line_q：DRAIN 完成时复制的 ACTIVE line，轻量 flush 只扫描该固定集合；
l2_flush_state：只控制新 A.ready、snapshot/flush Probe 调度和 io_l2_flush_done，不拥有 shared memory 或主表。
```

capacity 由 compile-time `MEMBLOCK_DUT_DCACHE_A_MAX_OUTSTANDING=16` 决定：queued record、current
D hold、ReleaseData reservation 和 CBO reservation 共用这一上限；GrantAck wait 不占 record capacity，但会占用动态 sink。
不同 alias 的 Acquire 在 A.fire 时额外预留一个 response slot 与 sink，直到旧 alias Probe(toN) 完成后才交回
普通 Grant builder，防止已接受的 A 在等待期间因其它流量耗尽资源。

### 3.2 `sample_dcache_response_delay()`

抽象功能描述：在一轮 DCache scheduler 首次看到 eligible record 时，按四档 runtime 权重抽样额外
返回延迟。该函数是纯随机选择，不访问 protocol state。

```text
ZERO -> 0；SMALL -> 1..10；MEDIUM -> 10..100；LARGE -> 101..1000；
四项全零在 seq_csr_common 初始化阶段 fatal；
```

### 3.3 `can_accept_dcache_a_request()`、`enqueue_dcache_response()` 与 `accept_dcache_a_request()`

抽象功能描述：这组函数定义 coherent A channel 的准入边界。检查函数没有副作用；accept 函数只消费
真实 A.fire，把合法 request 固化为一个 response record；enqueue 是统一容量入口。

```text
AcquireBlock/AcquirePerm：需要 record capacity + free sink；
若 line 处于 Probe/GrantAck/deferred 中间态则 backpressure；不同 alias 的 Acquire 还要求同 line 无 Probe 且
probe_record_q 少于固定 16 笔，保证 A.fire 后可以创建 deferred owner；
CBO：无已有 CBO context 且 response record 有容量；命中 ACTIVE line 还要求无同 line Probe/deferred Acquire 且
  `probe_record_q` 未满；
AcquireBlock：读取两个 memory beat，创建两拍 GrantData，记录 alias/isKeyword/Hint；
AcquirePerm：创建单拍 Grant；CBO miss 直接创建 CBOAck，CBO hit 先建 context/reservation 和
  `Probe(Clean->toB, Flush/Inval->toN)`，C completion 后再创建 CBOAck；
所有 record：eligible_cycle = accept_cycle + 3；
```

不支持的 coherent A opcode 会在 response record 建立前 fatal，不能被当成 Uncache load。
不同 alias 的 Acquire 不直接创建 D response：它先保存为 deferred Acquire，创建针对旧 alias 的
Probe(toN)，待合法 C reply 收敛后再复用 `accept_dcache_a_request()` 建立普通 Grant/GrantData。

### 3.4 `service_dcache_response_scheduler()`

抽象功能描述：该函数是 DCache queue 到 current D hold 的唯一 scheduler owner。它在没有 current
D hold 时启动 timer，到期后根据 mode 选择一条 eligible record；不会重读 memory、重分配 sink 或
修改 D payload。

```text
若已有 current D hold：return；
无 timer 且存在本拍前可见、eligible 的 record：抽一次 DCache delay，记录 due cycle；
due 前：保持 timer；
due 时：ORDERED 选最早 eligible，REORDER 在 eligible 集合随机选；
将 record 移至 current_d_record，清 timer；
若是带 Hint 的 GrantData：把 Hint payload 入 dcache_hint_q。
```

`visible_count` 是当前拍进入 scheduler 前的 queue 长度。它避免当前 A/C fire 新加 record 被当拍选中。
D.ready=0 时 current record 保持，timer 和 queue 不会重复随机或释放。

### 3.5 `build_current_d_xaction()`、`process_d_fire()` 与 `process_e_fire()`

抽象功能描述：三者分别生成 D payload、在真实 D.fire 后推进 record、在真实 E.fire 后按 sink 完成
GrantAck。它们共同维护 D/E 生命周期，但不决定下一条 queue record。

```text
GrantData：第一个 D.fire 仅推进 beat_idx；最后 beat 才结束 record；
Grant/GrantData 最后 D.fire：将 {line, alias, sink} 转入 grant_ack_wait_q，并将 line record 标记为 GRANT_WAIT_E；
CBOAck/ReleaseAck 最后 D.fire：释放 record；CBOAck 仅校验并清 context，命中 CBO 的 line 更新已在 Probe C
  完成时执行；
E.fire：E.bits.sink 必须已知并唯一命中 grant_ack_wait_q；命中后建立 ACTIVE line record，释放 sink；
```

`process_e_fire()` 拒绝 X/Z sink 和未知 sink，防止二态折叠把错误 GrantAck 误匹配为 sink 0。

### 3.6 `start_c_assembly()`、`consume_c_beat()` 与 `complete_release_c_assembly()`

抽象功能描述：这些 task 处理 C channel 的 Release/ReleaseData 和 ProbeAck/ProbeAckData，两拍 data
只在完整收齐后写 memory overlay 或建立 ReleaseAck。它们不绕过 response scheduler。

```text
Release：建立 ReleaseAck record；
ReleaseData 首 beat：检查 capacity，建立 C assembly + reservation；
第二 beat：校验 header 连续性，收齐 data；
完整 ReleaseData：无 corrupt 时写 DCache batch；删除 line record；reservation 转为 ReleaseAck record；
ProbeAck/ProbeAckData：先按 physical line 唯一定位 WAIT_C probe record，再通过 record token 读取旧 alias、target cap 和 owner；
ProbeAckData 首拍：保存 c_assembly_probe_token 并把 record 标记 C_ASSEMBLY；第二拍必须仍命中同 token；
完整 ProbeAckData：无 corrupt 写 DCache batch；corrupt 时跳过写并置 line record.data_valid=0；两种情况都按 target cap 完成 record；
CBO owner 的 record 完成：先校验 token/context，再把 A.fire 已保留的 CBO reservation 转为 CBOAck；
```

不同 alias Acquire 继续遵守 `deferred A payload -> Probe(toN) 旧 alias -> C completion -> 重建原 A 的
Grant/GrantData -> E.fire 新 alias ACTIVE`。随机 policy 已使用同一 `submit_probe()` 建立 multi-batch
`toB/toN` record：每条选中的 line 立即转为 `PROBE_PENDING`，所以同一 batch 不会重复。flush policy 也只
复用 `cached_line_by_addr/probe_record_q/token`，不能回退为 `pending_probe_*` 或另建第二份 alias map。
同一 line 的 Release/ReleaseData 若与未完成 Probe 冲突则 fatal，防止 Release 先删除该 Probe 的 line record；
不同 line 的 Release 继续走原有兼容路径。

### 3.7 `body()` 与 `service_hint()`

抽象功能描述：`body()` 统一确认上一拍 A/B/C/D/E fire、调用各 lifecycle helper、驱动下一拍；
`service_hint()` 在 record 已最终被 scheduler 选中时输出一次 Hint，不参与 D response capacity。

```text
每个 drv_cb：begin_shared_mem_sample -> sample A/B/C/D/E 与 other_ctrl 的 flush level -> 检查 X/Z -> 确认 fire；
按 D -> E -> C -> B -> A 顺序消费旧 handshake；
调用 service_l2_flush：先 DRAIN 既有 owner；DRAIN/PROBE 关闭新 A.ready；DRAIN 完成时固定 ACTIVE snapshot；
PROBE 逐条建立 toN record；全部 C 收敛后 DONE 保持到外部 request 撤销；
调 scheduler、填 D hold、开放 E ready、处理 C/A 新准入；
同一轮 service_hint 输出与最终 GrantData 匹配的 io_l2_hint；Probe C reply 优先于新的 B launch，B payload
由当前 B-hold token 保持稳定；仅在 IDLE 且无 owner 时按随机 batch 参数尝试 Probe(toB/toN)；
io_l2_flush_done 仅在 DONE 为 1；
stop 时先完成已启动 flush 的 level handshake，再 drain queue/timer/D hold/GrantAck/Hint/CBO context/Probe/C assembly。
```

## 4. Uncache TL-UL Responder

### 4.1 `decode_uncache_a_opcode()` 与 `create_uncache_response_record()`

抽象功能描述：第一个函数将真实 A.fire 的 opcode 映射为 store ack 或 load data；第二个函数进行一次
shared-memory 访问并创建当拍不可返回的 record。两者不驱动 D，也不启动 timer。

```text
PutFullData/PutPartialData -> STORE_ACK -> AccessAck；
Get -> LOAD_DATA -> AccessAckData；
其它 opcode -> fatal；
record.eligible_cycle = accept_cycle + 1；入 uncache_rsp_q；
```

`MEMBLOCK_DUT_UNCACHE_MAX_OUTSTANDING=16` 是 compile-time record 容量。当前 D-error 实现只在
record 创建点调用 `apply_uncache_d_error_injection()`：`Get -> AccessAckData` 将 backend error 与
`MEMBLOCK_UNCACHE_DENIED_WT/CORRUPT_WT` 一次采样合并，denied 命中强制 corrupt=1；
`Put* -> AccessAck` 只允许 denied，corrupt 固定为 0。scheduler、D hold 和重排都只搬运 record
快照，不能再次随机。

### 4.2 `sample_uncache_response_delay()` 与 `service_uncache_response_scheduler()`

抽象功能描述：两个函数分别抽取 Uncache 的四档延迟、把 eligible record 移至 current D hold。它们完全
独立于 DCache timer 和 queue。

```text
无 current D hold 且存在 visible eligible record：按 Uncache weight 启动 timer；
timer 到：ORDERED 取最早 eligible，REORDER 随机取 eligible；
记录成为 current D hold；
D.fire：process_uncache_d_fire 清 record，容量立即归还；
```

### 4.3 `service_uncache_d_hold_watchdog()` 与 `body()`

抽象功能描述：watchdog 只诊断长 D.ready backpressure；body 负责 A/D fire 确认、record 创建、scheduler、
global-stop drain 和唯一 Uncache item 驱动。

```text
D hold 连续 1000 个 driver 边界无 D.fire：warning 一次；保持 payload；
body：当前 A.valid -> arm A.ready；下一 sample 确认 A.fire 后建立 record；
global stop：不接受新 A，已 armed/fire record 继续 drain；queue/timer/current D/armed 收敛后退出。
```

## 5. 参数与文件同步

runtime 参数路径：

```text
env/plus.sv -> seq_csr_common::load_from_plus()/validate_and_clamp() -> getter -> responder scheduler
```

compile-time capacity 路径：

```text
cfg/memblock_compile_params.svh -> memblock_dispatch_types.sv typed localparam -> responder capacity check
```

随机 Probe 参数替代旧单百分比门：

```text
MEMBLOCK_L2_PROBE_EN
  -> MEMBLOCK_L2_PROBE_PRE_START_WT
  -> MEMBLOCK_L2_PROBE_COUNT_{ONE,MID,LARGE}_WT
  -> MEMBLOCK_L2_PROBE_TO_B_WT
  -> seq_csr_common getter
  -> try_start_probe()/sample_probe_*()
```

`MEMBLOCK_L2_PROBE_EN` 只控制随机 policy。DUT `io_outer_l2_flush_en` 是功能性 level request，
`service_l2_flush()` 无论随机开关均可提交固定 `Probe(toN)`，否则 flush 会永久无法完成。

相关现行 flow：

- `AI_DOC/mem_ut_flow_doc/dcache_l2_response_hint_probe_model_flow.md`
- `AI_DOC/mem_ut_flow_doc/dcache_sbuffer_memory_responder_flow.md`

历史 plan/review 中的 `pending_d_*`、固定 sink 0、DCache 三档延迟或 Uncache 即时回复描述只反映
当时实现；以本分析与当前 response-delay implementation review 为准。

## 6. 修改类型总结

- 字段/参数：增加两组四档 delay weight、两组 reorder enable，以及两个编译期 outstanding macro；
- 功能：DCache/Uncache 都从单 pending response 改为独立 response queue + timer + D hold；
- 功能：DCache Grant 使用动态 sink 和多笔 GrantAck wait；ReleaseData 使用 capacity reservation；
- 功能：Uncache 对 V2 A opcode 使用显式白名单，并提供 D.ready 长期阻塞 warning；
- 功能：DCache alias conflict 已具备 `A.fire -> deferred Acquire -> Probe(toN) old alias -> C token closure -> GrantAck`
  闭环；C two-beat data 使用 token 防止其它 Probe 覆盖。
- 参数迁移：删除 `MEMBLOCK_L2_PROBE_ENABLE_WT`，改为随机 batch 总开关、开始权重、三档数量权重和 toB 权重；
  `seq_csr_common` 校验两个 10000 比例权重及非零数量权重。
- 功能：随机 policy 从单笔 toN 扩展为一批互不重复的 `Probe(toB/toN)`；每条仍使用已有 token、line record
  和单 B hold，capacity 固定为 16。
- 功能：新增轻量 L2 flush `IDLE/DRAIN/PROBE/DONE`，DRAIN 后建立固定 ACTIVE snapshot 并逐条 toN，DONE
  保持 `io_l2_flush_done` 到 request 撤销；driver 允许 known-one sideband。
- 功能：CBO 从直接 CBOAck 扩展为 miss 直接 Ack、hit 先 Probe 后 Ack 的单笔 context 闭环；错误位与
  response 容量均在 A.fire 固定，Probe C 完成只转换 reservation，CBOAck D.fire 才清 context。
- 保护：命中 CBO 在 A.ready 前预检 Probe record capacity；同一 line Release/ReleaseData 与 Probe 冲突
  立即 fail-fast，避免破坏 token 对应的 line lifecycle。
- 保持不变：shared memory backing/overlay 的数据职责、主表/LSQ/issue/commit/terminal owner、完整 CoupledL2
  directory/set/way 扫描、多个 CBO 并发和主动 CBO 主表闭环。
