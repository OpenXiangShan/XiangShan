# mem_ut V2 LSQ MMIO/Status 适配最终 Coding Plan

| 项目 | 内容 |
|---|---|
| 状态 | `undo`，待 coding |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| V2 接口权威 | `build_memblock/rtl/MemBlock.sv` |
| 测试框架入口 | `memblock_lsqcommit_dispatch_base_sequence::send_lsqcommit_cycle()`、`memblock_dispatch_base_sequence::collect_monitor_event_batch()` |
| 适配原则 | 区分 V2 ROB head sideband、normal commit、fault convergence 和真实 LSQ deq；不把 output 观察字段混入 pass/fail |
| 创建/修订日期 | 2026-07-15 |

## 1. 范围与边界

本 plan 只整理 V2 `loadMmio/loadMmioUop/storeMmio/storeMmioUop/pendingMMIOld/pendingst/scommit`
以及 `lqDeq/sqDeq/sbIsEmpty/memoryViolation` 对测试框架运行期状态的影响。每个问题均说明 V2
问题、修改原因、最终修改逻辑和可直接 coding 的文字伪代码。

本轮支持范围：

- V2 ROB -> LSQ sideband：`pendingPtr/pendingst/pendingMMIOld/scommit`。
- normal commit batch 与 fault convergence 互斥处理。
- V2 `sqDeq` count-only 回收；有 `sqDeqPtr` 的 profile 保留 pointer 分支。
- monitor ctrl raw 的 semantic event 与 LQ/SQ/SB 状态推进分离。
- lsqcommit active driver 在 no-item/gap 周期保持 level sideband。
- 主动 loop 使用真实 progress 边沿暴露 fault 缺 deq 等 blocker。

本轮不支持：

- MMIO 正确性 checker、RM、scoreboard 或 coverage。
- PMP/L2TLB 顶层 output monitor。
- 修改 `terminal_done`、pass/fail/fault 定义。
- 把 `loadMmio/storeMmio` 直接接入 pass/fail 或 terminal。MMIO raw/tag producer 由
  `mem_ut_v2_pending_mmio_load_sideband_execution_plan_20260710.md` 唯一拥有。
- 修改 `AI_DOC/mem_ut_flow_doc` 下 flow 文档或其它专项 plan。

同一原子 coding 批次硬前置：

- compile/width 专项已经提供 `` `MEMBLOCK_DUT_HAS_SQ_DEQ_PTR``、ROB/LQ/SQ value width 等宏；
  V2 profile 固定 `` `MEMBLOCK_DUT_HAS_SQ_DEQ_PTR=0``。
- pending-MMIO 专项先提供 `dispatch_raw_ctrl_t.sq_deq_ptr_valid`、MMIO raw tag 字段、
  ctrl interface accessor、monitor 采样和 `common_data_transaction::uid_is_mmio_load()`。
- 缺任一宏、raw 字段或 query 时必须编译失败，禁止 `ifdef` fallback 默认 0。

## 2. 问题一：V2 output 状态缺少职责分类

### V2 问题

V2 顶层暴露 `loadMmio/loadMmioUop/storeMmio/storeMmioUop/pendingMMIOld/pendingst/scommit`。
旧文档只说这些字段需要关注，但没有明确哪些字段推进当前 flow，哪些只是 debug 或后续专项输入。
如果分类不清，容易把 output 观察字段误接到 pass/fail，或把当前必需的 sideband 当成可丢弃日志。

### 修改原因

测试框架 plan 必须只把会影响激励、状态生命周期和主动 flow 退出的字段纳入本轮主逻辑。MMIO
正确性和覆盖率属于后续组件，不应混入本 plan。

### 修改方案与修改逻辑

固定分类如下：

| 信号 | 分类 | 本 plan 处理 |
|---|---|---|
| `lqDeq/sqDeq/lqDeqPtr/sbIsEmpty` | `FLOW_REQUIRED` | raw ctrl full wrapper 消费，推进 LSQ map、free count、terminal 和 SB empty |
| `memoryViolation` | `FLOW_REQUIRED` | 作为 semantic redirect event 进入 redirect-first batch |
| `loadMmio/loadMmioUop` | `FLOW_REQUIRED` 的前置 tag 输入 | producer/query 由 pending-MMIO 专项拥有，本 plan 只通过 `uid_is_mmio_load()` 消费已落表 tag |
| `storeMmio/storeMmioUop` | `FLOW_REQUIRED` 的前置 tag 输入 | producer/query 由 pending-MMIO 专项拥有，本 plan 不用它直接改变 pass/fail |
| `pendingPtr` | driver control | 每拍来自 modeled ROB head 完整 key |
| `pendingst` | driver control | 当前 sideband head 是 scalar store 且无 global block 时置 1 |
| `pendingMMIOld` | driver control | 当前 sideband head 是 load 且 status MMIO tag 为 1 时置 1 |
| `scommit` | driver control | 只统计本拍真实 normal scalar store commit 数；fault 不计 |

### 文字伪代码

```text
构建本拍 lsqcommit xaction：
  先同步 modeled ROB head；
  用 modeled head 初始化 pendingPtr；
  如果当前处于 fault waiting：
    pendingst=0；
    pendingMMIOld=0；
    scommit=0；
    返回保持 fault head；

  解析 modeled head 的 active UID；
  如果 head valid 且无 global block：
    读取 main transaction 和 behavior；
    pendingst = behavior.commit_is_store；
    pendingMMIOld = behavior.commit_is_load && data.uid_is_mmio_load(head_uid)；
  否则：
    pendingst=0；
    pendingMMIOld=0；

  如果本拍有 normal commit batch：
    scommit = batch 中 scalar store commit 数；
  如果本拍是 fault convergence 或无 normal commit：
    scommit=0；

  loadMmio/storeMmio output tag 的采样、ROB value-only 反查和 tag 落表不在本函数实现；
  本函数只消费 pending-MMIO 专项提供的 uid_is_mmio_load() 结果。
```

## 3. 问题二：`pendingPtr` 仍被当作 commit batch tail

### V2 问题

V2 ROB 中 `pendingPtr := RegNext(deqPtr)`，语义是当前 ROB commit/deq head 的 sideband，不是本拍
commit batch tail。旧逻辑把 `pendingPtr` 写成本拍 batch 最后一个 UID 的 ROB key，或对 tail key
做加一，manual 主表 ROB key 不连续时会驱动错误 head。

### 修改原因

`pendingPtr/pendingst/pendingMMIOld` 是 level sideband，必须来自独立 modeled ROB head。normal
commit 成功后才能把 UID cursor 推进到 batch tail 的下一 UID，再从权威 status 读取下一完整 ROB key。
不能用 ROB key 算术推导。

### 修改方案与修改逻辑

`lsq_commit_handler` 新增并唯一维护以下私有状态：

```text
memblock_rob_key_t modeled_rob_deq_ptr
bit modeled_rob_deq_ptr_initialized
bit modeled_rob_deq_ptr_valid
bit fault_head_waiting
memblock_uid_t fault_head_uid
int unsigned fault_head_dynamic_epoch
```

`initialized` 表示本轮 main table 已尝试初始化；`valid` 表示当前有可驱动的完整 head key。两者不得
混用。cursor/head 同步只允许由 `rebase_framework_head_from_commit_cursor()` 完成；该 helper 是
`advance_commit_cursor_past_done()` 的唯一调用者。

### 文字伪代码

```text
reset_lsqcommit_runtime_state()：
  commit_cursor_uid=0；
  modeled_rob_deq_ptr=0；
  modeled_rob_deq_ptr_initialized=0；
  modeled_rob_deq_ptr_valid=0；
  fault_head_waiting=0；
  fault_head_uid=0；
  fault_head_dynamic_epoch=0；
  只清 handler 私有状态，不写 status、active map 或 LSQ pointer。

ensure_modeled_rob_deq_ptr_initialized()：
  如果 initialized=1，直接返回；
  如果 main_table_ready=0，fatal；
  如果 fault_head_waiting=1，fatal；
  调用 rebase_framework_head_from_commit_cursor()；
  initialized=1；
  返回时 initialized 必须为 1。

rebase_framework_head_from_commit_cursor()：
  如果 fault_head_waiting=1，fatal；
  调用 advance_commit_cursor_past_done() 跳过连续 terminal_done 前缀；
  如果 commit_cursor_uid > main_trans_num，fatal；
  如果 commit_cursor_uid == main_trans_num：
    modeled_rob_deq_ptr=0；
    modeled_rob_deq_ptr_valid=0；
    返回；
  status = data.get_status(commit_cursor_uid)；
  next_key = status.get_rob_key()；
  检查 next_key 位宽和合法性；
  modeled_rob_deq_ptr = next_key；
  modeled_rob_deq_ptr_valid=1；
  不对旧 key 或 batch tail key 做 rob_advance/key+1。

resolve_sideband_head_uid(output uid)：
  如果 initialized=0，fatal；
  如果 modeled_rob_deq_ptr_valid=0，返回 0；
  用 modeled_rob_deq_ptr 查 active ROB map；
  未命中表示暂时无 active head，返回 0；
  命中后读取 status；
  要求 status.active=1 且未 terminal/flushed/killed；
  要求 status.get_rob_key() 等于 modeled_rob_deq_ptr；
  全部成立时写 uid 并返回 1。
```

## 4. 问题三：fault head 被 normal commit batch 提前跨过

### V2 问题

旧 `select_rob_commit_batch()` 会把 fault candidate 放进 normal batch tail，随后
`mark_rob_commit_batch()` 推进 `commit_cursor_uid/modeled_rob_deq_ptr`。如果 fault 的 LQ/SQ mapping
尚未真实 deq，框架会越过 fault head，掩盖缺 deq 死锁。

### 修改原因

V2 fault/exception head 不产生 normal `commitValid/scommit`，也不会按 normal commit 推进 ROB
deqPtr。测试框架的 `status.rob_commit` fault token 只服务 terminal convergence，不等价于 DUT
normal commit。

### 修改方案与修改逻辑

normal commit 和 fault convergence 分成互斥路径：

- `select_rob_commit_batch()` 只返回从 cursor 开始的连续 normal candidate，遇 fault head 立即停止。
- `select_fault_head_candidate()` 只在 normal batch 为空时检查当前 cursor/head 是否为同一 fault
  candidate。
- `mark_rob_commit_batch()` 对 normal batch 先全批预检查，再逐 UID 落表；全批成功后才以
  `batch_last_uid+1` 推进 cursor 并 rebase head。
- `mark_fault_rob_commit_uid()` 只置 fault `rob_commit` token、记录 waiting uid/epoch，并调用
  `try_retire_committed_uid()`；不写 `lsq_deq`，不推进 cursor/head，不产生 `scommit`。
- `sync_modeled_head_after_fault_terminal()` 在每次 builder 首步执行。只有完整 fault terminal tuple
  成立后，才清 waiting 并以 `fault_uid+1` 执行框架 head rebase。

### 文字伪代码

```text
select_rob_commit_batch(output normal_uids)：
  清 normal_uids；
  如果 cursor 越界，fatal；
  如果 cursor 到表尾，返回空；
  读取 cursor status；
  如果 status.terminal_done=1，fatal，说明 rebase owner 漏同步；
  如果 global flush 或 fault_head_waiting，返回空；
  uid = commit_cursor_uid；
  while uid < main_trans_num 且 normal_uids 未达到 commit width：
    如果 uid 是 fault terminal candidate，停止；
    如果 uid 不是 normal commit candidate，停止；
    normal_uids.push_back(uid)；
    uid++；
  函数不修改 cursor、modeled head、status 或 waiting。

select_fault_head_candidate(output fault_uid)：
  清 fault_uid；
  如果 global flush，返回 0；
  如果 fault_head_waiting=1，fatal；
  如果 cursor 到表尾，返回 0；
  如果 cursor status terminal_done=1 或 rob_commit=1，fatal；
  如果 cursor uid 不是 fault candidate，返回 0；
  调用 resolve_sideband_head_uid()；
  要求 resolved_uid == commit_cursor_uid；
  要求 status.get_rob_key() == modeled_rob_deq_ptr；
  写 fault_uid 并返回 1；

mark_rob_commit_batch(normal_uids)：
  空 batch 直接返回；
  预检查 fault_head_waiting=0；
  预检查 normal_uids[0] == commit_cursor_uid；
  预检查 batch head key 等于 modeled head；
  预检查所有 uid 按主表 UID 连续，且都是 normal candidate；
  预检查没有任何 fault candidate；
  foreach uid：
    调用 mark_rob_commit_uid(uid)；
    如果返回 0，fatal；
  commit_cursor_uid = batch_last_uid + 1；
  调用 rebase_framework_head_from_commit_cursor()。

mark_fault_rob_commit_uid(fault_uid)：
  如果 build/mark 间出现 global flush，返回 0 且不写状态；
  要求 waiting=0；
  要求 fault_uid == commit_cursor_uid；
  要求 resolve_sideband_head_uid() 命中同一 uid；
  要求 status 仍是未置 token 的 fault candidate；
  status.rob_commit=1；
  fault_head_waiting=1；
  fault_head_uid=fault_uid；
  fault_head_dynamic_epoch=status.dynamic_epoch；
  调用 data.try_retire_committed_uid(fault_uid)；
  返回 1。

sync_modeled_head_after_fault_terminal()：
  先 ensure modeled head 已初始化；
  如果没有 waiting：
    如果 cursor status 已 terminal_done，调用 rebase helper；
    返回 0；
  读取 fault_head_uid 的 status；
  如果 dynamic_epoch 改变、redirect/reissue 清旧实例、flushed/killed 或 token 已被清：
    清 waiting 和旧 modeled valid；
    cursor 保持在同一 uid，不越过；
    如果同 uid 新实例已 active，调用 rebase helper 重新绑定 key；
    返回 0；
  如果 terminal tuple 不完整：
    保持 waiting 和 fault pendingPtr；
    返回 0；
  terminal tuple 必须满足：
    terminal_done=1、lsq_deq=1、active=0、success=0、fault=1、
    active_lq_mapped=0、active_sq_mapped=0；
  满足后清 waiting；
  commit_cursor_uid = fault_uid + 1；
  调用 rebase helper；
  返回 1，表示发生 fault terminal rebase progress。
```

## 5. 问题四：V2 只有 `sqDeq` count，没有 `sqDeqPtr`

### V2 问题

V2 顶层只输出 `sqDeq[1:0]`，没有 `sqDeqPtr`。旧 adapter 仍可能把 raw struct 中默认 0 的
`sq_deq_ptr_*` 传给 pointer helper，等价于把不存在的 pointer 当成真实 DUT payload。

### 修改原因

SQ deq pointer 是否存在是 compile-time capability。V2 必须使用软件 `sq_deq_ptr` 作为起点做
count-only 释放；有 pointer 的 profile 才能读取 pointer payload。capability/presence 检查必须由
唯一 full-raw owner 执行，不能散落在 adapter、collector 或 helper 调用点。

### 修改方案与修改逻辑

`dispatch_monitor_event_adapter::apply_raw_ctrl_deq(raw)` 只转发完整 raw 到
`lsq_commit_handler::apply_raw_ctrl_deq(raw)`。handler wrapper 是唯一 owner：

- 更新 `sb_is_empty`。
- 检查 `sq_deq_ptr_valid` 与 `` `MEMBLOCK_DUT_HAS_SQ_DEQ_PTR`` 的一致性。
- 选择 LQ pointer helper、SQ pointer helper 或 V2 SQ count-only helper。

新增 `lsq_commit_handler::apply_dut_sq_deq_count_only(count)`。该 helper 先预检查连续 count 个
software SQ head key 都有 active mapping 且对应 UID 已 `rob_commit`，全部通过后才 release pointer、
删除 map、调用 retire，并递增 SQ deq event sequence 一次。

### 文字伪代码

```text
dispatch_monitor_event_adapter::apply_raw_ctrl_deq(raw)：
  ensure monitor_commit_handler 可用；
  调用 monitor_commit_handler.apply_raw_ctrl_deq(raw)；
  adapter 不读取 sq_deq_ptr_valid/flag/value；
  adapter 不直接调用 apply_dut_lq_deq/apply_dut_sq_deq；

lsq_commit_handler::apply_raw_ctrl_deq(raw)：
  update_sb_is_empty(raw.sb_is_empty)；
  如果 raw.sq_deq==0 且 raw.sq_deq_ptr_valid=1，fatal；
  如果 MEMBLOCK_DUT_HAS_SQ_DEQ_PTR=0 且 raw.sq_deq_ptr_valid=1，fatal；
  如果 MEMBLOCK_DUT_HAS_SQ_DEQ_PTR=1 且 raw.sq_deq!=0 且 raw.sq_deq_ptr_valid=0，fatal；

  如果 raw.lq_deq!=0：
    用 raw.lq_deq_ptr 构造 LQ key；
    调用 apply_dut_lq_deq(raw.lq_deq, lq_key)；

  如果 raw.sq_deq!=0 且 MEMBLOCK_DUT_HAS_SQ_DEQ_PTR=1：
    用 raw.sq_deq_ptr 构造 SQ key；
    调用 apply_dut_sq_deq(raw.sq_deq, sq_key)；
  否则如果 raw.sq_deq!=0：
    调用 apply_dut_sq_deq_count_only(raw.sq_deq)；

apply_dut_sq_deq_count_only(count)：
  如果 count=0，返回；
  如果 MEMBLOCK_DUT_HAS_SQ_DEQ_PTR=1，fatal；
  start_key = lsq_ctrl.sq_deq_ptr；
  清临时 deq_uids；
  对 idx=0..count-1：
    key = advance_sq_key(start_key, idx)；
    用 active SQ map 查 uid；
    要求唯一命中；
    要求 uid 未重复；
    要求 status.active_sq_mapped=1；
    要求 status.rob_commit=1；
    任一失败都 report_deq_mismatch 并返回，且不释放任何前缀；
  全部通过后：
    lsq_ctrl.release_sq(count)；
    foreach uid in deq_uids：
      data.release_uid_sq_mapping(uid)；
      data.try_retire_committed_uid(uid)；
    lsq_ctrl.note_sq_deq_event()；
```

## 6. 问题五：ctrl raw 过早应用会删除同批 event 的 active map

### V2 问题

ctrl raw 同时包含 `memoryViolation`、LQ/SQ deq 和 `sbIsEmpty`。如果 collector 在 semantic
event claim/handler 前直接应用 deq，就可能删除同一 service batch 中 IQ/WB event 解析所需的
active map。

### 修改原因

semantic event 的 redirect-first 仲裁和 allowed-event handler 必须先完成，再应用本 batch 的
deferred full raw。即使 semantic events 为空或 normalize 后为空，也不能跳过 count=0 raw 的
`sb_is_empty` 更新。

### 修改方案与修改逻辑

`collect_monitor_event_batch()` 使用栈帧内 automatic `deferred_ctrl_updates[$]` 保存完整 ctrl raw：

1. collect WB/IQ semantic events。
2. collect ctrl semantic events，并把完整 ctrl raw 按 pop 顺序写入 deferred queue。
3. process semantic batch。
4. apply deferred ctrl updates，逐项调用 full-raw wrapper。

pending-MMIO 专项只拥有 ctrl collector 内的 MMIO tag、memoryViolation 转换和 deferred raw 生产。
本 plan 拥有总编排、deferred consumer 和 LQ/SQ/SB 状态推进。

### 文字伪代码

```text
memblock_dispatch_base_sequence::collect_monitor_event_batch()：
  automatic events[$]；
  automatic deferred_ctrl_updates[$]；

  monitor_adapter.collect_writeback_events_batch(events)；
  monitor_adapter.collect_ctrl_redirect_events_batch(events, deferred_ctrl_updates)；
  monitor_batch_handler.process_monitor_event_batch(events)；
  monitor_adapter.apply_deferred_ctrl_updates_batch(deferred_ctrl_updates)；
  task 返回后，上层再调用既有 exception_redirect_replay_task() apply redirect；

dispatch_monitor_event_adapter::apply_deferred_ctrl_updates_batch(ref deferred_ctrl_updates)：
  while deferred_ctrl_updates 非空：
    raw = deferred_ctrl_updates[0]；
    调用 apply_raw_ctrl_deq(raw)；
    只有 full-raw 调用正常返回后，才 pop_front；
  函数末检查 queue 为空；
  函数不缓存 UID，不展开 count/pointer，不直接读 active map；

顺序合同：
  semantic events 为空也必须进入 deferred consumer；
  count=0 raw 也必须调用 full-raw wrapper，以更新 sb_is_empty；
  actual redirect 不能在 deferred apply 前删除本批 claim 所需 map；
  如果 map 已被先前 batch redirect 删除，full-raw wrapper 按当前 map 预检查和 mismatch policy 处理，
  不使用采样时 UID 强制 release 或复活 entry。
```

## 7. 问题六：active idle 清零 sideband 且 progress 被 busy level 伪造

### V2 问题

`pendingPtr/pendingst/pendingMMIOld` 是 level sideband。旧 driver 在 no-item/gap 周期调用通用
`drive_idle()`，可能把 sideband 清零；主动 loop 又把 `flushsb_busy()` 或重复 pending level 当成
progress，掩盖 fault 缺真实 deq 的 blocker。

### 修改原因

lsqcommit 是主动发射 flow。无 item/gap 不能破坏上一拍有效 head sideband；长期无真实推进应持续
报 no-progress error，并最终由 UVM timeout 暴露，而不是靠 busy level 重置 idle_count。

### 修改方案与修改逻辑

driver 新增 active sideband cache：

```text
active_sideband_cache_valid
cached_pending_ptr_flag
cached_pending_ptr_value
cached_pending_mmio_ld
cached_pending_st
```

`send_pkt()` 驱动 item 后调用 `cache_active_sideband()`。active main phase 的 no-item/gap 调用
`drive_active_idle()`，保持四个 level 字段，只清 `scommit/flushSb` pulse。reset 路径调用
`clear_active_sideband_cache()`，继续使用 configured idle。

`lsq_ctrl_model` 新增 `lq_deq_event_seq/sq_deq_event_seq`，只在 nonzero DUT deq 完整成功后由
唯一 note helper 递增一次。sequence `observe_lsqcommit_progress_edges()` 只比较四项固定 snapshot：
两个 deq event sequence、`flushsb_waiting_empty`、`active_flushsb_req_valid`。不读取 free count、
pointer、terminal prefix 或 active map。

### 文字伪代码

```text
driver cache_active_sideband(tr)：
  tr 为空时 fatal；
  缓存 tr.pendingPtr flag/value；
  缓存 tr.pendingMMIOld；
  缓存 tr.pendingst；
  active_sideband_cache_valid=1；

driver drive_active_idle()：
  如果 cache valid：
    重驱 cached pendingPtr/pendingMMIOld/pendingst；
  否则：
    驱 pendingPtr/pendingMMIOld/pendingst 为 0；
  无论 cache 是否 valid：
    驱 scommit=0；
    驱 flushSb=0；

driver reset_phase()：
  第一拍 reset drive 前调用 clear_active_sideband_cache()；
  reset/configured idle 继续走原 drive_idle()；

lsq_ctrl_model::note_lq_deq_event()/note_sq_deq_event()：
  如果对应 sequence 已到全 1，fatal；
  sequence++；
  不修改 pointer、free count、status 或 map；
  count>1 仍只加 1，表示一次 DUT deq event。

observe_lsqcommit_progress_edges()：
  current = {lq_deq_event_seq, sq_deq_event_seq,
             flushsb_waiting_empty, active_flushsb_req_valid}；
  如果 current 含 X/Z，fatal；
  如果没有 last snapshot：
    保存 current，返回 0；
  edge = current != last；
  保存 current；
  返回 edge；

send_lsqcommit_cycle(output has_progress, output terminal_idle_published)：
  build xaction，builder 首步同步 fault terminal；
  可选驱动 flushSb pulse，并把 flushSb driven 记为当拍 edge；
  start_item/finish_item 完成 driver send_pkt/cache；
  如果有 normal batch，调用 mark_rob_commit_batch()；
  否则如果有 fault candidate，调用 mark_fault_rob_commit_uid()；
  external_edge = observe_lsqcommit_progress_edges()；
  has_progress = normal_commit_marked ||
                 fault_token_marked ||
                 fault_terminal_rebased ||
                 flushsb_driven_edge ||
                 external_edge；
  terminal_idle_published 只有在本拍 item 已发送且 sideband 全 0、cursor 到表尾、
    waiting=0、modeled head invalid、flushSb 无 pending 时置 1；
  不把重复 pending level、flushsb_busy level 或 flushsb_request_pending level计为 progress。

drive_lsqcommit_loop()：
  进入 loop 前 prime 一次 progress snapshot；
  forever：
    先调用 send_lsqcommit_cycle()；
    有 progress 时 idle_count=0；
    无 progress 时 idle_count++；
    如果 no_progress_warn_cycles!=0 且 idle_count 是阈值整数倍：
      报 uvm_error，并继续运行，不清 idle_count，不 break；
    本轮发送完成后才检查正常退出：
      global_stop_requested=1；
      terminal_done_uid >= main_trans_num；
      fault_head_waiting=0；
      commit_cursor_uid == main_trans_num；
      modeled_rob_deq_ptr_initialized=1；
      modeled_rob_deq_ptr_valid=0；
      terminal_idle_published=1；
      flushsb_request_pending()=0；
    满足全部条件才 break。
```

## 8. Coding 落点汇总

| 文件 | 对应问题与修改 |
|---|---|
| `mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv` | 问题二至五：modeled head、normal/fault 分流、full-raw owner、SQ count-only、fault sync |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_ctrl_model.sv` | 问题六：LQ/SQ deq event sequence 和 note helper |
| `mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqcommit_dispatch_base_sequence.sv` | 问题一、三、六：build/send/loop、progress snapshot、terminal idle 发布 |
| `mem_ut/ver/ut/memblock/agent/lsqcommit_agent_agent/src/lsqcommit_agent_agent_driver.sv` | 问题六：active sideband cache、active idle hold |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv` | 问题四、五：full-raw forwarding 和 deferred ctrl consumer |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv` | 问题五：semantic batch 与 deferred full raw 总编排 |
| `mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_dispatch_smoke_sequence.sv` | 问题一、四：normal smoke 检查 pendingst/scommit 与 V2 count-only SQ deq |
| `mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_dispatch_fault_smoke_sequence.sv` | 问题三、六：fault-at-tail、redirect/reissue、final idle 和 driver hold directed 覆盖 |

明确不修改：

```text
dispatch_raw_ctrl_t 字段定义、make_empty_raw_ctrl()、ctrl interface sample_sq_deq_ptr() accessor、
ctrl monitor 对 MMIO tag 和 sq_deq_ptr_valid 的生产逻辑
pass/fail/fault/terminal_done 定义
RM、scoreboard、checker、coverage
AI_DOC/mem_ut_flow_doc 下 flow 文档
其它专项 plan
```

## 9. 修改类型与原逻辑对比总结

| 修改项 | 类型 | 修改前逻辑 | 修改原因 | 修改后逻辑 |
|---|---|---|---|---|
| output 分类 | 范围收敛 | MMIO/status 字段职责混杂 | 防止 output 观察字段进入 pass/fail | 明确 FLOW_REQUIRED、driver control 和 pending-MMIO owner |
| `pendingPtr` 来源 | 功能逻辑修改 | 使用 batch tail 或 key+1 | V2 语义是当前 ROB deq head | modeled head 从 cursor UID 的权威 status 读取完整 ROB key |
| cursor/head owner | 状态生命周期修改 | selector 或 per-uid mark 可推进 cursor | 容易 head 与 cursor 分叉 | `rebase_framework_head_from_commit_cursor()` 是唯一同步 owner |
| normal commit | 功能逻辑修改 | fault 可混入 batch tail | fault 不是 DUT normal commit | normal batch 只含连续 normal candidate，全批成功后 rebase |
| fault convergence | 功能逻辑修改 | fault token 置位后立即推进 | 会掩盖缺真实 LSQ deq | fault waiting 保持 head，完整 terminal tuple 后才框架 rebase |
| V2 SQ deq | 接口适配 | 读取默认 0 的不存在 pointer | V2 无 `sqDeqPtr` | capability 检查后走 count-only 预检查与原子 release |
| ctrl raw 应用顺序 | 功能逻辑修改 | deq 可能早于同批 event claim | 会删除同批 active map | semantic batch 后再 deferred full-raw apply |
| driver idle | 时序逻辑修改 | active no-item/gap 调通用 idle 清 sideband | level sideband 不能被气泡清零 | active idle 重驱 cache，只清 pulse |
| progress 统计 | 性能/退出逻辑修改 | busy level 或 free count 变化计 progress | 会掩盖 blocker 或被 allocate/cancel 伪造 | 只看 normal commit、fault token、fault rebase、真实 deq event 和 flushSb 边沿 |
| no-progress | 失败暴露修改 | 阈值 warning 后清计数或退出 | 主动 flow 无推进不是正常退出 | 阈值整数倍报 `uvm_error`，继续等待 UVM timeout |

保持不变的主体逻辑：main table/status 基本字段、`try_retire_committed_uid()` 和
`consume_fault_retire()` 的 terminal 形成、LQ pointer deq 主体、flushSb request owner、redirect/replay
handler、pass/fail 语义。
