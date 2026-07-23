# mem_ut V2 LSQ MMIO/Status 适配最终 Coding Plan

| 项目 | 内容 |
|---|---|
| 状态 | `do`，coding、最终回归和独立终审均已完成并归档 |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| V2 接口权威 | `build_memblock/rtl/MemBlock.sv` |
| 测试框架入口 | `memblock_lsqcommit_dispatch_base_sequence::send_lsqcommit_cycle()`、`memblock_dispatch_base_sequence::collect_monitor_event_batch()` |
| 适配原则 | 区分 V2 ROB head sideband、normal commit、fault convergence 和真实 LSQ deq；不把 output 观察字段混入 pass/fail |
| 创建/修订日期 | 2026-07-21 |

## 1. 范围与边界

本 plan 只整理 V2 `loadMmio/loadMmioUop/storeMmio/storeMmioUop/pendingMMIOld/pendingst/scommit`
以及 `lqDeq/sqDeq/lqCancelCnt/sqCancelCnt/sbIsEmpty/memoryViolation` 对测试框架运行期状态的影响。
其中 `lqCancelCnt/sqCancelCnt` 只作为 redirect cancel 的延迟观测和对账输入，不作为第二套 LSQ
状态写者。除运行期状态外，本文还
明确 `sqDeq` count 的编译期宽度契约，避免 raw/interface/monitor 链继续把 V2 当前的 `[1:0]`
当成第二权威。每个问题均说明 V2 问题、修改原因、最终修改逻辑和可直接 coding 的文字伪代码。

本轮支持范围：

- V2 ROB -> LSQ sideband：`pendingPtr/pendingst/pendingMMIOld/scommit`。
- normal commit batch 与 fault convergence 互斥处理。
- V2 `sqDeq` count-only 回收；有 `sqDeqPtr` 的 profile 保留 pointer 分支。
- scalar redirect cancel 的 `software_cancel_lq/sq` 与 DUT `lqCancelCnt/sqCancelCnt` 延迟输出直接对账；
  同一 software count 既作为软件资源回退量，也作为 DUT compare 值，对账不改变 free count 的写者和
  `sqDeq` physical release owner。
- monitor ctrl raw 的 semantic event 与 LQ/SQ/SB 状态推进分离。
- lsqcommit active driver 在 no-item/gap 周期保持 level sideband。
- 主动 loop 保留现有轻量 `has_progress` activity/watchdog 语义；不新增精细 deq event 计数，
  flushSb 长时间不收敛由已有 flushSb timeout warning 与最终 UVM timeout 暴露。

本轮不支持：

- MMIO 正确性 checker、RM、scoreboard 或 coverage。
- PMP/L2TLB 顶层 output monitor。
- 修改 `terminal_done`、pass/fail/fault 定义。
- 把 `loadMmio/storeMmio` 直接接入 pass/fail 或 terminal。MMIO raw/tag producer 由
  `mem_ut_v2_pending_mmio_load_sideband_execution_plan_20260710.md` 唯一拥有。
- 本次 plan 整理阶段不直接修改 `AI_DOC/mem_ut_flow_doc`；后续按本 plan 完成 coding 后，必须按
  执行规则同步 `lsq_admission_flow.md`、`redirect_flow.md`、
  `rob_commit_lq_sq_deq_flow.md` 和 `virtual_sequence_unified_dispatch_flow.md` 的真实调用链。
  其它专项 plan 只保留依赖边界，不复制本 plan 的 pending 实现细节。

同一原子 coding 批次硬前置：

- compile/width 基线已经提供 `` `MEMBLOCK_DUT_HAS_SQ_DEQ_PTR``、ROB/LQ/SQ value width 等宏；本
  `undo` 专项是以下新增 compile delta 的唯一 coding owner：`MEMBLOCK_DUT_ENSBUFFER_WIDTH`、
  `MEMBLOCK_SQ_DEQ_COUNT_W`、LQ/SQ cancel count width、redirect-to-LSQ latency、DUT cancel output
  latency、monitor sample offset 和派生 observe latency。V2 profile 固定
  `MEMBLOCK_DUT_ENSBUFFER_WIDTH=2`、`` `MEMBLOCK_DUT_HAS_SQ_DEQ_PTR=0``；两份已归档
  compile/enqueue `do` plan 只保留依赖边界，不再拥有这些未实现字段或函数。
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
| `lqDeq/sqDeq/lqDeqPtr/sbIsEmpty` | `FLOW_REQUIRED` | raw ctrl full wrapper 消费，推进 LSQ map、free count、terminal 和 SB empty；`sqDeq` 只表示 SQ entry 释放数量 |
| `lqCancelCnt/sqCancelCnt` | `FLOW_REQUIRED` 的 redirect snapshot | ctrl monitor 每拍写专用 snapshot sideband，reconcile owner只在每个redirect compare target sample取一次并与该record的`software_cancel_lq/sq`直接对账；不得调用 `release_lq/release_sq` 或再次调用 `cancel_lq/cancel_sq` |
| `memoryViolation` | `FLOW_REQUIRED` | 作为 semantic redirect event 进入 redirect-first batch |
| `loadMmio/loadMmioUop` | `FLOW_REQUIRED` 的前置 tag 输入 | producer/query 由 pending-MMIO 专项拥有，本 plan 只通过 `uid_is_mmio_load()` 消费已落表 tag |
| `storeMmio/storeMmioUop` | `FLOW_REQUIRED` 的前置 tag 输入 | producer/query 由 pending-MMIO 专项拥有，本 plan 不用它直接改变 pass/fail |
| `pendingPtr` | driver control | 每拍来自 modeled ROB head 完整 key |
| `pendingst` | driver control | 当前 sideband head 属于V2 scalar ROB store commit分类且无 global block 时置1；该分类是`CommitType.STORE && !vls`，包含普通STU store和STU CBO |
| `pendingMMIOld` | driver control | 当前 sideband head 是 load 且 status MMIO tag 为 1 时置 1 |
| `scommit` | driver control | 只统计本拍真实normal scalar ROB store commit数；普通STU store和STU CBO计入，fault/vector不计；不推进软件`sq_deq_ptr`或SQ free count |

### `scommit` 与 `sqDeq` 的解耦合同

这两个字段必须使用完整方向名理解，不能因为下游 Bundle 中存在同名 `scommit` 而合并语义：

| 测试框架语义名 | V2 DUT 信号 | 方向 | 生产者与计数单位 | 测试框架唯一直接影响 |
|---|---|---|---|---|
| `rob_scommit_count` | `io_ooo_to_mem_lsqio_scommit` | ROB -> MemBlock 输入 | ROB 本拍 normal commit batch 中 scalar store `commitValid` 数量；不是 SQ 物理释放数 | normal batch 的全部 UID 都写 `status.rob_commit`；该字段只驱动其中 scalar store 子集的数量；不调用 `release_sq()` |
| `dut_sq_deq_count` | `io_mem_to_ooo_sqDeq` | MemBlock -> 后端输出 | StoreQueue 本拍连续完成并离开 SQ 的 entry 数量；最大值由 `MEMBLOCK_DUT_ENSBUFFER_WIDTH` 给出，字段宽度由 `MEMBLOCK_SQ_DEQ_COUNT_W=$clog2(MEMBLOCK_DUT_ENSBUFFER_WIDTH+1)` 派生 | 根据 raw `sqDeq` 查 SQ owner，调用 `release_uid_sq_mapping()` 和 `lsq_ctrl.release_sq(count)`；不设置 `status.rob_commit` |

完整 core 中 Dispatch/Scheduler 的某些 Bundle 将 MemBlock 的 `sqDeq` 接入名为
`scommit` 的下游字段；该字段仍然是 `dut_sq_deq_count`，不是 ROB 输入
`io_ooo_to_mem_lsqio_scommit`。测试框架内部 helper 和注释应优先使用方向名或别名，避免把两者
当作同一计数器。

V2 RTL 的生产条件和寄存器边界也不同：ROB 先从 `commitValid` 计算 scalar store 子集，
通过 `io.lsq.scommit := RegNext(...)` 产生 MemBlock 输入；StoreQueue 内部消费该输入前还有
`GatedRegNext(io.rob.scommit)`。另一方面，StoreQueue 的 `sqDeqCnt` 由队头
`allocated && completed` 连续条件产生，顶层通过 `io.sqDeq := RegNext(sqDeqCnt)` 输出。
SBuffer、uncache、MMIO/CBO response 和异常完成都可能改变 `completed` 的时间。因此二者没有同拍
相等合同，不能建立 `scommit == sqDeq` 断言，也不能用 `scommit` 直接推进 `sq_deq_ptr`。

`sqDeq` 的计数单位是 SQ entry，不是 ROB commit 数，也不是 SBuffer beat 数。一个非对齐 store
可能产生多个 DataBuffer/SBuffer beat，但仍按其 SQ entry 的一次物理出队计数；`sqDeq=2` 表示两个
连续队头 SQ entry 被释放。

`sqDeq` 的“最大值”和“字段宽度”必须分开管理：`MEMBLOCK_DUT_ENSBUFFER_WIDTH` 是来自 V2
`EnsbufferWidth` 的硬件结构参数，表示本拍 `sqDeqCnt` 可能表示的最大 SQ entry 数；
`MEMBLOCK_SQ_DEQ_COUNT_W` 只是由该参数派生的 packed width，精确复刻 RTL 的
`log2Ceil(EnsbufferWidth + 1)`。它不能复用 `MEMBLOCK_DUT_SQ_VALUE_W`、
`MEMBLOCK_DUT_SQ_SIZE`、`MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH` 或 ROB commit count width。

合法组合及处理如下：

| `scommit` | `sqDeq` | 语义 | 处理 |
|---:|---:|---|---|
| 0 | 0 | 没有 scalar store commit 或物理 SQ 释放；仍可能有 load/其他非 store normal commit | commit flow 如有 normal batch 仍照常处理；本拍不产生 SQ deq |
| >0 | 0 | ROB commit 先到，SQ entry 尚未完成 | 只更新 commit 状态和 sideband，等待后续 raw deq |
| 0 | >0 | 延迟 deq，或 MMIO/CBO 的 deq-before-commit | 只释放 SQ mapping/free count，不能补造 ROB commit |
| >0 | >0 | 两个独立事件恰好同拍到达 | 分别消费，数值不要求相等 |

文字伪代码：

```text
消费 ROB commit batch：
  normal batch 中的全部 UID 都标记 status.rob_commit；
  rob_scommit_count = 该 batch 中 scalar store UID 子集的数量；
  生成/发送 scommit 输入；
  不调用 lsq_ctrl.release_sq()；
  不修改 lsq_ctrl.lq_deq_ptr/sq_deq_ptr/lq_free_count/sq_free_count；

消费 ctrl raw：
  dut_sq_deq_count = raw.sq_deq；
  如果 dut_sq_deq_count > 0：
    按软件 sq_deq_ptr 查连续 SQ owner；
    原子释放 mapping，并调用 lsq_ctrl.release_sq(dut_sq_deq_count)；
    调用 try_retire_committed_uid()，但不设置 rob_commit；
  如果 dut_sq_deq_count == 0：
    不因 scommit 的值补发 SQ deq；

状态写者合同：
  status.rob_commit 只由 commit flow 写；
  对有 LQ/SQ mapping 的 UID，status.lsq_deq 只由真实 lqDeq/sqDeq mapping release 写；无 LSQ mapping 的
  non-LSQ UID 可由 commit flow 按“无资源需要释放”置 1，但不得按 scommit 数量批量置位；
  commit_cursor/modeled_rob_deq_ptr 只由 commit/fault rebase 写；
  lq_deq_ptr/sq_deq_ptr 只由 reset 和成功消费真实 lqDeq/sqDeq 的 release_lq/release_sq() 写；
  lq_free_count/sq_free_count 由 reset、enqueue allocation、redirect cancel 和真实 lqDeq/sqDeq release 写；
  success/terminal_done 仍由 try_retire_committed_uid() 在两条条件都满足后收口。
```

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
    pendingst = memblock_op_behavior_util::is_scalar_rob_store_commit(behavior)；
    pendingMMIOld = behavior.commit_is_load && data.uid_is_mmio_load(head_uid)；
  否则：
    pendingst=0；
    pendingMMIOld=0；

  如果本拍有 normal commit batch：
    scommit = batch 中 is_scalar_rob_store_commit(behavior) 为1的数量；
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

这里的约束不是“LQ/SQ 必须先 deq，fault 才能提交”。fault 指令不存在 normal architectural
commit；当前实现只是复用 `status.rob_commit` 保存“fault 已到 ROB head”的框架 token。该 token 与
真实 `lqDeq/sqDeq` 是两个独立事件，二者没有先后要求。需要等待二者全部完成的是 fault terminal 和
modeled ROB head rebase，而不是任一事件本身。

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

### Fault token 与 LQ/SQ deq 独立顺序合同

本文后续把“保存在 `status.rob_commit` 中的 fault token”记为 `fault_head_seen` 语义别名；这只是
文档和日志语义，不新增状态字段，也不把它解释成 DUT normal `commitValid`。

| 观察顺序 | 合法性 | 状态处理 |
|---|---|---|
| `fault_head_seen` 先于 `lqDeq/sqDeq` | 合法 | 先记录 fault token 和 waiting；deq 到达后释放 mapping，再尝试 terminal |
| `lqDeq/sqDeq` 先于 `fault_head_seen` | 合法 | 先释放 mapping 并记录 `lsq_deq`；因 token 未到暂不 terminal，后续 token 到达后再次尝试收口 |
| 两者同一 service batch 到达 | 合法 | semantic fault 处理和 deferred raw deq 按既有编排依次落表；该软件执行顺序不构成 DUT 时序约束 |

固定 coding 合同：

- `select_fault_head_candidate()` 和 `mark_fault_rob_commit_uid()` 不得把 `status.lsq_deq` 或 mapping
  已释放作为记录 fault token 的前置条件。
- `apply_dut_lq_deq()`、pointer 型 `apply_dut_sq_deq()` 和 V2
  `apply_dut_sq_deq_count_only()` 不得把 `status.rob_commit`、`fault_head_waiting` 或本拍
  `scommit` 作为接受真实 deq 的前置条件；它们只按 pointer/count、active owner 和动态实例合法性
  释放 mapping。
- 每次 fault token 或真实 deq 落表后都调用既有 `try_retire_committed_uid()`；条件未齐时只等待，
  不补造另一事件。
- 只有 `sync_modeled_head_after_fault_terminal()` 等待
  `fault_head_seen && fault 状态完整 && LSQ mapping 已释放`，完整 terminal tuple 成立后才推进
  `commit_cursor_uid/modeled_rob_deq_ptr`。这是一条 convergence barrier，不是 deq/commit 顺序规则。

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
  不要求 status.lsq_deq=1，也不要求 LQ/SQ mapping 已释放；
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
  不读取 status.lsq_deq 作为前置条件；
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
  fault token 和真实 deq 可以按任意顺序到达；这里只检查二者最终是否全部收敛；
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

本问题还必须与 ROB 输入 `scommit` 解耦：`scommit` 是 scalar store 的 ROB commit 数量，
`sqDeq` 是已完成并离开 SQ 的 entry 数量。二者可能跨多个周期到达，不能用 `scommit` 推进
`sq_deq_ptr`，也不能要求两者同拍或数值相等。

### 修改方案与修改逻辑

`dispatch_monitor_event_adapter::apply_raw_ctrl_deq(raw)` 只转发完整 raw 到
`lsq_commit_handler::apply_raw_ctrl_deq(raw)`。handler wrapper 是唯一 owner：

- 更新 `sb_is_empty`。
- 检查 `sq_deq_ptr_valid` 与 `` `MEMBLOCK_DUT_HAS_SQ_DEQ_PTR`` 的一致性。
- 选择 LQ pointer helper、SQ pointer helper 或 V2 SQ count-only helper。

新增 `lsq_commit_handler::apply_dut_sq_deq_count_only(count)`。该 helper 先预检查连续 count 个
software SQ head key 都有唯一 active mapping，且对应 status 仍属于当前 active 实例；预检查不得把
`status.rob_commit` 作为 SQ raw deq 的硬门槛。全部通过后才 release pointer、删除 map、调用
`try_retire_committed_uid()`；不新增或递增仅为 `has_progress` 服务的 SQ deq event sequence。

`status.rob_commit` 只属于测试框架的 ROB commit/最终 retire 状态，不等价于 DUT 已经观测到的
`lqDeq/sqDeq`；fault 路径复用该字段时，其语义只是 `fault_head_seen`。V2 `sqDeqCnt` 的 RTL 消费条件
是 SQ entry 已 allocated 且 completed；MMIO/CBO 路径还可能在 ROB `commitValid/scommit` 到达前先
完成并产生 `sqDeq`。因此现有 `apply_dut_lq_deq()`、pointer 型 `apply_dut_sq_deq()` 与 V2
count-only 分支必须采用同一条契约：真实 raw deq 独立释放对应 LSQ mapping，不等待 normal commit
或 fault token；`try_retire_committed_uid()` 内部继续等待 `status.rob_commit` 后才允许
`success/terminal_done`。这保持现有 LQ/SQ pointer 路径行为，只约束新增 count-only 路径不得引入
额外顺序门槛。

这只是现有 LSQ commit/deq flow 的职责解耦，不新增第二套 commit 状态机，也不改变 normal commit
candidate 的选择条件。

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
  raw.sq_deq 的消费不读取或等待本拍 scommit；
  不因 scommit 非零直接调用 release_sq()；

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
    要求 status.active=1；
    要求 status.active_sq_mapped=1；
    不要求 status.rob_commit=1；deq-before-commit 是允许的 DUT 观察顺序；
    任一失败都 report_deq_mismatch 并返回，且不释放任何前缀；
  全部通过后：
    lsq_ctrl.release_sq(count)；
    foreach uid in deq_uids：
      data.release_uid_sq_mapping(uid)；
      data.try_retire_committed_uid(uid)；
```

### SQ deq 先于 ROB commit 时的状态生命周期

```text
收到真实 sqDeq raw，且 SQ head/count/active mapping 预检查通过：
  release_uid_sq_mapping(uid)；
  status.active_sq_mapped=0；
  status.lsq_deq = (LQ/SQ mapping 均已释放)；
  保留 status.active=1 和 status.rob_commit=0；
  try_retire_committed_uid(uid) 看到 rob_commit=0 时直接等待，不置 success/terminal_done；

后续收到合法 ROB commit：
  mark_rob_commit_uid(uid) 置 status.rob_commit=1；
  再次调用 try_retire_committed_uid(uid)；
  若 writeback/pass/异常条件均已满足，则正常形成 success 或 fault terminal；

若先 ROB commit 后 SQ deq：
  仍沿用现有顺序，mark_rob_commit_uid() 先记录 commit，真实 deq 再释放 mapping，
  最后由 try_retire_committed_uid() 收口。

任一顺序都不得：
  因 rob_commit=0 丢弃真实 sqDeq raw；
  因 SQ mapping 已释放提前推进 commit_cursor/modeled_rob_deq_ptr；
  因 deq 事件直接置 success/terminal_done。
```

### 问题四-B：`sqDeq` count 链仍固定为 `[1:0]`

#### V2 问题

V2 当前 `EnsbufferWidth=2`，所以 `io_mem_to_ooo_sqDeq` 暂时显示为 `[1:0]`；但 Scala/RTL 的
真实定义是 `UInt(log2Ceil(EnsbufferWidth + 1).W)`。当前测试框架仍在
`dispatch_raw_ctrl_t.sq_deq`、ctrl interface、xaction、monitor 临时变量和 monitor X/Z 检查中
重复写 `[1:0]`/宽度 `2`。这会使未来 profile 修改 `EnsbufferWidth` 时出现 interface、raw 和检查
逻辑之间的宽度分叉。

`dut_inst.sv` 是按当前 RTL 逐端口展开的具体边界，继续保留与当前 V2 RTL 一致的 `[1:0]`；它不属于
公共测试框架的第二参数权威。`io_mem_to_ooo_ctrl_agent_connect.sv` 只保留同宽直连，不新增宽度声明。
公共 agent/raw/helper 不得反向从 `dut_inst.sv` 复制该 literal。

权威依据：`src/main/scala/xiangshan/mem/MemBlock.scala:133` 定义
`log2Ceil(EnsbufferWidth + 1)` 的顶层 count 宽度，`src/main/scala/xiangshan/mem/lsqueue/StoreQueue.scala:191,334,347`
定义 `sqDeqCnt` 的同源宽度和寄存器输出，V2 `Parameters.scala:226` 给出 `EnsbufferWidth=2`。

`MEMBLOCK_DUT_HAS_SQ_DEQ_PTR` 是 pointer presence capability，只决定 `sqDeqPtr` 字段是否存在以及
pointer/count-only 分支，不能作为 `sqDeq` count width 的替代参数。以下组合都必须在设计上可表达：

```text
无 pointer + 2-bit count
有 pointer + 2-bit count
无 pointer + 3-bit count
有 pointer + 3-bit count
```

#### 修改原因

`sqDeq` 是一个独立的 count 总线。把它绑定到 SQ pointer 的存在性，会让没有 pointer 的 V2 profile
继续依赖固定 `[1:0]`，也会在未来有 pointer 但 count width 改变时误用 pointer 宏。该问题属于
字段表示和编译期参数链适配，不改变 `sqDeq` 的计数单位、count-only 释放顺序或
`scommit`/`sqDeq` 解耦逻辑。

#### 修改方案与修改逻辑

1. 在 `memblock_compile_params.svh` 由版本 profile 提供硬件结构主宏
   `MEMBLOCK_DUT_ENSBUFFER_WIDTH`，V2 默认值为 `2`。
2. 在同一编译期参数链只派生一次
   `MEMBLOCK_SQ_DEQ_COUNT_W = $clog2(MEMBLOCK_DUT_ENSBUFFER_WIDTH + 1)`；派生宏不得
   通过 plusarg、env cfg 或独立 runtime override 修改。`memblock_dispatch_types.sv` 必须暴露
   对应 typed localparam，且只能引用 compile 宏，不能复制默认数值。
3. 以下公共测试框架字段链全部消费 `MEMBLOCK_SQ_DEQ_COUNT_W`：ctrl interface/clocking、ctrl
   xaction 字段及其 constraint/automation/print/compare、ctrl monitor 临时变量、
   `dispatch_raw_ctrl_t.sq_deq` 以及 monitor 的 X/Z 检查宽度。xaction 若被默认 sequence 随机化，
   `sqDeq` 合法范围必须约束为 `0..MEMBLOCK_DUT_ENSBUFFER_WIDTH`。`dut_inst.sv` 保留 RTL 展开的具体宽度；
   `io_mem_to_ooo_ctrl_agent_connect.sv` 继续只做同宽连接，不建立第二个 literal width。
   现有空约束 `default_io_mem_to_ooo_sqDeq_cons` 必须改为：

   ```systemverilog
   io_mem_to_ooo_sqDeq inside {[0:`MEMBLOCK_DUT_ENSBUFFER_WIDTH]};
   ```
4. `MEMBLOCK_DUT_HAS_SQ_DEQ_PTR` 只参与 `sqDeqPtr` presence/validity 检查和 pointer/count-only
   分支；无论该值为 0 还是 1，`sq_deq` count 字段都必须存在并使用
   `MEMBLOCK_SQ_DEQ_COUNT_W`。
5. `check_compile_param_consistency()` 在运行期首次使用前检查
   `MEMBLOCK_DUT_ENSBUFFER_WIDTH > 0`、`MEMBLOCK_SQ_DEQ_COUNT_W > 0` 和派生公式一致。
   X/Z 检查仍由 monitor 在 raw 入队前完成；handler 收到 raw 后只做无符号转换，并防御检查
   `raw.sq_deq` 不超过 `MEMBLOCK_DUT_ENSBUFFER_WIDTH`。

#### 新增/修改 helper 合同

| helper | 目的与调用者 | 输入 | 输出/副作用 |
|---|---|---|---|
| `begin_lsq_reservation_launch()` | `confirm_lsq_candidates()` 在真实 launch 的 `commit_allocate()` 后调用，用动态实例 token 替代只按 UID 反查 | UID；当前 status/mapping | 返回单调 launch epoch；写 pending reservation metadata，不改 pointer/free count |
| `mark_lsq_reservation_sampled()` | `complete_v2_pending_sample()` 在下一 `finish_item()` 返回后调用，固定 sample 事实 | UID、launch epoch、统一 sample sequence | 校验 token/state，写 `sample_valid/sample_seq/DUT_VISIBLE`；不读 flush epoch、不开放 issue |
| `release_uid_lq_mapping()` / `release_uid_sq_mapping()` reservation增量 | 真实deq owner在最后一个LSQ mapping释放后结束visibility生命周期 | UID和真实deq raw | 清state/sample但保留launch epoch；不写cancel record/ROB/terminal |
| `get_dut_sample_seq()` | ctrl/redirect monitor 和 LSQ sample completion 统一标记同一 posedge | `$time` | 同一时刻复用序号、新时刻递增；不改业务状态 |
| `peek_latest_dut_sample_seq()` | negedge readiness/deadline 只读当前 sample 进度 | 无 | 返回当前序号；不递增、不更新时间 |
| `cancel_redirect_scan_ready()` | `advance_active_redirect()` 判断唯一扫描是否已到安全时点 | active record、anchor、sample/drain watermark | 返回 ready bit；不改 record/status/map |
| `collect_cancel_snapshots()` / `collect_redirect_sample_anchors()` | main service 单点 drain 两条时序 sideband | package raw queue | 写 bounded 本地队列/record anchor；不写 pass/fail/free count |
| `service_cancel_reconcile()` | main service 在 recovery 后对 exact target snapshot 做逐 record 对账和清理 | finalized record、snapshot ring、watermark | 写 observed/debug计数并清完成record；不调用 LSQ release/cancel |
| `request_global_stop_if_done()` | 防止 UID terminal 后提前停止 monitor service | 原 transaction done + cancel/anchor/raw/software pending | 只在全部收敛后置 global stop；不改变单 UID terminal |

#### 文字伪代码

```text
加载 V2 compile profile：
  读取 MEMBLOCK_DUT_ENSBUFFER_WIDTH=2；
  派生 MEMBLOCK_SQ_DEQ_COUNT_W=$clog2(MEMBLOCK_DUT_ENSBUFFER_WIDTH+1)=2；
  不从 plus/cfg 读取上述两个结构参数；
  如果主参数为0、派生宽度为0或公式不一致，check_compile_param_consistency() 直接 fatal；

展开 ctrl/raw 字段链：
  interface、xaction、monitor 临时变量和 dispatch_raw_ctrl_t.sq_deq
  全部声明为 MEMBLOCK_SQ_DEQ_COUNT_W；
  monitor X/Z 检查使用同一宽度；
  dut_inst 保留当前 RTL 展开的 [1:0] 边界；
  connect 只传递该信号，不重新声明或截断；

消费 raw sqDeq：
  先由 monitor 完成 X/Z 检查；
  handler 将 raw.sq_deq 作为无符号 count 读取，并检查 count<=MEMBLOCK_DUT_ENSBUFFER_WIDTH；
  count=0 只更新本拍其它 raw sideband，不调用 release；
  count>0 继续沿用现有 pointer 或 count-only 分支；
  分支选择只读取 MEMBLOCK_DUT_HAS_SQ_DEQ_PTR，绝不据此推导 count width；
  release、deq pointer、lq/sq free count、status 和 terminal 的更新逻辑保持原合同；
  cancel snapshot 只进入 reconcile owner，不参与上述 release。
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

`collect_monitor_event_batch()` 使用栈帧内 automatic `deferred_ctrl_updates[$]` 收集本拍完整 ctrl raw；
semantic batch 完成后，adapter 把这些 raw 追加到同步包中的持久
`deferred_raw_ctrl_q`，再按队首成功语义消费：

1. collect WB/IQ semantic events。
2. collect ctrl semantic events，并把完整 ctrl raw 按 pop 顺序写入 deferred queue。
3. process semantic batch。
4. 把本拍 deferred ctrl 追加到持久 FIFO。
5. 从持久 FIFO 队首调用 full-raw wrapper；只有 wrapper 返回成功才 pop，resync mismatch 返回失败并停止本拍消费。

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
  按原顺序把本拍deferred_ctrl_updates追加到memblock_sync_pkg::deferred_raw_ctrl_q；
  清空栈帧内deferred_ctrl_updates；
  while 持久FIFO非空：
    raw = 持久FIFO[0]；
    success = apply_raw_ctrl_deq(raw)；
    如果success=0：停止本拍消费，保留当前队首和全部后续raw；
    如果success=1：pop_front当前队首并继续；
  函数不缓存 UID，不展开 count/pointer，不直接读 active map；

lsq_commit_handler::apply_raw_ctrl_deq(raw)：
  严格模式的mismatch仍uvm_fatal；
  resync模式的mismatch在report warning后返回0；
  LQ/SQ联合预检和提交全部成功后返回1；

runtime drain：
  raw_monitor_queue_size同时统计raw_ctrl_q和deferred_raw_ctrl_q；
  持久FIFO仍有等待重试的raw时禁止global stop；

顺序合同：
  semantic events 为空也必须进入 deferred consumer；
  count=0 raw 也必须调用 full-raw wrapper，以更新 sb_is_empty；
  actual redirect 不能在 deferred apply 前删除本批 claim 所需 map；
  如果 map 已被先前 batch redirect 删除，full-raw wrapper 按当前 map 预检查和 mismatch policy 处理；
  strict模式停止于fatal，resync模式保留队首重试，不使用采样时 UID 强制 release 或复活 entry。
```

### 5-A. Redirect cancel、free count 与 DUT 输出对账

#### V2 问题

`lqDeq/sqDeq` 和 `lqCancelCnt/sqCancelCnt` 都是 MemBlock 输出，但它们改变的是不同方向的
LSQ 状态：deq 释放已经完成的队头 entry 并推进 deq pointer；cancel 清除 redirect 命中的年轻
entry，并回退 enqueue tail。当前软件模型已经在 `cancel_lq/cancel_sq()` 中恢复 free count，
但 ctrl monitor 只做了 cancel count 的局部采样和 X/Z 检查，没有逐拍 snapshot sideband，也没有验证
软件 rollback count 与 DUT observed count 是否一致。cancel count 不得补进按 semantic event 有条件入队的
`dispatch_raw_ctrl_t`，否则无法表达无 valid、零值也必须采集的 held output。

不能把 free count 本身与 cancel count 比较。对每个队列，逻辑关系是：

```text
free_after = free_before - alloc_count + physical_deq_count + cancel_count
```

因此本专项直接对账同一 redirect epoch 的 `software_cancel_lq/sq` 与延迟后的
`lqCancelCnt/sqCancelCnt`，而不是再计算一套重复的 `expected_cancel_lq/sq`，也不是把 DUT count
再次加到软件 free count 上。

#### V2 cancel count 的精确口径与采样边界

V2 `VirtualLoadQueue`/`StoreQueue` 的 cancel 输出不是“当前 active status 表中有多少条被
flush 的 UID”的直接拷贝。以顶层 redirect 被 DUT 在采样边界 `T0_top` 看到为锚点，V2
`RegNextWithEnable`使LSQ在`T0_lsq=T0_top+1`采样redirect。scalar-only本轮的 software cancel count
必须覆盖 LSQ 内部同一 `T0_lsq` 的两部分，并直接作为 DUT compare 值：

```text
software_cancel_lq(T0_lsq) =
    T0_lsq 之前已分配、且 robIdx.needFlush(redirect) 的 LQ entry 数
  + T0_lsq 当拍采样、且同一 redirect 命中的 LQ enqueue element 数

software_cancel_sq(T0_lsq) =
    T0_lsq 之前已分配、且 robIdx.needFlush(redirect) 的 SQ entry 数
  + T0_lsq 当拍采样、且同一 redirect 命中的 SQ enqueue element 数
```

这是对 `VirtualLoadQueue.lastCycleCancelCount + lastEnqCancel`、
`StoreQueue.lastCycleCancelCount + lastEnqCancel` 的软件镜像。当前 plan 明确只支持
`numLsElem=1` 的 scalar request，所以 entry 数和 element 数在本轮单位一致；任何
`numLsElem>1`、vector 或无法判断 DUT 是否已经采样的情况都必须沿用 scalar-only 的
`uvm_fatal`/专项边界，不能用 active UID 数量静默近似。

`T0_top/T0_lsq` 都不能用 `dispatch_service_cycle` 推测。LSQ driver 的 clock-first 合同规定：active request
launch 后保持在 VIF，下一次 `drv_cb` 边界先由 DUT 采样上一拍值，再由 driver 覆盖下一拍值；即使
该边界之前已经出现 redirect，上一拍已 launch 的 request 仍先按“已采样”分类。若 request 在
launch 前被 abort，则没有软件 reservation，也不计入 software cancel。

对账使用统一的 DUT sample sequence：ctrl monitor、redirect input monitor、LSQ pending-sample
completion 和其它需要建立该时序关系的组件都调用同一个
`memblock_sync_pkg::get_dut_sample_seq($time)`，按相同仿真
时刻返回同一个单调序号。独立 cancel snapshot/redirect anchor 中保存 `sample_seq`，reconcile record 保存 redirect input
实际被 DUT 采样的 `redirect_sample_seq`；`$time` 只用于日志，不能与
`dispatch_service_cycle` 混算。redirect input monitor 只上报轻量 sample anchor，不把该输入
再次送入 recovery/status 主队列。

为避免只靠 `status.active_lq_mapped/active_sq_mapped` 猜测 launch/sample 边界，
`status_transaction` 增加 framework-only 的 `lsq_reservation_state`、
`lsq_reservation_launch_epoch`、`lsq_reservation_sample_seq`、
`lsq_reservation_sample_valid` 和 `lsq_cancel_accounted_epoch`。状态枚举固定为：

```text
NONE
LAUNCHED_PENDING_SAMPLE
DUT_VISIBLE
CANCEL_ACCOUNTED
```

`commit_allocate()` 的现有 `void` 签名和单一 allocation owner 保持不变。真实 V2 LSQ sequence 在
每次 `commit_allocate()` 成功后，紧接着调用
`common_data_transaction::begin_lsq_reservation_launch(uid)`：该 helper 只为实际已 launch 的 LSQ UID
递增并返回稳定 `lsq_reservation_launch_epoch`，把状态置为 `LAUNCHED_PENDING_SAMPLE`。pending batch
使用新增的 `memblock_lsq_reservation_token_t` queue 保存 `(uid,reservation_launch_epoch)`，替换原来的
UID-only queue；原 `v2_pending_sample_epoch` 继续保存该 batch 的 dispatch/flush epoch，只用于后续
`complete_admission()` gate，不得与 reservation launch epoch 合并或互相比较。

下一次 `finish_item()` 返回意味着 LSQ driver 已先经过一个 `drv_cb` 边界。sequence 在该返回点只调用
一次 `memblock_sync_pkg::get_dut_sample_seq($time)`，再逐 token 调用
`mark_lsq_reservation_sampled(uid,launch_epoch,sample_seq)`，把同一动态实例置为 `DUT_VISIBLE`；之后才按
flush epoch 决定是否调用 `complete_admission()` 开放 issue。`request_launched=1` 后的 request 按
clock-first 合同必然经过下一 sample 边界，即使该边界同时存在 redirect，也由 RTL 的
`lastEnqCancel` 计数，所以本轮不建立“已 launch 但未 sample”的正常状态。launch 前 abort 不调用
`commit_allocate()` 和 `begin_lsq_reservation_launch()`；token missing、duplicate、late callback 或重发
实例不匹配全部 `uvm_fatal`。

redirect scan 只接受 `DUT_VISIBLE`：`sample_seq < redirect_lsq_sample_seq(T0_lsq)` 计入 allocated
entry，`sample_seq == T0_lsq` 计入 same-cycle enqueue；`sample_seq > T0_lsq` 表示 framework 在 flush
gate 后仍 launch 了不属于本次 redirect 的 request，直接 `uvm_fatal`，不能静默只做 software rollback。
`lsq_cancel_accounted_epoch` 防止同一 UID 在同一 redirect epoch 中重复计数。

`lsq_reservation_launch_epoch` 是每 UID 的单调动态实例号，只在 `reset_all_tables()` 清零。
redirect 的 `clear_uid_dispatch_result()` 可以清 reservation state/sample-valid/sample-seq，但必须保留
launch epoch；下一次 reissue launch 再递增。这样旧 callback 即使晚到，也会因 token/state 不匹配
fatal，不能写入新动态实例。真实 `lqDeq/sqDeq` release 在该 UID 最后一个 active LSQ mapping 被删除后，
也清 state/sample-valid/sample-seq但保留 launch epoch；因此 deq-before-commit 后再遇 redirect 的 UID
不再被误计 cancel，同时后续重发仍不能复用旧 token。

`status_transaction::snapshot_from_main()` 只复制主表中的 UID 和 ROB/LQ/SQ 静态 key，不得写
reservation state、launch epoch、sample-valid/sample-seq 或 cancel-accounted epoch。
`common_data_transaction::activate_uid()` 在首次 launch 和 redirect reissue 时都继续调用该轻量 snapshot，
但不能调用会执行 `status.reset()` 的初始化路径。`init_status_for_uid()` 只用于首次建表或
`reset_all_tables()` 管理的全局重置阶段，不得作为 reissue activation helper。真实 launch 的调用顺序固定为
`commit_allocate()`（内部调用 `activate_uid()/snapshot_from_main()`）之后立即调用
`begin_lsq_reservation_launch()`，保证新 token
在 key/map 激活后递增，同时保留旧实例 epoch 用于拒绝 late callback。

`lqCancelCnt/sqCancelCnt` 本身没有 `valid`。RTL 中 `redirectCancelCount` 使用 `RegEnable`，没有
新 redirect 时保持上一次值，因此“count 非零”或“count 发生变化”都不能作为新事件 valid：前者会把
同一个结果重复入队，后者会漏掉连续两次相同 count。最终方案使用 per-record target-sample snapshot：

- ctrl monitor 每个 post-reset DUT sample 都生成一条包含 LQ/SQ count 和 `sample_seq` 的
  `dispatch_raw_cancel_snapshot_t`，包括两个 count 都为 0 的周期。
- `raw_cancel_snapshot_q` 是 cancel 专用时序 sideband，不进入 semantic ctrl raw batch；
  `dispatch_raw_ctrl_t` 不新增同名 cancel 字段，避免形成第二 producer 和 held-level 误消费路径。
- reconcile owner 只在某个 record 的 `compare_snapshot_sample_seq` 取一次 snapshot。后续相同的
  held level 不会形成第二个 observation；下一个 redirect 即使 count 相同，也会在它自己的 target
  sample 再取一次。
- 没有 pending record 时，snapshot 只更新 held-level baseline。若 count 在非 compare target
  sample 发生变化，或 reset 后在任何 redirect target 建立前从 0 变为非 0，立即 `uvm_fatal`。

由于接口没有硬件 valid，连续两次 redirect 产生相同 count 时只能证明“各自 target sample 上数值
正确”，不能从端口证明内部寄存器在第二次重新写入；plan 不伪造额外 valid，也不把 held level 当作
新事件。这个边界必须写入 review，不影响本轮 software cancel 与可观测 value 的逐 epoch 对账。

#### Owner 合同

| 内容 | 唯一 owner | 规则 |
|---|---|---|
| redirect cancel record 创建 | `common_data_transaction::request_redirect_flush()` | 每个 framework redirect epoch 创建一个未绑定 sample anchor 的 record；不得由 ctrl/redirect monitor 创建第二个 recovery record |
| software cancel count 来源 | `common_data_transaction::apply_redirect_flush_range()` / `prepare_uid_for_redirect_reissue()` + LSQ reservation ledger | 原 active-window scan 仍是唯一 flush/状态清理入口；按 V2 RTL 的 allocated entry + 同拍 DUT-visible enqueue element 公式登记同一套 `software_cancel_lq/sq`；scan 时残留 `LAUNCHED_PENDING_SAMPLE` 直接 fatal，launch 前 abort 不登记 |
| 软件 free count 回退 | `memblock_lsqenq_dispatch_base_sequence::apply_pending_lsq_cancels()` -> `lsq_ctrl_model::cancel_lq/cancel_sq()` | 以 reconcile record FIFO 中的 `software_cancel_*_count` 为唯一待应用清单；每条 record 只应用一次，同时回退 enqueue pointer、增加对应 free count；兼容计数器只能是该 FIFO 的派生总和 |
| DUT cancel snapshot producer | `io_mem_to_ooo_ctrl_agent_agent_monitor` | 每个 post-reset sample 将 `lqCancelCnt/sqCancelCnt/sample_seq` 写入专用 `raw_cancel_snapshot_q`，零值也必须保留；禁止用非零/变化派生 valid |
| DUT cancel snapshot consumer | `dispatch_monitor_event_adapter::collect_cancel_snapshots()` | 每个主 service cycle 单点 drain snapshot 到 reconcile owner；不修改 status、LSQ pointer 或 free count，不从 semantic ctrl raw 重复生产 |
| redirect sample anchor producer | `redirect_agent_agent_monitor` | 顶层 `io_redirect_valid` sample 时写 `{level,rob_key,sample_seq}`；接口不携带framework-only `flush_itself`，只表达DUT可观测输入采样事实，不反灌recovery/status |
| redirect sample anchor consumer | `dispatch_monitor_event_adapter::collect_redirect_sample_anchors()` | FIFO绑定最老未锚定record，并校验可观测投影`{level,rob_key}`；record内部`flush_itself`保持原值，不按payload-only map猜epoch，不修改redirect状态 |
| software/observed 对账 owner | `common_data_transaction` 中的 cancel reconcile queue/helper | 先按 redirect sample anchor 固化 epoch，再按 sample sequence、固定 latency 和 FIFO 将 DUT observed count 与本record的software count直接比较；对账完成前不能丢弃记录或请求 global stop |
| physical deq owner | `lsq_commit_handler::apply_raw_ctrl_deq()` | 只消费 `lqDeq/sqDeq`，不得消费 cancel count |

`lq_free_count` 和 `sq_free_count` 都必须列入状态写者合同。cancel 观测值即使匹配成功，也不
再次调用 `cancel_lq/cancel_sq()`，否则会重复回退 pointer 和重复增加 free count。

#### Reconcile record 与生命周期

在 `memblock_dispatch_types.sv` 增加 framework-only 的 `memblock_cancel_reconcile_t`，至少保存：

```text
redirect_epoch
cancel_record_id
redirect_service_cycle
redirect_drive_done_valid
redirect_drive_done_service_cycle
state_flush_applied_service_cycle
anchor_deadline_service_cycle
redirect_sample_seq
redirect_anchor_valid
redirect_lsq_sample_seq
software_cancel_lq_count
software_cancel_sq_count
observed_lq_count
observed_sq_count
software_applied
observed_valid
active_scan_done
software_count_finalized
dut_cancel_update_sample_seq
compare_snapshot_sample_seq
deadline_sample_seq
```

`software_cancel_*_count` 和 record 内部累计值使用 `int unsigned`；独立snapshot/interface/xaction/monitor 的
DUT-facing cancel 字段使用由 LQ/SQ 容量派生的
`MEMBLOCK_LQ_CANCEL_COUNT_W`、`MEMBLOCK_SQ_CANCEL_COUNT_W`，不得继续保留独立的
`[6:0]`、`[5:0]` 第二权威。两个派生宽度、
`MEMBLOCK_DUT_REDIRECT_TO_LSQ_LATENCY`、`MEMBLOCK_DUT_CANCEL_OUTPUT_LATENCY`、
`MEMBLOCK_TB_CANCEL_MONITOR_SAMPLE_OFFSET`和派生的
`MEMBLOCK_CANCEL_SNAPSHOT_OBSERVE_LATENCY`均由本 `undo` 专项写入统一 compile header，并由
`memblock_dispatch_types.sv`只读暴露 typed localparam；不得在已归档 compile plan、agent、sequence、
runtime plus 或 cfg 中建立第二 owner。

ctrl xaction 中现有 `lqCancelCnt inside {[7'd0:7'd72]}`、
`sqCancelCnt inside {[6'd0:6'd56]}` 也必须删除 literal，分别改为
`0..MEMBLOCK_DUT_LQ_SIZE` 和 `0..MEMBLOCK_DUT_SQ_SIZE`。monitor 在 snapshot 入队前使用同一容量宏
检查 observed count；`prepare_uid_for_redirect_reissue()` 每次累计以及 record finalize 前检查
software count 不超过对应容量。超出说明字段截断、重复累计或 active map/free-count
账本失配，立即 `uvm_fatal`，不能依赖 packed width 静默容纳非法值。

V2 RTL 的 cancel output 具有固定寄存器延迟，但现有 monitor 的 clocking input 在时钟沿前采样，
不能把 RTL 更新边界直接当成 monitor 可见 snapshot 边界。compile profile 增加：

```text
MEMBLOCK_DUT_ENSBUFFER_WIDTH = 2
MEMBLOCK_SQ_DEQ_COUNT_W = $clog2(MEMBLOCK_DUT_ENSBUFFER_WIDTH + 1)  // V2为2
MEMBLOCK_LQ_CANCEL_COUNT_W = $clog2(MEMBLOCK_DUT_LQ_SIZE + 1)
MEMBLOCK_SQ_CANCEL_COUNT_W = $clog2(MEMBLOCK_DUT_SQ_SIZE + 1)
MEMBLOCK_DUT_CANCEL_OUTPUT_LATENCY = 2
MEMBLOCK_DUT_REDIRECT_TO_LSQ_LATENCY = 1
MEMBLOCK_TB_CANCEL_MONITOR_SAMPLE_OFFSET = 1
MEMBLOCK_CANCEL_SNAPSHOT_OBSERVE_LATENCY =
    MEMBLOCK_DUT_CANCEL_OUTPUT_LATENCY + MEMBLOCK_TB_CANCEL_MONITOR_SAMPLE_OFFSET  // V2为3
```

`ENSBUFFER_WIDTH`、LQ/SQ size、redirect-to-LSQ latency、DUT output latency 和 monitor offset 是基础
参数；三个 count width 与 observe latency 只允许按表达式派生，不可独立覆盖。
`REDIRECT_TO_LSQ_LATENCY=1`表示顶层redirect被采样后，LSQ在下一拍把寄存后的redirect作为
`needCancel/enqCancel`采样条件；硬件output latency的2不是直接照抄
`StoreQueue` 注释，而是以顶层 `io_redirect` 被 DUT 采样的边界为 `T0` 后，对当前 V2 权威路径
逐级计数的结果：

```text
redirect driver 把值放到VIF：launch边界，不是T0；
redirect monitor在后续posedge看到顶层io_redirect_valid：T0，DUT顶层采样锚点；
MemBlock RegNextWithEnable(io.redirect)输出给LsqWrapper并由LSQ作为内部redirect采样：T0+1；
VirtualLoadQueue/StoreQueue redirectCancelCount更新并出现在顶层cancel output：T0+2；
之后无新redirect时，cancel output保持T0+2的结果，不自动回0。
```

ctrl monitor通过现有`mon_cb`的input sample读取稳定值：T0+2 callback在寄存器NBA更新前已经取样，
仍看到旧level；T0+3 callback才得到T0+2更新后的cancel值。因此DUT latency `2`只描述RTL更新边界，
实际target snapshot必须使用派生observation latency `3`。software cancel 的 DUT-visible cutoff 是
`redirect_lsq_sample_seq=T0+MEMBLOCK_DUT_REDIRECT_TO_LSQ_LATENCY=T0+1`：reservation sample早于该
cutoff属于已allocated entry，等于cutoff属于RTL `lastEnqCancel`的同拍enqueue，晚于cutoff不属于本次
redirect software cancel。所有时序参数都只允许与redirect monitor的顶层sample anchor配套使用，不能与
driver `item_done()`、`mark_redirect_drive_done()`、`apply_redirect_flush_range()` 或
`dispatch_service_cycle` 配套。V3 或其它 profile 必须按各自顶层路径覆盖该宏，不能在 sequence
中写死另一套延迟；monitor offset只有在clocking采样合同改变时才允许修改。上述参数只用于target
snapshot选择和有界queue深度，不参与free count计算。

`software_cancel_*_count` 同时表示软件必须回退的资源数和 DUT 在本次 redirect 应输出的 cancel 数。
该复用成立的前提是当前 clock-first real flow：launch 前 abort 不建立 allocation/token，launch 后必经
下一 sample 边界，同拍 enqueue 由 RTL `lastEnqCancel` 计数，redirect cutoff 后 launch 直接 fatal。
因此本 plan 删除独立 `expected_*_count` 及 software/expected equality check；若 reservation 不是
`DUT_VISIBLE` 或 sample 晚于 cutoff，不构造另一套 count，而是按时序断链处理。

`cancel_reconcile_q` 是 software/observed 对账和软件回退的唯一 record owner。redirect anchor 按
FIFO 绑定最老的 `redirect_anchor_valid=0` record，并要求 payload 与该 record 的 redirect payload
可观测投影 `{level,rob_key}` 完全一致；`flush_itself` 不在 V2 redirect agent interface 上，不能伪造
monitor compare。重复可观测 payload 也按 record id/FIFO 顺序绑定，不能用 payload-only map 合并。framework 的
`active_redirect` 仍保持 single-active，但已经完成状态 flush、仍等待 delayed output 的旧 record 可以与
下一笔 active redirect 的 record 并存。record queue 最大深度使用
`MEMBLOCK_CANCEL_SNAPSHOT_OBSERVE_LATENCY+2` 的 compile 派生常量；超过说明 service/redirect 生命周期失配，
立即 `uvm_fatal`，不新增 runtime backlog plus。现有
`pending_lq_cancel_count/pending_sq_cancel_count` 不能继续作为独立的聚合写者：允许保留为
兼容 debug 计数，但必须始终等于“尚未 `software_applied` 的 record
`software_cancel_*_count` 总和”，发生漂移
立即 `uvm_fatal`。同一 redirect epoch 只建立一个 record；`prepare_uid_for_redirect_reissue()`
只向该 record 累加一次，重复的 `(redirect_epoch, uid)` 必须被识别并 fatal。

`common_data_transaction`另保存`active_cancel_record_id_valid/active_cancel_record_id`，只指向当前
single-active redirect对应的record。`request_redirect_flush()`创建record时设置，
`mark_redirect_drive_done()`和`apply_redirect_flush_range()`直接用该id，不按可能重复的payload搜索；
state flush完成后清active id，但record可继续留在FIFO等待observed/software两个进度。

`dut_cancel_update_sample_seq = redirect_sample_seq + MEMBLOCK_DUT_CANCEL_OUTPUT_LATENCY`只用于日志和
RTL时序诊断；`compare_snapshot_sample_seq = redirect_sample_seq +
MEMBLOCK_CANCEL_SNAPSHOT_OBSERVE_LATENCY`才是实际compare target。只有该exact monitor sample sequence
的snapshot可以匹配本record；`deadline_sample_seq`取`compare_snapshot_sample_seq + 1`，只给monitor
snapshot queue一个完整采样和drain边界，不能
把晚一拍的 DUT 更新当作合法。`redirect_service_cycle` 只保留日志，不参与该公式。

record 生命周期如下：

1. recovery owner 调用 `request_redirect_flush()` 时创建 `cancel_record_id`，保存 payload/framework
   redirect epoch，但保持 `redirect_anchor_valid=0`；没有实际 DUT sample anchor 的 redirect 不能启动
   output latency deadline，也不能假定会产生 cancel output。`mark_redirect_drive_done()`把同一record
   标成drive-done并设置有界anchor watchdog；watchdog只检查monitor是否在预期后续posedge看到顶层valid。
2. redirect input monitor 报告 `{level,rob_key,redirect_sample_seq=T0}` 后，adapter 按 FIFO 绑定最老
   未锚定record并校验DUT可观测投影，设置`redirect_anchor_valid=1`、
   `redirect_lsq_sample_seq=T0+1`、`dut_cancel_update_sample_seq=T0+2`和
   `compare_snapshot_sample_seq=T0+3`。record中的`flush_itself`只保留framework原语义；本步骤只绑定
   时序，不扫描active表，不修改flush/recovery状态。
3. `advance_active_redirect()`不再在`redirect_drive_done_for()`后立即apply。它还必须确认record已绑定
   anchor、`peek_latest_dut_sample_seq()>=redirect_lsq_sample_seq`，且当前main service已经按固定顺序
   drain完该sample之前的cancel snapshot和semantic ctrl raw。条件不足时保持active redirect/freeze，
   下一service重试；因此尚未消费的`lqDeq/sqDeq`会先由原deq owner释放mapping，再进入cancel scan。
4. LSQ pending-sample completion在每次`finish_item()`返回后取得该driver边界的统一sample sequence，
   再按稳定`(uid,reservation_launch_epoch)`更新status中的
   `sample_valid/sample_seq/reservation_state`。即使flush epoch失效，也必须先把实际已sample的实例标成
   `DUT_VISIBLE`，再拒绝`complete_admission()`。missing、duplicate或dynamic instance token不匹配均
   `uvm_fatal`；它不需要record id，也不直接累计software cancel count。
5. readiness满足后，原`apply_redirect_flush_range()`才单点扫描active window，并调用
   `prepare_uid_for_redirect_reissue()`清理status/map，并登记同时用于rollback和DUT compare的
   `software_cancel_*_count`。此时任何
   `LAUNCHED_PENDING_SAMPLE`都表示driver/sample账本断链，立即`uvm_fatal`。`DUT_VISIBLE`且
   `sample_seq<redirect_lsq_sample_seq`归为allocated entry，等于cutoff归为same-cycle enqueue；大于
   cutoff表示flush gate后仍发生非法launch，立即fatal。每个UID/epoch只累计一次；scan完成后检查
   software count不超过对应LQ/SQ容量，再置`active_scan_done/software_count_finalized`并记录
   `state_flush_applied_service_cycle`。
6. record software count finalized 后，`apply_pending_lsq_cancels()` 按 record FIFO 消费尚未应用的
   `software_cancel_*_count`，调用 `cancel_lq/cancel_sq()` 一次并标记 `software_applied`；不再用一个聚合
   pending count 猜测 record 归属。record 在 observed 对账前保留。
7. ctrl monitor 的每拍 cancel snapshot 先进入 bounded raw snapshot queue；它可能先于 software count
   finalize，因此 monitor/adapter 不直接匹配、删除或修改 LSQ 状态。adapter drain后，本地 ring只保留
   尚可能被未锚定或未完成record使用的sample：没有任何record时，snapshot完成held-baseline检查后
   立即消费，不保留“最近N拍”；存在未锚定record时，最多保留最近
   `MEMBLOCK_CANCEL_SNAPSHOT_OBSERVE_LATENCY+2`个sample；所有已知target之前且已完成baseline/target检查的
   snapshot立即删除。raw queue或本地ring超过`2*MEMBLOCK_CANCEL_SNAPSHOT_OBSERVE_LATENCY+8`说明主service
   未及时drain或record未收敛，必须`uvm_fatal`。
8. 当 `sample_seq == T0 + MEMBLOCK_CANCEL_SNAPSHOT_OBSERVE_LATENCY` 时，按 FIFO 让 target sample 最老的
   anchored record 消费该 snapshot；更早或更晚的 level 变化都是时序 mismatch。下一 redirect 可以在
   前一 record 等待 delayed output 时建立自己的 record/anchor，只要 target sample sequence 严格递增且
   queue 未越界。连续 redirect 即使 observed count 完全相同，也分别在各自 target sample 比较一次；
   同一 sample 出现两个 record target、target 逆序或无法按 FIFO 归属时 `uvm_fatal`。匹配必须同时比较
   `snapshot.lq_count == software_cancel_lq_count` 和
   `snapshot.sq_count == software_cancel_sq_count`；成功后把两个 snapshot 值保存到 record 的
   `observed_lq_count/observed_sq_count`。
9. `software_cancel_lq/sq=0` 的 record 也必须取得 target sample 的 0/0 snapshot 后才能关闭；target snapshot
   缺失、count 不等、非target sample发生新level变化或snapshot乱序，均 `uvm_fatal`。
10. observed和software rollback是同一record内的两个独立进度。前一record若`observed_valid=1`但
   `software_applied=0`，不得阻塞后一record在自己的target sample比较；reconcile在有界record queue中
   查找最老`observed_valid=0`项，software apply独立查找最老`software_applied=0`项。cleanup只从FIFO
   队头连续删除`software_applied && observed_valid`的record，不能因其中一项完成而提前删除。所有 record、snapshot、
   anchor queue和本地snapshot ring收敛后，才允许 global-stop/end-test；单UID `terminal_done` 不由
   cancel compare回退或重写；end-test 不能
   用 `clear_raw_monitor_queues()` 静默抹掉未完成的 cancel 对账。

#### 文字伪代码

```text
seq_csr_common::check_compile_param_consistency() 的本专项增量：
  检查ENSBUFFER_WIDTH、LQ_SIZE、SQ_SIZE均大于0；
  检查SQ_DEQ_COUNT_W等于clog2(ENSBUFFER_WIDTH+1)；
  检查LQ/SQ_CANCEL_COUNT_W分别等于clog2(LQ/SQ_SIZE+1)；
  检查redirect-to-LSQ和cancel DUT update均大于0，当前clocking合同下monitor offset等于1；
  检查observe latency严格等于DUT update latency加monitor offset；
  任一不一致uvm_fatal；V2精确默认1/2/1只在compile header/profile定义，不在helper复制；
  函数只校验，不clamp、不修改compile值；

memblock_sync_pkg::get_dut_sample_seq(sample_time)：
  读取package内last_dut_sample_time、last_time_valid和dut_sample_seq；
  如果last_time_valid=0或sample_time大于last_dut_sample_time：
    dut_sample_seq递增一次；
    保存sample_time并置last_time_valid；
  如果sample_time等于last_dut_sample_time：
    返回同一个dut_sample_seq，不再次递增；
  如果sample_time小于last_dut_sample_time：
    uvm_fatal，禁止时间倒退导致跨sample误配；
  function不读service cycle，不修改任何status/record/LSQ状态；
  reset_all_tables调用clear_raw_monitor_queues()，后者调用reset_dut_sample_seq()清零上述状态；

memblock_sync_pkg::reset_dut_sample_seq()：
  dut_sample_seq=0；
  last_dut_sample_time=0；
  last_time_valid=0；
  只允许初始化/reset路径调用，不在redirect或普通service中重置；

memblock_sync_pkg::peek_latest_dut_sample_seq()：
  只读返回dut_sample_seq；
  不接收$time，不递增sequence，不修改last_dut_sample_time/last_time_valid；
  供negedge main service的readiness/deadline判断使用；
  非clocking-sample路径禁止调用递增型get_dut_sample_seq($time)；

ctrl monitor 每拍：
  如果rst_n、reset_backend_done或dispatch_monitor_capture_en无效，保持原XZ/reset行为且不写sideband；
  sample_seq = memblock_sync_pkg::get_dut_sample_seq($time)；
  sample lqCancelCnt/sqCancelCnt；
  做 X/Z 检查，并分别检查不超过MEMBLOCK_DUT_LQ_SIZE/MEMBLOCK_DUT_SQ_SIZE；
  snapshot = {sample_seq, lqCancelCnt, sqCancelCnt}；
  push_raw_cancel_snapshot(snapshot)，包括0/0；
  同拍semantic raw_ctrl继续只保存deq/memoryViolation/SBuffer状态；
  不向dispatch_raw_ctrl_t复制cancel count；

redirect input monitor 每拍：
  sample_seq = memblock_sync_pkg::get_dut_sample_seq($time)；
  如果rst_n、reset_backend_done和dispatch_monitor_capture_en均有效且io_redirect_valid：
    push_redirect_sample_anchor(payload, sample_seq)；
  不把该 anchor 当作新的 recovery event；

request_redirect_flush(redirect)：
  执行原有freeze/epoch/active_redirect状态更新；
  创建本epoch唯一cancel reconcile record，只保存redirect payload和framework epoch；
  把新record id写入active_cancel_record_id并置valid；
  record保持anchor无效、software count未finalize，不启动latency deadline；

collect_redirect_sample_anchors()：
  如果不存在未锚定record，uvm_fatal，禁止把无framework record的redirect输入静默丢弃；
  按FIFO取最老未锚定record；
  校验anchor的level/rob_key与record可观测投影完全一致；
  不要求anchor提供接口上不存在的flush_itself；
  写入redirect_sample_seq并计算LSQ cutoff、DUT update和monitor snapshot三个时点；
  计算redirect_lsq_sample_seq=redirect_sample_seq+REDIRECT_TO_LSQ_LATENCY；
  不扫描active表，不调用request/apply redirect helper；

mark_redirect_drive_done(payload)：
  执行原redirect drive done状态更新；
  通过active_cancel_record_id直接取得当前record，并校验payload；
  设置anchor_deadline_service_cycle=当前service cycle+2；
  该deadline只验证后续posedge monitor anchor，不作为T0或output target；

cancel_redirect_scan_ready(redirect)：
  找到当前active redirect的record；
  如果driver尚未done或anchor尚未绑定，返回0；
  latest_sample_seq=memblock_sync_pkg::peek_latest_dut_sample_seq()，只读不递增；
  如果latest_sample_seq或data.latest_drained_cancel_sample_seq小于record.redirect_lsq_sample_seq，返回0；
  要求本helper只在service_monitor_once已经drain snapshot并完成semantic/deferred ctrl raw后调用；
  满足全部条件返回1，不修改record/status/map；

advance_active_redirect()：
  保留原active redirect和freeze timeout检查；
  只有redirect_drive_done_for且cancel_redirect_scan_ready同时为1时调用apply_redirect_flush；
  条件不足时保持freeze并等待下一main service，不提前清active map；

common_data_transaction::begin_lsq_reservation_launch(uid)：
  要求uid对应status已经由commit_allocate建立active/enq和所需LQ/SQ mapping；
  要求本次activate_uid只完成key/map激活，snapshot_from_main没有清理任何reservation metadata；
  要求当前reservation state严格为NONE且sample_valid=0，防止覆盖尚未消费或未完成清理的实例；
  对status.lsq_reservation_launch_epoch单调加1，溢出或回到0时uvm_fatal；
  清sample_valid/sample_seq，把state置LAUNCHED_PENDING_SAMPLE；
  返回新的launch_epoch，不修改pointer、free count、issue-ready或redirect record；

confirm_lsq_candidates() 的真实V2 launch分支：
  request_launched=1时删除当前源码按global flush或epoch mismatch直接返回的旧分支；
  保持现有preview和commit_allocate单一allocation owner；
  每个commit_allocate成功后立即调用begin_lsq_reservation_launch(uid)取得动态实例token；
  将(uid,launch_epoch)按candidate顺序写入单深度pending token batch；
  继续把tr.flush_epoch写入独立pending batch flush epoch，不能用reservation launch epoch替代；
  request_launched=0的launch前abort分支不allocate、不建token；

common_data_transaction::mark_lsq_reservation_sampled(uid, launch_epoch, sample_seq)：
  要求status当前launch_epoch等于入参，state为LAUNCHED_PENDING_SAMPLE且sample_valid=0；
  任一条件不满足表示missing、duplicate、late callback或重发实例误匹配，uvm_fatal；
  写sample_seq/sample_valid并把state置DUT_VISIBLE；
  不读取flush epoch，不开放issue，不累计cancel count；

complete_v2_pending_sample()：
  如果pending token batch为空，直接返回；
  finish_item已在driver的drv_cb边界之后返回，只调用一次get_dut_sample_seq($time)取得本批sample_seq；
  对pending batch每个(uid,reservation_launch_epoch)调用mark_lsq_reservation_sampled，唯一匹配动态实例；
  所有token在判断flush epoch之前都必须先转成DUT_VISIBLE；
  保存的pending batch dispatch/flush epoch仍有效时继续原complete_admission，失效时不开放issue并等待统一redirect scan；
  清空pending token batch；不在本helper回退pointer/free count；

apply_redirect_flush_range(redirect)：
  找到当前 redirect epoch 对应的 cancel record；
  要求record anchor有效且peek_latest_dut_sample_seq不早于redirect_lsq_sample_seq，否则uvm_fatal；
  沿用原active-window和rob_need_flush扫描；
  对每个命中UID调用 `prepare_uid_for_redirect_reissue()`：该helper仍负责清status/map并按
    reservation sample sequence向record登记同一套software rollback/DUT compare count；
  如果看到LAUNCHED_PENDING_SAMPLE，说明pending-sample账本未在cutoff前收敛，uvm_fatal；
  sample早于redirect_lsq_sample_seq计allocated，等于该seq计same-cycle enqueue；
  sample晚于redirect_lsq_sample_seq表示flush gate时序错误，uvm_fatal；
  不把 launch前 abort计入；
  置active_scan_done；
  finalize前再次检查LQ/SQ software count均在对应compile容量范围内；
  立即置software_count_finalized；
  state flush完成后清active_cancel_record_id_valid，但不删除record；

prepare_uid_for_redirect_reissue(uid, redirect)：
  在retire_active_uid和clear_uid_dispatch_result之前，先快照had_lq_mapping、had_sq_mapping、
    numLsElem、reservation state/sample sequence和当前redirect epoch；
  如果status.lsq_cancel_accounted_epoch已经等于当前epoch：
    拒绝第二次向record累计，报告framework重复调用fatal；
  如果had_lq_mapping或had_sq_mapping：
    要求reservation state为DUT_VISIBLE且sample_valid=1；
    按sample sequence与redirect_lsq_sample_seq分类，并向当前record累计software_cancel；
    该count同时作为后续DUT observed compare值，晚于cutoff直接fatal；
    每次累计后检查LQ/SQ software count不超过对应compile容量，越界fatal；
  如果两个mapping都不存在：
    不累计任何cancel count，允许state已由真实deq owner清为NONE；
  写lsq_cancel_accounted_epoch并把reservation状态转成CANCEL_ACCOUNTED；
  再执行原retire/remove queue、清dispatch结果、dynamic_epoch递增和reissue状态更新；
  generic clear不得在record累计前擦除reservation metadata；clear只清state/sample，不清单调launch_epoch；
  下一次真实launch由begin_lsq_reservation_launch递增到新动态实例；

release_uid_lq_mapping()/release_uid_sq_mapping() 的reservation增量：
  保持原active map删除、status mapping清零和lsq_ctrl.release流程；
  当该UID的active_lq_mapped和active_sq_mapped都已经为0时，清reservation state/sample-valid/sample-seq；
  不清lsq_reservation_launch_epoch，不写cancel record，不改变rob_commit/terminal；

status_transaction::snapshot_from_main(main_tr) 与 status 初始化约束：
  snapshot_from_main只更新uid和ROB/LQ/SQ key，不更新任何reservation字段；
  activate_uid在首次launch和redirect reissue中均可调用snapshot_from_main；
  init_status_for_uid/status.reset只允许首次建表或reset_all_tables全局重置调用；
  redirect reissue、真实deq和普通retire不得通过status.reset清launch epoch；
  违反上述调用阶段或发现reissue前launch epoch倒退时uvm_fatal；

try_retire_committed_uid(uid) 的active redirect分支：
  如果active_redirect命中本uid，只返回等待统一redirect scan；
  不直接调用prepare_uid_for_redirect_reissue，不删除mapping，不累计cancel record；
  后续advance_active_redirect在T0_lsq之前的ctrl raw已drain后调用唯一apply_redirect_flush_range处理；

apply_pending_lsq_cancels()：
  从FIFO查找最老!software_applied的reconcile record；
  如果该record software_count_finalized=0，停止，本轮不修改pointer/free count；
  while 存在已经software_count_finalized且!software_applied的record：
    只调用该record对应的cancel_lq/cancel_sq一次；
    lq_free_count/sq_free_count增加该record software_cancel count；
    回退lq_enq_ptr/sq_enq_ptr；
    标记该record software_applied；
    finalized/applied之后禁止继续修改该record的software count；
  校验兼容 pending_lq/sq_cancel_count == 未应用record software_cancel总和；
  不读取或再次应用 DUT observed count；

dispatch_monitor_event_adapter::collect_cancel_snapshots()：
  while pop_raw_cancel_snapshot(snapshot)：
    data.push_cancel_snapshot(snapshot)；
    data.latest_drained_cancel_sample_seq=该snapshot.sample_seq，要求严格单调且不可倒退；
  不调用 release_lq/release_sq/cancel_lq/cancel_sq；

service_cancel_reconcile()：
  record本身已由request_redirect_flush按epoch入队；
  对所有redirect drive已done但仍未锚定的有界record检查anchor deadline；
  当前service已先drain anchor queue，超过deadline仍无anchor时uvm_fatal；
  在compile上界约束的record FIFO中查找最老observed_valid=0的record；
  只处理已经绑定anchor且software count已finalize的最老observation-pending record；
    current_sample_seq=data.latest_drained_cancel_sample_seq，只读已drain watermark；
    如果current_sample_seq < compare_snapshot_sample_seq：等待；
    若 snapshot.sample_seq == compare_snapshot_sample_seq：
      比较 snapshot.lq_count == record.software_cancel_lq_count；
      比较 snapshot.sq_count == record.software_cancel_sq_count；
      保存 record.observed_lq_count = snapshot.lq_count；
      保存 record.observed_sq_count = snapshot.sq_count；
    若 snapshot.sample_seq != compare_snapshot_sample_seq且发生新level变化：uvm_fatal；
    若不相等、snapshot缺失或snapshot乱序：uvm_fatal；
    若当前sample_seq超过deadline_sample_seq仍未比较：uvm_fatal；
    software cancel为0/0也必须在target sample明确比较0/0后才能关闭；
    observed匹配后置observed_valid，并把held-level baseline更新为本target的observed LQ/SQ值；
    即使本record software_applied=0，也继续检查下一个已ready的observation-pending record；
  从FIFO队头开始连续pop software_applied和observed_valid都为1的record；
  遇到任一进度未完成的队头即停止cleanup，但该项不阻塞后续record各自独立的observed/software service；
  如果当前没有任何record：
    对剩余snapshot逐拍完成held-baseline/非target变化检查后立即删除；
  如果仍有record：
    删除所有已早于最老可能target且已经检查过的snapshot，只保留后续仍可能匹配的有界窗口；
  只有reconcile record、redirect anchor、raw snapshot和本地snapshot ring都收敛时才允许global stop。
```

#### 失败与边界

- cancel count 不得进入 `apply_raw_ctrl_deq()`，不得设置 `status.lsq_deq`、`rob_commit`、pass 或
  terminal。
- 不支持的 vector/multi-element、AMO/CBO 或无法确认 DUT-visible reservation 的路径，不得用
  默认 `numLsElem=1` 伪造 software cancel count；沿用对应专项的 fail-fast 边界。
- `software_cancel != target snapshot`、target sample snapshot 缺失、没有 reconcile record 时 cancel level
  发生新变化、snapshot/anchor 乱序，均为当前 scalar redirect flow 的状态不一致，使用
  `uvm_fatal`，不能静默 drop 后继续驱动。已成功匹配后的 held level 不重复报错。
- anchor找不到未锚定record、可观测`level/rob_key`不匹配，或redirect drive done后的有界watchdog内仍未
  收到anchor，均`uvm_fatal`。anchor watchdog只暴露driver/monitor断链，不替代顶层T0，也不参与
  output latency公式。
- 本功能是测试框架状态账本与 DUT output 的时序一致性检查，不改变 pass/fail、功能 RM 或 coverage
  归属；不建立第二套 redirect/cancel 状态机。

#### Directed 验证分层

software-only sequence只能验证record、reservation token、software rollback和queue cleanup算法，不能
宣称验证DUT observed cancel。保留`soft_test_memblock_dispatch_fault_smoke_sequence`作为ledger单元测试，
但从其职责中删除“software count/DUT cancel count对账通过”的表述。该 software-only sequence 在
`commit_allocate()` 后显式调用 `begin_lsq_reservation_launch()`，再用测试自身的单调 synthetic sample
sequence 调用 `mark_lsq_reservation_sampled()`，只覆盖 token、allocated/same-cycle分类和 finalized
rollback；它不得调用 package 的 DUT clock sample getter、push monitor snapshot或设置 observed-valid。
普通无 redirect software smoke 不需要伪造 reservation sample metadata。

两层 directed 验证的覆盖边界如下，不得用其中一层的结果替代另一层：

| 验证层 | 输入与状态来源 | 本层覆盖 | 本层不覆盖 |
|---|---|---|---|
| software-only 账本测试 | 测试直接构造 framework record、reservation token 和 synthetic sample sequence | token 生命周期、allocated/same-cycle 分类、software rollback、record/queue cleanup | 真实 redirect driver、DUT redirect sample anchor、DUT `lqCancelCnt/sqCancelCnt` snapshot 和 software/observed 对账 |
| 真实 DUT cancel directed vseq | 真实 LSQ enqueue/issue、redirect driver、redirect monitor anchor、ctrl monitor cancel snapshot 和主 service | 年轻 scalar load/store victim 的非零 cancel、逐 epoch software/observed 对账、rollback、reissue 和终态收敛 | vector/multi-element、AMO/CBO、MMIO 专项组合及所有 redirect level 的穷举覆盖 |

新增真实DUT directed flow：

```text
memblock_main_dispatch_cancel_reconcile_sequence：
  继承现有manual real main sequence；
  只override build_directed_mixed_main_table()，复用继承的body/service/end-check；
  clear manual table后构造uid0较老load、uid1较年轻load、uid2较年轻store，ROB value固定0/1/2；
  uid0.delay=0；uid1/uid2.delay固定为32，使年轻victim在redirect注入前留在issue queue且不被发射；
  三笔使用互不冲突的合法地址，再调用import_manual_main_table()建立真实status；
  继续使用真实LSQ enqueue/issue/writeback/commit/deq和main service，不直接改status制造完成事件；

memblock_dispatch_real_cancel_reconcile_vseq::drive_directed_redirect_when_ready()：
  通过common_data_transaction::get()取得唯一公共状态；按main sequence相同config_db路径取得service vif；
  每个negedge先等main_trans_num=3且uid0..2 status已经建立，再读取两个固定victim，不扫描主表；
  最多等待256个service边界；超时、任一victim提前issue/writeback/deq/terminal或global stop均uvm_fatal；
  等到uid1 LQ和uid2 SQ mapping均active、sample_valid=1且reservation state为DUT_VISIBLE；
  用uid0 ROB key构造valid=1、level=flushAfter(0)、flush_itself=0的redirect，使uid1/uid2成为年轻victim；
  要求当前无active redirect/drive queue，否则fatal；
  调用既有request_redirect_flush()建立framework record和freeze；
  调用既有push_redirect_drive()让redirect agent真实驱动DUT；
  置本vseq局部redirect_injected=1并返回；
  不直接写anchor、cancel snapshot、software count、observed或free count；

memblock_dispatch_real_cancel_reconcile_vseq::new()/pre_body()/post_body()：
  继承memblock_dispatch_real_smoke_vseq以复用sequencer检查和responder构造；
  constructor调用set_automatic_phase_objection(1)，不依赖已知可能同拍结束的旧手工objection路径；
  pre_body要求starting_phase非null并设置drain time，但不再调用父类的手工raise；
  post_body保证dispatch_real_smoke_active清0；phase objection由UVM automatic机制成对释放；

memblock_dispatch_real_cancel_reconcile_vseq::start_background_responders()：
  background_responders_done先清0；
  调用已经迁移为uvm_do_on的父类helper，在真实dcache/sbuffer/redirect sequencer并发启动三个responder；
  三个responder在global stop且各自无inflight request/response/redirect后先驱安全idle再自然退出；
  父类helper的join返回后置background_responders_done=1；不kill sequence；

memblock_dispatch_real_cancel_reconcile_vseq::start_core_dispatch_flow()：
  override父类virtual task；声明既有lsqenq/issue/lsqcommit/L2TLB sequence和专用3-entry main sequence；
  在六个fork分支中分别使用uvm_do_on把四个agent sequence启动到对应p_sequencer agent handle、
    把main sequence启动到p_sequencer，同时执行drive_directed_redirect_when_ready()；
  所有六个分支自然结束后检查redirect_injected=1；
  检查cancel_reconcile_match_count非零，且LQ/SQ nonzero match计数都至少为1；
  任一检查失败uvm_fatal；main sequence的end_test_check继续要求三笔UID terminal、record/anchor/snapshot
    为空、active map为空和LQ/SQ free count恢复；

memblock_dispatch_real_cancel_reconcile_vseq::body()：
  执行父类相同的virtual/agent sequencer null检查和seq_csr_common初始化；
  置dispatch_real_smoke_active=1，后台启动override后的responder task；
  同步执行override后的start_core_dispatch_flow()，等待真实redirect、anchor、snapshot、reconcile和reissue闭环；
  core结束后最多再等256个service边界，要求background_responders_done=1；超时fatal；
  清dispatch_real_smoke_active并正常返回，automatic objection随后释放；

memblock_dispatch_real_smoke_vseq::start_background_responders()/start_core_dispatch_flow()：
  保持原sequence类型、并发fork/join、sequencer handle和退出条件；
  只把每个seq.start(p_sequencer.*)替换为uvm_do_on(seq,p_sequencer.*)；
  main sequence使用uvm_do_on(main_seq,p_sequencer)；
  不改变既有real smoke的payload、随机化约束、状态owner或终态条件；
```

上述 `delay=32` 只使用现有每 transaction issue `ready_cycle`，不是新增硬件参数、plus 或全局 issue
freeze；redirect/reissue 后同一 UID 重新入 issue queue时仍按既有逻辑递减并最终发射。256-cycle 只作为
directed 场景失败 watchdog，不是正常退出条件。`tc_dispatch_real_cancel_reconcile_smoke.cfg` 固定
`MEMBLOCK_ENQ_PER_CYCLE=3`、关闭 enqueue/issue pipe 随机，打开 LSQ enqueue/issue/commit/redirect/L2TLB，
并把 `MEMBLOCK_L2TLB_IDLE_STOP_CYCLE` 设为有界小值；不新增任何 cfg key。

```text
+MEMBLOCK_MAIN_TRANS_NUM=3
+MEMBLOCK_ENQ_PER_CYCLE=3
+MEMBLOCK_ENQ_PER_CYCLE_RAND_EN=0
+MEMBLOCK_LOAD_PIP_NUM_LIMIT=3
+MEMBLOCK_STA_PIP_NUM_LIMIT=2
+MEMBLOCK_STD_PIP_NUM_LIMIT=2
+MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN=0
+MEMBLOCK_STA_PIP_NUM_RANDOM_EN=0
+MEMBLOCK_STD_PIP_NUM_RANDOM_EN=0
+MEMBLOCK_LSQENQ_SEQ_EN=1
+MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=1
+MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN=0
+MEMBLOCK_LSQCOMMIT_SEQ_EN=1
+MEMBLOCK_REDIRECT_SEQ_EN=1
+MEMBLOCK_L2TLB_SEQ_EN=1
+MEMBLOCK_L2TLB_IDLE_STOP_CYCLE=128
+MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES=60000
```

其余 TLB permission/ready timeout 配置复用现有 `tc_dispatch_real_smoke.cfg` 的合法默认值；新 cfg 必须
显式写出这些继承值，因为 cfg 文件不支持 include。`MAIN_TRANS_NUM=3`只保持日志/配置自洽，实际三笔表
仍由专用 manual builder 唯一构造。

为让后台分支自然退出，本 plan 只给以下既有 responder loop 增加统一的退出分支，不改变 request/response
payload、延迟或主功能：

```text
dcache_mem__access_base_sequence / sbuffer_mem_access_base_sequence：
  每轮先完成已经接收的完整response；
  回到无inflight的loop顶部后，如果dispatch_real_smoke_active且global_stop_requested：
    驱动一笔安全idle并break；

memblock_redirect_dispatch_base_sequence：
  如果global stop、redirect drive queue/inflight为空且active_redirect无效：
    驱动一笔redirect idle并break；
  否则保持原queue drive和timeout逻辑；
```

这些退出分支只影响 real-smoke active 场景的 phase 收敛，不改变普通 agent default sequence；它们避免
新 vseq 依赖 phase 强杀无限 responder。已归档 enqueue plan 记录的旧 real-smoke vseq 同拍结束问题必须在
本场景 smoke 中以“仿真时间推进、三笔main table建立、至少一次真实redirect anchor”三个检查点证明已解决，
不能只看到 vseq start/completed 日志就判通过。

`common_data_transaction`增加只用于directed/debug的三个单调计数器：
`cancel_reconcile_match_count`、`cancel_reconcile_lq_nonzero_match_count`、
`cancel_reconcile_sq_nonzero_match_count`。它们只在target snapshot成功compare后递增，reset清零，不参与
software cancel计算、pass/fail、单UID terminal或global-stop gate。该真实flow必须经过redirect driver、redirect
monitor anchor、ctrl monitor snapshot和`service_monitor_once()`；禁止software sequence直接push snapshot
或置observed-valid伪造覆盖。

场景通过统一入口运行，不新增testcase类：

```text
make eda_run tc=basicTest ts=memblock_dispatch_real_cancel_reconcile_vseq \
  mode=base_fun cfg=tc_dispatch_real_cancel_reconcile_smoke
```

新增base/vseq必须同步`seq_pkg.sv`和`seq.f`，base sequence先include，vseq后include；`basicTest`只负责解析
`+VSEQ_MAIN`、按factory wrapper创建目标vseq并显式调用`start(env.vsqr)`，场景调度全部留在vseq中，
不得再通过`env.vsqr.main_phase.default_sequence`间接启动。

#### 主 service、reset 与 global-stop 单点合同

cancel reconcile 不能由 `memblock_lsqcommit_dispatch_base_sequence` 自己调度。真实主 owner 固定为
`memblock_main_dispatch_auto_build_main_table_base_sequence::service_monitor_once()`：每个 service
cycle 先 drain redirect sample anchor 和 cancel snapshot，再完成原 semantic batch/redirect recovery，
最后调用一次 `common_data_transaction::service_cancel_reconcile()`。lsqcommit/LSQ enqueue 子 sequence
只读取 `cancel_reconcile_pending()` 和应用自身负责的软件 cancel，不建立第二个 service loop。

`common_data_transaction::request_global_stop_if_done()` 必须从原来的单一
`transaction_done()` 条件收紧为：

```text
transaction_done()
&& !cancel_reconcile_pending()
&& !redirect_sample_anchor_pending()
&& !raw_cancel_snapshot_pending()
&& !cancel_snapshot_buffer_pending()
&& pending_lq_cancel_count == 0
&& pending_sq_cancel_count == 0
```

只有该函数仍拥有 `global_stop_requested=1` 的写权限。`all_transactions_terminal_done()` 保持调用
该函数，并保留现有 `service_real_dispatch_flow()` 在每拍 `service_monitor_once()` 和 issue route 之后
只调用一次 `all_transactions_terminal_done()` 的调度点。`service_monitor_once()` 不得直接再调用
`request_global_stop_if_done()`，其它 sequence 也不复制 gate。这样即使所有 UID 已经 terminal，主 monitor service 仍继续运行，直到
target snapshot 已采集、software/observed 已对账、软件 rollback 已应用。

reset/结束合同：

- `reset_all_tables()` 同时清 `cancel_reconcile_q`、snapshot ring、redirect anchor、record id、
  active cancel record id、reservation state、held-level baseline、`latest_drained_cancel_sample_seq`和
  sample-seq package状态。
- 正常退出前，最后一个 negedge `service_monitor_once()` 只完成 snapshot/anchor drain 和 reconcile；随后
  由同一外层service loop唯一调用`all_transactions_terminal_done()`执行global-stop gate；
  `end_test_check()` 再关闭 `dispatch_monitor_capture_en`并检查上述状态全部为空。
  任何残留使用 `uvm_fatal` 或至少 `uvm_error` 并保持失败，不能先调用
  `clear_raw_monitor_queues()` 后继续通过。
- snapshot queue 最大深度采用前述 compile latency 派生常量，不新增 runtime plus；溢出说明主
  service 停止 drain，立即 fatal。

文字伪代码：

```text
service_monitor_once()：
  tick_dispatch_service_cycle()；
  collect_runtime_context_events()；
  monitor_adapter.collect_cancel_snapshots()；
  monitor_adapter.collect_redirect_sample_anchors()；
  collect_monitor_event_batch()；
  exception_redirect_replay_task()；
  data.service_cancel_reconcile()；
  不调用request_global_stop_if_done，返回既有外层service loop；

service_real_dispatch_flow()：
  调用service_monitor_once和route_all_issue_queues；
  每拍只调用一次all_transactions_terminal_done()；
  由all_transactions_terminal_done内部调用request_global_stop_if_done；

request_global_stop_if_done()：
  如果 transaction_done且所有cancel record、anchor、raw snapshot、本地snapshot ring和
    software-pending状态为空：
    global_stop_requested=1；
  否则保持0，让主service继续drain；
```

## 7. 问题六：active idle 清零 level sideband；`has_progress` 保持轻量 activity 语义

### V2 问题

`pendingPtr/pendingst/pendingMMIOld` 是 level sideband。旧 driver 在 no-item/gap 周期调用通用
`drive_idle()`，可能把 sideband 清零。这会让未改变的 modeled ROB head 在接口上出现一拍或多拍
错误跳变，属于必须修复的 DUT 输入语义问题。

现有 `has_progress = has_commit || has_flushsb_progress || data.flushsb_busy()` 只用于辅助 debug 和
`idle_count`，不参与 pass/fail、`terminal_done`、ROB/LSQ pointer 或正常退出判定。这里的
`has_progress` 表示“本拍有 commit/新 flushSb，或仍有 flushSb outstanding work”，不是精确的
forward-progress checker。本轮接受该精度边界，不把 `flushsb_busy()` level 视为需要修复的功能问题。

### 修改原因

lsqcommit 是主动发射 flow。无 item/gap 不能破坏上一拍有效 head sideband，因此只修改 driver 的
active idle 行为。`flushsb_busy()` 在 `mark_flushsb_driven()` 后置 1，并在 ctrl raw 观察到
`sbIsEmpty=1`、调用 `update_sb_is_empty()` 后清 0；正常 flow 下它最终会清除。

如果 DUT、monitor 或 raw drain 异常导致 busy 不清，已有 `warn_flushsb_timeout_if_needed()` 负责一次性
warning，`flushsb_request_pending()` 继续阻止正常 global stop，最终由 UVM timeout 暴露。基于本项只做
辅助 activity debug 的定位，不为此增加 deq event sequence、pointer snapshot 或 edge helper。

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

`has_progress` 保持现有轻量实现，不新增以下状态或 helper：

```text
lq_deq_event_seq
sq_deq_event_seq
note_lq_deq_event()
note_sq_deq_event()
observe_lsqcommit_progress_edges()
```

真实 `lqDeq/sqDeq` 是否单独计入 `has_progress` 不影响功能状态更新；deq 仍由既有 raw handler 释放
mapping、推进 deq pointer 并尝试 terminal。本轮只接受 watchdog 精度较粗，不改变 deq owner。

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

send_lsqcommit_cycle(output has_progress, output terminal_idle_published)：
  build xaction，builder 首步同步 fault terminal；
  可选驱动 flushSb pulse；
  start_item/finish_item 完成 driver send_pkt/cache；
  如果有 normal batch，调用 mark_rob_commit_batch()；
  否则如果有 fault candidate，调用 mark_fault_rob_commit_uid()；
  has_progress = normal_commit_marked ||
                 flushsb_driven ||
                 data.flushsb_busy()；
  如果 issue 被 global flush 阻塞，沿用现有近似语义：
    has_progress = data.flushsb_request_pending()；
  terminal_idle_published 只有在本拍 item 已发送且 sideband 全 0、cursor 到表尾、
    waiting=0、modeled head invalid、flushSb 无 pending、cancel reconcile queue 和
    snapshot/anchor queue 均为空时置 1；
  fault token、fault rebase 和真实 deq 不要求单独计入该辅助值；
  重复 pendingPtr/pendingst/pendingMMIOld level 不计入 has_progress。

drive_lsqcommit_loop()：
  forever：
    先调用 send_lsqcommit_cycle()；
    有 progress 时 idle_count=0；
    无 progress 时 idle_count++；
    如果 no_progress_warn_cycles!=0 且 idle_count>=阈值：
      沿用现有行为报 uvm_warning；
      idle_count=0；
      不 break；
    本轮发送完成后才检查正常退出：
      global_stop_requested=1；
      terminal_done_uid >= main_trans_num；
      fault_head_waiting=0；
      commit_cursor_uid == main_trans_num；
      modeled_rob_deq_ptr_initialized=1；
      modeled_rob_deq_ptr_valid=0；
      terminal_idle_published=1；
      flushsb_request_pending()=0；
      cancel_reconcile_pending()=0；
    满足全部条件才 break。

has_progress 边界：
  该值只决定 idle_count 是否清零；
  不写 status、pointer、free count、pass/fail 或 terminal；
  flushsb_busy 卡住时 generic no-progress warning 可能被抑制，这是已接受的 debug 精度边界；
  flushSb 专项 timeout warning 和最终 UVM timeout 仍保留。
```

## 8. Coding 落点汇总

| 文件 | 对应问题与修改 |
|---|---|
| `mem_ut/ver/ut/memblock/cfg/memblock_compile_params.svh` | 问题四-B、问题五-A：本专项唯一新增`MEMBLOCK_DUT_ENSBUFFER_WIDTH`、SQ deq/cancel count派生宽度、redirect-to-LSQ=1、DUT update=2和monitor offset=1；observe latency=3只按表达式派生，不新增runtime plus |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv` | 问题四-B、问题五-A：暴露上述compile typed localparam、`memblock_lsq_reservation_token_t`、cancel reconcile/snapshot/anchor record；只引用宏不复制数值，record保存redirect payload、anchor-valid、software cancel/observed状态和compare target，不保留独立 `expected` count |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/status_transaction.sv` | 问题五-A：增加 framework-only reservation state、launch epoch、sample-valid/sample-sequence、cancel-accounted epoch；`snapshot_from_main()`保持只复制静态key，`reset()`仅由首次建表/全局重置使用；不改变 pass/fail/terminal 字段语义 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv` | 问题四-B、问题五-A：`check_compile_param_consistency()`检查主参数非零、派生宽度和`observe_latency=output_latency+monitor_offset`；V2默认值由compile profile提供，不在helper中复制2/1/3 |
| `mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv` | 问题四-B、问题五-A：`dispatch_raw_ctrl_t.sq_deq`与独立cancel snapshot使用派生宽度；新增clocking sample递增型`get_dut_sample_seq()`、negedge只读`peek_latest_dut_sample_seq()`及bounded snapshot/anchor queue；semantic raw ctrl不新增cancel字段 |
| `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_interface.sv` | 问题四-B、问题五-A：interface/clocking 中 `sqDeq`、`lqCancelCnt/sqCancelCnt` 使用 profile 派生宽度 |
| `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_xaction.sv` | 问题四-B、问题五-A：xaction、field automation/compare/print 中 `sqDeq` 和 cancel count 使用统一派生宽度；`sqDeq`范围用ENSBUFFER宏，LQ/SQ cancel范围分别用LQ/SQ容量宏，删除2/72/56 literal |
| `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_monitor.sv` | 问题四-B、问题五-A：monitor 临时变量和 X/Z 检查使用派生宽度；每个 post-reset sample push 0/非0 snapshot，不截断、不用count派生valid |
| `mem_ut/ver/ut/memblock/agent/redirect_agent_agent/src/redirect_agent_agent_monitor.sv` | 问题五-A：只上报顶层 redirect 实际 sample anchor/payload；不把 DUT input 反灌成 recovery event |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv` | 问题五-A：新增`begin_lsq_reservation_launch()`/`mark_lsq_reservation_sampled()`；`request_redirect_flush()`创建per-epoch record；原`apply_redirect_flush_range()`单点扫描并维护reservation/software-cancel；software count同时作为rollback和DUT compare值；绑定anchor、target-snapshot reconcile、reset/end check并收紧`request_global_stop_if_done()`；observed count不写LSQ状态 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/exception_redirect_replay_handler.sv` | 问题五-A：`advance_active_redirect()`在driver done后继续等待anchor、T0_lsq sample和ctrl drain readiness，再调用唯一state flush；不提前清active map |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv` | 问题一至五、问题五-A：commit/sqDeq 解耦、modeled head、normal/fault 分流、fault token 与 LQ/SQ deq 无序记录、full-raw owner、SQ count-only、`sqDeq` profile 最大值检查、cancel snapshot 不进入 deq、fault terminal sync |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_ctrl_model.sv` | 问题一、四、五-A：`sq_deq_ptr` 只由 reset/真实 `sqDeq` release 更新；`lq/sq_free_count` 由 reset/allocation/cancel/deq 更新；cancel state writer 不消费 DUT observed count；问题六不新增 deq event sequence/note helper |
| `mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqcommit_dispatch_base_sequence.sv` | 问题一、三、五-A、六：独立生成 scommit、build/send/loop、保留轻量 `has_progress` activity/watchdog、terminal idle 发布；不新增 progress snapshot，只查询 reconcile pending，不调度 reconcile service |
| `mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqenq_dispatch_base_sequence.sv` | 问题五-A：保持`commit_allocate()` void签名；把UID-only pending queue升级为`(uid,reservation_launch_epoch)`token queue，同时保留独立batch dispatch/flush epoch；真实launch后调用begin helper保存token，下一`finish_item()`返回后取一次sample-seq并逐token标记DUT_VISIBLE，再按batch flush epoch决定是否开放issue；rollback只消费software-count-finalized record，不消费DUT snapshot |
| `mem_ut/ver/ut/memblock/agent/lsqcommit_agent_agent/src/lsqcommit_agent_agent_driver.sv` | 问题六：active sideband cache、active idle hold |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv` | 问题四、五、五-A：full-raw forwarding、deferred ctrl consumer、snapshot/anchor单点drain和latest-drained watermark；不从raw ctrl重复生产snapshot |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_base_sequence.sv` | 问题五、五-A：semantic batch、deferred full raw 总编排；不在子 sequence 分散调 reconcile |
| `mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_auto_build_main_table_base_sequence.sv` | 问题五-A：`service_monitor_once()` 是 snapshot/anchor drain 和 reconcile 唯一 scheduler；global stop 前继续 service |
| `mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_dispatch_smoke_sequence.sv` | 问题一、四、五-A：normal smoke 检查 pendingst/scommit、独立 sqDeq 释放、V2 count-only SQ deq 和无 redirect cancel 的零基线 |
| `mem_ut/ver/ut/memblock/seq/base_seq/soft_test/soft_test_memblock_dispatch_fault_smoke_sequence.sv` | 问题三、五-A：software-only fault/record/rollback ledger单元测试；不得宣称覆盖DUT anchor/snapshot对账 |
| `mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_cancel_reconcile_sequence.sv` | 问题五-A：本轮已新增（执行前不存在）的真实DUT三笔manual main sequence，提供older anchor load和younger load/store victims |
| `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_cancel_reconcile_vseq.sv` | 问题五-A：本轮已新增（执行前不存在），实现automatic phase objection、background完成握手、core flow override、确定性victim barrier、真实redirect注入与完整agent/main-service对账flow |
| `mem_ut/ver/ut/memblock/seq/virtual_sequence/memblock_dispatch_real_smoke_vseq.sv` | 问题五-A：保持父类两个helper的sequence类型和并发行为，只把直接`.start()`迁移为项目强制的`uvm_do_on`；child background复用迁移后的helper，child core override也只使用`uvm_do_on` |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv` | 问题五-A：DCache/SBuffer responder在real-smoke active且global stop、无inflight时驱idle并自然退出；payload/延迟逻辑不变 |
| `mem_ut/ver/ut/memblock/seq/base_seq/memblock_redirect_dispatch_base_sequence.sv` | 问题五-A：redirect queue/inflight与active redirect收敛后按global stop驱idle并自然退出；原drive/timeout逻辑不变 |
| `mem_ut/ver/ut/memblock/seq/seq_pkg.sv`、`seq/seq.f` | 问题五-A：两个注册文件执行前已存在；本轮已按base先于vseq的依赖顺序注册上述两个新增sequence，没有新增散落testcase sequence |
| `mem_ut/ver/ut/memblock/seq/plus_cfg/tc_dispatch_real_cancel_reconcile_smoke.cfg` | 问题五-A：本轮已新增（执行前不存在）的basicTest+vseq场景preset，用于要求LQ/SQ nonzero successful match和最终全状态收敛 |
| `mem_ut/ver/ut/memblock/rule/memblock_parameter_management_rule.md`、`AI_DOC/project_management/mem_ut_parameter_management.md` | coding完成后补充ENSBUFFER/SQ-deq/cancel/latency宏均属编译期结构、directed cfg只复用既有runtime key，不新增plus镜像 |
| `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md` | coding完成后同步LSQCOMMIT说明：删除“pendingPtr推进到batch tail”和“V2未暴露scommit”的旧描述，改为modeled ROB head level sideband与真实normal scalar store scommit；明确本专项不新增/删除/迁移runtime plus，并在implementation review记录检查结果 |
| `AI_DOC/mem_ut_flow_doc/lsq_admission_flow.md`、`redirect_flow.md`、`rob_commit_lq_sq_deq_flow.md`、`virtual_sequence_unified_dispatch_flow.md` | coding完成后按真实实现同步token/sample、延迟scan、cancel对账、global-stop和vseq自然退出调用链；本轮只修改plan，不提前改flow |

明确不修改：

```text
make_empty_raw_ctrl() 的非cancel字段清零语义、ctrl interface sample_sq_deq_ptr() accessor、
ctrl monitor 对 MMIO tag 和 sq_deq_ptr_valid 的生产逻辑；`dispatch_raw_ctrl_t.sq_deq`、专用
cancel snapshot字段宽度按问题四-B/五-A修改，其余 raw 字段初始化和生产逻辑不变；
按当前 RTL 展开的 dut_inst.sv 固定端口声明、io_mem_to_ooo_ctrl_agent_connect.sv 同宽直连
pass/fail/fault/terminal_done 定义
RM、scoreboard、checker、coverage
其它专项 plan
```

## 与初步 plan 差异说明

| 修改项 | 类型 | 修改前逻辑 | 修改原因 | 修改后逻辑 |
|---|---|---|---|---|
| output 分类 | 范围收敛 | MMIO/status 字段职责混杂 | 防止 output 观察字段进入 pass/fail | 明确 FLOW_REQUIRED、driver control 和 pending-MMIO owner |
| `pendingPtr` 来源 | 功能逻辑修改 | 使用 batch tail 或 key+1 | V2 语义是当前 ROB deq head | modeled head 从 cursor UID 的权威 status 读取完整 ROB key |
| cursor/head owner | 状态生命周期修改 | selector 或 per-uid mark 可推进 cursor | 容易 head 与 cursor 分叉 | `rebase_framework_head_from_commit_cursor()` 是唯一同步 owner |
| normal commit | 功能逻辑修改 | fault 可混入 batch tail | fault 不是 DUT normal commit | normal batch 只含连续 normal candidate，全批成功后 rebase |
| fault convergence | 功能逻辑修改与顺序合同澄清 | fault token 置位后立即推进，且容易被误解为必须 deq 后才能记录 token | synthetic fault token 不是 normal commit；仅凭 token 越过会掩盖缺真实 LSQ deq，但 token 与 deq 本身没有先后要求 | `fault_head_seen` token 与真实 LQ/SQ deq 独立记录、任意顺序均合法；fault waiting 保持 head，只有完整 terminal tuple 后才框架 rebase |
| V2 SQ deq payload | 接口适配 | 读取默认 0 的不存在 pointer | V2 无 `sqDeqPtr` | capability 检查后走软件 SQ head 的 count-only 预检查与原子 release |
| `sqDeq` count 宽度 | 字段/编译期参数适配，合法运行期功能逻辑不变 | raw、ctrl agent 和 monitor X/Z 分别固定 `[1:0]` 或宽度 `2` | 当前 2 bit 只是 `EnsbufferWidth=2` 的派生结果；pointer presence 与 count width 相互独立 | `MEMBLOCK_DUT_ENSBUFFER_WIDTH` 是 profile 主参数，`MEMBLOCK_SQ_DEQ_COUNT_W` 按 RTL 公式唯一派生；raw/interface/xaction/monitor/XZ 全链消费同一宽度，`dut_inst` 保持 RTL 展开事实，release 和 terminal 逻辑不变 |
| xaction `sqDeq` 合法范围 | 字段约束适配，不改变 DUT observed flow | `default_io_mem_to_ooo_sqDeq_cons` 为空，随机 transaction 可生成超出 V2 `EnsbufferWidth` 的值 | xaction/default sequence 仍必须表达合法结构范围，即使 driver 不主动驱动该 output | 约束 `io_mem_to_ooo_sqDeq inside {[0:MEMBLOCK_DUT_ENSBUFFER_WIDTH]}`；只限制随机 transaction，不修改 raw 消费和终态逻辑 |
| `sqDeq` count 合法范围 | 失败策略新增 | packed 2-bit 值可编码 3，handler 没有按 `EnsbufferWidth=2` 拒绝该 RTL 不可能值 | 字段宽度只保证可表示 0..2，不能单独表达合法最大值 | handler 在 release 前检查 `count<=MEMBLOCK_DUT_ENSBUFFER_WIDTH`，越界 `uvm_fatal`；合法 0..max 的 pointer/count-only、status 和 terminal 流程不变 |
| LQ/SQ deq 与 ROB/fault token 顺序 | 功能逻辑修正与既有行为固化 | 现有 LQ/SQ pointer 路径本来按真实 deq 释放 mapping；V2 count-only 草案额外把 `status.rob_commit=1` 当作释放前置条件，fault 说明又可能被误读成 deq-before-token | deq 与 normal commit/fault token 是独立事件；V2 `sqDeqCnt` 按 allocated/completed 消费，MMIO/CBO 允许 deq 先于 ROB `commitValid/scommit`，fault token 与 deq 也允许任意顺序 | LQ pointer、SQ pointer 和 SQ count-only raw deq 都只校验 pointer/count、active owner 与动态实例，不等待 `rob_commit/fault_head_waiting/scommit`；`try_retire_committed_uid()` 仍以两类事件最终齐备门控 success/terminal，fault modeled head 只在完整 terminal 后 rebase |
| `scommit` 与 `sqDeq` 事件解耦 | 功能逻辑修正 | 可能把两个同名/近名计数当作同一拍、同一数量，或用 `scommit` 推进 `sq_deq_ptr`；normal batch 与 scalar 子集未区分 | 两者方向、生产阶段、延迟和计数单位不同；`sqDeq` 还可能晚于或早于 ROB commit | normal batch 全部写 `rob_commit`，`scommit` 只传 scalar 子集；raw deq 只释放 SQ mapping；`sq_deq_ptr` 只由 reset/真实 deq release 写，`sq_free_count` 独立按 reset/allocation/cancel/deq 更新；二者状态处理独立，不要求辅助 `has_progress` 精确统计每次 deq |
| SQ deq/cancel 编译期结构参数 | 字段/编译期参数适配 | ctrl字段链固定`[1:0]`、`[6:0]`、`[5:0]`，且未实现宏可能被归到已归档compile plan | count宽度来自不同硬件容量，pending功能不能写入`do` plan形成第二owner | 本`undo` plan唯一在compile header新增ENSBUFFER主宏、SQ deq/LQ cancel/SQ cancel派生宽度和redirect/cancel时序宏；typed localparam和全链consumer只读同源，不新增runtime plus |
| cancel count 合法范围 | 字段约束与失败策略适配 | xaction约束写死`7'd72/6'd56`，monitor/record累计缺少统一容量防御检查 | 参数化位宽仍可能表示容量外数值，literal范围会在版本切换后与LQ/SQ size分叉 | xaction、monitor observed、software count累计和finalize均使用`MEMBLOCK_DUT_LQ_SIZE/SQ_SIZE`检查；越界fatal，不修改合法cancel、free-count或terminal流程 |
| cancel compare count owner | 方案简化，功能边界不变 | 初步方案分别累计 `expected_cancel_*` 与 `software_cancel_*`，先要求二者相等，再用 expected 对比 DUT | 当前 clock-first 合同已保证 launch 前不分配、launch 后必采样、同拍由 `lastEnqCancel` 统计，合法 flow 中两套 count 必须相等 | 删除独立 expected 字段、累计和 equality check；每个 record 只 finalize 一套 `software_cancel_*`，既用于一次性 `cancel_lq/cancel_sq`，也在 compare target sample 与 DUT observed 直接比较 |
| reservation token/sample API 与状态初始化 | 功能逻辑新增，allocation主体不变 | `commit_allocate()`后pending batch只有UID queue和batch flush epoch，无法区分redirect后同UID重发实例；sample完成没有统一stamp写者；若reissue复用`status.reset()`会使旧token重新命中新实例 | UID会复用，晚callback可能污染新实例；真实sample事实必须先于flush epoch判断；reservation launch epoch必须跨reissue和deq单调保留，且不能替代batch flush gate | 保持`commit_allocate()` void签名；UID queue升级为`(uid,reservation_launch_epoch)`token queue，原batch dispatch/flush epoch独立保留；真实launch在内部activate后调用begin取得token，下一`finish_item()`返回后先统一mark sample，再按batch flush epoch决定issue；`snapshot_from_main()`只复制静态key，`status.reset()`仅用于首次建表/全表reset；redirect clear和真实deq只清visibility state/sample并保留launch epoch，token/state mismatch fatal |
| redirect scan时点、软件cancel与pending-sample分类 | 功能逻辑新增/修正 | driver done后可立即apply，早于顶层anchor/LSQ内部redirect采样；`try_retire_committed_uid()`还可旁路统一scan直接prepare | 早扫会把尚未消费的deq mapping多算；software count必须在internal T0后读取稳定reservation，且同一UID不能被两个入口累计 | `advance_active_redirect()`等待anchor、T0_lsq sample和ctrl raw drain；try-retire命中redirect只defer；唯一active scan要求token均为DUT_VISIBLE，sample早于/等于cutoff分别计allocated/same-cycle，晚于cutoff fatal；只finalize一套software count并直接用于rollback和DUT compare |
| held cancel output 的 observation valid | 功能逻辑新增 | monitor只采样/XZ；草案曾按nonzero或semantic ctrl raw生成event | `redirectCancelCount` 无valid且由`RegEnable`保持，nonzero会重复、semantic-event gate会漏target、value-change会漏相同count | ctrl monitor每拍生成0/非0独立snapshot；semantic raw ctrl不增加cancel字段；reconcile只在compare target sample取一次，已匹配held level不重复消费；非target变化fatal |
| 统一 DUT sample sequence | 同步helper新增 | ctrl、redirect和LSQ pending-sample分别使用`$time`或service cycle，negedge reconcile又缺少只读当前值 | clocking sample必须同posedge同序号，非sample路径不能因查询而额外递增 | `get_dut_sample_seq($time)`只供clocking sample递增/复用；`peek_latest_dut_sample_seq()`和latest-drained watermark供negedge readiness/deadline只读；reset helper只在table reset清零，时间倒退fatal |
| cancel latency 与 monitor observation 时间域 | 编译期时序参数与功能逻辑新增 | software/DUT compare使用service cycle、直接套StoreQueue内部T0/T2，或假设T0+2 monitor callback已经看到NBA新值 | 顶层还有`RegNextWithEnable(io.redirect)`，driver launch不等于DUT sample；现有clocking input又比RTL更新晚一个callback观察到新level | redirect monitor提供顶层T0；DUT update latency=2，monitor sample offset=1，唯一派生observation latency=3；实际compare target snapshot=`T0+3`，`T0+2`只作RTL诊断，service cycle仅用于日志/watchdog |
| cancel record、anchor、软件回退与 DUT 对账 | 功能逻辑新增/对账闭环，free-count主体公式不变 | 软件聚合pending count会回退free count，但无per-epoch record；DUT output未对账 | 多个record无法从聚合count反查`software_applied`，monitor创建record会形成第二redirect状态机，observed再次写状态会双重回退 | `request_redirect_flush()`创建per-epoch record；anchor按FIFO比较接口可观测`level/rob_key`，不伪造`flush_itself`；原`apply_redirect_flush_range()`仍唯一扫描/清状态；有界多record支持连续redirect；software/observed用独立进度，互不阻塞，cleanup仅删除两者都完成的队头项 |
| cancel reconcile 调度、snapshot ring与全局退出 | 退出逻辑修改 | `request_global_stop_if_done()`只看terminal；若由lsqcommit子sequence调reconcile，主monitor可能提前停止；“永远保留最近N拍”会使ring永不为空 | UID terminal不代表延迟cancel output已采样；停止主service会遗留raw/record或假通过，常驻历史snapshot又会永久阻塞退出 | 主`service_monitor_once()`单点drain/reconcile但不直接请求stop；ring只保留未完成record可能使用的有界sample，无record时baseline检查后立即消费；既有外层每拍唯一`all_transactions_terminal_done()`调用收紧后的global-stop helper，并增加record/anchor/raw/local snapshot/software-pending gate |
| cancel directed验证 | 验证flow新增 | software-only fault smoke直接改公共状态；既有`basicTest + real_smoke_vseq`还记录过同拍结束，且mapping出现后victim可能已issue；父vseq直接`.start()`不符合项目启动规则 | ledger单元测试不能证明DUT链路，非确定性时点也不能稳定产生LQ/SQ非零cancel，且新vseq必须统一由virtual sequencer调度 | software-only只测账本；父helper和新child core均使用`uvm_do_on`；新vseq使用automatic phase objection、3-entry main和victim delay，等待DUT_VISIBLE后注入flushAfter redirect，要求真实anchor/snapshot、LQ/SQ非零match、reissue和三UID终态收敛 |
| real-smoke responder退出 | 运行期退出逻辑修改，payload不变 | DCache/SBuffer/redirect responder无限loop，vseq body结束依赖phase终止 | directed vseq必须自然等待后台分支，不能靠强杀证明通过 | 仅在`dispatch_real_smoke_active && global_stop`且无inflight时驱安全idle并break；vseq等待background done，256-cycle仅作失败watchdog |
| ctrl raw 应用顺序 | 功能逻辑修改 | deq 可能早于同批 event claim | 会删除同批 active map | semantic batch 后再 deferred full-raw apply |
| driver idle | 时序逻辑修改 | active no-item/gap 调通用 idle 清 sideband | level sideband 不能被气泡清零 | active idle 重驱 cache，只清 pulse |
| `has_progress` activity 统计 | 辅助 debug 逻辑保持 | `has_commit || has_flushsb_progress || flushsb_busy()`；busy level 会持续清 idle_count，真实 deq 不单独统计 | 用户确认该值只需粗略表示 flow 有活动或 outstanding work，不承担精确 forward-progress 检查 | 保持现有轻量公式，不新增 deq event sequence、pointer snapshot 或 edge helper；不进入 pass/fail/terminal，flushSb 卡住由专项 timeout warning 和最终 UVM timeout 暴露 |
| no-progress | 辅助 debug 逻辑保持 | 达到阈值后报 `uvm_warning` 并清 `idle_count`，继续运行 | 该机制只提供周期性诊断，不作为功能失败判定或正常退出依据 | 保持现有 warning/reset 行为；正常退出继续使用独立 terminal/global-stop/pending 条件 |

关键函数差异：`begin_lsq_reservation_launch()`输入UID、输出单调token并只写reservation metadata；
`mark_lsq_reservation_sampled()`输入UID/token/sample-seq、输出`DUT_VISIBLE`事实；
`advance_active_redirect()`修改前只等driver done，修改后还等anchor、T0_lsq和ctrl drain；
`apply_redirect_flush_range()`修改前只清active状态并累计聚合cancel，修改后仍是唯一扫描，同时按token
sample分类并finalize per-record software count；该count同时作为rollback和DUT compare值；`apply_pending_lsq_cancels()`修改前消费聚合count，
修改后只消费finalized record；`service_cancel_reconcile()`是新增的bounded事件路径，只比较target snapshot；
新vseq的phase/background/core override分别负责objection、responder自然退出和确定性真实DUT场景。各函数
输入、输出、副作用、return/fatal分支均已在问题五-A和Directed验证章节给出文字伪代码。

保持不变的主体逻辑：main table/status 基本字段、`try_retire_committed_uid()` 中对
`rob_commit` 的最终 retire 门控、
`consume_fault_retire()` 的 terminal 形成、LQ pointer deq 主体、flushSb request owner、redirect/replay
handler、pass/fail 语义。

### 审稿用四要素与差异影响

```text
修改目的：
  正确驱动V2 ROB-head sideband，解耦ROB commit与LSQ physical deq，并把redirect软件回退与DUT cancel输出闭环。
修改前逻辑行为：
  pendingPtr可能来自batch tail；fault混入normal commit；V2缺失sqDeqPtr仍走pointer路径；
  cancel只按软件聚合回退，没有动态instance token、顶层sample anchor或DUT output对账。
修改后逻辑行为：
  modeled head只从cursor UID权威status重建；normal/fault分流；fault token与真实LQ/SQ deq任意顺序独立落表，
  只在完整fault terminal后rebase；V2 sqDeq走count-only原子release；
  launch token和统一sample sequence确定DUT-visible集合，每个redirect record在T0+3比较held snapshot，
  software rollback与observed进度独立收敛后才允许global stop。
差异影响：
  改变pending sideband、fault convergence、SQ count-only、redirect cancel对账、active idle和退出收敛时序；
  has_progress/no-progress保持轻量辅助debug语义，不新增精确事件统计；
  不改变合法pass/fail定义、真实deq的pointer/free-count所有权或RM/checker/coverage职责。
```

### 新增/修改 Helper 详细伪代码

```text
rebase_framework_head_from_commit_cursor() / resolve_sideband_head_uid()：
  添加原因：pendingPtr不能由batch tail或ROB value算术推导。
  前者跳过连续terminal前缀并从cursor status复制完整ROB key；后者用该key查询active map并返回当前head UID；
  只写handler私有modeled-head/cursor状态，不写LSQ pointer/free count；非法key、stale map或fault等待冲突fatal。

select_fault_head_candidate() / mark_fault_rob_commit_uid() / sync_modeled_head_after_fault_terminal()：
  添加原因：fault head不能进入normal commit batch，也不能仅凭synthetic fault token跨过；这不表示LSQ deq必须先于token。
  selector只读cursor/head且不要求lsq_deq；mark只给同一动态实例置fault_head_seen token和waiting且不要求mapping已释放；
  sync等待token、fault状态和mapping release最终全部收敛后推进cursor；token先、deq先或同批到达均合法；
  redirect实例变化时清旧waiting但不越过UID，任一身份或tuple矛盾fatal。

apply_dut_lq_deq() / apply_dut_sq_deq() / apply_dut_sq_deq_count_only(count) / apply_raw_ctrl_deq(raw)：
  添加原因：V2只有sqDeq count，没有sqDeqPtr。
  wrapper按capability选择pointer/count-only；count-only先验证连续SQ head均有唯一active owner，再原子release并删mapping；
  所有LQ/SQ deq路径均不要求rob_commit/fault_head_waiting，不消费scommit；现有pointer路径行为保持不变；
  预检查失败不释放前缀；合法release后只更新既有`status.lsq_deq`并调用
  `try_retire_committed_uid()`，不新增或递增任何deq event sequence。

begin_lsq_reservation_launch(uid) / mark_lsq_reservation_sampled(uid,token,sample_seq)：
  添加原因：同UID redirect reissue后，UID-only pending callback可能污染新实例。
  begin要求active mapping和NONE state，递增单调token并置pending-sample；mark校验token/state后置DUT_VISIBLE；
  两者不改pointer/free count，batch dispatch/flush epoch独立决定后续是否complete_admission。

release_uid_lq_mapping()/release_uid_sq_mapping() reservation增量：
  修改前只删mapping；修改后在最后一个mapping删除时清visibility state/sample但保留launch epoch；
  不登记cancel、不清token、不修改rob_commit/terminal，deq-before-commit后不会被redirect重复计数。

get_dut_sample_seq(time) / peek_latest_dut_sample_seq()：
  添加原因：ctrl、redirect和LSQ sample不能混用service cycle或各自时间轴。
  get在同一仿真时刻复用序号、新时刻递增；peek只读不递增；时间倒退fatal；仅全表reset清零package状态。

cancel_redirect_scan_ready(record) / apply_redirect_flush_range(redirect)：
  修改原因：driver done不等于顶层/LSQ已经采样redirect。
  ready只读drive-done、anchor、T0_lsq和drain watermark；满足后唯一scan按sample早于/等于cutoff累计；
  晚于cutoff、pending-sample、重复UID/epoch或容量越界fatal；scan完成只finalize software count，不消费observed。

collect_cancel_snapshots()/collect_redirect_sample_anchors()/service_cancel_reconcile()：
  添加原因：held output无valid，且monitor不能创建第二redirect状态机。
  collector单点drain逐拍snapshot和FIFO anchor；reconcile只在每条record exact T0+3 snapshot比较software/observed；
  零count也比较，非target变化/缺失/乱序fatal；只写record/debug进度，不写status、pointer或free count。

apply_pending_lsq_cancels() / request_global_stop_if_done()：
  修改前消费聚合count且terminal即可stop；修改后前者只消费software_count_finalized record并各调用一次cancel_lq/sq；
  后者仅由all_transactions_terminal_done单点调用，并等待record、anchor、raw/ring和software pending全空；
  observed匹配不再次回退资源，service_monitor_once不重复调用global-stop helper。

cache_active_sideband()/drive_active_idle()：
  添加原因：气泡不能清level sideband；这是DUT输入语义修复，与辅助has_progress精度无关。
  send后缓存pendingPtr/pendingst/pendingMMIOld；active idle重驱level且只清scommit/flushSb pulse；reset清cache。
  has_progress继续使用has_commit、新flushSb和flushsb_busy的轻量公式；不新增deq event sequence、snapshot或edge helper，
  no-progress达到阈值后继续按现有逻辑warning并清idle_count。

memblock_dispatch_real_cancel_reconcile_vseq的background/core/redirect tasks：
  添加原因：software-only账本不能证明真实DUT cancel链路。
  automatic objection下并发真实responder和五个core sequence；所有sequence通过uvm_do_on启动；
  等uid1/uid2 DUT_VISIBLE后驱flushAfter redirect，等待anchor/snapshot/reissue和三UID终态；
  responder在global stop且无inflight时安全idle后自然退出，256-cycle仅作失败watchdog。
```

## 执行中补充/修正（IMPLEMENTATION_DELTA）

### [IMPLEMENTATION_DELTA] 最后 normal commit batch 后的 `pendingPtr` watermark

- **来源**：专项真实仿真中，UID1 load 与 UID2 store 同拍 normal commit 后，软件
  `commit_cursor_uid` 已到 `main_trans_num`，原实现把 `modeled_head_valid` 和
  `pendingPtr` 一起清零；V2 `StoreQueue` 的 `committed` 仍要求
  `uop.robIdx <= GatedRegNext(io.rob.pendingPtr)`，因此 UID2 没有进入 committed、
  request、completed 和 `sqDeq` 链路。
- **原 plan/原实现**：`pendingPtr` 只在 `modeled_head_valid` 时发布；没有下一条 modeled head
  时驱动零 key。该做法保持了“不能用 key+1 推导新 head”的约束，但遗漏了最后一个已提交
  store 仍需要一个 DUT 可见的 ROB watermark 的场景。
- **实现调整**：`lsq_commit_handler` 增加 `committed_rob_watermark` 和
  `committed_rob_watermark_valid`。每次 normal commit batch 全部成功后，保存该 batch 最后 UID
  的权威完整 `robIdx(flag/value)`；若 rebase 后 `modeled_head_valid=1`，无论 active map 当前是否命中，
  `pendingPtr` 都无条件发布 modeled head。active-map lookup 只决定 `pendingst/pendingMMIOld`，不得决定
  `pendingPtr`；若 cursor 已到表尾，则保持
  `modeled_head_valid=0`，但在 active idle 和 terminal drain 周期继续发布保存的 watermark。
- **边界**：watermark 不是新的 ROB head，不触发任何 `pending*` pulse/level，不推进
  `commit_cursor`、SQ/LQ pointer、free count、pass/fail 或 terminal；它只覆盖已知已提交 batch
  的 StoreQueue ROB 比较门槛。完整 `flag/value` 直接来自 status，禁止 `rob key + 1`，因此不引入
  manual 主表 key 不连续或 wrap 的算术假设。terminal idle 判断只检查 pulse 和未收敛队列，不能
  再要求 `pendingPtr` 为零。

中文伪代码：

```text
mark_rob_commit_batch(uids)：
  先按原流程预检查并逐 UID 标记 status.rob_commit；
  读取 uids 最后一项的完整 rob key，保存为 committed_rob_watermark；
  commit_cursor 推进到 batch 尾后的 UID；
  rebase：cursor未到表尾时从对应status重建下一条权威modeled head，否则只清modeled_head_valid并保留watermark。

clear_lsqcommit_xaction(tr)：
  如果 modeled_head_valid=1，无条件让pendingPtr发布modeled_rob_deq_ptr；
  否则如果final watermark满足publishable条件，pendingPtr发布最后已提交batch tail；
  否则发布零 key；
  active-map lookup不参与上述pointer选择，只在build阶段决定pendingst/pendingMMIOld；
  scommit/flushSb 仍按原 pulse 逻辑清零。

terminal_idle 判断：
  只要求 commit cursor、modeled head、fault、cancel/raw/anchor 和 pulse 字段收敛；
  允许 pendingPtr 保持 watermark，因为它不代表仍有 active ROB head。
```

### [IMPLEMENTATION_DELTA] active idle 与 configured idle 路径分离

- **来源**：pending-MMIO directed 仿真收尾 review 发现，初版 driver 虽然已经缓存
  `pendingPtr/pendingst/pendingMMIOld`，但 main-phase 的 no-item、pre-gap 和 post-gap 仍调用
  `drive_idle(cfg.drv_mode)`；只有 `DRV_0` 分支重驱 cache，`DRV_1/DRV_X/DRV_RAND/DRV_LST`
  会改写或遗漏 active level。
- **原 plan 要求**：问题六要求 active 气泡与 configured/reset idle 分离；active idle 必须无条件保持
  最近一次 level，只清 `scommit/flushSb` pulse，reset 清 cache。
- **实现调整**：新增 `drive_active_idle()`。main-phase 三类 active 气泡统一调用该 task，不再读取
  `cfg.drv_mode`；`reset_phase()` 和其它 configured idle 场景继续调用原 `drive_idle(cfg.drv_mode)`。
  `send_pkt()` 驱动 transaction 后缓存三项 level，`reset_phase()` 同时清 valid 和全部 cache payload。
- **调用关系**：`main_phase()` 在 pre-gap、post-gap、no-item 分支调用 `drive_active_idle()`；有 item
  的有效拍调用 `send_pkt()` 更新 VIF 和 cache；`reset_phase()` 只调用 `drive_idle()` 并清 cache。
- **模式边界**：active 路径不再进入 `DRV_0/DRV_1/DRV_X/DRV_RAND/DRV_LST` 分支，因此 X、随机值、
  全 1 或 legacy idle 都不会污染 active level；configured/reset idle 的原模式行为不变。本修复不新增
  transaction、随机约束、状态机、timeout 或结束条件。
- **影响范围**：
  `mem_ut/ver/ut/memblock/agent/lsqcommit_agent_agent/src/lsqcommit_agent_agent_driver.sv`。

### [IMPLEMENTATION_DELTA] 统一 runtime drain 后再请求 global stop

抽象功能描述：`common_data_transaction::runtime_drain_complete()` 在主动 flow 已形成连续
`terminal_done` 前缀后，统一判断所有运行期 producer、queue 和 recovery 控制是否收敛。它只读取
queue size、associative map count、pending bit 和 phase，不扫描 `main_table_by_uid/status_by_uid`。

- **来源**：原 `request_global_stop_if_done()` 只等待 cancel record、anchor、snapshot 和 raw timing
  sideband，可能在 monitor raw、exception、issue、redirect、PTW replay 或 flushSb 尚未消费时提前停止。
- **实现调整**：新增 `runtime_drain_complete()`，覆盖 `raw_monitor_queue_size()`、`exception_event_q`、
  load/STA/STD issue queue、active ROB/LQ/SQ map、redirect pending/inflight/control、
  `ptw_wait_replay_q`、flushSb request/active/waiting 状态，以及 cancel record、software apply、
  pending count、anchor、local snapshot 和 package raw timing queue。`request_global_stop_if_done()` 只在
  `transaction_done() && runtime_drain_complete()` 时置位 stop；`end_test_check()` 复用同一 predicate 做
  低成本终态自检。
- **性能边界**：`transaction_done()` 只推进连续 terminal prefix；runtime predicate 不做每拍主表扫描。
  cancel apply query 最多扫描 compile-bound record FIFO，其他条件均为 O(1) size/count/bit 读取。

中文伪代码：

```text
request_global_stop_if_done()：
  推进并检查terminal_done连续前缀；
  如果尚未覆盖main_trans_num，保持global_stop_requested=0；
  如果任一raw/event/issue/map/redirect/PTW/flushSb/cancel/timing状态未drain，保持为0；
  只有transaction完成且统一runtime drain返回1时，置global_stop_requested=1。
```

### [IMPLEMENTATION_DELTA] 未锚定 cancel record 保留 snapshot

抽象功能描述：`common_data_transaction::service_cancel_reconcile()` 在 anchor 未到达时保留本地
snapshot，使后续绑定得到的 record 仍能按精确 sample sequence 对账；只有 record FIFO 为空时才执行
held-level baseline 消费。

- **来源**：当前实现用“找不到 anchored observation-pending record”直接进入 baseline 分支，混淆了
  “完全无 record”和“已有 record 但最老项尚未绑定 anchor”。后者会提前 pop 未来 target snapshot。
- **实现调整**：完成 anchor bind 和既有 anchor deadline 检查后，若找不到 anchored pending record 但
  `cancel_record_q` 非空，立即停止 snapshot loop并保留队首；若 record FIFO 为空，才做 baseline check并
  pop。record 已锚定后仍只消费 `compare_snapshot_sample_seq` 的 exact sample，早于 target 的 snapshot
  按 baseline 检查，晚于 target、target 缺失、count 不匹配和 deadline 超期继续 `uvm_fatal`。

### [IMPLEMENTATION_DELTA] full ctrl raw 的 SQ capability 最终分支

抽象功能描述：`lsq_commit_handler::apply_raw_ctrl_deq()` 对同一 full ctrl raw 的 LQ/SQ owner 先做联合
预检，再原子提交两侧 release。SQ 预检按 compile capability 显式选择 pointer 或 V2 count-only 语义，
但 capability 不参与 `sq_deq` 字段宽度计算。

- **原 plan**：要求 full raw 继续沿用 pointer/count-only wrapper，但没有明确 wrapper 会立即提交，因而
  不能直接用于同一 raw 的 LQ/SQ 联合原子预检。
- **实现调整**：提取共享的 SQ start-key/owner 预检核心，并增加独立
  `preflight_dut_sq_deq_count_only()`。当 `MEMBLOCK_DUT_HAS_SQ_DEQ_PTR=1` 时，从 raw pointer 计算 start key；
  当其为 0 时，显式忽略 raw pointer payload并从软件 `sq_deq_ptr` 取得起点。两侧预检全部成功后才依次
  `commit_dut_lq_deq()`、`commit_dut_sq_deq()`；任一失败都不允许另一侧部分 release。
- **宽度边界**：`raw.sq_deq` 始终使用 `MEMBLOCK_SQ_DEQ_COUNT_W`，并继续检查
  `count <= MEMBLOCK_DUT_ENSBUFFER_WIDTH`。`MEMBLOCK_DUT_HAS_SQ_DEQ_PTR` 只选择 pointer presence、validity
  和语义分支，不能作为 count width。

### [IMPLEMENTATION_DELTA] directed sequence/testcase 文件范围与实际实现对齐

- **来源**：coding 过程已按问题五-A实现真实 DUT cancel directed flow，但原 plan 文末仍保留
  “本批次写权限不包含 directed sequence/testcase，本轮不修改”的限制，与正文、Coding 落点
  和已落地源码矛盾。
- **原 plan/原限制**：问题五-A正文规划新增三笔 manual main sequence、真实 DUT cancel vseq 和专用
  cfg；但文末又将所有 directed sequence/testcase 排除在本批次写范围外，只允许记录验证建议。
  该限制是执行前 plan 的原始约束，不应被改写成“执行前已存在 directed 文件”。
- **实现调整**：撤销上述笼统排除，把本轮 directed 写范围精确限定为以下 plan-defined 内容：
  1. 新增 `memblock_main_dispatch_cancel_reconcile_sequence.sv`；
  2. 新增 `memblock_dispatch_real_cancel_reconcile_vseq.sv`；
  3. 新增 `tc_dispatch_real_cancel_reconcile_smoke.cfg`；
  4. 在执行前已存在的 `seq_pkg.sv` 和 `seq.f` 中注册两个新 sequence。
  三个新文件在本 plan 执行前均不存在；它们是本轮 coding 产物，不是执行前基线。
- **原因**：software-only 账本测试只能验证 framework 内部 token、record、rollback 和 cleanup，无法经过
  redirect driver、redirect monitor anchor、ctrl monitor cancel snapshot 和主 service 验证真实 DUT observed cancel
  链路。真实 directed vseq 是闭合该链路的必要验证入口，因此必须纳入本轮实现范围。
- **影响范围**：只修正上述三个新文件与两个注册文件的 plan 状态，不扩展任何其他
  directed testcase、不新增 cfg key，也不改变本 plan 的 commit/deq、cancel reconcile、global-stop 或
  pass/fail/terminal 功能方案。
- **覆盖边界**：software-only 账本测试仍只覆盖 synthetic token/record/rollback/cleanup，不宣称 DUT
  observed cancel；真实 DUT cancel directed vseq 覆盖 scalar younger load/store victim 的真实 redirect、anchor、
  cancel snapshot、software/observed 对账、reissue 和终态收敛，不扩展到 vector/multi-element、AMO/CBO、
  MMIO 或所有 redirect level 的穷举验证。
- **验证状态**：2026-07-23 最终工作区使用独立 mode `v2_lsq_mmio_cbo_final_20260723` 完成 VCS/KDB
  compile；最终 KDB 摘要为 `0 error(s), 0 warning(s)`，完整 transcript 另有一条工具自身的
  `LCA_FEATURES_ENABLED` usage warning。真实 cancel directed、默认 real smoke和pending-MMIO directed
  均为`TEST_PASS`且`UVM_ERROR=0/UVM_FATAL=0`。
  `tc_sanity + default.cfg`不会建立本plan main table，已作为错误组合终止，不作为失败或通过证据。

### [IMPLEMENTATION_DELTA] real-smoke responder 完成握手

- **来源**：独立 review 发现父类 `memblock_dispatch_real_smoke_vseq::body()` 在
  `start_core_dispatch_flow()` 返回后立即清除 `dispatch_real_smoke_active`，但后台 DCache、SBuffer 和
  redirect responder 仍可能尚未返回；它们依赖该 active 标志与 `global_stop_requested` 的组合观察最终退出边界。
- **原逻辑**：后台 responder 通过`join_none`启动，core flow返回后直接清 active并结束 vseq；若 responder
  尚未抢到最后一个 stop sample，会错过自然退出条件。
- **实现调整**：core flow返回后执行`wait fork`，等待同一父 task 创建的后台 responder fork完整返回，再清
  `dispatch_real_smoke_active`。不改变 responder的global-stop/inflight退出条件，也不新增第二个状态 owner。
- **原因**：把场景完成定义为“core flow完成且后台 responder完成握手”，消除收尾时序竞争；若 responder真正
  卡住，则由既有UVM timeout暴露，而不是静默结束主vseq。
- **影响范围**：仅`memblock_dispatch_real_smoke_vseq::body()`的vseq收尾顺序和本段文档说明；cancel
  reconcile子类已有独立`wait_for_background_responders()`，不改变其实现。

### [IMPLEMENTATION_DELTA] deferred ctrl resync 队首保留

- **来源**：独立源码 review 发现，`apply_raw_ctrl_deq()` 在
  `MEMBLOCK_LSQ_RESYNC_ON_MISMATCH=1` 时只打印 warning 后 `return`，旧 consumer 又无返回值并用
  automatic queue 的 `foreach` 继续执行，当前 raw 会在 task 返回时被静默丢弃。
- **原实现**：ctrl raw 从 `raw_ctrl_q` 弹出后只保存在本次 service task 的栈帧队列；handler 的
  LQ/SQ preflight 失败没有 success 返回值，consumer无法区分“成功应用”和“resync warning”。
- **实现调整**：`lsq_commit_handler::apply_raw_ctrl_deq()` 和 adapter wrapper 改为返回 success bit；
  `memblock_sync_pkg` 新增 `deferred_raw_ctrl_q` 及 push/peek/pop API。semantic batch完成后先把本拍 raw
  追加到持久 FIFO，再从队首消费；success=1才pop，success=0立即停止并留待下一service tick重试。
  `clear_raw_monitor_queues()`清该FIFO，`raw_monitor_queue_size()`把它计入runtime drain。
- **原因**：既保留strict模式的fail-fast，又使resync模式真正具备“暂不应用、后续重试”语义；raw不会
  丢失，后续raw也不会越过失败队首，global stop不会漏看已完成semantic conversion但尚未完成deq的事实。
- **副作用边界**：MMIO normalize仍在deq删除active map前执行，重复重试按canonical setter保持幂等；
  `sbIsEmpty`是level观察值，可重复更新。该队列不保存UID，不强制release，不改变pass/fail/terminal。

文字伪代码：

```text
每拍先把ctrl raw转换出的memoryViolation加入semantic batch，并把完整raw保存在本拍临时队列；
semantic redirect-first处理返回后，把临时队列全部追加到持久deferred FIFO；
查看持久FIFO队首并调用full-raw owner：
  若LQ/SQ联合预检成功并完成release，返回成功并弹出队首；
  若resync mismatch，返回失败，保留队首和全部后续raw到下一service tick；
  若strict mismatch，按原策略uvm_fatal；
runtime drain把持久FIFO计入未完成工作，队列非空时不允许global stop。
```

### [IMPLEMENTATION_DELTA] software-only smoke 复用 singleton commit owner

- **来源**：独立源码 review 发现 normal/fault software-only smoke 仍通过
  `lsq_commit_handler::type_id::create("commit_handler")` 建私有实例，绕过真实 flow 的 singleton
  cursor、modeled head、watermark和fault token合同。
- **原实现**：两个 smoke 的 `commit_and_deq_lsq()` 按需创建私有 handler；单测可能在私有状态上通过，
  却不能证明真实 adapter/lsqcommit sequence共享owner的行为。
- **实现调整**：两个 smoke 都改用 `lsq_commit_handler::get()`；各自 `body()` 开始时绑定公共
  `lsq_ctrl` 并调用一次 `reset_lsqcommit_runtime_state()`。后续 commit/deq helper只复用该句柄，不再创建
  第二实例。
- **原因**：软件场景之间需要清理singleton私有游标，但不能复制或直接清公共status/map/LSQ pointer；
  reset API正好只清handler私有生命周期状态。
- **验证边界**：legacy `soft_test_tc_dispatch_smoke/fault_smoke` 在当前V2环境会在场景启动前被既有
  int-WB monitor X/Z检查终止，因此本轮不把该testcase结果写成通过；VCS编译和真实flow回归已经覆盖
  singleton API的类型与共享owner主路径。

文字伪代码：

```text
software-only normal/fault body开始：
  取得lsq_commit_handler::get()返回的唯一实例；
  绑定公共lsq_ctrl；
  只reset handler私有commit cursor、modeled head、watermark和fault token；
  再构建本场景main/status并执行commit/deq；
commit_and_deq_lsq若句柄为空，只再次调用get，不再factory create私有handler。
```

### [IMPLEMENTATION_DELTA] CBO 的 scalar ROB store sideband 分类显式化

- **来源**：最后一轮独立review发现，plan和review使用“scalar store”描述`pendingst/scommit`，但初版
  实现直接读取`behavior.commit_is_store`；该字段对普通STU store和STU CBO都为1，文档没有说明CBO
  是否属于本sideband分类，现有真实回归也没有执行完整CBO flow。
- **V2权威语义**：`src/main/scala/xiangshan/backend/rob/Rob.scala`用
  `commitType == CommitType.STORE && !robEntry.vls`生成`scommit`，并用ROB head的
  `commitType == CommitType.STORE`生成`pendingst`。CBO解码为`FuType.STU`，所以属于非vector
  `CommitType.STORE`；它不是普通memory store，但属于本接口的scalar ROB store分类。
- **原plan/初版实现**：原plan要求只统计“normal scalar store”，但未定义该术语是否包含CBO；初版
  实现用`commit_is_store`同时驱动两个字段，功能方向符合RTL，但缺少白名单helper、差异说明和可观察
  分类检查，容易被误改成只接受`behavior.kind==STORE`。
- **实现调整**：`memblock_op_behavior_util`新增无状态
  `is_scalar_rob_store_commit(behavior)`，只接受`commit_is_store=1`且kind为STORE或CBO的行为；
  `lsq_commit_handler::build_lsqcommit_xaction()`构造`pendingst`和统计`scommit`时统一调用该helper。
  real cancel directed main sequence在导入真实三笔load/store主表前构造一个不入表的CBO probe，要求
  helper对LOAD/STORE/CBO返回0/1/1，并打印`ROB_STORE_CLASS`通过日志。
- **原因**：用同一个helper保持level字段和pulse字段分类一致，同时与V2 ROB的`STORE && !vls`语义
  对齐；显式STORE/CBO白名单又避免未来新增store-like behavior时无审查地进入接口。
- **影响范围**：不改变main table、LSQ allocation、issue/writeback、deq、pass/fail/terminal或CBO
  支持开关；CBO probe不进入主表、不驱DUT，也不创建状态/map。默认CBO激励仍由
  `MEMBLOCK_OP_CLASS_CBO_WT=0`边界关闭。
- **验证边界**：本轮只验证ROB sideband分类helper并继续回归普通store真实闭环，不宣称支持或覆盖
  CBO enqueue、issue、writeback、DCache response、commit/deq完整flow。

文字伪代码：

```text
is_scalar_rob_store_commit(behavior)：
  返回 behavior.commit_is_store
     且 behavior不是atomic
     且 behavior.kind属于STORE或CBO；

构造pendingst和scommit：
  pendingst只读取当前active head的同一helper结果；
  scommit只累计normal commit batch中同一helper为1的uid；
  fault、load、atomic和vector不贡献；

real cancel directed分类检查：
  从现有load/store transaction派生behavior；
  额外构造不入主表的CBO STU probe并派生behavior；
  若helper结果不是LOAD/STORE/CBO=0/1/1则fatal；
  只输出分类通过日志，不推进任何runtime状态。
```

以下仍作为补充验证建议保留，不用于扩张上述本轮 directed 文件范围，也不在终回归前标记为已通过：

- 增加 modeled head 有效但 active-map 暂未命中的 sideband 检查：`pendingPtr` 保持 modeled key，
  `pendingst/pendingMMIOld` 为 0；表尾只发布 final committed watermark。
- 在 cancel reconcile directed flow 中先送 snapshot、后送 anchor，确认 snapshot 不被 baseline pop；再覆盖
  无 record baseline 消费、exact target 匹配和 deadline/missing-target fatal。
- 在 terminal 后分别保留 raw monitor、exception、三类 issue、redirect、PTW replay、flushSb 和 cancel
  pending，确认 global stop 均保持 0，并在逐项 drain 后才变为 1。
- 覆盖 V2 同一 full raw 同时携带 LQ/SQ deq，以及 SQ owner 预检失败场景，确认失败时 LQ pointer/free
  count 也不发生部分更新。
