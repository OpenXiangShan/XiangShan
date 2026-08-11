# V2 L2TLB 同拍 Producer Barrier 修正执行 Plan

状态：`undo`，待 coding。本文只修正 L2TLB driver 在同一 `drv_cb` 内过早判定 sample ready 的问题；不执行 coding，不移动既有 `plan/do` 文件。

关联已归档 plan：

- `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_l2tlb_sfence_flush_token_timing_correction_plan_20260805.md`
- `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_sfence_hfence_stage_aware_live_entry_invalidation_plan_20260804.md`

关联源码：

- `mem_ut/ver/ut/memblock/agent/L2tlb_agent_agent/src/L2tlb_agent_agent_driver.sv`
- `mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv`

## 1. 专有名词与抽象功能说明

| 术语 | 当前含义和代码落点 | 示例 |
|---|---|---|
| `drv_cb` | L2TLB driver 每拍进入的 clocking-block 边界。 | C0 的 `drv_cb` 读取 DUT 在 C0 的 request/response 采样值。 |
| sample anchor | CSR monitor 为当前仿真时间建立的全局 sample 编号。 | `dut_sample_time==$time` 且 `dut_sample_seq=C0`。 |
| producer | 对当前 sample 发布语义完成证明的 monitor。 | CSR monitor 发布 CSR history；Fence monitor 报告本拍已检查 fence。 |
| watermark | producer 已完成到哪个 DUT global sample 的单调编号。 | `csr_history_published_seq=C0` 说明 CSR history 已可供 C0 使用。 |
| producer barrier | CSR/Fence 两个 producer 都完成当前 sample 后才开放的公共边界。 | 无 SFENCE 的 C0 也必须有 Fence producer done，表示“已确认本拍没有 fence”。 |
| `READY` sample | sample anchor 和两个 watermark 均对应当前 sample，可安全解释 C-2 CSR、C0 event 与 token/UID。 | C0 的 CSR/Fence watermark 都为 C0。 |
| `NOT_READY` sample | 当前时刻内未能取得完整 producer barrier 的 sample。 | CSR 已完成，但 Fence monitor 尚未完成 C0。 |
| probe | 在不离开当前仿真时间的前提下，driver 让出一个 NBA/delta 调度窗口后重新检查 anchor/watermark 的一次尝试。 | `MEMBLOCK_L2TLB_SAMPLE_PROBE_MAX_DELTA=2` 表示初始检查后最多再让出两次同拍调度窗口。 |
| NBA/delta | 同一个 `$time` 内的 SystemVerilog 调度阶段，不是下一时钟拍。 | `uvm_wait_for_nba_region(); #0;` 后仍要求 `$time==drv_anchor_time`。 |
| frozen VIF sample | driver 在进入本次 `drv_cb` 时一次性锁存的 request/response 物理信号集合。 | `req_valid/req_ready/vpn/s2xlate/resp_valid` 与 response payload 只从这一份采样构造。 |

### `sample_previous_vif()` 抽象功能描述

`sample_previous_vif()` 在当前 `drv_cb` 锁存一份完整 VIF 物理采样，并只在当前 `$time` 的有限 NBA/delta 窗口内等待 CSR/Fence producer barrier。它输出不可变的 `memblock_l2tlb_drv_sample_t`，供 sequence 与 monitor 解释本拍真实 request fire、response valid 和 lifecycle metadata。它不推进 global sample、不消费 raw fence、不创建 token、不重新读取下一拍 VIF。

## 2. 问题与修改边界

### 2.1 当前问题

当前 `L2tlb_agent_agent_driver::sample_previous_vif()` 先等待 CSR sample anchor，再立刻调用
`memblock_sync_pkg::l2tlb_sample_ready()` 形成 `sample_ready_result`。现有循环只检查：

```text
dut_sample_time_valid && dut_sample_time == $time
```

没有在同一轮 probe 内等待 `lifecycle_event_published_seq` 到达当前 sample。

因此可能出现以下合法 DUT 时序：

```text
C0 同一 $time：
  前一 cycle item 已驱动 req_ready=1。
  DUT 采样 req_valid=1、req_ready=1，真实 request fire。
  CSR monitor 已建立 C0 anchor 并发布 CSR watermark。
  driver 先于 Fence monitor 运行，看到 Fence watermark 仍为 C-1。
  driver 过早冻结 C0 为 NOT_READY。
  Fence monitor 随后在同一 $time 发布 C0 event/done。
```

此时 sequence 收到 `sample_ready_result=NOT_READY` 与 `sampled_req_fire=1` 的矛盾组合，可能报错；若没有 fire，则 C0 fence event 可能在后续 sample 被作为迟到事件解释。

### 2.2 本计划边界

本计划只修改 driver 的同拍 sample-ready 判定顺序。

- 不改 DUT interface、request/response 字段、L2TLB connect 或 driver 的正常 cycle-item 传输机制。
- 不改 CSR monitor 的 sample 推进职责，也不让 CSR monitor 代替 Fence monitor 写 producer done。
- 不改 Fence monitor 的 event/raw-fence 语义。
- 不改变 sequence 的 token、C0/C4、UID、response 或 flush barrier 主体逻辑。
- 不新增 runtime plusarg；继续使用 `MEMBLOCK_L2TLB_SAMPLE_PROBE_MAX_DELTA` 作为同拍固定上限。
- 不新增第二套 `NOT_READY` 计数器。现有 `memblock_l2tlb_base_sequence::note_l2tlb_sample_not_ready()` 继续作为跨 sample watchdog 的唯一维护者。

## 3. 修改方案

### 3.1 VIF 锁存必须先于 producer probe

修改对象：`L2tlb_agent_agent_driver::sample_previous_vif()`。

修改原因：同一 C0 的 VIF request/response 必须与最终 sample-ready 判定属于同一物理时钟边界。若在 NBA/delta probe 后再混合读取 live VIF，后续修改可能使 sample 不再是同一份冻结输入。

修改后逻辑：在记录 `drv_anchor_time=$time` 后，先一次性锁存当前 `mon_cb` 中所有当前函数已经消费的 request、response、response payload 和上一 item metadata；随后 producer probe 只读取 `memblock_sync_pkg`，不得再次读取 VIF。

文字伪代码：

```text
sample_previous_vif(sample):
  drv_anchor_time = $time
  sample.transport_sample_seq = next transport sequence

  一次性锁存当前 mon_cb：
    req_valid、req_ready、vpn、s2xlate、resp_valid
    全部 response payload 字段
    上一 driver item metadata

  对锁存的 req_valid/req_ready/resp_valid 执行已有 X/Z 检查。
  仅由锁存值计算 sampled_req_fire。

  调用本 plan 第 3.2 节的同拍 barrier probe。
  把 probe 的 sample_valid、dut_sample_seq、READY/NOT_READY 写入 sample。
  后续 reset/final/baseline metadata 仍只从此 frozen sample 推导。
```

### 3.2 同拍 CSR/Fence producer barrier probe

修改对象：`L2tlb_agent_agent_driver::sample_previous_vif()` 中现有只等待 anchor 的循环。

修改原因：`dut_sample_time_valid` 只能证明 CSR anchor 已建立，不能证明 Fence monitor 已完成当前 sample。`l2tlb_sample_ready(sample_seq)` 才表达完整 producer barrier。

修改后逻辑：在同一 `$time` 内最多执行固定次数的 NBA/delta probe。每轮同时检查 anchor、future watermark 和 current producer barrier；绝不等待下一次 `drv_cb`、`posedge` 或 `mon_cb`。

文字伪代码：

```text
anchor_seen = 0
ready_seen = 0
sample_seq = 0

for probe_count = 0 .. MEMBLOCK_L2TLB_SAMPLE_PROBE_MAX_DELTA:
  若 $time != drv_anchor_time：
    uvm_fatal
    // helper 不允许跨物理时钟边界。

  若 dut_sample_time_valid && dut_sample_time == drv_anchor_time：
    anchor_seen = 1
    sample_seq = peek_current_dut_global_sample()
    若 sample_seq == 0：uvm_fatal

    若 csr_history_published_seq > sample_seq 或
       lifecycle_event_published_seq > sample_seq：
      uvm_fatal
      // future watermark 不能解释当前 sample。

    若 l2tlb_sample_ready(sample_seq)：
      ready_seen = 1
      break

  若 probe_count 未到上限：
    uvm_wait_for_nba_region()
    #0
    // 仍在 drv_anchor_time，只给 CSR/Fence monitor 同拍完成机会。

sample.sample_valid = anchor_seen
sample.dut_sample_seq = sample_seq
sample.sample_ready_result = ready_seen ? READY : NOT_READY
```

同拍晚到与下一拍的边界固定如下：

```text
Fence watermark 在 drv_anchor_time 内、有限 probe 前达到 sample_seq：
  同拍晚到，当前 sample 记为 READY。

有限 probe 后仍未达到 sample_seq：
  当前 sample 记为 NOT_READY；不把之后的更新回填为本 sample。

下一次 drv_cb 才达到：
  属于下一 sample；新的调用使用新的 drv_anchor_time/new sample_seq，不能补写旧 sample。
```

### 3.3 `NOT_READY` 的真实 transport 处理边界

修改对象：driver 生成的 frozen sample 与现有 sequence `NOT_READY` 分支的协作边界。

修改原因：producer 未 ready 不能抹除真实 DUT handshake。把 `sampled_req_fire` 或 `sampled_resp_valid` 强行置零会隐藏当前 C0 的协议事实。

修改后逻辑：

```text
READY：
  sequence 正常消费 C-2 CSR、C0 event、token/UID 和 response 状态。

NOT_READY 且 frozen VIF 为 inactive：
  sequence 维持既有 inactive item；下一 drv_cb 重新锁存，不复用旧 VIF。

NOT_READY 且 frozen VIF 有 req_fire 或 resp_valid：
  保留真实锁存值并走现有 fatal。
  不得清零，不得延迟到下一 sample 假装本拍没有 handshake。
```

现有 sequence 的 `note_l2tlb_sample_not_ready()` 已负责跨 sample 的连续 `NOT_READY` watchdog。driver 本轮只负责消除同拍 producer 正常调度导致的伪 `NOT_READY`，不重复维护 timeout 状态。

## 4. 源码落点与状态副作用

| 文件 | 修改对象 | 修改内容 | 状态副作用 |
|---|---|---|---|
| `agent/L2tlb_agent_agent/src/L2tlb_agent_agent_driver.sv` | `sample_previous_vif()` | VIF 先锁存；probe 从只等 CSR anchor 改为等待完整 CSR/Fence producer barrier。 | 仅改变 frozen sample 的 `sample_valid/dut_sample_seq/sample_ready_result` 形成时机；不新增 package state。 |
| `common/memblock_common/src/memblock_sync_pkg.sv` | `l2tlb_sample_ready()` | 只复用现有 helper，不改变定义。 | 无新增写者。 |
| `seq/base_seq/memblock_l2tlb_base_sequence.sv` | `note_l2tlb_sample_not_ready()` 和既有 `NOT_READY` 分支 | 不改主逻辑；保留为唯一跨 sample watchdog 与真实 active transport fail-fast 入口。 | 继续维护现有连续 NOT_READY 计数。 |

高频路径说明：该 probe 位于每拍 driver 路径，但最多使用既有固定 `MEMBLOCK_L2TLB_SAMPLE_PROBE_MAX_DELTA` 次 NBA/delta 让步，只读取少量 package 标量，不扫描 table、queue 或 UID map。

## 5. 与原测试框架逻辑对比

| 修改类型 | 原逻辑 | 修改原因 | 修改后逻辑 |
|---|---|---|---|
| 功能逻辑修正 | 先取得 CSR anchor 就立即计算 `sample_ready_result`。 | Fence monitor 在同一 `$time` 内晚于 driver 时，真实 C0 request fire 会被标为 NOT_READY。 | 在当前 `$time` 的固定 NBA/delta probe 内等待 CSR/Fence watermark 均达到当前 sample，再冻结 READY。 |
| 时序采样修正 | VIF 字段在 anchor probe 后读取，与同拍 package 状态交错。 | 需要保证 C0 physical handshake 与 ready 判定属于同一不可变 sample。 | 进入 `drv_cb` 时先锁存全部 VIF/metadata，后续只等待 package producer 状态，不重读 VIF。 |
| 防御性边界澄清 | NOT_READY 可能被误解为 request/response 应当清零。 | 清零会隐藏 DUT 已经发生的 request fire 或 response valid。 | 保留真实 frozen handshake；producer timeout 且 active transport 时继续 fatal，只有 inactive sample 才发送 inactive item。 |
| 参数/字段变更 | 无。 | 本问题是同拍调度顺序，不是 V2 interface 字段变化。 | 不新增 plusarg、宏、interface 字段、transaction 字段或第二个 watchdog。 |

## 6. 非本计划内容

本计划不处理以下已识别的独立问题：

- 连续 flush barrier 覆盖较晚 hold。
- UID request-fire marker 错误依赖 issue-time CSR。
- `MEMBLOCK_MAIN_MEM_RANGES_EN` 改变 raw PPN。
- pre-ready baseline、post-close fire、event duplicate/gap、NO_OWNER passive driver 等其它 lifecycle corrective 项。

上述问题必须分别建立或补充对应 corrective plan，不能在本 plan coding 时顺带修改。
