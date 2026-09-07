# MemBlock fault 后 STD inflight candidate 记账问题分析（2026-08-28）

| 项目 | 内容 |
|---|---|
| 状态 | 已完成波形取证；确认当前问题属于测试框架 issue-fire 记账边界，尚未确认 RTL 问题 |
| 分支 | `mem_ut_uvm_v2`（V2） |
| testcase | `basicTest` |
| VSEQ | `memblock_dispatch_real_smoke_vseq` |
| preset cfg | `tc_dispatch_real_mmu_sv39_smoke` |
| seed | `666666` |
| 目标规模 | `MEMBLOCK_MAIN_TRANS_NUM=10000` |
| 失败时刻 | `447.800ns` |
| 失败日志 | `mem_ut/ver/ut/memblock/sim/rm_sv39_10k_issuefire_mask_20260828/log/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_mmu_sv39_smoke_seed=666666_rtl.log` |
| FSDB | `mem_ut/ver/ut/memblock/sim/rm_sv39_10k_issuefire_mask_20260828/wave/tc=basicTest_ts=memblock_dispatch_real_smoke_vseq_cfg=tc_dispatch_real_mmu_sv39_smoke_seed=666666_rtl.fsdb` |
| 完整 VCD | `/nfs/home/lixiangrui/memblock_issuefire_mask_full_20260828.vcd` |
| VCD 片段 | `/nfs/home/lixiangrui/memblock_issuefire_mask_20260828.vcd` |
| RTL 修改 | 未修改；本次只分析 UVM framework 状态与 DUT 已有接口行为 |

## 1. 术语与判定范围

| 术语 | 当前含义 | 代码落点 | 本文示例 |
|---|---|---|---|
| `candidate` | issue scheduler 在一次 `send_issue_cycle()` 中选出并冻结到 xaction 的候选描述 | `memblock_issue_q_item_t`、`fired_items` | UID84 的 STD candidate 带 ROB `1/37`、SQ `1/39` |
| `inflight` | candidate 已交给 driver/DUT，但 framework 尚未完成 `std_dispatched` 记账的时间段 | issue xaction 与 `finish_item()` 之间 | UID84 在 fault 后仍保持 STD valid |
| `fire` | DUT issue 端口在同一采样边界满足 `valid && ready` 的真实接收事实 | `record_dispatch_issue_fire()`、`memblock_dispatch_fired_mask` | STD1 在 `445.3ns` 被 driver 观察到 fire |
| `issue snapshot` | fire 后写入 status 的 issue epoch、实例 flush epoch 和 target dispatched 状态 | `mark_issue_snapshot()`、`status.std_issue_epoch` | 只有建立 snapshot 后 raw STD 才能归属 UID |
| `dynamic epoch` | 同一 UID 被 redirect/reissue 后递增的动态实例编号 | `status.dynamic_epoch` | 旧 candidate 与新实例编号不一致时必须丢弃 |
| `value-only` | V2 STD writeback 只有 ROB value，没有 ROB flag，需在 flag 0/1 中反查 owner | `resolve_std_uid_by_rob_value_only()` | raw ROB value `37` |
| `fault pending` | 某个 target 已产生 fault，UID 仍 active，尚未完成 fault retire | `status.fault`、`status.exception_pending` | UID84 的 STA fault 后仍等待 STD 输出 |

本文只判定 RM/framework 归属和下一步修复边界。若后续波形证明 DUT 在没有任何真实 issue fire、且 framework 已建立对应 snapshot 的情况下仍生成无法归属的 raw output，才升级为 RTL 候选；升级后必须由独立 subagent 复核两次结论，不能直接修改 RTL。

## 2. 最新失败事实

日志中的关键片段为：

```text
427.800ns  issue feedback success uid=84 target=2
437.800ns  fault feedback uid=84 target=2 port=1 rob=1/37 exception_vec=0x8080
437.800ns  consume fault recovery event uid=84 target=2 issue_epoch=52
445.300ns  dispatch issue fire sta_port=0
445.300ns  dispatch issue fire std_port=0
445.300ns  dispatch issue fire std_port=1
445.300ns  skip stale issue item uid=84 target=3
447.800ns  INT_WB_STD_KEY，STD ROB value=37，无有效 owner
```

fatal 诊断同时给出了 UID84 的状态：

```text
uid=84 active=1 enq=1 std_dispatched=0 std_wb=0
fault=1 terminal=0 flushed=0 redirect=0 killed=0
std_issue_epoch=0
rob=1/37
```

`MEMBLOCK_ISSUE_TARGET_STA=2`、`MEMBLOCK_ISSUE_TARGET_STD=3`。因此 `std_dispatched=0` 并不是 UID 已经被 retire 或 redirect 清除，而是 candidate 仍 active 却没有完成 fire 记账。

## 3. FSDB/VCD 波形证据

### 3.1 DUT-facing STD1 信号

VCD timescale 为 `1ps`，`top_tb.U_MEMBLOCK` 层级下的 identifier 和变化如下：

| 信号 | identifier | `440.3ns` / `445.3ns` 观察 |
|---|---|---|
| `io_ooo_to_mem_issueStd_1_valid` | `B7` | `440.3ns=1`，`445.3ns=0` |
| `io_ooo_to_mem_issueStd_1_bits_uop_robIdx_value` | `E7` | `440.3ns=0x25`（37） |
| `io_ooo_to_mem_issueStd_1_bits_uop_sqIdx_value` | `G7` | `440.3ns=0x27`（39） |
| `io_ooo_to_mem_issueStd_1_ready` | `7'` | 该区间保持可接收 |
| `io_mem_to_ooo_writebackStd_1_valid` | `}*` | `440.3ns=1`，`445.3ns=0` |
| `io_mem_to_ooo_writebackStd_1_bits_uop_robIdx_value` | `~*` | `440.3ns=0x25`（37） |

### 3.2 内部 STD 执行单元

生成 RTL 的既有连接（仅用于解释波形，不是本次修改对象）是：

```text
MemExeUnit.io_out_valid = Std.io_out_valid
Std.io_out_valid = Std.io_in_valid
MemExeUnit 的 fuType、fuOpType、SQ metadata 直接取当前 io_in bits
```

因此该单元是零延迟的 valid/data 透传路径。`issueStd_1_valid` 在 driver 通过 clocking block 输出后立即可见，writeback monitor 可以在下一个 driver `clear_ready_dispatch_issue_ports()` 之前采到同一 ROB value。VCD 还记录了 `_inner_stdExeUnits_1_io_out_valid` 在 `440.3ns` 置 1，而 driver fire 日志在 `445.3ns` 打出；这解释了日志与波形时间相差一个 clock sample，并不等价于“DUT 没有输入”。

### 3.3 STA fault 与 STD candidate 的顺序

| 时间 | 事实 | framework 状态含义 |
|---|---|---|
| `415.3ns` | UID84 的 STA1 issue fire，ROB `1/37` | STA issue snapshot 已建立 |
| `427.8ns` | STA issue feedback success | 只表示 issue response，不代表 STD 已 fire |
| `437.8ns` | STA raw fault `0x8080` 被消费 | `status.fault=1`、`exception_pending=1`；UID84 仍 active |
| `440.3ns` | STD1 valid/payload ROB `37` 与 writeback valid 同时出现 | DUT 已有可观察 STD output；sequence candidate 仍等待 driver fire 记账 |
| `445.3ns` | driver 观察 STD1 `valid&&ready` 并置 fired mask | watcher 调用普通 scheduler，但资格检查拒绝 `exception_pending` |
| `447.8ns` | adapter 解析 value-only STD raw | UID84 缺少 `std_dispatched/std_issue_epoch`，触发 `INT_WB_STD_KEY` |

## 4. 当前 framework 控制流

### 4.1 旧路径

```text
issue_queue_scheduler::select_issue_candidates()
  -> send_issue_cycle() 冻结 fired_items
  -> driver 只置 memblock_dispatch_fired_mask
  -> fault handler 置 status.exception_pending
  -> driver fire watcher 调用 mark_issue_fire()
  -> is_issue_item_state_eligible() 因 exception_pending 返回 0
  -> std_dispatched/std_issue_epoch 仍为 0
  -> adapter 严格拒绝 value-only raw
```

`is_issue_item_state_eligible()` 同时用于正常仲裁和 fire 记账。它拒绝 `exception_pending` 对正常新候选是正确的，但对已经冻结、已经交给 DUT、随后才发生 fault 的 candidate 过于严格。driver 的 fired mask 只能证明真实 fire，不能替代 sequence 对 UID、动态实例和 issue epoch 的更新，所以不能用“忽略 adapter fatal”来修复。

### 4.2 为什么这是 framework 问题

1. UID84 在 fatal 时仍 `active=1、enq=1、flushed=0、redirect=0、killed=0`，不是 active map 删除后的 stale output。
2. `std_issue_epoch=0` 与 `std_dispatched=0` 只说明 issue snapshot 没建立，不能说明 DUT 没有接受 payload。
3. VCD 同时观察到 STD1 valid、ready 可接收、内部 `stdExeUnits_1` output valid，以及同 ROB value 的 writeback，说明 candidate 确实进入 DUT-facing 通路。
4. `status.exception_pending` 是 STA fault 的全局恢复标记；它不应取消同一动态实例中已经在飞行的 STD payload。
5. raw writeback 的值与 active UID84 的 ROB value/SQ candidate 一致；没有证据表明 DUT 凭空生成了另一个 ROB owner。

所以本次 fatal 是 framework 在 fault 与 inflight fire 之间缺少受限记账分支，RM 的 value-only 严格检查只是把状态缺口暴露出来。

## 5. 排除与 RTL 升级条件

当前不启动 RTL subagent，理由是还没有满足独立复核门槛。以下现象不能单独证明 RTL 错误：

- writeback 与 issue valid 在同一 VCD timestamp 出现；`MemExeUnit/Std` 本身是零延迟透传。
- driver 日志比 VCD 晚一个 clock sample；这是 clocking block 输出/采样顺序。
- raw STD 没有 ROB flag；V2 接口定义就是 value-only，必须依赖 active map 和 snapshot。

只有同时满足以下条件，才允许升级：

1. driver 已在对应 candidate 上确认一次真实 `valid&&ready` fire；
2. framework 已成功建立同一 UID、同一 `dynamic_epoch`、同一 target 的 issue snapshot；
3. candidate 没有被 redirect、flush、kill、retire；
4. 波形仍显示 DUT 产生了不携带任何对应输入 fire 的 STD raw，或携带不可能的 ROB/SQ identity；
5. 独立 subagent 复核源码连接、输入 fire 和输出时序后再次确认 RTL 归属。

满足第 5 条后，本任务只新增 RTL 问题记录（含错误点和波形路径），不修改 RTL。

## 6. 最优 framework 修改方案

关联可执行 plan：

```text
AI_DOC/plan/test_framework/plan/undo/memblock_dispatch_inflight_fault_boundary_plan_20260828.md
```

### 6.1 冻结 candidate 保存动态实例身份

在 `memblock_issue_q_item_t` 中增加 `dynamic_epoch`，由 `issue_queue_scheduler::make_issue_item()` 从当前 `status.dynamic_epoch` 快照。UID、target、ROB/SQ key、replay sequence 和该 epoch 共同定义一个冻结 candidate。redirect/reissue 后 UID 的 `dynamic_epoch` 会递增，旧 xaction 即使迟到也不能给新实例建立 snapshot。

### 6.2 增加受限的 fault-inflight fire 路径

新增 `issue_queue_scheduler::mark_issue_fire_from_frozen_candidate()`，只在普通 `mark_issue_fire()` 失败后调用。该 helper 必须同时验证：

- global flush 未阻塞；
- UID active/enq/issue_ready，且未 terminal、flushed、redirect_pending、issue_killed；
- status ROB key、LQ/SQ key 与 candidate 完全一致；
- status.dynamic_epoch 与 candidate.dynamic_epoch 一致；
- target 仍 queued、未 dispatched、未 writeback/pass/fault；
- 当前确实是 `fault && exception_pending`，而不是任意异常标记。

通过后复用正常 fire 的唯一状态转换：分配 issue epoch、调用 `mark_issue_snapshot()`、删除对应 issue queue entry、清 queued bit、置 target dispatched、清 replay target。该 helper 不放宽正常 candidate 选择，不修改 RM compare，也不读/写 RTL 信号。

### 6.3 sequence 只对真实 fired candidate 调用 fallback

`mark_fired_items()` 先按当前 flush 状态调用原有 `mark_issue_fire()` 或 `mark_issue_fire_already_accepted()`；返回失败时，再调用新的 frozen-candidate helper。成功后记录一次 `bookkept_fired_mask`，避免 watcher 和 `finish_item()` 补偿重复分配 issue epoch。helper 失败仍保留一次 stale warning，不把未知 raw 静默丢弃。

### 6.4 结果语义

UID84 的 STA fault 仍保持 `fault=1`，STD 只获得“已真实发射”的 snapshot。后续正常 STD writeback 若因全局 fault 被 `mark_target_normal_pass()` 拒绝，则保持 fault 终态，不会把 UID 误判为成功；若 STD raw 自身带 fault，则按同一 issue epoch 进入 target fault。这个方案既不把 fault 改成 pass，也不把未 fire 的新候选放行。

## 7. 验证计划

1. 先执行 `git diff --check` 和 VCS compile，确认新增字段在所有 struct copy/初始化路径可编译。
2. 用同一 seed、同一 cfg、`wave=on` 重跑 real-dispatch；确认 UID84 不再因 `INT_WB_STD_KEY` 终止。
3. 每次新 RM/framework 错误都记录独立日志、FSDB/VCD 路径和时序；若可由状态生命周期解释，新增分析与 plan 后继续修改并重跑。
4. 只有满足第 5 节全部 RTL 升级条件时才启动独立 subagent。subagent 复核若否定 RTL 归属，则回到 RM/framework 分析流程。
5. 目标是完成 10000 笔请求并看到 `TEST CASE PASSED`、`UVM_ERROR=0`、`UVM_FATAL=0`；若确认 RTL 问题则按用户要求记录证据后结束，不修改 RTL。

## 8. 文档和提交边界

- 本文是本次失败的独立分析记录；不替代历史 `memblock_rm_std_value_only_unowned_writeback_analysis_20260828.md`。
- 代码修改只涉及 `memblock_dispatch_types.sv`、`issue_queue_scheduler.sv`、`memblock_issue_dispatch_base_sequence.sv` 及本 plan 要求的 review/analysis 文档。
- 不修改 Scala/Chisel、生成 RTL、RM 异常比较、DUT interface、cfg 权重或用户已有无关改动。
- 该 feature 完成 coding、验证和 implementation review 后，plan 才能从 `undo` 移到 `do`；每个功能修改单独本地 commit，不 push。

## 9. RM 协同支持

本分析不实现 RM/checker/scoreboard。RM 继续只使用已冻结的 TLB/PMA/PMP context、DCache sticky ledger 和 issue snapshot；本方案仅保证 snapshot 在真实 inflight fire 后及时存在。

## 10. 功能覆盖率协同支持

本分析不实现 coveragent/covergroup。后续可使用 `fault_pending + frozen_candidate_fire + target` 作为场景统计维度，但不作为本次 coding 的验收条件。
