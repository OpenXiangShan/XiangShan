# V2 DCache/SBuffer global-stop 尾请求收敛实现 Review

状态：已完成代码与双人复核，待人工 Review。
日期：2026-09-06
范围：`mem_ut` 测试框架的 DCache/SBuffer responder、real-dispatch 停止编排和最终检查；不修改 RTL、DUT 接口、PBMT/PBMTE 语义、RM 比较规则。

## 1. 术语与抽象功能说明

| 术语 | 当前实现中的中文含义 | 代码落点 | 示例 |
|---|---|---|---|
| `global stop prepare` | UID 已全部进入终态且公共 runtime 工作已收敛后、真正停止 responder 前的准备阶段。 | `common_data_transaction::global_stop_prepare_requested` | 327.8ns 的最后一个 UID retire 后先进入该阶段。 |
| `quiet window` | 从最近一次真实 memory-responder activity 起连续没有新请求或生命周期推进的最小等待窗口。 | `MEMBLOCK_GLOBAL_STOP_PREPARE_QUIET_TIME`、`global_stop_prepare_last_activity_time` | DCache 收到新的 A.valid 或 D.fire 时，1us 重新计时。 |
| responder sample | DCache/SBuffer 在其自身 `drv_cb` 时钟边界采到的一次真实接口样本。 | `note_global_stop_prepare_responder_sample()` | 两个 responder 都跨过 quiet deadline 的 sample 后，才允许提交 global stop。 |
| lifecycle 摘要 | 用固定数量的 valid、owner、queue size、timer 和 reservation 状态表达 responder 是否发生真实进展的轻量快照。 | `dcache_stop_prepare_state_t`、`sbuffer_stop_prepare_state_t` | 一个卡住但不变化的 D hold 不会每拍重置 quiet timer。 |
| `pre-stop A snapshot` | global stop 前已看到、但尚未 A.fire 的唯一 A 通道完整 payload。 | 两个 responder 的 `pre_stop_a_snapshot` | stop 后仅允许这份 payload 严格一致地完成一次握手。 |
| `terminal idle` | responder 已自然排空瞬态状态后发送的最后一个全 idle transaction。 | `build_dcache_idle_xaction()`、`build_sbuffer_idle_xaction()` | terminal idle 交付后才置 `*_responder_done`。 |
| `drain audit` | 不改变状态、只检查剩余 owner/queue/map 的结束审计。 | `audit_dcache_responder_state()`、`audit_sbuffer_responder_state()` | 发现 queue 非空时报告 `UVM_ERROR`，不 `.delete()` 掩盖现场。 |
| shared write batch | 两个 responder 共同使用、等待下一个 sample 提交到 shared memory 的写入暂存。 | `dcache_write_batch`、`uncache_write_batch` | 两边完成后再额外运行一个 monitor 边界，确认 batch 为空。 |

## 2. 修改原因与完整行为变化

### 2.1 修改前的问题

此前 `request_global_stop_if_done()` 在 UID 全部 terminal 且公共 runtime drain 完成时，直接置 `global_stop_requested`。这个条件只能证明主表、LSQ、monitor/adapter 的公共状态已收敛，不能证明已经从 DUT 内部流水发出的 DCache/SBuffer A 请求都已抵达 responder。

因此存在以下竞态：主表 terminal 后的同一拍或随后数拍，DCache/SBuffer 才第一次看到一个尚未 fire 的 A.valid。旧逻辑已经关掉 A.ready，并把该 A 判成“stop 后新请求”而 fatal；如果不 fatal，又会因无人接收而互等。原有 `pre_stop_a_snapshot_valid` 只能证明某次 A 在 stop 前可见，无法覆盖 global stop 过早提交这一根因。

### 2.2 修改后的总链路

1. 主表 terminal 前缀和公共 runtime drain 满足时，不再立即停止，先进入 `global stop prepare`。
2. prepare 期间禁止主框架继续产生新 issue route、周期 FlushSb 和新的随机 DCache Probe；已经在 DUT 或 responder 内部的 A/B/C/D/E、response、GrantAck、Probe 等仍正常推进。
3. DCache/SBuffer 每拍只比较固定大小的 lifecycle 摘要；出现 A/B/C/D/E fire、A/C/E/flush level 改变、queue/timer/owner/reservation 改变时，刷新 quiet window。稳定但卡住的 record 不被误认为持续进展。
4. 连续 1us 无 activity 且两个 responder 都实际采过 quiet deadline 之后的样本，才置 `global_stop_requested`。
5. stop 后只允许 stop 前冻结的 A payload 以严格稳定性检查完成一次 handshake；没有历史 snapshot 的新 A.valid 仍为协议错误。
6. DCache/SBuffer 都发送 terminal idle 且完成本地只读审计后，主 service 再运行一个完整 monitor service 边界，审计 shared write batch 和 DCache fragment observer；最后由 scenario 调用 `end_test_check()`，检查 raw/status 并关闭 monitor capture。

这是一条 responder 生命周期修复，不改变请求的功能结果，也不增加任何 UID 主表全表扫描。高频路径只读取 O(1) summary、queue size、bit 和 timestamp；map 遍历仅发生在 terminal audit/超时诊断路径。

## 3. 功能一：两阶段 global stop 与静默重计时

### 3.1 公共状态和 stop 提交条件

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`，`request_global_stop_if_done()`。

抽象功能描述：该函数仍是主 dispatch service 唯一的 global-stop 发布者。它消费主表终态和公共 runtime drain 结果，先建立 responder 的静默观察期；只有观察期满足后才把对外可见的 `global_stop_requested` 置位，不拥有 DCache/SBuffer 的私有 queue。

```systemverilog
if (!transaction_done() || !runtime_drain_complete()) begin
    reset_global_stop_prepare();
    return;
end
if (!global_stop_prepare_requested) begin
    global_stop_prepare_requested = 1'b1;
    global_stop_prepare_last_activity_time = $time;
    global_stop_prepare_dcache_last_sample_time = 0;
    global_stop_prepare_sbuffer_last_sample_time = 0;
    return;
end
if (($time - global_stop_prepare_last_activity_time) >=
        MEMBLOCK_GLOBAL_STOP_PREPARE_QUIET_TIME &&
    global_stop_prepare_responder_samples_settled()) begin
    global_stop_requested = 1'b1;
end
```

中文伪代码：

该逻辑负责把“主表已经完成”转换为“可以安全停止 memory responder”。如果 transaction 或公共 runtime work 再次出现，先调用 `reset_global_stop_prepare()` 清掉旧时间戳，避免把上一次的静默时间借给新的工作。第一次满足完成条件时只记录当前时刻并进入 prepare，不关闭任何 responder。随后每轮检查距离最后 activity 是否已满 1us，并调用 `global_stop_prepare_responder_samples_settled()` 确认 DCache 和 SBuffer 都真实经过 deadline 之后的采样边界；两个条件同时满足才提交 global stop。

`reset_global_stop_prepare()` 只清 prepare bit 和三个 timestamp，不清 responder 私有 queue、不改 UID terminal 状态。`global_stop_prepare_responder_samples_settled()` 只读两条 responder sample timestamp；software-only 场景没有这些 responder 时保持既有立即 stop 语义。

修改前是完成条件满足即写 `global_stop_requested=1`；修改后增加 prepare state，原因是主框架的完成条件不等价于 memory 接口已静止。

### 3.2 prepare 期间阻止新框架工作

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_auto_build_main_table_base_sequence.sv` 的 `service_real_dispatch_flow()`，以及 `memblock_issue_dispatch_base_sequence.sv`、`memblock_flushsb_base_sequence.sv`。

抽象功能描述：这些调用点只负责在 prepare 期间停止创建新的框架刺激；它们不截断已进入 DUT/responder 的请求，也不判断 DCache/SBuffer 是否已经排空。

```systemverilog
if (!data.is_global_stop_requested() &&
    !data.is_global_stop_prepare_requested()) begin
    route_all_issue_queues();
end

if (!data.is_global_stop_prepare_requested() &&
    !data.is_global_stop_requested()) begin
    issue_sched.route_all_ready_uids();
end
```

中文伪代码：

主 dispatch service 和独立 issue loop 都先检查 prepare/committed stop。两者均未置位时维持原有 route 行为；进入 prepare 后不再把 ready UID 放入新的 issue queue。若公共 runtime 工作重现，公共函数会撤销 prepare，下一个循环会自动恢复正常 route。FlushSb producer 同理在 prepare 中只等待时钟、不生成周期性请求；DCache 的 `try_start_probe()` 也增加 prepare gate，因此不会人为制造让 quiet window 永远无法结束的新 Probe。

### 3.3 DCache/SBuffer 的 O(1) activity 观察

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，`sample_dcache_stop_prepare_state()`、`sample_sbuffer_stop_prepare_state()`，以及两个 responder `body()`。

抽象功能描述：两个 helper 将当前 responder 的有限生命周期压缩为可前后比较的摘要。`body()` 仅在 prepare 期间用该摘要和真实 fire 判定 activity，再将 activity 回报给公共 stop owner；它不扫描 main table，也不修改协议 queue。

```systemverilog
current_stop_prepare_state = sample_dcache_stop_prepare_state(
    sampled_a_valid, sampled_c_valid, sampled_e_valid, sampled_l2_flush_en);
if (data.is_global_stop_prepare_requested()) begin
    data.note_global_stop_prepare_responder_sample(1'b1);
    if ((dcache_stop_prepare_state_valid &&
         (dcache_stop_prepare_state !== current_stop_prepare_state)) ||
        a_fire || b_fire || c_fire || d_fire || e_fire) begin
        data.note_global_stop_prepare_activity();
    end
end
dcache_stop_prepare_state = current_stop_prepare_state;
dcache_stop_prepare_state_valid = 1'b1;
```

中文伪代码：

每个非 reset sample 先取 DCache 当前的 valid、owner、queue size、timer、Probe/CBO/flush/reservation、write batch 等固定字段。prepare 中先登记“本 responder 已经看到本拍”的 timestamp，再比较新旧摘要；任何字段变化或任意 channel fire 都表示真实进展，调用 `note_global_stop_prepare_activity()` 将 quiet 起点更新到当前时间。之后保存本拍摘要供下一拍比较。SBuffer 使用完全对称的 A、D、response queue/timer、D hold 和写 batch 摘要。摘要不含 `cached_line_by_addr`，不会把稳定的 resident line 或卡住的 record 当成每拍 activity。

## 4. 功能二：pre-stop A snapshot 的严格尾请求处理

### 4.1 snapshot 生命周期

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，DCache/SBuffer 的 `body()`。

抽象功能描述：snapshot 是一个只属于单个 responder 的临时 A owner。它在 stop 前保存未 fire 的完整 request，并在 fire、valid 撤销或 reset 时清除；它不能用来授权 stop 后第一次出现的请求。

```systemverilog
if (!reset_active && !data.is_global_stop_requested() &&
    sampled_a_valid && !a_fire && !pre_stop_a_snapshot_valid) begin
    capture_dcache_a_xaction(pre_stop_a_snapshot);
    pre_stop_a_snapshot_valid = 1'b1;
end else if (!reset_active && !sampled_a_valid && !a_accept_armed) begin
    pre_stop_a_snapshot_valid = 1'b0;
    pre_stop_a_snapshot = null;
end
```

中文伪代码：

在 global stop 尚未提交时，如果已经看到 A.valid 但该拍还没有 A.fire，首次完整抓取 A payload 并置 snapshot valid。若 DUT 在 ready 生效前撤销 valid，且没有已经 arm 的握手 owner，则清除 snapshot，防止下一笔独立请求借用它。正常 A.fire 后同样清除。DCache 和 SBuffer 都使用这一生命周期，因此 snapshot 表示的是“stop 前实际可见的那一笔 A”，不是宽泛的“允许任何 tail request”。

### 4.2 stop 后只准同 payload 的已知 A handshake

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，DCache/SBuffer A admission 分支。

抽象功能描述：该分支在 stop 后执行最终协议保护。它允许有历史 snapshot 的 A 继续完成一次普通 A.ready/A.fire，但会比较当前 payload；无 snapshot 的 A.valid 仍立即报告协议错误。

```systemverilog
if (!a_fire && sampled_a_valid &&
    (!data.is_global_stop_requested() || pre_stop_a_snapshot_valid)) begin
    capture_dcache_a_xaction(sampled_req_xact);
    if (data.is_global_stop_requested()) begin
        if (pre_stop_a_snapshot == null) begin
            `uvm_fatal(get_type_name(), "global-stop DCache A admission lost its pre-stop snapshot")
        end
        check_a_payload_stable(pre_stop_a_snapshot, sampled_req_xact);
        sampled_req_xact = pre_stop_a_snapshot;
    end
    // 原有容量和 opcode 检查后才 arm A.ready
end
```

中文伪代码：

若未 stop，保持原来的 A admission 行为。若已 stop，只有 `pre_stop_a_snapshot_valid` 为真才进入该分支；先重新采当前 A payload，再调用 `check_a_payload_stable()` 验证它与冻结 snapshot 完全一致，任何变化均 fatal。通过后用冻结 payload 继续沿用既有 opcode 和容量检查，最终只会 arm 一次 A.ready。没有 snapshot 的 stop 后 A.valid 在前一保护分支直接 fatal，因此不会被错误接收或永久阻塞。

## 5. 功能三：responder 完成确认与非破坏性 drain audit

### 5.1 terminal idle 后才发布 done

源码位置：`mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv`、`mem_base_sequence.sv`。

抽象功能描述：`dcache_responder_done` 和新增的 `sbuffer_responder_done` 是当前 testcase responder 生命周期的完成回执。它们只在最后一个 idle item 已交付且本地 audit 已执行后置位；新主表开始、responder 启动和 reset 都会清除，不能继承上一 testcase 的结论。

```systemverilog
send_sbuffer_xaction(idle_xact);
last_cycle_xact  = idle_xact;
last_cycle_valid = 1'b1;
service_cycle++;
void'(audit_sbuffer_responder_state(sampled_a_valid,
                                    current_d_valid,
                                    "terminal idle published"));
memblock_sync_pkg::sbuffer_responder_done = 1'b1;
break;
```

中文伪代码：

SBuffer 先把最终 idle item 正常送往 driver，再调用只读 audit 检查 A owner、D hold、response queue/timer 和 snapshot。audit 不会清掉任何对象；即使发现残留也只报 `UVM_ERROR` 并保留现场。之后才置 done 并退出。DCache 采用同样顺序，额外覆盖 C/E、GrantAck、Hint、Probe、C assembly、CBO、flush、sink reservation 和 transient cached-line lifecycle。这样 scenario 观察到 done 时，至少已完成最后一次协议输出和本地状态检查。

### 5.2 timeout 只诊断，不伪造完成

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，两个 responder 的 `stop_wait_cycles` 分支。

抽象功能描述：stop 后若 responder 很长时间无法自然 drain，timeout path 只报告一次 `UVM_ERROR` 和当前 audit 结果，保留 queue/map/owner 现场，既不清空也不置 done。

```systemverilog
if (stop_wait_cycles >= DCACHE_STOP_DRAIN_TIMEOUT_CYCLES &&
    !stop_drain_timeout_reported) begin
    void'(audit_dcache_responder_state(..., "global-stop drain timeout"));
    `uvm_error(get_type_name(),
               "DCache responder did not drain within 10000 cycles; queues are preserved for audit")
    stop_drain_timeout_reported = 1'b1;
end
```

中文伪代码：

global stop 已提交但退出条件仍不满足时，计数器递增。达到 10000 周期时，调用同一只读 audit 输出剩余 owner/queue，并仅首次报错，防止日志被每拍淹没。`stop_drain_timeout_reported` 只控制诊断频率，不改变正常退出条件；因此不会把潜在环境问题伪装成已完成。

## 6. 功能四：最终 shared-memory/raw 审计的时序

### 6.1 先等两个 responder，再运行一个 monitor 边界

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_main_dispatch_auto_build_main_table_base_sequence.sv`，`memory_responders_ready_for_final_audit()`。

抽象功能描述：该 helper 由主 dispatch service 在 global stop 后调用。它等待两个 done 回执、额外跨过一个 service cycle，再对 shared write batch 和 fragment observer 执行只读审计；它不拥有 responder queue，也不关闭 monitor capture。

```systemverilog
if (memblock_sync_pkg::dcache_responder_done !== 1'b1 ||
    memblock_sync_pkg::sbuffer_responder_done !== 1'b1) begin
    return 1'b0;
end
if (!memory_responder_final_settle_pending) begin
    memory_responder_final_settle_pending = 1'b1;
    memory_responder_final_settle_service_cycle =
        memblock_sync_pkg::get_dispatch_service_cycle();
    return 1'b0;
end
if (memblock_sync_pkg::get_dispatch_service_cycle() <=
    memory_responder_final_settle_service_cycle) begin
    return 1'b0;
end
void'(mem_access_base_sequence::audit_shared_memory_drain_state(...));
return 1'b1;
```

中文伪代码：

若任一 responder 未 done，主 service 保持运行 monitor，不做最终审计。第一次观察到两者都 done 时，记录当前 service cycle 并故意再等一次；这保证同拍收尾的 raw monitor sample 不会在关闭 capture 前遗漏。下一完整 service 边界后，调用 `audit_shared_memory_drain_state()` 检查 DCache/Uncache write batch 与 DCache fragment observer 是否为空。audit 只报告，不清掉 batch/map；随后函数返回真，主 service 才允许退出。

### 6.2 `end_test_check()` 后置关闭 capture

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv`，`end_test_check()`；调用点为 real-smoke vseq、cancel-reconcile vseq 和 legacy testcase 生命周期末尾。

抽象功能描述：`end_test_check()` 是 scenario 的最后公共核验点。它在 responder 全部自然结束之后检查 raw/status/terminal/runtime 残留，最后才关闭 `dispatch_monitor_capture_en`；它不负责提前结束 responder。

```systemverilog
if (memblock_sync_pkg::raw_monitor_queue_size() != 0) begin
    `uvm_error("COMMON_DATA", "raw monitor queues are not drained at end_test_check")
end
if (memblock_sync_pkg::raw_pma_pmp_csr_write_q.size() != 0) begin
    `uvm_error("COMMON_DATA", "PMA/PMP CSR write FIFO is not drained at end_test_check")
end
// 检查 UID terminal 和 runtime drain
memblock_sync_pkg::dispatch_monitor_capture_en = 1'b0;
```

中文伪代码：

最终检查先读取公共 raw queue；PMA/PMP CSR FIFO 单独核验，因为它的正常消费者是下一次 request fire，terminal 后不能再让它反向阻塞 global stop，也不能偷偷删除。随后沿用既有 UID terminal、status 和 runtime drain 检查。所有检查完成后才关 capture，因此最后一个 responder sample 仍可被收集和消费。

## 7. 正确性检查与验证结果

### 7.1 Review 结论

本 agent 独立检查了状态初始化、reset、prepare 撤销、sample deadline、pre-stop snapshot、terminal idle、timeout、shared-memory audit 和 scenario lifecycle 的调用关系。

两名独立 subagent review 结论均为：P0=0、P1=0、P2=0。首轮 review 曾指出 `dcache_transient_drain_complete()` 的注释范围不够精确，已明确为“仅供 terminal drain audit 使用”。最后一次复核特别检查了 `dcache_responder_done`/`sbuffer_responder_done` 不会跨 testcase 继承。

### 7.2 静态检查

已对本轮文件执行 `git diff --check`，无空白错误。高频 prepare 路径只使用固定摘要和 queue size；`cached_line_by_addr` 的遍历仅在 terminal audit/diagnostic 路径执行，符合运行期逻辑构建规则。

### 7.3 编译和 smoke

已重新编译：

```text
make eda_compile tc=tc_dispatch_real_smoke \
  mode=stop_drain_basic_20260906 \
  wave=off pl=UVM_LOW timeout_ns=100000000
```

已通过 load 实路径 smoke：

```text
make eda_batch_run tc=tc_dispatch_real_smoke \
  mode=stop_drain_basic_20260906 \
  cfg=tc_dispatch_real_smoke \
  wave=off pl=UVM_LOW timeout_ns=100000000
```

日志显示 417.8ns terminal retire 后进入 prepare，1422.8ns 连续静默 1us 后提交 global stop，1425.3ns DCache/SBuffer 都发布 terminal idle，1452.8ns 正常结束，`UVM_ERROR=0`、`UVM_FATAL=0`。

已通过 store/SBuffer 实路径 smoke：

```text
make eda_batch_run tc=tc_dispatch_real_store_smoke \
  mode=stop_drain_basic_20260906 \
  cfg=tc_dispatch_real_store_smoke \
  wave=off pl=UVM_LOW timeout_ns=100000000
```

日志显示 327.8ns terminal retire 后进入 prepare，1332.8ns 提交 global stop，1335.3ns 两个 responder 都发布 terminal idle，1362.8ns `TEST CASE PASSED`，`UVM_ERROR=0`、`UVM_FATAL=0`。

## 8. Plan 对齐检查

本功能没有独立的 stop-drain plan；直接来源是用户要求的“DCache/SBuffer 支持有新请求/进展即重新计时、保留 pre-stop A snapshot、两个 responder 真正结束后再检查队列且不可直接清空”。查找范围包括 `AI_DOC/plan/test_framework/plan/undo` 与 `do`，未发现专门覆盖该功能的独立 plan。

`AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_l2tlb_pbmt_csr_gate_plan_20260902.md` 仅增加了一个执行补充，说明后续 PBMT real-DUT 场景依赖这一已完成的 responder 生命周期；它不是本功能的原始设计 plan，也不改变 PBMT 本身的实现边界。

### 8.1 实现与 Plan 不一致项

无独立 plan 可逐项比对。实现与用户明确需求一致：新增真实 activity 的静默重计时，保留并收紧 pre-stop snapshot，完成后进行非破坏性 queue/map audit，发现残留使用 `UVM_ERROR`。

### 8.2 Plan 未说明但 Coding 落实的细节

| 细节 | 原因与作用 | 代码位置 |
|---|---|---|
| 两条 responder sample deadline 证明 | 仅等待 1us 无 activity 仍可能错过同一时刻后半个 `drv_cb` 样本；要求 DCache/SBuffer 都跨 deadline 采样，避免 race。 | `common_data_transaction::global_stop_prepare_responder_samples_settled()` |
| 10000 cycle 单次超时审计 | 正常结束不用 timeout；它仅保留现场并报告卡住状态，防止无日志的无限等待。 | DCache/SBuffer `body()` |
| 一个额外 monitor service 边界 | 防止 terminal idle 同拍的 raw sample 在 capture 关闭前漏收。 | `memory_responders_ready_for_final_audit()` |
| 清 done 标志的三层入口 | 新主表、DCache 启动、SBuffer 启动都清零，覆盖 legacy default 与显式 vseq 两种拓扑。 | `reset_all_tables()`、两个 responder `body()` |

## 9. 非本次修改的逻辑分析

当前工作区还包含 PBMT/RM、boundary 地址、trigger 检查、RTL 知识库和其他未跟踪分析文档的既有改动；它们未纳入本 review、不会随本功能提交。`seq_pkg.sv` 中的 boundary soft-test include 也不属于本次 stop-drain 功能。

## 10. 结论与人工 Review 关注点

本实现已完成：DCache/SBuffer 收到真实 tail activity 时重新计时；global stop 不再过早关闭 responder；pre-stop A 只允许原 payload 一次性完成；terminal 后的检查不删除现场；两个 responder 与共享 memory 都在最终 audit 完成后才结束。

建议人工 Review 重点确认：1）1us quiet window 是否符合希望的固定策略；2）activity summary 是否覆盖需要认定为“进展”的所有 responder 状态；3）stop 后严格 payload 稳定性检查是否符合对 A 通道的协议要求；4）timeout 仅报 `UVM_ERROR`、不清状态的行为是否符合调试期望。完成人工确认后，再启动严格 PBMT=00/non-NC 的后续 10k/多 seed 测试。
