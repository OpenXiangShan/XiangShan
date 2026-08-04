# mem_ut V2 DCache CBO Probe 闭环实施 Review

| 项目 | 内容 |
|---|---|
| 关联 plan | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_dcache_cbo_probe_closure_plan_20260731.md` |
| Review 范围 | DCache coherent responder 的 CBO A/B/C/D 生命周期、response capacity、Probe token、退出条件与文档同步 |
| 目标版本 | V2，`mem_ut_uvm_v2` |
| Review 结论 | 通过。实现只扩展 responder 内部 CBO 生命周期，不改变主表、LSQ、ROB、pass/fail 或 terminal owner。 |
| Review 日期 | 2026-08-04 |

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 对应代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| `CBO context` | 单笔已真实接受 CBO 从 A.fire 到 CBOAck D.fire 的唯一软件所有者 | `cbo_context_valid` 与 `pending_cbo_probe_*` | CBO hit 等待 Probe C reply 时，下一笔 CBO 被 A.ready 反压。 |
| `reservation` | 已接受 CBO 为未来 CBOAck 提前占用的一笔 DCache response capacity | `cbo_response_reserved` | Probe 等待期间其它 response 不能抢走 CBOAck 的容量。 |
| `probe record` | 一笔 B Probe 的稳定身份、旧 alias、target cap 和 B/C 阶段 | `probe_record_q` | CBOClean 使用 toB，CBOFlush/Inval 使用 toN。 |
| `probe token` | 测试框架内部唯一 Probe 编号，不在 C channel 传输 | `dcache_probe_token_t`、`pending_cbo_probe_token` | ProbeAckData 两拍期间用 token 锁定同一 owner。 |
| `C assembly` | 两拍 ProbeAckData 或 ReleaseData 的 C channel 收集状态 | `c_assembly_owner`、`c_assembly_probe_token` | 第二拍必须属于第一拍锁定的 Probe。 |
| `direct miss` | CBO 地址没有 ACTIVE line record 时不需要 B/C Probe 的路径 | `enqueue_pending_cbo_ack()` | A.fire 后立即把 reservation 转为 CBOAck record。 |
| `toB/toN` | B Probe 要求 DCache 保留为 Branch 或失效为 None 的目标权限 | `TL_CAP_TOB/TL_CAP_TON` | Clean 固定 toB；Flush/Inval 固定 toN。 |

## 2. Review 范围与结论

抽象功能描述：本专项将旧的“CBO A.fire 后直接 CBOAck”替换为“miss 直接 Ack、hit 先 Probe 后 Ack”。
它只处理 DCache responder 已观察到的 coherent A/B/C/D handshake；不产生 CBO A request、不把 CBO
写入主表或状态表，也不改变当前 V2 主流程对主动 CBO 的 admission-fatal 边界。

本 agent 按原 plan、`IMPLEMENTATION_DELTA`、当前 `git diff` 和 VCS smoke 复查了所有新增状态的设置、
读取、清理、capacity 计数、token 校验、flush/stop drain 和文档描述。未发现 reservation 漏计、命中路径
提前 Ack、CBOAck 重复删除 line、或 Release 同线覆盖 Probe owner 的遗漏。

| 场景 | 修改前 | 当前实现 | 判断 |
|---|---|---|---|
| CBO miss | A.fire 后建立 CBOAck | 保留该行为，但以 context/reservation 统一到 D.fire 清理 | 一致且可串行下一笔 CBO。 |
| CBOClean hit | 直接 CBOAck | `Probe(toB)` 完成后才 CBOAck，line 保留 ACTIVE | 满足 CBO coherent 闭环。 |
| CBOFlush/Inval hit | 直接 CBOAck D.fire 删除 line | `Probe(toN)` 完成时删除 line，随后 CBOAck | 不会在 C response 之前提前完成。 |
| CBO error | CBOAck record 创建点采样 | A.fire 时采样并保存，命中路径延后复用 | 不随 Probe delay 改变结果。 |
| 同线 Release 与 Probe | 可能先删除 line record 后使 Probe C 无 owner | C 入口 fail-fast | 明确轻量模型不支持的合并语义。 |

## 3. 源码逻辑 Review

### 3.1 `clear_cbo_context()` 与 response capacity

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，函数：`clear_cbo_context()`、`get_dcache_response_count()`。

抽象功能描述：`clear_cbo_context()` 是 CBO context 的唯一清理入口；它只清 CBO 私有状态。`get_dcache_response_count()`
把 reservation 纳入已有 DCache capacity 账本；两者不建立 D response、不操作 Probe queue。

```systemverilog
function void dcache_mem__access_base_sequence::clear_cbo_context();
    cbo_context_valid        = 1'b0;
    cbo_response_reserved    = 1'b0;
    pending_cbo_probe_valid  = 1'b0;
    pending_cbo_probe_opcode = '0;
    pending_cbo_probe_line   = '0;
    pending_cbo_probe_source = '0;
    pending_cbo_probe_cap    = '0;
    pending_cbo_probe_token  = '0;
    pending_cbo_ack_denied   = 1'b0;
    pending_cbo_ack_corrupt  = 1'b0;
endfunction

```

中文伪代码：该函数把 CBO context、reservation、Probe 关联字段和错误快照全部清零。reset 或 CBOAck
D.fire 调用它后，下一笔 CBO 才能重新建立自己的 owner；它不清共享 line map，也不清普通 Probe。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，函数：`get_dcache_response_count()`。

抽象功能描述：该函数只统计当前 DCache response capacity 的占用，不改变任何 queue 或状态。

```systemverilog
function int unsigned dcache_mem__access_base_sequence::get_dcache_response_count();
    return dcache_rsp_q.size() + (current_d_valid ? 1 : 0) +
           (c_assembly_response_reserved ? 1 : 0) +
           (cbo_response_reserved ? 1 : 0) +
           deferred_response_reservation_count;
endfunction
```

中文伪代码：该函数把 queued/current D、ReleaseData reservation、CBO reservation 和 deferred Acquire
统一相加，返回值被 `has_dcache_response_capacity()` 用来决定 A/C ready。GrantAck wait 不占该表容量，
但仍独立占用 sink；因此 CBO 等待 Probe 时会真实占用一笔 response slot。

### 3.2 `can_accept_dcache_a_request()` 的命中 CBO 准入

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，函数：`can_accept_dcache_a_request()`。

抽象功能描述：该函数只根据当前 responder 状态决定下一拍是否打开 A.ready；它不创建 CBO context、
不分配 token，也不改变 line record。它保证一旦 CBO A.fire，后续资源一定可被建立。

```systemverilog
if (cbo_context_valid) begin
    return 1'b0;
end
if (cached_line_by_addr.exists(line_addr)) begin
    line_record = cached_line_by_addr[line_addr];
    if (!line_record.alias_valid ||
        (line_record.lifecycle_state != DCACHE_LINE_ACTIVE) ||
        line_record.deferred_acquire_valid ||
        has_probe_for_line(line_addr)) begin
        return 1'b0;
    end
    if (probe_record_q.size() >= DCACHE_MAX_PROBE_RECORDS) begin
        return 1'b0;
    end
end
return has_dcache_response_capacity();
```

中文伪代码：先拒绝第二笔 CBO，保证只有一个 context。若当前地址有 line record，则它必须是 ACTIVE，
不得已有 deferred Acquire 或同线 Probe；否则保持 A.ready=0。随后检查共享 Probe queue 未满，因为命中
CBO 必须创建一个 token 化 Probe；最后检查 response capacity。miss 不需要 Probe queue，只需 response
capacity。这避免了“已经接受 CBO、reservation 已占用、却无法建立 Probe owner”的不可收敛状态。

### 3.3 `accept_dcache_a_request()` 的 CBO 分流与 error snapshot

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，task：`accept_dcache_a_request()`。

抽象功能描述：该 task 只消费已确认的 A.fire。它将 CBO 的 A payload 固化为 context，选择 direct miss
或 hit Probe 路径；不直接驱动 D、不等待 C，也不改动主表。

```systemverilog
clear_cbo_context();
cbo_context_valid        = 1'b1;
cbo_response_reserved    = 1'b1;
pending_cbo_probe_opcode = req_xact.auto_inner_dcache_client_out_a_bits_opcode;
pending_cbo_probe_line   = line_addr;
pending_cbo_probe_source = req_xact.auto_inner_dcache_client_out_a_bits_source;
pending_cbo_ack_denied   = sample_d_error_enable(
    seq_csr_common::get_l2_cbo_ack_denied_wt(), "DCache CBOAck denied"
);
pending_cbo_ack_corrupt  = sample_d_error_enable(
    seq_csr_common::get_l2_cbo_ack_corrupt_wt(), "DCache CBOAck corrupt"
);

if (!cached_line_by_addr.exists(line_addr)) begin
    enqueue_pending_cbo_ack(accept_cycle);
    return;
end
```

中文伪代码：真实 A.fire 后先清空旧值并建立新的单笔 context，保存 opcode、line、source 以及只采样一次的
error snapshot，同时使 reservation 生效。若 line 不存在，这是 CBO miss，调用
`enqueue_pending_cbo_ack()` 将 reservation 转为可调度 D record 后返回。若 line 存在，后续分支检查其
ACTIVE 状态，按 CBO opcode 选择 toB 或 toN，调用 `submit_probe()` 创建共享 record/token；此时不创建
CBOAck，必须等待 Probe C completion。

### 3.4 `enqueue_pending_cbo_ack()` 与 `complete_cbo_probe()`

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，task：`enqueue_pending_cbo_ack()`、`complete_cbo_probe()`。

抽象功能描述：前者将已预留的容量转换为真正的 CBOAck response record；后者只处理已收敛且确属当前
CBO 的 Probe record。二者不选择 D scheduler、不处理 C assembly 数据写回。

```systemverilog
if ((probe_record.owner != DCACHE_PROBE_OWNER_CBO) ||
    !cbo_context_valid || !pending_cbo_probe_valid ||
    (probe_record.token != pending_cbo_probe_token) ||
    (probe_record.line_addr != pending_cbo_probe_line) ||
    (probe_record.target_cap != pending_cbo_probe_cap)) begin
    `uvm_fatal(get_type_name(), "CBO Probe completion does not match context")
end
pending_cbo_probe_valid = 1'b0;
enqueue_pending_cbo_ack(complete_cycle);
```

中文伪代码：先确认完成的 Probe owner、token、line 和 target cap 全都等于 context 保存值；任意不一致表示
C response 被错误归属，立即 fatal 而不是错误回复 CBOAck。验证成功后清 `pending_cbo_probe_valid`，再调用
`enqueue_pending_cbo_ack()`。后者复制 A.fire 时保存的 source/opcode/line/error，到期时间按现有 `+3`
规则建立 CBOAck record，并原子清除 reservation；因此容量不会被重复占用或提前释放。

### 3.5 `complete_probe_record()` 的 line 更新顺序

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，task：`complete_probe_record()`。

抽象功能描述：该 task 是共享 Probe service 的完成入口。它先更新 line lifecycle，再根据 owner 触发
CBOAck 转换；它不直接处理 D.fire 或 E.fire。

```systemverilog
case (probe_record.target_cap)
    TL_CAP_TOB: begin
        line_record.alias_valid     = 1'b1;
        line_record.lifecycle_state = DCACHE_LINE_ACTIVE;
        cached_line_by_addr[probe_record.line_addr] = line_record;
        probe_record_q.delete(probe_index);
        if (probe_record.owner == DCACHE_PROBE_OWNER_CBO) begin
            complete_cbo_probe(probe_record, complete_cycle);
        end
    end
    TL_CAP_TON: begin
        probe_record_q.delete(probe_index);
        remove_cached_line(probe_record.line_addr, "probe_toN");
        if (probe_record.owner == DCACHE_PROBE_OWNER_CBO) begin
            complete_cbo_probe(probe_record, complete_cycle);
        end
    end
endcase
```

中文伪代码：toB 将原 line 恢复为 ACTIVE 并保留 alias；toN 删除该 line。两种情况都先释放共享 Probe record，
再让 CBO owner 将 reservation 转为 Ack。这保证 CBOClean 在 Ack 前已完成降级，CBOFlush/Inval 在 Ack 前
已失效；`process_d_fire()` 无需重复删除 line。ProbeAckData 的数据写入与 corrupt 处理仍由现有 C assembly
先完成，随后才进入本函数。

### 3.6 `process_d_fire()` 与同线 Release 防御

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，函数：`process_d_fire()`、task：`start_c_assembly()`。

抽象功能描述：`process_d_fire()` 在真正完成 CBOAck 时只终结匹配 context；`start_c_assembly()` 在 C input
入口拒绝轻量模型无法合并的同线 Release/Probe 冲突。二者共同避免终态提前和 line record 被覆盖。

```systemverilog
DCACHE_PENDING_D_CBO_ACK: begin
    if (!cbo_context_valid || cbo_response_reserved || pending_cbo_probe_valid ||
        (completed_record.cbo_opcode != pending_cbo_probe_opcode) ||
        (completed_record.line_addr != pending_cbo_probe_line) ||
        (completed_record.source != pending_cbo_probe_source)) begin
        `uvm_fatal(get_type_name(), "CBOAck D.fire does not match active CBO context")
    end
    clear_cbo_context();
end
```

中文伪代码：D.fire 前必须证明当前 CBOAck 仍对应唯一 context，且没有未完成 Probe 或 reservation。验证后只清
context；命中 CBO 的 line 已由 Probe C completion 处理，miss 没有可删除 line。这样不会在错误的 D response
上释放下一笔 CBO 的状态。

```systemverilog
if ((c_req_xact.auto_inner_dcache_client_out_c_bits_opcode inside {
        TL_C_OPCODE_RELEASE,
        TL_C_OPCODE_RELEASEDATA
    }) && has_probe_for_line(line_addr)) begin
    `uvm_fatal(get_type_name(),
               $sformatf("Release/ReleaseData conflicts with an active Probe owner line=0x%0h",
                         line_addr))
end
```

中文伪代码：C Release/ReleaseData 到来时先把地址归一化为 physical line。若已有未收敛 Probe record，
该 Release 可能删除该 line，使后续 ProbeAck/Data 失去 token 对应的 owner，因此明确 fatal；不同 line
不满足该条件，继续使用原 Release/ReleaseData assembly 和 ReleaseAck 路径。

### 3.7 flush、随机 Probe 与 global stop

抽象功能描述：`try_start_probe()`、`is_l2_flush_drain_complete()` 和 `body()` 的 terminal drain 只把 CBO
context 作为已有协议 owner 纳入阻塞条件。它们不撤销 CBO、不创建第二套队列，也不改变 L2 flush 的 level
handshake。

代码复查结论：随机 Probe 只有 `cbo_context_valid=0`、无 reservation、无 CBO Probe 时才可新建；L2 flush
DRAIN 和 global stop 只有全部三项清零后才完成。这避免随机 Probe 抢占 CBO、flush DONE 提前拉高、或
global stop 在 CBOAck 未发送时退出 responder。

## 4. 与 Plan 的对齐检查

| Plan 项 | 实现判断 | 复查结果 |
|---|---|---|
| 单笔 CBO lifecycle owner | 一致 | `cbo_context_valid` 从 A.fire 持续到对应 CBOAck D.fire。 |
| miss direct CBOAck | 一致 | `enqueue_pending_cbo_ack()` 在 A.fire 后转换 reservation。 |
| hit `Clean->toB`、`Flush/Inval->toN` | 一致 | `accept_dcache_a_request()` 选择 cap，`complete_probe_record()` 更新 line。 |
| token 校验与两拍 C assembly | 一致 | 复用共享 token/C assembly；CBO completion 再校验 context token。 |
| CBOAck 不等待 E | 一致 | 不分配 sink；D.fire 清 context。 |
| 普通 Acquire/Release/Uncache/主表不改变 | 一致 | 改动局限在 DCache responder 的 CBO 分支和 C 同线冲突检查。 |

### 4.1 与原 Plan 不一致的实现

存在两项执行中修正，均已写入关联 plan 的 `IMPLEMENTATION_DELTA`：

| 原 plan | 当前实现 | 原因 |
|---|---|---|
| 只描述 response reservation，未明确 error snapshot 的保存和转换时点 | A.fire 保存 `denied/corrupt`，miss/hit 共用同一 reservation 转换 | 防止 Probe delay 导致二次随机，也避免容量被其它 response 占满。 |
| `submit_probe()` 失败后才报错，Release 可沿用兼容路径 | 命中 CBO 在 A.ready 预检 Probe capacity；同线 Release/Probe 冲突 fail-fast | 防止 A.fire 后形成无 owner context，以及 Release 删除 CBO Probe 的 line record。 |

### 4.2 Plan 未说明但实现补充的细节

无额外架构逻辑。`try_start_probe()`、flush drain 和 global stop 对 CBO state 的等待，是单笔 context
生命周期完整性所必需的最小收敛条件，已在 plan 的“主循环仲裁/状态优先级”和本轮
`IMPLEMENTATION_DELTA` 中明确。

## 5. 文档与 TODO 同步 Review

已同步的有效文档：

- `AI_DOC/mem_ut_flow_doc/dcache_l2_response_hint_probe_model_flow.md`：当前 CBO hit/miss、reservation、
  C completion、同线 Release 边界和 drain 条件。
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/mem_base_sequence.md`：state、准入、D.fire、C assembly
  与修改类型总结。
- `AI_DOC/analysis/interface/v2/agents/dcache_agent.md`：A/C/D 的 CBO 字段合同与 responder 行为边界。
- `AI_DOC/plan/test_framework/plan/undo/mem_ut_test_framework_todo_20260614.md`：明确本专项完成的是 responder，
  不删除主表主动 CBO/CBO.ZERO/early fault/commit-deq TODO。

旧 response-delay、D-error、L2 responder plan/review 已追加实施注记。旧文档中的“直接 CBOAck”仅指
CBO miss 或历史行为，不再作为当前 hit 的实现说明；早期完整 TileLink 计划和 sideband 历史计划也已
追加 owner/边界注记。

## 6. 验证与静态检查

| 检查 | 结果 |
|---|---|
| `git diff --check` | 通过。 |
| CBO context/reservation/token 交叉检索 | 每个新增字段均有设置、读取、reset/D.fire 清理和 drain consumer。 |
| VCS compile | `make eda_compile tc=basicTest ts=virtual_base_sequence mode=dcache_cbo_probe_20260804 partcmp_op=off` 通过，`0 error(s), 0 warning(s)`。 |
| 真实 smoke | `make eda_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=dcache_cbo_probe_20260804 cfg=tc_dispatch_real_smoke partcmp_op=off` 通过，`TEST_PASS`、`UVM_ERROR=0`、`UVM_FATAL=0`。 |

当前 smoke 的 `MEMBLOCK_OP_CLASS_CBO_WT=0`，因此未产生真实 CBO hit。该结果验证本次修改编译、现有
DCache responder、global stop 和普通 dispatch 流程未回归；它不是 CBO B/C/Ack directed 覆盖证据。
后续若开放 V2 主表 CBO，必须新增受控 CBO producer 和 hit/miss、ProbeAck/ProbeAckData、toB/toN、
error、backpressure 的 directed testcase，再评估主表/LSQ 专用完成语义。

## 7. 最终结论与剩余边界

Review 通过。CBO hit 从 A.fire 到 CBOAck D.fire 的 response capacity、source/opcode、error snapshot 和
Probe token 都由单一 context 维护；CBOAck 只在相应 Probe C 生命周期完成后创建，且不会再次删除已处理的
line。普通 DCache/Uncache 逻辑和测试框架主控制保持原有所有权。

未实现的边界是：完整 CoupledL2 directory/dirty owner、多 CBO context、同线 Release/Probe 合并、
CBO.ZERO、主动 CBO 主表/LSQ/ROB flow、early CBO fault、RM/scoreboard/coverage 和 CBO directed runtime
测试。上述边界已保留在 TODO，未被本提交隐式宣称支持。
