# DCache Alias 状态基础实现 Review

| 项目 | 内容 |
|---|---|
| 关联 plan | `AI_DOC/plan/test_framework/plan/do/mem_ut_dcache_multi_probe_alias_state_plan_20260803.md` |
| 实现范围 | `dcache_mem__access_base_sequence` 的 line/probe 生命周期基础 |
| 评审结论 | 通过：实现与 plan 的 `IMPLEMENTATION_DELTA` 一致；未发现阻断性逻辑遗漏 |
| 未覆盖边界 | 本 review 对应的 alias foundation 提交未包含随机 multi-batch/toB、CBO Probe closure、轻量 l2Flush；2026-08-04 后续专项已完成 multi-batch/toB 与轻量 l2Flush，CBO closure 仍待实现 |

## 1. 术语与抽象功能说明

| 英文术语 | 当前文档中的中文含义 | 代码对象或落点 | 使用场景/示例 |
|---|---|---|---|
| `line record` | 一个 physical line 的唯一轻量 alias 生命周期状态，不是完整 L2 directory | `cached_line_by_addr[line_addr]` | GrantAck 后记录 ACTIVE alias；alias conflict 时保留旧 alias |
| `probe record` | 一笔 B Probe 从创建到 C reply 完成的唯一软件 owner | `probe_record_q` | 保存 token、旧 alias、target cap、owner 和 B/C 阶段 |
| `token` | 测试框架内部递增的 Probe 唯一编号，C bundle 不携带它 | `dcache_probe_token_t` | ProbeAckData 第一拍写入 `c_assembly_probe_token`，第二拍继续同一笔 |
| `B hold` | 已选中且正在 B channel 等待 ready 的唯一 Probe | `probe_b_hold_valid/probe_b_hold_token` | B.ready=0 时仍从同一 record 输出稳定 payload |
| `deferred Acquire` | 已经 A.fire、但必须先清除旧 alias 的新 alias Acquire | `line_record.deferred_acquire` | old-alias Probe(toN) 完成后才恢复普通 Grant/GrantData 流程 |
| `ACTIVE` | DCache 已确认具有该 line/alias，可作为新 Probe 或 alias 检查候选 | `DCACHE_LINE_ACTIVE` | E.fire 完成 GrantAck 后进入 |

## 2. Review 范围与结论

本次替换了旧的 `cached_alias_by_line` 和单一 `pending_probe_*` 状态。旧模型只能保存一条 alias，
且所有 B/C 生命周期依赖一组全局 line/alias 标志；一旦后续出现多个 Probe、C two-beat 或 alias conflict，
新请求可能覆盖旧 owner，C response 也无法可靠反查。

实现后的唯一真源是 `cached_line_by_addr` 与 `probe_record_q`。同一 physical line 只能保留一笔未收敛
Probe；不同 line 可以各自 WAIT_C；B channel 仍只保持一笔 payload。此基础不改变 DCache A/C/D/E 基本握手、
shared memory physical key、response delay、sink 或主表/LSQ 逻辑。

## 3. 状态迁移

抽象功能描述：状态迁移把 line 的长期 alias 生命周期和 B/C Probe 的短期生命周期拆开保存，使后续策略
可以共用同一 owner，而不是再引入第二份 alias map 或全局 pending 标志。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，
`dcache_cached_line_record_t`、`dcache_probe_record_t`。

```systemverilog
dcache_cached_line_record_t cached_line_by_addr[mem_addr_t];
dcache_probe_record_t       probe_record_q[$];
dcache_probe_token_t        next_probe_token;
bit                         probe_b_hold_valid;
dcache_probe_token_t        probe_b_hold_token;
```
中文伪代码：

该字段组在 responder 中承担长期 line 状态和短期 Probe 状态的唯一存储职责。每次 GrantAck 通过
`cached_line_by_addr` 写入 ACTIVE alias；每次 `submit_probe()` 通过 `probe_record_q` 增加一笔独立 record；
`probe_b_hold_token` 只选择其中一笔给 B channel。reset 时 `clear_runtime_state()` 清空 queue、map、token 和
B hold，因此旧 run 的 owner 不能带入下一次启动。

## 4. Probe 创建与 B 驱动

抽象功能描述：`submit_probe()` 验证 line 生命周期和容量后创建 immutable Probe 身份；
`service_probe_b_hold()` 只在 B 无 owner 时挑选一笔 QUEUED record；`build_probe_b_xaction()` 只把当前
hold record 映射到 B payload，不决定 target 或删除 line。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，
`submit_probe()` 与 `build_probe_b_xaction()`。

```systemverilog
probe_record.token       = next_probe_token;
probe_record.line_addr   = line_key;
probe_record.probe_alias = line_record.active_alias;
probe_record.target_cap  = target_cap;
probe_record.owner       = probe_owner;
probe_record.state       = DCACHE_PROBE_STATE_QUEUED;
probe_record_q.push_back(probe_record);

cycle_xact.auto_inner_dcache_client_out_b_bits_param      = probe_record.target_cap;
cycle_xact.auto_inner_dcache_client_out_b_bits_address    = probe_record.line_addr;
cycle_xact.auto_inner_dcache_client_out_b_bits_data[2:1]  = probe_record.probe_alias;
```
中文伪代码：

`submit_probe()` 先确认 line 是 ACTIVE，或在 alias conflict 场景是 `ALIAS_CONFLICT`；若同 line 已有 record
或 queue 达到固定 16 笔则返回失败，不创建半条记录。成功后复制创建时的旧 alias、target cap 和 owner，
分配递增 token 并入队。B builder 只读取该 record，因此 B.ready 未打开时 line、param、alias 全部稳定；
B.fire 后 `process_probe_b_fire()` 仅把这笔 record 转为 WAIT_C，不提前删除 line。

## 5. Alias Conflict Deferred Acquire

抽象功能描述：`start_alias_conflict()` 在不同 alias 的 Acquire 已经真实 A.fire 后保存其完整 payload，
同时预留未来 Grant 所需资源并向旧 alias 发 Probe(toN)；`complete_probe_record()` 在旧 alias 收敛后
恢复同一份 A 请求。它们不直接伪造 D Grant，也不跳过 E GrantAck。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，
`start_alias_conflict()` 与 `complete_probe_record()`。

```systemverilog
line_record.deferred_acquire.copy(req_xact);
line_record.deferred_acquire_valid      = 1'b1;
line_record.deferred_response_reserved  = 1'b1;
line_record.lifecycle_state             = DCACHE_LINE_ALIAS_CONFLICT;
submit_probe(line_addr, TL_CAP_TON, DCACHE_PROBE_OWNER_ALIAS_CONFLICT, probe_token);

deferred_acquire = line_record.deferred_acquire;
release_deferred_acquire_resources(line_record);
probe_record_q.delete(probe_index);
cached_line_by_addr.delete(probe_record.line_addr);
accept_dcache_a_request(deferred_acquire, complete_cycle);
```
中文伪代码：

当 A.fire 发现同 physical line 的 ACTIVE alias 不同，框架先预留一个 response capacity 和 sink，避免等待
Probe 时被其它 request 占满；随后复制完整新 A payload，保留旧 alias，并建立 toN Probe。C reply 完成后，
若这笔 record owner 是 alias conflict，先释放预留资源、删除旧 line record，再把原 A payload 重新交给
普通 A builder。新 GrantData/Grant 仍照常等待 D.fire 和 E.fire；只有 E.fire 后 `record_cached_line()` 才把
new alias 写为 ACTIVE，因此旧 DCache 副本不会被提前覆盖。

## 6. C Reply Token 收敛

抽象功能描述：`start_c_assembly()` 负责用 C observable line 唯一定位 WAIT_C record；
`consume_c_beat()` 保证两拍 data 不会换 owner；`complete_probe_c_assembly()` 在完整 data 后写回或标记
corrupt，并把最终 target 生命周期交给 `complete_probe_record()`。这些 task 不调度 D response。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，
`start_c_assembly()`、`consume_c_beat()`、`complete_probe_c_assembly()`。

```systemverilog
probe_index = find_waiting_probe_record_by_line(line_addr);
probe_record = probe_record_q[probe_index];
check_probe_response_param(probe_record, c_param, "ProbeAckData");
c_assembly_probe_token = probe_record.token;
probe_record_q[probe_index].state = DCACHE_PROBE_STATE_C_ASSEMBLY;

if (!c_assembly_corrupt_seen) begin
    dcache_mem_access_task(...);
end
complete_probe_record(probe_index, 1'b1, !c_assembly_corrupt_seen, complete_cycle);
```
中文伪代码：

ProbeAck 与 ProbeAckData 都先按 line 查找唯一 WAIT_C record；无命中或多命中立即 fatal，不能按地址随意删除
record。ProbeAckData 首拍保存 token 并将 record 转为 C_ASSEMBLY；第二拍先验证 token 仍存在、状态仍正确，
再检查 opcode/address/source/size/param 连续性。两拍完整后，正常数据写 shared memory overlay；任一 beat
corrupt 则报告 `uvm_error`、跳过 writeback 并将 `data_valid=0`，但无论是否 corrupt 都按 toN 删除旧 line/
解锁 deferred Acquire，或按 toB 保留 ACTIVE line，确保不遗留等待状态。

## 7. 主循环与退出条件

抽象功能描述：`body()` 仍是唯一时序 owner；它先确认上一拍 D/E/C/B/A fire，再以 C assembly、已到达 C
reply、B hold、A request 的优先级产生下一拍 item。它不把 responder 状态写入主表或 terminal。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv`，`body()`。

```systemverilog
service_probe_b_hold();
if (!c_fire && (c_assembly_owner != DCACHE_C_OWNER_NONE)) begin
    // 优先继续同一笔 C assembly
end else if (!c_fire && has_waiting_probe_c()) begin
    // 优先接受已到达的 ProbeAck/ProbeAckData
end else if (probe_b_hold_valid) begin
    build_probe_b_xaction(cycle_xact);
end

if (data.is_global_stop_requested() &&
    !probe_b_hold_valid && (probe_record_q.size() == 0) &&
    (c_assembly_owner == DCACHE_C_OWNER_NONE)) begin
    // 允许 responder 完整退出
end
```
中文伪代码：

每个 driver sample 先消费上一拍的 fire；B.fire 将当前 hold record 改为 WAIT_C，A.fire 使用当前 fired
snapshot 而不是过期 armed snapshot 建立 alias/response owner。下一拍仲裁中，C assembly 优先保证第二 beat
不可被其它 C response 插入；已有 WAIT_C 的实际 C.valid 优先于新 B；没有 C.valid 时才能发已 hold 的 B。
global stop 不会直接删除 record，而是等待 B hold、probe queue 和 C assembly 都归零后退出，避免已发 Probe
无 C response 时被静默丢弃。

## 8. 与 Plan 对齐

执行前 plan 的 `IMPLEMENTATION_DELTA` 指定本提交只建立共享 lifecycle foundation，而不在同一提交加入
multi-batch/toB 权重、flush done 或 CBO deferred context。实际实现严格遵守这一边界：

- `probe_owner` 和 `target_cap` 已为后续 policy 预留，但 legacy random 路径仍只提交单笔 `toN`。
- 没有新增 plus 参数、interface 字段、agent、主表、LSQ、Uncache 或 response delay 行为。
- 额外修正：A.fire 的状态更新改为使用 `fired_a_req_xact`。这是 plan 所要求的“真实 A.fire payload”落点，
  旧 `armed_a_req_xact` 继续只用于 valid 等待 ready 的稳定性比较。

没有执行前 plan 未说明且影响架构语义的实现；上述 A snapshot 修正已在 `IMPLEMENTATION_DELTA` 的
“A.fire 后保存完整 request”职责内，并在本 review 中显式记录。

## 9. 验证与复查

- 静态：`git diff --check` 通过；旧 `pending_probe_b_valid`、`waiting_probe_c`、`pending_probe_line`、
  `pending_probe_alias`、`cached_alias_by_line` 在实现文件中无残留。
- 编译：`make eda_compile tc=basicTest ts=virtual_base_sequence mode=alias_foundation` 通过。
- 仿真：`make eda_run tc=basicTest ts=virtual_base_sequence mode=alias_foundation` 通过，
  `UVM_ERROR=0`、`UVM_FATAL=0`。
- 真实 responder smoke：`make eda_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=alias_smoke cfg=tc_dispatch_real_smoke partcmp_op=off` 通过，
  `UVM_ERROR=0`、`UVM_FATAL=0`；日志显示 DCache responder 完成 GrantAck、line record 建立和 drain。

`base_fun` 的既有并发 VCS 任务会破坏共享 `tdc.sdb`，因此本次使用独立 mode 进行验证；该限制不影响源码
结论，也没有修改任何共享仿真基础设施。
