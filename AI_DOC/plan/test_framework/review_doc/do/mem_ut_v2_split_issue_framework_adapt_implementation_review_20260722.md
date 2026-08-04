# V2 Split Issue 测试框架适配 Implementation Review

| 项目 | 内容 |
|---|---|
| 关联执行 plan | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_split_issue_framework_adapt_execution_plan_20260708.md` |
| review 日期 | 2026-07-22 |
| 目标版本 | V2，`mem_ut_uvm_v2` |
| review 范围 | 当前 split-issue 子计划产生的源码、flow、analysis、plan 和网页流程图修改 |
| review 角色 | 本 agent 独立 review；另由 subagent 做最终遗漏检查 |
| 当前状态 | coding、文档同步、VCS 编译和 load smoke 已完成；subagent 最终 review 已通过 |

## 1. 结论摘要

本次实现把旧的扁平 `intIssue` 解释和固定 `7'h7f` fired-mask 收敛为 V2
`issueLda/issueSta/issueStd` 三组 split port。主表、issue queue、replay/redirect、writeback、commit、deq
和 terminal 的 owner 没有迁移；新增逻辑只位于字段适配、握手确认、unsupported 边界和高频 pending 诊断。

本 agent 初审未发现会改变 scalar issue 主体行为的 blocker。implementation review 已与 `git diff`
逐项对齐，环境中已有的 store SQ deq mismatch 已与本子计划结果分开记录；subagent 最后一轮结论为
`FINAL PASS`。

## 2. 修改前后总览

| 特性 | 修改前逻辑 | 修改后逻辑 | 修改类型 |
|---|---|---|---|
| issue 端口语义 | 文档和部分控制代码仍按 `intIssue_0..6` 解释，局部使用固定 3/2/2 | 字段直接落到 V2 `issueLda/issueSta/issueStd`；fired-mask 只在 `port_idx_for_item()` 中由 target-local pipe 映射 | 字段/参数适配 |
| candidate fire | blocking 路径可用全 1 mask 推进候选 | driver 只回填实际 `valid && ready`，sequence 校验候选外 bit，并按真实 mask 更新状态 | 功能逻辑修改 |
| redirect/flush | finish 后可能先因 epoch 变化跳过全部 marking | 先消费已确认 fire，再取消未 fire 候选；首次 launch 前也检查 epoch | 功能逻辑修改 |
| issue 字段 | 可能复用 V3 字段或把不同 target 混写 | 唯一 wrapper 检查 FuType/fuOpType/behavior/target，再按 V2 port 字段写入 | 字段适配 + 合法性 gate |
| vector issue | scalar testcase 仍可能启动 vecissue 默认 sequence | 删除三处默认入口，driver 对非零 `issueVldu` 直接 fatal | 功能边界修改 |
| pending 诊断 | 高频路径可能扫描主表或把无进展阈值当退出 | 只读三个 issue queue 的 `size()`；无 fire 只报错，global stop 才退出 | 性能/退出逻辑修改 |
| pipe 展开 | 宏变化可能到运行时才在不存在的物理字段处失败 | 初始化时拒绝超过当前显式 LDA/STA/STD=3/2/2 的 split profile | 编译期能力约束 |

## 3. 字段和 FuType 适配

### 3.1 唯一 pipe 上限和合法矩阵

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/issue_field_assigner.sv:43-66`，函数
`get_target_pipe_limit()`、`check_pipe_idx()`。这两个 helper 的职责是让所有字段赋值入口共享
compile-time pipe 数；它们不修改 transaction 或 queue。

```systemverilog
function int unsigned get_target_pipe_limit(input memblock_issue_target_e target);
    case (target)
        MEMBLOCK_ISSUE_TARGET_LOAD: return MEMBLOCK_DUT_LOAD_PIPE_NUM;
        MEMBLOCK_ISSUE_TARGET_STA:  return MEMBLOCK_DUT_STA_PIPE_NUM;
        MEMBLOCK_ISSUE_TARGET_STD:  return MEMBLOCK_DUT_STD_PIPE_NUM;
        default: `uvm_fatal("ISSUE_FIELD", "unsupported target")
    endcase
endfunction
```
中文伪代码：该 helper 根据目标返回当前 profile 的物理 pipe 数；LOAD、STA、STD 分别读取各自编译期数量，未知 target 立即报 fatal，不创建默认合法值。

```systemverilog
pipe_limit = get_target_pipe_limit(target);
if (pipe_idx >= pipe_limit) begin
    `uvm_fatal("ISSUE_FIELD", $sformatf("%s pipe out of range", caller))
end
```
中文伪代码：调用者传入 target 和本地 pipe 下标；先取得该 target 的真实上限，越界时在写 transaction 前终止，合法时不改变 item 或状态。

源码位置：同文件 `check_target_futype_fuoptype()`，该函数是字段赋值前的唯一行为矩阵检查入口。

```systemverilog
if (lsq_ctrl_model::is_vector_ls_futype(main_tr.fuType) ||
    main_tr.fuType == MEMBLOCK_FUTYPE_MOU || behavior.is_atomic) begin
    `uvm_fatal("ISSUE_FIELD", "outside scalar split issue scope")
end
case (main_tr.fuType)
    MEMBLOCK_FUTYPE_LDU: begin
        // 只允许普通 load 或 prefetch，且 target 必须是 LOAD
    end
    MEMBLOCK_FUTYPE_STU: begin
        // 只允许普通 store，且 target 必须是 STA 或 STD
    end
    default: `uvm_fatal("ISSUE_FIELD", "unsupported FuType")
endcase
```
中文伪代码：先拒绝 vector、MOU/AMO 和 atomic；LDU 必须由既有 `lsq_ctrl_model` 判定为普通 load 或 prefetch，并且只能进入 LOAD；STU 必须是普通 store、不能是 CBO，且只能进入 STA 或 STD；其它组合在字段写入前 fatal。函数只检查，不重写 behavior、queue 或 status。

### 3.2 V2 各 split port 字段

源码位置：`issue_field_assigner.sv:393-457`，函数 `assign_sta_main_fields()` 和
`assign_std_main_fields()`。它们把内部主表字段转换为 DUT-facing payload；STA/STD 的 FuType
先经过统一编码检查。

```systemverilog
dut_futype = encode_and_fit_dut_futype(main_tr.fuType, caller);
tr.io_ooo_to_mem_issueSta_0_bits_uop_fuType = dut_futype;
tr.io_ooo_to_mem_issueSta_0_bits_uop_robIdx_flag = main_tr.robIdx_flag;
tr.io_ooo_to_mem_issueSta_0_bits_uop_sqIdx_value = item.sq_key.value;
```
中文伪代码：STA 写入 V2 的 35-bit FuType、fuOpType、源操作数、立即数、完整 ROB key 和 SQ key；编码 helper 先确认内部 one-hot 没有超出 DUT 宽度，失败时不允许发送。

```systemverilog
tr.io_ooo_to_mem_issueStd_0_bits_uop_fuType = dut_futype;
tr.io_ooo_to_mem_issueStd_0_bits_uop_robIdx_value = main_tr.robIdx_value;
tr.io_ooo_to_mem_issueStd_0_bits_uop_sqIdx_flag = item.sq_key.flag;
```
中文伪代码：STD 只写 V2 实际存在的 ROB value，不伪造不存在的 `robIdx_flag`；同时写 FuType、fuOpType、源操作数和完整 SQ key。STD 不从 STA 或 LDA 借用不存在字段。

源码位置：`issue_field_assigner.sv:627-655`，函数 `assign_issue_item_fields()`。这是公共 wrapper，负责取得主表、检查 split profile 和行为矩阵，然后按原顺序调用三个字段 helper。

```systemverilog
main_tr = data.get_main_transaction(item.uid);
check_pipe_idx(item.target, pipe_idx, "assign_issue_item_fields");
behavior = lsq_ctrl_model::derive_op_behavior(main_tr);
check_target_futype_fuoptype(main_tr, behavior, item.target);
assign_main_issue_fields(tr, item, pipe_idx);
assign_issue_dep_fields(tr, item, pipe_idx);
assign_backend_meta_fields(tr, item, pipe_idx);
```
中文伪代码：先按 uid 找到主表 transaction，检查 target-local pipe 和 split capability，再复用既有行为派生；所有检查通过后才按原来的主字段、依赖字段、backend metadata 顺序写入 xaction。任何失败都在状态推进前 fatal。

## 4. Issue queue、真实握手和 redirect 边界

### 4.1 O(1) pending 判断

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/issue_queue_scheduler.sv:205-239`，函数
`route_all_ready_uids()` 和 `has_pending_issue_work()`。前者仍使用原 active window 路由，只把扫描上限换成 compile-time LSQ slot；后者是每拍诊断 helper。

```systemverilog
scan_limit = MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM;
for (uid = begin_uid; uid < end_uid && scanned < scan_limit; uid++) begin
    route_uid(uid);
    scanned++;
end
```
中文伪代码：每轮只在已有 active window 内按 uid 顺序扫描，最多处理编译期 LSQ slot 数量；不会为 issue pending 判断扫描完整主表。

```systemverilog
return data.load_issue_q.size() != 0 ||
       data.sta_issue_q.size()  != 0 ||
       data.std_issue_q.size()  != 0;
```
中文伪代码：只要三个 issue queue 任一个非空就报告有待发工作，否则报告没有待发 issue；函数不读取 item、不修改 map/status，也不把等待 writeback/commit/deq 的状态误判为 issue stall。

### 4.2 sequence 的真实 fired-mask 闭环

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq/memblock_issue_dispatch_base_sequence.sv:66-111`，函数
`drive_dispatch_issue_loop()`。它仍由 global stop 结束，no-progress 只产生诊断。

```systemverilog
issue_sched.route_all_ready_uids();
send_issue_cycle(cycle_idx, has_fire);
issue_sched.advance_issue_queue_delays();
pending_issue_work = issue_sched.has_pending_issue_work();
if (data.is_global_stop_requested()) break;
```
中文伪代码：每轮先路由 ready uid，再发送一拍 issue，随后推进 queue delay 并读取 queue 是否仍有工作；global stop 成立才退出。无 fire 且 queue 非空时增加诊断计数并按周期报告 `uvm_error`，不 break；queue 为空时清诊断计数，允许继续等待其它 flow 收敛。

源码位置：同文件 `send_issue_cycle()`，主要控制分支位于 `:113-190`；它把 candidate、driver 实际 fire、flush/epoch 变化分开处理。

```systemverilog
candidate_mask = '0;
foreach (fired_items[idx]) begin
    candidate_mask[port_idx_for_item(fired_items[idx])] = 1'b1;
end
effective_fired_mask = tr.memblock_dispatch_fired_mask & candidate_mask;
if ((tr.memblock_dispatch_fired_mask & ~candidate_mask) != '0) begin
    `uvm_fatal(get_type_name(), "driver returned fired bits outside candidate mask")
end
```
中文伪代码：finish_item 返回后，根据本拍真正挑选的 item 建立 candidate mask；driver 返回的 fired bit 若落在 candidate 外立即 fatal；有效 fire 只取返回 mask 与 candidate mask 的交集。

```systemverilog
if (effective_fired_mask != '0) begin
    mark_fired_items(fired_items, effective_fired_mask);
    has_fire = 1'b1;
end
if (tr.memblock_dispatch_aborted_by_redirect || flush_or_epoch_changed) begin
    return;
end
```
中文伪代码：无论是否随后发生 redirect/flush，先用已确认的真实 fire 调 scheduler 更新 dispatched 状态，再把未确认的候选留在 queue；abort 或 epoch 变化只结束本轮，不把未握手 item 当作已发射。

### 4.3 driver 的 sample 边界和 ready 判定

源码位置：`mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_driver.sv:65-101`，
`main_phase()`；`wait_dispatch_issue_ready()` 位于 `:223-245`，`drive_dispatch_issue_one_cycle()` 位于
`:248-273`。driver 每次取 item 前清空 `req`，首次上 VIF 前检查 flush epoch，之后按阻塞/非阻塞模式采样。

```systemverilog
while (1) begin
    req = null;
    seq_item_port.try_next_item(req);
    if (req != null) begin
        @this.vif.drv_mp.drv_cb;
        if (req.memblock_dispatch_wait_ready &&
            (memblock_sync_pkg::dispatch_flush_in_progress ||
             req.memblock_dispatch_flush_epoch != memblock_sync_pkg::dispatch_flush_epoch)) begin
            req.memblock_dispatch_fired_mask = '0;
            clear_dispatch_issue_ports(req);
            this.send_pkt(req);
            req.memblock_dispatch_aborted_by_redirect = 1'b1;
        end else begin
            this.send_pkt(req);
            if (req.memblock_dispatch_wait_ready) begin
                if (req.memblock_dispatch_nonblocking_issue)
                    this.drive_dispatch_issue_one_cycle(req);
                else
                    this.wait_dispatch_issue_ready(req);
            end
        end
    end
end
```
中文伪代码：每轮先把 item 句柄置空，避免无 item 时复用上一轮对象；取到 item 后先跨一个 clocking-block sample 边界。只有要求 ready 握手的 dispatch item 才执行首次 launch gate；若全局 flush 已开始或 item 保存的 epoch 已过期，先清 fired-mask 和全部 issue valid，再调用 `send_pkt()` 把 idle 明确驱到 VIF，最后置 abort。gate 通过时先发送 payload；若 item 要求 ready，再按 nonblocking 开关选择只采样一个边界或持续等待全部 valid port fire。

源码位置：同 driver `clear_ready_dispatch_issue_ports()`，`:303-377`。该函数是实际 fire 的唯一采样点。

```systemverilog
if (tr.io_ooo_to_mem_issueLda_0_valid) begin
    if ($isunknown(vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_ready)) begin
        `uvm_fatal(get_type_name(), "issueLda_0_ready is X/Z while valid")
    end
    if (vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_ready === 1'b1) begin
        tr.io_ooo_to_mem_issueLda_0_valid = 1'b0;
        record_dispatch_issue_fire(`MEMBLOCK_DUT_LOAD_PORT_BASE + 0, tr);
    end
end
```
中文伪代码：在 clocking-block 输入采样边界读取 ready；valid 为 1 且 ready 是确定的 1 才清 valid、设置对应 fired bit 并报告 fire。ready 为 X/Z 直接 fatal，ready 为 0 则保留 valid 等待下一轮；STA/STD 使用同样规则和 target-local base。

### 4.4 fired-mask 映射和状态 owner

源码位置：`memblock_issue_dispatch_base_sequence.sv:251-290`，函数 `port_idx_for_item()` 和
`mark_fired_items()`。`port_idx_for_item()` 是 target-local pipe 到 fired-mask 的唯一映射；scheduler 仍是 queue/status 生命周期 owner。

```systemverilog
case (item.target)
    MEMBLOCK_ISSUE_TARGET_LOAD: port_idx = MEMBLOCK_DUT_LOAD_PORT_BASE + item.uop_index;
    MEMBLOCK_ISSUE_TARGET_STA:  port_idx = MEMBLOCK_DUT_STA_PORT_BASE + item.uop_index;
    MEMBLOCK_ISSUE_TARGET_STD:  port_idx = MEMBLOCK_DUT_STD_PORT_BASE + item.uop_index;
endcase
```
中文伪代码：按 target 选择对应 compile-time base，再加 item 在该 target 内的本地 pipe；检查本地 pipe 和最终 bit 都在合法范围内，返回统一 mask 下标。

```systemverilog
if (data.issue_blocked_by_global_flush())
    fire_marked = issue_sched.mark_issue_fire_already_accepted(item);
else
    fire_marked = issue_sched.mark_issue_fire(item);
```
中文伪代码：正常时期由 `mark_issue_fire()` 校验当前可发状态并完成 issue epoch、queue 删除、queued 清除、dispatched 置位；flush/redirect 边界使用不再次要求全局未阻塞的兼容入口，保留已经被 DUT 接收的 fire，再由 redirect owner 处理未 fire 项。

## 5. Vector 和 unsupported 边界

源码位置：`mem_ut/ver/ut/memblock/agent/vecissue_agent_agent/src/vecissue_agent_agent_driver.sv:84-99`，
`send_pkt()`。本轮不建立 vector issue 正向闭环，因此 driver 只提供明确边界。

```systemverilog
if (tr.io_ooo_to_mem_issueVldu_0_valid !== 1'b0 ||
    tr.io_ooo_to_mem_issueVldu_1_valid !== 1'b0) begin
    `uvm_fatal(get_type_name(), "vector issue is outside current scalar scope")
end
drive_idle(tcnt_dec_base::DRV_0);
```
中文伪代码：收到 vector transaction 后，两个 valid 只允许是确定的 0；任何 1、X 或 Z 都立即 fatal，确定为 0 时把 vector VIF 保持 idle。该 driver 不写公共 status、pass/fail 或 terminal。

同时删除了 `tc_base.sv`、`tc_dispatch_real_smoke.sv` 和
`soft_test_tc_dispatch_smoke.sv` 中的 vecissue 默认 sequence。VSTU feedback 和 vector writeback monitor
仍由各自后续专项负责，split issue 本次没有重复实现它们。

## 6. 编译期能力和 xaction 宽度

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv:574-644`，
`check_compile_param_consistency()`。该函数只检查 compile-time 结构关系，不修改 runtime plus。

```systemverilog
if (MEMBLOCK_DUT_ISSUE_PORT_STYLE_SPLIT &&
    (MEMBLOCK_DUT_LOAD_PIPE_NUM > 3 ||
     MEMBLOCK_DUT_STA_PIPE_NUM > 2 ||
     MEMBLOCK_DUT_STD_PIPE_NUM > 2)) begin
    `uvm_fatal("SEQ_COMPILE_CFG", "split issue physical expansion is wider than explicit fields")
end
```
中文伪代码：初始化阶段检查当前 interface/xaction/driver 已显式展开的物理 port 上限；超过 LDA/STA/STD=3/2/2 就 fatal，避免宏看似可配置但实际没有对应字段。

`lintsissue_agent_agent_xaction.sv:23-25` 的 fired-mask 宽度由 load base 加三类 pipe 数派生；
`lintsissue_agent_agent_pkg.sv` 显式 include `memblock_compile_params.svh`，避免 compilation unit 之间依赖宏副作用。

中文伪代码：xaction、driver 和 sequence 使用同一组 compile-time 数量计算 mask 宽度；package 在声明 interface/xaction 前主动引入宏定义，任何 profile 变更都在编译/初始化检查处暴露。

## 7. 与原计划的对齐和实现补充

### 7.1 已按原计划实现

- V2 split 字段路径、FTQ offset 宽度和 DUT-facing FuType 宽度适配。
- target/FuType/fuOpType/behavior 矩阵检查。
- queue-size pending 判断和 compile-time route scan 上限。
- 真实 fired-mask、candidate 覆盖检查、redirect/epoch partial fire 处理。
- vector 默认入口删除和 driver fail-fast。
- flow、源码分析、调用图及网页资产同步到当前 split issue 语义。

### 7.2 `IMPLEMENTATION_DELTA` 对照

| plan 中的补充标记 | 当前实现 | review 判断 |
|---|---|---|
| package include | `lintsissue_agent_agent_pkg.sv` 显式 include compile params | 必要的 compilation unit 隔离；不改行为 |
| launch epoch gate | driver 首次 send 前检查 flush/epoch | 防止过期 item 首次上 VIF；与计划边界一致 |
| confirmed fire 保留 | sequence 先 mark 实际 fired，再处理 abort/epoch | 防止已握手 item 被重复或漏记 |
| blocking mask 校验 | blocking 正常完成要求真实 mask 覆盖 candidate | 保留阻塞语义，去除全 1 假 fire |
| ready X/Z | valid 时 ready 未知直接 fatal | 将握手协议错误显式化 |
| no-progress 单位 | 日志写 issue-loop iteration | 只改善诊断含义，不改变退出条件 |
| stale item handle | 每轮 `req = null` | 防止 UVM output handle 残留 |
| split expansion guard | 初始化拒绝超过 3/2/2 | 与当前显式物理字段能力一致 |

未发现超出这些 delta 的隐藏运行期机制；没有新增 runtime plus，也没有改变 scheduler 主体仲裁或 recovery owner。

## 8. 文档同步检查

已同步：

- `AI_DOC/mem_ut_flow_doc/load_sta_std_issue_flow.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/issue_field_assigner.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/issue_queue_scheduler.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_dispatch_types.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_lintsissue_dispatch_sequence.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/seq_csr_common.md`
- `AI_DOC/web/memblock_dispatch_control_flow_callgraph.md` 及两个 `assets/app.js`
- 执行 plan 本身的状态、实现 delta 和原逻辑对比章节（归档前完成状态更新）

旧 `intIssue_0..6` 仅在 flow 的“旧版本说明”中保留为历史解释；当前有效字段和源码分析已改为
`issueLda/issueSta/issueStd`，不再把旧名字作为 V2 consumer。

## 9. 验证和残留风险

已执行：

```text
node --check AI_DOC/web/memblock_dispatch_control_flow_callgraph/assets/app.js
node --check AI_DOC/web/memblock_dispatch_control_flow_callgraph_enhanced/assets/app.js
git diff --check
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_dispatch_real_smoke mode=base_fun cfg=tc_dispatch_real_smoke
make eda_run tc=tc_dispatch_real_smoke ts=virtual_base_sequence mode=base_fun \
  cfg=tc_dispatch_real_smoke plus_file=../seq/plus_cfg seed=666666
```
中文说明：网页 JavaScript 和文档空白检查通过；VCS 编译已成功生成 `simv`，没有 SV compile error；
正确 preset 的 load smoke 已完成实际运行。

正确 preset 的运行已显式传入 `plus_file=../seq/plus_cfg`、`cfg=tc_dispatch_real_smoke` 和对应 testcase，
结果为 `TEST CASE PASSED`、`UVM_ERROR=0`、`UVM_FATAL=0`。唯一 `UVM_WARNING` 是 scalar-only profile
删除 vecissue default sequence 后，通用 vecissue sequencer 报告没有 default sequence；vector VIF 保持 idle，
该 warning 不表示 vector transaction 被发送。此前一次直接运行 simv 未传 `plus_file`，触发默认配置 100us
timeout，该次结果不作为本计划验收结果。

store smoke 的
`lsq_commit_handler.sv` `SQ deq pointer mismatch` 属于此前 pendingPtr/SQ deq 下游适配问题；本子计划没有
修改该 handler，也没有把该失败伪装成 split issue 已解决。后续应由 LSQ commit/SQ deq 专项继续处理。

## 10. 与原始 Plan 不一致的实现

执行前原始 plan 与最终实现存在两处有意差异，均已通过执行 plan 的 `IMPLEMENTATION_DELTA` 回写，不能
把回写后的 plan 当作原始方案来掩盖差异。

| 差异 | 原始 plan 逻辑 | 最终实现 | 原因与源码 |
|---|---|---|---|
| blocking fired-mask | blocking 可使用参数化 all-ones 表示完成 | driver 仍逐 port 记录真实 fire；sequence 要求真实 mask 完整覆盖 candidate | all-ones 会掩盖 port mapping 或 ready 采样缺口；`memblock_issue_dispatch_base_sequence.sv::send_issue_cycle()` |
| redirect/epoch 后的 confirmed fire | finish 后先检查 flush/epoch，存在整批跳过 marking 的描述 | 先按真实 mask 标记已确认 fire，再只取消未 fire candidate | DUT 已握手 item 不能因稍后观察到 redirect 而重新 issue；`send_issue_cycle()`、`mark_fired_items()` |

文字伪代码：blocking 返回后先核对 driver mask 与 candidate，一位不缺才允许正常完成；任意模式只按
真实 mask 更新 issue 状态。若同轮发生 redirect/epoch 变化，已经 fire 的 item 保留 dispatched，未 fire
item 留给 recovery/后续仲裁，禁止构造全 1 mask。

除以上两项外，未发现其它与执行前原始 plan 不一致的运行期实现。

## 11. Plan 未说明但 Coding 落实的细节

以下细节不是执行前原始 plan 的完整条目，coding 后已用 `IMPLEMENTATION_DELTA` 回写并完成 review：

| 补充细节 | 实现和原因 | 行为影响 |
|---|---|---|
| package 自包含 | `lintsissue_agent_agent_pkg.sv` 显式 include compile header，避免 compilation unit 宏副作用 | 无运行期行为变化 |
| 首次 launch gate | payload 首次上 VIF 前检查 `wait_ready`、flush 和 epoch；失败时清 mask/valid并明确发送 idle | 阻止过期请求首次 launch |
| ready X/Z | valid 为 1 时 ready 未知直接 fatal | 将协议未知值显式化，不改变合法握手 |
| stale item handle | 每轮 `try_next_item()` 前执行 `req = null` | 防止无 item 时复用旧句柄 |
| no-progress 单位 | 日志使用 issue-loop iteration，而非 DUT cycle | 仅修正诊断口径 |
| split expansion guard | 初始化拒绝超过当前显式 LDA/STA/STD=3/2/2 的 profile | 提前暴露 compile profile 与物理字段不一致 |

文字伪代码：package 加载时取得当前 compile profile；driver 每轮清空旧句柄，取到 dispatch item 后在首次
sample 边界检查是否仍可 launch；合法 launch 只接受确定的 ready；初始化阶段先检查物理展开能力；诊断
计数只报告 issue loop 停滞，不作为正常退出条件。

未发现除本节所列内容之外的 Plan 未说明实现细节。

## 12. 最终 review 结论

本 agent 的代码和文档对照结论：本子计划的字段适配、unsupported gate、真实握手和 no-progress 逻辑均已
落到计划列出的 owner；未发现主表、scheduler、replay/redirect、writeback/commit/deq 或 terminal 主体被
无意改写的问题。subagent 最后一轮已确认 `FINAL PASS`，执行 plan 已从 `undo` 移到 `do`，可以提交独立
commit。
