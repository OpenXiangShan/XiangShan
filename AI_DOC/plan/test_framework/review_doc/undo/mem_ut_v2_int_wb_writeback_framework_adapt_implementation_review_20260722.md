# V2 Int-WB 测试框架适配 Implementation Review

| 项目 | 内容 |
|---|---|
| 关联 Plan | `AI_DOC/plan/test_framework/plan/do/mem_ut_v2_int_wb_writeback_framework_adapt_execution_plan_20260708.md` |
| 目标版本 | V2，分支 `mem_ut_uvm_v2` |
| Review 范围 | V2 `writebackLda/Sta/Std` raw producer、key 归一化、STD real-WB 完成 owner、相关参数和活跃文档 |
| Review 日期 | 2026-07-22 |
| 当前状态 | 代码和文档已完成本轮修正，等待最终 subagent review 通过后归档 plan |

## 1. 结论摘要

本轮把 V2 split int writeback 接口按真实端口能力接入公共状态流：

- `writebackLda_0/1/2`、`writebackSta_0/1`、`writebackStd_0/1` 使用独立 `source_kind`，
  `port_id` 只表示类别内 lane，不再沿用 V3 全局 `intWriteback_0..6` 语义。
- LDA/STA 使用真实 ROB key，再从当前 active status/map 补齐 LQ/SQ、issue epoch 和 replay
  快照；STD 只有 ROB value 时固定探测两个 flag，并先按 STD 当前状态过滤候选，再要求唯一命中。
- STD issue fire 不再合成 `STD_FEEDBACK` pass；只有真实 `writebackStd_0/1` 经过 value-only
  反查和 ROB/SQ owner 校验后才能设置 `std_writeback/std_pass`。
- 为避免 flush 后误把旧 raw 绑定到新动态实例，状态表新增 target 级实例 flush epoch；
  raw sample epoch 在 monitor 采样拍冻结，adapter 按 target 实例校验。
- absent metadata 和各 split lane 不存在的 `exceptionVec` 位现在有 fail-fast capability guard。

本轮没有修改 pass/fail/terminal owner、RM/checker、主表生成、redirect 主仲裁或 LSQ deq
逻辑。真实 store smoke 中 STA/STD writeback 已成功，随后暴露的 SQ deq pointer mismatch
仍属于 LSQ MMIO/status/SQ deq 子计划，详见第 8 节。

## 2. 修改前后逻辑

### 2.1 旧逻辑

旧 monitor 把 V3 风格的全局 `port_id=0..6` 和一组可能不存在的 ROB/LQ/SQ key 直接放入
raw。adapter 按全局 port 推导 LOAD/STA/STD，并把 raw 中的 partial key 当作可完成事件。
STD 在 `MEMBLOCK_STD_REAL_WB_PASS_EN=0` 时还可以由 issue fire 构造 synthetic
`STD_FEEDBACK` pass，因此真实 STD writeback 丢失时主动 flow 仍可能提前继续。

这套行为有三个 V2 风险：

1. V2 split port 的字段能力不相同，统一 port 编号会把类别和 lane 混淆。
2. `writebackStd_0/1` 没有 ROB flag 和 SQ key，置零或沿用旧字段会生成误导身份。
3. key 失败时静默 drop 或 synthetic pass 会掩盖 DUT event 丢失，使状态表永远等待或错误
   通过。

### 2.2 新逻辑

```text
monitor 在 clocking block 采样真实 V2 split port
  -> builder 只构造一个 raw，不写 queue
  -> mon_data 按 LDA0/1/2、STA0/1、STD0/1 顺序各 push 一次
  -> adapter 校验 source/lane/metadata/exception capability
  -> LDA/STA 用 ROB key 查 active map，并按 target 实例 flush epoch 补 snapshot
  -> STD 对 flag=0/1 各做一次 active ROB probe，先过滤合法 STD candidate，再要求唯一
  -> normalize 校验 raw key 与 status/map owner 一致
  -> writeback handler 只接受真实 LOAD/STA/STD writeback
  -> STD 只有真实 writeback 才置 writeback/pass
```

该变化属于字段适配加局部身份关联功能增强；没有重写 writeback handler 的状态职责。

## 3. 源码修改审查

### 3.1 raw schema：source/lane、缺失 key 和 metadata provenance

源码位置：`mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv:23-66`，
对象：`memblock_int_wb_source_kind_e`、`dispatch_raw_int_wb_t`。

这段定义证明 raw 现在保存真实 split 类别、类别内 lane、采样 epoch、key 缺失原因、
metadata valid 标志和完整 24-bit exception 容器。字段宽度继续从
`MEMBLOCK_DUT_*_VALUE_W` 派生。

```systemverilog
typedef enum bit [1:0] {
    MEMBLOCK_INT_WB_SOURCE_INVALID    = 2'd0,
    MEMBLOCK_INT_WB_SOURCE_SCALAR_LDA = 2'd1,
    MEMBLOCK_INT_WB_SOURCE_STA        = 2'd2,
    MEMBLOCK_INT_WB_SOURCE_STD        = 2'd3
} memblock_int_wb_source_kind_e;

typedef struct {
    bit                           valid;
    memblock_int_wb_source_kind_e source_kind;
    int unsigned                  port_id;
    int unsigned                  sample_flush_epoch;
    bit                           key_needs_state_lookup;
    bit                           rob_value_only_without_flag;
    bit                           rob_valid;
    bit                           rob_flag;
    bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] rob_value;
    bit                           replay_inst_valid;
    bit                           flush_pipe_valid;
    bit                           trigger_valid;
    bit                           replay_inst;
    bit                           flush_pipe;
    bit [3:0]                     trigger;
    bit                           debug_is_mmio;
    bit                           debug_is_ncio;
    bit [23:0]                    exception_vec;
    longint unsigned              cycle;
} dispatch_raw_int_wb_t;
```
中文伪代码：

该结构承担“物理端口事实层”的职责。先把来源类别和类别内 lane 写入 raw；再保存采样
时的 flush epoch 和 cycle，避免 adapter 消费时重新猜时间。LDA/STA 的 ROB key 标为真实
有效，STD 则用 `rob_value_only_without_flag` 明确说明缺少 flag；不存在的 metadata 由
valid 标志和中性值共同表示。`exception_vec` 统一承载后续 fault handler 所需的位，但
adapter 还会按 lane mask 拒绝不存在的位。

`make_empty_raw_int_wb()` 在所有字段上置中性值：`source_kind=INVALID`、各 key valid 清零、
`trigger=4'hf`、exception 全零。这样每次 builder 都从干净对象开始，不会继承上一 lane 的
payload。

源码位置：同文件 `make_empty_raw_int_wb():158-190`，功能：为每个 raw producer 提供完整
中性初值。

```systemverilog
item.valid                       = 1'b0;
item.source_kind                 = MEMBLOCK_INT_WB_SOURCE_INVALID;
item.key_needs_state_lookup      = 1'b0;
item.rob_value_only_without_flag = 1'b0;
item.replay_inst_valid           = 1'b0;
item.flush_pipe_valid            = 1'b0;
item.trigger_valid               = 1'b0;
item.replay_inst                 = 1'b0;
item.flush_pipe                  = 1'b0;
item.trigger                     = 4'hf;
item.exception_vec               = '0;
```
中文伪代码：

每次构造 raw 先清 valid、来源、key lookup 标志和所有 metadata valid；再把不存在的控制
字段置为中性值、exception 位图清零。调用者若要表示真实 event，必须随后显式写 valid、
source、lane 和真实 payload，不能依靠上一次 raw 的残留值。

### 3.2 monitor：唯一 raw queue push owner

源码位置：`mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_monitor.sv:39-211`，
函数：`build_raw_int_wb_from_v2_port()`。

builder 只读当前 clocking block 并返回 raw，不触碰共享 queue。它按 source/lane 读取 valid，
然后复制该 lane 的真实 ROB、metadata 和 exception 位；STD 只复制 ROB value。

```systemverilog
raw = memblock_sync_pkg::make_empty_raw_int_wb();
case (source_kind)
    memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_SCALAR_LDA: begin
        // valid 来自 writebackLda_0/1/2
    end
    memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STA: begin
        // valid 来自 writebackSta_0/1
    end
    memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STD: begin
        // valid 来自 writebackStd_0/1
    end
    default: `uvm_fatal("INT_WB_MON", "invalid source/lane")
endcase
if (sampled_valid === 1'b0) return raw;
if (sampled_valid !== 1'b1) `uvm_fatal("INT_WB_MON", "writeback valid is X/Z");
raw.valid = 1'b1;
raw.source_kind = source_kind;
raw.port_id = port_id;
raw.sample_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
raw.key_needs_state_lookup = 1'b1;
```
中文伪代码：

该 helper 的功能是把一个真实 V2 lane 转成一个事实 raw。先创建空 raw；按 source/lane
选择对应 valid；valid 为零时返回空对象，不入队；valid 为 X/Z 时立即 fatal；valid 为一
时冻结来源、lane、采样 epoch 和 cycle，再复制该 lane 实际存在的字段。调用者只能拿返回
值继续处理，不能在这里修改状态表或 active map。

源码位置：同文件 `mon_data():213-620`，功能：按固定端口顺序把 valid raw 入共享 queue。

```systemverilog
// mon_data 是唯一 raw queue push owner。
raw_int_wb = build_raw_int_wb_from_v2_port(
    memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STD, 0);
if (!raw_int_wb.valid) `uvm_fatal("INT_WB_MON", "builder dropped valid STD0 event");
memblock_sync_pkg::push_raw_int_wb(raw_int_wb);
```
中文伪代码：

`mon_data()` 每拍按 `LDA0 -> LDA1 -> LDA2 -> STA0 -> STA1 -> STD0 -> STD1` 检查；无效 lane
直接跳过；有效 lane 调 builder，若 builder 返回无效则 fatal，随后恰好调用一次
`push_raw_int_wb()`。因此不会重复入队，也不会让有效 event 被静默吞掉。该 task 不解析 uid、
不写 status、不推进 pass/fail/terminal。

### 3.3 target 级动态实例 flush epoch

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/status_transaction.sv:58-76,130-145,190-260`，
对象：`*_instance_flush_epoch*` 和 getter/setter。

```systemverilog
bit                 active_instance_flush_epoch_valid;
int unsigned        active_instance_flush_epoch;
bit                 load_instance_flush_epoch_valid;
bit                 sta_instance_flush_epoch_valid;
bit                 std_instance_flush_epoch_valid;
int unsigned        load_instance_flush_epoch;
int unsigned        sta_instance_flush_epoch;
int unsigned        std_instance_flush_epoch;
```
中文伪代码：

状态表为每个 uid 保存最近 issue 动态实例的 flush epoch；generic 字段保留最近一次 uid
级快照，三个 target 字段分别保存 LOAD、STA、STD 的实例版本。reset 时全部清零；真实
issue 时由 setter 设置对应 target valid 和 epoch；redirect 清理时统一失效。adapter 读取
target getter，不把不同拍 issue 的 STA/STD 误当成同一个实例。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv:440-460`，
函数：`mark_issue_snapshot()`。

```systemverilog
status.set_target_issue_epoch(issue_target, issue_epoch);
status.set_target_instance_flush_epoch(
    issue_target,
    memblock_sync_pkg::dispatch_flush_epoch);
status.issue_killed = 1'b0;
register_uid_tlb_record_on_issue(uid);
```
中文伪代码：

发射被 scheduler 确认后，先保存原有 target issue epoch，再把当前全局 flush epoch 固定到
该 target 的实例快照，清除 issue_killed 并登记已有 TLB issue record。后续 raw event 的
sample epoch 不能早于这个 target 快照；未被 redirect 杀死的老指令可以在更晚的全局 epoch
写回。redirect 清除 dispatch result 时同时清空实例 epoch，防止旧 event 绑定到 reissue
后的新实例。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv:805-835`，
函数：`clear_uid_dispatch_result()`，功能：redirect/replay 时清理旧动态实例。

```systemverilog
status.issue_killed     = 1'b1;
status.redirect_pending = 1'b0;
status.rob_commit       = 1'b0;
status.lsq_deq          = 1'b0;
status.terminal_done    = 1'b0;
status.exception_vec    = '0;
status.clear_target_instance_flush_epochs();
```
中文伪代码：

redirect 命中旧实例时先清除 dispatched、writeback、pass、fault 等过程状态并标记 killed；
同时清掉 commit/deq/terminal 和异常快照，最后调用实例 epoch 清理函数。这样旧 raw 即使
还留在 monitor queue，也不能被当作重发实例的当前 writeback。

### 3.4 LDA/STA snapshot attach 和 STD 候选过滤

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv:80-218`，
函数：`fill_current_issue_snapshot()`。

该函数是当前 int-WB 的 snapshot owner。它检查 active、terminal、flush、kill、redirect、
target dispatched、ROB canonical key、target issue epoch，以及 LQ/SQ active map owner；
通过后才把 uid、key、epoch、replay sequence 写回 event。

```systemverilog
status = data.get_status(uid);
if (!status.active || status.terminal_done || status.flushed ||
    status.issue_killed || status.redirect_pending) begin
    if (strict_candidate) `uvm_fatal("INT_WB_ATTACH", "ROB candidate is not current");
    return 1'b0;
end
if (!data.target_dispatched(status, wb_event.target)) begin
    if (strict_candidate) `uvm_fatal("INT_WB_ATTACH", "target was not dispatched");
    return 1'b0;
end
if (sample_flush_epoch < active_instance_flush_epoch) begin
    if (strict_candidate) `uvm_fatal("INT_WB_ATTACH", "raw belongs to an older instance");
    return 1'b0;
end
```
中文伪代码：

先读取候选 uid 的 status；如果它已不 active、已终态、已被 flush/kill/redirect，则严格选中
路径 fatal，候选筛选路径返回无效。再确认对应 target 确实已经 dispatched。若 raw epoch
晚于全局 epoch，说明 producer/队列不可信而 fatal；若 raw epoch 早于该 target 的实例 epoch，
候选路径排除、最终选中路径 fatal；更晚 epoch 仍允许未被 kill 的老指令写回。之后比较
canonical ROB key，并确认 LOAD 的
LQ owner 或 STA/STD 的 SQ owner 仍映射到同一 uid，最后补齐 event 的全部 correlation 字段。

源码位置：同文件 `probe_std_candidate():286-310` 和
`resolve_std_uid_by_rob_value_only():312-349`。

```systemverilog
hit0 = probe_std_candidate(wb_event, 1'b0, raw.rob_value,
                           1'b1, raw.sample_flush_epoch, uid0, candidate0);
hit1 = probe_std_candidate(wb_event, 1'b1, raw.rob_value,
                           1'b1, raw.sample_flush_epoch, uid1, candidate1);
if (hit0 && hit1) `uvm_fatal("INT_WB_STD_KEY", "two valid STD candidates");
if (!hit0 && !hit1) `uvm_fatal("INT_WB_STD_KEY", "zero valid STD candidates");
wb_event = hit0 ? candidate0 : candidate1;
```
中文伪代码：

STD value-only event 先构造两个可能的 ROB flag。每个 flag 只做一次 active ROB map 查询；
命中后调用 `fill_current_issue_snapshot()`，把非 STD、未 dispatched、无 SQ mapping、旧
实例等命中过滤掉。过滤后的合法候选为零或两个时，在任何 status 修改前 fatal；恰好一个
时复制该候选的完整 uid/ROB/SQ snapshot。整个过程最多两个 map probe，不扫描主表。

### 3.5 capability、metadata 和 exceptionVec guard

源码位置：`dispatch_monitor_event_adapter.sv:351-416`，函数：
`check_raw_int_wb_capability()`。

```systemverilog
case (raw.source_kind)
    memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_SCALAR_LDA: begin
        if (raw.port_id > 2 || !raw.rob_valid || raw.rob_value_only_without_flag ||
            !raw.replay_inst_valid || !raw.flush_pipe_valid || !raw.trigger_valid)
            `uvm_fatal("INT_WB_CAP", "invalid LDA capability");
        allowed_exception_mask = raw.port_id == 0 ? 24'hA8A0F8 : 24'h282038;
    end
    memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STA: begin
        if (raw.port_id > 1 || !raw.rob_valid || raw.replay_inst_valid ||
            raw.flush_pipe_valid != (raw.port_id == 0) || raw.replay_inst ||
            (!raw.flush_pipe_valid && raw.flush_pipe))
            `uvm_fatal("INT_WB_CAP", "invalid STA capability");
        allowed_exception_mask = raw.port_id == 0 ? 24'hffffff : 24'h8880C8;
    end
    memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STD: begin
        if (raw.port_id > 1 || raw.rob_valid || !raw.rob_value_only_without_flag ||
            raw.replay_inst || raw.flush_pipe || raw.trigger != 4'hf ||
            raw.exception_vec != 24'b0)
            `uvm_fatal("INT_WB_CAP", "invalid STD capability");
    end
endcase
if ((raw.exception_vec & ~allowed_exception_mask) != 24'b0)
    `uvm_fatal("INT_WB_CAP", "exception bit is absent on this V2 lane");
```
中文伪代码：

根据 source/lane 检查合法端口范围和 key 能力；STA1/STD 的 absent metadata 只能保持中性值，
不能用非零值伪装成真实字段。然后选择该 lane 允许的 exception 位掩码：LDA0、LDA1/2、
STA0、STA1 和 STD 分别使用 V2 实际字段集合；raw 携带任何不存在的位就 fatal。这样即使
未来有第二个 raw producer，也不能绕过接口能力边界。

`check_raw_int_wb_metadata()` 继续拒绝当前不支持的 SCALAR_LDA replayInst、普通 LDA
flushPipe、Debug/Trace trigger 和未实现的 STA0 CBO flush；它只在 capability 校验之后、
状态落表之前运行。

### 3.6 conversion、normalize 和 real-WB owner

源码位置：`dispatch_monitor_event_adapter.sv:456-535`，函数：
`normalize_v2_int_wb_key()`、`convert_raw_int_wb()`。

```systemverilog
case (raw.source_kind)
    memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_SCALAR_LDA: begin
        wb_event.source = MEMBLOCK_WB_EVENT_SOURCE_LOAD_WB;
        wb_event.target = MEMBLOCK_ISSUE_TARGET_LOAD;
        wb_event.has_rob = raw_rob_to_key(raw.rob_valid, raw.rob_flag,
                                          raw.rob_value, wb_event.rob_key);
        attach_current_issue_snapshot(wb_event, 1'b0, 1'b1,
                                      raw.sample_flush_epoch);
    end
    memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STA: begin
        wb_event.source = MEMBLOCK_WB_EVENT_SOURCE_STORE_WB;
        wb_event.target = MEMBLOCK_ISSUE_TARGET_STA;
        wb_event.has_rob = raw_rob_to_key(raw.rob_valid, raw.rob_flag,
                                          raw.rob_value, wb_event.rob_key);
        attach_current_issue_snapshot(wb_event, 1'b0, 1'b1,
                                      raw.sample_flush_epoch);
    end
    memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STD: begin
        wb_event.source = MEMBLOCK_WB_EVENT_SOURCE_STORE_WB;
        wb_event.target = MEMBLOCK_ISSUE_TARGET_STD;
        resolve_std_uid_by_rob_value_only(raw, wb_event);
    end
endcase
wb_event.real_wb_valid = 1'b1;
```
中文伪代码：

先为 raw 创建空 event，拒绝无效 raw；再执行 capability 和 metadata guard。LDA/STA 设定
真实来源和 target，带真实 ROB key 进入唯一 snapshot owner；STD 不制造 flag，进入双候选
value-only resolver。归一化阶段再次确认 source、target、ROB、LQ/SQ、issue epoch 和 replay
sequence 完整且与 raw 一致；成功后把 `real_wb_valid` 置一。event 随后才允许进入 batch
handler/writeback handler，失败不会静默生成部分 `has_*` 事件。

源码位置：`mem_ut/ver/ut/memblock/seq/base_seq_help/writeback_status_handler.sv:50-70,138-174,210-225`，
函数：`target_real_wb_pass_enabled()`、`handle_issue_feedback_event()`、`handle_event()`。

```systemverilog
function bit target_real_wb_pass_enabled(input memblock_issue_target_e target);
    if (!seq_csr_common::is_initialized()) return 1'b0;
    return target == MEMBLOCK_ISSUE_TARGET_STA &&
           seq_csr_common::get_sta_real_wb_pass_en();
endfunction

if (wb_event.target == MEMBLOCK_ISSUE_TARGET_STD ||
    wb_event.source == MEMBLOCK_WB_EVENT_SOURCE_STD_FEEDBACK) begin
    `uvm_fatal("WB_STATUS", "STD issue feedback cannot complete target");
end
```
中文伪代码：

handler 只保留 STA 的既有 real-WB 兼容开关；STD 不再读取或切换 runtime 参数。若任何
STD IQ feedback 试图进入完成处理，立即 fatal；STA feedback 仍可按原配置只记 feedback
success 或走 STA 兼容 pass。真实 `STORE_WB` event 则沿用原有 fault/normal pass 分支，
由 `mark_target_fault()` 或 `mark_target_normal_pass()` 更新 target 状态。

### 3.7 删除 STD synthetic pass

源码位置：

- `mem_ut/ver/ut/memblock/seq/base_seq/memblock_issue_dispatch_base_sequence.sv:13-38,303-310`
- `mem_ut/ver/ut/memblock/env/plus.sv`
- `mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv`
- `mem_ut/ver/ut/memblock/seq/plus_cfg/*.cfg`

修改前，`mark_fired_items()` 在 STD issue fire 后可能调用
`submit_issue_accept_pass()`，再由 `writeback_status_handler` 把 synthetic feedback 当作
STD pass。修改后 issue fire 只调用 scheduler 的 dispatched/snapshot 更新，不再构造该事件；
旧参数、getter、相关 cfg 行和 helper 均删除。这样真实 writeback 缺失会停在
`std_dispatched=1,std_pass=0`，但 raw key 无法归一化时会立即 fatal，不会静默卡住。

源码位置：`memblock_issue_dispatch_base_sequence.sv:287-307`。

```systemverilog
fire_marked = issue_sched.mark_issue_fire(fired_items[idx]);
if (!fire_marked) begin
    `uvm_warning(get_type_name(), $sformatf("fire item became stale uid=%0d", fired_items[idx].uid));
end
```
中文伪代码：

对 driver 返回 fired mask 的每个真实 fire item，调用 scheduler 记录 issue snapshot、删除
issue queue 项、清 queued、置 dispatched。若 item 已被 redirect/flush 变成 stale，只报警而
不伪造完成事件；后续由真实 `writebackStd` 决定 STD 是否 pass。

## 4. 修改类型和原逻辑对比

| 修改点 | 类型 | 原逻辑 | 原逻辑问题 | 当前逻辑 |
|---|---|---|---|---|
| `source_kind/port_id` | 字段适配 | V3 全局 0..6 隐含 target | V2 split 类别和 lane 被混淆 | source 表示 LDA/STA/STD，port 只表示类别内 lane |
| raw metadata valid | 字段适配 | 缺失字段和真实零值难区分 | absent 字段可能被误当真实控制 | `*_valid` + 中性值，adapter 做 capability guard |
| raw exceptionVec | 字段适配 | 端口字段可能统一复制 | 不存在的 bit 会污染 fault 判断 | 按 V2 lane 允许 mask 复制和校验 |
| raw queue owner | 逻辑收敛 | builder/port 分支可能重复 push | 重复事件或有效事件丢失 | `mon_data()` 唯一 push owner |
| LDA/STA key | 功能适配 | 缺 LQ/SQ 时直接 pass/伪造 key | uid 和资源 owner 可能错配 | ROB -> active status/map snapshot -> normalize |
| STD value-only key | 新增关联功能 | 置零 flag 或沿用 V3 SQ key | 无法证明真实 owner | 两个 ROB flag probe，过滤后唯一命中，从 status 补 SQ |
| 实例 flush epoch | 新增状态字段/校验 | 只用消费时全局 epoch | flush 后可能误绑旧/新动态实例 | issue fire 保存 target epoch，raw 按 target epoch 校验 |
| STD completion owner | 功能语义修改 | issue accept 可 synthetic pass | 掩盖真实 STD WB 丢失 | 仅真实 `writebackStd_0/1` 设置 pass |
| STD failure policy | 错误处理修改 | warning/drop 或参数切换 | 主动 flow 可能永久等待 | 零/双候选、owner/key 不一致统一 fatal |
| `MEMBLOCK_STD_REAL_WB_PASS_EN` | 参数删除 | runtime 切换两套完成 owner | 物理 V2 语义不稳定 | 从 plus/getter/cfg/helper 全部删除 |
| writeback handler 主职责 | 保持不变 | handler 更新 target pass/fault | 不应迁移 owner | 仍由 handler 更新状态；adapter 只负责事实归一化 |

## 5. 与原始 Plan 对齐检查

### 5.1 已按 Plan 完成

- V2 split source/lane 和 raw factory：完成。
- `mon_data()` 唯一 raw push owner：完成。
- LDA/STA 真实 ROB key、current snapshot、LQ/SQ owner 校验：完成。
- STD ROB value-only 双 flag 反查、SQ 补齐和 all-or-fatal：完成；并按 review 增加了候选先过滤。
- exceptionVec、replayInst、flushPipe、trigger 的分层 capability/unsupported guard：完成。
- 删除 STD synthetic pass、runtime 参数和 issue fire helper：完成。
- 不改变 writeback handler、pass/fail/terminal owner、RM/checker/coverage：完成。

### 5.2 实现与 Plan 不一致项

本节只记录执行前原始 Plan 与实际 coding 的差异；所有差异均已在执行 plan 的
`IMPLEMENTATION_DELTA` 中标记。

1. **STD 候选判定顺序**
   - Plan 原意是两个 flag probe 后判唯一，再验证 snapshot。
   - 实际实现先调用 `probe_std_candidate()` 完成 target/status/SQ/epoch 过滤，再对“合法候选”
     判零/一/双命中。
   - 原因是另一 flag 可能命中 active 但非 STD uid；直接按 active hit 判双命中会误 fatal。
   - 结论：保留实际实现，行为更严格且仍满足最多两个 map probe。

2. **实例 epoch 由单一字段细化为 target 级字段**
   - Plan 使用 `active_instance_flush_epoch_valid/active_instance_flush_epoch` 描述 uid 级
     快照。
   - 实际实现保留 generic 最近 issue 字段，同时增加 LOAD/STA/STD 各自 valid/value 和 getter。
   - 原因是同一 uid 的 STA、STD 可能不同拍 issue，单字段会被后发 target 覆盖。
   - 结论：target 级实现是必要的语义细化，不改变原有 issue epoch/replay_seq owner。

3. **验证日志重新生成**
   - 首轮编译/smoke 日志早于最后一次修正，不能作为最终源码证明。
   - 本轮已在最后一次 `sample_flush_epoch` 比较修正后重新执行 VCS compile 和 store smoke，结果见第 7 节。

### 5.3 Plan 未说明但 Coding 落实的细节

- `check_raw_int_wb_capability()` 使用 lane-specific exception mask，保护未来 raw producer
  不越过接口字段能力。
- `attach_current_issue_snapshot()` 的 value-only 分支也复用过滤后的候选 helper，避免未来
  IQ 专项调用该 API 时重新引入 active-hit 双命中误判。
- status reset、redirect clear 和 issue snapshot 三个生命周期点均维护实例 epoch，避免只
  增加字段而没有清除/重建 owner。
- 活跃参数、flow、analysis 和 web callgraph 已同步删除旧 STD synthetic pass 操作说明。

## 6. 文档同步检查

已同步：

- `AI_DOC/mem_ut_flow_doc/load_sta_std_issue_flow.md`
- `AI_DOC/mem_ut_flow_doc/writeback_function_call_flow.md`
- `AI_DOC/project_management/mem_ut_parameter_management.md`
- `mem_ut/ver/ut/memblock/rule/memblock_parameter_management_rule.md`
- `mem_ut/ver/ut/memblock/rule/plus_demo_migration_plan.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/status_transaction.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_dispatch_types.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_dispatch_real_mixed_smoke_sequence.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/dispatch_monitor_event_adapter.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/memblock_sync_pkg.md`
- `AI_DOC/analysis/source_sv/dispatch_framework_sv/writeback_status_handler.md`
- `AI_DOC/mem_ut_flow_doc/replay_flow.md`
- `AI_DOC/analysis/interface/v2/agents/int_writeback_agent.md`
- `AI_DOC/web/memblock_dispatch_control_flow_callgraph.md`
- `AI_DOC/web/web_assets/memblock_dispatch_doc.js`
- 两份 dispatch callgraph `assets/app.js`

活跃文档不再把 `MEMBLOCK_STD_REAL_WB_PASS_EN=0` 或 issue-accept synthetic STD pass 描述为
可用路径。历史 plan/review 若保留旧行为，只表示历史事实，不作为当前配置说明。

## 7. 验证结果

### 7.1 静态检查

执行：

```text
git diff --check
```

结果：通过，无 whitespace error。

同时检查旧符号：

```text
rg -n "submit_issue_accept_pass|item_needs_issue_accept_pass|make_issue_accept_pass_event|MEMBLOCK_STD_REAL_WB_PASS_EN" AI_DOC/web AI_DOC/mem_ut_flow_doc AI_DOC/analysis/source_sv/dispatch_framework_sv AI_DOC/project_management mem_ut/ver/ut/memblock
```

结果：活跃代码和说明已删除旧 helper；保留的参数名只出现在“明确说明该参数已删除”的
迁移/管理文档中。

### 7.2 远端编译

执行：

```text
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
```

结果：通过。第一次增量运行曾命中生成数据库 `tdc.sdb` 损坏；确认无残留 VCS/simv 进程后，
只删除 `base_fun/exec` 和 `base_fun/partitionlib` 生成目录并重新构建。最后一次远端 VCS/Verdi
elaboration 退出码为 0，报告 `0 error(s), 0 warning(s)`；日志为
`mem_ut/ver/ut/memblock/sim/base_fun/log/vcs_compile_rtl.log`，完成时间为
2026-07-22 13:43（本机时区）。

### 7.3 基础 sanity

执行：

```text
make eda_run tc=tc_sanity mode=base_fun
```

结果：`TEST CASE PASSED`，`UVM_ERROR=0`、`UVM_FATAL=0`。日志中的 133 条 `UVM_WARNING`
来自默认 active LSQ sequence 在 `main_trans_num=0` 时周期性报告“等待主表”，属于现有 sanity
配置行为；仿真在 1.4ms 正常结束。

### 7.4 真实 store writeback smoke

执行：

```text
make eda_run tc=tc_dispatch_real_store_wb_smoke mode=base_fun cfg=tc_dispatch_real_store_wb_smoke
```

结果：

- 290.3ns：STA/STD issue fire 均被 driver 采样。
- 292.8ns：真实 STD writeback 进入 handler，设置 STD normal pass。
- 302.8ns：STA IQ feedback 只记录 feedback success，没有提前取代 real-WB。
- 312.8ns：真实 STA writeback 进入 handler，设置 STA normal pass。
- 320.3ns：ROB commit 正常识别 store。
- 327.8ns：既有 `lsq_commit_handler` 报：

```text
DUT sqDeq start flag=1 value=55 mismatches software SQ head flag=0 value=0 count=1
```

该失败发生在 int-WB 已经完成之后，属于 V2 LSQ count-only/deq pointer owner 子计划，
不是本轮 STD key/real-WB 链路失败。本轮没有越界修改 SQ deq 逻辑。该 testcase 的 make
包装在 fatal 后仍返回 shell 退出码 0，因此不能把命令退出码当作 testcase 通过依据；以日志中的
`UVM_FATAL` 和后续 `SQ deq` 诊断为准。

## 8. 剩余风险和边界

### 8.1 已知下游风险

SQ deq pointer mismatch 需要由 LSQ MMIO/status/SQ deq 子计划处理，重点检查：

- `sqDeq` count 与 pointer 的 V2 宽度/语义；
- `scommit`、pendingPtr 和 SQ deq 的时序解耦；
- store commit 后经过 SBuffer/uncache 才产生 SQ deq 的延迟关系；
- software SQ head 与 DUT `sqDeq` output 的 owner 对账。

本 int-WB plan 不应把该问题改成“STD writeback 失败”，否则会混淆两个 owner。

### 8.2 当前明确不支持

- HybridUnit `writebackHyuLda/Sta` 和 replayInst 合法运行路径；当前 profile `HyuCnt=0`。
- Debug/Trace trigger、STA0 CBO flushAfter 的完整状态收口。
- STD backend replay feedback；STD feedback 只作为严格拒绝防线。

### 8.3 正常主流程保持不变

主表、LSQ admission、issue candidate 选择、redirect-first batch 仲裁、writeback handler
状态落表、commit/deq/terminal owner 均未在本专项改变。新增逻辑只在 V2 raw 事实归一化和
动态实例身份校验边界生效。

## 9. 最终 Review 状态

本 agent 已完成代码、文档、编译和专项 smoke 的独立检查；首轮 subagent review 发现的
STD 候选过滤、target 实例 epoch、metadata guard 和文档/验证问题已经修复。后续只读 subagent
复核结果为 `FINAL PASS`，确认 V2 source/lane、唯一 raw push、LDA/STA snapshot、STD value-only
唯一候选、target epoch、capability guard、real-WB owner 和下游 SQ deq 边界均无遗漏。

最后一次干净远端编译于 2026-07-22 完成，VCS/Verdi 结果为 `0 error(s), 0 warning(s)`；随后
真实 store smoke 在 327.8ns 观察到既有 `lsq_commit_handler` 的 SQ deq pointer mismatch。
该现象发生在 STD/STA real-WB 和 ROB commit 成功之后，仍归属 LSQ MMIO/status/SQ deq 子计划，
不能回写为本专项 writeback 失败。review 文档按规则继续保留在 `review_doc/undo`，等待用户
后续统一归档 review。
