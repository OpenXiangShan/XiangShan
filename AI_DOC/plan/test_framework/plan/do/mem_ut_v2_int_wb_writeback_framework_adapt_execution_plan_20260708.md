# mem_ut V2 Int Writeback 测试框架适配最终 Coding Plan

| 项目 | 内容 |
|---|---|
| 状态 | coding 已完成，review 通过，待归档到 `plan/do` |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| V2 接口权威 | `build_memblock/rtl/MemBlock.sv` |
| 测试框架入口 | `io_mem_to_ooo_int_wb_agent_agent_monitor::mon_data()`、`dispatch_monitor_event_adapter::convert_raw_int_wb()` |
| 适配原则 | 只重构 V2 split writeback raw producer 和 adapter key 归一化，不改变 writeback handler 主职责、RM/checker/coverage |
| 创建/修订日期 | 2026-07-20 |

## 1. 范围与边界

本 plan 只整理 V2 `writebackLda/Sta/Std` split writeback 进入公共状态流时需要解决的问题。每个问题均说明修改原因、最终方案、修改的原有逻辑和可直接 coding 的文字伪代码。

当前权威 `build_memblock/rtl/MemBlock.sv` profile 没有
`writebackHyuLda/writebackHyuSta` 顶层端口，也没有 `HybridUnit` 实例，因此当前 elaboration
边界为 `HyuCnt=0`。本 plan 中的 `SCALAR_LDA` 只表示
`writebackLda_0/1/2`，不包含可选的 `writebackHyuLda`。

本轮支持范围：

- V2 scalar `writebackLda_0/1/2`、`writebackSta_0/1`、`writebackStd_0/1` raw event 保真采样。
- raw event 同时保留 `source_kind` 与 kind 内 `port_id`。
- SCALAR_LDA/STA 使用真实 ROB key 加 current issue snapshot 补齐 UID、target key、`issue_epoch/replay_seq`。
- STD 只有 `robIdx_value` 时，通过两个 ROB flag 的 active map 固定探测补齐完整 ROB/SQ key。
- STD 采用单一严格完成语义：`issueStd fire` 只表示 DUT 接收，只有真实
  `writebackStd` 才能设置 `std_writeback/std_pass`。
- SCALAR_LDA `replayInst`、AMO owner、STA/STD metadata 和缺 key 场景采用分层 fail-fast 策略。

本轮不支持：

- 不从 connect 层猜常量来伪造缺失 key。
- 不把 RTL 内部 `_inner_stdExeUnits_*_sqIdx_*` wire 当作当前 top-level STD SQ key 来源。
- 不新增历史 event 恢复 API、第二套 issue-generation 状态或全表扫描反查。
- 不采样 `writebackHyuLda/writebackHyuSta`，不实现 HybridUnit replay 适配，也不把未来
  `HYU_LDA` 合并进当前 `SCALAR_LDA` source kind。
- 不保留 `STD_REAL_WB_PASS_EN` 兼容开关，不在 `issueStd fire` 后合成
  `STD_FEEDBACK/iq_feedback_valid` pass。
- 不实现 RM、checker、scoreboard 或 coverage。
- 不改变 `writeback_status_handler` 的主职责和 pass/fail/terminal owner。

## 执行中补充/修正（IMPLEMENTATION_DELTA）

### [IMPLEMENTATION_DELTA] 公共 current issue snapshot API 的 owner 前移

来源：总控 plan 要求 int-WB 先于 IQ feedback/replay 执行，而本 plan 初稿在
SCALAR_LDA/STA 归一化处引用了后续 IQ 专项定义的
`attach_current_issue_snapshot()`。

原 plan：由 IQ 专项先定义该 helper，int-WB 在后续阶段调用。

实现调整：本专项建立唯一的公共 `attach_current_issue_snapshot()` API 及其 owner，先完成
ROB/STD 相关的 snapshot 校验、UID/target key/`issue_epoch`/`replay_seq` 补齐；IQ 专项只在
后续扩展该 API 的 STA SQ-only 分支，不得重新定义同名函数、第二个 active-map owner 或
第二套 generation 状态。

原因：按总控执行顺序 coding 时，int-WB 不能依赖尚未落地的 IQ 源码接口；同时必须保持
current issue generation、active map 和 key 生命周期只有一个权威来源。

影响范围：`dispatch_monitor_event_adapter.sv`、`common_data_transaction.sv` 及后续 IQ
专项的接口合同。该调整不改变 raw 字段、STD value-only 双 flag 探测、strict real-WB
完成 owner 或 pass/fail/terminal 的职责边界。

### [IMPLEMENTATION_DELTA] STD 候选先过滤后判唯一

来源：首轮 review 发现只按 active ROB map 的两个 flag hit 判双命中，会把另一 flag 上的
非 STD uid 误判为两个合法 STD 候选。

原 plan：对 `{flag=0,value}` 和 `{flag=1,value}` 做 probe 后要求唯一命中，再补齐 STD
snapshot。

实现调整：新增 `probe_std_candidate()`。每个 flag 命中 active ROB 后，先检查该 uid 的
`std_dispatched`、active/SQ mapping、terminal/flush/kill、ROB/SQ owner、target issue epoch
和 target instance flush epoch；只有通过这些检查的候选才参与零/一/双候选判定。最终选中的
候选沿用同一份 snapshot，不重复猜测 key。

原因：ROB value 相同但 flag 不同的 active uid 可能属于 LOAD/STA 或已经失效的动态实例，
不能把“active ROB 命中”直接等同于“合法 STD writeback owner”。

影响范围：`dispatch_monitor_event_adapter.sv`；仍然只执行两个 active ROB probe，不扫描
主表或完整 status 表；零/双合法候选仍在状态修改前 `uvm_fatal`。

### [IMPLEMENTATION_DELTA] 记录 target 级动态实例 flush epoch

来源：首轮 review 发现仅将 raw sample epoch 与消费时全局
`dispatch_flush_epoch` 比较，无法证明 raw 属于当前 target 的动态 issue instance，且会把
flush 前采样、flush 后 drain 的事件误判为 fatal。

原 plan：STD/LOAD/STA 反查时使用 `active_instance_flush_epoch_valid/active_instance_flush_epoch`
验证 raw sample provenance。

实现调整：`status_transaction` 增加 generic 最近 issue 快照和 LOAD/STA/STD target 级
`*_instance_flush_epoch_valid/*_instance_flush_epoch`；`common_data_transaction::mark_issue_snapshot()`
在真实 fire 边界记录当前 `dispatch_flush_epoch`，redirect 清理时统一失效。adapter 按
`get_target_instance_flush_epoch()` 与 raw sample epoch 比较，future epoch 仍 fatal；只有
sample epoch 早于 target 实例 epoch 才视为旧实例，未被 redirect 杀死的老指令可以在更晚的
全局 epoch 写回。候选不匹配时先过滤，最终选中项不匹配时 fatal。

原因：同一 uid 的不同 target 可能在不同拍 issue，单一 uid epoch 会覆盖另一个 target；
target 级字段保留原有 target issue epoch 的粒度，避免跨 target 误关联。

影响范围：`status_transaction.sv`、`common_data_transaction.sv`、
`dispatch_monitor_event_adapter.sv` 及 status 分析文档；不改变 `dynamic_epoch/replay_seq`
的既有 owner。

### [IMPLEMENTATION_DELTA] 完整 absent metadata/exception capability guard

来源：首轮 review 发现 capability valid flag 已校验，但 absent 的 `replayInst/flushPipe`
非中性值以及不存在于某 lane 的 exceptionVec 位没有 fail-fast 检查。

原 plan：不存在的字段保持中性默认值，并按真实 port 位图采样 exceptionVec。

实现调整：`check_raw_int_wb_capability()` 增加 STA1/STD absent metadata 中性值检查，并按
LDA0/LDA1-2/STA0/STA1/STD 的允许 bit mask 拒绝不存在的 exceptionVec 位。

原因：当前 monitor factory 虽然会生成中性值，但 raw struct 可能由未来 producer 或测试
注入构造；adapter 必须在状态更新前保护物理接口能力边界。

影响范围：`dispatch_monitor_event_adapter.sv`；不改变真实 exceptionVec bit 的复制或 fault
handler 逻辑。

执行 coding 前必须确认：

```bash
test -e build_memblock/rtl/MemBlock.sv
test -e build_memblock/rtl/filelist.f
```

## 2. 问题一：V2 split writeback raw 缺少 source/lane/provenance 权威字段

### V2 问题

旧 raw 逻辑容易沿用 V3 `intWriteback_0..6` 的全局 port 编码，并直接把每个 raw valid 解释成有完整 ROB/LQ/SQ key。V2 实际是 split 端口：

```text
writebackLda_0/1/2
writebackSta_0/1
writebackStd_0/1
```

`writebackStd_0/1` 当前只有 `valid` 和 `robIdx_value`。SCALAR_LDA/STA/STD 的字段来源和缺失字段不同，不能只用一个全局 `port_id` 推导 target。

### 修改原因

adapter 需要知道 raw 来自 SCALAR_LDA、STA 还是 STD；同类内 lane 只用于物理端口编号。monitor 采样拍的 flush epoch 和 cycle 也必须与 payload 同拍冻结，不能在 adapter pop 时用 current 值回填。

### 修改方案与修改逻辑

在 `memblock_sync_pkg` 中扩展 `dispatch_raw_int_wb_t`：

```systemverilog
typedef enum bit [1:0] {
    MEMBLOCK_INT_WB_SOURCE_INVALID    = 2'd0,
    MEMBLOCK_INT_WB_SOURCE_SCALAR_LDA = 2'd1,
    MEMBLOCK_INT_WB_SOURCE_STA        = 2'd2,
    MEMBLOCK_INT_WB_SOURCE_STD        = 2'd3
} memblock_int_wb_source_kind_e;
```

新增或保留字段：

```text
source_kind
port_id
sample_flush_epoch
cycle
key_needs_state_lookup
rob_value_only_without_flag
replay_inst_valid
flush_pipe_valid
trigger_valid
replay_inst
flush_pipe
trigger[3:0]
```

合法端口映射：

| `source_kind` | `port_id` 合法范围 | V2 端口 |
|---|---:|---|
| `SCALAR_LDA` | 0..2 | `writebackLda_0/1/2` |
| `STA` | 0..1 | `writebackSta_0/1` |
| `STD` | 0..1 | `writebackStd_0/1` |

`make_empty_raw_int_wb()` 必须清空所有字段和完整 24-bit `exception_vec`，`source_kind=INVALID`，`sample_flush_epoch/cycle=0`。
metadata 的默认值必须使用语义中性值：`replay_inst=0`、`flush_pipe=0`、
`trigger=4'hf`（V2 `TriggerAction.None`），并把三个 `*_valid` 标志清零。

### 文字伪代码

```text
make_empty_raw_int_wb()：
  raw.valid = 0；
  raw.source_kind = INVALID；
  raw.port_id = 0；
  raw.sample_flush_epoch = 0；
  raw.cycle = 0；
  raw.rob_valid/lq_valid/sq_valid = 0；
  raw.key_needs_state_lookup = 0；
  raw.rob_value_only_without_flag = 0；
  raw.replay_inst_valid = 0；
  raw.flush_pipe_valid = 0；
  raw.trigger_valid = 0；
  raw.replay_inst = 0；
  raw.flush_pipe = 0；
  raw.trigger = 4'hf；
  raw.exception_vec[23:0] = 0；
  返回 raw；

build_raw_int_wb_from_v2_port(source_kind, port_id)：
  raw = make_empty_raw_int_wb()；
  如果 source_kind/port_id 不在 V2 固定映射内，uvm_fatal；
  读取该 lane 的真实 valid；
  valid 为 0 时返回 empty raw；
  valid 为 X/Z 时 uvm_fatal；
  valid 为 1 时复制该 lane 真实 payload；
  raw.valid = 1；
  raw.source_kind = source_kind；
  raw.port_id = port_id；
  case (source_kind)
    SCALAR_LDA：
      raw.replay_inst_valid = 1；
      raw.flush_pipe_valid = 1；
      raw.trigger_valid = 1；
    STA：
      raw.flush_pipe_valid = (port_id == 0)；
      raw.trigger_valid = 1；
    STD：
      // STD 没有 replay/flush/trigger 顶层字段，保持 valid=0、trigger=None。
  endcase
  只在对应 `*_valid=1` 时复制真实 metadata；不存在的字段保持默认值；
  raw.sample_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch；
  raw.cycle = $time；
  返回 raw；
```

## 3. 问题二：monitor producer 必须是唯一 raw 入队 owner

### V2 问题

如果 builder、adapter 或多个 port 分支都可能调用 `push_raw_int_wb()`，同一个 sampled valid event 可能重复入队，或者 valid event 因 helper 返回 empty raw 被静默跳过。

### 修改原因

raw queue 是 dispatch monitor event 的唯一输入源。每个 valid lane 应恰好产生一个 raw event；无 valid lane 不入队。入队 owner 分散会让 batch order、计数和 debug provenance 变得不可追踪。

### 修改方案与修改逻辑

`io_mem_to_ooo_int_wb_agent_agent_monitor::mon_data()` 是唯一 push owner。`build_raw_int_wb_from_v2_port()` 只构造并返回 raw，不读取或写入 `raw_int_wb_q`。

`mon_data()` 按固定顺序检查：

```text
LDA0 -> LDA1 -> LDA2 -> STA0 -> STA1 -> STD0 -> STD1
```

每个 sampled valid 为 1 的 lane：

1. 调 builder。
2. 要求返回 `raw.valid=1`。
3. 调用一次 `memblock_sync_pkg::push_raw_int_wb(raw)`。

### 文字伪代码

```text
io_mem_to_ooo_int_wb_agent_agent_monitor::mon_data()：
  对 LDA0、LDA1、LDA2、STA0、STA1、STD0、STD1 依次处理：
    sampled_valid = 对应 lane valid；
    如果 sampled_valid === 0，继续；
    如果 sampled_valid !== 1，uvm_fatal；

    raw = build_raw_int_wb_from_v2_port(source_kind, kind 内 port_id)；
    如果 raw.valid == 0：
      uvm_fatal，因为 sampled valid event 被 builder 丢失；
    push_raw_int_wb(raw)；

  本 task 不查 uid；
  不写 status、active map、issue-generation state 或 pass/fail；
  不调用 mon_item_port.write；
```

## 4. 问题三：SCALAR_LDA/STA 缺 LQ/SQ key，需要 current issue snapshot 补齐

### V2 问题

V2 SCALAR_LDA/STA raw 具备真实 ROB key，但 SCALAR_LDA 顶层没有 LQ key，STA 顶层没有 SQ key。旧逻辑直接按 raw valid 设置 `real_wb_valid=1`，再用缺失或伪造 key 进入 writeback handler，会导致 uid 误归属、stale event 过滤失败或 pass/fault 状态错误。

### 修改原因

SCALAR_LDA/STA 的物理身份来源是真实 ROB key。LQ/SQ 是 target metadata，必须在确认当前 active issue generation 后从 status 补齐。不能在 adapter 中扫描全表或用 sample epoch 猜历史 generation。

### 修改方案与修改逻辑

`convert_raw_int_wb()` 对 SCALAR_LDA/STA 执行顺序固定为：

1. 入口先执行 source/lane capability 和 metadata guard；当前 SCALAR_LDA source invariant
   违例、未支持的 Debug/Trace action、未支持的 CBO flush 在任何状态更新前 fatal。
2. 用 raw 完整 ROB key 构造半成品 `wb_event`。
3. 调用 IQ feedback/replay 专项定义的唯一 API：

```text
attach_current_issue_snapshot(wb_event)
```

4. attach 成功后调用 `normalize_v2_int_wb_key(raw, wb_event)`。
5. normalize 只校验 raw ROB key 与 current snapshot 一致，并检查 target required key 完整，不再次查 active map。

LDA0（`SCALAR_LDA`、`port_id=0`）可能由 AtomicsUnit 覆盖。attach 解析 UID 后必须读取 `main_control_transaction.op_class`，若 `source==LOAD_WB && op_class==MEMBLOCK_OP_CLASS_AMO`，在 LOAD/LQ 生命周期判断前固定 fatal 为 unsupported atomic。

### 文字伪代码

```text
convert_raw_int_wb(raw) 处理 SCALAR_LDA/STA：
  如果 raw.source_kind == SCALAR_LDA &&
     raw.replay_inst_valid && raw.replay_inst == 1：
    uvm_fatal("INT_WB_SCALAR_LDA_REPLAY_INST_INVARIANT")；

  执行 metadata_guard(raw)：
    检查 source/lane 对应的 `replay_inst_valid/flush_pipe_valid/trigger_valid`；
    检查 trigger action、exceptionVec[breakPoint] 和来源是否一致；
    对当前测试框架没有 consumer 的 Debug/Trace/CBO flush 组合 fail-fast；

  如果 raw.rob_valid == 0 或 raw.rob_value_only_without_flag == 1：
    uvm_fatal，因为 SCALAR_LDA/STA 必须有真实 ROB key；

  wb_event = make_wb_event_base()；
  wb_event.source = LOAD_WB 或 STA_WB；
  wb_event.rob_key = {raw.rob_flag, raw.rob_value}；
  wb_event.has_rob = 1；

  调用 attach_current_issue_snapshot(wb_event)：
    使用 active ROB map 做固定次数 O(1) 查询；
    解析唯一 UID；
    读取 current status；
    检查 active、target dispatched、未 kill、未 redirect、未 flush、未 terminal；
    对 SCALAR_LDA 的 port_id=0（LDA0）AMO owner 在 LOAD/LQ 检查前 fatal；
    从 current status 补 uid、target key、issue_epoch、replay_seq；

  调用 normalize_v2_int_wb_key(raw, wb_event)：
    比较 raw ROB key 与 snapshot ROB key；
    SCALAR_LDA 要求 target=LOAD 且 UID/ROB/LQ 完整；
    STA 要求 target=STA 且 UID/ROB/SQ 完整；
    不再次查询其它 uid 或 status；
```

## 5. 问题四：STD 只有 ROB value，不能伪造 ROB flag/SQ key

### V2 问题

`writebackStd_0/1` 当前确认只有 `valid` 和 `robIdx_value`，没有 `robIdx_flag`、`sqIdx_flag` 或 `sqIdx_value`。旧逻辑若把 flag 置 0 或从历史 V3 port 推导 SQ key，会把缺失字段伪装成真实 key。

### 修改原因

STD real writeback 闭环需要 UID、完整 ROB key 和 SQ key。V2 raw 只能提供 ROB value，因此只能在当前 active ROB map 中对两个可能 flag 做固定探测，并从 status 补 SQ key。不得扫描 `main_trans_num` 全表，也不得使用不稳定内部 wire。

### 修改方案与修改逻辑

STD monitor：

- `raw.rob_valid=0`。
- `raw.rob_value=真实 robIdx_value`。
- `raw.sq_valid=0`。
- `raw.rob_value_only_without_flag=1`。
- `raw.key_needs_state_lookup=1`。

adapter 新增 `resolve_std_uid_by_rob_value_only(raw, wb_event)`：

- 构造 `{flag=0,value}` 和 `{flag=1,value}` 两个 ROB key。
- 对 active ROB map 做两次 O(1) lookup。
- 每个 hit 读取 status 并检查 STD dispatched、active SQ mapped、未 terminal/killed/flushed。
- 通过 active SQ map 确认 SQ key owner 仍是同一个 uid。
- 只允许唯一候选。
- 成功后补齐 UID、ROB key、SQ key。

STD real writeback 是 `std_writeback/std_pass` 的唯一完成 owner。value-only 双 flag 解析若
出现零候选、双候选、active ROB/SQ owner 不一致或 required key 不完整，一律在任何状态
修改前 fatal；不再保留参数化 warning/drop 分支。

### 文字伪代码

```text
resolve_std_uid_by_rob_value_only(raw, wb_event)：
  如果 raw.sample_flush_epoch > current dispatch_flush_epoch：
    uvm_fatal；

  key0 = {flag:0, value:raw.rob_value}；
  key1 = {flag:1, value:raw.rob_value}；

  hit0 = lookup_active_uid_by_rob(key0, uid0)；
  如果 hit0：
    status0 = get_status(uid0)；
    sq_key0 = {status0.sqIdx_flag, status0.sqIdx_value}；
    hit0 = status0.active
           && status0.std_dispatched
           && !status0.terminal_done
           && !status0.flushed
           && !status0.issue_killed
           && status0.active_sq_mapped
           && status0.rob key 等于 key0
           && status0.active_instance_flush_epoch_valid
           && raw.sample_flush_epoch >= status0.active_instance_flush_epoch
           && lookup_active_uid_by_sq(sq_key0) == uid0；

  hit1 = lookup_active_uid_by_rob(key1, uid1)，执行同样检查；

  如果 hit0 和 hit1 同时成立：
    返回失败，多候选不允许；
  如果二者都不成立：
    返回失败；

  选择唯一命中项；
  wb_event.uid = uid；
  wb_event.rob_key = 对应 key；
  wb_event.sq_key = 对应 status SQ key；
  wb_event.has_uid/has_rob/has_sq = 1；
  wb_event.target = STD；
  返回成功；

  全程只做两个 active map probe 和必要 status 读取；
  不扫描 main_trans_num、status 全表或 active window。
```

## 6. 问题五：metadata、exceptionVec 和 fail-fast 策略必须按真实端口定义

### V2 问题

SCALAR_LDA/STA/STD 各 port 的 `exceptionVec` 位图不一致；SCALAR_LDA 有
`replayInst`，STA 没有；STA0 和 STA1 的 `flushPipe` 能力也不同。旧逻辑若统一复制或默认置
valid，会把不存在的 metadata 当成真实字段。

### 修改原因

raw event 是物理接口事实，不是语义猜测。不存在的 exception bit 和 metadata 必须保持 factory 0。
RTL 侧由 Backend/ExceptionGen/ROB 消费这些字段；但当前测试框架 adapter 尚未实现
DebugMode、Trace action 和 CBO `flushAfter` 的对应状态收口，因此这些组合必须在状态更新前
fail-fast，不能把它们当作普通 pass event。

### 运行时语义约束（不是 capability）

`FuConfig.flushPipe/replayInst/trigger` 只表示执行单元在 elaboration 时是否生成可选字段，
不能直接推导某一拍写回值。当前 V2 Scala/生成 RTL 的运行时约束如下：

| 字段 | 当前 V2 producer 与置位场景 | 本 plan 对 split writeback 的约束 |
|---|---|---|
| `replayInst` | 可选 `HybridUnit` 可在 `s3_rep_frm_fetch`（store-to-load forwarding 的物理/虚拟地址 CAM 匹配失败）时置 1，并由同一条件产生 `rollback.valid`；它通过独立 `writebackHyuLda` 输出。当前 profile 为 `HyuCnt=0`，`writebackLda_0/1/2` 的 scalar LoadUnit、LoadMisalignBuffer 路径显式清 0。 | 当前 SCALAR_LDA 看到 1 是 source invariant 违例，入口 fatal；不能当普通 Load pass。未来若接入 `HYU_LDA`，必须使用独立 source kind，并与同源 `memoryViolation` 关联。 |
| `flushPipe` | Fence/SFENCE 等由 Decode 属性产生；STA0 的 StoreQueue CBO/CMO 写回可动态置 1；普通 SCALAR_LDA/STA 写回为 0，STA1/STD 不承载该字段。HybridUnit 内部名为 `s3_flushPipe` 的 load-load violation 只参与直接 rollback，没有写入 `uop.flushPipe`。 | SCALAR_LDA 为 1 视为 producer 违例；STA0 为 1 只允许 CBO/CMO，当前 adapter 未实现 CBO `flushAfter` 时 fatal；STA1/STD 保持 absent/0。 |
| `trigger[3:0]` | Memory trigger 在 Load/Store 地址阶段按 CSR、地址、chain/timing 生成。`0=BreakpointExp`、`1=DebugMode`、`2/3/4=Trace`、`15=None`。STA0 StoreQueue uncache/CBO 还可能把 trigger 清为 0。 | 只接受 `None`，或 `BreakpointExp + exceptionVec[breakPoint]=1` 进入既有 fault path；STA0 的 uncache/CBO `0 + breakPoint=0` 仅按来源识别为中性值。Debug/Trace/未知编码 fail-fast。 |

### 修改方案与修改逻辑

逐 port exception 位：

| port | exceptionVec 位 |
|---|---|
| LDA0 | `{3,4,5,6,7,13,15,19,21,23}` |
| LDA1/LDA2 | `{3,4,5,13,19,21}` |
| STA0 | `{0..23}` |
| STA1 | `{3,6,7,15,19,23}` |
| STD0/STD1 | 无，保持 0 |

metadata 策略：

- 任一当前 SCALAR_LDA raw `replay_inst=1`：converter 入口固定 fatal；不得把它转换成普通
  real-WB/pass event。
- SCALAR_LDA `flush_pipe=1`：当前 SCALAR_LDA producer invariant 违例，fatal。
- STA0 `flush_pipe=1`：仅允许 StoreQueue CBO/CMO 来源；当前 adapter 未实现 CBO
  `flushAfter` 收口时按 unsupported CBO flush fatal。普通 STA0 必须为 0。
- STA1 没有 `flushPipe` 顶层字段，`flush_pipe_valid=0` 且 raw 值固定为 0。
- STD 没有 `replayInst/flushPipe/trigger` 顶层字段，三个 valid flag 均为 0，值保持中性默认值。
- `trigger=4'hf`（`TriggerAction.None`）：无动作，继续正常 key 归一化。
- `trigger=4'h0`（`BreakpointExp`）：要求 `exceptionVec[breakPoint]=1`，交给既有 fault
  路径；STA0 StoreQueue uncache/CBO 来源若同时 `breakPoint=0`，按来源识别为非规范中性值，
  不生成 breakpoint fault。
- `trigger=4'h1`（`DebugMode`）以及 `4'h2/3/4`（Trace action）：RTL 编码合法，但当前
  adapter/handler 没有对应 consumer，入口 fatal。
- 其它 trigger 编码：fatal。
- `rfWen/fpWen/pdest/data/isFromLoadUnit/debug_isMMIO/isNCIO/isPerfCnt` 可作为观察/debug metadata，但当前不直接决定 pass/fault/terminal。

### 文字伪代码

```text
build_raw_int_wb_from_v2_port() 复制 exceptionVec：
  raw = make_empty_raw_int_wb()，先保证 exception_vec 全 0；
  如果是 LDA0，只复制真实存在的 10 个 bit；
  如果是 LDA1/LDA2，只复制真实存在的 6 个 bit；
  如果是 STA0，复制 0..23 全部 bit；
  如果是 STA1，只复制真实存在的 6 个 bit；
  如果是 STD，保持 exception_vec=0；

convert_raw_int_wb() metadata guard：
  先校验 source/lane 对应的 metadata valid flag；不存在的字段若不是中性默认值，uvm_fatal；

  如果 raw.source_kind == SCALAR_LDA 且 raw.replay_inst_valid && raw.replay_inst==1：
    uvm_fatal("INT_WB_SCALAR_LDA_REPLAY_INST_INVARIANT")；
  如果 raw.source_kind == SCALAR_LDA 且 raw.flush_pipe_valid && raw.flush_pipe==1：
    uvm_fatal("INT_WB_SCALAR_LDA_FLUSH_PIPE_INVARIANT")；

  如果 raw.source_kind 是 STA0 且 raw.flush_pipe_valid && raw.flush_pipe==1：
    要求 raw provenance 表明 StoreQueue CBO/CMO；
    当前 adapter 没有 CBO flushAfter consumer 时 uvm_fatal；
  如果 raw.source_kind 是 STA1：
    要求 flush_pipe_valid==0 且 flush_pipe==0；

  如果 trigger_valid==0：
    要求 trigger==4'hf；
  否则按 trigger 编码分支：
    4'hf：继续；
    4'h0：普通 SCALAR_LDA/STA 要求 exceptionVec[breakPoint]==1；
         STA0 StoreQueue uncache/CBO 允许 provenance 表明 breakPoint==0 的中性值；
    4'h1、4'h2、4'h3、4'h4：当前 adapter 无 consumer，uvm_fatal；
    其它：uvm_fatal；

  STD metadata 不生成 replay/flush/trigger 事件；
```

### `replayInst=1` 为什么重发且当前指令不提交

`replayInst` 不是 IQ feedback miss、LoadQueueReplay 或普通 TLB replay 的统称，而是
写回给 ExceptionGen/ROB 的精确重放标志。它表示本次执行结果不能作为架构完成结果使用，
必须清掉当前指令及其之后的投机状态，再从当前指令自身重新取指和执行。

通用 replay writeback/ROB 安全语义为：

```text
如果一个 replayInst writeback 保留到 Backend：
  replayInst=1 的结果仍先携带原 robIdx；
  Backend 将它送入 ROB 的 replay 字段，writebackNeedFlush 同拍反映需要 flush；
  ExceptionGen 按 robIdx 保存 replayInst，并与同一条指令的其它异常信息合并；

当该 robIdx 到达 ROB head 且 commit_w 成立：
  deqHasReplayInst=1；
  ROB 产生 flushOut，redirect.level=RedirectLevel.flush；
  redirect.flushItself()=1，目标 PC 使用当前指令 PC，而不是 PC+指令长度；
  ROB/Rename/Issue/LSQ 清除当前指令及其后的旧执行状态，前端重新取当前指令；
  当前指令不计入 commit，重新发射后必须收到普通完成写回才允许提交。
```

但 `HybridUnit.s3_rep_frm_fetch` 的实际恢复路径不能只描述成“等待 ROB head”。当
store-to-load forward 的物理/虚拟地址 CAM 匹配结果失效时，同一个 S3 条件同时产生：

```text
HybridUnit.s3_rep_frm_fetch = 1
  ├─ writebackHyuLda.uop.replayInst = 1
  └─ ldu_io.rollback.valid = 1
       rollback.level = RedirectLevel.flush
                    ↓
MemBlock 在 allRedirect 中选择最老项
                    ↓
mem_to_ooo.memoryViolation
                    ↓
Backend CtrlBlock redirect
                    ↓
立即 flush 当前指令及更年轻指令，从当前指令重新取指
```

因此 HybridUnit 的 `replayInst` writeback 和 `memoryViolation` 不是两个独立 replay，二者来自
同一个 `s3_rep_frm_fetch`。直接 `memoryViolation/redirect` 是恢复和重新发射的唯一 owner；
若 replay writeback 同时被观察到，它只能作为同一恢复原因的 metadata/ROB 安全信息，不能
再创建第二次 replay、再次增加 `replay_seq`，也不能被当作普通
`real_wb_valid && pass`。

当前 plan 不接入 HybridUnit。当前 V2 `SCALAR_LDA` 端口即使保留 `replayInst` 字段，实际
producer 仍将其清零，因此观察到 `replayInst=1` 必须在任何状态修改前按 source invariant
fatal，不能静默吞掉该位。

未来只有在 profile 启用 `HyuCnt>0` 时，才能扩展如下独立流程：

```text
将 source_kind enum 从 2 bit 扩为 3 bit，并新增 MEMBLOCK_INT_WB_SOURCE_HYU_LDA；
独立采样 writebackHyuLda，不复用 SCALAR_LDA；
HYU_LDA replayInst=1 必须匹配同一 ROB/issue generation 的 memoryViolation；
执行 redirect-first 仲裁；
由 memoryViolation 唯一关闭旧 generation 并令 replay_seq + 1；
replayInst writeback 不进入普通 pass handler，也不重复触发 replay；
无法证明二者属于同一 generation 时 fatal，不允许猜测。
```

源码核验后确认：`ExceptionGen.scala` 的 `in_wb_valids` 使用的是
`RobExceptionInfo.has_exception` 方法，而不是只读取 `hasException` 字段。该方法定义为
`hasException || flushPipe || singleStep || replayInst || TriggerAction.isDmode(trigger)`，
因此即使 `replayInst=1` 且 `exceptionVec=0`，replay-only writeback 仍会进入
ExceptionGen；`Rob.scala` 中 `exc_wb.bits.hasException := exceptionVec.orR` 只保存
exceptionVec 摘要，不是 ExceptionGen 的唯一收集门控。当前 SCALAR_LDA 路径仍显式把
`replayInst` 清零，所以 adapter 对 SCALAR_LDA 观察到 1 仍按 source invariant 违例
固定 fatal；这与“replay-only 无法被 ExceptionGen 收集”是两件不同的事。

## 7. 问题六：key 归一化失败策略不能静默 drop 关键事件

### V2 问题

旧逻辑在 key 缺失时可能 warning/drop 或留下部分 `has_rob/has_lq/has_sq` 状态。
SCALAR_LDA/STA/STD 的真实 valid writeback 都是当前严格完成流的关键事件；任何一个事件
被丢弃，都可能让对应 target 永远等待，或者掩盖 DUT/monitor 的身份错误。

### 修改原因

writeback event 一旦误归一化，会直接影响 pass/fault/terminal。STD 不再提供
`STD_REAL_WB_PASS_EN=0` 的 synthetic issue-accept pass 兼容路径，因此不存在“真实
writeback 不是 terminal 必需事件”的合法配置。所有受支持 target 都必须遵循
all-or-fatal，避免 silently wrong 和 silently stuck 两类问题。

### 修改方案与修改逻辑

`normalize_v2_int_wb_key(raw, wb_event)`：

- SCALAR_LDA/STA：任何 raw ROB 缺失、snapshot 缺失、snapshot 与 raw key 不一致、required key 不完整均 fatal。
- STD：先尝试 value-only 双 flag resolve；零候选、双候选、ROB/SQ owner 不一致、snapshot
  不完整或 required key 缺失均 fatal。
- 不再实现 `fail_by_real_wb_policy()`、STD drop counter 或参数化 warning/drop 分支。

STD 完成 owner 固定为：

```text
issueStd fire：
  只设置std_dispatched、issue_epoch和当前generation snapshot；
  不设置std_writeback/std_pass；
  不生成STD_FEEDBACK synthetic event；

真实writebackStd：
  monitor采样raw；
  adapter完成唯一key归一化；
  writeback handler设置std_writeback/std_pass；
```

### 文字伪代码

```text
normalize_v2_int_wb_key(raw, wb_event)：
  case raw.source_kind:
    SCALAR_LDA 或 STA：
      要求 wb_event 已经 attach current snapshot；
      要求 raw.rob_valid=1 且不是 value-only；
      raw_rob_key = {raw.rob_flag, raw.rob_value}；
      如果 wb_event.rob_key != raw_rob_key，uvm_fatal；
      如果 SCALAR_LDA 但 target 不是 LOAD，uvm_fatal；
      如果 STA 但 target 不是 STA，uvm_fatal；

    STD：
      要求 raw 是 value-only 形态；
      如果 resolve_std_uid_by_rob_value_only() 失败：
        uvm_fatal("INT_WB_STD_KEY_NORMALIZE_FAILED")；

    default：
      uvm_fatal；

  按 target 检查 required key：
    LOAD 必须有 UID/ROB/LQ；
    STA 必须有 UID/ROB/SQ；
    STD 必须有 UID/ROB/SQ；
  任一 required key 缺失均 uvm_fatal；
  成功返回 1；
```

### 删除 `STD_REAL_WB_PASS_EN` 兼容路径

coding 时必须同步删除：

- `plus.sv` 中 `MEMBLOCK_STD_REAL_WB_PASS_EN` 的定义与 `load_bit()`。
- `seq_csr_common.sv` 中 `std_real_wb_pass_en` 状态、加载和 getter。
- 所有 plus cfg 中 `+MEMBLOCK_STD_REAL_WB_PASS_EN=...`。
- `memblock_issue_dispatch_base_sequence.sv` 中
  `item_needs_issue_accept_pass()`、`make_issue_accept_pass_event()`、
  `submit_issue_accept_pass()` 及 issue fire 后的调用。
- `writeback_status_handler.sv` 中 STD 根据该参数在 issue feedback 与 real-WB pass 之间切换
  的逻辑。STD pass 只接受归一化后的真实 `writebackStd` event；意外出现
  `MEMBLOCK_WB_EVENT_SOURCE_STD_FEEDBACK` 时 fail-fast，不能完成 STD target。
- 活跃规则/说明文档中“设置为 0 可启用 STD synthetic pass”的操作说明。历史完成记录只
  保留历史事实，不作为当前可用配置继续引用。

### 严格模式验收约束

```text
1. issueStd fire后、writebackStd到达前：std_dispatched=1，std_pass=0；
2. 唯一匹配的writebackStd到达后：std_writeback=1，std_pass=1；
3. STD value-only ROB零命中或双命中：状态不变并立即fatal；
4. STD补齐后的ROB/SQ owner不一致：状态不变并立即fatal；
5. active代码和plus cfg中不再出现STD_REAL_WB_PASS_EN；
6. 不再存在issue-accept STD synthetic pass producer或STD key failure drop counter。
```

## 8. Coding 落点汇总

| 文件 | 对应问题与修改 |
|---|---|
| `mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv` | 问题一、六：raw enum、字段、factory；STD 归一化失败不新增 drop counter |
| `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_interface.sv` | 问题一、五：V2 split 字段与 metadata 真实采样 |
| `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_xaction.sv` | 问题一、五：字段结构与打印/比较同步 |
| `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_monitor.sv` | 问题二、五：唯一 raw push owner、builder、exception/metadata 采样 |
| `mem_ut/ver/ut/memblock/tb/io_mem_to_ooo_int_wb_agent_connect.sv` | 问题一、五：确认只连接 V2 `writebackLda/Sta/Std` 顶层真实字段 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv` | 问题三、四、六：current snapshot attach、STD value-only resolve、all-or-fatal normalize policy |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/writeback_status_handler.sv` | 只消费归一化后的事件；删除 STD 参数分支，拒绝 synthetic `STD_FEEDBACK` 完成 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv` | 问题四：active ROB/SQ map O(1) lookup 复用 |
| `mem_ut/ver/ut/memblock/env/plus.sv` | 问题六：删除 `MEMBLOCK_STD_REAL_WB_PASS_EN` 定义和加载 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv` | 问题六：删除 STD real-WB pass 参数镜像和 getter |
| `mem_ut/ver/ut/memblock/seq/base_seq/memblock_issue_dispatch_base_sequence.sv` | 问题六：删除 STD issue-accept synthetic pass producer 和调用 |
| `mem_ut/ver/ut/memblock/seq/plus_cfg/*.cfg` | 问题六：删除废弃的 `+MEMBLOCK_STD_REAL_WB_PASS_EN=...` |
| 活跃测试框架规则/说明文档 | 问题六：删除切换 STD synthetic pass 的用户操作说明，统一描述为严格 real-WB owner |

明确不修改：

```text
writeback_status_handler 主职责
pass/fail/terminal owner
RM/checker/scoreboard/coverage
历史 V3 intWriteback port 编码
内部 stdExeUnits wire 采样
全表扫描反查 uid
```

## 9. 修改类型与原逻辑对比总结

| 修改项 | 类型 | 修改前逻辑 | 修改原因 | 修改后逻辑 |
|---|---|---|---|---|
| `source_kind/port_id` | 字段适配 | 全局 port 0..6 隐含 target | V2 是 SCALAR_LDA/STA/STD split port，当前 profile 不含 HYU | `SCALAR_LDA` 只表示 `writebackLda_0/1/2`；kind 表示类别，port_id 只表示 kind 内 lane |
| raw provenance | 字段适配 | adapter 可能用 current epoch 回填 | payload 和 epoch 必须同拍 | monitor 采样拍冻结 `sample_flush_epoch/cycle` |
| raw factory | 字段适配 | 新字段默认不完整 | 防止 metadata 残留 | empty raw 清所有 valid、metadata、exceptionVec |
| monitor push owner | 功能逻辑修改 | helper 分散构造/入队风险 | 每个 valid lane 必须唯一 raw | `mon_data()` 唯一 push，builder 只返回 raw |
| SCALAR_LDA/STA key | 功能逻辑修改 | 缺 LQ/SQ 时可能伪造或直接 pass | LQ/SQ 不是顶层 raw 物理字段 | raw ROB -> current snapshot -> normalize 校验 |
| `replayInst` | unsupported gate | 可被误当普通 real-WB/pass | 当前 SCALAR_LDA producer 不应置高；Hybrid 合法置位时还会由同源 rollback 产生 `memoryViolation` | SCALAR_LDA converter 入口 invariant fatal；未来 HYU 使用独立 source kind，并以 `memoryViolation` 为唯一 replay owner |
| `flushPipe` | 语义/能力适配 | 把所有 lane 的字段统一当作可运行时置高 | 普通 SCALAR_LDA/STA 为 0，STA0 CBO/CMO 可为 1，STA1/STD absent | 按 source/lane/provenance 检查；仅未支持 CBO flush fatal |
| `trigger[3:0]` | 编码适配 | 用 `trigger != 0` 判断有无动作 | `None=4'hf`，`0` 是 BreakpointExp，1/2/3/4 是 Debug/Trace | 按枚举和 exceptionVec/source 联合判断 |
| LDA0 atomic owner | unsupported gate | 可能按 LOAD 缺 LQ 报错 | LDA0 可被 AtomicsUnit 覆盖 | 解析 UID 后按 `op_class==AMO` 固定 fatal |
| STD key | 功能逻辑修改 | 伪造 ROB flag/SQ key 或 drop | V2 只有 ROB value | 双 flag active ROB map probe，唯一命中后从 status 补 SQ |
| STD 完成 owner | 功能逻辑收敛 | 参数可切换 real-WB 与 issue-accept synthetic pass | 双 owner 会掩盖真实 writeback 丢失或错误 key | 删除 `STD_REAL_WB_PASS_EN` 和 synthetic producer，只有真实 `writebackStd` 可设置 pass |
| STD 失败策略 | fail-fast | 参数关闭时可 warning/drop | 严格模式下每个真实 STD writeback 都是 terminal 必需事件 | value-only key 零命中、双命中、owner 不一致或 key 不完整一律 fatal |
| exceptionVec | 字段适配 | 可能统一复制或默认 valid | 各 port 位图不同 | 按 port 真实位复制，不存在位保持 0 |
| adapter 查询成本 | 性能边界 | 可能按 uid 全表反查 | monitor event 是高频路径 | SCALAR_LDA/STA 固定 active map 查询；STD 仅两个 ROB flag probe |

保持不变的主体逻辑：raw batch 入口、writeback handler 状态推进职责、current issue token owner、redirect-first batch 仲裁、pass/fail/terminal 收口、RM/checker/coverage deferred 状态。

## 10. 执行结果

- 代码、相关 flow/analysis/规则文档和 implementation review 已同步完成。
- 独立 subagent review 结果为 `FINAL PASS`；本 agent 已复核 review 覆盖范围、旧符号清理和下游边界。
- 干净远端 VCS/Verdi 编译通过，结果为 `0 error(s), 0 warning(s)`。
- `tc_sanity` 通过，`UVM_ERROR=0`、`UVM_FATAL=0`。
- 真实 store writeback smoke 已观察到 `STD writeback -> STA IQ feedback -> STA writeback -> ROB commit`；随后暴露的
  `sqDeq` pointer mismatch 属于 LSQ MMIO/status/SQ deq 子计划，不属于本 plan 的 Int-WB owner。
- 本 plan 不修改 SQ deq、commit/deq 顺序或 terminal owner；完成后可按执行规则移动到 `plan/do`。
