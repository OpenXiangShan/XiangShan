# mem_ut V2 Int Writeback 测试框架适配最终 Coding Plan

| 项目 | 内容 |
|---|---|
| 状态 | `undo`，待 coding |
| 目标版本 | V2 |
| 当前分支 | `mem_ut_uvm_v2` |
| V2 接口权威 | `build_memblock/rtl/MemBlock.sv` |
| 测试框架入口 | `io_mem_to_ooo_int_wb_agent_agent_monitor::mon_data()`、`dispatch_monitor_event_adapter::convert_raw_int_wb()` |
| 适配原则 | 只重构 V2 split writeback raw producer 和 adapter key 归一化，不改变 writeback handler 主职责、RM/checker/coverage |
| 创建/修订日期 | 2026-07-15 |

## 1. 范围与边界

本 plan 只整理 V2 `writebackLda/Sta/Std` split writeback 进入公共状态流时需要解决的问题。每个问题均说明修改原因、最终方案、修改的原有逻辑和可直接 coding 的文字伪代码。

本轮支持范围：

- V2 `writebackLda_0/1/2`、`writebackSta_0/1`、`writebackStd_0/1` raw event 保真采样。
- raw event 同时保留 `source_kind` 与 kind 内 `port_id`。
- LDA/STA 使用真实 ROB key 加 current issue snapshot 补齐 UID、target key、`issue_epoch/replay_seq`。
- STD 只有 `robIdx_value` 时，通过两个 ROB flag 的 active map 固定探测补齐完整 ROB/SQ key。
- LDA `replayInst`、AMO owner、STA/STD metadata 和缺 key 场景采用分层 fail-fast 策略。

本轮不支持：

- 不从 connect 层猜常量来伪造缺失 key。
- 不把 RTL 内部 `_inner_stdExeUnits_*_sqIdx_*` wire 当作当前 top-level STD SQ key 来源。
- 不新增历史 event 恢复 API、第二套 issue-generation 状态或全表扫描反查。
- 不实现 RM、checker、scoreboard 或 coverage。
- 不改变 `writeback_status_handler` 的主职责和 pass/fail/terminal owner。

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

`writebackStd_0/1` 当前只有 `valid` 和 `robIdx_value`。LDA/STA/STD 的字段来源和缺失字段不同，不能只用一个全局 `port_id` 推导 target。

### 修改原因

adapter 需要知道 raw 来自 LDA、STA 还是 STD；同类内 lane 只用于物理端口编号。monitor 采样拍的 flush epoch 和 cycle 也必须与 payload 同拍冻结，不能在 adapter pop 时用 current 值回填。

### 修改方案与修改逻辑

在 `memblock_sync_pkg` 中扩展 `dispatch_raw_int_wb_t`：

```systemverilog
typedef enum bit [1:0] {
    MEMBLOCK_INT_WB_SOURCE_INVALID = 2'd0,
    MEMBLOCK_INT_WB_SOURCE_LDA     = 2'd1,
    MEMBLOCK_INT_WB_SOURCE_STA     = 2'd2,
    MEMBLOCK_INT_WB_SOURCE_STD     = 2'd3
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
replay_inst
flush_pipe
trigger
```

合法端口映射：

| `source_kind` | `port_id` 合法范围 | V2 端口 |
|---|---:|---|
| `LDA` | 0..2 | `writebackLda_0/1/2` |
| `STA` | 0..1 | `writebackSta_0/1` |
| `STD` | 0..1 | `writebackStd_0/1` |

`make_empty_raw_int_wb()` 必须清空所有字段和完整 24-bit `exception_vec`，`source_kind=INVALID`，`sample_flush_epoch/cycle=0`。

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
  raw.replay_inst/flush_pipe/trigger = 0；
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

## 4. 问题三：LDA/STA 缺 LQ/SQ key，需要 current issue snapshot 补齐

### V2 问题

V2 LDA/STA raw 具备真实 ROB key，但 LDA 顶层没有 LQ key，STA 顶层没有 SQ key。旧逻辑直接按 raw valid 设置 `real_wb_valid=1`，再用缺失或伪造 key 进入 writeback handler，会导致 uid 误归属、stale event 过滤失败或 pass/fault 状态错误。

### 修改原因

LDA/STA 的物理身份来源是真实 ROB key。LQ/SQ 是 target metadata，必须在确认当前 active issue generation 后从 status 补齐。不能在 adapter 中扫描全表或用 sample epoch 猜历史 generation。

### 修改方案与修改逻辑

`convert_raw_int_wb()` 对 LDA/STA 执行顺序固定为：

1. 入口先处理不可达 `replay_inst` 和 metadata fatal。
2. 用 raw 完整 ROB key 构造半成品 `wb_event`。
3. 调用 IQ feedback/replay 专项定义的唯一 API：

```text
attach_current_issue_snapshot(wb_event)
```

4. attach 成功后调用 `normalize_v2_int_wb_key(raw, wb_event)`。
5. normalize 只校验 raw ROB key 与 current snapshot 一致，并检查 target required key 完整，不再次查 active map。

LDA0 可能由 AtomicsUnit 覆盖。attach 解析 UID 后必须读取 `main_control_transaction.op_class`，若 `source==LOAD_WB && op_class==MEMBLOCK_OP_CLASS_AMO`，在 LOAD/LQ 生命周期判断前固定 fatal 为 unsupported atomic。

### 文字伪代码

```text
convert_raw_int_wb(raw) 处理 LDA/STA：
  如果 raw.replay_inst == 1：
    uvm_fatal("INT_WB_REPLAY_INST_UNREACHABLE")；

  如果 raw.flush_pipe != 0 或 raw.trigger != 0：
    uvm_fatal，因为当前 scalar flow 没有 consumer；

  如果 raw.rob_valid == 0 或 raw.rob_value_only_without_flag == 1：
    uvm_fatal，因为 LDA/STA 必须有真实 ROB key；

  wb_event = make_wb_event_base()；
  wb_event.source = LOAD_WB 或 STA_WB；
  wb_event.rob_key = {raw.rob_flag, raw.rob_value}；
  wb_event.has_rob = 1；

  调用 attach_current_issue_snapshot(wb_event)：
    使用 active ROB map 做固定次数 O(1) 查询；
    解析唯一 UID；
    读取 current status；
    检查 active、target dispatched、未 kill、未 redirect、未 flush、未 terminal；
    对 LDA0 的 AMO owner 在 LOAD/LQ 检查前 fatal；
    从 current status 补 uid、target key、issue_epoch、replay_seq；

  调用 normalize_v2_int_wb_key(raw, wb_event)：
    比较 raw ROB key 与 snapshot ROB key；
    LDA 要求 target=LOAD 且 UID/ROB/LQ 完整；
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

`STD_REAL_WB_PASS_EN=1` 时解析失败必须 fatal；为 0 时允许 warning+计数 drop，但必须清空半成品 event。

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

LDA/STA/STD 各 port 的 `exceptionVec` 位图不一致；LDA 有 `replayInst`，STA 没有；STA0 和 STA1 的 `flushPipe` 能力也不同。旧逻辑若统一复制或默认置 valid，会把不存在的 metadata 当成真实字段。

### 修改原因

raw event 是物理接口事实，不是语义猜测。不存在的 exception bit 和 metadata 必须保持 factory 0。当前 scalar flow 没有 `replayInst/trigger/flushPipe` 正向 consumer，不能生成半成品 event 继续跑。

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

- 任一 LDA raw `replay_inst=1`：converter 入口固定 fatal。
- LDA `flush_pipe/trigger` 非 0：fatal。
- STA `trigger` 非 0：fatal。
- STA0 `flush_pipe` 非 0：fatal。
- STA1 `flush_pipe` 不存在，raw 保持 0。
- STD 不建模 replay/flush/trigger。
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
  如果 raw.source_kind 是 LDA 且 replay_inst=1，uvm_fatal；
  如果 raw.source_kind 是 LDA 且 flush_pipe或trigger非0，uvm_fatal；
  如果 raw.source_kind 是 STA 且 trigger非0，uvm_fatal；
  如果 raw.source_kind 是 STA0 且 flush_pipe非0，uvm_fatal；
  STD metadata 不生成 replay/flush/trigger 事件；
```

## 7. 问题六：key 归一化失败策略不能静默 drop 关键事件

### V2 问题

旧逻辑在 key 缺失时可能 warning/drop 或留下部分 `has_rob/has_lq/has_sq` 状态。对于 LDA/STA，真实 valid raw 缺 current snapshot 是接口或生命周期错误；对于 STD，兼容参数关闭时可以 drop，但必须可见且不污染半成品 event。

### 修改原因

writeback event 一旦误归一化，会直接影响 pass/fault/terminal。失败策略必须按 target 分层，避免 silently wrong 和 silently stuck 两类问题。

### 修改方案与修改逻辑

`normalize_v2_int_wb_key(raw, wb_event)`：

- LDA/STA：任何 raw ROB 缺失、snapshot 缺失、snapshot 与 raw key 不一致、required key 不完整均 fatal。
- STD：先尝试 value-only 双 flag resolve；失败时调用 `fail_by_real_wb_policy(raw, STD, reason)`。

`fail_by_real_wb_policy()`：

- `STD_REAL_WB_PASS_EN=1`：fatal。
- `STD_REAL_WB_PASS_EN=0`：warning，计数，返回 drop。
- drop 前必须把 `wb_event` 恢复为 empty base event。

### 文字伪代码

```text
normalize_v2_int_wb_key(raw, wb_event)：
  case raw.source_kind:
    LDA 或 STA：
      要求 wb_event 已经 attach current snapshot；
      要求 raw.rob_valid=1 且不是 value-only；
      raw_rob_key = {raw.rob_flag, raw.rob_value}；
      如果 wb_event.rob_key != raw_rob_key，uvm_fatal；
      如果 LDA 但 target 不是 LOAD，uvm_fatal；
      如果 STA 但 target 不是 STA，uvm_fatal；

    STD：
      要求 raw 是 value-only 形态；
      如果 resolve_std_uid_by_rob_value_only() 失败：
        wb_event = make_wb_event_base()；
        return fail_by_real_wb_policy(raw, STD, reason)；

    default：
      uvm_fatal；

  按 target 检查 required key：
    LOAD 必须有 UID/ROB/LQ；
    STA 必须有 UID/ROB/SQ；
    STD 必须有 UID/ROB/SQ；
  缺失时 LDA/STA fatal，STD 走 real-WB policy；
  成功返回 1；

fail_by_real_wb_policy(raw, target, reason)：
  如果 target 是 STD 且 MEMBLOCK_STD_REAL_WB_PASS_EN == 0：
    记录 warning 和 drop counter；
    返回 0；
  否则：
    uvm_fatal；
```

## 8. Coding 落点汇总

| 文件 | 对应问题与修改 |
|---|---|
| `mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv` | 问题一、六：raw enum、字段、factory、drop 计数入口 |
| `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_interface.sv` | 问题一、五：V2 split 字段与 metadata 真实采样 |
| `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_xaction.sv` | 问题一、五：字段结构与打印/比较同步 |
| `mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_monitor.sv` | 问题二、五：唯一 raw push owner、builder、exception/metadata 采样 |
| `mem_ut/ver/ut/memblock/tb/io_mem_to_ooo_int_wb_agent_connect.sv` | 问题一、五：确认只连接 V2 `writebackLda/Sta/Std` 顶层真实字段 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv` | 问题三、四、六：current snapshot attach、STD value-only resolve、normalize/fail policy |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/writeback_status_handler.sv` | 只消费归一化后的事件，不承担 key 猜测 |
| `mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv` | 问题四：active ROB/SQ map O(1) lookup 复用 |

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
| `source_kind/port_id` | 字段适配 | 全局 port 0..6 隐含 target | V2 是 LDA/STA/STD split port | kind 表示类别，port_id 只表示 kind 内 lane |
| raw provenance | 字段适配 | adapter 可能用 current epoch 回填 | payload 和 epoch 必须同拍 | monitor 采样拍冻结 `sample_flush_epoch/cycle` |
| raw factory | 字段适配 | 新字段默认不完整 | 防止 metadata 残留 | empty raw 清所有 valid、metadata、exceptionVec |
| monitor push owner | 功能逻辑修改 | helper 分散构造/入队风险 | 每个 valid lane 必须唯一 raw | `mon_data()` 唯一 push，builder 只返回 raw |
| LDA/STA key | 功能逻辑修改 | 缺 LQ/SQ 时可能伪造或直接 pass | LQ/SQ 不是顶层 raw 物理字段 | raw ROB -> current snapshot -> normalize 校验 |
| `replayInst` | unsupported gate | 可被误当 replay event | 当前受支持 scalar LDA producer 不应置高 | converter 入口 fatal，不生成 event |
| LDA0 atomic owner | unsupported gate | 可能按 LOAD 缺 LQ 报错 | LDA0 可被 AtomicsUnit 覆盖 | 解析 UID 后按 `op_class==AMO` 固定 fatal |
| STD key | 功能逻辑修改 | 伪造 ROB flag/SQ key 或 drop | V2 只有 ROB value | 双 flag active ROB map probe，唯一命中后从 status 补 SQ |
| STD 失败策略 | 兼容逻辑 | drop/fatal 边界不清 | STD real pass 可由参数控制 | pass 开启 fatal，关闭 warning+计数 drop |
| exceptionVec | 字段适配 | 可能统一复制或默认 valid | 各 port 位图不同 | 按 port 真实位复制，不存在位保持 0 |
| adapter 查询成本 | 性能边界 | 可能按 uid 全表反查 | monitor event 是高频路径 | LDA/STA 固定 active map 查询；STD 仅两个 ROB flag probe |

保持不变的主体逻辑：raw batch 入口、writeback handler 状态推进职责、current issue token owner、redirect-first batch 仲裁、pass/fail/terminal 收口、RM/checker/coverage deferred 状态。
