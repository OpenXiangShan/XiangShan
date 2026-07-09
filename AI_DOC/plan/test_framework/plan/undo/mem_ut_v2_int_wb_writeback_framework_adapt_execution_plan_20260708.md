# mem_ut V2 int writeback raw event 适配执行 Plan

## 1. Plan 定位

本文是 V2 `writebackLda/Sta/Std` split writeback 进入公共状态流的正式执行 plan。目标是保证 `dispatch_raw_int_wb_t -> memblock_wb_event_t -> writeback_status_handler` 的字段来源自洽。

本文不处理 monitor analysis port 输出分类；该内容由 monitor output plan 处理。

## 2. 范围边界

涉及文件：

```text
mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv
mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_monitor.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_batch_handler.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/writeback_status_handler.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv
```

允许修改：

- raw event 增加 `source_kind` 或等价枚举，区分 LDA/STA/STD。
- monitor 只对 V2 RTL 真实来源字段置 valid。
- adapter 根据 raw 中已有完整 key 和公共 active map 归一化 key。
- key 缺失时采用 fail-fast 策略：非必需 event drop + warning；若对应 target real writeback pass 被启用且该 event 是闭环必需，则 fatal，不能静默 drop 卡死。

不允许修改：

- 不把缺失字段在 connect 层猜常量后当真实 key。
- 不改变 writeback_status_handler 的主职责。
- 不实现 RM/checker/coverage。

### 2.1 执行前 RTL 基线确认

执行本 plan 前必须从仓库根目录确认当前 V2 RTL 权威输入真实存在：

```bash
test -e build/rtl/MemBlock.sv
test -e build_memblock/rtl/MemBlock.sv
test -e build_memblock/rtl/filelist.f
```

若任一文件不存在，必须先确认当前 worktree 的 RTL 生成状态和 V2 profile，不得继续沿用不存在的 `build_memblock/rtl/MemBlockTop.sv` 或同级旧 worktree 作为接口事实来源。本 plan 虽然修改 monitor/adapter 逻辑，但 V2 `writebackLda/Sta/Std` 字段来源必须以实际 RTL 为准；该检查不代表本 plan 会直接修改 RTL。

## 3. 问题依据

当前 monitor 对 port 0/1/2/3/4/5/6 直接设置：

```text
raw_int_wb.rob_valid = 1
raw_int_wb.lq_valid = 1 或 sq_valid = 1
raw_int_wb.exception_vec = interface exception bits
```

V2 接口事实：

- `writebackLda` 有 load writeback 字段和部分 exceptionVec。
- `writebackSta` 有 store address writeback 字段和部分 exceptionVec。
- `writebackStd_0/1` 当前确认只有 `valid` 和 `robIdx_value` 形态，不能继续读取 `robIdx_flag/sqIdx_flag/sqIdx_value`。
- 当前 `io_mem_to_ooo_int_wb_agent_connect.sv` 中的兼容命名 `io_mem_to_ooo_intWriteback_5/6_0_*` 只从 `writebackStd_0/1` 接入 `valid` 和 `robIdx_value`，没有 `sqIdx` 来源。
- 生成 RTL 内部仍可能存在 `_inner_stdExeUnits_*_io_out_bits_uop_sqIdx_*` wire，但这些 wire 没有作为当前顶层 `writebackStd_0/1` 输出，也没有接入当前 int writeback agent。测试框架当前阶段不得把它们当作 raw STD writeback 的合法 SQ key 来源。

当前 adapter 问题：

- `convert_raw_int_wb()` 只要 raw valid 就把 `real_wb_valid=1`。
- `raw_rob_to_key()`、`raw_lq_to_key()`、`raw_sq_to_key()` 直接按 valid 生成 key。
- key 缺失时没有 V2 专门反查策略。

## 4. 修改原因

公共状态表依赖 ROB/LQ/SQ key 反查 uid、issue_epoch 和 replay_seq。若 monitor 把 V2 无来源字段当成 valid key，会导致：

- writeback 误归属到错误 uid。
- stale event 无法正确过滤。
- fault/pass 更新错误 target。
- 主动 flow 可能因真正事件被误 drop 或误 pass 卡死。

## 5. 修改后方案

### 5.1 raw event 字段语义

建议在 `dispatch_raw_int_wb_t` 中新增：

```text
int unsigned source_kind; // 1=LDA, 2=STA, 3=STD
bit key_needs_state_lookup;
```

若不新增字段，也必须通过 `port_id` 明确：

```text
0/1/2 -> LDA
3/4 -> STA
5/6 -> STD
```

### 5.2 monitor 采样规则

monitor 对每类 port：

- LDA：只在 V2 真实有 rob/lq/exception 字段时置 `rob_valid/lq_valid/exception_vec`。如果 LQ key 字段在当前 V2 链路无来源，则 `lq_valid=0`，并设置需要 adapter 反查。
- STA：只在 V2 真实有 rob/sq/exception 字段时置 valid。
- STD：若只有 `robIdx_value`，则 `rob_valid` 只有在能获得完整 flag/value 时才置 1；否则本 plan 默认不做 ROB value-only 猜测反查，raw event 进入 adapter 后按缺 key 策略 drop/warning 或 fatal。不能把 `rob_flag=0` 当真实 key。
- STD 不允许从 `_inner_stdExeUnits_*_io_out_bits_uop_sqIdx_*` 直接补 `sq_valid/sq_key`，除非另建“内部 stdExeUnits SQ key 接管/采样”专项 plan，明确 internal wire 稳定性、connect 点、采样时序、与 top-level `writebackStd` 的一致性和重新生成 RTL 后的 profile 更新规则。

### 5.3 adapter 归一化规则

在 `dispatch_monitor_event_adapter` 增加 helper：

```text
normalize_v2_int_wb_key(raw, wb_event)
```

归一化优先级：

1. raw 中有完整 ROB/LQ/SQ key 时直接使用。
2. raw 有 ROB key 且缺 LQ/SQ：通过 `common_data_transaction` 的 active map 或 status 反查 target 对应 LQ/SQ。
3. raw 只有 ROB value 没 flag：本 plan 默认不能匹配，直接走缺 key 策略；不新增 ROB value-only 猜测反查。后续若要支持 value-only，必须另建 active-window 反查专项 plan，定义窗口、冲突处理和性能边界。
4. 无法得到 target 必需 key：若对应 target real writeback pass 未启用，打印 `uvm_warning` 后 drop；若 real writeback pass 已启用且该 target 依赖真实 writeback 闭环，必须 `uvm_fatal` 或至少 `uvm_error` 后请求 global stop，不能静默 drop 导致主动 flow 卡死。

与 real writeback pass 参数关系：

- `MEMBLOCK_STA_REAL_WB_PASS_EN=1` 时，STA 真实 writeback/pass 是闭环必需事件；STA raw event 无法归一化 key 必须 fail fast。
- `MEMBLOCK_STD_REAL_WB_PASS_EN=1` 时，STD 真实 writeback/pass 是闭环必需事件；STD raw event 缺 ROB flag/SQ key 时必须 fail fast。
- 对应参数为 0 时，兼容路径可由 IQ feedback 或既有策略推进，无法归一化的 raw event 可以 warning 后 drop，但必须记录计数或日志，便于 review 判断是否误丢关键事件。

不允许每个 event 全表扫描 `main_trans_num`。如需反查，只允许：

- active ROB map。
- uid 已解析后的 status。
- issue snapshot/status 中当前 target 的 key。
- 有明确上界的 active window helper。

### 5.4 STD SQ key 来源策略

当前 V2 STD writeback 对测试框架的可用来源分三层：

1. 顶层/agent 当前可用来源：`writebackStd_0/1_valid` 和 `writebackStd_0/1_bits_uop_robIdx_value`，只能说明有 STD writeback 和 ROB value。
2. 兼容 agent 命名：`io_mem_to_ooo_intWriteback_5/6_0_*` 只是测试框架旧接口名映射到 V2 `writebackStd_0/1`，不代表 V2 仍有完整 V3 intWriteback bundle。
3. RTL 内部 wire：`_inner_stdExeUnits_*_io_out_bits_uop_sqIdx_*` 可在当前生成 RTL 中出现，但不属于当前 agent/connect 的稳定接口事实。

因此当前执行 plan 的结论是：

- 不从 `intWriteback_5/6` 构造 `sq_valid=1`。
- 不从 `_inner_stdExeUnits_*_sqIdx_*` 隐式补 SQ key。
- 不做 ROB value-only 推测 ROB flag 或 SQ key。
- `STD_REAL_WB_PASS_EN=1` 时，如果 STD raw event 无法通过已有 active map/status 归一化为完整 key，必须 fail fast。
- `STD_REAL_WB_PASS_EN=0` 时，可以 warning 后 drop 该 STD raw event，并记录原因。

如果后续确实要使用 `_inner_stdExeUnits_*_sqIdx_*`，必须新建 DUT internal signal 采样专项，不得在本 plan 中作为“细节补字段”顺手接入。该专项至少要说明：

```text
internal wire 是否随 RTL 生成稳定
采样时序是否与 writebackStd valid 同拍一致
vector std/vstd mux 对 sqIdx 的影响
connect 文件和 agent interface 如何版本隔离
重新生成 RTL 后如何用 V2 profile 复查
```

## 6. 函数/任务级伪代码

### 6.1 `build_raw_int_wb_from_v2_port()`

函数目的：monitor 中按 V2 source port 构造 raw event，替代每个 port 手写不完整字段。

输入：

- `source_kind`
- `port_id`
- 当前采样到的 V2 interface 字段。

输出/副作用：

- 返回 `dispatch_raw_int_wb_t`。
- 只写真实来源字段的 valid。
- 调用 `memblock_sync_pkg::push_raw_int_wb()` 入队。

源码级伪代码：

```text
function dispatch_raw_int_wb_t build_raw_int_wb_from_v2_port(source_kind, port_id);
    raw = make_empty_raw_int_wb();
    raw.valid = sampled_valid;
    raw.port_id = port_id;
    raw.source_kind = source_kind;
    raw.cycle = $time;

    if (source_kind == LDA) begin
        raw.rob_valid = has_v2_rob_flag_and_value;
        raw.rob_flag = sampled_rob_flag;
        raw.rob_value = sampled_rob_value;
        raw.lq_valid = has_v2_lq_flag_and_value;
        raw.lq_flag = sampled_lq_flag;
        raw.lq_value = sampled_lq_value;
        raw.exception_vec = sampled_lda_exception_vec;
    end
    if (source_kind == STA) begin
        raw.rob_valid = has_v2_rob_flag_and_value;
        raw.sq_valid = has_v2_sq_flag_and_value;
        raw.exception_vec = sampled_sta_exception_vec;
    end
    if (source_kind == STD) begin
        raw.rob_valid = has_v2_full_rob_key;
        raw.sq_valid = has_v2_full_sq_key;
        raw.key_needs_state_lookup = missing_required_key;
        raw.rob_value_only_without_flag = sampled_rob_value_available_without_flag;
        // 不从 _inner_stdExeUnits_*_sqIdx_* 隐式补 sq_valid。
    end
    return raw;
endfunction
```

中文文字伪代码：

该函数在 monitor 每次看到 V2 writeback valid 时调用。它先创建空 raw event，再写 port 类型和采样时间。对 LDA，只采集 V2 真实提供的 ROB、LQ 和异常字段；字段不存在时对应 valid 保持 0。STA 同理只采集真实 ROB、SQ 和异常字段。STD 因为当前 V2 agent 可用链路只有 `writebackStd valid/robIdx_value`，不允许默认补 flag 或 SQ key；即使生成 RTL 内部存在 `_inner_stdExeUnits_*_sqIdx_*`，本 plan 也不把它当作当前 raw event 来源。若 key 不完整，函数只标记 key 缺失，后续 adapter 按 fail-fast 策略处理，不做 value-only 猜测。最后 monitor 只把 valid raw event 推入 raw queue。

### 6.2 `normalize_v2_int_wb_key()`

函数目的：adapter 中把 raw key 归一化成 `memblock_wb_event_t` 可消费的完整 key。

输入：`dispatch_raw_int_wb_t raw`、初始 `wb_event`。

输出/副作用：

- 成功时填 `has_rob/has_lq/has_sq` 和 key。
- 必要时调用 `data.resolve_uid_for_event()` 或 active map helper。
- 失败时返回 0，不更新公共状态。

源码级伪代码：

```text
function bit normalize_v2_int_wb_key(raw, ref wb_event);
    if (raw.rob_valid) wb_event.has_rob = raw_rob_to_key(...);
    if (raw.lq_valid) wb_event.has_lq = raw_lq_to_key(...);
    if (raw.sq_valid) wb_event.has_sq = raw_sq_to_key(...);

    if (required_key_present_for_target(wb_event)) return 1;

    if (raw has rob_value_only_without_flag) begin
        return fail_by_real_wb_policy(raw, wb_event.target, "ROB value without flag");
    end

    if (data.resolve_uid_for_event(wb_event, uid)) begin
        status = data.get_status(uid);
        wb_event.uid = uid;
        wb_event.has_uid = 1;
        fill_missing_key_from_status(status, wb_event.target, wb_event);
        return required_key_present_for_target(wb_event);
    end

    return fail_by_real_wb_policy(raw, wb_event.target, "key cannot be normalized");
endfunction
```

中文文字伪代码：

该函数先把 raw 中已经真实存在的 ROB/LQ/SQ key 写入 wb event。然后检查当前 target 是否已经具备必要 key。若 raw 只有 ROB value 而没有 flag，函数不猜默认 flag，也不做 value-only 反查，直接按 real writeback pass 策略失败。若 key 仍缺失但已有完整 key 可用于 active map 解析 uid，它才调用 `data.resolve_uid_for_event()`；解析成功后读取 status 中当前 uid 的 active LQ/SQ key，补齐缺失字段。若无法解析 uid或补齐后仍缺必要 key，函数按 `MEMBLOCK_STA_REAL_WB_PASS_EN` / `MEMBLOCK_STD_REAL_WB_PASS_EN` 决定 warning drop 还是 fatal。整个过程只使用 active map 或已解析 uid 的 status，不扫描全表。

### 6.3 `fail_by_real_wb_policy()`

函数目的：统一处理 V2 raw int writeback key 无法归一化时的 drop/fatal 策略。

输入：raw event、target、原因字符串。

输出/副作用：返回 0 表示 adapter 不生成 wb_event；real writeback pass 启用且 target 必需时 fatal；否则 warning。

源码级伪代码：

```text
function bit fail_by_real_wb_policy(raw, target, reason);
    if (target == STA && seq_csr_common::get_sta_real_wb_pass_en()) begin
        fatal("STA real writeback enabled but V2 raw event key missing");
    end
    if (target == STD && seq_csr_common::get_std_real_wb_pass_en()) begin
        fatal("STD real writeback enabled but V2 raw event key missing");
    end
    warning("drop V2 int wb raw event because key missing");
    return 0;
endfunction
```

中文文字伪代码：

该函数只在 adapter 无法归一化 key 时调用。它先看 target 是否是 STA 或 STD，再读取对应 real writeback pass 参数。如果参数打开，说明当前测试期望真实 writeback 事件推进 pass，缺 key 会导致闭环不可恢复，因此必须 fatal 暴露问题。如果参数关闭，说明当前可以依赖兼容路径推进，该 raw event 可以 warning 后 drop，但仍必须留下日志，避免误以为 writeback 已被正常处理。

### 6.4 `convert_raw_int_wb()`

函数目的：把 V2 source kind、key 归一化和原有 source/target 设置串起来。

源码级伪代码：

```text
function bit convert_raw_int_wb(raw, output wb_event);
    wb_event = make_wb_event_base();
    if (!raw.valid) return 0;
    set_source_and_target_by_port_or_kind(raw, wb_event);
    wb_event.real_wb_valid = 1;
    wb_event.exception_vec = raw.exception_vec;
    wb_event.has_exception = raw.exception_vec != 0;
    if (!normalize_v2_int_wb_key(raw, wb_event)) return 0;
    wb_event.cycle = raw.cycle;
    return 1;
endfunction
```

中文文字伪代码：

该函数先过滤无效 raw event，再根据 V2 port/source kind 设置 LOAD、STA 或 STD target。然后写真实 writeback 标志和异常向量。接下来调用 key 归一化函数；如果 key 不完整且无法反查，函数直接返回 0，表示 drop 该 raw event。只有 source、target、异常和 key 都自洽时，才返回成功并让 batch handler 继续补 uid、issue_epoch、replay_seq。

## 7. 验收标准

1. monitor 不再把 V2 无来源字段的 valid 固定为 1。
2. `writebackStd_0/1` 不再用悬空 `robIdx_flag/sqIdx_flag/sqIdx_value` 生成 key。
3. `convert_raw_int_wb()` 在 key 缺失时不会生成 `has_rob/has_lq/has_sq=1` 的误导 event。
4. key 反查只使用 active map、已解析 uid 的 status 或受限 active window，不新增每 event 全表扫描。
5. ROB value-only 无 flag 场景不做猜测反查；后续支持必须另建 plan。
6. `MEMBLOCK_STA_REAL_WB_PASS_EN` / `MEMBLOCK_STD_REAL_WB_PASS_EN` 打开时，对应 target key 无法归一化必须 fail fast，不静默 drop。
7. vector writeback 不进入本 scalar int writeback flow。
8. plan/review 明确区分“当前 agent/top-level 无 STD sqIdx 来源”和“RTL 内部可能仍有 `_inner_stdExeUnits_*_sqIdx_*` wire”；未建专项前不得使用内部 wire 作为当前测试框架 STD SQ key 来源。

## 8. 验证命令或静态检查

```bash
git diff --check -- mem_ut/ver/ut/memblock/common mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent mem_ut/ver/ut/memblock/seq/base_seq_help AI_DOC
rg -n "raw_int_wb\\.(rob|lq|sq)_valid\\s*=\\s*1'b1|robIdx_flag|sqIdx_flag|writebackStd" mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_monitor.sv mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv
rg -n "io_mem_to_ooo_intWriteback_[56].*sqIdx|writebackStd_.*sqIdx" mem_ut/ver/ut/memblock/tb/io_mem_to_ooo_int_wb_agent_connect.sv mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src build/rtl/MemBlock.sv build_memblock/rtl/MemBlock.sv
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
make eda_run tc=tc_sanity mode=base_fun
```

真实 dispatch 主流程修改后增加：

```bash
make eda_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=base_fun cfg=tc_dispatch_real_smoke
```

## 9. 与原始/初步 plan 差异说明

原始 int writeback plan 记录了字段问题和总伪代码。本文把它整理成可 coding 的 monitor 构造函数、adapter key 归一化函数和 `convert_raw_int_wb()` 修改方案，并明确 key 缺失时的 drop/error 策略与性能边界。

## 10. 风险与非目标

风险：

- 如果 V2 writeback 缺失完整 ROB key，仅靠 ROB value 反查可能不可靠；执行者必须优先确认 RTL 是否有 flag 来源。
- 若 STD pass 是当前 testcase 必需事件，而 key 无法归一化，仿真应失败暴露，而不是用兼容 pass 掩盖。
- 当前生成 RTL 内部可能存在 `_inner_stdExeUnits_*_sqIdx_*`，但它不是当前 int writeback agent 的稳定来源。直接使用内部 wire 会引入 RTL 生成名不稳定、vector std mux 时序和 connect 版本隔离风险。

非目标：

- 不恢复 monitor analysis port。
- 不实现 vector writeback。
- 不实现 RM/checker 对写回结果的正确性判断。
- 不在本 plan 中接入 `_inner_stdExeUnits_*_sqIdx_*` 内部 wire；如需接入，另建 internal signal 采样专项。

## 11. 与原测试框架逻辑对比和修改类型总结

修改类型结论：`局部逻辑适配 + 仅字段/参数适配`。monitor raw event 构造和 adapter key 归一化会改变局部实现；字段/参数适配集中在 V2 raw event valid/key/source 字段；`raw -> wb_event -> dispatch_monitor_batch_handler -> writeback_status_handler` 主 flow、handler 状态推进规则和主体架构不改变。

原测试框架逻辑：

- `io_mem_to_ooo_int_wb_agent_agent_monitor::mon_data()` 每拍采样 int writeback 相关字段，在 reset/backend done 后构造 `memblock_sync_pkg::dispatch_raw_int_wb_t` 并推入 raw queue。
- `dispatch_monitor_event_adapter::collect_writeback_events_batch()` 通过 `pop_raw_int_wb()` 取 raw event，调用 `convert_raw_int_wb()` 转成 `memblock_wb_event_t`。
- 原 `convert_raw_int_wb()` 按 `port_id` 0..2 归为 LOAD、3..4 归为 STA、5..6 归为 STD，并把 raw 的 ROB/LQ/SQ valid/flag/value 直接写入 event。
- 后续 batch handler 负责补 uid、issue_epoch、replay_seq；`writeback_status_handler::handle_event()` 再按 real writeback、issue feedback、fault/replay 更新 status。

本 plan 修改后逻辑：

- monitor 按 V2 split source port 构造 raw event，只对 RTL/connect 真实来源字段置 valid。
- adapter 先按 V2 source kind/port 得到 target，再通过 `normalize_v2_int_wb_key()` 确保必要 key 自洽；key 缺失时按 real writeback pass 策略 warning drop 或 fail-fast。
- STD 只有 ROB value、没有 flag/SQ key 时，不做 value-only 猜测，不构造 `has_rob/has_sq=1` 的伪完整 event。
- 对 STD，当前 agent/top-level 链路不提供 SQ key；即使 RTL 内部存在 `_inner_stdExeUnits_*_sqIdx_*`，本 plan 也不把内部 wire 作为当前 raw event 来源。
- handler 的 `mark_target_normal_pass()`、`mark_target_fault()`、`mark_issue_feedback_success()` 等状态推进规则保持不变。

逻辑改变项：

1. 新增 `build_raw_int_wb_from_v2_port()` 或等价 monitor helper。原因是 V2 `writebackLda/Sta/Std` 字段不是 V3 聚合 `intWriteback` 的完整 bundle，必须按真实来源置 valid。该 helper 只构造 raw event，不直接改 status。
2. 新增 `normalize_v2_int_wb_key()`。原因是 V2 某些 port 缺完整 key，需要在进入 handler 前 fail-fast 或受限补齐。该逻辑只使用 active map、已解析 uid 的 status 或受限窗口，不做全表扫描。
3. key 缺失 fail-fast 是局部逻辑适配。原因是 real writeback pass 打开时，缺 key 会让 pass/fail 闭环不可恢复；失败暴露比静默 drop 更符合测试框架生命周期。它不是主体 flow 改写，因为 raw queue、batch handler 和 writeback handler 仍保持原链路。
4. ROB value-only 场景不再猜 flag。原因是 ROB wrap flag 是 key 的一部分，value-only 反查不可靠。

字段/参数改变项：

- raw event 需要显式记录 V2 source kind 或 port kind，区分 LDA、STA、STD。
- raw event 中 `rob_valid/lq_valid/sq_valid` 只能由真实字段来源置位。
- `exception_vec` 只从 V2 port 真实存在的 exception bit 组合。
- STD raw event 不新增 `sqIdx` 字段来源；`sq_valid` 不能由 `intWriteback_5/6` 旧命名或 `_inner_stdExeUnits_*_sqIdx_*` 隐式置位。
- `MEMBLOCK_STA_REAL_WB_PASS_EN`、`MEMBLOCK_STD_REAL_WB_PASS_EN` 决定 key 缺失时 fail-fast 还是 warning drop。

性能/生命周期影响：

- RTL 基线路径确认只发生在执行前准备阶段，用于防止误读不存在的 `MemBlockTop.sv` 或错误 worktree，不属于测试框架 runtime 逻辑改变。
- 不新增每 event 全表扫描；反查只能走 active map、已解析 uid status 或明确受限 active window。
- 不改变 raw queue 消费方式，不改变 writeback batch 的出队顺序。
- 不改变 active map 插入/删除生命周期；若无法使用现有 map 解析，必须 fail/drop 而不是新增宽松状态。
- real writeback pass 打开时会更早 fatal，这是错误暴露策略变化，不是 terminal/pass/fail 定义变化。

覆盖性结论：

本 plan 覆盖 V2 split int writeback 进入公共状态表的必要适配，是 8 个 flow 中最明确的局部逻辑实现改变。它不恢复 analysis port、不实现 vector writeback、不修改 checker/RM。结论是：改变限于 V2 raw event 字段语义和 key 归一化，测试框架主体 writeback 状态机不变。

补充结论：当前源码检查对测试框架的影响是，STD writeback 不能依赖旧 `intWriteback_5/6 sqIdx` 或内部 `_inner_stdExeUnits_*_sqIdx_*` 来补 SQ key。当前 plan 已将该点纳入 STD key 缺失策略；如果 `STD_REAL_WB_PASS_EN` 打开而 key 仍不可归一化，应失败暴露，而不是用内部 wire 或默认 flag/value 临时拼 key。
