# mem_ut V2 int writeback 测试框架适配修改 Plan

## 1. Plan 定位

本文记录 `io_mem_to_ooo_int_wb_agent` 适配 V2 `writebackLda/Sta/Std` split writeback 后，对测试框架运行期逻辑的后续修改计划。

本 plan 只覆盖会进入公共状态流的 int writeback 字段，也就是从 monitor 写入：

```text
memblock_sync_pkg::dispatch_raw_int_wb_t
```

再由 `dispatch_monitor_event_adapter.sv`、`dispatch_monitor_batch_handler.sv`、`writeback_status_handler.sv`、`exception_redirect_replay_handler.sv` 和公共状态表消费的字段。

不进入 `dispatch_raw_int_wb_t`、不影响公共状态流的 V2 无来源字段不归本 plan 实现，应在 interface/xaction/monitor 清理中直接删除。

monitor analysis port 输出策略和 V2 新增顶层 output 主功能影响分析不归本 plan 覆盖，统一由以下 plan 跟踪：

```text
AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_monitor_output_framework_adapt_plan_20260708.md
```

## 2. 问题来源

V2 整核 `MemBlock.sv` 中，int writeback 已从 V3 聚合 `intWriteback_*` 形态变为 V2 split 形态：

```text
writebackLda_0/1/2
writebackSta_0/1
writebackStd_0/1
```

当前 `io_mem_to_ooo_int_wb_agent` 仍保留部分 V3 聚合字段。部分字段没有 V2 RTL 来源，却会被 monitor 写入 `dispatch_raw_int_wb_t`，进而影响测试框架公共状态。

需要重点处理的字段包括：

| V2 writeback 类别 | 当前风险字段 | 当前风险 |
|---|---|---|
| `writebackLda_0/1/2` | 缺失的 `exceptionVec[]` 位、`lqIdx_flag/value` | 这些字段进入 `raw_int_wb.exception_vec`、`lq_flag`、`lq_value`，会影响 load fault/exception 和 LQ key 匹配。 |
| `writebackSta_0/1` | `sqIdx_flag/value`、部分聚合 `exceptionVec[]` | 这些字段进入 `raw_int_wb.sq_flag`、`sq_value`、`exception_vec`，会影响 store address fault/exception 和 SQ key 匹配。 |
| `writebackStd_0/1` | `robIdx_flag`、`sqIdx_flag/value` | V2 顶层仅暴露 `valid` 和 `robIdx_value`，当前 raw event 仍尝试写 `rob_flag/sq_flag/sq_value`。 |

## 3. 非测试框架直接删除项：已完成

以下字段不写入 `dispatch_raw_int_wb_t`，也不参与公共状态表、handler 或 event adapter。它们没有 V2 RTL 来源，应从 interface、xaction、monitor 局部变量、X/Z 检查和注释掉的 transaction 赋值中删除。本轮已完成删除：

```text
io_mem_to_ooo_intWriteback_4_0_bits_toRob_bits_isRVC
io_mem_to_ooo_intWriteback_3_0_ready
io_mem_to_ooo_intWriteback_3_0_bits_toRob_bits_isRVC
io_mem_to_ooo_intWriteback_3_0_bits_pdest
io_mem_to_ooo_intWriteback_2_0_bits_toRob_bits_isRVC
io_mem_to_ooo_intWriteback_1_0_bits_toRob_bits_isRVC
io_mem_to_ooo_intWriteback_0_0_bits_toRob_bits_isRVC
```

这些字段不作为测试框架逻辑修改项保留待办。后续 coding 只需要继续处理第 4 节中进入公共状态流的字段语义来源。

## 4. 测试框架待办项

### 4.1 `rob_flag` 语义来源

当前 raw event 使用：

```text
raw_int_wb.rob_valid
raw_int_wb.rob_flag
raw_int_wb.rob_value
```

V2 部分 writeback 端口只有 `robIdx_value`，没有 `robIdx_flag`。后续修改必须明确：

1. `rob_valid` 是否可由对应 `writeback*_valid` 直接表示。
2. `rob_flag` 是否能从 V2 RTL 其他信号获取。
3. 如果 V2 RTL 没有 `rob_flag`，是否应从测试框架 ROB 分配状态表反查。
4. 如果当前 V2 测试范围不需要 flag，是否允许在 adapter 层用明确规则常量化，并说明不会造成 ROB key 冲突。

### 4.2 `lq_flag/lq_value` 语义来源

`writebackLda_0/1/2` 当前 raw event 需要：

```text
raw_int_wb.lq_valid
raw_int_wb.lq_flag
raw_int_wb.lq_value
```

后续修改必须明确 V2 load writeback 的 LQ key 来源：

1. 若 V2 writeback 端口真实存在 LQ index 字段，则补齐 connect/interface/monitor。
2. 若 V2 writeback 不再携带 LQ index，则从 issue/enqueue 时测试框架保存的 uid 到 LQ key 映射反查。
3. 若无法反查，adapter 不得生成 `has_lq=1` 的 writeback event，应记录 warning 或 fatal 策略。

### 4.3 `sq_flag/sq_value` 语义来源

`writebackSta_0/1` 和 `writebackStd_0/1` 当前 raw event 需要：

```text
raw_int_wb.sq_valid
raw_int_wb.sq_flag
raw_int_wb.sq_value
```

后续修改必须明确 V2 store writeback 的 SQ key 来源：

1. `writebackSta_0/1` 若存在真实 SQ index 字段，则补齐链路。
2. `writebackStd_0/1` 当前只确认暴露 `valid` 和 `robIdx_value`，不能继续读取未连接的 `sqIdx_flag/value`。
3. 对 `writebackStd_0/1`，优先从 ROB key 或 uid 反查 SQ key；若不能反查，需要调整 adapter 规则，避免生成错误的 store data writeback 状态事件。

### 4.4 `exception_vec` 语义来源

当前 monitor 会把部分未连接的 `exceptionVec[]` 位写入：

```text
raw_int_wb.exception_vec
```

后续修改必须逐 port 确认 V2 `writebackLda/Sta/Std` 的异常向量来源：

1. V2 RTL 存在的异常位保留真实连接。
2. V2 RTL 不存在但 V3 聚合形态中存在的异常位不能继续从 interface 读取。
3. 如果缺失异常位在 V2 语义中固定不存在，应在 adapter 或 monitor 中显式置 0，并在文档中说明来源。
4. 如果缺失异常位需要由其他 V2 信号或框架状态推导，必须补充 helper，并明确输入、输出和失败策略。

## 5. 修改 Flow 伪代码

代码式伪代码：

```text
for each raw int writeback event from monitor:
    identify v2_port_type = LDA / STA / STD
    collect connected RTL fields only

    if field has V2 RTL source:
        raw.field = connected_value
    else if field is required by dispatch_raw_int_wb_t:
        raw.field = derive_from_framework_state_or_defined_default()
        record derivation rule
    else:
        remove field from agent interface/xaction/monitor

    if required key cannot be derived:
        do not emit misleading key-valid event
        report warning or fatal according to configured policy

    push raw event only after rob/lq/sq/exception semantics are self-consistent
```

文字伪代码：

monitor 先按 V2 真实 writeback port 类型区分 load、store address 和 store data。对每个 port，只采集当前 V2 RTL 真实存在的字段。若字段是 `dispatch_raw_int_wb_t` 必需字段但 V2 port 不直接提供，不能从未连接 interface 读取，而要调用测试框架状态表或 helper 推导；如果推导不到，就不能把对应 key-valid 置 1 后继续入队。对不进入公共状态的字段，直接从 agent 局部链路删除。最后只有在 ROB/LQ/SQ key 和 exception 语义自洽时，才把 raw event 推入公共队列。

## 6. 影响文件

后续 coding 至少需要检查：

```text
mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_int_wb_agent_agent/src/io_mem_to_ooo_int_wb_agent_agent_monitor.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_batch_handler.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/writeback_status_handler.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/exception_redirect_replay_handler.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv
mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv
```

如果新增 helper 或参数，还需同步检查：

```text
mem_ut/ver/ut/memblock/env/plus.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv
mem_ut/ver/ut/memblock/seq/plus_cfg/default.cfg
```

## 7. 验收标准

1. `io_mem_to_ooo_int_wb_agent` 不再读取 V2 RTL 无来源的 interface 字段。
2. 进入 `dispatch_raw_int_wb_t` 的每个字段都有 V2 RTL 来源、测试框架状态来源或明确默认值规则。
3. `writebackStd_0/1` 不再用未连接的 `robIdx_flag/sqIdx_flag/sqIdx_value` 生成公共状态事件。
4. `dispatch_monitor_event_adapter.sv` 不会把缺失 key 的 raw event 错误转换成 `has_lq/has_sq/has_rob=1` 的 event。
5. 文档中记录每个默认值或推导规则的语义来源。
6. 通过 `git diff --check -- mem_ut/ver/ut/memblock AI_DOC`。
7. 后续 coding 完成后执行远端编译；如果触发运行期状态流修改，还需执行 `tc_sanity/base_fun` 仿真。
