# mem_ut V2 测试框架逻辑适配分析执行 Plan

## Plan 定位

本文是 `mem_ut_v2_test_framework_logic_adapt_analysis_plan_20260707.md` 的执行级 plan，只用于后续分析文档产出，不进行 coding，不修改源码。

本 plan 的目标是分析 V2 DUT/interface 变化是否会影响 mem_ut 测试框架运行期逻辑。分析结果必须区分三类：纯 interface/connect 差异、可通过参数或宏隔离的结构差异、确实需要测试框架逻辑改变的差异。

关联上层 plan：

```text
AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_test_framework_logic_adapt_analysis_plan_20260707.md
```

## 执行边界

1. 本 plan 只产出分析文档，不修改源码、RTL、脚本、cfg 或规则文件。
2. 分析必须先建立当前 V2 RTL 和 testbench 接口事实，再阅读现有测试框架文档和源码。
3. 不允许把可参数化的位宽、idx 宽度或 channel 数差异直接升级为运行期逻辑修改。
4. 不允许建议引入每拍全表扫描、无界 retry 或固定 `max_cycles` 作为正常退出条件。
5. 若确实需要 coding，必须在最终分析文档中作为后续专项 coding plan 输入，不在本 plan 中直接实现。

## 目标产物

后续执行本 plan 时新增分析文档：

```text
AI_DOC/analysis/framework_design/mem_ut_v2_test_framework_logic_adapt_analysis_20260707.md
```

分析文档必须包含：

1. 当前 V2 RTL 接口事实表。
2. 当前 testbench 和 agent 字段事实表。
3. 已阅读文档和关键源码清单。
4. 逐模块运行期影响分析。
5. 三类影响分类表。
6. 参数隔离、V2/V3 宏隔离和公共 helper 封装建议。
7. 不需要 coding 的项。
8. 需要回到 DUT/interface plan 处理的项。
9. 需要后续测试框架逻辑 coding plan 的项。
10. 风险和验证建议。

## 分析 Flow 总览

```text
检查当前分支和 V2 RTL 产物
  -> 建立 V2 RTL/interface 事实表
  -> 建立当前 testbench/agent 字段事实表
  -> 阅读 flow 文档和关键源码
  -> 按模块分析运行期依赖
  -> 将影响项分为三类
  -> 为可兼容项提出参数/宏/helper 方案
  -> 为确需逻辑改变项生成后续 coding plan 输入
  -> 写出 analysis 文档
  -> 执行文档格式和中文检查
```

## 分析逻辑伪代码

### 1. 接口事实建立伪代码

代码式伪代码：

```text
v2_interface_facts = collect_from_verilog(
    files=[
        "build_memblock/rtl/MemBlock.sv",
        "build_memblock/rtl/MemBlockTop.sv"
    ],
    fields=[
        port_name,
        direction,
        width,
        channel_index,
        valid_ready_role,
        request_response_role
    ]
)

tb_agent_facts = collect_from_testbench_and_agents(
    files=[
        "mem_ut/ver/ut/memblock/tb/top_tb.sv",
        "mem_ut/ver/ut/memblock/tb/dut_inst.sv",
        "mem_ut/ver/ut/memblock/tb/tc_if_connect.sv",
        "mem_ut/ver/ut/memblock/tb/memblock_connect.sv",
        "mem_ut/ver/ut/memblock/tb/*_agent_connect.sv",
        "mem_ut/ver/ut/memblock/agent/**/src/*",
        "mem_ut/ver/ut/memblock/subagent/**/src/*"
    ],
    fields=[
        signal_name,
        field_name,
        driver_use,
        monitor_use,
        transaction_use,
        rm_or_sequence_use
    ]
)

interface_delta = compare(v2_interface_facts, tb_agent_facts)
```

文字伪代码：

先从当前 V2 Verilog 中提取真实接口事实，包括端口名、方向、位宽、通道编号、valid/ready 角色和 request/response 方向。再从 testbench 和 agent 中提取当前验证环境事实，包括 DUT 实例连接、connect 引用、interface 字段、xaction 字段、driver 驱动字段、monitor 采样字段以及是否被 sequence、RM 或公共状态表使用。最后将两组事实对比，形成后续运行期分析的输入，避免先按 V3 文档或历史认知推断 V2 行为。

### 2. 运行期影响分类伪代码

代码式伪代码：

```text
for each delta in interface_delta:
    users = find_runtime_users(delta.field_or_signal)

    if users.only_in(["dut_inst", "agent_connect", "interface"]) and no_protocol_change(delta):
        classify(delta, "纯 interface/connect 差异")
        continue

    if is_static_width_channel_or_idx_delta(delta) and can_use_compile_param(delta):
        classify(delta, "可参数或宏隔离适配")
        suggest_param_or_macro(delta)
        continue

    if changes_protocol_lifecycle_or_state(delta):
        classify(delta, "需要测试框架逻辑改变")
        record_evidence(delta, users)
        create_coding_plan_input(delta, users)
        continue

    classify(delta, "待确认")
    record_missing_evidence(delta)
```

文字伪代码：

对每个接口差异先查找运行期使用者。如果差异只影响 `dut_inst.sv`、connect 文件或 interface 字段，并且没有改变握手、生命周期和状态推进，则归为纯 interface/connect 差异。如果差异是位宽、idx 宽度、channel 数或数组维度变化，并且可由编译期参数或版本宏统一表达，则归为可参数或宏隔离适配。如果差异改变 valid/ready 时序、request/response 生命周期、monitor event 含义、状态表推进条件、sequence 退出条件或 responder 驱动语义，则归为需要测试框架逻辑改变，并记录源码证据和后续 coding plan 输入。证据不足的项不能直接下结论，必须标为待确认。

### 3. 逐模块阅读和判断伪代码

代码式伪代码：

```text
modules = [
    "sequence",
    "handler",
    "scheduler",
    "adapter",
    "driver/responder",
    "monitor service loop",
    "env/RM/cfg",
    "状态表生命周期"
]

for each module in modules:
    source_files = locate_source_files(module)
    flow_docs = locate_flow_docs(module)
    read(flow_docs)
    read(source_files)

    for each delta related to module:
        evidence = inspect_source_usage(delta, source_files)
        if evidence.requires_logic_change:
            record_logic_change_candidate(module, delta, evidence)
        else:
            record_no_logic_change_reason(module, delta, evidence)
```

文字伪代码：

按 sequence、handler、scheduler、adapter、driver/responder、monitor service loop、env/RM/cfg 和状态表生命周期逐项阅读文档和源码。每读一个模块，都要把接口差异映射到该模块的真实使用点：字段在哪里产生，在哪里转换，在哪里入队，在哪里消费，在哪里清理，是否影响 flush/replay/redirect 恢复。若源码证据显示只是字段名或静态宽度变化，则记录不需要运行期逻辑修改；若证据显示状态生命周期、事件时序或退出条件发生变化，则记录为后续 coding plan 候选项。

### 4. 兼容策略生成伪代码

代码式伪代码：

```text
for each classified_delta:
    if delta.category == "可参数或宏隔离适配":
        if compile_time_structure(delta):
            propose("memblock_compile_params.svh macro or parameter")
        else:
            propose("seq_csr_common getter or common helper")

    if delta.category == "需要测试框架逻辑改变":
        propose_new_coding_plan(
            problem=delta.problem,
            evidence=delta.evidence,
            affected_files=delta.users,
            boundary="do not mix with interface-only coding",
            verification=delta.verification_hint
        )
```

文字伪代码：

对可兼容项先判断它属于编译期静态结构还是运行期 helper 语义。编译期静态结构，例如端口是否存在、channel 数、数组维度和 idx 位宽，建议落在 `memblock_compile_params.svh` 或既有版本宏体系。运行期 helper 语义，例如根据版本选择 getter 或统一字段转换，建议单独生成专项 coding plan，不能藏在分析文档里直接修改。对确实需要逻辑改变的项，必须整理成后续 coding plan 输入，包含问题、证据、影响文件、边界、验证入口和风险。

## 必读文档

执行本 plan 时，至少阅读：

```text
mem_ut/ver/ut/memblock/rule/memblock_l2tlb_agent_rule.md
mem_ut/ver/ut/memblock/rule/version/v2/l2tlb_interface_profile.md
AI_DOC/mem_ut_flow_doc/main_table_build_and_stimulus_flow.md
AI_DOC/mem_ut_flow_doc/virtual_sequence_unified_dispatch_flow.md
AI_DOC/mem_ut_flow_doc/lsq_admission_flow.md
AI_DOC/mem_ut_flow_doc/load_sta_std_issue_flow.md
AI_DOC/mem_ut_flow_doc/rob_commit_lq_sq_deq_flow.md
AI_DOC/mem_ut_flow_doc/writeback_function_call_flow.md
AI_DOC/mem_ut_flow_doc/replay_flow.md
AI_DOC/mem_ut_flow_doc/redirect_flow.md
AI_DOC/mem_ut_flow_doc/tlb_l2tlb_responder_flow.md
AI_DOC/mem_ut_flow_doc/csr_runtime_sync_flow.md
AI_DOC/analysis/source_sv/common_data_transaction_function_analysis.md
AI_DOC/analysis/framework_design/dispatch_backend_interface_closure_code_changes.md
AI_DOC/analysis/framework_design/dispatch_testbench_global_sync.md
```

若文档缺失，必须在 analysis 文档中记录缺失项、影响范围和风险。

## 必查源码入口

```text
mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/main_control_transaction.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_batch_handler.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/issue_queue_scheduler.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_ctrl_model.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/lsq_commit_handler.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/writeback_status_handler.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/exception_redirect_replay_handler.sv
mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqenq_dispatch_base_sequence.sv
mem_ut/ver/ut/memblock/seq/base_seq/memblock_issue_dispatch_base_sequence.sv
mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv
mem_ut/ver/ut/memblock/env/src/memblock_env.sv
mem_ut/ver/ut/memblock/env/src/memblock_env_cfg.sv
mem_ut/ver/ut/memblock/env/src/memblock_rm.sv
```

## 分析输出模板

最终 analysis 文档中，每个影响项必须按以下格式记录：

| 字段 | 要求 |
| --- | --- |
| 影响项 | 明确写出端口、字段、channel 或状态语义 |
| V2 RTL 事实 | 来自当前 Verilog 的端口、方向、位宽和协议事实 |
| 当前框架使用点 | 写出源码路径、类、函数或字段 |
| 分类 | 三类之一：纯 interface/connect 差异、可参数或宏隔离适配、需要测试框架逻辑改变 |
| 分类理由 | 用源码证据说明为什么这么分类 |
| 建议处理 | 回到 interface plan、生成参数/helper plan、或生成运行期逻辑 coding plan |
| 验证入口 | 静态检查、编译或后续仿真建议 |

## 验收标准

1. analysis 文档先列 V2 RTL/interface 事实，再给测试框架判断。
2. 每个“需要逻辑改变”的结论都有源码证据。
3. 每个可参数化差异都优先给出参数或宏隔离方案。
4. 没有建议在高频路径新增全表扫描、无界 retry 或固定 `max_cycles` 正常退出条件。
5. 没有把 coverage、scoreboard 或 RM 正确性判断混入测试框架运行期逻辑分析。
6. 文档正文为中文，英文只用于路径、标识符、命令和固定术语。

## Plan 对齐检查

本执行 plan 与上层分析 plan 保持一致：

1. 只分析，不 coding。
2. 先梳理 V2 RTL 和 interface 事实。
3. 再阅读文档和关键源码。
4. 按三类影响分流。
5. 优先参数隔离、版本宏隔离和公共 helper 封装。
6. 只有接口语义或状态生命周期确实不同，才允许后续 coding plan 修改测试框架逻辑。
