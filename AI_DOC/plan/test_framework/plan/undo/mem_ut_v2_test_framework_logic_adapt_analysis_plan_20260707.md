# mem_ut V2 测试框架运行期逻辑适配分析 Plan

## Plan 定位

本文是 V2 测试框架运行期逻辑适配的分析 plan，只用于后续分析，不进行 coding，不修改源码，不实现测试框架适配。

本 plan 接收 DUT/interface/connect/agent 字段适配 plan 中登记的运行期影响项，分析 V2 接口变化是否会导致 sequence、handler、scheduler、adapter、driver/responder、monitor service loop、env/RM/cfg 或状态表生命周期变化。

## 当前基线

分析必须先梳理当前生成的 V2 RTL 接口和协议事实，再阅读现有测试框架文档、flow 文档、analysis 文档和关键逻辑代码。

当前接口权威来源：

```text
build_memblock/rtl/filelist.f
build_memblock/rtl/MemBlock.sv
build_memblock/rtl/MemBlockTop.sv
mem_ut/ver/ut/memblock/tb/top_tb.sv
mem_ut/ver/ut/memblock/tb/dut_inst.sv
mem_ut/ver/ut/memblock/tb/memblock_connect.sv
mem_ut/ver/ut/memblock/tb/*_agent_connect.sv
```

## 目标

1. 建立当前 V2 RTL 接口事实表，覆盖端口、方向、位宽、channel 数、idx 宽度、valid/ready 关系和 request/response 方向。
2. 阅读现有测试框架文档、flow 文档、analysis 文档和关键逻辑代码。
3. 分析 V2 接口变化是否导致测试框架运行期逻辑变化。
4. 区分三类影响：纯 interface/connect 差异；字段、位宽、channel 数可通过参数或宏隔离适配；确实需要测试框架逻辑改变。
5. 优先要求通过参数隔离、编译期 V2/V3 宏隔离、公共 helper 封装来兼容两个版本。
6. 只有分析证明接口语义或状态生命周期不同，才允许后续 plan 提出逻辑修改。
7. 产出分析结论、影响清单和后续 coding plan 输入。

## 非目标

1. 不修改源码。
2. 不实现 DUT/interface/connect/agent 字段适配。
3. 不实现测试框架适配。
4. 不新增 sequence、handler、scheduler、adapter、driver/responder、monitor service loop、env、RM 或 cfg 功能。
5. 不实现 coverage、scoreboard 或 RM 算法。
6. 不修改生成后的 V2 RTL，不修改 Scala 源码。
7. 不用本 plan 替代后续 coding plan。

## 验收标准

- AC-1：接口和协议事实先行
  - 正向检查：分析文档先列出当前 V2 RTL 接口、方向、位宽、channel 数、valid/ready 关系和关键语义。
  - 反向检查：不得只依赖旧 seed 文档、历史 V3 认知或单个编译报错。

- AC-2：文档和代码阅读范围完整
  - 正向检查：已阅读现有测试框架 plan、flow 文档、analysis 文档和关键逻辑代码。
  - 正向检查：每个被判定为“需要逻辑改变”的结论都有对应源码路径、函数或状态表依据。

- AC-3：影响分类清晰
  - 正向检查：每个影响项被归入“纯 interface/connect 差异”“可参数或宏隔离适配”“需要测试框架逻辑改变”三类之一。
  - 反向检查：不能把可参数化的位宽或 channel 数差异直接升级成运行期逻辑修改。

- AC-4：兼容策略优先
  - 正向检查：对字段、位宽、idx、channel 数差异，优先给出统一参数、编译期宏或公共 helper 封装方案。
  - 反向检查：只有接口语义或状态生命周期确实不同，才允许后续 coding plan 修改运行期逻辑。

- AC-5：运行期逻辑风险覆盖
  - 正向检查：sequence、handler、scheduler、adapter、driver/responder、monitor service loop、env/RM/cfg、状态表生命周期均有检查结论。
  - 反向检查：不能引入每拍全表扫描、无界 retry 或固定 `max_cycles` 作为正常退出建议。

- AC-6：输出可作为后续 coding plan 输入
  - 正向检查：每个后续 coding 候选项包含问题描述、证据、影响文件、建议边界、验证入口和风险。

## 路径边界

允许阅读和分析：

```text
build_memblock/rtl/filelist.f
build_memblock/rtl/MemBlock.sv
build_memblock/rtl/MemBlockTop.sv
AI_DOC/analysis/**
AI_DOC/mem_ut_flow_doc/**
AI_DOC/plan/test_framework/plan/**
mem_ut/ver/ut/memblock/tb/**
mem_ut/ver/ut/memblock/agent/**
mem_ut/ver/ut/memblock/subagent/**
mem_ut/ver/ut/memblock/env/**
mem_ut/ver/ut/memblock/seq/**
mem_ut/ver/ut/memblock/common/**
mem_ut/ver/ut/memblock/cfg/**
```

后续执行本 plan 时允许产出分析文档，建议路径：

```text
AI_DOC/analysis/framework_design/mem_ut_v2_test_framework_logic_adapt_analysis_20260707.md
```

本 plan 不允许修改任何源码、RTL、脚本、cfg 或规则文件。

## 分析流程

1. 建立 V2 接口和协议事实：从当前 V2 Verilog、testbench connect、agent interface/xaction/driver/monitor 抽取端口、方向、位宽、channel 数、valid/ready、request/response 和 idx 合法范围。
2. 阅读现有文档和关键代码：覆盖 sequence、handler、scheduler、adapter、driver/responder、monitor service loop、env/RM/cfg 和状态表相关代码。
3. 对接口变化进行三类分类：

| 影响项 | 接口事实 | 现有框架使用点 | 分类 | 分类理由 | 后续处理 |
| --- | --- | --- | --- | --- | --- |
| 待填写 | 待填写 | 待填写 | 纯 interface/connect 差异 / 可参数或宏隔离适配 / 需要逻辑改变 | 待填写 | 待填写 |

4. 逐模块分析运行期影响：
   - sequence：检查字段名、channel 数、idx 宽度、transaction 生成顺序、fallback/fatal 策略。
   - handler 和 scheduler：检查事件分发、候选选择、channel/idx 依赖和高频扫描风险。
   - adapter：检查 monitor item 到公共 transaction 或状态表字段的转换边界。
   - driver/responder：检查 DUT input/output 方向、request/response 生命周期和 L2TLB responder 语义。
   - monitor service loop：检查 valid/ready 采样点、raw queue、pending counter 和退出条件。
   - env/RM/cfg：检查 agent 集成、analysis port、RM 消费字段和参数分类。
   - 状态表生命周期：检查状态创建、激活、更新、消费、删除、flush/replay/redirect 恢复。

## 必读文档和源码入口

执行本 plan 时，至少阅读以下文档：

```text
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

至少检查以下源码入口：

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

如果某个文档或源码入口缺失，必须在最终 analysis 中记录缺失项和风险，不得静默跳过。

## 里程碑

1. V2 接口事实表：产出当前 V2 RTL 接口事实表。
2. 现有测试框架阅读摘要：覆盖 sequence、handler、scheduler、adapter、driver/responder、monitor service loop、env/RM/cfg、状态表。
3. 影响分类表：每个影响项都有分类理由和后续处理建议。
4. 兼容策略建议：产出参数隔离、编译期 V2/V3 宏隔离和公共 helper 封装建议。
5. 后续 coding plan 输入：每个 coding 候选项包含问题、证据、影响文件、建议边界、验证入口和风险。

## 输出文档要求

后续执行本 analysis plan 时，建议新增分析文档：

```text
AI_DOC/analysis/framework_design/mem_ut_v2_test_framework_logic_adapt_analysis_20260707.md
```

分析文档必须包含 V2 RTL 接口事实表、当前 testbench 和 agent 字段事实表、已阅读文档和关键代码清单、逐模块影响分析、三类影响分类表、参数隔离和 helper 封装建议、不需要 coding 的项、需要后续 interface plan 处理的项、需要后续测试框架逻辑 coding plan 的项、风险和验证建议。

## 风险与处理

| 风险 | 影响 | 处理 |
| --- | --- | --- |
| 先读旧文档再推断 V2 行为 | 旧 V3 认知污染分析结论 | 先建立当前 V2 RTL 接口和协议事实，再读文档和源码 |
| 把字段名变化误判为逻辑变化 | 后续 coding plan 过度修改运行期逻辑 | 三类分类中优先归入纯 interface/connect 或参数/宏隔离 |
| 把 channel 数变化散落硬编码 | V2/V3 维护成本上升 | 优先统一参数或编译期宏，并保持单一权威来源 |
| 为兼容 V2 删除 V3 支持 | 破坏双版本验证环境 | 使用 V2/V3 编译期宏隔离，保留两个版本路径 |
| 为规避复杂逻辑引入全表扫描 | 高频路径性能退化 | 按逻辑构建规则优先 cursor、map、queue、pending counter、pool |
| L2TLB 语义误判 | responder 方向和 lookup 生命周期错误 | 分析 L2TLB 时先读专项规则和 V2 profile，确认 DTLB/L2TLB 上游 request/response |

## 后续衔接

本 analysis plan 完成后，按结论分流：

1. 纯 interface/connect 差异：回到 `mem_ut_v2_dut_interface_adapt_plan_20260706.md` 执行。
2. 可参数或宏隔离适配：若只影响 connect-time 静态结构，回到接口 plan；若影响 runtime getter/helper，生成专项 coding plan。
3. 确实需要测试框架逻辑改变：新建测试框架逻辑 coding plan。
