# mem_ut V2 测试框架适配修改 Plan

## 1. Plan 定位

本文是 `mem_ut_uvm_v2` 分支的 V2 测试框架运行期适配总控修改 plan。

本文不替代 DUT/interface/connect/agent 字段适配 plan。凡只涉及 `dut_inst.sv`、`*_agent_connect.sv`、agent interface、xaction、driver、monitor 字段集合与 V2 RTL 对齐的机械问题，仍归入 DUT interface 适配范围。本文只记录会影响测试框架运行期行为的事项，包括：

- 激励生成字段和 V2 split issue/LSQ enqueue 语义。
- monitor raw queue 到公共状态表的转换。
- writeback、commit、redirect、replay、fault 生命周期。
- L2TLB responder 查表和 response 字段语义。
- CSR/runtime snapshot、控制类 output 和 monitor transaction 输出策略。
- V2/V3 index、channel、FU 编码宽度的单一权威参数来源。

关联输入文档：

```text
AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_test_framework_logic_adapt_analysis_plan_20260707.md
AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_test_framework_logic_adapt_analysis_execution_plan_20260707.md
AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_dut_interface_adapt_plan_20260706.md
AI_DOC/analysis/interface/v2/mem_ut_v2_interface_alignment_issue_review_20260708.md
AI_DOC/analysis/interface/v2/mem_ut_v2_dut_framework_followup_notes_20260707.md
AI_DOC/analysis/interface/v2/memblock_v2whole_v3_memblock_interface_delta_20260707.md
AI_DOC/analysis/interface/v2/memblock_v2whole_v3_port_diff_detail_20260707.md
mem_ut/ver/ut/memblock/rule/version/v2/dut_interface_baseline.md
mem_ut/ver/ut/memblock/rule/version/v2/l2tlb_interface_profile.md
```

已有专项 plan：

```text
AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_int_wb_writeback_framework_adapt_plan_20260708.md
AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_monitor_output_framework_adapt_plan_20260708.md
```

## 2. 问题总览

当前 V2 RTL 相对 V3 的核心差异不是单纯端口改名，而是接口组织方式变化：

```text
V2:
  issueLda / issueSta / issueStd / issueVldu
  writebackLda / writebackSta / writebackStd / writebackVldu
  enqLsq_req 字段保留更多 uop 嵌套字段

V3:
  intIssue / vecIssue
  intWriteback / vecWriteback
  MemBlock 内部按 FU 类型 filter 到拆分 load/store/vector 通路
```

因此 V2 适配不能只依赖 connect 层把字段置 0 或改名。测试框架必须明确每个公共状态事件的语义来源，避免把 V2 不存在的字段当成真实 DUT event 输入。

## 3. 目标功能 Flow 总览

```text
V2 RTL/interface 事实
  -> 版本参数和字段语义归一
  -> LSQ enqueue 激励按 V2 enqLsq_req 补齐字段
  -> issue 激励按 V2 split issue port 生成
  -> writeback monitor 按 V2 split writeback port 生成 raw event
  -> dispatch_monitor_event_adapter 转换 raw event
  -> writeback/redirect/replay/fault handler 更新公共状态表
  -> L2TLB responder 按 V2 DTLB/L2TLB internal path 返回 response
  -> monitor output 分类决定 raw queue / analysis port / XZ only
  -> tc_sanity/base_fun 编译和基础运行闭环
```

执行原则：

1. 每个进入公共状态表的字段必须有 V2 RTL 来源、测试框架状态来源或明确默认值规则。
2. 每个 V2 无来源字段不得从悬空 interface 读取。
3. 每个默认值必须说明是 V2 语义固定不存在、当前 testcase 不覆盖，还是临时待后续专项支持。
4. 高频路径不得新增全表扫描；反查 uid、ROB/LQ/SQ key 时优先复用现有 map、status table、active window 或公共 helper。
5. coverage、checker、RM 正确性判断不进入本 plan 主流程；本 plan 只保证激励和运行期状态事件自洽。
6. 执行前必须重新确认 RTL 权威路径。当前 V2 整核接口分析以 `build/rtl/MemBlock.sv` 为主；若某个 plan 或脚本仍写 `build_memblock/rtl/MemBlockTop.sv`，执行时必须先确认该文件真实存在，否则不得把缺失文件静默当作已检查基线。

## 4. 必须写入测试框架适配的问题

### 4.1 V2 split issue 激励生成需要语义确认

问题：

V2 使用 `issueLda/issueSta/issueStd/issueVldu` 拆分端口；当前 `lintsissue_agent_agent_driver.sv` 和 `vecissue_agent_agent_driver.sv` 已按拆分端口驱动，但公共测试框架仍需要确认 `main_control_transaction`、`lsq_ctrl_model::derive_op_behavior()`、`issue_queue_scheduler` 和 `issue_field_assigner` 生成的字段是否满足 V2 源码语义。

源码依据：

```text
mem_ut/ver/ut/memblock/agent/lintsissue_agent_agent/src/lintsissue_agent_agent_driver.sv
mem_ut/ver/ut/memblock/agent/vecissue_agent_agent/src/vecissue_agent_agent_driver.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/issue_field_assigner.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/issue_queue_scheduler.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv
AI_DOC/analysis/interface/v2/memblock_v2whole_v3_memblock_interface_delta_20260707.md
```

必须处理：

1. 建立 `MEMBLOCK_ISSUE_TARGET_LOAD/STA/STD` 到 V2 `issueLda/Sta/Std` port 的权威映射。
2. 确认 `issueVldu` 当前是否属于本 plan 支持范围。若当前主流程只支持 scalar load/store，则 vector load/store issue 必须显式 drop/fatal 或转入后续 vector 专项 plan，不能静默当作 scalar 处理。
3. 检查 `issue_field_assigner` 中 `ftqOffset`、`ftqPtr`、`robIdx`、`lqIdx`、`sqIdx`、`fuType`、`fuOpType`、`numLsElem` 对 V2 位宽和语义是否一致。
4. 若 V2 源码中某个 `fuType/fuOpType/target` 组合不会产生对应 split issue port，测试框架不得把该组合作为合法行为激励。

后续落点：

新建或补充专项 coding plan：

```text
AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_split_issue_framework_adapt_plan_20260708.md
```

### 4.2 LSQ enqueue 字段和 idx 宽度需要版本化

问题：

`memblock_lsqenq_dispatch_base_sequence.sv` 当前按 `seq_csr_common::get_real_enq_width()` 和 `set_req_fields()` 生成 LSQ enqueue。V2 `enqLsq_req` 保留 `exceptionVec`、`flushPipe`、`fuOpType`、`lastUop`、`trigger` 等字段，且 ROB/FU type 位宽与 V3 存在差异。若这些字段继续默认为 0，必须证明该默认值是合法 V2 激励；若 V2 源码要求这些字段参与行为流，则必须补齐生成规则。

源码依据：

```text
mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqenq_dispatch_base_sequence.sv
mem_ut/ver/ut/memblock/agent/lsqenq_agent_agent/src/lsqenq_agent_agent_driver.sv
mem_ut/ver/ut/memblock/tb/lsqenq_agent_connect.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv
AI_DOC/analysis/interface/v2/memblock_v2whole_v3_port_diff_detail_20260707.md
```

必须处理：

1. 将 `MEMBLOCK_ROB_VALUE_W`、`MEMBLOCK_LQ_VALUE_W`、`MEMBLOCK_SQ_VALUE_W`、`MEMBLOCK_FUTYPE_W`、`MEMBLOCK_LSQ_ENQ_NUM` 等静态结构统一为版本参数，不允许散落硬编码。
2. `set_req_fields()` 不能只写 `fuType/uopIdx/rob/lq/sq/numLsElem`；必须对 V2 真实存在且影响行为的 `fuOpType/exceptionVec/flushPipe/lastUop/trigger` 给出生成、默认或不支持策略。
3. 若默认置 0，必须写明该字段在当前 testcase 激励目标中不触发对应异常/trigger/flush 行为，且不会让标签和实际激励语义不一致。
4. `collect_lsq_candidates()` 仍按 active window/cursor 推进，不得为 V2 字段补齐引入每拍全表扫描。

后续落点：

新建或补充专项 coding plan：

```text
AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_lsq_enqueue_framework_adapt_plan_20260708.md
```

### 4.3 int writeback raw event 已有专项 plan，必须作为阻塞项执行

问题：

V2 使用 `writebackLda/Sta/Std` split writeback。当前 `io_mem_to_ooo_int_wb_agent_agent_monitor.sv` 仍将部分 V3 聚合字段写入 `dispatch_raw_int_wb_t`，包括 `rob_flag/lq_flag/lq_value/sq_flag/sq_value/exception_vec[]`。若这些字段没有 V2 RTL 来源，会污染 `dispatch_monitor_event_adapter`、`writeback_status_handler`、`exception_redirect_replay_handler` 和公共状态表。

专项 plan：

```text
AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_int_wb_writeback_framework_adapt_plan_20260708.md
```

本总控 plan 对该专项 plan 增补约束：

1. `dispatch_monitor_event_adapter::convert_raw_int_wb()` 在 key 缺失时不得生成 `has_rob/has_lq/has_sq=1` 的误导事件。
2. `writebackStd_0/1` 若只有 `robIdx_value`，不得继续使用悬空的 `robIdx_flag/sqIdx_flag/sqIdx_value`。
3. 若从测试框架状态反查 key，必须说明 key 来源、生命周期、flush/replay/redirect 后如何失效或恢复。
4. 如果无法反查且该 event 是当前 testcase 必需事件，必须 `uvm_error` 或 `uvm_fatal`，不能静默 drop 后让主动 flow 卡住。

### 4.4 vector issue/writeback 当前不应被静默当作 scalar 支持

问题：

V2 暴露 `issueVldu` 和 `writebackVldu`。当前公共状态类型里已有 `vector_ls` 预留字段，但主流程注释说明当前初版不支持 vector LS writeback/replay。若现有 cfg 或 testcase 可能生成 `VLDU/VSTU/VSEGLDU/VSEGSTU`，必须在 V2 适配中给出显式策略。

源码依据：

```text
mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/issue_field_assigner.sv
mem_ut/ver/ut/memblock/agent/vecissue_agent_agent/src/vecissue_agent_agent_driver.sv
AI_DOC/analysis/interface/v2/memblock_v2whole_v3_memblock_interface_delta_20260707.md
```

必须处理：

1. 若当前 V2 smoke 不支持 vector LS，主表生成或 issue scheduler 必须禁止 vector op 进入真实 DUT flow，并用参数/标签说明。
2. 若允许生成 vector op，则必须新增 vector issue/writeback 专项 plan，不能复用 scalar LOAD/STA/STD handler 直接处理。
3. `dispatch_monitor_event_adapter` 中遇到 vector feedback/writeback 时必须保留当前 drop/fatal 边界，并在 plan 中说明原因。

后续落点：

本 plan 首轮默认不实现 vector LS 支持，只要求显式禁用或 fatal。完整 vector 支持另建专项 plan。

### 4.5 L2TLB V2 `s2_entry_perm_g/u` 常量化需要专项确认

问题：

V2 RTL 内部存在 `_inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_g/u`，当前 connect 将其固定为 0；`memblock_l2tlb_base_sequence::fill_dtlb_resp_from_entry()` 只填 `s2_entry_perm_d/a/x/w/r`，未填 `g/u`。若 V2 二阶段权限语义依赖 G/U 位，当前 responder response 会丢失权限信息。

源码依据：

```text
build/rtl/MemBlock.sv
mem_ut/ver/ut/memblock/tb/L2tlb_agent_connect.sv
mem_ut/ver/ut/memblock/seq/base_seq/memblock_l2tlb_base_sequence.sv
mem_ut/ver/ut/memblock/rule/version/v2/l2tlb_interface_profile.md
```

必须处理：

1. 保持 L2TLB agent 语义为 DTLB -> L2TLB request 和 L2TLB -> DTLB response，不得改成 L2Cache/PTW 下游模型。
2. 确认 V2 `_inner_ptw_io_tlb_1_resp_bits_s2_entry_perm_g/u` 是否应由 `tlb_entry` 的 `pte_g/pte_u` 驱动。
3. 若当前 testcase 不覆盖该语义，允许常量 0，但必须在 plan 和 review 中记录“不覆盖二阶段 G/U 权限”的边界。
4. 若需要支持，必须补齐 interface、xaction、driver、monitor、sequence `clear_l2tlb_xaction()` 和 `fill_dtlb_resp_from_entry()` 字段链路。

后续落点：

新建或补充专项 coding plan：

```text
AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_l2tlb_response_permission_adapt_plan_20260708.md
```

### 4.6 CSR control 命名差异需要语义分层，不可只按近义映射

问题：

V2 CSR control 使用 `btb_enable/ras_enable/sc_enable/tage_enable/ubtb_enable`、`hd_misalign_ld/st_enable`、`tlbCsr_priv_debug` 等字段。V3 中存在 `abtb/mbtb/ittage` 等不同字段集合。当前 connect 层已按 V2 字段连接，但测试框架 runtime CSR snapshot 和控制类 raw event 需要确认哪些字段参与当前 TLB/异常/redirect 行为，哪些只是旁路观察。

源码依据：

```text
mem_ut/ver/ut/memblock/tb/csr_ctrl_agent_connect.sv
mem_ut/ver/ut/memblock/agent/csr_ctrl_agent_agent/src/csr_ctrl_agent_agent_monitor.sv
mem_ut/ver/ut/memblock/common/memblock_common/src/memblock_sync_pkg.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv
AI_DOC/analysis/interface/v2/memblock_v2whole_v3_memblock_interface_delta_20260707.md
```

必须处理：

1. runtime CSR snapshot 只保存 TLB lookup、权限、sfence/hfence 和异常相关的必要字段。
2. branch predictor enable 类字段不得混入 TLB lookup key 或主表 pass/fault 判断。
3. `misalign_ld/st_enable`、`priv_debug` 若会影响异常激励合法性或 DUT 行为模拟层，必须单独记录默认值和 testcase 控制策略。
4. monitor analysis port 输出由 monitor 输出专项 plan 分类处理；本项只处理 raw queue/runtime snapshot 对公共状态的影响。

后续落点：

若确认当前 raw CSR 队列已满足 TLB lookup，本文只需在 implementation review 中记录无代码修改；若发现必要字段缺失，另建 CSR runtime snapshot 专项 plan。

### 4.7 monitor analysis port 和 V2-only output 已有专项 plan，必须先分类再 coding

问题：

顶层 agent monitor 大多没有执行 `mon_item_port.write(mon_tr)`。另外 V2-only output 如 `io_l2_tlb_req_resp_*`、`io_l2_pmp_resp_*`、`io_outer_l2PfCtrl_*`、`io_wfi_wfiSafe` 当前没有 agent 采样策略。它们可能不影响当前 `tc_sanity/base_fun`，但必须有主功能影响结论。

专项 plan：

```text
AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_monitor_output_framework_adapt_plan_20260708.md
```

本总控 plan 对该专项 plan 增补约束：

1. 每个 monitor 必须分类为 `XZ_ONLY`、`RAW_QUEUE`、`ANALYSIS_PORT` 或 `RAW_AND_ANALYSIS`。
2. 若已有 raw queue 足够支撑公共状态推进，不强制恢复 analysis port。
3. 若 RM/checker/coverage 需要标准 transaction，只记录协同入口；本 plan 不实现 RM/checker/coverage。
4. `io_l2_tlb_req_resp_*` 不得误接到内部 `L2TLB_agent`。

### 4.8 reset/halt/WFI 控制输出命名需要统一说明

问题：

V2 使用 `io_outer_cpu_halt`、`io_reset_backend`、`io_wfi_wfiSafe` 等控制/status 输出。历史文档和旧接口中可能仍用 `cpuWfi` 或 `WFI` 语义描述。若测试框架把 halt/WFI/reset backend 混用，会影响 reset 后启动、低功耗状态观察和 monitor 输出分类。

源码依据：

```text
mem_ut/ver/ut/memblock/tb/other_ctrl_agent_connect.sv
mem_ut/ver/ut/memblock/agent/other_ctrl_agent_agent/src/other_ctrl_agent_agent_monitor.sv
mem_ut/ver/ut/memblock/tb/top_tb.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/mem_base_sequence.sv
```

必须处理：

1. `memblock_sync_pkg::reset_backend_done` 仍是 testbench 同步标志，不等同于 DUT `io_reset_backend` output。
2. `io_outer_cpu_halt` 只作为 V2 halt/status output，不再称为 `cpuWfi`。
3. `io_wfi_wfiSafe` 是否进入 agent 由 monitor 输出专项 plan 判断；未接入前不得作为测试通过条件。

### 4.9 V2 MMIO/LSQ-ROB 状态信号需要明确公共状态语义

问题：

V2 暴露 `loadMmio/loadMmioUop/storeMmio/storeMmioUop/pendingMMIOld/pendingst/scommit` 等 LSQ/ROB 状态信号。当前接口层已能看到部分字段，`lsqcommit_agent` 也会驱动 `pendingMMIOld/pendingst/scommit`，但公共 raw event 和终态推进目前主要消费 `lq_deq/sq_deq/memoryViolation/sbIsEmpty`。如果这些 V2 MMIO/LSQ-ROB 状态影响 commit、terminal_done、pending、flushSb 或当前 testcase 的结束条件，不能只停留在 connect 层。

源码依据：

```text
build/rtl/MemBlock.sv
mem_ut/ver/ut/memblock/agent/lsqcommit_agent_agent/src/lsqcommit_agent_agent_driver.sv
mem_ut/ver/ut/memblock/agent/io_mem_to_ooo_ctrl_agent_agent/src/io_mem_to_ooo_ctrl_agent_agent_monitor.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/dispatch_monitor_event_adapter.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/common_data_transaction.sv
```

必须处理：

1. 先判断这些 MMIO/LSQ-ROB 状态是否属于当前 V2 smoke 主功能闭环。若不覆盖，必须在 plan/review 中写明当前 testcase 不依赖这些信号推进终态。
2. 若影响 commit 或 terminal_done，必须定义 raw event 字段、adapter 转换规则和状态表更新点，不能只在 monitor 中采样后丢弃。
3. 若只影响 debug/观察，不得把它们混入 pass/fail 或 terminal_done 判断。
4. `pendingMMIOld/pendingst/scommit` 若由 driver 驱动，必须说明默认值、驱动时机和与 LSQ commit/deq flow 的关系。
5. 新增状态消费不得在每拍全表扫描主表；应复用已有 deq/commit event、pending counter 或 active window。

后续落点：

新建或补充专项 coding plan：

```text
AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_lsq_mmio_status_framework_adapt_plan_20260708.md
```

### 4.10 参数和宏单一权威来源必须先落地

问题：

V2/V3 存在 ROB/LQ/SQ value width、FU type width、LSQ enqueue width、issue/writeback port 数量差异。当前 `memblock_dispatch_types.sv` 中 `MEMBLOCK_ROB_VALUE_W=9`、`MEMBLOCK_FUTYPE_*` 为 36 bit 常量，和 V2 detail 文档中部分 8 bit ROB、35 bit FU type 事实存在冲突风险。

源码依据：

```text
mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_dispatch_types.sv
mem_ut/ver/ut/memblock/seq/base_seq/memblock_lsqenq_dispatch_base_sequence.sv
mem_ut/ver/ut/memblock/cfg/memblock_compile_params.svh
mem_ut/ver/ut/memblock/env/plus.sv
mem_ut/ver/ut/memblock/seq/base_seq_help/seq_csr_common.sv
```

必须处理：

1. connect-time 静态结构放入 `memblock_compile_params.svh` 或版本 profile 指定的编译期宏体系。
2. runtime plusarg 不得改变 port 数、interface 数组维度或字段位宽。
3. 公共测试框架 helper 读取版本化常量时必须只有一个权威入口。
4. 若 V2 和 V3 不能共用同一个 packed typedef，必须用版本宏分支，不得用截断赋值掩盖位宽差异。

后续落点：

新建或补充专项 coding plan：

```text
AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_compile_param_and_width_adapt_plan_20260708.md
```

### 4.11 RTL 基线路径必须先修正，避免分析输入失真

问题：

历史 V2 analysis plan 中列出的 `build_memblock/rtl/MemBlockTop.sv` 在当前工作区可能不存在，而当前 V2 整核接口分析实际使用 `build/rtl/MemBlock.sv`。如果后续执行 plan 时沿用不存在的 `MemBlockTop.sv` 路径，会导致接口事实表缺项或静默跳过。

必须处理：

1. 每个 V2 适配专项 plan 执行前必须列出实际存在的 RTL 权威文件，并用 `test -e` 或等价检查确认。
2. 当前优先检查：

   ```text
   build/rtl/MemBlock.sv
   build_memblock/rtl/MemBlock.sv
   build_memblock/rtl/filelist.f
   ```

3. 如果 `MemBlockTop.sv` 不存在，不得继续把它写成已检查基线；必须改用当前 profile 或 analysis 文档确认的真实入口。
4. 该项属于 plan 输入风险，不直接修改测试框架运行期逻辑；但它必须作为所有 V2 专项 plan 的执行前检查项。

## 5. 不属于本文 coding 的问题

以下问题只归 DUT/interface/connect/agent 字段适配 plan，不进入本测试框架运行期修改：

1. `dut_inst.sv` 端口声明、方向、位宽与 V2 RTL 机械对齐。
2. `*_agent_connect.sv` 中不存在 RTL 路径或 interface 字段的机械错误。
3. 不进入 raw queue、公共状态表、sequence、handler、scheduler、adapter 的 agent 局部字段删除。
4. driver idle/reset 对新增 DUT input 字段的机械赋默认值。
5. monitor X/Z 检查字段与 interface 字段集合对齐，但不改变 raw queue 或 transaction 输出策略。

若这些问题影响公共状态事件或测试框架生命周期，必须重新归入第 4 节对应项。

## 6. 修改顺序

推荐执行顺序：

1. 先确认 RTL 权威路径，修正仍引用不存在 `MemBlockTop.sv` 的执行输入。
2. 执行参数和宽度适配计划，建立 V2/V3 静态结构单一权威来源。
3. 执行 LSQ enqueue 适配，确保 main table admission 到 LSQ key 的基础状态闭合。
4. 执行 split issue 适配，确保 LOAD/STA/STD target 驱动字段符合 V2 split port。
5. 执行 int writeback 专项 plan，确保 raw int writeback event 不再读取 V2 无来源字段。
6. 执行 LSQ MMIO/status 专项 plan，明确 V2 MMIO/LSQ-ROB 状态是否参与 commit、pending 和终态推进。
7. 执行 L2TLB response permission 专项 plan，明确 `s2_entry_perm_g/u` 策略。
8. 执行 monitor output 专项 plan，逐 agent 分类 analysis port/raw queue/XZ 职责。
9. 最后执行 CSR/control 输出影响复查，确认没有新的主功能依赖遗漏。

## 7. 验证与 smoke 方案

静态检查：

```bash
git diff --check -- mem_ut/ver/ut/memblock AI_DOC
rg -n "cpuWfi|intWriteback|vecWriteback|vecIssue|robIdx_flag|sqIdx_flag|s2_entry_perm_g|s2_entry_perm_u" mem_ut/ver/ut/memblock AI_DOC
```

参数一致性检查：

```bash
rg -n "MEMBLOCK_ROB_VALUE_W|MEMBLOCK_LQ_VALUE_W|MEMBLOCK_SQ_VALUE_W|MEMBLOCK_FUTYPE|MEMBLOCK_LSQ_ENQ" mem_ut/ver/ut/memblock
```

RTL 基线存在性检查：

```bash
test -e build/rtl/MemBlock.sv
test -e build_memblock/rtl/MemBlock.sv
test -e build_memblock/rtl/filelist.f
```

远端编译：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_compile tc=tc_sanity mode=base_fun
```

基础运行：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_run tc=tc_sanity mode=base_fun
```

若修改影响真实 dispatch 主流程，增加：

```bash
cd mem_ut/ver/ut/memblock/sim
make eda_run tc=basicTest ts=memblock_dispatch_real_smoke_vseq mode=base_fun cfg=tc_dispatch_real_smoke
```

通过标准：

- 编译无端口、层级、interface 字段、transaction 字段相关错误。
- `tc_sanity/base_fun` 不因 V2 无来源字段、错误 key、错误 vector 路径或 L2TLB response 字段进入 `UVM_FATAL`。
- 若运行失败，日志必须能定位到具体未支持路径，而不是静默卡死。

## 8. RM 协同支持

本 plan 不实现 RM/checker/scoreboard。

后续 RM/checker 可使用的测试框架输出入口包括：

- `dispatch_raw_int_wb_t` 转换后的 `memblock_wb_event_t`。
- monitor output 专项 plan 中标记为 `ANALYSIS_PORT` 或 `RAW_AND_ANALYSIS` 的 agent transaction。
- L2TLB responder 中明确填充的 TLB response 字段。
- LSQ enqueue 和 issue flow 中记录的 uid、ROB/LQ/SQ key、target、issue_epoch、replay_seq。

## 9. 功能覆盖率协同支持

本 plan 不实现 coveragent/covergroup。

后续功能覆盖率可采样：

- V2 split issue target：LOAD、STA、STD、VLD/VST 后续专项。
- V2 split writeback source：LDA、STA、STD、VLD 后续专项。
- key 来源：RTL 直接字段、状态表反查、明确默认值。
- unsupported/drop/fatal 分类标签。

## 10. 与初步 plan 差异说明

本计划相对 `mem_ut_v2_test_framework_logic_adapt_analysis_plan_20260707.md` 和执行 plan 的差异如下。

修改目的：

```text
分析 plan 只要求产出 V2 运行期影响分析，不提供 coding 顺序和专项落点。
本 plan 将已确认的问题转成后续可执行修改入口，并把已有专项 plan 纳入总控依赖。
```

修改前逻辑行为：

```text
接口适配 plan 只登记运行期影响；
analysis plan 只要求扫描 sequence、handler、scheduler、adapter、driver/responder、monitor service loop、env/RM/cfg 和状态表；
已发现问题分散在 interface review、followup notes 和两个专项 plan 中；
后续 coding 容易只执行 int_wb/monitor 两个专项，而遗漏 split issue、LSQ enqueue、L2TLB permission、CSR/control 和宽度参数化。
```

修改后逻辑行为：

```text
本 plan 先建立 V2 测试框架适配总控清单；
把已存在的 int writeback 和 monitor output 专项 plan 作为阻塞子计划；
新增 split issue、LSQ enqueue、LSQ MMIO/status、L2TLB permission、compile parameter/width 这五类必须生成专项 plan 的入口；
对 CSR/control、halt/WFI/reset backend 这类需要先复查的项，明确测试框架职责和不属于 DUT interface 机械适配的边界；
执行顺序从 RTL 基线确认、参数和静态结构开始，再到 admission/issue/writeback/MMIO-status/L2TLB/monitor，避免后续 coding 在位宽和字段语义未统一时修改运行期状态表。
```

差异影响：

```text
本 plan 不直接改变源码行为；
本 plan 改变后续 coding 的任务拆分和顺序；
本 plan 要求后续 coding 不能只以远端编译通过作为 V2 适配完成标准，还必须证明公共状态事件字段语义来源闭合；
本 plan 把 vector LS 当前默认为不支持/显式禁用或 fatal，避免误按 scalar 路径静默处理；
本 plan 要求 L2TLB s2_entry_perm_g/u 必须有明确策略，不能长期只在 review 中记录风险。
本 plan 要求 MMIO/LSQ-ROB 状态信号必须有“不覆盖”或“进入公共状态流”的明确结论。
本 plan 要求先确认实际 RTL 权威路径，避免沿用不存在的 `MemBlockTop.sv` 作为执行基线。
```
