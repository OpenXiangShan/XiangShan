# Ask Codex Input

## Question

你是独立文档主审。只读审查当前工作区以下12份目标文档，不编辑任何文件，不评论其它脏改动：AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_pending_mmio_load_sideband_execution_plan_20260710.md；AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_lsq_mmio_status_framework_adapt_execution_plan_20260708.md；AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_monitor_output_framework_adapt_execution_plan_20260708.md；AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_test_framework_adapt_coding_plan_20260708.md；AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_test_framework_logic_adapt_analysis_execution_plan_20260707.md；AI_DOC/mem_ut_flow_doc/load_sta_std_issue_flow.md；AI_DOC/mem_ut_flow_doc/replay_flow.md；AI_DOC/mem_ut_flow_doc/writeback_function_call_flow.md；AI_DOC/mem_ut_flow_doc/push_feedback_event_writeback_flow.md；AI_DOC/mem_ut_flow_doc/normal_pass_flow.md；AI_DOC/analysis/interface/v2/mem_ut_v2_agent_interface_signal_matrix_20260709.md；AI_DOC/analysis/interface/v2/memblock_v2only_port_scala_semantic_analysis_20260707.md。权威只读参考：AI_DOC/plan/test_framework/plan/undo/mem_ut_v2_iq_feedback_replay_framework_adapt_execution_plan_20260711.md。验收重点：1) 所有IQ/real-WB流程必须redirect-first后validate claim无副作用，再调用原handler/完成replay入队，只有handler成功或唯一精确STA compat no-op后commit；STA miss/LDA replay/fault不得no-op；covered/拒绝event不消费token。检查全文流程图、伪代码、函数说明、生命周期、端到端总结。2) software PREFETCH默认权重1，LDU+prefetch由derive_op_behavior得到PREFETCH/is_prefetch并route LOAD到V2 issueLda，是原支持逻辑，不得unsupported/fatal；CBO/atomic/vector仍边界。3) 总控参数顺序必须是validate_and_clamp先纯check compile localparam，再clamp runtime并由自身做post-condition；check不读写runtime。4) pending-MMIO past/current/future epoch、current 0/2候选、dynamic query mismatch、sideband不计progress策略一致；LSQ最后章逐函数完整。5) 十份专项、执行顺序、stage2 deferred、六组V2-only output无runtime修改、接口STA SQ-only/generation缺口完整。6) 各plan最后章对字段/参数、新增功能、修改功能、不变/不支持，以及新增/修改helper原逻辑、原因、新逻辑、输入输出副作用无遗漏。请按严重度列出精确文件:行号和修复建议；若所有要求都满足且无矛盾，最后单独输出 REVIEW_PASS。

## Configuration

- Model: gpt-5.4
- Effort: high
- Timeout: 1800s
- Timestamp: 2026-07-11_18-04-46
