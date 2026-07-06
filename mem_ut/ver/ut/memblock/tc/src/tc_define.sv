//=========================================================
//File name    : tc_define.sv
//Author       : OpenAI_Codex
//Module name  : tc_define
//Discribution : tc_define : micro define for TC
//Date         : 2026-04-12
//=========================================================
`ifndef TC_DEFINE__SV
`define TC_DEFINE__SV

`define seq_backendToTopBypass_agent(tc) ``tc``__seq_backendToTopBypass_agent
`define seq_fence_agent(tc) ``tc``__seq_fence_agent
`define seq_csr_ctrl_agent(tc) ``tc``__seq_csr_ctrl_agent
`define seq_lsqcommit_agent(tc) ``tc``__seq_lsqcommit_agent
`define seq_lsqenq_agent(tc) ``tc``__seq_lsqenq_agent
`define seq_lintsissue_agent(tc) ``tc``__seq_lintsissue_agent
`define seq_vecissue_agent(tc) ``tc``__seq_vecissue_agent
`define seq_redirect_agent(tc) ``tc``__seq_redirect_agent
`define seq_sbuffer_agent(tc) ``tc``__seq_sbuffer_agent
`define seq_dcache_agent(tc) ``tc``__seq_dcache_agent
`define seq_int_sink_agent(tc) ``tc``__seq_int_sink_agent
`define seq_itlb_agent(tc) ``tc``__seq_itlb_agent
`define seq_other_ctrl_agent(tc) ``tc``__seq_other_ctrl_agent


`endif
