//=========================================================
//File name    : memblock_connect.sv
//Author       : OpenAI_Codex
//Module name  : memblock_connect
//Discribution : memblock_connect : memblock connection macro
//Date         : 2026-04-12
//=========================================================
`ifndef MEMBLOCK_CONNECT__SV
`define MEMBLOCK_CONNECT__SV

`include "memblock_compile_params.svh"
`include "../../../ut/memblock/tb/backendToTopBypass_agent_connect.sv"
`include "../../../ut/memblock/tb/fence_agent_connect.sv"
`include "../../../ut/memblock/tb/csr_ctrl_agent_connect.sv"
`include "../../../ut/memblock/tb/lsqcommit_agent_connect.sv"
`include "../../../ut/memblock/tb/lsqenq_agent_connect.sv"
`include "../../../ut/memblock/tb/lintsissue_agent_connect.sv"
`include "../../../ut/memblock/tb/vecissue_agent_connect.sv"
`include "../../../ut/memblock/tb/redirect_agent_connect.sv"
`include "../../../ut/memblock/tb/sbuffer_agent_connect.sv"
`include "../../../ut/memblock/tb/dcache_agent_connect.sv"
`include "../../../ut/memblock/tb/int_sink_agent_connect.sv"
`include "../../../ut/memblock/tb/L2tlb_agent_connect.sv"
`include "../../../ut/memblock/tb/itlb_agent_connect.sv"
`include "../../../ut/memblock/tb/prefetch_agent_connect.sv"
`include "../../../ut/memblock/tb/io_mem_to_ooo_ctrl_agent_connect.sv"
`include "../../../ut/memblock/tb/io_mem_to_ooo_int_wb_agent_connect.sv"
`include "../../../ut/memblock/tb/io_mem_to_ooo_vec_wb_agent_connect.sv"
`include "../../../ut/memblock/tb/io_mem_to_ooo_wakeup_agent_connect.sv"
`include "../../../ut/memblock/tb/io_mem_to_ooo_iq_feedback_agent_connect.sv"
`include "../../../ut/memblock/tb/other_ctrl_agent_connect.sv"

`define MEMBLOCK_CONNECT(ENV_PATH,RTL_PATH) \
    `MEMBLOCK__BACKENDTOTOPBYPASS_AGENT_CONNECT(u_memblock__backendToTopBypass_agent_if, ENV_PATH.u_backendToTopBypass_agent_agent, RTL_PATH) \
    `MEMBLOCK__FENCE_AGENT_CONNECT(u_memblock__fence_agent_if, ENV_PATH.u_fence_agent_agent, RTL_PATH) \
    `MEMBLOCK__CSR_CTRL_AGENT_CONNECT(u_memblock__csr_ctrl_agent_if, ENV_PATH.u_csr_ctrl_agent_agent, RTL_PATH) \
    `MEMBLOCK__LSQCOMMIT_AGENT_CONNECT(u_memblock__lsqcommit_agent_if, ENV_PATH.u_lsqcommit_agent_agent, RTL_PATH) \
    `MEMBLOCK__LSQENQ_AGENT_CONNECT(u_memblock__lsqenq_agent_if, ENV_PATH.u_lsqenq_agent_agent, RTL_PATH) \
    `MEMBLOCK__LINTSISSUE_AGENT_CONNECT(u_memblock__lintsissue_agent_if, ENV_PATH.u_lintsissue_agent_agent, RTL_PATH) \
    `MEMBLOCK__VECISSUE_AGENT_CONNECT(u_memblock__vecissue_agent_if, ENV_PATH.u_vecissue_agent_agent, RTL_PATH) \
    `MEMBLOCK__REDIRECT_AGENT_CONNECT(u_memblock__redirect_agent_if, ENV_PATH.u_redirect_agent_agent, RTL_PATH) \
    `MEMBLOCK__SBUFFER_AGENT_CONNECT(u_memblock__sbuffer_agent_if, ENV_PATH.u_sbuffer_agent_agent, RTL_PATH) \
    `MEMBLOCK__DCACHE_AGENT_CONNECT(u_memblock__dcache_agent_if, ENV_PATH.u_dcache_agent_agent, RTL_PATH) \
    `MEMBLOCK__INT_SINK_AGENT_CONNECT(u_memblock__int_sink_agent_if, ENV_PATH.u_int_sink_agent_agent, RTL_PATH) \
    `MEMBLOCK__L2TLB_AGENT_CONNECT(u_memblock__L2tlb_agent_if, ENV_PATH.u_L2tlb_agent_agent, RTL_PATH) \
    `MEMBLOCK__ITLB_AGENT_CONNECT(u_memblock__itlb_agent_if, ENV_PATH.u_itlb_agent_agent, RTL_PATH) \
    `MEMBLOCK__PREFETCH_AGENT_CONNECT(u_memblock__prefetch_agent_if, ENV_PATH.u_prefetch_agent_agent, RTL_PATH) \
    `MEMBLOCK__IO_MEM_TO_OOO_CTRL_AGENT_CONNECT(u_memblock__io_mem_to_ooo_ctrl_agent_if, ENV_PATH.u_io_mem_to_ooo_ctrl_agent_agent, RTL_PATH) \
    `MEMBLOCK__IO_MEM_TO_OOO_INT_WB_AGENT_CONNECT(u_memblock__io_mem_to_ooo_int_wb_agent_if, ENV_PATH.u_io_mem_to_ooo_int_wb_agent_agent, RTL_PATH) \
    `MEMBLOCK__IO_MEM_TO_OOO_VEC_WB_AGENT_CONNECT(u_memblock__io_mem_to_ooo_vec_wb_agent_if, ENV_PATH.u_io_mem_to_ooo_vec_wb_agent_agent, RTL_PATH) \
    `MEMBLOCK__IO_MEM_TO_OOO_WAKEUP_AGENT_CONNECT(u_memblock__io_mem_to_ooo_wakeup_agent_if, ENV_PATH.u_io_mem_to_ooo_wakeup_agent_agent, RTL_PATH) \
    `MEMBLOCK__IO_MEM_TO_OOO_IQ_FEEDBACK_AGENT_CONNECT(u_memblock__io_mem_to_ooo_iq_feedback_agent_if, ENV_PATH.u_io_mem_to_ooo_iq_feedback_agent_agent, RTL_PATH) \
    `MEMBLOCK__OTHER_CTRL_AGENT_CONNECT(u_memblock__other_ctrl_agent_if, ENV_PATH.u_other_ctrl_agent_agent, RTL_PATH)

`endif
