//=========================================================
//File name    : memblock_common_pkg.sv
//Author       : OpenAI_Codex
//Module name  : memblock_common_pkg
//Discribution : memblock_common_pkg : package
//Date         : 2026-04-12
//=========================================================
`ifndef MEMBLOCK_COMMON_PKG__SV
`define MEMBLOCK_COMMON_PKG__SV

`ifndef TCNT_HAD_INCLUDE_UVM_MACROS
`define TCNT_HAD_INCLUDE_UVM_MACROS
    `include "uvm_macros.svh"
`endif

`include "memblock_dec.sv"
package memblock_common_pkg;

    import uvm_pkg::*;
    import tcnt_realtime::*;
    import tcnt_dec_base::*;
    import tcnt_common_method::*;
    import tcnt_base_pkg::*;

    import memblock_dec::*;

    import backendToTopBypass_agent_agent_dec::*;
    import backendToTopBypass_agent_agent_pkg::*;
    import fence_agent_agent_dec::*;
    import fence_agent_agent_pkg::*;
    import csr_ctrl_agent_agent_dec::*;
    import csr_ctrl_agent_agent_pkg::*;
    import lsqcommit_agent_agent_dec::*;
    import lsqcommit_agent_agent_pkg::*;
    import lsqenq_agent_agent_dec::*;
    import lsqenq_agent_agent_pkg::*;
    import lintsissue_agent_agent_dec::*;
    import lintsissue_agent_agent_pkg::*;
    import vecissue_agent_agent_dec::*;
    import vecissue_agent_agent_pkg::*;
    import redirect_agent_agent_dec::*;
    import redirect_agent_agent_pkg::*;
    import sbuffer_agent_agent_dec::*;
    import sbuffer_agent_agent_pkg::*;
    import dcache_agent_agent_dec::*;
    import dcache_agent_agent_pkg::*;
    import int_sink_agent_agent_dec::*;
    import int_sink_agent_agent_pkg::*;
    import L2tlb_agent_agent_dec::*;
    import L2tlb_agent_agent_pkg::*;
    import itlb_agent_agent_dec::*;
    import itlb_agent_agent_pkg::*;
    import prefetch_agent_agent_dec::*;
    import prefetch_agent_agent_pkg::*;
    import io_mem_to_ooo_ctrl_agent_agent_dec::*;
    import io_mem_to_ooo_ctrl_agent_agent_pkg::*;
    import io_mem_to_ooo_int_wb_agent_agent_dec::*;
    import io_mem_to_ooo_int_wb_agent_agent_pkg::*;
    import io_mem_to_ooo_vec_wb_agent_agent_dec::*;
    import io_mem_to_ooo_vec_wb_agent_agent_pkg::*;
    import io_mem_to_ooo_wakeup_agent_agent_dec::*;
    import io_mem_to_ooo_wakeup_agent_agent_pkg::*;
    import io_mem_to_ooo_iq_feedback_agent_agent_dec::*;
    import io_mem_to_ooo_iq_feedback_agent_agent_pkg::*;
    import other_ctrl_agent_agent_dec::*;
    import other_ctrl_agent_agent_pkg::*;

    `include "memblock_common_xaction.sv"
    `include "memblock_fcov.sv"

endpackage

import memblock_common_pkg::*;

`endif
