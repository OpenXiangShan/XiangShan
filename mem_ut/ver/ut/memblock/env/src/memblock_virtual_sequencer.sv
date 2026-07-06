//=========================================================
//File name    : memblock_virtual_sequencer.sv
//Author       : OpenAI_Codex
//Module name  : memblock_virtual_sequencer
//Discribution : memblock virtual sequencer
//Date         : 2026-07-03
//=========================================================
`ifndef MEMBLOCK_VIRTUAL_SEQUENCER__SV
`define MEMBLOCK_VIRTUAL_SEQUENCER__SV

class memblock_virtual_sequencer extends uvm_sequencer;

    backendToTopBypass_agent_agent_sequencer       backendToTopBypass_sqr;
    fence_agent_agent_sequencer                    fence_sqr;
    csr_ctrl_agent_agent_sequencer                 csr_ctrl_sqr;
    lsqcommit_agent_agent_sequencer                lsqcommit_sqr;
    lsqenq_agent_agent_sequencer                   lsqenq_sqr;
    lintsissue_agent_agent_sequencer               lintsissue_sqr;
    vecissue_agent_agent_sequencer                 vecissue_sqr;
    redirect_agent_agent_sequencer                 redirect_sqr;
    sbuffer_agent_agent_sequencer                  sbuffer_sqr;
    dcache_agent_agent_sequencer                   dcache_sqr;
    int_sink_agent_agent_sequencer                 int_sink_sqr;
    L2tlb_agent_agent_sequencer                    L2tlb_sqr;
    itlb_agent_agent_sequencer                     itlb_sqr;
    prefetch_agent_agent_sequencer                 prefetch_sqr;
    io_mem_to_ooo_ctrl_agent_agent_sequencer       io_mem_to_ooo_ctrl_sqr;
    io_mem_to_ooo_int_wb_agent_agent_sequencer     io_mem_to_ooo_int_wb_sqr;
    io_mem_to_ooo_vec_wb_agent_agent_sequencer     io_mem_to_ooo_vec_wb_sqr;
    io_mem_to_ooo_wakeup_agent_agent_sequencer     io_mem_to_ooo_wakeup_sqr;
    io_mem_to_ooo_iq_feedback_agent_agent_sequencer io_mem_to_ooo_iq_feedback_sqr;
    other_ctrl_agent_agent_sequencer               other_ctrl_sqr;

    `uvm_component_utils(memblock_virtual_sequencer)

    extern function new(string name = "memblock_virtual_sequencer",
                        uvm_component parent = null);

endclass:memblock_virtual_sequencer

function memblock_virtual_sequencer::new(string name = "memblock_virtual_sequencer",
                                         uvm_component parent = null);
    super.new(name, parent);
endfunction:new

`endif
