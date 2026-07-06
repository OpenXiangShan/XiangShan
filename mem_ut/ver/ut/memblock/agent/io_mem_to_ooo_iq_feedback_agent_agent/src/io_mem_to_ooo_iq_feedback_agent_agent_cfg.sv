//=========================================================
//File name    : io_mem_to_ooo_iq_feedback_agent_agent_cfg.sv
//Author       : OpenAI_Codex
//Module name  : io_mem_to_ooo_iq_feedback_agent_agent_cfg
//Discribution : io_mem_to_ooo_iq_feedback_agent_agent_cfg : agent configuration
//Date         : 2026-04-12
//=========================================================
`ifndef IO_MEM_TO_OOO_IQ_FEEDBACK_AGENT_AGENT_CFG__SV
`define IO_MEM_TO_OOO_IQ_FEEDBACK_AGENT_AGENT_CFG__SV

class io_mem_to_ooo_iq_feedback_agent_agent_cfg extends tcnt_agent_cfg_base;

    `uvm_object_utils_begin(io_mem_to_ooo_iq_feedback_agent_agent_cfg)
    `uvm_object_utils_end

    extern function new(string name="io_mem_to_ooo_iq_feedback_agent_agent_cfg");
    extern function void pre_randomize();
    extern function void post_randomize();

endclass:io_mem_to_ooo_iq_feedback_agent_agent_cfg

function io_mem_to_ooo_iq_feedback_agent_agent_cfg::new(string  name = "io_mem_to_ooo_iq_feedback_agent_agent_cfg");
    super.new(name);
endfunction:new

function void io_mem_to_ooo_iq_feedback_agent_agent_cfg::pre_randomize();
    super.pre_randomize();
endfunction:pre_randomize

function void io_mem_to_ooo_iq_feedback_agent_agent_cfg::post_randomize();
    super.post_randomize();
endfunction:post_randomize

`endif
