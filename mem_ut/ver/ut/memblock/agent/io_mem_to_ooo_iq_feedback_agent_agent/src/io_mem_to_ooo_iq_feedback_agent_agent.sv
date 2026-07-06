//=========================================================
//File name    : io_mem_to_ooo_iq_feedback_agent_agent.sv
//Author       : OpenAI_Codex
//Module name  : io_mem_to_ooo_iq_feedback_agent_agent
//Discribution : io_mem_to_ooo_iq_feedback_agent_agent : agent top
//Date         : 2026-04-12
//=========================================================
`ifndef IO_MEM_TO_OOO_IQ_FEEDBACK_AGENT_AGENT__SV
`define IO_MEM_TO_OOO_IQ_FEEDBACK_AGENT_AGENT__SV

class io_mem_to_ooo_iq_feedback_agent_agent  extends tcnt_agent_base#(
                                        .VIF_BUS(virtual io_mem_to_ooo_iq_feedback_agent_agent_interface),
                                        .cfg_t(io_mem_to_ooo_iq_feedback_agent_agent_cfg),
                                        .seq_t(io_mem_to_ooo_iq_feedback_agent_agent_xaction),
                                        .sqr_t(io_mem_to_ooo_iq_feedback_agent_agent_sequencer),
                                        .drv_t(io_mem_to_ooo_iq_feedback_agent_agent_driver),
                                        .mon_t(io_mem_to_ooo_iq_feedback_agent_agent_monitor));

    `uvm_component_utils(io_mem_to_ooo_iq_feedback_agent_agent)
    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern virtual function void connect_phase(uvm_phase phase);

endclass:io_mem_to_ooo_iq_feedback_agent_agent

function io_mem_to_ooo_iq_feedback_agent_agent::new(string name,uvm_component parent);
    super.new(name,parent);
endfunction:new

function void io_mem_to_ooo_iq_feedback_agent_agent::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

function void io_mem_to_ooo_iq_feedback_agent_agent::connect_phase(uvm_phase phase);
    super.connect_phase(phase);
endfunction:connect_phase

`endif
