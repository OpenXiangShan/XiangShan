//=========================================================
//File name    : sbuffer_agent_agent.sv
//Author       : OpenAI_Codex
//Module name  : sbuffer_agent_agent
//Discribution : sbuffer_agent_agent : agent top
//Date         : 2026-04-12
//=========================================================
`ifndef SBUFFER_AGENT_AGENT__SV
`define SBUFFER_AGENT_AGENT__SV

class sbuffer_agent_agent  extends tcnt_agent_base#(
                                        .VIF_BUS(virtual sbuffer_agent_agent_interface),
                                        .cfg_t(sbuffer_agent_agent_cfg),
                                        .seq_t(sbuffer_agent_agent_xaction),
                                        .sqr_t(sbuffer_agent_agent_sequencer),
                                        .drv_t(sbuffer_agent_agent_driver),
                                        .mon_t(sbuffer_agent_agent_monitor));

    `uvm_component_utils(sbuffer_agent_agent)
    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern virtual function void connect_phase(uvm_phase phase);

endclass:sbuffer_agent_agent

function sbuffer_agent_agent::new(string name,uvm_component parent);
    super.new(name,parent);
endfunction:new

function void sbuffer_agent_agent::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

function void sbuffer_agent_agent::connect_phase(uvm_phase phase);
    super.connect_phase(phase);
endfunction:connect_phase

`endif
