//=========================================================
//File name    : backendToTopBypass_agent_agent.sv
//Author       : OpenAI_Codex
//Module name  : backendToTopBypass_agent_agent
//Discribution : backendToTopBypass_agent_agent : agent top
//Date         : 2026-04-12
//=========================================================
`ifndef BACKENDTOTOPBYPASS_AGENT_AGENT__SV
`define BACKENDTOTOPBYPASS_AGENT_AGENT__SV

class backendToTopBypass_agent_agent  extends tcnt_agent_base#(
                                        .VIF_BUS(virtual backendToTopBypass_agent_agent_interface),
                                        .cfg_t(backendToTopBypass_agent_agent_cfg),
                                        .seq_t(backendToTopBypass_agent_agent_xaction),
                                        .sqr_t(backendToTopBypass_agent_agent_sequencer),
                                        .drv_t(backendToTopBypass_agent_agent_driver),
                                        .mon_t(backendToTopBypass_agent_agent_monitor));

    `uvm_component_utils(backendToTopBypass_agent_agent)
    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern virtual function void connect_phase(uvm_phase phase);

endclass:backendToTopBypass_agent_agent

function backendToTopBypass_agent_agent::new(string name,uvm_component parent);
    super.new(name,parent);
endfunction:new

function void backendToTopBypass_agent_agent::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

function void backendToTopBypass_agent_agent::connect_phase(uvm_phase phase);
    super.connect_phase(phase);
endfunction:connect_phase

`endif
