//=========================================================
//File name    : lsqenq_agent_agent.sv
//Author       : OpenAI_Codex
//Module name  : lsqenq_agent_agent
//Discribution : lsqenq_agent_agent : agent top
//Date         : 2026-04-12
//=========================================================
`ifndef LSQENQ_AGENT_AGENT__SV
`define LSQENQ_AGENT_AGENT__SV

class lsqenq_agent_agent  extends tcnt_agent_base#(
                                        .VIF_BUS(virtual lsqenq_agent_agent_interface),
                                        .cfg_t(lsqenq_agent_agent_cfg),
                                        .seq_t(lsqenq_agent_agent_xaction),
                                        .sqr_t(lsqenq_agent_agent_sequencer),
                                        .drv_t(lsqenq_agent_agent_driver),
                                        .mon_t(lsqenq_agent_agent_monitor));

    `uvm_component_utils(lsqenq_agent_agent)
    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern virtual function void connect_phase(uvm_phase phase);

endclass:lsqenq_agent_agent

function lsqenq_agent_agent::new(string name,uvm_component parent);
    super.new(name,parent);
endfunction:new

function void lsqenq_agent_agent::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

function void lsqenq_agent_agent::connect_phase(uvm_phase phase);
    super.connect_phase(phase);
endfunction:connect_phase

`endif
