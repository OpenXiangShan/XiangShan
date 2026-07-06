//=========================================================
//File name    : prefetch_agent_agent.sv
//Author       : OpenAI_Codex
//Module name  : prefetch_agent_agent
//Discribution : prefetch_agent_agent : agent top
//Date         : 2026-04-12
//=========================================================
`ifndef PREFETCH_AGENT_AGENT__SV
`define PREFETCH_AGENT_AGENT__SV

class prefetch_agent_agent  extends tcnt_agent_base#(
                                        .VIF_BUS(virtual prefetch_agent_agent_interface),
                                        .cfg_t(prefetch_agent_agent_cfg),
                                        .seq_t(prefetch_agent_agent_xaction),
                                        .sqr_t(prefetch_agent_agent_sequencer),
                                        .drv_t(prefetch_agent_agent_driver),
                                        .mon_t(prefetch_agent_agent_monitor));

    `uvm_component_utils(prefetch_agent_agent)
    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern virtual function void connect_phase(uvm_phase phase);

endclass:prefetch_agent_agent

function prefetch_agent_agent::new(string name,uvm_component parent);
    super.new(name,parent);
endfunction:new

function void prefetch_agent_agent::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

function void prefetch_agent_agent::connect_phase(uvm_phase phase);
    super.connect_phase(phase);
endfunction:connect_phase

`endif
