//=========================================================
//File name    : other_ctrl_agent_agent.sv
//Author       : OpenAI_Codex
//Module name  : other_ctrl_agent_agent
//Discribution : other_ctrl_agent_agent : agent top
//Date         : 2026-04-12
//=========================================================
`ifndef OTHER_CTRL_AGENT_AGENT__SV
`define OTHER_CTRL_AGENT_AGENT__SV

class other_ctrl_agent_agent  extends tcnt_agent_base#(
                                        .VIF_BUS(virtual other_ctrl_agent_agent_interface),
                                        .cfg_t(other_ctrl_agent_agent_cfg),
                                        .seq_t(other_ctrl_agent_agent_xaction),
                                        .sqr_t(other_ctrl_agent_agent_sequencer),
                                        .drv_t(other_ctrl_agent_agent_driver),
                                        .mon_t(other_ctrl_agent_agent_monitor));

    `uvm_component_utils(other_ctrl_agent_agent)
    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern virtual function void connect_phase(uvm_phase phase);

endclass:other_ctrl_agent_agent

function other_ctrl_agent_agent::new(string name,uvm_component parent);
    super.new(name,parent);
endfunction:new

function void other_ctrl_agent_agent::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

function void other_ctrl_agent_agent::connect_phase(uvm_phase phase);
    super.connect_phase(phase);
endfunction:connect_phase

`endif
