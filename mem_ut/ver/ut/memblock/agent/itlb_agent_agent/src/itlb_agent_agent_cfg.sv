//=========================================================
//File name    : itlb_agent_agent_cfg.sv
//Author       : OpenAI_Codex
//Module name  : itlb_agent_agent_cfg
//Discribution : itlb_agent_agent_cfg : agent configuration
//Date         : 2026-04-12
//=========================================================
`ifndef ITLB_AGENT_AGENT_CFG__SV
`define ITLB_AGENT_AGENT_CFG__SV

class itlb_agent_agent_cfg extends tcnt_agent_cfg_base;

    `uvm_object_utils_begin(itlb_agent_agent_cfg)
    `uvm_object_utils_end

    extern function new(string name="itlb_agent_agent_cfg");
    extern function void pre_randomize();
    extern function void post_randomize();

endclass:itlb_agent_agent_cfg

function itlb_agent_agent_cfg::new(string  name = "itlb_agent_agent_cfg");
    super.new(name);
endfunction:new

function void itlb_agent_agent_cfg::pre_randomize();
    super.pre_randomize();
endfunction:pre_randomize

function void itlb_agent_agent_cfg::post_randomize();
    super.post_randomize();
endfunction:post_randomize

`endif
