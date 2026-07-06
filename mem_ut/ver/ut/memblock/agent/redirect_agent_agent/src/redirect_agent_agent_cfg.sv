//=========================================================
//File name    : redirect_agent_agent_cfg.sv
//Author       : OpenAI_Codex
//Module name  : redirect_agent_agent_cfg
//Discribution : redirect_agent_agent_cfg : agent configuration
//Date         : 2026-04-12
//=========================================================
`ifndef REDIRECT_AGENT_AGENT_CFG__SV
`define REDIRECT_AGENT_AGENT_CFG__SV

class redirect_agent_agent_cfg extends tcnt_agent_cfg_base;

    `uvm_object_utils_begin(redirect_agent_agent_cfg)
    `uvm_object_utils_end

    extern function new(string name="redirect_agent_agent_cfg");
    extern function void pre_randomize();
    extern function void post_randomize();

endclass:redirect_agent_agent_cfg

function redirect_agent_agent_cfg::new(string  name = "redirect_agent_agent_cfg");
    super.new(name);
endfunction:new

function void redirect_agent_agent_cfg::pre_randomize();
    super.pre_randomize();
endfunction:pre_randomize

function void redirect_agent_agent_cfg::post_randomize();
    super.post_randomize();
endfunction:post_randomize

`endif
