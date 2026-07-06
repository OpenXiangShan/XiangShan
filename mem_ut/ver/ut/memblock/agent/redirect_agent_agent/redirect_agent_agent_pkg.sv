//=========================================================
//File name    : redirect_agent_agent_pkg.sv
//Author       : OpenAI_Codex
//Module name  : redirect_agent_agent_pkg
//Discribution : redirect_agent_agent_pkg : package
//Date         : 2026-04-12
//=========================================================
`ifndef REDIRECT_AGENT_AGENT_PKG__SV
`define REDIRECT_AGENT_AGENT_PKG__SV

`ifndef TCNT_HAD_INCLUDE_UVM_MACROS
`define TCNT_HAD_INCLUDE_UVM_MACROS
    `include "uvm_macros.svh"
`endif

`include "redirect_agent_agent_dec.sv"
`include "redirect_agent_agent_interface.sv"
package redirect_agent_agent_pkg;

    import uvm_pkg::*;
    import tcnt_realtime::*;
    import tcnt_dec_base::*;
    import tcnt_common_method::*;
    import tcnt_base_pkg::*;

    import redirect_agent_agent_dec::*;

    `include "redirect_agent_agent_cfg.sv"
    `include "redirect_agent_agent_xaction.sv"
    `include "redirect_agent_agent_default_sequence.sv"
    `include "redirect_agent_agent_driver.sv"
    `include "redirect_agent_agent_monitor.sv"
    `include "redirect_agent_agent_sequencer.sv"
    `include "redirect_agent_agent.sv"

endpackage

import redirect_agent_agent_pkg::*;

`endif
