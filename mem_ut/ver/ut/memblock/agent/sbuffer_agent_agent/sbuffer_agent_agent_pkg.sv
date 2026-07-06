//=========================================================
//File name    : sbuffer_agent_agent_pkg.sv
//Author       : OpenAI_Codex
//Module name  : sbuffer_agent_agent_pkg
//Discribution : sbuffer_agent_agent_pkg : package
//Date         : 2026-04-12
//=========================================================
`ifndef SBUFFER_AGENT_AGENT_PKG__SV
`define SBUFFER_AGENT_AGENT_PKG__SV

`ifndef TCNT_HAD_INCLUDE_UVM_MACROS
`define TCNT_HAD_INCLUDE_UVM_MACROS
    `include "uvm_macros.svh"
`endif

`include "sbuffer_agent_agent_dec.sv"
`include "sbuffer_agent_agent_interface.sv"
package sbuffer_agent_agent_pkg;

    import uvm_pkg::*;
    import tcnt_realtime::*;
    import tcnt_dec_base::*;
    import tcnt_common_method::*;
    import tcnt_base_pkg::*;

    import sbuffer_agent_agent_dec::*;

    `include "sbuffer_agent_agent_cfg.sv"
    `include "sbuffer_agent_agent_xaction.sv"
    `include "sbuffer_agent_agent_default_sequence.sv"
    `include "sbuffer_agent_agent_driver.sv"
    `include "sbuffer_agent_agent_monitor.sv"
    `include "sbuffer_agent_agent_sequencer.sv"
    `include "sbuffer_agent_agent.sv"

endpackage

import sbuffer_agent_agent_pkg::*;

`endif
