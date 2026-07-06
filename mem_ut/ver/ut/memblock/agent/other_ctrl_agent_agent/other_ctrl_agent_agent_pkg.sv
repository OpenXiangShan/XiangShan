//=========================================================
//File name    : other_ctrl_agent_agent_pkg.sv
//Author       : OpenAI_Codex
//Module name  : other_ctrl_agent_agent_pkg
//Discribution : other_ctrl_agent_agent_pkg : package
//Date         : 2026-04-12
//=========================================================
`ifndef OTHER_CTRL_AGENT_AGENT_PKG__SV
`define OTHER_CTRL_AGENT_AGENT_PKG__SV

`ifndef TCNT_HAD_INCLUDE_UVM_MACROS
`define TCNT_HAD_INCLUDE_UVM_MACROS
    `include "uvm_macros.svh"
`endif

`include "other_ctrl_agent_agent_dec.sv"
`include "other_ctrl_agent_agent_interface.sv"
package other_ctrl_agent_agent_pkg;

    import uvm_pkg::*;
    import tcnt_realtime::*;
    import tcnt_dec_base::*;
    import tcnt_common_method::*;
    import tcnt_base_pkg::*;

    import other_ctrl_agent_agent_dec::*;

    `include "other_ctrl_agent_agent_cfg.sv"
    `include "other_ctrl_agent_agent_xaction.sv"
    `include "other_ctrl_agent_agent_default_sequence.sv"
    `include "other_ctrl_agent_agent_driver.sv"
    `include "other_ctrl_agent_agent_monitor.sv"
    `include "other_ctrl_agent_agent_sequencer.sv"
    `include "other_ctrl_agent_agent.sv"

endpackage

import other_ctrl_agent_agent_pkg::*;

`endif
