//=========================================================
//File name    : int_sink_agent_agent_pkg.sv
//Author       : OpenAI_Codex
//Module name  : int_sink_agent_agent_pkg
//Discribution : int_sink_agent_agent_pkg : package
//Date         : 2026-04-12
//=========================================================
`ifndef INT_SINK_AGENT_AGENT_PKG__SV
`define INT_SINK_AGENT_AGENT_PKG__SV

`ifndef TCNT_HAD_INCLUDE_UVM_MACROS
`define TCNT_HAD_INCLUDE_UVM_MACROS
    `include "uvm_macros.svh"
`endif

`include "int_sink_agent_agent_dec.sv"
`include "int_sink_agent_agent_interface.sv"
package int_sink_agent_agent_pkg;

    import uvm_pkg::*;
    import tcnt_realtime::*;
    import tcnt_dec_base::*;
    import tcnt_common_method::*;
    import tcnt_base_pkg::*;

    import int_sink_agent_agent_dec::*;

    `include "int_sink_agent_agent_cfg.sv"
    `include "int_sink_agent_agent_xaction.sv"
    `include "int_sink_agent_agent_default_sequence.sv"
    `include "int_sink_agent_agent_driver.sv"
    `include "int_sink_agent_agent_monitor.sv"
    `include "int_sink_agent_agent_sequencer.sv"
    `include "int_sink_agent_agent.sv"

endpackage

import int_sink_agent_agent_pkg::*;

`endif
