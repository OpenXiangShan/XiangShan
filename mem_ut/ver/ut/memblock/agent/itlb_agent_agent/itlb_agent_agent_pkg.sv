//=========================================================
//File name    : itlb_agent_agent_pkg.sv
//Author       : OpenAI_Codex
//Module name  : itlb_agent_agent_pkg
//Discribution : itlb_agent_agent_pkg : package
//Date         : 2026-04-12
//=========================================================
`ifndef ITLB_AGENT_AGENT_PKG__SV
`define ITLB_AGENT_AGENT_PKG__SV

`ifndef TCNT_HAD_INCLUDE_UVM_MACROS
`define TCNT_HAD_INCLUDE_UVM_MACROS
    `include "uvm_macros.svh"
`endif

`include "itlb_agent_agent_dec.sv"
`include "itlb_agent_agent_interface.sv"
package itlb_agent_agent_pkg;

    import uvm_pkg::*;
    import tcnt_realtime::*;
    import tcnt_dec_base::*;
    import tcnt_common_method::*;
    import tcnt_base_pkg::*;

    import itlb_agent_agent_dec::*;

    `include "itlb_agent_agent_cfg.sv"
    `include "itlb_agent_agent_xaction.sv"
    `include "itlb_agent_agent_default_sequence.sv"
    `include "itlb_agent_agent_driver.sv"
    `include "itlb_agent_agent_monitor.sv"
    `include "itlb_agent_agent_sequencer.sv"
    `include "itlb_agent_agent.sv"

endpackage

import itlb_agent_agent_pkg::*;

`endif
