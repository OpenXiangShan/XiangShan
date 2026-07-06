//=========================================================
//File name    : L2tlb_agent_agent_pkg.sv
//Author       : OpenAI_Codex
//Module name  : L2tlb_agent_agent_pkg
//Discribution : L2tlb_agent_agent_pkg : package
//Date         : 2026-04-12
//=========================================================
`ifndef L2TLB_AGENT_AGENT_PKG__SV
`define L2TLB_AGENT_AGENT_PKG__SV

`ifndef TCNT_HAD_INCLUDE_UVM_MACROS
`define TCNT_HAD_INCLUDE_UVM_MACROS
    `include "uvm_macros.svh"
`endif

`include "L2tlb_agent_agent_dec.sv"
`include "L2tlb_agent_agent_interface.sv"
package L2tlb_agent_agent_pkg;

    import uvm_pkg::*;
    import tcnt_realtime::*;
    import tcnt_dec_base::*;
    import tcnt_common_method::*;
    import tcnt_base_pkg::*;

    import L2tlb_agent_agent_dec::*;

    `include "L2tlb_agent_agent_cfg.sv"
    `include "L2tlb_agent_agent_xaction.sv"
    `include "L2tlb_agent_agent_default_sequence.sv"
    `include "L2tlb_agent_agent_driver.sv"
    `include "L2tlb_agent_agent_monitor.sv"
    `include "L2tlb_agent_agent_sequencer.sv"
    `include "L2tlb_agent_agent.sv"

endpackage

import L2tlb_agent_agent_pkg::*;

`endif
