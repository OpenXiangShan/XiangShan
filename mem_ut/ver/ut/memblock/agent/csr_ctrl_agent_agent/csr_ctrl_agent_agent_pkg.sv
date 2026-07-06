//=========================================================
//File name    : csr_ctrl_agent_agent_pkg.sv
//Author       : OpenAI_Codex
//Module name  : csr_ctrl_agent_agent_pkg
//Discribution : csr_ctrl_agent_agent_pkg : package
//Date         : 2026-04-12
//=========================================================
`ifndef CSR_CTRL_AGENT_AGENT_PKG__SV
`define CSR_CTRL_AGENT_AGENT_PKG__SV

`ifndef TCNT_HAD_INCLUDE_UVM_MACROS
`define TCNT_HAD_INCLUDE_UVM_MACROS
    `include "uvm_macros.svh"
`endif

`include "csr_ctrl_agent_agent_dec.sv"
`include "csr_ctrl_agent_agent_interface.sv"
package csr_ctrl_agent_agent_pkg;

    import uvm_pkg::*;
    import tcnt_realtime::*;
    import tcnt_dec_base::*;
    import tcnt_common_method::*;
    import tcnt_base_pkg::*;

    import csr_ctrl_agent_agent_dec::*;

    `include "csr_ctrl_agent_agent_cfg.sv"
    `include "csr_ctrl_agent_agent_xaction.sv"
    `include "csr_ctrl_agent_agent_default_sequence.sv"
    `include "csr_ctrl_agent_agent_driver.sv"
    `include "csr_ctrl_agent_agent_monitor.sv"
    `include "csr_ctrl_agent_agent_sequencer.sv"
    `include "csr_ctrl_agent_agent.sv"

endpackage

import csr_ctrl_agent_agent_pkg::*;

`endif
