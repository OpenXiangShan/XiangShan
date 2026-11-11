`ifndef CMO_PATH_AGENT_PKG_SV
`define CMO_PATH_AGENT_PKG_SV
`include "uvm_macros.svh"
`include "cmo_path_agent_dec.sv"
`include "cmo_path_agent_interface.sv"
package cmo_path_agent_pkg;
    import uvm_pkg::*;
    import cmo_path_agent_dec::*;
    `include "cmo_path_agent_cfg.sv"
    `include "cmo_path_agent_xaction.sv"
    `include "cmo_path_agent_default_sequence.sv"
    `include "cmo_path_agent_driver.sv"
    `include "cmo_path_agent_monitor.sv"
    `include "cmo_path_agent_sequencer.sv"
    `include "cmo_path_agent.sv"
endpackage
`endif
