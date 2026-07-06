`ifndef TCNT_BASE_PKG__SV
`define TCNT_BASE_PKG__SV

`ifndef TCNT_HAD_INCLUDE_UVM_MACROS
`define TCNT_HAD_INCLUDE_UVM_MACROS
    `include "uvm_macros.svh"
`endif

`timescale 1ns/1ps

`include "tcnt_realtime.sv"
`include "tcnt_dec_base.sv"
`include "tcnt_common_method.sv"
`include "tcnt_clk_gen.sv"
//`include "tcnt_gen_wave.sv"
`include "tcnt_macro_define_base.sv"
`include "tcnt_macro_for_rtl.sv"

package tcnt_base_pkg;

    import uvm_pkg::*;
    import tcnt_realtime::*;
    import tcnt_dec_base::*;
    import tcnt_common_method::*;
    `include "tcnt_data_base.sv"
    `include "tcnt_agent_cfg_base.sv"
    `include "tcnt_default_sequence_base.sv"
    `include "tcnt_sequencer_base.sv"
    `include "tcnt_driver_base.sv"
    `include "tcnt_monitor_base.sv"
    `include "tcnt_agent_base.sv"
    `include "tcnt_rm_base.sv"
    `include "tcnt_scb_base.sv"
    `include "tcnt_env_base.sv"
    `include "tcnt_report_server_base.sv"
    `include "tcnt_test_base.sv"
    `include "tcnt_mem.sv"

endpackage

import tcnt_base_pkg::*;

`endif
