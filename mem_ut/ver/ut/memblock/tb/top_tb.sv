//=========================================================
//File name    : top_tb.sv
//Author       : OpenAI_Codex
//Module name  : top_tb
//Discribution : top_tb : testbench top
//Date         : 2026-04-12
//=========================================================
`ifndef TOP_TB__SV
`define TOP_TB__SV

`timescale 1ns/1ps

`ifndef TCNT_HAD_INCLUDE_UVM_MACROS
`define TCNT_HAD_INCLUDE_UVM_MACROS
    `include "uvm_macros.svh"
`endif

`ifndef TCNT_HAD_IMPORT_UVM_PKG
`define TCNT_HAD_IMPORT_UVM_PKG
    import uvm_pkg::*;
`endif

`include "../../../common/tcnt_base/src/tcnt_clk_gen.sv"

module top_tb;

    import tcnt_realtime::*;
    import tcnt_dec_base::*;
    import memblock_sync_pkg::*;
    import tc_pkg::*;

    reg clk;

    `CLK_GEN(clk,200)
    `RST_GEN(tc_if.rst_n,100)

    `include "../tb/dut_inst.sv"
    `include "../tb/tc_if_connect.sv"
    `include "../../../ut/memblock/tb/memblock_connect.sv"
    `MEMBLOCK_CONNECT(env,top_tb.U_MEMBLOCK)

    initial begin
        memblock_sync_pkg::reset_backend_done = 1'b0;
        wait(tc_if.rst_n === 1'b0);
        wait(tc_if.rst_n === 1'b1);
        repeat (2) @(posedge clk);
        memblock_sync_pkg::reset_backend_done = 1'b1;
    end

    initial begin
       run_test();
    end

    //`include "../tb/gen_wave.sv"
    `include "../tb/read_sdf.sv"

endmodule
`endif
