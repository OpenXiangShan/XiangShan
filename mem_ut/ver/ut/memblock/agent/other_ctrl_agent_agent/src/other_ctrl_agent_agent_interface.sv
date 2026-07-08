//=========================================================
//File name    : other_ctrl_agent_agent_interface.sv
//Author       : OpenAI_Codex
//Module name  : other_ctrl_agent_agent_interface
//Discribution : other_ctrl_agent_agent_interface : signal interface
//Date         : 2026-04-12
//=========================================================
`ifndef OTHER_CTRL_AGENT_AGENT_INTERFACE__SV
`define OTHER_CTRL_AGENT_AGENT_INTERFACE__SV

`ifndef DEF_SETUP_TIME
    `define DEF_SETUP_TIME 1
`endif
`ifndef DEF_HOLD_TIME
    `define DEF_HOLD_TIME 1
`endif

interface other_ctrl_agent_agent_interface  (input bit clk,input bit rst_n);

    logic [5:0] io_hartId              ;
    logic io_dcacheError_ecc_error_valid;
    logic [47:0] io_dcacheError_ecc_error_bits;
    logic io_uncacheError_ecc_error_valid;
    logic [47:0] io_uncacheError_ecc_error_bits;
    logic [47:0] io_inner_reset_vector ;
    logic [47:0] io_outer_reset_vector ;
    logic io_outer_l2_flush_en         ;
    logic io_outer_power_down_en       ;
    logic io_outer_cpu_critical_error  ;
    logic io_inner_beu_errors_icache_ecc_error_valid;
    logic [47:0] io_inner_beu_errors_icache_ecc_error_bits;
    logic io_outer_beu_errors_icache_ecc_error_valid;
    logic [47:0] io_outer_beu_errors_icache_ecc_error_bits;
    logic io_reset_backend             ;

    logic io_outer_cpu_halt;

    clocking drv_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        output io_hartId;
        input  io_dcacheError_ecc_error_valid;
        input  io_dcacheError_ecc_error_bits;
        input  io_uncacheError_ecc_error_valid;
        input  io_uncacheError_ecc_error_bits;
        input  io_inner_reset_vector;
        output io_outer_reset_vector;
        input  io_outer_l2_flush_en;
        input  io_outer_power_down_en;
        input  io_outer_cpu_critical_error;
        output io_inner_beu_errors_icache_ecc_error_valid;
        output io_inner_beu_errors_icache_ecc_error_bits;
        input  io_outer_beu_errors_icache_ecc_error_valid;
        input  io_outer_beu_errors_icache_ecc_error_bits;
        input  io_reset_backend;

        input  io_outer_cpu_halt;
    endclocking:drv_cb

    clocking mon_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        input  io_hartId;
        input  io_dcacheError_ecc_error_valid;
        input  io_dcacheError_ecc_error_bits;
        input  io_uncacheError_ecc_error_valid;
        input  io_uncacheError_ecc_error_bits;
        input  io_inner_reset_vector;
        input  io_outer_reset_vector;
        input  io_outer_l2_flush_en;
        input  io_outer_power_down_en;
        input  io_outer_cpu_critical_error;
        input  io_inner_beu_errors_icache_ecc_error_valid;
        input  io_inner_beu_errors_icache_ecc_error_bits;
        input  io_outer_beu_errors_icache_ecc_error_valid;
        input  io_outer_beu_errors_icache_ecc_error_bits;
        input  io_reset_backend;

        input  io_outer_cpu_halt;
    endclocking:mon_cb

    modport drv_mp (clocking drv_cb);
    modport mon_mp (clocking mon_cb);

endinterface:other_ctrl_agent_agent_interface

`endif
