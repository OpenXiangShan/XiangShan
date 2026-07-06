//=========================================================
//File name    : redirect_agent_agent_interface.sv
//Author       : OpenAI_Codex
//Module name  : redirect_agent_agent_interface
//Discribution : redirect_agent_agent_interface : signal interface
//Date         : 2026-04-12
//=========================================================
`ifndef REDIRECT_AGENT_AGENT_INTERFACE__SV
`define REDIRECT_AGENT_AGENT_INTERFACE__SV

`ifndef DEF_SETUP_TIME
    `define DEF_SETUP_TIME 1
`endif
`ifndef DEF_HOLD_TIME
    `define DEF_HOLD_TIME 1
`endif

interface redirect_agent_agent_interface  (input bit clk,input bit rst_n);

    logic io_redirect_valid            ;
    logic io_redirect_bits_level       ;
    logic io_redirect_bits_robIdx_flag ;
    logic [8:0] io_redirect_bits_robIdx_value;

    clocking drv_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        output io_redirect_valid;
        output io_redirect_bits_level;
        output io_redirect_bits_robIdx_flag;
        output io_redirect_bits_robIdx_value;

    endclocking:drv_cb

    clocking mon_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        input  io_redirect_valid;
        input  io_redirect_bits_level;
        input  io_redirect_bits_robIdx_flag;
        input  io_redirect_bits_robIdx_value;

    endclocking:mon_cb

    modport drv_mp (clocking drv_cb);
    modport mon_mp (clocking mon_cb);

endinterface:redirect_agent_agent_interface

`endif
