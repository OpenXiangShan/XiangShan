//=========================================================
//File name    : backendToTopBypass_agent_agent_interface.sv
//Author       : OpenAI_Codex
//Module name  : backendToTopBypass_agent_agent_interface
//Discribution : backendToTopBypass_agent_agent_interface : signal interface
//Date         : 2026-04-12
//=========================================================
`ifndef BACKENDTOTOPBYPASS_AGENT_AGENT_INTERFACE__SV
`define BACKENDTOTOPBYPASS_AGENT_AGENT_INTERFACE__SV

`ifndef DEF_SETUP_TIME
    `define DEF_SETUP_TIME 1
`endif
`ifndef DEF_HOLD_TIME
    `define DEF_HOLD_TIME 1
`endif

interface backendToTopBypass_agent_agent_interface  (input bit clk,input bit rst_n);

    logic io_ooo_to_mem_backendToTopBypass_cpuWfi;
    logic io_ooo_to_mem_backendToTopBypass_cpuCriticalError;
    logic io_ooo_to_mem_backendToTopBypass_msiAck;

    clocking drv_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        output io_ooo_to_mem_backendToTopBypass_cpuWfi;
        output io_ooo_to_mem_backendToTopBypass_cpuCriticalError;
        output io_ooo_to_mem_backendToTopBypass_msiAck;

    endclocking:drv_cb

    clocking mon_cb @(posedge clk);
        `ifdef INTERFACE_ADD_DELAY
            default input #`DEF_SETUP_TIME output #`DEF_HOLD_TIME;
        `endif
        input  io_ooo_to_mem_backendToTopBypass_cpuWfi;
        input  io_ooo_to_mem_backendToTopBypass_cpuCriticalError;
        input  io_ooo_to_mem_backendToTopBypass_msiAck;

    endclocking:mon_cb

    modport drv_mp (clocking drv_cb);
    modport mon_mp (clocking mon_cb);

endinterface:backendToTopBypass_agent_agent_interface

`endif
