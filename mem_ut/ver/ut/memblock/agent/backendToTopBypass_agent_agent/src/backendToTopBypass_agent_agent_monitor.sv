//=========================================================
//File name    : backendToTopBypass_agent_agent_monitor.sv
//Author       : OpenAI_Codex
//Module name  : backendToTopBypass_agent_agent_monitor
//Discribution : backendToTopBypass_agent_agent_monitor : monitor
//Date         : 2026-04-12
//=========================================================
`ifndef BACKENDTOTOPBYPASS_AGENT_AGENT_MONITOR__SV
`define BACKENDTOTOPBYPASS_AGENT_AGENT_MONITOR__SV

class backendToTopBypass_agent_agent_monitor  extends tcnt_monitor_base#(virtual backendToTopBypass_agent_agent_interface,backendToTopBypass_agent_agent_cfg,backendToTopBypass_agent_agent_xaction);

    `uvm_component_utils(backendToTopBypass_agent_agent_monitor)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern task run_phase(uvm_phase phase);
    extern task mon_data();
endclass:backendToTopBypass_agent_agent_monitor

function backendToTopBypass_agent_agent_monitor::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void backendToTopBypass_agent_agent_monitor::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task backendToTopBypass_agent_agent_monitor::run_phase(uvm_phase phase);
    super.run_phase(phase);
    this.mon_data();
endtask:run_phase

task backendToTopBypass_agent_agent_monitor::mon_data();

    logic io_ooo_to_mem_backendToTopBypass_cpuWfi;
    logic io_ooo_to_mem_backendToTopBypass_cpuCriticalError;
    logic io_ooo_to_mem_backendToTopBypass_msiAck;

    backendToTopBypass_agent_agent_xaction  mon_tr;
    while(1) begin
        @this.vif.mon_mp.mon_cb;
        io_ooo_to_mem_backendToTopBypass_cpuWfi = this.vif.mon_mp.mon_cb.io_ooo_to_mem_backendToTopBypass_cpuWfi;
        io_ooo_to_mem_backendToTopBypass_cpuCriticalError = this.vif.mon_mp.mon_cb.io_ooo_to_mem_backendToTopBypass_cpuCriticalError;
        io_ooo_to_mem_backendToTopBypass_msiAck = this.vif.mon_mp.mon_cb.io_ooo_to_mem_backendToTopBypass_msiAck;

        if(this.cfg.xz_sw==tcnt_dec_base::ON && this.vif.rst_n==1'b1 && memblock_sync_pkg::reset_backend_done==1'b1) begin
            `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_backendToTopBypass_cpuWfi,io_ooo_to_mem_backendToTopBypass_cpuWfi,1);
            `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_backendToTopBypass_cpuCriticalError,io_ooo_to_mem_backendToTopBypass_cpuCriticalError,1);
            `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_backendToTopBypass_msiAck,io_ooo_to_mem_backendToTopBypass_msiAck,1);

        end
        //if(xxxTODOxxx==1'b1) begin
        //    mon_tr = backendToTopBypass_agent_agent_xaction::type_id::create("mon_tr");
        //    mon_tr.io_ooo_to_mem_backendToTopBypass_cpuWfi = io_ooo_to_mem_backendToTopBypass_cpuWfi;
        //    mon_tr.io_ooo_to_mem_backendToTopBypass_cpuCriticalError = io_ooo_to_mem_backendToTopBypass_cpuCriticalError;
        //    mon_tr.io_ooo_to_mem_backendToTopBypass_msiAck = io_ooo_to_mem_backendToTopBypass_msiAck;

        //    mon_tr.channel_id = this.cfg.channel_id;
        //    mon_tr.unpack();
        //    this.mon_item_port.write(mon_tr);
        //end
    end
endtask:mon_data

`endif
