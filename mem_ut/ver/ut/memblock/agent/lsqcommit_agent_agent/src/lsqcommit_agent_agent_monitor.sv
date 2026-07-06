//=========================================================
//File name    : lsqcommit_agent_agent_monitor.sv
//Author       : OpenAI_Codex
//Module name  : lsqcommit_agent_agent_monitor
//Discribution : lsqcommit_agent_agent_monitor : monitor
//Date         : 2026-04-12
//=========================================================
`ifndef LSQCOMMIT_AGENT_AGENT_MONITOR__SV
`define LSQCOMMIT_AGENT_AGENT_MONITOR__SV

class lsqcommit_agent_agent_monitor  extends tcnt_monitor_base#(virtual lsqcommit_agent_agent_interface,lsqcommit_agent_agent_cfg,lsqcommit_agent_agent_xaction);

    `uvm_component_utils(lsqcommit_agent_agent_monitor)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern task run_phase(uvm_phase phase);
    extern task mon_data();
endclass:lsqcommit_agent_agent_monitor

function lsqcommit_agent_agent_monitor::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void lsqcommit_agent_agent_monitor::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task lsqcommit_agent_agent_monitor::run_phase(uvm_phase phase);
    super.run_phase(phase);
    this.mon_data();
endtask:run_phase

task lsqcommit_agent_agent_monitor::mon_data();

    logic io_ooo_to_mem_lsqio_pendingPtr_flag;
    logic [8:0] io_ooo_to_mem_lsqio_pendingPtr_value;
    logic io_ooo_to_mem_flushSb        ;

    lsqcommit_agent_agent_xaction  mon_tr;
    while(1) begin
        @this.vif.mon_mp.mon_cb;
        io_ooo_to_mem_lsqio_pendingPtr_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_lsqio_pendingPtr_flag;
        io_ooo_to_mem_lsqio_pendingPtr_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_lsqio_pendingPtr_value;
        io_ooo_to_mem_flushSb = this.vif.mon_mp.mon_cb.io_ooo_to_mem_flushSb;

        if(this.cfg.xz_sw==tcnt_dec_base::ON && this.vif.rst_n==1'b1 && memblock_sync_pkg::reset_backend_done==1'b1) begin
            `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_lsqio_pendingPtr_flag,io_ooo_to_mem_lsqio_pendingPtr_flag,1);
            `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_lsqio_pendingPtr_value,io_ooo_to_mem_lsqio_pendingPtr_value,9);
            `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_flushSb,io_ooo_to_mem_flushSb,1);

        end
        //if(xxxTODOxxx==1'b1) begin
        //    mon_tr = lsqcommit_agent_agent_xaction::type_id::create("mon_tr");
        //    mon_tr.io_ooo_to_mem_lsqio_pendingPtr_flag = io_ooo_to_mem_lsqio_pendingPtr_flag;
        //    mon_tr.io_ooo_to_mem_lsqio_pendingPtr_value = io_ooo_to_mem_lsqio_pendingPtr_value;
        //    mon_tr.io_ooo_to_mem_flushSb = io_ooo_to_mem_flushSb;

        //    mon_tr.channel_id = this.cfg.channel_id;
        //    mon_tr.unpack();
        //    this.mon_item_port.write(mon_tr);
        //end
    end
endtask:mon_data

`endif
