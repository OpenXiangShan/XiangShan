//=========================================================
//File name    : lsqcommit_agent_agent_monitor.sv
//Author       : OpenAI_Codex
//Module name  : lsqcommit_agent_agent_monitor
//Discribution : lsqcommit_agent_agent_monitor : monitor
//Date         : 2026-04-12
//=========================================================
`ifndef LSQCOMMIT_AGENT_AGENT_MONITOR__SV
`define LSQCOMMIT_AGENT_AGENT_MONITOR__SV

`include "memblock_compile_params.svh"

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
    logic [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] io_ooo_to_mem_lsqio_pendingPtr_value;
    logic io_ooo_to_mem_flushSb        ;

    logic io_ooo_to_mem_lsqio_pendingMMIOld;
    logic io_ooo_to_mem_lsqio_pendingst;
    logic [3:0] io_ooo_to_mem_lsqio_scommit;
    logic io_ooo_to_mem_isStoreException;
    longint unsigned sample_seq;
    lsqcommit_agent_agent_xaction  mon_tr;
    while(1) begin
        @this.vif.mon_mp.mon_cb;
        io_ooo_to_mem_lsqio_pendingPtr_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_lsqio_pendingPtr_flag;
        io_ooo_to_mem_lsqio_pendingPtr_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_lsqio_pendingPtr_value;
        io_ooo_to_mem_flushSb = this.vif.mon_mp.mon_cb.io_ooo_to_mem_flushSb;

        io_ooo_to_mem_lsqio_pendingMMIOld = this.vif.mon_mp.mon_cb.io_ooo_to_mem_lsqio_pendingMMIOld;
        io_ooo_to_mem_lsqio_pendingst = this.vif.mon_mp.mon_cb.io_ooo_to_mem_lsqio_pendingst;
        io_ooo_to_mem_lsqio_scommit = this.vif.mon_mp.mon_cb.io_ooo_to_mem_lsqio_scommit;
        io_ooo_to_mem_isStoreException = this.vif.mon_mp.mon_cb.io_ooo_to_mem_isStoreException;

        if(this.cfg.xz_sw==tcnt_dec_base::ON && this.vif.rst_n==1'b1 && memblock_sync_pkg::reset_backend_done==1'b1) begin
            `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_lsqio_pendingPtr_flag,io_ooo_to_mem_lsqio_pendingPtr_flag,1);
            `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_lsqio_pendingPtr_value,io_ooo_to_mem_lsqio_pendingPtr_value,`MEMBLOCK_DUT_ROB_VALUE_W);
            `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_flushSb,io_ooo_to_mem_flushSb,1);
            `ifdef MEMBLOCK_UT
                `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_isStoreException,io_ooo_to_mem_isStoreException,1);
            `endif

        end
        if (this.vif.rst_n === 1'b1 && memblock_sync_pkg::reset_backend_done === 1'b1 &&
            !memblock_sync_pkg::l2tlb_reset_active()) begin
            memblock_sync_pkg::backend_cycle_input_t raw;
            memblock_sync_pkg::wait_for_l2tlb_sample_anchor($time, sample_seq);
            raw = '{default:'0};
            raw.reset_epoch = memblock_sync_pkg::get_l2tlb_current_reset_epoch();
            raw.sample_seq = sample_seq;
            raw.value.pending_flag = io_ooo_to_mem_lsqio_pendingPtr_flag;
            raw.value.pending_value = io_ooo_to_mem_lsqio_pendingPtr_value;
            raw.value.pending_st = io_ooo_to_mem_lsqio_pendingst;
            raw.value.pending_mmio = io_ooo_to_mem_lsqio_pendingMMIOld;
            raw.value.store_exception = io_ooo_to_mem_isStoreException;
            raw.value.scommit = io_ooo_to_mem_lsqio_scommit;
            memblock_sync_pkg::backend_lsq_sample_time = $time;
            memblock_sync_pkg::publish_backend_input(0, raw);
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
