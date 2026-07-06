//=========================================================
//File name    : fence_agent_agent_monitor.sv
//Author       : OpenAI_Codex
//Module name  : fence_agent_agent_monitor
//Discribution : fence_agent_agent_monitor : monitor
//Date         : 2026-04-12
//=========================================================
`ifndef FENCE_AGENT_AGENT_MONITOR__SV
`define FENCE_AGENT_AGENT_MONITOR__SV

class fence_agent_agent_monitor  extends tcnt_monitor_base#(virtual fence_agent_agent_interface,fence_agent_agent_cfg,fence_agent_agent_xaction);

    `uvm_component_utils(fence_agent_agent_monitor)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern task run_phase(uvm_phase phase);
    extern task mon_data();
endclass:fence_agent_agent_monitor

function fence_agent_agent_monitor::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void fence_agent_agent_monitor::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task fence_agent_agent_monitor::run_phase(uvm_phase phase);
    super.run_phase(phase);
    this.mon_data();
endtask:run_phase

task fence_agent_agent_monitor::mon_data();

    logic io_ooo_to_mem_sfence_valid   ;
    logic io_ooo_to_mem_sfence_bits_rs1;
    logic io_ooo_to_mem_sfence_bits_rs2;
    logic [49:0] io_ooo_to_mem_sfence_bits_addr;
    logic [15:0] io_ooo_to_mem_sfence_bits_id;
    logic io_ooo_to_mem_sfence_bits_hv ;
    logic io_ooo_to_mem_sfence_bits_hg ;

    fence_agent_agent_xaction  mon_tr;
    memblock_sync_pkg::dispatch_raw_sfence_t raw_sfence;
    while(1) begin
        @this.vif.mon_mp.mon_cb;
        io_ooo_to_mem_sfence_valid = this.vif.mon_mp.mon_cb.io_ooo_to_mem_sfence_valid;
        io_ooo_to_mem_sfence_bits_rs1 = this.vif.mon_mp.mon_cb.io_ooo_to_mem_sfence_bits_rs1;
        io_ooo_to_mem_sfence_bits_rs2 = this.vif.mon_mp.mon_cb.io_ooo_to_mem_sfence_bits_rs2;
        io_ooo_to_mem_sfence_bits_addr = this.vif.mon_mp.mon_cb.io_ooo_to_mem_sfence_bits_addr;
        io_ooo_to_mem_sfence_bits_id = this.vif.mon_mp.mon_cb.io_ooo_to_mem_sfence_bits_id;
        io_ooo_to_mem_sfence_bits_hv = this.vif.mon_mp.mon_cb.io_ooo_to_mem_sfence_bits_hv;
        io_ooo_to_mem_sfence_bits_hg = this.vif.mon_mp.mon_cb.io_ooo_to_mem_sfence_bits_hg;

        if(this.cfg.xz_sw==tcnt_dec_base::ON && this.vif.rst_n==1'b1 && memblock_sync_pkg::reset_backend_done==1'b1) begin
            `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_sfence_valid,io_ooo_to_mem_sfence_valid,1);
            `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_sfence_bits_rs1,io_ooo_to_mem_sfence_bits_rs1,1);
            `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_sfence_bits_rs2,io_ooo_to_mem_sfence_bits_rs2,1);
            `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_sfence_bits_addr,io_ooo_to_mem_sfence_bits_addr,50);
            `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_sfence_bits_id,io_ooo_to_mem_sfence_bits_id,16);
            `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_sfence_bits_hv,io_ooo_to_mem_sfence_bits_hv,1);
            `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_sfence_bits_hg,io_ooo_to_mem_sfence_bits_hg,1);

        end
        if(this.vif.rst_n==1'b1 &&
           memblock_sync_pkg::reset_backend_done==1'b1 &&
           io_ooo_to_mem_sfence_valid==1'b1) begin
            raw_sfence = memblock_sync_pkg::make_empty_raw_sfence();
            raw_sfence.valid = 1'b1;
            raw_sfence.rs1   = io_ooo_to_mem_sfence_bits_rs1;
            raw_sfence.rs2   = io_ooo_to_mem_sfence_bits_rs2;
            raw_sfence.addr  = io_ooo_to_mem_sfence_bits_addr;
            raw_sfence.id    = io_ooo_to_mem_sfence_bits_id;
            raw_sfence.hv    = io_ooo_to_mem_sfence_bits_hv;
            raw_sfence.hg    = io_ooo_to_mem_sfence_bits_hg;
            raw_sfence.cycle = memblock_sync_pkg::get_dispatch_service_cycle();
            memblock_sync_pkg::push_raw_sfence(raw_sfence);
        end
        //if(xxxTODOxxx==1'b1) begin
        //    mon_tr = fence_agent_agent_xaction::type_id::create("mon_tr");
        //    mon_tr.io_ooo_to_mem_sfence_valid = io_ooo_to_mem_sfence_valid;
        //    mon_tr.io_ooo_to_mem_sfence_bits_rs1 = io_ooo_to_mem_sfence_bits_rs1;
        //    mon_tr.io_ooo_to_mem_sfence_bits_rs2 = io_ooo_to_mem_sfence_bits_rs2;
        //    mon_tr.io_ooo_to_mem_sfence_bits_addr = io_ooo_to_mem_sfence_bits_addr;
        //    mon_tr.io_ooo_to_mem_sfence_bits_id = io_ooo_to_mem_sfence_bits_id;
        //    mon_tr.io_ooo_to_mem_sfence_bits_hv = io_ooo_to_mem_sfence_bits_hv;
        //    mon_tr.io_ooo_to_mem_sfence_bits_hg = io_ooo_to_mem_sfence_bits_hg;

        //    mon_tr.channel_id = this.cfg.channel_id;
        //    mon_tr.unpack();
        //    this.mon_item_port.write(mon_tr);
        //end
    end
endtask:mon_data

`endif
