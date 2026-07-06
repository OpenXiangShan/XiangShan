//=========================================================
//File name    : io_mem_to_ooo_wakeup_agent_agent_monitor.sv
//Author       : OpenAI_Codex
//Module name  : io_mem_to_ooo_wakeup_agent_agent_monitor
//Discribution : io_mem_to_ooo_wakeup_agent_agent_monitor : monitor
//Date         : 2026-04-12
//=========================================================
`ifndef IO_MEM_TO_OOO_WAKEUP_AGENT_AGENT_MONITOR__SV
`define IO_MEM_TO_OOO_WAKEUP_AGENT_AGENT_MONITOR__SV

class io_mem_to_ooo_wakeup_agent_agent_monitor  extends tcnt_monitor_base#(virtual io_mem_to_ooo_wakeup_agent_agent_interface,io_mem_to_ooo_wakeup_agent_agent_cfg,io_mem_to_ooo_wakeup_agent_agent_xaction);

    `uvm_component_utils(io_mem_to_ooo_wakeup_agent_agent_monitor)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern task run_phase(uvm_phase phase);
    extern task mon_data();
endclass:io_mem_to_ooo_wakeup_agent_agent_monitor

function io_mem_to_ooo_wakeup_agent_agent_monitor::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void io_mem_to_ooo_wakeup_agent_agent_monitor::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task io_mem_to_ooo_wakeup_agent_agent_monitor::run_phase(uvm_phase phase);
    super.run_phase(phase);
    this.mon_data();
endtask:run_phase

task io_mem_to_ooo_wakeup_agent_agent_monitor::mon_data();

    logic io_mem_to_ooo_wakeup_0_valid ;
    logic io_mem_to_ooo_wakeup_0_bits_rfWen;
    logic io_mem_to_ooo_wakeup_0_bits_fpWen;
    logic io_mem_to_ooo_wakeup_0_bits_vecWen;
    logic io_mem_to_ooo_wakeup_0_bits_v0Wen;
    logic io_mem_to_ooo_wakeup_0_bits_vlWen;
    logic [7:0] io_mem_to_ooo_wakeup_0_bits_pdest;
    logic io_mem_to_ooo_wakeup_1_valid ;
    logic io_mem_to_ooo_wakeup_1_bits_rfWen;
    logic io_mem_to_ooo_wakeup_1_bits_fpWen;
    logic io_mem_to_ooo_wakeup_1_bits_vecWen;
    logic io_mem_to_ooo_wakeup_1_bits_v0Wen;
    logic io_mem_to_ooo_wakeup_1_bits_vlWen;
    logic [7:0] io_mem_to_ooo_wakeup_1_bits_pdest;
    logic io_mem_to_ooo_wakeup_2_valid ;
    logic io_mem_to_ooo_wakeup_2_bits_rfWen;
    logic io_mem_to_ooo_wakeup_2_bits_fpWen;
    logic io_mem_to_ooo_wakeup_2_bits_vecWen;
    logic io_mem_to_ooo_wakeup_2_bits_v0Wen;
    logic io_mem_to_ooo_wakeup_2_bits_vlWen;
    logic [7:0] io_mem_to_ooo_wakeup_2_bits_pdest;

    io_mem_to_ooo_wakeup_agent_agent_xaction  mon_tr;
    while(1) begin
        @this.vif.mon_mp.mon_cb;
        io_mem_to_ooo_wakeup_0_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_wakeup_0_valid;
        io_mem_to_ooo_wakeup_0_bits_rfWen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_wakeup_0_bits_rfWen;
        io_mem_to_ooo_wakeup_0_bits_fpWen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_wakeup_0_bits_fpWen;
        io_mem_to_ooo_wakeup_0_bits_vecWen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_wakeup_0_bits_vecWen;
        io_mem_to_ooo_wakeup_0_bits_v0Wen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_wakeup_0_bits_v0Wen;
        io_mem_to_ooo_wakeup_0_bits_vlWen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_wakeup_0_bits_vlWen;
        io_mem_to_ooo_wakeup_0_bits_pdest = this.vif.mon_mp.mon_cb.io_mem_to_ooo_wakeup_0_bits_pdest;
        io_mem_to_ooo_wakeup_1_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_wakeup_1_valid;
        io_mem_to_ooo_wakeup_1_bits_rfWen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_wakeup_1_bits_rfWen;
        io_mem_to_ooo_wakeup_1_bits_fpWen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_wakeup_1_bits_fpWen;
        io_mem_to_ooo_wakeup_1_bits_vecWen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_wakeup_1_bits_vecWen;
        io_mem_to_ooo_wakeup_1_bits_v0Wen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_wakeup_1_bits_v0Wen;
        io_mem_to_ooo_wakeup_1_bits_vlWen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_wakeup_1_bits_vlWen;
        io_mem_to_ooo_wakeup_1_bits_pdest = this.vif.mon_mp.mon_cb.io_mem_to_ooo_wakeup_1_bits_pdest;
        io_mem_to_ooo_wakeup_2_valid = this.vif.mon_mp.mon_cb.io_mem_to_ooo_wakeup_2_valid;
        io_mem_to_ooo_wakeup_2_bits_rfWen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_wakeup_2_bits_rfWen;
        io_mem_to_ooo_wakeup_2_bits_fpWen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_wakeup_2_bits_fpWen;
        io_mem_to_ooo_wakeup_2_bits_vecWen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_wakeup_2_bits_vecWen;
        io_mem_to_ooo_wakeup_2_bits_v0Wen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_wakeup_2_bits_v0Wen;
        io_mem_to_ooo_wakeup_2_bits_vlWen = this.vif.mon_mp.mon_cb.io_mem_to_ooo_wakeup_2_bits_vlWen;
        io_mem_to_ooo_wakeup_2_bits_pdest = this.vif.mon_mp.mon_cb.io_mem_to_ooo_wakeup_2_bits_pdest;

        if(this.cfg.xz_sw==tcnt_dec_base::ON && this.vif.rst_n==1'b1 && memblock_sync_pkg::reset_backend_done==1'b1) begin
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_wakeup_0_valid,io_mem_to_ooo_wakeup_0_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_wakeup_0_bits_rfWen,io_mem_to_ooo_wakeup_0_bits_rfWen,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_wakeup_0_bits_fpWen,io_mem_to_ooo_wakeup_0_bits_fpWen,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_wakeup_0_bits_vecWen,io_mem_to_ooo_wakeup_0_bits_vecWen,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_wakeup_0_bits_v0Wen,io_mem_to_ooo_wakeup_0_bits_v0Wen,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_wakeup_0_bits_vlWen,io_mem_to_ooo_wakeup_0_bits_vlWen,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_wakeup_0_bits_pdest,io_mem_to_ooo_wakeup_0_bits_pdest,8);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_wakeup_1_valid,io_mem_to_ooo_wakeup_1_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_wakeup_1_bits_rfWen,io_mem_to_ooo_wakeup_1_bits_rfWen,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_wakeup_1_bits_fpWen,io_mem_to_ooo_wakeup_1_bits_fpWen,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_wakeup_1_bits_vecWen,io_mem_to_ooo_wakeup_1_bits_vecWen,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_wakeup_1_bits_v0Wen,io_mem_to_ooo_wakeup_1_bits_v0Wen,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_wakeup_1_bits_vlWen,io_mem_to_ooo_wakeup_1_bits_vlWen,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_wakeup_1_bits_pdest,io_mem_to_ooo_wakeup_1_bits_pdest,8);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_wakeup_2_valid,io_mem_to_ooo_wakeup_2_valid,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_wakeup_2_bits_rfWen,io_mem_to_ooo_wakeup_2_bits_rfWen,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_wakeup_2_bits_fpWen,io_mem_to_ooo_wakeup_2_bits_fpWen,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_wakeup_2_bits_vecWen,io_mem_to_ooo_wakeup_2_bits_vecWen,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_wakeup_2_bits_v0Wen,io_mem_to_ooo_wakeup_2_bits_v0Wen,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_wakeup_2_bits_vlWen,io_mem_to_ooo_wakeup_2_bits_vlWen,1);
            `TCNT_CHECK_SIG_XZ(io_mem_to_ooo_wakeup_2_bits_pdest,io_mem_to_ooo_wakeup_2_bits_pdest,8);

        end
        //if(xxxTODOxxx==1'b1) begin
        //    mon_tr = io_mem_to_ooo_wakeup_agent_agent_xaction::type_id::create("mon_tr");
        //    mon_tr.io_mem_to_ooo_wakeup_0_valid = io_mem_to_ooo_wakeup_0_valid;
        //    mon_tr.io_mem_to_ooo_wakeup_0_bits_rfWen = io_mem_to_ooo_wakeup_0_bits_rfWen;
        //    mon_tr.io_mem_to_ooo_wakeup_0_bits_fpWen = io_mem_to_ooo_wakeup_0_bits_fpWen;
        //    mon_tr.io_mem_to_ooo_wakeup_0_bits_vecWen = io_mem_to_ooo_wakeup_0_bits_vecWen;
        //    mon_tr.io_mem_to_ooo_wakeup_0_bits_v0Wen = io_mem_to_ooo_wakeup_0_bits_v0Wen;
        //    mon_tr.io_mem_to_ooo_wakeup_0_bits_vlWen = io_mem_to_ooo_wakeup_0_bits_vlWen;
        //    mon_tr.io_mem_to_ooo_wakeup_0_bits_pdest = io_mem_to_ooo_wakeup_0_bits_pdest;
        //    mon_tr.io_mem_to_ooo_wakeup_1_valid = io_mem_to_ooo_wakeup_1_valid;
        //    mon_tr.io_mem_to_ooo_wakeup_1_bits_rfWen = io_mem_to_ooo_wakeup_1_bits_rfWen;
        //    mon_tr.io_mem_to_ooo_wakeup_1_bits_fpWen = io_mem_to_ooo_wakeup_1_bits_fpWen;
        //    mon_tr.io_mem_to_ooo_wakeup_1_bits_vecWen = io_mem_to_ooo_wakeup_1_bits_vecWen;
        //    mon_tr.io_mem_to_ooo_wakeup_1_bits_v0Wen = io_mem_to_ooo_wakeup_1_bits_v0Wen;
        //    mon_tr.io_mem_to_ooo_wakeup_1_bits_vlWen = io_mem_to_ooo_wakeup_1_bits_vlWen;
        //    mon_tr.io_mem_to_ooo_wakeup_1_bits_pdest = io_mem_to_ooo_wakeup_1_bits_pdest;
        //    mon_tr.io_mem_to_ooo_wakeup_2_valid = io_mem_to_ooo_wakeup_2_valid;
        //    mon_tr.io_mem_to_ooo_wakeup_2_bits_rfWen = io_mem_to_ooo_wakeup_2_bits_rfWen;
        //    mon_tr.io_mem_to_ooo_wakeup_2_bits_fpWen = io_mem_to_ooo_wakeup_2_bits_fpWen;
        //    mon_tr.io_mem_to_ooo_wakeup_2_bits_vecWen = io_mem_to_ooo_wakeup_2_bits_vecWen;
        //    mon_tr.io_mem_to_ooo_wakeup_2_bits_v0Wen = io_mem_to_ooo_wakeup_2_bits_v0Wen;
        //    mon_tr.io_mem_to_ooo_wakeup_2_bits_vlWen = io_mem_to_ooo_wakeup_2_bits_vlWen;
        //    mon_tr.io_mem_to_ooo_wakeup_2_bits_pdest = io_mem_to_ooo_wakeup_2_bits_pdest;

        //    mon_tr.channel_id = this.cfg.channel_id;
        //    mon_tr.unpack();
        //    this.mon_item_port.write(mon_tr);
        //end
    end
endtask:mon_data

`endif
