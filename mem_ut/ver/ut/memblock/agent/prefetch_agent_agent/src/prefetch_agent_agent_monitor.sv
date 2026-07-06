//=========================================================
//File name    : prefetch_agent_agent_monitor.sv
//Author       : OpenAI_Codex
//Module name  : prefetch_agent_agent_monitor
//Discribution : prefetch_agent_agent_monitor : monitor
//Date         : 2026-04-12
//=========================================================
`ifndef PREFETCH_AGENT_AGENT_MONITOR__SV
`define PREFETCH_AGENT_AGENT_MONITOR__SV

class prefetch_agent_agent_monitor  extends tcnt_monitor_base#(virtual prefetch_agent_agent_interface,prefetch_agent_agent_cfg,prefetch_agent_agent_xaction);

    `uvm_component_utils(prefetch_agent_agent_monitor)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern task run_phase(uvm_phase phase);
    extern task mon_data();
endclass:prefetch_agent_agent_monitor

function prefetch_agent_agent_monitor::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void prefetch_agent_agent_monitor::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task prefetch_agent_agent_monitor::run_phase(uvm_phase phase);
    super.run_phase(phase);
    this.mon_data();
endtask:run_phase

task prefetch_agent_agent_monitor::mon_data();

    logic [63:0] auto_inner_l3_pf_sender_out_addr;
    logic auto_inner_l3_pf_sender_out_addr_valid;
    logic auto_inner_l3_pf_sender_out_l2_pf_en;
    logic [63:0] auto_inner_l2_pf_sender_out_addr;
    logic [4:0] auto_inner_l2_pf_sender_out_pf_source;
    logic auto_inner_l2_pf_sender_out_addr_valid;
    logic auto_inner_l2_pf_sender_out_l2_pf_en;
    logic io_ifetchPrefetch_0_valid    ;
    logic [49:0] io_ifetchPrefetch_0_bits_vaddr;
    logic io_ifetchPrefetch_1_valid    ;
    logic [49:0] io_ifetchPrefetch_1_bits_vaddr;
    logic io_ifetchPrefetch_2_valid    ;
    logic [49:0] io_ifetchPrefetch_2_bits_vaddr;

    prefetch_agent_agent_xaction  mon_tr;
    while(1) begin
        @this.vif.mon_mp.mon_cb;
        auto_inner_l3_pf_sender_out_addr = this.vif.mon_mp.mon_cb.auto_inner_l3_pf_sender_out_addr;
        auto_inner_l3_pf_sender_out_addr_valid = this.vif.mon_mp.mon_cb.auto_inner_l3_pf_sender_out_addr_valid;
        auto_inner_l3_pf_sender_out_l2_pf_en = this.vif.mon_mp.mon_cb.auto_inner_l3_pf_sender_out_l2_pf_en;
        auto_inner_l2_pf_sender_out_addr = this.vif.mon_mp.mon_cb.auto_inner_l2_pf_sender_out_addr;
        auto_inner_l2_pf_sender_out_pf_source = this.vif.mon_mp.mon_cb.auto_inner_l2_pf_sender_out_pf_source;
        auto_inner_l2_pf_sender_out_addr_valid = this.vif.mon_mp.mon_cb.auto_inner_l2_pf_sender_out_addr_valid;
        auto_inner_l2_pf_sender_out_l2_pf_en = this.vif.mon_mp.mon_cb.auto_inner_l2_pf_sender_out_l2_pf_en;
        io_ifetchPrefetch_0_valid = this.vif.mon_mp.mon_cb.io_ifetchPrefetch_0_valid;
        io_ifetchPrefetch_0_bits_vaddr = this.vif.mon_mp.mon_cb.io_ifetchPrefetch_0_bits_vaddr;
        io_ifetchPrefetch_1_valid = this.vif.mon_mp.mon_cb.io_ifetchPrefetch_1_valid;
        io_ifetchPrefetch_1_bits_vaddr = this.vif.mon_mp.mon_cb.io_ifetchPrefetch_1_bits_vaddr;
        io_ifetchPrefetch_2_valid = this.vif.mon_mp.mon_cb.io_ifetchPrefetch_2_valid;
        io_ifetchPrefetch_2_bits_vaddr = this.vif.mon_mp.mon_cb.io_ifetchPrefetch_2_bits_vaddr;

        if(this.cfg.xz_sw==tcnt_dec_base::ON && this.vif.rst_n==1'b1 && memblock_sync_pkg::reset_backend_done==1'b1) begin
            `TCNT_CHECK_SIG_XZ(auto_inner_l3_pf_sender_out_addr,auto_inner_l3_pf_sender_out_addr,64);
            `TCNT_CHECK_SIG_XZ(auto_inner_l3_pf_sender_out_addr_valid,auto_inner_l3_pf_sender_out_addr_valid,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_l3_pf_sender_out_l2_pf_en,auto_inner_l3_pf_sender_out_l2_pf_en,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_l2_pf_sender_out_addr,auto_inner_l2_pf_sender_out_addr,64);
            `TCNT_CHECK_SIG_XZ(auto_inner_l2_pf_sender_out_pf_source,auto_inner_l2_pf_sender_out_pf_source,5);
            `TCNT_CHECK_SIG_XZ(auto_inner_l2_pf_sender_out_addr_valid,auto_inner_l2_pf_sender_out_addr_valid,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_l2_pf_sender_out_l2_pf_en,auto_inner_l2_pf_sender_out_l2_pf_en,1);
            `TCNT_CHECK_SIG_XZ(io_ifetchPrefetch_0_valid,io_ifetchPrefetch_0_valid,1);
            `TCNT_CHECK_SIG_XZ(io_ifetchPrefetch_0_bits_vaddr,io_ifetchPrefetch_0_bits_vaddr,50);
            `TCNT_CHECK_SIG_XZ(io_ifetchPrefetch_1_valid,io_ifetchPrefetch_1_valid,1);
            `TCNT_CHECK_SIG_XZ(io_ifetchPrefetch_1_bits_vaddr,io_ifetchPrefetch_1_bits_vaddr,50);
            `TCNT_CHECK_SIG_XZ(io_ifetchPrefetch_2_valid,io_ifetchPrefetch_2_valid,1);
            `TCNT_CHECK_SIG_XZ(io_ifetchPrefetch_2_bits_vaddr,io_ifetchPrefetch_2_bits_vaddr,50);

        end
        //if(xxxTODOxxx==1'b1) begin
        //    mon_tr = prefetch_agent_agent_xaction::type_id::create("mon_tr");
        //    mon_tr.auto_inner_l3_pf_sender_out_addr = auto_inner_l3_pf_sender_out_addr;
        //    mon_tr.auto_inner_l3_pf_sender_out_addr_valid = auto_inner_l3_pf_sender_out_addr_valid;
        //    mon_tr.auto_inner_l3_pf_sender_out_l2_pf_en = auto_inner_l3_pf_sender_out_l2_pf_en;
        //    mon_tr.auto_inner_l2_pf_sender_out_addr = auto_inner_l2_pf_sender_out_addr;
        //    mon_tr.auto_inner_l2_pf_sender_out_pf_source = auto_inner_l2_pf_sender_out_pf_source;
        //    mon_tr.auto_inner_l2_pf_sender_out_addr_valid = auto_inner_l2_pf_sender_out_addr_valid;
        //    mon_tr.auto_inner_l2_pf_sender_out_l2_pf_en = auto_inner_l2_pf_sender_out_l2_pf_en;
        //    mon_tr.io_ifetchPrefetch_0_valid = io_ifetchPrefetch_0_valid;
        //    mon_tr.io_ifetchPrefetch_0_bits_vaddr = io_ifetchPrefetch_0_bits_vaddr;
        //    mon_tr.io_ifetchPrefetch_1_valid = io_ifetchPrefetch_1_valid;
        //    mon_tr.io_ifetchPrefetch_1_bits_vaddr = io_ifetchPrefetch_1_bits_vaddr;
        //    mon_tr.io_ifetchPrefetch_2_valid = io_ifetchPrefetch_2_valid;
        //    mon_tr.io_ifetchPrefetch_2_bits_vaddr = io_ifetchPrefetch_2_bits_vaddr;

        //    mon_tr.channel_id = this.cfg.channel_id;
        //    mon_tr.unpack();
        //    this.mon_item_port.write(mon_tr);
        //end
    end
endtask:mon_data

`endif
