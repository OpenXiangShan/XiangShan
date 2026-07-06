//=========================================================
//File name    : int_sink_agent_agent_monitor.sv
//Author       : OpenAI_Codex
//Module name  : int_sink_agent_agent_monitor
//Discribution : int_sink_agent_agent_monitor : monitor
//Date         : 2026-04-12
//=========================================================
`ifndef INT_SINK_AGENT_AGENT_MONITOR__SV
`define INT_SINK_AGENT_AGENT_MONITOR__SV

class int_sink_agent_agent_monitor  extends tcnt_monitor_base#(virtual int_sink_agent_agent_interface,int_sink_agent_agent_cfg,int_sink_agent_agent_xaction);

    `uvm_component_utils(int_sink_agent_agent_monitor)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern task run_phase(uvm_phase phase);
    extern task mon_data();
endclass:int_sink_agent_agent_monitor

function int_sink_agent_agent_monitor::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void int_sink_agent_agent_monitor::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task int_sink_agent_agent_monitor::run_phase(uvm_phase phase);
    super.run_phase(phase);
    this.mon_data();
endtask:run_phase

task int_sink_agent_agent_monitor::mon_data();

    logic auto_inner_beu_local_int_sink_in_0;
    logic auto_inner_nmi_int_sink_in_0 ;
    logic auto_inner_nmi_int_sink_in_1 ;
    logic auto_inner_plic_int_sink_in_1_0;
    logic auto_inner_plic_int_sink_in_0_0;
    logic auto_inner_clint_int_sink_in_0;
    logic auto_inner_clint_int_sink_in_1;

    int_sink_agent_agent_xaction  mon_tr;
    while(1) begin
        @this.vif.mon_mp.mon_cb;
        auto_inner_beu_local_int_sink_in_0 = this.vif.mon_mp.mon_cb.auto_inner_beu_local_int_sink_in_0;
        auto_inner_nmi_int_sink_in_0 = this.vif.mon_mp.mon_cb.auto_inner_nmi_int_sink_in_0;
        auto_inner_nmi_int_sink_in_1 = this.vif.mon_mp.mon_cb.auto_inner_nmi_int_sink_in_1;
        auto_inner_plic_int_sink_in_1_0 = this.vif.mon_mp.mon_cb.auto_inner_plic_int_sink_in_1_0;
        auto_inner_plic_int_sink_in_0_0 = this.vif.mon_mp.mon_cb.auto_inner_plic_int_sink_in_0_0;
        auto_inner_clint_int_sink_in_0 = this.vif.mon_mp.mon_cb.auto_inner_clint_int_sink_in_0;
        auto_inner_clint_int_sink_in_1 = this.vif.mon_mp.mon_cb.auto_inner_clint_int_sink_in_1;

        if(this.cfg.xz_sw==tcnt_dec_base::ON && this.vif.rst_n==1'b1 && memblock_sync_pkg::reset_backend_done==1'b1) begin
            `TCNT_CHECK_SIG_XZ(auto_inner_beu_local_int_sink_in_0,auto_inner_beu_local_int_sink_in_0,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_nmi_int_sink_in_0,auto_inner_nmi_int_sink_in_0,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_nmi_int_sink_in_1,auto_inner_nmi_int_sink_in_1,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_plic_int_sink_in_1_0,auto_inner_plic_int_sink_in_1_0,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_plic_int_sink_in_0_0,auto_inner_plic_int_sink_in_0_0,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_clint_int_sink_in_0,auto_inner_clint_int_sink_in_0,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_clint_int_sink_in_1,auto_inner_clint_int_sink_in_1,1);

        end
        //if(xxxTODOxxx==1'b1) begin
        //    mon_tr = int_sink_agent_agent_xaction::type_id::create("mon_tr");
        //    mon_tr.auto_inner_beu_local_int_sink_in_0 = auto_inner_beu_local_int_sink_in_0;
        //    mon_tr.auto_inner_nmi_int_sink_in_0 = auto_inner_nmi_int_sink_in_0;
        //    mon_tr.auto_inner_nmi_int_sink_in_1 = auto_inner_nmi_int_sink_in_1;
        //    mon_tr.auto_inner_plic_int_sink_in_1_0 = auto_inner_plic_int_sink_in_1_0;
        //    mon_tr.auto_inner_plic_int_sink_in_0_0 = auto_inner_plic_int_sink_in_0_0;
        //    mon_tr.auto_inner_clint_int_sink_in_0 = auto_inner_clint_int_sink_in_0;
        //    mon_tr.auto_inner_clint_int_sink_in_1 = auto_inner_clint_int_sink_in_1;

        //    mon_tr.channel_id = this.cfg.channel_id;
        //    mon_tr.unpack();
        //    this.mon_item_port.write(mon_tr);
        //end
    end
endtask:mon_data

`endif
