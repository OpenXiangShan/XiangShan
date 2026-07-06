//=========================================================
//File name    : sbuffer_agent_agent_monitor.sv
//Author       : OpenAI_Codex
//Module name  : sbuffer_agent_agent_monitor
//Discribution : sbuffer_agent_agent_monitor : monitor
//Date         : 2026-04-12
//=========================================================
`ifndef SBUFFER_AGENT_AGENT_MONITOR__SV
`define SBUFFER_AGENT_AGENT_MONITOR__SV

class sbuffer_agent_agent_monitor  extends tcnt_monitor_base#(virtual sbuffer_agent_agent_interface,sbuffer_agent_agent_cfg,sbuffer_agent_agent_xaction);

    `uvm_component_utils(sbuffer_agent_agent_monitor)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern task run_phase(uvm_phase phase);
    extern task mon_data();
endclass:sbuffer_agent_agent_monitor

function sbuffer_agent_agent_monitor::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void sbuffer_agent_agent_monitor::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task sbuffer_agent_agent_monitor::run_phase(uvm_phase phase);
    super.run_phase(phase);
    this.mon_data();
endtask:run_phase

task sbuffer_agent_agent_monitor::mon_data();

    logic auto_inner_buffers_out_a_ready;
    logic auto_inner_buffers_out_a_valid;
    logic [3:0] auto_inner_buffers_out_a_bits_opcode;
    logic [2:0] auto_inner_buffers_out_a_bits_param;
    logic [2:0] auto_inner_buffers_out_a_bits_size;
    logic [3:0] auto_inner_buffers_out_a_bits_source;
    logic [47:0] auto_inner_buffers_out_a_bits_address;
    logic auto_inner_buffers_out_a_bits_user_memBackType_MM;
    logic auto_inner_buffers_out_a_bits_user_memPageType_NC;
    logic [7:0] auto_inner_buffers_out_a_bits_mask;
    logic [63:0] auto_inner_buffers_out_a_bits_data;
    logic auto_inner_buffers_out_a_bits_corrupt;
    logic auto_inner_buffers_out_d_ready;
    logic auto_inner_buffers_out_d_valid;
    logic [3:0] auto_inner_buffers_out_d_bits_opcode;
    logic [1:0] auto_inner_buffers_out_d_bits_param;
    logic [2:0] auto_inner_buffers_out_d_bits_size;
    logic [3:0] auto_inner_buffers_out_d_bits_source;
    logic auto_inner_buffers_out_d_bits_sink;
    logic auto_inner_buffers_out_d_bits_denied;
    logic [63:0] auto_inner_buffers_out_d_bits_data;
    logic auto_inner_buffers_out_d_bits_corrupt;

    sbuffer_agent_agent_xaction  mon_tr;
    while(1) begin
        @this.vif.mon_mp.mon_cb;
        auto_inner_buffers_out_a_ready = this.vif.mon_mp.mon_cb.auto_inner_buffers_out_a_ready;
        auto_inner_buffers_out_a_valid = this.vif.mon_mp.mon_cb.auto_inner_buffers_out_a_valid;
        auto_inner_buffers_out_a_bits_opcode = this.vif.mon_mp.mon_cb.auto_inner_buffers_out_a_bits_opcode;
        auto_inner_buffers_out_a_bits_param = this.vif.mon_mp.mon_cb.auto_inner_buffers_out_a_bits_param;
        auto_inner_buffers_out_a_bits_size = this.vif.mon_mp.mon_cb.auto_inner_buffers_out_a_bits_size;
        auto_inner_buffers_out_a_bits_source = this.vif.mon_mp.mon_cb.auto_inner_buffers_out_a_bits_source;
        auto_inner_buffers_out_a_bits_address = this.vif.mon_mp.mon_cb.auto_inner_buffers_out_a_bits_address;
        auto_inner_buffers_out_a_bits_user_memBackType_MM = this.vif.mon_mp.mon_cb.auto_inner_buffers_out_a_bits_user_memBackType_MM;
        auto_inner_buffers_out_a_bits_user_memPageType_NC = this.vif.mon_mp.mon_cb.auto_inner_buffers_out_a_bits_user_memPageType_NC;
        auto_inner_buffers_out_a_bits_mask = this.vif.mon_mp.mon_cb.auto_inner_buffers_out_a_bits_mask;
        auto_inner_buffers_out_a_bits_data = this.vif.mon_mp.mon_cb.auto_inner_buffers_out_a_bits_data;
        auto_inner_buffers_out_a_bits_corrupt = this.vif.mon_mp.mon_cb.auto_inner_buffers_out_a_bits_corrupt;
        auto_inner_buffers_out_d_ready = this.vif.mon_mp.mon_cb.auto_inner_buffers_out_d_ready;
        auto_inner_buffers_out_d_valid = this.vif.mon_mp.mon_cb.auto_inner_buffers_out_d_valid;
        auto_inner_buffers_out_d_bits_opcode = this.vif.mon_mp.mon_cb.auto_inner_buffers_out_d_bits_opcode;
        auto_inner_buffers_out_d_bits_param = this.vif.mon_mp.mon_cb.auto_inner_buffers_out_d_bits_param;
        auto_inner_buffers_out_d_bits_size = this.vif.mon_mp.mon_cb.auto_inner_buffers_out_d_bits_size;
        auto_inner_buffers_out_d_bits_source = this.vif.mon_mp.mon_cb.auto_inner_buffers_out_d_bits_source;
        auto_inner_buffers_out_d_bits_sink = this.vif.mon_mp.mon_cb.auto_inner_buffers_out_d_bits_sink;
        auto_inner_buffers_out_d_bits_denied = this.vif.mon_mp.mon_cb.auto_inner_buffers_out_d_bits_denied;
        auto_inner_buffers_out_d_bits_data = this.vif.mon_mp.mon_cb.auto_inner_buffers_out_d_bits_data;
        auto_inner_buffers_out_d_bits_corrupt = this.vif.mon_mp.mon_cb.auto_inner_buffers_out_d_bits_corrupt;

        if(this.cfg.xz_sw==tcnt_dec_base::ON && this.vif.rst_n==1'b1 && memblock_sync_pkg::reset_backend_done==1'b1) begin
            `TCNT_CHECK_SIG_XZ(auto_inner_buffers_out_a_ready,auto_inner_buffers_out_a_ready,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_buffers_out_a_valid,auto_inner_buffers_out_a_valid,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_buffers_out_a_bits_opcode,auto_inner_buffers_out_a_bits_opcode,4);
            `TCNT_CHECK_SIG_XZ(auto_inner_buffers_out_a_bits_param,auto_inner_buffers_out_a_bits_param,3);
            `TCNT_CHECK_SIG_XZ(auto_inner_buffers_out_a_bits_size,auto_inner_buffers_out_a_bits_size,3);
            `TCNT_CHECK_SIG_XZ(auto_inner_buffers_out_a_bits_source,auto_inner_buffers_out_a_bits_source,4);
            `TCNT_CHECK_SIG_XZ(auto_inner_buffers_out_a_bits_address,auto_inner_buffers_out_a_bits_address,48);
            `TCNT_CHECK_SIG_XZ(auto_inner_buffers_out_a_bits_user_memBackType_MM,auto_inner_buffers_out_a_bits_user_memBackType_MM,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_buffers_out_a_bits_user_memPageType_NC,auto_inner_buffers_out_a_bits_user_memPageType_NC,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_buffers_out_a_bits_mask,auto_inner_buffers_out_a_bits_mask,8);
            `TCNT_CHECK_SIG_XZ(auto_inner_buffers_out_a_bits_data,auto_inner_buffers_out_a_bits_data,64);
            `TCNT_CHECK_SIG_XZ(auto_inner_buffers_out_a_bits_corrupt,auto_inner_buffers_out_a_bits_corrupt,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_buffers_out_d_ready,auto_inner_buffers_out_d_ready,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_buffers_out_d_valid,auto_inner_buffers_out_d_valid,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_buffers_out_d_bits_opcode,auto_inner_buffers_out_d_bits_opcode,4);
            `TCNT_CHECK_SIG_XZ(auto_inner_buffers_out_d_bits_param,auto_inner_buffers_out_d_bits_param,2);
            `TCNT_CHECK_SIG_XZ(auto_inner_buffers_out_d_bits_size,auto_inner_buffers_out_d_bits_size,3);
            `TCNT_CHECK_SIG_XZ(auto_inner_buffers_out_d_bits_source,auto_inner_buffers_out_d_bits_source,4);
            `TCNT_CHECK_SIG_XZ(auto_inner_buffers_out_d_bits_sink,auto_inner_buffers_out_d_bits_sink,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_buffers_out_d_bits_denied,auto_inner_buffers_out_d_bits_denied,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_buffers_out_d_bits_data,auto_inner_buffers_out_d_bits_data,64);
            `TCNT_CHECK_SIG_XZ(auto_inner_buffers_out_d_bits_corrupt,auto_inner_buffers_out_d_bits_corrupt,1);

        end
        //if(xxxTODOxxx==1'b1) begin
        //    mon_tr = sbuffer_agent_agent_xaction::type_id::create("mon_tr");
        //    mon_tr.auto_inner_buffers_out_a_ready = auto_inner_buffers_out_a_ready;
        //    mon_tr.auto_inner_buffers_out_a_valid = auto_inner_buffers_out_a_valid;
        //    mon_tr.auto_inner_buffers_out_a_bits_opcode = auto_inner_buffers_out_a_bits_opcode;
        //    mon_tr.auto_inner_buffers_out_a_bits_param = auto_inner_buffers_out_a_bits_param;
        //    mon_tr.auto_inner_buffers_out_a_bits_size = auto_inner_buffers_out_a_bits_size;
        //    mon_tr.auto_inner_buffers_out_a_bits_source = auto_inner_buffers_out_a_bits_source;
        //    mon_tr.auto_inner_buffers_out_a_bits_address = auto_inner_buffers_out_a_bits_address;
        //    mon_tr.auto_inner_buffers_out_a_bits_user_memBackType_MM = auto_inner_buffers_out_a_bits_user_memBackType_MM;
        //    mon_tr.auto_inner_buffers_out_a_bits_user_memPageType_NC = auto_inner_buffers_out_a_bits_user_memPageType_NC;
        //    mon_tr.auto_inner_buffers_out_a_bits_mask = auto_inner_buffers_out_a_bits_mask;
        //    mon_tr.auto_inner_buffers_out_a_bits_data = auto_inner_buffers_out_a_bits_data;
        //    mon_tr.auto_inner_buffers_out_a_bits_corrupt = auto_inner_buffers_out_a_bits_corrupt;
        //    mon_tr.auto_inner_buffers_out_d_ready = auto_inner_buffers_out_d_ready;
        //    mon_tr.auto_inner_buffers_out_d_valid = auto_inner_buffers_out_d_valid;
        //    mon_tr.auto_inner_buffers_out_d_bits_opcode = auto_inner_buffers_out_d_bits_opcode;
        //    mon_tr.auto_inner_buffers_out_d_bits_param = auto_inner_buffers_out_d_bits_param;
        //    mon_tr.auto_inner_buffers_out_d_bits_size = auto_inner_buffers_out_d_bits_size;
        //    mon_tr.auto_inner_buffers_out_d_bits_source = auto_inner_buffers_out_d_bits_source;
        //    mon_tr.auto_inner_buffers_out_d_bits_sink = auto_inner_buffers_out_d_bits_sink;
        //    mon_tr.auto_inner_buffers_out_d_bits_denied = auto_inner_buffers_out_d_bits_denied;
        //    mon_tr.auto_inner_buffers_out_d_bits_data = auto_inner_buffers_out_d_bits_data;
        //    mon_tr.auto_inner_buffers_out_d_bits_corrupt = auto_inner_buffers_out_d_bits_corrupt;

        //    mon_tr.channel_id = this.cfg.channel_id;
        //    mon_tr.unpack();
        //    this.mon_item_port.write(mon_tr);
        //end
    end
endtask:mon_data

`endif
