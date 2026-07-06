//=========================================================
//File name    : dcache_agent_agent_monitor.sv
//Author       : OpenAI_Codex
//Module name  : dcache_agent_agent_monitor
//Discribution : dcache_agent_agent_monitor : monitor
//Date         : 2026-04-12
//=========================================================
`ifndef DCACHE_AGENT_AGENT_MONITOR__SV
`define DCACHE_AGENT_AGENT_MONITOR__SV

class dcache_agent_agent_monitor  extends tcnt_monitor_base#(virtual dcache_agent_agent_interface,dcache_agent_agent_cfg,dcache_agent_agent_xaction);

    `uvm_component_utils(dcache_agent_agent_monitor)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern task run_phase(uvm_phase phase);
    extern task mon_data();
endclass:dcache_agent_agent_monitor

function dcache_agent_agent_monitor::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void dcache_agent_agent_monitor::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task dcache_agent_agent_monitor::run_phase(uvm_phase phase);
    super.run_phase(phase);
    this.mon_data();
endtask:run_phase

task dcache_agent_agent_monitor::mon_data();

    logic auto_inner_dcache_client_out_a_ready;
    logic auto_inner_dcache_client_out_a_valid;
    logic [3:0] auto_inner_dcache_client_out_a_bits_opcode;
    logic [2:0] auto_inner_dcache_client_out_a_bits_param;
    logic [2:0] auto_inner_dcache_client_out_a_bits_size;
    logic [5:0] auto_inner_dcache_client_out_a_bits_source;
    logic [47:0] auto_inner_dcache_client_out_a_bits_address;
    logic [1:0] auto_inner_dcache_client_out_a_bits_user_alias;
    logic auto_inner_dcache_client_out_a_bits_user_memPageType_NC;
    logic auto_inner_dcache_client_out_a_bits_user_memBackType_MM;
    logic [43:0] auto_inner_dcache_client_out_a_bits_user_vaddr;
    logic [4:0] auto_inner_dcache_client_out_a_bits_user_reqSource;
    logic auto_inner_dcache_client_out_a_bits_user_needHint;
    logic auto_inner_dcache_client_out_a_bits_echo_isKeyword;
    logic [31:0] auto_inner_dcache_client_out_a_bits_mask;
    logic [255:0] auto_inner_dcache_client_out_a_bits_data;
    logic auto_inner_dcache_client_out_a_bits_corrupt;
    logic auto_inner_dcache_client_out_b_ready;
    logic auto_inner_dcache_client_out_b_valid;
    logic [2:0] auto_inner_dcache_client_out_b_bits_opcode;
    logic [1:0] auto_inner_dcache_client_out_b_bits_param;
    logic [2:0] auto_inner_dcache_client_out_b_bits_size;
    logic [5:0] auto_inner_dcache_client_out_b_bits_source;
    logic [47:0] auto_inner_dcache_client_out_b_bits_address;
    logic [31:0] auto_inner_dcache_client_out_b_bits_mask;
    logic [255:0] auto_inner_dcache_client_out_b_bits_data;
    logic auto_inner_dcache_client_out_b_bits_corrupt;
    logic auto_inner_dcache_client_out_c_ready;
    logic auto_inner_dcache_client_out_c_valid;
    logic [2:0] auto_inner_dcache_client_out_c_bits_opcode;
    logic [2:0] auto_inner_dcache_client_out_c_bits_param;
    logic [2:0] auto_inner_dcache_client_out_c_bits_size;
    logic [5:0] auto_inner_dcache_client_out_c_bits_source;
    logic [47:0] auto_inner_dcache_client_out_c_bits_address;
    logic [1:0] auto_inner_dcache_client_out_c_bits_user_alias;
    logic auto_inner_dcache_client_out_c_bits_user_memPageType_NC;
    logic auto_inner_dcache_client_out_c_bits_user_memBackType_MM;
    logic [43:0] auto_inner_dcache_client_out_c_bits_user_vaddr;
    logic [4:0] auto_inner_dcache_client_out_c_bits_user_reqSource;
    logic auto_inner_dcache_client_out_c_bits_user_needHint;
    logic auto_inner_dcache_client_out_c_bits_echo_isKeyword;
    logic [255:0] auto_inner_dcache_client_out_c_bits_data;
    logic auto_inner_dcache_client_out_c_bits_corrupt;
    logic auto_inner_dcache_client_out_d_ready;
    logic auto_inner_dcache_client_out_d_valid;
    logic [3:0] auto_inner_dcache_client_out_d_bits_opcode;
    logic [1:0] auto_inner_dcache_client_out_d_bits_param;
    logic [2:0] auto_inner_dcache_client_out_d_bits_size;
    logic [5:0] auto_inner_dcache_client_out_d_bits_source;
    logic [9:0] auto_inner_dcache_client_out_d_bits_sink;
    logic auto_inner_dcache_client_out_d_bits_denied;
    logic auto_inner_dcache_client_out_d_bits_echo_isKeyword;
    logic [255:0] auto_inner_dcache_client_out_d_bits_data;
    logic auto_inner_dcache_client_out_d_bits_corrupt;
    logic auto_inner_dcache_client_out_e_ready;
    logic auto_inner_dcache_client_out_e_valid;
    logic [9:0] auto_inner_dcache_client_out_e_bits_sink;

    dcache_agent_agent_xaction  mon_tr;
    while(1) begin
        @this.vif.mon_mp.mon_cb;
        auto_inner_dcache_client_out_a_ready = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_a_ready;
        auto_inner_dcache_client_out_a_valid = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_a_valid;
        auto_inner_dcache_client_out_a_bits_opcode = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_a_bits_opcode;
        auto_inner_dcache_client_out_a_bits_param = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_a_bits_param;
        auto_inner_dcache_client_out_a_bits_size = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_a_bits_size;
        auto_inner_dcache_client_out_a_bits_source = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_a_bits_source;
        auto_inner_dcache_client_out_a_bits_address = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_a_bits_address;
        auto_inner_dcache_client_out_a_bits_user_alias = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_a_bits_user_alias;
        auto_inner_dcache_client_out_a_bits_user_memPageType_NC = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_a_bits_user_memPageType_NC;
        auto_inner_dcache_client_out_a_bits_user_memBackType_MM = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_a_bits_user_memBackType_MM;
        auto_inner_dcache_client_out_a_bits_user_vaddr = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_a_bits_user_vaddr;
        auto_inner_dcache_client_out_a_bits_user_reqSource = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_a_bits_user_reqSource;
        auto_inner_dcache_client_out_a_bits_user_needHint = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_a_bits_user_needHint;
        auto_inner_dcache_client_out_a_bits_echo_isKeyword = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_a_bits_echo_isKeyword;
        auto_inner_dcache_client_out_a_bits_mask = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_a_bits_mask;
        auto_inner_dcache_client_out_a_bits_data = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_a_bits_data;
        auto_inner_dcache_client_out_a_bits_corrupt = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_a_bits_corrupt;
        auto_inner_dcache_client_out_b_ready = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_b_ready;
        auto_inner_dcache_client_out_b_valid = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_b_valid;
        auto_inner_dcache_client_out_b_bits_opcode = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_b_bits_opcode;
        auto_inner_dcache_client_out_b_bits_param = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_b_bits_param;
        auto_inner_dcache_client_out_b_bits_size = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_b_bits_size;
        auto_inner_dcache_client_out_b_bits_source = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_b_bits_source;
        auto_inner_dcache_client_out_b_bits_address = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_b_bits_address;
        auto_inner_dcache_client_out_b_bits_mask = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_b_bits_mask;
        auto_inner_dcache_client_out_b_bits_data = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_b_bits_data;
        auto_inner_dcache_client_out_b_bits_corrupt = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_b_bits_corrupt;
        auto_inner_dcache_client_out_c_ready = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_c_ready;
        auto_inner_dcache_client_out_c_valid = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_c_valid;
        auto_inner_dcache_client_out_c_bits_opcode = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_c_bits_opcode;
        auto_inner_dcache_client_out_c_bits_param = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_c_bits_param;
        auto_inner_dcache_client_out_c_bits_size = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_c_bits_size;
        auto_inner_dcache_client_out_c_bits_source = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_c_bits_source;
        auto_inner_dcache_client_out_c_bits_address = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_c_bits_address;
        auto_inner_dcache_client_out_c_bits_user_alias = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_c_bits_user_alias;
        auto_inner_dcache_client_out_c_bits_user_memPageType_NC = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_c_bits_user_memPageType_NC;
        auto_inner_dcache_client_out_c_bits_user_memBackType_MM = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_c_bits_user_memBackType_MM;
        auto_inner_dcache_client_out_c_bits_user_vaddr = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_c_bits_user_vaddr;
        auto_inner_dcache_client_out_c_bits_user_reqSource = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_c_bits_user_reqSource;
        auto_inner_dcache_client_out_c_bits_user_needHint = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_c_bits_user_needHint;
        auto_inner_dcache_client_out_c_bits_echo_isKeyword = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_c_bits_echo_isKeyword;
        auto_inner_dcache_client_out_c_bits_data = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_c_bits_data;
        auto_inner_dcache_client_out_c_bits_corrupt = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_c_bits_corrupt;
        auto_inner_dcache_client_out_d_ready = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_d_ready;
        auto_inner_dcache_client_out_d_valid = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_d_valid;
        auto_inner_dcache_client_out_d_bits_opcode = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_d_bits_opcode;
        auto_inner_dcache_client_out_d_bits_param = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_d_bits_param;
        auto_inner_dcache_client_out_d_bits_size = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_d_bits_size;
        auto_inner_dcache_client_out_d_bits_source = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_d_bits_source;
        auto_inner_dcache_client_out_d_bits_sink = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_d_bits_sink;
        auto_inner_dcache_client_out_d_bits_denied = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_d_bits_denied;
        auto_inner_dcache_client_out_d_bits_echo_isKeyword = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_d_bits_echo_isKeyword;
        auto_inner_dcache_client_out_d_bits_data = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_d_bits_data;
        auto_inner_dcache_client_out_d_bits_corrupt = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_d_bits_corrupt;
        auto_inner_dcache_client_out_e_ready = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_e_ready;
        auto_inner_dcache_client_out_e_valid = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_e_valid;
        auto_inner_dcache_client_out_e_bits_sink = this.vif.mon_mp.mon_cb.auto_inner_dcache_client_out_e_bits_sink;

        if(this.cfg.xz_sw==tcnt_dec_base::ON && this.vif.rst_n==1'b1 && memblock_sync_pkg::reset_backend_done==1'b1) begin
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_a_ready,auto_inner_dcache_client_out_a_ready,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_a_valid,auto_inner_dcache_client_out_a_valid,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_a_bits_opcode,auto_inner_dcache_client_out_a_bits_opcode,4);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_a_bits_param,auto_inner_dcache_client_out_a_bits_param,3);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_a_bits_size,auto_inner_dcache_client_out_a_bits_size,3);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_a_bits_source,auto_inner_dcache_client_out_a_bits_source,6);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_a_bits_address,auto_inner_dcache_client_out_a_bits_address,48);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_a_bits_user_alias,auto_inner_dcache_client_out_a_bits_user_alias,2);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_a_bits_user_memPageType_NC,auto_inner_dcache_client_out_a_bits_user_memPageType_NC,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_a_bits_user_memBackType_MM,auto_inner_dcache_client_out_a_bits_user_memBackType_MM,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_a_bits_user_vaddr,auto_inner_dcache_client_out_a_bits_user_vaddr,44);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_a_bits_user_reqSource,auto_inner_dcache_client_out_a_bits_user_reqSource,5);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_a_bits_user_needHint,auto_inner_dcache_client_out_a_bits_user_needHint,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_a_bits_echo_isKeyword,auto_inner_dcache_client_out_a_bits_echo_isKeyword,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_a_bits_mask,auto_inner_dcache_client_out_a_bits_mask,32);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_a_bits_data,auto_inner_dcache_client_out_a_bits_data,256);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_a_bits_corrupt,auto_inner_dcache_client_out_a_bits_corrupt,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_b_ready,auto_inner_dcache_client_out_b_ready,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_b_valid,auto_inner_dcache_client_out_b_valid,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_b_bits_opcode,auto_inner_dcache_client_out_b_bits_opcode,3);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_b_bits_param,auto_inner_dcache_client_out_b_bits_param,2);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_b_bits_size,auto_inner_dcache_client_out_b_bits_size,3);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_b_bits_source,auto_inner_dcache_client_out_b_bits_source,6);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_b_bits_address,auto_inner_dcache_client_out_b_bits_address,48);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_b_bits_mask,auto_inner_dcache_client_out_b_bits_mask,32);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_b_bits_data,auto_inner_dcache_client_out_b_bits_data,256);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_b_bits_corrupt,auto_inner_dcache_client_out_b_bits_corrupt,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_c_ready,auto_inner_dcache_client_out_c_ready,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_c_valid,auto_inner_dcache_client_out_c_valid,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_c_bits_opcode,auto_inner_dcache_client_out_c_bits_opcode,3);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_c_bits_param,auto_inner_dcache_client_out_c_bits_param,3);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_c_bits_size,auto_inner_dcache_client_out_c_bits_size,3);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_c_bits_source,auto_inner_dcache_client_out_c_bits_source,6);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_c_bits_address,auto_inner_dcache_client_out_c_bits_address,48);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_c_bits_user_alias,auto_inner_dcache_client_out_c_bits_user_alias,2);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_c_bits_user_memPageType_NC,auto_inner_dcache_client_out_c_bits_user_memPageType_NC,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_c_bits_user_memBackType_MM,auto_inner_dcache_client_out_c_bits_user_memBackType_MM,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_c_bits_user_vaddr,auto_inner_dcache_client_out_c_bits_user_vaddr,44);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_c_bits_user_reqSource,auto_inner_dcache_client_out_c_bits_user_reqSource,5);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_c_bits_user_needHint,auto_inner_dcache_client_out_c_bits_user_needHint,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_c_bits_echo_isKeyword,auto_inner_dcache_client_out_c_bits_echo_isKeyword,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_c_bits_data,auto_inner_dcache_client_out_c_bits_data,256);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_c_bits_corrupt,auto_inner_dcache_client_out_c_bits_corrupt,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_d_ready,auto_inner_dcache_client_out_d_ready,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_d_valid,auto_inner_dcache_client_out_d_valid,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_d_bits_opcode,auto_inner_dcache_client_out_d_bits_opcode,4);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_d_bits_param,auto_inner_dcache_client_out_d_bits_param,2);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_d_bits_size,auto_inner_dcache_client_out_d_bits_size,3);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_d_bits_source,auto_inner_dcache_client_out_d_bits_source,6);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_d_bits_sink,auto_inner_dcache_client_out_d_bits_sink,10);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_d_bits_denied,auto_inner_dcache_client_out_d_bits_denied,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_d_bits_echo_isKeyword,auto_inner_dcache_client_out_d_bits_echo_isKeyword,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_d_bits_data,auto_inner_dcache_client_out_d_bits_data,256);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_d_bits_corrupt,auto_inner_dcache_client_out_d_bits_corrupt,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_e_ready,auto_inner_dcache_client_out_e_ready,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_e_valid,auto_inner_dcache_client_out_e_valid,1);
            `TCNT_CHECK_SIG_XZ(auto_inner_dcache_client_out_e_bits_sink,auto_inner_dcache_client_out_e_bits_sink,10);

        end
        //if(xxxTODOxxx==1'b1) begin
        //    mon_tr = dcache_agent_agent_xaction::type_id::create("mon_tr");
        //    mon_tr.auto_inner_dcache_client_out_a_ready = auto_inner_dcache_client_out_a_ready;
        //    mon_tr.auto_inner_dcache_client_out_a_valid = auto_inner_dcache_client_out_a_valid;
        //    mon_tr.auto_inner_dcache_client_out_a_bits_opcode = auto_inner_dcache_client_out_a_bits_opcode;
        //    mon_tr.auto_inner_dcache_client_out_a_bits_param = auto_inner_dcache_client_out_a_bits_param;
        //    mon_tr.auto_inner_dcache_client_out_a_bits_size = auto_inner_dcache_client_out_a_bits_size;
        //    mon_tr.auto_inner_dcache_client_out_a_bits_source = auto_inner_dcache_client_out_a_bits_source;
        //    mon_tr.auto_inner_dcache_client_out_a_bits_address = auto_inner_dcache_client_out_a_bits_address;
        //    mon_tr.auto_inner_dcache_client_out_a_bits_user_alias = auto_inner_dcache_client_out_a_bits_user_alias;
        //    mon_tr.auto_inner_dcache_client_out_a_bits_user_memPageType_NC = auto_inner_dcache_client_out_a_bits_user_memPageType_NC;
        //    mon_tr.auto_inner_dcache_client_out_a_bits_user_memBackType_MM = auto_inner_dcache_client_out_a_bits_user_memBackType_MM;
        //    mon_tr.auto_inner_dcache_client_out_a_bits_user_vaddr = auto_inner_dcache_client_out_a_bits_user_vaddr;
        //    mon_tr.auto_inner_dcache_client_out_a_bits_user_reqSource = auto_inner_dcache_client_out_a_bits_user_reqSource;
        //    mon_tr.auto_inner_dcache_client_out_a_bits_user_needHint = auto_inner_dcache_client_out_a_bits_user_needHint;
        //    mon_tr.auto_inner_dcache_client_out_a_bits_echo_isKeyword = auto_inner_dcache_client_out_a_bits_echo_isKeyword;
        //    mon_tr.auto_inner_dcache_client_out_a_bits_mask = auto_inner_dcache_client_out_a_bits_mask;
        //    mon_tr.auto_inner_dcache_client_out_a_bits_data = auto_inner_dcache_client_out_a_bits_data;
        //    mon_tr.auto_inner_dcache_client_out_a_bits_corrupt = auto_inner_dcache_client_out_a_bits_corrupt;
        //    mon_tr.auto_inner_dcache_client_out_b_ready = auto_inner_dcache_client_out_b_ready;
        //    mon_tr.auto_inner_dcache_client_out_b_valid = auto_inner_dcache_client_out_b_valid;
        //    mon_tr.auto_inner_dcache_client_out_b_bits_opcode = auto_inner_dcache_client_out_b_bits_opcode;
        //    mon_tr.auto_inner_dcache_client_out_b_bits_param = auto_inner_dcache_client_out_b_bits_param;
        //    mon_tr.auto_inner_dcache_client_out_b_bits_size = auto_inner_dcache_client_out_b_bits_size;
        //    mon_tr.auto_inner_dcache_client_out_b_bits_source = auto_inner_dcache_client_out_b_bits_source;
        //    mon_tr.auto_inner_dcache_client_out_b_bits_address = auto_inner_dcache_client_out_b_bits_address;
        //    mon_tr.auto_inner_dcache_client_out_b_bits_mask = auto_inner_dcache_client_out_b_bits_mask;
        //    mon_tr.auto_inner_dcache_client_out_b_bits_data = auto_inner_dcache_client_out_b_bits_data;
        //    mon_tr.auto_inner_dcache_client_out_b_bits_corrupt = auto_inner_dcache_client_out_b_bits_corrupt;
        //    mon_tr.auto_inner_dcache_client_out_c_ready = auto_inner_dcache_client_out_c_ready;
        //    mon_tr.auto_inner_dcache_client_out_c_valid = auto_inner_dcache_client_out_c_valid;
        //    mon_tr.auto_inner_dcache_client_out_c_bits_opcode = auto_inner_dcache_client_out_c_bits_opcode;
        //    mon_tr.auto_inner_dcache_client_out_c_bits_param = auto_inner_dcache_client_out_c_bits_param;
        //    mon_tr.auto_inner_dcache_client_out_c_bits_size = auto_inner_dcache_client_out_c_bits_size;
        //    mon_tr.auto_inner_dcache_client_out_c_bits_source = auto_inner_dcache_client_out_c_bits_source;
        //    mon_tr.auto_inner_dcache_client_out_c_bits_address = auto_inner_dcache_client_out_c_bits_address;
        //    mon_tr.auto_inner_dcache_client_out_c_bits_user_alias = auto_inner_dcache_client_out_c_bits_user_alias;
        //    mon_tr.auto_inner_dcache_client_out_c_bits_user_memPageType_NC = auto_inner_dcache_client_out_c_bits_user_memPageType_NC;
        //    mon_tr.auto_inner_dcache_client_out_c_bits_user_memBackType_MM = auto_inner_dcache_client_out_c_bits_user_memBackType_MM;
        //    mon_tr.auto_inner_dcache_client_out_c_bits_user_vaddr = auto_inner_dcache_client_out_c_bits_user_vaddr;
        //    mon_tr.auto_inner_dcache_client_out_c_bits_user_reqSource = auto_inner_dcache_client_out_c_bits_user_reqSource;
        //    mon_tr.auto_inner_dcache_client_out_c_bits_user_needHint = auto_inner_dcache_client_out_c_bits_user_needHint;
        //    mon_tr.auto_inner_dcache_client_out_c_bits_echo_isKeyword = auto_inner_dcache_client_out_c_bits_echo_isKeyword;
        //    mon_tr.auto_inner_dcache_client_out_c_bits_data = auto_inner_dcache_client_out_c_bits_data;
        //    mon_tr.auto_inner_dcache_client_out_c_bits_corrupt = auto_inner_dcache_client_out_c_bits_corrupt;
        //    mon_tr.auto_inner_dcache_client_out_d_ready = auto_inner_dcache_client_out_d_ready;
        //    mon_tr.auto_inner_dcache_client_out_d_valid = auto_inner_dcache_client_out_d_valid;
        //    mon_tr.auto_inner_dcache_client_out_d_bits_opcode = auto_inner_dcache_client_out_d_bits_opcode;
        //    mon_tr.auto_inner_dcache_client_out_d_bits_param = auto_inner_dcache_client_out_d_bits_param;
        //    mon_tr.auto_inner_dcache_client_out_d_bits_size = auto_inner_dcache_client_out_d_bits_size;
        //    mon_tr.auto_inner_dcache_client_out_d_bits_source = auto_inner_dcache_client_out_d_bits_source;
        //    mon_tr.auto_inner_dcache_client_out_d_bits_sink = auto_inner_dcache_client_out_d_bits_sink;
        //    mon_tr.auto_inner_dcache_client_out_d_bits_denied = auto_inner_dcache_client_out_d_bits_denied;
        //    mon_tr.auto_inner_dcache_client_out_d_bits_echo_isKeyword = auto_inner_dcache_client_out_d_bits_echo_isKeyword;
        //    mon_tr.auto_inner_dcache_client_out_d_bits_data = auto_inner_dcache_client_out_d_bits_data;
        //    mon_tr.auto_inner_dcache_client_out_d_bits_corrupt = auto_inner_dcache_client_out_d_bits_corrupt;
        //    mon_tr.auto_inner_dcache_client_out_e_ready = auto_inner_dcache_client_out_e_ready;
        //    mon_tr.auto_inner_dcache_client_out_e_valid = auto_inner_dcache_client_out_e_valid;
        //    mon_tr.auto_inner_dcache_client_out_e_bits_sink = auto_inner_dcache_client_out_e_bits_sink;

        //    mon_tr.channel_id = this.cfg.channel_id;
        //    mon_tr.unpack();
        //    this.mon_item_port.write(mon_tr);
        //end
    end
endtask:mon_data

`endif
