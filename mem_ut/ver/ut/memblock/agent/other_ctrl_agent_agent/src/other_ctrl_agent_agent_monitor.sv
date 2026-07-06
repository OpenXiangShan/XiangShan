//=========================================================
//File name    : other_ctrl_agent_agent_monitor.sv
//Author       : OpenAI_Codex
//Module name  : other_ctrl_agent_agent_monitor
//Discribution : other_ctrl_agent_agent_monitor : monitor
//Date         : 2026-04-12
//=========================================================
`ifndef OTHER_CTRL_AGENT_AGENT_MONITOR__SV
`define OTHER_CTRL_AGENT_AGENT_MONITOR__SV

class other_ctrl_agent_agent_monitor  extends tcnt_monitor_base#(virtual other_ctrl_agent_agent_interface,other_ctrl_agent_agent_cfg,other_ctrl_agent_agent_xaction);

    `uvm_component_utils(other_ctrl_agent_agent_monitor)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern task run_phase(uvm_phase phase);
    extern task mon_data();
endclass:other_ctrl_agent_agent_monitor

function other_ctrl_agent_agent_monitor::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void other_ctrl_agent_agent_monitor::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task other_ctrl_agent_agent_monitor::run_phase(uvm_phase phase);
    super.run_phase(phase);
    this.mon_data();
endtask:run_phase

task other_ctrl_agent_agent_monitor::mon_data();

    logic [5:0] io_hartId              ;
    logic io_dcacheError_ecc_error_valid;
    logic [47:0] io_dcacheError_ecc_error_bits;
    logic io_uncacheError_ecc_error_valid;
    logic [47:0] io_uncacheError_ecc_error_bits;
    logic io_memInfo_sqFull            ;
    logic io_memInfo_lqFull            ;
    logic io_memInfo_dcacheMSHRFull    ;
    logic [5:0] io_inner_hartId        ;
    logic [47:0] io_inner_reset_vector ;
    logic [47:0] io_outer_reset_vector ;
    logic io_outer_cpu_wfi             ;
    logic io_outer_l2_flush_en         ;
    logic io_outer_power_down_en       ;
    logic io_outer_cpu_critical_error  ;
    logic io_outer_msi_ack             ;
    logic io_inner_beu_errors_icache_ecc_error_valid;
    logic [47:0] io_inner_beu_errors_icache_ecc_error_bits;
    logic io_outer_beu_errors_icache_ecc_error_valid;
    logic [47:0] io_outer_beu_errors_icache_ecc_error_bits;
    logic io_reset_backend             ;

    other_ctrl_agent_agent_xaction  mon_tr;
    while(1) begin
        @this.vif.mon_mp.mon_cb;
        io_hartId = this.vif.mon_mp.mon_cb.io_hartId;
        io_dcacheError_ecc_error_valid = this.vif.mon_mp.mon_cb.io_dcacheError_ecc_error_valid;
        io_dcacheError_ecc_error_bits = this.vif.mon_mp.mon_cb.io_dcacheError_ecc_error_bits;
        io_uncacheError_ecc_error_valid = this.vif.mon_mp.mon_cb.io_uncacheError_ecc_error_valid;
        io_uncacheError_ecc_error_bits = this.vif.mon_mp.mon_cb.io_uncacheError_ecc_error_bits;
        io_memInfo_sqFull = this.vif.mon_mp.mon_cb.io_memInfo_sqFull;
        io_memInfo_lqFull = this.vif.mon_mp.mon_cb.io_memInfo_lqFull;
        io_memInfo_dcacheMSHRFull = this.vif.mon_mp.mon_cb.io_memInfo_dcacheMSHRFull;
        io_inner_hartId = this.vif.mon_mp.mon_cb.io_inner_hartId;
        io_inner_reset_vector = this.vif.mon_mp.mon_cb.io_inner_reset_vector;
        io_outer_reset_vector = this.vif.mon_mp.mon_cb.io_outer_reset_vector;
        io_outer_cpu_wfi = this.vif.mon_mp.mon_cb.io_outer_cpu_wfi;
        io_outer_l2_flush_en = this.vif.mon_mp.mon_cb.io_outer_l2_flush_en;
        io_outer_power_down_en = this.vif.mon_mp.mon_cb.io_outer_power_down_en;
        io_outer_cpu_critical_error = this.vif.mon_mp.mon_cb.io_outer_cpu_critical_error;
        io_outer_msi_ack = this.vif.mon_mp.mon_cb.io_outer_msi_ack;
        io_inner_beu_errors_icache_ecc_error_valid = this.vif.mon_mp.mon_cb.io_inner_beu_errors_icache_ecc_error_valid;
        io_inner_beu_errors_icache_ecc_error_bits = this.vif.mon_mp.mon_cb.io_inner_beu_errors_icache_ecc_error_bits;
        io_outer_beu_errors_icache_ecc_error_valid = this.vif.mon_mp.mon_cb.io_outer_beu_errors_icache_ecc_error_valid;
        io_outer_beu_errors_icache_ecc_error_bits = this.vif.mon_mp.mon_cb.io_outer_beu_errors_icache_ecc_error_bits;
        io_reset_backend = this.vif.mon_mp.mon_cb.io_reset_backend;

        if(this.cfg.xz_sw==tcnt_dec_base::ON && this.vif.rst_n==1'b1 && memblock_sync_pkg::reset_backend_done==1'b1) begin
            `TCNT_CHECK_SIG_XZ(io_hartId,io_hartId,6);
            `TCNT_CHECK_SIG_XZ(io_dcacheError_ecc_error_valid,io_dcacheError_ecc_error_valid,1);
            `TCNT_CHECK_SIG_XZ(io_dcacheError_ecc_error_bits,io_dcacheError_ecc_error_bits,48);
            `TCNT_CHECK_SIG_XZ(io_uncacheError_ecc_error_valid,io_uncacheError_ecc_error_valid,1);
            `TCNT_CHECK_SIG_XZ(io_uncacheError_ecc_error_bits,io_uncacheError_ecc_error_bits,48);
            `TCNT_CHECK_SIG_XZ(io_memInfo_sqFull,io_memInfo_sqFull,1);
            `TCNT_CHECK_SIG_XZ(io_memInfo_lqFull,io_memInfo_lqFull,1);
            `TCNT_CHECK_SIG_XZ(io_memInfo_dcacheMSHRFull,io_memInfo_dcacheMSHRFull,1);
            `TCNT_CHECK_SIG_XZ(io_inner_hartId,io_inner_hartId,6);
            `TCNT_CHECK_SIG_XZ(io_inner_reset_vector,io_inner_reset_vector,48);
            `TCNT_CHECK_SIG_XZ(io_outer_reset_vector,io_outer_reset_vector,48);
            `TCNT_CHECK_SIG_XZ(io_outer_cpu_wfi,io_outer_cpu_wfi,1);
            `TCNT_CHECK_SIG_XZ(io_outer_l2_flush_en,io_outer_l2_flush_en,1);
            `TCNT_CHECK_SIG_XZ(io_outer_power_down_en,io_outer_power_down_en,1);
            `TCNT_CHECK_SIG_XZ(io_outer_cpu_critical_error,io_outer_cpu_critical_error,1);
            `TCNT_CHECK_SIG_XZ(io_outer_msi_ack,io_outer_msi_ack,1);
            `TCNT_CHECK_SIG_XZ(io_inner_beu_errors_icache_ecc_error_valid,io_inner_beu_errors_icache_ecc_error_valid,1);
            `TCNT_CHECK_SIG_XZ(io_inner_beu_errors_icache_ecc_error_bits,io_inner_beu_errors_icache_ecc_error_bits,48);
            `TCNT_CHECK_SIG_XZ(io_outer_beu_errors_icache_ecc_error_valid,io_outer_beu_errors_icache_ecc_error_valid,1);
            `TCNT_CHECK_SIG_XZ(io_outer_beu_errors_icache_ecc_error_bits,io_outer_beu_errors_icache_ecc_error_bits,48);
            `TCNT_CHECK_SIG_XZ(io_reset_backend,io_reset_backend,1);

        end
        //if(xxxTODOxxx==1'b1) begin
        //    mon_tr = other_ctrl_agent_agent_xaction::type_id::create("mon_tr");
        //    mon_tr.io_hartId = io_hartId;
        //    mon_tr.io_dcacheError_ecc_error_valid = io_dcacheError_ecc_error_valid;
        //    mon_tr.io_dcacheError_ecc_error_bits = io_dcacheError_ecc_error_bits;
        //    mon_tr.io_uncacheError_ecc_error_valid = io_uncacheError_ecc_error_valid;
        //    mon_tr.io_uncacheError_ecc_error_bits = io_uncacheError_ecc_error_bits;
        //    mon_tr.io_memInfo_sqFull = io_memInfo_sqFull;
        //    mon_tr.io_memInfo_lqFull = io_memInfo_lqFull;
        //    mon_tr.io_memInfo_dcacheMSHRFull = io_memInfo_dcacheMSHRFull;
        //    mon_tr.io_inner_hartId = io_inner_hartId;
        //    mon_tr.io_inner_reset_vector = io_inner_reset_vector;
        //    mon_tr.io_outer_reset_vector = io_outer_reset_vector;
        //    mon_tr.io_outer_cpu_wfi = io_outer_cpu_wfi;
        //    mon_tr.io_outer_l2_flush_en = io_outer_l2_flush_en;
        //    mon_tr.io_outer_power_down_en = io_outer_power_down_en;
        //    mon_tr.io_outer_cpu_critical_error = io_outer_cpu_critical_error;
        //    mon_tr.io_outer_msi_ack = io_outer_msi_ack;
        //    mon_tr.io_inner_beu_errors_icache_ecc_error_valid = io_inner_beu_errors_icache_ecc_error_valid;
        //    mon_tr.io_inner_beu_errors_icache_ecc_error_bits = io_inner_beu_errors_icache_ecc_error_bits;
        //    mon_tr.io_outer_beu_errors_icache_ecc_error_valid = io_outer_beu_errors_icache_ecc_error_valid;
        //    mon_tr.io_outer_beu_errors_icache_ecc_error_bits = io_outer_beu_errors_icache_ecc_error_bits;
        //    mon_tr.io_reset_backend = io_reset_backend;

        //    mon_tr.channel_id = this.cfg.channel_id;
        //    mon_tr.unpack();
        //    this.mon_item_port.write(mon_tr);
        //end
    end
endtask:mon_data

`endif
