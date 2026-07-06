//=========================================================
//File name    : redirect_agent_agent_monitor.sv
//Author       : OpenAI_Codex
//Module name  : redirect_agent_agent_monitor
//Discribution : redirect_agent_agent_monitor : monitor
//Date         : 2026-04-12
//=========================================================
`ifndef REDIRECT_AGENT_AGENT_MONITOR__SV
`define REDIRECT_AGENT_AGENT_MONITOR__SV

class redirect_agent_agent_monitor  extends tcnt_monitor_base#(virtual redirect_agent_agent_interface,redirect_agent_agent_cfg,redirect_agent_agent_xaction);

    `uvm_component_utils(redirect_agent_agent_monitor)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern task run_phase(uvm_phase phase);
    extern task mon_data();
endclass:redirect_agent_agent_monitor

function redirect_agent_agent_monitor::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void redirect_agent_agent_monitor::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task redirect_agent_agent_monitor::run_phase(uvm_phase phase);
    super.run_phase(phase);
    this.mon_data();
endtask:run_phase

task redirect_agent_agent_monitor::mon_data();

    logic io_redirect_valid            ;
    logic io_redirect_bits_level       ;
    logic io_redirect_bits_robIdx_flag ;
    logic [8:0] io_redirect_bits_robIdx_value;

    redirect_agent_agent_xaction  mon_tr;
    while(1) begin
        @this.vif.mon_mp.mon_cb;
        io_redirect_valid = this.vif.mon_mp.mon_cb.io_redirect_valid;
        io_redirect_bits_level = this.vif.mon_mp.mon_cb.io_redirect_bits_level;
        io_redirect_bits_robIdx_flag = this.vif.mon_mp.mon_cb.io_redirect_bits_robIdx_flag;
        io_redirect_bits_robIdx_value = this.vif.mon_mp.mon_cb.io_redirect_bits_robIdx_value;

        if(this.cfg.xz_sw==tcnt_dec_base::ON && this.vif.rst_n==1'b1 && memblock_sync_pkg::reset_backend_done==1'b1) begin
            `TCNT_CHECK_SIG_XZ(io_redirect_valid,io_redirect_valid,1);
            `TCNT_CHECK_SIG_XZ(io_redirect_bits_level,io_redirect_bits_level,1);
            `TCNT_CHECK_SIG_XZ(io_redirect_bits_robIdx_flag,io_redirect_bits_robIdx_flag,1);
            `TCNT_CHECK_SIG_XZ(io_redirect_bits_robIdx_value,io_redirect_bits_robIdx_value,9);

        end
        // io_redirect_* is a DUT input driven by redirect sequences. Do not
        // feed monitor samples back into dispatch recovery; recovery is sourced
        // from DUT output events such as io_mem_to_ooo_ctrl.memoryViolation.
        //if(xxxTODOxxx==1'b1) begin
        //    mon_tr = redirect_agent_agent_xaction::type_id::create("mon_tr");
        //    mon_tr.io_redirect_valid = io_redirect_valid;
        //    mon_tr.io_redirect_bits_level = io_redirect_bits_level;
        //    mon_tr.io_redirect_bits_robIdx_flag = io_redirect_bits_robIdx_flag;
        //    mon_tr.io_redirect_bits_robIdx_value = io_redirect_bits_robIdx_value;

        //    mon_tr.channel_id = this.cfg.channel_id;
        //    mon_tr.unpack();
        //    this.mon_item_port.write(mon_tr);
        //end
    end
endtask:mon_data

`endif
