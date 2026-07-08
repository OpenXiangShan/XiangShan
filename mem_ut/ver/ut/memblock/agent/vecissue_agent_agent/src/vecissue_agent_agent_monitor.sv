//=========================================================
//File name    : vecissue_agent_agent_monitor.sv
//Author       : OpenAI_Codex
//Module name  : vecissue_agent_agent_monitor
//Discribution : vecissue_agent_agent_monitor : monitor
//Date         : 2026-04-12
//=========================================================
`ifndef VECISSUE_AGENT_AGENT_MONITOR__SV
`define VECISSUE_AGENT_AGENT_MONITOR__SV

class vecissue_agent_agent_monitor  extends tcnt_monitor_base#(virtual vecissue_agent_agent_interface,vecissue_agent_agent_cfg,vecissue_agent_agent_xaction);

    `uvm_component_utils(vecissue_agent_agent_monitor)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern task run_phase(uvm_phase phase);
    extern task mon_data();
endclass:vecissue_agent_agent_monitor

function vecissue_agent_agent_monitor::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void vecissue_agent_agent_monitor::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task vecissue_agent_agent_monitor::run_phase(uvm_phase phase);
    super.run_phase(phase);
    this.mon_data();
endtask:run_phase

task vecissue_agent_agent_monitor::mon_data();


    logic io_ooo_to_mem_isStoreException;
    logic [4:0] io_ooo_to_mem_issueVldu_0_bits_flowNum;
    logic io_ooo_to_mem_issueVldu_0_bits_isVecPartReplay;
    logic [127:0] io_ooo_to_mem_issueVldu_0_bits_src_0;
    logic [127:0] io_ooo_to_mem_issueVldu_0_bits_src_1;
    logic [127:0] io_ooo_to_mem_issueVldu_0_bits_src_2;
    logic [127:0] io_ooo_to_mem_issueVldu_0_bits_src_3;
    logic [127:0] io_ooo_to_mem_issueVldu_0_bits_src_4;
    logic [3:0] io_ooo_to_mem_issueVldu_0_bits_uop_ftqOffset;
    logic io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_flag;
    logic [5:0] io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_value;
    logic [8:0] io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType;
    logic [34:0] io_ooo_to_mem_issueVldu_0_bits_uop_fuType;
    logic io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_flag;
    logic [6:0] io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_value;
    logic [7:0] io_ooo_to_mem_issueVldu_0_bits_uop_pdest;
    logic io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_flag;
    logic [7:0] io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_value;
    logic io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_flag;
    logic [5:0] io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_value;
    logic io_ooo_to_mem_issueVldu_0_bits_uop_v0Wen;
    logic io_ooo_to_mem_issueVldu_0_bits_uop_vecWen;
    logic io_ooo_to_mem_issueVldu_0_bits_uop_vlWen;
    logic io_ooo_to_mem_issueVldu_0_bits_uop_vpu_isVleff;
    logic io_ooo_to_mem_issueVldu_0_bits_uop_vpu_lastUop;
    logic [2:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_nf;
    logic [1:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_veew;
    logic [2:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vlmul;
    logic io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vm;
    logic io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vma;
    logic [127:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vmask;
    logic [1:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vsew;
    logic [7:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vstart;
    logic io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vta;
    logic [6:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vuopIdx;
    logic [15:0] io_ooo_to_mem_issueVldu_0_bits_vecReplayMask;
    logic [3:0] io_ooo_to_mem_issueVldu_0_bits_vecReplayMbIdx;
    logic io_ooo_to_mem_issueVldu_0_ready;
    logic io_ooo_to_mem_issueVldu_0_valid;
    logic [4:0] io_ooo_to_mem_issueVldu_1_bits_flowNum;
    logic io_ooo_to_mem_issueVldu_1_bits_isVecPartReplay;
    logic [127:0] io_ooo_to_mem_issueVldu_1_bits_src_0;
    logic [127:0] io_ooo_to_mem_issueVldu_1_bits_src_1;
    logic [127:0] io_ooo_to_mem_issueVldu_1_bits_src_2;
    logic [127:0] io_ooo_to_mem_issueVldu_1_bits_src_3;
    logic [127:0] io_ooo_to_mem_issueVldu_1_bits_src_4;
    logic [3:0] io_ooo_to_mem_issueVldu_1_bits_uop_ftqOffset;
    logic io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_flag;
    logic [5:0] io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_value;
    logic [8:0] io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType;
    logic io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_flag;
    logic [6:0] io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_value;
    logic [7:0] io_ooo_to_mem_issueVldu_1_bits_uop_pdest;
    logic io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_flag;
    logic [7:0] io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_value;
    logic io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_flag;
    logic [5:0] io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_value;
    logic io_ooo_to_mem_issueVldu_1_bits_uop_v0Wen;
    logic io_ooo_to_mem_issueVldu_1_bits_uop_vecWen;
    logic io_ooo_to_mem_issueVldu_1_bits_uop_vlWen;
    logic io_ooo_to_mem_issueVldu_1_bits_uop_vpu_isVleff;
    logic io_ooo_to_mem_issueVldu_1_bits_uop_vpu_lastUop;
    logic [2:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_nf;
    logic [1:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_veew;
    logic [2:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vlmul;
    logic io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vm;
    logic io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vma;
    logic [127:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vmask;
    logic [1:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vsew;
    logic [7:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vstart;
    logic io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vta;
    logic [6:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vuopIdx;
    logic [15:0] io_ooo_to_mem_issueVldu_1_bits_vecReplayMask;
    logic [3:0] io_ooo_to_mem_issueVldu_1_bits_vecReplayMbIdx;
    logic io_ooo_to_mem_issueVldu_1_ready;
    logic io_ooo_to_mem_issueVldu_1_valid;
    vecissue_agent_agent_xaction  mon_tr;
    while(1) begin
        @this.vif.mon_mp.mon_cb;

        io_ooo_to_mem_isStoreException = this.vif.mon_mp.mon_cb.io_ooo_to_mem_isStoreException;
        io_ooo_to_mem_issueVldu_0_bits_flowNum = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_flowNum;
        io_ooo_to_mem_issueVldu_0_bits_isVecPartReplay = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_isVecPartReplay;
        io_ooo_to_mem_issueVldu_0_bits_src_0 = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_src_0;
        io_ooo_to_mem_issueVldu_0_bits_src_1 = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_src_1;
        io_ooo_to_mem_issueVldu_0_bits_src_2 = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_src_2;
        io_ooo_to_mem_issueVldu_0_bits_src_3 = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_src_3;
        io_ooo_to_mem_issueVldu_0_bits_src_4 = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_src_4;
        io_ooo_to_mem_issueVldu_0_bits_uop_ftqOffset = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_ftqOffset;
        io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_flag;
        io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_value;
        io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType;
        io_ooo_to_mem_issueVldu_0_bits_uop_fuType = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_fuType;
        io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_flag;
        io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_value;
        io_ooo_to_mem_issueVldu_0_bits_uop_pdest = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_pdest;
        io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_flag;
        io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_value;
        io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_flag;
        io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_value;
        io_ooo_to_mem_issueVldu_0_bits_uop_v0Wen = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_v0Wen;
        io_ooo_to_mem_issueVldu_0_bits_uop_vecWen = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vecWen;
        io_ooo_to_mem_issueVldu_0_bits_uop_vlWen = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vlWen;
        io_ooo_to_mem_issueVldu_0_bits_uop_vpu_isVleff = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_isVleff;
        io_ooo_to_mem_issueVldu_0_bits_uop_vpu_lastUop = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_lastUop;
        io_ooo_to_mem_issueVldu_0_bits_uop_vpu_nf = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_nf;
        io_ooo_to_mem_issueVldu_0_bits_uop_vpu_veew = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_veew;
        io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vlmul = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vlmul;
        io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vm = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vm;
        io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vma = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vma;
        io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vmask = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vmask;
        io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vsew = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vsew;
        io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vstart = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vstart;
        io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vta = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vta;
        io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vuopIdx = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vuopIdx;
        io_ooo_to_mem_issueVldu_0_bits_vecReplayMask = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_vecReplayMask;
        io_ooo_to_mem_issueVldu_0_bits_vecReplayMbIdx = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_bits_vecReplayMbIdx;
        io_ooo_to_mem_issueVldu_0_ready = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_ready;
        io_ooo_to_mem_issueVldu_0_valid = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_0_valid;
        io_ooo_to_mem_issueVldu_1_bits_flowNum = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_flowNum;
        io_ooo_to_mem_issueVldu_1_bits_isVecPartReplay = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_isVecPartReplay;
        io_ooo_to_mem_issueVldu_1_bits_src_0 = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_src_0;
        io_ooo_to_mem_issueVldu_1_bits_src_1 = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_src_1;
        io_ooo_to_mem_issueVldu_1_bits_src_2 = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_src_2;
        io_ooo_to_mem_issueVldu_1_bits_src_3 = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_src_3;
        io_ooo_to_mem_issueVldu_1_bits_src_4 = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_src_4;
        io_ooo_to_mem_issueVldu_1_bits_uop_ftqOffset = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_uop_ftqOffset;
        io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_flag;
        io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_value;
        io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType;
        io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_flag;
        io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_value;
        io_ooo_to_mem_issueVldu_1_bits_uop_pdest = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_uop_pdest;
        io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_flag;
        io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_value;
        io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_flag = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_flag;
        io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_value = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_value;
        io_ooo_to_mem_issueVldu_1_bits_uop_v0Wen = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_uop_v0Wen;
        io_ooo_to_mem_issueVldu_1_bits_uop_vecWen = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vecWen;
        io_ooo_to_mem_issueVldu_1_bits_uop_vlWen = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vlWen;
        io_ooo_to_mem_issueVldu_1_bits_uop_vpu_isVleff = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_isVleff;
        io_ooo_to_mem_issueVldu_1_bits_uop_vpu_lastUop = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_lastUop;
        io_ooo_to_mem_issueVldu_1_bits_uop_vpu_nf = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_nf;
        io_ooo_to_mem_issueVldu_1_bits_uop_vpu_veew = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_veew;
        io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vlmul = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vlmul;
        io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vm = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vm;
        io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vma = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vma;
        io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vmask = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vmask;
        io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vsew = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vsew;
        io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vstart = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vstart;
        io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vta = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vta;
        io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vuopIdx = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vuopIdx;
        io_ooo_to_mem_issueVldu_1_bits_vecReplayMask = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_vecReplayMask;
        io_ooo_to_mem_issueVldu_1_bits_vecReplayMbIdx = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_bits_vecReplayMbIdx;
        io_ooo_to_mem_issueVldu_1_ready = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_ready;
        io_ooo_to_mem_issueVldu_1_valid = this.vif.mon_mp.mon_cb.io_ooo_to_mem_issueVldu_1_valid;

        if(this.cfg.xz_sw==tcnt_dec_base::ON && this.vif.rst_n==1'b1 && memblock_sync_pkg::reset_backend_done==1'b1) begin

        end
        //if(xxxTODOxxx==1'b1) begin
        //    mon_tr = vecissue_agent_agent_xaction::type_id::create("mon_tr");

        //    mon_tr.channel_id = this.cfg.channel_id;
        //    mon_tr.unpack();
        //    this.mon_item_port.write(mon_tr);
        //end
    end
endtask:mon_data

`endif
