//=========================================================
//File name    : vecissue_agent_agent_driver.sv
//Author       : OpenAI_Codex
//Module name  : vecissue_agent_agent_driver
//Discribution : vecissue_agent_agent_driver : driver
//Date         : 2026-04-12
//=========================================================
`ifndef VECISSUE_AGENT_AGENT_DRIVER__SV
`define VECISSUE_AGENT_AGENT_DRIVER__SV

class vecissue_agent_agent_driver  extends tcnt_driver_base#(virtual vecissue_agent_agent_interface,vecissue_agent_agent_cfg,vecissue_agent_agent_xaction);

    `uvm_component_utils(vecissue_agent_agent_driver)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern virtual task reset_phase(uvm_phase phase);
    extern task main_phase(uvm_phase phase);
    extern task send_pkt(vecissue_agent_agent_xaction tr);
    extern task drive_idle(tcnt_dec_base::drv_mode_e drv_mode);
endclass:vecissue_agent_agent_driver

function vecissue_agent_agent_driver::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void vecissue_agent_agent_driver::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task vecissue_agent_agent_driver::reset_phase(uvm_phase phase);

    super.reset_phase(phase);
    phase.raise_objection(this);

    repeat(2) begin
        @this.vif.drv_mp.drv_cb;
        this.drive_idle(this.cfg.drv_mode);
    end
    wait(vif.rst_n == 1'b1);
    wait(memblock_sync_pkg::reset_backend_done == 1'b1);
    repeat(20) begin
        @this.vif.drv_mp.drv_cb;
        this.drive_idle(this.cfg.drv_mode);
    end

    phase.drop_objection(this);
endtask:reset_phase

task vecissue_agent_agent_driver::main_phase(uvm_phase phase);
    super.main_phase(phase);
    //while(1) begin
    if(this.cfg.sqr_sw==tcnt_dec_base::ON && this.cfg.drv_sw==tcnt_dec_base::ON) begin
        while(1) begin
            seq_item_port.try_next_item(req);
            if(req!=null) begin
                repeat(req.pre_pkt_gap) begin
                    @this.vif.drv_mp.drv_cb;
                    this.drive_idle(this.cfg.drv_mode);
                end
                @this.vif.drv_mp.drv_cb;
                this.send_pkt(req);
                repeat(req.post_pkt_gap) begin
                    @this.vif.drv_mp.drv_cb;
                    this.drive_idle(this.cfg.drv_mode);
                end
                seq_item_port.item_done();
            end
            else begin
                @this.vif.drv_mp.drv_cb;
                this.drive_idle(this.cfg.drv_mode);
            end
        end
    end
    else if (this.cfg.drv_sw==tcnt_dec_base::ON) begin
        while(1) begin
            @this.vif.drv_mp.drv_cb;
            `uvm_fatal(get_type_name(), $sformatf("sqr_sw==OFF & drv_sw==ON, please give a driver send task!"))
            //send task
        end
    end
endtask:main_phase

task vecissue_agent_agent_driver::send_pkt(vecissue_agent_agent_xaction tr);

    vif.drv_mp.drv_cb.io_ooo_to_mem_isStoreException <= tr.io_ooo_to_mem_isStoreException;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_flowNum <= tr.io_ooo_to_mem_issueVldu_0_bits_flowNum;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_isVecPartReplay <= tr.io_ooo_to_mem_issueVldu_0_bits_isVecPartReplay;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_src_0 <= tr.io_ooo_to_mem_issueVldu_0_bits_src_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_src_1 <= tr.io_ooo_to_mem_issueVldu_0_bits_src_1;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_src_2 <= tr.io_ooo_to_mem_issueVldu_0_bits_src_2;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_src_3 <= tr.io_ooo_to_mem_issueVldu_0_bits_src_3;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_src_4 <= tr.io_ooo_to_mem_issueVldu_0_bits_src_4;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_ftqOffset <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_ftqOffset;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_flag <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_value <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_fuType <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_fuType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_flag <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_value <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_pdest <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_pdest;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_flag <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_value <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_flag <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_value <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_v0Wen <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_v0Wen;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vecWen <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_vecWen;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vlWen <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_vlWen;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_isVleff <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_isVleff;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_lastUop <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_lastUop;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_nf <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_nf;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_veew <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_veew;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vlmul <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vlmul;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vm <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vm;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vma <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vma;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vmask <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vmask;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vsew <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vsew;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vstart <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vstart;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vta <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vta;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vuopIdx <= tr.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vuopIdx;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_vecReplayMask <= tr.io_ooo_to_mem_issueVldu_0_bits_vecReplayMask;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_vecReplayMbIdx <= tr.io_ooo_to_mem_issueVldu_0_bits_vecReplayMbIdx;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_valid <= tr.io_ooo_to_mem_issueVldu_0_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_flowNum <= tr.io_ooo_to_mem_issueVldu_1_bits_flowNum;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_isVecPartReplay <= tr.io_ooo_to_mem_issueVldu_1_bits_isVecPartReplay;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_src_0 <= tr.io_ooo_to_mem_issueVldu_1_bits_src_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_src_1 <= tr.io_ooo_to_mem_issueVldu_1_bits_src_1;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_src_2 <= tr.io_ooo_to_mem_issueVldu_1_bits_src_2;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_src_3 <= tr.io_ooo_to_mem_issueVldu_1_bits_src_3;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_src_4 <= tr.io_ooo_to_mem_issueVldu_1_bits_src_4;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_ftqOffset <= tr.io_ooo_to_mem_issueVldu_1_bits_uop_ftqOffset;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_flag <= tr.io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_value <= tr.io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType <= tr.io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_flag <= tr.io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_value <= tr.io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_pdest <= tr.io_ooo_to_mem_issueVldu_1_bits_uop_pdest;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_flag <= tr.io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_value <= tr.io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_flag <= tr.io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_value <= tr.io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_v0Wen <= tr.io_ooo_to_mem_issueVldu_1_bits_uop_v0Wen;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vecWen <= tr.io_ooo_to_mem_issueVldu_1_bits_uop_vecWen;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vlWen <= tr.io_ooo_to_mem_issueVldu_1_bits_uop_vlWen;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_isVleff <= tr.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_isVleff;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_lastUop <= tr.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_lastUop;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_nf <= tr.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_nf;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_veew <= tr.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_veew;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vlmul <= tr.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vlmul;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vm <= tr.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vm;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vma <= tr.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vma;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vmask <= tr.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vmask;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vsew <= tr.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vsew;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vstart <= tr.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vstart;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vta <= tr.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vta;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vuopIdx <= tr.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vuopIdx;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_vecReplayMask <= tr.io_ooo_to_mem_issueVldu_1_bits_vecReplayMask;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_vecReplayMbIdx <= tr.io_ooo_to_mem_issueVldu_1_bits_vecReplayMbIdx;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_valid <= tr.io_ooo_to_mem_issueVldu_1_valid;

endtask:send_pkt

task vecissue_agent_agent_driver::drive_idle(tcnt_dec_base::drv_mode_e drv_mode);

    if(drv_mode==tcnt_dec_base::DRV_0) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_isStoreException <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_flowNum <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_isVecPartReplay <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_src_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_src_1 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_src_2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_src_3 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_src_4 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_ftqOffset <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_ftqPtr_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_pdest <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_v0Wen <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vecWen <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vlWen <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_isVleff <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_lastUop <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_nf <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_veew <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vlmul <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vm <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vma <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vmask <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vsew <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vstart <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vta <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vuopIdx <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_vecReplayMask <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_bits_vecReplayMbIdx <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_0_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_flowNum <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_isVecPartReplay <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_src_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_src_1 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_src_2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_src_3 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_src_4 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_ftqOffset <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_ftqPtr_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_pdest <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_v0Wen <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vecWen <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vlWen <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_isVleff <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_lastUop <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_nf <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_veew <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vlmul <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vm <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vma <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vmask <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vsew <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vstart <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vta <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vuopIdx <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_vecReplayMask <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_bits_vecReplayMbIdx <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueVldu_1_valid <= '0;

    end
    else if(drv_mode==tcnt_dec_base::DRV_1) begin

    end
    else if(drv_mode==tcnt_dec_base::DRV_X) begin

    end
    else if(drv_mode==tcnt_dec_base::DRV_RAND) begin

    end
    else if(drv_mode==tcnt_dec_base::DRV_LST) begin

    end

endtask:drive_idle

`endif
