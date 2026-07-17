//=========================================================
//File name    : lsqenq_agent_agent_driver.sv
//Author       : OpenAI_Codex
//Module name  : lsqenq_agent_agent_driver
//Discribution : lsqenq_agent_agent_driver : driver
//Date         : 2026-04-12
//=========================================================
`ifndef LSQENQ_AGENT_AGENT_DRIVER__SV
`define LSQENQ_AGENT_AGENT_DRIVER__SV

class lsqenq_agent_agent_driver  extends tcnt_driver_base#(virtual lsqenq_agent_agent_interface,lsqenq_agent_agent_cfg,lsqenq_agent_agent_xaction);

    `uvm_component_utils(lsqenq_agent_agent_driver)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern virtual task reset_phase(uvm_phase phase);
    extern task main_phase(uvm_phase phase);
    extern task send_pkt(lsqenq_agent_agent_xaction tr);
    extern task wait_lsq_can_accept(lsqenq_agent_agent_xaction tr);
    extern task sample_lsqenq_resp(lsqenq_agent_agent_xaction tr);
    extern task drive_idle(tcnt_dec_base::drv_mode_e drv_mode);
    extern function bit has_active_request(lsqenq_agent_agent_xaction tr);
    extern function void validate_v2_scalar_item(lsqenq_agent_agent_xaction tr);
endclass:lsqenq_agent_agent_driver

function lsqenq_agent_agent_driver::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void lsqenq_agent_agent_driver::build_phase(uvm_phase phase);
    super.build_phase(phase);
    if (cfg.drv_sw == tcnt_dec_base::ON && cfg.drv_mode != tcnt_dec_base::DRV_0) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("V2 LSQ enqueue active driver requires DRV_0 idle mode, got %0d",
                             cfg.drv_mode))
    end
endfunction:build_phase

task lsqenq_agent_agent_driver::reset_phase(uvm_phase phase);

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

task lsqenq_agent_agent_driver::main_phase(uvm_phase phase);
    super.main_phase(phase);
    //while(1) begin
    if(this.cfg.sqr_sw==tcnt_dec_base::ON && this.cfg.drv_sw==tcnt_dec_base::ON) begin
        while(1) begin
            @this.vif.drv_mp.drv_cb;
            req = null;
            seq_item_port.try_next_item(req);
            if(req!=null) begin
                bit active_request;

                if (req.pre_pkt_gap != 0 || req.post_pkt_gap != 0) begin
                    `uvm_fatal(get_type_name(),
                               $sformatf("V2 LSQ streaming requires pre/post gap 0, got %0d/%0d",
                                         req.pre_pkt_gap,
                                         req.post_pkt_gap))
                end
                req.memblock_dispatch_request_launched = 1'b0;
                req.memblock_dispatch_aborted_by_redirect = 1'b0;
                active_request = has_active_request(req);
                if (active_request &&
                    (memblock_sync_pkg::dispatch_flush_in_progress ||
                     memblock_sync_pkg::dispatch_flush_epoch != req.memblock_dispatch_flush_epoch)) begin
                    req.memblock_dispatch_aborted_by_redirect = 1'b1;
                    this.drive_idle(this.cfg.drv_mode);
                end else begin
                    this.send_pkt(req);
                    req.memblock_dispatch_request_launched = active_request;
                end
                seq_item_port.item_done();
            end
            else begin
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

task lsqenq_agent_agent_driver::send_pkt(lsqenq_agent_agent_xaction tr);
    validate_v2_scalar_item(tr);
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_0 <= tr.io_ooo_to_mem_enqLsq_needAlloc_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_1 <= tr.io_ooo_to_mem_enqLsq_needAlloc_1;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_2 <= tr.io_ooo_to_mem_enqLsq_needAlloc_2;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_3 <= tr.io_ooo_to_mem_enqLsq_needAlloc_3;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_4 <= tr.io_ooo_to_mem_enqLsq_needAlloc_4;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_5 <= tr.io_ooo_to_mem_enqLsq_needAlloc_5;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_valid <= tr.io_ooo_to_mem_enqLsq_req_0_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_fuType <= tr.io_ooo_to_mem_enqLsq_req_0_bits_fuType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_uopIdx <= tr.io_ooo_to_mem_enqLsq_req_0_bits_uopIdx;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag <= tr.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value <= tr.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag <= tr.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value <= tr.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag <= tr.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value <= tr.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_numLsElem <= tr.io_ooo_to_mem_enqLsq_req_0_bits_numLsElem;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_valid <= tr.io_ooo_to_mem_enqLsq_req_1_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_fuType <= tr.io_ooo_to_mem_enqLsq_req_1_bits_fuType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_uopIdx <= tr.io_ooo_to_mem_enqLsq_req_1_bits_uopIdx;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag <= tr.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value <= tr.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag <= tr.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value <= tr.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag <= tr.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value <= tr.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_numLsElem <= tr.io_ooo_to_mem_enqLsq_req_1_bits_numLsElem;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_valid <= tr.io_ooo_to_mem_enqLsq_req_2_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_fuType <= tr.io_ooo_to_mem_enqLsq_req_2_bits_fuType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_uopIdx <= tr.io_ooo_to_mem_enqLsq_req_2_bits_uopIdx;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag <= tr.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value <= tr.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag <= tr.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value <= tr.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag <= tr.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value <= tr.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_numLsElem <= tr.io_ooo_to_mem_enqLsq_req_2_bits_numLsElem;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_valid <= tr.io_ooo_to_mem_enqLsq_req_3_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_fuType <= tr.io_ooo_to_mem_enqLsq_req_3_bits_fuType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_uopIdx <= tr.io_ooo_to_mem_enqLsq_req_3_bits_uopIdx;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag <= tr.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value <= tr.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag <= tr.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value <= tr.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag <= tr.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value <= tr.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_numLsElem <= tr.io_ooo_to_mem_enqLsq_req_3_bits_numLsElem;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_valid <= tr.io_ooo_to_mem_enqLsq_req_4_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_fuType <= tr.io_ooo_to_mem_enqLsq_req_4_bits_fuType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_uopIdx <= tr.io_ooo_to_mem_enqLsq_req_4_bits_uopIdx;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag <= tr.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value <= tr.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag <= tr.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value <= tr.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag <= tr.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value <= tr.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_numLsElem <= tr.io_ooo_to_mem_enqLsq_req_4_bits_numLsElem;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_valid <= tr.io_ooo_to_mem_enqLsq_req_5_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_fuType <= tr.io_ooo_to_mem_enqLsq_req_5_bits_fuType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_uopIdx <= tr.io_ooo_to_mem_enqLsq_req_5_bits_uopIdx;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag <= tr.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value <= tr.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag <= tr.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value <= tr.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag <= tr.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value <= tr.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_numLsElem <= tr.io_ooo_to_mem_enqLsq_req_5_bits_numLsElem;

    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_0 <= tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_1 <= tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_1;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_10 <= tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_10;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_11 <= tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_11;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_12 <= tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_12;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_13 <= tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_13;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_14 <= tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_14;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_15 <= tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_15;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_16 <= tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_16;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_17 <= tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_17;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_18 <= tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_18;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_19 <= tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_19;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_2 <= tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_2;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_20 <= tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_20;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_21 <= tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_21;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_22 <= tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_22;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_23 <= tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_23;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_3 <= tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_3;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_4 <= tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_4;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_5 <= tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_5;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_6 <= tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_6;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_7 <= tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_7;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_8 <= tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_8;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_9 <= tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_9;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_flushPipe <= tr.io_ooo_to_mem_enqLsq_req_0_bits_flushPipe;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_fuOpType <= tr.io_ooo_to_mem_enqLsq_req_0_bits_fuOpType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_lastUop <= tr.io_ooo_to_mem_enqLsq_req_0_bits_lastUop;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_trigger <= tr.io_ooo_to_mem_enqLsq_req_0_bits_trigger;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_0 <= tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_1 <= tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_1;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_10 <= tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_10;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_11 <= tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_11;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_12 <= tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_12;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_13 <= tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_13;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_14 <= tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_14;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_15 <= tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_15;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_16 <= tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_16;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_17 <= tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_17;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_18 <= tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_18;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_19 <= tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_19;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_2 <= tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_2;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_20 <= tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_20;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_21 <= tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_21;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_22 <= tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_22;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_23 <= tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_23;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_3 <= tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_3;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_4 <= tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_4;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_5 <= tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_5;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_6 <= tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_6;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_7 <= tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_7;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_8 <= tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_8;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_9 <= tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_9;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_flushPipe <= tr.io_ooo_to_mem_enqLsq_req_1_bits_flushPipe;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_fuOpType <= tr.io_ooo_to_mem_enqLsq_req_1_bits_fuOpType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_lastUop <= tr.io_ooo_to_mem_enqLsq_req_1_bits_lastUop;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_trigger <= tr.io_ooo_to_mem_enqLsq_req_1_bits_trigger;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_0 <= tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_1 <= tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_1;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_10 <= tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_10;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_11 <= tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_11;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_12 <= tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_12;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_13 <= tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_13;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_14 <= tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_14;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_15 <= tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_15;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_16 <= tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_16;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_17 <= tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_17;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_18 <= tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_18;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_19 <= tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_19;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_2 <= tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_2;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_20 <= tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_20;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_21 <= tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_21;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_22 <= tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_22;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_23 <= tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_23;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_3 <= tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_3;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_4 <= tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_4;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_5 <= tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_5;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_6 <= tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_6;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_7 <= tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_7;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_8 <= tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_8;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_9 <= tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_9;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_flushPipe <= tr.io_ooo_to_mem_enqLsq_req_2_bits_flushPipe;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_fuOpType <= tr.io_ooo_to_mem_enqLsq_req_2_bits_fuOpType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_lastUop <= tr.io_ooo_to_mem_enqLsq_req_2_bits_lastUop;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_trigger <= tr.io_ooo_to_mem_enqLsq_req_2_bits_trigger;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_0 <= tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_1 <= tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_1;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_10 <= tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_10;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_11 <= tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_11;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_12 <= tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_12;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_13 <= tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_13;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_14 <= tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_14;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_15 <= tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_15;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_16 <= tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_16;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_17 <= tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_17;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_18 <= tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_18;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_19 <= tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_19;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_2 <= tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_2;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_20 <= tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_20;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_21 <= tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_21;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_22 <= tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_22;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_23 <= tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_23;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_3 <= tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_3;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_4 <= tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_4;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_5 <= tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_5;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_6 <= tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_6;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_7 <= tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_7;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_8 <= tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_8;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_9 <= tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_9;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_flushPipe <= tr.io_ooo_to_mem_enqLsq_req_3_bits_flushPipe;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_fuOpType <= tr.io_ooo_to_mem_enqLsq_req_3_bits_fuOpType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_lastUop <= tr.io_ooo_to_mem_enqLsq_req_3_bits_lastUop;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_trigger <= tr.io_ooo_to_mem_enqLsq_req_3_bits_trigger;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_0 <= tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_1 <= tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_1;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_10 <= tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_10;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_11 <= tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_11;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_12 <= tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_12;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_13 <= tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_13;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_14 <= tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_14;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_15 <= tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_15;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_16 <= tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_16;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_17 <= tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_17;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_18 <= tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_18;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_19 <= tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_19;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_2 <= tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_2;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_20 <= tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_20;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_21 <= tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_21;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_22 <= tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_22;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_23 <= tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_23;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_3 <= tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_3;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_4 <= tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_4;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_5 <= tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_5;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_6 <= tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_6;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_7 <= tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_7;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_8 <= tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_8;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_9 <= tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_9;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_flushPipe <= tr.io_ooo_to_mem_enqLsq_req_4_bits_flushPipe;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_fuOpType <= tr.io_ooo_to_mem_enqLsq_req_4_bits_fuOpType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_lastUop <= tr.io_ooo_to_mem_enqLsq_req_4_bits_lastUop;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_trigger <= tr.io_ooo_to_mem_enqLsq_req_4_bits_trigger;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_0 <= tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_1 <= tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_1;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_10 <= tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_10;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_11 <= tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_11;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_12 <= tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_12;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_13 <= tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_13;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_14 <= tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_14;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_15 <= tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_15;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_16 <= tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_16;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_17 <= tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_17;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_18 <= tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_18;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_19 <= tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_19;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_2 <= tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_2;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_20 <= tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_20;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_21 <= tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_21;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_22 <= tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_22;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_23 <= tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_23;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_3 <= tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_3;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_4 <= tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_4;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_5 <= tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_5;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_6 <= tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_6;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_7 <= tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_7;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_8 <= tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_8;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_9 <= tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_9;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_flushPipe <= tr.io_ooo_to_mem_enqLsq_req_5_bits_flushPipe;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_fuOpType <= tr.io_ooo_to_mem_enqLsq_req_5_bits_fuOpType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_lastUop <= tr.io_ooo_to_mem_enqLsq_req_5_bits_lastUop;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_trigger <= tr.io_ooo_to_mem_enqLsq_req_5_bits_trigger;

endtask:send_pkt

task lsqenq_agent_agent_driver::wait_lsq_can_accept(lsqenq_agent_agent_xaction tr);
    int unsigned wait_count;
    bit          sent_once;

    wait_count = 0;
    sent_once = 1'b0;
    tr.memblock_dispatch_aborted_by_redirect = 1'b0;
    forever begin
        @this.vif.drv_mp.drv_cb;
        if (memblock_sync_pkg::dispatch_flush_in_progress ||
            memblock_sync_pkg::dispatch_flush_epoch != tr.memblock_dispatch_flush_epoch) begin
            tr.memblock_dispatch_aborted_by_redirect = 1'b1;
            this.drive_idle(this.cfg.drv_mode);
            return;
        end
        if (tr.memblock_dispatch_ready_timeout != 0 &&
            wait_count >= tr.memblock_dispatch_ready_timeout) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("lsqenq canAccept timeout after %0d cycles",
                                 wait_count))
        end
        this.send_pkt(tr);
        sent_once = 1'b1;
        wait_count++;
    end
endtask:wait_lsq_can_accept

task lsqenq_agent_agent_driver::sample_lsqenq_resp(lsqenq_agent_agent_xaction tr);
endtask:sample_lsqenq_resp

function bit lsqenq_agent_agent_driver::has_active_request(lsqenq_agent_agent_xaction tr);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "has_active_request got null transaction")
    end
    return tr.io_ooo_to_mem_enqLsq_req_0_valid ||
           tr.io_ooo_to_mem_enqLsq_req_1_valid ||
           tr.io_ooo_to_mem_enqLsq_req_2_valid ||
           tr.io_ooo_to_mem_enqLsq_req_3_valid ||
           tr.io_ooo_to_mem_enqLsq_req_4_valid ||
           tr.io_ooo_to_mem_enqLsq_req_5_valid;
endfunction:has_active_request

function void lsqenq_agent_agent_driver::validate_v2_scalar_item(lsqenq_agent_agent_xaction tr);
    bit valid;
    bit [1:0] need_alloc;
    bit [`MEMBLOCK_DUT_FUTYPE_W-1:0] fu_type;
    bit [`MEMBLOCK_DUT_UOP_IDX_W-1:0] uop_idx;
    bit rob_idx_flag;
    bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] rob_idx_value;
    bit lq_idx_flag;
    bit [`MEMBLOCK_DUT_LQ_VALUE_W-1:0] lq_idx_value;
    bit sq_idx_flag;
    bit [`MEMBLOCK_DUT_SQ_VALUE_W-1:0] sq_idx_value;
    bit [`MEMBLOCK_DUT_NUM_LS_ELEM_W-1:0] num_ls_elem;
    bit [23:0] exception_vec;
    bit [3:0] trigger;
    bit [8:0] fu_op_type;
    bit flush_pipe;
    bit last_uop;
    bit [`MEMBLOCK_DUT_FUTYPE_W-1:0] load_fu_type;
    bit [`MEMBLOCK_DUT_FUTYPE_W-1:0] store_fu_type;
    int unsigned load_count;
    int unsigned store_count;

    if (tr == null) begin
        `uvm_fatal(get_type_name(), "validate_v2_scalar_item got null transaction")
    end
    load_fu_type = (1 << `MEMBLOCK_DUT_FUTYPE_LDU_BIT);
    store_fu_type = (1 << `MEMBLOCK_DUT_FUTYPE_STU_BIT);
    load_count = 0;
    store_count = 0;

    for (int unsigned slot = 0; slot < `MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM; slot++) begin
        case (slot)
            0: begin
                valid = tr.io_ooo_to_mem_enqLsq_req_0_valid;
                need_alloc = tr.io_ooo_to_mem_enqLsq_needAlloc_0;
                fu_type = tr.io_ooo_to_mem_enqLsq_req_0_bits_fuType;
                uop_idx = tr.io_ooo_to_mem_enqLsq_req_0_bits_uopIdx;
                rob_idx_flag = tr.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag;
                rob_idx_value = tr.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value;
                lq_idx_flag = tr.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag;
                lq_idx_value = tr.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value;
                sq_idx_flag = tr.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag;
                sq_idx_value = tr.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value;
                num_ls_elem = tr.io_ooo_to_mem_enqLsq_req_0_bits_numLsElem;
                exception_vec = {tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_23, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_22, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_21, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_20, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_19, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_18, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_17, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_16, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_15, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_14, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_13, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_12, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_11, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_10, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_9, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_8, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_7, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_6, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_5, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_4, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_3, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_2, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_1, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_0};
                trigger = tr.io_ooo_to_mem_enqLsq_req_0_bits_trigger;
                fu_op_type = tr.io_ooo_to_mem_enqLsq_req_0_bits_fuOpType;
                flush_pipe = tr.io_ooo_to_mem_enqLsq_req_0_bits_flushPipe;
                last_uop = tr.io_ooo_to_mem_enqLsq_req_0_bits_lastUop;
            end
            1: begin
                valid = tr.io_ooo_to_mem_enqLsq_req_1_valid;
                need_alloc = tr.io_ooo_to_mem_enqLsq_needAlloc_1;
                fu_type = tr.io_ooo_to_mem_enqLsq_req_1_bits_fuType;
                uop_idx = tr.io_ooo_to_mem_enqLsq_req_1_bits_uopIdx;
                rob_idx_flag = tr.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag;
                rob_idx_value = tr.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value;
                lq_idx_flag = tr.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag;
                lq_idx_value = tr.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value;
                sq_idx_flag = tr.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag;
                sq_idx_value = tr.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value;
                num_ls_elem = tr.io_ooo_to_mem_enqLsq_req_1_bits_numLsElem;
                exception_vec = {tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_23, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_22, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_21, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_20, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_19, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_18, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_17, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_16, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_15, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_14, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_13, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_12, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_11, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_10, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_9, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_8, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_7, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_6, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_5, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_4, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_3, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_2, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_1, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_0};
                trigger = tr.io_ooo_to_mem_enqLsq_req_1_bits_trigger;
                fu_op_type = tr.io_ooo_to_mem_enqLsq_req_1_bits_fuOpType;
                flush_pipe = tr.io_ooo_to_mem_enqLsq_req_1_bits_flushPipe;
                last_uop = tr.io_ooo_to_mem_enqLsq_req_1_bits_lastUop;
            end
            2: begin
                valid = tr.io_ooo_to_mem_enqLsq_req_2_valid;
                need_alloc = tr.io_ooo_to_mem_enqLsq_needAlloc_2;
                fu_type = tr.io_ooo_to_mem_enqLsq_req_2_bits_fuType;
                uop_idx = tr.io_ooo_to_mem_enqLsq_req_2_bits_uopIdx;
                rob_idx_flag = tr.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag;
                rob_idx_value = tr.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value;
                lq_idx_flag = tr.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag;
                lq_idx_value = tr.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value;
                sq_idx_flag = tr.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag;
                sq_idx_value = tr.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value;
                num_ls_elem = tr.io_ooo_to_mem_enqLsq_req_2_bits_numLsElem;
                exception_vec = {tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_23, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_22, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_21, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_20, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_19, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_18, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_17, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_16, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_15, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_14, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_13, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_12, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_11, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_10, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_9, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_8, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_7, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_6, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_5, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_4, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_3, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_2, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_1, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_0};
                trigger = tr.io_ooo_to_mem_enqLsq_req_2_bits_trigger;
                fu_op_type = tr.io_ooo_to_mem_enqLsq_req_2_bits_fuOpType;
                flush_pipe = tr.io_ooo_to_mem_enqLsq_req_2_bits_flushPipe;
                last_uop = tr.io_ooo_to_mem_enqLsq_req_2_bits_lastUop;
            end
            3: begin
                valid = tr.io_ooo_to_mem_enqLsq_req_3_valid;
                need_alloc = tr.io_ooo_to_mem_enqLsq_needAlloc_3;
                fu_type = tr.io_ooo_to_mem_enqLsq_req_3_bits_fuType;
                uop_idx = tr.io_ooo_to_mem_enqLsq_req_3_bits_uopIdx;
                rob_idx_flag = tr.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag;
                rob_idx_value = tr.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value;
                lq_idx_flag = tr.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag;
                lq_idx_value = tr.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value;
                sq_idx_flag = tr.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag;
                sq_idx_value = tr.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value;
                num_ls_elem = tr.io_ooo_to_mem_enqLsq_req_3_bits_numLsElem;
                exception_vec = {tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_23, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_22, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_21, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_20, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_19, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_18, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_17, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_16, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_15, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_14, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_13, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_12, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_11, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_10, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_9, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_8, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_7, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_6, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_5, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_4, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_3, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_2, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_1, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_0};
                trigger = tr.io_ooo_to_mem_enqLsq_req_3_bits_trigger;
                fu_op_type = tr.io_ooo_to_mem_enqLsq_req_3_bits_fuOpType;
                flush_pipe = tr.io_ooo_to_mem_enqLsq_req_3_bits_flushPipe;
                last_uop = tr.io_ooo_to_mem_enqLsq_req_3_bits_lastUop;
            end
            4: begin
                valid = tr.io_ooo_to_mem_enqLsq_req_4_valid;
                need_alloc = tr.io_ooo_to_mem_enqLsq_needAlloc_4;
                fu_type = tr.io_ooo_to_mem_enqLsq_req_4_bits_fuType;
                uop_idx = tr.io_ooo_to_mem_enqLsq_req_4_bits_uopIdx;
                rob_idx_flag = tr.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag;
                rob_idx_value = tr.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value;
                lq_idx_flag = tr.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag;
                lq_idx_value = tr.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value;
                sq_idx_flag = tr.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag;
                sq_idx_value = tr.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value;
                num_ls_elem = tr.io_ooo_to_mem_enqLsq_req_4_bits_numLsElem;
                exception_vec = {tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_23, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_22, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_21, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_20, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_19, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_18, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_17, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_16, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_15, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_14, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_13, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_12, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_11, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_10, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_9, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_8, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_7, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_6, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_5, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_4, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_3, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_2, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_1, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_0};
                trigger = tr.io_ooo_to_mem_enqLsq_req_4_bits_trigger;
                fu_op_type = tr.io_ooo_to_mem_enqLsq_req_4_bits_fuOpType;
                flush_pipe = tr.io_ooo_to_mem_enqLsq_req_4_bits_flushPipe;
                last_uop = tr.io_ooo_to_mem_enqLsq_req_4_bits_lastUop;
            end
            5: begin
                valid = tr.io_ooo_to_mem_enqLsq_req_5_valid;
                need_alloc = tr.io_ooo_to_mem_enqLsq_needAlloc_5;
                fu_type = tr.io_ooo_to_mem_enqLsq_req_5_bits_fuType;
                uop_idx = tr.io_ooo_to_mem_enqLsq_req_5_bits_uopIdx;
                rob_idx_flag = tr.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag;
                rob_idx_value = tr.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value;
                lq_idx_flag = tr.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag;
                lq_idx_value = tr.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value;
                sq_idx_flag = tr.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag;
                sq_idx_value = tr.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value;
                num_ls_elem = tr.io_ooo_to_mem_enqLsq_req_5_bits_numLsElem;
                exception_vec = {tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_23, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_22, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_21, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_20, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_19, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_18, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_17, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_16, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_15, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_14, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_13, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_12, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_11, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_10, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_9, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_8, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_7, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_6, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_5, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_4, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_3, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_2, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_1, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_0};
                trigger = tr.io_ooo_to_mem_enqLsq_req_5_bits_trigger;
                fu_op_type = tr.io_ooo_to_mem_enqLsq_req_5_bits_fuOpType;
                flush_pipe = tr.io_ooo_to_mem_enqLsq_req_5_bits_flushPipe;
                last_uop = tr.io_ooo_to_mem_enqLsq_req_5_bits_lastUop;
            end
            default: begin
                `uvm_fatal(get_type_name(), $sformatf("unsupported V2 LSQ enqueue slot=%0d", slot))
            end
        endcase

        if (!valid) begin
            if (need_alloc != 2'b00 || fu_type != '0 || uop_idx != '0 ||
                rob_idx_flag || rob_idx_value != '0 ||
                lq_idx_flag || lq_idx_value != '0 ||
                sq_idx_flag || sq_idx_value != '0 || num_ls_elem != '0 ||
                exception_vec != '0 || trigger != '0 || fu_op_type != '0 ||
                flush_pipe || last_uop) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("inactive slot=%0d must have zero qualifier and payload", slot))
            end
            continue;
        end
        case (need_alloc)
            2'b01: begin
                if (fu_type != load_fu_type) begin
                    `uvm_fatal(get_type_name(), $sformatf("load slot=%0d has FuType=0x%0h", slot, fu_type))
                end
                if (!lsqenq_agent_agent_xaction::is_supported_v2_load_or_prefetch_fuoptype(fu_op_type)) begin
                    `uvm_fatal(get_type_name(),
                               $sformatf("load/prefetch slot=%0d has unsupported fuOpType=0x%0h",
                                         slot, fu_op_type))
                end
                load_count++;
                if (load_count > `MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH) begin
                    `uvm_fatal(get_type_name(),
                               $sformatf("V2 LSQ batch load count=%0d exceeds width=%0d",
                                         load_count, `MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH))
                end
            end
            2'b10: begin
                if (fu_type != store_fu_type) begin
                    `uvm_fatal(get_type_name(), $sformatf("store slot=%0d has FuType=0x%0h", slot, fu_type))
                end
                if (!lsqenq_agent_agent_xaction::is_supported_v2_store_fuoptype(fu_op_type)) begin
                    `uvm_fatal(get_type_name(),
                               $sformatf("store slot=%0d has unsupported fuOpType=0x%0h",
                                         slot, fu_op_type))
                end
                store_count++;
                if (store_count > `MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH) begin
                    `uvm_fatal(get_type_name(),
                               $sformatf("V2 LSQ batch store count=%0d exceeds width=%0d",
                                         store_count, `MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH))
                end
            end
            default: begin
                `uvm_fatal(get_type_name(),
                           $sformatf("active slot=%0d has illegal needAlloc=%0b", slot, need_alloc))
            end
        endcase
        if (rob_idx_value >= `MEMBLOCK_DUT_ROB_SIZE ||
            lq_idx_value >= `MEMBLOCK_DUT_LQ_SIZE ||
            sq_idx_value >= `MEMBLOCK_DUT_SQ_SIZE) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("slot=%0d key value exceeds resource size: rob=%0d lq=%0d sq=%0d",
                                 slot, rob_idx_value, lq_idx_value, sq_idx_value))
        end
        if (uop_idx != 0 || num_ls_elem != 1 || !last_uop ||
            exception_vec != '0 || trigger != '0 || flush_pipe) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("slot=%0d violates scalar LSQ contract: uopIdx=%0d numLsElem=%0d lastUop=%0b exceptionVec=0x%0h trigger=0x%0h flushPipe=%0b",
                                 slot, uop_idx, num_ls_elem, last_uop,
                                 exception_vec, trigger, flush_pipe))
        end
    end
endfunction:validate_v2_scalar_item

task lsqenq_agent_agent_driver::drive_idle(tcnt_dec_base::drv_mode_e drv_mode);

    if(drv_mode==tcnt_dec_base::DRV_0) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_1 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_10 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_11 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_12 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_13 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_14 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_15 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_16 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_17 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_18 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_19 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_20 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_21 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_22 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_23 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_3 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_4 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_5 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_6 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_7 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_8 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_9 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_flushPipe <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_lastUop <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_trigger <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_1 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_10 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_11 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_12 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_13 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_14 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_15 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_16 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_17 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_18 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_19 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_20 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_21 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_22 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_23 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_3 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_4 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_5 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_6 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_7 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_8 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_9 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_flushPipe <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_lastUop <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_trigger <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_1 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_10 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_11 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_12 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_13 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_14 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_15 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_16 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_17 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_18 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_19 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_20 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_21 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_22 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_23 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_3 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_4 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_5 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_6 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_7 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_8 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_9 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_flushPipe <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_lastUop <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_trigger <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_1 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_10 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_11 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_12 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_13 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_14 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_15 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_16 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_17 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_18 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_19 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_20 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_21 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_22 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_23 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_3 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_4 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_5 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_6 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_7 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_8 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_9 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_flushPipe <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_lastUop <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_trigger <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_1 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_10 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_11 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_12 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_13 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_14 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_15 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_16 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_17 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_18 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_19 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_20 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_21 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_22 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_23 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_3 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_4 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_5 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_6 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_7 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_8 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_9 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_flushPipe <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_lastUop <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_trigger <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_1 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_10 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_11 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_12 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_13 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_14 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_15 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_16 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_17 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_18 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_19 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_20 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_21 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_22 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_23 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_3 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_4 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_5 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_6 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_7 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_8 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_9 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_flushPipe <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_lastUop <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_trigger <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_1 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_3 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_4 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_5 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_uopIdx <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_numLsElem <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_uopIdx <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_numLsElem <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_uopIdx <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_numLsElem <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_uopIdx <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_numLsElem <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_uopIdx <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_numLsElem <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_uopIdx <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_numLsElem <= '0;

    end
    else if(drv_mode==tcnt_dec_base::DRV_1) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_0 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_1 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_2 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_3 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_4 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_5 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_valid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_fuType <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_uopIdx <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_numLsElem <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_valid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_fuType <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_uopIdx <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_numLsElem <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_valid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_fuType <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_uopIdx <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_numLsElem <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_valid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_fuType <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_uopIdx <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_numLsElem <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_valid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_fuType <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_uopIdx <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_numLsElem <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_valid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_fuType <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_uopIdx <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_numLsElem <= '1;

    end
    else if(drv_mode==tcnt_dec_base::DRV_X) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_0 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_1 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_2 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_3 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_4 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_5 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_valid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_fuType <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_uopIdx <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_numLsElem <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_valid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_fuType <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_uopIdx <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_numLsElem <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_valid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_fuType <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_uopIdx <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_numLsElem <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_valid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_fuType <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_uopIdx <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_numLsElem <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_valid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_fuType <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_uopIdx <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_numLsElem <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_valid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_fuType <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_uopIdx <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_numLsElem <= 'x;

    end
    else if(drv_mode==tcnt_dec_base::DRV_RAND) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_0 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_1 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_2 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_3 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_4 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_5 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_valid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_fuType <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_uopIdx <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_numLsElem <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_valid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_fuType <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_uopIdx <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_numLsElem <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_valid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_fuType <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_uopIdx <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_numLsElem <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_valid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_fuType <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_uopIdx <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_numLsElem <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_valid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_fuType <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_uopIdx <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_numLsElem <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_valid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_fuType <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_uopIdx <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_numLsElem <= $urandom;

    end
    else if(drv_mode==tcnt_dec_base::DRV_LST) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_1 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_3 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_4 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_5 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_uopIdx <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_0_bits_numLsElem <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_uopIdx <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_1_bits_numLsElem <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_uopIdx <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_2_bits_numLsElem <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_uopIdx <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_3_bits_numLsElem <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_uopIdx <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_4_bits_numLsElem <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_uopIdx <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_5_bits_numLsElem <= '0;

    end

endtask:drive_idle

`endif
