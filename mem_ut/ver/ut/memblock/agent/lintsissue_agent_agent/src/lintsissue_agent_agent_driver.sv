//=========================================================
//File name    : lintsissue_agent_agent_driver.sv
//Author       : OpenAI_Codex
//Module name  : lintsissue_agent_agent_driver
//Discribution : lintsissue_agent_agent_driver : driver
//Date         : 2026-04-12
//=========================================================
`ifndef LINTSISSUE_AGENT_AGENT_DRIVER__SV
`define LINTSISSUE_AGENT_AGENT_DRIVER__SV

class lintsissue_agent_agent_driver  extends tcnt_driver_base#(virtual lintsissue_agent_agent_interface,lintsissue_agent_agent_cfg,lintsissue_agent_agent_xaction);

    `uvm_component_utils(lintsissue_agent_agent_driver)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern virtual task reset_phase(uvm_phase phase);
    extern task main_phase(uvm_phase phase);
    extern task send_pkt(lintsissue_agent_agent_xaction tr);
    extern task wait_dispatch_issue_ready(lintsissue_agent_agent_xaction tr);
    extern task drive_dispatch_issue_one_cycle(lintsissue_agent_agent_xaction tr);
    extern function bit has_dispatch_issue_pending(lintsissue_agent_agent_xaction tr);
    extern function void clear_dispatch_issue_ports(lintsissue_agent_agent_xaction tr);
    extern function void clear_ready_dispatch_issue_ports(lintsissue_agent_agent_xaction tr);
    extern function void record_dispatch_issue_fire(input int unsigned port_idx,
                                                    lintsissue_agent_agent_xaction tr);
    extern function void report_dispatch_issue_fire(input int unsigned port_idx,
                                                    lintsissue_agent_agent_xaction tr);
    extern function void report_dispatch_issue_timeout(lintsissue_agent_agent_xaction tr,
                                                       int unsigned wait_cycles);
    extern function void report_dispatch_hdl_bit(input string path);
    extern function void report_dispatch_hdl_value(input string path);
    extern task drive_idle(tcnt_dec_base::drv_mode_e drv_mode);
endclass:lintsissue_agent_agent_driver

function lintsissue_agent_agent_driver::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void lintsissue_agent_agent_driver::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task lintsissue_agent_agent_driver::reset_phase(uvm_phase phase);

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

task lintsissue_agent_agent_driver::main_phase(uvm_phase phase);
    super.main_phase(phase);
    //while(1) begin
    if(this.cfg.sqr_sw==tcnt_dec_base::ON && this.cfg.drv_sw==tcnt_dec_base::ON) begin
        while(1) begin
            req = null;
            seq_item_port.try_next_item(req);
            if(req!=null) begin
                repeat(req.pre_pkt_gap) begin
                    @this.vif.drv_mp.drv_cb;
                    this.drive_idle(this.cfg.drv_mode);
                end
                @this.vif.drv_mp.drv_cb;
                if (req.memblock_dispatch_wait_ready &&
                    (memblock_sync_pkg::dispatch_flush_in_progress ||
                     req.memblock_dispatch_flush_epoch != memblock_sync_pkg::dispatch_flush_epoch)) begin
                    req.memblock_dispatch_fired_mask = '0;
                    clear_dispatch_issue_ports(req);
                    this.send_pkt(req);
                    req.memblock_dispatch_aborted_by_redirect = 1'b1;
                end else begin
                    this.send_pkt(req);
                    if (req.memblock_dispatch_wait_ready) begin
                        if (req.memblock_dispatch_nonblocking_issue) begin
                            this.drive_dispatch_issue_one_cycle(req);
                        end else begin
                            this.wait_dispatch_issue_ready(req);
                        end
                    end
                end
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

task lintsissue_agent_agent_driver::send_pkt(lintsissue_agent_agent_xaction tr);

    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_src_0 <= tr.io_ooo_to_mem_issueLda_0_bits_src_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_fpWen <= tr.io_ooo_to_mem_issueLda_0_bits_uop_fpWen;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_ftqOffset <= tr.io_ooo_to_mem_issueLda_0_bits_uop_ftqOffset;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_ftqPtr_flag <= tr.io_ooo_to_mem_issueLda_0_bits_uop_ftqPtr_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_ftqPtr_value <= tr.io_ooo_to_mem_issueLda_0_bits_uop_ftqPtr_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_fuOpType <= tr.io_ooo_to_mem_issueLda_0_bits_uop_fuOpType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_imm <= tr.io_ooo_to_mem_issueLda_0_bits_uop_imm;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_loadWaitBit <= tr.io_ooo_to_mem_issueLda_0_bits_uop_loadWaitBit;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_loadWaitStrict <= tr.io_ooo_to_mem_issueLda_0_bits_uop_loadWaitStrict;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_lqIdx_flag <= tr.io_ooo_to_mem_issueLda_0_bits_uop_lqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_lqIdx_value <= tr.io_ooo_to_mem_issueLda_0_bits_uop_lqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_pc <= tr.io_ooo_to_mem_issueLda_0_bits_uop_pc;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_pdest <= tr.io_ooo_to_mem_issueLda_0_bits_uop_pdest;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_preDecodeInfo_isRVC <= tr.io_ooo_to_mem_issueLda_0_bits_uop_preDecodeInfo_isRVC;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_rfWen <= tr.io_ooo_to_mem_issueLda_0_bits_uop_rfWen;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_robIdx_flag <= tr.io_ooo_to_mem_issueLda_0_bits_uop_robIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_robIdx_value <= tr.io_ooo_to_mem_issueLda_0_bits_uop_robIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_sqIdx_flag <= tr.io_ooo_to_mem_issueLda_0_bits_uop_sqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_sqIdx_value <= tr.io_ooo_to_mem_issueLda_0_bits_uop_sqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_storeSetHit <= tr.io_ooo_to_mem_issueLda_0_bits_uop_storeSetHit;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_waitForRobIdx_flag <= tr.io_ooo_to_mem_issueLda_0_bits_uop_waitForRobIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_waitForRobIdx_value <= tr.io_ooo_to_mem_issueLda_0_bits_uop_waitForRobIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_valid <= tr.io_ooo_to_mem_issueLda_0_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_src_0 <= tr.io_ooo_to_mem_issueLda_1_bits_src_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_fpWen <= tr.io_ooo_to_mem_issueLda_1_bits_uop_fpWen;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_ftqOffset <= tr.io_ooo_to_mem_issueLda_1_bits_uop_ftqOffset;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_ftqPtr_flag <= tr.io_ooo_to_mem_issueLda_1_bits_uop_ftqPtr_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_ftqPtr_value <= tr.io_ooo_to_mem_issueLda_1_bits_uop_ftqPtr_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_fuOpType <= tr.io_ooo_to_mem_issueLda_1_bits_uop_fuOpType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_imm <= tr.io_ooo_to_mem_issueLda_1_bits_uop_imm;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_loadWaitBit <= tr.io_ooo_to_mem_issueLda_1_bits_uop_loadWaitBit;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_loadWaitStrict <= tr.io_ooo_to_mem_issueLda_1_bits_uop_loadWaitStrict;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_lqIdx_flag <= tr.io_ooo_to_mem_issueLda_1_bits_uop_lqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_lqIdx_value <= tr.io_ooo_to_mem_issueLda_1_bits_uop_lqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_pc <= tr.io_ooo_to_mem_issueLda_1_bits_uop_pc;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_pdest <= tr.io_ooo_to_mem_issueLda_1_bits_uop_pdest;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_preDecodeInfo_isRVC <= tr.io_ooo_to_mem_issueLda_1_bits_uop_preDecodeInfo_isRVC;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_rfWen <= tr.io_ooo_to_mem_issueLda_1_bits_uop_rfWen;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_robIdx_flag <= tr.io_ooo_to_mem_issueLda_1_bits_uop_robIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_robIdx_value <= tr.io_ooo_to_mem_issueLda_1_bits_uop_robIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_sqIdx_flag <= tr.io_ooo_to_mem_issueLda_1_bits_uop_sqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_sqIdx_value <= tr.io_ooo_to_mem_issueLda_1_bits_uop_sqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_storeSetHit <= tr.io_ooo_to_mem_issueLda_1_bits_uop_storeSetHit;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_waitForRobIdx_flag <= tr.io_ooo_to_mem_issueLda_1_bits_uop_waitForRobIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_waitForRobIdx_value <= tr.io_ooo_to_mem_issueLda_1_bits_uop_waitForRobIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_valid <= tr.io_ooo_to_mem_issueLda_1_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_src_0 <= tr.io_ooo_to_mem_issueLda_2_bits_src_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_fpWen <= tr.io_ooo_to_mem_issueLda_2_bits_uop_fpWen;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_ftqOffset <= tr.io_ooo_to_mem_issueLda_2_bits_uop_ftqOffset;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_ftqPtr_flag <= tr.io_ooo_to_mem_issueLda_2_bits_uop_ftqPtr_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_ftqPtr_value <= tr.io_ooo_to_mem_issueLda_2_bits_uop_ftqPtr_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_fuOpType <= tr.io_ooo_to_mem_issueLda_2_bits_uop_fuOpType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_imm <= tr.io_ooo_to_mem_issueLda_2_bits_uop_imm;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_loadWaitBit <= tr.io_ooo_to_mem_issueLda_2_bits_uop_loadWaitBit;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_loadWaitStrict <= tr.io_ooo_to_mem_issueLda_2_bits_uop_loadWaitStrict;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_lqIdx_flag <= tr.io_ooo_to_mem_issueLda_2_bits_uop_lqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_lqIdx_value <= tr.io_ooo_to_mem_issueLda_2_bits_uop_lqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_pc <= tr.io_ooo_to_mem_issueLda_2_bits_uop_pc;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_pdest <= tr.io_ooo_to_mem_issueLda_2_bits_uop_pdest;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_preDecodeInfo_isRVC <= tr.io_ooo_to_mem_issueLda_2_bits_uop_preDecodeInfo_isRVC;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_rfWen <= tr.io_ooo_to_mem_issueLda_2_bits_uop_rfWen;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_robIdx_flag <= tr.io_ooo_to_mem_issueLda_2_bits_uop_robIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_robIdx_value <= tr.io_ooo_to_mem_issueLda_2_bits_uop_robIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_sqIdx_flag <= tr.io_ooo_to_mem_issueLda_2_bits_uop_sqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_sqIdx_value <= tr.io_ooo_to_mem_issueLda_2_bits_uop_sqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_storeSetHit <= tr.io_ooo_to_mem_issueLda_2_bits_uop_storeSetHit;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_waitForRobIdx_flag <= tr.io_ooo_to_mem_issueLda_2_bits_uop_waitForRobIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_waitForRobIdx_value <= tr.io_ooo_to_mem_issueLda_2_bits_uop_waitForRobIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_valid <= tr.io_ooo_to_mem_issueLda_2_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_0_bits_src_0 <= tr.io_ooo_to_mem_issueSta_0_bits_src_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_0_bits_uop_fuOpType <= tr.io_ooo_to_mem_issueSta_0_bits_uop_fuOpType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_0_bits_uop_fuType <= tr.io_ooo_to_mem_issueSta_0_bits_uop_fuType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_0_bits_uop_imm <= tr.io_ooo_to_mem_issueSta_0_bits_uop_imm;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_0_bits_uop_pdest <= tr.io_ooo_to_mem_issueSta_0_bits_uop_pdest;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_0_bits_uop_rfWen <= tr.io_ooo_to_mem_issueSta_0_bits_uop_rfWen;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_0_bits_uop_robIdx_flag <= tr.io_ooo_to_mem_issueSta_0_bits_uop_robIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_0_bits_uop_robIdx_value <= tr.io_ooo_to_mem_issueSta_0_bits_uop_robIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_0_bits_uop_sqIdx_flag <= tr.io_ooo_to_mem_issueSta_0_bits_uop_sqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_0_bits_uop_sqIdx_value <= tr.io_ooo_to_mem_issueSta_0_bits_uop_sqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_0_valid <= tr.io_ooo_to_mem_issueSta_0_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_1_bits_src_0 <= tr.io_ooo_to_mem_issueSta_1_bits_src_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_1_bits_uop_fuOpType <= tr.io_ooo_to_mem_issueSta_1_bits_uop_fuOpType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_1_bits_uop_fuType <= tr.io_ooo_to_mem_issueSta_1_bits_uop_fuType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_1_bits_uop_imm <= tr.io_ooo_to_mem_issueSta_1_bits_uop_imm;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_1_bits_uop_pdest <= tr.io_ooo_to_mem_issueSta_1_bits_uop_pdest;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_1_bits_uop_rfWen <= tr.io_ooo_to_mem_issueSta_1_bits_uop_rfWen;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_1_bits_uop_robIdx_flag <= tr.io_ooo_to_mem_issueSta_1_bits_uop_robIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_1_bits_uop_robIdx_value <= tr.io_ooo_to_mem_issueSta_1_bits_uop_robIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_1_bits_uop_sqIdx_flag <= tr.io_ooo_to_mem_issueSta_1_bits_uop_sqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_1_bits_uop_sqIdx_value <= tr.io_ooo_to_mem_issueSta_1_bits_uop_sqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_1_valid <= tr.io_ooo_to_mem_issueSta_1_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_0_bits_src_0 <= tr.io_ooo_to_mem_issueStd_0_bits_src_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_0_bits_uop_fuOpType <= tr.io_ooo_to_mem_issueStd_0_bits_uop_fuOpType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_0_bits_uop_fuType <= tr.io_ooo_to_mem_issueStd_0_bits_uop_fuType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_0_bits_uop_robIdx_value <= tr.io_ooo_to_mem_issueStd_0_bits_uop_robIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_0_bits_uop_sqIdx_flag <= tr.io_ooo_to_mem_issueStd_0_bits_uop_sqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_0_bits_uop_sqIdx_value <= tr.io_ooo_to_mem_issueStd_0_bits_uop_sqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_0_valid <= tr.io_ooo_to_mem_issueStd_0_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_1_bits_src_0 <= tr.io_ooo_to_mem_issueStd_1_bits_src_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_1_bits_uop_fuOpType <= tr.io_ooo_to_mem_issueStd_1_bits_uop_fuOpType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_1_bits_uop_fuType <= tr.io_ooo_to_mem_issueStd_1_bits_uop_fuType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_1_bits_uop_robIdx_value <= tr.io_ooo_to_mem_issueStd_1_bits_uop_robIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_1_bits_uop_sqIdx_flag <= tr.io_ooo_to_mem_issueStd_1_bits_uop_sqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_1_bits_uop_sqIdx_value <= tr.io_ooo_to_mem_issueStd_1_bits_uop_sqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_1_valid <= tr.io_ooo_to_mem_issueStd_1_valid;

endtask:send_pkt

task lintsissue_agent_agent_driver::wait_dispatch_issue_ready(lintsissue_agent_agent_xaction tr);
    int unsigned wait_cycles;

    wait_cycles = 0;
    tr.memblock_dispatch_fired_mask = '0;
    while (has_dispatch_issue_pending(tr)) begin
        @this.vif.drv_mp.drv_cb;
        clear_ready_dispatch_issue_ports(tr);
        if (memblock_sync_pkg::dispatch_flush_in_progress ||
            tr.memblock_dispatch_flush_epoch != memblock_sync_pkg::dispatch_flush_epoch) begin
            clear_dispatch_issue_ports(tr);
            this.send_pkt(tr);
            tr.memblock_dispatch_aborted_by_redirect = 1'b1;
            return;
        end
        this.send_pkt(tr);
        wait_cycles++;
        if (tr.memblock_dispatch_ready_timeout != 0 &&
            wait_cycles >= tr.memblock_dispatch_ready_timeout &&
            has_dispatch_issue_pending(tr)) begin
            report_dispatch_issue_timeout(tr, wait_cycles);
            `uvm_fatal(get_type_name(),
                       $sformatf("dispatch lintsissue ready timeout after %0d cycles", wait_cycles))
        end
    end
endtask:wait_dispatch_issue_ready

task lintsissue_agent_agent_driver::drive_dispatch_issue_one_cycle(lintsissue_agent_agent_xaction tr);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "drive_dispatch_issue_one_cycle got null xaction")
    end

    tr.memblock_dispatch_fired_mask = '0;
    if (!has_dispatch_issue_pending(tr)) begin
        return;
    end

    @this.vif.drv_mp.drv_cb;

    clear_ready_dispatch_issue_ports(tr);

    if (memblock_sync_pkg::dispatch_flush_in_progress ||
        tr.memblock_dispatch_flush_epoch != memblock_sync_pkg::dispatch_flush_epoch) begin
        clear_dispatch_issue_ports(tr);
        this.send_pkt(tr);
        tr.memblock_dispatch_aborted_by_redirect = 1'b1;
        return;
    end

    clear_dispatch_issue_ports(tr);
    this.send_pkt(tr);
endtask:drive_dispatch_issue_one_cycle

function bit lintsissue_agent_agent_driver::has_dispatch_issue_pending(lintsissue_agent_agent_xaction tr);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "has_dispatch_issue_pending got null xaction")
    end
    return ((`MEMBLOCK_DUT_LOAD_PIPE_NUM > 0) && tr.io_ooo_to_mem_issueLda_0_valid) ||
           ((`MEMBLOCK_DUT_LOAD_PIPE_NUM > 1) && tr.io_ooo_to_mem_issueLda_1_valid) ||
           ((`MEMBLOCK_DUT_LOAD_PIPE_NUM > 2) && tr.io_ooo_to_mem_issueLda_2_valid) ||
           ((`MEMBLOCK_DUT_STA_PIPE_NUM > 0) && tr.io_ooo_to_mem_issueSta_0_valid) ||
           ((`MEMBLOCK_DUT_STA_PIPE_NUM > 1) && tr.io_ooo_to_mem_issueSta_1_valid) ||
           ((`MEMBLOCK_DUT_STD_PIPE_NUM > 0) && tr.io_ooo_to_mem_issueStd_0_valid) ||
           ((`MEMBLOCK_DUT_STD_PIPE_NUM > 1) && tr.io_ooo_to_mem_issueStd_1_valid);
endfunction:has_dispatch_issue_pending

function void lintsissue_agent_agent_driver::clear_dispatch_issue_ports(lintsissue_agent_agent_xaction tr);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "clear_dispatch_issue_ports got null xaction")
    end
    tr.io_ooo_to_mem_issueLda_0_valid = 1'b0;
    tr.io_ooo_to_mem_issueLda_1_valid = 1'b0;
    tr.io_ooo_to_mem_issueLda_2_valid = 1'b0;
    tr.io_ooo_to_mem_issueSta_0_valid = 1'b0;
    tr.io_ooo_to_mem_issueSta_1_valid = 1'b0;
    tr.io_ooo_to_mem_issueStd_0_valid = 1'b0;
    tr.io_ooo_to_mem_issueStd_1_valid = 1'b0;
endfunction:clear_dispatch_issue_ports

function void lintsissue_agent_agent_driver::clear_ready_dispatch_issue_ports(lintsissue_agent_agent_xaction tr);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "clear_ready_dispatch_issue_ports got null xaction")
    end
    if ((`MEMBLOCK_DUT_LOAD_PIPE_NUM > 0) && tr.io_ooo_to_mem_issueLda_0_valid) begin
        if ($isunknown(vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_ready)) begin
            `uvm_fatal(get_type_name(), "issueLda_0_ready is X/Z while valid")
        end
        if (vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_ready === 1'b1) begin
            tr.io_ooo_to_mem_issueLda_0_valid = 1'b0;
            record_dispatch_issue_fire(`MEMBLOCK_DUT_LOAD_PORT_BASE + 0, tr);
        end
    end
    if ((`MEMBLOCK_DUT_LOAD_PIPE_NUM > 1) && tr.io_ooo_to_mem_issueLda_1_valid) begin
        if ($isunknown(vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_ready)) begin
            `uvm_fatal(get_type_name(), "issueLda_1_ready is X/Z while valid")
        end
        if (vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_ready === 1'b1) begin
            tr.io_ooo_to_mem_issueLda_1_valid = 1'b0;
            record_dispatch_issue_fire(`MEMBLOCK_DUT_LOAD_PORT_BASE + 1, tr);
        end
    end
    if ((`MEMBLOCK_DUT_LOAD_PIPE_NUM > 2) && tr.io_ooo_to_mem_issueLda_2_valid) begin
        if ($isunknown(vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_ready)) begin
            `uvm_fatal(get_type_name(), "issueLda_2_ready is X/Z while valid")
        end
        if (vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_ready === 1'b1) begin
            tr.io_ooo_to_mem_issueLda_2_valid = 1'b0;
            record_dispatch_issue_fire(`MEMBLOCK_DUT_LOAD_PORT_BASE + 2, tr);
        end
    end
    if ((`MEMBLOCK_DUT_STA_PIPE_NUM > 0) && tr.io_ooo_to_mem_issueSta_0_valid) begin
        if ($isunknown(vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_0_ready)) begin
            `uvm_fatal(get_type_name(), "issueSta_0_ready is X/Z while valid")
        end
        if (vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_0_ready === 1'b1) begin
            tr.io_ooo_to_mem_issueSta_0_valid = 1'b0;
            record_dispatch_issue_fire(`MEMBLOCK_DUT_LOAD_PORT_BASE + `MEMBLOCK_DUT_LOAD_PIPE_NUM + 0, tr);
        end
    end
    if ((`MEMBLOCK_DUT_STA_PIPE_NUM > 1) && tr.io_ooo_to_mem_issueSta_1_valid) begin
        if ($isunknown(vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_1_ready)) begin
            `uvm_fatal(get_type_name(), "issueSta_1_ready is X/Z while valid")
        end
        if (vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_1_ready === 1'b1) begin
            tr.io_ooo_to_mem_issueSta_1_valid = 1'b0;
            record_dispatch_issue_fire(`MEMBLOCK_DUT_LOAD_PORT_BASE + `MEMBLOCK_DUT_LOAD_PIPE_NUM + 1, tr);
        end
    end
    if ((`MEMBLOCK_DUT_STD_PIPE_NUM > 0) && tr.io_ooo_to_mem_issueStd_0_valid) begin
        if ($isunknown(vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_0_ready)) begin
            `uvm_fatal(get_type_name(), "issueStd_0_ready is X/Z while valid")
        end
        if (vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_0_ready === 1'b1) begin
            tr.io_ooo_to_mem_issueStd_0_valid = 1'b0;
            record_dispatch_issue_fire(`MEMBLOCK_DUT_LOAD_PORT_BASE + `MEMBLOCK_DUT_LOAD_PIPE_NUM +
                                        `MEMBLOCK_DUT_STA_PIPE_NUM + 0, tr);
        end
    end
    if ((`MEMBLOCK_DUT_STD_PIPE_NUM > 1) && tr.io_ooo_to_mem_issueStd_1_valid) begin
        if ($isunknown(vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_1_ready)) begin
            `uvm_fatal(get_type_name(), "issueStd_1_ready is X/Z while valid")
        end
        if (vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_1_ready === 1'b1) begin
            tr.io_ooo_to_mem_issueStd_1_valid = 1'b0;
            record_dispatch_issue_fire(`MEMBLOCK_DUT_LOAD_PORT_BASE + `MEMBLOCK_DUT_LOAD_PIPE_NUM +
                                        `MEMBLOCK_DUT_STA_PIPE_NUM + 1, tr);
        end
    end
endfunction:clear_ready_dispatch_issue_ports

function void lintsissue_agent_agent_driver::record_dispatch_issue_fire(input int unsigned port_idx,
                                                                         lintsissue_agent_agent_xaction tr);
    localparam int unsigned issue_port_num =
        `MEMBLOCK_DUT_LOAD_PORT_BASE + `MEMBLOCK_DUT_LOAD_PIPE_NUM +
        `MEMBLOCK_DUT_STA_PIPE_NUM + `MEMBLOCK_DUT_STD_PIPE_NUM;

    if (tr == null) begin
        `uvm_fatal(get_type_name(), "record_dispatch_issue_fire got null xaction")
    end
    if (port_idx < `MEMBLOCK_DUT_LOAD_PORT_BASE || port_idx >= issue_port_num) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("fire port=%0d outside scalar issue range=[%0d,%0d)",
                             port_idx, `MEMBLOCK_DUT_LOAD_PORT_BASE, issue_port_num))
    end
    tr.memblock_dispatch_fired_mask[port_idx] = 1'b1;
    report_dispatch_issue_fire(port_idx, tr);
endfunction:record_dispatch_issue_fire

function void lintsissue_agent_agent_driver::report_dispatch_issue_fire(input int unsigned port_idx,
                                                                        lintsissue_agent_agent_xaction tr);
    localparam int unsigned load_base = `MEMBLOCK_DUT_LOAD_PORT_BASE;
    localparam int unsigned sta_base  = load_base + `MEMBLOCK_DUT_LOAD_PIPE_NUM;
    localparam int unsigned std_base  = sta_base + `MEMBLOCK_DUT_STA_PIPE_NUM;
    localparam int unsigned issue_port_num = std_base + `MEMBLOCK_DUT_STD_PIPE_NUM;

    if (tr == null) begin
        return;
    end
    if (port_idx < load_base || port_idx >= issue_port_num) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("report_dispatch_issue_fire got invalid port=%0d range=[%0d,%0d)",
                             port_idx, load_base, issue_port_num))
    end
    if (port_idx >= load_base && port_idx < sta_base) begin
        `uvm_info(get_type_name(),
                  $sformatf("dispatch issue fire lda_port=%0d", port_idx - load_base),
                  UVM_LOW)
        report_dispatch_hdl_bit("top_tb.U_MEMBLOCK._inner_LoadUnit_0_io_tlb_req_valid");
        report_dispatch_hdl_bit("top_tb.U_MEMBLOCK._inner_LoadUnit_0_io_dcache_req_valid");
        report_dispatch_hdl_bit("top_tb.U_MEMBLOCK._inner_dcache_io_lsu_load_0_req_ready");
    end else if (port_idx >= sta_base && port_idx < std_base) begin
        `uvm_info(get_type_name(),
                  $sformatf("dispatch issue fire sta_port=%0d", port_idx - sta_base),
                  UVM_LOW)
    end else begin
        `uvm_info(get_type_name(),
                  $sformatf("dispatch issue fire std_port=%0d", port_idx - std_base),
                  UVM_LOW)
    end
endfunction:report_dispatch_issue_fire

function void lintsissue_agent_agent_driver::report_dispatch_issue_timeout(lintsissue_agent_agent_xaction tr,
                                                                           int unsigned wait_cycles);
    if (tr == null) begin
        `uvm_info(get_type_name(), "dispatch timeout report got null xaction", UVM_LOW)
        return;
    end

    `uvm_info(get_type_name(),
              $sformatf("dispatch timeout wait_cycles=%0d lda_valid=%0b%0b%0b sta_valid=%0b%0b std_valid=%0b%0b",
                        wait_cycles,
                        tr.io_ooo_to_mem_issueLda_2_valid,
                        tr.io_ooo_to_mem_issueLda_1_valid,
                        tr.io_ooo_to_mem_issueLda_0_valid,
                        tr.io_ooo_to_mem_issueSta_1_valid,
                        tr.io_ooo_to_mem_issueSta_0_valid,
                        tr.io_ooo_to_mem_issueStd_1_valid,
                        tr.io_ooo_to_mem_issueStd_0_valid),
              UVM_LOW)
    `uvm_info(get_type_name(),
              $sformatf("load0 payload fuOpType=0x%0h src=0x%0h rob=%0d:%0d lq=%0d:%0d sq=%0d:%0d",
                        tr.io_ooo_to_mem_issueLda_0_bits_uop_fuOpType,
                        tr.io_ooo_to_mem_issueLda_0_bits_src_0,
                        tr.io_ooo_to_mem_issueLda_0_bits_uop_robIdx_flag,
                        tr.io_ooo_to_mem_issueLda_0_bits_uop_robIdx_value,
                        tr.io_ooo_to_mem_issueLda_0_bits_uop_lqIdx_flag,
                        tr.io_ooo_to_mem_issueLda_0_bits_uop_lqIdx_value,
                        tr.io_ooo_to_mem_issueLda_0_bits_uop_sqIdx_flag,
                        tr.io_ooo_to_mem_issueLda_0_bits_uop_sqIdx_value),
              UVM_LOW)
    report_dispatch_hdl_bit("top_tb.U_MEMBLOCK._inner_dcache_io_lsu_load_0_req_ready");
    report_dispatch_hdl_bit("top_tb.U_MEMBLOCK._inner_dcache_io_lsu_load_1_req_ready");
    report_dispatch_hdl_bit("top_tb.U_MEMBLOCK._inner_dcache_io_lsu_load_2_req_ready");
    report_dispatch_hdl_bit("top_tb.U_MEMBLOCK.auto_inner_dcache_client_out_a_ready");
    report_dispatch_hdl_bit("top_tb.U_MEMBLOCK.auto_inner_dcache_client_out_a_valid");
    report_dispatch_hdl_bit("top_tb.U_MEMBLOCK._inner_ptw_io_tlb_1_req_0_ready");
    report_dispatch_hdl_bit("top_tb.U_MEMBLOCK.inner_vSegmentFlag");
endfunction:report_dispatch_issue_timeout

function void lintsissue_agent_agent_driver::report_dispatch_hdl_bit(input string path);
    uvm_hdl_data_t value;

    if (uvm_hdl_read(path, value)) begin
        `uvm_info(get_type_name(),
                  $sformatf("hdl %s=%0b", path, value[0]),
                  UVM_LOW)
    end else begin
        `uvm_info(get_type_name(),
                  $sformatf("hdl %s unreadable", path),
                  UVM_LOW)
    end
endfunction:report_dispatch_hdl_bit

function void lintsissue_agent_agent_driver::report_dispatch_hdl_value(input string path);
    uvm_hdl_data_t value;

    if (uvm_hdl_read(path, value)) begin
        `uvm_info(get_type_name(),
                  $sformatf("hdl %s=0x%0h", path, value),
                  UVM_LOW)
    end else begin
        `uvm_info(get_type_name(),
                  $sformatf("hdl %s unreadable", path),
                  UVM_LOW)
    end
endfunction:report_dispatch_hdl_value

task lintsissue_agent_agent_driver::drive_idle(tcnt_dec_base::drv_mode_e drv_mode);

    if(drv_mode==tcnt_dec_base::DRV_0) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_src_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_fpWen <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_ftqOffset <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_ftqPtr_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_ftqPtr_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_imm <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_loadWaitBit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_loadWaitStrict <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_pc <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_pdest <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_preDecodeInfo_isRVC <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_rfWen <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_storeSetHit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_waitForRobIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_bits_uop_waitForRobIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_0_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_src_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_fpWen <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_ftqOffset <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_ftqPtr_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_ftqPtr_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_imm <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_loadWaitBit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_loadWaitStrict <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_pc <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_pdest <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_preDecodeInfo_isRVC <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_rfWen <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_storeSetHit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_waitForRobIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_bits_uop_waitForRobIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_1_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_src_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_fpWen <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_ftqOffset <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_ftqPtr_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_ftqPtr_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_imm <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_loadWaitBit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_loadWaitStrict <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_pc <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_pdest <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_preDecodeInfo_isRVC <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_rfWen <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_storeSetHit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_waitForRobIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_bits_uop_waitForRobIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueLda_2_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_0_bits_src_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_0_bits_uop_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_0_bits_uop_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_0_bits_uop_imm <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_0_bits_uop_pdest <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_0_bits_uop_rfWen <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_0_bits_uop_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_0_bits_uop_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_0_bits_uop_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_0_bits_uop_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_0_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_1_bits_src_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_1_bits_uop_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_1_bits_uop_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_1_bits_uop_imm <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_1_bits_uop_pdest <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_1_bits_uop_rfWen <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_1_bits_uop_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_1_bits_uop_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_1_bits_uop_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_1_bits_uop_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueSta_1_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_0_bits_src_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_0_bits_uop_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_0_bits_uop_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_0_bits_uop_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_0_bits_uop_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_0_bits_uop_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_0_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_1_bits_src_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_1_bits_uop_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_1_bits_uop_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_1_bits_uop_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_1_bits_uop_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_1_bits_uop_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_issueStd_1_valid <= '0;

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
