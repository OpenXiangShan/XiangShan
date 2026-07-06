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
            seq_item_port.try_next_item(req);
            if(req!=null) begin
                repeat(req.pre_pkt_gap) begin
                    @this.vif.drv_mp.drv_cb;
                    this.drive_idle(this.cfg.drv_mode);
                end
                @this.vif.drv_mp.drv_cb;
                this.send_pkt(req);
                if (req.memblock_dispatch_wait_ready) begin
                    if (req.memblock_dispatch_nonblocking_issue) begin
                        this.drive_dispatch_issue_one_cycle(req);
                    end else begin
                        this.wait_dispatch_issue_ready(req);
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
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_valid <= tr.io_ooo_to_mem_intIssue_6_0_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_fuType <= tr.io_ooo_to_mem_intIssue_6_0_bits_fuType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_fuOpType <= tr.io_ooo_to_mem_intIssue_6_0_bits_fuOpType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_src_0 <= tr.io_ooo_to_mem_intIssue_6_0_bits_src_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_robIdx_flag <= tr.io_ooo_to_mem_intIssue_6_0_bits_robIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_robIdx_value <= tr.io_ooo_to_mem_intIssue_6_0_bits_robIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_flag <= tr.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_value <= tr.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_valid <= tr.io_ooo_to_mem_intIssue_5_0_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_fuType <= tr.io_ooo_to_mem_intIssue_5_0_bits_fuType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_fuOpType <= tr.io_ooo_to_mem_intIssue_5_0_bits_fuOpType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_src_0 <= tr.io_ooo_to_mem_intIssue_5_0_bits_src_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_robIdx_flag <= tr.io_ooo_to_mem_intIssue_5_0_bits_robIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_robIdx_value <= tr.io_ooo_to_mem_intIssue_5_0_bits_robIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_flag <= tr.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_value <= tr.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_valid <= tr.io_ooo_to_mem_intIssue_4_0_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_fuType <= tr.io_ooo_to_mem_intIssue_4_0_bits_fuType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_fuOpType <= tr.io_ooo_to_mem_intIssue_4_0_bits_fuOpType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_src_0 <= tr.io_ooo_to_mem_intIssue_4_0_bits_src_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_imm <= tr.io_ooo_to_mem_intIssue_4_0_bits_imm;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_robIdx_flag <= tr.io_ooo_to_mem_intIssue_4_0_bits_robIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_robIdx_value <= tr.io_ooo_to_mem_intIssue_4_0_bits_robIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_isFirstIssue <= tr.io_ooo_to_mem_intIssue_4_0_bits_isFirstIssue;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_pdest <= tr.io_ooo_to_mem_intIssue_4_0_bits_pdest;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_isRVC <= tr.io_ooo_to_mem_intIssue_4_0_bits_isRVC;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_flag <= tr.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_value <= tr.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_ftqOffset <= tr.io_ooo_to_mem_intIssue_4_0_bits_ftqOffset;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_storeSetHit <= tr.io_ooo_to_mem_intIssue_4_0_bits_storeSetHit;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_ssid <= tr.io_ooo_to_mem_intIssue_4_0_bits_ssid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_flag <= tr.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_value <= tr.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_valid <= tr.io_ooo_to_mem_intIssue_3_0_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_fuType <= tr.io_ooo_to_mem_intIssue_3_0_bits_fuType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_fuOpType <= tr.io_ooo_to_mem_intIssue_3_0_bits_fuOpType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_src_0 <= tr.io_ooo_to_mem_intIssue_3_0_bits_src_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_imm <= tr.io_ooo_to_mem_intIssue_3_0_bits_imm;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_robIdx_flag <= tr.io_ooo_to_mem_intIssue_3_0_bits_robIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_robIdx_value <= tr.io_ooo_to_mem_intIssue_3_0_bits_robIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_isFirstIssue <= tr.io_ooo_to_mem_intIssue_3_0_bits_isFirstIssue;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_pdest <= tr.io_ooo_to_mem_intIssue_3_0_bits_pdest;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_isRVC <= tr.io_ooo_to_mem_intIssue_3_0_bits_isRVC;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_flag <= tr.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_value <= tr.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_ftqOffset <= tr.io_ooo_to_mem_intIssue_3_0_bits_ftqOffset;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_storeSetHit <= tr.io_ooo_to_mem_intIssue_3_0_bits_storeSetHit;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_ssid <= tr.io_ooo_to_mem_intIssue_3_0_bits_ssid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_flag <= tr.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_value <= tr.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_valid <= tr.io_ooo_to_mem_intIssue_2_0_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_fuOpType <= tr.io_ooo_to_mem_intIssue_2_0_bits_fuOpType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_src_0 <= tr.io_ooo_to_mem_intIssue_2_0_bits_src_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_imm <= tr.io_ooo_to_mem_intIssue_2_0_bits_imm;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_robIdx_flag <= tr.io_ooo_to_mem_intIssue_2_0_bits_robIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_robIdx_value <= tr.io_ooo_to_mem_intIssue_2_0_bits_robIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_pdest <= tr.io_ooo_to_mem_intIssue_2_0_bits_pdest;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_rfWen <= tr.io_ooo_to_mem_intIssue_2_0_bits_rfWen;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_fpWen <= tr.io_ooo_to_mem_intIssue_2_0_bits_fpWen;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_pc <= tr.io_ooo_to_mem_intIssue_2_0_bits_pc;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_isRVC <= tr.io_ooo_to_mem_intIssue_2_0_bits_isRVC;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_flag <= tr.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_value <= tr.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_ftqOffset <= tr.io_ooo_to_mem_intIssue_2_0_bits_ftqOffset;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_loadWaitBit <= tr.io_ooo_to_mem_intIssue_2_0_bits_loadWaitBit;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_flag <= tr.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_value <= tr.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_storeSetHit <= tr.io_ooo_to_mem_intIssue_2_0_bits_storeSetHit;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_loadWaitStrict <= tr.io_ooo_to_mem_intIssue_2_0_bits_loadWaitStrict;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_flag <= tr.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_value <= tr.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_flag <= tr.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_value <= tr.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_valid <= tr.io_ooo_to_mem_intIssue_1_0_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_fuOpType <= tr.io_ooo_to_mem_intIssue_1_0_bits_fuOpType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_src_0 <= tr.io_ooo_to_mem_intIssue_1_0_bits_src_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_imm <= tr.io_ooo_to_mem_intIssue_1_0_bits_imm;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_robIdx_flag <= tr.io_ooo_to_mem_intIssue_1_0_bits_robIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_robIdx_value <= tr.io_ooo_to_mem_intIssue_1_0_bits_robIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_pdest <= tr.io_ooo_to_mem_intIssue_1_0_bits_pdest;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_rfWen <= tr.io_ooo_to_mem_intIssue_1_0_bits_rfWen;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_fpWen <= tr.io_ooo_to_mem_intIssue_1_0_bits_fpWen;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_pc <= tr.io_ooo_to_mem_intIssue_1_0_bits_pc;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_isRVC <= tr.io_ooo_to_mem_intIssue_1_0_bits_isRVC;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_flag <= tr.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_value <= tr.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_ftqOffset <= tr.io_ooo_to_mem_intIssue_1_0_bits_ftqOffset;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_loadWaitBit <= tr.io_ooo_to_mem_intIssue_1_0_bits_loadWaitBit;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_flag <= tr.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_value <= tr.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_storeSetHit <= tr.io_ooo_to_mem_intIssue_1_0_bits_storeSetHit;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_loadWaitStrict <= tr.io_ooo_to_mem_intIssue_1_0_bits_loadWaitStrict;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_flag <= tr.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_value <= tr.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_flag <= tr.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_value <= tr.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_valid <= tr.io_ooo_to_mem_intIssue_0_0_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_fuOpType <= tr.io_ooo_to_mem_intIssue_0_0_bits_fuOpType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_src_0 <= tr.io_ooo_to_mem_intIssue_0_0_bits_src_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_imm <= tr.io_ooo_to_mem_intIssue_0_0_bits_imm;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_robIdx_flag <= tr.io_ooo_to_mem_intIssue_0_0_bits_robIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_robIdx_value <= tr.io_ooo_to_mem_intIssue_0_0_bits_robIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_pdest <= tr.io_ooo_to_mem_intIssue_0_0_bits_pdest;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_rfWen <= tr.io_ooo_to_mem_intIssue_0_0_bits_rfWen;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_fpWen <= tr.io_ooo_to_mem_intIssue_0_0_bits_fpWen;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_pc <= tr.io_ooo_to_mem_intIssue_0_0_bits_pc;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_isRVC <= tr.io_ooo_to_mem_intIssue_0_0_bits_isRVC;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_flag <= tr.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_value <= tr.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_ftqOffset <= tr.io_ooo_to_mem_intIssue_0_0_bits_ftqOffset;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_loadWaitBit <= tr.io_ooo_to_mem_intIssue_0_0_bits_loadWaitBit;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_flag <= tr.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_value <= tr.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_storeSetHit <= tr.io_ooo_to_mem_intIssue_0_0_bits_storeSetHit;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_loadWaitStrict <= tr.io_ooo_to_mem_intIssue_0_0_bits_loadWaitStrict;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_flag <= tr.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_value <= tr.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_flag <= tr.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_value <= tr.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_value;

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

    if (memblock_sync_pkg::dispatch_flush_in_progress ||
        tr.memblock_dispatch_flush_epoch != memblock_sync_pkg::dispatch_flush_epoch) begin
        clear_dispatch_issue_ports(tr);
        this.send_pkt(tr);
        tr.memblock_dispatch_aborted_by_redirect = 1'b1;
        return;
    end

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
    return tr.io_ooo_to_mem_intIssue_6_0_valid ||
           tr.io_ooo_to_mem_intIssue_5_0_valid ||
           tr.io_ooo_to_mem_intIssue_4_0_valid ||
           tr.io_ooo_to_mem_intIssue_3_0_valid ||
           tr.io_ooo_to_mem_intIssue_2_0_valid ||
           tr.io_ooo_to_mem_intIssue_1_0_valid ||
           tr.io_ooo_to_mem_intIssue_0_0_valid;
endfunction:has_dispatch_issue_pending

function void lintsissue_agent_agent_driver::clear_dispatch_issue_ports(lintsissue_agent_agent_xaction tr);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "clear_dispatch_issue_ports got null xaction")
    end
    tr.io_ooo_to_mem_intIssue_6_0_valid = 1'b0;
    tr.io_ooo_to_mem_intIssue_5_0_valid = 1'b0;
    tr.io_ooo_to_mem_intIssue_4_0_valid = 1'b0;
    tr.io_ooo_to_mem_intIssue_3_0_valid = 1'b0;
    tr.io_ooo_to_mem_intIssue_2_0_valid = 1'b0;
    tr.io_ooo_to_mem_intIssue_1_0_valid = 1'b0;
    tr.io_ooo_to_mem_intIssue_0_0_valid = 1'b0;
endfunction:clear_dispatch_issue_ports

function void lintsissue_agent_agent_driver::clear_ready_dispatch_issue_ports(lintsissue_agent_agent_xaction tr);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "clear_ready_dispatch_issue_ports got null xaction")
    end
    if (tr.io_ooo_to_mem_intIssue_6_0_valid && vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_ready) begin
        report_dispatch_issue_fire(6, tr);
        tr.memblock_dispatch_fired_mask[6] = 1'b1;
        tr.io_ooo_to_mem_intIssue_6_0_valid = 1'b0;
    end
    if (tr.io_ooo_to_mem_intIssue_5_0_valid && vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_ready) begin
        report_dispatch_issue_fire(5, tr);
        tr.memblock_dispatch_fired_mask[5] = 1'b1;
        tr.io_ooo_to_mem_intIssue_5_0_valid = 1'b0;
    end
    if (tr.io_ooo_to_mem_intIssue_4_0_valid && vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_ready) begin
        report_dispatch_issue_fire(4, tr);
        tr.memblock_dispatch_fired_mask[4] = 1'b1;
        tr.io_ooo_to_mem_intIssue_4_0_valid = 1'b0;
    end
    if (tr.io_ooo_to_mem_intIssue_3_0_valid && vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_ready) begin
        report_dispatch_issue_fire(3, tr);
        tr.memblock_dispatch_fired_mask[3] = 1'b1;
        tr.io_ooo_to_mem_intIssue_3_0_valid = 1'b0;
    end
    if (tr.io_ooo_to_mem_intIssue_2_0_valid && vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_ready) begin
        report_dispatch_issue_fire(2, tr);
        tr.memblock_dispatch_fired_mask[2] = 1'b1;
        tr.io_ooo_to_mem_intIssue_2_0_valid = 1'b0;
    end
    if (tr.io_ooo_to_mem_intIssue_1_0_valid && vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_ready) begin
        report_dispatch_issue_fire(1, tr);
        tr.memblock_dispatch_fired_mask[1] = 1'b1;
        tr.io_ooo_to_mem_intIssue_1_0_valid = 1'b0;
    end
    if (tr.io_ooo_to_mem_intIssue_0_0_valid && vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_ready) begin
        report_dispatch_issue_fire(0, tr);
        tr.memblock_dispatch_fired_mask[0] = 1'b1;
        tr.io_ooo_to_mem_intIssue_0_0_valid = 1'b0;
    end
endfunction:clear_ready_dispatch_issue_ports

function void lintsissue_agent_agent_driver::report_dispatch_issue_fire(input int unsigned port_idx,
                                                                        lintsissue_agent_agent_xaction tr);
    if (tr == null) begin
        return;
    end
    if (port_idx == 0) begin
        `uvm_info(get_type_name(),
                  $sformatf("dispatch issue fire port=%0d fuOpType=0x%0h src=0x%0h imm=0x%0h rob=%0d:%0d lq=%0d:%0d sq=%0d:%0d rf/fp=%0d/%0d",
                            port_idx,
                            tr.io_ooo_to_mem_intIssue_0_0_bits_fuOpType,
                            tr.io_ooo_to_mem_intIssue_0_0_bits_src_0,
                            tr.io_ooo_to_mem_intIssue_0_0_bits_imm,
                            tr.io_ooo_to_mem_intIssue_0_0_bits_robIdx_flag,
                            tr.io_ooo_to_mem_intIssue_0_0_bits_robIdx_value,
                            tr.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_flag,
                            tr.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_value,
                            tr.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_flag,
                            tr.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_value,
                            tr.io_ooo_to_mem_intIssue_0_0_bits_rfWen,
                            tr.io_ooo_to_mem_intIssue_0_0_bits_fpWen),
                  UVM_LOW)
        report_dispatch_hdl_bit("top_tb.U_MEMBLOCK.io_ooo_to_mem_intIssue_0_0_valid");
        report_dispatch_hdl_bit("top_tb.U_MEMBLOCK.io_ooo_to_mem_intIssue_0_0_ready");
        report_dispatch_hdl_value("top_tb.U_MEMBLOCK.io_ooo_to_mem_intIssue_0_0_bits_fuOpType");
        report_dispatch_hdl_value("top_tb.U_MEMBLOCK.io_ooo_to_mem_intIssue_0_0_bits_src_0");
        report_dispatch_hdl_value("top_tb.U_MEMBLOCK.io_ooo_to_mem_intIssue_0_0_bits_imm");
        report_dispatch_hdl_bit("top_tb.U_MEMBLOCK._inner_LoadUnit_0_io_tlb_req_valid");
        report_dispatch_hdl_bit("top_tb.U_MEMBLOCK._inner_LoadUnit_0_io_dcache_req_valid");
        report_dispatch_hdl_bit("top_tb.U_MEMBLOCK._inner_dcache_io_lsu_load_0_req_ready");
    end else begin
        `uvm_info(get_type_name(),
                  $sformatf("dispatch issue fire port=%0d", port_idx),
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
              $sformatf("dispatch timeout wait_cycles=%0d vld[6:0]=%0b%0b%0b%0b%0b%0b%0b rdy[6:0]=%0b%0b%0b%0b%0b%0b%0b",
                        wait_cycles,
                        tr.io_ooo_to_mem_intIssue_6_0_valid,
                        tr.io_ooo_to_mem_intIssue_5_0_valid,
                        tr.io_ooo_to_mem_intIssue_4_0_valid,
                        tr.io_ooo_to_mem_intIssue_3_0_valid,
                        tr.io_ooo_to_mem_intIssue_2_0_valid,
                        tr.io_ooo_to_mem_intIssue_1_0_valid,
                        tr.io_ooo_to_mem_intIssue_0_0_valid,
                        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_ready,
                        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_ready,
                        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_ready,
                        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_ready,
                        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_ready,
                        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_ready,
                        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_ready),
              UVM_LOW)
    `uvm_info(get_type_name(),
              $sformatf("load0 payload fuOpType=0x%0h src=0x%0h imm=0x%0h rob=%0d:%0d lq=%0d:%0d sq=%0d:%0d rf/fp=%0d/%0d",
                        tr.io_ooo_to_mem_intIssue_0_0_bits_fuOpType,
                        tr.io_ooo_to_mem_intIssue_0_0_bits_src_0,
                        tr.io_ooo_to_mem_intIssue_0_0_bits_imm,
                        tr.io_ooo_to_mem_intIssue_0_0_bits_robIdx_flag,
                        tr.io_ooo_to_mem_intIssue_0_0_bits_robIdx_value,
                        tr.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_flag,
                        tr.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_value,
                        tr.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_flag,
                        tr.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_value,
                        tr.io_ooo_to_mem_intIssue_0_0_bits_rfWen,
                        tr.io_ooo_to_mem_intIssue_0_0_bits_fpWen),
              UVM_LOW)
    report_dispatch_hdl_bit("top_tb.U_MEMBLOCK.io_ooo_to_mem_intIssue_0_0_ready");
    report_dispatch_hdl_bit("top_tb.U_MEMBLOCK.io_ooo_to_mem_intIssue_1_0_ready");
    report_dispatch_hdl_bit("top_tb.U_MEMBLOCK.io_ooo_to_mem_intIssue_2_0_ready");
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
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_src_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_src_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_src_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_imm <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_isFirstIssue <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_pdest <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_isRVC <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_ftqOffset <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_storeSetHit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_ssid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_src_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_imm <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_isFirstIssue <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_pdest <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_isRVC <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_ftqOffset <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_storeSetHit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_ssid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_src_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_imm <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_pdest <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_rfWen <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_fpWen <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_pc <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_isRVC <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_ftqOffset <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_loadWaitBit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_storeSetHit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_loadWaitStrict <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_src_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_imm <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_pdest <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_rfWen <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_fpWen <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_pc <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_isRVC <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_ftqOffset <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_loadWaitBit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_storeSetHit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_loadWaitStrict <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_src_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_imm <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_pdest <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_rfWen <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_fpWen <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_pc <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_isRVC <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_ftqOffset <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_loadWaitBit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_storeSetHit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_loadWaitStrict <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_value <= '0;

    end
    else if(drv_mode==tcnt_dec_base::DRV_1) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_valid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_fuType <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_fuOpType <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_src_0 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_robIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_robIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_valid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_fuType <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_fuOpType <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_src_0 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_robIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_robIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_valid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_fuType <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_fuOpType <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_src_0 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_imm <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_robIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_robIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_isFirstIssue <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_pdest <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_isRVC <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_ftqOffset <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_storeSetHit <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_ssid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_valid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_fuType <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_fuOpType <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_src_0 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_imm <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_robIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_robIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_isFirstIssue <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_pdest <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_isRVC <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_ftqOffset <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_storeSetHit <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_ssid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_valid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_fuOpType <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_src_0 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_imm <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_robIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_robIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_pdest <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_rfWen <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_fpWen <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_pc <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_isRVC <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_ftqOffset <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_loadWaitBit <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_storeSetHit <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_loadWaitStrict <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_valid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_fuOpType <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_src_0 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_imm <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_robIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_robIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_pdest <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_rfWen <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_fpWen <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_pc <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_isRVC <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_ftqOffset <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_loadWaitBit <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_storeSetHit <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_loadWaitStrict <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_valid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_fuOpType <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_src_0 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_imm <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_robIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_robIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_pdest <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_rfWen <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_fpWen <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_pc <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_isRVC <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_ftqOffset <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_loadWaitBit <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_storeSetHit <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_loadWaitStrict <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_value <= '1;

    end
    else if(drv_mode==tcnt_dec_base::DRV_X) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_valid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_fuType <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_fuOpType <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_src_0 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_robIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_robIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_valid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_fuType <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_fuOpType <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_src_0 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_robIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_robIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_valid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_fuType <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_fuOpType <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_src_0 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_imm <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_robIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_robIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_isFirstIssue <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_pdest <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_isRVC <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_ftqOffset <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_storeSetHit <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_ssid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_valid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_fuType <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_fuOpType <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_src_0 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_imm <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_robIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_robIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_isFirstIssue <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_pdest <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_isRVC <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_ftqOffset <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_storeSetHit <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_ssid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_valid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_fuOpType <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_src_0 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_imm <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_robIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_robIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_pdest <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_rfWen <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_fpWen <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_pc <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_isRVC <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_ftqOffset <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_loadWaitBit <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_storeSetHit <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_loadWaitStrict <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_valid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_fuOpType <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_src_0 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_imm <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_robIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_robIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_pdest <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_rfWen <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_fpWen <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_pc <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_isRVC <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_ftqOffset <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_loadWaitBit <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_storeSetHit <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_loadWaitStrict <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_valid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_fuOpType <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_src_0 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_imm <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_robIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_robIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_pdest <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_rfWen <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_fpWen <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_pc <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_isRVC <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_ftqOffset <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_loadWaitBit <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_storeSetHit <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_loadWaitStrict <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_value <= 'x;

    end
    else if(drv_mode==tcnt_dec_base::DRV_RAND) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_valid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_fuType <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_fuOpType <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_src_0 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_robIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_robIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_valid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_fuType <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_fuOpType <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_src_0 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_robIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_robIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_valid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_fuType <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_fuOpType <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_src_0 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_imm <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_robIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_robIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_isFirstIssue <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_pdest <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_isRVC <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_ftqOffset <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_storeSetHit <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_ssid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_valid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_fuType <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_fuOpType <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_src_0 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_imm <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_robIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_robIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_isFirstIssue <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_pdest <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_isRVC <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_ftqOffset <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_storeSetHit <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_ssid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_valid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_fuOpType <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_src_0 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_imm <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_robIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_robIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_pdest <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_rfWen <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_fpWen <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_pc <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_isRVC <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_ftqOffset <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_loadWaitBit <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_storeSetHit <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_loadWaitStrict <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_valid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_fuOpType <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_src_0 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_imm <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_robIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_robIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_pdest <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_rfWen <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_fpWen <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_pc <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_isRVC <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_ftqOffset <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_loadWaitBit <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_storeSetHit <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_loadWaitStrict <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_valid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_fuOpType <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_src_0 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_imm <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_robIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_robIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_pdest <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_rfWen <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_fpWen <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_pc <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_isRVC <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_ftqOffset <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_loadWaitBit <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_storeSetHit <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_loadWaitStrict <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_value <= $urandom;

    end
    else if(drv_mode==tcnt_dec_base::DRV_LST) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_src_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_src_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_src_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_imm <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_isFirstIssue <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_pdest <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_isRVC <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_ftqOffset <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_storeSetHit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_ssid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_src_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_imm <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_isFirstIssue <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_pdest <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_isRVC <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_ftqOffset <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_storeSetHit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_ssid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_src_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_imm <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_pdest <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_rfWen <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_fpWen <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_pc <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_isRVC <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_ftqOffset <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_loadWaitBit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_storeSetHit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_loadWaitStrict <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_src_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_imm <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_pdest <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_rfWen <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_fpWen <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_pc <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_isRVC <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_ftqOffset <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_loadWaitBit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_storeSetHit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_loadWaitStrict <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_fuOpType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_src_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_imm <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_pdest <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_rfWen <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_fpWen <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_pc <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_isRVC <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_ftqOffset <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_loadWaitBit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_storeSetHit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_loadWaitStrict <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_value <= '0;

    end

endtask:drive_idle

`endif
