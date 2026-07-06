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
endclass:lsqenq_agent_agent_driver

function lsqenq_agent_agent_driver::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void lsqenq_agent_agent_driver::build_phase(uvm_phase phase);
    super.build_phase(phase);
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
            seq_item_port.try_next_item(req);
            if(req!=null) begin
                repeat(req.pre_pkt_gap) begin
                    @this.vif.drv_mp.drv_cb;
                    this.drive_idle(this.cfg.drv_mode);
                end
                if (req.memblock_dispatch_wait_can_accept) begin
                    this.wait_lsq_can_accept(req);
                end else begin
                    @this.vif.drv_mp.drv_cb;
                    this.send_pkt(req);
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

task lsqenq_agent_agent_driver::send_pkt(lsqenq_agent_agent_xaction tr);
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_0 <= tr.io_ooo_to_mem_enqLsq_needAlloc_0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_1 <= tr.io_ooo_to_mem_enqLsq_needAlloc_1;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_2 <= tr.io_ooo_to_mem_enqLsq_needAlloc_2;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_3 <= tr.io_ooo_to_mem_enqLsq_needAlloc_3;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_4 <= tr.io_ooo_to_mem_enqLsq_needAlloc_4;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_5 <= tr.io_ooo_to_mem_enqLsq_needAlloc_5;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_6 <= tr.io_ooo_to_mem_enqLsq_needAlloc_6;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_7 <= tr.io_ooo_to_mem_enqLsq_needAlloc_7;
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
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_valid <= tr.io_ooo_to_mem_enqLsq_req_6_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_fuType <= tr.io_ooo_to_mem_enqLsq_req_6_bits_fuType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_uopIdx <= tr.io_ooo_to_mem_enqLsq_req_6_bits_uopIdx;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_flag <= tr.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_value <= tr.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_flag <= tr.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_value <= tr.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_flag <= tr.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_value <= tr.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_numLsElem <= tr.io_ooo_to_mem_enqLsq_req_6_bits_numLsElem;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_valid <= tr.io_ooo_to_mem_enqLsq_req_7_valid;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_fuType <= tr.io_ooo_to_mem_enqLsq_req_7_bits_fuType;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_uopIdx <= tr.io_ooo_to_mem_enqLsq_req_7_bits_uopIdx;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_flag <= tr.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_value <= tr.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_flag <= tr.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_value <= tr.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_flag <= tr.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_value <= tr.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_numLsElem <= tr.io_ooo_to_mem_enqLsq_req_7_bits_numLsElem;

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
        if (sent_once && this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_canAccept === 1'b1) begin
            this.sample_lsqenq_resp(tr);
            this.drive_idle(this.cfg.drv_mode);
            return;
        end
        if (tr.memblock_dispatch_ready_timeout != 0 &&
            wait_count >= tr.memblock_dispatch_ready_timeout) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("timeout waiting for io_ooo_to_mem_enqLsq_canAccept after %0d cycles",
                                 wait_count))
        end
        this.send_pkt(tr);
        sent_once = 1'b1;
        wait_count++;
    end
endtask:wait_lsq_can_accept

task lsqenq_agent_agent_driver::sample_lsqenq_resp(lsqenq_agent_agent_xaction tr);
    tr.io_ooo_to_mem_enqLsq_canAccept = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_canAccept;
    tr.io_ooo_to_mem_enqLsq_resp_0_lqIdx_flag = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_0_lqIdx_flag;
    tr.io_ooo_to_mem_enqLsq_resp_0_lqIdx_value = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_0_lqIdx_value;
    tr.io_ooo_to_mem_enqLsq_resp_0_sqIdx_flag = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_0_sqIdx_flag;
    tr.io_ooo_to_mem_enqLsq_resp_0_sqIdx_value = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_0_sqIdx_value;
    tr.io_ooo_to_mem_enqLsq_resp_1_lqIdx_flag = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_1_lqIdx_flag;
    tr.io_ooo_to_mem_enqLsq_resp_1_lqIdx_value = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_1_lqIdx_value;
    tr.io_ooo_to_mem_enqLsq_resp_1_sqIdx_flag = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_1_sqIdx_flag;
    tr.io_ooo_to_mem_enqLsq_resp_1_sqIdx_value = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_1_sqIdx_value;
    tr.io_ooo_to_mem_enqLsq_resp_2_lqIdx_flag = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_2_lqIdx_flag;
    tr.io_ooo_to_mem_enqLsq_resp_2_lqIdx_value = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_2_lqIdx_value;
    tr.io_ooo_to_mem_enqLsq_resp_2_sqIdx_flag = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_2_sqIdx_flag;
    tr.io_ooo_to_mem_enqLsq_resp_2_sqIdx_value = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_2_sqIdx_value;
    tr.io_ooo_to_mem_enqLsq_resp_3_lqIdx_flag = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_3_lqIdx_flag;
    tr.io_ooo_to_mem_enqLsq_resp_3_lqIdx_value = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_3_lqIdx_value;
    tr.io_ooo_to_mem_enqLsq_resp_3_sqIdx_flag = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_3_sqIdx_flag;
    tr.io_ooo_to_mem_enqLsq_resp_3_sqIdx_value = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_3_sqIdx_value;
    tr.io_ooo_to_mem_enqLsq_resp_4_lqIdx_flag = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_4_lqIdx_flag;
    tr.io_ooo_to_mem_enqLsq_resp_4_lqIdx_value = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_4_lqIdx_value;
    tr.io_ooo_to_mem_enqLsq_resp_4_sqIdx_flag = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_4_sqIdx_flag;
    tr.io_ooo_to_mem_enqLsq_resp_4_sqIdx_value = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_4_sqIdx_value;
    tr.io_ooo_to_mem_enqLsq_resp_5_lqIdx_flag = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_5_lqIdx_flag;
    tr.io_ooo_to_mem_enqLsq_resp_5_lqIdx_value = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_5_lqIdx_value;
    tr.io_ooo_to_mem_enqLsq_resp_5_sqIdx_flag = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_5_sqIdx_flag;
    tr.io_ooo_to_mem_enqLsq_resp_5_sqIdx_value = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_5_sqIdx_value;
    tr.io_ooo_to_mem_enqLsq_resp_6_lqIdx_flag = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_6_lqIdx_flag;
    tr.io_ooo_to_mem_enqLsq_resp_6_lqIdx_value = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_6_lqIdx_value;
    tr.io_ooo_to_mem_enqLsq_resp_6_sqIdx_flag = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_6_sqIdx_flag;
    tr.io_ooo_to_mem_enqLsq_resp_6_sqIdx_value = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_6_sqIdx_value;
    tr.io_ooo_to_mem_enqLsq_resp_7_lqIdx_flag = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_7_lqIdx_flag;
    tr.io_ooo_to_mem_enqLsq_resp_7_lqIdx_value = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_7_lqIdx_value;
    tr.io_ooo_to_mem_enqLsq_resp_7_sqIdx_flag = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_7_sqIdx_flag;
    tr.io_ooo_to_mem_enqLsq_resp_7_sqIdx_value = this.vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_resp_7_sqIdx_value;
endtask:sample_lsqenq_resp

task lsqenq_agent_agent_driver::drive_idle(tcnt_dec_base::drv_mode_e drv_mode);

    if(drv_mode==tcnt_dec_base::DRV_0) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_1 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_3 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_4 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_5 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_6 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_7 <= '0;
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
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_uopIdx <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_numLsElem <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_uopIdx <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_numLsElem <= '0;

    end
    else if(drv_mode==tcnt_dec_base::DRV_1) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_0 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_1 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_2 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_3 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_4 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_5 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_6 <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_7 <= '1;
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
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_valid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_fuType <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_uopIdx <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_numLsElem <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_valid <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_fuType <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_uopIdx <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_numLsElem <= '1;

    end
    else if(drv_mode==tcnt_dec_base::DRV_X) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_0 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_1 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_2 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_3 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_4 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_5 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_6 <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_7 <= 'x;
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
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_valid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_fuType <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_uopIdx <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_numLsElem <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_valid <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_fuType <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_uopIdx <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_numLsElem <= 'x;

    end
    else if(drv_mode==tcnt_dec_base::DRV_RAND) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_0 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_1 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_2 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_3 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_4 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_5 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_6 <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_7 <= $urandom;
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
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_valid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_fuType <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_uopIdx <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_numLsElem <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_valid <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_fuType <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_uopIdx <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_numLsElem <= $urandom;

    end
    else if(drv_mode==tcnt_dec_base::DRV_LST) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_0 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_1 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_2 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_3 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_4 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_5 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_6 <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_needAlloc_7 <= '0;
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
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_uopIdx <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_6_bits_numLsElem <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_valid <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_fuType <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_uopIdx <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_enqLsq_req_7_bits_numLsElem <= '0;

    end

endtask:drive_idle

`endif
