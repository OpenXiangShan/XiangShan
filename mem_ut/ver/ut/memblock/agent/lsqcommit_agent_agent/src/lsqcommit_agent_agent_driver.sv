//=========================================================
//File name    : lsqcommit_agent_agent_driver.sv
//Author       : OpenAI_Codex
//Module name  : lsqcommit_agent_agent_driver
//Discribution : lsqcommit_agent_agent_driver : driver
//Date         : 2026-04-12
//=========================================================
`ifndef LSQCOMMIT_AGENT_AGENT_DRIVER__SV
`define LSQCOMMIT_AGENT_AGENT_DRIVER__SV

class lsqcommit_agent_agent_driver  extends tcnt_driver_base#(virtual lsqcommit_agent_agent_interface,lsqcommit_agent_agent_cfg,lsqcommit_agent_agent_xaction);

    bit cached_sideband_valid;
    bit cached_pending_ptr_flag;
    bit [`MEMBLOCK_DUT_ROB_VALUE_W-1:0] cached_pending_ptr_value;
    bit cached_pending_st;
    bit cached_pending_mmio_ld;
    bit cached_is_store_exception;

    `uvm_component_utils(lsqcommit_agent_agent_driver)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern virtual task reset_phase(uvm_phase phase);
    extern task main_phase(uvm_phase phase);
    extern task send_pkt(lsqcommit_agent_agent_xaction tr);
    extern task drive_active_idle();
    extern task drive_idle(tcnt_dec_base::drv_mode_e drv_mode);
endclass:lsqcommit_agent_agent_driver

function lsqcommit_agent_agent_driver::new(string name, uvm_component parent);
    super.new(name,parent);
    cached_sideband_valid = 1'b0;
    cached_pending_ptr_flag = 1'b0;
    cached_pending_ptr_value = '0;
    cached_pending_st = 1'b0;
    cached_pending_mmio_ld = 1'b0;
    cached_is_store_exception = 1'b0;
endfunction:new

function void lsqcommit_agent_agent_driver::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task lsqcommit_agent_agent_driver::reset_phase(uvm_phase phase);

    super.reset_phase(phase);
    phase.raise_objection(this);
    cached_sideband_valid = 1'b0;
    cached_pending_ptr_flag = 1'b0;
    cached_pending_ptr_value = '0;
    cached_pending_st = 1'b0;
    cached_pending_mmio_ld = 1'b0;
    cached_is_store_exception = 1'b0;

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

task lsqcommit_agent_agent_driver::main_phase(uvm_phase phase);
    super.main_phase(phase);
    //while(1) begin
    if(this.cfg.sqr_sw==tcnt_dec_base::ON && this.cfg.drv_sw==tcnt_dec_base::ON) begin
        while(1) begin
            seq_item_port.try_next_item(req);
            if(req!=null) begin
                repeat(req.pre_pkt_gap) begin
                    @this.vif.drv_mp.drv_cb;
                    this.drive_active_idle();
                end
                @this.vif.drv_mp.drv_cb;
                this.send_pkt(req);
                repeat(req.post_pkt_gap) begin
                    @this.vif.drv_mp.drv_cb;
                    this.drive_active_idle();
                end
                seq_item_port.item_done();
            end
            else begin
                @this.vif.drv_mp.drv_cb;
                this.drive_active_idle();
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

task lsqcommit_agent_agent_driver::send_pkt(lsqcommit_agent_agent_xaction tr);
    vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingPtr_flag <= tr.io_ooo_to_mem_lsqio_pendingPtr_flag;
    vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingPtr_value <= tr.io_ooo_to_mem_lsqio_pendingPtr_value;
    vif.drv_mp.drv_cb.io_ooo_to_mem_flushSb <= tr.io_ooo_to_mem_flushSb;

    vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingMMIOld <= tr.io_ooo_to_mem_lsqio_pendingMMIOld;
    vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingst <= tr.io_ooo_to_mem_lsqio_pendingst;
    vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_scommit <= tr.io_ooo_to_mem_lsqio_scommit;
    vif.drv_mp.drv_cb.io_ooo_to_mem_isStoreException <= tr.io_ooo_to_mem_isStoreException;
    cached_sideband_valid = 1'b1;
    cached_pending_ptr_flag = tr.io_ooo_to_mem_lsqio_pendingPtr_flag;
    cached_pending_ptr_value = tr.io_ooo_to_mem_lsqio_pendingPtr_value;
    cached_pending_st = tr.io_ooo_to_mem_lsqio_pendingst;
    cached_pending_mmio_ld = tr.io_ooo_to_mem_lsqio_pendingMMIOld;
    cached_is_store_exception = tr.io_ooo_to_mem_isStoreException;

endtask:send_pkt

// 中文注释：active main_phase 的 no-item/pre-gap/post-gap 不属于 configured
// reset idle。无论 drv_mode 为何都保持最近一次 pending level，只清单拍 pulse。
task lsqcommit_agent_agent_driver::drive_active_idle();
    vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingMMIOld <=
        cached_sideband_valid ? cached_pending_mmio_ld : 1'b0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingst <=
        cached_sideband_valid ? cached_pending_st : 1'b0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingPtr_flag <=
        cached_sideband_valid ? cached_pending_ptr_flag : 1'b0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingPtr_value <=
        cached_sideband_valid ? cached_pending_ptr_value : '0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_isStoreException <=
        cached_sideband_valid ? cached_is_store_exception : 1'b0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_scommit <= '0;
    vif.drv_mp.drv_cb.io_ooo_to_mem_flushSb <= '0;
endtask:drive_active_idle

task lsqcommit_agent_agent_driver::drive_idle(tcnt_dec_base::drv_mode_e drv_mode);

    if(drv_mode==tcnt_dec_base::DRV_0) begin
        // 中文伪代码：active 气泡继续保持 ROB-head level sideband，
        // 只清 scommit/flushSb 这两个单拍 pulse。
        vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingMMIOld <=
            cached_sideband_valid ? cached_pending_mmio_ld : 1'b0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingst <=
            cached_sideband_valid ? cached_pending_st : 1'b0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_scommit <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingPtr_flag <=
            cached_sideband_valid ? cached_pending_ptr_flag : 1'b0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingPtr_value <=
            cached_sideband_valid ? cached_pending_ptr_value : '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_isStoreException <=
            cached_sideband_valid ? cached_is_store_exception : 1'b0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_flushSb <= '0;

    end
    else if(drv_mode==tcnt_dec_base::DRV_1) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingPtr_flag <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingPtr_value <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_flushSb <= '1;
        vif.drv_mp.drv_cb.io_ooo_to_mem_isStoreException <= '1;

    end
    else if(drv_mode==tcnt_dec_base::DRV_X) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingPtr_flag <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingPtr_value <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_flushSb <= 'x;
        vif.drv_mp.drv_cb.io_ooo_to_mem_isStoreException <= 'x;

    end
    else if(drv_mode==tcnt_dec_base::DRV_RAND) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingPtr_flag <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingPtr_value <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_flushSb <= $urandom;
        vif.drv_mp.drv_cb.io_ooo_to_mem_isStoreException <= $urandom;

    end
    else if(drv_mode==tcnt_dec_base::DRV_LST) begin
        vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingPtr_flag <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_lsqio_pendingPtr_value <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_flushSb <= '0;
        vif.drv_mp.drv_cb.io_ooo_to_mem_isStoreException <= '0;

    end

endtask:drive_idle

`endif
