//=========================================================
//File name    : redirect_agent_agent_driver.sv
//Author       : OpenAI_Codex
//Module name  : redirect_agent_agent_driver
//Discribution : redirect_agent_agent_driver : driver
//Date         : 2026-04-12
//=========================================================
`ifndef REDIRECT_AGENT_AGENT_DRIVER__SV
`define REDIRECT_AGENT_AGENT_DRIVER__SV

class redirect_agent_agent_driver  extends tcnt_driver_base#(virtual redirect_agent_agent_interface,redirect_agent_agent_cfg,redirect_agent_agent_xaction);

    `uvm_component_utils(redirect_agent_agent_driver)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern virtual task reset_phase(uvm_phase phase);
    extern task drive_backend_item(redirect_agent_agent_xaction tr);
    extern task main_phase(uvm_phase phase);
    extern task send_pkt(redirect_agent_agent_xaction tr);
    extern task drive_idle(tcnt_dec_base::drv_mode_e drv_mode);
endclass:redirect_agent_agent_driver

function redirect_agent_agent_driver::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void redirect_agent_agent_driver::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task redirect_agent_agent_driver::reset_phase(uvm_phase phase);

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

task redirect_agent_agent_driver::main_phase(uvm_phase phase);
    super.main_phase(phase);
    //while(1) begin
    if(this.cfg.sqr_sw==tcnt_dec_base::ON && this.cfg.drv_sw==tcnt_dec_base::ON) begin
        while(1) begin
            seq_item_port.try_next_item(req);
            if(req!=null && req.backend_sync) begin
                drive_backend_item(req);
                seq_item_port.item_done();
            end
            else if(req!=null) begin
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

task redirect_agent_agent_driver::send_pkt(redirect_agent_agent_xaction tr);
    vif.drv_mp.drv_cb.io_redirect_valid <= tr.io_redirect_valid;
    vif.drv_mp.drv_cb.io_redirect_bits_level <= tr.io_redirect_bits_level;
    vif.drv_mp.drv_cb.io_redirect_bits_isVlsException <= tr.io_redirect_bits_isVlsException;
    vif.drv_mp.drv_cb.io_redirect_bits_robIdx_flag <= tr.io_redirect_bits_robIdx_flag;
    vif.drv_mp.drv_cb.io_redirect_bits_robIdx_value <= tr.io_redirect_bits_robIdx_value;

endtask:send_pkt

task redirect_agent_agent_driver::drive_idle(tcnt_dec_base::drv_mode_e drv_mode);

    if(drv_mode==tcnt_dec_base::DRV_0) begin
        vif.drv_mp.drv_cb.io_redirect_valid <= '0;
        vif.drv_mp.drv_cb.io_redirect_bits_level <= '0;
        vif.drv_mp.drv_cb.io_redirect_bits_isVlsException <= '0;
        vif.drv_mp.drv_cb.io_redirect_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_redirect_bits_robIdx_value <= '0;

    end
    else if(drv_mode==tcnt_dec_base::DRV_1) begin
        vif.drv_mp.drv_cb.io_redirect_valid <= '1;
        vif.drv_mp.drv_cb.io_redirect_bits_level <= '1;
        vif.drv_mp.drv_cb.io_redirect_bits_isVlsException <= '1;
        vif.drv_mp.drv_cb.io_redirect_bits_robIdx_flag <= '1;
        vif.drv_mp.drv_cb.io_redirect_bits_robIdx_value <= '1;

    end
    else if(drv_mode==tcnt_dec_base::DRV_X) begin
        vif.drv_mp.drv_cb.io_redirect_valid <= 'x;
        vif.drv_mp.drv_cb.io_redirect_bits_level <= 'x;
        vif.drv_mp.drv_cb.io_redirect_bits_isVlsException <= 'x;
        vif.drv_mp.drv_cb.io_redirect_bits_robIdx_flag <= 'x;
        vif.drv_mp.drv_cb.io_redirect_bits_robIdx_value <= 'x;

    end
    else if(drv_mode==tcnt_dec_base::DRV_RAND) begin
        vif.drv_mp.drv_cb.io_redirect_valid <= $urandom;
        vif.drv_mp.drv_cb.io_redirect_bits_level <= $urandom;
        vif.drv_mp.drv_cb.io_redirect_bits_isVlsException <= $urandom;
        vif.drv_mp.drv_cb.io_redirect_bits_robIdx_flag <= $urandom;
        vif.drv_mp.drv_cb.io_redirect_bits_robIdx_value <= $urandom;

    end
    else if(drv_mode==tcnt_dec_base::DRV_LST) begin
        vif.drv_mp.drv_cb.io_redirect_valid <= '0;
        vif.drv_mp.drv_cb.io_redirect_bits_level <= '0;
        vif.drv_mp.drv_cb.io_redirect_bits_isVlsException <= '0;
        vif.drv_mp.drv_cb.io_redirect_bits_robIdx_flag <= '0;
        vif.drv_mp.drv_cb.io_redirect_bits_robIdx_value <= '0;

    end

endtask:drive_idle

// 抽象职责：保留一个完整发布单元，逐物理边沿服务准备、取消、主帧和紧邻尾拍。
// 主帧回执不依赖item_done；等待期间仍逐拍输出idle并合成独立flushSb。
task redirect_agent_agent_driver::drive_backend_item(redirect_agent_agent_xaction tr);
    memblock_sync_pkg::backend_cycle_release_t release_state;
    memblock_sync_pkg::backend_cycle_projection_t projection;
    redirect_agent_agent_xaction pkt;
    longint unsigned edge_sample;
    bit main_driven;
    bit tail;

    main_driven = 0;
    pkt = redirect_agent_agent_xaction::type_id::create("backend_projection");
    memblock_sync_pkg::acknowledge_backend_ready(tr.backend_cycle.frame_id, 1);
    forever begin
        @this.vif.drv_mp.drv_cb;
        if (vif.rst_n !== 1'b1 || memblock_sync_pkg::l2tlb_reset_active()) begin
            this.drive_idle(tcnt_dec_base::DRV_0);
            if (memblock_sync_pkg::backend_release_by_id.exists(tr.backend_cycle.frame_id))
                memblock_sync_pkg::backend_release_by_id[tr.backend_cycle.frame_id].finished[1] = 1;
            return;
        end
        memblock_sync_pkg::wait_for_l2tlb_sample_anchor($time, edge_sample);
        release_state = memblock_sync_pkg::get_backend_release(tr.backend_cycle.frame_id);
        if (!release_state.valid || release_state.canceled ||
            tr.backend_cycle.reset_epoch != memblock_sync_pkg::get_l2tlb_current_reset_epoch()) begin
            this.drive_idle(tcnt_dec_base::DRV_0);
            if (memblock_sync_pkg::backend_release_by_id.exists(tr.backend_cycle.frame_id))
                memblock_sync_pkg::backend_release_by_id[tr.backend_cycle.frame_id].finished[1] = 1;
            return;
        end
        tail = main_driven && tr.backend_cycle.has_tail;
        if (release_state.committed && edge_sample == release_state.edge_sample + (tail ? 1 : 0)) begin
            projection = tail ? tr.backend_cycle.tail : tr.backend_cycle.head;
            pkt.io_redirect_valid = projection.redirect_valid;
            pkt.io_redirect_bits_level = projection.redirect_level;
            pkt.io_redirect_bits_isVlsException = projection.redirect_vls;
            pkt.io_redirect_bits_robIdx_flag = projection.redirect_flag;
            pkt.io_redirect_bits_robIdx_value = projection.redirect_value;
            this.send_pkt(pkt);
            memblock_sync_pkg::acknowledge_backend_drive(tr.backend_cycle.frame_id, 1, tail, edge_sample);
            if (tail || !tr.backend_cycle.has_tail) begin
                if (memblock_sync_pkg::backend_release_by_id.exists(tr.backend_cycle.frame_id))
                memblock_sync_pkg::backend_release_by_id[tr.backend_cycle.frame_id].finished[1] = 1;
                return;
            end
            main_driven = 1;
        end else begin
            if (release_state.committed && edge_sample > release_state.edge_sample + (tail ? 1 : 0)) begin
                `uvm_fatal("BACKEND_FRAME", "committed backend projection missed its physical edge")
            end
            this.drive_idle(tcnt_dec_base::DRV_0);
        end
    end
endtask

`endif
