//=========================================================
//File name    : fence_agent_agent_monitor.sv
//Author       : OpenAI_Codex
//Module name  : fence_agent_agent_monitor
//Discribution : fence_agent_agent_monitor : monitor
//Date         : 2026-04-12
//=========================================================
`ifndef FENCE_AGENT_AGENT_MONITOR__SV
`define FENCE_AGENT_AGENT_MONITOR__SV

class fence_agent_agent_monitor  extends tcnt_monitor_base#(virtual fence_agent_agent_interface,fence_agent_agent_cfg,fence_agent_agent_xaction);

    `uvm_component_utils(fence_agent_agent_monitor)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern task run_phase(uvm_phase phase);
    extern task mon_data();
endclass:fence_agent_agent_monitor

function fence_agent_agent_monitor::new(string name, uvm_component parent);
    super.new(name,parent);
endfunction:new

function void fence_agent_agent_monitor::build_phase(uvm_phase phase);
    super.build_phase(phase);
endfunction:build_phase

task fence_agent_agent_monitor::run_phase(uvm_phase phase);
    super.run_phase(phase);
    this.mon_data();
endtask:run_phase

task fence_agent_agent_monitor::mon_data();

    logic io_ooo_to_mem_sfence_valid   ;
    logic io_ooo_to_mem_sfence_bits_rs1;
    logic io_ooo_to_mem_sfence_bits_rs2;
    logic [49:0] io_ooo_to_mem_sfence_bits_addr;
    logic [15:0] io_ooo_to_mem_sfence_bits_id;
    logic io_ooo_to_mem_sfence_bits_hv ;
    logic io_ooo_to_mem_sfence_bits_hg ;

    logic io_ooo_to_mem_sfence_bits_flushPipe;
    fence_agent_agent_xaction  mon_tr;
    memblock_sync_pkg::dispatch_raw_sfence_t raw_sfence;
    longint unsigned sample_seq;
    longint unsigned event_seq;
    longint unsigned reset_epoch;
    while(1) begin
        @this.vif.mon_mp.mon_cb;
        io_ooo_to_mem_sfence_valid = this.vif.mon_mp.mon_cb.io_ooo_to_mem_sfence_valid;
        io_ooo_to_mem_sfence_bits_rs1 = this.vif.mon_mp.mon_cb.io_ooo_to_mem_sfence_bits_rs1;
        io_ooo_to_mem_sfence_bits_rs2 = this.vif.mon_mp.mon_cb.io_ooo_to_mem_sfence_bits_rs2;
        io_ooo_to_mem_sfence_bits_addr = this.vif.mon_mp.mon_cb.io_ooo_to_mem_sfence_bits_addr;
        io_ooo_to_mem_sfence_bits_id = this.vif.mon_mp.mon_cb.io_ooo_to_mem_sfence_bits_id;
        io_ooo_to_mem_sfence_bits_hv = this.vif.mon_mp.mon_cb.io_ooo_to_mem_sfence_bits_hv;
        io_ooo_to_mem_sfence_bits_hg = this.vif.mon_mp.mon_cb.io_ooo_to_mem_sfence_bits_hg;

        io_ooo_to_mem_sfence_bits_flushPipe = this.vif.mon_mp.mon_cb.io_ooo_to_mem_sfence_bits_flushPipe;

        if(this.cfg.xz_sw==tcnt_dec_base::ON && this.vif.rst_n==1'b1 &&
           memblock_sync_pkg::reset_backend_done==1'b1 &&
           !memblock_sync_pkg::l2tlb_reset_active()) begin
            `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_sfence_valid,io_ooo_to_mem_sfence_valid,1);
            if (io_ooo_to_mem_sfence_valid===1'b1) begin
                `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_sfence_bits_rs1,io_ooo_to_mem_sfence_bits_rs1,1);
                `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_sfence_bits_rs2,io_ooo_to_mem_sfence_bits_rs2,1);
                `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_sfence_bits_addr,io_ooo_to_mem_sfence_bits_addr,50);
                `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_sfence_bits_id,io_ooo_to_mem_sfence_bits_id,16);
                `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_sfence_bits_hv,io_ooo_to_mem_sfence_bits_hv,1);
                `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_sfence_bits_hg,io_ooo_to_mem_sfence_bits_hg,1);
                `TCNT_CHECK_SIG_XZ(io_ooo_to_mem_sfence_bits_flushPipe,io_ooo_to_mem_sfence_bits_flushPipe,1);
            end
        end
        if (this.vif.rst_n !== 1'b1 ||
            memblock_sync_pkg::reset_backend_done !== 1'b1) begin
            continue;
        end
        if (memblock_sync_pkg::l2tlb_reset_active()) begin
            // CSR monitor creates the epoch; this monitor only clears its
            // producer-local state and acknowledges that epoch.
            reset_epoch = memblock_sync_pkg::get_l2tlb_current_reset_epoch();
            memblock_sync_pkg::reset_l2tlb_fence_runtime_state(reset_epoch);

            // The CSR monitor may close the reset handshake later in this
            // same clocking-block callback.  Recheck in the NBA region so the
            // first post-reset sample is either seen by both producers or by
            // neither; do not leave its producer barrier half closed.
            uvm_wait_for_nba_region();
            #0;
            if (memblock_sync_pkg::l2tlb_reset_active()) begin
                continue;
            end
        end
        if(this.vif.rst_n==1'b1 &&
           memblock_sync_pkg::reset_backend_done==1'b1) begin
            // Fence is a same-edge producer: anchor to the CSR monitor's
            // sample, never advance the global clock here.
            memblock_sync_pkg::wait_for_l2tlb_sample_anchor($time, sample_seq);
            raw_sfence = memblock_sync_pkg::make_empty_raw_sfence();
            raw_sfence.sample_seq = sample_seq;
            raw_sfence.sample_time = $time;
            raw_sfence.reset_epoch = memblock_sync_pkg::get_l2tlb_current_reset_epoch();
            if (io_ooo_to_mem_sfence_valid===1'b1) begin
                // release 的 raw intake 已在前一个完整 sample 封闭。此时不能先
                // 分配 lifecycle event 再由 FIFO producer 报错，否则会留下无人
                // 消费的 event history。
                if (memblock_sync_pkg::dispatch_l2tlb_lookup_active &&
                    memblock_sync_pkg::l2tlb_raw_fence_intake_closed) begin
                    `uvm_fatal(get_type_name(),
                               $sformatf("SFENCE/HFENCE arrived after raw intake close sample=%0d epoch=%0d generation=%0d",
                                         sample_seq,
                                         raw_sfence.reset_epoch,
                                         memblock_sync_pkg::l2tlb_raw_fence_intake_closed_generation))
                end
                event_seq = memblock_sync_pkg::note_l2tlb_flush_event(
                    $time, memblock_sync_pkg::MEMBLOCK_L2TLB_REASON_FENCE);
                raw_sfence.valid = 1'b1;
                raw_sfence.rs1   = io_ooo_to_mem_sfence_bits_rs1;
                raw_sfence.rs2   = io_ooo_to_mem_sfence_bits_rs2;
                raw_sfence.addr  = io_ooo_to_mem_sfence_bits_addr;
                raw_sfence.id    = io_ooo_to_mem_sfence_bits_id;
                raw_sfence.hv    = io_ooo_to_mem_sfence_bits_hv;
                raw_sfence.hg    = io_ooo_to_mem_sfence_bits_hg;
                raw_sfence.lifecycle_event_seq = event_seq;
                raw_sfence.cycle = memblock_sync_pkg::get_dispatch_service_cycle();
                // 同步包按固定 topology 决定 dispatch-active 入 FIFO 或
                // no-dispatch 丢弃；不能由 monitor 猜测 CSR context 是否已经到达。
                memblock_sync_pkg::push_raw_sfence(raw_sfence);
            end
            // Even an empty sample must close the producer barrier.
            memblock_sync_pkg::mark_l2tlb_sample_producer_done(
                sample_seq, 2'b10);
        end
        //if(xxxTODOxxx==1'b1) begin
        //    mon_tr = fence_agent_agent_xaction::type_id::create("mon_tr");
        //    mon_tr.io_ooo_to_mem_sfence_valid = io_ooo_to_mem_sfence_valid;
        //    mon_tr.io_ooo_to_mem_sfence_bits_rs1 = io_ooo_to_mem_sfence_bits_rs1;
        //    mon_tr.io_ooo_to_mem_sfence_bits_rs2 = io_ooo_to_mem_sfence_bits_rs2;
        //    mon_tr.io_ooo_to_mem_sfence_bits_addr = io_ooo_to_mem_sfence_bits_addr;
        //    mon_tr.io_ooo_to_mem_sfence_bits_id = io_ooo_to_mem_sfence_bits_id;
        //    mon_tr.io_ooo_to_mem_sfence_bits_hv = io_ooo_to_mem_sfence_bits_hv;
        //    mon_tr.io_ooo_to_mem_sfence_bits_hg = io_ooo_to_mem_sfence_bits_hg;

        //    mon_tr.channel_id = this.cfg.channel_id;
        //    mon_tr.unpack();
        //    this.mon_item_port.write(mon_tr);
        //end
    end
endtask:mon_data

`endif
