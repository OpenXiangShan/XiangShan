//=========================================================
//File name    : memblock_dispatch_real_cancel_reconcile_vseq.sv
//Author       : OpenAI_Codex
//Module name  : memblock_dispatch_real_cancel_reconcile_vseq
//Discribution : real DUT LSQ redirect-cancel reconcile scenario
//Date         : 2026-07-22
//=========================================================
`ifndef MEMBLOCK_DISPATCH_REAL_CANCEL_RECONCILE_VSEQ__SV
`define MEMBLOCK_DISPATCH_REAL_CANCEL_RECONCILE_VSEQ__SV

class memblock_dispatch_real_cancel_reconcile_vseq extends memblock_dispatch_real_smoke_vseq;

    // 中文注释：redirect_injected 只记录本 directed 场景是否完成真实接口注入；
    // background_responders_done 只在三个 responder 都从无 inflight 边界自然返回后置位。
    // 两者不参与公共 pass/fail、cancel 计数或 global-stop 判定。
    bit redirect_injected;
    bit background_responders_done;
    common_data_transaction data;
    virtual lintsissue_agent_agent_interface service_vif;

    `uvm_object_utils(memblock_dispatch_real_cancel_reconcile_vseq)

    extern function new(string name = "memblock_dispatch_real_cancel_reconcile_vseq");
    extern virtual task pre_body();
    extern virtual task post_body();
    extern virtual task body();
    extern virtual function void ensure_service_vif();
    extern virtual task start_background_responders();
    extern virtual task start_core_dispatch_flow();
    extern virtual task drive_directed_redirect_when_ready();
    extern virtual task wait_for_background_responders();

endclass:memblock_dispatch_real_cancel_reconcile_vseq

function memblock_dispatch_real_cancel_reconcile_vseq::new(string name = "memblock_dispatch_real_cancel_reconcile_vseq");
    super.new(name);
    // 中文注释：该场景依赖后台 responder 的自然退出，automatic objection 覆盖完整
    // body 生命周期，避免父类手工 raise/drop 在同一仿真时刻提前结束 main phase。
    set_automatic_phase_objection(1'b1);
    redirect_injected = 1'b0;
    background_responders_done = 1'b0;
    data = null;
    service_vif = null;
endfunction:new

task memblock_dispatch_real_cancel_reconcile_vseq::pre_body();
    uvm_phase phase;

    // UVM 1.2 default-sequence startup writes the standard phase DAP.  The
    // deprecated public starting_phase alias can remain null in this library.
    phase = get_starting_phase();
    if (phase == null) begin
        `uvm_fatal(get_type_name(),
                   "cancel reconcile vseq requires a phase-owned default-sequence start")
    end
    phase.phase_done.set_drain_time(this, 1us);
endtask:pre_body

task memblock_dispatch_real_cancel_reconcile_vseq::post_body();
    // automatic phase objection 由 UVM 配对释放；这里仅保证场景 active 标志收口。
    memblock_sync_pkg::dispatch_real_smoke_active = 1'b0;
endtask:post_body

task memblock_dispatch_real_cancel_reconcile_vseq::body();
    require_real_smoke_sqr();
    seq_csr_common::init();

    data = common_data_transaction::get();
    if (data == null) begin
        `uvm_fatal(get_type_name(), "failed to get common_data_transaction")
    end

    redirect_injected = 1'b0;
    background_responders_done = 1'b0;
    memblock_sync_pkg::dispatch_real_smoke_active = 1'b1;
    `uvm_info(get_type_name(), "real cancel reconcile virtual sequence start", UVM_LOW)

    // 中文注释：派生 vseq 同样在 fork DCache/Uncache responder 前成为本 testcase 的唯一
    // shared memory lifecycle owner，不能依赖上一次场景残留的 static initialized 标志。
    initialize_shared_memory_store();

    fork : cancel_reconcile_background_fork
        start_background_responders();
    join_none

    start_core_dispatch_flow();
    wait_for_background_responders();

    memblock_sync_pkg::dispatch_real_smoke_active = 1'b0;
    `uvm_info(get_type_name(), "real cancel reconcile virtual sequence completed", UVM_LOW)
endtask:body

function void memblock_dispatch_real_cancel_reconcile_vseq::ensure_service_vif();
    if (service_vif != null) begin
        return;
    end
    if (!uvm_config_db#(virtual lintsissue_agent_agent_interface)::get(null,
                                                                      get_full_name(),
                                                                      "vif",
                                                                      service_vif) &&
        !uvm_config_db#(virtual lintsissue_agent_agent_interface)::get(null,
                                                                      "uvm_test_top.env.u_lintsissue_agent_agent*",
                                                                      "vif",
                                                                      service_vif)) begin
        `uvm_fatal(get_type_name(), "failed to get lintsissue service clock vif")
    end
endfunction:ensure_service_vif

task memblock_dispatch_real_cancel_reconcile_vseq::start_background_responders();
    background_responders_done = 1'b0;
    super.start_background_responders();
    // 父 task 使用 fork/join；返回即表示 DCache/SBuffer/redirect responder 均已自然退出。
    background_responders_done = 1'b1;
endtask:start_background_responders

task memblock_dispatch_real_cancel_reconcile_vseq::start_core_dispatch_flow();
    memblock_lsqenq_dispatch_base_sequence                   lsqenq_seq;
    memblock_issue_dispatch_base_sequence                    issue_seq;
    memblock_lsqcommit_dispatch_base_sequence                lsqcommit_seq;
    memblock_l2tlb_base_sequence                             l2tlb_seq;
    memblock_main_dispatch_cancel_reconcile_sequence         main_seq;

    // 中文注释：五个真实 sequence 与 directed redirect barrier 同步运行；所有
    // sequence 都由 virtual sequencer 使用 uvm_do_on 调度，不直接调用 start()。
    fork
        begin : start_cancel_lsqenq_sequence
            `uvm_do_on(lsqenq_seq, p_sequencer.lsqenq_sqr)
        end
        begin : start_cancel_issue_sequence
            `uvm_do_on(issue_seq, p_sequencer.lintsissue_sqr)
        end
        begin : start_cancel_lsqcommit_sequence
            `uvm_do_on(lsqcommit_seq, p_sequencer.lsqcommit_sqr)
        end
        begin : start_cancel_l2tlb_sequence
            `uvm_do_on(l2tlb_seq, p_sequencer.L2tlb_sqr)
        end
        begin : start_cancel_main_sequence
            `uvm_do_on(main_seq, p_sequencer)
        end
        begin : drive_cancel_redirect
            drive_directed_redirect_when_ready();
        end
    join

    if (!redirect_injected) begin
        `uvm_fatal(get_type_name(), "directed redirect was not injected")
    end
    if (data.cancel_reconcile_match_count == 0 ||
        data.cancel_reconcile_lq_nonzero_match_count == 0 ||
        data.cancel_reconcile_sq_nonzero_match_count == 0) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("cancel reconcile coverage missing: all/lq_nonzero/sq_nonzero=%0d/%0d/%0d",
                             data.cancel_reconcile_match_count,
                             data.cancel_reconcile_lq_nonzero_match_count,
                             data.cancel_reconcile_sq_nonzero_match_count))
    end
endtask:start_core_dispatch_flow

task memblock_dispatch_real_cancel_reconcile_vseq::drive_directed_redirect_when_ready();
    int unsigned wait_cycles;

    ensure_service_vif();
    wait_cycles = 0;
    forever begin
        status_transaction anchor_status;
        status_transaction victim_load_status;
        status_transaction victim_store_status;
        memblock_redirect_payload_t redirect;

        @(negedge service_vif.clk);
        if (service_vif.rst_n !== 1'b1 ||
            memblock_sync_pkg::reset_backend_done !== 1'b1) begin
            continue;
        end

        wait_cycles++;
        if (wait_cycles > 256) begin
            `uvm_fatal(get_type_name(),
                       "timeout waiting for uid1/uid2 DUT-visible LSQ reservations")
        end
        if (data.is_global_stop_requested()) begin
            `uvm_fatal(get_type_name(), "global stop arrived before directed redirect injection")
        end
        if (!data.main_table_ready) begin
            continue;
        end
        if (data.main_trans_num != 3) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("cancel reconcile main table must contain 3 entries, got %0d",
                                 data.main_trans_num))
        end

        anchor_status = data.get_status(0);
        victim_load_status = data.get_status(1);
        victim_store_status = data.get_status(2);

        // 中文注释：victim 必须在真实 issue/writeback/deq 前被 redirect 命中；否则本场景
        // 不能证明非零 LQ/SQ cancel，直接失败而不是退化为零计数场景。
        if (victim_load_status.load_dispatched ||
            victim_load_status.load_writeback ||
            victim_load_status.writeback ||
            victim_load_status.lsq_deq ||
            victim_load_status.terminal_done) begin
            `uvm_fatal(get_type_name(), "uid1 load victim progressed before redirect injection")
        end
        if (victim_store_status.sta_dispatched ||
            victim_store_status.std_dispatched ||
            victim_store_status.sta_writeback ||
            victim_store_status.std_writeback ||
            victim_store_status.writeback ||
            victim_store_status.lsq_deq ||
            victim_store_status.terminal_done) begin
            `uvm_fatal(get_type_name(), "uid2 store victim progressed before redirect injection")
        end

        if (!(victim_load_status.active_lq_mapped &&
              victim_load_status.lsq_reservation_sample_valid &&
              victim_load_status.lsq_reservation_state == MEMBLOCK_LSQ_RESERVATION_DUT_VISIBLE &&
              victim_store_status.active_sq_mapped &&
              victim_store_status.lsq_reservation_sample_valid &&
              victim_store_status.lsq_reservation_state == MEMBLOCK_LSQ_RESERVATION_DUT_VISIBLE)) begin
            continue;
        end

        if (data.active_redirect.valid ||
            data.has_pending_redirect_drive() ||
            data.flush_in_progress ||
            data.redirect_phase != MEMBLOCK_REDIRECT_PHASE_IDLE) begin
            `uvm_fatal(get_type_name(), "redirect state was not idle before directed injection")
        end

        redirect = '{default:'0};
        redirect.valid = 1'b1;
        redirect.flush_itself = 1'b0;
        redirect.level = 1'b0;
        redirect.rob_key = anchor_status.get_rob_key();
        if (!rob_order_util::rob_need_flush(victim_load_status.get_rob_key(), redirect) ||
            !rob_order_util::rob_need_flush(victim_store_status.get_rob_key(), redirect)) begin
            `uvm_fatal(get_type_name(), "directed redirect does not cover both younger victims")
        end

        // request_redirect_flush() 建立 framework record/freeze；redirect responder 随后
        // 从 drive queue 取同一 payload 并真实驱动 DUT。这里不伪造 monitor sideband。
        data.request_redirect_flush(redirect);
        data.push_redirect_drive(redirect);
        redirect_injected = 1'b1;
        `uvm_info(get_type_name(),
                  $sformatf("injected flushAfter redirect at ROB=%0d/%0d after %0d service cycles",
                            redirect.rob_key.flag,
                            redirect.rob_key.value,
                            wait_cycles),
                  UVM_LOW)
        return;
    end
endtask:drive_directed_redirect_when_ready

task memblock_dispatch_real_cancel_reconcile_vseq::wait_for_background_responders();
    ensure_service_vif();
    for (int unsigned wait_cycles = 0; wait_cycles < 256; wait_cycles++) begin
        if (background_responders_done) begin
            return;
        end
        @(negedge service_vif.clk);
    end
    if (!background_responders_done) begin
        `uvm_fatal(get_type_name(), "background responders did not exit within 256 service cycles")
    end
endtask:wait_for_background_responders

`endif
