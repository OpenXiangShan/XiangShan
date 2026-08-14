//=========================================================
//File name    : memblock_lsqcommit_dispatch_base_sequence.sv
//Author       : OpenAI_Codex
//Module name  : memblock_lsqcommit_dispatch_base_sequence
//Discribution : lsqcommit pendingPtr dispatch driver sequence
//Date         : 2026-05-18
//=========================================================
`ifndef MEMBLOCK_LSQCOMMIT_DISPATCH_BASE_SEQUENCE__SV
`define MEMBLOCK_LSQCOMMIT_DISPATCH_BASE_SEQUENCE__SV

class memblock_lsqcommit_dispatch_base_sequence extends lsqcommit_agent_agent_default_sequence;

    common_data_transaction data;
    lsq_commit_handler      commit_handler;
    virtual lsqcommit_agent_agent_interface lsqcommit_vif;

    bit          enable;
    int unsigned no_progress_warn_cycles;

    `uvm_object_utils(memblock_lsqcommit_dispatch_base_sequence)

    extern function new(string name = "memblock_lsqcommit_dispatch_base_sequence");
    extern virtual task pre_body();
    extern virtual task body();
    extern virtual task drive_lsqcommit_loop();
    extern virtual task send_lsqcommit_cycle(input int unsigned cycle_idx,
                                             output bit has_progress,
                                             output bit terminal_idle_published);
    extern task wait_for_main_table();
    extern task wait_clock_tick();
    extern function void configure_from_plus();
    extern function void ensure_helpers();
    extern function void ensure_lsqcommit_vif();

endclass:memblock_lsqcommit_dispatch_base_sequence

function memblock_lsqcommit_dispatch_base_sequence::new(string name = "memblock_lsqcommit_dispatch_base_sequence");
    super.new(name);
    lsqcommit_vif = null;
    enable = 1'b0;
    no_progress_warn_cycles = 10000;
endfunction:new

task memblock_lsqcommit_dispatch_base_sequence::pre_body();
    super.pre_body();
endtask:pre_body

task memblock_lsqcommit_dispatch_base_sequence::body();
    seq_csr_common::init();
    configure_from_plus();
    if (!enable) begin
        `uvm_info(get_type_name(), "MEMBLOCK_LSQCOMMIT_SEQ_EN=0, LSQ commit dispatch sequence stays idle", UVM_LOW)
        return;
    end
    ensure_helpers();
    wait_for_main_table();
    drive_lsqcommit_loop();
endtask:body

task memblock_lsqcommit_dispatch_base_sequence::drive_lsqcommit_loop();
    int unsigned cycle_idx;
    int unsigned idle_count;

    cycle_idx = 0;
    idle_count = 0;
    forever begin
        bit has_progress;
        bit terminal_idle_published;

        send_lsqcommit_cycle(cycle_idx, has_progress, terminal_idle_published);
        cycle_idx++;
        if (has_progress) begin
            idle_count = 0;
        end else begin
            idle_count++;
            if (no_progress_warn_cycles != 0 &&
                idle_count >= no_progress_warn_cycles) begin
                `uvm_warning(get_type_name(),
                             $sformatf("no LSQ commit/flushSb progress for %0d cycles: cycle=%0d terminal_done_uid=%0d main_trans_num=%0d flushsb_q=%0d waiting_empty=%0d active_req_valid=%0d",
                                       idle_count,
                                       cycle_idx,
                                       data.dispatch_progress.terminal_done_uid,
                                       data.main_trans_num,
                                       data.flushsb_req_q.size(),
                                       data.flushsb_waiting_empty,
                                       data.active_flushsb_req_valid))
                idle_count = 0;
            end
        end
        if (data.is_global_stop_requested() &&
            !data.flushsb_request_pending() &&
            terminal_idle_published) begin
            `uvm_info(get_type_name(),
                      $sformatf("stop LSQ commit loop after publishing terminal idle at cycle=%0d",
                                cycle_idx),
                      UVM_LOW)
            break;
        end
    end
endtask:drive_lsqcommit_loop

task memblock_lsqcommit_dispatch_base_sequence::send_lsqcommit_cycle(input int unsigned cycle_idx,
                                                                output bit has_progress,
                                                                output bit terminal_idle_published);
    lsqcommit_agent_agent_xaction tr;
    memblock_uid_t                commit_uids[$];
    memblock_flushsb_req_t        flushsb_req;
    bit                           has_commit;
    bit                           has_fault_head;
    memblock_uid_t                fault_uid;
    bit                           has_control_head;
    memblock_uid_t                control_uid;
    bit                           has_flushsb_progress;

    has_commit = 1'b0;
    has_fault_head = 1'b0;
    fault_uid = 0;
    has_control_head = 1'b0;
    control_uid = 0;
    has_flushsb_progress = 1'b0;
    has_progress = 1'b0;
    terminal_idle_published = 1'b0;
    data.warn_flushsb_timeout_if_needed(seq_csr_common::get_flushsb_timeout());
    if (data.issue_blocked_by_global_flush()) begin
        tr = lsqcommit_agent_agent_xaction::type_id::create($sformatf("lsqcommit_dispatch_idle_tr_%0d", cycle_idx));
        commit_handler.clear_lsqcommit_xaction(tr);
        start_item(tr);
        finish_item(tr);
        has_progress = data.flushsb_request_pending();
        return;
    end
    commit_handler.build_lsqcommit_xaction(tr, commit_uids, has_commit,
                                           has_fault_head, fault_uid);
    if (!has_commit && !has_fault_head) begin
        has_control_head = commit_handler.select_control_head_candidate(control_uid);
    end
    tr.set_name($sformatf("lsqcommit_dispatch_tr_%0d", cycle_idx));
    if (data.try_pop_flushsb_request(flushsb_req)) begin
        tr.io_ooo_to_mem_flushSb = 1'b1;
        data.mark_flushsb_driven(flushsb_req,
                                 memblock_sync_pkg::get_dispatch_service_cycle());
        has_flushsb_progress = 1'b1;
    end

    start_item(tr);
    finish_item(tr);

    if (has_commit) begin
        commit_handler.mark_rob_commit_batch(commit_uids);
    end else if (has_fault_head) begin
        commit_handler.mark_fault_rob_commit_uid(fault_uid);
    end else if (has_control_head) begin
        void'(commit_handler.mark_control_rob_commit_uid(control_uid));
    end
    has_progress = has_commit ||
                   has_fault_head ||
                   has_control_head ||
                   has_flushsb_progress ||
                   data.flushsb_busy();
    terminal_idle_published = data.is_global_stop_requested() &&
                              commit_handler.commit_cursor_uid == data.main_trans_num &&
                              commit_handler.modeled_rob_deq_ptr_initialized &&
                              !commit_handler.modeled_head_valid &&
                              !commit_handler.fault_head_waiting &&
                              !data.flushsb_request_pending() &&
                              !data.cancel_reconcile_pending() &&
                              !data.redirect_sample_anchor_pending() &&
                              !data.cancel_snapshot_buffer_pending() &&
                              !tr.io_ooo_to_mem_lsqio_pendingMMIOld &&
                              !tr.io_ooo_to_mem_lsqio_pendingst &&
                              tr.io_ooo_to_mem_lsqio_scommit == '0 &&
                              !tr.io_ooo_to_mem_flushSb;
endtask:send_lsqcommit_cycle

task memblock_lsqcommit_dispatch_base_sequence::wait_for_main_table();
    int unsigned wait_count;

    wait_count = 0;
    while (!data.main_table_ready) begin
        if (no_progress_warn_cycles != 0 &&
            wait_count != 0 &&
            (wait_count % no_progress_warn_cycles) == 0) begin
            `uvm_warning(get_type_name(),
                         $sformatf("still waiting for main table before LSQ commit drive: wait_count=%0d main_trans_num=%0d next_uid=%0d",
                                   wait_count,
                                   data.main_trans_num,
                                   data.next_uid))
        end
        wait_clock_tick();
        wait_count++;
    end
endtask:wait_for_main_table

task memblock_lsqcommit_dispatch_base_sequence::wait_clock_tick();
    ensure_lsqcommit_vif();
    @(posedge lsqcommit_vif.clk);
endtask:wait_clock_tick

function void memblock_lsqcommit_dispatch_base_sequence::configure_from_plus();
    enable = seq_csr_common::get_lsqcommit_seq_en();
    no_progress_warn_cycles = seq_csr_common::get_active_seq_no_progress_warn_cycles();
endfunction:configure_from_plus

function void memblock_lsqcommit_dispatch_base_sequence::ensure_helpers();
    data = common_data_transaction::get();
    if (commit_handler == null) begin
        commit_handler = lsq_commit_handler::get();
    end
    commit_handler.reset_lsqcommit_runtime_state();
    commit_handler.bind_lsq_ctrl(lsq_ctrl_model::get());
    if (data == null || commit_handler == null) begin
        `uvm_fatal(get_type_name(), "failed to initialize lsqcommit helpers")
    end
    ensure_lsqcommit_vif();
endfunction:ensure_helpers

function void memblock_lsqcommit_dispatch_base_sequence::ensure_lsqcommit_vif();
    if (lsqcommit_vif != null) begin
        return;
    end
    if (!uvm_config_db#(virtual lsqcommit_agent_agent_interface)::get(null, get_full_name(), "vif", lsqcommit_vif) &&
        !uvm_config_db#(virtual lsqcommit_agent_agent_interface)::get(null, "uvm_test_top.env.u_lsqcommit_agent_agent*", "vif", lsqcommit_vif)) begin
        `uvm_fatal(get_type_name(), "LSQ commit virtual interface is not set")
    end
endfunction:ensure_lsqcommit_vif

`endif
