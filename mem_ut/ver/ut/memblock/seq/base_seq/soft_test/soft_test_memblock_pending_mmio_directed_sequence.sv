//=========================================================
//File name    : soft_test_memblock_pending_mmio_directed_sequence.sv
//Author       : OpenAI_Codex
//Module name  : soft_test_memblock_pending_mmio_directed_sequence
//Discribution : software-only pending-MMIO owner-contract checks
//Date         : 2026-07-22
//=========================================================
`ifndef SOFT_TEST_MEMBLOCK_PENDING_MMIO_DIRECTED_SEQUENCE__SV
`define SOFT_TEST_MEMBLOCK_PENDING_MMIO_DIRECTED_SEQUENCE__SV

// 中文注释：仅在 ambiguous/new-owner directed 场景的单次 resolver 调用期间
// 捕获预期 MMIO_RESOLVE fatal；ID相同但消息原因不同的 fatal 仍继续抛出。
class memblock_pending_mmio_expected_fatal_catcher extends uvm_report_catcher;
    int unsigned caught_count;
    string expected_message_pattern;

    function new(
        string name = "memblock_pending_mmio_expected_fatal_catcher",
        string message_pattern = "*cannot prove LOAD MMIO stale ownership*"
    );
        super.new(name);
        caught_count = 0;
        expected_message_pattern = message_pattern;
    endfunction:new

    virtual function action_e catch();
        if (get_severity() == UVM_FATAL &&
            get_id() == "MMIO_RESOLVE" &&
            uvm_pkg::uvm_is_match(expected_message_pattern, get_message())) begin
            caught_count++;
            return CAUGHT;
        end
        return THROW;
    endfunction:catch
endclass:memblock_pending_mmio_expected_fatal_catcher

class memblock_pending_mmio_rob_expected_fatal_catcher extends uvm_report_catcher;
    int unsigned caught_count;

    function new(string name = "memblock_pending_mmio_rob_expected_fatal_catcher");
        super.new(name);
        caught_count = 0;
    endfunction:new

    virtual function action_e catch();
        if (get_severity() == UVM_FATAL &&
            get_id() == "ROB_ADMISSION_WINDOW") begin
            caught_count++;
            return CAUGHT;
        end
        return THROW;
    endfunction:catch
endclass:memblock_pending_mmio_rob_expected_fatal_catcher

class soft_test_memblock_pending_mmio_directed_sequence extends
    soft_test_memblock_dispatch_smoke_sequence;

    `uvm_object_utils(soft_test_memblock_pending_mmio_directed_sequence)
    localparam int unsigned MEMBLOCK_LOAD_MMIO_TEST_MAX_SAMPLE_OFFSET = 2;

    extern function new(
        string name = "soft_test_memblock_pending_mmio_directed_sequence"
    );
    extern virtual task body();
    extern virtual task ensure_directed_helpers();
    extern virtual task reset_directed_owner_state();
    extern virtual task wait_for_dut_sample_watermark(
        input longint unsigned target_sample_seq,
        input string scenario_name
    );
    extern virtual task run_inactive_head_pointer_scenario();
    extern virtual task run_directed_tag_scenario();
    extern virtual task run_monitor_raw_scenario();
    extern virtual task run_uncache_retry_boost_scenario();
    extern virtual task run_rollback_xmr_path_scenario();
    extern virtual task run_rollback_subtype_decode_scenario();
    extern virtual task run_rollback_cross_sample_cache_scenario();
    extern virtual task run_stale_load_overlap_scenario(input bit one_cycle_late);
    extern virtual task run_surviving_load_overlap_scenario(input bit one_cycle_late);
    extern virtual task run_mixed_wrap_overlap_fatal_scenario(input bit one_cycle_late);
    extern virtual task run_new_owner_r_overlap_fatal_scenario();
    extern virtual task run_incompatible_r_overlap_fatal_scenario();
    extern virtual task run_deleted_load_stale_scenario(input int unsigned sample_offset);
    extern virtual task run_r2_active_survivor_current_scenario();
    extern virtual task run_r2_survivor_with_deleted_history_fatal_scenario();
    extern virtual task run_r3_deleted_load_fatal_scenario();
    extern virtual task run_r2_no_history_fatal_scenario();
    extern virtual task run_new_owner_overlap_fatal_scenario();
    extern virtual task run_r2_wrap_ambiguity_fatal_scenario();
    extern virtual task run_r2_non_load_fatal_scenario();
    extern virtual task run_fault_head_suppress_scenario();
    extern virtual task run_commit_batch_redirect_survivor_scenario();
    extern virtual task run_commit_batch_redirect_mixed_cancel_scenario();
    extern virtual task run_global_stop_raw_drain_scenario();
    extern virtual task build_load_store_table();
    extern virtual task build_load_table(input int unsigned load_count);
    extern virtual task build_store_table();
    extern virtual task run_rob_wrap_admission_fatal_scenario(input string scenario_name);
    extern virtual task establish_redirect_anchor(
        input memblock_redirect_payload_t redirect,
        input string scenario_name,
        output longint unsigned anchor_sample_seq
    );
    extern virtual task prepare_redirect_deleted_owner_window(
        input memblock_uid_t victim_uids[$],
        input memblock_redirect_payload_t redirect,
        input string scenario_name,
        output longint unsigned anchor_sample_seq
    );
    extern virtual task apply_load_mmio_raw(
        input bit [MEMBLOCK_ROB_VALUE_W-1:0] rob_value,
        input longint unsigned sample_seq,
        input string scenario_name,
        input bit expect_fatal = 1'b0,
        input string expected_fatal_pattern = ""
    );
    extern virtual task check_load_head_sideband(input memblock_uid_t expected_uid,
                                                 input string scenario_name);
    extern virtual function memblock_wb_event_t make_uncache_nack_event(
        input memblock_uid_t uid,
        input memblock_rollback_subtype_e subtype = MEMBLOCK_ROLLBACK_SUBTYPE_UNCACHE_NACK,
        input bit flush_itself = 1'b1,
        input bit has_dynamic_epoch = 1'b1
    );

endclass:soft_test_memblock_pending_mmio_directed_sequence

function soft_test_memblock_pending_mmio_directed_sequence::new(
    string name = "soft_test_memblock_pending_mmio_directed_sequence"
);
    super.new(name);
endfunction:new

task soft_test_memblock_pending_mmio_directed_sequence::body();
    ensure_directed_helpers();
    run_inactive_head_pointer_scenario();
    run_directed_tag_scenario();
    run_monitor_raw_scenario();
    run_uncache_retry_boost_scenario();
    run_rollback_xmr_path_scenario();
    run_rollback_subtype_decode_scenario();
    run_rollback_cross_sample_cache_scenario();
    run_stale_load_overlap_scenario(1'b0);
    run_stale_load_overlap_scenario(1'b1);
    run_surviving_load_overlap_scenario(1'b0);
    run_surviving_load_overlap_scenario(1'b1);
    run_mixed_wrap_overlap_fatal_scenario(1'b0);
    run_mixed_wrap_overlap_fatal_scenario(1'b1);
    run_new_owner_r_overlap_fatal_scenario();
    run_incompatible_r_overlap_fatal_scenario();
    run_deleted_load_stale_scenario(0);
    run_deleted_load_stale_scenario(1);
    run_deleted_load_stale_scenario(2);
    run_r2_active_survivor_current_scenario();
    run_r2_survivor_with_deleted_history_fatal_scenario();
    run_r3_deleted_load_fatal_scenario();
    run_r2_no_history_fatal_scenario();
    run_new_owner_overlap_fatal_scenario();
    run_r2_wrap_ambiguity_fatal_scenario();
    run_r2_non_load_fatal_scenario();
    run_fault_head_suppress_scenario();
    run_commit_batch_redirect_survivor_scenario();
    run_commit_batch_redirect_mixed_cancel_scenario();
    run_global_stop_raw_drain_scenario();
    `uvm_info(get_type_name(), "pending-MMIO directed owner-contract checks completed", UVM_LOW)
endtask:body

// 中文注释：uvm_do_on 可能跳过继承的 pre_body，本 helper 按 base sequence
// 的现有构造合同幂等补齐 handle 与 bind；不调用 pre_body，也不 reset 公共状态。
task soft_test_memblock_pending_mmio_directed_sequence::ensure_directed_helpers();
    seq_csr_common::init();
    data = common_data_transaction::get();
    if (lsq_ctrl == null) begin
        lsq_ctrl = lsq_ctrl_model::get();
    end
    if (issue_sched == null) begin
        issue_sched = issue_queue_scheduler::type_id::create("issue_sched");
    end
    if (field_assigner == null) begin
        field_assigner = issue_field_assigner::type_id::create("field_assigner");
    end
    if (writeback_handler == null) begin
        writeback_handler = writeback_status_handler::type_id::create("writeback_handler");
    end
    if (monitor_batch_handler == null) begin
        monitor_batch_handler = dispatch_monitor_batch_handler::type_id::create(
            "monitor_batch_handler");
    end
    if (exception_handler == null) begin
        exception_handler = exception_redirect_replay_handler::type_id::create(
            "exception_handler");
    end
    monitor_commit_handler = lsq_commit_handler::get();
    if (monitor_adapter == null) begin
        monitor_adapter = dispatch_monitor_event_adapter::type_id::create("monitor_adapter");
    end

    if (data == null || lsq_ctrl == null || issue_sched == null ||
        field_assigner == null || writeback_handler == null ||
        monitor_batch_handler == null || exception_handler == null ||
        monitor_commit_handler == null || monitor_adapter == null) begin
        `uvm_fatal(get_type_name(), "failed to initialize pending-MMIO directed helpers")
    end
    monitor_batch_handler.bind_writeback_handler(writeback_handler);
    monitor_commit_handler.bind_lsq_ctrl(lsq_ctrl);
    monitor_adapter.bind_commit_handler(monitor_commit_handler);
    // directed 场景与 monitor adapter 必须观察同一个 commit/deq/head owner。
    commit_handler = monitor_commit_handler;
endtask:ensure_directed_helpers

task soft_test_memblock_pending_mmio_directed_sequence::run_rollback_xmr_path_scenario();
    string paths[$];
    uvm_hdl_data_t value;

    for (int idx = 0; idx < 3; idx++) begin
        paths.push_back($sformatf(
            "top_tb.U_MEMBLOCK._inner_LoadUnit_%0d_io_rollback_valid", idx));
        paths.push_back($sformatf(
            "top_tb.U_MEMBLOCK.inner_LoadUnit_%0d.s3_vp_match_fail", idx));
        paths.push_back($sformatf(
            "top_tb.U_MEMBLOCK.inner_LoadUnit_%0d.s3_frm_mis_flush", idx));
        paths.push_back($sformatf(
            "top_tb.U_MEMBLOCK.inner_LoadUnit_%0d.s3_ldld_rep_inst_last_REG", idx));
    end
    paths.push_back("top_tb.U_MEMBLOCK._inner_lsq_io_nack_rollback_0_valid");
    paths.push_back("top_tb.U_MEMBLOCK._inner_lsq_io_nuke_rollback_0_valid");
    paths.push_back("top_tb.U_MEMBLOCK._inner_lsq_io_nuke_rollback_1_valid");
    paths.push_back(
        "top_tb.U_MEMBLOCK.inner_lsq.loadQueue.uncacheBuffer._freeList_io_canAllocate_0");
    paths.push_back(
        "top_tb.U_MEMBLOCK.inner_lsq.loadQueue.loadQueueRAW.io_storeIn_0_valid");
    paths.push_back(
        "top_tb.U_MEMBLOCK.inner_lsq.loadQueue.loadQueueRAW.io_storeIn_1_valid");
    foreach (paths[idx]) begin
        value = '0;
        if (!uvm_hdl_read(paths[idx], value)) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("rollback diagnostic XMR path is unreadable: %s",
                                 paths[idx]))
        end
    end
endtask:run_rollback_xmr_path_scenario

task soft_test_memblock_pending_mmio_directed_sequence::run_rollback_subtype_decode_scenario();
    memblock_sync_pkg::dispatch_raw_ctrl_t raw;
    memblock_wb_event_t wb_event;
    memblock_rollback_subtype_e expected[6];

    expected[0] = MEMBLOCK_ROLLBACK_SUBTYPE_LOAD_VP_MATCH;
    expected[1] = MEMBLOCK_ROLLBACK_SUBTYPE_LOAD_FRM_MISALIGN;
    expected[2] = MEMBLOCK_ROLLBACK_SUBTYPE_LOAD_LDLD_FETCH;
    expected[3] = MEMBLOCK_ROLLBACK_SUBTYPE_UNCACHE_NACK;
    expected[4] = MEMBLOCK_ROLLBACK_SUBTYPE_RAW_NUKE_0;
    expected[5] = MEMBLOCK_ROLLBACK_SUBTYPE_RAW_NUKE_1;
    for (int idx = 0; idx < 6; idx++) begin
        raw = memblock_sync_pkg::make_empty_raw_ctrl();
        raw.valid = 1'b1;
        raw.memory_violation_valid = 1'b1;
        raw.memory_violation_rob_valid = 1'b1;
        raw.memory_violation_rob_flag = 1'b1;
        raw.memory_violation_rob_value = 8'h52 + idx;
        raw.memory_violation_level = idx == 2 ? 1'b0 : 1'b1;
        raw.rollback_diag_valid = 1'b1;
        raw.rollback_diag_complete = 1'b1;
        // Only the selected input is required to be observable.  The zero
        // known masks on the other five lanes model legal inactive X bundles.
        raw.rollback_candidate_known[idx] = 1'b1;
        raw.rollback_candidate_valid[idx] = 1'b1;
        raw.rollback_candidate_rob_flag[idx] = 1'b1;
        raw.rollback_candidate_rob_value[idx] = 8'h52 + idx;
        raw.rollback_candidate_level[idx] = raw.memory_violation_level;
        raw.rollback_winner_known[idx] = 1'b1;
        raw.rollback_winner[idx] = 1'b1;
        case (idx)
            0: begin
                raw.rollback_lu_reason_known[0] = 1'b1;
                raw.rollback_lu_vp_match_fail[0] = 1'b1;
            end
            1: begin
                raw.rollback_lu_reason_known[1] = 1'b1;
                raw.rollback_lu_frm_mis_flush[1] = 1'b1;
            end
            2: begin
                raw.rollback_lu_reason_known[2] = 1'b1;
                raw.rollback_lu_ldld_resp_valid[2] = 1'b1;
                raw.rollback_lu_rep_frm_fetch[2] = 1'b1;
                raw.rollback_lu_ldld_rep_inst[2] = 1'b1;
            end
            3: raw.rollback_uncache_known = '1;
            default: begin end
        endcase
        if (!monitor_adapter.convert_raw_memory_violation(raw, wb_event) ||
            !wb_event.rollback_diag_valid || !wb_event.rollback_diag_complete ||
            wb_event.rollback_selected_input != idx ||
            wb_event.rollback_subtype != expected[idx]) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("rollback subtype directed mismatch input=%0d got=%0d selected=%0d",
                                 idx, wb_event.rollback_subtype,
                                 wb_event.rollback_selected_input))
        end
    end
endtask:run_rollback_subtype_decode_scenario

task soft_test_memblock_pending_mmio_directed_sequence::run_rollback_cross_sample_cache_scenario();
    memblock_sync_pkg::dispatch_raw_ctrl_t input_snapshot;
    memblock_sync_pkg::dispatch_raw_ctrl_t repeated_snapshot;
    memblock_sync_pkg::dispatch_raw_ctrl_t resolved_snapshot;
    longint unsigned input_sample_seq;
    bit conflicting;

    memblock_sync_pkg::clear_rollback_provenance_cache();
    input_snapshot = memblock_sync_pkg::make_empty_raw_ctrl();
    input_snapshot.memory_violation_valid = 1'b1;
    input_snapshot.memory_violation_rob_valid = 1'b1;
    input_snapshot.memory_violation_rob_flag = 1'b1;
    input_snapshot.memory_violation_rob_value = 8'h7e;
    input_snapshot.memory_violation_level = 1'b1;
    input_snapshot.rollback_diag_valid = 1'b1;
    input_snapshot.rollback_diag_complete = 1'b1;
    // 中文注释：复刻原seed同ROB1/126的RAW-nuke0到RAW-nuke1跨拍来源变化。
    input_snapshot.rollback_candidate_known = '1;
    input_snapshot.rollback_candidate_valid[4] = 1'b1;
    input_snapshot.rollback_candidate_rob_flag[4] = 1'b1;
    input_snapshot.rollback_candidate_rob_value[4] = 8'h7e;
    input_snapshot.rollback_candidate_level[4] = 1'b1;
    input_snapshot.rollback_winner_known = 6'h37;
    input_snapshot.rollback_winner[4] = 1'b1;
    input_snapshot.rollback_raw_store_known = '1;
    memblock_sync_pkg::cache_rollback_input_provenance(input_snapshot, 100);

    if (!memblock_sync_pkg::resolve_rollback_output_provenance(
            1'b1, 8'h7e, 1'b1, 100,
            resolved_snapshot, input_sample_seq, conflicting) ||
        conflicting || input_sample_seq != 100 ||
        !memblock_sync_pkg::rollback_provenance_payload_equal(resolved_snapshot, input_snapshot)) begin
        `uvm_fatal(get_type_name(),
                   "same-sample rollback provenance did not copy the complete snapshot")
    end
    if (memblock_sync_pkg::resolve_rollback_output_provenance(
            1'b1, 8'h7e, 1'b1, 102,
            resolved_snapshot, input_sample_seq, conflicting) ||
        conflicting || input_sample_seq != 0 || resolved_snapshot.memory_violation_valid ||
        resolved_snapshot.rollback_diag_valid || resolved_snapshot.rollback_winner != '0 ||
        resolved_snapshot.rollback_candidate_valid != '0) begin
        `uvm_fatal(get_type_name(),
                   "missing current sample borrowed historical rollback provenance")
    end

    repeated_snapshot = input_snapshot;
    repeated_snapshot.rollback_candidate_valid = 6'b10_0000;
    repeated_snapshot.rollback_candidate_rob_flag = 6'b10_0000;
    repeated_snapshot.rollback_candidate_rob_value = '0;
    repeated_snapshot.rollback_candidate_rob_value[5] = 8'h7e;
    repeated_snapshot.rollback_candidate_level = 6'b10_0000;
    repeated_snapshot.rollback_winner = 6'b10_0000;
    memblock_sync_pkg::cache_rollback_input_provenance(repeated_snapshot, 102);
    if (!memblock_sync_pkg::resolve_rollback_output_provenance(
            1'b1, 8'h7e, 1'b1, 102,
            resolved_snapshot, input_sample_seq, conflicting) ||
        conflicting || input_sample_seq != 102 ||
        !memblock_sync_pkg::rollback_provenance_payload_equal(resolved_snapshot, repeated_snapshot)) begin
        `uvm_fatal(get_type_name(),
                   "cross-sample winner4 to winner5 change falsely conflicted")
    end
    // 中文注释：完整ROB flag/value和level仍参与本拍身份，错误身份必须失败且无旧值泄漏。
    for (int idx = 0; idx < 3; idx++) begin
        if (memblock_sync_pkg::resolve_rollback_output_provenance(
                idx == 0 ? 1'b0 : 1'b1,
                idx == 1 ? 8'h7f : 8'h7e,
                idx == 2 ? 1'b0 : 1'b1, 102,
                resolved_snapshot, input_sample_seq, conflicting) ||
            conflicting || input_sample_seq != 0 || resolved_snapshot.memory_violation_valid ||
            resolved_snapshot.rollback_diag_valid || resolved_snapshot.rollback_winner != '0) begin
            `uvm_fatal(get_type_name(), "rollback provenance ignored current ROB/level identity")
        end
    end
    // 中文注释：另一拍增加未获选候选合法；同一拍的重复证明才要求完整一致。
    repeated_snapshot.rollback_candidate_known[1] = 1'b1;
    repeated_snapshot.rollback_candidate_valid[1] = 1'b1;
    repeated_snapshot.rollback_candidate_rob_flag[1] = 1'b1;
    repeated_snapshot.rollback_candidate_rob_value[1] = 8'h7f;
    repeated_snapshot.rollback_candidate_level[1] = 1'b1;
    memblock_sync_pkg::cache_rollback_input_provenance(repeated_snapshot, 104);
    memblock_sync_pkg::cache_rollback_input_provenance(repeated_snapshot, 104);
    if (!memblock_sync_pkg::resolve_rollback_output_provenance(
            1'b1, 8'h7e, 1'b1, 104,
            resolved_snapshot, input_sample_seq, conflicting) || conflicting || input_sample_seq != 104 ||
        !memblock_sync_pkg::rollback_provenance_payload_equal(resolved_snapshot, repeated_snapshot)) begin
        `uvm_fatal(get_type_name(), "same-sample identical proof or cross-sample nonwinner change failed")
    end
    repeated_snapshot.rollback_candidate_rob_value[1] = 8'h80;
    memblock_sync_pkg::cache_rollback_input_provenance(repeated_snapshot, 104);
    if (memblock_sync_pkg::resolve_rollback_output_provenance(
            1'b1, 8'h7e, 1'b1, 104,
            resolved_snapshot, input_sample_seq, conflicting) ||
        !conflicting || input_sample_seq != 0 || resolved_snapshot.memory_violation_valid ||
        resolved_snapshot.rollback_diag_valid || resolved_snapshot.rollback_winner != '0) begin
        `uvm_fatal(get_type_name(),
                   "same-sample rollback proof conflict was not rejected atomically")
    end

    memblock_sync_pkg::clear_rollback_provenance_cache();
    memblock_sync_pkg::cache_rollback_input_provenance(input_snapshot, 108);
    if (memblock_sync_pkg::resolve_rollback_output_provenance(
            1'b1, 8'h7e, 1'b1, 107,
            resolved_snapshot, input_sample_seq, conflicting) || conflicting || input_sample_seq != 0 ||
        resolved_snapshot.memory_violation_valid || resolved_snapshot.rollback_diag_valid) begin
        `uvm_fatal(get_type_name(), "future rollback proof was used for an earlier output")
    end
    memblock_sync_pkg::clear_rollback_provenance_cache();
    if (memblock_sync_pkg::resolve_rollback_output_provenance(
            1'b1, 8'h7e, 1'b1, 108,
            resolved_snapshot, input_sample_seq, conflicting) || conflicting || input_sample_seq != 0 ||
        resolved_snapshot.memory_violation_valid || resolved_snapshot.rollback_diag_valid) begin
        `uvm_fatal(get_type_name(), "reset-cleared rollback provenance remained visible")
    end
    memblock_sync_pkg::cache_rollback_input_provenance(input_snapshot, 100);
    if (memblock_sync_pkg::resolve_rollback_output_provenance(
            1'b1, 8'h7e, 1'b1, 105,
            resolved_snapshot, input_sample_seq, conflicting) || conflicting || input_sample_seq != 0 ||
        memblock_sync_pkg::rollback_provenance_cache.size() != 0) begin
        `uvm_fatal(get_type_name(),
                   "expired rollback provenance remained visible")
    end
    memblock_sync_pkg::clear_rollback_provenance_cache();
    `uvm_info("ROLLBACK_SAME_SAMPLE_DIRECTED",
              "winner4 to winner5 isolation, current identity, strict same-sample conflicts and reset checks PASS", UVM_LOW)
endtask:run_rollback_cross_sample_cache_scenario

// 中文注释：每个 directed 场景只通过 LSQ/data owner 的公开 reset/bind API
// 建立独立 runtime；不直接写 status、active map、modeled head 或 LSQ pointer。
task soft_test_memblock_pending_mmio_directed_sequence::reset_directed_owner_state();
    if (data == null || lsq_ctrl == null || monitor_adapter == null) begin
        `uvm_fatal(get_type_name(),
                   "pending-MMIO directed test requires initialized data/LSQ/adapter owners")
    end
    lsq_ctrl.reset();
    if (commit_handler == null) begin
        commit_handler = lsq_commit_handler::get();
    end
    if (commit_handler == null) begin
        `uvm_fatal(get_type_name(), "LSQ sideband owner is unavailable; directed test is disabled")
    end
    commit_handler.reset_lsqcommit_runtime_state();
    commit_handler.bind_lsq_ctrl(lsq_ctrl);
endtask:reset_directed_owner_state

// 中文注释：directed sequence只等待 ctrl monitor/cancel snapshot owner推进的真实
// DUT sample watermark；本 helper只读取CSR monitor发布的global sample。
task soft_test_memblock_pending_mmio_directed_sequence::wait_for_dut_sample_watermark(
    input longint unsigned target_sample_seq,
    input string scenario_name
);
    if (target_sample_seq == 0) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("%s requested zero DUT sample watermark", scenario_name))
    end
    while (memblock_sync_pkg::peek_current_dut_global_sample() < target_sample_seq) begin
        @(memblock_sync_pkg::dut_sample_seq);
    end
endtask:wait_for_dut_sample_watermark

task soft_test_memblock_pending_mmio_directed_sequence::build_load_store_table();
    clear_manual_main_table();
    set_manual_main_transaction(
        0,
        make_directed_transaction("pending_mmio_directed_load",
                                  MEMBLOCK_OP_CLASS_INT_LOAD,
                                  0,
                                  64'h0000_0000_8100_1000));
    set_manual_main_transaction(
        1,
        make_directed_transaction("pending_mmio_directed_store",
                                  MEMBLOCK_OP_CLASS_STORE,
                                  1,
                                  64'h0000_0000_8100_2000));
    import_manual_main_table();
endtask:build_load_store_table

task soft_test_memblock_pending_mmio_directed_sequence::build_load_table(
    input int unsigned load_count
);
    if (load_count == 0 || load_count > 2) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("directed load table supports one or two entries, got %0d",
                             load_count))
    end
    clear_manual_main_table();
    for (int unsigned idx = 0; idx < load_count; idx++) begin
        set_manual_main_transaction(
            idx,
            make_directed_transaction($sformatf("pending_mmio_load_%0d", idx),
                                      MEMBLOCK_OP_CLASS_INT_LOAD,
                                      idx,
                                      64'h0000_0000_8200_1000 + (idx * 64'h1000)));
    end
    import_manual_main_table();
endtask:build_load_table

task soft_test_memblock_pending_mmio_directed_sequence::build_store_table();
    clear_manual_main_table();
    set_manual_main_transaction(
        0,
        make_directed_transaction("pending_mmio_store_0",
                                  MEMBLOCK_OP_CLASS_STORE,
                                  0,
                                  64'h0000_0000_8300_1000));
    set_manual_main_transaction(
        1,
        make_directed_transaction("pending_mmio_store_1",
                                  MEMBLOCK_OP_CLASS_STORE,
                                  1,
                                  64'h0000_0000_8300_2000));
    import_manual_main_table();
endtask:build_store_table

task soft_test_memblock_pending_mmio_directed_sequence::run_rob_wrap_admission_fatal_scenario(
    input string scenario_name
);
    memblock_pending_mmio_rob_expected_fatal_catcher catcher;
    memblock_uid_t candidate_uid;

    // 两个相邻 UID 不能伪造为同一 value 的 flag0/flag1 pair；该 pair
    // 在合法连续 ROB 序列中相隔 MEMBLOCK_ROB_SIZE。用完整 161-entry
    // 主表验证真实 invariant：距离160时上游 admission 被阻塞，不能建立 owner。
    build_random_main_table(MEMBLOCK_ROB_SIZE + 1);
    for (int unsigned uid = 0; uid < MEMBLOCK_ROB_SIZE; uid++) begin
        data.mark_uid_enqueued(uid);
    end
    candidate_uid = MEMBLOCK_ROB_SIZE;
    if (data.rob_admission_window_has_room(candidate_uid, scenario_name)) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("%s admitted distance=%0d candidate", scenario_name,
                             MEMBLOCK_ROB_SIZE))
    end

    catcher = new({scenario_name, "_catcher"});
    uvm_report_cb::add(null, catcher);
    data.activate_uid(candidate_uid, 1'b0, 1'b0);
    uvm_report_cb::delete(null, catcher);
    if (catcher.caught_count != 1 || data.uid_by_active_rob.num() != 0) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("%s expected ROB admission fatal/no owner, caught=%0d active=%0d",
                             scenario_name, catcher.caught_count,
                             data.uid_by_active_rob.num()))
    end
endtask:run_rob_wrap_admission_fatal_scenario

// 中文注释：使用与真实 responder/monitor相同的公开入口建立 redirect record、
// drive completion和monitor anchor；不直接写cancel record或anchor历史队列。
task soft_test_memblock_pending_mmio_directed_sequence::establish_redirect_anchor(
    input memblock_redirect_payload_t redirect,
    input string scenario_name,
    output longint unsigned anchor_sample_seq
);
    memblock_redirect_payload_t driven_redirect;
    memblock_sync_pkg::dispatch_raw_redirect_anchor_t anchor;

    data.request_redirect_flush(redirect);
    data.push_redirect_drive(redirect);
    if (!data.try_pop_redirect_drive(driven_redirect) ||
        !data.redirect_payload_equal(driven_redirect, redirect)) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("%s failed to acquire redirect drive", scenario_name))
    end
    data.mark_redirect_drive_done(driven_redirect);

    wait_for_dut_sample_watermark(1, {scenario_name, "-anchor"});
    anchor_sample_seq = memblock_sync_pkg::peek_current_dut_global_sample();
    anchor = memblock_sync_pkg::make_empty_raw_redirect_anchor();
    anchor.valid = 1'b1;
    anchor.level = redirect.level;
    anchor.is_vls_exception = redirect.is_vls_exception;
    anchor.effective_level = memblock_redirect_effective_level(redirect);
    anchor.rob_flag = redirect.rob_key.flag;
    anchor.rob_value = redirect.rob_key.value;
    anchor.sample_seq = anchor_sample_seq;
    anchor.cycle = $time;
    memblock_sync_pkg::push_raw_redirect_anchor(anchor);
    monitor_adapter.drain_lsq_timing_sidebands();
    monitor_adapter.service_lsq_timing_reconcile();
endtask:establish_redirect_anchor

// 中文注释：先通过reservation token公开API证明victim已对DUT可见，再等到R+1
// cancel watermark并调用正式redirect flush。deleted-owner窗口只能由生产代码创建。
task soft_test_memblock_pending_mmio_directed_sequence::prepare_redirect_deleted_owner_window(
    input memblock_uid_t victim_uids[$],
    input memblock_redirect_payload_t redirect,
    input string scenario_name,
    output longint unsigned anchor_sample_seq
);
    longint unsigned reservation_sample_seq;
    longint unsigned redirect_lsq_sample_seq;

    if (victim_uids.size() == 0) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("%s requires at least one redirect victim", scenario_name))
    end
    wait_for_dut_sample_watermark(1, {scenario_name, "-reservation"});
    reservation_sample_seq = memblock_sync_pkg::peek_current_dut_global_sample();
    foreach (victim_uids[idx]) begin
        int unsigned launch_epoch;

        launch_epoch = data.begin_lsq_reservation_launch(victim_uids[idx]);
        data.mark_lsq_reservation_sampled(victim_uids[idx], launch_epoch,
                                          reservation_sample_seq);
    end

    establish_redirect_anchor(redirect, scenario_name, anchor_sample_seq);
    redirect_lsq_sample_seq = anchor_sample_seq + 1;
    while (data.latest_drained_cancel_sample_seq < redirect_lsq_sample_seq) begin
        wait_for_dut_sample_watermark(redirect_lsq_sample_seq,
                                      {scenario_name, "-cancel-watermark"});
        monitor_adapter.drain_lsq_timing_sidebands();
        monitor_adapter.service_lsq_timing_reconcile();
        if (data.latest_drained_cancel_sample_seq < redirect_lsq_sample_seq) begin
            @(memblock_sync_pkg::dut_sample_seq);
        end
    end
    if (!data.cancel_redirect_scan_ready(redirect)) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("%s redirect scan was not ready at R+1", scenario_name))
    end
    if (!data.apply_redirect_flush(redirect)) begin
        `uvm_fatal(get_type_name(), "directed redirect flush preparation failed")
        return;
    end
    if (!data.redirect_deleted_owner_window_pending()) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("%s did not create redirect-deleted owner window",
                             scenario_name))
    end
endtask:prepare_redirect_deleted_owner_window

task soft_test_memblock_pending_mmio_directed_sequence::apply_load_mmio_raw(
    input bit [MEMBLOCK_ROB_VALUE_W-1:0] rob_value,
    input longint unsigned sample_seq,
    input string scenario_name,
    input bit expect_fatal = 1'b0,
    input string expected_fatal_pattern = ""
);
    memblock_sync_pkg::dispatch_raw_ctrl_t raw;
    memblock_pending_mmio_expected_fatal_catcher catcher;

    wait_for_dut_sample_watermark(sample_seq, scenario_name);
    raw = memblock_sync_pkg::make_empty_raw_ctrl();
    raw.valid = 1'b1;
    raw.load_mmio_valid[0] = 1'b1;
    raw.load_mmio_rob_value[0] = rob_value;
    raw.mmio_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
    raw.mmio_sample_seq = sample_seq;
    raw.cycle = $time;

    if (!expect_fatal) begin
        monitor_adapter.apply_raw_ctrl_mmio_tags(raw);
        return;
    end
    catcher = new({scenario_name, "_catcher"}, expected_fatal_pattern);
    uvm_report_cb::add(null, catcher);
    monitor_adapter.apply_raw_ctrl_mmio_tags(raw);
    uvm_report_cb::delete(null, catcher);
    if (catcher.caught_count != 1) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("%s expected one MMIO_RESOLVE fatal, got %0d",
                             scenario_name, catcher.caught_count))
    end
endtask:apply_load_mmio_raw

task soft_test_memblock_pending_mmio_directed_sequence::check_load_head_sideband(
    input memblock_uid_t expected_uid,
    input string scenario_name
);
    lsqcommit_agent_agent_xaction tr;
    memblock_uid_t commit_uids[$];
    bit has_commit;
    bit has_fault_head;
    memblock_uid_t fault_uid;
    memblock_rob_key_t expected_key;

    expected_key = data.get_status(expected_uid).get_rob_key();
    commit_handler.build_lsqcommit_xaction(tr, commit_uids, has_commit,
                                           has_fault_head, fault_uid);
    if (has_fault_head || !tr.io_ooo_to_mem_lsqio_pendingMMIOld ||
        tr.io_ooo_to_mem_lsqio_pendingst ||
        tr.io_ooo_to_mem_lsqio_pendingPtr_flag != expected_key.flag ||
        tr.io_ooo_to_mem_lsqio_pendingPtr_value != expected_key.value) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("%s sideband mismatch uid=%0d pendingPtr=%0d/%0d expected=%0d/%0d pendingMMIOld=%0d pendingst=%0d fault=%0d",
                             scenario_name, expected_uid,
                             tr.io_ooo_to_mem_lsqio_pendingPtr_flag,
                             tr.io_ooo_to_mem_lsqio_pendingPtr_value,
                             expected_key.flag, expected_key.value,
                             tr.io_ooo_to_mem_lsqio_pendingMMIOld,
                             tr.io_ooo_to_mem_lsqio_pendingst,
                             has_fault_head))
    end
endtask:check_load_head_sideband

task soft_test_memblock_pending_mmio_directed_sequence::run_inactive_head_pointer_scenario();
    lsqcommit_agent_agent_xaction tr;
    memblock_uid_t commit_uids[$];
    memblock_rob_key_t expected_key;
    bit has_commit;
    bit has_fault_head;
    memblock_uid_t fault_uid;

    reset_directed_owner_state();
    build_load_table(2);
    expected_key = data.get_status(0).get_rob_key();
    commit_handler.build_lsqcommit_xaction(tr, commit_uids, has_commit,
                                           has_fault_head, fault_uid);
    if (has_commit || has_fault_head || commit_uids.size() != 0 ||
        tr.io_ooo_to_mem_lsqio_pendingPtr_flag != expected_key.flag ||
        tr.io_ooo_to_mem_lsqio_pendingPtr_value != expected_key.value ||
        tr.io_ooo_to_mem_lsqio_pendingst ||
        tr.io_ooo_to_mem_lsqio_pendingMMIOld ||
        tr.io_ooo_to_mem_lsqio_scommit != '0) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("inactive modeled head mismatch pendingPtr=%0d/%0d expected=%0d/%0d pendingst/mmio/scommit=%0d/%0d/%0d",
                             tr.io_ooo_to_mem_lsqio_pendingPtr_flag,
                             tr.io_ooo_to_mem_lsqio_pendingPtr_value,
                             expected_key.flag,
                             expected_key.value,
                             tr.io_ooo_to_mem_lsqio_pendingst,
                             tr.io_ooo_to_mem_lsqio_pendingMMIOld,
                             tr.io_ooo_to_mem_lsqio_scommit))
    end
endtask:run_inactive_head_pointer_scenario

task soft_test_memblock_pending_mmio_directed_sequence::run_directed_tag_scenario();
    memblock_issue_q_item_t fired_items[$];

    reset_directed_owner_state();
    build_load_store_table();
    admit_lsq_and_route_issue();
    fire_all_issue_items(fired_items);
    data.set_uid_mmio_tag(0, MEMBLOCK_MMIO_KIND_LOAD,
                          MEMBLOCK_MMIO_TAG_DIRECTED);
    data.set_uid_mmio_tag(1, MEMBLOCK_MMIO_KIND_STORE,
                          MEMBLOCK_MMIO_TAG_DIRECTED);
    if (!data.uid_is_mmio_load(0) || data.uid_is_mmio_store(0) ||
        !data.uid_is_mmio_store(1) || data.uid_is_mmio_load(1)) begin
        `uvm_fatal(get_type_name(), "directed canonical load/store MMIO queries disagree")
    end
    check_load_head_sideband(0, "directed-tag");
endtask:run_directed_tag_scenario

task soft_test_memblock_pending_mmio_directed_sequence::run_monitor_raw_scenario();
    memblock_issue_q_item_t fired_items[$];
    memblock_sync_pkg::dispatch_raw_ctrl_t raw;

    reset_directed_owner_state();
    build_load_table(2);
    admit_lsq_and_route_issue();
    fire_all_issue_items(fired_items);

    raw = memblock_sync_pkg::make_empty_raw_ctrl();
    raw.valid = 1'b1;
    raw.load_mmio_valid[0] = 1'b1;
    raw.load_mmio_rob_value[0] = data.get_status(0).robIdx_value;
    raw.mmio_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
    wait_for_dut_sample_watermark(1, "monitor-raw");
    raw.mmio_sample_seq = memblock_sync_pkg::peek_current_dut_global_sample();
    raw.cycle = $time;
    monitor_adapter.apply_raw_ctrl_mmio_tags(raw);
    if (!data.uid_is_mmio_load(0) ||
        data.get_status(0).mmio_tag_source != MEMBLOCK_MMIO_TAG_MONITOR) begin
        `uvm_fatal(get_type_name(), "monitor-like raw did not produce canonical load MMIO tag")
    end
    check_load_head_sideband(0, "monitor-raw");
endtask:run_monitor_raw_scenario

function memblock_wb_event_t
soft_test_memblock_pending_mmio_directed_sequence::make_uncache_nack_event(
    input memblock_uid_t uid,
    input memblock_rollback_subtype_e subtype = MEMBLOCK_ROLLBACK_SUBTYPE_UNCACHE_NACK,
    input bit flush_itself = 1'b1,
    input bit has_dynamic_epoch = 1'b1
);
    memblock_wb_event_t wb_event;
    status_transaction status;

    wb_event = data.make_empty_wb_event();
    status = data.get_status(uid);
    wb_event.valid = 1'b1;
    wb_event.source = MEMBLOCK_WB_EVENT_SOURCE_MEMORY_VIOLATION;
    wb_event.uid = uid;
    wb_event.has_uid = 1'b1;
    wb_event.rob_key = status.get_rob_key();
    wb_event.has_rob = 1'b1;
    wb_event.redirect.valid = 1'b1;
    wb_event.redirect_valid = 1'b1;
    wb_event.redirect.rob_key = wb_event.rob_key;
    wb_event.redirect.flush_itself = flush_itself;
    wb_event.redirect.level = flush_itself;
    wb_event.rollback_diag_valid = 1'b1;
    wb_event.rollback_diag_complete = 1'b1;
    wb_event.rollback_selected_input = 3;
    wb_event.rollback_subtype = subtype;
    wb_event.has_dynamic_epoch = has_dynamic_epoch;
    wb_event.dynamic_epoch = status.dynamic_epoch;
    return wb_event;
endfunction:make_uncache_nack_event

task soft_test_memblock_pending_mmio_directed_sequence::run_uncache_retry_boost_scenario();
    memblock_wb_event_t wb_evt;
    memblock_wb_event_t events[$];
    memblock_issue_q_item_t load_items[$];
    memblock_issue_q_item_t sta_items[$];
    memblock_issue_q_item_t std_items[$];
    memblock_issue_q_item_t uid0_item;
    memblock_issue_q_item_t uid1_item;
    memblock_op_behavior_t uid0_behavior;
    memblock_op_behavior_t uid1_behavior;
    memblock_rob_key_t old_rob_key;
    int unsigned old_dynamic_epoch;
    status_transaction status;

    // 最终 selected 正向路径：只允许 subtype=UNCACHE_NACK 且 flush_itself=1 置位。
    reset_directed_owner_state();
    build_load_table(2);
    admit_lsq_and_route_issue();

    // uid1虽然是UNCACHE_NACK，但同batch更老的uid0才是最终winner，因此不能提前置位。
    wb_evt = make_uncache_nack_event(0, MEMBLOCK_ROLLBACK_SUBTYPE_LOAD_VP_MATCH);
    events.push_back(wb_evt);
    wb_evt = make_uncache_nack_event(1);
    events.push_back(wb_evt);
    monitor_batch_handler.process_monitor_event_batch(events);
    if (data.get_status(0).uncache_retry_boost ||
        data.get_status(1).uncache_retry_boost) begin
        `uvm_fatal(get_type_name(), "non-selected UNCACHE_NACK incorrectly set retry boost")
    end
    events.delete();
    data.clear_feedback_events();

    wb_evt = make_uncache_nack_event(0);
    events.push_back(wb_evt);
    monitor_batch_handler.process_monitor_event_batch(events);
    status = data.get_status(0);
    if (!status.uncache_retry_boost || status.uncache_retry_count != 1 ||
        !status.uncache_retry_identity_valid ||
        status.uncache_retry_dynamic_epoch != status.dynamic_epoch) begin
        `uvm_fatal(get_type_name(), "selected UNCACHE_NACK did not set retry boost identity")
    end
    data.clear_feedback_events();

    wb_evt = make_uncache_nack_event(1, MEMBLOCK_ROLLBACK_SUBTYPE_LOAD_VP_MATCH);
    if (data.apply_uncache_retry_boost(wb_evt) || data.get_status(1).uncache_retry_boost) begin
        `uvm_fatal(get_type_name(), "non-UNCACHE subtype incorrectly set retry boost")
    end
    wb_evt = make_uncache_nack_event(1, MEMBLOCK_ROLLBACK_SUBTYPE_UNCACHE_NACK, 1'b0);
    if (data.apply_uncache_retry_boost(wb_evt) || data.get_status(1).uncache_retry_boost) begin
        `uvm_fatal(get_type_name(), "flush_itself=0 incorrectly set retry boost")
    end
    wb_evt = make_uncache_nack_event(1, MEMBLOCK_ROLLBACK_SUBTYPE_UNCACHE_NACK, 1'b1, 1'b0);
    if (data.apply_uncache_retry_boost(wb_evt) || data.get_status(1).uncache_retry_boost) begin
        `uvm_fatal(get_type_name(), "missing dynamic identity incorrectly set retry boost")
    end
    wb_evt = make_uncache_nack_event(1, MEMBLOCK_ROLLBACK_SUBTYPE_RAW_NUKE_0);
    if (data.apply_uncache_retry_boost(wb_evt) || data.get_status(1).uncache_retry_boost) begin
        `uvm_fatal(get_type_name(), "RAW nuke incorrectly set retry boost")
    end
    wb_evt = make_uncache_nack_event(1);
    wb_evt.redirect.is_vls_exception = 1'b1;
    if (data.apply_uncache_retry_boost(wb_evt) || data.get_status(1).uncache_retry_boost) begin
        `uvm_fatal(get_type_name(), "exception redirect incorrectly set retry boost")
    end
    wb_evt = make_uncache_nack_event(1);
    wb_evt.source = MEMBLOCK_WB_EVENT_SOURCE_STA_FEEDBACK;
    wb_evt.replay_valid = 1'b1;
    if (data.apply_uncache_retry_boost(wb_evt) || data.get_status(1).uncache_retry_boost) begin
        `uvm_fatal(get_type_name(), "ordinary replay incorrectly set retry boost")
    end
    wb_evt = make_uncache_nack_event(1);
    wb_evt.source = MEMBLOCK_WB_EVENT_SOURCE_STORE_WB;
    if (data.apply_uncache_retry_boost(wb_evt) || data.get_status(1).uncache_retry_boost) begin
        `uvm_fatal(get_type_name(), "flushAfter-class event incorrectly set retry boost")
    end
    wb_evt = make_uncache_nack_event(1);
    wb_evt.dynamic_epoch++;
    if (data.apply_uncache_retry_boost(wb_evt) || data.get_status(1).uncache_retry_boost) begin
        `uvm_fatal(get_type_name(), "stale dynamic identity incorrectly set retry boost")
    end

    // clear_uid_dispatch_result 不能吞掉跨 redirect 生命周期所需的 boost；紧邻 E+1
    // 只允许同 UID/ROB/旧 epoch 继承，terminal 则必须清零。
    old_rob_key = status.get_rob_key();
    old_dynamic_epoch = status.dynamic_epoch;
    void'(data.clear_uid_dispatch_result(0));
    if (!status.uncache_retry_boost || status.uncache_retry_count != 1) begin
        `uvm_fatal(get_type_name(), "dispatch-result clear dropped uncache retry boost")
    end
    status.dynamic_epoch = old_dynamic_epoch + 1;
    data.inherit_uncache_retry_boost_for_reissue(0, old_rob_key,
                                                old_dynamic_epoch,
                                                status.dynamic_epoch);
    if (!status.uncache_retry_boost ||
        status.uncache_retry_dynamic_epoch != old_dynamic_epoch + 1) begin
        `uvm_fatal(get_type_name(), "valid adjacent redirect reissue did not inherit boost")
    end
    data.set_status_field(0, MEMBLOCK_STATUS_TERMINAL_DONE, 1'b1);
    if (status.uncache_retry_boost || status.uncache_retry_count != 0) begin
        `uvm_fatal(get_type_name(), "terminal status did not clear retry boost")
    end

    // reset_all_tables经正式directed reset入口重建status，不能继承上个场景的boost。
    wb_evt = make_uncache_nack_event(1);
    void'(data.apply_uncache_retry_boost(wb_evt));
    reset_directed_owner_state();
    build_load_table(2);
    status = data.get_status(1);
    if (status.uncache_retry_boost || status.uncache_retry_count != 0 ||
        status.uncache_retry_identity_valid || status.uncache_retry_has_dynamic_epoch) begin
        `uvm_fatal(get_type_name(), "table reset leaked uncache retry boost state")
    end

    // scheduler snapshot：boosted item 优先于高 send_pri 普通 item；两个 boosted item
    // 只允许全局最老者取得本拍唯一 token。成功 fire 消费 token，未 fire 保留；
    // 后续 subtype5 可以显式重新挂起。
    data.get_main_transaction(0).send_pri = 1;
    data.get_main_transaction(1).send_pri = 100;
    data.get_main_transaction(0).delay = 50;
    data.get_main_transaction(1).delay = 50;
    admit_lsq_and_route_issue();
    data.remove_uid_from_issue_queues(0);
    data.remove_uid_from_issue_queues(1);

    // 普通请求仍按send_pri比较，年轻高priority的uid1优先。
    uid0_behavior = derive_op_behavior(data.get_main_transaction(0));
    uid1_behavior = derive_op_behavior(data.get_main_transaction(1));
    uid0_item = issue_sched.make_issue_item(0, MEMBLOCK_ISSUE_TARGET_LOAD,
                                            uid0_behavior);
    uid1_item = issue_sched.make_issue_item(1, MEMBLOCK_ISSUE_TARGET_LOAD,
                                            uid1_behavior);
    if (uid0_item.ready_cycle != 50 || uid1_item.ready_cycle != 50) begin
        `uvm_fatal(get_type_name(), "ordinary issue item did not preserve randomized delay")
    end
    if (!issue_sched.item_is_better(uid1_item, uid0_item, 1'b1) ||
        issue_sched.item_is_better(uid0_item, uid1_item, 1'b1)) begin
        `uvm_fatal(get_type_name(), "ordinary item priority ordering changed by boost support")
    end

    wb_evt = make_uncache_nack_event(0);
    void'(data.apply_uncache_retry_boost(wb_evt));
    issue_sched.route_uid(0);
    issue_sched.route_uid(1);
    if (data.load_issue_q.size() != 2 ||
        data.load_issue_q[0].uid != 0 || data.load_issue_q[0].ready_cycle != 0 ||
        data.load_issue_q[1].uid != 1 || data.load_issue_q[1].ready_cycle != 50) begin
        `uvm_fatal(get_type_name(),
                   "retry boost did not bypass only the boosted reissue delay")
    end
    // uid0是低send_pri boost，uid1是高send_pri普通项；global预筛选必须选uid0。
    issue_sched.select_issue_candidates(load_items, sta_items, std_items);
    if (load_items.size() == 0 || load_items[0].uid != 0 ||
        !load_items[0].uncache_retry_boost) begin
        `uvm_fatal(get_type_name(), "low-priority boosted item did not beat normal high-priority item")
    end
    if (!data.get_status(0).uncache_retry_boost) begin
        `uvm_fatal(get_type_name(), "non-fired retry winner lost its live token")
    end

    // 重新冻结uid1为boost；无论每拍pipeline数量是多少，只能选择较老uid0。
    data.remove_uid_from_issue_queues(1);
    wb_evt = make_uncache_nack_event(1);
    void'(data.apply_uncache_retry_boost(wb_evt));
    issue_sched.route_uid(1);
    issue_sched.select_issue_candidates(load_items, sta_items, std_items);
    if (load_items.size() + sta_items.size() + std_items.size() != 1 ||
        load_items.size() != 1 || load_items[0].uid != 0 ||
        !load_items[0].uncache_retry_boost) begin
        `uvm_fatal(get_type_name(), "multiple boosted items did not produce one oldest global winner")
    end
    if (!issue_sched.mark_issue_fire(load_items[0])) begin
        `uvm_fatal(get_type_name(), "oldest boosted winner did not fire")
    end
    status = data.get_status(0);
    if (status.uncache_retry_boost || status.uncache_retry_identity_valid ||
        status.uncache_retry_has_dynamic_epoch || status.uncache_retry_count != 1) begin
        `uvm_fatal(get_type_name(), "successful boosted fire did not consume only the live token")
    end

    // uid0 token被消费后，下一最老的uid1立即取得唯一winner token。
    issue_sched.select_issue_candidates(load_items, sta_items, std_items);
    if (load_items.size() + sta_items.size() + std_items.size() != 1 ||
        load_items.size() != 1 || load_items[0].uid != 1 ||
        !data.get_status(1).uncache_retry_boost) begin
        `uvm_fatal(get_type_name(), "next-oldest retry token did not take ownership after winner fire")
    end

    // 同一动态实例再次返回subtype5时重新挂起，累计计数不能因token消费清零。
    wb_evt = make_uncache_nack_event(0);
    if (!data.apply_uncache_retry_boost(wb_evt)) begin
        `uvm_fatal(get_type_name(), "subsequent subtype5 did not re-arm consumed retry token")
    end
    status = data.get_status(0);
    if (!status.uncache_retry_boost || status.uncache_retry_count != 2) begin
        `uvm_fatal(get_type_name(), "re-armed retry token lost cumulative retry count")
    end
    data.clear_uncache_retry_boost(0, "directed cleanup");
    data.clear_uncache_retry_boost(1, "directed cleanup");
    `uvm_info(get_type_name(), "uncache retry boost directed checks completed", UVM_LOW)
endtask:run_uncache_retry_boost_scenario

// 中文注释：模拟 LoadQueueUncache loadMmio 在 redirect sample R 或 R+1 到达。
// 旧 active owner 的完整 ROB key 被 redirect 覆盖时，adapter必须只丢该 port且不写 tag。
task soft_test_memblock_pending_mmio_directed_sequence::run_stale_load_overlap_scenario(
    input bit one_cycle_late
);
    memblock_issue_q_item_t fired_items[$];
    memblock_redirect_payload_t redirect;
    memblock_sync_pkg::dispatch_raw_redirect_anchor_t anchor;
    memblock_sync_pkg::dispatch_raw_ctrl_t raw;
    memblock_rob_key_t owner_key;

    reset_directed_owner_state();
    build_load_table(2);
    admit_lsq_and_route_issue();
    fire_all_issue_items(fired_items);
    owner_key = data.get_status(1).get_rob_key();

    redirect = '{default:'0};
    redirect.valid = 1'b1;
    redirect.flush_itself = 1'b1;
    redirect.level = 1'b1;
    redirect.is_vls_exception = 1'b0;
    redirect.rob_key = owner_key;
    data.request_redirect_flush(redirect);

    anchor = memblock_sync_pkg::make_empty_raw_redirect_anchor();
    anchor.valid = 1'b1;
    anchor.level = redirect.level;
    anchor.is_vls_exception = redirect.is_vls_exception;
    anchor.effective_level = memblock_redirect_effective_level(redirect);
    anchor.rob_flag = redirect.rob_key.flag;
    anchor.rob_value = redirect.rob_key.value;
    wait_for_dut_sample_watermark(1, "stale-load-overlap-anchor");
    anchor.sample_seq = memblock_sync_pkg::peek_current_dut_global_sample();
    anchor.cycle = $time;
    memblock_sync_pkg::push_raw_redirect_anchor(anchor);
    monitor_adapter.drain_lsq_timing_sidebands();

    raw = memblock_sync_pkg::make_empty_raw_ctrl();
    raw.valid = 1'b1;
    raw.load_mmio_valid[0] = 1'b1;
    raw.load_mmio_rob_value[0] = owner_key.value;
    raw.mmio_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
    raw.mmio_sample_seq = one_cycle_late ? anchor.sample_seq + 1 : anchor.sample_seq;
    wait_for_dut_sample_watermark(raw.mmio_sample_seq,
                                  one_cycle_late ? "stale-load-overlap-R+1" :
                                                   "stale-load-overlap-R");
    raw.cycle = $time;
    monitor_adapter.apply_raw_ctrl_mmio_tags(raw);
    if (data.get_status(1).mmio_tag_valid) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("stale loadMmio overlap offset=%0d incorrectly tagged old uid",
                             one_cycle_late))
    end
endtask:run_stale_load_overlap_scenario

// redirect边界位于更年轻的uid1，uid0在R/R+1仍是唯一active survivor。
// resolver必须把raw归给uid0并写monitor tag，不能按overlap一律stale-drop/fatal。
task soft_test_memblock_pending_mmio_directed_sequence::run_surviving_load_overlap_scenario(
    input bit one_cycle_late
);
    memblock_issue_q_item_t fired_items[$];
    memblock_redirect_payload_t redirect;
    memblock_rob_key_t survivor_key;
    memblock_rob_key_t redirect_key;
    longint unsigned anchor_sample_seq;

    reset_directed_owner_state();
    build_load_table(2);
    admit_lsq_and_route_issue();
    fire_all_issue_items(fired_items);
    survivor_key = data.get_status(0).get_rob_key();
    redirect_key = data.get_status(1).get_rob_key();
    redirect = '{default:'0};
    redirect.valid = 1'b1;
    redirect.flush_itself = 1'b1;
    redirect.level = 1'b1;
    redirect.rob_key = redirect_key;
    if (rob_order_util::rob_need_flush(survivor_key, redirect) ||
        !rob_order_util::rob_need_flush(redirect_key, redirect)) begin
        `uvm_fatal(get_type_name(), "surviving overlap setup has invalid ROB ordering")
    end
    establish_redirect_anchor(
        redirect,
        one_cycle_late ? "surviving-load-overlap-R+1" :
                         "surviving-load-overlap-R",
        anchor_sample_seq);
    apply_load_mmio_raw(
        survivor_key.value,
        anchor_sample_seq + (one_cycle_late ? 1 : 0),
        one_cycle_late ? "surviving-load-overlap-R+1" :
                         "surviving-load-overlap-R");
    if (!data.uid_is_mmio_load(0) || data.get_status(1).mmio_tag_valid ||
        data.get_status(0).mmio_tag_source != MEMBLOCK_MMIO_TAG_MONITOR) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("surviving load overlap offset=%0d did not tag only uid0",
                             one_cycle_late))
    end
endtask:run_surviving_load_overlap_scenario

// 同一value同时命中flag0/flag1 active owner，其中一个covered、一个uncovered。
// 即使存在唯一survivor分类也必须因active候选混合而fatal。
task soft_test_memblock_pending_mmio_directed_sequence::run_mixed_wrap_overlap_fatal_scenario(
    input bit one_cycle_late
);
    reset_directed_owner_state();
    run_rob_wrap_admission_fatal_scenario(
        one_cycle_late ? "mixed-wrap-overlap-R+1" : "mixed-wrap-overlap-R");
endtask:run_mixed_wrap_overlap_fatal_scenario

task soft_test_memblock_pending_mmio_directed_sequence::run_new_owner_r_overlap_fatal_scenario();
    main_control_transaction main_tr;
    memblock_op_behavior_t behavior;
    memblock_issue_q_item_t fired_items[$];
    memblock_redirect_payload_t redirect;
    memblock_rob_key_t owner_key;
    longint unsigned anchor_sample_seq;

    reset_directed_owner_state();
    build_load_table(2);
    main_tr = data.get_main_transaction(0);
    behavior = derive_op_behavior(main_tr);
    owner_key = data.get_status(0).get_rob_key();
    redirect = '{default:'0};
    redirect.valid = 1'b1;
    redirect.flush_itself = 1'b1;
    redirect.level = 1'b1;
    redirect.rob_key = owner_key;
    establish_redirect_anchor(redirect, "new-owner-overlap-R", anchor_sample_seq);
    lsq_ctrl.commit_allocate(0, behavior, main_tr);
    // redirect freeze期间生产issue scheduler必须保持冻结；本negative case只通过
    // 公共status API建立resolver所需的“新epoch内已dispatch Load”事实。
    data.set_status_field(0, MEMBLOCK_STATUS_LOAD_DISPATCHED, 1'b1);
    apply_load_mmio_raw(owner_key.value, anchor_sample_seq,
                        "new-owner-overlap-R", 1'b1,
                        "*cannot prove LOAD MMIO stale ownership*");
    if (data.get_status(0).mmio_tag_valid) begin
        `uvm_fatal(get_type_name(), "new owner R-overlap fatal path wrote an MMIO tag")
    end
endtask:run_new_owner_r_overlap_fatal_scenario

task soft_test_memblock_pending_mmio_directed_sequence::run_incompatible_r_overlap_fatal_scenario();
    memblock_issue_q_item_t fired_items[$];
    memblock_redirect_payload_t redirect;
    memblock_rob_key_t store_key;
    longint unsigned anchor_sample_seq;

    reset_directed_owner_state();
    build_store_table();
    admit_lsq_and_route_issue();
    fire_all_issue_items(fired_items);
    store_key = data.get_status(0).get_rob_key();
    redirect = '{default:'0};
    redirect.valid = 1'b1;
    redirect.flush_itself = 1'b1;
    redirect.level = 1'b1;
    redirect.rob_key = data.get_status(1).get_rob_key();
    if (rob_order_util::rob_need_flush(store_key, redirect)) begin
        `uvm_fatal(get_type_name(), "incompatible overlap setup did not preserve store owner")
    end
    establish_redirect_anchor(redirect, "incompatible-overlap-R", anchor_sample_seq);
    apply_load_mmio_raw(store_key.value, anchor_sample_seq,
                        "incompatible-overlap-R", 1'b1,
                        "*cannot prove LOAD MMIO stale ownership*");
    if (data.get_status(0).mmio_tag_valid) begin
        `uvm_fatal(get_type_name(), "incompatible R-overlap fatal path wrote an MMIO tag")
    end
endtask:run_incompatible_r_overlap_fatal_scenario

// 精确R+2仍存在唯一redirect前、未被覆盖的active scalar Load时，ROB admission
// 与双flag active probe已经排除同value旧producer，resolver必须归为CURRENT。
task soft_test_memblock_pending_mmio_directed_sequence::run_r2_active_survivor_current_scenario();
    memblock_issue_q_item_t fired_items[$];
    memblock_redirect_payload_t redirect;
    memblock_rob_key_t survivor_key;
    longint unsigned anchor_sample_seq;

    reset_directed_owner_state();
    build_load_table(2);
    admit_lsq_and_route_issue();
    fire_all_issue_items(fired_items);
    survivor_key = data.get_status(0).get_rob_key();
    redirect = '{default:'0};
    redirect.valid = 1'b1;
    redirect.flush_itself = 1'b1;
    redirect.level = 1'b1;
    redirect.rob_key = data.get_status(1).get_rob_key();
    establish_redirect_anchor(redirect, "r2-active-survivor-current", anchor_sample_seq);
    apply_load_mmio_raw(survivor_key.value, anchor_sample_seq + 2,
                        "r2-active-survivor-current");
    if (!data.uid_is_mmio_load(0) || data.get_status(1).mmio_tag_valid ||
        data.get_status(0).mmio_tag_source != MEMBLOCK_MMIO_TAG_MONITOR) begin
        `uvm_fatal(get_type_name(),
                   "R+2 active survivor did not tag only the surviving uid")
    end
endtask:run_r2_active_survivor_current_scenario

// 同一ROB value的flag1 owner被redirect删除，而flag0 owner继续active存活。
// R+2 raw同时存在current survivor与covered deleted producer两种可能来源，
// value-only接口无法区分，必须保持fatal，不能仅凭唯一active owner写CURRENT tag。
task soft_test_memblock_pending_mmio_directed_sequence::run_r2_survivor_with_deleted_history_fatal_scenario();
    memblock_issue_q_item_t fired_items[$];
    memblock_uid_t victim_uids[$];
    memblock_redirect_payload_t redirect;
    memblock_rob_key_t survivor_key;
    memblock_rob_key_t victim_key;
    longint unsigned anchor_sample_seq;

    reset_directed_owner_state();
    run_rob_wrap_admission_fatal_scenario("r2-survivor-with-deleted-history-fatal");
endtask:run_r2_survivor_with_deleted_history_fatal_scenario

// 中文注释：真实redirect flush先删除active owner后，R/R+1/R+2的旧loadMmio都必须由
// status与redirect-deleted LQ owner联合唯一证明，并只丢raw、不写MMIO tag。
task soft_test_memblock_pending_mmio_directed_sequence::run_deleted_load_stale_scenario(
    input int unsigned sample_offset
);
    memblock_issue_q_item_t fired_items[$];
    memblock_uid_t victim_uids[$];
    memblock_redirect_payload_t redirect;
    memblock_rob_key_t owner_key;
    longint unsigned anchor_sample_seq;
    string scenario_name;

    if (sample_offset > MEMBLOCK_LOAD_MMIO_TEST_MAX_SAMPLE_OFFSET) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("deleted-load stale scenario offset=%0d exceeds overlap window",
                             sample_offset))
    end
    scenario_name = $sformatf("r%0d-deleted-load-stale", sample_offset);

    reset_directed_owner_state();
    build_load_table(2);
    admit_lsq_and_route_issue();
    fire_all_issue_items(fired_items);
    owner_key = data.get_status(1).get_rob_key();
    victim_uids.push_back(1);

    redirect = '{default:'0};
    redirect.valid = 1'b1;
    redirect.flush_itself = 1'b1;
    redirect.level = 1'b1;
    redirect.rob_key = owner_key;
    prepare_redirect_deleted_owner_window(victim_uids, redirect,
                                          scenario_name,
                                          anchor_sample_seq);
    if (data.get_status(1).active || !data.get_status(1).flushed ||
        !data.get_status(1).issue_killed ||
        !data.get_status(1).redirect_pending) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("R+%0d stale scenario did not preserve inactive flushed/killed history",
                             sample_offset))
    end
    apply_load_mmio_raw(owner_key.value, anchor_sample_seq + sample_offset,
                        scenario_name);
    if (data.get_status(1).mmio_tag_valid) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("R+%0d stale loadMmio incorrectly tagged deleted uid",
                             sample_offset))
    end
endtask:run_deleted_load_stale_scenario

task soft_test_memblock_pending_mmio_directed_sequence::run_r3_deleted_load_fatal_scenario();
    memblock_issue_q_item_t fired_items[$];
    memblock_uid_t victim_uids[$];
    memblock_redirect_payload_t redirect;
    memblock_rob_key_t owner_key;
    longint unsigned anchor_sample_seq;

    reset_directed_owner_state();
    build_load_table(2);
    admit_lsq_and_route_issue();
    fire_all_issue_items(fired_items);
    owner_key = data.get_status(1).get_rob_key();
    victim_uids.push_back(1);
    redirect = '{default:'0};
    redirect.valid = 1'b1;
    redirect.flush_itself = 1'b1;
    redirect.level = 1'b1;
    redirect.rob_key = owner_key;
    prepare_redirect_deleted_owner_window(victim_uids, redirect,
                                          "r3-deleted-load-fatal",
                                          anchor_sample_seq);
    apply_load_mmio_raw(owner_key.value, anchor_sample_seq + 3,
                        "r3-deleted-load-fatal", 1'b1,
                        "*cannot prove MMIO raw ownership*");
    if (data.get_status(1).mmio_tag_valid) begin
        `uvm_fatal(get_type_name(), "R+3 loadMmio fatal path wrote an MMIO tag")
    end
endtask:run_r3_deleted_load_fatal_scenario

task soft_test_memblock_pending_mmio_directed_sequence::run_r2_no_history_fatal_scenario();
    memblock_redirect_payload_t redirect;
    memblock_rob_key_t owner_key;
    longint unsigned anchor_sample_seq;

    reset_directed_owner_state();
    build_load_table(2);
    owner_key = data.get_status(1).get_rob_key();
    redirect = '{default:'0};
    redirect.valid = 1'b1;
    redirect.flush_itself = 1'b1;
    redirect.level = 1'b1;
    redirect.rob_key = owner_key;
    establish_redirect_anchor(redirect, "r2-no-history-fatal", anchor_sample_seq);
    apply_load_mmio_raw(owner_key.value, anchor_sample_seq + 2,
                        "r2-no-history-fatal", 1'b1,
                        "*cannot prove LOAD MMIO stale ownership in redirect overlap*");
endtask:run_r2_no_history_fatal_scenario

// 中文注释：overlap 只能命中 redirect epoch 内新建实例时，旧 owner 已无法唯一证明；
// resolver必须 fatal，不能把迟到 loadMmio 静默写到新实例。
task soft_test_memblock_pending_mmio_directed_sequence::run_new_owner_overlap_fatal_scenario();
    main_control_transaction main_tr;
    memblock_op_behavior_t behavior;
    memblock_issue_q_item_t fired_items[$];
    memblock_uid_t victim_uids[$];
    memblock_redirect_payload_t redirect;
    memblock_rob_key_t owner_key;
    longint unsigned anchor_sample_seq;

    reset_directed_owner_state();
    build_load_table(2);
    admit_lsq_and_route_issue();
    fire_all_issue_items(fired_items);
    main_tr = data.get_main_transaction(1);
    behavior = derive_op_behavior(main_tr);
    owner_key = data.get_status(1).get_rob_key();
    victim_uids.push_back(1);

    redirect = '{default:'0};
    redirect.valid = 1'b1;
    redirect.flush_itself = 1'b1;
    redirect.level = 1'b1;
    redirect.is_vls_exception = 1'b0;
    redirect.rob_key = owner_key;
    prepare_redirect_deleted_owner_window(victim_uids, redirect,
                                          "r2-new-owner-fatal",
                                          anchor_sample_seq);

    // redirect完成后用正式admission/issue API建立同ROB key的新动态实例。
    lsq_ctrl.commit_allocate(1, behavior, main_tr);
    prepare_issue_route_for_uid(1);
    route_all_issue_queues();
    fire_all_issue_items(fired_items);
    apply_load_mmio_raw(owner_key.value, anchor_sample_seq + 2,
                        "r2-new-owner-fatal", 1'b1,
                        "*cannot prove LOAD MMIO stale ownership*");
    if (data.get_status(1).mmio_tag_valid) begin
        `uvm_fatal(get_type_name(),
                   "R+2 new-owner fatal path incorrectly tagged the new instance")
    end
endtask:run_new_owner_overlap_fatal_scenario

task soft_test_memblock_pending_mmio_directed_sequence::run_r2_wrap_ambiguity_fatal_scenario();
    reset_directed_owner_state();
    run_rob_wrap_admission_fatal_scenario("r2-wrap-ambiguity-fatal");
endtask:run_r2_wrap_ambiguity_fatal_scenario

task soft_test_memblock_pending_mmio_directed_sequence::run_r2_non_load_fatal_scenario();
    memblock_issue_q_item_t fired_items[$];
    memblock_uid_t victim_uids[$];
    memblock_redirect_payload_t redirect;
    memblock_rob_key_t owner_key;
    longint unsigned anchor_sample_seq;

    reset_directed_owner_state();
    build_store_table();
    admit_lsq_and_route_issue();
    fire_all_issue_items(fired_items);
    owner_key = data.get_status(1).get_rob_key();
    victim_uids.push_back(1);
    redirect = '{default:'0};
    redirect.valid = 1'b1;
    redirect.flush_itself = 1'b1;
    redirect.level = 1'b1;
    redirect.rob_key = owner_key;
    prepare_redirect_deleted_owner_window(victim_uids, redirect,
                                          "r2-non-load-fatal",
                                          anchor_sample_seq);
    apply_load_mmio_raw(owner_key.value, anchor_sample_seq + 2,
                        "r2-non-load-fatal", 1'b1,
                        "*cannot prove LOAD MMIO stale ownership in redirect overlap*");
endtask:run_r2_non_load_fatal_scenario

task soft_test_memblock_pending_mmio_directed_sequence::run_fault_head_suppress_scenario();
    memblock_issue_q_item_t fired_items[$];
    lsqcommit_agent_agent_xaction tr;
    memblock_uid_t commit_uids[$];
    memblock_lq_key_t lq_deq_head;
    bit has_commit;
    bit has_fault_head;
    memblock_uid_t fault_uid;

    reset_directed_owner_state();
    build_load_table(2);
    admit_lsq_and_route_issue();
    fire_all_issue_items(fired_items);
    data.set_uid_mmio_tag(0, MEMBLOCK_MMIO_KIND_LOAD,
                          MEMBLOCK_MMIO_TAG_DIRECTED);
    data.set_uid_mmio_tag(1, MEMBLOCK_MMIO_KIND_LOAD,
                          MEMBLOCK_MMIO_TAG_DIRECTED);

    foreach (fired_items[idx]) begin
        memblock_wb_event_t wb_event;

        wb_event = make_pass_wb_event(fired_items[idx]);
        if (fired_items[idx].uid == 0 &&
            fired_items[idx].target == MEMBLOCK_ISSUE_TARGET_LOAD) begin
            wb_event.has_exception = 1'b1;
            wb_event.exception_vec = 24'h1;
        end
        submit_writeback_event(wb_event);
    end

    commit_handler.build_lsqcommit_xaction(tr, commit_uids, has_commit,
                                           has_fault_head, fault_uid);
    if (has_commit || !has_fault_head || fault_uid != 0 ||
        tr.io_ooo_to_mem_lsqio_pendingMMIOld ||
        tr.io_ooo_to_mem_lsqio_pendingst ||
        tr.io_ooo_to_mem_lsqio_scommit != '0) begin
        `uvm_fatal(get_type_name(),
                   "fault-head owner did not suppress pendingMMIOld/pendingst/scommit")
    end

    commit_handler.mark_fault_rob_commit_uid(fault_uid);
    lq_deq_head = lsq_ctrl.lq_deq_ptr;
    commit_handler.apply_dut_lq_deq(1, lq_deq_head, 1'b0);
    check_load_head_sideband(1, "post-fault-rebase");

    commit_handler.build_lsqcommit_xaction(tr, commit_uids, has_commit,
                                           has_fault_head, fault_uid);
    if (!has_commit || has_fault_head || commit_uids.size() != 1 ||
        commit_uids[0] != 1) begin
        `uvm_fatal(get_type_name(), "post-fault head did not enter normal commit owner flow")
    end
    commit_handler.mark_rob_commit_batch(commit_uids);
    lq_deq_head = lsq_ctrl.lq_deq_ptr;
    commit_handler.apply_dut_lq_deq(1, lq_deq_head, 1'b0);
    if (!data.get_status(0).terminal_done ||
        !data.get_status(1).terminal_done ||
        data.uid_by_active_rob.num() != 0 ||
        data.uid_by_lq.num() != 0) begin
        `uvm_fatal(get_type_name(), "fault/rebase directed scenario did not drain owner state")
    end
endtask:run_fault_head_suppress_scenario

task soft_test_memblock_pending_mmio_directed_sequence::run_commit_batch_redirect_survivor_scenario();
    memblock_issue_q_item_t fired_items[$];
    memblock_wb_event_t wb_event;
    lsqcommit_agent_agent_xaction tr;
    memblock_uid_t commit_uids[$];
    memblock_redirect_payload_t redirect;
    bit has_commit;
    bit has_fault_head;
    memblock_uid_t fault_uid;

    reset_directed_owner_state();
    build_load_table(2);
    admit_lsq_and_route_issue();
    fire_all_issue_items(fired_items);
    foreach (fired_items[idx]) begin
        if (fired_items[idx].uid == 0) begin
            wb_event = make_pass_wb_event(fired_items[idx]);
            submit_writeback_event(wb_event);
        end
    end
    commit_handler.build_lsqcommit_xaction(tr, commit_uids, has_commit,
                                           has_fault_head, fault_uid);
    if (!has_commit || has_fault_head || commit_uids.size() != 1 ||
        commit_uids[0] != 0) begin
        `uvm_fatal(get_type_name(),
                   "redirect survivor scenario did not select uid0 normal head")
    end
    redirect = '{default:'0};
    redirect.valid = 1'b1;
    redirect.rob_key = data.get_status(1).get_rob_key();
    redirect.flush_itself = 1'b1;
    redirect.level = 1'b1;
    data.request_redirect_flush(redirect);
    if (!commit_handler.mark_rob_commit_batch(commit_uids) ||
        !data.get_status(0).rob_commit ||
        commit_handler.commit_cursor_uid != 1 ||
        data.dispatch_progress.rob_commit_frontier_uid != 1) begin
        `uvm_fatal(get_type_name(),
                   "older normal head did not commit atomically during younger redirect")
    end
endtask:run_commit_batch_redirect_survivor_scenario

task soft_test_memblock_pending_mmio_directed_sequence::run_commit_batch_redirect_mixed_cancel_scenario();
    memblock_issue_q_item_t fired_items[$];
    memblock_wb_event_t wb_event;
    lsqcommit_agent_agent_xaction tr;
    memblock_uid_t commit_uids[$];
    memblock_redirect_payload_t redirect;
    bit has_commit;
    bit has_fault_head;
    memblock_uid_t fault_uid;

    reset_directed_owner_state();
    build_load_table(2);
    admit_lsq_and_route_issue();
    fire_all_issue_items(fired_items);
    foreach (fired_items[idx]) begin
        wb_event = make_pass_wb_event(fired_items[idx]);
        submit_writeback_event(wb_event);
    end
    commit_handler.build_lsqcommit_xaction(tr, commit_uids, has_commit,
                                           has_fault_head, fault_uid);
    if (!has_commit || has_fault_head || commit_uids.size() != 2) begin
        `uvm_fatal(get_type_name(),
                   "redirect mixed scenario did not select two-entry batch")
    end
    redirect = '{default:'0};
    redirect.valid = 1'b1;
    redirect.rob_key = data.get_status(1).get_rob_key();
    redirect.flush_itself = 1'b1;
    redirect.level = 1'b1;
    data.request_redirect_flush(redirect);
    if (commit_handler.mark_rob_commit_batch(commit_uids) ||
        data.get_status(0).rob_commit || data.get_status(1).rob_commit ||
        commit_handler.commit_cursor_uid != 0 ||
        data.dispatch_progress.rob_commit_frontier_uid != 0) begin
        `uvm_fatal(get_type_name(),
                   "mixed survivor/covered batch was not canceled with zero side effects")
    end
endtask:run_commit_batch_redirect_mixed_cancel_scenario

task soft_test_memblock_pending_mmio_directed_sequence::run_global_stop_raw_drain_scenario();
    memblock_issue_q_item_t fired_items[$];
    memblock_sync_pkg::dispatch_raw_ctrl_t raw;
    memblock_sync_pkg::dispatch_raw_ctrl_t popped_raw;
    memblock_wb_event_t wb_event;
    lsqcommit_agent_agent_xaction tr;
    memblock_uid_t commit_uids[$];
    memblock_lq_key_t lq_deq_head;
    bit has_commit;
    bit has_fault_head;
    memblock_uid_t fault_uid;

    reset_directed_owner_state();
    build_load_table(2);
    admit_lsq_and_route_issue();
    fire_all_issue_items(fired_items);
    if (fired_items.size() != 2) begin
        `uvm_fatal(get_type_name(), "global-stop directed scenario expected two fired loads")
    end
    foreach (fired_items[idx]) begin
        if (fired_items[idx].target != MEMBLOCK_ISSUE_TARGET_LOAD) begin
            `uvm_fatal(get_type_name(),
                       "global-stop directed scenario got a non-load issue item")
        end
        wb_event = make_pass_wb_event(fired_items[idx]);
        submit_writeback_event(wb_event);
    end
    commit_handler.build_lsqcommit_xaction(tr, commit_uids, has_commit,
                                           has_fault_head, fault_uid);
    if (!has_commit || has_fault_head || commit_uids.size() != 2 ||
        commit_uids[0] != 0 || commit_uids[1] != 1) begin
        `uvm_fatal(get_type_name(),
                   "global-stop directed scenario did not select both commits")
    end
    commit_handler.mark_rob_commit_batch(commit_uids);
    lq_deq_head = lsq_ctrl.lq_deq_ptr;
    commit_handler.apply_dut_lq_deq(2, lq_deq_head, 1'b0);
    if (!data.transaction_done() || !data.runtime_drain_complete()) begin
        `uvm_fatal(get_type_name(), "global-stop directed scenario did not reach drained terminal state")
    end

    raw = memblock_sync_pkg::make_empty_raw_ctrl();
    raw.valid = 1'b1;
    raw.cycle = $time;
    memblock_sync_pkg::push_raw_ctrl(raw);
    data.request_global_stop_if_done();
    if (data.is_global_stop_requested() || data.runtime_drain_complete()) begin
        `uvm_fatal(get_type_name(), "global stop ignored a pending raw monitor item")
    end
    if (!memblock_sync_pkg::pop_raw_ctrl(popped_raw) || !popped_raw.valid) begin
        `uvm_fatal(get_type_name(), "failed to drain directed raw monitor item")
    end
    data.request_global_stop_if_done();
    if (!data.is_global_stop_requested() || !data.runtime_drain_complete()) begin
        `uvm_fatal(get_type_name(), "global stop was not requested after runtime drain completed")
    end
endtask:run_global_stop_raw_drain_scenario

`endif
