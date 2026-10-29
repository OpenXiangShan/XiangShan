//=========================================================
//File name    : soft_test_memblock_rob_admission_window_sequence.sv
//Author       : OpenAI_Codex
//Module name  : soft_test_memblock_rob_admission_window_sequence
//Discribution : software-only ROB admission boundary checks
//Date         : 2026-10-13
//=========================================================
`ifndef SOFT_TEST_MEMBLOCK_ROB_ADMISSION_WINDOW_SEQUENCE__SV
`define SOFT_TEST_MEMBLOCK_ROB_ADMISSION_WINDOW_SEQUENCE__SV

class memblock_rob_admission_expected_fatal_catcher extends uvm_report_catcher;
    int unsigned caught_count;
    string expected_id;

    function new(string name = "memblock_rob_admission_expected_fatal_catcher",
                 string expected_id_i = "ROB_ADMISSION_WINDOW");
        super.new(name);
        caught_count = 0;
        expected_id = expected_id_i;
    endfunction:new

    virtual function action_e catch();
        if (get_severity() == UVM_FATAL &&
            get_id() == expected_id) begin
            caught_count++;
            return CAUGHT;
        end
        return THROW;
    endfunction:catch
endclass:memblock_rob_admission_expected_fatal_catcher

// Test-only full-state snapshot used by lifecycle duplicate cases.  It copies
// every mutable container and owner-visible field that a production entry may
// touch, so a caught fatal proves the complete transaction remained atomic.
class memblock_control_atomic_snapshot;
    memblock_csr_control_action_t       csr_q[$];
    memblock_sfence_control_action_t    sfence_q[$];
    memblock_l2_flush_release_request_t release_q[$];
    memblock_flushsb_req_t              flushsb_q[$];
    memblock_uid_t uid_by_active_rob[memblock_rob_map_key_t];
    memblock_uid_t uid_by_lq[memblock_lq_map_key_t];
    memblock_uid_t uid_by_sq[memblock_sq_map_key_t];
    memblock_control_sfence_observation_t sfence_c0_observation;
    memblock_control_sfence_observation_t sfence_effective_observation;
    memblock_flushsb_req_t attached_flushsb_req;
    bit attached_flushsb_req_valid;
    memblock_flushsb_req_t active_flushsb_req;
    bit active_flushsb_req_valid;
    memblock_flushsb_completion_t flushsb_completed;
    int unsigned next_flushsb_req_id;
    bit flushsb_waiting_empty;
    longint unsigned flushsb_start_cycle;
    bit last_sb_is_empty;
    bit flushsb_timeout_warned;
    memblock_uid_t terminal_done_uid;
    bit barrier_valid;
    memblock_uid_t barrier_uid;

    bit status_active;
    bit status_success;
    bit status_terminal_done;
    bit status_active_lq_mapped;
    bit status_active_sq_mapped;
    bit status_rob_commit;
    memblock_control_owner_t status_owner;
    int unsigned status_generation;
    memblock_control_state_e status_state;
    bit status_action_enqueued;
    int unsigned status_reset_epoch;
    int unsigned audit_creation;
    int unsigned audit_owner_bind;
    int unsigned audit_csr_enqueue;
    int unsigned audit_csr_start;
    int unsigned audit_csr_completion;
    int unsigned audit_sfence_enqueue;
    int unsigned audit_sfence_start;
    int unsigned audit_sfence_completion;
    int unsigned audit_l2_assert_enqueue;
    int unsigned audit_l2_assert_start;
    int unsigned audit_l2_assert_completion;
    int unsigned audit_l2_release_enqueue;
    int unsigned audit_l2_release_start;
    int unsigned audit_l2_release_completion;
    int unsigned audit_terminal;
    int unsigned audit_barrier_release;
    bit runtime_csr_snapshot_valid;
    memblock_sync_pkg::dispatch_raw_csr_t runtime_csr_snapshot;
    int unsigned runtime_csr_snapshot_seq;
    int unsigned runtime_snapshot_seq_before_drive;
    bit expected_runtime_csr_valid;
    memblock_sync_pkg::dispatch_raw_csr_t expected_runtime_csr;
    bit expected_sfence_valid;
    memblock_sfence_payload_t expected_sfence;
    bit l2_csr_baseline_valid;
    memblock_sync_pkg::dispatch_raw_csr_t l2_csr_baseline;
    int unsigned flushsb_req_id;
    bit flushsb_request_queued;
    bit sfence_c0_armed;
    longint unsigned sfence_pre_drive_event_seq;
    longint unsigned sfence_c0_event_seq;
    longint unsigned l2tlb_reset_epoch_at_arm;
    longint unsigned assert_done_baseline_seq;
    longint unsigned release_done_baseline_seq;
    longint unsigned last_event_cycle;

    bit package_runtime_snapshot_valid;
    memblock_sync_pkg::dispatch_raw_csr_t package_runtime_snapshot;
    int unsigned package_runtime_snapshot_seq;
    memblock_sync_pkg::memblock_control_csr_runtime_baseline_t package_csr_baseline;
    memblock_sync_pkg::memblock_control_level_observation_t package_l2_observation;

    function new();
    endfunction:new

    function void capture(common_data_transaction data,
                          status_transaction status);
        csr_q = data.csr_control_action_q;
        sfence_q = data.sfence_control_action_q;
        release_q = data.l2_flush_release_request_q;
        flushsb_q = data.flushsb_req_q;
        uid_by_active_rob.delete();
        foreach (data.uid_by_active_rob[key]) uid_by_active_rob[key] = data.uid_by_active_rob[key];
        uid_by_lq.delete();
        foreach (data.uid_by_lq[key]) uid_by_lq[key] = data.uid_by_lq[key];
        uid_by_sq.delete();
        foreach (data.uid_by_sq[key]) uid_by_sq[key] = data.uid_by_sq[key];
        sfence_c0_observation = data.control_sfence_c0_observation;
        sfence_effective_observation = data.control_sfence_effective_observation;
        attached_flushsb_req = data.attached_flushsb_req;
        attached_flushsb_req_valid = data.attached_flushsb_req_valid;
        active_flushsb_req = data.active_flushsb_req;
        active_flushsb_req_valid = data.active_flushsb_req_valid;
        flushsb_completed = data.flushsb_completed;
        next_flushsb_req_id = data.next_flushsb_req_id;
        flushsb_waiting_empty = data.flushsb_waiting_empty;
        flushsb_start_cycle = data.flushsb_start_cycle;
        last_sb_is_empty = data.last_sb_is_empty;
        flushsb_timeout_warned = data.flushsb_timeout_warned;
        terminal_done_uid = data.dispatch_progress.terminal_done_uid;
        barrier_valid = data.active_control_barrier_valid;
        barrier_uid = data.active_control_barrier_uid;

        status_active = status.active;
        status_success = status.success;
        status_terminal_done = status.terminal_done;
        status_active_lq_mapped = status.active_lq_mapped;
        status_active_sq_mapped = status.active_sq_mapped;
        status_rob_commit = status.rob_commit;
        status_owner = status.control_owner;
        status_generation = status.control_action_generation;
        status_state = status.control_state;
        status_action_enqueued = status.control_action_enqueued;
        status_reset_epoch = status.control_reset_epoch;
        audit_creation = status.control_audit_creation_count;
        audit_owner_bind = status.control_audit_owner_bind_count;
        audit_csr_enqueue = status.control_audit_csr_enqueue_count;
        audit_csr_start = status.control_audit_csr_start_count;
        audit_csr_completion = status.control_audit_csr_completion_count;
        audit_sfence_enqueue = status.control_audit_sfence_enqueue_count;
        audit_sfence_start = status.control_audit_sfence_start_count;
        audit_sfence_completion = status.control_audit_sfence_completion_count;
        audit_l2_assert_enqueue = status.control_audit_l2_assert_enqueue_count;
        audit_l2_assert_start = status.control_audit_l2_assert_start_count;
        audit_l2_assert_completion = status.control_audit_l2_assert_completion_count;
        audit_l2_release_enqueue = status.control_audit_l2_release_enqueue_count;
        audit_l2_release_start = status.control_audit_l2_release_start_count;
        audit_l2_release_completion = status.control_audit_l2_release_completion_count;
        audit_terminal = status.control_audit_terminal_count;
        audit_barrier_release = status.control_audit_barrier_release_count;
        runtime_csr_snapshot_valid = status.control_runtime_csr_snapshot_valid;
        runtime_csr_snapshot = status.control_runtime_csr_snapshot;
        runtime_csr_snapshot_seq = status.control_runtime_csr_snapshot_seq;
        runtime_snapshot_seq_before_drive = status.control_runtime_snapshot_seq_before_drive;
        expected_runtime_csr_valid = status.control_expected_runtime_csr_valid;
        expected_runtime_csr = status.control_expected_runtime_csr;
        expected_sfence_valid = status.control_expected_sfence_valid;
        expected_sfence = status.control_expected_sfence;
        l2_csr_baseline_valid = status.control_l2_csr_baseline_valid;
        l2_csr_baseline = status.control_l2_csr_baseline;
        flushsb_req_id = status.control_flushsb_req_id;
        flushsb_request_queued = status.control_flushsb_request_queued;
        sfence_c0_armed = status.control_sfence_c0_armed;
        sfence_pre_drive_event_seq = status.control_sfence_pre_drive_event_seq;
        sfence_c0_event_seq = status.control_sfence_c0_event_seq;
        l2tlb_reset_epoch_at_arm = status.control_l2tlb_reset_epoch_at_arm;
        assert_done_baseline_seq = status.control_assert_done_baseline_seq;
        release_done_baseline_seq = status.control_release_done_baseline_seq;
        last_event_cycle = status.last_event_cycle;

        package_runtime_snapshot_valid = memblock_sync_pkg::runtime_csr_snapshot_valid;
        package_runtime_snapshot = memblock_sync_pkg::runtime_csr_snapshot;
        package_runtime_snapshot_seq = memblock_sync_pkg::runtime_csr_snapshot_seq;
        package_csr_baseline = memblock_sync_pkg::control_csr_runtime_baseline;
        package_l2_observation = memblock_sync_pkg::control_l2_flush_done_observation;
    endfunction:capture

    function bit state_matches(common_data_transaction data,
                               status_transaction status);
        if (csr_q.size() != data.csr_control_action_q.size() ||
            sfence_q.size() != data.sfence_control_action_q.size() ||
            release_q.size() != data.l2_flush_release_request_q.size() ||
            flushsb_q.size() != data.flushsb_req_q.size()) return 1'b0;
        foreach (csr_q[idx]) if (csr_q[idx] != data.csr_control_action_q[idx]) return 1'b0;
        foreach (sfence_q[idx]) if (sfence_q[idx] != data.sfence_control_action_q[idx]) return 1'b0;
        foreach (release_q[idx]) if (release_q[idx] != data.l2_flush_release_request_q[idx]) return 1'b0;
        foreach (flushsb_q[idx]) if (flushsb_q[idx] != data.flushsb_req_q[idx]) return 1'b0;
        if (uid_by_active_rob.num() != data.uid_by_active_rob.num() ||
            uid_by_lq.num() != data.uid_by_lq.num() ||
            uid_by_sq.num() != data.uid_by_sq.num()) return 1'b0;
        foreach (uid_by_active_rob[key]) begin
            if (!data.uid_by_active_rob.exists(key) ||
                data.uid_by_active_rob[key] != uid_by_active_rob[key]) return 1'b0;
        end
        foreach (uid_by_lq[key]) begin
            if (!data.uid_by_lq.exists(key) || data.uid_by_lq[key] != uid_by_lq[key]) return 1'b0;
        end
        foreach (uid_by_sq[key]) begin
            if (!data.uid_by_sq.exists(key) || data.uid_by_sq[key] != uid_by_sq[key]) return 1'b0;
        end
        return sfence_c0_observation == data.control_sfence_c0_observation &&
               sfence_effective_observation == data.control_sfence_effective_observation &&
               attached_flushsb_req == data.attached_flushsb_req &&
               attached_flushsb_req_valid == data.attached_flushsb_req_valid &&
               active_flushsb_req == data.active_flushsb_req &&
               active_flushsb_req_valid == data.active_flushsb_req_valid &&
               flushsb_completed == data.flushsb_completed &&
               next_flushsb_req_id == data.next_flushsb_req_id &&
               flushsb_waiting_empty == data.flushsb_waiting_empty &&
               flushsb_start_cycle == data.flushsb_start_cycle &&
               last_sb_is_empty == data.last_sb_is_empty &&
               flushsb_timeout_warned == data.flushsb_timeout_warned &&
               terminal_done_uid == data.dispatch_progress.terminal_done_uid &&
               barrier_valid == data.active_control_barrier_valid &&
               barrier_uid == data.active_control_barrier_uid &&
               status_active == status.active && status_success == status.success &&
               status_terminal_done == status.terminal_done &&
               status_active_lq_mapped == status.active_lq_mapped &&
               status_active_sq_mapped == status.active_sq_mapped &&
               status_rob_commit == status.rob_commit && status_owner == status.control_owner &&
               status_generation == status.control_action_generation &&
               status_state == status.control_state &&
               status_action_enqueued == status.control_action_enqueued &&
               status_reset_epoch == status.control_reset_epoch &&
               audit_creation == status.control_audit_creation_count &&
               audit_owner_bind == status.control_audit_owner_bind_count &&
               audit_csr_enqueue == status.control_audit_csr_enqueue_count &&
               audit_csr_start == status.control_audit_csr_start_count &&
               audit_csr_completion == status.control_audit_csr_completion_count &&
               audit_sfence_enqueue == status.control_audit_sfence_enqueue_count &&
               audit_sfence_start == status.control_audit_sfence_start_count &&
               audit_sfence_completion == status.control_audit_sfence_completion_count &&
               audit_l2_assert_enqueue == status.control_audit_l2_assert_enqueue_count &&
               audit_l2_assert_start == status.control_audit_l2_assert_start_count &&
               audit_l2_assert_completion == status.control_audit_l2_assert_completion_count &&
               audit_l2_release_enqueue == status.control_audit_l2_release_enqueue_count &&
               audit_l2_release_start == status.control_audit_l2_release_start_count &&
               audit_l2_release_completion == status.control_audit_l2_release_completion_count &&
               audit_terminal == status.control_audit_terminal_count &&
               audit_barrier_release == status.control_audit_barrier_release_count &&
               runtime_csr_snapshot_valid == status.control_runtime_csr_snapshot_valid &&
               runtime_csr_snapshot == status.control_runtime_csr_snapshot &&
               runtime_csr_snapshot_seq == status.control_runtime_csr_snapshot_seq &&
               runtime_snapshot_seq_before_drive == status.control_runtime_snapshot_seq_before_drive &&
               expected_runtime_csr_valid == status.control_expected_runtime_csr_valid &&
               expected_runtime_csr == status.control_expected_runtime_csr &&
               expected_sfence_valid == status.control_expected_sfence_valid &&
               expected_sfence == status.control_expected_sfence &&
               l2_csr_baseline_valid == status.control_l2_csr_baseline_valid &&
               l2_csr_baseline == status.control_l2_csr_baseline &&
               flushsb_req_id == status.control_flushsb_req_id &&
               flushsb_request_queued == status.control_flushsb_request_queued &&
               sfence_c0_armed == status.control_sfence_c0_armed &&
               sfence_pre_drive_event_seq == status.control_sfence_pre_drive_event_seq &&
               sfence_c0_event_seq == status.control_sfence_c0_event_seq &&
               l2tlb_reset_epoch_at_arm == status.control_l2tlb_reset_epoch_at_arm &&
               assert_done_baseline_seq == status.control_assert_done_baseline_seq &&
               release_done_baseline_seq == status.control_release_done_baseline_seq &&
               last_event_cycle == status.last_event_cycle &&
               package_runtime_snapshot_valid == memblock_sync_pkg::runtime_csr_snapshot_valid &&
               package_runtime_snapshot == memblock_sync_pkg::runtime_csr_snapshot &&
               package_runtime_snapshot_seq == memblock_sync_pkg::runtime_csr_snapshot_seq &&
               package_csr_baseline == memblock_sync_pkg::control_csr_runtime_baseline &&
               package_l2_observation == memblock_sync_pkg::control_l2_flush_done_observation;
    endfunction:state_matches
endclass:memblock_control_atomic_snapshot

class soft_test_memblock_rob_admission_window_sequence extends memblock_dispatch_base_sequence;

    typedef struct packed {
        int unsigned creation;
        int unsigned owner_bind;
        int unsigned csr_enqueue;
        int unsigned csr_start;
        int unsigned csr_completion;
        int unsigned sfence_enqueue;
        int unsigned sfence_start;
        int unsigned sfence_completion;
        int unsigned l2_assert_enqueue;
        int unsigned l2_assert_start;
        int unsigned l2_assert_completion;
        int unsigned l2_release_enqueue;
        int unsigned l2_release_start;
        int unsigned l2_release_completion;
        int unsigned terminal_count;
        int unsigned barrier_release;
    } memblock_control_audit_snapshot_t;

    `uvm_object_utils(soft_test_memblock_rob_admission_window_sequence)

    extern function new(string name = "soft_test_memblock_rob_admission_window_sequence");
    extern virtual task body();
    extern virtual task run_boundary_checks();
    extern virtual task run_redirect_later_split_cancel_checks();
    extern virtual function memblock_later_split_tlb_record build_later_split_pending_owner(
        input memblock_uid_t uid,
        input longint unsigned token,
        input longint unsigned sample_seq
    );
    extern virtual task run_redirect_caught_fatal_atomicity_checks();
    extern virtual task run_redirect_caught_fatal_atomicity_case(
        input bit inject_late_prefix_failure,
        input string scenario_name
    );
    extern virtual task run_control_lifecycle_duplicate_checks();
    extern virtual task run_sfence_lifecycle_duplicate_checks(
        input memblock_control_barrier_service service
    );
    extern virtual task run_check_store_lifecycle_duplicate_checks(
        input memblock_control_barrier_service service
    );
    extern virtual task build_boundary_main_table();
    extern virtual function memblock_control_audit_snapshot_t
        snapshot_control_audit_counts(input status_transaction status);

endclass:soft_test_memblock_rob_admission_window_sequence

function soft_test_memblock_rob_admission_window_sequence::new(
    string name = "soft_test_memblock_rob_admission_window_sequence"
);
    super.new(name);
endfunction:new

function soft_test_memblock_rob_admission_window_sequence::memblock_control_audit_snapshot_t
soft_test_memblock_rob_admission_window_sequence::snapshot_control_audit_counts(
    input status_transaction status
);
    memblock_control_audit_snapshot_t snapshot;

    snapshot = '{
        creation: status.control_audit_creation_count,
        owner_bind: status.control_audit_owner_bind_count,
        csr_enqueue: status.control_audit_csr_enqueue_count,
        csr_start: status.control_audit_csr_start_count,
        csr_completion: status.control_audit_csr_completion_count,
        sfence_enqueue: status.control_audit_sfence_enqueue_count,
        sfence_start: status.control_audit_sfence_start_count,
        sfence_completion: status.control_audit_sfence_completion_count,
        l2_assert_enqueue: status.control_audit_l2_assert_enqueue_count,
        l2_assert_start: status.control_audit_l2_assert_start_count,
        l2_assert_completion: status.control_audit_l2_assert_completion_count,
        l2_release_enqueue: status.control_audit_l2_release_enqueue_count,
        l2_release_start: status.control_audit_l2_release_start_count,
        l2_release_completion: status.control_audit_l2_release_completion_count,
        terminal_count: status.control_audit_terminal_count,
        barrier_release: status.control_audit_barrier_release_count
    };
    return snapshot;
endfunction:snapshot_control_audit_counts

task soft_test_memblock_rob_admission_window_sequence::body();
    seq_csr_common::init();
    data = common_data_transaction::get();
    lsq_ctrl = lsq_ctrl_model::get();
    if (data == null || lsq_ctrl == null) begin
        `uvm_fatal(get_type_name(), "failed to initialize ROB admission boundary helpers")
    end
    lsq_ctrl.reset();
    build_boundary_main_table();
    run_boundary_checks();
    run_redirect_later_split_cancel_checks();
    run_redirect_caught_fatal_atomicity_checks();
    run_control_lifecycle_duplicate_checks();
    `uvm_info(get_type_name(), "ROB admission window boundary checks completed", UVM_LOW)
endtask:body

task soft_test_memblock_rob_admission_window_sequence::build_boundary_main_table();
    build_random_main_table(MEMBLOCK_ROB_SIZE + 2);
    if (!data.main_table_ready || data.main_trans_num != MEMBLOCK_ROB_SIZE + 2) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("boundary main table was not built: ready=%0d count=%0d",
                             data.main_table_ready, data.main_trans_num))
    end
endtask:build_boundary_main_table

task soft_test_memblock_rob_admission_window_sequence::run_boundary_checks();
    memblock_uid_t frontier_uid;
    memblock_uid_t candidate_uid;
    memblock_rob_key_t frontier_key;
    memblock_rob_key_t candidate_key;
    memblock_rob_admission_expected_fatal_catcher catcher;

    // 模拟 ROB head 未提交、但 LSQ admission prefix 已经推进到窗口边界前一项。
    for (int unsigned uid = 0; uid < MEMBLOCK_ROB_SIZE - 1; uid++) begin
        data.mark_uid_enqueued(uid);
    end
    frontier_uid = data.dispatch_progress.rob_commit_frontier_uid;
    if (frontier_uid != 0 || data.get_next_new_admit_uid() != MEMBLOCK_ROB_SIZE - 1) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("unexpected boundary setup frontier=%0d next_admit=%0d",
                             frontier_uid, data.get_next_new_admit_uid()))
    end

    candidate_uid = MEMBLOCK_ROB_SIZE - 1;
    if (!data.rob_admission_window_has_room(candidate_uid, "rob_boundary_distance_159")) begin
        `uvm_fatal(get_type_name(), "distance=159 candidate was incorrectly blocked")
    end
    frontier_key = data.get_main_transaction(frontier_uid).get_rob_key();
    candidate_key = data.get_main_transaction(candidate_uid).get_rob_key();
    if (candidate_key != rob_order_util::rob_advance(frontier_key, MEMBLOCK_ROB_SIZE - 1)) begin
        `uvm_fatal(get_type_name(), "distance=159 ROB key continuity check failed")
    end

    candidate_uid = MEMBLOCK_ROB_SIZE;
    if (data.rob_admission_window_has_room(candidate_uid, "rob_boundary_distance_160")) begin
        `uvm_fatal(get_type_name(), "distance=160 candidate was incorrectly admitted")
    end

    catcher = new();
    uvm_report_cb::add(null, catcher);
    data.activate_uid(candidate_uid, 1'b0, 1'b0);
    uvm_report_cb::delete(null, catcher);
    if (catcher.caught_count != 1 || data.uid_by_active_rob.num() != 0) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("activate fail-fast mismatch caught=%0d active_rob=%0d",
                             catcher.caught_count, data.uid_by_active_rob.num()))
    end

    // 提交 frontier 的第一项后，原 distance=160 candidate 变成 distance=159，应立即放行。
    data.get_status(0).rob_commit = 1'b1;
    data.publish_rob_commit_frontier(1, "rob_boundary_frontier_advance");
    if (!data.rob_admission_window_has_room(candidate_uid,
                                             "rob_boundary_after_frontier_advance")) begin
        `uvm_fatal(get_type_name(), "candidate did not open after frontier advanced")
    end
endtask:run_boundary_checks

function memblock_later_split_tlb_record
soft_test_memblock_rob_admission_window_sequence::build_later_split_pending_owner(
    input memblock_uid_t uid,
    input longint unsigned token,
    input longint unsigned sample_seq
);
    bit [63:0] split_va;
    memblock_rob_key_t rob_key;
    memblock_tlb_lookup_key_t lookup_key;
    memblock_later_split_tlb_record owner;

    split_va = (data.main_table_by_uid[uid].vaddr & 64'hfffffffffffff000) + 64'h1000;
    rob_key = data.main_table_by_uid[uid].get_rob_key();
    // 通过实际 observer 和 token 绑定入口建立在途记录，保留与 pending response 相同的对象句柄。
    data.observe_later_split_tlb_sample(
        1'b1, rob_key, split_va, 1'b1, 1'b1, 1'b0,
        rob_key, split_va, 1'b1, split_va[49:12], 2'b00, sample_seq);
    lookup_key = '{default:'0};
    lookup_key.vpn = split_va[63:12];
    owner = data.bind_later_split_l2_request_token(lookup_key, token, sample_seq + 1);
    if (owner == null || owner.uid != uid || owner.response_token != token ||
        owner.state != memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_WAIT_RESPONSE) begin
        `uvm_fatal(get_type_name(), "failed to build exact pending later-split owner")
    end
    return owner;
endfunction:build_later_split_pending_owner

task soft_test_memblock_rob_admission_window_sequence::run_redirect_later_split_cancel_checks();
    memblock_later_split_tlb_record old_owner;
    memblock_later_split_tlb_record new_owner;
    memblock_later_split_tlb_record older_owner;
    memblock_tlb_entry response;
    memblock_redirect_payload_t redirect;
    int unsigned old_epoch;

    // 普通 reissue 与未提交 fault 两条路径，分别覆盖迟到 completion 在新 owner 建立前、后到达。
    for (int scenario = 0; scenario < 4; scenario++) begin
        build_random_main_table(4);
        for (memblock_uid_t uid = 0; uid < 3; uid++) begin
            data.main_table_by_uid[uid].op_class = MEMBLOCK_OP_CLASS_INT_LOAD;
            data.main_table_by_uid[uid].numLsElem = 1;
            data.status_by_uid[uid].snapshot_from_main(data.main_table_by_uid[uid]);
            data.activate_uid(uid, 1'b0, 1'b0);
            data.mark_uid_enqueued(uid);
        end
        older_owner = build_later_split_pending_owner(0, 101, 10);
        old_owner = build_later_split_pending_owner(1, 102, 12);
        old_epoch = old_owner.dynamic_epoch;
        // 非当前 epoch 和无记录 UID 不得改变现有记录或创建新记录。
        data.cancel_later_split_tlb_record_for_uid(0, older_owner.dynamic_epoch + 1, "epoch_mismatch_probe");
        data.cancel_later_split_tlb_record_for_uid(3, 0, "missing_record_probe");
        if (older_owner.state != memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_WAIT_RESPONSE ||
            data.later_split_tlb_record_by_uid.num() != 2) begin
            `uvm_fatal(get_type_name(), "nonmatching later-split cancel changed records")
        end
        if (scenario >= 2) begin
            data.status_by_uid[1].fault = 1'b1;
            data.status_by_uid[1].exception_pending = 1'b1;
            data.status_by_uid[1].load_fault = 1'b1;
        end
        redirect = '{default:'0};
        redirect.valid = 1'b1;
        redirect.rob_key = data.main_table_by_uid[0].get_rob_key();
        data.request_redirect_flush(redirect);
        data.cancel_record_q[0].redirect_anchor_valid = 1'b1;
        data.cancel_record_q[0].redirect_sample_seq = 0;
        data.cancel_record_q[0].redirect_lsq_sample_seq = 0;
        data.latest_drained_cancel_sample_seq = 0;
        if (!data.apply_redirect_flush(redirect)) begin
            `uvm_fatal(get_type_name(), "later-split redirect wrapper failed")
        end
        if (data.status_by_uid[1].dynamic_epoch != old_epoch + 1 ||
            older_owner.state != memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_WAIT_RESPONSE ||
            older_owner.response_token != 101 || older_owner.response_sample_seq != 0) begin
            `uvm_fatal(get_type_name(), "redirect changed an unrelated later-split owner or failed to advance epoch")
        end
        new_owner = null;
        if (scenario % 2 == 1) begin
            data.activate_uid(1, 1'b0, 1'b0);
            data.mark_uid_enqueued(1);
            new_owner = build_later_split_pending_owner(1, 202, 30);
        end
        response = memblock_tlb_entry::type_id::create("late_split_response");
        response.lookup_key = old_owner.request_lookup_key;
        response.entry_generation = 7;
        response.s1_entry_pbmt = 1;
        // 旧 completion 必须走原有 canceled-stale 路径，不能写入旧 payload 或新 canonical owner。
        data.complete_later_split_l2_response(old_owner, 102, old_owner.request_lookup_key,
                                              response.lookup_key, 7, response, 40);
        if (old_owner.state != memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_CANCELED ||
            old_owner.dynamic_epoch != old_epoch || old_owner.response_token != 102 ||
            old_owner.response_sample_seq != 0 || old_owner.entry_generation != 0 ||
            old_owner.payload.entry_generation != 0 || old_owner.payload.s1_entry_pbmt != 0) begin
            `uvm_fatal(get_type_name(), "late completion changed canceled owner payload")
        end
        if (new_owner != null &&
            (data.later_split_tlb_record_by_uid[1] != new_owner ||
             new_owner.dynamic_epoch != old_epoch + 1 || new_owner.response_token != 202 ||
             new_owner.state != memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_WAIT_RESPONSE ||
             new_owner.response_sample_seq != 0 || new_owner.payload.entry_generation != 0)) begin
            `uvm_fatal(get_type_name(), "old completion changed the new-epoch owner")
        end
        data.cancel_later_split_tlb_record_for_uid(0, older_owner.dynamic_epoch, "cleanup_probe");
        data.cancel_later_split_tlb_record_for_uid(0, older_owner.dynamic_epoch, "already_canceled_probe");
        if (older_owner.state != memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_CANCELED ||
            older_owner.response_token != 101 || older_owner.response_sample_seq != 0) begin
            `uvm_fatal(get_type_name(), "repeated cancel changed completed identity")
        end
    end
    `uvm_info("REDIRECT_SPLIT_CANCEL", "all four redirect/later-split completion scenarios passed", UVM_LOW)
endtask:run_redirect_later_split_cancel_checks

task soft_test_memblock_rob_admission_window_sequence::run_redirect_caught_fatal_atomicity_checks();
    run_redirect_caught_fatal_atomicity_case(
        1'b0, "redirect_before_scan_atomicity");
    run_redirect_caught_fatal_atomicity_case(
        1'b1, "redirect_late_prefix_atomicity");
endtask:run_redirect_caught_fatal_atomicity_checks

task soft_test_memblock_rob_admission_window_sequence::run_redirect_caught_fatal_atomicity_case(
    input bit inject_late_prefix_failure,
    input string scenario_name
);
    memblock_uid_t redirect_uid;
    memblock_uid_t victim_uid;
    memblock_redirect_payload_t redirect;
    memblock_rob_key_t redirect_rob_key;
    memblock_rob_map_key_t redirect_rob_map_key;
    memblock_rob_map_key_t victim_rob_map_key;
    memblock_lq_map_key_t victim_lq_map_key;
    memblock_sq_map_key_t victim_sq_map_key;
    memblock_dispatch_progress_t progress_before;
    memblock_issue_q_item_t issue_item;
    memblock_ptw_wait_replay_t ptw_item;
    memblock_lsq_cancel_record_t cancel_before;
    status_transaction victim_status;
    memblock_later_split_tlb_record victim_split_owner;
    memblock_control_owner_t owner_before;
    memblock_rob_admission_expected_fatal_catcher catcher;
    int unsigned active_rob_count_before;
    int unsigned lq_count_before;
    int unsigned sq_count_before;
    int unsigned load_q_count_before;
    int unsigned sta_q_count_before;
    int unsigned std_q_count_before;
    int unsigned ptw_q_count_before;
    int unsigned redirect_q_count_before;
    int unsigned cancel_q_count_before;
    int unsigned pending_lq_before;
    int unsigned pending_sq_before;
    int unsigned deleted_lq_count_before;
    int unsigned deleted_sq_count_before;
    int unsigned active_cancel_id_before;
    bit active_cancel_valid_before;
    bit deleted_window_before;
    bit redirect_inflight_before;
    memblock_redirect_payload_t redirect_inflight_payload_before;
    memblock_redirect_phase_e redirect_phase_before;
    bit flush_before;
    bit dispatch_flush_before;
    bit freeze_before;
    memblock_redirect_payload_t active_redirect_before;
    int unsigned victim_dynamic_epoch_before;
    bit victim_active_before;
    bit victim_redirect_pending_before;
    bit victim_flushed_before;
    bit victim_lq_mapped_before;
    bit victim_sq_mapped_before;
    bit victim_queued_load_before;
    int unsigned owner_generation_before;
    memblock_control_state_e control_state_before;
    bit provenance_valid_before;
    memblock_uid_t provenance_uid_before;
    memblock_redirect_payload_t provenance_redirect_before;
    memblock_uid_t provenance_old_end_before;
    memblock_uid_t provenance_new_end_before;
    bit wrapper_result;

    build_random_main_table(4);
    redirect_uid = 0;
    victim_uid = 1;
    data.main_table_by_uid[redirect_uid].op_class = MEMBLOCK_OP_CLASS_INT_LOAD;
    data.main_table_by_uid[redirect_uid].numLsElem = 1;
    data.main_table_by_uid[victim_uid].op_class = MEMBLOCK_OP_CLASS_INT_LOAD;
    data.main_table_by_uid[victim_uid].numLsElem = 1;
    data.main_table_by_uid[redirect_uid].lqIdx_flag = 1'b0;
    data.main_table_by_uid[redirect_uid].lqIdx_value = 0;
    data.main_table_by_uid[redirect_uid].sqIdx_flag = 1'b0;
    data.main_table_by_uid[redirect_uid].sqIdx_value = 0;
    data.main_table_by_uid[victim_uid].lqIdx_flag = 1'b0;
    data.main_table_by_uid[victim_uid].lqIdx_value = 1;
    data.main_table_by_uid[victim_uid].sqIdx_flag = 1'b0;
    data.main_table_by_uid[victim_uid].sqIdx_value = 1;
    data.status_by_uid[redirect_uid].snapshot_from_main(data.main_table_by_uid[redirect_uid]);
    data.status_by_uid[victim_uid].snapshot_from_main(data.main_table_by_uid[victim_uid]);
    data.activate_uid(redirect_uid, 1'b1, 1'b1);
    data.activate_uid(victim_uid, 1'b1, 1'b1);
    data.mark_uid_enqueued(redirect_uid);
    data.mark_uid_enqueued(victim_uid);
    victim_status = data.status_by_uid[victim_uid];
    victim_status.lsq_reservation_state = MEMBLOCK_LSQ_RESERVATION_DUT_VISIBLE;
    victim_status.lsq_reservation_sample_valid = 1'b1;
    victim_status.lsq_reservation_sample_seq = 0;
    victim_status.load_dispatched = 1'b1;
    victim_status.queued_load = 1'b1;
    victim_split_owner = build_later_split_pending_owner(victim_uid, 501, 10);

    issue_item = '{default:'0};
    issue_item.uid = victim_uid;
    issue_item.rob_key = victim_status.get_rob_key();
    issue_item.target = MEMBLOCK_ISSUE_TARGET_LOAD;
    issue_item.dynamic_epoch = victim_status.dynamic_epoch;
    issue_item.replay_seq = victim_status.replay_seq;
    data.push_issue_queue_item(issue_item);
    ptw_item = '{default:'0};
    ptw_item.valid = 1'b1;
    ptw_item.uid = victim_uid;
    ptw_item.target = MEMBLOCK_ISSUE_TARGET_LOAD;
    ptw_item.issue_epoch = victim_status.load_issue_epoch;
    ptw_item.replay_seq = victim_status.replay_seq;
    ptw_item.start_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
    data.ptw_wait_replay_q.push_back(ptw_item);

    redirect = '{default:'0};
    redirect.valid = 1'b1;
    redirect.flush_itself = 1'b0;
    redirect.level = 0;
    redirect.rob_key = data.main_table_by_uid[redirect_uid].get_rob_key();
    data.request_redirect_flush(redirect);
    data.push_redirect_drive(redirect);
    if (data.cancel_record_q.size() != 1) begin
        `uvm_fatal(get_type_name(), {scenario_name, " failed to allocate cancel record"})
    end
    data.cancel_record_q[0].redirect_anchor_valid = 1'b1;
    data.cancel_record_q[0].redirect_sample_seq = 0;
    data.cancel_record_q[0].redirect_lsq_sample_seq = 0;
    data.latest_drained_cancel_sample_seq = 0;

    redirect_rob_key = data.main_table_by_uid[redirect_uid].get_rob_key();
    redirect_rob_map_key = rob_order_util::rob_to_map_key(redirect_rob_key);
    victim_rob_map_key = rob_order_util::rob_to_map_key(victim_status.get_rob_key());
    victim_lq_map_key = rob_order_util::lq_to_map_key(data.main_table_by_uid[victim_uid].get_lq_key());
    victim_sq_map_key = rob_order_util::sq_to_map_key(data.main_table_by_uid[victim_uid].get_sq_key());

    if (inject_late_prefix_failure) begin
        data.control_barrier_prefix_external_provenance_valid = 1'b1;
        data.control_barrier_prefix_external_provenance_uid = 3;
        data.control_barrier_prefix_external_origin_redirect = redirect;
        data.control_barrier_prefix_external_origin_old_end_uid = 2;
        data.control_barrier_prefix_external_origin_new_end_uid = 1;
    end else begin
        data.uid_by_active_rob[victim_rob_map_key] = redirect_uid;
    end

    progress_before = data.dispatch_progress;
    active_rob_count_before = data.uid_by_active_rob.num();
    lq_count_before = data.uid_by_lq.num();
    sq_count_before = data.uid_by_sq.num();
    load_q_count_before = data.load_issue_q.size();
    sta_q_count_before = data.sta_issue_q.size();
    std_q_count_before = data.std_issue_q.size();
    ptw_q_count_before = data.ptw_wait_replay_q.size();
    redirect_q_count_before = data.pending_redirect_drive_q.size();
    cancel_q_count_before = data.cancel_record_q.size();
    cancel_before = data.cancel_record_q[0];
    pending_lq_before = data.pending_lq_cancel_count;
    pending_sq_before = data.pending_sq_cancel_count;
    deleted_window_before = data.redirect_deleted_owner_window_active;
    deleted_lq_count_before = data.redirect_deleted_lq_owner_by_key.num();
    deleted_sq_count_before = data.redirect_deleted_sq_owner_by_key.num();
    active_cancel_valid_before = data.active_cancel_record_id_valid;
    active_cancel_id_before = data.active_cancel_record_id;
    redirect_inflight_before = data.redirect_drive_inflight;
    redirect_inflight_payload_before = data.redirect_drive_inflight_payload;
    redirect_phase_before = data.redirect_phase;
    flush_before = data.flush_in_progress;
    dispatch_flush_before = memblock_sync_pkg::dispatch_flush_in_progress;
    freeze_before = data.issue_freeze_ack;
    active_redirect_before = data.active_redirect;
    owner_before = victim_status.control_owner;
    owner_generation_before = victim_status.control_action_generation;
    control_state_before = victim_status.control_state;
    victim_dynamic_epoch_before = victim_status.dynamic_epoch;
    victim_active_before = victim_status.active;
    victim_redirect_pending_before = victim_status.redirect_pending;
    victim_flushed_before = victim_status.flushed;
    victim_lq_mapped_before = victim_status.active_lq_mapped;
    victim_sq_mapped_before = victim_status.active_sq_mapped;
    victim_queued_load_before = victim_status.queued_load;
    provenance_valid_before = data.control_barrier_prefix_external_provenance_valid;
    provenance_uid_before = data.control_barrier_prefix_external_provenance_uid;
    provenance_redirect_before = data.control_barrier_prefix_external_origin_redirect;
    provenance_old_end_before = data.control_barrier_prefix_external_origin_old_end_uid;
    provenance_new_end_before = data.control_barrier_prefix_external_origin_new_end_uid;

    catcher = new({scenario_name, "_catcher"},
                  inject_late_prefix_failure ? "ROB_ADMISSION_PREFIX" :
                                               "ROB_ADMISSION_WINDOW");
    uvm_report_cb::add(null, catcher);
    wrapper_result = data.apply_redirect_flush(redirect);
    uvm_report_cb::delete(null, catcher);
    if (wrapper_result || catcher.caught_count != 1) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("%s expected wrapper failure/caught=1 got result=%0d caught=%0d",
                             scenario_name, wrapper_result, catcher.caught_count))
    end

    if (data.dispatch_progress != progress_before ||
        data.uid_by_active_rob.num() != active_rob_count_before ||
        data.uid_by_lq.num() != lq_count_before ||
        data.uid_by_sq.num() != sq_count_before ||
        !data.uid_by_active_rob.exists(redirect_rob_map_key) ||
        !data.uid_by_active_rob.exists(victim_rob_map_key) ||
        data.uid_by_active_rob[victim_rob_map_key] !=
            (inject_late_prefix_failure ? victim_uid : redirect_uid) ||
        !data.uid_by_lq.exists(victim_lq_map_key) ||
        data.uid_by_lq[victim_lq_map_key] != victim_uid ||
        !data.uid_by_sq.exists(victim_sq_map_key) ||
        data.uid_by_sq[victim_sq_map_key] != victim_uid ||
        data.load_issue_q.size() != load_q_count_before ||
        data.sta_issue_q.size() != sta_q_count_before ||
        data.std_issue_q.size() != std_q_count_before ||
        data.load_issue_q[0].uid != victim_uid ||
        data.ptw_wait_replay_q.size() != ptw_q_count_before ||
        data.ptw_wait_replay_q[0].uid != victim_uid ||
        data.pending_redirect_drive_q.size() != redirect_q_count_before ||
        !data.redirect_payload_equal(data.pending_redirect_drive_q[0], redirect) ||
        data.redirect_drive_inflight != redirect_inflight_before ||
        !data.redirect_payload_equal(data.redirect_drive_inflight_payload,
                                     redirect_inflight_payload_before) ||
        data.cancel_record_q.size() != cancel_q_count_before ||
        data.cancel_record_q[0] != cancel_before ||
        data.pending_lq_cancel_count != pending_lq_before ||
        data.pending_sq_cancel_count != pending_sq_before ||
        data.redirect_deleted_owner_window_active != deleted_window_before ||
        data.redirect_deleted_lq_owner_by_key.num() != deleted_lq_count_before ||
        data.redirect_deleted_sq_owner_by_key.num() != deleted_sq_count_before ||
        data.active_cancel_record_id_valid != active_cancel_valid_before ||
        data.active_cancel_record_id != active_cancel_id_before ||
        data.redirect_phase != redirect_phase_before ||
        data.flush_in_progress != flush_before ||
        memblock_sync_pkg::dispatch_flush_in_progress != dispatch_flush_before ||
        data.issue_freeze_ack != freeze_before ||
        !data.redirect_payload_equal(data.active_redirect, active_redirect_before) ||
        victim_status.control_owner != owner_before ||
        victim_status.control_action_generation != owner_generation_before ||
        victim_status.control_state != control_state_before ||
        victim_status.dynamic_epoch != victim_dynamic_epoch_before ||
        victim_status.active != victim_active_before ||
        victim_status.redirect_pending != victim_redirect_pending_before ||
        victim_status.flushed != victim_flushed_before ||
        victim_status.active_lq_mapped != victim_lq_mapped_before ||
        victim_status.active_sq_mapped != victim_sq_mapped_before ||
        victim_status.queued_load != victim_queued_load_before ||
        victim_split_owner.state != memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_WAIT_RESPONSE ||
        victim_split_owner.dynamic_epoch != victim_dynamic_epoch_before ||
        victim_split_owner.response_token != 501 ||
        victim_split_owner.response_sample_seq != 0 ||
        data.control_barrier_prefix_external_provenance_valid != provenance_valid_before ||
        data.control_barrier_prefix_external_provenance_uid != provenance_uid_before ||
        !data.redirect_payload_equal(data.control_barrier_prefix_external_origin_redirect,
                                     provenance_redirect_before) ||
        data.control_barrier_prefix_external_origin_old_end_uid != provenance_old_end_before ||
        data.control_barrier_prefix_external_origin_new_end_uid != provenance_new_end_before) begin
        `uvm_fatal(get_type_name(),
                   {scenario_name, " caught-fatal changed snapshotted redirect state"})
    end

    if (!inject_late_prefix_failure) begin
        data.uid_by_active_rob[victim_rob_map_key] = victim_uid;
    end else begin
        data.clear_control_barrier_prefix_external_provenance();
    end
    data.reset_all_tables(MEMBLOCK_ROB_SIZE + 2);
endtask:run_redirect_caught_fatal_atomicity_case

task soft_test_memblock_rob_admission_window_sequence::run_control_lifecycle_duplicate_checks();
    memblock_uid_t uid;
    status_transaction status;
    memblock_control_barrier_service service;
    memblock_csr_control_action_t action;
    memblock_csr_control_action_t popped_action;
    memblock_csr_control_action_t queue_front_before;
    memblock_sync_pkg::dispatch_raw_csr_t runtime_snapshot;
    memblock_rob_admission_expected_fatal_catcher catcher;
    memblock_control_audit_snapshot_t audit_before;
    int unsigned generation_before;
    memblock_control_owner_t owner_before;
    memblock_control_state_e state_before;
    memblock_rob_key_t rob_key;
    memblock_rob_map_key_t rob_map_key;
    int unsigned queue_size_before;
    int unsigned runtime_snapshot_seq;
    int unsigned terminal_frontier_before;
    int unsigned active_rob_count_before;
    int unsigned lq_map_count_before;
    int unsigned sq_map_count_before;
    int unsigned load_q_count_before;
    int unsigned sta_q_count_before;
    int unsigned std_q_count_before;
    int unsigned ptw_q_count_before;
    int unsigned std_tombstone_count_before;
    int unsigned sta_tombstone_count_before;
    bit retry_boost_before;
    int unsigned retry_count_before;
    bit retry_identity_before;
    memblock_rob_key_t retry_rob_before;
    bit retry_epoch_valid_before;
    int unsigned retry_epoch_before;
    bit active_before;
    bit terminal_before;
    bit barrier_valid_before;
    memblock_uid_t barrier_uid_before;

    // 软件定向不启动真实 worker；通过真实生产入口验证 duplicate precheck
    // 必须发生在 queue/status/barrier mutation 之前。
    build_random_main_table(4);
    uid = 1;
    data.main_table_by_uid[uid].op_class = MEMBLOCK_OP_CLASS_CSR_CONTROL;
    status = data.status_by_uid[uid];
    status.snapshot_from_main(data.main_table_by_uid[uid]);
    status.control_kind = MEMBLOCK_CONTROL_KIND_CSR;
    status.control_audit_creation_count = 1;
    status.control_audit_owner_bind_count = 1;
    status.control_action_generation = 0;
    status.control_owner = '{default:'0};
    status.control_state = MEMBLOCK_CONTROL_STATE_WAIT_OLDER_ROB_COMMIT;
    data.active_control_barrier_valid = 1'b1;
    data.active_control_barrier_uid = uid;
    service = memblock_control_barrier_service::type_id::create(
        "duplicate_control_lifecycle_service");
    service.ensure_handles();

    action = '{default:'0};
    action.completion_profile = MEMBLOCK_CONTROL_COMPLETION_RUNTIME_CSR_SNAPSHOT;
    action.l2_flush_phase = MEMBLOCK_L2_FLUSH_PHASE_NONE;
    owner_before = status.control_owner;
    generation_before = status.control_action_generation;

    // Duplicate owner bind uses the production owner-binding entry.
    catcher = new("duplicate_owner_bind_catcher", "CONTROL_LIFECYCLE_AUDIT");
    uvm_report_cb::add(null, catcher);
    service.bind_control_owner(status);
    uvm_report_cb::delete(null, catcher);
    if (catcher.caught_count != 1 || status.control_owner != owner_before ||
        status.control_action_generation != generation_before ||
        status.control_audit_owner_bind_count != 1) begin
        `uvm_fatal(get_type_name(),
                   "duplicate owner-bind changed owner/generation/count")
    end

    // Build the already-bound state required by subsequent production entries.
    status.control_action_generation = 1;
    status.control_owner.valid = 1'b1;
    status.control_owner.uid = uid;
    status.control_owner.dynamic_epoch = status.dynamic_epoch;
    status.control_owner.action_generation = 1;
    status.control_owner.kind = MEMBLOCK_CONTROL_KIND_CSR;
    status.control_state = MEMBLOCK_CONTROL_STATE_CSR_CONFIG_PENDING;
    action.owner = status.control_owner;
    owner_before = status.control_owner;
    generation_before = status.control_action_generation;

    // Duplicate enqueue must not append a second queue item or alter its front.
    status.control_audit_csr_enqueue_count = 1;
    status.control_action_enqueued = 1'b1;
    data.csr_control_action_q.push_back(action);
    queue_size_before = data.csr_control_action_q.size();
    queue_front_before = data.csr_control_action_q[0];
    audit_before = snapshot_control_audit_counts(status);
    state_before = status.control_state;
    catcher = new("duplicate_csr_enqueue_catcher", "CONTROL_LIFECYCLE_AUDIT");
    uvm_report_cb::add(null, catcher);
    data.enqueue_csr_control_action(action);
    uvm_report_cb::delete(null, catcher);
    if (catcher.caught_count != 1 ||
        data.csr_control_action_q.size() != queue_size_before ||
        data.csr_control_action_q[0] != queue_front_before ||
        status.control_state != state_before || !status.control_action_enqueued ||
        snapshot_control_audit_counts(status) != audit_before ||
        status.control_owner != owner_before ||
        status.control_action_generation != generation_before) begin
        `uvm_fatal(get_type_name(),
                   "duplicate CSR enqueue changed queue/status/owner/audit")
    end

    // Duplicate start must peek/precheck before pop, preserving the queue front.
    status.control_audit_csr_start_count = 1;
    audit_before = snapshot_control_audit_counts(status);
    queue_size_before = data.csr_control_action_q.size();
    queue_front_before = data.csr_control_action_q[0];
    catcher = new("duplicate_csr_start_catcher", "CONTROL_LIFECYCLE_AUDIT");
    uvm_report_cb::add(null, catcher);
    if (data.try_pop_csr_control_action(popped_action)) begin
        `uvm_fatal(get_type_name(), "duplicate CSR start unexpectedly popped queue")
    end
    uvm_report_cb::delete(null, catcher);
    if (catcher.caught_count != 1 ||
        data.csr_control_action_q.size() != queue_size_before ||
        data.csr_control_action_q[0] != queue_front_before ||
        popped_action.owner.valid ||
        popped_action.completion_profile != MEMBLOCK_CONTROL_COMPLETION_NONE ||
        popped_action.l2_flush_phase != MEMBLOCK_L2_FLUSH_PHASE_NONE ||
        snapshot_control_audit_counts(status) != audit_before ||
        status.control_owner != owner_before ||
        status.control_action_generation != generation_before) begin
        `uvm_fatal(get_type_name(),
                   "duplicate CSR start changed queue front/owner/audit")
    end

    // Duplicate completion uses the real control service completion entry.
    data.csr_control_action_q.delete();
    status.control_action_enqueued = 1'b0;
    status.control_state = MEMBLOCK_CONTROL_STATE_CONTROL_COMMIT_READY;
    status.control_audit_csr_completion_count = 1;
    runtime_snapshot = memblock_sync_pkg::make_empty_raw_csr();
    runtime_snapshot.valid = 1'b1;
    memblock_sync_pkg::publish_runtime_csr_snapshot(runtime_snapshot, 1'b1);
    if (!memblock_sync_pkg::get_latest_runtime_csr_snapshot(
            runtime_snapshot, runtime_snapshot_seq)) begin
        `uvm_fatal(get_type_name(), "failed to publish duplicate completion snapshot")
    end
    status.control_expected_runtime_csr_valid = 1'b1;
    status.control_expected_runtime_csr = runtime_snapshot;
    status.control_runtime_snapshot_seq_before_drive = runtime_snapshot_seq - 1;
    audit_before = snapshot_control_audit_counts(status);
    state_before = status.control_state;
    catcher = new("duplicate_csr_completion_catcher", "CONTROL_LIFECYCLE_AUDIT");
    uvm_report_cb::add(null, catcher);
    service.complete_csr_runtime_snapshot(status);
    uvm_report_cb::delete(null, catcher);
    if (catcher.caught_count != 1 || status.control_state != state_before ||
        status.control_runtime_csr_snapshot_valid ||
        snapshot_control_audit_counts(status) != audit_before ||
        status.control_owner != owner_before ||
        status.control_action_generation != generation_before) begin
        `uvm_fatal(get_type_name(),
                   "duplicate CSR completion changed status/owner/audit")
    end

    // 正常 terminal_count=0，但 active ROB identity 被破坏时，生产入口必须
    // 在任何 status/frontier/work cleanup 前 fail-fast。该 case 专门覆盖 terminal
    // lifecycle precheck 已通过、retire precheck 失败的原子性边界。
    status.active = 1'b1;
    status.rob_commit = 1'b1;
    status.terminal_done = 1'b0;
    status.success = 1'b0;
    status.control_state = MEMBLOCK_CONTROL_STATE_CONTROL_COMMIT_READY;
    status.control_audit_terminal_count = 0;
    status.uncache_retry_boost = 1'b1;
    status.uncache_retry_count = 3;
    status.uncache_retry_identity_valid = 1'b1;
    status.uncache_retry_rob_key = status.get_rob_key();
    status.uncache_retry_has_dynamic_epoch = 1'b1;
    status.uncache_retry_dynamic_epoch = status.dynamic_epoch;
    data.active_redirect = '{default:'0};
    rob_key = status.get_rob_key();
    rob_map_key = rob_order_util::rob_to_map_key(rob_key);
    data.uid_by_active_rob.delete(rob_map_key);
    audit_before = snapshot_control_audit_counts(status);
    state_before = status.control_state;
    owner_before = status.control_owner;
    generation_before = status.control_action_generation;
    active_before = status.active;
    terminal_before = status.terminal_done;
    terminal_frontier_before = data.dispatch_progress.terminal_done_uid;
    active_rob_count_before = data.uid_by_active_rob.num();
    lq_map_count_before = data.uid_by_lq.num();
    sq_map_count_before = data.uid_by_sq.num();
    load_q_count_before = data.load_issue_q.size();
    sta_q_count_before = data.sta_issue_q.size();
    std_q_count_before = data.std_issue_q.size();
    ptw_q_count_before = data.ptw_wait_replay_q.size();
    std_tombstone_count_before = data.std_late_raw_tombstone_by_rob.num();
    sta_tombstone_count_before = status.sta_late_fault_tombstone_q.size();
    retry_boost_before = status.uncache_retry_boost;
    retry_count_before = status.uncache_retry_count;
    retry_identity_before = status.uncache_retry_identity_valid;
    retry_rob_before = status.uncache_retry_rob_key;
    retry_epoch_valid_before = status.uncache_retry_has_dynamic_epoch;
    retry_epoch_before = status.uncache_retry_dynamic_epoch;
    barrier_valid_before = data.active_control_barrier_valid;
    barrier_uid_before = data.active_control_barrier_uid;
    catcher = new("control_terminal_rob_map_atomicity_catcher", "CONTROL_COMMIT");
    uvm_report_cb::add(null, catcher);
    data.try_retire_control_committed_uid(uid);
    uvm_report_cb::delete(null, catcher);
    if (catcher.caught_count != 1 ||
        status.control_state != state_before || status.active != active_before ||
        status.terminal_done != terminal_before || status.success ||
        data.dispatch_progress.terminal_done_uid != terminal_frontier_before ||
        data.uid_by_active_rob.num() != active_rob_count_before ||
        data.uid_by_active_rob.exists(rob_map_key) ||
        data.uid_by_lq.num() != lq_map_count_before ||
        data.uid_by_sq.num() != sq_map_count_before ||
        data.load_issue_q.size() != load_q_count_before ||
        data.sta_issue_q.size() != sta_q_count_before ||
        data.std_issue_q.size() != std_q_count_before ||
        data.ptw_wait_replay_q.size() != ptw_q_count_before ||
        data.std_late_raw_tombstone_by_rob.num() != std_tombstone_count_before ||
        status.sta_late_fault_tombstone_q.size() != sta_tombstone_count_before ||
        status.uncache_retry_boost != retry_boost_before ||
        status.uncache_retry_count != retry_count_before ||
        status.uncache_retry_identity_valid != retry_identity_before ||
        status.uncache_retry_rob_key != retry_rob_before ||
        status.uncache_retry_has_dynamic_epoch != retry_epoch_valid_before ||
        status.uncache_retry_dynamic_epoch != retry_epoch_before ||
        data.active_control_barrier_valid != barrier_valid_before ||
        data.active_control_barrier_uid != barrier_uid_before ||
        snapshot_control_audit_counts(status) != audit_before ||
        status.control_owner != owner_before ||
        status.control_action_generation != generation_before) begin
        `uvm_fatal(get_type_name(),
                   "control terminal ROB-map precheck changed snapshotted state")
    end

    // Restore the valid active owner for the independent duplicate-terminal case.
    data.uid_by_active_rob[rob_map_key] = uid;
    status.uncache_retry_boost = 1'b0;
    status.uncache_retry_count = 0;
    status.uncache_retry_identity_valid = 1'b0;
    status.uncache_retry_rob_key = '{default:'0};
    status.uncache_retry_has_dynamic_epoch = 1'b0;
    status.uncache_retry_dynamic_epoch = 0;

    // Duplicate terminal must fail before success/terminal/active-map mutation.
    status.active = 1'b1;
    status.rob_commit = 1'b1;
    status.terminal_done = 1'b0;
    status.success = 1'b0;
    status.control_state = MEMBLOCK_CONTROL_STATE_CONTROL_COMMIT_READY;
    status.control_audit_terminal_count = 1;
    data.active_redirect = '{default:'0};
    audit_before = snapshot_control_audit_counts(status);
    state_before = status.control_state;
    active_before = status.active;
    terminal_before = status.terminal_done;
    barrier_valid_before = data.active_control_barrier_valid;
    barrier_uid_before = data.active_control_barrier_uid;
    catcher = new("duplicate_control_terminal_catcher", "CONTROL_LIFECYCLE_AUDIT");
    uvm_report_cb::add(null, catcher);
    data.try_retire_control_committed_uid(uid);
    uvm_report_cb::delete(null, catcher);
    if (catcher.caught_count != 1 || status.control_state != state_before ||
        status.active != active_before || status.terminal_done != terminal_before ||
        status.success || data.active_control_barrier_valid != barrier_valid_before ||
        data.active_control_barrier_uid != barrier_uid_before ||
        snapshot_control_audit_counts(status) != audit_before ||
        status.control_owner != owner_before ||
        status.control_action_generation != generation_before) begin
        `uvm_fatal(get_type_name(),
                   "duplicate terminal changed active/terminal/barrier/audit")
    end

    // Duplicate release must leave the active barrier and owner intact.
    status.active = 1'b0;
    status.terminal_done = 1'b1;
    status.success = 1'b1;
    status.control_audit_barrier_release_count = 1;
    audit_before = snapshot_control_audit_counts(status);
    owner_before = status.control_owner;
    barrier_valid_before = data.active_control_barrier_valid;
    barrier_uid_before = data.active_control_barrier_uid;
    catcher = new("duplicate_barrier_release_catcher", "CONTROL_LIFECYCLE_AUDIT");
    uvm_report_cb::add(null, catcher);
    data.release_control_barrier_after_terminal(uid);
    uvm_report_cb::delete(null, catcher);
    if (catcher.caught_count != 1 ||
        data.active_control_barrier_valid != barrier_valid_before ||
        data.active_control_barrier_uid != barrier_uid_before ||
        status.control_owner != owner_before ||
        snapshot_control_audit_counts(status) != audit_before ||
        status.control_action_generation != generation_before) begin
        `uvm_fatal(get_type_name(),
                   "duplicate barrier release changed barrier/owner/audit")
    end

    // 清除人工状态，避免污染 directed 收尾。
    data.uid_by_active_rob.delete(rob_map_key);
    data.csr_control_action_q.delete();
    data.active_control_barrier_valid = 1'b0;
    data.active_control_barrier_uid = 0;
    status.reset(uid);
    status.snapshot_from_main(data.main_table_by_uid[uid]);

    run_sfence_lifecycle_duplicate_checks(service);
    run_check_store_lifecycle_duplicate_checks(service);
endtask:run_control_lifecycle_duplicate_checks

task soft_test_memblock_rob_admission_window_sequence::run_sfence_lifecycle_duplicate_checks(
    input memblock_control_barrier_service service
);
    memblock_uid_t uid;
    status_transaction status;
    memblock_sfence_control_action_t action0;
    memblock_sfence_control_action_t action1;
    memblock_sfence_control_action_t popped_action;
    memblock_control_atomic_snapshot snapshot;
    memblock_rob_admission_expected_fatal_catcher catcher;
    bit saved_lifecycle_initialized;
    memblock_sync_pkg::memblock_l2tlb_responder_mode_e saved_responder_mode;
    memblock_sync_pkg::memblock_l2tlb_dispatch_topology_e saved_dispatch_topology;
    memblock_sync_pkg::memblock_l2tlb_start_mode_e saved_start_mode;
    bit saved_needs_response;
    string saved_topology_name;
    bit saved_dispatch_lookup_active;
    bit saved_adapter_active;
    string saved_adapter_owner;
    bit saved_owner_claimed;
    bit saved_owner_claimed_once;
    string saved_owner_name;
    longint unsigned saved_reset_epoch;
    bit saved_reset_active;
    bit saved_baseline_valid;
    longint unsigned saved_baseline_epoch;
    longint unsigned saved_baseline_sample;

    saved_lifecycle_initialized = memblock_sync_pkg::l2tlb_testcase_lifecycle_initialized;
    saved_responder_mode = memblock_sync_pkg::l2tlb_testcase_responder_mode;
    saved_dispatch_topology = memblock_sync_pkg::l2tlb_testcase_dispatch_topology;
    saved_start_mode = memblock_sync_pkg::l2tlb_testcase_start_mode;
    saved_needs_response = memblock_sync_pkg::l2tlb_testcase_needs_response;
    saved_topology_name = memblock_sync_pkg::l2tlb_testcase_topology_name;
    saved_dispatch_lookup_active = memblock_sync_pkg::dispatch_l2tlb_lookup_active;
    saved_adapter_active = memblock_sync_pkg::l2tlb_adapter_service_active;
    saved_adapter_owner = memblock_sync_pkg::l2tlb_adapter_service_owner_name;
    saved_owner_claimed = memblock_sync_pkg::l2tlb_lifecycle_owner_claimed;
    saved_owner_claimed_once = memblock_sync_pkg::l2tlb_owner_claimed_once;
    saved_owner_name = memblock_sync_pkg::l2tlb_lifecycle_owner_name;
    saved_reset_epoch = memblock_sync_pkg::l2tlb_current_reset_epoch;
    saved_reset_active = memblock_sync_pkg::l2tlb_runtime_reset_active;
    saved_baseline_valid = memblock_sync_pkg::l2tlb_post_reset_baseline_done_valid;
    saved_baseline_epoch = memblock_sync_pkg::l2tlb_post_reset_baseline_done_epoch;
    saved_baseline_sample = memblock_sync_pkg::l2tlb_post_reset_baseline_done_sample_seq;

    memblock_sync_pkg::l2tlb_testcase_lifecycle_initialized = 1'b1;
    memblock_sync_pkg::l2tlb_testcase_responder_mode =
        memblock_sync_pkg::MEMBLOCK_L2TLB_RESPONDER_ENABLED;
    memblock_sync_pkg::l2tlb_testcase_dispatch_topology =
        memblock_sync_pkg::MEMBLOCK_L2TLB_TOPOLOGY_DISPATCH_ACTIVE;
    memblock_sync_pkg::l2tlb_testcase_start_mode =
        memblock_sync_pkg::MEMBLOCK_L2TLB_START_DEFAULT;
    memblock_sync_pkg::l2tlb_testcase_needs_response = 1'b1;
    memblock_sync_pkg::l2tlb_testcase_topology_name = "directed_duplicate_sfence";
    memblock_sync_pkg::dispatch_l2tlb_lookup_active = 1'b1;
    memblock_sync_pkg::l2tlb_adapter_service_active = 1'b1;
    memblock_sync_pkg::l2tlb_adapter_service_owner_name = "directed_duplicate_adapter";
    memblock_sync_pkg::l2tlb_lifecycle_owner_claimed = 1'b1;
    memblock_sync_pkg::l2tlb_owner_claimed_once = 1'b1;
    memblock_sync_pkg::l2tlb_lifecycle_owner_name = "directed_duplicate_owner";
    memblock_sync_pkg::l2tlb_runtime_reset_active = 1'b0;
    memblock_sync_pkg::l2tlb_current_reset_epoch = 9;
    memblock_sync_pkg::l2tlb_post_reset_baseline_done_valid = 1'b1;
    memblock_sync_pkg::l2tlb_post_reset_baseline_done_epoch = 9;
    memblock_sync_pkg::l2tlb_post_reset_baseline_done_sample_seq = 91;

    build_random_main_table(4);
    uid = 1;
    data.main_table_by_uid[uid].op_class = MEMBLOCK_OP_CLASS_SFENCE_CONTROL;
    status = data.status_by_uid[uid];
    status.snapshot_from_main(data.main_table_by_uid[uid]);
    status.control_kind = MEMBLOCK_CONTROL_KIND_SFENCE;
    status.control_audit_creation_count = 1;
    status.control_audit_owner_bind_count = 1;
    status.control_action_generation = 1;
    status.control_owner = '{valid:1'b1, uid:uid,
                             dynamic_epoch:status.dynamic_epoch,
                             action_generation:1,
                             kind:MEMBLOCK_CONTROL_KIND_SFENCE};
    status.control_state = MEMBLOCK_CONTROL_STATE_SFENCE_REQ;
    data.active_control_barrier_valid = 1'b1;
    data.active_control_barrier_uid = uid;
    action0 = '{default:'0};
    action0.owner = status.control_owner;
    action0.expected_fence.valid = 1'b1;
    action0.expected_fence.target_stage = MEMBLOCK_SFENCE_TARGET_HS_S1;
    action0.expected_fence.addr = 50'h111;
    action1 = action0;
    action1.expected_fence.addr = 50'h222;

    // SFence enqueue: call the real service chain.  Two sentinels prove the
    // duplicate precheck does not append, replace, or reorder the queue.
    status.control_audit_sfence_enqueue_count = 1;
    status.control_action_enqueued = 1'b0;
    data.sfence_control_action_q.push_back(action0);
    data.sfence_control_action_q.push_back(action1);
    snapshot = new();
    snapshot.capture(data, status);
    catcher = new("duplicate_sfence_enqueue_catcher", "CONTROL_LIFECYCLE_AUDIT");
    uvm_report_cb::add(null, catcher);
    service.enqueue_sfence_action(status);
    uvm_report_cb::delete(null, catcher);
    if (catcher.caught_count != 1 || !snapshot.state_matches(data, status)) begin
        `uvm_fatal(get_type_name(), "duplicate SFence enqueue changed full control state")
    end

    // SFence start: the real queue consumer must peek/precheck before pop.
    status.control_action_enqueued = 1'b1;
    status.control_audit_sfence_start_count = 1;
    snapshot.capture(data, status);
    catcher = new("duplicate_sfence_start_catcher", "CONTROL_LIFECYCLE_AUDIT");
    uvm_report_cb::add(null, catcher);
    if (data.try_pop_sfence_control_action(popped_action)) begin
        `uvm_fatal(get_type_name(), "duplicate SFence start unexpectedly popped queue")
    end
    uvm_report_cb::delete(null, catcher);
    if (catcher.caught_count != 1 || !snapshot.state_matches(data, status)) begin
        `uvm_fatal(get_type_name(), "duplicate SFence start changed full control state")
    end

    // SFence completion: service must not consume the effective observation.
    data.sfence_control_action_q.delete();
    status.control_action_enqueued = 1'b0;
    status.control_state = MEMBLOCK_CONTROL_STATE_WAIT_L2TLB_FLUSH_EFFECTIVE;
    status.control_audit_sfence_completion_count = 1;
    status.control_sfence_c0_event_seq = 17;
    status.control_l2tlb_reset_epoch_at_arm = 9;
    data.control_sfence_c0_observation = '{
        valid:1'b1, owner:status.control_owner, payload:action0.expected_fence,
        lifecycle_event_seq:17, reset_epoch:9,
        anchor_sample_seq:101, due_sample_seq:0
    };
    data.control_sfence_effective_observation = data.control_sfence_c0_observation;
    data.control_sfence_effective_observation.due_sample_seq = 105;
    snapshot.capture(data, status);
    catcher = new("duplicate_sfence_completion_catcher", "CONTROL_LIFECYCLE_AUDIT");
    uvm_report_cb::add(null, catcher);
    service.service_sfence_control(status);
    uvm_report_cb::delete(null, catcher);
    if (catcher.caught_count != 1 || !snapshot.state_matches(data, status)) begin
        `uvm_fatal(get_type_name(), "duplicate SFence completion changed full control state")
    end

    data.reset_control_action_runtime();
    data.active_control_barrier_valid = 1'b0;
    data.active_control_barrier_uid = 0;
    status.reset(uid);
    status.snapshot_from_main(data.main_table_by_uid[uid]);

    memblock_sync_pkg::l2tlb_testcase_lifecycle_initialized = saved_lifecycle_initialized;
    memblock_sync_pkg::l2tlb_testcase_responder_mode = saved_responder_mode;
    memblock_sync_pkg::l2tlb_testcase_dispatch_topology = saved_dispatch_topology;
    memblock_sync_pkg::l2tlb_testcase_start_mode = saved_start_mode;
    memblock_sync_pkg::l2tlb_testcase_needs_response = saved_needs_response;
    memblock_sync_pkg::l2tlb_testcase_topology_name = saved_topology_name;
    memblock_sync_pkg::dispatch_l2tlb_lookup_active = saved_dispatch_lookup_active;
    memblock_sync_pkg::l2tlb_adapter_service_active = saved_adapter_active;
    memblock_sync_pkg::l2tlb_adapter_service_owner_name = saved_adapter_owner;
    memblock_sync_pkg::l2tlb_lifecycle_owner_claimed = saved_owner_claimed;
    memblock_sync_pkg::l2tlb_owner_claimed_once = saved_owner_claimed_once;
    memblock_sync_pkg::l2tlb_lifecycle_owner_name = saved_owner_name;
    memblock_sync_pkg::l2tlb_current_reset_epoch = saved_reset_epoch;
    memblock_sync_pkg::l2tlb_runtime_reset_active = saved_reset_active;
    memblock_sync_pkg::l2tlb_post_reset_baseline_done_valid = saved_baseline_valid;
    memblock_sync_pkg::l2tlb_post_reset_baseline_done_epoch = saved_baseline_epoch;
    memblock_sync_pkg::l2tlb_post_reset_baseline_done_sample_seq = saved_baseline_sample;
endtask:run_sfence_lifecycle_duplicate_checks

task soft_test_memblock_rob_admission_window_sequence::run_check_store_lifecycle_duplicate_checks(
    input memblock_control_barrier_service service
);
    memblock_uid_t uid;
    status_transaction status;
    memblock_csr_control_action_t assert0;
    memblock_csr_control_action_t assert1;
    memblock_csr_control_action_t popped_assert;
    memblock_l2_flush_release_request_t release0;
    memblock_l2_flush_release_request_t release1;
    memblock_l2_flush_release_request_t popped_release;
    memblock_control_atomic_snapshot snapshot;
    memblock_rob_admission_expected_fatal_catcher catcher;
    bit saved_topology_initialized;
    memblock_sync_pkg::memblock_control_worker_topology_mode_e saved_topology_mode;
    bit saved_topology_active;
    string saved_topology_initializer;
    bit saved_reset_request_active;
    int unsigned saved_reset_epoch;
    bit saved_runtime_ready;
    string saved_reset_reason;
    int unsigned saved_driver_ack;
    int unsigned saved_csr_monitor_ack;
    int unsigned saved_dcache_monitor_ack;
    int unsigned saved_ctrl_monitor_ack;
    memblock_sync_pkg::memblock_control_csr_runtime_baseline_t saved_csr_baseline;
    bit saved_runtime_snapshot_valid;
    memblock_sync_pkg::dispatch_raw_csr_t saved_runtime_snapshot;
    int unsigned saved_runtime_snapshot_seq;
    memblock_sync_pkg::memblock_control_level_observation_t saved_l2_observation;
    memblock_sync_pkg::dispatch_raw_csr_t runtime_snapshot;

    saved_topology_initialized = memblock_sync_pkg::control_worker_topology_initialized;
    saved_topology_mode = memblock_sync_pkg::control_worker_topology_mode;
    saved_topology_active = memblock_sync_pkg::control_worker_topology_active;
    saved_topology_initializer = memblock_sync_pkg::control_worker_topology_initializer;
    saved_reset_request_active = memblock_sync_pkg::control_reset_request_active;
    saved_reset_epoch = memblock_sync_pkg::control_reset_epoch;
    saved_runtime_ready = memblock_sync_pkg::control_runtime_ready;
    saved_reset_reason = memblock_sync_pkg::control_reset_request_reason;
    saved_driver_ack = memblock_sync_pkg::control_csr_driver_reset_ack_epoch;
    saved_csr_monitor_ack = memblock_sync_pkg::control_csr_monitor_reset_ack_epoch;
    saved_dcache_monitor_ack = memblock_sync_pkg::control_dcache_monitor_reset_ack_epoch;
    saved_ctrl_monitor_ack = memblock_sync_pkg::control_ctrl_monitor_reset_ack_epoch;
    saved_csr_baseline = memblock_sync_pkg::control_csr_runtime_baseline;
    saved_runtime_snapshot_valid = memblock_sync_pkg::runtime_csr_snapshot_valid;
    saved_runtime_snapshot = memblock_sync_pkg::runtime_csr_snapshot;
    saved_runtime_snapshot_seq = memblock_sync_pkg::runtime_csr_snapshot_seq;
    saved_l2_observation = memblock_sync_pkg::control_l2_flush_done_observation;

    memblock_sync_pkg::control_worker_topology_initialized = 1'b1;
    memblock_sync_pkg::control_worker_topology_mode =
        memblock_sync_pkg::MEMBLOCK_CONTROL_TOPOLOGY_MANUAL_CONTROL_TABLE;
    memblock_sync_pkg::control_worker_topology_active = 1'b1;
    memblock_sync_pkg::control_worker_topology_initializer = "directed_duplicate_check_store";
    memblock_sync_pkg::control_reset_request_active = 1'b0;
    memblock_sync_pkg::control_reset_epoch = 7;
    memblock_sync_pkg::control_runtime_ready = 1'b1;
    memblock_sync_pkg::control_reset_request_reason = "";
    memblock_sync_pkg::control_csr_driver_reset_ack_epoch = 7;
    memblock_sync_pkg::control_csr_monitor_reset_ack_epoch = 7;
    memblock_sync_pkg::control_dcache_monitor_reset_ack_epoch = 7;
    memblock_sync_pkg::control_ctrl_monitor_reset_ack_epoch = 7;
    runtime_snapshot = memblock_sync_pkg::make_empty_raw_csr();
    runtime_snapshot.valid = 1'b1;
    runtime_snapshot.satp_mode = 4'd8;
    memblock_sync_pkg::runtime_csr_snapshot_valid = 1'b1;
    memblock_sync_pkg::runtime_csr_snapshot = runtime_snapshot;
    memblock_sync_pkg::runtime_csr_snapshot_seq = 31;
    memblock_sync_pkg::control_csr_runtime_baseline =
        '{valid:1'b1, reset_epoch:7, first_snapshot_seq:31};
    memblock_sync_pkg::control_l2_flush_done_observation =
        '{valid:1'b1, level:1'b0, observation_seq:41, sample_seq:401};

    build_random_main_table(4);
    uid = 1;
    data.main_table_by_uid[uid].op_class = MEMBLOCK_OP_CLASS_CHECK_STORE;
    status = data.status_by_uid[uid];
    status.snapshot_from_main(data.main_table_by_uid[uid]);
    status.control_kind = MEMBLOCK_CONTROL_KIND_CHECK_STORE;
    status.control_audit_creation_count = 1;
    status.control_audit_owner_bind_count = 1;
    status.control_action_generation = 1;
    status.control_owner = '{valid:1'b1, uid:uid,
                             dynamic_epoch:status.dynamic_epoch,
                             action_generation:1,
                             kind:MEMBLOCK_CONTROL_KIND_CHECK_STORE};
    status.control_reset_epoch = 7;
    status.control_state = MEMBLOCK_CONTROL_STATE_CHECK_STORE_L2_CSR_ASSERT;
    data.active_control_barrier_valid = 1'b1;
    data.active_control_barrier_uid = uid;
    assert0 = '{default:'0};
    assert0.owner = status.control_owner;
    assert0.completion_profile = MEMBLOCK_CONTROL_COMPLETION_L2_FLUSH_LEVEL;
    assert0.l2_flush_phase = MEMBLOCK_L2_FLUSH_PHASE_ASSERT;
    assert0.csr_baseline_valid = 1'b1;
    assert0.csr_baseline = runtime_snapshot;
    assert0.csr_baseline_snapshot_seq = 31;
    assert0.control_reset_epoch = 7;
    assert1 = assert0;
    assert1.csr_baseline_snapshot_seq = 32;

    // L2 ASSERT enqueue through the real service entry.  The baseline fields
    // must remain unchanged now that lifecycle precheck precedes their commit.
    status.control_audit_l2_assert_enqueue_count = 1;
    data.csr_control_action_q.push_back(assert0);
    data.csr_control_action_q.push_back(assert1);
    snapshot = new();
    snapshot.capture(data, status);
    catcher = new("duplicate_l2_assert_enqueue_catcher", "CONTROL_LIFECYCLE_AUDIT");
    uvm_report_cb::add(null, catcher);
    service.enqueue_check_store_l2_assert(status);
    uvm_report_cb::delete(null, catcher);
    if (catcher.caught_count != 1 || !snapshot.state_matches(data, status)) begin
        `uvm_fatal(get_type_name(), "duplicate L2 ASSERT enqueue changed full control state")
    end

    // L2 ASSERT start through the shared CSR queue consumer.
    status.control_action_enqueued = 1'b1;
    status.control_audit_l2_assert_start_count = 1;
    snapshot.capture(data, status);
    catcher = new("duplicate_l2_assert_start_catcher", "CONTROL_LIFECYCLE_AUDIT");
    uvm_report_cb::add(null, catcher);
    if (data.try_pop_csr_control_action(popped_assert)) begin
        `uvm_fatal(get_type_name(), "duplicate L2 ASSERT start unexpectedly popped queue")
    end
    uvm_report_cb::delete(null, catcher);
    if (catcher.caught_count != 1 || !snapshot.state_matches(data, status)) begin
        `uvm_fatal(get_type_name(), "duplicate L2 ASSERT start changed full control state")
    end

    data.csr_control_action_q.delete();
    status.control_action_enqueued = 1'b0;
    status.control_state = MEMBLOCK_CONTROL_STATE_WAIT_L2_FLUSH_DONE;
    status.control_assert_done_baseline_seq = 41;
    memblock_sync_pkg::control_l2_flush_done_observation =
        '{valid:1'b1, level:1'b1, observation_seq:42, sample_seq:402};

    // L2 ASSERT completion duplicate must not record completion or enqueue RELEASE.
    status.control_audit_l2_assert_completion_count = 1;
    snapshot.capture(data, status);
    catcher = new("duplicate_l2_assert_completion_catcher", "CONTROL_LIFECYCLE_AUDIT");
    uvm_report_cb::add(null, catcher);
    service.service_check_store_control(status);
    uvm_report_cb::delete(null, catcher);
    if (catcher.caught_count != 1 || !snapshot.state_matches(data, status)) begin
        `uvm_fatal(get_type_name(), "duplicate L2 ASSERT completion changed full control state")
    end

    // RELEASE enqueue is the second half of the same done-high transaction.
    status.control_audit_l2_assert_completion_count = 0;
    status.control_audit_l2_release_enqueue_count = 1;
    snapshot.capture(data, status);
    catcher = new("duplicate_l2_release_enqueue_catcher", "CONTROL_LIFECYCLE_AUDIT");
    uvm_report_cb::add(null, catcher);
    service.service_check_store_control(status);
    uvm_report_cb::delete(null, catcher);
    if (catcher.caught_count != 1 || !snapshot.state_matches(data, status)) begin
        `uvm_fatal(get_type_name(), "duplicate L2 RELEASE enqueue changed full control state")
    end

    // L2 RELEASE start preserves the complete two-element request order.
    status.control_audit_l2_assert_completion_count = 1;
    status.control_state = MEMBLOCK_CONTROL_STATE_CHECK_STORE_L2_CSR_RELEASE;
    status.control_audit_l2_release_start_count = 1;
    release0 = '{default:'0};
    release0.owner = status.control_owner;
    release0.control_reset_epoch = 7;
    release0.release_baseline_observation_seq = 42;
    release1 = release0;
    release1.release_baseline_observation_seq = 43;
    data.l2_flush_release_request_q.push_back(release0);
    data.l2_flush_release_request_q.push_back(release1);
    snapshot.capture(data, status);
    catcher = new("duplicate_l2_release_start_catcher", "CONTROL_LIFECYCLE_AUDIT");
    uvm_report_cb::add(null, catcher);
    if (data.try_pop_l2_flush_release_request(popped_release)) begin
        `uvm_fatal(get_type_name(), "duplicate L2 RELEASE start unexpectedly popped queue")
    end
    uvm_report_cb::delete(null, catcher);
    if (catcher.caught_count != 1 || !snapshot.state_matches(data, status)) begin
        `uvm_fatal(get_type_name(), "duplicate L2 RELEASE start changed full control state")
    end

    // L2 RELEASE completion duplicate must leave state and monitor fact intact.
    data.l2_flush_release_request_q.delete();
    status.control_state = MEMBLOCK_CONTROL_STATE_WAIT_L2_FLUSH_IDLE;
    status.control_release_done_baseline_seq = 43;
    status.control_audit_l2_release_completion_count = 1;
    memblock_sync_pkg::control_l2_flush_done_observation =
        '{valid:1'b1, level:1'b0, observation_seq:44, sample_seq:404};
    snapshot.capture(data, status);
    catcher = new("duplicate_l2_release_completion_catcher", "CONTROL_LIFECYCLE_AUDIT");
    uvm_report_cb::add(null, catcher);
    service.service_check_store_control(status);
    uvm_report_cb::delete(null, catcher);
    if (catcher.caught_count != 1 || !snapshot.state_matches(data, status)) begin
        `uvm_fatal(get_type_name(), "duplicate L2 RELEASE completion changed full control state")
    end

    data.reset_control_action_runtime();
    data.active_control_barrier_valid = 1'b0;
    data.active_control_barrier_uid = 0;
    status.reset(uid);
    status.snapshot_from_main(data.main_table_by_uid[uid]);

    memblock_sync_pkg::control_worker_topology_initialized = saved_topology_initialized;
    memblock_sync_pkg::control_worker_topology_mode = saved_topology_mode;
    memblock_sync_pkg::control_worker_topology_active = saved_topology_active;
    memblock_sync_pkg::control_worker_topology_initializer = saved_topology_initializer;
    memblock_sync_pkg::control_reset_request_active = saved_reset_request_active;
    memblock_sync_pkg::control_reset_epoch = saved_reset_epoch;
    memblock_sync_pkg::control_runtime_ready = saved_runtime_ready;
    memblock_sync_pkg::control_reset_request_reason = saved_reset_reason;
    memblock_sync_pkg::control_csr_driver_reset_ack_epoch = saved_driver_ack;
    memblock_sync_pkg::control_csr_monitor_reset_ack_epoch = saved_csr_monitor_ack;
    memblock_sync_pkg::control_dcache_monitor_reset_ack_epoch = saved_dcache_monitor_ack;
    memblock_sync_pkg::control_ctrl_monitor_reset_ack_epoch = saved_ctrl_monitor_ack;
    memblock_sync_pkg::control_csr_runtime_baseline = saved_csr_baseline;
    memblock_sync_pkg::runtime_csr_snapshot_valid = saved_runtime_snapshot_valid;
    memblock_sync_pkg::runtime_csr_snapshot = saved_runtime_snapshot;
    memblock_sync_pkg::runtime_csr_snapshot_seq = saved_runtime_snapshot_seq;
    memblock_sync_pkg::control_l2_flush_done_observation = saved_l2_observation;
endtask:run_check_store_lifecycle_duplicate_checks

`endif
