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

    function new(string name = "memblock_rob_admission_expected_fatal_catcher");
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
endclass:memblock_rob_admission_expected_fatal_catcher

class soft_test_memblock_rob_admission_window_sequence extends memblock_dispatch_base_sequence;

    `uvm_object_utils(soft_test_memblock_rob_admission_window_sequence)

    extern function new(string name = "soft_test_memblock_rob_admission_window_sequence");
    extern virtual task body();
    extern virtual task run_boundary_checks();
    extern virtual task build_boundary_main_table();

endclass:soft_test_memblock_rob_admission_window_sequence

function soft_test_memblock_rob_admission_window_sequence::new(
    string name = "soft_test_memblock_rob_admission_window_sequence"
);
    super.new(name);
endfunction:new

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

`endif
