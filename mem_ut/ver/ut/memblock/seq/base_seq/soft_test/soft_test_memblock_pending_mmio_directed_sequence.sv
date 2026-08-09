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

class soft_test_memblock_pending_mmio_directed_sequence extends
    soft_test_memblock_dispatch_smoke_sequence;

    `uvm_object_utils(soft_test_memblock_pending_mmio_directed_sequence)

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
    extern virtual task run_stale_load_overlap_scenario(input bit one_cycle_late);
    extern virtual task run_new_owner_overlap_fatal_scenario();
    extern virtual task run_fault_head_suppress_scenario();
    extern virtual task run_global_stop_raw_drain_scenario();
    extern virtual task build_load_store_table();
    extern virtual task build_load_table(input int unsigned load_count);
    extern virtual task check_load_head_sideband(input memblock_uid_t expected_uid,
                                                 input string scenario_name);

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
    run_stale_load_overlap_scenario(1'b0);
    run_stale_load_overlap_scenario(1'b1);
    run_new_owner_overlap_fatal_scenario();
    run_fault_head_suppress_scenario();
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
    build_load_table(1);
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
    build_load_table(1);
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
    build_load_table(1);
    admit_lsq_and_route_issue();
    fire_all_issue_items(fired_items);
    owner_key = data.get_status(0).get_rob_key();

    redirect = '{default:'0};
    redirect.valid = 1'b1;
    redirect.flush_itself = 1'b1;
    redirect.level = 1'b1;
    redirect.rob_key = owner_key;
    data.request_redirect_flush(redirect);

    anchor = memblock_sync_pkg::make_empty_raw_redirect_anchor();
    anchor.valid = 1'b1;
    anchor.level = redirect.level;
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
    if (data.get_status(0).mmio_tag_valid) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("stale loadMmio overlap offset=%0d incorrectly tagged old uid",
                             one_cycle_late))
    end
endtask:run_stale_load_overlap_scenario

// 中文注释：overlap 只能命中 redirect epoch 内新建实例时，旧 owner 已无法唯一证明；
// resolver必须 fatal，不能把迟到 loadMmio 静默写到新实例。
task soft_test_memblock_pending_mmio_directed_sequence::run_new_owner_overlap_fatal_scenario();
    main_control_transaction main_tr;
    memblock_op_behavior_t behavior;
    memblock_redirect_payload_t redirect;
    memblock_sync_pkg::dispatch_raw_redirect_anchor_t anchor;
    memblock_sync_pkg::dispatch_raw_ctrl_t raw;
    memblock_rob_key_t owner_key;
    memblock_pending_mmio_expected_fatal_catcher catcher;

    reset_directed_owner_state();
    build_load_table(1);
    main_tr = data.get_main_transaction(0);
    behavior = derive_op_behavior(main_tr);
    owner_key = data.get_status(0).get_rob_key();

    redirect = '{default:'0};
    redirect.valid = 1'b1;
    redirect.flush_itself = 1'b1;
    redirect.level = 1'b1;
    redirect.rob_key = owner_key;
    data.request_redirect_flush(redirect);

    anchor = memblock_sync_pkg::make_empty_raw_redirect_anchor();
    anchor.valid = 1'b1;
    anchor.level = redirect.level;
    anchor.rob_flag = redirect.rob_key.flag;
    anchor.rob_value = redirect.rob_key.value;
    wait_for_dut_sample_watermark(1, "new-owner-overlap-anchor");
    anchor.sample_seq = memblock_sync_pkg::peek_current_dut_global_sample();
    anchor.cycle = $time;
    memblock_sync_pkg::push_raw_redirect_anchor(anchor);
    monitor_adapter.drain_lsq_timing_sidebands();

    // 在 redirect epoch 内建立同 ROB key的新实例，专门验证不能误归属。
    lsq_ctrl.commit_allocate(0, behavior, main_tr);
    data.set_status_field(0, MEMBLOCK_STATUS_LOAD_DISPATCHED, 1'b1);
    raw = memblock_sync_pkg::make_empty_raw_ctrl();
    raw.valid = 1'b1;
    raw.load_mmio_valid[0] = 1'b1;
    raw.load_mmio_rob_value[0] = owner_key.value;
    raw.mmio_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
    raw.mmio_sample_seq = anchor.sample_seq;
    raw.cycle = $time;

    catcher = new();
    uvm_report_cb::add(null, catcher);
    monitor_adapter.apply_raw_ctrl_mmio_tags(raw);
    uvm_report_cb::delete(null, catcher);
    if (catcher.caught_count != 1 || data.get_status(0).mmio_tag_valid) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("new-owner overlap expected one fatal/no tag, got fatal=%0d tag=%0d",
                             catcher.caught_count, data.get_status(0).mmio_tag_valid))
    end
endtask:run_new_owner_overlap_fatal_scenario

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
    build_load_table(1);
    admit_lsq_and_route_issue();
    fire_all_issue_items(fired_items);
    if (fired_items.size() != 1 ||
        fired_items[0].target != MEMBLOCK_ISSUE_TARGET_LOAD) begin
        `uvm_fatal(get_type_name(), "global-stop directed scenario expected one fired load")
    end
    wb_event = make_pass_wb_event(fired_items[0]);
    submit_writeback_event(wb_event);
    commit_handler.build_lsqcommit_xaction(tr, commit_uids, has_commit,
                                           has_fault_head, fault_uid);
    if (!has_commit || has_fault_head || commit_uids.size() != 1 ||
        commit_uids[0] != 0) begin
        `uvm_fatal(get_type_name(), "global-stop directed scenario did not select uid0 commit")
    end
    commit_handler.mark_rob_commit_batch(commit_uids);
    lq_deq_head = lsq_ctrl.lq_deq_ptr;
    commit_handler.apply_dut_lq_deq(1, lq_deq_head, 1'b0);
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
