//=========================================================
//File name    : soft_test_memblock_dispatch_fault_smoke_sequence.sv
//Author       : OpenAI_Codex
//Module name  : soft_test_memblock_dispatch_fault_smoke_sequence
//Discribution : software-only dispatch fault terminal_done smoke sequence
//Date         : 2026-06-28
//=========================================================
`ifndef SOFT_TEST_MEMBLOCK_DISPATCH_FAULT_SMOKE_SEQUENCE__SV
`define SOFT_TEST_MEMBLOCK_DISPATCH_FAULT_SMOKE_SEQUENCE__SV

class soft_test_memblock_dispatch_fault_smoke_sequence extends soft_test_memblock_dispatch_smoke_sequence;

    `uvm_object_utils(soft_test_memblock_dispatch_fault_smoke_sequence)

    extern function new(string name = "soft_test_memblock_dispatch_fault_smoke_sequence");
    extern virtual task body();
    extern virtual task run_fault_case(input memblock_op_class_e fault_op_class,
                                       input memblock_issue_target_e fault_target,
                                       input bit expected_is_store_exception);
    extern virtual task build_fault_case_main_table(input memblock_op_class_e fault_op_class);
    extern task commit_and_deq_fault_lsq(input memblock_issue_target_e fault_target,
                                         input bit expected_is_store_exception);
    extern virtual task inject_fault_writeback_events(input memblock_issue_q_item_t fired_items[$],
                                                      input memblock_uid_t fault_uid,
                                                      input memblock_issue_target_e fault_target);
    extern virtual task submit_raw_sta_iq_feedback(input memblock_issue_q_item_t item,
                                                    input bit hit);
    extern virtual function bit find_fired_item(input memblock_issue_q_item_t fired_items[$],
                                                input memblock_uid_t uid,
                                                input memblock_issue_target_e target,
                                                output memblock_issue_q_item_t item);
    extern virtual function memblock_wb_event_t make_fault_wb_event(input memblock_issue_q_item_t item,
                                                                    input bit [23:0] exception_vec);
    extern virtual task check_fault_terminal_status(input memblock_uid_t fault_uid,
                                                    input memblock_issue_target_e fault_target);

endclass:soft_test_memblock_dispatch_fault_smoke_sequence

function soft_test_memblock_dispatch_fault_smoke_sequence::new(string name = "soft_test_memblock_dispatch_fault_smoke_sequence");
    super.new(name);
endfunction:new

task soft_test_memblock_dispatch_fault_smoke_sequence::body();
    commit_handler = lsq_commit_handler::get();
    commit_handler.bind_lsq_ctrl(lsq_ctrl);
    run_fault_case(MEMBLOCK_OP_CLASS_INT_LOAD,
                   MEMBLOCK_ISSUE_TARGET_LOAD,
                   1'b0);
    run_fault_case(MEMBLOCK_OP_CLASS_STORE,
                   MEMBLOCK_ISSUE_TARGET_STA,
                   1'b1);
    data.end_test_check();
    `uvm_info(get_type_name(), "dispatch load/store fault smoke sequence completed", UVM_LOW)
endtask:body

task soft_test_memblock_dispatch_fault_smoke_sequence::run_fault_case(
    input memblock_op_class_e fault_op_class,
    input memblock_issue_target_e fault_target,
    input bit expected_is_store_exception
);
    memblock_issue_q_item_t fired_items[$];
    memblock_issue_q_item_t fault_item;
    lsqcommit_agent_agent_xaction initial_tr;
    memblock_uid_t initial_commit_uids[$];
    memblock_uid_t initial_fault_uid;
    bit initial_has_commit;
    bit initial_has_fault_head;

    // 每个 directed case 都重置公共表、LSQ pointer 和 handler 私有 latch；
    // 第一笔 transaction 因而从确定的非 store 初值开始。
    lsq_ctrl.reset();
    commit_handler.reset_lsqcommit_runtime_state();
    build_fault_case_main_table(fault_op_class);
    commit_handler.build_lsqcommit_xaction(initial_tr,
                                           initial_commit_uids,
                                           initial_has_commit,
                                           initial_has_fault_head,
                                           initial_fault_uid);
    if (initial_has_commit || initial_has_fault_head ||
        initial_commit_uids.size() != 0 ||
        initial_tr.io_ooo_to_mem_isStoreException != 1'b0) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("fault case did not start from idle isStoreException=0: op=%0d commit=%0d fault=%0d size=%0d value=%0d",
                             fault_op_class,
                             initial_has_commit,
                             initial_has_fault_head,
                             initial_commit_uids.size(),
                             initial_tr.io_ooo_to_mem_isStoreException))
    end
    admit_lsq_and_route_issue();
    fire_all_issue_items(fired_items);
    if (!find_fired_item(fired_items, 0, fault_target, fault_item)) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("fault smoke did not fire directed target=%0d item", fault_target))
    end
    inject_fault_writeback_events(fired_items, 0, fault_target);
    // Fault writeback has already updated target status and queued its recovery
    // event. Reuse the normal recovery owner so the software-only case drains
    // the queue before its commit/deq closure and end_test_check().
    exception_redirect_replay_task();
    commit_and_deq_fault_lsq(fault_target, expected_is_store_exception);
    check_fault_terminal_status(0, fault_target);
endtask:run_fault_case

task soft_test_memblock_dispatch_fault_smoke_sequence::build_fault_case_main_table(
    input memblock_op_class_e fault_op_class
);
    memblock_op_class_e younger_op_class;

    case (fault_op_class)
        // The younger UID only proves that a normal commit preserves the fault
        // sideband. Keep it a load so this focused fault smoke does not also
        // need to exercise the separate strict STA real-writeback contract.
        MEMBLOCK_OP_CLASS_INT_LOAD,
        MEMBLOCK_OP_CLASS_STORE:    younger_op_class = MEMBLOCK_OP_CLASS_INT_LOAD;
        default: begin
            `uvm_fatal(get_type_name(),
                       $sformatf("unsupported fault smoke op_class=%0d", fault_op_class))
        end
    endcase
    clear_manual_main_table();
    set_manual_main_transaction(0,
        make_directed_transaction("dispatch_fault_head", fault_op_class, 0,
                                  64'h0000_0000_8000_1000));
    set_manual_main_transaction(1,
        make_directed_transaction("dispatch_fault_younger", younger_op_class, 1,
                                  64'h0000_0000_8000_2000));
    import_manual_main_table();
endtask:build_fault_case_main_table

task soft_test_memblock_dispatch_fault_smoke_sequence::inject_fault_writeback_events(input memblock_issue_q_item_t fired_items[$],
                                                                           input memblock_uid_t fault_uid,
                                                                           input memblock_issue_target_e fault_target);
    if (fired_items.size() == 0) begin
        `uvm_fatal(get_type_name(), "fault smoke has no fired issue items for writeback")
    end
    foreach (fired_items[idx]) begin
        // Strict V2 STA real writeback, including a fault writeback, requires
        // an IQ-hit event for the current issue instance before the writeback.
        if (fired_items[idx].target == MEMBLOCK_ISSUE_TARGET_STA) begin
            submit_raw_sta_iq_feedback(fired_items[idx], 1'b1);
        end
        if (fired_items[idx].uid == fault_uid &&
            fired_items[idx].target == fault_target) begin
            submit_writeback_event(make_fault_wb_event(fired_items[idx], 24'h1));
        end else begin
            submit_writeback_event(make_pass_wb_event(fired_items[idx]));
        end
    end
endtask:inject_fault_writeback_events

task soft_test_memblock_dispatch_fault_smoke_sequence::submit_raw_sta_iq_feedback(
    input memblock_issue_q_item_t item,
    input bit hit
);
    memblock_sync_pkg::dispatch_raw_iq_feedback_t raw_iq;

    if (item.target != MEMBLOCK_ISSUE_TARGET_STA || !item.has_sqIdx) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("STA IQ feedback requires an active STA item, uid=%0d target=%0d has_sq=%0d",
                             item.uid, item.target, item.has_sqIdx))
    end
    raw_iq = memblock_sync_pkg::make_empty_raw_iq_feedback();
    raw_iq.valid    = 1'b1;
    raw_iq.port_id  = item.uop_index;
    raw_iq.is_sta   = 1'b1;
    raw_iq.sq_valid = 1'b1;
    raw_iq.sq_flag  = item.sq_key.flag;
    raw_iq.sq_value = item.sq_key.value;
    raw_iq.hit      = hit;
    raw_iq.cycle    = $time;
    memblock_sync_pkg::push_raw_iq_feedback(raw_iq);
    collect_monitor_event_batch();
endtask:submit_raw_sta_iq_feedback

task soft_test_memblock_dispatch_fault_smoke_sequence::commit_and_deq_fault_lsq(
    input memblock_issue_target_e fault_target,
    input bit expected_is_store_exception
);
    lsqcommit_agent_agent_xaction commit_tr;
    lsqcommit_agent_agent_xaction hold_tr;
    lsqcommit_agent_agent_xaction terminal_tr;
    memblock_uid_t                commit_uids[$];
    memblock_lq_key_t             lq_deq_head;
    memblock_sq_key_t             sq_deq_head;
    memblock_op_behavior_t        fault_behavior;
    memblock_op_behavior_t        younger_behavior;
    bit                           has_commit;
    bit                           has_fault_head;
    memblock_uid_t                fault_uid;

    if (commit_handler == null) begin
        commit_handler = lsq_commit_handler::get();
    end
    commit_handler.bind_lsq_ctrl(lsq_ctrl);

    commit_handler.build_lsqcommit_xaction(commit_tr, commit_uids, has_commit,
                                           has_fault_head, fault_uid);
    if (has_commit || !has_fault_head || fault_uid != 0 || commit_uids.size() != 0) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("expected first cycle to carry only fault uid0 token, got commit=%0d fault=%0d fault_uid=%0d size=%0d",
                             has_commit,
                             has_fault_head,
                             fault_uid,
                             commit_uids.size()))
    end
    if (commit_tr.io_ooo_to_mem_isStoreException != expected_is_store_exception) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("fault target=%0d expected isStoreException=%0d got=%0d",
                             fault_target,
                             expected_is_store_exception,
                             commit_tr.io_ooo_to_mem_isStoreException))
    end
    if (commit_tr.io_ooo_to_mem_lsqio_pendingst ||
        commit_tr.io_ooo_to_mem_lsqio_pendingMMIOld ||
        commit_tr.io_ooo_to_mem_lsqio_scommit != '0) begin
        `uvm_fatal(get_type_name(),
                   "fault-head LSQ commit transaction must keep pendingst/pendingMMIOld/scommit clear")
    end
    if (!commit_handler.mark_fault_rob_commit_uid(fault_uid)) begin
        `uvm_fatal(get_type_name(), "fault head transaction was not committed")
    end

    commit_handler.build_lsqcommit_xaction(hold_tr, commit_uids, has_commit,
                                           has_fault_head, fault_uid);
    if (has_commit || has_fault_head || commit_uids.size() != 0 ||
        hold_tr.io_ooo_to_mem_isStoreException != expected_is_store_exception) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("fault waiting idle did not hold isStoreException=%0d: commit=%0d fault=%0d size=%0d value=%0d",
                             expected_is_store_exception,
                             has_commit,
                             has_fault_head,
                             commit_uids.size(),
                             hold_tr.io_ooo_to_mem_isStoreException))
    end

    fault_behavior = derive_op_behavior(data.get_main_transaction(0));
    if (fault_behavior.uses_lq) begin
        lq_deq_head = lsq_ctrl.lq_deq_ptr;
        commit_handler.apply_dut_lq_deq(1, lq_deq_head, 1'b0);
    end
    if (fault_behavior.uses_sq) begin
        sq_deq_head = lsq_ctrl.sq_deq_ptr;
        commit_handler.apply_dut_sq_deq(1, sq_deq_head, 1'b0);
    end

    commit_handler.build_lsqcommit_xaction(commit_tr, commit_uids, has_commit,
                                           has_fault_head, fault_uid);
    if (!has_commit || has_fault_head || commit_uids.size() != 1) begin
        `uvm_fatal(get_type_name(), "expected second commit batch after fault uid terminal_done")
    end
    if (commit_uids[0] != 1 ||
        commit_tr.io_ooo_to_mem_isStoreException != expected_is_store_exception) begin
        `uvm_fatal(get_type_name(), "second commit batch did not preserve the expected uid or isStoreException value")
    end
    commit_handler.mark_rob_commit_batch(commit_uids);
    younger_behavior = derive_op_behavior(data.get_main_transaction(1));
    if (younger_behavior.uses_lq) begin
        lq_deq_head = lsq_ctrl.lq_deq_ptr;
        commit_handler.apply_dut_lq_deq(1, lq_deq_head, 1'b0);
    end
    if (younger_behavior.uses_sq) begin
        sq_deq_head = lsq_ctrl.sq_deq_ptr;
        commit_handler.apply_dut_sq_deq(1, sq_deq_head, 1'b0);
    end

    commit_handler.build_lsqcommit_xaction(terminal_tr, commit_uids, has_commit,
                                           has_fault_head, fault_uid);
    if (has_commit || has_fault_head || commit_uids.size() != 0 ||
        terminal_tr.io_ooo_to_mem_isStoreException != expected_is_store_exception) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("terminal idle did not hold isStoreException=%0d: commit=%0d fault=%0d size=%0d value=%0d",
                             expected_is_store_exception,
                             has_commit,
                             has_fault_head,
                             commit_uids.size(),
                             terminal_tr.io_ooo_to_mem_isStoreException))
    end
endtask:commit_and_deq_fault_lsq

function bit soft_test_memblock_dispatch_fault_smoke_sequence::find_fired_item(input memblock_issue_q_item_t fired_items[$],
                                                                     input memblock_uid_t uid,
                                                                     input memblock_issue_target_e target,
                                                                     output memblock_issue_q_item_t item);
    foreach (fired_items[idx]) begin
        if (fired_items[idx].uid == uid && fired_items[idx].target == target) begin
            item = fired_items[idx];
            return 1'b1;
        end
    end
    item.uid         = 0;
    item.rob_key     = '{default:'0};
    item.target      = MEMBLOCK_ISSUE_TARGET_NONE;
    item.send_pri    = 0;
    item.ready_cycle = 0;
    item.replay_seq  = 0;
    item.has_lqIdx   = 1'b0;
    item.lq_key      = '{default:'0};
    item.has_sqIdx   = 1'b0;
    item.sq_key      = '{default:'0};
    item.numLsElem   = '0;
    item.uop_index   = 0;
    item.uop_count   = 0;
    return 1'b0;
endfunction:find_fired_item

function memblock_wb_event_t soft_test_memblock_dispatch_fault_smoke_sequence::make_fault_wb_event(input memblock_issue_q_item_t item,
                                                                                         input bit [23:0] exception_vec);
    memblock_wb_event_t wb_event;

    wb_event = make_pass_wb_event(item);
    wb_event.has_exception = 1'b1;
    wb_event.exception_vec = exception_vec;
    return wb_event;
endfunction:make_fault_wb_event

task soft_test_memblock_dispatch_fault_smoke_sequence::check_fault_terminal_status(input memblock_uid_t fault_uid,
                                                                         input memblock_issue_target_e fault_target);
    if (!data.transaction_done() || !data.is_global_stop_requested()) begin
        data.request_global_stop_if_done();
    end
    for (int unsigned uid = 0; uid < data.main_trans_num; uid++) begin
        status_transaction       status;
        main_control_transaction main_tr;
        memblock_op_behavior_t   behavior;

        status   = data.get_status(uid);
        main_tr  = data.get_main_transaction(uid);
        behavior = derive_op_behavior(main_tr);
        if (uid == fault_uid) begin
            if (!status.terminal_done || status.success || !status.fault ||
                status.exception_pending || status.active ||
                !status.rob_commit || !status.lsq_deq) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("fault uid=%0d terminal status mismatch terminal=%0d success=%0d fault=%0d exc_pending=%0d active=%0d rob=%0d deq=%0d",
                                     uid,
                                     status.terminal_done,
                                     status.success,
                                     status.fault,
                                     status.exception_pending,
                                     status.active,
                                     status.rob_commit,
                                     status.lsq_deq))
            end
            if (fault_target == MEMBLOCK_ISSUE_TARGET_LOAD && !status.load_fault) begin
                `uvm_fatal(get_type_name(), "expected load_fault on directed fault uid")
            end
            if (fault_target == MEMBLOCK_ISSUE_TARGET_STA && !status.sta_fault) begin
                `uvm_fatal(get_type_name(), "expected sta_fault on directed fault uid")
            end
            if (fault_target == MEMBLOCK_ISSUE_TARGET_STD && !status.std_fault) begin
                `uvm_fatal(get_type_name(), "expected std_fault on directed fault uid")
            end
        end else if (status.active || !status.enq || !status.issue_ready ||
                     !status.writeback || !status.pass || status.fault ||
                     !status.rob_commit || !status.lsq_deq ||
                     !status.success || !status.terminal_done) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("non-fault uid=%0d final status mismatch active=%0d wb=%0d pass=%0d fault=%0d rob=%0d deq=%0d success=%0d terminal=%0d",
                                 uid,
                                 status.active,
                                 status.writeback,
                                 status.pass,
                                 status.fault,
                                 status.rob_commit,
                                 status.lsq_deq,
                                 status.success,
                                 status.terminal_done))
        end
        if ((uid != fault_uid || fault_target != MEMBLOCK_ISSUE_TARGET_LOAD) &&
            behavior.route_load && (!status.load_dispatched || !status.load_writeback || !status.load_pass)) begin
            `uvm_fatal(get_type_name(), $sformatf("uid=%0d load target did not normal pass", uid))
        end
    end
    if (data.dispatch_progress.terminal_done_uid != data.main_trans_num) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("terminal_done_uid did not reach main_trans_num: terminal_done_uid=%0d main_trans_num=%0d",
                             data.dispatch_progress.terminal_done_uid,
                             data.main_trans_num))
    end
endtask:check_fault_terminal_status

`endif
