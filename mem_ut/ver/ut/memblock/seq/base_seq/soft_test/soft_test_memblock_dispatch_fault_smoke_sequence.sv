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
    extern virtual task commit_and_deq_lsq();
    extern virtual task inject_fault_writeback_events(input memblock_issue_q_item_t fired_items[$],
                                                      input memblock_uid_t fault_uid,
                                                      input memblock_issue_target_e fault_target);
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
    memblock_issue_q_item_t fired_items[$];
    memblock_issue_q_item_t fault_item;

    build_directed_main_table();
    admit_lsq_and_route_issue();
    fire_all_issue_items(fired_items);
    if (!find_fired_item(fired_items, 0, MEMBLOCK_ISSUE_TARGET_LOAD, fault_item)) begin
        `uvm_fatal(get_type_name(), "fault smoke did not fire directed LOAD item")
    end
    inject_fault_writeback_events(fired_items, 0, MEMBLOCK_ISSUE_TARGET_LOAD);
    commit_and_deq_lsq();
    check_fault_terminal_status(0, MEMBLOCK_ISSUE_TARGET_LOAD);
    data.end_test_check();
    `uvm_info(get_type_name(), "dispatch fault smoke sequence completed", UVM_LOW)
endtask:body

task soft_test_memblock_dispatch_fault_smoke_sequence::inject_fault_writeback_events(input memblock_issue_q_item_t fired_items[$],
                                                                           input memblock_uid_t fault_uid,
                                                                           input memblock_issue_target_e fault_target);
    if (fired_items.size() == 0) begin
        `uvm_fatal(get_type_name(), "fault smoke has no fired issue items for writeback")
    end
    foreach (fired_items[idx]) begin
        if (fired_items[idx].uid == fault_uid &&
            fired_items[idx].target == fault_target) begin
            submit_writeback_event(make_fault_wb_event(fired_items[idx], 24'h1));
        end else begin
            submit_writeback_event(make_pass_wb_event(fired_items[idx]));
        end
    end
endtask:inject_fault_writeback_events

task soft_test_memblock_dispatch_fault_smoke_sequence::commit_and_deq_lsq();
    lsqcommit_agent_agent_xaction commit_tr;
    memblock_uid_t                commit_uids[$];
    memblock_lq_key_t             lq_deq_head;
    memblock_sq_key_t             sq_deq_head;
    bit                           has_commit;

    if (commit_handler == null) begin
        commit_handler = lsq_commit_handler::type_id::create("commit_handler");
    end
    commit_handler.bind_lsq_ctrl(lsq_ctrl);

    commit_handler.build_lsqcommit_xaction(commit_tr, commit_uids, has_commit);
    if (!has_commit || commit_uids.size() != 1 || commit_uids[0] != 0) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("expected first commit batch to contain only fault uid0, got has_commit=%0d size=%0d first=%0d",
                             has_commit,
                             commit_uids.size(),
                             commit_uids.size() == 0 ? 0 : commit_uids[0]))
    end
    commit_handler.mark_rob_commit_batch(commit_uids);
    lq_deq_head = lsq_ctrl.lq_deq_ptr;
    commit_handler.apply_dut_lq_deq(1, lq_deq_head, 1'b0);

    commit_handler.build_lsqcommit_xaction(commit_tr, commit_uids, has_commit);
    if (!has_commit || commit_uids.size() == 0) begin
        `uvm_fatal(get_type_name(), "expected second commit batch after fault uid terminal_done")
    end
    commit_handler.mark_rob_commit_batch(commit_uids);
    sq_deq_head = lsq_ctrl.sq_deq_ptr;
    commit_handler.apply_dut_sq_deq(1, sq_deq_head, 1'b0);
endtask:commit_and_deq_lsq

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
