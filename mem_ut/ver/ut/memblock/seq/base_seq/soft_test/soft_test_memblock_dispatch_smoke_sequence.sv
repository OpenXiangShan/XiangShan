//=========================================================
//File name    : soft_test_memblock_dispatch_smoke_sequence.sv
//Author       : OpenAI_Codex
//Module name  : soft_test_memblock_dispatch_smoke_sequence
//Discribution : software-only end-to-end dispatch smoke sequence
//Date         : 2026-05-19
//=========================================================
`ifndef SOFT_TEST_MEMBLOCK_DISPATCH_SMOKE_SEQUENCE__SV
`define SOFT_TEST_MEMBLOCK_DISPATCH_SMOKE_SEQUENCE__SV

class soft_test_memblock_dispatch_smoke_sequence extends memblock_dispatch_base_sequence;

    int unsigned       dispatch_smoke_trans_num;
    lsq_commit_handler commit_handler;

    `uvm_object_utils(soft_test_memblock_dispatch_smoke_sequence)

    extern function new(string name = "soft_test_memblock_dispatch_smoke_sequence");
    extern virtual task body();
    extern virtual task build_directed_main_table();
    extern virtual task admit_lsq_and_route_issue();
    extern virtual task fire_all_issue_items(output memblock_issue_q_item_t fired_items[$]);
    extern virtual task fire_selected_items(input memblock_issue_q_item_t items[$],
                                            ref memblock_issue_q_item_t fired_items[$],
                                            ref bit had_fire);
    extern virtual task inject_writeback_events(input memblock_issue_q_item_t fired_items[$]);
    extern virtual task commit_and_deq_lsq();
    extern virtual task check_final_status();
    extern virtual task prepare_issue_route_for_uid(input memblock_uid_t uid);
    extern virtual task select_issue_candidates(output memblock_issue_q_item_t load_items[$],
                                                output memblock_issue_q_item_t sta_items[$],
                                                output memblock_issue_q_item_t std_items[$]);
    extern virtual task mark_issue_item_fire(input memblock_issue_q_item_t item,
                                             output bit fired);
    extern virtual task submit_writeback_event(input memblock_wb_event_t wb_event);
    extern virtual function main_control_transaction make_directed_transaction(input string tr_name,
                                                                              input memblock_op_class_e op_class,
                                                                              input int unsigned rob_value,
                                                                              input bit [63:0] base_addr);
    extern virtual function bit all_required_targets_dispatched();
    extern virtual function memblock_wb_event_t make_pass_wb_event(input memblock_issue_q_item_t item);

endclass:soft_test_memblock_dispatch_smoke_sequence

function soft_test_memblock_dispatch_smoke_sequence::new(string name = "soft_test_memblock_dispatch_smoke_sequence");
    super.new(name);
    dispatch_smoke_trans_num = 3;
endfunction:new

task soft_test_memblock_dispatch_smoke_sequence::body();
    memblock_issue_q_item_t fired_items[$];

    build_directed_main_table();
    admit_lsq_and_route_issue();
    fire_all_issue_items(fired_items);
    inject_writeback_events(fired_items);
    commit_and_deq_lsq();
    check_final_status();
    `uvm_info(get_type_name(), "dispatch software smoke sequence completed", UVM_LOW)
endtask:body

task soft_test_memblock_dispatch_smoke_sequence::build_directed_main_table();
    clear_manual_main_table();
    set_manual_main_transaction(0, make_directed_transaction("dispatch_smoke_load",
                                                             MEMBLOCK_OP_CLASS_INT_LOAD,
                                                             0,
                                                             64'h0000_0000_8000_1000));
    set_manual_main_transaction(1, make_directed_transaction("dispatch_smoke_store",
                                                             MEMBLOCK_OP_CLASS_STORE,
                                                             1,
                                                             64'h0000_0000_8000_2000));
    set_manual_main_transaction(2, make_directed_transaction("dispatch_smoke_amo",
                                                             MEMBLOCK_OP_CLASS_AMO,
                                                             2,
                                                             64'h0000_0000_8000_3000));
    import_manual_main_table();
endtask:build_directed_main_table

task soft_test_memblock_dispatch_smoke_sequence::admit_lsq_and_route_issue();
    for (int unsigned uid = 0; uid < data.main_trans_num; uid++) begin
        main_control_transaction main_tr;
        memblock_op_behavior_t   behavior;

        main_tr  = data.get_main_transaction(uid);
        behavior = derive_op_behavior(main_tr);
        if (behavior.need_alloc == 2'b00) begin
            lsq_ctrl.commit_non_lsq_admission(uid, behavior, main_tr);
        end else begin
            lsq_ctrl.commit_allocate(uid, behavior, main_tr);
        end
        prepare_issue_route_for_uid(uid);
    end
    route_all_issue_queues();
endtask:admit_lsq_and_route_issue

task soft_test_memblock_dispatch_smoke_sequence::fire_all_issue_items(output memblock_issue_q_item_t fired_items[$]);
    fired_items.delete();
    for (int unsigned cycle_idx = 0; cycle_idx < 8; cycle_idx++) begin
        memblock_issue_q_item_t load_items[$];
        memblock_issue_q_item_t sta_items[$];
        memblock_issue_q_item_t std_items[$];
        bit                    had_fire;

        route_all_issue_queues();
        select_issue_candidates(load_items, sta_items, std_items);
        had_fire = 1'b0;
        fire_selected_items(load_items, fired_items, had_fire);
        fire_selected_items(sta_items, fired_items, had_fire);
        fire_selected_items(std_items, fired_items, had_fire);

        if (all_required_targets_dispatched()) begin
            return;
        end
        if (!had_fire) begin
            `uvm_fatal(get_type_name(), "dispatch smoke issue loop made no progress")
        end
    end
    `uvm_fatal(get_type_name(), "dispatch smoke did not fire all required issue targets")
endtask:fire_all_issue_items

task soft_test_memblock_dispatch_smoke_sequence::fire_selected_items(input memblock_issue_q_item_t items[$],
                                                           ref memblock_issue_q_item_t fired_items[$],
                                                           ref bit had_fire);
    foreach (items[idx]) begin
        bit fired;

        mark_issue_item_fire(items[idx], fired);
        if (!fired) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("failed to fire uid=%0d target=%0d", items[idx].uid, items[idx].target))
        end
        fired_items.push_back(items[idx]);
        had_fire = 1'b1;
    end
endtask:fire_selected_items

task soft_test_memblock_dispatch_smoke_sequence::inject_writeback_events(input memblock_issue_q_item_t fired_items[$]);
    if (fired_items.size() == 0) begin
        `uvm_fatal(get_type_name(), "dispatch smoke has no fired issue items for writeback")
    end
    foreach (fired_items[idx]) begin
        submit_writeback_event(make_pass_wb_event(fired_items[idx]));
    end
endtask:inject_writeback_events

task soft_test_memblock_dispatch_smoke_sequence::commit_and_deq_lsq();
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
    if (!has_commit || commit_uids.size() != dispatch_smoke_trans_num) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("expected %0d ROB commits, got has_commit=%0d size=%0d",
                             dispatch_smoke_trans_num,
                             has_commit,
                             commit_uids.size()))
    end
    commit_handler.mark_rob_commit_batch(commit_uids);

    lq_deq_head = lsq_ctrl.lq_deq_ptr;
    sq_deq_head = lsq_ctrl.sq_deq_ptr;
    commit_handler.apply_dut_lq_deq(1, lq_deq_head, 1'b0);
    commit_handler.apply_dut_sq_deq(1, sq_deq_head, 1'b0);
endtask:commit_and_deq_lsq

task soft_test_memblock_dispatch_smoke_sequence::check_final_status();
    if (data.main_trans_num != dispatch_smoke_trans_num) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("unexpected main_trans_num=%0d", data.main_trans_num))
    end
    for (int unsigned uid = 0; uid < data.main_trans_num; uid++) begin
        status_transaction       status;
        main_control_transaction main_tr;
        memblock_op_behavior_t   behavior;

        status   = data.get_status(uid);
        main_tr  = data.get_main_transaction(uid);
        behavior = derive_op_behavior(main_tr);
        if (status.active || !status.enq || !status.issue_ready ||
            !status.writeback || !status.pass || status.fault ||
            !status.rob_commit || !status.lsq_deq ||
            !status.success || !status.terminal_done) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("uid=%0d final status mismatch active=%0d enq=%0d issue_ready=%0d wb=%0d pass=%0d fault=%0d rob=%0d deq=%0d success=%0d terminal_done=%0d",
                                 uid,
                                 status.active,
                                 status.enq,
                                 status.issue_ready,
                                 status.writeback,
                                 status.pass,
                                 status.fault,
                                 status.rob_commit,
                                 status.lsq_deq,
                                 status.success,
                                 status.terminal_done))
        end
        if ((behavior.route_load && (!status.load_dispatched || !status.load_writeback || !status.load_pass)) ||
            (behavior.route_sta  && (!status.sta_dispatched  || !status.sta_writeback  || !status.sta_pass)) ||
            (behavior.route_std  && (!status.std_dispatched  || !status.std_writeback  || !status.std_pass))) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("uid=%0d target status mismatch route_load=%0d route_sta=%0d route_std=%0d",
                                 uid,
                                 behavior.route_load,
                                 behavior.route_sta,
                                 behavior.route_std))
        end
    end
    if (data.uid_by_active_rob.num() != 0 || data.uid_by_lq.num() != 0 || data.uid_by_sq.num() != 0) begin
        `uvm_fatal(get_type_name(), "active ROB/LQ/SQ lookup tables are not empty after smoke")
    end
    if (lsq_ctrl.lq_free_count != MEMBLOCK_LQ_SIZE || lsq_ctrl.sq_free_count != MEMBLOCK_SQ_SIZE) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("LSQ free count mismatch lq=%0d sq=%0d",
                             lsq_ctrl.lq_free_count,
                             lsq_ctrl.sq_free_count))
    end
endtask:check_final_status

task soft_test_memblock_dispatch_smoke_sequence::prepare_issue_route_for_uid(input memblock_uid_t uid);
    if (data == null) begin
        data = common_data_transaction::get();
    end
    if (issue_sched == null) begin
        issue_sched = issue_queue_scheduler::type_id::create("issue_sched");
    end
    collect_csr_runtime_events();
    issue_sched.prepare_issue_route_for_uid(uid);
endtask:prepare_issue_route_for_uid

task soft_test_memblock_dispatch_smoke_sequence::select_issue_candidates(output memblock_issue_q_item_t load_items[$],
                                                                         output memblock_issue_q_item_t sta_items[$],
                                                                         output memblock_issue_q_item_t std_items[$]);
    if (issue_sched == null) begin
        issue_sched = issue_queue_scheduler::type_id::create("issue_sched");
    end
    issue_sched.select_issue_candidates(load_items, sta_items, std_items);
endtask:select_issue_candidates

task soft_test_memblock_dispatch_smoke_sequence::mark_issue_item_fire(input memblock_issue_q_item_t item,
                                                                      output bit fired);
    if (issue_sched == null) begin
        issue_sched = issue_queue_scheduler::type_id::create("issue_sched");
    end
    fired = issue_sched.mark_issue_fire(item);
endtask:mark_issue_item_fire

task soft_test_memblock_dispatch_smoke_sequence::submit_writeback_event(input memblock_wb_event_t wb_event);
    if (writeback_handler == null) begin
        writeback_handler = writeback_status_handler::type_id::create("writeback_handler");
    end
    void'(writeback_handler.handle_event(wb_event));
endtask:submit_writeback_event

function main_control_transaction soft_test_memblock_dispatch_smoke_sequence::make_directed_transaction(input string tr_name,
                                                                                              input memblock_op_class_e op_class,
                                                                                              input int unsigned rob_value,
                                                                                              input bit [63:0] base_addr);
    main_control_transaction tr;

    tr = main_control_transaction::type_id::create(tr_name);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), $sformatf("failed to create %s", tr_name))
    end

    tr.op_class     = op_class;
    tr.robIdx_flag  = 1'b0;
    tr.robIdx_value = rob_value[8:0];
    tr.lqIdx_flag   = 1'b0;
    tr.lqIdx_value  = '0;
    tr.sqIdx_flag   = 1'b0;
    tr.sqIdx_value  = '0;
    tr.src_0        = base_addr;
    tr.imm          = 64'h0;
    tr.tlbAF        = 1'b0;
    tr.tlbPF        = 1'b0;
    tr.tlbGPF       = 1'b0;
    tr.PBMT         = '0;
    tr.pmaAF        = 1'b0;
    tr.corrupt      = 1'b0;
    tr.denied       = 1'b0;
    tr.delay        = 0;
    tr.send_pri     = 0;
    tr.send_pri_std = 0;

    case (op_class)
        MEMBLOCK_OP_CLASS_INT_LOAD: begin
            tr.fuType    = MEMBLOCK_FUTYPE_LDU;
            tr.lsq_flow  = MEMBLOCK_LSQ_FLOW_LOAD;
            tr.fuOpType  = MEMBLOCK_LSUOP_LD;
            tr.numLsElem = 5'd1;
        end
        MEMBLOCK_OP_CLASS_STORE: begin
            tr.fuType    = MEMBLOCK_FUTYPE_STU;
            tr.lsq_flow  = MEMBLOCK_LSQ_FLOW_STORE;
            tr.fuOpType  = MEMBLOCK_LSUOP_SD;
            tr.numLsElem = 5'd1;
        end
        MEMBLOCK_OP_CLASS_AMO: begin
            tr.fuType    = MEMBLOCK_FUTYPE_MOU;
            tr.lsq_flow  = MEMBLOCK_LSQ_FLOW_ATOMIC;
            tr.fuOpType  = MEMBLOCK_LSUOP_AMOADD_D;
            tr.numLsElem = 5'd0;
        end
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("unsupported smoke op_class=%0d", op_class))
        end
    endcase
    tr.update_vaddr();
    return tr;
endfunction:make_directed_transaction

function bit soft_test_memblock_dispatch_smoke_sequence::all_required_targets_dispatched();
    if (data == null || data.main_trans_num == 0) begin
        return 1'b0;
    end
    for (int unsigned uid = 0; uid < data.main_trans_num; uid++) begin
        main_control_transaction main_tr;
        status_transaction       status;
        memblock_op_behavior_t   behavior;

        main_tr  = data.get_main_transaction(uid);
        status   = data.get_status(uid);
        behavior = derive_op_behavior(main_tr);
        if (behavior.route_load && !status.load_dispatched) begin
            return 1'b0;
        end
        if (behavior.route_sta && !status.sta_dispatched) begin
            return 1'b0;
        end
        if (behavior.route_std && !status.std_dispatched) begin
            return 1'b0;
        end
    end
    return 1'b1;
endfunction:all_required_targets_dispatched

function memblock_wb_event_t soft_test_memblock_dispatch_smoke_sequence::make_pass_wb_event(input memblock_issue_q_item_t item);
    memblock_wb_event_t wb_event;
    status_transaction  status;

    wb_event = data.make_empty_wb_event();
    status   = data.get_status(item.uid);

    wb_event.valid               = 1'b1;
    wb_event.target              = item.target;
    wb_event.uid                 = item.uid;
    wb_event.has_uid             = 1'b1;
    wb_event.rob_key             = item.rob_key;
    wb_event.has_rob             = 1'b1;
    wb_event.issue_epoch         = status.get_target_issue_epoch(item.target);
    wb_event.has_issue_epoch     = 1'b1;
    wb_event.replay_seq          = status.replay_seq;
    wb_event.has_replay_seq      = 1'b1;
    wb_event.real_wb_valid       = 1'b1;
    wb_event.cycle               = $time;

    case (item.target)
        MEMBLOCK_ISSUE_TARGET_LOAD: wb_event.source = MEMBLOCK_WB_EVENT_SOURCE_LOAD_WB;
        MEMBLOCK_ISSUE_TARGET_STA:  wb_event.source = MEMBLOCK_WB_EVENT_SOURCE_STORE_WB;
        MEMBLOCK_ISSUE_TARGET_STD:  wb_event.source = MEMBLOCK_WB_EVENT_SOURCE_STORE_WB;
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("unsupported writeback target=%0d", item.target))
        end
    endcase
    return wb_event;
endfunction:make_pass_wb_event

`endif
