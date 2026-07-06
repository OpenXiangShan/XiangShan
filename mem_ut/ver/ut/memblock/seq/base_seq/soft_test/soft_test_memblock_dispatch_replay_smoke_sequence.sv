//=========================================================
//File name    : soft_test_memblock_dispatch_replay_smoke_sequence.sv
//Author       : OpenAI_Codex
//Module name  : soft_test_memblock_dispatch_replay_smoke_sequence
//Discribution : software-only dispatch replay closure smoke sequence
//Date         : 2026-05-19
//=========================================================
`ifndef SOFT_TEST_MEMBLOCK_DISPATCH_REPLAY_SMOKE_SEQUENCE__SV
`define SOFT_TEST_MEMBLOCK_DISPATCH_REPLAY_SMOKE_SEQUENCE__SV

class soft_test_memblock_dispatch_replay_smoke_sequence extends soft_test_memblock_dispatch_smoke_sequence;

    `uvm_object_utils(soft_test_memblock_dispatch_replay_smoke_sequence)

    extern function new(string name = "soft_test_memblock_dispatch_replay_smoke_sequence");
    extern virtual task body();
    extern virtual task inject_writeback_events_except(input memblock_issue_q_item_t fired_items[$],
                                                       input memblock_uid_t skip_uid,
                                                       input memblock_issue_target_e skip_target);
    extern virtual task fire_replay_sta_item(input memblock_uid_t uid,
                                             output memblock_issue_q_item_t replay_item);
    extern virtual function bit find_fired_item(input memblock_issue_q_item_t fired_items[$],
                                                input memblock_uid_t uid,
                                                input memblock_issue_target_e target,
                                                output memblock_issue_q_item_t item);
    extern virtual function memblock_wb_event_t make_replay_wb_event(input memblock_issue_q_item_t item);
    extern virtual function memblock_wb_event_t make_pass_wb_event_with_snapshot(input memblock_issue_q_item_t item,
                                                                                input int unsigned issue_epoch,
                                                                                input int unsigned replay_seq);
    extern virtual function void check_replay_pending_state(input memblock_uid_t uid,
                                                            input int unsigned old_issue_epoch,
                                                            input int unsigned old_replay_seq);
    extern virtual function void check_stale_pass_ignored(input memblock_uid_t uid);
    extern virtual function void check_replay_final_status(input memblock_uid_t uid,
                                                           input int unsigned expected_replay_seq);

endclass:soft_test_memblock_dispatch_replay_smoke_sequence

function soft_test_memblock_dispatch_replay_smoke_sequence::new(string name = "soft_test_memblock_dispatch_replay_smoke_sequence");
    super.new(name);
endfunction:new

task soft_test_memblock_dispatch_replay_smoke_sequence::body();
    memblock_issue_q_item_t fired_items[$];
    memblock_issue_q_item_t first_sta_item;
    memblock_issue_q_item_t replay_sta_item;
    status_transaction      status;
    int unsigned            old_sta_issue_epoch;
    int unsigned            old_replay_seq;

    build_directed_main_table();
    admit_lsq_and_route_issue();
    fire_all_issue_items(fired_items);

    if (!find_fired_item(fired_items, 1, MEMBLOCK_ISSUE_TARGET_STA, first_sta_item)) begin
        `uvm_fatal(get_type_name(), "replay smoke did not fire the directed store STA item")
    end

    status = data.get_status(1);
    old_sta_issue_epoch = status.get_target_issue_epoch(MEMBLOCK_ISSUE_TARGET_STA);
    old_replay_seq      = status.replay_seq;

    inject_writeback_events_except(fired_items, 1, MEMBLOCK_ISSUE_TARGET_STA);
    submit_writeback_event(make_replay_wb_event(first_sta_item));
    exception_redirect_replay_task();
    check_replay_pending_state(1, old_sta_issue_epoch, old_replay_seq);

    fire_replay_sta_item(1, replay_sta_item);

    submit_writeback_event(make_pass_wb_event_with_snapshot(replay_sta_item,
                                                            old_sta_issue_epoch,
                                                            data.get_status(1).replay_seq));
    check_stale_pass_ignored(1);

    submit_writeback_event(make_pass_wb_event_with_snapshot(first_sta_item,
                                                            old_sta_issue_epoch,
                                                            old_replay_seq));
    check_stale_pass_ignored(1);

    submit_writeback_event(make_pass_wb_event(replay_sta_item));
    check_replay_final_status(1, old_replay_seq + 1);

    commit_and_deq_lsq();
    check_final_status();
    data.end_test_check();
    `uvm_info(get_type_name(), "dispatch replay smoke sequence completed", UVM_LOW)
endtask:body

task soft_test_memblock_dispatch_replay_smoke_sequence::inject_writeback_events_except(input memblock_issue_q_item_t fired_items[$],
                                                                             input memblock_uid_t skip_uid,
                                                                             input memblock_issue_target_e skip_target);
    if (fired_items.size() == 0) begin
        `uvm_fatal(get_type_name(), "replay smoke has no fired issue items for writeback")
    end
    foreach (fired_items[idx]) begin
        if (fired_items[idx].uid == skip_uid && fired_items[idx].target == skip_target) begin
            continue;
        end
        submit_writeback_event(make_pass_wb_event(fired_items[idx]));
    end
endtask:inject_writeback_events_except

task soft_test_memblock_dispatch_replay_smoke_sequence::fire_replay_sta_item(input memblock_uid_t uid,
                                                                   output memblock_issue_q_item_t replay_item);
    memblock_issue_q_item_t load_items[$];
    memblock_issue_q_item_t sta_items[$];
    memblock_issue_q_item_t std_items[$];
    bit fired;

    route_all_issue_queues();
    select_issue_candidates(load_items, sta_items, std_items);
    if (load_items.size() != 0 || std_items.size() != 0 || sta_items.size() != 1) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("expected exactly one STA replay candidate, got load=%0d sta=%0d std=%0d",
                             load_items.size(), sta_items.size(), std_items.size()))
    end
    replay_item = sta_items[0];
    if (replay_item.uid != uid ||
        replay_item.target != MEMBLOCK_ISSUE_TARGET_STA ||
        replay_item.replay_seq != data.get_status(uid).replay_seq) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("bad STA replay item uid=%0d target=%0d item_seq=%0d status_seq=%0d",
                             replay_item.uid,
                             replay_item.target,
                             replay_item.replay_seq,
                             data.get_status(uid).replay_seq))
    end
    mark_issue_item_fire(replay_item, fired);
    if (!fired) begin
        `uvm_fatal(get_type_name(), "failed to fire replay STA item")
    end
endtask:fire_replay_sta_item

function bit soft_test_memblock_dispatch_replay_smoke_sequence::find_fired_item(input memblock_issue_q_item_t fired_items[$],
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

function memblock_wb_event_t soft_test_memblock_dispatch_replay_smoke_sequence::make_replay_wb_event(input memblock_issue_q_item_t item);
    memblock_wb_event_t wb_event;
    status_transaction  status;

    wb_event = data.make_empty_wb_event();
    status   = data.get_status(item.uid);

    wb_event.valid           = 1'b1;
    wb_event.source          = MEMBLOCK_WB_EVENT_SOURCE_STA_FEEDBACK;
    wb_event.target          = item.target;
    wb_event.uid             = item.uid;
    wb_event.has_uid         = 1'b1;
    wb_event.rob_key         = item.rob_key;
    wb_event.has_rob         = 1'b1;
    wb_event.sq_key          = item.sq_key;
    wb_event.has_sq          = item.has_sqIdx;
    wb_event.issue_epoch     = status.get_target_issue_epoch(item.target);
    wb_event.has_issue_epoch = 1'b1;
    wb_event.replay_seq      = status.replay_seq;
    wb_event.has_replay_seq  = 1'b1;
    wb_event.iq_feedback_valid  = 1'b1;
    wb_event.iq_feedback_hit    = 1'b0;
    wb_event.iq_feedback_failed = 1'b1;
    wb_event.replay_valid    = 1'b1;
    wb_event.cycle           = $time;
    return wb_event;
endfunction:make_replay_wb_event

function memblock_wb_event_t soft_test_memblock_dispatch_replay_smoke_sequence::make_pass_wb_event_with_snapshot(input memblock_issue_q_item_t item,
                                                                                                      input int unsigned issue_epoch,
                                                                                                      input int unsigned replay_seq);
    memblock_wb_event_t wb_event;

    wb_event = make_pass_wb_event(item);
    wb_event.issue_epoch     = issue_epoch;
    wb_event.has_issue_epoch = 1'b1;
    wb_event.replay_seq      = replay_seq;
    wb_event.has_replay_seq  = 1'b1;
    return wb_event;
endfunction:make_pass_wb_event_with_snapshot

function void soft_test_memblock_dispatch_replay_smoke_sequence::check_replay_pending_state(input memblock_uid_t uid,
                                                                                  input int unsigned old_issue_epoch,
                                                                                  input int unsigned old_replay_seq);
    status_transaction status;

    status = data.get_status(uid);
    if (!status.replay_pending ||
        !status.replay_target_sta || status.replay_target_load ||
        status.replay_target_std ||
        status.sta_dispatched || status.sta_writeback || status.sta_pass ||
        status.writeback || status.pass ||
        status.replay_seq != old_replay_seq + 1 ||
        status.get_target_issue_epoch(MEMBLOCK_ISSUE_TARGET_STA) != old_issue_epoch) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("bad replay pending state uid=%0d pending=%0d target_l/s/a=%0d/%0d/%0d sta_disp/wb/pass=%0d/%0d/%0d wb=%0d pass=%0d seq=%0d old_seq=%0d sta_epoch=%0d old_epoch=%0d",
                             uid,
                             status.replay_pending,
                             status.replay_target_load,
                             status.replay_target_sta,
                             status.replay_target_std,
                             status.sta_dispatched,
                             status.sta_writeback,
                             status.sta_pass,
                             status.writeback,
                             status.pass,
                             status.replay_seq,
                             old_replay_seq,
                             status.get_target_issue_epoch(MEMBLOCK_ISSUE_TARGET_STA),
                             old_issue_epoch))
    end
    `uvm_info(get_type_name(),
              $sformatf("replay pending uid=%0d old_epoch=%0d old_seq=%0d new_seq=%0d",
                        uid, old_issue_epoch, old_replay_seq, status.replay_seq),
              UVM_LOW)
endfunction:check_replay_pending_state

function void soft_test_memblock_dispatch_replay_smoke_sequence::check_stale_pass_ignored(input memblock_uid_t uid);
    status_transaction status;

    status = data.get_status(uid);
    if (status.sta_writeback || status.sta_pass || status.writeback || status.pass) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("stale STA pass was not ignored uid=%0d sta_wb=%0d sta_pass=%0d wb=%0d pass=%0d",
                             uid,
                             status.sta_writeback,
                             status.sta_pass,
                             status.writeback,
                             status.pass))
    end
    `uvm_info(get_type_name(),
              $sformatf("stale STA pass ignored uid=%0d replay_seq=%0d",
                        uid, status.replay_seq),
              UVM_LOW)
endfunction:check_stale_pass_ignored

function void soft_test_memblock_dispatch_replay_smoke_sequence::check_replay_final_status(input memblock_uid_t uid,
                                                                                 input int unsigned expected_replay_seq);
    status_transaction status;

    status = data.get_status(uid);
    if (status.replay_pending ||
        status.replay_target_load || status.replay_target_sta || status.replay_target_std ||
        !status.sta_dispatched || !status.sta_writeback || !status.sta_pass ||
        !status.std_pass || !status.writeback || !status.pass ||
        status.replay_seq != expected_replay_seq) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("bad replay final state uid=%0d pending=%0d target_l/s/a=%0d/%0d/%0d sta_disp/wb/pass=%0d/%0d/%0d std_pass=%0d wb=%0d pass=%0d seq=%0d expected_seq=%0d",
                             uid,
                             status.replay_pending,
                             status.replay_target_load,
                             status.replay_target_sta,
                             status.replay_target_std,
                             status.sta_dispatched,
                             status.sta_writeback,
                             status.sta_pass,
                             status.std_pass,
                             status.writeback,
                             status.pass,
                             status.replay_seq,
                             expected_replay_seq))
    end
    `uvm_info(get_type_name(),
              $sformatf("replay final uid=%0d sta_epoch=%0d replay_seq=%0d",
                        uid,
                        status.get_target_issue_epoch(MEMBLOCK_ISSUE_TARGET_STA),
                        status.replay_seq),
              UVM_LOW)
endfunction:check_replay_final_status

`endif
