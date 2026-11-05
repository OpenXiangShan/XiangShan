//=========================================================
//File name    : soft_test_memblock_lsq_amo_admission_sequence.sv
//Author       : OpenAI_Codex
//Module name  : soft_test_memblock_lsq_amo_admission_sequence
//Discribution : directed software checks for LSQ D/R/Q and AMO issue fields
//Date         : 2026-11-03
//=========================================================
`ifndef SOFT_TEST_MEMBLOCK_LSQ_AMO_ADMISSION_SEQUENCE__SV
`define SOFT_TEST_MEMBLOCK_LSQ_AMO_ADMISSION_SEQUENCE__SV

class soft_test_memblock_lsq_amo_admission_sequence extends memblock_dispatch_base_sequence;

    longint unsigned sample_seq;

    `uvm_object_utils(soft_test_memblock_lsq_amo_admission_sequence)

    function new(string name = "soft_test_memblock_lsq_amo_admission_sequence");
        super.new(name);
        sample_seq = 0;
    endfunction:new

    function void check_expect(input bit condition, input string message);
        if (!condition) begin
            `uvm_fatal("LSQ_AMO_SOFT", message)
        end
    endfunction:check_expect

    function main_control_transaction make_scalar(input string name,
                                                  input memblock_op_class_e op_class,
                                                  input int unsigned rob_value);
        main_control_transaction tr;

        tr = main_control_transaction::type_id::create(name);
        tr.op_class = op_class;
        tr.robIdx_flag = 1'b0;
        tr.robIdx_value = MEMBLOCK_ROB_VALUE_W'(rob_value);
        tr.src_0 = 64'h8000_0000 + (rob_value * 64);
        tr.imm = 0;
        tr.numLsElem = memblock_num_ls_elem_t'(1);
        if (op_class == MEMBLOCK_OP_CLASS_STORE) begin
            tr.fuType = MEMBLOCK_FUTYPE_STU;
            tr.fuOpType = MEMBLOCK_LSUOP_SD;
            tr.lsq_flow = MEMBLOCK_LSQ_FLOW_STORE;
        end else begin
            tr.fuType = MEMBLOCK_FUTYPE_LDU;
            tr.fuOpType = MEMBLOCK_LSUOP_LD;
            tr.lsq_flow = MEMBLOCK_LSQ_FLOW_LOAD;
        end
        tr.update_vaddr();
        return tr;
    endfunction:make_scalar

    function memblock_sync_pkg::dispatch_raw_cancel_snapshot_t next_snapshot();
        memblock_sync_pkg::dispatch_raw_cancel_snapshot_t snapshot;

        sample_seq++;
        snapshot = memblock_sync_pkg::make_empty_raw_cancel_snapshot();
        snapshot.valid = 1'b1;
        snapshot.complete = 1'b1;
        snapshot.backend_done = 1'b1;
        snapshot.control_reset_epoch = 1;
        snapshot.sample_seq = sample_seq;
        snapshot.cycle = sample_seq;
        return snapshot;
    endfunction:next_snapshot

    task check_fixed_lane_prefix_and_partial();
        memblock_lsqenq_dispatch_base_sequence admission_seq;
        memblock_uid_t uids[$];
        int unsigned slots[$];
        main_control_transaction trs[$];
        memblock_op_behavior_t behaviors[$];
        memblock_lq_key_t lq_keys[$];
        memblock_sq_key_t sq_keys[$];
        memblock_sync_pkg::dispatch_raw_cancel_snapshot_t snapshot;
        bit [MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM-1:0] fire_mask;

        clear_manual_main_table();
        set_manual_main_transaction(0, make_scalar("soft_store0", MEMBLOCK_OP_CLASS_STORE, 0));
        set_manual_main_transaction(1, make_scalar("soft_load1", MEMBLOCK_OP_CLASS_INT_LOAD, 1));
        set_manual_main_transaction(2, make_scalar("soft_store2", MEMBLOCK_OP_CLASS_STORE, 2));
        import_manual_main_table();
        lsq_ctrl.reset_rtl_shadow();
        sample_seq = 0;
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);

        admission_seq = memblock_lsqenq_dispatch_base_sequence::type_id::create("admission_seq");
        admission_seq.ensure_helpers();
        check_expect(!admission_seq.collect_lsq_candidates(uids, slots, trs, behaviors,
                                                     lq_keys, sq_keys),
               "first collect must capture D without issuing");
        check_expect(lsq_ctrl.shadow_d_valid[0] && lsq_ctrl.shadow_d_valid[1] &&
               lsq_ctrl.shadow_d_valid[2], "D bundle capture failed");

        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        check_expect(lsq_ctrl.shadow_r_valid[0] && !lsq_ctrl.shadow_q_valid[0],
               "D did not advance to R on its own edge");
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        check_expect(lsq_ctrl.shadow_q_valid[0] && lsq_ctrl.shadow_q_valid[1] &&
               lsq_ctrl.shadow_q_valid[2], "R did not advance to fixed-lane Q");
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        check_expect(!lsq_ctrl.shadow_iq_recompute_hold, "capture recompute hold did not clear");
        check_expect(lsq_ctrl.shadow_conserve_flow_total[0] == 1 &&
               lsq_ctrl.shadow_conserve_flow_total[1] == 2 &&
               lsq_ctrl.shadow_conserve_flow_total[2] == 3,
               "mixed conserveFlowTotal prefix is incorrect");

        // Unit-check strict mixed prefix independently of the common 6/4
        // registered headroom gate: store passes SQ credit, following load is
        // blocked by strict LQ > conserve and younger store cannot bypass it.
        lsq_ctrl.shadow_can_accept = 1'b1;
        lsq_ctrl.shadow_lq_credit = 2;
        lsq_ctrl.shadow_sq_credit = 20;
        check_expect(admission_seq.collect_lsq_candidates(uids, slots, trs, behaviors,
                                                    lq_keys, sq_keys),
               "strict mixed prefix produced no candidate");
        check_expect(uids.size() == 1 && uids[0] == 0 && slots[0] == 0,
               "strict mixed prefix bypassed the blocked load");

        fire_mask = '0;
        fire_mask[0] = 1'b1;
        lsq_ctrl.note_source_offer(fire_mask, 1'b1, 0, 1, sample_seq + 1);
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        check_expect(!lsq_ctrl.shadow_q_valid[0] && lsq_ctrl.shadow_q_valid[1] &&
               lsq_ctrl.shadow_q_valid[2], "partial fire compressed or lost Q lanes");
        check_expect(lsq_ctrl.shadow_iq_recompute_hold,
               "partial fire did not request a fixed-lane prefix recompute");
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        check_expect(lsq_ctrl.shadow_conserve_flow_total[0] == 0 &&
               lsq_ctrl.shadow_conserve_flow_total[1] == 1 &&
               lsq_ctrl.shadow_conserve_flow_total[2] == 2,
               "partial-hole conserveFlowTotal recompute is incorrect");
    endtask:check_fixed_lane_prefix_and_partial

    task check_source_offer_target_sample();
        memblock_sync_pkg::dispatch_raw_cancel_snapshot_t snapshot;
        bit [MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM-1:0] fire_mask;

        lsq_ctrl.reset_rtl_shadow();
        sample_seq = 0;
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        fire_mask = '0;
        lsq_ctrl.note_source_offer(fire_mask, 1'b0, 0, 0, sample_seq + 2);
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        check_expect(lsq_ctrl.shadow_source_offer_valid &&
               lsq_ctrl.shadow_source_target_sample_seq == sample_seq + 1,
               "source offer was consumed before its target sample");
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        check_expect(!lsq_ctrl.shadow_source_offer_valid,
               "source offer was not consumed at its exact target sample");

        // 连续真实 driver 周期必须逐拍冻结并逐拍消费，不能因为上一 offer
        // 刚清除就漏记下一 active 或下一 idle 周期。
        lsq_ctrl.shadow_q_valid[0] = 1'b1;
        lsq_ctrl.shadow_q_uid[0] = 3;
        lsq_ctrl.shadow_q_is_load[0] = 1'b1;
        fire_mask = '0;
        fire_mask[0] = 1'b1;
        lsq_ctrl.note_source_offer(fire_mask, 1'b1, 1, 0, sample_seq + 1);
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        check_expect(!lsq_ctrl.shadow_source_offer_valid && !lsq_ctrl.shadow_q_valid[0],
               "first active offer did not settle on its target sample");

        lsq_ctrl.shadow_q_valid[0] = 1'b1;
        lsq_ctrl.shadow_q_uid[0] = 4;
        lsq_ctrl.shadow_q_is_load[0] = 1'b1;
        lsq_ctrl.note_source_offer(fire_mask, 1'b1, 1, 0, sample_seq + 1);
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        check_expect(!lsq_ctrl.shadow_source_offer_valid && !lsq_ctrl.shadow_q_valid[0],
               "active-to-active source offer was skipped");

        fire_mask = '0;
        lsq_ctrl.note_source_offer(fire_mask, 1'b0, 0, 0, sample_seq + 1);
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        check_expect(!lsq_ctrl.shadow_source_offer_valid,
               "active-to-idle source offer was skipped");

        lsq_ctrl.shadow_q_valid[0] = 1'b1;
        lsq_ctrl.shadow_q_uid[0] = 5;
        lsq_ctrl.shadow_q_is_load[0] = 1'b0;
        lsq_ctrl.shadow_q_is_store[0] = 1'b0;
        lsq_ctrl.shadow_q_is_atomic[0] = 1'b1;
        fire_mask[0] = 1'b1;
        lsq_ctrl.note_source_offer(fire_mask, 1'b0, 0, 0, sample_seq + 1);
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        check_expect(!lsq_ctrl.shadow_source_offer_valid && !lsq_ctrl.shadow_q_valid[0],
               "atomic Q-fire source offer did not consume lane0");
    endtask:check_source_offer_target_sample

    task check_driver_launch_sample_binding();
        memblock_lsqenq_dispatch_base_sequence admission_seq;
        lsqenq_agent_agent_xaction tr;
        memblock_sync_pkg::dispatch_lsq_source_offer_t offer;
        memblock_sync_pkg::dispatch_raw_cancel_snapshot_t snapshot;
        bit [MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM-1:0] fire_mask;
        memblock_lq_key_t lq_before;
        memblock_sq_key_t sq_before;

        memblock_sync_pkg::clear_lsq_source_offers();
        lsq_ctrl.reset_rtl_shadow();
        sample_seq = 0;
        lsq_ctrl.advance_driver_bound_admission_sample(next_snapshot(), 1'b0);
        admission_seq = memblock_lsqenq_dispatch_base_sequence::type_id::create(
            "driver_launch_binding_seq");
        admission_seq.ensure_helpers();
        tr = lsqenq_agent_agent_xaction::type_id::create("driver_launch_binding_tr");
        fire_mask = '0;
        fire_mask[0] = 1'b1;
        admission_seq.configure_source_offer_metadata(tr, fire_mask, 1'b1, 1, 0,
                                                     "soft delayed launch");
        check_expect(!tr.memblock_source_offer_published &&
               memblock_sync_pkg::lsq_source_offer_queue_size() == 0 &&
               !lsq_ctrl.shadow_source_offer_valid,
               "item construction speculated a source offer before driver launch");

        // 用真实 driver 共用的绑定函数模拟 launch=4；item 早在 sample=1
        // 构造也不能使 target 变成 2。FIFO/consumer 则走正式 main service 路径。
        offer = memblock_sync_pkg::make_lsq_source_offer_at_launch(4, 1);
        offer.fire_mask = tr.memblock_source_offer_fire_mask;
        offer.need_alloc_nonzero = tr.memblock_source_offer_need_alloc_nonzero;
        offer.lq_alloc = tr.memblock_source_offer_lq_alloc;
        memblock_sync_pkg::push_lsq_source_offer(offer);
        check_expect(offer.target_sample_seq == 4 && offer.dut_sample_seq == 5,
               "driver launch/source edge and next DUT sample binding are incorrect");
        offer = memblock_sync_pkg::make_lsq_source_offer_at_launch(5, 1);
        offer.fire_mask = fire_mask;
        offer.need_alloc_nonzero = 1'b1;
        offer.sq_alloc = 1;
        memblock_sync_pkg::push_lsq_source_offer(offer);
        // 自动 idle 无 item，但仍必须占据唯一一个连续输出拍。
        offer = memblock_sync_pkg::make_lsq_source_offer_at_launch(6, 1);
        memblock_sync_pkg::push_lsq_source_offer(offer);
        offer = memblock_sync_pkg::make_lsq_source_offer_at_launch(7, 1);
        offer.fire_mask = fire_mask;
        memblock_sync_pkg::push_lsq_source_offer(offer);

        lsq_ctrl.shadow_q_valid[0] = 1'b1;
        lsq_ctrl.shadow_q_is_load[0] = 1'b1;
        lsq_ctrl.shadow_q_is_store[0] = 1'b0;
        lsq_ctrl.shadow_q_is_atomic[0] = 1'b0;
        repeat (2) begin
            snapshot = next_snapshot();
            lsq_ctrl.advance_driver_bound_admission_sample(snapshot, 1'b0);
            check_expect(lsq_ctrl.shadow_q_valid[0] &&
                   lsq_ctrl.shadow_lq_credit == MEMBLOCK_LQ_SIZE &&
                   memblock_sync_pkg::lsq_source_offer_queue_size() == 4,
                   "driver offer changed shadow before the true source edge");
        end
        lsq_ctrl.advance_driver_bound_admission_sample(next_snapshot(), 1'b0);
        check_expect(!lsq_ctrl.shadow_q_valid[0] &&
               lsq_ctrl.shadow_lq_credit == MEMBLOCK_LQ_SIZE - 1 &&
               memblock_sync_pkg::lsq_source_offer_queue_size() == 3,
               "active driver offer was not consumed exactly at its launch/source edge");
        lsq_ctrl.shadow_q_valid[0] = 1'b1;
        lsq_ctrl.shadow_q_is_load[0] = 1'b0;
        lsq_ctrl.shadow_q_is_store[0] = 1'b1;
        lsq_ctrl.advance_driver_bound_admission_sample(next_snapshot(), 1'b0);
        check_expect(!lsq_ctrl.shadow_q_valid[0] &&
               lsq_ctrl.shadow_sq_credit == MEMBLOCK_SQ_SIZE - 1 &&
               memblock_sync_pkg::lsq_source_offer_queue_size() == 2,
               "consecutive active-to-active driver offer was lost");
        lsq_ctrl.shadow_q_valid[0] = 1'b1;
        lsq_ctrl.shadow_q_is_store[0] = 1'b0;
        lsq_ctrl.shadow_q_is_atomic[0] = 1'b1;
        lq_before = lsq_ctrl.shadow_lq_enq_ptr;
        sq_before = lsq_ctrl.shadow_sq_enq_ptr;
        lsq_ctrl.advance_driver_bound_admission_sample(next_snapshot(), 1'b0);
        check_expect(lsq_ctrl.shadow_q_valid[0] &&
               memblock_sync_pkg::lsq_source_offer_queue_size() == 1,
               "active-to-automatic-idle offer was skipped or fired atomic Q");
        lsq_ctrl.advance_driver_bound_admission_sample(next_snapshot(), 1'b0);
        check_expect(!lsq_ctrl.shadow_q_valid[0] &&
               lsq_ctrl.shadow_lq_enq_ptr == lq_before &&
               lsq_ctrl.shadow_sq_enq_ptr == sq_before &&
               memblock_sync_pkg::lsq_source_offer_queue_size() == 0,
               "atomic Q offer was not sampled once with zero LSQ allocation");
    endtask:check_driver_launch_sample_binding

    task check_reset_pending_reservation_cancel();
        memblock_lsqenq_dispatch_base_sequence admission_seq;
        main_control_transaction tr;
        status_transaction status;
        memblock_op_behavior_t behavior;
        memblock_lsq_reservation_token_t token;
        bit saved_backend_done;
        bit has_progress;
        memblock_lq_key_t initial_lq_enq_ptr;
        memblock_sq_key_t initial_sq_enq_ptr;
        int unsigned initial_lq_free_count;
        int unsigned initial_sq_free_count;

        clear_manual_main_table();
        set_manual_main_transaction(0, make_scalar("soft_reset_pending_load",
                                                   MEMBLOCK_OP_CLASS_INT_LOAD, 0));
        import_manual_main_table();
        lsq_ctrl.reset_rtl_shadow();
        sample_seq = 0;
        lsq_ctrl.advance_rtl_admission_sample(next_snapshot(), 1'b0);
        admission_seq = memblock_lsqenq_dispatch_base_sequence::type_id::create(
            "reset_pending_reservation_seq");
        admission_seq.ensure_helpers();
        tr = data.get_main_transaction(0);
        behavior = memblock_op_behavior_util::derive_op_behavior(tr);
        initial_lq_enq_ptr = lsq_ctrl.lq_enq_ptr;
        initial_sq_enq_ptr = lsq_ctrl.sq_enq_ptr;
        initial_lq_free_count = lsq_ctrl.lq_free_count;
        initial_sq_free_count = lsq_ctrl.sq_free_count;
        lsq_ctrl.commit_allocate(0, behavior, tr);
        token.valid = 1'b1;
        token.uid = 0;
        token.launch_epoch = data.begin_lsq_reservation_launch(0);
        admission_seq.pending_sample_tokens.push_back(token);
        admission_seq.pending_sample_valid = 1'b1;
        admission_seq.pending_sample_target_sample_seq = sample_seq + 1;
        admission_seq.pending_sample_control_reset_epoch = memblock_sync_pkg::control_reset_epoch;
        admission_seq.pending_sample_shadow_generation = lsq_ctrl.shadow_baseline_generation;
        admission_seq.publish_sequence_local_state();
        status = data.get_status(0);
        check_expect(status.lsq_reservation_state ==
                         MEMBLOCK_LSQ_RESERVATION_LAUNCHED_PENDING_SAMPLE &&
               lsq_ctrl.shadow_last_sample_seq < admission_seq.pending_sample_target_sample_seq &&
               data.lsq_sequence_pending_sample,
               "reset directed test did not create an unsampled pending reservation");

        // 不推进 sample；reset 必须立即取消，不得等待 target 或调用 sampled。
        saved_backend_done = memblock_sync_pkg::reset_backend_done;
        memblock_sync_pkg::reset_backend_done = 1'b0;
        admission_seq.wait_or_cancel_pending_sample();
        has_progress = 1'b0;
        admission_seq.complete_v2_pending_sample(has_progress);
        check_expect(!admission_seq.pending_sample_valid &&
               admission_seq.pending_sample_tokens.size() == 0 &&
               admission_seq.pending_sample_target_sample_seq == 0 &&
               admission_seq.pending_sample_control_reset_epoch == 0 &&
               admission_seq.pending_sample_shadow_generation == 0 &&
               !data.lsq_sequence_pending_sample && !has_progress &&
               status.lsq_reservation_state == MEMBLOCK_LSQ_RESERVATION_NONE &&
               !status.lsq_reservation_sample_valid && status.lsq_reservation_sample_seq == 0 &&
               status.lsq_reservation_launch_epoch == 0 && !status.active && !status.enq &&
               !status.active_lq_mapped && !status.active_sq_mapped &&
               lsq_ctrl.lq_enq_ptr == initial_lq_enq_ptr &&
               lsq_ctrl.sq_enq_ptr == initial_sq_enq_ptr &&
               lsq_ctrl.lq_free_count == initial_lq_free_count &&
               lsq_ctrl.sq_free_count == initial_sq_free_count &&
               !status.issue_ready,
               "reset did not rollback pending token, owner, pointer and credit state");
        memblock_sync_pkg::reset_backend_done = saved_backend_done;
    endtask:check_reset_pending_reservation_cancel

    task check_atomic_held_piece_stability();
        main_control_transaction amo;
        memblock_op_behavior_t behavior;
        status_transaction status;
        memblock_issue_q_item_t item;
        memblock_issue_q_item_t first_item;
        memblock_issue_q_item_t load_items[$];
        memblock_issue_q_item_t sta_items[$];
        memblock_issue_q_item_t std_items[$];

        clear_manual_main_table();
        amo = main_control_transaction::type_id::create("soft_atomic_held_piece");
        amo.op_class = MEMBLOCK_OP_CLASS_AMO;
        amo.fuType = MEMBLOCK_FUTYPE_MOU;
        amo.fuOpType = MEMBLOCK_LSUOP_AMOCAS_D;
        amo.lsq_flow = MEMBLOCK_LSQ_FLOW_ATOMIC;
        amo.robIdx_flag = 1'b0;
        amo.robIdx_value = MEMBLOCK_ROB_VALUE_W'(0);
        amo.src_0 = 64'h9000_0000;
        amo.numLsElem = 0;
        amo.update_vaddr();
        set_manual_main_transaction(0, amo);
        import_manual_main_table();
        behavior = memblock_op_behavior_util::derive_op_behavior(
            data.get_main_transaction(0));
        lsq_ctrl.commit_atomic_non_lsq_admission(
            0, behavior, data.get_main_transaction(0),
            '{default:'0}, '{default:'0});
        data.establish_atomic_barrier(0);
        status = data.get_status(0);
        status.issue_ready = 1'b1;
        item = issue_sched.make_issue_item(0, MEMBLOCK_ISSUE_TARGET_STD, behavior);
        item.uop_index = 1;
        item.issue_pipe_index = 0;
        data.push_issue_queue_item(item);
        status.queued_std = 1'b1;

        issue_sched.select_issue_candidates(load_items, sta_items, std_items);
        check_expect(std_items.size() == 1 && data.atomic_held_piece_valid,
               "atomic selector did not freeze its first nonblocking piece");
        first_item = std_items[0];
        issue_sched.select_issue_candidates(load_items, sta_items, std_items);
        check_expect(std_items.size() == 1 &&
               data.atomic_held_piece_matches(std_items[0]) &&
               std_items[0].target == first_item.target &&
               std_items[0].uop_index == first_item.uop_index &&
               std_items[0].dynamic_epoch == first_item.dynamic_epoch,
               "ready=0 reselection changed atomic target/slot/payload identity");
        data.reset_active_atomic_after_physical_reset("directed soft test");
        check_expect(!data.active_atomic_barrier_valid &&
               !data.atomic_held_piece_valid && !status.active && !status.enq &&
               !status.queued_std && data.get_next_new_admit_uid() == 0,
               "physical reset did not clear atomic owner/barrier/held piece and reopen uid");
    endtask:check_atomic_held_piece_stability

    task check_atomic_token_reset_binding();
        memblock_lsqenq_dispatch_base_sequence admission_seq;

        lsq_ctrl.reset_rtl_shadow();
        sample_seq = 0;
        lsq_ctrl.advance_rtl_admission_sample(next_snapshot(), 1'b0);
        admission_seq = memblock_lsqenq_dispatch_base_sequence::type_id::create(
            "atomic_token_reset_binding_seq");
        admission_seq.ensure_helpers();
        admission_seq.atomic_token_stage = 1;
        admission_seq.atomic_token_control_reset_epoch =
            lsq_ctrl.shadow_control_reset_epoch;
        admission_seq.atomic_token_shadow_generation =
            lsq_ctrl.shadow_baseline_generation;
        check_expect(!admission_seq.atomic_token_reset_binding_invalid(),
               "fresh atomic token reset binding is unexpectedly invalid");
        admission_seq.atomic_token_control_reset_epoch++;
        check_expect(admission_seq.atomic_token_reset_binding_invalid(),
               "control epoch change did not invalidate pre-fire atomic token");
        admission_seq.atomic_token_control_reset_epoch =
            lsq_ctrl.shadow_control_reset_epoch;
        lsq_ctrl.reset_rtl_shadow();
        check_expect(admission_seq.atomic_token_reset_binding_invalid(),
               "shadow baseline rebuild did not invalidate pre-fire atomic token");
        admission_seq.clear_pre_fire_atomic_token("soft-test cleanup");
    endtask:check_atomic_token_reset_binding

    task check_cancel_shadow_restore_credential();
        memblock_sync_pkg::dispatch_raw_cancel_snapshot_t snapshot;
        memblock_lsq_cancel_record_t record;

        check_expect(data.cancel_record_q.size() == 0,
               "cancel restore soft test requires an empty record queue");
        lsq_ctrl.reset_rtl_shadow();
        sample_seq = 0;
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);

        record = '{default:'0};
        record.valid = 1'b1;
        record.redirect_epoch = 7;
        record.cancel_record_id = 91;
        record.redirect_anchor_valid = 1'b1;
        record.dut_cancel_update_sample_seq = sample_seq + 1;
        data.cancel_record_q.push_back(record);
        lsq_ctrl.shadow_t2_redirect = 1'b1;
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        check_expect(lsq_ctrl.shadow_t3_update &&
               lsq_ctrl.shadow_t3_cancel_record_valid &&
               lsq_ctrl.shadow_t3_cancel_record_id == record.cancel_record_id &&
               !data.cancel_record_q[0].shadow_restore_consumed,
               "t2 update did not bind the exact cancel record identity");
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        check_expect(data.cancel_record_q[0].shadow_restore_consumed &&
               data.cancel_record_q[0].shadow_restore_sample_seq == sample_seq,
               "K=0 t3 restore did not publish its consumption credential");
        data.cancel_record_q.delete();
    endtask:check_cancel_shadow_restore_credential

    task check_atomic_fault_redirect_classification();
        main_control_transaction amo;
        status_transaction status;
        memblock_op_behavior_t behavior;
        memblock_redirect_payload_t redirect;

        amo = main_control_transaction::type_id::create("soft_atomic_fault_redirect");
        amo.op_class = MEMBLOCK_OP_CLASS_AMO;
        amo.fuType = MEMBLOCK_FUTYPE_MOU;
        amo.fuOpType = MEMBLOCK_LSUOP_AMOADD_D;
        amo.lsq_flow = MEMBLOCK_LSQ_FLOW_ATOMIC;
        amo.numLsElem = 0;
        behavior = memblock_op_behavior_util::derive_op_behavior(amo);
        status = status_transaction::type_id::create("soft_atomic_fault_status");
        status.active = 1'b1;
        status.rob_commit = 1'b1;
        status.robIdx_flag = 1'b0;
        status.robIdx_value = MEMBLOCK_ROB_VALUE_W'(3);
        status.atomic_piece_fire_count = 2;
        status.atomic_finalized = 1'b1;
        status.fault = 1'b1;
        redirect = '{default:'0};
        redirect.valid = 1'b1;
        redirect.flush_itself = 1'b1;
        redirect.rob_key = status.get_rob_key();
        check_expect(data.fired_atomic_redirect_is_self_fault(
                   status, behavior, redirect, 1'b1),
               "fired atomic self fault-head redirect was rejected");
        check_expect(!data.fired_atomic_redirect_is_self_fault(
                    status, behavior, redirect, 1'b0),
               "external/non-fault-head redirect accepted fired atomic pieces");
        redirect.flush_itself = 1'b0;
        check_expect(!data.fired_atomic_redirect_is_self_fault(
                    status, behavior, redirect, 1'b1),
               "non-self redirect accepted fired atomic pieces");
    endtask:check_atomic_fault_redirect_classification

    task check_atomic_issue_fields();
        main_control_transaction amo;
        bit [63:0] std_data[4];
        bit [7:0] pdest0;
        bit [7:0] pdest2;

        amo = main_control_transaction::type_id::create("soft_amocas_q");
        amo.uid = 9;
        amo.op_class = MEMBLOCK_OP_CLASS_AMO;
        amo.fuType = MEMBLOCK_FUTYPE_MOU;
        amo.fuOpType = MEMBLOCK_LSUOP_AMOCAS_Q;
        amo.lsq_flow = MEMBLOCK_LSQ_FLOW_ATOMIC;
        amo.src_0 = 64'h8123_4567_89ab_cdef;
        amo.imm = 0;
        amo.numLsElem = 0;
        for (int unsigned slot = 0; slot < 4; slot++) begin
            std_data[slot] = field_assigner.derive_atomic_std_data(amo, slot);
        end
        for (int unsigned lhs = 0; lhs < 4; lhs++) begin
            for (int unsigned rhs = lhs + 1; rhs < 4; rhs++) begin
                check_expect(std_data[lhs] != std_data[rhs],
                       $sformatf("CAS.Q STD data aliases slots %0d/%0d", lhs, rhs));
            end
        end
        pdest0 = field_assigner.compute_atomic_sta_pdest(amo.uid, 0, 1'b1);
        pdest2 = field_assigner.compute_atomic_sta_pdest(amo.uid, 2, 1'b1);
        check_expect(pdest0 != pdest2, "CAS.Q STA0/STA2 pdest must be distinct");
    endtask:check_atomic_issue_fields

    task check_amo_addr_reuse_type_guard();
        localparam int unsigned AMO_ENTRY_COUNT = 16;
        int unsigned saved_op_class_int_load_wt;
        int unsigned saved_op_class_fp_load_wt;
        int unsigned saved_op_class_store_wt;
        int unsigned saved_op_class_prefetch_wt;
        int unsigned saved_op_class_amo_wt;
        int unsigned saved_op_class_cbo_wt;
        int unsigned saved_addr_reuse_en_1_wt;
        int unsigned saved_addr_reuse_en_0_wt;
        int unsigned saved_addr_reuse_load_after_store_wt;
        int unsigned saved_addr_reuse_load_after_load_wt;
        int unsigned saved_addr_reuse_store_after_load_wt;
        int unsigned saved_addr_reuse_store_after_store_wt;
        bit saved_boundary_profile_gen_en;
        memblock_uid_t recent_load_uid_q[$];
        memblock_uid_t recent_store_uid_q[$];
        main_control_transaction tr;

        saved_op_class_int_load_wt = seq_csr_common::op_class_int_load_wt;
        saved_op_class_fp_load_wt = seq_csr_common::op_class_fp_load_wt;
        saved_op_class_store_wt = seq_csr_common::op_class_store_wt;
        saved_op_class_prefetch_wt = seq_csr_common::op_class_prefetch_wt;
        saved_op_class_amo_wt = seq_csr_common::op_class_amo_wt;
        saved_op_class_cbo_wt = seq_csr_common::op_class_cbo_wt;
        saved_addr_reuse_en_1_wt = seq_csr_common::addr_reuse_en_1_wt;
        saved_addr_reuse_en_0_wt = seq_csr_common::addr_reuse_en_0_wt;
        saved_addr_reuse_load_after_store_wt =
            seq_csr_common::addr_reuse_load_after_store_wt;
        saved_addr_reuse_load_after_load_wt =
            seq_csr_common::addr_reuse_load_after_load_wt;
        saved_addr_reuse_store_after_load_wt =
            seq_csr_common::addr_reuse_store_after_load_wt;
        saved_addr_reuse_store_after_store_wt =
            seq_csr_common::addr_reuse_store_after_store_wt;
        saved_boundary_profile_gen_en = seq_csr_common::boundary_profile_gen_en;

        seq_csr_common::op_class_int_load_wt = 0;
        seq_csr_common::op_class_fp_load_wt = 0;
        seq_csr_common::op_class_store_wt = 0;
        seq_csr_common::op_class_prefetch_wt = 0;
        seq_csr_common::op_class_amo_wt = 1;
        seq_csr_common::op_class_cbo_wt = 0;
        seq_csr_common::addr_reuse_en_1_wt = 1;
        seq_csr_common::addr_reuse_en_0_wt = 0;
        seq_csr_common::addr_reuse_load_after_store_wt = 1;
        seq_csr_common::addr_reuse_load_after_load_wt = 0;
        seq_csr_common::addr_reuse_store_after_load_wt = 0;
        seq_csr_common::addr_reuse_store_after_store_wt = 0;
        seq_csr_common::boundary_profile_gen_en = 1'b0;

        build_random_main_table(AMO_ENTRY_COUNT);
        for (memblock_uid_t uid = 0; uid < AMO_ENTRY_COUNT; uid++) begin
            tr = data.get_main_transaction(uid);
            check_expect(tr.op_class == MEMBLOCK_OP_CLASS_AMO,
                   $sformatf("100%% address reuse rewrote AMO uid=%0d to op_class=%s",
                             uid, op_class_name(tr.op_class)));
            check_expect(is_amo_fuoptype(tr.fuOpType),
                   $sformatf("AMO-only uid=%0d has illegal atomic fuOpType=0x%0h",
                             uid, tr.fuOpType));
            check_expect(tr.send_pri == 100 && tr.send_pri_std == 100,
                   $sformatf("AMO-only uid=%0d lost fixed priority send/std=%0d/%0d",
                             uid, tr.send_pri, tr.send_pri_std));
        end

        tr = make_scalar("soft_normal_addr_reuse", MEMBLOCK_OP_CLASS_INT_LOAD,
                         AMO_ENTRY_COUNT);
        tr.uid = memblock_uid_t'(AMO_ENTRY_COUNT);
        tr.send_pri = 37;
        tr.send_pri_std = 42;
        apply_addr_reuse_window(tr, tr.uid, recent_load_uid_q, recent_store_uid_q);
        check_expect(tr.op_class == MEMBLOCK_OP_CLASS_STORE &&
               memblock_op_behavior_util::is_store_fuoptype(tr.fuOpType),
               "ordinary no-reference address-reuse fallback no longer selects a legal store");
        check_expect(tr.send_pri == 37 && tr.send_pri_std == 42,
               "ordinary address reuse unexpectedly inherited AMO priority");

        seq_csr_common::op_class_int_load_wt = saved_op_class_int_load_wt;
        seq_csr_common::op_class_fp_load_wt = saved_op_class_fp_load_wt;
        seq_csr_common::op_class_store_wt = saved_op_class_store_wt;
        seq_csr_common::op_class_prefetch_wt = saved_op_class_prefetch_wt;
        seq_csr_common::op_class_amo_wt = saved_op_class_amo_wt;
        seq_csr_common::op_class_cbo_wt = saved_op_class_cbo_wt;
        seq_csr_common::addr_reuse_en_1_wt = saved_addr_reuse_en_1_wt;
        seq_csr_common::addr_reuse_en_0_wt = saved_addr_reuse_en_0_wt;
        seq_csr_common::addr_reuse_load_after_store_wt =
            saved_addr_reuse_load_after_store_wt;
        seq_csr_common::addr_reuse_load_after_load_wt =
            saved_addr_reuse_load_after_load_wt;
        seq_csr_common::addr_reuse_store_after_load_wt =
            saved_addr_reuse_store_after_load_wt;
        seq_csr_common::addr_reuse_store_after_store_wt =
            saved_addr_reuse_store_after_store_wt;
        seq_csr_common::boundary_profile_gen_en = saved_boundary_profile_gen_en;
    endtask:check_amo_addr_reuse_type_guard

    virtual task body();
        ensure_dispatch_runtime_helpers();
        check_fixed_lane_prefix_and_partial();
        check_source_offer_target_sample();
        check_driver_launch_sample_binding();
        check_reset_pending_reservation_cancel();
        check_cancel_shadow_restore_credential();
        check_atomic_fault_redirect_classification();
        check_atomic_issue_fields();
        check_amo_addr_reuse_type_guard();
        check_atomic_held_piece_stability();
        check_atomic_token_reset_binding();
        data.publish_lsq_sequence_local_state(1'b1, 1'b0);
        check_expect(!data.runtime_drain_complete(),
               "runtime drain ignored sequence-local pending sample");
        data.publish_lsq_sequence_local_state(1'b0, 1'b1);
        check_expect(!data.runtime_drain_complete(),
               "runtime drain ignored atomic token/frozen offer");
        data.publish_lsq_sequence_local_state(1'b0, 1'b0);
        `uvm_info(get_type_name(), "LSQ/AMO directed software checks completed", UVM_LOW)
    endtask:body

endclass:soft_test_memblock_lsq_amo_admission_sequence

`endif
