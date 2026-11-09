//=========================================================
//File name    : lsq_ctrl_model.sv
//Author       : OpenAI_Codex
//Module name  : lsq_ctrl_model
//Discribution : software LSQ allocation mirror for dispatch framework
//Date         : 2026-05-18
//=========================================================
`ifndef LSQ_CTRL_MODEL__SV
`define LSQ_CTRL_MODEL__SV

class lsq_ctrl_model extends uvm_object;

    static lsq_ctrl_model m_inst;

    common_data_transaction data;

    memblock_lq_key_t lq_enq_ptr;
    memblock_sq_key_t sq_enq_ptr;
    memblock_lq_key_t lq_deq_ptr;
    memblock_sq_key_t sq_deq_ptr;
    int unsigned      lq_free_count;
    int unsigned      sq_free_count;

    // Registered upstream admission shadow.  These fields model NewDispatch /
    // LsqEnqCtrl source credit and must never be updated by software owner
    // release/cancel helpers below.
    memblock_lq_key_t shadow_lq_enq_ptr;
    memblock_sq_key_t shadow_sq_enq_ptr;
    int unsigned      shadow_lq_credit;
    int unsigned      shadow_sq_credit;
    bit               shadow_can_accept;
    bit               shadow_baseline_valid;
    bit               shadow_t1_redirect;
    bit               shadow_t2_redirect;
    bit               shadow_t3_update;
    bit               shadow_t3_cancel_record_valid;
    int unsigned      shadow_t3_cancel_record_id;
    int unsigned      shadow_t3_redirect_epoch;
    longint unsigned  shadow_last_sample_seq;
    int unsigned      shadow_control_reset_epoch;
    int unsigned      shadow_baseline_generation;
    bit               shadow_source_offer_valid;
    longint unsigned  shadow_source_target_sample_seq;
    bit               shadow_source_need_alloc_nonzero;
    int unsigned      shadow_source_lq_alloc;
    int unsigned      shadow_source_sq_alloc;
    bit [MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM-1:0] shadow_source_fire_mask;
    bit               shadow_d_valid[MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM];
    bit               shadow_r_valid[MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM];
    bit               shadow_q_valid[MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM];
    memblock_uid_t    shadow_d_uid[MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM];
    memblock_uid_t    shadow_r_uid[MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM];
    memblock_uid_t    shadow_q_uid[MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM];
    bit               shadow_d_is_load[MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM];
    bit               shadow_r_is_load[MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM];
    bit               shadow_q_is_load[MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM];
    bit               shadow_d_is_store[MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM];
    bit               shadow_r_is_store[MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM];
    bit               shadow_q_is_store[MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM];
    bit               shadow_d_is_atomic[MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM];
    bit               shadow_r_is_atomic[MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM];
    bit               shadow_q_is_atomic[MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM];
    int unsigned      shadow_conserve_flow_total[MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM];
    bit               shadow_iq_recompute_hold;

    `uvm_object_utils(lsq_ctrl_model)

    function new(string name = "lsq_ctrl_model");
        super.new(name);
        data = common_data_transaction::get();
        shadow_baseline_generation = 0;
        reset();
    endfunction:new

    static function lsq_ctrl_model get();
        if (m_inst == null) begin
            m_inst = new("lsq_ctrl_model_singleton");
        end
        return m_inst;
    endfunction:get

    function void reset();
        lq_enq_ptr    = '{default:'0};
        sq_enq_ptr    = '{default:'0};
        lq_deq_ptr    = '{default:'0};
        sq_deq_ptr    = '{default:'0};
        lq_free_count = MEMBLOCK_LQ_SIZE;
        sq_free_count = MEMBLOCK_SQ_SIZE;
        reset_rtl_shadow();
    endfunction:reset

    function void reset_rtl_shadow();
        shadow_baseline_generation++;
        if (shadow_baseline_generation == 0) begin
            `uvm_fatal("LSQ_SHADOW", "shadow baseline generation wrapped")
        end
        shadow_lq_enq_ptr = '{default:'0};
        shadow_sq_enq_ptr = '{default:'0};
        shadow_lq_credit = MEMBLOCK_LQ_SIZE;
        shadow_sq_credit = MEMBLOCK_SQ_SIZE;
        shadow_can_accept = 1'b0;
        shadow_baseline_valid = 1'b0;
        shadow_t1_redirect = 1'b0;
        shadow_t2_redirect = 1'b0;
        shadow_t3_update = 1'b0;
        shadow_t3_cancel_record_valid = 1'b0;
        shadow_t3_cancel_record_id = 0;
        shadow_t3_redirect_epoch = 0;
        shadow_last_sample_seq = 0;
        shadow_control_reset_epoch = 0;
        shadow_source_offer_valid = 1'b0;
        shadow_source_target_sample_seq = 0;
        shadow_source_need_alloc_nonzero = 1'b0;
        shadow_source_lq_alloc = 0;
        shadow_source_sq_alloc = 0;
        shadow_source_fire_mask = '0;
        shadow_iq_recompute_hold = 1'b1;
        foreach (shadow_d_valid[idx]) begin
            shadow_d_valid[idx] = 1'b0;
            shadow_r_valid[idx] = 1'b0;
            shadow_q_valid[idx] = 1'b0;
            shadow_d_is_atomic[idx] = 1'b0;
            shadow_r_is_atomic[idx] = 1'b0;
            shadow_q_is_atomic[idx] = 1'b0;
            shadow_conserve_flow_total[idx] = 0;
        end
        if (data != null) begin
            data.publish_lsq_shadow_redirect_pipeline_idle(1'b1);
            data.publish_lsq_shadow_source_state(1'b1, 1'b0);
        end
    endfunction:reset_rtl_shadow

    function bit rtl_pipe_empty();
        foreach (shadow_d_valid[idx]) begin
            if (shadow_d_valid[idx] || shadow_r_valid[idx] || shadow_q_valid[idx]) begin
                return 1'b0;
            end
        end
        return 1'b1;
    endfunction:rtl_pipe_empty

    function bit rtl_q_has_valid();
        foreach (shadow_q_valid[idx]) begin
            if (shadow_q_valid[idx]) begin
                return 1'b1;
            end
        end
        return 1'b0;
    endfunction:rtl_q_has_valid

    function void publish_source_state();
        if (data == null) begin
            data = common_data_transaction::get();
        end
        data.publish_lsq_shadow_source_state(rtl_pipe_empty(),
                                             shadow_source_offer_valid);
    endfunction:publish_source_state

    function bit stage_dispatch_bundle(input memblock_uid_t uids[$],
                                       input memblock_op_behavior_t behaviors[$],
                                       input bit is_atomic = 1'b0);
        if (uids.size() == 0 || uids.size() != behaviors.size() ||
            uids.size() > MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM || !rtl_pipe_empty() ||
            shadow_source_offer_valid) begin
            return 1'b0;
        end
        if (is_atomic && uids.size() != 1) begin
            `uvm_fatal("LSQ_SHADOW", "atomic D bundle must contain exactly lane0")
        end
        foreach (shadow_d_valid[idx]) begin
            shadow_d_valid[idx] = 1'b0;
            shadow_d_is_atomic[idx] = 1'b0;
        end
        foreach (uids[idx]) begin
            if (is_atomic) begin
                if (!behaviors[idx].is_atomic || behaviors[idx].need_alloc != 2'b00 ||
                    behaviors[idx].uses_lq || behaviors[idx].uses_sq) begin
                    `uvm_fatal("LSQ_SHADOW",
                               $sformatf("invalid atomic D token uid=%0d needAlloc=%0b uses_lq/sq=%0b/%0b",
                                         uids[idx], behaviors[idx].need_alloc,
                                         behaviors[idx].uses_lq, behaviors[idx].uses_sq))
                end
            end else if (behaviors[idx].is_atomic ||
                         behaviors[idx].num_ls_elem != memblock_num_ls_elem_t'(1) ||
                         !(behaviors[idx].need_alloc inside {2'b01, 2'b10})) begin
                `uvm_fatal("LSQ_SHADOW",
                           $sformatf("invalid D bundle lane=%0d uid=%0d needAlloc=%0b numLsElem=%0d",
                                     idx, uids[idx], behaviors[idx].need_alloc,
                                     behaviors[idx].num_ls_elem))
            end
            shadow_d_valid[idx] = 1'b1;
            shadow_d_uid[idx] = uids[idx];
            shadow_d_is_load[idx] = behaviors[idx].uses_lq;
            shadow_d_is_store[idx] = behaviors[idx].uses_sq;
            shadow_d_is_atomic[idx] = is_atomic;
        end
        publish_source_state();
        return 1'b1;
    endfunction:stage_dispatch_bundle

    function bit rtl_admission_ready();
        return shadow_baseline_valid && shadow_can_accept;
    endfunction:rtl_admission_ready

    function bit rtl_redirect_pipeline_idle();
        return !shadow_t1_redirect && !shadow_t2_redirect &&
               !shadow_t3_update;
    endfunction:rtl_redirect_pipeline_idle

    function void note_source_offer(
        input bit [MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM-1:0] fire_mask,
        input bit need_alloc_nonzero,
        input int unsigned lq_alloc,
        input int unsigned sq_alloc,
        input longint unsigned target_sample_seq);
        if (shadow_source_offer_valid) begin
            `uvm_fatal("LSQ_SHADOW", "source offer overwritten before its DUT sample was serviced")
        end
        if (lq_alloc > MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH ||
            sq_alloc > MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH) begin
            `uvm_fatal("LSQ_SHADOW",
                       $sformatf("source launch exceeds width lq=%0d/%0d sq=%0d/%0d",
                                 lq_alloc, MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH,
                                 sq_alloc, MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH))
        end
        if (!need_alloc_nonzero && (lq_alloc != 0 || sq_alloc != 0)) begin
            `uvm_fatal("LSQ_SHADOW", "zero-needAlloc source offer cannot allocate LQ/SQ entries")
        end
        if (target_sample_seq == 0 || target_sample_seq <= shadow_last_sample_seq) begin
            `uvm_fatal("LSQ_SHADOW",
                       $sformatf("source offer target=%0d must be newer than completed shadow sample=%0d",
                                 target_sample_seq, shadow_last_sample_seq))
        end
        shadow_source_offer_valid = 1'b1;
        shadow_source_target_sample_seq = target_sample_seq;
        shadow_source_need_alloc_nonzero = need_alloc_nonzero;
        shadow_source_lq_alloc = lq_alloc;
        shadow_source_sq_alloc = sq_alloc;
        shadow_source_fire_mask = fire_mask;
        publish_source_state();
    endfunction:note_source_offer

    // main service 先安装本拍的 driver offer，再推进 shadow。未来拍 offer
    // 保留在 FIFO 中，不能因 service 看到了较早的 snapshot 而提前消费。
    function void advance_driver_bound_admission_sample(
        input memblock_sync_pkg::dispatch_raw_cancel_snapshot_t snapshot,
        input bit redirect_sampled
    );
        memblock_sync_pkg::dispatch_lsq_source_offer_t source_offer;
        bit consumed_offer;

        if (snapshot.sample_seq <= shadow_last_sample_seq) return;
        consumed_offer = 1'b0;
        while (memblock_sync_pkg::peek_lsq_source_offer(source_offer)) begin
            if (source_offer.control_reset_epoch < snapshot.control_reset_epoch) begin
                void'(memblock_sync_pkg::pop_lsq_source_offer(source_offer));
                continue;
            end
            if (source_offer.control_reset_epoch > snapshot.control_reset_epoch) break;
            if (source_offer.target_sample_seq > snapshot.sample_seq) break;
            if (source_offer.target_sample_seq < snapshot.sample_seq) begin
                `uvm_fatal("LSQ_SHADOW",
                           $sformatf("missed driver source offer target/current=%0d/%0d",
                                     source_offer.target_sample_seq, snapshot.sample_seq))
            end
            void'(memblock_sync_pkg::pop_lsq_source_offer(source_offer));
            note_source_offer(source_offer.fire_mask,
                              source_offer.need_alloc_nonzero,
                              source_offer.lq_alloc,
                              source_offer.sq_alloc,
                              source_offer.target_sample_seq);
            consumed_offer = 1'b1;
            break;
        end
        if (!consumed_offer && memblock_sync_pkg::lsq_source_offer_required(
                snapshot.sample_seq, snapshot.control_reset_epoch)) begin
            `uvm_fatal("LSQ_SHADOW", "missing driver source offer for completed source edge")
        end
        advance_rtl_admission_sample(snapshot, redirect_sampled);
    endfunction:advance_driver_bound_admission_sample

    function void advance_rtl_admission_sample(
        input memblock_sync_pkg::dispatch_raw_cancel_snapshot_t snapshot,
        input bit redirect_sampled
    );
        int unsigned accepted_lq;
        int unsigned accepted_sq;
        int unsigned next_lq_credit;
        int unsigned next_sq_credit;
        bit          t2_update;
        bit          next_can_accept;
        bit          any_q;
        bit          any_r;
        bit          any_d;
        bit          any_fire;
        bit          all_fire;
        bit          can_acc;
        bit          r_ready;
        bit          left_fire;
        bit          capture_q;
        bit          consume_source_offer;
        bit          source_offer_for_sample;
        int unsigned rename_prefix;
        int unsigned dispatch_prefix;
        int          shadow_restore_record_idx;
        memblock_lsq_cancel_record_t shadow_restore_record;

        if (!snapshot.valid || !snapshot.complete || snapshot.sample_seq == 0) begin
            `uvm_fatal("LSQ_SHADOW", "advance requires one complete, non-zero ctrl sample")
        end
        if (shadow_baseline_valid &&
            snapshot.control_reset_epoch != shadow_control_reset_epoch) begin
            reset_rtl_shadow();
        end
        if (shadow_last_sample_seq != 0 && snapshot.sample_seq != shadow_last_sample_seq + 1) begin
            `uvm_fatal("LSQ_SHADOW",
                       $sformatf("ctrl sample discontinuity previous=%0d current=%0d",
                                 shadow_last_sample_seq, snapshot.sample_seq))
        end
        if (!snapshot.backend_done) begin
            reset_rtl_shadow();
            return;
        end

        if (shadow_source_offer_valid &&
            snapshot.sample_seq > shadow_source_target_sample_seq) begin
            `uvm_fatal("LSQ_SHADOW",
                       $sformatf("missed source offer target sample target=%0d observed=%0d",
                                 shadow_source_target_sample_seq, snapshot.sample_seq))
        end
        source_offer_for_sample = shadow_source_offer_valid &&
                                  snapshot.sample_seq == shadow_source_target_sample_seq;
        consume_source_offer = source_offer_for_sample;

        accepted_lq = (source_offer_for_sample && shadow_can_accept && !redirect_sampled) ?
                      shadow_source_lq_alloc : 0;
        accepted_sq = (source_offer_for_sample && shadow_can_accept && !redirect_sampled) ?
                      shadow_source_sq_alloc : 0;
        any_q = 1'b0;
        any_r = 1'b0;
        any_d = 1'b0;
        any_fire = 1'b0;
        all_fire = 1'b1;
        foreach (shadow_q_valid[idx]) begin
            any_q |= shadow_q_valid[idx];
            any_r |= shadow_r_valid[idx];
            any_d |= shadow_d_valid[idx];
            if (source_offer_for_sample && shadow_source_fire_mask[idx]) begin
                if (!shadow_q_valid[idx]) begin
                    `uvm_fatal("LSQ_SHADOW",
                               $sformatf("source fire lane=%0d has no matching valid Q lane", idx))
                end
                any_fire = 1'b1;
            end
            all_fire &= !shadow_q_valid[idx] ||
                        (source_offer_for_sample && shadow_source_fire_mask[idx]);
        end
        can_acc = all_fire || !any_q;
        r_ready = !shadow_r_valid[0] || can_acc;
        left_fire = r_ready && any_d;
        capture_q = any_r && can_acc;
        t2_update = shadow_t2_redirect &&
                    !(source_offer_for_sample && shadow_source_need_alloc_nonzero);

        // next_can uses old registered credit and this edge's accepted amount;
        // physical deq is intentionally not combinationally bypassed.
        next_can_accept = shadow_lq_credit >= accepted_lq + MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH &&
                          shadow_sq_credit >= accepted_sq + MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH &&
                          !t2_update;

        next_lq_credit = shadow_lq_credit + snapshot.lq_deq_count;
        next_sq_credit = shadow_sq_credit + snapshot.sq_deq_count;
        if (shadow_t3_update) begin
            if (!shadow_t3_cancel_record_valid) begin
                `uvm_fatal("LSQ_SHADOW", "t3 restore has no bound cancel record identity")
            end
            shadow_restore_record_idx =
                data.find_cancel_record_index_by_id(shadow_t3_cancel_record_id);
            if (shadow_restore_record_idx < 0) begin
                `uvm_fatal("LSQ_SHADOW",
                           $sformatf("t3 restore lost bound cancel record id=%0d epoch=%0d sample=%0d",
                                     shadow_t3_cancel_record_id,
                                     shadow_t3_redirect_epoch,
                                     snapshot.sample_seq))
                return;
            end
            shadow_restore_record = data.cancel_record_q[shadow_restore_record_idx];
            if (!shadow_restore_record.valid ||
                shadow_restore_record.redirect_epoch != shadow_t3_redirect_epoch ||
                shadow_restore_record.compare_snapshot_sample_seq != snapshot.sample_seq) begin
                `uvm_fatal("LSQ_SHADOW",
                           $sformatf("t3 restore sample mismatch id=%0d epoch=%0d sample=%0d record_idx=%0d compare_sample=%0d",
                                     shadow_t3_cancel_record_id,
                                     shadow_t3_redirect_epoch,
                                     snapshot.sample_seq,
                                     shadow_restore_record_idx,
                                     shadow_restore_record.compare_snapshot_sample_seq))
            end
            // 中文注释：t2 只绑定 cancel record；monitor offset 后的 compare snapshot
            // 才携带 DUT 可见取消数，t3 必须用本拍观测值恢复 shadow credit 和 pointer。
            next_lq_credit += snapshot.lq_cancel_count;
            next_sq_credit += snapshot.sq_cancel_count;
            shadow_lq_enq_ptr = rewind_lq_key(shadow_lq_enq_ptr, snapshot.lq_cancel_count);
            shadow_sq_enq_ptr = rewind_sq_key(shadow_sq_enq_ptr, snapshot.sq_cancel_count);
            data.mark_cancel_shadow_restore_consumed(
                shadow_t3_cancel_record_id,
                shadow_t3_redirect_epoch,
                snapshot.sample_seq);
        end else if (!redirect_sampled && shadow_can_accept) begin
            if (next_lq_credit < accepted_lq || next_sq_credit < accepted_sq) begin
                `uvm_fatal("LSQ_SHADOW", "registered credit underflow on accepted source launch")
            end
            next_lq_credit -= accepted_lq;
            next_sq_credit -= accepted_sq;
            shadow_lq_enq_ptr = advance_lq_key(shadow_lq_enq_ptr, accepted_lq);
            shadow_sq_enq_ptr = advance_sq_key(shadow_sq_enq_ptr, accepted_sq);
        end
        if (next_lq_credit > MEMBLOCK_LQ_SIZE || next_sq_credit > MEMBLOCK_SQ_SIZE) begin
            `uvm_fatal("LSQ_SHADOW",
                       $sformatf("registered credit overflow next=%0d/%0d capacity=%0d/%0d",
                                 next_lq_credit, next_sq_credit,
                                 MEMBLOCK_LQ_SIZE, MEMBLOCK_SQ_SIZE))
        end

        shadow_lq_credit = next_lq_credit;
        shadow_sq_credit = next_sq_credit;
        shadow_can_accept = next_can_accept;
        shadow_baseline_valid = 1'b1;
        shadow_t3_update = t2_update;
        if (t2_update) begin
            if (!data.bind_cancel_record_for_shadow_restore(
                    snapshot.sample_seq,
                    shadow_t3_cancel_record_id,
                    shadow_t3_redirect_epoch)) begin
                `uvm_fatal("LSQ_SHADOW",
                           $sformatf("t2 update sample=%0d has no exact cancel record binding",
                                     snapshot.sample_seq))
            end
            shadow_t3_cancel_record_valid = 1'b1;
        end else begin
            shadow_t3_cancel_record_valid = 1'b0;
            shadow_t3_cancel_record_id = 0;
            shadow_t3_redirect_epoch = 0;
        end
        shadow_t2_redirect = shadow_t1_redirect;
        shadow_t1_redirect = redirect_sampled;
        shadow_last_sample_seq = snapshot.sample_seq;
        shadow_control_reset_epoch = snapshot.control_reset_epoch;

        rename_prefix = 0;
        dispatch_prefix = 0;
        foreach (shadow_conserve_flow_total[idx]) begin
            if ((shadow_r_valid[idx] && shadow_r_is_load[idx]) ||
                shadow_r_is_store[idx]) begin
                rename_prefix++;
            end
            if (shadow_q_valid[idx] &&
                (shadow_q_is_load[idx] || shadow_q_is_store[idx])) begin
                dispatch_prefix++;
            end
            shadow_conserve_flow_total[idx] = all_fire ?
                                                      rename_prefix :
                                                      dispatch_prefix;
        end

        if (redirect_sampled) begin
            foreach (shadow_q_valid[idx]) begin
                shadow_d_valid[idx] = 1'b0;
                shadow_r_valid[idx] = 1'b0;
                shadow_q_valid[idx] = 1'b0;
            end
            shadow_iq_recompute_hold = 1'b1;
        end else begin
            // PipeGroupConnect fixed-lane priority: capture a complete R bundle
            // when canAcc, otherwise clear only the lanes that actually fired.
            foreach (shadow_q_valid[idx]) begin
                if (capture_q) begin
                    shadow_q_valid[idx] = shadow_r_valid[idx];
                    if (shadow_r_valid[idx]) begin
                        shadow_q_uid[idx] = shadow_r_uid[idx];
                        shadow_q_is_load[idx] = shadow_r_is_load[idx];
                        shadow_q_is_store[idx] = shadow_r_is_store[idx];
                        shadow_q_is_atomic[idx] = shadow_r_is_atomic[idx];
                    end
                end else if (source_offer_for_sample &&
                             shadow_source_fire_mask[idx]) begin
                    shadow_q_valid[idx] = 1'b0;
                end
            end

            if (left_fire) begin
                foreach (shadow_r_valid[idx]) begin
                    shadow_r_valid[idx] = shadow_d_valid[idx];
                    if (shadow_d_valid[idx]) begin
                        shadow_r_uid[idx] = shadow_d_uid[idx];
                        shadow_r_is_load[idx] = shadow_d_is_load[idx];
                        shadow_r_is_store[idx] = shadow_d_is_store[idx];
                        shadow_r_is_atomic[idx] = shadow_d_is_atomic[idx];
                    end
                    shadow_d_valid[idx] = 1'b0;
                end
            end else if (r_ready) begin
                foreach (shadow_r_valid[idx]) begin
                    // Keep bits/uid/category intact when valid clears.  In
                    // particular an invalid old Store still contributes to
                    // RenamePrefix exactly as the RTL expression does.
                    shadow_r_valid[idx] = 1'b0;
                end
            end

            if (capture_q || (any_fire && !all_fire)) begin
                shadow_iq_recompute_hold = 1'b1;
            end else if (shadow_iq_recompute_hold) begin
                shadow_iq_recompute_hold = 1'b0;
            end
        end

        if (consume_source_offer) begin
            shadow_source_offer_valid = 1'b0;
            shadow_source_target_sample_seq = 0;
            shadow_source_need_alloc_nonzero = 1'b0;
            shadow_source_lq_alloc = 0;
            shadow_source_sq_alloc = 0;
            shadow_source_fire_mask = '0;
        end
        data.publish_lsq_shadow_redirect_pipeline_idle(rtl_redirect_pipeline_idle());
        publish_source_state();
    endfunction:advance_rtl_admission_sample

    static function bit is_vector_ls_futype(input bit [MEMBLOCK_INTERNAL_FUTYPE_W-1:0] fuType);
        return memblock_op_behavior_util::is_vector_ls_futype(fuType);
    endfunction:is_vector_ls_futype

    static function bit is_load_fuoptype(input bit [8:0] fuOpType);
        return memblock_op_behavior_util::is_load_fuoptype(fuOpType);
    endfunction:is_load_fuoptype

    static function bit is_prefetch_fuoptype(input bit [8:0] fuOpType);
        return memblock_op_behavior_util::is_prefetch_fuoptype(fuOpType);
    endfunction:is_prefetch_fuoptype

    static function bit is_store_fuoptype(input bit [8:0] fuOpType);
        return memblock_op_behavior_util::is_store_fuoptype(fuOpType);
    endfunction:is_store_fuoptype

    static function bit is_cbo_fuoptype(input bit [8:0] fuOpType);
        return memblock_op_behavior_util::is_cbo_fuoptype(fuOpType);
    endfunction:is_cbo_fuoptype

    static function bit is_amocas_q_fuoptype(input bit [8:0] fuOpType);
        return memblock_op_behavior_util::is_amocas_q_fuoptype(fuOpType);
    endfunction:is_amocas_q_fuoptype

    static function bit is_amocas_wd_fuoptype(input bit [8:0] fuOpType);
        return memblock_op_behavior_util::is_amocas_wd_fuoptype(fuOpType);
    endfunction:is_amocas_wd_fuoptype

    static function bit is_amo_fuoptype(input bit [8:0] fuOpType);
        return memblock_op_behavior_util::is_amo_fuoptype(fuOpType);
    endfunction:is_amo_fuoptype

    static function memblock_op_behavior_t make_default_behavior();
        return memblock_op_behavior_util::make_default_behavior();
    endfunction:make_default_behavior

    static function memblock_op_behavior_t derive_op_behavior(input main_control_transaction tr);
        return memblock_op_behavior_util::derive_op_behavior(tr);
    endfunction:derive_op_behavior

//   所以 atomic_data_uop_count 表示：
//   这个 atomic 需要发几个“数据侧 uop”。
//   举例：
//   AMOCAS_Q:
//     atomic_sta_uop_count  = 2
//     atomic_data_uop_count = 4
//   意思是测试框架会把这个 AMOCAS_Q 抽象成：
//   - 2 个地址侧 STA uop
//   - 4 个数据侧 STD/data uop
//   为什么 CAS 会更多？因为 CAS 需要比较旧值和写入新值，宽度越大，内部拆分的数据片越多。Q 比 W/D 更宽，所以 data uop 数更多。
//   STA uop 数表示地址侧要发几份；data/STD uop 数表示数据侧要发几份。它们用于测试框架决定 atomic 要往 STA/STD 两条 issue 路径拆多少个微操作。

    static function memblock_lq_key_t advance_lq_key(input memblock_lq_key_t base,
                                                     input int unsigned step);
        memblock_lq_key_t cur;

        if (base.value >= MEMBLOCK_LQ_SIZE) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("lqIdx value=%0d exceeds LQ size=%0d", base.value, MEMBLOCK_LQ_SIZE))
        end
        cur = base;
        repeat (step) begin
            if (cur.value == MEMBLOCK_LQ_SIZE - 1) begin
                cur.value = '0;
                cur.flag  = ~cur.flag;
            end else begin
                cur.value = cur.value + 1'b1;
            end
        end
        return cur;
    endfunction:advance_lq_key

    static function memblock_sq_key_t advance_sq_key(input memblock_sq_key_t base,
                                                     input int unsigned step);
        memblock_sq_key_t cur;

        if (base.value >= MEMBLOCK_SQ_SIZE) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("sqIdx value=%0d exceeds SQ size=%0d", base.value, MEMBLOCK_SQ_SIZE))
        end
        cur = base;
        repeat (step) begin
            if (cur.value == MEMBLOCK_SQ_SIZE - 1) begin
                cur.value = '0;
                cur.flag  = ~cur.flag;
            end else begin
                cur.value = cur.value + 1'b1;
            end
        end
        return cur;
    endfunction:advance_sq_key

    static function memblock_lq_key_t rewind_lq_key(input memblock_lq_key_t base,
                                                    input int unsigned step);
        memblock_lq_key_t cur;

        if (base.value >= MEMBLOCK_LQ_SIZE) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("lqIdx value=%0d exceeds LQ size=%0d", base.value, MEMBLOCK_LQ_SIZE))
        end
        cur = base;
        repeat (step) begin
            if (cur.value == 0) begin
                cur.value = MEMBLOCK_LQ_SIZE - 1;
                cur.flag  = ~cur.flag;
            end else begin
                cur.value = cur.value - 1'b1;
            end
        end
        return cur;
    endfunction:rewind_lq_key

    static function memblock_sq_key_t rewind_sq_key(input memblock_sq_key_t base,
                                                    input int unsigned step);
        memblock_sq_key_t cur;

        if (base.value >= MEMBLOCK_SQ_SIZE) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("sqIdx value=%0d exceeds SQ size=%0d", base.value, MEMBLOCK_SQ_SIZE))
        end
        cur = base;
        repeat (step) begin
            if (cur.value == 0) begin
                cur.value = MEMBLOCK_SQ_SIZE - 1;
                cur.flag  = ~cur.flag;
            end else begin
                cur.value = cur.value - 1'b1;
            end
        end
        return cur;
    endfunction:rewind_sq_key

    function bit can_allocate(input memblock_op_behavior_t behavior);
        if (behavior.uses_lq && lq_free_count < behavior.num_ls_elem) begin
            return 1'b0;
        end
        if (behavior.uses_sq && sq_free_count < behavior.num_ls_elem) begin
            return 1'b0;
        end
        return 1'b1;
    endfunction:can_allocate

    function void preview_allocate(input memblock_op_behavior_t behavior,
                                   output memblock_lq_key_t lq_key,
                                   output memblock_sq_key_t sq_key);
        lq_key = lq_enq_ptr;
        sq_key = sq_enq_ptr;
        if (!can_allocate(behavior)) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("preview_allocate lacks resources: kind=%0d lq_free=%0d sq_free=%0d need=%0d",
                                             behavior.kind, lq_free_count, sq_free_count, behavior.num_ls_elem))
        end
    endfunction:preview_allocate

    function void commit_allocate(input memblock_uid_t uid,
                                  input memblock_op_behavior_t behavior,
                                  input main_control_transaction tr);
        memblock_lq_key_t lq_key;
        memblock_sq_key_t sq_key;

        if (data == null) begin
            data = common_data_transaction::get();
        end
        if (tr == null) begin
            `uvm_fatal("LSQ_CTRL", "commit_allocate got null transaction")
        end
        if (tr.uid != uid) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("commit_allocate uid mismatch: arg=%0d tr.uid=%0d", uid, tr.uid))
        end
        preview_allocate(behavior, lq_key, sq_key);

        tr.lqIdx_flag  = lq_key.flag;
        tr.lqIdx_value = lq_key.value;
        tr.sqIdx_flag  = sq_key.flag;
        tr.sqIdx_value = sq_key.value;
        tr.numLsElem   = behavior.num_ls_elem;

        data.set_main_transaction(uid, tr);
        data.activate_uid(uid, behavior.uses_lq, behavior.uses_sq);
        data.set_status_field(uid, MEMBLOCK_STATUS_ENQ, 1'b1);

        if (behavior.uses_lq) begin
            lq_enq_ptr = advance_lq_key(lq_enq_ptr, behavior.num_ls_elem);
            lq_free_count -= behavior.num_ls_elem;
        end
        if (behavior.uses_sq) begin
            sq_enq_ptr = advance_sq_key(sq_enq_ptr, behavior.num_ls_elem);
            sq_free_count -= behavior.num_ls_elem;
        end
    endfunction:commit_allocate

    function void commit_allocate_with_resp(input memblock_uid_t uid,
                                            input memblock_op_behavior_t behavior,
                                            input main_control_transaction tr,
                                            input memblock_lq_key_t dut_lq_key,
                                            input memblock_sq_key_t dut_sq_key);
        memblock_lq_key_t expected_lq_key;
        memblock_sq_key_t expected_sq_key;

        if (data == null) begin
            data = common_data_transaction::get();
        end
        if (tr == null) begin
            `uvm_fatal("LSQ_CTRL", "commit_allocate_with_resp got null transaction")
        end
        if (tr.uid != uid) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("commit_allocate_with_resp uid mismatch: arg=%0d tr.uid=%0d", uid, tr.uid))
        end
        if (behavior.need_alloc == 2'b00) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("commit_allocate_with_resp got non-LSQ admission uid=%0d", uid))
        end

        preview_allocate(behavior, expected_lq_key, expected_sq_key);
        if ((behavior.uses_lq && dut_lq_key != expected_lq_key) ||
            (behavior.uses_sq && dut_sq_key != expected_sq_key)) begin
            `uvm_fatal("LSQ_CTRL",
                       $sformatf("uid=%0d LSQ enq resp mismatch: expected lq={%0d,%0d} sq={%0d,%0d}, got lq={%0d,%0d} sq={%0d,%0d}",
                                 uid,
                                 expected_lq_key.flag,
                                 expected_lq_key.value,
                                 expected_sq_key.flag,
                                 expected_sq_key.value,
                                 dut_lq_key.flag,
                                 dut_lq_key.value,
                                 dut_sq_key.flag,
                                 dut_sq_key.value))
        end

        commit_allocate(uid, behavior, tr);
    endfunction:commit_allocate_with_resp

    function void commit_non_lsq_admission(input memblock_uid_t uid,
                                           input memblock_op_behavior_t behavior,
                                           input main_control_transaction tr);
        if (behavior.need_alloc != 2'b00 || behavior.uses_lq || behavior.uses_sq) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("commit_non_lsq_admission got LSQ allocating kind=%0d", behavior.kind))
        end
        commit_allocate(uid, behavior, tr);
    endfunction:commit_non_lsq_admission

    function void commit_atomic_non_lsq_admission(
        input memblock_uid_t uid,
        input memblock_op_behavior_t behavior,
        input main_control_transaction tr,
        input memblock_lq_key_t shadow_lq_snapshot,
        input memblock_sq_key_t shadow_sq_snapshot
    );
        status_transaction status;

        if (!behavior.is_atomic || behavior.need_alloc != 2'b00 ||
            behavior.uses_lq || behavior.uses_sq) begin
            `uvm_fatal("LSQ_CTRL", "atomic non-LSQ admission received allocating behavior")
        end
        if (tr == null || tr.uid != uid) begin
            `uvm_fatal("LSQ_CTRL", "atomic non-LSQ admission transaction identity mismatch")
        end
        tr.lqIdx_flag = shadow_lq_snapshot.flag;
        tr.lqIdx_value = shadow_lq_snapshot.value;
        tr.sqIdx_flag = shadow_sq_snapshot.flag;
        tr.sqIdx_value = shadow_sq_snapshot.value;
        tr.numLsElem = '0;
        data.set_main_transaction(uid, tr);
        data.activate_uid(uid, 1'b0, 1'b0);
        data.set_status_field(uid, MEMBLOCK_STATUS_ENQ, 1'b1);
        data.set_status_field(uid, MEMBLOCK_STATUS_LSQ_DEQ, 1'b1);
        status = data.get_status(uid);
        if (status.active_lq_mapped || status.active_sq_mapped ||
            status.lsq_reservation_state != MEMBLOCK_LSQ_RESERVATION_NONE) begin
            `uvm_fatal("LSQ_CTRL", "atomic non-LSQ admission created an LSQ owner or reservation")
        end
    endfunction:commit_atomic_non_lsq_admission

    function void release_lq(input int unsigned count);
        if (count > (MEMBLOCK_LQ_SIZE - lq_free_count)) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("release_lq count=%0d exceeds allocated count=%0d",
                                             count, MEMBLOCK_LQ_SIZE - lq_free_count))
        end
        lq_deq_ptr = advance_lq_key(lq_deq_ptr, count);
        lq_free_count += count;
    endfunction:release_lq

    function void release_sq(input int unsigned count);
        if (count > (MEMBLOCK_SQ_SIZE - sq_free_count)) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("release_sq count=%0d exceeds allocated count=%0d",
                                             count, MEMBLOCK_SQ_SIZE - sq_free_count))
        end
        sq_deq_ptr = advance_sq_key(sq_deq_ptr, count);
        sq_free_count += count;
    endfunction:release_sq

    function void cancel_lq(input int unsigned count);
        if (count > (MEMBLOCK_LQ_SIZE - lq_free_count)) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("cancel_lq count=%0d exceeds allocated count=%0d",
                                             count, MEMBLOCK_LQ_SIZE - lq_free_count))
        end
        lq_enq_ptr = rewind_lq_key(lq_enq_ptr, count);
        lq_free_count += count;
    endfunction:cancel_lq

    function void cancel_sq(input int unsigned count);
        if (count > (MEMBLOCK_SQ_SIZE - sq_free_count)) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("cancel_sq count=%0d exceeds allocated count=%0d",
                                             count, MEMBLOCK_SQ_SIZE - sq_free_count))
        end
        sq_enq_ptr = rewind_sq_key(sq_enq_ptr, count);
        sq_free_count += count;
    endfunction:cancel_sq

endclass:lsq_ctrl_model

`endif
