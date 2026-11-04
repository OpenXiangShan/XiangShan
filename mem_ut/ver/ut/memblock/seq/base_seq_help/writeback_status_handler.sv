//=========================================================
//File name    : writeback_status_handler.sv
//Author       : OpenAI_Codex
//Module name  : writeback_status_handler
//Discribution : dispatch writeback/replay wb_event status updater
//Date         : 2026-05-18
//=========================================================
`ifndef WRITEBACK_STATUS_HANDLER__SV
`define WRITEBACK_STATUS_HANDLER__SV

class writeback_status_handler extends uvm_object;

    common_data_transaction data;

    `uvm_object_utils(writeback_status_handler)

    function new(string name = "writeback_status_handler");
        super.new(name);
        data = common_data_transaction::get();
    endfunction:new

    function void ensure_data();
        if (data == null) begin
            data = common_data_transaction::get();
        end
    endfunction:ensure_data

    function bit event_has_fault(input memblock_wb_event_t wb_event);
        return wb_event.has_exception || (wb_event.exception_vec != '0);
    endfunction:event_has_fault

    function bit event_is_redirect(input memblock_wb_event_t wb_event);
        if (wb_event.redirect_valid !== wb_event.redirect.valid) begin
            `uvm_fatal("WB_STATUS", $sformatf("redirect valid mismatch: redirect_valid=%0b redirect.valid=%0b source=%0d",
                                              wb_event.redirect_valid, wb_event.redirect.valid, wb_event.source))
        end
        return wb_event.redirect.valid;
    endfunction:event_is_redirect

    function bit event_is_replay(input memblock_wb_event_t wb_event);
        return wb_event.replay_valid;
    endfunction:event_is_replay

    function bit event_is_real_writeback(input memblock_wb_event_t wb_event);
        return wb_event.real_wb_valid;
    endfunction:event_is_real_writeback

    function bit event_is_issue_feedback(input memblock_wb_event_t wb_event);
        return wb_event.iq_feedback_valid;
    endfunction:event_is_issue_feedback

    function bit event_is_normal_pass(input memblock_wb_event_t wb_event);
        return event_is_real_writeback(wb_event) &&
               !event_is_redirect(wb_event) &&
               !event_is_replay(wb_event) &&
               !event_has_fault(wb_event);
    endfunction:event_is_normal_pass

    function bit target_real_wb_pass_enabled(input memblock_issue_target_e target);
        if (!seq_csr_common::is_initialized()) begin
            return 1'b0;
        end
        // STA 的兼容开关由后续 IQ 专项继续拥有；STD 不再有第二个完成 owner。
        return target == MEMBLOCK_ISSUE_TARGET_STA && seq_csr_common::get_sta_real_wb_pass_en();
    endfunction:target_real_wb_pass_enabled

    function void validate_event_target(input memblock_wb_event_t wb_event,
                                        input string caller);
        if (wb_event.target != MEMBLOCK_ISSUE_TARGET_LOAD &&
            wb_event.target != MEMBLOCK_ISSUE_TARGET_STA &&
            wb_event.target != MEMBLOCK_ISSUE_TARGET_STD) begin
            `uvm_fatal("WB_STATUS", $sformatf("%s got unsupported target=%0d", caller, wb_event.target))
        end
    endfunction:validate_event_target

    function bit handle_real_writeback_event(input memblock_wb_event_t wb_event);
        memblock_uid_t   uid;
        int unsigned     issue_epoch;
        int unsigned     replay_seq;
        status_transaction status;

        if (!event_is_real_writeback(wb_event) && !event_has_fault(wb_event)) begin
            return 1'b0;
        end
        uid = wb_event.uid;
        issue_epoch = wb_event.issue_epoch;
        replay_seq = wb_event.replay_seq;
        status = data.get_status(uid);
        if (event_has_fault(wb_event)) begin
            `uvm_info("WB_STATUS",
                      $sformatf("fault feedback uid=%0d target=%0d port=%0d rob_valid=%0d rob=%0d/%0d lq_valid=%0d lq=%0d/%0d exception_vec=0x%0h",
                                uid,
                                wb_event.target,
                                wb_event.port_id,
                                wb_event.has_rob,
                                wb_event.rob_key.flag,
                                wb_event.rob_key.value,
                                wb_event.has_lq,
                                wb_event.lq_key.flag,
                                wb_event.lq_key.value,
                                wb_event.exception_vec),
                      UVM_LOW)
            if (!data.mark_target_fault(uid, wb_event.target, issue_epoch, replay_seq, wb_event.exception_vec, wb_event.cycle)) begin
                `uvm_info("WB_STATUS", $sformatf("drop stale fault uid=%0d target=%0d", uid, wb_event.target), UVM_LOW)
                return 1'b0;
            end
            data.push_feedback_event(wb_event);
            return 1'b1;
        end
        // 中文注释：normal STA raw 必须仍有 current IQ success；late raw 只有 fault
        // 才能在 adapter 中借 tombstone 还原，不能放宽正常 writeback 的归属。
        if (wb_event.target == MEMBLOCK_ISSUE_TARGET_STA &&
            target_real_wb_pass_enabled(MEMBLOCK_ISSUE_TARGET_STA) &&
            !status.sta_issue_feedback_success) begin
            `uvm_fatal("WB_STATUS_STA_ORDER",
                       $sformatf("STA real writeback arrived before IQ hit uid=%0d issue_epoch=%0d replay_seq=%0d",
                                 uid, issue_epoch, replay_seq))
        end
        if (event_is_normal_pass(wb_event)) begin
            if (!data.mark_target_normal_pass(uid, wb_event.target, issue_epoch, replay_seq, wb_event.cycle)) begin
                `uvm_info("WB_STATUS", $sformatf("drop stale pass uid=%0d target=%0d", uid, wb_event.target), UVM_LOW)
                return 1'b0;
            end
            // 中文注释：pending STA/STD fault 的 target normal WB 只完成对应 real-WB
            // 记录；UID 级异常仍由 batch finalize 在双 real-WB 屏障后统一提交。
            void'(data.finalize_pending_store_fault(uid));
            `uvm_info("WB_STATUS",
                      $sformatf("normal pass uid=%0d target=%0d source=%0d port=%0d rob=%0d/%0d lq_valid=%0d lq=%0d/%0d sq_valid=%0d sq=%0d/%0d",
                                uid,
                                wb_event.target,
                                wb_event.source,
                                wb_event.port_id,
                                wb_event.rob_key.flag,
                                wb_event.rob_key.value,
                                wb_event.has_lq,
                                wb_event.lq_key.flag,
                                wb_event.lq_key.value,
                                wb_event.has_sq,
                                wb_event.sq_key.flag,
                                wb_event.sq_key.value),
                      UVM_LOW)
            return 1'b1;
        end
        return 1'b0;
    endfunction:handle_real_writeback_event

    function bit handle_atomic_final_writeback_event(input memblock_wb_event_t wb_event);
        status_transaction status;

        if (wb_event.source != MEMBLOCK_WB_EVENT_SOURCE_ATOMIC_WB ||
            !event_is_real_writeback(wb_event)) begin
            return 1'b0;
        end
        if (!data.is_atomic_uid(wb_event.uid) || wb_event.has_lq || wb_event.has_sq ||
            wb_event.target != MEMBLOCK_ISSUE_TARGET_STA) begin
            `uvm_fatal("ATOMIC_WB",
                       $sformatf("invalid atomic final WB uid=%0d target=%0d lq/sq=%0d/%0d",
                                 wb_event.uid, wb_event.target,
                                 wb_event.has_lq, wb_event.has_sq))
        end
        status = data.get_status(wb_event.uid);
        if (wb_event.has_dynamic_epoch &&
            wb_event.dynamic_epoch != status.dynamic_epoch) begin
            `uvm_info("ATOMIC_WB",
                      $sformatf("drop stale atomic final WB uid=%0d event/status epoch=%0d/%0d",
                                wb_event.uid, wb_event.dynamic_epoch,
                                status.dynamic_epoch), UVM_LOW)
            return 1'b0;
        end
        if (!data.record_atomic_final_wb(wb_event.uid,
                                         wb_event.issue_epoch,
                                         wb_event.exception_vec,
                                         wb_event.cycle)) begin
            `uvm_info("ATOMIC_WB",
                      $sformatf("drop stale atomic final WB uid=%0d issue_epoch=%0d",
                                wb_event.uid, wb_event.issue_epoch), UVM_LOW)
            return 1'b0;
        end
        status = data.get_status(wb_event.uid);
        `uvm_info("ATOMIC_WB",
                  $sformatf("atomic final WB uid=%0d count=%0d/%0d fault_pending=%0d finalized=%0d pieces=%0h/%0h:%0h/%0h",
                            wb_event.uid,
                            status.atomic_final_wb_seen_count,
                            status.atomic_final_wb_expected_count,
                            status.atomic_pending_fault,
                            status.atomic_finalized,
                            status.atomic_sta_fired_mask,
                            status.atomic_required_sta_mask,
                            status.atomic_std_fired_mask,
                            status.atomic_required_std_mask), UVM_LOW)
        return 1'b1;
    endfunction:handle_atomic_final_writeback_event

    function bit handle_atomic_feedback_event(input memblock_wb_event_t wb_event);
        status_transaction status;

        if (!event_is_issue_feedback(wb_event) ||
            !data.is_atomic_uid(wb_event.uid)) begin
            return 1'b0;
        end
        if (wb_event.source != MEMBLOCK_WB_EVENT_SOURCE_STA_FEEDBACK ||
            wb_event.target != MEMBLOCK_ISSUE_TARGET_STA ||
            wb_event.has_lq || wb_event.has_sq || !wb_event.iq_feedback_hit ||
            wb_event.iq_feedback_failed) begin
            `uvm_fatal("ATOMIC_FEEDBACK",
                       $sformatf("invalid atomic STA feedback uid=%0d source=%0d target=%0d lq/sq=%0d/%0d hit/failed=%0d/%0d",
                                 wb_event.uid, wb_event.source, wb_event.target,
                                 wb_event.has_lq, wb_event.has_sq,
                                 wb_event.iq_feedback_hit,
                                 wb_event.iq_feedback_failed))
        end
        status = data.get_status(wb_event.uid);
        if (wb_event.has_dynamic_epoch &&
            wb_event.dynamic_epoch != status.dynamic_epoch) begin
            return 1'b0;
        end
        if (!data.record_atomic_feedback(wb_event.uid,
                                         wb_event.uop_index,
                                         wb_event.issue_epoch,
                                         wb_event.cycle)) begin
            return 1'b0;
        end
        `uvm_info("ATOMIC_FEEDBACK",
                  $sformatf("consume atomic STA feedback uid=%0d slot=%0d count=%0d/%0d",
                            wb_event.uid, wb_event.uop_index,
                            status.atomic_feedback_seen_count,
                            status.atomic_feedback_expected_count), UVM_LOW)
        return 1'b1;
    endfunction:handle_atomic_feedback_event

    function bit handle_issue_feedback_event(input memblock_wb_event_t wb_event);
        memblock_uid_t   uid;
        int unsigned     issue_epoch;
        int unsigned     replay_seq;
        bit              tombstone_created;

        if (!event_is_issue_feedback(wb_event)) begin
            return 1'b0;
        end
        if (data.is_atomic_uid(wb_event.uid)) begin
            return handle_atomic_feedback_event(wb_event);
        end
        if (wb_event.target == MEMBLOCK_ISSUE_TARGET_STD ||
            wb_event.source == MEMBLOCK_WB_EVENT_SOURCE_STD_FEEDBACK) begin
            `uvm_fatal("WB_STATUS",
                       $sformatf("STD issue feedback cannot complete target; source=%0d hit=%0d failed=%0d uid=%0d",
                                 wb_event.source,
                                 wb_event.iq_feedback_hit,
                                 wb_event.iq_feedback_failed,
                                 wb_event.uid))
        end
        uid = wb_event.uid;
        issue_epoch = wb_event.issue_epoch;
        replay_seq = wb_event.replay_seq;
        if (wb_event.iq_feedback_failed) begin
            if (wb_event.target == MEMBLOCK_ISSUE_TARGET_STD) begin
                `uvm_warning("WB_STATUS",
                             $sformatf("drop STD issue feedback failed uid=%0d: no backend STD replay path", uid))
                return 1'b0;
            end
            if (wb_event.target == MEMBLOCK_ISSUE_TARGET_STA) begin
                if (!data.capture_sta_late_fault_tombstone(uid,
                                                           issue_epoch,
                                                           replay_seq,
                                                           wb_event.cycle,
                                                           tombstone_created)) begin
                    `uvm_info("WB_STATUS",
                              $sformatf("drop stale STA replay feedback uid=%0d issue_epoch=%0d replay_seq=%0d",
                                        uid, issue_epoch, replay_seq),
                              UVM_LOW)
                    return 1'b0;
                end
                if (!tombstone_created) begin
                    `uvm_info("WB_STATUS",
                              $sformatf("coalesced STA late-fault tombstone uid=%0d issue_epoch=%0d replay_seq=%0d; enqueue current replay with retained tombstone",
                                        uid, issue_epoch, replay_seq), UVM_LOW)
                end else begin
                    `uvm_info("WB_STATUS",
                              $sformatf("capture STA late-fault tombstone uid=%0d issue_epoch=%0d replay_seq=%0d; enqueue replay immediately",
                                        uid, issue_epoch, replay_seq),
                              UVM_LOW)
                end
                data.push_feedback_event(wb_event);
                return 1'b1;
            end
            data.push_feedback_event(wb_event);
            return 1'b1;
        end
        if (wb_event.iq_feedback_hit) begin
            if (target_real_wb_pass_enabled(wb_event.target)) begin
                if (!data.mark_issue_feedback_success(uid, wb_event.target, issue_epoch, replay_seq, wb_event.cycle)) begin
                    `uvm_info("WB_STATUS",
                              $sformatf("drop stale issue feedback success uid=%0d target=%0d", uid, wb_event.target),
                              UVM_LOW)
                    return 1'b0;
                end
                `uvm_info("WB_STATUS",
                          $sformatf("issue feedback success uid=%0d target=%0d source=%0d wait real writeback",
                                    uid, wb_event.target, wb_event.source),
                          UVM_LOW)
                return 1'b1;
            end
            if (!data.mark_target_normal_pass(uid, wb_event.target, issue_epoch, replay_seq, wb_event.cycle)) begin
                `uvm_info("WB_STATUS",
                          $sformatf("drop stale compatible issue feedback pass uid=%0d target=%0d", uid, wb_event.target),
                          UVM_LOW)
                return 1'b0;
            end
            `uvm_info("WB_STATUS",
                      $sformatf("compatible issue feedback pass uid=%0d target=%0d source=%0d",
                                uid, wb_event.target, wb_event.source),
                      UVM_LOW)
            return 1'b1;
        end
        return 1'b0;
    endfunction:handle_issue_feedback_event

    function bit handle_event(input memblock_wb_event_t wb_event);
        ensure_data();
        if (!wb_event.valid) begin
            return 1'b0;
        end
        if (wb_event.vector_ls) begin
            `uvm_fatal("WB_STATUS", "UNSUPPORTED_VECTOR_LS_WB")
        end
        if (!wb_event.has_uid || !wb_event.has_rob || !wb_event.has_replay_seq) begin
            `uvm_warning("WB_STATUS", "drop unnormalized feedback wb_event; monitor events must pass dispatch_monitor_batch_handler")
            return 1'b0;
        end
        if (!event_is_redirect(wb_event)) begin
            validate_event_target(wb_event, "handle_event");
            if (!wb_event.has_issue_epoch) begin
                `uvm_warning("WB_STATUS", "drop unnormalized non-redirect wb_event without issue_epoch")
                return 1'b0;
            end
        end
        if (event_is_redirect(wb_event)) begin
            `uvm_warning("WB_STATUS", "drop redirect event: monitor redirect must enter dispatch_monitor_batch_handler")
            return 1'b0;
        end
        case (wb_event.source)
            MEMBLOCK_WB_EVENT_SOURCE_LOAD_WB,
            MEMBLOCK_WB_EVENT_SOURCE_STORE_WB: begin
                return handle_real_writeback_event(wb_event);
            end
            MEMBLOCK_WB_EVENT_SOURCE_ATOMIC_WB: begin
                return handle_atomic_final_writeback_event(wb_event);
            end
            MEMBLOCK_WB_EVENT_SOURCE_STA_FEEDBACK: begin
                return handle_issue_feedback_event(wb_event);
            end
            MEMBLOCK_WB_EVENT_SOURCE_STD_FEEDBACK: begin
                `uvm_fatal("WB_STATUS", "STD_FEEDBACK is not a completion source in strict V2 int-WB mode")
            end
            default: begin
                if (event_is_replay(wb_event)) begin
                    data.push_feedback_event(wb_event);
                    return 1'b1;
                end
            end
        endcase
        `uvm_info("WB_STATUS", $sformatf("drop unclassified feedback wb_event source=%0d", wb_event.source), UVM_HIGH)
        return 1'b0;
    endfunction:handle_event

endclass:writeback_status_handler

`endif
