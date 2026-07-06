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
        case (target)
            MEMBLOCK_ISSUE_TARGET_STA: return seq_csr_common::get_sta_real_wb_pass_en();
            MEMBLOCK_ISSUE_TARGET_STD: return seq_csr_common::get_std_real_wb_pass_en();
            default: return 1'b0;
        endcase
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

        if (!event_is_real_writeback(wb_event) && !event_has_fault(wb_event)) begin
            return 1'b0;
        end
        uid = wb_event.uid;
        issue_epoch = wb_event.issue_epoch;
        replay_seq = wb_event.replay_seq;
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
        if (event_is_normal_pass(wb_event)) begin
            if (!data.mark_target_normal_pass(uid, wb_event.target, issue_epoch, replay_seq, wb_event.cycle)) begin
                `uvm_info("WB_STATUS", $sformatf("drop stale pass uid=%0d target=%0d", uid, wb_event.target), UVM_LOW)
                return 1'b0;
            end
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

    function bit handle_issue_feedback_event(input memblock_wb_event_t wb_event);
        memblock_uid_t   uid;
        int unsigned     issue_epoch;
        int unsigned     replay_seq;

        if (!event_is_issue_feedback(wb_event)) begin
            return 1'b0;
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
            MEMBLOCK_WB_EVENT_SOURCE_STA_FEEDBACK,
            MEMBLOCK_WB_EVENT_SOURCE_STD_FEEDBACK: begin
                return handle_issue_feedback_event(wb_event);
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
