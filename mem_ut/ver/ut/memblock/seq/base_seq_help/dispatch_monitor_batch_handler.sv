//=========================================================
//File name    : dispatch_monitor_batch_handler.sv
//Author       : OpenAI_Codex
//Module name  : dispatch_monitor_batch_handler
//Discribution : monitor semantic event batch redirect-first arbiter
//Date         : 2026-06-10
//=========================================================
`ifndef DISPATCH_MONITOR_BATCH_HANDLER__SV
`define DISPATCH_MONITOR_BATCH_HANDLER__SV

class dispatch_monitor_batch_handler extends uvm_object;

    common_data_transaction data;
    writeback_status_handler writeback_handler;

    `uvm_object_utils(dispatch_monitor_batch_handler)

    function new(string name = "dispatch_monitor_batch_handler");
        super.new(name);
        data = common_data_transaction::get();
        writeback_handler = null;
    endfunction:new

    function void bind_writeback_handler(input writeback_status_handler handler);
        writeback_handler = handler;
    endfunction:bind_writeback_handler

    function void ensure_handles();
        if (data == null) begin
            data = common_data_transaction::get();
        end
        if (writeback_handler == null) begin
            writeback_handler = writeback_status_handler::type_id::create("batch_writeback_handler");
        end
    endfunction:ensure_handles

    function bit event_is_redirect(input memblock_wb_event_t wb_event);
        if (wb_event.redirect_valid !== wb_event.redirect.valid) begin
            `uvm_fatal("DISP_MON_BATCH",
                       $sformatf("redirect valid mismatch: redirect_valid=%0b redirect.valid=%0b source=%0d",
                                 wb_event.redirect_valid,
                                 wb_event.redirect.valid,
                                 wb_event.source))
        end
        return wb_event.redirect.valid;
    endfunction:event_is_redirect

    function bit event_is_replay(input memblock_wb_event_t wb_event);
        return wb_event.replay_valid;
    endfunction:event_is_replay

    function bit event_has_fault(input memblock_wb_event_t wb_event);
        return wb_event.has_exception || wb_event.exception_vec != '0;
    endfunction:event_has_fault

    function memblock_redirect_payload_t redirect_from_event(input memblock_wb_event_t wb_event);
        if (!event_is_redirect(wb_event)) begin
            `uvm_fatal("DISP_MON_BATCH", "redirect_from_event requires redirect event")
        end
        return wb_event.redirect;
    endfunction:redirect_from_event

    function bit redirect_event_is_older(input memblock_wb_event_t candidate,
                                         input memblock_wb_event_t best);
        memblock_redirect_payload_t cand_redirect;
        memblock_redirect_payload_t best_redirect;

        cand_redirect = redirect_from_event(candidate);
        best_redirect = redirect_from_event(best);
        if (cand_redirect.rob_key.flag == best_redirect.rob_key.flag &&
            cand_redirect.rob_key.value == best_redirect.rob_key.value) begin
            return candidate.port_id < best.port_id;
        end
        return rob_order_util::rob_is_after(best_redirect.rob_key, cand_redirect.rob_key);
    endfunction:redirect_event_is_older

    function bit select_oldest_redirect(input memblock_wb_event_t events[$],
                                        output memblock_wb_event_t selected);
        bit found;

        ensure_handles();
        found = 1'b0;
        selected = data.make_empty_wb_event();
        foreach (events[idx]) begin
            if (!event_is_redirect(events[idx])) begin
                continue;
            end
            if (!found || redirect_event_is_older(events[idx], selected)) begin
                selected = events[idx];
                found = 1'b1;
            end
        end
        return found;
    endfunction:select_oldest_redirect

    function bit event_covered_by_redirect(input memblock_wb_event_t wb_event,
                                           input memblock_redirect_payload_t redirect);
        if (!wb_event.valid || !wb_event.has_rob) begin
            return 1'b0;
        end
        return rob_order_util::rob_need_flush(wb_event.rob_key, redirect);
    endfunction:event_covered_by_redirect

    function bit same_redirect_event(input memblock_wb_event_t lhs,
                                     input memblock_wb_event_t rhs);
        if (!event_is_redirect(lhs) || !event_is_redirect(rhs)) begin
            return 1'b0;
        end
        return lhs.source == rhs.source &&
               lhs.port_id == rhs.port_id &&
               lhs.rob_key.flag == rhs.rob_key.flag &&
               lhs.rob_key.value == rhs.rob_key.value &&
               lhs.redirect.flush_itself == rhs.redirect.flush_itself &&
               lhs.redirect.level == rhs.redirect.level;
    endfunction:same_redirect_event

    function bit normalize_event_batch(input memblock_wb_event_t events[$],
                                       ref memblock_wb_event_t normalized_events[$]);
        memblock_wb_event_t normalized_event;

        ensure_handles();
        normalized_events.delete();
        foreach (events[idx]) begin
            if (!events[idx].valid) begin
                continue;
            end
            if (!data.normalize_feedback_event(events[idx], normalized_event)) begin
                `uvm_warning("DISP_MON_BATCH",
                             $sformatf("drop monitor event source=%0d target=%0d rob_valid=%0d rob=%0d/%0d because uid/rob_key cannot be resolved",
                                       events[idx].source,
                                       events[idx].target,
                                       events[idx].has_rob,
                                       events[idx].rob_key.flag,
                                       events[idx].rob_key.value))
                continue;
            end
            normalized_events.push_back(normalized_event);
        end
        return normalized_events.size() != 0;
    endfunction:normalize_event_batch

    function bit process_allowed_non_redirect_event(input memblock_wb_event_t wb_event);
        if (event_is_redirect(wb_event)) begin
            `uvm_fatal("DISP_MON_BATCH", "process_allowed_non_redirect_event got redirect event")
        end
        case (wb_event.source)
            MEMBLOCK_WB_EVENT_SOURCE_LOAD_WB,
            MEMBLOCK_WB_EVENT_SOURCE_STORE_WB: begin
                return writeback_handler.handle_real_writeback_event(wb_event);
            end
            MEMBLOCK_WB_EVENT_SOURCE_STA_FEEDBACK,
            MEMBLOCK_WB_EVENT_SOURCE_STD_FEEDBACK: begin
                return writeback_handler.handle_issue_feedback_event(wb_event);
            end
            default: begin
                if (event_is_replay(wb_event) || event_has_fault(wb_event)) begin
                    data.push_feedback_event(wb_event);
                    return 1'b1;
                end
            end
        endcase
        `uvm_info("DISP_MON_BATCH",
                  $sformatf("drop unclassified allowed monitor event source=%0d target=%0d uid=%0d",
                            wb_event.source,
                            wb_event.target,
                            wb_event.uid),
                  UVM_HIGH)
        return 1'b0;
    endfunction:process_allowed_non_redirect_event

    task process_monitor_event_batch(input memblock_wb_event_t events[$]);
        memblock_wb_event_t normalized_events[$];
        memblock_wb_event_t selected_redirect_event;
        memblock_redirect_payload_t selected_redirect;

        ensure_handles();
        if (!normalize_event_batch(events, normalized_events)) begin
            return;
        end

        // 中文注释：active_redirect 是跨 service batch / redirect drive 层的保护。
        // 当前已有 redirect 尚未 drive/apply flush 时，batch handler 只过滤其覆盖的旧动态实例结果；
        // 不在 writeback handler 中再次参与 pass/fault/replay 仲裁。
        if (data.active_redirect.valid) begin
            foreach (normalized_events[idx]) begin
                if (event_covered_by_redirect(normalized_events[idx], data.active_redirect)) begin
                    `uvm_info("DISP_MON_BATCH",
                              $sformatf("drop event covered by active redirect uid=%0d source=%0d rob=%0d/%0d",
                                        normalized_events[idx].uid,
                                        normalized_events[idx].source,
                                        normalized_events[idx].rob_key.flag,
                                        normalized_events[idx].rob_key.value),
                              UVM_LOW)
                    continue;
                end
                if (event_is_redirect(normalized_events[idx])) begin
                    data.push_feedback_event(normalized_events[idx]);
                end else begin
                    void'(process_allowed_non_redirect_event(normalized_events[idx]));
                end
            end
            return;
        end

        if (select_oldest_redirect(normalized_events, selected_redirect_event)) begin
            selected_redirect = redirect_from_event(selected_redirect_event);
            data.push_feedback_event(selected_redirect_event);
            foreach (normalized_events[idx]) begin
                if (same_redirect_event(normalized_events[idx], selected_redirect_event)) begin
                    continue;
                end
                if (event_covered_by_redirect(normalized_events[idx], selected_redirect)) begin
                    `uvm_info("DISP_MON_BATCH",
                              $sformatf("drop same-batch event covered by oldest redirect uid=%0d source=%0d rob=%0d/%0d",
                                        normalized_events[idx].uid,
                                        normalized_events[idx].source,
                                        normalized_events[idx].rob_key.flag,
                                        normalized_events[idx].rob_key.value),
                              UVM_LOW)
                    continue;
                end
                if (event_is_redirect(normalized_events[idx])) begin
                    `uvm_info("DISP_MON_BATCH",
                              $sformatf("defer same-batch redirect not covered by selected redirect uid=%0d rob=%0d/%0d",
                                        normalized_events[idx].uid,
                                        normalized_events[idx].rob_key.flag,
                                        normalized_events[idx].rob_key.value),
                              UVM_LOW)
                    data.push_feedback_event(normalized_events[idx]);
                end else begin
                    void'(process_allowed_non_redirect_event(normalized_events[idx]));
                end
            end
            return;
        end

        foreach (normalized_events[idx]) begin
            void'(process_allowed_non_redirect_event(normalized_events[idx]));
        end
    endtask:process_monitor_event_batch

endclass:dispatch_monitor_batch_handler

`endif
