//=========================================================
//File name    : exception_redirect_replay_handler.sv
//Author       : OpenAI_Codex
//Module name  : exception_redirect_replay_handler
//Discribution : dispatch exception/replay/redirect recovery helper
//Date         : 2026-05-18
//=========================================================
`ifndef EXCEPTION_REDIRECT_REPLAY_HANDLER__SV
`define EXCEPTION_REDIRECT_REPLAY_HANDLER__SV

class exception_redirect_replay_handler extends uvm_object;

    common_data_transaction data;

    `uvm_object_utils(exception_redirect_replay_handler)

    function new(string name = "exception_redirect_replay_handler");
        super.new(name);
        data = common_data_transaction::get();
    endfunction:new

    function void ensure_data();
        if (data == null) begin
            data = common_data_transaction::get();
        end
    endfunction:ensure_data

    function bit event_is_redirect(input memblock_wb_event_t wb_event);
        if (wb_event.redirect_valid !== wb_event.redirect.valid) begin
            `uvm_fatal("EXC_REDIRECT", $sformatf("redirect valid mismatch: redirect_valid=%0b redirect.valid=%0b source=%0d",
                                                 wb_event.redirect_valid, wb_event.redirect.valid, wb_event.source))
        end
        return wb_event.redirect.valid;
    endfunction:event_is_redirect

    function bit event_is_replay(input memblock_wb_event_t wb_event);
        return wb_event.replay_valid;
    endfunction:event_is_replay

    function bit event_is_fault(input memblock_wb_event_t wb_event);
        return wb_event.has_exception || wb_event.exception_vec != '0;
    endfunction:event_is_fault

    function bit event_should_wait_ptw(input memblock_wb_event_t wb_event);
        if (!seq_csr_common::is_initialized() ||
            !seq_csr_common::get_replay_wait_ptw_en()) begin
            return 1'b0;
        end
        return wb_event.ptw_back_replay;
    endfunction:event_should_wait_ptw

    function memblock_redirect_payload_t redirect_from_event(input memblock_wb_event_t wb_event);
        memblock_redirect_payload_t redirect;

        if (wb_event.redirect.valid) begin
            return wb_event.redirect;
        end
        `uvm_fatal("EXC_REDIRECT", "redirect wb_event requires redirect.valid payload")
        redirect = '{default:'0};
        return redirect;
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

    function bit queue_has_redirect();
        foreach (data.exception_event_q[idx]) begin
            if (event_is_redirect(data.exception_event_q[idx])) begin
                return 1'b1;
            end
        end
        return 1'b0;
    endfunction:queue_has_redirect

    function void handle_replay_event(input memblock_wb_event_t wb_event);
        memblock_uid_t uid;
        int unsigned   issue_epoch;
        int unsigned   replay_seq;

        if (!data.resolve_uid_for_event(wb_event, uid)) begin
            `uvm_warning("EXC_REDIRECT", "drop replay wb_event because active uid lookup failed")
            return;
        end
        issue_epoch = data.get_event_issue_epoch(wb_event, uid);
        replay_seq  = data.get_event_replay_seq(wb_event, uid);
        if (event_should_wait_ptw(wb_event)) begin
            data.push_ptw_wait_replay(uid,
                                      wb_event.target,
                                      issue_epoch,
                                      replay_seq,
                                      memblock_sync_pkg::get_dispatch_service_cycle());
            return;
        end
        void'(data.mark_replay_pending(uid, wb_event.target, issue_epoch, replay_seq, wb_event.cycle));
    endfunction:handle_replay_event

    function void handle_fault_event(input memblock_wb_event_t wb_event);
        memblock_uid_t uid;
        int unsigned   issue_epoch;
        int unsigned   replay_seq;

        // 中文注释：fault target 状态已经由 writeback_status_handler 首次采到 fault 时
        // 调用 mark_target_fault() 落表。本函数只消费 recovery 队列中的 fault event，
        // 保留 uid/epoch/replay_seq 解析用于过滤失效事件和调试，不再重复写 target fault 状态。
        if (!data.resolve_uid_for_event(wb_event, uid)) begin
            `uvm_warning("EXC_REDIRECT", "drop fault wb_event because active uid lookup failed")
            return;
        end
        issue_epoch = data.get_event_issue_epoch(wb_event, uid);
        replay_seq  = data.get_event_replay_seq(wb_event, uid);
        `uvm_info("EXC_REDIRECT",
                  $sformatf("consume fault recovery event uid=%0d target=%0d issue_epoch=%0d replay_seq=%0d",
                            uid, wb_event.target, issue_epoch, replay_seq),
                  UVM_HIGH)
    endfunction:handle_fault_event

    function void service_ptw_wait_replay();
        memblock_ptw_wait_replay_t wait_item;
        bit timed_out;

        ensure_data();
        if (data.active_redirect.valid) begin
            // active redirect期间route/issue整体冻结，PTW wait replay保持等待，避免先置replay_pending再被flush清掉。
            return;
        end
        while (data.pop_ready_ptw_wait_replay(seq_csr_common::get_replay_wait_ptw_timeout(),
                                              wait_item,
                                              timed_out)) begin
            if (timed_out) begin
                `uvm_warning("EXC_REDIRECT",
                             $sformatf("release ptw_wait_replay by timeout uid=%0d target=%0d replay_seq=%0d",
                                       wait_item.uid,
                                       wait_item.target,
                                       wait_item.replay_seq))
            end
            void'(data.mark_replay_pending(wait_item.uid,
                                           wait_item.target,
                                           wait_item.issue_epoch,
                                           wait_item.replay_seq,
                                           memblock_sync_pkg::get_dispatch_service_cycle()));
        end
    endfunction:service_ptw_wait_replay

    function void requeue_events_not_flushed_by_redirect(input memblock_wb_event_t events[$],
                                                         input memblock_redirect_payload_t redirect);
        for (int idx = events.size(); idx > 0; idx--) begin
            memblock_wb_event_t wb_item;

            wb_item = events[idx - 1];
            if (event_is_redirect(wb_item)) begin
                if (wb_item.has_rob &&
                    wb_item.rob_key.flag == redirect.rob_key.flag &&
                    wb_item.rob_key.value == redirect.rob_key.value) begin
                    continue;
                end
                if (wb_item.has_rob &&
                    rob_order_util::rob_need_flush(wb_item.rob_key, redirect)) begin
                    `uvm_info("EXC_REDIRECT",
                              $sformatf("drop pending redirect covered by active redirect uid=%0d rob=%0d/%0d",
                                        wb_item.uid,
                                        wb_item.rob_key.flag,
                                        wb_item.rob_key.value),
                              UVM_LOW)
                    continue;
                end
                // 中文注释：recovery handler 只保留跨 batch/drive 层的 redirect 单飞保护。
                // 未被当前 active redirect 覆盖的其它 redirect 不能参与 writeback 状态仲裁，
                // 但需要留在 recovery queue，等当前 redirect drive/flush 完成后再按 oldest 规则处理。
                data.exception_event_q.push_front(wb_item);
                continue;
            end
            if (wb_item.has_rob &&
                rob_order_util::rob_need_flush(wb_item.rob_key, redirect)) begin
                `uvm_info("EXC_REDIRECT",
                          $sformatf("drop stale feedback covered by redirect uid=%0d source=%0d rob=%0d/%0d",
                                    wb_item.uid,
                                    wb_item.source,
                                    wb_item.rob_key.flag,
                                    wb_item.rob_key.value),
                          UVM_LOW)
                continue;
            end
            data.exception_event_q.push_front(wb_item);
        end
    endfunction:requeue_events_not_flushed_by_redirect

    function void advance_active_redirect();
        memblock_redirect_payload_t redirect;

        if (!data.active_redirect.valid) begin
            return;
        end
        redirect = data.active_redirect;
        if (data.redirect_drive_done_for(redirect)) begin
            data.apply_redirect_flush(redirect);
        end else if (seq_csr_common::is_initialized() &&
                     seq_csr_common::get_redirect_freeze_timeout() != 0 &&
                     (memblock_sync_pkg::get_dispatch_service_cycle() - data.redirect_freeze_cycle) >=
                         seq_csr_common::get_redirect_freeze_timeout()) begin
            `uvm_warning("EXC_REDIRECT",
                         $sformatf("timeout waiting redirect drive rob=%0d/%0d; keep waiting and let unified no-progress/global-stop flow decide test exit",
                                   redirect.rob_key.flag,
                                   redirect.rob_key.value))
        end
    endfunction:advance_active_redirect

    task process_pending_events();
        memblock_wb_event_t wb_event;
        memblock_wb_event_t events[$];
        memblock_wb_event_t redirect_event;
        memblock_redirect_payload_t redirect;

        ensure_data();
        service_ptw_wait_replay();
        advance_active_redirect();
        if (data.active_redirect.valid) begin
            return;
        end

        while (data.pop_feedback_event(wb_event)) begin
            events.push_back(wb_event);
        end

        if (select_oldest_redirect(events, redirect_event)) begin
            redirect = redirect_from_event(redirect_event);
            if (seq_csr_common::is_initialized() &&
                !seq_csr_common::get_redirect_seq_en()) begin
                `uvm_fatal("EXC_REDIRECT",
                           "redirect event requires MEMBLOCK_REDIRECT_SEQ_EN=1 so recovery payload can be driven")
            end
            if (!data.active_redirect.valid) begin
                data.request_redirect_flush(redirect);
                data.push_redirect_drive(redirect);
            end
        end

        if (data.active_redirect.valid) begin
            requeue_events_not_flushed_by_redirect(events, data.active_redirect);
            return;
        end

        foreach (events[idx]) begin
            if (event_is_redirect(events[idx])) begin
                continue;
            end
            if (event_is_replay(events[idx])) begin
                handle_replay_event(events[idx]);
            end else if (event_is_fault(events[idx])) begin
                handle_fault_event(events[idx]);
            end
        end
    endtask:process_pending_events

endclass:exception_redirect_replay_handler

`endif
