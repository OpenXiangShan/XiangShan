//=========================================================
//File name    : dispatch_monitor_event_adapter.sv
//Author       : OpenAI_Codex
//Module name  : dispatch_monitor_event_adapter
//Discribution : raw monitor fact adapter for dispatch framework
//Date         : 2026-05-19
//=========================================================
`ifndef DISPATCH_MONITOR_EVENT_ADAPTER__SV
`define DISPATCH_MONITOR_EVENT_ADAPTER__SV

class dispatch_monitor_event_adapter extends uvm_object;

    common_data_transaction data;
    lsq_commit_handler      monitor_commit_handler;

    `uvm_object_utils(dispatch_monitor_event_adapter)

    function new(string name = "dispatch_monitor_event_adapter");
        super.new(name);
        data = common_data_transaction::get();
        monitor_commit_handler = null;
    endfunction:new

    function void bind_commit_handler(input lsq_commit_handler handler);
        monitor_commit_handler = handler;
    endfunction:bind_commit_handler

    function void ensure_handles();
        if (data == null) begin
            data = common_data_transaction::get();
        end
        if (monitor_commit_handler == null) begin
            monitor_commit_handler = lsq_commit_handler::type_id::create("dispatch_monitor_lsq_commit_handler");
        end
    endfunction:ensure_handles

    function memblock_wb_event_t make_wb_event_base();
        ensure_handles();
        return data.make_empty_wb_event();
    endfunction:make_wb_event_base

    function bit raw_rob_to_key(input bit valid,
                                input bit flag,
                                input bit [8:0] value,
                                output memblock_rob_key_t key);
        key.flag  = flag;
        key.value = value;
        return valid;
    endfunction:raw_rob_to_key

    function bit raw_lq_to_key(input bit valid,
                               input bit flag,
                               input bit [6:0] value,
                               output memblock_lq_key_t key);
        key.flag  = flag;
        key.value = value;
        return valid;
    endfunction:raw_lq_to_key

    function bit raw_sq_to_key(input bit valid,
                               input bit flag,
                               input bit [5:0] value,
                               output memblock_sq_key_t key);
        key.flag  = flag;
        key.value = value;
        return valid;
    endfunction:raw_sq_to_key

    function bit event_has_active_uid(input memblock_wb_event_t wb_event);
        memblock_uid_t uid;

        ensure_handles();
        return data.resolve_uid_for_event(wb_event, uid);
    endfunction:event_has_active_uid

    function bit convert_raw_int_wb(input memblock_sync_pkg::dispatch_raw_int_wb_t raw,
                                    output memblock_wb_event_t wb_event);
        wb_event = make_wb_event_base();
        if (!raw.valid) begin
            return 1'b0;
        end
        wb_event.valid         = 1'b1;
        wb_event.port_id       = raw.port_id;
        wb_event.real_wb_valid = 1'b1;
        wb_event.has_exception = raw.exception_vec != '0;
        wb_event.exception_vec = raw.exception_vec;
        wb_event.has_rob       = raw_rob_to_key(raw.rob_valid, raw.rob_flag, raw.rob_value, wb_event.rob_key);
        case (raw.port_id)
            0, 1, 2: begin
                wb_event.source = MEMBLOCK_WB_EVENT_SOURCE_LOAD_WB;
                wb_event.target = MEMBLOCK_ISSUE_TARGET_LOAD;
                wb_event.has_lq = raw_lq_to_key(raw.lq_valid, raw.lq_flag, raw.lq_value, wb_event.lq_key);
                wb_event.has_sq = 1'b0;
            end
            3, 4: begin
                wb_event.source = MEMBLOCK_WB_EVENT_SOURCE_STORE_WB;
                wb_event.target = MEMBLOCK_ISSUE_TARGET_STA;
                wb_event.has_lq = 1'b0;
                wb_event.has_sq = raw_sq_to_key(raw.sq_valid, raw.sq_flag, raw.sq_value, wb_event.sq_key);
            end
            5, 6: begin
                wb_event.source = MEMBLOCK_WB_EVENT_SOURCE_STORE_WB;
                wb_event.target = MEMBLOCK_ISSUE_TARGET_STD;
                wb_event.has_lq = 1'b0;
                wb_event.has_sq = raw_sq_to_key(raw.sq_valid, raw.sq_flag, raw.sq_value, wb_event.sq_key);
            end
            default: begin
                `uvm_info("DISP_MON_ADAPT",
                          $sformatf("drop raw int wb with unsupported memblock port=%0d", raw.port_id),
                          UVM_LOW)
                return 1'b0;
            end
        endcase
        wb_event.cycle               = raw.cycle;
        return 1'b1;
    endfunction:convert_raw_int_wb

    function bit convert_raw_iq_feedback(input memblock_sync_pkg::dispatch_raw_iq_feedback_t raw,
                                         output memblock_wb_event_t wb_event);
        wb_event = make_wb_event_base();
        if (!raw.valid) begin
            return 1'b0;
        end
        if (raw.vector_feedback) begin
            `uvm_info("DISP_MON_ADAPT",
                      $sformatf("drop vector iq feedback port=%0d source_type=0x%0h", raw.port_id, raw.source_type),
                      UVM_LOW)
            return 1'b0;
        end
        if (!raw.is_sta && !raw.is_std) begin
            `uvm_info("DISP_MON_ADAPT", "drop iq feedback with unknown scalar target", UVM_LOW)
            return 1'b0;
        end

        wb_event.valid         = 1'b1;
        wb_event.port_id       = raw.port_id;
        if (raw.is_sta) begin
            wb_event.target = MEMBLOCK_ISSUE_TARGET_STA;
            wb_event.source = MEMBLOCK_WB_EVENT_SOURCE_STA_FEEDBACK;
        end else begin
            wb_event.target = MEMBLOCK_ISSUE_TARGET_STD;
            wb_event.source = MEMBLOCK_WB_EVENT_SOURCE_STD_FEEDBACK;
        end
        if (raw.is_std && !raw.hit) begin
            `uvm_warning("DISP_MON_ADAPT",
                         $sformatf("drop STD iq feedback miss port=%0d: MemBlock has no backend STD replay feedback path",
                                   raw.port_id))
            return 1'b0;
        end
        wb_event.has_rob       = raw_rob_to_key(raw.rob_valid, raw.rob_flag, raw.rob_value, wb_event.rob_key);
        if (raw.is_std) begin
            wb_event.has_lq = raw_lq_to_key(raw.lq_valid, raw.lq_flag, raw.lq_value, wb_event.lq_key);
        end else begin
            wb_event.has_lq = 1'b0;
        end
        wb_event.has_sq        = raw_sq_to_key(raw.sq_valid, raw.sq_flag, raw.sq_value, wb_event.sq_key);
        // 中文注释：IQ feedback 是 IssueQueue response，不是真实 ROB/RF writeback。
        // hit/finalSuccess 只写 iq_feedback_*；STA miss 额外生成 replay，STD miss 已在本函数 warning/drop。
        wb_event.iq_feedback_valid       = 1'b1;
        wb_event.iq_feedback_hit         = raw.hit;
        wb_event.iq_feedback_failed      = !raw.hit;
        wb_event.iq_feedback_flush_state = raw.flush_state;
        wb_event.replay_valid            = raw.is_sta && !raw.hit;
        wb_event.ptw_back_replay         = raw.is_sta && !raw.hit && raw.flush_state;
        wb_event.cycle         = raw.cycle;

        return 1'b1;
    endfunction:convert_raw_iq_feedback

    function bit convert_raw_memory_violation(input memblock_sync_pkg::dispatch_raw_ctrl_t raw,
                                             output memblock_wb_event_t wb_event);
        wb_event = make_wb_event_base();
        if (!raw.memory_violation_valid) begin
            return 1'b0;
        end
        wb_event.valid                  = 1'b1;
        wb_event.source                 = MEMBLOCK_WB_EVENT_SOURCE_MEMORY_VIOLATION;
        wb_event.target                 = MEMBLOCK_ISSUE_TARGET_NONE;
        wb_event.redirect_valid         = 1'b1;
        wb_event.redirect.valid         = 1'b1;
        wb_event.redirect.flush_itself  = raw.memory_violation_level;
        wb_event.redirect.level         = raw.memory_violation_level;
        wb_event.has_rob                = raw_rob_to_key(raw.memory_violation_rob_valid,
                                                         raw.memory_violation_rob_flag,
                                                         raw.memory_violation_rob_value,
                                                         wb_event.rob_key);
        wb_event.redirect.rob_key       = wb_event.rob_key;
        wb_event.cycle                  = raw.cycle;
        return 1'b1;
    endfunction:convert_raw_memory_violation

    function void apply_raw_ctrl_deq(input memblock_sync_pkg::dispatch_raw_ctrl_t raw);
        memblock_lq_key_t lq_ptr;
        memblock_sq_key_t sq_ptr;

        ensure_handles();
        data.update_sb_is_empty(raw.sb_is_empty);
        if (raw.lq_deq == 0 && raw.sq_deq == 0) begin
            return;
        end
        lq_ptr.flag  = raw.lq_deq_ptr_flag;
        lq_ptr.value = raw.lq_deq_ptr_value;
        sq_ptr.flag  = raw.sq_deq_ptr_flag;
        sq_ptr.value = raw.sq_deq_ptr_value;
        monitor_commit_handler.apply_raw_ctrl_deq(raw.lq_deq, lq_ptr, raw.sq_deq, sq_ptr);
    endfunction:apply_raw_ctrl_deq

    function void drain_csr_events();
        memblock_sync_pkg::dispatch_raw_csr_t raw_csr;
        int unsigned raw_csr_seq;

        ensure_handles();
        if (memblock_sync_pkg::get_latest_raw_csr(raw_csr, raw_csr_seq)) begin
            data.apply_raw_csr_runtime(raw_csr, raw_csr_seq);
        end
    endfunction:drain_csr_events

    function void drain_sfence_events();
        memblock_sync_pkg::dispatch_raw_sfence_t raw_sfence;

        ensure_handles();
        while (memblock_sync_pkg::pop_raw_sfence(raw_sfence)) begin
            void'(data.apply_raw_sfence(raw_sfence));
        end
    endfunction:drain_sfence_events

    task collect_writeback_events_batch(ref memblock_wb_event_t events[$]);
        memblock_sync_pkg::dispatch_raw_int_wb_t raw_int_wb;
        memblock_sync_pkg::dispatch_raw_iq_feedback_t raw_iq;
        memblock_wb_event_t wb_event;

        while (memblock_sync_pkg::pop_raw_int_wb(raw_int_wb)) begin
            if (convert_raw_int_wb(raw_int_wb, wb_event)) begin
                events.push_back(wb_event);
            end
        end
        while (memblock_sync_pkg::pop_raw_iq_feedback(raw_iq)) begin
            if (convert_raw_iq_feedback(raw_iq, wb_event)) begin
                events.push_back(wb_event);
            end
        end
    endtask:collect_writeback_events_batch

    task collect_ctrl_redirect_events_batch(ref memblock_wb_event_t events[$]);
        memblock_sync_pkg::dispatch_raw_ctrl_t raw_ctrl;
        memblock_wb_event_t wb_event;

        while (memblock_sync_pkg::pop_raw_ctrl(raw_ctrl)) begin
            apply_raw_ctrl_deq(raw_ctrl);
            if (convert_raw_memory_violation(raw_ctrl, wb_event)) begin
                events.push_back(wb_event);
            end
        end
    endtask:collect_ctrl_redirect_events_batch

endclass:dispatch_monitor_event_adapter

`endif
