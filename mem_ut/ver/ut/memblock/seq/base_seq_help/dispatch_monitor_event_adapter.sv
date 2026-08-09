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

    typedef struct {
        memblock_uid_t            uid;
        memblock_mmio_kind_e      kind;
        int unsigned              first_port_order;
    } memblock_mmio_tag_stage_t;

    common_data_transaction data;
    lsq_commit_handler      monitor_commit_handler;

    typedef struct {
        memblock_sync_pkg::dispatch_raw_sfence_t raw;
        longint unsigned due_sample_seq;
    } pending_l2tlb_sfence_t;
    pending_l2tlb_sfence_t pending_l2tlb_sfence_q[$];
    longint unsigned adapter_reset_serviced_epoch;

    `uvm_object_utils(dispatch_monitor_event_adapter)

    function new(string name = "dispatch_monitor_event_adapter");
        super.new(name);
        data = common_data_transaction::get();
        monitor_commit_handler = null;
        pending_l2tlb_sfence_q.delete();
        adapter_reset_serviced_epoch = 0;
    endfunction:new

    function void bind_commit_handler(input lsq_commit_handler handler);
        monitor_commit_handler = handler;
    endfunction:bind_commit_handler

    function void reset_l2tlb_sfence_state();
        // Runtime reset owns the epoch boundary. The adapter only drops its
        // private scheduled work and its package FIFO/live-entry state; it does
        // not clear CSR/fence history or any response-owner state.
        ensure_handles();
        pending_l2tlb_sfence_q.delete();
        if (memblock_sync_pkg::l2tlb_reset_active() &&
            memblock_sync_pkg::get_l2tlb_current_reset_epoch() != 0) begin
            longint unsigned reset_epoch;
            reset_epoch = memblock_sync_pkg::get_l2tlb_current_reset_epoch();
            if (adapter_reset_serviced_epoch != reset_epoch) begin
                memblock_sync_pkg::reset_l2tlb_adapter_runtime_state(reset_epoch);
                data.clear_dispatch_l2tlb_live_entries();
                adapter_reset_serviced_epoch = reset_epoch;
                if ((memblock_sync_pkg::l2tlb_runtime_reset_required_ack_mask &
                     memblock_sync_pkg::MEMBLOCK_L2TLB_RESET_ACK_ADAPTER) != '0) begin
                    memblock_sync_pkg::acknowledge_l2tlb_runtime_reset(
                        memblock_sync_pkg::MEMBLOCK_L2TLB_RESET_ACK_ADAPTER,
                        reset_epoch,
                        "dispatch_monitor_event_adapter");
                end
            end
        end
    endfunction:reset_l2tlb_sfence_state

    function void ensure_handles();
        if (data == null) begin
            data = common_data_transaction::get();
        end
        if (monitor_commit_handler == null) begin
            // LSQ commit/deq/head lifecycle 只能有一个 owner；adapter fallback 也必须
            // 绑定 singleton，禁止创建第二个带独立 cursor/pointer 的 handler。
            monitor_commit_handler = lsq_commit_handler::get();
        end
    endfunction:ensure_handles

    function memblock_wb_event_t make_wb_event_base();
        ensure_handles();
        return data.make_empty_wb_event();
    endfunction:make_wb_event_base

    function bit raw_rob_to_key(input bit valid,
                                input bit flag,
                                input bit [MEMBLOCK_ROB_VALUE_W-1:0] value,
                                output memblock_rob_key_t key);
        key.flag  = flag;
        key.value = value;
        return valid;
    endfunction:raw_rob_to_key

    function bit raw_lq_to_key(input bit valid,
                               input bit flag,
                               input bit [MEMBLOCK_LQ_VALUE_W-1:0] value,
                               output memblock_lq_key_t key);
        key.flag  = flag;
        key.value = value;
        return valid;
    endfunction:raw_lq_to_key

    function bit raw_sq_to_key(input bit valid,
                               input bit flag,
                               input bit [MEMBLOCK_SQ_VALUE_W-1:0] value,
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

    // 中文注释：唯一的 current issue snapshot owner。int-WB 先使用 ROB 精确 key 和
    // STD value-only 双 flag 分支；后续 IQ 只能扩展本函数，不能另建同名 API 或第二套 map owner。
    // 中文注释：把一个 active ROB 候选的当前 status/key 快照复制到 event。
    // strict_candidate=0 仅供 value-only 分支筛选候选；真正选中后仍必须以 strict=1 再校验。
    function bit fill_current_issue_snapshot(
        ref memblock_wb_event_t wb_event,
        input memblock_uid_t uid,
        input memblock_rob_key_t rob_key,
        input bit sample_flush_epoch_valid,
        input int unsigned sample_flush_epoch,
        input bit strict_candidate
    );
        status_transaction       status;
        main_control_transaction main_tr;
        memblock_rob_key_t       canonical_rob_key;
        memblock_lq_key_t        lq_key;
        memblock_sq_key_t        sq_key;
        memblock_uid_t            mapped_uid;
        int unsigned              target_epoch;
        int unsigned              active_instance_flush_epoch;

        status = data.get_status(uid);
        if (!status.active || status.terminal_done || status.flushed ||
            status.issue_killed || status.redirect_pending) begin
            if (strict_candidate) begin
                `uvm_fatal("INT_WB_ATTACH",
                           $sformatf("ROB candidate is not current: uid=%0d active=%0d terminal=%0d flushed=%0d killed=%0d redirect_pending=%0d",
                                     uid, status.active, status.terminal_done, status.flushed,
                                     status.issue_killed, status.redirect_pending))
            end
            return 1'b0;
        end
        if (!data.target_dispatched(status, wb_event.target)) begin
            if (strict_candidate) begin
                `uvm_fatal("INT_WB_ATTACH",
                           $sformatf("writeback target was not dispatched: uid=%0d target=%0d",
                                     uid, wb_event.target))
            end
            return 1'b0;
        end

        if (sample_flush_epoch_valid) begin
            if (sample_flush_epoch > memblock_sync_pkg::dispatch_flush_epoch) begin
                `uvm_fatal("INT_WB_ATTACH",
                           $sformatf("raw sample flush epoch is from the future: sample=%0d current=%0d",
                                     sample_flush_epoch, memblock_sync_pkg::dispatch_flush_epoch))
            end
            if (!status.get_target_instance_flush_epoch(wb_event.target,
                                                        active_instance_flush_epoch)) begin
                if (strict_candidate) begin
                    `uvm_fatal("INT_WB_ATTACH",
                               $sformatf("uid=%0d target=%0d has no active instance flush epoch",
                                         uid, wb_event.target))
                end
                return 1'b0;
            end
            // raw epoch 不能早于该 target 最近一次真实 issue；不能只比较当前全局 epoch，
            // 因为未被 redirect 杀死的老指令可能在年轻指令 redirect 后才写回。
            if (sample_flush_epoch < active_instance_flush_epoch) begin
                if (strict_candidate) begin
                    `uvm_fatal("INT_WB_ATTACH",
                               $sformatf("raw sample epoch is older than current target instance: uid=%0d target=%0d sample=%0d instance=%0d",
                                         uid, wb_event.target, sample_flush_epoch,
                                         active_instance_flush_epoch))
                end
                return 1'b0;
            end
        end

        canonical_rob_key = status.get_rob_key();
        if (canonical_rob_key.flag != rob_key.flag || canonical_rob_key.value != rob_key.value) begin
            `uvm_fatal("INT_WB_ATTACH",
                       $sformatf("ROB key owner mismatch uid=%0d raw=%0d/%0d status=%0d/%0d",
                                 uid, rob_key.flag, rob_key.value,
                                 canonical_rob_key.flag, canonical_rob_key.value))
        end

        main_tr = data.get_main_transaction(uid);
        if (wb_event.source == MEMBLOCK_WB_EVENT_SOURCE_LOAD_WB &&
            wb_event.target == MEMBLOCK_ISSUE_TARGET_LOAD &&
            wb_event.port_id == 0 &&
            main_tr.op_class == MEMBLOCK_OP_CLASS_AMO) begin
            `uvm_fatal("INT_WB_LDA0_AMO",
                       $sformatf("LDA0 is owned by unsupported AMO uid=%0d; do not enter LOAD/LQ lifecycle", uid))
        end

        target_epoch = status.get_target_issue_epoch(wb_event.target);
        if (target_epoch == 0) begin
            if (strict_candidate) begin
                `uvm_fatal("INT_WB_ATTACH",
                           $sformatf("current issue snapshot has no issue_epoch: uid=%0d target=%0d",
                                     uid, wb_event.target))
            end
            return 1'b0;
        end

        case (wb_event.target)
            MEMBLOCK_ISSUE_TARGET_LOAD: begin
                if (!status.active_lq_mapped) begin
                    if (strict_candidate) `uvm_fatal("INT_WB_ATTACH", $sformatf("LOAD uid=%0d has no active LQ mapping", uid))
                    return 1'b0;
                end
                lq_key.flag = status.lqIdx_flag;
                lq_key.value = status.lqIdx_value;
                if (!data.is_valid_lq_key(lq_key)) begin
                    `uvm_fatal("INT_WB_ATTACH", $sformatf("LOAD uid=%0d has incomplete LQ key", uid))
                end
                if (!data.lookup_active_uid_by_lq(lq_key, mapped_uid) || mapped_uid != uid) begin
                    `uvm_fatal("INT_WB_ATTACH", $sformatf("LOAD LQ owner mismatch uid=%0d", uid))
                end
                wb_event.lq_key = lq_key;
                wb_event.has_lq = 1'b1;
            end
            MEMBLOCK_ISSUE_TARGET_STA,
            MEMBLOCK_ISSUE_TARGET_STD: begin
                if (!status.active_sq_mapped) begin
                    if (strict_candidate) `uvm_fatal("INT_WB_ATTACH", $sformatf("STORE uid=%0d has no active SQ mapping", uid))
                    return 1'b0;
                end
                sq_key.flag = status.sqIdx_flag;
                sq_key.value = status.sqIdx_value;
                if (!data.is_valid_sq_key(sq_key)) begin
                    `uvm_fatal("INT_WB_ATTACH", $sformatf("STORE uid=%0d has incomplete SQ key", uid))
                end
                if (!data.lookup_active_uid_by_sq(sq_key, mapped_uid) || mapped_uid != uid) begin
                    `uvm_fatal("INT_WB_ATTACH", $sformatf("STORE SQ owner mismatch uid=%0d", uid))
                end
                wb_event.sq_key = sq_key;
                wb_event.has_sq = 1'b1;
            end
            default: `uvm_fatal("INT_WB_ATTACH", $sformatf("unsupported snapshot target=%0d", wb_event.target))
        endcase

        wb_event.uid = uid;
        wb_event.has_uid = 1'b1;
        wb_event.rob_key = canonical_rob_key;
        wb_event.has_rob = 1'b1;
        wb_event.issue_epoch = target_epoch;
        wb_event.has_issue_epoch = 1'b1;
        wb_event.replay_seq = status.replay_seq;
        wb_event.has_replay_seq = 1'b1;
        return 1'b1;
    endfunction:fill_current_issue_snapshot

    function void attach_current_issue_snapshot(
        ref memblock_wb_event_t wb_event,
        input bit rob_value_only_without_flag = 1'b0,
        input bit sample_flush_epoch_valid = 1'b0,
        input int unsigned sample_flush_epoch = 0
    );
        memblock_uid_t uid;
        memblock_rob_key_t rob_key;
        memblock_wb_event_t candidate;

        ensure_handles();
        if (!wb_event.valid) begin
            `uvm_fatal("INT_WB_ATTACH", "attach_current_issue_snapshot requires valid event")
        end
        if (sample_flush_epoch_valid && sample_flush_epoch > memblock_sync_pkg::dispatch_flush_epoch) begin
            `uvm_fatal("INT_WB_ATTACH", "snapshot attach received a future sample epoch")
        end

        // 中文注释：V2 STA IQ feedback 只携带 SQ。先用当前 active SQ mapping
        // 反查 owner，再复用公共 ROB/target snapshot 检查补齐 event 身份。
        if (wb_event.source == MEMBLOCK_WB_EVENT_SOURCE_STA_FEEDBACK &&
            wb_event.target == MEMBLOCK_ISSUE_TARGET_STA &&
            wb_event.has_sq && !wb_event.has_rob && !wb_event.has_lq) begin
            memblock_uid_t iq_uid;
            status_transaction iq_status;
            memblock_sq_key_t canonical_sq;
            memblock_wb_event_t iq_candidate;

            if (!data.is_valid_sq_key(wb_event.sq_key)) begin
                `uvm_fatal("IQ_FEEDBACK_ATTACH",
                           $sformatf("STA IQ raw SQ key is incomplete=%0d/%0d",
                                     wb_event.sq_key.flag, wb_event.sq_key.value))
            end
            if (!data.lookup_active_uid_by_sq(wb_event.sq_key, iq_uid)) begin
                `uvm_fatal("IQ_FEEDBACK_ATTACH",
                           $sformatf("no active uid for STA IQ SQ key=%0d/%0d",
                                     wb_event.sq_key.flag, wb_event.sq_key.value))
            end
            iq_status = data.get_status(iq_uid);
            canonical_sq.flag  = iq_status.sqIdx_flag;
            canonical_sq.value = iq_status.sqIdx_value;
            if (!iq_status.active_sq_mapped ||
                !iq_status.sta_dispatched ||
                canonical_sq.flag != wb_event.sq_key.flag ||
                canonical_sq.value != wb_event.sq_key.value) begin
                `uvm_fatal("IQ_FEEDBACK_ATTACH",
                           $sformatf("STA IQ SQ owner mismatch uid=%0d raw=%0d/%0d status=%0d/%0d mapped=%0d",
                                     iq_uid,
                                     wb_event.sq_key.flag, wb_event.sq_key.value,
                                     canonical_sq.flag, canonical_sq.value,
                                     iq_status.active_sq_mapped))
            end
            iq_candidate = wb_event;
            iq_candidate.uid = iq_uid;
            iq_candidate.has_uid = 1'b1;
            iq_candidate.rob_key = iq_status.get_rob_key();
            iq_candidate.has_rob = 1'b1;
            if (!fill_current_issue_snapshot(iq_candidate,
                                             iq_uid,
                                             iq_candidate.rob_key,
                                             1'b0,
                                             0,
                                             1'b1)) begin
                `uvm_fatal("IQ_FEEDBACK_ATTACH", "STA IQ current snapshot validation failed")
            end
            wb_event = iq_candidate;
            return;
        end

        if (rob_value_only_without_flag) begin
            if (wb_event.has_rob) begin
                `uvm_fatal("INT_WB_ATTACH", "STD value-only event must not claim a ROB flag")
            end
            begin : attach_value_only_candidates
                memblock_uid_t uid0;
                memblock_uid_t uid1;
                memblock_wb_event_t candidate0;
                memblock_wb_event_t candidate1;
                bit hit0;
                bit hit1;

                hit0 = probe_std_candidate(wb_event, 1'b0, wb_event.rob_key.value,
                                           sample_flush_epoch_valid, sample_flush_epoch,
                                           uid0, candidate0);
                hit1 = probe_std_candidate(wb_event, 1'b1, wb_event.rob_key.value,
                                           sample_flush_epoch_valid, sample_flush_epoch,
                                           uid1, candidate1);
                if (hit0 && hit1) begin
                    `uvm_fatal("INT_WB_STD_KEY", "STD value-only attach has two valid candidates")
                end
                if (!hit0 && !hit1) begin
                    `uvm_fatal("INT_WB_STD_KEY", "STD value-only attach has no valid candidate")
                end
                wb_event = hit0 ? candidate0 : candidate1;
            end
            return;
        end

        if (!wb_event.has_rob) begin
            `uvm_fatal("INT_WB_ATTACH", "ROB snapshot attach requires a complete raw ROB key")
        end
        rob_key = wb_event.rob_key;
        if (!data.lookup_active_uid_by_rob(rob_key, uid)) begin
            `uvm_fatal("INT_WB_ATTACH",
                       $sformatf("no active uid for ROB key=%0d/%0d", rob_key.flag, rob_key.value))
        end
        candidate = wb_event;
        if (!fill_current_issue_snapshot(candidate, uid, rob_key,
                                         sample_flush_epoch_valid, sample_flush_epoch, 1'b1)) begin
            `uvm_fatal("INT_WB_ATTACH", "ROB candidate failed current snapshot validation")
        end
        wb_event = candidate;
    endfunction:attach_current_issue_snapshot

    // 中文注释：对一个可能的 STD ROB flag 做一次有限候选检查。
    // active ROB 命中后先按 STD target/status/SQ owner/实例 epoch 过滤，再参与唯一性判断；
    // 因而另一 flag 命中一个非 STD uid 时不会误触发“双候选” fatal。
    function bit probe_std_candidate(
        input memblock_wb_event_t template_event,
        input bit rob_flag,
        input bit [MEMBLOCK_ROB_VALUE_W-1:0] rob_value,
        input bit sample_flush_epoch_valid,
        input int unsigned sample_flush_epoch,
        output memblock_uid_t uid,
        output memblock_wb_event_t candidate
    );
        memblock_rob_key_t rob_key;

        uid = 0;
        candidate = template_event;
        rob_key.flag = rob_flag;
        rob_key.value = rob_value;
        candidate.rob_key = rob_key;
        candidate.has_rob = 1'b0;
        if (!data.lookup_active_uid_by_rob(rob_key, uid)) begin
            return 1'b0;
        end
        return fill_current_issue_snapshot(candidate, uid, rob_key,
                                           sample_flush_epoch_valid,
                                           sample_flush_epoch,
                                           1'b0);
    endfunction:probe_std_candidate

    function void resolve_std_uid_by_rob_value_only(
        input memblock_sync_pkg::dispatch_raw_int_wb_t raw,
        ref memblock_wb_event_t wb_event
    );
        wb_event.rob_key.flag = 1'b0;
        wb_event.rob_key.value = raw.rob_value;
        wb_event.has_rob = 1'b0;
        begin : resolve_candidates
            memblock_uid_t uid0;
            memblock_uid_t uid1;
            memblock_wb_event_t candidate0;
            memblock_wb_event_t candidate1;
            bit hit0;
            bit hit1;

            hit0 = probe_std_candidate(wb_event, 1'b0, raw.rob_value,
                                       1'b1, raw.sample_flush_epoch,
                                       uid0, candidate0);
            hit1 = probe_std_candidate(wb_event, 1'b1, raw.rob_value,
                                       1'b1, raw.sample_flush_epoch,
                                       uid1, candidate1);
            if (hit0 && hit1) begin
                `uvm_fatal("INT_WB_STD_KEY",
                           $sformatf("STD ROB value=%0d has two valid active STD flag candidates uid0=%0d uid1=%0d",
                                     raw.rob_value, uid0, uid1))
            end
            if (!hit0 && !hit1) begin
                `uvm_fatal("INT_WB_STD_KEY",
                           $sformatf("STD ROB value=%0d has zero valid active STD flag candidates",
                                     raw.rob_value))
            end
            if (hit0) begin
                wb_event = candidate0;
            end else begin
                wb_event = candidate1;
            end
        end
    endfunction:resolve_std_uid_by_rob_value_only

    function void check_raw_int_wb_capability(input memblock_sync_pkg::dispatch_raw_int_wb_t raw);
        bit [23:0] allowed_exception_mask;

        allowed_exception_mask = 24'b0;
        if (raw.source_kind == memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_INVALID) begin
            `uvm_fatal("INT_WB_CAP", "valid raw event has INVALID source_kind")
        end
        case (raw.source_kind)
            memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_SCALAR_LDA: begin
                if (raw.port_id > 2 || !raw.rob_valid || raw.rob_value_only_without_flag ||
                    raw.lq_valid || raw.sq_valid || !raw.key_needs_state_lookup ||
                    !raw.replay_inst_valid || !raw.flush_pipe_valid || !raw.trigger_valid) begin
                    `uvm_fatal("INT_WB_CAP", $sformatf("invalid SCALAR_LDA raw capability lane=%0d", raw.port_id))
                end
                if (raw.port_id == 0) begin
                    allowed_exception_mask = 24'hA8A0F8;
                end else begin
                    allowed_exception_mask = 24'h282038;
                end
            end
            memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STA: begin
                if (raw.port_id > 1 || !raw.rob_valid || raw.rob_value_only_without_flag ||
                    raw.lq_valid || raw.sq_valid || !raw.key_needs_state_lookup ||
                    raw.replay_inst_valid || !raw.trigger_valid ||
                    raw.flush_pipe_valid != (raw.port_id == 0) ||
                    raw.replay_inst || (!raw.flush_pipe_valid && raw.flush_pipe)) begin
                    `uvm_fatal("INT_WB_CAP", $sformatf("invalid STA raw capability lane=%0d", raw.port_id))
                end
                if (raw.port_id == 0) begin
                    allowed_exception_mask = 24'hffffff;
                end else begin
                    allowed_exception_mask = 24'h8880C8;
                end
            end
            memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STD: begin
                if (raw.port_id > 1 || raw.rob_valid || !raw.rob_value_only_without_flag ||
                    raw.lq_valid || raw.sq_valid || !raw.key_needs_state_lookup ||
                    raw.replay_inst_valid || raw.flush_pipe_valid || raw.trigger_valid ||
                    raw.replay_inst || raw.flush_pipe ||
                    raw.exception_vec != 24'b0 || raw.trigger != 4'hf) begin
                    `uvm_fatal("INT_WB_CAP", $sformatf("invalid STD value-only raw capability lane=%0d", raw.port_id))
                end
            end
            default: `uvm_fatal("INT_WB_CAP", $sformatf("unsupported source_kind=%0d", raw.source_kind))
        endcase
        if ((raw.exception_vec & ~allowed_exception_mask) != 24'b0) begin
            `uvm_fatal("INT_WB_CAP",
                       $sformatf("raw exceptionVec contains bits absent from V2 source/lane kind=%0d lane=%0d value=0x%0h mask=0x%0h",
                                 raw.source_kind, raw.port_id, raw.exception_vec,
                                 allowed_exception_mask))
        end
    endfunction:check_raw_int_wb_capability

    function void check_raw_int_wb_metadata(input memblock_sync_pkg::dispatch_raw_int_wb_t raw);
        if (raw.source_kind == memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_SCALAR_LDA) begin
            if (raw.replay_inst) begin
                `uvm_fatal("INT_WB_SCALAR_LDA_REPLAY_INST_INVARIANT", "SCALAR_LDA replayInst must be zero in current V2 profile")
            end
            if (raw.flush_pipe) begin
                `uvm_fatal("INT_WB_SCALAR_LDA_FLUSH_PIPE_INVARIANT", "SCALAR_LDA flushPipe must be zero in current V2 profile")
            end
        end
        if (!raw.trigger_valid) begin
            if (raw.trigger != 4'hf) begin
                `uvm_fatal("INT_WB_METADATA", "absent trigger metadata must keep TriggerAction.None")
            end
            return;
        end
        case (raw.trigger)
            4'hf: ;
            4'h0: begin
                if (!raw.exception_vec[3] &&
                    !(raw.source_kind == memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STA && raw.port_id == 0)) begin
                    `uvm_fatal("INT_WB_TRIGGER", "BreakpointExp trigger requires exceptionVec[breakPoint]")
                end
            end
            4'h1, 4'h2, 4'h3, 4'h4: begin
                `uvm_fatal("INT_WB_TRIGGER_UNSUPPORTED", $sformatf("unsupported trigger action=0x%0h", raw.trigger))
            end
            default: `uvm_fatal("INT_WB_TRIGGER", $sformatf("unknown trigger action=0x%0h", raw.trigger))
        endcase
    endfunction:check_raw_int_wb_metadata

    function void check_attached_int_wb_metadata(
        input memblock_sync_pkg::dispatch_raw_int_wb_t raw,
        input memblock_wb_event_t wb_event
    );
        main_control_transaction main_tr;

        if (raw.source_kind == memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STA && raw.port_id == 0) begin
            main_tr = data.get_main_transaction(wb_event.uid);
            if (raw.flush_pipe) begin
                if (main_tr.op_class != MEMBLOCK_OP_CLASS_CBO) begin
                    `uvm_fatal("INT_WB_STA0_FLUSH_PIPE", "STA0 flushPipe is only legal for CBO/CMO producer")
                end
                `uvm_fatal("INT_WB_STA0_CBO_UNSUPPORTED", "STA0 CBO flushAfter has no current adapter consumer")
            end
            if (raw.trigger == 4'h0 && !raw.exception_vec[3] &&
                main_tr.op_class != MEMBLOCK_OP_CLASS_CBO &&
                !raw.debug_is_mmio && !raw.debug_is_ncio) begin
                `uvm_fatal("INT_WB_STA0_TRIGGER_PROVENANCE", "STA0 trigger=0 without breakpoint needs uncache/CBO provenance")
            end
        end
    endfunction:check_attached_int_wb_metadata

    function bit normalize_v2_int_wb_key(
        input memblock_sync_pkg::dispatch_raw_int_wb_t raw,
        ref memblock_wb_event_t wb_event
    );
        memblock_rob_key_t raw_rob_key;

        raw_rob_key.flag = raw.rob_flag;
        raw_rob_key.value = raw.rob_value;
        case (raw.source_kind)
            memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_SCALAR_LDA: begin
                if (wb_event.source != MEMBLOCK_WB_EVENT_SOURCE_LOAD_WB ||
                    wb_event.target != MEMBLOCK_ISSUE_TARGET_LOAD ||
                    !raw.rob_valid || raw.rob_value_only_without_flag ||
                    !wb_event.has_uid || !wb_event.has_rob || !wb_event.has_lq ||
                    !wb_event.has_issue_epoch || !wb_event.has_replay_seq ||
                    wb_event.rob_key.flag != raw_rob_key.flag ||
                    wb_event.rob_key.value != raw_rob_key.value) begin
                    `uvm_fatal("INT_WB_KEY", "SCALAR_LDA key normalization failed")
                end
            end
            memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STA: begin
                if (wb_event.source != MEMBLOCK_WB_EVENT_SOURCE_STORE_WB ||
                    wb_event.target != MEMBLOCK_ISSUE_TARGET_STA ||
                    !raw.rob_valid || raw.rob_value_only_without_flag ||
                    !wb_event.has_uid || !wb_event.has_rob || !wb_event.has_sq ||
                    !wb_event.has_issue_epoch || !wb_event.has_replay_seq ||
                    wb_event.rob_key.flag != raw_rob_key.flag ||
                    wb_event.rob_key.value != raw_rob_key.value) begin
                    `uvm_fatal("INT_WB_KEY", "STA key normalization failed")
                end
            end
            memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STD: begin
                if (wb_event.source != MEMBLOCK_WB_EVENT_SOURCE_STORE_WB ||
                    wb_event.target != MEMBLOCK_ISSUE_TARGET_STD ||
                    !raw.rob_value_only_without_flag || !wb_event.has_uid ||
                    !wb_event.has_rob || !wb_event.has_sq ||
                    !wb_event.has_issue_epoch || !wb_event.has_replay_seq ||
                    wb_event.rob_key.value != raw.rob_value) begin
                    `uvm_fatal("INT_WB_STD_KEY_NORMALIZE_FAILED", "STD value-only key normalization failed")
                end
            end
            default: `uvm_fatal("INT_WB_KEY", "unsupported V2 raw source during normalization")
        endcase
        wb_event.real_wb_valid = 1'b1;
        return 1'b1;
    endfunction:normalize_v2_int_wb_key

    function bit convert_raw_int_wb(input memblock_sync_pkg::dispatch_raw_int_wb_t raw,
                                    output memblock_wb_event_t wb_event);
        wb_event = make_wb_event_base();
        if (!raw.valid) begin
            return 1'b0;
        end
        ensure_handles();
        check_raw_int_wb_capability(raw);
        check_raw_int_wb_metadata(raw);
        wb_event.valid = 1'b1;
        wb_event.port_id = raw.port_id;
        wb_event.exception_vec = raw.exception_vec;
        wb_event.has_exception = raw.exception_vec != 24'b0;
        case (raw.source_kind)
            memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_SCALAR_LDA: begin
                wb_event.source = MEMBLOCK_WB_EVENT_SOURCE_LOAD_WB;
                wb_event.target = MEMBLOCK_ISSUE_TARGET_LOAD;
                wb_event.has_rob = raw_rob_to_key(raw.rob_valid, raw.rob_flag, raw.rob_value, wb_event.rob_key);
                attach_current_issue_snapshot(wb_event, 1'b0, 1'b1, raw.sample_flush_epoch);
            end
            memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STA: begin
                wb_event.source = MEMBLOCK_WB_EVENT_SOURCE_STORE_WB;
                wb_event.target = MEMBLOCK_ISSUE_TARGET_STA;
                wb_event.has_rob = raw_rob_to_key(raw.rob_valid, raw.rob_flag, raw.rob_value, wb_event.rob_key);
                attach_current_issue_snapshot(wb_event, 1'b0, 1'b1, raw.sample_flush_epoch);
            end
            memblock_sync_pkg::MEMBLOCK_INT_WB_SOURCE_STD: begin
                wb_event.source = MEMBLOCK_WB_EVENT_SOURCE_STORE_WB;
                wb_event.target = MEMBLOCK_ISSUE_TARGET_STD;
                resolve_std_uid_by_rob_value_only(raw, wb_event);
            end
            default: `uvm_fatal("DISP_MON_ADAPT", "invalid V2 int-WB source kind")
        endcase
        check_attached_int_wb_metadata(raw, wb_event);
        wb_event.cycle = raw.cycle;
        if (!normalize_v2_int_wb_key(raw, wb_event)) begin
            `uvm_fatal("INT_WB_KEY", "V2 int-WB normalization returned failure")
        end
        return 1'b1;
    endfunction:convert_raw_int_wb

    function bit convert_raw_iq_feedback(input memblock_sync_pkg::dispatch_raw_iq_feedback_t raw,
                                         output memblock_wb_event_t wb_event);
        wb_event = make_wb_event_base();
        if (!raw.valid) begin
            return 1'b0;
        end
        if (raw.vector_feedback) begin
            `uvm_fatal("DISP_MON_ADAPT",
                       $sformatf("vector IQ feedback is unsupported port=%0d source_type=0x%0h",
                                 raw.port_id, raw.source_type))
        end
        if (!raw.is_sta && !raw.is_std) begin
            `uvm_fatal("DISP_MON_ADAPT", "IQ feedback has no supported scalar target")
        end
        if (raw.is_std) begin
            `uvm_fatal("DISP_MON_ADAPT",
                       $sformatf("STD IQ feedback cannot complete strict V2 STD real-WB target: port=%0d hit=%0d",
                                 raw.port_id, raw.hit))
        end
        if (!raw.sq_valid || raw.rob_valid || raw.lq_valid) begin
            `uvm_fatal("DISP_MON_ADAPT",
                       $sformatf("STA IQ feedback must be SQ-only: sq_valid=%0d rob_valid=%0d lq_valid=%0d",
                                 raw.sq_valid, raw.rob_valid, raw.lq_valid))
        end

        wb_event.valid         = 1'b1;
        wb_event.port_id       = raw.port_id;
        wb_event.target         = MEMBLOCK_ISSUE_TARGET_STA;
        wb_event.source         = MEMBLOCK_WB_EVENT_SOURCE_STA_FEEDBACK;
        wb_event.has_rob       = raw_rob_to_key(raw.rob_valid, raw.rob_flag, raw.rob_value, wb_event.rob_key);
        wb_event.has_lq         = 1'b0;
        wb_event.has_sq         = raw_sq_to_key(raw.sq_valid, raw.sq_flag, raw.sq_value, wb_event.sq_key);
        attach_current_issue_snapshot(wb_event);
        if (!wb_event.has_uid || !wb_event.has_rob || !wb_event.has_sq ||
            !wb_event.has_issue_epoch || !wb_event.has_replay_seq) begin
            `uvm_fatal("DISP_MON_ADAPT", "STA IQ feedback snapshot is incomplete")
        end
        // 中文注释：IQ feedback 是 IssueQueue response，不是真实 ROB/RF writeback。
        // hit/finalSuccess 只写 iq_feedback_*；STA miss 额外生成 replay；STD 在入口直接 fatal。
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

    // 中文注释：把同一 ctrl raw 的 value-only MMIO facts 先全部归一化、去重和
    // dry-run 预检，再按首 port 顺序原子提交。任何冲突都发生在 status 写入前。
    function void apply_raw_ctrl_mmio_tags(
        input memblock_sync_pkg::dispatch_raw_ctrl_t raw
    );
        memblock_mmio_tag_stage_t staged_tags[$];
        int unsigned port_order;

        ensure_handles();
        if (raw.load_mmio_valid == '0 && !raw.store_mmio_valid) begin
            return;
        end
        // 必须在第一次 active-map probe 前拒绝 future raw。
        if (raw.mmio_flush_epoch > memblock_sync_pkg::dispatch_flush_epoch) begin
            `uvm_fatal("MMIO_RAW",
                       $sformatf("future ctrl raw epoch=%0d current=%0d cycle=%0d",
                                 raw.mmio_flush_epoch,
                                 memblock_sync_pkg::dispatch_flush_epoch,
                                 raw.cycle))
        end

        port_order = 0;
        foreach (raw.load_mmio_valid[port]) begin
            if (raw.load_mmio_valid[port]) begin
                memblock_uid_t uid;
                string stale_reason;
                memblock_mmio_resolve_result_e result;
                int existing_idx;

                result = data.resolve_mmio_uid_by_rob_value(
                    raw.load_mmio_rob_value[port],
                    MEMBLOCK_MMIO_KIND_LOAD,
                    raw.mmio_flush_epoch,
                    raw.mmio_sample_seq,
                    uid,
                    stale_reason);
                if (result == MEMBLOCK_MMIO_RESOLVE_STALE_DROP) begin
                    `uvm_info("MMIO_RAW",
                              $sformatf("drop stale loadMmio port=%0d ROB value=%0d epoch=%0d sample=%0d reason=%s",
                                        port, raw.load_mmio_rob_value[port],
                                        raw.mmio_flush_epoch, raw.mmio_sample_seq,
                                        stale_reason),
                              UVM_LOW)
                end else begin
                    existing_idx = -1;
                    foreach (staged_tags[idx]) begin
                        if (staged_tags[idx].uid == uid) begin
                            existing_idx = idx;
                            break;
                        end
                    end
                    if (existing_idx >= 0 &&
                        staged_tags[existing_idx].kind != MEMBLOCK_MMIO_KIND_LOAD) begin
                        `uvm_fatal("MMIO_RAW",
                                   $sformatf("same raw assigns load/store MMIO kinds to uid=%0d",
                                             uid))
                    end
                    if (existing_idx < 0) begin
                        memblock_mmio_tag_stage_t stage;

                        stage.uid = uid;
                        stage.kind = MEMBLOCK_MMIO_KIND_LOAD;
                        stage.first_port_order = port_order;
                        staged_tags.push_back(stage);
                    end
                end
            end
            port_order++;
        end

        if (raw.store_mmio_valid) begin
            memblock_uid_t uid;
            string stale_reason;
            memblock_mmio_resolve_result_e result;
            int existing_idx;

            result = data.resolve_mmio_uid_by_rob_value(
                raw.store_mmio_rob_value,
                MEMBLOCK_MMIO_KIND_STORE,
                raw.mmio_flush_epoch,
                raw.mmio_sample_seq,
                uid,
                stale_reason);
            if (result == MEMBLOCK_MMIO_RESOLVE_STALE_DROP) begin
                `uvm_info("MMIO_RAW",
                          $sformatf("drop stale storeMmio ROB value=%0d epoch=%0d reason=%s",
                                    raw.store_mmio_rob_value,
                                    raw.mmio_flush_epoch, stale_reason),
                          UVM_LOW)
            end else begin
                existing_idx = -1;
                foreach (staged_tags[idx]) begin
                    if (staged_tags[idx].uid == uid) begin
                        existing_idx = idx;
                        break;
                    end
                end
                if (existing_idx >= 0 &&
                    staged_tags[existing_idx].kind != MEMBLOCK_MMIO_KIND_STORE) begin
                    `uvm_fatal("MMIO_RAW",
                               $sformatf("same raw assigns load/store MMIO kinds to uid=%0d",
                                         uid))
                end
                if (existing_idx < 0) begin
                    memblock_mmio_tag_stage_t stage;

                    stage.uid = uid;
                    stage.kind = MEMBLOCK_MMIO_KIND_STORE;
                    stage.first_port_order = port_order;
                    staged_tags.push_back(stage);
                end
            end
        end

        foreach (staged_tags[idx]) begin
            data.set_uid_mmio_tag(staged_tags[idx].uid,
                                  staged_tags[idx].kind,
                                  MEMBLOCK_MMIO_TAG_MONITOR,
                                  1'b0);
        end
        foreach (staged_tags[idx]) begin
            data.set_uid_mmio_tag(staged_tags[idx].uid,
                                  staged_tags[idx].kind,
                                  MEMBLOCK_MMIO_TAG_MONITOR,
                                  1'b1);
        end
    endfunction:apply_raw_ctrl_mmio_tags

    function bit apply_raw_ctrl_deq(input memblock_sync_pkg::dispatch_raw_ctrl_t raw);
        ensure_handles();
        // MMIO normalization 必须先于 full-raw deq；后者可能删除 active ROB/LQ/SQ map。
        apply_raw_ctrl_mmio_tags(raw);
        return monitor_commit_handler.apply_raw_ctrl_deq(raw);
    endfunction:apply_raw_ctrl_deq

    // 将本轮已经转换过 semantic event 的 full raw 转入持久 FIFO，并按队首
    // 成功语义消费。resync mismatch 只阻塞队首，不丢弃当前 raw 或后续 raw。
    function void apply_deferred_ctrl_updates_batch(
        ref memblock_sync_pkg::dispatch_raw_ctrl_t deferred_ctrl[$]);
        memblock_sync_pkg::dispatch_raw_ctrl_t raw;
        memblock_sync_pkg::dispatch_raw_ctrl_t applied_raw;

        ensure_handles();
        foreach (deferred_ctrl[idx]) begin
            memblock_sync_pkg::push_deferred_raw_ctrl(deferred_ctrl[idx]);
        end
        deferred_ctrl.delete();

        while (memblock_sync_pkg::peek_deferred_raw_ctrl(raw)) begin
            if (!apply_raw_ctrl_deq(raw)) begin
                break;
            end
            if (!memblock_sync_pkg::pop_deferred_raw_ctrl(applied_raw)) begin
                `uvm_fatal("DISP_MON_BATCH", "deferred ctrl apply succeeded but queue pop failed")
            end
        end
    endfunction:apply_deferred_ctrl_updates_batch

    function void drain_csr_events();
        memblock_sync_pkg::dispatch_raw_csr_t raw_csr;
        int unsigned raw_csr_seq;

        ensure_handles();
        if (memblock_sync_pkg::get_latest_raw_csr(raw_csr, raw_csr_seq)) begin
            data.apply_raw_csr_runtime(raw_csr, raw_csr_seq);
        end
    endfunction:drain_csr_events

    function void drain_l2tlb_sfence_events(input longint unsigned current_sample_seq);
        memblock_sync_pkg::dispatch_raw_sfence_t raw_sfence;
        pending_l2tlb_sfence_t pending;

        ensure_handles();
        if (!memblock_sync_pkg::dispatch_l2tlb_lookup_active) begin
            return;
        end
        if (current_sample_seq == 0 ||
            !memblock_sync_pkg::l2tlb_sample_ready(current_sample_seq)) begin
            return;
        end
        while (memblock_sync_pkg::pop_raw_sfence(raw_sfence)) begin
            if (raw_sfence.sample_seq == 0 ||
                raw_sfence.sample_seq > current_sample_seq ||
                raw_sfence.lifecycle_event_seq ==
                    memblock_sync_pkg::MEMBLOCK_L2TLB_EVENT_SEQ_NONE ||
                raw_sfence.lifecycle_event_seq >
                    memblock_sync_pkg::last_allocated_l2tlb_event_seq) begin
                `uvm_fatal("DISP_RAW_SFENCE",
                           $sformatf("invalid raw fence provenance sample=%0d current=%0d event_seq=%0d last_event=%0d",
                                     raw_sfence.sample_seq, current_sample_seq,
                                     raw_sfence.lifecycle_event_seq,
                                     memblock_sync_pkg::last_allocated_l2tlb_event_seq))
            end
            pending.raw = raw_sfence;
            pending.due_sample_seq = raw_sfence.sample_seq +
                                     MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES;
            pending_l2tlb_sfence_q.push_back(pending);
        end
    endfunction:drain_l2tlb_sfence_events

    function void apply_due_l2tlb_sfence_events(input longint unsigned current_sample_seq);
        pending_l2tlb_sfence_t pending;

        ensure_handles();
        while (pending_l2tlb_sfence_q.size() != 0 &&
               pending_l2tlb_sfence_q[0].due_sample_seq <= current_sample_seq) begin
            pending = pending_l2tlb_sfence_q.pop_front();
            // C4 is the first destructive point. Keep apply_raw_sfence as the
            // existing live-entry owner and invoke it only after due sample.
            void'(data.apply_raw_sfence(pending.raw));
        end
    endfunction:apply_due_l2tlb_sfence_events

    function void service_l2tlb_sfence_events();
        longint unsigned current_sample_seq;

        ensure_handles();
        if (memblock_sync_pkg::reset_backend_done !== 1'b1) begin
            reset_l2tlb_sfence_state();
            return;
        end
        current_sample_seq = memblock_sync_pkg::peek_current_dut_global_sample();
        if (current_sample_seq == 0 ||
            !memblock_sync_pkg::l2tlb_sample_ready(current_sample_seq)) begin
            return;
        end
        drain_l2tlb_sfence_events(current_sample_seq);
        apply_due_l2tlb_sfence_events(current_sample_seq);
        publish_l2tlb_adapter_drain_proof(current_sample_seq);
    endfunction:service_l2tlb_sfence_events

    function void publish_l2tlb_adapter_drain_proof(
        input longint unsigned current_sample_seq);
        // Abstract function: publish that this adapter has no scheduled SFENCE
        // work left at the current sample.  It does not touch response ownership.
        if (!memblock_sync_pkg::dispatch_l2tlb_lookup_active ||
            !memblock_sync_pkg::l2tlb_release_admission_close_requested ||
            memblock_sync_pkg::l2tlb_reset_active() ||
            current_sample_seq == 0 ||
            !memblock_sync_pkg::l2tlb_sample_ready(current_sample_seq)) begin
            return;
        end
        if (pending_l2tlb_sfence_q.size() != 0 ||
            memblock_sync_pkg::raw_sfence_q.size() != 0) begin
            return;
        end
        memblock_sync_pkg::mark_l2tlb_adapter_drain_done(
            memblock_sync_pkg::get_l2tlb_current_reset_epoch(),
            memblock_sync_pkg::l2tlb_release_admission_close_generation);
    endfunction:publish_l2tlb_adapter_drain_proof

    function void drain_lsq_timing_sidebands();
        memblock_sync_pkg::dispatch_raw_cancel_snapshot_t cancel_snapshot;
        memblock_sync_pkg::dispatch_raw_redirect_anchor_t redirect_anchor;

        ensure_handles();
        // 中文伪代码：collector 只保存采样事实，可在同一 service tick 前后各
        // 调用一次；reconcile 由独立入口在 exception/redirect 处理后只执行一次。
        while (memblock_sync_pkg::pop_raw_cancel_snapshot(cancel_snapshot)) begin
            data.add_cancel_snapshot(cancel_snapshot);
        end
        while (memblock_sync_pkg::pop_raw_redirect_anchor(redirect_anchor)) begin
            data.add_redirect_anchor(redirect_anchor);
        end
    endfunction:drain_lsq_timing_sidebands

    // 中文注释：每个 dispatch service tick 的唯一 reconcile 调用入口。
    // 对账不调用 release/cancel，也不写 pass/fail/terminal。
    function void service_lsq_timing_reconcile();
        ensure_handles();
        data.service_cancel_reconcile();
    endfunction:service_lsq_timing_reconcile

    task check_raw_sample_cycle(input longint unsigned raw_cycle,
                                ref longint unsigned sample_cycle,
                                ref bit sample_cycle_valid,
                                input string source_name);
        if (!sample_cycle_valid) begin
            sample_cycle = raw_cycle;
            sample_cycle_valid = 1'b1;
        end else if (sample_cycle != raw_cycle) begin
            `uvm_fatal("DISP_MON_BATCH",
                       $sformatf("mixed monitor sample cycle source=%s expected=%0d actual=%0d",
                                 source_name, sample_cycle, raw_cycle))
        end
    endtask:check_raw_sample_cycle

    task collect_writeback_events_batch(ref memblock_wb_event_t events[$],
                                        ref longint unsigned sample_cycle,
                                        ref bit sample_cycle_valid);
        memblock_sync_pkg::dispatch_raw_int_wb_t raw_int_wb;
        memblock_sync_pkg::dispatch_raw_iq_feedback_t raw_iq;
        memblock_wb_event_t wb_event;

        // 中文注释：IQ hit 是 STA real-WB 的语义前置事件。各 raw queue 内仍保持 FIFO，
        // 这里只固定同一采样 batch 的跨队列顺序，再交给 batch handler 做 redirect-first。
        while (memblock_sync_pkg::pop_raw_iq_feedback(raw_iq)) begin
            check_raw_sample_cycle(raw_iq.cycle, sample_cycle, sample_cycle_valid, "iq_feedback");
            if (convert_raw_iq_feedback(raw_iq, wb_event)) begin
                events.push_back(wb_event);
            end
        end
        while (memblock_sync_pkg::pop_raw_int_wb(raw_int_wb)) begin
            check_raw_sample_cycle(raw_int_wb.cycle, sample_cycle, sample_cycle_valid, "int_wb");
            if (convert_raw_int_wb(raw_int_wb, wb_event)) begin
                events.push_back(wb_event);
            end
        end
    endtask:collect_writeback_events_batch

    task collect_ctrl_redirect_events_batch(ref memblock_wb_event_t events[$],
                                            ref memblock_sync_pkg::dispatch_raw_ctrl_t deferred_ctrl[$],
                                            ref longint unsigned sample_cycle,
                                            ref bit sample_cycle_valid);
        memblock_sync_pkg::dispatch_raw_ctrl_t raw_ctrl;
        memblock_wb_event_t wb_event;

        while (memblock_sync_pkg::pop_raw_ctrl(raw_ctrl)) begin
            check_raw_sample_cycle(raw_ctrl.cycle, sample_cycle, sample_cycle_valid, "ctrl");
            deferred_ctrl.push_back(raw_ctrl);
            if (convert_raw_memory_violation(raw_ctrl, wb_event)) begin
                events.push_back(wb_event);
            end
        end
    endtask:collect_ctrl_redirect_events_batch

endclass:dispatch_monitor_event_adapter

`endif
