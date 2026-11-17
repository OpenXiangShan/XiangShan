//=========================================================
// 后端周期帧协调器：由原唯一dispatch service逐拍调用，不创建并发owner。
//=========================================================
`ifndef MEMBLOCK_BACKEND_CYCLE_SCHEDULER__SV
`define MEMBLOCK_BACKEND_CYCLE_SCHEDULER__SV
class memblock_backend_cycle_scheduler extends uvm_object;
    static memblock_backend_cycle_scheduler inst;
    common_data_transaction data;
    lsq_commit_handler commits;
    longint unsigned next_frame_id;
    longint unsigned last_source_sample;
    // 中文注释：未绑定来源拍的合法older预占，与已提交物理前级槽分离。
    memblock_backend_cycle_frame_t reserved_older_decision;
    typedef struct {
        bit valid, consumed;
        longint unsigned source_sample, reset_epoch, reservation_id;
        memblock_sync_pkg::backend_cycle_projection_t projection;
    } memblock_rob_decision_slot_t;
    memblock_rob_decision_slot_t rob_source_slot;
    memblock_sync_pkg::backend_cycle_projection_t last_published;
    bit last_published_valid;
    bit active_flushsb;
    memblock_flushsb_req_t flushsb_req;
    int unsigned auto_fault_intent_id;

    `uvm_object_utils(memblock_backend_cycle_scheduler)
    function new(string name="memblock_backend_cycle_scheduler");
        super.new(name);
        data = common_data_transaction::get();
        commits = lsq_commit_handler::get();
    endfunction
    static function memblock_backend_cycle_scheduler get();
        if (inst == null) inst = new();
        return inst;
    endfunction

    // 抽象职责：原reset gate取消整个运输单元及地址等待，隔离旧执行代次。
    function void reset_runtime();
        foreach (data.backend_address_wait_q[idx]) begin
            data.backend_fault_address_by_uid[data.backend_address_wait_q[idx]].address_status =
                memblock_backend_cycle_frame_t::CANCELLED_RESET;
        end
        data.backend_address_wait_q.delete();
        data.backend_frame = null;
        data.backend_lsq_transport_q.delete();
        data.backend_redirect_transport_q.delete();
        data.backend_redirect_reserved = 0;
        data.backend_fault_prepared = 0;
        data.backend_redirect_sampled_applied = 0;
        if (auto_fault_intent_id != 0) data.retire_redirect_intent(auto_fault_intent_id);
        auto_fault_intent_id = 0;
        active_flushsb = 0;
        reserved_older_decision = null;
        last_published_valid = 0;
        last_source_sample = 0;
        rob_source_slot = '{default:'0};
        memblock_sync_pkg::reset_backend_cycle_transport(0);
    endfunction

    // 抽象职责：启动时确认共同运输所需的真实driver/sequencer/monitor配置，缺能力立即失败。
    function void check_topology();
        lsqcommit_agent_agent_cfg lsq_cfg;
        redirect_agent_agent_cfg redirect_cfg;
        if (!uvm_config_db#(lsqcommit_agent_agent_cfg)::get(null,"uvm_test_top.env.u_lsqcommit_agent_agent","cfg",lsq_cfg) ||
            !uvm_config_db#(redirect_agent_agent_cfg)::get(null,"uvm_test_top.env.u_redirect_agent_agent","cfg",redirect_cfg)) begin
            `uvm_fatal("BACKEND_FRAME", "backend cycle agent cfg missing")
        end
        if (lsq_cfg.sqr_sw != tcnt_dec_base::ON || lsq_cfg.drv_sw != tcnt_dec_base::ON ||
            lsq_cfg.mon_sw != tcnt_dec_base::ON || redirect_cfg.sqr_sw != tcnt_dec_base::ON ||
            redirect_cfg.drv_sw != tcnt_dec_base::ON || redirect_cfg.mon_sw != tcnt_dec_base::ON) begin
            `uvm_fatal("BACKEND_FRAME", "backend cycle requires both active drivers/sequencers and input monitors")
        end
    endfunction

    // 抽象职责：按公共物理sample推进前级决定来源。未确认的非零候选不绑定历史拍，
    // idle仍推进；来源只在最终确定未来E/S时绑定，已驱动列表不会再次消费。
    function void advance_rob_source_slot(input longint unsigned sample_seq);
        if (sample_seq <= last_source_sample) return;
        // 空闲物理拍也推进，只保留当前前级；非零列表在COMMITTED绑定后只消费一次。
        if (!rob_source_slot.valid || rob_source_slot.source_sample < sample_seq) begin
            rob_source_slot = '{default:'0};
            rob_source_slot.valid = 1;
            rob_source_slot.source_sample = sample_seq;
            rob_source_slot.reset_epoch = memblock_sync_pkg::get_l2tlb_current_reset_epoch();
            rob_source_slot.projection = last_published;
            rob_source_slot.projection.scommit = 0;
            rob_source_slot.projection.redirect_valid = 0;
        end
        last_source_sample = sample_seq;
    endfunction

    // 抽象职责：真实fault到达ROB前沿后先准备身份与intent，解除“先发LSQ才能产生redirect”依赖。
    function void prepare_fault_candidate();
        memblock_uid_t uid;
        memblock_redirect_payload_t redirect;
        if (!memblock_sync_pkg::backend_cycle_mode || data.backend_frame != null) return;
        if (data.backend_fault_prepared) begin
            if (data.get_status(data.backend_prepared_fault_uid).dynamic_epoch != data.backend_prepared_fault_epoch ||
                data.get_status(data.backend_prepared_fault_uid).flushed ||
                data.get_status(data.backend_prepared_fault_uid).issue_killed) begin
                if (auto_fault_intent_id != 0) data.retire_redirect_intent(auto_fault_intent_id);
                auto_fault_intent_id = 0;
                data.backend_fault_prepared = 0;
            end
            return;
        end
        if (!commits.select_fault_head_candidate(uid)) return;
        data.backend_fault_prepared = 1;
        data.backend_prepared_fault_uid = uid;
        data.backend_prepared_fault_key = data.get_status(uid).get_rob_key();
        data.backend_prepared_fault_epoch = data.get_status(uid).dynamic_epoch;
        redirect = '{default:'0};
        redirect.valid = 1;
        redirect.rob_key = data.backend_prepared_fault_key;
        redirect.flush_itself = 1;
        redirect.level = 1;
        if (!data.submit_redirect_intent(uid, redirect, auto_fault_intent_id)) begin
            `uvm_fatal("BACKEND_FRAME", "fault head could not prepare its redirect intent")
        end
    endfunction

    // 抽象职责：独立运输既有Fence排空请求；双边准备和恢复不会阻塞flushSb驱动。
    function void service_flushsb_request();
        if (active_flushsb && memblock_sync_pkg::backend_flushsb_driven) begin
            data.mark_flushsb_request_driver_sendover(flushsb_req, memblock_sync_pkg::get_dispatch_service_cycle());
            active_flushsb = 0;
            memblock_sync_pkg::backend_flushsb_driven = 0;
        end
        if (!active_flushsb && data.try_pop_flushsb_request(flushsb_req)) begin
            data.mark_flushsb_request_attached_to_lsqcommit_xaction(flushsb_req, memblock_sync_pkg::get_dispatch_service_cycle());
            active_flushsb = 1;
            memblock_sync_pkg::backend_flushsb_requested = 1;
        end
        data.warn_flushsb_timeout_if_needed(seq_csr_common::get_flushsb_timeout());
    endfunction

    // 抽象职责：将原commit候选及唯一获选redirect组合成不可变主/尾投影。
    function memblock_backend_cycle_frame_t build_cycle_frame();
        memblock_backend_cycle_frame_t frame;
        lsqcommit_agent_agent_xaction tr;
        memblock_uid_t uids[$];
        memblock_uid_t fault_uid, control_uid;
        bit has_commit, has_fault, has_control;
        frame = new();
        commits.build_lsqcommit_xaction(tr, uids, has_commit, has_fault, fault_uid);
        has_control = !has_commit && !has_fault && commits.select_control_head_candidate(control_uid);
        frame.transport.head = '{default:'0};
        frame.transport.head.pending_flag = tr.io_ooo_to_mem_lsqio_pendingPtr_flag;
        frame.transport.head.pending_value = tr.io_ooo_to_mem_lsqio_pendingPtr_value;
        frame.transport.head.pending_st = tr.io_ooo_to_mem_lsqio_pendingst;
        frame.transport.head.pending_mmio = tr.io_ooo_to_mem_lsqio_pendingMMIOld;
        frame.transport.head.store_exception = commits.latched_is_store_exception;
        frame.transport.head.scommit = tr.io_ooo_to_mem_lsqio_scommit;
        frame.commit_uids = uids;
        frame.has_control = has_control;
        frame.uid = control_uid;
        if (data.backend_redirect_reserved) begin
            frame.has_redirect = 1;
            frame.redirect_event = data.backend_reserved_redirect;
            frame.transport.head.redirect_valid = 1;
            frame.transport.head.redirect_level = frame.redirect_event.redirect.level;
            frame.transport.head.redirect_vls = frame.redirect_event.redirect.is_vls_exception;
            frame.transport.head.redirect_flag = frame.redirect_event.redirect.rob_key.flag;
            frame.transport.head.redirect_value = frame.redirect_event.redirect.rob_key.value;
            frame.rollback = frame.redirect_event.source == MEMBLOCK_WB_EVENT_SOURCE_MEMORY_VIOLATION;
            frame.has_fault = data.backend_fault_prepared &&
                frame.redirect_event.uid == data.backend_prepared_fault_uid &&
                frame.redirect_event.redirect.rob_key == data.backend_prepared_fault_key && !frame.rollback;
            frame.uid = frame.redirect_event.uid;
            // rollback使用已保存且仍合法的前级older列表，未确认运输不被追认为过去周期。
            if (frame.rollback && reserved_older_decision != null) begin
                frame.commit_uids.delete();
                frame.transport.head = reserved_older_decision.transport.head;
                frame.transport.head.scommit = 0;
                foreach (reserved_older_decision.commit_uids[idx]) begin
                    memblock_uid_t older_uid;
                    memblock_op_behavior_t behavior;
                    older_uid = reserved_older_decision.commit_uids[idx];
                    if (older_uid != commits.commit_cursor_uid + idx ||
                        !commits.uid_is_normal_commit_candidate(older_uid) ||
                        data.get_status(older_uid).get_rob_key() != reserved_older_decision.commit_keys[idx] ||
                        data.get_status(older_uid).dynamic_epoch != reserved_older_decision.commit_epochs[idx] ||
                        data.get_status(older_uid).load_issue_epoch != reserved_older_decision.commit_instances[idx].load_issue ||
                        data.get_status(older_uid).sta_issue_epoch != reserved_older_decision.commit_instances[idx].sta_issue ||
                        data.get_status(older_uid).std_issue_epoch != reserved_older_decision.commit_instances[idx].std_issue ||
                        data.get_status(older_uid).replay_seq != reserved_older_decision.commit_instances[idx].replay_seq ||
                        rob_order_util::rob_need_flush(reserved_older_decision.commit_keys[idx], frame.redirect_event.redirect)) break;
                    frame.commit_uids.push_back(older_uid);
                    frame.commit_keys.push_back(reserved_older_decision.commit_keys[idx]);
                    frame.commit_epochs.push_back(reserved_older_decision.commit_epochs[idx]);
                    frame.commit_instances.push_back(reserved_older_decision.commit_instances[idx]);
                    behavior = lsq_ctrl_model::derive_op_behavior(data.get_main_transaction(older_uid));
                    if (memblock_op_behavior_util::is_scalar_rob_store_commit(behavior))
                        frame.transport.head.scommit++;
                end
                // 被覆盖的队首不能继续发布pending，合法older仍保持原冻结指针。
                if (rob_order_util::rob_need_flush(
                        memblock_rob_key_t'({frame.transport.head.pending_flag, frame.transport.head.pending_value}),
                        frame.redirect_event.redirect)) begin
                    frame.transport.head.pending_st = 0;
                    frame.transport.head.pending_mmio = 0;
                end
                frame.transport.head.redirect_valid = 1;
                frame.transport.head.redirect_level = frame.redirect_event.redirect.level;
                frame.transport.head.redirect_vls = frame.redirect_event.redirect.is_vls_exception;
                frame.transport.head.redirect_flag = frame.redirect_event.redirect.rob_key.flag;
                frame.transport.head.redirect_value = frame.redirect_event.redirect.rob_key.value;
            end
            frame.has_control = 0;
            if (frame.has_fault) begin
                frame.commit_uids.delete();
                frame.transport.head.scommit = 0;
                frame.transport.head.pending_st = 0;
                frame.transport.head.pending_mmio = 0;
                frame.transport.head.pending_flag = data.backend_prepared_fault_key.flag;
                frame.transport.head.pending_value = data.backend_prepared_fault_key.value;
                frame.transport.head.store_exception = commits.fault_uid_is_store_exception(frame.uid);
            end
            if (frame.rollback) begin
                memblock_uid_t next_head;
                frame.transport.has_tail = 1;
                frame.transport.tail = frame.transport.head;
                frame.transport.tail.scommit = 0;
                frame.transport.tail.pending_st = 0;
                frame.transport.tail.pending_mmio = 0;
                frame.transport.tail.redirect_valid = 0;
                next_head = commits.commit_cursor_uid;
                if (frame.commit_uids.size() != 0) next_head = frame.commit_uids[frame.commit_uids.size()-1]+1;
                if (next_head < data.main_trans_num && data.get_status(next_head).active) begin
                    memblock_rob_key_t next_key;
                    next_key = data.get_status(next_head).get_rob_key();
                    frame.transport.tail.pending_flag = next_key.flag;
                    frame.transport.tail.pending_value = next_key.value;
                end
            end
        end else if (has_fault) begin
            // 等待原handler仲裁本fault的intent；准备不能提前发布新类别。
            return null;
        end
        if (frame.has_fault || frame.has_control) begin
            status_transaction status;
            status = data.get_status(frame.uid);
            frame.rob_key = status.get_rob_key();
            frame.dynamic_epoch = status.dynamic_epoch;
            frame.issue_epoch = data.is_scalar_store_uid(frame.uid) ? status.sta_issue_epoch : status.load_issue_epoch;
            frame.replay_seq = status.replay_seq;
            frame.exception_vec = status.exception_vec;
        end
        if (frame.has_fault) begin
            memblock_op_class_e op;
            op = data.get_main_transaction(frame.uid).op_class;
            frame.address_status = memblock_backend_cycle_frame_t::NOT_CAPTURED_BY_THIS_PLAN;
            frame.address_required = op inside {MEMBLOCK_OP_CLASS_INT_LOAD, MEMBLOCK_OP_CLASS_FP_LOAD,
                MEMBLOCK_OP_CLASS_STORE, MEMBLOCK_OP_CLASS_PREFETCH, MEMBLOCK_OP_CLASS_CBO};
            frame.address_source = frame.address_required ? memblock_backend_cycle_frame_t::ADDRESS_LSQ :
                (data.is_atomic_uid(frame.uid) ? memblock_backend_cycle_frame_t::ADDRESS_ATOMIC :
                 memblock_backend_cycle_frame_t::ADDRESS_UNKNOWN);
            if (frame.address_required) frame.address_status = memblock_backend_cycle_frame_t::WAITING;
            if (!frame.address_required && !data.is_atomic_uid(frame.uid)) begin
                `uvm_fatal("BACKEND_ADDRESS", "fault route has no defined address source")
            end
            if (data.uid_tlb_record_by_uid.exists(frame.uid) &&
                data.uid_tlb_record_by_uid[frame.uid].csr_snapshot != null) begin
                frame.issue_csr = new();
                frame.issue_csr.copy_from(data.uid_tlb_record_by_uid[frame.uid].csr_snapshot);
            end
        end
        if (frame.commit_keys.size() == 0) foreach (frame.commit_uids[idx]) begin
            frame.commit_keys.push_back(data.get_status(frame.commit_uids[idx]).get_rob_key());
            frame.commit_epochs.push_back(data.get_status(frame.commit_uids[idx]).dynamic_epoch);
            frame.commit_instances.push_back('{
                load_issue:data.get_status(frame.commit_uids[idx]).load_issue_epoch,
                sta_issue:data.get_status(frame.commit_uids[idx]).sta_issue_epoch,
                std_issue:data.get_status(frame.commit_uids[idx]).std_issue_epoch,
                replay_seq:data.get_status(frame.commit_uids[idx]).replay_seq});
        end
        if (!frame.has_redirect && !frame.has_control && frame.commit_uids.size() == 0 &&
            last_published_valid && frame.transport.head == last_published) return null;
        frame.transport.frame_id = ++next_frame_id;
        if (frame.transport.has_tail) frame.transport.tail_id = ++next_frame_id;
        frame.transport.reset_epoch = memblock_sync_pkg::get_l2tlb_current_reset_epoch();
        return frame;
    endfunction

    // 抽象职责：主service最终检查身份并在共同未来E发布；只在COMMITTED激活原owner。
    function void service_frame_publish();
        memblock_backend_cycle_frame_t frame;
        memblock_sync_pkg::backend_cycle_release_t release_state;
        longint unsigned sample_seq;
        if (!memblock_sync_pkg::backend_cycle_mode) return;
        service_flushsb_request();
        sample_seq = memblock_sync_pkg::peek_current_dut_global_sample();
        advance_rob_source_slot(sample_seq);
        commits.sync_modeled_head_after_fault_terminal();
        frame = data.backend_frame;
        if (frame != null) begin
            release_state = memblock_sync_pkg::get_backend_release(frame.transport.frame_id);
            if (release_state.canceled) begin
                if (release_state.finished == 2'b11) begin
                    memblock_sync_pkg::backend_release_by_id.delete(frame.transport.frame_id);
                    data.backend_frame = null;
                end
                return;
            end
            if (release_state.committed) return;
            if ((data.backend_redirect_reserved && (!frame.has_redirect ||
                    data.backend_reserved_redirect.redirect_intent_id != frame.redirect_event.redirect_intent_id)) ||
                !commits.check_backend_cycle_commit_eligibility(frame)) begin
                if (!frame.has_redirect && frame.commit_uids.size() != 0 &&
                    commits.check_backend_cycle_commit_eligibility(frame)) reserved_older_decision = frame;
                else reserved_older_decision = null;
                memblock_sync_pkg::backend_release_by_id[frame.transport.frame_id].canceled = 1;
                return;
            end
            if (release_state.ready != 2'b11) return;
            if (!memblock_sync_pkg::backend_lsq_worker_online || !memblock_sync_pkg::backend_redirect_worker_online ||
                memblock_sync_pkg::backend_lsq_watermark != sample_seq ||
                memblock_sync_pkg::backend_redirect_watermark != sample_seq ||
                memblock_sync_pkg::backend_lsq_sample_time != memblock_sync_pkg::backend_redirect_sample_time) begin
                `uvm_fatal("BACKEND_FRAME", "both workers and monitors must share the current physical sample")
            end
            if (frame.has_redirect) begin
                data.request_redirect_flush(frame.redirect_event.redirect);
                data.backend_redirect_sampled_applied = 0;
                data.backend_redirect_reserved = 0;
                if (frame.redirect_event.source == MEMBLOCK_WB_EVENT_SOURCE_REDIRECT) begin
                    data.mark_redirect_intent_accepted(frame.redirect_event.redirect_intent_id);
                    if (auto_fault_intent_id == frame.redirect_event.redirect_intent_id) begin
                        void'(data.consume_redirect_intent_accepted(auto_fault_intent_id));
                        auto_fault_intent_id = 0;
                    end
                end
            end
            frame.approved = 1;
            frame.input_sample = sample_seq + 2;
            frame.lsq_source_seq = frame.input_sample-1;
            // 候选在最终共同边沿确定后进入唯一前级槽，再恰好消费到本发布单元。
            // prepared运输等待期间不把未驱动候选冒充过去的物理决定。
            rob_source_slot.valid = 1;
            rob_source_slot.consumed = 0;
            rob_source_slot.source_sample = frame.lsq_source_seq;
            rob_source_slot.reset_epoch = frame.transport.reset_epoch;
            rob_source_slot.reservation_id = frame.transport.frame_id;
            rob_source_slot.projection = frame.transport.head;
            frame.transport.head = rob_source_slot.projection;
            frame.lsq_source_seq = rob_source_slot.source_sample;
            rob_source_slot.consumed = 1;
            memblock_sync_pkg::backend_input_required_sample = frame.input_sample;
            if (frame.address_required) begin
                frame.target_sample = frame.input_sample+2;
                memblock_sync_pkg::backend_address_required_sample = frame.target_sample;
            end
            frame.redirect_source_seq = frame.rollback ? frame.input_sample : frame.lsq_source_seq;
            memblock_sync_pkg::backend_release_by_id[frame.transport.frame_id].edge_sample = sample_seq+1;
            memblock_sync_pkg::backend_release_by_id[frame.transport.frame_id].committed = 1;
            reserved_older_decision = null;
            return;
        end
        if (data.active_redirect.valid || data.redirect_deleted_owner_window_pending() || data.is_global_stop_requested()) return;
        frame = build_cycle_frame();
        if (frame == null) return;
        data.backend_frame = frame;
        memblock_sync_pkg::prepare_backend_transport(frame.transport.frame_id);
        data.backend_lsq_transport_q.push_back(frame.transport);
        data.backend_redirect_transport_q.push_back(frame.transport);
    endfunction

    // 抽象职责：配对同S的两路实际输入，应用业务记账后才允许原deq/cancel服务。
    function void service_backend_cycle_samples();
        memblock_backend_cycle_frame_t frame;
        memblock_sync_pkg::backend_cycle_release_t release_state;
        memblock_sync_pkg::backend_cycle_projection_t actual, expected;
        longint unsigned sample_seq;
        bit is_tail;
        data.service_backend_fault_address_samples();
        frame = data.backend_frame;
        if (frame == null) return;
        release_state = memblock_sync_pkg::get_backend_release(frame.transport.frame_id);
        if (!release_state.committed) return;
        if (release_state.driven == 2'b11 && frame.has_redirect && !frame.drive_acknowledged) begin
            data.mark_redirect_drive_done(frame.redirect_event.redirect);
            frame.drive_acknowledged = 1;
        end
        is_tail = frame.applied && frame.transport.has_tail;
        sample_seq = frame.input_sample + (is_tail ? 1 : 0);
        if (memblock_sync_pkg::backend_lsq_watermark < sample_seq ||
            memblock_sync_pkg::backend_redirect_watermark < sample_seq) return;
        if (!memblock_sync_pkg::backend_lsq_input_by_sample.exists(sample_seq) ||
            !memblock_sync_pkg::backend_redirect_input_by_sample.exists(sample_seq)) begin
            `uvm_fatal("BACKEND_FRAME", "required backend input sample missing from bounded history")
        end
        actual = memblock_sync_pkg::backend_lsq_input_by_sample[sample_seq].value |
                 memblock_sync_pkg::backend_redirect_input_by_sample[sample_seq].value;
        expected = is_tail ? frame.transport.tail : frame.transport.head;
        if (actual !== expected ||
            memblock_sync_pkg::backend_lsq_input_by_sample[sample_seq].reset_epoch != frame.transport.reset_epoch ||
            memblock_sync_pkg::backend_redirect_input_by_sample[sample_seq].reset_epoch != frame.transport.reset_epoch) begin
            `uvm_fatal("BACKEND_FRAME", $sformatf("backend input mismatch frame=%0d tail=%0d sample=%0d actual=%h expected=%h", frame.transport.frame_id, is_tail, sample_seq, actual, expected))
        end
        if (!frame.applied) begin
            commits.apply_sampled_backend_cycle(frame);
            last_published = frame.transport.head;
            last_published.scommit = 0;
            last_published.redirect_valid = 0;
            last_published_valid = 1;
            if (frame.transport.has_tail) begin
                memblock_sync_pkg::backend_input_required_sample = frame.input_sample+1;
                return;
            end
        end else if (frame.transport.has_tail) begin
            last_published = frame.transport.tail;
        end
        if (release_state.finished != 2'b11) return;
        memblock_sync_pkg::backend_release_by_id.delete(frame.transport.frame_id);
        memblock_sync_pkg::backend_input_required_sample = 0;
        data.backend_frame = null;
    endfunction
endclass
`endif
