//=========================================================
//File name    : memblock_lsqenq_dispatch_base_sequence.sv
//Author       : OpenAI_Codex
//Module name  : memblock_lsqenq_dispatch_base_sequence
//Discribution : LSQ enqueue admission driver sequence
//Date         : 2026-05-19
//=========================================================
`ifndef MEMBLOCK_LSQENQ_DISPATCH_BASE_SEQUENCE__SV
`define MEMBLOCK_LSQENQ_DISPATCH_BASE_SEQUENCE__SV

class memblock_lsqenq_dispatch_base_sequence extends lsqenq_agent_agent_default_sequence;

    common_data_transaction data;
    lsq_ctrl_model          lsq_ctrl;
    issue_queue_scheduler   issue_sched;
    dispatch_monitor_event_adapter monitor_adapter;

    bit          enable;
    int unsigned no_progress_warn_cycles;
    // 已launch并预留资源、但尚未跨过下一driver采样边界的单深度batch；confirm写入，sample helper清理。
    bit          pending_sample_valid;
    memblock_lsq_reservation_token_t pending_sample_tokens[$];
    int unsigned pending_sample_flush_epoch;
    longint unsigned pending_sample_launch_cycle;
    longint unsigned pending_sample_target_sample_seq;
    // pending reservation 与 launch 时的 reset/baseline 绑定；reset 只取消，
    // 不把尚未到达 target 的 token 标记为 DUT_VISIBLE。
    int unsigned pending_sample_control_reset_epoch;
    int unsigned pending_sample_shadow_generation;
    // Lifecycle only: 0=no token, 1=token lives in shared D/R/Q,
    // 2=the shared-Q fire offer awaits its source-edge shadow settlement.
    int unsigned             atomic_token_stage;
    memblock_uid_t           atomic_token_uid;
    main_control_transaction atomic_token_tr;
    memblock_op_behavior_t   atomic_token_behavior;
    // 中文注释：pre-fire token 绑定创建时的 control reset epoch 与 shadow
    // baseline generation。任一代际变化都必须丢弃 token，不能把 reset 清 Q
    // 误判成一次成功的 atomic Q fire。
    int unsigned             atomic_token_control_reset_epoch;
    int unsigned             atomic_token_shadow_generation;
    longint unsigned         atomic_token_target_sample_seq;

    `uvm_object_utils(memblock_lsqenq_dispatch_base_sequence)

    extern function new(string name = "memblock_lsqenq_dispatch_base_sequence");
    extern virtual task pre_body();
    extern virtual task body();
    extern virtual task drive_lsqenq_loop();
    extern virtual task send_lsqenq_cycle(input int unsigned cycle_idx,
                                          output bit has_progress);
    extern virtual task send_idle_lsqenq_boundary(input int unsigned cycle_idx,
                                                  input string reason,
                                                  inout bit has_progress);
    extern task wait_or_cancel_pending_sample();
    extern function void configure_from_plus();
    extern function void ensure_helpers();
    extern function void drain_csr_runtime_events();
    extern function void apply_pending_lsq_cancels();
    extern task wait_for_main_table();
    extern function bit admission_blocked_by_flush();
    extern function bit lsq_allocating_admission_blocked();
    extern function bit next_uid_needs_lsq_admission(output memblock_uid_t uid,
                                                     output main_control_transaction main_tr,
                                                     output memblock_op_behavior_t behavior);
    extern function bit collect_lsq_candidates(output memblock_uid_t uids[$],
                                               output int unsigned slots[$],
                                               output main_control_transaction trs[$],
                                               output memblock_op_behavior_t behaviors[$],
                                               output memblock_lq_key_t lq_keys[$],
                                               output memblock_sq_key_t sq_keys[$]);
    extern function void check_cbo_lsq_behavior(input main_control_transaction main_tr,
                                                input memblock_op_behavior_t behavior,
                                                input string caller);
    extern function void clear_lsqenq_xaction(input lsqenq_agent_agent_xaction tr);
    extern function void assign_lsqenq_slot(input lsqenq_agent_agent_xaction tr,
                                            input int unsigned slot,
                                            input main_control_transaction main_tr,
                                            input memblock_op_behavior_t behavior,
                                            input memblock_lq_key_t lq_key,
                                            input memblock_sq_key_t sq_key);
    extern function void set_need_alloc(input lsqenq_agent_agent_xaction tr,
                                        input int unsigned slot,
                                        input bit [1:0] need_alloc);
    extern function void set_req_fields(input lsqenq_agent_agent_xaction tr,
                                        input int unsigned slot,
                                        input bit valid,
                                        input main_control_transaction main_tr,
                                        input memblock_op_behavior_t behavior,
                                        input memblock_lq_key_t lq_key,
                                        input memblock_sq_key_t sq_key);
    extern function void confirm_lsq_candidates(input lsqenq_agent_agent_xaction tr,
                                                input memblock_uid_t uids[$],
                                                input int unsigned slots[$],
                                                input main_control_transaction trs[$],
                                                input memblock_op_behavior_t behaviors[$],
                                                input memblock_lq_key_t lq_keys[$],
                                                input memblock_sq_key_t sq_keys[$],
                                                inout bit has_progress);
    extern function void complete_v2_pending_sample(inout bit has_progress);
    extern function void clear_v2_pending_sample();
    extern function void complete_admission(input memblock_uid_t uid);
    extern function bit admit_control_marker_if_ready(output bit has_progress);
    extern function bit admit_non_lsq_if_ready(output bit has_progress);
    extern function bit service_atomic_admission_token(output bit has_progress);
    extern function void configure_source_offer_metadata(
        input lsqenq_agent_agent_xaction tr,
        input bit [MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM-1:0] fire_mask,
        input bit need_alloc_nonzero,
        input int unsigned lq_alloc,
        input int unsigned sq_alloc,
        input string reason);
    extern function void cancel_v2_pending_sample_on_reset(input string reason);
    extern function bit pending_sample_reset_binding_invalid();
    extern function void clear_pre_fire_atomic_token(input string reason);
    extern function bit atomic_token_reset_binding_invalid();
    extern function void publish_sequence_local_state();

endclass:memblock_lsqenq_dispatch_base_sequence

function memblock_lsqenq_dispatch_base_sequence::new(string name = "memblock_lsqenq_dispatch_base_sequence");
    super.new(name);
    enable = 1'b0;
    no_progress_warn_cycles = 10000;
    pending_sample_valid = 1'b0;
    pending_sample_flush_epoch = 0;
    pending_sample_launch_cycle = 0;
    pending_sample_target_sample_seq = 0;
    pending_sample_control_reset_epoch = 0;
    pending_sample_shadow_generation = 0;
    atomic_token_stage = 0;
    atomic_token_uid = 0;
    atomic_token_tr = null;
    atomic_token_behavior = lsq_ctrl_model::make_default_behavior();
    atomic_token_control_reset_epoch = 0;
    atomic_token_shadow_generation = 0;
    atomic_token_target_sample_seq = 0;
endfunction:new

task memblock_lsqenq_dispatch_base_sequence::pre_body();
    super.pre_body();
endtask:pre_body

task memblock_lsqenq_dispatch_base_sequence::body();
    seq_csr_common::init();
    configure_from_plus();
    if (!enable) begin
        `uvm_info(get_type_name(), "MEMBLOCK_LSQENQ_SEQ_EN=0, LSQ enqueue dispatch sequence stays idle", UVM_LOW)
        return;
    end
    ensure_helpers();
    wait_for_main_table();
    memblock_sync_pkg::set_lsq_source_offer_publish_en(1'b1);
    drive_lsqenq_loop();
endtask:body

task memblock_lsqenq_dispatch_base_sequence::drive_lsqenq_loop();
    int unsigned idle_count;
    int unsigned cycle_idx;

    idle_count = 0;
    cycle_idx = 0;
    forever begin
        bit has_progress;

        has_progress = 1'b0;
        if (data.is_global_stop_prepare_requested() && !data.is_global_stop_requested()) begin
            memblock_sync_pkg::set_lsq_source_offer_publish_en(1'b0);
            wait_or_cancel_pending_sample();
            if (pending_sample_valid) complete_v2_pending_sample(has_progress);
            #1;
            continue;
        end
        if (data.is_global_stop_requested()) begin
            has_progress = 1'b0;
            memblock_sync_pkg::set_lsq_source_offer_publish_en(1'b0);
            wait_or_cancel_pending_sample();
            if (pending_sample_valid) complete_v2_pending_sample(has_progress);
            `uvm_info(get_type_name(),
                      $sformatf("stop LSQ enqueue loop by global_stop_requested at cycle=%0d",
                                cycle_idx),
                      UVM_LOW)
            break;
        end
        // terminal 后先停止生产新的 idle offer，让最后一个 target 自然结算；
        // 否则永久新增的 idle FIFO 项会反过来阻止公共 runtime drain。
        if (data.transaction_done() && !pending_sample_valid &&
            atomic_token_stage == 0 && lsq_ctrl.rtl_pipe_empty() &&
            !data.has_pending_lsq_cancel_apply()) begin
            memblock_sync_pkg::set_lsq_source_offer_publish_en(1'b0);
            #1;
            continue;
        end
        // global-stop prepare 可能因新的 runtime work 被撤销。恢复前先让暂停前的
        // offer 和 D/R/Q shadow 自然结算，再从下一拍建立新的连续发布窗口。
        if (!memblock_sync_pkg::get_lsq_source_offer_publish_en() &&
            (memblock_sync_pkg::lsq_source_offer_queue_size() != 0 ||
             lsq_ctrl.shadow_source_offer_valid || !lsq_ctrl.rtl_pipe_empty())) begin
            #1;
            continue;
        end
        memblock_sync_pkg::set_lsq_source_offer_publish_en(1'b1);
        send_lsqenq_cycle(cycle_idx, has_progress);
        cycle_idx++;
        if (has_progress) begin
            idle_count = 0;
        end else begin
            idle_count++;
            if (no_progress_warn_cycles != 0 &&
                idle_count >= no_progress_warn_cycles) begin
                `uvm_warning(get_type_name(),
                             $sformatf("no LSQ enqueue progress for %0d cycles: cycle=%0d terminal_done_uid=%0d main_trans_num=%0d",
                                       idle_count,
                                       cycle_idx,
                                       data.dispatch_progress.terminal_done_uid,
                                       data.main_trans_num))
                idle_count = 0;
            end
        end
    end
endtask:drive_lsqenq_loop

task memblock_lsqenq_dispatch_base_sequence::send_lsqenq_cycle(input int unsigned cycle_idx,
                                                          output bit has_progress);
    lsqenq_agent_agent_xaction tr;
    memblock_uid_t            uids[$];
    int unsigned              slots[$];
    main_control_transaction  trs[$];
    memblock_op_behavior_t    behaviors[$];
    memblock_lq_key_t         lq_keys[$];
    memblock_sq_key_t         sq_keys[$];
    bit                       admission_progress;

    has_progress = 1'b0;
    wait_or_cancel_pending_sample();
    if (pending_sample_valid) complete_v2_pending_sample(has_progress);
    apply_pending_lsq_cancels();
    admission_progress = 1'b0;
    if (admit_control_marker_if_ready(admission_progress)) begin
        has_progress |= admission_progress;
        return;
    end
    if (service_atomic_admission_token(admission_progress)) begin
        has_progress |= admission_progress;
        if (memblock_sync_pkg::reset_backend_done === 1'b1 &&
            lsq_ctrl.shadow_baseline_valid) begin
            send_idle_lsqenq_boundary(cycle_idx, "atomic D/R/Q admission token", has_progress);
        end
        return;
    end
    if (admit_non_lsq_if_ready(admission_progress)) begin
        has_progress |= admission_progress;
        return;
    end
    if (!collect_lsq_candidates(uids, slots, trs, behaviors, lq_keys, sq_keys)) begin
        send_idle_lsqenq_boundary(cycle_idx, "no LSQ candidate", has_progress);
        return;
    end

    tr = lsqenq_agent_agent_xaction::type_id::create($sformatf("lsqenq_dispatch_tr_%0d", cycle_idx));
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "failed to create lsqenq xaction")
    end
    clear_lsqenq_xaction(tr);
    tr.memblock_dispatch_wait_can_accept = 1'b0;
    tr.memblock_dispatch_ready_timeout = 0;
    tr.memblock_dispatch_aborted_by_redirect = 1'b0;
    tr.memblock_dispatch_request_launched = 1'b0;
    tr.memblock_dispatch_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
    tr.memblock_source_offer_metadata_valid = 1'b0;
    tr.memblock_source_offer_published = 1'b0;
    tr.pre_pkt_gap = 0;
    tr.post_pkt_gap = 0;
    foreach (uids[idx]) begin
        assign_lsqenq_slot(tr, slots[idx], trs[idx], behaviors[idx], lq_keys[idx], sq_keys[idx]);
    end
    begin
        int unsigned accepted_lq;
        int unsigned accepted_sq;
        bit [MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM-1:0] fire_mask;

        accepted_lq = 0;
        accepted_sq = 0;
        fire_mask = '0;
        foreach (behaviors[idx]) begin
            if (slots[idx] >= MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM || fire_mask[slots[idx]]) begin
                `uvm_fatal(get_type_name(), "LSQ fire slots are invalid or duplicated")
            end
            fire_mask[slots[idx]] = 1'b1;
            if (behaviors[idx].uses_lq) accepted_lq += behaviors[idx].num_ls_elem;
            if (behaviors[idx].uses_sq) accepted_sq += behaviors[idx].num_ls_elem;
        end
        configure_source_offer_metadata(tr, fire_mask, 1'b1,
                                        accepted_lq, accepted_sq,
                                        "active LSQ launch");
    end

    start_item(tr);
    finish_item(tr);
    confirm_lsq_candidates(tr, uids, slots, trs, behaviors, lq_keys, sq_keys, has_progress);
endtask:send_lsqenq_cycle

task memblock_lsqenq_dispatch_base_sequence::send_idle_lsqenq_boundary(input int unsigned cycle_idx,
                                                                  input string reason,
                                                                  inout bit has_progress);
    lsqenq_agent_agent_xaction tr;

    tr = lsqenq_agent_agent_xaction::type_id::create($sformatf("lsqenq_dispatch_idle_tr_%0d", cycle_idx));
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "failed to create idle lsqenq xaction")
    end
    clear_lsqenq_xaction(tr);
    tr.memblock_dispatch_wait_can_accept = 1'b0;
    tr.memblock_dispatch_ready_timeout = 0;
    tr.memblock_dispatch_aborted_by_redirect = 1'b0;
    tr.memblock_dispatch_request_launched = 1'b0;
    tr.memblock_dispatch_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
    tr.memblock_source_offer_metadata_valid = 1'b0;
    tr.memblock_source_offer_published = 1'b0;
    tr.pre_pkt_gap = 0;
    tr.post_pkt_gap = 0;
    `uvm_info(get_type_name(), $sformatf("send LSQ idle boundary: %s", reason), UVM_DEBUG)
    begin
        bit [MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM-1:0] fire_mask;

        fire_mask = '0;
        if (atomic_token_stage == 2 && atomic_token_target_sample_seq == 0) begin
            if (!lsq_ctrl.shadow_q_valid[0] ||
                !lsq_ctrl.shadow_q_is_atomic[0] ||
                lsq_ctrl.shadow_q_uid[0] != atomic_token_uid) begin
                `uvm_fatal(get_type_name(), "atomic Q fire offer lost its shared lane0 token")
            end
            fire_mask[0] = 1'b1;
        end
        configure_source_offer_metadata(tr, fire_mask, 1'b0, 0, 0, reason);
    end
    start_item(tr);
    finish_item(tr);
    if (tr.memblock_dispatch_request_launched ||
        tr.memblock_dispatch_aborted_by_redirect) begin
        `uvm_fatal(get_type_name(), "idle LSQ cycle returned active launch/redirect state")
    end
    if (!tr.memblock_source_offer_published ||
        tr.memblock_source_offer_target_sample_seq == 0 ||
        tr.memblock_source_offer_dut_sample_seq != tr.memblock_source_offer_target_sample_seq + 1) begin
        `uvm_fatal(get_type_name(), "idle LSQ cycle has no driver-bound source offer")
    end
    if (atomic_token_stage == 2 && atomic_token_target_sample_seq == 0) begin
        atomic_token_target_sample_seq = tr.memblock_source_offer_target_sample_seq;
        publish_sequence_local_state();
    end
endtask:send_idle_lsqenq_boundary

task memblock_lsqenq_dispatch_base_sequence::wait_or_cancel_pending_sample();
    int unsigned wait_count;

    ensure_helpers();
    wait_count = 0;
    while (pending_sample_valid &&
           lsq_ctrl.shadow_last_sample_seq < pending_sample_target_sample_seq) begin
        if (pending_sample_reset_binding_invalid()) begin
            cancel_v2_pending_sample_on_reset("backend reset before reservation sample");
            return;
        end
        #1;
        wait_count++;
        if (no_progress_warn_cycles != 0 &&
            (wait_count % no_progress_warn_cycles) == 0) begin
            `uvm_warning(get_type_name(),
                         $sformatf("waiting for pending reservation target sample=%0d current shadow sample=%0d",
                                   pending_sample_target_sample_seq,
                                   lsq_ctrl.shadow_last_sample_seq))
        end
    end
    if (pending_sample_reset_binding_invalid()) begin
        cancel_v2_pending_sample_on_reset("backend reset at reservation sample boundary");
    end
endtask:wait_or_cancel_pending_sample

function void memblock_lsqenq_dispatch_base_sequence::configure_from_plus();
    enable = seq_csr_common::get_lsqenq_seq_en();
    no_progress_warn_cycles = seq_csr_common::get_active_seq_no_progress_warn_cycles();
endfunction:configure_from_plus

function void memblock_lsqenq_dispatch_base_sequence::ensure_helpers();
    data = common_data_transaction::get();
    lsq_ctrl = lsq_ctrl_model::get();
    if (issue_sched == null) begin
        issue_sched = issue_queue_scheduler::type_id::create("issue_sched");
    end
    if (monitor_adapter == null) begin
        monitor_adapter = dispatch_monitor_event_adapter::type_id::create("monitor_adapter");
    end
    if (data == null || lsq_ctrl == null || issue_sched == null ||
        monitor_adapter == null) begin
        `uvm_fatal(get_type_name(), "failed to initialize LSQ enqueue dispatch helpers")
    end
    publish_sequence_local_state();
endfunction:ensure_helpers

function void memblock_lsqenq_dispatch_base_sequence::publish_sequence_local_state();
    if (data != null) begin
        data.publish_lsq_sequence_local_state(pending_sample_valid,
                                              atomic_token_stage != 0);
    end
endfunction:publish_sequence_local_state

function void memblock_lsqenq_dispatch_base_sequence::apply_pending_lsq_cancels();
    ensure_helpers();
    foreach (data.cancel_record_q[idx]) begin
        int unsigned lq_count;
        int unsigned sq_count;
        int unsigned redirect_epoch;

        if (!data.cancel_record_q[idx].valid ||
            data.cancel_record_q[idx].software_applied) begin
            continue;
        end
        if (!data.cancel_record_q[idx].software_count_finalized) begin
            break;
        end
        // fault SQ cancel count is decided by the target DUT snapshot.  Do
        // not apply a provisional zero-count record before that observation.
        if (data.fault_sq_candidate_snapshot_pending(
                data.cancel_record_q[idx].redirect_epoch)) begin
            break;
        end
        lq_count = data.cancel_record_q[idx].software_cancel_lq_count;
        sq_count = data.cancel_record_q[idx].software_cancel_sq_count;
        redirect_epoch = data.cancel_record_q[idx].redirect_epoch;
        // 中文伪代码：software count 既是资源回退量也是 DUT compare 期望值；
        // observed count 只核对，不再次调用 cancel_lq/cancel_sq。
        if (lq_count != 0) begin
            lsq_ctrl.cancel_lq(lq_count);
            if (data.pending_lq_cancel_count < lq_count) begin
                `uvm_fatal(get_type_name(), "pending LQ cancel aggregate underflow")
            end
            data.pending_lq_cancel_count -= lq_count;
        end
        if (sq_count != 0) begin
            lsq_ctrl.cancel_sq(sq_count);
            if (data.pending_sq_cancel_count < sq_count) begin
                `uvm_fatal(get_type_name(), "pending SQ cancel aggregate underflow")
            end
            data.pending_sq_cancel_count -= sq_count;
        end
        data.mark_cancel_record_applied(redirect_epoch);
    end
    data.check_cancel_pending_aggregate();
endfunction:apply_pending_lsq_cancels

function void memblock_lsqenq_dispatch_base_sequence::drain_csr_runtime_events();
    ensure_helpers();
    monitor_adapter.drain_csr_events();
endfunction:drain_csr_runtime_events

task memblock_lsqenq_dispatch_base_sequence::wait_for_main_table();
    int unsigned wait_count;

    wait_count = 0;
    while (!data.main_table_ready) begin
        if (no_progress_warn_cycles != 0 &&
            wait_count != 0 &&
            (wait_count % no_progress_warn_cycles) == 0) begin
            `uvm_warning(get_type_name(),
                         $sformatf("still waiting for main table before LSQ enqueue admission: wait_count=%0d main_trans_num=%0d next_uid=%0d",
                                   wait_count,
                                   data.main_trans_num,
                                   data.next_uid))
        end
        #1;
        wait_count++;
    end
endtask:wait_for_main_table

function bit memblock_lsqenq_dispatch_base_sequence::admission_blocked_by_flush();
    ensure_helpers();
    return data.issue_blocked_by_global_flush();
endfunction:admission_blocked_by_flush

// 中文注释：只阻止需要分配物理 LQ/SQ key 的新 launch。redirect 后 software
// allocator 会先按 cancel record 回退，DUT shadow pointer 到 t3 才回退；两者未
// 收敛时不得生成新 key。control、non-LSQ 和已 launch pending sample 保持原语义。
function bit memblock_lsqenq_dispatch_base_sequence::lsq_allocating_admission_blocked();
    ensure_helpers();
    return admission_blocked_by_flush() ||
           data.redirect_deleted_owner_window_pending() ||
           lsq_ctrl.lq_enq_ptr != lsq_ctrl.shadow_lq_enq_ptr ||
           lsq_ctrl.sq_enq_ptr != lsq_ctrl.shadow_sq_enq_ptr;
endfunction:lsq_allocating_admission_blocked

function bit memblock_lsqenq_dispatch_base_sequence::next_uid_needs_lsq_admission(output memblock_uid_t uid,
                                                                             output main_control_transaction main_tr,
                                                                             output memblock_op_behavior_t behavior);
    ensure_helpers();
    uid = 0;
    main_tr = null;
    behavior = lsq_ctrl_model::make_default_behavior();
    if (admission_blocked_by_flush()) begin
        return 1'b0;
    end
    uid = data.get_next_new_admit_uid();
    if (uid < data.main_trans_num) begin
        status_transaction status;

        main_tr = data.get_main_transaction(uid);
        if (data.control_barrier_blocks_admission(uid) ||
            is_control_op_class(main_tr.op_class)) begin
            return 1'b0;
        end
        status = data.get_status(uid);
        behavior = lsq_ctrl_model::derive_op_behavior(main_tr);
        if (status.terminal_done || status.active || status.enq ||
            status.exception_pending || status.replay_pending) begin
            return 1'b0;
        end
        if (!data.rob_admission_window_has_room(uid, "next_uid_needs_lsq_admission")) begin
            return 1'b0;
        end
        // redirect_pending/flushed表示旧动态实例已被kill；同uid现在允许按公共高水位重新admission。
        return 1'b1;
    end
    return 1'b0;
endfunction:next_uid_needs_lsq_admission

// 中文注释：CBO 必须作为普通 scalar-store lane 复用公共 D/R/Q 和 SQ owner。
// 本检查只防止分类漂移，不读取 credit、canAccept 或 free count，也不建立专属 admission 状态。
function void memblock_lsqenq_dispatch_base_sequence::check_cbo_lsq_behavior(
    input main_control_transaction main_tr,
    input memblock_op_behavior_t behavior,
    input string caller
);
    if (main_tr == null || main_tr.op_class != MEMBLOCK_OP_CLASS_CBO) begin
        return;
    end
    if (main_tr.fuType != MEMBLOCK_FUTYPE_STU ||
        main_tr.lsq_flow != MEMBLOCK_LSQ_FLOW_CBO ||
        !lsq_ctrl_model::is_cbo_fuoptype(main_tr.fuOpType) ||
        behavior.kind != MEMBLOCK_OP_BEHAVIOR_CBO ||
        !behavior.is_cbo || behavior.is_atomic ||
        behavior.need_alloc != 2'b10 ||
        behavior.uses_lq || !behavior.uses_sq ||
        behavior.num_ls_elem != memblock_num_ls_elem_t'(1)) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("%s CBO classification drift uid=%0d fuType=0x%0h fuOpType=0x%0h flow=%0d needAlloc=%0b uses_lq/sq=%0b/%0b numLsElem=%0d",
                             caller, main_tr.uid, main_tr.fuType, main_tr.fuOpType,
                             main_tr.lsq_flow, behavior.need_alloc,
                             behavior.uses_lq, behavior.uses_sq,
                             behavior.num_ls_elem))
    end
endfunction:check_cbo_lsq_behavior

function bit memblock_lsqenq_dispatch_base_sequence::collect_lsq_candidates(output memblock_uid_t uids[$],
                                                                       output int unsigned slots[$],
                                                                       output main_control_transaction trs[$],
                                                                       output memblock_op_behavior_t behaviors[$],
                                                                       output memblock_lq_key_t lq_keys[$],
                                                                       output memblock_sq_key_t sq_keys[$]);
    int unsigned max_enq;
    memblock_lq_key_t lq_tmp;
    memblock_sq_key_t sq_tmp;
    int unsigned load_elem_count;
    int unsigned store_elem_count;

    uids.delete();
    slots.delete();
    trs.delete();
    behaviors.delete();
    lq_keys.delete();
    sq_keys.delete();
    if (admission_blocked_by_flush() || !lsq_ctrl.shadow_baseline_valid) begin
        return 1'b0;
    end
    max_enq = seq_csr_common::get_enq_per_cycle();
    if (max_enq == 0) begin
        return 1'b0;
    end

    // Existing Q state is authoritative.  Preserve physical lane numbers and
    // holes; the first blocked valid lane stops the complete younger prefix.
    if (lsq_ctrl.rtl_q_has_valid()) begin
        bit prefix_blocked;

        if (!lsq_ctrl.rtl_admission_ready() || lsq_ctrl.shadow_iq_recompute_hold ||
            lsq_allocating_admission_blocked()) begin
            return 1'b0;
        end
        lq_tmp = lsq_ctrl.shadow_lq_enq_ptr;
        sq_tmp = lsq_ctrl.shadow_sq_enq_ptr;
        load_elem_count = 0;
        store_elem_count = 0;
        prefix_blocked = 1'b0;
        for (int unsigned lane = 0; lane < MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM; lane++) begin
            memblock_uid_t uid;
            main_control_transaction main_tr;
            memblock_op_behavior_t behavior;
            status_transaction status;
            int unsigned tentative_load;
            int unsigned tentative_store;
            bit lane_allowed;

            if (!lsq_ctrl.shadow_q_valid[lane]) begin
                continue;
            end
            if (prefix_blocked || uids.size() >= max_enq) begin
                break;
            end
            uid = lsq_ctrl.shadow_q_uid[lane];
            main_tr = data.get_main_transaction(uid);
            status = data.get_status(uid);
            behavior = lsq_ctrl_model::derive_op_behavior(main_tr);
            check_cbo_lsq_behavior(main_tr, behavior, "collect_lsq_candidates_q");
            if (is_control_op_class(main_tr.op_class) || behavior.need_alloc == 2'b00 ||
                behavior.num_ls_elem != memblock_num_ls_elem_t'(1) ||
                status.terminal_done || status.active || status.enq ||
                status.exception_pending || status.replay_pending ||
                data.control_barrier_blocks_admission(uid) ||
                !data.rob_admission_window_has_room(uid, "collect_lsq_candidates_q")) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("invalid retained LSQ Q lane=%0d uid=%0d needAlloc=%0b active/enq=%0b/%0b",
                                     lane, uid, behavior.need_alloc, status.active, status.enq))
            end
            if (behavior.uses_lq != lsq_ctrl.shadow_q_is_load[lane] ||
                behavior.uses_sq != lsq_ctrl.shadow_q_is_store[lane]) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("Q lane category drift lane=%0d uid=%0d", lane, uid))
            end
            tentative_load = load_elem_count + (behavior.uses_lq ? 1 : 0);
            tentative_store = store_elem_count + (behavior.uses_sq ? 1 : 0);
            lane_allowed = tentative_load <= MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH &&
                           tentative_store <= MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH;
            if (behavior.uses_lq) begin
                lane_allowed &= lsq_ctrl.shadow_lq_credit >
                                lsq_ctrl.shadow_conserve_flow_total[lane];
            end
            if (behavior.uses_sq) begin
                lane_allowed &= lsq_ctrl.shadow_sq_credit >
                                lsq_ctrl.shadow_conserve_flow_total[lane];
            end
            if (!lane_allowed) begin
                prefix_blocked = 1'b1;
                break;
            end

            uids.push_back(uid);
            slots.push_back(lane);
            trs.push_back(main_tr);
            behaviors.push_back(behavior);
            lq_keys.push_back(lq_tmp);
            sq_keys.push_back(sq_tmp);
            load_elem_count = tentative_load;
            store_elem_count = tentative_store;
            if (behavior.uses_lq) begin
                lq_tmp = lsq_ctrl_model::advance_lq_key(lq_tmp, 1);
            end
            if (behavior.uses_sq) begin
                sq_tmp = lsq_ctrl_model::advance_sq_key(sq_tmp, 1);
            end
        end
        return uids.size() != 0;
    end

    // No Q offer is ready.  Only an entirely empty D/R/Q pipe may capture the
    // next ordered source bundle.  Capture does not allocate software LSQ state.
    if (!lsq_ctrl.rtl_pipe_empty() || lsq_allocating_admission_blocked()) begin
        return 1'b0;
    end
    begin
        memblock_uid_t staged_uids[$];
        memblock_op_behavior_t staged_behaviors[$];
        memblock_uid_t base_uid;

        base_uid = data.get_next_new_admit_uid();
        for (int unsigned lane = 0; lane < MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM; lane++) begin
            memblock_uid_t uid;
            main_control_transaction main_tr;
            memblock_op_behavior_t behavior;
            status_transaction status;

            uid = base_uid + lane;
            if (uid >= data.main_trans_num || data.control_barrier_blocks_admission(uid) ||
                !data.rob_admission_window_has_room(uid, "collect_lsq_candidates_d")) begin
                break;
            end
            main_tr = data.get_main_transaction(uid);
            if (is_control_op_class(main_tr.op_class)) begin
                break;
            end
            status = data.get_status(uid);
            behavior = lsq_ctrl_model::derive_op_behavior(main_tr);
            check_cbo_lsq_behavior(main_tr, behavior, "collect_lsq_candidates_d");
            if (behavior.need_alloc == 2'b00) begin
                break;
            end
            if (behavior.num_ls_elem != memblock_num_ls_elem_t'(1)) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("uid=%0d scalar D bundle requires numLsElem=1", uid))
            end
            if (status.terminal_done || status.active || status.enq ||
                status.exception_pending || status.replay_pending) begin
                break;
            end
            staged_uids.push_back(uid);
            staged_behaviors.push_back(behavior);
        end
        if (staged_uids.size() != 0 &&
            !lsq_ctrl.stage_dispatch_bundle(staged_uids, staged_behaviors)) begin
            `uvm_fatal(get_type_name(), "failed to capture an otherwise legal LSQ D bundle")
        end
    end
    return 1'b0;
endfunction:collect_lsq_candidates

function void memblock_lsqenq_dispatch_base_sequence::clear_lsqenq_xaction(input lsqenq_agent_agent_xaction tr);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "clear_lsqenq_xaction got null xaction")
    end
    for (int unsigned slot = 0; slot < MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM; slot++) begin
        set_need_alloc(tr, slot, 2'b00);
        set_req_fields(tr,
                       slot,
                       1'b0,
                       null,
                       lsq_ctrl_model::make_default_behavior(),
                       '{default:'0},
                       '{default:'0});
    end
endfunction:clear_lsqenq_xaction

function void memblock_lsqenq_dispatch_base_sequence::assign_lsqenq_slot(input lsqenq_agent_agent_xaction tr,
                                                                    input int unsigned slot,
                                                                    input main_control_transaction main_tr,
                                                                    input memblock_op_behavior_t behavior,
                                                                    input memblock_lq_key_t lq_key,
                                                                    input memblock_sq_key_t sq_key);
    if (slot >= MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("slot=%0d exceeds compile-time LSQ enqueue slot count=%0d",
                             slot,
                             MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM))
    end
    set_need_alloc(tr, slot, behavior.need_alloc);
    set_req_fields(tr,
                   slot,
                   1'b1,
                   main_tr,
                   behavior,
                   lq_key,
                   sq_key);
endfunction:assign_lsqenq_slot

function void memblock_lsqenq_dispatch_base_sequence::set_need_alloc(input lsqenq_agent_agent_xaction tr,
                                                                input int unsigned slot,
                                                                input bit [1:0] need_alloc);
    case (slot)
        0: tr.io_ooo_to_mem_enqLsq_needAlloc_0 = need_alloc;
        1: tr.io_ooo_to_mem_enqLsq_needAlloc_1 = need_alloc;
        2: tr.io_ooo_to_mem_enqLsq_needAlloc_2 = need_alloc;
        3: tr.io_ooo_to_mem_enqLsq_needAlloc_3 = need_alloc;
        4: tr.io_ooo_to_mem_enqLsq_needAlloc_4 = need_alloc;
        5: tr.io_ooo_to_mem_enqLsq_needAlloc_5 = need_alloc;
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("set_need_alloc got unsupported slot=%0d", slot))
        end
    endcase
endfunction:set_need_alloc

function void memblock_lsqenq_dispatch_base_sequence::set_req_fields(input lsqenq_agent_agent_xaction tr,
                                                                input int unsigned slot,
                                                                input bit valid,
                                                                input main_control_transaction main_tr,
                                                                input memblock_op_behavior_t behavior,
                                                                input memblock_lq_key_t lq_key,
                                                                input memblock_sq_key_t sq_key);
    bit [MEMBLOCK_DUT_FUTYPE_W-1:0] dut_futype;
    bit [MEMBLOCK_DUT_UOP_IDX_W-1:0] uop_idx;
    memblock_rob_key_t rob_key;
    memblock_num_ls_elem_t num_ls_elem;
    memblock_op_behavior_t default_behavior;
    bit [8:0] fu_op_type;
    bit last_uop;

    if (tr == null) begin
        `uvm_fatal(get_type_name(), "set_req_fields got null xaction")
    end
    if (slot >= MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("set_req_fields slot=%0d exceeds compile-time slot count=%0d",
                             slot,
                             MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM))
    end
    default_behavior = lsq_ctrl_model::make_default_behavior();
    if (valid) begin
        if (main_tr == null) begin
            `uvm_fatal(get_type_name(), $sformatf("active LSQ slot=%0d got null main transaction", slot))
        end
        if (behavior.num_ls_elem != memblock_num_ls_elem_t'(1) ||
            main_tr.numLsElem != memblock_num_ls_elem_t'(1) ||
            !(behavior.need_alloc inside {2'b01, 2'b10}) ||
            (behavior.need_alloc == 2'b01 && (!behavior.uses_lq || behavior.uses_sq)) ||
            (behavior.need_alloc == 2'b10 && (behavior.uses_lq || !behavior.uses_sq))) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("slot=%0d violates scalar LSQ behavior: needAlloc=%0b uses_lq/sq=%0b/%0b main/behavior numLsElem=%0d/%0d",
                                 slot,
                                 behavior.need_alloc,
                                 behavior.uses_lq,
                                 behavior.uses_sq,
                                 main_tr.numLsElem,
                                 behavior.num_ls_elem))
        end
        check_cbo_lsq_behavior(main_tr, behavior, "set_req_fields");
        dut_futype = encode_and_fit_dut_futype(
            main_tr.fuType,
            $sformatf("%s::set_req_fields(slot=%0d)", get_type_name(), slot));
        rob_key = main_tr.get_rob_key();
        uop_idx = '0;
        num_ls_elem = behavior.num_ls_elem;
        fu_op_type = main_tr.fuOpType;
        last_uop = 1'b1;
    end else begin
        if (main_tr != null || behavior != default_behavior ||
            lq_key.flag || lq_key.value != '0 ||
            sq_key.flag || sq_key.value != '0) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("idle slot=%0d requires null main transaction, default behavior, and zero keys",
                                 slot))
        end
        dut_futype = '0;
        rob_key = '{default:'0};
        uop_idx = '0;
        num_ls_elem = '0;
        fu_op_type = '0;
        last_uop = 1'b0;
    end
    case (slot)
        0: begin
            tr.io_ooo_to_mem_enqLsq_req_0_valid = valid;
            tr.io_ooo_to_mem_enqLsq_req_0_bits_fuType = dut_futype;
            tr.io_ooo_to_mem_enqLsq_req_0_bits_uopIdx = uop_idx;
            tr.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag = rob_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value = rob_key.value;
            tr.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag = lq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value = lq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag = sq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value = sq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_0_bits_numLsElem = num_ls_elem;
            {tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_23, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_22, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_21, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_20, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_19, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_18, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_17, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_16, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_15, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_14, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_13, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_12, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_11, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_10, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_9, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_8, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_7, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_6, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_5, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_4, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_3, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_2, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_1, tr.io_ooo_to_mem_enqLsq_req_0_bits_exceptionVec_0} = '0;
            tr.io_ooo_to_mem_enqLsq_req_0_bits_trigger = '0;
            tr.io_ooo_to_mem_enqLsq_req_0_bits_fuOpType = fu_op_type;
            tr.io_ooo_to_mem_enqLsq_req_0_bits_flushPipe = 1'b0;
            tr.io_ooo_to_mem_enqLsq_req_0_bits_lastUop = last_uop;
        end
        1: begin
            tr.io_ooo_to_mem_enqLsq_req_1_valid = valid;
            tr.io_ooo_to_mem_enqLsq_req_1_bits_fuType = dut_futype;
            tr.io_ooo_to_mem_enqLsq_req_1_bits_uopIdx = uop_idx;
            tr.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag = rob_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value = rob_key.value;
            tr.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag = lq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value = lq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag = sq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value = sq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_1_bits_numLsElem = num_ls_elem;
            {tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_23, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_22, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_21, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_20, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_19, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_18, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_17, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_16, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_15, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_14, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_13, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_12, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_11, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_10, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_9, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_8, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_7, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_6, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_5, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_4, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_3, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_2, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_1, tr.io_ooo_to_mem_enqLsq_req_1_bits_exceptionVec_0} = '0;
            tr.io_ooo_to_mem_enqLsq_req_1_bits_trigger = '0;
            tr.io_ooo_to_mem_enqLsq_req_1_bits_fuOpType = fu_op_type;
            tr.io_ooo_to_mem_enqLsq_req_1_bits_flushPipe = 1'b0;
            tr.io_ooo_to_mem_enqLsq_req_1_bits_lastUop = last_uop;
        end
        2: begin
            tr.io_ooo_to_mem_enqLsq_req_2_valid = valid;
            tr.io_ooo_to_mem_enqLsq_req_2_bits_fuType = dut_futype;
            tr.io_ooo_to_mem_enqLsq_req_2_bits_uopIdx = uop_idx;
            tr.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag = rob_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value = rob_key.value;
            tr.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag = lq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value = lq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag = sq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value = sq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_2_bits_numLsElem = num_ls_elem;
            {tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_23, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_22, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_21, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_20, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_19, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_18, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_17, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_16, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_15, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_14, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_13, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_12, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_11, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_10, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_9, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_8, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_7, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_6, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_5, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_4, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_3, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_2, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_1, tr.io_ooo_to_mem_enqLsq_req_2_bits_exceptionVec_0} = '0;
            tr.io_ooo_to_mem_enqLsq_req_2_bits_trigger = '0;
            tr.io_ooo_to_mem_enqLsq_req_2_bits_fuOpType = fu_op_type;
            tr.io_ooo_to_mem_enqLsq_req_2_bits_flushPipe = 1'b0;
            tr.io_ooo_to_mem_enqLsq_req_2_bits_lastUop = last_uop;
        end
        3: begin
            tr.io_ooo_to_mem_enqLsq_req_3_valid = valid;
            tr.io_ooo_to_mem_enqLsq_req_3_bits_fuType = dut_futype;
            tr.io_ooo_to_mem_enqLsq_req_3_bits_uopIdx = uop_idx;
            tr.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag = rob_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value = rob_key.value;
            tr.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag = lq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value = lq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag = sq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value = sq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_3_bits_numLsElem = num_ls_elem;
            {tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_23, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_22, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_21, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_20, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_19, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_18, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_17, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_16, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_15, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_14, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_13, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_12, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_11, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_10, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_9, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_8, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_7, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_6, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_5, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_4, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_3, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_2, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_1, tr.io_ooo_to_mem_enqLsq_req_3_bits_exceptionVec_0} = '0;
            tr.io_ooo_to_mem_enqLsq_req_3_bits_trigger = '0;
            tr.io_ooo_to_mem_enqLsq_req_3_bits_fuOpType = fu_op_type;
            tr.io_ooo_to_mem_enqLsq_req_3_bits_flushPipe = 1'b0;
            tr.io_ooo_to_mem_enqLsq_req_3_bits_lastUop = last_uop;
        end
        4: begin
            tr.io_ooo_to_mem_enqLsq_req_4_valid = valid;
            tr.io_ooo_to_mem_enqLsq_req_4_bits_fuType = dut_futype;
            tr.io_ooo_to_mem_enqLsq_req_4_bits_uopIdx = uop_idx;
            tr.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag = rob_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value = rob_key.value;
            tr.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag = lq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value = lq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag = sq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value = sq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_4_bits_numLsElem = num_ls_elem;
            {tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_23, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_22, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_21, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_20, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_19, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_18, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_17, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_16, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_15, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_14, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_13, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_12, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_11, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_10, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_9, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_8, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_7, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_6, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_5, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_4, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_3, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_2, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_1, tr.io_ooo_to_mem_enqLsq_req_4_bits_exceptionVec_0} = '0;
            tr.io_ooo_to_mem_enqLsq_req_4_bits_trigger = '0;
            tr.io_ooo_to_mem_enqLsq_req_4_bits_fuOpType = fu_op_type;
            tr.io_ooo_to_mem_enqLsq_req_4_bits_flushPipe = 1'b0;
            tr.io_ooo_to_mem_enqLsq_req_4_bits_lastUop = last_uop;
        end
        5: begin
            tr.io_ooo_to_mem_enqLsq_req_5_valid = valid;
            tr.io_ooo_to_mem_enqLsq_req_5_bits_fuType = dut_futype;
            tr.io_ooo_to_mem_enqLsq_req_5_bits_uopIdx = uop_idx;
            tr.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag = rob_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value = rob_key.value;
            tr.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag = lq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value = lq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag = sq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value = sq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_5_bits_numLsElem = num_ls_elem;
            {tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_23, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_22, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_21, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_20, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_19, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_18, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_17, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_16, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_15, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_14, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_13, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_12, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_11, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_10, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_9, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_8, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_7, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_6, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_5, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_4, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_3, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_2, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_1, tr.io_ooo_to_mem_enqLsq_req_5_bits_exceptionVec_0} = '0;
            tr.io_ooo_to_mem_enqLsq_req_5_bits_trigger = '0;
            tr.io_ooo_to_mem_enqLsq_req_5_bits_fuOpType = fu_op_type;
            tr.io_ooo_to_mem_enqLsq_req_5_bits_flushPipe = 1'b0;
            tr.io_ooo_to_mem_enqLsq_req_5_bits_lastUop = last_uop;
        end
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("set_req_fields got unsupported slot=%0d", slot))
        end
    endcase
endfunction:set_req_fields

function void memblock_lsqenq_dispatch_base_sequence::confirm_lsq_candidates(input lsqenq_agent_agent_xaction tr,
                                                                        input memblock_uid_t uids[$],
                                                                        input int unsigned slots[$],
                                                                        input main_control_transaction trs[$],
                                                                        input memblock_op_behavior_t behaviors[$],
                                                                        input memblock_lq_key_t lq_keys[$],
                                                                        input memblock_sq_key_t sq_keys[$],
                                                                        inout bit has_progress);
    if (!tr.memblock_dispatch_request_launched) begin
        if (!tr.memblock_dispatch_aborted_by_redirect &&
            !admission_blocked_by_flush() &&
            tr.memblock_dispatch_flush_epoch == memblock_sync_pkg::dispatch_flush_epoch) begin
            `uvm_fatal(get_type_name(), "active LSQ candidate returned without launch or redirect abort")
        end
        return;
    end
    if (tr.memblock_dispatch_aborted_by_redirect) begin
        `uvm_fatal(get_type_name(), "LSQ transaction cannot be both launched and aborted before launch")
    end
    if (!tr.memblock_source_offer_published ||
        tr.memblock_source_offer_target_sample_seq == 0 ||
        tr.memblock_source_offer_dut_sample_seq != tr.memblock_source_offer_target_sample_seq + 1) begin
        `uvm_fatal(get_type_name(), "active LSQ launch has no driver-bound source offer")
    end
    if (pending_sample_valid) begin
        `uvm_fatal(get_type_name(), "cannot reserve current LSQ batch before completing previous sample")
    end
    if (uids.size() == 0 || uids.size() != slots.size() || uids.size() != trs.size() ||
        uids.size() != behaviors.size() || uids.size() != lq_keys.size() ||
        uids.size() != sq_keys.size()) begin
        `uvm_fatal(get_type_name(), "LSQ candidate queues are empty or have inconsistent sizes")
    end
    foreach (uids[idx]) begin
        memblock_lq_key_t expected_lq_key;
        memblock_sq_key_t expected_sq_key;
        memblock_lsq_reservation_token_t token;

        if (!data.rob_admission_window_has_room(uids[idx], "confirm_lsq_candidates")) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("LSQ candidate uid=%0d left ROB admission window before activation",
                                 uids[idx]))
        end

        check_cbo_lsq_behavior(trs[idx], behaviors[idx], "confirm_lsq_candidates");
        lsq_ctrl.preview_allocate(behaviors[idx], expected_lq_key, expected_sq_key);
        if (behaviors[idx].uses_lq && expected_lq_key != lq_keys[idx]) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("uid=%0d LQ preview drift: expected={%0d,%0d} candidate={%0d,%0d}",
                                 uids[idx], expected_lq_key.flag, expected_lq_key.value,
                                 lq_keys[idx].flag, lq_keys[idx].value))
        end
        if (behaviors[idx].uses_sq && expected_sq_key != sq_keys[idx]) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("uid=%0d SQ preview drift: expected={%0d,%0d} candidate={%0d,%0d}",
                                 uids[idx], expected_sq_key.flag, expected_sq_key.value,
                                 sq_keys[idx].flag, sq_keys[idx].value))
        end
        lsq_ctrl.commit_allocate(uids[idx], behaviors[idx], trs[idx]);
        token.valid = 1'b1;
        token.uid = uids[idx];
        token.launch_epoch = data.begin_lsq_reservation_launch(uids[idx]);
        pending_sample_tokens.push_back(token);
        has_progress = 1'b1;
    end
    pending_sample_flush_epoch = tr.memblock_dispatch_flush_epoch;
    pending_sample_launch_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
    pending_sample_target_sample_seq = tr.memblock_source_offer_dut_sample_seq;
    pending_sample_control_reset_epoch = memblock_sync_pkg::control_reset_epoch;
    pending_sample_shadow_generation = lsq_ctrl.shadow_baseline_generation;
    pending_sample_valid = 1'b1;
    publish_sequence_local_state();
endfunction:confirm_lsq_candidates

function void memblock_lsqenq_dispatch_base_sequence::complete_v2_pending_sample(inout bit has_progress);
    longint unsigned sample_seq;

    if (!pending_sample_valid) begin
        return;
    end
    if (pending_sample_reset_binding_invalid()) begin
        cancel_v2_pending_sample_on_reset("reset invalidated pending reservation before complete");
        return;
    end
    // LSQ enqueue service is a consumer; CSR monitor owns sample advancement.
    sample_seq = pending_sample_target_sample_seq;
    if (sample_seq == 0 || lsq_ctrl.shadow_last_sample_seq < sample_seq) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("pending reservation target sample is not complete target/shadow=%0d/%0d",
                             sample_seq,
                             lsq_ctrl.shadow_last_sample_seq))
    end
    foreach (pending_sample_tokens[idx]) begin
        if (!pending_sample_tokens[idx].valid) begin
            `uvm_fatal(get_type_name(), "pending LSQ reservation token is invalid")
        end
        data.mark_lsq_reservation_sampled(pending_sample_tokens[idx].uid,
                                          pending_sample_tokens[idx].launch_epoch,
                                          sample_seq);
    end
    if (!admission_blocked_by_flush() &&
        pending_sample_flush_epoch == memblock_sync_pkg::dispatch_flush_epoch) begin
        foreach (pending_sample_tokens[idx]) begin
            complete_admission(pending_sample_tokens[idx].uid);
            has_progress = 1'b1;
        end
    end else begin
        `uvm_info(get_type_name(),
                  $sformatf("discard LSQ pending sample after redirect: launch_cycle=%0d saved_epoch=%0d current_epoch=%0d",
                            pending_sample_launch_cycle,
                            pending_sample_flush_epoch,
                            memblock_sync_pkg::dispatch_flush_epoch),
                  UVM_LOW)
    end
    clear_v2_pending_sample();
endfunction:complete_v2_pending_sample

function void memblock_lsqenq_dispatch_base_sequence::clear_v2_pending_sample();
    pending_sample_tokens.delete();
    pending_sample_valid = 1'b0;
    pending_sample_flush_epoch = 0;
    pending_sample_launch_cycle = 0;
    pending_sample_target_sample_seq = 0;
    pending_sample_control_reset_epoch = 0;
    pending_sample_shadow_generation = 0;
    publish_sequence_local_state();
endfunction:clear_v2_pending_sample

function void memblock_lsqenq_dispatch_base_sequence::complete_admission(input memblock_uid_t uid);
    if (is_control_op_class(data.get_main_transaction(uid).op_class)) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("control uid=%0d must not enter ordinary complete_admission", uid))
    end
    drain_csr_runtime_events();
    issue_sched.prepare_issue_route_for_uid(uid);
endfunction:complete_admission

// 抽象职责：在普通 LSQ/non-LSQ admission 前处理控制 UID。它只调用公共控制
// admission API 或 redirect 后 prefix 恢复，不构造 LSQ xaction、不调用 issue scheduler。
function bit memblock_lsqenq_dispatch_base_sequence::admit_control_marker_if_ready(
    output bit has_progress
);
    memblock_uid_t uid;
    main_control_transaction main_tr;
    status_transaction status;

    has_progress = 1'b0;
    if (admission_blocked_by_flush()) begin
        return 1'b0;
    end
    uid = data.get_next_new_admit_uid();
    if (uid >= data.main_trans_num || data.control_barrier_blocks_admission(uid)) begin
        return 1'b0;
    end
    main_tr = data.get_main_transaction(uid);
    if (!is_control_op_class(main_tr.op_class)) begin
        return 1'b0;
    end
    status = data.get_status(uid);
    if (!data.rob_admission_window_has_room(uid, "admit_control_marker_if_ready")) begin
        return 1'b0;
    end
    if (status.active || status.enq) begin
        data.restore_control_admission_prefix(uid);
    end else begin
        data.activate_control_uid(uid);
    end
    has_progress = 1'b1;
    return 1'b1;
endfunction:admit_control_marker_if_ready

function bit memblock_lsqenq_dispatch_base_sequence::admit_non_lsq_if_ready(output bit has_progress);
    memblock_uid_t uid;
    main_control_transaction main_tr;
    memblock_op_behavior_t behavior;

    has_progress = 1'b0;
    if (!next_uid_needs_lsq_admission(uid, main_tr, behavior)) begin
        return 1'b0;
    end
    if (behavior.need_alloc != 2'b00) begin
        return 1'b0;
    end
    if (behavior.is_atomic) begin
        // Atomic admission owns a sampled D/R/Q token path.  When reset
        // baseline or isolation is not ready, leave it for that path and let
        // the caller send an idle boundary so the shadow model can advance.
        return 1'b0;
    end
    if (!lsq_ctrl.rtl_admission_ready()) begin
        return 1'b0;
    end
    lsq_ctrl.commit_non_lsq_admission(uid, behavior, main_tr);
    complete_admission(uid);
    has_progress = 1'b1;
    return 1'b1;
endfunction:admit_non_lsq_if_ready

function void memblock_lsqenq_dispatch_base_sequence::configure_source_offer_metadata(
    input lsqenq_agent_agent_xaction tr,
    input bit [MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM-1:0] fire_mask,
    input bit need_alloc_nonzero,
    input int unsigned lq_alloc,
    input int unsigned sq_alloc,
    input string reason
);
    if (tr == null || tr.memblock_source_offer_metadata_valid) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("%s attempted invalid/duplicate source-offer metadata", reason))
    end
    tr.memblock_source_offer_metadata_valid = 1'b1;
    tr.memblock_source_offer_fire_mask = fire_mask;
    tr.memblock_source_offer_need_alloc_nonzero = need_alloc_nonzero;
    tr.memblock_source_offer_lq_alloc = lq_alloc;
    tr.memblock_source_offer_sq_alloc = sq_alloc;
endfunction:configure_source_offer_metadata

function bit memblock_lsqenq_dispatch_base_sequence::pending_sample_reset_binding_invalid();
    return pending_sample_valid &&
           (memblock_sync_pkg::reset_backend_done !== 1'b1 ||
            memblock_sync_pkg::l2tlb_reset_active() ||
            pending_sample_control_reset_epoch != memblock_sync_pkg::control_reset_epoch ||
            pending_sample_shadow_generation != lsq_ctrl.shadow_baseline_generation);
endfunction:pending_sample_reset_binding_invalid

function void memblock_lsqenq_dispatch_base_sequence::cancel_v2_pending_sample_on_reset(
    input string reason
);
    int unsigned rollback_lq_count;
    int unsigned rollback_sq_count;

    if (!pending_sample_valid) begin
        return;
    end
    rollback_lq_count = 0;
    rollback_sq_count = 0;
    for (int idx = pending_sample_tokens.size(); idx > 0; idx--) begin
        memblock_uid_t uid;
        memblock_op_behavior_t behavior;

        if (!pending_sample_tokens[idx - 1].valid) begin
            `uvm_fatal(get_type_name(), "reset cancel found invalid pending reservation token")
        end
        uid = pending_sample_tokens[idx - 1].uid;
        behavior = lsq_ctrl_model::derive_op_behavior(
            data.get_main_transaction(uid));
        data.cancel_pending_lsq_reservation_on_reset(
            uid,
            pending_sample_tokens[idx - 1].launch_epoch);
        if (behavior.uses_lq) rollback_lq_count += behavior.num_ls_elem;
        if (behavior.uses_sq) rollback_sq_count += behavior.num_ls_elem;
        data.retire_active_uid(uid);
        void'(data.init_status_for_uid(uid));
    end
    if (rollback_lq_count != 0) lsq_ctrl.cancel_lq(rollback_lq_count);
    if (rollback_sq_count != 0) lsq_ctrl.cancel_sq(rollback_sq_count);
    `uvm_info(get_type_name(),
              $sformatf("cancel pending LSQ reservation on reset: %s rollback_lq/sq=%0d/%0d",
                        reason, rollback_lq_count, rollback_sq_count),
              UVM_LOW)
    clear_v2_pending_sample();
endfunction:cancel_v2_pending_sample_on_reset

function void memblock_lsqenq_dispatch_base_sequence::clear_pre_fire_atomic_token(
    input string reason
);
    if (atomic_token_stage == 0) begin
        return;
    end
    `uvm_info(get_type_name(),
              $sformatf("clear atomic admission token uid=%0d stage=%0d reason=%s",
                        atomic_token_uid, atomic_token_stage, reason),
              UVM_LOW)
    atomic_token_stage = 0;
    atomic_token_uid = 0;
    atomic_token_tr = null;
    atomic_token_behavior = lsq_ctrl_model::make_default_behavior();
    atomic_token_control_reset_epoch = 0;
    atomic_token_shadow_generation = 0;
    atomic_token_target_sample_seq = 0;
    publish_sequence_local_state();
endfunction:clear_pre_fire_atomic_token

function bit memblock_lsqenq_dispatch_base_sequence::atomic_token_reset_binding_invalid();
    if (atomic_token_stage == 0) begin
        return 1'b0;
    end
    return memblock_sync_pkg::reset_backend_done !== 1'b1 ||
           !lsq_ctrl.shadow_baseline_valid ||
           atomic_token_shadow_generation != lsq_ctrl.shadow_baseline_generation ||
           atomic_token_control_reset_epoch != lsq_ctrl.shadow_control_reset_epoch;
endfunction:atomic_token_reset_binding_invalid

function bit memblock_lsqenq_dispatch_base_sequence::service_atomic_admission_token(
    output bit has_progress
);
    memblock_uid_t uid;
    main_control_transaction main_tr;
    memblock_op_behavior_t behavior;
    status_transaction status;

    has_progress = 1'b0;
    ensure_helpers();

    if (atomic_token_stage == 0 &&
        data.release_unfired_atomic_barrier_after_redirect_recovery()) begin
        has_progress = 1'b1;
    end

    if (atomic_token_reset_binding_invalid()) begin
        // Token 与当前 generation 相同但 reset epoch/backend/baseline 已变化时，
        // 尚未 fire 的 atomic 仍可能停在 D/R/Q。整体失效 shadow，禁止该旧 token
        // 漂移到 Q 后被普通 LSQ 路径接管，也禁止把 reset 清 Q 当成 atomic fire。
        if (atomic_token_shadow_generation == lsq_ctrl.shadow_baseline_generation) begin
            lsq_ctrl.reset_rtl_shadow();
        end
        clear_pre_fire_atomic_token("control reset epoch/backend/shadow baseline changed");
        has_progress = 1'b1;
        return 1'b1;
    end

    if (atomic_token_stage != 0 && admission_blocked_by_flush()) begin
        // A pre-fire token owns no ROB/LSQ/barrier state and is discarded by
        // an effective redirect.  Fired atomic owners are handled elsewhere.
        clear_pre_fire_atomic_token("effective redirect before atomic Q fire");
        has_progress = 1'b1;
        return 1'b1;
    end

    if (atomic_token_stage == 2) begin
        if (atomic_token_target_sample_seq == 0 ||
            lsq_ctrl.shadow_last_sample_seq < atomic_token_target_sample_seq) begin
            return 1'b1;
        end
        if (lsq_ctrl.rtl_q_has_valid()) begin
            `uvm_fatal(get_type_name(), "sampled atomic Q fire did not consume the shared lane")
        end
        if (data.dispatch_progress.terminal_done_uid != atomic_token_uid) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("atomic Q owner uid=%0d lost older-terminal isolation frontier=%0d",
                                 atomic_token_uid,
                                 data.dispatch_progress.terminal_done_uid))
        end
        lsq_ctrl.commit_atomic_non_lsq_admission(
            atomic_token_uid,
            atomic_token_behavior,
            atomic_token_tr,
            lsq_ctrl.shadow_lq_enq_ptr,
            lsq_ctrl.shadow_sq_enq_ptr);
        data.establish_atomic_barrier(atomic_token_uid);
        complete_admission(atomic_token_uid);
        clear_pre_fire_atomic_token("atomic Q fire committed");
        has_progress = 1'b1;
        return 1'b1;
    end

    if (atomic_token_stage == 1) begin
        if (lsq_ctrl.shadow_q_valid[0]) begin
            if (!lsq_ctrl.shadow_q_is_atomic[0] ||
                lsq_ctrl.shadow_q_uid[0] != atomic_token_uid) begin
                `uvm_fatal(get_type_name(), "shared Q lane0 atomic identity mismatch")
            end
            for (int unsigned lane = 1; lane < MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM; lane++) begin
                if (lsq_ctrl.shadow_q_valid[lane]) begin
                    `uvm_fatal(get_type_name(), "atomic token must occupy the shared Q bundle alone")
                end
            end
            if (lsq_ctrl.rtl_admission_ready() &&
                !lsq_ctrl.shadow_iq_recompute_hold &&
                !lsq_ctrl.shadow_source_offer_valid &&
                !pending_sample_valid &&
                data.dispatch_progress.terminal_done_uid == atomic_token_uid &&
                !data.active_atomic_barrier_valid &&
                !data.active_control_barrier_valid) begin
                atomic_token_stage = 2;
                atomic_token_target_sample_seq = 0;
                publish_sequence_local_state();
                has_progress = 1'b1;
            end
        end
        return 1'b1;
    end

    if (admission_blocked_by_flush()) begin
        return 1'b0;
    end
    uid = data.get_next_new_admit_uid();
    if (uid >= data.main_trans_num || data.control_barrier_blocks_admission(uid) ||
        is_control_op_class(data.get_main_transaction(uid).op_class)) begin
        return 1'b0;
    end
    main_tr = data.get_main_transaction(uid);
    behavior = lsq_ctrl_model::derive_op_behavior(main_tr);
    if (!behavior.is_atomic) begin
        return 1'b0;
    end
    status = data.get_status(uid);
    if (status.terminal_done || status.active || status.enq ||
        status.exception_pending || status.replay_pending) begin
        return 1'b0;
    end
    if (!data.rob_admission_window_has_room(uid, "service_atomic_admission_token")) begin
        return 1'b0;
    end
    // Strict AtomicsUnit isolation: capture only when every older UID is
    // terminal and no prior atomic/control barrier is active.
    if (data.dispatch_progress.terminal_done_uid != uid ||
        data.active_atomic_barrier_valid || data.active_control_barrier_valid) begin
        return 1'b0;
    end
    if (!lsq_ctrl.shadow_baseline_valid ||
        !lsq_ctrl.rtl_pipe_empty() || lsq_ctrl.shadow_source_offer_valid ||
        pending_sample_valid) begin
        return 1'b0;
    end
    begin
        memblock_uid_t atomic_uids[$];
        memblock_op_behavior_t atomic_behaviors[$];

        atomic_uids.push_back(uid);
        atomic_behaviors.push_back(behavior);
        if (!lsq_ctrl.stage_dispatch_bundle(atomic_uids, atomic_behaviors, 1'b1)) begin
            `uvm_fatal(get_type_name(), "failed to stage isolated atomic token in shared D/R/Q")
        end
    end
    atomic_token_stage = 1;
    atomic_token_uid = uid;
    atomic_token_tr = main_tr;
    atomic_token_behavior = behavior;
    atomic_token_control_reset_epoch = lsq_ctrl.shadow_control_reset_epoch;
    atomic_token_shadow_generation = lsq_ctrl.shadow_baseline_generation;
    publish_sequence_local_state();
    has_progress = 1'b1;
    return 1'b1;
endfunction:service_atomic_admission_token

`endif
