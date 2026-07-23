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
    extern function void configure_from_plus();
    extern function void ensure_helpers();
    extern function void drain_csr_runtime_events();
    extern function void apply_pending_lsq_cancels();
    extern task wait_for_main_table();
    extern function bit admission_blocked_by_flush();
    extern function bit next_uid_needs_lsq_admission(output memblock_uid_t uid,
                                                     output main_control_transaction main_tr,
                                                     output memblock_op_behavior_t behavior);
    extern function bit collect_lsq_candidates(output memblock_uid_t uids[$],
                                               output main_control_transaction trs[$],
                                               output memblock_op_behavior_t behaviors[$],
                                               output memblock_lq_key_t lq_keys[$],
                                               output memblock_sq_key_t sq_keys[$]);
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
                                                input main_control_transaction trs[$],
                                                input memblock_op_behavior_t behaviors[$],
                                                input memblock_lq_key_t lq_keys[$],
                                                input memblock_sq_key_t sq_keys[$],
                                                inout bit has_progress);
    extern function void complete_v2_pending_sample(inout bit has_progress);
    extern function void clear_v2_pending_sample();
    extern function void complete_admission(input memblock_uid_t uid);
    extern function bit admit_non_lsq_if_ready(output bit has_progress);

endclass:memblock_lsqenq_dispatch_base_sequence

function memblock_lsqenq_dispatch_base_sequence::new(string name = "memblock_lsqenq_dispatch_base_sequence");
    super.new(name);
    enable = 1'b0;
    no_progress_warn_cycles = 10000;
    pending_sample_valid = 1'b0;
    pending_sample_flush_epoch = 0;
    pending_sample_launch_cycle = 0;
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
    drive_lsqenq_loop();
endtask:body

task memblock_lsqenq_dispatch_base_sequence::drive_lsqenq_loop();
    int unsigned idle_count;
    int unsigned cycle_idx;

    idle_count = 0;
    cycle_idx = 0;
    forever begin
        bit has_progress;

        if (data.is_global_stop_requested()) begin
            has_progress = 1'b0;
            if (pending_sample_valid) begin
                send_idle_lsqenq_boundary(cycle_idx, "global_stop trailing sample", has_progress);
            end
            `uvm_info(get_type_name(),
                      $sformatf("stop LSQ enqueue loop by global_stop_requested at cycle=%0d",
                                cycle_idx),
                      UVM_LOW)
            break;
        end
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
    main_control_transaction  trs[$];
    memblock_op_behavior_t    behaviors[$];
    memblock_lq_key_t         lq_keys[$];
    memblock_sq_key_t         sq_keys[$];
    bit                       admission_progress;

    has_progress = 1'b0;
    apply_pending_lsq_cancels();
    if (pending_sample_valid) begin
        memblock_uid_t probe_uid;
        main_control_transaction probe_tr;
        memblock_op_behavior_t probe_behavior;

        if (next_uid_needs_lsq_admission(probe_uid, probe_tr, probe_behavior) &&
            probe_behavior.need_alloc == 2'b00) begin
            send_idle_lsqenq_boundary(cycle_idx, "non-LSQ sample boundary", has_progress);
        end
    end
    admission_progress = 1'b0;
    if (admit_non_lsq_if_ready(admission_progress)) begin
        has_progress |= admission_progress;
        return;
    end
    if (!collect_lsq_candidates(uids, trs, behaviors, lq_keys, sq_keys)) begin
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
    tr.pre_pkt_gap = 0;
    tr.post_pkt_gap = 0;
    foreach (uids[idx]) begin
        assign_lsqenq_slot(tr, idx, trs[idx], behaviors[idx], lq_keys[idx], sq_keys[idx]);
    end

    start_item(tr);
    finish_item(tr);
    complete_v2_pending_sample(has_progress);
    confirm_lsq_candidates(tr, uids, trs, behaviors, lq_keys, sq_keys, has_progress);
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
    tr.pre_pkt_gap = 0;
    tr.post_pkt_gap = 0;
    `uvm_info(get_type_name(), $sformatf("send LSQ idle boundary: %s", reason), UVM_DEBUG)
    start_item(tr);
    finish_item(tr);
    complete_v2_pending_sample(has_progress);
endtask:send_idle_lsqenq_boundary

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
endfunction:ensure_helpers

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

function bit memblock_lsqenq_dispatch_base_sequence::next_uid_needs_lsq_admission(output memblock_uid_t uid,
                                                                             output main_control_transaction main_tr,
                                                                             output memblock_op_behavior_t behavior);
    ensure_helpers();
    if (admission_blocked_by_flush()) begin
        return 1'b0;
    end
    uid = data.get_next_new_admit_uid();
    if (uid < data.main_trans_num) begin
        status_transaction status;

        status = data.get_status(uid);
        main_tr = data.get_main_transaction(uid);
        behavior = lsq_ctrl_model::derive_op_behavior(main_tr);
        if (status.terminal_done || status.active || status.enq ||
            status.exception_pending || status.replay_pending) begin
            return 1'b0;
        end
        // redirect_pending/flushed表示旧动态实例已被kill；同uid现在允许按公共高水位重新admission。
        return 1'b1;
    end
    uid = 0;
    main_tr = null;
    behavior = lsq_ctrl_model::make_default_behavior();
    return 1'b0;
endfunction:next_uid_needs_lsq_admission

function bit memblock_lsqenq_dispatch_base_sequence::collect_lsq_candidates(output memblock_uid_t uids[$],
                                                                       output main_control_transaction trs[$],
                                                                       output memblock_op_behavior_t behaviors[$],
                                                                       output memblock_lq_key_t lq_keys[$],
                                                                       output memblock_sq_key_t sq_keys[$]);
    int unsigned max_enq;
    memblock_lq_key_t lq_tmp;
    memblock_sq_key_t sq_tmp;
    int unsigned lq_free_tmp;
    int unsigned sq_free_tmp;
    int unsigned load_elem_count;
    int unsigned store_elem_count;
    memblock_uid_t uid;
    main_control_transaction main_tr;
    memblock_op_behavior_t behavior;

    uids.delete();
    trs.delete();
    behaviors.delete();
    lq_keys.delete();
    sq_keys.delete();
    if (admission_blocked_by_flush()) begin
        return 1'b0;
    end
    max_enq = seq_csr_common::get_enq_per_cycle();
    if (max_enq == 0) begin
        return 1'b0;
    end
    lq_tmp = lsq_ctrl.lq_enq_ptr;
    sq_tmp = lsq_ctrl.sq_enq_ptr;
    lq_free_tmp = lsq_ctrl.lq_free_count;
    sq_free_tmp = lsq_ctrl.sq_free_count;
    load_elem_count = 0;
    store_elem_count = 0;
    while (uids.size() < max_enq) begin
        memblock_lq_key_t lq_key;
        memblock_sq_key_t sq_key;
        status_transaction status;
        int unsigned tentative_load;
        int unsigned tentative_store;

        if (admission_blocked_by_flush()) begin
            break;
        end
        uid = data.get_next_new_admit_uid() + uids.size();
        if (uid >= data.main_trans_num) begin
            break;
        end
        main_tr = data.get_main_transaction(uid);
        status = data.get_status(uid);
        if (status.terminal_done || status.active || status.enq ||
            status.exception_pending || status.replay_pending) begin
            break;
        end
        behavior = lsq_ctrl_model::derive_op_behavior(main_tr);
        if (behavior.need_alloc == 2'b00) begin
            break;
        end
        if (behavior.num_ls_elem != memblock_num_ls_elem_t'(1)) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("uid=%0d scalar-only LSQ enqueue requires num_ls_elem=1, got %0d",
                                 uid,
                                 behavior.num_ls_elem))
        end
        tentative_load = load_elem_count + (behavior.uses_lq ? behavior.num_ls_elem : 0);
        tentative_store = store_elem_count + (behavior.uses_sq ? behavior.num_ls_elem : 0);
        if (tentative_load > MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH ||
            tentative_store > MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH ||
            tentative_load > lq_free_tmp ||
            tentative_store > sq_free_tmp) begin
            break;
        end
        lq_key = lq_tmp;
        sq_key = sq_tmp;
        uids.push_back(uid);
        trs.push_back(main_tr);
        behaviors.push_back(behavior);
        lq_keys.push_back(lq_key);
        sq_keys.push_back(sq_key);
        load_elem_count = tentative_load;
        store_elem_count = tentative_store;
        if (behavior.uses_lq) begin
            lq_tmp = lsq_ctrl_model::advance_lq_key(lq_tmp, behavior.num_ls_elem);
        end
        if (behavior.uses_sq) begin
            sq_tmp = lsq_ctrl_model::advance_sq_key(sq_tmp, behavior.num_ls_elem);
        end
    end
    return uids.size() != 0;
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
    if (pending_sample_valid) begin
        `uvm_fatal(get_type_name(), "cannot reserve current LSQ batch before completing previous sample")
    end
    if (uids.size() == 0 || uids.size() != trs.size() ||
        uids.size() != behaviors.size() || uids.size() != lq_keys.size() ||
        uids.size() != sq_keys.size()) begin
        `uvm_fatal(get_type_name(), "LSQ candidate queues are empty or have inconsistent sizes")
    end
    foreach (uids[idx]) begin
        memblock_lq_key_t expected_lq_key;
        memblock_sq_key_t expected_sq_key;
        memblock_lsq_reservation_token_t token;

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
    pending_sample_valid = 1'b1;
endfunction:confirm_lsq_candidates

function void memblock_lsqenq_dispatch_base_sequence::complete_v2_pending_sample(inout bit has_progress);
    longint unsigned sample_seq;

    if (!pending_sample_valid) begin
        return;
    end
    sample_seq = memblock_sync_pkg::get_dut_sample_seq($time);
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
endfunction:clear_v2_pending_sample

function void memblock_lsqenq_dispatch_base_sequence::complete_admission(input memblock_uid_t uid);
    drain_csr_runtime_events();
    issue_sched.prepare_issue_route_for_uid(uid);
endfunction:complete_admission

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
    lsq_ctrl.commit_non_lsq_admission(uid, behavior, main_tr);
    complete_admission(uid);
    has_progress = 1'b1;
    return 1'b1;
endfunction:admit_non_lsq_if_ready

`endif
