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
    int unsigned ready_timeout;

    `uvm_object_utils(memblock_lsqenq_dispatch_base_sequence)

    extern function new(string name = "memblock_lsqenq_dispatch_base_sequence");
    extern virtual task pre_body();
    extern virtual task body();
    extern virtual task drive_lsqenq_loop();
    extern virtual task send_lsqenq_cycle(input int unsigned cycle_idx,
                                          output bit has_progress);
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
                                            input memblock_uid_t uid,
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
                                        input bit [35:0] fuType,
                                        input bit [6:0] uopIdx,
                                        input memblock_rob_key_t rob_key,
                                        input memblock_lq_key_t lq_key,
                                        input memblock_sq_key_t sq_key,
                                        input bit [4:0] numLsElem);
    extern function void get_resp_keys(input lsqenq_agent_agent_xaction tr,
                                       input int unsigned slot,
                                       output memblock_lq_key_t lq_key,
                                       output memblock_sq_key_t sq_key);
    extern function void confirm_lsq_candidates(input lsqenq_agent_agent_xaction tr,
                                                input memblock_uid_t uids[$],
                                                input main_control_transaction trs[$],
                                                input memblock_op_behavior_t behaviors[$],
                                                output bit has_progress);
    extern function void complete_admission(input memblock_uid_t uid);
    extern function bit admit_non_lsq_if_ready(output bit has_progress);

endclass:memblock_lsqenq_dispatch_base_sequence

function memblock_lsqenq_dispatch_base_sequence::new(string name = "memblock_lsqenq_dispatch_base_sequence");
    super.new(name);
    enable = 1'b0;
    no_progress_warn_cycles = 10000;
    ready_timeout = 1000;
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

    has_progress = 1'b0;
    apply_pending_lsq_cancels();
    if (admit_non_lsq_if_ready(has_progress)) begin
        return;
    end
    if (!collect_lsq_candidates(uids, trs, behaviors, lq_keys, sq_keys)) begin
        tr = lsqenq_agent_agent_xaction::type_id::create($sformatf("lsqenq_dispatch_idle_tr_%0d", cycle_idx));
        if (tr == null) begin
            `uvm_fatal(get_type_name(), "failed to create idle lsqenq xaction")
        end
        clear_lsqenq_xaction(tr);
        tr.memblock_dispatch_wait_can_accept = 1'b0;
        tr.memblock_dispatch_ready_timeout = ready_timeout;
        tr.memblock_dispatch_aborted_by_redirect = 1'b0;
        tr.memblock_dispatch_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
        tr.pre_pkt_gap = 0;
        tr.post_pkt_gap = 0;
        start_item(tr);
        finish_item(tr);
        return;
    end

    tr = lsqenq_agent_agent_xaction::type_id::create($sformatf("lsqenq_dispatch_tr_%0d", cycle_idx));
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "failed to create lsqenq xaction")
    end
    clear_lsqenq_xaction(tr);
    tr.memblock_dispatch_wait_can_accept = 1'b1;
    tr.memblock_dispatch_ready_timeout = ready_timeout;
    tr.memblock_dispatch_aborted_by_redirect = 1'b0;
    tr.memblock_dispatch_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
    tr.pre_pkt_gap = 0;
    tr.post_pkt_gap = 0;
    foreach (uids[idx]) begin
        assign_lsqenq_slot(tr, idx, uids[idx], trs[idx], behaviors[idx], lq_keys[idx], sq_keys[idx]);
    end

    start_item(tr);
    finish_item(tr);
    confirm_lsq_candidates(tr, uids, trs, behaviors, has_progress);
endtask:send_lsqenq_cycle

function void memblock_lsqenq_dispatch_base_sequence::configure_from_plus();
    enable = seq_csr_common::get_lsqenq_seq_en();
    no_progress_warn_cycles = seq_csr_common::get_active_seq_no_progress_warn_cycles();
    ready_timeout = seq_csr_common::get_lsqenq_ready_timeout();
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
    if (data.pending_lq_cancel_count != 0) begin
        // redirect flush后回退软件LSQ admission镜像，确保重入队仍与DUT response key一致。
        lsq_ctrl.cancel_lq(data.pending_lq_cancel_count);
        data.pending_lq_cancel_count = 0;
    end
    if (data.pending_sq_cancel_count != 0) begin
        // redirect flush后回退软件SQ admission镜像，后续uid从公共高水位顺序重入队。
        lsq_ctrl.cancel_sq(data.pending_sq_cancel_count);
        data.pending_sq_cancel_count = 0;
    end
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
    memblock_uid_t uid;
    main_control_transaction main_tr;
    memblock_op_behavior_t behavior;

    uids.delete();
    trs.delete();
    behaviors.delete();
    lq_keys.delete();
    sq_keys.delete();
    if (!next_uid_needs_lsq_admission(uid, main_tr, behavior)) begin
        return 1'b0;
    end
    if (behavior.need_alloc == 2'b00) begin
        return 1'b0;
    end

    max_enq = seq_csr_common::get_enq_per_cycle();
    lq_tmp = lsq_ctrl.lq_enq_ptr;
    sq_tmp = lsq_ctrl.sq_enq_ptr;
    lq_free_tmp = lsq_ctrl.lq_free_count;
    sq_free_tmp = lsq_ctrl.sq_free_count;
    while (uids.size() < max_enq) begin
        memblock_lq_key_t lq_key;
        memblock_sq_key_t sq_key;
        status_transaction status;

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
        if ((behavior.uses_lq && lq_free_tmp < behavior.num_ls_elem) ||
            (behavior.uses_sq && sq_free_tmp < behavior.num_ls_elem)) begin
            break;
        end
        lq_key = lq_tmp;
        sq_key = sq_tmp;
        uids.push_back(uid);
        trs.push_back(main_tr);
        behaviors.push_back(behavior);
        lq_keys.push_back(lq_key);
        sq_keys.push_back(sq_key);
        if (behavior.uses_lq) begin
            lq_tmp = lsq_ctrl_model::advance_lq_key(lq_tmp, behavior.num_ls_elem);
            lq_free_tmp -= behavior.num_ls_elem;
        end
        if (behavior.uses_sq) begin
            sq_tmp = lsq_ctrl_model::advance_sq_key(sq_tmp, behavior.num_ls_elem);
            sq_free_tmp -= behavior.num_ls_elem;
        end
    end
    return uids.size() != 0;
endfunction:collect_lsq_candidates

function void memblock_lsqenq_dispatch_base_sequence::clear_lsqenq_xaction(input lsqenq_agent_agent_xaction tr);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "clear_lsqenq_xaction got null xaction")
    end
    tr.io_ooo_to_mem_enqLsq_canAccept = 1'b0;
    for (int unsigned slot = 0; slot < seq_csr_common::get_real_enq_width(); slot++) begin
        set_need_alloc(tr, slot, 2'b00);
        set_req_fields(tr,
                       slot,
                       1'b0,
                       '0,
                       '0,
                       '{default:'0},
                       '{default:'0},
                       '{default:'0},
                       '0);
    end
endfunction:clear_lsqenq_xaction

function void memblock_lsqenq_dispatch_base_sequence::assign_lsqenq_slot(input lsqenq_agent_agent_xaction tr,
                                                                    input int unsigned slot,
                                                                    input memblock_uid_t uid,
                                                                    input main_control_transaction main_tr,
                                                                    input memblock_op_behavior_t behavior,
                                                                    input memblock_lq_key_t lq_key,
                                                                    input memblock_sq_key_t sq_key);
    if (slot >= seq_csr_common::get_real_enq_width()) begin
        `uvm_fatal(get_type_name(), $sformatf("slot=%0d exceeds real_enq_width", slot))
    end
    set_need_alloc(tr, slot, behavior.need_alloc);
    set_req_fields(tr,
                   slot,
                   1'b1,
                   main_tr.fuType,
                   uid[6:0],
                   main_tr.get_rob_key(),
                   lq_key,
                   sq_key,
                   behavior.num_ls_elem);
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
        6: tr.io_ooo_to_mem_enqLsq_needAlloc_6 = need_alloc;
        7: tr.io_ooo_to_mem_enqLsq_needAlloc_7 = need_alloc;
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("set_need_alloc got unsupported slot=%0d", slot))
        end
    endcase
endfunction:set_need_alloc

function void memblock_lsqenq_dispatch_base_sequence::set_req_fields(input lsqenq_agent_agent_xaction tr,
                                                                input int unsigned slot,
                                                                input bit valid,
                                                                input bit [35:0] fuType,
                                                                input bit [6:0] uopIdx,
                                                                input memblock_rob_key_t rob_key,
                                                                input memblock_lq_key_t lq_key,
                                                                input memblock_sq_key_t sq_key,
                                                                input bit [4:0] numLsElem);
    case (slot)
        0: begin
            tr.io_ooo_to_mem_enqLsq_req_0_valid = valid;
            tr.io_ooo_to_mem_enqLsq_req_0_bits_fuType = fuType;
            tr.io_ooo_to_mem_enqLsq_req_0_bits_uopIdx = uopIdx;
            tr.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_flag = rob_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_0_bits_robIdx_value = rob_key.value;
            tr.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_flag = lq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_0_bits_lqIdx_value = lq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_flag = sq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_0_bits_sqIdx_value = sq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_0_bits_numLsElem = numLsElem;
        end
        1: begin
            tr.io_ooo_to_mem_enqLsq_req_1_valid = valid;
            tr.io_ooo_to_mem_enqLsq_req_1_bits_fuType = fuType;
            tr.io_ooo_to_mem_enqLsq_req_1_bits_uopIdx = uopIdx;
            tr.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_flag = rob_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_1_bits_robIdx_value = rob_key.value;
            tr.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_flag = lq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_1_bits_lqIdx_value = lq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_flag = sq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_1_bits_sqIdx_value = sq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_1_bits_numLsElem = numLsElem;
        end
        2: begin
            tr.io_ooo_to_mem_enqLsq_req_2_valid = valid;
            tr.io_ooo_to_mem_enqLsq_req_2_bits_fuType = fuType;
            tr.io_ooo_to_mem_enqLsq_req_2_bits_uopIdx = uopIdx;
            tr.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_flag = rob_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_2_bits_robIdx_value = rob_key.value;
            tr.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_flag = lq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_2_bits_lqIdx_value = lq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_flag = sq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_2_bits_sqIdx_value = sq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_2_bits_numLsElem = numLsElem;
        end
        3: begin
            tr.io_ooo_to_mem_enqLsq_req_3_valid = valid;
            tr.io_ooo_to_mem_enqLsq_req_3_bits_fuType = fuType;
            tr.io_ooo_to_mem_enqLsq_req_3_bits_uopIdx = uopIdx;
            tr.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_flag = rob_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_3_bits_robIdx_value = rob_key.value;
            tr.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_flag = lq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_3_bits_lqIdx_value = lq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_flag = sq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_3_bits_sqIdx_value = sq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_3_bits_numLsElem = numLsElem;
        end
        4: begin
            tr.io_ooo_to_mem_enqLsq_req_4_valid = valid;
            tr.io_ooo_to_mem_enqLsq_req_4_bits_fuType = fuType;
            tr.io_ooo_to_mem_enqLsq_req_4_bits_uopIdx = uopIdx;
            tr.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_flag = rob_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_4_bits_robIdx_value = rob_key.value;
            tr.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_flag = lq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_4_bits_lqIdx_value = lq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_flag = sq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_4_bits_sqIdx_value = sq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_4_bits_numLsElem = numLsElem;
        end
        5: begin
            tr.io_ooo_to_mem_enqLsq_req_5_valid = valid;
            tr.io_ooo_to_mem_enqLsq_req_5_bits_fuType = fuType;
            tr.io_ooo_to_mem_enqLsq_req_5_bits_uopIdx = uopIdx;
            tr.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_flag = rob_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_5_bits_robIdx_value = rob_key.value;
            tr.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_flag = lq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_5_bits_lqIdx_value = lq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_flag = sq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_5_bits_sqIdx_value = sq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_5_bits_numLsElem = numLsElem;
        end
        6: begin
            tr.io_ooo_to_mem_enqLsq_req_6_valid = valid;
            tr.io_ooo_to_mem_enqLsq_req_6_bits_fuType = fuType;
            tr.io_ooo_to_mem_enqLsq_req_6_bits_uopIdx = uopIdx;
            tr.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_flag = rob_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_6_bits_robIdx_value = rob_key.value;
            tr.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_flag = lq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_6_bits_lqIdx_value = lq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_flag = sq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_6_bits_sqIdx_value = sq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_6_bits_numLsElem = numLsElem;
        end
        7: begin
            tr.io_ooo_to_mem_enqLsq_req_7_valid = valid;
            tr.io_ooo_to_mem_enqLsq_req_7_bits_fuType = fuType;
            tr.io_ooo_to_mem_enqLsq_req_7_bits_uopIdx = uopIdx;
            tr.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_flag = rob_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_7_bits_robIdx_value = rob_key.value;
            tr.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_flag = lq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_7_bits_lqIdx_value = lq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_flag = sq_key.flag;
            tr.io_ooo_to_mem_enqLsq_req_7_bits_sqIdx_value = sq_key.value;
            tr.io_ooo_to_mem_enqLsq_req_7_bits_numLsElem = numLsElem;
        end
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("set_req_fields got unsupported slot=%0d", slot))
        end
    endcase
endfunction:set_req_fields

function void memblock_lsqenq_dispatch_base_sequence::get_resp_keys(input lsqenq_agent_agent_xaction tr,
                                                               input int unsigned slot,
                                                               output memblock_lq_key_t lq_key,
                                                               output memblock_sq_key_t sq_key);
    lq_key = '{default:'0};
    sq_key = '{default:'0};
    case (slot)
        0: begin
            lq_key.flag  = tr.io_ooo_to_mem_enqLsq_resp_0_lqIdx_flag;
            lq_key.value = tr.io_ooo_to_mem_enqLsq_resp_0_lqIdx_value;
            sq_key.flag  = tr.io_ooo_to_mem_enqLsq_resp_0_sqIdx_flag;
            sq_key.value = tr.io_ooo_to_mem_enqLsq_resp_0_sqIdx_value;
        end
        1: begin
            lq_key.flag  = tr.io_ooo_to_mem_enqLsq_resp_1_lqIdx_flag;
            lq_key.value = tr.io_ooo_to_mem_enqLsq_resp_1_lqIdx_value;
            sq_key.flag  = tr.io_ooo_to_mem_enqLsq_resp_1_sqIdx_flag;
            sq_key.value = tr.io_ooo_to_mem_enqLsq_resp_1_sqIdx_value;
        end
        2: begin
            lq_key.flag  = tr.io_ooo_to_mem_enqLsq_resp_2_lqIdx_flag;
            lq_key.value = tr.io_ooo_to_mem_enqLsq_resp_2_lqIdx_value;
            sq_key.flag  = tr.io_ooo_to_mem_enqLsq_resp_2_sqIdx_flag;
            sq_key.value = tr.io_ooo_to_mem_enqLsq_resp_2_sqIdx_value;
        end
        3: begin
            lq_key.flag  = tr.io_ooo_to_mem_enqLsq_resp_3_lqIdx_flag;
            lq_key.value = tr.io_ooo_to_mem_enqLsq_resp_3_lqIdx_value;
            sq_key.flag  = tr.io_ooo_to_mem_enqLsq_resp_3_sqIdx_flag;
            sq_key.value = tr.io_ooo_to_mem_enqLsq_resp_3_sqIdx_value;
        end
        4: begin
            lq_key.flag  = tr.io_ooo_to_mem_enqLsq_resp_4_lqIdx_flag;
            lq_key.value = tr.io_ooo_to_mem_enqLsq_resp_4_lqIdx_value;
            sq_key.flag  = tr.io_ooo_to_mem_enqLsq_resp_4_sqIdx_flag;
            sq_key.value = tr.io_ooo_to_mem_enqLsq_resp_4_sqIdx_value;
        end
        5: begin
            lq_key.flag  = tr.io_ooo_to_mem_enqLsq_resp_5_lqIdx_flag;
            lq_key.value = tr.io_ooo_to_mem_enqLsq_resp_5_lqIdx_value;
            sq_key.flag  = tr.io_ooo_to_mem_enqLsq_resp_5_sqIdx_flag;
            sq_key.value = tr.io_ooo_to_mem_enqLsq_resp_5_sqIdx_value;
        end
        6: begin
            lq_key.flag  = tr.io_ooo_to_mem_enqLsq_resp_6_lqIdx_flag;
            lq_key.value = tr.io_ooo_to_mem_enqLsq_resp_6_lqIdx_value;
            sq_key.flag  = tr.io_ooo_to_mem_enqLsq_resp_6_sqIdx_flag;
            sq_key.value = tr.io_ooo_to_mem_enqLsq_resp_6_sqIdx_value;
        end
        7: begin
            lq_key.flag  = tr.io_ooo_to_mem_enqLsq_resp_7_lqIdx_flag;
            lq_key.value = tr.io_ooo_to_mem_enqLsq_resp_7_lqIdx_value;
            sq_key.flag  = tr.io_ooo_to_mem_enqLsq_resp_7_sqIdx_flag;
            sq_key.value = tr.io_ooo_to_mem_enqLsq_resp_7_sqIdx_value;
        end
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("get_resp_keys got unsupported slot=%0d", slot))
        end
    endcase
endfunction:get_resp_keys

function void memblock_lsqenq_dispatch_base_sequence::confirm_lsq_candidates(input lsqenq_agent_agent_xaction tr,
                                                                        input memblock_uid_t uids[$],
                                                                        input main_control_transaction trs[$],
                                                                        input memblock_op_behavior_t behaviors[$],
                                                                        output bit has_progress);
    has_progress = 1'b0;
    if (tr.memblock_dispatch_aborted_by_redirect ||
        admission_blocked_by_flush() ||
        tr.memblock_dispatch_flush_epoch != memblock_sync_pkg::dispatch_flush_epoch) begin
        `uvm_info(get_type_name(), "skip LSQ enqueue confirmation because redirect/flush is in progress", UVM_LOW)
        return;
    end
    foreach (uids[idx]) begin
        memblock_lq_key_t dut_lq_key;
        memblock_sq_key_t dut_sq_key;

        get_resp_keys(tr, idx, dut_lq_key, dut_sq_key);
        lsq_ctrl.commit_allocate_with_resp(uids[idx], behaviors[idx], trs[idx], dut_lq_key, dut_sq_key);
        complete_admission(uids[idx]);
        has_progress = 1'b1;
    end
endfunction:confirm_lsq_candidates

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
