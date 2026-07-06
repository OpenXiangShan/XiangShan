//=========================================================
//File name    : memblock_issue_dispatch_base_sequence.sv
//Author       : OpenAI_Codex
//Module name  : memblock_issue_dispatch_base_sequence
//Discribution : lintsissue dispatch issue driver sequence
//Date         : 2026-05-18
//=========================================================
`ifndef MEMBLOCK_ISSUE_DISPATCH_BASE_SEQUENCE__SV
`define MEMBLOCK_ISSUE_DISPATCH_BASE_SEQUENCE__SV

class memblock_issue_dispatch_base_sequence extends lintsissue_agent_agent_default_sequence;

    common_data_transaction data;
    issue_queue_scheduler   issue_sched;
    issue_field_assigner    field_assigner;
    writeback_status_handler issue_accept_wb_handler;

    bit          enable;
    int unsigned no_progress_warn_cycles;

    `uvm_object_utils(memblock_issue_dispatch_base_sequence)

    extern function new(string name = "memblock_issue_dispatch_base_sequence");
    extern virtual task pre_body();
    extern virtual task body();
    extern virtual task drive_dispatch_issue_loop();
    extern virtual task send_issue_cycle(input int unsigned cycle_idx,
                                         output bit has_fire);
    extern task wait_for_main_table();
    extern function void configure_from_plus();
    extern function void ensure_helpers();
    extern function void assign_issue_items(input lintsissue_agent_agent_xaction tr,
                                            input memblock_issue_q_item_t items[$],
                                            ref memblock_issue_q_item_t fired_items[$]);
    extern function void mark_fired_items(input memblock_issue_q_item_t fired_items[$],
                                          input bit [6:0] fired_mask);
    extern function bit item_needs_issue_accept_pass(input memblock_issue_q_item_t item);
    extern function memblock_wb_event_t make_issue_accept_pass_event(input memblock_issue_q_item_t item);
    extern function void submit_issue_accept_pass(input memblock_issue_q_item_t item);

endclass:memblock_issue_dispatch_base_sequence

function memblock_issue_dispatch_base_sequence::new(string name = "memblock_issue_dispatch_base_sequence");
    super.new(name);
    enable = 1'b0;
    no_progress_warn_cycles = 10000;
endfunction:new

task memblock_issue_dispatch_base_sequence::pre_body();
    super.pre_body();
endtask:pre_body

task memblock_issue_dispatch_base_sequence::body();
    seq_csr_common::init();
    configure_from_plus();
    if (!enable) begin
        `uvm_info(get_type_name(), "MEMBLOCK_DISPATCH_ISSUE_SEQ_EN=0, lintsissue dispatch sequence stays idle", UVM_LOW)
        return;
    end
    ensure_helpers();
    wait_for_main_table();
    drive_dispatch_issue_loop();
endtask:body

task memblock_issue_dispatch_base_sequence::drive_dispatch_issue_loop();
    int unsigned cycle_idx;
    int unsigned idle_count;

    cycle_idx = 0;
    idle_count = 0;
    forever begin
        bit has_fire;

        issue_sched.route_all_ready_uids();
        send_issue_cycle(cycle_idx, has_fire);
        issue_sched.advance_issue_queue_delays();

        if (data.is_global_stop_requested()) begin
            `uvm_info(get_type_name(),
                      $sformatf("stop dispatch issue loop by global_stop_requested at cycle=%0d",
                                cycle_idx),
                      UVM_LOW)
            break;
        end

        if (has_fire) begin
            idle_count = 0;
        end else begin
            idle_count++;
            if (no_progress_warn_cycles != 0 &&
                idle_count >= no_progress_warn_cycles) begin
                `uvm_warning(get_type_name(),
                             $sformatf("no issue fire for %0d cycles: cycle=%0d terminal_done_uid=%0d main_trans_num=%0d load_q=%0d sta_q=%0d std_q=%0d",
                                       idle_count,
                                       cycle_idx,
                                       data.dispatch_progress.terminal_done_uid,
                                       data.main_trans_num,
                                       data.load_issue_q.size(),
                                       data.sta_issue_q.size(),
                                       data.std_issue_q.size()))
                idle_count = 0;
            end
        end
        cycle_idx++;
    end
endtask:drive_dispatch_issue_loop

task memblock_issue_dispatch_base_sequence::send_issue_cycle(input int unsigned cycle_idx,
                                                             output bit has_fire);
    lintsissue_agent_agent_xaction tr;
    memblock_issue_q_item_t load_items[$];
    memblock_issue_q_item_t sta_items[$];
    memblock_issue_q_item_t std_items[$];
    memblock_issue_q_item_t fired_items[$];

    has_fire = 1'b0;
    tr = lintsissue_agent_agent_xaction::type_id::create($sformatf("lintsissue_dispatch_tr_%0d", cycle_idx));
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "failed to create lintsissue xaction")
    end

    field_assigner.clear_lintsissue_xaction(tr);
    // 中文注释：以下 memblock_dispatch_* 字段只用于测试框架 driver/sequence 协作，
    // 不是发给 DUT 的 intIssue payload。它们用于处理 valid/ready 等待、timeout、
    // redirect/flush 边界拍 partial fire，以及只标记 DUT 真正接收的 issue port。
    // wait_ready=1 表示 driver 在 send_pkt 后继续等待所有 valid port 被 DUT ready 接收。
    tr.memblock_dispatch_wait_ready = 1'b1;
    // nonblocking_issue=1 表示 driver 只采样一次 ready；未 fire port 不出队，下轮重新参与仲裁。
    tr.memblock_dispatch_nonblocking_issue = seq_csr_common::get_dispatch_issue_nonblocking_en();
    // ready_timeout 控制等待 ready 的最大周期数，避免 DUT 长时间不 ready 时仿真卡死。
    tr.memblock_dispatch_ready_timeout = seq_csr_common::get_dispatch_ready_timeout();
    // aborted_by_redirect 由 driver 回填；等待 ready 期间遇到 redirect/flush 时置高。
    tr.memblock_dispatch_aborted_by_redirect = 1'b0;
    // flush_epoch 记录本次发射开始时的全局 flush 版本，用于识别等待 ready 期间是否跨过 flush。
    tr.memblock_dispatch_flush_epoch = memblock_sync_pkg::dispatch_flush_epoch;
    // fired_mask 由 driver 回填，bit[0:2] 对应 load port，bit[3:4] 对应 STA，bit[5:6] 对应 STD。
    tr.memblock_dispatch_fired_mask = '0;
    if (!data.issue_blocked_by_global_flush()) begin
        issue_sched.select_issue_candidates(load_items, sta_items, std_items);
        if (!data.issue_blocked_by_global_flush()) begin
            assign_issue_items(tr, load_items, fired_items);
            assign_issue_items(tr, sta_items, fired_items);
            assign_issue_items(tr, std_items, fired_items);
        end
    end

    start_item(tr);
    finish_item(tr);

    if (tr.memblock_dispatch_aborted_by_redirect) begin
        if (tr.memblock_dispatch_fired_mask != '0) begin
            mark_fired_items(fired_items, tr.memblock_dispatch_fired_mask);
            has_fire = 1'b1;
        end
        if (fired_items.size() != 0) begin
            `uvm_info(get_type_name(), "partial issue fire marking after redirect abort", UVM_LOW)
        end
        return;
    end

    if (data.issue_blocked_by_global_flush() ||
        tr.memblock_dispatch_flush_epoch != memblock_sync_pkg::dispatch_flush_epoch) begin
        if (fired_items.size() != 0) begin
            `uvm_info(get_type_name(), "skip issue fire marking because redirect/flush is in progress", UVM_LOW)
        end
        return;
    end

    if (fired_items.size() != 0) begin
        bit [6:0] effective_fired_mask;

        if (tr.memblock_dispatch_nonblocking_issue) begin
            effective_fired_mask = tr.memblock_dispatch_fired_mask;
        end else begin
            effective_fired_mask = 7'h7f;
        end

        if (effective_fired_mask != '0) begin
            mark_fired_items(fired_items, effective_fired_mask);
            has_fire = 1'b1;
        end
    end
endtask:send_issue_cycle

task memblock_issue_dispatch_base_sequence::wait_for_main_table();
    int unsigned wait_count;

    wait_count = 0;
    while (!data.main_table_ready) begin
        if (no_progress_warn_cycles != 0 &&
            wait_count != 0 &&
            (wait_count % no_progress_warn_cycles) == 0) begin
            `uvm_warning(get_type_name(),
                         $sformatf("still waiting for main table before dispatch issue: wait_count=%0d main_trans_num=%0d next_uid=%0d",
                                   wait_count,
                                   data.main_trans_num,
                                   data.next_uid))
        end
        #1;
        wait_count++;
    end
endtask:wait_for_main_table

function void memblock_issue_dispatch_base_sequence::configure_from_plus();
    enable = seq_csr_common::get_dispatch_issue_seq_en();
    no_progress_warn_cycles = seq_csr_common::get_active_seq_no_progress_warn_cycles();
endfunction:configure_from_plus

function void memblock_issue_dispatch_base_sequence::ensure_helpers();
    data = common_data_transaction::get();
    if (issue_sched == null) begin
        issue_sched = issue_queue_scheduler::type_id::create("issue_sched");
    end
    if (field_assigner == null) begin
        field_assigner = issue_field_assigner::type_id::create("field_assigner");
    end
    if (issue_accept_wb_handler == null) begin
        issue_accept_wb_handler = writeback_status_handler::type_id::create("issue_accept_wb_handler");
    end
    if (data == null || issue_sched == null || field_assigner == null || issue_accept_wb_handler == null) begin
        `uvm_fatal(get_type_name(), "failed to initialize dispatch issue helpers")
    end
endfunction:ensure_helpers

function void memblock_issue_dispatch_base_sequence::assign_issue_items(input lintsissue_agent_agent_xaction tr,
                                                                        input memblock_issue_q_item_t items[$],
                                                                        ref memblock_issue_q_item_t fired_items[$]);
    foreach (items[idx]) begin
        int unsigned pipe_idx;
        memblock_issue_q_item_t fired_item;

        pipe_idx = idx;
        field_assigner.assign_issue_item_fields(tr, items[idx], pipe_idx);
        fired_item = items[idx];
        fired_item.uop_index = pipe_idx;
        fired_items.push_back(fired_item);
    end
endfunction:assign_issue_items

function void memblock_issue_dispatch_base_sequence::mark_fired_items(input memblock_issue_q_item_t fired_items[$],
                                                                      input bit [6:0] fired_mask);
    foreach (fired_items[idx]) begin
        int unsigned port_idx;
        bit          fire_marked;

        port_idx = fired_items[idx].uop_index;
        case (fired_items[idx].target)
            MEMBLOCK_ISSUE_TARGET_LOAD: port_idx = fired_items[idx].uop_index;
            MEMBLOCK_ISSUE_TARGET_STA:  port_idx = fired_items[idx].uop_index + 3;
            MEMBLOCK_ISSUE_TARGET_STD:  port_idx = fired_items[idx].uop_index + 5;
            default: begin
                `uvm_fatal(get_type_name(),
                           $sformatf("mark_fired_items got unsupported target=%0d",
                                     fired_items[idx].target))
            end
        endcase
        if (!fired_mask[port_idx]) begin
            continue;
        end
        if (data.issue_blocked_by_global_flush()) begin
            fire_marked = issue_sched.mark_issue_fire_already_accepted(fired_items[idx]);
        end else begin
            fire_marked = issue_sched.mark_issue_fire(fired_items[idx]);
        end
        if (!fire_marked) begin
            `uvm_warning(get_type_name(),
                         $sformatf("skip stale issue item uid=%0d target=%0d",
                                   fired_items[idx].uid,
                                   fired_items[idx].target))
        end else begin
            submit_issue_accept_pass(fired_items[idx]);
        end
    end
endfunction:mark_fired_items

function bit memblock_issue_dispatch_base_sequence::item_needs_issue_accept_pass(input memblock_issue_q_item_t item);
    main_control_transaction main_tr;

    ensure_helpers();
    if (seq_csr_common::get_std_real_wb_pass_en()) begin
        return 1'b0;
    end
    if (item.target != MEMBLOCK_ISSUE_TARGET_STD || !data.is_valid_uid(item.uid)) begin
        return 1'b0;
    end
    main_tr = data.get_main_transaction(item.uid);
    return main_tr.op_class == MEMBLOCK_OP_CLASS_STORE &&
           main_tr.fuType == MEMBLOCK_FUTYPE_STU &&
           lsq_ctrl_model::is_store_fuoptype(main_tr.fuOpType);
endfunction:item_needs_issue_accept_pass

function memblock_wb_event_t memblock_issue_dispatch_base_sequence::make_issue_accept_pass_event(input memblock_issue_q_item_t item);
    memblock_wb_event_t     wb_event;
    status_transaction      status;

    ensure_helpers();
    status = data.get_status(item.uid);
    wb_event = data.make_empty_wb_event();
    wb_event.valid               = 1'b1;
    wb_event.source              = MEMBLOCK_WB_EVENT_SOURCE_STD_FEEDBACK;
    wb_event.port_id             = item.uop_index;
    wb_event.target              = MEMBLOCK_ISSUE_TARGET_STD;
    wb_event.uid                 = item.uid;
    wb_event.has_uid             = 1'b1;
    wb_event.rob_key             = item.rob_key;
    wb_event.has_rob             = 1'b1;
    wb_event.sq_key              = item.sq_key;
    wb_event.has_sq              = item.has_sqIdx;
    wb_event.issue_epoch         = status.get_target_issue_epoch(MEMBLOCK_ISSUE_TARGET_STD);
    wb_event.has_issue_epoch     = 1'b1;
    wb_event.replay_seq          = item.replay_seq;
    wb_event.has_replay_seq      = 1'b1;
    // 中文注释：STD issue-accept pass 是兼容路径，只表示 STD issue response 成功。
    // 当 MEMBLOCK_STD_REAL_WB_PASS_EN=0 时，handler 才会把该 feedback 转成 target pass。
    wb_event.iq_feedback_valid   = 1'b1;
    wb_event.iq_feedback_hit     = 1'b1;
    wb_event.iq_feedback_failed  = 1'b0;
    wb_event.cycle               = $time;
    return wb_event;
endfunction:make_issue_accept_pass_event

function void memblock_issue_dispatch_base_sequence::submit_issue_accept_pass(input memblock_issue_q_item_t item);
    memblock_wb_event_t wb_event;

    if (!item_needs_issue_accept_pass(item)) begin
        return;
    end
    wb_event = make_issue_accept_pass_event(item);
    if (issue_accept_wb_handler.handle_event(wb_event)) begin
        `uvm_info(get_type_name(),
                  $sformatf("issue-accept STD pass uid=%0d rob=%0d:%0d sq=%0d:%0d",
                            item.uid,
                            item.rob_key.flag,
                            item.rob_key.value,
                            item.sq_key.flag,
                            item.sq_key.value),
                  UVM_LOW)
    end
endfunction:submit_issue_accept_pass

`endif
