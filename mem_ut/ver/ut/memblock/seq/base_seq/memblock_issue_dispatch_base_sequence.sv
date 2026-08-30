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
    extern function int unsigned port_idx_for_item(input memblock_issue_q_item_t item);
    extern function void mark_fired_items(input memblock_issue_q_item_t fired_items[$],
                                          input bit [MEMBLOCK_DUT_SCALAR_ISSUE_MASK_W-1:0] fired_mask,
                                          ref bit [MEMBLOCK_DUT_SCALAR_ISSUE_MASK_W-1:0] bookkept_fired_mask);

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
        bit pending_issue_work;

        issue_sched.route_all_ready_uids();
        send_issue_cycle(cycle_idx, has_fire);
        issue_sched.advance_issue_queue_delays();
        pending_issue_work = issue_sched.has_pending_issue_work();

        if (data.is_global_stop_requested()) begin
            `uvm_info(get_type_name(),
                      $sformatf("stop dispatch issue loop by global_stop_requested at cycle=%0d",
                                cycle_idx),
                      UVM_LOW)
            break;
        end

        if (has_fire) begin
            idle_count = 0;
        end else if (pending_issue_work) begin
            idle_count++;
            if (no_progress_warn_cycles != 0 &&
                (idle_count % no_progress_warn_cycles) == 0) begin
                `uvm_error(get_type_name(),
                           $sformatf("issue queue has pending work but no fire for %0d issue-loop iterations: iteration=%0d terminal_done_uid=%0d main_trans_num=%0d load_q=%0d sta_q=%0d std_q=%0d",
                                     idle_count,
                                     cycle_idx,
                                     data.dispatch_progress.terminal_done_uid,
                                     data.main_trans_num,
                                     data.load_issue_q.size(),
                                     data.sta_issue_q.size(),
                                     data.std_issue_q.size()))
            end
        end else begin
            // 队列已空时可能仍在等待 writeback/commit/deq/terminal，不能把它当作 issue stall。
            idle_count = 0;
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
    bit [MEMBLOCK_DUT_SCALAR_ISSUE_MASK_W-1:0] candidate_mask;
    bit [MEMBLOCK_DUT_SCALAR_ISSUE_MASK_W-1:0] effective_fired_mask;
    // 中文注释：只记录当前 xaction 已由真实握手 watcher 完成的 port；finish_item()
    // 返回后的补偿只处理该 mask 尚未覆盖的 bit，禁止为同一 fire 重复分配 issue epoch。
    bit [MEMBLOCK_DUT_SCALAR_ISSUE_MASK_W-1:0] bookkept_fired_mask;
    bit flush_or_epoch_changed;

    has_fire = 1'b0;
    tr = lintsissue_agent_agent_xaction::type_id::create($sformatf("lintsissue_dispatch_tr_%0d", cycle_idx));
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "failed to create lintsissue xaction")
    end

    field_assigner.clear_lintsissue_xaction(tr);
    // 当前 issue xaction 是一个周期性的 valid/ready item，不允许父类随机 gap 让
    // finish_item() 时序偏离真实握手记账边界。
    tr.pre_pkt_gap = 0;
    tr.post_pkt_gap = 0;
    // 中文注释：以下 memblock_dispatch_* 字段只用于测试框架 driver/sequence 协作，
    // 不是发给 DUT 的 split issue payload。它们用于处理 valid/ready 等待、timeout、
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
    // fired_mask 由 driver 回填，target 区间由 compile-time port base/count 派生。
    tr.memblock_dispatch_fired_mask = '0;
    bookkept_fired_mask = '0;
    if (!data.issue_blocked_by_global_flush()) begin
        issue_sched.select_issue_candidates(load_items, sta_items, std_items);
        if (!data.issue_blocked_by_global_flush()) begin
            assign_issue_items(tr, load_items, fired_items);
            assign_issue_items(tr, sta_items, fired_items);
            assign_issue_items(tr, std_items, fired_items);
        end
    end

    candidate_mask = '0;
    foreach (fired_items[idx]) begin
        candidate_mask[port_idx_for_item(fired_items[idx])] = 1'b1;
    end

    start_item(tr);
    // 中文注释：fired_mask 是 driver 在真实 valid&&ready 后置位的持久状态。watcher
    // 等待尚未记账的 bit，而不是等待瞬时 event；即使 driver 先于 watcher 运行，条件
    // 仍保持为真，随后仍会在 finish_item() 返回前完成 scheduler 记账。
    fork : dispatch_fire_bookkeeping_watcher
        begin
            forever begin
                wait ((tr.memblock_dispatch_fired_mask & candidate_mask &
                       ~bookkept_fired_mask) != '0);
                mark_fired_items(fired_items,
                                 tr.memblock_dispatch_fired_mask & candidate_mask,
                                 bookkept_fired_mask);
            end
        end
    join_none
    finish_item(tr);

    effective_fired_mask = tr.memblock_dispatch_fired_mask & candidate_mask;
    if ((tr.memblock_dispatch_fired_mask & ~candidate_mask) != '0) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("driver returned fired bits outside candidate mask: fired=0x%0h candidate=0x%0h",
                             tr.memblock_dispatch_fired_mask, candidate_mask))
    end
    flush_or_epoch_changed = data.issue_blocked_by_global_flush() ||
                             tr.memblock_dispatch_flush_epoch != memblock_sync_pkg::dispatch_flush_epoch;

    if (!tr.memblock_dispatch_aborted_by_redirect && !flush_or_epoch_changed &&
        !tr.memblock_dispatch_nonblocking_issue &&
        effective_fired_mask != candidate_mask) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("blocking issue completed without all candidate fires: fired=0x%0h candidate=0x%0h",
                             effective_fired_mask, candidate_mask))
    end

    if (effective_fired_mask != '0) begin
        // item_done 与 watcher 位于同一 UVM delta 时，先等待 watcher 把当前
        // fired bit 全部消费；随后只对未覆盖 bit 做一次补偿，避免双重 scheduler 更新。
        wait ((effective_fired_mask & ~bookkept_fired_mask) == '0);
        mark_fired_items(fired_items, effective_fired_mask, bookkept_fired_mask);
        has_fire = 1'b1;
    end
    disable dispatch_fire_bookkeeping_watcher;

    if (tr.memblock_dispatch_aborted_by_redirect) begin
        if (fired_items.size() != 0) begin
            `uvm_info(get_type_name(), "partial issue fire marking after redirect abort", UVM_LOW)
        end
        return;
    end

    if (flush_or_epoch_changed) begin
        if (fired_items.size() != 0) begin
            `uvm_info(get_type_name(), "cancel unfired issue candidates because redirect/flush is in progress", UVM_LOW)
        end
        return;
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
    if (data == null || issue_sched == null || field_assigner == null) begin
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

function int unsigned memblock_issue_dispatch_base_sequence::port_idx_for_item(input memblock_issue_q_item_t item);
    int unsigned port_idx;
    int unsigned pipe_limit;

    case (item.target)
        MEMBLOCK_ISSUE_TARGET_LOAD: begin
            pipe_limit = MEMBLOCK_DUT_LOAD_PIPE_NUM;
            port_idx = MEMBLOCK_DUT_LOAD_PORT_BASE + item.uop_index;
        end
        MEMBLOCK_ISSUE_TARGET_STA: begin
            pipe_limit = MEMBLOCK_DUT_STA_PIPE_NUM;
            port_idx = MEMBLOCK_DUT_STA_PORT_BASE + item.uop_index;
        end
        MEMBLOCK_ISSUE_TARGET_STD: begin
            pipe_limit = MEMBLOCK_DUT_STD_PIPE_NUM;
            port_idx = MEMBLOCK_DUT_STD_PORT_BASE + item.uop_index;
        end
        default: begin
            `uvm_fatal(get_type_name(),
                       $sformatf("port_idx_for_item got unsupported target=%0d", item.target))
            return 0;
        end
    endcase
    if (item.uop_index >= pipe_limit) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("port_idx_for_item target=%0d local_pipe=%0d limit=%0d",
                             item.target, item.uop_index, pipe_limit))
    end
    if (port_idx >= MEMBLOCK_DUT_SCALAR_ISSUE_PORT_NUM ||
        port_idx >= MEMBLOCK_DUT_SCALAR_ISSUE_MASK_W) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("port_idx_for_item target=%0d local_pipe=%0d gives port=%0d total=%0d mask_w=%0d",
                             item.target, item.uop_index, port_idx,
                             MEMBLOCK_DUT_SCALAR_ISSUE_PORT_NUM,
                             MEMBLOCK_DUT_SCALAR_ISSUE_MASK_W))
    end
    return port_idx;
endfunction:port_idx_for_item

function void memblock_issue_dispatch_base_sequence::mark_fired_items(input memblock_issue_q_item_t fired_items[$],
                                                                      input bit [MEMBLOCK_DUT_SCALAR_ISSUE_MASK_W-1:0] fired_mask,
                                                                      ref bit [MEMBLOCK_DUT_SCALAR_ISSUE_MASK_W-1:0] bookkept_fired_mask);
    foreach (fired_items[idx]) begin
        int unsigned port_idx;
        bit          fire_marked;

        port_idx = port_idx_for_item(fired_items[idx]);
        if (!fired_mask[port_idx] || bookkept_fired_mask[port_idx]) begin
            continue;
        end
        if (data.issue_blocked_by_global_flush()) begin
            fire_marked = issue_sched.mark_issue_fire_already_accepted(fired_items[idx]);
        end else begin
            fire_marked = issue_sched.mark_issue_fire(fired_items[idx]);
        end
        if (!fire_marked) begin
            // 中文注释：candidate 已由 driver 真实 fire，但 fault 可能在 watcher
            // 记账前置位 exception_pending；仅此时尝试严格的冻结实例 fallback。
            fire_marked = issue_sched.mark_issue_fire_from_frozen_candidate(fired_items[idx]);
        end
        if (!fire_marked) begin
            `uvm_warning(get_type_name(),
                         $sformatf("skip stale issue item uid=%0d target=%0d",
                                   fired_items[idx].uid,
                                   fired_items[idx].target))
        end
        // 中文注释：该 port 已经过真实握手并完成一次 scheduler 尝试。即使 recovery
        // 已使 candidate 失效，也不能让 finish_item() 补偿路径对同一 port 重复尝试。
        bookkept_fired_mask[port_idx] = 1'b1;
    end
endfunction:mark_fired_items

`endif
