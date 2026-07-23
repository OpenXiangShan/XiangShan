//=========================================================
//File name    : memblock_l2tlb_base_sequence.sv
//Author       : OpenAI_Codex
//Module name  : memblock_l2tlb_base_sequence
//Discribution : bounded DTLB -> L2TLB request / L2TLB -> DTLB response service
//Date         : 2026-05-18
//=========================================================
`ifndef MEMBLOCK_L2TLB_BASE_SEQUENCE__SV
`define MEMBLOCK_L2TLB_BASE_SEQUENCE__SV

typedef enum int unsigned {
    // request在下一sample边界即可最早完成response。
    L2TLB_LATENCY_1C,
    // request使用MEMBLOCK_L2TLB_RESP_MID_LATENCY作为最早response间隔。
    L2TLB_LATENCY_MID,
    // request使用MEMBLOCK_L2TLB_RESP_LONG_LATENCY作为最早response间隔。
    L2TLB_LATENCY_LONG
} memblock_l2tlb_latency_bucket_e;

class memblock_l2tlb_pending_req extends uvm_object;
    // 中文注释：每次DTLB request fire分配一个单调token，仅用于测试框架生命周期审计。
    // reset/flush取消也保留accepted分类，不写入任何DUT response字段。
    longint unsigned request_token;
    bit [37:0] vpn;
    bit [1:0] s2xlate;
    // 中文注释：request fire时冻结的CSR、lookup key、TLB entry和response payload。
    // response等待期间live CSR/table变化不会回写这些快照。
    mmu_csr_runtime_state csr_snapshot;
    memblock_tlb_lookup_key_t lookup_key;
    memblock_tlb_entry entry_snapshot;
    L2tlb_agent_agent_xaction resp_tr;
    // 中文注释：accept/due序号定义最早response边界；complete允许因端口竞争晚于due。
    longint unsigned accept_sample_seq;
    memblock_l2tlb_latency_bucket_e latency_bucket;
    int unsigned min_latency;
    longint unsigned due_sample_seq;
    // 中文注释：接受request时已观察到的flush event版本；新event只取消更旧版本的pending。
    longint unsigned accept_flush_event_seq;

    `uvm_object_utils(memblock_l2tlb_pending_req)

    function new(string name = "memblock_l2tlb_pending_req");
        super.new(name);
        request_token = 0;
        vpn = '0;
        s2xlate = '0;
        csr_snapshot = null;
        lookup_key = '{default:'0};
        entry_snapshot = null;
        resp_tr = null;
        accept_sample_seq = 0;
        latency_bucket = L2TLB_LATENCY_1C;
        min_latency = 1;
        due_sample_seq = 0;
        accept_flush_event_seq = 0;
    endfunction:new
endclass:memblock_l2tlb_pending_req

class memblock_l2tlb_base_sequence extends L2tlb_agent_agent_default_sequence;

    common_data_transaction data;
    virtual L2tlb_agent_agent_interface l2tlb_vif;

    // 中文注释：seq_csr_common完成合法性检查和compile资源收敛后，由configure_from_plus()冻结。
    bit          enable;
    int unsigned max_outstanding;
    bit          resp_reorder_en;
    int unsigned resp_mid_latency;
    int unsigned resp_long_latency;
    int unsigned resp_1c_wt;
    int unsigned resp_mid_wt;
    int unsigned resp_long_wt;
    int unsigned idle_stop_cycle;

    // 中文注释：pending_q保存已fire但尚未放上response端口的token；driving_req保存已经驱动、
    // 等待下一DUT sample确认完成的唯一token。两者总数始终不超过max_outstanding。
    memblock_l2tlb_pending_req pending_q[$];
    memblock_l2tlb_pending_req driving_req;
    bit driving_valid;

    // 中文注释：累计计数在同一sequence生命周期内跨DUT reset保持单调。
    // 每个accepted token必须落入completed、flush/reset canceled或当前outstanding之一。
    longint unsigned accepted_count;
    longint unsigned completed_count;
    longint unsigned flush_canceled_count;
    longint unsigned reset_canceled_count;
    longint unsigned next_request_token;

    // 中文注释：本地sample/flush/hold状态只由当前lifecycle owner维护。
    // acceptance_opened_since_reset置位后，新flush event必须来自当前sample。
    longint unsigned sample_seq;
    longint unsigned last_seen_flush_event_seq;
    longint unsigned accept_hold_until_sample;
    bit acceptance_opened_since_reset;
    // 中文注释：reset或flush hold解除后，必须至少发出一拍可接受ready，
    // 才允许idle-stop重新计数；该标志与reset freshness独立维护。
    bit ready_opportunity_since_lifecycle_block;
    bit csr_snapshot_valid;
    // 中文注释：DUT reset后必须等待CSR monitor发布一个更新的runtime snapshot sequence。
    // package latest本身不随semantic raw clear而删除，不能把reset前快照用于重新开放ready。
    bit require_post_reset_csr_refresh;
    int unsigned reset_runtime_csr_seq_baseline;
    bit stopping;
    int unsigned idle_count;
    string lifecycle_owner_name;

    // 中文注释：进入每个service tick时立即锁存的真实request握手字段。
    // 后续NBA等待和queue处理只读该快照，不重新读取live VIF。
    bit sampled_req_valid;
    bit sampled_req_ready;
    bit [37:0] sampled_req_vpn;
    bit [1:0] sampled_req_s2xlate;

    `uvm_object_utils(memblock_l2tlb_base_sequence)

    extern function new(string name = "memblock_l2tlb_base_sequence");
    extern virtual task pre_body();
    extern virtual function void do_kill();
    extern virtual task body();
    extern virtual task drive_l2tlb_loop();
    extern virtual task send_l2tlb_cycle(output bit has_progress,
                                         output bit should_exit);
    extern virtual task send_l2tlb_item(input L2tlb_agent_agent_xaction tr);
    extern function void configure_from_plus();
    extern function void ensure_context();
    extern function void initialize_lifecycle_state();
    extern function void drain_csr_runtime_events();
    extern function bit request_fire();
    extern function int unsigned outstanding_count();
    extern function void check_l2tlb_lifecycle_accounting(input string audit_context);
    extern function void cancel_outstanding_by_reset();
    extern function memblock_l2tlb_pending_req capture_fired_request();
    extern function void record_flush_killed_request(input longint unsigned event_seq,
                                                     input time event_sample_time);
    extern function int unsigned handle_l2tlb_flush_event(input longint unsigned event_seq,
                                                          input time event_sample_time,
                                                          output bit request_killed);
    extern function bit select_due_response(input longint unsigned next_sample_seq,
                                            output L2tlb_agent_agent_xaction cycle_tr);
    extern function void complete_driving_response();
    extern function L2tlb_agent_agent_xaction create_l2tlb_xaction(input string name);
    extern function void clear_l2tlb_xaction(input L2tlb_agent_agent_xaction tr);
    extern function void fill_dtlb_resp_from_entry(input memblock_tlb_entry entry,
                                                   ref L2tlb_agent_agent_xaction resp);
    extern function int unsigned choose_latency(output memblock_l2tlb_latency_bucket_e bucket);

endclass:memblock_l2tlb_base_sequence

function memblock_l2tlb_base_sequence::new(string name = "memblock_l2tlb_base_sequence");
    super.new(name);
    enable = 1'b0;
    max_outstanding = 8;
    resp_reorder_en = 1'b0;
    resp_mid_latency = 4;
    resp_long_latency = 16;
    resp_1c_wt = 8;
    resp_mid_wt = 3;
    resp_long_wt = 1;
    idle_stop_cycle = 5000;
    initialize_lifecycle_state();
endfunction:new

task memblock_l2tlb_base_sequence::pre_body();
    super.pre_body();
endtask:pre_body

function void memblock_l2tlb_base_sequence::do_kill();
    string current_owner;

    // 中文注释：UVM sequence.kill()/stop_sequences()不会调用post_body；在被杀之前主动释放owner，
    // 让driver的下一个观察边界离开阻塞get_next_item。正常自然退出时owner已清，该操作是幂等的。
    if (lifecycle_owner_name != "") begin
        void'(memblock_sync_pkg::try_release_l2tlb_lifecycle_owner(
            lifecycle_owner_name, current_owner));
    end
    super.do_kill();
endfunction:do_kill

task memblock_l2tlb_base_sequence::body();
    string current_owner;

    seq_csr_common::init();
    configure_from_plus();
    if (!enable) begin
        return;
    end
    ensure_context();
    if (!memblock_sync_pkg::l2tlb_responder_active) begin
        `uvm_fatal(get_type_name(),
                   "MEMBLOCK_L2TLB_SEQ_EN is enabled but L2TLB connect takeover is not active; enable compile macro MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN")
    end

    lifecycle_owner_name = get_full_name();
    if (!memblock_sync_pkg::try_claim_l2tlb_lifecycle_owner(lifecycle_owner_name,
                                                            current_owner)) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("L2TLB lifecycle owner claim failed: requester=%s current=%s",
                             lifecycle_owner_name, current_owner))
    end
    initialize_lifecycle_state();
    lifecycle_owner_name = get_full_name();
    `uvm_info(get_type_name(),
              $sformatf("L2TLB responder start owner=%s max_outstanding=%0d reorder=%0d latency=1/%0d/%0d weights=%0d/%0d/%0d idle_stop=%0d",
                        lifecycle_owner_name, max_outstanding, resp_reorder_en,
                        resp_mid_latency, resp_long_latency,
                        resp_1c_wt, resp_mid_wt, resp_long_wt, idle_stop_cycle),
              UVM_LOW)

    drive_l2tlb_loop();
    check_l2tlb_lifecycle_accounting("owner_release");
    if (outstanding_count() != 0) begin
        `uvm_fatal(get_type_name(), "attempt to release L2TLB lifecycle owner with outstanding requests")
    end
    if (!memblock_sync_pkg::try_release_l2tlb_lifecycle_owner(lifecycle_owner_name,
                                                              current_owner)) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("L2TLB lifecycle owner release failed: requester=%s current=%s",
                             lifecycle_owner_name, current_owner))
    end
endtask:body

task memblock_l2tlb_base_sequence::drive_l2tlb_loop();
    forever begin
        bit has_progress;
        bit should_exit;

        @(l2tlb_vif.drv_cb);
        sample_seq++;
        send_l2tlb_cycle(has_progress, should_exit);
        if (should_exit) begin
            break;
        end
    end
endtask:drive_l2tlb_loop

// 中文注释：每个drv_cb边界推进一次完整L2TLB lifecycle service。
// 固定顺序为锁存fire、NBA后校验flush、确认response、同步CSR、处理flush/fire、调度下一cycle item。
task memblock_l2tlb_base_sequence::send_l2tlb_cycle(output bit has_progress,
                                                    output bit should_exit);
    longint unsigned flush_event_seq;
    time flush_sample_time;
    bit flush_event_valid;
    bit new_flush_event;
    bit request_killed;
    bit response_selected;
    bit hold_active;
    bit lifecycle_blocked;
    bit next_ready;
    L2tlb_agent_agent_xaction cycle_tr;
    memblock_sync_pkg::dispatch_raw_csr_t ignored_runtime_csr;
    int unsigned latest_runtime_csr_seq;

    has_progress = 1'b0;
    should_exit = 1'b0;
    sampled_req_valid = (l2tlb_vif.drv_cb.io_ptw_req_0_valid === 1'b1);
    sampled_req_ready = (l2tlb_vif.mon_cb.io_ptw_req_0_ready === 1'b1);
    sampled_req_vpn = l2tlb_vif.drv_cb.io_ptw_req_0_bits_vpn;
    sampled_req_s2xlate = l2tlb_vif.drv_cb.io_ptw_req_0_bits_s2xlate;

    uvm_wait_for_nba_region();
    memblock_sync_pkg::get_latest_l2tlb_flush_event(flush_event_seq,
                                                    flush_sample_time,
                                                    flush_event_valid);

    if (l2tlb_vif.rst_n !== 1'b1 ||
        memblock_sync_pkg::reset_backend_done !== 1'b1) begin
        cancel_outstanding_by_reset();
        acceptance_opened_since_reset = 1'b0;
        ready_opportunity_since_lifecycle_block = 1'b0;
        csr_snapshot_valid = 1'b0;
        if (!require_post_reset_csr_refresh) begin
            void'(memblock_sync_pkg::get_latest_runtime_csr_snapshot(
                ignored_runtime_csr, latest_runtime_csr_seq));
            reset_runtime_csr_seq_baseline = latest_runtime_csr_seq;
            require_post_reset_csr_refresh = 1'b1;
        end
        accept_hold_until_sample = 0;
        idle_count = 0;
        stopping = data.is_global_stop_requested();
        if (flush_event_valid) begin
            last_seen_flush_event_seq = flush_event_seq;
        end
        cycle_tr = create_l2tlb_xaction($sformatf("l2tlb_reset_idle_%0d", sample_seq));
        send_l2tlb_item(cycle_tr);
        should_exit = stopping;
        return;
    end

    if (flush_event_valid && flush_event_seq < last_seen_flush_event_seq) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("L2TLB flush event sequence moved backwards: last=%0d latest=%0d",
                             last_seen_flush_event_seq, flush_event_seq))
    end
    new_flush_event = flush_event_valid &&
                      flush_event_seq > last_seen_flush_event_seq;
    if (new_flush_event && acceptance_opened_since_reset &&
        flush_sample_time != $time) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("stale/future L2TLB flush event before lifecycle mutation: event_seq=%0d sample_time=%0t current_time=%0t",
                             flush_event_seq, flush_sample_time, $time))
    end

    if (driving_valid) begin
        complete_driving_response();
        has_progress = 1'b1;
    end

    drain_csr_runtime_events();

    request_killed = 1'b0;
    if (new_flush_event) begin
        void'(handle_l2tlb_flush_event(flush_event_seq,
                                       flush_sample_time,
                                       request_killed));
        has_progress = 1'b1;
    end

    if (request_fire() && !request_killed) begin
        if (!csr_snapshot_valid) begin
            `uvm_fatal(get_type_name(), "L2TLB request fired before first runtime CSR snapshot")
        end
        void'(capture_fired_request());
        has_progress = 1'b1;
    end

    if (data.is_global_stop_requested()) begin
        stopping = 1'b1;
    end

    hold_active = sample_seq < accept_hold_until_sample;
    lifecycle_blocked = !csr_snapshot_valid || hold_active;
    if (has_progress || lifecycle_blocked || stopping ||
        outstanding_count() != 0 || !acceptance_opened_since_reset ||
        !ready_opportunity_since_lifecycle_block) begin
        idle_count = 0;
    end else begin
        idle_count++;
        if (idle_count >= idle_stop_cycle) begin
            stopping = 1'b1;
            `uvm_info(get_type_name(),
                      $sformatf("L2TLB responder idle-stop at sample=%0d idle_count=%0d",
                                sample_seq, idle_count),
                      UVM_LOW)
        end
    end

    cycle_tr = null;
    response_selected = 1'b0;
    if (csr_snapshot_valid && !hold_active) begin
        response_selected = select_due_response(sample_seq + 1, cycle_tr);
        if (response_selected) begin
            has_progress = 1'b1;
        end
    end
    if (cycle_tr == null) begin
        cycle_tr = create_l2tlb_xaction($sformatf("l2tlb_cycle_%0d", sample_seq));
    end

    next_ready = !stopping && csr_snapshot_valid && !hold_active &&
                 outstanding_count() < max_outstanding;
    cycle_tr.io_ptw_req_0_ready = next_ready;
    cycle_tr.pre_pkt_gap = 0;
    cycle_tr.post_pkt_gap = 0;
    if (next_ready) begin
        acceptance_opened_since_reset = 1'b1;
    end
    if (hold_active && cycle_tr.io_ptw_resp_valid) begin
        `uvm_fatal(get_type_name(), "flush hold attempted to drive an L2TLB response")
    end

    send_l2tlb_item(cycle_tr);
    if (next_ready) begin
        // 中文注释：finish_item返回后，ready机会才算真正交给driver/DUT。
        ready_opportunity_since_lifecycle_block = 1'b1;
    end
    if (stopping && outstanding_count() == 0) begin
        if (cycle_tr.io_ptw_req_0_ready || cycle_tr.io_ptw_resp_valid) begin
            `uvm_fatal(get_type_name(), "L2TLB stop exit requires a final inactive cycle item")
        end
        should_exit = 1'b1;
    end
endtask:send_l2tlb_cycle

task memblock_l2tlb_base_sequence::send_l2tlb_item(input L2tlb_agent_agent_xaction tr);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "send_l2tlb_item got null xaction")
    end
    if (tr.pre_pkt_gap != 0 || tr.post_pkt_gap != 0) begin
        `uvm_fatal(get_type_name(), "L2TLB cycle item must use pre_pkt_gap=0 and post_pkt_gap=0")
    end
    start_item(tr);
    finish_item(tr);
endtask:send_l2tlb_item

function void memblock_l2tlb_base_sequence::configure_from_plus();
    enable = seq_csr_common::get_l2tlb_seq_en();
    max_outstanding = seq_csr_common::get_l2tlb_max_outstanding();
    resp_reorder_en = seq_csr_common::get_l2tlb_resp_reorder_en();
    resp_mid_latency = seq_csr_common::get_l2tlb_resp_mid_latency();
    resp_long_latency = seq_csr_common::get_l2tlb_resp_long_latency();
    resp_1c_wt = seq_csr_common::get_l2tlb_resp_1c_wt();
    resp_mid_wt = seq_csr_common::get_l2tlb_resp_mid_wt();
    resp_long_wt = seq_csr_common::get_l2tlb_resp_long_wt();
    idle_stop_cycle = seq_csr_common::get_l2tlb_idle_stop_cycle();
endfunction:configure_from_plus

function void memblock_l2tlb_base_sequence::ensure_context();
    data = common_data_transaction::get();
    if (data == null) begin
        `uvm_fatal(get_type_name(), "failed to get common_data_transaction")
    end
    if (!uvm_config_db#(virtual L2tlb_agent_agent_interface)::get(null, get_full_name(), "vif", l2tlb_vif) &&
        !uvm_config_db#(virtual L2tlb_agent_agent_interface)::get(null, "uvm_test_top.env.u_L2tlb_agent_agent*", "vif", l2tlb_vif)) begin
        `uvm_fatal(get_type_name(), "L2TLB virtual interface is not set")
    end
endfunction:ensure_context

function void memblock_l2tlb_base_sequence::initialize_lifecycle_state();
    pending_q.delete();
    driving_req = null;
    driving_valid = 1'b0;
    accepted_count = 0;
    completed_count = 0;
    flush_canceled_count = 0;
    reset_canceled_count = 0;
    next_request_token = 0;
    sample_seq = 0;
    last_seen_flush_event_seq = 0;
    accept_hold_until_sample = 0;
    acceptance_opened_since_reset = 1'b0;
    ready_opportunity_since_lifecycle_block = 1'b0;
    csr_snapshot_valid = 1'b0;
    require_post_reset_csr_refresh = 1'b0;
    reset_runtime_csr_seq_baseline = 0;
    stopping = 1'b0;
    idle_count = 0;
    sampled_req_valid = 1'b0;
    sampled_req_ready = 1'b0;
    sampled_req_vpn = '0;
    sampled_req_s2xlate = '0;
endfunction:initialize_lifecycle_state

function void memblock_l2tlb_base_sequence::drain_csr_runtime_events();
    memblock_sync_pkg::dispatch_raw_csr_t raw_csr;
    int unsigned raw_csr_seq;

    if (!memblock_sync_pkg::get_latest_runtime_csr_snapshot(raw_csr, raw_csr_seq)) begin
        return;
    end
    if (require_post_reset_csr_refresh &&
        raw_csr_seq <= reset_runtime_csr_seq_baseline) begin
        return;
    end
    data.apply_raw_csr_runtime(raw_csr, raw_csr_seq);
    csr_snapshot_valid = 1'b1;
    require_post_reset_csr_refresh = 1'b0;
endfunction:drain_csr_runtime_events

function bit memblock_l2tlb_base_sequence::request_fire();
    return sampled_req_valid && sampled_req_ready;
endfunction:request_fire

function int unsigned memblock_l2tlb_base_sequence::outstanding_count();
    return pending_q.size() + (driving_valid ? 1 : 0);
endfunction:outstanding_count

function void memblock_l2tlb_base_sequence::check_l2tlb_lifecycle_accounting(input string audit_context);
    longint unsigned accounted_count;

    accounted_count = completed_count + flush_canceled_count +
                      reset_canceled_count + outstanding_count();
    if (accepted_count != accounted_count) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("L2TLB lifecycle mismatch context=%s accepted=%0d completed=%0d flush_canceled=%0d reset_canceled=%0d pending=%0d driving=%0d accounted=%0d",
                             audit_context, accepted_count, completed_count,
                             flush_canceled_count, reset_canceled_count,
                             pending_q.size(), driving_valid, accounted_count))
    end
endfunction:check_l2tlb_lifecycle_accounting

function void memblock_l2tlb_base_sequence::cancel_outstanding_by_reset();
    int unsigned canceled_count;

    canceled_count = outstanding_count();
    reset_canceled_count += canceled_count;
    pending_q.delete();
    driving_req = null;
    driving_valid = 1'b0;
    if (canceled_count != 0) begin
        `uvm_info(get_type_name(),
                  $sformatf("reset canceled %0d L2TLB tokens at sample=%0d",
                            canceled_count, sample_seq),
                  UVM_LOW)
    end
    check_l2tlb_lifecycle_accounting("reset_cancel");
endfunction:cancel_outstanding_by_reset

function memblock_l2tlb_pending_req memblock_l2tlb_base_sequence::capture_fired_request();
    memblock_l2tlb_pending_req pending;
    memblock_tlb_entry live_entry;
    memblock_tlb_lookup_key_t returned_key;
    bit created;

    if (outstanding_count() >= max_outstanding) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("L2TLB request fired at full queue: outstanding=%0d max=%0d",
                             outstanding_count(), max_outstanding))
    end
    pending = memblock_l2tlb_pending_req::type_id::create(
        $sformatf("l2tlb_pending_%0d", next_request_token));
    if (pending == null) begin
        `uvm_fatal(get_type_name(), "failed to create L2TLB pending record")
    end
    pending.request_token = next_request_token;
    next_request_token++;
    pending.vpn = sampled_req_vpn;
    pending.s2xlate = sampled_req_s2xlate;
    data.get_mmu_csr_snapshot(pending.csr_snapshot);
    if (pending.csr_snapshot == null) begin
        `uvm_fatal(get_type_name(), "failed to capture request-time CSR snapshot")
    end
    pending.lookup_key = pending.csr_snapshot.make_lookup_key({26'b0, pending.vpn},
                                                               pending.s2xlate);
    if (!data.get_or_create_tlb_entry_by_req_with_snapshot(pending.vpn,
                                                            pending.s2xlate,
                                                            pending.csr_snapshot,
                                                            returned_key,
                                                            live_entry,
                                                            created) ||
        live_entry == null) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("failed to get/create L2TLB entry vpn=0x%0h s2xlate=%0d",
                             pending.vpn, pending.s2xlate))
    end
    if (returned_key != pending.lookup_key) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("request-time L2TLB key drift token=%0d snapshot_vpn=0x%0h returned_vpn=0x%0h",
                             pending.request_token,
                             pending.lookup_key.vpn,
                             returned_key.vpn))
    end
    pending.entry_snapshot = memblock_tlb_entry::type_id::create(
        $sformatf("l2tlb_entry_snapshot_%0d", pending.request_token));
    if (pending.entry_snapshot == null) begin
        `uvm_fatal(get_type_name(), "failed to create request-time L2TLB entry snapshot")
    end
    pending.entry_snapshot.copy_from(live_entry);
    pending.resp_tr = create_l2tlb_xaction(
        $sformatf("l2tlb_resp_token_%0d", pending.request_token));
    pending.resp_tr.io_ptw_req_0_valid = 1'b1;
    pending.resp_tr.io_ptw_req_0_bits_vpn = pending.vpn;
    pending.resp_tr.io_ptw_req_0_bits_s2xlate = pending.s2xlate;
    fill_dtlb_resp_from_entry(pending.entry_snapshot, pending.resp_tr);
    pending.min_latency = choose_latency(pending.latency_bucket);
    pending.accept_sample_seq = sample_seq;
    pending.due_sample_seq = sample_seq + pending.min_latency;
    pending.accept_flush_event_seq = last_seen_flush_event_seq;
    pending_q.push_back(pending);
    accepted_count++;
    `uvm_info(get_type_name(),
              $sformatf("accept L2TLB token=%0d vpn=0x%0h s2xlate=%0d created=%0d due=%0d bucket=%0d outstanding=%0d",
                        pending.request_token, pending.vpn, pending.s2xlate,
                        created, pending.due_sample_seq,
                        pending.latency_bucket, outstanding_count()),
              UVM_LOW)
    check_l2tlb_lifecycle_accounting("request_accept");
    return pending;
endfunction:capture_fired_request

function void memblock_l2tlb_base_sequence::record_flush_killed_request(
    input longint unsigned event_seq,
    input time event_sample_time);
    longint unsigned token;

    token = next_request_token;
    next_request_token++;
    accepted_count++;
    flush_canceled_count++;
    `uvm_info(get_type_name(),
              $sformatf("flush-event-window canceled L2TLB token=%0d vpn=0x%0h s2xlate=%0d sample=%0d event_seq=%0d event_time=%0t",
                        token, sampled_req_vpn, sampled_req_s2xlate,
                        sample_seq, event_seq, event_sample_time),
              UVM_LOW)
    check_l2tlb_lifecycle_accounting("flush_window_cancel");
endfunction:record_flush_killed_request

function int unsigned memblock_l2tlb_base_sequence::handle_l2tlb_flush_event(
    input longint unsigned event_seq,
    input time event_sample_time,
    output bit request_killed);
    int unsigned drop_count;

    request_killed = 1'b0;
    drop_count = 0;
    for (int idx = int'(pending_q.size()) - 1; idx >= 0; idx--) begin
        if (pending_q[idx].accept_flush_event_seq < event_seq) begin
            pending_q.delete(idx);
            drop_count++;
        end
    end
    flush_canceled_count += drop_count;
    last_seen_flush_event_seq = event_seq;
    accept_hold_until_sample = sample_seq + MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES;
    ready_opportunity_since_lifecycle_block = 1'b0;
    if (acceptance_opened_since_reset && event_sample_time == $time && request_fire()) begin
        record_flush_killed_request(event_seq, event_sample_time);
        request_killed = 1'b1;
    end
    `uvm_info(get_type_name(),
              $sformatf("apply L2TLB flush event_seq=%0d event_time=%0t sample=%0d dropped=%0d killed_current=%0d hold_until=%0d",
                        event_seq, event_sample_time, sample_seq,
                        drop_count, request_killed, accept_hold_until_sample),
              UVM_LOW)
    check_l2tlb_lifecycle_accounting("flush_event");
    return drop_count;
endfunction:handle_l2tlb_flush_event

function bit memblock_l2tlb_base_sequence::select_due_response(
    input longint unsigned next_sample_seq,
    output L2tlb_agent_agent_xaction cycle_tr);
    int unsigned selected_index;
    int unsigned eligible_indices[$];
    int unsigned eligible_count;
    int unsigned choice;

    cycle_tr = null;
    if (pending_q.size() == 0) begin
        return 1'b0;
    end
    if (stopping || !resp_reorder_en) begin
        if (pending_q[0].due_sample_seq > next_sample_seq) begin
            return 1'b0;
        end
        selected_index = 0;
    end else begin
        foreach (pending_q[idx]) begin
            if (pending_q[idx].due_sample_seq <= next_sample_seq) begin
                eligible_indices.push_back(idx);
            end
        end
        if (eligible_indices.size() == 0) begin
            return 1'b0;
        end
        eligible_count = eligible_indices.size();
        if (!std::randomize(choice) with {
                choice < eligible_count;
            }) begin
            `uvm_fatal(get_type_name(), "failed to randomize eligible L2TLB response index")
        end
        selected_index = eligible_indices[choice];
    end
    if (pending_q[selected_index].accept_flush_event_seq != last_seen_flush_event_seq) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("selected stale L2TLB token=%0d accept_event=%0d current_event=%0d",
                             pending_q[selected_index].request_token,
                             pending_q[selected_index].accept_flush_event_seq,
                             last_seen_flush_event_seq))
    end
    driving_req = pending_q[selected_index];
    pending_q.delete(selected_index);
    driving_valid = 1'b1;
    cycle_tr = driving_req.resp_tr;
    if (cycle_tr == null) begin
        `uvm_fatal(get_type_name(), "selected L2TLB pending record has null response transaction")
    end
    check_l2tlb_lifecycle_accounting("response_select");
    return 1'b1;
endfunction:select_due_response

function void memblock_l2tlb_base_sequence::complete_driving_response();
    int unsigned record_update_count;
    longint unsigned complete_sample_seq;

    if (!driving_valid || driving_req == null) begin
        `uvm_fatal(get_type_name(), "complete_driving_response got invalid driving slot")
    end
    complete_sample_seq = sample_seq;
    if (complete_sample_seq < driving_req.due_sample_seq) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("L2TLB response completed before due token=%0d accept=%0d due=%0d complete=%0d",
                             driving_req.request_token,
                             driving_req.accept_sample_seq,
                             driving_req.due_sample_seq,
                             complete_sample_seq))
    end
    record_update_count = data.update_uid_tlb_records_by_entry(
        driving_req.lookup_key,
        driving_req.entry_snapshot);
    `uvm_info(get_type_name(),
              $sformatf("complete L2TLB token=%0d bucket=%0d min_latency=%0d accept=%0d due=%0d complete=%0d extra_wait=%0d uid_records=%0d pending=%0d",
                        driving_req.request_token,
                        driving_req.latency_bucket,
                        driving_req.min_latency,
                        driving_req.accept_sample_seq,
                        driving_req.due_sample_seq,
                        complete_sample_seq,
                        complete_sample_seq - driving_req.due_sample_seq,
                        record_update_count,
                        pending_q.size()),
              UVM_LOW)
    driving_req = null;
    driving_valid = 1'b0;
    completed_count++;
    check_l2tlb_lifecycle_accounting("response_complete");
endfunction:complete_driving_response

function L2tlb_agent_agent_xaction memblock_l2tlb_base_sequence::create_l2tlb_xaction(input string name);
    L2tlb_agent_agent_xaction tr;

    tr = L2tlb_agent_agent_xaction::type_id::create(name);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "failed to create L2TLB xaction")
    end
    clear_l2tlb_xaction(tr);
    return tr;
endfunction:create_l2tlb_xaction

function void memblock_l2tlb_base_sequence::clear_l2tlb_xaction(input L2tlb_agent_agent_xaction tr);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "clear_l2tlb_xaction got null xaction")
    end
    tr.io_ptw_req_0_ready = 1'b0;
    tr.io_ptw_req_0_valid = 1'b0;
    tr.io_ptw_req_0_bits_vpn = '0;
    tr.io_ptw_req_0_bits_s2xlate = '0;
    tr.io_ptw_resp_valid = 1'b0;
    tr.io_ptw_resp_bits_s2xlate = '0;
    tr.io_ptw_resp_bits_s1_entry_tag = '0;
    tr.io_ptw_resp_bits_s1_entry_asid = '0;
    tr.io_ptw_resp_bits_s1_entry_vmid = '0;
    tr.io_ptw_resp_bits_s1_entry_n = '0;
    tr.io_ptw_resp_bits_s1_entry_pbmt = '0;
    tr.io_ptw_resp_bits_s1_entry_perm_d = '0;
    tr.io_ptw_resp_bits_s1_entry_perm_a = '0;
    tr.io_ptw_resp_bits_s1_entry_perm_g = '0;
    tr.io_ptw_resp_bits_s1_entry_perm_u = '0;
    tr.io_ptw_resp_bits_s1_entry_perm_x = '0;
    tr.io_ptw_resp_bits_s1_entry_perm_w = '0;
    tr.io_ptw_resp_bits_s1_entry_perm_r = '0;
    tr.io_ptw_resp_bits_s1_entry_level = '0;
    tr.io_ptw_resp_bits_s1_entry_v = '0;
    tr.io_ptw_resp_bits_s1_entry_ppn = '0;
    tr.io_ptw_resp_bits_s1_addr_low = '0;
    tr.io_ptw_resp_bits_s1_ppn_low_0 = '0;
    tr.io_ptw_resp_bits_s1_ppn_low_1 = '0;
    tr.io_ptw_resp_bits_s1_ppn_low_2 = '0;
    tr.io_ptw_resp_bits_s1_ppn_low_3 = '0;
    tr.io_ptw_resp_bits_s1_ppn_low_4 = '0;
    tr.io_ptw_resp_bits_s1_ppn_low_5 = '0;
    tr.io_ptw_resp_bits_s1_ppn_low_6 = '0;
    tr.io_ptw_resp_bits_s1_ppn_low_7 = '0;
    tr.io_ptw_resp_bits_s1_valididx_0 = '0;
    tr.io_ptw_resp_bits_s1_valididx_1 = '0;
    tr.io_ptw_resp_bits_s1_valididx_2 = '0;
    tr.io_ptw_resp_bits_s1_valididx_3 = '0;
    tr.io_ptw_resp_bits_s1_valididx_4 = '0;
    tr.io_ptw_resp_bits_s1_valididx_5 = '0;
    tr.io_ptw_resp_bits_s1_valididx_6 = '0;
    tr.io_ptw_resp_bits_s1_valididx_7 = '0;
    tr.io_ptw_resp_bits_s1_pteidx_0 = '0;
    tr.io_ptw_resp_bits_s1_pteidx_1 = '0;
    tr.io_ptw_resp_bits_s1_pteidx_2 = '0;
    tr.io_ptw_resp_bits_s1_pteidx_3 = '0;
    tr.io_ptw_resp_bits_s1_pteidx_4 = '0;
    tr.io_ptw_resp_bits_s1_pteidx_5 = '0;
    tr.io_ptw_resp_bits_s1_pteidx_6 = '0;
    tr.io_ptw_resp_bits_s1_pteidx_7 = '0;
    tr.io_ptw_resp_bits_s1_pf = '0;
    tr.io_ptw_resp_bits_s1_af = '0;
    tr.io_ptw_resp_bits_s2_entry_tag = '0;
    tr.io_ptw_resp_bits_s2_entry_vmid = '0;
    tr.io_ptw_resp_bits_s2_entry_n = '0;
    tr.io_ptw_resp_bits_s2_entry_pbmt = '0;
    tr.io_ptw_resp_bits_s2_entry_ppn = '0;
    tr.io_ptw_resp_bits_s2_entry_perm_d = '0;
    tr.io_ptw_resp_bits_s2_entry_perm_a = '0;
    tr.io_ptw_resp_bits_s2_entry_perm_g = '0;
    tr.io_ptw_resp_bits_s2_entry_perm_u = '0;
    tr.io_ptw_resp_bits_s2_entry_perm_x = '0;
    tr.io_ptw_resp_bits_s2_entry_perm_w = '0;
    tr.io_ptw_resp_bits_s2_entry_perm_r = '0;
    tr.io_ptw_resp_bits_s2_entry_level = '0;
    tr.io_ptw_resp_bits_s2_gpf = '0;
    tr.io_ptw_resp_bits_s2_gaf = '0;
    tr.pre_pkt_gap = 0;
    tr.post_pkt_gap = 0;
endfunction:clear_l2tlb_xaction

function void memblock_l2tlb_base_sequence::fill_dtlb_resp_from_entry(input memblock_tlb_entry entry,
                                                                      ref L2tlb_agent_agent_xaction resp);
    if (resp == null || entry == null) begin
        `uvm_fatal(get_type_name(), "fill_dtlb_resp_from_entry got null input")
    end

    resp.io_ptw_resp_valid = 1'b1;
    resp.io_ptw_resp_bits_s2xlate = entry.s2xlate;
    resp.io_ptw_resp_bits_s1_entry_tag = entry.lookup_key.vpn[34:0];
    resp.io_ptw_resp_bits_s1_entry_asid = entry.asid[15:0];
    resp.io_ptw_resp_bits_s1_entry_vmid = entry.vmid[13:0];
    resp.io_ptw_resp_bits_s1_entry_n = entry.pte_n;
    resp.io_ptw_resp_bits_s1_entry_pbmt = entry.pbmt;
    resp.io_ptw_resp_bits_s1_entry_perm_d = entry.pte_d;
    resp.io_ptw_resp_bits_s1_entry_perm_a = entry.pte_a;
    resp.io_ptw_resp_bits_s1_entry_perm_g = entry.pte_g;
    resp.io_ptw_resp_bits_s1_entry_perm_u = entry.pte_u;
    resp.io_ptw_resp_bits_s1_entry_perm_x = entry.pte_x;
    resp.io_ptw_resp_bits_s1_entry_perm_w = entry.pte_w;
    resp.io_ptw_resp_bits_s1_entry_perm_r = entry.pte_r;
    resp.io_ptw_resp_bits_s1_entry_level = entry.level;
    resp.io_ptw_resp_bits_s1_entry_v = entry.pte_v;
    resp.io_ptw_resp_bits_s1_entry_ppn = entry.ppn[40:0];
    resp.io_ptw_resp_bits_s1_addr_low = entry.addr_low;
    resp.io_ptw_resp_bits_s1_ppn_low_0 = entry.ppn_low[0];
    resp.io_ptw_resp_bits_s1_ppn_low_1 = entry.ppn_low[1];
    resp.io_ptw_resp_bits_s1_ppn_low_2 = entry.ppn_low[2];
    resp.io_ptw_resp_bits_s1_ppn_low_3 = entry.ppn_low[3];
    resp.io_ptw_resp_bits_s1_ppn_low_4 = entry.ppn_low[4];
    resp.io_ptw_resp_bits_s1_ppn_low_5 = entry.ppn_low[5];
    resp.io_ptw_resp_bits_s1_ppn_low_6 = entry.ppn_low[6];
    resp.io_ptw_resp_bits_s1_ppn_low_7 = entry.ppn_low[7];
    resp.io_ptw_resp_bits_s1_valididx_0 = entry.valididx[0];
    resp.io_ptw_resp_bits_s1_valididx_1 = entry.valididx[1];
    resp.io_ptw_resp_bits_s1_valididx_2 = entry.valididx[2];
    resp.io_ptw_resp_bits_s1_valididx_3 = entry.valididx[3];
    resp.io_ptw_resp_bits_s1_valididx_4 = entry.valididx[4];
    resp.io_ptw_resp_bits_s1_valididx_5 = entry.valididx[5];
    resp.io_ptw_resp_bits_s1_valididx_6 = entry.valididx[6];
    resp.io_ptw_resp_bits_s1_valididx_7 = entry.valididx[7];
    resp.io_ptw_resp_bits_s1_pteidx_0 = (entry.pteidx[0] != 0);
    resp.io_ptw_resp_bits_s1_pteidx_1 = (entry.pteidx[1] != 0);
    resp.io_ptw_resp_bits_s1_pteidx_2 = (entry.pteidx[2] != 0);
    resp.io_ptw_resp_bits_s1_pteidx_3 = (entry.pteidx[3] != 0);
    resp.io_ptw_resp_bits_s1_pteidx_4 = (entry.pteidx[4] != 0);
    resp.io_ptw_resp_bits_s1_pteidx_5 = (entry.pteidx[5] != 0);
    resp.io_ptw_resp_bits_s1_pteidx_6 = (entry.pteidx[6] != 0);
    resp.io_ptw_resp_bits_s1_pteidx_7 = (entry.pteidx[7] != 0);
    resp.io_ptw_resp_bits_s1_pf = entry.tlbPF;
    resp.io_ptw_resp_bits_s1_af = entry.tlbAF || entry.pmaAF;
    resp.io_ptw_resp_bits_s2_entry_tag = entry.lookup_key.vpn[37:0];
    resp.io_ptw_resp_bits_s2_entry_vmid = entry.vmid[13:0];
    resp.io_ptw_resp_bits_s2_entry_n = entry.pte_n;
    resp.io_ptw_resp_bits_s2_entry_pbmt = entry.pbmt;
    resp.io_ptw_resp_bits_s2_entry_ppn = entry.ppn[37:0];
    resp.io_ptw_resp_bits_s2_entry_perm_d = entry.pte_d;
    resp.io_ptw_resp_bits_s2_entry_perm_a = entry.pte_a;
    // permission g/u继续直接来自request-time entry snapshot的pte_g/pte_u。
    resp.io_ptw_resp_bits_s2_entry_perm_g = entry.pte_g;
    resp.io_ptw_resp_bits_s2_entry_perm_u = entry.pte_u;
    resp.io_ptw_resp_bits_s2_entry_perm_x = entry.pte_x;
    resp.io_ptw_resp_bits_s2_entry_perm_w = entry.pte_w;
    resp.io_ptw_resp_bits_s2_entry_perm_r = entry.pte_r;
    resp.io_ptw_resp_bits_s2_entry_level = entry.level;
    resp.io_ptw_resp_bits_s2_gpf = entry.tlbGPF;
    resp.io_ptw_resp_bits_s2_gaf = 1'b0;
endfunction:fill_dtlb_resp_from_entry

function int unsigned memblock_l2tlb_base_sequence::choose_latency(
    output memblock_l2tlb_latency_bucket_e bucket);
    if (!std::randomize(bucket) with {
            bucket dist {
                L2TLB_LATENCY_1C   := resp_1c_wt,
                L2TLB_LATENCY_MID  := resp_mid_wt,
                L2TLB_LATENCY_LONG := resp_long_wt
            };
        }) begin
        `uvm_fatal(get_type_name(), "failed to randomize L2TLB response latency bucket")
    end
    case (bucket)
        L2TLB_LATENCY_1C:   return 1;
        L2TLB_LATENCY_MID:  return resp_mid_latency;
        L2TLB_LATENCY_LONG: return resp_long_latency;
        default: begin
            `uvm_fatal(get_type_name(), "randomized invalid L2TLB latency bucket")
            return 1;
        end
    endcase
endfunction:choose_latency

`endif
