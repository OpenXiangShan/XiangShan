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
    // 中文注释：request fire时冻结的CSR、request key、canonical anchor key、
    // TLB entry和response payload。range hit 时 request/anchor key 可不同，
    // 但 driver 仍只消费冻结的 raw entry snapshot。
    // response等待期间live CSR/table变化不会回写这些快照。
    mmu_csr_runtime_state csr_snapshot;
    memblock_tlb_lookup_key_t request_lookup_key;
    memblock_tlb_lookup_key_t entry_anchor_key;
    memblock_tlb_lookup_result_e lookup_result;
    memblock_tlb_entry entry_snapshot;
    // 中文注释：该 generation 与 snapshot 同源，标识本 token 命中的 live entry 时代。
    // response 完成、flush 或 driver 重试都不能从 live table 重新读取或改写它。
    longint unsigned pending_entry_generation;
    L2tlb_agent_agent_xaction resp_tr;
    // 中文注释：accept/due序号定义最早response边界；complete允许因端口竞争晚于due。
    longint unsigned accept_sample_seq;
    memblock_l2tlb_latency_bucket_e latency_bucket;
    int unsigned min_latency;
    longint unsigned due_sample_seq;
    // 中文注释：接受request时已观察到的flush event版本；新event只取消更旧版本的pending。
    longint unsigned accept_flush_event_seq;
    // Request-specific normal-translation debug state.  It is derived from
    // this token's frozen snapshot, not from a later live-table lookup.
    bit request_derived_valid;
    bit [43:0] request_s1_resolved_ppn;
    bit [43:0] request_s2_resolved_ppn;
    bit [51:0] request_gvpn;

    `uvm_object_utils(memblock_l2tlb_pending_req)

    function new(string name = "memblock_l2tlb_pending_req");
        super.new(name);
        request_token = 0;
        vpn = '0;
        s2xlate = '0;
        csr_snapshot = null;
        request_lookup_key = '{default:'0};
        entry_anchor_key = '{default:'0};
        lookup_result = MEMBLOCK_TLB_LOOKUP_MISS_BUILD;
        entry_snapshot = null;
        pending_entry_generation = 0;
        resp_tr = null;
        accept_sample_seq = 0;
        latency_bucket = L2TLB_LATENCY_1C;
        min_latency = 1;
        due_sample_seq = 0;
        accept_flush_event_seq = 0;
        request_derived_valid = 1'b0;
        request_s1_resolved_ppn = '0;
        request_s2_resolved_ppn = '0;
        request_gvpn = '0;
    endfunction:new
endclass:memblock_l2tlb_pending_req

class memblock_l2tlb_base_sequence extends L2tlb_agent_agent_default_sequence;

    common_data_transaction data;
    L2tlb_agent_agent_sequencer l2tlb_sqr;
    // The reset epoch is copied from the frozen transport sample currently
    // being consumed.  Items created for that sample inherit this value;
    // sequence code never needs to read the live VIF.
    longint unsigned current_sample_reset_epoch;

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
    // C0 only records a barrier.  Pending requests remain serviceable until
    // the corresponding V2 filter due sample (C4 by default).
    memblock_sync_pkg::memblock_l2tlb_event_record_t barrier_q[$];
    bit due_barrier_this_sample;
    longint unsigned fire_visible_event_seq;

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
    // 启动期历史 event 的保守等待，不代表运行期 C0/C4 barrier。
    longint unsigned pre_ready_hold_until_sample;
    bit acceptance_opened_since_reset;
    // 首次向 DUT 开放 ready 前，启动 transport/semantic 空状态已验证。
    bit owner_start_baseline_done;
    // 中文注释：reset或flush hold解除后，必须至少发出一拍可接受ready，
    // 才允许no-progress诊断重新计数；该标志与reset freshness独立维护。
    bit ready_opportunity_since_lifecycle_block;
    bit csr_snapshot_valid;
    // 中文注释：DUT reset后必须等待CSR monitor发布一个更新的runtime snapshot sequence。
    // package latest本身不随semantic raw clear而删除，不能把reset前快照用于重新开放ready。
    bit require_post_reset_csr_refresh;
    int unsigned reset_runtime_csr_seq_baseline;
    bit stopping;
    int unsigned idle_count;
    string lifecycle_owner_name;
    bit release_close_requested;
    longint unsigned release_generation;
    bit final_item_sent;
    longint unsigned baseline_sent_sample_seq;
    // 中文注释：进入每个service tick时立即锁存的真实request握手字段。
    // 后续NBA等待和queue处理只读该快照，不重新读取live VIF。
    logic sampled_req_valid;
    logic sampled_req_ready;
    logic [37:0] sampled_req_vpn;
    logic [1:0] sampled_req_s2xlate;
    logic sampled_req_fire;
    logic sampled_resp_valid;
    int unsigned consecutive_not_ready_samples;
    longint unsigned last_not_ready_sample_seq;

    `uvm_object_utils(memblock_l2tlb_base_sequence)

    extern function new(string name = "memblock_l2tlb_base_sequence");
    extern virtual task pre_body();
    extern virtual function void do_kill();
    extern virtual task body();
    extern virtual task drive_l2tlb_loop();
    extern virtual task send_l2tlb_cycle(
        input memblock_l2tlb_drv_sample_t sample,
        output bit has_progress,
        output bit should_exit,
        output memblock_sync_pkg::memblock_l2tlb_transport_terminal_e terminal_kind);
    extern virtual task send_l2tlb_item(input L2tlb_agent_agent_xaction tr);
    extern virtual task wait_for_l2tlb_transport_sample(
        output L2tlb_agent_agent_transport_sample sample_ref,
        output memblock_l2tlb_drv_sample_t sample);
    extern virtual function void ack_l2tlb_transport_sample(
        input longint unsigned transport_sample_seq,
        input memblock_sync_pkg::memblock_l2tlb_transport_terminal_e terminal_kind);
    // Abstract responsibility: consume a frozen sample that became stale
    // before the semantic owner reached it, release its slot, and drive one
    // inactive item so the driver can continue servicing the clock boundary.
    extern virtual task drop_stale_l2tlb_transport_sample(
        input memblock_l2tlb_drv_sample_t sample);
    // Abstract responsibility: bound consecutive samples for which the global
    // anchor or producer watermarks are not yet available.
    extern virtual function void note_l2tlb_sample_not_ready(
        input memblock_l2tlb_drv_sample_t sample,
        input string reason);
    extern function void configure_from_plus();
    extern function void ensure_context();
    extern function void initialize_lifecycle_state();
    extern function void drain_csr_runtime_events();
    extern function bit request_fire();
    extern function int unsigned outstanding_count();
    extern function void check_l2tlb_lifecycle_accounting(input string audit_context);
    extern function void cancel_outstanding_by_reset();
    extern function memblock_l2tlb_pending_req capture_fired_request();
    extern function int unsigned handle_l2tlb_flush_event(
        input memblock_sync_pkg::memblock_l2tlb_event_record_t event_record);
    extern function int unsigned apply_due_l2tlb_flush_barriers(
        input longint unsigned current_sample_seq);
    extern function bit get_request_csr_snapshot(output mmu_csr_runtime_state snapshot);
    extern function bit select_due_response(input longint unsigned next_sample_seq,
                                            output L2tlb_agent_agent_xaction cycle_tr);
    extern function void complete_driving_response();
    extern function L2tlb_agent_agent_xaction create_l2tlb_xaction(input string name);
    extern function void clear_l2tlb_xaction(input L2tlb_agent_agent_xaction tr);
    extern function void fill_dtlb_resp_from_entry(input memblock_tlb_entry entry,
                                                   ref L2tlb_agent_agent_xaction resp);
    extern function void stamp_lifecycle_item(
        input L2tlb_agent_agent_xaction tr,
        input memblock_sync_pkg::memblock_l2tlb_release_item_kind_e item_kind,
        input longint unsigned generation,
        input bit is_post_reset_baseline);
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
    // UVM kill is not a lifecycle release path.  Releasing here would leave
    // an unobserved final transport sample and let a later sequence take over
    // an owner with live token state.
    if (lifecycle_owner_name != "" &&
        memblock_sync_pkg::l2tlb_lifecycle_owner_claimed) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("L2TLB sequence killed while owner is claimed: %s",
                             lifecycle_owner_name))
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
    if (!memblock_sync_pkg::l2tlb_responder_enabled() ||
        !memblock_sync_pkg::l2tlb_dispatch_active()) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("L2TLB responder started outside an enabled dispatch topology: initialized=%0d responder=%0d dispatch=%0d topology=%s",
                             memblock_sync_pkg::l2tlb_testcase_lifecycle_initialized,
                             memblock_sync_pkg::l2tlb_responder_enabled(),
                             memblock_sync_pkg::l2tlb_dispatch_active(),
                             memblock_sync_pkg::l2tlb_testcase_topology_name))
    end

    if (!$cast(l2tlb_sqr, m_sequencer) || l2tlb_sqr == null) begin
        `uvm_fatal(get_type_name(), "L2TLB sequence must run on L2TLB agent sequencer")
    end

    // Owner claim is a post-reset lifecycle operation.  Waiting here avoids
    // turning a sequence that starts during reset into a false owner-claim
    // failure; the reset coordinator remains responsible for convergence.
    wait (memblock_sync_pkg::reset_backend_done === 1'b1);
    wait (memblock_sync_pkg::l2tlb_runtime_reset_active === 1'b0);

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
    if (memblock_sync_pkg::l2tlb_lifecycle_owner_claimed) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("L2TLB sequence exited with owner still claimed: %s",
                             lifecycle_owner_name))
    end
endtask:body

task memblock_l2tlb_base_sequence::drive_l2tlb_loop();
    string current_owner;

    forever begin
        L2tlb_agent_agent_transport_sample sample_ref;
        memblock_l2tlb_drv_sample_t sample;
        bit has_progress;
        bit should_exit;
        memblock_sync_pkg::memblock_l2tlb_transport_terminal_e terminal_kind;

        wait_for_l2tlb_transport_sample(sample_ref, sample);
        send_l2tlb_cycle(sample, has_progress, should_exit, terminal_kind);

        if (final_item_sent && sample.sampled_final_inactive_proof_valid) begin
            bit granted;
            longint unsigned expected_reset_epoch;

            expected_reset_epoch = sample.sampled_reset_epoch;
            memblock_sync_pkg::wait_for_l2tlb_release_grant_or_reset(
                lifecycle_owner_name,
                expected_reset_epoch,
                release_generation,
                granted);
            if (!granted) begin
                // The next reset sample will cancel/re-arm the owner state.
                final_item_sent = 1'b0;
                release_close_requested = 1'b0;
                continue;
            end
            if (!memblock_sync_pkg::try_release_l2tlb_lifecycle_owner(
                    lifecycle_owner_name, current_owner)) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("L2TLB final release failed owner=%s current=%s",
                                     lifecycle_owner_name, current_owner))
            end
            break;
        end
        if (should_exit) begin
            break;
        end
    end
endtask:drive_l2tlb_loop

// 中文注释：每个drv_cb边界推进一次完整L2TLB lifecycle service。
// 固定顺序为锁存fire、NBA后校验flush、确认response、同步CSR、处理flush/fire、调度下一cycle item。
task memblock_l2tlb_base_sequence::send_l2tlb_cycle(
    input memblock_l2tlb_drv_sample_t sample,
    output bit has_progress,
    output bit should_exit,
    output memblock_sync_pkg::memblock_l2tlb_transport_terminal_e terminal_kind);
    memblock_sync_pkg::memblock_l2tlb_event_record_t event_record;
    bit response_selected;
    bit hold_active;
    bit lifecycle_blocked;
    bit request_csr_history_valid;
    bit next_ready;
    L2tlb_agent_agent_xaction cycle_tr;
    memblock_sync_pkg::dispatch_raw_csr_t ignored_runtime_csr;
    memblock_sync_pkg::dispatch_raw_csr_t request_csr_raw;
    int unsigned latest_runtime_csr_seq;
    int unsigned release_waiting_count;
    bit startup_history_seen;

    has_progress = 1'b0;
    should_exit = 1'b0;
    terminal_kind = memblock_sync_pkg::MEMBLOCK_L2TLB_SAMPLE_CONSUMED;
    release_waiting_count = 0;
    startup_history_seen = 1'b0;
    sample_seq = sample.dut_sample_seq;
    current_sample_reset_epoch = sample.sampled_reset_epoch;
    sampled_req_valid = sample.sampled_req_valid;
    sampled_req_ready = sample.sampled_req_ready;
    sampled_req_vpn = sample.sampled_req_vpn;
    sampled_req_s2xlate = sample.sampled_req_s2xlate;
    sampled_req_fire = sample.sampled_req_fire;
    sampled_resp_valid = sample.sampled_resp_valid;

    // The reset epoch belongs to the frozen transport sample. A sample that
    // was published before a later reset may still occupy the one-slot
    // mailbox, but it must never create token/UID work in the new epoch.
    if (sample.sampled_reset_epoch <
            memblock_sync_pkg::get_l2tlb_current_reset_epoch()) begin
        drop_stale_l2tlb_transport_sample(sample);
        terminal_kind = memblock_sync_pkg::MEMBLOCK_L2TLB_SAMPLE_DROPPED;
        return;
    end
    if (sample.sampled_reset_epoch >
            memblock_sync_pkg::get_l2tlb_current_reset_epoch()) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("future L2TLB transport sample epoch=%0d current=%0d transport=%0d",
                             sample.sampled_reset_epoch,
                             memblock_sync_pkg::get_l2tlb_current_reset_epoch(),
                             sample.transport_sample_seq))
    end

    if (sample.sampled_reset_active) begin
        cancel_outstanding_by_reset();
        memblock_sync_pkg::reset_l2tlb_response_owner_runtime_state(
            lifecycle_owner_name, sample.sampled_reset_epoch);
        // Reset discards old event history but preserves the allocator's
        // monotonic baseline.  Re-arm this owner at that baseline so only
        // post-reset events can create a new C0/C4 barrier.
        last_seen_flush_event_seq =
            memblock_sync_pkg::last_allocated_l2tlb_event_seq;
        fire_visible_event_seq = last_seen_flush_event_seq;
        accept_hold_until_sample = 0;
        acceptance_opened_since_reset = 1'b0;
        ready_opportunity_since_lifecycle_block = 1'b0;
        csr_snapshot_valid = 1'b0;
        require_post_reset_csr_refresh = 1'b1;
        if (memblock_sync_pkg::get_latest_runtime_csr_snapshot(
                ignored_runtime_csr, latest_runtime_csr_seq)) begin
            reset_runtime_csr_seq_baseline = latest_runtime_csr_seq;
        end else begin
            reset_runtime_csr_seq_baseline = 0;
        end
        baseline_sent_sample_seq = 0;
        pre_ready_hold_until_sample = 0;
        owner_start_baseline_done = 1'b0;
        release_close_requested = 1'b0;
        release_generation = 0;
        final_item_sent = 1'b0;
        idle_count = 0;
        stopping = data.is_global_stop_requested();
        cycle_tr = create_l2tlb_xaction($sformatf("l2tlb_reset_idle_%0d", sample.transport_sample_seq));
        stamp_lifecycle_item(cycle_tr,
                             memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_NORMAL,
                             0, 1'b0);
        ack_l2tlb_transport_sample(
            sample.transport_sample_seq,
            memblock_sync_pkg::MEMBLOCK_L2TLB_SAMPLE_DROPPED);
        send_l2tlb_item(cycle_tr);
        terminal_kind = memblock_sync_pkg::MEMBLOCK_L2TLB_SAMPLE_DROPPED;
        return;
    end

    if (!sample.sample_valid) begin
        note_l2tlb_sample_not_ready(sample, "sample_anchor_missing");
        if (sample.sampled_req_ready !== 1'b0 ||
            sample.sampled_req_fire || sample.sampled_resp_valid) begin
            `uvm_fatal(get_type_name(),
                       "invalid unanchored L2TLB transport sample contains fire/response")
        end
        cycle_tr = create_l2tlb_xaction($sformatf("l2tlb_unanchored_idle_%0d",
                                                   sample.transport_sample_seq));
        stamp_lifecycle_item(cycle_tr,
                             memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_NORMAL,
                             0, 1'b0);
        ack_l2tlb_transport_sample(
            sample.transport_sample_seq,
            memblock_sync_pkg::MEMBLOCK_L2TLB_SAMPLE_CONSUMED);
        send_l2tlb_item(cycle_tr);
        return;
    end

    // Terminal final proof is a transport fact, not a semantic CSR/event
    // lookup.  Consume it before the NOT_READY branch so a producer watermark
    // delay cannot strand the owner after the final item crossed the VIF.
    if (sample.sampled_final_inactive_proof_valid) begin
        if (!final_item_sent ||
            sample.sampled_item_kind !=
                memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_RELEASE_FINAL_INACTIVE ||
            sample.sampled_item_owner_name != lifecycle_owner_name ||
            sample.sampled_item_generation != release_generation ||
            sample.sampled_item_reset_epoch != sample.sampled_reset_epoch ||
            sample.sampled_req_ready !== 1'b0 ||
            sample.sampled_req_fire ||
            sample.sampled_resp_valid !== 1'b0 ||
            !memblock_sync_pkg::monitor_final_sample_settled(
                sample.sampled_reset_epoch, sample.transport_sample_seq)) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("invalid L2TLB final inactive proof sample=%0d owner=%s kind=%0d gen=%0d epoch=%0d ready=%b fire=%0d resp_valid=%b monitor_settled=%0d",
                                 sample.transport_sample_seq,
                                 sample.sampled_item_owner_name,
                                 sample.sampled_item_kind,
                                 sample.sampled_item_generation,
                                 sample.sampled_item_reset_epoch,
                                 sample.sampled_req_ready,
                                 sample.sampled_req_fire,
                                 sample.sampled_resp_valid,
                                 memblock_sync_pkg::monitor_final_sample_settled(
                                     sample.sampled_reset_epoch,
                                     sample.transport_sample_seq)))
        end
        memblock_sync_pkg::begin_l2tlb_release_closing(lifecycle_owner_name);
        ack_l2tlb_transport_sample(
            sample.transport_sample_seq,
            memblock_sync_pkg::MEMBLOCK_L2TLB_SAMPLE_CONSUMED);
        terminal_kind = memblock_sync_pkg::MEMBLOCK_L2TLB_SAMPLE_CONSUMED;
        return;
    end

    if (sample.sample_ready_result != memblock_sync_pkg::MEMBLOCK_L2TLB_SAMPLE_READY) begin
        note_l2tlb_sample_not_ready(sample, "sample_producer_not_ready");
        if (sample.sampled_req_ready !== 1'b0 ||
            sample.sampled_req_fire || sample.sampled_resp_valid) begin
            `uvm_fatal(get_type_name(),
                       "NOT_READY L2TLB sample contains an active request/response")
        end
        cycle_tr = create_l2tlb_xaction($sformatf("l2tlb_not_ready_%0d", sample.transport_sample_seq));
        stamp_lifecycle_item(cycle_tr,
                             memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_NORMAL,
                             0, 1'b0);
        ack_l2tlb_transport_sample(
            sample.transport_sample_seq,
            memblock_sync_pkg::MEMBLOCK_L2TLB_SAMPLE_CONSUMED);
        send_l2tlb_item(cycle_tr);
        return;
    end

    consecutive_not_ready_samples = 0;
    last_not_ready_sample_seq = 0;

    if (sample.baseline_required) begin
        if (sample.sampled_req_fire || sample.sampled_resp_valid) begin
            `uvm_fatal(get_type_name(),
                       "post-reset L2TLB baseline observed request/response activity")
        end
        if (!sample.baseline_proof_pending) begin
            cycle_tr = create_l2tlb_xaction($sformatf("l2tlb_post_reset_baseline_%0d",
                                                       sample.transport_sample_seq));
            stamp_lifecycle_item(cycle_tr,
                                 memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_NORMAL,
                                 0, 1'b1);
        end
        else begin
            if (sample.dut_sample_seq <= sample.baseline_sent_sample_seq ||
                sample.dut_sample_seq - sample.baseline_sent_sample_seq >
                    `MEMBLOCK_L2TLB_BASELINE_MAX_SAMPLE_DISTANCE) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("post-reset L2TLB baseline did not settle sent=%0d current=%0d",
                                     sample.baseline_sent_sample_seq,
                                     sample.dut_sample_seq))
            end
            cycle_tr = create_l2tlb_xaction($sformatf("l2tlb_post_reset_baseline_wait_%0d",
                                                       sample.transport_sample_seq));
            stamp_lifecycle_item(cycle_tr,
                                 memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_NORMAL,
                                 0, 1'b0);
        end
        ack_l2tlb_transport_sample(
            sample.transport_sample_seq,
            memblock_sync_pkg::MEMBLOCK_L2TLB_SAMPLE_CONSUMED);
        send_l2tlb_item(cycle_tr);
        return;
    end

    drain_csr_runtime_events();
    request_csr_history_valid =
        memblock_sync_pkg::get_l2tlb_request_csr_history(sample_seq,
                                                         request_csr_raw);
    if (!request_csr_history_valid) begin
        // C-2 history is part of READY for lifecycle consumers. Do not move
        // the event cursor or interpret a request fire before it is present.
        cycle_tr = create_l2tlb_xaction($sformatf("l2tlb_history_not_ready_%0d", sample_seq));
        stamp_lifecycle_item(cycle_tr,
                             memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_NORMAL,
                             0, 1'b0);
        ack_l2tlb_transport_sample(
            sample.transport_sample_seq,
            memblock_sync_pkg::MEMBLOCK_L2TLB_SAMPLE_CONSUMED);
        send_l2tlb_item(cycle_tr);
        return;
    end

    // Establish the owner-start baseline before interpreting old history.
    // Startup alignment must not silently inherit transport or semantic work
    // from a previous owner/reset epoch.
    if (!owner_start_baseline_done && !acceptance_opened_since_reset) begin
        data.check_l2tlb_release_uid_waiting(release_waiting_count);
        if (sampled_req_fire || sampled_resp_valid ||
            pending_q.size() != 0 || driving_valid || barrier_q.size() != 0 ||
            release_waiting_count != 0) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("L2TLB owner startup baseline is not empty sample=%0d req_fire=%0d resp_valid=%0d pending=%0d driving=%0d barriers=%0d waiting_uid=%0d",
                                 sample_seq, sampled_req_fire, sampled_resp_valid,
                                 pending_q.size(), driving_valid, barrier_q.size(),
                                 release_waiting_count))
        end
    end

    // Consume immutable event records by sequence. C0 records a barrier;
    // an older event may only be skipped during pre-ready startup.
    fire_visible_event_seq = last_seen_flush_event_seq;
    while (memblock_sync_pkg::get_l2tlb_event_after(last_seen_flush_event_seq,
                                                    event_record)) begin
        if (event_record.anchor_sample_seq > sample_seq) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("future L2TLB event seq=%0d anchor=%0d current=%0d",
                                 event_record.event_seq,
                                 event_record.anchor_sample_seq,
                                 sample_seq))
        end
        last_seen_flush_event_seq = event_record.event_seq;
        if (event_record.anchor_sample_seq < sample_seq) begin
            if (acceptance_opened_since_reset) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("late active L2TLB event seq=%0d anchor=%0d current=%0d",
                                     event_record.event_seq,
                                     event_record.anchor_sample_seq,
                                     sample_seq))
            end
            pre_ready_hold_until_sample =
                (pre_ready_hold_until_sample >
                 sample_seq + MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES) ?
                    pre_ready_hold_until_sample :
                    sample_seq + MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES;
            startup_history_seen = 1'b1;
            has_progress = 1'b1;
            continue;
        end
        void'(handle_l2tlb_flush_event(event_record));
        has_progress = 1'b1;
    end
    if (last_seen_flush_event_seq >
        memblock_sync_pkg::response_owner_event_cursor) begin
        memblock_sync_pkg::retire_l2tlb_event_history_prefix(
            last_seen_flush_event_seq);
    end

    if (startup_history_seen) begin
        owner_start_baseline_done = 1'b1;
        ready_opportunity_since_lifecycle_block = 1'b0;
    end

    due_barrier_this_sample =
        apply_due_l2tlb_flush_barriers(sample_seq) != 0;

    if (driving_valid && sampled_resp_valid) begin
        if (due_barrier_this_sample) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("L2TLB response would fire on filter due sample=%0d",
                                 sample_seq))
        end
        complete_driving_response();
        has_progress = 1'b1;
    end
    else if (driving_valid && !sampled_resp_valid) begin
        if (due_barrier_this_sample) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("L2TLB response remains driving at filter due sample=%0d",
                                 sample_seq))
        end
        // Keep the selected response visible until the frozen transport sample
        // proves that the DUT observed its valid pulse.
        cycle_tr = driving_req.resp_tr;
    end
    else if (!driving_valid && sampled_resp_valid) begin
        `uvm_fatal(get_type_name(),
                   "L2TLB response valid observed without a driving response token")
    end

    // C0 request fire is real DUT admission and must always get a token,
    // including the sample that first observes a fence.
    if (request_fire()) begin
        if (!csr_snapshot_valid) begin
            `uvm_fatal(get_type_name(), "L2TLB request fired before first runtime CSR snapshot")
        end
        void'(capture_fired_request());
        has_progress = 1'b1;
    end

    if (data.is_global_stop_requested()) begin
        stopping = 1'b1;
    end

    if (!release_close_requested && stopping) begin
        memblock_sync_pkg::mark_l2tlb_owner_admission_settled(
            lifecycle_owner_name, sample_seq);
        release_generation = memblock_sync_pkg::close_l2tlb_admission_for_release(
            lifecycle_owner_name, sample_seq);
        release_close_requested = 1'b1;
    end

    if (!stopping && !release_close_requested) begin
        memblock_sync_pkg::mark_l2tlb_owner_admission_settled(
            lifecycle_owner_name, sample_seq);
    end

    hold_active = (sample_seq < accept_hold_until_sample) ||
                  (sample_seq < pre_ready_hold_until_sample);
    lifecycle_blocked = !csr_snapshot_valid ||
                        !request_csr_history_valid || hold_active;
    if (has_progress || lifecycle_blocked || stopping ||
        outstanding_count() != 0 || !acceptance_opened_since_reset ||
        !ready_opportunity_since_lifecycle_block) begin
        idle_count = 0;
    end else if (idle_count < idle_stop_cycle) begin
        idle_count++;
        if (idle_count == idle_stop_cycle) begin
            // idle threshold is only a diagnostic watchdog.  Closing
            // admission here could leave a still-active dispatch flow without
            // an L2TLB responder; only global_stop_requested may set stopping.
            `uvm_warning(get_type_name(),
                         $sformatf("L2TLB responder no-progress diagnostic at sample=%0d idle_count=%0d; keep owner active until global stop",
                                   sample_seq, idle_count))
        end
    end

    if (release_close_requested &&
        memblock_sync_pkg::l2tlb_release_admission_closed &&
        !final_item_sent && outstanding_count() == 0 &&
        barrier_q.size() == 0) begin
        // An issue-time UID record is only a candidate until the responder
        // observes its real request fire.  Once admission is closed and all
        // transport work is drained, marker==0 proves that this UID took a
        // DTLB-hit/Bare path and has no L2TLB response to wait for.
        void'(data.cancel_unbound_uid_tlb_records_at_release(
            "admission cutoff reached without L2TLB request fire"));
        // At this point no token or barrier can produce another UID payload.
        // Make any remaining WAITING instance a diagnosable lifecycle error;
        // do not spin until the global UVM timeout.
        data.check_l2tlb_release_uid_waiting(release_waiting_count);
        if (release_waiting_count != 0) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("L2TLB release has %0d unresolved WAITING UID instance(s)",
                                 release_waiting_count))
        end
        memblock_sync_pkg::mark_l2tlb_response_drain_done(lifecycle_owner_name);
        cycle_tr = create_l2tlb_xaction($sformatf("l2tlb_release_final_%0d", sample_seq));
        stamp_lifecycle_item(cycle_tr,
                             memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_RELEASE_FINAL_INACTIVE,
                             release_generation, 1'b0);
        final_item_sent = 1'b1;
        ack_l2tlb_transport_sample(
            sample.transport_sample_seq,
            memblock_sync_pkg::MEMBLOCK_L2TLB_SAMPLE_CONSUMED);
        send_l2tlb_item(cycle_tr);
        return;
    end

    response_selected = 1'b0;
    if (cycle_tr == null && csr_snapshot_valid && request_csr_history_valid &&
        !due_barrier_this_sample) begin
        response_selected = select_due_response(sample_seq + 1, cycle_tr);
        if (response_selected) begin
            has_progress = 1'b1;
        end
    end
    if (cycle_tr == null) begin
        cycle_tr = create_l2tlb_xaction($sformatf("l2tlb_cycle_%0d", sample_seq));
    end

    next_ready = !stopping && !release_close_requested &&
                 csr_snapshot_valid && request_csr_history_valid &&
                 !hold_active &&
                 outstanding_count() < max_outstanding;
    if (next_ready && !owner_start_baseline_done) begin
        data.check_l2tlb_release_uid_waiting(release_waiting_count);
        if (sampled_req_fire || sampled_resp_valid ||
            pending_q.size() != 0 || driving_valid || barrier_q.size() != 0 ||
            release_waiting_count != 0) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("L2TLB owner startup baseline is not empty sample=%0d req_fire=%0d resp_valid=%0d pending=%0d driving=%0d barriers=%0d waiting_uid=%0d",
                                 sample_seq, sampled_req_fire, sampled_resp_valid,
                                 pending_q.size(), driving_valid, barrier_q.size(),
                                 release_waiting_count))
        end
        owner_start_baseline_done = 1'b1;
    end
    cycle_tr.io_ptw_req_0_ready = next_ready;
    cycle_tr.pre_pkt_gap = 0;
    cycle_tr.post_pkt_gap = 0;
    if (release_close_requested &&
        !memblock_sync_pkg::l2tlb_release_admission_closed) begin
        cycle_tr.io_ptw_req_0_ready = 1'b0;
        stamp_lifecycle_item(cycle_tr,
                             memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_RELEASE_STOP,
                             release_generation, 1'b0);
    end
    else if (release_close_requested) begin
        // Admission is already closed.  Keep any selected response visible;
        // the item is now an ordinary inactive transport item and must not
        // re-confirm RELEASE_STOP on every following cycle.
        cycle_tr.io_ptw_req_0_ready = 1'b0;
        stamp_lifecycle_item(cycle_tr,
                             memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_NORMAL,
                             0, 1'b0);
    end
    else begin
        stamp_lifecycle_item(cycle_tr,
                             memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_NORMAL,
                             0, 1'b0);
    end
    if (next_ready) begin
        acceptance_opened_since_reset = 1'b1;
    end
    ack_l2tlb_transport_sample(
        sample.transport_sample_seq,
        memblock_sync_pkg::MEMBLOCK_L2TLB_SAMPLE_CONSUMED);
    send_l2tlb_item(cycle_tr);
    if (next_ready) begin
        // 中文注释：finish_item返回后，ready机会才算真正交给driver/DUT。
        ready_opportunity_since_lifecycle_block = 1'b1;
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
    // The parameters have already been snapshotted by seq_csr_common::init;
    // repeat the pure validation before this owner can make ready visible.
    seq_csr_common::check_l2tlb_payload_weight_cfg();
endfunction:configure_from_plus

function void memblock_l2tlb_base_sequence::ensure_context();
    data = common_data_transaction::get();
    if (data == null) begin
        `uvm_fatal(get_type_name(), "failed to get common_data_transaction")
    end
endfunction:ensure_context

function void memblock_l2tlb_base_sequence::initialize_lifecycle_state();
    pending_q.delete();
    barrier_q.delete();
    driving_req = null;
    driving_valid = 1'b0;
    due_barrier_this_sample = 1'b0;
    fire_visible_event_seq = 0;
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
    sampled_req_fire = 1'b0;
    sampled_resp_valid = 1'b0;
    consecutive_not_ready_samples = 0;
    last_not_ready_sample_seq = 0;
    current_sample_reset_epoch = 0;
    release_close_requested = 1'b0;
    release_generation = 0;
    final_item_sent = 1'b0;
    baseline_sent_sample_seq = 0;
    pre_ready_hold_until_sample = 0;
    owner_start_baseline_done = 1'b0;
endfunction:initialize_lifecycle_state

task memblock_l2tlb_base_sequence::wait_for_l2tlb_transport_sample(
    output L2tlb_agent_agent_transport_sample sample_ref,
    output memblock_l2tlb_drv_sample_t sample);
    sample_ref = null;
    sample = '{default:'0,
               sample_ready_result:
                   memblock_sync_pkg::MEMBLOCK_L2TLB_SAMPLE_NOT_READY,
               sampled_item_kind:
                   memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_NORMAL};
    if (l2tlb_sqr == null) begin
        `uvm_fatal(get_type_name(), "L2TLB transport sample requested before sequencer context")
    end
    l2tlb_sqr.wait_transport_sample(sample_ref);
    if (sample_ref == null || !sample_ref.get_payload(sample)) begin
        `uvm_fatal(get_type_name(), "failed to obtain frozen L2TLB transport sample")
    end
    if (sample.transport_sample_seq == 0 ||
        (sample.dut_sample_seq == 0 && sample.sample_valid)) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("invalid frozen L2TLB sample transport=%0d dut=%0d valid=%0d",
                             sample.transport_sample_seq,
                             sample.dut_sample_seq,
                             sample.sample_valid))
    end
endtask:wait_for_l2tlb_transport_sample

function void memblock_l2tlb_base_sequence::ack_l2tlb_transport_sample(
    input longint unsigned transport_sample_seq,
    input memblock_sync_pkg::memblock_l2tlb_transport_terminal_e terminal_kind);
    if (l2tlb_sqr == null ||
        !l2tlb_sqr.ack_transport_sample(transport_sample_seq, terminal_kind)) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("failed to acknowledge L2TLB transport sample seq=%0d kind=%0d",
                             transport_sample_seq, terminal_kind))
    end
endfunction:ack_l2tlb_transport_sample

task memblock_l2tlb_base_sequence::drop_stale_l2tlb_transport_sample(
    input memblock_l2tlb_drv_sample_t sample);
    L2tlb_agent_agent_xaction cycle_tr;

    cycle_tr = create_l2tlb_xaction($sformatf("l2tlb_stale_epoch_idle_%0d",
                                               sample.transport_sample_seq));
    stamp_lifecycle_item(cycle_tr,
                         memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_NORMAL,
                         0, 1'b0);
    ack_l2tlb_transport_sample(
        sample.transport_sample_seq,
        memblock_sync_pkg::MEMBLOCK_L2TLB_SAMPLE_DROPPED);
    send_l2tlb_item(cycle_tr);
endtask:drop_stale_l2tlb_transport_sample

function void memblock_l2tlb_base_sequence::note_l2tlb_sample_not_ready(
    input memblock_l2tlb_drv_sample_t sample,
    input string reason);
    if (last_not_ready_sample_seq != sample.transport_sample_seq) begin
        consecutive_not_ready_samples++;
        last_not_ready_sample_seq = sample.transport_sample_seq;
    end
    if (consecutive_not_ready_samples >
        `MEMBLOCK_L2TLB_SAMPLE_NOT_READY_MAX_SAMPLES) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("L2TLB sample readiness watchdog expired reason=%s epoch=%0d transport=%0d dut_sample=%0d count=%0d csr_watermark=%0d event_watermark=%0d",
                             reason,
                             sample.sampled_reset_epoch,
                             sample.transport_sample_seq,
                             sample.dut_sample_seq,
                             consecutive_not_ready_samples,
                             memblock_sync_pkg::csr_history_published_seq,
                             memblock_sync_pkg::lifecycle_event_published_seq))
    end
endfunction:note_l2tlb_sample_not_ready

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
    return sampled_req_fire;
endfunction:request_fire

function int unsigned memblock_l2tlb_base_sequence::outstanding_count();
    return pending_q.size() + (driving_valid ? 1 : 0);
endfunction:outstanding_count

function bit memblock_l2tlb_base_sequence::get_request_csr_snapshot(
    output mmu_csr_runtime_state snapshot);
    memblock_sync_pkg::dispatch_raw_csr_t raw_csr;

    snapshot = null;
    if (!memblock_sync_pkg::get_l2tlb_request_csr_history(sample_seq,
                                                          raw_csr)) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("missing V2 C-2 CSR history for request sample=%0d",
                             sample_seq))
        return 1'b0;
    end
    snapshot = mmu_csr_runtime_state::type_id::create(
        $sformatf("l2tlb_request_csr_%0d", sample_seq));
    if (snapshot == null) begin
        `uvm_fatal(get_type_name(), "failed to allocate request CSR snapshot")
        return 1'b0;
    end
    snapshot.reset();
    snapshot.update_from_raw_csr(raw_csr);
    return 1'b1;
endfunction:get_request_csr_snapshot

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
    if (audit_context == "owner_release" &&
        (barrier_q.size() != 0 ||
         (data != null && data.has_waiting_uid_tlb_record()))) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("L2TLB lifecycle release is not quiescent barrier_count=%0d waiting_uid=%0d",
                             barrier_q.size(),
                             (data != null) ? data.has_waiting_uid_tlb_record() : 1'b0))
    end
endfunction:check_l2tlb_lifecycle_accounting

function void memblock_l2tlb_base_sequence::cancel_outstanding_by_reset();
    int unsigned canceled_count;

    canceled_count = outstanding_count();
    reset_canceled_count += canceled_count;
    void'(data.cancel_waiting_uid_tlb_records("l2tlb_runtime_reset"));
    pending_q.delete();
    barrier_q.delete();
    driving_req = null;
    driving_valid = 1'b0;
    due_barrier_this_sample = 1'b0;
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
    bit created;

    // A real fire is still accepted before the same-sample stop path seals
    // admission.  Once the local or shared cutoff is visible, reject the
    // fire before allocating a token, pending record, or UID marker.
    if (release_close_requested) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("L2TLB request fired after local admission cutoff sample=%0d vpn=0x%0h s2xlate=%0d",
                             sample_seq, sampled_req_vpn, sampled_req_s2xlate))
    end
    data.check_l2tlb_uid_registration_open("capture_fired_request");
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
    if (!get_request_csr_snapshot(pending.csr_snapshot)) begin
        pending.csr_snapshot = null;
    end
    if (pending.csr_snapshot == null) begin
        `uvm_fatal(get_type_name(), "failed to capture request-time CSR snapshot")
    end
    pending.request_lookup_key = pending.csr_snapshot.make_lookup_key(
        {26'b0, pending.vpn}, pending.s2xlate);
    if (!data.get_or_create_tlb_entry_by_req_with_snapshot(pending.vpn,
                                                            pending.s2xlate,
                                                            pending.csr_snapshot,
                                                            pending.request_lookup_key,
                                                            pending.entry_anchor_key,
                                                            pending.lookup_result,
                                                            live_entry,
                                                            created) ||
        live_entry == null) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("failed to get/create L2TLB entry vpn=0x%0h s2xlate=%0d",
                             pending.vpn, pending.s2xlate))
    end
    if (pending.request_lookup_key !=
        pending.csr_snapshot.make_lookup_key({26'b0, pending.vpn},
                                              pending.s2xlate)) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("request-time L2TLB key drift token=%0d snapshot_vpn=0x%0h request_vpn=0x%0h",
                             pending.request_token,
                             pending.vpn,
                             pending.request_lookup_key.vpn))
    end
    if (live_entry.lookup_key != pending.entry_anchor_key ||
        live_entry.entry_generation == 0) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("L2TLB canonical anchor mismatch token=%0d request_vpn=0x%0h anchor_vpn=0x%0h",
                             pending.request_token,
                             pending.request_lookup_key.vpn,
                             pending.entry_anchor_key.vpn))
    end
    // The UID ledger records only a real DTLB/L2TLB request fire.  It is not
    // inferred from issue time or from a later response lookup.
    void'(data.mark_uid_tlb_record_request_fire(pending.request_lookup_key, sample_seq));
    pending.entry_snapshot = memblock_tlb_entry::type_id::create(
        $sformatf("l2tlb_entry_snapshot_%0d", pending.request_token));
    if (pending.entry_snapshot == null) begin
        `uvm_fatal(get_type_name(), "failed to create request-time L2TLB entry snapshot")
    end
    pending.entry_snapshot.copy_from(live_entry);
    pending.pending_entry_generation = pending.entry_snapshot.entry_generation;
    if (pending.pending_entry_generation == 0 ||
        pending.pending_entry_generation != live_entry.entry_generation) begin
        `uvm_fatal(get_type_name(), "L2TLB pending entry generation is not frozen from live entry")
    end
    // Derived addresses belong to this request, not to the first lookup that
    // created the live entry.  In particular, a superpage/NAPOT response may
    // raw-hit a different waiting VPN; derive it from that request's frozen
    // VPN/GVPN and the request-visible CSR snapshot.
    data.derive_tlb_request_fields(
        pending.entry_snapshot,
        {14'b0, pending.vpn},
        pending.s2xlate,
        pending.csr_snapshot,
        pending.request_derived_valid,
        pending.request_s1_resolved_ppn,
        pending.request_s2_resolved_ppn,
        pending.request_gvpn);
    pending.resp_tr = create_l2tlb_xaction(
        $sformatf("l2tlb_resp_token_%0d", pending.request_token));
    pending.resp_tr.io_ptw_req_0_valid = 1'b1;
    pending.resp_tr.io_ptw_req_0_bits_vpn = pending.vpn;
    pending.resp_tr.io_ptw_req_0_bits_s2xlate = pending.s2xlate;
    fill_dtlb_resp_from_entry(pending.entry_snapshot, pending.resp_tr);
    pending.min_latency = choose_latency(pending.latency_bucket);
    pending.accept_sample_seq = sample_seq;
    pending.due_sample_seq = sample_seq + pending.min_latency;
    // A request firing in the same sample as a new event belongs to the
    // pre-event visibility window; C4 cancellation compares against this
    // cursor, not the event just observed at C0.
    pending.accept_flush_event_seq = fire_visible_event_seq;
    pending_q.push_back(pending);
    accepted_count++;
    `uvm_info(get_type_name(),
              $sformatf("accept L2TLB token=%0d vpn=0x%0h s2xlate=%0d lookup=%0d anchor_vpn=0x%0h created=%0d due=%0d bucket=%0d outstanding=%0d",
                        pending.request_token, pending.vpn, pending.s2xlate,
                        pending.lookup_result, pending.entry_anchor_key.vpn,
                        created, pending.due_sample_seq,
                        pending.latency_bucket, outstanding_count()),
              UVM_LOW)
    check_l2tlb_lifecycle_accounting("request_accept");
    return pending;
endfunction:capture_fired_request

function int unsigned memblock_l2tlb_base_sequence::handle_l2tlb_flush_event(
    input memblock_sync_pkg::memblock_l2tlb_event_record_t event_record);
    if (event_record.event_seq == memblock_sync_pkg::MEMBLOCK_L2TLB_EVENT_SEQ_NONE ||
        event_record.anchor_sample_seq != sample_seq) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("invalid current L2TLB barrier event_seq=%0d anchor=%0d sample=%0d",
                             event_record.event_seq,
                             event_record.anchor_sample_seq,
                             sample_seq))
    end
    barrier_q.push_back(event_record);
    accept_hold_until_sample = sample_seq + MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES;
    ready_opportunity_since_lifecycle_block = 1'b0;
    `uvm_info(get_type_name(),
              $sformatf("record L2TLB flush barrier event_seq=%0d anchor=%0d due=%0d hold_until=%0d",
                        event_record.event_seq,
                        event_record.anchor_sample_seq,
                        event_record.anchor_sample_seq + MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES,
                        accept_hold_until_sample),
              UVM_LOW)
    return 1;
endfunction:handle_l2tlb_flush_event

function int unsigned memblock_l2tlb_base_sequence::apply_due_l2tlb_flush_barriers(
    input longint unsigned current_sample_seq);
    int unsigned due_count;

    due_count = 0;
    while (barrier_q.size() != 0 &&
           barrier_q[0].anchor_sample_seq + MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES <=
           current_sample_seq) begin
        memblock_sync_pkg::memblock_l2tlb_event_record_t barrier;
        int unsigned barrier_cancel_count;

        barrier = barrier_q.pop_front();
        barrier_cancel_count = 0;
        due_count++;
        // A response selected for the due sample is checked by the caller
        // before completion; only still-pending tokens are canceled here.
        for (int idx = int'(pending_q.size()) - 1; idx >= 0; idx--) begin
            if (pending_q[idx].accept_flush_event_seq < barrier.event_seq) begin
                pending_q.delete(idx);
                barrier_cancel_count++;
            end
        end
        // C4 cancels only UID records with an observed request-fire marker in
        // the pre-barrier visibility window.  UNBOUND records remain eligible
        // for a later real request and are never canceled by this path.
        void'(data.cancel_waiting_uid_tlb_records_through_sample(
            barrier.anchor_sample_seq,
            $sformatf("l2tlb_flush_due_event_%0d", barrier.event_seq)));
        flush_canceled_count += barrier_cancel_count;
        // The due sample itself emits an inactive cycle; reopen admission on
        // the following sample.
        accept_hold_until_sample = current_sample_seq + 1;
        `uvm_info(get_type_name(),
                  $sformatf("apply L2TLB due barrier event_seq=%0d due=%0d canceled=%0d",
                            barrier.event_seq,
                            barrier.anchor_sample_seq + MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES,
                            barrier_cancel_count),
                  UVM_LOW)
    end
    if (due_count != 0) begin
        check_l2tlb_lifecycle_accounting("due_flush");
    end
    return due_count;
endfunction:apply_due_l2tlb_flush_barriers

function bit memblock_l2tlb_base_sequence::select_due_response(
    input longint unsigned next_sample_seq,
    output L2tlb_agent_agent_xaction cycle_tr);
    int unsigned selected_index;
    int unsigned eligible_indices[$];
    int unsigned eligible_count;
    int unsigned choice;

    cycle_tr = null;
    foreach (barrier_q[barrier_idx]) begin
        if (barrier_q[barrier_idx].anchor_sample_seq +
            MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES <= next_sample_seq) begin
            // The response would be visible on the filter due sample.
            return 1'b0;
        end
    end
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
    mmu_csr_runtime_state response_filter_csr_snapshot;

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
    if (!get_request_csr_snapshot(response_filter_csr_snapshot) ||
        response_filter_csr_snapshot == null) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("missing response-visible C-2 CSR for L2TLB response sample=%0d",
                             complete_sample_seq))
    end
    record_update_count = data.complete_waiting_uid_records_by_response(
        driving_req.entry_snapshot,
        response_filter_csr_snapshot);
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

function void memblock_l2tlb_base_sequence::stamp_lifecycle_item(
    input L2tlb_agent_agent_xaction tr,
    input memblock_sync_pkg::memblock_l2tlb_release_item_kind_e item_kind,
    input longint unsigned generation,
    input bit is_post_reset_baseline);
    longint unsigned item_epoch;

    if (tr == null) begin
        `uvm_fatal(get_type_name(), "stamp_lifecycle_item got null xaction")
    end
    item_epoch = current_sample_reset_epoch;
    if (lifecycle_owner_name == "") begin
        `uvm_fatal(get_type_name(), "cannot stamp L2TLB item without lifecycle owner")
    end
    if (is_post_reset_baseline &&
        item_kind != memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_NORMAL) begin
        `uvm_fatal(get_type_name(), "post-reset baseline metadata requires NORMAL item")
    end
    if (item_kind != memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_NORMAL &&
        generation == 0) begin
        `uvm_fatal(get_type_name(), "release L2TLB item requires non-zero generation")
    end
    if (item_kind == memblock_sync_pkg::MEMBLOCK_L2TLB_ITEM_NORMAL &&
        generation != 0) begin
        `uvm_fatal(get_type_name(), "NORMAL L2TLB item cannot carry release generation")
    end

    tr.item_kind = item_kind;
    tr.item_generation = generation;
    tr.item_reset_epoch = item_epoch;
    tr.item_owner_name = lifecycle_owner_name;
    tr.is_post_reset_baseline = is_post_reset_baseline;
endfunction:stamp_lifecycle_item

function void memblock_l2tlb_base_sequence::fill_dtlb_resp_from_entry(input memblock_tlb_entry entry,
                                                                      ref L2tlb_agent_agent_xaction resp);
    if (resp == null || entry == null) begin
        `uvm_fatal(get_type_name(), "fill_dtlb_resp_from_entry got null input")
    end

    resp.io_ptw_resp_valid = 1'b1;
    resp.io_ptw_resp_bits_s2xlate = entry.s2xlate;
    if (entry.pmaAF && entry.has_effective_fault())
        `uvm_fatal("L2TLB_PMA_FAULT_MIX", "pmaAF cannot be combined with modeled TLB fault")
    if (|entry.s1_vmid[15:14] || |entry.s2_vmid[15:14]) begin
        `uvm_fatal("L2TLB_PAYLOAD_VMID_WIDTH",
                   $sformatf("response VMID cannot be encoded s1=0x%0h s2=0x%0h",
                             entry.s1_vmid, entry.s2_vmid))
    end
    entry.check_inactive_stage_defaults("DRIVE");
    entry.validate_s1_sector_payload_consistency("DRIVE");
    resp.io_ptw_resp_bits_s1_entry_tag = entry.s1_tag;
    resp.io_ptw_resp_bits_s1_entry_asid = entry.s1_asid;
    resp.io_ptw_resp_bits_s1_entry_vmid = entry.s1_vmid[13:0];
    resp.io_ptw_resp_bits_s1_entry_n = entry.s1_pte_n;
    resp.io_ptw_resp_bits_s1_entry_pbmt = entry.s1_entry_pbmt;
    resp.io_ptw_resp_bits_s1_entry_perm_d = entry.s1_pte_d;
    resp.io_ptw_resp_bits_s1_entry_perm_a = entry.s1_pte_a;
    resp.io_ptw_resp_bits_s1_entry_perm_g = entry.s1_pte_g;
    resp.io_ptw_resp_bits_s1_entry_perm_u = entry.s1_pte_u;
    resp.io_ptw_resp_bits_s1_entry_perm_x = entry.s1_pte_x;
    resp.io_ptw_resp_bits_s1_entry_perm_w = entry.s1_pte_w;
    resp.io_ptw_resp_bits_s1_entry_perm_r = entry.s1_pte_r;
    resp.io_ptw_resp_bits_s1_entry_level = entry.s1_level;
    resp.io_ptw_resp_bits_s1_entry_v = entry.s1_pte_v;
    resp.io_ptw_resp_bits_s1_entry_ppn = entry.s1_entry_ppn_raw;
    resp.io_ptw_resp_bits_s1_addr_low = entry.s1_addr_low;
    resp.io_ptw_resp_bits_s1_ppn_low_0 = entry.s1_ppn_low[0]; resp.io_ptw_resp_bits_s1_ppn_low_1 = entry.s1_ppn_low[1];
    resp.io_ptw_resp_bits_s1_ppn_low_2 = entry.s1_ppn_low[2]; resp.io_ptw_resp_bits_s1_ppn_low_3 = entry.s1_ppn_low[3];
    resp.io_ptw_resp_bits_s1_ppn_low_4 = entry.s1_ppn_low[4]; resp.io_ptw_resp_bits_s1_ppn_low_5 = entry.s1_ppn_low[5];
    resp.io_ptw_resp_bits_s1_ppn_low_6 = entry.s1_ppn_low[6]; resp.io_ptw_resp_bits_s1_ppn_low_7 = entry.s1_ppn_low[7];
    resp.io_ptw_resp_bits_s1_valididx_0 = entry.s1_valididx[0]; resp.io_ptw_resp_bits_s1_valididx_1 = entry.s1_valididx[1];
    resp.io_ptw_resp_bits_s1_valididx_2 = entry.s1_valididx[2]; resp.io_ptw_resp_bits_s1_valididx_3 = entry.s1_valididx[3];
    resp.io_ptw_resp_bits_s1_valididx_4 = entry.s1_valididx[4]; resp.io_ptw_resp_bits_s1_valididx_5 = entry.s1_valididx[5];
    resp.io_ptw_resp_bits_s1_valididx_6 = entry.s1_valididx[6]; resp.io_ptw_resp_bits_s1_valididx_7 = entry.s1_valididx[7];
    resp.io_ptw_resp_bits_s1_pteidx_0 = entry.s1_pteidx[0]; resp.io_ptw_resp_bits_s1_pteidx_1 = entry.s1_pteidx[1];
    resp.io_ptw_resp_bits_s1_pteidx_2 = entry.s1_pteidx[2]; resp.io_ptw_resp_bits_s1_pteidx_3 = entry.s1_pteidx[3];
    resp.io_ptw_resp_bits_s1_pteidx_4 = entry.s1_pteidx[4]; resp.io_ptw_resp_bits_s1_pteidx_5 = entry.s1_pteidx[5];
    resp.io_ptw_resp_bits_s1_pteidx_6 = entry.s1_pteidx[6]; resp.io_ptw_resp_bits_s1_pteidx_7 = entry.s1_pteidx[7];
    resp.io_ptw_resp_bits_s1_pf = entry.fault_effective_s1_pf;
    resp.io_ptw_resp_bits_s1_af = entry.fault_effective_s1_af || entry.pmaAF;
    resp.io_ptw_resp_bits_s2_entry_tag = entry.s2_tag;
    resp.io_ptw_resp_bits_s2_entry_vmid = entry.s2_vmid[13:0];
    resp.io_ptw_resp_bits_s2_entry_n = entry.s2_pte_n;
    resp.io_ptw_resp_bits_s2_entry_pbmt = entry.s2_entry_pbmt;
    resp.io_ptw_resp_bits_s2_entry_ppn = entry.s2_entry_ppn_raw;
    resp.io_ptw_resp_bits_s2_entry_perm_d = entry.s2_pte_d;
    resp.io_ptw_resp_bits_s2_entry_perm_a = entry.s2_pte_a;
    resp.io_ptw_resp_bits_s2_entry_perm_g = entry.s2_pte_g;
    resp.io_ptw_resp_bits_s2_entry_perm_u = entry.s2_pte_u;
    resp.io_ptw_resp_bits_s2_entry_perm_x = entry.s2_pte_x;
    resp.io_ptw_resp_bits_s2_entry_perm_w = entry.s2_pte_w;
    resp.io_ptw_resp_bits_s2_entry_perm_r = entry.s2_pte_r;
    resp.io_ptw_resp_bits_s2_entry_level = entry.s2_level;
    resp.io_ptw_resp_bits_s2_gpf = entry.fault_effective_s2_gpf;
    resp.io_ptw_resp_bits_s2_gaf = entry.fault_effective_s2_gaf;
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
