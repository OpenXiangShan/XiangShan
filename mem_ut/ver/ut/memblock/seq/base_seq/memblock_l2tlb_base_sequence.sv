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
    memblock_later_split_tlb_record later_split_owner;
    // 中文注释：该 generation 与 snapshot 同源，标识本 token 命中的 live entry 时代。
    // response 完成、flush 或 driver 重试都不能从 live table 重新读取或改写它。
    longint unsigned pending_entry_generation;
    L2tlb_agent_agent_xaction resp_tr;
    // 中文注释：response select 时绑定的 DUT sample 和 PBMTE 结果。首次 select
    // 会在 entry_snapshot 上叠加 effective PF/GPF 并置 frozen；completion 必须
    // 使用同一 sample/C-2 PBMTE，不能让后续 CSR 变化改写已驱动 payload。
    longint unsigned response_visible_sample_seq;
    bit response_payload_frozen;
    bit response_m_pbmt_en;
    bit response_h_pbmt_en;
    bit response_pbmt_forced_s1_pf;
    bit response_pbmt_forced_s2_gpf;
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
        later_split_owner = null;
        pending_entry_generation = 0;
        resp_tr = null;
        response_visible_sample_seq = 0;
        response_payload_frozen = 1'b0;
        response_m_pbmt_en = 1'b0;
        response_h_pbmt_en = 1'b0;
        response_pbmt_forced_s1_pf = 1'b0;
        response_pbmt_forced_s2_gpf = 1'b0;
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
    // 中文注释：本 responder 启动时冻结的 PPN reuse 配置。
    // EN=0 时 request capture 不会扫描 history，response completion 也不会写 FIFO，确保默认随机序列不变。
    bit          ppn_reuse_en;
    int unsigned ppn_reuse_history_size;
    int unsigned ppn_reuse_wt;

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
    memblock_pbmt_rf_audit_slot_t pbmt_rf_audit[2];
    bit                           pbmt_rf_audit_final_read;
    int unsigned                  pbmt_rf_readiness_wait_samples;
    memblock_sync_pkg::memblock_pbmt_rf_phase_e pbmt_rf_readiness_wait_phase;

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
    extern function bit check_l2tlb_lifecycle_accounting(input string audit_context);
    extern function void reset_pbmt_rf_audit_ledger();
    extern function bit get_pbmt_rf_audit_slot(
        input memblock_pbmt_rf_slot_kind_e slot,
        output memblock_pbmt_rf_audit_slot_t slot_copy);
    extern function void cleanup_pbmt_rf_audit_ledger_after_final_read();
    extern function void service_pbmt_rf_readiness(
        input longint unsigned current_sample_seq,
        input memblock_sync_pkg::dispatch_raw_csr_t request_csr_raw);
    extern function void mark_pbmt_rf_audit_canceled(
        input longint unsigned request_token,
        input string reason);
    extern function bit capture_pbmt_rf_complete(
        input memblock_l2tlb_pending_req pending,
        input mmu_csr_runtime_state actual_response_csr_snapshot);
    extern function longint unsigned get_l2tlb_accepted_count();
    extern function longint unsigned get_l2tlb_completed_count();
    extern function longint unsigned get_l2tlb_canceled_count();
    extern function void cancel_outstanding_by_reset();
    extern function memblock_l2tlb_pending_req capture_fired_request();
    extern function bit precheck_pbmt_rf_request_fire_commit(
        input memblock_l2tlb_pending_req pending,
        input memblock_uid_tlb_record resolved_record,
        input pma_pmp_uid_context_plan_t context_plan,
        input memblock_pbmt_rf_audit_slot_t prepared_slot,
        input memblock_sync_pkg::memblock_pbmt_rf_phase_e expected_phase,
        input memblock_sync_pkg::memblock_pbmt_rf_phase_e accepted_phase);
    extern function void commit_pbmt_rf_request_fire_prechecked(
        input memblock_l2tlb_pending_req pending,
        input memblock_uid_tlb_record resolved_record,
        input pma_pmp_uid_context_plan_t context_plan,
        input memblock_pbmt_rf_audit_slot_t prepared_slot,
        input memblock_sync_pkg::memblock_pbmt_rf_phase_e expected_phase,
        input memblock_sync_pkg::memblock_pbmt_rf_phase_e accepted_phase);
    extern function int unsigned handle_l2tlb_flush_event(
        input memblock_sync_pkg::memblock_l2tlb_event_record_t event_record);
    extern function int unsigned apply_due_l2tlb_flush_barriers(
        input longint unsigned current_sample_seq);
    extern function bit get_csr_snapshot_for_sample(
        input longint unsigned target_sample_seq,
        output mmu_csr_runtime_state snapshot);
    extern function bit select_due_response(input longint unsigned next_sample_seq,
                                            output L2tlb_agent_agent_xaction cycle_tr);
    extern function void complete_driving_response();
    extern function L2tlb_agent_agent_xaction create_l2tlb_xaction(input string name);
    extern function void clear_l2tlb_xaction(input L2tlb_agent_agent_xaction tr);
    extern function void fill_dtlb_resp_from_entry(input memblock_tlb_entry entry,
                                                   ref L2tlb_agent_agent_xaction resp);
    extern function void apply_pbmt_response_overlay(
        input memblock_tlb_entry entry,
        input mmu_csr_runtime_state response_csr,
        output bit force_s1_pf,
        output bit force_s2_gpf);
    extern function void stamp_lifecycle_item(
        input L2tlb_agent_agent_xaction tr,
        input memblock_sync_pkg::memblock_l2tlb_release_item_kind_e item_kind,
        input longint unsigned generation,
        input bit is_post_reset_baseline);
    extern function int unsigned choose_latency(output memblock_l2tlb_latency_bucket_e bucket);

endclass:memblock_l2tlb_base_sequence

function memblock_l2tlb_base_sequence::new(string name = "memblock_l2tlb_base_sequence");
    super.new(name);
    reset_pbmt_rf_audit_ledger();
    pbmt_rf_readiness_wait_samples = 0;
    pbmt_rf_readiness_wait_phase = memblock_sync_pkg::PBMT_RF_RESET;
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
    void'(check_l2tlb_lifecycle_accounting("owner_release"));
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
    // mailbox, but it must never create or advance later-split ledger state.
    // All transport epoch/reset gates therefore precede the observer.
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
        return;
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
        if (sample.sampled_req_ready === 1'b1 ||
            sample.sampled_req_fire || sample.sampled_resp_valid === 1'b1) begin
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
            sample.sampled_req_ready === 1'b1 ||
            sample.sampled_req_fire ||
            sample.sampled_resp_valid === 1'b1 ||
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
        if (sample.sampled_req_ready === 1'b1 ||
            sample.sampled_req_fire || sample.sampled_resp_valid === 1'b1) begin
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
    service_pbmt_rf_readiness(sample_seq, request_csr_raw);

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

    // Only mutate split provenance after the sample anchor, producer
    // watermarks, CSR history, and same-sample lifecycle events are settled.
    if (data.split_observed_sample_seq == 0 ||
        data.split_observed_reset_epoch != sample.sampled_reset_epoch) begin
        if (acceptance_opened_since_reset || sample.sampled_split_load_req_fire ||
            sample.sampled_lu1_split_data_valid || sample.sampled_split_load_resp_valid) begin
            `uvm_fatal("FIRST_SPLIT_ACCESS", "first observation baseline is not startup/reset silent")
            return;
        end
    end
    data.observe_first_split_request_sample(
        sample.sampled_split_load_req_fire &&
            !sample.sampled_split_load_req_misalign_need_wakeup,
        '{flag: sample.sampled_split_load_req_rob_flag,
          value: sample.sampled_split_load_req_rob_value},
        sample.sampled_split_load_req_fullva, sample_seq, sample.sampled_reset_epoch);
    data.observe_first_split_response_sample(
        sample.sampled_split_load_resp_valid &&
            !sample.sampled_split_load_resp_misalign_need_wakeup,
        '{flag: sample.sampled_split_load_resp_rob_flag,
          value: sample.sampled_split_load_resp_rob_value},
        sample.sampled_split_load_resp_vaddr,
        sample.sampled_split_load_resp_nc, sample.sampled_split_load_resp_mmio,
        sample.sampled_split_load_resp_need_replay,
        sample_seq, sample.sampled_reset_epoch);
    data.observe_first_split_data_sample(
        sample.sampled_lu1_split_data_valid === 1'b1,
        '{flag: sample.sampled_lu1_split_data_rob_flag,
          value: sample.sampled_lu1_split_data_rob_value},
        sample.sampled_lu1_split_data_vaddr,
        sample.sampled_lu1_split_data_cache_eligible === 1'b1,
        sample_seq, sample.sampled_reset_epoch);
    data.observe_later_split_tlb_sample(
        sample.sampled_split_load_req_fire &&
            !sample.sampled_split_load_req_misalign_need_wakeup,
        '{flag: sample.sampled_split_load_req_rob_flag,
          value: sample.sampled_split_load_req_rob_value},
        sample.sampled_split_load_req_fullva,
        sample.sampled_lu1_tlb_req_valid,
        sample.sampled_lu1_tlb_req_frm_mabuf,
        sample.sampled_lu1_tlb_req_kill,
        '{flag: sample.sampled_lu1_tlb_req_rob_flag,
          value: sample.sampled_lu1_tlb_req_rob_value},
        sample.sampled_lu1_tlb_req_fullva,
        sample.sampled_lu1_ptw_req_valid,
        sample.sampled_lu1_ptw_req_vpn,
        sample.sampled_lu1_ptw_req_s2xlate,
        sample_seq);
    data.observe_later_split_load_response_sample(
        sample.sampled_split_load_resp_valid &&
            !sample.sampled_split_load_resp_misalign_need_wakeup,
        '{flag: sample.sampled_split_load_resp_rob_flag,
          value: sample.sampled_split_load_resp_rob_value},
        sample.sampled_split_load_resp_vaddr,
        sample.sampled_split_load_resp_nc,
        sample.sampled_split_load_resp_mmio,
        sample.sampled_split_load_resp_need_replay,
        sample_seq);

    // 中文注释：只在完整消费后推进连续边界；早退造成的缺口不得被后拍跨越。
    if (data.split_observed_sample_seq == 0 ||
        data.split_observed_reset_epoch != sample.sampled_reset_epoch ||
        sample_seq == data.split_observed_sample_seq + 1) begin
        data.split_observed_reset_epoch = sample.sampled_reset_epoch;
        data.split_observed_sample_seq = sample_seq;
    end

    if (driving_valid && sampled_resp_valid) begin
        if (due_barrier_this_sample) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("L2TLB response would fire on filter due sample=%0d",
                                 sample_seq))
            return;
        end
        complete_driving_response();
        has_progress = 1'b1;
    end
    else if (driving_valid && !sampled_resp_valid) begin
        if (due_barrier_this_sample) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("L2TLB response remains driving at filter due sample=%0d",
                                 sample_seq))
            return;
        end
        // Keep the selected response visible until the frozen transport sample
        // proves that the DUT observed its valid pulse.
        cycle_tr = driving_req.resp_tr;
    end
    else if (!driving_valid && sampled_resp_valid) begin
        `uvm_fatal(get_type_name(),
                   "L2TLB response valid observed without a driving response token")
        return;
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
    ppn_reuse_en = seq_csr_common::get_l2tlb_ppn_reuse_en();
    ppn_reuse_history_size = seq_csr_common::get_l2tlb_ppn_reuse_history_size();
    ppn_reuse_wt = seq_csr_common::get_l2tlb_ppn_reuse_wt();
    // The parameters have already been snapshotted by seq_csr_common::init;
    // repeat the pure validation before this owner can make ready visible.
    seq_csr_common::check_l2tlb_payload_weight_cfg();
    seq_csr_common::check_l2tlb_ppn_reuse_cfg();
    `uvm_info(get_type_name(),
              $sformatf("L2TLB PPN reuse config enable=%0d history_size=%0d weight=%0d",
                        ppn_reuse_en, ppn_reuse_history_size, ppn_reuse_wt),
              UVM_LOW)
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

function bit memblock_l2tlb_base_sequence::get_csr_snapshot_for_sample(
    input longint unsigned target_sample_seq,
    output mmu_csr_runtime_state snapshot);
    memblock_sync_pkg::dispatch_raw_csr_t raw_csr;

    snapshot = null;
    if (!memblock_sync_pkg::get_l2tlb_request_csr_history(target_sample_seq,
                                                          raw_csr)) begin
        return 1'b0;
    end
    snapshot = mmu_csr_runtime_state::type_id::create(
        $sformatf("l2tlb_csr_sample_%0d", target_sample_seq));
    if (snapshot == null) begin
        `uvm_fatal(get_type_name(), "failed to allocate request CSR snapshot")
        return 1'b0;
    end
    snapshot.reset();
    snapshot.update_from_raw_csr(raw_csr);
    return 1'b1;
endfunction:get_csr_snapshot_for_sample

function bit memblock_l2tlb_base_sequence::check_l2tlb_lifecycle_accounting(input string audit_context);
    longint unsigned accounted_count;

    accounted_count = completed_count + flush_canceled_count +
                      reset_canceled_count + outstanding_count();
    if (accepted_count != accounted_count) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("L2TLB lifecycle mismatch context=%s accepted=%0d completed=%0d flush_canceled=%0d reset_canceled=%0d pending=%0d driving=%0d accounted=%0d",
                             audit_context, accepted_count, completed_count,
                             flush_canceled_count, reset_canceled_count,
                             pending_q.size(), driving_valid, accounted_count))
        return 1'b0;
    end
    if (audit_context == "owner_release" &&
        (barrier_q.size() != 0 ||
         (data != null && data.has_waiting_uid_tlb_record()))) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("L2TLB lifecycle release is not quiescent barrier_count=%0d waiting_uid=%0d",
                             barrier_q.size(),
                             (data != null) ? data.has_waiting_uid_tlb_record() : 1'b0))
        return 1'b0;
    end
    return 1'b1;
endfunction:check_l2tlb_lifecycle_accounting

function void memblock_l2tlb_base_sequence::reset_pbmt_rf_audit_ledger();
    for (int idx = 0; idx < 2; idx++) begin
        pbmt_rf_audit[idx] = '{default:'0};
        pbmt_rf_audit[idx].slot = memblock_pbmt_rf_slot_kind_e'(idx);
        pbmt_rf_audit[idx].cancel_reason = "";
    end
    pbmt_rf_audit_final_read = 1'b0;
endfunction:reset_pbmt_rf_audit_ledger

function bit memblock_l2tlb_base_sequence::get_pbmt_rf_audit_slot(
    input memblock_pbmt_rf_slot_kind_e slot,
    output memblock_pbmt_rf_audit_slot_t slot_copy);
    slot_copy = pbmt_rf_audit[int'(slot)];
    pbmt_rf_audit_final_read = 1'b1;
    return slot_copy.valid;
endfunction:get_pbmt_rf_audit_slot

function void memblock_l2tlb_base_sequence::cleanup_pbmt_rf_audit_ledger_after_final_read();
    if (memblock_sync_pkg::get_pbmt_rf_phase() != memblock_sync_pkg::PBMT_RF_AUDIT_PASSED ||
        !pbmt_rf_audit_final_read) begin
        `uvm_fatal(get_type_name(), "PBMT RF ledger cleanup before successful final read")
        return;
    end
    reset_pbmt_rf_audit_ledger();
endfunction:cleanup_pbmt_rf_audit_ledger_after_final_read

function longint unsigned memblock_l2tlb_base_sequence::get_l2tlb_accepted_count();
    return accepted_count;
endfunction:get_l2tlb_accepted_count

function longint unsigned memblock_l2tlb_base_sequence::get_l2tlb_completed_count();
    return completed_count;
endfunction:get_l2tlb_completed_count

function longint unsigned memblock_l2tlb_base_sequence::get_l2tlb_canceled_count();
    return flush_canceled_count + reset_canceled_count;
endfunction:get_l2tlb_canceled_count

function void memblock_l2tlb_base_sequence::service_pbmt_rf_readiness(
    input longint unsigned current_sample_seq,
    input memblock_sync_pkg::dispatch_raw_csr_t request_csr_raw);
    longint unsigned pmpaddr0_sample;
    longint unsigned pmpcfg0_sample;
    longint unsigned applied_sample;
    int unsigned latest_runtime_csr_seq;
    bit bootstrap_matches;
    bit pmp_observations_ready;
    memblock_sync_pkg::dispatch_raw_csr_t latest_runtime_csr;
    memblock_sync_pkg::memblock_pbmt_rf_phase_e phase;

    if (!memblock_sync_pkg::is_pbmt_rf_scenario_active()) begin
        return;
    end
    phase = memblock_sync_pkg::get_pbmt_rf_phase();
    if (phase != pbmt_rf_readiness_wait_phase) begin
        pbmt_rf_readiness_wait_phase = phase;
        pbmt_rf_readiness_wait_samples = 0;
        `uvm_info("PBMT_RF_READY",
                  $sformatf("enter readiness phase=%0d sample=%0d history_published=%0d",
                            phase, current_sample_seq,
                            memblock_sync_pkg::csr_history_published_seq),
                  UVM_LOW)
    end
    if (phase == memblock_sync_pkg::PBMT_RF_ANCHOR_PUBLISHED) begin
        if (!data.service_csr_pmp_pma_write_observations()) return;
        applied_sample = data.get_pma_pmp_last_applied_csr_sample();
        pmp_observations_ready =
            memblock_sync_pkg::get_pbmt_rf_pmp_write_observations(
                pmpaddr0_sample, pmpcfg0_sample);
        bootstrap_matches = data.pbmt_rf_bootstrap_matches();
        void'(memblock_sync_pkg::get_latest_runtime_csr_snapshot(
            latest_runtime_csr, latest_runtime_csr_seq));
        if (pmp_observations_ready &&
            applied_sample >= pmpcfg0_sample &&
            bootstrap_matches &&
            request_csr_raw.valid && request_csr_raw.satp_mode == 4'd8 &&
            request_csr_raw.priv_dmode == 2'd0 && !request_csr_raw.priv_virt &&
            !request_csr_raw.m_pbmt_en && !request_csr_raw.h_pbmt_en) begin
            if (!memblock_sync_pkg::publish_pbmt_rf_b_runtime_ready(
                    current_sample_seq, this)) return;
            pbmt_rf_readiness_wait_samples = 0;
        end else begin
            pbmt_rf_readiness_wait_samples++;
            if (pbmt_rf_readiness_wait_samples == 1 ||
                (pbmt_rf_readiness_wait_samples % 1000) == 0) begin
                `uvm_info("PBMT_RF_READY",
                          $sformatf("B wait=%0d sample=%0d observed=%0d addr_sample=%0d cfg_sample=%0d applied=%0d bootstrap=%0d request(valid/mode/dmode/virt/m/h)=%0d/%0h/%0d/%0d/%0d/%0d runtime_seq=%0d history_published=%0d",
                                    pbmt_rf_readiness_wait_samples,
                                    current_sample_seq,
                                    pmp_observations_ready,
                                    pmpaddr0_sample, pmpcfg0_sample,
                                    applied_sample, bootstrap_matches,
                                    request_csr_raw.valid,
                                    request_csr_raw.satp_mode,
                                    request_csr_raw.priv_dmode,
                                    request_csr_raw.priv_virt,
                                    request_csr_raw.m_pbmt_en,
                                    request_csr_raw.h_pbmt_en,
                                    latest_runtime_csr_seq,
                                    memblock_sync_pkg::csr_history_published_seq),
                          UVM_LOW)
            end
            if (pbmt_rf_readiness_wait_samples >
                seq_csr_common::get_active_seq_no_progress_warn_cycles()) begin
                `uvm_fatal("PBMT_RF_READY", "B_RUNTIME_READY watchdog expired")
                return;
            end
        end
    end
    else if (phase == memblock_sync_pkg::PBMT_RF_C_ENABLE_MONITORED &&
             request_csr_raw.valid && request_csr_raw.satp_mode == 4'd8 &&
             request_csr_raw.priv_dmode == 2'd0 && !request_csr_raw.priv_virt &&
             request_csr_raw.m_pbmt_en && !request_csr_raw.h_pbmt_en) begin
        if (!memblock_sync_pkg::publish_pbmt_rf_c_c2_ready(
                current_sample_seq, this)) return;
        pbmt_rf_readiness_wait_samples = 0;
    end
    else if (phase == memblock_sync_pkg::PBMT_RF_C_ENABLE_MONITORED) begin
        pbmt_rf_readiness_wait_samples++;
        if (pbmt_rf_readiness_wait_samples == 1 ||
            (pbmt_rf_readiness_wait_samples % 1000) == 0) begin
            `uvm_info("PBMT_RF_READY",
                      $sformatf("C wait=%0d sample=%0d request(valid/mode/dmode/virt/m/h)=%0d/%0h/%0d/%0d/%0d/%0d history_published=%0d",
                                pbmt_rf_readiness_wait_samples,
                                current_sample_seq,
                                request_csr_raw.valid,
                                request_csr_raw.satp_mode,
                                request_csr_raw.priv_dmode,
                                request_csr_raw.priv_virt,
                                request_csr_raw.m_pbmt_en,
                                request_csr_raw.h_pbmt_en,
                                memblock_sync_pkg::csr_history_published_seq),
                      UVM_LOW)
        end
        if (pbmt_rf_readiness_wait_samples >
            seq_csr_common::get_active_seq_no_progress_warn_cycles()) begin
            `uvm_fatal("PBMT_RF_READY", "C_C2_READY_ACK watchdog expired")
            return;
        end
    end
endfunction:service_pbmt_rf_readiness

function void memblock_l2tlb_base_sequence::mark_pbmt_rf_audit_canceled(
    input longint unsigned request_token,
    input string reason);
    if (!memblock_sync_pkg::is_pbmt_rf_scenario_active()) begin
        return;
    end
    for (int idx = 0; idx < 2; idx++) begin
        if (pbmt_rf_audit[idx].valid &&
            pbmt_rf_audit[idx].request_token == request_token) begin
            if (pbmt_rf_audit[idx].complete_valid || pbmt_rf_audit[idx].canceled) begin
                `uvm_fatal(get_type_name(), "PBMT RF token canceled after terminal ledger state")
                return;
            end
            pbmt_rf_audit[idx].canceled = 1'b1;
            pbmt_rf_audit[idx].cancel_reason = reason;
            return;
        end
    end
    `uvm_fatal(get_type_name(),
               $sformatf("PBMT RF cancellation has no ledger token=%0d reason=%s",
                         request_token, reason))
endfunction:mark_pbmt_rf_audit_canceled

function bit memblock_l2tlb_base_sequence::capture_pbmt_rf_complete(
    input memblock_l2tlb_pending_req pending,
    input mmu_csr_runtime_state actual_response_csr_snapshot);
    int match_index;
    int match_count;
    memblock_sync_pkg::memblock_pbmt_rf_phase_e expected_phase;
    memblock_sync_pkg::memblock_pbmt_rf_phase_e completed_phase;

    if (!memblock_sync_pkg::is_pbmt_rf_scenario_active()) begin
        return 1'b1;
    end
    if (pending == null || actual_response_csr_snapshot == null) begin
        return 1'b0;
    end
    match_index = -1;
    match_count = 0;
    for (int idx = 0; idx < 2; idx++) begin
        if (pbmt_rf_audit[idx].valid &&
            pbmt_rf_audit[idx].request_token == pending.request_token) begin
            match_index = idx;
            match_count++;
        end
    end
    if (match_count != 1 || pbmt_rf_audit[match_index].canceled ||
        pbmt_rf_audit[match_index].complete_valid ||
        pending.response_visible_sample_seq != sample_seq) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("PBMT RF completion ledger mismatch token=%0d matches=%0d sample=%0d visible=%0d",
                             pending.request_token, match_count, sample_seq,
                             pending.response_visible_sample_seq))
        return 1'b0;
    end
    expected_phase = match_index == int'(PBMT_RF_SLOT_B) ?
                         memblock_sync_pkg::PBMT_RF_B_ACCEPTED :
                         memblock_sync_pkg::PBMT_RF_C_ACCEPTED;
    completed_phase = match_index == int'(PBMT_RF_SLOT_B) ?
                          memblock_sync_pkg::PBMT_RF_B_COMPLETED :
                          memblock_sync_pkg::PBMT_RF_C_COMPLETED;
    if (!memblock_sync_pkg::can_publish_pbmt_rf_phase(
            expected_phase, completed_phase, this)) begin
        `uvm_fatal(get_type_name(), "PBMT RF completion phase/owner precheck failed")
        return 1'b0;
    end
    // Completion commit is one non-failing mutation block after all checks.
    pbmt_rf_audit[match_index].complete_valid = 1'b1;
    pbmt_rf_audit[match_index].complete_dut_sample_seq = sample_seq;
    pbmt_rf_audit[match_index].complete_csr_history_sample_seq = sample_seq;
    pbmt_rf_audit[match_index].complete_m_pbmt_en = actual_response_csr_snapshot.m_pbmt_en;
    pbmt_rf_audit[match_index].complete_h_pbmt_en = actual_response_csr_snapshot.h_pbmt_en;
    pbmt_rf_audit[match_index].raw_s1_pf = pending.entry_snapshot.fault_raw_s1_pf;
    pbmt_rf_audit[match_index].raw_s1_af = pending.entry_snapshot.fault_raw_s1_af;
    pbmt_rf_audit[match_index].raw_s2_gpf = pending.entry_snapshot.fault_raw_s2_gpf;
    pbmt_rf_audit[match_index].raw_s2_gaf = pending.entry_snapshot.fault_raw_s2_gaf;
    pbmt_rf_audit[match_index].effective_s1_pf = pending.entry_snapshot.fault_effective_s1_pf;
    pbmt_rf_audit[match_index].effective_s1_af = pending.entry_snapshot.fault_effective_s1_af;
    pbmt_rf_audit[match_index].effective_s2_gpf = pending.entry_snapshot.fault_effective_s2_gpf;
    pbmt_rf_audit[match_index].effective_s2_gaf = pending.entry_snapshot.fault_effective_s2_gaf;
    pbmt_rf_audit[match_index].pma_af = pending.entry_snapshot.pmaAF;
    pbmt_rf_audit[match_index].forced_s1_pf = pending.response_pbmt_forced_s1_pf;
    pbmt_rf_audit[match_index].forced_s2_gpf = pending.response_pbmt_forced_s2_gpf;
    memblock_sync_pkg::commit_pbmt_rf_phase_prechecked(
        expected_phase, completed_phase, this);
    return 1'b1;
endfunction:capture_pbmt_rf_complete

function void memblock_l2tlb_base_sequence::cancel_outstanding_by_reset();
    int unsigned canceled_count;

    canceled_count = outstanding_count();
    foreach (pending_q[idx]) begin
        mark_pbmt_rf_audit_canceled(pending_q[idx].request_token,
                                    "l2tlb_runtime_reset");
    end
    if (driving_valid && driving_req != null) begin
        mark_pbmt_rf_audit_canceled(driving_req.request_token,
                                    "l2tlb_runtime_reset");
    end
    reset_canceled_count += canceled_count;
    void'(data.cancel_waiting_uid_tlb_records("l2tlb_runtime_reset"));
    data.cancel_all_later_split_tlb_records("l2tlb_runtime_reset");
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
    void'(check_l2tlb_lifecycle_accounting("reset_cancel"));
endfunction:cancel_outstanding_by_reset

// Abstract responsibility: validate the complete PBMT request-fire commit set
// without changing UID, PMA/PMP, ledger, queue, token, counters, or phase.  The
// caller may safely catch the fatal and compare an exact before/after snapshot.
function bit memblock_l2tlb_base_sequence::precheck_pbmt_rf_request_fire_commit(
    input memblock_l2tlb_pending_req pending,
    input memblock_uid_tlb_record resolved_record,
    input pma_pmp_uid_context_plan_t context_plan,
    input memblock_pbmt_rf_audit_slot_t prepared_slot,
    input memblock_sync_pkg::memblock_pbmt_rf_phase_e expected_phase,
    input memblock_sync_pkg::memblock_pbmt_rf_phase_e accepted_phase);
    int slot_index;

    slot_index = int'(prepared_slot.slot);
    if (pending == null || resolved_record == null || !context_plan.valid ||
        !prepared_slot.valid || slot_index < 0 || slot_index >= 2 ||
        pending.request_token != next_request_token ||
        prepared_slot.request_token != pending.request_token ||
        prepared_slot.request_key != pending.request_lookup_key ||
        prepared_slot.request_vpn != pending.vpn ||
        prepared_slot.s2xlate != pending.s2xlate ||
        resolved_record.uid != prepared_slot.uid ||
        !resolved_record.is_waiting() ||
        resolved_record.uid_tlb_first_request_fire_sample_seq != 0 ||
        context_plan.context.uid != prepared_slot.uid ||
        pending_q.size() + (driving_valid ? 1 : 0) >= max_outstanding ||
        accepted_count != completed_count + flush_canceled_count +
                          reset_canceled_count + outstanding_count() ||
        pbmt_rf_audit[slot_index].valid ||
        !memblock_sync_pkg::can_publish_pbmt_rf_phase(
            expected_phase, accepted_phase, this)) begin
        `uvm_fatal("PBMT_RF_COMMIT",
                   $sformatf("request-fire commit precheck failed pending=%0d record=%0d plan=%0d slot_valid=%0d slot=%0d token=%0d next=%0d pending_q=%0d driving=%0d max=%0d accepted=%0d completed=%0d",
                             pending != null, resolved_record != null,
                             context_plan.valid, prepared_slot.valid, slot_index,
                             pending == null ? 0 : pending.request_token,
                             next_request_token, pending_q.size(), driving_valid,
                             max_outstanding, accepted_count, completed_count))
        return 1'b0;
    end
    for (int idx = 0; idx < 2; idx++) begin
        if (idx != slot_index && pbmt_rf_audit[idx].valid &&
            (pbmt_rf_audit[idx].uid == prepared_slot.uid ||
             pbmt_rf_audit[idx].request_token == prepared_slot.request_token)) begin
            `uvm_fatal("PBMT_RF_COMMIT",
                       $sformatf("request-fire commit duplicate provenance slot=%0d other=%0d uid=%0d token=%0d",
                                 slot_index, idx, prepared_slot.uid,
                                 prepared_slot.request_token))
            return 1'b0;
        end
    end
    return 1'b1;
endfunction:precheck_pbmt_rf_request_fire_commit

// Abstract responsibility: perform the already-proven PBMT request-fire state
// transition as one mutation-only block.  No allocation, lookup, report, or
// fatal-capable helper is allowed below this point.
function void memblock_l2tlb_base_sequence::commit_pbmt_rf_request_fire_prechecked(
    input memblock_l2tlb_pending_req pending,
    input memblock_uid_tlb_record resolved_record,
    input pma_pmp_uid_context_plan_t context_plan,
    input memblock_pbmt_rf_audit_slot_t prepared_slot,
    input memblock_sync_pkg::memblock_pbmt_rf_phase_e expected_phase,
    input memblock_sync_pkg::memblock_pbmt_rf_phase_e accepted_phase);
    data.commit_pbmt_rf_request_fire_prechecked(
        resolved_record, sample_seq, context_plan,
        prepared_slot, pbmt_rf_audit);
    pending_q.push_back(pending);
    next_request_token++;
    accepted_count++;
    memblock_sync_pkg::commit_pbmt_rf_phase_prechecked(
        expected_phase, accepted_phase, this);
endfunction:commit_pbmt_rf_request_fire_prechecked

function memblock_l2tlb_pending_req memblock_l2tlb_base_sequence::capture_fired_request();
    memblock_l2tlb_pending_req pending;
    memblock_tlb_entry live_entry;
    bit created;
    bit scenario_active;
    memblock_uid_t resolved_uid;
    memblock_uid_tlb_record resolved_record;
    mmu_csr_runtime_state strict_request_csr;
    pma_pmp_uid_context_plan_t context_plan;
    memblock_pbmt_rf_audit_slot_t prepared_slot;
    memblock_pbmt_rf_slot_kind_e expected_slot;
    memblock_sync_pkg::memblock_pbmt_rf_phase_e expected_phase;
    memblock_sync_pkg::memblock_pbmt_rf_phase_e accepted_phase;
    int unsigned legacy_marked_count;

    scenario_active = memblock_sync_pkg::is_pbmt_rf_scenario_active();
    if (release_close_requested) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("L2TLB request fired after local admission cutoff sample=%0d vpn=0x%0h s2xlate=%0d",
                             sample_seq, sampled_req_vpn, sampled_req_s2xlate))
        return null;
    end
    if (memblock_sync_pkg::l2tlb_release_admission_close_requested ||
        memblock_sync_pkg::l2tlb_release_admission_closed ||
        memblock_sync_pkg::l2tlb_release_closing) begin
        `uvm_fatal(get_type_name(), "L2TLB request fired after shared admission seal")
        return null;
    end
    if (outstanding_count() >= max_outstanding) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("L2TLB request fired at full queue: outstanding=%0d max=%0d",
                             outstanding_count(), max_outstanding))
        return null;
    end

    // Planning phase: allocate and derive every object before any UID, context,
    // ledger, phase, token, queue, or lifecycle counter mutation.
    pending = memblock_l2tlb_pending_req::type_id::create(
        $sformatf("l2tlb_pending_%0d", next_request_token));
    if (pending == null) begin
        `uvm_fatal(get_type_name(), "failed to create L2TLB pending record")
        return null;
    end
    pending.request_token = next_request_token;
    pending.vpn = sampled_req_vpn;
    pending.s2xlate = sampled_req_s2xlate;
    if (!get_csr_snapshot_for_sample(sample_seq, pending.csr_snapshot) ||
        pending.csr_snapshot == null) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("missing V2 C-2 CSR history for request sample=%0d",
                             sample_seq))
        return null;
    end
    pending.request_lookup_key = pending.csr_snapshot.make_lookup_key(
        {26'b0, pending.vpn}, pending.s2xlate);
    created = 1'b0;
    if (scenario_active) begin
        if (!data.find_existing_l2tlb_entry_by_req_with_snapshot(
                pending.vpn, pending.s2xlate, pending.csr_snapshot,
                pending.request_lookup_key, pending.entry_anchor_key,
                pending.lookup_result, live_entry) || live_entry == null) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("PBMT RF request did not hit canonical entry vpn=0x%0h s2xlate=%0d",
                                 pending.vpn, pending.s2xlate))
            return null;
        end
    end else if (!data.get_or_create_l2tlb_entry_by_req_with_snapshot(
                     pending.vpn, pending.s2xlate, pending.csr_snapshot,
                     ppn_reuse_en, ppn_reuse_wt,
                     pending.request_lookup_key, pending.entry_anchor_key,
                     pending.lookup_result, live_entry, created, sample_seq) ||
                 live_entry == null) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("failed to get/create L2TLB entry vpn=0x%0h s2xlate=%0d",
                             pending.vpn, pending.s2xlate))
        return null;
    end
    if (pending.request_lookup_key !=
        pending.csr_snapshot.make_lookup_key({26'b0, pending.vpn},
                                              pending.s2xlate) ||
        live_entry.lookup_key != pending.entry_anchor_key ||
        live_entry.entry_generation == 0) begin
        `uvm_fatal(get_type_name(), "L2TLB request planning canonical/key mismatch")
        return null;
    end
    pending.entry_snapshot = memblock_tlb_entry::type_id::create(
        $sformatf("l2tlb_entry_snapshot_%0d", pending.request_token));
    if (pending.entry_snapshot == null) begin
        `uvm_fatal(get_type_name(), "failed to create request-time L2TLB entry snapshot")
        return null;
    end
    pending.entry_snapshot.copy_from(live_entry);
    pending.pending_entry_generation = pending.entry_snapshot.entry_generation;
    if (pending.pending_entry_generation == 0 ||
        pending.pending_entry_generation != live_entry.entry_generation) begin
        `uvm_fatal(get_type_name(), "L2TLB pending entry generation planning mismatch")
        return null;
    end
    pending.later_split_owner = data.bind_later_split_l2_request_token(
        pending.request_lookup_key, pending.request_token, sample_seq);
    // Derived addresses belong to this request, not to the first lookup that
    // created the live entry.  In particular, a superpage/NAPOT response may
    // raw-hit a different waiting VPN; derive it from that request's frozen
    // VPN/GVPN and the request-visible CSR snapshot.
    data.derive_tlb_request_fields(
        pending.entry_snapshot, {14'b0, pending.vpn}, pending.s2xlate,
        pending.csr_snapshot, pending.request_derived_valid,
        pending.request_s1_resolved_ppn, pending.request_s2_resolved_ppn,
        pending.request_gvpn);
    pending.resp_tr = create_l2tlb_xaction(
        $sformatf("l2tlb_resp_token_%0d", pending.request_token));
    if (pending.resp_tr == null) return null;
    pending.resp_tr.io_ptw_req_0_valid = 1'b1;
    pending.resp_tr.io_ptw_req_0_bits_vpn = pending.vpn;
    pending.resp_tr.io_ptw_req_0_bits_s2xlate = pending.s2xlate;
    if (scenario_active) begin
        pending.latency_bucket = L2TLB_LATENCY_1C;
        pending.min_latency = 1;
    end else begin
        pending.min_latency = choose_latency(pending.latency_bucket);
    end
    pending.accept_sample_seq = sample_seq;
    pending.due_sample_seq = sample_seq + pending.min_latency;
    pending.accept_flush_event_seq = fire_visible_event_seq;

    if (scenario_active) begin
        prepared_slot = '{default:'0};
        expected_slot = memblock_sync_pkg::get_pbmt_rf_phase() ==
                            memblock_sync_pkg::PBMT_RF_B_RUNTIME_READY ?
                            PBMT_RF_SLOT_B : PBMT_RF_SLOT_C;
        expected_phase = expected_slot == PBMT_RF_SLOT_B ?
                             memblock_sync_pkg::PBMT_RF_B_RUNTIME_READY :
                             memblock_sync_pkg::PBMT_RF_C_C2_READY_ACK;
        accepted_phase = expected_slot == PBMT_RF_SLOT_B ?
                             memblock_sync_pkg::PBMT_RF_B_ACCEPTED :
                             memblock_sync_pkg::PBMT_RF_C_ACCEPTED;
        prepared_slot.valid = 1'b1;
        prepared_slot.slot = expected_slot;
        prepared_slot.uid = expected_slot == PBMT_RF_SLOT_B ? 0 : 1;
        prepared_slot.requestor = expected_slot == PBMT_RF_SLOT_B ?
                                      PBMT_RF_REQ_STORE : PBMT_RF_REQ_LOAD;
        prepared_slot.request_token = pending.request_token;
        prepared_slot.request_vpn = pending.vpn;
        prepared_slot.s2xlate = pending.s2xlate;
        prepared_slot.lookup_kind = pending.lookup_result;
        prepared_slot.request_key = pending.request_lookup_key;
        prepared_slot.anchor_key = pending.entry_anchor_key;
        prepared_slot.entry_generation = live_entry.entry_generation;
        prepared_slot.accept_dut_sample_seq = sample_seq;
        prepared_slot.accept_csr_history_sample_seq = sample_seq;
        prepared_slot.accept_m_pbmt_en = pending.csr_snapshot.m_pbmt_en;
        prepared_slot.accept_h_pbmt_en = pending.csr_snapshot.h_pbmt_en;
        prepared_slot.cancel_reason = "";
        if (!data.resolve_and_plan_pbmt_rf_request_fire(
                pending.request_lookup_key, sample_seq, prepared_slot,
                pbmt_rf_audit, resolved_uid, strict_request_csr,
                resolved_record, context_plan) ||
            strict_request_csr == null || resolved_record == null ||
            strict_request_csr.make_lookup_key({26'b0, pending.vpn},
                                                pending.s2xlate) !=
                pending.request_lookup_key ||
            !precheck_pbmt_rf_request_fire_commit(
                pending, resolved_record, context_plan, prepared_slot,
                expected_phase, accepted_phase)) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("PBMT RF strict request-fire planning failed token=%0d sample=%0d vpn=0x%0h",
                                 pending.request_token, sample_seq,
                                 pending.request_lookup_key.vpn))
            return null;
        end

        // Single mutation commit: every operation below is prechecked and has
        // no fatal-capable helper call or allocation.
        commit_pbmt_rf_request_fire_prechecked(
            pending, resolved_record, context_plan, prepared_slot,
            expected_phase, accepted_phase);
    end else begin
        if (!data.mark_uid_tlb_record_request_fire(
                pending.request_lookup_key, sample_seq,
                legacy_marked_count)) begin
            return null;
        end
        next_request_token++;
        pending_q.push_back(pending);
        accepted_count++;
    end

    `uvm_info(get_type_name(),
              $sformatf("accept L2TLB token=%0d vpn=0x%0h s2xlate=%0d lookup=%0d anchor_vpn=0x%0h created=%0d due=%0d bucket=%0d outstanding=%0d",
                        pending.request_token, pending.vpn, pending.s2xlate,
                        pending.lookup_result, pending.entry_anchor_key.vpn,
                        created, pending.due_sample_seq,
                        pending.latency_bucket, outstanding_count()),
              UVM_LOW)
    void'(check_l2tlb_lifecycle_accounting("request_accept"));
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
                data.cancel_later_split_l2_token(
                    pending_q[idx].request_token,
                    $sformatf("l2tlb_flush_event_%0d", barrier.event_seq));
                mark_pbmt_rf_audit_canceled(
                    pending_q[idx].request_token,
                    $sformatf("l2tlb_flush_due_event_%0d", barrier.event_seq));
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
        void'(check_l2tlb_lifecycle_accounting("due_flush"));
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
    mmu_csr_runtime_state response_csr;
    bit force_s1_pf;
    bit force_s2_gpf;

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
    // The response payload is defined by the C-2 CSR visible at the sample in
    // which valid will be raised.  Keep the token pending if that history is
    // not published yet; selecting it earlier would make PBMT fault state
    // depend on a later/current CSR read.
    if (!get_csr_snapshot_for_sample(next_sample_seq, response_csr) ||
        response_csr == null) begin
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
    if (driving_req.response_payload_frozen) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("selected L2TLB token already has frozen response payload token=%0d",
                             driving_req.request_token))
    end
    apply_pbmt_response_overlay(driving_req.entry_snapshot, response_csr,
                                force_s1_pf, force_s2_gpf);
    driving_req.response_visible_sample_seq = next_sample_seq;
    driving_req.response_m_pbmt_en = response_csr.m_pbmt_en;
    driving_req.response_h_pbmt_en = response_csr.h_pbmt_en;
    driving_req.response_pbmt_forced_s1_pf = force_s1_pf;
    driving_req.response_pbmt_forced_s2_gpf = force_s2_gpf;
    driving_req.response_payload_frozen = 1'b1;
    fill_dtlb_resp_from_entry(driving_req.entry_snapshot, driving_req.resp_tr);
    cycle_tr = driving_req.resp_tr;
    if (cycle_tr == null) begin
        `uvm_fatal(get_type_name(), "selected L2TLB pending record has null response transaction")
    end
    void'(check_l2tlb_lifecycle_accounting("response_select"));
    return 1'b1;
endfunction:select_due_response

function void memblock_l2tlb_base_sequence::complete_driving_response();
    int unsigned record_update_count;
    longint unsigned complete_sample_seq;
    mmu_csr_runtime_state actual_response_csr_snapshot;

    if (!driving_valid || driving_req == null) begin
        `uvm_fatal(get_type_name(), "complete_driving_response got invalid driving slot")
        return;
    end
    complete_sample_seq = sample_seq;
    if (complete_sample_seq < driving_req.due_sample_seq) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("L2TLB response completed before due token=%0d accept=%0d due=%0d complete=%0d",
                             driving_req.request_token,
                             driving_req.accept_sample_seq,
                             driving_req.due_sample_seq,
                             complete_sample_seq))
        return;
    end
    if (!driving_req.response_payload_frozen) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("L2TLB response completed before payload freeze token=%0d",
                             driving_req.request_token))
        return;
    end
    if (driving_req.response_visible_sample_seq != complete_sample_seq) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("L2TLB response sample drift token=%0d selected=%0d complete=%0d",
                             driving_req.request_token,
                             driving_req.response_visible_sample_seq,
                             complete_sample_seq))
        return;
    end
    if (!get_csr_snapshot_for_sample(complete_sample_seq,
                                     actual_response_csr_snapshot) ||
        actual_response_csr_snapshot == null) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("missing response-visible C-2 CSR for L2TLB response sample=%0d",
                             complete_sample_seq))
        return;
    end
    if (actual_response_csr_snapshot.m_pbmt_en != driving_req.response_m_pbmt_en ||
        actual_response_csr_snapshot.h_pbmt_en != driving_req.response_h_pbmt_en) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("L2TLB response PBMTE drift token=%0d selected m/h=%0d/%0d actual=%0d/%0d sample=%0d",
                             driving_req.request_token,
                             driving_req.response_m_pbmt_en,
                             driving_req.response_h_pbmt_en,
                             actual_response_csr_snapshot.m_pbmt_en,
                             actual_response_csr_snapshot.h_pbmt_en,
                             complete_sample_seq))
        return;
    end
    if (!capture_pbmt_rf_complete(driving_req,
                                  actual_response_csr_snapshot)) begin
        return;
    end
    if (ppn_reuse_en) begin
        // 中文注释：只在 sampled_resp_valid 已确认的 completion 边界入队；
        // 使用 frozen token 的 request-specific PPN 和 effective entry，不能回读 live table。
        data.record_l2tlb_completed_ppn_history(
            driving_req.s2xlate,
            driving_req.request_derived_valid,
            driving_req.request_s1_resolved_ppn,
            driving_req.request_s2_resolved_ppn,
            driving_req.entry_snapshot,
            driving_req.request_token,
            complete_sample_seq,
            ppn_reuse_history_size);
    end
    // Publish only after the DUT-visible response has been sampled.  UID issue
    // can then prove an L1 hit without consulting a pending/live entry.
    data.record_completed_tlb_response(
        driving_req.entry_snapshot,
        driving_req.request_token,
        complete_sample_seq);
    if (driving_req.later_split_owner != null) begin
        data.complete_later_split_l2_response(
            driving_req.later_split_owner,
            driving_req.request_token,
            driving_req.request_lookup_key,
            driving_req.entry_anchor_key,
            driving_req.pending_entry_generation,
            driving_req.entry_snapshot,
            complete_sample_seq);
    end
    record_update_count = data.complete_waiting_uid_records_by_response(
        driving_req.entry_snapshot,
        actual_response_csr_snapshot);
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
    void'(check_l2tlb_lifecycle_accounting("response_complete"));
endfunction:complete_driving_response

// 中文注释：response select 前依据目标 sample 的 C-2 CSR，在 token 私有 entry
// snapshot 上一次性叠加 PBMT 导致的 S1 PF/S2 GPF。该函数不访问 live table、UID
// 记录或其他 pending token；调用者随后将同一 snapshot 同时交给 driver 和 UID 回填。
function void memblock_l2tlb_base_sequence::apply_pbmt_response_overlay(
    input memblock_tlb_entry entry,
    input mmu_csr_runtime_state response_csr,
    output bit force_s1_pf,
    output bit force_s2_gpf);
    force_s1_pf = 1'b0;
    force_s2_gpf = 1'b0;
    if (entry == null || response_csr == null) begin
        `uvm_fatal(get_type_name(), "apply_pbmt_response_overlay got null input")
    end
    entry.check_inactive_stage_defaults("RESPONSE_OVERLAY");
    if (entry.s1_stage_active && entry.s1_entry_pbmt == 2'b11) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("response S1 PBMT=11 is reserved token entry vpn=0x%0h",
                             entry.lookup_key.vpn))
    end
    if (entry.s2_stage_active && entry.s2_entry_pbmt == 2'b11) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("response S2 PBMT=11 is reserved token entry vpn=0x%0h",
                             entry.lookup_key.vpn))
    end
    mmu_csr_runtime_state::compute_pbmt_fault_overlay(
        entry.s1_stage_active, entry.s2_stage_active, entry.s2xlate,
        entry.s1_entry_pbmt, entry.s2_entry_pbmt,
        response_csr.m_pbmt_en, response_csr.h_pbmt_en,
        force_s1_pf, force_s2_gpf);
    // PBMT remains visible in the response.  Only the effective fault bits are
    // overlaid, and raw fault provenance is intentionally left untouched.
    entry.fault_effective_s1_pf |= force_s1_pf;
    entry.fault_effective_s2_gpf |= force_s2_gpf;
endfunction:apply_pbmt_response_overlay

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
