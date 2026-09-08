//=========================================================
//File name    : soft_test_l2tlb_ppn_reuse_sequence.sv
//Module name  : soft_test_l2tlb_ppn_reuse_sequence
//Discribution : software-only V2 L2TLB completed PPN reuse closure
//=========================================================
`ifndef SOFT_TEST_L2TLB_PPN_REUSE_SEQUENCE__SV
`define SOFT_TEST_L2TLB_PPN_REUSE_SEQUENCE__SV

class soft_test_l2tlb_ppn_reuse_sequence extends memblock_dispatch_base_sequence;

    localparam longint unsigned COMPLETION_SAMPLE = 3;

    memblock_l2tlb_base_sequence responder;
    tlb_map_builder               builder;

    `uvm_object_utils(soft_test_l2tlb_ppn_reuse_sequence)

    extern function new(string name = "soft_test_l2tlb_ppn_reuse_sequence");
    extern virtual task body();
    extern virtual function void expect_true(input bit condition, input string message);
    extern virtual function mmu_csr_runtime_state make_s1_csr_snapshot();
    extern virtual function mmu_csr_runtime_state make_s2_csr_snapshot();
    extern virtual function void publish_completion_csr_history(
        input mmu_csr_runtime_state csr_snapshot);
    extern virtual function memblock_tlb_entry lookup_entry(
        input bit [37:0] vpn,
        input bit [1:0] s2xlate,
        input mmu_csr_runtime_state csr_snapshot,
        input bit reuse_en,
        input int unsigned reuse_wt,
        output memblock_tlb_lookup_result_e lookup_result,
        output bit created);
    extern virtual task complete_frozen_response(
        input memblock_tlb_entry entry,
        input bit [37:0] vpn,
        input bit [1:0] s2xlate,
        input mmu_csr_runtime_state csr_snapshot,
        input longint unsigned token,
        input int unsigned history_size,
        input bit completion_visible,
        input bit reuse_en,
        output bit request_derived_valid,
        output bit [43:0] final_ppn);
    extern virtual function memblock_tlb_entry make_s1_superpage_entry(
        input memblock_tlb_lookup_key_t anchor_key,
        input mmu_csr_runtime_state csr_snapshot,
        input bit [43:0] canonical_ppn);
    extern virtual function void install_entry(
        input memblock_tlb_lookup_key_t anchor_key,
        input memblock_tlb_entry entry);
    extern virtual task check_disabled_and_weight_zero();
    extern virtual task check_completion_history_and_same_sample_reuse();
    extern virtual task check_fifo_invalid_and_reset_lifecycle();
    extern virtual task check_exact_range_and_target_encoding();
    extern virtual function void check_smoke_cfg_snapshot();

endclass:soft_test_l2tlb_ppn_reuse_sequence

function soft_test_l2tlb_ppn_reuse_sequence::new(
    string name = "soft_test_l2tlb_ppn_reuse_sequence");
    super.new(name);
    responder = null;
    builder = null;
endfunction:new

task soft_test_l2tlb_ppn_reuse_sequence::body();
    // 中文注释：此 software smoke 只使用公共 TLB/history API 和一个未启动的 responder 对象。
    // 不申请 lifecycle owner、不向 sequencer 发送 item，因此不会与真实 DUT responder 并发。
    ensure_dispatch_runtime_helpers();
    responder = memblock_l2tlb_base_sequence::type_id::create("ppn_reuse_completion_harness");
    builder = tlb_map_builder::type_id::create("ppn_reuse_builder");
    if (responder == null || builder == null) begin
        `uvm_fatal(get_type_name(), "failed to create PPN reuse software-test helpers")
    end
    responder.ensure_context();
    responder.configure_from_plus();
    check_smoke_cfg_snapshot();
    publish_completion_csr_history(make_s1_csr_snapshot());
    check_disabled_and_weight_zero();
    check_completion_history_and_same_sample_reuse();
    check_fifo_invalid_and_reset_lifecycle();
    check_exact_range_and_target_encoding();
    `uvm_info(get_type_name(),
              "V2 L2TLB completed PPN reuse software closure completed", UVM_LOW)
endtask:body

function void soft_test_l2tlb_ppn_reuse_sequence::expect_true(
    input bit condition,
    input string message);
    if (!condition) begin
        `uvm_fatal(get_type_name(), message)
    end
endfunction:expect_true

function void soft_test_l2tlb_ppn_reuse_sequence::check_smoke_cfg_snapshot();
    // 中文注释：专项 cfg 必须经过 plus -> seq_csr_common -> responder 配置快照；
    // 此处不替代后续各定向分支的局部 policy，只验证 testcase preset 的真实消费链。
    expect_true(seq_csr_common::get_l2tlb_ppn_reuse_en() == 1'b1 &&
                seq_csr_common::get_l2tlb_ppn_reuse_history_size() == 1 &&
                seq_csr_common::get_l2tlb_ppn_reuse_wt() == 100,
                "PPN reuse smoke cfg must reach seq_csr_common getters");
    expect_true(responder.ppn_reuse_en == 1'b1 &&
                responder.ppn_reuse_history_size == 1 &&
                responder.ppn_reuse_wt == 100,
                "PPN reuse smoke cfg must be frozen by responder.configure_from_plus");
endfunction:check_smoke_cfg_snapshot

function mmu_csr_runtime_state
    soft_test_l2tlb_ppn_reuse_sequence::make_s1_csr_snapshot();
    mmu_csr_runtime_state csr_snapshot;

    csr_snapshot = mmu_csr_runtime_state::type_id::create("ppn_reuse_s1_csr");
    if (csr_snapshot == null) begin
        `uvm_fatal(get_type_name(), "failed to create S1 CSR snapshot")
    end
    csr_snapshot.reset();
    csr_snapshot.satp_mode = 4'd8;
    csr_snapshot.satp_asid = 16'h0055;
    csr_snapshot.satp_ppn = 44'h0000_0001_234;
    csr_snapshot.update_seq = 1;
    return csr_snapshot;
endfunction:make_s1_csr_snapshot

function mmu_csr_runtime_state
    soft_test_l2tlb_ppn_reuse_sequence::make_s2_csr_snapshot();
    mmu_csr_runtime_state csr_snapshot;

    csr_snapshot = mmu_csr_runtime_state::type_id::create("ppn_reuse_s2_csr");
    if (csr_snapshot == null) begin
        `uvm_fatal(get_type_name(), "failed to create S2 CSR snapshot")
    end
    csr_snapshot.reset();
    csr_snapshot.hgatp_mode = 4'd8;
    csr_snapshot.hgatp_vmid = 16'h0007;
    csr_snapshot.hgatp_ppn = 44'h0000_0005_678;
    csr_snapshot.priv_virt = 1'b1;
    csr_snapshot.update_seq = 1;
    return csr_snapshot;
endfunction:make_s2_csr_snapshot

function void soft_test_l2tlb_ppn_reuse_sequence::publish_completion_csr_history(
    input mmu_csr_runtime_state csr_snapshot);
    memblock_sync_pkg::dispatch_raw_csr_t raw_csr;
    longint unsigned saved_dut_sample_seq;

    if (csr_snapshot == null) begin
        `uvm_fatal(get_type_name(), "completion CSR history needs a snapshot")
    end
    raw_csr = memblock_sync_pkg::make_empty_raw_csr();
    raw_csr.valid = 1'b1;
    raw_csr.satp_mode = csr_snapshot.satp_mode;
    raw_csr.satp_asid = csr_snapshot.satp_asid;
    raw_csr.satp_ppn = csr_snapshot.satp_ppn;
    raw_csr.vsatp_mode = csr_snapshot.vsatp_mode;
    raw_csr.vsatp_asid = csr_snapshot.vsatp_asid;
    raw_csr.vsatp_ppn = csr_snapshot.vsatp_ppn;
    raw_csr.hgatp_mode = csr_snapshot.hgatp_mode;
    raw_csr.hgatp_vmid = csr_snapshot.hgatp_vmid;
    raw_csr.hgatp_ppn = csr_snapshot.hgatp_ppn;
    raw_csr.priv_virt = csr_snapshot.priv_virt;
    raw_csr.m_pbmt_en = csr_snapshot.m_pbmt_en;
    raw_csr.h_pbmt_en = csr_snapshot.h_pbmt_en;
    // V2 responder 取 completion sample 的 C-2 CSR。为 COMPLETION_SAMPLE=3 发布 sample=1。
    saved_dut_sample_seq = memblock_sync_pkg::dut_sample_seq;
    memblock_sync_pkg::dut_sample_seq = 1;
    memblock_sync_pkg::publish_l2tlb_csr_history(raw_csr, 1);
    memblock_sync_pkg::dut_sample_seq = saved_dut_sample_seq;
endfunction:publish_completion_csr_history

function memblock_tlb_entry soft_test_l2tlb_ppn_reuse_sequence::lookup_entry(
    input bit [37:0] vpn,
    input bit [1:0] s2xlate,
    input mmu_csr_runtime_state csr_snapshot,
    input bit reuse_en,
    input int unsigned reuse_wt,
    output memblock_tlb_lookup_result_e lookup_result,
    output bit created);
    memblock_tlb_lookup_key_t request_key;
    memblock_tlb_lookup_key_t anchor_key;
    memblock_tlb_entry entry;

    if (!data.get_or_create_l2tlb_entry_by_req_with_snapshot(
            vpn, s2xlate, csr_snapshot, reuse_en, reuse_wt, request_key,
            anchor_key, lookup_result, entry, created) || entry == null) begin
        `uvm_fatal(get_type_name(), "PPN reuse lookup did not return an entry")
    end
    return entry;
endfunction:lookup_entry

task soft_test_l2tlb_ppn_reuse_sequence::complete_frozen_response(
    input memblock_tlb_entry entry,
    input bit [37:0] vpn,
    input bit [1:0] s2xlate,
    input mmu_csr_runtime_state csr_snapshot,
    input longint unsigned token,
    input int unsigned history_size,
    input bit completion_visible,
    input bit reuse_en,
    output bit request_derived_valid,
    output bit [43:0] final_ppn);
    memblock_l2tlb_pending_req pending;
    bit [43:0] request_s1_ppn;
    bit [43:0] request_s2_ppn;
    bit [51:0] request_gvpn;
    int unsigned history_size_before;
    int unsigned expected_history_size;

    if (entry == null || csr_snapshot == null) begin
        `uvm_fatal(get_type_name(), "completion harness got invalid frozen token input")
    end
    pending = memblock_l2tlb_pending_req::type_id::create(
        $sformatf("ppn_reuse_pending_%0d", token));
    if (pending == null) begin
        `uvm_fatal(get_type_name(), "failed to create PPN reuse pending token")
    end
    pending.entry_snapshot = memblock_tlb_entry::type_id::create(
        $sformatf("ppn_reuse_entry_snapshot_%0d", token));
    if (pending.entry_snapshot == null) begin
        `uvm_fatal(get_type_name(), "failed to create PPN reuse entry snapshot")
    end
    pending.entry_snapshot.copy_from(entry);
    data.derive_tlb_request_fields(pending.entry_snapshot, {14'b0, vpn}, s2xlate,
                                   csr_snapshot, request_derived_valid,
                                   request_s1_ppn, request_s2_ppn, request_gvpn);
    pending.request_token = token;
    pending.vpn = vpn;
    pending.s2xlate = s2xlate;
    pending.request_derived_valid = request_derived_valid;
    pending.request_s1_resolved_ppn = request_s1_ppn;
    pending.request_s2_resolved_ppn = request_s2_ppn;
    pending.request_gvpn = request_gvpn;
    pending.accept_sample_seq = COMPLETION_SAMPLE - 1;
    pending.due_sample_seq = COMPLETION_SAMPLE;
    pending.response_visible_sample_seq = COMPLETION_SAMPLE;
    pending.response_payload_frozen = 1'b1;
    pending.response_m_pbmt_en = csr_snapshot.m_pbmt_en;
    pending.response_h_pbmt_en = csr_snapshot.h_pbmt_en;

    responder.initialize_lifecycle_state();
    responder.data = data;
    responder.ppn_reuse_en = reuse_en;
    responder.ppn_reuse_history_size = history_size;
    responder.sample_seq = COMPLETION_SAMPLE;
    responder.driving_req = pending;
    responder.driving_valid = 1'b1;
    responder.accepted_count = 1;
    responder.sampled_resp_valid = completion_visible;
    history_size_before = data.l2tlb_ppn_history_q.size();
    // 中文注释：software harness 显式复刻 service 的 completion guard；
    // sampled_resp_valid=0 时不能调用 complete_driving_response()，history 保持不变。
    if (responder.driving_valid && responder.sampled_resp_valid) begin
        responder.complete_driving_response();
        if (reuse_en) begin
            expected_history_size = history_size_before + 1;
            if (expected_history_size > history_size) begin
                expected_history_size = history_size;
            end
            expect_true(data.l2tlb_ppn_history_q.size() == expected_history_size,
                        "enabled visible response completion must append one FIFO record");
        end else begin
            expect_true(data.l2tlb_ppn_history_q.size() == history_size_before,
                        "disabled visible response completion must not collect PPN history");
        end
    end else begin
        expect_true(data.l2tlb_ppn_history_q.size() == history_size_before,
                    "non-visible response must not append PPN history");
    end
    final_ppn = (s2xlate inside {2'd0, 2'd1}) ? request_s1_ppn : request_s2_ppn;
endtask:complete_frozen_response

function memblock_tlb_entry
    soft_test_l2tlb_ppn_reuse_sequence::make_s1_superpage_entry(
        input memblock_tlb_lookup_key_t anchor_key,
        input mmu_csr_runtime_state csr_snapshot,
        input bit [43:0] canonical_ppn);
    memblock_tlb_entry entry;

    entry = memblock_tlb_entry::type_id::create("ppn_reuse_superpage_entry");
    if (entry == null || csr_snapshot == null) begin
        `uvm_fatal(get_type_name(), "failed to create PPN reuse superpage entry")
    end
    entry.reset();
    entry.lookup_key = anchor_key;
    entry.s2xlate = 2'd0;
    entry.entry_generation = data.allocate_tlb_entry_generation();
    entry.s1_stage_active = 1'b1;
    entry.s1_translation_mode_at_build = csr_snapshot.satp_mode;
    entry.s1_pte_mode_at_build = memblock_tlb_entry::MEMBLOCK_TLB_PTE_MODE_LEGAL;
    entry.s1_root_ppn_at_build = csr_snapshot.satp_ppn;
    entry.csr_context_seq_at_build = csr_snapshot.update_seq;
    entry.s1_tag = anchor_key.vpn[37:3];
    entry.s1_asid = anchor_key.asid;
    entry.s1_level = 2'd1;
    entry.s1_pte_n = 1'b0;
    entry.s1_pte_r = 1'b1;
    entry.s1_pte_a = 1'b1;
    entry.s1_pte_d = 1'b1;
    entry.s1_pte_v = 1'b1;
    entry.s1_addr_low = anchor_key.vpn[2:0];
    entry.s1_resolved_ppn = canonical_ppn;
    entry.s1_resolved_ppn_valid = 1'b1;
    builder.build_s1_sector_payload(canonical_ppn, entry);
    return entry;
endfunction:make_s1_superpage_entry

function void soft_test_l2tlb_ppn_reuse_sequence::install_entry(
    input memblock_tlb_lookup_key_t anchor_key,
    input memblock_tlb_entry entry);
    if (entry == null || data.has_tlb_entry(anchor_key)) begin
        `uvm_fatal(get_type_name(), "invalid directed PPN reuse entry install")
    end
    data.insert_tlb_entry(anchor_key, entry);
    if (!data.register_tlb_range_index(anchor_key, entry)) begin
        `uvm_fatal(get_type_name(), "failed to register directed PPN reuse range entry")
    end
endfunction:install_entry

task soft_test_l2tlb_ppn_reuse_sequence::check_disabled_and_weight_zero();
    mmu_csr_runtime_state csr_snapshot;
    memblock_tlb_lookup_result_e lookup_result;
    memblock_tlb_entry source_entry;
    memblock_tlb_entry no_reuse_entry;
    memblock_tlb_entry wt_zero_entry;
    bit created;
    bit derived_valid;
    bit [43:0] source_ppn;
    bit [43:0] ignored_ppn;
    bit [43:0] wt_zero_baseline_ppn;

    csr_snapshot = make_s1_csr_snapshot();
    data.reset_all_tables(1);
    source_entry = lookup_entry(38'h000_0000_100, 2'd0, csr_snapshot,
                                1'b0, 0, lookup_result, created);
    expect_true(created && lookup_result == MEMBLOCK_TLB_LOOKUP_MISS_BUILD,
                "disabled-path source must be a miss build");
    complete_frozen_response(source_entry, 38'h000_0000_100, 2'd0, csr_snapshot,
                             1, 1, 1'b1, 1'b0, derived_valid, source_ppn);
    expect_true(data.l2tlb_ppn_history_q.size() == 0,
                "reuse disabled completion must not collect PPN history");
    no_reuse_entry = lookup_entry(38'h000_0000_200, 2'd0, csr_snapshot,
                                  1'b0, 100, lookup_result, created);
    expect_true(no_reuse_entry.get_s1_selected_canonical_ppn() != source_ppn,
                "reuse disabled miss must keep its own builder PPN");

    data.reset_all_tables(1);
    source_entry = lookup_entry(38'h000_0000_300, 2'd0, csr_snapshot,
                                1'b0, 0, lookup_result, created);
    complete_frozen_response(source_entry, 38'h000_0000_300, 2'd0, csr_snapshot,
                             2, 2, 1'b1, 1'b1, derived_valid, source_ppn);
    wt_zero_entry = lookup_entry(38'h000_0000_380, 2'd0, csr_snapshot,
                                 1'b0, 0, lookup_result, created);
    wt_zero_baseline_ppn = wt_zero_entry.get_s1_selected_canonical_ppn();
    data.clear_dispatch_l2tlb_live_entries();
    wt_zero_entry = lookup_entry(38'h000_0000_380, 2'd0, csr_snapshot,
                                 1'b1, 0, lookup_result, created);
    expect_true(wt_zero_entry.get_s1_selected_canonical_ppn() == wt_zero_baseline_ppn,
                "WT=0 must keep the new mapping builder PPN");
    expect_true(data.l2tlb_ppn_history_q.size() == 1,
                "WT=0 lookup must not consume or rewrite history");
    ignored_ppn = source_ppn;
endtask:check_disabled_and_weight_zero

task soft_test_l2tlb_ppn_reuse_sequence::check_completion_history_and_same_sample_reuse();
    mmu_csr_runtime_state csr_snapshot;
    memblock_tlb_lookup_result_e lookup_result;
    memblock_tlb_entry source_entry;
    memblock_tlb_entry invisible_entry;
    memblock_tlb_entry reused_entry;
    bit created;
    bit derived_valid;
    bit [43:0] source_ppn;
    bit [43:0] ignored_ppn;

    csr_snapshot = make_s1_csr_snapshot();
    data.reset_all_tables(1);
    source_entry = lookup_entry(38'h000_0000_500, 2'd0, csr_snapshot,
                                1'b0, 0, lookup_result, created);
    complete_frozen_response(source_entry, 38'h000_0000_500, 2'd0, csr_snapshot,
                             0, 1, 1'b1, 1'b1, derived_valid, source_ppn);
    expect_true(derived_valid && data.l2tlb_ppn_history_q.size() == 1 &&
                data.l2tlb_ppn_history_q[0].ppn_valid &&
                data.l2tlb_ppn_history_q[0].ppn == source_ppn,
                "token-0 normal completion must record its request-specific S1 PPN");
    invisible_entry = lookup_entry(38'h000_0000_540, 2'd0, csr_snapshot,
                                   1'b0, 0, lookup_result, created);
    complete_frozen_response(invisible_entry, 38'h000_0000_540, 2'd0, csr_snapshot,
                             1, 1, 1'b0, 1'b1, derived_valid, ignored_ppn);
    expect_true(data.l2tlb_ppn_history_q.size() == 1 &&
                data.l2tlb_ppn_history_q[0].ppn == source_ppn,
                "non-visible response must not alter completed PPN history");
    // 中文注释：这次 lookup 紧随同一 software sample 的 completion 之后，复刻 send_l2tlb_cycle 的 completion-before-capture 顺序。
    reused_entry = lookup_entry(38'h000_0000_580, 2'd0, csr_snapshot,
                                1'b1, 100, lookup_result, created);
    expect_true(created && lookup_result == MEMBLOCK_TLB_LOOKUP_MISS_BUILD &&
                reused_entry.get_s1_selected_canonical_ppn() == source_ppn,
                "same-sample 4KB miss must reuse the just-completed PPN at WT=100");
    expect_true(reused_entry.s1_entry_ppn_raw == source_ppn[43:3] &&
                reused_entry.s1_ppn_low[reused_entry.s1_addr_low] == source_ppn[2:0],
                "S1 reuse must rebuild split entry_ppn and ppn_low fields");
    reused_entry.validate_s1_sector_payload_consistency("PPN_REUSE_SOFT_TEST");
    ignored_ppn = source_ppn;
endtask:check_completion_history_and_same_sample_reuse

task soft_test_l2tlb_ppn_reuse_sequence::check_fifo_invalid_and_reset_lifecycle();
    mmu_csr_runtime_state csr_snapshot;
    memblock_tlb_lookup_result_e lookup_result;
    memblock_tlb_entry entry0;
    memblock_tlb_entry entry1;
    memblock_tlb_entry entry2;
    memblock_tlb_entry fault_entry;
    memblock_tlb_entry pma_entry;
    memblock_tlb_entry no_derived_entry;
    memblock_tlb_entry fallback_entry;
    memblock_tlb_lookup_key_t c4_target_key;
    memblock_sfence_payload_t c4_payload;
    bit created;
    bit derived_valid;
    bit [43:0] ppn0;
    bit [43:0] ppn1;
    bit [43:0] ppn2;
    bit [43:0] ignored_ppn;
    bit [43:0] fallback_baseline_ppn;
    int unsigned c4_deleted_count;
    int unsigned c4_history_size;
    longint unsigned c4_anchor_sample;
    longint unsigned c4_event_seq;
    longint unsigned c4_reset_epoch;
    longint unsigned saved_last_event_seq;

    csr_snapshot = make_s1_csr_snapshot();
    data.reset_all_tables(1);
    entry0 = lookup_entry(38'h000_0000_700, 2'd0, csr_snapshot, 1'b0, 0,
                          lookup_result, created);
    complete_frozen_response(entry0, 38'h000_0000_700, 2'd0, csr_snapshot,
                             20, 2, 1'b1, 1'b1, derived_valid, ppn0);
    entry1 = lookup_entry(38'h000_0000_780, 2'd0, csr_snapshot, 1'b0, 0,
                          lookup_result, created);
    complete_frozen_response(entry1, 38'h000_0000_780, 2'd0, csr_snapshot,
                             21, 2, 1'b1, 1'b1, derived_valid, ppn1);
    entry2 = lookup_entry(38'h000_0000_800, 2'd0, csr_snapshot, 1'b0, 0,
                          lookup_result, created);
    complete_frozen_response(entry2, 38'h000_0000_800, 2'd0, csr_snapshot,
                             22, 2, 1'b1, 1'b1, derived_valid, ppn2);
    expect_true(data.l2tlb_ppn_history_q.size() == 2 &&
                data.l2tlb_ppn_history_q[0].ppn == ppn1 &&
                data.l2tlb_ppn_history_q[1].ppn == ppn2,
                "M=2 must evict only the oldest completed PPN record");

    data.reset_all_tables(1);
    entry0 = lookup_entry(38'h000_0000_900, 2'd0, csr_snapshot, 1'b0, 0,
                          lookup_result, created);
    fault_entry = memblock_tlb_entry::type_id::create("ppn_reuse_fault_entry");
    pma_entry = memblock_tlb_entry::type_id::create("ppn_reuse_pma_entry");
    no_derived_entry = memblock_tlb_entry::type_id::create("ppn_reuse_no_derived_entry");
    if (fault_entry == null || pma_entry == null || no_derived_entry == null) begin
        `uvm_fatal(get_type_name(), "failed to create invalid PPN history entries")
    end
    fault_entry.copy_from(entry0);
    pma_entry.copy_from(entry0);
    no_derived_entry.copy_from(entry0);
    fault_entry.fault_effective_s1_pf = 1'b1;
    pma_entry.pmaAF = 1'b1;
    data.record_l2tlb_completed_ppn_history(2'd0, 1'b1, 44'h111, '0,
                                            fault_entry, 30, COMPLETION_SAMPLE, 5);
    data.record_l2tlb_completed_ppn_history(2'd0, 1'b1, 44'h222, '0,
                                            pma_entry, 31, COMPLETION_SAMPLE, 5);
    data.record_l2tlb_completed_ppn_history(2'd0, 1'b0, 44'h333, '0,
                                            no_derived_entry, 32, COMPLETION_SAMPLE, 5);
    expect_true(data.l2tlb_ppn_history_q.size() == 3 &&
                !data.l2tlb_ppn_history_q[0].ppn_valid &&
                !data.l2tlb_ppn_history_q[1].ppn_valid &&
                !data.l2tlb_ppn_history_q[2].ppn_valid,
                "fault, PMA AF and unresolvable completion must keep invalid FIFO positions");
    fallback_entry = lookup_entry(38'h000_0000_980, 2'd0, csr_snapshot,
                                  1'b0, 0, lookup_result, created);
    fallback_baseline_ppn = fallback_entry.get_s1_selected_canonical_ppn();
    data.clear_dispatch_l2tlb_live_entries();
    fallback_entry = lookup_entry(38'h000_0000_980, 2'd0, csr_snapshot,
                                  1'b1, 100, lookup_result, created);
    expect_true(fallback_entry.get_s1_selected_canonical_ppn() == fallback_baseline_ppn,
                "invalid history records must never become reuse candidates");
    data.clear_dispatch_l2tlb_live_entries();
    expect_true(data.l2tlb_ppn_history_q.size() == 3,
                "clear_dispatch_l2tlb_live_entries must preserve completed PPN history");
    // 中文注释：不启动真实 adapter/owner，仅为 C4 helper 构造一笔当前 epoch 的
    // fence provenance；event sequence 在本段结束时恢复，避免 software smoke 污染全局 lifecycle history。
    data.clear_l2tlb_ppn_history();
    data.record_l2tlb_completed_ppn_history(2'd0, 1'b1, 44'h444, '0,
                                            entry0, 33, COMPLETION_SAMPLE, 4);
    data.clear_dispatch_l2tlb_live_entries();
    fallback_entry = lookup_entry(38'h000_0000_990, 2'd0, csr_snapshot,
                                  1'b0, 0, lookup_result, created);
    c4_target_key = csr_snapshot.make_lookup_key({26'b0, 38'h000_0000_990}, 2'd0);
    expect_true(created && data.has_tlb_entry(c4_target_key),
                "C4 directed target must publish one live mapping");
    c4_reset_epoch = memblock_sync_pkg::get_l2tlb_current_reset_epoch();
    c4_anchor_sample = COMPLETION_SAMPLE + 10;
    saved_last_event_seq = memblock_sync_pkg::last_allocated_l2tlb_event_seq;
    c4_event_seq = saved_last_event_seq + 1;
    if (c4_event_seq == memblock_sync_pkg::MEMBLOCK_L2TLB_EVENT_SEQ_NONE) begin
        `uvm_fatal(get_type_name(), "PPN reuse C4 directed event sequence overflow")
    end
    memblock_sync_pkg::last_allocated_l2tlb_event_seq = c4_event_seq;
    c4_payload = '{default:'0};
    c4_payload.valid = 1'b1;
    c4_payload.ignore_addr = 1'b1;
    c4_payload.ignore_id = 1'b1;
    c4_payload.target_stage = MEMBLOCK_SFENCE_TARGET_HS_S1;
    c4_payload.sample_seq = c4_anchor_sample;
    c4_payload.reset_epoch = c4_reset_epoch;
    c4_payload.lifecycle_event_seq = c4_event_seq;
    c4_history_size = data.l2tlb_ppn_history_q.size();
    if (!data.schedule_sfence_invalidate(c4_payload, c4_anchor_sample,
                                         c4_reset_epoch, c4_event_seq)) begin
        `uvm_fatal(get_type_name(), "PPN reuse C4 directed fence schedule failed")
    end
    c4_deleted_count = data.apply_due_sfence_invalidate(
        c4_anchor_sample + MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES,
        c4_reset_epoch);
    memblock_sync_pkg::last_allocated_l2tlb_event_seq = saved_last_event_seq;
    expect_true(c4_deleted_count == 1 && !data.has_tlb_entry(c4_target_key),
                "ordinary SFENCE/HFENCE C4 must delete its matching live entry");
    expect_true(data.l2tlb_ppn_history_q.size() == c4_history_size &&
                data.l2tlb_ppn_history_q[0].ppn_valid &&
                data.l2tlb_ppn_history_q[0].ppn == 44'h444,
                "ordinary SFENCE/HFENCE C4 must preserve completed PPN history");
    data.clear_l2tlb_ppn_history();
    expect_true(data.l2tlb_ppn_history_q.size() == 0,
                "dedicated PPN history clear must not require live-entry reset");
    data.reset_all_tables(1);
    expect_true(data.l2tlb_ppn_history_q.size() == 0,
                "reset_all_tables must clear PPN history for the next testcase");
    ignored_ppn = ppn0;
endtask:check_fifo_invalid_and_reset_lifecycle

task soft_test_l2tlb_ppn_reuse_sequence::check_exact_range_and_target_encoding();
    mmu_csr_runtime_state s1_csr;
    mmu_csr_runtime_state s2_csr;
    memblock_tlb_lookup_result_e lookup_result;
    memblock_tlb_lookup_key_t range_anchor_key;
    memblock_tlb_entry miss_entry;
    memblock_tlb_entry exact_entry;
    memblock_tlb_entry range_entry;
    memblock_tlb_entry range_hit_entry;
    memblock_tlb_entry s2_entry;
    memblock_tlb_entry s2_reused_entry;
    memblock_tlb_entry s2_baseline_entry;
    memblock_tlb_entry s2_nonleaf_target;
    memblock_tlb_entry allstage_nonleaf_target;
    memblock_tlb_entry superpage_target;
    bit created;
    bit derived_valid;
    bit reused_ppn;
    bit [43:0] miss_ppn;
    bit [43:0] exact_ppn;
    bit [43:0] range_ppn;
    bit [43:0] ignored_ppn;
    bit [43:0] range_raw_ppn;
    bit [43:0] s2_baseline_ppn;

    s1_csr = make_s1_csr_snapshot();
    data.reset_all_tables(1);
    miss_entry = lookup_entry(38'h000_0000_a00, 2'd0, s1_csr, 1'b0, 0,
                              lookup_result, created);
    expect_true(created && lookup_result == MEMBLOCK_TLB_LOOKUP_MISS_BUILD,
                "directed miss token must report MISS_BUILD");
    complete_frozen_response(miss_entry, 38'h000_0000_a00, 2'd0, s1_csr,
                             40, 8, 1'b1, 1'b1, derived_valid, miss_ppn);
    exact_entry = lookup_entry(38'h000_0000_a00, 2'd0, s1_csr, 1'b1, 100,
                               lookup_result, created);
    expect_true(!created && lookup_result == MEMBLOCK_TLB_LOOKUP_EXACT_HIT &&
                exact_entry == miss_entry,
                "exact hit must not create or rewrite a live entry");
    complete_frozen_response(exact_entry, 38'h000_0000_a00, 2'd0, s1_csr,
                             41, 8, 1'b1, 1'b1, derived_valid, exact_ppn);

    range_raw_ppn = 44'h0000_0000_1c0;
    range_anchor_key = s1_csr.make_lookup_key({26'b0, 38'h000_0000_b00}, 2'd0);
    range_entry = make_s1_superpage_entry(range_anchor_key, s1_csr, range_raw_ppn);
    install_entry(range_anchor_key, range_entry);
    range_hit_entry = lookup_entry(38'h000_0000_b07, 2'd0, s1_csr, 1'b1, 100,
                                   lookup_result, created);
    expect_true(!created && lookup_result == MEMBLOCK_TLB_LOOKUP_RANGE_HIT &&
                range_hit_entry == range_entry,
                "range hit must not enter the new-entry reuse path");
    complete_frozen_response(range_hit_entry, 38'h000_0000_b07, 2'd0, s1_csr,
                             42, 8, 1'b1, 1'b1, derived_valid, range_ppn);
    expect_true(derived_valid && range_ppn != range_raw_ppn &&
                data.l2tlb_ppn_history_q[$].ppn == range_ppn,
                "range completion must record request-specific PPN instead of anchor raw PPN");

    s2_csr = make_s2_csr_snapshot();
    data.reset_all_tables(1);
    s2_entry = lookup_entry(38'h000_0000_c00, 2'd2, s2_csr, 1'b0, 0,
                            lookup_result, created);
    data.record_l2tlb_completed_ppn_history(2'd2, 1'b1, '0,
                                            44'h0000_0000_456,
                                            s2_entry, 50, COMPLETION_SAMPLE, 4);
    s2_reused_entry = lookup_entry(38'h000_0000_c80, 2'd2, s2_csr, 1'b1, 100,
                                   lookup_result, created);
    expect_true(s2_reused_entry.s2_entry_ppn_raw == 38'h456,
                "S2 reuse must encode only the legal 38-bit response PPN");
    data.reset_all_tables(1);
    s2_entry = lookup_entry(38'h000_0000_d00, 2'd2, s2_csr, 1'b0, 0,
                            lookup_result, created);
    data.record_l2tlb_completed_ppn_history(2'd2, 1'b1, '0,
                                            44'h0400_0000_456,
                                            s2_entry, 51, COMPLETION_SAMPLE, 4);
    s2_baseline_entry = lookup_entry(38'h000_0000_d80, 2'd2, s2_csr, 1'b0, 0,
                                     lookup_result, created);
    s2_baseline_ppn = s2_baseline_entry.s2_entry_ppn_raw;
    data.clear_dispatch_l2tlb_live_entries();
    s2_reused_entry = lookup_entry(38'h000_0000_d80, 2'd2, s2_csr, 1'b1, 100,
                                   lookup_result, created);
    expect_true(s2_reused_entry.s2_entry_ppn_raw == s2_baseline_ppn,
                "unencodable S2 history PPN must not be selected");

    data.reset_all_tables(1);
    s2_entry = lookup_entry(38'h000_0000_d00, 2'd2, s2_csr, 1'b0, 0,
                            lookup_result, created);
    data.record_l2tlb_completed_ppn_history(2'd2, 1'b1, '0,
                                            44'h0000_0000_456,
                                            s2_entry, 52, COMPLETION_SAMPLE, 4);
    s2_nonleaf_target = memblock_tlb_entry::type_id::create("ppn_reuse_s2_nonleaf_target");
    allstage_nonleaf_target = memblock_tlb_entry::type_id::create(
        "ppn_reuse_allstage_nonleaf_target");
    if (s2_nonleaf_target == null || allstage_nonleaf_target == null) begin
        `uvm_fatal(get_type_name(), "failed to create S2 non-leaf PPN reuse targets")
    end
    s2_nonleaf_target.copy_from(s2_entry);
    s2_nonleaf_target.s2_pte_r = 1'b0;
    s2_nonleaf_target.s2_pte_w = 1'b0;
    s2_nonleaf_target.s2_pte_x = 1'b0;
    s2_baseline_ppn = s2_nonleaf_target.s2_entry_ppn_raw;
    data.try_apply_l2tlb_ppn_reuse_to_new_entry(s2_nonleaf_target, 100, reused_ppn);
    expect_true(!reused_ppn &&
                s2_nonleaf_target.s2_entry_ppn_raw == s2_baseline_ppn,
                "S2 non-leaf target must not accept a reused PPN");
    allstage_nonleaf_target.copy_from(s2_entry);
    allstage_nonleaf_target.s2xlate = 2'd3;
    allstage_nonleaf_target.s1_stage_active = 1'b1;
    allstage_nonleaf_target.s2_pte_r = 1'b0;
    allstage_nonleaf_target.s2_pte_w = 1'b0;
    allstage_nonleaf_target.s2_pte_x = 1'b0;
    s2_baseline_ppn = allstage_nonleaf_target.s2_entry_ppn_raw;
    data.try_apply_l2tlb_ppn_reuse_to_new_entry(allstage_nonleaf_target, 100, reused_ppn);
    expect_true(!reused_ppn &&
                allstage_nonleaf_target.s2_entry_ppn_raw == s2_baseline_ppn,
                "allStage S2 non-leaf target must not accept a reused PPN");

    data.reset_all_tables(1);
    miss_entry = lookup_entry(38'h000_0000_e00, 2'd0, s1_csr, 1'b0, 0,
                              lookup_result, created);
    data.record_l2tlb_completed_ppn_history(2'd0, 1'b1, 44'h0000_0000_777,
                                            '0, miss_entry, 60,
                                            COMPLETION_SAMPLE, 4);
    superpage_target = make_s1_superpage_entry(
        s1_csr.make_lookup_key({26'b0, 38'h000_0000_f00}, 2'd0), s1_csr,
        44'h0000_0000_1c0);
    s2_baseline_ppn = superpage_target.get_s1_selected_canonical_ppn();
    data.try_apply_l2tlb_ppn_reuse_to_new_entry(superpage_target, 100, reused_ppn);
    expect_true(!reused_ppn &&
                superpage_target.get_s1_selected_canonical_ppn() == s2_baseline_ppn,
                "superpage target must keep builder PPN even with valid history");
    ignored_ppn = miss_ppn;
    ignored_ppn = exact_ppn;
    ignored_ppn = range_ppn;
endtask:check_exact_range_and_target_encoding

`endif
