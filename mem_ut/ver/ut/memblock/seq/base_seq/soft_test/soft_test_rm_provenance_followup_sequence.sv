//=========================================================
//File name    : soft_test_rm_provenance_followup_sequence.sv
//Module name  : soft_test_rm_provenance_followup_sequence
//Discribution : software-only RM UID-TLB and L2 D-error provenance checks
//=========================================================
`ifndef SOFT_TEST_RM_PROVENANCE_FOLLOWUP_SEQUENCE__SV
`define SOFT_TEST_RM_PROVENANCE_FOLLOWUP_SEQUENCE__SV

class memblock_uncache_owner_expected_fatal_catcher extends uvm_report_catcher;
    int unsigned caught_count;
    string expected_message_pattern;

    function new(
        string name = "memblock_uncache_owner_expected_fatal_catcher",
        string message_pattern = "*Uncache source*"
    );
        super.new(name);
        caught_count = 0;
        expected_message_pattern = message_pattern;
    endfunction:new

    virtual function action_e catch();
        if (get_severity() == UVM_FATAL &&
            uvm_pkg::uvm_is_match(expected_message_pattern, get_message())) begin
            caught_count++;
            return CAUGHT;
        end
        return THROW;
    endfunction:catch
endclass:memblock_uncache_owner_expected_fatal_catcher

class soft_test_rm_provenance_followup_sequence extends memblock_dispatch_base_sequence;

    `uvm_object_utils(soft_test_rm_provenance_followup_sequence)

    extern function new(string name = "soft_test_rm_provenance_followup_sequence");
    extern virtual task body();
    extern virtual function void expect_true(input bit condition,
                                             input string message);
    extern virtual task check_uid_tlb_payload_over_live_entry();
    extern virtual task check_l1_hit_issue_snapshot_lifecycle();
    extern virtual task check_l1_hit_overlap_resolution();
    extern virtual task check_l2_d_error_sample_boundaries();
    extern virtual task check_uncache_source_lifecycle();

endclass:soft_test_rm_provenance_followup_sequence

function soft_test_rm_provenance_followup_sequence::new(
    string name = "soft_test_rm_provenance_followup_sequence");
    super.new(name);
endfunction:new

function void soft_test_rm_provenance_followup_sequence::expect_true(
    input bit condition,
    input string message);
    if (!condition) begin
        `uvm_fatal(get_type_name(), message)
    end
endfunction:expect_true

task soft_test_rm_provenance_followup_sequence::body();
    seq_csr_common::init();
    data = common_data_transaction::get();
    expect_true(data != null, "common data owner is unavailable");
    check_uid_tlb_payload_over_live_entry();
    check_l1_hit_issue_snapshot_lifecycle();
    check_l1_hit_overlap_resolution();
    check_l2_d_error_sample_boundaries();
    check_uncache_source_lifecycle();
    `uvm_info(get_type_name(),
              "RM provenance follow-up directed checks completed", UVM_LOW)
endtask:body

task soft_test_rm_provenance_followup_sequence::check_uid_tlb_payload_over_live_entry();
    localparam memblock_uid_t TEST_UID = 7;
    localparam bit [51:0] TEST_VPN = 52'h0000_0000_8cd69;
    mmu_csr_runtime_state csr;
    memblock_uid_tlb_record record;
    memblock_tlb_entry uid_entry;
    memblock_tlb_entry overwritten_live_entry;
    memblock_rm_readonly_api::tlb_request_context_view_t context_view;
    memblock_rm_readonly_api::tlb_entry_view_t entry_view;
    memblock_rm_readonly_api::tlb_entry_view_t live_view;

    csr = mmu_csr_runtime_state::type_id::create("rm_provenance_csr");
    csr.reset();
    csr.satp_mode = 4'd8;
    csr.satp_asid = 16'h31;
    csr.update_seq = 9;
    record = memblock_uid_tlb_record::type_id::create("rm_provenance_uid_record");
    record.init_context(TEST_UID, TEST_VPN, 2'd0, 1'b0, csr, 10);

    uid_entry = memblock_tlb_entry::type_id::create("rm_provenance_uid_entry");
    uid_entry.lookup_key = record.lookup_key;
    uid_entry.s2xlate = record.s2xlate;
    uid_entry.entry_generation = 11;
    uid_entry.csr_context_seq_at_build = csr.update_seq;
    record.copy_entry_fields(uid_entry);
    record.request_derived_valid = 1'b1;
    record.request_s1_resolved_ppn = 44'h12345;
    record.request_gvpn = TEST_VPN;
    record.mark_completed();

    overwritten_live_entry = memblock_tlb_entry::type_id::create(
        "rm_provenance_overwritten_live_entry");
    overwritten_live_entry.lookup_key = record.lookup_key;
    overwritten_live_entry.s2xlate = record.s2xlate;
    overwritten_live_entry.entry_generation = 12;
    overwritten_live_entry.csr_context_seq_at_build = csr.update_seq;
    overwritten_live_entry.fault_effective_s1_pf = 1'b1;
    overwritten_live_entry.fault_stage_selected =
        memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_S1;

    data.uid_tlb_record_by_uid[TEST_UID] = record;
    data.tlb_entry_by_key[record.lookup_key] = overwritten_live_entry;
    expect_true(memblock_rm_readonly_api::get().read_tlb_entry_for_rm(
                    record.lookup_key, live_view) &&
                live_view.fault_effective_s1_pf,
                "directed setup did not overwrite live canonical entry with PF");
    expect_true(memblock_rm_readonly_api::get().resolve_tlb_entry_for_rm(
                    TEST_UID, TEST_VPN, context_view, entry_view),
                "UID completed TLB payload resolver failed");
    expect_true(entry_view.entry_generation == 11 &&
                !entry_view.fault_effective_s1_pf &&
                context_view.request_translation_valid &&
                context_view.request_s1_resolved_ppn == 44'h12345,
                "resolver used overwritten live entry instead of UID payload snapshot");

    data.uid_tlb_record_by_uid.delete(TEST_UID);
    data.tlb_entry_by_key.delete(record.lookup_key);
endtask:check_uid_tlb_payload_over_live_entry

task soft_test_rm_provenance_followup_sequence::check_l1_hit_issue_snapshot_lifecycle();
    localparam memblock_uid_t TEST_UID = 8;
    localparam bit [51:0] TEST_VPN = 52'h0000_0000_8d34a;
    // The production L2TLB token allocator starts at zero.  Exercise token 0
    // through cache publication, UID freeze, RM query, and invalidation.
    localparam longint unsigned TEST_TOKEN = 0;
    localparam longint unsigned TEST_SAMPLE = 64'h29;
    mmu_csr_runtime_state csr;
    memblock_tlb_lookup_key_t key;
    memblock_uid_tlb_record record;
    memblock_tlb_entry completed_entry;
    memblock_tlb_entry overwritten_live_entry;
    memblock_rm_readonly_api::tlb_request_context_view_t context_view;
    memblock_rm_readonly_api::tlb_entry_view_t entry_view;
    longint unsigned completed_generation;

    csr = mmu_csr_runtime_state::type_id::create("rm_l1_hit_csr");
    csr.reset();
    csr.satp_mode = 4'd8;
    csr.satp_asid = 16'h52;
    csr.update_seq = 17;
    key = csr.make_lookup_key(TEST_VPN, 2'd0);
    completed_entry = data.build_tlb_entry_for_key_with_csr(key, csr);
    completed_generation = completed_entry.entry_generation;
    data.record_completed_tlb_response(completed_entry, TEST_TOKEN, TEST_SAMPLE);

    record = memblock_uid_tlb_record::type_id::create("rm_l1_hit_uid_record");
    record.init_context(TEST_UID, TEST_VPN, 2'd0, 1'b0, csr, TEST_SAMPLE + 1);
    data.uid_tlb_record_by_uid[TEST_UID] = record;
    expect_true(data.capture_uid_issue_hit_snapshot(record),
                "completed response cache did not produce an issue-hit snapshot");
    expect_true(record.is_waiting() && !record.pte_valid &&
                record.issue_hit_snapshot_valid &&
                record.uid_tlb_first_request_fire_sample_seq == 0 &&
                record.issue_hit_response_token == TEST_TOKEN &&
                record.issue_hit_response_sample_seq == TEST_SAMPLE,
                "issue-hit snapshot provenance/state is invalid");

    overwritten_live_entry = data.build_tlb_entry_for_key_with_csr(key, csr);
    data.tlb_entry_by_key[key] = overwritten_live_entry;
    expect_true(memblock_rm_readonly_api::get().resolve_tlb_entry_for_rm(
                    TEST_UID, TEST_VPN, context_view, entry_view) &&
                entry_view.entry_generation == completed_generation,
                "RM did not use immutable no-request-fire issue-hit payload");

    record.mark_request_fire(TEST_SAMPLE + 2);
    expect_true(!record.issue_hit_snapshot_valid &&
                record.uid_tlb_first_request_fire_sample_seq == TEST_SAMPLE + 2,
                "request fire did not invalidate issue-hit fallback");

    record.init_context(TEST_UID, TEST_VPN, 2'd0, 1'b0, csr, TEST_SAMPLE + 3);
    expect_true(data.capture_uid_issue_hit_snapshot(record),
                "reissue did not capture a fresh issue-hit snapshot");
    record.mark_canceled();
    expect_true(!record.issue_hit_snapshot_valid &&
                record.uid_tlb_wait_state ==
                    memblock_uid_tlb_record::MEMBLOCK_UID_TLB_CANCELED,
                "cancel did not invalidate issue-hit fallback");

    data.delete_completed_tlb_entry_by_anchor_key(key, "directed lifecycle cleanup");
    expect_true(!data.completed_tlb_entry_by_key.exists(key),
                "completed-response cache delete left the anchor visible");
    data.uid_tlb_record_by_uid.delete(TEST_UID);
    data.tlb_entry_by_key.delete(key);
endtask:check_l1_hit_issue_snapshot_lifecycle

task soft_test_rm_provenance_followup_sequence::check_l1_hit_overlap_resolution();
    localparam memblock_uid_t TEST_UID = 9;
    localparam bit [51:0] TEST_VPN = 52'h0000_0000_8cd69;
    localparam longint unsigned SUPER_TOKEN = 5;
    localparam longint unsigned SUPER_SAMPLE = 102;
    localparam longint unsigned EXACT_TOKEN = 0;
    localparam longint unsigned EXACT_SAMPLE = 108;
    localparam longint unsigned ISSUE_SAMPLE = 120;
    mmu_csr_runtime_state csr;
    memblock_tlb_lookup_key_t exact_key;
    memblock_tlb_lookup_key_t super_key;
    memblock_uid_tlb_record record;
    memblock_tlb_entry exact_pf_entry;
    memblock_tlb_entry super_normal_entry;
    memblock_rm_readonly_api::tlb_request_context_view_t context_view;
    memblock_rm_readonly_api::tlb_entry_view_t entry_view;
    longint unsigned super_generation;

    csr = mmu_csr_runtime_state::type_id::create("rm_overlap_csr");
    csr.reset();
    csr.satp_mode = 4'd8;
    csr.satp_asid = 16'h0;
    csr.update_seq = 23;
    exact_key = csr.make_lookup_key(TEST_VPN, 2'd0);
    super_key = csr.make_lookup_key({TEST_VPN[51:9], 9'b0}, 2'd0);

    super_normal_entry = data.build_tlb_entry_for_key_with_csr(super_key, csr);
    super_normal_entry.s1_level = 2'd1;
    super_normal_entry.s1_pte_n = 1'b0;
    super_normal_entry.fault_raw_s1_pf = 1'b0;
    super_normal_entry.fault_raw_s1_af = 1'b0;
    super_normal_entry.fault_effective_s1_pf = 1'b0;
    super_normal_entry.fault_effective_s1_af = 1'b0;
    super_normal_entry.fault_stage_selected =
        memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_NONE;
    super_normal_entry.s1_pte_v = 1'b1;
    super_normal_entry.s1_pte_r = 1'b1;
    super_normal_entry.s1_pte_a = 1'b1;
    foreach (super_normal_entry.s1_valididx[idx]) begin
        super_normal_entry.s1_valididx[idx] = 1'b1;
        super_normal_entry.s1_ppn_low[idx] =
            super_normal_entry.s1_ppn_low[super_normal_entry.s1_addr_low];
    end
    super_normal_entry.validate_s1_sector_payload_consistency(
        "RM_OVERLAP_SUPER_SETUP");
    super_generation = super_normal_entry.entry_generation;
    data.record_completed_tlb_response(
        super_normal_entry, SUPER_TOKEN, SUPER_SAMPLE);

    exact_pf_entry = data.build_tlb_entry_for_key_with_csr(exact_key, csr);
    exact_pf_entry.fault_effective_s1_pf = 1'b1;
    exact_pf_entry.fault_stage_selected =
        memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_S1;
    data.record_completed_tlb_response(
        exact_pf_entry, EXACT_TOKEN, EXACT_SAMPLE);

    record = memblock_uid_tlb_record::type_id::create("rm_overlap_uid_record");
    record.init_context(TEST_UID, TEST_VPN, 2'd0, 1'b0, csr, ISSUE_SAMPLE);
    data.uid_tlb_record_by_uid[TEST_UID] = record;
    expect_true(data.capture_uid_issue_hit_snapshot(record) &&
                record.issue_hit_payload.entry_generation ==
                    exact_pf_entry.entry_generation &&
                record.issue_hit_payload.has_effective_fault(),
                "directed setup did not reproduce exact-PF first selection");
    expect_true(memblock_rm_readonly_api::get().resolve_tlb_entry_for_rm(
                    TEST_UID, TEST_VPN, context_view, entry_view),
                "RM overlap issue-hit resolver failed");
    expect_true(entry_view.entry_generation == super_generation &&
                !entry_view.fault_effective_s1_pf &&
                context_view.range_hit &&
                context_view.request_translation_valid,
                "RM did not select the widest issue-visible normal response");

    data.delete_completed_tlb_entry_by_anchor_key(
        exact_key, "RM overlap directed cleanup");
    data.delete_completed_tlb_entry_by_anchor_key(
        super_key, "RM overlap directed cleanup");
    data.uid_tlb_record_by_uid.delete(TEST_UID);
endtask:check_l1_hit_overlap_resolution

task soft_test_rm_provenance_followup_sequence::check_l2_d_error_sample_boundaries();
    mem_access_base_sequence::mem_addr_t line_addr;
    mem_access_base_sequence::mem_addr_t current_sample_line_addr;
    mem_access_base_sequence::mem_addr_t reserved_line_addr;
    memblock_rm_readonly_api::l2_d_error_view_t view;
    longint unsigned current_sample;
    longint unsigned reservation_generation;
    bit denied;
    bit corrupt;
    bit needs_candidate;

    line_addr = 48'h0000_0b93_a1c0;
    mem_access_base_sequence::reset_l2_d_error_ledger(1'b1);
    mem_access_base_sequence::commit_l2_d_error_on_grant_d_fire(
        line_addr, 1'b0, 1'b1, 10, 10'h12, 0);
    expect_true(memblock_rm_readonly_api::get().read_l2_d_error_for_rm(
                    line_addr, 9, view) && view.valid &&
                !view.corrupt && !view.denied,
                "corrupt became visible before its activation sample");
    expect_true(memblock_rm_readonly_api::get().read_l2_d_error_for_rm(
                    line_addr, 10, view) && view.corrupt && !view.denied,
                "corrupt was not visible at its activation sample");

    mem_access_base_sequence::commit_l2_d_error_on_grant_d_fire(
        line_addr, 1'b1, 1'b1, 20, 10'h13, 0);
    expect_true(memblock_rm_readonly_api::get().read_l2_d_error_for_rm(
                    line_addr, 19, view) && view.corrupt && !view.denied,
                "denied upgrade leaked before its activation sample");
    expect_true(memblock_rm_readonly_api::get().read_l2_d_error_for_rm(
                    line_addr, 20, view) && view.corrupt && view.denied,
                "denied upgrade was not visible at its activation sample");

    current_sample_line_addr = 48'h0000_0b93_a200;
    current_sample = memblock_sync_pkg::peek_current_dut_global_sample();
    expect_true(current_sample > 1,
                "DUT global sample is unavailable for production D-error wrapper test");
    mem_access_base_sequence::commit_l2_d_error_on_current_grant_d_fire(
        current_sample_line_addr, 1'b0, 1'b1, 10'h14, 0);
    expect_true(memblock_rm_readonly_api::get().read_l2_d_error_for_rm(
                    current_sample_line_addr, current_sample - 1, view) &&
                !view.corrupt && !view.denied,
                "current-sample GrantData corrupt leaked into the previous sample");
    expect_true(memblock_rm_readonly_api::get().read_l2_d_error_for_rm(
                    current_sample_line_addr, current_sample, view) &&
                view.corrupt && !view.denied &&
                view.corrupt_activation_sample == current_sample,
                "production GrantData wrapper did not publish DUT-sample provenance");

    reserved_line_addr = 48'h0000_0b93_a240;
    expect_true(mem_access_base_sequence::prepare_l2_d_error_for_grant(
                    reserved_line_addr, current_sample, 10'h15,
                    1'b1, 1'b0, 1'b1,
                    denied, corrupt, needs_candidate,
                    reservation_generation) &&
                !denied && corrupt && !needs_candidate &&
                reservation_generation != 0,
                "GrantData corrupt reservation did not retain provenance");
    mem_access_base_sequence::commit_l2_d_error_on_current_grant_d_fire(
        reserved_line_addr, denied, corrupt, 10'h15,
        reservation_generation);
    expect_true(memblock_rm_readonly_api::get().read_l2_d_error_for_rm(
                    reserved_line_addr, current_sample, view) &&
                view.corrupt && !view.denied &&
                view.generation == reservation_generation &&
                view.source_valid && view.source == 10'h15,
                "GrantData reservation/commit changed line or provenance");
endtask:check_l2_d_error_sample_boundaries

task soft_test_rm_provenance_followup_sequence::check_uncache_source_lifecycle();
    sbuffer_mem_access_base_sequence responder;
    memblock_uncache_owner_expected_fatal_catcher catcher;

    responder = sbuffer_mem_access_base_sequence::type_id::create(
        "uncache_source_lifecycle_responder");
    responder.clear_runtime_state();
    responder.note_uncache_source_accept(4'h3, 48'h8000_0100, 10);
    responder.note_uncache_source_accept(4'h7, 48'h8000_0200, 11);
    expect_true(responder.uncache_source_outstanding == 16'h0088,
                "Uncache source ledger did not retain two independent owners");
    responder.note_uncache_source_complete(4'h7, 20);
    expect_true(responder.uncache_source_outstanding == 16'h0008 &&
                responder.uncache_source_a_fire_count[7] == 1 &&
                responder.uncache_source_d_fire_count[7] == 1,
                "out-of-order Uncache D completion released the wrong source");
    responder.note_uncache_source_complete(4'h3, 21);
    expect_true(responder.uncache_source_outstanding == '0 &&
                responder.uncache_source_a_fire_count[3] == 1 &&
                responder.uncache_source_d_fire_count[3] == 1,
                "Uncache source ledger did not close A-to-D ownership");
    responder.clear_runtime_state();
    expect_true(responder.uncache_source_outstanding == '0 &&
                responder.uncache_source_a_fire_count[3] == 0 &&
                responder.uncache_source_d_fire_count[7] == 0,
                "Uncache source ledger reset left stale ownership");

    responder.note_uncache_source_accept(4'h3, 48'h8000_0300, 30);
    catcher = new("uncache_source_reuse_catcher",
                  "*Uncache source reused before D completion source=3*");
    uvm_report_cb::add(null, catcher);
    responder.note_uncache_source_accept(4'h3, 48'h8000_0400, 31);
    uvm_report_cb::delete(null, catcher);
    expect_true(catcher.caught_count == 1,
                "same-source reuse did not trigger the expected owner fatal");

    responder.clear_runtime_state();
    catcher = new("uncache_no_owner_d_catcher",
                  "*Uncache D.fire has no accepted source owner source=5*");
    uvm_report_cb::add(null, catcher);
    responder.note_uncache_source_complete(4'h5, 40);
    uvm_report_cb::delete(null, catcher);
    expect_true(catcher.caught_count == 1,
                "ownerless D completion did not trigger the expected fatal");
    responder.clear_runtime_state();
endtask:check_uncache_source_lifecycle

`endif
