//=========================================================
//File name    : soft_test_l2tlb_pbmt_csr_gate_sequence.sv
//Module name  : soft_test_l2tlb_pbmt_csr_gate_sequence
//Discribution : software-only V2 L2TLB PBMT/PBMTE closure
//=========================================================
`ifndef SOFT_TEST_L2TLB_PBMT_CSR_GATE_SEQUENCE__SV
`define SOFT_TEST_L2TLB_PBMT_CSR_GATE_SEQUENCE__SV

class soft_test_l2tlb_pbmt_csr_gate_sequence extends memblock_dispatch_base_sequence;

    `uvm_object_utils(soft_test_l2tlb_pbmt_csr_gate_sequence)

    tlb_map_builder                  builder;
    memblock_l2tlb_base_sequence     responder;

    extern function new(string name = "soft_test_l2tlb_pbmt_csr_gate_sequence");
    extern virtual task body();
    extern virtual function mmu_csr_runtime_state make_csr_snapshot(
        input bit [1:0] s2xlate,
        input bit       m_pbmt_en,
        input bit       h_pbmt_en);
    extern virtual function memblock_tlb_entry make_response_entry(
        input bit [1:0] s2xlate,
        input bit       s1_active,
        input bit       s2_active,
        input bit [1:0] s1_pbmt,
        input bit [1:0] s2_pbmt);
    extern virtual function void expect_true(input bit condition, input string message);
    extern virtual task check_stage_mapping();
    extern virtual task check_build_gate();
    extern virtual task check_response_overlay();

endclass:soft_test_l2tlb_pbmt_csr_gate_sequence

function soft_test_l2tlb_pbmt_csr_gate_sequence::new(
    string name = "soft_test_l2tlb_pbmt_csr_gate_sequence");
    super.new(name);
    builder = null;
    responder = null;
endfunction:new

task soft_test_l2tlb_pbmt_csr_gate_sequence::body();
    ensure_dispatch_runtime_helpers();
    builder = tlb_map_builder::type_id::create("pbmt_gate_builder");
    responder = memblock_l2tlb_base_sequence::type_id::create("pbmt_gate_responder");
    if (builder == null || responder == null) begin
        `uvm_fatal(get_type_name(), "failed to create PBMT software-test helpers")
    end
    check_stage_mapping();
    check_build_gate();
    check_response_overlay();
    `uvm_info(get_type_name(),
              "L2TLB PBMT/PBMTE software closure completed", UVM_LOW)
endtask:body

function mmu_csr_runtime_state
    soft_test_l2tlb_pbmt_csr_gate_sequence::make_csr_snapshot(
        input bit [1:0] s2xlate,
        input bit       m_pbmt_en,
        input bit       h_pbmt_en);
    mmu_csr_runtime_state csr_snapshot;

    csr_snapshot = mmu_csr_runtime_state::type_id::create(
        $sformatf("pbmt_gate_csr_%0d_%0d_%0d", s2xlate, m_pbmt_en, h_pbmt_en));
    if (csr_snapshot == null) begin
        `uvm_fatal(get_type_name(), "failed to create PBMT CSR snapshot")
    end
    csr_snapshot.reset();
    csr_snapshot.satp_mode = 4'd8;
    csr_snapshot.satp_asid = 16'h11;
    csr_snapshot.satp_ppn = 44'h1;
    csr_snapshot.vsatp_mode = (s2xlate == 2'd1 || s2xlate == 2'd3) ? 4'd8 : 4'd0;
    csr_snapshot.vsatp_asid = 16'h22;
    csr_snapshot.vsatp_ppn = 44'h2;
    csr_snapshot.hgatp_mode = (s2xlate == 2'd2 || s2xlate == 2'd3) ? 4'd8 : 4'd0;
    csr_snapshot.hgatp_vmid = 16'h3;
    csr_snapshot.hgatp_ppn = 44'h3;
    csr_snapshot.priv_virt = s2xlate != 2'd0;
    csr_snapshot.m_pbmt_en = m_pbmt_en;
    csr_snapshot.h_pbmt_en = h_pbmt_en;
    csr_snapshot.update_seq = 1;
    return csr_snapshot;
endfunction:make_csr_snapshot

function memblock_tlb_entry
    soft_test_l2tlb_pbmt_csr_gate_sequence::make_response_entry(
        input bit [1:0] s2xlate,
        input bit       s1_active,
        input bit       s2_active,
        input bit [1:0] s1_pbmt,
        input bit [1:0] s2_pbmt);
    memblock_tlb_entry entry;

    entry = memblock_tlb_entry::type_id::create(
        $sformatf("pbmt_response_entry_%0d", s2xlate));
    if (entry == null) begin
        `uvm_fatal(get_type_name(), "failed to create PBMT response entry")
    end
    entry.reset();
    entry.s2xlate = s2xlate;
    entry.s1_stage_active = s1_active;
    entry.s2_stage_active = s2_active;
    entry.s1_entry_pbmt = s1_pbmt;
    entry.s2_entry_pbmt = s2_pbmt;
    if (s1_active) begin
        // Keep the S1 sector shape valid so copy_from() can verify snapshot
        // ownership exactly as the responder does.
        entry.s1_addr_low = 3'd0;
        entry.s1_entry_ppn_raw = 41'h1;
        entry.s1_pte_r = 1'b1;
        entry.s1_pte_a = 1'b1;
        entry.s1_pte_d = 1'b1;
        entry.s1_pte_v = 1'b1;
        entry.s1_valididx[0] = 1'b1;
        entry.s1_pteidx[0] = 1'b1;
    end
    if (s2_active) begin
        entry.s2_entry_ppn_raw = 38'h1;
        entry.s2_pte_r = 1'b1;
        entry.s2_pte_a = 1'b1;
        entry.s2_pte_d = 1'b1;
    end
    return entry;
endfunction:make_response_entry

function void soft_test_l2tlb_pbmt_csr_gate_sequence::expect_true(
    input bit condition,
    input string message);
    if (!condition) begin
        `uvm_fatal(get_type_name(), message)
    end
endfunction:expect_true

task soft_test_l2tlb_pbmt_csr_gate_sequence::check_stage_mapping();
    expect_true(
        mmu_csr_runtime_state::get_stage_pbmt_enable_from_bits(1'b1, 2'd0, 1'b1, 1'b0) == 1'b1,
        "s2xlate=0 S1 must use mPBMTE");
    expect_true(
        mmu_csr_runtime_state::get_stage_pbmt_enable_from_bits(1'b1, 2'd1, 1'b0, 1'b1) == 1'b1,
        "s2xlate=1 S1 must use hPBMTE");
    expect_true(
        mmu_csr_runtime_state::get_stage_pbmt_enable_from_bits(1'b0, 2'd2, 1'b1, 1'b0) == 1'b1,
        "s2xlate=2 S2 must use mPBMTE");
    expect_true(
        mmu_csr_runtime_state::get_stage_pbmt_enable_from_bits(1'b1, 2'd3, 1'b0, 1'b1) == 1'b1,
        "allStage S1 must use hPBMTE");
    expect_true(
        mmu_csr_runtime_state::get_stage_pbmt_enable_from_bits(1'b0, 2'd3, 1'b1, 1'b0) == 1'b1,
        "allStage S2 must use mPBMTE");
    expect_true(
        mmu_csr_runtime_state::get_stage_pbmt_enable_from_bits(1'b0, 2'd0, 1'b1, 1'b1) == 1'b0,
        "inactive S2 in noS2xlate must not consume mPBMTE");
endtask:check_stage_mapping

task soft_test_l2tlb_pbmt_csr_gate_sequence::check_build_gate();
    mmu_csr_runtime_state csr_disabled;
    mmu_csr_runtime_state csr_enabled;
    bit [1:0] pbmt;

    if (seq_csr_common::get_l2tlb_pbmt_wt(1'b1, 1) == 0 &&
        seq_csr_common::get_l2tlb_pbmt_wt(1'b1, 2) == 0) begin
        `uvm_fatal(get_type_name(),
                   "software PBMT gate test requires a non-zero S1 nonzero PBMT weight")
    end
    if (seq_csr_common::get_l2tlb_pbmt_wt(1'b0, 1) == 0 &&
        seq_csr_common::get_l2tlb_pbmt_wt(1'b0, 2) == 0) begin
        `uvm_fatal(get_type_name(),
                   "software PBMT gate test requires a non-zero S2 nonzero PBMT weight")
    end

    csr_disabled = make_csr_snapshot(2'd0, 1'b0, 1'b0);
    csr_enabled = make_csr_snapshot(2'd0, 1'b1, 1'b1);
    pbmt = builder.choose_pbmt(1'b1, 2'd0, csr_disabled);
    expect_true(pbmt == 2'd0, "disabled S1 PBMTE must force build PBMT=00");
    builder.check_pbmt_build_csr_compatibility(1'b1, 2'd0, csr_disabled, pbmt);
    pbmt = builder.choose_pbmt(1'b1, 2'd0, csr_enabled);
    expect_true(pbmt != 2'd0, "enabled S1 PBMTE must allow configured nonzero PBMT");
    builder.check_pbmt_build_csr_compatibility(1'b1, 2'd0, csr_enabled, pbmt);

    csr_disabled = make_csr_snapshot(2'd2, 1'b0, 1'b0);
    csr_enabled = make_csr_snapshot(2'd2, 1'b1, 1'b1);
    pbmt = builder.choose_pbmt(1'b0, 2'd2, csr_disabled);
    expect_true(pbmt == 2'd0, "disabled S2 PBMTE must force build PBMT=00");
    builder.check_pbmt_build_csr_compatibility(1'b0, 2'd2, csr_disabled, pbmt);
    pbmt = builder.choose_pbmt(1'b0, 2'd2, csr_enabled);
    expect_true(pbmt != 2'd0, "enabled S2 PBMTE must allow configured nonzero PBMT");
    builder.check_pbmt_build_csr_compatibility(1'b0, 2'd2, csr_enabled, pbmt);
endtask:check_build_gate

task soft_test_l2tlb_pbmt_csr_gate_sequence::check_response_overlay();
    mmu_csr_runtime_state csr;
    memblock_tlb_entry raw_entry;
    memblock_tlb_entry disabled_token;
    memblock_tlb_entry enabled_token;
    bit force_s1_pf;
    bit force_s2_gpf;

    raw_entry = make_response_entry(2'd0, 1'b1, 1'b0, 2'd1, 2'd0);
    raw_entry.fault_effective_s1_af = 1'b1;
    csr = make_csr_snapshot(2'd0, 1'b0, 1'b1);
    responder.apply_pbmt_response_overlay(raw_entry, csr, force_s1_pf, force_s2_gpf);
    expect_true(force_s1_pf && !force_s2_gpf,
                "disabled noS2 S1 PBMT must force S1 PF only");
    expect_true(raw_entry.fault_effective_s1_pf && raw_entry.fault_effective_s1_af,
                "S1 PBMT overlay must OR with existing S1 AF");
    expect_true(raw_entry.s1_entry_pbmt == 2'd1,
                "S1 PBMT must remain visible after overlay");

    raw_entry = make_response_entry(2'd1, 1'b1, 1'b0, 2'd2, 2'd0);
    csr = make_csr_snapshot(2'd1, 1'b1, 1'b0);
    responder.apply_pbmt_response_overlay(raw_entry, csr, force_s1_pf, force_s2_gpf);
    expect_true(force_s1_pf && !force_s2_gpf,
                "onlyStage1 S1 PBMT must use hPBMTE");

    raw_entry = make_response_entry(2'd2, 1'b0, 1'b1, 2'd0, 2'd1);
    raw_entry.fault_effective_s2_gaf = 1'b1;
    csr = make_csr_snapshot(2'd2, 1'b0, 1'b1);
    responder.apply_pbmt_response_overlay(raw_entry, csr, force_s1_pf, force_s2_gpf);
    expect_true(!force_s1_pf && force_s2_gpf,
                "onlyStage2 S2 PBMT must force S2 GPF");
    expect_true(raw_entry.fault_effective_s2_gpf && raw_entry.fault_effective_s2_gaf,
                "S2 PBMT overlay must OR with existing S2 GAF");

    raw_entry = make_response_entry(2'd3, 1'b1, 1'b1, 2'd1, 2'd2);
    raw_entry.fault_effective_s1_pf = 1'b1;
    raw_entry.fault_effective_s2_gpf = 1'b1;
    csr = make_csr_snapshot(2'd3, 1'b0, 1'b0);
    responder.apply_pbmt_response_overlay(raw_entry, csr, force_s1_pf, force_s2_gpf);
    expect_true(force_s1_pf && force_s2_gpf,
                "allStage disabled PBMTE must force both stages independently");
    expect_true(raw_entry.fault_effective_s1_pf && raw_entry.fault_effective_s2_gpf,
                "allStage overlay must preserve existing PF/GPF");

    raw_entry = make_response_entry(2'd3, 1'b1, 1'b1, 2'd1, 2'd1);
    csr = make_csr_snapshot(2'd3, 1'b1, 1'b0);
    responder.apply_pbmt_response_overlay(raw_entry, csr, force_s1_pf, force_s2_gpf);
    expect_true(force_s1_pf && !force_s2_gpf,
                "allStage h=0,m=1 must fault S1 only");
    csr = make_csr_snapshot(2'd3, 1'b0, 1'b1);
    raw_entry = make_response_entry(2'd3, 1'b1, 1'b1, 2'd1, 2'd1);
    responder.apply_pbmt_response_overlay(raw_entry, csr, force_s1_pf, force_s2_gpf);
    expect_true(!force_s1_pf && force_s2_gpf,
                "allStage h=1,m=0 must fault S2 only");

    // Two detached token snapshots must not share the first token's overlay.
    raw_entry = make_response_entry(2'd0, 1'b1, 1'b0, 2'd1, 2'd0);
    disabled_token = memblock_tlb_entry::type_id::create("pbmt_disabled_token");
    enabled_token = memblock_tlb_entry::type_id::create("pbmt_enabled_token");
    disabled_token.copy_from(raw_entry);
    enabled_token.copy_from(raw_entry);
    responder.apply_pbmt_response_overlay(
        disabled_token, make_csr_snapshot(2'd0, 1'b0, 1'b0),
        force_s1_pf, force_s2_gpf);
    expect_true(force_s1_pf && disabled_token.fault_effective_s1_pf,
                "first disabled token must carry S1 PF overlay");
    responder.apply_pbmt_response_overlay(
        enabled_token, make_csr_snapshot(2'd0, 1'b1, 1'b0),
        force_s1_pf, force_s2_gpf);
    expect_true(!force_s1_pf && !enabled_token.fault_effective_s1_pf,
                "re-enabled second token must not inherit first overlay");
    expect_true(raw_entry.fault_effective_s1_pf == 1'b0 &&
                raw_entry.s1_entry_pbmt == 2'd1,
                "live raw entry must remain unchanged by token overlays");

    raw_entry = make_response_entry(2'd0, 1'b1, 1'b0, 2'd0, 2'd0);
    responder.apply_pbmt_response_overlay(
        raw_entry, make_csr_snapshot(2'd0, 1'b0, 1'b0),
        force_s1_pf, force_s2_gpf);
    expect_true(!force_s1_pf && !force_s2_gpf,
                "PBMT=00 must never create PBMT overlay fault");
endtask:check_response_overlay

`endif
