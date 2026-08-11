//=========================================================
//File name    : soft_test_l2tlb_range_lookup_sequence.sv
//Module name  : soft_test_l2tlb_range_lookup_sequence
//Discribution : software-only V2 L2TLB range index closure
//=========================================================
`ifndef SOFT_TEST_L2TLB_RANGE_LOOKUP_SEQUENCE__SV
`define SOFT_TEST_L2TLB_RANGE_LOOKUP_SEQUENCE__SV

class soft_test_l2tlb_range_lookup_sequence extends memblock_dispatch_base_sequence;

    `uvm_object_utils(soft_test_l2tlb_range_lookup_sequence)

    extern function new(string name = "soft_test_l2tlb_range_lookup_sequence");
    extern virtual task body();
    extern virtual function mmu_csr_runtime_state make_s1_csr_snapshot();
    extern virtual function memblock_tlb_entry make_s1_range_entry(
        input memblock_tlb_lookup_key_t anchor_key,
        input mmu_csr_runtime_state csr_snapshot,
        input bit [1:0] level,
        input bit napot);
    extern virtual function void install_s1_range_entry(
        input memblock_tlb_lookup_key_t anchor_key,
        input memblock_tlb_entry entry);
    extern virtual task check_napot_reuse_delete_and_rebuild();
    extern virtual task check_overlap_selects_widest_entry();

endclass:soft_test_l2tlb_range_lookup_sequence

function soft_test_l2tlb_range_lookup_sequence::new(
    string name = "soft_test_l2tlb_range_lookup_sequence");
    super.new(name);
endfunction:new

task soft_test_l2tlb_range_lookup_sequence::body();
    // 中文注释：该软件专项只验证 common_data 的 canonical-entry/index
    // 闭环，不驱动 L2TLB request/response wire，也不创建 token/UID owner。
    check_napot_reuse_delete_and_rebuild();
    check_overlap_selects_widest_entry();
    `uvm_info(get_type_name(), "V2 L2TLB range lookup software closure completed", UVM_LOW)
endtask:body

function mmu_csr_runtime_state
    soft_test_l2tlb_range_lookup_sequence::make_s1_csr_snapshot();
    mmu_csr_runtime_state csr_snapshot;

    csr_snapshot = mmu_csr_runtime_state::type_id::create("range_lookup_s1_csr");
    if (csr_snapshot == null) begin
        `uvm_fatal(get_type_name(), "failed to create range lookup CSR snapshot")
    end
    csr_snapshot.reset();
    csr_snapshot.satp_mode = 4'd8;
    csr_snapshot.satp_asid = 16'h0055;
    csr_snapshot.satp_ppn = 44'h0000_0001_234;
    csr_snapshot.update_seq = 1;
    return csr_snapshot;
endfunction:make_s1_csr_snapshot

function memblock_tlb_entry
    soft_test_l2tlb_range_lookup_sequence::make_s1_range_entry(
        input memblock_tlb_lookup_key_t anchor_key,
        input mmu_csr_runtime_state csr_snapshot,
        input bit [1:0] level,
        input bit napot);
    memblock_tlb_entry entry;

    if (csr_snapshot == null || anchor_key.s2xlate != 2'd0 ||
        (napot && level != 2'd0)) begin
        `uvm_fatal(get_type_name(), "invalid directed S1 range-entry request")
    end
    entry = memblock_tlb_entry::type_id::create(
        $sformatf("directed_range_entry_%0h_%0d_%0d", anchor_key.vpn, level, napot));
    if (entry == null) begin
        `uvm_fatal(get_type_name(), "failed to create directed S1 range entry")
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
    entry.s1_vmid = '0;
    entry.s1_level = level;
    entry.s1_pte_n = napot;
    entry.s1_entry_ppn_raw = 41'h0000_0000_001;
    entry.s1_pte_r = 1'b1;
    entry.s1_pte_w = 1'b0;
    entry.s1_pte_x = 1'b0;
    entry.s1_pte_u = 1'b0;
    entry.s1_pte_g = 1'b0;
    entry.s1_pte_a = 1'b1;
    entry.s1_pte_d = 1'b1;
    entry.s1_pte_v = 1'b1;
    entry.s1_entry_pbmt = '0;
    entry.s1_addr_low = anchor_key.vpn[2:0];
    foreach (entry.s1_ppn_low[idx]) begin
        entry.s1_ppn_low[idx] = '0;
        entry.s1_valididx[idx] = (level != 2'd0) || napot ||
                                  (idx == entry.s1_addr_low);
        entry.s1_pteidx[idx] = idx == entry.s1_addr_low;
    end
    return entry;
endfunction:make_s1_range_entry

function void soft_test_l2tlb_range_lookup_sequence::install_s1_range_entry(
    input memblock_tlb_lookup_key_t anchor_key,
    input memblock_tlb_entry entry);
    if (entry == null || entry.lookup_key != anchor_key ||
        data.has_tlb_entry(anchor_key)) begin
        `uvm_fatal(get_type_name(), "invalid directed S1 range-entry install")
    end
    data.insert_tlb_entry(anchor_key, entry);
    if (!data.register_tlb_range_index(anchor_key, entry)) begin
        `uvm_fatal(get_type_name(), "failed to register directed S1 range entry")
    end
endfunction:install_s1_range_entry

task soft_test_l2tlb_range_lookup_sequence::check_napot_reuse_delete_and_rebuild();
    mmu_csr_runtime_state csr_snapshot;
    memblock_tlb_lookup_key_t anchor_key;
    memblock_tlb_lookup_key_t request_key;
    memblock_tlb_lookup_key_t returned_anchor_key;
    memblock_tlb_entry anchor_entry;
    memblock_tlb_entry returned_entry;
    memblock_tlb_lookup_result_e lookup_result;
    bit created;
    bit found;
    bit [37:0] anchor_vpn;
    bit [37:0] range_vpn;
    longint unsigned old_generation;

    data.reset_all_tables(1);
    csr_snapshot = make_s1_csr_snapshot();
    anchor_vpn = 38'h000_0010_100;
    range_vpn = anchor_vpn + 38'd7;
    anchor_key = csr_snapshot.make_lookup_key({26'b0, anchor_vpn}, 2'd0);
    anchor_entry = make_s1_range_entry(anchor_key, csr_snapshot, 2'd0, 1'b1);
    old_generation = anchor_entry.entry_generation;
    install_s1_range_entry(anchor_key, anchor_entry);

    if (!data.get_or_create_tlb_entry_by_req_with_snapshot(
            range_vpn, 2'd0, csr_snapshot, request_key, returned_anchor_key,
            lookup_result, returned_entry, created)) begin
        `uvm_fatal(get_type_name(), "NAPOT range lookup did not return an entry")
    end
    if (created || lookup_result != MEMBLOCK_TLB_LOOKUP_RANGE_HIT ||
        returned_anchor_key != anchor_key || returned_entry != anchor_entry ||
        returned_entry.entry_generation != old_generation ||
        data.has_tlb_entry(request_key)) begin
        `uvm_fatal(get_type_name(),
                   "NAPOT exact-miss did not reuse the canonical raw entry")
    end

    data.delete_live_tlb_entry_by_anchor_key(anchor_key, "directed NAPOT delete");
    found = data.find_tlb_range_hit_by_req(request_key, csr_snapshot,
                                           returned_anchor_key, returned_entry);
    if (found || data.tlb_anchor_keys_by_range_key.num() != 0) begin
        `uvm_fatal(get_type_name(), "NAPOT delete left a discoverable range index entry")
    end
    if (!data.get_or_create_tlb_entry_by_req_with_snapshot(
            range_vpn, 2'd0, csr_snapshot, request_key, returned_anchor_key,
            lookup_result, returned_entry, created)) begin
        `uvm_fatal(get_type_name(), "NAPOT rebuild lookup did not return an entry")
    end
    if (!created || lookup_result != MEMBLOCK_TLB_LOOKUP_MISS_BUILD ||
        returned_anchor_key != request_key ||
        returned_entry.entry_generation == old_generation) begin
        `uvm_fatal(get_type_name(), "NAPOT delete did not force a new canonical generation")
    end
    data.clear_dispatch_l2tlb_live_entries();
endtask:check_napot_reuse_delete_and_rebuild

task soft_test_l2tlb_range_lookup_sequence::check_overlap_selects_widest_entry();
    mmu_csr_runtime_state csr_snapshot;
    memblock_tlb_lookup_key_t napot_anchor_key;
    memblock_tlb_lookup_key_t superpage_anchor_key;
    memblock_tlb_lookup_key_t request_key;
    memblock_tlb_lookup_key_t returned_anchor_key;
    memblock_tlb_entry napot_entry;
    memblock_tlb_entry superpage_entry;
    memblock_tlb_entry returned_entry;
    memblock_tlb_lookup_result_e lookup_result;
    bit created;
    bit [37:0] napot_anchor_vpn;
    bit [37:0] superpage_anchor_vpn;
    bit [37:0] request_vpn;

    data.reset_all_tables(1);
    csr_snapshot = make_s1_csr_snapshot();
    napot_anchor_vpn = 38'h000_0010_100;
    superpage_anchor_vpn = 38'h000_0011_000;
    request_vpn = napot_anchor_vpn + 38'd1;
    napot_anchor_key = csr_snapshot.make_lookup_key({26'b0, napot_anchor_vpn}, 2'd0);
    superpage_anchor_key = csr_snapshot.make_lookup_key(
        {26'b0, superpage_anchor_vpn}, 2'd0);
    napot_entry = make_s1_range_entry(napot_anchor_key, csr_snapshot, 2'd0, 1'b1);
    superpage_entry = make_s1_range_entry(superpage_anchor_key, csr_snapshot, 2'd2, 1'b0);
    install_s1_range_entry(napot_anchor_key, napot_entry);
    install_s1_range_entry(superpage_anchor_key, superpage_entry);

    if (!data.get_or_create_tlb_entry_by_req_with_snapshot(
            request_vpn, 2'd0, csr_snapshot, request_key, returned_anchor_key,
            lookup_result, returned_entry, created)) begin
        `uvm_fatal(get_type_name(), "overlap lookup did not return an entry")
    end
    if (created || lookup_result != MEMBLOCK_TLB_LOOKUP_RANGE_HIT ||
        returned_anchor_key != superpage_anchor_key ||
        returned_entry != superpage_entry) begin
        `uvm_fatal(get_type_name(),
                   "overlap lookup did not select the widest raw coverage entry")
    end
    data.clear_dispatch_l2tlb_live_entries();
endtask:check_overlap_selects_widest_entry

`endif
