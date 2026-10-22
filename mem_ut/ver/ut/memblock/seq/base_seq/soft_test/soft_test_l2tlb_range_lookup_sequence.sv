//=========================================================
//File name    : soft_test_l2tlb_range_lookup_sequence.sv
//Module name  : soft_test_l2tlb_range_lookup_sequence
//Discribution : software-only V2 L2TLB range index closure
//=========================================================
`ifndef SOFT_TEST_L2TLB_RANGE_LOOKUP_SEQUENCE__SV
`define SOFT_TEST_L2TLB_RANGE_LOOKUP_SEQUENCE__SV

class memblock_l2tlb_overlap_expected_fatal_catcher extends uvm_report_catcher;
    int unsigned caught_count;

    function new(string name = "memblock_l2tlb_overlap_expected_fatal_catcher");
        super.new(name);
        caught_count = 0;
    endfunction:new

    virtual function action_e catch();
        if (get_severity() == UVM_FATAL && get_id() == "COMMON_DATA" &&
            uvm_pkg::uvm_is_match("*illegal overlapping L2TLB live hit*",
                                  get_message())) begin
            caught_count++;
            return CAUGHT;
        end
        return THROW;
    endfunction:catch
endclass:memblock_l2tlb_overlap_expected_fatal_catcher

class memblock_l2tlb_prepared_contract_expected_fatal_catcher extends uvm_report_catcher;
    int unsigned caught_count;

    function new(string name = "memblock_l2tlb_prepared_contract_expected_fatal_catcher");
        super.new(name);
        caught_count = 0;
    endfunction:new

    virtual function action_e catch();
        if (get_severity() == UVM_FATAL && get_id() == "COMMON_DATA" &&
            uvm_pkg::uvm_is_match("*global fallback requires a detached prepared entry*",
                                  get_message())) begin
            caught_count++;
            return CAUGHT;
        end
        return THROW;
    endfunction:catch
endclass:memblock_l2tlb_prepared_contract_expected_fatal_catcher

class memblock_l2tlb_fallback_warning_catcher extends uvm_report_catcher;
    int unsigned caught_count;

    function new(string name = "memblock_l2tlb_fallback_warning_catcher");
        super.new(name);
        caught_count = 0;
    endfunction:new

    virtual function action_e catch();
        if (get_severity() == UVM_WARNING &&
            get_id() == "L2TLB_SHAPE_FALLBACK") begin
            caught_count++;
            return CAUGHT;
        end
        return THROW;
    endfunction:catch
endclass:memblock_l2tlb_fallback_warning_catcher

class soft_test_l2tlb_range_lookup_sequence extends memblock_dispatch_base_sequence;

    `uvm_object_utils(soft_test_l2tlb_range_lookup_sequence)

    extern function new(string name = "soft_test_l2tlb_range_lookup_sequence");
    extern virtual task body();
    extern virtual function mmu_csr_runtime_state make_s1_csr_snapshot();
    extern virtual function mmu_csr_runtime_state make_stage_csr_snapshot(
        input bit [1:0] s2xlate,
        input bit [15:0] asid,
        input bit [15:0] vmid);
    extern virtual function memblock_tlb_entry make_stage_range_entry(
        input memblock_tlb_lookup_key_t anchor_key,
        input mmu_csr_runtime_state csr_snapshot,
        input bit [1:0] level,
        input bit napot,
        input bit global_mapping = 1'b0);
    extern virtual function memblock_tlb_entry make_stage_prepared_entry(
        input memblock_tlb_lookup_key_t anchor_key,
        input mmu_csr_runtime_state csr_snapshot,
        input bit global_mapping = 1'b0);
    extern virtual function memblock_tlb_entry make_s1_range_entry(
        input memblock_tlb_lookup_key_t anchor_key,
        input mmu_csr_runtime_state csr_snapshot,
        input bit [1:0] level,
        input bit napot,
        input bit global_mapping = 1'b0);
    extern virtual function void install_range_entry(
        input memblock_tlb_lookup_key_t anchor_key,
        input memblock_tlb_entry entry,
        input mmu_csr_runtime_state csr_snapshot);
    extern virtual function void shape_to_level_n(
        input memblock_tlb_range_coverage_rank_e shape,
        output bit [1:0] level,
        output bit napot);
    extern virtual function void inject_live_overlap_for_expected_fatal_test_only(
        input memblock_tlb_lookup_key_t first_anchor_key,
        input memblock_tlb_entry first_entry,
        input memblock_tlb_lookup_key_t second_anchor_key,
        input memblock_tlb_entry second_entry);
    extern virtual task check_napot_reuse_delete_and_rebuild();
    extern virtual task check_parent_overlap_shape_matrix();
    extern virtual task check_disjoint_regions_remain_allowed();
    extern virtual task check_global_cross_asid_overlap();
    extern virtual task check_stage_context_overlap_semantics();
    extern virtual task check_allstage_shape_and_fallback_coverage();
    extern virtual task check_parent_ownership_lifecycle();
    extern virtual task check_finalized_detached_fallback_rejected();
    extern virtual task check_illegal_overlap_fails_fast();

endclass:soft_test_l2tlb_range_lookup_sequence

function soft_test_l2tlb_range_lookup_sequence::new(
    string name = "soft_test_l2tlb_range_lookup_sequence");
    super.new(name);
endfunction:new

task soft_test_l2tlb_range_lookup_sequence::body();
    // 中文注释：该软件专项只验证 common_data 的 canonical-entry/index
    // 闭环，不驱动 L2TLB request/response wire，也不创建 token/UID owner。
    check_napot_reuse_delete_and_rebuild();
    check_parent_overlap_shape_matrix();
    check_disjoint_regions_remain_allowed();
    check_global_cross_asid_overlap();
    check_stage_context_overlap_semantics();
    check_allstage_shape_and_fallback_coverage();
    check_parent_ownership_lifecycle();
    check_finalized_detached_fallback_rejected();
    check_illegal_overlap_fails_fast();
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

function mmu_csr_runtime_state
    soft_test_l2tlb_range_lookup_sequence::make_stage_csr_snapshot(
        input bit [1:0] s2xlate,
        input bit [15:0] asid,
        input bit [15:0] vmid);
    mmu_csr_runtime_state csr_snapshot;

    csr_snapshot = mmu_csr_runtime_state::type_id::create(
        $sformatf("range_lookup_stage_csr_%0d_%0h_%0h", s2xlate, asid, vmid));
    if (csr_snapshot == null) begin
        `uvm_fatal(get_type_name(), "failed to create stage CSR snapshot")
    end
    csr_snapshot.reset();
    csr_snapshot.satp_mode = s2xlate == 2'd0 ? 4'd9 : 4'd0;
    csr_snapshot.satp_asid = asid;
    csr_snapshot.satp_ppn = 44'h0000_0001_234;
    csr_snapshot.vsatp_mode = (s2xlate == 2'd1 || s2xlate == 2'd3) ?
                              4'd9 : 4'd0;
    csr_snapshot.vsatp_asid = asid;
    csr_snapshot.vsatp_ppn = 44'h0000_0002_345;
    csr_snapshot.hgatp_mode = (s2xlate == 2'd2 || s2xlate == 2'd3) ?
                              4'd9 : 4'd0;
    csr_snapshot.hgatp_vmid = vmid;
    csr_snapshot.hgatp_ppn = 44'h0000_0003_456;
    csr_snapshot.priv_virt = s2xlate != 2'd0;
    csr_snapshot.update_seq = 1;
    if (csr_snapshot.expected_s2xlate(1'b0) != s2xlate) begin
        `uvm_fatal(get_type_name(), "directed CSR snapshot cannot produce requested s2xlate")
    end
    return csr_snapshot;
endfunction:make_stage_csr_snapshot

function memblock_tlb_entry
    soft_test_l2tlb_range_lookup_sequence::make_stage_prepared_entry(
        input memblock_tlb_lookup_key_t anchor_key,
        input mmu_csr_runtime_state csr_snapshot,
        input bit global_mapping = 1'b0);
    memblock_tlb_entry entry;
    tlb_map_builder builder;

    if (csr_snapshot == null) begin
        `uvm_fatal(get_type_name(), "invalid directed prepared-entry request")
    end
    builder = tlb_map_builder::type_id::create("directed_range_builder");
    if (builder == null) begin
        `uvm_fatal(get_type_name(), "failed to create directed range builder")
    end
    entry = builder.prepare_tlb_payload_context(anchor_key, csr_snapshot);
    if (entry == null || entry.payload_phase !=
        memblock_tlb_entry::MEMBLOCK_TLB_PAYLOAD_PREPARED) begin
        `uvm_fatal(get_type_name(), "builder did not return a prepared directed entry")
    end
    entry.fault_raw_s1_pf = 1'b0;
    entry.fault_raw_s1_af = 1'b0;
    entry.fault_raw_s2_gpf = 1'b0;
    entry.fault_raw_s2_gaf = 1'b0;
    entry.fault_effective_s1_pf = 1'b0;
    entry.fault_effective_s1_af = 1'b0;
    entry.fault_effective_s2_gpf = 1'b0;
    entry.fault_effective_s2_gaf = 1'b0;
    entry.fault_stage_selected = memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_NONE;
    entry.pmaAF = 1'b0;

    if (entry.s1_stage_active) begin
        entry.s1_pte_mode_at_build = memblock_tlb_entry::MEMBLOCK_TLB_PTE_MODE_LEGAL;
        entry.s1_pte_r = 1'b1;
        entry.s1_pte_w = 1'b0;
        entry.s1_pte_x = 1'b0;
        entry.s1_pte_u = 1'b0;
        entry.s1_pte_g = global_mapping;
        entry.s1_pte_a = 1'b1;
        entry.s1_pte_d = 1'b1;
        entry.s1_pte_v = 1'b1;
    end
    if (entry.s2_stage_active) begin
        entry.s2_pte_mode_at_build = memblock_tlb_entry::MEMBLOCK_TLB_PTE_MODE_LEGAL;
        entry.s2_pte_r = 1'b1;
        entry.s2_pte_w = 1'b0;
        entry.s2_pte_x = 1'b0;
        entry.s2_pte_u = 1'b0;
        entry.s2_pte_g = 1'b0;
        entry.s2_pte_a = 1'b1;
        entry.s2_pte_d = 1'b1;
    end
    return entry;
endfunction:make_stage_prepared_entry

function memblock_tlb_entry
    soft_test_l2tlb_range_lookup_sequence::make_stage_range_entry(
        input memblock_tlb_lookup_key_t anchor_key,
        input mmu_csr_runtime_state csr_snapshot,
        input bit [1:0] level,
        input bit napot,
        input bit global_mapping = 1'b0);
    memblock_tlb_entry entry;
    tlb_map_builder builder;
    memblock_tlb_range_coverage_rank_e shape;
    bit [4:0] allowed_shape_mask;
    bit pte_n_overridden;
    bit forced_4k;

    if (csr_snapshot == null || (napot && level != 2'd0)) begin
        `uvm_fatal(get_type_name(), "invalid directed stage range-entry request")
    end
    if (napot) begin
        shape = MEMBLOCK_TLB_COVERAGE_64K;
    end
    else begin
        case (level)
            2'd0: shape = MEMBLOCK_TLB_COVERAGE_4K;
            2'd1: shape = MEMBLOCK_TLB_COVERAGE_2M;
            2'd2: shape = MEMBLOCK_TLB_COVERAGE_1G;
            2'd3: shape = MEMBLOCK_TLB_COVERAGE_512G;
            default: `uvm_fatal(get_type_name(), "invalid directed range level")
        endcase
    end
    entry = make_stage_prepared_entry(anchor_key, csr_snapshot, global_mapping);
    builder = tlb_map_builder::type_id::create("directed_range_finalizer");
    if (builder == null) begin
        `uvm_fatal(get_type_name(), "failed to create directed range finalizer")
    end
    allowed_shape_mask = '0;
    allowed_shape_mask[shape] = 1'b1;
    entry = builder.finalize_tlb_payload_shape_and_payload(
        entry, anchor_key, csr_snapshot, allowed_shape_mask,
        pte_n_overridden, forced_4k, 1'b1, shape);
    if (entry == null || pte_n_overridden || forced_4k ||
        entry.payload_phase != memblock_tlb_entry::MEMBLOCK_TLB_PAYLOAD_FINALIZED ||
        (entry.s1_stage_active && !entry.s1_resolved_ppn_valid) ||
        (entry.s2_stage_active && !entry.s2_resolved_ppn_valid)) begin
        `uvm_fatal(get_type_name(), "directed builder did not create a complete final payload")
    end
    entry.entry_generation = data.allocate_tlb_entry_generation();
    return entry;
endfunction:make_stage_range_entry

function memblock_tlb_entry
    soft_test_l2tlb_range_lookup_sequence::make_s1_range_entry(
        input memblock_tlb_lookup_key_t anchor_key,
        input mmu_csr_runtime_state csr_snapshot,
        input bit [1:0] level,
        input bit napot,
        input bit global_mapping = 1'b0);
    if (csr_snapshot == null || anchor_key.s2xlate != 2'd0 ||
        (napot && level != 2'd0)) begin
        `uvm_fatal(get_type_name(), "invalid directed S1 range-entry request")
    end
    return make_stage_range_entry(anchor_key, csr_snapshot, level, napot,
                                  global_mapping);
endfunction:make_s1_range_entry

function void soft_test_l2tlb_range_lookup_sequence::install_range_entry(
    input memblock_tlb_lookup_key_t anchor_key,
    input memblock_tlb_entry entry,
    input mmu_csr_runtime_state csr_snapshot);
    if (entry == null || csr_snapshot == null || entry.lookup_key != anchor_key ||
        data.has_tlb_entry(anchor_key)) begin
        `uvm_fatal(get_type_name(), "invalid directed S1 range-entry install")
    end
    data.publish_live_tlb_entry(anchor_key, entry, csr_snapshot);
endfunction:install_range_entry

function void soft_test_l2tlb_range_lookup_sequence::shape_to_level_n(
    input memblock_tlb_range_coverage_rank_e shape,
    output bit [1:0] level,
    output bit napot);
    napot = 1'b0;
    case (shape)
        MEMBLOCK_TLB_COVERAGE_4K: level = 2'd0;
        MEMBLOCK_TLB_COVERAGE_64K: begin level = 2'd0; napot = 1'b1; end
        MEMBLOCK_TLB_COVERAGE_2M: level = 2'd1;
        MEMBLOCK_TLB_COVERAGE_1G: level = 2'd2;
        MEMBLOCK_TLB_COVERAGE_512G: level = 2'd3;
        default: `uvm_fatal(get_type_name(), "invalid directed coverage shape")
    endcase
endfunction:shape_to_level_n

function void
    soft_test_l2tlb_range_lookup_sequence::inject_live_overlap_for_expected_fatal_test_only(
        input memblock_tlb_lookup_key_t first_anchor_key,
        input memblock_tlb_entry first_entry,
        input memblock_tlb_lookup_key_t second_anchor_key,
        input memblock_tlb_entry second_entry);
    if (first_entry == null || second_entry == null ||
        first_entry.lookup_key != first_anchor_key ||
        second_entry.lookup_key != second_anchor_key ||
        first_entry.payload_phase !=
            memblock_tlb_entry::MEMBLOCK_TLB_PAYLOAD_FINALIZED ||
        second_entry.payload_phase !=
            memblock_tlb_entry::MEMBLOCK_TLB_PAYLOAD_FINALIZED ||
        data.has_tlb_entry(first_anchor_key) ||
        data.has_tlb_entry(second_anchor_key)) begin
        `uvm_fatal(get_type_name(), "invalid test-only live overlap injection")
    end

    // 中文注释：本helper只为覆盖生产publish已禁止的多命中防御分支。
    // 故意只安装canonical/range状态，不登记parent ownership；调用者必须整表reset。
    data.insert_tlb_entry(first_anchor_key, first_entry);
    if (!data.register_tlb_range_index(first_anchor_key, first_entry)) begin
        `uvm_fatal(get_type_name(), "test-only first overlap injection failed")
    end
    data.insert_tlb_entry(second_anchor_key, second_entry);
    if (!data.register_tlb_range_index(second_anchor_key, second_entry)) begin
        `uvm_fatal(get_type_name(), "test-only second overlap injection failed")
    end
endfunction:inject_live_overlap_for_expected_fatal_test_only

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
    install_range_entry(anchor_key, anchor_entry, csr_snapshot);

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

task soft_test_l2tlb_range_lookup_sequence::check_parent_overlap_shape_matrix();
    mmu_csr_runtime_state csr_snapshot;
    memblock_tlb_lookup_key_t child_key;
    memblock_tlb_lookup_key_t parent_key;
    memblock_tlb_entry child_entry;
    memblock_tlb_entry parent_entry;
    bit [4:0] allowed_shape_mask;
    bit [1:0] child_level;
    bit [1:0] parent_level;
    bit child_napot;
    bit parent_napot;
    int unsigned matrix_count;

    csr_snapshot = make_s1_csr_snapshot();
    csr_snapshot.satp_mode = 4'd9;
    matrix_count = 0;
    for (int unsigned child_idx = int'(MEMBLOCK_TLB_COVERAGE_4K);
         child_idx < int'(MEMBLOCK_TLB_COVERAGE_512G); child_idx++) begin
        for (int unsigned parent_idx = child_idx + 1;
             parent_idx <= int'(MEMBLOCK_TLB_COVERAGE_512G); parent_idx++) begin
            memblock_tlb_range_coverage_rank_e child_shape;
            memblock_tlb_range_coverage_rank_e parent_shape;
            bit [37:0] region_base_vpn;

            data.reset_all_tables(1);
            child_shape = memblock_tlb_range_coverage_rank_e'(child_idx);
            parent_shape = memblock_tlb_range_coverage_rank_e'(parent_idx);
            region_base_vpn = 38'h000_0020_000 + (matrix_count << 18);
            shape_to_level_n(child_shape, child_level, child_napot);
            shape_to_level_n(parent_shape, parent_level, parent_napot);
            child_key = csr_snapshot.make_lookup_key(
                {26'b0, region_base_vpn + 38'd1}, 2'd0);
            parent_key = csr_snapshot.make_lookup_key(
                {26'b0, region_base_vpn + 38'd2}, 2'd0);
            child_entry = make_s1_range_entry(
                child_key, csr_snapshot, child_level, child_napot);
            parent_entry = make_s1_range_entry(
                parent_key, csr_snapshot, parent_level, parent_napot);
            install_range_entry(child_key, child_entry, csr_snapshot);

            allowed_shape_mask = data.derive_allowed_tlb_shape_mask(
                parent_key.vpn, parent_entry);
            if (allowed_shape_mask[parent_idx]) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("parent overlap matrix failed child=%0d parent=%0d",
                                     child_idx, parent_idx))
            end
            data.delete_live_tlb_entry_by_anchor_key(
                child_key, "directed parent matrix child cleanup");
            allowed_shape_mask = data.derive_allowed_tlb_shape_mask(
                parent_key.vpn, parent_entry);
            if (!allowed_shape_mask[parent_idx] ||
                data.tlb_parent_occupancy_count.num() != 0 ||
                data.tlb_parent_owner_set.num() != 0) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("parent ownership did not clear child=%0d parent=%0d",
                                     child_idx, parent_idx))
            end
            matrix_count++;
        end
    end
    if (matrix_count != 10) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("parent overlap matrix count=%0d expected=10",
                             matrix_count))
    end
endtask:check_parent_overlap_shape_matrix

task soft_test_l2tlb_range_lookup_sequence::check_disjoint_regions_remain_allowed();
    mmu_csr_runtime_state csr_snapshot;
    memblock_tlb_lookup_key_t owner_key;
    memblock_tlb_lookup_key_t candidate_key;
    memblock_tlb_entry owner_entry;
    memblock_tlb_entry candidate_entry;
    bit [4:0] allowed_shape_mask;
    bit [37:0] region_base_vpn;

    data.reset_all_tables(1);
    csr_snapshot = make_s1_csr_snapshot();
    csr_snapshot.satp_mode = 4'd9;
    region_base_vpn = 38'h000_0040_000;
    owner_key = csr_snapshot.make_lookup_key(
        {26'b0, region_base_vpn + 38'd1}, 2'd0);
    owner_entry = make_s1_range_entry(
        owner_key, csr_snapshot, 2'd0, 1'b0);
    install_range_entry(owner_key, owner_entry, csr_snapshot);

    candidate_key = csr_snapshot.make_lookup_key(
        {26'b0, region_base_vpn + 38'd2}, 2'd0);
    candidate_entry = make_s1_range_entry(
        candidate_key, csr_snapshot, 2'd0, 1'b0);
    allowed_shape_mask = data.derive_allowed_tlb_shape_mask(
        candidate_key.vpn, candidate_entry);
    if (!allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_4K]) begin
        `uvm_fatal(get_type_name(),
                   "disjoint 4KB candidate was blocked by a sibling 4KB owner")
    end

    candidate_key = csr_snapshot.make_lookup_key(
        {26'b0, region_base_vpn + 38'd512 + 38'd1}, 2'd0);
    candidate_entry = make_s1_range_entry(
        candidate_key, csr_snapshot, 2'd1, 1'b0);
    allowed_shape_mask = data.derive_allowed_tlb_shape_mask(
        candidate_key.vpn, candidate_entry);
    if (!allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_2M] ||
        allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_1G]) begin
        `uvm_fatal(get_type_name(),
                   "sibling 2MB candidate was blocked or containing 1GB candidate escaped")
    end

    data.delete_live_tlb_entry_by_anchor_key(
        owner_key, "directed disjoint-region cleanup");
    if (data.tlb_parent_occupancy_count.num() != 0 ||
        data.tlb_parent_owner_set.num() != 0) begin
        `uvm_fatal(get_type_name(),
                   "disjoint-region owner cleanup left occupancy state")
    end

    // Exercise owner_shape > candidate_shape: an internal 4KB candidate must
    // be blocked, while a sibling 4KB outside the owner's real 1GB range stays legal.
    data.reset_all_tables(1);
    region_base_vpn = 38'h000_0080_000;
    owner_key = csr_snapshot.make_lookup_key(
        {26'b0, region_base_vpn + 38'd1}, 2'd0);
    owner_entry = make_s1_range_entry(
        owner_key, csr_snapshot, 2'd2, 1'b0);
    install_range_entry(owner_key, owner_entry, csr_snapshot);

    candidate_key = csr_snapshot.make_lookup_key(
        {26'b0, region_base_vpn + 38'd2}, 2'd0);
    candidate_entry = make_s1_range_entry(
        candidate_key, csr_snapshot, 2'd0, 1'b0);
    allowed_shape_mask = data.derive_allowed_tlb_shape_mask(
        candidate_key.vpn, candidate_entry);
    if (allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_4K]) begin
        `uvm_fatal(get_type_name(),
                   "4KB candidate inside an existing 1GB owner was not blocked")
    end

    candidate_key = csr_snapshot.make_lookup_key(
        {26'b0, region_base_vpn + (38'd1 << 18)}, 2'd0);
    candidate_entry = make_s1_range_entry(
        candidate_key, csr_snapshot, 2'd0, 1'b0);
    allowed_shape_mask = data.derive_allowed_tlb_shape_mask(
        candidate_key.vpn, candidate_entry);
    if (!allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_4K]) begin
        `uvm_fatal(get_type_name(),
                   "4KB candidate outside an existing 1GB owner was falsely blocked")
    end
    data.delete_live_tlb_entry_by_anchor_key(
        owner_key, "directed larger-owner cleanup");
endtask:check_disjoint_regions_remain_allowed

task soft_test_l2tlb_range_lookup_sequence::check_global_cross_asid_overlap();
    mmu_csr_runtime_state owner_csr;
    mmu_csr_runtime_state global_csr;
    memblock_tlb_lookup_key_t owner_key;
    memblock_tlb_lookup_key_t global_key;
    memblock_tlb_entry owner_entry;
    memblock_tlb_entry global_entry;
    bit [4:0] allowed_shape_mask;
    bit pte_g_overridden;
    bit [37:0] region_base_vpn;

    data.reset_all_tables(1);
    owner_csr = make_s1_csr_snapshot();
    owner_csr.satp_asid = 16'h0011;
    global_csr = make_s1_csr_snapshot();
    global_csr.satp_asid = 16'h0022;
    region_base_vpn = 38'h000_0030_000;
    owner_key = owner_csr.make_lookup_key(
        {26'b0, region_base_vpn + 38'd1}, 2'd0);
    global_key = global_csr.make_lookup_key(
        {26'b0, region_base_vpn + 38'd2}, 2'd0);
    owner_entry = make_s1_range_entry(
        owner_key, owner_csr, 2'd1, 1'b0);
    global_entry = make_stage_prepared_entry(
        global_key, global_csr, 1'b1);
    install_range_entry(owner_key, owner_entry, owner_csr);

    allowed_shape_mask = data.derive_allowed_tlb_shape_mask(
        global_key.vpn, global_entry);
    if (allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_2M] ||
        allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_4K]) begin
        `uvm_fatal(get_type_name(),
                   "global candidate crossed ASID and bypassed same/larger owner occupancy")
    end
    allowed_shape_mask = data.derive_allowed_tlb_shape_mask_with_global_fallback(
        global_key.vpn, global_entry, pte_g_overridden);
    if (!pte_g_overridden || global_entry.s1_pte_g ||
        !allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_2M] ||
        !allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_4K]) begin
        `uvm_fatal(get_type_name(),
                   "global cross-ASID fallback did not restore a non-global candidate")
    end
    if (!data.tlb_entry_by_key.exists(owner_key) ||
        data.tlb_entry_by_key[owner_key] != owner_entry) begin
        `uvm_fatal(get_type_name(),
                   "global fallback modified or deleted the existing owner")
    end
    global_entry.s1_pte_g = 1'b1;
    data.delete_live_tlb_entry_by_anchor_key(
        owner_key, "directed global cross-ASID cleanup");
    allowed_shape_mask = data.derive_allowed_tlb_shape_mask(
        global_key.vpn, global_entry);
    if (!allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_2M] ||
        !allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_4K]) begin
        `uvm_fatal(get_type_name(),
                   "global candidate remained blocked after owner deletion")
    end
    data.clear_dispatch_l2tlb_live_entries();

    data.reset_all_tables(1);
    global_csr.satp_asid = owner_csr.satp_asid;
    owner_key = owner_csr.make_lookup_key(
        {26'b0, region_base_vpn + 38'd1}, 2'd0);
    global_key = global_csr.make_lookup_key(
        {26'b0, region_base_vpn + 38'd2}, 2'd0);
    owner_entry = make_s1_range_entry(
        owner_key, owner_csr, 2'd1, 1'b0);
    global_entry = make_stage_prepared_entry(
        global_key, global_csr, 1'b1);
    install_range_entry(owner_key, owner_entry, owner_csr);
    allowed_shape_mask = data.derive_allowed_tlb_shape_mask_with_global_fallback(
        global_key.vpn, global_entry, pte_g_overridden);
    if (!pte_g_overridden || global_entry.s1_pte_g ||
        allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_4K] ||
        allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_2M]) begin
        `uvm_fatal(get_type_name(),
                   "same-ASID owner was bypassed after global fallback")
    end
    data.delete_live_tlb_entry_by_anchor_key(
        owner_key, "directed same-ASID owner cleanup");

    data.reset_all_tables(1);
    global_csr.satp_asid = 16'h0022;
    owner_key = owner_csr.make_lookup_key(
        {26'b0, region_base_vpn + 38'd1}, 2'd0);
    global_key = global_csr.make_lookup_key(
        {26'b0, region_base_vpn + 38'd2}, 2'd0);
    owner_entry = make_s1_range_entry(
        owner_key, owner_csr, 2'd1, 1'b0, 1'b1);
    global_entry = make_stage_prepared_entry(
        global_key, global_csr, 1'b1);
    install_range_entry(owner_key, owner_entry, owner_csr);
    allowed_shape_mask = data.derive_allowed_tlb_shape_mask_with_global_fallback(
        global_key.vpn, global_entry, pte_g_overridden);
    if (!pte_g_overridden || global_entry.s1_pte_g ||
        allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_4K] ||
        allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_2M]) begin
        `uvm_fatal(get_type_name(),
                   "existing global owner was bypassed after global fallback")
    end
    data.delete_live_tlb_entry_by_anchor_key(
        owner_key, "directed existing-global owner cleanup");
endtask:check_global_cross_asid_overlap

task soft_test_l2tlb_range_lookup_sequence::check_stage_context_overlap_semantics();
    mmu_csr_runtime_state owner_csr;
    mmu_csr_runtime_state same_context_csr;
    mmu_csr_runtime_state other_context_csr;
    memblock_tlb_lookup_key_t owner_key;
    memblock_tlb_lookup_key_t candidate_key;
    memblock_tlb_lookup_key_t returned_anchor_key;
    memblock_tlb_entry owner_entry;
    memblock_tlb_entry candidate_entry;
    memblock_tlb_entry returned_entry;
    bit [4:0] allowed_shape_mask;
    bit found;
    bit pte_g_overridden;
    bit [37:0] region_base_vpn;

    // onlyStage1 and onlyStage2 both isolate owners by hgatp.vmid.  The same
    // VMID must observe the owner; a different VMID must remain independent.
    for (int unsigned mode = 1; mode <= 2; mode++) begin
        data.reset_all_tables(1);
        region_base_vpn = 38'h000_0100_000 + (mode << 12);
        owner_csr = make_stage_csr_snapshot(mode[1:0], 16'h0031, 16'h0041);
        same_context_csr = make_stage_csr_snapshot(
            mode[1:0], 16'h0031, 16'h0041);
        other_context_csr = make_stage_csr_snapshot(
            mode[1:0], 16'h0031, 16'h0042);
        owner_key = owner_csr.make_lookup_key(
            {26'b0, region_base_vpn + 38'd1}, mode[1:0]);
        candidate_key = same_context_csr.make_lookup_key(
            {26'b0, region_base_vpn + 38'd2}, mode[1:0]);
        if (owner_key.vmid != 16'h0041 || candidate_key.vmid != 16'h0041) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("s2xlate=%0d did not preserve request VMID", mode))
        end
        owner_entry = make_stage_range_entry(
            owner_key, owner_csr, 2'd1, 1'b0);
        candidate_entry = make_stage_range_entry(
            candidate_key, same_context_csr, 2'd1, 1'b0);
        install_range_entry(owner_key, owner_entry, owner_csr);
        allowed_shape_mask = data.derive_allowed_tlb_shape_mask(
            candidate_key.vpn, candidate_entry);
        if (allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_4K] ||
            allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_2M]) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("s2xlate=%0d same-VMID owner was bypassed", mode))
        end
        found = data.find_tlb_range_hit_by_req(
            candidate_key, same_context_csr, returned_anchor_key, returned_entry);
        if (!found || returned_anchor_key != owner_key ||
            returned_entry != owner_entry) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("s2xlate=%0d same-VMID range hit was lost", mode))
        end

        candidate_key = other_context_csr.make_lookup_key(
            {26'b0, region_base_vpn + 38'd2}, mode[1:0]);
        candidate_entry = make_stage_range_entry(
            candidate_key, other_context_csr, 2'd1, 1'b0);
        allowed_shape_mask = data.derive_allowed_tlb_shape_mask(
            candidate_key.vpn, candidate_entry);
        if (!allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_4K] ||
            !allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_2M]) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("s2xlate=%0d different-VMID candidate was blocked", mode))
        end
        found = data.find_tlb_range_hit_by_req(
            candidate_key, other_context_csr,
            returned_anchor_key, returned_entry);
        if (found || data.has_tlb_entry(candidate_key)) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("s2xlate=%0d crossed VMID during lookup", mode))
        end
        data.delete_live_tlb_entry_by_anchor_key(
            owner_key, "directed stage VMID cleanup");
    end

    // allStage uses S1 ASID/G semantics inside one VMID, and VMID remains an
    // outer isolation boundary.  Cross-ASID non-global candidates are legal;
    // a random global candidate may be localized before publication.
    data.reset_all_tables(1);
    region_base_vpn = 38'h000_0120_000;
    owner_csr = make_stage_csr_snapshot(2'd3, 16'h0051, 16'h0061);
    same_context_csr = make_stage_csr_snapshot(2'd3, 16'h0051, 16'h0061);
    other_context_csr = make_stage_csr_snapshot(2'd3, 16'h0052, 16'h0061);
    owner_key = owner_csr.make_lookup_key(
        {26'b0, region_base_vpn + 38'd1}, 2'd3);
    owner_entry = make_stage_range_entry(
        owner_key, owner_csr, 2'd1, 1'b0);
    install_range_entry(owner_key, owner_entry, owner_csr);

    candidate_key = same_context_csr.make_lookup_key(
        {26'b0, region_base_vpn + 38'd2}, 2'd3);
    candidate_entry = make_stage_range_entry(
        candidate_key, same_context_csr, 2'd1, 1'b0);
    allowed_shape_mask = data.derive_allowed_tlb_shape_mask(
        candidate_key.vpn, candidate_entry);
    if (allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_4K] ||
        allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_2M]) begin
        `uvm_fatal(get_type_name(), "allStage same-ASID owner was bypassed")
    end

    candidate_key = other_context_csr.make_lookup_key(
        {26'b0, region_base_vpn + 38'd2}, 2'd3);
    candidate_entry = make_stage_range_entry(
        candidate_key, other_context_csr, 2'd1, 1'b0);
    allowed_shape_mask = data.derive_allowed_tlb_shape_mask(
        candidate_key.vpn, candidate_entry);
    if (!allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_4K] ||
        !allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_2M]) begin
        `uvm_fatal(get_type_name(), "allStage cross-ASID non-global candidate was blocked")
    end
    candidate_entry = make_stage_prepared_entry(
        candidate_key, other_context_csr, 1'b1);
    allowed_shape_mask = data.derive_allowed_tlb_shape_mask_with_global_fallback(
        candidate_key.vpn, candidate_entry, pte_g_overridden);
    if (!pte_g_overridden || candidate_entry.s1_pte_g ||
        !allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_4K] ||
        !allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_2M]) begin
        `uvm_fatal(get_type_name(), "allStage global fallback did not localize candidate")
    end

    other_context_csr = make_stage_csr_snapshot(2'd3, 16'h0051, 16'h0062);
    candidate_key = other_context_csr.make_lookup_key(
        {26'b0, region_base_vpn + 38'd2}, 2'd3);
    candidate_entry = make_stage_range_entry(
        candidate_key, other_context_csr, 2'd1, 1'b0);
    allowed_shape_mask = data.derive_allowed_tlb_shape_mask(
        candidate_key.vpn, candidate_entry);
    if (!allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_4K] ||
        !allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_2M]) begin
        `uvm_fatal(get_type_name(), "allStage different-VMID candidate was blocked")
    end
    data.delete_live_tlb_entry_by_anchor_key(
        owner_key, "directed allStage context cleanup");

    data.reset_all_tables(1);
    owner_csr = make_stage_csr_snapshot(2'd3, 16'h0051, 16'h0061);
    other_context_csr = make_stage_csr_snapshot(2'd3, 16'h0052, 16'h0061);
    owner_key = owner_csr.make_lookup_key(
        {26'b0, region_base_vpn + 38'd1}, 2'd3);
    owner_entry = make_stage_range_entry(
        owner_key, owner_csr, 2'd1, 1'b0, 1'b1);
    install_range_entry(owner_key, owner_entry, owner_csr);
    candidate_key = other_context_csr.make_lookup_key(
        {26'b0, region_base_vpn + 38'd2}, 2'd3);
    candidate_entry = make_stage_range_entry(
        candidate_key, other_context_csr, 2'd1, 1'b0);
    allowed_shape_mask = data.derive_allowed_tlb_shape_mask(
        candidate_key.vpn, candidate_entry);
    if (allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_4K] ||
        allowed_shape_mask[MEMBLOCK_TLB_COVERAGE_2M]) begin
        `uvm_fatal(get_type_name(), "allStage existing global owner was bypassed")
    end
    data.delete_live_tlb_entry_by_anchor_key(
        owner_key, "directed allStage global cleanup");
endtask:check_stage_context_overlap_semantics

task soft_test_l2tlb_range_lookup_sequence::check_allstage_shape_and_fallback_coverage();
    tlb_map_builder builder;
    tlb_map_builder::memblock_tlb_shape_bucket_t buckets[$];
    mmu_csr_runtime_state csr_snapshot;
    memblock_tlb_lookup_key_t owner_key;
    memblock_tlb_lookup_key_t candidate_key;
    memblock_tlb_entry owner_entry;
    memblock_tlb_entry prepared_entry;
    memblock_tlb_entry built_entry;
    memblock_l2tlb_fallback_warning_catcher warning_catcher;
    int unsigned s1_weights[4];
    int unsigned s2_weights[4];
    int unsigned saved_s1_level_wt[4];
    int unsigned saved_s2_level_wt[4];
    int unsigned saved_s1_fault_wt[2];
    int unsigned saved_s2_fault_wt[2];
    bit saved_level_weight_en;
    int unsigned saved_s1_n_wt;
    int unsigned saved_s2_n_wt;
    longint unsigned total_positive_weight;
    bit [1:0] effective_level;
    bit effective_n;
    bit clear_s1_n;
    bit clear_s2_n;
    bit pte_n_overridden;
    bit forced_4k;
    bit [37:0] region_base_vpn;

    builder = tlb_map_builder::type_id::create("directed_allstage_shape_builder");
    if (builder == null) begin
        `uvm_fatal(get_type_name(), "failed to create allStage shape builder")
    end

    saved_level_weight_en = seq_csr_common::l2tlb_level_weight_en;
    foreach (saved_s1_level_wt[idx]) begin
        saved_s1_level_wt[idx] = seq_csr_common::l2tlb_s1_level_wt[idx];
        saved_s2_level_wt[idx] = seq_csr_common::l2tlb_s2_level_wt[idx];
    end
    foreach (saved_s1_fault_wt[idx]) begin
        saved_s1_fault_wt[idx] = seq_csr_common::l2tlb_s1_fault_wt[idx];
        saved_s2_fault_wt[idx] = seq_csr_common::l2tlb_s2_fault_wt[idx];
    end
    saved_s1_n_wt = seq_csr_common::l2tlb_s1_pte_1_wt[7];
    saved_s2_n_wt = seq_csr_common::l2tlb_s2_pte_1_wt[7];
    seq_csr_common::l2tlb_level_weight_en = 1'b1;

    // 逐项通过生产bucket构建覆盖16个raw level组合、全部N组合及fault/no-fault转换。
    csr_snapshot = make_stage_csr_snapshot(2'd3, 16'h0070, 16'h0080);
    candidate_key = csr_snapshot.make_lookup_key(
        {26'b0, 38'h000_0150_001}, 2'd3);
    prepared_entry = make_stage_prepared_entry(candidate_key, csr_snapshot);
    clear_s1_n = 1'b0;
    clear_s2_n = 1'b0;
    for (int unsigned fault_mode = 0; fault_mode < 2; fault_mode++) begin
        prepared_entry.fault_effective_s1_pf = fault_mode != 0;
        prepared_entry.fault_stage_selected = fault_mode != 0 ?
            memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_S1 :
            memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_NONE;
        for (int unsigned s1_level = 0; s1_level < 4; s1_level++) begin
            for (int unsigned s2_level = 0; s2_level < 4; s2_level++) begin
                foreach (seq_csr_common::l2tlb_s1_level_wt[idx]) begin
                    seq_csr_common::l2tlb_s1_level_wt[idx] = idx == s1_level;
                    seq_csr_common::l2tlb_s2_level_wt[idx] = idx == s2_level;
                end
                for (int unsigned s1_n = 0; s1_n < 2; s1_n++) begin
                    for (int unsigned s2_n = 0; s2_n < 2; s2_n++) begin
                        bit [1:0] expected_s1_level;
                        bit [1:0] expected_s2_level;
                        memblock_tlb_range_coverage_rank_e expected_shape;

                        prepared_entry.s1_pte_n = s1_n[0];
                        prepared_entry.s2_pte_n = s2_n[0];
                        builder.build_shape_candidate_buckets(
                            prepared_entry, 5'b1_1111, clear_s1_n, clear_s2_n,
                            buckets, total_positive_weight);
                        expected_s1_level = (!fault_mode && s1_n != 0) ?
                                            2'd0 : s1_level[1:0];
                        expected_s2_level = (!fault_mode && s2_n != 0) ?
                                            2'd0 : s2_level[1:0];
                        memblock_tlb_entry::derive_allstage_lookup_shape(
                            expected_s1_level, s1_n[0],
                            expected_s2_level, s2_n[0],
                            effective_level, effective_n);
                        expected_shape = builder.coverage_from_level_n(
                            effective_level,
                            effective_level == 2'd0 && effective_n);
                        if (buckets.size() != 1 || total_positive_weight != 1 ||
                            buckets[0].weight != 1 ||
                            buckets[0].s1_level != expected_s1_level ||
                            buckets[0].s2_level != expected_s2_level ||
                            buckets[0].s1_pte_n != s1_n[0] ||
                            buckets[0].s2_pte_n != s2_n[0] ||
                            buckets[0].shape != expected_shape) begin
                            `uvm_fatal(get_type_name(),
                                       $sformatf("allStage bucket mismatch fault=%0d s1=%0d/%0d s2=%0d/%0d buckets=%0d weight=%0d",
                                                 fault_mode, s1_level, s1_n,
                                                 s2_level, s2_n, buckets.size(),
                                                 total_positive_weight))
                        end
                    end
                end
            end
        end
    end

    // Sv39侧必须在与另一stage做笛卡尔积之前屏蔽raw level 3；反向组合同样成立。
    foreach (seq_csr_common::l2tlb_s1_level_wt[idx]) begin
        seq_csr_common::l2tlb_s1_level_wt[idx] = 1;
        seq_csr_common::l2tlb_s2_level_wt[idx] = 1;
    end
    csr_snapshot = make_stage_csr_snapshot(2'd3, 16'h0071, 16'h0081);
    csr_snapshot.vsatp_mode = 4'd8;
    csr_snapshot.hgatp_mode = 4'd9;
    candidate_key = csr_snapshot.make_lookup_key(
        {26'b0, 38'h000_0160_001}, 2'd3);
    prepared_entry = make_stage_prepared_entry(candidate_key, csr_snapshot);
    builder.get_stage_level_weights(1'b1, prepared_entry.s1_stage_active,
                                    prepared_entry.s1_translation_mode_at_build,
                                    s1_weights);
    builder.get_stage_level_weights(1'b0, prepared_entry.s2_stage_active,
                                    prepared_entry.s2_translation_mode_at_build,
                                    s2_weights);
    if (s1_weights[3] != 0 || s2_weights[3] == 0) begin
        `uvm_fatal(get_type_name(), "Sv39/Sv48 mixed allStage level filtering failed")
    end
    builder.build_shape_candidate_buckets(
        prepared_entry, 5'b1_1111, 1'b0, 1'b0,
        buckets, total_positive_weight);
    if (buckets.size() != 12 || total_positive_weight != 12) begin
        `uvm_fatal(get_type_name(),
                   "Sv39/Sv48 mixed allStage Cartesian bucket count is invalid")
    end
    foreach (buckets[idx]) begin
        if (buckets[idx].s1_level == 2'd3) begin
            `uvm_fatal(get_type_name(),
                       "Sv39 S1 level 3 reappeared after Cartesian bucket construction")
        end
    end
    csr_snapshot.vsatp_mode = 4'd9;
    csr_snapshot.hgatp_mode = 4'd8;
    candidate_key = csr_snapshot.make_lookup_key(
        {26'b0, 38'h000_0160_002}, 2'd3);
    prepared_entry = make_stage_prepared_entry(candidate_key, csr_snapshot);
    builder.get_stage_level_weights(1'b1, prepared_entry.s1_stage_active,
                                    prepared_entry.s1_translation_mode_at_build,
                                    s1_weights);
    builder.get_stage_level_weights(1'b0, prepared_entry.s2_stage_active,
                                    prepared_entry.s2_translation_mode_at_build,
                                    s2_weights);
    if (s1_weights[3] == 0 || s2_weights[3] != 0) begin
        `uvm_fatal(get_type_name(), "Sv48/Sv39 mixed allStage level filtering failed")
    end
    builder.build_shape_candidate_buckets(
        prepared_entry, 5'b1_1111, 1'b0, 1'b0,
        buckets, total_positive_weight);
    if (buckets.size() != 12 || total_positive_weight != 12) begin
        `uvm_fatal(get_type_name(),
                   "Sv48/Sv39 mixed allStage Cartesian bucket count is invalid")
    end
    foreach (buckets[idx]) begin
        if (buckets[idx].s2_level == 2'd3) begin
            `uvm_fatal(get_type_name(),
                       "Sv39 S2 level 3 reappeared after Cartesian bucket construction")
        end
    end

    // fault payload不执行无fault NAPOT收敛，raw level=0,N=1与level>0,N=1均需保留。
    csr_snapshot = make_stage_csr_snapshot(2'd3, 16'h0071, 16'h0081);
    candidate_key = csr_snapshot.make_lookup_key(
        {26'b0, 38'h000_0160_003}, 2'd3);
    prepared_entry = make_stage_prepared_entry(candidate_key, csr_snapshot);
    prepared_entry.fault_effective_s1_pf = 1'b1;
    prepared_entry.fault_stage_selected =
        memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_S1;
    prepared_entry.s1_pte_n = 1'b1;
    prepared_entry.s2_pte_n = 1'b0;
    foreach (seq_csr_common::l2tlb_s1_level_wt[idx]) begin
        seq_csr_common::l2tlb_s1_level_wt[idx] = idx == 0;
        seq_csr_common::l2tlb_s2_level_wt[idx] = idx == 1;
    end
    clear_s1_n = 1'b0;
    clear_s2_n = 1'b0;
    builder.build_shape_candidate_buckets(
        prepared_entry, 5'b1_1111, clear_s1_n, clear_s2_n,
        buckets, total_positive_weight);
    if (buckets.size() != 1 || buckets[0].s1_level != 2'd0 ||
        !buckets[0].s1_pte_n || buckets[0].s2_level != 2'd1 ||
        buckets[0].shape != MEMBLOCK_TLB_COVERAGE_64K) begin
        `uvm_fatal(get_type_name(), "fault raw level=0,N=1 was not preserved")
    end

    foreach (seq_csr_common::l2tlb_s1_level_wt[idx]) begin
        seq_csr_common::l2tlb_s1_level_wt[idx] = idx == 2;
        seq_csr_common::l2tlb_s2_level_wt[idx] = idx == 3;
    end
    builder.build_shape_candidate_buckets(
        prepared_entry, 5'b1_1111, clear_s1_n, clear_s2_n,
        buckets, total_positive_weight);
    if (buckets.size() != 1 || buckets[0].s1_level != 2'd2 ||
        !buckets[0].s1_pte_n || buckets[0].s2_level != 2'd3 ||
        buckets[0].shape != MEMBLOCK_TLB_COVERAGE_1G) begin
        `uvm_fatal(get_type_name(), "fault raw level>0,N=1 was not preserved")
    end

    // N=1只产生64KB正权重候选时，用同区域4KB owner屏蔽64KB，必须清N恢复4KB。
    data.reset_all_tables(1);
    foreach (seq_csr_common::l2tlb_s1_level_wt[idx]) begin
        seq_csr_common::l2tlb_s1_level_wt[idx] = idx == 0;
        seq_csr_common::l2tlb_s2_level_wt[idx] = idx == 0;
    end
    seq_csr_common::l2tlb_s1_fault_wt[0] = 0;
    seq_csr_common::l2tlb_s1_fault_wt[1] = 0;
    seq_csr_common::l2tlb_s1_pte_1_wt[7] = 100;
    csr_snapshot = make_s1_csr_snapshot();
    csr_snapshot.satp_mode = 4'd9;
    region_base_vpn = 38'h000_0180_000;
    owner_key = csr_snapshot.make_lookup_key(
        {26'b0, region_base_vpn + 38'd1}, 2'd0);
    owner_entry = make_s1_range_entry(owner_key, csr_snapshot, 2'd0, 1'b0);
    install_range_entry(owner_key, owner_entry, csr_snapshot);
    candidate_key = csr_snapshot.make_lookup_key(
        {26'b0, region_base_vpn + 38'd2}, 2'd0);
    built_entry = data.build_tlb_entry_for_key_with_csr(candidate_key, csr_snapshot);
    if (built_entry.s1_level != 2'd0 || built_entry.s1_pte_n ||
        data.pte_n_override_count != 1) begin
        `uvm_fatal(get_type_name(), "N override did not restore an ordinary 4KB entry")
    end

    // allStage双N被64KB occupancy屏蔽后，最小override必须只清S2 N。
    data.reset_all_tables(1);
    seq_csr_common::l2tlb_s2_fault_wt[0] = 0;
    seq_csr_common::l2tlb_s2_fault_wt[1] = 0;
    seq_csr_common::l2tlb_s2_pte_1_wt[7] = 100;
    csr_snapshot = make_stage_csr_snapshot(2'd3, 16'h0072, 16'h0082);
    region_base_vpn = 38'h000_0190_000;
    owner_key = csr_snapshot.make_lookup_key(
        {26'b0, region_base_vpn + 38'd1}, 2'd3);
    owner_entry = make_stage_range_entry(owner_key, csr_snapshot, 2'd0, 1'b0);
    install_range_entry(owner_key, owner_entry, csr_snapshot);
    candidate_key = csr_snapshot.make_lookup_key(
        {26'b0, region_base_vpn + 38'd2}, 2'd3);
    built_entry = data.build_tlb_entry_for_key_with_csr(candidate_key, csr_snapshot);
    memblock_tlb_entry::derive_allstage_lookup_shape(
        built_entry.s1_level, built_entry.s1_pte_n,
        built_entry.s2_level, built_entry.s2_pte_n,
        effective_level, effective_n);
    if (built_entry.s1_level != 2'd0 || !built_entry.s1_pte_n ||
        built_entry.s2_level != 2'd0 || built_entry.s2_pte_n ||
        effective_level != 2'd0 || effective_n ||
        data.pte_n_override_count != 1) begin
        `uvm_fatal(get_type_name(),
                   "allStage N override did not prefer clearing only S2 N")
    end

    // S2-only clear仍为64KB时，生产fallback必须继续尝试S1-only clear并成功。
    data.reset_all_tables(1);
    foreach (seq_csr_common::l2tlb_s1_level_wt[idx]) begin
        seq_csr_common::l2tlb_s1_level_wt[idx] = idx == 0;
        seq_csr_common::l2tlb_s2_level_wt[idx] = idx == 1;
    end
    csr_snapshot = make_stage_csr_snapshot(2'd3, 16'h0073, 16'h0083);
    region_base_vpn = 38'h000_0191_000;
    owner_key = csr_snapshot.make_lookup_key(
        {26'b0, region_base_vpn + 38'd1}, 2'd3);
    owner_entry = make_stage_range_entry(owner_key, csr_snapshot, 2'd0, 1'b0);
    install_range_entry(owner_key, owner_entry, csr_snapshot);
    candidate_key = csr_snapshot.make_lookup_key(
        {26'b0, region_base_vpn + 38'd2}, 2'd3);
    built_entry = data.build_tlb_entry_for_key_with_csr(candidate_key, csr_snapshot);
    memblock_tlb_entry::derive_allstage_lookup_shape(
        built_entry.s1_level, built_entry.s1_pte_n,
        built_entry.s2_level, built_entry.s2_pte_n,
        effective_level, effective_n);
    if (built_entry.s1_level != 2'd0 || built_entry.s1_pte_n ||
        built_entry.s2_level != 2'd0 || !built_entry.s2_pte_n ||
        effective_level != 2'd0 || effective_n ||
        data.pte_n_override_count != 1) begin
        `uvm_fatal(get_type_name(),
                   "allStage N override did not fall through to clearing only S1 N")
    end

    // 非单调mask只用于覆盖最后一个builder防御分支：两个单bit clear都仍为
    // 64KB，只有同时清N后raw 2MB候选可用。真实occupancy mask保持单调。
    foreach (seq_csr_common::l2tlb_s1_level_wt[idx]) begin
        seq_csr_common::l2tlb_s1_level_wt[idx] = idx == 1;
        seq_csr_common::l2tlb_s2_level_wt[idx] = idx == 1;
    end
    csr_snapshot = make_stage_csr_snapshot(2'd3, 16'h0074, 16'h0084);
    candidate_key = csr_snapshot.make_lookup_key(
        {26'b0, 38'h000_0192_002}, 2'd3);
    prepared_entry = make_stage_prepared_entry(candidate_key, csr_snapshot);
    prepared_entry.s1_pte_n = 1'b1;
    prepared_entry.s2_pte_n = 1'b1;
    built_entry = builder.finalize_tlb_payload_shape_and_payload(
        prepared_entry, candidate_key, csr_snapshot, 5'b0_0100,
        pte_n_overridden, forced_4k, 1'b0,
        MEMBLOCK_TLB_COVERAGE_4K);
    if (built_entry.s1_level != 2'd1 || built_entry.s1_pte_n ||
        built_entry.s2_level != 2'd1 || built_entry.s2_pte_n ||
        !pte_n_overridden || forced_4k) begin
        `uvm_fatal(get_type_name(),
                   "allStage N override did not reach the clear-both fallback branch")
    end

    // 只给2MB正权重且该shape被child占用时，必须强制4KB；前四次warning后限流。
    data.reset_all_tables(1);
    foreach (seq_csr_common::l2tlb_s1_level_wt[idx]) begin
        seq_csr_common::l2tlb_s1_level_wt[idx] = idx == 1;
        seq_csr_common::l2tlb_s2_level_wt[idx] = idx == 0;
    end
    seq_csr_common::l2tlb_s1_fault_wt[0] = 100;
    seq_csr_common::l2tlb_s1_fault_wt[1] = 0;
    seq_csr_common::l2tlb_s1_pte_1_wt[7] = 0;
    seq_csr_common::l2tlb_s2_pte_1_wt[7] = 0;
    csr_snapshot = make_s1_csr_snapshot();
    csr_snapshot.satp_mode = 4'd9;
    region_base_vpn = 38'h000_01a0_000;
    owner_key = csr_snapshot.make_lookup_key(
        {26'b0, region_base_vpn + 38'd1}, 2'd0);
    owner_entry = make_s1_range_entry(owner_key, csr_snapshot, 2'd0, 1'b0);
    install_range_entry(owner_key, owner_entry, csr_snapshot);
    warning_catcher = new();
    uvm_report_cb::add(null, warning_catcher);
    for (int unsigned occurrence = 0; occurrence < 6; occurrence++) begin
        candidate_key = csr_snapshot.make_lookup_key(
            {26'b0, region_base_vpn + 38'h100 + occurrence}, 2'd0);
        built_entry = data.build_tlb_entry_for_key_with_csr(
            candidate_key, csr_snapshot);
        if (built_entry.s1_level != 2'd0 || built_entry.s1_pte_n ||
            !built_entry.fault_effective_s1_pf ||
            built_entry.fault_stage_selected !=
                memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_S1) begin
            `uvm_fatal(get_type_name(),
                       "forced 4KB did not preserve the prepared fault payload")
        end
    end
    uvm_report_cb::delete(null, warning_catcher);
    if (data.forced_4k_fallback_count != 6 ||
        warning_catcher.caught_count != 4) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("forced 4KB counter/warning mismatch count=%0d warnings=%0d",
                             data.forced_4k_fallback_count,
                             warning_catcher.caught_count))
    end

    seq_csr_common::l2tlb_level_weight_en = saved_level_weight_en;
    foreach (saved_s1_level_wt[idx]) begin
        seq_csr_common::l2tlb_s1_level_wt[idx] = saved_s1_level_wt[idx];
        seq_csr_common::l2tlb_s2_level_wt[idx] = saved_s2_level_wt[idx];
    end
    foreach (saved_s1_fault_wt[idx]) begin
        seq_csr_common::l2tlb_s1_fault_wt[idx] = saved_s1_fault_wt[idx];
        seq_csr_common::l2tlb_s2_fault_wt[idx] = saved_s2_fault_wt[idx];
    end
    seq_csr_common::l2tlb_s1_pte_1_wt[7] = saved_s1_n_wt;
    seq_csr_common::l2tlb_s2_pte_1_wt[7] = saved_s2_n_wt;
    data.reset_all_tables(1);
endtask:check_allstage_shape_and_fallback_coverage

task soft_test_l2tlb_range_lookup_sequence::check_parent_ownership_lifecycle();
    mmu_csr_runtime_state csr_snapshot;
    memblock_tlb_lookup_key_t first_key;
    memblock_tlb_lookup_key_t second_key;
    memblock_tlb_entry first_entry;
    memblock_tlb_entry second_entry;
    memblock_sfence_payload_t payload;
    int unsigned deleted_count;
    int unsigned second_parent_key_count;
    longint unsigned reset_epoch;
    longint unsigned anchor_sample;
    longint unsigned event_seq;
    longint unsigned saved_last_event_seq;

    data.reset_all_tables(1);
    csr_snapshot = make_s1_csr_snapshot();
    csr_snapshot.satp_mode = 4'd9;
    first_key = csr_snapshot.make_lookup_key(
        {26'b0, 38'h000_01c0_001}, 2'd0);
    second_key = csr_snapshot.make_lookup_key(
        {26'b0, 38'h000_01e0_001}, 2'd0);
    first_entry = make_s1_range_entry(first_key, csr_snapshot, 2'd0, 1'b0);
    second_entry = make_s1_range_entry(second_key, csr_snapshot, 2'd0, 1'b0);
    install_range_entry(first_key, first_entry, csr_snapshot);
    install_range_entry(second_key, second_entry, csr_snapshot);
    second_parent_key_count = second_entry.parent_occupancy_keys.size();

    reset_epoch = memblock_sync_pkg::get_l2tlb_current_reset_epoch();
    saved_last_event_seq = memblock_sync_pkg::last_allocated_l2tlb_event_seq;
    event_seq = saved_last_event_seq + 1;
    anchor_sample = 64'h400;
    memblock_sync_pkg::last_allocated_l2tlb_event_seq = event_seq;
    payload = '{default:'0};
    payload.valid = 1'b1;
    payload.ignore_addr = 1'b0;
    payload.ignore_id = 1'b1;
    payload.target_stage = MEMBLOCK_SFENCE_TARGET_HS_S1;
    payload.s1_vpn = first_key.vpn[37:0];
    payload.sample_seq = anchor_sample;
    payload.reset_epoch = reset_epoch;
    payload.lifecycle_event_seq = event_seq;
    if (!data.schedule_sfence_invalidate(
            payload, anchor_sample, reset_epoch, event_seq)) begin
        `uvm_fatal(get_type_name(), "address Fence schedule failed")
    end
    deleted_count = data.apply_due_sfence_invalidate(
        anchor_sample + MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES,
        reset_epoch);
    if (deleted_count != 1 || data.has_tlb_entry(first_key) ||
        !data.has_tlb_entry(second_key) ||
        data.tlb_parent_owner_set.num() != second_parent_key_count ||
        data.tlb_parent_occupancy_count.num() != second_parent_key_count) begin
        `uvm_fatal(get_type_name(),
                   "address Fence did not remove exactly one parent owner")
    end

    event_seq++;
    anchor_sample += 16;
    memblock_sync_pkg::last_allocated_l2tlb_event_seq = event_seq;
    payload.ignore_addr = 1'b1;
    payload.sample_seq = anchor_sample;
    payload.lifecycle_event_seq = event_seq;
    if (!data.schedule_sfence_invalidate(
            payload, anchor_sample, reset_epoch, event_seq)) begin
        `uvm_fatal(get_type_name(), "full Fence schedule failed")
    end
    deleted_count = data.apply_due_sfence_invalidate(
        anchor_sample + MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES,
        reset_epoch);
    memblock_sync_pkg::last_allocated_l2tlb_event_seq = saved_last_event_seq;
    if (deleted_count != 1 || data.tlb_entry_by_key.num() != 0 ||
        data.tlb_anchor_keys_by_range_key.num() != 0 ||
        data.tlb_parent_occupancy_count.num() != 0 ||
        data.tlb_parent_owner_set.num() != 0) begin
        `uvm_fatal(get_type_name(),
                   "full Fence left canonical/range/parent ownership state")
    end

    first_entry = make_s1_range_entry(first_key, csr_snapshot, 2'd0, 1'b0);
    install_range_entry(first_key, first_entry, csr_snapshot);
    data.clear_dispatch_l2tlb_live_entries();
    if (data.tlb_entry_by_key.num() != 0 ||
        data.tlb_anchor_keys_by_range_key.num() != 0 ||
        data.tlb_parent_occupancy_count.num() != 0 ||
        data.tlb_parent_owner_set.num() != 0) begin
        `uvm_fatal(get_type_name(),
                   "runtime reset clear left canonical/range/parent state")
    end

    first_entry = make_s1_range_entry(first_key, csr_snapshot, 2'd0, 1'b0);
    install_range_entry(first_key, first_entry, csr_snapshot);
    data.reset_all_tables(1);
    if (data.tlb_entry_by_key.num() != 0 ||
        data.tlb_anchor_keys_by_range_key.num() != 0 ||
        data.tlb_parent_occupancy_count.num() != 0 ||
        data.tlb_parent_owner_set.num() != 0) begin
        `uvm_fatal(get_type_name(),
                   "reset_all_tables left canonical/range/parent state")
    end
endtask:check_parent_ownership_lifecycle

task soft_test_l2tlb_range_lookup_sequence::check_finalized_detached_fallback_rejected();
    mmu_csr_runtime_state csr_snapshot;
    memblock_tlb_lookup_key_t key;
    memblock_tlb_entry finalized_entry;
    memblock_l2tlb_prepared_contract_expected_fatal_catcher catcher;
    bit [4:0] ignored_mask;
    bit ignored_g_override;

    data.reset_all_tables(1);
    csr_snapshot = make_s1_csr_snapshot();
    key = csr_snapshot.make_lookup_key({26'b0, 38'h000_0140_001}, 2'd0);
    finalized_entry = make_s1_range_entry(
        key, csr_snapshot, 2'd0, 1'b0, 1'b1);
    finalized_entry.entry_generation = 0;
    if (finalized_entry.payload_phase !=
        memblock_tlb_entry::MEMBLOCK_TLB_PAYLOAD_FINALIZED) begin
        `uvm_fatal(get_type_name(), "negative fallback test lost FINALIZED phase")
    end

    catcher = new();
    uvm_report_cb::add(null, catcher);
    ignored_mask = data.derive_allowed_tlb_shape_mask_with_global_fallback(
        key.vpn, finalized_entry, ignored_g_override);
    uvm_report_cb::delete(null, catcher);
    if (catcher.caught_count != 1) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("finalized detached fallback expected one fatal got=%0d mask=0x%0h",
                             catcher.caught_count, ignored_mask))
    end
endtask:check_finalized_detached_fallback_rejected

task soft_test_l2tlb_range_lookup_sequence::check_illegal_overlap_fails_fast();
    mmu_csr_runtime_state csr_snapshot;
    memblock_tlb_lookup_key_t napot_anchor_key;
    memblock_tlb_lookup_key_t superpage_anchor_key;
    memblock_tlb_lookup_key_t request_key;
    memblock_tlb_lookup_key_t returned_anchor_key;
    memblock_tlb_entry napot_entry;
    memblock_tlb_entry superpage_entry;
    memblock_tlb_entry returned_entry;
    memblock_l2tlb_overlap_expected_fatal_catcher catcher;
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
    request_key = csr_snapshot.make_lookup_key({26'b0, request_vpn}, 2'd0);
    napot_entry = make_s1_range_entry(napot_anchor_key, csr_snapshot, 2'd0, 1'b1);
    superpage_entry = make_s1_range_entry(superpage_anchor_key, csr_snapshot, 2'd2, 1'b0);
    inject_live_overlap_for_expected_fatal_test_only(
        napot_anchor_key, napot_entry,
        superpage_anchor_key, superpage_entry);

    catcher = new();
    uvm_report_cb::add(null, catcher);
    // The expected fatal is caught so the directed test can inspect it.  Call
    // only the read-only range lookup here: a caught fatal returns control to
    // the caller, and the full get-or-create path would otherwise continue into
    // an intentionally illegal live table and produce an unrelated build fatal.
    void'(data.find_tlb_range_hit_by_req(
        request_key, csr_snapshot, returned_anchor_key, returned_entry));
    uvm_report_cb::delete(null, catcher);
    if (catcher.caught_count != 1) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("illegal live overlap expected one fatal got=%0d",
                             catcher.caught_count))
    end
    // 非法注入没有parent ownership，必须整表reset，不能走生产逐entry删除。
    data.reset_all_tables(1);
endtask:check_illegal_overlap_fails_fast

`endif
