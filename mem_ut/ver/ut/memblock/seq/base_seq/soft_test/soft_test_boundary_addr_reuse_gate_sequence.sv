//=========================================================
//File name    : soft_test_boundary_addr_reuse_gate_sequence.sv
//Module name  : soft_test_boundary_addr_reuse_gate_sequence
//Discribution : software-only boundary address-reuse gate check
//=========================================================
`ifndef SOFT_TEST_BOUNDARY_ADDR_REUSE_GATE_SEQUENCE__SV
`define SOFT_TEST_BOUNDARY_ADDR_REUSE_GATE_SEQUENCE__SV

class soft_test_boundary_addr_reuse_gate_sequence extends memblock_dispatch_base_sequence;

    `uvm_object_utils(soft_test_boundary_addr_reuse_gate_sequence)

    extern function new(string name = "soft_test_boundary_addr_reuse_gate_sequence");
    extern virtual task body();

endclass:soft_test_boundary_addr_reuse_gate_sequence

function soft_test_boundary_addr_reuse_gate_sequence::new(
    string name = "soft_test_boundary_addr_reuse_gate_sequence");
    super.new(name);
endfunction:new

// 抽象职责：该软件专项只构造一个无 reference 的 automatic boundary entry，验证
// fallback 从 Load 改为 Store 时仍遵守 STORE x CROSS_8B gate；不启动 LSQ/issue/DUT wire。
task soft_test_boundary_addr_reuse_gate_sequence::body();
    main_control_transaction tr;
    memblock_boundary_profile_e actual_profile;

    ensure_dispatch_runtime_helpers();
    if (!seq_csr_common::get_boundary_profile_gen_en() ||
        seq_csr_common::get_store_cross_8b_within_16b_en() ||
        seq_csr_common::get_boundary_profile_weight(
            MEMBLOCK_BOUNDARY_PROFILE_CROSS_8B_WITHIN_16B) == 0 ||
        seq_csr_common::get_addr_reuse_en_1_wt() == 0 ||
        seq_csr_common::get_addr_reuse_en_0_wt() != 0 ||
        seq_csr_common::get_addr_reuse_load_after_store_wt() == 0 ||
        seq_csr_common::get_addr_reuse_load_after_load_wt() != 0 ||
        seq_csr_common::get_addr_reuse_store_after_load_wt() != 0 ||
        seq_csr_common::get_addr_reuse_store_after_store_wt() != 0) begin
        `uvm_fatal(get_type_name(),
                   "boundary fallback gate test requires its fixed cross-8B/no-reference cfg")
    end

    build_random_main_table(1);
    tr = data.get_main_transaction(0);
    actual_profile = classify_boundary_profile(tr.vaddr, tr.boundary_size_bytes);
    if (tr.op_class != MEMBLOCK_OP_CLASS_STORE ||
        tr.fuOpType != MEMBLOCK_LSUOP_SH ||
        tr.boundary_size_bytes != 2 ||
        tr.boundary_profile != MEMBLOCK_BOUNDARY_PROFILE_ALIGNED ||
        actual_profile != MEMBLOCK_BOUNDARY_PROFILE_ALIGNED) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("Store cross-8B gate was bypassed op_class=%s fuOpType=0x%0h profile=%s actual=%s size=%0d",
                             op_class_name(tr.op_class),
                             tr.fuOpType,
                             boundary_profile_name(tr.boundary_profile),
                             boundary_profile_name(actual_profile),
                             tr.boundary_size_bytes))
    end
    check_boundary_full_vaddr_span(tr, tr.boundary_size_bytes,
                                   "boundary fallback Store cross-8B gate test");
    `uvm_info(get_type_name(),
              "boundary no-reference fallback honored STORE x CROSS_8B gate", UVM_LOW)
endtask:body

`endif
