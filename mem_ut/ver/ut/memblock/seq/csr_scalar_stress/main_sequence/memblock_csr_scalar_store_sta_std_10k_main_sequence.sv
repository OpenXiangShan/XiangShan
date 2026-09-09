`ifndef MEMBLOCK_CSR_SCALAR_STORE_STA_STD_10K_MAIN_SEQUENCE__SV
`define MEMBLOCK_CSR_SCALAR_STORE_STA_STD_10K_MAIN_SEQUENCE__SV

class memblock_csr_scalar_store_sta_std_10k_main_sequence extends
    memblock_csr_scalar_stress_main_sequence_base;
    `uvm_object_utils(memblock_csr_scalar_store_sta_std_10k_main_sequence)

    function new(string name = "memblock_csr_scalar_store_sta_std_10k_main_sequence");
        super.new(name);
    endfunction:new

    virtual function int unsigned expected_normal_slot_count();
        return 10025;
    endfunction:expected_normal_slot_count

    virtual function int unsigned expected_csr_marker_count();
        return 25;
    endfunction:expected_csr_marker_count

    virtual function bit business_op_class_allowed(input memblock_op_class_e op_class);
        return op_class == MEMBLOCK_OP_CLASS_STORE;
    endfunction:business_op_class_allowed

    virtual function string stress_profile_name();
        return "SCALAR_STORE_STA_STD_10K";
    endfunction:stress_profile_name
endclass:memblock_csr_scalar_store_sta_std_10k_main_sequence

`endif
