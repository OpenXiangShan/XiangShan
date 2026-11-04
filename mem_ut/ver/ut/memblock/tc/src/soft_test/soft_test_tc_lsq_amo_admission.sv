`ifndef SOFT_TEST_TC_LSQ_AMO_ADMISSION__SV
`define SOFT_TEST_TC_LSQ_AMO_ADMISSION__SV

class soft_test_tc_lsq_amo_admission extends soft_test_tc_dispatch_smoke;
    `uvm_component_utils(soft_test_tc_lsq_amo_admission)

    function new(string name = "soft_test_tc_lsq_amo_admission",
                 uvm_component parent = null);
        super.new(name, parent);
    endfunction:new

    virtual task run_dispatch_smoke_sequence();
        soft_test_memblock_lsq_amo_admission_sequence seq;

        seq = soft_test_memblock_lsq_amo_admission_sequence::type_id::create(
            "lsq_amo_admission_seq");
        if (seq == null) begin
            `uvm_fatal(get_type_name(), "failed to create LSQ/AMO admission soft sequence")
        end
        seq.start(null);
    endtask:run_dispatch_smoke_sequence
endclass:soft_test_tc_lsq_amo_admission

class tc_lsq_amo_admission_soft extends soft_test_tc_lsq_amo_admission;
    `uvm_component_utils(tc_lsq_amo_admission_soft)

    function new(string name = "tc_lsq_amo_admission_soft",
                 uvm_component parent = null);
        super.new(name, parent);
    endfunction:new
endclass:tc_lsq_amo_admission_soft

`endif
