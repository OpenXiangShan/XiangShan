//=========================================================
//File name    : soft_test_tc_l2tlb_pbmt_csr_gate.sv
//Module name  : soft_test_tc_l2tlb_pbmt_csr_gate
//Discribution : software-only V2 L2TLB PBMT/PBMTE test
//=========================================================
`ifndef SOFT_TEST_TC_L2TLB_PBMT_CSR_GATE__SV
`define SOFT_TEST_TC_L2TLB_PBMT_CSR_GATE__SV

class soft_test_tc_l2tlb_pbmt_csr_gate extends soft_test_tc_dispatch_smoke;

    `uvm_component_utils(soft_test_tc_l2tlb_pbmt_csr_gate)

    function new(string name = "soft_test_tc_l2tlb_pbmt_csr_gate",
                 uvm_component parent = null);
        super.new(name, parent);
    endfunction:new

    virtual task run_dispatch_smoke_sequence();
        soft_test_l2tlb_pbmt_csr_gate_sequence pbmt_seq;

        pbmt_seq = soft_test_l2tlb_pbmt_csr_gate_sequence::type_id::create(
            "pbmt_csr_gate_seq");
        if (pbmt_seq == null) begin
            `uvm_fatal(get_type_name(),
                       "failed to create soft_test_l2tlb_pbmt_csr_gate_sequence")
        end
        pbmt_seq.start(null);
    endtask:run_dispatch_smoke_sequence

endclass:soft_test_tc_l2tlb_pbmt_csr_gate

class tc_l2tlb_pbmt_csr_gate extends soft_test_tc_l2tlb_pbmt_csr_gate;

    `uvm_component_utils(tc_l2tlb_pbmt_csr_gate)

    function new(string name = "tc_l2tlb_pbmt_csr_gate",
                 uvm_component parent = null);
        super.new(name, parent);
    endfunction:new

endclass:tc_l2tlb_pbmt_csr_gate

`endif
