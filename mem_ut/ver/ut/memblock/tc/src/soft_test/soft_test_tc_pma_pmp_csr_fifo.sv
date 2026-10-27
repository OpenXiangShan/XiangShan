`ifndef SOFT_TEST_TC_PMA_PMP_CSR_FIFO__SV
`define SOFT_TEST_TC_PMA_PMP_CSR_FIFO__SV
class soft_test_tc_pma_pmp_csr_fifo extends soft_test_tc_dispatch_smoke;
    `uvm_component_utils(soft_test_tc_pma_pmp_csr_fifo)
    function new(string name="soft_test_tc_pma_pmp_csr_fifo",uvm_component parent=null);
        super.new(name,parent);
    endfunction
    virtual task run_dispatch_smoke_sequence();
        soft_test_pma_pmp_csr_fifo_sequence seq;
        seq=soft_test_pma_pmp_csr_fifo_sequence::type_id::create("pma_pmp_csr_fifo_seq");
        seq.start(null);
    endtask
endclass
`endif
