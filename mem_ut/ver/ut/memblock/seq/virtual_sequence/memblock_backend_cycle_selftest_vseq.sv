`ifndef MEMBLOCK_BACKEND_CYCLE_SELFTEST_VSEQ__SV
`define MEMBLOCK_BACKEND_CYCLE_SELFTEST_VSEQ__SV
class memblock_backend_cycle_selftest_vseq extends virtual_base_sequence;
    `uvm_object_utils(memblock_backend_cycle_selftest_vseq)
    function new(string name="memblock_backend_cycle_selftest_vseq"); super.new(name); endfunction
    virtual task body();
        soft_test_memblock_backend_cycle_sequence checks;
        require_virtual_sqr();
        seq_csr_common::init();
        `uvm_do_on(checks, p_sequencer)
    endtask
endclass
`endif
