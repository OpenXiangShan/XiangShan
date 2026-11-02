`ifndef MEMBLOCK_L2TLB_ROUTED_PPN_SOFT_VSEQ__SV
`define MEMBLOCK_L2TLB_ROUTED_PPN_SOFT_VSEQ__SV

class memblock_l2tlb_routed_ppn_soft_vseq extends virtual_base_sequence;
    `uvm_object_utils(memblock_l2tlb_routed_ppn_soft_vseq)
    function new(string name = "memblock_l2tlb_routed_ppn_soft_vseq");
        super.new(name);
    endfunction
    virtual task body();
        soft_test_l2tlb_routed_ppn_sequence seq;
        seq_csr_common::init();
        require_virtual_sqr();
        `uvm_do_on(seq, p_sequencer)
    endtask
endclass
`endif
