`ifndef MEMBLOCK_CSR_SCALAR_LOAD_10K_VSEQ__SV
`define MEMBLOCK_CSR_SCALAR_LOAD_10K_VSEQ__SV

class memblock_csr_scalar_load_10k_vseq extends memblock_csr_scalar_stress_vseq_base;
    `uvm_object_utils(memblock_csr_scalar_load_10k_vseq)

    function new(string name = "memblock_csr_scalar_load_10k_vseq");
        super.new(name);
    endfunction:new

    virtual function memblock_csr_scalar_stress_main_sequence_base
        create_stress_main_sequence();
        memblock_csr_scalar_load_10k_main_sequence main_seq;
        main_seq = memblock_csr_scalar_load_10k_main_sequence::type_id::create("main_seq");
        return main_seq;
    endfunction:create_stress_main_sequence
endclass:memblock_csr_scalar_load_10k_vseq

`endif
