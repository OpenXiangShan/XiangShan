//=========================================================
// CSR scalar 10K stress virtual-sequence base
//=========================================================
`ifndef MEMBLOCK_CSR_SCALAR_STRESS_VSEQ_BASE__SV
`define MEMBLOCK_CSR_SCALAR_STRESS_VSEQ_BASE__SV

class memblock_csr_scalar_stress_vseq_base extends memblock_csr_random_config_vseq;
    `uvm_object_utils(memblock_csr_scalar_stress_vseq_base)

    extern function new(string name = "memblock_csr_scalar_stress_vseq_base");
    extern virtual function memblock_csr_scalar_stress_main_sequence_base
        create_stress_main_sequence();
    extern virtual task start_core_dispatch_flow();
endclass:memblock_csr_scalar_stress_vseq_base

function memblock_csr_scalar_stress_vseq_base::new(
    string name = "memblock_csr_scalar_stress_vseq_base"
);
    super.new(name);
endfunction:new

function memblock_csr_scalar_stress_main_sequence_base
memblock_csr_scalar_stress_vseq_base::create_stress_main_sequence();
    return null;
endfunction:create_stress_main_sequence

task memblock_csr_scalar_stress_vseq_base::start_core_dispatch_flow();
    common_data_transaction data;
    memblock_lsqenq_dispatch_base_sequence lsqenq_seq;
    memblock_issue_dispatch_base_sequence issue_seq;
    memblock_lsqcommit_dispatch_base_sequence lsqcommit_seq;
    memblock_l2tlb_base_sequence l2tlb_seq;
    memblock_csr_scalar_stress_main_sequence_base main_seq;
    memblock_csr_initial_config_sequence initial_csr_seq;
    memblock_csr_control_base_sequence csr_control_seq;
    memblock_sfence_control_base_sequence sfence_control_seq;

    data = common_data_transaction::get();
    main_seq = create_stress_main_sequence();
    if (main_seq == null) begin
        `uvm_fatal(get_type_name(), "stress VSEQ did not create its dedicated main sequence")
    end

    // Keep the proven CSR ownership order: main-table/bootstrap first, initial
    // CSR on the sole CSR sequencer, then the dynamic worker and producers.
    fork
        begin
            main_seq.start(p_sequencer, this);
        end
        begin
            `uvm_do_on(initial_csr_seq, p_sequencer.csr_ctrl_sqr)
            `uvm_do_on(csr_control_seq, p_sequencer.csr_ctrl_sqr)
        end
        begin
            wait_for_initial_config(data);
            `uvm_do_on(lsqenq_seq, p_sequencer.lsqenq_sqr)
        end
        begin
            wait_for_initial_config(data);
            `uvm_do_on(issue_seq, p_sequencer.lintsissue_sqr)
        end
        begin
            wait_for_initial_config(data);
            `uvm_do_on(lsqcommit_seq, p_sequencer.lsqcommit_sqr)
        end
        begin
            wait_for_initial_config(data);
            `uvm_do_on(l2tlb_seq, p_sequencer.L2tlb_sqr)
        end
        begin
            wait_for_initial_config(data);
            `uvm_do_on(sfence_control_seq, p_sequencer.fence_sqr)
        end
    join
endtask:start_core_dispatch_flow

`endif
