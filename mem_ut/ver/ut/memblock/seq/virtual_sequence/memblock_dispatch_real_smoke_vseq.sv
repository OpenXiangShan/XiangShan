//=========================================================
//File name    : memblock_dispatch_real_smoke_vseq.sv
//Author       : OpenAI_Codex
//Module name  : memblock_dispatch_real_smoke_vseq
//Discribution : real dispatch smoke virtual sequence
//Date         : 2026-07-03
//=========================================================
`ifndef MEMBLOCK_DISPATCH_REAL_SMOKE_VSEQ__SV
`define MEMBLOCK_DISPATCH_REAL_SMOKE_VSEQ__SV

class memblock_dispatch_real_smoke_vseq extends virtual_base_sequence;

    `uvm_object_utils(memblock_dispatch_real_smoke_vseq)

    extern function new(string name = "memblock_dispatch_real_smoke_vseq");
    extern virtual task pre_body();
    extern virtual task body();
    extern virtual function void require_real_smoke_sqr();
    extern virtual task start_background_responders();
    extern virtual task start_core_dispatch_flow();

endclass:memblock_dispatch_real_smoke_vseq

function memblock_dispatch_real_smoke_vseq::new(string name = "memblock_dispatch_real_smoke_vseq");
    super.new(name);
endfunction:new

task memblock_dispatch_real_smoke_vseq::pre_body();
    if (starting_phase != null) begin
        starting_phase.phase_done.set_drain_time(this, 1us);
        starting_phase.raise_objection(this);
    end
endtask:pre_body

task memblock_dispatch_real_smoke_vseq::body();
    require_real_smoke_sqr();
    seq_csr_common::init();

    memblock_sync_pkg::dispatch_real_smoke_active = 1'b1;
    `uvm_info(get_type_name(), "real dispatch smoke virtual sequence start", UVM_LOW)

    fork : background_responder_fork
        start_background_responders();
    join_none

    start_core_dispatch_flow();

    memblock_sync_pkg::dispatch_real_smoke_active = 1'b0;
    `uvm_info(get_type_name(), "real dispatch smoke virtual sequence completed", UVM_LOW)
endtask:body

function void memblock_dispatch_real_smoke_vseq::require_real_smoke_sqr();
    require_virtual_sqr();
    require_agent_sqr("lsqenq", p_sequencer.lsqenq_sqr);
    require_agent_sqr("lintsissue", p_sequencer.lintsissue_sqr);
    require_agent_sqr("lsqcommit", p_sequencer.lsqcommit_sqr);
    require_agent_sqr("L2tlb", p_sequencer.L2tlb_sqr);
    require_agent_sqr("dcache", p_sequencer.dcache_sqr);
    require_agent_sqr("sbuffer", p_sequencer.sbuffer_sqr);
    require_agent_sqr("redirect", p_sequencer.redirect_sqr);
endfunction:require_real_smoke_sqr

task memblock_dispatch_real_smoke_vseq::start_background_responders();
    dcache_mem__access_base_sequence        dcache_seq;
    sbuffer_mem_access_base_sequence        sbuffer_seq;
    memblock_redirect_dispatch_base_sequence redirect_seq;

    dcache_seq = dcache_mem__access_base_sequence::type_id::create("dcache_seq");
    sbuffer_seq = sbuffer_mem_access_base_sequence::type_id::create("sbuffer_seq");
    redirect_seq = memblock_redirect_dispatch_base_sequence::type_id::create("redirect_seq");

    fork
        dcache_seq.start(p_sequencer.dcache_sqr);
        sbuffer_seq.start(p_sequencer.sbuffer_sqr);
        redirect_seq.start(p_sequencer.redirect_sqr);
    join
endtask:start_background_responders

task memblock_dispatch_real_smoke_vseq::start_core_dispatch_flow();
    memblock_lsqenq_dispatch_base_sequence                  lsqenq_seq;
    memblock_issue_dispatch_base_sequence                   issue_seq;
    memblock_lsqcommit_dispatch_base_sequence               lsqcommit_seq;
    memblock_l2tlb_base_sequence                            l2tlb_seq;
    memblock_main_dispatch_auto_build_main_table_base_sequence main_seq;

    lsqenq_seq = memblock_lsqenq_dispatch_base_sequence::type_id::create("lsqenq_seq");
    issue_seq = memblock_issue_dispatch_base_sequence::type_id::create("issue_seq");
    lsqcommit_seq = memblock_lsqcommit_dispatch_base_sequence::type_id::create("lsqcommit_seq");
    l2tlb_seq = memblock_l2tlb_base_sequence::type_id::create("l2tlb_seq");
    main_seq = memblock_main_dispatch_auto_build_main_table_base_sequence::type_id::create("main_seq");

    fork
        lsqenq_seq.start(p_sequencer.lsqenq_sqr);
        issue_seq.start(p_sequencer.lintsissue_sqr);
        lsqcommit_seq.start(p_sequencer.lsqcommit_sqr);
        l2tlb_seq.start(p_sequencer.L2tlb_sqr);
        main_seq.start(p_sequencer);
    join
endtask:start_core_dispatch_flow

`endif
