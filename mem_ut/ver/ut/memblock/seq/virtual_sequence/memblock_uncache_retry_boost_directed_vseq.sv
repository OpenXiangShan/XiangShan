//=========================================================
//File name    : memblock_uncache_retry_boost_directed_vseq.sv
//Author       : OpenAI_Codex
//Module name  : memblock_uncache_retry_boost_directed_vseq
//Discribution : focused uncache retry boost virtual entry
//Date         : 2026-10-15
//=========================================================
`ifndef MEMBLOCK_UNCACHE_RETRY_BOOST_DIRECTED_VSEQ__SV
`define MEMBLOCK_UNCACHE_RETRY_BOOST_DIRECTED_VSEQ__SV

class memblock_uncache_retry_boost_directed_vseq extends virtual_base_sequence;

    `uvm_object_utils(memblock_uncache_retry_boost_directed_vseq)

    extern function new(
        string name = "memblock_uncache_retry_boost_directed_vseq"
    );
    extern virtual task body();

endclass:memblock_uncache_retry_boost_directed_vseq

function memblock_uncache_retry_boost_directed_vseq::new(
    string name = "memblock_uncache_retry_boost_directed_vseq"
);
    super.new(name);
endfunction:new

task memblock_uncache_retry_boost_directed_vseq::body();
    soft_test_memblock_uncache_retry_boost_sequence directed_seq;

    require_virtual_sqr();
    `uvm_do_on(directed_seq, p_sequencer)
endtask:body

`endif
