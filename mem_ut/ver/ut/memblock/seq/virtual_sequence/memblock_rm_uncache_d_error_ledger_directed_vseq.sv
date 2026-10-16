//=========================================================
//File name    : memblock_rm_uncache_d_error_ledger_directed_vseq.sv
//Module name  : memblock_rm_uncache_d_error_ledger_directed_vseq
//Discribution : Virtual wrapper for RM Uncache D-error ledger checks
//Date         : 2026-10-15
//=========================================================
`ifndef MEMBLOCK_RM_UNCACHE_D_ERROR_LEDGER_DIRECTED_VSEQ__SV
`define MEMBLOCK_RM_UNCACHE_D_ERROR_LEDGER_DIRECTED_VSEQ__SV

class memblock_rm_uncache_d_error_ledger_directed_vseq extends virtual_base_sequence;
    `uvm_object_utils(memblock_rm_uncache_d_error_ledger_directed_vseq)

    extern function new(string name = "memblock_rm_uncache_d_error_ledger_directed_vseq");
    extern task body();
endclass:memblock_rm_uncache_d_error_ledger_directed_vseq

function memblock_rm_uncache_d_error_ledger_directed_vseq::new(
    string name = "memblock_rm_uncache_d_error_ledger_directed_vseq"
);
    super.new(name);
endfunction:new

task memblock_rm_uncache_d_error_ledger_directed_vseq::body();
    soft_test_rm_uncache_d_error_ledger_sequence ledger_seq;
    ledger_seq = soft_test_rm_uncache_d_error_ledger_sequence::type_id::create("ledger_seq");
    ledger_seq.start(null);
endtask:body

`endif
