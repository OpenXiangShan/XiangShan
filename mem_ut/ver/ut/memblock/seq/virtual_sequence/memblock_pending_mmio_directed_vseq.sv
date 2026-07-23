//=========================================================
//File name    : memblock_pending_mmio_directed_vseq.sv
//Author       : OpenAI_Codex
//Module name  : memblock_pending_mmio_directed_vseq
//Discribution : basicTest pending-MMIO software directed entry
//Date         : 2026-07-22
//=========================================================
`ifndef MEMBLOCK_PENDING_MMIO_DIRECTED_VSEQ__SV
`define MEMBLOCK_PENDING_MMIO_DIRECTED_VSEQ__SV

class memblock_pending_mmio_directed_vseq extends virtual_base_sequence;

    `uvm_object_utils(memblock_pending_mmio_directed_vseq)

    extern function new(string name = "memblock_pending_mmio_directed_vseq");
    extern virtual task body();

endclass:memblock_pending_mmio_directed_vseq

function memblock_pending_mmio_directed_vseq::new(
    string name = "memblock_pending_mmio_directed_vseq"
);
    super.new(name);
endfunction:new

task memblock_pending_mmio_directed_vseq::body();
    soft_test_memblock_pending_mmio_directed_sequence directed_seq;

    require_virtual_sqr();
    `uvm_do_on(directed_seq, p_sequencer)
endtask:body

`endif
