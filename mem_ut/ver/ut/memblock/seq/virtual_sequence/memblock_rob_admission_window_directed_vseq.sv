//=========================================================
//File name    : memblock_rob_admission_window_directed_vseq.sv
//Author       : OpenAI_Codex
//Module name  : memblock_rob_admission_window_directed_vseq
//Discribution : ROB admission window software entry
//Date         : 2026-10-13
//=========================================================
`ifndef MEMBLOCK_ROB_ADMISSION_WINDOW_DIRECTED_VSEQ__SV
`define MEMBLOCK_ROB_ADMISSION_WINDOW_DIRECTED_VSEQ__SV

class memblock_rob_admission_window_directed_vseq extends virtual_base_sequence;

    `uvm_object_utils(memblock_rob_admission_window_directed_vseq)

    extern function new(string name = "memblock_rob_admission_window_directed_vseq");
    extern virtual task body();

endclass:memblock_rob_admission_window_directed_vseq

function memblock_rob_admission_window_directed_vseq::new(
    string name = "memblock_rob_admission_window_directed_vseq"
);
    super.new(name);
endfunction:new

task memblock_rob_admission_window_directed_vseq::body();
    soft_test_memblock_rob_admission_window_sequence boundary_seq;

    require_virtual_sqr();
    `uvm_do_on(boundary_seq, p_sequencer)
endtask:body

`endif
