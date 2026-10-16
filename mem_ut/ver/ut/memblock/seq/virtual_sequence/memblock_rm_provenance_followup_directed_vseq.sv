//=========================================================
//File name    : memblock_rm_provenance_followup_directed_vseq.sv
//Module name  : memblock_rm_provenance_followup_directed_vseq
//Discribution : RM provenance software-directed entry
//=========================================================
`ifndef MEMBLOCK_RM_PROVENANCE_FOLLOWUP_DIRECTED_VSEQ__SV
`define MEMBLOCK_RM_PROVENANCE_FOLLOWUP_DIRECTED_VSEQ__SV

class memblock_rm_provenance_followup_directed_vseq extends virtual_base_sequence;

    `uvm_object_utils(memblock_rm_provenance_followup_directed_vseq)

    extern function new(
        string name = "memblock_rm_provenance_followup_directed_vseq");
    extern virtual task body();

endclass:memblock_rm_provenance_followup_directed_vseq

function memblock_rm_provenance_followup_directed_vseq::new(
    string name = "memblock_rm_provenance_followup_directed_vseq");
    super.new(name);
endfunction:new

task memblock_rm_provenance_followup_directed_vseq::body();
    soft_test_rm_provenance_followup_sequence directed_seq;

    require_virtual_sqr();
    `uvm_do_on(directed_seq, p_sequencer)
endtask:body

`endif
