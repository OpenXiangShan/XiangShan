//=========================================================
//File name    : virtual_base_sequence.sv
//Author       : OpenAI_Codex
//Module name  : virtual_base_sequence
//Discribution : memblock virtual sequence base
//Date         : 2026-07-03
//=========================================================
`ifndef VIRTUAL_BASE_SEQUENCE__SV
`define VIRTUAL_BASE_SEQUENCE__SV

class virtual_base_sequence extends uvm_sequence;

    `uvm_object_utils(virtual_base_sequence)
    `uvm_declare_p_sequencer(memblock_virtual_sequencer)

    extern function new(string name = "virtual_base_sequence");
    extern virtual task pre_body();
    extern virtual task body();
    extern virtual task post_body();
    extern virtual function void require_virtual_sqr();
    extern virtual function void require_agent_sqr(input string agent_name,
                                                   input uvm_sequencer_base sqr);

endclass:virtual_base_sequence

function virtual_base_sequence::new(string name = "virtual_base_sequence");
    super.new(name);
endfunction:new

task virtual_base_sequence::pre_body();
    if (starting_phase != null) begin
        starting_phase.raise_objection(this);
    end
endtask:pre_body

task virtual_base_sequence::body();
    `uvm_info(get_type_name(), "virtual_base_sequence body is empty", UVM_LOW)
endtask:body

task virtual_base_sequence::post_body();
    if (starting_phase != null) begin
        starting_phase.drop_objection(this);
    end
endtask:post_body

function void virtual_base_sequence::require_virtual_sqr();
    if (p_sequencer == null) begin
        `uvm_fatal(get_type_name(), "p_sequencer is null; start this sequence on memblock_env.vsqr")
    end
endfunction:require_virtual_sqr

function void virtual_base_sequence::require_agent_sqr(input string agent_name,
                                                       input uvm_sequencer_base sqr);
    require_virtual_sqr();
    if (sqr == null) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("required agent sequencer is null: %0s", agent_name))
    end
endfunction:require_agent_sqr

`endif
