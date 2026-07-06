//=========================================================
//File name    : L2tlb_agent_agent_default_sequence.sv
//Author       : OpenAI_Codex
//Module name  : L2tlb_agent_agent_default_sequence
//Discribution : L2tlb_agent_agent_default_sequence : default sequence
//Date         : 2026-04-12
//=========================================================
`ifndef L2TLB_AGENT_AGENT_DEFAULT_SEQUENCE__SV
`define L2TLB_AGENT_AGENT_DEFAULT_SEQUENCE__SV

class L2tlb_agent_agent_default_sequence  extends tcnt_default_sequence_base #(L2tlb_agent_agent_xaction);

    `uvm_object_utils(L2tlb_agent_agent_default_sequence)

    extern function new(string name="L2tlb_agent_agent_default_sequence");
    extern virtual task pre_body();
    extern virtual task body();
    extern virtual task post_body();

endclass:L2tlb_agent_agent_default_sequence

function  L2tlb_agent_agent_default_sequence::new(string name= "L2tlb_agent_agent_default_sequence");
    super.new(name);
endfunction:new

task L2tlb_agent_agent_default_sequence::pre_body();
    if(starting_phase != null)
        starting_phase.raise_objection(this);
endtask:pre_body

task L2tlb_agent_agent_default_sequence::body();
    repeat (10) begin
        `uvm_do(req)
    end
endtask:body

task L2tlb_agent_agent_default_sequence::post_body();
    if(starting_phase != null)
        starting_phase.drop_objection(this);
endtask:post_body

`endif
