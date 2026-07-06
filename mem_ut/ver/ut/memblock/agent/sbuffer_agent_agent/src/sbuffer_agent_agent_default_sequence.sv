//=========================================================
//File name    : sbuffer_agent_agent_default_sequence.sv
//Author       : OpenAI_Codex
//Module name  : sbuffer_agent_agent_default_sequence
//Discribution : sbuffer_agent_agent_default_sequence : default sequence
//Date         : 2026-04-12
//=========================================================
`ifndef SBUFFER_AGENT_AGENT_DEFAULT_SEQUENCE__SV
`define SBUFFER_AGENT_AGENT_DEFAULT_SEQUENCE__SV

class sbuffer_agent_agent_default_sequence  extends tcnt_default_sequence_base #(sbuffer_agent_agent_xaction);

    `uvm_object_utils(sbuffer_agent_agent_default_sequence)

    extern function new(string name="sbuffer_agent_agent_default_sequence");
    extern virtual task pre_body();
    extern virtual task body();
    extern virtual task post_body();

endclass:sbuffer_agent_agent_default_sequence

function  sbuffer_agent_agent_default_sequence::new(string name= "sbuffer_agent_agent_default_sequence");
    super.new(name);
endfunction:new

task sbuffer_agent_agent_default_sequence::pre_body();
    if(starting_phase != null)
        starting_phase.raise_objection(this);
endtask:pre_body

task sbuffer_agent_agent_default_sequence::body();
    repeat (10) begin
        `uvm_do(req)
    end
endtask:body

task sbuffer_agent_agent_default_sequence::post_body();
    if(starting_phase != null)
        starting_phase.drop_objection(this);
endtask:post_body

`endif
