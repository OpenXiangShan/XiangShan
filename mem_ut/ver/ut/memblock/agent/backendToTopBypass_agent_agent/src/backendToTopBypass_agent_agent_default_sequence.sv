//=========================================================
//File name    : backendToTopBypass_agent_agent_default_sequence.sv
//Author       : OpenAI_Codex
//Module name  : backendToTopBypass_agent_agent_default_sequence
//Discribution : backendToTopBypass_agent_agent_default_sequence : default sequence
//Date         : 2026-04-12
//=========================================================
`ifndef BACKENDTOTOPBYPASS_AGENT_AGENT_DEFAULT_SEQUENCE__SV
`define BACKENDTOTOPBYPASS_AGENT_AGENT_DEFAULT_SEQUENCE__SV

class backendToTopBypass_agent_agent_default_sequence  extends tcnt_default_sequence_base #(backendToTopBypass_agent_agent_xaction);

    `uvm_object_utils(backendToTopBypass_agent_agent_default_sequence)

    extern function new(string name="backendToTopBypass_agent_agent_default_sequence");
    extern virtual task pre_body();
    extern virtual task body();
    extern virtual task post_body();

endclass:backendToTopBypass_agent_agent_default_sequence

function  backendToTopBypass_agent_agent_default_sequence::new(string name= "backendToTopBypass_agent_agent_default_sequence");
    super.new(name);
endfunction:new

task backendToTopBypass_agent_agent_default_sequence::pre_body();
    if(starting_phase != null)
        starting_phase.raise_objection(this);
endtask:pre_body

task backendToTopBypass_agent_agent_default_sequence::body();
    repeat (10) begin
        `uvm_do(req)
    end
endtask:body

task backendToTopBypass_agent_agent_default_sequence::post_body();
    if(starting_phase != null)
        starting_phase.drop_objection(this);
endtask:post_body

`endif
