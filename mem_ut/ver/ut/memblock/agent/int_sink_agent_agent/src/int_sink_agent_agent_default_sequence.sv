//=========================================================
//File name    : int_sink_agent_agent_default_sequence.sv
//Author       : OpenAI_Codex
//Module name  : int_sink_agent_agent_default_sequence
//Discribution : int_sink_agent_agent_default_sequence : default sequence
//Date         : 2026-04-12
//=========================================================
`ifndef INT_SINK_AGENT_AGENT_DEFAULT_SEQUENCE__SV
`define INT_SINK_AGENT_AGENT_DEFAULT_SEQUENCE__SV

class int_sink_agent_agent_default_sequence  extends tcnt_default_sequence_base #(int_sink_agent_agent_xaction);

    `uvm_object_utils(int_sink_agent_agent_default_sequence)

    extern function new(string name="int_sink_agent_agent_default_sequence");
    extern virtual task pre_body();
    extern virtual task body();
    extern virtual task post_body();

endclass:int_sink_agent_agent_default_sequence

function  int_sink_agent_agent_default_sequence::new(string name= "int_sink_agent_agent_default_sequence");
    super.new(name);
endfunction:new

task int_sink_agent_agent_default_sequence::pre_body();
    if(starting_phase != null)
        starting_phase.raise_objection(this);
endtask:pre_body

task int_sink_agent_agent_default_sequence::body();
    repeat (10) begin
        `uvm_do(req)
    end
endtask:body

task int_sink_agent_agent_default_sequence::post_body();
    if(starting_phase != null)
        starting_phase.drop_objection(this);
endtask:post_body

`endif
