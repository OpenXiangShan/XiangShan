//=========================================================
//File name    : redirect_agent_agent_sequencer.sv
//Author       : OpenAI_Codex
//Module name  : redirect_agent_agent_sequencer
//Discribution : redirect_agent_agent_sequencer : sequencer
//Date         : 2026-04-12
//=========================================================
`ifndef REDIRECT_AGENT_AGENT_SEQUENCER__SV
`define REDIRECT_AGENT_AGENT_SEQUENCER__SV

class redirect_agent_agent_sequencer  extends tcnt_sequencer_base #(redirect_agent_agent_xaction);
    `uvm_component_utils(redirect_agent_agent_sequencer)
    extern function new(string name, uvm_component parent);
    extern task main_phase(uvm_phase phase);
endclass:redirect_agent_agent_sequencer

function redirect_agent_agent_sequencer::new(string name, uvm_component parent);
    super.new(name, parent);
endfunction:new

task redirect_agent_agent_sequencer::main_phase(uvm_phase phase);
    super.main_phase(phase);
    phase.raise_objection(this);
    if(!(uvm_config_db#(uvm_object_wrapper)::exists(this, "main_phase", "default_sequence", 0))) begin
        tcnt_default_sequence_base#(seq_item_t) seq;
        `uvm_warning(get_type_name(),"had no get the default_sequence, please check!!")
        seq = tcnt_default_sequence_base#(seq_item_t)::type_id::create("seq");
        seq.starting_phase = phase;
        seq.start(this);
    end
    phase.drop_objection(this);
endtask:main_phase

`endif
