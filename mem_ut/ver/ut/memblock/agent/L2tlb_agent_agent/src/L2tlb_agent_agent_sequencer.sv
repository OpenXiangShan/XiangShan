//=========================================================
//File name    : L2tlb_agent_agent_sequencer.sv
//Author       : OpenAI_Codex
//Module name  : L2tlb_agent_agent_sequencer
//Discribution : L2tlb_agent_agent_sequencer : sequencer
//Date         : 2026-04-12
//=========================================================
`ifndef L2TLB_AGENT_AGENT_SEQUENCER__SV
`define L2TLB_AGENT_AGENT_SEQUENCER__SV

class L2tlb_agent_agent_sequencer  extends tcnt_sequencer_base #(L2tlb_agent_agent_xaction);
    `uvm_component_utils(L2tlb_agent_agent_sequencer)
    extern function new(string name, uvm_component parent);
    extern task main_phase(uvm_phase phase);
endclass:L2tlb_agent_agent_sequencer

function L2tlb_agent_agent_sequencer::new(string name, uvm_component parent);
    super.new(name, parent);
endfunction:new

task L2tlb_agent_agent_sequencer::main_phase(uvm_phase phase);
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
