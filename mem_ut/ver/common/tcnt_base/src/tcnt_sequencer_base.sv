`ifndef TCNT_SEQUENCER_BASE__SV
`define TCNT_SEQUENCER_BASE__SV

class tcnt_sequencer_base #(type seq_item_t=tcnt_data_base) extends uvm_sequencer #(seq_item_t);
    `uvm_component_param_utils(tcnt_sequencer_base#(seq_item_t))
    extern function new(string name, uvm_component parent);
    extern task main_phase(uvm_phase phase);
endclass:tcnt_sequencer_base

function tcnt_sequencer_base::new(string name, uvm_component parent);
    super.new(name, parent);
endfunction:new

task tcnt_sequencer_base::main_phase(uvm_phase phase);
    super.main_phase(phase);
    //phase.raise_objection(this);
    //if(!(uvm_config_db#(uvm_object_wrapper)::exists(this, "main_phase", "default_sequence", 0))) begin
    //    tcnt_default_sequence_base#(seq_item_t) seq;
    //    //`uvm_info(get_type_name(),"had no get",UVM_NONE)
    //    seq = tcnt_default_sequence_base#(seq_item_t)::type_id::create("seq");
    //    seq.starting_phase = phase;
    //    seq.start(this);
    //end
    ////else begin
    ////    `uvm_info(get_type_name(),"had get",UVM_NONE)
    ////end
    //phase.drop_objection(this);
endtask:main_phase

`endif

