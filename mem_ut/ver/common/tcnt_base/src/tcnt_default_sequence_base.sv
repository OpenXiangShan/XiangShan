`ifndef TCNT_DEFAULT_SEQUENCE_BASE__SV
`define TCNT_DEFAULT_SEQUENCE_BASE__SV

class tcnt_default_sequence_base #(type seq_item_t=tcnt_data_base) extends uvm_sequence#(seq_item_t);
    
    `uvm_object_param_utils(tcnt_default_sequence_base#(seq_item_t))

    extern function new(string name="tcnt_default_sequence_base");
    extern virtual task pre_body();
    extern virtual task body();
    extern virtual task post_body();

endclass:tcnt_default_sequence_base

function  tcnt_default_sequence_base::new(string name= "tcnt_default_sequence_base");
    super.new(name);
endfunction:new

task tcnt_default_sequence_base::pre_body();  
    if(starting_phase != null) 
        starting_phase.raise_objection(this);
endtask:pre_body

task tcnt_default_sequence_base::body();
    //repeat (10) begin
    //    `uvm_do(req)
    //end
endtask:body

task tcnt_default_sequence_base::post_body();
    if(starting_phase != null) 
        starting_phase.drop_objection(this);
endtask:post_body

`endif

