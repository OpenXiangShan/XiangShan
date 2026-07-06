`ifndef TCNT_RM_BASE__SV
`define TCNT_RM_BASE__SV

class tcnt_rm_base #(type seq_item_t=tcnt_data_base) extends uvm_component;

    `uvm_component_param_utils(tcnt_rm_base #(seq_item_t))

    uvm_analysis_port #(seq_item_t)  rm_item_exp_port;
    uvm_analysis_port #(seq_item_t)  rm_item_act_port;
   
    extern         function      new(string name , uvm_component parent);
    extern         function void build_phase(uvm_phase phase);
    extern virtual task main_phase(uvm_phase phase);
    extern virtual task body();

endclass
function tcnt_rm_base::new(string name , uvm_component parent);
    super.new(name, parent);
endfunction
function void tcnt_rm_base::build_phase(uvm_phase phase);
    super.build_phase(phase);
    this.rm_item_exp_port = new("rm_item_exp_port", this);
    this.rm_item_act_port = new("rm_item_act_port", this);
endfunction
task tcnt_rm_base::main_phase(uvm_phase phase);
    super.main_phase(phase);
    this.body();
endtask
task tcnt_rm_base::body();
    //main process
endtask

`endif

