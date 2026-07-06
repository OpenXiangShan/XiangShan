`ifndef TCNT_ENV_BASE__SV
`define TCNT_ENV_BASE__SV

class tcnt_env_base extends uvm_env;
    
    `uvm_component_utils(tcnt_env_base)

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern virtual function void connect_phase(uvm_phase phase);
    extern virtual task pre_reset_phase(uvm_phase phase);
    extern virtual task reset_phase(uvm_phase phase);
    extern virtual task post_reset_phase(uvm_phase phase);
    extern virtual task pre_configure_phase(uvm_phase phase);
    extern virtual task configure_phase(uvm_phase phase);
    extern virtual task post_configure_phase(uvm_phase phase);
    extern virtual task pre_main_phase(uvm_phase phase);
    extern virtual task main_phase(uvm_phase phase);
    extern virtual task post_main_phase(uvm_phase phase);
    extern virtual task shutdown_phase(uvm_phase phase);
    extern virtual function void report_phase(uvm_phase phase);
    
endclass: tcnt_env_base

function tcnt_env_base::new(string name,uvm_component parent);
    super.new(name,parent);
endfunction:new

function void tcnt_env_base::build_phase(uvm_phase phase);
    super.build_phase(phase);
    `uvm_info(get_type_name(),$sformatf("------ build_phase start -------"),UVM_FULL);
endfunction:build_phase

function void tcnt_env_base::connect_phase(uvm_phase phase);
    super.connect_phase(phase);
    `uvm_info(get_type_name(),$sformatf("------ connect_phase start -------"),UVM_FULL);
endfunction:connect_phase

task tcnt_env_base::pre_reset_phase(uvm_phase phase);
    super.pre_reset_phase(phase);
    //`uvm_info(get_type_name(),$sformatf("------ pre_reset_phase start -------"),UVM_FULL);
endtask:pre_reset_phase

task tcnt_env_base::reset_phase(uvm_phase phase);
    super.reset_phase(phase);
    `uvm_info(get_type_name(),$sformatf("------ reset_phase start -------"),UVM_FULL);
endtask:reset_phase

task tcnt_env_base::post_reset_phase(uvm_phase phase);
    super.post_reset_phase(phase);
    //`uvm_info(get_type_name(),$sformatf("------ post_reset_phase start -------"),UVM_FULL);
endtask:post_reset_phase

task tcnt_env_base::pre_configure_phase(uvm_phase phase);
    super.pre_configure_phase(phase);
    //`uvm_info(get_type_name(),$sformatf("------ pre_configure_phase start -------"),UVM_FULL);
endtask:pre_configure_phase

task tcnt_env_base::configure_phase(uvm_phase phase);
    super.configure_phase(phase);
    `uvm_info(get_type_name(),$sformatf("------ configure_phase start -------"),UVM_FULL);
endtask:configure_phase

task tcnt_env_base::post_configure_phase(uvm_phase phase);
    super.post_configure_phase(phase);
    //`uvm_info(get_type_name(),$sformatf("------ post_configure_phase start -------"),UVM_FULL);
endtask:post_configure_phase

task tcnt_env_base::pre_main_phase(uvm_phase phase);
    super.pre_main_phase(phase);
    //`uvm_info(get_type_name(),$sformatf("------ pre_main_phase start -------"),UVM_FULL);
endtask:pre_main_phase

task tcnt_env_base::main_phase(uvm_phase phase);
    super.main_phase(phase);
    `uvm_info(get_type_name(),$sformatf("------ main_phase start -------"),UVM_FULL);
endtask:main_phase

task tcnt_env_base::post_main_phase(uvm_phase phase);
    super.post_main_phase(phase);
    //`uvm_info(get_type_name(),$sformatf("------ post_main_phase start -------"),UVM_FULL);
endtask:post_main_phase

task tcnt_env_base::shutdown_phase(uvm_phase phase);
    super.shutdown_phase(phase);
    `uvm_info(get_type_name(),$sformatf("------ shutdown_phase start -------"),UVM_FULL);
endtask:shutdown_phase

function void tcnt_env_base::report_phase(uvm_phase phase);
    super.report_phase(phase);
    `uvm_info(get_type_name(),$sformatf("------ report_phase start -------"),UVM_FULL);
endfunction:report_phase

`endif

