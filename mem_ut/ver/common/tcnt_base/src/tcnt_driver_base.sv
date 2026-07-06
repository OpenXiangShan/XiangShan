`ifndef TCNT_DRIVER_BASE__SV
`define TCNT_DRIVER_BASE__SV

class tcnt_driver_base #(type VIF_BUS=int, type cfg_t=tcnt_agent_cfg_base, type seq_item_t=tcnt_data_base) extends uvm_driver#(seq_item_t);
    protected VIF_BUS                     vif;
    cfg_t                                 cfg;
    uvm_event                             notify;

    uvm_analysis_port #(seq_item_t)  drv_item_port;

    `uvm_component_param_utils_begin(tcnt_driver_base #(VIF_BUS,cfg_t,seq_item_t))
        `uvm_field_object(cfg,UVM_ALL_ON)
    `uvm_component_utils_end

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern virtual task reset_phase(uvm_phase phase);
    extern task main_phase(uvm_phase phase);
endclass:tcnt_driver_base

function tcnt_driver_base::new(string name, uvm_component parent);
    super.new(name,parent);
    notify = new("notify");
endfunction:new
function void tcnt_driver_base::build_phase(uvm_phase phase);
    super.build_phase(phase);
    if(!uvm_config_db#(VIF_BUS)::get(this,"","vif",this.vif)) begin
        `uvm_fatal(get_type_name(),$sformatf("build_phase: virtual interface is not set!!!"));
    end
    if(!uvm_config_db#(cfg_t)::get(this,"","cfg",this.cfg)) begin
        `uvm_fatal(get_type_name(),$sformatf("build_phase: cfg is not set!!!"));
    end else begin
        `uvm_info(get_type_name(),$sformatf("build_phase: get_cfg!!!"),UVM_DEBUG);
    end
    this.drv_item_port = new("drv_item_port",this);
endfunction:build_phase
task tcnt_driver_base::reset_phase(uvm_phase phase);
    super.reset_phase(phase);
    phase.raise_objection(this);

    phase.drop_objection(this);
endtask:reset_phase
task tcnt_driver_base::main_phase(uvm_phase phase);
    super.main_phase(phase);
    //while(1) begin
    //    seq_item_port.get_next_item(req);
    //    
    //    seq_item_port.item_done();
    //end
endtask:main_phase

`endif

