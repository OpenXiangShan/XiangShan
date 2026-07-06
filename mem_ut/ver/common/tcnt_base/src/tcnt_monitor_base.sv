`ifndef TCNT_MONITOR_BASE__SV
`define TCNT_MONITOR_BASE__SV

class tcnt_monitor_base #(type VIF_BUS=int, type cfg_t=tcnt_agent_cfg_base, type seq_item_t=tcnt_data_base) extends uvm_monitor;
    uvm_event                                notify;
    protected  VIF_BUS                       vif;
    cfg_t                                    cfg;

    uvm_analysis_port #(seq_item_t)  mon_item_port;

    `uvm_component_param_utils_begin(tcnt_monitor_base #(VIF_BUS,cfg_t,seq_item_t))
        `uvm_field_object(cfg,UVM_ALL_ON)
    `uvm_component_utils_end

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern task run_phase(uvm_phase phase);
    extern task mon_data();
endclass:tcnt_monitor_base

function tcnt_monitor_base::new(string name, uvm_component parent);
    super.new(name,parent);
    notify = new("notify");
endfunction:new
function void tcnt_monitor_base::build_phase(uvm_phase phase);
    super.build_phase(phase);
    if(!uvm_config_db#(VIF_BUS)::get(this,"","vif",vif)) begin
        `uvm_fatal(get_type_name(),$sformatf("build_phase: virtual interface is not set!!!"));
    end
    if(!uvm_config_db#(cfg_t)::get(this,"","cfg",this.cfg)) begin
        `uvm_fatal(get_type_name(),$sformatf("build_phase: cfg is not set!!!"));
    end
    this.mon_item_port = new("mon_item_port",this);
endfunction:build_phase
task tcnt_monitor_base::run_phase(uvm_phase phase);
    super.run_phase(phase);
    this.mon_data();
endtask:run_phase
task tcnt_monitor_base::mon_data();
    seq_item_t mon_tr;
    //while(1) begin
    //    @(posedge this.vif.clk);
    //    if(this.cfg.xz_sw==tcnt_dec_base::ON & this.vif.rst_n==1'b1) begin
    //    end
    //    //mon_tr = new("mon_tr");
    //    //this.mon_item_port.write(mon_tr);
    //end
endtask:mon_data

`endif

