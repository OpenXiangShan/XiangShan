`ifndef TCNT_AGENT_BASE__SV
`define TCNT_AGENT_BASE__SV

class tcnt_agent_base #(type VIF_BUS=int, 
                      type cfg_t=tcnt_agent_cfg_base,
                      type seq_t=tcnt_data_base, 
                      type sqr_t=tcnt_sequencer_base, 
                      type drv_t=tcnt_driver_base#(VIF_BUS,cfg_t,seq_t), 
                      type mon_t=tcnt_monitor_base#(VIF_BUS,cfg_t,seq_t)) extends uvm_agent;

    uvm_analysis_port #(seq_t)  drv_item_port;
    uvm_analysis_port #(seq_t)  mon_item_port;

    protected VIF_BUS                        vif;
    cfg_t                                    cfg;
    sqr_t                                    sqr;
    drv_t                                    drv;
    mon_t                                    mon;

    `uvm_component_param_utils_begin(tcnt_agent_base#(VIF_BUS,cfg_t,seq_t,sqr_t,drv_t,mon_t))
        `uvm_field_object(cfg,UVM_ALL_ON)
        `uvm_field_object(sqr,UVM_ALL_ON)
        `uvm_field_object(drv,UVM_ALL_ON)
        `uvm_field_object(mon,UVM_ALL_ON)
    `uvm_component_utils_end    

    extern function new(string name, uvm_component parent);
    extern virtual function void build_phase(uvm_phase phase);
    extern virtual function void connect_phase(uvm_phase phase);
endclass:tcnt_agent_base

function tcnt_agent_base::new(string name,uvm_component parent);
    super.new(name,parent);
endfunction:new
function void tcnt_agent_base::build_phase(uvm_phase phase);
    super.build_phase(phase);
    //agent get interface, then set interface to driver and monitor
    if(!uvm_config_db#(VIF_BUS)::get(this,"","vif",this.vif)) begin
        `uvm_fatal(get_type_name(),$sformatf("build_phase: virtual interface is not set!!!"));
    end
    uvm_config_db#(VIF_BUS)::set(this,"*","vif",this.vif);

    //agent get cfg, then set cfg to driver and monitor
    if(!uvm_config_db#(cfg_t)::get(this,"","cfg",this.cfg)) begin
        cfg = cfg_t::type_id::create("cfg",this);
        assert(cfg.randomize());
        `uvm_fatal(get_type_name(),$sformatf("build_phase: cfg is not set, create and randomize by agent_self!!!"));
    end else begin
        `uvm_info(get_type_name(),$sformatf("build_phase: get_cfg !!!"),UVM_DEBUG);
    end
    uvm_config_db#(cfg_t)::set(this,"*","cfg",this.cfg);

    //instance sqr, drv and mon
    if(this.cfg.sqr_sw==tcnt_dec_base::ON) begin
        sqr = sqr_t::type_id::create("sqr",this);
    end
    if(this.cfg.drv_sw==tcnt_dec_base::ON) begin
        drv = drv_t::type_id::create("drv",this);
        is_active = UVM_ACTIVE;
        this.drv_item_port = new("drv_item_port", this);
    end
    if(this.cfg.mon_sw==tcnt_dec_base::ON) begin
        mon = mon_t::type_id::create("mon",this);
        this.mon_item_port = new("mon_item_port", this);
    end
endfunction:build_phase
function void tcnt_agent_base::connect_phase(uvm_phase phase);
    super.connect_phase(phase);
    if(this.cfg.drv_sw==tcnt_dec_base::ON && this.cfg.sqr_sw==tcnt_dec_base::ON) begin
        drv.seq_item_port.connect(sqr.seq_item_export);
    end
    if(this.cfg.drv_sw==tcnt_dec_base::ON) begin
        this.drv.drv_item_port.connect(this.drv_item_port);
    end
    if(this.cfg.mon_sw==tcnt_dec_base::ON) begin
        this.mon.mon_item_port.connect(this.mon_item_port);
    end
endfunction:connect_phase

`endif

