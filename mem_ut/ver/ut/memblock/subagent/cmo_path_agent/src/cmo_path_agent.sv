`ifndef CMO_PATH_AGENT_SV
`define CMO_PATH_AGENT_SV
class cmo_path_agent extends uvm_agent;
    `uvm_component_utils(cmo_path_agent)
    cmo_path_agent_cfg cfg;
    cmo_path_agent_monitor mon;
    uvm_analysis_port#(cmo_path_agent_xaction) mon_item_port;
    function new(string name,uvm_component parent);
        super.new(name,parent); mon_item_port=new("mon_item_port",this);
    endfunction
    function void build_phase(uvm_phase phase);
        super.build_phase(phase);
        if(!uvm_config_db#(cmo_path_agent_cfg)::get(this,"","cfg",cfg))
            cfg=cmo_path_agent_cfg::type_id::create("cfg");
        is_active=UVM_PASSIVE;
        mon=cmo_path_agent_monitor::type_id::create("mon",this);
    endfunction
    function void connect_phase(uvm_phase phase);
        super.connect_phase(phase); mon.mon_item_port.connect(mon_item_port);
    endfunction
endclass
`endif
