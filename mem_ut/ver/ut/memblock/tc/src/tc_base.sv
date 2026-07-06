//=========================================================
//File name    : tc_base.sv
//Author       : OpenAI_Codex
//Module name  : tc_base
//Discribution : tc_base : TC basic
//Date         : 2026-04-12
//=========================================================
`ifndef TC_BASE__SV
`define TC_BASE__SV

`define TC_NAME tc_base

class `TC_NAME extends tcnt_test_base;

    virtual tc_if vif;
    memblock_env  env;
    plus plus_args;

  ///aa_test_reg_model   reg_model;
  ///aa_test_reg_adapter reg_adapter;

    function new(string name = "`TC_NAME", uvm_component parent = null);
        super.new(name,parent);
        this.plus_args = new();
    endfunction
    extern virtual function void build_phase(uvm_phase phase);
    extern virtual function void connect_phase(uvm_phase phase);
    extern virtual function void end_of_elaboration_phase(uvm_phase phase);
    extern virtual task main_phase(uvm_phase phase);
    `uvm_component_utils(`TC_NAME)
endclass

function void `TC_NAME::build_phase(uvm_phase phase);
    super.build_phase(phase);
    if(!uvm_config_db#(virtual tc_if)::get(this, "", "vif", vif)) begin
        `uvm_fatal(get_type_name(),$sformatf("virtual interface must be set for vif(tc_if)!!!"))
    end
    this.env  =  memblock_env::type_id::create("env", this);

  ///reg_model = aa_test_reg_model::type_id::create("reg_model",this);
  ///reg_model.configure(null, "");
  ///reg_model.build();
  ///reg_model.lock_model();
  ///reg_model.reset();
    ///reg_model.set_hdl_path_root("top_tb.dut");
  ///env.reg_model = this.reg_model;
  ///reg_adapter = new("reg_adapter");

    //factory default_sequence
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_backendToTopBypass_agent_agent.sqr.main_phase"  , "default_sequence", backendToTopBypass_agent_agent_default_sequence::type_id::get());
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_fence_agent_agent.sqr.main_phase"  , "default_sequence", fence_agent_agent_default_sequence::type_id::get());
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_csr_ctrl_agent_agent.sqr.main_phase"  , "default_sequence", csr_ctrl_agent_agent_default_sequence::type_id::get());
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_lsqcommit_agent_agent.sqr.main_phase"  , "default_sequence", memblock_lsqcommit_dispatch_base_sequence::type_id::get());
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_lsqenq_agent_agent.sqr.main_phase"  , "default_sequence", memblock_lsqenq_dispatch_base_sequence::type_id::get());
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_lintsissue_agent_agent.sqr.main_phase"  , "default_sequence", memblock_issue_dispatch_base_sequence::type_id::get());
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_vecissue_agent_agent.sqr.main_phase"  , "default_sequence", vecissue_agent_agent_default_sequence::type_id::get());
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_redirect_agent_agent.sqr.main_phase"  , "default_sequence", memblock_redirect_dispatch_base_sequence::type_id::get());
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_sbuffer_agent_agent.sqr.main_phase"  , "default_sequence", sbuffer_mem_access_base_sequence::type_id::get());
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_dcache_agent_agent.sqr.main_phase"  , "default_sequence", dcache_mem__access_base_sequence::type_id::get());
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_int_sink_agent_agent.sqr.main_phase"  , "default_sequence", int_sink_agent_agent_default_sequence::type_id::get());
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_L2tlb_agent_agent.sqr.main_phase"  , "default_sequence", memblock_l2tlb_base_sequence::type_id::get());
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_itlb_agent_agent.sqr.main_phase"  , "default_sequence", itlb_agent_agent_default_sequence::type_id::get());
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_other_ctrl_agent_agent.sqr.main_phase"  , "default_sequence", other_ctrl_agent_agent_default_sequence::type_id::get());

endfunction
function void `TC_NAME::connect_phase(uvm_phase phase);
    super.connect_phase(phase);
    ///reg_model.default_map.set_sequencer(env.xxx_agt.sqr, reg_adapter);
    ///reg_model.default_map.set_auto_predict(1);
endfunction

function void `TC_NAME::end_of_elaboration_phase(uvm_phase phase);
    super.end_of_elaboration_phase(phase);
endfunction

task `TC_NAME::main_phase(uvm_phase phase);
    super.main_phase(phase);
    //@(posedge vif.clk);
    //@(posedge vif.rst_n);
    //vif.rst_n = xx;
endtask

`undef TC_NAME

`endif
