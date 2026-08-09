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
    string l2tlb_main_vseq_name;

  ///aa_test_reg_model   reg_model;
  ///aa_test_reg_adapter reg_adapter;

    function new(string name = "`TC_NAME", uvm_component parent = null);
        super.new(name,parent);
        this.plus_args = new();
        this.l2tlb_main_vseq_name = "";
    endfunction
    extern virtual function void build_phase(uvm_phase phase);
    extern virtual function void connect_phase(uvm_phase phase);
    extern virtual function void end_of_elaboration_phase(uvm_phase phase);
    extern virtual task main_phase(uvm_phase phase);
    extern virtual function bit vseq_starts_l2tlb(input string vseq_name);
    extern virtual function bit l2tlb_dispatch_topology_active();
    extern virtual function void initialize_l2tlb_testcase_lifecycle();
    `uvm_component_utils(`TC_NAME)
endclass

function void `TC_NAME::build_phase(uvm_phase phase);
    uvm_cmdline_processor uvm_cmdline_proc;
    string main_vseq_name;
    bit skip_legacy_l2tlb_default;

    super.build_phase(phase);
    seq_csr_common::reload_from_plus();
    if(!uvm_config_db#(virtual tc_if)::get(this, "", "vif", vif)) begin
        `uvm_fatal(get_type_name(),$sformatf("virtual interface must be set for vif(tc_if)!!!"))
    end
    this.env  =  memblock_env::type_id::create("env", this);
    uvm_cmdline_proc = uvm_cmdline_processor::get_inst();
    main_vseq_name = "";
    if (!$value$plusargs("VSEQ_MAIN=%s", main_vseq_name)) begin
        void'(uvm_cmdline_proc.get_arg_value("+VSEQ_MAIN=", main_vseq_name));
    end
    l2tlb_main_vseq_name = main_vseq_name;
    skip_legacy_l2tlb_default = vseq_starts_l2tlb(main_vseq_name);

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
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_redirect_agent_agent.sqr.main_phase"  , "default_sequence", memblock_redirect_dispatch_base_sequence::type_id::get());
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_sbuffer_agent_agent.sqr.main_phase"  , "default_sequence", sbuffer_mem_access_base_sequence::type_id::get());
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_dcache_agent_agent.sqr.main_phase"  , "default_sequence", dcache_mem__access_base_sequence::type_id::get());
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_int_sink_agent_agent.sqr.main_phase"  , "default_sequence", int_sink_agent_agent_default_sequence::type_id::get());
    if (!skip_legacy_l2tlb_default &&
        seq_csr_common::get_l2tlb_seq_en() &&
        l2tlb_dispatch_topology_active()) begin
        uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_L2tlb_agent_agent.sqr.main_phase"  , "default_sequence", memblock_l2tlb_base_sequence::type_id::get());
    end
    else if (skip_legacy_l2tlb_default) begin
        `uvm_info(get_type_name(),
                  $sformatf("skip legacy L2TLB default sequence because +VSEQ_MAIN=%0s starts L2TLB explicitly",
                            main_vseq_name),
                  UVM_LOW)
    end
    else if (seq_csr_common::get_l2tlb_seq_en() &&
             !l2tlb_dispatch_topology_active()) begin
        `uvm_info(get_type_name(),
                  "skip legacy L2TLB default sequence because testcase has no dispatch topology",
                  UVM_LOW)
    end
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_itlb_agent_agent.sqr.main_phase"  , "default_sequence", itlb_agent_agent_default_sequence::type_id::get());
    uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_other_ctrl_agent_agent.sqr.main_phase"  , "default_sequence", other_ctrl_agent_agent_default_sequence::type_id::get());

endfunction

// 抽象职责：识别会在 VSEQ_MAIN 中显式启动 L2TLB responder 的场景入口。
// 调用者据此只关闭冲突的 legacy L2TLB default sequence；普通 legacy testcase 不传这些 vseq 时保持原启动拓扑。
function bit `TC_NAME::vseq_starts_l2tlb(input string vseq_name);
    if (vseq_name == "memblock_dispatch_real_smoke_vseq") begin
        return 1'b1;
    end
    if (vseq_name == "memblock_dispatch_real_cancel_reconcile_vseq") begin
        return 1'b1;
    end
    return 1'b0;
endfunction

function void `TC_NAME::connect_phase(uvm_phase phase);
    super.connect_phase(phase);
    ///reg_model.default_map.set_sequencer(env.xxx_agt.sqr, reg_adapter);
    ///reg_model.default_map.set_auto_predict(1);
endfunction

function void `TC_NAME::end_of_elaboration_phase(uvm_phase phase);
    super.end_of_elaboration_phase(phase);
    initialize_l2tlb_testcase_lifecycle();
endfunction

function bit `TC_NAME::l2tlb_dispatch_topology_active();
    return 1'b0;
endfunction

function void `TC_NAME::initialize_l2tlb_testcase_lifecycle();
    bit needs_response;
    memblock_sync_pkg::memblock_l2tlb_responder_mode_e responder_mode;
    memblock_sync_pkg::memblock_l2tlb_dispatch_topology_e dispatch_topology;
    memblock_sync_pkg::memblock_l2tlb_start_mode_e start_mode;

    // The public plus is a responder capability request, while the testcase
    // topology decides whether there is an upstream DTLB request stream to
    // service.  Legacy no-dispatch tests must not start an idle responder just
    // because the shared default preset leaves the plus enabled.
    needs_response = seq_csr_common::get_l2tlb_seq_en() &&
                     l2tlb_dispatch_topology_active();
    responder_mode = needs_response ?
        memblock_sync_pkg::MEMBLOCK_L2TLB_RESPONDER_ENABLED :
        memblock_sync_pkg::MEMBLOCK_L2TLB_RESPONDER_DISABLED;
    dispatch_topology = l2tlb_dispatch_topology_active() ?
        memblock_sync_pkg::MEMBLOCK_L2TLB_TOPOLOGY_DISPATCH_ACTIVE :
        memblock_sync_pkg::MEMBLOCK_L2TLB_TOPOLOGY_NO_DISPATCH;
    if (!needs_response) begin
        start_mode = memblock_sync_pkg::MEMBLOCK_L2TLB_START_DISABLED;
    end
    else if (vseq_starts_l2tlb(l2tlb_main_vseq_name)) begin
        start_mode = memblock_sync_pkg::MEMBLOCK_L2TLB_START_EXPLICIT;
    end
    else begin
        start_mode = memblock_sync_pkg::MEMBLOCK_L2TLB_START_DEFAULT;
    end
    memblock_sync_pkg::initialize_l2tlb_testcase_lifecycle(
        responder_mode,
        dispatch_topology,
        start_mode,
        needs_response,
        memblock_sync_pkg::l2tlb_responder_active,
        get_type_name());
    if (seq_csr_common::get_l2tlb_seq_en() &&
        !l2tlb_dispatch_topology_active()) begin
        `uvm_info(get_type_name(),
                  "MEMBLOCK_L2TLB_SEQ_EN is ignored for no-dispatch testcase topology; no DTLB request stream exists",
                  UVM_LOW)
    end
endfunction

task `TC_NAME::main_phase(uvm_phase phase);
    super.main_phase(phase);
    //@(posedge vif.clk);
    //@(posedge vif.rst_n);
    //vif.rst_n = xx;
endtask

`undef TC_NAME

`endif
