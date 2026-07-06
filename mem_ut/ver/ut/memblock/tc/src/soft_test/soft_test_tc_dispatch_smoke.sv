//=========================================================
//File name    : soft_test_tc_dispatch_smoke.sv
//Author       : OpenAI_Codex
//Module name  : soft_test_tc_dispatch_smoke
//Discribution : software-only dispatch end-to-end smoke test
//Date         : 2026-05-19
//=========================================================
`ifndef SOFT_TEST_TC_DISPATCH_SMOKE__SV
`define SOFT_TEST_TC_DISPATCH_SMOKE__SV

class soft_test_tc_dispatch_smoke extends tc_smoke;

    `uvm_component_utils(soft_test_tc_dispatch_smoke)

    function new(string name = "soft_test_tc_dispatch_smoke", uvm_component parent = null);
        super.new(name, parent);
    endfunction:new

    virtual function void build_phase(uvm_phase phase);
        super.build_phase(phase);
        configure_software_smoke_default_sequences();
    endfunction:build_phase

    virtual task run_dispatch_smoke_sequence();
        soft_test_memblock_dispatch_smoke_sequence smoke_seq;

        `uvm_info(get_type_name(), "Starting software dispatch smoke", UVM_MEDIUM)
        smoke_seq = soft_test_memblock_dispatch_smoke_sequence::type_id::create("smoke_seq");
        if (smoke_seq == null) begin
            `uvm_fatal(get_type_name(), "failed to create soft_test_memblock_dispatch_smoke_sequence")
        end
        smoke_seq.start(null);
        `uvm_info(get_type_name(), "soft_test_tc_dispatch_smoke completed", UVM_MEDIUM)
    endtask:run_dispatch_smoke_sequence

    virtual function void configure_software_smoke_default_sequences();
        uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_backendToTopBypass_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(backendToTopBypass_agent_agent_xaction)::type_id::get());
        uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_fence_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(fence_agent_agent_xaction)::type_id::get());
        uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_csr_ctrl_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(csr_ctrl_agent_agent_xaction)::type_id::get());
        uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_lsqcommit_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(lsqcommit_agent_agent_xaction)::type_id::get());
        uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_lsqenq_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(lsqenq_agent_agent_xaction)::type_id::get());
        uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_lintsissue_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(lintsissue_agent_agent_xaction)::type_id::get());
        uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_vecissue_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(vecissue_agent_agent_xaction)::type_id::get());
        uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_redirect_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(redirect_agent_agent_xaction)::type_id::get());
        uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_sbuffer_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(sbuffer_agent_agent_xaction)::type_id::get());
        uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_dcache_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(dcache_agent_agent_xaction)::type_id::get());
        uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_int_sink_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(int_sink_agent_agent_xaction)::type_id::get());
        uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_itlb_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(itlb_agent_agent_xaction)::type_id::get());
        uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_other_ctrl_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(other_ctrl_agent_agent_xaction)::type_id::get());
    endfunction:configure_software_smoke_default_sequences

    virtual task main_phase(uvm_phase phase);
        phase.raise_objection(this);
        super.main_phase(phase);
        run_dispatch_smoke_sequence();
        phase.drop_objection(this);
    endtask:main_phase

endclass:soft_test_tc_dispatch_smoke

class tc_dispatch_smoke extends soft_test_tc_dispatch_smoke;

    `uvm_component_utils(tc_dispatch_smoke)

    function new(string name = "tc_dispatch_smoke", uvm_component parent = null);
        super.new(name, parent);
    endfunction:new

endclass:tc_dispatch_smoke

`endif
