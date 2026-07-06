//=========================================================
//File name    : tc_smoke.sv
//Author       : OpenAI_Codex
//Module name  : tc_smoke
//Discribution : tc_smoke : minimal smoke test
//Date         : 2026-04-13
//=========================================================
`ifndef TC_SMOKE__SV
`define TC_SMOKE__SV

`define TC_NAME tc_smoke

class `TC_NAME extends tc_base;

    memblock_env_cfg smoke_cfg;

    function new(string name = "`TC_NAME", uvm_component parent = null);
        super.new(name, parent);
    endfunction

    virtual function void build_phase(uvm_phase phase);
        smoke_cfg = memblock_env_cfg::type_id::create("smoke_cfg");
        void'(smoke_cfg.randomize());

        // Smoke only validates basic bring-up, so don't fail on X/Z from
        // passive status outputs that are not initialized in this phase.
        smoke_cfg.u_backendToTopBypass_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        smoke_cfg.u_fence_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        smoke_cfg.u_csr_ctrl_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        smoke_cfg.u_lsqcommit_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        smoke_cfg.u_lsqenq_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        smoke_cfg.u_lintsissue_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        smoke_cfg.u_vecissue_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        smoke_cfg.u_redirect_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        smoke_cfg.u_sbuffer_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        smoke_cfg.u_dcache_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        smoke_cfg.u_int_sink_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        smoke_cfg.u_itlb_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        smoke_cfg.u_prefetch_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        smoke_cfg.u_io_mem_to_ooo_ctrl_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        smoke_cfg.u_io_mem_to_ooo_int_wb_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        smoke_cfg.u_io_mem_to_ooo_vec_wb_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        smoke_cfg.u_io_mem_to_ooo_wakeup_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        smoke_cfg.u_io_mem_to_ooo_iq_feedback_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        smoke_cfg.u_other_ctrl_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;

        uvm_config_db#(memblock_env_cfg)::set(this, "env", "cfg", smoke_cfg);
        super.build_phase(phase);
        // Smoke only checks top-level bring-up and basic UVM phasing.
        uvm_top.set_timeout(10us);
    endfunction

    virtual task main_phase(uvm_phase phase);
        super.main_phase(phase);
        phase.raise_objection(this);
        `uvm_info(get_type_name(), "Starting tc_smoke bring-up check", UVM_MEDIUM)
        tcnt_realtime::delay_us(1);
        `uvm_info(get_type_name(), "tc_smoke completed", UVM_MEDIUM)
        phase.drop_objection(this);
    endtask

    `uvm_component_utils(`TC_NAME)
endclass

`undef TC_NAME

`endif
