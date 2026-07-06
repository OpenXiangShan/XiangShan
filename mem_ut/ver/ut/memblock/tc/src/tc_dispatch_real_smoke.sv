//=========================================================
//File name    : tc_dispatch_real_smoke.sv
//Author       : OpenAI_Codex
//Module name  : tc_dispatch_real_smoke
//Discribution : real DUT dispatch minimum smoke test
//Date         : 2026-05-19
//=========================================================
`ifndef TC_DISPATCH_REAL_SMOKE__SV
`define TC_DISPATCH_REAL_SMOKE__SV

class tc_dispatch_real_smoke extends tc_base;

    memblock_env_cfg real_smoke_cfg;

    `uvm_component_utils(tc_dispatch_real_smoke)

    function new(string name = "tc_dispatch_real_smoke", uvm_component parent = null);
        super.new(name, parent);
    endfunction:new

    virtual function void build_phase(uvm_phase phase);
        seq_csr_common::reload_from_plus();

        real_smoke_cfg = memblock_env_cfg::type_id::create("real_smoke_cfg");
        void'(real_smoke_cfg.randomize());
        configure_real_smoke_env_cfg(real_smoke_cfg);
        uvm_config_db#(memblock_env_cfg)::set(this, "env", "cfg", real_smoke_cfg);

        super.build_phase(phase);
        configure_real_smoke_default_sequences();
        uvm_top.set_timeout(100us);
    endfunction:build_phase

    virtual function void configure_real_smoke_env_cfg(input memblock_env_cfg cfg);
        if (cfg == null) begin
            `uvm_fatal(get_type_name(), "configure_real_smoke_env_cfg got null cfg")
        end

        cfg.u_backendToTopBypass_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_fence_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_csr_ctrl_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_lsqcommit_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_lsqenq_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_lintsissue_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_vecissue_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_redirect_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_sbuffer_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_dcache_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_int_sink_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_L2tlb_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_itlb_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_prefetch_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_io_mem_to_ooo_ctrl_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_io_mem_to_ooo_int_wb_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_io_mem_to_ooo_vec_wb_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_io_mem_to_ooo_wakeup_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_io_mem_to_ooo_iq_feedback_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;
        cfg.u_other_ctrl_agent_agent_cfg.drv_mode = tcnt_dec_base::DRV_0;

        cfg.u_backendToTopBypass_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_fence_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_csr_ctrl_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_lsqcommit_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_lsqenq_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_lintsissue_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_vecissue_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_redirect_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_sbuffer_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_dcache_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_int_sink_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_L2tlb_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_itlb_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_prefetch_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_io_mem_to_ooo_ctrl_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_io_mem_to_ooo_int_wb_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_io_mem_to_ooo_vec_wb_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_io_mem_to_ooo_wakeup_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_io_mem_to_ooo_iq_feedback_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
        cfg.u_other_ctrl_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF;
    endfunction:configure_real_smoke_env_cfg

    virtual function void configure_real_smoke_default_sequences();
        uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_backendToTopBypass_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(backendToTopBypass_agent_agent_xaction)::type_id::get());
        uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_fence_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(fence_agent_agent_xaction)::type_id::get());
        uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_csr_ctrl_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(csr_ctrl_agent_agent_xaction)::type_id::get());
        uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_vecissue_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(vecissue_agent_agent_xaction)::type_id::get());
        uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_redirect_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(redirect_agent_agent_xaction)::type_id::get());
        uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_int_sink_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(int_sink_agent_agent_xaction)::type_id::get());
        uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_itlb_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(itlb_agent_agent_xaction)::type_id::get());
        uvm_config_db#(uvm_object_wrapper)::set(this, "env.u_other_ctrl_agent_agent.sqr.main_phase", "default_sequence", tcnt_default_sequence_base#(other_ctrl_agent_agent_xaction)::type_id::get());
    endfunction:configure_real_smoke_default_sequences

    virtual task run_real_smoke_sequence();
        memblock_main_dispatch_auto_build_main_table_base_sequence real_smoke_seq;

        `uvm_info(get_type_name(), "Starting real dispatch smoke", UVM_MEDIUM)
        real_smoke_seq = memblock_main_dispatch_auto_build_main_table_base_sequence::type_id::create("real_smoke_seq");
        if (real_smoke_seq == null) begin
            `uvm_fatal(get_type_name(), "failed to create memblock_main_dispatch_auto_build_main_table_base_sequence")
        end
        real_smoke_seq.start(null);
        `uvm_info(get_type_name(), "tc_dispatch_real_smoke completed", UVM_MEDIUM)
    endtask:run_real_smoke_sequence

    virtual task main_phase(uvm_phase phase);
        phase.raise_objection(this);
        memblock_sync_pkg::dispatch_real_smoke_active = 1'b1;
        super.main_phase(phase);
        run_real_smoke_sequence();
        memblock_sync_pkg::dispatch_real_smoke_active = 1'b0;
        phase.drop_objection(this);
    endtask:main_phase

endclass:tc_dispatch_real_smoke

`endif
