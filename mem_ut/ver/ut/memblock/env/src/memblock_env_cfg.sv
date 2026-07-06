//=========================================================
//File name    : memblock_env_cfg.sv
//Author       : OpenAI_Codex
//Module name  : memblock_env_cfg
//Discribution : memblock_env_cfg : environment configuration
//Date         : 2026-04-12
//=========================================================
`ifndef MEMBLOCK_ENV_CFG__SV
`define MEMBLOCK_ENV_CFG__SV

class memblock_env_cfg extends uvm_object;

    memblock_user_cfg u_user_cfg;

    rand backendToTopBypass_agent_agent_cfg u_backendToTopBypass_agent_agent_cfg;
    rand fence_agent_agent_cfg     u_fence_agent_agent_cfg;
    rand csr_ctrl_agent_agent_cfg  u_csr_ctrl_agent_agent_cfg;
    rand lsqcommit_agent_agent_cfg u_lsqcommit_agent_agent_cfg;
    rand lsqenq_agent_agent_cfg    u_lsqenq_agent_agent_cfg;
    rand lintsissue_agent_agent_cfg u_lintsissue_agent_agent_cfg;
    rand vecissue_agent_agent_cfg  u_vecissue_agent_agent_cfg;
    rand redirect_agent_agent_cfg  u_redirect_agent_agent_cfg;
    rand sbuffer_agent_agent_cfg   u_sbuffer_agent_agent_cfg;
    rand dcache_agent_agent_cfg    u_dcache_agent_agent_cfg;
    rand int_sink_agent_agent_cfg  u_int_sink_agent_agent_cfg;
    rand L2tlb_agent_agent_cfg     u_L2tlb_agent_agent_cfg;
    rand itlb_agent_agent_cfg      u_itlb_agent_agent_cfg;
    rand prefetch_agent_agent_cfg  u_prefetch_agent_agent_cfg;
    rand io_mem_to_ooo_ctrl_agent_agent_cfg u_io_mem_to_ooo_ctrl_agent_agent_cfg;
    rand io_mem_to_ooo_int_wb_agent_agent_cfg u_io_mem_to_ooo_int_wb_agent_agent_cfg;
    rand io_mem_to_ooo_vec_wb_agent_agent_cfg u_io_mem_to_ooo_vec_wb_agent_agent_cfg;
    rand io_mem_to_ooo_wakeup_agent_agent_cfg u_io_mem_to_ooo_wakeup_agent_agent_cfg;
    rand io_mem_to_ooo_iq_feedback_agent_agent_cfg u_io_mem_to_ooo_iq_feedback_agent_agent_cfg;
    rand other_ctrl_agent_agent_cfg u_other_ctrl_agent_agent_cfg;

    `uvm_object_utils_begin(memblock_env_cfg)
        `uvm_field_object(u_user_cfg, UVM_ALL_ON)
    `uvm_object_utils_end

    extern function new(string name="memblock_env_cfg");
    extern function void pre_randomize();
    extern function void post_randomize();
    extern function void apply_user_cfg();
    extern function void apply_user_agent_ctrl(memblock_user_agent_ctrl user_agent_ctrl,
                                               tcnt_agent_cfg_base agent_cfg);

endclass:memblock_env_cfg

function memblock_env_cfg::new(string  name = "memblock_env_cfg");
    super.new(name);
    this.u_user_cfg = memblock_user_cfg::type_id::create("u_user_cfg");
    this.u_backendToTopBypass_agent_agent_cfg = backendToTopBypass_agent_agent_cfg::type_id::create("u_backendToTopBypass_agent_agent_cfg");
    this.u_fence_agent_agent_cfg   = fence_agent_agent_cfg::type_id::create("u_fence_agent_agent_cfg");
    this.u_csr_ctrl_agent_agent_cfg = csr_ctrl_agent_agent_cfg::type_id::create("u_csr_ctrl_agent_agent_cfg");
    this.u_lsqcommit_agent_agent_cfg = lsqcommit_agent_agent_cfg::type_id::create("u_lsqcommit_agent_agent_cfg");
    this.u_lsqenq_agent_agent_cfg  = lsqenq_agent_agent_cfg::type_id::create("u_lsqenq_agent_agent_cfg");
    this.u_lintsissue_agent_agent_cfg = lintsissue_agent_agent_cfg::type_id::create("u_lintsissue_agent_agent_cfg");
    this.u_vecissue_agent_agent_cfg = vecissue_agent_agent_cfg::type_id::create("u_vecissue_agent_agent_cfg");
    this.u_redirect_agent_agent_cfg = redirect_agent_agent_cfg::type_id::create("u_redirect_agent_agent_cfg");
    this.u_sbuffer_agent_agent_cfg = sbuffer_agent_agent_cfg::type_id::create("u_sbuffer_agent_agent_cfg");
    this.u_dcache_agent_agent_cfg  = dcache_agent_agent_cfg::type_id::create("u_dcache_agent_agent_cfg");
    this.u_int_sink_agent_agent_cfg = int_sink_agent_agent_cfg::type_id::create("u_int_sink_agent_agent_cfg");
    this.u_L2tlb_agent_agent_cfg   = L2tlb_agent_agent_cfg::type_id::create("u_L2tlb_agent_agent_cfg");
    this.u_itlb_agent_agent_cfg    = itlb_agent_agent_cfg::type_id::create("u_itlb_agent_agent_cfg");
    this.u_prefetch_agent_agent_cfg = prefetch_agent_agent_cfg::type_id::create("u_prefetch_agent_agent_cfg");
    this.u_io_mem_to_ooo_ctrl_agent_agent_cfg = io_mem_to_ooo_ctrl_agent_agent_cfg::type_id::create("u_io_mem_to_ooo_ctrl_agent_agent_cfg");
    this.u_io_mem_to_ooo_int_wb_agent_agent_cfg = io_mem_to_ooo_int_wb_agent_agent_cfg::type_id::create("u_io_mem_to_ooo_int_wb_agent_agent_cfg");
    this.u_io_mem_to_ooo_vec_wb_agent_agent_cfg = io_mem_to_ooo_vec_wb_agent_agent_cfg::type_id::create("u_io_mem_to_ooo_vec_wb_agent_agent_cfg");
    this.u_io_mem_to_ooo_wakeup_agent_agent_cfg = io_mem_to_ooo_wakeup_agent_agent_cfg::type_id::create("u_io_mem_to_ooo_wakeup_agent_agent_cfg");
    this.u_io_mem_to_ooo_iq_feedback_agent_agent_cfg = io_mem_to_ooo_iq_feedback_agent_agent_cfg::type_id::create("u_io_mem_to_ooo_iq_feedback_agent_agent_cfg");
    this.u_other_ctrl_agent_agent_cfg = other_ctrl_agent_agent_cfg::type_id::create("u_other_ctrl_agent_agent_cfg");

endfunction:new

function void memblock_env_cfg::pre_randomize();
    super.pre_randomize();
endfunction:pre_randomize

function void memblock_env_cfg::post_randomize();
    super.post_randomize();

    this.u_backendToTopBypass_agent_agent_cfg.sqr_sw = tcnt_dec_base::ON  ;
    this.u_backendToTopBypass_agent_agent_cfg.drv_sw = tcnt_dec_base::ON  ;
    this.u_backendToTopBypass_agent_agent_cfg.mon_sw = tcnt_dec_base::ON ;
    this.u_backendToTopBypass_agent_agent_cfg.channel_id = 0;

    this.u_fence_agent_agent_cfg.sqr_sw = tcnt_dec_base::ON  ;
    this.u_fence_agent_agent_cfg.drv_sw = tcnt_dec_base::ON  ;
    this.u_fence_agent_agent_cfg.mon_sw = tcnt_dec_base::ON ;
    this.u_fence_agent_agent_cfg.channel_id = 1;

    this.u_csr_ctrl_agent_agent_cfg.sqr_sw = tcnt_dec_base::ON  ;
    this.u_csr_ctrl_agent_agent_cfg.drv_sw = tcnt_dec_base::ON  ;
    this.u_csr_ctrl_agent_agent_cfg.mon_sw = tcnt_dec_base::ON ;
    this.u_csr_ctrl_agent_agent_cfg.channel_id = 2;

    this.u_lsqcommit_agent_agent_cfg.sqr_sw = tcnt_dec_base::ON  ;
    this.u_lsqcommit_agent_agent_cfg.drv_sw = tcnt_dec_base::ON  ;
    this.u_lsqcommit_agent_agent_cfg.mon_sw = tcnt_dec_base::ON ;
    this.u_lsqcommit_agent_agent_cfg.channel_id = 3;

    this.u_lsqenq_agent_agent_cfg.sqr_sw = tcnt_dec_base::ON  ;
    this.u_lsqenq_agent_agent_cfg.drv_sw = tcnt_dec_base::ON  ;
    this.u_lsqenq_agent_agent_cfg.mon_sw = tcnt_dec_base::ON ;
    this.u_lsqenq_agent_agent_cfg.channel_id = 4;

    this.u_lintsissue_agent_agent_cfg.sqr_sw = tcnt_dec_base::ON  ;
    this.u_lintsissue_agent_agent_cfg.drv_sw = tcnt_dec_base::ON  ;
    this.u_lintsissue_agent_agent_cfg.mon_sw = tcnt_dec_base::ON ;
    this.u_lintsissue_agent_agent_cfg.channel_id = 5;

    this.u_vecissue_agent_agent_cfg.sqr_sw = tcnt_dec_base::ON  ;
    this.u_vecissue_agent_agent_cfg.drv_sw = tcnt_dec_base::ON  ;
    this.u_vecissue_agent_agent_cfg.mon_sw = tcnt_dec_base::ON ;
    this.u_vecissue_agent_agent_cfg.channel_id = 6;

    this.u_redirect_agent_agent_cfg.sqr_sw = tcnt_dec_base::ON  ;
    this.u_redirect_agent_agent_cfg.drv_sw = tcnt_dec_base::ON  ;
    this.u_redirect_agent_agent_cfg.mon_sw = tcnt_dec_base::ON ;
    this.u_redirect_agent_agent_cfg.channel_id = 7;

    this.u_sbuffer_agent_agent_cfg.sqr_sw = tcnt_dec_base::ON  ;
    this.u_sbuffer_agent_agent_cfg.drv_sw = tcnt_dec_base::ON  ;
    this.u_sbuffer_agent_agent_cfg.mon_sw = tcnt_dec_base::ON ;
    this.u_sbuffer_agent_agent_cfg.channel_id = 8;

    this.u_dcache_agent_agent_cfg.sqr_sw = tcnt_dec_base::ON  ;
    this.u_dcache_agent_agent_cfg.drv_sw = tcnt_dec_base::ON  ;
    this.u_dcache_agent_agent_cfg.mon_sw = tcnt_dec_base::ON ;
    this.u_dcache_agent_agent_cfg.channel_id = 9;

    this.u_int_sink_agent_agent_cfg.sqr_sw = tcnt_dec_base::ON  ;
    this.u_int_sink_agent_agent_cfg.drv_sw = tcnt_dec_base::ON  ;
    this.u_int_sink_agent_agent_cfg.mon_sw = tcnt_dec_base::ON ;
    this.u_int_sink_agent_agent_cfg.channel_id = 10;

    this.u_L2tlb_agent_agent_cfg.sqr_sw = tcnt_dec_base::ON ;
    this.u_L2tlb_agent_agent_cfg.drv_sw = tcnt_dec_base::ON ;
    this.u_L2tlb_agent_agent_cfg.mon_sw = tcnt_dec_base::ON ;
    this.u_L2tlb_agent_agent_cfg.xz_sw = tcnt_dec_base::OFF ;
    this.u_L2tlb_agent_agent_cfg.channel_id = 19;

    this.u_itlb_agent_agent_cfg.sqr_sw = tcnt_dec_base::ON  ;
    this.u_itlb_agent_agent_cfg.drv_sw = tcnt_dec_base::ON  ;
    this.u_itlb_agent_agent_cfg.mon_sw = tcnt_dec_base::ON ;
    this.u_itlb_agent_agent_cfg.channel_id = 11;

    this.u_prefetch_agent_agent_cfg.sqr_sw = tcnt_dec_base::OFF ;
    this.u_prefetch_agent_agent_cfg.drv_sw = tcnt_dec_base::OFF ;
    this.u_prefetch_agent_agent_cfg.mon_sw = tcnt_dec_base::ON ;
    this.u_prefetch_agent_agent_cfg.channel_id = 12;

    this.u_io_mem_to_ooo_ctrl_agent_agent_cfg.sqr_sw = tcnt_dec_base::OFF ;
    this.u_io_mem_to_ooo_ctrl_agent_agent_cfg.drv_sw = tcnt_dec_base::OFF ;
    this.u_io_mem_to_ooo_ctrl_agent_agent_cfg.mon_sw = tcnt_dec_base::ON ;
    this.u_io_mem_to_ooo_ctrl_agent_agent_cfg.channel_id = 13;

    this.u_io_mem_to_ooo_int_wb_agent_agent_cfg.sqr_sw = tcnt_dec_base::OFF ;
    this.u_io_mem_to_ooo_int_wb_agent_agent_cfg.drv_sw = tcnt_dec_base::OFF ;
    this.u_io_mem_to_ooo_int_wb_agent_agent_cfg.mon_sw = tcnt_dec_base::ON ;
    this.u_io_mem_to_ooo_int_wb_agent_agent_cfg.channel_id = 14;

    this.u_io_mem_to_ooo_vec_wb_agent_agent_cfg.sqr_sw = tcnt_dec_base::OFF ;
    this.u_io_mem_to_ooo_vec_wb_agent_agent_cfg.drv_sw = tcnt_dec_base::OFF ;
    this.u_io_mem_to_ooo_vec_wb_agent_agent_cfg.mon_sw = tcnt_dec_base::ON ;
    this.u_io_mem_to_ooo_vec_wb_agent_agent_cfg.channel_id = 15;

    this.u_io_mem_to_ooo_wakeup_agent_agent_cfg.sqr_sw = tcnt_dec_base::OFF ;
    this.u_io_mem_to_ooo_wakeup_agent_agent_cfg.drv_sw = tcnt_dec_base::OFF ;
    this.u_io_mem_to_ooo_wakeup_agent_agent_cfg.mon_sw = tcnt_dec_base::ON ;
    this.u_io_mem_to_ooo_wakeup_agent_agent_cfg.channel_id = 16;

    this.u_io_mem_to_ooo_iq_feedback_agent_agent_cfg.sqr_sw = tcnt_dec_base::OFF ;
    this.u_io_mem_to_ooo_iq_feedback_agent_agent_cfg.drv_sw = tcnt_dec_base::OFF ;
    this.u_io_mem_to_ooo_iq_feedback_agent_agent_cfg.mon_sw = tcnt_dec_base::ON ;
    this.u_io_mem_to_ooo_iq_feedback_agent_agent_cfg.channel_id = 17;

    this.u_other_ctrl_agent_agent_cfg.sqr_sw = tcnt_dec_base::ON  ;
    this.u_other_ctrl_agent_agent_cfg.drv_sw = tcnt_dec_base::ON  ;
    this.u_other_ctrl_agent_agent_cfg.mon_sw = tcnt_dec_base::ON ;
    this.u_other_ctrl_agent_agent_cfg.channel_id = 18;

endfunction:post_randomize

function void memblock_env_cfg::apply_user_agent_ctrl(memblock_user_agent_ctrl user_agent_ctrl,
                                                      tcnt_agent_cfg_base agent_cfg);
    if(user_agent_ctrl == null) begin
        return;
    end
    user_agent_ctrl.apply_to(agent_cfg);
endfunction:apply_user_agent_ctrl

function void memblock_env_cfg::apply_user_cfg();
    if(this.u_user_cfg == null) begin
        return;
    end

    this.u_user_cfg.load_local();
    if(!this.u_user_cfg.enable) begin
        return;
    end

    this.apply_user_agent_ctrl(this.u_user_cfg.all_agent_ctrl, this.u_backendToTopBypass_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.all_agent_ctrl, this.u_fence_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.all_agent_ctrl, this.u_csr_ctrl_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.all_agent_ctrl, this.u_lsqcommit_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.all_agent_ctrl, this.u_lsqenq_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.all_agent_ctrl, this.u_lintsissue_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.all_agent_ctrl, this.u_vecissue_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.all_agent_ctrl, this.u_redirect_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.all_agent_ctrl, this.u_sbuffer_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.all_agent_ctrl, this.u_dcache_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.all_agent_ctrl, this.u_int_sink_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.all_agent_ctrl, this.u_L2tlb_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.all_agent_ctrl, this.u_itlb_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.all_agent_ctrl, this.u_prefetch_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.all_agent_ctrl, this.u_io_mem_to_ooo_ctrl_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.all_agent_ctrl, this.u_io_mem_to_ooo_int_wb_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.all_agent_ctrl, this.u_io_mem_to_ooo_vec_wb_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.all_agent_ctrl, this.u_io_mem_to_ooo_wakeup_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.all_agent_ctrl, this.u_io_mem_to_ooo_iq_feedback_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.all_agent_ctrl, this.u_other_ctrl_agent_agent_cfg);

    this.apply_user_agent_ctrl(this.u_user_cfg.u_backendToTopBypass_agent_agent_ctrl, this.u_backendToTopBypass_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.u_fence_agent_agent_ctrl, this.u_fence_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.u_csr_ctrl_agent_agent_ctrl, this.u_csr_ctrl_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.u_lsqcommit_agent_agent_ctrl, this.u_lsqcommit_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.u_lsqenq_agent_agent_ctrl, this.u_lsqenq_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.u_lintsissue_agent_agent_ctrl, this.u_lintsissue_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.u_vecissue_agent_agent_ctrl, this.u_vecissue_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.u_redirect_agent_agent_ctrl, this.u_redirect_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.u_sbuffer_agent_agent_ctrl, this.u_sbuffer_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.u_dcache_agent_agent_ctrl, this.u_dcache_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.u_int_sink_agent_agent_ctrl, this.u_int_sink_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.u_L2tlb_agent_agent_ctrl, this.u_L2tlb_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.u_itlb_agent_agent_ctrl, this.u_itlb_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.u_prefetch_agent_agent_ctrl, this.u_prefetch_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.u_io_mem_to_ooo_ctrl_agent_agent_ctrl, this.u_io_mem_to_ooo_ctrl_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.u_io_mem_to_ooo_int_wb_agent_agent_ctrl, this.u_io_mem_to_ooo_int_wb_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.u_io_mem_to_ooo_vec_wb_agent_agent_ctrl, this.u_io_mem_to_ooo_vec_wb_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.u_io_mem_to_ooo_wakeup_agent_agent_ctrl, this.u_io_mem_to_ooo_wakeup_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.u_io_mem_to_ooo_iq_feedback_agent_agent_ctrl, this.u_io_mem_to_ooo_iq_feedback_agent_agent_cfg);
    this.apply_user_agent_ctrl(this.u_user_cfg.u_other_ctrl_agent_agent_ctrl, this.u_other_ctrl_agent_agent_cfg);
endfunction:apply_user_cfg

`endif
