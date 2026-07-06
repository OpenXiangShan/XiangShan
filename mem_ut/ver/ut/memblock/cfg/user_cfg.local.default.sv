// Default personal user_ctrl overrides.
// This file is copied to user_cfg.local.sv when the local file is missing.
// All per-field valid bits are 0 by default, so the template does not change
// env/testcase configuration until a user explicitly enables selected fields.

// all_agent_ctrl
this.all_agent_ctrl.sqr_sw_valid = 1'b0;
this.all_agent_ctrl.sqr_sw = tcnt_dec_base::ON;
this.all_agent_ctrl.drv_sw_valid = 1'b0;
this.all_agent_ctrl.drv_sw = tcnt_dec_base::ON;
this.all_agent_ctrl.mon_sw_valid = 1'b0;
this.all_agent_ctrl.mon_sw = tcnt_dec_base::ON;
this.all_agent_ctrl.xz_sw_valid = 1'b0;
this.all_agent_ctrl.xz_sw = tcnt_dec_base::ON;
this.all_agent_ctrl.drv_mode_valid = 1'b0;
this.all_agent_ctrl.drv_mode = tcnt_dec_base::DRV_0;
this.all_agent_ctrl.channel_id_valid = 1'b0;
this.all_agent_ctrl.channel_id = 0;

// u_backendToTopBypass_agent_agent_ctrl
this.u_backendToTopBypass_agent_agent_ctrl.sqr_sw_valid = 1'b0;
this.u_backendToTopBypass_agent_agent_ctrl.sqr_sw = tcnt_dec_base::ON;
this.u_backendToTopBypass_agent_agent_ctrl.drv_sw_valid = 1'b0;
this.u_backendToTopBypass_agent_agent_ctrl.drv_sw = tcnt_dec_base::ON;
this.u_backendToTopBypass_agent_agent_ctrl.mon_sw_valid = 1'b0;
this.u_backendToTopBypass_agent_agent_ctrl.mon_sw = tcnt_dec_base::ON;
this.u_backendToTopBypass_agent_agent_ctrl.xz_sw_valid = 1'b0;
this.u_backendToTopBypass_agent_agent_ctrl.xz_sw = tcnt_dec_base::ON;
this.u_backendToTopBypass_agent_agent_ctrl.drv_mode_valid = 1'b0;
this.u_backendToTopBypass_agent_agent_ctrl.drv_mode = tcnt_dec_base::DRV_0;
this.u_backendToTopBypass_agent_agent_ctrl.channel_id_valid = 1'b0;
this.u_backendToTopBypass_agent_agent_ctrl.channel_id = 0;

// u_fence_agent_agent_ctrl
this.u_fence_agent_agent_ctrl.sqr_sw_valid = 1'b0;
this.u_fence_agent_agent_ctrl.sqr_sw = tcnt_dec_base::ON;
this.u_fence_agent_agent_ctrl.drv_sw_valid = 1'b0;
this.u_fence_agent_agent_ctrl.drv_sw = tcnt_dec_base::ON;
this.u_fence_agent_agent_ctrl.mon_sw_valid = 1'b0;
this.u_fence_agent_agent_ctrl.mon_sw = tcnt_dec_base::ON;
this.u_fence_agent_agent_ctrl.xz_sw_valid = 1'b0;
this.u_fence_agent_agent_ctrl.xz_sw = tcnt_dec_base::ON;
this.u_fence_agent_agent_ctrl.drv_mode_valid = 1'b0;
this.u_fence_agent_agent_ctrl.drv_mode = tcnt_dec_base::DRV_0;
this.u_fence_agent_agent_ctrl.channel_id_valid = 1'b0;
this.u_fence_agent_agent_ctrl.channel_id = 1;

// u_csr_ctrl_agent_agent_ctrl
this.u_csr_ctrl_agent_agent_ctrl.sqr_sw_valid = 1'b0;
this.u_csr_ctrl_agent_agent_ctrl.sqr_sw = tcnt_dec_base::ON;
this.u_csr_ctrl_agent_agent_ctrl.drv_sw_valid = 1'b0;
this.u_csr_ctrl_agent_agent_ctrl.drv_sw = tcnt_dec_base::ON;
this.u_csr_ctrl_agent_agent_ctrl.mon_sw_valid = 1'b0;
this.u_csr_ctrl_agent_agent_ctrl.mon_sw = tcnt_dec_base::ON;
this.u_csr_ctrl_agent_agent_ctrl.xz_sw_valid = 1'b0;
this.u_csr_ctrl_agent_agent_ctrl.xz_sw = tcnt_dec_base::ON;
this.u_csr_ctrl_agent_agent_ctrl.drv_mode_valid = 1'b0;
this.u_csr_ctrl_agent_agent_ctrl.drv_mode = tcnt_dec_base::DRV_0;
this.u_csr_ctrl_agent_agent_ctrl.channel_id_valid = 1'b0;
this.u_csr_ctrl_agent_agent_ctrl.channel_id = 2;

// u_lsqcommit_agent_agent_ctrl
this.u_lsqcommit_agent_agent_ctrl.sqr_sw_valid = 1'b0;
this.u_lsqcommit_agent_agent_ctrl.sqr_sw = tcnt_dec_base::ON;
this.u_lsqcommit_agent_agent_ctrl.drv_sw_valid = 1'b0;
this.u_lsqcommit_agent_agent_ctrl.drv_sw = tcnt_dec_base::ON;
this.u_lsqcommit_agent_agent_ctrl.mon_sw_valid = 1'b0;
this.u_lsqcommit_agent_agent_ctrl.mon_sw = tcnt_dec_base::ON;
this.u_lsqcommit_agent_agent_ctrl.xz_sw_valid = 1'b0;
this.u_lsqcommit_agent_agent_ctrl.xz_sw = tcnt_dec_base::ON;
this.u_lsqcommit_agent_agent_ctrl.drv_mode_valid = 1'b0;
this.u_lsqcommit_agent_agent_ctrl.drv_mode = tcnt_dec_base::DRV_0;
this.u_lsqcommit_agent_agent_ctrl.channel_id_valid = 1'b0;
this.u_lsqcommit_agent_agent_ctrl.channel_id = 3;

// u_lsqenq_agent_agent_ctrl
this.u_lsqenq_agent_agent_ctrl.sqr_sw_valid = 1'b0;
this.u_lsqenq_agent_agent_ctrl.sqr_sw = tcnt_dec_base::ON;
this.u_lsqenq_agent_agent_ctrl.drv_sw_valid = 1'b0;
this.u_lsqenq_agent_agent_ctrl.drv_sw = tcnt_dec_base::ON;
this.u_lsqenq_agent_agent_ctrl.mon_sw_valid = 1'b0;
this.u_lsqenq_agent_agent_ctrl.mon_sw = tcnt_dec_base::ON;
this.u_lsqenq_agent_agent_ctrl.xz_sw_valid = 1'b0;
this.u_lsqenq_agent_agent_ctrl.xz_sw = tcnt_dec_base::ON;
this.u_lsqenq_agent_agent_ctrl.drv_mode_valid = 1'b0;
this.u_lsqenq_agent_agent_ctrl.drv_mode = tcnt_dec_base::DRV_0;
this.u_lsqenq_agent_agent_ctrl.channel_id_valid = 1'b0;
this.u_lsqenq_agent_agent_ctrl.channel_id = 4;

// u_lintsissue_agent_agent_ctrl
this.u_lintsissue_agent_agent_ctrl.sqr_sw_valid = 1'b0;
this.u_lintsissue_agent_agent_ctrl.sqr_sw = tcnt_dec_base::ON;
this.u_lintsissue_agent_agent_ctrl.drv_sw_valid = 1'b0;
this.u_lintsissue_agent_agent_ctrl.drv_sw = tcnt_dec_base::ON;
this.u_lintsissue_agent_agent_ctrl.mon_sw_valid = 1'b0;
this.u_lintsissue_agent_agent_ctrl.mon_sw = tcnt_dec_base::ON;
this.u_lintsissue_agent_agent_ctrl.xz_sw_valid = 1'b0;
this.u_lintsissue_agent_agent_ctrl.xz_sw = tcnt_dec_base::ON;
this.u_lintsissue_agent_agent_ctrl.drv_mode_valid = 1'b0;
this.u_lintsissue_agent_agent_ctrl.drv_mode = tcnt_dec_base::DRV_0;
this.u_lintsissue_agent_agent_ctrl.channel_id_valid = 1'b0;
this.u_lintsissue_agent_agent_ctrl.channel_id = 5;

// u_vecissue_agent_agent_ctrl
this.u_vecissue_agent_agent_ctrl.sqr_sw_valid = 1'b0;
this.u_vecissue_agent_agent_ctrl.sqr_sw = tcnt_dec_base::ON;
this.u_vecissue_agent_agent_ctrl.drv_sw_valid = 1'b0;
this.u_vecissue_agent_agent_ctrl.drv_sw = tcnt_dec_base::ON;
this.u_vecissue_agent_agent_ctrl.mon_sw_valid = 1'b0;
this.u_vecissue_agent_agent_ctrl.mon_sw = tcnt_dec_base::ON;
this.u_vecissue_agent_agent_ctrl.xz_sw_valid = 1'b0;
this.u_vecissue_agent_agent_ctrl.xz_sw = tcnt_dec_base::ON;
this.u_vecissue_agent_agent_ctrl.drv_mode_valid = 1'b0;
this.u_vecissue_agent_agent_ctrl.drv_mode = tcnt_dec_base::DRV_0;
this.u_vecissue_agent_agent_ctrl.channel_id_valid = 1'b0;
this.u_vecissue_agent_agent_ctrl.channel_id = 6;

// u_redirect_agent_agent_ctrl
this.u_redirect_agent_agent_ctrl.sqr_sw_valid = 1'b0;
this.u_redirect_agent_agent_ctrl.sqr_sw = tcnt_dec_base::ON;
this.u_redirect_agent_agent_ctrl.drv_sw_valid = 1'b0;
this.u_redirect_agent_agent_ctrl.drv_sw = tcnt_dec_base::ON;
this.u_redirect_agent_agent_ctrl.mon_sw_valid = 1'b0;
this.u_redirect_agent_agent_ctrl.mon_sw = tcnt_dec_base::ON;
this.u_redirect_agent_agent_ctrl.xz_sw_valid = 1'b0;
this.u_redirect_agent_agent_ctrl.xz_sw = tcnt_dec_base::ON;
this.u_redirect_agent_agent_ctrl.drv_mode_valid = 1'b0;
this.u_redirect_agent_agent_ctrl.drv_mode = tcnt_dec_base::DRV_0;
this.u_redirect_agent_agent_ctrl.channel_id_valid = 1'b0;
this.u_redirect_agent_agent_ctrl.channel_id = 7;

// u_sbuffer_agent_agent_ctrl
this.u_sbuffer_agent_agent_ctrl.sqr_sw_valid = 1'b0;
this.u_sbuffer_agent_agent_ctrl.sqr_sw = tcnt_dec_base::ON;
this.u_sbuffer_agent_agent_ctrl.drv_sw_valid = 1'b0;
this.u_sbuffer_agent_agent_ctrl.drv_sw = tcnt_dec_base::ON;
this.u_sbuffer_agent_agent_ctrl.mon_sw_valid = 1'b0;
this.u_sbuffer_agent_agent_ctrl.mon_sw = tcnt_dec_base::ON;
this.u_sbuffer_agent_agent_ctrl.xz_sw_valid = 1'b0;
this.u_sbuffer_agent_agent_ctrl.xz_sw = tcnt_dec_base::ON;
this.u_sbuffer_agent_agent_ctrl.drv_mode_valid = 1'b0;
this.u_sbuffer_agent_agent_ctrl.drv_mode = tcnt_dec_base::DRV_0;
this.u_sbuffer_agent_agent_ctrl.channel_id_valid = 1'b0;
this.u_sbuffer_agent_agent_ctrl.channel_id = 8;

// u_dcache_agent_agent_ctrl
this.u_dcache_agent_agent_ctrl.sqr_sw_valid = 1'b0;
this.u_dcache_agent_agent_ctrl.sqr_sw = tcnt_dec_base::ON;
this.u_dcache_agent_agent_ctrl.drv_sw_valid = 1'b0;
this.u_dcache_agent_agent_ctrl.drv_sw = tcnt_dec_base::ON;
this.u_dcache_agent_agent_ctrl.mon_sw_valid = 1'b0;
this.u_dcache_agent_agent_ctrl.mon_sw = tcnt_dec_base::ON;
this.u_dcache_agent_agent_ctrl.xz_sw_valid = 1'b0;
this.u_dcache_agent_agent_ctrl.xz_sw = tcnt_dec_base::ON;
this.u_dcache_agent_agent_ctrl.drv_mode_valid = 1'b0;
this.u_dcache_agent_agent_ctrl.drv_mode = tcnt_dec_base::DRV_0;
this.u_dcache_agent_agent_ctrl.channel_id_valid = 1'b0;
this.u_dcache_agent_agent_ctrl.channel_id = 9;

// u_int_sink_agent_agent_ctrl
this.u_int_sink_agent_agent_ctrl.sqr_sw_valid = 1'b0;
this.u_int_sink_agent_agent_ctrl.sqr_sw = tcnt_dec_base::ON;
this.u_int_sink_agent_agent_ctrl.drv_sw_valid = 1'b0;
this.u_int_sink_agent_agent_ctrl.drv_sw = tcnt_dec_base::ON;
this.u_int_sink_agent_agent_ctrl.mon_sw_valid = 1'b0;
this.u_int_sink_agent_agent_ctrl.mon_sw = tcnt_dec_base::ON;
this.u_int_sink_agent_agent_ctrl.xz_sw_valid = 1'b0;
this.u_int_sink_agent_agent_ctrl.xz_sw = tcnt_dec_base::ON;
this.u_int_sink_agent_agent_ctrl.drv_mode_valid = 1'b0;
this.u_int_sink_agent_agent_ctrl.drv_mode = tcnt_dec_base::DRV_0;
this.u_int_sink_agent_agent_ctrl.channel_id_valid = 1'b0;
this.u_int_sink_agent_agent_ctrl.channel_id = 10;

// u_L2tlb_agent_agent_ctrl
this.u_L2tlb_agent_agent_ctrl.sqr_sw_valid = 1'b0;
this.u_L2tlb_agent_agent_ctrl.sqr_sw = tcnt_dec_base::ON;
this.u_L2tlb_agent_agent_ctrl.drv_sw_valid = 1'b0;
this.u_L2tlb_agent_agent_ctrl.drv_sw = tcnt_dec_base::ON;
this.u_L2tlb_agent_agent_ctrl.mon_sw_valid = 1'b0;
this.u_L2tlb_agent_agent_ctrl.mon_sw = tcnt_dec_base::ON;
this.u_L2tlb_agent_agent_ctrl.xz_sw_valid = 1'b0;
this.u_L2tlb_agent_agent_ctrl.xz_sw = tcnt_dec_base::OFF;
this.u_L2tlb_agent_agent_ctrl.drv_mode_valid = 1'b0;
this.u_L2tlb_agent_agent_ctrl.drv_mode = tcnt_dec_base::DRV_0;
this.u_L2tlb_agent_agent_ctrl.channel_id_valid = 1'b0;
this.u_L2tlb_agent_agent_ctrl.channel_id = 19;

// u_itlb_agent_agent_ctrl
this.u_itlb_agent_agent_ctrl.sqr_sw_valid = 1'b0;
this.u_itlb_agent_agent_ctrl.sqr_sw = tcnt_dec_base::ON;
this.u_itlb_agent_agent_ctrl.drv_sw_valid = 1'b0;
this.u_itlb_agent_agent_ctrl.drv_sw = tcnt_dec_base::ON;
this.u_itlb_agent_agent_ctrl.mon_sw_valid = 1'b0;
this.u_itlb_agent_agent_ctrl.mon_sw = tcnt_dec_base::ON;
this.u_itlb_agent_agent_ctrl.xz_sw_valid = 1'b0;
this.u_itlb_agent_agent_ctrl.xz_sw = tcnt_dec_base::ON;
this.u_itlb_agent_agent_ctrl.drv_mode_valid = 1'b0;
this.u_itlb_agent_agent_ctrl.drv_mode = tcnt_dec_base::DRV_0;
this.u_itlb_agent_agent_ctrl.channel_id_valid = 1'b0;
this.u_itlb_agent_agent_ctrl.channel_id = 11;

// u_prefetch_agent_agent_ctrl
this.u_prefetch_agent_agent_ctrl.sqr_sw_valid = 1'b0;
this.u_prefetch_agent_agent_ctrl.sqr_sw = tcnt_dec_base::OFF;
this.u_prefetch_agent_agent_ctrl.drv_sw_valid = 1'b0;
this.u_prefetch_agent_agent_ctrl.drv_sw = tcnt_dec_base::OFF;
this.u_prefetch_agent_agent_ctrl.mon_sw_valid = 1'b0;
this.u_prefetch_agent_agent_ctrl.mon_sw = tcnt_dec_base::ON;
this.u_prefetch_agent_agent_ctrl.xz_sw_valid = 1'b0;
this.u_prefetch_agent_agent_ctrl.xz_sw = tcnt_dec_base::ON;
this.u_prefetch_agent_agent_ctrl.drv_mode_valid = 1'b0;
this.u_prefetch_agent_agent_ctrl.drv_mode = tcnt_dec_base::DRV_0;
this.u_prefetch_agent_agent_ctrl.channel_id_valid = 1'b0;
this.u_prefetch_agent_agent_ctrl.channel_id = 12;

// u_io_mem_to_ooo_ctrl_agent_agent_ctrl
this.u_io_mem_to_ooo_ctrl_agent_agent_ctrl.sqr_sw_valid = 1'b0;
this.u_io_mem_to_ooo_ctrl_agent_agent_ctrl.sqr_sw = tcnt_dec_base::OFF;
this.u_io_mem_to_ooo_ctrl_agent_agent_ctrl.drv_sw_valid = 1'b0;
this.u_io_mem_to_ooo_ctrl_agent_agent_ctrl.drv_sw = tcnt_dec_base::OFF;
this.u_io_mem_to_ooo_ctrl_agent_agent_ctrl.mon_sw_valid = 1'b0;
this.u_io_mem_to_ooo_ctrl_agent_agent_ctrl.mon_sw = tcnt_dec_base::ON;
this.u_io_mem_to_ooo_ctrl_agent_agent_ctrl.xz_sw_valid = 1'b0;
this.u_io_mem_to_ooo_ctrl_agent_agent_ctrl.xz_sw = tcnt_dec_base::ON;
this.u_io_mem_to_ooo_ctrl_agent_agent_ctrl.drv_mode_valid = 1'b0;
this.u_io_mem_to_ooo_ctrl_agent_agent_ctrl.drv_mode = tcnt_dec_base::DRV_0;
this.u_io_mem_to_ooo_ctrl_agent_agent_ctrl.channel_id_valid = 1'b0;
this.u_io_mem_to_ooo_ctrl_agent_agent_ctrl.channel_id = 13;

// u_io_mem_to_ooo_int_wb_agent_agent_ctrl
this.u_io_mem_to_ooo_int_wb_agent_agent_ctrl.sqr_sw_valid = 1'b0;
this.u_io_mem_to_ooo_int_wb_agent_agent_ctrl.sqr_sw = tcnt_dec_base::OFF;
this.u_io_mem_to_ooo_int_wb_agent_agent_ctrl.drv_sw_valid = 1'b0;
this.u_io_mem_to_ooo_int_wb_agent_agent_ctrl.drv_sw = tcnt_dec_base::OFF;
this.u_io_mem_to_ooo_int_wb_agent_agent_ctrl.mon_sw_valid = 1'b0;
this.u_io_mem_to_ooo_int_wb_agent_agent_ctrl.mon_sw = tcnt_dec_base::ON;
this.u_io_mem_to_ooo_int_wb_agent_agent_ctrl.xz_sw_valid = 1'b0;
this.u_io_mem_to_ooo_int_wb_agent_agent_ctrl.xz_sw = tcnt_dec_base::ON;
this.u_io_mem_to_ooo_int_wb_agent_agent_ctrl.drv_mode_valid = 1'b0;
this.u_io_mem_to_ooo_int_wb_agent_agent_ctrl.drv_mode = tcnt_dec_base::DRV_0;
this.u_io_mem_to_ooo_int_wb_agent_agent_ctrl.channel_id_valid = 1'b0;
this.u_io_mem_to_ooo_int_wb_agent_agent_ctrl.channel_id = 14;

// u_io_mem_to_ooo_vec_wb_agent_agent_ctrl
this.u_io_mem_to_ooo_vec_wb_agent_agent_ctrl.sqr_sw_valid = 1'b0;
this.u_io_mem_to_ooo_vec_wb_agent_agent_ctrl.sqr_sw = tcnt_dec_base::OFF;
this.u_io_mem_to_ooo_vec_wb_agent_agent_ctrl.drv_sw_valid = 1'b0;
this.u_io_mem_to_ooo_vec_wb_agent_agent_ctrl.drv_sw = tcnt_dec_base::OFF;
this.u_io_mem_to_ooo_vec_wb_agent_agent_ctrl.mon_sw_valid = 1'b0;
this.u_io_mem_to_ooo_vec_wb_agent_agent_ctrl.mon_sw = tcnt_dec_base::ON;
this.u_io_mem_to_ooo_vec_wb_agent_agent_ctrl.xz_sw_valid = 1'b0;
this.u_io_mem_to_ooo_vec_wb_agent_agent_ctrl.xz_sw = tcnt_dec_base::ON;
this.u_io_mem_to_ooo_vec_wb_agent_agent_ctrl.drv_mode_valid = 1'b0;
this.u_io_mem_to_ooo_vec_wb_agent_agent_ctrl.drv_mode = tcnt_dec_base::DRV_0;
this.u_io_mem_to_ooo_vec_wb_agent_agent_ctrl.channel_id_valid = 1'b0;
this.u_io_mem_to_ooo_vec_wb_agent_agent_ctrl.channel_id = 15;

// u_io_mem_to_ooo_wakeup_agent_agent_ctrl
this.u_io_mem_to_ooo_wakeup_agent_agent_ctrl.sqr_sw_valid = 1'b0;
this.u_io_mem_to_ooo_wakeup_agent_agent_ctrl.sqr_sw = tcnt_dec_base::OFF;
this.u_io_mem_to_ooo_wakeup_agent_agent_ctrl.drv_sw_valid = 1'b0;
this.u_io_mem_to_ooo_wakeup_agent_agent_ctrl.drv_sw = tcnt_dec_base::OFF;
this.u_io_mem_to_ooo_wakeup_agent_agent_ctrl.mon_sw_valid = 1'b0;
this.u_io_mem_to_ooo_wakeup_agent_agent_ctrl.mon_sw = tcnt_dec_base::ON;
this.u_io_mem_to_ooo_wakeup_agent_agent_ctrl.xz_sw_valid = 1'b0;
this.u_io_mem_to_ooo_wakeup_agent_agent_ctrl.xz_sw = tcnt_dec_base::ON;
this.u_io_mem_to_ooo_wakeup_agent_agent_ctrl.drv_mode_valid = 1'b0;
this.u_io_mem_to_ooo_wakeup_agent_agent_ctrl.drv_mode = tcnt_dec_base::DRV_0;
this.u_io_mem_to_ooo_wakeup_agent_agent_ctrl.channel_id_valid = 1'b0;
this.u_io_mem_to_ooo_wakeup_agent_agent_ctrl.channel_id = 16;

// u_io_mem_to_ooo_iq_feedback_agent_agent_ctrl
this.u_io_mem_to_ooo_iq_feedback_agent_agent_ctrl.sqr_sw_valid = 1'b0;
this.u_io_mem_to_ooo_iq_feedback_agent_agent_ctrl.sqr_sw = tcnt_dec_base::OFF;
this.u_io_mem_to_ooo_iq_feedback_agent_agent_ctrl.drv_sw_valid = 1'b0;
this.u_io_mem_to_ooo_iq_feedback_agent_agent_ctrl.drv_sw = tcnt_dec_base::OFF;
this.u_io_mem_to_ooo_iq_feedback_agent_agent_ctrl.mon_sw_valid = 1'b0;
this.u_io_mem_to_ooo_iq_feedback_agent_agent_ctrl.mon_sw = tcnt_dec_base::ON;
this.u_io_mem_to_ooo_iq_feedback_agent_agent_ctrl.xz_sw_valid = 1'b0;
this.u_io_mem_to_ooo_iq_feedback_agent_agent_ctrl.xz_sw = tcnt_dec_base::ON;
this.u_io_mem_to_ooo_iq_feedback_agent_agent_ctrl.drv_mode_valid = 1'b0;
this.u_io_mem_to_ooo_iq_feedback_agent_agent_ctrl.drv_mode = tcnt_dec_base::DRV_0;
this.u_io_mem_to_ooo_iq_feedback_agent_agent_ctrl.channel_id_valid = 1'b0;
this.u_io_mem_to_ooo_iq_feedback_agent_agent_ctrl.channel_id = 17;

// u_other_ctrl_agent_agent_ctrl
this.u_other_ctrl_agent_agent_ctrl.sqr_sw_valid = 1'b0;
this.u_other_ctrl_agent_agent_ctrl.sqr_sw = tcnt_dec_base::ON;
this.u_other_ctrl_agent_agent_ctrl.drv_sw_valid = 1'b0;
this.u_other_ctrl_agent_agent_ctrl.drv_sw = tcnt_dec_base::ON;
this.u_other_ctrl_agent_agent_ctrl.mon_sw_valid = 1'b0;
this.u_other_ctrl_agent_agent_ctrl.mon_sw = tcnt_dec_base::ON;
this.u_other_ctrl_agent_agent_ctrl.xz_sw_valid = 1'b0;
this.u_other_ctrl_agent_agent_ctrl.xz_sw = tcnt_dec_base::ON;
this.u_other_ctrl_agent_agent_ctrl.drv_mode_valid = 1'b0;
this.u_other_ctrl_agent_agent_ctrl.drv_mode = tcnt_dec_base::DRV_0;
this.u_other_ctrl_agent_agent_ctrl.channel_id_valid = 1'b0;
this.u_other_ctrl_agent_agent_ctrl.channel_id = 18;
