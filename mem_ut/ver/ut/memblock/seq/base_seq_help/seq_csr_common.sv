//=========================================================
//File name    : seq_csr_common.sv
//Author       : OpenAI_Codex
//Module name  : seq_csr_common
//Discribution : dispatch sequence common parameter snapshot
//Date         : 2026-05-18
//=========================================================
`ifndef SEQ_CSR_COMMON__SV
`define SEQ_CSR_COMMON__SV

class seq_csr_common;

    static bit initialized = 1'b0;
    static semaphore init_sem = new(1);

    static int unsigned main_trans_num = 1;
    static bit          use_manual_main_table = 1'b0;
    static int unsigned enq_per_cycle = 4;
    // 控制 get_enq_per_cycle() 是否每次在 [1:real_enq_width] 内均匀随机。
    static bit          enq_per_cycle_rand_en = 1'b0;
    // 当前DUT每拍LSQ enqueue总slot上限；route阶段每拍从连续success前缀后最多扫描该数量的uid，默认按8-wide配置设置。
    static int unsigned real_lsq_enq_max = 8;
    // LOAD/STA/STD issue pipe配置上限；由plus加载并被真实pipe数clamp。
    // 调度路径不直接把它当本拍发射数，必须通过sample_*_pip_num()按随机开关采样。
    static int unsigned load_pip_num_limit = 3;
    static int unsigned sta_pip_num_limit = 2;
    static int unsigned std_pip_num_limit = 2;
    // pipe随机开关：0表示每拍固定返回*_pip_num_limit；1表示每拍在[1:limit]内随机。
    static bit          load_pip_num_random_en = 1'b0;
    static bit          sta_pip_num_random_en = 1'b0;
    static bit          std_pip_num_random_en = 1'b0;

    // LSQ enqueue真实slot宽度兼容字段，必须与real_lsq_enq_max保持一致。
    static int unsigned real_enq_width = 8;
    static int unsigned real_load_pipe_num = 3;
    static int unsigned real_sta_pipe_num = 2;
    static int unsigned real_std_pipe_num = 2;

    static int unsigned op_class_int_load_wt = 8;
    static int unsigned op_class_fp_load_wt = 1;
    static int unsigned op_class_store_wt = 6;
    static int unsigned op_class_prefetch_wt = 1;
    static int unsigned op_class_amo_wt = 1;
    static int unsigned op_class_cbo_wt = 0;
    static int unsigned load_fuop_lb_wt = 1;
    static int unsigned load_fuop_lh_wt = 1;
    static int unsigned load_fuop_lw_wt = 1;
    static int unsigned load_fuop_ld_wt = 1;
    static int unsigned load_fuop_lbu_wt = 1;
    static int unsigned load_fuop_lhu_wt = 1;
    static int unsigned load_fuop_lwu_wt = 1;
    static int unsigned store_fuop_sb_wt = 1;
    static int unsigned store_fuop_sh_wt = 1;
    static int unsigned store_fuop_sw_wt = 1;
    static int unsigned store_fuop_sd_wt = 1;
    static int unsigned prefetch_fuop_i_wt = 1;
    static int unsigned prefetch_fuop_r_wt = 1;
    static int unsigned prefetch_fuop_w_wt = 1;
    static int unsigned cbo_fuop_zero_wt = 1;
    static int unsigned cbo_fuop_clean_wt = 1;
    static int unsigned cbo_fuop_flush_wt = 1;
    static int unsigned cbo_fuop_inval_wt = 1;
    static int unsigned amo_fuop_lr_w_wt = 1;
    static int unsigned amo_fuop_sc_w_wt = 1;
    static int unsigned amo_fuop_amoswap_w_wt = 1;
    static int unsigned amo_fuop_amoadd_w_wt = 1;
    static int unsigned amo_fuop_amoxor_w_wt = 1;
    static int unsigned amo_fuop_amoand_w_wt = 1;
    static int unsigned amo_fuop_amoor_w_wt = 1;
    static int unsigned amo_fuop_amomin_w_wt = 1;
    static int unsigned amo_fuop_amomax_w_wt = 1;
    static int unsigned amo_fuop_amominu_w_wt = 1;
    static int unsigned amo_fuop_amomaxu_w_wt = 1;
    static int unsigned amo_fuop_lr_d_wt = 1;
    static int unsigned amo_fuop_sc_d_wt = 1;
    static int unsigned amo_fuop_amoswap_d_wt = 1;
    static int unsigned amo_fuop_amoadd_d_wt = 1;
    static int unsigned amo_fuop_amoxor_d_wt = 1;
    static int unsigned amo_fuop_amoand_d_wt = 1;
    static int unsigned amo_fuop_amoor_d_wt = 1;
    static int unsigned amo_fuop_amomin_d_wt = 1;
    static int unsigned amo_fuop_amomax_d_wt = 1;
    static int unsigned amo_fuop_amominu_d_wt = 1;
    static int unsigned amo_fuop_amomaxu_d_wt = 1;
    // boundary_profile生成默认关闭；关闭时主表继续使用原地址模板。
    static bit          boundary_profile_gen_en = 1'b0;
    static int unsigned boundary_aligned_wt = 1;
    static int unsigned boundary_misalign_within_8b_wt = 0;
    static int unsigned boundary_cross_8b_within_16b_wt = 0;
    static int unsigned boundary_cross_16b_same_line_wt = 0;
    static int unsigned boundary_cross_cacheline_same_4k_wt = 0;
    static int unsigned boundary_cross_4k_wt = 0;
    static bit          store_cross_8b_within_16b_en = 1'b0;
    static bit          lsq_resync_on_mismatch = 1'b0;

    // send_pri仲裁总开关：0时主表使用default priority，issue只按ROB age；1时主表随机priority且issue比较priority。
    static bit          send_pri_mode_en = 1'b1;
    // send_pri模式下本拍启用global priority filter的随机权重；sample_global_send_pri_en()读取它。
    static int unsigned global_send_pri_en_wt = 0;
    static int unsigned send_pri_default = 50;
    static int unsigned send_pri_low_wt = 1;
    static int unsigned send_pri_mid_wt = 8;
    static int unsigned send_pri_high_wt = 1;
    static int unsigned send_pri_std_default = 50;
    static int unsigned send_pri_std_low_wt = 1;
    static int unsigned send_pri_std_mid_wt = 8;
    static int unsigned send_pri_std_high_wt = 1;

    // 主表生成期地址复用参数：recent queue只保存窗口内uid，避免后处理全表扫描。
    static int unsigned addr_reuse_en_1_wt = 1;
    static int unsigned addr_reuse_en_0_wt = 19;
    static int unsigned addr_reuse_load_after_store_wt = 1;
    static int unsigned addr_reuse_load_after_load_wt = 0;
    static int unsigned addr_reuse_store_after_load_wt = 1;
    static int unsigned addr_reuse_store_after_store_wt = 0;
    static int unsigned addr_reuse_keep_ref_size_en_1_wt = 0;
    static int unsigned addr_reuse_keep_ref_size_en_0_wt = 1;
    static int unsigned addr_ref_window_fixed = 0;
    static int unsigned addr_ref_window_small_weight = 0;
    static int unsigned addr_ref_window_medium_weight = 0;
    static int unsigned addr_ref_window_large_weight = 1;
    // ROB起始只随机value，初始flag固定为0，避免人为制造跨flag初始态。
    static bit          rob_start_fixed_en = 1'b1;
    static int unsigned rob_start_fixed_value = 0;
    static int unsigned rob_start_zero_wt = 1;
    static int unsigned rob_start_mid_wt = 0;
    static int unsigned rob_start_near_wrap_wt = 0;

    static int unsigned delay_0_wt = 10;
    static int unsigned delay_1_20_wt = 5;
    static int unsigned delay_21_50_wt = 1;

    static int unsigned mdp_load_wait_wt = 0;
    static int unsigned mdp_storeset_hit_wt = 0;
    static int unsigned load_wait_strict_wt = 0;
    static int unsigned rvc_wt = 0;
    static bit [63:0]   pc_base = 64'h8000_0000;
    static int unsigned pc_stride = 4;
    static int unsigned ftq_idx_base = 0;
    static int unsigned pdest_base = 1;
    static int unsigned pdest_range = 128;

    static int unsigned tlb_pte_r_1_wt = 8;
    static int unsigned tlb_pte_r_0_wt = 1;
    static int unsigned tlb_pte_w_1_wt = 6;
    static int unsigned tlb_pte_w_0_wt = 1;
    static int unsigned tlb_pte_x_1_wt = 4;
    static int unsigned tlb_pte_x_0_wt = 1;
    static int unsigned tlb_pte_u_1_wt = 1;
    static int unsigned tlb_pte_u_0_wt = 8;
    static int unsigned tlb_pte_g_1_wt = 1;
    static int unsigned tlb_pte_g_0_wt = 8;
    static int unsigned tlb_pte_a_1_wt = 8;
    static int unsigned tlb_pte_a_0_wt = 1;
    static int unsigned tlb_pte_d_1_wt = 8;
    static int unsigned tlb_pte_d_0_wt = 1;
    static int unsigned tlb_pte_n_1_wt = 1;
    static int unsigned tlb_pte_n_0_wt = 8;
    static int unsigned tlb_pte_v_1_wt = 9;
    static int unsigned tlb_pte_v_0_wt = 1;
    static int unsigned tlb_level_mode = 0;
    static int unsigned tlb_level_fixed_value = 0;
    static int unsigned tlb_level_random_low = 0;
    static int unsigned tlb_level_random_high = 2;
    static int unsigned tlb_pte_mode = 0;

    static bit [63:0]   paddr_base = 64'h8000_0000;
    static bit [63:0]   paddr_range = 64'h1000_0000;
    static int unsigned active_seq_no_progress_warn_cycles = 10000;
    static bit          dispatch_issue_seq_en = 1'b0;
    // 中文注释：lintsissue 非阻塞发射模式开关。
    // 为 1 时 driver 只采样一次 ready，未 fire 的 issue item 不出队，后续重新仲裁。
    static bit          dispatch_issue_nonblocking_en = 1'b0;
    static int unsigned dispatch_ready_timeout = 1000;
    static bit          redirect_seq_en = 1'b1;
    static int unsigned redirect_drive_timeout = 1000;
    static int unsigned redirect_freeze_timeout = 1000;
    static bit          sta_real_wb_pass_en = 1'b0;
    static bit          std_real_wb_pass_en = 1'b0;
    static bit          lsqenq_seq_en = 1'b1;
    static int unsigned lsqenq_ready_timeout = 1000;
    static bit          lsqcommit_seq_en = 1'b1;
    static bit          flushsb_seq_en = 1'b0;
    static int unsigned flushsb_request_cycle = 0;
    static int unsigned flushsb_timeout = 1000;
    static bit          replay_wait_ptw_en = 1'b0;
    static int unsigned replay_wait_ptw_timeout = 1000;
    static bit          l2tlb_seq_en = 1'b0;
    static int unsigned l2tlb_min_latency = 1;
    static int unsigned l2tlb_max_latency = 4;
    static int unsigned l2tlb_idle_stop_cycle = 5000;

    static task init();
        init_sem.get();
        if (initialized) begin
            init_sem.put();
            return;
        end

        plus::reload_from_cmdline();
        load_from_plus();
        validate_and_clamp();
        initialized = 1'b1;
        init_sem.put();
    endtask:init

    static function void reload_from_plus();
        plus::reload_from_cmdline();
        load_from_plus();
        validate_and_clamp();
        initialized = 1'b1;
    endfunction:reload_from_plus

    static function bit is_initialized();
        return initialized;
    endfunction:is_initialized

    static function void check_initialized(string caller = "seq_csr_common");
        if (!initialized) begin
            `uvm_fatal("SEQ_CSR_INIT", $sformatf("%s called before seq_csr_common::init()", caller))
        end
    endfunction:check_initialized

    static function void load_from_plus();
        main_trans_num              = get_non_negative_int("MEMBLOCK_MAIN_TRANS_NUM", plus::MEMBLOCK_MAIN_TRANS_NUM);
        use_manual_main_table       = plus::MEMBLOCK_USE_MANUAL_MAIN_TABLE;
        enq_per_cycle               = get_non_negative_int("MEMBLOCK_ENQ_PER_CYCLE", plus::MEMBLOCK_ENQ_PER_CYCLE);
        enq_per_cycle_rand_en       = plus::MEMBLOCK_ENQ_PER_CYCLE_RAND_EN;
        real_lsq_enq_max            = get_non_negative_int("MEMBLOCK_REAL_LSQ_ENQ_MAX", plus::MEMBLOCK_REAL_LSQ_ENQ_MAX);
        load_pip_num_limit          = get_non_negative_int("MEMBLOCK_LOAD_PIP_NUM_LIMIT", plus::MEMBLOCK_LOAD_PIP_NUM_LIMIT);
        sta_pip_num_limit           = get_non_negative_int("MEMBLOCK_STA_PIP_NUM_LIMIT", plus::MEMBLOCK_STA_PIP_NUM_LIMIT);
        std_pip_num_limit           = get_non_negative_int("MEMBLOCK_STD_PIP_NUM_LIMIT", plus::MEMBLOCK_STD_PIP_NUM_LIMIT);
        load_pip_num_random_en      = plus::MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN;
        sta_pip_num_random_en       = plus::MEMBLOCK_STA_PIP_NUM_RANDOM_EN;
        std_pip_num_random_en       = plus::MEMBLOCK_STD_PIP_NUM_RANDOM_EN;

        real_enq_width              = get_non_negative_int("MEMBLOCK_REAL_ENQ_WIDTH", plus::MEMBLOCK_REAL_ENQ_WIDTH);
        real_load_pipe_num          = get_non_negative_int("MEMBLOCK_REAL_LOAD_PIPE_NUM", plus::MEMBLOCK_REAL_LOAD_PIPE_NUM);
        real_sta_pipe_num           = get_non_negative_int("MEMBLOCK_REAL_STA_PIPE_NUM", plus::MEMBLOCK_REAL_STA_PIPE_NUM);
        real_std_pipe_num           = get_non_negative_int("MEMBLOCK_REAL_STD_PIPE_NUM", plus::MEMBLOCK_REAL_STD_PIPE_NUM);

        op_class_int_load_wt        = get_non_negative_int("MEMBLOCK_OP_CLASS_INT_LOAD_WT", plus::MEMBLOCK_OP_CLASS_INT_LOAD_WT);
        op_class_fp_load_wt         = get_non_negative_int("MEMBLOCK_OP_CLASS_FP_LOAD_WT", plus::MEMBLOCK_OP_CLASS_FP_LOAD_WT);
        op_class_store_wt           = get_non_negative_int("MEMBLOCK_OP_CLASS_STORE_WT", plus::MEMBLOCK_OP_CLASS_STORE_WT);
        op_class_prefetch_wt        = get_non_negative_int("MEMBLOCK_OP_CLASS_PREFETCH_WT", plus::MEMBLOCK_OP_CLASS_PREFETCH_WT);
        op_class_amo_wt             = get_non_negative_int("MEMBLOCK_OP_CLASS_AMO_WT", plus::MEMBLOCK_OP_CLASS_AMO_WT);
        op_class_cbo_wt             = get_non_negative_int("MEMBLOCK_OP_CLASS_CBO_WT", plus::MEMBLOCK_OP_CLASS_CBO_WT);
        load_fuop_lb_wt             = get_non_negative_int("MEMBLOCK_LOAD_FUOP_LB_WT", plus::MEMBLOCK_LOAD_FUOP_LB_WT);
        load_fuop_lh_wt             = get_non_negative_int("MEMBLOCK_LOAD_FUOP_LH_WT", plus::MEMBLOCK_LOAD_FUOP_LH_WT);
        load_fuop_lw_wt             = get_non_negative_int("MEMBLOCK_LOAD_FUOP_LW_WT", plus::MEMBLOCK_LOAD_FUOP_LW_WT);
        load_fuop_ld_wt             = get_non_negative_int("MEMBLOCK_LOAD_FUOP_LD_WT", plus::MEMBLOCK_LOAD_FUOP_LD_WT);
        load_fuop_lbu_wt            = get_non_negative_int("MEMBLOCK_LOAD_FUOP_LBU_WT", plus::MEMBLOCK_LOAD_FUOP_LBU_WT);
        load_fuop_lhu_wt            = get_non_negative_int("MEMBLOCK_LOAD_FUOP_LHU_WT", plus::MEMBLOCK_LOAD_FUOP_LHU_WT);
        load_fuop_lwu_wt            = get_non_negative_int("MEMBLOCK_LOAD_FUOP_LWU_WT", plus::MEMBLOCK_LOAD_FUOP_LWU_WT);
        store_fuop_sb_wt            = get_non_negative_int("MEMBLOCK_STORE_FUOP_SB_WT", plus::MEMBLOCK_STORE_FUOP_SB_WT);
        store_fuop_sh_wt            = get_non_negative_int("MEMBLOCK_STORE_FUOP_SH_WT", plus::MEMBLOCK_STORE_FUOP_SH_WT);
        store_fuop_sw_wt            = get_non_negative_int("MEMBLOCK_STORE_FUOP_SW_WT", plus::MEMBLOCK_STORE_FUOP_SW_WT);
        store_fuop_sd_wt            = get_non_negative_int("MEMBLOCK_STORE_FUOP_SD_WT", plus::MEMBLOCK_STORE_FUOP_SD_WT);
        prefetch_fuop_i_wt          = get_non_negative_int("MEMBLOCK_PREFETCH_FUOP_I_WT", plus::MEMBLOCK_PREFETCH_FUOP_I_WT);
        prefetch_fuop_r_wt          = get_non_negative_int("MEMBLOCK_PREFETCH_FUOP_R_WT", plus::MEMBLOCK_PREFETCH_FUOP_R_WT);
        prefetch_fuop_w_wt          = get_non_negative_int("MEMBLOCK_PREFETCH_FUOP_W_WT", plus::MEMBLOCK_PREFETCH_FUOP_W_WT);
        cbo_fuop_zero_wt            = get_non_negative_int("MEMBLOCK_CBO_FUOP_ZERO_WT", plus::MEMBLOCK_CBO_FUOP_ZERO_WT);
        cbo_fuop_clean_wt           = get_non_negative_int("MEMBLOCK_CBO_FUOP_CLEAN_WT", plus::MEMBLOCK_CBO_FUOP_CLEAN_WT);
        cbo_fuop_flush_wt           = get_non_negative_int("MEMBLOCK_CBO_FUOP_FLUSH_WT", plus::MEMBLOCK_CBO_FUOP_FLUSH_WT);
        cbo_fuop_inval_wt           = get_non_negative_int("MEMBLOCK_CBO_FUOP_INVAL_WT", plus::MEMBLOCK_CBO_FUOP_INVAL_WT);
        amo_fuop_lr_w_wt            = get_non_negative_int("MEMBLOCK_AMO_FUOP_LR_W_WT", plus::MEMBLOCK_AMO_FUOP_LR_W_WT);
        amo_fuop_sc_w_wt            = get_non_negative_int("MEMBLOCK_AMO_FUOP_SC_W_WT", plus::MEMBLOCK_AMO_FUOP_SC_W_WT);
        amo_fuop_amoswap_w_wt       = get_non_negative_int("MEMBLOCK_AMO_FUOP_AMOSWAP_W_WT", plus::MEMBLOCK_AMO_FUOP_AMOSWAP_W_WT);
        amo_fuop_amoadd_w_wt        = get_non_negative_int("MEMBLOCK_AMO_FUOP_AMOADD_W_WT", plus::MEMBLOCK_AMO_FUOP_AMOADD_W_WT);
        amo_fuop_amoxor_w_wt        = get_non_negative_int("MEMBLOCK_AMO_FUOP_AMOXOR_W_WT", plus::MEMBLOCK_AMO_FUOP_AMOXOR_W_WT);
        amo_fuop_amoand_w_wt        = get_non_negative_int("MEMBLOCK_AMO_FUOP_AMOAND_W_WT", plus::MEMBLOCK_AMO_FUOP_AMOAND_W_WT);
        amo_fuop_amoor_w_wt         = get_non_negative_int("MEMBLOCK_AMO_FUOP_AMOOR_W_WT", plus::MEMBLOCK_AMO_FUOP_AMOOR_W_WT);
        amo_fuop_amomin_w_wt        = get_non_negative_int("MEMBLOCK_AMO_FUOP_AMOMIN_W_WT", plus::MEMBLOCK_AMO_FUOP_AMOMIN_W_WT);
        amo_fuop_amomax_w_wt        = get_non_negative_int("MEMBLOCK_AMO_FUOP_AMOMAX_W_WT", plus::MEMBLOCK_AMO_FUOP_AMOMAX_W_WT);
        amo_fuop_amominu_w_wt       = get_non_negative_int("MEMBLOCK_AMO_FUOP_AMOMINU_W_WT", plus::MEMBLOCK_AMO_FUOP_AMOMINU_W_WT);
        amo_fuop_amomaxu_w_wt       = get_non_negative_int("MEMBLOCK_AMO_FUOP_AMOMAXU_W_WT", plus::MEMBLOCK_AMO_FUOP_AMOMAXU_W_WT);
        amo_fuop_lr_d_wt            = get_non_negative_int("MEMBLOCK_AMO_FUOP_LR_D_WT", plus::MEMBLOCK_AMO_FUOP_LR_D_WT);
        amo_fuop_sc_d_wt            = get_non_negative_int("MEMBLOCK_AMO_FUOP_SC_D_WT", plus::MEMBLOCK_AMO_FUOP_SC_D_WT);
        amo_fuop_amoswap_d_wt       = get_non_negative_int("MEMBLOCK_AMO_FUOP_AMOSWAP_D_WT", plus::MEMBLOCK_AMO_FUOP_AMOSWAP_D_WT);
        amo_fuop_amoadd_d_wt        = get_non_negative_int("MEMBLOCK_AMO_FUOP_AMOADD_D_WT", plus::MEMBLOCK_AMO_FUOP_AMOADD_D_WT);
        amo_fuop_amoxor_d_wt        = get_non_negative_int("MEMBLOCK_AMO_FUOP_AMOXOR_D_WT", plus::MEMBLOCK_AMO_FUOP_AMOXOR_D_WT);
        amo_fuop_amoand_d_wt        = get_non_negative_int("MEMBLOCK_AMO_FUOP_AMOAND_D_WT", plus::MEMBLOCK_AMO_FUOP_AMOAND_D_WT);
        amo_fuop_amoor_d_wt         = get_non_negative_int("MEMBLOCK_AMO_FUOP_AMOOR_D_WT", plus::MEMBLOCK_AMO_FUOP_AMOOR_D_WT);
        amo_fuop_amomin_d_wt        = get_non_negative_int("MEMBLOCK_AMO_FUOP_AMOMIN_D_WT", plus::MEMBLOCK_AMO_FUOP_AMOMIN_D_WT);
        amo_fuop_amomax_d_wt        = get_non_negative_int("MEMBLOCK_AMO_FUOP_AMOMAX_D_WT", plus::MEMBLOCK_AMO_FUOP_AMOMAX_D_WT);
        amo_fuop_amominu_d_wt       = get_non_negative_int("MEMBLOCK_AMO_FUOP_AMOMINU_D_WT", plus::MEMBLOCK_AMO_FUOP_AMOMINU_D_WT);
        amo_fuop_amomaxu_d_wt       = get_non_negative_int("MEMBLOCK_AMO_FUOP_AMOMAXU_D_WT", plus::MEMBLOCK_AMO_FUOP_AMOMAXU_D_WT);
        boundary_profile_gen_en     = plus::MEMBLOCK_BOUNDARY_PROFILE_GEN_EN;
        boundary_aligned_wt         = get_non_negative_int("MEMBLOCK_BOUNDARY_ALIGNED_WT", plus::MEMBLOCK_BOUNDARY_ALIGNED_WT);
        boundary_misalign_within_8b_wt = get_non_negative_int("MEMBLOCK_BOUNDARY_MISALIGN_WITHIN_8B_WT", plus::MEMBLOCK_BOUNDARY_MISALIGN_WITHIN_8B_WT);
        boundary_cross_8b_within_16b_wt = get_non_negative_int("MEMBLOCK_BOUNDARY_CROSS_8B_WITHIN_16B_WT", plus::MEMBLOCK_BOUNDARY_CROSS_8B_WITHIN_16B_WT);
        boundary_cross_16b_same_line_wt = get_non_negative_int("MEMBLOCK_BOUNDARY_CROSS_16B_SAME_LINE_WT", plus::MEMBLOCK_BOUNDARY_CROSS_16B_SAME_LINE_WT);
        boundary_cross_cacheline_same_4k_wt = get_non_negative_int("MEMBLOCK_BOUNDARY_CROSS_CACHELINE_SAME_4K_WT", plus::MEMBLOCK_BOUNDARY_CROSS_CACHELINE_SAME_4K_WT);
        boundary_cross_4k_wt        = get_non_negative_int("MEMBLOCK_BOUNDARY_CROSS_4K_WT", plus::MEMBLOCK_BOUNDARY_CROSS_4K_WT);
        store_cross_8b_within_16b_en = plus::MEMBLOCK_STORE_CROSS_8B_WITHIN_16B_EN;
        lsq_resync_on_mismatch      = plus::MEMBLOCK_LSQ_RESYNC_ON_MISMATCH;

        send_pri_mode_en            = plus::MEMBLOCK_SEND_PRI_MODE_EN;
        global_send_pri_en_wt       = get_non_negative_int("MEMBLOCK_GLOBAL_SEND_PRI_EN_WT", plus::MEMBLOCK_GLOBAL_SEND_PRI_EN_WT);
        send_pri_default            = get_non_negative_int("MEMBLOCK_SEND_PRI_DEFAULT", plus::MEMBLOCK_SEND_PRI_DEFAULT);
        send_pri_low_wt             = get_non_negative_int("MEMBLOCK_SEND_PRI_LOW_WT", plus::MEMBLOCK_SEND_PRI_LOW_WT);
        send_pri_mid_wt             = get_non_negative_int("MEMBLOCK_SEND_PRI_MID_WT", plus::MEMBLOCK_SEND_PRI_MID_WT);
        send_pri_high_wt            = get_non_negative_int("MEMBLOCK_SEND_PRI_HIGH_WT", plus::MEMBLOCK_SEND_PRI_HIGH_WT);
        send_pri_std_default        = get_non_negative_int("MEMBLOCK_SEND_PRI_STD_DEFAULT", plus::MEMBLOCK_SEND_PRI_STD_DEFAULT);
        send_pri_std_low_wt         = get_non_negative_int("MEMBLOCK_SEND_PRI_STD_LOW_WT", plus::MEMBLOCK_SEND_PRI_STD_LOW_WT);
        send_pri_std_mid_wt         = get_non_negative_int("MEMBLOCK_SEND_PRI_STD_MID_WT", plus::MEMBLOCK_SEND_PRI_STD_MID_WT);
        send_pri_std_high_wt        = get_non_negative_int("MEMBLOCK_SEND_PRI_STD_HIGH_WT", plus::MEMBLOCK_SEND_PRI_STD_HIGH_WT);

        addr_reuse_en_1_wt          = get_non_negative_int("MEMBLOCK_ADDR_REUSE_EN_1_WT", plus::MEMBLOCK_ADDR_REUSE_EN_1_WT);
        addr_reuse_en_0_wt          = get_non_negative_int("MEMBLOCK_ADDR_REUSE_EN_0_WT", plus::MEMBLOCK_ADDR_REUSE_EN_0_WT);
        addr_reuse_load_after_store_wt = get_non_negative_int("MEMBLOCK_ADDR_REUSE_LOAD_AFTER_STORE_WT", plus::MEMBLOCK_ADDR_REUSE_LOAD_AFTER_STORE_WT);
        addr_reuse_load_after_load_wt  = get_non_negative_int("MEMBLOCK_ADDR_REUSE_LOAD_AFTER_LOAD_WT", plus::MEMBLOCK_ADDR_REUSE_LOAD_AFTER_LOAD_WT);
        addr_reuse_store_after_load_wt = get_non_negative_int("MEMBLOCK_ADDR_REUSE_STORE_AFTER_LOAD_WT", plus::MEMBLOCK_ADDR_REUSE_STORE_AFTER_LOAD_WT);
        addr_reuse_store_after_store_wt = get_non_negative_int("MEMBLOCK_ADDR_REUSE_STORE_AFTER_STORE_WT", plus::MEMBLOCK_ADDR_REUSE_STORE_AFTER_STORE_WT);
        addr_reuse_keep_ref_size_en_1_wt = get_non_negative_int("MEMBLOCK_ADDR_REUSE_KEEP_REF_SIZE_EN_1_WT", plus::MEMBLOCK_ADDR_REUSE_KEEP_REF_SIZE_EN_1_WT);
        addr_reuse_keep_ref_size_en_0_wt = get_non_negative_int("MEMBLOCK_ADDR_REUSE_KEEP_REF_SIZE_EN_0_WT", plus::MEMBLOCK_ADDR_REUSE_KEEP_REF_SIZE_EN_0_WT);
        addr_ref_window_fixed       = get_non_negative_int("MEMBLOCK_ADDR_REF_WINDOW_FIXED", plus::MEMBLOCK_ADDR_REF_WINDOW_FIXED);
        addr_ref_window_small_weight = get_non_negative_int("MEMBLOCK_ADDR_REF_WINDOW_SMALL_WEIGHT", plus::MEMBLOCK_ADDR_REF_WINDOW_SMALL_WEIGHT);
        addr_ref_window_medium_weight = get_non_negative_int("MEMBLOCK_ADDR_REF_WINDOW_MEDIUM_WEIGHT", plus::MEMBLOCK_ADDR_REF_WINDOW_MEDIUM_WEIGHT);
        addr_ref_window_large_weight = get_non_negative_int("MEMBLOCK_ADDR_REF_WINDOW_LARGE_WEIGHT", plus::MEMBLOCK_ADDR_REF_WINDOW_LARGE_WEIGHT);
        rob_start_fixed_en          = plus::MEMBLOCK_ROB_START_FIXED_EN;
        rob_start_fixed_value       = get_non_negative_int("MEMBLOCK_ROB_START_FIXED_VALUE", plus::MEMBLOCK_ROB_START_FIXED_VALUE);
        rob_start_zero_wt           = get_non_negative_int("MEMBLOCK_ROB_START_ZERO_WT", plus::MEMBLOCK_ROB_START_ZERO_WT);
        rob_start_mid_wt            = get_non_negative_int("MEMBLOCK_ROB_START_MID_WT", plus::MEMBLOCK_ROB_START_MID_WT);
        rob_start_near_wrap_wt      = get_non_negative_int("MEMBLOCK_ROB_START_NEAR_WRAP_WT", plus::MEMBLOCK_ROB_START_NEAR_WRAP_WT);

        delay_0_wt                  = get_non_negative_int("MEMBLOCK_DELAY_0_WT", plus::MEMBLOCK_DELAY_0_WT);
        delay_1_20_wt               = get_non_negative_int("MEMBLOCK_DELAY_1_20_WT", plus::MEMBLOCK_DELAY_1_20_WT);
        delay_21_50_wt              = get_non_negative_int("MEMBLOCK_DELAY_21_50_WT", plus::MEMBLOCK_DELAY_21_50_WT);

        mdp_load_wait_wt            = get_non_negative_int("MEMBLOCK_MDP_LOAD_WAIT_WT", plus::MEMBLOCK_MDP_LOAD_WAIT_WT);
        mdp_storeset_hit_wt         = get_non_negative_int("MEMBLOCK_MDP_STORESET_HIT_WT", plus::MEMBLOCK_MDP_STORESET_HIT_WT);
        load_wait_strict_wt         = get_non_negative_int("MEMBLOCK_LOAD_WAIT_STRICT_WT", plus::MEMBLOCK_LOAD_WAIT_STRICT_WT);
        rvc_wt                      = get_non_negative_int("MEMBLOCK_RVC_WT", plus::MEMBLOCK_RVC_WT);
        pc_base                     = plus::MEMBLOCK_PC_BASE;
        pc_stride                   = get_non_negative_int("MEMBLOCK_PC_STRIDE", plus::MEMBLOCK_PC_STRIDE);
        ftq_idx_base                = get_non_negative_int("MEMBLOCK_FTQ_IDX_BASE", plus::MEMBLOCK_FTQ_IDX_BASE);
        pdest_base                  = get_non_negative_int("MEMBLOCK_PDEST_BASE", plus::MEMBLOCK_PDEST_BASE);
        pdest_range                 = get_non_negative_int("MEMBLOCK_PDEST_RANGE", plus::MEMBLOCK_PDEST_RANGE);

        tlb_pte_r_1_wt              = get_non_negative_int("MEMBLOCK_TLB_PTE_R_1_WT", plus::MEMBLOCK_TLB_PTE_R_1_WT);
        tlb_pte_r_0_wt              = get_non_negative_int("MEMBLOCK_TLB_PTE_R_0_WT", plus::MEMBLOCK_TLB_PTE_R_0_WT);
        tlb_pte_w_1_wt              = get_non_negative_int("MEMBLOCK_TLB_PTE_W_1_WT", plus::MEMBLOCK_TLB_PTE_W_1_WT);
        tlb_pte_w_0_wt              = get_non_negative_int("MEMBLOCK_TLB_PTE_W_0_WT", plus::MEMBLOCK_TLB_PTE_W_0_WT);
        tlb_pte_x_1_wt              = get_non_negative_int("MEMBLOCK_TLB_PTE_X_1_WT", plus::MEMBLOCK_TLB_PTE_X_1_WT);
        tlb_pte_x_0_wt              = get_non_negative_int("MEMBLOCK_TLB_PTE_X_0_WT", plus::MEMBLOCK_TLB_PTE_X_0_WT);
        tlb_pte_u_1_wt              = get_non_negative_int("MEMBLOCK_TLB_PTE_U_1_WT", plus::MEMBLOCK_TLB_PTE_U_1_WT);
        tlb_pte_u_0_wt              = get_non_negative_int("MEMBLOCK_TLB_PTE_U_0_WT", plus::MEMBLOCK_TLB_PTE_U_0_WT);
        tlb_pte_g_1_wt              = get_non_negative_int("MEMBLOCK_TLB_PTE_G_1_WT", plus::MEMBLOCK_TLB_PTE_G_1_WT);
        tlb_pte_g_0_wt              = get_non_negative_int("MEMBLOCK_TLB_PTE_G_0_WT", plus::MEMBLOCK_TLB_PTE_G_0_WT);
        tlb_pte_a_1_wt              = get_non_negative_int("MEMBLOCK_TLB_PTE_A_1_WT", plus::MEMBLOCK_TLB_PTE_A_1_WT);
        tlb_pte_a_0_wt              = get_non_negative_int("MEMBLOCK_TLB_PTE_A_0_WT", plus::MEMBLOCK_TLB_PTE_A_0_WT);
        tlb_pte_d_1_wt              = get_non_negative_int("MEMBLOCK_TLB_PTE_D_1_WT", plus::MEMBLOCK_TLB_PTE_D_1_WT);
        tlb_pte_d_0_wt              = get_non_negative_int("MEMBLOCK_TLB_PTE_D_0_WT", plus::MEMBLOCK_TLB_PTE_D_0_WT);
        tlb_pte_n_1_wt              = get_non_negative_int("MEMBLOCK_TLB_PTE_N_1_WT", plus::MEMBLOCK_TLB_PTE_N_1_WT);
        tlb_pte_n_0_wt              = get_non_negative_int("MEMBLOCK_TLB_PTE_N_0_WT", plus::MEMBLOCK_TLB_PTE_N_0_WT);
        tlb_pte_v_1_wt              = get_non_negative_int("MEMBLOCK_TLB_PTE_V_1_WT", plus::MEMBLOCK_TLB_PTE_V_1_WT);
        tlb_pte_v_0_wt              = get_non_negative_int("MEMBLOCK_TLB_PTE_V_0_WT", plus::MEMBLOCK_TLB_PTE_V_0_WT);
        tlb_level_mode              = get_non_negative_int("MEMBLOCK_TLB_LEVEL_MODE", plus::MEMBLOCK_TLB_LEVEL_MODE);
        tlb_level_fixed_value       = get_non_negative_int("MEMBLOCK_TLB_LEVEL_FIXED_VALUE", plus::MEMBLOCK_TLB_LEVEL_FIXED_VALUE);
        tlb_level_random_low        = get_non_negative_int("MEMBLOCK_TLB_LEVEL_RANDOM_LOW", plus::MEMBLOCK_TLB_LEVEL_RANDOM_LOW);
        tlb_level_random_high       = get_non_negative_int("MEMBLOCK_TLB_LEVEL_RANDOM_HIGH", plus::MEMBLOCK_TLB_LEVEL_RANDOM_HIGH);
        tlb_pte_mode                = get_non_negative_int("MEMBLOCK_TLB_PTE_MODE", plus::MEMBLOCK_TLB_PTE_MODE);

        paddr_base                  = plus::MEMBLOCK_PADDR_BASE;
        paddr_range                 = plus::MEMBLOCK_PADDR_RANGE;
        active_seq_no_progress_warn_cycles = get_non_negative_int("MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES", plus::MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES);
        dispatch_issue_seq_en       = plus::MEMBLOCK_DISPATCH_ISSUE_SEQ_EN;
        dispatch_issue_nonblocking_en = plus::MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN;
        dispatch_ready_timeout      = get_non_negative_int("MEMBLOCK_DISPATCH_READY_TIMEOUT", plus::MEMBLOCK_DISPATCH_READY_TIMEOUT);
        redirect_seq_en             = plus::MEMBLOCK_REDIRECT_SEQ_EN;
        redirect_drive_timeout      = get_non_negative_int("MEMBLOCK_REDIRECT_DRIVE_TIMEOUT", plus::MEMBLOCK_REDIRECT_DRIVE_TIMEOUT);
        redirect_freeze_timeout     = get_non_negative_int("MEMBLOCK_REDIRECT_FREEZE_TIMEOUT", plus::MEMBLOCK_REDIRECT_FREEZE_TIMEOUT);
        sta_real_wb_pass_en         = plus::MEMBLOCK_STA_REAL_WB_PASS_EN;
        std_real_wb_pass_en         = plus::MEMBLOCK_STD_REAL_WB_PASS_EN;
        lsqenq_seq_en               = plus::MEMBLOCK_LSQENQ_SEQ_EN;
        lsqenq_ready_timeout        = get_non_negative_int("MEMBLOCK_LSQENQ_READY_TIMEOUT", plus::MEMBLOCK_LSQENQ_READY_TIMEOUT);
        lsqcommit_seq_en            = plus::MEMBLOCK_LSQCOMMIT_SEQ_EN;
        flushsb_seq_en              = plus::MEMBLOCK_FLUSHSB_SEQ_EN;
        flushsb_request_cycle       = get_non_negative_int("MEMBLOCK_FLUSHSB_REQUEST_CYCLE", plus::MEMBLOCK_FLUSHSB_REQUEST_CYCLE);
        flushsb_timeout             = get_non_negative_int("MEMBLOCK_FLUSHSB_TIMEOUT", plus::MEMBLOCK_FLUSHSB_TIMEOUT);
        replay_wait_ptw_en          = plus::MEMBLOCK_REPLAY_WAIT_PTW_EN;
        replay_wait_ptw_timeout     = get_non_negative_int("MEMBLOCK_REPLAY_WAIT_PTW_TIMEOUT", plus::MEMBLOCK_REPLAY_WAIT_PTW_TIMEOUT);
        l2tlb_seq_en                = plus::MEMBLOCK_L2TLB_SEQ_EN;
        l2tlb_min_latency           = get_non_negative_int("MEMBLOCK_L2TLB_MIN_LATENCY", plus::MEMBLOCK_L2TLB_MIN_LATENCY);
        l2tlb_max_latency           = get_non_negative_int("MEMBLOCK_L2TLB_MAX_LATENCY", plus::MEMBLOCK_L2TLB_MAX_LATENCY);
        l2tlb_idle_stop_cycle       = get_non_negative_int("MEMBLOCK_L2TLB_IDLE_STOP_CYCLE", plus::MEMBLOCK_L2TLB_IDLE_STOP_CYCLE);
    endfunction:load_from_plus

    static function void validate_and_clamp();
        fatal_if_zero("main_trans_num", main_trans_num);
        fatal_if_zero("paddr_range", paddr_range);
        fatal_if_zero("real_enq_width", real_enq_width);
        fatal_if_zero("real_load_pipe_num", real_load_pipe_num);
        fatal_if_zero("real_sta_pipe_num", real_sta_pipe_num);
        fatal_if_zero("real_std_pipe_num", real_std_pipe_num);
        fatal_if_zero("real_lsq_enq_max", real_lsq_enq_max);
        clamp_int("real_enq_width", real_enq_width, 1, 8);
        clamp_int("real_lsq_enq_max", real_lsq_enq_max, 1, 8);
        if (real_enq_width != real_lsq_enq_max) begin
            `uvm_fatal("SEQ_CSR_CFG",
                       $sformatf("MEMBLOCK_REAL_ENQ_WIDTH=%0d must equal MEMBLOCK_REAL_LSQ_ENQ_MAX=%0d",
                                 real_enq_width,
                                 real_lsq_enq_max))
        end

        if ((paddr_base + paddr_range) < paddr_base) begin
            `uvm_fatal("SEQ_CSR_CFG", "paddr_base + paddr_range overflows")
        end

        clamp_int("send_pri_default", send_pri_default, 0, 100);
        clamp_int("send_pri_std_default", send_pri_std_default, 0, 100);
        clamp_int("global_send_pri_en_wt", global_send_pri_en_wt, 0, 100);
        fatal_if_all_zero2("addr_reuse_en weights", addr_reuse_en_1_wt, addr_reuse_en_0_wt);
        fatal_if_all_zero4("addr_reuse kind weights",
                           addr_reuse_load_after_store_wt,
                           addr_reuse_load_after_load_wt,
                           addr_reuse_store_after_load_wt,
                           addr_reuse_store_after_store_wt);
        fatal_if_all_zero2("addr_reuse_keep_ref_size_en weights",
                           addr_reuse_keep_ref_size_en_1_wt,
                           addr_reuse_keep_ref_size_en_0_wt);
        if (addr_ref_window_fixed > 0) begin
            clamp_int("addr_ref_window_fixed", addr_ref_window_fixed, 1, get_addr_ref_window_max());
        end else begin
            fatal_if_all_zero3("addr_ref_window weights",
                               addr_ref_window_small_weight,
                               addr_ref_window_medium_weight,
                               addr_ref_window_large_weight);
        end
        clamp_int("rob_start_fixed_value", rob_start_fixed_value, 0, MEMBLOCK_ROB_SIZE - 1);
        if (!rob_start_fixed_en) begin
            fatal_if_all_zero3("rob_start weights", rob_start_zero_wt, rob_start_mid_wt, rob_start_near_wrap_wt);
        end
        clamp_int("mdp_load_wait_wt", mdp_load_wait_wt, 0, 100);
        clamp_int("mdp_storeset_hit_wt", mdp_storeset_hit_wt, 0, 100);
        clamp_int("load_wait_strict_wt", load_wait_strict_wt, 0, 100);
        clamp_int("rvc_wt", rvc_wt, 0, 100);

        if (enq_per_cycle == 0 || enq_per_cycle > real_enq_width) begin
            `uvm_fatal("SEQ_CSR_CFG",
                       $sformatf("MEMBLOCK_ENQ_PER_CYCLE=%0d must be in [1:%0d]",
                                 enq_per_cycle,
                                 real_enq_width))
        end
        clamp_int("load_pip_num_limit", load_pip_num_limit, 1, real_load_pipe_num);
        clamp_int("sta_pip_num_limit", sta_pip_num_limit, 1, real_sta_pipe_num);
        clamp_int("std_pip_num_limit", std_pip_num_limit, 1, real_std_pipe_num);
        if (dispatch_issue_seq_en && dispatch_ready_timeout == 0) begin
            `uvm_warning("SEQ_CSR_CFG", "dispatch_ready_timeout=0 while dispatch issue sequence is enabled, clamp to 1")
            dispatch_ready_timeout = 1;
        end
        if (redirect_seq_en && redirect_drive_timeout == 0) begin
            `uvm_warning("SEQ_CSR_CFG", "redirect_drive_timeout=0 while redirect sequence is enabled, clamp to 1")
            redirect_drive_timeout = 1;
        end
        if (redirect_freeze_timeout == 0) begin
            `uvm_warning("SEQ_CSR_CFG", "redirect_freeze_timeout=0, clamp to 1")
            redirect_freeze_timeout = 1;
        end
        if (lsqenq_seq_en && lsqenq_ready_timeout == 0) begin
            `uvm_warning("SEQ_CSR_CFG", "lsqenq_ready_timeout=0 while lsqenq sequence is enabled, clamp to 1")
            lsqenq_ready_timeout = 1;
        end
        if (flushsb_seq_en && flushsb_request_cycle != 0 && !lsqcommit_seq_en) begin
            `uvm_info("SEQ_CSR_CFG",
                      "MEMBLOCK_FLUSHSB_REQUEST_CYCLE is configured but MEMBLOCK_LSQCOMMIT_SEQ_EN=0; periodic flushSb producer will stay idle",
                      UVM_LOW)
        end
        if (replay_wait_ptw_en && replay_wait_ptw_timeout == 0) begin
            `uvm_warning("SEQ_CSR_CFG", "replay_wait_ptw_timeout=0 while replay wait PTW is enabled, clamp to 1")
            replay_wait_ptw_timeout = 1;
        end
        if (l2tlb_min_latency > l2tlb_max_latency) begin
            `uvm_warning("SEQ_CSR_CFG",
                         $sformatf("l2tlb_min_latency=%0d exceeds max=%0d, set max to min",
                                   l2tlb_min_latency,
                                   l2tlb_max_latency))
            l2tlb_max_latency = l2tlb_min_latency;
        end
        if (l2tlb_seq_en && l2tlb_idle_stop_cycle == 0) begin
            `uvm_warning("SEQ_CSR_CFG", "l2tlb_idle_stop_cycle=0 while L2TLB sequence is enabled, clamp to 1")
            l2tlb_idle_stop_cycle = 1;
        end
        fatal_if_zero("pc_stride", pc_stride);
        fatal_if_zero("pdest_range", pdest_range);
        clamp_int("ftq_idx_base", ftq_idx_base, 0, 63);
        clamp_int("pdest_base", pdest_base, 0, 255);
        clamp_int("pdest_range", pdest_range, 1, 256);

        if (send_pri_mode_en) begin
            fatal_if_all_zero3("send_pri weights", send_pri_low_wt, send_pri_mid_wt, send_pri_high_wt);
            fatal_if_all_zero3("send_pri_std weights", send_pri_std_low_wt, send_pri_std_mid_wt, send_pri_std_high_wt);
        end
        fatal_if_all_zero3("delay weights", delay_0_wt, delay_1_20_wt, delay_21_50_wt);
        fatal_if_all_zero6("op_class weights",
                           op_class_int_load_wt,
                           op_class_fp_load_wt,
                           op_class_store_wt,
                           op_class_prefetch_wt,
                           op_class_amo_wt,
                           op_class_cbo_wt);
        if (!boundary_profile_gen_en) begin
            if ((op_class_int_load_wt != 0 || op_class_fp_load_wt != 0) &&
                (load_fuop_lb_wt + load_fuop_lh_wt + load_fuop_lw_wt + load_fuop_ld_wt +
                 load_fuop_lbu_wt + load_fuop_lhu_wt + load_fuop_lwu_wt) == 0) begin
                `uvm_fatal("SEQ_CSR_CFG", "load fuOpType weights must not be all zero when LOAD op_class weight is non-zero")
            end
            if (op_class_store_wt != 0 &&
                (store_fuop_sb_wt + store_fuop_sh_wt + store_fuop_sw_wt + store_fuop_sd_wt) == 0) begin
                `uvm_fatal("SEQ_CSR_CFG", "store fuOpType weights must not be all zero when STORE op_class weight is non-zero")
            end
            if (op_class_prefetch_wt != 0 &&
                (prefetch_fuop_i_wt + prefetch_fuop_r_wt + prefetch_fuop_w_wt) == 0) begin
                `uvm_fatal("SEQ_CSR_CFG", "prefetch fuOpType weights must not be all zero when PREFETCH op_class weight is non-zero")
            end
            if (op_class_cbo_wt != 0 &&
                (cbo_fuop_zero_wt + cbo_fuop_clean_wt + cbo_fuop_flush_wt + cbo_fuop_inval_wt) == 0) begin
                `uvm_fatal("SEQ_CSR_CFG", "CBO fuOpType weights must not be all zero when CBO op_class weight is non-zero")
            end
            if (op_class_amo_wt != 0 &&
                (amo_fuop_lr_w_wt + amo_fuop_sc_w_wt + amo_fuop_amoswap_w_wt + amo_fuop_amoadd_w_wt +
                 amo_fuop_amoxor_w_wt + amo_fuop_amoand_w_wt + amo_fuop_amoor_w_wt +
                 amo_fuop_amomin_w_wt + amo_fuop_amomax_w_wt + amo_fuop_amominu_w_wt +
                 amo_fuop_amomaxu_w_wt + amo_fuop_lr_d_wt + amo_fuop_sc_d_wt +
                 amo_fuop_amoswap_d_wt + amo_fuop_amoadd_d_wt + amo_fuop_amoxor_d_wt +
                 amo_fuop_amoand_d_wt + amo_fuop_amoor_d_wt + amo_fuop_amomin_d_wt +
                 amo_fuop_amomax_d_wt + amo_fuop_amominu_d_wt + amo_fuop_amomaxu_d_wt) == 0) begin
                `uvm_fatal("SEQ_CSR_CFG", "AMO fuOpType weights must not be all zero when AMO op_class weight is non-zero")
            end
        end
        if (boundary_profile_gen_en) begin
            fatal_if_all_zero6("boundary_profile weights",
                               boundary_aligned_wt,
                               boundary_misalign_within_8b_wt,
                               boundary_cross_8b_within_16b_wt,
                               boundary_cross_16b_same_line_wt,
                               boundary_cross_cacheline_same_4k_wt,
                               boundary_cross_4k_wt);
        end

        fatal_if_all_zero2("tlb_pte_r weights", tlb_pte_r_1_wt, tlb_pte_r_0_wt);
        fatal_if_all_zero2("tlb_pte_w weights", tlb_pte_w_1_wt, tlb_pte_w_0_wt);
        fatal_if_all_zero2("tlb_pte_x weights", tlb_pte_x_1_wt, tlb_pte_x_0_wt);
        fatal_if_all_zero2("tlb_pte_u weights", tlb_pte_u_1_wt, tlb_pte_u_0_wt);
        fatal_if_all_zero2("tlb_pte_g weights", tlb_pte_g_1_wt, tlb_pte_g_0_wt);
        fatal_if_all_zero2("tlb_pte_a weights", tlb_pte_a_1_wt, tlb_pte_a_0_wt);
        fatal_if_all_zero2("tlb_pte_d weights", tlb_pte_d_1_wt, tlb_pte_d_0_wt);
        fatal_if_all_zero2("tlb_pte_n weights", tlb_pte_n_1_wt, tlb_pte_n_0_wt);
        fatal_if_all_zero2("tlb_pte_v weights", tlb_pte_v_1_wt, tlb_pte_v_0_wt);
        clamp_int("tlb_level_mode", tlb_level_mode, 0, 2);
        clamp_int("tlb_level_fixed_value", tlb_level_fixed_value, 0, 3);
        clamp_int("tlb_level_random_low", tlb_level_random_low, 0, 3);
        clamp_int("tlb_level_random_high", tlb_level_random_high, 0, 3);
        clamp_int("tlb_pte_mode", tlb_pte_mode, 0, 2);
        if (tlb_level_random_high < tlb_level_random_low) begin
            `uvm_warning("SEQ_CSR_CFG",
                         $sformatf("tlb_level_random_high(%0d) < tlb_level_random_low(%0d), clamp high to low",
                                   tlb_level_random_high, tlb_level_random_low))
            tlb_level_random_high = tlb_level_random_low;
        end
    endfunction:validate_and_clamp

    static function int unsigned get_non_negative_int(string name, int value);
        if (value < 0) begin
            `uvm_fatal("SEQ_CSR_CFG", $sformatf("%s must not be negative, got %0d", name, value))
        end
        return value;
    endfunction:get_non_negative_int

    static function void fatal_if_zero(string name, longint unsigned value);
        if (value == 0) begin
            `uvm_fatal("SEQ_CSR_CFG", $sformatf("%s must be non-zero", name))
        end
    endfunction:fatal_if_zero

    static function void fatal_if_all_zero2(string name, int unsigned w0, int unsigned w1);
        if (w0 == 0 && w1 == 0) begin
            `uvm_fatal("SEQ_CSR_CFG", $sformatf("%s must not be all zero", name))
        end
    endfunction:fatal_if_all_zero2

    static function void fatal_if_all_zero3(string name, int unsigned w0, int unsigned w1, int unsigned w2);
        if (w0 == 0 && w1 == 0 && w2 == 0) begin
            `uvm_fatal("SEQ_CSR_CFG", $sformatf("%s must not be all zero", name))
        end
    endfunction:fatal_if_all_zero3

    static function void fatal_if_all_zero4(string name, int unsigned w0, int unsigned w1, int unsigned w2, int unsigned w3);
        if (w0 == 0 && w1 == 0 && w2 == 0 && w3 == 0) begin
            `uvm_fatal("SEQ_CSR_CFG", $sformatf("%s must not be all zero", name))
        end
    endfunction:fatal_if_all_zero4

    static function void fatal_if_all_zero5(string name, int unsigned w0, int unsigned w1, int unsigned w2, int unsigned w3, int unsigned w4);
        if (w0 == 0 && w1 == 0 && w2 == 0 && w3 == 0 && w4 == 0) begin
            `uvm_fatal("SEQ_CSR_CFG", $sformatf("%s must not be all zero", name))
        end
    endfunction:fatal_if_all_zero5

    static function void fatal_if_all_zero6(string name,
                                            int unsigned w0,
                                            int unsigned w1,
                                            int unsigned w2,
                                            int unsigned w3,
                                            int unsigned w4,
                                            int unsigned w5);
        if (w0 == 0 && w1 == 0 && w2 == 0 && w3 == 0 && w4 == 0 && w5 == 0) begin
            `uvm_fatal("SEQ_CSR_CFG", $sformatf("%s must not be all zero", name))
        end
    endfunction:fatal_if_all_zero6

    static function void clamp_int(input string name, ref int unsigned value, input int unsigned min_v, input int unsigned max_v);
        if (value < min_v) begin
            `uvm_warning("SEQ_CSR_CFG", $sformatf("%s=%0d is below min %0d, clamp to %0d", name, value, min_v, min_v))
            value = min_v;
        end else if (value > max_v) begin
            `uvm_warning("SEQ_CSR_CFG", $sformatf("%s=%0d exceeds max %0d, clamp to %0d", name, value, max_v, max_v))
            value = max_v;
        end
    endfunction:clamp_int

    static function int unsigned get_main_trans_num();
        check_initialized("get_main_trans_num");
        return main_trans_num;
    endfunction:get_main_trans_num

    static function bit get_use_manual_main_table();
        check_initialized("get_use_manual_main_table");
        return use_manual_main_table;
    endfunction:get_use_manual_main_table

    static function int unsigned get_enq_per_cycle();
        check_initialized("get_enq_per_cycle");
        if (enq_per_cycle_rand_en) begin
            return $urandom_range(real_enq_width, 1);
        end
        return enq_per_cycle;
    endfunction:get_enq_per_cycle

    static function int unsigned get_real_lsq_enq_max();
        check_initialized("get_real_lsq_enq_max");
        return real_lsq_enq_max;
    endfunction:get_real_lsq_enq_max

    static function int unsigned get_load_pip_num_limit();
        check_initialized("get_load_pip_num_limit");
        return load_pip_num_limit;
    endfunction:get_load_pip_num_limit

    static function int unsigned get_sta_pip_num_limit();
        check_initialized("get_sta_pip_num_limit");
        return sta_pip_num_limit;
    endfunction:get_sta_pip_num_limit

    static function int unsigned get_std_pip_num_limit();
        check_initialized("get_std_pip_num_limit");
        return std_pip_num_limit;
    endfunction:get_std_pip_num_limit

    static function bit get_load_pip_num_random_en();
        check_initialized("get_load_pip_num_random_en");
        return load_pip_num_random_en;
    endfunction:get_load_pip_num_random_en

    static function bit get_sta_pip_num_random_en();
        check_initialized("get_sta_pip_num_random_en");
        return sta_pip_num_random_en;
    endfunction:get_sta_pip_num_random_en

    static function bit get_std_pip_num_random_en();
        check_initialized("get_std_pip_num_random_en");
        return std_pip_num_random_en;
    endfunction:get_std_pip_num_random_en

    static function int unsigned sample_load_pip_num();
        check_initialized("sample_load_pip_num");
        if (load_pip_num_random_en) begin
            return $urandom_range(load_pip_num_limit, 1);
        end
        return load_pip_num_limit;
    endfunction:sample_load_pip_num

    static function int unsigned sample_sta_pip_num();
        check_initialized("sample_sta_pip_num");
        if (sta_pip_num_random_en) begin
            return $urandom_range(sta_pip_num_limit, 1);
        end
        return sta_pip_num_limit;
    endfunction:sample_sta_pip_num

    static function int unsigned sample_std_pip_num();
        check_initialized("sample_std_pip_num");
        if (std_pip_num_random_en) begin
            return $urandom_range(std_pip_num_limit, 1);
        end
        return std_pip_num_limit;
    endfunction:sample_std_pip_num

    static function int unsigned get_real_enq_width();
        check_initialized("get_real_enq_width");
        return real_enq_width;
    endfunction:get_real_enq_width

    static function int unsigned get_real_load_pipe_num();
        check_initialized("get_real_load_pipe_num");
        return real_load_pipe_num;
    endfunction:get_real_load_pipe_num

    static function int unsigned get_real_sta_pipe_num();
        check_initialized("get_real_sta_pipe_num");
        return real_sta_pipe_num;
    endfunction:get_real_sta_pipe_num

    static function int unsigned get_real_std_pipe_num();
        check_initialized("get_real_std_pipe_num");
        return real_std_pipe_num;
    endfunction:get_real_std_pipe_num

    static function int unsigned get_op_class_int_load_wt();
        check_initialized("get_op_class_int_load_wt");
        return op_class_int_load_wt;
    endfunction:get_op_class_int_load_wt

    static function int unsigned get_op_class_fp_load_wt();
        check_initialized("get_op_class_fp_load_wt");
        return op_class_fp_load_wt;
    endfunction:get_op_class_fp_load_wt

    static function int unsigned get_op_class_store_wt();
        check_initialized("get_op_class_store_wt");
        return op_class_store_wt;
    endfunction:get_op_class_store_wt

    static function int unsigned get_op_class_prefetch_wt();
        check_initialized("get_op_class_prefetch_wt");
        return op_class_prefetch_wt;
    endfunction:get_op_class_prefetch_wt

    static function int unsigned get_op_class_amo_wt();
        check_initialized("get_op_class_amo_wt");
        return op_class_amo_wt;
    endfunction:get_op_class_amo_wt

    static function int unsigned get_op_class_cbo_wt();
        check_initialized("get_op_class_cbo_wt");
        return op_class_cbo_wt;
    endfunction:get_op_class_cbo_wt

    static function int unsigned get_fuop_weight(input memblock_op_class_e op_class,
                                                 input bit [8:0] fuOpType);
        check_initialized("get_fuop_weight");
        case (op_class)
            MEMBLOCK_OP_CLASS_INT_LOAD,
            MEMBLOCK_OP_CLASS_FP_LOAD: begin
                case (fuOpType)
                    MEMBLOCK_LSUOP_LB:  return load_fuop_lb_wt;
                    MEMBLOCK_LSUOP_LH:  return load_fuop_lh_wt;
                    MEMBLOCK_LSUOP_LW:  return load_fuop_lw_wt;
                    MEMBLOCK_LSUOP_LD:  return load_fuop_ld_wt;
                    MEMBLOCK_LSUOP_LBU: return load_fuop_lbu_wt;
                    MEMBLOCK_LSUOP_LHU: return load_fuop_lhu_wt;
                    MEMBLOCK_LSUOP_LWU: return load_fuop_lwu_wt;
                    default:            return 0;
                endcase
            end
            MEMBLOCK_OP_CLASS_STORE: begin
                case (fuOpType)
                    MEMBLOCK_LSUOP_SB: return store_fuop_sb_wt;
                    MEMBLOCK_LSUOP_SH: return store_fuop_sh_wt;
                    MEMBLOCK_LSUOP_SW: return store_fuop_sw_wt;
                    MEMBLOCK_LSUOP_SD: return store_fuop_sd_wt;
                    default:           return 0;
                endcase
            end
            MEMBLOCK_OP_CLASS_PREFETCH: begin
                case (fuOpType)
                    MEMBLOCK_LSUOP_PREFETCH_I: return prefetch_fuop_i_wt;
                    MEMBLOCK_LSUOP_PREFETCH_R: return prefetch_fuop_r_wt;
                    MEMBLOCK_LSUOP_PREFETCH_W: return prefetch_fuop_w_wt;
                    default:                   return 0;
                endcase
            end
            MEMBLOCK_OP_CLASS_CBO: begin
                case (fuOpType)
                    MEMBLOCK_LSUOP_CBO_ZERO:  return cbo_fuop_zero_wt;
                    MEMBLOCK_LSUOP_CBO_CLEAN: return cbo_fuop_clean_wt;
                    MEMBLOCK_LSUOP_CBO_FLUSH: return cbo_fuop_flush_wt;
                    MEMBLOCK_LSUOP_CBO_INVAL: return cbo_fuop_inval_wt;
                    default:                  return 0;
                endcase
            end
            MEMBLOCK_OP_CLASS_AMO: begin
                case (fuOpType)
                    MEMBLOCK_LSUOP_LR_W:       return amo_fuop_lr_w_wt;
                    MEMBLOCK_LSUOP_SC_W:       return amo_fuop_sc_w_wt;
                    MEMBLOCK_LSUOP_AMOSWAP_W:  return amo_fuop_amoswap_w_wt;
                    MEMBLOCK_LSUOP_AMOADD_W:   return amo_fuop_amoadd_w_wt;
                    MEMBLOCK_LSUOP_AMOXOR_W:   return amo_fuop_amoxor_w_wt;
                    MEMBLOCK_LSUOP_AMOAND_W:   return amo_fuop_amoand_w_wt;
                    MEMBLOCK_LSUOP_AMOOR_W:    return amo_fuop_amoor_w_wt;
                    MEMBLOCK_LSUOP_AMOMIN_W:   return amo_fuop_amomin_w_wt;
                    MEMBLOCK_LSUOP_AMOMAX_W:   return amo_fuop_amomax_w_wt;
                    MEMBLOCK_LSUOP_AMOMINU_W:  return amo_fuop_amominu_w_wt;
                    MEMBLOCK_LSUOP_AMOMAXU_W:  return amo_fuop_amomaxu_w_wt;
                    MEMBLOCK_LSUOP_LR_D:       return amo_fuop_lr_d_wt;
                    MEMBLOCK_LSUOP_SC_D:       return amo_fuop_sc_d_wt;
                    MEMBLOCK_LSUOP_AMOSWAP_D:  return amo_fuop_amoswap_d_wt;
                    MEMBLOCK_LSUOP_AMOADD_D:   return amo_fuop_amoadd_d_wt;
                    MEMBLOCK_LSUOP_AMOXOR_D:   return amo_fuop_amoxor_d_wt;
                    MEMBLOCK_LSUOP_AMOAND_D:   return amo_fuop_amoand_d_wt;
                    MEMBLOCK_LSUOP_AMOOR_D:    return amo_fuop_amoor_d_wt;
                    MEMBLOCK_LSUOP_AMOMIN_D:   return amo_fuop_amomin_d_wt;
                    MEMBLOCK_LSUOP_AMOMAX_D:   return amo_fuop_amomax_d_wt;
                    MEMBLOCK_LSUOP_AMOMINU_D:  return amo_fuop_amominu_d_wt;
                    MEMBLOCK_LSUOP_AMOMAXU_D:  return amo_fuop_amomaxu_d_wt;
                    default:                   return 0;
                endcase
            end
            default: begin
                return 0;
            end
        endcase
    endfunction:get_fuop_weight

    static function bit get_boundary_profile_gen_en();
        check_initialized("get_boundary_profile_gen_en");
        return boundary_profile_gen_en;
    endfunction:get_boundary_profile_gen_en

    static function int unsigned get_boundary_profile_weight(input memblock_boundary_profile_e profile);
        check_initialized("get_boundary_profile_weight");
        case (profile)
            MEMBLOCK_BOUNDARY_PROFILE_ALIGNED:                 return boundary_aligned_wt;
            MEMBLOCK_BOUNDARY_PROFILE_MISALIGN_WITHIN_8B:      return boundary_misalign_within_8b_wt;
            MEMBLOCK_BOUNDARY_PROFILE_CROSS_8B_WITHIN_16B:     return boundary_cross_8b_within_16b_wt;
            MEMBLOCK_BOUNDARY_PROFILE_CROSS_16B_SAME_LINE:     return boundary_cross_16b_same_line_wt;
            MEMBLOCK_BOUNDARY_PROFILE_CROSS_CACHELINE_SAME_4K: return boundary_cross_cacheline_same_4k_wt;
            MEMBLOCK_BOUNDARY_PROFILE_CROSS_4K:                return boundary_cross_4k_wt;
            default:                                           return 0;
        endcase
    endfunction:get_boundary_profile_weight

    static function bit get_store_cross_8b_within_16b_en();
        check_initialized("get_store_cross_8b_within_16b_en");
        return store_cross_8b_within_16b_en;
    endfunction:get_store_cross_8b_within_16b_en

    static function bit get_lsq_resync_on_mismatch();
        check_initialized("get_lsq_resync_on_mismatch");
        return lsq_resync_on_mismatch;
    endfunction:get_lsq_resync_on_mismatch

    static function bit get_send_pri_mode_en();
        check_initialized("get_send_pri_mode_en");
        return send_pri_mode_en;
    endfunction:get_send_pri_mode_en

    static function int unsigned get_global_send_pri_en_wt();
        check_initialized("get_global_send_pri_en_wt");
        return global_send_pri_en_wt;
    endfunction:get_global_send_pri_en_wt

    static function bit sample_global_send_pri_en();
        check_initialized("sample_global_send_pri_en");
        if (!send_pri_mode_en || global_send_pri_en_wt == 0) begin
            return 1'b0;
        end
        if (global_send_pri_en_wt >= 100) begin
            return 1'b1;
        end
        return $urandom_range(99, 0) < global_send_pri_en_wt;
    endfunction:sample_global_send_pri_en

    static function int unsigned get_send_pri_default();
        check_initialized("get_send_pri_default");
        return send_pri_default;
    endfunction:get_send_pri_default

    static function int unsigned get_send_pri_std_default();
        check_initialized("get_send_pri_std_default");
        return send_pri_std_default;
    endfunction:get_send_pri_std_default

    static function int unsigned get_send_pri_low_wt();
        check_initialized("get_send_pri_low_wt");
        return send_pri_low_wt;
    endfunction:get_send_pri_low_wt

    static function int unsigned get_send_pri_mid_wt();
        check_initialized("get_send_pri_mid_wt");
        return send_pri_mid_wt;
    endfunction:get_send_pri_mid_wt

    static function int unsigned get_send_pri_high_wt();
        check_initialized("get_send_pri_high_wt");
        return send_pri_high_wt;
    endfunction:get_send_pri_high_wt

    static function int unsigned get_send_pri_std_low_wt();
        check_initialized("get_send_pri_std_low_wt");
        return send_pri_std_low_wt;
    endfunction:get_send_pri_std_low_wt

    static function int unsigned get_send_pri_std_mid_wt();
        check_initialized("get_send_pri_std_mid_wt");
        return send_pri_std_mid_wt;
    endfunction:get_send_pri_std_mid_wt

    static function int unsigned get_send_pri_std_high_wt();
        check_initialized("get_send_pri_std_high_wt");
        return send_pri_std_high_wt;
    endfunction:get_send_pri_std_high_wt

    static function int unsigned get_addr_reuse_en_1_wt();
        check_initialized("get_addr_reuse_en_1_wt");
        return addr_reuse_en_1_wt;
    endfunction:get_addr_reuse_en_1_wt

    static function int unsigned get_addr_reuse_en_0_wt();
        check_initialized("get_addr_reuse_en_0_wt");
        return addr_reuse_en_0_wt;
    endfunction:get_addr_reuse_en_0_wt

    static function int unsigned get_addr_reuse_load_after_store_wt();
        check_initialized("get_addr_reuse_load_after_store_wt");
        return addr_reuse_load_after_store_wt;
    endfunction:get_addr_reuse_load_after_store_wt

    static function int unsigned get_addr_reuse_load_after_load_wt();
        check_initialized("get_addr_reuse_load_after_load_wt");
        return addr_reuse_load_after_load_wt;
    endfunction:get_addr_reuse_load_after_load_wt

    static function int unsigned get_addr_reuse_store_after_load_wt();
        check_initialized("get_addr_reuse_store_after_load_wt");
        return addr_reuse_store_after_load_wt;
    endfunction:get_addr_reuse_store_after_load_wt

    static function int unsigned get_addr_reuse_store_after_store_wt();
        check_initialized("get_addr_reuse_store_after_store_wt");
        return addr_reuse_store_after_store_wt;
    endfunction:get_addr_reuse_store_after_store_wt

    static function int unsigned get_addr_reuse_keep_ref_size_en_1_wt();
        check_initialized("get_addr_reuse_keep_ref_size_en_1_wt");
        return addr_reuse_keep_ref_size_en_1_wt;
    endfunction:get_addr_reuse_keep_ref_size_en_1_wt

    static function int unsigned get_addr_reuse_keep_ref_size_en_0_wt();
        check_initialized("get_addr_reuse_keep_ref_size_en_0_wt");
        return addr_reuse_keep_ref_size_en_0_wt;
    endfunction:get_addr_reuse_keep_ref_size_en_0_wt

    static function int unsigned get_addr_ref_window_fixed();
        check_initialized("get_addr_ref_window_fixed");
        return addr_ref_window_fixed;
    endfunction:get_addr_ref_window_fixed

    static function int unsigned get_addr_ref_window_small_weight();
        check_initialized("get_addr_ref_window_small_weight");
        return addr_ref_window_small_weight;
    endfunction:get_addr_ref_window_small_weight

    static function int unsigned get_addr_ref_window_medium_weight();
        check_initialized("get_addr_ref_window_medium_weight");
        return addr_ref_window_medium_weight;
    endfunction:get_addr_ref_window_medium_weight

    static function int unsigned get_addr_ref_window_large_weight();
        check_initialized("get_addr_ref_window_large_weight");
        return addr_ref_window_large_weight;
    endfunction:get_addr_ref_window_large_weight

    static function int unsigned get_addr_ref_window_max();
        return (MEMBLOCK_LQ_SIZE < MEMBLOCK_SQ_SIZE) ? MEMBLOCK_LQ_SIZE : MEMBLOCK_SQ_SIZE;
    endfunction:get_addr_ref_window_max

    static function bit get_rob_start_fixed_en();
        check_initialized("get_rob_start_fixed_en");
        return rob_start_fixed_en;
    endfunction:get_rob_start_fixed_en

    static function int unsigned get_rob_start_fixed_value();
        check_initialized("get_rob_start_fixed_value");
        return rob_start_fixed_value;
    endfunction:get_rob_start_fixed_value

    static function int unsigned get_rob_start_zero_wt();
        check_initialized("get_rob_start_zero_wt");
        return rob_start_zero_wt;
    endfunction:get_rob_start_zero_wt

    static function int unsigned get_rob_start_mid_wt();
        check_initialized("get_rob_start_mid_wt");
        return rob_start_mid_wt;
    endfunction:get_rob_start_mid_wt

    static function int unsigned get_rob_start_near_wrap_wt();
        check_initialized("get_rob_start_near_wrap_wt");
        return rob_start_near_wrap_wt;
    endfunction:get_rob_start_near_wrap_wt

    static function int unsigned get_delay_0_wt();
        check_initialized("get_delay_0_wt");
        return delay_0_wt;
    endfunction:get_delay_0_wt

    static function int unsigned get_delay_1_20_wt();
        check_initialized("get_delay_1_20_wt");
        return delay_1_20_wt;
    endfunction:get_delay_1_20_wt

    static function int unsigned get_delay_21_50_wt();
        check_initialized("get_delay_21_50_wt");
        return delay_21_50_wt;
    endfunction:get_delay_21_50_wt

    static function int unsigned get_mdp_load_wait_wt();
        check_initialized("get_mdp_load_wait_wt");
        return mdp_load_wait_wt;
    endfunction:get_mdp_load_wait_wt

    static function int unsigned get_mdp_storeset_hit_wt();
        check_initialized("get_mdp_storeset_hit_wt");
        return mdp_storeset_hit_wt;
    endfunction:get_mdp_storeset_hit_wt

    static function int unsigned get_load_wait_strict_wt();
        check_initialized("get_load_wait_strict_wt");
        return load_wait_strict_wt;
    endfunction:get_load_wait_strict_wt

    static function int unsigned get_rvc_wt();
        check_initialized("get_rvc_wt");
        return rvc_wt;
    endfunction:get_rvc_wt

    static function bit [63:0] get_pc_base();
        check_initialized("get_pc_base");
        return pc_base;
    endfunction:get_pc_base

    static function int unsigned get_pc_stride();
        check_initialized("get_pc_stride");
        return pc_stride;
    endfunction:get_pc_stride

    static function int unsigned get_ftq_idx_base();
        check_initialized("get_ftq_idx_base");
        return ftq_idx_base;
    endfunction:get_ftq_idx_base

    static function int unsigned get_pdest_base();
        check_initialized("get_pdest_base");
        return pdest_base;
    endfunction:get_pdest_base

    static function int unsigned get_pdest_range();
        check_initialized("get_pdest_range");
        return pdest_range;
    endfunction:get_pdest_range

    static function int unsigned get_tlb_pte_r_1_wt();
        check_initialized("get_tlb_pte_r_1_wt");
        return tlb_pte_r_1_wt;
    endfunction:get_tlb_pte_r_1_wt

    static function int unsigned get_tlb_pte_r_0_wt();
        check_initialized("get_tlb_pte_r_0_wt");
        return tlb_pte_r_0_wt;
    endfunction:get_tlb_pte_r_0_wt

    static function int unsigned get_tlb_pte_w_1_wt();
        check_initialized("get_tlb_pte_w_1_wt");
        return tlb_pte_w_1_wt;
    endfunction:get_tlb_pte_w_1_wt

    static function int unsigned get_tlb_pte_w_0_wt();
        check_initialized("get_tlb_pte_w_0_wt");
        return tlb_pte_w_0_wt;
    endfunction:get_tlb_pte_w_0_wt

    static function int unsigned get_tlb_pte_x_1_wt();
        check_initialized("get_tlb_pte_x_1_wt");
        return tlb_pte_x_1_wt;
    endfunction:get_tlb_pte_x_1_wt

    static function int unsigned get_tlb_pte_x_0_wt();
        check_initialized("get_tlb_pte_x_0_wt");
        return tlb_pte_x_0_wt;
    endfunction:get_tlb_pte_x_0_wt

    static function int unsigned get_tlb_pte_u_1_wt();
        check_initialized("get_tlb_pte_u_1_wt");
        return tlb_pte_u_1_wt;
    endfunction:get_tlb_pte_u_1_wt

    static function int unsigned get_tlb_pte_u_0_wt();
        check_initialized("get_tlb_pte_u_0_wt");
        return tlb_pte_u_0_wt;
    endfunction:get_tlb_pte_u_0_wt

    static function int unsigned get_tlb_pte_g_1_wt();
        check_initialized("get_tlb_pte_g_1_wt");
        return tlb_pte_g_1_wt;
    endfunction:get_tlb_pte_g_1_wt

    static function int unsigned get_tlb_pte_g_0_wt();
        check_initialized("get_tlb_pte_g_0_wt");
        return tlb_pte_g_0_wt;
    endfunction:get_tlb_pte_g_0_wt

    static function int unsigned get_tlb_pte_a_1_wt();
        check_initialized("get_tlb_pte_a_1_wt");
        return tlb_pte_a_1_wt;
    endfunction:get_tlb_pte_a_1_wt

    static function int unsigned get_tlb_pte_a_0_wt();
        check_initialized("get_tlb_pte_a_0_wt");
        return tlb_pte_a_0_wt;
    endfunction:get_tlb_pte_a_0_wt

    static function int unsigned get_tlb_pte_d_1_wt();
        check_initialized("get_tlb_pte_d_1_wt");
        return tlb_pte_d_1_wt;
    endfunction:get_tlb_pte_d_1_wt

    static function int unsigned get_tlb_pte_d_0_wt();
        check_initialized("get_tlb_pte_d_0_wt");
        return tlb_pte_d_0_wt;
    endfunction:get_tlb_pte_d_0_wt

    static function int unsigned get_tlb_pte_n_1_wt();
        check_initialized("get_tlb_pte_n_1_wt");
        return tlb_pte_n_1_wt;
    endfunction:get_tlb_pte_n_1_wt

    static function int unsigned get_tlb_pte_n_0_wt();
        check_initialized("get_tlb_pte_n_0_wt");
        return tlb_pte_n_0_wt;
    endfunction:get_tlb_pte_n_0_wt

    static function int unsigned get_tlb_pte_v_1_wt();
        check_initialized("get_tlb_pte_v_1_wt");
        return tlb_pte_v_1_wt;
    endfunction:get_tlb_pte_v_1_wt

    static function int unsigned get_tlb_pte_v_0_wt();
        check_initialized("get_tlb_pte_v_0_wt");
        return tlb_pte_v_0_wt;
    endfunction:get_tlb_pte_v_0_wt

    static function int unsigned get_tlb_level_mode();
        check_initialized("get_tlb_level_mode");
        return tlb_level_mode;
    endfunction:get_tlb_level_mode

    static function int unsigned get_tlb_level_fixed_value();
        check_initialized("get_tlb_level_fixed_value");
        return tlb_level_fixed_value;
    endfunction:get_tlb_level_fixed_value

    static function int unsigned get_tlb_level_random_low();
        check_initialized("get_tlb_level_random_low");
        return tlb_level_random_low;
    endfunction:get_tlb_level_random_low

    static function int unsigned get_tlb_level_random_high();
        check_initialized("get_tlb_level_random_high");
        return tlb_level_random_high;
    endfunction:get_tlb_level_random_high

    static function int unsigned get_tlb_pte_mode();
        check_initialized("get_tlb_pte_mode");
        return tlb_pte_mode;
    endfunction:get_tlb_pte_mode

    static function bit [63:0] get_paddr_base();
        check_initialized("get_paddr_base");
        return paddr_base;
    endfunction:get_paddr_base

    static function bit [63:0] get_paddr_range();
        check_initialized("get_paddr_range");
        return paddr_range;
    endfunction:get_paddr_range

    static function bit get_dispatch_issue_seq_en();
        check_initialized("get_dispatch_issue_seq_en");
        return dispatch_issue_seq_en;
    endfunction:get_dispatch_issue_seq_en

    static function bit get_dispatch_issue_nonblocking_en();
        check_initialized("get_dispatch_issue_nonblocking_en");
        return dispatch_issue_nonblocking_en;
    endfunction:get_dispatch_issue_nonblocking_en

    static function int unsigned get_active_seq_no_progress_warn_cycles();
        check_initialized("get_active_seq_no_progress_warn_cycles");
        return active_seq_no_progress_warn_cycles;
    endfunction:get_active_seq_no_progress_warn_cycles

    static function int unsigned get_dispatch_ready_timeout();
        check_initialized("get_dispatch_ready_timeout");
        return dispatch_ready_timeout;
    endfunction:get_dispatch_ready_timeout

    static function bit get_redirect_seq_en();
        check_initialized("get_redirect_seq_en");
        return redirect_seq_en;
    endfunction:get_redirect_seq_en

    static function int unsigned get_redirect_drive_timeout();
        check_initialized("get_redirect_drive_timeout");
        return redirect_drive_timeout;
    endfunction:get_redirect_drive_timeout

    static function int unsigned get_redirect_freeze_timeout();
        check_initialized("get_redirect_freeze_timeout");
        return redirect_freeze_timeout;
    endfunction:get_redirect_freeze_timeout

    static function bit get_sta_real_wb_pass_en();
        check_initialized("get_sta_real_wb_pass_en");
        return sta_real_wb_pass_en;
    endfunction:get_sta_real_wb_pass_en

    static function bit get_std_real_wb_pass_en();
        check_initialized("get_std_real_wb_pass_en");
        return std_real_wb_pass_en;
    endfunction:get_std_real_wb_pass_en

    static function bit get_lsqenq_seq_en();
        check_initialized("get_lsqenq_seq_en");
        return lsqenq_seq_en;
    endfunction:get_lsqenq_seq_en

    static function int unsigned get_lsqenq_ready_timeout();
        check_initialized("get_lsqenq_ready_timeout");
        return lsqenq_ready_timeout;
    endfunction:get_lsqenq_ready_timeout

    static function bit get_lsqcommit_seq_en();
        check_initialized("get_lsqcommit_seq_en");
        return lsqcommit_seq_en;
    endfunction:get_lsqcommit_seq_en

    static function bit get_flushsb_seq_en();
        check_initialized("get_flushsb_seq_en");
        return flushsb_seq_en;
    endfunction:get_flushsb_seq_en

    static function int unsigned get_flushsb_timeout();
        check_initialized("get_flushsb_timeout");
        return flushsb_timeout;
    endfunction:get_flushsb_timeout

    static function int unsigned get_flushsb_request_cycle();
        check_initialized("get_flushsb_request_cycle");
        return flushsb_request_cycle;
    endfunction:get_flushsb_request_cycle

    static function bit get_replay_wait_ptw_en();
        check_initialized("get_replay_wait_ptw_en");
        return replay_wait_ptw_en;
    endfunction:get_replay_wait_ptw_en

    static function int unsigned get_replay_wait_ptw_timeout();
        check_initialized("get_replay_wait_ptw_timeout");
        return replay_wait_ptw_timeout;
    endfunction:get_replay_wait_ptw_timeout

    static function bit get_l2tlb_seq_en();
        check_initialized("get_l2tlb_seq_en");
        return l2tlb_seq_en;
    endfunction:get_l2tlb_seq_en

    static function int unsigned get_l2tlb_min_latency();
        check_initialized("get_l2tlb_min_latency");
        return l2tlb_min_latency;
    endfunction:get_l2tlb_min_latency

    static function int unsigned get_l2tlb_max_latency();
        check_initialized("get_l2tlb_max_latency");
        return l2tlb_max_latency;
    endfunction:get_l2tlb_max_latency

    static function int unsigned get_l2tlb_idle_stop_cycle();
        check_initialized("get_l2tlb_idle_stop_cycle");
        return l2tlb_idle_stop_cycle;
    endfunction:get_l2tlb_idle_stop_cycle

endclass:seq_csr_common

`endif
