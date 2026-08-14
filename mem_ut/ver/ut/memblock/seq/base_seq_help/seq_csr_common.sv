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
    // 中文注释：控制 worker topology 的 plus 快照。load_from_plus() 写入，
    // check_control_worker_topology_mode() 只校验合法范围；测试运行期不得改写它。
    static int          control_worker_topology_mode = 0;
    // 中文注释：AUTO 主表的 CSR/SFence 预约参数。enable 为 0 时 interval 不消费；
    // enable 为 1 时 check_control_interval_config() 要求 min>=1 且 max>=min。
    static bit          csr_control_enable = 1'b0;
    static int          csr_control_min_interval = 1;
    static int          csr_control_max_interval = 1;
    static bit          sfence_control_enable = 1'b0;
    static int          sfence_control_min_interval = 1;
    static int          sfence_control_max_interval = 1;
    static int unsigned enq_per_cycle = 4;
    // 控制get_enq_per_cycle()是否按ZERO/MIDDLE/MAX权重采样[0:物理slot数]目标上限。
    static bit          enq_per_cycle_rand_en = 1'b0;
    // 三个raw权重来自plus；MIDDLE=-1表示AUTO，effective字段保存按物理slot数解析后的非负值。
    // ZERO允许成为唯一非零权重；此时sequence只发送idle，退出仍由既有外部global-stop/UVM phase负责。
    static int unsigned enq_per_cycle_zero_weight = 0;
    static int          enq_per_cycle_middle_weight = -1;
    static int unsigned enq_per_cycle_effective_middle_weight = 0;
    static int unsigned enq_per_cycle_max_weight = 1;
    // LOAD/STA/STD issue运行期使用上限；由plus加载并被编译期物理pipe数clamp。
    // 调度路径不直接把它当本拍发射数，必须通过sample_*_pip_num()按随机开关采样。
    static int unsigned load_pip_num_limit = 3;
    static int unsigned sta_pip_num_limit = 2;
    static int unsigned std_pip_num_limit = 2;
    // pipe随机开关：0表示每拍固定返回*_pip_num_limit；1表示每拍在[1:limit]内随机。
    static bit          load_pip_num_random_en = 1'b0;
    static bit          sta_pip_num_random_en = 1'b0;
    static bit          std_pip_num_random_en = 1'b0;

    static int unsigned op_class_int_load_wt = 8;
    static int unsigned op_class_fp_load_wt = 1;
    static int unsigned op_class_store_wt = 6;
    static int unsigned op_class_prefetch_wt = 1;
    static int unsigned op_class_amo_wt = 0;
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

    // 中文注释：L2TLB payload 配置由 plus 读取、这里快照化并提供给 builder。
    // 下标顺序固定为 R/W/X/U/G/A/D/N/V；S2 数组只有 R/W/X/U/G/A/D/N，没有 V。
    static bit l2tlb_level_weight_en = 1'b0;
    static int unsigned l2tlb_s1_fault_wt[2];
    static int unsigned l2tlb_s2_fault_wt[2];
    static int unsigned l2tlb_s1_level_wt[4];
    static int unsigned l2tlb_s2_level_wt[4];
    static int unsigned l2tlb_s1_pte_1_wt[9];
    static int unsigned l2tlb_s2_pte_1_wt[8];
    static int unsigned l2tlb_s1_pbmt_wt[3];
    static int unsigned l2tlb_s2_pbmt_wt[3];
    static int unsigned l2tlb_s1_pte_mode = 0;
    static int unsigned l2tlb_s2_pte_mode = 0;

    // 中文注释：自动主表 normal transaction 的虚拟地址生成窗口。
    // 由 plus/default cfg 初始化，apply_legal_addr_template() 只读；默认值保持既有 Bare smoke 分布。
    static bit [63:0]   main_vaddr_base = 64'h8000_0000;
    static bit [63:0]   main_vaddr_range = 64'h1000_0000;
    // 中文注释：TLB entry 的物理地址映射窗口，由 tlb_map_builder 只读消费。
    // 该窗口与自动主表虚拟地址窗口可独立配置，不再作为 vaddr 生成约束。
    static bit [63:0]   paddr_base = 64'h8000_0000;
    static bit [63:0]   paddr_range = 64'h1000_0000;
    // 中文注释：DCache/Uncache shared sparse memory 是否启用 PADDR window 严格检查。
    // 设置：plus/default cfg 在 load_from_plus() 写入；读取：memory lifecycle owner 初始化 shared store。
    // 作用：为 1 时两个 memory-facing responder 都限制在 PADDR window，为 0 时按 48-bit 地址懒分配；
    // 不改变 tlb_map_builder 的 PPN 生成窗口。
    static bit          main_mem_ranges_en = 1'b1;
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
    static bit          lsqenq_seq_en = 1'b1;
    static int unsigned lsqenq_ready_timeout = 1000;
    static bit          lsqcommit_seq_en = 1'b1;
    static bit          flushsb_seq_en = 1'b0;
    static int unsigned flushsb_request_cycle = 0;
    static int unsigned flushsb_timeout = 1000;
    static bit          replay_wait_ptw_en = 1'b0;
    static int unsigned replay_wait_ptw_timeout = 1000;
    // 中文注释：DCache/Uncache 各自的四档 response scheduling delay 权重。
    // 设置：plus/default cfg 写入；读取：各 responder scheduler 在一轮返回调度开始时一次采样。
    // 作用：只控制 record 入队后的返回等待，不改变物理 outstanding 上限或 A-channel handshake。
    static int unsigned l2_rsp_delay_zero_wt = 0;
    static int unsigned l2_rsp_delay_small_wt = 1;
    static int unsigned l2_rsp_delay_medium_wt = 0;
    static int unsigned l2_rsp_delay_large_wt = 0;
    static int unsigned uncache_rsp_delay_zero_wt = 0;
    static int unsigned uncache_rsp_delay_small_wt = 1;
    static int unsigned uncache_rsp_delay_medium_wt = 0;
    static int unsigned uncache_rsp_delay_large_wt = 0;
    static bit          l2_rsp_reorder_en = 1'b0;
    static bit          uncache_rsp_reorder_en = 1'b0;
    // 中文注释：DCache/Uncache D-channel 错误注入的百分比权重。
    // 设置：plus 读取后由 validate_and_clamp() 校验为 0..100；读取：response record 创建时一次采样。
    // 作用：只决定该 record 的 denied/corrupt 字段，不改变 A handshake、scheduler、D hold 或 terminal。
    static int unsigned l2_grantdata_denied_wt = 0;
    static int unsigned l2_grantdata_corrupt_wt = 0;
    static int unsigned l2_cbo_ack_denied_wt = 0;
    static int unsigned l2_cbo_ack_corrupt_wt = 0;
    static int unsigned uncache_denied_wt = 0;
    static int unsigned uncache_corrupt_wt = 0;
    // 中文注释：每个 AcquireBlock 产生 hint 的百分比权重。
    // 0 表示关闭，100 表示每次都命中；sequence 只读 getter，不在 getter 内修改状态。
    static int unsigned l2_hint_valid_wt = 0;
    // 中文注释：Probe batch 的公共运行时配置。
    // 设置：plus/default cfg 在 init 时写入；读取：DCache responder 只经 getter 采样。
    // 作用：EN 关闭时不创建随机 Probe；其余权重依次控制每拍 batch 启动、1/MID/LARGE 数量
    // 和 Probe(toB) 概率。它们不改变固定的 16 笔 Probe record 容量。
    static bit          l2_probe_en = 1'b0;
    static int unsigned l2_probe_pre_start_wt = 0;
    static int unsigned l2_probe_count_one_wt = 1;
    static int unsigned l2_probe_count_mid_wt = 0;
    static int unsigned l2_probe_count_large_wt = 0;
    static int unsigned l2_probe_to_b_wt = 0;
    // 中文注释：L2TLB responder运行期开关与队列/调度策略。
    // load_from_plus()设置，validate/apply入口校验并按compile filter容量收敛，sequence只读getter。
    static bit          l2tlb_seq_en = 1'b1;
    static int unsigned l2tlb_max_outstanding = 8;
    static bit          l2tlb_resp_reorder_en = 1'b0;
    // 中文注释：三档最早response间隔及权重；1C档固定为1拍，MID/LONG由plus配置。
    // 三项权重至少一项非零，sequence使用std::randomize/dist选择每个request的due类别。
    static int unsigned l2tlb_resp_mid_latency = 4;
    static int unsigned l2tlb_resp_long_latency = 16;
    static int unsigned l2tlb_resp_1c_wt = 8;
    static int unsigned l2tlb_resp_mid_wt = 3;
    static int unsigned l2tlb_resp_long_wt = 1;
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
        control_worker_topology_mode = plus::MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE;
        csr_control_enable          = plus::MEMBLOCK_CSR_CONTROL_ENABLE;
        csr_control_min_interval    = plus::MEMBLOCK_CSR_CONTROL_MIN_INTERVAL;
        csr_control_max_interval    = plus::MEMBLOCK_CSR_CONTROL_MAX_INTERVAL;
        sfence_control_enable       = plus::MEMBLOCK_SFENCE_CONTROL_ENABLE;
        sfence_control_min_interval = plus::MEMBLOCK_SFENCE_CONTROL_MIN_INTERVAL;
        sfence_control_max_interval = plus::MEMBLOCK_SFENCE_CONTROL_MAX_INTERVAL;
        enq_per_cycle               = get_non_negative_int("MEMBLOCK_ENQ_PER_CYCLE", plus::MEMBLOCK_ENQ_PER_CYCLE);
        enq_per_cycle_rand_en       = plus::MEMBLOCK_ENQ_PER_CYCLE_RAND_EN;
        enq_per_cycle_zero_weight   = get_non_negative_int("MEMBLOCK_ENQ_PER_CYCLE_ZERO_WEIGHT",
                                                            plus::MEMBLOCK_ENQ_PER_CYCLE_ZERO_WEIGHT);
        enq_per_cycle_middle_weight = plus::MEMBLOCK_ENQ_PER_CYCLE_MIDDLE_WEIGHT;
        enq_per_cycle_max_weight    = get_non_negative_int("MEMBLOCK_ENQ_PER_CYCLE_MAX_WEIGHT",
                                                            plus::MEMBLOCK_ENQ_PER_CYCLE_MAX_WEIGHT);
        load_pip_num_limit          = get_non_negative_int("MEMBLOCK_LOAD_PIP_NUM_LIMIT", plus::MEMBLOCK_LOAD_PIP_NUM_LIMIT);
        sta_pip_num_limit           = get_non_negative_int("MEMBLOCK_STA_PIP_NUM_LIMIT", plus::MEMBLOCK_STA_PIP_NUM_LIMIT);
        std_pip_num_limit           = get_non_negative_int("MEMBLOCK_STD_PIP_NUM_LIMIT", plus::MEMBLOCK_STD_PIP_NUM_LIMIT);
        load_pip_num_random_en      = plus::MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN;
        sta_pip_num_random_en       = plus::MEMBLOCK_STA_PIP_NUM_RANDOM_EN;
        std_pip_num_random_en       = plus::MEMBLOCK_STD_PIP_NUM_RANDOM_EN;

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

        l2tlb_level_weight_en = plus::MEMBLOCK_L2TLB_LEVEL_WEIGHT_EN;
        l2tlb_s1_fault_wt[0] = get_non_negative_int("MEMBLOCK_L2TLB_S1_PF_1_WT", plus::MEMBLOCK_L2TLB_S1_PF_1_WT);
        l2tlb_s1_fault_wt[1] = get_non_negative_int("MEMBLOCK_L2TLB_S1_AF_1_WT", plus::MEMBLOCK_L2TLB_S1_AF_1_WT);
        l2tlb_s2_fault_wt[0] = get_non_negative_int("MEMBLOCK_L2TLB_S2_GPF_1_WT", plus::MEMBLOCK_L2TLB_S2_GPF_1_WT);
        l2tlb_s2_fault_wt[1] = get_non_negative_int("MEMBLOCK_L2TLB_S2_GAF_1_WT", plus::MEMBLOCK_L2TLB_S2_GAF_1_WT);
        l2tlb_s1_level_wt[0] = get_non_negative_int("MEMBLOCK_L2TLB_S1_LEVEL_0_WT", plus::MEMBLOCK_L2TLB_S1_LEVEL_0_WT);
        l2tlb_s1_level_wt[1] = get_non_negative_int("MEMBLOCK_L2TLB_S1_LEVEL_1_WT", plus::MEMBLOCK_L2TLB_S1_LEVEL_1_WT);
        l2tlb_s1_level_wt[2] = get_non_negative_int("MEMBLOCK_L2TLB_S1_LEVEL_2_WT", plus::MEMBLOCK_L2TLB_S1_LEVEL_2_WT);
        l2tlb_s1_level_wt[3] = get_non_negative_int("MEMBLOCK_L2TLB_S1_LEVEL_3_WT", plus::MEMBLOCK_L2TLB_S1_LEVEL_3_WT);
        l2tlb_s2_level_wt[0] = get_non_negative_int("MEMBLOCK_L2TLB_S2_LEVEL_0_WT", plus::MEMBLOCK_L2TLB_S2_LEVEL_0_WT);
        l2tlb_s2_level_wt[1] = get_non_negative_int("MEMBLOCK_L2TLB_S2_LEVEL_1_WT", plus::MEMBLOCK_L2TLB_S2_LEVEL_1_WT);
        l2tlb_s2_level_wt[2] = get_non_negative_int("MEMBLOCK_L2TLB_S2_LEVEL_2_WT", plus::MEMBLOCK_L2TLB_S2_LEVEL_2_WT);
        l2tlb_s2_level_wt[3] = get_non_negative_int("MEMBLOCK_L2TLB_S2_LEVEL_3_WT", plus::MEMBLOCK_L2TLB_S2_LEVEL_3_WT);
        l2tlb_s1_pte_mode = get_non_negative_int("MEMBLOCK_L2TLB_S1_PTE_MODE", plus::MEMBLOCK_L2TLB_S1_PTE_MODE);
        l2tlb_s2_pte_mode = get_non_negative_int("MEMBLOCK_L2TLB_S2_PTE_MODE", plus::MEMBLOCK_L2TLB_S2_PTE_MODE);
        l2tlb_s1_pte_1_wt[0] = get_non_negative_int("MEMBLOCK_L2TLB_S1_PTE_R_1_WT", plus::MEMBLOCK_L2TLB_S1_PTE_R_1_WT);
        l2tlb_s1_pte_1_wt[1] = get_non_negative_int("MEMBLOCK_L2TLB_S1_PTE_W_1_WT", plus::MEMBLOCK_L2TLB_S1_PTE_W_1_WT);
        l2tlb_s1_pte_1_wt[2] = get_non_negative_int("MEMBLOCK_L2TLB_S1_PTE_X_1_WT", plus::MEMBLOCK_L2TLB_S1_PTE_X_1_WT);
        l2tlb_s1_pte_1_wt[3] = get_non_negative_int("MEMBLOCK_L2TLB_S1_PTE_U_1_WT", plus::MEMBLOCK_L2TLB_S1_PTE_U_1_WT);
        l2tlb_s1_pte_1_wt[4] = get_non_negative_int("MEMBLOCK_L2TLB_S1_PTE_G_1_WT", plus::MEMBLOCK_L2TLB_S1_PTE_G_1_WT);
        l2tlb_s1_pte_1_wt[5] = get_non_negative_int("MEMBLOCK_L2TLB_S1_PTE_A_1_WT", plus::MEMBLOCK_L2TLB_S1_PTE_A_1_WT);
        l2tlb_s1_pte_1_wt[6] = get_non_negative_int("MEMBLOCK_L2TLB_S1_PTE_D_1_WT", plus::MEMBLOCK_L2TLB_S1_PTE_D_1_WT);
        l2tlb_s1_pte_1_wt[7] = get_non_negative_int("MEMBLOCK_L2TLB_S1_PTE_N_1_WT", plus::MEMBLOCK_L2TLB_S1_PTE_N_1_WT);
        l2tlb_s1_pte_1_wt[8] = get_non_negative_int("MEMBLOCK_L2TLB_S1_PTE_V_1_WT", plus::MEMBLOCK_L2TLB_S1_PTE_V_1_WT);
        for (int i = 0; i < 8; i++) begin
            string field_name;
            case (i)
                0: field_name = "R"; 1: field_name = "W"; 2: field_name = "X"; 3: field_name = "U";
                4: field_name = "G"; 5: field_name = "A"; 6: field_name = "D"; default: field_name = "N";
            endcase
            l2tlb_s2_pte_1_wt[i] = get_non_negative_int(
                $sformatf("MEMBLOCK_L2TLB_S2_PTE_%s_1_WT", field_name),
                (i == 0) ? plus::MEMBLOCK_L2TLB_S2_PTE_R_1_WT :
                (i == 1) ? plus::MEMBLOCK_L2TLB_S2_PTE_W_1_WT :
                (i == 2) ? plus::MEMBLOCK_L2TLB_S2_PTE_X_1_WT :
                (i == 3) ? plus::MEMBLOCK_L2TLB_S2_PTE_U_1_WT :
                (i == 4) ? plus::MEMBLOCK_L2TLB_S2_PTE_G_1_WT :
                (i == 5) ? plus::MEMBLOCK_L2TLB_S2_PTE_A_1_WT :
                (i == 6) ? plus::MEMBLOCK_L2TLB_S2_PTE_D_1_WT : plus::MEMBLOCK_L2TLB_S2_PTE_N_1_WT);
        end
        l2tlb_s1_pbmt_wt[0] = get_non_negative_int("MEMBLOCK_L2TLB_S1_PBMT_0_WT", plus::MEMBLOCK_L2TLB_S1_PBMT_0_WT);
        l2tlb_s1_pbmt_wt[1] = get_non_negative_int("MEMBLOCK_L2TLB_S1_PBMT_1_WT", plus::MEMBLOCK_L2TLB_S1_PBMT_1_WT);
        l2tlb_s1_pbmt_wt[2] = get_non_negative_int("MEMBLOCK_L2TLB_S1_PBMT_2_WT", plus::MEMBLOCK_L2TLB_S1_PBMT_2_WT);
        l2tlb_s2_pbmt_wt[0] = get_non_negative_int("MEMBLOCK_L2TLB_S2_PBMT_0_WT", plus::MEMBLOCK_L2TLB_S2_PBMT_0_WT);
        l2tlb_s2_pbmt_wt[1] = get_non_negative_int("MEMBLOCK_L2TLB_S2_PBMT_1_WT", plus::MEMBLOCK_L2TLB_S2_PBMT_1_WT);
        l2tlb_s2_pbmt_wt[2] = get_non_negative_int("MEMBLOCK_L2TLB_S2_PBMT_2_WT", plus::MEMBLOCK_L2TLB_S2_PBMT_2_WT);

        main_vaddr_base             = plus::MEMBLOCK_MAIN_VADDR_BASE;
        main_vaddr_range            = plus::MEMBLOCK_MAIN_VADDR_RANGE;
        paddr_base                  = plus::MEMBLOCK_PADDR_BASE;
        paddr_range                 = plus::MEMBLOCK_PADDR_RANGE;
        main_mem_ranges_en          = plus::MEMBLOCK_MAIN_MEM_RANGES_EN;
        active_seq_no_progress_warn_cycles = get_non_negative_int("MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES", plus::MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES);
        dispatch_issue_seq_en       = plus::MEMBLOCK_DISPATCH_ISSUE_SEQ_EN;
        dispatch_issue_nonblocking_en = plus::MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN;
        dispatch_ready_timeout      = get_non_negative_int("MEMBLOCK_DISPATCH_READY_TIMEOUT", plus::MEMBLOCK_DISPATCH_READY_TIMEOUT);
        redirect_seq_en             = plus::MEMBLOCK_REDIRECT_SEQ_EN;
        redirect_drive_timeout      = get_non_negative_int("MEMBLOCK_REDIRECT_DRIVE_TIMEOUT", plus::MEMBLOCK_REDIRECT_DRIVE_TIMEOUT);
        redirect_freeze_timeout     = get_non_negative_int("MEMBLOCK_REDIRECT_FREEZE_TIMEOUT", plus::MEMBLOCK_REDIRECT_FREEZE_TIMEOUT);
        sta_real_wb_pass_en         = plus::MEMBLOCK_STA_REAL_WB_PASS_EN;
        lsqenq_seq_en               = plus::MEMBLOCK_LSQENQ_SEQ_EN;
        lsqenq_ready_timeout        = get_non_negative_int("MEMBLOCK_LSQENQ_READY_TIMEOUT", plus::MEMBLOCK_LSQENQ_READY_TIMEOUT);
        lsqcommit_seq_en            = plus::MEMBLOCK_LSQCOMMIT_SEQ_EN;
        flushsb_seq_en              = plus::MEMBLOCK_FLUSHSB_SEQ_EN;
        flushsb_request_cycle       = get_non_negative_int("MEMBLOCK_FLUSHSB_REQUEST_CYCLE", plus::MEMBLOCK_FLUSHSB_REQUEST_CYCLE);
        flushsb_timeout             = get_non_negative_int("MEMBLOCK_FLUSHSB_TIMEOUT", plus::MEMBLOCK_FLUSHSB_TIMEOUT);
        replay_wait_ptw_en          = plus::MEMBLOCK_REPLAY_WAIT_PTW_EN;
        replay_wait_ptw_timeout     = get_non_negative_int("MEMBLOCK_REPLAY_WAIT_PTW_TIMEOUT", plus::MEMBLOCK_REPLAY_WAIT_PTW_TIMEOUT);
        l2_rsp_delay_zero_wt        = get_non_negative_int("MEMBLOCK_L2_RSP_DELAY_ZERO_WT", plus::MEMBLOCK_L2_RSP_DELAY_ZERO_WT);
        l2_rsp_delay_small_wt       = get_non_negative_int("MEMBLOCK_L2_RSP_DELAY_SMALL_WT", plus::MEMBLOCK_L2_RSP_DELAY_SMALL_WT);
        l2_rsp_delay_medium_wt      = get_non_negative_int("MEMBLOCK_L2_RSP_DELAY_MEDIUM_WT", plus::MEMBLOCK_L2_RSP_DELAY_MEDIUM_WT);
        l2_rsp_delay_large_wt       = get_non_negative_int("MEMBLOCK_L2_RSP_DELAY_LARGE_WT", plus::MEMBLOCK_L2_RSP_DELAY_LARGE_WT);
        uncache_rsp_delay_zero_wt   = get_non_negative_int("MEMBLOCK_UNCACHE_RSP_DELAY_ZERO_WT", plus::MEMBLOCK_UNCACHE_RSP_DELAY_ZERO_WT);
        uncache_rsp_delay_small_wt  = get_non_negative_int("MEMBLOCK_UNCACHE_RSP_DELAY_SMALL_WT", plus::MEMBLOCK_UNCACHE_RSP_DELAY_SMALL_WT);
        uncache_rsp_delay_medium_wt = get_non_negative_int("MEMBLOCK_UNCACHE_RSP_DELAY_MEDIUM_WT", plus::MEMBLOCK_UNCACHE_RSP_DELAY_MEDIUM_WT);
        uncache_rsp_delay_large_wt  = get_non_negative_int("MEMBLOCK_UNCACHE_RSP_DELAY_LARGE_WT", plus::MEMBLOCK_UNCACHE_RSP_DELAY_LARGE_WT);
        l2_rsp_reorder_en           = plus::MEMBLOCK_L2_RSP_REORDER_EN;
        uncache_rsp_reorder_en      = plus::MEMBLOCK_UNCACHE_RSP_REORDER_EN;
        l2_grantdata_denied_wt      = get_non_negative_int("MEMBLOCK_L2_GRANTDATA_DENIED_WT", plus::MEMBLOCK_L2_GRANTDATA_DENIED_WT);
        l2_grantdata_corrupt_wt     = get_non_negative_int("MEMBLOCK_L2_GRANTDATA_CORRUPT_WT", plus::MEMBLOCK_L2_GRANTDATA_CORRUPT_WT);
        l2_cbo_ack_denied_wt        = get_non_negative_int("MEMBLOCK_L2_CBO_ACK_DENIED_WT", plus::MEMBLOCK_L2_CBO_ACK_DENIED_WT);
        l2_cbo_ack_corrupt_wt       = get_non_negative_int("MEMBLOCK_L2_CBO_ACK_CORRUPT_WT", plus::MEMBLOCK_L2_CBO_ACK_CORRUPT_WT);
        uncache_denied_wt           = get_non_negative_int("MEMBLOCK_UNCACHE_DENIED_WT", plus::MEMBLOCK_UNCACHE_DENIED_WT);
        uncache_corrupt_wt          = get_non_negative_int("MEMBLOCK_UNCACHE_CORRUPT_WT", plus::MEMBLOCK_UNCACHE_CORRUPT_WT);
        l2_hint_valid_wt            = get_non_negative_int("MEMBLOCK_L2_HINT_VALID_WT", plus::MEMBLOCK_L2_HINT_VALID_WT);
        l2_probe_en                  = plus::MEMBLOCK_L2_PROBE_EN;
        l2_probe_pre_start_wt        = get_non_negative_int("MEMBLOCK_L2_PROBE_PRE_START_WT", plus::MEMBLOCK_L2_PROBE_PRE_START_WT);
        l2_probe_count_one_wt        = get_non_negative_int("MEMBLOCK_L2_PROBE_COUNT_ONE_WT", plus::MEMBLOCK_L2_PROBE_COUNT_ONE_WT);
        l2_probe_count_mid_wt        = get_non_negative_int("MEMBLOCK_L2_PROBE_COUNT_MID_WT", plus::MEMBLOCK_L2_PROBE_COUNT_MID_WT);
        l2_probe_count_large_wt      = get_non_negative_int("MEMBLOCK_L2_PROBE_COUNT_LARGE_WT", plus::MEMBLOCK_L2_PROBE_COUNT_LARGE_WT);
        l2_probe_to_b_wt             = get_non_negative_int("MEMBLOCK_L2_PROBE_TO_B_WT", plus::MEMBLOCK_L2_PROBE_TO_B_WT);
        l2tlb_seq_en                = plus::MEMBLOCK_L2TLB_SEQ_EN;
        l2tlb_max_outstanding       = get_non_negative_int("MEMBLOCK_L2TLB_MAX_OUTSTANDING", plus::MEMBLOCK_L2TLB_MAX_OUTSTANDING);
        l2tlb_resp_reorder_en       = plus::MEMBLOCK_L2TLB_RESP_REORDER_EN;
        l2tlb_resp_mid_latency      = get_non_negative_int("MEMBLOCK_L2TLB_RESP_MID_LATENCY", plus::MEMBLOCK_L2TLB_RESP_MID_LATENCY);
        l2tlb_resp_long_latency     = get_non_negative_int("MEMBLOCK_L2TLB_RESP_LONG_LATENCY", plus::MEMBLOCK_L2TLB_RESP_LONG_LATENCY);
        l2tlb_resp_1c_wt            = get_non_negative_int("MEMBLOCK_L2TLB_RESP_1C_WT", plus::MEMBLOCK_L2TLB_RESP_1C_WT);
        l2tlb_resp_mid_wt           = get_non_negative_int("MEMBLOCK_L2TLB_RESP_MID_WT", plus::MEMBLOCK_L2TLB_RESP_MID_WT);
        l2tlb_resp_long_wt          = get_non_negative_int("MEMBLOCK_L2TLB_RESP_LONG_WT", plus::MEMBLOCK_L2TLB_RESP_LONG_WT);
        l2tlb_idle_stop_cycle       = get_non_negative_int("MEMBLOCK_L2TLB_IDLE_STOP_CYCLE", plus::MEMBLOCK_L2TLB_IDLE_STOP_CYCLE);
    endfunction:load_from_plus

    // 中文注释：在 responder 开放 ready 前校验本专项的静态 payload 配置。
    // 该函数只诊断配置，不选择 level/PBMT，也不修改 entry、pending 或 lifecycle 状态。
    static function void check_l2tlb_payload_weight_cfg();
        for (int i = 0; i < 4; i++) begin
            if (l2tlb_s1_level_wt[i] > 100 || l2tlb_s2_level_wt[i] > 100) begin
                if (l2tlb_level_weight_en) `uvm_fatal("SEQ_CSR_CFG", "enabled L2TLB level weight must be in [0:100]")
            end
        end
        for (int i = 0; i < 2; i++) begin
            if (l2tlb_s1_fault_wt[i] > 100 || l2tlb_s2_fault_wt[i] > 100)
                `uvm_fatal("SEQ_CSR_CFG", "L2TLB fault weight must be in [0:100]");
        end
        for (int i = 0; i < 9; i++) begin
            if (l2tlb_s1_pte_1_wt[i] > 100) `uvm_fatal("SEQ_CSR_CFG", "S1 PTE weight must be in [0:100]");
        end
        for (int i = 0; i < 8; i++) begin
            if (l2tlb_s2_pte_1_wt[i] > 100) `uvm_fatal("SEQ_CSR_CFG", "S2 PTE weight must be in [0:100]");
        end
        for (int i = 0; i < 3; i++) begin
            if (l2tlb_s1_pbmt_wt[i] > 100 || l2tlb_s2_pbmt_wt[i] > 100)
                `uvm_fatal("SEQ_CSR_CFG", "L2TLB PBMT weight must be in [0:100]");
        end
        if (l2tlb_s1_pbmt_wt[0] + l2tlb_s1_pbmt_wt[1] + l2tlb_s1_pbmt_wt[2] == 0 ||
            l2tlb_s2_pbmt_wt[0] + l2tlb_s2_pbmt_wt[1] + l2tlb_s2_pbmt_wt[2] == 0)
            `uvm_fatal("SEQ_CSR_CFG", "S1/S2 PBMT weights must not be all zero");
        if (l2tlb_s1_pte_mode > 2 || l2tlb_s2_pte_mode > 2)
            `uvm_fatal("SEQ_CSR_CFG", "S1/S2 PTE mode must be LEGAL, MIXED or EXCEPTION_BIASED");
        if (l2tlb_level_weight_en && main_mem_ranges_en)
            `uvm_fatal("SEQ_CSR_CFG", "LEVEL_WEIGHT_EN requires MAIN_MEM_RANGES_EN=0");
    endfunction:check_l2tlb_payload_weight_cfg

    // 抽象职责：只校验控制 worker topology plus 的枚举范围；不冻结、不改写运行期状态。
    static function void check_control_worker_topology_mode();
        if (control_worker_topology_mode < 0 || control_worker_topology_mode > 3) begin
            `uvm_fatal("SEQ_CSR_CFG",
                       $sformatf("MEMBLOCK_CONTROL_WORKER_TOPOLOGY_MODE=%0d must be within [0:3]",
                                 control_worker_topology_mode))
        end
    endfunction:check_control_worker_topology_mode

    // 抽象职责：只校验已启用控制标记的预约间隔；enable=0 时该组 interval 不参与语义。
    static function void check_control_interval_config(input string control_name,
                                                       input bit enabled,
                                                       input int min_interval,
                                                       input int max_interval);
        if (!enabled) begin
            return;
        end
        if (min_interval < 1 || max_interval < min_interval) begin
            `uvm_fatal("SEQ_CSR_CFG",
                       $sformatf("%0s control interval is invalid: min=%0d max=%0d; require min>=1 and max>=min",
                                 control_name, min_interval, max_interval))
        end
    endfunction:check_control_interval_config

    static function void validate_and_clamp();
        bit [63:0] main_vaddr_upper;

        check_compile_param_consistency();
        check_control_worker_topology_mode();
        check_control_interval_config("CSR",
                                      csr_control_enable,
                                      csr_control_min_interval,
                                      csr_control_max_interval);
        check_control_interval_config("SFence",
                                      sfence_control_enable,
                                      sfence_control_min_interval,
                                      sfence_control_max_interval);
        fatal_if_zero("main_trans_num", main_trans_num);
        fatal_if_zero("main_vaddr_range", main_vaddr_range);
        fatal_if_zero("paddr_range", paddr_range);
        fatal_if_zero("l2tlb_max_outstanding", l2tlb_max_outstanding);
        apply_runtime_resource_limits();

        if (op_class_amo_wt != 0) begin
            `uvm_fatal("SEQ_CSR_CFG", "MEMBLOCK_OP_CLASS_AMO_WT must be 0 until the V2 scalar atomic flow is complete")
        end
        if (op_class_cbo_wt != 0) begin
            `uvm_fatal("SEQ_CSR_CFG", "MEMBLOCK_OP_CLASS_CBO_WT must be 0 until the V2 scalar CBO flow is complete")
        end

        main_vaddr_upper = main_vaddr_base + main_vaddr_range - 1;
        if (main_vaddr_upper < main_vaddr_base) begin
            `uvm_fatal("SEQ_CSR_CFG", "main_vaddr_base + main_vaddr_range - 1 overflows")
        end
        if (main_vaddr_base[63:38] != '0 || main_vaddr_upper[63:38] != '0) begin
            `uvm_fatal("SEQ_CSR_CFG",
                       $sformatf("main vaddr window must stay in Sv39 positive-canonical space: base=0x%0h upper=0x%0h",
                                 main_vaddr_base,
                                 main_vaddr_upper))
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
        if (MEMBLOCK_DUT_LSQ_ENQ_HAS_ACCEPT_RESP &&
            lsqenq_seq_en && lsqenq_ready_timeout == 0) begin
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
        fatal_if_all_zero4("DCache L2 response delay weights",
                           l2_rsp_delay_zero_wt,
                           l2_rsp_delay_small_wt,
                           l2_rsp_delay_medium_wt,
                           l2_rsp_delay_large_wt);
        fatal_if_all_zero4("Uncache response delay weights",
                           uncache_rsp_delay_zero_wt,
                           uncache_rsp_delay_small_wt,
                           uncache_rsp_delay_medium_wt,
                           uncache_rsp_delay_large_wt);
        if (l2_grantdata_denied_wt > 100 || l2_grantdata_corrupt_wt > 100 ||
            l2_cbo_ack_denied_wt > 100 || l2_cbo_ack_corrupt_wt > 100 ||
            uncache_denied_wt > 100 || uncache_corrupt_wt > 100) begin
            `uvm_fatal("SEQ_CSR_CFG",
                       $sformatf("D response error weights must be within [0:100]: grantData=%0d/%0d cboAck=%0d/%0d uncache=%0d/%0d",
                                 l2_grantdata_denied_wt,
                                 l2_grantdata_corrupt_wt,
                                 l2_cbo_ack_denied_wt,
                                 l2_cbo_ack_corrupt_wt,
                                 uncache_denied_wt,
                                 uncache_corrupt_wt))
        end
        if (l2_hint_valid_wt > 100) begin
            `uvm_fatal("SEQ_CSR_CFG",
                       $sformatf("MEMBLOCK_L2_HINT_VALID_WT=%0d must be within [0:100]", l2_hint_valid_wt))
        end
        if (l2_probe_pre_start_wt > 10000 || l2_probe_to_b_wt > 10000) begin
            `uvm_fatal("SEQ_CSR_CFG",
                       $sformatf("L2 Probe 10000-scale weights exceed range: pre_start=%0d toB=%0d",
                                 l2_probe_pre_start_wt,
                                 l2_probe_to_b_wt))
        end
        fatal_if_all_zero3("L2 Probe batch count weights",
                           l2_probe_count_one_wt,
                           l2_probe_count_mid_wt,
                           l2_probe_count_large_wt);
        if (l2tlb_resp_mid_latency <= 1) begin
            `uvm_fatal("SEQ_CSR_CFG", "MEMBLOCK_L2TLB_RESP_MID_LATENCY must be greater than 1")
        end
        if (l2tlb_resp_long_latency <= l2tlb_resp_mid_latency) begin
            `uvm_fatal("SEQ_CSR_CFG", "MEMBLOCK_L2TLB_RESP_LONG_LATENCY must exceed MID_LATENCY")
        end
        fatal_if_all_zero3("L2TLB response latency weights",
                           l2tlb_resp_1c_wt,
                           l2tlb_resp_mid_wt,
                           l2tlb_resp_long_wt);
        if (l2tlb_seq_en && l2tlb_idle_stop_cycle == 0) begin
            `uvm_warning("SEQ_CSR_CFG", "l2tlb_idle_stop_cycle=0 while L2TLB sequence is enabled, clamp to 1")
            l2tlb_idle_stop_cycle = 1;
        end
        fatal_if_zero("pc_stride", pc_stride);
        fatal_if_zero("pdest_range", pdest_range);
        clamp_int("ftq_idx_base",
                  ftq_idx_base,
                  0,
                  int'((64'd1 << MEMBLOCK_FTQ_PTR_VALUE_W) - 1));
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

        check_l2tlb_payload_weight_cfg();
    endfunction:validate_and_clamp

    // 中文注释：只检查编译期结构常量的可编码关系，不修改任何runtime参数。
    static function void check_compile_param_consistency();
        int unsigned fu_bits[$];

        if (MEMBLOCK_ROB_VALUE_W == 0 || MEMBLOCK_LQ_VALUE_W == 0 || MEMBLOCK_SQ_VALUE_W == 0 ||
            MEMBLOCK_INTERNAL_FUTYPE_W == 0 || MEMBLOCK_DUT_FUTYPE_W == 0 ||
            MEMBLOCK_FTQ_PTR_VALUE_W == 0 || MEMBLOCK_FTQ_OFFSET_W == 0 ||
            MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM == 0 || MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH == 0 ||
            MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH == 0 || MEMBLOCK_DUT_LOAD_PIPE_NUM == 0 ||
            MEMBLOCK_DUT_STA_PIPE_NUM == 0 || MEMBLOCK_DUT_STD_PIPE_NUM == 0 ||
            MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM == 0 || MEMBLOCK_DUT_MAX_UOP_SIZE == 0 ||
            MEMBLOCK_DUT_UOP_IDX_W == 0 || MEMBLOCK_DUT_VLEN == 0 ||
            MEMBLOCK_DUT_MAX_LS_ELEM == 0 || MEMBLOCK_DUT_NUM_LS_ELEM_W == 0 ||
            MEMBLOCK_DUT_ENSBUFFER_WIDTH == 0 || MEMBLOCK_SQ_DEQ_COUNT_W == 0 ||
            MEMBLOCK_LQ_CANCEL_COUNT_W == 0 || MEMBLOCK_SQ_CANCEL_COUNT_W == 0 ||
            MEMBLOCK_DUT_L2TLB_DFILTER_SIZE == 0 ||
            MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES == 0) begin
            `uvm_fatal("SEQ_COMPILE_CFG", "compile-time width/count values must be non-zero")
        end
        if (MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH > MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM ||
            MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH > MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM) begin
            `uvm_fatal("SEQ_COMPILE_CFG", "LSQ load/store enqueue width exceeds total slot count")
        end
        if (MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM != 6 ||
            MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH != 6 ||
            MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH != 4) begin
            `uvm_fatal("SEQ_COMPILE_CFG",
                       $sformatf("current V2 LSQ field expansion requires slot/load/store tuple 6/6/4, got %0d/%0d/%0d",
                                 MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM,
                                 MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH,
                                 MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH))
        end
        if (MEMBLOCK_DUT_UOP_IDX_W != $clog2(MEMBLOCK_DUT_MAX_UOP_SIZE + 1)) begin
            `uvm_fatal("SEQ_COMPILE_CFG", "UOP_IDX_W is not derived from MAX_UOP_SIZE")
        end
        if (MEMBLOCK_SQ_DEQ_COUNT_W != $clog2(MEMBLOCK_DUT_ENSBUFFER_WIDTH + 1) ||
            MEMBLOCK_LQ_CANCEL_COUNT_W != $clog2(MEMBLOCK_LQ_SIZE + 1) ||
            MEMBLOCK_SQ_CANCEL_COUNT_W != $clog2(MEMBLOCK_SQ_SIZE + 1)) begin
            `uvm_fatal("SEQ_COMPILE_CFG", "SQ deq/cancel widths are not derived from their compile-time owners")
        end
        if (MEMBLOCK_DUT_REDIRECT_TO_LSQ_LATENCY == 0 ||
            MEMBLOCK_DUT_CANCEL_OUTPUT_LATENCY == 0 ||
            MEMBLOCK_TB_CANCEL_MONITOR_SAMPLE_OFFSET != 1 ||
            MEMBLOCK_CANCEL_SNAPSHOT_OBSERVE_LATENCY !=
                MEMBLOCK_DUT_CANCEL_OUTPUT_LATENCY +
                MEMBLOCK_TB_CANCEL_MONITOR_SAMPLE_OFFSET ||
            MEMBLOCK_CANCEL_RECORD_MAX_DEPTH !=
                MEMBLOCK_CANCEL_SNAPSHOT_OBSERVE_LATENCY + 2 ||
            MEMBLOCK_CANCEL_SNAPSHOT_QUEUE_MAX_DEPTH !=
                2 * MEMBLOCK_CANCEL_SNAPSHOT_OBSERVE_LATENCY + 8) begin
            `uvm_fatal("SEQ_COMPILE_CFG", "cancel observe latency is not derived from DUT update and monitor offset")
        end
        if ((MEMBLOCK_DUT_VLEN % 8) != 0 ||
            MEMBLOCK_DUT_MAX_LS_ELEM != (MEMBLOCK_DUT_VLEN / 8) ||
            MEMBLOCK_DUT_NUM_LS_ELEM_W != ($clog2(MEMBLOCK_DUT_MAX_LS_ELEM) + 1)) begin
            `uvm_fatal("SEQ_COMPILE_CFG", "numLsElem constants are not derived from VLEN")
        end
        if (MEMBLOCK_DUT_FUTYPE_W > MEMBLOCK_INTERNAL_FUTYPE_W) begin
            `uvm_fatal("SEQ_COMPILE_CFG",
                       $sformatf("DUT FuType width=%0d exceeds internal width=%0d",
                                 MEMBLOCK_DUT_FUTYPE_W,
                                 MEMBLOCK_INTERNAL_FUTYPE_W))
        end
        if (MEMBLOCK_DUT_STA_PORT_BASE != MEMBLOCK_DUT_LOAD_PORT_BASE + MEMBLOCK_DUT_LOAD_PIPE_NUM ||
            MEMBLOCK_DUT_STD_PORT_BASE != MEMBLOCK_DUT_STA_PORT_BASE + MEMBLOCK_DUT_STA_PIPE_NUM ||
            MEMBLOCK_DUT_SCALAR_ISSUE_PORT_NUM != MEMBLOCK_DUT_STD_PORT_BASE + MEMBLOCK_DUT_STD_PIPE_NUM ||
            MEMBLOCK_DUT_SCALAR_ISSUE_MASK_W != MEMBLOCK_DUT_SCALAR_ISSUE_PORT_NUM ||
            MEMBLOCK_DUT_LOAD_PORT_BASE >= MEMBLOCK_DUT_SCALAR_ISSUE_PORT_NUM) begin
            `uvm_fatal("SEQ_COMPILE_CFG", "scalar issue PORT_BASE/PORT_NUM/MASK_W constants are inconsistent")
        end
        if (MEMBLOCK_DUT_ISSUE_PORT_STYLE_SPLIT &&
            (MEMBLOCK_DUT_LOAD_PIPE_NUM > 3 ||
             MEMBLOCK_DUT_STA_PIPE_NUM > 2 ||
             MEMBLOCK_DUT_STD_PIPE_NUM > 2)) begin
            `uvm_fatal("SEQ_COMPILE_CFG",
                       $sformatf("split issue physical expansion supports at most LDA/STA/STD=3/2/2, got %0d/%0d/%0d",
                                 MEMBLOCK_DUT_LOAD_PIPE_NUM,
                                 MEMBLOCK_DUT_STA_PIPE_NUM,
                                 MEMBLOCK_DUT_STD_PIPE_NUM))
        end
        for (int unsigned port_idx = MEMBLOCK_DUT_LOAD_PORT_BASE;
             port_idx < MEMBLOCK_DUT_SCALAR_ISSUE_PORT_NUM;
             port_idx++) begin
            int unsigned membership;
            membership = 0;
            if (port_idx >= MEMBLOCK_DUT_LOAD_PORT_BASE && port_idx < MEMBLOCK_DUT_STA_PORT_BASE) membership++;
            if (port_idx >= MEMBLOCK_DUT_STA_PORT_BASE && port_idx < MEMBLOCK_DUT_STD_PORT_BASE) membership++;
            if (port_idx >= MEMBLOCK_DUT_STD_PORT_BASE && port_idx < MEMBLOCK_DUT_SCALAR_ISSUE_PORT_NUM) membership++;
            if (membership != 1) begin
                `uvm_fatal("SEQ_COMPILE_CFG", $sformatf("scalar issue port=%0d has membership=%0d", port_idx, membership))
            end
        end

        fu_bits.push_back(MEMBLOCK_DUT_FUTYPE_LDU_BIT);
        fu_bits.push_back(MEMBLOCK_DUT_FUTYPE_STU_BIT);
        fu_bits.push_back(MEMBLOCK_DUT_FUTYPE_MOU_BIT);
        fu_bits.push_back(MEMBLOCK_DUT_FUTYPE_VLDU_BIT);
        fu_bits.push_back(MEMBLOCK_DUT_FUTYPE_VSTU_BIT);
        fu_bits.push_back(MEMBLOCK_DUT_FUTYPE_VSEGLDU_BIT);
        fu_bits.push_back(MEMBLOCK_DUT_FUTYPE_VSEGSTU_BIT);
        foreach (fu_bits[i]) begin
            if (fu_bits[i] >= MEMBLOCK_INTERNAL_FUTYPE_W) begin
                `uvm_fatal("SEQ_COMPILE_CFG", $sformatf("FUTYPE bit=%0d exceeds internal width=%0d", fu_bits[i], MEMBLOCK_INTERNAL_FUTYPE_W))
            end
            if (fu_bits[i] >= MEMBLOCK_DUT_FUTYPE_W) begin
                `uvm_fatal("SEQ_COMPILE_CFG", $sformatf("FUTYPE bit=%0d exceeds DUT width=%0d", fu_bits[i], MEMBLOCK_DUT_FUTYPE_W))
            end
            for (int unsigned j = 0; j < i; j++) begin
                if (fu_bits[i] == fu_bits[j]) begin
                    `uvm_fatal("SEQ_COMPILE_CFG", $sformatf("duplicate FUTYPE bit=%0d", fu_bits[i]))
                end
            end
        end
    endfunction:check_compile_param_consistency

    // 中文注释：runtime资源参数只控制本次testcase使用量，物理上限统一取编译期DUT常量。
    // 本函数是enqueue/issue资源限制的唯一修改点；interface尺寸和driver物理循环不读取plus。
    static function void apply_runtime_resource_limits();
        longint unsigned enq_weight_sum;

        if (MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM == 0 ||
            MEMBLOCK_DUT_LOAD_PIPE_NUM == 0 ||
            MEMBLOCK_DUT_STA_PIPE_NUM == 0 ||
            MEMBLOCK_DUT_STD_PIPE_NUM == 0 ||
            MEMBLOCK_DUT_L2TLB_DFILTER_SIZE == 0) begin
            `uvm_fatal("SEQ_CSR_CFG", "compile-time LSQ enqueue/issue resource counts must be non-zero")
        end

        if (enq_per_cycle == 0 || enq_per_cycle > MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM) begin
            `uvm_fatal("SEQ_CSR_CFG",
                       $sformatf("MEMBLOCK_ENQ_PER_CYCLE=%0d must be in [1:%0d]",
                                 enq_per_cycle,
                                 MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM))
        end
        if (enq_per_cycle_middle_weight < -1) begin
            `uvm_fatal("SEQ_CSR_CFG",
                       $sformatf("MEMBLOCK_ENQ_PER_CYCLE_MIDDLE_WEIGHT=%0d must be -1(AUTO) or non-negative",
                                 enq_per_cycle_middle_weight))
        end
        if (enq_per_cycle_middle_weight == -1) begin
            enq_per_cycle_effective_middle_weight = MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM - 1;
        end else begin
            enq_per_cycle_effective_middle_weight = enq_per_cycle_middle_weight;
        end
        if (enq_per_cycle_rand_en) begin
            if (MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM <= 1 &&
                enq_per_cycle_effective_middle_weight != 0) begin
                `uvm_fatal("SEQ_CSR_CFG", "MIDDLE enqueue weight requires at least two physical slots")
            end
            enq_weight_sum = enq_per_cycle_zero_weight;
            enq_weight_sum += enq_per_cycle_effective_middle_weight;
            enq_weight_sum += enq_per_cycle_max_weight;
            if (enq_weight_sum == 0) begin
                `uvm_fatal("SEQ_CSR_CFG", "LSQ enqueue ZERO/MIDDLE/MAX weights must not all be zero")
            end
        end

        clamp_int("load_pip_num_limit", load_pip_num_limit, 1, MEMBLOCK_DUT_LOAD_PIPE_NUM);
        clamp_int("sta_pip_num_limit", sta_pip_num_limit, 1, MEMBLOCK_DUT_STA_PIPE_NUM);
        clamp_int("std_pip_num_limit", std_pip_num_limit, 1, MEMBLOCK_DUT_STD_PIPE_NUM);
        if (l2tlb_max_outstanding > MEMBLOCK_DUT_L2TLB_DFILTER_SIZE) begin
            `uvm_warning("SEQ_CSR_CFG",
                         $sformatf("MEMBLOCK_L2TLB_MAX_OUTSTANDING=%0d exceeds DUT DTLB filter size=%0d, clamp to compile limit",
                                   l2tlb_max_outstanding,
                                   MEMBLOCK_DUT_L2TLB_DFILTER_SIZE))
            l2tlb_max_outstanding = MEMBLOCK_DUT_L2TLB_DFILTER_SIZE;
        end
    endfunction:apply_runtime_resource_limits

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

    static function int get_control_worker_topology_mode();
        check_initialized("get_control_worker_topology_mode");
        return control_worker_topology_mode;
    endfunction:get_control_worker_topology_mode

    static function bit get_csr_control_enable();
        check_initialized("get_csr_control_enable");
        return csr_control_enable;
    endfunction:get_csr_control_enable

    static function int get_csr_control_min_interval();
        check_initialized("get_csr_control_min_interval");
        return csr_control_min_interval;
    endfunction:get_csr_control_min_interval

    static function int get_csr_control_max_interval();
        check_initialized("get_csr_control_max_interval");
        return csr_control_max_interval;
    endfunction:get_csr_control_max_interval

    static function bit get_sfence_control_enable();
        check_initialized("get_sfence_control_enable");
        return sfence_control_enable;
    endfunction:get_sfence_control_enable

    static function int get_sfence_control_min_interval();
        check_initialized("get_sfence_control_min_interval");
        return sfence_control_min_interval;
    endfunction:get_sfence_control_min_interval

    static function int get_sfence_control_max_interval();
        check_initialized("get_sfence_control_max_interval");
        return sfence_control_max_interval;
    endfunction:get_sfence_control_max_interval

    static function bit get_use_manual_main_table();
        check_initialized("get_use_manual_main_table");
        return use_manual_main_table;
    endfunction:get_use_manual_main_table

    static function int unsigned get_enq_per_cycle();
        int unsigned sample_class;
        int unsigned middle_value;
        int unsigned zero_weight;
        int unsigned middle_weight;
        int unsigned max_weight;

        check_initialized("get_enq_per_cycle");
        if (!enq_per_cycle_rand_en) begin
            return enq_per_cycle;
        end

        zero_weight = enq_per_cycle_zero_weight;
        middle_weight = enq_per_cycle_effective_middle_weight;
        max_weight = enq_per_cycle_max_weight;
        if (!std::randomize(sample_class) with {
                sample_class dist {
                    0 := zero_weight,
                    1 := middle_weight,
                    2 := max_weight
                };
            }) begin
            `uvm_fatal("SEQ_CSR_CFG", "failed to randomize LSQ enqueue ZERO/MIDDLE/MAX class")
        end
        case (sample_class)
            0: return 0;
            1: begin
                if (!std::randomize(middle_value) with {
                        middle_value inside {[1:MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM-1]};
                    }) begin
                    `uvm_fatal("SEQ_CSR_CFG", "failed to randomize LSQ enqueue MIDDLE value")
                end
                return middle_value;
            end
            2: return MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM;
            default: begin
                `uvm_fatal("SEQ_CSR_CFG", $sformatf("invalid LSQ enqueue random class=%0d", sample_class))
            end
        endcase
        return 0;
    endfunction:get_enq_per_cycle

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

    static function bit get_l2tlb_level_weight_en(); check_initialized("get_l2tlb_level_weight_en"); return l2tlb_level_weight_en; endfunction
    static function int unsigned get_l2tlb_fault_1_wt(input bit s1, input int unsigned fault_idx);
        check_initialized("get_l2tlb_fault_1_wt");
        if (fault_idx > 1) `uvm_fatal("SEQ_CSR_CFG", "invalid L2TLB fault index");
        return s1 ? l2tlb_s1_fault_wt[fault_idx] : l2tlb_s2_fault_wt[fault_idx];
    endfunction
    static function int unsigned get_l2tlb_level_wt(input bit s1, input int unsigned level_idx);
        check_initialized("get_l2tlb_level_wt");
        if (level_idx > 3) `uvm_fatal("SEQ_CSR_CFG", "invalid L2TLB level index");
        return s1 ? l2tlb_s1_level_wt[level_idx] : l2tlb_s2_level_wt[level_idx];
    endfunction
    static function int unsigned get_l2tlb_pte_1_wt(input bit s1, input int unsigned field_idx);
        check_initialized("get_l2tlb_pte_1_wt");
        if (field_idx > (s1 ? 8 : 7)) `uvm_fatal("SEQ_CSR_CFG", "invalid L2TLB PTE field index");
        return s1 ? l2tlb_s1_pte_1_wt[field_idx] : l2tlb_s2_pte_1_wt[field_idx];
    endfunction
    static function int unsigned get_l2tlb_pbmt_wt(input bit s1, input int unsigned pbmt_idx);
        check_initialized("get_l2tlb_pbmt_wt");
        if (pbmt_idx > 2) `uvm_fatal("SEQ_CSR_CFG", "invalid L2TLB PBMT index");
        return s1 ? l2tlb_s1_pbmt_wt[pbmt_idx] : l2tlb_s2_pbmt_wt[pbmt_idx];
    endfunction
    static function int unsigned get_l2tlb_pte_mode(input bit s1);
        check_initialized("get_l2tlb_pte_mode");
        return s1 ? l2tlb_s1_pte_mode : l2tlb_s2_pte_mode;
    endfunction

    static function bit [63:0] get_main_vaddr_base();
        check_initialized("get_main_vaddr_base");
        return main_vaddr_base;
    endfunction:get_main_vaddr_base

    static function bit [63:0] get_main_vaddr_range();
        check_initialized("get_main_vaddr_range");
        return main_vaddr_range;
    endfunction:get_main_vaddr_range

    static function bit [63:0] get_paddr_base();
        check_initialized("get_paddr_base");
        return paddr_base;
    endfunction:get_paddr_base

    static function bit [63:0] get_paddr_range();
        check_initialized("get_paddr_range");
        return paddr_range;
    endfunction:get_paddr_range

    static function bit get_main_mem_ranges_en();
        check_initialized("get_main_mem_ranges_en");
        return main_mem_ranges_en;
    endfunction:get_main_mem_ranges_en

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

    static function int unsigned get_l2_rsp_delay_small_wt();
        check_initialized("get_l2_rsp_delay_small_wt");
        return l2_rsp_delay_small_wt;
    endfunction:get_l2_rsp_delay_small_wt

    static function int unsigned get_l2_rsp_delay_zero_wt();
        check_initialized("get_l2_rsp_delay_zero_wt");
        return l2_rsp_delay_zero_wt;
    endfunction:get_l2_rsp_delay_zero_wt

    static function int unsigned get_l2_rsp_delay_medium_wt();
        check_initialized("get_l2_rsp_delay_medium_wt");
        return l2_rsp_delay_medium_wt;
    endfunction:get_l2_rsp_delay_medium_wt

    static function int unsigned get_l2_rsp_delay_large_wt();
        check_initialized("get_l2_rsp_delay_large_wt");
        return l2_rsp_delay_large_wt;
    endfunction:get_l2_rsp_delay_large_wt

    static function int unsigned get_uncache_rsp_delay_zero_wt();
        check_initialized("get_uncache_rsp_delay_zero_wt");
        return uncache_rsp_delay_zero_wt;
    endfunction:get_uncache_rsp_delay_zero_wt

    static function int unsigned get_uncache_rsp_delay_small_wt();
        check_initialized("get_uncache_rsp_delay_small_wt");
        return uncache_rsp_delay_small_wt;
    endfunction:get_uncache_rsp_delay_small_wt

    static function int unsigned get_uncache_rsp_delay_medium_wt();
        check_initialized("get_uncache_rsp_delay_medium_wt");
        return uncache_rsp_delay_medium_wt;
    endfunction:get_uncache_rsp_delay_medium_wt

    static function int unsigned get_uncache_rsp_delay_large_wt();
        check_initialized("get_uncache_rsp_delay_large_wt");
        return uncache_rsp_delay_large_wt;
    endfunction:get_uncache_rsp_delay_large_wt

    static function bit get_l2_rsp_reorder_en();
        check_initialized("get_l2_rsp_reorder_en");
        return l2_rsp_reorder_en;
    endfunction:get_l2_rsp_reorder_en

    static function bit get_uncache_rsp_reorder_en();
        check_initialized("get_uncache_rsp_reorder_en");
        return uncache_rsp_reorder_en;
    endfunction:get_uncache_rsp_reorder_en

    static function int unsigned get_l2_grantdata_denied_wt();
        check_initialized("get_l2_grantdata_denied_wt");
        return l2_grantdata_denied_wt;
    endfunction:get_l2_grantdata_denied_wt

    static function int unsigned get_l2_grantdata_corrupt_wt();
        check_initialized("get_l2_grantdata_corrupt_wt");
        return l2_grantdata_corrupt_wt;
    endfunction:get_l2_grantdata_corrupt_wt

    static function int unsigned get_l2_cbo_ack_denied_wt();
        check_initialized("get_l2_cbo_ack_denied_wt");
        return l2_cbo_ack_denied_wt;
    endfunction:get_l2_cbo_ack_denied_wt

    static function int unsigned get_l2_cbo_ack_corrupt_wt();
        check_initialized("get_l2_cbo_ack_corrupt_wt");
        return l2_cbo_ack_corrupt_wt;
    endfunction:get_l2_cbo_ack_corrupt_wt

    static function int unsigned get_uncache_denied_wt();
        check_initialized("get_uncache_denied_wt");
        return uncache_denied_wt;
    endfunction:get_uncache_denied_wt

    static function int unsigned get_uncache_corrupt_wt();
        check_initialized("get_uncache_corrupt_wt");
        return uncache_corrupt_wt;
    endfunction:get_uncache_corrupt_wt

    static function int unsigned get_l2_hint_valid_wt();
        check_initialized("get_l2_hint_valid_wt");
        return l2_hint_valid_wt;
    endfunction:get_l2_hint_valid_wt

    static function bit get_l2_probe_en();
        check_initialized("get_l2_probe_en");
        return l2_probe_en;
    endfunction:get_l2_probe_en

    static function int unsigned get_l2_probe_pre_start_wt();
        check_initialized("get_l2_probe_pre_start_wt");
        return l2_probe_pre_start_wt;
    endfunction:get_l2_probe_pre_start_wt

    static function int unsigned get_l2_probe_count_one_wt();
        check_initialized("get_l2_probe_count_one_wt");
        return l2_probe_count_one_wt;
    endfunction:get_l2_probe_count_one_wt

    static function int unsigned get_l2_probe_count_mid_wt();
        check_initialized("get_l2_probe_count_mid_wt");
        return l2_probe_count_mid_wt;
    endfunction:get_l2_probe_count_mid_wt

    static function int unsigned get_l2_probe_count_large_wt();
        check_initialized("get_l2_probe_count_large_wt");
        return l2_probe_count_large_wt;
    endfunction:get_l2_probe_count_large_wt

    static function int unsigned get_l2_probe_to_b_wt();
        check_initialized("get_l2_probe_to_b_wt");
        return l2_probe_to_b_wt;
    endfunction:get_l2_probe_to_b_wt

    static function bit get_l2tlb_seq_en();
        check_initialized("get_l2tlb_seq_en");
        return l2tlb_seq_en;
    endfunction:get_l2tlb_seq_en

    static function int unsigned get_l2tlb_max_outstanding();
        check_initialized("get_l2tlb_max_outstanding");
        return l2tlb_max_outstanding;
    endfunction:get_l2tlb_max_outstanding

    static function bit get_l2tlb_resp_reorder_en();
        check_initialized("get_l2tlb_resp_reorder_en");
        return l2tlb_resp_reorder_en;
    endfunction:get_l2tlb_resp_reorder_en

    static function int unsigned get_l2tlb_resp_mid_latency();
        check_initialized("get_l2tlb_resp_mid_latency");
        return l2tlb_resp_mid_latency;
    endfunction:get_l2tlb_resp_mid_latency

    static function int unsigned get_l2tlb_resp_long_latency();
        check_initialized("get_l2tlb_resp_long_latency");
        return l2tlb_resp_long_latency;
    endfunction:get_l2tlb_resp_long_latency

    static function int unsigned get_l2tlb_resp_1c_wt();
        check_initialized("get_l2tlb_resp_1c_wt");
        return l2tlb_resp_1c_wt;
    endfunction:get_l2tlb_resp_1c_wt

    static function int unsigned get_l2tlb_resp_mid_wt();
        check_initialized("get_l2tlb_resp_mid_wt");
        return l2tlb_resp_mid_wt;
    endfunction:get_l2tlb_resp_mid_wt

    static function int unsigned get_l2tlb_resp_long_wt();
        check_initialized("get_l2tlb_resp_long_wt");
        return l2tlb_resp_long_wt;
    endfunction:get_l2tlb_resp_long_wt

    static function int unsigned get_l2tlb_idle_stop_cycle();
        check_initialized("get_l2tlb_idle_stop_cycle");
        return l2tlb_idle_stop_cycle;
    endfunction:get_l2tlb_idle_stop_cycle

endclass:seq_csr_common

`endif
