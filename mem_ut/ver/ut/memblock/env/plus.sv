//=========================================================
//File name    : plus.sv
//Author       : OpenAI_Codex
//Module name  : plus
//Discribution : command-line parameter controls for memblock UT
//Date         : 2026-05-14
//=========================================================
`ifndef MEMBLOCK_PLUS__SV
`define MEMBLOCK_PLUS__SV

`define MEMBLOCK_PLUS_ARGS_DEFINE(name, type, val) \
    static type name = val;

class plus;

    // Generic parameter controls. These are intentionally not env component cfg.
    `MEMBLOCK_PLUS_ARGS_DEFINE(plus_memblock_demo_enable, bit, 1'b0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(plus_memblock_demo_depth, int, 0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(plus_memblock_demo_addr, bit [63:0], 64'h0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(plus_memblock_demo_data, bit [63:0], 64'h0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(plus_memblock_demo_stride, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(UVM_VERBOSITY, string, "UVM_HIGH")

    // Dispatch framework parameters. Fields intentionally match plusarg names.
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_MAIN_TRANS_NUM, int, 100)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_USE_MANUAL_MAIN_TABLE, bit, 1'b0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ENQ_PER_CYCLE, int, 4)
    // 使能后每次LSQ admission从[1:MEMBLOCK_REAL_ENQ_WIDTH]均匀随机本拍入队数量。
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ENQ_PER_CYCLE_RAND_EN, bit, 1'b0)
    // 当前DUT每拍LSQ enqueue总slot宽度镜像，默认按8-wide配置。
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_REAL_LSQ_ENQ_MAX, int, 8)
    // LOAD/STA/STD issue pipe配置上限；随机开关关闭时固定使用该上限，打开时每拍从[1:上限]采样。
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_LOAD_PIP_NUM_LIMIT, int, 3)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_STA_PIP_NUM_LIMIT, int, 2)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_STD_PIP_NUM_LIMIT, int, 2)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN, bit, 1'b0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_STA_PIP_NUM_RANDOM_EN, bit, 1'b0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_STD_PIP_NUM_RANDOM_EN, bit, 1'b0)

    // 历史兼容字段，必须与MEMBLOCK_REAL_LSQ_ENQ_MAX保持一致。
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_REAL_ENQ_WIDTH, int, 8)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_REAL_LOAD_PIPE_NUM, int, 3)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_REAL_STA_PIPE_NUM, int, 2)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_REAL_STD_PIPE_NUM, int, 2)

    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_OP_CLASS_INT_LOAD_WT, int, 8)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_OP_CLASS_FP_LOAD_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_OP_CLASS_STORE_WT, int, 6)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_OP_CLASS_PREFETCH_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_OP_CLASS_AMO_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_OP_CLASS_CBO_WT, int, 0)

    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_LOAD_FUOP_LB_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_LOAD_FUOP_LH_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_LOAD_FUOP_LW_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_LOAD_FUOP_LD_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_LOAD_FUOP_LBU_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_LOAD_FUOP_LHU_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_LOAD_FUOP_LWU_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_STORE_FUOP_SB_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_STORE_FUOP_SH_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_STORE_FUOP_SW_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_STORE_FUOP_SD_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_PREFETCH_FUOP_I_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_PREFETCH_FUOP_R_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_PREFETCH_FUOP_W_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_CBO_FUOP_ZERO_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_CBO_FUOP_CLEAN_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_CBO_FUOP_FLUSH_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_CBO_FUOP_INVAL_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_AMO_FUOP_LR_W_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_AMO_FUOP_SC_W_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_AMO_FUOP_AMOSWAP_W_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_AMO_FUOP_AMOADD_W_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_AMO_FUOP_AMOXOR_W_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_AMO_FUOP_AMOAND_W_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_AMO_FUOP_AMOOR_W_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_AMO_FUOP_AMOMIN_W_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_AMO_FUOP_AMOMAX_W_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_AMO_FUOP_AMOMINU_W_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_AMO_FUOP_AMOMAXU_W_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_AMO_FUOP_LR_D_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_AMO_FUOP_SC_D_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_AMO_FUOP_AMOSWAP_D_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_AMO_FUOP_AMOADD_D_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_AMO_FUOP_AMOXOR_D_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_AMO_FUOP_AMOAND_D_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_AMO_FUOP_AMOOR_D_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_AMO_FUOP_AMOMIN_D_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_AMO_FUOP_AMOMAX_D_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_AMO_FUOP_AMOMINU_D_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_AMO_FUOP_AMOMAXU_D_WT, int, 1)

    // boundary_profile 主表生成总开关。默认关闭，保持原 apply_minimal_op_template + apply_legal_addr_template 路径。
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_BOUNDARY_PROFILE_GEN_EN, bit, 1'b0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_BOUNDARY_ALIGNED_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_BOUNDARY_MISALIGN_WITHIN_8B_WT, int, 0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_BOUNDARY_CROSS_8B_WITHIN_16B_WT, int, 0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_BOUNDARY_CROSS_16B_SAME_LINE_WT, int, 0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_BOUNDARY_CROSS_CACHELINE_SAME_4K_WT, int, 0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_BOUNDARY_CROSS_4K_WT, int, 0)
    // STORE x CROSS_8B_WITHIN_16B 是可选 within-16B 子类，默认不作为 store 核心边界激励生成。
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_STORE_CROSS_8B_WITHIN_16B_EN, bit, 1'b0)

    // send_pri仲裁总开关；关闭时主表使用default priority，issue只按ROB age。
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_SEND_PRI_MODE_EN, bit, 1'b1)
    // send_pri模式下每拍进入global priority filter的权重，范围[0:100]。
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_GLOBAL_SEND_PRI_EN_WT, int, 0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_SEND_PRI_DEFAULT, int, 50)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_SEND_PRI_LOW_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_SEND_PRI_MID_WT, int, 8)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_SEND_PRI_HIGH_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_SEND_PRI_STD_DEFAULT, int, 50)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_SEND_PRI_STD_LOW_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_SEND_PRI_STD_MID_WT, int, 8)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_SEND_PRI_STD_HIGH_WT, int, 1)

    // 主表生成期地址复用控制：默认每条transaction约5%尝试一次recent-window复用。
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ADDR_REUSE_EN_1_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ADDR_REUSE_EN_0_WT, int, 19)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ADDR_REUSE_LOAD_AFTER_STORE_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ADDR_REUSE_LOAD_AFTER_LOAD_WT, int, 0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ADDR_REUSE_STORE_AFTER_LOAD_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ADDR_REUSE_STORE_AFTER_STORE_WT, int, 0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ADDR_REUSE_KEEP_REF_SIZE_EN_1_WT, int, 0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ADDR_REUSE_KEEP_REF_SIZE_EN_0_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ADDR_REF_WINDOW_FIXED, int, 0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ADDR_REF_WINDOW_SMALL_WEIGHT, int, 0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ADDR_REF_WINDOW_MEDIUM_WEIGHT, int, 0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ADDR_REF_WINDOW_LARGE_WEIGHT, int, 1)
    // 只随机ROB起始value，初始flag固定为0，后续由rob_advance按ROB_SIZE翻转。
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ROB_START_FIXED_EN, bit, 1'b1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ROB_START_FIXED_VALUE, int, 0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ROB_START_ZERO_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ROB_START_MID_WT, int, 0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ROB_START_NEAR_WRAP_WT, int, 0)

    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_DELAY_0_WT, int, 10)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_DELAY_1_20_WT, int, 5)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_DELAY_21_50_WT, int, 1)

    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_MDP_LOAD_WAIT_WT, int, 0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_MDP_STORESET_HIT_WT, int, 0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_LOAD_WAIT_STRICT_WT, int, 0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_RVC_WT, int, 0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_PC_BASE, bit [63:0], 64'h8000_0000)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_PC_STRIDE, int, 4)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_FTQ_IDX_BASE, int, 0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_PDEST_BASE, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_PDEST_RANGE, int, 128)

    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_TLB_PTE_R_1_WT, int, 8)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_TLB_PTE_R_0_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_TLB_PTE_W_1_WT, int, 6)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_TLB_PTE_W_0_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_TLB_PTE_X_1_WT, int, 4)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_TLB_PTE_X_0_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_TLB_PTE_U_1_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_TLB_PTE_U_0_WT, int, 8)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_TLB_PTE_G_1_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_TLB_PTE_G_0_WT, int, 8)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_TLB_PTE_A_1_WT, int, 8)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_TLB_PTE_A_0_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_TLB_PTE_D_1_WT, int, 8)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_TLB_PTE_D_0_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_TLB_PTE_N_1_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_TLB_PTE_N_0_WT, int, 8)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_TLB_PTE_V_1_WT, int, 9)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_TLB_PTE_V_0_WT, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_TLB_LEVEL_MODE, int, 0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_TLB_LEVEL_FIXED_VALUE, int, 0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_TLB_LEVEL_RANDOM_LOW, int, 0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_TLB_LEVEL_RANDOM_HIGH, int, 2)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_TLB_PTE_MODE, int, 0)

    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_PADDR_BASE, bit [63:0], 64'h8000_0000)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_PADDR_RANGE, bit [63:0], 64'h1000_0000)

    // 非严格 DUT 行为 / smoke 兼容控制。
    // 这些参数只影响 UT 框架如何补齐事件、处理 miss 或处理本地模型不一致，
    // 不会驱动 DUT 改变 RTL 行为。默认值偏向 early smoke 可闭环；
    // 做严格 DUT 行为验证时，需要按下面单项说明切到更严格配置。
    // 0: LSQ deq/free count 不一致时报 fatal；1: 仅报 warning，便于继续观察后续流程。
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_LSQ_RESYNC_ON_MISMATCH, bit, 1'b0)
    // 0: 允许 STA IQ feedback pass；1: 丢弃该兼容 pass，等待真实 STA writeback/feedback。默认等待真实路径。
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_STA_REAL_WB_PASS_EN, bit, 1'b1)
    // 0: 普通 store STD issue accept 后注入 synthetic pass；1: 等待真实 STD feedback/writeback。默认等待真实路径。
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_STD_REAL_WB_PASS_EN, bit, 1'b1)

    // 主动主流程driver的无进展debug阈值。该参数只打印warning，不作为正常退出条件。
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES, int, 10000)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_DISPATCH_ISSUE_SEQ_EN, bit, 1'b0)
    // issue driver 非阻塞上流水开关。
    // 0：保持旧行为，当前 xaction 内所有 valid port 都 ready 后才结束。
    // 1：每个 xaction 只采样一次 ready，未 fire item 留在 issue queue 下轮重试。
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN, bit, 1'b0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_DISPATCH_READY_TIMEOUT, int, 1000)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_REDIRECT_SEQ_EN, bit, 1'b1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_REDIRECT_DRIVE_TIMEOUT, int, 1000)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_REDIRECT_FREEZE_TIMEOUT, int, 1000)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_LSQENQ_SEQ_EN, bit, 1'b1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_LSQENQ_READY_TIMEOUT, int, 1000)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_LSQCOMMIT_SEQ_EN, bit, 1'b1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_FLUSHSB_SEQ_EN, bit, 1'b0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_FLUSHSB_REQUEST_CYCLE, int, 0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_FLUSHSB_TIMEOUT, int, 1000)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_REPLAY_WAIT_PTW_EN, bit, 1'b0)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_REPLAY_WAIT_PTW_TIMEOUT, int, 1000)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_L2TLB_SEQ_EN, bit, 1'b1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_L2TLB_MIN_LATENCY, int, 1)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_L2TLB_MAX_LATENCY, int, 4)
    `MEMBLOCK_PLUS_ARGS_DEFINE(MEMBLOCK_L2TLB_IDLE_STOP_CYCLE, int, 5000)

    function new();
        reload_from_cmdline();
    endfunction:new

    static function void reload_from_cmdline();
        load_bit("plus_memblock_demo_enable", plus_memblock_demo_enable);
        load_int("plus_memblock_demo_depth", plus_memblock_demo_depth);
        load_hex64("plus_memblock_demo_addr", plus_memblock_demo_addr);
        load_hex64("plus_memblock_demo_data", plus_memblock_demo_data);
        load_int("plus_memblock_demo_stride", plus_memblock_demo_stride);
        load_string("UVM_VERBOSITY", UVM_VERBOSITY);

        load_int("MEMBLOCK_MAIN_TRANS_NUM", MEMBLOCK_MAIN_TRANS_NUM);
        load_bit("MEMBLOCK_USE_MANUAL_MAIN_TABLE", MEMBLOCK_USE_MANUAL_MAIN_TABLE);
        load_int("MEMBLOCK_ENQ_PER_CYCLE", MEMBLOCK_ENQ_PER_CYCLE);
        load_bit("MEMBLOCK_ENQ_PER_CYCLE_RAND_EN", MEMBLOCK_ENQ_PER_CYCLE_RAND_EN);
        load_int("MEMBLOCK_REAL_LSQ_ENQ_MAX", MEMBLOCK_REAL_LSQ_ENQ_MAX);
        load_int("MEMBLOCK_LOAD_PIP_NUM_LIMIT", MEMBLOCK_LOAD_PIP_NUM_LIMIT);
        load_int("MEMBLOCK_STA_PIP_NUM_LIMIT", MEMBLOCK_STA_PIP_NUM_LIMIT);
        load_int("MEMBLOCK_STD_PIP_NUM_LIMIT", MEMBLOCK_STD_PIP_NUM_LIMIT);
        load_bit("MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN", MEMBLOCK_LOAD_PIP_NUM_RANDOM_EN);
        load_bit("MEMBLOCK_STA_PIP_NUM_RANDOM_EN", MEMBLOCK_STA_PIP_NUM_RANDOM_EN);
        load_bit("MEMBLOCK_STD_PIP_NUM_RANDOM_EN", MEMBLOCK_STD_PIP_NUM_RANDOM_EN);

        load_int("MEMBLOCK_REAL_ENQ_WIDTH", MEMBLOCK_REAL_ENQ_WIDTH);
        load_int("MEMBLOCK_REAL_LOAD_PIPE_NUM", MEMBLOCK_REAL_LOAD_PIPE_NUM);
        load_int("MEMBLOCK_REAL_STA_PIPE_NUM", MEMBLOCK_REAL_STA_PIPE_NUM);
        load_int("MEMBLOCK_REAL_STD_PIPE_NUM", MEMBLOCK_REAL_STD_PIPE_NUM);

        load_int("MEMBLOCK_OP_CLASS_INT_LOAD_WT", MEMBLOCK_OP_CLASS_INT_LOAD_WT);
        load_int("MEMBLOCK_OP_CLASS_FP_LOAD_WT", MEMBLOCK_OP_CLASS_FP_LOAD_WT);
        load_int("MEMBLOCK_OP_CLASS_STORE_WT", MEMBLOCK_OP_CLASS_STORE_WT);
        load_int("MEMBLOCK_OP_CLASS_PREFETCH_WT", MEMBLOCK_OP_CLASS_PREFETCH_WT);
        load_int("MEMBLOCK_OP_CLASS_AMO_WT", MEMBLOCK_OP_CLASS_AMO_WT);
        load_int("MEMBLOCK_OP_CLASS_CBO_WT", MEMBLOCK_OP_CLASS_CBO_WT);
        load_int("MEMBLOCK_LOAD_FUOP_LB_WT", MEMBLOCK_LOAD_FUOP_LB_WT);
        load_int("MEMBLOCK_LOAD_FUOP_LH_WT", MEMBLOCK_LOAD_FUOP_LH_WT);
        load_int("MEMBLOCK_LOAD_FUOP_LW_WT", MEMBLOCK_LOAD_FUOP_LW_WT);
        load_int("MEMBLOCK_LOAD_FUOP_LD_WT", MEMBLOCK_LOAD_FUOP_LD_WT);
        load_int("MEMBLOCK_LOAD_FUOP_LBU_WT", MEMBLOCK_LOAD_FUOP_LBU_WT);
        load_int("MEMBLOCK_LOAD_FUOP_LHU_WT", MEMBLOCK_LOAD_FUOP_LHU_WT);
        load_int("MEMBLOCK_LOAD_FUOP_LWU_WT", MEMBLOCK_LOAD_FUOP_LWU_WT);
        load_int("MEMBLOCK_STORE_FUOP_SB_WT", MEMBLOCK_STORE_FUOP_SB_WT);
        load_int("MEMBLOCK_STORE_FUOP_SH_WT", MEMBLOCK_STORE_FUOP_SH_WT);
        load_int("MEMBLOCK_STORE_FUOP_SW_WT", MEMBLOCK_STORE_FUOP_SW_WT);
        load_int("MEMBLOCK_STORE_FUOP_SD_WT", MEMBLOCK_STORE_FUOP_SD_WT);
        load_int("MEMBLOCK_PREFETCH_FUOP_I_WT", MEMBLOCK_PREFETCH_FUOP_I_WT);
        load_int("MEMBLOCK_PREFETCH_FUOP_R_WT", MEMBLOCK_PREFETCH_FUOP_R_WT);
        load_int("MEMBLOCK_PREFETCH_FUOP_W_WT", MEMBLOCK_PREFETCH_FUOP_W_WT);
        load_int("MEMBLOCK_CBO_FUOP_ZERO_WT", MEMBLOCK_CBO_FUOP_ZERO_WT);
        load_int("MEMBLOCK_CBO_FUOP_CLEAN_WT", MEMBLOCK_CBO_FUOP_CLEAN_WT);
        load_int("MEMBLOCK_CBO_FUOP_FLUSH_WT", MEMBLOCK_CBO_FUOP_FLUSH_WT);
        load_int("MEMBLOCK_CBO_FUOP_INVAL_WT", MEMBLOCK_CBO_FUOP_INVAL_WT);
        load_int("MEMBLOCK_AMO_FUOP_LR_W_WT", MEMBLOCK_AMO_FUOP_LR_W_WT);
        load_int("MEMBLOCK_AMO_FUOP_SC_W_WT", MEMBLOCK_AMO_FUOP_SC_W_WT);
        load_int("MEMBLOCK_AMO_FUOP_AMOSWAP_W_WT", MEMBLOCK_AMO_FUOP_AMOSWAP_W_WT);
        load_int("MEMBLOCK_AMO_FUOP_AMOADD_W_WT", MEMBLOCK_AMO_FUOP_AMOADD_W_WT);
        load_int("MEMBLOCK_AMO_FUOP_AMOXOR_W_WT", MEMBLOCK_AMO_FUOP_AMOXOR_W_WT);
        load_int("MEMBLOCK_AMO_FUOP_AMOAND_W_WT", MEMBLOCK_AMO_FUOP_AMOAND_W_WT);
        load_int("MEMBLOCK_AMO_FUOP_AMOOR_W_WT", MEMBLOCK_AMO_FUOP_AMOOR_W_WT);
        load_int("MEMBLOCK_AMO_FUOP_AMOMIN_W_WT", MEMBLOCK_AMO_FUOP_AMOMIN_W_WT);
        load_int("MEMBLOCK_AMO_FUOP_AMOMAX_W_WT", MEMBLOCK_AMO_FUOP_AMOMAX_W_WT);
        load_int("MEMBLOCK_AMO_FUOP_AMOMINU_W_WT", MEMBLOCK_AMO_FUOP_AMOMINU_W_WT);
        load_int("MEMBLOCK_AMO_FUOP_AMOMAXU_W_WT", MEMBLOCK_AMO_FUOP_AMOMAXU_W_WT);
        load_int("MEMBLOCK_AMO_FUOP_LR_D_WT", MEMBLOCK_AMO_FUOP_LR_D_WT);
        load_int("MEMBLOCK_AMO_FUOP_SC_D_WT", MEMBLOCK_AMO_FUOP_SC_D_WT);
        load_int("MEMBLOCK_AMO_FUOP_AMOSWAP_D_WT", MEMBLOCK_AMO_FUOP_AMOSWAP_D_WT);
        load_int("MEMBLOCK_AMO_FUOP_AMOADD_D_WT", MEMBLOCK_AMO_FUOP_AMOADD_D_WT);
        load_int("MEMBLOCK_AMO_FUOP_AMOXOR_D_WT", MEMBLOCK_AMO_FUOP_AMOXOR_D_WT);
        load_int("MEMBLOCK_AMO_FUOP_AMOAND_D_WT", MEMBLOCK_AMO_FUOP_AMOAND_D_WT);
        load_int("MEMBLOCK_AMO_FUOP_AMOOR_D_WT", MEMBLOCK_AMO_FUOP_AMOOR_D_WT);
        load_int("MEMBLOCK_AMO_FUOP_AMOMIN_D_WT", MEMBLOCK_AMO_FUOP_AMOMIN_D_WT);
        load_int("MEMBLOCK_AMO_FUOP_AMOMAX_D_WT", MEMBLOCK_AMO_FUOP_AMOMAX_D_WT);
        load_int("MEMBLOCK_AMO_FUOP_AMOMINU_D_WT", MEMBLOCK_AMO_FUOP_AMOMINU_D_WT);
        load_int("MEMBLOCK_AMO_FUOP_AMOMAXU_D_WT", MEMBLOCK_AMO_FUOP_AMOMAXU_D_WT);
        load_bit("MEMBLOCK_BOUNDARY_PROFILE_GEN_EN", MEMBLOCK_BOUNDARY_PROFILE_GEN_EN);
        load_int("MEMBLOCK_BOUNDARY_ALIGNED_WT", MEMBLOCK_BOUNDARY_ALIGNED_WT);
        load_int("MEMBLOCK_BOUNDARY_MISALIGN_WITHIN_8B_WT", MEMBLOCK_BOUNDARY_MISALIGN_WITHIN_8B_WT);
        load_int("MEMBLOCK_BOUNDARY_CROSS_8B_WITHIN_16B_WT", MEMBLOCK_BOUNDARY_CROSS_8B_WITHIN_16B_WT);
        load_int("MEMBLOCK_BOUNDARY_CROSS_16B_SAME_LINE_WT", MEMBLOCK_BOUNDARY_CROSS_16B_SAME_LINE_WT);
        load_int("MEMBLOCK_BOUNDARY_CROSS_CACHELINE_SAME_4K_WT", MEMBLOCK_BOUNDARY_CROSS_CACHELINE_SAME_4K_WT);
        load_int("MEMBLOCK_BOUNDARY_CROSS_4K_WT", MEMBLOCK_BOUNDARY_CROSS_4K_WT);
        load_bit("MEMBLOCK_STORE_CROSS_8B_WITHIN_16B_EN", MEMBLOCK_STORE_CROSS_8B_WITHIN_16B_EN);

        load_bit("MEMBLOCK_SEND_PRI_MODE_EN", MEMBLOCK_SEND_PRI_MODE_EN);
        load_int("MEMBLOCK_GLOBAL_SEND_PRI_EN_WT", MEMBLOCK_GLOBAL_SEND_PRI_EN_WT);
        load_int("MEMBLOCK_SEND_PRI_DEFAULT", MEMBLOCK_SEND_PRI_DEFAULT);
        load_int("MEMBLOCK_SEND_PRI_LOW_WT", MEMBLOCK_SEND_PRI_LOW_WT);
        load_int("MEMBLOCK_SEND_PRI_MID_WT", MEMBLOCK_SEND_PRI_MID_WT);
        load_int("MEMBLOCK_SEND_PRI_HIGH_WT", MEMBLOCK_SEND_PRI_HIGH_WT);
        load_int("MEMBLOCK_SEND_PRI_STD_DEFAULT", MEMBLOCK_SEND_PRI_STD_DEFAULT);
        load_int("MEMBLOCK_SEND_PRI_STD_LOW_WT", MEMBLOCK_SEND_PRI_STD_LOW_WT);
        load_int("MEMBLOCK_SEND_PRI_STD_MID_WT", MEMBLOCK_SEND_PRI_STD_MID_WT);
        load_int("MEMBLOCK_SEND_PRI_STD_HIGH_WT", MEMBLOCK_SEND_PRI_STD_HIGH_WT);

        load_int("MEMBLOCK_ADDR_REUSE_EN_1_WT", MEMBLOCK_ADDR_REUSE_EN_1_WT);
        load_int("MEMBLOCK_ADDR_REUSE_EN_0_WT", MEMBLOCK_ADDR_REUSE_EN_0_WT);
        load_int("MEMBLOCK_ADDR_REUSE_LOAD_AFTER_STORE_WT", MEMBLOCK_ADDR_REUSE_LOAD_AFTER_STORE_WT);
        load_int("MEMBLOCK_ADDR_REUSE_LOAD_AFTER_LOAD_WT", MEMBLOCK_ADDR_REUSE_LOAD_AFTER_LOAD_WT);
        load_int("MEMBLOCK_ADDR_REUSE_STORE_AFTER_LOAD_WT", MEMBLOCK_ADDR_REUSE_STORE_AFTER_LOAD_WT);
        load_int("MEMBLOCK_ADDR_REUSE_STORE_AFTER_STORE_WT", MEMBLOCK_ADDR_REUSE_STORE_AFTER_STORE_WT);
        load_int("MEMBLOCK_ADDR_REUSE_KEEP_REF_SIZE_EN_1_WT", MEMBLOCK_ADDR_REUSE_KEEP_REF_SIZE_EN_1_WT);
        load_int("MEMBLOCK_ADDR_REUSE_KEEP_REF_SIZE_EN_0_WT", MEMBLOCK_ADDR_REUSE_KEEP_REF_SIZE_EN_0_WT);
        load_int("MEMBLOCK_ADDR_REF_WINDOW_FIXED", MEMBLOCK_ADDR_REF_WINDOW_FIXED);
        load_int("MEMBLOCK_ADDR_REF_WINDOW_SMALL_WEIGHT", MEMBLOCK_ADDR_REF_WINDOW_SMALL_WEIGHT);
        load_int("MEMBLOCK_ADDR_REF_WINDOW_MEDIUM_WEIGHT", MEMBLOCK_ADDR_REF_WINDOW_MEDIUM_WEIGHT);
        load_int("MEMBLOCK_ADDR_REF_WINDOW_LARGE_WEIGHT", MEMBLOCK_ADDR_REF_WINDOW_LARGE_WEIGHT);
        load_bit("MEMBLOCK_ROB_START_FIXED_EN", MEMBLOCK_ROB_START_FIXED_EN);
        load_int("MEMBLOCK_ROB_START_FIXED_VALUE", MEMBLOCK_ROB_START_FIXED_VALUE);
        load_int("MEMBLOCK_ROB_START_ZERO_WT", MEMBLOCK_ROB_START_ZERO_WT);
        load_int("MEMBLOCK_ROB_START_MID_WT", MEMBLOCK_ROB_START_MID_WT);
        load_int("MEMBLOCK_ROB_START_NEAR_WRAP_WT", MEMBLOCK_ROB_START_NEAR_WRAP_WT);

        load_int("MEMBLOCK_DELAY_0_WT", MEMBLOCK_DELAY_0_WT);
        load_int("MEMBLOCK_DELAY_1_20_WT", MEMBLOCK_DELAY_1_20_WT);
        load_int("MEMBLOCK_DELAY_21_50_WT", MEMBLOCK_DELAY_21_50_WT);

        load_int("MEMBLOCK_MDP_LOAD_WAIT_WT", MEMBLOCK_MDP_LOAD_WAIT_WT);
        load_int("MEMBLOCK_MDP_STORESET_HIT_WT", MEMBLOCK_MDP_STORESET_HIT_WT);
        load_int("MEMBLOCK_LOAD_WAIT_STRICT_WT", MEMBLOCK_LOAD_WAIT_STRICT_WT);
        load_int("MEMBLOCK_RVC_WT", MEMBLOCK_RVC_WT);
        load_hex64("MEMBLOCK_PC_BASE", MEMBLOCK_PC_BASE);
        load_int("MEMBLOCK_PC_STRIDE", MEMBLOCK_PC_STRIDE);
        load_int("MEMBLOCK_FTQ_IDX_BASE", MEMBLOCK_FTQ_IDX_BASE);
        load_int("MEMBLOCK_PDEST_BASE", MEMBLOCK_PDEST_BASE);
        load_int("MEMBLOCK_PDEST_RANGE", MEMBLOCK_PDEST_RANGE);

        load_int("MEMBLOCK_TLB_PTE_R_1_WT", MEMBLOCK_TLB_PTE_R_1_WT);
        load_int("MEMBLOCK_TLB_PTE_R_0_WT", MEMBLOCK_TLB_PTE_R_0_WT);
        load_int("MEMBLOCK_TLB_PTE_W_1_WT", MEMBLOCK_TLB_PTE_W_1_WT);
        load_int("MEMBLOCK_TLB_PTE_W_0_WT", MEMBLOCK_TLB_PTE_W_0_WT);
        load_int("MEMBLOCK_TLB_PTE_X_1_WT", MEMBLOCK_TLB_PTE_X_1_WT);
        load_int("MEMBLOCK_TLB_PTE_X_0_WT", MEMBLOCK_TLB_PTE_X_0_WT);
        load_int("MEMBLOCK_TLB_PTE_U_1_WT", MEMBLOCK_TLB_PTE_U_1_WT);
        load_int("MEMBLOCK_TLB_PTE_U_0_WT", MEMBLOCK_TLB_PTE_U_0_WT);
        load_int("MEMBLOCK_TLB_PTE_G_1_WT", MEMBLOCK_TLB_PTE_G_1_WT);
        load_int("MEMBLOCK_TLB_PTE_G_0_WT", MEMBLOCK_TLB_PTE_G_0_WT);
        load_int("MEMBLOCK_TLB_PTE_A_1_WT", MEMBLOCK_TLB_PTE_A_1_WT);
        load_int("MEMBLOCK_TLB_PTE_A_0_WT", MEMBLOCK_TLB_PTE_A_0_WT);
        load_int("MEMBLOCK_TLB_PTE_D_1_WT", MEMBLOCK_TLB_PTE_D_1_WT);
        load_int("MEMBLOCK_TLB_PTE_D_0_WT", MEMBLOCK_TLB_PTE_D_0_WT);
        load_int("MEMBLOCK_TLB_PTE_N_1_WT", MEMBLOCK_TLB_PTE_N_1_WT);
        load_int("MEMBLOCK_TLB_PTE_N_0_WT", MEMBLOCK_TLB_PTE_N_0_WT);
        load_int("MEMBLOCK_TLB_PTE_V_1_WT", MEMBLOCK_TLB_PTE_V_1_WT);
        load_int("MEMBLOCK_TLB_PTE_V_0_WT", MEMBLOCK_TLB_PTE_V_0_WT);
        load_int("MEMBLOCK_TLB_LEVEL_MODE", MEMBLOCK_TLB_LEVEL_MODE);
        load_int("MEMBLOCK_TLB_LEVEL_FIXED_VALUE", MEMBLOCK_TLB_LEVEL_FIXED_VALUE);
        load_int("MEMBLOCK_TLB_LEVEL_RANDOM_LOW", MEMBLOCK_TLB_LEVEL_RANDOM_LOW);
        load_int("MEMBLOCK_TLB_LEVEL_RANDOM_HIGH", MEMBLOCK_TLB_LEVEL_RANDOM_HIGH);
        load_int("MEMBLOCK_TLB_PTE_MODE", MEMBLOCK_TLB_PTE_MODE);

        load_hex64("MEMBLOCK_PADDR_BASE", MEMBLOCK_PADDR_BASE);
        load_hex64("MEMBLOCK_PADDR_RANGE", MEMBLOCK_PADDR_RANGE);

        // 读取非严格 DUT 行为 / smoke 兼容控制。严格 DUT 行为验证时重点检查这些开关。
        load_bit("MEMBLOCK_LSQ_RESYNC_ON_MISMATCH", MEMBLOCK_LSQ_RESYNC_ON_MISMATCH);
        load_bit("MEMBLOCK_STA_REAL_WB_PASS_EN", MEMBLOCK_STA_REAL_WB_PASS_EN);
        load_bit("MEMBLOCK_STD_REAL_WB_PASS_EN", MEMBLOCK_STD_REAL_WB_PASS_EN);

        load_int("MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES", MEMBLOCK_ACTIVE_SEQ_NO_PROGRESS_WARN_CYCLES);
        load_bit("MEMBLOCK_DISPATCH_ISSUE_SEQ_EN", MEMBLOCK_DISPATCH_ISSUE_SEQ_EN);
        load_bit("MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN", MEMBLOCK_DISPATCH_ISSUE_NONBLOCKING_EN);
        load_int("MEMBLOCK_DISPATCH_READY_TIMEOUT", MEMBLOCK_DISPATCH_READY_TIMEOUT);
        load_bit("MEMBLOCK_REDIRECT_SEQ_EN", MEMBLOCK_REDIRECT_SEQ_EN);
        load_int("MEMBLOCK_REDIRECT_DRIVE_TIMEOUT", MEMBLOCK_REDIRECT_DRIVE_TIMEOUT);
        load_int("MEMBLOCK_REDIRECT_FREEZE_TIMEOUT", MEMBLOCK_REDIRECT_FREEZE_TIMEOUT);
        load_bit("MEMBLOCK_LSQENQ_SEQ_EN", MEMBLOCK_LSQENQ_SEQ_EN);
        load_int("MEMBLOCK_LSQENQ_READY_TIMEOUT", MEMBLOCK_LSQENQ_READY_TIMEOUT);
        load_bit("MEMBLOCK_LSQCOMMIT_SEQ_EN", MEMBLOCK_LSQCOMMIT_SEQ_EN);
        load_bit("MEMBLOCK_FLUSHSB_SEQ_EN", MEMBLOCK_FLUSHSB_SEQ_EN);
        load_int("MEMBLOCK_FLUSHSB_REQUEST_CYCLE", MEMBLOCK_FLUSHSB_REQUEST_CYCLE);
        load_int("MEMBLOCK_FLUSHSB_TIMEOUT", MEMBLOCK_FLUSHSB_TIMEOUT);
        load_bit("MEMBLOCK_REPLAY_WAIT_PTW_EN", MEMBLOCK_REPLAY_WAIT_PTW_EN);
        load_int("MEMBLOCK_REPLAY_WAIT_PTW_TIMEOUT", MEMBLOCK_REPLAY_WAIT_PTW_TIMEOUT);
        load_bit("MEMBLOCK_L2TLB_SEQ_EN", MEMBLOCK_L2TLB_SEQ_EN);
        load_int("MEMBLOCK_L2TLB_MIN_LATENCY", MEMBLOCK_L2TLB_MIN_LATENCY);
        load_int("MEMBLOCK_L2TLB_MAX_LATENCY", MEMBLOCK_L2TLB_MAX_LATENCY);
        load_int("MEMBLOCK_L2TLB_IDLE_STOP_CYCLE", MEMBLOCK_L2TLB_IDLE_STOP_CYCLE);
    endfunction:reload_from_cmdline

    static function void load_string(string name, ref string dst);
        string value;

        if (get_plusarg_value(name, value)) begin
            dst = value;
            $display("%s = %s", name, dst);
        end
    endfunction:load_string

    static function void load_int(string name, ref int dst);
        string value;
        int tmp;

        if (get_plusarg_value(name, value)) begin
            if ($sscanf(value, "%0d", tmp) == 1) begin
                dst = tmp;
                $display("%s = %0d", name, dst);
            end else begin
                `uvm_warning("MEMBLOCK_PLUS", $sformatf("Ignore invalid %s=%s", name, value))
            end
        end
    endfunction:load_int

    static function void load_bit(string name, ref bit dst);
        string value;

        if (get_plusarg_value(name, value)) begin
            if (value == "0") begin
                dst = 1'b0;
            end else if (value == "1") begin
                dst = 1'b1;
            end else begin
                `uvm_fatal("MEMBLOCK_PLUS", $sformatf("%s must be 0 or 1, got %s", name, value))
            end
            $display("%s = %0d", name, dst);
        end
    endfunction:load_bit

    static function void load_hex64(string name, ref bit [63:0] dst);
        string value;
        string parse_value;
        bit [63:0] tmp;

        if (get_plusarg_value(name, value)) begin
            if (value.len() == 0 || value.substr(0, 0) == "-") begin
                `uvm_fatal("MEMBLOCK_PLUS", $sformatf("%s must be a non-negative hex value, got %s", name, value))
            end
            if (!is_legal_hex_string(value)) begin
                `uvm_fatal("MEMBLOCK_PLUS", $sformatf("%s has illegal hex value %s", name, value))
            end
            if (get_hex_digit_count(value) > 16) begin
                `uvm_fatal("MEMBLOCK_PLUS", $sformatf("%s exceeds 64-bit hex width: %s", name, value))
            end

            parse_value = value;
            if (value.len() >= 2 &&
                (value.substr(0, 1) == "0x" || value.substr(0, 1) == "0X")) begin
                parse_value = value.substr(2, value.len() - 1);
            end

            if ($sscanf(parse_value, "%h", tmp) == 1) begin
                dst = tmp;
                $display("%s = 0x%0h", name, dst);
            end else begin
                `uvm_fatal("MEMBLOCK_PLUS", $sformatf("%s has illegal hex value %s", name, value))
            end
        end
    endfunction:load_hex64

    static function bit get_plusarg_value(string name, ref string value);
        value = "";
        return $value$plusargs({name, "=%s"}, value);
    endfunction:get_plusarg_value

    static function bit is_legal_hex_string(string value);
        int unsigned start_idx;
        int unsigned digit_cnt;
        byte c;

        start_idx = 0;
        digit_cnt = 0;
        if (value.len() >= 2 &&
            (value.substr(0, 1) == "0x" || value.substr(0, 1) == "0X")) begin
            start_idx = 2;
        end

        for (int unsigned i = start_idx; i < value.len(); i++) begin
            c = value[i];
            if (c == "_") begin
                continue;
            end
            if ((c >= "0" && c <= "9") ||
                (c >= "a" && c <= "f") ||
                (c >= "A" && c <= "F")) begin
                digit_cnt++;
                continue;
            end
            return 1'b0;
        end

        return digit_cnt != 0;
    endfunction:is_legal_hex_string

    static function int unsigned get_hex_digit_count(string value);
        int unsigned start_idx;
        int unsigned digit_cnt;
        byte c;

        start_idx = 0;
        digit_cnt = 0;
        if (value.len() >= 2 &&
            (value.substr(0, 1) == "0x" || value.substr(0, 1) == "0X")) begin
            start_idx = 2;
        end

        for (int unsigned i = start_idx; i < value.len(); i++) begin
            c = value[i];
            if (c == "_") begin
                continue;
            end
            digit_cnt++;
        end

        return digit_cnt;
    endfunction:get_hex_digit_count

endclass:plus

`undef MEMBLOCK_PLUS_ARGS_DEFINE

`endif
