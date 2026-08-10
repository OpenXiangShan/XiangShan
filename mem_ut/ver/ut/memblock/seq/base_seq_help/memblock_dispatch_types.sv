//=========================================================
//File name    : memblock_dispatch_types.sv
//Author       : OpenAI_Codex
//Module name  : memblock_dispatch_types
//Discribution : shared dispatch framework typedefs
//Date         : 2026-05-18
//=========================================================
`ifndef MEMBLOCK_DISPATCH_TYPES__SV
`define MEMBLOCK_DISPATCH_TYPES__SV

`include "memblock_compile_params.svh"

localparam int unsigned MEMBLOCK_ROB_SIZE = `MEMBLOCK_DUT_ROB_SIZE;
localparam int unsigned MEMBLOCK_LQ_SIZE  = `MEMBLOCK_DUT_LQ_SIZE;
localparam int unsigned MEMBLOCK_SQ_SIZE  = `MEMBLOCK_DUT_SQ_SIZE;
localparam int unsigned MEMBLOCK_COMMIT_WIDTH = `MEMBLOCK_DUT_COMMIT_WIDTH;

// 中文注释：DUT物理LSQ enqueue slot和scalar issue pipe数量。
// interface/driver/scheduler直接消费这些编译期常量；runtime plus只能调小行为使用量。
localparam int unsigned MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM = `MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM;
localparam int unsigned MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH = `MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH;
localparam int unsigned MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH = `MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH;
localparam int unsigned MEMBLOCK_DUT_LOAD_PIPE_NUM    = `MEMBLOCK_DUT_LOAD_PIPE_NUM;
localparam int unsigned MEMBLOCK_DUT_STA_PIPE_NUM     = `MEMBLOCK_DUT_STA_PIPE_NUM;
localparam int unsigned MEMBLOCK_DUT_STD_PIPE_NUM     = `MEMBLOCK_DUT_STD_PIPE_NUM;
localparam int unsigned MEMBLOCK_DUT_LOAD_PORT_BASE   = `MEMBLOCK_DUT_LOAD_PORT_BASE;
localparam int unsigned MEMBLOCK_DUT_STA_PORT_BASE =
    MEMBLOCK_DUT_LOAD_PORT_BASE + MEMBLOCK_DUT_LOAD_PIPE_NUM;
localparam int unsigned MEMBLOCK_DUT_STD_PORT_BASE =
    MEMBLOCK_DUT_STA_PORT_BASE + MEMBLOCK_DUT_STA_PIPE_NUM;
localparam int unsigned MEMBLOCK_DUT_SCALAR_ISSUE_PORT_NUM =
    MEMBLOCK_DUT_STD_PORT_BASE + MEMBLOCK_DUT_STD_PIPE_NUM;
localparam int unsigned MEMBLOCK_DUT_SCALAR_ISSUE_MASK_W =
    MEMBLOCK_DUT_SCALAR_ISSUE_PORT_NUM;
localparam int unsigned MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM = `MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM;
// 中文注释：两个外部 memory responder 的物理 response record 上限。
// 设置：由 V2 compile profile 宏固定；使用：DCache/Uncache scheduler 准入时直接读取。
// 作用：避免 runtime plus 生成超过 DUT in-flight 能力的 request 接收行为。
localparam int unsigned MEMBLOCK_DUT_DCACHE_A_MAX_OUTSTANDING =
    `MEMBLOCK_DUT_DCACHE_A_MAX_OUTSTANDING;
localparam int unsigned MEMBLOCK_DUT_UNCACHE_MAX_OUTSTANDING =
    `MEMBLOCK_DUT_UNCACHE_MAX_OUTSTANDING;
localparam bit MEMBLOCK_DUT_ISSUE_PORT_STYLE_SPLIT = `MEMBLOCK_DUT_ISSUE_PORT_STYLE_SPLIT;
localparam bit MEMBLOCK_DUT_LSQ_ENQ_HAS_ACCEPT_RESP = `MEMBLOCK_DUT_LSQ_ENQ_HAS_ACCEPT_RESP;
localparam bit MEMBLOCK_DUT_HAS_SQ_DEQ_PTR = `MEMBLOCK_DUT_HAS_SQ_DEQ_PTR;
localparam int unsigned MEMBLOCK_DUT_ENSBUFFER_WIDTH = `MEMBLOCK_DUT_ENSBUFFER_WIDTH;
localparam int unsigned MEMBLOCK_SQ_DEQ_COUNT_W = `MEMBLOCK_SQ_DEQ_COUNT_W;
localparam int unsigned MEMBLOCK_LQ_CANCEL_COUNT_W = `MEMBLOCK_LQ_CANCEL_COUNT_W;
localparam int unsigned MEMBLOCK_SQ_CANCEL_COUNT_W = `MEMBLOCK_SQ_CANCEL_COUNT_W;
localparam int unsigned MEMBLOCK_DUT_REDIRECT_TO_LSQ_LATENCY = `MEMBLOCK_DUT_REDIRECT_TO_LSQ_LATENCY;
localparam int unsigned MEMBLOCK_DUT_CANCEL_OUTPUT_LATENCY = `MEMBLOCK_DUT_CANCEL_OUTPUT_LATENCY;
localparam int unsigned MEMBLOCK_TB_CANCEL_MONITOR_SAMPLE_OFFSET =
    `MEMBLOCK_TB_CANCEL_MONITOR_SAMPLE_OFFSET;
localparam int unsigned MEMBLOCK_CANCEL_SNAPSHOT_OBSERVE_LATENCY =
    `MEMBLOCK_CANCEL_SNAPSHOT_OBSERVE_LATENCY;
localparam int unsigned MEMBLOCK_CANCEL_RECORD_MAX_DEPTH = `MEMBLOCK_CANCEL_RECORD_MAX_DEPTH;
localparam int unsigned MEMBLOCK_CANCEL_SNAPSHOT_QUEUE_MAX_DEPTH =
    `MEMBLOCK_CANCEL_SNAPSHOT_QUEUE_MAX_DEPTH;
// 中文注释：V2 DTLB filter容量和顶层flush观测到filter清空的hold拍数。
// L2TLB responder只读这些typed localparam，用于queue上界、参数收敛和ready恢复边界。
localparam int unsigned MEMBLOCK_DUT_L2TLB_DFILTER_SIZE =
    `MEMBLOCK_DUT_L2TLB_DFILTER_SIZE;
localparam int unsigned MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES =
    `MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES;
localparam int unsigned MEMBLOCK_DUT_L2TLB_CSR_PIPE_STAGES =
    `MEMBLOCK_DUT_L2TLB_CSR_PIPE_STAGES;
localparam int unsigned MEMBLOCK_L2TLB_EVENT_HISTORY_MAX_DEPTH =
    `MEMBLOCK_L2TLB_EVENT_HISTORY_MAX_DEPTH;
localparam int unsigned MEMBLOCK_L2TLB_SAMPLE_MAILBOX_DEPTH =
    `MEMBLOCK_L2TLB_SAMPLE_MAILBOX_DEPTH;
localparam int unsigned MEMBLOCK_L2TLB_SAMPLE_PROBE_MAX_DELTA =
    `MEMBLOCK_L2TLB_SAMPLE_PROBE_MAX_DELTA;
localparam int unsigned MEMBLOCK_L2TLB_SAMPLE_NOT_READY_MAX_SAMPLES =
    `MEMBLOCK_L2TLB_SAMPLE_NOT_READY_MAX_SAMPLES;
localparam int unsigned MEMBLOCK_L2TLB_BASELINE_MAX_SAMPLE_DISTANCE =
    `MEMBLOCK_L2TLB_BASELINE_MAX_SAMPLE_DISTANCE;
localparam int unsigned MEMBLOCK_DUT_MAX_UOP_SIZE = `MEMBLOCK_DUT_MAX_UOP_SIZE;
localparam int unsigned MEMBLOCK_DUT_UOP_IDX_W = `MEMBLOCK_DUT_UOP_IDX_W;
localparam int unsigned MEMBLOCK_DUT_VLEN = `MEMBLOCK_DUT_VLEN;
localparam int unsigned MEMBLOCK_DUT_MAX_LS_ELEM = `MEMBLOCK_DUT_MAX_LS_ELEM;
localparam int unsigned MEMBLOCK_DUT_NUM_LS_ELEM_W = `MEMBLOCK_DUT_NUM_LS_ELEM_W;

// 中文注释：index value字段宽度只覆盖value本体，wrap flag单独保存在memblock_*_key_t。
// V2默认ROB value为8 bit；合法取值仍由MEMBLOCK_*_SIZE限制，避免随机到DUT不存在的entry。
localparam int unsigned MEMBLOCK_ROB_VALUE_W = `MEMBLOCK_DUT_ROB_VALUE_W;
localparam int unsigned MEMBLOCK_LQ_VALUE_W  = `MEMBLOCK_DUT_LQ_VALUE_W;
localparam int unsigned MEMBLOCK_SQ_VALUE_W  = `MEMBLOCK_DUT_SQ_VALUE_W;
localparam int unsigned MEMBLOCK_FTQ_PTR_VALUE_W = `MEMBLOCK_DUT_FTQ_PTR_VALUE_W;
localparam int unsigned MEMBLOCK_FTQ_OFFSET_W    = `MEMBLOCK_DUT_FTQ_OFFSET_W;

// 当前版本FuType one-hot编码。内部容器保留36 bit，DUT-facing宽度单独检查。
localparam int unsigned MEMBLOCK_INTERNAL_FUTYPE_W = `MEMBLOCK_INTERNAL_FUTYPE_W;
localparam int unsigned MEMBLOCK_DUT_FUTYPE_W      = `MEMBLOCK_DUT_FUTYPE_W;
localparam int unsigned MEMBLOCK_DUT_FUTYPE_LDU_BIT     = `MEMBLOCK_DUT_FUTYPE_LDU_BIT;
localparam int unsigned MEMBLOCK_DUT_FUTYPE_STU_BIT     = `MEMBLOCK_DUT_FUTYPE_STU_BIT;
localparam int unsigned MEMBLOCK_DUT_FUTYPE_MOU_BIT     = `MEMBLOCK_DUT_FUTYPE_MOU_BIT;
localparam int unsigned MEMBLOCK_DUT_FUTYPE_VLDU_BIT    = `MEMBLOCK_DUT_FUTYPE_VLDU_BIT;
localparam int unsigned MEMBLOCK_DUT_FUTYPE_VSTU_BIT    = `MEMBLOCK_DUT_FUTYPE_VSTU_BIT;
localparam int unsigned MEMBLOCK_DUT_FUTYPE_VSEGLDU_BIT = `MEMBLOCK_DUT_FUTYPE_VSEGLDU_BIT;
localparam int unsigned MEMBLOCK_DUT_FUTYPE_VSEGSTU_BIT = `MEMBLOCK_DUT_FUTYPE_VSEGSTU_BIT;

localparam bit [MEMBLOCK_INTERNAL_FUTYPE_W-1:0] MEMBLOCK_FUTYPE_LDU =
    MEMBLOCK_INTERNAL_FUTYPE_W'(1) << MEMBLOCK_DUT_FUTYPE_LDU_BIT;
localparam bit [MEMBLOCK_INTERNAL_FUTYPE_W-1:0] MEMBLOCK_FUTYPE_STU =
    MEMBLOCK_INTERNAL_FUTYPE_W'(1) << MEMBLOCK_DUT_FUTYPE_STU_BIT;
localparam bit [MEMBLOCK_INTERNAL_FUTYPE_W-1:0] MEMBLOCK_FUTYPE_MOU =
    MEMBLOCK_INTERNAL_FUTYPE_W'(1) << MEMBLOCK_DUT_FUTYPE_MOU_BIT;
localparam bit [MEMBLOCK_INTERNAL_FUTYPE_W-1:0] MEMBLOCK_FUTYPE_VLDU =
    MEMBLOCK_INTERNAL_FUTYPE_W'(1) << MEMBLOCK_DUT_FUTYPE_VLDU_BIT;
localparam bit [MEMBLOCK_INTERNAL_FUTYPE_W-1:0] MEMBLOCK_FUTYPE_VSTU =
    MEMBLOCK_INTERNAL_FUTYPE_W'(1) << MEMBLOCK_DUT_FUTYPE_VSTU_BIT;
localparam bit [MEMBLOCK_INTERNAL_FUTYPE_W-1:0] MEMBLOCK_FUTYPE_VSEGLDU =
    MEMBLOCK_INTERNAL_FUTYPE_W'(1) << MEMBLOCK_DUT_FUTYPE_VSEGLDU_BIT;
localparam bit [MEMBLOCK_INTERNAL_FUTYPE_W-1:0] MEMBLOCK_FUTYPE_VSEGSTU =
    MEMBLOCK_INTERNAL_FUTYPE_W'(1) << MEMBLOCK_DUT_FUTYPE_VSEGSTU_BIT;

function automatic bit [MEMBLOCK_DUT_FUTYPE_W-1:0]
encode_and_fit_dut_futype(input bit [MEMBLOCK_INTERNAL_FUTYPE_W-1:0] internal_fuType,
                          input string caller);
    case (internal_fuType)
        MEMBLOCK_FUTYPE_LDU,
        MEMBLOCK_FUTYPE_STU,
        MEMBLOCK_FUTYPE_MOU: begin
        end
        MEMBLOCK_FUTYPE_VLDU,
        MEMBLOCK_FUTYPE_VSTU,
        MEMBLOCK_FUTYPE_VSEGLDU,
        MEMBLOCK_FUTYPE_VSEGSTU: begin
            `uvm_fatal("MEMBLOCK_FUTYPE", $sformatf("%s does not support vector LS FuType=0x%0h", caller, internal_fuType))
        end
        default: begin
            `uvm_fatal("MEMBLOCK_FUTYPE", $sformatf("%s got unknown FuType=0x%0h", caller, internal_fuType))
        end
    endcase

    if ((internal_fuType >> MEMBLOCK_DUT_FUTYPE_W) != '0) begin
        `uvm_fatal("MEMBLOCK_FUTYPE",
                   $sformatf("%s FuType=0x%0h has bits above DUT width=%0d",
                             caller, internal_fuType, MEMBLOCK_DUT_FUTYPE_W))
    end
    return internal_fuType[MEMBLOCK_DUT_FUTYPE_W-1:0];
endfunction:encode_and_fit_dut_futype

function automatic bit [MEMBLOCK_ROB_VALUE_W-1:0]
fit_directed_rob_value_or_fatal(input int unsigned value,
                                input string caller_context);
    longint unsigned exclusive_limit;
    longint unsigned promoted_value;

    if (MEMBLOCK_ROB_VALUE_W == 0) begin
        `uvm_fatal("MEMBLOCK_ROB_FIT", $sformatf("%s ROB value width is zero", caller_context))
    end
    if (MEMBLOCK_ROB_VALUE_W >= $bits(longint unsigned)) begin
        `uvm_fatal("MEMBLOCK_ROB_FIT",
                   $sformatf("%s ROB value width=%0d cannot form a 64-bit exclusive limit",
                             caller_context, MEMBLOCK_ROB_VALUE_W))
    end

    exclusive_limit = 64'd1 << MEMBLOCK_ROB_VALUE_W;
    promoted_value = value;
    if (promoted_value >= exclusive_limit) begin
        `uvm_fatal("MEMBLOCK_ROB_FIT",
                   $sformatf("%s ROB value=%0d exceeds width=%0d maximum=%0d",
                             caller_context, value, MEMBLOCK_ROB_VALUE_W, exclusive_limit - 1))
    end
    return MEMBLOCK_ROB_VALUE_W'(value);
endfunction:fit_directed_rob_value_or_fatal

// LSUOpType constants from src/main/scala/xiangshan/package.scala.
localparam bit [8:0] MEMBLOCK_LSUOP_LB          = 9'd0;
localparam bit [8:0] MEMBLOCK_LSUOP_LH          = 9'd1;
localparam bit [8:0] MEMBLOCK_LSUOP_LW          = 9'd2;
localparam bit [8:0] MEMBLOCK_LSUOP_LD          = 9'd3;
localparam bit [8:0] MEMBLOCK_LSUOP_LBU         = 9'd4;
localparam bit [8:0] MEMBLOCK_LSUOP_LHU         = 9'd5;
localparam bit [8:0] MEMBLOCK_LSUOP_LWU         = 9'd6;
localparam bit [8:0] MEMBLOCK_LSUOP_SB          = 9'd0;
localparam bit [8:0] MEMBLOCK_LSUOP_SH          = 9'd1;
localparam bit [8:0] MEMBLOCK_LSUOP_SW          = 9'd2;
localparam bit [8:0] MEMBLOCK_LSUOP_SD          = 9'd3;
localparam bit [8:0] MEMBLOCK_LSUOP_PREFETCH_I  = 9'd8;
localparam bit [8:0] MEMBLOCK_LSUOP_PREFETCH_R  = 9'd9;
localparam bit [8:0] MEMBLOCK_LSUOP_PREFETCH_W  = 9'd10;
localparam bit [8:0] MEMBLOCK_LSUOP_CBO_ZERO    = 9'd7;
localparam bit [8:0] MEMBLOCK_LSUOP_CBO_CLEAN   = 9'd12;
localparam bit [8:0] MEMBLOCK_LSUOP_CBO_FLUSH   = 9'd13;
localparam bit [8:0] MEMBLOCK_LSUOP_CBO_INVAL   = 9'd14;
localparam bit [8:0] MEMBLOCK_LSUOP_LR_W        = 9'd2;
localparam bit [8:0] MEMBLOCK_LSUOP_SC_W        = 9'd6;
localparam bit [8:0] MEMBLOCK_LSUOP_AMOSWAP_W   = 9'd10;
localparam bit [8:0] MEMBLOCK_LSUOP_AMOADD_W    = 9'd14;
localparam bit [8:0] MEMBLOCK_LSUOP_AMOXOR_W    = 9'd18;
localparam bit [8:0] MEMBLOCK_LSUOP_AMOAND_W    = 9'd22;
localparam bit [8:0] MEMBLOCK_LSUOP_AMOOR_W     = 9'd26;
localparam bit [8:0] MEMBLOCK_LSUOP_AMOMIN_W    = 9'd30;
localparam bit [8:0] MEMBLOCK_LSUOP_AMOMAX_W    = 9'd34;
localparam bit [8:0] MEMBLOCK_LSUOP_AMOMINU_W   = 9'd38;
localparam bit [8:0] MEMBLOCK_LSUOP_AMOMAXU_W   = 9'd42;
localparam bit [8:0] MEMBLOCK_LSUOP_AMOCAS_W    = 9'd46;
localparam bit [8:0] MEMBLOCK_LSUOP_LR_D        = 9'd3;
localparam bit [8:0] MEMBLOCK_LSUOP_SC_D        = 9'd7;
localparam bit [8:0] MEMBLOCK_LSUOP_AMOSWAP_D   = 9'd11;
localparam bit [8:0] MEMBLOCK_LSUOP_AMOADD_D    = 9'd15;
localparam bit [8:0] MEMBLOCK_LSUOP_AMOXOR_D    = 9'd19;
localparam bit [8:0] MEMBLOCK_LSUOP_AMOAND_D    = 9'd23;
localparam bit [8:0] MEMBLOCK_LSUOP_AMOOR_D     = 9'd27;
localparam bit [8:0] MEMBLOCK_LSUOP_AMOMIN_D    = 9'd31;
localparam bit [8:0] MEMBLOCK_LSUOP_AMOMAX_D    = 9'd35;
localparam bit [8:0] MEMBLOCK_LSUOP_AMOMINU_D   = 9'd39;
localparam bit [8:0] MEMBLOCK_LSUOP_AMOMAXU_D   = 9'd43;
localparam bit [8:0] MEMBLOCK_LSUOP_AMOCAS_D    = 9'd47;
localparam bit [8:0] MEMBLOCK_LSUOP_AMOCAS_Q    = 9'd44;
localparam bit [5:0] MEMBLOCK_LSUOP_AMOCAS_W_LO = 6'd46;
localparam bit [5:0] MEMBLOCK_LSUOP_AMOCAS_D_LO = 6'd47;
localparam bit [5:0] MEMBLOCK_LSUOP_AMOCAS_Q_LO = 6'd44;

typedef int unsigned memblock_uid_t;
typedef bit [MEMBLOCK_DUT_NUM_LS_ELEM_W-1:0] memblock_num_ls_elem_t;

typedef struct {
    // 从0开始连续terminal_done后的第一个uid；route/redirect/reissue都从这里开始扫描。
    memblock_uid_t terminal_done_uid;
    // 当前连续有效LSQ admission高水位；redirect后会回退到最老flush uid的前一个uid。
    memblock_uid_t max_enqueued_uid;
    // max_enqueued_uid是否有效；还没有任何uid成功admission时为0。
    bit            max_enqueued_uid_valid;
} memblock_dispatch_progress_t;

// Circular pointer key used by DUT-facing ROB/LQ/SQ indices.
// Do not compare value alone; helpers must account for flag/wrap semantics.
typedef struct packed {
    bit       flag;
    bit [MEMBLOCK_ROB_VALUE_W-1:0] value;
} memblock_rob_key_t;

typedef struct packed {
    bit       flag;
    bit [MEMBLOCK_LQ_VALUE_W-1:0] value;
} memblock_lq_key_t;

typedef struct packed {
    bit       flag;
    bit [MEMBLOCK_SQ_VALUE_W-1:0] value;
} memblock_sq_key_t;

// Packed associative-array key form: {flag, value}, width is VALUE_W + 1.
typedef bit [MEMBLOCK_ROB_VALUE_W:0] memblock_rob_map_key_t;
typedef bit [MEMBLOCK_LQ_VALUE_W:0]  memblock_lq_map_key_t;
typedef bit [MEMBLOCK_SQ_VALUE_W:0]  memblock_sq_map_key_t;

typedef struct packed {
    bit                valid;
    bit                flush_itself;
    bit                level;
    memblock_rob_key_t rob_key;
} memblock_redirect_payload_t;

typedef enum int unsigned {
    MEMBLOCK_REDIRECT_PHASE_IDLE                = 0,
    MEMBLOCK_REDIRECT_PHASE_DETECTED            = 1,
    MEMBLOCK_REDIRECT_PHASE_FREEZE_REQUESTED    = 2,
    MEMBLOCK_REDIRECT_PHASE_REDIRECT_DRIVEN     = 3,
    MEMBLOCK_REDIRECT_PHASE_STATE_FLUSH_APPLIED = 4
} memblock_redirect_phase_e;

typedef struct packed {
    bit [51:0] vpn;
    bit [15:0] asid;
    bit [15:0] vmid;
    bit [1:0]  s2xlate;
} memblock_tlb_lookup_key_t;

typedef enum int unsigned {
    // HS/S-stage SFENCE.VMA，只作用于 noS2xlate 的 S1 entry。
    MEMBLOCK_SFENCE_TARGET_HS_S1 = 0,
    // 虚拟态 SFENCE.VMA 或 HFENCE.VVMA，作用于 VS S1 entry。
    MEMBLOCK_SFENCE_TARGET_VS_S1 = 1,
    // HFENCE.GVMA，作用于 G-stage S2 entry。
    MEMBLOCK_SFENCE_TARGET_G_S2  = 2
} memblock_sfence_target_stage_e;

typedef struct {
    bit               valid;
    bit               ignore_addr;
    bit               ignore_id;
    bit [49:0]        addr;
    bit [15:0]        id;
    bit               hv;
    bit               hg;
    // 中文注释：target_stage 和 S1/S2 地址均在 raw fence 采样时确定。
    // matcher 只读这些冻结字段，不能在 drain 时从 current CSR 重算 stage。
    memblock_sfence_target_stage_e target_stage;
    bit [37:0]        s1_vpn;
    bit [51:0]        s2_gvpn;
    bit               priv_virt_at_sample;
    bit [15:0]        hgatp_vmid_at_sample;
    bit [3:0]         satp_mode_at_sample;
    bit [3:0]         vsatp_mode_at_sample;
    bit [3:0]         hgatp_mode_at_sample;
    longint unsigned  sample_seq;
    longint unsigned  reset_epoch;
    longint unsigned  lifecycle_event_seq;
    longint unsigned  cycle;
} memblock_sfence_payload_t;

typedef enum int unsigned {
    MEMBLOCK_OP_CLASS_UNKNOWN  = 0,
    MEMBLOCK_OP_CLASS_INT_LOAD = 1,
    MEMBLOCK_OP_CLASS_FP_LOAD  = 2,
    MEMBLOCK_OP_CLASS_STORE    = 3,
    MEMBLOCK_OP_CLASS_PREFETCH = 4,
    MEMBLOCK_OP_CLASS_AMO      = 5,
    MEMBLOCK_OP_CLASS_CBO      = 6
} memblock_op_class_e;

// boundary_profile 是主表生成侧的地址边界标签，只描述激励构造目标。
// DUT结果正确性、coverage命中和RM对比不读取该字段做通过/失败判断。
typedef enum int unsigned {
    MEMBLOCK_BOUNDARY_PROFILE_UNKNOWN                 = 0,
    MEMBLOCK_BOUNDARY_PROFILE_ALIGNED                 = 1,
    MEMBLOCK_BOUNDARY_PROFILE_MISALIGN_WITHIN_8B      = 2,
    MEMBLOCK_BOUNDARY_PROFILE_CROSS_8B_WITHIN_16B     = 3,
    MEMBLOCK_BOUNDARY_PROFILE_CROSS_16B_SAME_LINE     = 4,
    MEMBLOCK_BOUNDARY_PROFILE_CROSS_CACHELINE_SAME_4K = 5,
    MEMBLOCK_BOUNDARY_PROFILE_CROSS_4K                = 6
} memblock_boundary_profile_e;

typedef struct {
    bit [8:0]                  fuOpType;
    int unsigned               size_bytes;
    int unsigned               cfg_fuop_weight;
    int unsigned               effective_weight;
    bit                        use_default;
} memblock_boundary_fuop_candidate_t;

typedef struct {
    memblock_op_class_e        op_class;
    int unsigned               op_class_weight;
    memblock_boundary_fuop_candidate_t fuop_cache[$];
} memblock_boundary_op_candidate_t;

typedef struct {
    memblock_boundary_profile_e profile;
    int unsigned                profile_weight;
    memblock_boundary_op_candidate_t op_cache[$];
} memblock_boundary_profile_candidate_t;

typedef enum int unsigned {
    MEMBLOCK_ADDR_REUSE_LOAD_AFTER_STORE  = 0,
    MEMBLOCK_ADDR_REUSE_LOAD_AFTER_LOAD   = 1,
    MEMBLOCK_ADDR_REUSE_STORE_AFTER_LOAD  = 2,
    MEMBLOCK_ADDR_REUSE_STORE_AFTER_STORE = 3
} memblock_addr_reuse_kind_e;

typedef enum int unsigned {
    MEMBLOCK_LSQ_FLOW_NONE   = 0,
    MEMBLOCK_LSQ_FLOW_LOAD   = 1,
    MEMBLOCK_LSQ_FLOW_STORE  = 2,
    MEMBLOCK_LSQ_FLOW_ATOMIC = 3,
    MEMBLOCK_LSQ_FLOW_CBO    = 4
} memblock_lsq_flow_e;

typedef enum int unsigned {
    MEMBLOCK_ISSUE_TARGET_NONE = 0,
    MEMBLOCK_ISSUE_TARGET_LOAD = 1,
    MEMBLOCK_ISSUE_TARGET_STA  = 2,
    MEMBLOCK_ISSUE_TARGET_STD  = 3
} memblock_issue_target_e;

typedef enum int unsigned {
    MEMBLOCK_WB_EVENT_SOURCE_NONE             = 0,
    MEMBLOCK_WB_EVENT_SOURCE_LOAD_WB          = 1,
    MEMBLOCK_WB_EVENT_SOURCE_ATOMIC_WB        = 2,
    MEMBLOCK_WB_EVENT_SOURCE_STORE_WB         = 3,
    MEMBLOCK_WB_EVENT_SOURCE_SQ_WB            = 4,
    MEMBLOCK_WB_EVENT_SOURCE_STA_FEEDBACK     = 5,
    MEMBLOCK_WB_EVENT_SOURCE_STD_FEEDBACK     = 6,
    MEMBLOCK_WB_EVENT_SOURCE_EXCEPTION_INFO   = 7,
    MEMBLOCK_WB_EVENT_SOURCE_MEMORY_VIOLATION = 8,
    MEMBLOCK_WB_EVENT_SOURCE_BACKEND_REPLAY   = 9,
    MEMBLOCK_WB_EVENT_SOURCE_REDIRECT         = 10
} memblock_wb_event_source_e;

typedef enum bit [2:0] {
    MEMBLOCK_OP_BEHAVIOR_UNKNOWN  = 3'd0,
    MEMBLOCK_OP_BEHAVIOR_LOAD     = 3'd1,
    MEMBLOCK_OP_BEHAVIOR_PREFETCH = 3'd2,
    MEMBLOCK_OP_BEHAVIOR_STORE    = 3'd3,
    MEMBLOCK_OP_BEHAVIOR_CBO      = 3'd4,
    MEMBLOCK_OP_BEHAVIOR_ATOMIC   = 3'd5
} memblock_op_behavior_kind_e;

typedef enum bit [1:0] {
    // 当前动态实例没有 MMIO 属性；不得作为 setter 的有效 kind。
    MEMBLOCK_MMIO_KIND_NONE  = 2'd0,
    // scalar load 的 MMIO 属性；只允许绑定到 active load 动态实例。
    MEMBLOCK_MMIO_KIND_LOAD  = 2'd1,
    // scalar store 的 MMIO 属性；只允许绑定到 active store 动态实例。
    MEMBLOCK_MMIO_KIND_STORE = 2'd2
} memblock_mmio_kind_e;

typedef enum bit [1:0] {
    // 空 tag 的中性来源；有效 tag 不允许使用该值。
    MEMBLOCK_MMIO_TAG_NONE     = 2'd0,
    // software-only directed 场景通过 canonical API 预置的属性。
    MEMBLOCK_MMIO_TAG_DIRECTED = 2'd1,
    // ctrl monitor 的真实 loadMmio/storeMmio output，经 adapter 归一化后的属性。
    MEMBLOCK_MMIO_TAG_MONITOR  = 2'd2
} memblock_mmio_tag_source_e;

typedef enum bit {
    // raw ROB value 唯一归属于当前 active 动态实例，可以进入 tag staging。
    MEMBLOCK_MMIO_RESOLVE_CURRENT    = 1'b0,
    // raw 可证明早于当前动态实例或属于已经消失的旧 epoch，只丢弃该 port。
    MEMBLOCK_MMIO_RESOLVE_STALE_DROP = 1'b1
} memblock_mmio_resolve_result_e;

typedef struct packed {
    memblock_op_behavior_kind_e kind;
    bit [1:0]                   need_alloc;
    bit                         uses_lq;
    bit                         uses_sq;
    bit                         route_load;
    bit                         route_sta;
    bit                         route_std;
    // Commit classification flags.  The current dispatch framework mainly
    // uses need_alloc/uses_lq/uses_sq to choose the LSQ admission path.
    // commit_is_normal marks operations that are not modeled as ordinary
    // load-commit or store-commit LSQ users, such as the current simplified
    // MOU/atomic behavior.
    bit                         commit_is_load;
    bit                         commit_is_store;
    bit                         commit_is_normal;
    bit                         is_prefetch;
    bit                         is_cbo;
    bit                         is_atomic;
    memblock_num_ls_elem_t      num_ls_elem;
    bit [2:0]                   atomic_sta_uop_count;
    bit [2:0]                   atomic_data_uop_count;
} memblock_op_behavior_t;

typedef struct {
    memblock_uid_t            uid;
    memblock_rob_key_t        rob_key;
    memblock_issue_target_e   target;
    int unsigned              send_pri;
    longint unsigned          ready_cycle;
    int unsigned              replay_seq;
    bit                       has_lqIdx;
    memblock_lq_key_t         lq_key;
    bit                       has_sqIdx;
    memblock_sq_key_t         sq_key;
    memblock_num_ls_elem_t    numLsElem;
    int unsigned              uop_index;
    int unsigned              uop_count;
} memblock_issue_q_item_t;

typedef struct {
    bit                       valid;
    memblock_uid_t            uid;
    memblock_issue_target_e   target;
    int unsigned              issue_epoch;
    int unsigned              replay_seq;
    longint unsigned          start_cycle;
} memblock_ptw_wait_replay_t;

typedef struct {
    // flushSb请求编号只用于日志/debug，不参与DUT接口赋值。
    int unsigned              req_id;
    // 请求入队时的dispatch service cycle，用于定位请求滞留时间。
    longint unsigned          enqueue_cycle;
    // 请求来源标签：0=directed/unknown，1=periodic，后续可扩展其它producer。
    int unsigned              source;
} memblock_flushsb_req_t;

typedef enum bit [1:0] {
    MEMBLOCK_LSQ_RESERVATION_NONE = 2'd0,
    MEMBLOCK_LSQ_RESERVATION_LAUNCHED_PENDING_SAMPLE = 2'd1,
    MEMBLOCK_LSQ_RESERVATION_DUT_VISIBLE = 2'd2,
    MEMBLOCK_LSQ_RESERVATION_CANCEL_ACCOUNTED = 2'd3
} memblock_lsq_reservation_state_e;

// 中文伪代码：真实 LSQ launch 后创建 token；下一采样边界把同一 token 标记为 DUT_VISIBLE。
typedef struct {
    bit          valid;
    memblock_uid_t uid;
    int unsigned launch_epoch;
} memblock_lsq_reservation_token_t;

// 中文注释：每个 framework redirect epoch 唯一的 cancel 对账记录。
// request_redirect_flush() 创建，redirect monitor anchor 按 FIFO 绑定；active scan 写
// software count，LSQ enqueue sequence 只应用该 count，main service 只比较 DUT snapshot。
// software_applied 与 observed_valid 是独立进度，两者均完成后才允许从 FIFO 头删除。
typedef struct {
    bit                         valid;
    int unsigned                redirect_epoch;
    int unsigned                cancel_record_id;
    memblock_redirect_payload_t redirect;
    longint unsigned            redirect_service_cycle;
    bit                         redirect_drive_done_valid;
    longint unsigned            redirect_drive_done_service_cycle;
    longint unsigned            state_flush_applied_service_cycle;
    longint unsigned            anchor_deadline_service_cycle;
    bit                         redirect_anchor_valid;
    longint unsigned            redirect_sample_seq;
    longint unsigned            redirect_lsq_sample_seq;
    longint unsigned            dut_cancel_update_sample_seq;
    longint unsigned            compare_snapshot_sample_seq;
    longint unsigned            deadline_sample_seq;
    int unsigned                software_cancel_lq_count;
    int unsigned                software_cancel_sq_count;
    bit                         active_scan_done;
    bit                         software_count_finalized;
    bit                         software_applied;
    bit                         observed_valid;
    int unsigned                observed_cancel_lq_count;
    int unsigned                observed_cancel_sq_count;
} memblock_cancel_reconcile_t;

typedef memblock_cancel_reconcile_t memblock_lsq_cancel_record_t;

typedef struct {
    bit                         valid;
    memblock_wb_event_source_e  source;
    int unsigned                port_id;
    memblock_issue_target_e     target;
    memblock_uid_t              uid;
    bit                         has_uid;
    memblock_rob_key_t          rob_key;
    bit                         has_rob;
    memblock_lq_key_t           lq_key;
    bit                         has_lq;
    memblock_sq_key_t           sq_key;
    bit                         has_sq;
    int unsigned                issue_epoch;
    bit                         has_issue_epoch;
    int unsigned                replay_seq;
    bit                         has_replay_seq;
    // 中文注释：DUT真实 int writeback/pass/fault 有效标志，只能由真实 int writeback 置位。
    // IQ feedback hit 不允许写该字段；handler 只在真实 writeback 分支用它更新 target writeback/pass。
    bit                         real_wb_valid;
    bit                         has_exception;
    bit [23:0]                  exception_vec;
    // 中文注释：DUT IssueQueue feedback 有效标志。置位后只表示本次 issue response 已返回，不等价于 ROB/RF writeback。
    // 由 convert_raw_iq_feedback() 设置；handler 按 target 的兼容策略决定是否只记录 feedback done。
    bit                         iq_feedback_valid;
    // 中文注释：IssueQueue feedback hit/finalSuccess。为1时表示该 target 本次 issue 被 IQ 接受成功。
    // STA 可由既有兼容开关决定是否只更新 issue_feedback_success；STD 永不作为 pass 来源。
    bit                         iq_feedback_hit;
    // 中文注释：IssueQueue feedback failed。为1时表示该 target 本次 issue 失败；当前 STA failed 转 replay，STD 进入严格拒绝路径。
    bit                         iq_feedback_failed;
    // 中文注释：IssueQueue feedback flush_state 原始语义保留位，用于区分 PTW/TLB back replay 等状态来源。
    // 它本身不代表真实 writeback，也不单独生成 pass。
    bit                         iq_feedback_flush_state;
    bit                         replay_valid;
    bit                         redirect_valid;
    memblock_redirect_payload_t redirect;
    bit                         ptw_back_replay;
    bit                         vector_ls;
    int unsigned                uop_index;
    longint unsigned            cycle;
} memblock_wb_event_t;

//   | uop_index | 拆分后的 micro-op 编号 | 预留给 atomic/vector 或多 uop 操作定位具体子 uop。当前普通标量 load/sta/std 基本都是 0，主流程不依赖它做状态转移。 |
//   | ptw_back_replay | 由 raw feedback monitor 的 flush_state 派生出来的“PTW-back replay”标志 | 当前逻辑大致是 STA && !hit && flush_state 时置高。wb_event 只保存该语义化字段；它才是真正参与 replay 控制的字段：如果开启等待 PTW/L2TLB 相关流程，会先进入 PTW wait replay，而不是立刻重新入队。 |
//   | vector_ls | 表示该 writeback/feedback 来自 vector load/store | 当前框架初版不支持 vector LS writeback/replay，检测到会直接 fatal，避免误按 scalar load/store 流程处理。 |

typedef enum int unsigned {
    MEMBLOCK_STATUS_ACTIVE             = 0,
    MEMBLOCK_STATUS_ENQ                = 1,
    MEMBLOCK_STATUS_TLB_MAPPED         = 2,
    MEMBLOCK_STATUS_QUEUED_LOAD        = 3,
    MEMBLOCK_STATUS_QUEUED_STA         = 4,
    MEMBLOCK_STATUS_QUEUED_STD         = 5,
    MEMBLOCK_STATUS_LOAD_DISPATCHED    = 6,
    MEMBLOCK_STATUS_STA_DISPATCHED     = 7,
    MEMBLOCK_STATUS_STD_DISPATCHED     = 8,
    MEMBLOCK_STATUS_WRITEBACK          = 9,
    MEMBLOCK_STATUS_PASS               = 10,
    MEMBLOCK_STATUS_FAULT              = 11,
    MEMBLOCK_STATUS_EXCEPTION_PENDING  = 12,
    MEMBLOCK_STATUS_REPLAY_PENDING     = 13,
    MEMBLOCK_STATUS_REDIRECT_PENDING   = 14,
    MEMBLOCK_STATUS_FLUSHED            = 15,
    MEMBLOCK_STATUS_ISSUE_READY        = 16,
    MEMBLOCK_STATUS_ROB_COMMIT         = 17,
    MEMBLOCK_STATUS_LSQ_DEQ            = 18,
    MEMBLOCK_STATUS_SUCCESS            = 19,
    MEMBLOCK_STATUS_TERMINAL_DONE      = 20,
    MEMBLOCK_STATUS_LOAD_WRITEBACK     = 21,
    MEMBLOCK_STATUS_STA_WRITEBACK      = 22,
    MEMBLOCK_STATUS_STD_WRITEBACK      = 23,
    MEMBLOCK_STATUS_LOAD_PASS          = 24,
    MEMBLOCK_STATUS_STA_PASS           = 25,
    MEMBLOCK_STATUS_STD_PASS           = 26,
    MEMBLOCK_STATUS_LOAD_FAULT         = 27,
    MEMBLOCK_STATUS_STA_FAULT          = 28,
    MEMBLOCK_STATUS_STD_FAULT          = 29
} memblock_status_field_e;

`endif
