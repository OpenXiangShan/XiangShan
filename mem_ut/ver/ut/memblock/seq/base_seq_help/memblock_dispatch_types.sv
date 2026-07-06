//=========================================================
//File name    : memblock_dispatch_types.sv
//Author       : OpenAI_Codex
//Module name  : memblock_dispatch_types
//Discribution : shared dispatch framework typedefs
//Date         : 2026-05-18
//=========================================================
`ifndef MEMBLOCK_DISPATCH_TYPES__SV
`define MEMBLOCK_DISPATCH_TYPES__SV

localparam int unsigned MEMBLOCK_ROB_SIZE = 352;
localparam int unsigned MEMBLOCK_LQ_SIZE  = 72;
localparam int unsigned MEMBLOCK_SQ_SIZE  = 56;
localparam int unsigned MEMBLOCK_COMMIT_WIDTH = 8;

// Width of the index value field only. The wrap flag is stored separately
// in memblock_*_key_t, and legal values are still bounded by MEMBLOCK_*_SIZE.
localparam int unsigned MEMBLOCK_ROB_VALUE_W = 9;
localparam int unsigned MEMBLOCK_LQ_VALUE_W  = 7;
localparam int unsigned MEMBLOCK_SQ_VALUE_W  = 6;

// FuType one-hot constants from src/main/scala/xiangshan/backend/fu/FuType.scala.
localparam bit [35:0] MEMBLOCK_FUTYPE_LDU     = 36'h0_0001_0000;
localparam bit [35:0] MEMBLOCK_FUTYPE_STU     = 36'h0_0002_0000;
localparam bit [35:0] MEMBLOCK_FUTYPE_MOU     = 36'h0_0004_0000;
localparam bit [35:0] MEMBLOCK_FUTYPE_VLDU    = 36'h1_0000_0000;
localparam bit [35:0] MEMBLOCK_FUTYPE_VSTU    = 36'h2_0000_0000;
localparam bit [35:0] MEMBLOCK_FUTYPE_VSEGLDU = 36'h4_0000_0000;
localparam bit [35:0] MEMBLOCK_FUTYPE_VSEGSTU = 36'h8_0000_0000;

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

typedef struct {
    bit               valid;
    bit               ignore_addr;
    bit               ignore_id;
    bit [49:0]        addr;
    bit [15:0]        id;
    bit               hv;
    bit               hg;
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
    bit [4:0]                   num_ls_elem;
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
    bit [4:0]                 numLsElem;
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
    // 中文注释：DUT真实 int writeback/pass/fault 有效标志，只能由 int writeback 或明确模拟真实写回的 synthetic event 置位。
    // IQ feedback hit 不允许写该字段；handler 只在真实 writeback 分支用它更新 target writeback/pass。
    bit                         real_wb_valid;
    bit                         has_exception;
    bit [23:0]                  exception_vec;
    // 中文注释：DUT IssueQueue feedback 有效标志。置位后只表示本次 issue response 已返回，不等价于 ROB/RF writeback。
    // 由 convert_raw_iq_feedback() 或 issue-accept 兼容路径设置，handler 根据 real_wb pass 配置决定是否仅记录 feedback done。
    bit                         iq_feedback_valid;
    // 中文注释：IssueQueue feedback hit/finalSuccess。为1时表示该 target 本次 issue 被 IQ 接受成功。
    // 当真实 writeback pass 开启时只更新 issue_feedback_success；关闭时才作为兼容 pass 来源。
    bit                         iq_feedback_hit;
    // 中文注释：IssueQueue feedback failed。为1时表示该 target 本次 issue 失败；当前 STA failed 转 replay，STD failed warning/drop。
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
