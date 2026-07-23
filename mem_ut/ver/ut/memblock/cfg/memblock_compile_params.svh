//=========================================================
//File name    : memblock_compile_params.svh
//Author       : OpenAI_Codex
//Module name  : memblock_compile_params
//Discribution : compile-time parameters for memblock UT
//Date         : 2026-05-24
//=========================================================
`ifndef MEMBLOCK_COMPILE_PARAMS__SVH
`define MEMBLOCK_COMPILE_PARAMS__SVH

// V2默认参数：当前worktree生成的MemBlock顶层ROB value为8 bit，
// XSCoreParameters.RobSize为160；LQ/SQ value分别为7/6 bit。
// 这些宏描述DUT接口与公共状态表宽度，V3环境如需复用可在编译期覆盖。
`ifndef MEMBLOCK_DUT_ROB_SIZE
    `define MEMBLOCK_DUT_ROB_SIZE 160
`endif
`ifndef MEMBLOCK_DUT_LQ_SIZE
    `define MEMBLOCK_DUT_LQ_SIZE 72
`endif
`ifndef MEMBLOCK_DUT_SQ_SIZE
    `define MEMBLOCK_DUT_SQ_SIZE 56
`endif
`ifndef MEMBLOCK_DUT_COMMIT_WIDTH
    `define MEMBLOCK_DUT_COMMIT_WIDTH 8
`endif

// V2物理资源数量：LSQ enqueue为6个slot，scalar issue为3/2/2条LOAD/STA/STD pipe。
// 当前LSQ字段链显式展开6个slot，非6/6/4覆盖会被compile consistency检查拒绝；
// 后续只有在其它profile同步参数化全部显式consumer后才能放开该tuple。
`ifndef MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM
    `define MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM 6
`endif
`ifndef MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH
    `define MEMBLOCK_DUT_LSQ_LD_ENQ_WIDTH 6
`endif
`ifndef MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH
    `define MEMBLOCK_DUT_LSQ_ST_ENQ_WIDTH 4
`endif
`ifndef MEMBLOCK_DUT_LOAD_PIPE_NUM
    `define MEMBLOCK_DUT_LOAD_PIPE_NUM 3
`endif
`ifndef MEMBLOCK_DUT_STA_PIPE_NUM
    `define MEMBLOCK_DUT_STA_PIPE_NUM 2
`endif
`ifndef MEMBLOCK_DUT_STD_PIPE_NUM
    `define MEMBLOCK_DUT_STD_PIPE_NUM 2
`endif

`ifndef MEMBLOCK_DUT_ROB_VALUE_W
    `define MEMBLOCK_DUT_ROB_VALUE_W 8
`endif
`ifndef MEMBLOCK_DUT_LQ_VALUE_W
    `define MEMBLOCK_DUT_LQ_VALUE_W 7
`endif
`ifndef MEMBLOCK_DUT_SQ_VALUE_W
    `define MEMBLOCK_DUT_SQ_VALUE_W 6
`endif

// V2 MicroOp字段的结构上限。派生宽度不得单独覆盖，避免interface和sequence使用不同权威。
`ifndef MEMBLOCK_DUT_MAX_UOP_SIZE
    `define MEMBLOCK_DUT_MAX_UOP_SIZE 65
`endif
`define MEMBLOCK_DUT_UOP_IDX_W ($clog2(`MEMBLOCK_DUT_MAX_UOP_SIZE + 1))
`ifndef MEMBLOCK_DUT_VLEN
    `define MEMBLOCK_DUT_VLEN 128
`endif
`define MEMBLOCK_DUT_MAX_LS_ELEM (`MEMBLOCK_DUT_VLEN / 8)
`define MEMBLOCK_DUT_NUM_LS_ELEM_W ($clog2(`MEMBLOCK_DUT_MAX_LS_ELEM) + 1)

// V2默认FuType编码与DUT端口宽度。内部容器保留跨V2/V3最大36 bit，
// DUT-facing端口宽度和one-hot位置只能由当前版本profile在编译期覆盖。
`ifndef MEMBLOCK_INTERNAL_FUTYPE_W
    `define MEMBLOCK_INTERNAL_FUTYPE_W 36
`endif
`ifndef MEMBLOCK_DUT_FUTYPE_W
    `define MEMBLOCK_DUT_FUTYPE_W 35
`endif
`ifndef MEMBLOCK_DUT_FUTYPE_LDU_BIT
    `define MEMBLOCK_DUT_FUTYPE_LDU_BIT 15
`endif
`ifndef MEMBLOCK_DUT_FUTYPE_STU_BIT
    `define MEMBLOCK_DUT_FUTYPE_STU_BIT 16
`endif
`ifndef MEMBLOCK_DUT_FUTYPE_MOU_BIT
    `define MEMBLOCK_DUT_FUTYPE_MOU_BIT 17
`endif
`ifndef MEMBLOCK_DUT_FUTYPE_VLDU_BIT
    `define MEMBLOCK_DUT_FUTYPE_VLDU_BIT 31
`endif
`ifndef MEMBLOCK_DUT_FUTYPE_VSTU_BIT
    `define MEMBLOCK_DUT_FUTYPE_VSTU_BIT 32
`endif
`ifndef MEMBLOCK_DUT_FUTYPE_VSEGLDU_BIT
    `define MEMBLOCK_DUT_FUTYPE_VSEGLDU_BIT 33
`endif
`ifndef MEMBLOCK_DUT_FUTYPE_VSEGSTU_BIT
    `define MEMBLOCK_DUT_FUTYPE_VSEGSTU_BIT 34
`endif

// V2默认FTQ字段、MMIO load输出数量和scalar issue端口布局。
// 这些值决定packed字段或物理端口解释，runtime plus不得修改。
`ifndef MEMBLOCK_DUT_FTQ_PTR_VALUE_W
    `define MEMBLOCK_DUT_FTQ_PTR_VALUE_W 6
`endif
`ifndef MEMBLOCK_DUT_FTQ_OFFSET_W
    `define MEMBLOCK_DUT_FTQ_OFFSET_W 4
`endif
`ifndef MEMBLOCK_DUT_LOAD_PORT_BASE
`define MEMBLOCK_DUT_LOAD_PORT_BASE 0
`endif
`ifndef MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM
    `define MEMBLOCK_DUT_MMIO_LOAD_PORT_NUM 3
`endif
`ifndef MEMBLOCK_DUT_ISSUE_PORT_STYLE_SPLIT
    `define MEMBLOCK_DUT_ISSUE_PORT_STYLE_SPLIT 1
`endif

// V2顶层没有LSQ enqueue accept response，也没有SQ deq pointer。
// 后续专项只能按该编译期能力选择字段，禁止runtime探测不存在端口。
`ifndef MEMBLOCK_DUT_LSQ_ENQ_HAS_ACCEPT_RESP
    `define MEMBLOCK_DUT_LSQ_ENQ_HAS_ACCEPT_RESP 0
`endif
`ifndef MEMBLOCK_DUT_HAS_SQ_DEQ_PTR
    `define MEMBLOCK_DUT_HAS_SQ_DEQ_PTR 0
`endif

// V2 StoreQueue 的 sqDeq 是 entry count，不是 SQ pointer。count width 必须
// 与 EnsbufferWidth 独立派生；cancel count width 则分别由 LQ/SQ 容量派生。
`ifndef MEMBLOCK_DUT_ENSBUFFER_WIDTH
    `define MEMBLOCK_DUT_ENSBUFFER_WIDTH 2
`endif
`define MEMBLOCK_SQ_DEQ_COUNT_W ($clog2(`MEMBLOCK_DUT_ENSBUFFER_WIDTH + 1))
`define MEMBLOCK_LQ_CANCEL_COUNT_W ($clog2(`MEMBLOCK_DUT_LQ_SIZE + 1))
`define MEMBLOCK_SQ_CANCEL_COUNT_W ($clog2(`MEMBLOCK_DUT_SQ_SIZE + 1))

// Redirect cancel 对账使用编译期 RTL/monitor 时序合同，不提供 runtime plus 镜像。
// 中文注释：以下值描述 profile 的结构/采样合同；sequence 只能读取派生结果，不能
// 在运行期重新解释或建立同义 plus 参数。
`ifndef MEMBLOCK_DUT_REDIRECT_TO_LSQ_LATENCY
    `define MEMBLOCK_DUT_REDIRECT_TO_LSQ_LATENCY 1
`endif
`ifndef MEMBLOCK_DUT_CANCEL_OUTPUT_LATENCY
    `define MEMBLOCK_DUT_CANCEL_OUTPUT_LATENCY 2
`endif
`ifndef MEMBLOCK_TB_CANCEL_MONITOR_SAMPLE_OFFSET
    `define MEMBLOCK_TB_CANCEL_MONITOR_SAMPLE_OFFSET 1
`endif
`define MEMBLOCK_CANCEL_SNAPSHOT_OBSERVE_LATENCY \
    (`MEMBLOCK_DUT_CANCEL_OUTPUT_LATENCY + `MEMBLOCK_TB_CANCEL_MONITOR_SAMPLE_OFFSET)
`define MEMBLOCK_CANCEL_RECORD_MAX_DEPTH \
    (`MEMBLOCK_CANCEL_SNAPSHOT_OBSERVE_LATENCY + 2)
`define MEMBLOCK_CANCEL_SNAPSHOT_QUEUE_MAX_DEPTH \
    (2 * `MEMBLOCK_CANCEL_SNAPSHOT_OBSERVE_LATENCY + 8)

// V2 L2TLB/DTLB responder结构合同。DFILTER_SIZE限制可接受request总数；
// FLUSH_HOLD_CYCLES覆盖顶层CSR/sfence观测点到DTLB filter清空点的总延迟。
// 两者只允许由版本compile profile覆盖，不建立runtime plus镜像。
`ifndef MEMBLOCK_DUT_L2TLB_DFILTER_SIZE
    `define MEMBLOCK_DUT_L2TLB_DFILTER_SIZE 32
`endif
`ifndef MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES
    `define MEMBLOCK_DUT_L2TLB_FLUSH_HOLD_CYCLES 4
`endif

// L2TLB connect-time takeover switch.
// 1: mem_ut L2TLB_agent owns the DTLB <-> L2TLB response path.
//    V2 takes over the internal dtlbRepeater <-> inner_ptw/L2TLB path by default.
// 0: keep L2TLB_agent inactive; this mode is not a passive observation connection.
// This is a compile-time connection decision; runtime sequence enable remains in plus/seq_csr_common.
`ifndef MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN
    `define MEMBLOCK_L2TLB_CONNECT_TAKEOVER_EN 1
`endif

`endif
