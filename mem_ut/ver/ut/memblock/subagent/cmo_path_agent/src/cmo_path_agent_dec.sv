`ifndef CMO_PATH_AGENT_DEC_SV
`define CMO_PATH_AGENT_DEC_SV
`include "memblock_compile_params.svh"
package cmo_path_agent_dec;
    // 事件只表示真实接口接受/流水采样，不根据地址猜测CBO或预期异常。
    typedef enum logic [7:0] {
        CMO_PATH_SQ_ENQUEUE,
        CMO_PATH_SQ_SBUFFER,
        CMO_PATH_SB_ACCESS,
        CMO_PATH_SB_DCACHE_REQUEST,
        CMO_PATH_SB_DCACHE_SUCCESS,
        CMO_PATH_SB_DCACHE_REPLAY,
        CMO_PATH_SQ_CMO_REQUEST,
        CMO_PATH_SQ_UNCACHE_REQUEST,
        CMO_PATH_UNCACHE_ALLOC,
        CMO_PATH_ERROR_ARRAY_WRITE,
        CMO_PATH_ERROR_ARRAY_READ,
        CMO_PATH_LOAD_PIPE_HIT,
        CMO_PATH_MAIN_PIPE_S1,
        CMO_PATH_MAIN_PIPE_REFILL,
        CMO_PATH_MAIN_PIPE_S2,
        CMO_PATH_LOAD_FORWARD_SOURCE,
        CMO_PATH_LOAD_S2,
        CMO_PATH_LOAD_S3,
        CMO_PATH_ATOMIC_REQUEST,
        CMO_PATH_ATOMIC_RESPONSE,
        CMO_PATH_ATOMIC_LATCH_CONTROL,
        CMO_PATH_MAIN_ATOMIC_RESPONSE,
        CMO_PATH_META_WRITE,
        CMO_PATH_TAG_WRITE,
        CMO_PATH_DCACHE_TL_A,
        CMO_PATH_DCACHE_TL_D,
        CMO_PATH_UNCACHE_TL_A,
        CMO_PATH_UNCACHE_TL_D,
        CMO_PATH_DCACHE_TL_D_OFFER,
        CMO_PATH_UNCACHE_CONSUME_D
    } cmo_path_kind_e;
    // 固定物理观察口：6个SQ enqueue、2个SB输入、3个Load，来自当前V2 profile。
    // 端口数量在connect用真实RTL信号宽度检查；这里不建立runtime配置镜像。
    localparam int CMO_PATH_PROBE_NUM = 48;
    localparam int CMO_PATH_LOAD_PORTS = `MEMBLOCK_DUT_LOAD_PIPE_NUM;
    localparam int CMO_PATH_ERROR_READ_PORTS = CMO_PATH_LOAD_PORTS + 1;
    typedef struct packed {
        // valid仅表示本口事件发生；monitor只发布valid项，空批次仍推进sample水位。
        logic valid;
        cmo_path_kind_e kind;
        logic [7:0] port_id;
        // 原始SQ/ROB/LQ身份，由真实payload或已锁存uop采集；缺失字段保持0。
        // owner_valid表示真实ROB有效；SQ/LQ是否携带按kind区分。SQ->SB仅带sqPtr.value时由唯一adapter关联。
        logic owner_valid;
        logic [8:0] rob;
        logic [6:0] sq;
        logic [7:0] lq;
        logic [8:0] op;
        // 真正接受的地址、数据和字节掩码；wline不以data==0推断。
        logic [63:0] addr;
        logic [511:0] data;
        logic [63:0] mask;
        logic [31:0] id;
        logic [31:0] slot;
        // idx/way来自真实阵列端口，不从PA推算VIPT索引。
        logic [15:0] idx;
        logic [15:0] way;
        // raw[1]=denied、raw[0]=corrupt；raw_extra保存第二项独立贡献，不能选其一。
        logic [1:0] raw;
        logic [1:0] raw_extra;
        logic [31:0] actual_by_way;
        // 独立资格位，具体事件定义在connect；不采集最终exceptionVec作为答案。
        logic [15:0] qual;
        // 状态/附加身份：保存真实miss/replay、首S1或slot分配merge等观察事实。
        logic [31:0] state;
        logic [31:0] aux;
    } cmo_path_probe_t;
endpackage
`endif
