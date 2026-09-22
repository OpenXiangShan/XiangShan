//=========================================================
// V2 StoreUnit scalar functional coverage.
//
// The first implementation contains only single-cycle P0
// covergroups. Temporal causality stays in the SVA file.
//=========================================================
`ifndef MEMBLOCK_STOREUNIT_SCALAR_FCOV__SV
`define MEMBLOCK_STOREUNIT_SCALAR_FCOV__SV

`ifdef MEMBLOCK_UT_FCOV
module memblock_storeunit_scalar_fcov (
    input wire clock,
    input wire reset,
    input wire reset_backend_done,
    input wire io_redirect_valid,
    input wire io_csrCtrl_hd_misalign_st_enable,
    input wire io_stin_valid,
    input wire io_stin_ready,
    input wire [8:0] io_stin_fuOpType,
    input wire [31:0] io_stin_imm,
    input wire io_stin_robIdx_flag,
    input wire [7:0] io_stin_robIdx_value,
    input wire io_stin_sqIdx_flag,
    input wire [5:0] io_stin_sqIdx_value,
    input wire [63:0] io_stin_src0,
    input wire io_vecstin_valid,
    input wire io_vecstin_ready,
    input wire io_misalign_stin_valid,
    input wire io_misalign_stin_ready,
    input wire s0_valid,
    input wire s0_fire,
    input wire s0_out_isvec,
    input wire [49:0] s0_vaddr,
    input wire [15:0] s0_mask,
    input wire s0_isCbo,
    input wire s0_addr_aligned,
    input wire s0_rs_cross16Bytes,
    input wire s0_misalignWith16Byte,
    input wire s1_valid,
    input wire s1_isvec,
    input wire s1_isFrmMisAlignBuf,
    input wire s1_is128bit,
    input wire s1_isCbo,
    input wire s1_kill,
    input wire s1_tlb_miss,
    input wire s1_tlb_hit,
    input wire [1:0] s1_pbmt,
    input wire s1_misalignNeedReplay,
    input wire [3:0] s1_trigger_action,
    input wire s1_trigger_debug_mode,
    input wire [8:0] s1_fuOpType,
    input wire [15:0] s1_mask,
    input wire io_tlb_req_valid,
    input wire [49:0] io_tlb_req_vaddr,
    input wire io_tlb_req_checkfullva,
    input wire [2:0] io_tlb_req_cmd,
    input wire io_tlb_req_hyperinst,
    input wire io_tlb_req_frm_mabuf,
    input wire [2:0] io_tlb_req_size,
    input wire io_tlb_resp_valid,
    input wire io_tlb_resp_miss,
    input wire [1:0] io_tlb_resp_pbmt,
    input wire io_tlb_resp_pf,
    input wire io_tlb_resp_gpf,
    input wire io_tlb_resp_af,
    input wire io_tlb_resp_vaNeedExt,
    input wire io_tlb_resp_isHyper,
    input wire io_tlb_resp_isForVSnonLeafPTE,
    input wire io_pmp_ld,
    input wire io_pmp_st,
    input wire io_pmp_mmio,
    input wire io_lsq_valid,
    input wire [8:0] io_lsq_fuOpType,
    input wire [5:0] io_lsq_exceptionVec,
    input wire [3:0] io_lsq_trigger,
    input wire io_lsq_robIdx_flag,
    input wire [7:0] io_lsq_robIdx_value,
    input wire io_lsq_sqIdx_flag,
    input wire [5:0] io_lsq_sqIdx_value,
    input wire [15:0] io_lsq_mask,
    input wire io_lsq_wlineflag,
    input wire io_lsq_miss,
    input wire io_lsq_nc,
    input wire io_lsq_isvec,
    input wire io_lsq_isFrmMisAlignBuf,
    input wire io_lsq_isMisalign,
    input wire io_lsq_misalignWith16Byte,
    input wire io_lsq_updateAddrValid,
    input wire io_stld_nuke_query_valid,
    input wire io_stld_nuke_query_robIdx_flag,
    input wire [7:0] io_stld_nuke_query_robIdx_value,
    input wire [47:0] io_stld_nuke_query_paddr,
    input wire [15:0] io_stld_nuke_query_mask,
    input wire [1:0] io_stld_nuke_query_matchType,
    input wire s2_valid,
    input wire s2_isvec,
    input wire s2_isFrmMisAlignBuf,
    input wire s2_isCbo,
    input wire s2_kill,
    input wire s2_exception,
    input wire s2_mmio,
    input wire s2_nc,
    input wire s2_out_af,
    input wire s2_mis_align,
    input wire s2_isCbo_noZero,
    input wire s2_misalignNeedReplay,
    input wire io_lsq_replenish_af,
    input wire io_lsq_replenish_mmio,
    input wire io_lsq_replenish_memBackTypeMM,
    input wire io_lsq_replenish_hasException,
    input wire io_lsq_replenish_isvec,
    input wire io_lsq_replenish_updateAddrValid,
    input wire io_feedback_slow_valid,
    input wire io_feedback_slow_hit,
    input wire io_feedback_slow_sqIdx_flag,
    input wire [5:0] io_feedback_slow_sqIdx_value,
    input wire io_misalign_enq_valid,
    input wire io_misalign_enq_ready,
    input wire io_misalign_enq_revoke,
    input wire s1_toMisalignBufferValid,
    input wire s1_isFinalSplit,
    input wire s1_misalignWith16Byte,
    input wire [2:0] s1_alignedType,
    input wire [4:0] s1_splitIndex,
    input wire io_misalign_stout_valid,
    input wire io_misalign_stout_need_rep,
    input wire io_misalign_stout_nc,
    input wire io_misalign_stout_mmio,
    input wire [5:0] io_misalign_stout_exceptionVec,
    input wire io_stout_valid,
    input wire [5:0] io_stout_exceptionVec,
    input wire [3:0] io_stout_trigger,
    input wire io_stout_robIdx_flag,
    input wire [7:0] io_stout_robIdx_value,
    input wire io_stout_isMMIO,
    input wire io_stout_isNCIO,
    input wire io_vecstout_valid,
    input wire io_st_mask_out_valid,
    input wire [15:0] io_st_mask_out_mask,
    input wire [5:0] io_st_mask_out_sqIdx,
    input wire sx_valid,
    input wire sx_isvec,
    input wire sx_vecFeedback,
    input wire sx_hasException,
    input wire s3_valid,
    input wire s3_kill,
    input wire cur_kill
);

    localparam [8:0] OP_SB = 9'h000;
    localparam [8:0] OP_SH = 9'h001;
    localparam [8:0] OP_SW = 9'h002;
    localparam [8:0] OP_SD = 9'h003;

    wire store_fcov_enable = (reset === 1'b0) && (reset_backend_done === 1'b1);
    wire scalar_stin_fire = store_fcov_enable && io_stin_valid && io_stin_ready;
    wire scalar_s0_sample = store_fcov_enable && s0_fire && !s0_out_isvec &&
                            !io_misalign_stin_valid;
    wire scalar_s1_sample = store_fcov_enable && s1_valid && !s1_isvec &&
                            !s1_isFrmMisAlignBuf;
    wire scalar_s2_sample = store_fcov_enable && s2_valid && !s2_isvec &&
                            !s2_isFrmMisAlignBuf;
    wire tlb_req_sample = store_fcov_enable && io_tlb_req_valid && !s0_out_isvec;
    wire tlb_resp_sample = store_fcov_enable && io_tlb_resp_valid && s1_valid &&
                           !s1_isvec;
    wire scalar_lsq_sample = store_fcov_enable && io_lsq_valid && !io_lsq_isvec &&
                             !io_lsq_isFrmMisAlignBuf;
    wire scalar_stout_sample = store_fcov_enable && io_stout_valid;

    wire [2:0] source_class = io_misalign_stin_valid ? 3'd3 :
                              s0_out_isvec ? 3'd2 :
                              io_stin_valid ? 3'd1 : 3'd0;

    function automatic [2:0] classify_mask;
        input [15:0] mask;
        integer ones;
        reg [15:0] low_bit;
        begin
            ones = $countones(mask);
            low_bit = mask & (~mask + 16'h0001);
            if (ones == 0) classify_mask = 3'd0;
            else if (ones == 1) classify_mask = 3'd1;
            else if (mask == 16'hffff) classify_mask = 3'd4;
            else if ((mask & (mask + low_bit)) == 16'h0000) classify_mask = 3'd2;
            else classify_mask = 3'd3;
        end
    endfunction

    wire [2:0] s0_mask_class = classify_mask(s0_mask);
    wire [2:0] stld_mask_class = classify_mask(io_stld_nuke_query_mask);

    covergroup cg_storeunit_scalar_pipeline_control @(posedge clock);
        option.per_instance = 1;
        cp_s1 : coverpoint {s1_valid, s1_kill, s1_tlb_miss}
            iff (store_fcov_enable) {
            bins idle = {3'b000};
            bins valid = {3'b100};
            bins kill = {3'b110};
            bins tlb_miss = {3'b101};
            bins other = default;
        }
        cp_s2 : coverpoint {s2_valid, s2_kill, s2_exception, s2_misalignNeedReplay}
            iff (store_fcov_enable) {
            bins idle = {4'b0000};
            bins valid = {4'b1000};
            bins exception = {4'b1010};
            bins kill = {4'b1100};
            bins replay = {4'b1001};
            bins other = default;
        }
        cp_s3 : coverpoint {s3_valid, s3_kill}
            iff (store_fcov_enable) {
            bins idle = {2'b00};
            bins valid = {2'b10};
            bins killed = {2'b11};
        }
        cp_sx : coverpoint {sx_valid, cur_kill, sx_vecFeedback, sx_hasException}
            iff (store_fcov_enable) {
            bins idle = {4'b0000};
            bins scalar_pass = {4'b1000};
            bins scalar_exception = {4'b1001};
            bins scalar_feedback = {4'b1010};
            bins killed = {4'b0100};
            bins other = default;
        }
        cross cp_s1, cp_s2;
        cross cp_s3, cp_sx;
    endgroup

    covergroup cg_storeunit_scalar_mask_output @(posedge clock);
        option.per_instance = 1;
        cp_valid : coverpoint io_st_mask_out_valid iff (store_fcov_enable) {
            bins no = {0};
            bins yes = {1};
        }
        cp_mask : coverpoint classify_mask(io_st_mask_out_mask)
            iff (store_fcov_enable && io_st_mask_out_valid) {
            bins none = {0};
            bins one = {1};
            bins contiguous = {2};
            bins non_contiguous = {3};
            bins full = {4};
        }
        cp_sq_idx : coverpoint io_st_mask_out_sqIdx
            iff (store_fcov_enable && io_st_mask_out_valid) {
            bins zero = {0};
            bins middle = {[1:62]};
            bins max = {63};
        }
        cross cp_valid, cp_mask;
    endgroup

    covergroup cg_storeunit_scalar_source @(posedge clock);
        option.per_instance = 1;
        cp_source : coverpoint source_class iff (store_fcov_enable && s0_valid) {
            ignore_bins none = {0};
            bins scalar = {1};
            ignore_bins vector = {2};
            bins mab_return = {3};
        }
        cp_input_valids : coverpoint {
            io_misalign_stin_valid, io_vecstin_valid, io_stin_valid
        } iff (store_fcov_enable &&
               (io_misalign_stin_valid || io_vecstin_valid || io_stin_valid)) {
            ignore_bins none = {3'b000};
            bins combinations[] = {[1:7]};
        }
        cp_scalar_handshake : coverpoint {io_stin_valid, io_stin_ready}
            iff (store_fcov_enable && io_stin_valid) {
            bins blocked = {2'b10};
            bins accepted = {2'b11};
        }
        cp_vector_handshake : coverpoint {io_vecstin_valid, io_vecstin_ready}
            iff (store_fcov_enable && io_vecstin_valid) {
            bins blocked = {2'b10};
            bins accepted = {2'b11};
        }
        cp_mab_handshake : coverpoint {
            io_misalign_stin_valid, io_misalign_stin_ready
        } iff (store_fcov_enable && io_misalign_stin_valid) {
            bins blocked = {2'b10};
            bins accepted = {2'b11};
        }
        cp_redirect : coverpoint io_redirect_valid iff (store_fcov_enable && s0_valid) {
            bins no = {0};
            bins yes = {1};
        }
        cross cp_source, cp_redirect;
    endgroup

    covergroup cg_storeunit_scalar_address @(posedge clock);
        option.per_instance = 1;
        cp_fu_op_type : coverpoint io_stin_fuOpType iff (scalar_stin_fire) {
            bins sb = {OP_SB};
            bins sh = {OP_SH};
            bins sw = {OP_SW};
            bins sd = {OP_SD};
            bins other = default;
        }
        cp_addr_low : coverpoint s0_vaddr[5:0] iff (scalar_s0_sample) {
            bins offset[] = {[0:63]};
        }
        cp_shape : coverpoint {
            s0_isCbo, s0_rs_cross16Bytes, s0_misalignWith16Byte, s0_addr_aligned
        } iff (scalar_s0_sample) {
            bins combinations[] = {[0:15]};
        }
        cp_mask : coverpoint s0_mask_class iff (scalar_s0_sample) {
            bins none = {0};
            bins one = {1};
            bins contiguous = {2};
            bins non_contiguous = {3};
            bins full = {4};
        }
        cp_imm : coverpoint io_stin_imm[11:0] iff (scalar_stin_fire) {
            bins zero = {12'h000};
            bins positive = {[12'h001:12'h7ff]};
            bins negative = {[12'h800:12'hfff]};
        }
        cp_base_addr_low : coverpoint io_stin_src0[5:0] iff (scalar_stin_fire) {
            bins offset[] = {[0:63]};
        }
        cp_rob_flag : coverpoint io_stin_robIdx_flag iff (scalar_stin_fire) {
            bins flag[] = {0,1};
        }
        cp_rob_value : coverpoint io_stin_robIdx_value iff (scalar_stin_fire) {
            bins zero = {0};
            bins middle = {[1:254]};
            bins max = {255};
        }
        cp_sq_flag : coverpoint io_stin_sqIdx_flag iff (scalar_stin_fire) {
            bins flag[] = {0,1};
        }
        cp_sq_value : coverpoint io_stin_sqIdx_value iff (scalar_stin_fire) {
            bins zero = {0};
            bins middle = {[1:62]};
            bins max = {63};
        }
        cross cp_fu_op_type, cp_shape;
        cross cp_shape, cp_mask;
        cross cp_rob_flag, cp_sq_flag;
    endgroup

    covergroup cg_storeunit_scalar_tlb @(posedge clock);
        option.per_instance = 1;
        cp_req_cmd : coverpoint io_tlb_req_cmd iff (tlb_req_sample) {
            bins read = {0};
            bins write = {1};
            bins other = default;
        }
        cp_req_flags : coverpoint {
            io_tlb_req_checkfullva, io_tlb_req_hyperinst, io_tlb_req_frm_mabuf
        } iff (tlb_req_sample) {
            bins combinations[] = {[0:7]};
        }
        cp_req_size : coverpoint io_tlb_req_size iff (tlb_req_sample) {
            bins byte_size = {0};
            bins half = {1};
            bins word = {2};
            bins double = {3};
            bins other = default;
        }
        cp_req_addr_low : coverpoint io_tlb_req_vaddr[11:0] iff (tlb_req_sample) {
            bins page_start = {12'h000};
            bins low = {[12'h001:12'h7ff]};
            bins high = {[12'h800:12'hffe]};
            bins page_end = {12'hfff};
        }
        cp_resp_miss : coverpoint io_tlb_resp_miss iff (tlb_resp_sample) {
            bins hit = {0};
            bins miss = {1};
        }
        cp_pbmt : coverpoint io_tlb_resp_pbmt iff (tlb_resp_sample) {
            bins normal = {0};
            bins nc = {1};
            bins io = {2};
            bins other = default;
        }
        cp_fault : coverpoint {io_tlb_resp_gpf, io_tlb_resp_pf, io_tlb_resp_af}
            iff (tlb_resp_sample) {
            bins none = {3'b000};
            bins gpf = {3'b100};
            bins pf = {3'b010};
            bins af = {3'b001};
            bins multiple = default;
        }
        cp_resp_flags : coverpoint {
            io_tlb_resp_vaNeedExt, io_tlb_resp_isHyper,
            io_tlb_resp_isForVSnonLeafPTE
        } iff (tlb_resp_sample) {
            bins combinations[] = {[0:7]};
        }
        cp_s1_state : coverpoint {
            s1_kill, s1_tlb_hit, s1_tlb_miss, s1_isFrmMisAlignBuf
        } iff (store_fcov_enable && s1_valid) {
            bins combinations[] = {[0:15]};
        }
        cp_s1_pbmt : coverpoint s1_pbmt iff (store_fcov_enable && s1_valid) {
            bins normal = {0};
            bins nc = {1};
            bins io = {2};
            bins other = default;
        }
        cross cp_resp_miss, cp_pbmt, cp_fault;
    endgroup

    covergroup cg_storeunit_scalar_stld_query @(posedge clock);
        option.per_instance = 1;
        cp_match_type : coverpoint io_stld_nuke_query_matchType
            iff (store_fcov_enable && io_stld_nuke_query_valid) {
            bins normal = {0};
            bins quad_word = {1};
            bins cache_line = {2};
            bins reserved = {3};
        }
        cp_mask : coverpoint stld_mask_class
            iff (store_fcov_enable && io_stld_nuke_query_valid) {
            bins none = {0};
            bins one = {1};
            bins contiguous = {2};
            bins non_contiguous = {3};
            bins full = {4};
        }
        cp_paddr_low : coverpoint io_stld_nuke_query_paddr[5:0]
            iff (store_fcov_enable && io_stld_nuke_query_valid) {
            bins offset[] = {[0:63]};
        }
        cp_rob_flag : coverpoint io_stld_nuke_query_robIdx_flag
            iff (store_fcov_enable && io_stld_nuke_query_valid) {
            bins flag[] = {0,1};
        }
        cp_rob_value : coverpoint io_stld_nuke_query_robIdx_value
            iff (store_fcov_enable && io_stld_nuke_query_valid) {
            bins zero = {0};
            bins middle = {[1:254]};
            bins max = {255};
        }
        cross cp_match_type, cp_mask;
    endgroup

    covergroup cg_storeunit_scalar_lsq @(posedge clock);
        option.per_instance = 1;
        cp_fu_op_type : coverpoint io_lsq_fuOpType iff (scalar_lsq_sample) {
            bins sb = {OP_SB};
            bins sh = {OP_SH};
            bins sw = {OP_SW};
            bins sd = {OP_SD};
            bins other = default;
        }
        cp_status : coverpoint {
            io_lsq_updateAddrValid, io_lsq_misalignWith16Byte,
            io_lsq_isMisalign, io_lsq_miss
        } iff (scalar_lsq_sample) {
            bins combinations[] = {[0:15]};
        }
        cp_mask : coverpoint classify_mask(io_lsq_mask) iff (scalar_lsq_sample) {
            bins none = {0};
            bins one = {1};
            bins contiguous = {2};
            bins non_contiguous = {3};
            bins full = {4};
        }
        cp_attr : coverpoint {io_lsq_wlineflag, io_lsq_nc}
            iff (scalar_lsq_sample) {
            bins combinations[] = {[0:3]};
        }
        cp_exception : coverpoint io_lsq_exceptionVec iff (scalar_lsq_sample) {
            bins none = {0};
            bins breakpoint = {6'b000001};
            bins misalign = {6'b000010};
            bins af = {6'b000100};
            bins pf = {6'b001000};
            bins gpf = {6'b100000};
            bins other = default;
        }
        cp_trigger : coverpoint io_lsq_trigger iff (scalar_lsq_sample) {
            bins breakpoint = {0};
            bins debug_mode = {1};
            bins none = {15};
            bins other = default;
        }
        cp_rob_flag : coverpoint io_lsq_robIdx_flag iff (scalar_lsq_sample) {
            bins flag[] = {0,1};
        }
        cp_sq_flag : coverpoint io_lsq_sqIdx_flag iff (scalar_lsq_sample) {
            bins flag[] = {0,1};
        }
        cp_rob_value : coverpoint io_lsq_robIdx_value iff (scalar_lsq_sample) {
            bins zero = {0};
            bins middle = {[1:254]};
            bins max = {255};
        }
        cp_sq_value : coverpoint io_lsq_sqIdx_value iff (scalar_lsq_sample) {
            bins zero = {0};
            bins middle = {[1:62]};
            bins max = {63};
        }
        cross cp_status, cp_attr;
        cross cp_exception, cp_attr;
    endgroup

    covergroup cg_storeunit_scalar_attribute @(posedge clock);
        option.per_instance = 1;
        cp_pmp : coverpoint {io_pmp_ld, io_pmp_st, io_pmp_mmio}
            iff (scalar_s2_sample) {
            bins combinations[] = {[0:7]};
        }
        cp_s2_attr : coverpoint {s2_nc, s2_mmio, s2_isCbo}
            iff (scalar_s2_sample) {
            bins combinations[] = {[0:7]};
        }
        cp_result : coverpoint {
            io_lsq_replenish_hasException, io_lsq_replenish_mmio,
            io_lsq_replenish_af, io_lsq_replenish_updateAddrValid,
            io_lsq_replenish_memBackTypeMM, io_lsq_replenish_isvec
        } iff (scalar_s2_sample) {
            bins combinations[] = {[0:63]};
        }
        cp_internal : coverpoint {s2_kill, s2_exception}
            iff (store_fcov_enable && s2_valid) {
            bins none = {2'b00};
            bins exception = {2'b01};
            bins kill = {2'b10};
            bins both = {2'b11};
        }
        cp_kill : coverpoint s2_kill iff (scalar_s2_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cp_result_detail : coverpoint {
            s2_exception, s2_out_af, s2_mis_align, s2_isCbo_noZero,
            s2_mmio, s2_nc
        } iff (scalar_s2_sample) {
            bins combinations[] = {[0:63]};
        }
        cross cp_s2_attr, cp_result;
        cross cp_pmp, cp_result_detail;
    endgroup

    covergroup cg_storeunit_scalar_feedback @(posedge clock);
        option.per_instance = 1;
        cp_hit : coverpoint io_feedback_slow_hit
            iff (store_fcov_enable && io_feedback_slow_valid) {
            bins block = {0};
            bins hit = {1};
        }
        cp_sq_flag : coverpoint io_feedback_slow_sqIdx_flag
            iff (store_fcov_enable && io_feedback_slow_valid) {
            bins flag[] = {0,1};
        }
        cp_sq_value : coverpoint io_feedback_slow_sqIdx_value
            iff (store_fcov_enable && io_feedback_slow_valid) {
            bins zero = {0};
            bins middle = {[1:62]};
            bins max = {63};
        }
        cp_replay_candidate : coverpoint s2_misalignNeedReplay
            iff (store_fcov_enable && io_feedback_slow_valid) {
            bins no = {0};
            bins yes = {1};
        }
        cross cp_hit, cp_replay_candidate;
    endgroup

    covergroup cg_storeunit_scalar_misalign @(posedge clock);
        option.per_instance = 1;
        cp_enqueue : coverpoint {io_misalign_enq_valid, io_misalign_enq_ready}
            iff (store_fcov_enable && s1_valid) {
            bins idle = {2'b00};
            bins ready_idle = {2'b01};
            bins blocked = {2'b10};
            bins accepted = {2'b11};
        }
        cp_candidate : coverpoint {
            s1_toMisalignBufferValid, s1_isFinalSplit,
            s1_misalignWith16Byte, s1_alignedType
        } iff (store_fcov_enable && s1_valid) {
            bins combinations[] = {[0:63]};
        }
        cp_split_index : coverpoint s1_splitIndex
            iff (store_fcov_enable && io_misalign_enq_valid) {
            bins first = {0};
            bins middle = {[1:30]};
            bins last = {31};
        }
        cp_revoke : coverpoint io_misalign_enq_revoke
            iff (store_fcov_enable && s2_valid) {
            bins no = {0};
            bins yes = {1};
        }
        cp_return : coverpoint {
            io_misalign_stout_need_rep, io_misalign_stout_mmio,
            io_misalign_stout_nc
        } iff (store_fcov_enable && io_misalign_stout_valid) {
            bins combinations[] = {[0:7]};
        }
        cp_return_exception : coverpoint io_misalign_stout_exceptionVec
            iff (store_fcov_enable && io_misalign_stout_valid) {
            bins none = {0};
            bins nonzero = default;
        }
        cp_csr_enable : coverpoint io_csrCtrl_hd_misalign_st_enable
            iff (store_fcov_enable && s1_valid) {
            bins disabled = {0};
            bins enabled = {1};
        }
        cp_s1_candidate : coverpoint s1_misalignNeedReplay
            iff (store_fcov_enable && s1_valid) {
            bins no = {0};
            bins yes = {1};
        }
        cp_mab_fu_op : coverpoint s1_fuOpType
            iff (store_fcov_enable && io_misalign_enq_valid) {
            bins sb = {OP_SB};
            bins sh = {OP_SH};
            bins sw = {OP_SW};
            bins sd = {OP_SD};
            bins other = default;
        }
        cp_mab_mask : coverpoint classify_mask(s1_mask)
            iff (store_fcov_enable && io_misalign_enq_valid) {
            bins none = {0};
            bins one = {1};
            bins contiguous = {2};
            bins non_contiguous = {3};
            bins full = {4};
        }
    endgroup

    covergroup cg_storeunit_scalar_trigger @(posedge clock);
        option.per_instance = 1;
        cp_action : coverpoint s1_trigger_action iff (scalar_s1_sample) {
            bins breakpoint = {0};
            bins debug_mode = {1};
            bins none = {15};
            bins other = default;
        }
        cp_context : coverpoint {s1_isCbo, s1_is128bit, s1_tlb_miss}
            iff (scalar_s1_sample) {
            bins combinations[] = {[0:7]};
        }
        cp_debug_mode : coverpoint s1_trigger_debug_mode iff (scalar_s1_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cross cp_action, cp_context;
    endgroup

    covergroup cg_storeunit_scalar_writeback @(posedge clock);
        option.per_instance = 1;
        cp_route : coverpoint {io_vecstout_valid, io_stout_valid}
            iff (store_fcov_enable && sx_valid) {
            ignore_bins none = {2'b00};
            bins scalar = {2'b01};
            ignore_bins vector = {2'b10};
            illegal_bins both = {2'b11};
        }
        cp_exception : coverpoint io_stout_exceptionVec iff (scalar_stout_sample) {
            bins none = {0};
            bins breakpoint = {6'b000001};
            bins misalign = {6'b000010};
            bins af = {6'b000100};
            bins pf = {6'b001000};
            bins gpf = {6'b100000};
            bins other = default;
        }
        cp_trigger : coverpoint io_stout_trigger iff (scalar_stout_sample) {
            bins breakpoint = {0};
            bins debug_mode = {1};
            bins none = {15};
            bins other = default;
        }
        cp_attr : coverpoint {io_stout_isMMIO, io_stout_isNCIO}
            iff (scalar_stout_sample) {
            bins normal = {2'b00};
            bins nc = {2'b01};
            bins mmio = {2'b10};
            bins both = {2'b11};
        }
        cp_rob_flag : coverpoint io_stout_robIdx_flag iff (scalar_stout_sample) {
            bins flag[] = {0,1};
        }
        cp_rob_value : coverpoint io_stout_robIdx_value iff (scalar_stout_sample) {
            bins zero = {0};
            bins middle = {[1:254]};
            bins max = {255};
        }
        cp_redirect_kill : coverpoint io_redirect_valid
            iff (store_fcov_enable && sx_valid && !sx_isvec) {
            bins no = {0};
            bins yes = {1};
        }
        cross cp_exception, cp_attr;
    endgroup

    cg_storeunit_scalar_source source_cg;
    cg_storeunit_scalar_pipeline_control pipeline_control_cg;
    cg_storeunit_scalar_mask_output mask_output_cg;
    cg_storeunit_scalar_address address_cg;
    cg_storeunit_scalar_tlb tlb_cg;
    cg_storeunit_scalar_stld_query stld_query_cg;
    cg_storeunit_scalar_lsq lsq_cg;
    cg_storeunit_scalar_attribute attribute_cg;
    cg_storeunit_scalar_feedback feedback_cg;
    cg_storeunit_scalar_misalign misalign_cg;
    cg_storeunit_scalar_trigger trigger_cg;
    cg_storeunit_scalar_writeback writeback_cg;

    initial begin
        source_cg = new();
        pipeline_control_cg = new();
        mask_output_cg = new();
        address_cg = new();
        tlb_cg = new();
        stld_query_cg = new();
        lsq_cg = new();
        attribute_cg = new();
        feedback_cg = new();
        misalign_cg = new();
        trigger_cg = new();
        writeback_cg = new();
    end

endmodule: memblock_storeunit_scalar_fcov
`endif

`endif
