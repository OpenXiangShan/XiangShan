//=========================================================
// V2 LoadUnit scalar functional coverage.
//
// This first implementation follows memblock_rtl_fcov.sv:
//   * covergroups sample static signals on posedge clock;
//   * every group has per-instance coverage and an explicit iff gate;
//   * helper classification is local and stateless;
//   * no temporal assertion, scoreboard state, or replay tracker is kept.
//=========================================================
`ifndef MEMBLOCK_LDU_SCALAR_FCOV__SV
`define MEMBLOCK_LDU_SCALAR_FCOV__SV

`ifdef MEMBLOCK_UT_FCOV
module memblock_ldu_scalar_fcov (
    input wire clock,
    input wire reset,
    input wire reset_backend_done,
    input wire io_ldin_valid,
    input wire io_ldin_ready,
    input wire [8:0] io_ldin_fuOpType,
    input wire io_ldin_rfWen,
    input wire io_ldin_fpWen,
    input wire [31:0] io_ldin_imm,
    input wire [7:0] io_ldin_pdest,
    input wire io_ldin_robIdx_flag,
    input wire [7:0] io_ldin_robIdx_value,
    input wire io_ldin_lqIdx_flag,
    input wire [6:0] io_ldin_lqIdx_value,
    input wire [63:0] io_ldin_src0,
    input wire io_misalign_ldin_valid,
    input wire io_fast_rep_in_valid,
    input wire io_vecldin_valid,
    input wire io_lsq_uncache_valid,
    input wire io_lsq_nc_ldin_valid,
    input wire s0_src_selector_1,
    input wire s0_src_selector_3,
    input wire s0_src_selector_4,
    input wire s0_src_select_vec_2,
    input wire s0_src_select_vec_5,
    input wire s0_src_select_vec_6,
    input wire s0_src_select_vec_7,
    input wire s0_src_select_vec_8,
    input wire s0_src_valid_vec_10,
    input wire s0_rep_stall,
    input wire s0_fire,
    input wire [8:0] s0_sel_src_fuOpType,
    input wire s0_out_tlbNoQuery,
    input wire s0_addr_aligned,
    input wire s0_rs_cross16Bytes,
    input wire s0_misalignWith16Byte,
    input wire s0_is128bit,
    input wire s0_out_isHWPrefetch,
    input wire s0_sel_src_isnc,
    input wire s0_sel_src_frm_mabuf,
    input wire s0_mmio_fire,
    input wire s0_nc_fire,
    input wire s1_valid,
    input wire s1_in_r_isvec,
    input wire s1_kill,
    input wire s1_misalign_kill,
    input wire s1_tlb_miss,
    input wire s1_trigger_debug_mode,
    input wire s1_trigger_breakpoint,
    input wire io_redirect_valid,
    input wire io_tlb_req_valid,
    input wire [2:0] io_tlb_req_cmd,
    input wire [49:0] io_tlb_req_vaddr,
    input wire io_tlb_req_bits_checkfullva,
    input wire io_tlb_req_bits_hyperinst,
    input wire io_tlb_req_bits_hlvx,
    input wire io_tlb_req_bits_kill,
    input wire io_tlb_req_bits_isPrefetch,
    input wire io_tlb_req_bits_no_translate,
    input wire io_tlb_resp_valid,
    input wire io_tlb_resp_bits_miss,
    input wire [1:0] io_tlb_resp_bits_pbmt,
    input wire io_tlb_resp_bits_excp_gpf,
    input wire io_tlb_resp_bits_excp_pf,
    input wire io_tlb_resp_bits_excp_af,
    input wire io_pmp_ld,
    input wire io_pmp_st,
    input wire io_pmp_instr,
    input wire io_pmp_mmio,
    input wire io_dcache_req_valid,
    input wire io_dcache_req_ready,
    input wire [4:0] io_dcache_req_cmd,
    input wire [3:0] io_dcache_req_instrtype,
    input wire [49:0] io_dcache_req_vaddr,
    input wire io_dcache_req_lqIdx_flag,
    input wire [6:0] io_dcache_req_lqIdx_value,
    input wire io_dcache_is128Req,
    input wire io_dcache_resp_bits_miss,
    input wire [3:0] io_dcache_resp_bits_mshr_id,
    input wire [2:0] io_dcache_resp_bits_meta_prefetch,
    input wire io_dcache_resp_bits_handled,
    input wire io_dcache_resp_bits_denied,
    input wire io_dcache_resp_bits_corrupt,
    input wire io_dcache_s2_bank_conflict,
    input wire io_dcache_s2_mq_nack,
    input wire io_dcache_s1_kill,
    input wire io_dcache_s1_kill_data_read,
    input wire io_sbuffer_valid,
    input wire [15:0] io_sbuffer_forwardMask,
    input wire io_sbuffer_matchInvalid,
    input wire io_ubuffer_valid,
    input wire [15:0] io_ubuffer_forwardMask,
    input wire io_ubuffer_matchInvalid,
    input wire io_lsq_forward_valid,
    input wire [15:0] io_lsq_forward_forwardMask,
    input wire io_lsq_forward_dataInvalid,
    input wire io_lsq_forward_matchInvalid,
    input wire io_lsq_forward_addrInvalid,
    input wire [15:0] s2_fwd_mask,
    input wire s2_full_fwd,
    input wire s2_mem_amb,
    input wire s2_fwd_fail,
    input wire s2_safe_wakeup,
    input wire s2_real_exception,
    input wire s2_valid,
    input wire s2_isvec,
    input wire [8:0] s2_in_r_fuOpType,
    input wire s2_in_r_is128bit,
    input wire s2_in_r_nc,
    input wire s2_in_r_mmio,
    input wire s2_in_r_isPrefetch,
    input wire s2_in_r_isLoadReplay,
    input wire s2_in_r_isFastReplay,
    input wire s2_in_r_isFrmMisAlignBuf,
    input wire s2_in_r_rep_info_cause_9,
    input wire s2_nuke,
    input wire io_stld_nuke_query_0_valid,
    input wire [1:0] io_stld_nuke_query_0_matchType,
    input wire io_stld_nuke_query_1_valid,
    input wire [1:0] io_stld_nuke_query_1_matchType,
    input wire io_lsq_stld_nuke_query_req_valid,
    input wire io_lsq_stld_nuke_query_req_ready,
    input wire io_lsq_stld_nuke_query_req_bits_data_valid,
    input wire io_lsq_stld_nuke_query_revoke,
    input wire io_lsq_ldld_nuke_query_req_valid,
    input wire io_lsq_ldld_nuke_query_req_ready,
    input wire io_lsq_ldld_nuke_query_req_bits_data_valid,
    input wire io_lsq_ldld_nuke_query_req_bits_is_nc,
    input wire io_lsq_ldld_nuke_query_revoke,
    input wire [10:0] io_lsq_ldin_rep_cause,
    input wire io_lsq_ldin_valid,
    input wire io_fast_rep_out_valid,
    input wire io_misalign_enq_req_valid,
    input wire io_misalign_enq_req_ready,
    input wire io_misalign_ldout_valid,
    input wire s3_in_isFrmMisAlignBuf,
    input wire s3_misalign_can_go,
    input wire s3_misalign_wakeup_req_valid,
    input wire io_rollback_valid,
    input wire io_rollback_level,
    input wire io_wakeup_valid,
    input wire io_wakeup_rfWen,
    input wire io_wakeup_fpWen,
    input wire io_ldCancel_ld2Cancel,
    input wire s3_valid_last_REG,
    input wire s3_isvec,
    input wire s3_exception,
    input wire s3_safe_writeback,
    input wire io_csrCtrl_ldld_vio_check_enable,
    input wire io_csrCtrl_hd_misalign_ld_enable,
    input wire io_ldout_valid,
    input wire io_ldout_ready,
    input wire [5:0] io_ldout_exceptionVec,
    input wire io_ldout_rfWen,
    input wire io_ldout_fpWen,
    input wire [7:0] io_ldout_pdest,
    input wire io_ldout_robIdx_flag,
    input wire [7:0] io_ldout_robIdx_value,
    input wire io_ldout_replayInst,
    input wire io_ldout_debug_isMMIO,
    input wire io_ldout_debug_isNCIO
);

    localparam [8:0] OP_LB = 9'h000, OP_LH = 9'h001, OP_LW = 9'h002, OP_LD = 9'h003;
    localparam [8:0] OP_LBU = 9'h004, OP_LHU = 9'h005, OP_LWU = 9'h006;
    localparam [8:0] OP_PREFETCH_I = 9'h008, OP_PREFETCH_R = 9'h009, OP_PREFETCH_W = 9'h00a;

    wire scalar_fcov_sample_enable = (reset === 1'b0) && (reset_backend_done === 1'b1);
    wire scalar_ldin_fire = scalar_fcov_sample_enable && io_ldin_valid && io_ldin_ready;
    wire scalar_s0_sample = scalar_fcov_sample_enable && s0_fire && !s0_src_select_vec_5;
    wire scalar_s0_path_sample = scalar_fcov_sample_enable && !s0_src_select_vec_5 &&
                                 (s0_fire || s0_mmio_fire || s0_nc_fire);
    wire scalar_s1_sample = scalar_fcov_sample_enable && s1_valid && !s1_in_r_isvec;
    wire scalar_s2_sample = scalar_fcov_sample_enable && s2_valid && !s2_isvec;
    wire scalar_s3_sample = scalar_fcov_sample_enable && s3_valid_last_REG && !s3_isvec;
    wire scalar_dcache_req_sample = scalar_s0_sample && io_dcache_req_valid;
    wire scalar_tlb_req_sample = scalar_s0_sample && io_tlb_req_valid;
    wire scalar_tlb_resp_sample = scalar_s1_sample && io_tlb_resp_valid;
    wire scalar_ldout_fire = scalar_fcov_sample_enable && io_ldout_valid && io_ldout_ready;
    wire scalar_lsq_replay_sample = scalar_s3_sample && io_lsq_ldin_valid;
    wire scalar_stage_sample = scalar_s1_sample || scalar_s2_sample;
    wire scalar_wakeup_sample = scalar_fcov_sample_enable && io_wakeup_valid;
    wire scalar_rollback_sample = scalar_s3_sample && io_rollback_valid;

    wire [10:0] source_valid_mask = {
        s0_src_valid_vec_10, 1'b0, io_lsq_nc_ldin_valid,
        io_lsq_uncache_valid, io_ldin_valid, io_vecldin_valid,
        s0_src_selector_4, s0_src_selector_3, io_fast_rep_in_valid,
        s0_src_selector_1, io_misalign_ldin_valid
    };
    wire [10:0] source_selected_mask =
        io_misalign_ldin_valid ? 11'h001 :
        s0_src_selector_1 ? 11'h002 :
        s0_src_select_vec_2 ? 11'h004 :
        s0_src_selector_3 ? 11'h008 :
        s0_src_selector_4 ? 11'h010 :
        s0_src_select_vec_5 ? 11'h020 :
        s0_src_select_vec_6 ? 11'h040 :
        s0_src_select_vec_7 ? 11'h080 :
        s0_src_select_vec_8 ? 11'h100 :
        s0_src_valid_vec_10 ? 11'h400 : 11'h000;

    wire [4:0] output_route_mask = {
        io_ldout_valid, io_fast_rep_out_valid,
        io_misalign_enq_req_valid, io_misalign_ldout_valid,
        io_rollback_valid
    };

    function automatic [3:0] classify_dcache_response;
        input miss, handled, bank_conflict, mq_nack, denied, corrupt;
        begin
            if (denied && corrupt) classify_dcache_response = 4'd7;
            else if (denied) classify_dcache_response = 4'd5;
            else if (corrupt) classify_dcache_response = 4'd6;
            else if (!miss && !bank_conflict) classify_dcache_response = 4'd0;
            else if (!miss && bank_conflict) classify_dcache_response = 4'd1;
            else if (miss && mq_nack) classify_dcache_response = 4'd3;
            else if (miss && handled) classify_dcache_response = 4'd2;
            else classify_dcache_response = 4'd4;
        end
    endfunction

    function automatic [2:0] mask_kind;
        input [15:0] mask;
        begin
            if (mask == 16'h0000) mask_kind = 3'd0;
            else if (mask == 16'hffff) mask_kind = 3'd2;
            else mask_kind = 3'd1;
        end
    endfunction

    function automatic [2:0] mask_popcount_class;
        input [15:0] mask;
        integer count;
        begin
            count = $countones(mask);
            if (count == 0) mask_popcount_class = 3'd0;
            else if (count == 1) mask_popcount_class = 3'd1;
            else if (count <= 7) mask_popcount_class = 3'd2;
            else if (count <= 15) mask_popcount_class = 3'd3;
            else mask_popcount_class = 3'd4;
        end
    endfunction

    function automatic [3:0] classify_replay_cause;
        input [10:0] cause;
        begin
            case (cause)
                11'h000: classify_replay_cause = 4'd0;
                11'h001: classify_replay_cause = 4'd1;
                11'h002: classify_replay_cause = 4'd2;
                11'h004: classify_replay_cause = 4'd3;
                11'h008: classify_replay_cause = 4'd4;
                11'h010: classify_replay_cause = 4'd5;
                11'h020: classify_replay_cause = 4'd6;
                11'h040: classify_replay_cause = 4'd7;
                11'h080: classify_replay_cause = 4'd8;
                11'h100: classify_replay_cause = 4'd9;
                11'h200: classify_replay_cause = 4'd10;
                11'h400: classify_replay_cause = 4'd11;
                default: classify_replay_cause = 4'd12;
            endcase
        end
    endfunction

    function automatic [3:0] classify_source_mask;
        input [10:0] mask;
        begin
            case (mask)
                11'h000: classify_source_mask = 4'd0;
                11'h001: classify_source_mask = 4'd1;
                11'h002: classify_source_mask = 4'd2;
                11'h004: classify_source_mask = 4'd3;
                11'h008: classify_source_mask = 4'd4;
                11'h010: classify_source_mask = 4'd5;
                11'h020: classify_source_mask = 4'd6;
                11'h040: classify_source_mask = 4'd7;
                11'h080: classify_source_mask = 4'd8;
                11'h100: classify_source_mask = 4'd9;
                11'h400: classify_source_mask = 4'd10;
                default: classify_source_mask = 4'd11;
            endcase
        end
    endfunction

    wire [3:0] dcache_response_class = classify_dcache_response(
        io_dcache_resp_bits_miss, io_dcache_resp_bits_handled,
        io_dcache_s2_bank_conflict, io_dcache_s2_mq_nack,
        io_dcache_resp_bits_denied, io_dcache_resp_bits_corrupt);
    wire [2:0] lsq_forward_kind = mask_kind(io_lsq_forward_forwardMask);
    wire [2:0] sbuffer_forward_kind = mask_kind(io_sbuffer_forwardMask);
    wire [2:0] ubuffer_forward_kind = mask_kind(io_ubuffer_forwardMask);
    wire [2:0] merged_forward_kind = mask_kind(s2_fwd_mask);
    wire [2:0] lsq_forward_popcount_class = mask_popcount_class(io_lsq_forward_forwardMask);
    wire [2:0] sbuffer_forward_popcount_class = mask_popcount_class(io_sbuffer_forwardMask);
    wire [2:0] ubuffer_forward_popcount_class = mask_popcount_class(io_ubuffer_forwardMask);
    wire [3:0] cause_count = $countones(io_lsq_ldin_rep_cause);
    wire [3:0] replay_cause_class = classify_replay_cause(io_lsq_ldin_rep_cause);
    wire [3:0] source_count = $countones(source_valid_mask);
    wire [10:0] blocked_source_mask = source_valid_mask & ~source_selected_mask;
    wire [3:0] blocked_source_class = classify_source_mask(blocked_source_mask);
    wire [2:0] output_route_count = $countones(output_route_mask);
    wire [2:0] s2_source_class =
        s2_in_r_isFrmMisAlignBuf ? 3'd6 :
        s2_in_r_isPrefetch ? 3'd5 :
        s2_in_r_mmio ? 3'd4 :
        s2_in_r_nc ? 3'd3 :
        s2_in_r_isFastReplay ? 3'd2 :
        s2_in_r_isLoadReplay ? 3'd1 : 3'd0;

    covergroup cg_ldu_scalar_ldin @(posedge clock);
        option.per_instance = 1;
        cp_fu_op_type : coverpoint io_ldin_fuOpType iff (scalar_ldin_fire) {
            bins lb = {OP_LB};
            bins lh = {OP_LH};
            bins lw = {OP_LW};
            bins ld = {OP_LD};
            bins lbu = {OP_LBU};
            bins lhu = {OP_LHU};
            bins lwu = {OP_LWU};
            bins prefetch = {OP_PREFETCH_I, OP_PREFETCH_R, OP_PREFETCH_W};
            bins other = default;
        }
        cp_rf_fp_wen : coverpoint {io_ldin_rfWen, io_ldin_fpWen} iff (scalar_ldin_fire) {
            bins none = {2'b00};
            bins int_wb = {2'b10};
            bins fp = {2'b01};
            bins both = {2'b11};
        }
        cp_imm_class : coverpoint io_ldin_imm[11:0] iff (scalar_ldin_fire) {
            bins zero = {12'h000};
            bins positive = {[12'h001:12'h7fe]};
            bins max_positive = {12'h7ff};
            bins min_negative = {12'h800};
            bins negative = {[12'h801:12'hfff]};
        }
        cp_pdest : coverpoint io_ldin_pdest iff (scalar_ldin_fire) {
            bins zero = {8'h00};
            bins low = {[8'h01:8'h7f]};
            bins high = {[8'h80:8'hff]};
        }
        cp_rob_flag : coverpoint io_ldin_robIdx_flag iff (scalar_ldin_fire) {
            bins flag[] = {0,1};
        }
        cp_rob_value : coverpoint io_ldin_robIdx_value iff (scalar_ldin_fire) {
            bins zero = {0};
            bins middle = {[1:254]};
            bins max = {255};
        }
        cp_lq_flag : coverpoint io_ldin_lqIdx_flag iff (scalar_ldin_fire) {
            bins flag[] = {0,1};
        }
        cp_lq_value : coverpoint io_ldin_lqIdx_value iff (scalar_ldin_fire) {
            bins zero = {0};
            bins middle = {[1:126]};
            bins max = {127};
        }
        cp_addr_low : coverpoint io_ldin_src0[3:0] iff (scalar_ldin_fire) {
            bins offset[] = {[0:15]};
        }
        cross cp_fu_op_type, cp_rf_fp_wen, cp_addr_low;
        cross cp_rob_flag, cp_rob_value;
        cross cp_lq_flag, cp_lq_value;
        cross cp_rob_flag, cp_lq_flag;
    endgroup

    covergroup cg_ldu_scalar_source @(posedge clock);
        option.per_instance = 1;
        cp_valid_count : coverpoint source_count iff (scalar_s0_path_sample) {
            bins none = {0};
            bins one = {1};
            bins two = {2};
            bins many = {[3:11]};
        }
        cp_selected_source : coverpoint source_selected_mask iff (scalar_s0_path_sample) {
            bins misalign = {11'h001};
            bins replay_tld = {11'h002};
            bins fast_replay = {11'h004};
            bins replay = {11'h008};
            bins high_conf_prefetch = {11'h010};
            ignore_bins vector = {11'h020};
            bins scalar = {11'h040};
            bins uncache = {11'h080};
            bins nc = {11'h100};
            bins low_conf_prefetch = {11'h400};
        }
        cp_rep_stall : coverpoint s0_rep_stall iff (scalar_s0_path_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cp_blocked_source : coverpoint blocked_source_class iff (scalar_s0_path_sample) {
            bins none = {0};
            bins misalign = {1};
            bins replay_tld = {2};
            bins fast_replay = {3};
            bins replay = {4};
            bins high_conf_prefetch = {5};
            ignore_bins vector = {6};
            bins scalar = {7};
            bins uncache = {8};
            bins nc = {9};
            bins low_conf_prefetch = {10};
            bins multiple = {11};
        }
        cp_dcache_ready : coverpoint io_dcache_req_ready iff (scalar_s0_path_sample) {
            bins blocked = {0};
            bins ready = {1};
        }
        cross cp_valid_count, cp_selected_source, cp_rep_stall;
        cross cp_selected_source, cp_dcache_ready;
    endgroup

    covergroup cg_ldu_scalar_s0_special_path @(posedge clock);
        option.per_instance = 1;
        cp_path_flags : coverpoint {
            s0_out_isHWPrefetch, s0_sel_src_isnc, s0_sel_src_frm_mabuf,
            s0_mmio_fire, s0_nc_fire
        } iff (scalar_s0_path_sample) {
            bins combinations[] = {[0:31]};
        }
        cp_shape : coverpoint {
            s0_addr_aligned, s0_rs_cross16Bytes,
            s0_misalignWith16Byte, s0_is128bit
        } iff (scalar_s0_path_sample) {
            bins combinations[] = {[0:15]};
        }
        cp_tlb_no_query : coverpoint s0_out_tlbNoQuery iff (scalar_s0_path_sample) {
            bins query = {0};
            bins no_query = {1};
        }
        cross cp_tlb_no_query, cp_shape;
    endgroup

    covergroup cg_ldu_scalar_dcache_request @(posedge clock);
        option.per_instance = 1;
        cp_valid : coverpoint io_dcache_req_valid iff (scalar_fcov_sample_enable) {
            bins no = {0};
            bins yes = {1};
        }
        cp_ready : coverpoint io_dcache_req_ready iff (scalar_fcov_sample_enable) {
            bins blocked = {0};
            bins accepted = {1};
        }
        cp_cmd : coverpoint io_dcache_req_cmd iff (scalar_dcache_req_sample) {
            bins load = {5'b00000};
            bins prefetch_read = {5'b00010};
            bins prefetch_write = {5'b00011};
            bins other = default;
        }
        cp_instr_type : coverpoint io_dcache_req_instrtype iff (scalar_dcache_req_sample) {
            bins normal = {0};
            bins other = default;
        }
        cp_addr_low : coverpoint io_dcache_req_vaddr[11:0] iff (scalar_dcache_req_sample) {
            bins page_start = {12'h000};
            bins low = {[12'h001:12'h7ff]};
            bins high = {[12'h800:12'hffe]};
            bins page_end = {12'hfff};
        }
        cp_cache_line_offset : coverpoint io_dcache_req_vaddr[5:0]
            iff (scalar_dcache_req_sample) {
            bins offset[] = {[0:63]};
        }
        cp_fu_op_type : coverpoint s0_sel_src_fuOpType iff (scalar_dcache_req_sample) {
            bins lb = {OP_LB};
            bins lh = {OP_LH};
            bins lw = {OP_LW};
            bins ld = {OP_LD};
            bins lbu = {OP_LBU};
            bins lhu = {OP_LHU};
            bins lwu = {OP_LWU};
            bins prefetch = {OP_PREFETCH_I, OP_PREFETCH_R, OP_PREFETCH_W};
            bins other = default;
        }
        cp_source : coverpoint source_selected_mask iff (scalar_dcache_req_sample) {
            bins misalign = {11'h001};
            bins replay_tld = {11'h002};
            bins fast_replay = {11'h004};
            bins replay = {11'h008};
            bins high_conf_prefetch = {11'h010};
            bins scalar = {11'h040};
            bins low_conf_prefetch = {11'h400};
        }
        cp_lq_flag : coverpoint io_dcache_req_lqIdx_flag iff (scalar_dcache_req_sample) {
            bins flag[] = {0,1};
        }
        cp_lq_value : coverpoint io_dcache_req_lqIdx_value iff (scalar_dcache_req_sample) {
            bins zero = {0};
            bins middle = {[1:126]};
            bins max = {127};
        }
        cp_is128 : coverpoint io_dcache_is128Req iff (scalar_dcache_req_sample) {
            bins normal = {0};
            bins wide = {1};
        }
        cp_tlb_no_query : coverpoint s0_out_tlbNoQuery iff (scalar_s0_sample) {
            bins query = {0};
            bins no_query = {1};
        }
        cp_alignment : coverpoint {s0_addr_aligned, s0_rs_cross16Bytes, s0_misalignWith16Byte} iff (scalar_s0_sample) {
            bins aligned = {3'b100};
            bins misalign_16b = {3'b000};
            bins cross_16b = {3'b011};
            bins other = default;
        }
        cross cp_cmd, cp_is128, cp_alignment;
        cross cp_fu_op_type, cp_is128;
        cross cp_source, cp_ready;
        cross cp_source, cp_alignment;
        cross cp_ready, cp_valid;
    endgroup

    covergroup cg_ldu_scalar_s1_kill @(posedge clock);
        option.per_instance = 1;
        cp_s1_kill : coverpoint s1_kill iff (scalar_s1_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cp_tlb_miss : coverpoint s1_tlb_miss iff (scalar_s1_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cp_misalign_kill : coverpoint s1_misalign_kill iff (scalar_s1_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cp_dcache_kill : coverpoint io_dcache_s1_kill iff (scalar_s1_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cp_data_read_kill : coverpoint io_dcache_s1_kill_data_read iff (scalar_s1_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cp_trigger : coverpoint {
            s1_trigger_debug_mode, s1_trigger_breakpoint
        } iff (scalar_s1_sample) {
            bins none = {2'b00};
            bins breakpoint = {2'b01};
            bins debug_mode = {2'b10};
            bins both = {2'b11};
        }
        cp_redirect : coverpoint io_redirect_valid iff (scalar_s1_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cross cp_s1_kill, cp_tlb_miss, cp_misalign_kill;
        cross cp_dcache_kill, cp_data_read_kill;
        cross cp_s1_kill, cp_redirect;
    endgroup

    covergroup cg_ldu_scalar_tlb @(posedge clock);
        option.per_instance = 1;
        cp_req_cmd : coverpoint io_tlb_req_cmd iff (scalar_tlb_req_sample) {
            bins read = {0};
            bins write = {1};
            bins other = default;
        }
        cp_req_flags : coverpoint {
            io_tlb_req_bits_checkfullva, io_tlb_req_bits_hyperinst,
            io_tlb_req_bits_hlvx, io_tlb_req_bits_isPrefetch,
            io_tlb_req_bits_no_translate, io_tlb_req_bits_kill
        } iff (scalar_tlb_req_sample) {
            bins combinations[] = {[0:63]};
        }
        cp_req_addr_low : coverpoint io_tlb_req_vaddr[11:0] iff (scalar_tlb_req_sample) {
            bins page_start = {12'h000};
            bins low = {[12'h001:12'h7ff]};
            bins high = {[12'h800:12'hffe]};
            bins page_end = {12'hfff};
        }
        cp_req_source : coverpoint source_selected_mask iff (scalar_tlb_req_sample) {
            bins misalign = {11'h001};
            bins replay_tld = {11'h002};
            bins replay = {11'h008};
            bins scalar = {11'h040};
        }
        cp_resp_miss : coverpoint io_tlb_resp_bits_miss iff (scalar_tlb_resp_sample) {
            bins hit = {0};
            bins miss = {1};
        }
        cp_pbmt : coverpoint io_tlb_resp_bits_pbmt iff (scalar_tlb_resp_sample) {
            bins pbmt[] = {[0:2]};
            bins other = default;
        }
        cp_fault_kind : coverpoint {
            io_tlb_resp_bits_excp_gpf, io_tlb_resp_bits_excp_pf,
            io_tlb_resp_bits_excp_af
        } iff (scalar_tlb_resp_sample) {
            bins none = {3'b000};
            bins gpf = {3'b100};
            bins pf = {3'b010};
            bins af = {3'b001};
            bins multiple = default;
        }
        cross cp_req_cmd, cp_resp_miss;
        cross cp_req_cmd, cp_req_source;
        cross cp_resp_miss, cp_pbmt, cp_fault_kind;
    endgroup

    covergroup cg_ldu_scalar_pmp @(posedge clock);
        option.per_instance = 1;
        cp_ld : coverpoint io_pmp_ld iff (scalar_s2_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cp_st : coverpoint io_pmp_st iff (scalar_s2_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cp_instr : coverpoint io_pmp_instr iff (scalar_s2_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cp_mmio : coverpoint io_pmp_mmio iff (scalar_s2_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cross cp_ld, cp_mmio;
        cross cp_ld, cp_st, cp_mmio;
    endgroup

    covergroup cg_ldu_scalar_dcache_response @(posedge clock);
        option.per_instance = 1;
        cp_class : coverpoint dcache_response_class iff (scalar_s2_sample) {
            bins hit = {0};
            bins hit_bank_conflict = {1};
            bins miss_handled = {2};
            bins miss_mq_nack = {3};
            bins miss_other = {4};
            bins denied = {5};
            bins corrupt = {6};
            bins denied_corrupt = {7};
        }
        cp_miss : coverpoint io_dcache_resp_bits_miss iff (scalar_s2_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cp_handled : coverpoint io_dcache_resp_bits_handled iff (scalar_s2_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cp_bank : coverpoint io_dcache_s2_bank_conflict iff (scalar_s2_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cp_mq_nack : coverpoint io_dcache_s2_mq_nack iff (scalar_s2_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cp_mshr : coverpoint io_dcache_resp_bits_mshr_id iff (scalar_s2_sample) {
            bins id[] = {[0:15]};
        }
        cp_meta_prefetch : coverpoint io_dcache_resp_bits_meta_prefetch iff (scalar_s2_sample) {
            bins meta[] = {[0:7]};
        }
        cp_error : coverpoint {
            io_dcache_resp_bits_denied, io_dcache_resp_bits_corrupt
        } iff (scalar_s2_sample) {
            bins none = {2'b00};
            bins denied = {2'b10};
            bins corrupt = {2'b01};
            bins both = {2'b11};
        }
        cp_fu_op_type : coverpoint s2_in_r_fuOpType iff (scalar_s2_sample) {
            bins lb = {OP_LB};
            bins lh = {OP_LH};
            bins lw = {OP_LW};
            bins ld = {OP_LD};
            bins lbu = {OP_LBU};
            bins lhu = {OP_LHU};
            bins lwu = {OP_LWU};
            bins other = default;
        }
        cp_source : coverpoint s2_source_class iff (scalar_s2_sample) {
            bins normal = {0};
            bins replay = {1};
            bins fast_replay = {2};
            bins nc = {3};
            bins mmio = {4};
            bins prefetch = {5};
            bins misalign_return = {6};
        }
        cp_is128 : coverpoint s2_in_r_is128bit iff (scalar_s2_sample) {
            bins normal = {0};
            bins wide = {1};
        }
        cross cp_class, cp_mshr;
        cross cp_class, cp_error;
        cross cp_class, cp_source, cp_is128;
        cross cp_miss, cp_handled, cp_bank, cp_mq_nack;
    endgroup

    covergroup cg_ldu_scalar_forward_query @(posedge clock);
        option.per_instance = 1;
        cp_lsq_valid : coverpoint io_lsq_forward_valid iff (scalar_s2_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cp_lsq_kind : coverpoint lsq_forward_kind iff (scalar_s2_sample) {
            bins none = {0};
            bins partial = {1};
            bins full = {2};
        }
        cp_sbuffer_kind : coverpoint sbuffer_forward_kind iff (scalar_s2_sample) {
            bins none = {0};
            bins partial = {1};
            bins full = {2};
        }
        cp_ubuffer_kind : coverpoint ubuffer_forward_kind iff (scalar_s2_sample) {
            bins none = {0};
            bins partial = {1};
            bins full = {2};
        }
        cp_merged_kind : coverpoint merged_forward_kind iff (scalar_s2_sample) {
            bins none = {0};
            bins partial = {1};
            bins full = {2};
        }
        cp_invalid : coverpoint {
            io_lsq_forward_dataInvalid, io_lsq_forward_matchInvalid,
            io_lsq_forward_addrInvalid, io_sbuffer_matchInvalid,
            io_ubuffer_matchInvalid
        } iff (scalar_s2_sample) {
            bins none = {5'b00000};
            bins some = default;
        }
        cp_full_fwd : coverpoint s2_full_fwd iff (scalar_s2_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cp_mem_amb : coverpoint s2_mem_amb iff (scalar_s2_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cp_fwd_fail : coverpoint s2_fwd_fail iff (scalar_s2_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cross cp_lsq_kind, cp_sbuffer_kind, cp_ubuffer_kind;
        cross cp_merged_kind, cp_full_fwd, cp_invalid;
    endgroup

    covergroup cg_ldu_scalar_sbuffer_ubuffer @(posedge clock);
        option.per_instance = 1;
        cp_request_valid : coverpoint {io_sbuffer_valid, io_ubuffer_valid} iff (scalar_s1_sample) {
            bins none = {2'b00};
            bins sbuffer = {2'b10};
            bins ubuffer = {2'b01};
            bins both = {2'b11};
        }
        cp_sbuffer_kind : coverpoint sbuffer_forward_kind iff (scalar_s2_sample) {
            bins none = {0};
            bins partial = {1};
            bins full = {2};
        }
        cp_sbuffer_popcount : coverpoint sbuffer_forward_popcount_class iff (scalar_s2_sample) {
            bins none = {0};
            bins one = {1};
            bins sparse = {2};
            bins dense = {3};
            bins full = {4};
        }
        cp_sbuffer_lanes : coverpoint {
            |io_sbuffer_forwardMask[15:8], |io_sbuffer_forwardMask[7:0]
        } iff (scalar_s2_sample) {
            bins none = {2'b00};
            bins low = {2'b01};
            bins high = {2'b10};
            bins both = {2'b11};
        }
        cp_ubuffer_kind : coverpoint ubuffer_forward_kind iff (scalar_s2_sample) {
            bins none = {0};
            bins partial = {1};
            bins full = {2};
        }
        cp_ubuffer_popcount : coverpoint ubuffer_forward_popcount_class iff (scalar_s2_sample) {
            bins none = {0};
            bins one = {1};
            bins sparse = {2};
            bins dense = {3};
            bins full = {4};
        }
        cp_ubuffer_lanes : coverpoint {
            |io_ubuffer_forwardMask[15:8], |io_ubuffer_forwardMask[7:0]
        } iff (scalar_s2_sample) {
            bins none = {2'b00};
            bins low = {2'b01};
            bins high = {2'b10};
            bins both = {2'b11};
        }
        cp_match_invalid : coverpoint {
            io_sbuffer_matchInvalid, io_ubuffer_matchInvalid
        } iff (scalar_s2_sample) {
            bins combinations[] = {[0:3]};
        }
        cross cp_sbuffer_kind, cp_ubuffer_kind, cp_match_invalid;
    endgroup

    covergroup cg_ldu_scalar_storequeue @(posedge clock);
        option.per_instance = 1;
        cp_request_valid : coverpoint io_lsq_forward_valid iff (scalar_s1_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cp_forward_kind : coverpoint lsq_forward_kind iff (scalar_s2_sample) {
            bins none = {0};
            bins partial = {1};
            bins full = {2};
        }
        cp_forward_popcount : coverpoint lsq_forward_popcount_class iff (scalar_s2_sample) {
            bins none = {0};
            bins one = {1};
            bins sparse = {2};
            bins dense = {3};
            bins full = {4};
        }
        cp_forward_lanes : coverpoint {
            |io_lsq_forward_forwardMask[15:8], |io_lsq_forward_forwardMask[7:0]
        } iff (scalar_s2_sample) {
            bins none = {2'b00};
            bins low = {2'b01};
            bins high = {2'b10};
            bins both = {2'b11};
        }
        cp_invalid : coverpoint {
            io_lsq_forward_matchInvalid,
            io_lsq_forward_addrInvalid,
            io_lsq_forward_dataInvalid
        } iff (scalar_s2_sample) {
            bins combinations[] = {[0:7]};
        }
        cross cp_forward_kind, cp_invalid;
    endgroup

    covergroup cg_ldu_scalar_lsq_query @(posedge clock);
        option.per_instance = 1;
        cp_stld : coverpoint {
            io_lsq_stld_nuke_query_req_valid, io_lsq_stld_nuke_query_req_ready,
            io_lsq_stld_nuke_query_req_bits_data_valid
        } iff (scalar_s2_sample) {
            bins combinations[] = {[0:7]};
        }
        cp_rar : coverpoint {
            io_lsq_ldld_nuke_query_req_valid, io_lsq_ldld_nuke_query_req_ready,
            io_lsq_ldld_nuke_query_req_bits_data_valid, io_lsq_ldld_nuke_query_req_bits_is_nc
        } iff (scalar_s2_sample) {
            bins combinations[] = {[0:15]};
        }
        cp_revoke : coverpoint {
            io_lsq_stld_nuke_query_revoke, io_lsq_ldld_nuke_query_revoke
        } iff (scalar_s3_sample) {
            bins none = {2'b00};
            bins stld = {2'b10};
            bins rar = {2'b01};
            bins both = {2'b11};
        }
        cross cp_stld, cp_rar;
    endgroup

    covergroup cg_ldu_scalar_storeunit_nuke @(posedge clock);
        option.per_instance = 1;
        cp_query_valids : coverpoint {
            io_stld_nuke_query_1_valid, io_stld_nuke_query_0_valid
        } iff (scalar_stage_sample) {
            bins combinations[] = {[0:3]};
        }
        cp_query_0_match_type : coverpoint io_stld_nuke_query_0_matchType
            iff (scalar_stage_sample && io_stld_nuke_query_0_valid) {
            bins match_type[] = {[0:3]};
        }
        cp_query_1_match_type : coverpoint io_stld_nuke_query_1_matchType
            iff (scalar_stage_sample && io_stld_nuke_query_1_valid) {
            bins match_type[] = {[0:3]};
        }
        cp_stage_valids : coverpoint {scalar_s2_sample, scalar_s1_sample}
            iff (scalar_stage_sample) {
            bins s1 = {2'b01};
            bins s2 = {2'b10};
            bins both = {2'b11};
        }
        cp_nuke : coverpoint {s2_nuke, s2_in_r_rep_info_cause_9}
            iff (scalar_s2_sample) {
            bins none = {2'b00};
            bins captured_s1 = {2'b01};
            bins current_s2 = {2'b10};
            bins both = {2'b11};
        }
        cross cp_query_valids, cp_stage_valids;
    endgroup

    covergroup cg_ldu_scalar_csr_control @(posedge clock);
        option.per_instance = 1;
        cp_ldld_vio_enable : coverpoint io_csrCtrl_ldld_vio_check_enable
            iff (scalar_stage_sample) {
            bins disabled = {0};
            bins enabled = {1};
        }
        cp_misalign_enable : coverpoint io_csrCtrl_hd_misalign_ld_enable
            iff (scalar_stage_sample) {
            bins disabled = {0};
            bins enabled = {1};
        }
        cross cp_ldld_vio_enable, cp_misalign_enable;
    endgroup

    covergroup cg_ldu_scalar_wakeup @(posedge clock);
        option.per_instance = 1;
        cp_valid : coverpoint io_wakeup_valid iff (scalar_s0_path_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cp_source : coverpoint source_selected_mask iff (scalar_wakeup_sample) {
            bins misalign = {11'h001};
            bins replay_tld = {11'h002};
            bins fast_replay = {11'h004};
            bins replay = {11'h008};
            bins scalar = {11'h040};
            bins uncache = {11'h080};
            bins nc = {11'h100};
            ignore_bins prefetch = {11'h010, 11'h400};
        }
        cp_destination : coverpoint {io_wakeup_rfWen, io_wakeup_fpWen}
            iff (scalar_wakeup_sample) {
            bins none = {2'b00};
            bins int_dest = {2'b10};
            bins fp_dest = {2'b01};
            bins both = {2'b11};
        }
        cross cp_source, cp_destination;
    endgroup

    covergroup cg_ldu_scalar_replay @(posedge clock);
        option.per_instance = 1;
        cp_cause_count : coverpoint cause_count iff (scalar_lsq_replay_sample) {
            bins none = {0};
            bins one = {1};
            bins multiple = {[2:11]};
        }
        cp_cause_mask : coverpoint io_lsq_ldin_rep_cause iff (scalar_lsq_replay_sample) {
            bins none = {11'b0};
            bins some = {[1:2047]};
        }
        cp_cause_class : coverpoint replay_cause_class iff (scalar_lsq_replay_sample) {
            bins none = {0};
            bins memory_ambiguity = {1};
            bins tlb_miss = {2};
            bins forward_fail = {3};
            bins dcache_replay_or_mq_nack = {4};
            bins dcache_miss = {5};
            bins wpu_prediction_fail = {6};
            bins dcache_bank_conflict = {7};
            bins rar_nack = {8};
            bins raw_nack = {9};
            bins stld_nuke = {10};
            bins misalign_buffer_nack = {11};
            bins multiple = {12};
        }
        cp_fast_replay_in : coverpoint io_fast_rep_in_valid iff (scalar_s0_path_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cp_fast_replay : coverpoint io_fast_rep_out_valid iff (scalar_s3_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cp_safe_wakeup : coverpoint s2_safe_wakeup iff (scalar_s2_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cp_safe_writeback : coverpoint s3_safe_writeback iff (scalar_s3_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cp_exception : coverpoint s2_real_exception iff (scalar_s2_sample) {
            bins no = {0};
            bins yes = {1};
        }
    endgroup

    covergroup cg_ldu_scalar_misalign @(posedge clock);
        option.per_instance = 1;
        cp_enqueue : coverpoint {
            io_misalign_enq_req_valid, io_misalign_enq_req_ready
        } iff (scalar_s3_sample) {
            bins idle = {2'b00};
            bins blocked = {2'b10};
            bins accepted = {2'b11};
            bins ready_idle = {2'b01};
        }
        cp_return : coverpoint {
            io_misalign_ldout_valid,
            s3_in_isFrmMisAlignBuf,
            s3_misalign_wakeup_req_valid
        } iff (scalar_fcov_sample_enable &&
               (scalar_s3_sample || s3_misalign_wakeup_req_valid)) {
            bins combinations[] = {[0:7]};
        }
        cp_can_go : coverpoint s3_misalign_can_go
            iff (scalar_s3_sample && io_misalign_enq_req_valid) {
            bins blocked = {0};
            bins allowed = {1};
        }
    endgroup

    covergroup cg_ldu_scalar_rollback @(posedge clock);
        option.per_instance = 1;
        cp_valid : coverpoint io_rollback_valid iff (scalar_s3_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cp_level : coverpoint io_rollback_level iff (scalar_rollback_sample) {
            bins flush_after = {0};
            bins flush_self = {1};
        }
        cp_exception_overlap : coverpoint {io_rollback_valid, s3_exception}
            iff (scalar_s3_sample) {
            bins neither = {2'b00};
            bins exception_only = {2'b01};
            bins rollback_only = {2'b10};
            illegal_bins overlap = {2'b11};
        }
    endgroup

    covergroup cg_ldu_scalar_ldout @(posedge clock);
        option.per_instance = 1;
        cp_exception : coverpoint io_ldout_exceptionVec iff (scalar_ldout_fire) {
            bins none = {6'b0};
            bins nonzero = default;
        }
        cp_rf_fp_wen : coverpoint {io_ldout_rfWen, io_ldout_fpWen} iff (scalar_ldout_fire) {
            bins none = {2'b00};
            bins int_wb = {2'b10};
            bins fp = {2'b01};
            bins both = {2'b11};
        }
        cp_pdest : coverpoint io_ldout_pdest iff (scalar_ldout_fire) {
            bins zero = {0};
            bins low = {[1:127]};
            bins high = {[128:255]};
        }
        cp_rob_flag : coverpoint io_ldout_robIdx_flag iff (scalar_ldout_fire) {
            bins flag[] = {0,1};
        }
        cp_rob_value : coverpoint io_ldout_robIdx_value iff (scalar_ldout_fire) {
            bins zero = {0};
            bins middle = {[1:254]};
            bins max = {255};
        }
        cp_replay : coverpoint io_ldout_replayInst iff (scalar_ldout_fire) {
            bins no = {0};
            bins yes = {1};
        }
        cp_mmio_nc : coverpoint {io_ldout_debug_isMMIO, io_ldout_debug_isNCIO} iff (scalar_ldout_fire) {
            bins normal = {2'b00};
            bins mmio = {2'b10};
            bins nc = {2'b01};
            bins both = {2'b11};
        }
        cross cp_exception, cp_rf_fp_wen;
        cross cp_replay, cp_mmio_nc;
    endgroup

    covergroup cg_ldu_scalar_output_route @(posedge clock);
        option.per_instance = 1;
        cp_route_count : coverpoint output_route_count iff (scalar_s3_sample) {
            bins none = {0};
            bins one = {1};
            bins multiple = {[2:5]};
        }
        cp_route_mask : coverpoint output_route_mask iff (scalar_s3_sample) {
            bins combinations[] = {[0:31]};
        }
        cp_lsq_update : coverpoint io_lsq_ldin_valid iff (scalar_s3_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cp_cancel : coverpoint io_ldCancel_ld2Cancel iff (scalar_s3_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cp_s3_safe_wb : coverpoint s3_safe_writeback iff (scalar_s3_sample) {
            bins no = {0};
            bins yes = {1};
        }
        cross cp_route_count, cp_s3_safe_wb;
    endgroup

    cg_ldu_scalar_ldin ldin_cg;
    cg_ldu_scalar_source source_cg;
    cg_ldu_scalar_s0_special_path s0_special_path_cg;
    cg_ldu_scalar_dcache_request dcache_req_cg;
    cg_ldu_scalar_s1_kill s1_kill_cg;
    cg_ldu_scalar_tlb tlb_cg;
    cg_ldu_scalar_pmp pmp_cg;
    cg_ldu_scalar_dcache_response dcache_resp_cg;
    cg_ldu_scalar_forward_query forward_cg;
    cg_ldu_scalar_sbuffer_ubuffer sbuffer_ubuffer_cg;
    cg_ldu_scalar_storequeue storequeue_cg;
    cg_ldu_scalar_lsq_query lsq_query_cg;
    cg_ldu_scalar_storeunit_nuke storeunit_nuke_cg;
    cg_ldu_scalar_csr_control csr_control_cg;
    cg_ldu_scalar_wakeup wakeup_cg;
    cg_ldu_scalar_replay replay_cg;
    cg_ldu_scalar_misalign misalign_cg;
    cg_ldu_scalar_rollback rollback_cg;
    cg_ldu_scalar_ldout ldout_cg;
    cg_ldu_scalar_output_route output_route_cg;

    initial begin
        ldin_cg = new();
        source_cg = new();
        s0_special_path_cg = new();
        dcache_req_cg = new();
        s1_kill_cg = new();
        tlb_cg = new();
        pmp_cg = new();
        dcache_resp_cg = new();
        forward_cg = new();
        sbuffer_ubuffer_cg = new();
        storequeue_cg = new();
        lsq_query_cg = new();
        storeunit_nuke_cg = new();
        csr_control_cg = new();
        wakeup_cg = new();
        replay_cg = new();
        misalign_cg = new();
        rollback_cg = new();
        ldout_cg = new();
        output_route_cg = new();
    end
endmodule: memblock_ldu_scalar_fcov
`endif

`endif
