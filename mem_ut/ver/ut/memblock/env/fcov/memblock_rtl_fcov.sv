//=========================================================
// MemBlock RTL functional coverage for the P0 unit-stride path.
//
// This file intentionally contains only static interface coverage.  A sample
// represents the signals observed in one clock cycle; no issue/writeback
// context table, latency model, replay tracker, or scoreboard state is kept.
//=========================================================
`ifndef MEMBLOCK_RTL_FCOV__SV
`define MEMBLOCK_RTL_FCOV__SV

`ifdef MEMBLOCK_UT_FCOV
module memblock_rtl_fcov (
    input wire clock,
    input wire reset,
    input wire reset_backend_done,

    input wire io_ooo_to_mem_issueVldu_0_valid,
    input wire io_ooo_to_mem_issueVldu_0_ready,
    input wire [34:0] io_ooo_to_mem_issueVldu_0_bits_uop_fuType,
    input wire [8:0] io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType,
    input wire io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vma,
    input wire io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vta,
    input wire [1:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vsew,
    input wire [2:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vlmul,
    input wire io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vm,
    input wire [7:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vstart,
    input wire [6:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vuopIdx,
    input wire io_ooo_to_mem_issueVldu_0_bits_uop_vpu_lastUop,
    input wire [127:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vmask,
    input wire [2:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_nf,
    input wire [1:0] io_ooo_to_mem_issueVldu_0_bits_uop_vpu_veew,
    input wire io_ooo_to_mem_issueVldu_0_bits_uop_vpu_isVleff,
    input wire [7:0] io_ooo_to_mem_issueVldu_0_bits_uop_robIdx_value,
    input wire [127:0] io_ooo_to_mem_issueVldu_0_bits_src_0,
    input wire [127:0] io_ooo_to_mem_issueVldu_0_bits_src_2,
    input wire [127:0] io_ooo_to_mem_issueVldu_0_bits_src_3,
    input wire [127:0] io_ooo_to_mem_issueVldu_0_bits_src_4,
    input wire [4:0] io_ooo_to_mem_issueVldu_0_bits_flowNum,
    input wire io_ooo_to_mem_issueVldu_0_bits_isVecPartReplay,

    input wire io_ooo_to_mem_issueVldu_1_valid,
    input wire io_ooo_to_mem_issueVldu_1_ready,
    input wire [8:0] io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType,
    input wire io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vma,
    input wire io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vta,
    input wire [1:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vsew,
    input wire [2:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vlmul,
    input wire io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vm,
    input wire [7:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vstart,
    input wire [6:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vuopIdx,
    input wire io_ooo_to_mem_issueVldu_1_bits_uop_vpu_lastUop,
    input wire [127:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vmask,
    input wire [2:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_nf,
    input wire [1:0] io_ooo_to_mem_issueVldu_1_bits_uop_vpu_veew,
    input wire io_ooo_to_mem_issueVldu_1_bits_uop_vpu_isVleff,
    input wire [7:0] io_ooo_to_mem_issueVldu_1_bits_uop_robIdx_value,
    input wire [127:0] io_ooo_to_mem_issueVldu_1_bits_src_0,
    input wire [127:0] io_ooo_to_mem_issueVldu_1_bits_src_2,
    input wire [127:0] io_ooo_to_mem_issueVldu_1_bits_src_3,
    input wire [127:0] io_ooo_to_mem_issueVldu_1_bits_src_4,
    input wire [4:0] io_ooo_to_mem_issueVldu_1_bits_flowNum,
    input wire io_ooo_to_mem_issueVldu_1_bits_isVecPartReplay,

    input wire io_mem_to_ooo_writebackVldu_0_valid,
    input wire [9:0] io_mem_to_ooo_writebackVldu_0_exceptionVec,
    input wire [8:0] io_mem_to_ooo_writebackVldu_0_bits_uop_fuOpType,
    input wire io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vma,
    input wire io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vta,
    input wire [1:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vsew,
    input wire [2:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vlmul,
    input wire io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vm,
    input wire [7:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vstart,
    input wire [6:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vuopIdx,
    input wire [127:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vmask,
    input wire [7:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vl,
    input wire [2:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_nf,
    input wire [1:0] io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_veew,
    input wire [7:0] io_mem_to_ooo_writebackVldu_0_bits_uop_pdest,
    input wire io_mem_to_ooo_writebackVldu_0_bits_uop_robIdx_flag,
    input wire [7:0] io_mem_to_ooo_writebackVldu_0_bits_uop_robIdx_value,
    input wire io_mem_to_ooo_writebackVldu_0_bits_uop_replayInst,
    input wire [127:0] io_mem_to_ooo_writebackVldu_0_bits_data,
    input wire [2:0] io_mem_to_ooo_writebackVldu_0_bits_vdIdxInField,

    input wire io_mem_to_ooo_writebackVldu_1_valid,
    input wire [9:0] io_mem_to_ooo_writebackVldu_1_exceptionVec,
    input wire [8:0] io_mem_to_ooo_writebackVldu_1_bits_uop_fuOpType,
    input wire io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vma,
    input wire io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vta,
    input wire [1:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vsew,
    input wire [2:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vlmul,
    input wire io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vm,
    input wire [7:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vstart,
    input wire [6:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vuopIdx,
    input wire [127:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vmask,
    input wire [7:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vl,
    input wire [2:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_nf,
    input wire [1:0] io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_veew,
    input wire [7:0] io_mem_to_ooo_writebackVldu_1_bits_uop_pdest,
    input wire io_mem_to_ooo_writebackVldu_1_bits_uop_robIdx_flag,
    input wire [7:0] io_mem_to_ooo_writebackVldu_1_bits_uop_robIdx_value,
    input wire io_mem_to_ooo_writebackVldu_1_bits_uop_replayInst,
    input wire [127:0] io_mem_to_ooo_writebackVldu_1_bits_data,
    input wire [2:0] io_mem_to_ooo_writebackVldu_1_bits_vdIdxInField,

    input wire io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_valid,
    input wire io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_hit,
    input wire io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_isVecPartReplay,
    input wire [15:0] io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_vecReplayMask,
    input wire [3:0] io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_vecReplayMbIdx,
    input wire io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_valid,
    input wire io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_hit,
    input wire io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_isVecPartReplay,
    input wire [15:0] io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_vecReplayMask,
    input wire [3:0] io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_vecReplayMbIdx,

    input wire io_redirect_valid,
    input wire io_redirect_bits_level,
    input wire io_redirect_bits_isVlsException,
    input wire [7:0] io_redirect_bits_robIdx_value
);

    localparam [8:0] VLE_OP = 9'b010000000;
    localparam [8:0] VSE_OP = 9'b100000000;

    // V2 lane1 fuType is optimized out in the generated DUT.  Keep the same
    // source-level classification used by the bind file for static coverage.
    wire [34:0] issue_vldu_1_fu_type_derived =
        (io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType == VLE_OP) ? (35'b1 << 31) :
        (io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType == VSE_OP) ? (35'b1 << 32) : 35'b0;

    wire issue_vldu_0_p0_candidate =
        ((io_ooo_to_mem_issueVldu_0_bits_uop_fuType[31] &&
          (io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType == VLE_OP)) ||
         (io_ooo_to_mem_issueVldu_0_bits_uop_fuType[32] &&
          (io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType == VSE_OP))) &&
        (io_ooo_to_mem_issueVldu_0_bits_uop_vpu_nf == 3'b000) &&
        !io_ooo_to_mem_issueVldu_0_bits_uop_vpu_isVleff;

    wire issue_vldu_1_p0_candidate =
        (((issue_vldu_1_fu_type_derived[31]) &&
          (io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType == VLE_OP)) ||
         ((issue_vldu_1_fu_type_derived[32]) &&
          (io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType == VSE_OP))) &&
        (io_ooo_to_mem_issueVldu_1_bits_uop_vpu_nf == 3'b000) &&
        !io_ooo_to_mem_issueVldu_1_bits_uop_vpu_isVleff;

    wire issue_vldu_0_fire = issue_vldu_0_p0_candidate &&
                              io_ooo_to_mem_issueVldu_0_valid &&
                              io_ooo_to_mem_issueVldu_0_ready;
    wire issue_vldu_1_fire = issue_vldu_1_p0_candidate &&
                              io_ooo_to_mem_issueVldu_1_valid &&
                              io_ooo_to_mem_issueVldu_1_ready;

    // The top-level V2 writeback interface has valid but no ready.  Therefore
    // valid is the observable completion event used by the static covergroup.
    wire writeback_vldu_0_p0 = io_mem_to_ooo_writebackVldu_0_valid &&
                               ((io_mem_to_ooo_writebackVldu_0_bits_uop_fuOpType == VLE_OP) ||
                                (io_mem_to_ooo_writebackVldu_0_bits_uop_fuOpType == VSE_OP));
    wire writeback_vldu_1_p0 = io_mem_to_ooo_writebackVldu_1_valid &&
                               ((io_mem_to_ooo_writebackVldu_1_bits_uop_fuOpType == VLE_OP) ||
                                (io_mem_to_ooo_writebackVldu_1_bits_uop_fuOpType == VSE_OP));

    wire issue_vldu_0_sample = (reset === 1'b0) &&
                                (reset_backend_done === 1'b1) &&
                                issue_vldu_0_fire;
    wire issue_vldu_1_sample = (reset === 1'b0) &&
                                (reset_backend_done === 1'b1) &&
                                issue_vldu_1_fire;
    wire issue_vldu_0_ready_sample = (reset === 1'b0) &&
                                     (reset_backend_done === 1'b1) &&
                                     issue_vldu_0_p0_candidate;
    wire issue_vldu_1_ready_sample = (reset === 1'b0) &&
                                     (reset_backend_done === 1'b1) &&
                                     issue_vldu_1_p0_candidate;
    wire writeback_vldu_0_sample = (reset === 1'b0) &&
                                   (reset_backend_done === 1'b1) &&
                                   writeback_vldu_0_p0;
    wire writeback_vldu_1_sample = (reset === 1'b0) &&
                                   (reset_backend_done === 1'b1) &&
                                   writeback_vldu_1_p0;
    wire feedback_0_sample = (reset === 1'b0) &&
                             (reset_backend_done === 1'b1) &&
                             io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_valid;
    wire feedback_1_sample = (reset === 1'b0) &&
                             (reset_backend_done === 1'b1) &&
                             io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_valid;
    wire redirect_sample = (reset === 1'b0) &&
                           (reset_backend_done === 1'b1) &&
                           io_redirect_valid;

    covergroup cg_us_top_issue_0 @(posedge clock);
        option.per_instance = 1;

        cp_op_kind : coverpoint (io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType == VLE_OP)
            iff (issue_vldu_0_sample) {
            bins store = {1'b0};
            bins load  = {1'b1};
        }
        cp_fu_op_type : coverpoint io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType
            iff (issue_vldu_0_sample) {
            bins vle = {VLE_OP};
            bins vse = {VSE_OP};
        }
        cp_eew : coverpoint io_ooo_to_mem_issueVldu_0_bits_uop_vpu_veew
            iff (issue_vldu_0_sample) { bins eew[] = {[0:3]}; }
        cp_vsew : coverpoint io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vsew
            iff (issue_vldu_0_sample) { bins vsew[] = {[0:3]}; }
        cp_vlmul : coverpoint io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vlmul
            iff (issue_vldu_0_sample) { bins lmul[] = {[0:7]}; }
        cp_base_offset : coverpoint io_ooo_to_mem_issueVldu_0_bits_src_0[3:0]
            iff (issue_vldu_0_sample) { bins offsets[] = {[0:15]}; }
        cp_align_class : coverpoint io_ooo_to_mem_issueVldu_0_bits_src_0[3:0]
            iff (issue_vldu_0_sample) {
            bins aligned_16B = {0};
            bins unaligned_16B = {[1:15]};
        }
        // Static coverage records raw VL/vstart ranges.  Exact semantic
        // partial/full/cross-uop classification requires temporal state.
        cp_vl_class : coverpoint io_ooo_to_mem_issueVldu_0_bits_src_4[7:0]
            iff (issue_vldu_0_sample) {
            bins zero = {0};
            bins one = {1};
            bins short = {[2:15]};
            bins vl_16_31 = {[16:31]};
            bins long = {[32:255]};
        }
        cp_vstart_class : coverpoint io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vstart
            iff (issue_vldu_0_sample) {
            bins zero = {0};
            bins one = {1};
            bins middle = {[2:254]};
            bins max = {255};
        }
        cp_vm : coverpoint io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vm
            iff (issue_vldu_0_sample) {
            bins masked = {1'b0};
            bins unmasked = {1'b1};
        }
        cp_mask_zero : coverpoint
            (io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vmask == 128'b0)
            iff (issue_vldu_0_sample) {
            bins nonzero = {1'b0};
            bins all_off = {1'b1};
        }
        cp_mask_all_one : coverpoint
            (io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vmask == {128{1'b1}})
            iff (issue_vldu_0_sample) {
            bins other = {1'b0};
            bins all_on = {1'b1};
        }
        cp_vma : coverpoint io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vma
            iff (issue_vldu_0_sample && (io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType == VLE_OP)) {
            bins undisturbed = {1'b0};
            bins agnostic = {1'b1};
        }
        cp_vta : coverpoint io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vta
            iff (issue_vldu_0_sample && (io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType == VLE_OP)) {
            bins undisturbed = {1'b0};
            bins agnostic = {1'b1};
        }
        cp_vuop_idx : coverpoint io_ooo_to_mem_issueVldu_0_bits_uop_vpu_vuopIdx
            iff (issue_vldu_0_sample) { bins field[] = {[0:127]}; }
        cp_last_uop : coverpoint io_ooo_to_mem_issueVldu_0_bits_uop_vpu_lastUop
            iff (issue_vldu_0_sample) {
            bins non_last = {1'b0};
            bins last = {1'b1};
        }
        cp_flow_budget : coverpoint io_ooo_to_mem_issueVldu_0_bits_flowNum
            iff (issue_vldu_0_sample) {
            bins zero = {0};
            bins one = {1};
            bins legal_two = {2};
            bins other = {[3:31]};
        }
        cp_store_data_zero : coverpoint
            (io_ooo_to_mem_issueVldu_0_bits_src_2 == 128'b0)
            iff (issue_vldu_0_sample && (io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType == VSE_OP)) {
            bins nonzero = {1'b0};
            bins all_zero = {1'b1};
        }
        cp_store_data_one : coverpoint
            (io_ooo_to_mem_issueVldu_0_bits_src_2 == {128{1'b1}})
            iff (issue_vldu_0_sample && (io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType == VSE_OP)) {
            bins other = {1'b0};
            bins all_one = {1'b1};
        }
        cp_replay : coverpoint io_ooo_to_mem_issueVldu_0_bits_isVecPartReplay
            iff (issue_vldu_0_sample) {
            bins no = {1'b0};
            bins yes = {1'b1};
        }

        cross cp_op_kind, cp_eew, cp_align_class;
        cross cp_vsew, cp_eew, cp_align_class;
        cross cp_op_kind, cp_vl_class, cp_vstart_class;
        cross cp_op_kind, cp_vm, cp_align_class;
        cross cp_vma, cp_vta, cp_vm;
        cross cp_last_uop, cp_op_kind, cp_replay;
        cross cp_op_kind, cp_flow_budget, cp_align_class;
    endgroup

    covergroup cg_us_top_issue_1 @(posedge clock);
        option.per_instance = 1;

        cp_op_kind : coverpoint (io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType == VLE_OP)
            iff (issue_vldu_1_sample) {
            bins store = {1'b0};
            bins load  = {1'b1};
        }
        cp_fu_op_type : coverpoint io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType
            iff (issue_vldu_1_sample) {
            bins vle = {VLE_OP};
            bins vse = {VSE_OP};
        }
        cp_eew : coverpoint io_ooo_to_mem_issueVldu_1_bits_uop_vpu_veew
            iff (issue_vldu_1_sample) { bins eew[] = {[0:3]}; }
        cp_vsew : coverpoint io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vsew
            iff (issue_vldu_1_sample) { bins vsew[] = {[0:3]}; }
        cp_vlmul : coverpoint io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vlmul
            iff (issue_vldu_1_sample) { bins lmul[] = {[0:7]}; }
        cp_base_offset : coverpoint io_ooo_to_mem_issueVldu_1_bits_src_0[3:0]
            iff (issue_vldu_1_sample) { bins offsets[] = {[0:15]}; }
        cp_align_class : coverpoint io_ooo_to_mem_issueVldu_1_bits_src_0[3:0]
            iff (issue_vldu_1_sample) {
            bins aligned_16B = {0};
            bins unaligned_16B = {[1:15]};
        }
        // Static coverage records raw VL/vstart ranges.  Exact semantic
        // partial/full/cross-uop classification requires temporal state.
        cp_vl_class : coverpoint io_ooo_to_mem_issueVldu_1_bits_src_4[7:0]
            iff (issue_vldu_1_sample) {
            bins zero = {0};
            bins one = {1};
            bins short = {[2:15]};
            bins vl_16_31 = {[16:31]};
            bins long = {[32:255]};
        }
        cp_vstart_class : coverpoint io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vstart
            iff (issue_vldu_1_sample) {
            bins zero = {0};
            bins one = {1};
            bins middle = {[2:254]};
            bins max = {255};
        }
        cp_vm : coverpoint io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vm
            iff (issue_vldu_1_sample) {
            bins masked = {1'b0};
            bins unmasked = {1'b1};
        }
        cp_mask_zero : coverpoint
            (io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vmask == 128'b0)
            iff (issue_vldu_1_sample) {
            bins nonzero = {1'b0};
            bins all_off = {1'b1};
        }
        cp_mask_all_one : coverpoint
            (io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vmask == {128{1'b1}})
            iff (issue_vldu_1_sample) {
            bins other = {1'b0};
            bins all_on = {1'b1};
        }
        cp_vma : coverpoint io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vma
            iff (issue_vldu_1_sample && (io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType == VLE_OP)) {
            bins undisturbed = {1'b0};
            bins agnostic = {1'b1};
        }
        cp_vta : coverpoint io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vta
            iff (issue_vldu_1_sample && (io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType == VLE_OP)) {
            bins undisturbed = {1'b0};
            bins agnostic = {1'b1};
        }
        cp_vuop_idx : coverpoint io_ooo_to_mem_issueVldu_1_bits_uop_vpu_vuopIdx
            iff (issue_vldu_1_sample) { bins field[] = {[0:127]}; }
        cp_last_uop : coverpoint io_ooo_to_mem_issueVldu_1_bits_uop_vpu_lastUop
            iff (issue_vldu_1_sample) {
            bins non_last = {1'b0};
            bins last = {1'b1};
        }
        cp_flow_budget : coverpoint io_ooo_to_mem_issueVldu_1_bits_flowNum
            iff (issue_vldu_1_sample) {
            bins zero = {0};
            bins one = {1};
            bins legal_two = {2};
            bins other = {[3:31]};
        }
        cp_store_data_zero : coverpoint
            (io_ooo_to_mem_issueVldu_1_bits_src_2 == 128'b0)
            iff (issue_vldu_1_sample && (io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType == VSE_OP)) {
            bins nonzero = {1'b0};
            bins all_zero = {1'b1};
        }
        cp_store_data_one : coverpoint
            (io_ooo_to_mem_issueVldu_1_bits_src_2 == {128{1'b1}})
            iff (issue_vldu_1_sample && (io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType == VSE_OP)) {
            bins other = {1'b0};
            bins all_one = {1'b1};
        }
        cp_replay : coverpoint io_ooo_to_mem_issueVldu_1_bits_isVecPartReplay
            iff (issue_vldu_1_sample) {
            bins no = {1'b0};
            bins yes = {1'b1};
        }

        cross cp_op_kind, cp_eew, cp_align_class;
        cross cp_vsew, cp_eew, cp_align_class;
        cross cp_op_kind, cp_vl_class, cp_vstart_class;
        cross cp_op_kind, cp_vm, cp_align_class;
        cross cp_vma, cp_vta, cp_vm;
        cross cp_last_uop, cp_op_kind, cp_replay;
        cross cp_op_kind, cp_flow_budget, cp_align_class;
    endgroup

    covergroup cg_us_top_complete_0 @(posedge clock);
        option.per_instance = 1;

        cp_op_kind : coverpoint (io_mem_to_ooo_writebackVldu_0_bits_uop_fuOpType == VLE_OP)
            iff (writeback_vldu_0_sample) {
            bins store = {1'b0};
            bins load  = {1'b1};
        }
        cp_fu_op_type : coverpoint io_mem_to_ooo_writebackVldu_0_bits_uop_fuOpType
            iff (writeback_vldu_0_sample) {
            bins vle = {VLE_OP};
            bins vse = {VSE_OP};
        }
        cp_terminal_outcome : coverpoint (io_mem_to_ooo_writebackVldu_0_exceptionVec == 0)
            iff (writeback_vldu_0_sample) {
            bins exception = {1'b0};
            bins normal = {1'b1};
        }
        cp_exception_nonzero : coverpoint (io_mem_to_ooo_writebackVldu_0_exceptionVec != 0)
            iff (writeback_vldu_0_sample) {
            bins none = {1'b0};
            bins nonzero = {1'b1};
        }
        cp_replay : coverpoint io_mem_to_ooo_writebackVldu_0_bits_uop_replayInst
            iff (writeback_vldu_0_sample) {
            bins no = {1'b0};
            bins yes = {1'b1};
        }
        cp_eew : coverpoint io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_veew
            iff (writeback_vldu_0_sample) { bins eew[] = {[0:3]}; }
        cp_vsew : coverpoint io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vsew
            iff (writeback_vldu_0_sample) { bins vsew[] = {[0:3]}; }
        cp_vlmul : coverpoint io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vlmul
            iff (writeback_vldu_0_sample) { bins lmul[] = {[0:7]}; }
        cp_vm : coverpoint io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vm
            iff (writeback_vldu_0_sample) {
            bins masked = {1'b0};
            bins unmasked = {1'b1};
        }
        cp_vl : coverpoint io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vl
            iff (writeback_vldu_0_sample) {
            bins zero = {0};
            bins one = {1};
            bins short = {[2:15]};
            bins vl_16_31 = {[16:31]};
            bins vl_32_255 = {[32:255]};
        }
        cp_out_vmask_zero : coverpoint
            (io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vmask == 128'b0)
            iff (writeback_vldu_0_sample && (io_mem_to_ooo_writebackVldu_0_bits_uop_fuOpType == VLE_OP)) {
            bins nonzero = {1'b0};
            bins all_off = {1'b1};
        }
        cp_out_vmask_one : coverpoint
            (io_mem_to_ooo_writebackVldu_0_bits_uop_vpu_vmask == {128{1'b1}})
            iff (writeback_vldu_0_sample && (io_mem_to_ooo_writebackVldu_0_bits_uop_fuOpType == VLE_OP)) {
            bins other = {1'b0};
            bins all_on = {1'b1};
        }
        cp_vd_idx : coverpoint io_mem_to_ooo_writebackVldu_0_bits_vdIdxInField
            iff (writeback_vldu_0_sample && (io_mem_to_ooo_writebackVldu_0_bits_uop_fuOpType == VLE_OP)) {
            bins field[] = {[0:7]};
        }

        cross cp_op_kind, cp_eew, cp_terminal_outcome;
        cross cp_op_kind, cp_vm, cp_vl;
        cross cp_op_kind, cp_replay, cp_exception_nonzero;
    endgroup

    covergroup cg_us_top_complete_1 @(posedge clock);
        option.per_instance = 1;

        cp_op_kind : coverpoint (io_mem_to_ooo_writebackVldu_1_bits_uop_fuOpType == VLE_OP)
            iff (writeback_vldu_1_sample) {
            bins store = {1'b0};
            bins load  = {1'b1};
        }
        cp_fu_op_type : coverpoint io_mem_to_ooo_writebackVldu_1_bits_uop_fuOpType
            iff (writeback_vldu_1_sample) {
            bins vle = {VLE_OP};
            bins vse = {VSE_OP};
        }
        cp_terminal_outcome : coverpoint (io_mem_to_ooo_writebackVldu_1_exceptionVec == 0)
            iff (writeback_vldu_1_sample) {
            bins exception = {1'b0};
            bins normal = {1'b1};
        }
        cp_exception_nonzero : coverpoint (io_mem_to_ooo_writebackVldu_1_exceptionVec != 0)
            iff (writeback_vldu_1_sample) {
            bins none = {1'b0};
            bins nonzero = {1'b1};
        }
        cp_replay : coverpoint io_mem_to_ooo_writebackVldu_1_bits_uop_replayInst
            iff (writeback_vldu_1_sample) {
            bins no = {1'b0};
            bins yes = {1'b1};
        }
        cp_eew : coverpoint io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_veew
            iff (writeback_vldu_1_sample) { bins eew[] = {[0:3]}; }
        cp_vsew : coverpoint io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vsew
            iff (writeback_vldu_1_sample) { bins vsew[] = {[0:3]}; }
        cp_vlmul : coverpoint io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vlmul
            iff (writeback_vldu_1_sample) { bins lmul[] = {[0:7]}; }
        cp_vm : coverpoint io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vm
            iff (writeback_vldu_1_sample) {
            bins masked = {1'b0};
            bins unmasked = {1'b1};
        }
        cp_vl : coverpoint io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vl
            iff (writeback_vldu_1_sample) {
            bins zero = {0};
            bins one = {1};
            bins short = {[2:15]};
            bins vl_16_31 = {[16:31]};
            bins vl_32_255 = {[32:255]};
        }
        cp_out_vmask_zero : coverpoint
            (io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vmask == 128'b0)
            iff (writeback_vldu_1_sample && (io_mem_to_ooo_writebackVldu_1_bits_uop_fuOpType == VLE_OP)) {
            bins nonzero = {1'b0};
            bins all_off = {1'b1};
        }
        cp_out_vmask_one : coverpoint
            (io_mem_to_ooo_writebackVldu_1_bits_uop_vpu_vmask == {128{1'b1}})
            iff (writeback_vldu_1_sample && (io_mem_to_ooo_writebackVldu_1_bits_uop_fuOpType == VLE_OP)) {
            bins other = {1'b0};
            bins all_on = {1'b1};
        }
        cp_vd_idx : coverpoint io_mem_to_ooo_writebackVldu_1_bits_vdIdxInField
            iff (writeback_vldu_1_sample && (io_mem_to_ooo_writebackVldu_1_bits_uop_fuOpType == VLE_OP)) {
            bins field[] = {[0:7]};
        }

        cross cp_op_kind, cp_eew, cp_terminal_outcome;
        cross cp_op_kind, cp_vm, cp_vl;
        cross cp_op_kind, cp_replay, cp_exception_nonzero;
    endgroup

    covergroup cg_us_top_feedback_0 @(posedge clock);
        option.per_instance = 1;

        cp_result : coverpoint {
            io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_isVecPartReplay,
            io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_hit
        } iff (feedback_0_sample) {
            bins no_feedback = {2'b00};
            bins success = {2'b01};
            bins block_replay = {2'b10, 2'b11};
        }
        cp_hit : coverpoint io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_hit
            iff (feedback_0_sample) {
            bins miss = {1'b0};
            bins hit = {1'b1};
        }
        cp_replay : coverpoint io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_isVecPartReplay
            iff (feedback_0_sample) {
            bins no = {1'b0};
            bins yes = {1'b1};
        }
        cp_mb_idx : coverpoint io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_vecReplayMbIdx
            iff (feedback_0_sample) { bins all[] = {[0:15]}; }
        cp_mask_zero : coverpoint
            (io_mem_to_ooo_vstuIqFeedback_0_feedbackSlow_bits_vecReplayMask == 16'b0)
            iff (feedback_0_sample) {
            bins nonzero = {1'b0};
            bins zero = {1'b1};
        }

        cross cp_result, cp_mb_idx;
        cross cp_hit, cp_replay, cp_mask_zero;
    endgroup

    covergroup cg_us_top_feedback_1 @(posedge clock);
        option.per_instance = 1;

        cp_result : coverpoint {
            io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_isVecPartReplay,
            io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_hit
        } iff (feedback_1_sample) {
            bins no_feedback = {2'b00};
            bins success = {2'b01};
            bins block_replay = {2'b10, 2'b11};
        }
        cp_hit : coverpoint io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_hit
            iff (feedback_1_sample) {
            bins miss = {1'b0};
            bins hit = {1'b1};
        }
        cp_replay : coverpoint io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_isVecPartReplay
            iff (feedback_1_sample) {
            bins no = {1'b0};
            bins yes = {1'b1};
        }
        cp_mb_idx : coverpoint io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_vecReplayMbIdx
            iff (feedback_1_sample) { bins all[] = {[0:15]}; }
        cp_mask_zero : coverpoint
            (io_mem_to_ooo_vstuIqFeedback_1_feedbackSlow_bits_vecReplayMask == 16'b0)
            iff (feedback_1_sample) {
            bins nonzero = {1'b0};
            bins zero = {1'b1};
        }

        cross cp_result, cp_mb_idx;
        cross cp_hit, cp_replay, cp_mask_zero;
    endgroup

    covergroup cg_us_redirect @(posedge clock);
        option.per_instance = 1;

        cp_level : coverpoint io_redirect_bits_level iff (redirect_sample) {
            bins low = {1'b0};
            bins high = {1'b1};
        }
        cp_vls_exception : coverpoint io_redirect_bits_isVlsException
            iff (redirect_sample) {
            bins no = {1'b0};
            bins yes = {1'b1};
        }

        cross cp_level, cp_vls_exception;
    endgroup

    covergroup cg_us_top_issue_ready_0 @(posedge clock);
        option.per_instance = 1;

        cp_op_kind : coverpoint (io_ooo_to_mem_issueVldu_0_bits_uop_fuOpType == VLE_OP)
            iff (issue_vldu_0_ready_sample) {
            bins store = {1'b0};
            bins load  = {1'b1};
        }
        cp_valid : coverpoint io_ooo_to_mem_issueVldu_0_valid
            iff (issue_vldu_0_ready_sample) {
            bins no = {1'b0};
            bins yes = {1'b1};
        }
        cp_ready : coverpoint io_ooo_to_mem_issueVldu_0_ready
            iff (issue_vldu_0_ready_sample) {
            bins blocked = {1'b0};
            bins accepted = {1'b1};
        }

        cross cp_op_kind, cp_valid, cp_ready;
    endgroup

    covergroup cg_us_top_issue_ready_1 @(posedge clock);
        option.per_instance = 1;

        cp_op_kind : coverpoint (io_ooo_to_mem_issueVldu_1_bits_uop_fuOpType == VLE_OP)
            iff (issue_vldu_1_ready_sample) {
            bins store = {1'b0};
            bins load  = {1'b1};
        }
        cp_valid : coverpoint io_ooo_to_mem_issueVldu_1_valid
            iff (issue_vldu_1_ready_sample) {
            bins no = {1'b0};
            bins yes = {1'b1};
        }
        cp_ready : coverpoint io_ooo_to_mem_issueVldu_1_ready
            iff (issue_vldu_1_ready_sample) {
            bins blocked = {1'b0};
            bins accepted = {1'b1};
        }

        cross cp_op_kind, cp_valid, cp_ready;
    endgroup

    cg_us_top_issue_0 issue_cg_0;
    cg_us_top_issue_1 issue_cg_1;
    cg_us_top_complete_0 complete_cg_0;
    cg_us_top_complete_1 complete_cg_1;
    cg_us_top_feedback_0 feedback_cg_0;
    cg_us_top_feedback_1 feedback_cg_1;
    cg_us_redirect redirect_cg;
    cg_us_top_issue_ready_0 issue_ready_cg_0;
    cg_us_top_issue_ready_1 issue_ready_cg_1;

    initial begin
        issue_cg_0 = new();
        issue_cg_1 = new();
        complete_cg_0 = new();
        complete_cg_1 = new();
        feedback_cg_0 = new();
        feedback_cg_1 = new();
        redirect_cg = new();
        issue_ready_cg_0 = new();
        issue_ready_cg_1 = new();
    end
endmodule: memblock_rtl_fcov
`endif
`endif
