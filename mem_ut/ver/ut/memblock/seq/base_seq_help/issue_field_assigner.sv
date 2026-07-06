//=========================================================
//File name    : issue_field_assigner.sv
//Author       : OpenAI_Codex
//Module name  : issue_field_assigner
//Discribution : pre-issue field assignment helper for lintsissue xaction
//Date         : 2026-05-18
//=========================================================
`ifndef ISSUE_FIELD_ASSIGNER__SV
`define ISSUE_FIELD_ASSIGNER__SV

class issue_field_assigner extends uvm_object;

    common_data_transaction data;

    `uvm_object_utils(issue_field_assigner)

    function new(string name = "issue_field_assigner");
        super.new(name);
        data = common_data_transaction::get();
    endfunction:new

    function void ensure_data();
        if (data == null) begin
            data = common_data_transaction::get();
        end
    endfunction:ensure_data

    function bit deterministic_percent_hit(input memblock_uid_t uid,
                                           input int unsigned salt,
                                           input int unsigned percent);
        int unsigned score;

        if (percent == 0) begin
            return 1'b0;
        end
        if (percent >= 100) begin
            return 1'b1;
        end
        score = ((uid * 37) + (salt * 17) + 11) % 100;
        return score < percent;
    endfunction:deterministic_percent_hit

    function bit is_valid_pipe_idx(input memblock_issue_target_e target,
                                   input int unsigned pipe_idx);
        case (target)
            MEMBLOCK_ISSUE_TARGET_LOAD: return pipe_idx < 3;
            MEMBLOCK_ISSUE_TARGET_STA:  return pipe_idx < 2;
            MEMBLOCK_ISSUE_TARGET_STD:  return pipe_idx < 2;
            default: return 1'b0;
        endcase
    endfunction:is_valid_pipe_idx

    function void check_pipe_idx(input memblock_issue_target_e target,
                                 input int unsigned pipe_idx,
                                 input string caller);
        if (!is_valid_pipe_idx(target, pipe_idx)) begin
            `uvm_fatal("ISSUE_FIELD", $sformatf("%s got target=%0d pipe_idx=%0d", caller, target, pipe_idx))
        end
    endfunction:check_pipe_idx

    function bit select_prior_store_for_load(input memblock_uid_t uid,
                                             output memblock_rob_key_t wait_rob_key);
        wait_rob_key = '{default:'0};
        ensure_data();
        if (!data.is_valid_uid(uid)) begin
            return 1'b0;
        end

        for (int idx = uid; idx > 0; idx--) begin
            memblock_uid_t cand_uid;
            main_control_transaction cand_tr;
            status_transaction cand_status;
            memblock_op_behavior_t behavior;

            cand_uid = idx - 1;
            cand_tr = data.get_main_transaction(cand_uid);
            cand_status = data.get_status(cand_uid);
            if (!cand_status.active || !cand_status.enq) begin
                continue;
            end
            behavior = lsq_ctrl_model::derive_op_behavior(cand_tr);
            if (behavior.uses_sq && behavior.route_std) begin
                wait_rob_key = cand_tr.get_rob_key();
                return 1'b1;
            end
        end
        return 1'b0;
    endfunction:select_prior_store_for_load

    function bit compute_is_rvc(input memblock_uid_t uid);
        return deterministic_percent_hit(uid, 5, seq_csr_common::get_rvc_wt());
    endfunction:compute_is_rvc

    function bit [49:0] compute_pc(input memblock_uid_t uid);
        bit [63:0] pc64;
        bit [63:0] uid64;

        uid64 = uid;
        pc64 = seq_csr_common::get_pc_base() + (uid64 * seq_csr_common::get_pc_stride());
        return pc64[49:0];
    endfunction:compute_pc

    function bit compute_ftq_flag(input memblock_uid_t uid);
        return 1'b1;
    endfunction:compute_ftq_flag

    function bit [5:0] compute_ftq_value(input memblock_uid_t uid);
        int unsigned ftq_value;

        ftq_value = seq_csr_common::get_ftq_idx_base() + uid;
        return ftq_value[5:0];
    endfunction:compute_ftq_value

    function bit [4:0] compute_ftq_offset(input memblock_uid_t uid);
        return uid[4:0];
    endfunction:compute_ftq_offset

    function bit [7:0] compute_pdest(input memblock_uid_t uid,
                                      input bit has_wb);
        int unsigned range;
        int unsigned pdest;

        if (!has_wb) begin
            return '0;
        end
        range = seq_csr_common::get_pdest_range();
        pdest = seq_csr_common::get_pdest_base() + (uid % range);
        return pdest[7:0];
    endfunction:compute_pdest

    function void derive_wen(input main_control_transaction main_tr,
                             output bit rfWen,
                             output bit fpWen);
        rfWen = 1'b0;
        fpWen = 1'b0;
        if (main_tr == null) begin
            `uvm_fatal("ISSUE_FIELD", "derive_wen got null main_tr")
        end

        case (main_tr.op_class)
            MEMBLOCK_OP_CLASS_INT_LOAD: begin
                rfWen = 1'b1;
            end
            MEMBLOCK_OP_CLASS_FP_LOAD: begin
                fpWen = 1'b1;
            end
            MEMBLOCK_OP_CLASS_AMO: begin
                rfWen = 1'b1;
            end
            default: begin
                rfWen = 1'b0;
                fpWen = 1'b0;
            end
        endcase
        if (rfWen && fpWen) begin
            `uvm_fatal("ISSUE_FIELD", $sformatf("uid=%0d rfWen/fpWen conflict", main_tr.uid))
        end
    endfunction:derive_wen

    function void clear_lintsissue_xaction(input lintsissue_agent_agent_xaction tr);
        if (tr == null) begin
            `uvm_fatal("ISSUE_FIELD", "clear_lintsissue_xaction got null xaction")
        end
        tr.io_ooo_to_mem_intIssue_6_0_ready = '0;
        tr.io_ooo_to_mem_intIssue_0_0_valid = 1'b0;
        tr.io_ooo_to_mem_intIssue_1_0_valid = 1'b0;
        tr.io_ooo_to_mem_intIssue_2_0_valid = 1'b0;
        tr.io_ooo_to_mem_intIssue_3_0_valid = 1'b0;
        tr.io_ooo_to_mem_intIssue_4_0_valid = 1'b0;
        tr.io_ooo_to_mem_intIssue_5_0_valid = 1'b0;
        tr.io_ooo_to_mem_intIssue_6_0_valid = 1'b0;
        tr.io_ooo_to_mem_intIssue_6_0_bits_fuType = '0;
        tr.io_ooo_to_mem_intIssue_6_0_bits_fuOpType = '0;
        tr.io_ooo_to_mem_intIssue_6_0_bits_src_0 = '0;
        tr.io_ooo_to_mem_intIssue_6_0_bits_robIdx_flag = '0;
        tr.io_ooo_to_mem_intIssue_6_0_bits_robIdx_value = '0;
        tr.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_flag = '0;
        tr.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_value = '0;

        tr.io_ooo_to_mem_intIssue_5_0_ready = '0;
        tr.io_ooo_to_mem_intIssue_5_0_bits_fuType = '0;
        tr.io_ooo_to_mem_intIssue_5_0_bits_fuOpType = '0;
        tr.io_ooo_to_mem_intIssue_5_0_bits_src_0 = '0;
        tr.io_ooo_to_mem_intIssue_5_0_bits_robIdx_flag = '0;
        tr.io_ooo_to_mem_intIssue_5_0_bits_robIdx_value = '0;
        tr.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_flag = '0;
        tr.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_value = '0;

        tr.io_ooo_to_mem_intIssue_4_0_ready = '0;
        tr.io_ooo_to_mem_intIssue_4_0_bits_fuType = '0;
        tr.io_ooo_to_mem_intIssue_4_0_bits_fuOpType = '0;
        tr.io_ooo_to_mem_intIssue_4_0_bits_src_0 = '0;
        tr.io_ooo_to_mem_intIssue_4_0_bits_imm = '0;
        tr.io_ooo_to_mem_intIssue_4_0_bits_robIdx_flag = '0;
        tr.io_ooo_to_mem_intIssue_4_0_bits_robIdx_value = '0;
        tr.io_ooo_to_mem_intIssue_4_0_bits_isFirstIssue = '0;
        tr.io_ooo_to_mem_intIssue_4_0_bits_pdest = '0;
        tr.io_ooo_to_mem_intIssue_4_0_bits_isRVC = '0;
        tr.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_flag = '0;
        tr.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_value = '0;
        tr.io_ooo_to_mem_intIssue_4_0_bits_ftqOffset = '0;
        tr.io_ooo_to_mem_intIssue_4_0_bits_storeSetHit = '0;
        tr.io_ooo_to_mem_intIssue_4_0_bits_ssid = '0;
        tr.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_flag = '0;
        tr.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_value = '0;

        tr.io_ooo_to_mem_intIssue_3_0_ready = '0;
        tr.io_ooo_to_mem_intIssue_3_0_bits_fuType = '0;
        tr.io_ooo_to_mem_intIssue_3_0_bits_fuOpType = '0;
        tr.io_ooo_to_mem_intIssue_3_0_bits_src_0 = '0;
        tr.io_ooo_to_mem_intIssue_3_0_bits_imm = '0;
        tr.io_ooo_to_mem_intIssue_3_0_bits_robIdx_flag = '0;
        tr.io_ooo_to_mem_intIssue_3_0_bits_robIdx_value = '0;
        tr.io_ooo_to_mem_intIssue_3_0_bits_isFirstIssue = '0;
        tr.io_ooo_to_mem_intIssue_3_0_bits_pdest = '0;
        tr.io_ooo_to_mem_intIssue_3_0_bits_isRVC = '0;
        tr.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_flag = '0;
        tr.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_value = '0;
        tr.io_ooo_to_mem_intIssue_3_0_bits_ftqOffset = '0;
        tr.io_ooo_to_mem_intIssue_3_0_bits_storeSetHit = '0;
        tr.io_ooo_to_mem_intIssue_3_0_bits_ssid = '0;
        tr.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_flag = '0;
        tr.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_value = '0;

        tr.io_ooo_to_mem_intIssue_2_0_ready = '0;
        tr.io_ooo_to_mem_intIssue_2_0_bits_fuOpType = '0;
        tr.io_ooo_to_mem_intIssue_2_0_bits_src_0 = '0;
        tr.io_ooo_to_mem_intIssue_2_0_bits_imm = '0;
        tr.io_ooo_to_mem_intIssue_2_0_bits_robIdx_flag = '0;
        tr.io_ooo_to_mem_intIssue_2_0_bits_robIdx_value = '0;
        tr.io_ooo_to_mem_intIssue_2_0_bits_pdest = '0;
        tr.io_ooo_to_mem_intIssue_2_0_bits_rfWen = '0;
        tr.io_ooo_to_mem_intIssue_2_0_bits_fpWen = '0;
        tr.io_ooo_to_mem_intIssue_2_0_bits_pc = '0;
        tr.io_ooo_to_mem_intIssue_2_0_bits_isRVC = '0;
        tr.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_flag = '0;
        tr.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_value = '0;
        tr.io_ooo_to_mem_intIssue_2_0_bits_ftqOffset = '0;
        tr.io_ooo_to_mem_intIssue_2_0_bits_loadWaitBit = '0;
        tr.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_flag = '0;
        tr.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_value = '0;
        tr.io_ooo_to_mem_intIssue_2_0_bits_storeSetHit = '0;
        tr.io_ooo_to_mem_intIssue_2_0_bits_loadWaitStrict = '0;
        tr.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_flag = '0;
        tr.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_value = '0;
        tr.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_flag = '0;
        tr.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_value = '0;

        tr.io_ooo_to_mem_intIssue_1_0_ready = '0;
        tr.io_ooo_to_mem_intIssue_1_0_bits_fuOpType = '0;
        tr.io_ooo_to_mem_intIssue_1_0_bits_src_0 = '0;
        tr.io_ooo_to_mem_intIssue_1_0_bits_imm = '0;
        tr.io_ooo_to_mem_intIssue_1_0_bits_robIdx_flag = '0;
        tr.io_ooo_to_mem_intIssue_1_0_bits_robIdx_value = '0;
        tr.io_ooo_to_mem_intIssue_1_0_bits_pdest = '0;
        tr.io_ooo_to_mem_intIssue_1_0_bits_rfWen = '0;
        tr.io_ooo_to_mem_intIssue_1_0_bits_fpWen = '0;
        tr.io_ooo_to_mem_intIssue_1_0_bits_pc = '0;
        tr.io_ooo_to_mem_intIssue_1_0_bits_isRVC = '0;
        tr.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_flag = '0;
        tr.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_value = '0;
        tr.io_ooo_to_mem_intIssue_1_0_bits_ftqOffset = '0;
        tr.io_ooo_to_mem_intIssue_1_0_bits_loadWaitBit = '0;
        tr.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_flag = '0;
        tr.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_value = '0;
        tr.io_ooo_to_mem_intIssue_1_0_bits_storeSetHit = '0;
        tr.io_ooo_to_mem_intIssue_1_0_bits_loadWaitStrict = '0;
        tr.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_flag = '0;
        tr.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_value = '0;
        tr.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_flag = '0;
        tr.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_value = '0;

        tr.io_ooo_to_mem_intIssue_0_0_ready = '0;
        tr.io_ooo_to_mem_intIssue_0_0_bits_fuOpType = '0;
        tr.io_ooo_to_mem_intIssue_0_0_bits_src_0 = '0;
        tr.io_ooo_to_mem_intIssue_0_0_bits_imm = '0;
        tr.io_ooo_to_mem_intIssue_0_0_bits_robIdx_flag = '0;
        tr.io_ooo_to_mem_intIssue_0_0_bits_robIdx_value = '0;
        tr.io_ooo_to_mem_intIssue_0_0_bits_pdest = '0;
        tr.io_ooo_to_mem_intIssue_0_0_bits_rfWen = '0;
        tr.io_ooo_to_mem_intIssue_0_0_bits_fpWen = '0;
        tr.io_ooo_to_mem_intIssue_0_0_bits_pc = '0;
        tr.io_ooo_to_mem_intIssue_0_0_bits_isRVC = '0;
        tr.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_flag = '0;
        tr.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_value = '0;
        tr.io_ooo_to_mem_intIssue_0_0_bits_ftqOffset = '0;
        tr.io_ooo_to_mem_intIssue_0_0_bits_loadWaitBit = '0;
        tr.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_flag = '0;
        tr.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_value = '0;
        tr.io_ooo_to_mem_intIssue_0_0_bits_storeSetHit = '0;
        tr.io_ooo_to_mem_intIssue_0_0_bits_loadWaitStrict = '0;
        tr.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_flag = '0;
        tr.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_value = '0;
        tr.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_flag = '0;
        tr.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_value = '0;
    endfunction:clear_lintsissue_xaction

    function void assign_load_main_fields(input lintsissue_agent_agent_xaction tr,
                                          input main_control_transaction main_tr,
                                          input memblock_issue_q_item_t item,
                                          input int unsigned pipe_idx);
        case (pipe_idx)
            0: begin
                tr.io_ooo_to_mem_intIssue_0_0_valid = 1'b1;
                tr.io_ooo_to_mem_intIssue_0_0_bits_fuOpType = main_tr.fuOpType;
                tr.io_ooo_to_mem_intIssue_0_0_bits_src_0 = main_tr.src_0;
                tr.io_ooo_to_mem_intIssue_0_0_bits_imm = main_tr.imm;
                tr.io_ooo_to_mem_intIssue_0_0_bits_robIdx_flag = main_tr.robIdx_flag;
                tr.io_ooo_to_mem_intIssue_0_0_bits_robIdx_value = main_tr.robIdx_value;
                tr.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_flag = item.lq_key.flag;
                tr.io_ooo_to_mem_intIssue_0_0_bits_lqIdx_value = item.lq_key.value;
                tr.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_flag = item.sq_key.flag;
                tr.io_ooo_to_mem_intIssue_0_0_bits_sqIdx_value = item.sq_key.value;
            end
            1: begin
                tr.io_ooo_to_mem_intIssue_1_0_valid = 1'b1;
                tr.io_ooo_to_mem_intIssue_1_0_bits_fuOpType = main_tr.fuOpType;
                tr.io_ooo_to_mem_intIssue_1_0_bits_src_0 = main_tr.src_0;
                tr.io_ooo_to_mem_intIssue_1_0_bits_imm = main_tr.imm;
                tr.io_ooo_to_mem_intIssue_1_0_bits_robIdx_flag = main_tr.robIdx_flag;
                tr.io_ooo_to_mem_intIssue_1_0_bits_robIdx_value = main_tr.robIdx_value;
                tr.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_flag = item.lq_key.flag;
                tr.io_ooo_to_mem_intIssue_1_0_bits_lqIdx_value = item.lq_key.value;
                tr.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_flag = item.sq_key.flag;
                tr.io_ooo_to_mem_intIssue_1_0_bits_sqIdx_value = item.sq_key.value;
            end
            2: begin
                tr.io_ooo_to_mem_intIssue_2_0_valid = 1'b1;
                tr.io_ooo_to_mem_intIssue_2_0_bits_fuOpType = main_tr.fuOpType;
                tr.io_ooo_to_mem_intIssue_2_0_bits_src_0 = main_tr.src_0;
                tr.io_ooo_to_mem_intIssue_2_0_bits_imm = main_tr.imm;
                tr.io_ooo_to_mem_intIssue_2_0_bits_robIdx_flag = main_tr.robIdx_flag;
                tr.io_ooo_to_mem_intIssue_2_0_bits_robIdx_value = main_tr.robIdx_value;
                tr.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_flag = item.lq_key.flag;
                tr.io_ooo_to_mem_intIssue_2_0_bits_lqIdx_value = item.lq_key.value;
                tr.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_flag = item.sq_key.flag;
                tr.io_ooo_to_mem_intIssue_2_0_bits_sqIdx_value = item.sq_key.value;
            end
            default: `uvm_fatal("ISSUE_FIELD", $sformatf("load pipe_idx=%0d out of range", pipe_idx))
        endcase
    endfunction:assign_load_main_fields

    function void assign_sta_main_fields(input lintsissue_agent_agent_xaction tr,
                                         input main_control_transaction main_tr,
                                         input memblock_issue_q_item_t item,
                                         input int unsigned pipe_idx);
        case (pipe_idx)
            0: begin
                tr.io_ooo_to_mem_intIssue_3_0_valid = 1'b1;
                tr.io_ooo_to_mem_intIssue_3_0_bits_fuType = main_tr.fuType;
                tr.io_ooo_to_mem_intIssue_3_0_bits_fuOpType = main_tr.fuOpType;
                tr.io_ooo_to_mem_intIssue_3_0_bits_src_0 = main_tr.src_0;
                tr.io_ooo_to_mem_intIssue_3_0_bits_imm = main_tr.imm;
                tr.io_ooo_to_mem_intIssue_3_0_bits_robIdx_flag = main_tr.robIdx_flag;
                tr.io_ooo_to_mem_intIssue_3_0_bits_robIdx_value = main_tr.robIdx_value;
                tr.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_flag = item.sq_key.flag;
                tr.io_ooo_to_mem_intIssue_3_0_bits_sqIdx_value = item.sq_key.value;
            end
            1: begin
                tr.io_ooo_to_mem_intIssue_4_0_valid = 1'b1;
                tr.io_ooo_to_mem_intIssue_4_0_bits_fuType = main_tr.fuType;
                tr.io_ooo_to_mem_intIssue_4_0_bits_fuOpType = main_tr.fuOpType;
                tr.io_ooo_to_mem_intIssue_4_0_bits_src_0 = main_tr.src_0;
                tr.io_ooo_to_mem_intIssue_4_0_bits_imm = main_tr.imm;
                tr.io_ooo_to_mem_intIssue_4_0_bits_robIdx_flag = main_tr.robIdx_flag;
                tr.io_ooo_to_mem_intIssue_4_0_bits_robIdx_value = main_tr.robIdx_value;
                tr.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_flag = item.sq_key.flag;
                tr.io_ooo_to_mem_intIssue_4_0_bits_sqIdx_value = item.sq_key.value;
            end
            default: `uvm_fatal("ISSUE_FIELD", $sformatf("STA pipe_idx=%0d out of range", pipe_idx))
        endcase
    endfunction:assign_sta_main_fields

    function void assign_std_main_fields(input lintsissue_agent_agent_xaction tr,
                                         input main_control_transaction main_tr,
                                         input memblock_issue_q_item_t item,
                                         input int unsigned pipe_idx);
        case (pipe_idx)
            0: begin
                tr.io_ooo_to_mem_intIssue_5_0_valid = 1'b1;
                tr.io_ooo_to_mem_intIssue_5_0_bits_fuType = main_tr.fuType;
                tr.io_ooo_to_mem_intIssue_5_0_bits_fuOpType = main_tr.fuOpType;
                tr.io_ooo_to_mem_intIssue_5_0_bits_src_0 = main_tr.src_0;
                tr.io_ooo_to_mem_intIssue_5_0_bits_robIdx_flag = main_tr.robIdx_flag;
                tr.io_ooo_to_mem_intIssue_5_0_bits_robIdx_value = main_tr.robIdx_value;
                tr.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_flag = item.sq_key.flag;
                tr.io_ooo_to_mem_intIssue_5_0_bits_sqIdx_value = item.sq_key.value;
            end
            1: begin
                tr.io_ooo_to_mem_intIssue_6_0_valid = 1'b1;
                tr.io_ooo_to_mem_intIssue_6_0_bits_fuType = main_tr.fuType;
                tr.io_ooo_to_mem_intIssue_6_0_bits_fuOpType = main_tr.fuOpType;
                tr.io_ooo_to_mem_intIssue_6_0_bits_src_0 = main_tr.src_0;
                tr.io_ooo_to_mem_intIssue_6_0_bits_robIdx_flag = main_tr.robIdx_flag;
                tr.io_ooo_to_mem_intIssue_6_0_bits_robIdx_value = main_tr.robIdx_value;
                tr.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_flag = item.sq_key.flag;
                tr.io_ooo_to_mem_intIssue_6_0_bits_sqIdx_value = item.sq_key.value;
            end
            default: `uvm_fatal("ISSUE_FIELD", $sformatf("STD pipe_idx=%0d out of range", pipe_idx))
        endcase
    endfunction:assign_std_main_fields

    function void assign_main_issue_fields(input lintsissue_agent_agent_xaction tr,
                                           input memblock_issue_q_item_t item,
                                           input int unsigned pipe_idx);
        main_control_transaction main_tr;

        ensure_data();
        if (tr == null) begin
            `uvm_fatal("ISSUE_FIELD", "assign_main_issue_fields got null xaction")
        end
        check_pipe_idx(item.target, pipe_idx, "assign_main_issue_fields");
        main_tr = data.get_main_transaction(item.uid);
        case (item.target)
            MEMBLOCK_ISSUE_TARGET_LOAD: assign_load_main_fields(tr, main_tr, item, pipe_idx);
            MEMBLOCK_ISSUE_TARGET_STA:  assign_sta_main_fields(tr, main_tr, item, pipe_idx);
            MEMBLOCK_ISSUE_TARGET_STD:  assign_std_main_fields(tr, main_tr, item, pipe_idx);
            default: `uvm_fatal("ISSUE_FIELD", $sformatf("unsupported issue target=%0d", item.target))
        endcase
    endfunction:assign_main_issue_fields

    function void assign_issue_dep_fields(input lintsissue_agent_agent_xaction tr,
                                          input memblock_issue_q_item_t item,
                                          input int unsigned pipe_idx);
        status_transaction status;
        memblock_rob_key_t wait_rob_key;
        bit wait_valid;
        bit store_set_hit;
        bit load_wait_strict;
        bit is_first_issue;
        bit [4:0] ssid;

        ensure_data();
        if (tr == null) begin
            `uvm_fatal("ISSUE_FIELD", "assign_issue_dep_fields got null xaction")
        end
        check_pipe_idx(item.target, pipe_idx, "assign_issue_dep_fields");
        status = data.get_status(item.uid);
        wait_valid = 1'b0;
        wait_rob_key = '{default:'0};
        if (item.target == MEMBLOCK_ISSUE_TARGET_LOAD &&
            deterministic_percent_hit(item.uid, 1, seq_csr_common::get_mdp_load_wait_wt())) begin
            wait_valid = select_prior_store_for_load(item.uid, wait_rob_key);
        end
        store_set_hit = wait_valid ||
                        deterministic_percent_hit(item.uid, 2, seq_csr_common::get_mdp_storeset_hit_wt());
        load_wait_strict = wait_valid &&
                           deterministic_percent_hit(item.uid, 3, seq_csr_common::get_load_wait_strict_wt());
        is_first_issue = (status.replay_seq == 0);
        ssid = item.uid[4:0];

        case (item.target)
            MEMBLOCK_ISSUE_TARGET_LOAD: begin
                case (pipe_idx)
                    0: begin
                        tr.io_ooo_to_mem_intIssue_0_0_bits_loadWaitBit = wait_valid;
                        tr.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_flag = wait_rob_key.flag;
                        tr.io_ooo_to_mem_intIssue_0_0_bits_waitForRobIdx_value = wait_rob_key.value;
                        tr.io_ooo_to_mem_intIssue_0_0_bits_storeSetHit = store_set_hit;
                        tr.io_ooo_to_mem_intIssue_0_0_bits_loadWaitStrict = load_wait_strict;
                    end
                    1: begin
                        tr.io_ooo_to_mem_intIssue_1_0_bits_loadWaitBit = wait_valid;
                        tr.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_flag = wait_rob_key.flag;
                        tr.io_ooo_to_mem_intIssue_1_0_bits_waitForRobIdx_value = wait_rob_key.value;
                        tr.io_ooo_to_mem_intIssue_1_0_bits_storeSetHit = store_set_hit;
                        tr.io_ooo_to_mem_intIssue_1_0_bits_loadWaitStrict = load_wait_strict;
                    end
                    2: begin
                        tr.io_ooo_to_mem_intIssue_2_0_bits_loadWaitBit = wait_valid;
                        tr.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_flag = wait_rob_key.flag;
                        tr.io_ooo_to_mem_intIssue_2_0_bits_waitForRobIdx_value = wait_rob_key.value;
                        tr.io_ooo_to_mem_intIssue_2_0_bits_storeSetHit = store_set_hit;
                        tr.io_ooo_to_mem_intIssue_2_0_bits_loadWaitStrict = load_wait_strict;
                    end
                endcase
            end
            MEMBLOCK_ISSUE_TARGET_STA: begin
                case (pipe_idx)
                    0: begin
                        tr.io_ooo_to_mem_intIssue_3_0_bits_isFirstIssue = is_first_issue;
                        tr.io_ooo_to_mem_intIssue_3_0_bits_storeSetHit = store_set_hit;
                        tr.io_ooo_to_mem_intIssue_3_0_bits_ssid = ssid;
                    end
                    1: begin
                        tr.io_ooo_to_mem_intIssue_4_0_bits_isFirstIssue = is_first_issue;
                        tr.io_ooo_to_mem_intIssue_4_0_bits_storeSetHit = store_set_hit;
                        tr.io_ooo_to_mem_intIssue_4_0_bits_ssid = ssid;
                    end
                endcase
            end
            MEMBLOCK_ISSUE_TARGET_STD: begin
                return;
            end
            default: `uvm_fatal("ISSUE_FIELD", $sformatf("unsupported issue target=%0d", item.target))
        endcase
    endfunction:assign_issue_dep_fields

    function void assign_backend_meta_fields(input lintsissue_agent_agent_xaction tr,
                                             input memblock_issue_q_item_t item,
                                             input int unsigned pipe_idx);
        main_control_transaction main_tr;
        bit rfWen;
        bit fpWen;
        bit has_wb;
        bit [7:0] pdest;
        bit [49:0] pc;
        bit is_rvc;
        bit ftq_flag;
        bit [5:0] ftq_value;
        bit [4:0] ftq_offset;

        ensure_data();
        if (tr == null) begin
            `uvm_fatal("ISSUE_FIELD", "assign_backend_meta_fields got null xaction")
        end
        check_pipe_idx(item.target, pipe_idx, "assign_backend_meta_fields");
        main_tr = data.get_main_transaction(item.uid);
        derive_wen(main_tr, rfWen, fpWen);
        has_wb = rfWen || fpWen;
        pdest = compute_pdest(item.uid, has_wb);
        pc = compute_pc(item.uid);
        is_rvc = compute_is_rvc(item.uid);
        ftq_flag = compute_ftq_flag(item.uid);
        ftq_value = compute_ftq_value(item.uid);
        ftq_offset = compute_ftq_offset(item.uid);

        case (item.target)
            MEMBLOCK_ISSUE_TARGET_LOAD: begin
                case (pipe_idx)
                    0: begin
                        tr.io_ooo_to_mem_intIssue_0_0_bits_pdest = pdest;
                        tr.io_ooo_to_mem_intIssue_0_0_bits_rfWen = rfWen;
                        tr.io_ooo_to_mem_intIssue_0_0_bits_fpWen = fpWen;
                        tr.io_ooo_to_mem_intIssue_0_0_bits_pc = pc;
                        tr.io_ooo_to_mem_intIssue_0_0_bits_isRVC = is_rvc;
                        tr.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_flag = ftq_flag;
                        tr.io_ooo_to_mem_intIssue_0_0_bits_ftqIdx_value = ftq_value;
                        tr.io_ooo_to_mem_intIssue_0_0_bits_ftqOffset = ftq_offset;
                    end
                    1: begin
                        tr.io_ooo_to_mem_intIssue_1_0_bits_pdest = pdest;
                        tr.io_ooo_to_mem_intIssue_1_0_bits_rfWen = rfWen;
                        tr.io_ooo_to_mem_intIssue_1_0_bits_fpWen = fpWen;
                        tr.io_ooo_to_mem_intIssue_1_0_bits_pc = pc;
                        tr.io_ooo_to_mem_intIssue_1_0_bits_isRVC = is_rvc;
                        tr.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_flag = ftq_flag;
                        tr.io_ooo_to_mem_intIssue_1_0_bits_ftqIdx_value = ftq_value;
                        tr.io_ooo_to_mem_intIssue_1_0_bits_ftqOffset = ftq_offset;
                    end
                    2: begin
                        tr.io_ooo_to_mem_intIssue_2_0_bits_pdest = pdest;
                        tr.io_ooo_to_mem_intIssue_2_0_bits_rfWen = rfWen;
                        tr.io_ooo_to_mem_intIssue_2_0_bits_fpWen = fpWen;
                        tr.io_ooo_to_mem_intIssue_2_0_bits_pc = pc;
                        tr.io_ooo_to_mem_intIssue_2_0_bits_isRVC = is_rvc;
                        tr.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_flag = ftq_flag;
                        tr.io_ooo_to_mem_intIssue_2_0_bits_ftqIdx_value = ftq_value;
                        tr.io_ooo_to_mem_intIssue_2_0_bits_ftqOffset = ftq_offset;
                    end
                endcase
            end
            MEMBLOCK_ISSUE_TARGET_STA: begin
                case (pipe_idx)
                    0: begin
                        tr.io_ooo_to_mem_intIssue_3_0_bits_pdest = pdest;
                        tr.io_ooo_to_mem_intIssue_3_0_bits_isRVC = is_rvc;
                        tr.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_flag = ftq_flag;
                        tr.io_ooo_to_mem_intIssue_3_0_bits_ftqIdx_value = ftq_value;
                        tr.io_ooo_to_mem_intIssue_3_0_bits_ftqOffset = ftq_offset;
                    end
                    1: begin
                        tr.io_ooo_to_mem_intIssue_4_0_bits_pdest = pdest;
                        tr.io_ooo_to_mem_intIssue_4_0_bits_isRVC = is_rvc;
                        tr.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_flag = ftq_flag;
                        tr.io_ooo_to_mem_intIssue_4_0_bits_ftqIdx_value = ftq_value;
                        tr.io_ooo_to_mem_intIssue_4_0_bits_ftqOffset = ftq_offset;
                    end
                endcase
            end
            MEMBLOCK_ISSUE_TARGET_STD: begin
                return;
            end
            default: `uvm_fatal("ISSUE_FIELD", $sformatf("unsupported issue target=%0d", item.target))
        endcase
    endfunction:assign_backend_meta_fields

    function void assign_issue_item_fields(input lintsissue_agent_agent_xaction tr,
                                           input memblock_issue_q_item_t item,
                                           input int unsigned pipe_idx);
        assign_main_issue_fields(tr, item, pipe_idx);
        assign_issue_dep_fields(tr, item, pipe_idx);
        assign_backend_meta_fields(tr, item, pipe_idx);
    endfunction:assign_issue_item_fields

endclass:issue_field_assigner

`endif
