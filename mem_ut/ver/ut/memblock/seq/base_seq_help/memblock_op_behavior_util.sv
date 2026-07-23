//=========================================================
//File name    : memblock_op_behavior_util.sv
//Author       : OpenAI_Codex
//Module name  : memblock_op_behavior_util
//Discribution : stateless scalar LS operation behavior matrix
//Date         : 2026-07-22
//=========================================================
`ifndef MEMBLOCK_OP_BEHAVIOR_UTIL__SV
`define MEMBLOCK_OP_BEHAVIOR_UTIL__SV

// 中文注释：该 class 是 LOAD/PREFETCH/STORE/CBO/AMO 分类和 behavior
// 派生的唯一真源。它不持有 common data、queue、map 或任何运行期状态。
class memblock_op_behavior_util;

    static function bit is_vector_ls_futype(
        input bit [MEMBLOCK_INTERNAL_FUTYPE_W-1:0] fuType
    );
        return fuType == MEMBLOCK_FUTYPE_VLDU    ||
               fuType == MEMBLOCK_FUTYPE_VSTU    ||
               fuType == MEMBLOCK_FUTYPE_VSEGLDU ||
               fuType == MEMBLOCK_FUTYPE_VSEGSTU;
    endfunction:is_vector_ls_futype

    static function bit is_load_fuoptype(input bit [8:0] fuOpType);
        return fuOpType == MEMBLOCK_LSUOP_LB  ||
               fuOpType == MEMBLOCK_LSUOP_LH  ||
               fuOpType == MEMBLOCK_LSUOP_LW  ||
               fuOpType == MEMBLOCK_LSUOP_LD  ||
               fuOpType == MEMBLOCK_LSUOP_LBU ||
               fuOpType == MEMBLOCK_LSUOP_LHU ||
               fuOpType == MEMBLOCK_LSUOP_LWU;
    endfunction:is_load_fuoptype

    static function bit is_prefetch_fuoptype(input bit [8:0] fuOpType);
        return fuOpType == MEMBLOCK_LSUOP_PREFETCH_I ||
               fuOpType == MEMBLOCK_LSUOP_PREFETCH_R ||
               fuOpType == MEMBLOCK_LSUOP_PREFETCH_W;
    endfunction:is_prefetch_fuoptype

    static function bit is_store_fuoptype(input bit [8:0] fuOpType);
        return fuOpType == MEMBLOCK_LSUOP_SB ||
               fuOpType == MEMBLOCK_LSUOP_SH ||
               fuOpType == MEMBLOCK_LSUOP_SW ||
               fuOpType == MEMBLOCK_LSUOP_SD;
    endfunction:is_store_fuoptype

    static function bit is_cbo_fuoptype(input bit [8:0] fuOpType);
        bit [3:0] low4;

        low4 = fuOpType[3:0];
        return ((fuOpType[3:2] == 2'b11) && (fuOpType[6:4] == 3'b000)) ||
               (low4 == MEMBLOCK_LSUOP_CBO_ZERO[3:0]);
    endfunction:is_cbo_fuoptype

    static function bit is_amocas_q_fuoptype(input bit [8:0] fuOpType);
        return fuOpType[5:0] == MEMBLOCK_LSUOP_AMOCAS_Q_LO;
    endfunction:is_amocas_q_fuoptype

    static function bit is_amocas_wd_fuoptype(input bit [8:0] fuOpType);
        return fuOpType[5:0] == MEMBLOCK_LSUOP_AMOCAS_W_LO ||
               fuOpType[5:0] == MEMBLOCK_LSUOP_AMOCAS_D_LO;
    endfunction:is_amocas_wd_fuoptype

    static function bit is_amo_fuoptype(input bit [8:0] fuOpType);
        return fuOpType == MEMBLOCK_LSUOP_LR_W      ||
               fuOpType == MEMBLOCK_LSUOP_SC_W      ||
               fuOpType == MEMBLOCK_LSUOP_AMOSWAP_W ||
               fuOpType == MEMBLOCK_LSUOP_AMOADD_W  ||
               fuOpType == MEMBLOCK_LSUOP_AMOXOR_W  ||
               fuOpType == MEMBLOCK_LSUOP_AMOAND_W  ||
               fuOpType == MEMBLOCK_LSUOP_AMOOR_W   ||
               fuOpType == MEMBLOCK_LSUOP_AMOMIN_W  ||
               fuOpType == MEMBLOCK_LSUOP_AMOMAX_W  ||
               fuOpType == MEMBLOCK_LSUOP_AMOMINU_W ||
               fuOpType == MEMBLOCK_LSUOP_AMOMAXU_W ||
               fuOpType == MEMBLOCK_LSUOP_LR_D      ||
               fuOpType == MEMBLOCK_LSUOP_SC_D      ||
               fuOpType == MEMBLOCK_LSUOP_AMOSWAP_D ||
               fuOpType == MEMBLOCK_LSUOP_AMOADD_D  ||
               fuOpType == MEMBLOCK_LSUOP_AMOXOR_D  ||
               fuOpType == MEMBLOCK_LSUOP_AMOAND_D  ||
               fuOpType == MEMBLOCK_LSUOP_AMOOR_D   ||
               fuOpType == MEMBLOCK_LSUOP_AMOMIN_D  ||
               fuOpType == MEMBLOCK_LSUOP_AMOMAX_D  ||
               fuOpType == MEMBLOCK_LSUOP_AMOMINU_D ||
               fuOpType == MEMBLOCK_LSUOP_AMOMAXU_D ||
               (fuOpType[5:2] == 4'b1011);
    endfunction:is_amo_fuoptype

    static function memblock_op_behavior_t make_default_behavior();
        memblock_op_behavior_t behavior;

        behavior.kind                   = MEMBLOCK_OP_BEHAVIOR_UNKNOWN;
        behavior.need_alloc             = 2'b00;
        behavior.uses_lq                = 1'b0;
        behavior.uses_sq                = 1'b0;
        behavior.route_load             = 1'b0;
        behavior.route_sta              = 1'b0;
        behavior.route_std              = 1'b0;
        behavior.commit_is_load         = 1'b0;
        behavior.commit_is_store        = 1'b0;
        behavior.commit_is_normal       = 1'b1;
        behavior.is_prefetch            = 1'b0;
        behavior.is_cbo                 = 1'b0;
        behavior.is_atomic              = 1'b0;
        behavior.num_ls_elem            = memblock_num_ls_elem_t'(0);
        behavior.atomic_sta_uop_count   = 3'd0;
        behavior.atomic_data_uop_count  = 3'd0;
        return behavior;
    endfunction:make_default_behavior

    // 中文注释：V2 ROB 的 scommit/pendingst 使用 CommitType.STORE && !vls。
    // STU CBO 也属于该 scalar ROB store 分类；vector LS 在 behavior 入口已拒绝，
    // atomic 则使用独立 kind。该 helper 只做分类，不改变 LSQ allocation/route 状态。
    static function bit is_scalar_rob_store_commit(
        input memblock_op_behavior_t behavior
    );
        return behavior.commit_is_store &&
               !behavior.is_atomic &&
               (behavior.kind == MEMBLOCK_OP_BEHAVIOR_STORE ||
                behavior.kind == MEMBLOCK_OP_BEHAVIOR_CBO);
    endfunction:is_scalar_rob_store_commit

    // 中文注释：从 main transaction 纯派生 LSQ allocation、route 和 commit
    // 行为；函数不读取或修改任何运行期 owner 状态。
    static function memblock_op_behavior_t derive_op_behavior(
        input main_control_transaction tr
    );
        memblock_op_behavior_t behavior;

        if (tr == null) begin
            `uvm_fatal("LSQ_CTRL", "derive_op_behavior got null transaction")
        end
        if (is_vector_ls_futype(tr.fuType)) begin
            `uvm_fatal("LSQ_CTRL",
                       $sformatf("uid=%0d vector LS is not supported by initial lsq_ctrl_model",
                                 tr.uid))
        end

        behavior = make_default_behavior();
        if (tr.fuType == MEMBLOCK_FUTYPE_LDU) begin
            behavior.need_alloc       = 2'b01;
            behavior.uses_lq          = 1'b1;
            behavior.route_load       = 1'b1;
            behavior.commit_is_load   = 1'b1;
            behavior.commit_is_normal = 1'b0;
            behavior.num_ls_elem      = memblock_num_ls_elem_t'(1);
            if (is_prefetch_fuoptype(tr.fuOpType)) begin
                behavior.kind        = MEMBLOCK_OP_BEHAVIOR_PREFETCH;
                behavior.is_prefetch = 1'b1;
            end else if (is_load_fuoptype(tr.fuOpType)) begin
                behavior.kind = MEMBLOCK_OP_BEHAVIOR_LOAD;
            end else begin
                `uvm_fatal("LSQ_CTRL",
                           $sformatf("uid=%0d has illegal LDU fuOpType=%0d",
                                     tr.uid, tr.fuOpType))
            end
        end else if (tr.fuType == MEMBLOCK_FUTYPE_STU) begin
            behavior.need_alloc       = 2'b10;
            behavior.uses_sq          = 1'b1;
            behavior.route_sta        = 1'b1;
            behavior.route_std        = 1'b1;
            behavior.commit_is_store  = 1'b1;
            behavior.commit_is_normal = 1'b0;
            behavior.num_ls_elem      = memblock_num_ls_elem_t'(1);
            if (is_cbo_fuoptype(tr.fuOpType)) begin
                behavior.kind   = MEMBLOCK_OP_BEHAVIOR_CBO;
                behavior.is_cbo = 1'b1;
            end else if (is_store_fuoptype(tr.fuOpType)) begin
                behavior.kind = MEMBLOCK_OP_BEHAVIOR_STORE;
            end else begin
                `uvm_fatal("LSQ_CTRL",
                           $sformatf("uid=%0d has illegal STU fuOpType=%0d",
                                     tr.uid, tr.fuOpType))
            end
        end else if (tr.fuType == MEMBLOCK_FUTYPE_MOU) begin
            if (!is_amo_fuoptype(tr.fuOpType)) begin
                `uvm_fatal("LSQ_CTRL",
                           $sformatf("uid=%0d has illegal MOU fuOpType=%0d",
                                     tr.uid, tr.fuOpType))
            end
            behavior.kind             = MEMBLOCK_OP_BEHAVIOR_ATOMIC;
            behavior.need_alloc       = 2'b00;
            behavior.route_sta        = 1'b1;
            behavior.route_std        = 1'b1;
            behavior.commit_is_normal = 1'b1;
            behavior.is_atomic        = 1'b1;
            behavior.num_ls_elem      = memblock_num_ls_elem_t'(0);
            if (is_amocas_q_fuoptype(tr.fuOpType)) begin
                behavior.atomic_sta_uop_count  = 3'd2;
                behavior.atomic_data_uop_count = 3'd4;
            end else if (is_amocas_wd_fuoptype(tr.fuOpType)) begin
                behavior.atomic_sta_uop_count  = 3'd1;
                behavior.atomic_data_uop_count = 3'd2;
            end else begin
                behavior.atomic_sta_uop_count  = 3'd1;
                behavior.atomic_data_uop_count = 3'd1;
            end
        end else begin
            `uvm_fatal("LSQ_CTRL",
                       $sformatf("uid=%0d has unsupported fuType=0x%0h",
                                 tr.uid, tr.fuType))
        end
        return behavior;
    endfunction:derive_op_behavior

endclass:memblock_op_behavior_util

`endif
