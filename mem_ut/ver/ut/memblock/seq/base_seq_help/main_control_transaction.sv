//=========================================================
//File name    : main_control_transaction.sv
//Author       : OpenAI_Codex
//Module name  : main_control_transaction
//Discribution : dispatch framework main control table entry
//Date         : 2026-05-18
//=========================================================
`ifndef MAIN_CONTROL_TRANSACTION__SV
`define MAIN_CONTROL_TRANSACTION__SV

class main_control_transaction extends uvm_object;

    memblock_uid_t uid;

    rand memblock_op_class_e op_class;
    memblock_boundary_profile_e boundary_profile;
    int unsigned             boundary_size_bytes;
    rand memblock_lsq_flow_e lsq_flow;
    rand bit [35:0]          fuType;
    rand bit [8:0]           fuOpType;
    rand bit [63:0]          src_0;
    rand bit [63:0]          imm;
    bit [63:0]               vaddr;

    rand bit                 robIdx_flag;
    rand bit [8:0]           robIdx_value;
    rand bit                 lqIdx_flag;
    rand bit [6:0]           lqIdx_value;
    rand bit                 sqIdx_flag;
    rand bit [5:0]           sqIdx_value;
    rand bit [4:0]           numLsElem;

    rand bit                 tlbAF;
    rand bit                 tlbPF;
    rand bit                 tlbGPF;
    rand bit [1:0]           PBMT;
    rand bit                 pmaAF;
    rand bit                 corrupt;
    rand bit                 denied;

    rand int unsigned        delay;
    rand int unsigned        send_pri;
    rand int unsigned        send_pri_std;

    `uvm_object_utils(main_control_transaction)

    constraint c_send_pri_range {
        send_pri inside {[0:100]};
        send_pri_std inside {[0:100]};
    }

    constraint c_scalar_lsq_elem {
        numLsElem inside {[0:31]};
    }

    function new(string name = "main_control_transaction");
        super.new(name);
        uid          = 0;
        op_class     = MEMBLOCK_OP_CLASS_UNKNOWN;
        boundary_profile = MEMBLOCK_BOUNDARY_PROFILE_UNKNOWN;
        boundary_size_bytes = 0;
        lsq_flow     = MEMBLOCK_LSQ_FLOW_NONE;
        fuType       = '0;
        fuOpType     = '0;
        src_0        = '0;
        imm          = '0;
        vaddr        = '0;
        robIdx_flag  = 1'b0;
        robIdx_value = '0;
        lqIdx_flag   = 1'b0;
        lqIdx_value  = '0;
        sqIdx_flag   = 1'b0;
        sqIdx_value  = '0;
        numLsElem    = '0;
        tlbAF        = 1'b0;
        tlbPF        = 1'b0;
        tlbGPF       = 1'b0;
        PBMT         = '0;
        pmaAF        = 1'b0;
        corrupt      = 1'b0;
        denied       = 1'b0;
        delay        = 0;
        send_pri     = 0;
        send_pri_std = 0;
    endfunction:new

    function void pre_randomize();
        seq_csr_common::check_initialized("main_control_transaction::pre_randomize");
    endfunction:pre_randomize

    function void post_randomize();
        update_vaddr();
        if (!validate_main_transaction()) begin
            `uvm_fatal("MAIN_TR", $sformatf("illegal main transaction uid=%0d", uid))
        end
    endfunction:post_randomize

    function void post_manual_config(input bit recompute_vaddr = 1'b1);
        if (recompute_vaddr) begin
            update_vaddr();
        end
        if (!validate_main_transaction()) begin
            `uvm_fatal("MAIN_TR", $sformatf("illegal manual main transaction uid=%0d", uid))
        end
    endfunction:post_manual_config

    function void update_vaddr();
        vaddr = src_0 + sign_extend_imm12(imm);
    endfunction:update_vaddr

    function bit validate_main_transaction();
        if (vaddr != (src_0 + sign_extend_imm12(imm))) begin
            return 1'b0;
        end
        if (send_pri > 100 || send_pri_std > 100) begin
            return 1'b0;
        end
        return 1'b1;
    endfunction:validate_main_transaction

    function memblock_rob_key_t get_rob_key();
        memblock_rob_key_t key;
        key.flag  = robIdx_flag;
        key.value = robIdx_value;
        return key;
    endfunction:get_rob_key

    function memblock_lq_key_t get_lq_key();
        memblock_lq_key_t key;
        key.flag  = lqIdx_flag;
        key.value = lqIdx_value;
        return key;
    endfunction:get_lq_key

    function memblock_sq_key_t get_sq_key();
        memblock_sq_key_t key;
        key.flag  = sqIdx_flag;
        key.value = sqIdx_value;
        return key;
    endfunction:get_sq_key

    static function bit [63:0] sign_extend_imm12(input bit [63:0] imm_i);
        bit [11:0] imm12;
        imm12 = imm_i[11:0];
        return {{52{imm12[11]}}, imm12};
    endfunction:sign_extend_imm12

endclass:main_control_transaction

`endif
