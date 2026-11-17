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
    // 中文注释：SFENCE_CONTROL主表原始目标；自动构造全范围，手工配置保留到owner结束。
    // 通配只由scope决定；addr/id为0仍可作为合法指定目标，GVMA的addr已是GPA>>2。
    memblock_sfence_scope_e sfence_scope;
    bit [49:0] sfence_addr;
    bit [15:0] sfence_id;

    rand memblock_op_class_e op_class;
    memblock_boundary_profile_e boundary_profile;
    int unsigned             boundary_size_bytes;
    rand memblock_lsq_flow_e lsq_flow;
    rand bit [MEMBLOCK_INTERNAL_FUTYPE_W-1:0] fuType;
    rand bit [8:0]           fuOpType;
    rand bit [63:0]          src_0;
    rand bit [63:0]          imm;
    bit [63:0]               vaddr;
    bit                      amo_expect_misaligned;
    int unsigned             amo_alignment_bytes;
    // 中文注释：建表时冻结的 STD 四槽数据，constructor 清零，生成器按成员角色设置。
    // valid 为 1 时 issue_field_assigner 优先消费，重发和 shuffle 不重新派生；普通
    // Store 只允许 slot0，CAS.W/D 必须同时设置 0/1，CAS.Q 必须同时设置四槽。
    bit                      std_data_override_valid[4];
    bit [63:0]               std_data_override[4];

    rand bit                 robIdx_flag;
    rand bit [MEMBLOCK_ROB_VALUE_W-1:0] robIdx_value;
    rand bit                 lqIdx_flag;
    rand bit [MEMBLOCK_LQ_VALUE_W-1:0] lqIdx_value;
    rand bit                 sqIdx_flag;
    rand bit [MEMBLOCK_SQ_VALUE_W-1:0] sqIdx_value;
    rand memblock_num_ls_elem_t numLsElem;

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
        numLsElem inside {[0:MEMBLOCK_DUT_MAX_LS_ELEM]};
    }

    function new(string name = "main_control_transaction");
        super.new(name);
        uid          = 0;
        sfence_scope = MEMBLOCK_SFENCE_SCOPE_ALL_ADDR_ALL_ID;
        sfence_addr = '0;
        sfence_id = '0;
        op_class     = MEMBLOCK_OP_CLASS_UNKNOWN;
        boundary_profile = MEMBLOCK_BOUNDARY_PROFILE_UNKNOWN;
        boundary_size_bytes = 0;
        lsq_flow     = MEMBLOCK_LSQ_FLOW_NONE;
        fuType       = '0;
        fuOpType     = '0;
        src_0        = '0;
        imm          = '0;
        vaddr        = '0;
        amo_expect_misaligned = 1'b0;
        amo_alignment_bytes = 0;
        foreach (std_data_override_valid[i]) begin
            std_data_override_valid[i] = 1'b0;
            std_data_override[i] = '0;
        end
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

    // 抽象职责：复制已冻结主表输入，特别保留手工失效范围和STD操作数；不重新生成字段。
    virtual function void do_copy(uvm_object rhs);
        main_control_transaction source;
        super.do_copy(rhs);
        if (!$cast(source, rhs)) begin
            `uvm_fatal("MAIN_TR", "main transaction copy requires matching type")
            return;
        end
        uid = source.uid;
        sfence_scope = source.sfence_scope;
        sfence_addr = source.sfence_addr;
        sfence_id = source.sfence_id;
        op_class = source.op_class;
        boundary_profile = source.boundary_profile;
        boundary_size_bytes = source.boundary_size_bytes;
        lsq_flow = source.lsq_flow;
        fuType = source.fuType;
        fuOpType = source.fuOpType;
        src_0 = source.src_0;
        imm = source.imm;
        vaddr = source.vaddr;
        amo_expect_misaligned = source.amo_expect_misaligned;
        amo_alignment_bytes = source.amo_alignment_bytes;
        robIdx_flag = source.robIdx_flag;
        robIdx_value = source.robIdx_value;
        lqIdx_flag = source.lqIdx_flag;
        lqIdx_value = source.lqIdx_value;
        sqIdx_flag = source.sqIdx_flag;
        sqIdx_value = source.sqIdx_value;
        numLsElem = source.numLsElem;
        tlbAF = source.tlbAF;
        tlbPF = source.tlbPF;
        tlbGPF = source.tlbGPF;
        PBMT = source.PBMT;
        pmaAF = source.pmaAF;
        corrupt = source.corrupt;
        denied = source.denied;
        delay = source.delay;
        send_pri = source.send_pri;
        send_pri_std = source.send_pri_std;
        foreach (std_data_override_valid[i]) begin
            std_data_override_valid[i] = source.std_data_override_valid[i];
            std_data_override[i] = source.std_data_override[i];
        end
    endfunction:do_copy

    // 抽象职责：打印手工/自动控制范围的真实原始输入，不依据addr/id推导通配。
    virtual function void do_print(uvm_printer printer);
        super.do_print(printer);
        printer.print_field_int("uid", uid, $bits(uid), UVM_DEC);
        printer.print_string("sfence_scope", sfence_scope.name());
        printer.print_field_int("sfence_addr", sfence_addr, 50, UVM_HEX);
        printer.print_field_int("sfence_id", sfence_id, 16, UVM_HEX);
    endfunction:do_print

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
        bit any_override;
        if (op_class == MEMBLOCK_OP_CLASS_SFENCE_CONTROL &&
            !(sfence_scope inside {MEMBLOCK_SFENCE_SCOPE_ALL_ADDR_ALL_ID,
                                   MEMBLOCK_SFENCE_SCOPE_ADDR_ALL_ID,
                                   MEMBLOCK_SFENCE_SCOPE_ALL_ADDR_ID,
                                   MEMBLOCK_SFENCE_SCOPE_ADDR_ID})) return 1'b0;
        any_override = 1'b0;
        foreach (std_data_override_valid[i]) any_override |= std_data_override_valid[i];
        if (any_override) begin
            if (op_class == MEMBLOCK_OP_CLASS_STORE) begin
                if (!std_data_override_valid[0] || std_data_override_valid[1] ||
                    std_data_override_valid[2] || std_data_override_valid[3]) return 1'b0;
            end else if (op_class == MEMBLOCK_OP_CLASS_AMO) begin
                bit [3:0] present_mask;
                bit [3:0] required_mask;
                present_mask = '0;
                foreach (std_data_override_valid[slot])
                    present_mask[slot] = std_data_override_valid[slot];
                if (fuOpType == MEMBLOCK_LSUOP_AMOCAS_Q) required_mask = 4'b1111;
                else if (fuOpType inside {MEMBLOCK_LSUOP_AMOCAS_W,
                                          MEMBLOCK_LSUOP_AMOCAS_D})
                    required_mask = 4'b0011;
                else required_mask = 4'b0001;
                if (present_mask != required_mask) return 1'b0;
            end else return 1'b0;
        end
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
