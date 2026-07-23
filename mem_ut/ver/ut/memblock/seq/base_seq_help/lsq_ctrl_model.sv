//=========================================================
//File name    : lsq_ctrl_model.sv
//Author       : OpenAI_Codex
//Module name  : lsq_ctrl_model
//Discribution : software LSQ allocation mirror for dispatch framework
//Date         : 2026-05-18
//=========================================================
`ifndef LSQ_CTRL_MODEL__SV
`define LSQ_CTRL_MODEL__SV

class lsq_ctrl_model extends uvm_object;

    static lsq_ctrl_model m_inst;

    common_data_transaction data;

    memblock_lq_key_t lq_enq_ptr;
    memblock_sq_key_t sq_enq_ptr;
    memblock_lq_key_t lq_deq_ptr;
    memblock_sq_key_t sq_deq_ptr;
    int unsigned      lq_free_count;
    int unsigned      sq_free_count;

    `uvm_object_utils(lsq_ctrl_model)

    function new(string name = "lsq_ctrl_model");
        super.new(name);
        data = common_data_transaction::get();
        reset();
    endfunction:new

    static function lsq_ctrl_model get();
        if (m_inst == null) begin
            m_inst = new("lsq_ctrl_model_singleton");
        end
        return m_inst;
    endfunction:get

    function void reset();
        lq_enq_ptr    = '{default:'0};
        sq_enq_ptr    = '{default:'0};
        lq_deq_ptr    = '{default:'0};
        sq_deq_ptr    = '{default:'0};
        lq_free_count = MEMBLOCK_LQ_SIZE;
        sq_free_count = MEMBLOCK_SQ_SIZE;
    endfunction:reset

    static function bit is_vector_ls_futype(input bit [MEMBLOCK_INTERNAL_FUTYPE_W-1:0] fuType);
        return memblock_op_behavior_util::is_vector_ls_futype(fuType);
    endfunction:is_vector_ls_futype

    static function bit is_load_fuoptype(input bit [8:0] fuOpType);
        return memblock_op_behavior_util::is_load_fuoptype(fuOpType);
    endfunction:is_load_fuoptype

    static function bit is_prefetch_fuoptype(input bit [8:0] fuOpType);
        return memblock_op_behavior_util::is_prefetch_fuoptype(fuOpType);
    endfunction:is_prefetch_fuoptype

    static function bit is_store_fuoptype(input bit [8:0] fuOpType);
        return memblock_op_behavior_util::is_store_fuoptype(fuOpType);
    endfunction:is_store_fuoptype

    static function bit is_cbo_fuoptype(input bit [8:0] fuOpType);
        return memblock_op_behavior_util::is_cbo_fuoptype(fuOpType);
    endfunction:is_cbo_fuoptype

    static function bit is_amocas_q_fuoptype(input bit [8:0] fuOpType);
        return memblock_op_behavior_util::is_amocas_q_fuoptype(fuOpType);
    endfunction:is_amocas_q_fuoptype

    static function bit is_amocas_wd_fuoptype(input bit [8:0] fuOpType);
        return memblock_op_behavior_util::is_amocas_wd_fuoptype(fuOpType);
    endfunction:is_amocas_wd_fuoptype

    static function bit is_amo_fuoptype(input bit [8:0] fuOpType);
        return memblock_op_behavior_util::is_amo_fuoptype(fuOpType);
    endfunction:is_amo_fuoptype

    static function memblock_op_behavior_t make_default_behavior();
        return memblock_op_behavior_util::make_default_behavior();
    endfunction:make_default_behavior

    static function memblock_op_behavior_t derive_op_behavior(input main_control_transaction tr);
        return memblock_op_behavior_util::derive_op_behavior(tr);
    endfunction:derive_op_behavior

//   所以 atomic_data_uop_count 表示：
//   这个 atomic 需要发几个“数据侧 uop”。
//   举例：
//   AMOCAS_Q:
//     atomic_sta_uop_count  = 2
//     atomic_data_uop_count = 4
//   意思是测试框架会把这个 AMOCAS_Q 抽象成：
//   - 2 个地址侧 STA uop
//   - 4 个数据侧 STD/data uop
//   为什么 CAS 会更多？因为 CAS 需要比较旧值和写入新值，宽度越大，内部拆分的数据片越多。Q 比 W/D 更宽，所以 data uop 数更多。
//   STA uop 数表示地址侧要发几份；data/STD uop 数表示数据侧要发几份。它们用于测试框架决定 atomic 要往 STA/STD 两条 issue 路径拆多少个微操作。

    static function memblock_lq_key_t advance_lq_key(input memblock_lq_key_t base,
                                                     input int unsigned step);
        memblock_lq_key_t cur;

        if (base.value >= MEMBLOCK_LQ_SIZE) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("lqIdx value=%0d exceeds LQ size=%0d", base.value, MEMBLOCK_LQ_SIZE))
        end
        cur = base;
        repeat (step) begin
            if (cur.value == MEMBLOCK_LQ_SIZE - 1) begin
                cur.value = '0;
                cur.flag  = ~cur.flag;
            end else begin
                cur.value = cur.value + 1'b1;
            end
        end
        return cur;
    endfunction:advance_lq_key

    static function memblock_sq_key_t advance_sq_key(input memblock_sq_key_t base,
                                                     input int unsigned step);
        memblock_sq_key_t cur;

        if (base.value >= MEMBLOCK_SQ_SIZE) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("sqIdx value=%0d exceeds SQ size=%0d", base.value, MEMBLOCK_SQ_SIZE))
        end
        cur = base;
        repeat (step) begin
            if (cur.value == MEMBLOCK_SQ_SIZE - 1) begin
                cur.value = '0;
                cur.flag  = ~cur.flag;
            end else begin
                cur.value = cur.value + 1'b1;
            end
        end
        return cur;
    endfunction:advance_sq_key

    static function memblock_lq_key_t rewind_lq_key(input memblock_lq_key_t base,
                                                    input int unsigned step);
        memblock_lq_key_t cur;

        if (base.value >= MEMBLOCK_LQ_SIZE) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("lqIdx value=%0d exceeds LQ size=%0d", base.value, MEMBLOCK_LQ_SIZE))
        end
        cur = base;
        repeat (step) begin
            if (cur.value == 0) begin
                cur.value = MEMBLOCK_LQ_SIZE - 1;
                cur.flag  = ~cur.flag;
            end else begin
                cur.value = cur.value - 1'b1;
            end
        end
        return cur;
    endfunction:rewind_lq_key

    static function memblock_sq_key_t rewind_sq_key(input memblock_sq_key_t base,
                                                    input int unsigned step);
        memblock_sq_key_t cur;

        if (base.value >= MEMBLOCK_SQ_SIZE) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("sqIdx value=%0d exceeds SQ size=%0d", base.value, MEMBLOCK_SQ_SIZE))
        end
        cur = base;
        repeat (step) begin
            if (cur.value == 0) begin
                cur.value = MEMBLOCK_SQ_SIZE - 1;
                cur.flag  = ~cur.flag;
            end else begin
                cur.value = cur.value - 1'b1;
            end
        end
        return cur;
    endfunction:rewind_sq_key

    function bit can_allocate(input memblock_op_behavior_t behavior);
        if (behavior.uses_lq && lq_free_count < behavior.num_ls_elem) begin
            return 1'b0;
        end
        if (behavior.uses_sq && sq_free_count < behavior.num_ls_elem) begin
            return 1'b0;
        end
        return 1'b1;
    endfunction:can_allocate

    function void preview_allocate(input memblock_op_behavior_t behavior,
                                   output memblock_lq_key_t lq_key,
                                   output memblock_sq_key_t sq_key);
        lq_key = lq_enq_ptr;
        sq_key = sq_enq_ptr;
        if (!can_allocate(behavior)) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("preview_allocate lacks resources: kind=%0d lq_free=%0d sq_free=%0d need=%0d",
                                             behavior.kind, lq_free_count, sq_free_count, behavior.num_ls_elem))
        end
    endfunction:preview_allocate

    function void commit_allocate(input memblock_uid_t uid,
                                  input memblock_op_behavior_t behavior,
                                  input main_control_transaction tr);
        memblock_lq_key_t lq_key;
        memblock_sq_key_t sq_key;

        if (data == null) begin
            data = common_data_transaction::get();
        end
        if (tr == null) begin
            `uvm_fatal("LSQ_CTRL", "commit_allocate got null transaction")
        end
        if (tr.uid != uid) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("commit_allocate uid mismatch: arg=%0d tr.uid=%0d", uid, tr.uid))
        end
        preview_allocate(behavior, lq_key, sq_key);

        tr.lqIdx_flag  = lq_key.flag;
        tr.lqIdx_value = lq_key.value;
        tr.sqIdx_flag  = sq_key.flag;
        tr.sqIdx_value = sq_key.value;
        tr.numLsElem   = behavior.num_ls_elem;

        data.set_main_transaction(uid, tr);
        data.activate_uid(uid, behavior.uses_lq, behavior.uses_sq);
        data.set_status_field(uid, MEMBLOCK_STATUS_ENQ, 1'b1);

        if (behavior.uses_lq) begin
            lq_enq_ptr = advance_lq_key(lq_enq_ptr, behavior.num_ls_elem);
            lq_free_count -= behavior.num_ls_elem;
        end
        if (behavior.uses_sq) begin
            sq_enq_ptr = advance_sq_key(sq_enq_ptr, behavior.num_ls_elem);
            sq_free_count -= behavior.num_ls_elem;
        end
    endfunction:commit_allocate

    function void commit_allocate_with_resp(input memblock_uid_t uid,
                                            input memblock_op_behavior_t behavior,
                                            input main_control_transaction tr,
                                            input memblock_lq_key_t dut_lq_key,
                                            input memblock_sq_key_t dut_sq_key);
        memblock_lq_key_t expected_lq_key;
        memblock_sq_key_t expected_sq_key;

        if (data == null) begin
            data = common_data_transaction::get();
        end
        if (tr == null) begin
            `uvm_fatal("LSQ_CTRL", "commit_allocate_with_resp got null transaction")
        end
        if (tr.uid != uid) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("commit_allocate_with_resp uid mismatch: arg=%0d tr.uid=%0d", uid, tr.uid))
        end
        if (behavior.need_alloc == 2'b00) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("commit_allocate_with_resp got non-LSQ admission uid=%0d", uid))
        end

        preview_allocate(behavior, expected_lq_key, expected_sq_key);
        if ((behavior.uses_lq && dut_lq_key != expected_lq_key) ||
            (behavior.uses_sq && dut_sq_key != expected_sq_key)) begin
            `uvm_fatal("LSQ_CTRL",
                       $sformatf("uid=%0d LSQ enq resp mismatch: expected lq={%0d,%0d} sq={%0d,%0d}, got lq={%0d,%0d} sq={%0d,%0d}",
                                 uid,
                                 expected_lq_key.flag,
                                 expected_lq_key.value,
                                 expected_sq_key.flag,
                                 expected_sq_key.value,
                                 dut_lq_key.flag,
                                 dut_lq_key.value,
                                 dut_sq_key.flag,
                                 dut_sq_key.value))
        end

        commit_allocate(uid, behavior, tr);
    endfunction:commit_allocate_with_resp

    function void commit_non_lsq_admission(input memblock_uid_t uid,
                                           input memblock_op_behavior_t behavior,
                                           input main_control_transaction tr);
        if (behavior.need_alloc != 2'b00 || behavior.uses_lq || behavior.uses_sq) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("commit_non_lsq_admission got LSQ allocating kind=%0d", behavior.kind))
        end
        commit_allocate(uid, behavior, tr);
    endfunction:commit_non_lsq_admission

    function void release_lq(input int unsigned count);
        if (count > (MEMBLOCK_LQ_SIZE - lq_free_count)) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("release_lq count=%0d exceeds allocated count=%0d",
                                             count, MEMBLOCK_LQ_SIZE - lq_free_count))
        end
        lq_deq_ptr = advance_lq_key(lq_deq_ptr, count);
        lq_free_count += count;
    endfunction:release_lq

    function void release_sq(input int unsigned count);
        if (count > (MEMBLOCK_SQ_SIZE - sq_free_count)) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("release_sq count=%0d exceeds allocated count=%0d",
                                             count, MEMBLOCK_SQ_SIZE - sq_free_count))
        end
        sq_deq_ptr = advance_sq_key(sq_deq_ptr, count);
        sq_free_count += count;
    endfunction:release_sq

    function void cancel_lq(input int unsigned count);
        if (count > (MEMBLOCK_LQ_SIZE - lq_free_count)) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("cancel_lq count=%0d exceeds allocated count=%0d",
                                             count, MEMBLOCK_LQ_SIZE - lq_free_count))
        end
        lq_enq_ptr = rewind_lq_key(lq_enq_ptr, count);
        lq_free_count += count;
    endfunction:cancel_lq

    function void cancel_sq(input int unsigned count);
        if (count > (MEMBLOCK_SQ_SIZE - sq_free_count)) begin
            `uvm_fatal("LSQ_CTRL", $sformatf("cancel_sq count=%0d exceeds allocated count=%0d",
                                             count, MEMBLOCK_SQ_SIZE - sq_free_count))
        end
        sq_enq_ptr = rewind_sq_key(sq_enq_ptr, count);
        sq_free_count += count;
    endfunction:cancel_sq

endclass:lsq_ctrl_model

`endif
