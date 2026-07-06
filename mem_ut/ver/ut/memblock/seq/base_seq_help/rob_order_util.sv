//=========================================================
//File name    : rob_order_util.sv
//Author       : OpenAI_Codex
//Module name  : rob_order_util
//Discribution : ROB circular pointer helper for dispatch framework
//Date         : 2026-05-18
//=========================================================
`ifndef ROB_ORDER_UTIL__SV
`define ROB_ORDER_UTIL__SV

class rob_order_util;

    static function void check_rob_key(input memblock_rob_key_t key,
                                       input string caller);
        if (key.value >= MEMBLOCK_ROB_SIZE) begin
            `uvm_fatal("ROB_UTIL",
                       $sformatf("%s got robIdx value=%0d exceeds ROB size=%0d",
                                 caller, key.value, MEMBLOCK_ROB_SIZE))
        end
    endfunction:check_rob_key

    static function memblock_rob_map_key_t rob_to_map_key(input memblock_rob_key_t key);
        check_rob_key(key, "rob_to_map_key");
        return {key.flag, key.value};
    endfunction:rob_to_map_key

    static function memblock_lq_map_key_t lq_to_map_key(input memblock_lq_key_t key);
        return {key.flag, key.value};
    endfunction:lq_to_map_key

    static function memblock_sq_map_key_t sq_to_map_key(input memblock_sq_key_t key);
        return {key.flag, key.value};
    endfunction:sq_to_map_key

    static function memblock_rob_key_t rob_advance(input memblock_rob_key_t base,
                                                   input int unsigned step);
        memblock_rob_key_t cur;
        check_rob_key(base, "rob_advance");
        cur = base;
        repeat (step) begin
            if (cur.value == MEMBLOCK_ROB_SIZE - 1) begin
                cur.value = '0;
                cur.flag  = ~cur.flag;
            end else begin
                cur.value = cur.value + 1'b1;
            end
        end
        return cur;
    endfunction:rob_advance

    static function bit rob_is_after(input memblock_rob_key_t left,
                                     input memblock_rob_key_t right);
        bit different_flag;
        bit compare_value;
        check_rob_key(left, "rob_is_after.left");
        check_rob_key(right, "rob_is_after.right");
        different_flag = left.flag ^ right.flag;
        compare_value  = left.value > right.value;
        return different_flag ^ compare_value;
    endfunction:rob_is_after

    static function bit rob_need_flush(input memblock_rob_key_t uop_rob,
                                       input memblock_redirect_payload_t redirect);
        bit same_rob;
        if (!redirect.valid) begin
            return 1'b0;
        end
        same_rob = rob_to_map_key(uop_rob) == rob_to_map_key(redirect.rob_key);
        return (redirect.flush_itself && same_rob) || rob_is_after(uop_rob, redirect.rob_key);
    endfunction:rob_need_flush

endclass:rob_order_util

`endif
