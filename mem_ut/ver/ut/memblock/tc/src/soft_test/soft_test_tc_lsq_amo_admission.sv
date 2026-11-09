`ifndef SOFT_TEST_TC_LSQ_AMO_ADMISSION__SV
`define SOFT_TEST_TC_LSQ_AMO_ADMISSION__SV

class memblock_amo_rm_core_soft_checker extends uvm_object;
    `uvm_object_utils(memblock_amo_rm_core_soft_checker)

    function new(string name = "memblock_amo_rm_core_soft_checker");
        super.new(name);
    endfunction:new

    function void check_expect(bit condition, string message);
        if (!condition) `uvm_fatal("AMO_RM_SOFT", message)
    endfunction:check_expect

    function rm_ls_program_item_t make_item(int unsigned uid,
                                            bit [8:0] op,
                                            rm_atomic_kind_e kind,
                                            int unsigned size_bytes,
                                            longint unsigned pa);
        rm_ls_program_item_t item;
        bit [15:0] required_mask;
        item = new(uid);
        item.valid = 1'b1;
        item.kind = RM_LS_KIND_ATOMIC;
        item.atomic_kind = kind;
        item.op = op;
        item.rob = '{flag:1'b0, value:uid[MEMBLOCK_ROB_VALUE_W-1:0]};
        item.computed_vaddr = pa;
        item.main_vaddr = pa;
        item.size_bytes = size_bytes;
        item.translation_valid = 1'b1;
        item.dynamic_epoch = 1;
        item.expected_atomic_wb_count = size_bytes == 16 ? 2 : 1;
        item.atomic_expected_pdest_valid = size_bytes == 16 ? 2'b11 : 2'b01;
        item.atomic_expected_pdest[0] = 8'h21;
        item.atomic_expected_pdest[1] = 8'h42;
        required_mask = 16'hffff >> (16 - size_bytes);
        item.pa_valid_mask = required_mask;
        for (int unsigned index = 0; index < size_bytes; index++)
            item.pa_by_byte[index] = pa + index;
        return item;
    endfunction:make_item

    function rm_atomic_actual_t make_actual(rm_ls_program_item_t item,
                                            bit [127:0] wb_data,
                                            bit reverse_q = 1'b0,
                                            bit [23:0] exception_vec = '0);
        rm_atomic_actual_t actual;
        int unsigned low_index;
        int unsigned high_index;
        actual = new();
        actual.valid = 1'b1;
        actual.uid = item.uid;
        actual.rob = item.rob;
        actual.dynamic_epoch = item.dynamic_epoch;
        actual.wb_count = item.expected_atomic_wb_count;
        low_index = reverse_q && actual.wb_count == 2 ? 1 : 0;
        high_index = reverse_q ? 0 : 1;
        actual.data_valid[low_index] = 1'b1;
        actual.data[low_index] = wb_data[63:0];
        actual.exception_vec[low_index] = exception_vec;
        actual.pdest_valid[low_index] = 1'b1;
        actual.pdest[low_index] = item.atomic_expected_pdest[0];
        actual.rf_wen_valid[low_index] = 1'b1;
        actual.rf_wen[low_index] = 1'b1;
        actual.sample_seq[low_index] = 10;
        if (actual.wb_count == 2) begin
            actual.data_valid[high_index] = 1'b1;
            actual.data[high_index] = wb_data[127:64];
            actual.exception_vec[high_index] = exception_vec;
            actual.pdest_valid[high_index] = 1'b1;
            actual.pdest[high_index] = item.atomic_expected_pdest[1];
            actual.rf_wen_valid[high_index] = 1'b1;
            actual.rf_wen[high_index] = 1'b1;
            actual.sample_seq[high_index] = 11;
        end
        return actual;
    endfunction:make_actual

    function void seed_memory(rm_ls_model_t model,
                              rm_ls_program_item_t item,
                              bit [127:0] value);
        for (int unsigned index = 0; index < item.size_bytes; index++)
            check_expect(model.set_backing_byte(
                item.pa_by_byte[index], value[(index * 8) +: 8]),
                $sformatf("failed to seed RM byte %0d", index));
    endfunction:seed_memory

    function void check_effect(rm_ls_model_t model,
                               rm_ls_program_item_t item,
                               bit [127:0] expected);
        byte unsigned actual_byte;
        for (int unsigned index = 0; index < item.size_bytes; index++) begin
            check_expect(model.read_expected_store_byte(item.pa_by_byte[index],
                                                        actual_byte),
                         $sformatf("RM effect byte %0d missing", index));
            check_expect(actual_byte == expected[(index * 8) +: 8],
                         $sformatf("RM effect byte %0d mismatch", index));
        end
    endfunction:check_effect

    function bit [127:0] independent_amo_result(bit [8:0] op,
                                                int unsigned size_bytes,
                                                bit [127:0] old_value,
                                                bit [127:0] operand);
        bit [127:0] result;
        result = old_value;
        if (size_bytes == 4) begin
            case (op)
                MEMBLOCK_LSUOP_AMOSWAP_W: result[31:0] = operand[31:0];
                MEMBLOCK_LSUOP_AMOADD_W:  result[31:0] = old_value[31:0] + operand[31:0];
                MEMBLOCK_LSUOP_AMOXOR_W:  result[31:0] = old_value[31:0] ^ operand[31:0];
                MEMBLOCK_LSUOP_AMOAND_W:  result[31:0] = old_value[31:0] & operand[31:0];
                MEMBLOCK_LSUOP_AMOOR_W:   result[31:0] = old_value[31:0] | operand[31:0];
                MEMBLOCK_LSUOP_AMOMIN_W:  result[31:0] = $signed(old_value[31:0]) < $signed(operand[31:0]) ? old_value[31:0] : operand[31:0];
                MEMBLOCK_LSUOP_AMOMAX_W:  result[31:0] = $signed(old_value[31:0]) > $signed(operand[31:0]) ? old_value[31:0] : operand[31:0];
                MEMBLOCK_LSUOP_AMOMINU_W: result[31:0] = old_value[31:0] < operand[31:0] ? old_value[31:0] : operand[31:0];
                MEMBLOCK_LSUOP_AMOMAXU_W: result[31:0] = old_value[31:0] > operand[31:0] ? old_value[31:0] : operand[31:0];
                default: result = 'x;
            endcase
        end else begin
            case (op)
                MEMBLOCK_LSUOP_AMOSWAP_D: result[63:0] = operand[63:0];
                MEMBLOCK_LSUOP_AMOADD_D:  result[63:0] = old_value[63:0] + operand[63:0];
                MEMBLOCK_LSUOP_AMOXOR_D:  result[63:0] = old_value[63:0] ^ operand[63:0];
                MEMBLOCK_LSUOP_AMOAND_D:  result[63:0] = old_value[63:0] & operand[63:0];
                MEMBLOCK_LSUOP_AMOOR_D:   result[63:0] = old_value[63:0] | operand[63:0];
                MEMBLOCK_LSUOP_AMOMIN_D:  result[63:0] = $signed(old_value[63:0]) < $signed(operand[63:0]) ? old_value[63:0] : operand[63:0];
                MEMBLOCK_LSUOP_AMOMAX_D:  result[63:0] = $signed(old_value[63:0]) > $signed(operand[63:0]) ? old_value[63:0] : operand[63:0];
                MEMBLOCK_LSUOP_AMOMINU_D: result[63:0] = old_value[63:0] < operand[63:0] ? old_value[63:0] : operand[63:0];
                MEMBLOCK_LSUOP_AMOMAXU_D: result[63:0] = old_value[63:0] > operand[63:0] ? old_value[63:0] : operand[63:0];
                default: result = 'x;
            endcase
        end
        return result;
    endfunction:independent_amo_result

    task run();
        rm_ls_model_t model;
        rm_ls_program_item_t item;
        rm_ls_program_item_t lr_item;
        rm_ls_program_item_t sc_item;
        rm_ls_program_item_t effect_item;
        rm_atomic_actual_t actual;
        rm_sc_no_write_obligation_t obligation;
        memblock_rm_readonly_api::main_transaction_view_t main_view;
        bit [8:0] amo_w_ops[$];
        bit [8:0] amo_d_ops[$];
        bit [127:0] old_value;
        bit [127:0] operand;
        bit [127:0] expected;
        longint unsigned base_pa;
        int unsigned atomic_wait_count;
        int unsigned decoded_slot;
        bit snapshot_valid;
        int unsigned snapshot_uid;
        int unsigned snapshot_dynamic_epoch;
        int unsigned snapshot_flush_epoch;
        bit [3:0] snapshot_sta_mask;
        bit [3:0] snapshot_std_mask;

        memblock_sync_pkg::publish_atomic_issue_owner_snapshot(
            17, 3, 5, 4'b0101, 4'b1111);
        memblock_sync_pkg::get_atomic_issue_owner_snapshot(
            snapshot_valid, snapshot_uid, snapshot_dynamic_epoch,
            snapshot_flush_epoch, snapshot_sta_mask, snapshot_std_mask);
        memblock_sync_pkg::clear_atomic_issue_owner_snapshot();
        check_expect(snapshot_valid && snapshot_uid == 17 &&
                     snapshot_dynamic_epoch == 3 && snapshot_flush_epoch == 5 &&
                     snapshot_sta_mask == 4'b0101 &&
                     snapshot_std_mask == 4'b1111,
                     "atomic issue snapshot did not freeze required masks");

        atomic_wait_count = 0;
        for (int unsigned poll = 0; poll < 256; poll++) begin
            check_expect(memblock_rm::observer_atomic_wb_wait_pending(
                             atomic_wait_count, 1'b1, RM_LS_KIND_ATOMIC),
                         $sformatf("atomic missing-WB poll %0d terminated early", poll));
        end
        check_expect(atomic_wait_count == 256 &&
                     !memblock_rm::observer_atomic_wb_wait_pending(
                         atomic_wait_count, 1'b1, RM_LS_KIND_ATOMIC) &&
                     atomic_wait_count == 0,
                     "atomic missing-WB timeout did not expire on poll 257");
        atomic_wait_count = 9;
        check_expect(!memblock_rm::observer_atomic_wb_wait_pending(
                         atomic_wait_count, 1'b0, RM_LS_KIND_ATOMIC) &&
                     atomic_wait_count == 0,
                     "atomic missing-WB wait counter did not reset after evidence appeared");

        main_view = '{default:'0};
        main_view.valid = 1'b1;
        main_view.fu_op_type = MEMBLOCK_LSUOP_AMOCAS_D;
        main_view.rob_idx_value = 7;
        main_view.atomic_std_valid_mask = 4'b0011;
        main_view.atomic_std_data_flat[63:0] = 64'h1111_2222_3333_4444;
        main_view.atomic_std_data_flat[127:64] = 64'haaaa_bbbb_cccc_dddd;
        check_expect(memblock_rm::observer_atomic_std_sample_matches(
                         main_view, 4'b0011, 0,
                         main_view.atomic_std_data_flat[63:0],
                         {3'd0, main_view.fu_op_type[5:0]}, 7, decoded_slot) &&
                     decoded_slot == 0,
                     "valid CAS.D compare STD sample was rejected");
        check_expect(!memblock_rm::observer_atomic_std_sample_matches(
                         main_view, 4'b0011, 0,
                         main_view.atomic_std_data_flat[127:64],
                         {3'd0, main_view.fu_op_type[5:0]}, 7, decoded_slot),
                     "CAS.D compare/new STD payload swap was accepted");
        check_expect(!memblock_rm::observer_atomic_std_sample_matches(
                         main_view, 4'b0011, 1,
                         main_view.atomic_std_data_flat[63:0],
                         {3'd0, main_view.fu_op_type[5:0]}, 7, decoded_slot),
                     "atomic STD sample on unsupported pipe 1 was accepted");

        amo_w_ops = '{MEMBLOCK_LSUOP_AMOSWAP_W, MEMBLOCK_LSUOP_AMOADD_W,
                      MEMBLOCK_LSUOP_AMOXOR_W, MEMBLOCK_LSUOP_AMOAND_W,
                      MEMBLOCK_LSUOP_AMOOR_W, MEMBLOCK_LSUOP_AMOMIN_W,
                      MEMBLOCK_LSUOP_AMOMAX_W, MEMBLOCK_LSUOP_AMOMINU_W,
                      MEMBLOCK_LSUOP_AMOMAXU_W};
        amo_d_ops = '{MEMBLOCK_LSUOP_AMOSWAP_D, MEMBLOCK_LSUOP_AMOADD_D,
                      MEMBLOCK_LSUOP_AMOXOR_D, MEMBLOCK_LSUOP_AMOAND_D,
                      MEMBLOCK_LSUOP_AMOOR_D, MEMBLOCK_LSUOP_AMOMIN_D,
                      MEMBLOCK_LSUOP_AMOMAX_D, MEMBLOCK_LSUOP_AMOMINU_D,
                      MEMBLOCK_LSUOP_AMOMAXU_D};
        old_value = 128'h0;
        old_value[63:0] = 64'h8000_0000_8000_0003;
        operand = 128'h0;
        operand[63:0] = 64'h7fff_ffff_7fff_fffe;
        model = new();
        foreach (amo_w_ops[index]) begin
            expected = independent_amo_result(amo_w_ops[index], 4, old_value, operand);
            check_expect(model.compute_amo_new_value(
                             amo_w_ops[index], 4, old_value, operand) === expected,
                         $sformatf("AMO.W ALU mismatch op=0x%0h", amo_w_ops[index]));
        end
        foreach (amo_d_ops[index]) begin
            expected = independent_amo_result(amo_d_ops[index], 8, old_value, operand);
            check_expect(model.compute_amo_new_value(
                             amo_d_ops[index], 8, old_value, operand) === expected,
                         $sformatf("AMO.D ALU mismatch op=0x%0h", amo_d_ops[index]));
        end

        base_pa = 64'h8000_4000;
        model = new();
        check_expect(model.configure(1), "AMO commit fixture configure failed");
        item = make_item(0, MEMBLOCK_LSUOP_AMOADD_D, RM_ATOMIC_AMO, 8, base_pa);
        item.atomic_operand = operand;
        seed_memory(model, item, old_value);
        check_expect(model.add_item(item), "AMO commit fixture add failed");
        check_expect(model.push_atomic_actual(make_actual(item, old_value)),
                     "AMO commit actual push failed");
        check_expect(model.commit(item.rob), model.last_error_text);
        check_effect(model, item,
                     independent_amo_result(item.op, 8, old_value, operand));

        model = new();
        check_expect(model.configure(2), "LR/SC success configure failed");
        lr_item = make_item(0, MEMBLOCK_LSUOP_LR_D, RM_ATOMIC_LR, 8,
                            base_pa + 64'h100);
        sc_item = make_item(1, MEMBLOCK_LSUOP_SC_D, RM_ATOMIC_SC, 8,
                            base_pa + 64'h100);
        sc_item.atomic_std_data[0] = 64'h0123_4567_89ab_cdef;
        seed_memory(model, lr_item, old_value);
        check_expect(model.add_item(lr_item) && model.add_item(sc_item),
                     "LR/SC success item add failed");
        check_expect(model.push_atomic_actual(make_actual(lr_item, old_value)) &&
                     model.push_atomic_actual(make_actual(sc_item, '0)),
                     "LR/SC success actual push failed");
        check_expect(model.commit(lr_item.rob) && model.commit(sc_item.rob),
                     model.last_error_text);
        expected = '0;
        expected[63:0] = sc_item.atomic_std_data[0];
        check_effect(model, sc_item, expected);

        model = new();
        check_expect(model.configure(2), "LR/SC failure configure failed");
        lr_item = make_item(0, MEMBLOCK_LSUOP_LR_D, RM_ATOMIC_LR, 8,
                            base_pa + 64'h140);
        sc_item = make_item(1, MEMBLOCK_LSUOP_SC_D, RM_ATOMIC_SC, 8,
                            base_pa + 64'h140);
        seed_memory(model, lr_item, old_value);
        check_expect(model.add_item(lr_item) && model.add_item(sc_item),
                     "LR/SC failure item add failed");
        check_expect(model.push_atomic_actual(make_actual(lr_item, old_value)) &&
                     model.push_atomic_actual(make_actual(sc_item, 128'd1)),
                     "LR/SC failure actual push failed");
        check_expect(model.commit(lr_item.rob) && model.commit(sc_item.rob),
                     model.last_error_text);
        check_expect(model.expected_store_entry_count() == 0 &&
                     model.sc_no_write_obligation_count() == 1 &&
                     model.read_sc_no_write_obligation(0, obligation),
                     "SC failure no-write obligation missing");
        check_expect(obligation.expected_before_valid_mask[7:0] == 8'hff &&
                     obligation.expected_before_value[63:0] == old_value[63:0],
                     "SC failure did not freeze the pre-SC expected bytes");

        model = new();
        check_expect(model.configure(1), "standalone SC failure configure failed");
        sc_item = make_item(0, MEMBLOCK_LSUOP_SC_D, RM_ATOMIC_SC, 8,
                            base_pa + 64'h180);
        sc_item.atomic_std_data[0] = 64'h55aa_aa55_0123_4567;
        check_expect(model.add_item(sc_item),
                     "standalone SC failure item add failed");
        check_expect(model.push_atomic_actual(make_actual(sc_item, 128'd1)),
                     "standalone SC failure actual push failed");
        check_expect(model.commit(sc_item.rob), model.last_error_text);
        check_expect(model.expected_store_entry_count() == 0 &&
                     model.sc_no_write_obligation_count() == 1 &&
                     model.read_sc_no_write_obligation(0, obligation) &&
                     obligation.expected_before_valid_mask == '0,
                     "standalone SC failure incorrectly required backing or wrote memory");

        model = new();
        check_expect(model.configure(2), "SC no-write shadow configure failed");
        sc_item = make_item(0, MEMBLOCK_LSUOP_SC_D, RM_ATOMIC_SC, 8,
                            base_pa + 64'h1a0);
        effect_item = make_item(1, MEMBLOCK_LSUOP_AMOSWAP_D, RM_ATOMIC_AMO, 8,
                                base_pa + 64'h1a0);
        effect_item.atomic_operand[63:0] = 64'hdeed_beef_1020_3040;
        seed_memory(model, sc_item, old_value);
        check_expect(model.add_item(sc_item) && model.add_item(effect_item) &&
                     model.push_atomic_actual(make_actual(sc_item, 128'd1)) &&
                     model.push_atomic_actual(make_actual(effect_item, old_value)),
                     "SC no-write shadow setup failed");
        check_expect(model.commit(sc_item.rob) && model.commit(effect_item.rob),
                     model.last_error_text);
        check_expect(model.read_sc_no_write_obligation(0, obligation) &&
                     obligation.shadowed_mask[7:0] == 8'hff &&
                     obligation.checked_mask == '0,
                     "later atomic effect did not shadow the earlier SC no-write obligation");

        model = new();
        check_expect(model.configure(2), "SC bad-WB reservation configure failed");
        lr_item = make_item(0, MEMBLOCK_LSUOP_LR_D, RM_ATOMIC_LR, 8,
                            base_pa + 64'h1c0);
        sc_item = make_item(1, MEMBLOCK_LSUOP_SC_D, RM_ATOMIC_SC, 8,
                            base_pa + 64'h1c0);
        seed_memory(model, lr_item, old_value);
        check_expect(model.add_item(lr_item) && model.add_item(sc_item),
                     "SC bad-WB reservation item add failed");
        actual = make_actual(sc_item, '0);
        actual.pdest[0] ^= 8'h1;
        check_expect(model.push_atomic_actual(make_actual(lr_item, old_value)) &&
                     model.push_atomic_actual(actual),
                     "SC bad-WB reservation actual push failed");
        check_expect(model.commit(lr_item.rob), model.last_error_text);
        check_expect(!model.commit(sc_item.rob) &&
                     model.reservation_state == RM_RESV_NONE,
                     "terminal SC WB error did not clear reservation");

        model = new();
        check_expect(model.configure(2), "SC missing-WB reservation configure failed");
        lr_item = make_item(0, MEMBLOCK_LSUOP_LR_D, RM_ATOMIC_LR, 8,
                            base_pa + 64'h1e0);
        sc_item = make_item(1, MEMBLOCK_LSUOP_SC_D, RM_ATOMIC_SC, 8,
                            base_pa + 64'h1e0);
        seed_memory(model, lr_item, old_value);
        check_expect(model.add_item(lr_item) && model.add_item(sc_item) &&
                     model.push_atomic_actual(make_actual(lr_item, old_value)),
                     "SC missing-WB reservation setup failed");
        check_expect(model.commit(lr_item.rob), model.last_error_text);
        check_expect(model.consume_current_commit_error(
                         sc_item.rob, RM_LS_ERR_ATOMIC_WB_NOT_READY,
                         "directed missing SC WB") &&
                     model.reservation_state == RM_RESV_NONE &&
                     model.current_uid == 2,
                     "missing SC WB terminal error did not clear reservation");

        model = new();
        check_expect(model.configure(2), "fault SC missing-WB configure failed");
        lr_item = make_item(0, MEMBLOCK_LSUOP_LR_D, RM_ATOMIC_LR, 8,
                            base_pa + 64'h1f0);
        sc_item = make_item(1, MEMBLOCK_LSUOP_SC_D, RM_ATOMIC_SC, 8,
                            base_pa + 64'h1f0);
        sc_item.expected_exception[7] = 1'b1;
        seed_memory(model, lr_item, old_value);
        check_expect(model.add_item(lr_item) && model.add_item(sc_item) &&
                     model.push_atomic_actual(make_actual(lr_item, old_value)) &&
                     model.commit(lr_item.rob),
                     "fault SC missing-WB setup failed");
        check_expect(model.consume_current_commit_error(
                         sc_item.rob, RM_LS_ERR_ATOMIC_WB_NOT_READY,
                         "directed pre-DCache fault SC missing WB") &&
                     model.reservation_state == RM_RESV_POSSIBLE,
                     "pre-DCache fault SC missing WB consumed reservation");

        model = new();
        check_expect(model.configure(2), "denied SC missing-WB configure failed");
        lr_item = make_item(0, MEMBLOCK_LSUOP_LR_D, RM_ATOMIC_LR, 8,
                            base_pa + 64'h200);
        sc_item = make_item(1, MEMBLOCK_LSUOP_SC_D, RM_ATOMIC_SC, 8,
                            base_pa + 64'h200);
        sc_item.expected_exception[7] = 1'b1;
        sc_item.expected_dcache_denied = 1'b1;
        seed_memory(model, lr_item, old_value);
        check_expect(model.add_item(lr_item) && model.add_item(sc_item) &&
                     model.push_atomic_actual(make_actual(lr_item, old_value)) &&
                     model.commit(lr_item.rob),
                     "denied SC missing-WB setup failed");
        check_expect(model.consume_current_commit_error(
                         sc_item.rob, RM_LS_ERR_ATOMIC_WB_NOT_READY,
                         "directed denied SC missing WB") &&
                     model.reservation_state == RM_RESV_NONE,
                     "denied SC missing WB did not consume reservation");

        model = new();
        check_expect(model.configure(2), "corrupt SC missing-WB configure failed");
        lr_item = make_item(0, MEMBLOCK_LSUOP_LR_D, RM_ATOMIC_LR, 8,
                            base_pa + 64'h220);
        sc_item = make_item(1, MEMBLOCK_LSUOP_SC_D, RM_ATOMIC_SC, 8,
                            base_pa + 64'h220);
        sc_item.expected_exception[19] = 1'b1;
        sc_item.expected_dcache_corrupt = 1'b1;
        seed_memory(model, lr_item, old_value);
        check_expect(model.add_item(lr_item) && model.add_item(sc_item) &&
                     model.push_atomic_actual(make_actual(lr_item, old_value)) &&
                     model.commit(lr_item.rob),
                     "corrupt SC missing-WB setup failed");
        check_expect(model.consume_current_commit_error(
                         sc_item.rob, RM_LS_ERR_ATOMIC_WB_NOT_READY,
                         "directed corrupt SC missing WB") &&
                     model.reservation_state == RM_RESV_NONE,
                     "corrupt SC missing WB did not consume reservation");

        for (int unsigned size_index = 0; size_index < 3; size_index++) begin
            int unsigned size_bytes;
            bit [8:0] cas_op;
            size_bytes = size_index == 0 ? 4 : (size_index == 1 ? 8 : 16);
            cas_op = size_bytes == 4 ? MEMBLOCK_LSUOP_AMOCAS_W :
                     (size_bytes == 8 ? MEMBLOCK_LSUOP_AMOCAS_D :
                                        MEMBLOCK_LSUOP_AMOCAS_Q);
            for (int unsigned hit = 0; hit < 2; hit++) begin
                model = new();
                check_expect(model.configure(1), "CAS configure failed");
                item = make_item(0, cas_op, RM_ATOMIC_CAS, size_bytes,
                                 base_pa + 64'h200 + size_index * 64 + hit * 16);
                old_value = 128'h0123_4567_89ab_cdef_fedc_ba98_7654_3210;
                item.atomic_compare_value = hit ? old_value : ~old_value;
                item.atomic_new_value = 128'h0011_2233_4455_6677_8899_aabb_ccdd_eeff;
                seed_memory(model, item, old_value);
                check_expect(model.add_item(item), "CAS item add failed");
                if (size_bytes == 16)
                    actual = make_actual(item, old_value, 1'b1);
                else
                    actual = make_actual(item,
                        {{64{1'b0}}, model.format_atomic_wb(old_value, size_bytes)});
                check_expect(model.push_atomic_actual(actual), "CAS actual push failed");
                check_expect(model.commit(item.rob), model.last_error_text);
                check_expect(model.expected_store_entry_count() == (hit ? 1 : 0),
                             "CAS compare result produced wrong effect count");
                if (hit) check_effect(model, item, item.atomic_new_value);
            end
        end

        model = new();
        check_expect(model.configure(1), "CAS.Q missing-high-WB configure failed");
        item = make_item(0, MEMBLOCK_LSUOP_AMOCAS_Q, RM_ATOMIC_CAS, 16,
                         base_pa + 64'h380);
        old_value = 128'h0123_4567_89ab_cdef_fedc_ba98_7654_3210;
        item.atomic_compare_value = old_value;
        item.atomic_new_value = ~old_value;
        seed_memory(model, item, old_value);
        actual = make_actual(item, old_value);
        actual.wb_count = 1;
        check_expect(model.add_item(item) && model.push_atomic_actual(actual),
                     "CAS.Q missing-high-WB setup failed");
        check_expect(!model.commit(item.rob) &&
                     model.last_error == RM_LS_ERR_ATOMIC_WB_COUNT_MISMATCH,
                     "CAS.Q missing high writeback was accepted");

        model = new();
        check_expect(model.configure(1), "CAS.Q pdest-swap configure failed");
        item = make_item(0, MEMBLOCK_LSUOP_AMOCAS_Q, RM_ATOMIC_CAS, 16,
                         base_pa + 64'h3c0);
        item.atomic_compare_value = old_value;
        item.atomic_new_value = ~old_value;
        seed_memory(model, item, old_value);
        actual = make_actual(item, old_value);
        actual.pdest[0] = item.atomic_expected_pdest[1];
        actual.pdest[1] = item.atomic_expected_pdest[0];
        check_expect(model.add_item(item) && model.push_atomic_actual(actual),
                     "CAS.Q pdest-swap setup failed");
        check_expect(!model.commit(item.rob) &&
                     model.last_error == RM_LS_ERR_CAS_RESULT_MISMATCH,
                     "CAS.Q low/high pdest swap was accepted");

        model = new();
        check_expect(model.configure(1), "CAS.Q missing-pdest configure failed");
        item = make_item(0, MEMBLOCK_LSUOP_AMOCAS_Q, RM_ATOMIC_CAS, 16,
                         base_pa + 64'h400);
        item.atomic_compare_value = old_value;
        item.atomic_new_value = ~old_value;
        seed_memory(model, item, old_value);
        actual = make_actual(item, old_value);
        actual.pdest_valid[1] = 1'b0;
        check_expect(model.add_item(item) && model.push_atomic_actual(actual),
                     "CAS.Q missing-pdest setup failed");
        check_expect(!model.commit(item.rob) &&
                     model.last_error == RM_LS_ERR_CASQ_WB_PAIR_MISMATCH,
                     "CAS.Q missing high pdest was accepted");

        model = new();
        check_expect(model.configure(1), "fault AMO configure failed");
        item = make_item(0, MEMBLOCK_LSUOP_AMOADD_D, RM_ATOMIC_AMO, 8,
                         base_pa + 64'h400);
        item.expected_exception[7] = 1'b1;
        actual = make_actual(item, '0, 1'b0, item.expected_exception);
        check_expect(model.add_item(item) && model.push_atomic_actual(actual),
                     "fault AMO setup failed");
        check_expect(model.commit(item.rob, item.expected_exception),
                     model.last_error_text);
        check_expect(model.expected_store_entry_count() == 0,
                     "fault AMO produced a memory effect");
        `uvm_info("AMO_RM_SOFT",
                  "AMO ALU, LR/SC, CAS.W/D/Q, timeout, payload, WB identity and fault checks passed",
                  UVM_LOW)
    endtask:run
endclass:memblock_amo_rm_core_soft_checker

class soft_test_tc_lsq_amo_admission extends soft_test_tc_dispatch_smoke;
    `uvm_component_utils(soft_test_tc_lsq_amo_admission)

    function new(string name = "soft_test_tc_lsq_amo_admission",
                 uvm_component parent = null);
        super.new(name, parent);
    endfunction:new

    virtual task run_dispatch_smoke_sequence();
        soft_test_memblock_lsq_amo_admission_sequence seq;
        memblock_amo_rm_core_soft_checker rm_checker;

        seq = soft_test_memblock_lsq_amo_admission_sequence::type_id::create(
            "lsq_amo_admission_seq");
        if (seq == null) begin
            `uvm_fatal(get_type_name(), "failed to create LSQ/AMO admission soft sequence")
        end
        seq.start(null);
        rm_checker = memblock_amo_rm_core_soft_checker::type_id::create("rm_checker");
        if (rm_checker == null)
            `uvm_fatal(get_type_name(), "failed to create AMO RM core soft checker")
        rm_checker.run();
    endtask:run_dispatch_smoke_sequence
endclass:soft_test_tc_lsq_amo_admission

class tc_lsq_amo_admission_soft extends soft_test_tc_lsq_amo_admission;
    `uvm_component_utils(tc_lsq_amo_admission_soft)

    function new(string name = "tc_lsq_amo_admission_soft",
                 uvm_component parent = null);
        super.new(name, parent);
    endfunction:new
endclass:tc_lsq_amo_admission_soft

`endif
