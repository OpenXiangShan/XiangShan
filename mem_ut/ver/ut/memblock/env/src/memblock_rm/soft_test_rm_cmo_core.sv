`ifndef SOFT_TEST_RM_CMO_CORE_SV
`define SOFT_TEST_RM_CMO_CORE_SV

// 抽象职责：直接验证RM核心的CMO唯一提交、部分效果与SC覆盖；不模拟请求调度。
// 编译于拥有rm_ls_core类型的package，thin testcase只需构造对象并调用run()。
class rm_cmo_core_soft_test_t;
    int unsigned checks;

    function new(); checks = 0; endfunction

    function void expect_true(input bit condition, input string message);
        checks++;
        if (!condition) begin
            `uvm_fatal("RM_CMO_CORE_SOFT", message)
        end
    endfunction

    function rm_ls_program_item_t make_item(input int unsigned uid,
                                            input bit [8:0] op,
                                            input rm_cmo_path_e path,
                                            input longint unsigned pa = 64'h1008);
        rm_ls_program_item_t item;
        item = new(uid);
        item.valid = 1;
        item.op = op;
        item.computed_vaddr = pa; item.main_vaddr = pa;
        item.rob.flag = 0; item.rob.value = 16'(uid);
        item.sq_flag = 0; item.sq_value = MEMBLOCK_SQ_VALUE_W'(uid);
        item.dynamic_epoch = 1;
        item.translation_valid = 1;
        item.pa_valid_mask = 16'h0001; item.pa_by_byte[0] = pa;
        expect_true(rm_ls_assign_cmo_data_semantics(item), "CMO decoder must recognize all four final operations");
        item.cmo_path = path;
        item.uncache_path = path == RM_CMO_PATH_UNCACHE_ZERO;
        item.cmo_requests_validated = 1;
        item.cmo_request_count = 1;
        item.cmo_request_token = uid + 1;
        item.cmo_request_watermark = 100;
        return item;
    endfunction

    function rm_cmo_completion_actual_t make_actual(input rm_ls_program_item_t item);
        rm_cmo_completion_actual_t actual;
        actual = new();
        actual.valid = 1; actual.uid = item.uid; actual.rob = item.rob;
        actual.sq_flag = item.sq_flag; actual.sq_value = item.sq_value;
        actual.dynamic_epoch = item.dynamic_epoch; actual.fu_op_type = item.op;
        actual.source_sta0 = 1; actual.is_final = 1;
        actual.flush_pipe = item.cmo_expect_flush_pipe;
        actual.exception_vec = item.expected_exception;
        actual.sample_seq = 20;
        actual.normal_pass = item.expected_exception == '0;
        actual.fault_terminal = item.expected_exception != '0;
        return actual;
    endfunction

    function void check_line_effect_and_shadow();
        rm_ls_model_t model;
        rm_ls_program_item_t item;
        rm_cmo_completion_actual_t actual;
        rm_ls_store_cache_entry_t entry;
        rm_sc_no_write_obligation_t obligation;
        byte unsigned value;
        model = new();
        expect_true(model.configure(1), "configure cacheable zero model");
        for (int unsigned i = 0; i < 66; i++)
            expect_true(model.set_backing_byte(64'h0fff + i, 8'h5a), "initialize zero line and adjacent bytes");
        obligation = new();
        obligation.valid = 1; obligation.size_bytes = 8;
        obligation.byte_mask = 16'h00ff;
        for (int unsigned i = 0; i < 8; i++) obligation.pa_by_byte[i] = 64'h1004 + i;
        model.sc_no_write_obligations.push_back(obligation);
        item = make_item(0, MEMBLOCK_LSUOP_CBO_ZERO, RM_CMO_PATH_CACHEABLE_ZERO);
        expect_true(item.cmo_permission_write && item.cmo_has_data_effect &&
                    !item.cmo_expect_flush_pipe && item.store_data == '0 && item.size_bytes == 0,
                    "zero semantics must use write permission and canonical zero data");
        expect_true(model.add_item(item), "add cacheable zero item");
        actual = make_actual(item);
        expect_true(model.push_cmo_actual(actual), "capture final zero STA0");
        expect_true(model.push_cmo_actual(actual), "same immutable completion poll must be idempotent");
        expect_true(model.commit(item.rob), "normal cacheable zero commit");
        expect_true(model.current_uid == 1 && model.consumed_commit_count == 1 &&
                    model.expected_store_entry_count() == 8, "zero must commit 64B as eight entries exactly once");
        for (int unsigned i = 0; i < 64; i++)
            expect_true(model.read_expected_memory_byte(64'h1000 + i, value) && value == 0,
                        "every expected zero line byte must be zero");
        expect_true(model.read_expected_memory_byte(64'h0fff, value) && value == 8'h5a,
                    "zero must preserve byte before line");
        expect_true(model.read_expected_memory_byte(64'h1040, value) && value == 8'h5a,
                    "zero must preserve byte after line");
        expect_true(obligation.shadowed_mask == 16'h00ff, "zero must shadow earlier failed SC bytes");
        for (int unsigned i = 0; i < 8; i++) begin
            expect_true(model.read_expected_store_entry(i, entry), "zero entry must be valid");
            expect_true(entry.size_bytes == 8 && entry.byte_mask == 16'h00ff &&
                        entry.source_kind == RM_MEMORY_EFFECT_CMO_ZERO &&
                        entry.pa_by_byte[0] == 64'h1000 + i * 8, "zero entry geometry/source");
        end
        expect_true(!model.rm_ls_commit_cmo(item, '0) &&
                    model.last_error == RM_LS_ERR_CMO_ACTUAL_NOT_READY &&
                    model.expected_store_entry_count() == 8, "consumed zero cannot apply effects again");
        expect_true(!model.rm_ls_validate_cmo_owner(item, actual) &&
                    model.last_error == RM_LS_ERR_CMO_DUPLICATE_COMPLETION,
                    "consumed owner must reject duplicate completion");
    endfunction

    function void check_uncache_partial(input bit [7:0] write_mask,
                                        input int unsigned expected_bytes,
                                        input bit [7:0] sent_mask = 8'h07,
                                        input bit response_fault = 1'b1);
        rm_ls_model_t model;
        rm_ls_program_item_t item;
        rm_cmo_completion_actual_t actual;
        rm_sc_no_write_obligation_t obligation;
        byte unsigned value;
        model = new();
        expect_true(model.configure(1), "configure partial zero model");
        for (int unsigned i = 0; i < 64; i++)
            expect_true(model.set_backing_byte(64'h1000 + i, 8'ha6), "initialize partial zero backing");
        obligation = new();
        obligation.valid = 1; obligation.size_bytes = 16; obligation.byte_mask = 16'hffff;
        for (int unsigned i = 0; i < 16; i++) obligation.pa_by_byte[i] = 64'h1010 + i;
        model.sc_no_write_obligations.push_back(obligation);
        item = make_item(0, MEMBLOCK_LSUOP_CBO_ZERO, RM_CMO_PATH_UNCACHE_ZERO);
        // 中文注释：此处只构造统一错误入口已经验证的结果，不复制denied映射算法。
        item.expected_exception[7] = response_fault;
        item.cmo_request_count = $countones(sent_mask);
        item.cmo_effect_valid = 1;
        item.cmo_segment_sent_mask = sent_mask;
        item.cmo_segment_completed_mask = sent_mask;
        item.cmo_segment_write_mask = write_mask;
        expect_true(model.add_item(item), "add partial zero");
        actual = make_actual(item);
        expect_true(model.push_cmo_actual(actual), "capture partial fault STA0");
        expect_true(model.commit(item.rob, item.expected_exception), "fault zero must commit verified partial effect");
        expect_true(actual.normal_pass != response_fault && item.cmo_completion_consumed &&
                    model.expected_store_entry_count() == expected_bytes / 8,
                    "partial fault must not become normal or insert unaccepted entries");
        for (int unsigned i = 0; i < 64; i++)
            expect_true(model.read_expected_memory_byte(64'h1000 + i, value) &&
                        value == (i < expected_bytes ? 8'h00 : 8'ha6), "partial zero byte effect");
        expect_true(obligation.shadowed_mask == (expected_bytes >= 32 ? 16'hffff :
                                                 expected_bytes == 24 ? 16'h00ff : 16'h0000),
                    "partial zero shadows only accepted failed-SC bytes");
    endfunction

    function void check_nonzero_and_fault();
        bit [8:0] ops[3];
        rm_ls_model_t model;
        rm_ls_program_item_t item;
        rm_cmo_completion_actual_t actual;
        byte unsigned value;
        ops[0] = MEMBLOCK_LSUOP_CBO_CLEAN;
        ops[1] = MEMBLOCK_LSUOP_CBO_FLUSH;
        ops[2] = MEMBLOCK_LSUOP_CBO_INVAL;
        foreach (ops[i]) begin
            model = new();
            expect_true(model.configure(1), "configure non-zero model");
            expect_true(model.set_backing_byte(64'h1008, 8'hd3), "non-zero baseline");
            item = make_item(0, ops[i], RM_CMO_PATH_DCACHE_CMO);
            expect_true(!item.cmo_permission_write && !item.cmo_has_data_effect &&
                        item.cmo_expect_flush_pipe, "non-zero must use read permission without data effect");
            expect_true(model.add_item(item), "add non-zero item");
            actual = make_actual(item);
            expect_true(model.push_cmo_actual(actual) && model.commit(item.rob), "non-zero normal commit");
            expect_true(model.expected_store_entry_count() == 0 &&
                        model.read_expected_memory_byte(64'h1008, value) && value == 8'hd3,
                        "non-zero must preserve all expected memory");
        end
        model = new(); expect_true(model.configure(1), "configure pre-access fault");
        item = make_item(0, MEMBLOCK_LSUOP_CBO_ZERO, RM_CMO_PATH_NONE);
        item.expected_exception[15] = 1;
        item.cmo_pre_access_exception = item.expected_exception;
        item.pa_valid_mask = '0; item.translation_valid = 0;
        item.cmo_request_count = 0; item.cmo_request_token = 0;
        expect_true(model.add_item(item), "add no-request pre-access fault");
        actual = make_actual(item);
        // fault不套用成功flushPipe约束。
        actual.flush_pipe = 1;
        expect_true(model.push_cmo_actual(actual) && model.commit(item.rob, item.expected_exception),
                    "pre-access fault must complete without requests or valid data PA");
        expect_true(model.expected_store_entry_count() == 0 && !item.cmo_effect_applied,
                    "pre-access fault has no zero effect");
    endfunction

    function void check_mmio_effect(input bit backend_accepted);
        rm_ls_model_t model;
        rm_ls_program_item_t item;
        rm_sc_no_write_obligation_t obligation;
        byte unsigned value;
        model = new(); expect_true(model.configure(1), "configure MMIO effect");
        item = new(0); item.valid = 1; item.kind = RM_LS_KIND_STORE;
        item.rob = '{default:'0}; item.computed_vaddr = 64'h2000; item.main_vaddr = 64'h2000;
        item.translation_valid = 1; item.size_bytes = 8;
        item.pa_valid_mask = 16'h00ff; item.store_byte_mask = 16'h00ff;
        item.store_data = 128'h1122_3344_5566_7788;
        item.expected_exception[7] = 1; item.uncache_path = 1;
        item.uncache_effect_valid = 1; item.uncache_write_accepted = backend_accepted;
        obligation = new(); obligation.valid = 1; obligation.size_bytes = 8; obligation.byte_mask = 16'h00ff;
        for (int unsigned i = 0; i < 8; i++) begin
            item.pa_by_byte[i] = 64'h2000 + i;
            obligation.pa_by_byte[i] = item.pa_by_byte[i];
            expect_true(model.set_backing_byte(item.pa_by_byte[i], 8'hbe), "MMIO baseline");
        end
        model.sc_no_write_obligations.push_back(obligation);
        expect_true(model.add_item(item), "add MMIO fault effect item");
        expect_true(model.commit(item.rob, item.expected_exception), "MMIO fault must preserve deterministic backend decision");
        expect_true(model.expected_store_entry_count() == int'(backend_accepted), "MMIO accepted effect exactly once");
        for (int unsigned i = 0; i < 8; i++)
            expect_true(model.read_expected_memory_byte(item.pa_by_byte[i], value) &&
                        value == (backend_accepted ? item.store_data[(i * 8) +: 8] : 8'hbe),
                        "MMIO accepted fault writes, backend refusal preserves bytes");
        expect_true(obligation.shadowed_mask == (backend_accepted ? 16'h00ff : 16'h0000),
                    "MMIO effect shadows only actually accepted bytes");
        expect_true(!model.rm_ls_apply_uncache_write_effect(item), "MMIO effect cannot apply twice");
    endfunction

    function void check_negative_evidence();
        rm_ls_model_t model;
        rm_ls_program_item_t item;
        rm_cmo_completion_actual_t actual;
        model = new(); expect_true(model.configure(1), "configure missing request negative");
        item = make_item(0, MEMBLOCK_LSUOP_CBO_ZERO, RM_CMO_PATH_CACHEABLE_ZERO);
        item.cmo_requests_validated = 0;
        expect_true(model.add_item(item), "add missing request negative");
        actual = make_actual(item); expect_true(model.push_cmo_actual(actual), "capture missing request completion");
        expect_true(!model.commit(item.rob) && model.last_error == RM_LS_ERR_CMO_REQUEST_MISMATCH &&
                    model.expected_store_entry_count() == 0 && !item.cmo_effect_applied,
                    "normal STA0 cannot hide missing actual request evidence");
        model = new(); expect_true(model.configure(1), "configure wrong flush negative");
        item = make_item(0, MEMBLOCK_LSUOP_CBO_ZERO, RM_CMO_PATH_CACHEABLE_ZERO);
        expect_true(model.add_item(item), "add wrong flush negative");
        actual = make_actual(item); actual.flush_pipe = 1;
        expect_true(model.push_cmo_actual(actual), "capture wrong flush completion");
        expect_true(!model.commit(item.rob) && model.last_error == RM_LS_ERR_CMO_COMPLETION_MISMATCH &&
                    model.expected_store_entry_count() == 0, "wrong flushPipe must not write zero effects");
        model = new(); expect_true(model.configure(1), "configure stale owner negative");
        item = make_item(0, MEMBLOCK_LSUOP_CBO_ZERO, RM_CMO_PATH_CACHEABLE_ZERO);
        expect_true(model.add_item(item), "add stale owner negative");
        actual = make_actual(item); actual.dynamic_epoch = 0;
        expect_true(!model.push_cmo_actual(actual) && model.last_error == RM_LS_ERR_CMO_STALE_COMPLETION &&
                    model.current_uid == 0 && model.expected_store_entry_count() == 0,
                    "stale completion must not advance or apply effects");
        actual.dynamic_epoch = item.dynamic_epoch;
        expect_true(model.push_cmo_actual(actual), "capture first final completion");
        actual.sample_seq++;
        expect_true(!model.push_cmo_actual(actual) && model.last_error == RM_LS_ERR_CMO_DUPLICATE_COMPLETION &&
                    model.current_uid == 0 && model.expected_store_entry_count() == 0,
                    "real second completion must fail without effects");
    endfunction

    function void run();
        check_line_effect_and_shadow();
        check_uncache_partial(8'h03, 16);
        check_uncache_partial(8'h07, 24);
        check_uncache_partial(8'h00, 0, 8'h01);
        check_uncache_partial(8'hff, 64, 8'hff);
        check_uncache_partial(8'hff, 64, 8'hff, 1'b0);
        check_nonzero_and_fault();
        check_mmio_effect(1'b0);
        check_mmio_effect(1'b1);
        check_negative_evidence();
        `uvm_info("RM_CMO_CORE_SOFT", $sformatf("CMO core checks passed: %0d", checks), UVM_LOW)
    endfunction
endclass

`endif
