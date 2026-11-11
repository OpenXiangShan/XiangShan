// 中文注释：软件定向验证首次行冻结和真实逐 beat 事实，不驱动 DUT、不预测指令异常。
`ifndef SOFT_TEST_L2_LINE_HISTORY_SEQUENCE__SV
`define SOFT_TEST_L2_LINE_HISTORY_SEQUENCE__SV
class soft_test_l2_line_history_sequence extends dcache_mem__access_base_sequence;
    `uvm_object_utils(soft_test_l2_line_history_sequence)
    function new(string name = "soft_test_l2_line_history_sequence");
        super.new(name);
    endfunction
    function void expect_true(input bit condition, input string message);
        if (!condition) `uvm_fatal(get_type_name(), message)
    endfunction
    // 抽象职责：仅在软件测试里构造 A.fire 输入，正式响应构造仍调用公共框架入口。
    function longint unsigned reserve_case(input bit [47:0] address,
        input bit [4:0] req_source, input bit [5:0] source, input bit [3:0] opcode = 4'd6,
        input bit keyword = 0);
        dcache_agent_agent_xaction request;
        request = dcache_agent_agent_xaction::type_id::create("line_history_test_a");
        request.auto_inner_dcache_client_out_a_bits_address = address;
        request.auto_inner_dcache_client_out_a_bits_user_reqSource = req_source;
        request.auto_inner_dcache_client_out_a_bits_source = source;
        request.auto_inner_dcache_client_out_a_bits_opcode = opcode;
        request.auto_inner_dcache_client_out_a_bits_echo_isKeyword = keyword;
        return reserve_l2_a_history(request);
    endfunction
    function l2_response_history_t read_case(input longint unsigned token,
        input bit [47:0] address, input bit [1:0] mask, input bit expected_available);
        l2_response_history_t record;
        bit available;
        expect_true(query_l2_response_history(token, address,
            current_l2_d_error_sample("soft line query")+100, mask, record, available), "token query failed");
        expect_true(record.valid && available == expected_available, "unexpected beat availability");
        return record;
    endfunction
    function void finish_case(input longint unsigned token, input bit two_beats);
        longint unsigned sample;
        sample = current_l2_d_error_sample("soft D.fire")+1;
        publish_l2_response_beat_history(token, 0, sample);
        if (two_beats) publish_l2_response_beat_history(token, 1, sample+1);
        complete_l2_response_history(token);
    endfunction
    virtual task body();
        int unsigned saved_denied, saved_corrupt, saved_cbo_denied, saved_cbo_corrupt;
        longint unsigned token, old_token, found_token;
        longint unsigned sample;
        l2_response_history_t record;
        l2_d_error_line_record_t choice;
        l2_response_owner_t owner;
        memblock_rm_readonly_api::l2_d_error_view_t view;
        bit needs_candidate;
        bit available;
        seq_csr_common::init();
        // 中文注释：软件fixture仍使用真实DUT采样锚点；main_phase可早于首份CSR，不伪造sample。
        wait(memblock_sync_pkg::reset_backend_done === 1'b1);
        wait(memblock_sync_pkg::dut_sample_seq > 0 && !memblock_sync_pkg::l2tlb_reset_active());

        saved_denied = seq_csr_common::l2_grantdata_denied_wt;
        saved_corrupt = seq_csr_common::l2_grantdata_corrupt_wt;
        saved_cbo_denied = seq_csr_common::l2_cbo_ack_denied_wt;
        saved_cbo_corrupt = seq_csr_common::l2_cbo_ack_corrupt_wt;
        reset_l2_line_history();
        seq_csr_common::l2_grantdata_denied_wt = 100;
        seq_csr_common::l2_grantdata_corrupt_wt = 100;
        sample = current_l2_d_error_sample("soft line start");

        token = reserve_case(48'h80001000, MEMBLOCK_REQ_CPU_STORE, 0, 6, 1);
        record = read_case(token, 48'h80001000, 1, 0);
        expect_true(!record.injection_allowed && !record.denied && !record.corrupt,
            "Store origin was injected at weight100");
        publish_l2_response_beat_history(token, 0, sample+1);
        record = read_case(token, 48'h80001000, 1, 1);
        expect_true(record.beat0.byte_mask == 64'hffff_ffff_0000_0000 && !record.full_complete,
            "keyword first beat physical coverage is wrong");
        expect_true(query_l2_response_history(token, 48'h80001000, sample, 1, record, available) &&
            !available, "future first beat leaked into older sample");
        record = read_case(token, 48'h80001000, 2, 0);
        publish_l2_response_beat_history(token, 1, sample+9);
        complete_l2_response_history(token);
        record = read_case(token, 48'h80001000, 3, 1);
        expect_true(record.full_complete && record.beat1.byte_mask == 64'h0000_0000_ffff_ffff,
            "second physical half or completion lost");
        expect_true(query_l2_line_history_at_sample(48'h80001000, sample, choice) &&
            choice.valid && choice.state == L2_D_ERROR_NONE && choice.generation != 0,
            "Store first CLEAN was not frozen immediately");

        token = reserve_case(48'h80001040, MEMBLOCK_REQ_CPU_LOAD, 1);
        record = read_case(token, 48'h80001040, 1, 0);
        expect_true(record.denied && record.corrupt && record.injection_allowed,
            "Load denied100 did not produce legal denied/corrupt");
        old_token = token;
        finish_case(token, 1);
        token = reserve_case(48'h80001040, MEMBLOCK_REQ_CPU_STORE, 2);
        record = read_case(token, 48'h80001040, 1, 0);
        expect_true(!record.denied && !record.corrupt && !record.injection_allowed,
            "existing erroneous choice bypassed Store exclusion");
        expect_true(query_l2_line_history_at_sample(48'h80001040, sample, choice) &&
            choice.state == L2_D_ERROR_DENIED, "Store overwrote immutable erroneous choice");
        finish_case(token, 1);
        token = reserve_case(48'h80001000, MEMBLOCK_REQ_CPU_LOAD, 3);
        record = read_case(token, 48'h80001000, 1, 0);
        expect_true(!record.denied && !record.corrupt, "CLEAN line was re-randomized");
        finish_case(token, 1);

        seq_csr_common::l2_grantdata_denied_wt = 0;
        seq_csr_common::l2_grantdata_corrupt_wt = 100;
        token = reserve_case(48'h80001080, MEMBLOCK_REQ_CPU_ATOMIC, 4);
        record = read_case(token, 48'h80001080, 1, 0);
        expect_true(!record.denied && record.corrupt, "Atomic corrupt100 selection failed");
        finish_case(token, 1);
        token = reserve_case(48'h800010c0, MEMBLOCK_REQ_L1_PREFETCH, 5);
        record = read_case(token, 48'h800010c0, 1, 0);
        expect_true(!record.denied && record.corrupt, "prefetch injection capability disappeared");
        finish_case(token, 1);

        seq_csr_common::l2_cbo_ack_denied_wt = 100;
        seq_csr_common::l2_cbo_ack_corrupt_wt = 100;
        token = reserve_case(48'h80001100, MEMBLOCK_REQ_CPU_STORE, 17, TL_A_OPCODE_CBO_CLEAN);
        record = read_case(token, 48'h80001100, 1, 0);
        expect_true(record.denied && !record.corrupt && record.kind == DCACHE_PENDING_D_CBO_ACK,
            "CBOAck denied priority encoding failed");
        owner = '0;
        owner.valid = 1;
        owner.uid = 7;
        owner.dynamic_epoch = 3;
        expect_true(!bind_l2_response_owner(token, sample+1, owner), "owner accepted wrong A sample");
        expect_true(find_l2_response_at_accept(sample, 17, 0, found_token) && found_token == token,
            "exact A.accept index did not find CBO token");
        expect_true(bind_l2_response_owner(token, sample, owner), "CBO exact owner binding failed");
        finish_case(token, 0);
        token = reserve_case(48'h80001100, MEMBLOCK_REQ_CPU_LOAD, 6);
        record = read_case(token, 48'h80001100, 1, 0);
        expect_true(record.denied && record.corrupt, "cross-kind frozen DENIED was lost");
        finish_case(token, 1);

        register_clean_l2_access(48'h80001140, sample, 7, DCACHE_PENDING_D_RELEASE_ACK);
        token = begin_l2_response_history(48'h80001140, sample, 7, 1,
            DCACHE_PENDING_D_RELEASE_ACK, 7, MEMBLOCK_REQ_CPU_STORE, 0, 0, 0, 0);
        finish_case(token, 0);
        token = reserve_case(48'h80001140, MEMBLOCK_REQ_CPU_LOAD, 8);
        record = read_case(token, 48'h80001140, 1, 0);
        expect_true(!record.denied && !record.corrupt, "Release first CLEAN allowed later randomization");
        finish_case(token, 1);

        expect_true(memblock_rm_readonly_api::get().read_l2_d_error_for_rm(48'h80001040, sample+100, view) &&
            view.line_seen && !view.consumption_proven && !view.denied && !view.corrupt,
            "PA-only query leaked permanent error oracle");
        abort_l2_response_pending();
        expect_true(!query_l2_response_history(old_token, 48'h80001040, sample+100, 1, record, available),
            "old reset response authorized new access");
        token = reserve_case(48'h80001040, MEMBLOCK_REQ_CPU_STORE, 1);
        record = read_case(token, 48'h80001040, 1, 0);
        expect_true(record.source_generation == 2 && record.reset_epoch == 1 && !record.denied && !record.corrupt,
            "reset lost generation isolation or Store exclusion");
        finish_case(token, 1);
        expect_true(query_l2_line_history_at_sample(48'h80001040, sample, choice) &&
            choice.state == L2_D_ERROR_DENIED, "DUT reset erased first line choice");
        reset_l2_line_history();
        expect_true(query_l2_line_history_at_sample(48'h80001040, sample, choice) && !choice.valid,
            "new testcase did not clear history");
        seq_csr_common::l2_grantdata_denied_wt = saved_denied;
        seq_csr_common::l2_grantdata_corrupt_wt = saved_corrupt;
        seq_csr_common::l2_cbo_ack_denied_wt = saved_cbo_denied;
        seq_csr_common::l2_cbo_ack_corrupt_wt = saved_cbo_corrupt;
        `uvm_info(get_type_name(), "L2 first-line and per-beat directed checks PASS", UVM_LOW)
    endtask
endclass
`endif
