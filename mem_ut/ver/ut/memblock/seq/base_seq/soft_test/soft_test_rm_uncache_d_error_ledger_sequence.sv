//=========================================================
//File name    : soft_test_rm_uncache_d_error_ledger_sequence.sv
//Module name  : soft_test_rm_uncache_d_error_ledger_sequence
//Discribution : Software-only checks for RM Uncache D-error provenance
//Date         : 2026-10-15
//=========================================================
`ifndef SOFT_TEST_RM_UNCACHE_D_ERROR_LEDGER_SEQUENCE__SV
`define SOFT_TEST_RM_UNCACHE_D_ERROR_LEDGER_SEQUENCE__SV

class soft_test_rm_uncache_d_error_ledger_sequence extends uvm_sequence;
    `uvm_object_utils(soft_test_rm_uncache_d_error_ledger_sequence)

    extern function new(string name = "soft_test_rm_uncache_d_error_ledger_sequence");
    extern task body();
    extern function void expect_true(input bit condition, input string message);
    extern function mem_access_base_sequence::uncache_d_error_owner_t make_owner(
        input memblock_uid_t uid,
        input int unsigned epoch,
        input bit rob_flag,
        input bit [MEMBLOCK_ROB_VALUE_W-1:0] rob_value,
        input bit lq_flag,
        input bit [MEMBLOCK_LQ_VALUE_W-1:0] lq_value,
        input bit [3:0] slot
    );
    extern task run_response_case(
        input bit [3:0] source,
        input memblock_uid_t uid,
        input int unsigned epoch,
        input bit [47:0] address,
        input bit denied,
        input bit corrupt,
        input longint unsigned a_sample,
        input longint unsigned d_sample,
        output longint unsigned generation
    );
    extern task check_error_matrix();
    extern task check_pending_and_sample_boundary();
    extern task check_source_reuse_and_same_address();
    extern task check_redirect_old_epoch();
    extern task check_multi_owner_nc_response();
    extern task check_l2_uncache_isolation();
    extern task check_load_expected_case(
        input string case_name,
        input bit [8:0] op,
        input bit fp_wen,
        input int unsigned size_bytes,
        input bit [63:0] raw_data,
        input bit [63:0] expected_data
    );
    extern task check_fp_load_nan_boxing();
endclass:soft_test_rm_uncache_d_error_ledger_sequence

function soft_test_rm_uncache_d_error_ledger_sequence::new(string name =
    "soft_test_rm_uncache_d_error_ledger_sequence");
    super.new(name);
endfunction:new

function void soft_test_rm_uncache_d_error_ledger_sequence::expect_true(
    input bit condition,
    input string message
);
    if (!condition) begin
        `uvm_fatal(get_type_name(), message)
    end
endfunction:expect_true

function mem_access_base_sequence::uncache_d_error_owner_t
soft_test_rm_uncache_d_error_ledger_sequence::make_owner(
    input memblock_uid_t uid,
    input int unsigned epoch,
    input bit rob_flag,
    input bit [MEMBLOCK_ROB_VALUE_W-1:0] rob_value,
    input bit lq_flag,
    input bit [MEMBLOCK_LQ_VALUE_W-1:0] lq_value,
    input bit [3:0] slot
);
    mem_access_base_sequence::uncache_d_error_owner_t owner;
    owner = '{valid:1'b1, uid:uid, dynamic_epoch:epoch,
              rob_flag:rob_flag, rob_value:rob_value,
              lq_flag:lq_flag, lq_value:lq_value,
              uncache_lq_slot:slot};
    return owner;
endfunction:make_owner

task soft_test_rm_uncache_d_error_ledger_sequence::run_response_case(
    input bit [3:0] source,
    input memblock_uid_t uid,
    input int unsigned epoch,
    input bit [47:0] address,
    input bit denied,
    input bit corrupt,
    input longint unsigned a_sample,
    input longint unsigned d_sample,
    output longint unsigned generation
);
    mem_access_base_sequence::uncache_d_error_owner_t owners[
        mem_access_base_sequence::UNCACHE_D_ERROR_MAX_OWNERS];
    mem_access_base_sequence::uncache_d_error_record_t record;

    foreach (owners[idx]) owners[idx] = '{default:'0};
    owners[0] = make_owner(uid, epoch, uid[0],
                           MEMBLOCK_ROB_VALUE_W'(uid), uid[0],
                           MEMBLOCK_LQ_VALUE_W'(uid), source);
    expect_true(mem_access_base_sequence::begin_uncache_d_error_record(
                    1'b1, source, address, 3'd2, a_sample, 1, owners,
                    generation),
                $sformatf("begin response failed uid=%0d", uid));
    record = '{default:'0};
    record.valid = 1'b1;
    record.is_load = 1'b1;
    record.source = source;
    record.generation = generation;
    record.address = address;
    record.size = 3'd2;
    record.a_sample = a_sample;
    record.denied = denied;
    record.corrupt = corrupt;
    record.owner_count = 1;
    record.owners[0] = owners[0];
    mem_access_base_sequence::commit_uncache_d_error_record(record, d_sample);
endtask:run_response_case

task soft_test_rm_uncache_d_error_ledger_sequence::check_error_matrix();
    memblock_rm_readonly_api ro;
    memblock_rm_readonly_api::uncache_d_error_view_t view;
    longint unsigned generation;

    ro = memblock_rm_readonly_api::get();
    for (int unsigned kind = 0; kind < 4; kind++) begin
        run_response_case(kind[3:0], 100 + kind, 1,
                          48'h1000 + kind * 16,
                          kind[1], kind[0],
                          10 + kind * 2, 11 + kind * 2, generation);
        expect_true(ro.read_uncache_d_error_for_rm(
                        100 + kind, 1, (100 + kind) & 1,
                        MEMBLOCK_ROB_VALUE_W'(100 + kind),
                        (100 + kind) & 1,
                        MEMBLOCK_LQ_VALUE_W'(100 + kind),
                        48'h1000 + kind * 16, 4, 11 + kind * 2, view),
                    "matrix query failed");
        expect_true(view.found && view.denied == kind[1] &&
                    view.corrupt == kind[0],
                    $sformatf("matrix mismatch kind=%0d found/denied/corrupt=%0d/%0d/%0d",
                              kind, view.found, view.denied, view.corrupt));
    end
endtask:check_error_matrix

task soft_test_rm_uncache_d_error_ledger_sequence::check_pending_and_sample_boundary();
    mem_access_base_sequence::uncache_d_error_owner_t owners[
        mem_access_base_sequence::UNCACHE_D_ERROR_MAX_OWNERS];
    mem_access_base_sequence::uncache_d_error_record_t record;
    memblock_rm_readonly_api::uncache_d_error_view_t view;
    memblock_rm_readonly_api ro;
    longint unsigned generation;

    foreach (owners[idx]) owners[idx] = '{default:'0};
    owners[0] = make_owner(200, 3, 1'b0, 16'd20, 1'b0, 7'd20, 4);
    expect_true(mem_access_base_sequence::begin_uncache_d_error_record(
                    1'b1, 4, 48'h2000, 2, 100, 1, owners, generation),
                "pending begin failed");
    ro = memblock_rm_readonly_api::get();
    expect_true(ro.read_uncache_d_error_for_rm(
                    200, 3, 0, 16'd20, 0, 7'd20,
                    48'h2000, 4, 100, view) && view.not_ready && !view.found,
                "pending query was not reported as not-ready");
    record = '{default:'0};
    record.valid = 1'b1; record.is_load = 1'b1;
    record.source = 4; record.generation = generation;
    record.address = 48'h2000; record.size = 2; record.a_sample = 100;
    record.owner_count = 1; record.owners[0] = owners[0];
    mem_access_base_sequence::commit_uncache_d_error_record(record, 101);
    expect_true(ro.read_uncache_d_error_for_rm(
                    200, 3, 0, 16'd20, 0, 7'd20,
                    48'h2000, 4, 100, view) && view.not_ready,
                "future D.fire leaked into earlier query sample");
    expect_true(ro.read_uncache_d_error_for_rm(
                    200, 3, 0, 16'd20, 0, 7'd20,
                    48'h2000, 4, 101, view) && view.found,
                "D.fire did not become visible at completion sample");
endtask:check_pending_and_sample_boundary

task soft_test_rm_uncache_d_error_ledger_sequence::check_source_reuse_and_same_address();
    memblock_rm_readonly_api ro;
    memblock_rm_readonly_api::uncache_d_error_view_t old_view;
    memblock_rm_readonly_api::uncache_d_error_view_t new_view;
    longint unsigned old_generation;
    longint unsigned new_generation;

    ro = memblock_rm_readonly_api::get();
    run_response_case(5, 300, 1, 48'h3000, 1'b1, 1'b1,
                      200, 201, old_generation);
    run_response_case(5, 301, 1, 48'h3000, 1'b0, 1'b0,
                      202, 203, new_generation);
    expect_true(new_generation == old_generation + 1,
                "source generation did not advance on reuse");
    expect_true(ro.read_uncache_d_error_for_rm(
                    300, 1, 0, MEMBLOCK_ROB_VALUE_W'(300), 0,
                    MEMBLOCK_LQ_VALUE_W'(300), 48'h3000, 4, 203, old_view) &&
                old_view.found && old_view.denied,
                "old source generation was overwritten");
    expect_true(ro.read_uncache_d_error_for_rm(
                    301, 1, 1, MEMBLOCK_ROB_VALUE_W'(301), 1,
                    MEMBLOCK_LQ_VALUE_W'(301), 48'h3000, 4, 203, new_view) &&
                new_view.found && !new_view.denied && !new_view.corrupt,
                "same-address owner selected another UID history");
endtask:check_source_reuse_and_same_address

task soft_test_rm_uncache_d_error_ledger_sequence::check_redirect_old_epoch();
    mem_access_base_sequence::uncache_d_error_owner_t owners[
        mem_access_base_sequence::UNCACHE_D_ERROR_MAX_OWNERS];
    mem_access_base_sequence::uncache_d_error_record_t record;
    memblock_rm_readonly_api ro;
    memblock_rm_readonly_api::uncache_d_error_view_t view;
    longint unsigned old_generation;
    longint unsigned new_generation;

    foreach (owners[idx]) owners[idx] = '{default:'0};
    owners[0] = make_owner(400, 7, 0, 16'd40, 0, 7'd40, 6);
    expect_true(mem_access_base_sequence::begin_uncache_d_error_record(
                    1, 6, 48'h4000, 2, 300, 1, owners, old_generation),
                "old epoch begin failed");
    owners[0] = make_owner(400, 8, 1, 16'd41, 1, 7'd41, 7);
    expect_true(mem_access_base_sequence::begin_uncache_d_error_record(
                    1, 7, 48'h4000, 2, 301, 1, owners, new_generation),
                "new epoch begin failed");
    record = '{default:'0};
    record.valid = 1; record.is_load = 1; record.source = 6;
    record.generation = old_generation; record.address = 48'h4000;
    record.size = 2; record.a_sample = 300; record.denied = 1;
    record.corrupt = 1; record.owner_count = 1;
    record.owners[0] = make_owner(400, 7, 0, 16'd40, 0, 7'd40, 6);
    mem_access_base_sequence::commit_uncache_d_error_record(record, 302);
    ro = memblock_rm_readonly_api::get();
    expect_true(ro.read_uncache_d_error_for_rm(
                    400, 7, 0, 16'd40, 0, 7'd40,
                    48'h4000, 4, 302, view) && view.found && view.denied,
                "late old-epoch response was not retained for old owner");
    expect_true(ro.read_uncache_d_error_for_rm(
                    400, 8, 1, 16'd41, 1, 7'd41,
                    48'h4000, 4, 302, view) && view.not_ready && !view.found,
                "old-epoch response contaminated new epoch");
    record = '{default:'0};
    record.valid = 1; record.is_load = 1; record.source = 7;
    record.generation = new_generation; record.address = 48'h4000;
    record.size = 2; record.a_sample = 301; record.owner_count = 1;
    record.owners[0] = make_owner(400, 8, 1, 16'd41, 1, 7'd41, 7);
    mem_access_base_sequence::commit_uncache_d_error_record(record, 303);
endtask:check_redirect_old_epoch

task soft_test_rm_uncache_d_error_ledger_sequence::check_multi_owner_nc_response();
    mem_access_base_sequence::uncache_d_error_owner_t owners[
        mem_access_base_sequence::UNCACHE_D_ERROR_MAX_OWNERS];
    mem_access_base_sequence::uncache_d_error_record_t record;
    memblock_rm_readonly_api::uncache_d_error_view_t view;
    memblock_rm_readonly_api ro;
    longint unsigned generation;

    foreach (owners[idx]) owners[idx] = '{default:'0};
    owners[0] = make_owner(600, 11, 1'b0, 16'd60, 1'b0, 7'd60, 8);
    owners[1] = make_owner(601, 12, 1'b0, 16'd61, 1'b0, 7'd61, 9);
    expect_true(mem_access_base_sequence::begin_uncache_d_error_record(
                    1'b1, 8, 48'h6000, 3'd3, 500, 2, owners, generation),
                "multi-owner NC begin failed");
    record = '{default:'0};
    record.valid = 1'b1; record.is_load = 1'b1;
    record.source = 8; record.generation = generation;
    record.address = 48'h6000; record.size = 3'd3;
    record.a_sample = 500; record.denied = 1'b1;
    record.corrupt = 1'b1; record.owner_count = 2;
    record.owners[0] = owners[0]; record.owners[1] = owners[1];
    mem_access_base_sequence::commit_uncache_d_error_record(record, 501);

    ro = memblock_rm_readonly_api::get();
    expect_true(ro.read_uncache_d_error_for_rm(
                    600, 11, 0, 16'd60, 0, 7'd60,
                    48'h6000, 8, 501, view) && view.found && view.denied &&
                view.source == 8 && view.generation == generation &&
                view.a_sample == 500 && view.d_sample == 501,
                "first NC merged owner did not resolve frozen response");
    expect_true(ro.read_uncache_d_error_for_rm(
                    601, 12, 0, 16'd61, 0, 7'd61,
                    48'h6000, 8, 501, view) && view.found && view.denied &&
                view.source == 8 && view.generation == generation,
                "second NC merged owner did not resolve frozen response");
    expect_true(ro.read_uncache_d_error_for_rm(
                    601, 12, 0, 16'd61, 0, 7'd62,
                    48'h6000, 8, 501, view) && !view.found &&
                view.identity_mismatch,
                "NC merged owner query accepted mismatched LQ identity");
endtask:check_multi_owner_nc_response

task soft_test_rm_uncache_d_error_ledger_sequence::check_l2_uncache_isolation();
    memblock_rm_readonly_api ro;
    memblock_rm_readonly_api::l2_d_error_view_t l2_view;
    memblock_rm_readonly_api::uncache_d_error_view_t uncache_view;

    mem_access_base_sequence::reset_l2_d_error_ledger(1'b1);
    mem_access_base_sequence::commit_l2_d_error_on_grant_d_fire(
        48'h5000, 1'b0, 1'b1, 400, 10'd1, 0);
    ro = memblock_rm_readonly_api::get();
    expect_true(ro.read_l2_d_error_for_rm(48'h5000, 400, l2_view) &&
                l2_view.valid && l2_view.corrupt,
                "cacheable L2 ledger query failed");
    expect_true(ro.read_uncache_d_error_for_rm(
                    500, 1, 0, 16'd50, 0, 7'd50,
                    48'h5000, 4, 400, uncache_view) &&
                !uncache_view.found && !uncache_view.not_ready,
                "L2 error leaked into Uncache ledger");
endtask:check_l2_uncache_isolation

task soft_test_rm_uncache_d_error_ledger_sequence::check_load_expected_case(
    input string case_name,
    input bit [8:0] op,
    input bit fp_wen,
    input int unsigned size_bytes,
    input bit [63:0] raw_data,
    input bit [63:0] expected_data
);
    bit [63:0] actual_expected;
    bit is_signed;

    is_signed = op == MEMBLOCK_LSUOP_LB ||
                op == MEMBLOCK_LSUOP_LH ||
                op == MEMBLOCK_LSUOP_LW ||
                op == MEMBLOCK_LSUOP_LD;
    actual_expected = memblock_rm_format_load_data(
        raw_data, op, fp_wen, is_signed, size_bytes);
    expect_true(actual_expected == expected_data,
                $sformatf("%s expected 0x%016h got 0x%016h",
                          case_name, expected_data, actual_expected));
endtask:check_load_expected_case

task soft_test_rm_uncache_d_error_ledger_sequence::check_fp_load_nan_boxing();
    check_load_expected_case("FP FLH positive NaN-box", MEMBLOCK_LSUOP_LH,
                             1'b1, 2, 64'h0000000000007ce7,
                             64'hffffffffffff7ce7);
    check_load_expected_case("INT LH positive sign extension", MEMBLOCK_LSUOP_LH,
                             1'b0, 2, 64'h0000000000007ce7,
                             64'h0000000000007ce7);
    check_load_expected_case("FP FLW positive NaN-box", MEMBLOCK_LSUOP_LW,
                             1'b1, 4, 64'h00000000671c0f03,
                             64'hffffffff671c0f03);
    check_load_expected_case("INT LW positive sign extension", MEMBLOCK_LSUOP_LW,
                             1'b0, 4, 64'h00000000671c0f03,
                             64'h00000000671c0f03);
    check_load_expected_case("FP FLD full width", MEMBLOCK_LSUOP_LD,
                             1'b1, 8, 64'h0123456789abcdef,
                             64'h0123456789abcdef);
endtask:check_fp_load_nan_boxing

task soft_test_rm_uncache_d_error_ledger_sequence::body();
    mem_access_base_sequence::reset_uncache_d_error_ledger();
    check_error_matrix();
    check_pending_and_sample_boundary();
    check_source_reuse_and_same_address();
    check_redirect_old_epoch();
    check_multi_owner_nc_response();
    check_l2_uncache_isolation();
    check_fp_load_nan_boxing();
    `uvm_info(get_type_name(),
              "RM Uncache D-error ledger and FP load NaN-box directed checks passed",
              UVM_LOW)
endtask:body

`endif
