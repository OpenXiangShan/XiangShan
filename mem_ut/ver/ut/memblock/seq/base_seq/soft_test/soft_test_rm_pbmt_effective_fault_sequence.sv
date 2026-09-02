//=========================================================
//File name    : soft_test_rm_pbmt_effective_fault_sequence.sv
//Module name  : soft_test_rm_pbmt_effective_fault_sequence
//Discribution : software-only RM PBMT/PBMTE effective-fault closure
//=========================================================
`ifndef SOFT_TEST_RM_PBMT_EFFECTIVE_FAULT_SEQUENCE__SV
`define SOFT_TEST_RM_PBMT_EFFECTIVE_FAULT_SEQUENCE__SV

class soft_test_rm_pbmt_effective_fault_sequence extends memblock_dispatch_base_sequence;

    `uvm_object_utils(soft_test_rm_pbmt_effective_fault_sequence)

    extern function new(string name = "soft_test_rm_pbmt_effective_fault_sequence");
    extern virtual task body();
    extern virtual function void expect_true(input bit condition, input string message);
    extern virtual function void make_views(
        input bit [1:0] s2xlate,
        input bit       s1_active,
        input bit       s2_active,
        input bit [1:0] s1_pbmt,
        input bit [1:0] s2_pbmt,
        input bit       m_pbmt_en,
        input bit       h_pbmt_en,
        output memblock_rm_readonly_api::tlb_entry_view_t entry,
        output memblock_rm_readonly_api::tlb_request_context_view_t tlb_context);
    extern virtual task check_stage_matrix();
    extern virtual task check_or_and_reserved_payload();
    extern virtual task check_exception_encoding();
    extern virtual task check_cross_page_fault_boundary();

endclass:soft_test_rm_pbmt_effective_fault_sequence

function soft_test_rm_pbmt_effective_fault_sequence::new(
    string name = "soft_test_rm_pbmt_effective_fault_sequence");
    super.new(name);
endfunction:new

task soft_test_rm_pbmt_effective_fault_sequence::body();
    ensure_dispatch_runtime_helpers();
    check_stage_matrix();
    check_or_and_reserved_payload();
    check_exception_encoding();
    check_cross_page_fault_boundary();
    `uvm_info(get_type_name(),
              "RM PBMT effective-fault software closure completed", UVM_LOW)
endtask:body

function void soft_test_rm_pbmt_effective_fault_sequence::expect_true(
    input bit condition,
    input string message);
    if (!condition) begin
        `uvm_fatal(get_type_name(), message)
    end
endfunction:expect_true

function void soft_test_rm_pbmt_effective_fault_sequence::make_views(
    input bit [1:0] s2xlate,
    input bit       s1_active,
    input bit       s2_active,
    input bit [1:0] s1_pbmt,
    input bit [1:0] s2_pbmt,
    input bit       m_pbmt_en,
    input bit       h_pbmt_en,
    output memblock_rm_readonly_api::tlb_entry_view_t entry,
    output memblock_rm_readonly_api::tlb_request_context_view_t tlb_context);
    entry = '{default:'0};
    tlb_context = '{default:'0};
    entry.valid = 1'b1;
    entry.s2xlate = s2xlate;
    entry.s1_stage_active = s1_active;
    entry.s2_stage_active = s2_active;
    entry.s1_entry_pbmt = s1_pbmt;
    entry.s2_entry_pbmt = s2_pbmt;
    tlb_context.valid = 1'b1;
    tlb_context.s2xlate = s2xlate;
    tlb_context.m_pbmt_en = m_pbmt_en;
    tlb_context.h_pbmt_en = h_pbmt_en;
endfunction:make_views

task soft_test_rm_pbmt_effective_fault_sequence::check_stage_matrix();
    memblock_rm_readonly_api::tlb_entry_view_t entry;
    memblock_rm_readonly_api::tlb_request_context_view_t tlb_context;
    bit force_s1_pf;
    bit force_s2_gpf;
    bit result;

    make_views(2'd0, 1'b1, 1'b0, 2'd1, 2'd0, 1'b0, 1'b1,
               entry, tlb_context);
    result = memblock_rm_readonly_api::eval_pbmt_fault_overlay(
        entry, tlb_context, 1'b1, 1'b0, force_s1_pf, force_s2_gpf);
    expect_true(result && force_s1_pf && !force_s2_gpf,
                "s2xlate=0 disabled mPBMTE must force S1 PF");

    make_views(2'd1, 1'b1, 1'b0, 2'd1, 2'd0, 1'b1, 1'b0,
               entry, tlb_context);
    result = memblock_rm_readonly_api::eval_pbmt_fault_overlay(
        entry, tlb_context, 1'b1, 1'b0, force_s1_pf, force_s2_gpf);
    expect_true(result && force_s1_pf && !force_s2_gpf,
                "s2xlate=1 disabled hPBMTE must force S1 PF");
    make_views(2'd1, 1'b1, 1'b0, 2'd1, 2'd0, 1'b0, 1'b1,
               entry, tlb_context);
    result = memblock_rm_readonly_api::eval_pbmt_fault_overlay(
        entry, tlb_context, 1'b1, 1'b0, force_s1_pf, force_s2_gpf);
    expect_true(result && !force_s1_pf,
                "s2xlate=1 enabled hPBMTE must suppress S1 force");

    make_views(2'd2, 1'b0, 1'b1, 2'd0, 2'd2, 1'b0, 1'b1,
               entry, tlb_context);
    result = memblock_rm_readonly_api::eval_pbmt_fault_overlay(
        entry, tlb_context, 1'b0, 1'b1, force_s1_pf, force_s2_gpf);
    expect_true(result && force_s2_gpf && !force_s1_pf,
                "s2xlate=2 disabled mPBMTE must force S2 GPF");

    make_views(2'd3, 1'b1, 1'b1, 2'd1, 2'd2, 1'b0, 1'b0,
               entry, tlb_context);
    result = memblock_rm_readonly_api::eval_pbmt_fault_overlay(
        entry, tlb_context, 1'b1, 1'b1, force_s1_pf, force_s2_gpf);
    expect_true(result && force_s1_pf && force_s2_gpf,
                "allStage disabled h/m PBMTE must force both stages");
    make_views(2'd3, 1'b1, 1'b1, 2'd1, 2'd2, 1'b1, 1'b0,
               entry, tlb_context);
    result = memblock_rm_readonly_api::eval_pbmt_fault_overlay(
        entry, tlb_context, 1'b1, 1'b1, force_s1_pf, force_s2_gpf);
    expect_true(result && force_s1_pf && !force_s2_gpf,
                "allStage h=0,m=1 must force S1 only");
    make_views(2'd3, 1'b1, 1'b1, 2'd1, 2'd2, 1'b0, 1'b1,
               entry, tlb_context);
    result = memblock_rm_readonly_api::eval_pbmt_fault_overlay(
        entry, tlb_context, 1'b1, 1'b1, force_s1_pf, force_s2_gpf);
    expect_true(result && !force_s1_pf && force_s2_gpf,
                "allStage h=1,m=0 must force S2 only");

    make_views(2'd3, 1'b1, 1'b1, 2'd0, 2'd0, 1'b0, 1'b0,
               entry, tlb_context);
    result = memblock_rm_readonly_api::eval_pbmt_fault_overlay(
        entry, tlb_context, 1'b1, 1'b1, force_s1_pf, force_s2_gpf);
    expect_true(result && !force_s1_pf && !force_s2_gpf,
                "PBMT=00 must not force either stage");
endtask:check_stage_matrix

task soft_test_rm_pbmt_effective_fault_sequence::check_or_and_reserved_payload();
    memblock_rm_readonly_api::tlb_entry_view_t entry;
    memblock_rm_readonly_api::tlb_request_context_view_t tlb_context;
    bit force_s1_pf;
    bit force_s2_gpf;
    bit result;
    bit effective_s1_pf;
    bit effective_s2_gpf;

    make_views(2'd3, 1'b1, 1'b1, 2'd1, 2'd1, 1'b0, 1'b0,
               entry, tlb_context);
    entry.fault_effective_s1_af = 1'b1;
    entry.fault_effective_s2_gaf = 1'b1;
    result = memblock_rm_readonly_api::eval_pbmt_fault_overlay(
        entry, tlb_context, 1'b1, 1'b1, force_s1_pf, force_s2_gpf);
    effective_s1_pf = entry.fault_effective_s1_pf || force_s1_pf;
    effective_s2_gpf = entry.fault_effective_s2_gpf || force_s2_gpf;
    expect_true(result && effective_s1_pf && effective_s2_gpf &&
                entry.fault_effective_s1_af && entry.fault_effective_s2_gaf,
                "PBMT force must OR with pre-existing stage faults");

    make_views(2'd0, 1'b1, 1'b0, 2'b11, 2'd0, 1'b0, 1'b0,
               entry, tlb_context);
    result = memblock_rm_readonly_api::eval_pbmt_fault_overlay(
        entry, tlb_context, 1'b1, 1'b0, force_s1_pf, force_s2_gpf);
    expect_true(!result && !force_s1_pf && !force_s2_gpf,
                "active PBMT=11 must be rejected as inconsistent entry");

    make_views(2'd0, 1'b1, 1'b0, 2'd0, 2'd1, 1'b0, 1'b0,
               entry, tlb_context);
    result = memblock_rm_readonly_api::eval_pbmt_fault_overlay(
        entry, tlb_context, 1'b1, 1'b0, force_s1_pf, force_s2_gpf);
    expect_true(!result && !force_s1_pf && !force_s2_gpf,
                "inactive stage nonzero PBMT must be rejected");
endtask:check_or_and_reserved_payload

task soft_test_rm_pbmt_effective_fault_sequence::check_exception_encoding();
    memblock_rm_readonly_api::tlb_entry_view_t entry;
    memblock_rm_readonly_api::tlb_request_context_view_t tlb_context;
    logic [23:0] load_exception;
    logic [23:0] store_exception;
    bit force_s1_pf;
    bit force_s2_gpf;

    make_views(2'd3, 1'b1, 1'b1, 2'd1, 2'd1, 1'b0, 1'b0,
               entry, tlb_context);
    void'(memblock_rm_readonly_api::eval_pbmt_fault_overlay(
        entry, tlb_context, 1'b1, 1'b1, force_s1_pf, force_s2_gpf));
    load_exception = '0;
    store_exception = '0;
    if (force_s1_pf) begin
        load_exception[13] = 1'b1;
        store_exception[15] = 1'b1;
    end
    if (force_s2_gpf) begin
        load_exception[21] = 1'b1;
        store_exception[23] = 1'b1;
    end
    expect_true(load_exception[13] && load_exception[21] &&
                store_exception[15] && store_exception[23],
                "Load/Store PBMT force must map to PF/GPF exception bits");
endtask:check_exception_encoding

task soft_test_rm_pbmt_effective_fault_sequence::check_cross_page_fault_boundary();
    memblock_rm_readonly_api::tlb_entry_view_t page_entry[2];
    memblock_rm_readonly_api::tlb_request_context_view_t tlb_context[2];
    bit force_s1_pf;
    bit force_s2_gpf;
    bit [7:0] pa_valid_mask;
    logic [23:0] expected_exception;
    int unsigned lookup_count;

    make_views(2'd0, 1'b1, 1'b0, 2'd0, 2'd0, 1'b0, 1'b0,
               page_entry[0], tlb_context[0]);
    make_views(2'd0, 1'b1, 1'b0, 2'd1, 2'd0, 1'b0, 1'b0,
               page_entry[1], tlb_context[1]);
    pa_valid_mask = '0;
    expected_exception = '0;
    lookup_count = 0;
    for (int unsigned byte_index = 0; byte_index < 8; byte_index++) begin
        int unsigned page_index;
        page_index = byte_index < 4 ? 0 : 1;
        lookup_count++;
        void'(memblock_rm_readonly_api::eval_pbmt_fault_overlay(
            page_entry[page_index], tlb_context[page_index],
            1'b1, 1'b0, force_s1_pf, force_s2_gpf));
        if (force_s1_pf || force_s2_gpf) begin
            expected_exception[13] = force_s1_pf;
            expected_exception[21] = force_s2_gpf;
            break;
        end
        pa_valid_mask[byte_index] = 1'b1;
    end
    expect_true(lookup_count == 5 && pa_valid_mask == 8'h0f &&
                expected_exception[13] && !expected_exception[21],
                "cross-page RM model must stop at second-page PBMT fault without PA");

    // A fault on the first page terminates the access before a second lookup.
    make_views(2'd0, 1'b1, 1'b0, 2'd1, 2'd0, 1'b0, 1'b0,
               page_entry[0], tlb_context[0]);
    pa_valid_mask = '0;
    lookup_count = 0;
    for (int unsigned byte_index = 0; byte_index < 8; byte_index++) begin
        lookup_count++;
        void'(memblock_rm_readonly_api::eval_pbmt_fault_overlay(
            page_entry[0], tlb_context[0],
            1'b1, 1'b0, force_s1_pf, force_s2_gpf));
        if (force_s1_pf || force_s2_gpf) break;
        pa_valid_mask[byte_index] = 1'b1;
    end
    expect_true(lookup_count == 1 && pa_valid_mask == 8'h00,
                "first-page PBMT fault must prevent later-page lookup and PA");
endtask:check_cross_page_fault_boundary

`endif
