//=========================================================
//File name    : soft_test_tc_rm_pbmt_effective_fault.sv
//Module name  : soft_test_tc_rm_pbmt_effective_fault
//Discribution : software-only RM PBMT/PBMTE effective-fault test
//=========================================================
`ifndef SOFT_TEST_TC_RM_PBMT_EFFECTIVE_FAULT__SV
`define SOFT_TEST_TC_RM_PBMT_EFFECTIVE_FAULT__SV

class memblock_rm_expected_error_catcher extends uvm_report_catcher;
    string expected_id;
    string expected_message_pattern;
    int unsigned caught_count;

    function new(string name = "memblock_rm_expected_error_catcher",
                 string id = "",
                 string message_pattern = "*");
        super.new(name);
        expected_id = id;
        expected_message_pattern = message_pattern;
        caught_count = 0;
    endfunction:new

    virtual function action_e catch();
        if (get_severity() == UVM_ERROR && get_id() == expected_id &&
            uvm_pkg::uvm_is_match(expected_message_pattern, get_message())) begin
            caught_count++;
            return CAUGHT;
        end
        return THROW;
    endfunction:catch
endclass:memblock_rm_expected_error_catcher

class soft_test_tc_rm_pbmt_effective_fault extends soft_test_tc_dispatch_smoke;

    `uvm_component_utils(soft_test_tc_rm_pbmt_effective_fault)

    function new(string name = "soft_test_tc_rm_pbmt_effective_fault",
                 uvm_component parent = null);
        super.new(name, parent);
    endfunction:new

    extern function void init_load_views(
        input memblock_uid_t uid,
        input bit [63:0] vaddr,
        output memblock_rm_readonly_api::main_transaction_view_t main_view,
        output memblock_rm_readonly_api::status_view_t status_view);
    extern function void init_store_views(
        input memblock_uid_t uid,
        input bit [63:0] vaddr,
        output memblock_rm_readonly_api::main_transaction_view_t main_view,
        output memblock_rm_readonly_api::status_view_t status_view);
    extern function memblock_uid_tlb_record make_uid_tlb_record(
        input memblock_uid_t uid,
        input bit [63:0] vaddr,
        input bit [1:0] s2xlate,
        input bit [1:0] effective_pbmt,
        input bit permission_read,
        input bit completed);
    extern function void publish_secondary_tlb_entry(
        input memblock_uid_tlb_record record,
        input bit [63:0] vaddr,
        input bit [1:0] effective_pbmt);
    extern task check_rm_production_commit_path();

    virtual task run_dispatch_smoke_sequence();
        soft_test_rm_pbmt_effective_fault_sequence pbmt_seq;
        memblock_rm_readonly_api::tlb_entry_view_t entry;
        memblock_rm_readonly_api::tlb_request_context_view_t tlb_context;
        bit force_s1_pf;
        bit force_s2_gpf;

        pbmt_seq = soft_test_rm_pbmt_effective_fault_sequence::type_id::create(
            "rm_pbmt_effective_fault_seq");
        if (pbmt_seq == null) begin
            `uvm_fatal(get_type_name(),
                       "failed to create soft_test_rm_pbmt_effective_fault_sequence")
        end
        pbmt_seq.start(null);

        // Call the actual RM component wrapper once as a smoke guard; the
        // sequence above exercises the same pure readonly implementation.
        if (env == null || env.rm == null) begin
            `uvm_fatal(get_type_name(), "RM component is unavailable for PBMT helper smoke")
        end
        entry = '{default:'0};
        tlb_context = '{default:'0};
        entry.valid = 1'b1;
        entry.s2xlate = 2'd0;
        entry.s1_stage_active = 1'b1;
        entry.s1_entry_pbmt = 2'd1;
        tlb_context.valid = 1'b1;
        tlb_context.s2xlate = 2'd0;
        tlb_context.m_pbmt_en = 1'b0;
        tlb_context.h_pbmt_en = 1'b1;
        if (!env.rm.observer_eval_pbmt_fault_overlay(
                entry, tlb_context, 1'b1, 1'b0,
                force_s1_pf, force_s2_gpf) ||
            !force_s1_pf || force_s2_gpf) begin
            `uvm_fatal(get_type_name(),
                       "RM component did not apply disabled mPBMTE S1 PF overlay")
        end
        check_rm_production_commit_path();
    endtask:run_dispatch_smoke_sequence

endclass:soft_test_tc_rm_pbmt_effective_fault

class tc_rm_pbmt_effective_fault extends soft_test_tc_rm_pbmt_effective_fault;

    `uvm_component_utils(tc_rm_pbmt_effective_fault)

    function new(string name = "tc_rm_pbmt_effective_fault",
                 uvm_component parent = null);
        super.new(name, parent);
    endfunction:new

endclass:tc_rm_pbmt_effective_fault

function void soft_test_tc_rm_pbmt_effective_fault::init_load_views(
    input memblock_uid_t uid,
    input bit [63:0] vaddr,
    output memblock_rm_readonly_api::main_transaction_view_t main_view,
    output memblock_rm_readonly_api::status_view_t status_view);
    main_view = '{default:'0};
    status_view = '{default:'0};
    main_view.valid = 1'b1;
    main_view.uid = uid;
    main_view.op_class = MEMBLOCK_OP_CLASS_INT_LOAD;
    main_view.fu_type = MEMBLOCK_FUTYPE_LDU;
    main_view.fu_op_type = MEMBLOCK_LSUOP_LD;
    main_view.src_0 = vaddr;
    main_view.vaddr = vaddr;
    main_view.rob_idx_value = uid[MEMBLOCK_ROB_VALUE_W-1:0];
    main_view.lq_idx_value = uid[MEMBLOCK_LQ_VALUE_W-1:0];
    status_view.valid = 1'b1;
    status_view.uid = uid;
    status_view.rob_idx_value = main_view.rob_idx_value;
    status_view.lq_idx_value = main_view.lq_idx_value;
    status_view.dynamic_epoch = 1;
endfunction:init_load_views

function void soft_test_tc_rm_pbmt_effective_fault::init_store_views(
    input memblock_uid_t uid,
    input bit [63:0] vaddr,
    output memblock_rm_readonly_api::main_transaction_view_t main_view,
    output memblock_rm_readonly_api::status_view_t status_view);
    init_load_views(uid, vaddr, main_view, status_view);
    main_view.op_class = MEMBLOCK_OP_CLASS_STORE;
    main_view.fu_type = MEMBLOCK_FUTYPE_STU;
    main_view.fu_op_type = MEMBLOCK_LSUOP_SD;
    main_view.sq_idx_value = uid[MEMBLOCK_SQ_VALUE_W-1:0];
    status_view.sq_idx_value = main_view.sq_idx_value;
endfunction:init_store_views

function memblock_uid_tlb_record
    soft_test_tc_rm_pbmt_effective_fault::make_uid_tlb_record(
        input memblock_uid_t uid,
        input bit [63:0] vaddr,
        input bit [1:0] s2xlate,
        input bit [1:0] effective_pbmt,
        input bit permission_read,
        input bit completed);
    common_data_transaction data;
    mmu_csr_runtime_state csr;
    memblock_uid_tlb_record record;
    memblock_tlb_entry entry;
    bit [51:0] vpn;

    data = common_data_transaction::get();
    csr = mmu_csr_runtime_state::type_id::create(
        $sformatf("rm_production_csr_%0d", uid));
    csr.reset();
    csr.priv_dmode = 2'd1;
    csr.m_pbmt_en = 1'b1;
    csr.h_pbmt_en = 1'b1;
    csr.update_seq = uid + 1;
    case (s2xlate)
        2'd0: begin
            csr.satp_mode = 4'd8;
            csr.satp_asid = 16'h100 + uid;
            csr.satp_ppn = 44'h12345;
        end
        2'd2: begin
            csr.priv_virt = 1'b1;
            csr.hgatp_mode = 4'd8;
            csr.hgatp_vmid = 16'h200 + uid;
            csr.hgatp_ppn = 44'h23456;
        end
        default: begin
            `uvm_fatal(get_type_name(), "directed RM record supports only S1 or S2-only")
        end
    endcase

    vpn = {14'b0, vaddr[49:12]};
    record = memblock_uid_tlb_record::type_id::create(
        $sformatf("rm_production_record_%0d", uid));
    record.init_context(uid, vpn, s2xlate, 1'b0, csr, 100 + uid);
    record.mark_request_fire(200 + uid);
    if (completed) begin
        entry = data.build_tlb_entry_for_key_with_csr(record.lookup_key, csr);
        entry.fault_raw_s1_pf = 1'b0;
        entry.fault_raw_s1_af = 1'b0;
        entry.fault_raw_s2_gpf = 1'b0;
        entry.fault_raw_s2_gaf = 1'b0;
        entry.fault_effective_s1_pf = 1'b0;
        entry.fault_effective_s1_af = 1'b0;
        entry.fault_effective_s2_gpf = 1'b0;
        entry.fault_effective_s2_gaf = 1'b0;
        entry.fault_stage_selected =
            memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_NONE;
        entry.pmaAF = 1'b0;
        if (s2xlate == 2'd0) begin
            entry.s1_pte_mode_at_build =
                memblock_tlb_entry::MEMBLOCK_TLB_PTE_MODE_LEGAL;
            entry.s1_pte_v = 1'b1;
            entry.s1_pte_r = permission_read;
            entry.s1_pte_w = permission_read;
            entry.s1_pte_x = 1'b0;
            entry.s1_pte_u = 1'b0;
            entry.s1_pte_a = 1'b1;
            entry.s1_pte_d = 1'b1;
            entry.s1_entry_pbmt = effective_pbmt;
            record.request_s1_resolved_ppn = 44'h34567 + uid;
        end else begin
            entry.s2_pte_mode_at_build =
                memblock_tlb_entry::MEMBLOCK_TLB_PTE_MODE_LEGAL;
            entry.s2_pte_r = permission_read;
            entry.s2_pte_w = permission_read;
            entry.s2_pte_x = 1'b0;
            entry.s2_pte_u = 1'b0;
            entry.s2_pte_a = 1'b1;
            entry.s2_pte_d = 1'b1;
            entry.s2_entry_pbmt = effective_pbmt;
            record.request_s2_resolved_ppn = 44'h45678 + uid;
        end
        record.copy_entry_fields(entry);
        record.request_derived_valid = 1'b1;
        record.request_gvpn = vpn;
        record.mark_completed();
    end
    data.uid_tlb_record_by_uid[uid] = record;
    return record;
endfunction:make_uid_tlb_record

function void soft_test_tc_rm_pbmt_effective_fault::publish_secondary_tlb_entry(
    input memblock_uid_tlb_record record,
    input bit [63:0] vaddr,
    input bit [1:0] effective_pbmt);
    common_data_transaction data;
    memblock_tlb_lookup_key_t key;
    memblock_tlb_entry entry;

    if (record == null || record.csr_snapshot == null || record.s2xlate != 2'd0) begin
        `uvm_fatal(get_type_name(), "secondary PBMT helper requires an S1 record")
    end
    data = common_data_transaction::get();
    key = record.csr_snapshot.make_lookup_key({14'b0, vaddr[49:12]}, 2'd0);
    entry = data.build_tlb_entry_for_key_with_csr(key, record.csr_snapshot);
    entry.fault_raw_s1_pf = 1'b0;
    entry.fault_raw_s1_af = 1'b0;
    entry.fault_effective_s1_pf = 1'b0;
    entry.fault_effective_s1_af = 1'b0;
    entry.fault_stage_selected = memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_NONE;
    entry.pmaAF = 1'b0;
    entry.s1_pte_mode_at_build = memblock_tlb_entry::MEMBLOCK_TLB_PTE_MODE_LEGAL;
    entry.s1_pte_v = 1'b1;
    entry.s1_pte_r = 1'b1;
    entry.s1_pte_w = 1'b1;
    entry.s1_pte_x = 1'b0;
    entry.s1_pte_u = 1'b0;
    entry.s1_pte_a = 1'b1;
    entry.s1_pte_d = 1'b1;
    entry.s1_entry_pbmt = effective_pbmt;
    data.publish_live_tlb_entry(key, entry, record.csr_snapshot);
endfunction:publish_secondary_tlb_entry

task soft_test_tc_rm_pbmt_effective_fault::check_rm_production_commit_path();
    common_data_transaction data;
    memblock_rm_readonly_api::main_transaction_view_t main_view;
    memblock_rm_readonly_api::status_view_t status_view;
    memblock_uid_tlb_record record;
    memblock_tlb_entry entry;
    rm_ls_program_item_t item;
    memblock_rm_expected_error_catcher catcher;
    pma_pmp_eval_t pma_eval;
    bit saved_pma_pmp_model_en;
    bit [47:0] permission_fault_mmio_pa;

    if (env == null || env.rm == null) begin
        `uvm_fatal(get_type_name(), "RM component is unavailable for production-path checks")
    end
    data = common_data_transaction::get();
    saved_pma_pmp_model_en = seq_csr_common::pma_pmp_model_en;
    seq_csr_common::pma_pmp_model_en = 1'b0;
    for (int unsigned uid = 0; uid < 40; uid++) begin
        data.uid_tlb_record_by_uid.delete(uid);
        env.rm.observer_trace_translation_emitted.delete(uid);
        env.rm.observer_trace_translation_path_emitted.delete(uid);
    end
    env.rm.observer_mismatch_reported = 1'b0;

    // A fired request that has no completed/current entry is an immediate RM error.
    init_load_views(0, 64'h0000_0000_8123_4000, main_view, status_view);
    record = make_uid_tlb_record(0, main_view.vaddr, 2'd0, 2'd0, 1'b1, 1'b0);
    catcher = new("rm_fired_missing_entry_catcher", "RM_READONLY_API",
                  "*tlb_entry_exact_or_range query miss*");
    uvm_report_cb::add(null, catcher);
    if (env.rm.observer_build_commit_item(main_view, status_view, item)) begin
        uvm_report_cb::delete(null, catcher);
        `uvm_fatal(get_type_name(), "fired incomplete translation unexpectedly succeeded")
    end
    uvm_report_cb::delete(null, catcher);
    if (catcher.caught_count != 1) begin
        `uvm_fatal(get_type_name(),
                   "fired incomplete translation did not produce one readonly query miss")
    end
    entry = data.build_tlb_entry_for_key_with_csr(record.lookup_key,
                                                   record.csr_snapshot);
    entry.fault_raw_s1_pf = 1'b0;
    entry.fault_raw_s1_af = 1'b0;
    entry.fault_effective_s1_pf = 1'b0;
    entry.fault_effective_s1_af = 1'b0;
    entry.fault_stage_selected = memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_NONE;
    entry.pmaAF = 1'b0;
    entry.s1_pte_mode_at_build = memblock_tlb_entry::MEMBLOCK_TLB_PTE_MODE_LEGAL;
    entry.s1_pte_v = 1'b1;
    entry.s1_pte_r = 1'b1;
    entry.s1_pte_w = 1'b1;
    entry.s1_pte_a = 1'b1;
    entry.s1_pte_d = 1'b1;
    entry.s1_entry_pbmt = 2'd0;
    record.copy_entry_fields(entry);
    record.request_derived_valid = 1'b1;
    record.request_s1_resolved_ppn = 44'h34567;
    record.request_gvpn = record.vpn;
    record.mark_completed();
    if (!env.rm.observer_build_commit_item(main_view, status_view, item) ||
        !item.translation_valid ||
        !env.rm.observer_trace_translation_emitted.exists(0)) begin
        `uvm_fatal(get_type_name(),
                   "completed translation did not use the response payload")
    end

    // A cross-page request must fail immediately when the first VPN is
    // completed but the secondary VPN has no current live entry.  This proves
    // the second page cannot reuse the first page's completed payload.
    init_load_views(1, 64'h0000_0000_8124_4ffd, main_view, status_view);
    void'(make_uid_tlb_record(1, main_view.vaddr, 2'd0, 2'd0, 1'b1, 1'b1));
    catcher = new("rm_cross_page_missing_entry_catcher", "RM_READONLY_API",
                  "*tlb_entry_exact_or_range query miss*");
    uvm_report_cb::add(null, catcher);
    if (env.rm.observer_build_commit_item(main_view, status_view, item)) begin
        uvm_report_cb::delete(null, catcher);
        `uvm_fatal(get_type_name(), "cross-page incomplete translation unexpectedly succeeded")
    end
    uvm_report_cb::delete(null, catcher);
    if (catcher.caught_count != 1) begin
        `uvm_fatal(get_type_name(),
                   "cross-page incomplete translation did not produce one readonly query miss")
    end

    // Missing provenance remains a real readonly query error, not pending.
    init_load_views(2, 64'h0000_0000_8125_5000, main_view, status_view);
    catcher = new("rm_query_miss_catcher", "RM_READONLY_API",
                  "*tlb_request_context query miss*");
    uvm_report_cb::add(null, catcher);
    if (env.rm.observer_build_commit_item(main_view, status_view, item)) begin
        `uvm_fatal(get_type_name(), "missing UID TLB record unexpectedly succeeded")
    end
    uvm_report_cb::delete(null, catcher);
    if (catcher.caught_count != 1) begin
        `uvm_fatal(get_type_name(), "ordinary RM query miss was not observable")
    end

    // First-byte PBMT-NC/IO stop immediately but select different causes.
    for (int unsigned store = 0; store <= 1; store++) begin
        for (int unsigned pbmt = 1; pbmt <= 2; pbmt++) begin
        memblock_uid_t uid;
        bit expected_misaligned;
        bit expected_access_fault;

        uid = 2 + pbmt + (store * 4);
        if (store) begin
            init_store_views(uid, 64'h0000_0000_8126_6ffd + (uid << 13),
                             main_view, status_view);
        end else begin
            init_load_views(uid, 64'h0000_0000_8126_6ffd + (uid << 13),
                            main_view, status_view);
        end
        void'(make_uid_tlb_record(uid, main_view.vaddr, 2'd0, pbmt[1:0],
                                  1'b1, 1'b1));
        expected_misaligned = pbmt == 1;
        expected_access_fault = pbmt == 2;
        if (!env.rm.observer_build_commit_item(main_view, status_view, item) ||
            item.expected_exception[store ? 6 : 4] != expected_misaligned ||
            item.expected_exception[store ? 7 : 5] != expected_access_fault ||
            item.expected_exception !=
                (24'h000010 << (store * 2 + (pbmt == 2))) ||
            item.pa_valid_mask != 8'h01 || !item.uncache_path) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("first-byte store=%0d PBMT=%0d cause/stop mismatch exception=0x%0h mask=0x%0h",
                                 store, pbmt, item.expected_exception,
                                 item.pa_valid_mask))
        end
        end
    end

    // A PMA first page continues into the second page. NC or IO on a later
    // split page is delegated by the DUT misalign buffer as address-misaligned.
    for (int unsigned store = 0; store <= 1; store++) begin
        for (int unsigned pbmt = 1; pbmt <= 2; pbmt++) begin
            memblock_uid_t uid;
            bit [63:0] vaddr;
            bit [63:0] secondary_vaddr;
            memblock_tlb_lookup_key_t secondary_key;

            uid = 12 + pbmt + (store * 4);
            vaddr = 64'h0000_0000_8180_0ffd + (uid << 13);
            secondary_vaddr = vaddr + 8;
            if (store) init_store_views(uid, vaddr, main_view, status_view);
            else init_load_views(uid, vaddr, main_view, status_view);
            record = make_uid_tlb_record(uid, vaddr, 2'd0, 2'd0, 1'b1, 1'b1);
            publish_secondary_tlb_entry(record, secondary_vaddr, pbmt[1:0]);
            if (!env.rm.observer_build_commit_item(main_view, status_view, item) ||
                !item.expected_exception[store ? 6 : 4] ||
                item.expected_exception[store ? 7 : 5] ||
                item.pa_valid_mask != 8'hff || !item.uncache_path) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("later split store=%0d PBMT=%0d did not remain address-misaligned exception=0x%0h mask=0x%0h",
                                     store, pbmt, item.expected_exception,
                                     item.pa_valid_mask))
            end
            secondary_key = record.csr_snapshot.make_lookup_key(
                {14'b0, secondary_vaddr[49:12]}, 2'd0);
            data.delete_live_tlb_entry_by_anchor_key(
                secondary_key,
                "RM mixed PBMT directed cleanup");
        end
    end

    // Every translation fault retains priority over both NC and IO first-byte
    // misalign semantics. Exercise the effective S1/S2 fault fields directly;
    // the legacy pmaAF sideband below is only an additional compatibility case.
    for (int unsigned store = 0; store <= 1; store++) begin
        for (int unsigned pbmt = 1; pbmt <= 2; pbmt++) begin
            for (int unsigned fault_kind = 0; fault_kind < 4; fault_kind++) begin
                memblock_uid_t uid;
                bit [1:0] s2xlate;
                bit [23:0] expected_exception;

                uid = 20 + (store * 8) + ((pbmt - 1) * 4) + fault_kind;
                s2xlate = fault_kind >= 2 ? 2'd2 : 2'd0;
                if (store) begin
                    init_store_views(uid,
                                     64'h0000_0000_81c0_0001 + (uid << 13),
                                     main_view, status_view);
                end else begin
                    init_load_views(uid,
                                    64'h0000_0000_81c0_0001 + (uid << 13),
                                    main_view, status_view);
                end
                record = make_uid_tlb_record(uid, main_view.vaddr, s2xlate,
                                             pbmt[1:0], 1'b1, 1'b1);
                case (fault_kind)
                    0: begin
                        record.payload.fault_effective_s1_af = 1'b1;
                        record.payload.fault_stage_selected =
                            memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_S1;
                        expected_exception = 24'h000020 << (store * 2);
                    end
                    1: begin
                        record.payload.fault_effective_s1_pf = 1'b1;
                        record.payload.fault_stage_selected =
                            memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_S1;
                        expected_exception = 24'h002000 << (store * 2);
                    end
                    2: begin
                        record.payload.fault_effective_s2_gaf = 1'b1;
                        record.payload.fault_stage_selected =
                            memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_S2;
                        expected_exception = 24'h000020 << (store * 2);
                    end
                    default: begin
                        record.payload.fault_effective_s2_gpf = 1'b1;
                        record.payload.fault_stage_selected =
                            memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_S2;
                        expected_exception = 24'h200000 << (store * 2);
                    end
                endcase
                if (!env.rm.observer_build_commit_item(main_view, status_view, item) ||
                    item.expected_exception !== expected_exception) begin
                    `uvm_fatal(get_type_name(),
                               $sformatf("translation priority store=%0d PBMT=%0d fault_kind=%0d mismatch exception=0x%0h expected=0x%0h",
                                         store, pbmt, fault_kind,
                                         item.expected_exception,
                                         expected_exception))
                end
            end
        end
    end

    for (int unsigned store = 0; store <= 1; store++) begin
        memblock_uid_t uid;
        bit [23:0] expected_exception;

        uid = 36 + store;
        if (store) init_store_views(uid, 64'h0000_0000_8200_0001 + (uid << 13),
                                    main_view, status_view);
        else init_load_views(uid, 64'h0000_0000_8200_0001 + (uid << 13),
                             main_view, status_view);
        record = make_uid_tlb_record(uid, main_view.vaddr, 2'd0, 2'd2, 1'b1, 1'b1);
        record.payload.pmaAF = 1'b1;
        expected_exception = 24'h000020 << (store * 2);
        if (!env.rm.observer_build_commit_item(main_view, status_view, item) ||
            item.expected_exception !== expected_exception) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("PBMT-IO store=%0d did not preserve legacy pmaAF exception=0x%0h",
                                 store, item.expected_exception))
        end
    end

    // Permission faults remain exclusive even when the translated first byte
    // belongs to an allowed PMA C=0/MMIO region.  This exercises the production
    // guard that prevents PMA/PMP from appending a Load AF after PF/GPF.
    seq_csr_common::pma_pmp_model_en = 1'b1;
    data.pma_pmp_model.reset_and_init_v2_profile();
    permission_fault_mmio_pa = 48'h0000_2001_0001;
    init_load_views(5, 64'h0000_0000_812a_a001, main_view, status_view);
    record = make_uid_tlb_record(5, main_view.vaddr, 2'd0, 2'd0, 1'b0, 1'b1);
    record.request_s1_resolved_ppn = permission_fault_mmio_pa[47:12];
    if (!data.pma_pmp_model.capture_uid_context(
            5, status_view.dynamic_epoch, 2'd3, 1'b0, 1'b0, 1'b0, '0,
            record.csr_snapshot.update_seq, 500 + 5) ||
        !data.pma_pmp_model.evaluate_for_uid_epoch(
            5, status_view.dynamic_epoch, 1'b1, permission_fault_mmio_pa,
            1, PMA_PMP_CMD_READ, pma_eval) ||
        !pma_eval.valid || !pma_eval.translation_eligible ||
        pma_eval.cacheable || pma_eval.ld_fault) begin
        `uvm_fatal(get_type_name(),
                   "S1 permission-fault precondition is not allowed PMA MMIO")
    end
    if (!env.rm.observer_build_commit_item(main_view, status_view, item) ||
        item.expected_exception !== 24'h002000) begin
        `uvm_fatal(get_type_name(),
                   "S1 permission PF was not preserved exclusively over PMA MMIO")
    end
    init_load_views(6, 64'h0000_0000_812b_b001, main_view, status_view);
    record = make_uid_tlb_record(6, main_view.vaddr, 2'd2, 2'd0, 1'b0, 1'b1);
    record.request_s2_resolved_ppn = permission_fault_mmio_pa[47:12];
    if (!data.pma_pmp_model.capture_uid_context(
            6, status_view.dynamic_epoch, 2'd3, 1'b0, 1'b0, 1'b0, '0,
            record.csr_snapshot.update_seq, 500 + 6) ||
        !data.pma_pmp_model.evaluate_for_uid_epoch(
            6, status_view.dynamic_epoch, 1'b1, permission_fault_mmio_pa,
            1, PMA_PMP_CMD_READ, pma_eval) ||
        !pma_eval.valid || !pma_eval.translation_eligible ||
        pma_eval.cacheable || pma_eval.ld_fault) begin
        `uvm_fatal(get_type_name(),
                   "S2 permission-fault precondition is not allowed PMA MMIO")
    end
    if (!env.rm.observer_build_commit_item(main_view, status_view, item) ||
        item.expected_exception !== 24'h200000) begin
        `uvm_fatal(get_type_name(),
                   "S2 permission GPF was not preserved exclusively over PMA MMIO")
    end

    // Load PBMT-NC misalignment is generated independently from the S2
    // PMA/PMP access fault. StoreUnit instead suppresses storeAddrMisaligned
    // when the S2 PMA/PMP access fault is present.
    for (int unsigned store = 0; store <= 1; store++) begin
        memblock_uid_t uid;
        bit [47:0] fault_pa;
        bit [23:0] expected_exception;

        uid = 28 + store;
        // V2 PMA TOR entry covering [0x39002000, 0x3a000000) denies R/W.
        fault_pa = 48'h0000_3900_3001;
        if (store) init_store_views(uid, 64'h0000_0000_81e0_0001 + (uid << 13),
                                    main_view, status_view);
        else init_load_views(uid, 64'h0000_0000_81e0_0001 + (uid << 13),
                             main_view, status_view);
        record = make_uid_tlb_record(uid, main_view.vaddr, 2'd0, 2'd1, 1'b1, 1'b1);
        record.request_s1_resolved_ppn = fault_pa[47:12];
        if (!data.pma_pmp_model.capture_uid_context(
                uid, status_view.dynamic_epoch, 2'd3, 1'b0, 1'b0, 1'b0, '0,
                record.csr_snapshot.update_seq, 600 + uid) ||
            !data.pma_pmp_model.evaluate_for_uid_epoch(
                uid, status_view.dynamic_epoch, 1'b1, fault_pa,
                1, store ? PMA_PMP_CMD_WRITE : PMA_PMP_CMD_READ, pma_eval) ||
            !pma_eval.valid || !pma_eval.translation_eligible ||
            !(store ? pma_eval.st_fault : pma_eval.ld_fault)) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("PBMT-NC store=%0d PMA/PMP AF precondition failed", store))
        end
        expected_exception = store ? 24'h000080 : 24'h000030;
        if (!env.rm.observer_build_commit_item(main_view, status_view, item) ||
            item.expected_exception !== expected_exception) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("PBMT-NC store=%0d did not preserve misaligned+PMA/PMP AF exception=0x%0h",
                                 store, item.expected_exception))
        end
    end

    // A real PMA/PMP access fault suppresses Load Address Misaligned for
    // ordinary PBMT-PMA and PBMT-IO paths.  Only PBMT-NC Load may retain the
    // misaligned bit beside the access fault.  Exercise both hardware
    // misalign-enable values so that the AF gate, rather than the helper's
    // normal PMA behavior, determines the result.
    for (int unsigned hd_misalign = 0; hd_misalign <= 1; hd_misalign++) begin
        memblock_uid_t uid;
        bit [47:0] fault_pa;

        uid = 30 + hd_misalign;
        fault_pa = 48'h0000_3900_3001;
        init_load_views(uid, 64'h0000_0000_8200_0001 + (uid << 13),
                        main_view, status_view);
        record = make_uid_tlb_record(uid, main_view.vaddr, 2'd0, 2'd0, 1'b1, 1'b1);
        record.csr_snapshot.hd_misalign_ld_enable = hd_misalign;
        record.request_s1_resolved_ppn = fault_pa[47:12];
        if (!data.pma_pmp_model.capture_uid_context(
                uid, status_view.dynamic_epoch, 2'd3, 1'b0, 1'b0, 1'b0, '0,
                record.csr_snapshot.update_seq, 700 + uid) ||
            !data.pma_pmp_model.evaluate_for_uid_epoch(
                uid, status_view.dynamic_epoch, 1'b1, fault_pa,
                1, PMA_PMP_CMD_READ, pma_eval) ||
            !pma_eval.valid || !pma_eval.translation_eligible ||
            !pma_eval.ld_fault) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("PBMT-PMA hd_misalign=%0d PMA/PMP AF precondition failed",
                                 hd_misalign))
        end
        if (!env.rm.observer_build_commit_item(main_view, status_view, item) ||
            item.expected_exception !== 24'h000020) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("PBMT-PMA hd_misalign=%0d incorrectly retained Load Address Misaligned exception=0x%0h",
                                 hd_misalign, item.expected_exception))
        end
    end

    begin
        memblock_uid_t uid;
        bit [47:0] fault_pa;

        uid = 32;
        fault_pa = 48'h0000_3900_3001;
        init_load_views(uid, 64'h0000_0000_8240_0001, main_view, status_view);
        record = make_uid_tlb_record(uid, main_view.vaddr, 2'd0, 2'd2, 1'b1, 1'b1);
        record.request_s1_resolved_ppn = fault_pa[47:12];
        if (!data.pma_pmp_model.capture_uid_context(
                uid, status_view.dynamic_epoch, 2'd3, 1'b0, 1'b0, 1'b0, '0,
                record.csr_snapshot.update_seq, 700 + uid) ||
            !data.pma_pmp_model.evaluate_for_uid_epoch(
                uid, status_view.dynamic_epoch, 1'b1, fault_pa,
                1, PMA_PMP_CMD_READ, pma_eval) ||
            !pma_eval.valid || !pma_eval.translation_eligible ||
            !pma_eval.ld_fault) begin
            `uvm_fatal(get_type_name(), "PBMT-IO PMA/PMP AF precondition failed")
        end
        if (!env.rm.observer_build_commit_item(main_view, status_view, item) ||
            item.expected_exception !== 24'h000020) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("PBMT-IO incorrectly retained Load Address Misaligned exception=0x%0h",
                                 item.expected_exception))
        end
    end

    env.rm.observer_mismatch_reported = 1'b0;
    seq_csr_common::pma_pmp_model_en = saved_pma_pmp_model_en;
    for (int unsigned uid = 0; uid < 40; uid++) begin
        data.uid_tlb_record_by_uid.delete(uid);
    end
endtask:check_rm_production_commit_path

`endif
