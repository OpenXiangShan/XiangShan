//=========================================================
//File name    : memblock_dispatch_base_sequence.sv
//Author       : OpenAI_Codex
//Module name  : memblock_dispatch_base_sequence
//Discribution : dispatch framework base sequence entry
//Date         : 2026-05-18
//=========================================================
`ifndef MEMBLOCK_DISPATCH_BASE_SEQUENCE__SV
`define MEMBLOCK_DISPATCH_BASE_SEQUENCE__SV

class memblock_dispatch_base_sequence extends uvm_sequence;

    common_data_transaction data;
    lsq_ctrl_model          lsq_ctrl;
    issue_queue_scheduler   issue_sched;
    issue_field_assigner    field_assigner;
    writeback_status_handler writeback_handler;
    dispatch_monitor_batch_handler monitor_batch_handler;
    exception_redirect_replay_handler exception_handler;
    lsq_commit_handler monitor_commit_handler;
    dispatch_monitor_event_adapter monitor_adapter;
    main_control_transaction manual_main_table_by_rob[int unsigned];
    memblock_boundary_profile_candidate_t boundary_profile_cache[$];
    bit boundary_candidate_cache_built;

    `uvm_object_utils(memblock_dispatch_base_sequence)

    extern function new(string name = "memblock_dispatch_base_sequence");
    extern virtual task pre_body();
    extern virtual task body();
    extern virtual task post_body();
    extern virtual task build_main_table();
    extern virtual task build_random_main_table(input int unsigned main_trans_num_i);
    extern virtual task import_manual_main_table();
    extern virtual task route_all_issue_queues();
    extern virtual function void collect_csr_runtime_events();
    extern virtual function void collect_runtime_context_events();
    extern virtual task collect_monitor_event_batch();
    extern virtual task exception_redirect_replay_task();
    extern virtual function void clear_manual_main_table();
    extern virtual function void set_manual_main_transaction(input int unsigned rob_key,
                                                             input main_control_transaction tr);
    extern virtual function void randomize_main_transaction(input main_control_transaction tr,
                                                            input memblock_uid_t uid,
                                                            input memblock_rob_key_t rob_key);
    extern virtual function memblock_op_class_e select_op_class_by_weight();
    extern virtual function void apply_minimal_op_template(input main_control_transaction tr);
    extern virtual function void apply_legal_addr_template(input main_control_transaction tr);
    extern virtual function void build_boundary_candidate_cache();
    extern virtual function void generate_boundary_op_from_cache(input main_control_transaction tr);
    extern virtual function void apply_op_class_template(input main_control_transaction tr,
                                                         input bit [8:0] fuOpType);
    extern virtual function void apply_boundary_addr_template(input main_control_transaction tr,
                                                              input memblock_boundary_profile_e profile,
                                                              input int unsigned size_bytes);
    extern virtual function bit [63:0] gen_final_vaddr_by_profile(input memblock_boundary_profile_e profile,
                                                                   input int unsigned size_bytes,
                                                                   input memblock_op_class_e op_class);
    extern virtual function void check_boundary_profile(input main_control_transaction tr,
                                                        input string caller);
    extern virtual function memblock_boundary_profile_e classify_boundary_profile(input bit [63:0] vaddr,
                                                                                  input int unsigned size_bytes);
    extern virtual function bit boundary_profile_supported_for_op(input memblock_op_class_e op_class,
                                                                  input memblock_boundary_profile_e profile);
    extern virtual function bit boundary_profile_supported_for_fuop(input memblock_op_class_e op_class,
                                                                    input bit [8:0] fuOpType,
                                                                    input memblock_boundary_profile_e profile,
                                                                    input int unsigned size_bytes);
    extern virtual function void enumerate_boundary_profiles(ref memblock_boundary_profile_e profiles[$]);
    extern virtual function void enumerate_op_classes(ref memblock_op_class_e op_classes[$]);
    extern virtual function void enumerate_fuoptypes(input memblock_op_class_e op_class,
                                                     ref bit [8:0] fuops[$]);
    extern virtual function bit fuop_belongs_to_op_class(input memblock_op_class_e op_class,
                                                         input bit [8:0] fuOpType);
    extern virtual function int unsigned get_op_class_weight(input memblock_op_class_e op_class);
    extern virtual function int unsigned get_fuop_weight(input memblock_op_class_e op_class,
                                                         input bit [8:0] fuOpType);
    extern virtual function bit [8:0] pick_fuop_by_weight(input memblock_op_class_e op_class,
                                                          input string caller);
    extern virtual function bit [8:0] default_fuop_by_op_profile(input memblock_op_class_e op_class,
                                                                 input memblock_boundary_profile_e profile);
    extern virtual function bit [8:0] default_fuop_by_op_class_and_size(input memblock_op_class_e op_class,
                                                                        input int unsigned size_bytes);
    extern virtual function bit [8:0] choose_fuop_by_op_class_and_size(input memblock_op_class_e op_class,
                                                                       input int unsigned size_bytes,
                                                                       input string caller);
    extern virtual function int unsigned derive_size_bytes(input memblock_op_class_e op_class,
                                                           input bit [8:0] fuOpType);
    extern virtual function int weighted_pick_index(input int unsigned weights[$]);
    extern virtual function bit is_sv39_positive_canonical(input bit [63:0] vaddr);
    extern virtual function bit [63:0] random64();
    extern virtual function bit [63:0] random_aligned_vaddr(input bit [63:0] align_bytes);
    extern virtual function bit [63:0] random_negative_imm12();
    extern virtual function string boundary_profile_name(input memblock_boundary_profile_e profile);
    extern virtual function string op_class_name(input memblock_op_class_e op_class);
    extern virtual function bit [8:0] random_load_fuoptype();
    extern virtual function bit [8:0] random_store_fuoptype();
    extern virtual function bit [8:0] random_prefetch_fuoptype();
    extern virtual function bit [8:0] random_cbo_fuoptype();
    extern virtual function bit [8:0] random_amo_fuoptype();
    extern virtual function memblock_rob_key_t choose_rob_start_key();
    extern virtual function int unsigned choose_addr_ref_window();
    extern virtual function memblock_addr_reuse_kind_e select_addr_reuse_kind();
    extern virtual function void prune_recent_uid_q(ref memblock_uid_t uid_q[$],
                                                    input memblock_uid_t cur_uid,
                                                    input int unsigned addr_ref_window);
    extern virtual function bit random_pick_recent_uid(ref memblock_uid_t uid_q[$],
                                                       output memblock_uid_t ref_uid,
                                                       input bit delete_after_pick);
    extern virtual function void set_transaction_ls_kind(input main_control_transaction tr,
                                                         input bit make_load);
    extern virtual function void fixup_after_addr_reuse(input main_control_transaction tr,
                                                        input main_control_transaction ref_tr,
                                                        input bit copy_addr,
                                                        input string caller);
    extern virtual function void apply_addr_reuse_window(input main_control_transaction tr,
                                                         input memblock_uid_t cur_uid,
                                                         ref memblock_uid_t recent_load_uid_q[$],
                                                         ref memblock_uid_t recent_store_uid_q[$]);
    extern virtual function void sync_boundary_profile_after_addr_reuse(input main_control_transaction tr,
                                                                        input string caller);
    extern virtual function void push_recent_uid(input main_control_transaction tr,
                                                 input memblock_uid_t uid,
                                                 ref memblock_uid_t recent_load_uid_q[$],
                                                 ref memblock_uid_t recent_store_uid_q[$]);
    extern virtual function int unsigned randomize_send_pri_value(input bit is_std);
    extern virtual function int unsigned randomize_delay_value();
    extern virtual function bit rand_percent_hit(input int unsigned percent);
    extern virtual function int unsigned rand_weighted2(input int unsigned w0,
                                                        input int unsigned w1);
    extern virtual function int unsigned rand_weighted3(input int unsigned w0,
                                                        input int unsigned w1,
                                                        input int unsigned w2);
    extern virtual function int unsigned rand_weighted4(input int unsigned w0,
                                                        input int unsigned w1,
                                                        input int unsigned w2,
                                                        input int unsigned w3);
    extern virtual function int unsigned rand_weighted5(input int unsigned w0,
                                                        input int unsigned w1,
                                                        input int unsigned w2,
                                                        input int unsigned w3,
                                                        input int unsigned w4);
    extern virtual function int unsigned rand_weighted6(input int unsigned w0,
                                                        input int unsigned w1,
                                                        input int unsigned w2,
                                                        input int unsigned w3,
                                                        input int unsigned w4,
                                                        input int unsigned w5);
    extern virtual function bit is_load_main_tr(input main_control_transaction tr);
    extern virtual function bit is_store_main_tr(input main_control_transaction tr);
    extern virtual function bit is_vector_ls_main_tr(input main_control_transaction tr);
    extern virtual function bit is_load_fuoptype(input bit [8:0] fuOpType);
    extern virtual function bit is_store_fuoptype(input bit [8:0] fuOpType);
    extern virtual function bit is_prefetch_fuoptype(input bit [8:0] fuOpType);
    extern virtual function bit is_amo_fuoptype(input bit [8:0] fuOpType);
    extern virtual function memblock_op_behavior_t derive_op_behavior(input main_control_transaction tr);
    extern virtual function void init_status_for_main_table();
    extern virtual function void validate_main_table_entry(input main_control_transaction tr,
                                                           input string caller);

endclass:memblock_dispatch_base_sequence

function memblock_dispatch_base_sequence::new(string name = "memblock_dispatch_base_sequence");
    super.new(name);
    boundary_candidate_cache_built = 1'b0;
endfunction:new

task memblock_dispatch_base_sequence::pre_body();
    super.pre_body();
    seq_csr_common::init();
    data = common_data_transaction::get();
    if (lsq_ctrl == null) begin
        lsq_ctrl = lsq_ctrl_model::get();
    end
    lsq_ctrl.reset();
    if (issue_sched == null) begin
        issue_sched = issue_queue_scheduler::type_id::create("issue_sched");
    end
    if (field_assigner == null) begin
        field_assigner = issue_field_assigner::type_id::create("field_assigner");
    end
    if (writeback_handler == null) begin
        writeback_handler = writeback_status_handler::type_id::create("writeback_handler");
    end
    if (monitor_batch_handler == null) begin
        monitor_batch_handler = dispatch_monitor_batch_handler::type_id::create("monitor_batch_handler");
        monitor_batch_handler.bind_writeback_handler(writeback_handler);
    end
    if (exception_handler == null) begin
        exception_handler = exception_redirect_replay_handler::type_id::create("exception_handler");
    end
    if (monitor_commit_handler == null) begin
        monitor_commit_handler = lsq_commit_handler::type_id::create("monitor_commit_handler");
        monitor_commit_handler.bind_lsq_ctrl(lsq_ctrl);
    end
    if (monitor_adapter == null) begin
        monitor_adapter = dispatch_monitor_event_adapter::type_id::create("monitor_adapter");
        monitor_adapter.bind_commit_handler(monitor_commit_handler);
    end
endtask:pre_body

task memblock_dispatch_base_sequence::body();
    `uvm_info(get_type_name(), "Task1 skeleton only; dispatch framework flow is not enabled yet.", UVM_LOW)
endtask:body

task memblock_dispatch_base_sequence::post_body();
    super.post_body();
endtask:post_body

task memblock_dispatch_base_sequence::build_main_table();
    if (data == null) begin
        data = common_data_transaction::get();
    end

    if (seq_csr_common::get_use_manual_main_table()) begin
        import_manual_main_table();
    end else begin
        build_random_main_table(seq_csr_common::get_main_trans_num());
    end
endtask:build_main_table

task memblock_dispatch_base_sequence::build_random_main_table(input int unsigned main_trans_num_i);
    memblock_rob_key_t rob_key;
    memblock_uid_t recent_load_uid_q[$];
    memblock_uid_t recent_store_uid_q[$];
    int unsigned addr_ref_window;

    if (data == null) begin
        data = common_data_transaction::get();
    end
    data.reset_all_tables(main_trans_num_i);
    rob_key = choose_rob_start_key();
    addr_ref_window = choose_addr_ref_window();
    if (seq_csr_common::get_boundary_profile_gen_en()) begin
        build_boundary_candidate_cache();
    end else begin
        boundary_candidate_cache_built = 1'b0;
        boundary_profile_cache.delete();
    end

    for (int unsigned idx = 0; idx < main_trans_num_i; idx++) begin
        memblock_uid_t uid;
        main_control_transaction tr;

        uid = data.alloc_uid();
        tr = main_control_transaction::type_id::create($sformatf("main_uid_%0d", uid));
        randomize_main_transaction(tr, uid, rob_key);
        prune_recent_uid_q(recent_load_uid_q, uid, addr_ref_window);
        prune_recent_uid_q(recent_store_uid_q, uid, addr_ref_window);
        apply_addr_reuse_window(tr, uid, recent_load_uid_q, recent_store_uid_q);
        if (seq_csr_common::get_boundary_profile_gen_en()) begin
            sync_boundary_profile_after_addr_reuse(tr,
                $sformatf("random uid=%0d boundary after addr reuse", uid));
        end
        data.set_main_transaction(uid, tr);
        push_recent_uid(tr, uid, recent_load_uid_q, recent_store_uid_q);
        rob_key = rob_order_util::rob_advance(rob_key, 1);
    end

    init_status_for_main_table();
    data.check_main_table_complete();
endtask:build_random_main_table

task memblock_dispatch_base_sequence::import_manual_main_table();
    int unsigned manual_num;
    int unsigned rob_keys[$];

    if (data == null) begin
        data = common_data_transaction::get();
    end
    manual_num = manual_main_table_by_rob.num();
    if (manual_num == 0) begin
        `uvm_fatal(get_type_name(), "manual main table mode requires at least one configured entry")
    end

    data.reset_all_tables(manual_num);
    foreach (manual_main_table_by_rob[rob_key]) begin
        rob_keys.push_back(rob_key);
    end
    rob_keys.sort();
    foreach (rob_keys[idx]) begin
        int unsigned rob_key;
        memblock_uid_t uid;
        main_control_transaction tr;

        rob_key = rob_keys[idx];
        tr = manual_main_table_by_rob[rob_key];
        if (tr == null) begin
            `uvm_fatal(get_type_name(), $sformatf("manual main table rob_key=%0d got null transaction", rob_key))
        end
        uid = data.alloc_uid();
        tr.uid = uid;
        tr.post_manual_config(1'b1);
        validate_main_table_entry(tr, $sformatf("manual rob_key=%0d", rob_key));
        data.set_main_transaction(uid, tr);
    end

    init_status_for_main_table();
    data.check_main_table_complete();
endtask:import_manual_main_table

task memblock_dispatch_base_sequence::route_all_issue_queues();
    if (issue_sched == null) begin
        issue_sched = issue_queue_scheduler::type_id::create("issue_sched");
    end
    issue_sched.route_all_ready_uids();
endtask:route_all_issue_queues

function void memblock_dispatch_base_sequence::collect_csr_runtime_events();
    if (monitor_adapter == null) begin
        monitor_adapter = dispatch_monitor_event_adapter::type_id::create("monitor_adapter");
    end
    if (monitor_commit_handler != null) begin
        monitor_adapter.bind_commit_handler(monitor_commit_handler);
    end
    monitor_adapter.drain_csr_events();
endfunction:collect_csr_runtime_events

function void memblock_dispatch_base_sequence::collect_runtime_context_events();
    if (monitor_adapter == null) begin
        monitor_adapter = dispatch_monitor_event_adapter::type_id::create("monitor_adapter");
    end
    if (monitor_commit_handler != null) begin
        monitor_adapter.bind_commit_handler(monitor_commit_handler);
    end
    // 中文注释：统一 service loop 显式先同步 CSR runtime，再消费 sfence/hfence。
    // 其它查表或 writeback/ctrl 路径如果只需要最新 CSR，应只调用 drain_csr_events()。
    monitor_adapter.drain_csr_events();
    monitor_adapter.drain_sfence_events();
endfunction:collect_runtime_context_events

task memblock_dispatch_base_sequence::collect_monitor_event_batch();
    memblock_wb_event_t events[$];

    if (writeback_handler == null) begin
        writeback_handler = writeback_status_handler::type_id::create("writeback_handler");
    end
    if (monitor_batch_handler == null) begin
        monitor_batch_handler = dispatch_monitor_batch_handler::type_id::create("monitor_batch_handler");
    end
    monitor_batch_handler.bind_writeback_handler(writeback_handler);
    if (monitor_commit_handler == null) begin
        monitor_commit_handler = lsq_commit_handler::type_id::create("monitor_commit_handler");
        if (lsq_ctrl != null) begin
            monitor_commit_handler.bind_lsq_ctrl(lsq_ctrl);
        end
    end
    if (monitor_adapter == null) begin
        monitor_adapter = dispatch_monitor_event_adapter::type_id::create("monitor_adapter");
    end
    monitor_adapter.bind_commit_handler(monitor_commit_handler);
    // 中文注释：同一 service cycle 内的 int writeback、IQ feedback 和 memoryViolation
    // 必须先形成同一个 semantic batch，再由 dispatch_monitor_batch_handler 做 redirect-first 仲裁。
    monitor_adapter.collect_writeback_events_batch(events);
    monitor_adapter.collect_ctrl_redirect_events_batch(events);
    monitor_batch_handler.process_monitor_event_batch(events);
endtask:collect_monitor_event_batch

task memblock_dispatch_base_sequence::exception_redirect_replay_task();
    if (exception_handler == null) begin
        exception_handler = exception_redirect_replay_handler::type_id::create("exception_handler");
    end
    exception_handler.process_pending_events();
endtask:exception_redirect_replay_task

function void memblock_dispatch_base_sequence::clear_manual_main_table();
    manual_main_table_by_rob.delete();
endfunction:clear_manual_main_table

function void memblock_dispatch_base_sequence::set_manual_main_transaction(input int unsigned rob_key,
                                                                           input main_control_transaction tr);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), $sformatf("set_manual_main_transaction rob_key=%0d got null transaction", rob_key))
    end
    manual_main_table_by_rob[rob_key] = tr;
endfunction:set_manual_main_transaction

function void memblock_dispatch_base_sequence::randomize_main_transaction(input main_control_transaction tr,
                                                                          input memblock_uid_t uid,
                                                                          input memblock_rob_key_t rob_key);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "randomize_main_transaction got null transaction")
    end
    if (!tr.randomize()) begin
        `uvm_fatal(get_type_name(), $sformatf("main transaction randomize failed uid=%0d", uid))
    end

    tr.uid          = uid;
    tr.robIdx_flag  = rob_key.flag;
    tr.robIdx_value = rob_key.value;
    tr.lqIdx_flag   = 1'b0;
    tr.lqIdx_value  = '0;
    tr.sqIdx_flag   = 1'b0;
    tr.sqIdx_value  = '0;
    tr.numLsElem    = 5'd1;
    tr.imm          = main_control_transaction::sign_extend_imm12(tr.imm);
    tr.tlbAF        = 1'b0;
    tr.tlbPF        = 1'b0;
    tr.tlbGPF       = 1'b0;
    tr.PBMT         = '0;
    tr.pmaAF        = 1'b0;
    tr.corrupt      = 1'b0;
    tr.denied       = 1'b0;
    if (seq_csr_common::get_boundary_profile_gen_en()) begin
        if (!boundary_candidate_cache_built) begin
            build_boundary_candidate_cache();
        end
        generate_boundary_op_from_cache(tr);
        apply_boundary_addr_template(tr, tr.boundary_profile, tr.boundary_size_bytes);
        check_boundary_profile(tr, $sformatf("random uid=%0d boundary", uid));
    end else begin
        tr.op_class = select_op_class_by_weight();
        apply_minimal_op_template(tr);
        apply_legal_addr_template(tr);
    end
    tr.send_pri     = randomize_send_pri_value(1'b0);
    tr.send_pri_std = randomize_send_pri_value(1'b1);
    tr.delay        = randomize_delay_value();
    tr.update_vaddr();
    validate_main_table_entry(tr, $sformatf("random uid=%0d", uid));
endfunction:randomize_main_transaction

function memblock_op_class_e memblock_dispatch_base_sequence::select_op_class_by_weight();
    int unsigned sel;

    sel = rand_weighted6(seq_csr_common::get_op_class_int_load_wt(),
                         seq_csr_common::get_op_class_fp_load_wt(),
                         seq_csr_common::get_op_class_store_wt(),
                         seq_csr_common::get_op_class_prefetch_wt(),
                         seq_csr_common::get_op_class_amo_wt(),
                         seq_csr_common::get_op_class_cbo_wt());
    case (sel)
        0: return MEMBLOCK_OP_CLASS_INT_LOAD;
        1: return MEMBLOCK_OP_CLASS_FP_LOAD;
        2: return MEMBLOCK_OP_CLASS_STORE;
        3: return MEMBLOCK_OP_CLASS_PREFETCH;
        4: return MEMBLOCK_OP_CLASS_AMO;
        5: return MEMBLOCK_OP_CLASS_CBO;
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("illegal op_class select=%0d", sel))
        end
    endcase
    return MEMBLOCK_OP_CLASS_UNKNOWN;
endfunction:select_op_class_by_weight

function void memblock_dispatch_base_sequence::apply_minimal_op_template(input main_control_transaction tr);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "apply_minimal_op_template got null transaction")
    end

    case (tr.op_class)
        MEMBLOCK_OP_CLASS_INT_LOAD,
        MEMBLOCK_OP_CLASS_FP_LOAD: begin
            tr.fuType   = MEMBLOCK_FUTYPE_LDU;
            tr.lsq_flow = MEMBLOCK_LSQ_FLOW_LOAD;
            tr.fuOpType = random_load_fuoptype();
            tr.numLsElem = 5'd1;
        end
        MEMBLOCK_OP_CLASS_STORE: begin
            tr.fuType   = MEMBLOCK_FUTYPE_STU;
            tr.lsq_flow = MEMBLOCK_LSQ_FLOW_STORE;
            tr.fuOpType = random_store_fuoptype();
            tr.numLsElem = 5'd1;
        end
        MEMBLOCK_OP_CLASS_PREFETCH: begin
            tr.fuType   = MEMBLOCK_FUTYPE_LDU;
            tr.lsq_flow = MEMBLOCK_LSQ_FLOW_LOAD;
            tr.fuOpType = random_prefetch_fuoptype();
            tr.numLsElem = 5'd1;
        end
        MEMBLOCK_OP_CLASS_CBO: begin
            tr.fuType   = MEMBLOCK_FUTYPE_STU;
            tr.lsq_flow = MEMBLOCK_LSQ_FLOW_CBO;
            tr.fuOpType = random_cbo_fuoptype();
            tr.numLsElem = 5'd1;
        end
        MEMBLOCK_OP_CLASS_AMO: begin
            tr.fuType   = MEMBLOCK_FUTYPE_MOU;
            tr.lsq_flow = MEMBLOCK_LSQ_FLOW_ATOMIC;
            tr.fuOpType = random_amo_fuoptype();
            tr.numLsElem = 5'd0;
        end
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("unsupported op_class=%0d", tr.op_class))
        end
    endcase
endfunction:apply_minimal_op_template

function void memblock_dispatch_base_sequence::apply_legal_addr_template(input main_control_transaction tr);
    bit [63:0] base;
    bit [63:0] range;
    bit [63:0] upper;
    bit [63:0] aligned_base;
    bit [63:0] align_mask;
    bit [63:0] slot_count;
    bit [63:0] slot_pick;

    if (tr == null) begin
        `uvm_fatal(get_type_name(), "apply_legal_addr_template got null transaction")
    end

    base       = seq_csr_common::get_paddr_base();
    range      = seq_csr_common::get_paddr_range();
    upper      = base + range - 1;
    align_mask = 64'd63;

    aligned_base = (base + align_mask) & ~align_mask;
    if (aligned_base > upper) begin
        aligned_base = base;
        slot_count   = 64'd1;
    end else begin
        slot_count = ((upper - aligned_base) >> 6) + 1;
    end

    if (slot_count <= 1) begin
        slot_pick = 64'd0;
    end else begin
        slot_pick = {$urandom(), $urandom()} % slot_count;
    end

    tr.src_0 = aligned_base + (slot_pick << 6);
    tr.imm   = 64'h0;
    tr.update_vaddr();
endfunction:apply_legal_addr_template

function void memblock_dispatch_base_sequence::build_boundary_candidate_cache();
    memblock_boundary_profile_e profiles[$];
    memblock_op_class_e op_classes[$];
    int unsigned profile_count;

    boundary_profile_cache.delete();
    boundary_candidate_cache_built = 1'b0;
    profile_count = 0;
    if (!seq_csr_common::get_boundary_profile_gen_en()) begin
        return;
    end

    enumerate_boundary_profiles(profiles);
    enumerate_op_classes(op_classes);
    foreach (profiles[pidx]) begin
        memblock_boundary_profile_candidate_t profile_entry;
        int unsigned profile_weight;

        profile_weight = seq_csr_common::get_boundary_profile_weight(profiles[pidx]);
        if (profile_weight == 0) begin
            continue;
        end

        profile_entry.profile = profiles[pidx];
        profile_entry.profile_weight = profile_weight;
        profile_entry.op_cache.delete();

        foreach (op_classes[oidx]) begin
            memblock_boundary_op_candidate_t op_entry;
            bit [8:0] legal_fuops[$];
            int unsigned legal_sizes[$];
            int unsigned positive_weight_count;
            int unsigned op_weight;

            op_weight = get_op_class_weight(op_classes[oidx]);
            if (op_weight == 0) begin
                continue;
            end
            if (!boundary_profile_supported_for_op(op_classes[oidx], profiles[pidx])) begin
                continue;
            end
            if (op_classes[oidx] == MEMBLOCK_OP_CLASS_STORE &&
                profiles[pidx] == MEMBLOCK_BOUNDARY_PROFILE_CROSS_8B_WITHIN_16B &&
                !seq_csr_common::get_store_cross_8b_within_16b_en()) begin
                continue;
            end

            begin
                bit [8:0] fuops[$];
                enumerate_fuoptypes(op_classes[oidx], fuops);
                foreach (fuops[fidx]) begin
                    int unsigned size_bytes;

                    size_bytes = derive_size_bytes(op_classes[oidx], fuops[fidx]);
                    if (boundary_profile_supported_for_fuop(op_classes[oidx], fuops[fidx], profiles[pidx], size_bytes)) begin
                        legal_fuops.push_back(fuops[fidx]);
                        legal_sizes.push_back(size_bytes);
                    end
                end
            end

            if (legal_fuops.size() == 0) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("boundary cache support matrix mismatch: profile=%s op_class=%s has no legal fuOpType",
                                     boundary_profile_name(profiles[pidx]),
                                     op_class_name(op_classes[oidx])))
            end

            op_entry.op_class = op_classes[oidx];
            op_entry.op_class_weight = op_weight;
            op_entry.fuop_cache.delete();
            positive_weight_count = 0;
            foreach (legal_fuops[lidx]) begin
                if (get_fuop_weight(op_classes[oidx], legal_fuops[lidx]) > 0) begin
                    positive_weight_count++;
                end
            end

            if (positive_weight_count > 0) begin
                foreach (legal_fuops[lidx]) begin
                    int unsigned fuop_weight;
                    fuop_weight = get_fuop_weight(op_classes[oidx], legal_fuops[lidx]);
                    if (fuop_weight > 0) begin
                        memblock_boundary_fuop_candidate_t fuop_entry;

                        fuop_entry.fuOpType = legal_fuops[lidx];
                        fuop_entry.size_bytes = legal_sizes[lidx];
                        fuop_entry.cfg_fuop_weight = fuop_weight;
                        fuop_entry.effective_weight = fuop_weight;
                        fuop_entry.use_default = 1'b0;
                        op_entry.fuop_cache.push_back(fuop_entry);
                    end
                end
            end else begin
                bit [8:0] default_fuop;
                int unsigned default_size;
                memblock_boundary_fuop_candidate_t fuop_entry;

                `uvm_error(get_type_name(),
                           $sformatf("boundary cache config error: profile=%s op_class=%s legal fuOpType weights are all zero, use default fuOpType",
                                     boundary_profile_name(profiles[pidx]),
                                     op_class_name(op_classes[oidx])))
                default_fuop = default_fuop_by_op_profile(op_classes[oidx], profiles[pidx]);
                default_size = derive_size_bytes(op_classes[oidx], default_fuop);
                if (!fuop_belongs_to_op_class(op_classes[oidx], default_fuop)) begin
                    `uvm_fatal(get_type_name(),
                               $sformatf("default fuOpType not in op_class enum list: profile=%s op_class=%s fuOpType=0x%0h",
                                         boundary_profile_name(profiles[pidx]),
                                         op_class_name(op_classes[oidx]),
                                         default_fuop))
                end
                if (!boundary_profile_supported_for_fuop(op_classes[oidx], default_fuop, profiles[pidx], default_size)) begin
                    `uvm_fatal(get_type_name(),
                               $sformatf("default fuOpType illegal: profile=%s op_class=%s fuOpType=0x%0h size=%0d",
                                         boundary_profile_name(profiles[pidx]),
                                         op_class_name(op_classes[oidx]),
                                         default_fuop,
                                         default_size))
                end
                fuop_entry.fuOpType = default_fuop;
                fuop_entry.size_bytes = default_size;
                fuop_entry.cfg_fuop_weight = 0;
                fuop_entry.effective_weight = 1;
                fuop_entry.use_default = 1'b1;
                op_entry.fuop_cache.push_back(fuop_entry);
            end

            profile_entry.op_cache.push_back(op_entry);
        end

        if (profile_entry.op_cache.size() == 0) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("boundary profile %s weight is non-zero but no legal op_class/fuOpType candidate exists",
                                 boundary_profile_name(profiles[pidx])))
        end

        boundary_profile_cache.push_back(profile_entry);
        profile_count++;
    end

    if (boundary_profile_cache.size() == 0 || profile_count == 0) begin
        `uvm_fatal(get_type_name(), "MEMBLOCK_BOUNDARY_PROFILE_GEN_EN=1 requires at least one legal boundary_profile candidate")
    end
    boundary_candidate_cache_built = 1'b1;
    `uvm_info(get_type_name(),
              $sformatf("built boundary candidate cache with %0d profile entries", boundary_profile_cache.size()),
              UVM_LOW)
endfunction:build_boundary_candidate_cache

function void memblock_dispatch_base_sequence::generate_boundary_op_from_cache(input main_control_transaction tr);
    int unsigned weights[$];
    int profile_idx;
    int op_idx;
    int fuop_idx;
    memblock_boundary_profile_candidate_t profile_entry;
    memblock_boundary_op_candidate_t op_entry;
    memblock_boundary_fuop_candidate_t fuop_entry;

    if (!seq_csr_common::get_boundary_profile_gen_en()) begin
        `uvm_fatal(get_type_name(), "generate_boundary_op_from_cache called while MEMBLOCK_BOUNDARY_PROFILE_GEN_EN=0")
    end
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "generate_boundary_op_from_cache got null transaction")
    end
    if (!boundary_candidate_cache_built) begin
        `uvm_fatal(get_type_name(), "boundary candidate cache is not built")
    end

    weights.delete();
    foreach (boundary_profile_cache[i]) begin
        weights.push_back(boundary_profile_cache[i].profile_weight);
    end
    profile_idx = weighted_pick_index(weights);
    profile_entry = boundary_profile_cache[profile_idx];
    tr.boundary_profile = profile_entry.profile;

    weights.delete();
    foreach (profile_entry.op_cache[i]) begin
        weights.push_back(profile_entry.op_cache[i].op_class_weight);
    end
    op_idx = weighted_pick_index(weights);
    op_entry = profile_entry.op_cache[op_idx];
    tr.op_class = op_entry.op_class;

    weights.delete();
    foreach (op_entry.fuop_cache[i]) begin
        weights.push_back(op_entry.fuop_cache[i].effective_weight);
    end
    fuop_idx = weighted_pick_index(weights);
    fuop_entry = op_entry.fuop_cache[fuop_idx];
    if (fuop_entry.use_default) begin
        `uvm_info(get_type_name(),
                  $sformatf("uid=%0d uses default boundary fuOpType profile=%s op_class=%s fuOpType=0x%0h",
                            tr.uid,
                            boundary_profile_name(profile_entry.profile),
                            op_class_name(op_entry.op_class),
                            fuop_entry.fuOpType),
                  UVM_MEDIUM)
    end

    apply_op_class_template(tr, fuop_entry.fuOpType);
    tr.boundary_size_bytes = fuop_entry.size_bytes;
endfunction:generate_boundary_op_from_cache

function void memblock_dispatch_base_sequence::apply_op_class_template(input main_control_transaction tr,
                                                                       input bit [8:0] fuOpType);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "apply_op_class_template got null transaction")
    end

    case (tr.op_class)
        MEMBLOCK_OP_CLASS_INT_LOAD,
        MEMBLOCK_OP_CLASS_FP_LOAD: begin
            tr.fuType    = MEMBLOCK_FUTYPE_LDU;
            tr.lsq_flow  = MEMBLOCK_LSQ_FLOW_LOAD;
            tr.fuOpType  = fuOpType;
            tr.numLsElem = 5'd1;
        end
        MEMBLOCK_OP_CLASS_STORE: begin
            tr.fuType    = MEMBLOCK_FUTYPE_STU;
            tr.lsq_flow  = MEMBLOCK_LSQ_FLOW_STORE;
            tr.fuOpType  = fuOpType;
            tr.numLsElem = 5'd1;
        end
        MEMBLOCK_OP_CLASS_PREFETCH: begin
            tr.fuType    = MEMBLOCK_FUTYPE_LDU;
            tr.lsq_flow  = MEMBLOCK_LSQ_FLOW_LOAD;
            tr.fuOpType  = fuOpType;
            tr.numLsElem = 5'd1;
        end
        MEMBLOCK_OP_CLASS_CBO: begin
            tr.fuType    = MEMBLOCK_FUTYPE_STU;
            tr.lsq_flow  = MEMBLOCK_LSQ_FLOW_CBO;
            tr.fuOpType  = fuOpType;
            tr.numLsElem = 5'd1;
        end
        MEMBLOCK_OP_CLASS_AMO: begin
            tr.fuType    = MEMBLOCK_FUTYPE_MOU;
            tr.lsq_flow  = MEMBLOCK_LSQ_FLOW_ATOMIC;
            tr.fuOpType  = fuOpType;
            tr.numLsElem = 5'd0;
        end
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("unsupported boundary op_class=%0d", tr.op_class))
        end
    endcase
endfunction:apply_op_class_template

function void memblock_dispatch_base_sequence::apply_boundary_addr_template(input main_control_transaction tr,
                                                                            input memblock_boundary_profile_e profile,
                                                                            input int unsigned size_bytes);
    bit [63:0] final_vaddr;
    bit [63:0] end_vaddr;
    bit [63:0] size_minus_one;
    bit [63:0] imm12;
    bit [63:0] imm_sext;
    bit [63:0] src_0;

    if (!seq_csr_common::get_boundary_profile_gen_en()) begin
        `uvm_fatal(get_type_name(), "apply_boundary_addr_template called while MEMBLOCK_BOUNDARY_PROFILE_GEN_EN=0")
    end
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "apply_boundary_addr_template got null transaction")
    end
    if (size_bytes == 0) begin
        `uvm_fatal(get_type_name(), $sformatf("apply_boundary_addr_template uid=%0d got zero size", tr.uid))
    end

    final_vaddr = gen_final_vaddr_by_profile(profile, size_bytes, tr.op_class);
    size_minus_one = size_bytes - 1;
    end_vaddr = final_vaddr + size_minus_one;
    if (end_vaddr < final_vaddr ||
        !is_sv39_positive_canonical(final_vaddr) ||
        !is_sv39_positive_canonical(end_vaddr)) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("boundary addr template bug uid=%0d profile=%s vaddr=0x%0h end=0x%0h size=%0d",
                             tr.uid, boundary_profile_name(profile), final_vaddr, end_vaddr, size_bytes))
    end

    imm12 = random_negative_imm12();
    imm_sext = main_control_transaction::sign_extend_imm12(imm12);
    src_0 = final_vaddr - imm_sext;
    if (src_0 + imm_sext != final_vaddr) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("boundary imm split bug uid=%0d final=0x%0h src=0x%0h imm=0x%0h",
                             tr.uid, final_vaddr, src_0, imm12))
    end

    tr.src_0 = src_0;
    tr.imm   = imm12;
    tr.update_vaddr();

    if (tr.vaddr != final_vaddr) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("boundary update_vaddr mismatch uid=%0d expect=0x%0h actual=0x%0h",
                             tr.uid, final_vaddr, tr.vaddr))
    end
    if (classify_boundary_profile(tr.vaddr, size_bytes) != profile) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("boundary classify mismatch uid=%0d target=%s actual=%s vaddr=0x%0h size=%0d",
                             tr.uid,
                             boundary_profile_name(profile),
                             boundary_profile_name(classify_boundary_profile(tr.vaddr, size_bytes)),
                             tr.vaddr,
                             size_bytes))
    end
endfunction:apply_boundary_addr_template

function bit [63:0] memblock_dispatch_base_sequence::gen_final_vaddr_by_profile(input memblock_boundary_profile_e profile,
                                                                                 input int unsigned size_bytes,
                                                                                 input memblock_op_class_e op_class);
    bit [63:0] base;
    bit [63:0] line_base;
    bit [63:0] page_base;
    int unsigned offset;
    int unsigned k;
    int unsigned bank;
    int unsigned line;
    longint unsigned page_count;

    if (size_bytes == 0) begin
        `uvm_fatal(get_type_name(), "gen_final_vaddr_by_profile got zero size")
    end

    case (profile)
        MEMBLOCK_BOUNDARY_PROFILE_ALIGNED: begin
            bit [63:0] align_bytes;
            align_bytes = (size_bytes >= 64) ? 64 : size_bytes;
            if (align_bytes == 0) begin
                align_bytes = 1;
            end
            return random_aligned_vaddr(align_bytes);
        end
        MEMBLOCK_BOUNDARY_PROFILE_MISALIGN_WITHIN_8B: begin
            if (!(size_bytes inside {2, 4})) begin
                `uvm_fatal(get_type_name(), $sformatf("MISALIGN_WITHIN_8B illegal size=%0d", size_bytes))
            end
            base = random_aligned_vaddr(8);
            if (size_bytes == 2) begin
                case ($urandom_range(2, 0))
                    0: offset = 1;
                    1: offset = 3;
                    default: offset = 5;
                endcase
            end else begin
                offset = $urandom_range(3, 1);
            end
            return base + offset;
        end
        MEMBLOCK_BOUNDARY_PROFILE_CROSS_8B_WITHIN_16B: begin
            int unsigned k_min;
            if (!(size_bytes inside {2, 4, 8})) begin
                `uvm_fatal(get_type_name(), $sformatf("CROSS_8B_WITHIN_16B illegal size=%0d", size_bytes))
            end
            base = random_aligned_vaddr(16);
            k_min = (size_bytes > 8) ? (size_bytes - 8) : 1;
            k = $urandom_range(size_bytes - 1, k_min);
            return base + 8 - k;
        end
        MEMBLOCK_BOUNDARY_PROFILE_CROSS_16B_SAME_LINE: begin
            if (!(size_bytes inside {2, 4, 8})) begin
                `uvm_fatal(get_type_name(), $sformatf("CROSS_16B_SAME_LINE illegal size=%0d", size_bytes))
            end
            line_base = random_aligned_vaddr(64);
            bank = $urandom_range(2, 0);
            k = $urandom_range(size_bytes - 1, 1);
            return line_base + bank * 16 + 16 - k;
        end
        MEMBLOCK_BOUNDARY_PROFILE_CROSS_CACHELINE_SAME_4K: begin
            if (!(size_bytes inside {2, 4, 8})) begin
                `uvm_fatal(get_type_name(), $sformatf("CROSS_CACHELINE_SAME_4K illegal size=%0d", size_bytes))
            end
            page_base = random_aligned_vaddr(4096);
            line = $urandom_range(62, 0);
            k = $urandom_range(size_bytes - 1, 1);
            return page_base + line * 64 + 64 - k;
        end
        MEMBLOCK_BOUNDARY_PROFILE_CROSS_4K: begin
            if (!(size_bytes inside {2, 4, 8})) begin
                `uvm_fatal(get_type_name(), $sformatf("CROSS_4K illegal size=%0d", size_bytes))
            end
            k = $urandom_range(size_bytes - 1, 1);
            page_count = (64'h0000_0080_0000_0000 - size_bytes + k) >> 12;
            if (page_count == 0) begin
                `uvm_fatal(get_type_name(), $sformatf("CROSS_4K no legal page for size=%0d k=%0d", size_bytes, k))
            end
            page_base = (random64() % page_count) << 12;
            return page_base + 4096 - k;
        end
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("unsupported boundary profile=%0d", profile))
        end
    endcase

    return '0;
endfunction:gen_final_vaddr_by_profile

function void memblock_dispatch_base_sequence::check_boundary_profile(input main_control_transaction tr,
                                                                      input string caller);
    memblock_boundary_profile_e actual;

    if (tr == null) begin
        `uvm_fatal(get_type_name(), $sformatf("%s got null transaction", caller))
    end
    if (!seq_csr_common::get_boundary_profile_gen_en()) begin
        return;
    end
    actual = classify_boundary_profile(tr.vaddr, tr.boundary_size_bytes);
    if (actual != tr.boundary_profile) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("%s uid=%0d boundary_profile mismatch target=%s actual=%s vaddr=0x%0h size=%0d",
                             caller,
                             tr.uid,
                             boundary_profile_name(tr.boundary_profile),
                             boundary_profile_name(actual),
                             tr.vaddr,
                             tr.boundary_size_bytes))
    end
endfunction:check_boundary_profile

function memblock_boundary_profile_e memblock_dispatch_base_sequence::classify_boundary_profile(input bit [63:0] vaddr,
                                                                                                input int unsigned size_bytes);
    bit [63:0] end_vaddr;
    bit [63:0] size_minus_one;

    if (size_bytes == 0) begin
        return MEMBLOCK_BOUNDARY_PROFILE_UNKNOWN;
    end
    size_minus_one = size_bytes - 1;
    end_vaddr = vaddr + size_minus_one;
    if (end_vaddr < vaddr) begin
        return MEMBLOCK_BOUNDARY_PROFILE_UNKNOWN;
    end
    if ((vaddr >> 12) != (end_vaddr >> 12)) begin
        return MEMBLOCK_BOUNDARY_PROFILE_CROSS_4K;
    end
    if ((vaddr >> 6) != (end_vaddr >> 6)) begin
        return MEMBLOCK_BOUNDARY_PROFILE_CROSS_CACHELINE_SAME_4K;
    end
    if ((vaddr >> 4) != (end_vaddr >> 4)) begin
        return MEMBLOCK_BOUNDARY_PROFILE_CROSS_16B_SAME_LINE;
    end
    if ((vaddr >> 3) != (end_vaddr >> 3)) begin
        return MEMBLOCK_BOUNDARY_PROFILE_CROSS_8B_WITHIN_16B;
    end
    if ((vaddr % size_bytes) == 0) begin
        return MEMBLOCK_BOUNDARY_PROFILE_ALIGNED;
    end
    return MEMBLOCK_BOUNDARY_PROFILE_MISALIGN_WITHIN_8B;
endfunction:classify_boundary_profile

function bit memblock_dispatch_base_sequence::boundary_profile_supported_for_op(input memblock_op_class_e op_class,
                                                                                input memblock_boundary_profile_e profile);
    case (op_class)
        MEMBLOCK_OP_CLASS_INT_LOAD,
        MEMBLOCK_OP_CLASS_FP_LOAD,
        MEMBLOCK_OP_CLASS_STORE: begin
            return profile == MEMBLOCK_BOUNDARY_PROFILE_ALIGNED ||
                   profile == MEMBLOCK_BOUNDARY_PROFILE_MISALIGN_WITHIN_8B ||
                   profile == MEMBLOCK_BOUNDARY_PROFILE_CROSS_8B_WITHIN_16B ||
                   profile == MEMBLOCK_BOUNDARY_PROFILE_CROSS_16B_SAME_LINE ||
                   profile == MEMBLOCK_BOUNDARY_PROFILE_CROSS_CACHELINE_SAME_4K ||
                   profile == MEMBLOCK_BOUNDARY_PROFILE_CROSS_4K;
        end
        MEMBLOCK_OP_CLASS_PREFETCH,
        MEMBLOCK_OP_CLASS_CBO,
        MEMBLOCK_OP_CLASS_AMO: begin
            return profile == MEMBLOCK_BOUNDARY_PROFILE_ALIGNED;
        end
        default: begin
            return 1'b0;
        end
    endcase
endfunction:boundary_profile_supported_for_op

function bit memblock_dispatch_base_sequence::boundary_profile_supported_for_fuop(input memblock_op_class_e op_class,
                                                                                  input bit [8:0] fuOpType,
                                                                                  input memblock_boundary_profile_e profile,
                                                                                  input int unsigned size_bytes);
    if (!boundary_profile_supported_for_op(op_class, profile) || size_bytes == 0) begin
        return 1'b0;
    end

    case (profile)
        MEMBLOCK_BOUNDARY_PROFILE_ALIGNED: begin
            return 1'b1;
        end
        MEMBLOCK_BOUNDARY_PROFILE_MISALIGN_WITHIN_8B: begin
            return size_bytes == 2 || size_bytes == 4;
        end
        MEMBLOCK_BOUNDARY_PROFILE_CROSS_8B_WITHIN_16B,
        MEMBLOCK_BOUNDARY_PROFILE_CROSS_16B_SAME_LINE,
        MEMBLOCK_BOUNDARY_PROFILE_CROSS_CACHELINE_SAME_4K,
        MEMBLOCK_BOUNDARY_PROFILE_CROSS_4K: begin
            return size_bytes == 2 || size_bytes == 4 || size_bytes == 8;
        end
        default: begin
            return 1'b0;
        end
    endcase
endfunction:boundary_profile_supported_for_fuop

function void memblock_dispatch_base_sequence::enumerate_boundary_profiles(ref memblock_boundary_profile_e profiles[$]);
    profiles.delete();
    profiles.push_back(MEMBLOCK_BOUNDARY_PROFILE_ALIGNED);
    profiles.push_back(MEMBLOCK_BOUNDARY_PROFILE_MISALIGN_WITHIN_8B);
    profiles.push_back(MEMBLOCK_BOUNDARY_PROFILE_CROSS_8B_WITHIN_16B);
    profiles.push_back(MEMBLOCK_BOUNDARY_PROFILE_CROSS_16B_SAME_LINE);
    profiles.push_back(MEMBLOCK_BOUNDARY_PROFILE_CROSS_CACHELINE_SAME_4K);
    profiles.push_back(MEMBLOCK_BOUNDARY_PROFILE_CROSS_4K);
endfunction:enumerate_boundary_profiles

function void memblock_dispatch_base_sequence::enumerate_op_classes(ref memblock_op_class_e op_classes[$]);
    op_classes.delete();
    op_classes.push_back(MEMBLOCK_OP_CLASS_INT_LOAD);
    op_classes.push_back(MEMBLOCK_OP_CLASS_FP_LOAD);
    op_classes.push_back(MEMBLOCK_OP_CLASS_STORE);
    op_classes.push_back(MEMBLOCK_OP_CLASS_PREFETCH);
    op_classes.push_back(MEMBLOCK_OP_CLASS_AMO);
    op_classes.push_back(MEMBLOCK_OP_CLASS_CBO);
endfunction:enumerate_op_classes

function void memblock_dispatch_base_sequence::enumerate_fuoptypes(input memblock_op_class_e op_class,
                                                                   ref bit [8:0] fuops[$]);
    fuops.delete();
    case (op_class)
        MEMBLOCK_OP_CLASS_INT_LOAD,
        MEMBLOCK_OP_CLASS_FP_LOAD: begin
            fuops.push_back(MEMBLOCK_LSUOP_LB);
            fuops.push_back(MEMBLOCK_LSUOP_LH);
            fuops.push_back(MEMBLOCK_LSUOP_LW);
            fuops.push_back(MEMBLOCK_LSUOP_LD);
            fuops.push_back(MEMBLOCK_LSUOP_LBU);
            fuops.push_back(MEMBLOCK_LSUOP_LHU);
            fuops.push_back(MEMBLOCK_LSUOP_LWU);
        end
        MEMBLOCK_OP_CLASS_STORE: begin
            fuops.push_back(MEMBLOCK_LSUOP_SB);
            fuops.push_back(MEMBLOCK_LSUOP_SH);
            fuops.push_back(MEMBLOCK_LSUOP_SW);
            fuops.push_back(MEMBLOCK_LSUOP_SD);
        end
        MEMBLOCK_OP_CLASS_PREFETCH: begin
            fuops.push_back(MEMBLOCK_LSUOP_PREFETCH_I);
            fuops.push_back(MEMBLOCK_LSUOP_PREFETCH_R);
            fuops.push_back(MEMBLOCK_LSUOP_PREFETCH_W);
        end
        MEMBLOCK_OP_CLASS_CBO: begin
            fuops.push_back(MEMBLOCK_LSUOP_CBO_ZERO);
            fuops.push_back(MEMBLOCK_LSUOP_CBO_CLEAN);
            fuops.push_back(MEMBLOCK_LSUOP_CBO_FLUSH);
            fuops.push_back(MEMBLOCK_LSUOP_CBO_INVAL);
        end
        MEMBLOCK_OP_CLASS_AMO: begin
            fuops.push_back(MEMBLOCK_LSUOP_LR_W);
            fuops.push_back(MEMBLOCK_LSUOP_SC_W);
            fuops.push_back(MEMBLOCK_LSUOP_AMOSWAP_W);
            fuops.push_back(MEMBLOCK_LSUOP_AMOADD_W);
            fuops.push_back(MEMBLOCK_LSUOP_AMOXOR_W);
            fuops.push_back(MEMBLOCK_LSUOP_AMOAND_W);
            fuops.push_back(MEMBLOCK_LSUOP_AMOOR_W);
            fuops.push_back(MEMBLOCK_LSUOP_AMOMIN_W);
            fuops.push_back(MEMBLOCK_LSUOP_AMOMAX_W);
            fuops.push_back(MEMBLOCK_LSUOP_AMOMINU_W);
            fuops.push_back(MEMBLOCK_LSUOP_AMOMAXU_W);
            fuops.push_back(MEMBLOCK_LSUOP_LR_D);
            fuops.push_back(MEMBLOCK_LSUOP_SC_D);
            fuops.push_back(MEMBLOCK_LSUOP_AMOSWAP_D);
            fuops.push_back(MEMBLOCK_LSUOP_AMOADD_D);
            fuops.push_back(MEMBLOCK_LSUOP_AMOXOR_D);
            fuops.push_back(MEMBLOCK_LSUOP_AMOAND_D);
            fuops.push_back(MEMBLOCK_LSUOP_AMOOR_D);
            fuops.push_back(MEMBLOCK_LSUOP_AMOMIN_D);
            fuops.push_back(MEMBLOCK_LSUOP_AMOMAX_D);
            fuops.push_back(MEMBLOCK_LSUOP_AMOMINU_D);
            fuops.push_back(MEMBLOCK_LSUOP_AMOMAXU_D);
        end
        default: begin
        end
    endcase
endfunction:enumerate_fuoptypes

function bit memblock_dispatch_base_sequence::fuop_belongs_to_op_class(input memblock_op_class_e op_class,
                                                                       input bit [8:0] fuOpType);
    bit [8:0] fuops[$];

    enumerate_fuoptypes(op_class, fuops);
    foreach (fuops[i]) begin
        if (fuops[i] == fuOpType) begin
            return 1'b1;
        end
    end
    return 1'b0;
endfunction:fuop_belongs_to_op_class

function int unsigned memblock_dispatch_base_sequence::get_op_class_weight(input memblock_op_class_e op_class);
    case (op_class)
        MEMBLOCK_OP_CLASS_INT_LOAD: return seq_csr_common::get_op_class_int_load_wt();
        MEMBLOCK_OP_CLASS_FP_LOAD:  return seq_csr_common::get_op_class_fp_load_wt();
        MEMBLOCK_OP_CLASS_STORE:    return seq_csr_common::get_op_class_store_wt();
        MEMBLOCK_OP_CLASS_PREFETCH: return seq_csr_common::get_op_class_prefetch_wt();
        MEMBLOCK_OP_CLASS_AMO:      return seq_csr_common::get_op_class_amo_wt();
        MEMBLOCK_OP_CLASS_CBO:      return seq_csr_common::get_op_class_cbo_wt();
        default:                    return 0;
    endcase
endfunction:get_op_class_weight

function int unsigned memblock_dispatch_base_sequence::get_fuop_weight(input memblock_op_class_e op_class,
                                                                       input bit [8:0] fuOpType);
    return seq_csr_common::get_fuop_weight(op_class, fuOpType);
endfunction:get_fuop_weight

function bit [8:0] memblock_dispatch_base_sequence::pick_fuop_by_weight(input memblock_op_class_e op_class,
                                                                        input string caller);
    bit [8:0] fuops[$];
    bit [8:0] legal_fuops[$];
    int unsigned weights[$];
    int idx;

    enumerate_fuoptypes(op_class, fuops);
    foreach (fuops[i]) begin
        int unsigned weight;

        weight = get_fuop_weight(op_class, fuops[i]);
        if (weight != 0) begin
            legal_fuops.push_back(fuops[i]);
            weights.push_back(weight);
        end
    end

    if (legal_fuops.size() == 0) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("%s op_class=%s has no fuOpType with non-zero weight",
                             caller, op_class_name(op_class)))
    end

    idx = weighted_pick_index(weights);
    return legal_fuops[idx];
endfunction:pick_fuop_by_weight

function bit [8:0] memblock_dispatch_base_sequence::default_fuop_by_op_profile(input memblock_op_class_e op_class,
                                                                               input memblock_boundary_profile_e profile);
    case (op_class)
        MEMBLOCK_OP_CLASS_INT_LOAD,
        MEMBLOCK_OP_CLASS_FP_LOAD: begin
            if (profile == MEMBLOCK_BOUNDARY_PROFILE_MISALIGN_WITHIN_8B) begin
                return MEMBLOCK_LSUOP_LW;
            end
            return MEMBLOCK_LSUOP_LD;
        end
        MEMBLOCK_OP_CLASS_STORE: begin
            if (profile == MEMBLOCK_BOUNDARY_PROFILE_MISALIGN_WITHIN_8B) begin
                return MEMBLOCK_LSUOP_SW;
            end
            return MEMBLOCK_LSUOP_SD;
        end
        MEMBLOCK_OP_CLASS_PREFETCH: return MEMBLOCK_LSUOP_PREFETCH_R;
        MEMBLOCK_OP_CLASS_CBO:      return MEMBLOCK_LSUOP_CBO_CLEAN;
        MEMBLOCK_OP_CLASS_AMO:      return MEMBLOCK_LSUOP_AMOADD_D;
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("no default fuOpType for op_class=%0d profile=%0d", op_class, profile))
        end
    endcase
    return '0;
endfunction:default_fuop_by_op_profile

function bit [8:0] memblock_dispatch_base_sequence::default_fuop_by_op_class_and_size(input memblock_op_class_e op_class,
                                                                                      input int unsigned size_bytes);
    case (op_class)
        MEMBLOCK_OP_CLASS_INT_LOAD,
        MEMBLOCK_OP_CLASS_FP_LOAD: begin
            case (size_bytes)
                1: return MEMBLOCK_LSUOP_LB;
                2: return MEMBLOCK_LSUOP_LH;
                4: return MEMBLOCK_LSUOP_LW;
                8: return MEMBLOCK_LSUOP_LD;
                default: begin
                    `uvm_fatal(get_type_name(), $sformatf("no default load fuOpType for size=%0d", size_bytes))
                end
            endcase
        end
        MEMBLOCK_OP_CLASS_STORE: begin
            case (size_bytes)
                1: return MEMBLOCK_LSUOP_SB;
                2: return MEMBLOCK_LSUOP_SH;
                4: return MEMBLOCK_LSUOP_SW;
                8: return MEMBLOCK_LSUOP_SD;
                default: begin
                    `uvm_fatal(get_type_name(), $sformatf("no default store fuOpType for size=%0d", size_bytes))
                end
            endcase
        end
        MEMBLOCK_OP_CLASS_PREFETCH: begin
            if (size_bytes == 64) begin
                return MEMBLOCK_LSUOP_PREFETCH_R;
            end
        end
        MEMBLOCK_OP_CLASS_CBO: begin
            if (size_bytes == 64) begin
                return MEMBLOCK_LSUOP_CBO_CLEAN;
            end
        end
        MEMBLOCK_OP_CLASS_AMO: begin
            case (size_bytes)
                4: return MEMBLOCK_LSUOP_AMOADD_W;
                8: return MEMBLOCK_LSUOP_AMOADD_D;
                default: begin
                    `uvm_fatal(get_type_name(), $sformatf("no default AMO fuOpType for size=%0d", size_bytes))
                end
            endcase
        end
        default: begin
        end
    endcase
    `uvm_fatal(get_type_name(), $sformatf("no default fuOpType for op_class=%s size=%0d",
                                          op_class_name(op_class), size_bytes))
    return '0;
endfunction:default_fuop_by_op_class_and_size

function bit [8:0] memblock_dispatch_base_sequence::choose_fuop_by_op_class_and_size(input memblock_op_class_e op_class,
                                                                                     input int unsigned size_bytes,
                                                                                     input string caller);
    bit [8:0] fuops[$];
    bit [8:0] legal_fuops[$];
    int unsigned weights[$];
    int unsigned legal_count;
    bit [8:0] default_fuop;
    int idx;

    enumerate_fuoptypes(op_class, fuops);
    legal_count = 0;
    foreach (fuops[i]) begin
        if (derive_size_bytes(op_class, fuops[i]) == size_bytes) begin
            int unsigned weight;

            legal_count++;
            weight = get_fuop_weight(op_class, fuops[i]);
            if (weight != 0) begin
                legal_fuops.push_back(fuops[i]);
                weights.push_back(weight);
            end
        end
    end

    if (legal_count == 0) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("%s op_class=%s has no legal fuOpType with size=%0d",
                             caller, op_class_name(op_class), size_bytes))
    end

    if (legal_fuops.size() == 0) begin
        `uvm_error(get_type_name(),
                   $sformatf("%s op_class=%s size=%0d legal fuOpType weights are all zero, use default fuOpType",
                             caller, op_class_name(op_class), size_bytes))
        default_fuop = default_fuop_by_op_class_and_size(op_class, size_bytes);
        if (!fuop_belongs_to_op_class(op_class, default_fuop) ||
            derive_size_bytes(op_class, default_fuop) != size_bytes) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("%s default fuOpType illegal op_class=%s fuOpType=0x%0h size=%0d",
                                 caller, op_class_name(op_class), default_fuop, size_bytes))
        end
        return default_fuop;
    end

    idx = weighted_pick_index(weights);
    return legal_fuops[idx];
endfunction:choose_fuop_by_op_class_and_size

function int unsigned memblock_dispatch_base_sequence::derive_size_bytes(input memblock_op_class_e op_class,
                                                                         input bit [8:0] fuOpType);
    case (op_class)
        MEMBLOCK_OP_CLASS_INT_LOAD,
        MEMBLOCK_OP_CLASS_FP_LOAD: begin
            case (fuOpType)
                MEMBLOCK_LSUOP_LB,
                MEMBLOCK_LSUOP_LBU: return 1;
                MEMBLOCK_LSUOP_LH,
                MEMBLOCK_LSUOP_LHU: return 2;
                MEMBLOCK_LSUOP_LW,
                MEMBLOCK_LSUOP_LWU: return 4;
                MEMBLOCK_LSUOP_LD:  return 8;
                default:            return 0;
            endcase
        end
        MEMBLOCK_OP_CLASS_STORE: begin
            case (fuOpType)
                MEMBLOCK_LSUOP_SB: return 1;
                MEMBLOCK_LSUOP_SH: return 2;
                MEMBLOCK_LSUOP_SW: return 4;
                MEMBLOCK_LSUOP_SD: return 8;
                default:           return 0;
            endcase
        end
        MEMBLOCK_OP_CLASS_PREFETCH,
        MEMBLOCK_OP_CLASS_CBO: begin
            return 64;
        end
        MEMBLOCK_OP_CLASS_AMO: begin
            if (fuOpType == MEMBLOCK_LSUOP_AMOCAS_Q) begin
                return 16;
            end
            if (fuOpType[1:0] == 2'b10) begin
                return 4;
            end
            return 8;
        end
        default: begin
            return 0;
        end
    endcase
endfunction:derive_size_bytes

function int memblock_dispatch_base_sequence::weighted_pick_index(input int unsigned weights[$]);
    int unsigned total;
    int unsigned pick;
    int unsigned acc;

    total = 0;
    foreach (weights[i]) begin
        total += weights[i];
    end
    if (total == 0) begin
        `uvm_fatal(get_type_name(), "weighted_pick_index got total weight zero")
    end
    pick = $urandom_range(total - 1, 0);
    acc = 0;
    foreach (weights[i]) begin
        acc += weights[i];
        if (pick < acc) begin
            return i;
        end
    end
    `uvm_fatal(get_type_name(), "weighted_pick_index reached unreachable state")
    return -1;
endfunction:weighted_pick_index

function bit memblock_dispatch_base_sequence::is_sv39_positive_canonical(input bit [63:0] vaddr);
    return vaddr[63:39] == '0;
endfunction:is_sv39_positive_canonical

function bit [63:0] memblock_dispatch_base_sequence::random64();
    return {$urandom(), $urandom()};
endfunction:random64

function bit [63:0] memblock_dispatch_base_sequence::random_aligned_vaddr(input bit [63:0] align_bytes);
    bit [63:0] vaddr_limit;
    bit [63:0] slot_count;
    bit [63:0] slot_pick;

    if (align_bytes == 0) begin
        `uvm_fatal(get_type_name(), "random_aligned_vaddr got zero alignment")
    end
    vaddr_limit = 64'h0000_0080_0000_0000;
    slot_count = vaddr_limit / align_bytes;
    if (slot_count == 0) begin
        `uvm_fatal(get_type_name(), $sformatf("random_aligned_vaddr align=%0d leaves no slot", align_bytes))
    end
    slot_pick = random64() % slot_count;
    return slot_pick * align_bytes;
endfunction:random_aligned_vaddr

function bit [63:0] memblock_dispatch_base_sequence::random_negative_imm12();
    bit [11:0] imm12;

    imm12 = $urandom_range(12'hfff, 12'h800);
    return {52'h0, imm12};
endfunction:random_negative_imm12

function string memblock_dispatch_base_sequence::boundary_profile_name(input memblock_boundary_profile_e profile);
    case (profile)
        MEMBLOCK_BOUNDARY_PROFILE_ALIGNED:                 return "ALIGNED";
        MEMBLOCK_BOUNDARY_PROFILE_MISALIGN_WITHIN_8B:      return "MISALIGN_WITHIN_8B";
        MEMBLOCK_BOUNDARY_PROFILE_CROSS_8B_WITHIN_16B:     return "CROSS_8B_WITHIN_16B";
        MEMBLOCK_BOUNDARY_PROFILE_CROSS_16B_SAME_LINE:     return "CROSS_16B_SAME_LINE";
        MEMBLOCK_BOUNDARY_PROFILE_CROSS_CACHELINE_SAME_4K: return "CROSS_CACHELINE_SAME_4K";
        MEMBLOCK_BOUNDARY_PROFILE_CROSS_4K:                return "CROSS_4K";
        default:                                           return "UNKNOWN";
    endcase
endfunction:boundary_profile_name

function string memblock_dispatch_base_sequence::op_class_name(input memblock_op_class_e op_class);
    case (op_class)
        MEMBLOCK_OP_CLASS_INT_LOAD: return "INT_LOAD";
        MEMBLOCK_OP_CLASS_FP_LOAD:  return "FP_LOAD";
        MEMBLOCK_OP_CLASS_STORE:    return "STORE";
        MEMBLOCK_OP_CLASS_PREFETCH: return "PREFETCH";
        MEMBLOCK_OP_CLASS_AMO:      return "AMO";
        MEMBLOCK_OP_CLASS_CBO:      return "CBO";
        default:                    return "UNKNOWN";
    endcase
endfunction:op_class_name


function bit [8:0] memblock_dispatch_base_sequence::random_load_fuoptype();
    return pick_fuop_by_weight(MEMBLOCK_OP_CLASS_INT_LOAD, "random_load_fuoptype");
endfunction:random_load_fuoptype

function bit [8:0] memblock_dispatch_base_sequence::random_store_fuoptype();
    return pick_fuop_by_weight(MEMBLOCK_OP_CLASS_STORE, "random_store_fuoptype");
endfunction:random_store_fuoptype

function bit [8:0] memblock_dispatch_base_sequence::random_prefetch_fuoptype();
    return pick_fuop_by_weight(MEMBLOCK_OP_CLASS_PREFETCH, "random_prefetch_fuoptype");
endfunction:random_prefetch_fuoptype

function bit [8:0] memblock_dispatch_base_sequence::random_cbo_fuoptype();
    return pick_fuop_by_weight(MEMBLOCK_OP_CLASS_CBO, "random_cbo_fuoptype");
endfunction:random_cbo_fuoptype

function bit [8:0] memblock_dispatch_base_sequence::random_amo_fuoptype();
    return pick_fuop_by_weight(MEMBLOCK_OP_CLASS_AMO, "random_amo_fuoptype");
endfunction:random_amo_fuoptype

function memblock_rob_key_t memblock_dispatch_base_sequence::choose_rob_start_key();
    memblock_rob_key_t key;
    int unsigned sel;
    int unsigned mid_lo;
    int unsigned mid_hi;
    int unsigned near_lo;

    key.flag = 1'b0;
    if (seq_csr_common::get_rob_start_fixed_en()) begin
        key.value = seq_csr_common::get_rob_start_fixed_value();
        return key;
    end

    sel = rand_weighted3(seq_csr_common::get_rob_start_zero_wt(),
                         seq_csr_common::get_rob_start_mid_wt(),
                         seq_csr_common::get_rob_start_near_wrap_wt());
    case (sel)
        0: key.value = '0;
        1: begin
            mid_lo = MEMBLOCK_ROB_SIZE / 4;
            mid_hi = (MEMBLOCK_ROB_SIZE * 3) / 4;
            if (mid_hi <= mid_lo) begin
                key.value = mid_lo;
            end else begin
                key.value = $urandom_range(mid_hi, mid_lo);
            end
        end
        default: begin
            near_lo = (MEMBLOCK_ROB_SIZE > 8) ? (MEMBLOCK_ROB_SIZE - 8) : 0;
            key.value = $urandom_range(MEMBLOCK_ROB_SIZE - 1, near_lo);
        end
    endcase
    return key;
endfunction:choose_rob_start_key

function int unsigned memblock_dispatch_base_sequence::choose_addr_ref_window();
    int unsigned fixed_window;
    int unsigned max_window;
    int unsigned sel;
    int unsigned lo;
    int unsigned hi;

    fixed_window = seq_csr_common::get_addr_ref_window_fixed();
    max_window = seq_csr_common::get_addr_ref_window_max();
    if (fixed_window > 0) begin
        return fixed_window;
    end

    sel = rand_weighted3(seq_csr_common::get_addr_ref_window_small_weight(),
                         seq_csr_common::get_addr_ref_window_medium_weight(),
                         seq_csr_common::get_addr_ref_window_large_weight());
    case (sel)
        0: begin
            lo = 1;
            hi = max_window / 4;
        end
        1: begin
            lo = max_window / 4;
            hi = max_window / 2;
        end
        default: begin
            lo = max_window / 2;
            hi = max_window;
        end
    endcase

    if (lo < 1) begin
        lo = 1;
    end
    if (hi < lo) begin
        hi = lo;
    end
    if (hi > max_window) begin
        hi = max_window;
    end
    return $urandom_range(hi, lo);
endfunction:choose_addr_ref_window

function memblock_addr_reuse_kind_e memblock_dispatch_base_sequence::select_addr_reuse_kind();
    int unsigned sel;

    sel = rand_weighted4(seq_csr_common::get_addr_reuse_load_after_store_wt(),
                         seq_csr_common::get_addr_reuse_load_after_load_wt(),
                         seq_csr_common::get_addr_reuse_store_after_load_wt(),
                         seq_csr_common::get_addr_reuse_store_after_store_wt());
    case (sel)
        0: return MEMBLOCK_ADDR_REUSE_LOAD_AFTER_STORE;
        1: return MEMBLOCK_ADDR_REUSE_LOAD_AFTER_LOAD;
        2: return MEMBLOCK_ADDR_REUSE_STORE_AFTER_LOAD;
        default: return MEMBLOCK_ADDR_REUSE_STORE_AFTER_STORE;
    endcase
endfunction:select_addr_reuse_kind

function void memblock_dispatch_base_sequence::prune_recent_uid_q(ref memblock_uid_t uid_q[$],
                                                                  input memblock_uid_t cur_uid,
                                                                  input int unsigned addr_ref_window);
    while (uid_q.size() > 0) begin
        if ((cur_uid - uid_q[0]) > addr_ref_window) begin
            uid_q.pop_front();
        end else begin
            break;
        end
    end
endfunction:prune_recent_uid_q

function bit memblock_dispatch_base_sequence::random_pick_recent_uid(ref memblock_uid_t uid_q[$],
                                                                     output memblock_uid_t ref_uid,
                                                                     input bit delete_after_pick);
    int unsigned idx;

    ref_uid = '0;
    if (uid_q.size() == 0) begin
        return 1'b0;
    end
    idx = $urandom_range(uid_q.size() - 1, 0);
    ref_uid = uid_q[idx];
    if (delete_after_pick) begin
        uid_q.delete(idx);
    end
    return 1'b1;
endfunction:random_pick_recent_uid

function void memblock_dispatch_base_sequence::set_transaction_ls_kind(input main_control_transaction tr,
                                                                       input bit make_load);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "set_transaction_ls_kind got null transaction")
    end

    if (make_load) begin
        tr.op_class = MEMBLOCK_OP_CLASS_INT_LOAD;
    end else begin
        tr.op_class = MEMBLOCK_OP_CLASS_STORE;
    end
    apply_minimal_op_template(tr);
    tr.update_vaddr();
endfunction:set_transaction_ls_kind

function void memblock_dispatch_base_sequence::fixup_after_addr_reuse(input main_control_transaction tr,
                                                                      input main_control_transaction ref_tr,
                                                                      input bit copy_addr,
                                                                      input string caller);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), $sformatf("%s got null transaction", caller))
    end
    if (copy_addr) begin
        if (ref_tr == null) begin
            `uvm_fatal(get_type_name(), $sformatf("%s got null reference transaction", caller))
        end
        tr.src_0 = ref_tr.src_0;
        tr.imm   = ref_tr.imm;
    end
    tr.update_vaddr();
    validate_main_table_entry(tr, caller);
endfunction:fixup_after_addr_reuse

function void memblock_dispatch_base_sequence::apply_addr_reuse_window(input main_control_transaction tr,
                                                                       input memblock_uid_t cur_uid,
                                                                       ref memblock_uid_t recent_load_uid_q[$],
                                                                       ref memblock_uid_t recent_store_uid_q[$]);
    memblock_addr_reuse_kind_e kind;
    memblock_op_class_e target_op_class;
    memblock_op_class_e fallback_op_class;
    memblock_uid_t ref_uid;
    main_control_transaction ref_tr;
    string caller_prefix;
    string reuse_caller;
    string fallback_caller;
    bit got_ref;
    bit delete_after_pick;
    bit keep_ref_size;

    if (rand_weighted2(seq_csr_common::get_addr_reuse_en_1_wt(),
                       seq_csr_common::get_addr_reuse_en_0_wt()) != 0) begin
        return;
    end

    kind = select_addr_reuse_kind();
    case (kind)
        MEMBLOCK_ADDR_REUSE_LOAD_AFTER_STORE: begin
            target_op_class = MEMBLOCK_OP_CLASS_INT_LOAD;
            fallback_op_class = MEMBLOCK_OP_CLASS_STORE;
            caller_prefix = "load_after_store";
            delete_after_pick = 1'b0;
            got_ref = random_pick_recent_uid(recent_store_uid_q, ref_uid, delete_after_pick);
        end
        MEMBLOCK_ADDR_REUSE_LOAD_AFTER_LOAD: begin
            target_op_class = MEMBLOCK_OP_CLASS_INT_LOAD;
            fallback_op_class = MEMBLOCK_OP_CLASS_INT_LOAD;
            caller_prefix = "load_after_load";
            delete_after_pick = 1'b1;
            got_ref = random_pick_recent_uid(recent_load_uid_q, ref_uid, delete_after_pick);
        end
        MEMBLOCK_ADDR_REUSE_STORE_AFTER_LOAD: begin
            target_op_class = MEMBLOCK_OP_CLASS_STORE;
            fallback_op_class = MEMBLOCK_OP_CLASS_INT_LOAD;
            caller_prefix = "store_after_load";
            delete_after_pick = 1'b0;
            got_ref = random_pick_recent_uid(recent_load_uid_q, ref_uid, delete_after_pick);
        end
        MEMBLOCK_ADDR_REUSE_STORE_AFTER_STORE: begin
            target_op_class = MEMBLOCK_OP_CLASS_STORE;
            fallback_op_class = MEMBLOCK_OP_CLASS_STORE;
            caller_prefix = "store_after_store";
            delete_after_pick = 1'b1;
            got_ref = random_pick_recent_uid(recent_store_uid_q, ref_uid, delete_after_pick);
        end
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("unsupported addr reuse kind=%0d", kind))
        end
    endcase

    if (!got_ref) begin
        fallback_caller = $sformatf("%s fallback uid=%0d", caller_prefix, cur_uid);
        tr.op_class = fallback_op_class;
        apply_minimal_op_template(tr);
        fixup_after_addr_reuse(tr, null, 1'b0, fallback_caller);
        return;
    end

    ref_tr = data.get_main_transaction(ref_uid);
    reuse_caller = $sformatf("%s uid=%0d ref_uid=%0d", caller_prefix, cur_uid, ref_uid);
    keep_ref_size = rand_weighted2(seq_csr_common::get_addr_reuse_keep_ref_size_en_1_wt(),
                                   seq_csr_common::get_addr_reuse_keep_ref_size_en_0_wt()) == 0;

    if (!keep_ref_size || ref_tr.op_class == MEMBLOCK_OP_CLASS_PREFETCH) begin
        tr.op_class = target_op_class;
        apply_minimal_op_template(tr);
        fixup_after_addr_reuse(tr, ref_tr, 1'b1, reuse_caller);
        return;
    end

    begin
        int unsigned ref_size;
        bit [8:0] target_fuOpType;

        ref_size = derive_size_bytes(ref_tr.op_class, ref_tr.fuOpType);
        if (ref_size == 0) begin
            `uvm_fatal(get_type_name(),
                       $sformatf("%s ref uid=%0d has invalid size op_class=%s fuOpType=0x%0h",
                                 reuse_caller, ref_uid, op_class_name(ref_tr.op_class), ref_tr.fuOpType))
        end
        target_fuOpType = choose_fuop_by_op_class_and_size(target_op_class, ref_size, reuse_caller);
        tr.op_class = target_op_class;
        apply_op_class_template(tr, target_fuOpType);
        fixup_after_addr_reuse(tr, ref_tr, 1'b1, reuse_caller);
    end
endfunction:apply_addr_reuse_window

function void memblock_dispatch_base_sequence::sync_boundary_profile_after_addr_reuse(input main_control_transaction tr,
                                                                                      input string caller);
    int unsigned size_bytes;
    memblock_boundary_profile_e actual_profile;

    if (tr == null) begin
        `uvm_fatal(get_type_name(), $sformatf("%s got null transaction", caller))
    end
    if (!seq_csr_common::get_boundary_profile_gen_en()) begin
        return;
    end

    size_bytes = derive_size_bytes(tr.op_class, tr.fuOpType);
    if (size_bytes == 0) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("%s uid=%0d cannot derive boundary size op_class=%s fuOpType=0x%0h",
                             caller, tr.uid, op_class_name(tr.op_class), tr.fuOpType))
    end

    tr.update_vaddr();
    actual_profile = classify_boundary_profile(tr.vaddr, size_bytes);
    if (actual_profile == MEMBLOCK_BOUNDARY_PROFILE_UNKNOWN) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("%s uid=%0d cannot classify final boundary profile vaddr=0x%0h size=%0d",
                             caller, tr.uid, tr.vaddr, size_bytes))
    end

    tr.boundary_size_bytes = size_bytes;
    tr.boundary_profile = actual_profile;
    validate_main_table_entry(tr, caller);
endfunction:sync_boundary_profile_after_addr_reuse

function void memblock_dispatch_base_sequence::push_recent_uid(input main_control_transaction tr,
                                                               input memblock_uid_t uid,
                                                               ref memblock_uid_t recent_load_uid_q[$],
                                                               ref memblock_uid_t recent_store_uid_q[$]);
    if (is_load_main_tr(tr)) begin
        recent_load_uid_q.push_back(uid);
    end else if (is_store_main_tr(tr)) begin
        recent_store_uid_q.push_back(uid);
    end
endfunction:push_recent_uid

function int unsigned memblock_dispatch_base_sequence::randomize_send_pri_value(input bit is_std);
    int unsigned sel;

    if (!seq_csr_common::get_send_pri_mode_en()) begin
        if (is_std) begin
            return seq_csr_common::get_send_pri_std_default();
        end
        return seq_csr_common::get_send_pri_default();
    end

    if (is_std) begin
        sel = rand_weighted3(seq_csr_common::get_send_pri_std_low_wt(),
                             seq_csr_common::get_send_pri_std_mid_wt(),
                             seq_csr_common::get_send_pri_std_high_wt());
    end else begin
        sel = rand_weighted3(seq_csr_common::get_send_pri_low_wt(),
                             seq_csr_common::get_send_pri_mid_wt(),
                             seq_csr_common::get_send_pri_high_wt());
    end

    case (sel)
        0: return $urandom_range(33, 0);
        1: return $urandom_range(66, 34);
        default: return $urandom_range(100, 67);
    endcase
endfunction:randomize_send_pri_value

function int unsigned memblock_dispatch_base_sequence::randomize_delay_value();
    int unsigned sel;

    sel = rand_weighted3(seq_csr_common::get_delay_0_wt(),
                         seq_csr_common::get_delay_1_20_wt(),
                         seq_csr_common::get_delay_21_50_wt());
    case (sel)
        0: return 0;
        1: return $urandom_range(20, 1);
        default: return $urandom_range(50, 21);
    endcase
endfunction:randomize_delay_value

function bit memblock_dispatch_base_sequence::rand_percent_hit(input int unsigned percent);
    if (percent == 0) begin
        return 1'b0;
    end
    if (percent >= 100) begin
        return 1'b1;
    end
    return $urandom_range(99, 0) < percent;
endfunction:rand_percent_hit

function int unsigned memblock_dispatch_base_sequence::rand_weighted2(input int unsigned w0,
                                                                      input int unsigned w1);
    int unsigned total;
    int unsigned pick;

    total = w0 + w1;
    if (total == 0) begin
        `uvm_fatal(get_type_name(), "rand_weighted2 got all zero weights")
    end
    pick = $urandom_range(total - 1, 0);
    if (pick < w0) begin
        return 0;
    end
    return 1;
endfunction:rand_weighted2

function int unsigned memblock_dispatch_base_sequence::rand_weighted3(input int unsigned w0,
                                                                      input int unsigned w1,
                                                                      input int unsigned w2);
    int unsigned total;
    int unsigned pick;

    total = w0 + w1 + w2;
    if (total == 0) begin
        `uvm_fatal(get_type_name(), "rand_weighted3 got all zero weights")
    end
    pick = $urandom_range(total - 1, 0);
    if (pick < w0) begin
        return 0;
    end
    if (pick < (w0 + w1)) begin
        return 1;
    end
    return 2;
endfunction:rand_weighted3

function int unsigned memblock_dispatch_base_sequence::rand_weighted4(input int unsigned w0,
                                                                      input int unsigned w1,
                                                                      input int unsigned w2,
                                                                      input int unsigned w3);
    int unsigned total;
    int unsigned pick;
    int unsigned acc;

    total = w0 + w1 + w2 + w3;
    if (total == 0) begin
        `uvm_fatal(get_type_name(), "rand_weighted4 got all zero weights")
    end
    pick = $urandom_range(total - 1, 0);
    acc = w0;
    if (pick < acc) begin
        return 0;
    end
    acc += w1;
    if (pick < acc) begin
        return 1;
    end
    acc += w2;
    if (pick < acc) begin
        return 2;
    end
    return 3;
endfunction:rand_weighted4

function int unsigned memblock_dispatch_base_sequence::rand_weighted5(input int unsigned w0,
                                                                      input int unsigned w1,
                                                                      input int unsigned w2,
                                                                      input int unsigned w3,
                                                                      input int unsigned w4);
    int unsigned total;
    int unsigned pick;
    int unsigned acc;

    total = w0 + w1 + w2 + w3 + w4;
    if (total == 0) begin
        `uvm_fatal(get_type_name(), "rand_weighted5 got all zero weights")
    end
    pick = $urandom_range(total - 1, 0);
    acc = w0;
    if (pick < acc) begin
        return 0;
    end
    acc += w1;
    if (pick < acc) begin
        return 1;
    end
    acc += w2;
    if (pick < acc) begin
        return 2;
    end
    acc += w3;
    if (pick < acc) begin
        return 3;
    end
    return 4;
endfunction:rand_weighted5

function int unsigned memblock_dispatch_base_sequence::rand_weighted6(input int unsigned w0,
                                                                      input int unsigned w1,
                                                                      input int unsigned w2,
                                                                      input int unsigned w3,
                                                                      input int unsigned w4,
                                                                      input int unsigned w5);
    int unsigned total;
    int unsigned pick;
    int unsigned acc;

    total = w0 + w1 + w2 + w3 + w4 + w5;
    if (total == 0) begin
        `uvm_fatal(get_type_name(), "rand_weighted6 got all zero weights")
    end
    pick = $urandom_range(total - 1, 0);
    acc = w0;
    if (pick < acc) begin
        return 0;
    end
    acc += w1;
    if (pick < acc) begin
        return 1;
    end
    acc += w2;
    if (pick < acc) begin
        return 2;
    end
    acc += w3;
    if (pick < acc) begin
        return 3;
    end
    acc += w4;
    if (pick < acc) begin
        return 4;
    end
    return 5;
endfunction:rand_weighted6

function bit memblock_dispatch_base_sequence::is_load_main_tr(input main_control_transaction tr);
    if (tr == null) begin
        return 1'b0;
    end
    return tr.op_class == MEMBLOCK_OP_CLASS_INT_LOAD ||
           tr.op_class == MEMBLOCK_OP_CLASS_FP_LOAD  ||
           tr.op_class == MEMBLOCK_OP_CLASS_PREFETCH;
endfunction:is_load_main_tr

function bit memblock_dispatch_base_sequence::is_store_main_tr(input main_control_transaction tr);
    if (tr == null) begin
        return 1'b0;
    end
    return tr.op_class == MEMBLOCK_OP_CLASS_STORE;
endfunction:is_store_main_tr

function bit memblock_dispatch_base_sequence::is_vector_ls_main_tr(input main_control_transaction tr);
    if (tr == null) begin
        return 1'b0;
    end
    return tr.fuType == MEMBLOCK_FUTYPE_VLDU    ||
           tr.fuType == MEMBLOCK_FUTYPE_VSTU    ||
           tr.fuType == MEMBLOCK_FUTYPE_VSEGLDU ||
           tr.fuType == MEMBLOCK_FUTYPE_VSEGSTU;
endfunction:is_vector_ls_main_tr

function bit memblock_dispatch_base_sequence::is_load_fuoptype(input bit [8:0] fuOpType);
    return lsq_ctrl_model::is_load_fuoptype(fuOpType);
endfunction:is_load_fuoptype

function bit memblock_dispatch_base_sequence::is_store_fuoptype(input bit [8:0] fuOpType);
    return lsq_ctrl_model::is_store_fuoptype(fuOpType);
endfunction:is_store_fuoptype

function bit memblock_dispatch_base_sequence::is_prefetch_fuoptype(input bit [8:0] fuOpType);
    return lsq_ctrl_model::is_prefetch_fuoptype(fuOpType);
endfunction:is_prefetch_fuoptype

function bit memblock_dispatch_base_sequence::is_amo_fuoptype(input bit [8:0] fuOpType);
    return lsq_ctrl_model::is_amo_fuoptype(fuOpType);
endfunction:is_amo_fuoptype

function memblock_op_behavior_t memblock_dispatch_base_sequence::derive_op_behavior(input main_control_transaction tr);
    return lsq_ctrl_model::derive_op_behavior(tr);
endfunction:derive_op_behavior

function void memblock_dispatch_base_sequence::init_status_for_main_table();
    if (data == null || data.main_trans_num == 0) begin
        `uvm_fatal(get_type_name(), "init_status_for_main_table called before main table build")
    end
    for (int unsigned uid = 0; uid < data.main_trans_num; uid++) begin
        void'(data.init_status_for_uid(uid));
    end
endfunction:init_status_for_main_table

function void memblock_dispatch_base_sequence::validate_main_table_entry(input main_control_transaction tr,
                                                                         input string caller);
    if (tr == null) begin
        `uvm_fatal(get_type_name(), $sformatf("%s got null transaction", caller))
    end
    if (!tr.validate_main_transaction()) begin
        `uvm_fatal(get_type_name(), $sformatf("%s got invalid derived fields uid=%0d", caller, tr.uid))
    end
    if (tr.robIdx_value >= MEMBLOCK_ROB_SIZE) begin
        `uvm_fatal(get_type_name(), $sformatf("%s uid=%0d robIdx_value=%0d exceeds ROB size=%0d",
                                              caller, tr.uid, tr.robIdx_value, MEMBLOCK_ROB_SIZE))
    end
    if (is_vector_ls_main_tr(tr)) begin
        `uvm_fatal(get_type_name(), $sformatf("%s uid=%0d vector LS is not supported in Task5", caller, tr.uid))
    end
    begin
        memblock_op_behavior_t behavior;
        behavior = derive_op_behavior(tr);
        if (tr.numLsElem != behavior.num_ls_elem) begin
            `uvm_fatal(get_type_name(), $sformatf("%s uid=%0d numLsElem=%0d expected=%0d",
                                                  caller, tr.uid, tr.numLsElem, behavior.num_ls_elem))
        end
    end

    case (tr.op_class)
        MEMBLOCK_OP_CLASS_INT_LOAD,
        MEMBLOCK_OP_CLASS_FP_LOAD: begin
            if (tr.fuType != MEMBLOCK_FUTYPE_LDU ||
                tr.lsq_flow != MEMBLOCK_LSQ_FLOW_LOAD ||
                !is_load_fuoptype(tr.fuOpType)) begin
                `uvm_fatal(get_type_name(), $sformatf("%s uid=%0d has illegal load template", caller, tr.uid))
            end
        end
        MEMBLOCK_OP_CLASS_STORE: begin
            if (tr.fuType != MEMBLOCK_FUTYPE_STU ||
                tr.lsq_flow != MEMBLOCK_LSQ_FLOW_STORE ||
                !is_store_fuoptype(tr.fuOpType)) begin
                `uvm_fatal(get_type_name(), $sformatf("%s uid=%0d has illegal store template", caller, tr.uid))
            end
        end
        MEMBLOCK_OP_CLASS_CBO: begin
            if (tr.fuType != MEMBLOCK_FUTYPE_STU ||
                tr.lsq_flow != MEMBLOCK_LSQ_FLOW_CBO ||
                !lsq_ctrl_model::is_cbo_fuoptype(tr.fuOpType)) begin
                `uvm_fatal(get_type_name(), $sformatf("%s uid=%0d has illegal CBO template", caller, tr.uid))
            end
        end
        MEMBLOCK_OP_CLASS_PREFETCH: begin
            if (tr.fuType != MEMBLOCK_FUTYPE_LDU ||
                tr.lsq_flow != MEMBLOCK_LSQ_FLOW_LOAD ||
                !is_prefetch_fuoptype(tr.fuOpType)) begin
                `uvm_fatal(get_type_name(), $sformatf("%s uid=%0d has illegal prefetch template", caller, tr.uid))
            end
        end
        MEMBLOCK_OP_CLASS_AMO: begin
            if (tr.fuType != MEMBLOCK_FUTYPE_MOU ||
                tr.lsq_flow != MEMBLOCK_LSQ_FLOW_ATOMIC ||
                !is_amo_fuoptype(tr.fuOpType)) begin
                `uvm_fatal(get_type_name(), $sformatf("%s uid=%0d has illegal AMO template", caller, tr.uid))
            end
        end
        default: begin
            `uvm_fatal(get_type_name(), $sformatf("%s uid=%0d has unsupported op_class=%0d",
                                                  caller, tr.uid, tr.op_class))
        end
    endcase
endfunction:validate_main_table_entry

`endif
