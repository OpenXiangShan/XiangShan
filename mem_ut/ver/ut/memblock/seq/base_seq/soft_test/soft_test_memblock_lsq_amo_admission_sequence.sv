//=========================================================
//File name    : soft_test_memblock_lsq_amo_admission_sequence.sv
//Author       : OpenAI_Codex
//Module name  : soft_test_memblock_lsq_amo_admission_sequence
//Discribution : directed software checks for LSQ D/R/Q and AMO issue fields
//Date         : 2026-11-03
//=========================================================
`ifndef SOFT_TEST_MEMBLOCK_LSQ_AMO_ADMISSION_SEQUENCE__SV
`define SOFT_TEST_MEMBLOCK_LSQ_AMO_ADMISSION_SEQUENCE__SV

// 中文注释：仅在单次负向软件检查期间捕获指定 ID/消息的预期 fatal，其他错误照常抛出。
class memblock_cas_prefill_expected_fatal_catcher extends uvm_report_catcher;
    string expected_id;
    string expected_pattern;
    int unsigned caught_count;
    function new(string id, string pattern);
        super.new("cas_prefill_expected_fatal");
        expected_id = id;
        expected_pattern = pattern;
        caught_count = 0;
    endfunction
    virtual function action_e catch();
        if (get_severity() == UVM_FATAL && get_id() == expected_id &&
            uvm_is_match(expected_pattern, get_message())) begin
            caught_count++;
            return CAUGHT;
        end
        return THROW;
    endfunction
endclass

class soft_test_memblock_lsq_amo_admission_sequence extends memblock_dispatch_base_sequence;

    longint unsigned sample_seq;
    // 中文注释：software-only 虚函数定向入口；默认旁路，只在 prefill 检查期间设置并恢复。
    int directed_cas_distance = 0;
    bit directed_cas_generation = 0;
    bit [8:0] directed_cas_opcode = MEMBLOCK_LSUOP_AMOCAS_D;

    `uvm_object_utils(soft_test_memblock_lsq_amo_admission_sequence)

    function new(string name = "soft_test_memblock_lsq_amo_admission_sequence");
        super.new(name);
        sample_seq = 0;
    endfunction:new

    function void check_expect(input bit condition, input string message);
        if (!condition) begin
            `uvm_fatal("LSQ_AMO_SOFT", message)
        end
    endfunction:check_expect

    virtual function int unsigned randomize_cas_prefill_distance();
        if (directed_cas_distance != 0) return directed_cas_distance;
        return super.randomize_cas_prefill_distance();
    endfunction

    function main_control_transaction make_prefill_cas(input memblock_uid_t uid,
                                                       input bit [8:0] opcode);
        main_control_transaction tr;
        tr = main_control_transaction::type_id::create("soft_prefill_cas");
        tr.uid = uid;
        tr.op_class = MEMBLOCK_OP_CLASS_AMO;
        apply_op_class_template(tr, opcode);
        tr.src_0 = seq_csr_common::get_main_vaddr_base();
        tr.imm = 0;
        tr.update_vaddr();
        tr.amo_alignment_bytes = derive_size_bytes(tr.op_class, tr.fuOpType);
        tr.send_pri = 100;
        tr.send_pri_std = 100;
        return tr;
    endfunction

    virtual function void randomize_main_transaction(input main_control_transaction tr,
        input memblock_uid_t uid, input memblock_rob_key_t rob_key);
        if (!directed_cas_generation) begin
            super.randomize_main_transaction(tr, uid, rob_key);
            return;
        end
        tr.uid = uid;
        tr.op_class = MEMBLOCK_OP_CLASS_AMO;
        apply_op_class_template(tr, directed_cas_opcode);
        tr.robIdx_flag = rob_key.flag;
        tr.robIdx_value = rob_key.value;
        apply_legal_addr_template(tr);
        tr.amo_alignment_bytes = derive_size_bytes(tr.op_class, tr.fuOpType);
        tr.send_pri = 100;
        tr.send_pri_std = 100;
    endfunction

    function void reset_prefill_fixture();
        sc_reservation_by_uid.delete();
        cas_prefill_store_high_by_uid.delete();
        cas_prefill_owner_by_uid.delete();
        cas_prefill_group_audit_q.delete();
    endfunction

    // 抽象职责：核验输入解析层到冻结 getter 的链路及非法配置的精确 fail-fast。
    task check_cas_prefill_parameters();
        int saved_en, saved_weights[3];
        int unsigned ignored;
        memblock_cas_prefill_expected_fatal_catcher catcher;
        saved_en = plus::MEMBLOCK_AMO_CAS_PREFILL_EN_1_WT;
        saved_weights = '{plus::MEMBLOCK_AMO_CAS_PREFILL_DIST_1_5_WT,
                          plus::MEMBLOCK_AMO_CAS_PREFILL_DIST_6_10_WT,
                          plus::MEMBLOCK_AMO_CAS_PREFILL_DIST_11_30_WT};
        plus::MEMBLOCK_AMO_CAS_PREFILL_EN_1_WT = 100;
        plus::MEMBLOCK_AMO_CAS_PREFILL_DIST_1_5_WT = 2;
        plus::MEMBLOCK_AMO_CAS_PREFILL_DIST_6_10_WT = 3;
        plus::MEMBLOCK_AMO_CAS_PREFILL_DIST_11_30_WT = 4;
        seq_csr_common::load_from_plus();
        check_expect(seq_csr_common::get_amo_cas_prefill_en_1_wt() == 100 &&
            seq_csr_common::get_amo_cas_prefill_dist_weight(0) == 2 &&
            seq_csr_common::get_amo_cas_prefill_dist_weight(1) == 3 &&
            seq_csr_common::get_amo_cas_prefill_dist_weight(2) == 4, "prefill plus snapshot/getter mismatch");
        seq_csr_common::check_amo_cas_prefill_config();
        seq_csr_common::amo_cas_prefill_en_1_wt = 101;
        catcher = new("SEQ_CSR_CFG", "*PREFILL_EN_1_WT*");
        uvm_report_cb::add(null, catcher);
        seq_csr_common::check_amo_cas_prefill_config();
        uvm_report_cb::delete(null, catcher);
        check_expect(catcher.caught_count == 1, "prefill percent >100 was accepted");
        seq_csr_common::amo_cas_prefill_en_1_wt = 0;
        seq_csr_common::amo_cas_prefill_dist_wt = '{0, 0, 0};
        catcher = new("SEQ_CSR_CFG", "*weights must not all be zero*");
        uvm_report_cb::add(null, catcher);
        seq_csr_common::check_amo_cas_prefill_config();
        uvm_report_cb::delete(null, catcher);
        check_expect(catcher.caught_count == 1, "all-zero prefill weights were accepted");
        catcher = new("SEQ_CSR_CFG", "*must not be negative*");
        uvm_report_cb::add(null, catcher);
        ignored = seq_csr_common::get_non_negative_int("MEMBLOCK_AMO_CAS_PREFILL_DIST_1_5_WT", -1);
        uvm_report_cb::delete(null, catcher);
        check_expect(catcher.caught_count == 1, "negative prefill weight was accepted");
        seq_csr_common::amo_cas_prefill_dist_wt = '{1, 1, 1};
        seq_csr_common::amo_cas_prefill_en_1_wt = 100;
        seq_csr_common::l2tlb_ppn_reuse_en = 1;
        catcher = new("SEQ_CSR_CFG", "*does not support PPN reuse*");
        uvm_report_cb::add(null, catcher);
        seq_csr_common::check_amo_cas_prefill_config();
        seq_csr_common::l2tlb_ppn_reuse_en = 0;
        seq_csr_common::l2tlb_ppn_region_id = 1;
        seq_csr_common::check_amo_cas_prefill_config();
        uvm_report_cb::delete(null, catcher);
        check_expect(catcher.caught_count == 2, "PPN alias configurations were accepted");
        plus::MEMBLOCK_AMO_CAS_PREFILL_EN_1_WT = saved_en;
        plus::MEMBLOCK_AMO_CAS_PREFILL_DIST_1_5_WT = saved_weights[0];
        plus::MEMBLOCK_AMO_CAS_PREFILL_DIST_6_10_WT = saved_weights[1];
        plus::MEMBLOCK_AMO_CAS_PREFILL_DIST_11_30_WT = saved_weights[2];
        seq_csr_common::load_from_plus();
    endtask

    // 抽象职责：验证写范围保护和 LR/SC 同地址承诺，以及 ordinary N / AUTO N+1 的真实建表入口。
    task check_cas_prefill_protection_and_builders();
        main_control_transaction original, low, probe, lr;
        memblock_cas_prefill_group_t group, older;
        memblock_lr_sc_reservation_t reservation;
        memblock_control_reservation_plan_t control0, control1;
        memblock_rob_key_t rob;
        memblock_cas_prefill_expected_fatal_catcher catcher;
        int unsigned saved_enable, saved_sc_enable, saved_sc_same, saved_sc_fixed, saved_sc_dist;
        int unsigned saved_sc_weights[4];
        bit saved_csr, saved_sfence;
        int saved_csr_min, saved_csr_max, saved_sfence_min, saved_sfence_max;
        bit [63:0] saved_base, saved_range;
        bit accepted;
        bit [8:0] opcodes[3];
        memblock_sync_pkg::memblock_control_worker_topology_mode_e saved_topology;
        saved_enable = seq_csr_common::amo_cas_prefill_en_1_wt;
        saved_sc_enable = seq_csr_common::amo_lr_sc_pair_en_1_wt;
        saved_sc_same = seq_csr_common::amo_lr_sc_same_addr_1_wt;
        saved_sc_fixed = seq_csr_common::amo_lr_sc_dist_fixed_wt;
        saved_sc_dist = seq_csr_common::amo_lr_sc_dist_fixed_value;
        saved_sc_weights = '{seq_csr_common::amo_lr_sc_dist_0_wt, seq_csr_common::amo_lr_sc_dist_1_5_wt,
            seq_csr_common::amo_lr_sc_dist_6_10_wt, seq_csr_common::amo_lr_sc_dist_11_100_wt};
        saved_csr = seq_csr_common::csr_control_enable;
        saved_sfence = seq_csr_common::sfence_control_enable;
        saved_csr_min = seq_csr_common::csr_control_min_interval;
        saved_csr_max = seq_csr_common::csr_control_max_interval;
        saved_sfence_min = seq_csr_common::sfence_control_min_interval;
        saved_sfence_max = seq_csr_common::sfence_control_max_interval;
        saved_base = seq_csr_common::main_vaddr_base;
        saved_range = seq_csr_common::main_vaddr_range;
        saved_topology = memblock_sync_pkg::control_worker_topology_mode;
        seq_csr_common::amo_cas_prefill_en_1_wt = 100;
        directed_cas_distance = 5;
        reset_prefill_fixture();
        original = make_prefill_cas(0, MEMBLOCK_LSUOP_AMOCAS_Q);
        accepted = try_plan_cas_prefill(original, 32, 0, 0, 0, 0, low);
        check_expect(accepted, "protection fixture could not plan Q");
        older = cas_prefill_group_audit_q[0];
        // 部分重叠 SW、SC、写型 AMO、CAS 和 CBO.ZERO 均必须搬地址且保留 opcode。
        opcodes = '{MEMBLOCK_LSUOP_SC_D, MEMBLOCK_LSUOP_AMOADD_D, MEMBLOCK_LSUOP_AMOCAS_D};
        for (int kind = 0; kind < 5; kind++) begin
            probe = main_control_transaction::type_id::create("protected_write");
            probe.uid = 2;
            if (kind == 0) begin
                probe.op_class = MEMBLOCK_OP_CLASS_STORE;
                apply_op_class_template(probe, MEMBLOCK_LSUOP_SW);
            end else if (kind == 4) begin
                probe.op_class = MEMBLOCK_OP_CLASS_CBO;
                apply_op_class_template(probe, MEMBLOCK_LSUOP_CBO_ZERO);
            end else begin
                probe.op_class = MEMBLOCK_OP_CLASS_AMO;
                apply_op_class_template(probe, opcodes[kind-1]);
                probe.amo_alignment_bytes = 8;
            end
            probe.src_0 = older.address + (kind == 0 ? 12 : 0);
            probe.update_vaddr();
            check_expect(overlaps_pending_cas_prefill(probe), "partial/atomic/CBO overlap was missed");
            avoid_pending_cas_prefill_write_overlap(probe);
            check_expect(!overlaps_pending_cas_prefill(probe) &&
                (kind == 0 ? probe.fuOpType == MEMBLOCK_LSUOP_SW :
                 kind == 4 ? probe.fuOpType == MEMBLOCK_LSUOP_CBO_ZERO : probe.fuOpType == opcodes[kind-1]),
                "write protection changed opcode or left overlap");
        end
        probe = make_scalar("allowed_read", MEMBLOCK_OP_CLASS_INT_LOAD, 2);
        probe.uid = 2; probe.src_0 = older.address; probe.update_vaddr();
        check_expect(!overlaps_pending_cas_prefill(probe), "load was incorrectly protected as write");
        probe.op_class = MEMBLOCK_OP_CLASS_AMO; apply_op_class_template(probe, MEMBLOCK_LSUOP_LR_D);
        check_expect(!overlaps_pending_cas_prefill(probe), "LR was incorrectly protected as write");
        seq_csr_common::amo_lr_sc_pair_en_1_wt = 100;
        seq_csr_common::amo_lr_sc_same_addr_1_wt = 100;
        seq_csr_common::amo_lr_sc_dist_fixed_wt = 1;
        seq_csr_common::amo_lr_sc_dist_fixed_value = 0;
        seq_csr_common::amo_lr_sc_dist_0_wt = 0;
        seq_csr_common::amo_lr_sc_dist_1_5_wt = 0;
        seq_csr_common::amo_lr_sc_dist_6_10_wt = 0;
        seq_csr_common::amo_lr_sc_dist_11_100_wt = 0;
        lr = probe;
        try_reserve_paired_sc(lr, 32, 0, 0, 0, 0);
        check_expect(sc_reservation_by_uid.num() == 0, "same-address SC broke a protected CAS group");
        original = make_prefill_cas(2, MEMBLOCK_LSUOP_AMOCAS_D);
        accepted = try_plan_cas_prefill(original, 32, 0, 0, 0, 0, low);
        check_expect(accepted, "second overlapping-window group failed to plan");
        group = cas_prefill_group_audit_q[1];
        probe = make_reserved_cas_transaction(group, '{default:0});
        check_expect(!cas_prefill_write_overlaps(probe, older), "new group overlaps older protected bytes");
        // 让所有普通随机重试只得到被保护的 line，确定性步长必须找到 line 内空闲地址。
        seq_csr_common::main_vaddr_base = older.address;
        seq_csr_common::main_vaddr_range = 64;
        probe = make_scalar("deterministic_fallback", MEMBLOCK_OP_CLASS_STORE, 2);
        probe.uid = 2; probe.src_0 = older.address; probe.update_vaddr();
        avoid_pending_cas_prefill_write_overlap(probe);
        check_expect(!overlaps_pending_cas_prefill(probe), "deterministic write address fallback failed");
        seq_csr_common::main_vaddr_base = saved_base;
        seq_csr_common::main_vaddr_range = saved_range;
        // 消费时核验冻结描述和审计账本，不允许错误 map 静默删除。
        cas_prefill_store_high_by_uid[older.store_high_uid].new_low ^= 1;
        catcher = new("CAS_PREFILL", "*frozen descriptor*");
        uvm_report_cb::add(null, catcher);
        probe = consume_cas_prefill_reservation(older.store_high_uid, '{default:0}, 1);
        uvm_report_cb::delete(null, catcher);
        check_expect(catcher.caught_count >= 1, "reservation mutation escaped consumption checks");
        reset_prefill_fixture();
        // 新 group 选址必须避开在先已经预约的同地址 SC，而非到发射时搬动 SC。
        reservation = '{default:0};
        reservation.valid = 1; reservation.sc_uid = 3; reservation.same_addr = 1;
        reservation.sc_fuOpType = MEMBLOCK_LSUOP_SC_D; reservation.lr_src_0 = saved_base;
        sc_reservation_by_uid[3] = reservation;
        group = '0; group.valid = 1; group.origin_uid = 0; group.cas_uid = 5;
        group.cas_fuOpType = MEMBLOCK_LSUOP_AMOCAS_D; group.address = saved_base;
        check_expect(cas_prefill_address_conflicts(group), "new group ignored an earlier same-address SC reservation");
        // SC 初始候选碰撞 CSR/SFence/high/owner 后顺延一次，距离不重新抽样。
        reset_prefill_fixture();
        group = '0; group.valid = 1; group.store_high_uid = 2; group.cas_uid = 3;
        cas_prefill_store_high_by_uid[2] = group;
        cas_prefill_owner_by_uid[3] = group;
        lr = make_prefill_cas(0, MEMBLOCK_LSUOP_LR_D);
        try_reserve_paired_sc(lr, 6, 1, 1, 1, 4);
        check_expect(sc_reservation_by_uid.exists(5) && sc_reservation_by_uid[5].selected_dist == 0 &&
            sc_reservation_by_uid[5].lr_src_0 == lr.src_0, "SC collision did not preserve distance/address commitment");
        reset_prefill_fixture();
        // 两个真实建表入口：定向 CAS.Q 一直生成，验证每个非末尾 CAS 都有完整组。
        directed_cas_generation = 1;
        directed_cas_opcode = MEMBLOCK_LSUOP_AMOCAS_Q;
        directed_cas_distance = 1;
        seq_csr_common::csr_control_enable = 0;
        seq_csr_common::sfence_control_enable = 0;
        build_random_main_table(12);
        check_expect(data.main_trans_num == 12 && cas_prefill_group_audit_q.size() == 4,
            "ordinary builder did not retain N/full Q groups");
        // 软件测试临时切换已冻结拓扑的观测值，真实 driver 不运行，结束后立即恢复。
        memblock_sync_pkg::control_worker_topology_mode = memblock_sync_pkg::MEMBLOCK_CONTROL_TOPOLOGY_AUTO_MAIN_TABLE;
        build_control_auto_main_table(12);
        check_expect(data.main_trans_num == 13 && cas_prefill_group_audit_q.size() == 4 &&
            data.get_main_transaction(12).op_class == MEMBLOCK_OP_CLASS_CHECK_STORE,
            "AUTO builder changed N+1 or check_store UID=N");
        // CSR/SFence 同 UID 保持 CSR 优先，两者从相同 base 再预约。
        seq_csr_common::csr_control_enable = 1;
        seq_csr_common::sfence_control_enable = 1;
        seq_csr_common::csr_control_min_interval = 3;
        seq_csr_common::csr_control_max_interval = 3;
        seq_csr_common::sfence_control_min_interval = 3;
        seq_csr_common::sfence_control_max_interval = 3;
        build_control_auto_main_table(12);
        check_expect(data.get_main_transaction(3).op_class == MEMBLOCK_OP_CLASS_CSR_CONTROL &&
            data.get_main_transaction(6).op_class == MEMBLOCK_OP_CLASS_CSR_CONTROL &&
            data.get_main_transaction(9).op_class == MEMBLOCK_OP_CLASS_CSR_CONTROL &&
            data.get_main_transaction(12).op_class == MEMBLOCK_OP_CLASS_CHECK_STORE,
            "CSR/SFence same-target priority/rescheduling changed");
        seq_csr_common::amo_cas_prefill_en_1_wt = 0;
        build_random_main_table(12);
        check_expect(cas_prefill_group_audit_q.size() == 0 && data.main_trans_num == 12,
            "repeated build retained stale prefill ledger");
        directed_cas_generation = 0;
        directed_cas_distance = 0;
        seq_csr_common::amo_cas_prefill_en_1_wt = saved_enable;
        seq_csr_common::amo_lr_sc_pair_en_1_wt = saved_sc_enable;
        seq_csr_common::amo_lr_sc_same_addr_1_wt = saved_sc_same;
        seq_csr_common::amo_lr_sc_dist_fixed_wt = saved_sc_fixed;
        seq_csr_common::amo_lr_sc_dist_fixed_value = saved_sc_dist;
        seq_csr_common::amo_lr_sc_dist_0_wt = saved_sc_weights[0];
        seq_csr_common::amo_lr_sc_dist_1_5_wt = saved_sc_weights[1];
        seq_csr_common::amo_lr_sc_dist_6_10_wt = saved_sc_weights[2];
        seq_csr_common::amo_lr_sc_dist_11_100_wt = saved_sc_weights[3];
        seq_csr_common::csr_control_enable = saved_csr;
        seq_csr_common::sfence_control_enable = saved_sfence;
        seq_csr_common::csr_control_min_interval = saved_csr_min;
        seq_csr_common::csr_control_max_interval = saved_csr_max;
        seq_csr_common::sfence_control_min_interval = saved_sfence_min;
        seq_csr_common::sfence_control_max_interval = saved_sfence_max;
        memblock_sync_pkg::control_worker_topology_mode = saved_topology;
        reset_prefill_fixture();
        `uvm_info("CAS_PREFILL_SOFT", "address protection/LR-SC/ordinary N/AUTO N+1/control/reset checks passed", UVM_LOW)
    endtask

    // 抽象职责：逐宽度和距离验证完整组构造、冻结数据、尾部回滚、预约审计及 UID 冲突。
    task check_cas_prefill_groups();
        main_control_transaction original, replacement, owner, high, probe;
        memblock_cas_prefill_group_t group;
        memblock_lr_sc_reservation_t sc;
        memblock_control_reservation_plan_t control;
        memblock_rob_key_t rob;
        bit [8:0] opcodes[3];
        int distances[6];
        memblock_uid_t selected;
        bit accepted;
        int unsigned saved_enable, saved_weights[3];
        bit seen[31];
        memblock_cas_prefill_expected_fatal_catcher catcher;
        saved_enable = seq_csr_common::amo_cas_prefill_en_1_wt;
        saved_weights = seq_csr_common::amo_cas_prefill_dist_wt;
        opcodes = '{MEMBLOCK_LSUOP_AMOCAS_W, MEMBLOCK_LSUOP_AMOCAS_D, MEMBLOCK_LSUOP_AMOCAS_Q};
        distances = '{1, 5, 6, 10, 11, 30};
        rob = '{default:0};
        reset_prefill_fixture();
        original = make_prefill_cas(0, MEMBLOCK_LSUOP_AMOCAS_D);
        seq_csr_common::amo_cas_prefill_en_1_wt = 0;
        accepted = try_plan_cas_prefill(original, 64, 0, 0, 0, 0, replacement);
        check_expect(!accepted && replacement == null && cas_prefill_owner_by_uid.num() == 0 &&
            original.op_class == MEMBLOCK_OP_CLASS_AMO && !original.std_data_override_valid[0],
            "prefill disabled changed the original CAS");
        seq_csr_common::amo_cas_prefill_en_1_wt = 100;
        foreach (opcodes[k]) begin
            foreach (distances[d]) begin
                reset_prefill_fixture();
                directed_cas_distance = distances[d];
                original = make_prefill_cas(0, opcodes[k]);
                accepted = try_plan_cas_prefill(original, 64, 0, 0, 0, 0, replacement);
                check_expect(accepted && cas_prefill_group_audit_q.size() == 1, "100% prefill failed with capacity");
                group = cas_prefill_group_audit_q[0];
                check_expect(group.cas_uid == (k == 2 ? 1 : 0) + distances[d] &&
                    group.selected_distance == distances[d], "CAS distance is not relative to last Store");
                data.reset_all_tables(group.cas_uid + 1);
                for (memblock_uid_t uid = 0; uid <= group.cas_uid; uid++) begin
                    if (uid == 0) probe = replacement;
                    else if (cas_prefill_store_high_by_uid.exists(uid)) begin
                        high = consume_cas_prefill_reservation(uid, rob, 1);
                        probe = high;
                    end else if (cas_prefill_owner_by_uid.exists(uid)) begin
                        owner = consume_cas_prefill_reservation(uid, rob, 0);
                        probe = owner;
                    end else begin
                        probe = make_scalar("prefill_gap_load", MEMBLOCK_OP_CLASS_INT_LOAD, uid);
                        probe.uid = uid;
                    end
                    data.set_main_transaction(uid, probe);
                end
                check_all_cas_prefill_groups(group.cas_uid + 1);
                check_expect(replacement.lsq_flow == MEMBLOCK_LSQ_FLOW_STORE && replacement.numLsElem == 1 &&
                    owner.numLsElem == 0 && owner.send_pri == 100 && owner.send_pri_std == 100,
                    "prefill did not preserve scalar/atomic templates");
                check_expect(field_assigner.derive_std_data(replacement, 0) == field_assigner.derive_std_data(owner, 0),
                    "CAS low compare differs from prefill Store");
                if (k == 0) check_expect(owner.std_data_override[0][63:32] == 0 &&
                    owner.std_data_override[1][63:32] == 0, "CAS.W high bits were not cleared");
                if (k == 2) check_expect(high.vaddr == replacement.vaddr + 8 &&
                    field_assigner.derive_std_data(high, 0) == field_assigner.derive_std_data(owner, 2),
                    "CAS.Q high compare/address mapping mismatch");
                // 逆序重复读取并改变 UID，证明冻结 new/compare 不依赖发射顺序或 UID 派生。
                owner.uid = 123;
                for (int slot = (k == 2 ? 3 : 1); slot >= 0; slot--) begin
                    bit [63:0] expected;
                    case (slot)
                        0: expected = group.compare_low;
                        1: expected = group.new_low;
                        2: expected = group.compare_high;
                        3: expected = group.new_high;
                    endcase
                    repeat (3) check_expect(field_assigner.derive_std_data(owner, slot) == expected, "STD override drifted on retry/shuffle");
                end
                owner.uid = group.cas_uid;
                // 审计负向：冻结 new 数据被破坏必须在 TLB 构造前报错。
                owner.std_data_override[1] ^= 64'd1;
                catcher = new("CAS_PREFILL", "*frozen data mismatch*");
                uvm_report_cb::add(null, catcher);
                check_all_cas_prefill_groups(group.cas_uid + 1);
                uvm_report_cb::delete(null, catcher);
                check_expect(catcher.caught_count == 1, "ledger did not detect changed owner data");
                owner.std_data_override[1] ^= 64'd1;
            end
        end
        // 三档随机本身的边界与大权重求和（不使用定向距离覆盖）。
        directed_cas_distance = 0;
        foreach (seen[i]) seen[i] = 0;
        for (int bucket = 0; bucket < 3; bucket++) begin
            seq_csr_common::amo_cas_prefill_dist_wt = '{0, 0, 0};
            seq_csr_common::amo_cas_prefill_dist_wt[bucket] = 1;
            repeat (512) begin
                int unsigned distance;
                distance = randomize_cas_prefill_distance();
                check_expect((bucket == 0 && distance inside {[1:5]}) ||
                    (bucket == 1 && distance inside {[6:10]}) ||
                    (bucket == 2 && distance inside {[11:30]}), "distance escaped selected bucket");
                seen[distance] = 1;
            end
        end
        foreach (distances[i]) check_expect(seen[distances[i]], "distance bucket endpoint was not exercised");
        seq_csr_common::amo_cas_prefill_dist_wt = '{32'h7fff_ffff, 32'h7fff_ffff, 32'h7fff_ffff};
        repeat (10) check_expect(randomize_cas_prefill_distance() inside {[1:30]}, "64-bit distance weight total overflow");
        directed_cas_distance = 1;
        foreach (opcodes[k]) begin
            reset_prefill_fixture();
            original = make_prefill_cas(0, opcodes[k]);
            accepted = try_plan_cas_prefill(original, k == 2 ? 2 : 1, 0, 0, 0, 0, replacement);
            check_expect(!accepted && replacement == null && cas_prefill_group_audit_q.size() == 0 &&
                cas_prefill_owner_by_uid.num() == 0 && cas_prefill_store_high_by_uid.num() == 0 &&
                original.fuOpType == opcodes[k] && !original.std_data_override_valid[0], "tail failure left a partial group");
        end
        reset_prefill_fixture();
        sc = '{default:0}; sc.valid = 1; sc.sc_uid = 1;
        sc_reservation_by_uid[1] = sc;
        group = '0; group.valid = 1; group.cas_uid = 2;
        cas_prefill_owner_by_uid[2] = group;
        group.store_high_uid = 3;
        cas_prefill_store_high_by_uid[3] = group;
        accepted = find_next_free_companion_uid(1, 7, 1, 4, 1, 5, selected);
        check_expect(accepted && selected == 6, "SC/CAS/high/CSR/SFence collision did not scan +1");
        check_expect(!find_next_free_companion_uid(1, 6, 1, 4, 1, 5, selected), "scan occupied AUTO check_store UID N");
        control = '{enabled:1, target_valid:0, next_uid:0};
        reschedule_control_reservation(control, 0, 1, 1, 4);
        check_expect(!control.target_valid, "control scan beyond N must expire");
        reschedule_control_reservation(control, 0, 1, 1, 7);
        check_expect(control.target_valid && control.next_uid == 4, "control did not skip SC/CAS reservations");
        // 允许 CSR/SFence 彼此同 target；其它重叠必须 fatal。
        check_expect(uid_reserved_for_companion(4, 1, 4, 1, 4, 7), "CSR/SFence same target was rejected");
        catcher = new("CAS_PREFILL", "*overlapping companion/control*");
        uvm_report_cb::add(null, catcher);
        accepted = uid_reserved_for_companion(1, 1, 1, 0, 0, 7);
        uvm_report_cb::delete(null, catcher);
        check_expect(catcher.caught_count == 1, "SC/control collision was silently prioritized");
        reset_prefill_fixture();
        directed_cas_distance = 0;
        seq_csr_common::amo_cas_prefill_en_1_wt = saved_enable;
        seq_csr_common::amo_cas_prefill_dist_wt = saved_weights;
        `uvm_info("CAS_PREFILL_SOFT", "parameters/override/W-D-Q/distance/collision/rollback/audit checks passed", UVM_LOW)
    endtask

    function main_control_transaction make_scalar(input string name,
                                                  input memblock_op_class_e op_class,
                                                  input int unsigned rob_value);
        main_control_transaction tr;

        tr = main_control_transaction::type_id::create(name);
        tr.op_class = op_class;
        tr.robIdx_flag = 1'b0;
        tr.robIdx_value = MEMBLOCK_ROB_VALUE_W'(rob_value);
        tr.src_0 = 64'h8000_0000 + (rob_value * 64);
        tr.imm = 0;
        tr.numLsElem = memblock_num_ls_elem_t'(1);
        if (op_class == MEMBLOCK_OP_CLASS_STORE) begin
            tr.fuType = MEMBLOCK_FUTYPE_STU;
            tr.fuOpType = MEMBLOCK_LSUOP_SD;
            tr.lsq_flow = MEMBLOCK_LSQ_FLOW_STORE;
        end else begin
            tr.fuType = MEMBLOCK_FUTYPE_LDU;
            tr.fuOpType = MEMBLOCK_LSUOP_LD;
            tr.lsq_flow = MEMBLOCK_LSQ_FLOW_LOAD;
        end
        tr.update_vaddr();
        return tr;
    endfunction:make_scalar

    function memblock_sync_pkg::dispatch_raw_cancel_snapshot_t next_snapshot();
        memblock_sync_pkg::dispatch_raw_cancel_snapshot_t snapshot;

        sample_seq++;
        snapshot = memblock_sync_pkg::make_empty_raw_cancel_snapshot();
        snapshot.valid = 1'b1;
        snapshot.complete = 1'b1;
        snapshot.backend_done = 1'b1;
        snapshot.control_reset_epoch = 1;
        snapshot.sample_seq = sample_seq;
        snapshot.cycle = sample_seq;
        return snapshot;
    endfunction:next_snapshot

    task check_fixed_lane_prefix_and_partial();
        memblock_lsqenq_dispatch_base_sequence admission_seq;
        memblock_uid_t uids[$];
        int unsigned slots[$];
        main_control_transaction trs[$];
        memblock_op_behavior_t behaviors[$];
        memblock_lq_key_t lq_keys[$];
        memblock_sq_key_t sq_keys[$];
        memblock_sync_pkg::dispatch_raw_cancel_snapshot_t snapshot;
        bit [MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM-1:0] fire_mask;

        clear_manual_main_table();
        set_manual_main_transaction(0, make_scalar("soft_store0", MEMBLOCK_OP_CLASS_STORE, 0));
        set_manual_main_transaction(1, make_scalar("soft_load1", MEMBLOCK_OP_CLASS_INT_LOAD, 1));
        set_manual_main_transaction(2, make_scalar("soft_store2", MEMBLOCK_OP_CLASS_STORE, 2));
        import_manual_main_table();
        lsq_ctrl.reset_rtl_shadow();
        sample_seq = 0;
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);

        admission_seq = memblock_lsqenq_dispatch_base_sequence::type_id::create("admission_seq");
        admission_seq.ensure_helpers();
        check_expect(!admission_seq.collect_lsq_candidates(uids, slots, trs, behaviors,
                                                     lq_keys, sq_keys),
               "first collect must capture D without issuing");
        check_expect(lsq_ctrl.shadow_d_valid[0] && lsq_ctrl.shadow_d_valid[1] &&
               lsq_ctrl.shadow_d_valid[2], "D bundle capture failed");

        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        check_expect(lsq_ctrl.shadow_r_valid[0] && !lsq_ctrl.shadow_q_valid[0],
               "D did not advance to R on its own edge");
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        check_expect(lsq_ctrl.shadow_q_valid[0] && lsq_ctrl.shadow_q_valid[1] &&
               lsq_ctrl.shadow_q_valid[2], "R did not advance to fixed-lane Q");
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        check_expect(!lsq_ctrl.shadow_iq_recompute_hold, "capture recompute hold did not clear");
        check_expect(lsq_ctrl.shadow_conserve_flow_total[0] == 1 &&
               lsq_ctrl.shadow_conserve_flow_total[1] == 2 &&
               lsq_ctrl.shadow_conserve_flow_total[2] == 3,
               "mixed conserveFlowTotal prefix is incorrect");

        // Unit-check strict mixed prefix independently of the common 6/4
        // registered headroom gate: store passes SQ credit, following load is
        // blocked by strict LQ > conserve and younger store cannot bypass it.
        lsq_ctrl.shadow_can_accept = 1'b1;
        lsq_ctrl.shadow_lq_credit = 2;
        lsq_ctrl.shadow_sq_credit = 20;
        check_expect(admission_seq.collect_lsq_candidates(uids, slots, trs, behaviors,
                                                    lq_keys, sq_keys),
               "strict mixed prefix produced no candidate");
        check_expect(uids.size() == 1 && uids[0] == 0 && slots[0] == 0,
               "strict mixed prefix bypassed the blocked load");

        fire_mask = '0;
        fire_mask[0] = 1'b1;
        lsq_ctrl.note_source_offer(fire_mask, 1'b1, 0, 1, sample_seq + 1);
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        check_expect(!lsq_ctrl.shadow_q_valid[0] && lsq_ctrl.shadow_q_valid[1] &&
               lsq_ctrl.shadow_q_valid[2], "partial fire compressed or lost Q lanes");
        check_expect(lsq_ctrl.shadow_iq_recompute_hold,
               "partial fire did not request a fixed-lane prefix recompute");
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        check_expect(lsq_ctrl.shadow_conserve_flow_total[0] == 0 &&
               lsq_ctrl.shadow_conserve_flow_total[1] == 1 &&
               lsq_ctrl.shadow_conserve_flow_total[2] == 2,
               "partial-hole conserveFlowTotal recompute is incorrect");
    endtask:check_fixed_lane_prefix_and_partial

    task check_source_offer_target_sample();
        memblock_sync_pkg::dispatch_raw_cancel_snapshot_t snapshot;
        bit [MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM-1:0] fire_mask;

        lsq_ctrl.reset_rtl_shadow();
        sample_seq = 0;
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        fire_mask = '0;
        lsq_ctrl.note_source_offer(fire_mask, 1'b0, 0, 0, sample_seq + 2);
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        check_expect(lsq_ctrl.shadow_source_offer_valid &&
               lsq_ctrl.shadow_source_target_sample_seq == sample_seq + 1,
               "source offer was consumed before its target sample");
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        check_expect(!lsq_ctrl.shadow_source_offer_valid,
               "source offer was not consumed at its exact target sample");

        // 连续真实 driver 周期必须逐拍冻结并逐拍消费，不能因为上一 offer
        // 刚清除就漏记下一 active 或下一 idle 周期。
        lsq_ctrl.shadow_q_valid[0] = 1'b1;
        lsq_ctrl.shadow_q_uid[0] = 3;
        lsq_ctrl.shadow_q_is_load[0] = 1'b1;
        fire_mask = '0;
        fire_mask[0] = 1'b1;
        lsq_ctrl.note_source_offer(fire_mask, 1'b1, 1, 0, sample_seq + 1);
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        check_expect(!lsq_ctrl.shadow_source_offer_valid && !lsq_ctrl.shadow_q_valid[0],
               "first active offer did not settle on its target sample");

        lsq_ctrl.shadow_q_valid[0] = 1'b1;
        lsq_ctrl.shadow_q_uid[0] = 4;
        lsq_ctrl.shadow_q_is_load[0] = 1'b1;
        lsq_ctrl.note_source_offer(fire_mask, 1'b1, 1, 0, sample_seq + 1);
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        check_expect(!lsq_ctrl.shadow_source_offer_valid && !lsq_ctrl.shadow_q_valid[0],
               "active-to-active source offer was skipped");

        fire_mask = '0;
        lsq_ctrl.note_source_offer(fire_mask, 1'b0, 0, 0, sample_seq + 1);
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        check_expect(!lsq_ctrl.shadow_source_offer_valid,
               "active-to-idle source offer was skipped");

        lsq_ctrl.shadow_q_valid[0] = 1'b1;
        lsq_ctrl.shadow_q_uid[0] = 5;
        lsq_ctrl.shadow_q_is_load[0] = 1'b0;
        lsq_ctrl.shadow_q_is_store[0] = 1'b0;
        lsq_ctrl.shadow_q_is_atomic[0] = 1'b1;
        fire_mask[0] = 1'b1;
        lsq_ctrl.note_source_offer(fire_mask, 1'b0, 0, 0, sample_seq + 1);
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        check_expect(!lsq_ctrl.shadow_source_offer_valid && !lsq_ctrl.shadow_q_valid[0],
               "atomic Q-fire source offer did not consume lane0");
    endtask:check_source_offer_target_sample

    task check_driver_launch_sample_binding();
        memblock_lsqenq_dispatch_base_sequence admission_seq;
        lsqenq_agent_agent_xaction tr;
        memblock_sync_pkg::dispatch_lsq_source_offer_t offer;
        memblock_sync_pkg::dispatch_raw_cancel_snapshot_t snapshot;
        bit [MEMBLOCK_DUT_LSQ_ENQ_SLOT_NUM-1:0] fire_mask;
        memblock_lq_key_t lq_before;
        memblock_sq_key_t sq_before;

        memblock_sync_pkg::clear_lsq_source_offers();
        lsq_ctrl.reset_rtl_shadow();
        sample_seq = 0;
        lsq_ctrl.advance_driver_bound_admission_sample(next_snapshot(), 1'b0);
        admission_seq = memblock_lsqenq_dispatch_base_sequence::type_id::create(
            "driver_launch_binding_seq");
        admission_seq.ensure_helpers();
        tr = lsqenq_agent_agent_xaction::type_id::create("driver_launch_binding_tr");
        fire_mask = '0;
        fire_mask[0] = 1'b1;
        admission_seq.configure_source_offer_metadata(tr, fire_mask, 1'b1, 1, 0,
                                                     "soft delayed launch");
        check_expect(!tr.memblock_source_offer_published &&
               memblock_sync_pkg::lsq_source_offer_queue_size() == 0 &&
               !lsq_ctrl.shadow_source_offer_valid,
               "item construction speculated a source offer before driver launch");

        // 用真实 driver 共用的绑定函数模拟 launch=4；item 早在 sample=1
        // 构造也不能使 target 变成 2。FIFO/consumer 则走正式 main service 路径。
        offer = memblock_sync_pkg::make_lsq_source_offer_at_launch(4, 1);
        offer.fire_mask = tr.memblock_source_offer_fire_mask;
        offer.need_alloc_nonzero = tr.memblock_source_offer_need_alloc_nonzero;
        offer.lq_alloc = tr.memblock_source_offer_lq_alloc;
        memblock_sync_pkg::push_lsq_source_offer(offer);
        check_expect(offer.target_sample_seq == 4 && offer.dut_sample_seq == 5,
               "driver launch/source edge and next DUT sample binding are incorrect");
        offer = memblock_sync_pkg::make_lsq_source_offer_at_launch(5, 1);
        offer.fire_mask = fire_mask;
        offer.need_alloc_nonzero = 1'b1;
        offer.sq_alloc = 1;
        memblock_sync_pkg::push_lsq_source_offer(offer);
        // 自动 idle 无 item，但仍必须占据唯一一个连续输出拍。
        offer = memblock_sync_pkg::make_lsq_source_offer_at_launch(6, 1);
        memblock_sync_pkg::push_lsq_source_offer(offer);
        offer = memblock_sync_pkg::make_lsq_source_offer_at_launch(7, 1);
        offer.fire_mask = fire_mask;
        memblock_sync_pkg::push_lsq_source_offer(offer);

        lsq_ctrl.shadow_q_valid[0] = 1'b1;
        lsq_ctrl.shadow_q_is_load[0] = 1'b1;
        lsq_ctrl.shadow_q_is_store[0] = 1'b0;
        lsq_ctrl.shadow_q_is_atomic[0] = 1'b0;
        repeat (2) begin
            snapshot = next_snapshot();
            lsq_ctrl.advance_driver_bound_admission_sample(snapshot, 1'b0);
            check_expect(lsq_ctrl.shadow_q_valid[0] &&
                   lsq_ctrl.shadow_lq_credit == MEMBLOCK_LQ_SIZE &&
                   memblock_sync_pkg::lsq_source_offer_queue_size() == 4,
                   "driver offer changed shadow before the true source edge");
        end
        lsq_ctrl.advance_driver_bound_admission_sample(next_snapshot(), 1'b0);
        check_expect(!lsq_ctrl.shadow_q_valid[0] &&
               lsq_ctrl.shadow_lq_credit == MEMBLOCK_LQ_SIZE - 1 &&
               memblock_sync_pkg::lsq_source_offer_queue_size() == 3,
               "active driver offer was not consumed exactly at its launch/source edge");
        lsq_ctrl.shadow_q_valid[0] = 1'b1;
        lsq_ctrl.shadow_q_is_load[0] = 1'b0;
        lsq_ctrl.shadow_q_is_store[0] = 1'b1;
        lsq_ctrl.advance_driver_bound_admission_sample(next_snapshot(), 1'b0);
        check_expect(!lsq_ctrl.shadow_q_valid[0] &&
               lsq_ctrl.shadow_sq_credit == MEMBLOCK_SQ_SIZE - 1 &&
               memblock_sync_pkg::lsq_source_offer_queue_size() == 2,
               "consecutive active-to-active driver offer was lost");
        lsq_ctrl.shadow_q_valid[0] = 1'b1;
        lsq_ctrl.shadow_q_is_store[0] = 1'b0;
        lsq_ctrl.shadow_q_is_atomic[0] = 1'b1;
        lq_before = lsq_ctrl.shadow_lq_enq_ptr;
        sq_before = lsq_ctrl.shadow_sq_enq_ptr;
        lsq_ctrl.advance_driver_bound_admission_sample(next_snapshot(), 1'b0);
        check_expect(lsq_ctrl.shadow_q_valid[0] &&
               memblock_sync_pkg::lsq_source_offer_queue_size() == 1,
               "active-to-automatic-idle offer was skipped or fired atomic Q");
        lsq_ctrl.advance_driver_bound_admission_sample(next_snapshot(), 1'b0);
        check_expect(!lsq_ctrl.shadow_q_valid[0] &&
               lsq_ctrl.shadow_lq_enq_ptr == lq_before &&
               lsq_ctrl.shadow_sq_enq_ptr == sq_before &&
               memblock_sync_pkg::lsq_source_offer_queue_size() == 0,
               "atomic Q offer was not sampled once with zero LSQ allocation");
    endtask:check_driver_launch_sample_binding

    task check_reset_pending_reservation_cancel();
        memblock_lsqenq_dispatch_base_sequence admission_seq;
        main_control_transaction tr;
        status_transaction status;
        memblock_op_behavior_t behavior;
        memblock_lsq_reservation_token_t token;
        bit saved_backend_done;
        bit has_progress;
        memblock_lq_key_t initial_lq_enq_ptr;
        memblock_sq_key_t initial_sq_enq_ptr;
        int unsigned initial_lq_free_count;
        int unsigned initial_sq_free_count;

        clear_manual_main_table();
        set_manual_main_transaction(0, make_scalar("soft_reset_pending_load",
                                                   MEMBLOCK_OP_CLASS_INT_LOAD, 0));
        import_manual_main_table();
        lsq_ctrl.reset_rtl_shadow();
        sample_seq = 0;
        lsq_ctrl.advance_rtl_admission_sample(next_snapshot(), 1'b0);
        admission_seq = memblock_lsqenq_dispatch_base_sequence::type_id::create(
            "reset_pending_reservation_seq");
        admission_seq.ensure_helpers();
        tr = data.get_main_transaction(0);
        behavior = memblock_op_behavior_util::derive_op_behavior(tr);
        initial_lq_enq_ptr = lsq_ctrl.lq_enq_ptr;
        initial_sq_enq_ptr = lsq_ctrl.sq_enq_ptr;
        initial_lq_free_count = lsq_ctrl.lq_free_count;
        initial_sq_free_count = lsq_ctrl.sq_free_count;
        lsq_ctrl.commit_allocate(0, behavior, tr);
        token.valid = 1'b1;
        token.uid = 0;
        token.launch_epoch = data.begin_lsq_reservation_launch(0);
        admission_seq.pending_sample_tokens.push_back(token);
        admission_seq.pending_sample_valid = 1'b1;
        admission_seq.pending_sample_target_sample_seq = sample_seq + 1;
        admission_seq.pending_sample_control_reset_epoch = memblock_sync_pkg::control_reset_epoch;
        admission_seq.pending_sample_shadow_generation = lsq_ctrl.shadow_baseline_generation;
        admission_seq.publish_sequence_local_state();
        status = data.get_status(0);
        check_expect(status.lsq_reservation_state ==
                         MEMBLOCK_LSQ_RESERVATION_LAUNCHED_PENDING_SAMPLE &&
               lsq_ctrl.shadow_last_sample_seq < admission_seq.pending_sample_target_sample_seq &&
               data.lsq_sequence_pending_sample,
               "reset directed test did not create an unsampled pending reservation");

        // 不推进 sample；reset 必须立即取消，不得等待 target 或调用 sampled。
        saved_backend_done = memblock_sync_pkg::reset_backend_done;
        memblock_sync_pkg::reset_backend_done = 1'b0;
        admission_seq.wait_or_cancel_pending_sample();
        has_progress = 1'b0;
        admission_seq.complete_v2_pending_sample(has_progress);
        check_expect(!admission_seq.pending_sample_valid &&
               admission_seq.pending_sample_tokens.size() == 0 &&
               admission_seq.pending_sample_target_sample_seq == 0 &&
               admission_seq.pending_sample_control_reset_epoch == 0 &&
               admission_seq.pending_sample_shadow_generation == 0 &&
               !data.lsq_sequence_pending_sample && !has_progress &&
               status.lsq_reservation_state == MEMBLOCK_LSQ_RESERVATION_NONE &&
               !status.lsq_reservation_sample_valid && status.lsq_reservation_sample_seq == 0 &&
               status.lsq_reservation_launch_epoch == 0 && !status.active && !status.enq &&
               !status.active_lq_mapped && !status.active_sq_mapped &&
               lsq_ctrl.lq_enq_ptr == initial_lq_enq_ptr &&
               lsq_ctrl.sq_enq_ptr == initial_sq_enq_ptr &&
               lsq_ctrl.lq_free_count == initial_lq_free_count &&
               lsq_ctrl.sq_free_count == initial_sq_free_count &&
               !status.issue_ready,
               "reset did not rollback pending token, owner, pointer and credit state");
        memblock_sync_pkg::reset_backend_done = saved_backend_done;
    endtask:check_reset_pending_reservation_cancel

    task check_atomic_held_piece_stability();
        main_control_transaction amo;
        memblock_op_behavior_t behavior;
        status_transaction status;
        memblock_issue_q_item_t item;
        memblock_issue_q_item_t first_item;
        memblock_issue_q_item_t load_items[$];
        memblock_issue_q_item_t sta_items[$];
        memblock_issue_q_item_t std_items[$];

        clear_manual_main_table();
        amo = main_control_transaction::type_id::create("soft_atomic_held_piece");
        amo.op_class = MEMBLOCK_OP_CLASS_AMO;
        amo.fuType = MEMBLOCK_FUTYPE_MOU;
        amo.fuOpType = MEMBLOCK_LSUOP_AMOCAS_D;
        amo.lsq_flow = MEMBLOCK_LSQ_FLOW_ATOMIC;
        amo.robIdx_flag = 1'b0;
        amo.robIdx_value = MEMBLOCK_ROB_VALUE_W'(0);
        amo.src_0 = 64'h9000_0000;
        amo.numLsElem = 0;
        amo.update_vaddr();
        set_manual_main_transaction(0, amo);
        import_manual_main_table();
        behavior = memblock_op_behavior_util::derive_op_behavior(
            data.get_main_transaction(0));
        lsq_ctrl.commit_atomic_non_lsq_admission(
            0, behavior, data.get_main_transaction(0),
            '{default:'0}, '{default:'0});
        data.establish_atomic_barrier(0);
        status = data.get_status(0);
        status.issue_ready = 1'b1;
        item = issue_sched.make_issue_item(0, MEMBLOCK_ISSUE_TARGET_STD, behavior);
        item.uop_index = 1;
        item.issue_pipe_index = 0;
        data.push_issue_queue_item(item);
        status.queued_std = 1'b1;

        issue_sched.select_issue_candidates(load_items, sta_items, std_items);
        check_expect(std_items.size() == 1 && data.atomic_held_piece_valid,
               "atomic selector did not freeze its first nonblocking piece");
        first_item = std_items[0];
        issue_sched.select_issue_candidates(load_items, sta_items, std_items);
        check_expect(std_items.size() == 1 &&
               data.atomic_held_piece_matches(std_items[0]) &&
               std_items[0].target == first_item.target &&
               std_items[0].uop_index == first_item.uop_index &&
               std_items[0].dynamic_epoch == first_item.dynamic_epoch,
               "ready=0 reselection changed atomic target/slot/payload identity");
        data.reset_active_atomic_after_physical_reset("directed soft test");
        check_expect(!data.active_atomic_barrier_valid &&
               !data.atomic_held_piece_valid && !status.active && !status.enq &&
               !status.queued_std && data.get_next_new_admit_uid() == 0,
               "physical reset did not clear atomic owner/barrier/held piece and reopen uid");
    endtask:check_atomic_held_piece_stability

    task check_atomic_token_reset_binding();
        memblock_lsqenq_dispatch_base_sequence admission_seq;

        lsq_ctrl.reset_rtl_shadow();
        sample_seq = 0;
        lsq_ctrl.advance_rtl_admission_sample(next_snapshot(), 1'b0);
        admission_seq = memblock_lsqenq_dispatch_base_sequence::type_id::create(
            "atomic_token_reset_binding_seq");
        admission_seq.ensure_helpers();
        admission_seq.atomic_token_stage = 1;
        admission_seq.atomic_token_control_reset_epoch =
            lsq_ctrl.shadow_control_reset_epoch;
        admission_seq.atomic_token_shadow_generation =
            lsq_ctrl.shadow_baseline_generation;
        check_expect(!admission_seq.atomic_token_reset_binding_invalid(),
               "fresh atomic token reset binding is unexpectedly invalid");
        admission_seq.atomic_token_control_reset_epoch++;
        check_expect(admission_seq.atomic_token_reset_binding_invalid(),
               "control epoch change did not invalidate pre-fire atomic token");
        admission_seq.atomic_token_control_reset_epoch =
            lsq_ctrl.shadow_control_reset_epoch;
        lsq_ctrl.reset_rtl_shadow();
        check_expect(admission_seq.atomic_token_reset_binding_invalid(),
               "shadow baseline rebuild did not invalidate pre-fire atomic token");
        admission_seq.clear_pre_fire_atomic_token("soft-test cleanup");
    endtask:check_atomic_token_reset_binding

    task check_cancel_shadow_restore_credential();
        memblock_sync_pkg::dispatch_raw_cancel_snapshot_t snapshot;
        memblock_lsq_cancel_record_t record;
        memblock_sq_key_t expected_sq_ptr;

        check_expect(data.cancel_record_q.size() == 0,
               "cancel restore soft test requires an empty record queue");
        lsq_ctrl.reset_rtl_shadow();
        sample_seq = 0;
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        lsq_ctrl.shadow_sq_enq_ptr =
            lsq_ctrl.advance_sq_key(lsq_ctrl.shadow_sq_enq_ptr, 4);
        lsq_ctrl.shadow_sq_credit = MEMBLOCK_SQ_SIZE - 4;
        expected_sq_ptr = lsq_ctrl.rewind_sq_key(lsq_ctrl.shadow_sq_enq_ptr, 2);

        record = '{default:'0};
        record.valid = 1'b1;
        record.redirect_epoch = 7;
        record.cancel_record_id = 91;
        record.redirect_anchor_valid = 1'b1;
        record.dut_cancel_update_sample_seq = sample_seq + 1;
        record.compare_snapshot_sample_seq =
            record.dut_cancel_update_sample_seq + MEMBLOCK_TB_CANCEL_MONITOR_SAMPLE_OFFSET;
        data.cancel_record_q.push_back(record);
        lsq_ctrl.shadow_t2_redirect = 1'b1;
        snapshot = next_snapshot();
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        check_expect(lsq_ctrl.shadow_t3_update &&
               lsq_ctrl.shadow_t3_cancel_record_valid &&
               lsq_ctrl.shadow_t3_cancel_record_id == record.cancel_record_id &&
               lsq_ctrl.shadow_sq_enq_ptr.value == 4 &&
               !data.cancel_record_q[0].shadow_restore_consumed,
               "t2 update did not bind the exact cancel record identity");
        snapshot = next_snapshot();
        snapshot.sq_cancel_count = 2;
        lsq_ctrl.advance_rtl_admission_sample(snapshot, 1'b0);
        check_expect(data.cancel_record_q[0].shadow_restore_consumed &&
               data.cancel_record_q[0].shadow_restore_sample_seq == sample_seq &&
               lsq_ctrl.shadow_sq_enq_ptr == expected_sq_ptr &&
               lsq_ctrl.shadow_sq_credit == MEMBLOCK_SQ_SIZE - 2,
               "non-zero t3 restore did not rewind SQ pointer 4->2 and consume its credential");
        data.cancel_record_q.delete();
    endtask:check_cancel_shadow_restore_credential

    task check_atomic_fault_redirect_classification();
        main_control_transaction amo;
        status_transaction status;
        memblock_op_behavior_t behavior;
        memblock_redirect_payload_t redirect;

        amo = main_control_transaction::type_id::create("soft_atomic_fault_redirect");
        amo.op_class = MEMBLOCK_OP_CLASS_AMO;
        amo.fuType = MEMBLOCK_FUTYPE_MOU;
        amo.fuOpType = MEMBLOCK_LSUOP_AMOADD_D;
        amo.lsq_flow = MEMBLOCK_LSQ_FLOW_ATOMIC;
        amo.numLsElem = 0;
        behavior = memblock_op_behavior_util::derive_op_behavior(amo);
        status = status_transaction::type_id::create("soft_atomic_fault_status");
        status.active = 1'b1;
        status.rob_commit = 1'b1;
        status.robIdx_flag = 1'b0;
        status.robIdx_value = MEMBLOCK_ROB_VALUE_W'(3);
        status.atomic_piece_fire_count = 2;
        status.atomic_finalized = 1'b1;
        status.fault = 1'b1;
        redirect = '{default:'0};
        redirect.valid = 1'b1;
        redirect.flush_itself = 1'b1;
        redirect.rob_key = status.get_rob_key();
        check_expect(data.fired_atomic_redirect_is_self_fault(
                   status, behavior, redirect, 1'b1),
               "fired atomic self fault-head redirect was rejected");
        check_expect(!data.fired_atomic_redirect_is_self_fault(
                    status, behavior, redirect, 1'b0),
               "external/non-fault-head redirect accepted fired atomic pieces");
        redirect.flush_itself = 1'b0;
        check_expect(!data.fired_atomic_redirect_is_self_fault(
                    status, behavior, redirect, 1'b1),
               "non-self redirect accepted fired atomic pieces");
    endtask:check_atomic_fault_redirect_classification

    task check_atomic_issue_fields();
        main_control_transaction amo;
        main_control_transaction store;
        lintsissue_agent_agent_xaction issue_tr;
        memblock_issue_q_item_t item;
        bit [63:0] std_data[4];
        bit [7:0] pdest0;
        bit [7:0] pdest2;

        amo = main_control_transaction::type_id::create("soft_amocas_q");
        amo.uid = 9;
        amo.op_class = MEMBLOCK_OP_CLASS_AMO;
        amo.fuType = MEMBLOCK_FUTYPE_MOU;
        amo.fuOpType = MEMBLOCK_LSUOP_AMOCAS_Q;
        amo.lsq_flow = MEMBLOCK_LSQ_FLOW_ATOMIC;
        amo.src_0 = 64'h8123_4567_89ab_cdef;
        amo.imm = 0;
        amo.numLsElem = 0;
        memblock_op_behavior_util::freeze_atomic_std_operands(amo);
        for (int unsigned slot = 0; slot < 4; slot++)
            std_data[slot] = amo.std_data_override[slot];
        for (int unsigned lhs = 0; lhs < 4; lhs++) begin
            for (int unsigned rhs = lhs + 1; rhs < 4; rhs++) begin
                check_expect(std_data[lhs] != std_data[rhs],
                       $sformatf("CAS.Q STD data aliases slots %0d/%0d", lhs, rhs));
            end
        end
        pdest0 = field_assigner.compute_atomic_sta_pdest(amo.uid, 0, 1'b1);
        pdest2 = field_assigner.compute_atomic_sta_pdest(amo.uid, 2, 1'b1);
        check_expect(pdest0 != pdest2, "CAS.Q STA0/STA2 pdest must be distinct");
        amo.update_vaddr();
        issue_tr = lintsissue_agent_agent_xaction::type_id::create("soft_override_issue");
        item = '{default:'0};
        foreach (amo.std_data_override_valid[slot]) begin
            amo.std_data_override_valid[slot] = 1;
            amo.std_data_override[slot] = 64'h1122_3344_5566_0000 + slot;
        end
        check_expect(amo.validate_main_transaction(), "complete CAS.Q override mask rejected");
        for (int slot = 3; slot >= 0; slot--) begin
            item.uop_index = slot;
            field_assigner.assign_std_main_fields(issue_tr, amo, item, 0);
            check_expect(issue_tr.io_ooo_to_mem_issueStd_0_bits_src_0 == amo.std_data_override[slot],
                "STD port did not consume selected frozen slot");
        end
        amo.std_data_override_valid[2] = 0;
        check_expect(!amo.validate_main_transaction(), "partial CAS.Q override mask accepted");
        amo.std_data_override_valid[3] = 0;
        amo.fuOpType = MEMBLOCK_LSUOP_AMOCAS_D;
        check_expect(amo.validate_main_transaction(), "CAS.D override mask rejected");
        amo.fuOpType = MEMBLOCK_LSUOP_AMOADD_D;
        check_expect(!amo.validate_main_transaction(), "non-CAS AMO inherited CAS override");
        store = make_scalar("soft_store_override", MEMBLOCK_OP_CLASS_STORE, 0);
        check_expect(field_assigner.derive_std_data(store, 0) == store.src_0, "legacy Store data fallback changed");
        store.std_data_override_valid[0] = 1;
        store.std_data_override[0] = 64'hfedc_ba98_7654_3210;
        check_expect(store.validate_main_transaction(), "Store slot0 override rejected");
        item.uop_index = 0;
        field_assigner.assign_std_main_fields(issue_tr, store, item, 0);
        check_expect(issue_tr.io_ooo_to_mem_issueStd_0_bits_src_0 == store.std_data_override[0],
            "ordinary Store port ignored override");
        store.std_data_override_valid[1] = 1;
        check_expect(!store.validate_main_transaction(), "Store inherited a second STD override slot");
    endtask:check_atomic_issue_fields

    task check_amo_addr_reuse_type_guard();
        localparam int unsigned AMO_ENTRY_COUNT = 16;
        int unsigned saved_op_class_int_load_wt;
        int unsigned saved_op_class_fp_load_wt;
        int unsigned saved_op_class_store_wt;
        int unsigned saved_op_class_prefetch_wt;
        int unsigned saved_op_class_amo_wt;
        int unsigned saved_op_class_cbo_wt;
        int unsigned saved_addr_reuse_en_1_wt;
        int unsigned saved_addr_reuse_en_0_wt;
        int unsigned saved_addr_reuse_load_after_store_wt;
        int unsigned saved_addr_reuse_load_after_load_wt;
        int unsigned saved_addr_reuse_store_after_load_wt;
        int unsigned saved_addr_reuse_store_after_store_wt;
        bit saved_boundary_profile_gen_en;
        memblock_uid_t recent_load_uid_q[$];
        memblock_uid_t recent_store_uid_q[$];
        main_control_transaction tr;

        saved_op_class_int_load_wt = seq_csr_common::op_class_int_load_wt;
        saved_op_class_fp_load_wt = seq_csr_common::op_class_fp_load_wt;
        saved_op_class_store_wt = seq_csr_common::op_class_store_wt;
        saved_op_class_prefetch_wt = seq_csr_common::op_class_prefetch_wt;
        saved_op_class_amo_wt = seq_csr_common::op_class_amo_wt;
        saved_op_class_cbo_wt = seq_csr_common::op_class_cbo_wt;
        saved_addr_reuse_en_1_wt = seq_csr_common::addr_reuse_en_1_wt;
        saved_addr_reuse_en_0_wt = seq_csr_common::addr_reuse_en_0_wt;
        saved_addr_reuse_load_after_store_wt =
            seq_csr_common::addr_reuse_load_after_store_wt;
        saved_addr_reuse_load_after_load_wt =
            seq_csr_common::addr_reuse_load_after_load_wt;
        saved_addr_reuse_store_after_load_wt =
            seq_csr_common::addr_reuse_store_after_load_wt;
        saved_addr_reuse_store_after_store_wt =
            seq_csr_common::addr_reuse_store_after_store_wt;
        saved_boundary_profile_gen_en = seq_csr_common::boundary_profile_gen_en;

        seq_csr_common::op_class_int_load_wt = 0;
        seq_csr_common::op_class_fp_load_wt = 0;
        seq_csr_common::op_class_store_wt = 0;
        seq_csr_common::op_class_prefetch_wt = 0;
        seq_csr_common::op_class_amo_wt = 1;
        seq_csr_common::op_class_cbo_wt = 0;
        seq_csr_common::addr_reuse_en_1_wt = 1;
        seq_csr_common::addr_reuse_en_0_wt = 0;
        seq_csr_common::addr_reuse_load_after_store_wt = 1;
        seq_csr_common::addr_reuse_load_after_load_wt = 0;
        seq_csr_common::addr_reuse_store_after_load_wt = 0;
        seq_csr_common::addr_reuse_store_after_store_wt = 0;
        seq_csr_common::boundary_profile_gen_en = 1'b0;

        build_random_main_table(AMO_ENTRY_COUNT);
        for (memblock_uid_t uid = 0; uid < AMO_ENTRY_COUNT; uid++) begin
            tr = data.get_main_transaction(uid);
            check_expect(tr.op_class == MEMBLOCK_OP_CLASS_AMO,
                   $sformatf("100%% address reuse rewrote AMO uid=%0d to op_class=%s",
                             uid, op_class_name(tr.op_class)));
            check_expect(is_amo_fuoptype(tr.fuOpType),
                   $sformatf("AMO-only uid=%0d has illegal atomic fuOpType=0x%0h",
                             uid, tr.fuOpType));
            check_expect(tr.send_pri == 100 && tr.send_pri_std == 100,
                   $sformatf("AMO-only uid=%0d lost fixed priority send/std=%0d/%0d",
                             uid, tr.send_pri, tr.send_pri_std));
        end

        tr = make_scalar("soft_normal_addr_reuse", MEMBLOCK_OP_CLASS_INT_LOAD,
                         AMO_ENTRY_COUNT);
        tr.uid = memblock_uid_t'(AMO_ENTRY_COUNT);
        tr.send_pri = 37;
        tr.send_pri_std = 42;
        apply_addr_reuse_window(tr, tr.uid, recent_load_uid_q, recent_store_uid_q);
        check_expect(tr.op_class == MEMBLOCK_OP_CLASS_STORE &&
               memblock_op_behavior_util::is_store_fuoptype(tr.fuOpType),
               "ordinary no-reference address-reuse fallback no longer selects a legal store");
        check_expect(tr.send_pri == 37 && tr.send_pri_std == 42,
               "ordinary address reuse unexpectedly inherited AMO priority");

        seq_csr_common::op_class_int_load_wt = saved_op_class_int_load_wt;
        seq_csr_common::op_class_fp_load_wt = saved_op_class_fp_load_wt;
        seq_csr_common::op_class_store_wt = saved_op_class_store_wt;
        seq_csr_common::op_class_prefetch_wt = saved_op_class_prefetch_wt;
        seq_csr_common::op_class_amo_wt = saved_op_class_amo_wt;
        seq_csr_common::op_class_cbo_wt = saved_op_class_cbo_wt;
        seq_csr_common::addr_reuse_en_1_wt = saved_addr_reuse_en_1_wt;
        seq_csr_common::addr_reuse_en_0_wt = saved_addr_reuse_en_0_wt;
        seq_csr_common::addr_reuse_load_after_store_wt =
            saved_addr_reuse_load_after_store_wt;
        seq_csr_common::addr_reuse_load_after_load_wt =
            saved_addr_reuse_load_after_load_wt;
        seq_csr_common::addr_reuse_store_after_load_wt =
            saved_addr_reuse_store_after_load_wt;
        seq_csr_common::addr_reuse_store_after_store_wt =
            saved_addr_reuse_store_after_store_wt;
        seq_csr_common::boundary_profile_gen_en = saved_boundary_profile_gen_en;
    endtask:check_amo_addr_reuse_type_guard
    virtual task body();
        ensure_dispatch_runtime_helpers();
        check_fixed_lane_prefix_and_partial();
        check_source_offer_target_sample();
        check_driver_launch_sample_binding();
        check_reset_pending_reservation_cancel();
        check_cancel_shadow_restore_credential();
        check_atomic_fault_redirect_classification();
        check_atomic_issue_fields();
        check_amo_addr_reuse_type_guard();
        check_cas_prefill_parameters();
        check_cas_prefill_groups();
        check_cas_prefill_protection_and_builders();
        check_atomic_held_piece_stability();
        check_atomic_token_reset_binding();
        data.publish_lsq_sequence_local_state(1'b1, 1'b0);
        check_expect(!data.runtime_drain_complete(),
               "runtime drain ignored sequence-local pending sample");
        data.publish_lsq_sequence_local_state(1'b0, 1'b1);
        check_expect(!data.runtime_drain_complete(),
               "runtime drain ignored atomic token/frozen offer");
        data.publish_lsq_sequence_local_state(1'b0, 1'b0);
        `uvm_info(get_type_name(), "LSQ/AMO directed software checks completed", UVM_LOW)
    endtask:body

endclass:soft_test_memblock_lsq_amo_admission_sequence

`endif
