`ifndef MEMBLOCK_VIRTUALIZATION_FRAMEWORK_SELFTEST_VSEQ__SV
`define MEMBLOCK_VIRTUALIZATION_FRAMEWORK_SELFTEST_VSEQ__SV

// 中文注释：仅调用生产helper检查框架合同，不发射访存或判断DUT执行结果。
// fence参数沿冻结plus读取；单类型/混合分布通过独立运行选择，不在测试中修改参数真源。
class memblock_virtualization_framework_selftest_vseq extends virtual_base_sequence;
    `uvm_object_utils(memblock_virtualization_framework_selftest_vseq)
    int unsigned check_count;

    function new(string name="memblock_virtualization_framework_selftest_vseq");
        super.new(name);
        check_count = 0;
    endfunction

    function void require_true(input bit condition, input string reason);
        if (!condition) `uvm_fatal("VIRTUALIZATION_SELFTEST", reason)
        check_count++;
    endfunction

    // 抽象职责：检查13种操作的分类、宽度和整数写回，再覆盖自动地址的三个生产出口。
    task check_guest_operations();
        bit [8:0] ops[13] = '{MEMBLOCK_LSUOP_HLVB,MEMBLOCK_LSUOP_HLVH,
            MEMBLOCK_LSUOP_HLVW,MEMBLOCK_LSUOP_HLVD,MEMBLOCK_LSUOP_HLVBU,
            MEMBLOCK_LSUOP_HLVHU,MEMBLOCK_LSUOP_HLVWU,MEMBLOCK_LSUOP_HLVXHU,
            MEMBLOCK_LSUOP_HLVXWU,MEMBLOCK_LSUOP_HSVB,MEMBLOCK_LSUOP_HSVH,
            MEMBLOCK_LSUOP_HSVW,MEMBLOCK_LSUOP_HSVD};
        int unsigned widths[13] = '{1,2,4,8,1,2,4,2,4,1,2,4,8};
        memblock_dispatch_base_sequence builder;
        issue_field_assigner assigner;
        common_data_transaction isolated_data;
        mmu_csr_runtime_state snapshot;
        bit [8:0] fp_ops[$];
        builder = new("guest_selftest_builder");
        assigner = new("guest_selftest_assigner");
        isolated_data = new("guest_selftest_data");
        snapshot = new("guest_selftest_snapshot");
        require_true(seq_csr_common::get_boundary_profile_gen_en(),
            "selftest requires MEMBLOCK_BOUNDARY_PROFILE_GEN_EN=1 for real boundary helper coverage");
        foreach (ops[i]) begin
            main_control_transaction tr, reference;
            memblock_op_behavior_t behavior;
            bit rf_wen, fp_wen;
            tr = new($sformatf("guest_op_%0d",i));
            tr.op_class = i < 9 ? MEMBLOCK_OP_CLASS_INT_LOAD : MEMBLOCK_OP_CLASS_STORE;
            builder.apply_op_class_template(tr,ops[i]);
            behavior = memblock_op_behavior_util::derive_op_behavior(tr);
            require_true(i < 9 ? (behavior.route_load && !behavior.route_sta && !behavior.route_std) :
                (!behavior.route_load && behavior.route_sta && behavior.route_std),
                $sformatf("op %0d route mismatch",i));
            require_true(memblock_op_behavior_util::is_hlvx(tr.fuType,tr.fuOpType) == (i inside {7,8}),
                "HLVX attribute mismatch");
            require_true(builder.derive_size_bytes(tr.op_class,tr.fuOpType) == widths[i],
                $sformatf("op %0d width mismatch",i));
            assigner.derive_wen(tr,rf_wen,fp_wen);
            require_true(!fp_wen && (i >= 9 || rf_wen),"Guest load used FP writeback");
            builder.apply_legal_addr_template(tr);
            require_true(tr.imm == 0 && tr.src_0 == tr.vaddr,"legal template did not normalize Guest VA");
            for (int profile_i = 1; profile_i <= 6; profile_i++) begin
                memblock_boundary_profile_e profile;
                profile = memblock_boundary_profile_e'(profile_i);
                if (builder.boundary_profile_supported_for_fuop(tr.op_class,tr.fuOpType,profile,widths[i])) begin
                    builder.apply_boundary_addr_template(tr,profile,widths[i]);
                    require_true(tr.imm == 0 && tr.src_0 == tr.vaddr,
                        "boundary template did not normalize Guest VA");
                end
            end
            reference = new("guest_reference");
            reference.op_class = MEMBLOCK_OP_CLASS_INT_LOAD;
            builder.apply_op_class_template(reference,MEMBLOCK_LSUOP_LD);
            reference.src_0 = seq_csr_common::get_main_vaddr_base() + 64'h108;
            reference.imm = 64'hff8;
            reference.update_vaddr();
            builder.fixup_after_addr_reuse(tr,reference,1'b1,"virtualization selftest reuse");
            require_true(tr.imm == 0 && tr.src_0 == reference.vaddr && tr.vaddr == reference.vaddr,
                "reuse copied ordinary negative immediate into Guest input");
            snapshot.priv_dmode = 2'd3;
            snapshot.priv_spvp = 0;
            require_true(isolated_data.effective_data_privilege(tr,snapshot) == 0,"Guest SPVP0 is not U");
            snapshot.priv_spvp = 1;
            require_true(isolated_data.effective_data_privilege(tr,snapshot) == 1,"Guest SPVP1 is not S");
            if (i >= 9) begin
                tr.std_data_override_valid[0] = 1;
                tr.std_data_override[0] = 64'haabbccdd;
                memblock_op_behavior_util::freeze_guest_std_operand(tr);
                tr.src_0 = 64'h12345678;
                memblock_op_behavior_util::freeze_guest_std_operand(tr);
                require_true(assigner.derive_std_data(tr,0) == 64'haabbccdd,
                    "HSV explicit STD operand changed after address update/retry");
                tr.std_data_override_valid[0] = 0;
                memblock_op_behavior_util::freeze_guest_std_operand(tr);
                tr.src_0++;
                memblock_op_behavior_util::freeze_guest_std_operand(tr);
                require_true(assigner.derive_std_data(tr,0) == 64'h12345678,
                    "HSV default STD operand was not frozen once");
            end
        end
        builder.enumerate_fuoptypes(MEMBLOCK_OP_CLASS_FP_LOAD,fp_ops);
        foreach (fp_ops[i]) require_true(!memblock_op_behavior_util::is_guest_read(MEMBLOCK_FUTYPE_LDU,fp_ops[i]),
            "FP candidates include HLV/HLVX");
        begin
            main_control_transaction amo, ordinary, manual;
            memblock_op_behavior_t behavior;
            memblock_uncache_owner_expected_fatal_catcher catcher;
            amo = new("overlap_amo");
            amo.op_class = MEMBLOCK_OP_CLASS_AMO;
            builder.apply_op_class_template(amo,MEMBLOCK_LSUOP_AMOXOR_W);
            behavior = memblock_op_behavior_util::derive_op_behavior(amo);
            require_true(behavior.is_atomic && !memblock_op_behavior_util::is_guest_memory(amo.fuType,amo.fuOpType),
                "AMOXOR overlapping code misclassified as Guest");
            ordinary = new("ordinary_identity");
            ordinary.op_class = MEMBLOCK_OP_CLASS_INT_LOAD;
            builder.apply_op_class_template(ordinary,MEMBLOCK_LSUOP_LD);
            require_true(isolated_data.effective_data_privilege(ordinary,snapshot) == 3,
                "ordinary operation lost dmode identity");
            manual = new("manual_invalid_imm");
            manual.op_class = MEMBLOCK_OP_CLASS_INT_LOAD;
            builder.apply_op_class_template(manual,MEMBLOCK_LSUOP_HLVD);
            manual.src_0 = seq_csr_common::get_main_vaddr_base();
            manual.imm = 8;
            manual.post_manual_config();
            require_true(manual.imm == 8,"manual finalize silently repaired Guest immediate");
            catcher = new("manual_imm_catcher","Guest memory requires integer route and imm=0");
            uvm_report_cb::add(null,catcher);
            builder.validate_main_table_entry(manual,"manual Guest immediate negative");
            uvm_report_cb::delete(null,catcher);
            require_true(catcher.caught_count == 1 && manual.imm == 8,
                "manual final validation did not reject original invalid immediate");
        end
        `uvm_info("VIRTUALIZATION_SELFTEST","guest_operations PASS",UVM_LOW)
    endtask

    // 抽象职责：直接求解SPVP约束并比较完整配置，检查只修改PMM和root不会被丢弃。
    task check_csr_helpers();
        memblock_csr_randomizer randomizer;
        memblock_csr_sequence_cfg_t cfg;
        memblock_csr_pmp_pma_profile_t region;
        csr_ctrl_agent_agent_xaction current, target;
        memblock_dynamic_csr_change_sequence dynamic_child;
        cfg = seq_csr_common::get_csr_sequence_cfg();
        region = '{default:'0};
        current = new("selftest_csr_current");
        target = new("selftest_csr_target");
        memblock_csr_config_state::configure_static_defaults(current);
        randomizer = new("selftest_csr_randomizer");
        for (int value_i = 0; value_i < 2; value_i++) begin
            cfg.init_spvp_0_wt = value_i == 0;
            cfg.init_spvp_1_wt = value_i == 1;
            randomizer.configure(cfg,0,null,region);
            require_true(randomizer.randomize(),"initial SPVP-only distribution unsatisfiable");
            target.copy(current);
            randomizer.apply_to_transaction(target);
            require_true(target.io_ooo_to_mem_tlbCsr_priv_spvp == value_i,"initial SPVP ignored exact weights");
        end
        cfg.change_satp_enable = 0; cfg.change_vsatp_enable = 0; cfg.change_hgatp_enable = 0;
        cfg.change_permission_enable = 0; cfg.change_priv_context_enable = 0;
        cfg.change_pmp_pma_enable = 0; cfg.change_spvp_enable = 0;
        current.io_ooo_to_mem_tlbCsr_priv_spvp = 1;
        randomizer.configure(cfg,1,current,region);
        randomizer.c_dynamic_must_change.constraint_mode(0);
        require_true(randomizer.randomize(),"disabled dynamic SPVP cannot retain committed value");
        target.copy(current); randomizer.apply_to_transaction(target);
        require_true(target.io_ooo_to_mem_tlbCsr_priv_spvp == 1,"disabled dynamic SPVP changed");
        randomizer.c_dynamic_must_change.constraint_mode(1);
        cfg.change_spvp_enable = 1; cfg.change_spvp_0_wt = 1; cfg.change_spvp_1_wt = 0;
        randomizer.configure(cfg,1,current,region);
        require_true(randomizer.randomize(),"SPVP-only dynamic must-change unsatisfiable");
        target.copy(current); randomizer.apply_to_transaction(target);
        require_true(target.io_ooo_to_mem_tlbCsr_priv_spvp == 0 &&
            memblock_csr_config_state::differs_from(current,target),"SPVP-only change lost");
        for (int field_i = 0; field_i < 5; field_i++) begin
            target.copy(current);
            case (field_i)
                0: target.io_ooo_to_mem_tlbCsr_pmm_mseccfg = 2;
                1: target.io_ooo_to_mem_tlbCsr_pmm_menvcfg = 2;
                2: target.io_ooo_to_mem_tlbCsr_pmm_henvcfg = 2;
                3: target.io_ooo_to_mem_tlbCsr_pmm_hstatus = 2;
                4: target.io_ooo_to_mem_tlbCsr_pmm_senvcfg = 2;
            endcase
            require_true(memblock_csr_config_state::differs_from(current,target),"PMM-only target lost");
            begin
                memblock_sync_pkg::dispatch_raw_csr_t raw;
                raw = memblock_csr_config_state::make_runtime_payload(target);
                case (field_i)
                    0: require_true(raw.pmm_mseccfg == 2,"mseccfg PMM missing in runtime payload");
                    1: require_true(raw.pmm_menvcfg == 2,"menvcfg PMM missing in runtime payload");
                    2: require_true(raw.pmm_henvcfg == 2,"henvcfg PMM missing in runtime payload");
                    3: require_true(raw.pmm_hstatus == 2,"hstatus PMM missing in runtime payload");
                    4: require_true(raw.pmm_senvcfg == 2,"senvcfg PMM missing in runtime payload");
                endcase
            end
        end
        dynamic_child = new("selftest_maintenance");
        target.copy(current);
        target.io_ooo_to_mem_tlbCsr_priv_spvp = !current.io_ooo_to_mem_tlbCsr_priv_spvp;
        dynamic_child.build_mapping_maintenance_steps(current,target);
        require_true(dynamic_child.maintenance_steps.size() == 1,"permission-free identical config created fences");
        current.io_ooo_to_mem_tlbCsr_vsatp_mode = 8;
        current.io_ooo_to_mem_tlbCsr_vsatp_ppn = 44'h80004;
        target.copy(current); target.io_ooo_to_mem_tlbCsr_vsatp_asid = 1;
        dynamic_child.build_mapping_maintenance_steps(current,target);
        require_true(dynamic_child.maintenance_steps.size() == 4 &&
            dynamic_child.maintenance_steps[0] == memblock_dynamic_csr_change_sequence::FENCE_VS &&
            dynamic_child.maintenance_steps[1] == memblock_dynamic_csr_change_sequence::WRITE_TARGET &&
            dynamic_child.maintenance_steps[2] == memblock_dynamic_csr_change_sequence::FENCE_VS &&
            dynamic_child.maintenance_steps[3] == memblock_dynamic_csr_change_sequence::FENCE_G,
            "Guest mapping steps are not old VVMA/write/new VVMA/GVMA");
        target.copy(current); current.io_ooo_to_mem_tlbCsr_hgatp_mode = 9;
        target.copy(current); target.io_ooo_to_mem_tlbCsr_pmm_henvcfg = 3;
        dynamic_child.build_mapping_maintenance_steps(current,target);
        require_true(dynamic_child.maintenance_steps.size() == 2 &&
            dynamic_child.maintenance_steps[1] == memblock_dynamic_csr_change_sequence::FENCE_G,
            "PMM16 Sv48x4 transition missed GVMA");
        target.io_ooo_to_mem_tlbCsr_vsatp_asid++;
        dynamic_child.build_mapping_maintenance_steps(current,target);
        require_true(dynamic_child.maintenance_steps.size() == 4,
            "PMM16 Guest mapping transition did not deduplicate GVMA");
        target.copy(current);
        target.io_ooo_to_mem_tlbCsr_satp_ppn++;
        require_true(memblock_csr_config_state::differs_from(current,target),"root-only change lost");
        // Guest期间HS上下文不可达，返回Host时即使SATP未再次变化也必须维护。
        current.io_ooo_to_mem_tlbCsr_priv_virt = 0;
        current.io_ooo_to_mem_tlbCsr_satp_mode = 8;
        target.copy(current);
        target.io_ooo_to_mem_tlbCsr_priv_virt = 1;
        target.io_ooo_to_mem_tlbCsr_satp_ppn++;
        dynamic_child.build_mapping_maintenance_steps(current,target);
        foreach (dynamic_child.maintenance_steps[i])
            require_true(dynamic_child.maintenance_steps[i] != memblock_dynamic_csr_change_sequence::FENCE_HS,
                         "Guest target attempted HS fence in VS context");
        current.copy(target);
        target.io_ooo_to_mem_tlbCsr_priv_virt = 0;
        dynamic_child.build_mapping_maintenance_steps(current,target);
        require_true(dynamic_child.maintenance_steps.size() == 5 &&
            dynamic_child.maintenance_steps[2] == memblock_dynamic_csr_change_sequence::FENCE_HS,
            "return to Host missed deferred inactive HS maintenance");
        `uvm_info("VIRTUALIZATION_SELFTEST","csr_helpers PASS",UVM_LOW)
    endtask

    // 抽象职责：等待真实monitor baseline后，仅构造fence item，验证主表范围和类型冻结。
    task check_fence_helpers();
        memblock_sfence_control_base_sequence worker;
        memblock_sfence_control_action_t action;
        fence_agent_agent_xaction tr, retry_tr;
        memblock_sync_pkg::memblock_control_csr_runtime_baseline_t baseline;
        int unsigned seen[5];
        int unsigned allowed_count, sole_kind;
        worker = new("selftest_fence_worker");
        require_true(memblock_sync_pkg::uses_control_barrier_topology(),
            "selftest requires active control topology for real fence baseline");
        memblock_sync_pkg::request_control_runtime_reset("virtualization framework selftest");
        for (int waited = 0; !memblock_sync_pkg::get_control_csr_runtime_baseline(baseline); waited++) begin
            require_true(waited < 1000,"real monitor baseline did not complete");
            #1ns;
        end
        allowed_count = 0; sole_kind = 0;
        foreach (seen[i]) begin
            seen[i] = 0;
            if (seq_csr_common::get_sfence_type_weight(i) != 0) begin allowed_count++; sole_kind = i; end
        end
        require_true(allowed_count != 0,"fence selftest has no positive type weight");
        for (int trial = 0; trial < 128; trial++) begin
            int unsigned kind;
            action = '{default:'0};
            action.owner = '{valid:1'b1,uid:0,dynamic_epoch:0,action_generation:1,kind:MEMBLOCK_CONTROL_KIND_SFENCE};
            action.scope = memblock_sfence_scope_e'(trial % 4);
            action.addr = trial < 4 ? 0 : 50'h12340;
            action.id = trial < 4 ? 0 : 16'h123;
            worker.configure_sfence_control_xaction(action,tr);
            kind = tr.io_ooo_to_mem_sfence_bits_hg ? (tr.io_ooo_to_mem_sfence_bits_flushPipe ? 2 : 4) :
                tr.io_ooo_to_mem_sfence_bits_hv ? (tr.io_ooo_to_mem_sfence_bits_flushPipe ? 1 : 3) : 0;
            require_true(seq_csr_common::get_sfence_type_weight(kind) != 0,"selected disabled fence type");
            require_true(!(tr.io_ooo_to_mem_sfence_bits_hv && tr.io_ooo_to_mem_sfence_bits_hg) &&
                (tr.io_ooo_to_mem_sfence_bits_hv || tr.io_ooo_to_mem_sfence_bits_hg ||
                 tr.io_ooo_to_mem_sfence_bits_flushPipe),"illegal hv/hg/flushPipe fence combination");
            require_true(tr.io_ooo_to_mem_sfence_valid && action.expected_fence.valid &&
                action.expected_fence.hv == tr.io_ooo_to_mem_sfence_bits_hv &&
                action.expected_fence.hg == tr.io_ooo_to_mem_sfence_bits_hg &&
                action.expected_fence.flushPipe == tr.io_ooo_to_mem_sfence_bits_flushPipe,
                "fence item differs from frozen expected observation");
            seen[kind]++;
            if (allowed_count == 1) require_true(kind == sole_kind,"single positive weight was not fixed type");
            require_true(tr.io_ooo_to_mem_sfence_bits_addr == action.addr &&
                tr.io_ooo_to_mem_sfence_bits_id == action.id,"fence changed raw addr/id (including GVMA units)");
            require_true(tr.io_ooo_to_mem_sfence_bits_rs1 == !((trial % 4) inside {1,3}) &&
                tr.io_ooo_to_mem_sfence_bits_rs2 == !((trial % 4) inside {2,3}),"scope wildcards used numeric zero");
            worker.configure_sfence_control_xaction(action,retry_tr);
            require_true(action.type_frozen &&
                tr.io_ooo_to_mem_sfence_bits_hv == retry_tr.io_ooo_to_mem_sfence_bits_hv &&
                tr.io_ooo_to_mem_sfence_bits_hg == retry_tr.io_ooo_to_mem_sfence_bits_hg &&
                tr.io_ooo_to_mem_sfence_bits_flushPipe == retry_tr.io_ooo_to_mem_sfence_bits_flushPipe &&
                tr.io_ooo_to_mem_sfence_bits_rs1 == retry_tr.io_ooo_to_mem_sfence_bits_rs1 &&
                tr.io_ooo_to_mem_sfence_bits_rs2 == retry_tr.io_ooo_to_mem_sfence_bits_rs2 &&
                tr.io_ooo_to_mem_sfence_bits_addr == retry_tr.io_ooo_to_mem_sfence_bits_addr &&
                tr.io_ooo_to_mem_sfence_bits_id == retry_tr.io_ooo_to_mem_sfence_bits_id,
                "same action retry selected a new type");
        end
        `uvm_info("VIRTUALIZATION_SELFTEST",$sformatf("fence_helpers PASS seen=%p",seen),UVM_LOW)
    endtask

    virtual task body();
        require_virtual_sqr();
        seq_csr_common::init();
        check_guest_operations();
        check_csr_helpers();
        check_fence_helpers();
        `uvm_info("VIRTUALIZATION_SELFTEST",$sformatf("PASS checks=%0d",check_count),UVM_LOW)
    endtask
endclass
`endif
