`ifndef MEMBLOCK_VIRTUALIZATION_RM_SELFTEST_VSEQ__SV
`define MEMBLOCK_VIRTUALIZATION_RM_SELFTEST_VSEQ__SV

// 中文注释：纯软件专项，调用正式RM构造/比较及只读接口；不发射DUT激励。
// 因RM类型晚于seq_pkg定义，本类由tc_pkg在导入env package后注册。
class memblock_virtualization_rm_expected_report extends uvm_report_catcher;
    string expected_id;
    int unsigned caught;
    function new(string id_v); super.new("virtualization_rm_expected"); expected_id=id_v; caught=0; endfunction
    virtual function action_e catch();
        if(get_id()==expected_id && get_severity()==UVM_ERROR) begin caught++;return CAUGHT;end
        return THROW;
    endfunction
endclass

class memblock_virtualization_rm_selftest_vseq extends virtual_base_sequence;
    `uvm_object_utils(memblock_virtualization_rm_selftest_vseq)
    memblock_env test_env;
    common_data_transaction data;
    memblock_rm_readonly_api ro;
    int unsigned checks;
    function new(string name="memblock_virtualization_rm_selftest_vseq"); super.new(name); checks=0; endfunction
    function void expect_true(bit condition,string text);
        checks++;
        if(!condition) `uvm_fatal("VIRTUALIZATION_RM_SELFTEST",text)
    endfunction
    function void passed(string scenario);
        `uvm_info("VIRTUALIZATION_RM_SELFTEST",{"scenario=",scenario," PASS"},UVM_LOW)
    endfunction

    // 抽象职责：用可区分的控制值检查五来源、优先级及禁止掩码分支。
    function void check_pmm_selection();
        expect_true(rm_ls_select_pmm(0,0,0,0,3,3,1,1,2,3,0,0,0)==2,"M PMM must precede MXR");passed("pmm_mseccfg_priority");
        expect_true(rm_ls_select_pmm(0,0,0,0,1,1,0,0,0,2,0,0,0)==2,"Host S menvcfg");passed("pmm_menvcfg");
        expect_true(rm_ls_select_pmm(0,1,1,1,3,0,0,0,0,0,3,2,0)==3,"Guest S must precede U execution HUPMM");passed("pmm_henvcfg_priority");
        expect_true(rm_ls_select_pmm(0,1,1,0,3,0,0,0,0,0,0,2,3)==2,"U HLV HUPMM");passed("pmm_hupmm");
        expect_true(rm_ls_select_pmm(0,0,1,0,0,0,0,0,0,0,0,0,3)==3,"VU senvcfg");passed("pmm_senvcfg");
        expect_true(rm_ls_select_pmm(0,0,0,0,1,1,1,0,0,3,3,3,3)==0,"Host MXR suppresses PMM");
        expect_true(rm_ls_select_pmm(0,1,1,1,1,1,0,1,0,3,3,3,3)==0,"Guest vMXR suppresses PMM");
        expect_true(rm_ls_select_pmm(0,1,1,1,1,1,1,0,0,3,3,3,3)==0,"Guest mMXR suppresses PMM");passed("pmm_mxr_disable");
        expect_true(rm_ls_select_pmm(1,1,1,1,3,3,0,0,3,3,3,3,3)==0,"HLVX always suppresses PMM");passed("pmm_hlvx_disable");
    endfunction

    // 抽象职责：检查数学地址处理、首末边界和独立故障上报，不复制生产计算算法。
    function void address_case(string scenario,bit [63:0] raw,bit [1:0] pmm,mode,stage,
        bit guest,bit checkfull,bit [3:0] satp,vsatp,hgatp,int unsigned size,
        bit [63:0] expected_effective,expected_report,bit [1:0] af,pf,gpf);
        bit [63:0] effective,report_va;
        bit [1:0] actual_af,actual_pf,actual_gpf;
        expect_true(rm_ls_compute_address_precheck(raw,pmm,mode,stage,guest,1,checkfull,
            satp,vsatp,hgatp,size,effective,report_va,actual_af,actual_pf,actual_gpf),{scenario," rejected legal PMM"});
        expect_true(effective==expected_effective && report_va==expected_report &&
            actual_af==af && actual_pf==pf && actual_gpf==gpf,
            $sformatf("%s actual effective=%h report=%h af/pf/gpf=%b/%b/%b",scenario,effective,report_va,actual_af,actual_pf,actual_gpf));
        passed(scenario);
    endfunction
    function void check_address_math();
        bit [63:0] effective,report_va;
        bit [1:0] af,pf,gpf;
        address_case("pmm57_sign_positive",64'hfe00000080001000,2,1,0,0,1,8,0,0,8,64'h80001000,64'h80001000,0,0,0);
        address_case("pmm57_sign_negative",64'h0100000000000000,2,1,0,0,1,9,0,0,1,64'hff00000000000000,64'hff00000000000000,0,3,0);
        address_case("pmm48_sign_negative",64'h1234ffff80001000,3,1,0,0,1,9,0,0,8,64'hffffffff80001000,64'hffffffff80001000,0,0,0);
        address_case("pmm57_zero_bare",64'hfe00000080001000,2,3,0,0,1,0,0,0,8,64'h80001000,64'h80001000,0,0,0);
        address_case("pmm48_zero_onlys2",64'hffff800080001000,3,1,2,1,1,0,0,9,8,64'h0000800080001000,64'h0000800080001000,0,0,0);
        address_case("pmm00_preaf",64'h0001000080001000,0,3,0,0,1,0,0,0,8,64'h0001000080001000,64'h0001000080001000,3,0,0);
        address_case("sv39_last_invalid",64'h0000003ffffffffc,0,1,0,0,1,8,0,0,8,64'h0000003ffffffffc,64'h0000004000000000,0,2,0);
        address_case("sv39_first_invalid",64'hffffffbffffffffc,0,1,0,0,1,8,0,0,8,64'hffffffbffffffffc,64'hffffffbffffffffc,0,1,0);
        address_case("sv48_last_invalid",64'h00007ffffffffffc,0,1,0,0,1,9,0,0,8,64'h00007ffffffffffc,64'h0000800000000000,0,2,0);
        address_case("sv39x4_last_invalid",64'h000001fffffffffc,0,1,2,1,1,0,0,8,8,64'h000001fffffffffc,64'h0000020000000000,0,0,2);
        address_case("sv48x4_last_invalid",64'h0003fffffffffffc,0,1,2,1,1,0,0,9,8,64'h0003fffffffffffc,64'h0004000000000000,0,0,2);
        address_case("physical_last_invalid",64'h0000fffffffffffc,0,3,0,0,1,0,0,0,8,64'h0000fffffffffffc,64'h0001000000000000,2,0,0);
        address_case("later_checkfullva_off",64'h0004000000000000,0,1,2,1,0,0,0,9,8,64'h0004000000000000,64'h0004000000000000,0,0,0);
        address_case("overflow_wrap",64'hfffffffffffffffc,0,3,0,0,1,0,0,0,8,64'hfffffffffffffffc,64'hfffffffffffffffc,1,0,0);
        expect_true(!rm_ls_compute_address_precheck(0,1,1,0,0,1,1,8,0,0,8,effective,report_va,af,pf,gpf),"PMM01 accepted");passed("pmm01_reject");
    endfunction

    // 抽象职责：使用同一PTE改变访问身份及MXR，检查两阶段读取/执行权限彼此独立。
    function void check_permissions();
        memblock_rm_readonly_api::tlb_entry_view_t entry;
        memblock_rm_readonly_api::tlb_request_context_view_t context_v;
        entry='0;context_v='0;
        entry.valid=1;entry.s1_stage_active=1;entry.s2_stage_active=1;
        entry.s1_pte_v=1;entry.s1_pte_r=1;entry.s1_pte_a=1;entry.s1_pte_d=1;
        entry.s2_pte_r=1;entry.s2_pte_a=1;entry.s2_pte_d=1;
        context_v.s2xlate=3;context_v.is_hypervisor_inst=1;context_v.priv_dmode=3;
        context_v.priv_spvp=0;
        expect_true(test_env.rm.observer_stage_permission_fault(entry,context_v,0,1),"SPVP U read supervisor page allowed");
        context_v.priv_spvp=1;
        expect_true(!test_env.rm.observer_stage_permission_fault(entry,context_v,0,1),"SPVP S rejected supervisor page");
        entry.s1_pte_u=1;
        expect_true(test_env.rm.observer_stage_permission_fault(entry,context_v,0,1),"SPVP S ignored VS SUM");
        context_v.priv_vsum=1;
        expect_true(!test_env.rm.observer_stage_permission_fault(entry,context_v,0,1),"VS SUM did not allow user page");passed("spvp_sum_identity");
        entry.s1_pte_u=0;entry.s1_pte_r=0;entry.s1_pte_x=1;
        context_v.priv_vmxr=1;
        expect_true(!test_env.rm.observer_stage_permission_fault(entry,context_v,0,1),"VS MXR did not permit X-only");
        context_v.priv_vmxr=0;context_v.priv_mxr=1;
        expect_true(!test_env.rm.observer_stage_permission_fault(entry,context_v,0,1),"M MXR did not permit VS X-only");
        entry.s2_pte_r=0;entry.s2_pte_x=1;context_v.priv_mxr=0;context_v.priv_vmxr=1;
        expect_true(test_env.rm.observer_stage_permission_fault(entry,context_v,0,0),"VS MXR leaked into G stage");
        context_v.priv_mxr=1;
        expect_true(!test_env.rm.observer_stage_permission_fault(entry,context_v,0,0),"M MXR missing G stage");passed("vs_g_mxr_independent");
        entry.s1_pte_r=1;entry.s1_pte_x=0;entry.s2_pte_r=1;entry.s2_pte_x=0;
        expect_true(test_env.rm.observer_stage_permission_fault(entry,context_v,0,1,1),"HLVX S1 accepted R-only");
        expect_true(test_env.rm.observer_stage_permission_fault(entry,context_v,0,0,1),"HLVX G accepted R-only");
        entry.s1_pte_x=1;entry.s2_pte_x=1;
        expect_true(!test_env.rm.observer_stage_permission_fault(entry,context_v,0,1,1) &&
            !test_env.rm.observer_stage_permission_fault(entry,context_v,0,0,1),"HLVX X page rejected");passed("hlvx_two_stage_x");
    endfunction

    // 抽象职责：通过生产构造入口消费人工的真实形状issue记录；检查13op与Bare合法tag。
    function void build_case(bit [8:0] op,bit store_access,bit [63:0] raw,
        bit [1:0] pmm,output rm_ls_program_item_t item,
        input bit [3:0] satp=0,vsatp=0,hgatp=0,input bit guest=0,input bit [1:0] mode=3);
        mmu_csr_runtime_state csr;
        memblock_uid_tlb_record record_v;
        memblock_rm_readonly_api::main_transaction_view_t main_v;
        memblock_rm_readonly_api::status_view_t status_v;
        bit deferred;
        csr=new("rm_selftest_csr");csr.satp_mode=satp;csr.vsatp_mode=vsatp;csr.hgatp_mode=hgatp;
        csr.priv_virt=guest;csr.priv_dmode=mode;csr.priv_imode=1;csr.priv_spvp=1;
        csr.pmm_mseccfg=pmm;csr.pmm_henvcfg=pmm;csr.pmm_senvcfg=pmm;
        data.main_table_by_uid[0]=new("rm_selftest_main");
        data.main_table_by_uid[0].uid=0;data.main_table_by_uid[0].fuType=store_access?MEMBLOCK_FUTYPE_STU:MEMBLOCK_FUTYPE_LDU;
        data.main_table_by_uid[0].fuOpType=op;data.main_table_by_uid[0].src_0=raw;data.main_table_by_uid[0].vaddr=raw;
        data.main_table_by_uid[0].op_class=store_access?MEMBLOCK_OP_CLASS_STORE:MEMBLOCK_OP_CLASS_INT_LOAD;
        data.status_by_uid[0]=new("rm_selftest_status");data.status_by_uid[0].reset(0);
        data.status_by_uid[0].dynamic_epoch=7;data.status_by_uid[0].robIdx_value=23;
        data.main_table_by_uid[0].robIdx_value=23;
        record_v=new("rm_selftest_record");
        record_v.init_context(0,raw[63:12],(guest || memblock_op_behavior_util::is_guest_memory(data.main_table_by_uid[0].fuType,op)) ?
            (vsatp!=0 ? (hgatp!=0 ? 3 : 1) : 2) : 0,
            memblock_op_behavior_util::is_guest_memory(data.main_table_by_uid[0].fuType,op),csr,10);
        data.uid_tlb_record_by_uid[0]=record_v;data.rm_access_snapshot_by_uid.delete(0);
        data.freeze_access_reference_for_rm(0);
        main_v='0;status_v='0;
        expect_true(ro.read_main_transaction_for_rm(0,main_v) && ro.read_status_for_rm(0,status_v),"main/status views unavailable");
        main_v.atomic_std_valid_mask=1;main_v.atomic_std_data_flat[63:0]=64'haabbccdd;
        expect_true(test_env.rm.observer_build_commit_item_with_observation(main_v,status_v,item,deferred) && !deferred,
            $sformatf("production build failed op=%h error=%s",op,test_env.rm.ls_model.last_error_text));
    endfunction
    function void check_production_opcodes();
        bit [8:0] ops[13]='{MEMBLOCK_LSUOP_HLVB,MEMBLOCK_LSUOP_HLVH,MEMBLOCK_LSUOP_HLVW,MEMBLOCK_LSUOP_HLVD,
            MEMBLOCK_LSUOP_HLVBU,MEMBLOCK_LSUOP_HLVHU,MEMBLOCK_LSUOP_HLVWU,MEMBLOCK_LSUOP_HLVXHU,
            MEMBLOCK_LSUOP_HLVXWU,MEMBLOCK_LSUOP_HSVB,MEMBLOCK_LSUOP_HSVH,MEMBLOCK_LSUOP_HSVW,MEMBLOCK_LSUOP_HSVD};
        int unsigned widths[13]='{1,2,4,8,1,2,4,2,4,1,2,4,8};
        rm_ls_program_item_t item;
        foreach(ops[i]) begin
            build_case(ops[i],i>=9,64'h80001000,0,item);
            expect_true(item.hyperinst && item.kind==(i>=9?RM_LS_KIND_STORE:RM_LS_KIND_LOAD) && item.size_bytes==widths[i] &&
                !item.fp_wen && item.hlvx==(i inside {7,8}) && item.is_signed==(i<4),$sformatf("13op semantic mismatch index=%0d",i));
            if(i>=9) expect_true(item.store_data==64'haabbccdd && item.store_data!=item.computed_vaddr,"HSV address replaced STD");
            passed($sformatf("production_op_%0d",i));
        end
        expect_true(!memblock_op_behavior_util::is_guest_memory(MEMBLOCK_FUTYPE_MOU,MEMBLOCK_LSUOP_AMOXOR_D),"AMOXOR classified Guest");passed("amoxor_isolation");
        build_case(MEMBLOCK_LSUOP_LD,0,64'habcd000080001000,3,item);
        expect_true(item.expected_exception==0 && item.pa_by_byte[0]==64'h80001000 && item.computed_vaddr==64'habcd000080001000,"Bare tagged data geometry");passed("production_bare_tag");
        build_case(MEMBLOCK_LSUOP_LD,0,64'h0001000080001000,0,item);
        expect_true(item.expected_exception[5] && item.pa_valid_mask==0,"preAF required nonexistent PTE");passed("production_preaf_without_pte");
        build_case(MEMBLOCK_LSUOP_LD,0,64'h0000004000001000,0,item,8,0,0,0,1);
        expect_true(item.expected_exception[13] && item.expected_vaddr==64'h0000004000001000 && item.pa_valid_mask==0,"production prePF expected independent address");passed("production_prepf_without_pte");
        build_case(MEMBLOCK_LSUOP_HLVH,0,64'h0004000000001000,0,item,0,0,9);
        expect_true(item.expected_exception[21] && item.expected_gpaddr_valid && item.expected_gpaddr==64'h0004000000001000 && !item.expected_vs_nonleaf,"production preGPF address and VS marker");passed("production_pregpf_without_pte");
        expect_true(memblock_rm_format_load_data(64'h80,MEMBLOCK_LSUOP_HLVB,0,1,1)==64'hffffffffffffff80 &&
            memblock_rm_format_load_data(64'h80,MEMBLOCK_LSUOP_HLVBU,0,0,1)==64'h80 &&
            memblock_rm_format_load_data(64'h8000,MEMBLOCK_LSUOP_HLVXHU,0,0,2)==64'h8000,"Guest signed/unsigned formatter");passed("guest_load_extension");
    endfunction

    // 抽象职责：在真实PMP模型冻结不同权限代次，再由生产HLVX构造合入读/执行AF。
    function void check_hlvx_pmp();
        memblock_pma_pmp_model model;
        rm_ls_program_item_t item;
        model=memblock_pma_pmp_model::get();
        model.reset_and_init_v2_profile();
        model.apply_csr_write(12'h3b0,64'h00003fffffffffff,1);
        model.apply_csr_write(12'h3a0,64'h09,2);
        expect_true(model.capture_uid_context(0,7,1,0,0,0,0,0,10),"read-only PMP context");
        build_case(MEMBLOCK_LSUOP_HLVXHU,0,64'h80001000,0,item);
        expect_true(item.expected_exception[5],"HLVX PMP X denied did not cause Load AF");passed("hlvx_pmp_x_failure");
        model.reset_and_init_v2_profile();
        model.apply_csr_write(12'h3b0,64'h00003fffffffffff,1);
        model.apply_csr_write(12'h3a0,64'h0c,2);
        expect_true(model.capture_uid_context(0,7,1,0,0,0,0,0,10),"execute-only PMP context");
        build_case(MEMBLOCK_LSUOP_HLVXHU,0,64'h80001000,0,item);
        expect_true(item.expected_exception[5],"HLVX PMP R denied did not cause Load AF");passed("hlvx_pmp_r_failure");
        model.reset_and_init_v2_profile();
        model.apply_csr_write(12'h3b0,64'h00003fffffffffff,1);
        model.apply_csr_write(12'h3a0,64'h0f,2);
        expect_true(model.capture_uid_context(0,7,1,0,0,0,0,0,10),"PMP restore context");
    endfunction

    // 抽象职责：直接核对独立S1输入生成的叶/fake/非叶GPA及onlyS2跨页几何。
    function void check_guest_gpa();
        rm_ls_program_item_t item;
        memblock_rm_readonly_api::tlb_entry_view_t entry;
        memblock_rm_readonly_api::tlb_request_context_view_t context_v;
        item=new(0);item.effective_fullva=64'h12345678;entry='0;context_v='0;
        context_v.s2xlate=3;context_v.vsatp_mode=8;
        entry.valid=1;entry.s1_stage_active=1;entry.s1_pte_v=1;entry.s1_pte_r=1;
        entry.s1_entry_ppn_raw=41'h2468;entry.s1_ppn_low[5]=3'h3;
        test_env.rm.observer_compute_guest_fault_address(item,context_v,entry,64'h12345678);
        expect_true(item.expected_gpaddr_valid && !item.expected_vs_nonleaf && item.expected_gpaddr==64'h12343678,"leaf GPA sector/offset");passed("guest_gpa_leaf");
        entry.s1_pte_r=0;entry.s1_level=2;
        test_env.rm.observer_compute_guest_fault_address(item,context_v,entry,64'h12345678);
        expect_true(item.expected_vs_nonleaf && item.expected_gpaddr==64'h12343488,"nonleaf PTE GPA index");passed("guest_gpa_nonleaf");
        entry.s1_pte_v=0;entry.s1_level=2;
        test_env.rm.observer_compute_guest_fault_address(item,context_v,entry,64'h12345678);
        expect_true(item.expected_vs_nonleaf && item.expected_gpaddr_valid && item.expected_gpaddr[11:0]==0,"fake root PTE GPA");passed("guest_gpa_fake");
        context_v.s2xlate=2;item.effective_fullva=64'hfff;
        test_env.rm.observer_compute_guest_fault_address(item,context_v,entry,64'h1000);
        expect_true(item.expected_gpaddr==64'h1000 && !item.expected_vs_nonleaf,"onlyS2 cross-page GPA");passed("guest_gpa_onlys2_cross");
    endfunction

    // 抽象职责：用既有core程序序门槛检查同PA共享数据，异常Store与未提交访问无写效果。
    function void check_memory_effects();
        rm_ls_model_t model;
        rm_ls_program_item_t store_item,load_item,fault_item;
        rm_ls_load_actual_t actual;
        byte unsigned value_v;
        model=new();expect_true(model.configure(3),"core configure");
        store_item=new(0);store_item.valid=1;store_item.kind=RM_LS_KIND_STORE;store_item.op=MEMBLOCK_LSUOP_HSVD;
        store_item.rob.value=1;store_item.main_vaddr=64'h1234;store_item.computed_vaddr=64'h1234;
        store_item.size_bytes=8;store_item.store_data=64'haabbccdd11223344;store_item.store_byte_mask=8'hff;
        store_item.translation_valid=1;store_item.pa_valid_mask=8'hff;
        for(int i=0;i<8;i++)store_item.pa_by_byte[i]=64'h80001000+i;
        expect_true(model.add_item(store_item) && model.consume_if_rob_commit(0,store_item.rob),"uncommitted HSV gate");
        expect_true(!model.read_expected_memory_byte(64'h80001000,value_v) && model.current_uid==0,"replay/uncommitted HSV wrote data");passed("hsv_no_effect_before_commit");
        expect_true(model.commit(store_item.rob),"HSV committed effect");
        load_item=new(1);load_item.valid=1;load_item.kind=RM_LS_KIND_LOAD;load_item.op=MEMBLOCK_LSUOP_LD;
        load_item.rob.value=2;load_item.main_vaddr=64'h8765;load_item.computed_vaddr=64'h8765;
        load_item.size_bytes=8;load_item.translation_valid=1;load_item.pa_valid_mask=8'hff;
        for(int i=0;i<8;i++)load_item.pa_by_byte[i]=64'h80001000+i;
        actual=new();actual.valid=1;actual.uid=1;actual.rob=load_item.rob;actual.data_valid=1;actual.data=64'haabbccdd11223344;
        expect_true(model.add_item(load_item) && model.push_load_actual(actual) && model.commit(load_item.rob),"ordinary Load did not share HSV HPA overlay");passed("hsv_shared_hpa_overlay");
        fault_item=new(2);fault_item.valid=1;fault_item.kind=RM_LS_KIND_STORE;fault_item.op=MEMBLOCK_LSUOP_HSVD;
        fault_item.rob.value=3;fault_item.main_vaddr=8;fault_item.computed_vaddr=8;fault_item.size_bytes=8;
        fault_item.expected_exception[23]=1;fault_item.store_data=0;fault_item.translation_valid=1;fault_item.pa_valid_mask=8'hff;
        for(int i=0;i<8;i++)fault_item.pa_by_byte[i]=64'h80001000+i;
        expect_true(model.add_item(fault_item) && model.commit(fault_item.rob,24'h800000),"fault HSV compare");
        expect_true(model.read_expected_memory_byte(64'h80001000,value_v) && value_v==8'h44 && model.consumed_commit_count==3,"fault HSV changed shared data");passed("hsv_fault_no_effect");
    endfunction

    // 抽象职责：以真实记录对象调用终态冻结；随后修改原控制和载荷验证值深拷贝。
    function void check_snapshot_history();
        memblock_uid_tlb_record record_v;
        memblock_rm_access_snapshot_t snapshot;
        memblock_tlb_entry entry_v;
        mmu_csr_runtime_state csr;
        memblock_rm_readonly_api::tlb_request_context_view_t context_v;
        memblock_rm_readonly_api::tlb_entry_view_t entry_view;
        memblock_rm_readonly_api::tlb_response_provenance_view_t provenance;
        memblock_later_split_tlb_record later;
        csr=new("snapshot_test_csr");csr.priv_dmode=1;csr.satp_mode=8;csr.pmm_menvcfg=3;
        record_v=new("snapshot_test_record");record_v.init_context(0,52'h80001,0,0,csr,10);
        entry_v=new("snapshot_test_entry");entry_v.lookup_key=csr.make_lookup_key(52'h80001,0);
        entry_v.entry_generation=11;entry_v.s1_entry_ppn_raw=41'h1234;
        entry_v.s1_stage_active=1;entry_v.s1_translation_mode_at_build=8;entry_v.s1_pte_v=1;entry_v.s1_pte_r=1;
        entry_v.s1_addr_low=1;entry_v.s1_valididx[1]=1;entry_v.s1_pteidx[1]=1;
        record_v.copy_entry_fields(entry_v);record_v.mark_completed();record_v.response_token=31;record_v.response_sample_seq=12;
        data.uid_tlb_record_by_uid[0]=record_v;
        data.main_table_by_uid[0].fuType=MEMBLOCK_FUTYPE_LDU;data.main_table_by_uid[0].fuOpType=MEMBLOCK_LSUOP_LD;
        data.main_table_by_uid[0].op_class=MEMBLOCK_OP_CLASS_INT_LOAD;
        data.main_table_by_uid[0].src_0=64'h80001ffc;data.main_table_by_uid[0].imm=0;data.main_table_by_uid[0].vaddr=64'h80001ffc;
        later=new("snapshot_test_later");later.dynamic_epoch=data.status_by_uid[0].dynamic_epoch;
        later.rob_key=data.status_by_uid[0].get_rob_key();later.split_vpn=52'h80002;later.s2xlate=0;
        later.request_lookup_key=csr.make_lookup_key(52'h80002,0);later.entry_anchor_key=later.request_lookup_key;
        later.payload.copy_from(entry_v);later.payload.lookup_key=later.entry_anchor_key;
        later.entry_generation=22;later.payload.entry_generation=22;later.issue_hit=1;later.response_sample_seq=13;
        later.state=memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_COMPLETED;
        data.later_split_tlb_record_by_uid[0]=later;data.rm_access_snapshot_by_uid.delete(0);
        data.freeze_access_reference_for_rm(0);
        expect_true(data.get_rm_access_snapshot(0,snapshot) && snapshot.page_count==2 && snapshot.page_valid[0] && snapshot.page_valid[1],"two-page snapshot absent");
        record_v.csr_snapshot.pmm_menvcfg=0;record_v.payload.s1_entry_ppn_raw=41'h7777;later.payload.s1_entry_ppn_raw=41'h8888;
        record_v.mark_canceled();data.tlb_entry_by_key.delete();
        expect_true(snapshot.address_csr.pmm_menvcfg==3 && snapshot.pages[0].s1_entry_ppn_raw==41'h1234 && snapshot.pages[1].s1_entry_ppn_raw==41'h1234,"cancel/current invalidation changed frozen reference");passed("snapshot_deep_copy_two_pages");
        expect_true(ro.read_access_translation_snapshot_for_rm(0,snapshot.dynamic_epoch,snapshot.rob_key,52'h80002,context_v,entry_view,provenance)==memblock_rm_readonly_api::RM_ACCESS_FOUND && provenance.entry_generation==22,"old later hit lost after current invalidation");passed("snapshot_later_hit_after_fence");
        expect_true(ro.read_access_translation_snapshot_for_rm(0,snapshot.dynamic_epoch+1,snapshot.rob_key,52'h80002,context_v,entry_view,provenance)==memblock_rm_readonly_api::RM_ACCESS_MISSING,"stale dynamic snapshot accepted");
        data.status_by_uid[0].replay_seq++;
        expect_true(ro.read_access_translation_snapshot_for_rm(0,snapshot.dynamic_epoch,snapshot.rob_key,52'h80002,context_v,entry_view,provenance)!=memblock_rm_readonly_api::RM_ACCESS_FOUND,"stale replay snapshot accepted");
        data.status_by_uid[0].replay_seq--;passed("snapshot_instance_reject");
        snapshot.reset_epoch++;
        expect_true(!ro.read_tlb_request_context_for_rm(0,52'h80001,context_v),"Bare/precheck accepted old reset");
        snapshot.reset_epoch--;snapshot.issue_epoch++;
        expect_true(!ro.read_tlb_request_context_for_rm(0,52'h80001,context_v),"Bare/precheck accepted old issue");
        snapshot.issue_epoch--;snapshot.rob_key.value++;
        expect_true(!ro.read_tlb_request_context_for_rm(0,52'h80001,context_v),"Bare/precheck accepted old ROB");
        snapshot.rob_key.value--;passed("snapshot_control_identity_reject");
        snapshot.source[1]=0;
        expect_true(ro.read_access_translation_snapshot_for_rm(0,snapshot.dynamic_epoch,snapshot.rob_key,52'h80002,context_v,entry_view,provenance)!=memblock_rm_readonly_api::RM_ACCESS_FOUND,"missing later provenance accepted");
        snapshot.source[1]=3;snapshot.generation[1]++;
        expect_true(ro.read_access_translation_snapshot_for_rm(0,snapshot.dynamic_epoch,snapshot.rob_key,52'h80002,context_v,entry_view,provenance)!=memblock_rm_readonly_api::RM_ACCESS_FOUND,"generation mismatch accepted");
        snapshot.generation[1]--;passed("snapshot_provenance_reject");
    endfunction

    // 抽象职责：区分范围内可证明的相同/不同PA与参考不足，不把key变化直接当DUT错。
    function void check_pmm_reference();
        bit [63:0] implementation_pa,architectural_pa;
        rm_pmm_reference_result_e result_v;
        result_v=rm_ls_check_pmm_stage2_reference(52'h200,52'h200,52'h200,0,0,38'h1234,12'h678,implementation_pa,architectural_pa);
        expect_true(result_v==RM_PMM_REFERENCE_MATCH && implementation_pa==64'h1234678 && architectural_pa==64'h1234678,"equal VPN reference PA");passed("pmm_reference_match");
        result_v=rm_ls_check_pmm_stage2_reference(52'h200,52'h201,52'h200,1,0,38'h1200,0,implementation_pa,architectural_pa);
        expect_true(result_v==RM_PMM_REFERENCE_MISMATCH && implementation_pa==64'h1200000 && architectural_pa==64'h1201000,"covered superpage failed independent VPN offset");passed("pmm_reference_covered_mismatch");
        result_v=rm_ls_check_pmm_stage2_reference(52'h200,52'h201,52'h200,0,0,38'h1234,0,implementation_pa,architectural_pa);
        expect_true(result_v==RM_PMM_REFERENCE_MISSING,"4K key difference must require independent evidence");passed("pmm_reference_missing_4k");
        result_v=rm_ls_check_pmm_stage2_reference(52'h1000000200,52'h200,52'h1000000200,3,0,38'h1234,0,implementation_pa,architectural_pa);
        expect_true(result_v==RM_PMM_REFERENCE_MISSING,"Sv48x4 bit48 masked edge falsely covered by level3");passed("pmm_reference_missing_high_tag");
    endfunction

    // 抽象职责：验证raw tag保留在原VA中，实际分段request/data/response使用低50位同形地址。
    function void check_tagged_first_split();
        memblock_rob_key_t key;
        memblock_rob_map_key_t map_key;
        memblock_uid_t saved_owner;
        bit owner_present,pending,saved_active;
        longint unsigned saved_sample,saved_reset,reset_v;
        bit [63:0] raw_va,request_va;
        memblock_first_split_access_context_t context_v;
        key=data.status_by_uid[0].get_rob_key();map_key=rob_order_util::rob_to_map_key(key);
        owner_present=data.uid_by_active_rob.exists(map_key);
        if(owner_present)saved_owner=data.uid_by_active_rob[map_key];
        saved_active=data.status_by_uid[0].active;data.status_by_uid[0].active=1;
        data.uid_by_active_rob[map_key]=0;
        saved_sample=data.split_observed_sample_seq;saved_reset=data.split_observed_reset_epoch;
        reset_v=memblock_sync_pkg::get_l2tlb_current_reset_epoch();
        data.main_table_by_uid[0].fuType=MEMBLOCK_FUTYPE_LDU;data.main_table_by_uid[0].fuOpType=MEMBLOCK_LSUOP_HLVH;
        for(int high_bit=0;high_bit<2;high_bit++) begin
            raw_va=high_bit ? 64'habce000080001fff : 64'habcc000080001fff;
            request_va={{14{raw_va[49]}},raw_va[49:0]};
            data.main_table_by_uid[0].vaddr=raw_va;
            data.status_by_uid[0].first_split_access='{default:'0};
            data.observe_first_split_request_sample(1,key,request_va,10,reset_v);
            data.observe_first_split_data_sample(1,key,request_va,1,20,reset_v);
            data.observe_first_split_response_sample(1,key,request_va,0,0,0,30,reset_v);
            data.split_observed_sample_seq=40;data.split_observed_reset_epoch=reset_v;
            expect_true(ro.try_read_first_split_access_context_for_rm(0,data.status_by_uid[0].dynamic_epoch,
                key,raw_va,40,context_v,pending) && !pending && context_v.original_vaddr==raw_va &&
                context_v.request_vaddr==request_va && context_v.data_sample_seq==20,
                "tagged raw VA lost low50 first-segment provenance");
            passed(high_bit ? "tagged_firstsplit_bit49_one" : "tagged_firstsplit_bit49_zero");
        end
        data.status_by_uid[0].first_split_access='{default:'0};
        data.split_observed_sample_seq=saved_sample;data.split_observed_reset_epoch=saved_reset;
        if(owner_present)data.uid_by_active_rob[map_key]=saved_owner;else data.uid_by_active_rob.delete(map_key);
        data.status_by_uid[0].active=saved_active;
    endfunction

    // 抽象职责：验证issue拍只复制预存masked映射，终态冻结后不读取重映射的新generation。
    function void check_masked_reference_history();
        mmu_csr_runtime_state csr;
        memblock_uid_tlb_record record_v;
        memblock_tlb_entry canonical;
        memblock_tlb_lookup_key_t key;
        memblock_rm_access_snapshot_t snapshot;
        rm_ls_program_item_t semantic_item;
        memblock_pma_pmp_model pmp;
        memblock_rm_readonly_api::tlb_request_context_view_t context_v;
        memblock_rm_readonly_api::tlb_entry_view_t entry_view;
        csr=new("masked_history_csr");csr.priv_dmode=1;csr.priv_virt=1;
        csr.vsatp_mode=0;csr.hgatp_mode=9;csr.hgatp_vmid=3;csr.pmm_senvcfg=3;
        key=csr.make_lookup_key(52'h200,2);
        canonical=new("masked_history_canonical");canonical.lookup_key=key;canonical.s2xlate=2;
        canonical.s2_stage_active=1;canonical.s2_translation_mode_at_build=9;
        canonical.s2_vmid=3;canonical.s2_tag=38'h200;canonical.s2_entry_ppn_raw=38'h80001;
        canonical.s2_pte_r=1;canonical.s2_pte_w=1;canonical.s2_pte_x=1;
        canonical.s2_pte_u=1;canonical.s2_pte_a=1;canonical.s2_pte_d=1;
        canonical.entry_generation=data.allocate_tlb_entry_generation();
        canonical.payload_phase=memblock_tlb_entry::MEMBLOCK_TLB_PAYLOAD_FINALIZED;
        data.publish_live_tlb_entry(key,canonical,csr);
        data.main_table_by_uid[0].fuType=MEMBLOCK_FUTYPE_LDU;data.main_table_by_uid[0].fuOpType=MEMBLOCK_LSUOP_LD;
        data.main_table_by_uid[0].op_class=MEMBLOCK_OP_CLASS_INT_LOAD;
        data.main_table_by_uid[0].src_0=64'h0001000000200000;data.main_table_by_uid[0].vaddr=64'h0001000000200000;
        record_v=new("masked_history_record");record_v.init_context(0,52'h1000000200,2,0,csr,10);
        data.capture_pmm_arch_reference_on_issue(record_v);
        expect_true(record_v.pmm_arch_reference_valid[0] && record_v.pmm_arch_vpn[0]==52'h200 &&
            record_v.pmm_arch_reference[0].s2_entry_ppn_raw==38'h80001,"issue did not capture independent preexisting masked entry");passed("masked_reference_issue_capture");
        data.uid_tlb_record_by_uid[0]=record_v;data.rm_access_snapshot_by_uid.delete(0);
        canonical.s2_entry_ppn_raw=38'h90002;
        data.delete_live_tlb_entry_by_anchor_key(key,"masked reference software invalidation");
        data.freeze_access_reference_for_rm(0);
        expect_true(data.get_rm_access_snapshot(0,snapshot) && snapshot.pmm_arch_reference_valid[0] &&
            ro.read_pmm_arch_reference_for_rm(0,snapshot.dynamic_epoch,snapshot.rob_key,snapshot.pmm_arch_vpn[0],context_v,entry_view) &&
            context_v.request_vpn==52'h200 && entry_view.s2_entry_ppn_raw==38'h80001 && entry_view.entry_generation==canonical.entry_generation &&
            context_v.request_s2_resolved_ppn==44'h80001,"frozen masked reference followed changed/deleted canonical mapping");passed("masked_reference_deep_copy_after_fence");
        semantic_item=new(0);semantic_item.valid=1;semantic_item.dynamic_epoch=snapshot.dynamic_epoch;
        semantic_item.rob.flag=snapshot.rob_key.flag;semantic_item.rob.value=snapshot.rob_key.value;
        semantic_item.kind=RM_LS_KIND_LOAD;semantic_item.size_bytes=1;
        semantic_item.pmm_address_semantic_mismatch=1;semantic_item.pmm_reference_result=RM_PMM_REFERENCE_MISSING;
        semantic_item.effective_fullva=64'h200000;semantic_item.pa_valid_mask=1;
        semantic_item.pa_by_byte[0]=64'h80001000;semantic_item.all_normal_cacheable=1;
        test_env.rm.observer_check_pmm_arch_reference(semantic_item);
        expect_true(semantic_item.pmm_reference_result==RM_PMM_REFERENCE_MATCH,"independent alias same HPA did not prove semantic match");passed("masked_reference_semantic_match");
        semantic_item.pa_by_byte[0]=64'h80002000;
        test_env.rm.observer_check_pmm_arch_reference(semantic_item);
        expect_true(semantic_item.pmm_reference_result==RM_PMM_REFERENCE_MISMATCH,"independent different HPA did not fail semantics");passed("masked_reference_semantic_mismatch");
        semantic_item.pa_by_byte[0]=64'h80001000;
        snapshot.address_csr.m_pbmt_en=1;snapshot.pmm_arch_reference[0].s2_entry_pbmt=1;
        test_env.rm.observer_check_pmm_arch_reference(semantic_item);
        expect_true(semantic_item.pmm_reference_result==RM_PMM_REFERENCE_MISMATCH,"independent PBMT path mismatch ignored");passed("masked_reference_pbmt_mismatch");
        snapshot.address_csr.m_pbmt_en=0;snapshot.pmm_arch_reference[0].s2_entry_pbmt=0;
        pmp=memblock_pma_pmp_model::get();pmp.reset_and_init_v2_profile();
        pmp.apply_csr_write(12'h3b0,64'h00003fffffffffff,1);pmp.apply_csr_write(12'h3a0,64'h0c,2);
        expect_true(pmp.capture_uid_context(0,7,1,0,0,0,0,0,10),"masked reference PMP deny context");
        test_env.rm.observer_check_pmm_arch_reference(semantic_item);
        expect_true(semantic_item.pmm_reference_result==RM_PMM_REFERENCE_MISMATCH,"independent PMP access mismatch ignored");passed("masked_reference_pmp_mismatch");
        semantic_item.size_bytes=8;semantic_item.pa_valid_mask=8'hff;
        for(int i=0;i<8;i++)semantic_item.pa_by_byte[i]=64'h80001000+64'(i);
        semantic_item.expected_exception[5]=1;
        test_env.rm.observer_check_pmm_arch_reference(semantic_item);
        expect_true(semantic_item.pmm_reference_result==RM_PMM_REFERENCE_MATCH,"aligned same Load PMP AF lost");passed("masked_aligned_load_same_pmp_af");
        semantic_item.kind=RM_LS_KIND_STORE;semantic_item.expected_exception=0;semantic_item.expected_exception[7]=1;
        test_env.rm.observer_check_pmm_arch_reference(semantic_item);
        expect_true(semantic_item.pmm_reference_result==RM_PMM_REFERENCE_MATCH,"aligned same Store PMP AF lost");passed("masked_aligned_store_same_pmp_af");
        semantic_item.expected_exception=0;
        test_env.rm.observer_check_pmm_arch_reference(semantic_item);
        expect_true(semantic_item.pmm_reference_result==RM_PMM_REFERENCE_MISMATCH,"aligned Store PMP AF difference ignored");passed("masked_aligned_store_pmp_difference");
        semantic_item.kind=RM_LS_KIND_LOAD;semantic_item.size_bytes=1;semantic_item.pa_valid_mask=1;
        pmp.reset_and_init_v2_profile();pmp.apply_csr_write(12'h3b0,64'h00003fffffffffff,1);pmp.apply_csr_write(12'h3a0,64'h0f,2);
        expect_true(pmp.capture_uid_context(0,7,1,0,0,0,0,0,10),"masked reference PMP context restore");
        snapshot.pmm_arch_reference[0].s2_pte_r=0;
        test_env.rm.observer_check_pmm_arch_reference(semantic_item);
        expect_true(semantic_item.pmm_reference_result==RM_PMM_REFERENCE_MISMATCH,"independent G-stage permission mismatch ignored");passed("masked_reference_permission_mismatch");
        snapshot.pmm_arch_reference[0].fault_effective_s2_gpf=1;semantic_item.expected_exception[21]=1;
        test_env.rm.observer_check_pmm_arch_reference(semantic_item);
        expect_true(semantic_item.pmm_reference_result==RM_PMM_REFERENCE_MATCH,"same GPF incorrectly required normal PA");passed("masked_reference_same_fault_match");
        snapshot.pmm_arch_reference_valid[0]=0;semantic_item.pmm_reference_result=RM_PMM_REFERENCE_MISSING;
        test_env.rm.observer_check_pmm_arch_reference(semantic_item);
        expect_true(semantic_item.pmm_reference_result==RM_PMM_REFERENCE_MISSING,"missing independent input passed");passed("masked_reference_semantic_missing");
        snapshot.pmm_arch_reference_valid[0]=1;
        snapshot.reset_epoch++;
        expect_true(!ro.read_pmm_arch_reference_for_rm(0,snapshot.dynamic_epoch,snapshot.rob_key,snapshot.pmm_arch_vpn[0],context_v,entry_view),"old reset masked reference accepted");
        snapshot.reset_epoch--;data.status_by_uid[0].replay_seq++;
        expect_true(!ro.read_pmm_arch_reference_for_rm(0,snapshot.dynamic_epoch,snapshot.rob_key,snapshot.pmm_arch_vpn[0],context_v,entry_view),"old replay masked reference accepted");
        data.status_by_uid[0].replay_seq--;passed("masked_reference_instance_reject");
        record_v=new("masked_history_missing");record_v.init_context(0,52'h1000000200,2,0,csr,10);
        data.capture_pmm_arch_reference_on_issue(record_v);
        expect_true(!record_v.pmm_arch_reference_valid[0],"absent masked mapping was created by history capture");passed("masked_reference_absent_no_create");
    endfunction

    // 抽象职责：issue时冻结两份独立4K架构输入，验证跨页换参考及首fault的消费边界。
    function void check_masked_crosspage_reference();
        mmu_csr_runtime_state csr;
        memblock_tlb_entry canonical[2];
        memblock_tlb_lookup_key_t key[2];
        memblock_uid_tlb_record record_v;
        memblock_rm_access_snapshot_t snapshot;
        rm_ls_program_item_t item;
        memblock_rm_readonly_api::tlb_request_context_view_t context_v;
        memblock_rm_readonly_api::tlb_entry_view_t entry_view;
        csr=new("masked_cross_csr");csr.priv_dmode=1;csr.priv_virt=1;
        csr.hgatp_mode=9;csr.hgatp_vmid=3;csr.pmm_senvcfg=3;
        data.main_table_by_uid[0].fuType=MEMBLOCK_FUTYPE_LDU;
        data.main_table_by_uid[0].fuOpType=MEMBLOCK_LSUOP_LD;
        data.main_table_by_uid[0].op_class=MEMBLOCK_OP_CLASS_INT_LOAD;
        data.main_table_by_uid[0].vaddr=64'h0001000000200ffc;
        data.main_table_by_uid[0].src_0=data.main_table_by_uid[0].vaddr;
        for(int page=0;page<2;page++) begin
            key[page]=csr.make_lookup_key(52'h200+52'(page),2);
            canonical[page]=new($sformatf("masked_cross_%0d",page));
            canonical[page].lookup_key=key[page];canonical[page].s2xlate=2;
            canonical[page].s2_stage_active=1;canonical[page].s2_translation_mode_at_build=9;
            canonical[page].s2_vmid=3;canonical[page].s2_tag=38'h200+38'(page);
            canonical[page].s2_entry_ppn_raw=page==0?38'h80001:38'h90002;
            canonical[page].s2_pte_r=1;canonical[page].s2_pte_w=1;canonical[page].s2_pte_x=1;
            canonical[page].s2_pte_u=1;canonical[page].s2_pte_a=1;canonical[page].s2_pte_d=1;
            canonical[page].entry_generation=data.allocate_tlb_entry_generation();
            canonical[page].payload_phase=memblock_tlb_entry::MEMBLOCK_TLB_PAYLOAD_FINALIZED;
            data.publish_live_tlb_entry(key[page],canonical[page],csr);
        end
        record_v=new("masked_cross_record");record_v.init_context(0,52'h1000000200,2,0,csr,10);
        data.capture_pmm_arch_reference_on_issue(record_v);
        expect_true(record_v.pmm_arch_reference_valid[0] && record_v.pmm_arch_reference_valid[1] &&
            record_v.pmm_arch_vpn[0]==52'h200 && record_v.pmm_arch_vpn[1]==52'h201,"crosspage issue did not capture two independent references");
        data.uid_tlb_record_by_uid[0]=record_v;data.rm_access_snapshot_by_uid.delete(0);
        data.freeze_access_reference_for_rm(0);
        expect_true(data.get_rm_access_snapshot(0,snapshot),"crosspage freeze missing");
        for(int page=0;page<2;page++) begin
            canonical[page].s2_entry_ppn_raw=38'hdead;
            data.delete_live_tlb_entry_by_anchor_key(key[page],"crosspage independent snapshot probe");
            expect_true(ro.read_pmm_arch_reference_for_rm(0,snapshot.dynamic_epoch,snapshot.rob_key,
                52'h200+52'(page),context_v,entry_view) && entry_view.s2_entry_ppn_raw==(page==0?38'h80001:38'h90002),
                "crosspage frozen reference followed live mapping");
        end
        passed("masked_crosspage_two_frozen_inputs");
        item=new(0);item.valid=1;item.dynamic_epoch=snapshot.dynamic_epoch;
        item.rob.flag=snapshot.rob_key.flag;item.rob.value=snapshot.rob_key.value;
        item.kind=RM_LS_KIND_LOAD;item.size_bytes=8;item.effective_fullva=64'h200ffc;
        item.pmm_address_semantic_mismatch=1;item.pmm_reference_result=RM_PMM_REFERENCE_MISSING;
        item.pa_valid_mask=8'hff;item.all_normal_cacheable=1;
        for(int i=0;i<8;i++)item.pa_by_byte[i]=i<4?64'h80001ffc+64'(i):64'h90002000+64'(i-4);
        test_env.rm.observer_check_pmm_arch_reference(item);
        expect_true(item.pmm_reference_result==RM_PMM_REFERENCE_MATCH,"two independent 4K references failed crosspage MATCH");passed("masked_crosspage_match");
        snapshot.address_csr.m_pbmt_en=1;snapshot.pmm_arch_reference[1].s2_entry_pbmt=1;
        test_env.rm.observer_check_pmm_arch_reference(item);
        expect_true(item.pmm_reference_result==RM_PMM_REFERENCE_MISMATCH,"later-page PBMT was ignored");passed("masked_crosspage_tail_pbmt");
        snapshot.address_csr.m_pbmt_en=0;snapshot.pmm_arch_reference[1].s2_entry_pbmt=0;
        snapshot.pmm_arch_reference[1].s2_pte_r=0;
        test_env.rm.observer_check_pmm_arch_reference(item);
        expect_true(item.pmm_reference_result==RM_PMM_REFERENCE_MISMATCH,"later-page permission was ignored");passed("masked_crosspage_tail_permission");
        snapshot.pmm_arch_reference[1].fault_effective_s2_gpf=1;item.expected_exception[21]=1;
        test_env.rm.observer_check_pmm_arch_reference(item);
        expect_true(item.pmm_reference_result==RM_PMM_REFERENCE_MATCH,"same later-page GPF did not match");passed("masked_crosspage_tail_fault");
        snapshot.address_csr.m_pbmt_en=1;snapshot.pmm_arch_reference[1].s2_entry_pbmt=1;
        item.expected_exception=0;item.expected_exception[4]=1;
        test_env.rm.observer_check_pmm_arch_reference(item);
        expect_true(item.pmm_reference_result==RM_PMM_REFERENCE_MATCH,"later NC did not override simultaneous GPF");passed("masked_crosspage_nc_fault_priority_match");
        item.expected_exception=0;item.expected_exception[21]=1;
        test_env.rm.observer_check_pmm_arch_reference(item);
        expect_true(item.pmm_reference_result==RM_PMM_REFERENCE_MISMATCH,"later NC priority difference ignored");passed("masked_crosspage_nc_fault_priority_difference");
        snapshot.address_csr.m_pbmt_en=0;snapshot.pmm_arch_reference[1].s2_entry_pbmt=0;
        snapshot.pmm_arch_reference[1].fault_effective_s2_gpf=0;snapshot.pmm_arch_reference[1].s2_pte_r=1;
        snapshot.pmm_arch_reference_valid[1]=0;item.expected_exception=0;
        test_env.rm.observer_check_pmm_arch_reference(item);
        expect_true(item.pmm_reference_result==RM_PMM_REFERENCE_MISSING,"required later-page input missing was accepted");passed("masked_crosspage_tail_missing");
        snapshot.pmm_arch_reference[0].fault_effective_s2_gpf=1;item.expected_exception[21]=1;
        test_env.rm.observer_check_pmm_arch_reference(item);
        expect_true(item.pmm_reference_result==RM_PMM_REFERENCE_MATCH,"first fault incorrectly required unconsumed later page");passed("masked_crosspage_first_fault_no_tail");
        snapshot.pmm_arch_reference[0].fault_effective_s2_gpf=0;item.expected_exception=0;item.expected_exception[5]=1;
        item.expected_dcache_denied=1;item.pa_valid_mask=4'hf;
        test_env.rm.observer_check_pmm_arch_reference(item);
        expect_true(item.pmm_reference_result==RM_PMM_REFERENCE_MATCH,"first data error prefix incorrectly required later page");passed("masked_crosspage_first_data_error_no_tail");
    endfunction

    // 抽象职责：先屏蔽原始tag再做架构末地址加法，保留跨48bit边界产生的新地址位。
    function void check_masked_boundary_carry_capture();
        mmu_csr_runtime_state csr;
        memblock_uid_tlb_record record_v;
        memblock_tlb_entry canonical;
        memblock_tlb_lookup_key_t key[2];
        bit [51:0] vpn[2];
        csr=new("masked_carry_csr");csr.priv_dmode=1;csr.priv_virt=1;
        csr.hgatp_mode=9;csr.hgatp_vmid=3;csr.pmm_senvcfg=3;
        data.main_table_by_uid[0].fuType=MEMBLOCK_FUTYPE_LDU;
        data.main_table_by_uid[0].fuOpType=MEMBLOCK_LSUOP_LD;
        data.main_table_by_uid[0].vaddr=64'h0001fffffffffffc;
        data.main_table_by_uid[0].src_0=data.main_table_by_uid[0].vaddr;
        vpn[0]=52'hfffffffff;vpn[1]=52'h1000000000;
        for(int page=0;page<2;page++)begin
            key[page]=csr.make_lookup_key(vpn[page],2);canonical=new($sformatf("masked_carry_%0d",page));
            canonical.lookup_key=key[page];canonical.s2xlate=2;
            canonical.s2_stage_active=1;canonical.s2_translation_mode_at_build=9;
            canonical.s2_vmid=3;canonical.s2_tag=vpn[page][37:0];canonical.s2_entry_ppn_raw=38'h80001+38'(page);
            canonical.s2_pte_r=1;canonical.s2_pte_w=1;canonical.s2_pte_x=1;
            canonical.s2_pte_u=1;canonical.s2_pte_a=1;canonical.s2_pte_d=1;
            canonical.entry_generation=data.allocate_tlb_entry_generation();
            canonical.payload_phase=memblock_tlb_entry::MEMBLOCK_TLB_PAYLOAD_FINALIZED;
            data.publish_live_tlb_entry(key[page],canonical,csr);
        end
        record_v=new("masked_carry_record");record_v.init_context(0,52'h1fffffffff,2,0,csr,10);
        data.capture_pmm_arch_reference_on_issue(record_v);
        expect_true(record_v.pmm_arch_reference_valid[0] && record_v.pmm_arch_reference_valid[1] &&
            record_v.pmm_arch_vpn[0]==vpn[0] && record_v.pmm_arch_vpn[1]==vpn[1],
            "masked endpoint lost carry above original masked width");passed("masked_crosspage_bit48_carry_capture");
        for(int page=0;page<2;page++)data.delete_live_tlb_entry_by_anchor_key(key[page],"masked carry cleanup");
    endfunction

    // 抽象职责：两个真实形状的后页observer共享一次L2 token，历史同VPN不进入活跃候选。
    function void check_split_shared_token();
        main_control_transaction saved_main[];
        status_transaction saved_status[];
        int unsigned saved_count;
        memblock_rob_key_t load_key,store_key;
        memblock_rob_map_key_t load_map,store_map;
        mmu_csr_runtime_state csr;
        memblock_uid_tlb_record first;
        memblock_later_split_tlb_record owner,historical;
        memblock_tlb_entry response_v;
        memblock_tlb_lookup_key_t request_key;
        saved_main=data.main_table_by_uid;saved_status=data.status_by_uid;saved_count=data.main_trans_num;
        data.main_trans_num=3;data.main_table_by_uid=new[3];data.status_by_uid=new[3];
        csr=new("shared_split_csr");csr.satp_mode=8;csr.priv_dmode=1;
        load_key='{flag:0,value:41};store_key='{flag:0,value:42};
        load_map=rob_order_util::rob_to_map_key(load_key);store_map=rob_order_util::rob_to_map_key(store_key);
        data.uid_by_active_rob[load_map]=0;data.uid_by_active_rob[store_map]=1;
        for(int i=0;i<3;i++) begin
            data.main_table_by_uid[i]=new($sformatf("shared_split_main_%0d",i));
            data.status_by_uid[i]=new($sformatf("shared_split_status_%0d",i));data.status_by_uid[i].reset(i);
            data.status_by_uid[i].active=1;data.status_by_uid[i].dynamic_epoch=7;
            data.status_by_uid[i].robIdx_value=MEMBLOCK_ROB_VALUE_W'(41+i);
            data.main_table_by_uid[i].robIdx_flag=0;
            data.main_table_by_uid[i].robIdx_value=MEMBLOCK_ROB_VALUE_W'(41+i);
            data.main_table_by_uid[i].fuType=i==1?MEMBLOCK_FUTYPE_STU:MEMBLOCK_FUTYPE_LDU;
            data.main_table_by_uid[i].fuOpType=i==1?MEMBLOCK_LSUOP_HSVH:MEMBLOCK_LSUOP_HLVH;
            data.main_table_by_uid[i].vaddr=64'h80001fff;
            first=new($sformatf("shared_split_first_%0d",i));first.init_context(i,52'h80001,0,0,csr,10);
            data.uid_tlb_record_by_uid[i]=first;
        end
        // 先建立已完成旧记录；它不在两个active cursor内，不能被新PTW反查选中。
        historical=new("shared_split_history");historical.uid=2;historical.dynamic_epoch=7;
        historical.split_vpn=52'h80002;historical.s2xlate=0;
        historical.state=memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_COMPLETED;
        data.later_split_tlb_record_by_uid[2]=historical;
        request_key=csr.make_lookup_key(52'h80002,0);
        for(int cancel_case=0;cancel_case<2;cancel_case++) begin
            data.later_split_tlb_record_by_uid.delete(0);data.later_split_tlb_record_by_uid.delete(1);
            data.active_load_later_split_valid=0;data.active_store_later_split_valid=0;
            data.observe_later_split_tlb_sample(1,load_key,64'h80002000,1,1,0,load_key,64'h80002000,1,38'h80002,0,20);
            data.observe_later_split_store_tlb_sample(1,store_key,64'h80002000,1,1,store_key,64'h80002000,1,38'h80002,0,20);
            owner=data.bind_later_split_l2_request_token(request_key,100+cancel_case,21);
            expect_true(owner!=null && data.later_split_owners_by_token[100+cancel_case].size()==2 &&
                data.later_split_tlb_record_by_uid[0].response_token==100+cancel_case &&
                data.later_split_tlb_record_by_uid[1].response_token==100+cancel_case &&
                historical.state==memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_COMPLETED,"Load/Store shared token binding or history isolation failed");
            if(cancel_case) begin
                data.cancel_later_split_l2_token(101,"shared token selftest");
                expect_true(data.later_split_tlb_record_by_uid[0].state==memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_CANCELED &&
                    data.later_split_tlb_record_by_uid[1].state==memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_CANCELED &&
                    !data.later_split_owners_by_token.exists(101),"shared token cancel did not release both owners");passed("split_shared_token_cancel");
            end else begin
                response_v=new("shared_split_response");response_v.lookup_key=request_key;
                response_v.entry_generation=123;response_v.s1_stage_active=1;response_v.s1_translation_mode_at_build=8;
                response_v.s1_addr_low=2;response_v.s1_pteidx[2]=1;response_v.s1_valididx[2]=1;
                data.complete_later_split_l2_response(owner,100,request_key,request_key,123,response_v,30);
                expect_true(data.later_split_tlb_record_by_uid[0].state==memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_COMPLETED &&
                    data.later_split_tlb_record_by_uid[1].state==memblock_later_split_tlb_record::MEMBLOCK_LATER_SPLIT_COMPLETED &&
                    data.later_split_tlb_record_by_uid[0].response_sample_seq==30 &&
                    data.later_split_tlb_record_by_uid[1].response_sample_seq==30,"single completion did not fan out to both owners");passed("split_shared_token_complete");
            end
        end
        for(int i=0;i<3;i++)begin data.uid_tlb_record_by_uid.delete(i);data.later_split_tlb_record_by_uid.delete(i);end
        data.uid_by_active_rob.delete(load_map);data.uid_by_active_rob.delete(store_map);
        data.active_load_later_split_valid=0;data.active_store_later_split_valid=0;
        data.main_table_by_uid=saved_main;data.status_by_uid=saved_status;data.main_trans_num=saved_count;
    endfunction

    // 抽象职责：构造被动WB历史值，验证ROB复用不能跨UID/代次/issue/采样界限取得旧值。
    function void check_wb_identity();
        memblock_rm_dut_writeback_observer observer;
        memblock_rm_dut_writeback_record_t record_v,result_v;
        memblock_rob_key_t key;
        key='{flag:0,value:23};observer=new("rm_selftest_wb");record_v='0;
        record_v.valid=1;record_v.owner_valid=1;record_v.owner_uid=0;record_v.owner_dynamic_epoch=7;
        record_v.owner_issue_epoch=3;record_v.replay_inst_valid=1;record_v.flush_pipe_valid=1;
        record_v.sample_seq=100;record_v.sample_flush_epoch=2;
        observer.load_history_by_rob[{key.flag,key.value}].push_back(record_v);
        expect_true(observer.read_load_instance(0,key,7,3,1,2,100,result_v),"matching WB rejected");
        expect_true(!observer.read_load_instance(1,key,7,3,1,2,100,result_v) &&
            !observer.read_load_instance(0,key,8,3,1,2,100,result_v) &&
            !observer.read_load_instance(0,key,7,4,1,2,100,result_v) &&
            !observer.read_load_instance(0,key,7,3,1,3,100,result_v) &&
            !observer.read_load_instance(0,key,7,3,1,2,101,result_v),"old WB identity accepted");passed("wb_instance_filter");
    endfunction

    // 抽象职责：让正式地址责任服务先等待再消费S+2，负例只捕获预期报告，不重新提交数据。
    function void check_address_obligation();
        memblock_backend_cycle_frame_t frame;
        rm_ls_program_item_t item;
        memblock_virtualization_rm_expected_report catcher;
        int unsigned before_commits;
        frame=new();frame.uid=0;frame.rob_key=data.status_by_uid[0].get_rob_key();frame.dynamic_epoch=7;
        frame.issue_epoch=3;frame.replay_seq=2;frame.transport.reset_epoch=4;frame.transport.frame_id=9;
        frame.input_sample=100;frame.target_sample=102;frame.address_required=1;frame.address_status=memblock_backend_cycle_frame_t::WAITING;
        data.backend_fault_address_by_uid[0]=frame;
        item=new(0);item.dynamic_epoch=7;item.rob.flag=frame.rob_key.flag;item.rob.value=frame.rob_key.value;
        item.address_inputs_valid=1;item.reset_epoch=4;item.issue_epoch=3;item.replay_seq=2;
        item.expected_exception[21]=1;item.expected_address_valid=1;item.expected_vaddr=0;
        item.expected_gpaddr_valid=1;item.expected_gpaddr=0;item.expected_vs_nonleaf=1;
        before_commits=test_env.rm.ls_model.consumed_commit_count;
        test_env.rm.observer_pending_address.push_back(item);test_env.rm.observer_service_exception_addresses();
        expect_true(test_env.rm.observer_pending_address.size()==1,"pending address consumed early");
        frame.address_captured=1;frame.address_status=memblock_backend_cycle_frame_t::CAPTURED;
        frame.address.sample_seq=102;frame.address.vaddr=0;frame.address.gpaddr=0;frame.address.is_for_vs_non_leaf=1;
        test_env.rm.observer_service_exception_addresses();
        expect_true(test_env.rm.observer_pending_address.size()==0 && test_env.rm.ls_model.consumed_commit_count==before_commits,"S+2 zero address lost or committed twice");passed("address_late_s_plus_2_zero");
        frame.address.sample_seq=101;test_env.rm.observer_pending_address.push_back(item);
        catcher=new("RM_EXCEPTION_ADDRESS_MISMATCH");uvm_report_cb::add(null,catcher);
        test_env.rm.observer_service_exception_addresses();uvm_report_cb::delete(null,catcher);
        expect_true(catcher.caught==1 && test_env.rm.observer_pending_address.size()==0,"wrong address sample accepted");passed("address_sample_reject");
        frame.address.sample_seq=102;frame.address.gpaddr=1;test_env.rm.observer_pending_address.push_back(item);
        catcher=new("RM_EXCEPTION_ADDRESS_MISMATCH");uvm_report_cb::add(null,catcher);
        test_env.rm.observer_service_exception_addresses();uvm_report_cb::delete(null,catcher);
        expect_true(catcher.caught==1,"wrong GPA accepted");passed("address_gpa_reject");
        data.backend_fault_address_by_uid.delete(0);
    endfunction

    virtual task body();
        main_control_transaction saved_main[];
        status_transaction saved_status[];
        int unsigned saved_count;
        rm_ls_model_t saved_model;
        memblock_pma_pmp_model pmp;
        require_virtual_sqr();
        if(!$cast(test_env,p_sequencer.get_parent()) || test_env.rm==null)
            `uvm_fatal("VIRTUALIZATION_RM_SELFTEST","virtual sequencer parent has no RM")
        data=common_data_transaction::get();ro=memblock_rm_readonly_api::get();
        saved_main=data.main_table_by_uid;saved_status=data.status_by_uid;saved_count=data.main_trans_num;
        saved_model=test_env.rm.ls_model;test_env.rm.ls_model=new();
        data.main_trans_num=1;data.main_table_by_uid=new[1];data.status_by_uid=new[1];
        pmp=memblock_pma_pmp_model::get();pmp.reset_and_init_v2_profile();
        pmp.apply_csr_write(12'h3b0,64'h00003fffffffffff,1);pmp.apply_csr_write(12'h3a0,64'h0f,2);
        data.pma_pmp_model=pmp;
        expect_true(pmp.capture_uid_context(0,7,1,0,0,0,0,0,10),"PMP context setup failed");
        check_pmm_selection();check_address_math();check_permissions();check_production_opcodes();
        check_hlvx_pmp();check_guest_gpa();check_memory_effects();check_snapshot_history();check_pmm_reference();check_tagged_first_split();check_masked_reference_history();check_masked_crosspage_reference();check_masked_boundary_carry_capture();check_split_shared_token();check_wb_identity();check_address_obligation();
        data.uid_tlb_record_by_uid.delete(0);data.later_split_tlb_record_by_uid.delete(0);data.rm_access_snapshot_by_uid.delete(0);
        data.main_table_by_uid=saved_main;data.status_by_uid=saved_status;data.main_trans_num=saved_count;
        test_env.rm.ls_model=saved_model;
        `uvm_info("VIRTUALIZATION_RM_SELFTEST",$sformatf("TEST CASE PASSED checks=%0d",checks),UVM_LOW)
    endtask
endclass
`endif
