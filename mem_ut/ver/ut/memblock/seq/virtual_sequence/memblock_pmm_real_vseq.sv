`ifndef MEMBLOCK_PMM_REAL_VSEQ__SV
`define MEMBLOCK_PMM_REAL_VSEQ__SV

// 测试专用CSR候选：只覆盖本场景需要的level，驱动、PMP初始化和四拍同步复用父类。
class memblock_pmm10_initial_sequence extends memblock_csr_initial_config_sequence;
    `uvm_object_utils(memblock_pmm10_initial_sequence)
    function new(string name="memblock_pmm10_initial_sequence");super.new(name);endfunction
    virtual function bit [1:0] selected_pmm();return 2'b10;endfunction
    virtual function bit uses_stage2_reference();return 0;endfunction
    virtual function void customize_target(input csr_ctrl_agent_agent_xaction target);
        target.io_ooo_to_mem_tlbCsr_satp_mode=0;
        target.io_ooo_to_mem_tlbCsr_vsatp_mode=0;
        target.io_ooo_to_mem_tlbCsr_hgatp_mode=uses_stage2_reference()?4'd9:4'd0;
        target.io_ooo_to_mem_tlbCsr_satp_asid=0;
        target.io_ooo_to_mem_tlbCsr_vsatp_asid=0;
        target.io_ooo_to_mem_tlbCsr_hgatp_vmid=uses_stage2_reference()?16'd3:16'd0;
        target.io_ooo_to_mem_tlbCsr_priv_virt=0;
        target.io_ooo_to_mem_tlbCsr_priv_imode=1;
        target.io_ooo_to_mem_tlbCsr_priv_dmode=1;
        target.io_ooo_to_mem_tlbCsr_priv_spvp=1;
        target.io_ooo_to_mem_tlbCsr_priv_mxr=0;
        target.io_ooo_to_mem_tlbCsr_priv_vmxr=0;
        target.io_ooo_to_mem_tlbCsr_priv_sum=1;
        target.io_ooo_to_mem_tlbCsr_priv_vsum=1;
        target.io_ooo_to_mem_tlbCsr_pmm_mseccfg=selected_pmm();
        target.io_ooo_to_mem_tlbCsr_pmm_menvcfg=selected_pmm();
        target.io_ooo_to_mem_tlbCsr_pmm_henvcfg=selected_pmm();
        target.io_ooo_to_mem_tlbCsr_pmm_hstatus=selected_pmm();
        target.io_ooo_to_mem_tlbCsr_pmm_senvcfg=selected_pmm();
        target.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable=1;
        target.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable=1;
    endfunction
endclass

class memblock_pmm11_initial_sequence extends memblock_pmm10_initial_sequence;
    `uvm_object_utils(memblock_pmm11_initial_sequence)
    function new(string name="memblock_pmm11_initial_sequence");super.new(name);endfunction
    virtual function bit [1:0] selected_pmm();return 2'b11;endfunction
endclass

class memblock_pmm00_initial_sequence extends memblock_pmm10_initial_sequence;
    `uvm_object_utils(memblock_pmm00_initial_sequence)
    function new(string name="memblock_pmm00_initial_sequence");super.new(name);endfunction
    virtual function bit [1:0] selected_pmm();return 2'b00;endfunction
endclass

class memblock_pmm_s2_initial_sequence extends memblock_pmm11_initial_sequence;
    `uvm_object_utils(memblock_pmm_s2_initial_sequence)
    function new(string name="memblock_pmm_s2_initial_sequence");super.new(name);endfunction
    virtual function bit uses_stage2_reference();return 1;endfunction

    // 抽象职责：流量开放前发布测试预定义映射；原VPN及maskedVPN均为独立输入，
    // 不读取DUT结果，不制造response token，不直接写UID或RM snapshot。
    function void publish_reference(input bit [51:0] vpn,input mmu_csr_runtime_state csr);
        common_data_transaction data;
        memblock_tlb_entry entry;
        memblock_tlb_lookup_key_t key;
        data=common_data_transaction::get();key=csr.make_lookup_key(vpn,2);
        entry=new($sformatf("pmm_s2_input_%h",vpn));
        entry.lookup_key=key;entry.s2xlate=2;entry.s2_stage_active=1;
        entry.s2_translation_mode_at_build=9;entry.s2_root_ppn_at_build=csr.hgatp_ppn;
        entry.csr_context_seq_at_build=csr.update_seq;
        entry.s2_vmid=csr.hgatp_vmid;entry.s2_tag=vpn[37:0];
        entry.s2_entry_ppn_raw=38'h80001;entry.s2_level=0;
        entry.s2_pte_r=1;entry.s2_pte_w=1;entry.s2_pte_x=1;
        entry.s2_pte_u=1;entry.s2_pte_a=1;entry.s2_pte_d=1;
        entry.entry_generation=data.allocate_tlb_entry_generation();
        entry.payload_phase=memblock_tlb_entry::MEMBLOCK_TLB_PAYLOAD_FINALIZED;
        data.publish_live_tlb_entry(key,entry,csr);
        `uvm_info("PMM_REAL_INPUT",$sformatf("preset VPN=%h VMID=%0d PPN=80001 generation=%0d",vpn,csr.hgatp_vmid,entry.entry_generation),UVM_LOW)
    endfunction

    virtual task body();
        common_data_transaction data;
        mmu_csr_runtime_state csr;
        super.body();
        // 父vseq的全部producer在#1轮询initial_done；此处零时间完成两个publish，
        // 因而任何真实issue之前已具备输入。主表reset和四拍CSR稳定均已结束。
        data=common_data_transaction::get();
        csr=new("pmm_s2_input_csr");csr.copy_from(data.mmu_csr_state);
        publish_reference(52'h1000080001,csr);
        publish_reference(52'h80001,csr);
    endtask
endclass

// 测试专用主表只建四条真实访存；共享HPA的数据值通过冻结STD指定。
class memblock_pmm_main_sequence extends memblock_main_dispatch_auto_build_main_table_base_sequence;
    `uvm_object_utils(memblock_pmm_main_sequence)
    function new(string name="memblock_pmm_main_sequence");super.new(name);endfunction
    virtual function bit uses_stage2_reference();return 0;endfunction
    virtual task build_main_table();
        main_control_transaction tr;
        ensure_dispatch_runtime_helpers();
        clear_manual_main_table();
        for(int unsigned i=0;i<4;i++) begin
            tr=new($sformatf("pmm_input_%0d",i));
            tr.robIdx_flag=0;tr.robIdx_value=MEMBLOCK_ROB_VALUE_W'(i);
            tr.op_class=i==1?MEMBLOCK_OP_CLASS_STORE:MEMBLOCK_OP_CLASS_INT_LOAD;
            tr.fuType=i==1?MEMBLOCK_FUTYPE_STU:MEMBLOCK_FUTYPE_LDU;
            tr.lsq_flow=i==1?MEMBLOCK_LSQ_FLOW_STORE:MEMBLOCK_LSQ_FLOW_LOAD;
            tr.fuOpType=i==1?MEMBLOCK_LSUOP_HSVD:i==3?MEMBLOCK_LSUOP_HLVD:MEMBLOCK_LSUOP_LD;
            tr.numLsElem=memblock_num_ls_elem_t'(1);
            // 高7位不同但低57位一致，PMM10/11都应命中同一物理地址。
            tr.src_0=(64'h8000000000000000+(64'(i)<<57))|64'h80001000;
            if(uses_stage2_reference())
                tr.src_0=(i==1 || i==3)?64'h0001000080001000:64'h80001000;
            tr.imm=0;tr.send_pri=50;tr.send_pri_std=50;
            tr.boundary_profile=MEMBLOCK_BOUNDARY_PROFILE_ALIGNED;tr.boundary_size_bytes=8;
            if(i==1) begin
                tr.std_data_override_valid[0]=1;
                tr.std_data_override[0]=64'haabbccddeeff0011;
            end
            tr.update_vaddr();
            set_manual_main_transaction(i,tr);
        end
        import_manual_main_table();
        if(memblock_sync_pkg::uses_control_barrier_topology())
            control_barrier_service.initialize_control_runtime_bootstrap();
    endtask
endclass

class memblock_pmm_s2_main_sequence extends memblock_pmm_main_sequence;
    `uvm_object_utils(memblock_pmm_s2_main_sequence)
    function new(string name="memblock_pmm_s2_main_sequence");super.new(name);endfunction
    virtual function bit uses_stage2_reference();return 1;endfunction
endclass

// 仅在当前vseq的两个准确factory上下文替换类型，完整CSR dedicated生命周期复用父类。
class memblock_pmm10_real_vseq extends memblock_csr_random_config_vseq;
    `uvm_object_utils(memblock_pmm10_real_vseq)
    function new(string name="memblock_pmm10_real_vseq");super.new(name);endfunction
    virtual function bit expects_preaf();return 0;endfunction
    virtual function bit expects_stage2_alias();return 0;endfunction
    virtual function uvm_object_wrapper initial_type();return memblock_pmm10_initial_sequence::get_type();endfunction
    // 抽象职责：父生命周期结束后检查场景结果，避免入口未覆盖目标分支也被记为通过。
    // 数据和异常值仍由生产RM核对；此处只验证四条输入均按本场景正常或预AF收口。
    virtual task body();
        common_data_transaction data;
        status_transaction status_v;
        super.body();data=common_data_transaction::get();
        if(data.main_trans_num!=4) `uvm_fatal("PMM_REAL_TEST","test main factory override did not create four inputs")
        for(int unsigned i=0;i<4;i++) begin
            bit [23:0] expected_exception;
            status_v=data.get_status(i);expected_exception=0;
            // 核对输入形状，随机默认main即使恰好四条也不能冒充此专项。
            if(data.main_table_by_uid[i]==null || data.main_table_by_uid[i].imm!=0 ||
               data.main_table_by_uid[i].fuOpType!=(i==1?MEMBLOCK_LSUOP_HSVD:i==3?MEMBLOCK_LSUOP_HLVD:MEMBLOCK_LSUOP_LD) ||
               data.main_table_by_uid[i].src_0!=(expects_stage2_alias()?
                   ((i==1 || i==3)?64'h0001000080001000:64'h80001000):
                   ((64'h8000000000000000+(64'(i)<<57))|64'h80001000)))
                `uvm_fatal("PMM_REAL_TEST","test main factory override missed the directed input shape")
            if(expects_preaf()) expected_exception[i==1?7:5]=1;
            if(status_v==null || !status_v.terminal_done || status_v.exception_vec!=expected_exception)
                `uvm_error("PMM_REAL_TEST",$sformatf("uid=%0d terminal/cause mismatch expected=%h",i,expected_exception))
        end
        `uvm_info("PMM_REAL_TEST","four real-DUT memory inputs completed scenario cause checks",UVM_LOW)
    endtask
    virtual function uvm_object_wrapper main_type();return memblock_pmm_main_sequence::get_type();endfunction
    virtual task pre_body();
        uvm_factory factory;
        super.pre_body();factory=uvm_factory::get();
        factory.set_inst_override_by_type(memblock_csr_initial_config_sequence::get_type(),
            initial_type(),{get_full_name(),".initial_csr_seq"});
        factory.set_inst_override_by_type(memblock_main_dispatch_auto_build_main_table_base_sequence::get_type(),
            main_type(),{get_full_name(),".main_seq"});
    endtask
endclass

class memblock_pmm11_real_vseq extends memblock_pmm10_real_vseq;
    `uvm_object_utils(memblock_pmm11_real_vseq)
    function new(string name="memblock_pmm11_real_vseq");super.new(name);endfunction
    virtual function uvm_object_wrapper initial_type();return memblock_pmm11_initial_sequence::get_type();endfunction
endclass
class memblock_pmm00_preaf_real_vseq extends memblock_pmm10_real_vseq;
    `uvm_object_utils(memblock_pmm00_preaf_real_vseq)
    function new(string name="memblock_pmm00_preaf_real_vseq");super.new(name);endfunction
    virtual function bit expects_preaf();return 1;endfunction
    virtual function uvm_object_wrapper initial_type();return memblock_pmm00_initial_sequence::get_type();endfunction
endclass
class memblock_pmm_s2_alias_real_vseq extends memblock_pmm10_real_vseq;
    `uvm_object_utils(memblock_pmm_s2_alias_real_vseq)
    function new(string name="memblock_pmm_s2_alias_real_vseq");super.new(name);endfunction
    virtual function bit expects_stage2_alias();return 1;endfunction
    virtual function uvm_object_wrapper initial_type();return memblock_pmm_s2_initial_sequence::get_type();endfunction
    virtual function uvm_object_wrapper main_type();return memblock_pmm_s2_main_sequence::get_type();endfunction
endclass
`endif
