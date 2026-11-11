`ifndef SOFT_TEST_TC_RM_CMO_TRANSLATION_SV
`define SOFT_TEST_TC_RM_CMO_TRANSLATION_SV

// 中文注释：复用已验证的UID冻结TLB记录构造，只调用真实RM生产入口；
// 不复制权限判定，不伪造最终STA0，不需要驱动DUT。
class soft_test_tc_rm_cmo_translation extends soft_test_tc_rm_pbmt_effective_fault;
    `uvm_component_utils(soft_test_tc_rm_cmo_translation)
    int unsigned checks;
    function new(string name="soft_test_tc_rm_cmo_translation",uvm_component parent=null);
        super.new(name,parent); checks=0;
    endfunction
    function void expect_cmo(input bit condition,input string detail);
        checks++;
        if(!condition) `uvm_fatal("RM_CMO_TRANSLATION_SOFT",detail)
    endfunction
    // 抽象职责：建立最终CMO的独立主表/状态视图，保留src+imm地址计算几何。
    function void init_cmo_views(input memblock_uid_t uid,input bit [8:0] op,
        input bit [63:0] address,
        output memblock_rm_readonly_api::main_transaction_view_t main_v,
        output memblock_rm_readonly_api::status_view_t status_v);
        init_store_views(uid,address,main_v,status_v);
        main_v.op_class=MEMBLOCK_OP_CLASS_CBO; main_v.fu_op_type=op;
        main_v.src_0=address-5; main_v.imm=5; main_v.vaddr=address;
        main_v.atomic_std_valid_mask=0; main_v.atomic_std_data_flat='1;
    endfunction
    function bit [8:0] cmo_op(input int unsigned index);
        case(index)
            0:return MEMBLOCK_LSUOP_CBO_ZERO;
            1:return MEMBLOCK_LSUOP_CBO_CLEAN;
            2:return MEMBLOCK_LSUOP_CBO_FLUSH;
            default:return MEMBLOCK_LSUOP_CBO_INVAL;
        endcase
    endfunction
    // 抽象职责：向PMA/PMP模型冻结与本UID/epoch一致的真实权限上下文，供生产入口查询。
    function void capture_pma_context(input memblock_uid_tlb_record record,
        input memblock_rm_readonly_api::status_view_t status_v,input bit [1:0] privilege=3);
        common_data_transaction data;
        data=common_data_transaction::get();
        expect_cmo(data.pma_pmp_model.capture_uid_context(record.uid,status_v.dynamic_epoch,
            privilege,0,0,0,0,record.csr_snapshot.update_seq,1000+record.uid),
            "CMO PMA/PMP frozen context unavailable");
    endfunction
    // 抽象职责：验证S1/S2权限命令与Store类异常映射，合法只读页允许nonzero而拒绝zero。
    function void check_page_permissions();
        memblock_rm_readonly_api::main_transaction_view_t main_v;
        memblock_rm_readonly_api::status_view_t status_v;
        memblock_uid_tlb_record record;
        rm_ls_program_item_t item;
        bit [1:0] stage;
        bit result_v;
        for(int s=0;s<2;s++) begin
            stage=s==0 ? 2'd0 : 2'd2;
            for(int op_i=0;op_i<4;op_i++) begin
                memblock_uid_t uid;
                uid=100+s*4+op_i;
                init_cmo_views(uid,cmo_op(op_i),64'h8123_403f+64'(uid)*8192,main_v,status_v);
                record=make_uid_tlb_record(uid,main_v.vaddr,stage,0,1,1);
                if(s==0) record.payload.s1_pte_w=0;
                else record.payload.s2_pte_w=0;
                result_v=env.rm.observer_build_commit_item(main_v,status_v,item);
                expect_cmo(result_v && item.kind==RM_LS_KIND_CMO &&
                    item.cmo_permission_write==(op_i==0),"CMO page permission command classification");
                expect_cmo(item.expected_exception==(op_i==0 ? (s==0 ? 24'h008000 : 24'h800000) : 24'h0),
                    $sformatf("CMO read-only page stage=%0d op=%0d got=0x%0h",s,op_i,item.expected_exception));
                expect_cmo(item.cmo_pre_access_exception==item.expected_exception && item.size_bytes==0 &&
                    item.store_data==0 && item.store_byte_mask==0,"CMO canonical data/size and pre-access fault");
                if(op_i!=0) expect_cmo(item.cmo_path==RM_CMO_PATH_DCACHE_CMO &&
                    item.computed_vaddr==main_v.vaddr && item.pa_by_byte[0][11:0]==main_v.vaddr[11:0] &&
                    item.pa_valid_mask==1,"normal nonzero offset geometry uses src+imm and one translated byte");
            end
        end
        init_cmo_views(108,MEMBLOCK_LSUOP_CBO_CLEAN,64'h8125_5001,main_v,status_v);
        record=make_uid_tlb_record(108,main_v.vaddr,0,0,0,1);
        expect_cmo(env.rm.observer_build_commit_item(main_v,status_v,item) &&
            item.expected_exception==24'h008000 && item.cmo_pre_access_exception==24'h008000,
            "nonzero denied read permission must expose Store Page Fault");
    endfunction
    // 抽象职责：直接从冻结TLB有效fault字段验证CMO的PF/AF/GPF/GAF均走Store外显体系。
    function void check_translation_faults();
        memblock_rm_readonly_api::main_transaction_view_t main_v;
        memblock_rm_readonly_api::status_view_t status_v;
        memblock_uid_tlb_record record;
        rm_ls_program_item_t item;
        bit [23:0] expected_v;
        for(int op_i=0;op_i<4;op_i++) begin
            for(int fault_i=0;fault_i<4;fault_i++) begin
                memblock_uid_t uid;
                uid=112+op_i*4+fault_i;
                init_cmo_views(uid,cmo_op(op_i),64'h8140_003f+64'(uid)*8192,main_v,status_v);
                record=make_uid_tlb_record(uid,main_v.vaddr,fault_i<2 ? 2'd0 : 2'd2,2,1,1);
                case(fault_i)
                    0:begin record.payload.fault_effective_s1_pf=1;
                        record.payload.fault_stage_selected=memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_S1;
                        expected_v=24'h008000; end
                    1:begin record.payload.fault_effective_s1_af=1;
                        record.payload.fault_stage_selected=memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_S1;
                        expected_v=24'h000080; end
                    2:begin record.payload.fault_effective_s2_gpf=1;
                        record.payload.fault_stage_selected=memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_S2;
                        expected_v=24'h800000; end
                    default:begin record.payload.fault_effective_s2_gaf=1;
                        record.payload.fault_stage_selected=memblock_tlb_entry::MEMBLOCK_TLB_FAULT_STAGE_S2;
                        expected_v=24'h000080; end
                endcase
                expect_cmo(env.rm.observer_build_commit_item(main_v,status_v,item) &&
                    item.expected_exception==expected_v && item.cmo_pre_access_exception==expected_v &&
                    item.size_bytes==0,"CMO TLB fault priority and Store-class exception mapping");
            end
        end
    endfunction
    // 抽象职责：确认PBMT只影响zero数据路径，nonzero仍是DCache CMO；低位offset不引入标量错位异常。
    function void check_pbmt_and_offset();
        memblock_rm_readonly_api::main_transaction_view_t main_v;
        memblock_rm_readonly_api::status_view_t status_v;
        memblock_uid_tlb_record record;
        rm_ls_program_item_t item;
        rm_cmo_path_e expected_path;
        for(int pbmt=0;pbmt<3;pbmt++) begin
            for(int op_i=0;op_i<4;op_i++) begin
                memblock_uid_t uid;
                uid=132+pbmt*4+op_i;
                init_cmo_views(uid,cmo_op(op_i),64'h8180_003f+64'(uid)*8192,main_v,status_v);
                record=make_uid_tlb_record(uid,main_v.vaddr,0,2'(pbmt),1,1);
                expected_path=op_i!=0 ? RM_CMO_PATH_DCACHE_CMO :
                    pbmt==0 ? RM_CMO_PATH_CACHEABLE_ZERO : RM_CMO_PATH_UNCACHE_ZERO;
                expect_cmo(env.rm.observer_build_commit_item(main_v,status_v,item) &&
                    item.expected_exception==0 && item.cmo_path==expected_path && item.size_bytes==0 &&
                    item.pa_valid_mask==1 && item.computed_vaddr==main_v.vaddr &&
                    item.pa_by_byte[0][5:0]==6'h3f && item.store_data==0,
                    $sformatf("CMO PBMT=%0d op=%0d offset/path classification",pbmt,op_i));
            end
        end
        // 有符号imm必须仍按地址操作数处理，不允许误套原子src0-only约定。
        init_cmo_views(145,MEMBLOCK_LSUOP_CBO_ZERO,64'h81a0_003f,main_v,status_v);
        main_v.src_0=main_v.vaddr+1; main_v.imm=64'hfff;
        record=make_uid_tlb_record(145,main_v.vaddr,0,0,1,1);
        expect_cmo(env.rm.observer_build_commit_item(main_v,status_v,item) && item.expected_exception==0 &&
            item.computed_vaddr==main_v.vaddr && item.pa_by_byte[0][5:0]==6'h3f,
            "CMO signed immediate -1 address geometry");
    endfunction
    // 抽象职责：使用真实PMA/PMP生产模型验证读写命令与Store AF，避免仅依赖TLB权限自测。
    function void check_pma_pmp_permissions();
        common_data_transaction data;
        memblock_rm_readonly_api::main_transaction_view_t main_v;
        memblock_rm_readonly_api::status_view_t status_v;
        memblock_uid_tlb_record record;
        rm_ls_program_item_t item;
        pma_pmp_eval_t eval_v;
        bit [47:0] address;
        data=common_data_transaction::get();
        seq_csr_common::pma_pmp_model_en=1;
        // 一个锁定的全空间NAPOT只读PMP项使M态也执行只读检查；合法PMA普通缓存区。
        data.pma_pmp_model.reset_and_init_v2_profile();
        data.pma_pmp_model.apply_csr_write(12'h3b0,64'h0000_1fff_ffff_ffff,1);
        data.pma_pmp_model.apply_csr_write(12'h3a0,64'h99,2);
        address=48'h0000_8000_103f;
        for(int op_i=0;op_i<4;op_i++) begin
            memblock_uid_t uid;
            uid=148+op_i;
            init_cmo_views(uid,cmo_op(op_i),64'h81c0_003f+64'(uid)*8192,main_v,status_v);
            record=make_uid_tlb_record(uid,main_v.vaddr,0,0,1,1);
            record.request_s1_resolved_ppn=44'(address[47:12]);
            capture_pma_context(record,status_v);
            expect_cmo(data.pma_pmp_model.evaluate_for_uid_epoch(uid,status_v.dynamic_epoch,1,
                address,1,op_i==0 ? PMA_PMP_CMD_WRITE : PMA_PMP_CMD_READ,eval_v) &&
                eval_v.valid && eval_v.pmp_st_fault==(op_i==0) && !eval_v.pma_st_fault &&
                !eval_v.pma_ld_fault,"read-only PMP precondition");
            expect_cmo(env.rm.observer_build_commit_item(main_v,status_v,item) &&
                item.expected_exception==(op_i==0 ? 24'h80 : 24'h0) &&
                item.expected_pmp_fault==(op_i==0) && item.cmo_permission_write==(op_i==0),
                "nonzero PMP uses read permission but denied zero exposes Store AF");
        end
        // V2默认PMA [0x39002000,0x3a000000)无读写权限；M态无PMP项默认通过。
        data.pma_pmp_model.reset_and_init_v2_profile(); address=48'h0000_3900_303f;
        for(int op_i=0;op_i<4;op_i++) begin
            memblock_uid_t uid;
            uid=152+op_i;
            init_cmo_views(uid,cmo_op(op_i),64'h81e0_003f+64'(uid)*8192,main_v,status_v);
            record=make_uid_tlb_record(uid,main_v.vaddr,0,0,1,1);
            record.request_s1_resolved_ppn=44'(address[47:12]); capture_pma_context(record,status_v);
            expect_cmo(env.rm.observer_build_commit_item(main_v,status_v,item) &&
                item.expected_exception==24'h80 && item.expected_pma_fault && !item.expected_pmp_fault &&
                item.cmo_pre_access_exception==24'h80,"PMA R/W denial maps every CMO to Store AF");
        end
        // PMA MMIO允许read/write仍使所有CMO在访问前报Store AF；PBMT IO本身不会报此异常。
        address=48'h0000_2001_003f;
        for(int op_i=0;op_i<4;op_i++) begin
            memblock_uid_t uid;
            uid=156+op_i;
            init_cmo_views(uid,cmo_op(op_i),64'h8200_003f+64'(uid)*8192,main_v,status_v);
            record=make_uid_tlb_record(uid,main_v.vaddr,0,0,1,1);
            record.request_s1_resolved_ppn=44'(address[47:12]); capture_pma_context(record,status_v);
            expect_cmo(env.rm.observer_build_commit_item(main_v,status_v,item) &&
                item.expected_exception==24'h80 && item.all_pma_mmio &&
                !item.expected_pmp_fault && !item.expected_pma_fault,
                "allowed PMA MMIO rejects CMO independently from read/write permission AF");
        end
        seq_csr_common::pma_pmp_model_en=0;
    endfunction
    // 抽象职责：拒绝opClass/fuType错配，数值重叠不应把其它功能单元误解释为CMO。
    function void check_classification_guards();
        memblock_rm_readonly_api::main_transaction_view_t main_v;
        memblock_rm_readonly_api::status_view_t status_v;
        rm_ls_program_item_t item;
        bit result_v;
        init_cmo_views(160,MEMBLOCK_LSUOP_CBO_CLEAN,64'h8240_0000,main_v,status_v);
        main_v.op_class=MEMBLOCK_OP_CLASS_STORE;
        result_v=env.rm.observer_build_commit_item(main_v,status_v,item);
        expect_cmo(!result_v || item.kind!=RM_LS_KIND_CMO,"ordinary Store opClass must not accept CMO final opcode");
        for(int fu_i=0;fu_i<2;fu_i++) begin
            init_cmo_views(161+fu_i,MEMBLOCK_LSUOP_CBO_ZERO,64'h8240_0000,main_v,status_v);
            main_v.fu_type=fu_i==0 ? MEMBLOCK_FUTYPE_LDU : MEMBLOCK_FUTYPE_MOU;
            result_v=env.rm.observer_build_commit_item(main_v,status_v,item);
            expect_cmo(!result_v || item.kind!=RM_LS_KIND_CMO,"non-STU fuType cannot be classified as CMO");
        end
    endfunction
    virtual task run_dispatch_smoke_sequence();
        common_data_transaction data;
        bit saved_pma_pmp_model_en;
        expect_cmo(env!=null && env.rm!=null,"production RM component absent");
        data=common_data_transaction::get(); saved_pma_pmp_model_en=seq_csr_common::pma_pmp_model_en;
        seq_csr_common::pma_pmp_model_en=0;
        for(int uid=100;uid<164;uid++) data.uid_tlb_record_by_uid.delete(uid);
        check_page_permissions(); check_translation_faults(); check_pbmt_and_offset();
        check_pma_pmp_permissions(); check_classification_guards();
        seq_csr_common::pma_pmp_model_en=saved_pma_pmp_model_en;
        for(int uid=100;uid<164;uid++) data.uid_tlb_record_by_uid.delete(uid);
        `uvm_info("RM_CMO_TRANSLATION_SOFT",$sformatf("TEST CASE PASSED: %0d production-path checks",checks),UVM_LOW)
    endtask
endclass
`endif
