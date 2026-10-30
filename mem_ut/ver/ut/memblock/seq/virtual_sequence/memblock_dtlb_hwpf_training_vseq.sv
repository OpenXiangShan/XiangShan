`ifndef MEMBLOCK_DTLB_HWPF_TRAINING_VSEQ__SV
`define MEMBLOCK_DTLB_HWPF_TRAINING_VSEQ__SV

// 抽象职责：只在公共建表流程中定向合法LD地址。真实接口、
// monitor、参考检查和终态审计仍由既有real-dispatch流程负责。
class memblock_dtlb_hwpf_training_main_sequence extends
    memblock_main_dispatch_auto_build_main_table_base_sequence;
    `uvm_object_utils(memblock_dtlb_hwpf_training_main_sequence)
    function new(string name = "memblock_dtlb_hwpf_training_main_sequence");
        super.new(name);
    endfunction
    // 建表期64B步长地址；无每拍全表扫描或运行期状态人工推进。
    virtual function bit [63:0] training_offset(input memblock_uid_t uid);
        bit [63:0] index64;
        index64 = uid;
        return index64 * 64'd64;
    endfunction
    virtual function void randomize_main_transaction(
        input main_control_transaction tr, input memblock_uid_t uid,
        input memblock_rob_key_t rob_key);
        bit [63:0] base_addr;
        bit [63:0] offset;
        bit [63:0] addr_range;
        super.randomize_main_transaction(tr, uid, rob_key);
        if (tr.op_class != MEMBLOCK_OP_CLASS_INT_LOAD) begin
            `uvm_fatal(get_type_name(), "training requires integer-load-only cfg")
        end
        if (seq_csr_common::get_boundary_profile_gen_en()) begin
            `uvm_fatal(get_type_name(), "directed training requires boundary profile disabled")
        end
        base_addr = seq_csr_common::get_main_vaddr_base();
        addr_range = seq_csr_common::get_main_vaddr_range();
        offset = training_offset(uid);
        if (base_addr[5:0] != 0 || addr_range < 8 ||
            offset > addr_range - 8 || base_addr + offset < base_addr) begin
            `uvm_fatal(get_type_name(), "training address must align and fit the VA window")
        end
        // 保持父类UID/ROB与合法scalar语义，只替换大小、地址和发送延迟。
        tr.fuOpType = MEMBLOCK_LSUOP_LD;
        tr.src_0 = base_addr + offset;
        tr.imm = 0;
        tr.delay = 0;
        tr.update_vaddr();
        validate_main_table_entry(tr, $sformatf("hwpf directed uid=%0d", uid));
    endfunction
endclass

// 抽象职责：每16笔进入新4KB区域，区域内重复四个间隔128B的偏移。
// 此模式探索SMS训练条件，不把访问模式当作已产生TLB请求的证明。
class memblock_dtlb_sms_training_main_sequence extends
    memblock_dtlb_hwpf_training_main_sequence;
    `uvm_object_utils(memblock_dtlb_sms_training_main_sequence)
    function new(string name = "memblock_dtlb_sms_training_main_sequence");
        super.new(name);
    endfunction
    virtual function bit [63:0] training_offset(input memblock_uid_t uid);
        bit [63:0] index64;
        index64 = uid;
        return (index64 / 64'd16) * 64'd4096 +
               (index64 % 64'd4) * 64'd128;
    endfunction
endclass

class memblock_dtlb_hwpf_training_vseq extends
    memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq;
    `uvm_object_utils(memblock_dtlb_hwpf_training_vseq)
    function new(string name = "memblock_dtlb_hwpf_training_vseq");
        super.new(name);
    endfunction
    // 在当前场景内选择主sequence，不安装全局factory override。
    virtual function memblock_dtlb_hwpf_training_main_sequence make_training_main();
        return memblock_dtlb_hwpf_training_main_sequence::type_id::create("training_main");
    endfunction
    // 抽象职责：复用唯一CSR/L2TLB owner与mirror barrier。
    // 背景responder、shared memory和end_test_check继续由父类body管理。
    virtual task start_core_dispatch_flow();
        memblock_lsqenq_dispatch_base_sequence enq_seq;
        memblock_issue_dispatch_base_sequence issue_seq;
        memblock_lsqcommit_dispatch_base_sequence commit_seq;
        memblock_l2tlb_base_sequence tlb_seq;
        memblock_mmu_sv39_pbmt0_non_nc_csr_sequence csr_seq;
        memblock_dtlb_hwpf_training_main_sequence main_seq;
        if (memblock_sync_pkg::get_control_worker_topology_mode() !=
            memblock_sync_pkg::MEMBLOCK_CONTROL_TOPOLOGY_DISABLED ||
            seq_csr_common::get_use_manual_main_table()) begin
            `uvm_fatal(get_type_name(), "training requires disabled control topology and automatic table")
        end
        main_seq = make_training_main();
        if (main_seq == null) begin
            `uvm_fatal(get_type_name(), "failed to create training main sequence")
        end
        `uvm_info(get_type_name(),
            "HWPF exploration: PC remains uid-based; PASS does not prove source-bin coverage", UVM_LOW)
        fork
            begin
                `uvm_do_on(enq_seq, p_sequencer.lsqenq_sqr)
            end
            begin
                wait_for_explicit_l2tlb_start_barrier();
                `uvm_do_on(issue_seq, p_sequencer.lintsissue_sqr)
            end
            begin
                `uvm_do_on(commit_seq, p_sequencer.lsqcommit_sqr)
            end
            begin
                wait_for_explicit_l2tlb_start_barrier();
                `uvm_do_on(tlb_seq, p_sequencer.L2tlb_sqr)
            end
            begin
                main_seq.start(p_sequencer);
            end
            begin
                `uvm_do_on(csr_seq, p_sequencer.csr_ctrl_sqr)
            end
        join
    endtask
endclass

class memblock_dtlb_sms_training_vseq extends memblock_dtlb_hwpf_training_vseq;
    `uvm_object_utils(memblock_dtlb_sms_training_vseq)
    function new(string name = "memblock_dtlb_sms_training_vseq");
        super.new(name);
    endfunction
    virtual function memblock_dtlb_hwpf_training_main_sequence make_training_main();
        return memblock_dtlb_sms_training_main_sequence::type_id::create("sms_training_main");
    endfunction
endclass
`endif
