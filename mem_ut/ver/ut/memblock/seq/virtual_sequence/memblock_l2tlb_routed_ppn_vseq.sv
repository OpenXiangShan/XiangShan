`ifndef MEMBLOCK_L2TLB_ROUTED_PPN_VSEQ__SV
`define MEMBLOCK_L2TLB_ROUTED_PPN_VSEQ__SV

class memblock_l2tlb_routed_ppn_vseq extends memblock_dispatch_real_smoke_vseq;
    `uvm_object_utils(memblock_l2tlb_routed_ppn_vseq)
    // 只记录真实 memory-facing A.fire 到达证据，不提供数据/异常期望或 RM 结果。
    longint unsigned actual_a_fire_count[6];
    virtual sbuffer_agent_agent_interface narrow_vif;

    function new(string name = "memblock_l2tlb_routed_ppn_vseq");
        super.new(name);
        foreach (actual_a_fire_count[i]) actual_a_fire_count[i] = 0;
    endfunction

    // 抽象职责：限制专项为单 stage 4K、稳定 CSR、无 reuse；全部 lifecycle 由父类执行。
    virtual task body();
        bit finished;
        seq_csr_common::init();
        if (seq_csr_common::get_main_mem_ranges_en() ||
            seq_csr_common::get_l2tlb_level_weight_en() ||
            seq_csr_common::get_l2tlb_ppn_reuse_en() ||
            seq_csr_common::get_addr_reuse_en_1_wt() != 0 ||
            seq_csr_common::get_boundary_profile_gen_en() ||
            seq_csr_common::get_use_manual_main_table() ||
            seq_csr_common::get_csr_control_enable() ||
            seq_csr_common::get_sfence_control_enable() ||
            !uses_static_mmu_sv39_csr() ||
            seq_csr_common::get_l2tlb_pte_1_wt(1, 7) != 0 ||
            seq_csr_common::get_l2tlb_fault_1_wt(1, 0) != 0 ||
            seq_csr_common::get_l2tlb_fault_1_wt(1, 1) != 0 ||
            seq_csr_common::get_l2tlb_pbmt_wt(1, 1) != 0 ||
            seq_csr_common::get_l2tlb_pbmt_wt(1, 2) != 0) begin
            `uvm_fatal(get_type_name(), "narrow routed PPN scenario requires sparse/static Sv39 S1 4K N0 PBMT00, no faults/control/address or PPN reuse")
        end
        if (!uvm_config_db#(virtual sbuffer_agent_agent_interface)::get(
                null, "uvm_test_top.env.u_sbuffer_agent_agent*", "vif", narrow_vif)) begin
            `uvm_fatal(get_type_name(), "cannot observe narrow-window external A.fire interface")
        end
        finished = 0;
        fork
            begin
                super.body();
                finished = 1;
            end
            begin
                observe_external_a_fires(finished);
            end
        join
        foreach (actual_a_fire_count[i]) begin
            bit [63:0] first, limit;
            string label;
            int signed region_id;
            memblock_main_dispatch_routed_ppn_sequence::narrow_region(i, first, limit, label, region_id);
            `uvm_info("ROUTED_PPN_NARROW_REACHED", $sformatf("window=%s actual_A_fire_count=%0d", label, actual_a_fire_count[i]), UVM_LOW)
        end
    endtask

    // 抽象职责：被动采样到达外部 responder 的请求；启动后 reset 终止专项，禁止
    // 公共 reset 清除 intent 后继续执行旧主表。计数用于结果证据，不充当 checker。
    virtual task observe_external_a_fires(ref bit finished);
        bit started;
        bit [63:0] pa;
        started = 0;
        while (!finished) begin
            @(narrow_vif.mon_cb);
            if (common_data_transaction::get().main_table_ready && narrow_vif.rst_n === 1'b1 &&
                memblock_sync_pkg::reset_backend_done === 1'b1) started = 1;
            if (started && (narrow_vif.rst_n !== 1'b1 ||
                            memblock_sync_pkg::reset_backend_done !== 1'b1)) begin
                `uvm_fatal(get_type_name(), "runtime reset aborts narrow PPN scenario; rerun with full initialization")
            end
            if (narrow_vif.rst_n !== 1'b1 ||
                narrow_vif.mon_cb.auto_inner_buffers_out_a_valid !== 1'b1 ||
                narrow_vif.mon_cb.auto_inner_buffers_out_a_ready !== 1'b1) continue;
            pa = {16'b0, narrow_vif.mon_cb.auto_inner_buffers_out_a_bits_address};
            for (int unsigned i = 0; i < 6; i++) begin
                bit [63:0] first, limit;
                string label;
                int signed region_id;
                memblock_main_dispatch_routed_ppn_sequence::narrow_region(i, first, limit, label, region_id);
                if (pa >= first && pa < limit) begin
                    actual_a_fire_count[i]++;
                    `uvm_info("ROUTED_PPN_NARROW_A_FIRE", $sformatf("window=%s pa=0x%0h", label, pa), UVM_LOW)
                end
            end
        end
    endtask

    // 抽象职责：保留父场景的 agent/CSR barrier 与 lifecycle，仅替换主表输入类型。
    // 显式调度避免全局 factory override 影响同一仿真中的其它场景。
    virtual task start_core_dispatch_flow();
        memblock_lsqenq_dispatch_base_sequence lsqenq_seq;
        memblock_issue_dispatch_base_sequence issue_seq;
        memblock_lsqcommit_dispatch_base_sequence lsqcommit_seq;
        memblock_l2tlb_base_sequence l2tlb_seq;
        memblock_main_dispatch_routed_ppn_sequence main_seq;
        memblock_mmu_sv39_csr_sequence mmu_sv39_csr_seq;
        fork
            begin
                `uvm_do_on(lsqenq_seq, p_sequencer.lsqenq_sqr)
            end
            begin
                wait_for_explicit_l2tlb_start_barrier();
                `uvm_do_on(issue_seq, p_sequencer.lintsissue_sqr)
            end
            begin
                `uvm_do_on(lsqcommit_seq, p_sequencer.lsqcommit_sqr)
            end
            begin
                wait_for_explicit_l2tlb_start_barrier();
                `uvm_do_on(l2tlb_seq, p_sequencer.L2tlb_sqr)
            end
            begin
                `uvm_do_on(main_seq, p_sequencer)
            end
            begin
                `uvm_do_on(mmu_sv39_csr_seq, p_sequencer.csr_ctrl_sqr)
            end
        join
    endtask
endclass
`endif
