//=========================================================
//File name    : memblock_dispatch_real_smoke_vseq.sv
//Author       : OpenAI_Codex
//Module name  : memblock_dispatch_real_smoke_vseq
//Discribution : real dispatch smoke virtual sequence
//Date         : 2026-07-03
//=========================================================
`ifndef MEMBLOCK_DISPATCH_REAL_SMOKE_VSEQ__SV
`define MEMBLOCK_DISPATCH_REAL_SMOKE_VSEQ__SV

class memblock_dispatch_real_smoke_vseq extends virtual_base_sequence;

    `uvm_object_utils(memblock_dispatch_real_smoke_vseq)

    extern function new(string name = "memblock_dispatch_real_smoke_vseq");
    extern virtual task pre_body();
    extern virtual task body();
    extern virtual function void require_real_smoke_sqr();
    extern virtual function void initialize_shared_memory_store();
    extern virtual task start_background_responders();
    extern virtual task start_core_dispatch_flow();
    extern virtual task wait_for_explicit_l2tlb_start_barrier();
    extern virtual function bit uses_explicit_control_workers();
    extern virtual function bit uses_static_mmu_sv39_csr();

endclass:memblock_dispatch_real_smoke_vseq

function memblock_dispatch_real_smoke_vseq::new(string name = "memblock_dispatch_real_smoke_vseq");
    super.new(name);
endfunction:new

task memblock_dispatch_real_smoke_vseq::pre_body();
    if (starting_phase != null) begin
        starting_phase.phase_done.set_drain_time(this, 1us);
        starting_phase.raise_objection(this);
    end
endtask:pre_body

task memblock_dispatch_real_smoke_vseq::body();
    require_real_smoke_sqr();
    seq_csr_common::init();

    memblock_sync_pkg::dispatch_real_smoke_active = 1'b1;
    `uvm_info(get_type_name(), "real dispatch smoke virtual sequence start", UVM_LOW)

    initialize_shared_memory_store();

    fork : background_responder_fork
        start_background_responders();
    join_none

    start_core_dispatch_flow();

    // The responders use global_stop_requested plus their own inflight state as
    // the natural exit condition.  Keep the scenario active until the forked
    // responder task has returned; clearing the activity bit first can make a
    // responder miss its final stop sample and wait forever.
    wait fork;

    memblock_sync_pkg::dispatch_real_smoke_active = 1'b0;
    `uvm_info(get_type_name(), "real dispatch smoke virtual sequence completed", UVM_LOW)
endtask:body

function void memblock_dispatch_real_smoke_vseq::require_real_smoke_sqr();
    require_virtual_sqr();
    require_agent_sqr("lsqenq", p_sequencer.lsqenq_sqr);
    require_agent_sqr("lintsissue", p_sequencer.lintsissue_sqr);
    require_agent_sqr("lsqcommit", p_sequencer.lsqcommit_sqr);
    require_agent_sqr("L2tlb", p_sequencer.L2tlb_sqr);
    require_agent_sqr("dcache", p_sequencer.dcache_sqr);
    require_agent_sqr("sbuffer", p_sequencer.sbuffer_sqr);
    require_agent_sqr("redirect", p_sequencer.redirect_sqr);
    require_agent_sqr("csr_ctrl", p_sequencer.csr_ctrl_sqr);
    if (uses_explicit_control_workers()) begin
        require_agent_sqr("fence", p_sequencer.fence_sqr);
    end
endfunction:require_real_smoke_sqr

// 抽象职责：VSEQ 只读取已冻结的 topology snapshot 决定是否显式启动两个 worker。
// 它不写 plus/mode，也不从 VSEQ 名称反推控制语义，确保 worker 与 main dispatch
// 位于同一场景生命周期域而不和 agent default producer 并发。
function bit memblock_dispatch_real_smoke_vseq::uses_explicit_control_workers();
    return memblock_sync_pkg::uses_control_barrier_topology();
endfunction:uses_explicit_control_workers

// 中文注释：禁用 control worker topology 时由本 VSEQ 持有稳定 Sv39 CSR producer。
// 其它 topology 保留原 CSR control worker 所有权，不能在同一 csr_ctrl_sqr 上双启动。
function bit memblock_dispatch_real_smoke_vseq::uses_static_mmu_sv39_csr();
    return memblock_sync_pkg::get_control_worker_topology_mode() ==
           memblock_sync_pkg::MEMBLOCK_CONTROL_TOPOLOGY_DISABLED;
endfunction:uses_static_mmu_sv39_csr

function void memblock_dispatch_real_smoke_vseq::initialize_shared_memory_store();
    // 中文注释：本 vseq 是 real-smoke topology 的唯一 shared memory lifecycle owner。
    // 在 DCache/Uncache responder fork 前清空上一次 testcase 的 backing、overlay 和写批次，
    // 再按 runtime range 开关配置统一物理窗口，避免两个 responder 的启动先后决定初始状态。
    mem_access_base_sequence::initialize_shared_memory_state(
        seq_csr_common::get_main_mem_ranges_en(),
        mem_access_base_sequence::mem_addr_t'(seq_csr_common::get_paddr_base()),
        seq_csr_common::get_paddr_range()
    );
endfunction:initialize_shared_memory_store

task memblock_dispatch_real_smoke_vseq::start_background_responders();
    dcache_mem__access_base_sequence        dcache_seq;
    sbuffer_mem_access_base_sequence        sbuffer_seq;
    memblock_redirect_dispatch_base_sequence redirect_seq;

    // 中文注释：三个 responder 都由 virtual sequencer 显式调度；它们在 real-smoke
    // global stop 且各自无 inflight 后自然退出，因此本 task 的 join 可作为完成握手。
    fork
        begin : start_dcache_responder
            `uvm_do_on(dcache_seq, p_sequencer.dcache_sqr)
        end
        begin : start_sbuffer_responder
            `uvm_do_on(sbuffer_seq, p_sequencer.sbuffer_sqr)
        end
        begin : start_redirect_responder
            `uvm_do_on(redirect_seq, p_sequencer.redirect_sqr)
        end
    join
endtask:start_background_responders

task memblock_dispatch_real_smoke_vseq::start_core_dispatch_flow();
    memblock_lsqenq_dispatch_base_sequence                  lsqenq_seq;
    memblock_issue_dispatch_base_sequence                   issue_seq;
    memblock_lsqcommit_dispatch_base_sequence               lsqcommit_seq;
    memblock_l2tlb_base_sequence                            l2tlb_seq;
    memblock_main_dispatch_auto_build_main_table_base_sequence main_seq;
    memblock_csr_control_base_sequence                      csr_control_seq;
    memblock_sfence_control_base_sequence                   sfence_control_seq;
    memblock_mmu_sv39_csr_sequence                          mmu_sv39_csr_seq;

    // 中文注释：agent sequence 只在对应真实 agent sequencer 上启动，主 orchestration
    // sequence 在 virtual sequencer 上启动；fork/join 和原 real-smoke 并发边界保持不变。
    fork
        begin : start_lsqenq_sequence
            `uvm_do_on(lsqenq_seq, p_sequencer.lsqenq_sqr)
        end
        begin : start_issue_sequence
            // 中文注释：issue 首次冻结 UID 的 TLB/PMA/PMP context；静态 Sv39
            // topology 下必须与 L2TLB responder 一样先等待 CSR mirror barrier，
            // 否则首批 issue 可能错误地继承 reset 的 Bare/M 态上下文。
            if (uses_static_mmu_sv39_csr()) begin
                wait_for_explicit_l2tlb_start_barrier();
            end
            `uvm_do_on(issue_seq, p_sequencer.lintsissue_sqr)
        end
        begin : start_lsqcommit_sequence
            `uvm_do_on(lsqcommit_seq, p_sequencer.lsqcommit_sqr)
        end
        begin : start_l2tlb_sequence
            wait_for_explicit_l2tlb_start_barrier();
            `uvm_do_on(l2tlb_seq, p_sequencer.L2tlb_sqr)
        end
        begin : start_main_sequence
            `uvm_do_on(main_seq, p_sequencer)
        end
        begin : start_static_mmu_sv39_csr_sequence
            if (uses_static_mmu_sv39_csr()) begin
                `uvm_do_on(mmu_sv39_csr_seq, p_sequencer.csr_ctrl_sqr)
            end
        end
        begin : start_csr_control_worker
            if (uses_explicit_control_workers()) begin
                `uvm_do_on(csr_control_seq, p_sequencer.csr_ctrl_sqr)
            end
        end
        begin : start_sfence_control_worker
            if (uses_explicit_control_workers()) begin
                `uvm_do_on(sfence_control_seq, p_sequencer.fence_sqr)
            end
        end
    join
endtask:start_core_dispatch_flow

// 抽象职责：显式 real-dispatch vseq 在启动 L2TLB responder 前等待主表完成初始化。
// 该屏障只属于 explicit vseq 拓扑；legacy default sequence 不调用它，普通无主表 testcase 不会被永久等待。
task memblock_dispatch_real_smoke_vseq::wait_for_explicit_l2tlb_start_barrier();
    common_data_transaction barrier_data;
    int unsigned wait_count;
    bit [63:0] expected_paddr_base;
    bit [43:0] expected_satp_ppn;
    bit [1:0] expected_priv_mode;

    barrier_data = common_data_transaction::get();
    if (barrier_data == null) begin
        `uvm_fatal(get_type_name(), "failed to get common_data_transaction for L2TLB start barrier")
    end
    wait_count = 0;
    while (!barrier_data.main_table_ready &&
           !barrier_data.is_global_stop_requested()) begin
        if (wait_count != 0 && (wait_count % 5000) == 0) begin
            `uvm_warning(get_type_name(),
                         $sformatf("still waiting for main table before explicit L2TLB start: wait_count=%0d main_trans_num=%0d next_uid=%0d",
                                   wait_count,
                                   barrier_data.main_trans_num,
                                   barrier_data.next_uid))
        end
        #1;
        wait_count++;
    end

    if (barrier_data.is_global_stop_requested() || !uses_static_mmu_sv39_csr()) begin
        return;
    end

    // 中文注释：static CSR sequence 必须先经 driver/monitor 写入 mmu_csr_state，
    // L2TLB responder 才能以 Sv39 的 C-2 runtime context 接收首个 request。仅等
    // main_table_ready 会让 responder 读取 reset 后的 Bare/M 态 snapshot。U 态是
    // Sv39 生效的必要条件，必须与 satp root 在同一份 monitor snapshot 中确认。
    expected_paddr_base = seq_csr_common::get_paddr_base();
    expected_satp_ppn = expected_paddr_base[55:12];
    expected_priv_mode = 2'd0;
    wait_count = 0;
    while ((barrier_data.mmu_csr_state == null ||
            barrier_data.mmu_csr_state.satp_mode != 4'd8 ||
            barrier_data.mmu_csr_state.satp_ppn != expected_satp_ppn ||
            barrier_data.mmu_csr_state.priv_virt != 1'b0 ||
            barrier_data.mmu_csr_state.priv_imode != expected_priv_mode ||
            barrier_data.mmu_csr_state.priv_dmode != expected_priv_mode) &&
           !barrier_data.is_global_stop_requested()) begin
        if (wait_count != 0 && (wait_count % 5000) == 0) begin
            `uvm_warning(get_type_name(),
                         $sformatf("still waiting for static Sv39/U CSR mirror: wait_count=%0d expected_ppn=0x%0h current_mode=0x%0h current_ppn=0x%0h current_virt=%0d current_imode=%0d current_dmode=%0d",
                                   wait_count,
                                   expected_satp_ppn,
                                   (barrier_data.mmu_csr_state == null) ? '0 : barrier_data.mmu_csr_state.satp_mode,
                                   (barrier_data.mmu_csr_state == null) ? '0 : barrier_data.mmu_csr_state.satp_ppn,
                                   (barrier_data.mmu_csr_state == null) ? '0 : barrier_data.mmu_csr_state.priv_virt,
                                   (barrier_data.mmu_csr_state == null) ? '0 : barrier_data.mmu_csr_state.priv_imode,
                                   (barrier_data.mmu_csr_state == null) ? '0 : barrier_data.mmu_csr_state.priv_dmode))
        end
        #1;
        wait_count++;
    end
endtask:wait_for_explicit_l2tlb_start_barrier

`endif
