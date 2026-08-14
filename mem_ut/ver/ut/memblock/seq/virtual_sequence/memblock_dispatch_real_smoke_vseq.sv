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
    if (uses_explicit_control_workers()) begin
        require_agent_sqr("csr_ctrl", p_sequencer.csr_ctrl_sqr);
        require_agent_sqr("fence", p_sequencer.fence_sqr);
    end
endfunction:require_real_smoke_sqr

// 抽象职责：VSEQ 只读取已冻结的 topology snapshot 决定是否显式启动两个 worker。
// 它不写 plus/mode，也不从 VSEQ 名称反推控制语义，确保 worker 与 main dispatch
// 位于同一场景生命周期域而不和 agent default producer 并发。
function bit memblock_dispatch_real_smoke_vseq::uses_explicit_control_workers();
    return memblock_sync_pkg::uses_control_barrier_topology();
endfunction:uses_explicit_control_workers

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

    // 中文注释：agent sequence 只在对应真实 agent sequencer 上启动，主 orchestration
    // sequence 在 virtual sequencer 上启动；fork/join 和原 real-smoke 并发边界保持不变。
    fork
        begin : start_lsqenq_sequence
            `uvm_do_on(lsqenq_seq, p_sequencer.lsqenq_sqr)
        end
        begin : start_issue_sequence
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

    barrier_data = common_data_transaction::get();
    if (barrier_data == null) begin
        `uvm_fatal(get_type_name(), "failed to get common_data_transaction for L2TLB start barrier")
    end
    wait_count = 0;
    while (!barrier_data.main_table_ready) begin
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
endtask:wait_for_explicit_l2tlb_start_barrier

`endif
