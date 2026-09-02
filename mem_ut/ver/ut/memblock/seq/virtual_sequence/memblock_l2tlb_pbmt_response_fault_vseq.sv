//=========================================================
//File name    : memblock_l2tlb_pbmt_response_fault_vseq.sv
//Module name  : memblock_l2tlb_pbmt_response_fault_vseq
//Discribution : real DUT PBMT dynamic response-fault virtual sequence
//=========================================================
`ifndef MEMBLOCK_L2TLB_PBMT_RESPONSE_FAULT_VSEQ__SV
`define MEMBLOCK_L2TLB_PBMT_RESPONSE_FAULT_VSEQ__SV

// 中文注释：该 VSEQ 复用 real-smoke 的 memory/redirect responder 生命周期，只替换
// generic main builder 和静态 CSR producer。L2TLB pending queue 仍只有 responder 的
// 一份；本场景不会建立第二套 token、CSR runtime state 或 TLB 表。
class memblock_l2tlb_pbmt_response_fault_vseq extends memblock_dispatch_real_smoke_vseq;

    `uvm_object_utils(memblock_l2tlb_pbmt_response_fault_vseq)

    extern function new(string name = "memblock_l2tlb_pbmt_response_fault_vseq");
    extern virtual task start_core_dispatch_flow();

endclass:memblock_l2tlb_pbmt_response_fault_vseq

function memblock_l2tlb_pbmt_response_fault_vseq::new(
    string name = "memblock_l2tlb_pbmt_response_fault_vseq"
);
    super.new(name);
endfunction:new

// 抽象职责：在 real-smoke 的已连接 sequencer 上并发启动真实 enqueue/issue/commit/
// L2TLB/main flow 和唯一 PBMT CSR producer。所有 child sequence 自然退出后才返回，
// 因此父类可在清 activity 标志前等待 memory responder 完成最终 drain。
task memblock_l2tlb_pbmt_response_fault_vseq::start_core_dispatch_flow();
    memblock_lsqenq_dispatch_base_sequence                  lsqenq_seq;
    memblock_issue_dispatch_base_sequence                   issue_seq;
    memblock_lsqcommit_dispatch_base_sequence               lsqcommit_seq;
    memblock_l2tlb_base_sequence                             l2tlb_seq;
    memblock_main_dispatch_pbmt_response_fault_sequence     main_seq;
    memblock_l2tlb_pbmt_toggle_csr_sequence                 csr_seq;

    if (memblock_sync_pkg::get_control_worker_topology_mode() !=
        memblock_sync_pkg::MEMBLOCK_CONTROL_TOPOLOGY_DISABLED) begin
        `uvm_fatal(get_type_name(),
                   "PBMT response-fault VSEQ requires disabled control topology")
    end

    fork
        begin : start_pbmt_lsqenq_sequence
            `uvm_do_on(lsqenq_seq, p_sequencer.lsqenq_sqr)
        end
        begin : start_pbmt_issue_sequence
            wait_for_explicit_l2tlb_start_barrier();
            `uvm_do_on(issue_seq, p_sequencer.lintsissue_sqr)
        end
        begin : start_pbmt_lsqcommit_sequence
            `uvm_do_on(lsqcommit_seq, p_sequencer.lsqcommit_sqr)
        end
        begin : start_pbmt_l2tlb_sequence
            wait_for_explicit_l2tlb_start_barrier();
            `uvm_do_on(l2tlb_seq, p_sequencer.L2tlb_sqr)
        end
        begin : start_pbmt_main_sequence
            `uvm_do_on(main_seq, p_sequencer)
        end
        begin : start_pbmt_toggle_csr_sequence
            `uvm_do_on(csr_seq, p_sequencer.csr_ctrl_sqr)
        end
    join
endtask:start_core_dispatch_flow

`endif
