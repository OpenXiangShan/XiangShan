//=========================================================
//File name    : memblock_dispatch_manual_control_vseq.sv
//Author       : OpenAI_Codex
//Module name  : memblock_dispatch_manual_control_vseq
//Discribution : real dispatch manual-control virtual sequence
//Date         : 2026-08-14
//=========================================================
`ifndef MEMBLOCK_DISPATCH_MANUAL_CONTROL_VSEQ__SV
`define MEMBLOCK_DISPATCH_MANUAL_CONTROL_VSEQ__SV

class memblock_dispatch_manual_control_vseq extends memblock_dispatch_real_smoke_vseq;

    `uvm_object_utils(memblock_dispatch_manual_control_vseq)

    extern function new(string name = "memblock_dispatch_manual_control_vseq");
    extern virtual task start_core_dispatch_flow();

endclass:memblock_dispatch_manual_control_vseq

function memblock_dispatch_manual_control_vseq::new(
    string name = "memblock_dispatch_manual_control_vseq"
);
    super.new(name);
endfunction:new

// 抽象职责：复用 real-smoke 的 responder、LSQ、issue、L2TLB 和两个显式 control
// worker 生命周期，只将 generic AUTO main builder 换为 direct manual-control builder。
// mode 仍由 testcase build 阶段冻结，本 VSEQ 只校验并消费该快照。
task memblock_dispatch_manual_control_vseq::start_core_dispatch_flow();
    memblock_lsqenq_dispatch_base_sequence                         lsqenq_seq;
    memblock_issue_dispatch_base_sequence                          issue_seq;
    memblock_lsqcommit_dispatch_base_sequence                      lsqcommit_seq;
    memblock_l2tlb_base_sequence                                   l2tlb_seq;
    memblock_main_dispatch_manual_control_main_table_sequence      main_seq;
    memblock_csr_control_base_sequence                             csr_control_seq;
    memblock_sfence_control_base_sequence                          sfence_control_seq;

    if (memblock_sync_pkg::get_control_worker_topology_mode() !=
        memblock_sync_pkg::MEMBLOCK_CONTROL_TOPOLOGY_MANUAL_CONTROL_TABLE) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("manual control VSEQ requires MANUAL_CONTROL_MAIN_TABLE mode, got %0d",
                             memblock_sync_pkg::get_control_worker_topology_mode()))
    end

    fork
        begin : start_manual_control_lsqenq_sequence
            `uvm_do_on(lsqenq_seq, p_sequencer.lsqenq_sqr)
        end
        begin : start_manual_control_issue_sequence
            `uvm_do_on(issue_seq, p_sequencer.lintsissue_sqr)
        end
        begin : start_manual_control_lsqcommit_sequence
            `uvm_do_on(lsqcommit_seq, p_sequencer.lsqcommit_sqr)
        end
        begin : start_manual_control_l2tlb_sequence
            wait_for_explicit_l2tlb_start_barrier();
            `uvm_do_on(l2tlb_seq, p_sequencer.L2tlb_sqr)
        end
        begin : start_manual_control_main_sequence
            `uvm_do_on(main_seq, p_sequencer)
        end
        begin : start_manual_control_csr_worker
            `uvm_do_on(csr_control_seq, p_sequencer.csr_ctrl_sqr)
        end
        begin : start_manual_control_sfence_worker
            `uvm_do_on(sfence_control_seq, p_sequencer.fence_sqr)
        end
    join
endtask:start_core_dispatch_flow

`endif
