//=========================================================
//File name    : memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq.sv
//Module name  : memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq
//Discribution : strict PBMT=0/non-NC real dispatch virtual sequence
//=========================================================
`ifndef MEMBLOCK_DISPATCH_REAL_MMU_SV39_PBMT0_NON_NC_VSEQ__SV
`define MEMBLOCK_DISPATCH_REAL_MMU_SV39_PBMT0_NON_NC_VSEQ__SV

// 中文注释：该 VSEQ 复用 real-smoke 的所有 memory/LSQ/issue/commit/L2TLB
// responder 生命周期，只替换 CSR producer。它不建立第二套 L2TLB queue，也不
// 修改现有 PBMT-on 或 legacy testcase 的启动拓扑。
class memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq extends
    memblock_dispatch_real_smoke_vseq;

    `uvm_object_utils(memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq)

    extern function new(string name =
        "memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq");
    extern virtual task start_core_dispatch_flow();

endclass:memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq

function memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq::new(
    string name = "memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq"
);
    super.new(name);
endfunction:new

// 抽象职责：在同一 real-smoke virtual sequencer 上并发启动所有真实 dispatch
// child sequence，并让严格 PBMT=0 CSR producer 成为 csr_ctrl_sqr 的唯一 owner。
// issue/L2TLB 仍沿用父类的 Sv39/U mirror barrier，保证 PMP bootstrap 完成后才
// 接受首个翻译请求。
task memblock_dispatch_real_mmu_sv39_pbmt0_non_nc_vseq::start_core_dispatch_flow();
    memblock_lsqenq_dispatch_base_sequence                  lsqenq_seq;
    memblock_issue_dispatch_base_sequence                   issue_seq;
    memblock_lsqcommit_dispatch_base_sequence               lsqcommit_seq;
    memblock_l2tlb_base_sequence                            l2tlb_seq;
    memblock_main_dispatch_auto_build_main_table_base_sequence main_seq;
    memblock_mmu_sv39_pbmt0_non_nc_csr_sequence              csr_seq;

    if (memblock_sync_pkg::get_control_worker_topology_mode() !=
        memblock_sync_pkg::MEMBLOCK_CONTROL_TOPOLOGY_DISABLED) begin
        `uvm_fatal(get_type_name(),
                   "strict PBMT=0 VSEQ requires disabled control topology")
    end

    fork
        begin : start_pbmt0_lsqenq_sequence
            `uvm_do_on(lsqenq_seq, p_sequencer.lsqenq_sqr)
        end
        begin : start_pbmt0_issue_sequence
            wait_for_explicit_l2tlb_start_barrier();
            `uvm_do_on(issue_seq, p_sequencer.lintsissue_sqr)
        end
        begin : start_pbmt0_lsqcommit_sequence
            `uvm_do_on(lsqcommit_seq, p_sequencer.lsqcommit_sqr)
        end
        begin : start_pbmt0_l2tlb_sequence
            wait_for_explicit_l2tlb_start_barrier();
            `uvm_do_on(l2tlb_seq, p_sequencer.L2tlb_sqr)
        end
        begin : start_pbmt0_main_sequence
            `uvm_do_on(main_seq, p_sequencer)
        end
        begin : start_pbmt0_csr_sequence
            `uvm_do_on(csr_seq, p_sequencer.csr_ctrl_sqr)
        end
    join
endtask:start_core_dispatch_flow

`endif
