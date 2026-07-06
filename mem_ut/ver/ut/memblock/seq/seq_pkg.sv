//=========================================================
//File name    : seq_pkg.sv
//Author       : OpenAI_Codex
//Module name  : seq_pkg
//Discribution : memblock sequence package
//Date         : 2026-05-21
//=========================================================
`ifndef MEMBLOCK_SEQ_PKG__SV
`define MEMBLOCK_SEQ_PKG__SV

`ifndef TCNT_HAD_INCLUDE_UVM_MACROS
`define TCNT_HAD_INCLUDE_UVM_MACROS
    `include "uvm_macros.svh"
`endif

package seq_pkg;

    import uvm_pkg::*;
    import tcnt_realtime::*;
    import tcnt_dec_base::*;
    import tcnt_common_method::*;
    import tcnt_base_pkg::*;
    import backendToTopBypass_agent_agent_dec::*;
    import backendToTopBypass_agent_agent_pkg::*;
    import fence_agent_agent_dec::*;
    import fence_agent_agent_pkg::*;
    import csr_ctrl_agent_agent_dec::*;
    import csr_ctrl_agent_agent_pkg::*;
    import lsqcommit_agent_agent_dec::*;
    import lsqcommit_agent_agent_pkg::*;
    import lsqenq_agent_agent_dec::*;
    import lsqenq_agent_agent_pkg::*;
    import lintsissue_agent_agent_dec::*;
    import lintsissue_agent_agent_pkg::*;
    import vecissue_agent_agent_dec::*;
    import vecissue_agent_agent_pkg::*;
    import redirect_agent_agent_dec::*;
    import redirect_agent_agent_pkg::*;
    import sbuffer_agent_agent_dec::*;
    import sbuffer_agent_agent_pkg::*;
    import dcache_agent_agent_dec::*;
    import dcache_agent_agent_pkg::*;
    import int_sink_agent_agent_dec::*;
    import int_sink_agent_agent_pkg::*;
    import L2tlb_agent_agent_dec::*;
    import L2tlb_agent_agent_pkg::*;
    import itlb_agent_agent_dec::*;
    import itlb_agent_agent_pkg::*;
    import prefetch_agent_agent_dec::*;
    import prefetch_agent_agent_pkg::*;
    import io_mem_to_ooo_ctrl_agent_agent_dec::*;
    import io_mem_to_ooo_ctrl_agent_agent_pkg::*;
    import io_mem_to_ooo_int_wb_agent_agent_dec::*;
    import io_mem_to_ooo_int_wb_agent_agent_pkg::*;
    import io_mem_to_ooo_vec_wb_agent_agent_dec::*;
    import io_mem_to_ooo_vec_wb_agent_agent_pkg::*;
    import io_mem_to_ooo_wakeup_agent_agent_dec::*;
    import io_mem_to_ooo_wakeup_agent_agent_pkg::*;
    import io_mem_to_ooo_iq_feedback_agent_agent_dec::*;
    import io_mem_to_ooo_iq_feedback_agent_agent_pkg::*;
    import other_ctrl_agent_agent_dec::*;
    import other_ctrl_agent_agent_pkg::*;

    import memblock_dec::*;
    import memblock_common_pkg::*;
    import plus_pkg::*;

    `include "memblock_dispatch_types.sv"
    `include "rob_order_util.sv"
    `include "seq_csr_common.sv"
    `include "mmu_csr_runtime_state.sv"
    `include "main_control_transaction.sv"
    `include "status_transaction.sv"
    `include "memblock_tlb_entry.sv"
    `include "tlb_map_builder.sv"
    `include "common_data_transaction.sv"
    `include "lsq_ctrl_model.sv"
    `include "issue_queue_scheduler.sv"
    `include "issue_field_assigner.sv"
    `include "writeback_status_handler.sv"
    `include "dispatch_monitor_batch_handler.sv"
    `include "exception_redirect_replay_handler.sv"
    `include "lsq_commit_handler.sv"
    `include "dispatch_monitor_event_adapter.sv"
    `include "memblock_dispatch_base_sequence.sv"
    `include "soft_test_memblock_dispatch_smoke_sequence.sv"
    `include "soft_test_memblock_dispatch_fault_smoke_sequence.sv"
    `include "soft_test_memblock_dispatch_replay_smoke_sequence.sv"
    `include "memblock_lsqenq_dispatch_base_sequence.sv"
    `include "memblock_issue_dispatch_base_sequence.sv"
    `include "memblock_lsqcommit_dispatch_base_sequence.sv"
    `include "memblock_flushsb_base_sequence.sv"
    `include "memblock_redirect_dispatch_base_sequence.sv"
    `include "memblock_l2tlb_base_sequence.sv"
    `include "memblock_main_dispatch_auto_build_main_table_base_sequence.sv"
    `include "memblock_main_dispatch_manual_main_table_sequence.sv"
    `include "mem_base_sequence.sv"
    `include "memblock_virtual_sequencer.sv"
    `include "virtual_base_sequence.sv"
    `include "memblock_dispatch_real_smoke_vseq.sv"

endpackage

import seq_pkg::*;

`endif
