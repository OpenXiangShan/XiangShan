//=========================================================
//File name    : memblock_sfence_control_base_sequence.sv
//Author       : OpenAI_Codex
//Module name  : memblock_sfence_control_base_sequence
//Discribution : SFence control barrier worker
//Date         : 2026-08-14
//=========================================================
`ifndef MEMBLOCK_SFENCE_CONTROL_BASE_SEQUENCE__SV
`define MEMBLOCK_SFENCE_CONTROL_BASE_SEQUENCE__SV

// 中文注释：该 worker 是 active control topology 下 fence_sqr 的唯一 producer。
// 它只消费 owner 化 token 并在 start_item 前 arm C0 匹配，不直接处理 raw fence、
// L2TLB C4 或 status completion。
class memblock_sfence_control_base_sequence extends uvm_sequence #(fence_agent_agent_xaction);

    common_data_transaction data;

    `uvm_object_utils(memblock_sfence_control_base_sequence)

    extern function new(string name = "memblock_sfence_control_base_sequence");
    extern virtual task body();
    extern virtual task wait_for_sfence_work_or_shutdown();
    extern virtual task configure_sfence_control_xaction(
        ref memblock_sfence_control_action_t action,
        output fence_agent_agent_xaction tr
    );
    extern virtual task drive_sfence_control_xaction(
        ref memblock_sfence_control_action_t action,
        input fence_agent_agent_xaction tr
    );

endclass:memblock_sfence_control_base_sequence

function memblock_sfence_control_base_sequence::new(
    string name = "memblock_sfence_control_base_sequence"
);
    super.new(name);
    data = null;
endfunction:new

task memblock_sfence_control_base_sequence::body();
    memblock_sfence_control_action_t action;
    fence_agent_agent_xaction tr;

    seq_csr_common::init();
    data = common_data_transaction::get();
    if (data == null) begin
        `uvm_fatal(get_type_name(), "failed to get common_data_transaction")
    end
    if (!memblock_sync_pkg::uses_control_barrier_topology()) begin
        `uvm_fatal(get_type_name(), "SFence control worker started outside active control topology")
    end

    forever begin
        if (data.try_pop_sfence_control_action(action)) begin
            configure_sfence_control_xaction(action, tr);
            drive_sfence_control_xaction(action, tr);
            continue;
        end
        if (data.control_worker_can_exit(1'b0)) begin
            data.mark_control_worker_exited(1'b0);
            break;
        end
        wait_for_sfence_work_or_shutdown();
    end
endtask:body

task memblock_sfence_control_base_sequence::wait_for_sfence_work_or_shutdown();
    fork
        @(data.sfence_control_action_available_ev);
        @(data.control_worker_shutdown_ev);
    join_any
    disable fork;
endtask:wait_for_sfence_work_or_shutdown

// 抽象职责：构造固定、非随机的基础 SFence payload。未来 SFence/HFence 专项只能
// 修改本函数的 payload 选择；C0/C4 owner 匹配和 worker 生命周期保持不变。
task memblock_sfence_control_base_sequence::configure_sfence_control_xaction(
    ref memblock_sfence_control_action_t action,
    output fence_agent_agent_xaction tr
);
    if (!action.owner.valid) begin
        `uvm_fatal(get_type_name(), "SFence action has invalid owner")
    end
    tr = fence_agent_agent_xaction::type_id::create(
        $sformatf("sfence_control_uid_%0d_gen_%0d", action.owner.uid,
                  action.owner.action_generation));
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "failed to create SFence xaction")
    end
    tr.pre_pkt_gap = 0;
    tr.post_pkt_gap = 0;
    tr.io_ooo_to_mem_sfence_valid = 1'b1;
    tr.io_ooo_to_mem_sfence_bits_rs1 = 1'b0;
    tr.io_ooo_to_mem_sfence_bits_rs2 = 1'b0;
    tr.io_ooo_to_mem_sfence_bits_addr = '0;
    tr.io_ooo_to_mem_sfence_bits_id = '0;
    tr.io_ooo_to_mem_sfence_bits_hv = 1'b0;
    tr.io_ooo_to_mem_sfence_bits_hg = 1'b0;
    tr.io_ooo_to_mem_sfence_bits_flushPipe = 1'b0;

    action.expected_fence = '{default:'0};
    action.expected_fence.valid = 1'b1;
    action.expected_fence.ignore_addr = 1'b0;
    action.expected_fence.ignore_id = 1'b0;
    action.expected_fence.addr = '0;
    action.expected_fence.id = '0;
    action.expected_fence.hv = 1'b0;
    action.expected_fence.hg = 1'b0;
    action.expected_fence.target_stage = MEMBLOCK_SFENCE_TARGET_HS_S1;
endtask:configure_sfence_control_xaction

// 抽象职责：在 start_item 前冻结 L2TLB event/reset baseline 并 arm C0，随后交付
// xaction。finish_item 仅表示 driver sendover；adapter 记录的 C0/C4 才能推进状态。
task memblock_sfence_control_base_sequence::drive_sfence_control_xaction(
    ref memblock_sfence_control_action_t action,
    input fence_agent_agent_xaction tr
);
    action.pre_drive_event_seq = memblock_sync_pkg::last_allocated_l2tlb_event_seq;
    action.l2tlb_reset_epoch_at_arm = memblock_sync_pkg::get_l2tlb_current_reset_epoch();
    action.sfence_c0_match_armed = 1'b1;
    data.arm_sfence_control_c0_match(action);
    start_item(tr);
    finish_item(tr);
    data.mark_sfence_control_sendover(action);
endtask:drive_sfence_control_xaction

`endif
