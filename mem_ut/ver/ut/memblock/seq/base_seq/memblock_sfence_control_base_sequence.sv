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

// 抽象职责：主表决定范围和原始目标；worker仅首次按五权重选择类型，重试复用冻结payload。
task memblock_sfence_control_base_sequence::configure_sfence_control_xaction(
    ref memblock_sfence_control_action_t action,
    output fence_agent_agent_xaction tr
);
    longint unsigned total_weight, pick, accumulated;
    int unsigned selected_kind;
    bit selected;
    memblock_sync_pkg::dispatch_raw_csr_t csr_snapshot;
    memblock_sync_pkg::memblock_control_csr_runtime_baseline_t csr_baseline;
    int unsigned csr_snapshot_seq;

    if (!action.owner.valid)
        `uvm_fatal(get_type_name(), "SFence action has invalid owner")
    if (!memblock_sync_pkg::get_control_csr_runtime_baseline(csr_baseline) ||
        !memblock_sync_pkg::get_latest_runtime_csr_snapshot(csr_snapshot, csr_snapshot_seq) ||
        !csr_snapshot.valid || csr_snapshot_seq < csr_baseline.first_snapshot_seq)
        `uvm_fatal(get_type_name(), "SFence requires current runtime CSR baseline")

    if (!action.type_frozen) begin
        total_weight = 0;
        for (int unsigned kind = 0; kind < 5; kind++)
            total_weight += 64'(seq_csr_common::get_sfence_type_weight(kind));
        if (total_weight == 0)
            `uvm_fatal(get_type_name(), "SFence has no positive type weight")
        // 中文注释：直接求解64位区间，避免urandom_range将总权重截为32位。
        if (!std::randomize(pick) with { pick < total_weight; })
            `uvm_fatal(get_type_name(), "SFence weighted type selection failed")
        accumulated = 0;
        selected = 0;
        selected_kind = 0;
        for (int unsigned kind = 0; kind < 5; kind++) begin
            accumulated += 64'(seq_csr_common::get_sfence_type_weight(kind));
            if (!selected && pick < accumulated) begin
                selected_kind = kind;
                selected = 1;
            end
        end
        if (!selected) `uvm_fatal(get_type_name(), "SFence selection missed cumulative range")
        action.expected_fence = '{default:'0};
        action.expected_fence.valid = 1;
        action.expected_fence.addr = action.addr;
        action.expected_fence.id = action.id;
        case (action.scope)
            MEMBLOCK_SFENCE_SCOPE_ALL_ADDR_ALL_ID: begin
                action.expected_fence.ignore_addr = 1; action.expected_fence.ignore_id = 1;
            end
            MEMBLOCK_SFENCE_SCOPE_ADDR_ALL_ID: begin
                action.expected_fence.ignore_addr = 0; action.expected_fence.ignore_id = 1;
            end
            MEMBLOCK_SFENCE_SCOPE_ALL_ADDR_ID: begin
                action.expected_fence.ignore_addr = 1; action.expected_fence.ignore_id = 0;
            end
            MEMBLOCK_SFENCE_SCOPE_ADDR_ID: begin
                action.expected_fence.ignore_addr = 0; action.expected_fence.ignore_id = 0;
            end
            default: `uvm_fatal(get_type_name(), "SFence action has invalid main-table scope")
        endcase
        action.expected_fence.hv = selected_kind inside {1, 3};
        action.expected_fence.hg = selected_kind inside {2, 4};
        action.expected_fence.flushPipe = selected_kind < 3;
        if (action.expected_fence.hg) begin
            action.expected_fence.target_stage = MEMBLOCK_SFENCE_TARGET_G_S2;
            if (!action.expected_fence.ignore_id && action.id[15:14] != 0)
                `uvm_fatal(get_type_name(), "GVMA specified VMID exceeds the V2 14-bit contract")
        end else if (action.expected_fence.hv || csr_snapshot.priv_virt)
            action.expected_fence.target_stage = MEMBLOCK_SFENCE_TARGET_VS_S1;
        else action.expected_fence.target_stage = MEMBLOCK_SFENCE_TARGET_HS_S1;
        action.type_frozen = 1;
    end
    tr = fence_agent_agent_xaction::type_id::create(
        $sformatf("sfence_control_uid_%0d_gen_%0d", action.owner.uid, action.owner.action_generation));
    if (tr == null) `uvm_fatal(get_type_name(), "failed to create SFence xaction")
    tr.pre_pkt_gap = 0;
    tr.post_pkt_gap = 0;
    tr.io_ooo_to_mem_sfence_valid = action.expected_fence.valid;
    tr.io_ooo_to_mem_sfence_bits_rs1 = action.expected_fence.ignore_addr;
    tr.io_ooo_to_mem_sfence_bits_rs2 = action.expected_fence.ignore_id;
    tr.io_ooo_to_mem_sfence_bits_addr = action.expected_fence.addr;
    tr.io_ooo_to_mem_sfence_bits_id = action.expected_fence.id;
    tr.io_ooo_to_mem_sfence_bits_hv = action.expected_fence.hv;
    tr.io_ooo_to_mem_sfence_bits_hg = action.expected_fence.hg;
    tr.io_ooo_to_mem_sfence_bits_flushPipe = action.expected_fence.flushPipe;
    `uvm_info("SFENCE_CONTROL_PAYLOAD",
              $sformatf("uid=%0d scope=%s addr=0x%0h id=0x%0h hv=%0b hg=%0b flushPipe=%0b",
                        action.owner.uid, action.scope.name(), action.expected_fence.addr,
                        action.expected_fence.id, action.expected_fence.hv,
                        action.expected_fence.hg, action.expected_fence.flushPipe), UVM_LOW)
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
