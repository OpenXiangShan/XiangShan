//=========================================================
//File name    : memblock_csr_control_base_sequence.sv
//Author       : OpenAI_Codex
//Module name  : memblock_csr_control_base_sequence
//Discribution : CSR control barrier worker
//Date         : 2026-08-14
//=========================================================
`ifndef MEMBLOCK_CSR_CONTROL_BASE_SEQUENCE__SV
`define MEMBLOCK_CSR_CONTROL_BASE_SEQUENCE__SV

// 中文注释：该 worker 是 active control topology 下 csr_ctrl_sqr 的唯一 producer。
// 它只消费 service 写入的持久 action token；不扫描主表、不生成 owner，也不把
// driver sendover 误写为 monitor runtime snapshot 完成。
class memblock_csr_control_base_sequence extends uvm_sequence #(csr_ctrl_agent_agent_xaction);

    common_data_transaction data;

    `uvm_object_utils(memblock_csr_control_base_sequence)

    extern function new(string name = "memblock_csr_control_base_sequence");
    extern virtual task body();
    extern virtual task wait_for_csr_work_or_shutdown();
    extern virtual task configure_csr_control_xaction(
        ref memblock_csr_control_action_t action,
        output csr_ctrl_agent_agent_xaction tr
    );
    extern virtual task drive_csr_control_xaction(
        input memblock_csr_control_action_t action,
        input csr_ctrl_agent_agent_xaction tr
    );
    extern virtual function void initialize_csr_xaction_from_runtime(
        input memblock_sync_pkg::dispatch_raw_csr_t runtime,
        input csr_ctrl_agent_agent_xaction tr
    );

endclass:memblock_csr_control_base_sequence

function memblock_csr_control_base_sequence::new(
    string name = "memblock_csr_control_base_sequence"
);
    super.new(name);
    data = null;
endfunction:new

task memblock_csr_control_base_sequence::body();
    memblock_csr_control_action_t action;
    csr_ctrl_agent_agent_xaction tr;

    seq_csr_common::init();
    data = common_data_transaction::get();
    if (data == null) begin
        `uvm_fatal(get_type_name(), "failed to get common_data_transaction")
    end
    if (!memblock_sync_pkg::uses_control_barrier_topology()) begin
        `uvm_fatal(get_type_name(), "CSR control worker started outside active control topology")
    end

    forever begin
        if (data.try_pop_csr_control_action(action)) begin
            if (action.completion_profile !=
                MEMBLOCK_CONTROL_COMPLETION_RUNTIME_CSR_SNAPSHOT) begin
                `uvm_fatal(get_type_name(),
                           $sformatf("CSR worker got unsupported completion profile=%0d uid=%0d",
                                     action.completion_profile, action.owner.uid))
            end
            configure_csr_control_xaction(action, tr);
            drive_csr_control_xaction(action, tr);
            continue;
        end
        if (data.control_worker_can_exit(1'b1)) begin
            data.mark_control_worker_exited(1'b1);
            break;
        end
        wait_for_csr_work_or_shutdown();
    end
endtask:body

// 抽象职责：只等待唤醒，不承担 token 存在性判断。两个 event 任一到达后都回到
// worker 主循环重查 queue，因此 worker 晚启动或连续 token 都不会丢失动作。
task memblock_csr_control_base_sequence::wait_for_csr_work_or_shutdown();
    fork
        @(data.csr_control_action_available_ev);
        @(data.control_worker_shutdown_ev);
    join_any
    disable fork;
endtask:wait_for_csr_work_or_shutdown

// 抽象职责：从 action-local monitor baseline 构造一个不随机化的 CSR xaction，
// 并选择首版可由 runtime snapshot 证明的 SATP ASID 改动。该函数不等待、不驱动、
// 不更新 status completion；后续 CSR 专项只能在这里扩展 payload。
task memblock_csr_control_base_sequence::configure_csr_control_xaction(
    ref memblock_csr_control_action_t action,
    output csr_ctrl_agent_agent_xaction tr
);
    memblock_sync_pkg::dispatch_raw_csr_t expected_runtime;

    if (!action.owner.valid || !action.csr_baseline_valid ||
        action.completion_profile !=
            MEMBLOCK_CONTROL_COMPLETION_RUNTIME_CSR_SNAPSHOT) begin
        `uvm_fatal(get_type_name(), "CSR action is missing owner/baseline/profile")
    end
    tr = csr_ctrl_agent_agent_xaction::type_id::create(
        $sformatf("csr_control_uid_%0d_gen_%0d", action.owner.uid,
                  action.owner.action_generation));
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "failed to create CSR control xaction")
    end

    expected_runtime = action.csr_baseline;
    expected_runtime.valid = 1'b1;
    expected_runtime.satp_asid =
        (action.csr_baseline.satp_asid == 16'hffff) ? 16'h0000 :
        action.csr_baseline.satp_asid + 16'h0001;
    expected_runtime.satp_changed = 1'b1;
    // 旧 monitor pulse 不能被重驱；首版只定义 SATP 变化作为 completion evidence。
    expected_runtime.vsatp_changed = 1'b0;
    expected_runtime.hgatp_changed = 1'b0;
    expected_runtime.priv_virt_changed = 1'b0;
    if (!memblock_sync_pkg::raw_csr_payload_changed(action.csr_baseline,
                                                     expected_runtime)) begin
        expected_runtime.satp_asid = action.csr_baseline.satp_asid ^ 16'h0001;
        if (!memblock_sync_pkg::raw_csr_payload_changed(action.csr_baseline,
                                                         expected_runtime)) begin
            `uvm_fatal(get_type_name(), "CSR control payload is monitor-visible no-op")
        end
    end

    initialize_csr_xaction_from_runtime(expected_runtime, tr);
    tr.pre_pkt_gap = 0;
    tr.post_pkt_gap = 0;
    tr.io_ooo_to_mem_csrCtrl_flush_l2_enable = 1'b0;
    action.expected_runtime_csr_valid = 1'b1;
    action.expected_runtime_csr = expected_runtime;
endtask:configure_csr_control_xaction

// 抽象职责：用 monitor raw 中存在的字段填写完整 CSR driver item，并显式关闭
// 不在该 raw snapshot completion 语义中的一次性写/trigger pulse。其余 agent xaction
// 字段均是 2-state 类型，new() 的确定零值构成 driver-safe baseline，不允许 randomize。
function void memblock_csr_control_base_sequence::initialize_csr_xaction_from_runtime(
    input memblock_sync_pkg::dispatch_raw_csr_t runtime,
    input csr_ctrl_agent_agent_xaction tr
);
    if (tr == null || !runtime.valid) begin
        `uvm_fatal(get_type_name(), "initialize_csr_xaction_from_runtime got invalid input")
    end
    tr.io_ooo_to_mem_tlbCsr_satp_mode = runtime.satp_mode;
    tr.io_ooo_to_mem_tlbCsr_satp_asid = runtime.satp_asid;
    tr.io_ooo_to_mem_tlbCsr_satp_ppn = runtime.satp_ppn;
    tr.io_ooo_to_mem_tlbCsr_satp_changed = runtime.satp_changed;
    tr.io_ooo_to_mem_tlbCsr_vsatp_mode = runtime.vsatp_mode;
    tr.io_ooo_to_mem_tlbCsr_vsatp_asid = runtime.vsatp_asid;
    tr.io_ooo_to_mem_tlbCsr_vsatp_ppn = runtime.vsatp_ppn;
    tr.io_ooo_to_mem_tlbCsr_vsatp_changed = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_hgatp_mode = runtime.hgatp_mode;
    tr.io_ooo_to_mem_tlbCsr_hgatp_vmid = runtime.hgatp_vmid;
    tr.io_ooo_to_mem_tlbCsr_hgatp_ppn = runtime.hgatp_ppn;
    tr.io_ooo_to_mem_tlbCsr_hgatp_changed = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_priv_mxr = runtime.priv_mxr;
    tr.io_ooo_to_mem_tlbCsr_priv_sum = runtime.priv_sum;
    tr.io_ooo_to_mem_tlbCsr_priv_vmxr = runtime.priv_vmxr;
    tr.io_ooo_to_mem_tlbCsr_priv_vsum = runtime.priv_vsum;
    tr.io_ooo_to_mem_tlbCsr_priv_virt = runtime.priv_virt;
    tr.io_ooo_to_mem_tlbCsr_priv_virt_changed = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_priv_spvp = runtime.priv_spvp;
    tr.io_ooo_to_mem_tlbCsr_priv_imode = runtime.priv_imode;
    tr.io_ooo_to_mem_tlbCsr_priv_dmode = runtime.priv_dmode;
    tr.io_ooo_to_mem_tlbCsr_mPBMTE = runtime.m_pbmt_en;
    tr.io_ooo_to_mem_tlbCsr_hPBMTE = runtime.h_pbmt_en;
    tr.io_ooo_to_mem_csrCtrl_hd_misalign_ld_enable = runtime.hd_misalign_ld_enable;
    tr.io_ooo_to_mem_csrCtrl_hd_misalign_st_enable = runtime.hd_misalign_st_enable;
    tr.io_ooo_to_mem_tlbCsr_priv_debug = runtime.priv_debug;
    tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid = 1'b0;
endfunction:initialize_csr_xaction_from_runtime

// 抽象职责：将已经配置的 action 交给 CSR driver，并只在 finish_item() 返回后登记
// sendover。完成证据仍必须由 control service 之后读取的 runtime monitor snapshot 提供。
task memblock_csr_control_base_sequence::drive_csr_control_xaction(
    input memblock_csr_control_action_t action,
    input csr_ctrl_agent_agent_xaction tr
);
    memblock_sync_pkg::dispatch_raw_csr_t ignored_runtime;
    int unsigned runtime_seq_before_drive;

    if (!memblock_sync_pkg::get_latest_runtime_csr_snapshot(ignored_runtime,
                                                             runtime_seq_before_drive)) begin
        `uvm_fatal(get_type_name(), "CSR action lost runtime snapshot before drive")
    end
    action.runtime_snapshot_seq_before_drive = runtime_seq_before_drive;
    start_item(tr);
    finish_item(tr);
    data.mark_csr_control_sendover(action);
endtask:drive_csr_control_xaction

`endif
