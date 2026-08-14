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
    extern virtual task configure_l2_flush_assert_xaction(
        input memblock_csr_control_action_t action,
        output csr_ctrl_agent_agent_xaction tr
    );
    extern virtual task configure_l2_flush_release_xaction(
        input memblock_l2_flush_release_request_t request,
        output csr_ctrl_agent_agent_xaction tr
    );
    extern virtual task drive_l2_flush_assert_xaction(
        input memblock_csr_control_action_t action,
        input csr_ctrl_agent_agent_xaction tr
    );
    extern virtual task drive_l2_flush_release_xaction(
        input memblock_l2_flush_release_request_t request,
        input csr_ctrl_agent_agent_xaction tr
    );
    extern virtual function void initialize_csr_xaction_from_runtime(
        input memblock_sync_pkg::dispatch_raw_csr_t runtime,
        input csr_ctrl_agent_agent_xaction tr
    );
    extern virtual function void initialize_l2_flush_control_metadata(
        input memblock_control_owner_t owner,
        input int unsigned control_reset_epoch,
        input int unsigned action_kind,
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
    memblock_l2_flush_release_request_t release_request;
    memblock_control_owner_t l2_flush_hold_owner;
    csr_ctrl_agent_agent_xaction tr;
    bit l2_flush_hold_active;
    int unsigned l2_flush_hold_control_reset_epoch;

    seq_csr_common::init();
    data = common_data_transaction::get();
    if (data == null) begin
        `uvm_fatal(get_type_name(), "failed to get common_data_transaction")
    end
    if (!memblock_sync_pkg::uses_control_barrier_topology()) begin
        `uvm_fatal(get_type_name(), "CSR control worker started outside active control topology")
    end
    l2_flush_hold_active = 1'b0;
    l2_flush_hold_owner = '{default:'0};
    l2_flush_hold_control_reset_epoch = 0;

    forever begin
        // 中文注释：ASSERT 到 RELEASE 间由同一 worker 保留 sequencer 所有权。
        // 这不是重复发送 high item；driver 的私有 hold 负责逐拍维持 high。
        if (l2_flush_hold_active) begin
            if (data.try_pop_l2_flush_release_request(release_request)) begin
                if (!memblock_control_owner_equal(release_request.owner,
                                                   l2_flush_hold_owner) ||
                    release_request.control_reset_epoch !=
                        l2_flush_hold_control_reset_epoch) begin
                    `uvm_fatal(get_type_name(),
                               "L2 flush RELEASE does not match CSR worker held owner")
                end
                configure_l2_flush_release_xaction(release_request, tr);
                drive_l2_flush_release_xaction(release_request, tr);
                l2_flush_hold_active = 1'b0;
                l2_flush_hold_owner = '{default:'0};
                l2_flush_hold_control_reset_epoch = 0;
                continue;
            end
            if (data.try_pop_csr_control_action(action)) begin
                `uvm_fatal(get_type_name(),
                           "CSR action arrived while L2 flush high hold is awaiting RELEASE")
            end
            if (data.control_worker_can_exit(1'b1)) begin
                `uvm_fatal(get_type_name(),
                           "CSR worker shutdown was requested while L2 flush hold is active")
            end
            wait_for_csr_work_or_shutdown();
            continue;
        end
        if (data.try_pop_l2_flush_release_request(release_request)) begin
            `uvm_fatal(get_type_name(),
                       "CSR worker received L2 flush RELEASE without an active ASSERT hold")
        end
        if (data.try_pop_csr_control_action(action)) begin
            case (action.completion_profile)
                MEMBLOCK_CONTROL_COMPLETION_RUNTIME_CSR_SNAPSHOT: begin
                    if (action.l2_flush_phase != MEMBLOCK_L2_FLUSH_PHASE_NONE) begin
                        `uvm_fatal(get_type_name(),
                                   "ordinary CSR action carries an unexpected L2 flush phase")
                    end
                    configure_csr_control_xaction(action, tr);
                    drive_csr_control_xaction(action, tr);
                end
                MEMBLOCK_CONTROL_COMPLETION_L2_FLUSH_LEVEL: begin
                    if (action.l2_flush_phase != MEMBLOCK_L2_FLUSH_PHASE_ASSERT) begin
                        `uvm_fatal(get_type_name(),
                                   "CSR action queue only accepts L2 flush ASSERT tokens")
                    end
                    configure_l2_flush_assert_xaction(action, tr);
                    drive_l2_flush_assert_xaction(action, tr);
                    l2_flush_hold_active = 1'b1;
                    l2_flush_hold_owner = action.owner;
                    l2_flush_hold_control_reset_epoch = action.control_reset_epoch;
                end
                default:
                    `uvm_fatal(get_type_name(),
                               $sformatf("CSR worker got unsupported completion profile=%0d uid=%0d",
                                         action.completion_profile, action.owner.uid))
            endcase
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
            MEMBLOCK_CONTROL_COMPLETION_RUNTIME_CSR_SNAPSHOT ||
        action.l2_flush_phase != MEMBLOCK_L2_FLUSH_PHASE_NONE) begin
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
    tr.control_l2_flush_metadata_valid = 1'b0;
    tr.control_l2_flush_baseline_valid = 1'b0;
    tr.control_l2_flush_action_kind =
        csr_ctrl_agent_agent_xaction::CONTROL_L2_FLUSH_ACTION_NONE;
    tr.control_l2_flush_owner_uid = 0;
    tr.control_l2_flush_owner_dynamic_epoch = 0;
    tr.control_l2_flush_owner_action_generation = 0;
    tr.control_l2_flush_owner_kind_code = 0;
    tr.control_l2_flush_control_reset_epoch = 0;
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

// 抽象职责：把 seq 层 owner 转成 CSR agent 可见的 primitive metadata。agent 不解释
// control kind，只在 driver 内逐字段比较，从而避免 agent package 依赖 seq typedef。
function void memblock_csr_control_base_sequence::initialize_l2_flush_control_metadata(
    input memblock_control_owner_t owner,
    input int unsigned control_reset_epoch,
    input int unsigned action_kind,
    input csr_ctrl_agent_agent_xaction tr
);
    if (!owner.valid || control_reset_epoch == 0 || tr == null ||
        !(action_kind inside {
            csr_ctrl_agent_agent_xaction::CONTROL_L2_FLUSH_ACTION_ASSERT,
            csr_ctrl_agent_agent_xaction::CONTROL_L2_FLUSH_ACTION_RELEASE})) begin
        `uvm_fatal(get_type_name(), "invalid L2 flush CSR metadata initialization")
    end
    tr.control_l2_flush_metadata_valid = 1'b1;
    tr.control_l2_flush_baseline_valid = 1'b1;
    tr.control_l2_flush_action_kind = action_kind;
    tr.control_l2_flush_owner_uid = owner.uid;
    tr.control_l2_flush_owner_dynamic_epoch = owner.dynamic_epoch;
    tr.control_l2_flush_owner_action_generation = owner.action_generation;
    tr.control_l2_flush_owner_kind_code = owner.kind;
    tr.control_l2_flush_control_reset_epoch = control_reset_epoch;
endfunction:initialize_l2_flush_control_metadata

// 抽象职责：从 check_store 冻结的 CSR baseline 创建一次 high ASSERT item。该函数只
// 填 xaction；driver 建立 hold、service 等待 done-high 均不在这里执行。
task memblock_csr_control_base_sequence::configure_l2_flush_assert_xaction(
    input memblock_csr_control_action_t action,
    output csr_ctrl_agent_agent_xaction tr
);
    if (!action.owner.valid || !action.csr_baseline_valid ||
        action.completion_profile != MEMBLOCK_CONTROL_COMPLETION_L2_FLUSH_LEVEL ||
        action.l2_flush_phase != MEMBLOCK_L2_FLUSH_PHASE_ASSERT ||
        action.control_reset_epoch == 0) begin
        `uvm_fatal(get_type_name(), "invalid L2 flush ASSERT action")
    end
    tr = csr_ctrl_agent_agent_xaction::type_id::create(
        $sformatf("l2_flush_assert_uid_%0d_gen_%0d", action.owner.uid,
                  action.owner.action_generation));
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "failed to create L2 flush ASSERT xaction")
    end
    initialize_csr_xaction_from_runtime(action.csr_baseline, tr);
    // L2 level hold 不得持续重驱 runtime snapshot 中的一次性 CSR pulse。
    tr.io_ooo_to_mem_tlbCsr_satp_changed = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_vsatp_changed = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_hgatp_changed = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_priv_virt_changed = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid = 1'b0;
    tr.pre_pkt_gap = 0;
    tr.post_pkt_gap = 0;
    tr.io_ooo_to_mem_csrCtrl_flush_l2_enable = 1'b1;
    initialize_l2_flush_control_metadata(
        action.owner, action.control_reset_epoch,
        csr_ctrl_agent_agent_xaction::CONTROL_L2_FLUSH_ACTION_ASSERT, tr);
endtask:configure_l2_flush_assert_xaction

// 抽象职责：为同一 check_store owner 构造一次 low RELEASE item。它只读取 status 中
// ASSERT 时冻结的 CSR baseline，不能读取随后变化的 global runtime snapshot。
task memblock_csr_control_base_sequence::configure_l2_flush_release_xaction(
    input memblock_l2_flush_release_request_t request,
    output csr_ctrl_agent_agent_xaction tr
);
    status_transaction status;

    if (!request.owner.valid || request.control_reset_epoch == 0) begin
        `uvm_fatal(get_type_name(), "invalid L2 flush RELEASE request")
    end
    status = data.get_status(request.owner.uid);
    if (!memblock_control_owner_equal(status.control_owner, request.owner) ||
        status.control_state != MEMBLOCK_CONTROL_STATE_CHECK_STORE_L2_CSR_RELEASE ||
        !status.control_l2_csr_baseline_valid ||
        status.control_reset_epoch != request.control_reset_epoch) begin
        `uvm_fatal(get_type_name(), "L2 flush RELEASE status/owner/baseline mismatch")
    end
    tr = csr_ctrl_agent_agent_xaction::type_id::create(
        $sformatf("l2_flush_release_uid_%0d_gen_%0d", request.owner.uid,
                  request.owner.action_generation));
    if (tr == null) begin
        `uvm_fatal(get_type_name(), "failed to create L2 flush RELEASE xaction")
    end
    initialize_csr_xaction_from_runtime(status.control_l2_csr_baseline, tr);
    tr.io_ooo_to_mem_tlbCsr_satp_changed = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_vsatp_changed = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_hgatp_changed = 1'b0;
    tr.io_ooo_to_mem_tlbCsr_priv_virt_changed = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_distribute_csr_w_valid = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_frontend_trigger_tUpdate_valid = 1'b0;
    tr.io_ooo_to_mem_csrCtrl_mem_trigger_tUpdate_valid = 1'b0;
    tr.pre_pkt_gap = 0;
    tr.post_pkt_gap = 0;
    tr.io_ooo_to_mem_csrCtrl_flush_l2_enable = 1'b0;
    initialize_l2_flush_control_metadata(
        request.owner, request.control_reset_epoch,
        csr_ctrl_agent_agent_xaction::CONTROL_L2_FLUSH_ACTION_RELEASE, tr);
endtask:configure_l2_flush_release_xaction

// 抽象职责：在 ASSERT driver item 完成后仅登记 sendover 与最新 done 序号下界。
// driver 已在同一 item 边界建立 hold；此处不把 sendover 当作 done-high。
task memblock_csr_control_base_sequence::drive_l2_flush_assert_xaction(
    input memblock_csr_control_action_t action,
    input csr_ctrl_agent_agent_xaction tr
);
    start_item(tr);
    finish_item(tr);
    data.mark_l2_flush_assert_sendover(action);
endtask:drive_l2_flush_assert_xaction

// 抽象职责：交付唯一的 RELEASE item 并记录 low completion 的新鲜 observation 下界。
// driver 在 item 边界按 owner 清 hold；service 仍必须等待 monitor 观察到 done-low。
task memblock_csr_control_base_sequence::drive_l2_flush_release_xaction(
    input memblock_l2_flush_release_request_t request,
    input csr_ctrl_agent_agent_xaction tr
);
    start_item(tr);
    finish_item(tr);
    data.mark_l2_flush_release_sendover(request);
endtask:drive_l2_flush_release_xaction

`endif
