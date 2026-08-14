//=========================================================
//File name    : memblock_control_barrier_service.sv
//Author       : OpenAI_Codex
//Module name  : memblock_control_barrier_service
//Discribution : control ROB barrier runtime service
//Date         : 2026-08-14
//=========================================================
`ifndef MEMBLOCK_CONTROL_BARRIER_SERVICE__SV
`define MEMBLOCK_CONTROL_BARRIER_SERVICE__SV

// 中文注释：控制 service 是控制标记状态表的唯一运行期推进者。它读取已经由
// monitor/worker 固化的事实，创建 owner 化 token，并在 terminal_done 后释放 admission
// barrier；它不直接驱动 CSR/Fence 接口，也不参与普通 LSQ/issue 路由。
class memblock_control_barrier_service extends uvm_object;

    common_data_transaction data;
    lsq_commit_handler      commit_handler;

    // 中文注释：bootstrap epoch 只属于本 service 的 control runtime 代际。建表完成时
    // 冻结三类 monitor observation 的下界；后续 sample 均越过下界后才允许控制 UID
    // 创建动作，避免把建表前的 latest 误归属给新表。
    bit               bootstrap_started;
    bit               runtime_ready;
    bit               reset_in_progress;
    int unsigned      control_runtime_epoch;
    int unsigned      bootstrap_csr_snapshot_seq;
    longint unsigned  bootstrap_sb_observation_seq;
    longint unsigned  bootstrap_l2_done_observation_seq;

    `uvm_object_utils(memblock_control_barrier_service)

    extern function new(string name = "memblock_control_barrier_service");
    extern virtual function void ensure_handles();
    extern virtual function void initialize_control_runtime_bootstrap();
    extern virtual function void begin_control_runtime_reset(input string reason);
    extern virtual function bit control_runtime_is_ready();
    extern virtual function void service_once();
    extern virtual function void service_active_control_barrier();
    extern virtual function void bind_control_owner(input status_transaction status);
    extern virtual function void enqueue_csr_action(input status_transaction status);
    extern virtual function void complete_csr_runtime_snapshot(input status_transaction status);
    extern virtual function bit csr_payload_equal(
        input memblock_sync_pkg::dispatch_raw_csr_t expected,
        input memblock_sync_pkg::dispatch_raw_csr_t observed
    );

endclass:memblock_control_barrier_service

function memblock_control_barrier_service::new(
    string name = "memblock_control_barrier_service"
);
    super.new(name);
    data = null;
    commit_handler = null;
    bootstrap_started = 1'b0;
    runtime_ready = 1'b0;
    reset_in_progress = 1'b0;
    control_runtime_epoch = 0;
    bootstrap_csr_snapshot_seq = 0;
    bootstrap_sb_observation_seq = 0;
    bootstrap_l2_done_observation_seq = 0;
endfunction:new

function void memblock_control_barrier_service::ensure_handles();
    if (data == null) begin
        data = common_data_transaction::get();
    end
    if (commit_handler == null) begin
        commit_handler = lsq_commit_handler::get();
    end
    if (data == null || commit_handler == null) begin
        `uvm_fatal(get_type_name(), "control barrier service failed to obtain shared handles")
    end
endfunction:ensure_handles

// 抽象职责：在 active control main table 完成后建立首个控制运行期代际。该函数
// 只冻结 monitor 事实的起始序号并清空未交付 action；不修改主表、ROB 或 status。
function void memblock_control_barrier_service::initialize_control_runtime_bootstrap();
    memblock_sync_pkg::dispatch_raw_csr_t ignored_csr;
    memblock_sync_pkg::memblock_control_level_observation_t observation;
    int unsigned csr_seq;

    ensure_handles();
    if (!memblock_sync_pkg::uses_control_barrier_topology()) begin
        return;
    end
    if (bootstrap_started) begin
        return;
    end
    control_runtime_epoch++;
    if (control_runtime_epoch == 0) begin
        `uvm_fatal(get_type_name(), "control runtime epoch wrapped during bootstrap")
    end
    void'(memblock_sync_pkg::get_latest_runtime_csr_snapshot(ignored_csr, csr_seq));
    bootstrap_csr_snapshot_seq = csr_seq;
    void'(memblock_sync_pkg::get_latest_control_sb_is_empty_observation(observation));
    bootstrap_sb_observation_seq = observation.observation_seq;
    void'(memblock_sync_pkg::get_latest_control_l2_flush_done_observation(observation));
    bootstrap_l2_done_observation_seq = observation.observation_seq;
    data.reset_control_action_runtime();
    bootstrap_started = 1'b1;
    runtime_ready = 1'b0;
    reset_in_progress = 1'b0;
endfunction:initialize_control_runtime_bootstrap

// 抽象职责：在物理 reset 返回前清除尚未创建 action 的控制运行期事实。当前控制
// 标记一旦已经 admission 即不能被普通 reset/redirect 静默重建，直接 fatal 保留
// 主表与 ROB 所有权不变量；未 admission 的启动窗口可安全建立新 epoch。
function void memblock_control_barrier_service::begin_control_runtime_reset(
    input string reason
);
    memblock_sync_pkg::dispatch_raw_csr_t ignored_csr;
    memblock_sync_pkg::memblock_control_level_observation_t observation;
    int unsigned csr_seq;

    ensure_handles();
    if (!memblock_sync_pkg::uses_control_barrier_topology() || !bootstrap_started) begin
        return;
    end
    if (reset_in_progress) begin
        return;
    end
    if (data.active_control_barrier_valid ||
        data.dispatch_progress.max_enqueued_uid_valid) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("control runtime reset after admission is unsupported: reason=%0s barrier=%0d uid=%0d max_enq_valid=%0d",
                             reason, data.active_control_barrier_valid,
                             data.active_control_barrier_uid,
                             data.dispatch_progress.max_enqueued_uid_valid))
    end
    control_runtime_epoch++;
    if (control_runtime_epoch == 0) begin
        `uvm_fatal(get_type_name(), "control runtime epoch wrapped during reset")
    end
    void'(memblock_sync_pkg::get_latest_runtime_csr_snapshot(ignored_csr, csr_seq));
    bootstrap_csr_snapshot_seq = csr_seq;
    void'(memblock_sync_pkg::get_latest_control_sb_is_empty_observation(observation));
    bootstrap_sb_observation_seq = observation.observation_seq;
    void'(memblock_sync_pkg::get_latest_control_l2_flush_done_observation(observation));
    bootstrap_l2_done_observation_seq = observation.observation_seq;
    data.reset_control_action_runtime();
    runtime_ready = 1'b0;
    reset_in_progress = 1'b1;
endfunction:begin_control_runtime_reset

// 抽象职责：确认三个既有 monitor 都已经在 bootstrap/reset 后发布至少一个新事实。
// 该 ready 只保护控制动作的观察边界，不把 raw queue、worker sendover 或控制完成
// 误当作 producer 初始化完成。
function bit memblock_control_barrier_service::control_runtime_is_ready();
    memblock_sync_pkg::dispatch_raw_csr_t csr_snapshot;
    memblock_sync_pkg::memblock_control_level_observation_t sb_observation;
    memblock_sync_pkg::memblock_control_level_observation_t l2_done_observation;
    int unsigned csr_seq;

    if (runtime_ready) begin
        return 1'b1;
    end
    if (!bootstrap_started ||
        !memblock_sync_pkg::get_latest_runtime_csr_snapshot(csr_snapshot, csr_seq) ||
        !memblock_sync_pkg::get_latest_control_sb_is_empty_observation(sb_observation) ||
        !memblock_sync_pkg::get_latest_control_l2_flush_done_observation(l2_done_observation)) begin
        return 1'b0;
    end
    // runtime CSR seq 只在 payload 改变时递增；bootstrap 时已经冻结的有效 latest
    // 本身可作为首个 CSR action baseline，不能强行等待一次无语义的新变化。
    if (csr_seq < bootstrap_csr_snapshot_seq ||
        sb_observation.observation_seq <= bootstrap_sb_observation_seq ||
        l2_done_observation.observation_seq <= bootstrap_l2_done_observation_seq) begin
        return 1'b0;
    end
    runtime_ready = 1'b1;
    reset_in_progress = 1'b0;
    return 1'b1;
endfunction:control_runtime_is_ready

// 抽象职责：在每个 dispatch service tick 推进当前唯一控制 barrier。终态控制 UID
// 先释放 barrier；非终态 UID 仅按自身 control_state 消费 monitor/worker 已完成事实。
function void memblock_control_barrier_service::service_once();
    ensure_handles();
    if (!memblock_sync_pkg::uses_control_barrier_topology() ||
        memblock_sync_pkg::reset_backend_done !== 1'b1) begin
        return;
    end
    if (!bootstrap_started) begin
        `uvm_fatal(get_type_name(), "active control topology reached service before bootstrap")
    end
    if (!control_runtime_is_ready() || !data.active_control_barrier_valid) begin
        return;
    end
    service_active_control_barrier();
endfunction:service_once

// 抽象职责：处理 active_control_barrier_uid 的静态等待、CSR token 与 CSR monitor
// 完成分支。SFence/check_store 的 owner 和 flushSb/L2 事实将由后续分支复用同一入口；
// 本函数不扫描整张主表，始终只读取当前 barrier 的 status。
function void memblock_control_barrier_service::service_active_control_barrier();
    memblock_uid_t uid;
    status_transaction status;

    uid = data.active_control_barrier_uid;
    status = data.get_status(uid);
    if (!status.active && status.terminal_done) begin
        data.release_control_barrier_after_terminal(uid);
        return;
    end
    if (!status.active || !status.enq || status.terminal_done ||
        !data.uid_is_control_marker(uid)) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("invalid active control barrier uid=%0d active/enq/done=%0d/%0d/%0d",
                             uid, status.active, status.enq, status.terminal_done))
    end

    case (status.control_state)
        MEMBLOCK_CONTROL_STATE_WAIT_OLDER_ROB_COMMIT: begin
            if (data.active_redirect.valid) begin
                return;
            end
            if (commit_handler.commit_cursor_uid != uid) begin
                return;
            end
            bind_control_owner(status);
            case (status.control_kind)
                MEMBLOCK_CONTROL_KIND_CSR:
                    status.control_state = MEMBLOCK_CONTROL_STATE_CSR_CONFIG_PENDING;
                MEMBLOCK_CONTROL_KIND_SFENCE:
                    status.control_state = MEMBLOCK_CONTROL_STATE_WAIT_FLUSHSB_REQ;
                MEMBLOCK_CONTROL_KIND_CHECK_STORE:
                    status.control_state = MEMBLOCK_CONTROL_STATE_CHECK_STORE_FLUSHSB_PENDING;
                default:
                    `uvm_fatal(get_type_name(),
                               $sformatf("control uid=%0d has unsupported kind=%0d",
                                         uid, status.control_kind))
            endcase
            status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
        end
        MEMBLOCK_CONTROL_STATE_CSR_CONFIG_PENDING: begin
            if (!status.control_action_enqueued) begin
                enqueue_csr_action(status);
            end
        end
        MEMBLOCK_CONTROL_STATE_CSR_SENDOVER: begin
            status.control_state = MEMBLOCK_CONTROL_STATE_WAIT_CSR_RUNTIME_SNAPSHOT;
            status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
        end
        MEMBLOCK_CONTROL_STATE_WAIT_CSR_RUNTIME_SNAPSHOT:
            complete_csr_runtime_snapshot(status);
        default: begin
            // SFence/check_store action states are intentionally left for their
            // dedicated completion flows; no ordinary issue/commit path may
            // advance a control marker while this service owns the barrier.
        end
    endcase
endfunction:service_active_control_barrier

// 抽象职责：在 commit cursor 到达控制 UID 时绑定其唯一 owner。owner 的 generation
// 由 status 单点递增，后续 worker token、flushSb completion 和 monitor record 都以它
// 匹配；本函数不创建任何 action。
function void memblock_control_barrier_service::bind_control_owner(
    input status_transaction status
);
    if (status.control_owner.valid || status.control_action_enqueued ||
        status.control_kind == MEMBLOCK_CONTROL_KIND_NONE) begin
        `uvm_fatal(get_type_name(), "cannot bind an already-owned or invalid control status")
    end
    status.control_action_generation++;
    if (status.control_action_generation == 0) begin
        `uvm_fatal(get_type_name(), "control action generation wrapped")
    end
    status.control_owner.valid = 1'b1;
    status.control_owner.uid = status.uid;
    status.control_owner.dynamic_epoch = status.dynamic_epoch;
    status.control_owner.action_generation = status.control_action_generation;
    status.control_owner.kind = status.control_kind;
    status.control_reset_epoch = control_runtime_epoch;
endfunction:bind_control_owner

// 抽象职责：把当前 runtime CSR snapshot 冻结为 CSR action token 并唤醒专用 worker。
// token 入队前 status 已记录 owner/enqueued 状态；该函数不等待 driver 或 monitor
// completion，避免把 queue/event 当作配置完成事实。
function void memblock_control_barrier_service::enqueue_csr_action(
    input status_transaction status
);
    memblock_csr_control_action_t action;
    memblock_sync_pkg::dispatch_raw_csr_t snapshot;
    int unsigned snapshot_seq;

    if (!status.control_owner.valid ||
        status.control_state != MEMBLOCK_CONTROL_STATE_CSR_CONFIG_PENDING ||
        status.control_action_enqueued ||
        !memblock_sync_pkg::get_latest_runtime_csr_snapshot(snapshot, snapshot_seq)) begin
        `uvm_fatal(get_type_name(), "CSR action enqueue has invalid status or no runtime snapshot")
    end
    action = '{default:'0};
    action.owner = status.control_owner;
    action.completion_profile = MEMBLOCK_CONTROL_COMPLETION_RUNTIME_CSR_SNAPSHOT;
    action.csr_baseline_valid = 1'b1;
    action.csr_baseline = snapshot;
    action.csr_baseline_snapshot_seq = snapshot_seq;
    action.control_reset_epoch = status.control_reset_epoch;
    status.control_action_enqueued = 1'b1;
    status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
    data.enqueue_csr_control_action(action);
endfunction:enqueue_csr_action

// 抽象职责：只用 monitor 已观察到的 runtime CSR snapshot 完成 CSR 标记。已发送
// xaction 只提供 expected payload 和 drive 前序号；本函数归档 observed snapshot 后
// 打开 control commit，不直接修改 ROB cursor。
function void memblock_control_barrier_service::complete_csr_runtime_snapshot(
    input status_transaction status
);
    memblock_sync_pkg::dispatch_raw_csr_t snapshot;
    int unsigned snapshot_seq;

    if (!status.control_owner.valid || !status.control_expected_runtime_csr_valid ||
        !memblock_sync_pkg::get_latest_runtime_csr_snapshot(snapshot, snapshot_seq)) begin
        return;
    end
    if (snapshot_seq <= status.control_runtime_snapshot_seq_before_drive ||
        !csr_payload_equal(status.control_expected_runtime_csr, snapshot)) begin
        return;
    end
    status.control_runtime_csr_snapshot_valid = 1'b1;
    status.control_runtime_csr_snapshot = snapshot;
    status.control_runtime_csr_snapshot_seq = snapshot_seq;
    status.control_state = MEMBLOCK_CONTROL_STATE_CONTROL_COMMIT_READY;
    status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
endfunction:complete_csr_runtime_snapshot

// 抽象职责：比较 CSR monitor 的完整 runtime payload，忽略 valid/cycle 这两个
// transport 字段。复用已有 raw_csr_payload_changed() 的字段覆盖，避免维护第二份
// 手写比较字段表；两个 payload 无变化即表示当前控制配置已被 monitor 观察到。
function bit memblock_control_barrier_service::csr_payload_equal(
    input memblock_sync_pkg::dispatch_raw_csr_t expected,
    input memblock_sync_pkg::dispatch_raw_csr_t observed
);
    memblock_sync_pkg::dispatch_raw_csr_t normalized_expected;
    memblock_sync_pkg::dispatch_raw_csr_t normalized_observed;

    normalized_expected = expected;
    normalized_observed = observed;
    normalized_expected.valid = 1'b1;
    normalized_observed.valid = 1'b1;
    normalized_expected.cycle = 0;
    normalized_observed.cycle = 0;
    return !memblock_sync_pkg::raw_csr_payload_changed(
        normalized_expected, normalized_observed);
endfunction:csr_payload_equal

`endif
