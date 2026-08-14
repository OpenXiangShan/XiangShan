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

    // 中文注释：bootstrap_started 只记录控制主表已经完成 post-build hook。实际 epoch、
    // reset request、四路 ack、runtime ready 与 CSR baseline 全部由 sync_pkg 单点维护；
    // service 不再私有保存 monitor observation 下界，避免不同 consumer 使用不同代际。
    bit               bootstrap_started;
    bit               reset_in_progress;

    `uvm_object_utils(memblock_control_barrier_service)

    extern function new(string name = "memblock_control_barrier_service");
    extern virtual function void ensure_handles();
    extern virtual function void initialize_control_runtime_bootstrap();
    extern virtual function void begin_control_runtime_reset(input string reason);
    extern virtual function bit control_runtime_is_ready();
    extern virtual function void service_once();
    extern virtual function void service_control_worker_shutdown();
    extern virtual function void service_active_control_barrier();
    extern virtual function void bind_control_owner(input status_transaction status);
    extern virtual function void enqueue_csr_action(input status_transaction status);
    extern virtual function void complete_csr_runtime_snapshot(input status_transaction status);
    extern virtual function void service_sfence_control(input status_transaction status);
    extern virtual function void service_check_store_control(input status_transaction status);
    extern virtual function void enqueue_control_flushsb_request(input status_transaction status);
    extern virtual function void enqueue_sfence_action(input status_transaction status);
    extern virtual function void enqueue_check_store_l2_assert(input status_transaction status);
    extern virtual function void enqueue_check_store_l2_release(input status_transaction status);
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
    reset_in_progress = 1'b0;
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

// 抽象职责：在 active control main table 完成后发起首个 control reset handshake。
// 它只清空未交付 action 并请求当前 epoch 的 driver/monitor ack；不修改主表、ROB 或 status。
function void memblock_control_barrier_service::initialize_control_runtime_bootstrap();
    ensure_handles();
    if (!memblock_sync_pkg::uses_control_barrier_topology()) begin
        return;
    end
    if (bootstrap_started) begin
        return;
    end
    data.reset_control_action_runtime();
    memblock_sync_pkg::request_control_runtime_reset("control bootstrap");
    bootstrap_started = 1'b1;
    reset_in_progress = 1'b0;
endfunction:initialize_control_runtime_bootstrap

// 抽象职责：在物理 reset 返回前清除尚未创建 action 的控制运行期事实。当前控制
// 标记一旦已经 admission 即不能被普通 reset/redirect 静默重建，直接 fatal 保留
// 主表与 ROB 所有权不变量；未 admission 的启动窗口可安全建立新 epoch。
function void memblock_control_barrier_service::begin_control_runtime_reset(
    input string reason
);
    ensure_handles();
    if (!memblock_sync_pkg::uses_control_barrier_topology() || !bootstrap_started) begin
        return;
    end
    if (reset_in_progress) begin
        return;
    end
    if (data.active_control_barrier_valid ||
        data.dispatch_progress.max_enqueued_uid_valid ||
        data.uid_by_active_rob.num() != 0) begin
        `uvm_fatal(get_type_name(),
                   $sformatf("control runtime reset after admission is unsupported: reason=%0s barrier=%0d uid=%0d max_enq_valid=%0d active_rob=%0d",
                             reason, data.active_control_barrier_valid,
                             data.active_control_barrier_uid,
                             data.dispatch_progress.max_enqueued_uid_valid,
                             data.uid_by_active_rob.num()))
    end
    data.reset_control_action_runtime();
    memblock_sync_pkg::request_control_runtime_reset(reason);
    reset_in_progress = 1'b1;
endfunction:begin_control_runtime_reset

// 抽象职责：确认 sync_pkg 的四路 reset ack 已完成，且 CSR monitor 已发布当前 epoch
// 的首份 runtime baseline。ready 只保护控制动作的观察边界，不把 queue 或 sendover
// 误当作 producer 初始化完成。
function bit memblock_control_barrier_service::control_runtime_is_ready();
    memblock_sync_pkg::memblock_control_csr_runtime_baseline_t baseline;

    if (!bootstrap_started ||
        !memblock_sync_pkg::control_runtime_ready_for_current_epoch() ||
        !memblock_sync_pkg::get_control_csr_runtime_baseline(baseline)) begin
        return 1'b0;
    end
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

// 抽象职责：在所有 UID 已 terminal_done 且 control action drain 完成后，先请求两个
// 显式 worker 退出。它不设置 global stop；global stop 仍由 common drain 谓词在
// 两条 exited acknowledgement 都到达后统一决定，从而切断 stop/worker 的循环依赖。
function void memblock_control_barrier_service::service_control_worker_shutdown();
    ensure_handles();
    if (!memblock_sync_pkg::uses_control_barrier_topology()) begin
        return;
    end
    if (!bootstrap_started) begin
        `uvm_fatal(get_type_name(), "active control topology reached worker shutdown before bootstrap")
    end
    if (!data.transaction_done() || !data.control_action_drain_complete()) begin
        return;
    end
    data.request_control_worker_shutdown();
endfunction:service_control_worker_shutdown

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
        MEMBLOCK_CONTROL_STATE_WAIT_FLUSHSB_REQ,
        MEMBLOCK_CONTROL_STATE_WAIT_SB_EMPTY,
        MEMBLOCK_CONTROL_STATE_SFENCE_REQ,
        MEMBLOCK_CONTROL_STATE_SFENCE_SENDOVER,
        MEMBLOCK_CONTROL_STATE_WAIT_L2TLB_FLUSH_EFFECTIVE:
            service_sfence_control(status);
        MEMBLOCK_CONTROL_STATE_CHECK_STORE_FLUSHSB_PENDING,
        MEMBLOCK_CONTROL_STATE_CHECK_STORE_WAIT_SB_EMPTY,
        MEMBLOCK_CONTROL_STATE_CHECK_STORE_L2_CSR_ASSERT,
        MEMBLOCK_CONTROL_STATE_WAIT_L2_FLUSH_DONE,
        MEMBLOCK_CONTROL_STATE_CHECK_STORE_L2_CSR_RELEASE,
        MEMBLOCK_CONTROL_STATE_WAIT_L2_FLUSH_IDLE:
            service_check_store_control(status);
        default: begin
            // CONTROL_COMMIT_READY 交给已有 control commit/retire 分支；其它状态
            // 不得由普通 issue/commit 路径推进当前 barrier。
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
    status.control_reset_epoch = memblock_sync_pkg::get_control_runtime_reset_epoch();
endfunction:bind_control_owner

// 抽象职责：把当前 runtime CSR snapshot 冻结为 CSR action token 并唤醒专用 worker。
// token 入队前 status 已记录 owner/enqueued 状态；该函数不等待 driver 或 monitor
// completion，避免把 queue/event 当作配置完成事实。
function void memblock_control_barrier_service::enqueue_csr_action(
    input status_transaction status
);
    memblock_csr_control_action_t action;
    memblock_sync_pkg::dispatch_raw_csr_t snapshot;
    memblock_sync_pkg::memblock_control_csr_runtime_baseline_t baseline;
    int unsigned snapshot_seq;

    if (!status.control_owner.valid ||
        status.control_state != MEMBLOCK_CONTROL_STATE_CSR_CONFIG_PENDING ||
        status.control_action_enqueued) begin
        `uvm_fatal(get_type_name(), "CSR action enqueue has invalid status")
    end
    if (!memblock_sync_pkg::get_control_csr_runtime_baseline(baseline) ||
        !memblock_sync_pkg::get_latest_runtime_csr_snapshot(snapshot, snapshot_seq)) begin
        return;
    end
    if (status.control_reset_epoch != baseline.reset_epoch ||
        snapshot_seq < baseline.first_snapshot_seq) begin
        `uvm_fatal(get_type_name(), "CSR action attempted to use a non-current runtime baseline")
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

// 抽象职责：推进 SFence 的 owner flushSb、worker action、C0 和 C4 completion。
// 它只读取 request/adapter observation，并由持久 queue 唤醒 worker；不直接驱动
// fence interface，也不依据固定等待拍数判断完成。
function void memblock_control_barrier_service::service_sfence_control(
    input status_transaction status
);
    memblock_flushsb_completion_t completion;
    memblock_control_sfence_observation_t observation;

    case (status.control_state)
        MEMBLOCK_CONTROL_STATE_WAIT_FLUSHSB_REQ: begin
            if (!status.control_flushsb_request_queued) begin
                enqueue_control_flushsb_request(status);
            end
            if (status.control_flushsb_request_queued &&
                data.control_flushsb_sendover_seen(status.control_owner,
                                                    status.control_flushsb_req_id)) begin
                status.control_state = MEMBLOCK_CONTROL_STATE_WAIT_SB_EMPTY;
                status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
            end
        end
        MEMBLOCK_CONTROL_STATE_WAIT_SB_EMPTY: begin
            if (data.try_consume_control_flushsb_completion(
                    status.control_owner, status.control_flushsb_req_id, completion)) begin
                status.control_flushsb_request_queued = 1'b0;
                status.control_state = MEMBLOCK_CONTROL_STATE_SFENCE_REQ;
                status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
                enqueue_sfence_action(status);
            end
        end
        MEMBLOCK_CONTROL_STATE_SFENCE_REQ: begin
            if (!status.control_action_enqueued) begin
                enqueue_sfence_action(status);
            end
        end
        MEMBLOCK_CONTROL_STATE_SFENCE_SENDOVER: begin
            if (data.get_control_sfence_c0_observation(status.control_owner,
                                                        observation)) begin
                status.control_sfence_c0_event_seq = observation.lifecycle_event_seq;
                status.control_state = MEMBLOCK_CONTROL_STATE_WAIT_L2TLB_FLUSH_EFFECTIVE;
                status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
            end
        end
        MEMBLOCK_CONTROL_STATE_WAIT_L2TLB_FLUSH_EFFECTIVE: begin
            if (data.try_consume_control_sfence_effective_observation(
                    status.control_owner, status.control_sfence_c0_event_seq,
                    status.control_l2tlb_reset_epoch_at_arm, observation)) begin
                status.control_state = MEMBLOCK_CONTROL_STATE_CONTROL_COMMIT_READY;
                status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
            end
        end
        default: begin
            `uvm_fatal(get_type_name(), "SFence service received an unsupported control state")
        end
    endcase
endfunction:service_sfence_control

// 抽象职责：向现有 flushSb FIFO 写入一个带 control owner 的请求，并将生成的 req_id
// 回填当前 status。LSQ commit sequence 仍是唯一 driver consumer；本函数不等待
// sbIsEmpty，也不会让 event 代替 queue 中的持久请求。
function void memblock_control_barrier_service::enqueue_control_flushsb_request(
    input status_transaction status
);
    memblock_flushsb_req_t request;

    if (!status.control_owner.valid || status.control_flushsb_request_queued ||
        !(status.control_kind inside {MEMBLOCK_CONTROL_KIND_SFENCE,
                                      MEMBLOCK_CONTROL_KIND_CHECK_STORE})) begin
        `uvm_fatal(get_type_name(), "invalid control status while enqueueing flushSb")
    end
    data.push_owner_flushsb_request(status.control_owner, request);
    status.control_flushsb_req_id = request.req_id;
    status.control_flushsb_request_queued = 1'b1;
    status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
endfunction:enqueue_control_flushsb_request

// 抽象职责：在 sbIsEmpty completion 后把 canonical SFence token 放入持久 action
// queue。L2TLB responder/adapter owner 尚未 ready 时保持 SFENCE_REQ 等待；已知
// 拓扑缺失则 fail-fast，避免控制 barrier 无声卡死。
function void memblock_control_barrier_service::enqueue_sfence_action(
    input status_transaction status
);
    memblock_sfence_control_action_t action;
    longint unsigned reset_epoch;

    if (!status.control_owner.valid ||
        status.control_state != MEMBLOCK_CONTROL_STATE_SFENCE_REQ ||
        status.control_action_enqueued) begin
        `uvm_fatal(get_type_name(), "invalid status while enqueueing SFence action")
    end
    if (!memblock_sync_pkg::l2tlb_adapter_service_active) begin
        `uvm_fatal(get_type_name(), "SFence control requires an active L2TLB adapter service")
    end
    if (!memblock_sync_pkg::l2tlb_lifecycle_owner_claimed) begin
        return;
    end
    reset_epoch = memblock_sync_pkg::get_l2tlb_current_reset_epoch();
    if (reset_epoch == 0 ||
        !memblock_sync_pkg::l2tlb_post_reset_baseline_done(reset_epoch)) begin
        return;
    end
    action = '{default:'0};
    action.owner = status.control_owner;
    status.control_action_enqueued = 1'b1;
    status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
    data.enqueue_sfence_control_action(action);
endfunction:enqueue_sfence_action

// 抽象职责：推进 check_store 的 flushSb、L2 ASSERT、done-high、RELEASE 和 done-low
// 状态。它只消费既有 owner flushSb completion 与 ctrl monitor latest done observation；
// CSR driver 的 high hold 完全由 worker/driver 维护，本函数不直接驱动任何接口。
function void memblock_control_barrier_service::service_check_store_control(
    input status_transaction status
);
    memblock_flushsb_completion_t completion;
    memblock_sync_pkg::memblock_control_level_observation_t observation;

    if (!status.control_owner.valid ||
        status.control_owner.kind != MEMBLOCK_CONTROL_KIND_CHECK_STORE ||
        status.control_reset_epoch != memblock_sync_pkg::get_control_runtime_reset_epoch()) begin
        `uvm_fatal(get_type_name(), "check_store control status lost its owner or runtime epoch")
    end
    case (status.control_state)
        MEMBLOCK_CONTROL_STATE_CHECK_STORE_FLUSHSB_PENDING: begin
            if (!status.control_flushsb_request_queued) begin
                enqueue_control_flushsb_request(status);
            end
            if (status.control_flushsb_request_queued &&
                data.control_flushsb_sendover_seen(status.control_owner,
                                                    status.control_flushsb_req_id)) begin
                status.control_state = MEMBLOCK_CONTROL_STATE_CHECK_STORE_WAIT_SB_EMPTY;
                status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
            end
        end
        MEMBLOCK_CONTROL_STATE_CHECK_STORE_WAIT_SB_EMPTY: begin
            if (data.try_consume_control_flushsb_completion(
                    status.control_owner, status.control_flushsb_req_id, completion)) begin
                status.control_flushsb_request_queued = 1'b0;
                status.control_state = MEMBLOCK_CONTROL_STATE_CHECK_STORE_L2_CSR_ASSERT;
                status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
            end
        end
        MEMBLOCK_CONTROL_STATE_CHECK_STORE_L2_CSR_ASSERT: begin
            if (!status.control_action_enqueued) begin
                enqueue_check_store_l2_assert(status);
            end
        end
        MEMBLOCK_CONTROL_STATE_WAIT_L2_FLUSH_DONE: begin
            if (memblock_sync_pkg::get_latest_control_l2_flush_done_observation(observation) &&
                observation.observation_seq > status.control_assert_done_baseline_seq &&
                observation.level) begin
                enqueue_check_store_l2_release(status);
            end
        end
        MEMBLOCK_CONTROL_STATE_CHECK_STORE_L2_CSR_RELEASE: begin
            // CSR worker 在匹配 RELEASE item sendover 后推进到 WAIT_L2_FLUSH_IDLE。
        end
        MEMBLOCK_CONTROL_STATE_WAIT_L2_FLUSH_IDLE: begin
            if (memblock_sync_pkg::get_latest_control_l2_flush_done_observation(observation) &&
                observation.observation_seq > status.control_release_done_baseline_seq &&
                !observation.level) begin
                status.control_state = MEMBLOCK_CONTROL_STATE_CONTROL_COMMIT_READY;
                status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
            end
        end
        default:
            `uvm_fatal(get_type_name(), "check_store service received an unsupported control state")
    endcase
endfunction:service_check_store_control

// 抽象职责：在 sbIsEmpty 后冻结当前 CSR runtime baseline，排队一次 L2 ASSERT。
// 当前 done 必须已为 low；若仍为 high，保持 ASSERT 状态等待，不把旧完成当作新请求完成。
function void memblock_control_barrier_service::enqueue_check_store_l2_assert(
    input status_transaction status
);
    memblock_csr_control_action_t action;
    memblock_sync_pkg::dispatch_raw_csr_t snapshot;
    memblock_sync_pkg::memblock_control_level_observation_t observation;
    memblock_sync_pkg::memblock_control_csr_runtime_baseline_t baseline;
    int unsigned snapshot_seq;

    if (!status.control_owner.valid ||
        status.control_state != MEMBLOCK_CONTROL_STATE_CHECK_STORE_L2_CSR_ASSERT ||
        status.control_action_enqueued ||
        status.control_reset_epoch != memblock_sync_pkg::get_control_runtime_reset_epoch()) begin
        `uvm_fatal(get_type_name(), "invalid check_store L2 ASSERT status")
    end
    if (!memblock_sync_pkg::get_latest_control_l2_flush_done_observation(observation) ||
        observation.level) begin
        return;
    end
    if (!memblock_sync_pkg::get_control_csr_runtime_baseline(baseline) ||
        !memblock_sync_pkg::get_latest_runtime_csr_snapshot(snapshot, snapshot_seq)) begin
        return;
    end
    if (status.control_reset_epoch != baseline.reset_epoch ||
        snapshot_seq < baseline.first_snapshot_seq) begin
        `uvm_fatal(get_type_name(), "check_store L2 ASSERT attempted to use a non-current CSR baseline")
    end
    action = '{default:'0};
    action.owner = status.control_owner;
    action.completion_profile = MEMBLOCK_CONTROL_COMPLETION_L2_FLUSH_LEVEL;
    action.l2_flush_phase = MEMBLOCK_L2_FLUSH_PHASE_ASSERT;
    action.csr_baseline_valid = 1'b1;
    action.csr_baseline = snapshot;
    action.csr_baseline_snapshot_seq = snapshot_seq;
    action.control_reset_epoch = status.control_reset_epoch;
    status.control_l2_csr_baseline_valid = 1'b1;
    status.control_l2_csr_baseline = snapshot;
    status.control_action_enqueued = 1'b1;
    status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
    data.enqueue_csr_control_action(action);
endfunction:enqueue_check_store_l2_assert

// 抽象职责：把当前 owner 的 done-high 转成一个持久 RELEASE 请求。状态先切到
// CHECK_STORE_L2_CSR_RELEASE，再唤醒 CSR worker，确保 worker 永远不会看到旧状态。
function void memblock_control_barrier_service::enqueue_check_store_l2_release(
    input status_transaction status
);
    memblock_l2_flush_release_request_t request;
    memblock_sync_pkg::memblock_control_level_observation_t observation;

    if (!status.control_owner.valid ||
        status.control_state != MEMBLOCK_CONTROL_STATE_WAIT_L2_FLUSH_DONE ||
        !status.control_l2_csr_baseline_valid ||
        status.control_reset_epoch != memblock_sync_pkg::get_control_runtime_reset_epoch() ||
        !memblock_sync_pkg::get_latest_control_l2_flush_done_observation(observation) ||
        !observation.level ||
        observation.observation_seq <= status.control_assert_done_baseline_seq) begin
        `uvm_fatal(get_type_name(), "invalid check_store L2 RELEASE completion")
    end
    request = '{default:'0};
    request.owner = status.control_owner;
    request.control_reset_epoch = status.control_reset_epoch;
    request.release_baseline_observation_seq = observation.observation_seq;
    status.control_state = MEMBLOCK_CONTROL_STATE_CHECK_STORE_L2_CSR_RELEASE;
    status.last_event_cycle = memblock_sync_pkg::get_dispatch_service_cycle();
    data.enqueue_l2_flush_release_request(request);
endfunction:enqueue_check_store_l2_release

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
