(function () {
  const content = document.querySelector(".content-body");
  const searchInput = document.querySelector("#doc-search");
  const clearButton = document.querySelector("#clear-search");
  const showAllButton = document.querySelector("#show-all");
  const resultCount = document.querySelector("#result-count");
  const emptyState = document.querySelector("#empty-state");
  const backTop = document.querySelector("#back-top");
  const tocLinks = Array.from(document.querySelectorAll(".toc a"));
  const filterButtons = Array.from(document.querySelectorAll("[data-filter]"));
  const scenarioSelect = document.querySelector("#flow-scenario");
  const flowGraph = document.querySelector("#flow-graph");
  const flowSummary = document.querySelector("#flow-summary");
  const functionDetail = document.querySelector("#function-detail");

  let activeFilter = "all";
  let activeFlowNode = "";

  const tagRules = [
    ["init", /初始化|参数|testcase|env|seq_csr_common/i, "初始化"],
    ["stimulus", /主表|激励|transaction|随机|手动/i, "激励生成"],
    ["admission", /LSQ Admission|入队|TLB|L2TLB|responder/i, "入队/TLB"],
    ["issue", /issue|发射|调度|字段填充|driver 边界/i, "调度发射"],
    ["monitor", /monitor|raw|writeback|采集|反馈/i, "监测输出"],
    ["commit", /commit|deq|出队|ROB|LQ\/SQ|flushSb/i, "提交出队"],
    ["recovery", /异常|redirect|replay|fault|flush/i, "异常恢复"],
    ["state", /状态|字段|key helper|公共表|重要字段/i, "状态字段"],
  ];

  const flowScenarios = [
    {
      id: "real-smoke",
      title: "端到端 real smoke 主控流程",
      summary: "展示 testcase 启动后，主控 sequence 如何构建主表、服务 monitor、路由 issue，并以状态表完成判定。",
      stages: [
        {
          title: "testcase/env",
          nodes: [
            fn("tc_dispatch_real_smoke::main_phase", "无", "启动 real smoke sequence；dispatch_real_smoke_active=1", "真实 DUT dispatch smoke 的 testcase 入口。", ["run_real_smoke_sequence()"]),
            fn("tc_base::build_phase", "uvm_phase", "创建 env；设置各 agent default_sequence", "把 LSQENQ/LINTSISSUE/LSQCOMMIT/REDIRECT/L2TLB sequence 挂到对应 sequencer。", ["memblock_env::type_id::create()", "uvm_config_db::set()"]),
          ],
        },
        {
          title: "main sequence",
          nodes: [
            fn("memblock_main_dispatch_auto_build_main_table_base_sequence::body", "无", "构建主表，启动服务循环，最后 end check", "real smoke 主控入口，不直接驱动 DUT 接口，而是依赖并行 agent sequence 和公共状态表闭环。", ["build_main_table()", "service_real_dispatch_flow()", "data.end_test_check()"]),
            fn("build_main_table", "无", "main_table_ready=1", "根据参数选择随机主表或手动主表。", ["build_random_main_table()", "import_manual_main_table()"]),
          ],
        },
        {
          title: "service loop",
          nodes: [
            fn("service_real_dispatch_flow", "无", "循环服务直到 terminal_done 或 timeout fatal", "每一轮先处理 DUT monitor 输出，再 route ready uid，最后检查是否所有 transaction 完成。", ["service_monitor_once()", "route_all_issue_queues()", "all_transactions_terminal_done()"]),
            fn("service_monitor_once", "无", "raw event 被消费；状态推进", "主控软件周期，统一处理 CSR、writeback、ctrl、redirect/replay。", ["tick_dispatch_service_cycle()", "collect_runtime_context_events()", "collect_monitor_event_batch()", "exception_redirect_replay_task()"]),
          ],
        },
        {
          title: "finish",
          nodes: [
            fn("all_transactions_terminal_done", "无", "bit", "要求 issue queue、active map、redirect/flushSb/PTW wait 都空，所有 uid 均 terminal_done。", ["data.get_status()", "data.has_pending_redirect_drive()"]),
            fn("data.end_test_check", "无", "关闭 monitor capture；fatal 未完成状态", "测试收尾一致性检查，确保没有 active uid、pending event、issue queue 或 raw queue 残留。", ["raw_monitor_queue_size()", "clear_raw_monitor_queues()"]),
          ],
        },
      ],
    },
    {
      id: "main-table",
      title: "主表构建与激励生成流程",
      summary: "展示随机/手动主表生成、uid/ROB 分配、地址复用注入和 status 初始化。",
      stages: [
        {
          title: "select",
          nodes: [
            fn("build_main_table", "无", "选择主表来源", "根据 use_manual_main_table 决定吃手动关联数组还是随机生成。", ["get_use_manual_main_table()", "build_random_main_table()", "import_manual_main_table()"]),
          ],
        },
        {
          title: "random/manual",
          nodes: [
            fn("build_random_main_table", "main_trans_num_i", "主表数组、uid、ROB 顺序", "reset 公共表后选择 ROB 起点和地址窗口，边生成 transaction 边做 recent-window 地址复用。", ["data.reset_all_tables()", "choose_rob_start_key()", "randomize_main_transaction()", "apply_addr_reuse_window()", "rob_advance()"]),
            fn("import_manual_main_table", "manual_main_table_by_rob", "按 ROB key 排序导入主表", "手动配置模式，不随机主表字段，只做合法化与校验。", ["post_manual_config()", "validate_main_table_entry()", "data.set_main_transaction()"], true),
          ],
        },
        {
          title: "fill fields",
          nodes: [
            fn("randomize_main_transaction", "tr, uid, rob_key", "填充主表字段", "先 randomize，再用 op template、合法地址、send_pri、delay 修正为可发激励。", ["select_op_class_by_weight()", "apply_minimal_op_template()", "apply_legal_addr_template()", "validate_main_table_entry()"]),
            fn("apply_minimal_op_template", "main_control_transaction", "fuType/fuOpType/lsq_flow/numLsElem", "根据 op_class 建立 load/store/prefetch/AMO 最小合法组合。", ["random_load_fuoptype()", "random_store_fuoptype()", "random_amo_fuoptype()"]),
          ],
        },
        {
          title: "finish",
          nodes: [
            fn("apply_addr_reuse_window", "tr, uid, recent queues", "可能修正类型和 src_0/imm/vaddr", "在主表生成期用 recent load/store uid 窗口提高 RAW/违例触发概率。", ["select_addr_reuse_kind()", "random_pick_recent_uid()", "set_transaction_ls_kind()", "fixup_after_addr_reuse()"]),
            fn("init_status_for_main_table", "无", "status_by_uid 初始化", "每个 uid 建立 runtime status，并 snapshot 主表 ROB/LQ/SQ key。", ["data.init_status_for_uid()"]),
            fn("data.check_main_table_complete", "无", "main_table_ready=1", "检查主表完整性，同时 arm scheduled flushSb。", ["arm_scheduled_flushsb()"]),
          ],
        },
      ],
    },
    {
      id: "lsq-admission",
      title: "LSQ admission 入队流程",
      summary: "展示主表 transaction 如何进入 DUT enqLsq，如何用 DUT resp 对齐软件 LSQ 模型，并进入 TLB/issue 后续流程。",
      stages: [
        {
          title: "sequence loop",
          nodes: [
            fn("memblock_lsqenq_dispatch_base_sequence::body", "无", "启动 LSQ enqueue sequence", "默认开启，读取 plus 参数，等待主表 ready 后进入 admission loop。", ["seq_csr_common::init()", "configure_from_plus()", "wait_for_main_table()", "drive_lsqenq_loop()"]),
            fn("drive_lsqenq_loop", "无", "多拍发送 admission", "forever loop 每拍调用 send_lsqenq_cycle，有进展则清 idle，连续空闲达到 idle_stop 后退出。", ["send_lsqenq_cycle()"]),
          ],
        },
        {
          title: "select uid",
          nodes: [
            fn("send_lsqenq_cycle", "cycle_idx", "xaction 发给 driver；成功后状态推进", "一拍 LSQ admission 主体，先处理 need_alloc=0，再收集可入队候选。", ["admit_non_lsq_if_ready()", "collect_lsq_candidates()", "assign_lsqenq_slot()", "confirm_lsq_candidates()"]),
            fn("collect_lsq_candidates", "output uids/trs/behaviors/lq_keys/sq_keys", "本拍候选队列", "从 common_data.get_next_new_admit_uid() 取得起点，按 get_enq_per_cycle() 的固定/随机上限、LQ/SQ free count 和 flush 状态选择连续 uid。", ["next_uid_needs_lsq_admission()", "advance_lq_key()", "advance_sq_key()"]),
          ],
        },
        {
          title: "drive DUT",
          nodes: [
            fn("assign_lsqenq_slot", "tr, slot, uid, main_tr, behavior, lq_key, sq_key", "填 enqLsq needAlloc/req", "把 ROB/fuType/uopIdx/LQ/SQ/numLsElem 写入对应 slot。", ["set_need_alloc()", "set_req_fields()"]),
            fn("lsqenq_agent_driver::wait_lsq_can_accept", "lsqenq xaction", "等待 canAccept；可能 abort", "driver 等 DUT canAccept，同时监测 flush_epoch，跨 redirect 则中止旧 admission。", ["sample_lsqenq_resp()"]),
          ],
        },
        {
          title: "commit software",
          nodes: [
            fn("confirm_lsq_candidates", "xaction 和候选列表", "更新软件 LSQ/status", "读取 DUT resp LQ/SQ key，确认与软件预览一致后提交。", ["get_resp_keys()", "lsq_ctrl.commit_allocate_with_resp()", "complete_admission()"]),
            fn("complete_admission", "uid", "TLB 表 ready，issue queue 路由", "入队成功后的后处理：drain runtime CSR、生成 TLB 表、把 uid route 到 issue queue。", ["drain_csr_runtime_events()", "tlb_builder.build_tlb_for_uid()", "issue_sched.route_uid()"]),
          ],
        },
      ],
    },
    {
      id: "issue-fire",
      title: "LOAD/STA/STD 调度与发射流程",
      summary: "展示 uid 如何进入 LOAD/STA/STD 队列，按 send_pri/ROB 年龄选择候选，填字段并发射到 DUT。",
      stages: [
        {
          title: "route",
          nodes: [
            fn("issue_sched.route_all_ready_uids", "无", "LOAD/STA/STD issue queue 更新", "扫描所有 uid，把 active/enq/tlb_mapped 且未 pending 的 uid route 到目标队列。", ["route_uid()"]),
            fn("route_uid", "uid", "目标队列可能新增 item", "根据 behavior 判断 route_load/route_sta/route_std。", ["lsq_ctrl_model::derive_op_behavior()", "route_target()"]),
            fn("route_target", "uid, target, behavior", "push issue queue，置 queued", "防重复入队；replay_pending 时只允许 replay target 入队。", ["target_already_queued_or_done()", "make_issue_item()", "data.push_issue_queue_item()"]),
          ],
        },
        {
          title: "select",
          nodes: [
            fn("select_issue_candidates", "output load/sta/std items", "每拍发射候选", "send_pri_mode_en 控制是否比较 priority，sample_global_send_pri_en 控制是否先找全局最大优先级，pipe 数由 sample_*_pip_num() 采样。", ["find_global_max_send_pri()", "select_target_candidates()"]),
            fn("select_target_candidates", "target, max_count, compare_pri, use_global_pri, global_pri", "selected queue", "在单 target 队列中按 global filter、优先级/ROB 年龄挑最多 max_count 个 eligible item。", ["is_issue_item_eligible()", "item_is_better()"]),
          ],
        },
        {
          title: "assign/drive",
          nodes: [
            fn("assign_issue_items", "xaction, selected items", "填 xaction 并记录 fired_items", "按 target 内候选数组 index 映射到对应 pipe。", ["field_assigner.assign_issue_item_fields()"]),
            fn("issue_field_assigner::assign_issue_item_fields", "tr, item, pipe_idx", "主字段/依赖字段/后端 meta 全部填好", "发射字段总入口。", ["assign_main_issue_fields()", "assign_issue_dep_fields()", "assign_backend_meta_fields()"]),
            fn("lintsissue_agent_driver::send_pkt", "issue xaction", "驱动 intIssue valid/payload，返回 fired_mask", "等待 pipe ready；flush_epoch 跨拍则 abort 未 fire 端口。", []),
          ],
        },
        {
          title: "mark fire",
          nodes: [
            fn("mark_fired_items", "fired_items, fired_mask", "状态表发射结果更新", "只标记 driver 确认 fired 的端口；redirect abort 时保留已接收 item。", ["mark_issue_fire()", "mark_issue_fire_already_accepted()", "submit_issue_accept_pass()"]),
            fn("mark_issue_fire", "issue item", "删除队列项，置 dispatched，分配 issue_epoch", "发射成功后的核心状态更新；replay target 发射后清 replay mask。", ["data.alloc_issue_epoch()", "data.mark_issue_snapshot()", "data.delete_issue_queue_entry()", "data.clear_replay_target_after_fire()"]),
          ],
        },
      ],
    },
    {
      id: "monitor-wb",
      title: "monitor 输出采集与 writeback 状态更新",
      summary: "展示 DUT 输出如何先进入 raw queue，再由 adapter 转成 wb_event，最后进入状态表或复杂事件队列。",
      stages: [
        {
          title: "raw monitor",
          nodes: [
            fn("io_mem_to_ooo_int_wb_monitor::mon_data", "DUT writeback ports", "push_raw_int_wb(raw)", "采 load/store writeback、ROB/LQ/SQ key 和 exception_vec。", ["memblock_sync_pkg::push_raw_int_wb()"]),
            fn("io_mem_to_ooo_iq_feedback_monitor::mon_data", "DUT IQ feedback", "push_raw_iq_feedback(raw)", "采 STA/STD feedback；hit 表示 pass，miss 表示 backend replay。", ["memblock_sync_pkg::push_raw_iq_feedback()"], true),
            fn("io_mem_to_ooo_ctrl_monitor::mon_data", "DUT ctrl", "push_raw_ctrl(raw)", "采 lqDeq/sqDeq/memoryViolation/sbIsEmpty。", ["memblock_sync_pkg::push_raw_ctrl()"], true),
          ],
        },
        {
          title: "adapter",
          nodes: [
            fn("service_monitor_once", "无", "统一 drain raw queue", "主控每轮服务 raw queue。", ["collect_runtime_context_events()", "collect_monitor_event_batch()"]),
            fn("collect_monitor_event_batch", "无", "normal pass/replay/fault/redirect event", "同批消费 int_wb、IQ feedback 和 ctrl memoryViolation，交给 batch handler 做 redirect-first 仲裁。", ["collect_writeback_events_batch()", "collect_ctrl_redirect_events_batch()", "process_monitor_event_batch()"]),
            fn("collect_ctrl_redirect_events_batch", "events[$]", "ctrl deq 和 memoryViolation redirect", "消费 ctrl raw queue，deq 直接交 commit handler，memoryViolation 转 redirect event 放入 batch。", ["apply_raw_ctrl_deq()", "convert_raw_memory_violation()"]),
          ],
        },
        {
          title: "status/event",
          nodes: [
            fn("writeback_handler.handle_event", "memblock_wb_event_t", "更新 status 或 push exception_event_q", "普通 pass 直接写状态；replay/redirect/fault 推给 exception handler。", ["data.normalize_feedback_event()", "data.mark_target_normal_pass()", "data.push_feedback_event()"]),
            fn("data.mark_target_normal_pass", "uid, target, issue_epoch, replay_seq, cycle", "target pass；可能置全局 writeback/pass", "按 target issue_epoch 和 replay_seq 过滤旧反馈。", ["conditional_set_target_status_field()", "required_targets_done()"]),
          ],
        },
      ],
    },
    {
      id: "commit-deq",
      title: "ROB commit 与 LQ/SQ deq 流程",
      summary: "展示 TB 主动驱动 pendingPtr，DUT ctrl monitor 返回 LQ/SQ deq，两者都完成后 retire uid。",
      stages: [
        {
          title: "commit drive",
          nodes: [
            fn("memblock_lsqcommit_dispatch_base_sequence::body", "无", "启动 commit sequence", "读取 plus 参数，等待主表 ready，进入 commit loop。", ["configure_from_plus()", "ensure_helpers()", "drive_lsqcommit_loop()"]),
            fn("send_lsqcommit_cycle", "cycle_idx", "驱动 pendingPtr 或 flushSb", "优先处理 scheduled flushSb/global flush，再构造 pendingPtr xaction。", ["request_scheduled_flushsb_if_due()", "drive_flushsb_if_needed()", "build_lsqcommit_xaction()"]),
            fn("build_lsqcommit_xaction", "output tr, commit_uids, has_commit", "pendingPtr 指向 batch 最后一条 ROB", "按 ROB 顺序选连续可 commit uid。", ["select_rob_commit_batch()", "clear_lsqcommit_xaction()"]),
          ],
        },
        {
          title: "mark commit",
          nodes: [
            fn("mark_rob_commit_batch", "uids[$]", "批量 rob_commit=1", "pendingPtr xaction 发送后更新软件状态。", ["mark_rob_commit_uid()"]),
            fn("mark_rob_commit_uid", "uid", "rob_commit=1；可能 lsq_deq=1 并 retire", "如果 LQ/SQ 映射已释放，可直接完成 retire 检查。", ["uid_is_commit_candidate()", "data.try_retire_committed_uid()"]),
          ],
        },
        {
          title: "DUT deq",
          nodes: [
            fn("apply_raw_ctrl_deq", "lq_count, lq_ptr, sq_count, sq_ptr", "LQ/SQ deq 状态更新", "当前唯一 ctrl deq 入口，由 raw ctrl adapter 调用。", ["apply_dut_lq_deq()", "apply_dut_sq_deq()"]),
            fn("apply_dut_lq_deq", "count, deq_ptr, ptr_is_next", "释放 LQ 资源和 active map", "检查 DUT deq 起点等于软件 head，逐项 lookup uid 后释放。", ["lq_deq_start_key()", "data.lookup_active_uid_by_lq()", "lsq_ctrl.release_lq()", "data.release_uid_lq_mapping()"]),
            fn("apply_dut_sq_deq", "count, deq_ptr, ptr_is_next", "释放 SQ 资源和 active map", "SQ 版本 deq 处理。", ["sq_deq_start_key()", "data.lookup_active_uid_by_sq()", "lsq_ctrl.release_sq()", "data.release_uid_sq_mapping()"], true),
          ],
        },
        {
          title: "retire",
          nodes: [
            fn("data.try_retire_committed_uid", "uid", "可能 success=1 并 retire", "要求 rob_commit=1 且 active_lq_mapped/active_sq_mapped 都为 0。", ["retire_active_uid()"]),
            fn("data.retire_active_uid", "uid", "删除 active ROB/LQ/SQ map，active=0", "uid 生命周期结束清理，remove_uid_from_issue_queues 只作兜底。", ["remove_uid_from_issue_queues()", "rob_to_map_key()", "lq_to_map_key()", "sq_to_map_key()"]),
          ],
        },
      ],
    },
    {
      id: "replay-redirect",
      title: "backend replay 与 redirect/flush 恢复流程",
      summary: "展示 replay/redirect/fault 复杂事件如何进入 exception handler，如何冻结发射、驱动 redirect、flush 状态，以及 replay 如何重新入队。",
      stages: [
        {
          title: "complex event",
          nodes: [
            fn("writeback_handler.handle_event", "wb_event", "复杂事件进入 exception_event_q", "replay/redirect/fault 不在 writeback handler 内完成恢复，只入队等待 exception handler。", ["data.push_feedback_event()"]),
            fn("exception_handler.process_pending_events", "无", "推进 replay/redirect/fault", "先服务 PTW wait replay，再推进 active redirect；redirect 优先于 replay/fault。", ["service_ptw_wait_replay()", "advance_active_redirect()", "select_oldest_redirect()", "handle_replay_event()"]),
          ],
        },
        {
          title: "redirect",
          nodes: [
            fn("select_oldest_redirect", "events[$]", "selected redirect event", "redirect 优先级最高，多个 redirect 选择 ROB 最老。", ["redirect_event_is_older()", "rob_is_after()"]),
            fn("data.request_redirect_flush", "redirect payload", "flush_in_progress=1；flush_epoch++；issue_freeze_ack=1", "开始 recovery，阻塞 admission/issue/commit，并通知 driver 等待 ready 时 abort。", ["push_redirect_drive()"]),
            fn("memblock_redirect_dispatch_base_sequence::drive_redirect_payload", "payload", "驱动 redirect_agent", "真实把 redirect payload 打到 DUT 接口，完成后标记 drive done。", ["assign_redirect_xaction()", "data.mark_redirect_drive_done()"]),
            fn("data.apply_redirect_flush", "redirect payload", "flush 命中 uid，清 recovery 状态", "按 rob_need_flush() retire 被 flush 的 active uid，并清 PTW wait replay。", ["rob_need_flush()", "clear_uid_dispatch_result()", "retire_active_uid()", "clear_ptw_wait_replay_by_redirect()"]),
          ],
        },
        {
          title: "backend replay",
          nodes: [
            fn("handle_replay_event", "wb_event", "replay_pending 或 PTW wait replay", "解析 uid、issue_epoch、replay_seq；PTW-back replay 可先进入 wait 队列。", ["data.resolve_uid_for_event()", "data.push_ptw_wait_replay()", "data.mark_replay_pending()"]),
            fn("data.mark_replay_pending", "uid, target, issue_epoch, replay_seq, cycle", "清旧队列；置 replay_pending；replay_seq++", "backend replay 核心状态切换，清 target dispatched/writeback/pass 并设置 replay_target。", ["remove_uid_from_issue_queues()", "bump_replay_seq()"]),
            fn("route_uid", "uid", "replay target 重新入队", "replay_pending 时只 route replay_target_requested 的 target，避免错误重复发射。", ["route_target()"]),
            fn("mark_issue_fire", "replay issue item", "新 issue_epoch；清 replay target", "replay item 重新 fire 后，所有 replay target 清空则 replay_pending=0。", ["mark_issue_snapshot()", "clear_replay_target_after_fire()"]),
          ],
        },
      ],
    },
    {
      id: "l2tlb",
      title: "L2TLB responder 查表回填流程",
      summary: "展示 DTLB->L2TLB request 如何通过 runtime CSR + TLB 表查 uid，并回填 response。",
      stages: [
        {
          title: "sequence",
          nodes: [
            fn("memblock_l2tlb_base_sequence::body", "无", "启动 L2TLB responder", "确认 plus enable 与 connect takeover 后直接进入 request responder loop。", ["configure_from_plus()", "ensure_context()", "drive_l2tlb_loop()"]),
            fn("drive_l2tlb_loop", "无", "多拍采样 request", "每拍在 l2tlb_vif.drv_cb 调 send_l2tlb_cycle；有 progress 清 idle_count，连续 idle 超过 idle_stop_cycle 后退出。", ["send_l2tlb_cycle()"]),
          ],
        },
        {
          title: "request",
          nodes: [
            fn("send_l2tlb_cycle", "idle_count", "ready/response xaction", "采 DTLB request，发 ready，按 latency 设置 response gap，查/建 TLB entry 后发 response。", ["request_valid()", "sample_request_fields()", "get_or_create_tlb_entry_by_req()", "fill_dtlb_resp_from_entry()"]),
            fn("sample_request_fields", "output vpn, s2xlate", "request key 字段", "从 DTLB/L2TLB 接口采样 vpn 和 s2xlate。", []),
          ],
        },
        {
          title: "lookup",
          nodes: [
            fn("get_or_create_tlb_entry_by_req", "vpn, s2xlate", "key, entry, created", "用 runtime CSR 生成 {vpn,asid,vmid,s2xlate} key，命中返回旧 entry，miss 自动建 entry。", ["make_tlb_key_by_req()", "build_tlb_entry_for_key()", "insert_tlb_entry()"]),
            fn("data.update_uid_tlb_records_by_entry", "key, entry", "匹配 uid record 被回填", "把 response entry 的 PTE 信息同步给所有匹配的 pending uid record。", ["memblock_uid_tlb_record::copy_entry_fields()"]),
          ],
        },
        {
          title: "response",
          nodes: [
            fn("fill_dtlb_resp_from_entry", "response xaction, entry", "写 PTE/PPN/权限/fault 字段", "把 by-key TLB entry 回填到 DTLB response。", []),
          ],
        },
      ],
    },
  ];

  function fn(name, input, output, desc, calls, branch) {
    return { name, input, output, desc, calls: calls || [], branch: Boolean(branch) };
  }

  function wrapTables() {
    document.querySelectorAll(".content-body table").forEach((table) => {
      if (table.parentElement && table.parentElement.classList.contains("table-wrap")) {
        return;
      }
      const wrap = document.createElement("div");
      wrap.className = "table-wrap";
      table.parentNode.insertBefore(wrap, table);
      wrap.appendChild(table);
    });
  }

  function enhanceCodeBlocks() {
    document.querySelectorAll(".content-body pre").forEach((pre, index) => {
      if (pre.parentElement && pre.parentElement.classList.contains("code-block")) {
        return;
      }
      const wrap = document.createElement("div");
      wrap.className = "code-block";
      pre.parentNode.insertBefore(wrap, pre);
      wrap.appendChild(pre);

      const button = document.createElement("button");
      button.type = "button";
      button.className = "copy-code";
      button.textContent = "复制";
      button.setAttribute("aria-label", `复制代码块 ${index + 1}`);
      button.addEventListener("click", async () => {
        const text = pre.textContent;
        try {
          await navigator.clipboard.writeText(text);
        } catch (error) {
          const area = document.createElement("textarea");
          area.value = text;
          document.body.appendChild(area);
          area.select();
          document.execCommand("copy");
          area.remove();
        }
        button.textContent = "已复制";
        window.setTimeout(() => {
          button.textContent = "复制";
        }, 1200);
      });
      wrap.appendChild(button);
    });
  }

  function addHeadingAnchors() {
    content.querySelectorAll("h1, h2, h3").forEach((head) => {
      if (!head.id || head.querySelector(".heading-anchor")) {
        return;
      }
      const link = document.createElement("a");
      link.className = "heading-anchor";
      link.href = `#${head.id}`;
      link.setAttribute("aria-label", "复制该章节链接");
      link.textContent = "#";
      head.prepend(link);
    });
  }

  function classifyHeading(text) {
    const tags = [];
    tagRules.forEach(([key, regex]) => {
      if (regex.test(text)) {
        tags.push(key);
      }
    });
    return tags.length ? tags : ["state"];
  }

  function sectionBlocks() {
    const heads = Array.from(content.querySelectorAll("h2, h3"));
    return heads.map((head, index) => {
      if (!head.dataset.tags) {
        const tags = classifyHeading(head.textContent);
        head.dataset.tags = tags.join(",");
        const primary = tagRules.find(([key]) => key === tags[0]);
        if (primary && !head.querySelector(".section-tag")) {
          const tag = document.createElement("span");
          tag.className = "section-tag";
          tag.textContent = primary[2];
          head.appendChild(tag);
        }
      }

      const nodes = [head];
      let cur = head.nextElementSibling;
      const next = heads[index + 1];
      while (cur && cur !== next) {
        nodes.push(cur);
        cur = cur.nextElementSibling;
      }
      return {
        head,
        nodes,
        tags: head.dataset.tags.split(","),
      };
    });
  }

  function clearMarks() {
    content.querySelectorAll("mark.search-hit").forEach((mark) => {
      mark.replaceWith(document.createTextNode(mark.textContent));
    });
    content.normalize();
  }

  function markNodeText(node, query) {
    const walker = document.createTreeWalker(node, NodeFilter.SHOW_TEXT, {
      acceptNode(textNode) {
        if (!textNode.nodeValue || !textNode.nodeValue.toLowerCase().includes(query)) {
          return NodeFilter.FILTER_REJECT;
        }
        if (textNode.parentElement && textNode.parentElement.closest("script, style, button")) {
          return NodeFilter.FILTER_REJECT;
        }
        return NodeFilter.FILTER_ACCEPT;
      },
    });
    const matches = [];
    while (walker.nextNode()) {
      matches.push(walker.currentNode);
    }
    matches.forEach((textNode) => {
      const value = textNode.nodeValue;
      const lower = value.toLowerCase();
      const frag = document.createDocumentFragment();
      let pos = 0;
      let idx = lower.indexOf(query, pos);
      while (idx !== -1) {
        frag.appendChild(document.createTextNode(value.slice(pos, idx)));
        const mark = document.createElement("mark");
        mark.className = "search-hit";
        mark.textContent = value.slice(idx, idx + query.length);
        frag.appendChild(mark);
        pos = idx + query.length;
        idx = lower.indexOf(query, pos);
      }
      frag.appendChild(document.createTextNode(value.slice(pos)));
      textNode.replaceWith(frag);
    });
  }

  function setActiveToc(ids) {
    tocLinks.forEach((link) => {
      const id = decodeURIComponent(link.hash.slice(1));
      link.classList.toggle("active", ids.has(id));
    });
  }

  function applyView() {
    const query = searchInput.value.trim().toLowerCase();
    const visibleIds = new Set();
    let visible = 0;

    clearMarks();
    sectionBlocks().forEach(({ head, nodes, tags }) => {
      const filterHit = activeFilter === "all" || tags.includes(activeFilter);
      const searchText = nodes.map((node) => node.textContent).join("\n").toLowerCase();
      const searchHit = !query || searchText.includes(query);
      const show = filterHit && searchHit;

      nodes.forEach((node) => node.classList.toggle("hidden-by-ui", !show));
      if (show) {
        visible += 1;
        if (head.id) {
          visibleIds.add(head.id);
        }
        if (query) {
          nodes.forEach((node) => markNodeText(node, query));
        }
      }
    });

    emptyState.classList.toggle("visible", visible === 0);
    const filterLabel = filterButtons.find((button) => button.dataset.filter === activeFilter)?.textContent || "全部";
    resultCount.textContent = query || activeFilter !== "all"
      ? `${filterLabel}：显示 ${visible} 个章节`
      : "";
    setActiveToc(query || activeFilter !== "all" ? visibleIds : new Set());
  }

  function resetView() {
    activeFilter = "all";
    searchInput.value = "";
    filterButtons.forEach((button) => {
      button.classList.toggle("active", button.dataset.filter === "all");
    });
    applyView();
  }

  function setupFilters() {
    filterButtons.forEach((button) => {
      button.addEventListener("click", () => {
        activeFilter = button.dataset.filter;
        filterButtons.forEach((item) => {
          item.classList.toggle("active", item === button);
        });
        applyView();
      });
    });
  }

  function setupActiveTocOnScroll() {
    const heads = Array.from(content.querySelectorAll("h2, h3"));
    if (!("IntersectionObserver" in window)) {
      return;
    }
    const observer = new IntersectionObserver((entries) => {
      if (searchInput.value.trim() || activeFilter !== "all") {
        return;
      }
      const visible = entries
        .filter((entry) => entry.isIntersecting)
        .sort((a, b) => b.intersectionRatio - a.intersectionRatio)[0];
      if (visible && visible.target.id) {
        setActiveToc(new Set([visible.target.id]));
      }
    }, {
      rootMargin: "0px 0px -72% 0px",
      threshold: [0.1, 0.4, 0.8],
    });
    heads.forEach((head) => observer.observe(head));
  }

  function setupBackTop() {
    window.addEventListener("scroll", () => {
      backTop.classList.toggle("visible", window.scrollY > 640);
    }, { passive: true });
    backTop.addEventListener("click", () => {
      window.scrollTo({ top: 0, behavior: "smooth" });
    });
  }

  function allFlowNodes(scenario) {
    return scenario.stages.flatMap((stage) => stage.nodes);
  }

  function findFlowNode(scenario, name) {
    return allFlowNodes(scenario).find((node) => node.name === name);
  }

  function findCallRelations(scenario, node) {
    if (!scenario || !node) {
      return { callers: [], callees: [], externalCalls: [] };
    }
    const knownNames = new Set(allFlowNodes(scenario).map((item) => item.name));
    const callers = allFlowNodes(scenario)
      .filter((item) => item.calls.includes(node.name))
      .map((item) => item.name);
    const callees = node.calls.filter((name) => knownNames.has(name));
    const externalCalls = node.calls.filter((name) => !knownNames.has(name));
    return { callers, callees, externalCalls };
  }

  function renderFunctionDetail(scenario, node) {
    if (!node) {
      functionDetail.innerHTML = [
        '<div class="function-detail-kicker">FUNCTION DETAIL</div>',
        "<h3>选择一个函数节点</h3>",
        "<p>右侧会展示该函数的输入、输出/副作用、功能，以及它继续调用的子函数。</p>",
      ].join("");
      return;
    }

    const relations = findCallRelations(scenario, node);
    const callers = relations.callers.length
      ? `<ul class="detail-calls">${relations.callers.map((item) => `<li>${escapeHtml(item)}</li>`).join("")}</ul>`
      : '<p class="detail-value">当前场景中没有展开的直接调用方，通常是该阶段入口或由外部 UVM phase 触发。</p>';
    const callees = node.calls.length
      ? `<ul class="detail-calls">${node.calls.map((item) => `<li class="${relations.callees.includes(item) ? "call-linked" : "call-external"}">${escapeHtml(item)}</li>`).join("")}</ul>`
      : '<p class="detail-value">无直接子调用或当前文档未展开。</p>';
    functionDetail.innerHTML = [
      '<div class="function-detail-kicker">FUNCTION DETAIL</div>',
      `<h3><code>${escapeHtml(node.name)}</code></h3>`,
      detailRow("输入", node.input),
      detailRow("输出/副作用", node.output),
      detailRow("功能", node.desc),
      '<div class="detail-row">',
      '<div class="detail-label">调用来源</div>',
      callers,
      "</div>",
      '<div class="detail-row">',
      '<div class="detail-label">继续调用</div>',
      callees,
      "</div>",
      relations.externalCalls.length
        ? '<p class="detail-hint">浅色标签表示当前场景未展开的 helper、driver 或公共 API；蓝色标签表示图中有对应函数节点。</p>'
        : "",
    ].join("");
  }

  function detailRow(label, value) {
    return [
      '<div class="detail-row">',
      `<div class="detail-label">${escapeHtml(label)}</div>`,
      `<p class="detail-value">${escapeHtml(value)}</p>`,
      "</div>",
    ].join("");
  }

  function escapeHtml(value) {
    return String(value)
      .replace(/&/g, "&amp;")
      .replace(/</g, "&lt;")
      .replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;");
  }

  function renderFlowScenario(scenarioId, preferredNode) {
    const scenario = flowScenarios.find((item) => item.id === scenarioId) || flowScenarios[0];
    const nodes = allFlowNodes(scenario);
    const knownNames = new Set(nodes.map((node) => node.name));
    const selectedName = preferredNode && findFlowNode(scenario, preferredNode)
      ? preferredNode
      : nodes[0]?.name;

    activeFlowNode = selectedName || "";
    flowSummary.textContent = scenario.summary;
    flowGraph.innerHTML = scenario.stages.map((stage) => {
      const nodeHtml = stage.nodes.map((node) => renderFlowNode(node, scenario, knownNames)).join("");
      return [
        '<section class="flow-stage">',
        `<div class="flow-stage-title">${escapeHtml(stage.title)}</div>`,
        nodeHtml,
        "</section>",
      ].join("");
    }).join("");

    flowGraph.querySelectorAll("[data-flow-node]").forEach((button) => {
      button.addEventListener("click", () => {
        activeFlowNode = button.dataset.flowNode;
        updateFlowHighlights(scenario);
        renderFunctionDetail(scenario, findFlowNode(scenario, activeFlowNode));
      });
    });
    updateFlowHighlights(scenario);
    renderFunctionDetail(scenario, findFlowNode(scenario, activeFlowNode));
  }

  function renderFlowNode(node, scenario, knownNames) {
    const linkedCalls = node.calls.filter((name) => knownNames.has(name));
    const visibleCalls = linkedCalls.slice(0, 3);
    const callLine = visibleCalls.length
      ? `<span class="flow-node-calls">调用 -> ${visibleCalls.map(escapeHtml).join(" / ")}${linkedCalls.length > 3 ? " ..." : ""}</span>`
      : '<span class="flow-node-calls muted">调用 -> 外部 helper/当前场景未展开</span>';
    const relation = findCallRelations(scenario, node);
    const callerLine = relation.callers.length
      ? `<span class="flow-node-callers">来源 <- ${relation.callers.map(escapeHtml).join(" / ")}</span>`
      : '<span class="flow-node-callers muted">来源 <- 场景入口或外部触发</span>';
    return [
      `<button type="button" class="flow-node${node.branch ? " branch" : ""}" data-flow-node="${escapeHtml(node.name)}">`,
      `<span class="flow-node-name">${escapeHtml(node.name)}</span>`,
      `<span class="flow-node-desc">${escapeHtml(node.desc)}</span>`,
      callerLine,
      callLine,
      "</button>",
    ].join("");
  }

  function updateFlowHighlights(scenario) {
    const selectedNode = findFlowNode(scenario, activeFlowNode);
    const relations = findCallRelations(scenario, selectedNode);
    const callerSet = new Set(relations.callers);
    const calleeSet = new Set(relations.callees);
    flowGraph.querySelectorAll(".flow-node").forEach((item) => {
      const name = item.dataset.flowNode;
      item.classList.toggle("active", name === activeFlowNode);
      item.classList.toggle("is-caller", callerSet.has(name));
      item.classList.toggle("is-callee", calleeSet.has(name));
    });
  }

  function setupFlowExplorer() {
    if (!scenarioSelect || !flowGraph || !functionDetail) {
      return;
    }
    scenarioSelect.innerHTML = flowScenarios.map((scenario) => {
      return `<option value="${escapeHtml(scenario.id)}">${escapeHtml(scenario.title)}</option>`;
    }).join("");
    scenarioSelect.addEventListener("change", () => {
      renderFlowScenario(scenarioSelect.value);
    });
    renderFlowScenario(flowScenarios[0].id);
  }

  wrapTables();
  enhanceCodeBlocks();
  addHeadingAnchors();
  sectionBlocks();
  setupFilters();
  setupActiveTocOnScroll();
  setupBackTop();
  setupFlowExplorer();

  searchInput.addEventListener("input", applyView);
  clearButton.addEventListener("click", resetView);
  showAllButton.addEventListener("click", resetView);
})();
